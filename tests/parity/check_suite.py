"""Check a change against original F77 or the fixed corrected reference.

For every requested variant this builds the original F77 reference (with the
documented R04 square rewrites and BC/ON link accommodations) and the
working tree, runs that variant's whole `tests/FVSxx` makefile in each, and
requires every generated file to match, restart (`.stop`) files included.
With --reference corrected, compare against the committed corrected sources
and the immediate pre-change commit instead, without original-defect masks.

Results are cached by the content of each variant's build and test inputs, so
the reference is built once, and a variant the change does not touch is not
rebuilt. Nothing is written inside the repository.
"""

from __future__ import annotations

import argparse
from concurrent.futures import ThreadPoolExecutor
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import tempfile

sys.dont_write_bytecode = True  # keep the repository free of __pycache__

from compare_outputs import UUID, compare_directories
import poison_storage
from prepare_builds import (CORRECTED_REFERENCE, CORRECTED_REFERENCE_NVEL,
                            REFERENCE, REFERENCE_NVEL, ROOT, archive,
                            copy_working_tree, git, reference_accommodations,
                            selected_inputs, sha256)

SOURCE_SUFFIXES = (".f90", ".f", ".f77", ".for", ".inc", ".c", ".cpp", ".h")
STATUS_LINE = re.compile(r"^(make.*Error \d+|STOP\b.*|Test .* ended)")
UUID_BYTES = re.compile(UUID.pattern.encode())
WINDOWS = sys.platform == "win32"


def make_command(*arguments: str) -> list[str]:
    """Run GNU make; on Windows, pass what MSYS make would not give recipes.

    The makefile selects its Windows settings from OS. MSYS make does not hand
    TMP/TEMP to the native compilers and FVS programs it starts, which then fail
    to create temporary files; command-line variables reach every recipe.
    """
    command = ["make", *arguments]
    if WINDOWS:
        scratch = os.environ.get("TMP") or tempfile.gettempdir()
        command += ["OS=Windows_NT", f"TMP={scratch}", f"TEMP={scratch}", f"GFORTRAN_TMPDIR={scratch}"]
    return command


def variants_in(root: Path) -> list[str]:
    return sorted(path.name[3:5] for path in (root / "bin").glob("FVS??_sourceList.txt"))


def static_check(baseline: str) -> list[str]:
    """Catch source-list mistakes before any compiler runs."""
    problems = []
    listed = set()
    for variant in variants_in(ROOT):
        manifest = ROOT / "bin" / f"FVS{variant}_sourceList.txt"
        names = manifest.read_text().split()
        for name in names:
            path = (manifest.parent / name).resolve()
            listed.add(path)
            if not path.is_file():
                problems.append(f"{manifest.name}: missing file {name}")
        basenames = [Path(name).name for name in names]
        for name in sorted({n for n in basenames if basenames.count(n) > 1}):
            problems.append(f"{manifest.name}: two files named {name}")
    for name in (ROOT / "bin/fortran_includes.txt").read_text().split():
        if not (ROOT / name).is_file():
            problems.append(f"fortran_includes.txt: missing file {name}")
    added = git(ROOT, "diff", "--name-only", "--diff-filter=A", baseline).splitlines()
    added += git(ROOT, "ls-files", "--others", "--exclude-standard").splitlines()
    for name in sorted(set(added)):
        path = (ROOT / name).resolve()
        # bin/ holds build tooling (bin/fvsbuild.c), not model source.
        if (name.lower().endswith(SOURCE_SUFFIXES) and not name.startswith(("tests/", "bin/"))
                and path.is_file() and path not in listed):
            problems.append(f"new source file is in no source list: {name}")
    return problems


def snapshot(state: str, revision: str | None, target: Path) -> None:
    """Extract one version, with its own NVEL, into an empty directory."""
    if target.exists():
        shutil.rmtree(target)
    if revision is None:
        copy_working_tree(ROOT, target)
        copy_working_tree(ROOT / "volume/NVEL", target / "volume/NVEL")
        return
    nvel = git(ROOT, "ls-tree", revision, "volume/NVEL").split()[2]
    if state == "original" and nvel != REFERENCE_NVEL:
        raise RuntimeError("Original NVEL identity differs from the recorded reference")
    if state == "corrected" and nvel != CORRECTED_REFERENCE_NVEL:
        raise RuntimeError("Corrected NVEL identity differs from the recorded reference")
    archive(ROOT, revision, target, target.with_suffix(".tar"))
    archive(ROOT / "volume/NVEL", nvel, target / "volume/NVEL", target.with_suffix(".nvel.tar"))
    if state == "original":
        reference_accommodations(target)


def use_working_decks(tree: Path, variants: list[str]) -> None:
    """Give a reference snapshot the working tree's test folders.

    New or changed decks then run on every reference as well as on the
    candidate, so their outputs are compared rather than reported as present
    only in the working tree. Generated outputs in the checkout are not copied.
    """
    for variant in variants:
        folder = f"tests/FVS{variant}"
        names = subprocess.check_output(["git", "-C", str(ROOT), "ls-files", "-z", "--cached", "--others",
                                         "--exclude-standard", "--", folder]).decode().split("\0")
        shutil.rmtree(tree / folder)
        for name in filter(None, names):
            if (ROOT / name).is_file():
                (tree / name).parent.mkdir(parents=True, exist_ok=True)
                shutil.copy2(ROOT / name, tree / name)


def inventory(directory: Path) -> dict[str, str]:
    return {str(path.relative_to(directory)): sha256(path)
            for path in sorted(directory.rglob("*")) if path.is_file()}


DEFAULT_PROGRAM = "FVS{variant}"


def program_target(template: str, variant: str) -> tuple[str, str]:
    """The command a variant's decks run, and the make target that builds it.

    The default runs each variant's own program. A template such as
    "FVSall --variant={variant}" will select a default configuration of a
    combined program once SP15/SP16 provide one; until then the build target
    must still be one of the per-variant programs.
    """
    command = template.format(variant=variant)
    target = command.split()[0]
    if not re.fullmatch(r"FVS[a-z]{2}", target):
        raise RuntimeError(f"no build rule for program {target!r} before SP15/SP16")
    return command, target


def run_key(tree: Path, variant: str, toolchain: str,
            mode: dict | None = None) -> tuple[str, dict[str, str]]:
    """Identify a run by everything that can change its outputs."""
    tests = inventory(tree / "tests" / f"FVS{variant}")
    target = program_target((mode or {}).get("program") or DEFAULT_PROGRAM, variant)[1]
    identity = {"toolchain": toolchain, "build": selected_inputs(tree, target[3:5]), "tests": tests}
    if mode:
        identity["mode"] = mode
    return hashlib.sha256(json.dumps(identity, sort_keys=True).encode()).hexdigest()[:24], tests


def build_and_run(tree: Path, variant: str, tests: dict[str, str], store: Path,
                  jobs: int, env: dict[str, str], template: str = DEFAULT_PROGRAM) -> None:
    """Build the program a variant's decks use, run them, keep only generated files."""
    command, target = program_target(template, variant)
    scratch = store.with_name(store.name + ".partial")
    if scratch.exists():
        shutil.rmtree(scratch)
    (scratch / "files").mkdir(parents=True)
    with (scratch / "build.log").open("wb") as log:
        built = subprocess.run(make_command(f"{target}.setup", f"-j{jobs}"), cwd=tree / "bin",
                               env=env, stdout=log, stderr=subprocess.STDOUT)
    program = tree / "bin" / (f"{target}.exe" if WINDOWS else target)
    if built.returncode or not program.is_file():
        raise RuntimeError(f"build failed; see {scratch / 'build.log'}")
    directory = tree / "tests" / f"FVS{variant}"
    if command != f"FVS{variant}":
        # Point this variant's decks at the chosen program configuration. The
        # rewritten makefile is an input, not a result.
        makefile = directory / "makefile"
        text = makefile.read_text()
        rewritten = re.sub(rf"\.\./\.\./bin/FVS{variant}\b", lambda _: "../../bin/" + command, text)
        if rewritten == text:
            raise RuntimeError(f"tests/FVS{variant}/makefile does not run ../../bin/FVS{variant}")
        makefile.write_text(rewritten)
        tests = {**tests, "makefile": sha256(makefile)}
    with (scratch / "suite.log").open("wb") as log:
        subprocess.run(make_command("--ignore-errors"), cwd=directory, env=env, stdin=subprocess.DEVNULL,
                       stdout=log, stderr=subprocess.STDOUT, timeout=3600)
    # Program stop codes and ignored make errors are results too.
    status = [line for line in (scratch / "suite.log").read_text(errors="replace").splitlines()
              if STATUS_LINE.match(line)]
    (scratch / "files" / "suite-status.txt").write_text("\n".join(status) + "\n")
    outputs = ["suite-status.txt"]
    for name, digest in inventory(directory).items():
        if tests.get(name) != digest:
            destination = scratch / "files" / name
            destination.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(directory / name, destination)
            outputs.append(name)
    (scratch / "outputs.json").write_text(json.dumps(sorted(outputs), indent=1) + "\n")
    scratch.rename(store)


def restart_bytes(path: Path) -> bytes:
    """Restart files are binary. Replace run identifiers, keeping every offset."""
    seen: dict[bytes, bytes] = {}
    return UUID_BYTES.sub(lambda m: seen.setdefault(m[0], (b"<UUID-%04d>" % len(seen)).ljust(36)),
                          path.read_bytes())


def restart_difference(left: Path, right: Path, variant: str,
                       original_defects: bool = True) -> str | None:
    """Compare two restart files, allowing only two documented original defects.

    Both are repaired in the working tree (right), but not in original F77
    (left), which reads uninitialized memory, so no two original runs need agree:
    * Original TT stores a location code (IGL) taken from a table that is
      never filled (FOUND_BUGS.md, bug 11). It is the 25th integer of each
      stand's record. The repaired working tree must store GRINIT's value, 1.
    * Original tree-file decay codes come from the previous record's history
      code, which is undefined for the first tree (bug 12). A live tree can
      therefore store 3 in the original; the repaired working tree must store 0.
    Anything else must match byte for byte.
    """
    a, b = restart_bytes(left), restart_bytes(right)
    if len(a) != len(b):
        return f"sizes differ ({len(a)} and {len(b)} bytes)"
    positions = [i for i in range(len(a)) if a[i] != b[i]] if a != b else []
    if not positions:
        return None
    # Corrected and immediate references already define these values. The
    # original's undefined TT/decay values must not hide a new regression.
    if not original_defects:
        return f"{len(positions)} bytes differ, first at offset {positions[0]}"
    allowed: set[int] = set()
    if variant == "tt":
        # The file starts with stop code, year, name length and name, then the
        # first stand's integers. Every stand repeats the same undefined value
        # on the left (original or pre-repair) side; the right must hold 1.
        first = 12 + int.from_bytes(a[8:12], "little") + 24 * 4
        ours, theirs = a[first:first + 4], b[first:first + 4]
        word = a.find(ours) if theirs == (1).to_bytes(4, "little") else -1
        while word >= 0:
            if b[word:word + 4] == theirs:
                allowed.update(range(word, word + 4))
            word = a.find(ours, word + 1)
    remaining = []
    for index, position in enumerate(positions):
        if position in allowed:
            continue
        alone = ((index == 0 or positions[index - 1] < position - 3)
                 and (index + 1 == len(positions) or positions[index + 1] > position + 3))
        decay = (a[position] == 3 and b[position] == 0
                 and a[position + 1:position + 4] == b[position + 1:position + 4] == b"\0\0\0")
        if not (alone and decay):
            remaining.append(position)
    if remaining:
        return f"{len(remaining)} bytes differ, first at offset {remaining[0]}"
    return None


def compare(left: Path, right: Path, variant: str,
            original_defects: bool = True) -> list[str]:
    """Return one line per difference between two stored runs."""
    names = [set(json.loads((side / "outputs.json").read_text())) for side in (left, right)]
    differences = [f"{name}: only in {'reference' if name in names[0] else 'working tree'}"
                   for name in sorted(names[0] ^ names[1])]
    restarts = sorted(name for name in names[0] & names[1] if name.endswith(".stop"))
    for name in restarts:
        message = restart_difference(left / "files" / name, right / "files" / name,
                                     variant, original_defects)
        if message:
            differences.append(f"{name}: {message}")
    others = [sorted(name for name in side if not name.endswith(".stop")) for side in names]
    try:
        result = compare_directories(left / "files", right / "files", others[0], others[1])
    except (ValueError, OSError) as error:
        return differences + [f"cannot compare: {error}"]
    for error in result["errors"]:
        if error["kind"] == "bytes/text":
            differences.append(f"{error['file']}: first difference at line {error['first_differing_line']}")
        elif error["kind"] == "sqlite":
            entry = result["files"][error["file"]]
            tables = [name for name in sorted(set(entry["left"]["tables"]) | set(entry["right"]["tables"]))
                      if entry["left"]["tables"].get(name) != entry["right"]["tables"].get(name)]
            shown = ", ".join(tables[:5]) + (f" and {len(tables) - 5} more" if len(tables) > 5 else "")
            differences.append(f"{error['file']}: tables differ: {shown or 'schema only'}")
        elif error["kind"] != "inventory":
            differences.append(f"{error.get('file', '')}: {error['kind']} differs")
    return differences


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--cache-dir", type=Path, required=True,
                        help="Temporary directory outside the repository; reuse it between runs")
    parser.add_argument("--baseline", default="HEAD",
                        help="Commit before the change, used only to find new source files "
                             "(default HEAD, for uncommitted work)")
    parser.add_argument("--reference", choices=("original", "corrected"), default="original",
                        help="Simulation reference; corrected also checks the immediate version")
    parser.add_argument("--immediate", metavar="REV",
                        help="Immediate pre-change commit for corrected comparisons (default HEAD); "
                             "this does not change the fixed corrected reference")
    parser.add_argument("--variants", nargs="+", choices=variants_in(ROOT), default=variants_in(ROOT))
    parser.add_argument("--workers", type=int, default=4, help="Variants built and tested at once")
    parser.add_argument("--jobs", type=int, default=4, help="Compiler processes per build")
    parser.add_argument("--static-only", action="store_true", help="Check the source lists and stop")
    parser.add_argument("--program", default=DEFAULT_PROGRAM, metavar="TEMPLATE",
                        help="Working-tree command each variant's decks run, {variant} substituted "
                             "(default FVS{variant}: each variant's own program)")
    parser.add_argument("--with-new-decks", action="store_true",
                        help="Run the working tree's tests/FVSxx decks on the references too "
                             "(for decks that the reference commits do not have yet)")
    parser.add_argument("--poison-storage", action="store_true",
                        help="Raise working-tree storage sizes and poison inactive COMMON slots (SP03); "
                             "see poison_storage.py")
    args = parser.parse_args()
    if args.immediate and args.reference != "corrected":
        parser.error("--immediate requires --reference corrected")
    cache = args.cache_dir.resolve()
    if cache.is_relative_to(ROOT):
        parser.error("Use a cache directory outside the repository")
    baseline = git(ROOT, "rev-parse", args.baseline)
    references = [("original", REFERENCE)]
    if args.reference == "corrected":
        references = [("corrected", CORRECTED_REFERENCE),
                      ("immediate", git(ROOT, "rev-parse", args.immediate or "HEAD"))]

    problems = static_check(baseline)
    for problem in problems:
        print(f"FAIL source lists: {problem}")
    if problems:
        return 1
    print("PASS source lists: every entry exists and every new source file is listed")
    if args.static_only:
        return 0

    for tool in ("gfortran", "gcc", "make", "sqlite3"):
        if not shutil.which(tool):
            parser.error(f"{tool} is required")
    toolchain = " | ".join(subprocess.check_output([tool, "--version"], text=True).splitlines()[0]
                           for tool in ("gfortran", "gcc", "sqlite3"))
    # Strip ambient switches the makefile reads, and fix locale and time zone.
    env = {k: v for k, v in os.environ.items()
           if k not in ("OS", "OSARCH", "OSTYPE", "mingw64", "sourceFileList", "sourceList")}
    env.update({"LC_ALL": "C", "TZ": "UTC"})
    (cache / "runs").mkdir(parents=True, exist_ok=True)

    stores: dict[tuple[str, str], Path] = {}
    for state, revision in references + [("candidate", None)]:
        tree = cache / "trees" / state
        print(f"Preparing {state}" + (f" {revision[:8]}" if revision else " (working tree)"), flush=True)
        snapshot(state, revision, tree)
        if revision is not None and args.with_new_decks:
            use_working_decks(tree, args.variants)
        # Only the working tree runs a chosen configuration or poisoned storage;
        # every reference runs its own per-variant programs.
        mode = None
        if state == "candidate" and (args.program != DEFAULT_PROGRAM or args.poison_storage):
            mode = {"program": args.program, "poison": args.poison_storage}
            if args.poison_storage:
                targets = sorted({program_target(args.program, v)[1][3:5] for v in args.variants})
                skipped = poison_storage.apply(tree, targets)
                print(f"  poisoned storage in {len(targets)} variants; not poisoned: "
                      f"{sum(len(s) for s in skipped.values())} logical or derived-extent arrays", flush=True)
        pending = []
        for variant in args.variants:
            key, tests = run_key(tree, variant, toolchain, mode)
            stores[state, variant] = cache / "runs" / key
            if not stores[state, variant].is_dir():
                pending.append((variant, tests))
        if pending:
            print(f"  building and testing {len(pending)} of {len(args.variants)} variants; "
                  f"the rest are cached", flush=True)

        def work(item: tuple[str, dict[str, str]]) -> str | None:
            try:
                build_and_run(tree, item[0], item[1], stores[state, item[0]], args.jobs, env,
                              args.program if mode else DEFAULT_PROGRAM)
            except (RuntimeError, subprocess.TimeoutExpired) as error:
                return f"FAIL {item[0]} ({state}): {error}"
            return None

        with ThreadPoolExecutor(max_workers=args.workers) as pool:
            failures = [message for message in pool.map(work, pending) if message]
        for message in failures:
            print(message)
        if failures:
            return 1
        shutil.rmtree(tree)

    any_failed = False
    for state, revision in references:
        failed = []
        label = (f"original F77 {revision[:8]} with R04 squares" if state == "original"
                 else f"{state} {revision[:8]}")
        for variant in args.variants:
            reference, candidate = stores[state, variant], stores["candidate", variant]
            differences = compare(reference, candidate, variant, original_defects=state == "original")
            outputs = json.loads((candidate / "outputs.json").read_text())
            note = ("  (original stops at keyword input; see FOUND_BUGS.md)"
                    if state == "original" and variant in ("bc", "on") else "")
            if differences:
                failed.append(variant)
                print(f"FAIL {variant}: {len(differences)} of {len(outputs)} files differ vs {label}{note}")
                for line in differences[:8]:
                    print(f"       {line}")
                if len(differences) > 8:
                    print(f"       ... and {len(differences) - 8} more")
                print(f"       outputs to diff: {state} {reference / 'files'}")
                print(f"                    working tree {candidate / 'files'}")
            else:
                print(f"PASS {variant}: {len(outputs)} files match {label}{note}")
        dirty = "+changes" if git(ROOT, "status", "--porcelain") else ""
        summary = (f"{len(args.variants) - len(failed)}/{len(args.variants)} variants; working tree "
                   f"{git(ROOT, 'rev-parse', '--short', 'HEAD')}{dirty} vs {label}")
        print(("FAIL " + summary + "; failed: " + " ".join(failed)) if failed else "PASS " + summary)
        any_failed |= bool(failed)
    return 1 if any_failed else 0


if __name__ == "__main__":
    sys.exit(main())
