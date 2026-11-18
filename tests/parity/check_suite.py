"""Check a change against original F77 using the existing regression suite.

For every requested variant this builds the original F77 reference (with the
documented R04 square rewrites and BC/ON link accommodations) and the
working tree, runs that variant's whole `tests/FVSxx` makefile in each, and
requires every generated file to match, restart (`.stop`) files included.

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

sys.dont_write_bytecode = True  # keep the repository free of __pycache__

from compare_outputs import UUID, compare_directories
from prepare_builds import (REFERENCE, REFERENCE_NVEL, ROOT, archive,
                            copy_working_tree, git, reference_accommodations,
                            selected_inputs, sha256)

SOURCE_SUFFIXES = (".f90", ".f", ".f77", ".for", ".inc", ".c", ".cpp", ".h")
STATUS_LINE = re.compile(r"^(make.*Error \d+|STOP\b.*|Test .* ended)")
UUID_BYTES = re.compile(UUID.pattern.encode())


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
    archive(ROOT, revision, target, target.with_suffix(".tar"))
    archive(ROOT / "volume/NVEL", nvel, target / "volume/NVEL", target.with_suffix(".nvel.tar"))
    if state == "original":
        reference_accommodations(target)


def inventory(directory: Path) -> dict[str, str]:
    return {str(path.relative_to(directory)): sha256(path)
            for path in sorted(directory.rglob("*")) if path.is_file()}


def run_key(tree: Path, variant: str, toolchain: str) -> tuple[str, dict[str, str]]:
    """Identify a run by everything that can change its outputs."""
    tests = inventory(tree / "tests" / f"FVS{variant}")
    identity = {"toolchain": toolchain, "build": selected_inputs(tree, variant), "tests": tests}
    return hashlib.sha256(json.dumps(identity, sort_keys=True).encode()).hexdigest()[:24], tests


def build_and_run(tree: Path, variant: str, tests: dict[str, str], store: Path,
                  jobs: int, env: dict[str, str]) -> None:
    """Build one variant, run its suite, and keep only the generated files."""
    scratch = store.with_name(store.name + ".partial")
    if scratch.exists():
        shutil.rmtree(scratch)
    (scratch / "files").mkdir(parents=True)
    with (scratch / "build.log").open("wb") as log:
        built = subprocess.run(["make", f"FVS{variant}.setup", f"-j{jobs}"], cwd=tree / "bin",
                               env=env, stdout=log, stderr=subprocess.STDOUT)
    if built.returncode or not (tree / "bin" / f"FVS{variant}").is_file():
        raise RuntimeError(f"build failed; see {scratch / 'build.log'}")
    directory = tree / "tests" / f"FVS{variant}"
    with (scratch / "suite.log").open("wb") as log:
        subprocess.run(["make", "--ignore-errors"], cwd=directory, env=env, stdin=subprocess.DEVNULL,
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


def restart_difference(left: Path, right: Path, variant: str) -> str | None:
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


def compare(left: Path, right: Path, variant: str) -> list[str]:
    """Return one line per difference between two stored runs."""
    names = [set(json.loads((side / "outputs.json").read_text())) for side in (left, right)]
    differences = [f"{name}: only in {'original' if name in names[0] else 'working tree'}"
                   for name in sorted(names[0] ^ names[1])]
    restarts = sorted(name for name in names[0] & names[1] if name.endswith(".stop"))
    for name in restarts:
        message = restart_difference(left / "files" / name, right / "files" / name, variant)
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
    parser.add_argument("--variants", nargs="+", choices=variants_in(ROOT), default=variants_in(ROOT))
    parser.add_argument("--workers", type=int, default=4, help="Variants built and tested at once")
    parser.add_argument("--jobs", type=int, default=4, help="Compiler processes per build")
    parser.add_argument("--static-only", action="store_true", help="Check the source lists and stop")
    args = parser.parse_args()
    cache = args.cache_dir.resolve()
    if cache.is_relative_to(ROOT):
        parser.error("Use a cache directory outside the repository")
    baseline = git(ROOT, "rev-parse", args.baseline)

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
    for state, revision in (("original", REFERENCE), ("candidate", None)):
        tree = cache / "trees" / state
        print(f"Preparing {state}" + (f" {revision[:8]}" if revision else " (working tree)"), flush=True)
        snapshot(state, revision, tree)
        pending = []
        for variant in args.variants:
            key, tests = run_key(tree, variant, toolchain)
            stores[state, variant] = cache / "runs" / key
            if not stores[state, variant].is_dir():
                pending.append((variant, tests))
        if pending:
            print(f"  building and testing {len(pending)} of {len(args.variants)} variants; "
                  f"the rest are cached", flush=True)

        def work(item: tuple[str, dict[str, str]]) -> str | None:
            try:
                build_and_run(tree, item[0], item[1], stores[state, item[0]], args.jobs, env)
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

    failed = []
    for variant in args.variants:
        original, candidate = stores["original", variant], stores["candidate", variant]
        differences = compare(original, candidate, variant)
        outputs = json.loads((candidate / "outputs.json").read_text())
        note = "  (original stops at keyword input; see FOUND_BUGS.md)" if variant in ("bc", "on") else ""
        if differences:
            failed.append(variant)
            print(f"FAIL {variant}: {len(differences)} of {len(outputs)} files differ{note}")
            for line in differences[:8]:
                print(f"       {line}")
            if len(differences) > 8:
                print(f"       ... and {len(differences) - 8} more")
            print(f"       outputs to diff: original {original / 'files'}")
            print(f"                    working tree {candidate / 'files'}")
        else:
            print(f"PASS {variant}: {len(outputs)} files match original F77 with R04 squares{note}")
    dirty = "+changes" if git(ROOT, "status", "--porcelain") else ""
    summary = (f"{len(args.variants) - len(failed)}/{len(args.variants)} variants; working tree "
               f"{git(ROOT, 'rev-parse', '--short', 'HEAD')}{dirty} vs original {REFERENCE[:8]} "
               "with R04 squares")
    print(("FAIL " + summary + "; failed: " + " ".join(failed)) if failed else "PASS " + summary)
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
