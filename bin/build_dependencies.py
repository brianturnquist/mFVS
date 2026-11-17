"""Prepare the flat include view and bootstrap GNU Make's dependency graph.

The compiler's -MMD files remain authoritative for active includes. This small
first-build graph orders module producers before consumers, including USEs in
Fortran INCLUDE files, without making every object depend on every module.
"""

from __future__ import annotations

import argparse
import json
import os
from pathlib import Path
import re
import shlex
import shutil
import subprocess
import sys

sys.dont_write_bytecode = True
from shared_compile import compiler_identity, digest


ROOT = Path(__file__).resolve().parents[1]
COMMENT = re.compile(r"'([^']|'')*'|\"([^\"]|\"\")*\"|(!.*)")
MODULE = re.compile(r"(?im)^\s*module\s+(?!procedure\b|function\b|subroutine\b)(\w+)")
USE = re.compile(r"(?im)^\s*use\b\s*(?:,\s*(?:non_)?intrinsic\s*)?(?:::\s*)?(\w+)")
INCLUDE = re.compile(r"(?im)^\s*include\s*['\"]([^'\"]+)['\"]")


def write_changed(path: Path, text: str) -> None:
    if not path.exists() or path.read_text() != text:
        path.write_text(text)


def make_path(path: Path | str) -> str:
    return str(path).replace("\\", "/").replace("$", "$$").replace("#", "\\#").replace(" ", "\\ ")


def cleaned(text: str) -> str:
    text = "\n".join(COMMENT.sub(lambda m: "" if m[3] else m[0], line)
                     for line in text.splitlines())
    return re.sub(r"&\s*\n\s*&?", "", text)


def fortran_text(path: Path, compiler: list[str], flags: list[str]) -> tuple[str, set[Path]]:
    """Expand the native INCLUDE search order; preprocess only units needing CPP."""
    included = set()

    def expand(source: Path, ancestors: set[Path]) -> str:
        source = source.resolve()
        if source in ancestors:
            raise ValueError(f"recursive Fortran INCLUDE: {source}")
        text = source.read_bytes().decode("latin1")
        if re.search(r"(?m)^\s*#", text):
            text = subprocess.check_output([*compiler, *flags, "-E", str(source)],
                                           stderr=subprocess.PIPE).decode("latin1")
            for name in re.findall(r'^#\s+\d+\s+"([^"]+)"', text, re.M):
                dependency = Path(name)
                if dependency.is_file() and dependency.resolve() != source:
                    included.add(dependency.resolve())
        text = cleaned(text)
        additions = []
        for name in INCLUDE.findall(text):
            candidates = (source.parent / name, Path.cwd() / name)
            target = next((p for p in candidates if p.is_file()), None)
            if target is None:
                raise ValueError(f"missing Fortran INCLUDE {name} in {source}")
            included.add(target.resolve())
            additions.append(expand(target, ancestors | {source}))
        return text + "\n" + "\n".join(additions)

    return expand(path, set()), included


def prepare(manifest: Path, native_sources: bool) -> None:
    paths = [(manifest.parent / name).resolve() for name in manifest.read_text().split()]
    names = [p.name for p in paths]
    if len(names) != len(set(names)):
        raise ValueError(f"duplicate selected basename in {manifest}")
    headers = {(ROOT / name).resolve() for name in (ROOT / "bin/fortran_includes.txt").read_text().split()}
    units = [p for p in paths if p not in headers and p.suffix.lower() in (".f90", ".f", ".for", ".c", ".cpp")]
    if native_sources:
        selected = {p.name: p for p in paths}
        for path in paths:
            if path.suffix.lower() not in (".f90", ".f", ".for", ".c", ".cpp", ".h", ".inc"):
                continue
            for name in re.findall(r"(?im)^\s*(?:#\s*)?include\s*['\"]([^'\"]+)['\"]", path.read_text(errors="replace")):
                adjacent = path.parent / name
                chosen = selected.get(name)
                if adjacent.is_file() and chosen and digest(adjacent) != digest(chosen):
                    raise ValueError(f"native-source lookup would change selected include {name}: {path}")
    # Stage regular files, never links to editable model sources. In native mode
    # only selected includes/headers need the historical flat lookup view.
    for path in paths:
        if native_sources and path in units:
            continue
        destination = Path(path.name)
        if not destination.exists() or (destination.stat().st_mtime_ns, destination.stat().st_size) != (path.stat().st_mtime_ns, path.stat().st_size):
            shutil.copy2(path, destination)
    mode = Path(".source-mode")
    value = "native\n" if native_sources else "staged\n"
    format_file = Path(".dependency-format")
    if (mode.exists() and mode.read_text() != value) or not format_file.exists():
        # Old .d files name the previous source view. Rebuild from the new view
        # rather than keeping stale dependencies on discarded staged sources.
        for path in Path.cwd().glob("*.d"):
            path.unlink()
    write_changed(mode, value)
    write_changed(format_file, "receipts-v1\n")
    if native_sources:
        for path in units:
            Path(path.name).unlink(missing_ok=True)

    # Ordinary compiler recipes need not spawn a second process just to touch
    # a stamp. On the next invocation, synchronize their local receipt to the
    # completed object. Shared objects already publish their own fresh receipt.
    for path in units:
        obj = Path(path.stem + ".o")
        receipt = Path(path.stem + ".o.stamp")
        if not obj.exists():
            receipt.unlink(missing_ok=True)
        elif not receipt.exists() or receipt.stat().st_mtime_ns < obj.stat().st_mtime_ns:
            receipt.touch()
            stamp = obj.stat().st_mtime_ns
            os.utime(receipt, ns=(stamp, stamp))

    fc = shlex.split(os.environ.get("FCprf", "") + os.environ.get("FC", "gfortran"))
    cc = shlex.split(os.environ.get("CCprf", "") + os.environ.get("CC", "gcc"))
    flags = {"fortran": shlex.split(os.environ.get("FFLAGS", "")),
             "c": shlex.split(os.environ.get("CFLAGS", ""))}
    cache = Path(os.environ.get("FVS_SHARED_CACHE", str(ROOT / "bin/.shared_build")))
    helpers = {p.name: digest(p) for p in (Path(__file__), ROOT / "bin/shared_compile.py", ROOT / "bin/makefile")}
    for language, compiler in (("fortran", fc), ("c", cc)):
        settings = {"compiler": compiler_identity(compiler[0], cache), "flags": flags[language],
                    "helpers": helpers, "native_sources": native_sources}
        write_changed(Path(f".{language}.flags"), json.dumps(settings, sort_keys=True))

    # Most invocations change no declarations. Reuse a scan only while all of
    # its source/INCLUDE/CPP inputs and compiler settings still match by content.
    # This avoids launching the preprocessor hundreds of times on a no-op build.
    scan_file = Path(".fortran-scan.json")
    scan_key = digest(Path(".fortran.flags"))
    previous = json.loads(scan_file.read_text()) if scan_file.exists() else {}
    if previous.get("settings") != scan_key:
        previous = {}
    scanned = {}
    hashes = {}

    def file_hash(path: Path) -> str | None:
        name = str(path.resolve())
        if name not in hashes:
            hashes[name] = digest(path) if path.is_file() else None
        return hashes[name]

    providers = {}
    records = {}
    for source in units:
        local = source if native_sources else Path(source.name)
        if source.suffix.lower() not in (".f90", ".f", ".for"):
            records[source] = (local, set(), [], [])
            continue
        key = str(local.resolve())
        record = previous.get("units", {}).get(key)
        if record and all(file_hash(Path(name)) == value for name, value in record["inputs"].items()):
            includes = {Path(name) for name in record["includes"]}
            modules, uses = record["modules"], record["uses"]
        else:
            text, includes = fortran_text(local, fc, flags["fortran"])
            modules = [name.lower() for name in MODULE.findall(text)]
            uses = [name.lower() for name in USE.findall(text)]
            record = {"inputs": {str(p.resolve()): file_hash(p) for p in [local, *sorted(includes)]},
                      "includes": [str(p) for p in sorted(includes)], "modules": modules, "uses": uses}
        scanned[key] = record
        records[source] = (local, includes, modules, uses)
        for name in modules:
            if name in providers and providers[name] != source:
                raise ValueError(f"two selected producers of module {name}")
            providers[name] = source
    write_changed(scan_file, json.dumps({"settings": scan_key, "units": scanned}, sort_keys=True))
    owned_modules = Path(".module-providers.json")
    previous_modules = set(json.loads(owned_modules.read_text())) if owned_modules.exists() else set()
    for name in previous_modules - set(providers):
        # A removed/renamed producer must not leave an interface that a later
        # build can accidentally read. Never prune caller-supplied modules.
        Path(name + ".mod").unlink(missing_ok=True)
    write_changed(owned_modules, json.dumps(sorted(providers)))

    lines = ["# Generated from selected sources; the compiler records active includes in *.d.\n"]
    for source, (local, includes, modules, uses) in records.items():
        obj = source.stem + ".o"
        language = "fortran" if source.suffix.lower() in (".f90", ".f", ".for") else "c"
        dependencies = [make_path(local), f".{language}.flags"]
        # Bootstrap header dependencies disappear once the compiler supplies
        # its precise active include closure. New USEs must be ordered even
        # when an older compiler .d file already exists for the source.
        if not Path(source.stem + ".d").exists():
            dependencies.extend(make_path(p) for p in sorted(includes))
        dependencies.extend(name + ".mod" for name in sorted(set(uses))
                            if name in providers and providers[name] != source)
        lines.append(make_path(obj + ".stamp") + ": " + " ".join(dependencies) + "\n")
        for name in modules:
            # .mod is a side effect of one object compile. Testing its presence
            # avoids the former second compile of the same module source.
            prefix = "$(FVS_SHARED_COMPILE) " if "/volume/NVEL/" in source.as_posix() else ""
            if prefix:
                prefix = prefix.replace("$(FVS_SHARED_COMPILE)",
                                        "$(PYTHON) $(FVS_ROOT)/bin/shared_compile.py --cache-dir \"$(FVS_SHARED_CACHE)\" --groups \"$(FVS_SHARED_GROUPS)\" --kind nvel --hardlink --")
            lines.append(f"{name}.mod: {make_path(obj)}\n")
            lines.append(f"\t@test -f $@ || {prefix}$(FCprf)$(FC) $(FFLAGS) $(call FVS_DEPFLAGS,{obj}) -c -o {make_path(obj)} {make_path(local)}\n")
    # Preserve manifest order and detect membership changes even if the removed
    # object's timestamp can no longer trigger a relink.
    write_changed(Path(".link-inputs"), "\n".join(p.stem + ".o" for p in units) + "\n")
    link = {name: os.environ.get(name, "") for name in ("LD", "FCprf", "DBLINK", "OSARCH", "SLIBSUFX", "PRGSUFX")}
    link["rules"] = helpers["makefile"]
    write_changed(Path(".link.flags"), json.dumps(link, sort_keys=True))
    write_changed(Path("dependencies.mk"), "".join(lines))


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--manifest", type=Path, required=True)
    parser.add_argument("--native-sources", action="store_true")
    args = parser.parse_args()
    prepare(args.manifest.resolve(), args.native_sources)
    print("ready")


if __name__ == "__main__":
    main()
