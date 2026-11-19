"""Exercise cache identity, concurrent publication and private linked state.

These are compiled behavioral cases, not expected hashes of the implementation.
They call the build's own launcher, bin/fvsbuild.c, compiled here with gcc as
Make and CMake compile it. All fixture sources, objects, libraries, the
compiled helper and cache entries are disposable.
"""

from __future__ import annotations

from concurrent.futures import ThreadPoolExecutor
import ctypes
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[2]
TOOL: Path  # compiled from bin/fvsbuild.c in main()


def compile(directory: Path, cache: Path, compiler: str, source: str,
            kind: str, flags: tuple[str, ...] = (), groups: str = "sqlite,nvel") -> str:
    result = subprocess.run([str(TOOL), "compile", "--cache-dir", str(cache),
                             "--kind", kind, "--groups", groups, "--hardlink", "--",
                             compiler, *flags, "-c", "-o", Path(source).stem + ".o", source],
                            cwd=directory, capture_output=True, text=True, check=True)
    return result.stdout


def c_source(directory: Path, value: int) -> None:
    directory.mkdir()
    (directory / "sqlite3.c").write_text('#include "value.h"\nstatic int n=VALUE;\nint number(void){return n++;}\n')
    (directory / "value.h").write_text(f"#ifndef VALUE\n#define VALUE {value}\n#endif\n")


def library(directory: Path, name: str):
    path = directory / name
    subprocess.run(["gcc", "-shared", "-o", str(path), "sqlite3.o"], cwd=directory, check=True)
    result = ctypes.CDLL(str(path))
    result.number.restype = ctypes.c_int
    return result


def check_c(root: Path) -> None:
    cache = root / "c-cache"
    first, second = root / "first", root / "second"
    c_source(first, 4)
    c_source(second, 4)
    flags = ("-O2", "-fPIC")
    assert "SHARED MISS" in compile(first, cache, "gcc", "sqlite3.c", "sqlite", flags)
    assert "SHARED HIT" in compile(second, cache, "gcc", "sqlite3.c", "sqlite", flags)
    a, b = library(first, "a.so"), library(second, "b.so")
    assert [a.number(), a.number(), b.number(), b.number()] == [4, 5, 4, 5]
    # Changed active headers, macro definitions and code-generation flags must
    # not be confused with the equal basename in the first build directory.
    (second / "value.h").write_text("#define VALUE 9\n")
    assert "SHARED MISS" in compile(second, cache, "gcc", "sqlite3.c", "sqlite", flags)
    assert library(second, "changed.so").number() == 9
    assert "SHARED MISS" in compile(first, cache, "gcc", "sqlite3.c", "sqlite", (*flags, "-DVALUE=7"))
    assert library(first, "defined.so").number() == 7
    assert "SHARED MISS" in compile(first, cache, "gcc", "sqlite3.c", "sqlite", ("-O0", "-fPIC"))
    # Turning reuse off must unlink a previous hard link before the compiler
    # writes it. Returning to the old configuration must still yield 4.
    (first / "sqlite3.c").write_text("int number(void){return 23;}\n")
    compile(first, cache, "gcc", "sqlite3.c", "sqlite", flags, groups="")
    assert library(first, "uncached.so").number() == 23
    third = root / "third"
    c_source(third, 4)
    assert "SHARED HIT" in compile(third, cache, "gcc", "sqlite3.c", "sqlite", flags)
    assert library(third, "original.so").number() == 4
    print("PASS C identity, immutable entries, disabled reuse and separate runtime state")


def check_parallel(root: Path) -> None:
    cache = root / "parallel-cache"
    directories = [root / f"parallel-{i}" for i in range(8)]
    for directory in directories:
        c_source(directory, 11)
    with ThreadPoolExecutor(max_workers=8) as pool:
        logs = list(pool.map(lambda p: compile(p, cache, "gcc", "sqlite3.c", "sqlite", ("-fPIC",)), directories))
    assert sum("SHARED MISS" in log for log in logs) == 1
    assert sum("SHARED HIT" in log for log in logs) == 7
    assert all(library(p, "parallel.so").number() == 11 for p in directories)
    print("PASS eight concurrent builds publish one complete entry")


def check_fortran(root: Path) -> None:
    cache = root / "fortran-cache"
    flags = ("-cpp", "-g", "-O2", "-ffp-contract=off")
    for index in range(2):
        directory = root / f"fortran-{index}"
        directory.mkdir()
        (directory / "m.f90").write_text("module M\ninteger,parameter :: P=3\nend\n")
        (directory / "f.f90").write_text("integer function F()\nuse M\nF=P\nend\n")
        (directory / "main.f90").write_text("integer F\nprint *,F()\nend\n")
        for source in ("m.f90", "f.f90"):
            log = compile(directory, cache, "gfortran", source, "nvel", flags)
            assert ("SHARED MISS" if index == 0 else "SHARED HIT") in log
    directory = root / "fortran-1"
    (directory / "m.mod").unlink()
    assert "SHARED HIT" in compile(directory, cache, "gfortran", "m.f90", "nvel", flags)
    assert (directory / "m.mod").is_file()
    (directory / "m.f90").write_text("module M\ninteger,parameter :: P=7\nend\n")
    assert "SHARED MISS" in compile(directory, cache, "gfortran", "m.f90", "nvel", flags)
    assert "SHARED MISS" in compile(directory, cache, "gfortran", "f.f90", "nvel", flags)
    subprocess.run(["gfortran", "main.f90", "m.o", "f.o", "-o", "run"], cwd=directory, check=True)
    assert subprocess.check_output([str(directory / "run")], text=True).strip() == "7"
    print("PASS Fortran interface invalidation and missing-module restoration")


def main() -> None:
    global TOOL
    if not shutil.which("gcc") or not shutil.which("gfortran"):
        raise SystemExit("GNU C and Fortran compilers are required")
    # Windows cannot delete the fixture libraries while ctypes holds them loaded.
    with tempfile.TemporaryDirectory(prefix="mfvs-build-check-", dir="/private/tmp" if sys.platform == "darwin" else None,
                                     ignore_cleanup_errors=sys.platform == "win32") as name:
        root = Path(name)
        TOOL = root / "fvsbuild"
        subprocess.run(["gcc", "-O2", "-o", str(TOOL), str(ROOT / "bin/fvsbuild.c")], check=True)
        check_c(root)
        check_parallel(root)
        check_fortran(root)


if __name__ == "__main__":
    main()
