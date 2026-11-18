"""Check the focused pre-module correctness repairs with real model objects.

By default build BC, ON, EM and IE in a disposable source snapshot. An explicit
--build-tree may reuse an already built snapshot. Relink the small driver with
the selected objects, excluding only the standalone main. No model providers
are stubbed. The changed routines are rebuilt with bounds checks and signaling
NaNs; ENTRY procedures retain the compiler coverage limit in bug 63.
"""

import argparse
from collections import Counter
from concurrent.futures import ThreadPoolExecutor
import importlib.util
from pathlib import Path
import re
import shutil
import sqlite3
import subprocess
import sys
import tempfile

sys.dont_write_bytecode = True
from check_suite import snapshot

ROOT = Path(__file__).resolve().parents[2]
CASES = {"bc": ("constants", "summary"),
         "on": ("constants", "summary", "live", "natcrs"),
         "em": ("regent",), "ie": ("constants", "fia")}
REPAIRED = {"evldx.f90", "vols.f90", "fmsvol.f90", "varvol.f90", "regent.f90",
            "dbssumry.f90", "dbs_fiavbc_cutlst.f90", "dbs_fiavbc_atrtls.f90"}
FLAGS = ["-g", "-fcheck=all", "-finit-real=snan", "-fbacktrace",
         "-ffpe-trap=invalid,zero,underflow,overflow"]


def check_memory_rows() -> None:
    spec = importlib.util.spec_from_file_location(
        "memtables", ROOT / "tests/APImemTables/memtables_ctypes.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    with sqlite3.connect(":memory:") as con:
        con.executescript('CREATE TABLE a(x REAL); INSERT INTO a VALUES(10);'
                          'CREATE TABLE b(x REAL); INSERT INTO b VALUES(99);'
                          'CREATE TABLE "odd""table"("odd""column" TEXT);'
                          'INSERT INTO "odd""table" VALUES(NULL),(\'\'),(\'value\'),(\'value\');')
        assert module._rows(con, "a", ["x"]) != module._rows(con, "b", ["x"])
        assert module._rows(con, 'odd"table', ['odd"column']) == Counter(
            {(None,): 2, ("value",): 2})
    print("PASS memory rows: changed values, quoted identifiers, duplicates and NULLs", flush=True)


def run(command: list[str], directory: Path) -> subprocess.CompletedProcess:
    result = subprocess.run(command, cwd=directory, capture_output=True, text=True, timeout=600)
    if result.returncode:
        raise RuntimeError(f"{Path(command[0]).name} exited {result.returncode}\n"
                           + result.stdout[-1500:] + result.stderr[-3000:])
    return result


def link_driver(tree: Path, variant: str, directory: Path) -> Path:
    directory.mkdir()
    manifest = tree / "bin" / f"FVS{variant}_sourceList.txt"
    selected = {(manifest.parent / name).name: (manifest.parent / name).resolve()
                for name in manifest.read_text().split()}
    includes = {(tree / name).resolve() for name in
                (tree / "bin/fortran_includes.txt").read_text().split()}
    for name, source in selected.items():
        if source in includes:
            shutil.copy2(source, directory / name)
    build = tree / "bin" / f"FVS{variant}_buildDir"
    objects = []
    for source in selected.values():
        if (source in includes or source.suffix.lower() not in (".f90", ".c", ".cpp")
                or source.name == "main.f90"):
            continue
        obj = build / (source.stem + ".o")
        if source.name in REPAIRED:
            obj = directory / obj.name
            run(["gfortran", "-cpp", "-DCMPgcc", "-I", str(directory), "-I", str(build),
                 *FLAGS, "-c", str(source), "-o", str(obj)], directory)
        if not obj.is_file():
            raise RuntimeError(f"Missing built object: {obj}")
        objects.append(str(obj))
    program = directory / "check"
    run(["gfortran", "-I", str(directory), *FLAGS,
         str(Path(__file__).with_name("focused_bug_cases.f90")), *objects, "-o", str(program)], directory)
    return program


def check_summary(database: Path, variant: str) -> None:
    table = "FVS_Summary_East_Metric" if variant == "on" else "FVS_Summary_Metric"
    with sqlite3.connect(database) as con:
        rows = con.execute(f'SELECT * FROM "{table}" ORDER BY Year').fetchall()
    expected = (101, 102, 103, 104, 105, 10.5, 201, 202, 203, 301, 401, 402, 403,
                501, 502, 503, 504, 20.5, 10, 601, 602, 30.5, 701, 2, 3)
    assert [(r[2], r[3], r[4:]) for r in rows] == [(2001, 51, expected), (2002, 52, expected)]


def check_legacy_summary(program: Path, original: Path, directory: Path) -> None:
    directory.mkdir()
    # Retain a previous row and the historical INSERT-compatible column names.
    # The writer must append without altering that row or migrating the schema.
    with sqlite3.connect(original) as con:
        schema = con.execute("SELECT sql FROM sqlite_master WHERE name='FVS_Summary_East_Metric'").fetchone()[0]
        previous = con.execute("SELECT * FROM FVS_Summary_East_Metric LIMIT 1").fetchone()
    renames = {"TCuM": "MCuM", "MCuM": "SCuM", "CCuM": "NCuM",
               "RTCuM": "RMCuM", "RMCuM": "RSCuM", "RCCuM": "RNCuM"}
    schema = re.sub(r"\b(" + "|".join(renames) + r")\b", lambda m: renames[m[0]], schema)
    database = directory / "summary.db"
    with sqlite3.connect(database) as con:
        con.execute(schema)
        con.execute("INSERT INTO FVS_Summary_East_Metric VALUES (" + ",".join("?" * len(previous)) + ")", previous)
    run([str(program), "summary"], directory)
    with sqlite3.connect(database) as con:
        rows = con.execute("SELECT * FROM FVS_Summary_East_Metric").fetchall()
        assert rows == [previous, previous, (previous[0], previous[1], 2002, 52, *previous[4:])]
        assert con.execute("SELECT sql FROM sqlite_master WHERE name='FVS_Summary_East_Metric'").fetchone()[0] == schema


def check_native_summary(tree: Path, variant: str, directory: Path) -> None:
    deck = "YSM-SkyRanch" if variant == "bc" else "Hardwood"
    shutil.copytree(tree / "tests" / f"FVS{variant}", directory)
    result = subprocess.run([str(tree / "bin" / f"FVS{variant}"), f"--keywordfile={deck}.key"],
                            cwd=directory, capture_output=True, text=True, timeout=120)
    assert result.returncode in (0, 10, 20), result.stderr[-1000:]
    groups = []
    for line in (directory / f"{deck}.sum").read_text().splitlines():
        if line.startswith("-999"):
            groups.append([])
        else:
            groups[-1].append((int(line[:4]), int(line[4:8])))
    assert groups and all(len(group) >= 3 for group in groups)
    for group in groups:
        first_year, first_age = group[0]
        assert all(age - first_age == year - first_year for year, age in group)
        assert 1 <= group[1][0] - first_year <= 5
        assert all(group[i][0] - group[i-1][0] == 5 for i in range(2, len(group)))
    print(f"PASS {variant} native summary: {sum(map(len, groups))} correctly indexed cycles", flush=True)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--build-tree", type=Path, help="Explicitly reuse a built source snapshot")
    args = parser.parse_args()
    check_memory_rows()
    with tempfile.TemporaryDirectory(prefix="mfvs-focused-bugs-") as temporary:
        directory = Path(temporary)
        tree = args.build_tree.resolve() if args.build_tree else directory / "source"
        if not args.build_tree:
            snapshot("focused", None, tree)
            with ThreadPoolExecutor(max_workers=2) as pool:
                list(pool.map(lambda v: run(["make", f"FVS{v}.setup", "-j4"], tree / "bin"), CASES))
        for variant, cases in CASES.items():
            program = link_driver(tree, variant, directory / variant)
            for case in cases:
                work = directory / f"{variant}-{case}"
                work.mkdir()
                if case == "regent":
                    for name in ("emt01.key", "emt01.tre"):
                        shutil.copy2(tree / "tests/FVSem" / name, work / name)
                result = run([str(program), case], work)
                if case == "summary":
                    check_summary(work / "summary.db", variant)
                    if variant == "on":
                        check_legacy_summary(program, work / "summary.db", directory / "on-legacy")
                        print("PASS on legacy summary: previous row and schema preserved", flush=True)
                print(next(line.strip() for line in result.stdout.splitlines() if line.strip().startswith("PASS")), flush=True)
            if variant in ("bc", "on"):
                check_native_summary(tree, variant, directory / f"{variant}-native")


if __name__ == "__main__":
    main()
