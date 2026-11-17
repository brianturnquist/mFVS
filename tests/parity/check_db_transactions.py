"""Compare DBSREFERENCE's batched rows with the pre-batching routine.

The regression decks only reach DBSREFERENCE with no transaction open and
every statement succeeding. db_transaction_cases.f90 drives both routines
through the repository's SQLite wrapper in the other states: an outer
transaction opened by the caller or left open by a failed DBSFMCANPR (bug
66), a prepare failure part-way through the species loop, and a commit
refused because another connection is reading. Printed observations
(autocommit state, outstanding statements, rows each connection sees, close
codes) and the final typed rows must match exactly. A few expected facts are
asserted, so that each case is known to reach its path. Test-only controls:
DBSCASE is a stub (CASEID is preset), and the busy case removes the busy
handler so a refusal is immediate. No generated files remain in the checkout.
"""

from pathlib import Path
import re
import shutil
import subprocess
import sys
import tempfile

sys.dont_write_bytecode = True
from compare_outputs import database

ROOT = Path(__file__).resolve().parents[2]
BASELINE = "f1a2538e58dfac63198f3d6b2047e27f87a695a1"  # before the batch
CASES = ("normal", "outer", "midfail", "busy", "leaked")
LINE = re.compile(r"(?P<label>[^:]+): autocommit (?P<auto>\d+), outstanding statements (?P<stmts>\d+), "
                  r"rows (?P<rows>-?\d+), rows seen by another connection (?P<seen>-?\d+)")
# (case, label): (autocommit, rows, rows seen by another connection), with
# IE's 23 species less the blank one giving 22 rows per stand.
EXPECTED = {
    ("normal", "first stand"): (1, 22, 22),
    ("normal", "second stand"): (1, 44, 44),
    ("outer", "inside caller transaction"): (0, 44, 22),
    ("outer", "after caller rollback"): (1, 22, 22),
    ("midfail", "after prepare failure"): (1, 2, 2),
    ("busy", "while another connection reads"): (1, 22, 22),
    ("busy", "after the read ends"): (1, 22, 22),
    ("leaked", "after failed canopy-profile writer"): (0, -1, -1),
    ("leaked", "after reference rows"): (0, 22, -1),
}
# FVS_InvReference rows in out.db after the program exits (None: no table).
# In "leaked" nothing commits the transaction DBSFMCANPR left open.
FINAL_ROWS = {"normal": 44, "outer": 22, "midfail": 2, "busy": 22, "leaked": None}
PROBES = r"""
/* Test-only probes of the wrapper's connections. */
#include "sqlite3.h"
extern sqlite3 *dbset[];
int fx_autocommit_(int *db) { return sqlite3_get_autocommit(dbset[*db]); }
int fx_nobusy_(int *db) { return sqlite3_busy_handler(dbset[*db], 0, 0); }
long long fx_memory_(void) { return sqlite3_memory_used(); }
int fx_statements_(int *db)
{
  int n = 0;
  sqlite3_stmt *s = 0;
  while ((s = sqlite3_next_stmt(dbset[*db], s))) n++;
  return n;
}
int fx_count_(int *db)
{
  sqlite3_stmt *s;
  int n = -1;
  if (sqlite3_prepare_v2(dbset[*db], "select count(*) from FVS_InvReference", -1, &s, 0)) return -1;
  if (sqlite3_step(s) == SQLITE_ROW) n = sqlite3_column_int(s, 0);
  sqlite3_finalize(s);
  return n;
}
"""


def build(directory: Path) -> Path:
    original = subprocess.check_output(
        ["git", "-C", str(ROOT), "show", f"{BASELINE}:vdbsqlite/dbsreference.f90"], text=True)
    if original.count("subroutine DBSREFERENCE") != 1 or "Begin;" in original:
        raise SystemExit("unexpected baseline DBSREFERENCE")
    (directory / "original.f90").write_text(
        original.replace("subroutine DBSREFERENCE", "subroutine BASE_DBSREFERENCE"))
    (directory / "probes.c").write_text(PROBES)
    manifest = ROOT / "bin" / "FVSie_sourceList.txt"
    selected = {(manifest.parent / name).name: (manifest.parent / name).resolve()
                for name in manifest.read_text().split()}
    for name in ("PRGPRM.f90", "DBSCOM.f90", "CONTRL.f90", "PLOT.f90", "VOLSTD.f90",
                 "dbsreference.f90", "dbsfmcanpr.f90", "sqlite3.c", "fvsqlite3.c"):
        shutil.copy2(selected[name], directory / name)
    shutil.copy2(ROOT / "dbsqlite" / "sqlite3.h", directory / "sqlite3.h")
    shutil.copy2(Path(__file__).with_name("db_transaction_cases.f90"), directory / "driver.f90")
    # The Makefile's SQLite settings on Unix; DBSCASE comes from the driver's stub.
    subprocess.run(["gcc", "-c", "-O2", "-DANSI", "-DCMPgcc", "-Dunix", "-DSQLITE_THREADSAFE=0",
                    "-DSQLITE_OMIT_LOAD_EXTENSION", "sqlite3.c", "fvsqlite3.c", "probes.c"],
                   cwd=directory, check=True)
    subprocess.run(["gfortran", "-cpp", "-DCMPgcc", "-g", "-fcheck=all", "-fbacktrace",
                    "-ffpe-trap=invalid,zero,overflow", "original.f90", "dbsreference.f90",
                    "dbsfmcanpr.f90", "driver.f90", "sqlite3.o", "fvsqlite3.o", "probes.o",
                    "-o", "check"], cwd=directory, check=True)
    return directory / "check"


def main() -> int:
    failures = 0
    with tempfile.TemporaryDirectory(prefix="mfvs-db-transactions-") as temporary:
        directory = Path(temporary)
        program = build(directory)
        for case in CASES:
            observed = {}
            problems = []
            for routine in ("base", "current"):
                run = directory / f"{case}-{routine}"
                run.mkdir()
                # Each case takes well under a second. A lock left behind
                # makes readers retry for 20 seconds per call instead.
                try:
                    result = subprocess.run([str(program), case, routine], cwd=run, check=True,
                                            capture_output=True, text=True, timeout=60)
                except (subprocess.CalledProcessError, subprocess.TimeoutExpired) as error:
                    problems.append(f"{routine} routine: {error}")
                    break
                lines = result.stdout.splitlines()
                rows = database(run / "out.db")
                del rows["aliases"]
                observed[routine] = ([line for line in lines if not line.startswith("memory")], rows,
                                     [line for line in lines if line.startswith("memory")])
            if problems:
                failures += 1
                print(f"FAIL {case}\n     ! " + "\n     ! ".join(problems))
                continue
            if observed["base"][:2] != observed["current"][:2]:
                problems.append("observations or database rows differ from the pre-batching routine")
            for line in observed["current"][0]:
                match = LINE.fullmatch(line)
                if match and (case, match["label"]) in EXPECTED:
                    actual = (int(match["auto"]), int(match["rows"]), int(match["seen"]))
                    if actual != EXPECTED[case, match["label"]]:
                        problems.append(f"{match['label']}: expected {EXPECTED[case, match['label']]}, "
                                        f"observed {actual}")
            table = observed["current"][1]["tables"].get("FVS_InvReference")
            if (table and table["rows"]) != FINAL_ROWS[case]:
                problems.append(f"out.db holds {table and table['rows']} reference rows, "
                                f"expected {FINAL_ROWS[case]}")
            reached = {match["label"] for match in map(LINE.fullmatch, observed["current"][0]) if match}
            problems += [f"{label}: not reported" for (name, label) in EXPECTED
                         if name == case and label not in reached]
            failures += bool(problems)
            print(("FAIL " if problems else "PASS ") + case)
            for line in observed["current"][0] + observed["current"][2]:
                print(f"       {line}")
            for problem in problems:
                print(f"     ! {problem}")
            if problems:
                for line in observed["base"][0]:
                    print(f"  base {line}")
    print(("FAIL" if failures else "PASS") + f" {len(CASES) - failures}/{len(CASES)} DBSREFERENCE cases")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
