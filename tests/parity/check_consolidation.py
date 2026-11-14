"""Compare planned routines directly with original F77 and pre-cleanup code.

Fixtures call the selected routine and unchanged numerical helpers. Harness
context supplies debug/habitat controls and the ESADDT option/response service;
C09 instead uses full-engine debug decks for its calibration dependencies.
A fail-fast NATCRS stub covers an unreachable helper (neither KT nor NC maps
to the aspen equation that calls it). Model sources and includes remain
untouched by the fixtures, including the Canadian input bugs.
"""

from __future__ import annotations

import argparse
import gzip
import hashlib
import json
from pathlib import Path
import re
import shlex
import shutil
import subprocess

from prepare_builds import ROOT, sha256


FIXTURES = Path(__file__).parent / "fixtures"
TEMPLATES = {"C01": "c01-timing", "C02": "c02-c03-probability", "C03": "c02-c03-probability",
             "C04": "c04-crown", "C05": "c05-fuel", "C06": "c06-snag",
             "C07": "c07-snag", "C08": "c08-initialization", "C09": None,
             "C10": "c10-addtrees", "C11": "c11-fire-initialization", "C12": "c12-organon"}
HELPERS = {"C04": ("RDPSRT", "PCTILE", "FMCROWW", "ALGSLP"),
           "C05": ("RDPSRT", "ALGSLP", "FMDYN", "FMCHKFWT"),
           "C08": ("LNKINT", "DBINIT"), "C10": ("MYOPEN",)}
STUBS = """
subroutine DBCHK(DEBUG,NAME,N,ICYC)
  implicit none
  logical :: DEBUG,TESTDEBUG
  integer :: N,ICYC
  character(*) :: NAME
  common /TESTDBG/ TESTDEBUG
  DEBUG = TESTDEBUG
end
subroutine PNWET(IWET)
  implicit none
  integer :: IWET,TESTWET,TESTWARN,TESTCOVER
  common /TESTCTX/ TESTWET,TESTWARN,TESTCOVER
  IWET = TESTWET
end
subroutine PNFGS(ICOV)
  implicit none
  integer :: ICOV,TESTWET,TESTWARN,TESTCOVER
  common /TESTCTX/ TESTWET,TESTWARN,TESTCOVER
  ICOV = TESTCOVER
end
subroutine RCDSET(I,L)
  implicit none
  integer :: I,TESTWET,TESTWARN,TESTCOVER
  logical :: L
  common /TESTCTX/ TESTWET,TESTWARN,TESTCOVER
  if (L) TESTWARN = TESTWARN+I
end
subroutine NATCRS
  error stop 'Unexpected aspen equation in the KT/NC fixture'
end
"""

ADDTREES_CONTEXT = """
subroutine OPFIND(N,ACT,NTODO)
  implicit none
  integer :: N,ACT(*),NTODO,TESTMODE,TESTDONE,TESTDELETE
  common /TESTADDT/ TESTMODE,TESTDONE,TESTDELETE
  NTODO = 1
  if (TESTMODE.eq.0) NTODO = 0
end
subroutine OPGET(I,N,IDT,IACTK,NP,PRMS)
  implicit none
  integer :: I,N,IDT,IACTK,NP,TESTMODE,TESTDONE,TESTDELETE
  real :: PRMS(*)
  common /TESTADDT/ TESTMODE,TESTDONE,TESTDELETE
  IDT = 2000
  IACTK = 432
  NP = 2
  PRMS(1) = 1.0
  PRMS(2) = 1.0
  if (TESTMODE.eq.3) PRMS(2) = 2.0
end
subroutine OPGETC(I,CMDLN)
  implicit none
  integer :: I
  character(*) :: CMDLN
  CMDLN = 'python3 external_model.py'
end
subroutine OPRDAT(I,KODE)
  implicit none
  integer :: I,KODE
  KODE = 0
end
subroutine OPDONE(I,IDT)
  implicit none
  integer :: I,IDT,TESTMODE,TESTDONE,TESTDELETE
  common /TESTADDT/ TESTMODE,TESTDONE,TESTDELETE
  TESTDONE = TESTDONE+1
end
subroutine OPDEL1(I)
  implicit none
  integer :: I,TESTMODE,TESTDONE,TESTDELETE
  common /TESTADDT/ TESTMODE,TESTDONE,TESTDELETE
  TESTDELETE = TESTDELETE+1
end
"""


def initialization_driver(template: str, task: dict) -> str:
    """Capture every assigned model variable, including complete arrays.

    Derive the inventory from canonical sources, not expected numeric values.
    Original/current/candidate implementations remain the independent oracles.
    """
    paths = [ROOT / task["canonical"]]
    if task["id"] == "C08":
        paths.extend((ROOT / "base/lnkint.f90", ROOT / "base/dbinit.f90"))
    includes = []
    names = set()
    for path in paths:
        code = "\n".join(line.split("!")[0] for line in path.read_text().splitlines())
        for name in re.findall(r"include '([^']+)'", code):
            if name not in includes:
                includes.append(name)
        names.update(re.findall(r"^\s*(\w+)(?:\([^\n=]*\))?\s*=", code, re.M))
    names.difference_update(("I", "J", "K"))
    return template.replace("@MODEL_INCLUDES@", "\n".join(f"  include '{name}'" for name in includes)).replace(
        "@STATE_WRITES@", "\n".join(f"        write(31) '{name.upper()}',{name}" for name in sorted(names)))


def fixed_form(source: str) -> str:
    """Format only the generated driver, never the original model/includes.

    The original .F77 includes require a fixed-form driver. Join free-form
    continuations, then split at column 72 with a fixed continuation marker.
    """
    lines = []
    pending = ""
    for line in source.splitlines():
        if not line.strip() or line.lstrip().startswith("!"):
            continue
        code = line.strip()
        pending += code.lstrip("&").rstrip("&")
        if code.endswith("&"):
            continue
        chunks = [pending[i:i + 66] for i in range(0, len(pending), 66)]
        lines.extend(("      " if i == 0 else "     &") + chunk for i, chunk in enumerate(chunks))
        pending = ""
    if pending:
        raise ValueError("Unterminated driver continuation")
    return "\n".join(lines) + "\n"


def normalize_tree(source: str) -> str:
    """Canonicalize generated temporary IDs while preserving their relationships."""
    names = {}
    def rename(match: re.Match) -> str:
        return names.setdefault(match[0], f"TEMP{len(names)}")
    return re.sub(r"\bD\.\d+\b", rename, source)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--evidence-dir", type=Path, required=True)
    parser.add_argument("--tasks", nargs="+", choices=tuple(TEMPLATES), default=list(TEMPLATES))
    parser.add_argument("--variants", nargs="+")
    args = parser.parse_args()
    evidence = args.evidence_dir.resolve()
    builds = json.loads((evidence / "builds.json").read_text())
    plan = json.loads((evidence / "plan.json").read_text())
    work = Path(builds["states"]["original"]["snapshot"]).parent
    compiler = builds["tools"]["fortran"]["path"]
    flags = shlex.split(builds["fflags"])
    report = {"flags": flags, "supplemental_runtime_check": "-fcheck=bounds", "runs": [], "comparisons": []}
    previous = evidence / "targeted.json"
    if previous.exists():
        report = json.loads(previous.read_text())
        report.pop("passed", None)
    tree_dir = evidence / "routine-trees"
    tree_dir.mkdir(exist_ok=True)
    for task in plan["tasks"]:
        if task["id"] not in args.tasks:
            continue
        if TEMPLATES[task["id"]] is None:
            # C09 reaches the actual CRATET diagnostics through full-engine
            # debug decks, preserving its real calibration dependencies.
            report.setdefault("integrated_targeted_tasks", []).append(task["id"])
            continue
        for variant in task["consumers"]:
            if args.variants and variant not in args.variants:
                continue
            case = f"{task['id']}-{variant}"
            records = []
            for state, identity in builds["states"].items():
                snapshot = Path(identity["snapshot"])
                manifest = snapshot / "bin" / f"FVS{variant}_sourceList.txt"
                paths = [(manifest.parent / entry).resolve() for entry in manifest.read_text().split()]
                by_name = {path.stem.upper(): path for path in paths}
                source = by_name[task["routine"]]
                directory = work / "targeted-build" / state / case
                directory.mkdir(parents=True, exist_ok=True)
                headers = directory / "includes"
                headers.mkdir(exist_ok=True)
                classification = snapshot / "bin/fortran_includes.txt"
                include_names = set(classification.read_text().split()) if classification.exists() else set()
                copied = []
                for path in paths:
                    relative = str(path.relative_to(snapshot))
                    if path.suffix.lower() in (".f77", ".inc", ".h") or relative in include_names:
                        shutil.copy2(path, headers / path.name)
                        copied.append(path)
                template = FIXTURES / (TEMPLATES[task["id"]] + ".f90.in")
                driver = template.read_text().replace("@ROUTINE@", task["routine"]).replace("@VARIANT@", variant.upper())
                if task["id"] in ("C08", "C11"):
                    driver = initialization_driver(driver, task)
                for include in re.findall(r"include '([^']+)'", driver):
                    actual = by_name[Path(include).stem.upper()]
                    driver = driver.replace(f"'{include}'", f"'{actual.name}'")
                fixed = state == "original"
                driver_path = directory / ("driver.f" if fixed else "driver.f90")
                driver_path.write_text(fixed_form(driver) if fixed else driver)
                stub_path = directory / "context.f90"
                stub_path.write_text(STUBS + (ADDTREES_CONTEXT if task["id"] == "C10" else ""))
                helpers = [by_name[name] for name in HELPERS.get(task["id"], ())]
                target = directory / "target.o"
                compile_command = [compiler, *flags, "-I", str(headers), "-fcheck=bounds", "-c", str(source), "-o", str(target)]
                link_command = [compiler, *flags, "-fcheck=bounds", "-I", str(headers), str(driver_path),
                                str(stub_path), *map(str, helpers), str(target), "-o", str(directory / "check")]
                compile_log = subprocess.run(compile_command, cwd=directory, capture_output=True)
                link_log = (subprocess.run(link_command, cwd=directory, capture_output=True)
                            if compile_log.returncode == 0 else None)
                log_bytes = compile_log.stdout + compile_log.stderr
                if link_log:
                    log_bytes += link_log.stdout + link_log.stderr
                log = evidence / f"targeted-{state}-{case}.log.gz"
                log.write_bytes(gzip.compress(log_bytes, mtime=0))
                if compile_log.returncode or link_log is None or link_log.returncode:
                    raise RuntimeError(f"Targeted build failed: {state}/{case}; {log}")
                run_dir = work / "runs/targeted" / state / case
                run_dir.mkdir(parents=True, exist_ok=False)
                shutil.copy2(driver_path, run_dir / driver_path.name)
                shutil.copy2(stub_path, run_dir / stub_path.name)
                extra_inputs = {}
                if task["id"] == "C10":
                    external = run_dir / "external_model.py"
                    external.write_text("from pathlib import Path\nimport sys\nPath(sys.argv[1]).with_suffix('.es2').write_text('         1\\n')\n")
                    extra_inputs[external.name] = sha256(external)
                with (run_dir / "stdout.txt").open("wb") as out, (run_dir / "stderr.txt").open("wb") as err:
                    result = subprocess.run([str(directory / "check")], cwd=run_dir, stdout=out, stderr=err, timeout=60)
                (run_dir / "returncode.txt").write_text(f"{result.returncode}\n")
                if result.returncode:
                    raise RuntimeError(f"Targeted execution failed: {state}/{case}; {run_dir}")
                # Record the compiler's actual-build-flag routine body too.
                # This is additional evidence, not a substitute for the runs.
                tree_command = [compiler, *flags, "-I", str(headers), "-fdump-tree-original", "-c", str(source), "-o", str(directory / "tree.o")]
                subprocess.run(tree_command, cwd=directory, capture_output=True, check=True)
                dump = next(directory.glob("tree*.original"))
                normalized = normalize_tree(dump.read_text())
                tree_path = tree_dir / f"{state}-{case}.txt"
                tree_path.write_text(normalized)
                output_names = ["state.bin", "stdout.txt", "stderr.txt", "returncode.txt"]
                output_names.extend(path.name for path in sorted(run_dir.glob("*.es[12]")))
                outputs = {name: sha256(run_dir / name) for name in output_names}
                record = {"state": state, "case": case, "task": task["id"], "variant": variant,
                          "directory": str(run_dir), "source": str(source.relative_to(snapshot)),
                          "source_inputs": {str(path.relative_to(snapshot)): sha256(path) for path in [source, *helpers, *copied]},
                          "compile_command": compile_command, "link_command": link_command,
                          "driver_sha256": sha256(driver_path), "template_sha256": sha256(template),
                          "context_sha256": sha256(stub_path), "log": log.name, "log_sha256": sha256(log),
                          "extra_inputs": extra_inputs,
                          "captured_variables": re.findall(r"write\(31\)\s*'([^']+)'", driver, re.I),
                          "returncode": result.returncode, "binary_sha256": sha256(directory / "check"),
                          "outputs": outputs, "state_bytes": (run_dir / "state.bin").stat().st_size,
                          "routine_tree": str(tree_path.relative_to(evidence)), "tree_sha256": sha256(tree_path)}
                records.append(record)
                report["runs"].append(record)
            for left, right in ((0, 1), (0, 2), (1, 2)):
                a, b = records[left], records[right]
                equal = a["outputs"] == b["outputs"]
                report["comparisons"].append({"case": case, "left": a["state"], "right": b["state"],
                    "exact_outputs_equal": equal, "routine_trees_equal": a["tree_sha256"] == b["tree_sha256"]})
                if not equal:
                    (evidence / "targeted.json").write_text(json.dumps(report, indent=2) + "\n")
                    raise RuntimeError(f"Targeted outputs differ: {case}: {a['state']}/{b['state']}")
            (evidence / "targeted.json").write_text(json.dumps(report, indent=2) + "\n")
            print(f"PASS {case}: original/current/candidate raw state bytes and diagnostics", flush=True)
    report["passed"] = True
    (evidence / "targeted.json").write_text(json.dumps(report, indent=2) + "\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
