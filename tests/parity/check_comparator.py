"""Prove the comparator fails on real mutations of the recorded IE outputs.

Each negative control updates the recorded SHA256 after its mutation, so a
rejection must come from the comparison itself, not just a stale capture hash.
The model executables and retained original outputs are never modified.
"""

from __future__ import annotations

import argparse
import copy
import hashlib
import json
import math
from pathlib import Path
import re
import shutil
import sqlite3
import subprocess
import sys
import tempfile

from compare_outputs import HEADER_END, normalize_text


COMPARATOR = Path(__file__).with_name("compare_outputs.py")


def mutate(directory: Path, name: str) -> dict:
    summary = directory / "iet01.sum"
    if name in ("model_text_value", "summary_header_count", "text_whitespace", "model_date_text"):
        lines = summary.read_bytes().splitlines(keepends=True)
        if name == "model_text_value":
            index = next(i for i, line in enumerate(lines) if line.startswith(b"1990"))
            lines[index] = lines[index].replace(b"1990", b"1991", 1)
        elif name == "summary_header_count":
            lines[0] = lines[0].replace(b"-999   11", b"-999   12", 1)
        elif name == "text_whitespace":
            lines[1] = b" " + lines[1]
        else:
            # A timestamp-shaped model record is not a recognized header.
            lines.append(b"MODEL OBSERVATION DATE 11-14-2026 04:40:54\n")
        summary.write_bytes(b"".join(lines))
        return {"file": summary.name, "mutation": name}
    db = directory / "iet01_Out.db"
    with sqlite3.connect(db) as connection:
        if name == "database_value_one_ulp":
            rowid, old = connection.execute(
                "SELECT rowid,QMD FROM FVS_Summary WHERE typeof(QMD)='real' ORDER BY rowid LIMIT 1"
            ).fetchone()
            new = math.nextafter(old, math.inf)
            connection.execute("UPDATE FVS_Summary SET QMD=? WHERE rowid=?", (new, rowid))
            return {"file": db.name, "column": "FVS_Summary.QMD", "old": old.hex(), "new": new.hex()}
        if name == "database_schema":
            connection.execute("ALTER TABLE FVS_Summary ADD COLUMN unexpected_model_value REAL")
        elif name == "database_case_link":
            rowid, old = connection.execute("SELECT rowid,CaseID FROM FVS_TreeList ORDER BY rowid LIMIT 1").fetchone()
            new = connection.execute("SELECT CaseID FROM FVS_Cases WHERE CaseID<>? LIMIT 1", (old,)).fetchone()[0]
            connection.execute("UPDATE FVS_TreeList SET CaseID=? WHERE rowid=?", (new, rowid))
        elif name == "database_row_multiplicity":
            connection.execute("INSERT INTO FVS_Summary SELECT * FROM FVS_Summary LIMIT 1")
        else:
            raise ValueError(name)
    return {"file": db.name, "mutation": name}


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--runs", type=Path, required=True)
    parser.add_argument("--evidence-dir", type=Path, required=True)
    args = parser.parse_args()
    args.evidence_dir.mkdir(parents=True, exist_ok=True)
    original = json.loads(args.runs.read_text())
    metadata_checks = []
    cases = ["ie-iet01", "on-Hardwood"]
    cases.extend(case for case in ("ca-cat01", "cr-crt01", "kt-ktt01", "so-sot01", "tt-ttt01", "ws-wst01")
                 if any(run["case"] == case for run in original["runs"]))
    for case in cases:
        run = next(run for run in original["runs"] if run["case"] == case and run["state"] == "original")
        listing = next((Path(run["directory"]) / name).read_bytes()
                       for name in run["outputs"] if name.endswith(".out"))
        headers = sorted(set(line for line in listing.splitlines(keepends=True)
                             if line.lstrip().startswith(b"FOREST VEGETATION SIMULATOR")))
        for header in headers:
            changed = re.sub(rb"\d{2}-\d{2}-\d{4}", b"12-31-2030", header.replace(b"unknown", b"changed"))
            changed = re.sub(rb"\d{2}:\d{2}:\d{2}", b"23:59:59", changed)
            before, counts = normalize_text(header, {})
            after, _ = normalize_text(changed, {})
            variant_changed = (header.replace(b"VERSION ON", b"VERSION XX") if case == "on-Hardwood"
                               else header.replace(b" -- ", b" -X ", 1))
            title, _ = normalize_text(variant_changed, {})
            residual_rejected = True
            end = HEADER_END.search(header.decode("latin1").rstrip("\r\n"))
            residual = end["separator"] if end else ""
            if end:
                # Title characters can survive either gap between metadata
                # fields. Each remains significant, including TT's final S.
                for group in ("separator", "clock_separator"):
                    for offset, character in enumerate(end[group]):
                        if character.isspace():
                            continue
                        position = end.start(group) + offset
                        mutated = header[:position] + b"?" + header[position + 1:]
                        residual_rejected &= normalize_text(mutated, {})[0] != before
            passed = (header != changed and before == after and counts.get("grohed_metadata") == 1
                      and title != before and residual_rejected)
            metadata_checks.append({"case": case, "passed": passed,
                                    "metadata_changes_accepted": before == after,
                                    "title_or_variant_change_rejected": title != before,
                                    "retained_title_suffix": residual,
                                    "retained_clock_suffix": end["clock_separator"] if end else "",
                                    "residual_title_change_rejected": residual_rejected})
    (args.evidence_dir / "metadata-controls.json").write_text(json.dumps(metadata_checks, indent=2) + "\n")
    selected = [run for run in original["runs"] if run["case"] == "ie-iet01" and run["state"] in ("original", "current")]
    checks = []
    names = ["model_text_value", "summary_header_count", "text_whitespace", "model_date_text",
             "database_value_one_ulp", "database_schema", "database_case_link",
             "database_row_multiplicity", "missing_output", "exit_status"]
    with tempfile.TemporaryDirectory(prefix="mfvs-comparator-controls-") as temporary:
        root = Path(temporary)
        for name in names:
            runs = copy.deepcopy(selected)
            left = next(run for run in runs if run["state"] == "original")
            right = next(run for run in runs if run["state"] == "current")
            directory = root / name
            shutil.copytree(right["directory"], directory)
            right["directory"] = str(directory)
            if name == "missing_output":
                (directory / "iet01.trl").unlink()
                del right["outputs"]["iet01.trl"]
                detail = {"file": "iet01.trl", "mutation": "removed"}
            elif name == "exit_status":
                right["returncode"] = 21
                (directory / "returncode.txt").write_text("21\n")
                detail = {"old": 20, "new": 21}
            else:
                detail = mutate(directory, name)
            for filename in right["outputs"]:
                path = directory / filename
                right["outputs"][filename] = {
                    "sha256": hashlib.sha256(path.read_bytes()).hexdigest(), "bytes": path.stat().st_size,
                }
            capture = root / f"{name}-runs.json"
            capture.write_text(json.dumps({"runs": [left, right]}))
            report = root / f"{name}-comparison.json"
            command = [sys.executable, str(COMPARATOR), "--runs", str(capture), "--left", "original",
                       "--right", "current", "--case", "ie-iet01", "--report", str(report)]
            result = subprocess.run(command, capture_output=True, text=True)
            comparison = json.loads(report.read_text()) if report.exists() else None
            rejected = result.returncode == 1 and comparison is not None and not comparison["equal"]
            checks.append({"name": name, "mutation": detail, "returncode": result.returncode,
                           "rejected": rejected, "errors": comparison["errors"] if comparison else None,
                           "stdout": result.stdout, "stderr": result.stderr})
            print(f"{'PASS' if rejected else 'FAIL'}: comparator rejected {name}", flush=True)
    args.evidence_dir.mkdir(parents=True, exist_ok=True)
    (args.evidence_dir / "comparator-controls.json").write_text(json.dumps(checks, indent=2) + "\n")
    return 0 if all(check["rejected"] for check in checks) and all(check["passed"] for check in metadata_checks) else 1


if __name__ == "__main__":
    raise SystemExit(main())
