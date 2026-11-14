"""Compare recorded FVS outputs exactly, with narrowly identified run metadata.

Text whitespace, numbers, file membership, process status, SQLite schema, value
types, and every database row are significant. SQLite storage order is ignored;
duplicate rows and CaseID relationships are retained. Case IDs are mapped using
the stable FVS_Cases descriptors, never dropped from child tables.
"""

from __future__ import annotations

import argparse
from collections import Counter
import hashlib
import json
from pathlib import Path
import re
import sqlite3


UUID = re.compile(r"[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}")
DATE = r"\d{2}-\d{2}-\d{4}"
TIME = r"\d{2}:\d{2}:\d{2}"
# GROHED backtracks to fixed columns for RV and date. Long CA/CR (and
# similar) titles can leave one or two title characters between those fields.
# A long TT title also survives in the two skipped columns before the time.
# Capture and RETAIN both gaps; only revision/date/time are metadata.
HEADER_END = re.compile(rf"RV:(?P<revision>.{{10}})(?P<separator>.{{0,2}})(?P<date>{DATE})(?P<clock_separator>.{{2}})(?P<time>{TIME})$")
SUMMARY_META = re.compile(rf"(?P<prefix>^-999.*? [A-Z]{{2}} )({DATE}) ({TIME}) (.{{10}})")
CASE_META = {"Version", "RV", "RunDateTime"}


def digest(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def value_token(value: object) -> list:
    """Preserve SQLite value types and exact floating-point representations."""
    if value is None:
        return ["null"]
    if isinstance(value, int):
        return ["integer", value]
    if isinstance(value, float):
        return ["real", value.hex()]
    if isinstance(value, bytes):
        return ["blob", value.hex()]
    if isinstance(value, str):
        return ["text", value]
    raise TypeError(type(value))


def quote_identifier(name: str) -> str:
    return '"' + name.replace('"', '""') + '"'


def case_ids(connection: sqlite3.Connection) -> dict[str, str]:
    found = connection.execute("SELECT 1 FROM sqlite_master WHERE type='table' AND name='FVS_Cases'").fetchone()
    if not found:
        return {}
    cursor = connection.execute('SELECT * FROM "FVS_Cases" ORDER BY rowid')
    columns = [c[0] for c in cursor.description]
    if "CaseID" not in columns:
        raise ValueError("FVS_Cases lacks CaseID")
    index = columns.index("CaseID")
    records = []
    for position, row in enumerate(cursor):
        identity = row[index]
        if not isinstance(identity, str) or not UUID.fullmatch(identity):
            raise ValueError(f"Unsupported FVS_Cases identity: {identity!r}")
        stable = [(column, value_token(value)) for column, value in zip(columns, row)
                  if column not in CASE_META | {"CaseID"}]
        records.append((json.dumps(stable, ensure_ascii=True), position, identity))
    identities = [identity for stable, position, identity in records]
    if len(set(identities)) != len(identities):
        raise ValueError("Duplicate FVS_Cases identity")
    # Descriptors anchor each stand/case. A stable occurrence ordering handles
    # repeated identical cases without erasing multiplicity or child links.
    return {identity: f"<CASE-{number:04d}>"
            for number, (stable, position, identity) in enumerate(sorted(records), 1)}


def normalize_text(data: bytes, aliases: dict[str, str]) -> tuple[bytes, dict[str, int]]:
    """Mask metadata fields in their record context; keep all other bytes."""
    counts = Counter()
    result = []
    for raw in data.decode("latin1").splitlines(keepends=True):
        # Retain the original line ending, including the absence of a newline.
        line = raw.rstrip("\r\n")
        ending = raw[len(line):]
        if line.lstrip().startswith("FOREST VEGETATION SIMULATOR") and HEADER_END.search(line):
            if ") -- ONTARIO" in line:
                line = re.sub(r"(?<=VERSION [A-Z]{2} \().{32}(?=\) -- ONTARIO)",
                              "<VERSION>".ljust(32), line, count=1)
            else:
                line = re.sub(r"(?<=VERSION ).{32}", "<VERSION>".ljust(32), line, count=1)
            line = HEADER_END.sub(lambda m: "RV:" + "<RV>".ljust(10) + m["separator"]
                                 + "<RUN-DATE>" + m["clock_separator"] + "<RUN-TIME>", line)
            counts["grohed_metadata"] += 1
        if SUMMARY_META.search(line):
            line = SUMMARY_META.sub(lambda m: m["prefix"] + "<RUN-DATE> <RUN-TIME> " + "<RV>".ljust(10), line, count=1)
            counts["summary_metadata"] += 1
        for identity, alias in aliases.items():
            occurrences = line.count(identity)
            if occurrences:
                line = line.replace(identity, alias)
                counts["case_identity"] += occurrences
        result.append(line + ending)
    return "".join(result).encode("latin1"), dict(counts)


def database(path: Path) -> dict:
    """Read every schema object and row, without changing the database file."""
    with sqlite3.connect(path.resolve().as_uri() + "?mode=ro", uri=True) as connection:
        aliases = case_ids(connection)
        schema = connection.execute(
            "SELECT type,name,tbl_name,sql FROM sqlite_master ORDER BY type,name"
        ).fetchall()
        result = {"schema": schema, "user_version": connection.execute("PRAGMA user_version").fetchone()[0],
                  "tables": {}, "aliases": aliases}
        for name, in connection.execute("SELECT name FROM sqlite_master WHERE type='table' ORDER BY name").fetchall():
            cursor = connection.execute(f"SELECT * FROM {quote_identifier(name)}")
            columns = [c[0] for c in cursor.description]
            rows = []
            metadata = Counter()
            for row in cursor:
                normalized = []
                for column, value in zip(columns, row):
                    if name == "FVS_Cases" and column in CASE_META:
                        # Preserve NULL/type information even for permitted
                        # metadata, so its disappearance is still observable.
                        if value is not None:
                            value = "<" + column + ">" if isinstance(value, str) else value
                        metadata[column] += 1
                    elif column == "CaseID" and value is not None:
                        if value not in aliases:
                            raise ValueError(f"Unresolved CaseID in {name}: {value!r}")
                        value = aliases[value]
                    normalized.append(value_token(value))
                rows.append(json.dumps(normalized, separators=(",", ":"), ensure_ascii=True))
            rows.sort()
            serial = "\n".join(rows).encode()
            result["tables"][name] = {"columns": columns, "rows": len(rows), "sha256": digest(serial),
                                      "metadata": dict(metadata)}
    return result


def compare_directories(left: Path, right: Path, left_files: list[str], right_files: list[str]) -> dict:
    errors = []
    files = {}
    a_names, b_names = set(left_files), set(right_files)
    if a_names != b_names:
        errors.append({"kind": "inventory", "left_only": sorted(a_names - b_names),
                       "right_only": sorted(b_names - a_names)})
    aliases = [{}, {}]
    dbs = {}
    for name in sorted(a_names & b_names):
        a, b = left / name, right / name
        if not a.is_file() or not b.is_file():
            errors.append({"kind": "missing_file", "file": name})
            continue
        if a.read_bytes().startswith(b"SQLite format 3\0") or b.read_bytes().startswith(b"SQLite format 3\0"):
            da, db = database(a), database(b)
            for target, mapping in zip(aliases, (da["aliases"], db["aliases"])):
                for key, value in mapping.items():
                    if key in target and target[key] != value:
                        raise ValueError("Conflicting case identities between databases")
                    target[key] = value
            # UUID spelling is the only excluded part of the alias maps.
            del da["aliases"], db["aliases"]
            dbs[name] = (da, db)
    for name in sorted(a_names & b_names):
        a, b = left / name, right / name
        if not a.is_file() or not b.is_file():
            continue
        if name in dbs:
            da, db = dbs[name]
            same = da == db
            files[name] = {"kind": "sqlite", "equal": same, "left": da, "right": db}
            if not same:
                errors.append({"kind": "sqlite", "file": name})
        else:
            na, ca = normalize_text(a.read_bytes(), aliases[0])
            nb, cb = normalize_text(b.read_bytes(), aliases[1])
            same = na == nb
            files[name] = {"kind": "bytes/text", "equal": same,
                           "left_sha256": digest(na), "right_sha256": digest(nb),
                           "left_normalizations": ca, "right_normalizations": cb}
            if not same:
                aa, bb = na.splitlines(keepends=True), nb.splitlines(keepends=True)
                first = next((i for i, pair in enumerate(zip(aa, bb), 1) if pair[0] != pair[1]), min(len(aa), len(bb)) + 1)
                errors.append({"kind": "bytes/text", "file": name, "first_differing_line": first})
    return {"equal": not errors, "errors": errors, "files": files}


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--runs", type=Path, required=True)
    parser.add_argument("--left", required=True)
    parser.add_argument("--right", required=True)
    parser.add_argument("--case", required=True)
    parser.add_argument("--report", type=Path, required=True)
    parser.add_argument("--artifacts-root", type=Path,
                        help="Extracted runs/ directory when replaying retained artifacts")
    args = parser.parse_args()
    runs = json.loads(args.runs.read_text())
    entries = {(run["state"], run["case"]): run for run in runs["runs"]}
    left, right = entries[args.left, args.case], entries[args.right, args.case]
    if args.artifacts_root:
        left, right = dict(left), dict(right)
        for run in (left, right):
            run["directory"] = str(args.artifacts_root / run["state"] / run["case"])
    # Verify the capture itself before interpreting or normalizing its bytes.
    for run in (left, right):
        for name, identity in run["outputs"].items():
            path = Path(run["directory"]) / name
            if not path.is_file() or digest(path.read_bytes()) != identity["sha256"]:
                raise ValueError(f"Recorded output changed or disappeared: {path}")
    result = compare_directories(Path(left["directory"]), Path(right["directory"]),
                                 list(left["outputs"]), list(right["outputs"]))
    result.update({"case": args.case, "left": args.left, "right": args.right,
                   "returncodes": [left["returncode"], right["returncode"]]})
    if left["returncode"] != right["returncode"]:
        result["errors"].append({"kind": "returncode"})
        result["equal"] = False
    args.report.parent.mkdir(parents=True, exist_ok=True)
    args.report.write_text(json.dumps(result, indent=2) + "\n")
    print(f"{'PASS' if result['equal'] else 'FAIL'} {args.case}: {args.left} vs {args.right}; {len(result['files'])} artifacts")
    return 0 if result["equal"] else 1


if __name__ == "__main__":
    raise SystemExit(main())
