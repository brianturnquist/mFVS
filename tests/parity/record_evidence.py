"""Retain source integrity, BC/ON boundary evidence, and raw replay artifacts.

This verifies that the original archive changed only in its documented link
manifests. Hidden Canadian interfaces are inspected statically, without fixing
or invoking their undefined argument paths after the keyword-input failure.
"""

from __future__ import annotations

import argparse
import gzip
import hashlib
import json
from pathlib import Path
import re
import shutil
import subprocess
import tarfile
import tempfile

from prepare_builds import ROOT, selected_inputs, sha256


def tree_integrity(repo: Path, revision: str, snapshot: Path, allowed: set[str]) -> dict:
    entries = subprocess.check_output(["git", "-C", str(repo), "ls-tree", "-rz", revision]).split(b"\0")
    mismatches = []
    checked = 0
    for entry in entries:
        if not entry:
            continue
        metadata, name = entry.split(b"\t", 1)
        mode, kind, expected = metadata.split()
        if kind != b"blob":
            continue
        relative = name.decode()
        path = snapshot / relative
        if mode == b"120000":
            data = str(path.readlink()).encode()
        else:
            data = path.read_bytes()
        actual = hashlib.sha1(b"blob " + str(len(data)).encode() + b"\0" + data).hexdigest()
        checked += 1
        if actual != expected.decode():
            mismatches.append(relative)
    if set(mismatches) != allowed:
        raise RuntimeError(f"Unexpected archive changes: {mismatches}; expected {sorted(allowed)}")
    return {"revision": revision, "blobs_checked": checked, "modified_paths": mismatches,
            "allowed_modified_paths": sorted(allowed), "passed": True}


def signature(path: Path, kind: str, name: str) -> dict:
    lines = []
    fixed = path.suffix.lower() == ".f"
    for raw in path.read_text(errors="replace").splitlines():
        if fixed:
            if raw[:1].upper() in ("C", "*", "!"):
                continue
            raw = raw[6:72]
        lines.append(raw.split("!")[0].replace("&", " "))
    text = " ".join(lines)
    match = re.search(rf"\b{kind}\s+{name}\s*\(", text, re.I)
    if not match:
        if kind in ("subroutine", "call") and re.search(rf"\b{kind}\s+{name}\b", text, re.I):
            return {"kind": kind, "name": name, "argument_count": 0, "arguments": [],
                    "source_sha256": sha256(path)}
        raise ValueError(f"Cannot locate {kind} {name} in {path}")
    start = match.end()
    depth = 1
    arguments = []
    field = ""
    for character in text[start:]:
        if character == "(":
            depth += 1
        elif character == ")":
            depth -= 1
            if not depth:
                arguments.append(field.strip())
                break
        if character == "," and depth == 1:
            arguments.append(field.strip())
            field = ""
        else:
            field += character
    return {"kind": kind, "name": name, "argument_count": len(arguments), "arguments": arguments,
            "source_sha256": sha256(path)}


def inspect_signature(snapshot: Path, relative: str, kind: str, name: str, expected: int) -> dict:
    """Keep live interface checks strict after the unused metric reader retires.

    D07 removes one historical comparison source, not its selected replacement
    or its callers. Only that exact unselected file may be absent. A missing
    live source or a manifest that selects the retired reader still fails.
    """
    path = snapshot / relative
    if relative == "metric/dbsqlite/dbstreesin.f90" and not path.exists():
        manifests = sorted((snapshot / "bin").glob("FVS*_sourceList.txt"))
        if len(manifests) != 24:
            raise RuntimeError("Reassess build selection before accepting a retired interface")
        for manifest in manifests:
            if any((manifest.parent / entry).resolve() == path.resolve()
                   for entry in manifest.read_text().split()):
                raise RuntimeError(f"Missing selected interface: {relative}")
        return {"file": relative, "status": "retired_unselected", "kind": kind, "name": name,
                "former_argument_count": expected, "selected_by": [],
                "replacement": "dbsqlite/dbstreesin.f90"}
    found = signature(path, kind, name)
    if found["argument_count"] != expected:
        raise RuntimeError(f"Unexpected interface: {relative}: {found}")
    return {"file": relative, **found}


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--evidence-dir", type=Path, required=True)
    args = parser.parse_args()
    evidence = args.evidence_dir.resolve()
    builds = json.loads((evidence / "builds.json").read_text())
    runs = json.loads((evidence / "runs.json").read_text())
    states = builds["states"]
    integrity = {}
    for state in ("original", "current"):
        snapshot = Path(states[state]["snapshot"])
        allowed = {"bin/FVSbc_sourceList.txt", "bin/FVSon_sourceList.txt"} if state == "original" else set()
        integrity[state] = tree_integrity(ROOT, states[state]["revision"], snapshot, allowed)
        integrity[state + "_nvel"] = tree_integrity(ROOT / "volume/NVEL", states[state]["nvel"], snapshot / "volume/NVEL", set())
    for variant in ("ie", "bc", "on"):
        recorded = next(entry["inputs"] for entry in builds["builds"]
                        if entry["state"] == "candidate" and entry["variant"] == variant)
        if selected_inputs(ROOT, variant) != recorded:
            raise RuntimeError(f"Working model/build inputs changed since capture: {variant}")
    integrity["working_selected_inputs_still_match_candidate"] = True
    # Reapply the retained patch to the pristine archived manifests. This
    # checks that the receipt can reconstruct the exact reference build inputs.
    with tempfile.TemporaryDirectory(prefix="mfvs-reference-patch-") as temporary:
        patch_root = Path(temporary)
        original_root = Path(states["original"]["snapshot"])
        with tarfile.open(original_root.parent / "original.tar") as stream:
            for relative in ("bin/FVSbc_sourceList.txt", "bin/FVSon_sourceList.txt"):
                stream.extract(relative, patch_root, filter="data")
        command = [shutil.which("patch"), "--batch", "-p1", "-i", str(evidence / "reference-build.patch")]
        patched = subprocess.run(command, cwd=patch_root, capture_output=True, text=True, check=True)
        for relative in ("bin/FVSbc_sourceList.txt", "bin/FVSon_sourceList.txt"):
            if sha256(patch_root / relative) != sha256(original_root / relative):
                raise RuntimeError(f"Reference patch replay changed: {relative}")
        integrity["reference_patch_replay"] = {
            "passed": True, "patch_sha256": sha256(evidence / "reference-build.patch"),
            "stdout": patched.stdout,
        }
    (evidence / "source-integrity.json").write_text(json.dumps(integrity, indent=2) + "\n")

    interfaces = {}
    for state in ("original", "current"):
        snapshot = Path(states[state]["snapshot"])
        suffix = ".f" if state == "original" else ".f90"
        signatures = []
        for stem, kind, name, expected in (
            ("dbsqlite/dbstreesin", "subroutine", "DBSTREESIN", 33),
            ("metric/base/intree", "call", "DBSTREESIN", 30),
            ("metric/dbsqlite/dbstreesin", "subroutine", "DBSTREESIN", 30),
            ("canada/fire/bc/fmsvol", "entry", "FMSVL2", 8),
            ("fire/base/fmsvol", "entry", "FMSVL2", 10),
            ("metric/fire/base/fmdout", "call", "FMSVL2", 8),
            ("base/svcwd", "call", "FMSVL2", 10),
        ):
            relative = stem + suffix
            signatures.append(inspect_signature(snapshot, relative, kind, name, expected))
        declarations = []
        for stem in ("base/keyrdr", "canada/bc/initre", "metric/vbase/initre", "metric/dbsqlite/dbsin", "metric/newmist/misin"):
            path = snapshot / (stem + suffix)
            for number, line in enumerate(path.read_text(errors="replace").splitlines(), 1):
                if re.match(r"\s*(?:real|logical|character|dimension)\b", line, re.I) and re.search(r"\b(?:ARRAY|KARD|LNOTBK)\((?:7|12)\)", line, re.I):
                    declarations.append({"file": stem + suffix, "line": number, "declaration": line.strip(),
                                         "source_sha256": sha256(path)})
        interfaces[state] = {"signatures": signatures, "keyword_declarations": declarations}
    (evidence / "static-interfaces.json").write_text(json.dumps(interfaces, indent=2) + "\n")

    boundaries = []
    for run in runs["runs"]:
        if runs["cases"][run["case"]]["variant"] not in ("bc", "on"):
            continue
        directory = Path(run["directory"])
        listing_name = next(name for name in run["outputs"] if name.endswith(".out"))
        listing = (directory / listing_name).read_bytes()
        record = {"state": run["state"], "case": run["case"], "returncode": run["returncode"],
                  "invalid_keyword_errors": listing.count(b"INVALID KEYWORD WAS SPECIFIED"),
                  "eight_nul_sequences": listing.count(b"\0" * 8),
                  "generated_summary_files": [name for name in run["outputs"] if name.endswith(".sum")],
                  "generated_databases": [name for name in run["outputs"] if name.endswith(".db")],
                  "simulation_parity": "blocked", "listing_sha256": sha256(directory / listing_name)}
        if record["returncode"] != 20 or not record["invalid_keyword_errors"] or record["generated_summary_files"] or record["generated_databases"]:
            raise RuntimeError(f"Canadian boundary differs from the recorded hypothesis: {record}")
        boundaries.append(record)
    (evidence / "canadian-boundary.json").write_text(json.dumps(boundaries, indent=2) + "\n")

    # Compress the first run's existing plain logs too. New prepare_builds runs
    # write compressed receipts directly after each compilation finishes.
    for build in builds["builds"]:
        if "log" in build:
            log = evidence / build["log"]
            if log.suffix != ".gz":
                compressed = log.with_name(log.name + ".gz")
                compressed.write_bytes(gzip.compress(log.read_bytes(), mtime=0))
                build["log"] = compressed.name
                log.unlink()
            build["log_sha256"] = sha256(evidence / build["log"])
    (evidence / "builds.json").write_text(json.dumps(builds, indent=2) + "\n")
    artifacts = evidence / "artifacts.tar.gz"
    with tarfile.open(artifacts, "w:gz") as stream:
        stream.add(Path(states["original"]["snapshot"]).parent / "runs", arcname="runs")
    archive_manifest = {"file": artifacts.name, "sha256": sha256(artifacts), "bytes": artifacts.stat().st_size,
                        "contents": "Raw run inputs, all generated outputs, stdout, stderr, and exit codes"}
    (evidence / "artifacts.json").write_text(json.dumps(archive_manifest, indent=2) + "\n")
    print("PASS: original/current source integrity, unchanged live model inputs, and Canadian boundary recorded")
    print(f"Retained replay artifacts: {artifacts} ({artifacts.stat().st_size} bytes)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
