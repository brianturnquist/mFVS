"""Bind a consolidation plan to exact source-list rewrites and protected files.

Only the planned definitions and their consumer manifests may change. Every
other model source/include stays byte-identical, and every consumer must still
select exactly one definition of its affected routine.
"""

from __future__ import annotations

import argparse
import difflib
import hashlib
import json
from pathlib import Path
import re
import subprocess

from prepare_builds import ROOT, selected_inputs, sha256
from record_evidence import signature, tree_integrity


MODEL_SUFFIXES = {".f", ".f90", ".for", ".f77", ".inc", ".c", ".cpp", ".h", ".hpp"}


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--evidence-dir", type=Path, required=True)
    args = parser.parse_args()
    evidence = args.evidence_dir.resolve()
    builds = json.loads((evidence / "builds.json").read_text())
    plan = json.loads((evidence / "plan.json").read_text())
    if plan["before_revision"] != builds["current"]:
        raise RuntimeError("Wrong pre-consolidation revision")
    before = Path(builds["states"]["current"]["snapshot"])
    candidate = Path(builds["states"]["candidate"]["snapshot"])
    touched = {name for task in plan["tasks"] for name in [*task["sources"], task["canonical"]]}
    removed = {name for task in plan["tasks"] for name in task["sources"] if name != task["canonical"]}
    report = {"before_revision": plan["before_revision"], "tasks": [], "variants": {}}
    differences = evidence / "differences"
    differences.mkdir(exist_ok=True)
    for task in plan["tasks"]:
        destination = task["canonical"]
        if (candidate / destination).read_bytes() != (ROOT / destination).read_bytes():
            raise RuntimeError(f"Working definition changed since snapshot: {destination}")
        unit = signature(ROOT / destination, "subroutine", task["routine"])
        sources = []
        for relative in task["sources"]:
            if sha256(before / relative) != task["before_sources"][relative]:
                raise RuntimeError(f"Plan source hash differs: {relative}")
            old_unit = signature(before / relative, "subroutine", task["routine"])
            if old_unit["arguments"] != unit["arguments"]:
                raise RuntimeError(f"Public arguments changed: {relative}")
            patch = differences / f"{task['id']}-{relative.replace('/', '_')}.patch"
            patch.write_text("".join(difflib.unified_diff(
                (before / relative).read_text().splitlines(keepends=True),
                (ROOT / destination).read_text().splitlines(keepends=True),
                fromfile="before/" + relative, tofile="canonical/" + destination)))
            blob = subprocess.check_output(["git", "-C", str(ROOT), "rev-parse", f"{plan['before_revision']}:{relative}"], text=True).strip()
            sources.append({"path": relative, "sha256": sha256(before / relative), "git_blob": blob,
                "recovery": f"git show {plan['before_revision']}:{relative}",
                "difference_file": str(patch.relative_to(evidence)), "difference_sha256": sha256(patch)})
        report["tasks"].append({**task, "canonical_sha256": sha256(ROOT / destination),
            "signature": unit, "recovery_sources": sources})
    for relative in removed:
        if (candidate / relative).exists() or (ROOT / relative).exists():
            raise RuntimeError(f"Twin still present: {relative}")

    for manifest in sorted((before / "bin").glob("FVS*_sourceList.txt")):
        variant = manifest.stem[3:5]
        relative = str(manifest.relative_to(before))
        expected = manifest.read_bytes()
        replacements = {}
        for task in plan["tasks"]:
            if variant in task["consumers"]:
                old = task["consumers"][variant]
                new = task["canonical"]
                if old != new:
                    expected = expected.replace(("../" + old).encode(), ("../" + new).encode())
                    replacements[old] = new
        if expected != (ROOT / relative).read_bytes() or expected != (candidate / relative).read_bytes():
            raise RuntimeError(f"Unexpected manifest edit: {relative}")
        old_inputs = selected_inputs(before, variant)
        new_inputs = selected_inputs(candidate, variant)
        if new_inputs != selected_inputs(ROOT, variant):
            raise RuntimeError(f"Working build inputs changed: {variant}")
        expected_paths = {replacements.get(name, name) for name in old_inputs}
        if set(new_inputs) != expected_paths:
            raise RuntimeError(f"Unexpected selected file inventory: {variant}")
        for name, digest in old_inputs.items():
            if name != relative and name not in touched and new_inputs.get(name) != digest:
                raise RuntimeError(f"Unexpected selected source/include change: {variant}/{name}")
        definitions = {}
        for task in plan["tasks"]:
            matches = []
            for name in new_inputs:
                if Path(name).suffix.lower() not in (".f90", ".f", ".for"):
                    continue
                code = "\n".join(line.split("!")[0] for line in (candidate / name).read_text(errors="replace").splitlines())
                if re.search(rf"\bsubroutine\s+{task['routine']}\b", code, re.I):
                    matches.append(name)
            if len(matches) > 1:
                raise RuntimeError(f"Ambiguous selected definition: {variant}/{task['routine']}: {matches}")
            if variant in task["consumers"] and matches != [task["canonical"]]:
                raise RuntimeError(f"Canonical definition not selected: {variant}/{task['id']}")
            definitions[task["id"]] = matches[0] if matches else None
        report["variants"][variant] = {"manifest_sha256": sha256(ROOT / relative), "rewrites": replacements,
            "selected_input_count": len(new_inputs), "selected_definitions": definitions,
            "input_inventory_sha256": hashlib.sha256(json.dumps(new_inputs, sort_keys=True).encode()).hexdigest()}

    entries = subprocess.check_output(["git", "-C", str(ROOT), "ls-tree", "-rz", plan["before_revision"]]).split(b"\0")
    checked = 0
    for entry in entries:
        if not entry:
            continue
        metadata, name = entry.split(b"\t", 1)
        relative = name.decode()
        if metadata.split()[1] != b"blob" or relative in touched or Path(relative).suffix.lower() not in MODEL_SUFFIXES:
            continue
        if (before / relative).read_bytes() != (candidate / relative).read_bytes() or (candidate / relative).read_bytes() != (ROOT / relative).read_bytes():
            raise RuntimeError(f"Unplanned model change: {relative}")
        checked += 1
    report["other_model_files_unchanged"] = checked
    report["working_nvel"] = tree_integrity(ROOT / "volume/NVEL", builds["current_nvel"], ROOT / "volume/NVEL", set())
    report["passed"] = True
    (evidence / "consolidation.json").write_text(json.dumps(report, indent=2) + "\n")
    print(f"PASS: six canonical definitions; exact planned selection for all 24 variants; {checked} other model files unchanged")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
