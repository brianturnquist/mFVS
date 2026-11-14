"""Verify a bounded retirement against its plan and original build snapshots.

Keep model sources in their canonical homes. A receipt retains exact Git
recovery identities, per-file replacement diffs and DATA statements instead of
creating another directory of duplicate Fortran implementations.
"""

from __future__ import annotations

import argparse
from collections import defaultdict
import difflib
import hashlib
import json
from pathlib import Path
import re
import subprocess

from prepare_builds import ROOT, selected_inputs, sha256
from record_evidence import tree_integrity


MODEL_SUFFIXES = {".f", ".f90", ".for", ".f77", ".inc", ".c", ".cpp", ".h", ".hpp"}


def statements(source: str) -> list[str]:
    """Join free-form continuations for readable DATA/equation receipts.

    These records aid review; byte hashes and full diffs are authoritative.
    The retired inputs are free form, without exclamation marks in literals.
    """
    result = []
    pending = ""
    for line in source.splitlines():
        code = line.split("!")[0].strip()
        if not code:
            continue
        pending += " " + code.lstrip("&").rstrip("&").strip()
        if not code.endswith("&"):
            result.append(pending.strip())
            pending = ""
    if pending:
        raise ValueError("Unterminated free-form continuation")
    return result


def input_digest(inputs: dict[str, str]) -> str:
    return hashlib.sha256(json.dumps(inputs, sort_keys=True).encode()).hexdigest()


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--evidence-dir", type=Path, required=True)
    args = parser.parse_args()
    evidence = args.evidence_dir.resolve()
    plan = json.loads((evidence / "plan.json").read_text())
    builds = json.loads((evidence / "builds.json").read_text())
    states = builds["states"]
    before = Path(states["current"]["snapshot"])
    candidate = Path(states["candidate"]["snapshot"])
    revision = states["current"]["revision"]
    if revision != plan["before_revision"]:
        raise RuntimeError("The retirement plan names a different baseline")
    retired = {item["path"] for item in plan["tasks"]}
    if len(retired) != len(plan["tasks"]):
        raise RuntimeError("Each retirement task must name a separate file")
    report = {"before_revision": revision, "nvel": states["current"]["nvel"],
              "variants": {}, "retired": [], "legacy_manifests": []}

    # Every supported variant must retain the entire set of selected input
    # bytes, including includes, source lists and build configuration.
    manifests = sorted((before / "bin").glob("FVS*_sourceList.txt"))
    if len(manifests) != 24:
        raise RuntimeError("Reassess supported variant scope before retirement")
    selected = defaultdict(set)
    for manifest in manifests:
        variant = manifest.stem[3:5]
        previous = selected_inputs(before, variant)
        following = selected_inputs(candidate, variant)
        working = selected_inputs(ROOT, variant)
        if previous != following or following != working:
            raise RuntimeError(f"Selected build inputs changed: {variant}")
        report["variants"][variant] = {"files": len(previous), "unchanged": True,
                                      "before_sha256": input_digest(previous),
                                      "candidate_sha256": input_digest(following)}
        for entry in manifest.read_text().split():
            relative = str((manifest.parent / entry).resolve().relative_to(before))
            selected[relative].add(variant)
    selected = dict(selected)
    diff_dir = evidence / "differences"
    diff_dir.mkdir(exist_ok=True)
    for item in plan["tasks"]:
        relative = item["path"]
        if relative in selected or (candidate / relative).exists() or (ROOT / relative).exists():
            raise RuntimeError(f"Retirement is not isolated: {relative}")
        data = subprocess.check_output(["git", "-C", str(ROOT), "show", f"{revision}:{relative}"])
        if data != (before / relative).read_bytes():
            raise RuntimeError(f"Recovery revision does not match: {relative}")
        blob = subprocess.check_output(["git", "-C", str(ROOT), "rev-parse", f"{revision}:{relative}"], text=True).strip()
        replacement = item["replacement"]
        if replacement not in selected:
            raise RuntimeError(f"Replacement is not selected: {replacement}")
        old, live = data.decode(), (before / replacement).read_text()
        old_statements = statements(old)
        units = [line for line in old_statements
                 if re.match(r"(?:subroutine|(?:\w+\s+)?function|entry)\b", line, re.I)]
        for unit in units:
            name = re.search(r"\b(?:subroutine|function|entry)\s+(\w+)", unit, re.I)[1]
            if not re.search(rf"\b(?:subroutine|function|entry)\s+{name}\b", live, re.I):
                raise RuntimeError(f"Replacement lacks {name}: {replacement}")
        patch = diff_dir / (item["id"] + ".patch")
        patch.write_text("".join(difflib.unified_diff(
            old.splitlines(keepends=True), live.splitlines(keepends=True),
            fromfile="retired/" + relative, tofile="retained/" + replacement)))
        data_statements = [line for line in old_statements if re.match(r"data\b", line, re.I)]
        body = [line.lower() for line in old_statements if not re.match(r"data\b", line, re.I)]
        report["retired"].append({**item, "git_blob": blob,
            "sha256": hashlib.sha256(data).hexdigest(), "bytes": len(data), "lines": len(data.splitlines()),
            "recover_with": f"git show {revision}:{relative}", "selected_by": [], "units": units,
            "data_statements": data_statements, "body_excluding_data_sha256": input_digest({"body": "\n".join(body)}),
            "replacement_sha256": sha256(before / replacement),
            "replacement_selected_by": sorted(selected[replacement]),
            "difference_file": str(patch.relative_to(evidence)), "difference_sha256": sha256(patch)})

    # Check unselected model files too: an accidental edit must not hide
    # outside the build-input proof. NVEL is checked separately below.
    tracked = subprocess.check_output(["git", "-C", str(ROOT), "ls-tree", "-rz", revision]).split(b"\0")
    names = []
    checked = 0
    for entry in tracked:
        if not entry:
            continue
        metadata, name = entry.split(b"\t", 1)
        relative = name.decode()
        if metadata.split()[1] != b"blob":
            continue
        names.append(relative)
        if Path(relative).suffix.lower() not in MODEL_SUFFIXES or relative in retired:
            continue
        if (before / relative).read_bytes() != (candidate / relative).read_bytes() or (candidate / relative).read_bytes() != (ROOT / relative).read_bytes():
            raise RuntimeError(f"Unexpected model edit: {relative}")
        checked += 1
    report["other_model_files_checked_unchanged"] = checked

    # Old Canadian manifests are not current build selectors. Record their
    # references using both their own directory and the historical bin root,
    # and prove that no repository recipe consumes these incomplete lists.
    selectors = [name for name in names if "tests/parity/" not in name and (
        Path(name).name.lower() in ("makefile", "cmakelists.txt") or
        Path(name).suffix.lower() in (".cmake", ".bat", ".sh", ".py", ".vfproj", ".vcxproj"))]
    for name in names:
        path = before / name
        if path.suffix != ".txt" or "canada/bin/" not in name:
            continue
        entries = path.read_text().split()
        resolutions = {}
        for label, base in (("own_directory", path.parent), ("root_bin", before / "bin")):
            refs = [entry for entry in entries if str((base / entry).resolve().relative_to(before)) in retired]
            if refs:
                resolutions[label] = {"references": refs,
                    "missing_before": sum(not (base / entry).is_file() for entry in entries),
                    "missing_after": sum(not (candidate / (base / entry).resolve().relative_to(before)).is_file() for entry in entries)}
        if not resolutions:
            continue
        consumers = []
        same_basename_mentions = []
        for selector in selectors:
            text = (before / selector).read_text(errors="replace")
            if name in text:
                consumers.append(selector)
            elif path.name in text:
                # The root ON list has the same basename as its Canadian
                # predecessor. CMake's bin-local glob names the root list;
                # a basename match alone must not invent a legacy consumer.
                resolved = (before / selector).parent / path.name
                if resolved == path or "canada/bin" in text or not resolved.is_file():
                    consumers.append(selector)
                else:
                    same_basename_mentions.append({"recipe": selector,
                        "resolved_root_list": str(resolved.relative_to(before))})
        if consumers or (before / name).read_bytes() != (ROOT / name).read_bytes():
            raise RuntimeError(f"Reassess historical manifest consumers: {name}: {consumers}")
        report["legacy_manifests"].append({"path": name, "entries": len(entries),
            "resolutions": resolutions, "recipe_consumers": consumers,
            "same_basename_mentions_of_root_list": same_basename_mentions, "unchanged": True})
    for selector in selectors:
        text = (before / selector).read_text(errors="replace").lower()
        if any(relative.lower() in text for relative in retired):
            raise RuntimeError(f"Build recipe references a retired path: {selector}")
        if (before / selector).read_bytes() != (ROOT / selector).read_bytes():
            raise RuntimeError(f"Build recipe changed: {selector}")
    for relative in selected:
        path = before / relative
        if path.suffix.lower() not in MODEL_SUFFIXES:
            continue
        code = "\n".join(line.split("!")[0] for line in path.read_text(errors="replace").splitlines()).lower()
        if any(retired_path.lower() in code for retired_path in retired):
            raise RuntimeError(f"Selected source references a retired path: {relative}")
        for included in re.findall(r"\binclude\s*['\"]([^'\"]+)", code):
            if Path(included).name in {Path(path).name.lower() for path in retired}:
                raise RuntimeError(f"Selected source includes a retired basename: {relative}: {included}")
    report["selected_sources_include_no_retired_basename"] = True
    report["build_recipes_checked_unchanged"] = len(selectors)
    report["archive_integrity"] = {}
    for state in ("original", "current"):
        snapshot = Path(states[state]["snapshot"])
        allowed = {"bin/FVSbc_sourceList.txt", "bin/FVSon_sourceList.txt"} if state == "original" else set()
        report["archive_integrity"][state] = tree_integrity(ROOT, states[state]["revision"], snapshot, allowed)
        report["archive_integrity"][state + "_nvel"] = tree_integrity(ROOT / "volume/NVEL", states[state]["nvel"], snapshot / "volume/NVEL", set())
    report["archive_integrity"]["working_nvel"] = tree_integrity(ROOT / "volume/NVEL", states["current"]["nvel"], ROOT / "volume/NVEL", set())
    report["totals"] = {"files": len(retired), "lines": sum(item["lines"] for item in report["retired"]),
                        "bytes": sum(item["bytes"] for item in report["retired"])}
    report["passed"] = True
    (evidence / "retirement.json").write_text(json.dumps(report, indent=2) + "\n")
    print(f"PASS: {len(retired)} retirements; all 24 selected build input sets unchanged; {checked} other model files unchanged")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
