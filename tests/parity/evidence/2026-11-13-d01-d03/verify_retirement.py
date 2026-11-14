"""Record D01-D03 retirement without changing any scientific implementation.

The complete selected input set must be unchanged for all 24 variants. The
retired source bytes remain recoverable from the recorded git commit/blob;
they are not copied into another source directory. Historical, already broken
Canadian manifests are recorded separately from supported build selection.
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
import sys


ROOT = Path(__file__).resolve().parents[4]
sys.path.insert(0, str(ROOT / "tests/parity"))
from prepare_builds import selected_inputs, sha256
from record_evidence import tree_integrity


RETIRED = ("sn/sitsetOld.f90", "rd/rdrout.f90", "vbase/natcrz.f90")


def git_bytes(revision: str, path: str) -> bytes:
    return subprocess.check_output(["git", "-C", str(ROOT), "show", f"{revision}:{path}"])


def signature(inputs: dict[str, str]) -> str:
    return hashlib.sha256(json.dumps(inputs, sort_keys=True).encode()).hexdigest()


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--evidence-dir", type=Path, default=Path(__file__).parent)
    args = parser.parse_args()
    evidence = args.evidence_dir.resolve()
    builds = json.loads((evidence / "builds.json").read_text())
    states = builds["states"]
    before = Path(states["current"]["snapshot"])
    candidate = Path(states["candidate"]["snapshot"])
    revision = states["current"]["revision"]
    report = {"before_revision": revision, "nvel": states["current"]["nvel"],
              "retired": [], "variants": {}, "legacy_manifests": []}

    manifests = sorted((before / "bin").glob("FVS*_sourceList.txt"))
    if len(manifests) != 24:
        raise RuntimeError("Reassess supported variant scope before retiring files")
    selected = defaultdict(set)
    for manifest in manifests:
        variant = manifest.stem[3:5]
        previous = selected_inputs(before, variant)
        following = selected_inputs(candidate, variant)
        working = selected_inputs(ROOT, variant)
        if previous != following or following != working:
            raise RuntimeError(f"Selected inputs changed for {variant}")
        report["variants"][variant] = {"selected_files_and_manifests": len(previous),
                                      "before_sha256": signature(previous),
                                      "candidate_sha256": signature(following), "unchanged": True}
        for entry in manifest.read_text().split():
            path = str((manifest.parent / entry).resolve().relative_to(before))
            selected[path].add(variant)
    # Use ordinary dictionaries for reads: looking up an unselected path must
    # not accidentally add it to the set of supposedly selected sources.
    selected = dict(selected)
    for relative in RETIRED:
        if relative in selected or (candidate / relative).exists() or (ROOT / relative).exists():
            raise RuntimeError(f"Retirement was not isolated: {relative}")
        data = git_bytes(revision, relative)
        if (before / relative).read_bytes() != data:
            raise RuntimeError(f"Pre-deletion bytes differ from recovery revision: {relative}")
        blob = subprocess.check_output(["git", "-C", str(ROOT), "rev-parse", f"{revision}:{relative}"], text=True).strip()
        report["retired"].append({"path": relative, "git_blob": blob,
                                  "sha256": hashlib.sha256(data).hexdigest(),
                                  "bytes": len(data), "lines": len(data.splitlines()),
                                  "recover_with": f"git show {revision}:{relative}",
                                  "selected_by": []})

    # A model source outside a manifest could still be accidentally edited.
    # Check every tracked Fortran/C/include file, not only compiled inputs.
    model_suffixes = {".f", ".f90", ".for", ".f77", ".inc", ".c", ".cpp", ".h", ".hpp"}
    tracked = subprocess.check_output(["git", "-C", str(ROOT), "ls-tree", "-rz", revision]).split(b"\0")
    checked = 0
    for entry in tracked:
        if not entry:
            continue
        metadata, name = entry.split(b"\t", 1)
        relative = name.decode()
        if metadata.split()[1] != b"blob" or Path(relative).suffix.lower() not in model_suffixes or relative in RETIRED:
            continue
        if (before / relative).read_bytes() != (candidate / relative).read_bytes() or (candidate / relative).read_bytes() != (ROOT / relative).read_bytes():
            raise RuntimeError(f"Unexpected model edit: {relative}")
        checked += 1
    report["other_model_files_checked_unchanged"] = checked

    selectors = []
    names = [entry.split(b"\t", 1)[1].decode() for entry in tracked if entry]
    for name in names:
        p = Path(name)
        if p.name.lower() in ("makefile", "cmakelists.txt") or p.suffix.lower() in (".cmake", ".bat", ".sh", ".py", ".vfproj", ".vcxproj"):
            selectors.append(name)
    for legacy in ("canada/bin/FVSon_sourceList.txt", "canada/bin/FVSbcc_notReady.txt"):
        entries = (before / legacy).read_text().split()
        refs = [entry for entry in entries if entry.removeprefix("../") in RETIRED]
        references = [name for name in selectors if legacy in (before / name).read_text(errors="replace")
                      or Path(legacy).name == "FVSbcc_notReady.txt" and "FVSbcc_notReady.txt" in (before / name).read_text(errors="replace")]
        if references:
            raise RuntimeError(f"Legacy source-list consumer needs investigation: {references}")
        report["legacy_manifests"].append({
            "path": legacy, "retired_references": refs, "supported_recipe_references": references,
            "entries": len(entries),
            "missing_before": sum(not (before / "bin" / entry).is_file() for entry in entries),
            "missing_after": sum(not (candidate / "bin" / entry).is_file() for entry in entries),
            "classification": "Unused historical list, already incomplete before deletion; retained unchanged",
        })
    for selector in ("bin/makefile", "bin/CMakeLists.txt", "bin/fortran_includes.txt"):
        if sha256(before / selector) != sha256(ROOT / selector):
            raise RuntimeError(f"Build selection changed: {selector}")
    report["build_selection_unchanged"] = True

    references = []
    for relative, variants in sorted(selected.items()):
        p = before / relative
        if p.suffix.lower() not in model_suffixes:
            continue
        for number, line in enumerate(p.read_text(errors="replace").splitlines(), 1):
            code = line.split("!")[0]
            if any(Path(retired).name.lower() in code.lower() for retired in RETIRED):
                raise RuntimeError(f"Selected source references a retired filename: {relative}:{number}")
            if re.search(r"\b(?:call|external|entry|subroutine|function)\s+(?:RDROUT|NATCRZ)\b", code, re.I):
                references.append({"file": relative, "line": number, "variants": sorted(variants), "statement": code.strip()})
    report["selected_rdrout_natcrz_statements"] = references
    report["preserved"] = {path: {"sha256": sha256(ROOT / path), "variants": sorted(selected.get(path, set()))}
                           for path in ("sn/sitset.f90", "base/genrpt.f90", "base/fvs.f90", "base/exrd.f90", "canada/bc/natcrz.f90")}
    report["archive_integrity"] = {}
    for state in ("original", "current"):
        snapshot = Path(states[state]["snapshot"])
        allowed = {"bin/FVSbc_sourceList.txt", "bin/FVSon_sourceList.txt"} if state == "original" else set()
        report["archive_integrity"][state] = tree_integrity(ROOT, states[state]["revision"], snapshot, allowed)
        report["archive_integrity"][state + "_nvel"] = tree_integrity(ROOT / "volume/NVEL", states[state]["nvel"], snapshot / "volume/NVEL", set())
    old = git_bytes(revision, "sn/sitsetOld.f90").decode()
    live = git_bytes(revision, "sn/sitset.f90").decode()
    (evidence / "D01-sitset-differences.patch").write_text("".join(difflib.unified_diff(
        old.splitlines(keepends=True), live.splitlines(keepends=True),
        fromfile="retired/sn/sitsetOld.f90", tofile="retained/sn/sitset.f90")))
    report["passed"] = True
    (evidence / "retirement.json").write_text(json.dumps(report, indent=2) + "\n")
    print(f"PASS: all 24 build input sets unchanged; {checked} other model files unchanged")
    print("PASS: three excluded sources retired with exact git recovery references")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
