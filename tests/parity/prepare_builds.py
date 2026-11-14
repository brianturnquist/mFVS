"""Prepare isolated F77/current/candidate builds for modernization comparisons.

Snapshots have no git metadata and are never built in the working checkout.
The original snapshot receives only the documented BC/ON source-list additions.
All source inputs, commands, compiler versions, and binary hashes are retained.
Candidate builds can reuse a current binary only when the complete selected
build inputs and compiler flags are identical; that reuse is recorded explicitly.
"""

from __future__ import annotations

import argparse
import difflib
import gzip
import hashlib
import json
import os
from pathlib import Path
import platform
import shutil
import subprocess
import tarfile
import time


ROOT = Path(__file__).resolve().parents[2]
REFERENCE = "c210536536d81fd9b46dccb9a571f0de1e55e944"
REFERENCE_NVEL = "38548071d5aa652bb90c7f111f86b427f798a1c9"


def git(repo: Path, *args: str) -> str:
    return subprocess.check_output(["git", "-C", str(repo), *args], text=True).strip()


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def archive(repo: Path, revision: str, target: Path, tar_path: Path) -> None:
    target.mkdir(parents=True, exist_ok=True)
    with tar_path.open("wb") as stream:
        subprocess.run(
            ["git", "-C", str(repo), "archive", revision], stdout=stream, check=True
        )
    with tarfile.open(tar_path) as stream:
        stream.extractall(target, filter="data")


def copy_working_tree(repo: Path, target: Path) -> None:
    """Include new source files as well as tracked files; omit parity receipts."""
    target.mkdir(parents=True, exist_ok=True)
    names = subprocess.check_output(
        ["git", "-C", str(repo), "ls-files", "-z", "--cached", "--others",
         "--exclude-standard"]
    ).decode().split("\0")
    for name in sorted(set(names)):
        if not name or name.startswith("tests/parity/evidence/"):
            continue
        source = repo / name
        if not source.is_file():
            continue
        destination = target / name
        destination.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(source, destination)


def reference_accommodations(snapshot: Path) -> str:
    """Append only missing link inputs; preserve the original model sources."""
    additions = [
        "../vdbsqlite/dbsreference.f",
        "../dbsqlite/dbs_fiavbc_atrtls.f",
        "../dbsqlite/dbs_fiavbc_cutlst.f",
        "../dbsqlite/dbs_fiavbc_trls.f",
    ]
    patch = []
    for variant in ("bc", "on"):
        relative = f"bin/FVS{variant}_sourceList.txt"
        path = snapshot / relative
        before = path.read_bytes().decode()
        requested = additions + (["../volume/NVEL/wdbkwtdata.inc"] if variant == "bc" else [])
        missing = [name for name in requested if name not in before.split()]
        for name in missing:
            if not (path.parent / name).is_file():
                raise RuntimeError(f"Reference link input is missing: {name}")
        after = before
        if missing:
            after += ("" if after.endswith("\n") else "\n") + "\n".join(missing) + "\n"
            path.write_bytes(after.encode())
        for line in difflib.unified_diff(
            before.splitlines(keepends=True), after.splitlines(keepends=True),
            fromfile=f"a/{relative}", tofile=f"b/{relative}",
        ):
            patch.append(line if line.endswith("\n") else line + "\n\\ No newline at end of file\n")
    return "".join(patch)


def selected_inputs(snapshot: Path, variant: str) -> dict[str, str]:
    """Hash manifests, include classification, and every selected input file."""
    manifest = snapshot / "bin" / f"FVS{variant}_sourceList.txt"
    paths = [manifest, snapshot / "bin/makefile"]
    includes = snapshot / "bin/fortran_includes.txt"
    if includes.exists():
        paths.append(includes)
    selected = [(manifest.parent / name).resolve() for name in manifest.read_text().split()]
    paths.extend(selected)
    basenames = [p.name for p in selected]
    if len(basenames) != len(set(basenames)):
        raise RuntimeError(f"Duplicate selected basename for {variant}")
    return {str(p.relative_to(snapshot)): sha256(p) for p in paths}


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--work-dir", type=Path, required=True)
    parser.add_argument("--evidence-dir", type=Path, required=True)
    parser.add_argument("--current-revision", default="HEAD")
    parser.add_argument("--variants", nargs="+", choices=("ie", "bc", "on"), default=["ie", "bc", "on"])
    parser.add_argument("--jobs", type=int, default=4)
    args = parser.parse_args()
    work = args.work_dir.resolve()
    evidence = args.evidence_dir.resolve()
    work.mkdir(parents=True, exist_ok=False)
    evidence.mkdir(parents=True, exist_ok=True)
    current = git(ROOT, "rev-parse", args.current_revision)
    original_nvel = git(ROOT, "ls-tree", REFERENCE, "volume/NVEL").split()[2]
    if original_nvel != REFERENCE_NVEL:
        raise RuntimeError("Original NVEL identity differs from the recorded reference")
    current_nvel = git(ROOT, "ls-tree", current, "volume/NVEL").split()[2]
    candidate_nvel = git(ROOT / "volume/NVEL", "rev-parse", "HEAD")

    # Keep the numerical flags shared. The added legacy-language switches let
    # the modern compiler accept the original code without editing its math.
    fc = shutil.which("gfortran")
    cc = shutil.which("gcc-15") or shutil.which("gcc")
    make = shutil.which("make")
    if not all((fc, cc, make)):
        raise RuntimeError("gfortran, a GNU C compiler, and make are required")
    fflags = (
        "-fPIC -g -cpp -DCMPgcc -Wall -Wno-integer-division "
        "-ffpe-trap=invalid,zero,underflow,overflow,denormal -fbacktrace "
        "-fdec -std=legacy -fallow-argument-mismatch"
    )
    cflags = (
        "-fPIC -std=gnu17 -DANSI -DCMPgcc -DSQLITE_THREADSAFE=0 "
        "-DSQLITE_OMIT_LOAD_EXTENSION -Dunix"
    )
    receipt = {
        "reference": REFERENCE, "reference_nvel": original_nvel,
        "current": current, "current_nvel": current_nvel,
        "candidate_base": git(ROOT, "rev-parse", "HEAD"),
        "candidate_nvel": candidate_nvel,
        "candidate_status": git(ROOT, "status", "--porcelain=v1", "--untracked-files=all"),
        "candidate_nvel_status": git(ROOT / "volume/NVEL", "status", "--porcelain=v1"),
        "platform": platform.platform(), "machine": platform.machine(),
        "fflags": fflags, "cflags": cflags,
        "tools": {}, "states": {}, "builds": [],
    }
    for name, executable in (("fortran", fc), ("c", cc), ("make", make), ("sqlite3", shutil.which("sqlite3"))):
        if executable:
            version_flag = "-version" if name == "sqlite3" else "--version"
            receipt["tools"][name] = {
                "path": executable,
                "version": subprocess.check_output([executable, version_flag], text=True).strip(),
            }

    def save() -> None:
        (evidence / "builds.json").write_text(json.dumps(receipt, indent=2) + "\n")

    # Git archives pin committed originals/current code. The candidate is an
    # independent working-tree snapshot so later cleanup patches can be tested.
    for state, revision, nvel in (
        ("original", REFERENCE, original_nvel),
        ("current", current, current_nvel),
        ("candidate", receipt["candidate_base"], candidate_nvel),
    ):
        snapshot = work / state
        print(f"Preparing {state}: {revision} / NVEL {nvel}", flush=True)
        if state == "candidate":
            copy_working_tree(ROOT, snapshot)
            copy_working_tree(ROOT / "volume/NVEL", snapshot / "volume/NVEL")
        else:
            archive(ROOT, revision, snapshot, work / f"{state}.tar")
            archive(ROOT / "volume/NVEL", nvel, snapshot / "volume/NVEL", work / f"{state}-nvel.tar")
        if state == "original":
            patch = reference_accommodations(snapshot)
            (evidence / "reference-build.patch").write_text(patch)
        receipt["states"][state] = {"revision": revision, "nvel": nvel, "snapshot": str(snapshot)}
        save()

    # Strip ambient OS switches; otherwise an inherited OS=Windows_NT can
    # select a different ABI even though this runner is executing on Unix.
    env = dict(os.environ)
    for name in ("OS", "OSARCH", "mingw64", "sourceFileList", "sourceList"):
        env.pop(name, None)
    env.update({"LC_ALL": "C", "TZ": "UTC", "TMPDIR": str(work)})
    successful = {}
    for state, identity in receipt["states"].items():
        snapshot = Path(identity["snapshot"])
        for variant in args.variants:
            inputs = selected_inputs(snapshot, variant)
            entry = {"state": state, "variant": variant, "inputs": inputs}
            receipt["builds"].append(entry)
            save()
            signature = hashlib.sha256(json.dumps(inputs, sort_keys=True).encode()).hexdigest()
            binary = snapshot / "bin" / f"FVS{variant}"
            prior = successful.get((variant, signature))
            if state == "candidate" and prior:
                # Identical source/build input identity is stronger than merely
                # observing equal outputs. Record this reuse, never imply a
                # separately compiled third implementation.
                binary.parent.mkdir(parents=True, exist_ok=True)
                shutil.copy2(prior, binary)
                entry.update({"returncode": 0, "reused_binary": str(prior), "binary_sha256": sha256(binary)})
                print(f"Reused identical current build inputs for candidate {variant}", flush=True)
                save()
                continue
            command = [
                make, f"-j{args.jobs}", f"FVS{variant}",
                f"FC={fc}", f"CC={cc}", f"LD={fc}",
                f"FFLAGS={fflags}", f"CFLAGS={cflags}",
            ]
            log = evidence / f"build-{state}-{variant}.log.gz"
            raw_log = work / f"build-{state}-{variant}.log"
            entry.update({"command": command, "cwd": str(snapshot / "bin"), "log": log.name})
            print(f"Building {state} {variant}; log: {log}", flush=True)
            start = time.monotonic()
            with raw_log.open("wb") as stream:
                result = subprocess.run(command, cwd=snapshot / "bin", env=env, stdout=stream, stderr=subprocess.STDOUT)
            log.write_bytes(gzip.compress(raw_log.read_bytes(), mtime=0))
            entry.update({"returncode": result.returncode, "seconds": round(time.monotonic() - start, 3)})
            if result.returncode == 0:
                entry["binary_sha256"] = sha256(binary)
                successful[(variant, signature)] = binary
            save()
            if result.returncode:
                print(f"FAILED {state} {variant}: exit {result.returncode}; inspect {log}", flush=True)
                return result.returncode
            print(f"Built {state} {variant} in {entry['seconds']} seconds", flush=True)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
