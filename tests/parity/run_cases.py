"""Run the bounded P02/P03 decks in independent directories without make tests.

Input bytes must agree across the three snapshots. Each run starts in a new
directory; every newly created or modified output and the actual process exit
code are inventoried. Inputs that a run modifies are reported explicitly.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import time


CASES = {
    "ie-iet01": ("ie", "iet01", ["iet01.key", "iet01.tre"]),
    "bc-StandStructure": ("bc", "StandStructure", ["StandStructure.key", "FVS-BC.Stand.Structure.Data.db"]),
    "bc-YSM-SkyRanch": ("bc", "YSM-SkyRanch", ["YSM-SkyRanch.key", "FVS-BC.YSM-SkyRanch.db"]),
    "on-Hardwood": ("on", "Hardwood", ["Hardwood.key", "FVSDataHardwood.db"]),
}


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--evidence-dir", type=Path, required=True)
    args = parser.parse_args()
    evidence = args.evidence_dir.resolve()
    builds = json.loads((evidence / "builds.json").read_text())
    snapshots = {name: Path(state["snapshot"]) for name, state in builds["states"].items()}
    work = snapshots["original"].parent
    receipt = {"cases": {}, "runs": []}
    env = dict(os.environ)
    env.update({"LC_ALL": "C", "TZ": "UTC"})
    for case, (variant, deck, filenames) in CASES.items():
        inputs = {}
        for name in filenames:
            paths = [snapshot / "tests" / f"FVS{variant}" / name for snapshot in snapshots.values()]
            hashes = [sha256(path) for path in paths]
            if len(set(hashes)) != 1:
                raise RuntimeError(f"Input mismatch between snapshots: {case}/{name}")
            inputs[name] = hashes[0]
        receipt["cases"][case] = {"variant": variant, "deck": deck, "inputs": inputs}
        # P02 exercises all three identities. P03 needs the unmodified original
        # and current versions to establish the pre-existing Canadian boundary.
        states = ("original", "current", "candidate") if variant == "ie" else ("original", "current")
        for state in states:
            binary = snapshots[state] / "bin" / f"FVS{variant}"
            directory = work / "runs" / state / case
            directory.mkdir(parents=True, exist_ok=False)
            for name in filenames:
                shutil.copy2(snapshots["original"] / "tests" / f"FVS{variant}" / name, directory / name)
            command = [str(binary), f"--keywordfile={deck}.key"]
            print(f"Running {state} {case}", flush=True)
            start = time.monotonic()
            with (directory / "stdout.txt").open("wb") as out, (directory / "stderr.txt").open("wb") as err:
                result = subprocess.run(command, cwd=directory, env=env, stdin=subprocess.DEVNULL,
                                        stdout=out, stderr=err, timeout=60)
            (directory / "returncode.txt").write_text(f"{result.returncode}\n")
            outputs = {}
            modified_inputs = []
            for path in sorted(directory.rglob("*")):
                if not path.is_file():
                    continue
                relative = str(path.relative_to(directory))
                digest = sha256(path)
                if relative in inputs:
                    if digest == inputs[relative]:
                        continue
                    modified_inputs.append(relative)
                outputs[relative] = {"sha256": digest, "bytes": path.stat().st_size}
            receipt["runs"].append({
                "state": state, "case": case, "directory": str(directory),
                "command": command, "returncode": result.returncode,
                "seconds": round(time.monotonic() - start, 3),
                "binary_sha256": sha256(binary), "outputs": outputs,
                "modified_inputs": modified_inputs,
            })
            (evidence / "runs.json").write_text(json.dumps(receipt, indent=2) + "\n")
            print(f"Finished {state} {case}: exit {result.returncode}, {len(outputs)} outputs", flush=True)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
