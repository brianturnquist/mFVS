"""Run selected parity decks in independent directories without make tests.

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
import subprocess
import time


CASES = {
    "ie-iet01": ("ie", "iet01", ["iet01.key", "iet01.tre"]),
    "bc-StandStructure": ("bc", "StandStructure", ["StandStructure.key", "FVS-BC.Stand.Structure.Data.db"]),
    "bc-YSM-SkyRanch": ("bc", "YSM-SkyRanch", ["YSM-SkyRanch.key", "FVS-BC.YSM-SkyRanch.db"]),
    "on-Hardwood": ("on", "Hardwood", ["Hardwood.key", "FVSDataHardwood.db"]),
    "sn-snt01": ("sn", "snt01", ["snt01.key", "snt01.tre"]),
    "sn-nc03": ("sn", "snt01", ["snt01.key", "snt01.tre"]),
    "sn-nc05": ("sn", "snt01", ["snt01.key", "snt01.tre"]),
}
DEFAULT_CASES = ["ie-iet01", "bc-StandStructure", "bc-YSM-SkyRanch", "on-Hardwood"]
SN_FORESTS = {"sn-nc03": 81103, "sn-nc05": 81105}


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def derived_sn_input(data: bytes, forest: int) -> bytes:
    """Exercise the NC merchantability branches in copies of the first stand.

    Keep the original repository deck intact. Use its first complete PROCESS
    block, replace only STDINFO field 1, and append STOP. These identical bytes
    are supplied to original/current/candidate, with the derivation recorded.
    """
    lines = data.decode("ascii").splitlines(keepends=True)
    end = next(i for i, line in enumerate(lines) if line.strip().upper() == "PROCESS")
    lines = lines[:end + 1]
    indices = [i for i, line in enumerate(lines) if line[:8].strip().upper() == "STDINFO"]
    if len(indices) != 1 or lines[indices[0]][10:20].strip() != "80106":
        raise ValueError("The source SN deck changed; recheck the targeted-input derivation")
    i = indices[0]
    lines[i] = lines[i][:10] + f"{forest:10d}" + lines[i][20:]
    return ("".join(lines).rstrip("\r\n") + "\nSTOP\n").encode("ascii")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--evidence-dir", type=Path, required=True)
    parser.add_argument("--cases", nargs="+", choices=tuple(CASES), default=DEFAULT_CASES)
    args = parser.parse_args()
    evidence = args.evidence_dir.resolve()
    builds = json.loads((evidence / "builds.json").read_text())
    snapshots = {name: Path(state["snapshot"]) for name, state in builds["states"].items()}
    work = snapshots["original"].parent
    receipt = {"cases": {}, "runs": []}
    env = dict(os.environ)
    env.update({"LC_ALL": "C", "TZ": "UTC"})
    for case in args.cases:
        variant, deck, filenames = CASES[case]
        inputs = {}
        source_inputs = {}
        payloads = {}
        for name in filenames:
            paths = [snapshot / "tests" / f"FVS{variant}" / name for snapshot in snapshots.values()]
            hashes = [sha256(path) for path in paths]
            if len(set(hashes)) != 1:
                raise RuntimeError(f"Input mismatch between snapshots: {case}/{name}")
            source_inputs[name] = hashes[0]
            data = paths[0].read_bytes()
            if case in SN_FORESTS and name == "snt01.key":
                data = derived_sn_input(data, SN_FORESTS[case])
            payloads[name] = data
            inputs[name] = hashlib.sha256(data).hexdigest()
        receipt["cases"][case] = {"variant": variant, "deck": deck, "inputs": inputs,
                                 "source_inputs": source_inputs}
        if case in SN_FORESTS:
            receipt["cases"][case]["derivation"] = {
                "source": "sn-snt01", "first_stand_only": True,
                "stdinfo_field_1": SN_FORESTS[case], "appended_keyword": "STOP",
            }
        # Canadian cases establish the existing input-failure boundary; the
        # simulation decks compare all three identities in separate processes.
        states = ("original", "current") if variant in ("bc", "on") else ("original", "current", "candidate")
        for state in states:
            binary = snapshots[state] / "bin" / f"FVS{variant}"
            directory = work / "runs" / state / case
            directory.mkdir(parents=True, exist_ok=False)
            for name in filenames:
                (directory / name).write_bytes(payloads[name])
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
