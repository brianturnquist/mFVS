"""Check the SP19 Canadian repairs (bugs 22, 28, 44 and 47).

`formats` (bug 22) needs only Git: every FORMAT whose text the free-form
conversion split with a quote join must print what the original fixed-form
F77 FORMAT at c2105365 printed. Each statement is tokenized into text items
and edit descriptors, adjacent text is merged (the F77 sources relied on
adjacent text items without a comma), and the two sequences must be equal.
No such split may remain in any selected source.

`decks` (bugs 28, 44 and 47) runs the BCBiocontrol, BCVoleqnumInvalid and
ONCrnmult decks with the FVSbc and FVSon programs of an already built tree
(--build-tree, e.g. a copy of the working tree after `make FVSbc.setup
FVSon.setup`) and checks the repaired behavior directly:

* bug 47: the listing's volume equation table holds only printable text, and
  species reset by an invalid VOLEQNUM show BC's default `---`;
* bug 28: DMBCI echoes the host species code, and DMBCA's activity is applied
  (DONE) by the mistletoe cycle;
* bug 44: in the first cycle a CRNMULT for MH changes MH crowns only,
  compared with the same deck without CRNMULT.

check_suite.py --with-new-decks compares the decks' complete outputs.
"""

from __future__ import annotations

import argparse
import os
from pathlib import Path
import re
import shutil
import sqlite3
import subprocess
import sys
import tempfile

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[2]
ORIGINAL = "c210536536d81fd9b46dccb9a571f0de1e55e944"

# Repaired FORMATs: (current source, label, a line of the repaired statement,
# original F77 source, original label). The line picks the right statement where
# a consolidated file holds more than one FORMAT with the label.
FORMATS = [
    ("canada/bc/becset.f90", "31", 1231, "canada/bc/becset.f", "31"),
    ("canada/bc/becset.f90", "34", 1243, "canada/bc/becset.f", "34"),
    ("canada/bc/becset.f90", "41", 1462, "canada/bc/becset.f", "41"),
    ("canada/bc/becset.f90", "61", 1522, "canada/bc/becset.f", "61"),
    ("canada/bc/becset.f90", "71", 1581, "canada/bc/becset.f", "71"),
    ("canada/bc/sitset.f90", "57", 251, "canada/bc/sitset.f", "57"),
    ("clim/base/clin.f90", "9106", 96, "metric/clim/clin.f", "106"),
    ("fire/base/fmsrecord_mod.f90", "201", 84, "metric/fire/base/fmsout.f", "200"),
    ("metric/fire/base/fmin.f90", "5610", 1356, "metric/fire/base/fmin.f", "5610"),
    ("metric/fire/vbase/fmfout.f90", "318", 331, "metric/fire/vbase/fmfout.f", "318"),
    ("metric/fire/vbase/fmpofl.f90", "132", 411, "metric/fire/vbase/fmpofl.f", "132"),
    ("metric/newmist/misprt.f90", "905", 930, "metric/newmist/misprt.f", "905"),
    ("vbase/morts.f90", "351", 3169, "canada/on/morts.f", "351"),
]


def free_statements(text: str) -> list[tuple[int, int, str]]:
    """Free-form statements as (first line, last line, text), continuations
    joined as the compiler joins them."""
    result, pending, first = [], "", 0
    for number, line in enumerate(text.splitlines(), 1):
        stripped = line.rstrip().lstrip()
        if not pending and (not stripped or stripped.startswith("!")):
            continue
        if not pending:
            first = number
        elif stripped.startswith("&"):
            stripped = stripped[1:]
        if stripped.endswith("&"):
            pending += stripped[:-1]
            continue
        result.append((first, number, pending + stripped))
        pending = ""
    return result


def fixed_statements(text: str) -> list[str]:
    """Fixed-form statements: columns 7-72, continuation in column 6."""
    result = []
    for line in text.splitlines():
        if not line or line[0] in "cC*!":
            continue
        body = line[6:72] if len(line) > 6 else ""
        if len(line) > 5 and line[5] not in " 0" and result:
            result[-1] = (result[-1][0], result[-1][1] + body)
        else:
            result.append((line[:5].strip(), body))
    return [(0, 0, f"{label} {body}") for label, body in result]


def format_items(statements: list[tuple[int, int, str]], label: str, line: int = 0) -> list[str]:
    """Text and descriptor items of FORMAT `label`, adjacent text merged."""
    pattern = re.compile(rf"^\s*{label}\s+format\s*\((.*)\)\s*$", re.I)
    found = [m[1] for first, last, s in statements if (m := pattern.match(s))
             and (not line or first <= line <= last)]
    if len(found) != 1:
        raise ValueError(f"FORMAT {label}: found {len(found)}")
    body, items, index = found[0], [], 0
    while index < len(body):
        char = body[index]
        if char in "'\"":
            end, text = index + 1, ""
            while True:
                if body[end] == char and end + 1 < len(body) and body[end + 1] == char:
                    text += char
                    end += 2
                elif body[end] == char:
                    break
                else:
                    text += body[end]
                    end += 1
            if items and items[-1].startswith("TEXT:"):
                items[-1] += text  # adjacent text items print as one
            else:
                items.append("TEXT:" + text)
            index = end + 1
        elif char in " ,\t":
            index += 1
        else:
            match = re.match(r"[^'\",]+", body[index:])
            token = re.sub(r"\s+", "", match[0]).upper()
            items.append(token)
            index += len(match[0])
    return items


def check_formats() -> list[str]:
    results = []
    for source, label, line, original, original_label in FORMATS:
        current = format_items(free_statements((ROOT / source).read_text(encoding="latin1")), label, line)
        old_text = subprocess.check_output(["git", "-C", str(ROOT), "show", f"{ORIGINAL}:{original}"]).decode("latin1")
        old = format_items(fixed_statements(old_text), original_label)
        ok = current == old
        results.append(f"{'PASS' if ok else 'FAIL'} {source} FORMAT {label} prints as {original} {original_label}")
    # No quote-joined split may remain in any selected source.
    remaining = []
    for manifest in (ROOT / "bin").glob("FVS??_sourceList.txt"):
        for name in manifest.read_text().split():
            path = (manifest.parent / name).resolve()
            if path.suffix.lower() != ".f90":
                continue
            lines = path.read_text(encoding="latin1").splitlines()
            remaining += [f"{path.relative_to(ROOT).as_posix()}:{i + 1}" for i in range(len(lines) - 1)
                          if re.search(r"'&\s*$", lines[i]) and re.match(r"\s*&'", lines[i + 1])]
    remaining = sorted(set(remaining))
    results.append(("FAIL " if remaining else "PASS ") + "no quote-joined FORMAT text remains"
                   + (": " + ", ".join(remaining) if remaining else ""))
    return results


def run_deck(program: Path, folder: Path, deck: str) -> None:
    subprocess.run([str(program), f"--keywordfile={deck}.key"], cwd=folder, capture_output=True,
                   timeout=900, env=dict(os.environ))


def check_decks(tree: Path) -> list[str]:
    suffix = ".exe" if sys.platform == "win32" else ""
    results = []
    with tempfile.TemporaryDirectory(prefix="mfvs-canadian-") as name:
        work = Path(name)
        bc, on = work / "FVSbc", work / "FVSon"
        shutil.copytree(ROOT / "tests/FVSbc", bc)
        shutil.copytree(ROOT / "tests/FVSon", on)
        bc_program, on_program = tree / "bin" / f"FVSbc{suffix}", tree / "bin" / f"FVSon{suffix}"

        run_deck(bc_program, bc, "BCVoleqnumInvalid")
        listing = (bc / "BCVoleqnumInvalid.out").read_bytes()
        start = listing.find(b"NATIONAL VOLUME ESTIMATOR LIBRARY EQUATION NUMBERS")
        dashes = listing.find(b"-------", start)
        end = re.search(rb"\r?\n[ \t]*\r?\n", listing[dashes:])  # the table ends at a blank line
        table = listing[start:dashes + end.start()] if start >= 0 and dashes >= 0 and end else b""
        printable = start >= 0 and all(32 <= c < 127 or c in (10, 13) for c in table)
        results.append(("PASS" if printable and b"---" in table else "FAIL")
                       + " bug 47: equation table is printable text with BC's default ---")

        run_deck(bc_program, bc, "BCBiocontrol")
        listing = (bc / "BCBiocontrol.out").read_text(encoding="latin1")
        echoed = re.search(r"Host species: PL\s+\(CODE=\s*\d+\)", listing)
        applied = re.search(r"\*UNKNOWN\s+2024\s+DONE IN 2024", listing)
        results.append(("PASS" if echoed else "FAIL") + " bug 28: DMBCI echoes the host species code")
        results.append(("PASS" if applied else "FAIL") + " bug 28: DMBCA's activity 2010 is applied by DMCYCL")

        run_deck(on_program, on, "ONCrnmult")
        (on / "ONCrnmultBase.key").write_text(
            "".join(line for line in (on / "ONCrnmult.key").read_text().splitlines(keepends=True)
                    if not line.startswith("CRNMULT")).replace("ONCrnmult.db", "ONCrnmultBase.db"))
        run_deck(on_program, on, "ONCrnmultBase")
        with sqlite3.connect(on / "ONCrnmult.db") as connection:
            connection.execute(f"ATTACH '{(on / 'ONCrnmultBase.db').as_posix()}' AS b")
            changed = dict(connection.execute(
                "SELECT t.SpeciesFVS, SUM(t.PctCr <> u.PctCr) FROM FVS_TreeList_East t "
                "JOIN b.FVS_TreeList_East u ON t.Year = u.Year AND t.TreeId = u.TreeId "
                "WHERE t.Year = 2004 GROUP BY t.SpeciesFVS").fetchall())
        connection.close()
        only_mh = changed.get("MH", 0) > 0 and all(v == 0 for k, v in changed.items() if k != "MH")
        results.append(("PASS" if only_mh else "FAIL")
                       + f" bug 44: first-cycle CRNMULT for MH changes MH crowns only {changed}")
    return results


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--component", choices=("formats", "decks", "all"), default="formats")
    parser.add_argument("--build-tree", type=Path, help="Built tree holding bin/FVSbc and bin/FVSon (decks)")
    args = parser.parse_args()
    if args.component in ("decks", "all") and not args.build_tree:
        parser.error("--component decks needs --build-tree")
    results = check_formats() if args.component in ("formats", "all") else []
    if args.component in ("decks", "all"):
        results += check_decks(args.build_tree.resolve())
    for line in results:
        print(line)
    failed = [line for line in results if line.startswith("FAIL")]
    print(("FAIL" if failed else "PASS") + f" Canadian repairs: {len(results) - len(failed)}/{len(results)}")
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
