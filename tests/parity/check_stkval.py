"""Check STKVAL result bits, stocking classes, diagnostics and missing rows.

The frozen pre-optimization routine receives only the documented coefficient
row repair, making this a controlled corrected-baseline comparison. Original
F77 comparisons remain the responsibility of check_suite.py, unchanged.
"""

from __future__ import annotations

import argparse
from pathlib import Path
import re
import shutil
import subprocess
import sys
import tempfile

sys.dont_write_bytecode = True

ROOT = Path(__file__).resolve().parents[2]
BASELINE = "d4341283e4c115ce2352eca5eee2a45503ff118c"


def corrected_reference(source: str) -> str:
    """Apply the zero-row repair only; retain the original five READ sites."""
    source = source.replace("integer :: IS,IFIA,I3,I2,I1,ISPC",
                            "integer :: IS,IFIA,I3,I2,I1,ISPC,ICOEF")
    pattern = (r"(?m)^( +)B0 = TAB2\(TAB3\(IFIA,2\),1\)([^\n]*)\n"
               r"\1B1 = TAB2\(TAB3\(IFIA,2\),2\)([^\n]*)")

    def repair(match: re.Match) -> str:
        indent = match[1]
        return (indent + "ICOEF = TAB3(IFIA,2)\n"
                + indent + "if (ICOEF.eq.0) ICOEF = TAB3(999,2)\n"
                + indent + "B0 = TAB2(ICOEF,1)" + match[2] + "\n"
                + indent + "B1 = TAB2(ICOEF,2)" + match[3])

    source, count = re.subn(pattern, repair, source)
    if count != 4:
        raise ValueError("The frozen coefficient-row reference no longer has four sites")
    return source.replace("subroutine STKVAL(", "subroutine BASE_STKVAL(", 1)


def main() -> None:
    variants = sorted(path.stem[3:5] for path in (ROOT / "bin").glob("FVS??_sourceList.txt"))
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--variants", nargs="+", choices=variants, default=variants)
    args = parser.parse_args()
    original = subprocess.check_output(
        ["git", "-C", str(ROOT), "show", f"{BASELINE}:vbase/stkval.f90"], text=True)
    with tempfile.TemporaryDirectory(prefix="mfvs-stkval-") as temporary:
        directory = Path(temporary)
        (directory / "original.f90").write_text(corrected_reference(original))
        shutil.copy2(ROOT / "vbase/stkval.f90", directory / "current.f90")
        shutil.copy2(Path(__file__).with_name("stkval_cases.f90"), directory / "driver.f90")
        for variant in args.variants:
            # Use each consumer's actual include selection and COMMON dimensions.
            manifest = ROOT / "bin" / f"FVS{variant}_sourceList.txt"
            selected = {(manifest.parent / name).name: (manifest.parent / name).resolve()
                        for name in manifest.read_text().split()}
            for name in ("PRGPRM.f90", "CONTRL.f90", "ARRAYS.f90", "PLOT.f90"):
                shutil.copy2(selected[name], directory / name)
            subprocess.run(["gfortran", "-g", "-ffpe-trap=invalid,zero,underflow,overflow",
                            "-fbacktrace", "-fcheck=bounds", "original.f90", "current.f90",
                            "driver.f90", "-o", "check"], cwd=directory, check=True)
            subprocess.run([str(directory / "check")], cwd=directory, check=True)


if __name__ == "__main__":
    main()
