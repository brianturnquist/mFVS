"""Check the Canadian snag-volume argument and merchantability repairs.

Use the selected native COMMON dimensions and the real Behre correction.
Small volume-provider stubs isolate their documented two-result contracts;
the full-suite and native snag decks remain separate integration checks.
"""

from pathlib import Path
import shutil
import subprocess
import sys
import tempfile

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[2]


def main() -> None:
    with tempfile.TemporaryDirectory(prefix="mfvs-snag-volumes-") as temporary:
        directory = Path(temporary)
        for variant in ("bc", "on"):
            manifest = ROOT / "bin" / f"FVS{variant}_sourceList.txt"
            selected = {(manifest.parent / name).name: (manifest.parent / name).resolve()
                        for name in manifest.read_text().split()}
            for name in ("PRGPRM.f90", "FMPARM.f90", "FMCOM.f90", "CONTRL.f90",
                         "COEFFS.f90", "fmsvol.f90", "cftopk.f90", "behprm.f90", "behre.f90"):
                shutil.copy2(selected[name], directory / name)
            shutil.copy2(Path(__file__).with_name("snag_volume_cases.f90"), directory / "driver.f90")
            for extra in ([], ["-finit-real=snan"]):
                subprocess.run(["gfortran", "-g", "-fcheck=all",
                                "-ffpe-trap=invalid,zero,underflow,overflow", "-fbacktrace",
                                *extra, "fmsvol.f90", "cftopk.f90", "behprm.f90", "behre.f90",
                                "driver.f90", "-o", "check"], cwd=directory, check=True)
                subprocess.run([str(directory / "check")], cwd=directory, check=True)


if __name__ == "__main__":
    main()
