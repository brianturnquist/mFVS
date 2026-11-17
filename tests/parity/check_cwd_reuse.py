"""Compare paired CWD entries with the pre-reuse routine, including fallbacks.

The volume stub counts calls and returns a geometry-dependent result. Native
provider integration, diagnostics, RNG and restart checks use the full suite
and exercising decks separately. No generated files remain in the checkout.
"""

from pathlib import Path
import shutil
import subprocess
import sys
import tempfile

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[2]
BASELINE = "e27f434845562f16a7db2e96a8f2f76da801b97c"


def main() -> None:
    original = subprocess.check_output(
        ["git", "-C", str(ROOT), "show", f"{BASELINE}:fire/base/fmcwd.f90"], text=True)
    original = original.replace("subroutine FMCWD(", "subroutine BASE_FMCWD(", 1)
    for entry in ("CWD1", "CWD2", "CWD3"):
        original = original.replace(f"entry {entry}(", f"entry BASE_{entry}(", 1)
    with tempfile.TemporaryDirectory(prefix="mfvs-cwd-reuse-") as temporary:
        directory = Path(temporary)
        (directory / "original.f90").write_text(original)
        shutil.copy2(ROOT / "fire/base/fmcwd.f90", directory / "current.f90")
        shutil.copy2(ROOT / "volume/NVEL/debug_mod.f90", directory / "debug_mod.f90")
        shutil.copy2(Path(__file__).with_name("cwd_reuse_cases.f90"), directory / "driver.f90")
        for manifest in sorted((ROOT / "bin").glob("FVS??_sourceList.txt")):
            selected = {(manifest.parent / name).name: (manifest.parent / name).resolve()
                        for name in manifest.read_text().split()}
            for name in ("PRGPRM.f90", "FMPARM.f90", "FMCOM.f90", "FMFCOM.f90",
                         "CONTRL.f90", "DBSTK.f90", "VOLSTD.f90"):
                shutil.copy2(selected[name], directory / name)
            subprocess.run(["gfortran", "-g", "-fcheck=all", "-finit-real=snan",
                            "-ffpe-trap=invalid,zero,underflow,overflow", "-fbacktrace",
                            "debug_mod.f90", "original.f90", "current.f90", "driver.f90",
                            "-o", "check"], cwd=directory, check=True)
            result = subprocess.run([str(directory / "check")], cwd=directory, check=True,
                                    capture_output=True, text=True)
            print(result.stdout.splitlines()[-1])


if __name__ == "__main__":
    main()
