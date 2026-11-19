"""Prove the SP03 poisoned-storage mode on a planted fixture.

A miniature PRGPRM and COMMON include stand in for a variant. The generator in
poison_storage.py raises MAXSP from its native 5 to 12, writes POISON_INACTIVE,
and the programs are compiled with the makefile's optimization and trap flags.

* Code that stays within the native extent runs and prints what it printed
  without poisoning.
* A planted real read of an inactive species slot stops with a floating-point
  exception.
* A planted integer read of an inactive slot changes the output.
* Logical and derived-extent arrays are reported as not poisoned.
* PRGPRM keeps exactly one rewritten size per name.
"""

from __future__ import annotations

import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile

sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).resolve().parent))
from poison_storage import native_values, poison_routine, raise_storage  # noqa: E402

WINDOWS = sys.platform == "win32"
FLAGS = ["-O2", "-ffp-contract=off", "-cpp",
         "-ffpe-trap=invalid,zero,underflow,overflow,denormal", "-fbacktrace"]

PRGPRM = """integer :: MAXSP,MAXTRE,MAXPLT
parameter (MAXSP = 5)
parameter (MAXTRE = 40)
parameter (MAXPLT = 3)
"""

COMMONS = """real    :: X(MAXSP),Z(MAXSP,3),T(MAXTRE)
integer :: I(MAXSP)
character(len=4) :: C(MAXSP)
logical :: L(MAXSP)
real    :: Q(MAXSP+1)
common /MINI/ X,Z,T,I,C,L,Q
"""

PROGRAM = """program FIXTURE
  implicit none
  include 'PRGPRM.f90'
  include 'MINICOM.f90'
  integer :: K, LAST
  real :: TOTAL
  integer :: ITOTAL
  ! What GRINIT would do for the default's own species and trees.
  do K = 1, NACTIVE
    X(K) = real(K)
    Z(K,:) = 0.5*real(K)
    I(K) = K
    C(K) = 'SP  '
  end do
  T(1:40) = 1.0
#ifdef POISON
  call POISON_INACTIVE
#endif
  LAST = READTO
  TOTAL = 0.0
  ITOTAL = 0
  do K = 1, LAST
    TOTAL = TOTAL + X(K) + Z(K,2)
  end do
#ifdef INTREAD
  ITOTAL = I(NACTIVE+1)
#endif
  write (*,'(A,F10.3,A,I0)') 'TOTAL=', TOTAL, ' ITOTAL=', ITOTAL
end program FIXTURE
"""


def build_and_run(work: Path, name: str, defines: list[str], poison: bool) -> subprocess.CompletedProcess:
    program = work / (name + (".exe" if WINDOWS else ""))
    sources = [str(work / "fixture.f90")] + ([str(work / "poison_inactive.f90")] if poison else [])
    subprocess.run(["gfortran", *FLAGS, *defines, f"-I{work}", f"-J{work / name}", *sources,
                    *(["-static"] if WINDOWS else []), "-o", str(program)],
                   cwd=work, check=True, capture_output=True, env=dict(os.environ))
    return subprocess.run([str(program)], cwd=work, capture_output=True, text=True, env=dict(os.environ))


def main() -> int:
    if not shutil.which("gfortran"):
        raise SystemExit("gfortran is required")
    results = []
    with tempfile.TemporaryDirectory(prefix="mfvs-poison-",
                                     dir="/private/tmp" if sys.platform == "darwin" else None) as name:
        work = Path(name)
        (work / "PRGPRM.f90").write_text(PRGPRM)
        (work / "MINICOM.f90").write_text(COMMONS)
        (work / "fixture.f90").write_text(PROGRAM)
        native = native_values(work / "PRGPRM.f90")
        assert native == {"MAXSP": 5, "MAXTRE": 40, "MAXPLT": 3}, native
        # Native build: the default as it is today, no poisoning.
        for sub in ("native", "clean", "real", "int"):
            (work / sub).mkdir()
        baseline = build_and_run(work, "native", ["-DNACTIVE=5", "-DREADTO=5"], poison=False)
        storage = {"MAXSP": 12, "MAXTRE": 60, "MAXPLT": 3}
        raise_storage(work / "PRGPRM.f90", storage)
        text = (work / "PRGPRM.f90").read_text()
        results.append(("storage raised once per size",
                        text.count("MAXSP = 12") == 1 and text.count("MAXTRE = 60") == 1))
        source, skipped = poison_routine([(str(work / "MINICOM.f90"), ["PRGPRM.f90", "MINICOM.f90"])],
                                         native, storage)
        (work / "poison_inactive.f90").write_text(source)
        results.append(("logical and derived-extent arrays reported",
                        any("L: logical" in s for s in skipped) and any(s.startswith("MINICOM.f90: Q(") for s in skipped)))
        clean = build_and_run(work, "clean", ["-DPOISON", "-DNACTIVE=5", "-DREADTO=5"], poison=True)
        results.append(("active-extent reads run unchanged under poisoning",
                        baseline.returncode == 0 and clean.returncode == 0 and clean.stdout == baseline.stdout))
        planted = build_and_run(work, "real", ["-DPOISON", "-DNACTIVE=5", "-DREADTO=MAXSP"], poison=True)
        trapped = planted.returncode != 0 and "Floating-point exception" in planted.stderr
        results.append(("planted real read of an inactive species slot traps", trapped))
        integer = build_and_run(work, "int", ["-DPOISON", "-DINTREAD", "-DNACTIVE=5", "-DREADTO=5"], poison=True)
        results.append(("planted integer read of an inactive slot changes output",
                         integer.returncode == 0 and integer.stdout != baseline.stdout and "-2147483647" in integer.stdout))
    for label, ok in results:
        print(("PASS " if ok else "FAIL ") + label)
    failed = [label for label, ok in results if not ok]
    print(("FAIL" if failed else "PASS") + f" poisoned-storage fixture: {len(results) - len(failed)}/{len(results)}")
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
