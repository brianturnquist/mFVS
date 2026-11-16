"""Check the R9HT range repair and R8PREPCOEF caller-owned coefficient records.

Build only the affected routines in a disposable directory. The R9HT oracle
is pinned original F77 with underflow halting disabled only in the fixture;
the repaired routine must match its result bits with underflow halting enabled.
R8 checks use explicit table values and retain caller-owned geometry fields.
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

from prepare_builds import REFERENCE_NVEL, ROOT


NVEL = ROOT / "volume/NVEL"
FLAGS = ["-g", "-ffpe-trap=invalid,zero,underflow,overflow", "-fbacktrace"]

POWER_DRIVER = """program POWER_CHECK
  use CLKCOEF_MOD
  use, intrinsic :: ieee_exceptions
  implicit none
  type(CLKCOEF) :: COEFFS
  real :: HEIGHTS(9),POWERS(14),DIAMETERS(3),EXPECTED,ACTUAL,BASE,LIMIT
  integer :: I,J,K,N,OLDERR,NEWERR,UNDERFLOWS
  logical :: UNDERFLOWED

  HEIGHTS = [17.3001,17.4,17.5,18.,20.,30.,60.,100.,500.]
  DIAMETERS = [0.5,0.9,1.2]
  N = 0
  UNDERFLOWS = 0
  do I = 1,size(HEIGHTS)
    BASE = 1.0-17.3/HEIGHTS(I)
    LIMIT = real(log(dble(tiny(BASE)))/log(dble(BASE)))
    POWERS = [1.,3.5,7.15667,13.62877,14.,14.5,17.80982,20.,30.,40., &
      LIMIT*(1.-4.e-7),LIMIT,LIMIT*(1.+4.e-7),LIMIT*1.1]
    do J = 1,size(POWERS)
      do K = 1,size(DIAMETERS)
        COEFFS = CLKCOEF(10.,8.,24.20656,0.46472,72.22792,POWERS(J), &
          2.27267,0.66773,0.,0.,0.,0.,HEIGHTS(I),0.,0.,0.,0.,0.,0)
        OLDERR = 0
        NEWERR = 0
        ! The original may form a subnormal Y. Its final height is the oracle;
        ! the candidate must produce those exact bits without suppressing traps.
        call ieee_set_halting_mode(ieee_underflow,.false.)
        call ieee_set_flag(ieee_underflow,.false.)
        call BASE_R9HT(EXPECTED,COEFFS,10.*DIAMETERS(K),OLDERR)
        call ieee_get_flag(ieee_underflow,UNDERFLOWED)
        if (UNDERFLOWED) UNDERFLOWS = UNDERFLOWS+1
        call ieee_set_flag(ieee_underflow,.false.)
        call ieee_set_halting_mode(ieee_underflow,.true.)
        call R9HT(ACTUAL,COEFFS,10.*DIAMETERS(K),NEWERR)
        if (transfer(ACTUAL,0).ne.transfer(EXPECTED,0).or.OLDERR.ne.NEWERR) stop 1
        N = N+1
      end do
    end do
  end do
  if (UNDERFLOWS.eq.0) stop 2
  write(*,*) 'PASS exact R9HT cases:',N,' original-underflow cases:',UNDERFLOWS
end program POWER_CHECK
"""

PREP_DRIVER = """program PREP_CHECK
  use CLKCOEF_MOD
  implicit none
  type(CLKCOEF) :: FIRST,SECOND,THIRD
  character(len=10) :: EQ
  integer :: ERR1,ERR2,WORDS(19)
  real :: EXPECTED(11)

  FIRST = CLKCOEF(13.,13.,13.,13.,13.,13.,13.,13.,13.,13.,13.,13., &
    13.,13.,13.,13.,13.,13.,13)
  SECOND = FIRST
  SECOND%DBHIB = 17.
  ERR1 = 0
  ERR2 = 0
  EQ = '810CLK0100'
  ! No intervening procedure call: the original retains matching stack bytes
  ! and skips filling SECOND, despite its independent coefficient record.
  call R8PREPCOEF(EQ,FIRST,ERR1)
  call R8PREPCOEF(EQ,SECOND,ERR2)
  EXPECTED = [24.20656,0.46472,72.22792,7.15667,2.27267,0.66773, &
    -0.41002,0.91414,0.83618,-0.91452,3.5]
  if (ERR1.ne.0.or.ERR2.ne.0.or.EQ.ne.'810CLK0100') stop 10
  call CHECK_TOTAL(FIRST)
  call CHECK_TOTAL(SECOND)
  if (FIRST%DBHIB.ne.13..or.SECOND%DBHIB.ne.17.) stop 11
  if (SECOND%DIB17.ne.13..or.SECOND%TOTHT.ne.13..or.SECOND%DX.ne.13.) stop 12
  if (SECOND%AFI.ne.13..or.SECOND%BFI.ne.13..or.SECOND%SPGRP.ne.13) stop 13

  THIRD = FIRST
  EQ = '814CLK0100'
  call R8PREPCOEF(EQ,THIRD,ERR1)
  if (any([THIRD%R,THIRD%C,THIRD%E,THIRD%P,THIRD%Q,THIRD%A17,THIRD%B17] &
    .ne.[19.57453,0.45136,94.08746,5.75423,0.87838,0.82213,-0.36900])) stop 14
  EQ = '810CLK0100'
  call R8PREPCOEF(EQ,THIRD,ERR1)
  call CHECK_TOTAL(THIRD)
  if (THIRD%Q.ne.0.87838) stop 15

  ! Species aliases must fill a new destination too; success does not clear
  ! a caller's existing error value. Invalid equations leave the coefficient
  ! record untouched.
  EQ = '810CLK0123'
  ERR2 = 37
  call R8PREPCOEF(EQ,SECOND,ERR2)
  call CHECK_TOTAL(SECOND)
  if (ERR2.ne.37.or.EQ.ne.'810CLK0123') stop 16
  WORDS = transfer(SECOND,WORDS)
  EQ = '880CLK0100'
  call R8PREPCOEF(EQ,SECOND,ERR2)
  if (ERR2.ne.1.or.any(transfer(SECOND,WORDS).ne.WORDS)) stop 17
  EQ = '810CLK0999'
  call R8PREPCOEF(EQ,SECOND,ERR2)
  if (ERR2.ne.6.or.any(transfer(SECOND,WORDS).ne.WORDS)) stop 18
  write(*,*) 'PASS R8PREPCOEF repeated/alternating records, aliases and errors'
contains
  subroutine CHECK_TOTAL(C)
    implicit none
    type(CLKCOEF), intent(in) :: C
    if (any([C%R,C%C,C%E,C%P,C%B,C%A,C%A4,C%B4,C%A17,C%B17,C%FIXDI] &
      .ne.EXPECTED)) stop 19
  end subroutine CHECK_TOTAL
end program PREP_CHECK
"""


def routine(source: str, name: str) -> str:
    """Retain the source form while selecting one complete program unit."""
    lines = source.splitlines(keepends=True)
    start = next(i for i, line in enumerate(lines)
                 if re.match(rf"\s*subroutine\s+{name}\s*\(", line, re.I))
    end = next(i for i in range(start + 1, len(lines))
               if re.fullmatch(r"\s*end(?:\s+subroutine(?:\s+\w+)?)?\s*", lines[i], re.I))
    return "".join(lines[start:end + 1])


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--component", choices=("all", "r9ht", "r8prep"), default="all")
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix="mfvs-nvel-repairs-") as temporary:
        directory = Path(temporary)
        shutil.copy2(NVEL / "clkcoef_mod.f90", directory)
        if args.component in ("all", "r9ht"):
            original = subprocess.check_output(
                ["git", "-C", str(NVEL), "show", f"{REFERENCE_NVEL}:r9clark.f"], text=True)
            original = routine(original, "r9ht")
            original = re.sub(r"(subroutine\s+)r9ht", r"\1BASE_R9HT", original, count=1, flags=re.I)
            (directory / "original.f").write_text(original)
            (directory / "current.f90").write_text(routine((NVEL / "r9clark.f90").read_text(), "r9ht"))
            (directory / "power.f90").write_text(POWER_DRIVER)
            subprocess.run(["gfortran", *FLAGS, "clkcoef_mod.f90", "original.f", "current.f90",
                            "power.f90", "-o", "power"], cwd=directory, check=True)
            subprocess.run([str(directory / "power")], cwd=directory, check=True)
        if args.component in ("all", "r8prep"):
            for name in ("r8clkcoef.inc", "r8dib.inc"):
                shutil.copy2(NVEL / name, directory)
            (directory / "prep.f90").write_text(routine((NVEL / "r8clkdib.f90").read_text(), "R8PREPCOEF"))
            (directory / "records.f90").write_text(PREP_DRIVER)
            for flags in ([], ["-fno-automatic"]):
                subprocess.run(["gfortran", *FLAGS, *flags, "clkcoef_mod.f90", "prep.f90",
                                "records.f90", "-o", "records"], cwd=directory, check=True)
                subprocess.run([str(directory / "records")], cwd=directory, check=True)


if __name__ == "__main__":
    main()
