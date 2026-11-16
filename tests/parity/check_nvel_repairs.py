"""Check the R9HT, R8PREPCOEF and VOLINITNVB NVEL correctness repairs.

Build only the affected routines in a disposable directory. The R9HT oracle
is pinned original F77 with underflow halting disabled only in the fixture;
the repaired routine must match its result bits with underflow halting enabled.
R8 checks use explicit table values and retain caller-owned geometry fields.
The VOLINITNVB check fills the stack with a chosen byte before each call. The
repaired routine must give the same bits for every byte; the frozen pre-repair
routine must match them for a blank and must change for 'P' (bug 51).
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

from prepare_builds import REFERENCE_NVEL, ROOT, archive
from check_nvel_parsing import BASELINE_NVEL, FLAGS as SUITE_FLAGS, compile_nvel, listed_nvel


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


NVBEQN_DRIVER = """program NVBEQN_CHECK
  use MRULES_MOD
  implicit none
  ! Default (non-NSVB) equations reach NVB_DefaultEq and NVBC.
  character(len=2), parameter :: VARS(5) = ['SN','PN','IE','CA','LS']
  integer, parameter :: REGIONS(5) = [8,6,1,5,9]
  character(len=2), parameter :: FORESTS(5) = ['05','12','03','05','09']
  integer, parameter :: SPECIES(5,6) = reshape([ &
    131,111,316,802,611,621, 202,263,351,242,122,17, &
    202,122,73,263,108,19, 122,81,15,117,202,631, 316,129,371,833,12,95], [5,6], order=[2,1])
  real, parameter :: DBHS(2) = [10.0,18.0], HEIGHTS(2) = [60.0,90.0]
  character(len=1), parameter :: LIVES(2) = ['L','D']
  character(len=1) :: PAINTS(6)
  character(len=10) :: EQ10
  integer :: V,K,T,L,P,ERR
  PAINTS = [' ','P','0','7',achar(0),achar(255)]
  do V = 1,size(VARS)
    do K = 1,size(SPECIES,2)
      ! Obtain the equation first, so that VOLEQDEF's frame cannot overwrite
      ! the filled stack that VOLINITNVB will occupy.
      call VOLEQDEF(VARS(V),REGIONS(V),FORESTS(V),'01',SPECIES(V,K),'01',EQ10,ERR)
      do T = 1,2
        do L = 1,2
          do P = 1,size(PAINTS)
            call PAINT(PAINTS(P))
            call ONE_TREE(VARS(V),REGIONS(V),FORESTS(V),EQ10,SPECIES(V,K),DBHS(T), &
              HEIGHTS(T),LIVES(L),P)
          end do
        end do
      end do
    end do
  end do
end program NVBEQN_CHECK

subroutine PAINT(C)
  implicit none
  character(len=1), intent(in) :: C
  character(len=32768) :: AREA
  AREA = repeat(C,len(AREA))
  call KEEP(AREA)
end subroutine PAINT

subroutine KEEP(AREA)
  implicit none
  character(len=*) :: AREA
  if (AREA(1:1).eq.'#') write(*,*) 'unexpected fill'
end subroutine KEEP

subroutine ONE_TREE(VAR,REGION,FOREST,EQ10,SPECIES,DBH,HEIGHT,LIVE_IN,PAINT)
  use MRULES_MOD
  implicit none
  character(len=2), intent(in) :: VAR,FOREST
  character(len=10), intent(in) :: EQ10
  character(len=1), intent(in) :: LIVE_IN
  integer, intent(in) :: REGION,SPECIES,PAINT
  real, intent(in) :: DBH,HEIGHT
  integer :: REGN,HTLOG,HTREF,FCLASS,DECAYCD,TLOGS,CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG
  integer :: HTTFLL,BA,SI,ERRFLAG,IDIST,FIASPCD,MRULEFLG
  real :: MTOPP,MTOPS,STUMP,DBHOB,DRCOB,HTTOT,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2
  real :: AVGZ1,AVGZ2,DBTBH,BTR,CR,CULL,NOLOGP,NOLOGS,BRKHT,BRKHTD,CULLMSTOP
  real :: VOL(15),LOGVOL(7,20),LOGDIA(21,3),LOGLEN(20),BOLHT(21),DRYBIO(15),GRNBIO(15)
  character(len=11) :: VOLEQ
  character(len=4) :: CONSPEC
  character(len=2) :: FORST,PROD
  character(len=1) :: HTTYPE,LIVE,CTYPE
  type(MERCHRULES) :: MERRULES
  REGN = REGION; FORST = FOREST; PROD = '01'; VOLEQ = EQ10
  MTOPP = 4; MTOPS = 4; STUMP = 0.5; DBHOB = DBH; DRCOB = 0; HTTYPE = 'F'; HTTOT = HEIGHT
  HTLOG = 0; HT1PRD = 0; HT2PRD = 0; UPSHT1 = 0; UPSHT2 = 0; UPSD1 = 0; UPSD2 = 0
  HTREF = 0; AVGZ1 = 0; AVGZ2 = 0; FCLASS = 0; DBTBH = 0; BTR = 0; CR = 40; CULL = 0
  DECAYCD = 0; CUTFLG = 1; BFPFLG = 1; CUPFLG = 1; CDPFLG = 1; SPFLG = 1; CONSPEC = '    '
  HTTFLL = 0; LIVE = LIVE_IN; BA = 0; SI = 0; CTYPE = 'F'; IDIST = 1; BRKHT = 0; BRKHTD = 0
  FIASPCD = SPECIES; MRULEFLG = 0; CULLMSTOP = 0
  VOL = 0; LOGVOL = 0; LOGDIA = 0; LOGLEN = 0; BOLHT = 0; DRYBIO = 0; GRNBIO = 0
  call VOLINITNVB(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,DRCOB,HTTYPE,HTTOT,HTLOG, &
    HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,CR,CULL, &
    DECAYCD,VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,BFPFLG,CUPFLG, &
    CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,BA,SI,CTYPE,ERRFLAG,IDIST,BRKHT,BRKHTD, &
    FIASPCD,DRYBIO,GRNBIO,MRULEFLG,MERRULES,CULLMSTOP)
  write(*,'(i1,1x,a2,1x,i4,1x,a10,1x,a1,1x,i3,*(1x,z8.8))') PAINT,VAR,SPECIES,EQ10, &
    LIVE,ERRFLAG,TLOGS,transfer([DBH,HEIGHT,NOLOGP,NOLOGS,VOL,DRYBIO,GRNBIO, &
    reshape(LOGVOL,[140]),reshape(LOGDIA,[63]),LOGLEN,BOLHT],0,330)
end subroutine ONE_TREE
"""


def routine(source: str, name: str) -> str:
    """Retain the source form while selecting one complete program unit."""
    lines = source.splitlines(keepends=True)
    start = next(i for i, line in enumerate(lines)
                 if re.match(rf"\s*subroutine\s+{name}\s*\(", line, re.I))
    end = next(i for i in range(start + 1, len(lines))
               if re.fullmatch(r"\s*end(?:\s+subroutine(?:\s+\w+)?)?\s*", lines[i], re.I))
    return "".join(lines[start:end + 1])


def check_nvbeqn(directory: Path) -> None:
    """Bug 51: VOLINITNVB must not pass NSVB an undefined eleventh character."""
    library = compile_nvel(NVEL, listed_nvel(), directory / "nvel-build", 8)
    frozen = directory / "frozen-nvel"
    archive(NVEL, BASELINE_NVEL, frozen, directory / "frozen-nvel.tar")
    (directory / "nvbeqn.f90").write_text(NVBEQN_DRIVER)
    build = ["gfortran", *SUITE_FLAGS, f"-I{directory / 'nvel-build'}", f"-J{directory}"]
    subprocess.run([*build, "-c", str(frozen / "volinit.f90"), "-o", "volinit_frozen.o"],
                   cwd=directory, check=True)
    outputs = {}
    for label, extra in (("repaired", []), ("frozen", ["volinit_frozen.o"])):
        subprocess.run([*build, "nvbeqn.f90", *extra, str(library), "-o", f"nvbeqn-{label}"],
                       cwd=directory, check=True)
        lines = subprocess.run([str(directory / f"nvbeqn-{label}")], cwd=directory, check=True,
                               capture_output=True, text=True).stdout.splitlines()
        outputs[label] = {}
        for line in lines:
            outputs[label].setdefault(int(line[0]), []).append(line[2:])
    repaired, frozen_runs = outputs["repaired"], outputs["frozen"]
    cases = len(repaired[1])
    if any(repaired[p] != repaired[1] for p in repaired):
        raise RuntimeError("repaired VOLINITNVB depends on the stack contents")
    if frozen_runs[1] != repaired[1]:
        raise RuntimeError("repaired VOLINITNVB differs from the pre-repair blank-byte results")
    changed = {p: sum(a != b for a, b in zip(frozen_runs[p], repaired[1])) for p in frozen_runs}
    if changed[2] == 0:
        raise RuntimeError("the stack fill did not reach NVBEQN(11:11); the check is not sensitive")
    # After the fill digit: A2, I4, A10, A1 and blanks precede ERRFLAG's I3.
    computed = sum(int(line[21:24]) == 0 for line in repaired[1])
    print(f"PASS VOLINITNVB: {cases} cases ({computed} with ERRFLAG 0) identical for six stack fills; "
          f"pre-repair results changed in {changed[2]} cases with 'P' and "
          f"{sum(changed[p] for p in (3, 4, 5, 6))} with digit/NUL/255 fills")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--component", choices=("all", "r9ht", "r8prep", "nvbeqn"), default="all")
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
        if args.component in ("all", "nvbeqn"):
            check_nvbeqn(directory)


if __name__ == "__main__":
    main()
