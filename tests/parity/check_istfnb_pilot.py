"""Check the MOD07 ISTFNB pilot: SUMHED screen output and the checked interface.

The regression suite does not compare screen output, and SUMHED is ISTFNB's only
internal caller. For each variant this builds one driver twice with that
variant's own include files: once with the frozen pre-pilot ISTFNB and SUMHED
(from --frozen, default PRE_PILOT), once with the working tree's ISTFNB_MOD, external
wrapper and SUMHED. Both runs must print the same bytes for

* SUMHED's header for several titles, including blank, leading-blank, full
  72-character, tab and NUL titles, with screen output on and off; and
* ISTFNB on all 87,381 strings of length 0-8 over blank, 'a', tab and NUL,
  each also checked against VERIFY(STRING,' ').

The working-tree run also requires the module function and the external
wrapper to agree on every string. Finally, a call passing an INTEGER to the
module's ISTFNB must be rejected by the compiler, while the same call to the
legacy external is not: that difference is what the pilot adds.

REVISE is the only stub (a fixed revision string), so no Git metadata enters
the output. Everything is built and run in a temporary directory.
"""

from __future__ import annotations

import argparse
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[2]
WINDOWS = sys.platform == "win32"
FLAGS = ["-O2", "-ffp-contract=off", "-g", "-cpp", "-fcheck=bounds",
         "-ffpe-trap=invalid,zero,underflow,overflow,denormal"]
# Last commit before MOD07; its ISTFNB and SUMHED are the pre-pilot oracle.
PRE_PILOT = "0cf347e4f58659de3b8f277fcb72d970d0a332f4"
FROZEN_SOURCES = ("base/istfnb.f90", "vbase/sumhed.f90", "base/istlnb.f90")
CURRENT_SOURCES = ("base/istfnb_mod.f90", "base/istfnb.f90", "vbase/sumhed.f90", "base/istlnb.f90")

REVISE = """subroutine REVISE(VAR, REV)
  implicit none
  character(len=*) :: VAR
  character(len=10) :: REV
  REV = 'PILOTREV  '
end subroutine REVISE
"""

DRIVER = """program PILOT
#ifdef CURRENT
  use ISTFNB_MOD, only: ISTFNB_CHECKED => ISTFNB
#endif
  implicit none
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'OUTCOM.f90'
  include 'PLOT.f90'
  include 'SCREEN.f90'
  integer, external :: ISTFNB
  character(len=72) :: TITLES(7)
  character(len=8) :: S
  character(len=1), parameter :: ALPHABET(0:3) = [' ', 'a', achar(9), achar(0)]
  integer :: I, K, N, CODE, LENGTH, TOTAL, BAD, DIFFER, SUMS
  integer(kind=8) :: WEIGHTED

  JOSCRN = 6
  VARACD = VARCODE
  IMODTY = 1
  NPLT = 'STAND-0001'
  MGMID = 'MGMT'
  TITLES(1) = ' '
  TITLES(2) = 'PILOT TITLE'
  TITLES(3) = '      LEADING BLANKS'
  TITLES(4) = repeat('X', 72)
  TITLES(5) = repeat(' ', 71) // 'Z'
  TITLES(6) = achar(9) // 'TAB TITLE'
  TITLES(7) = achar(0) // 'NUL TITLE'
  do I = 1, size(TITLES)
    ITITLE = TITLES(I)
    LSCRN = .true.
    write (*,'(A,I0)') '--- SUMHED title case ', I
    call SUMHED
  end do
  LSCRN = .false.
  write (*,'(A)') '--- SUMHED with LSCRN false'
  call SUMHED

  TOTAL = 0
  BAD = 0
  DIFFER = 0
  SUMS = 0
  WEIGHTED = 0
  do LENGTH = 0, 8
    do CODE = 0, 4**LENGTH - 1
      N = CODE
      S = ' '
      do K = 1, LENGTH
        S(K:K) = ALPHABET(mod(N, 4))
        N = N / 4
      end do
      I = ISTFNB(S(1:LENGTH))
      TOTAL = TOTAL + 1
      if (I .ne. verify(S(1:LENGTH), ' ')) BAD = BAD + 1
#ifdef CURRENT
      if (I .ne. ISTFNB_CHECKED(S(1:LENGTH))) DIFFER = DIFFER + 1
#endif
      SUMS = SUMS + I
      WEIGHTED = WEIGHTED + int(I, 8) * int(TOTAL, 8)
    end do
  end do
  write (*,'(A,I0,A,I0,A,I0,A,I0)') '--- strings ', TOTAL, ' verify-mismatches ', BAD, &
       ' sum ', SUMS, ' weighted ', WEIGHTED
#ifdef CURRENT
  if (DIFFER .ne. 0) then
    write (0,'(A,I0)') 'module and external ISTFNB differ on strings: ', DIFFER
    stop 3
  end if
#endif
end program PILOT
"""

CONTROL_CHECKED = """subroutine BADCALL
  use ISTFNB_MOD, only: ISTFNB
  implicit none
  integer :: I
  I = ISTFNB(5)
end subroutine BADCALL
"""

CONTROL_LEGACY = """subroutine BADLEGACY
  implicit none
  integer :: I, ISTFNB
  I = ISTFNB(5)
end subroutine BADLEGACY
"""


def variants() -> list[str]:
    return sorted(path.name[3:5] for path in (ROOT / "bin").glob("FVS??_sourceList.txt"))


def stage_includes(variant: str, target: Path) -> None:
    """Copy the variant's selected include files, as the build stages them."""
    listed = {(ROOT / name).resolve() for name in (ROOT / "bin/fortran_includes.txt").read_text().split()}
    manifest = ROOT / "bin" / f"FVS{variant}_sourceList.txt"
    target.mkdir(parents=True)
    for name in manifest.read_text().split():
        path = (manifest.parent / name).resolve()
        if path in listed or path.suffix.lower() in (".f77", ".inc"):
            shutil.copy2(path, target / path.name)


def compile_and_run(directory: Path, variant: str, sources: list[Path], includes: Path,
                    current: bool, env: dict[str, str]) -> bytes:
    directory.mkdir()
    driver = directory / "pilot.f90"
    driver.write_text(DRIVER)
    (directory / "revise.f90").write_text(REVISE)
    program = directory / ("pilot.exe" if WINDOWS else "pilot")
    # SUMHED selects its header by the variant code (CR, eastern, western).
    command = ["gfortran", *FLAGS, *(["-DCURRENT"] if current else []),
               f"-DVARCODE='{variant.upper()}'", f"-I{includes}",
               f"-J{directory}", *map(str, sources), str(directory / "revise.f90"), str(driver),
               *(["-static"] if WINDOWS else []), "-o", str(program)]
    subprocess.run(command, cwd=directory, env=env, check=True, capture_output=True)
    return subprocess.run([str(program)], cwd=directory, env=env, check=True,
                          capture_output=True).stdout


def check_variant(variant: str, frozen: str, work: Path, env: dict[str, str]) -> str:
    root = work / variant
    includes = root / "include"
    stage_includes(variant, includes)
    frozen_dir = root / "frozen-src"
    frozen_dir.mkdir()
    frozen_sources = []
    for name in FROZEN_SOURCES:
        path = frozen_dir / Path(name).name
        path.write_bytes(subprocess.check_output(["git", "-C", str(ROOT), "show", f"{frozen}:{name}"]))
        frozen_sources.append(path)
    before = compile_and_run(root / "frozen", variant, frozen_sources, includes, False, env)
    after = compile_and_run(root / "current", variant, [ROOT / name for name in CURRENT_SOURCES],
                            includes, True, env)
    if before != after:
        return f"FAIL {variant}: screen/string output differs ({len(before)} and {len(after)} bytes)"
    titles = before.count(b"--- SUMHED title case")
    if titles != 7 or b"verify-mismatches 0 " not in before or b"strings 87381 " not in before:
        return f"FAIL {variant}: unexpected output\n{before.decode(errors='replace')[-400:]}"
    return f"PASS {variant}: {len(before)} bytes identical; 7 titles, 87,381 strings"


def check_control(work: Path, env: dict[str, str]) -> str:
    directory = work / "control"
    directory.mkdir()
    subprocess.run(["gfortran", "-c", f"-J{directory}", str(ROOT / "base/istfnb_mod.f90"),
                    "-o", str(directory / "istfnb_mod.o")], cwd=directory, env=env, check=True,
                   capture_output=True)
    (directory / "checked.f90").write_text(CONTROL_CHECKED)
    (directory / "legacy.f90").write_text(CONTROL_LEGACY)
    checked = subprocess.run(["gfortran", "-c", f"-I{directory}", "checked.f90"], cwd=directory,
                             env=env, capture_output=True, text=True)
    legacy = subprocess.run(["gfortran", "-c", "legacy.f90"], cwd=directory, env=env,
                            capture_output=True, text=True)
    if checked.returncode == 0 or "Type mismatch in argument" not in checked.stderr:
        return "FAIL control: an INTEGER argument to ISTFNB_MOD's ISTFNB was not rejected"
    if legacy.returncode != 0:
        return "FAIL control: the legacy external call no longer compiles"
    return "PASS control: the checked call is rejected; the legacy external call is not"


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--frozen", default=PRE_PILOT,
                        help="Revision holding the pre-pilot sources (default: the commit before MOD07)")
    parser.add_argument("--variants", nargs="+", choices=variants(), default=variants())
    args = parser.parse_args()
    if not shutil.which("gfortran"):
        parser.error("gfortran is required")
    frozen = subprocess.check_output(["git", "-C", str(ROOT), "rev-parse", args.frozen], text=True).strip()
    env = dict(os.environ)
    with tempfile.TemporaryDirectory(prefix="mfvs-istfnb-",
                                     dir="/private/tmp" if sys.platform == "darwin" else None) as name:
        work = Path(name)
        results = [check_variant(variant, frozen, work, env) for variant in args.variants]
        results.append(check_control(work, env))
    for line in results:
        print(line)
    failed = [line for line in results if line.startswith("FAIL")]
    print(("FAIL " if failed else "PASS ") + f"ISTFNB pilot vs frozen {frozen[:8]}: "
          f"{len(results) - len(failed)}/{len(results)} checks")
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
