"""Check NVEL's digit conversion helpers and the routines that use them.

`digits` compares each helper's call-site pattern, which falls back to the
original formatted statement, with that statement alone.
- DIGITREAD: every all-digit field up to five characters, sampled wider fields
  with and without blanks, all one- and two-character fields, and selected
  three- and four-character fields with blanks, signs and other characters.
  DIGITREAD must take exactly the fields of digits and blanks. A narrower READ
  of a 4-character record must equal DIGITREAD of its leading characters.
- DIGITWRITE: widths 1-9 with records up to three characters longer, every
  value from -1000 to 99999, powers of ten, the integer limits and samples.
Values, IOSTAT codes and records (bytes beyond them included) must match, and
a declined call must leave its variable unchanged.

`nsvb` and `volinit` build the NVEL sources listed in the variant source lists
twice: at a frozen revision before the changes and from the working tree.
One compiled driver is linked with each build, and every case output,
mutated arguments included, must match byte for byte. Invalid fields must stop
both builds with the same runtime error. `nsvb` (nsvb_cases.f90, before O06)
covers SPEQCOEF, CALC_EQVW and NVBC. `volinit` (volinit_cases.f90, before
O09) covers NVB_EcoProv, GetRegnWF, NVB_DefaultEq, VOLINIT and VOLINITNVB.

`natcrs` compares O12's NATCRS statements with the originals over every FIA and
alpha species code in the variants' block data and forest numbers -150-1200.
"""

from __future__ import annotations

import argparse
from concurrent.futures import ThreadPoolExecutor
import filecmp
from pathlib import Path
import re
import subprocess
import sys
import tempfile

sys.dont_write_bytecode = True

from prepare_builds import ROOT, archive, copy_working_tree

NVEL = ROOT / "volume/NVEL"
BASELINE_NVEL = "d0e634fad1f91a2897bdc5f0c07bd0d21f658750"  # before O06-O08
PRE_O09_NVEL = "9655442c7097305285720c89081215ad15bab868"  # before O09-O11
# The suite's Fortran flags, without warnings or version macros.
FLAGS = ["-g", "-cpp", "-DCMPgcc",
         "-ffpe-trap=invalid,zero,underflow,overflow,denormal", "-fbacktrace"]

DIGITS_DRIVER = """program DIGITS_CHECK
  use DIGITREAD_MOD
  implicit none
  character(len=1) :: ALPHABET(21)
  character(len=9) :: TEXT
  integer :: I,J,K,L,W,N,FAST,FALLBACK,REJECTED
  integer(kind=8) :: STATE

  ALPHABET = ['0','1','5','9',' ','+','-','.','/',':','A','P','d','e',',', &
    '*',achar(9),achar(0),achar(127),achar(255),'D']
  FAST = 0
  FALLBACK = 0
  REJECTED = 0
  N = 0
  ! Every all-digit field of one to five characters.
  do W = 1,5
    do I = 0,10**W-1
      write(TEXT(1:W),'(i0.'//achar(48+W)//')') I
      call CHECK(TEXT(1:W))
    end do
  end do
  ! Wider all-digit fields: each digit in each position, and a sample.
  STATE = 1602_8
  do W = 6,9
    do I = 1,W
      do J = 0,9
        TEXT(1:W) = repeat('0',W)
        TEXT(I:I) = achar(48+J)
        call CHECK(TEXT(1:W))
        TEXT(1:W) = repeat('9',W)
        TEXT(I:I) = achar(48+J)
        call CHECK(TEXT(1:W))
      end do
    end do
    do I = 1,20000
      do J = 1,W
        STATE = mod(48271_8*STATE,2147483647_8)
        TEXT(J:J) = achar(48+int(mod(STATE,10_8)))
      end do
      call CHECK(TEXT(1:W))
      ! The same width with about a quarter of the characters blank.
      do J = 1,W
        STATE = mod(48271_8*STATE,2147483647_8)
        if (mod(STATE,4_8).eq.0) TEXT(J:J) = ' '
      end do
      call CHECK(TEXT(1:W))
    end do
  end do
  call CHECK('         ')
  call CHECK('    7    ')
  ! Ten digits can overflow; DIGITREAD must leave them to the READ.
  call CHECK('0000000001')
  call CHECK('2147483647')
  call CHECK('9999999999')
  call CHECK('')
  ! Every one- and two-character field.
  do I = 0,255
    call CHECK(achar(I))
    do J = 0,255
      call CHECK(achar(I)//achar(J))
    end do
  end do
  ! Three- and four-character fields over digits and other characters.
  do I = 1,size(ALPHABET)
    do J = 1,size(ALPHABET)
      do K = 1,size(ALPHABET)
        call CHECK(ALPHABET(I)//ALPHABET(J)//ALPHABET(K))
        do L = 1,size(ALPHABET)
          call CHECK(ALPHABET(I)//ALPHABET(J)//ALPHABET(K)//ALPHABET(L))
        end do
      end do
    end do
  end do
  write(*,'(a,i9,a,i8,a,i8,a,i9,a)') 'PASS DIGITREAD:',N,' fields;',FAST, &
    ' converted directly,',FALLBACK,' accepted only by the READ,',REJECTED, &
    ' rejected by the READ'
  ! A narrower READ of a longer variable, as in READ(CONSPEC,'(I3)') with a
  ! 4-character CONSPEC, must equal DIGITREAD of the leading characters.
  N = 0
  do I = 1,size(ALPHABET)
    do J = 1,size(ALPHABET)
      do K = 1,size(ALPHABET)
        do L = 1,size(ALPHABET)
          do W = 1,3
            call CHECK_PREFIX(ALPHABET(I)//ALPHABET(J)//ALPHABET(K)//ALPHABET(L),W)
          end do
        end do
      end do
    end do
  end do
  write(*,'(a,i9,a)') 'PASS DIGITREAD prefix:',N,' I1-I3 reads of 4-character records'
  ! DIGITWRITE against the original internal WRITE: widths 1-9, records up to
  ! three characters longer, every value from -1000 to 99999, both sides of
  ! every power of ten, the integer limits and a sample of each width.
  N = 0
  FAST = 0
  do W = 1,9
    do L = W,W+3
      do I = -1000,99999
        call CHECK_WRITE(I,W,L)
      end do
      do K = 0,9
        call CHECK_WRITE(10**K-1,W,L)
        call CHECK_WRITE(10**K,W,L)
        call CHECK_WRITE(10**K+1,W,L)
        call CHECK_WRITE(-(10**K),W,L)
      end do
      call CHECK_WRITE(huge(0),W,L)
      call CHECK_WRITE(-huge(0)-1,W,L)
      do J = 1,5000
        STATE = mod(48271_8*STATE,2147483647_8)
        call CHECK_WRITE(int(mod(STATE,2_8*10_8**W)),W,L)
      end do
    end do
  end do
  write(*,'(a,i9,a,i9,a)') 'PASS DIGITWRITE:',N,' writes;',FAST,' converted directly'
contains
  subroutine CHECK_WRITE(VALUE,W,L)
    integer, intent(in) :: VALUE,W,L
    character(len=12) :: ORIGINAL,CANDIDATE
    character(len=8) :: FORM
    integer :: IOS1,IOS2
    logical :: TAKEN
    write(FORM,'(a,i0,a)') '(i',W,')'
    ! Bytes past the record must stay untouched on both sides.
    ORIGINAL = repeat('Q',12)
    CANDIDATE = ORIGINAL
    IOS1 = 0
    IOS2 = 0
    write(ORIGINAL(1:L),FORM,iostat=IOS1) VALUE
    TAKEN = DIGITWRITE(VALUE,CANDIDATE(1:L),W)
    if (TAKEN.neqv.(VALUE.ge.0.and.VALUE.lt.10**W)) stop 6
    if (.not.TAKEN) then
      if (CANDIDATE.ne.repeat('Q',12)) stop 7
      write(CANDIDATE(1:L),FORM,iostat=IOS2) VALUE
    end if
    if (ORIGINAL.ne.CANDIDATE.or.IOS1.ne.IOS2) then
      write(*,*) 'write differs:',VALUE,W,L,' [',ORIGINAL,'] [',CANDIDATE,']',IOS1,IOS2
      stop 8
    end if
    if (TAKEN.and.IOS1.ne.0) stop 9
    N = N+1
    if (TAKEN) FAST = FAST+1
  end subroutine CHECK_WRITE

  subroutine CHECK_PREFIX(RECORD,W)
    character(len=4), intent(in) :: RECORD
    integer, intent(in) :: W
    character(len=8) :: FORM
    integer :: ORIGINAL,CANDIDATE,IOS1,IOS2
    write(FORM,'(a,i0,a)') '(i',W,')'
    ORIGINAL = -987654321
    IOS1 = 0
    read(RECORD,FORM,iostat=IOS1) ORIGINAL
    CANDIDATE = -987654321
    IOS2 = 0
    if (.not.DIGITREAD(RECORD(1:W),CANDIDATE)) read(RECORD,FORM,iostat=IOS2) CANDIDATE
    if (IOS1.ne.IOS2.or.ORIGINAL.ne.CANDIDATE) then
      write(*,*) 'prefix differs: [',RECORD,']',W,ORIGINAL,CANDIDATE,IOS1,IOS2
      stop 5
    end if
    N = N+1
  end subroutine CHECK_PREFIX

  subroutine CHECK(FIELD)
    character(len=*), intent(in) :: FIELD
    character(len=8) :: FORM
    integer :: ORIGINAL,CANDIDATE,IOS1,IOS2,I
    logical :: DIGITS,TAKEN
    write(FORM,'(a,i0,a)') '(i',max(len(FIELD),1),')'
    ! DIGITREAD takes exactly the one- to nine-character fields of digits
    ! and blanks; everything else must go to the READ.
    DIGITS = len(FIELD).ge.1.and.len(FIELD).le.9
    do I = 1,len(FIELD)
      if (FIELD(I:I).ne.' '.and.(FIELD(I:I).lt.'0'.or.FIELD(I:I).gt.'9')) DIGITS = .false.
    end do
    ! The original statement alone.
    ORIGINAL = -987654321
    IOS1 = 0
    read(FIELD,FORM,iostat=IOS1) ORIGINAL
    ! The call-site pattern: fast path, else the same READ.
    CANDIDATE = -987654321
    IOS2 = 0
    TAKEN = DIGITREAD(FIELD,CANDIDATE)
    if (TAKEN.neqv.DIGITS) stop 1
    if (.not.TAKEN) then
      if (CANDIDATE.ne.-987654321) stop 2
      read(FIELD,FORM,iostat=IOS2) CANDIDATE
    end if
    if (IOS1.ne.IOS2.or.ORIGINAL.ne.CANDIDATE) then
      write(*,*) 'differs: [',FIELD,']',ORIGINAL,CANDIDATE,IOS1,IOS2
      stop 3
    end if
    if (TAKEN.and.IOS1.ne.0) stop 4
    N = N+1
    if (TAKEN) then
      FAST = FAST+1
    else if (IOS1.eq.0) then
      FALLBACK = FALLBACK+1
    else
      REJECTED = REJECTED+1
    end if
  end subroutine CHECK
end program DIGITS_CHECK
"""

TABLE = """subroutine TABLE{k}(CNT,SP,JK)
  implicit none
  integer :: CNT
  real :: SP(*),JK(9,5)
  include 'tables{k}.inc'
  CNT = size(SPcoef,1)
  SP(1:13*CNT) = reshape(SPcoef,[13*CNT])
  JK = JKcoef
end subroutine TABLE{k}
"""

GETTABLE = """subroutine GETTABLE(K,CNT,SP,JK)
  implicit none
  integer :: K,CNT
  real :: SP(*),JK(9,5)
  select case (K)
{cases}  end select
end subroutine GETTABLE
"""

NATCRS_DRIVER = """program NATCRS_CHECK
  use DIGITREAD_MOD
  implicit none
  include 'natcrs_codes.inc'
  character(len=4) :: OLD4,NEW4
  character(len=2) :: OLD2,NEW2
  integer :: I,V1,V2,IOS1,IOS2,FAST
  ! FIA code READ: every code defined by any variant's block data.
  FAST = 0
  do I = 1,size(FIACODES)
    V1 = -987654321
    V2 = V1
    IOS1 = 0
    IOS2 = 0
    read(FIACODES(I),'(I4)',iostat=IOS1) V1
    if (DIGITREAD(FIACODES(I),V2)) then
      FAST = FAST+1
    else
      read(FIACODES(I),'(I4)',iostat=IOS2) V2
    end if
    if (V1.ne.V2.or.IOS1.ne.IOS2) then
      write(*,*) 'FIA code differs: [',FIACODES(I),']',V1,V2,IOS1,IOS2
      stop 1
    end if
  end do
  write(*,'(a,i5,a,i5,a)') 'PASS NATCRS FIA codes:',size(FIACODES),' distinct codes,',FAST, &
    ' converted directly'
  ! Forest number WRITE and zero fill, over and beyond two digits.
  FAST = 0
  do I = -150,1200
    OLD2 = 'QQ'
    NEW2 = 'QQ'
    write(OLD2,'(I2)') I
    if (I.lt.10) OLD2(1:1) = '0'
    if (DIGITWRITE(I,NEW2,2)) then
      FAST = FAST+1
    else
      write(NEW2,'(I2)') I
    end if
    if (I.lt.10) NEW2(1:1) = '0'
    if (OLD2.ne.NEW2) then
      write(*,*) 'forest differs:',I,' [',OLD2,'] [',NEW2,']'
      stop 2
    end if
  end do
  write(*,'(a,i5,a)') 'PASS NATCRS forest numbers -150 to 1200;',FAST,' converted directly'
  ! The (A) WRITE of an alpha species code and the assignment that replaces it.
  do I = 1,size(ALPHACODES)
    OLD4 = 'QQQQ'
    write(OLD4,('(A)')) ALPHACODES(I)
    NEW4 = ALPHACODES(I)
    if (OLD4.ne.NEW4) then
      write(*,*) 'alpha code differs: [',ALPHACODES(I),']'
      stop 3
    end if
  end do
  write(*,'(a,i5,a)') 'PASS NATCRS alpha codes:',size(ALPHACODES),' distinct codes copied exactly'
end program NATCRS_CHECK
"""

MODULE = re.compile(r"^\s*module\s+(?!procedure\b)\w+", re.I | re.M)
RUNTIME_ERROR = re.compile(r"^Fortran runtime error: .*$", re.M)


def compile_nvel(source: Path, names: list[str], build: Path, jobs: int) -> Path:
    """Compile the listed NVEL files, modules first, into one static archive."""
    build.mkdir()

    def compile_one(name: str) -> subprocess.CompletedProcess:
        return subprocess.run(["gfortran", *FLAGS, f"-I{source}", f"-J{build}", "-c",
                               str(source / name), "-o", str(build / (Path(name).stem + ".o"))],
                              capture_output=True, text=True)

    modules = [name for name in names if MODULE.search((source / name).read_text(errors="replace"))]
    # Modules may use one another, so repeat until every module compiles.
    pending = modules
    while pending:
        failed = [name for name in pending if compile_one(name).returncode]
        if len(failed) == len(pending):
            raise RuntimeError(f"cannot compile modules: {failed}")
        pending = failed
    with ThreadPoolExecutor(max_workers=jobs) as pool:
        results = list(pool.map(compile_one, [name for name in names if name not in modules]))
    for result in results:
        if result.returncode:
            raise RuntimeError(result.stderr)
    library = build / "libnvel.a"
    subprocess.run(["ar", "rcs", str(library), *sorted(str(p) for p in build.glob("*.o"))], check=True)
    return library


def listed_nvel() -> list[str]:
    """NVEL Fortran sources compiled by at least one variant."""
    names = set()
    for manifest in (ROOT / "bin").glob("FVS??_sourceList.txt"):
        for entry in manifest.read_text().split():
            if entry.startswith("../volume/NVEL/") and entry.endswith(".f90"):
                names.add(entry.removeprefix("../volume/NVEL/"))
    return sorted(names)


def check_digits(directory: Path) -> None:
    (directory / "digits.f90").write_text(DIGITS_DRIVER)
    for checks in ([], ["-fcheck=all"]):
        subprocess.run(["gfortran", *FLAGS, *checks, str(NVEL / "digitread_mod.f90"), "digits.f90",
                        "-o", "digits"], cwd=directory, check=True)
        subprocess.run([str(directory / "digits")], cwd=directory, check=True)


# Test-only controls for undefined values that no conversion change can make
# defined. Each is applied identically to the reference and current copies,
# so the comparison still isolates the code under test. Repository sources
# and the full-suite comparator are unchanged.
CONTROLS = {
    # Bug 53: R1KEMP subtracts NONSAW percent of the cubic volume, but never
    # assigns NONSAW, so VOL(5) depends on leftover memory.
    "r1kemp.f90": ("  if (NONSAW .gt.100) NONSAW = 100\n",
                   "  NONSAW = 0\n  if (NONSAW .gt.100) NONSAW = 100\n"),
}


def build_pair(directory: Path, revision: str, label: str, jobs: int) -> dict[str, Path]:
    """Build the listed NVEL sources at REVISION and from the working tree."""
    current = listed_nvel()
    sources = {"reference": directory / f"{label}-reference", "current": directory / f"{label}-current"}
    archive(NVEL, revision, sources["reference"], directory / f"{label}-reference.tar")
    copy_working_tree(NVEL, sources["current"])
    reference = [name for name in current if (sources["reference"] / name).is_file()]
    added = sorted(set(current) - set(reference))
    if added not in ([], ["digitread_mod.f90"]):
        raise RuntimeError(f"unexpected NVEL source-list additions: {added}")
    for source in sources.values():
        for name, (old, new) in CONTROLS.items():
            text = (source / name).read_text()
            if text.count(old) != 1:
                raise RuntimeError(f"control for {name} no longer applies")
            (source / name).write_text(text.replace(old, new))
    return {"reference": compile_nvel(sources["reference"], reference, directory / f"{label}-reference-build", jobs),
            "current": compile_nvel(sources["current"], current, directory / f"{label}-current-build", jobs)}


def compare_modes(directory: Path, label: str, libraries: dict[str, Path], driver: list[str],
                  modes: tuple[str, ...], bad_modes: tuple[str, ...]) -> None:
    """Link one compiled driver with each library; outputs must match byte for byte."""
    for side, library in libraries.items():
        subprocess.run(["gfortran", *FLAGS, *driver, str(library), "-o", f"cases-{label}-{side}"],
                       cwd=directory, check=True)
    for mode in modes:
        outputs = {}
        for side in libraries:
            outputs[side] = directory / f"{label}-{mode}-{side}.txt"
            with outputs[side].open("wb") as handle:
                subprocess.run([str(directory / f"cases-{label}-{side}"), mode], cwd=directory,
                               stdout=handle, check=True)
        if not filecmp.cmp(outputs["reference"], outputs["current"], shallow=False):
            pairs = zip(outputs["reference"].read_text().splitlines(),
                        outputs["current"].read_text().splitlines())
            number, (left, right) = next((i, pair) for i, pair in enumerate(pairs, 1)
                                         if pair[0] != pair[1])
            raise RuntimeError(f"{mode} outputs differ at case line {number}:\n"
                               f"  reference {left[:160]}\n  current   {right[:160]}")
        lines = outputs["current"].read_text().splitlines()
        print(f"PASS {mode}: {len(lines) - 1} cases identical; {lines[-1]}")
    for mode in bad_modes:
        results = [subprocess.run([str(directory / f"cases-{label}-{side}"), mode], cwd=directory,
                                  capture_output=True, text=True) for side in libraries]
        errors = [RUNTIME_ERROR.findall(result.stderr) for result in results]
        if (results[0].returncode != results[1].returncode or errors[0] != errors[1]
                or len(errors[0]) != 1 or results[0].returncode == 0):
            raise RuntimeError(f"{mode} stops differ: {[r.stderr for r in results]}")
        print(f"PASS {mode}: both stop with status {results[0].returncode}: {errors[0][0]}")


def compile_driver(directory: Path, sources: list[Path]) -> list[str]:
    """Compile a driver once, so both libraries are linked with the same object."""
    objects = []
    for path in sources:
        target = directory / (path.stem + ".o")
        subprocess.run(["gfortran", *FLAGS, f"-I{NVEL}", f"-I{directory}", f"-J{directory}", "-c",
                        str(path), "-o", str(target)], cwd=directory, check=True)
        objects.append(str(target))
    return objects


def check_nsvb(directory: Path, jobs: int) -> None:
    libraries = build_pair(directory, BASELINE_NVEL, "nsvb", jobs)
    # The table wrappers use the unchanged coefficient includes.
    tables = "".join(TABLE.format(k=k) for k in range(1, 10))
    cases = "".join(f"  case ({k})\n    call TABLE{k}(CNT,SP,JK)\n" for k in range(1, 10))
    (directory / "tables.f90").write_text(tables + GETTABLE.format(cases=cases))
    driver = compile_driver(directory, [Path(__file__).with_name("nsvb_cases.f90"),
                                        directory / "tables.f90"])
    compare_modes(directory, "nsvb", libraries, driver, ("speqcoef", "calc", "nvbc"),
                  ("bad-speqcoef", "bad-calc", "bad-nvbc"))


def check_volinit(directory: Path, jobs: int) -> None:
    libraries = build_pair(directory, PRE_O09_NVEL, "volinit", jobs)
    # The driver needs the module files of one build; both builds define them identically.
    for module in (directory / "volinit-current-build").glob("*.mod"):
        (directory / module.name).write_bytes(module.read_bytes())
    driver = compile_driver(directory, [Path(__file__).with_name("volinit_cases.f90")])
    compare_modes(directory, "volinit", libraries, driver,
                  ("ecoprov", "regnwf", "defaulteq", "volinit", "volinitnvb"),
                  ("bad-forst", "bad-han"))


def block_data_codes(name: str) -> list[str]:
    """Distinct 4-character values given to NAME by any variant's DATA statements."""
    values = set()
    statement = re.compile(rf"^\s*data\s+{name}\s*/(.*?)/", re.I | re.M | re.S)
    for manifest in (ROOT / "bin").glob("FVS??_sourceList.txt"):
        for entry in manifest.read_text().split():
            if entry.endswith("blkdat.f90"):
                text = (manifest.parent / entry).read_text()
                for block in statement.findall(text):
                    values.update(v.ljust(4)[:4] for v in re.findall(r"'([^']*)'", block))
    return sorted(values)


def check_natcrs(directory: Path) -> None:
    """O12: NATCRS's FIA-code READ, forest WRITE and (A) WRITE replacements."""
    fia, alpha = block_data_codes("FIAJSP"), block_data_codes("JSP")
    if len(fia) < 100 or len(alpha) < 100:
        raise RuntimeError(f"too few block-data codes found: {len(fia)} FIA, {len(alpha)} alpha")
    def array(name: str, values: list[str]) -> str:
        items = ",&\n  ".join("'" + v.replace("'", "''") + "'" for v in values)
        # Initialized variables, not parameters: a READ cannot use a constant as its unit.
        return f"character(len=4) :: {name}({len(values)}) = [character(len=4) ::&\n  {items}]\n"
    (directory / "natcrs_codes.inc").write_text(array("FIACODES", fia) + array("ALPHACODES", alpha))
    (directory / "natcrs.f90").write_text(NATCRS_DRIVER)
    subprocess.run(["gfortran", *FLAGS, "-fcheck=all", str(NVEL / "digitread_mod.f90"), "natcrs.f90",
                    "-o", "natcrs"], cwd=directory, check=True)
    subprocess.run([str(directory / "natcrs")], cwd=directory, check=True)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--component", choices=("all", "digits", "nsvb", "volinit", "natcrs"),
                        default="all")
    parser.add_argument("--jobs", type=int, default=8, help="Compiler processes for the NVEL builds")
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix="mfvs-nvel-parsing-") as temporary:
        directory = Path(temporary)
        if args.component in ("all", "digits"):
            check_digits(directory)
        if args.component in ("all", "nsvb"):
            check_nsvb(directory, args.jobs)
        if args.component in ("all", "volinit"):
            check_volinit(directory, args.jobs)
        if args.component in ("all", "natcrs"):
            check_natcrs(directory)


if __name__ == "__main__":
    main()
