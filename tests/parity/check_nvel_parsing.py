"""Check NVEL's all-digit integer fast path and the NSVB routines that use it.

`digits` compares DIGITREAD, and the call-site pattern that falls back to the
original formatted READ, with that READ alone: every all-digit field up to five
characters, sampled wider fields, all one- and two-character fields and
selected three- and four-character fields with blanks, signs and other
characters. Values and IOSTAT codes must match, and a declined field must
leave the variable unchanged.

`nsvb` builds the NVEL sources listed in the variant source lists twice: at
the frozen pre-optimization NVEL revision and from the working tree. One
compiled driver (nsvb_cases.f90) is linked with each. SPEQCOEF, CALC_EQVW and
NVBC case outputs, mutated arguments included, must match byte for byte.
Invalid fields must stop both builds with the same runtime error.
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

from prepare_builds import ROOT, archive

NVEL = ROOT / "volume/NVEL"
BASELINE_NVEL = "d0e634fad1f91a2897bdc5f0c07bd0d21f658750"  # before O06-O08
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
    end do
  end do
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
contains
  subroutine CHECK(FIELD)
    character(len=*), intent(in) :: FIELD
    character(len=8) :: FORM
    integer :: ORIGINAL,CANDIDATE,IOS1,IOS2,I
    logical :: DIGITS,TAKEN
    write(FORM,'(a,i0,a)') '(i',max(len(FIELD),1),')'
    DIGITS = len(FIELD).ge.1.and.len(FIELD).le.9
    do I = 1,len(FIELD)
      if (FIELD(I:I).lt.'0'.or.FIELD(I:I).gt.'9') DIGITS = .false.
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


def check_nsvb(directory: Path, jobs: int) -> None:
    current = listed_nvel()
    reference_source = directory / "reference"
    archive(NVEL, BASELINE_NVEL, reference_source, directory / "reference.tar")
    reference = [name for name in current if (reference_source / name).is_file()]
    added = sorted(set(current) - set(reference))
    if added != ["digitread_mod.f90"]:
        raise RuntimeError(f"unexpected NVEL source-list additions: {added}")
    libraries = {"reference": compile_nvel(reference_source, reference, directory / "reference-build", jobs),
                 "current": compile_nvel(NVEL, current, directory / "current-build", jobs)}
    # One driver object, compiled once against unchanged table includes.
    tables = "".join(TABLE.format(k=k) for k in range(1, 10))
    cases = "".join(f"  case ({k})\n    call TABLE{k}(CNT,SP,JK)\n" for k in range(1, 10))
    (directory / "tables.f90").write_text(tables + GETTABLE.format(cases=cases))
    driver = []
    for name in ("nsvb_cases.f90", "tables.f90"):
        path = Path(__file__).with_name(name) if name == "nsvb_cases.f90" else directory / name
        target = directory / (Path(name).stem + ".o")
        subprocess.run(["gfortran", *FLAGS, f"-I{NVEL}", f"-J{directory}", "-c", str(path),
                        "-o", str(target)], cwd=directory, check=True)
        driver.append(str(target))
    for label, library in libraries.items():
        subprocess.run(["gfortran", *FLAGS, *driver, str(library), "-o", f"cases-{label}"],
                       cwd=directory, check=True)
    for mode in ("speqcoef", "calc", "nvbc"):
        outputs = {}
        for label in libraries:
            outputs[label] = directory / f"{mode}-{label}.txt"
            with outputs[label].open("wb") as handle:
                subprocess.run([str(directory / f"cases-{label}"), mode], cwd=directory,
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
    for mode in ("bad-speqcoef", "bad-calc", "bad-nvbc"):
        results = [subprocess.run([str(directory / f"cases-{label}"), mode], cwd=directory,
                                  capture_output=True, text=True) for label in libraries]
        errors = [RUNTIME_ERROR.findall(result.stderr) for result in results]
        if (results[0].returncode != results[1].returncode or errors[0] != errors[1]
                or len(errors[0]) != 1 or results[0].returncode == 0):
            raise RuntimeError(f"{mode} stops differ: {[r.stderr for r in results]}")
        print(f"PASS {mode}: both stop with status {results[0].returncode}: {errors[0][0]}")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--component", choices=("all", "digits", "nsvb"), default="all")
    parser.add_argument("--jobs", type=int, default=8, help="Compiler processes for the NVEL builds")
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix="mfvs-nvel-parsing-") as temporary:
        directory = Path(temporary)
        if args.component in ("all", "digits"):
            check_digits(directory)
        if args.component in ("all", "nsvb"):
            check_nsvb(directory, args.jobs)


if __name__ == "__main__":
    main()
