!===================!
!   wwpb : bmdbhc   !
!===================!
! WWPB $Id$
subroutine BMDBHC (DBH,INDEX)

  !     PART OF THE MOUNTAIN PINE BEETLE EXTENSION OF PROGNOSIS.
  !     N.L. CROOKSTON--FOREST SCIENCES LAB--MOSCOW ID--JULY 1985

  !     PARAMETERS:
  !        DBH - FOUR BYTE REAL DBH OF TREE.
  !        INDEX - FOUR BYTE INTEGER SIZE CLASS.

  !     GIVEN A DBH, ASSIGNS A DBH CLASS FROM THE ARRAY 'UPSIZ'.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PPEPRM.f90'
  include 'BMPRM.f90'
  include 'BMCOM.f90'

  ! Variables
  integer :: I,INDEX

  INDEX = 1
  do I = 1,NSCL-1
     if (DBH .lt. FLOAT(UPSIZ(I))) goto 20
     INDEX = INDEX + 1
     10 continue
  end do
  20 continue

  return
end
