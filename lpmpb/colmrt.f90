!====================!
!   lpmpb : colmrt   !
!====================!
! LPMPB $Id$
subroutine COLMRT
  implicit none

  !     MORTALITY ROUTINE FOR COLES MPB MODEL.

  !     PART OF THE MOUNTAIN PINE BEETLE EXTENSION OF PROGNOSIS.
  !     FORESTRY SCIENCES LAB -- MOSCOW, IDAHO

  ! Revision History
  !   05/31/00 Last noted revision date.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'MPBCOM.f90'
  include 'COLCOM.f90'
  include 'PLOT.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'

  ! Variables
  integer :: I,I1,I2,J,INDEX,NUMYRS
  real    :: PRKILL(10),CFTVOL(10),SUMDED,XT

  SUMDED = 0.0
  NUMYRS = MPMXYR
  if (NUMYRS .gt. IFINT) NUMYRS = IFINT

  do I = 1,10
     if (START(I) .le.0.0) then
        PRKILL(I) = 0.0
     else
        PRKILL(I) = (START(I) - GREEN(NUMYRS,I)) / START(I)
     end if

     SUMDED = SUMDED + PRKILL(I)
     CFTVOL(I) = 0.0
     100 continue
  end do

  !     APPLY MOUNTAIN PINE BEETLE MORTALITY.

  !     Changed pointer array ISCT subscript from 7 to IDXLP to correspond
  !     with new species mapping and new location for LP (RNH Dec98, GEB May2000)

  I1 = ISCT(IDXLP,1)
  !      I1 = ISCT(7,1)
  if (I1.eq.0) goto 300

  !     Changed pointer array ISCT subscript from 7 to IDXLP to correspond
  !     with new species mapping and new location for LP (RNH Dec98, GEB May2000)

  I2 = ISCT(IDXLP,2)
  !      I2 = ISCT(7,2)

  do I = I1,I2
     J = IND1(I)

     !        LOAD THE WK2 ARRAY WITH THE NUMBER OF TREES PER ACRE
     !        KILLED BY MOUNTAIN PINE BEETLE FOR EACH TREE RECORD.

     call COLIND (DBH(J),INDEX)
     XT = PRKILL(INDEX) * PROB(J)
     CFTVOL(INDEX) = CFTVOL(INDEX) + XT * CFV(J)
     if (XT .gt. WK2(J)) WK2(J) = XT
     if (PROB(J) - WK2(J) .lt.1E-6) WK2(J) = PROB(J) - 1E-6
     200 continue
  end do

  call MPBTAB(CFTVOL)

  300 continue
  return
end
