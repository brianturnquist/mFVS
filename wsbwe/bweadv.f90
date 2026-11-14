!====================!
!   wsbwe : bweadv   !
!====================!
! WSBWE $Id$
subroutine BWEADV(IYR1)
  implicit none

  !     ADJUST PROGNOSIS VARIABLES.

  !     PART OF THE WESTERN SPRUCE BUDWORM MODEL/PROGNOSIS LINKAGE CODE.
  !     NL CROOKSTON--FORESTRY SCIENCES LAB--MOSCOW, MAY, 1984
  !     minor changes by K.Sheehan 7/96 to remove LBWDEB,LBWGRO,IHABIT
  !        STNDEL, SCSTAG

  !     DESCRIPTION :

  !     THIS ROUTINE ADJUSTS VARIABLES WHICH COME FROM PROGNOSIS SO
  !     THEY WILL FIT INTO BUDWORM MODEL COMPONENTS.

  !     CALLED FROM :

  !       BWESIT - CREATE A BUDWORM MODEL STAND FROM A PROGNOSIS STAND.

  !     PARAMETERS :

  !       IYR1   - INVENTORY YEAR OF THE STAND.

  ! Revision History:
  !  17-MAY-2005 Lance R. David (FHTET)
  !     Added FVS parameter file PRGPRM.F77.
  !  14-JUL-2010 Lance R. David (FMSC)
  !     Added IMPLICIT NONE and declared variables as needed.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'BWESTD.f90'
  include 'BWECOM.f90'

  ! Variables
  integer :: ICROWN,IHOST,IYR1

  !     IF THERE IS LARCH, ADD THE FOLIAGE TO NON-HOST AND SET
  !     THE LARCH FLAG EQUAL TO ZERO.

  if (IFHOST(6).eq.0) goto 1020
  do ICROWN = 1,NCROWN
     FOLNH(ICROWN) = FOLNH(ICROWN)+FOLADJ(6,ICROWN,1)+&
          &FOLADJ(6,ICROWN,2)+FOLADJ(6,ICROWN,3)+FOLADJ(6,ICROWN,4)
     1015 continue
  end do
  IFHOST(6) = 0
  1020 continue

  do IHOST = 1,NHOSTS
     if (IFHOST(IHOST).eq.0) goto 1040
     do ICROWN = 1,NCROWN
        FNEW(ICROWN,IHOST) = FOLADJ(IHOST,ICROWN,1)
        FOLD1(ICROWN,IHOST) = FOLADJ(IHOST,ICROWN,2)*&
             &PRBIO(IHOST,ICROWN,2)
        FOLD2(ICROWN,IHOST) = FOLADJ(IHOST,ICROWN,3)*&
             &PRBIO(IHOST,ICROWN,3)
        FREM(ICROWN,IHOST) = FOLADJ(IHOST,ICROWN,4)*&
             &PRBIO(IHOST,ICROWN,4)

        1035 continue
     end do
     1040 continue
  end do

  !     CHECK THE YEARS.
  IBWYR1 = IYR1

  return
end
