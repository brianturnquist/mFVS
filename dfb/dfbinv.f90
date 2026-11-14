!==================!
!   dfb : dfbinv   !
!==================!
! DFB $Id$
subroutine DFBINV
  implicit none

  !  COUNTS THE TREES/ACRE OF DOUGLAS-FIR READ IN AS KILLED BY DFB.

  !  CALLED BY :
  !     MAIN     [PROGNOSIS]

  !  CALLS :
  !     NONE

  !  LOCAL VARIABLES :
  !     I      - INDEX TO RECENTLY ATTACKED TREES.
  !     II     - COUNTER.

  !  COMMON BLOCK VARIABLES USED :
  !     IPT    - (DFBCOM)  INPUT
  !     LINV   - (DFBCOM)  INPUT
  !     NDAMS  - (DFBCOM)  INPUT
  !     PREKLL - (DFBCOM)  OUTPUT
  !     PROB   - (ARRAYS)  INPUT

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'DFBCOM.f90'

  ! Variables
  integer :: I,II

  !.... COUNT THE TREES/ACRE KILLED BY DFB AND READ IN THROUGH DAMAGE
  !.... CODES IN THE TREE LIST DATA.

  if (NDAMS .gt.0.and. LINV) then
     PREKLL = 0.0
     do II = 1,NDAMS
        I = IPT(II)
        PREKLL = PREKLL + PROB(I)
        100 continue
     end do
  end if

  return
end
