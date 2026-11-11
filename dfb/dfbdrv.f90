!==================!
!   dfb : dfbdrv   !
!==================!
! DFB $Id$
subroutine DFBDRV
  implicit none

  !  THIS SUBROUTINE CALLS THE DFB MODEL SUBROUTINES THAT CAUSE THE
  !  DOUGLAS-FIR MORTALITY.

  !  CALLED BY :
  !     GRADD   [PROGNOSIS]

  !  CALLS :
  !     DFBDBH  (SUBROUTINE)   [DFB]
  !     DFBMOD  (SUBROUTINE)   [DFB]
  !     DFBMRT  (SUBROUTINE)   [DFB]

  !  COMMON BLOCK VARIABLES USED :
  !     DEBUIN (DFBCOM)  INPUT
  !     JODFB  (DFBCOM)  INPUT

  ! Common blocks
  include 'PRGPRM.f90'
  include 'DFBCOM.f90'

  if (DEBUIN) write (JODFB,*) ' ** ENTERING SUBROUTINE DFBDRV'

  !     INITIALIZE START VALUES.

  call DFBDBH

  !     CALCULATE THE NUMBER OF TREES TO KILL.

  call DFBMOD

  !     PASS THE MORTALITY BACK TO PROGNOSIS.

  call DFBMRT

  if (DEBUIN) write (JODFB,*) ' ** LEAVING SUBROUTINE DFBDRV'

  return
end
