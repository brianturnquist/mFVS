!=========================================!
!   Compute stand mean annual increment   !
!=========================================!
! IE $Id$
subroutine MAICAL
  implicit none

  !  THIS SUBROUTINE CALCULATES THE MAI FOR THE STAND. IT IS CALLED
  !  FROM CRATET.

  ! Common blocks
  include 'PRGPRM.f90'
  include  'CONTRL.f90'

  include  'PLOT.f90'

  ! Variables
  logical :: DEBUG
  integer :: ISPNUM(MAXSP),ISICD,IERR
  real    :: SSSI,ADJMAI

  !  INITIALIZE INTERNAL VARIABLES:

  data ISPNUM/119,073,202,017,263,242,108,093,019,122,264,&
       &101,101,101,101,101,101,746,740,746,375,998,&
       &101/

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'MAICAL',6,ICYC)

  if (DEBUG) write(JOSTND,3)ICYC,ISISP,SITEAR(ISISP)
  3 format(' ENTERING SUBROUTINE MAICAL  CYCLE =',I5,1X,I3,&
       &F6.2)

  !   RMAI IS FUNCTION TO CALCULATE ADJUSTED MAI.

  if (ISISP .eq.0) ISISP = 3
  SSSI = SITEAR(ISISP)
  if (SSSI .eq.0.) SSSI = 140.
  ISICD = ISPNUM(ISISP)
  RMAI = ADJMAI(ISICD,SSSI,10.0,IERR)
  if (RMAI .gt.128.0)RMAI = 128.0
  if (DEBUG) write(JOSTND,10)ICYC,RMAI
  10 format(' LEAVING SUBROUTINE MAICAL  CYCLE =',I5,5X,'RMAI =',F10.3)

  return
end
