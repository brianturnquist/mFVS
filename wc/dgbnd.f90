!================!
!   wc : dgbnd   !
!================!
! WC $Id$
subroutine DGBND (ISPC,DBH,DDG)
  implicit none

  !  THIS SUBROUTINE IS USED TO INSURE THAT A MAXIMUM VALUE FOR DG
  !  IS NOT EXCEEDED.

  ! ALL SPECIES EXCEPT REDWOOD USE THE SAME BOUNDING FUNCTION WHICH
  ! WAS DEVELOPED FOR DOUGLAS-FIR.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'

  ! MAX DG CHECK.

  ! Variables
  integer :: ISPC
  real    :: DDG,DBH,TEMDBH,DGMAX

  ! INCOMING DIAMETER INCREMENT IS ASSIGNED TO DDG IF SPECIES IS REDWOOD
  ! ALL OTHER SPECIES HAVE BOUNDING LOGIC APPLIED
  select case(ISPC)
  case(17)
     DDG = DDG
  case default
     TEMDBH = DBH
     if (TEMDBH .gt.150.)TEMDBH = 150.
     DGMAX = (7.92*EXP(-0.03*TEMDBH))
     if (DDG .gt. DGMAX) DDG = DGMAX
     if (DDG .lt.0.0) DDG = 0.0
  end select

  ! CHECK FOR SIZE CAP COMPLIANCE.

  if ((DBH+DDG).gt.SIZCAP(ISPC,1).and.SIZCAP(ISPC,3).lt.1.5) then
     DDG = SIZCAP(ISPC,1)-DBH
     if (DDG .lt.0.01) DDG = 0.01
  end if

  return
end
