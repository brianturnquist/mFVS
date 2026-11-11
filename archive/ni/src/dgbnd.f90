!============================!
!   archive/ni/src : dgbnd   !
!============================!
subroutine DGBND (ISPC,DBH,DDG)
  implicit none

  !  **DGBND--NI  DATE OF LAST REVISION:  04/09/08

  !  THIS SUBROUTINE IS INSURES THAT A MAXIMUM VALUE FOR DIAMETER
  !  GROWTH IS NOT EXCEEDED.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'

  ! CHECK FOR SIZE CAP COMPLIANCE.
  ! Variables
  integer :: ISPC
  real    :: DDG,DBH

  if ((DBH+DDG).gt.SIZCAP(ISPC,1) .and. SIZCAP(ISPC,3).lt.1.5) then
     DDG = SIZCAP(ISPC,1)-DBH
     if (DDG .lt.0.01) DDG = 0.01
  end if

  return
end
