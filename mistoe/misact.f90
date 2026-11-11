!=====================!
!   mistoe : misact   !
!=====================!
! MISTOE $Id$
subroutine MISACT (LACTV)

  !  Purpose:
  !     Returns TRUE when called to signal that the "real" mistletoe
  !  submodel is linked to the program.  There is an entry in EXMIST
  !  that always returns FALSE.

  implicit none

  ! Variables
  logical :: LACTV
  LACTV = .true.

  return
end
