!==================================!
!   Provide height-diameter stub   !
!==================================!
! FIRE-BASE $Id$
!  Used by variants that have no HTDBH routine of their own; FMCROWE calls it.
subroutine HTDBH(I10,I11,X10,X11,I12)
  implicit none

  ! Variables
  integer :: I10,I11,I12
  real    :: X10,X11

  I10 = 0
  I11 = 0
  I12 = 0

  X10 = 0.0
  X11 = 0.0

  return
end subroutine HTDBH
