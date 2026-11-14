!===================!
!   dftm : y0comp   !
!===================!
! DFTM $Id$
subroutine Y0COMP
  implicit none

  !     DFTM MODEL SUBROUTINE - JIM COLBERT - JAN 1978.

  ! Common blocks
  include 'ICOND.f90'
  include 'GPASS.f90'
  include 'UPPER.f90'
  include 'LIMITS.f90'

  ! Variables
  integer :: INUM

  KP = 4
  INUM = IC(2) - IC(1) + 1
  call REDIST (KP, INUM)

  return
end
