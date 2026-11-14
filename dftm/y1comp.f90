!===================!
!   dftm : y1comp   !
!===================!
! DFTM $Id$
subroutine Y1COMP
  implicit none

  !     DFTM MODEL SUBROUTINE - JIM COLBERT - JAN 1978.

  ! Common blocks
  include 'ICOND.f90'
  include 'LOWER.f90'
  include 'LIMITS.f90'

  ! Variables
  integer :: I

  do I = 1,3
     Y1(I) = X1(I)
     10 continue
  end do

  X5(ICOUNT) = Y1(1)
  X6(ICOUNT) = Y1(2)
  X7(ICOUNT) = Y1(3)
  G19(ICOUNT) = G1(19)

  return
end
