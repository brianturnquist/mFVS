!===================!
!   dftm : dfole8   !
!===================!
! DFTM $Id$
subroutine DFOLE8
  implicit none

  !  DFTM MODEL SUBROUTINE - JIM COLBERT - JAN 1978.

  !  *** S(0) MODULE V3.1 DFTM MODEL ***

  !  REVISION HISTORY:
  !    01-APR-2013 Lance R. David (FMSC)
  !      A few variables defined locally were already defined
  !      in a common block. Local declaration removed.

  ! Common blocks
  include 'LIMITS.f90'
  include 'DFOL.f90'
  include 'ICOND.f90'

  ! Variables
  integer :: II,I

  II = K(3) - 1

  do I = 1,ICOUNT
     G19OUT(I,II) = G19(I)
     DPCENT(I,II) = 100.0 * (1.0 - X6(I) / Z3(I))
     10 continue
  end do

  return
end
