!===================!
!   dftm : g0comp   !
!===================!
! DFTM $Id$
subroutine G0COMP
  implicit none

  !     DFTM MODEL SUBROUTINE - JIM COLBERT - JAN 1978.

  ! Common blocks
  include 'ICOND.f90'
  include 'GPASS.f90'
  include 'UPPER.f90'
  include 'LIMITS.f90'

  !  REVISION HISTORY:
  !    01-APR-2013 Lance R. David (FMSC)
  !      A few variables defined locally were already defined
  !      in a common block. Local declaration removed.

  ! Variables
  integer :: INUM,IP,J,I,KPP

  if (K(3) .eq. K(1)) return
  INUM = IC(2) - IC(1) + 1
  KP = K(3) - 1
  IP = KP + 1

  call REDIST(KP, INUM)

  do J = 1,INUM
     G3(J) = AMIN1((1.0 - Z2(J) / 100.0) * Z3(J), X6(J))
     10 continue
  end do

  do I = 1,INUM
     X6(I) = G3(I)
     KPP = K(3) + 56
     X7(I) = (1.0 - B0(KPP)) * G2(I)
     120 continue
  end do

  return
end
