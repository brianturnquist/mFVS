!===================!
!   dftm : z1comp   !
!===================!
! DFTM $Id$
subroutine Z1COMP
  implicit none

  !     DFTM MODEL SUBROUTINE - JIM COLBERT - JAN 1978.

  ! Common blocks
  include 'ICOND.f90'
  include 'GPASS.f90'
  include 'UPPER.f90'
  include 'LOWER.f90'
  include 'LIMITS.f90'

  ! Variables
  integer :: I1,I2,I3,IP,J

  !***    READ IN THE Z VECTOR, INITIAL X VECTOR   ***

  Z1(3) = FLOAT(K(3))
  IP = K(3)
  Z1(1) = Z2(ICOUNT)
  Z1(2) = Z3(ICOUNT)
  X1(1) = X5(ICOUNT)
  X1(2) = X6(ICOUNT)
  X1(3) = X7(ICOUNT)
  Z1(4) = FLOAT(IZ6(ICOUNT))
  X1(4) = 0.0

  ! *** SET PHASE-SPECIFIC PARAMETERS B1(29) AND B1(34--36)

  B1(29) = Z1(3)

  B1(34) = B0(IP + 48)
  B1(35) = B0(IP + 52)
  B1(36) = B0(IP + 62)

  !***     READ IN HOST AND PHASE SPECIFIC MORTALITY VECTORS   ***

  do J = 1,6
     I1 = J + (IP - 1) * 6
     I2 = J + (IP + 3) * 6
     I3 = J + (IZ6(ICOUNT) - 1) * 6

     B1(J) = R0(J+12)
     R1(J) = B0(I1)
     R1(J+6) = B0(I2)
     R1(J+12) = R0(I3)
     6 continue
  end do

  return
end
