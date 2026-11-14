!===================!
!   dftm : redist   !
!===================!
! DFTM $Id$
subroutine REDIST(KP, INUM)
  implicit none

  !     DFTM MODEL SUBROUTINE - JIM COLBERT - JAN 1978.

  !  REVISION HISTORY:
  !    01-APR-2013 Lance R. David (FMSC)
  !      A few variables defined locally were already defined
  !      in a common block. Local declaration removed.

  ! Common blocks
  include 'ICOND.f90'
  include 'GPASS.f90'
  include 'UPPER.f90'

  ! Variables
  integer :: INUM,KP,J
  real    :: SUMEGG,TSTEM

  ! ***              REDISTRIBUTION OF EGGS        ***
  !          INITIALIZE LOCAL VARIABLES

  SUMEGG = 0.0
  TSTEM = 0.0

  !     CALCULATE TOTAL EGGS OVER STAND (SUMEGG)
  !     AND TOTAL WEIGHTED SUM OF TREES(TOTSTEM)

  do J = 1,INUM
     SUMEGG = SUMEGG + X7(J) * Z4(J) * Z5(J)
     TSTEM = TSTEM + Z4(J) * Z5(J)
     10 continue
  end do

  X0(KP) = SUMEGG / TSTEM

  !     CALCULATE VALUES OF G(2,J) FOR S0 HERE,
  !     INSTEAD OF IN FUNCTION G02 CODE AND WRITE ON LUN 3

  do J = 1,INUM
     G2(J) = X7(J) + B0(62) * (X0(KP) - X7(J))
     20 continue
  end do

  return
end
