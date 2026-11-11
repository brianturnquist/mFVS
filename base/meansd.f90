!===========================================!
!   Compute a mean and standard deviation   !
!===========================================!
! BASE $Id$
subroutine MEANSD (A,N,ABAR,VAR,STD)
  implicit none

  !     COMPUTE THE MEAN AND STANDARD DEVIATION OF A REAL VECTOR.

  !     PARAMETERS:

  !     A: REAL VECTOR TO COMPUTE MEAN AND STANDARD DEVIATION OVER.
  !     N: LENGTH OF A.
  !     ABAR: COMPUTED MEAN OF A.
  !     VAR: VARIANCE OF A.
  !     STD: COMPUTED STANDARD DEVIATION OF A.

  ! Variables
  real    :: A(N)
  integer :: I,N
  double precision :: APX,ABAR,VAR,STD,SUM,SUMSQ,DN,Z

  DN = N
  APX = 0D0
  do I = 1,N
     APX = APX+A(I)
     30 continue
  end do
  APX = APX/DN
  SUM = 0D0
  SUMSQ = 0D0
  do I = 1,N
     Z = A(I)-APX
     SUM = SUM+Z
     SUMSQ = SUMSQ+Z*Z
     40 continue
  end do
  ABAR = SUM/DN
  VAR = (SUMSQ-(SUM*ABAR))/(DN-1)
  STD = DSQRT(VAR)
  ABAR = ABAR+APX
  return
end subroutine MEANSD
