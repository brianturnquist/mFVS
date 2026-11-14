!===================!
!   lpmpb : betin   !
!===================!
! LPMPB $Id$
double precision function BETIN  (A,B,X)
  implicit none

  !     PART OF THE MOUNTAIN PINE BEETLE EXTENSION OF PROGNOSIS SYSTEM.

  !AUTION - BETIN IS AN INCOMPLETE BETA FUNCTION WRITEN IN DOUBLE
  !     PRECISION.  THUS A,B AND X MUST BE DECLARED DOUBLE PRECISION.

  !     THIS ROUTINE, WITH THE SUBPROGRAMS FORW, BACK, PQSML, AND DGAMMA
  !     EVALUATES THE NORMALIZED INCOMPLETE BETA FUNCTION:

  !     IF Y(A,R,X) = INTEGRAL FROM 0 TO X OF T**(A-1)*(1-T)**(B-1)DT
  !     THEN BETIN(A,B,X) = Y(A,B,X)/Y(A,B,1).
  !     RESTRICTIONS  A.GT.0, B.GT.0, & 0.LE.X.LE.1 MUST BE SATISFIED.

  !     REFERENCE:   COMM OF THE ACM, MARCH 1964,  ALG. 222.

  ! Revision History
  !   02/08/88 Last noted revision date.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  ! Common blocks
  include 'MPBETA.f90'

  ! Variables
  integer :: IA,M
  double precision :: A,B,P,Q,X,Y,P1,Q1,X1,X2,BACK,FORW,PQSML

  if (A.gt.0..and. B.gt.0..and.X.ge.0..and.X.le.1.) go to 5
  write(6,101)
  101 format('********   ERROR - LPMPB: PARAMETER RESTRICTIONS IN',&
       &' BETIN ARE NOT SATISFIED, BETIN HAS BEEN SET TO ZERO.')
  BETIN = 0
  return

  5 IA = A
  P = A-IA
  if (P.ne.0.) go to 1
  P = 1.
  IA = IA-1
  1 BETIN = X
  if (X.eq.0..or.X.eq.1.) return
  if (X .le..5) go to 3
  Y = 1. - X
  Q = P
  P = B
  M = P
  P1 = P-M
  if (P1.ne.0.) go to 2
  M = M-1
  P1 = 1.
  2 X1 = BACK(Y,P1,Q,PQSML(Y,P1,Q),M)
  X2 = BACK(Y,P1,Q+1,PQSML(Y,P1,Q+1.),M)
  BETIN = 1. - FORW(Y,P,Q,X1,X2,IA)
  if (BETIN .lt. BMIN) BETIN = 0.
  return
  3 Q = B
  M = Q
  Q1 = Q-M
  if (Q1.ne.0.) go to 4
  Q1 = 1.
  M = M-1
  4 X1 = PQSML(X,P,Q1)
  if (M.gt.0) X2 = PQSML(X,P,Q1+1.)
  BETIN = BACK(X,P,Q,FORW(X,P,Q1,X1,X2,M),IA)
  if (BETIN .lt. BMIN) BETIN = 0.
  return
end
