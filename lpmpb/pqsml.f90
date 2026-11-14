!===================!
!   lpmpb : pqsml   !
!===================!
! LPMPB $Id$
double precision function PQSML  (X,P,Q)
  implicit none

  !     PART OF THE MOUNTAIN PINE BEETLE EXTENSION OF PROGNOSIS SYSTEM.

  ! Revision History
  !   02/08/88 Last noted revision date.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  ! Common blocks
  include 'MPBETA.f90'

  double precision :: P,Q,S,U,V,X,XK,MPBGAM

  U = X**P
  S = U/P
  XK = 0.
  1 XK = XK + 1.
  U = (XK-Q)*X*U/XK
  V = U/(XK+P)
  S = S+ V
  if (DABS(V) .gt. DABS(EPS*S)) go to 1
  !     WRITE(16,777) P,Q,S
  777 format('IN PQSML, P,Q,S', 3D16.8)
  PQSML = S*MPBGAM(P+Q)/(MPBGAM(P)*MPBGAM(Q))
  !     WRITE(16,444) PQSML
  444 format('PQSML=',D16.8)
  return
end
