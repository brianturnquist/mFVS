!==================!
!   lpmpb : back   !
!==================!
! LPMPB $Id$
double precision function BACK  (X,P,Q,Z,N)
  implicit none

  !     PART OF THE MOUNTAIN PINE BEETLE EXTENSION OF PROGNOSIS SYSTEM.

  ! Revision History
  !   02/08/88 Last noted revision date.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  ! Common blocks
  include 'MPBETA.f90'
  ! Variables
  integer :: N
  double precision :: P,Q,R,T,U,X,Z,AP,FN,Q1,XN,XNU

  BACK = Z
  if (N.eq.0) return
  Q1 = Q-1.
  AP = Z
  FN = N
  XNU = N+N+6
  1 XN = XNU
  R = 0.
  BACK = Z
  2 XN = XN-1
  T = P + XN
  U = (T+Q1)*X
  R = U/(U+T-T*R)
  if (XN .le. FN) BACK = R*BACK
  if (BACK .lt. BMIN)return
  if (XN.gt.1.) go to 2
  if (DABS(BACK-AP) .lt. DABS(EPS*BACK))return
  AP = BACK
  XNU = XNU+5.
  go to 1
end
