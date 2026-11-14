!===================!
!   lpmpb : ptbno   !
!===================!
! LPMPB $Id$
function PTBNO(X)
  implicit none

  !     FIND LARGEST INTEGER POWER OF 10 LESS THAN X
  !     IF X IS LESS THAN 3.5*10**I, THEN PTBNO=10**I/2

  ! Revision History
  !   02/08/88 Last noted revision date.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  ! Variables
  real    :: P10,PTBNO,X,XRND,XX

  P10 = 10.
  XX = X
  if (XX .lt.0. ) XX = -XX
  XRND = 0.1005*XX
  if (XRND .le.10.) go to 1

  2 P10 = 10.*P10
  if (P10.le. XRND) go to 2
  go to 3

  1 P10 = P10/10.
  if (P10.gt. XRND) go to 1
  P10 = 10.*P10
  3 PTBNO = P10
  if (3.5*P10.ge. X) PTBNO = .5*P10
  return
end
