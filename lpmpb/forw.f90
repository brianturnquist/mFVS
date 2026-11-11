!==================!
!   lpmpb : forw   !
!==================!
! LPMPB $Id$
double precision function FORW  (X,P,Q,X1,X2,N)
  implicit none

  !     PART OF THE MOUNTAIN PINE BEETLE EXTENSION OF PROGNOSIS SYSTEM.

  ! Revision History
  !   02/08/88 Last noted revision date.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  ! Variables
  integer :: I,N,N1
  double precision :: P,Q,X,T1,XI,X1,X2,FOR,PQ1,XA1,TEMP
  if (N-1)1,2,3
  1 FORW = X1
  return
  2 FORW = X2
  return
  3 N1 = N-1
  T1 = X1
  PQ1 = P+Q-1.
  XA1 = 1.-X
  FOR = X2
  do I = 1,N1
     XI = I
     TEMP = FOR
     FOR = FOR+(FOR-T1)*(PQ1+XI)*XA1/(XI+Q)
     4 T1 = TEMP
  end do
  FORW = FOR
  return
end
