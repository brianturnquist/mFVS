!===================!
!   lpmpb : ptsrg   !
!===================!
! LPMPB $Id$
function PTSRG(X,BASNO)
  implicit none

  ! Revision History
  !   02/08/88 Last noted revision date.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  ! Variables
  integer :: J
  real    :: BASNO,C,PTSRG,X

  do J = 4,12,4
     C = J
     PTSRG = C*BASNO
     if (PTSRG .ge. X) return
     1 continue
  end do
  return
end
