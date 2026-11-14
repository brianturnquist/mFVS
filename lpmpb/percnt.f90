!====================!
!   lpmpb : percnt   !
!====================!
! LPMPB $Id$
function PERCNT(VALUE, BASE)
  implicit none

  !     PART OF THE MOUNTAIN PINE BEETLE EXTENSION OF PROGNOSIS SYSTEM.

  ! Revision History
  !   02/08/88 Last noted revision date.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  ! Variables
  real    :: BASE,PERCNT,VALUE

  PERCNT = 0.
  if (ABS(BASE) .ge.1E-30) PERCNT = 100.*VALUE/BASE
  return
end
