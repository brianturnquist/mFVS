!====================!
!   lpmpb : surflp   !
!====================!
! LPMPB $Id$
function SURFLP (DBH)
  implicit none

  !     PART OF THE MOUNTAIN PINE BEETLE EXTENSION OF PROGNOSIS SYSTEM.

  !     CALCULATES THE SURFACE AREA OF LPP.
  !     FUNCTION IS FROM DON BURNELL.

  ! Revision History
  !   02/08/88 Last noted revision date.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  ! Variables
  real    :: DBH,SURFLP

  SURFLP = DBH  *  0.672
  if ( DBH .gt.5.0 ) SURFLP = 8.835 * DBH - 40.82
  return
end
