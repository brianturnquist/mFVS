!====================!
!   lpmpb : mpbcup   !
!====================!
! LPMPB $Id$
subroutine MPBCUP
  implicit none

  !     INTERFACING PROGRAM TO CALL EITHER MPBDRV OR COLDRV.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'MPBCOM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'

  ! Revision History
  !   02/08/88 Last noted revision date.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  !************************* EXECUTION BEGINS ***************************

  if (LPOPDY) then
     call MPBDRV
  else
     call COLDRV
  end if
  return
end
