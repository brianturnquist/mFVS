!===================!
!   strp : esmsgs   !
!===================!
! STRP $Id$
subroutine ESMSGS (JOREGT)
  implicit none

  !     CALLED FROM ESIN TO PRINT HABITAT TYPE GROUPS FOR THE
  !     REGENERATION ESTABLISHMENT MODEL.

  ! Variables
  integer :: JOREGT

  write(JOREGT,100)
  100 format('HABITAT TYPE GROUPS USED IN THE REGENERATION',&
       &' ESTABLISHMENT MODEL',//,'GROUP   HABITAT TYPES',/,&
       &'-----   ',34('-'),/)
  return
end
