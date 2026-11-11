!====================!
!   lpmpb : coldrv   !
!====================!
! LPMPB $Id$
subroutine COLDRV
  implicit none

  !     PART OF THE MOUNTAIN PINE BEETLE EXTENSION OF PROGNOSIS.

  !     THIS MODULE DRIVES ROUTINES THAT KILL LODGEPOLE PINE
  !     USING COLE'S MODEL (1983).

  ! Revision History
  !   02/08/88 Last noted revision date.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'
  include 'COLCOM.f90'
  include 'MPBCOM.f90'

  call COLDBH
  call COLMOD (IFINT,ICYC,IY(ICYC),NPLT)
  call COLMRT
  return
end
