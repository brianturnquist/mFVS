!====================!
!   lpmpb : mpbout   !
!====================!
! LPMPB $Id$
subroutine MPBOUT
  implicit none

  !     PART OF THE MOUNTAIN PINE BEETLE EXTENSION OF PROGNOSIS SYSTEM.

  !     WRITES FINAL MPB SUMMARY OUTPUT.

  ! Revision History
  !   02/08/88 Last noted revision date.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'
  include 'MPBCOM.f90'

  ! Variables
  character(len=133) :: DSTRNG
  character(len=3) :: YES
  character(len=2) :: NO

  integer :: I

  data YES/'YES'/,NO/'NO'/

  if (.not.LMPB1) return
  call MPBHED
  write (JOMPB,9015)
  9015 format (8('-'),' MOUNTAIN PINE BEETLE ACTIVITY SUMMARY ',&
       &8('-'),//,'----- CYCLE -----    ',&
       &'PROBABILITY  WAS THERE AN OUTBREAK',/,&
       &'NUMBER      YEARS    OF OUTBREAK',&
       &'      IN THIS STAND',/,55('-'),/)

  do I = 1,NCYC
     if (WORKIN(I))write (JOMPB,9016) I,IY(I),IY(I+1),PRBMPB(I),YES
     if (.not.WORKIN(I))write(JOMPB,9016)I,IY(I),IY(I+1),PRBMPB(I),NO
     9016 format (I3,I7,' -',I5,F15.5,13X,A3)
     120 continue
  end do
  write(JOMPB,*)

  ENDFILE JOMPB

  !     THE REST OF THE SUBROUTINE COPIES THE MOUNTAIN PINE BEETLE
  !     OUTPUT TO THE MAIN PROGNOSIS PRINT FILE (JOSTND).

  if (JOMPB.eq.JOSTND) return
  rewind JOMPB
  125 continue
  read (JOMPB,130,end = 9999) DSTRNG
  130 format(A133)
  write (JOSTND,130) DSTRNG
  goto 125
  9999 continue
  rewind JOMPB
  return
end
