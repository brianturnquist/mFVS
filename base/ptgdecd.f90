!==============================!
!   Decode point group names   !
!==============================!
! BASE $Id$
subroutine PTGDECD(POINTNO,KARD,IFLAG)
  implicit none

  !     DECODE THE POINT GROUP NAME CODE
  !     POINTNO IS THE POINT NUMBER, IF THE POINT GROUP IS A NAME,
  !                THEN ASSIGN A SEQUENTIAL, NEGATIVE, GROUP NUMBER
  !     IFLAG = 0 IF POINT GROUP IS NOT FOUND; 1 IF IT IS.
  !     KARD  = CHARACTER REPRESENTATION OF POINT FIELD

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'

  ! Variables
  integer :: IFLAG,POINTNO,J,I
  character(len=10) :: TEMP
  character(len=10) :: KARD

  !  IF ISPC IS A POSITIVE NON-ZERO NUMBER THEN ITS A NUMERIC SPECIES
  !  CODE

  if (POINTNO .gt. 0) then
     IFLAG = 0
     go to 100
  end if

  !  POINTNO = 0, CHECK FOR ALPHA POINT GROUP NAME.
  !  LOAD TEMP WITH UP TO TEN NON-BLANK CHARS IN KARD.

  TEMP = '          '
  J = 0
  do I = 1,10
     if (KARD(I:I).ne.' ') then
        do J = 1,MIN(10,10-I+1)
           TEMP(J:J) = KARD(I+J-1:I+J-1)
           call UPCASE(TEMP(J:J))
           5 continue
        end do
        exit
     end if
     10 continue
  end do

  do I = 1,NPTGRP
     if (TEMP .eq. PTGNAME(I)) then
        POINTNO = -I
        IFLAG = 1
        go to 50
     end if
     30 continue
  end do

  IFLAG = 0
  go to 100

  50 continue
  KARD = TEMP

  100 continue

  ! NOTHING WAS FOUND, SO IT MUST BE BLANK AND NOT A POINT GROUP NAME.

  return
end subroutine PTGDECD
