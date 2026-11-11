!================================!
!   Decode species group codes   !
!================================!
! BASE $Id$
subroutine SGDECD (ISPC,KARD,IFLAG)
  implicit none

  !     DECODE THE SPECIES GROUP CODE.
  !     ISPC  = NUMERIC SPECIES CODE.
  !     IFLAG = 0 IF SPECIES GROUP IS NOT FOUND; 1 IF IT IS.
  !     KARD  = CHARACTER REPRESENTATION OF SPECIES FIELD

  ! Common blocks
  include 'PRGPRM.F77'
  include 'CONTRL.F77'

  ! Variables
  integer :: IFLAG,ISPC,J,I
  character(len=10) :: KARD
  character(len=10) :: TEMP

  !  IF ISPC IS A POSITIVE NON-ZERO NUMBER THEN ITS A NUMERIC SPECIES
  !  CODE

  if (ISPC .gt. 0) then
     IFLAG = 0
     go to 100
  end if

  !  IF ISPC IS A NEGATIVE NUMBER, THEN ITS A SPECIES GROUP INDICATOR

  if (ISPC .lt. 0) then
     KARD = NAMGRP(-ISPC)
     IFLAG = 1
     go to 100
  end if

  !  ISPC = 0, CHECK FOR ALPHA SPECIES GROUP NAME.
  !  LOAD TEMP WITH UP TO THREE NON-BLANK CHARS IN KARD.

  TEMP = '          '
  J = 0
  do I = 1,10
     if (KARD(I:I).ne.' ') then
        do J = 1,MIN(10,10-I+1)
           TEMP(J:J) = KARD(I+J-1:I+J-1)
           call UPCASE(TEMP(J:J))
           5 continue
        end do
        goto 20
     end if
     10 continue
  end do

  ! NOTHING WAS FOUND, SO IT MUST BE BLANK AND NOT A SPECIES GROUP NAME.

  go to 50

  ! CHECK TO SEE IF THE SPECIES GROUP NAME IS VALID.
  ! 'ALL' IS SPECIES CODE ZERO AND NOT A VALID GROUP NAME.

  20 continue
  if (TEMP.eq.'ALL') then
     IFLAG = 0
     go to 100
  end if

  do I = 1,NSPGRP
     if (TEMP .eq. NAMGRP(I)) then
        ISPC = -I
        IFLAG = 1
        go to 50
     end if
     30 continue
  end do

  ! NO VALID SPECIES GROUP CODE WAS FOUND. RETURN TO CHECK FOR VALID
  ! ALPHA SPECIES CODE

  IFLAG = 0
  go to 100

  50 continue
  KARD = TEMP

  100 continue
  return
end subroutine SGDECD
