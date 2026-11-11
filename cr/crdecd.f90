!=================!
!   cr : crdecd   !
!=================!
! CR $Id$
subroutine CRDECD (IHB,CNHB,MAXHB,ARRAY2,KARD2)
  implicit none

  !  CALLED FROM SUBROUTINE **HABTYP**.

  !     DECODE THE HABITAT TYPE/PLANT ASSOCIATION CODE.
  !     HT/PA CODE IS IN THE SECOND POSITION OF KARD AND ARRAY.
  !     AND IS USED AS A SCALAR KARD2 AND ARRAY2 IN THIS ROUTINE.

  !     IHB   = NUMERIC HABITAT CODE, ZERO IF NO HABITAT FOUND.
  !     CNHB  = C*8 VECTOR OF LENGTH MAXHB, HABITAT CODES.
  !     MAXHB = LENGTH OF CNHB.

  ! Variables
  integer :: IHB,MAXHB,J,I
  character(len=10) :: KARD2
  character(len=8) :: CNHB(MAXHB)
  character(len=8) :: TEMP
  real    :: ARRAY2
  real    :: RDANUW

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = ARRAY2

  IHB = 0

  !  LOAD TEMP WITH UP TO EIGHT NON-BLANK CHARS IN KARD.

  TEMP = 'UNKNOWN'
  J = 0
  do I = 1,10
     if (KARD2(I:I).ne.' ') then
        TEMP = ' '
        do J = 1,MIN(8,10-I+1)
           TEMP(J:J) = KARD2(I+J-1:I+J-1)
           call UPCASE(TEMP(J:J))
           5 continue
        end do
        go to 20
     end if
     10 continue
  end do

  !  NOTHING WAS FOUND.

  go to 60

  !  TRY TO DECODE THE HABITAT CODE.
  !  'UNKNOWN' IS HABITAT CODE ZERO.

  20 continue
  if (TEMP.ne.'UNKNOWN') then
     do I = 1,MAXHB
        if (TEMP(1:8).eq.CNHB(I)(1:8)) then
           IHB = I
           go to 60
        end if
        30 continue
     end do

     !  NO VALID HABITAT CODE WAS FOUND.  SET FLAG TO LOAD DEFAULT.

     IHB = -1
     go to 60
  end if

  60 continue
  return
end
