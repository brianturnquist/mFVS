!================================!
!   Check label set membership   !
!================================!
! BASE $Id$
  ! Variables
logical function LBMEMR (LENMEM,MEM,LENSET,SET)
  implicit none

  !     TRUE IF MEM IS A MEMBER OF SET, FALSE OTHERWISE.

  !     PART OF THE LABEL PROCESSING COMPONENT OF THE PROGNOSIS SYSTEM
  !     N.L. CROOKSTON -- INTERMOUNTAIN RESEARCH STATION -- JAN 1987

  !     LENMEM= LENGTH OF MEMBER STRING
  !     MEM   = C*250 STRING CONTAINING THE MEMBER
  !     LENSET= LENGTH OF THE DEFINED PART STRING SET
  !     SET   = C*250 STRING CONTAINING THE SET

  character(len=250) :: SET
  character(len=250) :: MEM
  character(len=250) :: WRK
  integer :: LENSET,LENMEM,IP,LENWRK

  LBMEMR = .false.

  !     LOOP THROUGH THE MEMBERS OF SET

  IP = 1
  10 continue

  !     IF IP IS GREATER THAN THE LENGTH OF THE SET, END THE LOOP

  if (IP.gt.LENSET) goto 20

  !     SET WRK BE THE FIRST MEMBER OF SET BETWEEN LOCATION IP AND
  !     LENSET.

  call LB1MEM (IP,LENSET,SET,LENWRK,WRK)

  !     COMPARE MEM WITH WRK...IF EQUAL, THEN MEMBERSHIP HAS BEEN
  !     ESTABLISHED.  IF NOT, CONTINUE THE SEARCH.

  if (LENMEM .eq. LENWRK) then
     if (MEM(1:LENMEM).eq.WRK(1:LENWRK)) then
        LBMEMR = .true.
        goto 20
     end if
  end if

  !     INCREMENT IP TO POINT TO THE NEXT PART OF SET AND BRANCH BACK.

  IP = IP+LENWRK+2
  goto 10
  20 continue
  return
end function LBMEMR
