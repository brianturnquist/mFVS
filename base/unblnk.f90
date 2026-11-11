!==========================================!
!   Remove blanks from character strings   !
!==========================================!
! BASE $Id$
subroutine UNBLNK (RECORD,IRLEN)
  implicit none

  !     THIS SUBROUTINE REMOVES ALL BLANKS FROM A CHARACTER
  !     STRING, LEAVING THE NONBLANK CONTENTS OF THE STRING
  !     LEFT JUSTIFIED, AND THE REST OF THE STRING BLANK.
  !     IRLEN IS RETURNED AS THE LENGTH OF THE NONBLANK PORTION.

  ! Variables
  integer :: IRLEN,LENREC,I
  character(len=*) :: RECORD

  IRLEN = 0

  LENREC = LEN(RECORD)
  do I = 1,LENREC
     if (RECORD(I:I).ne.' ') then
        IRLEN = IRLEN+1
        if (IRLEN.lt.I) RECORD(IRLEN:IRLEN) = RECORD(I:I)
     end if
  end do
  if (IRLEN.lt.LENREC) RECORD(IRLEN+1:LENREC) = ' '

  return
end subroutine UNBLNK
