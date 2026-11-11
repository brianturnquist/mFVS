!==========================================!
!   Remove blanks from character strings   !
!==========================================!
! CANADA-ON $Id$
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

  ! if ON or BC then allow embedded spaces with path/file names
  ! could also strip CHR(33) quotation marks with this sub.

  RECORD = ADJUSTL(RECORD)
  IRLEN = LEN_TRIM(RECORD)

  return
end
