!==============================!
!   Find keywords in a table   !
!==============================!
! BASE $Id$
subroutine FNDKEY (NUMBER,KEYWRD,TABLE, &
     & ISIZE,KODE,DEBUG,IOUT)
  implicit none

  !    FUNCTION TO FIND A KEYWORD IN A TABLE.

  !    NUMBER= THE KEYWORD NUMBER.
  !    ISIZE = THE NUMBER OF ENTRIES IN THE TABLE
  !    KODE  = RETURN CODE:
  !            0= NO ERRORS FOUND.
  !            1= KEYWORD NOT FOUND
  !    KEYWRD= THE KEYWORD AS A CHARACTER*8.
  !    TABLE = KEYWORD TABLE AS A CHARACTER*8 ARRAY OF LENGTH ISIZE
  !    DEBUG = TRUE IT DEBUGING.
  !    IOUT  = OUTPUT WRITE UNIT REFERENCE NUMBER

  !    NUMBER IS THE POSITION WITHIN THE TABLE WHICH HOLDS THE
  !    KEYWORD.

  ! Variables
  character(len=8) :: KEYWRD
  character(len=8) :: TABLE(ISIZE)
  logical :: DEBUG
  integer :: IOUT,KODE,NUMBER,ISIZE,J
  logical :: LDANUW

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  LDANUW = DEBUG

  NUMBER = 0

  do J = 1,ISIZE
     if (KEYWRD .eq. TABLE(J)) go to 110
     100 continue
  end do
  KODE = 1
  write (IOUT,85) KEYWRD
  85 format (/1X,'''',A8,''' :KEYWORD SPECIFIED')
  return
  110 continue
  KODE = 0
  NUMBER = J
  return
end subroutine FNDKEY
