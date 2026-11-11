!===================================!
!   Convert keywords to uppercase   !
!===================================!
! BASE $Id$
subroutine UPKEY (KEYWRD)
  implicit none

  !  THESE ROUTINES CONVERT KEYWORDS THAT ARE INPUT IN LOWER CASE LETTERS
  !  TO UPPER CASE LETTERS.

  ! Variables
  integer :: I
  character(len=8) :: KEYWRD
  do I = 1,8
     call UPCASE(KEYWRD(I:I))
     40 continue
  end do
  return
end subroutine UPKEY
