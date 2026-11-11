!====================!
!   wsbwe : bweuky   !
!====================!
! WSBWE $Id$
subroutine BWEUKY (KEYWRD)
  implicit none

  !  THESE ROUTINES CONVERT KEYWORDS THAT ARE INPUT IN LOWER CASE LETTERS
  !  TO UPPER CASE LETTERS.

  ! Variables
  character(len=8) :: KEYWRD
  integer :: I

  do I = 1,8
     call BWEUCA(KEYWRD(I:I))
     40 continue
  end do
  return
end
