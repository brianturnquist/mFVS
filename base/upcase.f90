!==================================!
!   Convert strings to uppercase   !
!==================================!
! BASE $Id$
subroutine UPCASE (C)
  implicit none

  ! Variables
  integer :: IP
  character(len=1) :: C
  character(len=26) :: UPPER
  character(len=26) :: LOWER
  data UPPER /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
  data LOWER /'abcdefghijklmnopqrstuvwxyz'/
  IP = INDEX(LOWER,C)
  if (IP.gt.0) C = UPPER(IP:IP)
  return
end subroutine UPCASE
