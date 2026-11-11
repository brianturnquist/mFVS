!====================!
!   wsbwe : bweuca   !
!====================!
! WSBWE $Id$
subroutine BWEUCA (C)
  implicit none

  ! Variables
  character(len=1) :: C
  character(len=26) :: UPPER
  character(len=26) :: LOWER
  integer :: IP

  data UPPER /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
  data LOWER /'abcdefghijklmnopqrstuvwxyz'/

  IP = INDEX(LOWER,C)
  if (IP.gt.0) C = UPPER(IP:IP)
  return
end
