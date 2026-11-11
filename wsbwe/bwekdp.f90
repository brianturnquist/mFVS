!====================!
!   wsbwe : bwekdp   !
!====================!
! WSBWE $Id$
subroutine BWEKDP (IOUT,IRECNT,KEYWRD,ARRAY,KARD)
  implicit none

  !  Revision History:
  !    14-JUL-2010 Lance R. David (FMSC)
  !       Previous note revision date 1986
  !       Added IMPLICIT NONE and declared variables as needed.

  ! Variables
  character(len=10) :: KARD(12)
  character(len=8) :: KEYWRD
  integer :: IOUT,IRECNT
  real    :: ARRAY(12)
  write (IOUT,70) IRECNT,KEYWRD,ARRAY,KARD
  70 format (/,'CARD NUM =',I5,'; KEYWORD FIELD = ''',A8,''''/,&
       &'     PARAMETERS ARE:',7F14.7,/,&
       &'     COL 11 TO 80 =''',7A10,'''')

  return
end
