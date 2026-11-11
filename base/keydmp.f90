!===========================!
!   Write keyword records   !
!===========================!
! BASE $Id$
subroutine KEYDMP (IOUT,IRECNT,KEYWRD,ARRAY,KARD)
  implicit none

  ! Variables
  integer :: IRECNT,IOUT
  real    :: ARRAY(12)
  character(len=10) :: KARD(12)
  character(len=8) :: KEYWRD
  write (IOUT,70) IRECNT,KEYWRD,ARRAY,KARD
  70 format (/,' CARD NUM =',I5,'; KEYWORD FIELD = ''',A8,''''/ &
       & '      PARAMETERS ARE:',12F14.12,/ &
       & '      COL 11 TO 130 =''',12A10,'''')

  return
end subroutine KEYDMP
