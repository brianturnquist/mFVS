!===============================!
!   Format two digit integers   !
!===============================!
! BASE $Id$
subroutine CH2NUM (C2,ICYC)
  implicit none

  ! Variables
  integer :: ICYC
  character(len=2) :: C2

  !     WRITES A TWO DIGIT INTEGER INTO A CHARACTER*2 STRING.

  write (C2,20) ICYC
  20 format (I2)
  return
end subroutine CH2NUM
