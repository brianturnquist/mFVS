!=======================================!
!   Handle unused habitat type inputs   !
!=======================================!
! AK $Id$
subroutine HABTYP (KARD2,ARRAY2)
  implicit none

  !  DUMMY HABITAT ROUTINE USED IN VARIANTS THAT DON'T USE
  !  HABITAT TYPE AS A VARIABLE IN GROWTH FUNCTIONS.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'VARCOM.f90'

  ! Variables
  character(len=10) :: KARD2

  integer :: ITYPE,KODTYP

  real    :: ARRAY2,RDANUW

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = ARRAY2

  KODTYP = 0
  ITYPE = 0
  ICL5 = 999
  KARD2 = 'UNKNOWN   '
  PCOM = 'UNKNOWN '
  if (LSTART)write(JOSTND,21)
  21 format(/,T12,'HABITAT TYPE IS NOT USED IN THIS VARIANT')
  return
end subroutine HABTYP
