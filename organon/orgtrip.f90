!=======================!
!   organon : orgtrip   !
!=======================!
! ORGANON $Id$
subroutine ORGTRIP(I,ITFN)
  implicit none

  !     TRIPLES TREE VARIABLES NEEDED FOR ORGANON PROCESSING.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ORGANON.f90'

  ! Variables
  integer :: I,ITFN

  IORG(ITFN) = IORG(I)

  return
end
