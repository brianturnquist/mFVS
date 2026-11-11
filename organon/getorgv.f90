!=======================!
!   organon : getorgv   !
!=======================!
! ORGANON $Id$
subroutine GETORGV(I,VALU)
  implicit none

  !  SUBROUTINE TO RETURN EVENT MONITOR VARIABLES

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ORGANON.f90'

  ! Variables
  integer :: I
  real    :: VALU

  select case (I)
  case(1)
     VALU = OCC
  case(2)
     VALU = OAHT
  end select

  return
end
