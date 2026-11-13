!======================================================!
!   Fixed heights for species with no regen equation   !
!======================================================!
! CI $Id$
subroutine ESFIXH (I,PN,HHT)
  implicit none

  !     HEIGHT OF THE TALLEST ADVANCE (ESADVH) OR SUBSEQUENT (ESSUBH)
  !     TREE FOR SPECIES THAT HAVE NO REGENERATION HEIGHT EQUATION.
  !     ESADVH AND ESSUBH USE THE SAME FIXED VALUES; THEY CALL THIS
  !     ROUTINE ONLY FOR THE SPECIES LISTED BELOW.

  ! Variables
  integer :: I
  real    :: PN,HHT

  PN = 0.0
  select case (I)

     !     WHITEBARK PINE, PACIFIC YEW, WESTERN JUNIPER,
     !     CURLLEAF MOUNTAIN-MAHOGANY, LIMBER PINE

  case (11,12,14,15,16)
     HHT = 0.5

     !     QUAKING ASPEN, BLACK COTTONWOOD, OTHER HARDWOODS

  case (13,17,19)
     HHT = 5.0
  end select
  return
end
