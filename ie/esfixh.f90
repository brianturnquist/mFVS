!======================================================!
!   Fixed heights for species with no regen equation   !
!======================================================!
! IE $Id$
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

     !     LIMBER PINE, PINYON, JUNIPER, PACIFIC YEW

  case (13,15,16,17)
     HHT = 0.5

     !     ASPEN, COTTONWOOD, MOUNTAIN MAPLE, PAPER BIRCH, OTHER HARDWOODS

  case (18,19,20,21,22)
     HHT = 5.0
  end select
  return
end
