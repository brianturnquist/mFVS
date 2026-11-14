!===================!
!   wpbr : brupdt   !
!===================!
! WPBR $Id$
subroutine BRUPDT
  implicit none

  !  Purpose:
  !  Updates tree ground diameter and converts it to centimeters
  !  for each cycle in the Blister Rust model.

  !  Revision History:

  !  dd-MMM-YYYY programmer_name
  !     description of change or update.
  !  15-SEP-2000 Lance David (FHTET)
  !     Transfered Glen Brink's July, 2000 modifications from older version
  !     of blister rust source code:
  !     Modified to allow blister rust on other species using ISPBR array.
  !     Species loop (label 45) and species temp index variable (I3)
  !     are new.
  !  09-MAY-2001 Lance R. David (FHTET)
  !     Changed ISPBR to BRSPM.

  !.... Common include files

  ! Common blocks
  include 'PRGPRM.f90'
  include 'BRCOM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'

  !.... Local variables.
  ! Variables
  integer :: I1,I2,I3,J,K

  !.... If no tree records then return.

  if (ITRN.eq.0) go to 50

  !.... Process the host pines, if none then return.
  !.... Start species loop.

  do I3 = 1, MAXSP

     if (BRSPM(I3) .eq.0) go to 45

     I1 = ISCT(I3,1)
     if (I1.eq.0) go to 45
     I2 = ISCT(I3,2)
     do J = I1,I2
        K = IND1(J)

        !....    Update the ground diameter of this tree.

        BRGD(K) = BRGD(K)+DG(K)*2.54
        22 continue
     end do

     !.... End species loop.
     45 continue
  end do

  !.... Common return.

  50 continue
  return
end
