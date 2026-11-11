!=====================!
!   mistoe : misget   !
!=====================!
! MISTOE $Id$
subroutine MISGET(ITREE,IDMR)

  !  Purpose:
  !     Gets dwarf mistletoe rating (stored in array IMIST) for the
  !  current tree record.

  !  Call list definitions:
  !     ITREE:  (I) Current tree record number.
  !     IDMR:   (O) Dwarf mistletoe rating for current tree record.

  !  Common block variables and parameters:
  !     IMIST:  From MISCOM; array containing tree record DMR's.

  implicit none

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'

  !.... Common include files.

  include 'MISCOM.f90'

  ! Variables
  integer :: IDMR,ITREE

  !.... Get dwarf mistletoe rating for current tree.

  IDMR = IMIST(ITREE)

  return
end
