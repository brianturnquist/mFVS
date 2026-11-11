!======================!
!   mistoe : misputz   !
!======================!
! MISTOE $Id$
subroutine MISPUTZ(ITREE,IDMR)

  !  Purpose:
  !     Sets dwarf mistletoe rating (stored in array IMIST) for the
  !  current tree record to the value passed in IDMR, and zeros
  !  elements in 3 DM arrays.
  !     Entry point MISPUT omits zeroing DM arrays, and is the same
  !  as the previous (before Sept. 1993) MISPUT code.

  !  Call list definitions:
  !     ITREE:  (I) Current tree record number.
  !     IDMR:   (I) Mistletoe rating to put into IMIST for current tree.

  !  Common block variables and parameters:
  !     IMIST:  From MISCOM; array containing tree record DMR's.

  implicit none

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'

  !.... Common include files.

  include 'MISCOM.f90'

  !.... Variable declarations.

  ! Variables
  integer :: IDMR,ITREE

  !.... Set zeros for initial values of DM variables.
  !.... At this time, MISPUTZ and MISPUT are equivalent.

  entry MISPUT(ITREE,IDMR)

  !.... Check for valid dwarf mistletoe rating.

  if (IDMR.ge.0.and.IDMR.le.6) then

     !....    Put value into IMIST for current tree record number.

     IMIST(ITREE) = IDMR

  else

     !....    Print error message and return.

     print*,' *MISPUT* Invalid dwarf mistletoe rating (',&
          &IDMR, ') input for tree ', ITREE
  end if

  return
end
