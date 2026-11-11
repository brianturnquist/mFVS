!=================!
!   rd : rdrani   !
!=================!
! RD $Id$
subroutine RDRANI(SSEED)
  implicit none

  !  THIS SUBROUTINE INITIALIZES THE RANDOM NUMBER GENERATOR FOR THE
  !  ROOT DISEASE MODEL.

  !  CALLED BY :
  !     RDMN1   [ROOT DISEASE]

  !  CALLS     :
  !     NONE

  !  PARAMETERS :
  !     SSEED - SEED VALUE FOR THE ROOT DISEASE RANDOM NUMBER GENERATOR.

  !  Revision History :
  !   11/06/89 - Last revision date.
  !   09/02/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  !OMMON

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'
  include 'RDADD.f90'

  ! Variables
  integer :: ISEED
  real    :: SSEED

  ISEED = INT(SSEED)
  if (MOD(ISEED,2) .eq.0) ISEED = ISEED + 1
  SS = FLOAT(ISEED)
  S0 = SS

  return
end
