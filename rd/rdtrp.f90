!================!
!   rd : rdtrp   !
!================!
! RD $Id$
subroutine RDTRP (LTRP)
  implicit none

  !  SUBROUTINE WHICH RETURNS LTRP TO INDICATE WHETHER TRIPLING WILL
  !  OCCUR IN THE CONTEXT OF THE ROOT DISEASE MODEL.  IF TRIPLING WILL
  !  RESULT IN MORE THAN IRRTRE TREE RECORDS BEING CREATED THEN
  !  TRIPLING IS SUPPRESSED.

  !  CALLED BY :
  !     GRINCR  [PROGNOSIS]

  !  CALLS     :
  !     RDATV   (SUBROUTINE)   [ROOT DISEASE]

  !  PARAMETERS :
  !     LTRP   -

  !  Revision History:
  !   11/06/89 - Last revision date.
  !   09/04/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'
  include 'CONTRL.f90'

  ! Variables
  logical :: LTRP,LGO,LTEE
  integer :: ILIM

  call RDATV (LGO,LTEE)
  if (LGO) then
     ILIM = IRRTRE / 3
     LTRP = (ICYC .le. ICL4.and. ITRN .le. ILIM .and..not. NOTRIP)
  end if

  return
end
