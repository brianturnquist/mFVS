!=================!
!   rd : rdroot   !
!=================!
! RD $Id$
subroutine RDROOT(I,A,ANS,F1,F2,HEI)
  implicit none

  !  FUNCTION FOR CALCULATING THE ROOT RADIUS OF LIVE TREES

  !  CALLED BY :
  !     RDDAM   [ROOT DISEASE]
  !     RDESTB  [ROOT DISEASE]
  !     RDIN    [ROOT DISEASE]
  !     RDPRIN  [ROOT DISEASE]
  !     RDSTR   [ROOT DISEASE]
  !     RDTREG  [ROOT DISEASE]

  !  CALLS     :
  !     NONE

  !  PARAMETERS :
  !     I      - SPECIES
  !     A      - DBH
  !     ANS    - ROOT RADIUS
  !     F1     - PROOT
  !     F2     - RSLOP
  !     HEI    - HEIGHT

  !  Revision History :
  !   01/20/94 - Last revision date.
  !   09/02/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'
  include 'PLOT.f90'
  include 'RDADD.f90'

  ! Variables
  integer :: I
  real    :: A,ANS,EFFECT,F1,F2,HEI,SDINEW
  integer :: IDANUW

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  IDANUW = I

  !     CALCULATE THE EFFECT OF SDI (IF KEYWORD WAS NOT USED,
  !     THERE MAY BE NO EFFECT)

  SDINEW = (OLDTPA / GROSPC) * (ORMSQD / 10.0) ** 1.605

  if (SDINEW .gt.0.0) then
     EFFECT = SDISLP * SDINEW + YINCPT
  else
     EFFECT = 1.0
  end if

  EFFECT = MIN(EFFECT,1.5)
  EFFECT = MAX(EFFECT,0.5)

  !     NOW CALCULATE THE ROOT RADIUS

  if (A .lt.3.5) goto 1000
  ANS = EFFECT * F2 * F1 * A / 12.0
  goto 9999

  !     ROOT RADIUS FOR SMALL (<3.5 DBH) TREES.

  1000 continue
  ANS = 0.01
  if (HEI .eq.0.0.or. BA .eq.0.0) goto 9999
  ANS = EXP(0.61157 * ALOG(HEI) + 0.04032 * ALOG(BA) - 0.80815)

  ANS = ANS * EFFECT

  9999 continue

  return
end
