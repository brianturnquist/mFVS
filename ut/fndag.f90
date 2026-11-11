!================!
!   ut : fndag   !
!================!
! UT $Id$
subroutine FNDAG(TAGE,SITE,RHT,BAUTBA,DEBUG)
  implicit none

  ! THIS ROUTINE USES THE LOGIC FROM GENGYM THAT CALCULATES A HEIGHT
  ! GIVEN A SITE TO FIND AN EFFECTIVE AGE.
  ! EVEN-AGED LOGIC FROM GEMHT

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'

  ! Variables
  logical :: DEBUG
  real    :: BAUTBA,RHT,SITE,TAGE,AGETEM,TOL,AP,AGEMAX
  real    :: HH,RATIO,DIFF,TSITE
  logical :: LDANUW

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  LDANUW = DEBUG

  TOL = 2.0
  AP = 10.0
  AGEMAX = 210.

  ! SPRUCE-FIR -- MODEL TYPE 4

  100 continue
  AGETEM = AP
  if (AGETEM .lt.30.0) AGETEM = 30.
  HH = (2.75780*SITE**0.83312) * ((1.0-EXP(-0.015701*AGETEM))&
       &**(22.71944*SITE**(-0.63557))) + 4.5
  !     IF(AP .LT. AGETEM) HH = ((HH-4.5) / AGETEM) * AP + 4.5
  RATIO = 1.0 - BAUTBA
  if (RATIO .lt.0.728) RATIO = 0.728
  HH = HH * RATIO
  DIFF = ABS(HH - RHT)
  if (DIFF .lt. TOL) go to 9990
  if (HH .gt. RHT) go to 9990
  AP = AP + 5.0
  if (AP .gt. AGEMAX) then
     TAGE = AGEMAX
     go to 9995
  end if
  go to 100
  9990 continue
  TAGE = AGETEM
  TSITE = SITE
  if (TSITE.lt.20.)TSITE = 20.
  TAGE = TAGE+4.5/(-0.22+0.0155*TSITE)
  9995 continue

  return
end
