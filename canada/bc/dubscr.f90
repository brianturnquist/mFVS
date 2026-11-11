!====================================================!
!   Estimate missing and regeneration crown ratios   !
!====================================================!
! CANADA-BC $Id$
subroutine DUBSCR(LVER,ISPC,XC,D,H,BAX,BAL,CR)
  implicit none

  !  THIS SUBROUTINE CALCULATES CROWN RATIOS FOR TREES INSERTED BY
  !  THE REGENERATION ESTABLISHMENT MODEL.  IT ALSO DUBS CROWN RATIOS
  !  FOR TREES IN THE INVENTORY THAT ARE MISSING CROWN RATIO
  !  MEASUREMENTS AND ARE LESS THAN 3.0 INCHES DBH.  FINALLY, IT IS
  !  USED TO REPLACE CROWN RATIO ESTIMATES FOR ALL TREES THAT
  !  CROSS THE THRESHOLD BETWEEN THE SMALL AND LARGE TREE MODELS.

  !  SPECIES EXPANSION:
  !  WB USE COEFFICIENTS FOR L
  !  LM AND PY USE COEFFICIENTS FROM TT FOR LM
  !  LL USE COEFFICIENTS FOR AF
  !  AS, MM, PB USE COEFFIECIENTS FOR AS FROM UT
  !  OS USE COEFFEICIENTS FOR MH
  !  CO, OH DO NOT USE THIS ROUTINE
  !  PI, JU DO NOT USE THIS ROUTINE

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'BCPLOT.f90'

  ! Variables
  logical :: LVER
  integer :: ISPC
  real    :: XC,D,H,BAX,BAL,CR

  !     V2
  external RANN
  real    :: SD,FCR,BACHLO
  real    :: BCR0(MAXSP),BCR1(MAXSP),BCR2(MAXSP),BCR3(MAXSP)
  real    :: CRSDX(MAXSP)

  data BCR0/&
       &-0.44316,-0.83965,-0.89122,-0.62646,-0.49548, 0.11847,&
       &-0.32466,-0.92007,-0.89014,-0.17561, 0.11847, 0.11847,&
       &0.11847,-0.89122, 0.11847/
  data BCR1/&
       &-0.48446,-0.16106,-0.18082,-0.06141, 0.00012,-0.39305,&
       &-0.20108,-0.22454,-0.18026,-0.33847,-0.39305,-0.39305,&
       &-0.39305,-0.18082,-0.39305/
  data BCR2/&
       &0.05825, 0.04161, 0.05186, 0.02360, 0.00362, 0.02783,&
       &0.04219, 0.03248, 0.02233, 0.05699, 0.02783, 0.02783,&
       &0.02783, 0.05186, 0.02783/
  data BCR3/&
       &0.00513, 0.00602, 0.00454, 0.00505, 0.00456, 0.00626,&
       &0.00436, 0.00620, 0.00614, 0.00692, 0.00626, 0.00626,&
       &0.00626, 0.00454, 0.00626/
  data CRSDX/&
       &0.9476,  0.7396,  0.8706,  0.9203,  0.9450,  0.8012,&
       &0.7707,  0.9721,  0.8871,  0.8866,  0.8012,  0.8012,&
       &0.8012,  0.8706,  0.8012/

  if (LVER) then
     CR = BCR0(ISPC) + BCR1(ISPC)*D + BCR2(ISPC)*H + BCR3(ISPC)*BAX
     SD = CRSDX(ISPC)
     10 continue
     FCR = 0.0
     if (DGSD.ge.1.0) FCR = BACHLO(0.0,SD,RANN)
     if (ABS(FCR).gt.SD) go to 10
     if (ABS(CR+FCR).ge.86.)CR = 86.
     CR = 1.0/(1.0 + EXP(CR+FCR))
  else
     call CRNMD (ISPC,XC,D,H,BAL,CR,0.0)
  end if
  if (CR .lt.0.05) CR = 0.05
  if (CR .gt.0.95) CR = 0.95

  return
end
