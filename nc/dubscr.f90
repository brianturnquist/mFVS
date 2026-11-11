!====================================================!
!   Estimate missing and regeneration crown ratios   !
!====================================================!
! NC $Id$
subroutine DUBSCR(ISPC,D,H,PRD,QMDPLT,CR,TPCT,TPCCF)
  implicit none

  !  THIS SUBROUTINE CALCULATES CROWN RATIOS FOR TREES INSERTED BY
  !  THE REGENERATION ESTABLISHMENT MODEL.  IT ALSO DUBS CROWN RATIOS
  !  FOR TREES IN THE INVENTORY THAT ARE MISSING CROWN RATIO
  !  MEASUREMENTS AND ARE LESS THAN 1.0 INCH DBH.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'
  include 'PDEN.f90'

  ! Variables
  logical :: DEBUG
  external RANN
  integer :: ISPC
  real    :: D,H,CR,FCR,SD,TPCT,TPCCF,BACHLO
  real    :: BCR0(MAXSP),BCR1(MAXSP),BCR2(MAXSP),BCR3(MAXSP),CRSD(MAXSP),BCR5(MAXSP),BCR6(MAXSP),&
       &BCR8(MAXSP),BCR9(MAXSP),BCR10(MAXSP)
  real    :: RDANUW,PRD,QMDPLT,HDR

  data BCR0/&
       &-1.669490, -1.669490,  -.426688,  -.426688,  -.426688,&
       &-0.426688, -1.669490,  -.426688,  -.426688, -1.669490,&
       &-2.19723,   0.00000/

  data BCR1/&
       &-.209765,  -.209765,  -.093105,  -.093105,  -.093105,&
       &-.093105,  -.209765,  -.093105,  -.093105,  -.209765,&
       &.000000,    0.00000/

  data BCR2/&
       &.000000,   .000000,   .022409,   .022409,   .022409,&
       &.022409,   .000000,   .022409,   .022409,   .000000,&
       &.000000,   0.00000/

  data BCR3/&
       &.003359,   .003359,   .002633,   .002633,   .002633,&
       &.002633,   .003359,   .002633,   .002633,   .003359,&
       &.000000,   0.00000/

  data BCR5/&
       &.011032,   .011032,   .000000,   .000000,   .000000,&
       &.000000,   .011032,   .000000,   .000000,   .011032,&
       &.000000,   0.00000/

  data BCR6/&
       &.000000,   .000000,  -.045532,  -.045532,  -.045532,&
       &-.045532,   .000000,  -.045532,  -.045532,   .000000,&
       &.000000,   0.00000/

  data BCR8/&
       &.017727,   .017727,   .000000,   .000000,   .000000,&
       &.000000,   .017727,   .000000,   .000000,   .017727,&
       &.000000,   0.00000/

  data BCR9/&
       &-.000053,  -.000053,   .000022,   .000022,   .000022,&
       &.000022,  -.000053,   .000022,   .000022,  -.000053,&
       &.000000,   0.00000/

  data BCR10/&
       &.014098,   .014098,  -.013115,  -.013115,  -.013115,&
       &-.013115,   .014098,  -.013115,  -.013115,   .014098,&
       &.000000,   0.00000/

  data CRSD/&
       &.5000,.5000,.6957,.6957, .6957, .9310,&
       &.6124,.6957,.6957,.4942, 0.200, .1500/

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = TPCT

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'DUBSCR',6,ICYC)

  ! RW CR CALCULATION
  ! EXPECTED CROWN RATIO FOR RW IS A FUNCTION OF DBH, HEIGHT DIAMETER RATIO (H/D),
  ! RELATIVE DENSITY AND RELATIVE DIAMETER THE MODEL IS BASED ON THE
  ! LOGISTIC FUNCTION, AND RETURNS A VALUE BETWEEN ZERO AND ONE.

  !  CALCULATE HEIGHT DIAMETER RATIO AND CONSTRAIN IF NEEDED
  HDR = (H*12)/D

  ! RW CR CALCULATION
  if (ISPC .eq.12) then
     CR = -1.021064 + 0.309296*LOG(HDR) +&
          &0.869720*PRD - 0.116274*(D/QMDPLT)

     ! RW DEBUG
     if (DEBUG)write(JOSTND,*)' IN DUBSCR - RW DEBUG',' D=',D,&
          &' H=',H,' PRD=',PRD,' QMDPLT=',QMDPLT,' HDR=', HDR,' CR=',CR

     !  EXPECTED CROWN RATIO IS A FUNCTION OF SPECIES, DBH, BASAL AREA, BAL,
     !  AND PCCF.  THE MODEL IS BASED ON THE LOGISTIC FUNCTION,
     !  AND RETURNS A VALUE BETWEEN ZERO AND ONE.

     ! ALL OTHER SPECIES CR CALCULATION
  else
     CR = BCR0(ISPC)&
          &+ BCR1(ISPC)*D&
          &+ BCR2(ISPC)*H&
          &+ BCR3(ISPC)*BA&
          &+ BCR5(ISPC)*TPCCF&
          &+ BCR6(ISPC)*(AVH/H)&
          &+ BCR8(ISPC)*AVH&
          &+ BCR9(ISPC)*(BA*TPCCF)&
          &+ BCR10(ISPC)*RMAI
  end if

  !  A RANDOM ERROR IS ASSIGNED TO THE CROWN RATIO PREDICTION
  !  PRIOR TO THE LOGISTIC TRANSFORMATION.  LINEAR REGRESSION
  !  WAS USED TO FIT THE MODELS AND THE ELEMENTS OF CRSD
  !  ARE THE STANDARD ERRORS FOR THE LINEARIZED MODELS BY SPECIES.

  SD = CRSD(ISPC)
  10 FCR = BACHLO(0.0,SD,RANN)
  if (ABS(FCR).gt.SD) go to 10
  if (ABS(CR+FCR).ge.86.)CR = 86.
  CR = 1.0/(1.0+EXP(CR+FCR))
  if (CR .gt..95) CR = .950
  if (CR .lt..05) CR = .05
  if (DEBUG)write(JOSTND,*)' IN DUBSCR','CR=',CR,' FCR=',FCR
  !      IF(DEBUG)WRITE(JOSTND,600)ISPC,D,H,TBA,TPCCF,CR,FCR,RMAI
  ! 600  FORMAT(' IN DUBSCR, ISPC=',I2,' DBH=',F4.1,' H=',F5.1,
  !    &       ' TBA=',F7.3,' TPCCF=',F8.4,' CR=',F4.3,
  !    &       ' RAN ERR = ',F6.4,' RMAI= ',F9.4)

  return
end
