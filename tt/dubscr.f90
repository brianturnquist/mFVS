!====================================================!
!   Estimate missing and regeneration crown ratios   !
!====================================================!
! TT $Id$
subroutine DUBSCR(ISPC,D,H,CR,TPCT,TPCCF)
  implicit none

  !  THIS SUBROUTINE CALCULATES CROWN RATIOS FOR TREES INSERTED BY
  !  THE REGENERATION ESTABLISHMENT MODEL.  IT ALSO DUBS CROWN RATIOS
  !  FOR TREES IN THE INVENTORY THAT ARE MISSING CROWN RATIO
  !  MEASUREMENTS AND ARE LESS THAN 5.0 INCHES DBH.  FINALLY, IT IS
  !  USED TO REPLACE CROWN RATIO ESTIMATES FOR ALL TREES THAT
  !  CROSS THE THRESHOLD BETWEEN THE SMALL AND LARGE TREE MODELS.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'
  include 'PDEN.f90'

  external RANN
  ! Variables
  logical :: DEBUG
  integer :: ISPC
  real    :: BCR0(MAXSP),BCR1(MAXSP),BCR2(MAXSP),BCR3(MAXSP),BCR5(MAXSP),BCR6(MAXSP),BCR8(MAXSP),&
       &BCR9(MAXSP),BCR10(MAXSP),CRSD(MAXSP)
  real    :: TPCCF,TPCT,CR,H,D,SD,FCR,BACHLO
  real    :: RDANUW

  ! SPECIES ORDER FOR TETONS VARIANT:

  !  1=WB,  2=LM,  3=DF,  4=PM,  5=BS,  6=AS,  7=LP,  8=ES,  9=AF, 10=PP,
  ! 11=UJ, 12=RM, 13=BI, 14=MM, 15=NC, 16=MC, 17=OS, 18=OH

  ! VARIANT EXPANSION:
  ! BS USES ES EQUATIONS FROM TT
  ! PM USES PI (COMMON PINYON) EQUATIONS FROM UT
  ! PP USES PP EQUATIONS FROM CI
  ! UJ AND RM USE WJ (WESTERN JUNIPER) EQUATIONS FROM UT
  ! BI USES BM (BIGLEAF MAPLE) EQUATIONS FROM SO
  ! MM USES MM EQUATIONS FROM IE
  ! NC AND OH USE NC (NARROWLEAF COTTONWOOD) EQUATIONS FROM CR
  ! MC USES MC (CURL-LEAF MTN-MAHOGANY) EQUATIONS FROM SO
  ! OS USES OT (OTHER SP.) EQUATIONS FROM TT

  data BCR0/&
       &-1.669490, -1.669490,  -.426688,  -2.19723,  -.426688,&
       &-.426688, -1.669490,  -.426688,  -.426688,  -0.17561,&
       &-2.19723,  -2.19723,       5.0,  -.426688,  -.426688,&
       &5.0,  -2.19723,  -.426688/

  data BCR1/&
       &-.209765,  -.209765,  -.093105,       0.0,  -.093105,&
       &-.093105,  -.209765,  -.093105,  -.093105,  -0.33847,&
       &0.0,       0.0,       0.0,  -.093105,  -.093105,&
       &0.0,       0.0,  -.093105/

  data BCR2/&
       &0.0,       0.0,   .022409,       0.0,   .022409,&
       &.022409,       0.0,   .022409,   .022409,   0.05699,&
       &0.0,       0.0,       0.0,   .022409,   .022409,&
       &0.0,       0.0,   .022409/

  data BCR3/&
       &.003359,   .003359,   .002633,       0.0,   .002633,&
       &.002633,   .003359,   .002633,   .002633,   0.00692,&
       &0.0,       0.0,       0.0,   .002633,   .002633,&
       &0.0,       0.0,   .002633/

  data BCR5/&
       &.011032,   .011032,       0.0,       0.0,       0.0,&
       &0.0,   .011032,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0/

  data BCR6/&
       &0.0,       0.0,  -.045532,       0.0,  -.045532,&
       &-.045532,       0.0,  -.045532,  -.045532,       0.0,&
       &0.0,       0.0,       0.0,  -.045532,  -.045532,&
       &0.0,       0.0,  -.045532/

  data BCR8/&
       &.017727,   .017727,       0.0,       0.0,       0.0,&
       &0.0,   .017727,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0/

  data BCR9/&
       &-.000053,  -.000053,   .000022,       0.0,   .000022,&
       &.000022,  -.000053,   .000022,   .000022,       0.0,&
       &0.0,       0.0,       0.0,   .000022,   .000022,&
       &0.0,       0.0,   .000022/

  data BCR10/&
       &.014098,   .014098,  -.013115,       0.0,  -.013115,&
       &-.013115,   .014098,  -.013115,  -.013115,       0.0,&
       &0.0,       0.0,       0.0,  -.013115,  -.013115,&
       &0.0,       0.0,  -.013115/

  data CRSD/&
       &.5000,     .5000,     .6957,     .2000,     .6957,&
       &.9310,     .6124,     .6957,     .6957,     .8866,&
       &.2000,     .2000,     .5000,     .9310,     .6957,&
       &.5000,       0.2,     .6957/

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = TPCT

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'DUBSCR',6,ICYC)
  if (DEBUG) write(JOSTND,3)ICYC
  3 format(' ENTERING SUBROUTINE DUBSCR  CYCLE =',I5)

  !  EXPECTED CROWN RATIO IS A FUNCTION OF SPECIES, DBH, BASAL AREA, BAL,
  !  AND PCCF.  THE MODEL IS BASED ON THE LOGISTIC FUNCTION,
  !  AND RETURNS A VALUE BETWEEN ZERO AND ONE.

  CR = BCR0(ISPC)&
       &+ BCR1(ISPC)*D&
       &+ BCR2(ISPC)*H&
       &+ BCR3(ISPC)*BA&
       &+ BCR5(ISPC)*TPCCF&
       &+ BCR6(ISPC)*(AVH/H)&
       &+ BCR8(ISPC)*AVH&
       &+ BCR9(ISPC)*(BA*TPCCF)&
       &+ BCR10(ISPC)*RMAI

  !  A RANDOM ERROR IS ASSIGNED TO THE CROWN RATIO PREDICTION
  !  PRIOR TO THE LOGISTIC TRANSFORMATION.  LINEAR REGRESSION
  !  WAS USED TO FIT THE MODELS AND THE ELEMENTS OF CRSD
  !  ARE THE STANDARD ERRORS FOR THE LINEARIZED MODELS BY SPECIES.

  SD = CRSD(ISPC)
  10 continue
  FCR = 0.0
  if (DGSD.ge.1.0) FCR = BACHLO(0.0,SD,RANN)
  if (ABS(FCR).gt.SD) go to 10

  select case (ISPC)
  case(13,16)
     CR = ((CR-1.0)*10.0 + 1.0)/100.
  case default
     if (ABS(CR+FCR).ge.86.)CR = 86.
     CR = 1.0/(1.0+EXP(CR+FCR))
  end select

  if (CR .gt..95) CR = .950
  if (CR .lt..05) CR = .05
  if (DEBUG)write(JOSTND,600)ISPC,D,H,TPCCF,CR,FCR,RMAI
  600 format(' IN DUBSCR, ISPC=',I2,' DBH=',F4.1,' H=',F5.1,&
       &' TPCCF=',F8.4,' CR=',F4.3,&
       &' RAN ERR = ',F6.4,' RMAI= ',F9.4)

  return
end
