!====================================================!
!   Estimate missing and regeneration crown ratios   !
!====================================================!
! CR $Id$
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
  real    :: BCR0(11),BCR1(11),BCR2(11),BCR3(11),CRSD(11),BCR5(11),BCR6(11),BCR8(11),BCR9(11),BCR10(11)
  integer :: MAP1(MAXSP),MAP2(MAXSP),MAP3(MAXSP),MAP4(MAXSP),MAP5(MAXSP)
  integer :: ISPC,IMAP
  real    :: TPCCF,TPCT,CR,H,D,SD,FCR,BACHLO
  real    :: RDANUW

  !  SPECIES ORDER:
  !   1=AF,  2=CB,  3=DF,  4=GF,  5=WF,  6=MH,  7=RC,  8=WL,  9=BC, 10=LM,
  !  11=LP, 12=PI, 13=PP, 14=WB, 15=SW, 16=UJ, 17=BS, 18=ES, 19=WS, 20=AS,
  !  21=NC, 22=PW, 23=GO, 24=AW, 25=EM, 26=BK, 27=SO, 28=PB, 29=AJ, 30=RM,
  !  31=OJ, 32=ER, 33=PM, 34=PD, 35=AZ, 36=CI, 37=OS, 38=OH

  !  SPECIES EXPANSION:
  !  UJ,AJ,RM,OJ,ER USE CR JU
  !  NC,PW USE CR CO
  !  GO,AW,EM,BK,SO USE CR OA
  !  PB USES CR AS
  !  PM,PD,AZ USE CR PI
  !  CI USES CR PP

  data MAP1/&
       &9,  9,  3,  9,  4,  2,  2,  2,  1,  1,&
       &2, 11, 10,  1,  1, 11,  7,  8,  8,  6,&
       &5,  5,  5,  5,  5,  5,  5,  6, 11, 11,&
       &11, 11, 11, 11, 11, 10,  2,  5/
  data MAP2/&
       &4,  4,  3,  4,  4,  2,  2,  2,  1,  1,&
       &2,  9, 10,  1,  1, 11,  7,  7,  7,  6,&
       &5,  5,  8,  8,  8,  8,  8,  6, 11, 11,&
       &11, 11,  9,  9,  9, 10,  2,  5/
  data MAP3/&
       &2,  2,  3,  2,  2,  2,  2,  2,  2,  2,&
       &7,  2, 10,  2,  2, 11,  8,  8,  8,  6,&
       &1,  1,  5,  5,  5,  5,  5,  6, 11, 11,&
       &11, 11,  2,  2,  2, 10,  2,  5/
  data MAP4/&
       &9,  9,  3,  9,  9,  2,  2,  2,  2,  2,&
       &7,  2,  2,  2,  2,  2,  8,  8,  8,  6,&
       &5,  5,  5,  5,  5,  5,  5,  6,  2,  2,&
       &2,  2,  2,  2,  2,  2,  2,  5/
  data MAP5/&
       &9,  9,  3,  9,  9,  2,  2,  2,  2,  2,&
       &7,  2, 10,  2,  2,  2,  8,  8,  8,  6,&
       &5,  5,  5,  5,  5,  5,  5,  6,  2,  2,&
       &2,  2,  2,  2,  2, 10,  2,  5/

  data BCR2/&
       &.000000,   .000000,   .022409,   .022409,   .022409,   .022409,&
       &.000000,   .022409,   .022409,   .000000,   .000000/
  data BCR1/&
       &-.209765,  -.209765,  -.093105,  -.093105,  -.093105,  -.093105,&
       &-.209765,  -.093105,  -.093105,  -.209765,   .000000/
  data BCR5/&
       &.011032,   .011032,   .000000,   .000000,   .000000,   .000000,&
       &.011032,   .000000,   .000000,   .011032,   .000000/
  data BCR6/&
       &.000000,   .000000,  -.045532,  -.045532,  -.045532,  -.045532,&
       &.000000,  -.045532,  -.045532,   .000000,   .000000/
  data BCR8/&
       &.017727,   .017727,   .000000,   .000000,   .000000,   .000000,&
       &.017727,   .000000,   .000000,   .017727,   .000000/
  data BCR3/&
       &.003359,   .003359,   .002633,   .002633,   .002633,   .002633,&
       &.003359,   .002633,   .002633,   .003359,   .000000/
  data BCR9/&
       &-.000053,  -.000053,   .000022,   .000022,   .000022,   .000022,&
       &-.000053,   .000022,   .000022,  -.000053,   .000000/
  data BCR10/&
       &.014098,   .014098,  -.013115,  -.013115,  -.013115,  -.013115,&
       &.014098,  -.013115,  -.013115,   .014098,   .000000/
  data BCR0/&
       &-1.669490, -1.669490,  -.426688,  -.426688,  -.426688,  -.426688,&
       &-1.669490,  -.426688,  -.426688, -1.669490,  -2.19723/
  data CRSD/&
       &.5000,.5000,.6957,.6957,.6957,.9310,&
       &.6124,.6957,.6957,.4942,0.200/

  !  CHECK FOR DEBUG.

  !     CALL DBCHK (DEBUG,'DUBSCR',6,ICYC)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = TPCT

  !  EXPECTED CROWN RATIO IS A FUNCTION OF SPECIES, DBH, BASAL AREA, BAL,
  !  AND PCCF.  THE MODEL IS BASED ON THE LOGISTIC FUNCTION,
  !  AND RETURNS A VALUE BETWEEN ZERO AND ONE.

  if (IMODTY .eq.1) then
     IMAP = MAP1(ISPC)
  else if (IMODTY .eq.2) then
     IMAP = MAP2(ISPC)
  else if (IMODTY .eq.3) then
     IMAP = MAP3(ISPC)
  else if (IMODTY .eq.4) then
     IMAP = MAP4(ISPC)
  else
     IMAP = MAP5(ISPC)
  end if
  CR = BCR2(IMAP)*H&
       &+ BCR1(IMAP)*D&
       &+ BCR5(IMAP)*TPCCF&
       &+ BCR6(IMAP)*(AVH/H)&
       &+ BCR8(IMAP)*AVH&
       &+ BCR3(IMAP)*BA&
       &+ BCR9(IMAP)*(BA*TPCCF)&
       &+ BCR10(IMAP)*RMAI&
       &+ BCR0(IMAP)

  !  A RANDOM ERROR IS ASSIGNED TO THE CROWN RATIO PREDICTION
  !  PRIOR TO THE LOGISTIC TRANSFORMATION.  LINEAR REGRESSION
  !  WAS USED TO FIT THE MODELS AND THE ELEMENTS OF CRSD
  !  ARE THE STANDARD ERRORS FOR THE LINEARIZED MODELS BY SPECIES.

  SD = CRSD(IMAP)
  10 FCR = BACHLO(0.0,SD,RANN)
  if (ABS(FCR).gt.SD) go to 10
  if (ABS(CR+FCR).ge.86.)CR = 86.
  CR = 1.0/(1.0+EXP(CR+FCR))
  if (CR .gt..95) CR = .950
  if (CR .lt..05) CR = .05
  !     IF(DEBUG)WRITE(JOSTND,600)IMAP,D,H,TTBA,TPCCF,CR,FCR,RMAI,TAVH
  ! 600 FORMAT(' IN DUBSCR, IMAP=',I2,' DBH=',F4.1,' H=',F5.1,
  !    & ' TTBA=',F7.3,' TPCCF=',F8.4,' CR=',F4.3,
  !    &   ' RAN ERR = ',F6.4,' RMAI= ',F9.4,' TAVH=',F9.4)
  return
end
