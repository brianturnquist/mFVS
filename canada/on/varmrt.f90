!========================!
!   canada/on : varmrt   !
!========================!
! CANADA-ON $Id$
subroutine VARMRT(TOKILL,DEBUG,SUMKIL,DQ10)
  implicit none

  ! SUBROUTINE TO DISTRIBUTE MORTALITY ACCORDING TO PERCENTILE AND
  ! SPECIES TOLERANCE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'OUTCOM.f90'
  include 'PLOT.f90'
  include 'COEFFS.f90'
  include 'ESTREE.f90'
  include 'MULTCM.f90'
  include 'PDEN.f90'
  include 'METRIC.f90'

  ! Variables
  logical :: DEBUG

  integer :: I,IG,ISWTCH,J,JPASS,JSPC,MINSTP,NPASS
  integer :: ITM_MAP(MAXSP)

  real    :: ADJUST,BALM,DM,DQM,DQ10,OTEM2,PASS1,PEFF,SHORT,SUMKIL
  real    :: TEMKIL,TEMSUM,TOKILL,TPALFT,XKILL
  real    :: EFFTR(MAXTRE),MB0(18),MB1(18),MB2(18),MB3(18),MB4(18),MB5(18)
  real    :: MB6(18),MB7(18),SIM,TEMWK2(MAXTRE),X

  !  DATA STATEMENTS

  !     INDIVIDUAL TREE MORTALITY COEFFICIENTS FROM MARGARET PENNER
  !     MB0..MB7;
  !     Source: FVS_Mortality_Report.docx, Table 5
  !     MAPPED BY ITM_MAP (INDIVIDUAL TREE MAP)
  data MB0 /&
       &1.56759,    -1.75438,     -3.67221,    -0.3542,    -0.83917,&
       &-0.22749,    -2.70186,     -1.05177,    -3.27602,   -2.27416,&
       &-3.67336,    -3.89531,     -4.5782,     -6.64434,   -3.39803,&
       &-1.84346,    -4.63558,     -3.60002 /
  data MB1 /&
       &-0.35954,    -0.13439,      0.13553,    -0.29402,   -0.075347,&
       &-0.17368,    -0.15224,      0.18688,    -0.06409,   -0.75413,&
       &-0.015301,   -0.39851,     -0.11127,    -0.60664,   -0.016078,&
       &-0.54492,    -0.10752,      0.035019 /
  data MB2 /&
       &-18.2223,      0.05,         0.0,        -4.41248,   -3.61772,&
       &-2.87478,    -1.77768,     -7.79339,    -2.26039,   -5.44546,&
       &-1.04581,    -0.028481,     2.24759,     0.0,       -1.73506,&
       &0.0,        -0.17508,     -1.79863 /
  data MB3 /&
       &0.004729459, 0.003717782, -0.007633363, 0.007291,   0.00429437,&
       &0.013202,    0.003566943, -0.000771621, 0.003381,   0.030003,&
       &0.000577445, 0.00392894,   0.0,         0.009326974,0.00275289,&
       &0.005057465, 0.002772803,  0.000206976 /
  data MB4 /&
       &-0.73227,    -2.54341,     -2.98566,    -7.32994,   -4.10504,&
       &-5.39914,    -0.53056,     -6.43672,    -1.18764,   -1.28947,&
       &-0.10899,     2.34899,      1.98514,     4.61152,   -1.8512,&
       &2.90385,     0.25158,     -1.3242 /
  data MB5 /&
       &-0.046773,    0.0,          0.029072,    0.04226,    0.018135,&
       &-0.038558,   -0.057377,     0.049045,   -0.030958,   0.031923,&
       &-0.018726,    0.0,          0.0,         0.0,        0.01346,&
       &0.0,         0.016405,     0.0 /
  data MB6 /&
       &0.0,        -1.833,        0.0,         0.5563,     0.0,&
       &0.0,         0.0,          0.0,         0.0,        0.0,&
       &0.0,         0.0,          0.0,         0.0,       -0.2,&
       &0.0,        -0.1917,       0.0 /
  data MB7 /&
       &0.0357,      0.034951,     0.026167,    0.085012,   0.064902,&
       &0.11252,     0.050818,     0.032482,    0.0381,     0.11331,&
       &0.020787,    0.078816,     0.024981,    0.13838,    0.019736,&
       &0.0,         0.055896,     0.018238 /

  !     MAP EACH SPECIES TO ONE OF 18 FITTED EQUATIONS
  !     ITM = INDIVIDUAL TREE MORTALITY
  data ITM_MAP /&
       &5, 4, 3, 4, 1, 7, 8,11, 9, 5,& ! 10
       &9,11, 3, 9,18,18,17,13,13,15,&
       &13,13,13,16,13,12,13,14,18,16,& ! 30
       &16,16,16,16,16,16,15,16,16,17,&
       &17,17,15,16,16,16,13,17,16,13,& ! 50
       &13,13,13,13,16,13,15,15,13,16,&
       &15,15,15,16,17,17,15,16, 6, 2,& ! 70
       &8, 10 /

  ! ADJUST = FINAL SCALAR ADJUSTMENT NEEDED TO SCALE MORTALITY VALUES
  !          TO ACHIEVE THE DESIRED MORTALITY LEVEL
  !  SHORT = TPA SHORT OF REACHING THE DESIRED MORTALITY LEVEL (DUE TO
  !          ALL THE TREE'S PROB BEING ATTRIBUTED TO MORTALITY BEFORE THE
  !          DESIRED STAND LEVEL MORTALITY LEVEL IS REACHED)
  ! TOKILL = NUMBER OF TREES TO KILL THIS CYCLE
  ! SUMKIL = RUNNING TOTAL OF NUMBER OF TREES KILLED SO FAR
  !  PASS1 = NUMBER OF TREES KILLED IN ONE PASS THROUGH THE TREELIST
  !          WITH THE SPECIFIED TREE LEVEL MORTALITY EFFICIENCIES
  !  JPASS = VARIABLE TO TRACK THE NUMBER OF PASSES THROUGH THE LOGIC
  ! TPALFT = DIFFERENCE BETWEEN INITIAL TPA AND MORTALITY TPA

  if (DEBUG)write(JOSTND,20)ICYC,TOKILL
  20 format('0ENTERING VARMRT CYCLE =',I3,' DENSITY RELATED TOKILL = ',&
       &F6.1)

  ! DETERMINE TOTAL NUMBER OF TREES TO KILL IF BACKGROUND MORTALITY
  ! IS STILL IN EFFECT.

  if (TOKILL .eq.0.0) then
     do I = 1,ITRN
        TOKILL = TOKILL+WK2(I)
     end do
     if (DEBUG)write(JOSTND,*)' BACKGROUND TOKILL = ',TOKILL
  end if

  ! INITIALIZE SCALARS AND ARRAYS.

  TEMKIL = TOKILL
  JPASS = 0
  PASS1 = 0.
  SUMKIL = 0.
  do I = 1,ITRN
     WK2(I) = 0.
     TEMWK2(I) = 0.
     EFFTR(I) = 0.
  end do

  ! PASS THROUGH THE TREELIST AND LOAD MORTALITY EFFICIENCY VALUES FOR
  ! EACH TREE RECORD.
  ! CALCULATE HOW MANY TPA WILL BE KILLED IN ONE PASS THROUGH THE TREELIST
  ! WITH EFFICIENCY VALUES SET AT THIS LEVEL.

  ! PENNER INDIVIDUAL MORT MODEL
  ! START-QMD CONSTRAINED TO 1CM. "FOR SPECIES WITH A LARGE, POSITIVE MB4
  ! COEFFICIENT, INCREASING THE MINIMUM WILL LEAD TO LOWER MORTALITY RATES
  ! (HIGHER SURVIVAL) WHEN THE DBHQ IS SMALL.  FOR SPECIES WITH A NEGATIVE
  ! MB4 COEFFICIENT, THE EFFECT IS MINIMAL." (MARGARET PENNER)

  do I = 1,ITRN
     JSPC = ISP(I)
     SIM = SITEAR(JSPC) * FTtoM
     J = ITM_MAP(JSPC)
     DM = DBH(I) * INtoCM
     DQM = MAX(1.0, DQ10 * INtoCM)
     BALM = (1.0 - (PCT(I)/100.)) * BA * FT2pACRtoM2pHA

     X = MB0(J)&
          &+ (MB1(J) * DM)&
          &+ (MB2(J) /(DM+MB6(J)))&
          &+ (MB3(J) * DM**2)&
          &+ (MB4(J) * DM / DQM)&
          &+ (MB5(J) * BALM)&
          &+ (MB7(J) * SIM)
     PEFF = 1.0/(1.0+EXP(MAX(-88.0,MIN(88.0,X)))) ! SURV/YR
     if (DEBUG)write(JOSTND,*)' PEFF,X,DM,DQM,BALM,SIM= ',&
          &PEFF,X,DM,DQM,BALM,SIM
     PEFF = 1.0 - PEFF ! MORT/YR

     if (PEFF .gt.1.0) PEFF = 1.0
     if (PEFF .lt.0.01) PEFF = 0.01
     EFFTR(I) = PEFF
     PASS1 = PASS1 + PROB(I)*EFFTR(I)
  end do
  if (DEBUG)write(JOSTND,30)ITRN,(EFFTR(IG),IG = 1,ITRN)
  30 format(' MORTALITY EFFICIENCY VALUES, ITRN = ',I7,&
       &(/10F10.5))
  if (DEBUG)write(JOSTND,*)' TREES KILLED IN ONE PASS = ',PASS1

  !  CALCULATE THE APPROXIMATE NUMBER OF PASSES NEEDED TO ACHIEVE THE
  !  DESIRED MORTALITY.

  NPASS = IFIX(TOKILL/PASS1)+1
  if (DEBUG)write(JOSTND,*)' APPROXIMATE NUMBER OF PASSES NEEDED = ',&
       &NPASS

  !  LOOP THROUGH THE TREES AND LOAD THE FIRST GUESS AT TREE RECORD
  !  MORTALITY BASED ON THE MORTALITY EFFICIENCY (r) AND APPROXIMATE NUMBER
  !  OF PASSES NEEDED (n). THIS IS A GEOMETRIC PROGRESSION WHERE THE RATE
  !  IS THE MORTALITY EFFICIENCY (r) AND THE STARTING VALUE IS THE INITIAL
  !  PROB VALUE (a). THE Nth TERM IN THE PROGRESSION IS a*r*(1-r)**(n-1)
  !  AND THE SUM OF N TERMS IS -a*((1-r)**n - 1).
  !  ACCUMULATE THE MORTALITY ACHIEVED WITH THIS FIRST PASS.

  100 continue
  JPASS = JPASS+1
  if (JPASS .gt.1)TEMKIL = SHORT
  ISWTCH = 0
  105 continue
  TEMSUM = 0.
  do I = 1,ITRN
     TPALFT = PROB(I)-WK2(I)
     if (TPALFT .gt.0.) then
        OTEM2 = TEMWK2(I)
        TEMWK2(I) = (-TPALFT*((1.0-EFFTR(I))**NPASS - 1.0))
        if (DEBUG)write(JOSTND,*)' I,PROB,WK2,TPALFT,EFFTR,TEMWK2,',&
             &'OTEM2= ',I,PROB(I),WK2(I),TPALFT,EFFTR(I),TEMWK2(I),OTEM2
        TEMSUM = TEMSUM+TEMWK2(I)
     end if
  end do
  if (DEBUG)write(JOSTND,*)' AFTER GUESS ',JPASS,' TEMSUM= ',TEMSUM,&
       &'  TOKILL= ',TOKILL

  ! ADJUST MORTALITY VALUES TO ACHIEVE DESIRED MORTALITY.
  ! IF AN ENTIRE TREE RECORD PROB GETS KILLED, ADJUST PASS1 VALUE FOR
  ! THE NEXT PASS.

  if (NPASS .gt.50) then
     MINSTP = 5
  else if (NPASS .gt.20) then
     MINSTP = 2
  else
     MINSTP = 1
  end if
  ADJUST = TEMKIL/TEMSUM
  if (ADJUST .lt.0.8) then
     if (ISWTCH .eq.2) go to 110
     if (DEBUG)write(JOSTND,*)' TEMKIL,TEMSUM,PASS1,NPASS= ',&
          &TEMKIL,TEMSUM,PASS1,NPASS
     NPASS = NPASS-MAX(MINSTP,IFIX((TEMSUM-TEMKIL)/PASS1))
     if (DEBUG)write(JOSTND,*)' ADJUST= ',ADJUST,'  IS TO SMALL,',&
          &' MIN STEP= ',MINSTP,' NEW NPASS= ',NPASS
     ISWTCH = 1
     if (NPASS .le.0)go to 110
     go to 105
  else if (ADJUST .gt.1.2) then
     if (ISWTCH .eq.1) go to 110
     NPASS = NPASS+MAX(MINSTP,IFIX((TEMKIL-TEMSUM)/PASS1))
     if (DEBUG)write(JOSTND,*)' ADJUST= ',ADJUST,'  IS TO BIG,',&
          &' MIN STEP= ',MINSTP,' NEW NPASS= ',NPASS
     ISWTCH = 2
     go to 105
  end if
  110 continue
  SHORT = 0.
  if (DEBUG)write(JOSTND,*)' TEMKIL= ',TEMKIL,'  TEMSUM= ',&
       &TEMSUM,'  ADJUSTMENT= ',ADJUST
  do I = 1,ITRN
     TPALFT = PROB(I)-WK2(I)
     if (TPALFT .lt.0.00001)go to 150
     XKILL = TEMWK2(I)*ADJUST
     if ((PROB(I)-WK2(I)-XKILL) .le.0.00001) then
        TEMWK2(I) = PROB(I)
        if (DEBUG)write(JOSTND,*)' SHORT,I,XKILL,PROB,WK2= ',&
             &SHORT,I,XKILL,PROB(I),WK2(I)
        SHORT = SHORT+(XKILL-PROB(I)+WK2(I))
        if (DEBUG)write(JOSTND,*)' SHORT= ',SHORT
        PASS1 = PASS1-EFFTR(I)
        go to 140
     end if
     TEMWK2(I) = XKILL
     140 continue
     WK2(I) = WK2(I)+TEMWK2(I)
     SUMKIL = SUMKIL+TEMWK2(I)
     150 continue
  end do
  if (DEBUG)write(JOSTND,23)ITRN,(WK2(IG),IG = 1,ITRN)
  23 format(' ADJUSTED MORTALITY VALUES, ITRN = ',I7,&
       &(/10F10.5))
  if (DEBUG)write(JOSTND,*)' SHORT = ',SHORT
  if (SHORT .gt.0.) then
     NPASS = IFIX(SHORT/PASS1)+1
     if (DEBUG)write(JOSTND,*)' SHORT,PASS1, ADJUSTED PASSES NEEDED',&
          &'= ',SHORT,PASS1,NPASS
     go to 100
  end if

  return
end
