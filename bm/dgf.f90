!========================================!
!   Compute large tree diameter growth   !
!========================================!
! BM $Id$
subroutine DGF(DIAM)
  implicit none

  !  THE SEPERATE EQUATIONS FOR TREES LESS THAN 10" DBH INCLUDED
  !  MANAGED STAND SURVEY (MSS) DATA.  THE EQUATION FOR LODGEPOLE PINE
  !  WAS REFIT TO INCLUDE TERMS FOR CCF AND BA EFFECTS.
  !  ALSO, SPLINE FOR DIAMETER ESTIMATES BASED ON MSS DATA, AND FOR
  !  ESTIMATES BASED ON LARGER TREES NOW COVERS WHOLE RANGE 3-10" DBH,
  !  INSTEAD OF ONLY 7-10".                    DENNIS DONNELLY 11/30/95

  !  Earlier revisions include coefficients for the following species:
  !  WL, DF, GF, LP, ES, AF, & PP.  The coefficients for WP, & OT are
  !  unchanged.  Species #4 is now GF (no WF in model) and Species #6
  !  is now 'blank'.  Species #5 changed from 'WH' to 'MH'.

  !  SEPERATE EQUATIONS ADDED FOR WL,DF,GF,LP,& PP LESS THAN 10" DBH
  !  DIXON 5/16/94. OTHER SPECIES ARE PROXIED

  !  REFIT EQUATIONS FOR TREES LESS THAN 10" DBH LOADED 9/9/99. THESE
  !  WERE REFIT IN 1996 BY RALPH JOHNSON.  DIXON 9/9/99

  !  THIS SUBROUTINE COMPUTES THE VALUE OF DDS (CHANGE IN SQUARED
  !  DIAMETER) FOR EACH TREE RECORD, AND LOADS IT INTO THE ARRAY
  !  WK2.  DDS IS PREDICTED FROM HABITAT TYPE, LOCATION, SLOPE,
  !  ASPECT, ELEVATION, DBH, CROWN RATIO, BASAL AREA IN LARGER TREES,
  !  AND CCF.  THE SET OF TREE DIAMETERS TO BE USED IS PASSED AS THE
  !  ARGUEMENT DIAM.  THE PROGRAM THUS HAS THE FLEXIBILITY TO
  !  PROCESS DIFFERENT CALIBRATION OPTIONS.  THIS ROUTINE IS CALLED
  !  BY **DGDRIV** DURING CALIBRATION AND WHILE CYCLING FOR GROWTH
  !  PREDICTION.  ENTRY **DGCONS** IS CALLED BY **RCON** TO LOAD SITE
  !  DEPENDENT COEFFICIENTS THAT NEED ONLY BE RESOLVED ONCE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CALCOM.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'
  include 'OUTCOM.f90'
  include 'PDEN.f90'
  include 'PLOT.f90'

  !  VARIABLE DEFINITIONS:

  !     DIAM -- ARRAY LOADED WITH TREE DIAMETERS (PASSED AS AN
  !             ARGUEMENT).
  !     DGLD -- ARRAY CONTAINING COEFFICIENTS FOR THE LOG(DIAMETER)
  !             TERM IN THE DDS MODEL (ONE COEFFICIENT FOR EACH
  !             SPECIES).
  !     DGCR -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
  !             RATIO TERM IN THE DDS MODEL (ONE COEFFICIENT FOR
  !             EACH SPECIES).
  !   DGCRSQ -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
  !             RATIO SQUARED TERM IN THE DDS MODEL (ONE
  !             COEFFICIENT FOR EACH SPECIES).
  !   DGDBAL -- ARRAY CONTAINING COEFFICIENTS FOR THE INTERACTION
  !             BETWEEN BASAL AREA IN LARGER TREES AND LN(DBH) (ONE
  !             COEFFICIENT PER SPECIES).

  ! Variables
  logical :: DEBUG

  integer :: I,I1,I2,I3,INDXH,INDXS,IPCCF,ISIC,ISPC,J,LSI

  integer :: IBSERV(5,3),SMMAPH(92,5),SMMAPS(MAXSP)

  real    :: ALBA,ASPDG,ASPTEM,BAL,BARK,BATEM,BRATIO,CCR,CCR2
  real    :: CDBAL,CDSQ,CLBA,CLD,CONSPP,CPCCF,CR,D,DDS,DDSL,DDSS,DF
  real    :: DIAGR,DPP,DSQ,DSQNEW,HOAVH,SI,SMCONS,TEMDG,TEMEL,TSITE
  real    :: XSITE,XWT

  real    :: DGBA(MAXSP),DGBAL(MAXSP),DGCASP(MAXSP),DGCCFA(MAXSP)
  real    :: DGCR(MAXSP),DGCRSQ(MAXSP),DGDBAL(MAXSP),DGDS(MAXSP)
  real    :: DGEL(MAXSP),DGEL2(MAXSP),DGFOR(4,MAXSP),DGLBA(MAXSP)
  real    :: DGLD(MAXSP),DGPCCF(MAXSP),DGSASP(MAXSP),DGSIC(MAXSP)
  real    :: DGSITE(MAXSP),DGSLOP(MAXSP),DGSLSQ(MAXSP),DIAM(MAXTRE)
  real    :: OBSERV(MAXSP),SMCASP(5),SMCR(5),SMCRSQ(5),SMDBAL(5),SMDS(5)
  real    :: SMEL(5),SMEL2(5),SMFOR(4,5),SMHAB(5,5),SMLBA(5),SMLD(5)
  real    :: SMPCCF(5),SMSASP(5),SMSLOP(5)

  !  DATA STATEMENTS:

  !  SPECIES ORDER:
  !   1=WP,  2=WL,  3=DF,  4=GF,  5=MH,  6=WJ,  7=LP,  8=ES,
  !   9=AF, 10=PP, 11=WB, 12=LM, 13=PY, 14=YC, 15=AS, 16=CW,
  !  17=OS, 18=OH

  !  SPECIES EXPANSION:
  !  WJ USES SO JU (ORIGINALLY FROM UT VARIANT; REALLY PP FROM CR VARIANT)
  !  WB USES SO WB (ORIGINALLY FROM TT VARIANT)
  !  LM USES UT LM
  !  PY USES SO PY (ORIGINALLY FROM WC VARIANT)
  !  YC USES WC YC
  !  AS USES SO AS (ORIGINALLY FROM UT VARIANT)
  !  CW USES SO CW (ORIGINALLY FROM WC VARIANT)
  !  OS USES BM PP BARK COEFFICIENT
  !  OH USES SO OH (ORIGINALLY FROM WC VARIANT)

  ! FOR BM ORIGINAL SPECIES: 1-5, 6-10, AND 17:
  ! COEFFICIENTS FOR TREES SMALLER THAN 10 INCHES.
  ! SMHAB IS DIMENSIONED BY (COEFF INDEX, SPECIES INDEX).
  ! SMMAPH IS DIMENSIONED BY (PA INDEX, SPECIES INDEX).

  data SMLD/&
       &1.20856, 1.12948, 1.52803, 1.00488, 1.04225/

  data SMCR/&
       &1.73596, 1.54957, 0.66664, 2.47118, 2.31970/

  data SMCRSQ/&
       &0.00000, 0.00000, 1.20070,-0.99894,-0.43073/

  data SMDBAL/&
       &-0.00066,-0.00223,-0.00199,-0.00358,-0.00105/

  data SMLBA/&
       &-0.24782,-0.15369,-0.13405,-0.24135,-0.24965/

  data SMPCCF/&
       &0.00000,-0.00003,-0.00167,      0., 0.00000/

  data SMFOR/&
       &-0.00991,-0.00991, 0.24298,-0.00991,&
       &0.12927, 0.12927, 0.42841, 0.31221,&
       &1.31341, 1.53206, 1.78409, 1.73754,&
       &-0.21988,-0.21988, 0.22239,-0.47388,&
       &1.61313, 1.75654, 1.90894, 1.75744/

  data SMMAPS/ 1,1,2,3,4,0,4,3,3,5,&
       &0,0,0,0,0,0,5,0/

  data SMHAB/&
       ! WESTERN LARCH
       &0., -0.131277, -0.328134,  0.      ,  0.      ,&
       ! DOUGLAS-FIR
       &0., -0.336855, -1.004248, -0.195972, -0.092403,&
       ! GRAND FIR
       &0., -0.137259,  0.282528,  0.      ,  0.      ,&
       ! LODGEPOLE PINE
       &0.,  0.119324,  0.425094,  0.      ,  0.      ,&
       ! PONDEROSA PINE
       &0.,  0.482619,  0.173487, -0.087731,  0.      /

  data SMMAPH /&
       ! WESTERN LARCH
       &27*0, 2, 0, 2, 31*0, 2, 7*0, 2*1, 5*0, 13*1, 3*0,&
       ! DOUGLAS-FIR
       &1, 1, 8*3, 2*4, 3, 7*0, 2*1, 0, 2*1, 0, 1, 2, 0, 2, 16*3,&
       &2*0, 8*3, 2*0, 3, 0, 3, 2, 24*0, 2*3, 0, 3*3,&
       ! GRAND FIR
       &85*0, 2, 2*1, 2, 3*0,&
       ! LODGEPOLE PINE
       &0, 0, 8*2, 2*0, 2, 17*0, 16*2, 6*0, 2*2, 15*0, 2*1,&
       &5*0, 5*1, 0, 3*1, 4*0, 3*2,&
       ! PONDEROSA PINE
       &30*0, 16*3, 2*0, 8*3, 2*0, 3, 0, 3, 6*0, 2*1, 2*2, 5*1,&
       &4*0, 2, 1, 2*0, 2, 4*0, 3*3/

  data SMDS/&
       &-0.000571,-0.000023,-0.000951,-0.000643,-0.000157/

  data SMCASP/&
       &-0.06358,-0.11174,-0.18548,-0.46361,-0.08695/

  data SMSASP/&
       &0.12754, 0.05022,-0.11202, 0.35696,-0.13976/

  data SMSLOP/&
       &-0.41366,-0.36252,-0.16110, 0.45733,-0.24248/

  data SMEL/&
       &0.00000,-0.00823,-0.09472, 0.00912,-0.07547/

  data SMEL2/&
       &0.00000, 0.00000, 0.00092, 0.00000, 0.00087/

  ! FOR BM ORIGINAL SPECIES: 1-5, 6-10, AND 17:
  !   COEFFICIENTS FOR TREES LARGER THAN 10 INCHES;
  ! FOR ALL OTHER SPECIES, USE FOR ALL TREES.

  data DGLD/&
       &0.77889,  0.41802,  0.57990,  1.01031,    0.8978,      0.0,&
       &0.70429,  1.12805,  0.83642,  0.44675,  0.213947, 0.213947,&
       &0.879338, 0.816880,      0.0, 0.889596,   0.44675, 0.889596/

  data DGCR/&
       &3.36606,  2.15440,  2.13121,  2.56530,    1.2840,      0.0,&
       &3.00236,  3.22770,  1.60755,  1.70901,  1.523464, 1.523464,&
       &1.970052, 2.471226,      0.0, 1.732535,   1.70901, 1.732535/

  data DGCRSQ/&
       &-1.80146, -1.03088, -0.40173, -0.91846,       0.0,      0.0,&
       &-1.24947, -1.13951,      0.0,      0.0,       0.0,      0.0,&
       &0.0,      0.0,      0.0,      0.0,       0.0,      0.0/

  data DGBA/&
       &0.0,      0.0,      0.0,      0.0,       0.0,      0.0,&
       &0.0,      0.0,      0.0,      0.0,       0.0,      0.0,&
       &-0.000173,-0.000147,      0.0,-0.000981,       0.0,-0.000981/

  data DGBAL/&
       &0.0,      0.0,      0.0,      0.0,       0.0,       0.0,&
       &0.0,      0.0,      0.0,      0.0, -0.358634, -0.358634,&
       &0.0,      0.0,      0.0,      0.0,       0.0,       0.0/

  data DGCCFA/&
       &0.0,      0.0,      0.0,      0.0,       0.0,       0.0,&
       &0.0,      0.0,      0.0,      0.0, -0.199592, -0.199592,&
       &0.0,      0.0,      0.0,      0.0,       0.0,       0.0/

  data DGSIC/&
       &0.0,      0.0,      0.0,      0.0,       0.0,       0.0,&
       &0.0,      0.0,      0.0,      0.0,  0.001766,  0.001766,&
       &0.0,      0.0,      0.0,      0.0,       0.0,       0.0/

  data DGSITE/&
       &0.0,  0.47469,  0.76217,  0.58666,      0.0,      0.0,&
       &0.34450,  0.34406,  0.51754,  0.73067,      0.0,      0.0,&
       &0.2528530, 0.244694,      0.0, 0.227307,  0.73067, 0.227307/

  data DGDBAL/&
       &-0.00897, -0.00801, -0.00886, -0.00557,-0.006611,      0.0,&
       &-0.00251, -0.00156, -0.00091, -0.01184,      0.0,      0.0,&
       &-0.004215,-0.005950,      0.0,-0.001265, -0.01184,-0.001265/

  data DGLBA/&
       &0.0,      0.0, -0.06574, -0.15658,      0.0,      0.0,&
       &-0.17037,      0.0, -0.18969, -0.10675,      0.0,      0.0,&
       &0.0,      0.0,      0.0,      0.0, -0.10675,      0.0/

  data DGPCCF/&
       &0.0,      0.0, -0.00034,      0.0,-0.001074,      0.0,&
       &-0.00032, -0.00014, -0.00038, -0.00057,      0.0,      0.0,&
       &0.0,      0.0,      0.0,      0.0, -0.00057,      0.0/

  data  OBSERV/&
       &256., 1209., 3478., 2963., 1000.,   0.0,&
       &1117.,  596.,  599., 6577.,   0.0,   0.0,&
       &475.,    0.,   0.0,  220., 6577.,  220./

  !  IBSERV IS THE NUMBER OF OBSERVATIONS IN THE DIAMETER GROWTH
  !  MODEL BY SPECIES AND SITE CLASS; USED FOR WJ, WB, LM, AND AS

  data  ((IBSERV(I,J),I = 1,5),J = 1,3)/&
       &5*1000,& !WJ
       &27,  70, 123, 101,  33,& !WB & LM
       &184, 429, 356, 162,  74/ !AS

  !  DGFOR CONTAINS LOCATION CLASS CONSTANTS FOR EACH SPECIES.

  data DGFOR/&
       &-0.23185, -0.23185, -0.23185, -0.23185,&
       &-0.56061, -0.56061, -0.56061, -0.56061,&
       &-1.69223, -1.69223, -1.69223, -1.78978,&
       &-1.16884, -1.16884, -1.16884, -1.16884,&
       &-1.6803,  -1.6803,  -1.6803,  -1.6803,&
       &0.0,      0.0,      0.0,      0.0,& !WJ
       &1.59448,  1.59448,  1.59448,  1.49879,&
       &-2.38952, -2.38952, -2.38952, -2.38952,&
       &-0.48027, -0.48027, -0.48027, -0.48027,&
       &0.05217, -0.04456,  0.05217,  0.11197,&
       &1.911884, 1.911884, 1.911884, 1.911884,& !WB USE TT BRIDGER NF
       &1.911884, 1.911884, 1.911884, 1.911884,& !LM
       &-1.310067,-1.310067,-1.310067,-1.310067,& !PY
       &-1.178041,-1.178041,-1.178041,-1.178041,& !YC
       &0.,       0.,       0.,       0.,& !AS
       &-0.107648,-0.107648,-0.107648,-0.107648,& !CW
       &0.05217, -0.04456,  0.05217,  0.11197,&
       &-0.107648,-0.107648,-0.107648,-0.107648/ !OH

  !  DGDS CONTAINS COEFFICIENTS FOR THE DIAMETER SQUARED TERMS
  !  IN THE DIAMETER INCREMENT MODELS; ARRAYED BY FOREST BY
  !  SPECIES.

  data DGDS/&
       &-0.000091,       0.0,-0.000380,-0.000541, -0.000484,       0.0,&
       &0.0, -0.000289, 0.000088,-0.000214,-0.0006538,-0.0006538,&
       &-0.0001323,-0.0002536,      0.0,      0.0, -0.000214,       0.0/

  !  DGEL CONTAINS THE COEFFICIENTS FOR THE ELEVATION TERM IN THE
  !  DIAMETER GROWTH EQUATION.  DGEL2 CONTAINS THE COEFFICIENTS FOR
  !  THE ELEVATION SQUARED TERM IN THE DIAMETER GROWTH EQUATION.
  !  DGSASP CONTAINS THE COEFFICIENTS FOR THE SIN(ASPECT)*SLOPE
  !  TERM IN THE DIAMETER GROWTH EQUATION.  DGCASP CONTAINS THE
  !  COEFFICIENTS FOR THE COS(ASPECT)*SLOPE TERM IN THE DIAMETER
  !  GROWTH EQUATION.  DGSLOP CONTAINS THE COEFFICIENTS FOR THE
  !  SLOPE TERM IN THE DIAMETER GROWTH EQUATION.  DGSLSQ CONTAINS
  !  COEFFICIENTS FOR THE (SLOPE)**2 TERM IN THE DIAMETER GROWTH
  !  MODELS ALL OF THESE ARRAYS ARE SUBSCRIPTED BY SPECIES.

  data DGCASP/&
       &0.12915,      0.0, -0.15167, -0.16504,    0.1794,       0.0,&
       &-0.37870, -0.11989, -0.44759, -0.02280, -0.609774, -0.609774,&
       &0.0,-0.023186,      0.0, 0.085958,  -0.02280,  0.085958/

  data DGSASP/&
       &-0.19278,      0.0, -0.11862, -0.19627,    0.1336,       0.0,&
       &0.09760,  0.35781, -0.27729, -0.12480, -0.017520, -0.017520,&
       &0.0, 0.679903,      0.0, -0.86398,  -0.12480,  -0.86398/

  data DGSLOP/&
       &0.77922,      0.0, -0.28123, -0.67496,    0.0763,       0.0,&
       &0.03990,      0.0,  0.35402, -0.16402, -2.057060, -2.057060,&
       &0.0,      0.0,      0.0,      0.0,  -0.16402,       0.0/

  data DGSLSQ/&
       &-0.93813,      0.0,      0.0,  0.76704,       0.0,       0.0,&
       &0.0,      0.0,      0.0,      0.0,   2.11326,  2.113263,&
       &0.0,      0.0,      0.0,      0.0,       0.0,       0.0/

  data DGEL/&
       &0.00279,      0.0,  0.00371, -0.00633,    0.0852,       0.0,&
       &-0.06908,      0.0, -0.01423, -0.05796,       0.0,       0.0,&
       &0.0,      0.0,      0.0,-0.075986,  -0.05796, -0.075986/

  data DGEL2/&
       &-0.00001,      0.0,      0.0,      0.0,  -0.00094,       0.0,&
       &0.00062,      0.0,      0.0,  0.00060,       0.0,       0.0,&
       &0.0,      0.0,      0.0, 0.001193,   0.00060,  0.001193/

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'DGF',3,ICYC)
  if (DEBUG) write(JOSTND,3)ICYC
  3 format(' ENTERING SUBROUTINE DGF  CYCLE =',I5)

  !  DEBUG OUTPUT: MODEL COEFFICIENTS.

  if (DEBUG) write(JOSTND,*) ' IN DGF,HTCON=',HTCON
  if (DEBUG)&
       &write(JOSTND,*) ' DGCON= ', DGCON
  !     IF(DEBUG)
  !    & WRITE(JOSTND,9000) DGCON,DGDSQ,DGLD,DGCR,DGCRSQ,DGPCCF,DGDBAL
  !9000 FORMAT(/,' DGCON',/12(1X,F9.5),/7(1X,F9.5),/,' DGDSQ',/12(1X,F9.5)
  !    & ,/7(1X,F9.5),/,' DGLD',/12(1X,F9.5),/7(1X,F9.5),/,' DGCR',
  !    & /12(1X,F9.5),/7(1X,F9.5),/,' DGCRSQ',/12(1X,F9.5),/7(1X,F9.5),
  !    & /,' DGPCCF',/12(1X,F9.5),/7(1X,F9.5),/,' DGDBAL',/12(1X,F9.5),
  !    & /7(1X,F9.5))

  !  BEGIN SPECIES LOOP.  ASSIGN VARIABLES WHICH ARE SPECIES
  !  DEPENDENT.

  ALBA = 0.0
  if (BA.gt.0.0)ALBA = ALOG(BA)
  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (I1.eq.0) go to 20
     I2 = ISCT(ISPC,2)
     CONSPP = DGCON(ISPC) + COR(ISPC) + 0.01*DGCCF(ISPC)*RELDEN
     SMCONS = SMCON(ISPC) + COR(ISPC)
     INDXS = SMMAPS(ISPC)
     CLD = DGLD(ISPC)
     CLBA = DGLBA(ISPC)
     CCR = DGCR(ISPC)
     CCR2 = DGCRSQ(ISPC)
     CDSQ = DGDSQ(ISPC)
     CDBAL = DGDBAL(ISPC)
     CPCCF = DGPCCF(ISPC)
     SI = SITEAR(ISPC)
     if (DEBUG)write(JOSTND,9002)ISPC,DGCON(ISPC),COR(ISPC),&
          &DGCCF(ISPC),RELDEN,CONSPP,SMCON(ISPC),SMCONS,INDXS
     9002 format(' IN DGF 9002 ISPC,DGCON,COR,DGCCF,RELDEN,CONSPP,',&
          &'SMCON,SMCONS,INDXS=',I5,7F10.5,I5)

     !  BEGIN TREE LOOP WITHIN SPECIES ISPC.

     do I3 = I1,I2
        I = IND1(I3)
        D = DIAM(I)
        if (D.le.0.0) goto 10

        select case (ISPC)

           !  ORIGINAL BM SPECIES

        case(1:5,7:10,17)
           CR = ICR(I)*0.01
           BAL = (1.0 - (PCT(I)/100.)) * BA
           IPCCF = ITRE(I)
           HOAVH = HT(I)/AVH
           if (HOAVH .gt.1.5)HOAVH = 1.5
           if (IPCCF .lt.100) HOAVH = 1.0
           DDSL = CONSPP + CLD*ALOG(D)&
                &+ CLBA*ALBA + CR*(CCR + CR*CCR2)&
                &+ CDSQ*D*D  + CDBAL*BAL/(ALOG(D+1.0))&
                &+ CPCCF*PCCF(IPCCF)

           !  EXTRA VARIABLES FOR INDIVIDUAL SPECIES

           if (ISPC .eq.1) DDSL = DDSL+ 0.00121 * BAL&
                &+ .00001 * .01 * RMAI * RELDEN&
                &- .0000016 * RELDEN
           if (ISPC .eq.2) DDSL = DDSL- 0.000695 * BA

           !  DEBUG OUTPUT: DDS FOR TREES >= 10-INCHES

           if (DEBUG) write(JOSTND,9010) ISPC, IPCCF,&
                &CONSPP, CLD, CDSQ, D, CLBA, ALBA, CCR, CCR2, CR,&
                &CDBAL, BAL, CPCCF, PCCF(IPCCF), RELDEN
           9010 format(1H0, 'DGF-9010F', 2I5, 14F8.4)

           !  EQUATION FOR TREES < 10" DBH.

           DDSS = 0.
           if (D.lt.10.) then
              DDSS = SMCONS + SMLD(INDXS)*ALOG(D)&
                   &+ SMLBA(INDXS)*ALBA + CR*(SMCR(INDXS) + CR*SMCRSQ(INDXS))&
                   &+ SMDS(INDXS)*D*D  + SMDBAL(INDXS)*BAL/(ALOG(D+1.0))&
                   &+ SMPCCF(INDXS)*PCCF(IPCCF)

              !  DEBUG OUTPUT: DDS FOR TREES < 10-INCHES

              if (DEBUG) write(JOSTND,9020) INDXS, SMCONS,&
                   &SMLD(INDXS), SMDS(INDXS), D,  SMLBA(INDXS), ALBA,&
                   &SMCR(INDXS), SMCRSQ(INDXS), CR,  SMDBAL(INDXS), BAL,&
                   &SMPCCF(INDXS), PCCF(IPCCF), IPCCF
              9020 format(' DGF-9020F', I5, 13F8.4, I5)

              !  EQN WAS FIT TO 5 YEAR DIAMETER GROWTHS, ADJUST TO 10 YEAR ESTIMATE.

              DSQ = EXP(DDSS)
              TEMDG = (SQRT(D*D+DSQ)-D)*2.
              DSQNEW = (TEMDG+D)**2. - D*D
              if (DSQNEW .le.0.) then
                 DDSS = 0.
              else
                 DDSS = ALOG(DSQNEW)
              end if
           end if

           !   SPLINE L & S ESTIMATES IF D AT CYCLE START IS IN RANGE 3-10 IN.
           !   TREES 3-10 IN. SIMULATED BASED ON MSS DATA IN ADDITION TO
           !   ORIGINAL SIMULATION.  COMBINE ESTIMATES OVER DIAM. RANGE 3-10 IN
           !   SO MSS DATA INFLUENCE PREDOMINATES FOR SMALLER TREES IN 3-10.
           !   GRADUALLY INCREASE INFLUENCE OF PREVIOUS ESTIMATES AS TREE SIZE
           !   APPROACHES 10 IN. WITH ONLY PREVIOUS ESTIMATES FROM LARGER TREES
           !   IN FORCE FOR DIAMETERS GREATER THAN 10 IN.   DMD   11/30/95

           XWT = 1.
           if (D.ge.3)XWT = (10.-D)/7.
           if (XWT.lt.0.)XWT = 0.
           DDS = XWT*DDSS + (1.-XWT)*DDSL

           if (DEBUG) write(JOSTND,9025) D, XWT, DDSS, DDSL, DDS
           9025 format(' DGF-9025F--D, XWT, DDSS, DDSL, DDS:', 5F10.5)
           if (DEBUG) write(JOSTND,9030)&
                &I,ISPC,CONSPP,D,BA,CR,BAL,PCCF(IPCCF),RELDEN,HT(I),AVH,SMCONS
           9030 format(' DGF-9030F',2I5,10F11.4)

           !  SPECIES FROM THE UT & TT VARIANTS

        case(6,11,12,15)
           BARK = BRATIO(ISPC,D,HT(I))
           if (ISPC.eq.6) then
              DPP = D
              if (DPP .lt.1.0) DPP = 1.0
              BATEM = BA
              if (BATEM .lt.1.0) BATEM = 1.0
              DF = 0.25897 + 1.03129 * DPP - 0.0002025464 * BATEM&
                   &+ 0.00177 * SI
              if ((DF-DPP) .gt.1.0) DF = DPP + 1.0
              if (DF .lt. DPP) DF = DPP
              DIAGR = (DF - DPP) * BARK
              if (DIAGR .le.0.) then
                 DDS = -9.21
              else
                 DDS = ALOG( (DIAGR * (2.0 * DPP * BARK + DIAGR)) ) + CONSPP
                 if (DDS .lt. -9.21) DDS = -9.21
              end if
              WK2(I) = DDS
              if (DEBUG)write(JOSTND,*)' ISPC,D,DIAGR,DPP,BA,BARK,CONSPP= '
              if (DEBUG)write(JOSTND,*)ISPC,D,DIAGR,DPP,BA,BARK,CONSPP
           else if (ISPC .eq.15) then
              CR = ICR(I)
              call DGFASP(D,ASPDG,CR,BARK,SI,DEBUG)
              DDS = ASPDG + ALOG(COR2(ISPC)) + COR(ISPC)
              WK2(I) = DDS
              if (DEBUG)write(JOSTND,*)' D,ASPDG,CR,BARK,SI= ',D,ASPDG,CR,&
                   &BARK,SI
              if (DEBUG)write(JOSTND,*)' COR2, COR= ',COR2(ISPC),COR(ISPC)
           else
              CR = ICR(I) * 0.01
              BAL = (1.0 - (PCT(I)/100.)) * BA/100.
              DDS = CONSPP + DGLD(ISPC)*ALOG(D) + DGBAL(ISPC)*BAL +&
                   &DGCR(ISPC)*CR + CR*CR*DGCRSQ(ISPC) + DGDSQ(ISPC)*D*D +&
                   &DGPCCF(ISPC)*PCCF(ITRE(I))
              WK2(I) = DDS
           end if

           !  SPECIES FROM THE WC VARIANT

        case(13,14,16,18)
           CR = ICR(I)*0.01
           BAL = (1.0 - (PCT(I)/100.)) * BA
           IPCCF = ITRE(I)
           DDS = CONSPP + DGLD(ISPC)*ALOG(D)&
                &+ CR*(DGCR(ISPC) + CR*DGCRSQ(ISPC))&
                &+ DGDSQ(ISPC)*D*D  + DGDBAL(ISPC)*BAL/(ALOG(D+1.0))
           DDS = DDS + DGPCCF(ISPC)*PCCF(IPCCF)&
                &+ DGLBA(ISPC)*ALOG(BA)&
                &+ DGBAL(ISPC)*BAL + DGBA(ISPC)*BA
           if (DEBUG) write(JOSTND,8000)&
                &I,ISPC,CONSPP,D,BA,CR,BAL,PCCF(IPCCF),RELDEN,HT(I),AVH
           8000 format(1H0,'IN DGF 8000F',2I5,9F11.4)

        end select
        if (DDS.lt.-9.21) DDS = -9.21
        WK2(I) = DDS

        !  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.

        if (DEBUG) then
           write(JOSTND,9040) I,ISPC,DDS
           9040 format(' DGF-9040F, I=',I4,',  ISPC=',I3,',  DDS=LN(DDS)=',F7.4)
        end if
        10 continue
     end do

     !  END OF SPECIES LOOP.

     20 continue
  end do
  if (DEBUG)write(JOSTND,100)ICYC
  100 format(' LEAVING SUBROUTINE DGF  CYCLE =',I5)
  return

  entry DGCONS

  !  ENTRY POINT FOR LOADING COEFFICIENTS OF THE DIAMETER INCREMENT
  !  MODEL THAT ARE SITE SPECIFIC AND NEED ONLY BE RESOLVED ONCE.
  !  IDTYPE IS A HABITAT TYPE INDEX THAT IS COMPUTED IN **RCON**.
  !  ASPECT IS STAND ASPECT.

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'DGF',3,ICYC)

  !  NOTE: FOR UT SPECIES 6(WJ), 12(LM), 15(AS) AND TT 11(WB) 0.7854 IS
  !        45 DEGREES IN RADIANS. THIS CORRECTION WAS
  !        PROBABLY DUE TO A PROBLEM IN THE WAY ASPECT WAS RECORDED
  !        IN THE DATA SET USED TO FIT THE MODEL.  GED 12-20-94.
  !  PUT SITE INTO ONE OF 5 CLASSES FOR UT SPECIES 6(WJ), 11(WB) AND 15(AS)

  XSITE = SITEAR(ISISP)
  LSI = INT(XSITE/10.)
  if (LSI.lt.2)ISIC = 1
  if (LSI.ge.5)ISIC = 5
  if (LSI.ge.3.and. LSI.lt.4)ISIC = 3
  if (LSI.ge.4.and. LSI.lt.5)ISIC = 4
  if (LSI.ge.2.and. LSI.lt.3)ISIC = 2

  !  ENTER LOOP TO LOAD SPECIES DEPENDENT VECTORS.

  do ISPC = 1,MAXSP
     DGCCF(ISPC) = DGCCFA(ISPC)

     !  COMPUTE CONSTANT TERM: LARGE TREES >= 10 INCHES.

     if (ISPC.eq.6.or. ISPC.eq.11.or. ISPC.eq.12.or. ISPC.eq.15) then
        ASPTEM = ASPECT-0.7854
     else
        ASPTEM = ASPECT
     end if
     TSITE = SITEAR(ISPC)
     if (ISPC.eq.7) then
        TSITE = -43.78 + 2.16 * TSITE
        if (TSITE.le.1.) TSITE = 1.
     end if
     if (ISPC.eq.5) TSITE = TSITE * 3.28
     TEMEL = ELEV
     if (ISPC.eq.16.or. ISPC.eq.18) then
        if (TEMEL .gt.30.)TEMEL = 30.
     end if
     DGCON(ISPC) = DGSIC(ISPC)*XSITE&
          &+ DGFOR(IFOR,ISPC)&
          &+ DGEL(ISPC) * TEMEL&
          &+ DGEL2(ISPC) * TEMEL * TEMEL&
          &+(DGSASP(ISPC) * SIN(ASPTEM)&
          &+ DGCASP(ISPC) * COS(ASPTEM)&
          &+ DGSLOP(ISPC)) * SLOPE&
          &+ DGSLSQ(ISPC) * SLOPE * SLOPE&
          &+ DGSITE(ISPC)*ALOG(TSITE)
     if (DEBUG) write(JOSTND,9050) IFOR, ISPC,    DGCON(ISPC),&
          &DGFOR(IFOR,ISPC), DGEL(ISPC), DGEL2(ISPC), TEMEL,&
          &DGSASP(ISPC), DGCASP(ISPC), ASPTEM,&
          &DGSLOP(ISPC), DGSLSQ(ISPC), SLOPE,&
          &DGSITE(ISPC), SITEAR(ISPC), TSITE
     9050 format(' DGF-9050F', 2I5, 14F8.4)

     !  COMPUTE CONSTANT TERM:  LARGE TREES < 10 IN.

     select case (ISPC)
     case (1:5,7:10,17)
        INDXS = SMMAPS(ISPC)
        INDXH = SMMAPH(ICL5,INDXS) + 1
        SMCON(ISPC) = SMHAB(INDXH,INDXS)&
             &+ SMFOR(IFOR,INDXS)&
             &+ SMEL(INDXS) * ELEV&
             &+ SMEL2(INDXS) * ELEV * ELEV&
             &+(SMSASP(INDXS) * SIN(ASPTEM)&
             &+ SMCASP(INDXS) * COS(ASPTEM)&
             &+ SMSLOP(INDXS)) * SLOPE
     case (6,11:16,18)
        SMCON(ISPC) = 0.0
     end select

     select case (ISPC)
     case (6)
        ATTEN(ISPC) = IBSERV(ISIC,1)
     case (11,12)
        ATTEN(ISPC) = IBSERV(ISIC,2)
     case (15)
        ATTEN(ISPC) = IBSERV(ISIC,3)
     case default
        ATTEN(ISPC) = OBSERV(ISPC)
     end select

     DGDSQ(ISPC) = DGDS(ISPC)
     if (DEBUG) write(JOSTND,9060) IFOR, ISPC, INDXS, ICL5, INDXH,&
          &SMCON(ISPC), SMHAB(INDXH,INDXS), SMFOR(IFOR,INDXS),&
          &SMEL(INDXS), SMEL2(INDXS), ELEV,&
          &SMSASP(INDXS), SMCASP(INDXS), ASPTEM,&
          &SMSLOP(INDXS), SLOPE,&
          &DGDSQ(ISPC), ATTEN(ISPC)
     9060 format(' DGF-9060F', 5I5, /' ', 12F8.4,F8.0)

     !  IF READCORD OR REUSCORD WAS SPECIFIED (LDCOR2 IS TRUE) ADD
     !  LN(COR2) TO THE BAI MODEL CONSTANT TERM (DGCON).  COR2 IS
     !  INITIALIZED TO 1.0 IN BLKDATA.

     if (LDCOR2.and.COR2(ISPC).gt.0.0) then
        DGCON(ISPC) = DGCON(ISPC)+ALOG(COR2(ISPC))

        select case (ISPC)
        case (1:5,7:10,17)
           SMCON(ISPC) = SMCON(ISPC)+ALOG(COR2(ISPC))
        end select

     end if
     30 continue
  end do
  return
end
