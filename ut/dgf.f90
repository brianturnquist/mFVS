!========================================!
!   Compute large tree diameter growth   !
!========================================!
! UT $Id$
subroutine DGF(DIAM)
  implicit none

  !  THIS SUBROUTINE COMPUTES THE VALUE OF DDS (CHANGE IN SQUARED
  !  DIAMETER) FOR EACH TREE RECORD, AND LOADS IT INTO THE ARRAY
  !  WK2.  THE SET OF TREE DIAMETERS TO BE USED IS PASSED AS THE
  !  ARGUEMENT DIAM.  THIS GIVES THE PROGRAM THE FLEXIBILITY TO
  !  COMPUTE THE GROWTHS NEEDED TO PROCESS THE DIFFERENT
  !  CALIBRATION OPTIONS.
  !   THIS ROUTINE IS CALLED
  !  BY **DGDRIV** DURING CALIBRATION AND WHILE CYCLING FOR GROWTH
  !  PREDICTION.  ENTRY **DGCONS** IS CALLED BY **RCON** TO LOAD SITE
  !  DEPENDENT COEFFICIENTS THAT NEED ONLY BE RESOLVED ONCE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CALCOM.f90'
  include 'ARRAYS.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'
  include 'OUTCOM.f90'
  include 'PLOT.f90'
  include 'PDEN.f90'
  include 'VARCOM.f90'
  include 'GGCOM.f90'

  !  DIMENSIONS FOR INTERNAL VARIABLES.

  !     DIAM -- ARRAY LOADED WITH TREE DIAMETERS (PASSED AS AN
  !             ARGUEMENT).
  !     DGLD -- ARRAY CONTAINING COEFFICIENTS FOR THE LOG(DIAMETER)
  !             TERM IN THE DDS MODEL (ONE COEFFICIENT FOR EACH
  !             SPECIES).
  !    DGBAL -- ARRAY CONTAINING COEFFICIENTS FOR THE BASAL AREA IN
  !             LARGER TREES TERM IN THE DDS MODEL
  !             (ONE COEFFICIENT FOR EACH SPECIES).
  !    DGCCF -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
  !             COMPETITION FACTOR TERM IN THE DDS MODEL (ONE
  !             COEFFICIENT FOR EACH SPECIES).
  !  DGCCFM -- ARRAY CONTAINING THE MAPFOR CCF SITE SPECIES COEFFICIENTS
  !            (ONE FOR EACH SPECIES)
  !     DGCR -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
  !             RATIO TERM IN THE DDS MODEL (ONE COEFFICIENT FOR
  !             EACH SPECIES).
  !   DGCRSQ -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
  !             RATIO SQUARED TERM IN THE DDS MODEL (ONE
  !             COEFFICIENT FOR EACH SPECIES).

  ! Variables
  logical :: DEBUG
  integer :: IBSERV(5,MAXSP)
  real    :: DGFOR(5,MAXSP),DGDS(4,MAXSP),TEMEL,YOUNG,OLD,SUMD2,SUMTRE
  real    :: DGSIC(5,MAXSP),DGQMD,STDSDI,RELSDI,ALD,DGLCCF,DGDBLS
  integer :: IDGSIM(7,MAXSP),ICLS,II
  real    :: DGSASP(MAXSP),DGCASP(MAXSP),DGSLOP(MAXSP),DGEL2(MAXSP)
  real    :: DGSLSQ(MAXSP),DGCCFA(MAXSP),OBSERV(MAXSP),DGEL(MAXSP)
  integer :: MAPLOC(6,MAXSP),MAPDSQ(6,MAXSP),ISMAP(MAXSP)
  real    :: DIAM(MAXTRE),DGLD(MAXSP),DGBAL(MAXSP),DGCR(MAXSP)
  real    :: DGCRSQ(MAXSP),DGPCCF(MAXSP),DGBA(MAXSP),DGDBAL(MAXSP)
  real    :: BA100,CONSPP,DGLDS,DGBALS,DGCRS,DGCRS2,DGDSQS,DGPTCC
  real    :: SI,D,BARK,BRATIO,DPP,BATEM,DF,DIAGR,DDS,CR,ASPDG,BAL
  real    :: XSITE,BAUTBA,ASPTEM,DPROB
  integer :: ISI,ISC,I1,I2,ISPC,I,LSI,ISIC,I3

  ! SPECIES ORDER FOR UTAH VARIANT:

  !  1=WB,  2=LM,  3=DF,  4=WF,  5=BS,  6=AS,  7=LP,  8=ES,  9=AF, 10=PP,
  ! 11=PI, 12=WJ, 13=GO, 14=PM, 15=RM, 16=UJ, 17=GB, 18=NC, 19=FC, 20=MC,
  ! 21=BI, 22=BE, 23=OS, 24=OH

  ! VARIANT EXPANSION:
  ! GO AND OH USE OA (OAK SP.) EQUATIONS FROM UT
  ! PM USES PI (COMMON PINYON) EQUATIONS FROM UT
  ! RM AND UJ USE WJ (WESTERN JUNIPER) EQUATIONS FROM UT
  ! GB USES BC (BRISTLECONE PINE) EQUATIONS FROM CR
  ! NC, FC, AND BE USE NC (NARROWLEAF COTTONWOOD) EQUATIONS FROM CR
  ! MC USES MC (CURL-LEAF MTN-MAHOGANY) EQUATIONS FROM SO
  ! BI USES BM (BIGLEAF MAPLE) EQUATIONS FROM SO
  ! OS USES OT (OTHER SP.) EQUATIONS FROM UT

  data DGLD/&
       &0.213947,  0.213947,  0.479631,  0.827167, 0.587579,&
       &0.0,  0.587503,  0.587579,  0.833096, 0.733305,&
       &0.0,       0.0,       0.0,       0.0,      0.0,&
       &0.0,       0.0,       0.0,       0.0, 0.889596,&
       &1.024186,       0.0,  0.213947,       0.0/

  data DGCR/&
       &1.523464,  1.523464,  3.182592, -0.207507, 0.331129,&
       &0.0,  2.148640,  0.331129,  1.422919, 1.315804,&
       &0.0,       0.0,       0.0,       0.0,      0.0,&
       &0.0,       0.0,       0.0,       0.0, 1.732535,&
       &0.459387,       0.0,  1.523464,       0.0/

  data DGCRSQ/&
       &0.0,       0.0, -1.310144,  1.578941, 0.816301,&
       &0.0, -0.598897,  0.816301,  0.225676, 0.238917,&
       &0.0,       0.0,       0.0,       0.0,      0.0,&
       &0.0,       0.0,       0.0,       0.0,      0.0,&
       &0.0,       0.0,       0.0,       0.0/

  data DGPCCF/&
       &0.0,       0.0, -0.001613, -0.000428,      0.0,&
       &0.0,  -.000467,       0.0,  -.000200, -.002576,&
       &0.0,       0.0,       0.0,       0.0,      0.0,&
       &0.0,       0.0,       0.0,       0.0,      0.0,&
       &-0.000757,       0.0,       0.0,       0.0/

  data DGBA/&
       &0.0,       0.0,       0.0,       0.0,      0.0,&
       &0.0,       0.0,       0.0,       0.0,      0.0,&
       &0.0,       0.0,       0.0,       0.0,      0.0,&
       &0.0,       0.0,       0.0,       0.0,-0.000981,&
       &0.0,       0.0,       0.0,       0.0/

  data DGBAL/&
       &-0.358634, -0.358634, -0.707380, -0.010478, -0.399357,&
       &0.0, -0.192073, -0.399357, -0.182808, -0.320124,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0, -0.358634,       0.0/

  data DGDBAL/&
       &0.0,       0.0,       0.0,       0.0,      0.0,&
       &0.0,       0.0,       0.0,       0.0,      0.0,&
       &0.0,       0.0,       0.0,       0.0,      0.0,&
       &0.0,       0.0,       0.0,       0.0,-0.001265,&
       &-0.010222,       0.0,       0.0,       0.0/

  data ISMAP/ 1, 1, 2, 6, 4, 5, 1, 4, 3, 7,&
       &1, 1, 1, 1, 1, 1, 1, 1, 1, 1,&
       &1, 1, 7, 1/

  !  IBSERV IS THE NUMBER OF OBSERVATIONS IN THE DIAMETER GROWTH
  !  MODEL BY SPECIES AND SITE CLASS.

  data  IBSERV/&
       &27,70,123,101,33,&
       &27,70,123,101,33,&
       &202,541,323,103,42,&
       &283,45,49,24,12,&
       &586,544,272,95,32,&
       &184,429,356,162,74,&
       &865,1023,372,10,1,&
       &586,544,272,95,32,&
       &203,432,397,126,84,&
       &289,241,41,24,5,&
       &5*1000,&
       &5*1000,&
       &5*1000,&
       &5*1000,&
       &5*1000,&
       &5*1000,&
       &5*1000,&
       &5*1000,&
       &5*1000,&
       &5*1000,&
       &5*1000,&
       &5*1000,&
       &27,70,123,101,33,&
       &5*1000/

  data OBSERV/&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,     220.0,&
       &78.0,       0.0,       0.0,       0.0/

  !  DGCCFA CONTAINS COEFFICIENTS FOR THE CCF TERM BY SPECIES BY
  !  HABITAT CLASS.

  data DGCCFA/&
       &-0.199592, -0.199592,       0.0, -0.098821, -0.043414,&
       &0.0, -0.407523, -0.043414,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0, -0.199592,       0.0/

  !  MAPLOC IS A VECTOR WHICH MAPS FOREST ONTO A LOCATION CLASS.

  data MAPLOC/&
       &6*1,&
       &6*1,&
       &1,2,2,2,3,4,&
       &1,2,3,3,4,4,&
       &1,1,1,2,3,4,&
       &1,1,1,1,1,1,&
       &1,1,1,1,2,3,&
       &1,1,1,2,3,4,&
       &1,2,1,1,1,3,&
       &1,2,2,3,3,3,&
       &6*1,&
       &6*1,&
       &6*1,&
       &6*1,&
       &6*1,&
       &6*1,&
       &6*1,&
       &6*1,&
       &6*1,&
       &6*1,&
       &6*1,&
       &6*1,&
       &6*1,&
       &6*1/

  !  DGFOR CONTAINS LOCATION CLASS CONSTANTS FOR EACH SPECIES.

  data DGFOR/&
       &1.911884,       0.0,       0.0,      0.0,      0.0,&
       &1.911884,       0.0,       0.0,      0.0,      0.0,&
       &0.192136, -0.064516,  0.477698, 0.589169,      0.0,&
       &-0.061856, -0.130667, -0.314746, 0.421806,      0.0,&
       &0.011943,  0.265071, -0.094861, 0.796948,      0.0,&
       &0.0,       0.0,       0.0,      0.0,      0.0,&
       &-0.256987, -0.425846,  0.530457,      0.0,      0.0,&
       &0.011943,  0.265071, -0.094861, 0.796948,      0.0,&
       &-0.467188, -0.638653,  0.116430,      0.0,      0.0,&
       &-0.13235, -0.460129, -0.302309,      0.0,      0.0,&
       &0.0,       0.0,       0.0,      0.0,      0.0,&
       &0.0,       0.0,       0.0,      0.0,      0.0,&
       &0.0,       0.0,       0.0,      0.0,      0.0,&
       &0.0,       0.0,       0.0,      0.0,      0.0,&
       &0.0,       0.0,       0.0,      0.0,      0.0,&
       &0.0,       0.0,       0.0,      0.0,      0.0,&
       &0.0,       0.0,       0.0,      0.0,      0.0,&
       &0.0,       0.0,       0.0,      0.0,      0.0,&
       &0.0,       0.0,       0.0,      0.0,      0.0,&
       &-0.107648,       0.0,       0.0,      0.0,      0.0,&
       &-7.753469,       0.0,       0.0,      0.0,      0.0,&
       &0.0,       0.0,       0.0,      0.0,      0.0,&
       &1.911884,       0.0,       0.0,      0.0,      0.0,&
       &0.0,       0.0,       0.0,      0.0,      0.0/

  !  MAPDSQ IS A VECTOR WHICH MAPS FOREST ONTO A DBH**2
  !  COEFFICIENT.

  data MAPDSQ/&
       &1,1,1,1,1,1,&
       &1,1,1,1,1,1,&
       &1,1,1,1,1,1,&
       &1,1,1,1,1,1,&
       &1,1,1,1,1,1,&
       &1,1,1,1,1,1,&
       &1,1,1,1,1,1,&
       &1,1,1,1,1,1,&
       &1,1,1,1,1,1,&
       &1,2,1,1,1,1,&
       &1,1,1,1,1,1,&
       &1,1,1,1,1,1,&
       &1,1,1,1,1,1,&
       &1,1,1,1,1,1,&
       &1,1,1,1,1,1,&
       &1,1,1,1,1,1,&
       &1,1,1,1,1,1,&
       &1,1,1,1,1,1,&
       &1,1,1,1,1,1,&
       &1,1,1,1,1,1,&
       &1,1,1,1,1,1,&
       &1,1,1,1,1,1,&
       &1,1,1,1,1,1,&
       &1,1,1,1,1,1/

  !  DGDS CONTAINS COEFFICIENTS FOR THE DIAMETER SQUARED TERMS
  !  IN THE DIAMETER INCREMENT MODELS; ARRAYED BY FOREST BY
  !  SPECIES.
  !  ORIGINAL EQN FOR SPECIES 1,2,14 HAD + TERM. CHANGED TO - TO
  !  AVOID RUNAWAY DIAMETER GROWTH.  GD. 1/11/00

  data DGDS/&
       &-0.0006538,        0.0,        0.0,        0.0,&
       &-0.0006538,        0.0,        0.0,        0.0,&
       &0.0,        0.0,        0.0,        0.0,&
       &-0.000018,        0.0,        0.0,        0.0,&
       &0.0,        0.0,        0.0,        0.0,&
       &0.0,        0.0,        0.0,        0.0,&
       &0.0,        0.0,        0.0,        0.0,&
       &0.0,        0.0,        0.0,        0.0,&
       &-0.0001672,        0.0,        0.0,        0.0,&
       &-0.0005345, -0.0006363,        0.0,        0.0,&
       &0.0,        0.0,        0.0,        0.0,&
       &0.0,        0.0,        0.0,        0.0,&
       &0.0,        0.0,        0.0,        0.0,&
       &0.0,        0.0,        0.0,        0.0,&
       &0.0,        0.0,        0.0,        0.0,&
       &0.0,        0.0,        0.0,        0.0,&
       &0.0,        0.0,        0.0,        0.0,&
       &0.0,        0.0,        0.0,        0.0,&
       &0.0,        0.0,        0.0,        0.0,&
       &0.0,        0.0,        0.0,        0.0,&
       &-0.0001737,        0.0,        0.0,        0.0,&
       &0.0,        0.0,        0.0,        0.0,&
       &-0.0006538,        0.0,        0.0,        0.0,&
       &0.0,        0.0,        0.0,        0.0/

  !  DGSASP CONTAINS THE COEFFICIENTS FOR THE SIN(ASPECT)*SLOPE
  !  TERM IN THE DIAMETER GROWTH EQUATION.  DGCASP CONTAINS THE
  !  COEFFICIENTS FOR THE COS(ASPECT)*SLOPE TERM IN THE DIAMETER
  !  GROWTH EQUATION.  DGSLOP CONTAINS THE COEFFICIENTS FOR THE
  !  SLOPE TERM IN THE DIAMETER GROWTH EQUATION.  DGSLSQ CONTAINS
  !  COEFFICIENTS FOR THE (SLOPE)**2 TERM IN THE DIAMETER GROWTH MODELS.
  !  ALL OF THESE ARRAYS ARE SUBSCRIPTED BY SPECIES.

  data DGCASP /&
       &-0.609774, -0.609774,  0.015235,  0.012704, -0.198194,&
       &0.0, -0.168522, -0.198194, -0.232267, -0.411292,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,  0.085958,&
       &0.0,       0.0, -0.609774,       0.0/
  data DGSASP/&
       &-0.017520, -0.017520,  0.022753, -0.082731, -0.122483,&
       &0.0,  0.128610, -0.122483, -0.192975, -0.287654,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,  -0.86398,&
       &0.0,       0.0, -0.017520,       0.0/
  data DGSLOP/&
       &-2.057060, -2.057060, -0.532905, -1.133123,  0.240433,&
       &0.0,  0.120589,  0.240433,  0.383578,  0.016965,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0, -2.057060,       0.0/
  data DGSLSQ/&
       &2.113263,  2.113263, -0.086518,  1.931351,       0.0,&
       &0.0,  -.266226,       0.0, -0.333955,  2.282665,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,  2.113263,       0.0/
  data DGEL/&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0, -0.075986,&
       &-0.012111,       0.0,       0.0,       0.0/
  data DGEL2/&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,  0.001193,&
       &0.0,       0.0,       0.0,       0.0/

  ! SITE BASE SPECIES REFERENCE
  ! 1-LP, 2-DF, 3-SAF, 4-ES, 5-ASP, 6-WF, 7-OTH,PP

  data IDGSIM/&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,2,1,2,1,1,1,&
       &1,2,1,1,1,1,2,&
       &1,2,2,2,2,2,2,&
       &1,1,1,1,1,1,1,&
       &1,1,1,2,2,2,2,&
       &1,2,2,2,2,2,2,&
       &1,1,2,2,3,2,2,&
       &1,1,1,1,1,2,3,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1/

  data DGSIC /&
       &0.001766,      0.0,       0.0,      0.0,      0.0,&
       &0.001766,      0.0,       0.0,      0.0,      0.0,&
       &0.010968, 0.006827,  0.199457,      0.0,      0.0,&
       &0.017663, 0.005327,       0.0,      0.0,      0.0,&
       &0.015133, 0.021085,       0.0,      0.0,      0.0,&
       &0.006460,      0.0,       0.0,      0.0,      0.0,&
       &0.021764, 0.027956,       0.0,      0.0,      0.0,&
       &0.015133, 0.021085,       0.0,      0.0,      0.0,&
       &0.004468, 0.008147, -0.015283,      0.0,      0.0,&
       &0.019282, 0.049804,   0.02943,      0.0,      0.0,&
       &0.0,      0.0,       0.0,      0.0,      0.0,&
       &0.0,      0.0,       0.0,      0.0,      0.0,&
       &0.0,      0.0,       0.0,      0.0,      0.0,&
       &0.0,      0.0,       0.0,      0.0,      0.0,&
       &0.0,      0.0,       0.0,      0.0,      0.0,&
       &0.0,      0.0,       0.0,      0.0,      0.0,&
       &0.0,      0.0,       0.0,      0.0,      0.0,&
       &0.0,      0.0,       0.0,      0.0,      0.0,&
       &0.0,      0.0,       0.0,      0.0,      0.0,&
       &0.227307,      0.0,       0.0,      0.0,      0.0,&
       &1.965888,      0.0,       0.0,      0.0,      0.0,&
       &0.0,      0.0,       0.0,      0.0,      0.0,&
       &0.001766,      0.0,       0.0,      0.0,      0.0,&
       &0.0,      0.0,       0.0,      0.0,      0.0/

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'DGF',3,ICYC)
  if (DEBUG) write(JOSTND,3)ICYC
  3 format(' ENTERING SUBROUTINE DGF  CYCLE =',I5)
  if (DEBUG)write(JOSTND,*) ' DGCON= ', DGCON

  !  SPECIES USING SURROGATE EQUATIONS FROM THE CR VARIANT HAVE SPECIAL
  !  NEEDS; COMPUTE THOSE HERE.

  AGERNG = 1000.
  YOUNG = 1000.
  OLD = 0.0

  ! CALL ARANGE AND BADIST

  call BADIST(DEBUG)

  ! FIND AGE DISTRIBUTION

  do I = 1,ITRN
     if (ABIRTH(I).le.1.)go to 5
     if (HT(I) .le.4.5) go to 5
     if (ABIRTH(I) .lt. YOUNG) YOUNG = ABIRTH(I)
     if (ABIRTH(I) .gt. OLD) OLD = ABIRTH(I)
     5 continue

     ! COMPUTE AGE RANGE.

     AGERNG = OLD - YOUNG
     AGERNG = ABS(AGERNG)
  end do

  ! COMPUTE STAGNATION MULTIPLIER DSTAG. DSTAG INITIALIZED IN GRINIT.
  ! IF THIS PASS THROUGH **DGF** IS DURING CALIBRATION (LSTART = .TRUE.)
  ! INCLUDE RECENT MORTALITY TREES (IMC(I)=7) IN THE CALCULATION.

  DSTAG = 1.0
  SUMD2 = 0.
  SUMTRE = 0.
  DGQMD = 0.
  if (ITRN .eq.0) go to 1001
  do II = 1,ITRN
     I = IND1(II)
     if (I.ge.IREC2) go to 1000
     SUMD2 = SUMD2+DBH(I)*DBH(I)*PROB(I)
     SUMTRE = SUMTRE+PROB(I)
     if (DEBUG)write(JOSTND,*)' DGF II,I,DBH,PROB,SUMD2,',&
          &'SUMTRE= ',II,I,DBH(I),PROB(I),SUMD2,SUMTRE
     1000 continue
  end do

  ! ADD IN RECENT MORTALITY TREES IF CALIBRATING

  if (LSTART) then
     do II = IREC2,MAXTRE
        ISPC = ISP(II)
        DPROB = PROB(II)/(FINT/FINTM)
        if (DEBUG)write(JOSTND,*)' II,ISPC,IMC,DBH,DPROB= ',&
             &II,ISPC,IMC(II),DBH(II),DPROB
        if (IMC(II) .eq.7) then
           SUMD2 = SUMD2+DBH(II)*DBH(II)*DPROB
           SUMTRE = SUMTRE+DPROB
           if (DEBUG)write(JOSTND,*)' DGF II,ISPC,DBH,DPROB,SUMD2,',&
                &'SUMTRE= ',II,ISPC,DBH(II),DPROB,SUMD2,SUMTRE
        end if
     end do
  end if

  if (DEBUG)write(JOSTND,*)' SUMD2,SUMTRE= ',SUMD2,SUMTRE
  DGQMD = SQRT(SUMD2/SUMTRE)
  1001 continue
  if (ICYC.gt.0.and. BA.gt.0.0) then
     if (DGQMD.gt.0.) then
        STDSDI = SUMTRE*(DGQMD/10.0)**1.605
     else
        STDSDI = 0.
     end if
     RELSDI = 0.
     call SDICAL(1,SDIMAX)
     if (SDIMAX.gt.0.)RELSDI = STDSDI/SDIMAX
     if (DEBUG)write(JOSTND,*)' STDSDI,SDIMAX,RELSDI= ',&
          &STDSDI,SDIMAX,RELSDI
     if (RELSDI .gt.0.85) RELSDI = 0.85
     if (RELSDI .gt.0.7) DSTAG = 3.33333 * (1.0 - RELSDI)
     if (DEBUG)write(JOSTND,*)' STAGNATION EFFECT= ',DSTAG
     if (DEBUG)write(JOSTND,*)' STAGNATION FLAGS = ',ISTAGF
  end if

  !  END OF SPECIAL NEEDS SECTION

  !  SCALE BASAL AREA.

  BA100 = BA/100.
  ISI = NINT(SITEAR(ISISP))
  if (ISI.gt.59)ISI = 60
  if (ISI.lt.20)ISI = 20
  ISC = INT(real(ISI)/10.)

  ! PUT THE SITE IN 10'S INTO A CLASS 1-5

  ISC = ISC-1

  !  BEGIN SPECIES LOOP.  ASSIGN VARIABLES WHICH ARE SPECIES DEPENDENT

  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (I1.eq.0) go to 20
     I2 = ISCT(ISPC,2)
     if (ISC.ge.4.and. ISPC.eq.8) DGCCF(8) = -0.154870
     CONSPP = DGCON(ISPC) + COR(ISPC) +0.01*DGCCF(ISPC)*RELDEN

     if (DEBUG)write(JOSTND,9001)ISPC,DGCON(ISPC),COR(ISPC),&
          &DGCCF(ISPC),RELDEN,CONSPP
     9001 format(' IN DGF 9001 ISPC,DGCON,COR,DGCCF,RELDEN,CONSPP=',&
          &I5,5F10.5)
     DGLDS = DGLD(ISPC)
     DGBALS = DGBAL(ISPC)
     DGCRS = DGCR(ISPC)
     DGCRS2 = DGCRSQ(ISPC)
     DGDSQS = DGDSQ(ISPC)
     DGPTCC = DGPCCF(ISPC)
     DGDBLS = DGDBAL(ISPC)
     SI = SITEAR(ISPC)

     if (DEBUG)write(JOSTND,9002)ISPC,CONSPP,DGLDS,DGBALS,&
          &DGCRS,DGCRS2,DGDSQS,ISC,DGPTCC,DGCCF(ISPC),DGPTCC
     9002 format(' DGF ISPC',I2,1X,6(E12.4,1X),I5,3E10.4)

     !  BEGIN TREE LOOP WITHIN SPECIES ISPC.

     do I3 = I1,I2
        BAL = 0.0
        CR = 0.0
        I = IND1(I3)
        D = DIAM(I)
        BARK = BRATIO(ISPC,D,HT(I))
        if (D .le.0.0) go to 10

        select case (ISPC)

        case(11:16,24)
           DPP = D
           if (DPP .lt.1.0) DPP = 1.0
           BATEM = BA
           if (BATEM .lt.1.0) BATEM = 1.0
           DF = 0.25897 + 1.03129 * DPP - 0.0002025464 * BATEM&
                &+ 0.00177 * SI
           if ((DF-DPP) .gt.1.0) DF = DPP + 1.0
           !CCC        IF(DF .GT. DBHMAX(IS)) DF = DBHMAX(IS)
           if (DF .lt. DPP) DF = DPP
           DIAGR = (DF - DPP) * BARK
           if (DIAGR .le.0.) then
              DDS = -9.21
           else
              DDS = ALOG( (DIAGR * (2.0 * DPP * BARK + DIAGR)) ) + CONSPP
              if (DDS .lt. -9.21) DDS = -9.21
           end if

        case(6)
           CR = ICR(I)
           call DGFASP(D,ASPDG,CR,BARK,SI,DEBUG)
           DDS = ASPDG + ALOG(COR2(ISPC)) + COR(ISPC)

        case(1:5,7:10,23)
           CR = ICR(I) * 0.01
           BAL = (1.0 - (PCT(I)/100.)) * BA100
           DDS = CONSPP + DGLDS*ALOG(D) + DGBALS*BAL + CR*DGCRS+CR*CR*DGCRS2&
                &+DGDSQS*D*D + DGPTCC*PCCF(ITRE(I))
           if (ISPC.eq.5)DDS = DDS*0.95

        case(20:21)
           ALD = ALOG(D)
           CR = ICR(I)*0.01
           BAL = (1.0 - (PCT(I)/100.)) * BA
           if (DEBUG)write(JOSTND,*)' BAL,BA,PCT(I)= ',BAL,BA,PCT(I)
           DGLCCF = 0.0
           DDS = CONSPP +DGLDS*ALD + DGBALS*BAL + CR*(DGCRS+CR*DGCRS2)&
                &+DGDSQS*D*D  + DGDBLS*BAL/(ALOG(D+1.0))&
                &+DGPTCC*PCCF(ITRE(I))&
                &+DGBA(ISPC)*BA

           !  USE SPRUCE-FIR MODEL TYPE LOWER BA LIMIT FOR SPECIES USING SURROGATE
           !  EQUATIONS FROM THE CR VARIANT

        case(17:19,22)
           ICLS = IFIX(D + 1.0)
           if (ICLS .gt.41) ICLS = 41
           BAUTBA = BAU(ICLS) / BA
           SI = SITEAR(ISPC)
           DPP = D
           if (DPP .lt.1.0) DPP = 1.0
           BATEM = BA
           if (BATEM .lt.5.0) BATEM = 5.0
           if (ISPC .eq.17) then
              DF = 0.25897 + 1.03129 * DPP - 0.0002025464 * BATEM&
                   &+ 0.00177 * SI
              if ((DF-DPP) .gt.1.0) DF = DPP + 1.0
           else
              DF = (1.55986 + 1.01825 * DPP - 0.29342 * ALOG(BATEM)&
                   &+ 0.00672*SI - 0.00073*BAUTBA)*1.05
           end if
           !CCC        IF(DF .GT. DBHMAX(ISPC)) DF = DBHMAX(ISPC)
           if (DF .lt. DPP) DF = DPP
           DIAGR = (DF - DPP) * BARK
           if (ISTAGF(ISPC).ne.0) DIAGR = DIAGR*DSTAG
           if (DIAGR .le.0.) then
              DDS = -9.21
           else
              DDS = ALOG( (DIAGR * (2.0 * DPP * BARK + DIAGR)) )&
                   &+ COR(ISPC) + DGCON(ISPC)
              if (DDS .lt. -9.21) DDS = -9.21
           end if

        end select

        if (DDS.lt.-9.21) DDS = -9.21
        WK2(I) = DDS

        !  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.

        if (.not.DEBUG) go to 10
        write(JOSTND,9000) I,ISPC,D,BAL,CR,BA,DDS
        9000 format(' IN DGF, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,&
             &',  BAL=',F7.2,',  CR=',F7.4,&
             &',  BA=',F9.3,',   LN(DDS),=',F7.4)
        10 continue
     end do

     !  END OF SPECIES LOOP.

     20 continue
  end do
  return

  entry DGCONS

  !  ENTRY POINT FOR LOADING COEFFICIENTS OF THE DIAMETER INCREMENT
  !  MODEL THAT ARE SITE SPECIFIC AND NEED ONLY BE RESOLVED ONCE.
  !  IDTYPE IS A HABITAT TYPE INDEX THAT IS COMPUTED IN **RCON**.

  !  PUT SITE INTO ONE OF 5 CLASSES

  !**   ISI=SITEAR(ISISP)/100.0
  !**   XSITE=SITEAR(ISISP)-ISI*100
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
     ISPFOR = MAPLOC(IFOR,ISPC)
     ISPDSQ = MAPDSQ(IFOR,ISPC)

     !  LOAD DGCON, DGDSQ,  AND ATTEN.   SHIFT ASPECT 45 DEGREES
     !  FOR THE DIAMETER INCREMENT MODEL INTERCEPT CALCULATIONS.

     !  NOTE: 0.7854 IS 45 DEGREES IN RADIANS. THIS CORRECTION IS
     !        PROBABLY DUE TO A PROBLEM IN THE WAY ASPECT WAS RECORDED
     !        IN THE DATA SET USED TO FIT THE MODEL.  GED 12-20-94.

     ISI = ISMAP(ISISP)
     TEMEL = ELEV
     XSITE = SITEAR(ISISP)
     select case(ISPC)
     case(20)
        if (TEMEL.gt.30.)TEMEL = 30.
        XSITE = ALOG(XSITE)
        ASPTEM = ASPECT
     case(21)
        XSITE = ALOG(XSITE)
        ASPTEM = ASPECT
     case default
        ASPTEM = ASPECT-0.7854
     end select
     DGCON(ISPC) = DGSIC(IDGSIM(ISI,ISPC),ISPC) *XSITE&
          &+ DGFOR(ISPFOR,ISPC)&
          &+(DGSASP(ISPC) * SIN(ASPTEM)&
          &+ DGCASP(ISPC) * COS(ASPTEM)&
          &+ DGSLOP(ISPC)) * SLOPE&
          &+ DGSLSQ(ISPC) * SLOPE * SLOPE&
          &+ DGEL(ISPC) * TEMEL&
          &+ DGEL2(ISPC) * TEMEL * TEMEL
     DGDSQ(ISPC) = DGDS(ISPDSQ,ISPC)
     SMCON(ISPC) = 0.
     select case (ISPC)
     case(20:21)
        ATTEN(ISPC) = OBSERV(ISPC)
     case default
        ATTEN(ISPC) = IBSERV(ISIC,ISPC)
     end select

     !  IF READCORD OR REUSCORD WAS SPECIFIED (LDCOR2 IS TRUE) ADD
     !  LN(COR2) TO THE BAI MODEL CONSTANT TERM (DGCON).  COR2 IS
     !  INITIALIZED TO 1.0 IN BLKDATA.

     if (LDCOR2.and.COR2(ISPC).gt.0.0) DGCON(ISPC) = DGCON(ISPC)&
          &+ ALOG(COR2(ISPC))
     30 continue
  end do

  return
end
