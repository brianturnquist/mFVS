!========================================!
!   Compute large tree diameter growth   !
!========================================!
! TT $Id$
subroutine DGF(DIAM)
  implicit none

  !  THIS SUBROUTINE COMPUTES THE VALUE OF DDS (CHANGE IN SQUARED
  !  DIAMETER) FOR EACH TREE RECORD, AND LOADS IT INTO THE ARRAY WK2.
  !  THE SET OF TREE DIAMETERS TO BE USED IS PASSED AS THE
  !  ARGUEMENT DIAM.  THE PROGRAM THUS HAS THE FLEXIBILITY TO
  !  PROCESS DIFFERENT CALIBRATION OPTIONS.  THIS ROUTINE IS CALLED
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
  !     DGCR -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
  !             RATIO TERM IN THE DDS MODEL (ONE COEFFICIENT FOR
  !             EACH SPECIES).
  !   DGCRSQ -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
  !             RATIO SQUARED TERM IN THE DDS MODEL (ONE
  !             COEFFICIENT FOR EACH SPECIES).
  !    DGBAL -- ARRAY CONTAINING COEFFICIENTS FOR THE BASAL AREA IN
  !             LARGER TREES TERM IN THE DDS MODEL
  !             (ONE COEFFICIENT FOR EACH SPECIES).
  !    DGCCF -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
  !             COMPETITION FACTOR TERM IN THE DDS MODEL (ONE
  !             COEFFICIENT FOR EACH SPECIES, LOADED IN RCON).

  ! Variables
  logical :: DEBUG
  integer :: IGCCFM(5,MAXSP)
  integer :: IBSERV(5,MAXSP),ISMAP(MAXSP),ICLS,MAPHAB,II
  integer :: IDGSIM(5,MAXSP),MAPLOC(4,MAXSP),MAPDSQ(4,MAXSP)
  integer :: ISI,ISC,ISPC,I1,I2,KSI,I3,I,LSI,ISIC,ISIC2,ICHBCL(363)
  real    :: DGFOR(5,MAXSP),DGSIC(5,MAXSP),DGCRSQ(MAXSP),DGHAB(7)
  real    :: DIAM(MAXTRE),DGLD(MAXSP),DGBAL(MAXSP),DGCR(MAXSP)
  real    :: DGDS(4,MAXSP),DGCCFC(5,MAXSP)
  real    :: DGSASP(MAXSP),DGCASP(MAXSP),DGSLOP(MAXSP),DGSLSQ(MAXSP)
  real    :: BA100,CONSPP,DGLDS,DGBALS,DGCRS,DGCRS2,DGDSQS,SI,D,CR
  real    :: ASPDG,BAL,DDS,XSITE,YOUNG,OLD,SUMD2,SUMTRE,DPROB
  real    :: DGEL(MAXSP),DGEL2(MAXSP),DGDBAL(MAXSP),DGBA(MAXSP),DGDBLS,DGPCCF(MAXSP),OBSERV(MAXSP),&
       &DGQMD,STDSDI,RELSDI,ALD,DGLCCF,DGPTCC,BARK,BRATIO,DPP,BATEM,DF,DIAGR,BAUTBA,ASPTEM,TEMEL,TEMBA,PBAL

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

  data DGLD/&
       &0.213947,  0.213947,  0.533965,       0.0,  0.378802,&
       &-0.368391,  0.563751,  0.378802,  0.648535,  0.822203,&
       &0.0,       0.0,  1.024186,       0.0,       0.0,&
       &0.889596,  0.213947,       0.0/

  data DGCR/&
       &1.523464,  1.523464,  1.931900,       0.0,  1.098353,&
       &4.034753,  2.164346,  1.098353,  0.137638,  1.768935,&
       &0.0,       0.0,  0.459387,       0.0,       0.0,&
       &1.732535,  1.523464,       0.0/

  data DGCRSQ/&
       &0.0,       0.0, -0.894368,       0.0,       0.0,&
       &-5.617552, -0.625799,       0.0, 1.0665429, -0.176164,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0/

  data DGBAL/&
       &-0.358634, -0.358634, -0.574858,       0.0, -0.490005,&
       &-0.704392, -0.469671, -0.490005, -0.312129,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0, -0.358634,       0.0/

  data IGCCFM/&
       &5*1,&
       &5*1,&
       &1,2,2,2,2,&
       &5*1,&
       &1,1,2,3,3,&
       &5*1,&
       &5*1,&
       &1,1,2,3,3,&
       &1,1,1,2,2,&
       &5*1,&
       &5*1,&
       &5*1,&
       &5*1,&
       &5*1,&
       &5*1,&
       &5*1,&
       &5*1,&
       &5*1/

  !  DGCCFC CONTAINS COEFFICIENTS FOR THE CCF TERM BY SPECIES BY
  !  HABITAT CLASS.

  data DGCCFC/&
       &-0.199592,    4*0.0,&
       &-0.199592,    4*0.0,&
       &-0.641932,-0.141370,    3*0.0,&
       &5*0.,&
       &-0.045495,-0.204852,-0.311383,    2*0.0,&
       &0.472247,    4*0.0,&
       &-0.206752,    4*0.0,&
       &-0.045495,-0.204852,-0.311383,    2*0.0,&
       &-0.186614,-0.023236,    3*0.0,&
       &5*0.,&
       &5*0.,&
       &5*0.,&
       &5*0.,&
       &5*0.,&
       &5*0.,&
       &5*0.,&
       &-0.199592,    4*0.0,&
       &5*0./

  !  ASPECT IS STAND ASPECT.  IBSERV CONTAINS THE NUMBER OF
  !  IBSERVATIONS BY HABITAT CLASS BY SPECIES FOR THE UNDERLYING
  !  MODEL (THIS DATA IS ACTUALLY USED BY **DGDRIV** FOR CALIBRATION).

  !  IBSERV IS THE NUMBER OF OBSERVATIONS IN THE DIAMETER GROWTH
  !  MODEL BY SPECIES AND SITE CLASS.

  data  IBSERV/&
       &27,  70, 123, 101,  33,&
       &27,  70, 123, 101,  33,&
       &2, 125, 387,1008,1075,&
       &5*1000,&
       &3, 111, 242, 281, 334,&
       &184, 429, 356, 162,  74,&
       &0, 112,1023,1208, 295,&
       &3, 111, 242, 281, 334,&
       &1, 140, 798, 819, 530,&
       &5*0,&
       &5*1000,&
       &5*1000,&
       &5*0,&
       &184, 429, 356, 162,  74,&
       &5*1000,&
       &5*0,&
       &27,  70, 123, 101,  33,&
       &5*1000/

  data OBSERV/&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,    8420.0,&
       &0.0,       0.0,      78.0,       0.0,       0.0,&
       &220.0,       0.0,       0.0/

  !  DGFOR CONTAINS LOCATION CLASS CONSTANTS FOR EACH SPECIES.
  !  MAPLOC IS AN ARRAY WHICH MAPS FOREST ONTO A LOCATION CLASS.

  data MAPLOC/&
       &1,1,2,3,&
       &1,1,2,3,&
       &1,1,2,3,&
       &4*1,&
       &1,1,2,3,&
       &1,1,2,1,&
       &1,1,2,2,&
       &1,1,2,3,&
       &1,1,2,3,&
       &4*1,&
       &4*1,&
       &4*1,&
       &4*1,&
       &4*1,&
       &4*1,&
       &4*1,&
       &1,1,2,3,&
       &4*1/

  data DGFOR/&
       &1.911884, 1.568742, 2.001195,      0.0,     0.0,&
       &1.911884, 1.568742, 2.001195,      0.0,     0.0,&
       &1.084994, 0.796640, 1.042871,      0.0,     0.0,&
       &5*0.0,&
       &1.543251, 0.943003, 0.792165,      0.0,     0.0,&
       &1.543622, 1.643733,      0.0,      0.0,     0.0,&
       &0.494205, 0.502908,      0.0,      0.0,     0.0,&
       &1.543251, 0.943003, 0.792165,      0.0,     0.0,&
       &0.921658, 0.807282, 0.914279,      0.0,     0.0,&
       &1.879603,     4*0.,&
       &5*0.0,&
       &5*0.0,&
       &-7.753469,      0.0,      0.0,      0.0,     0.0,&
       &5*0.0,&
       &5*0.0,&
       &-0.107648,      0.0,      0.0,      0.0,     0.0,&
       &1.911884, 1.568742, 2.001195,      0.0,     0.0,&
       &5*0.0/

  !    MAPDSQ IS A VECTOR WHICH MAPS FOREST ONTO A DBH**2
  !  COEFFICIENT.

  data MAPDSQ/&
       &1,1,1,1,&
       &1,1,1,1,&
       &1,1,1,1,&
       &4*1,&
       &1,1,1,1,&
       &1,1,1,1,&
       &1,1,2,3,&
       &1,1,1,1,&
       &1,1,1,2,&
       &4*1,&
       &4*1,&
       &4*1,&
       &4*1,&
       &4*1,&
       &4*1,&
       &4*1,&
       &1,1,1,1,&
       &4*1/

  !  DGDS CONTAINS COEFFICIENTS FOR THE DIAMETER SQUARED TERMS
  !  IN THE DIAMETER INCREMENT MODELS; ARRAYED BY FOREST BY
  !   SPECIES.
  !  ORIGINAL EQUATION FOR SPECIES 1,2,16 HAD + TERM, CHANGED TO - TERM
  !  TO AVOID RUNAWAY DIAMETER GROWTH  GD 01/11/00

  data DGDS/&
       &-0.0006538,      3*0.0,&
       &-0.0006538,      3*0.0,&
       &-0.0001997,      3*0.0,&
       &4*0.0,&
       &-0.0001056,      3*0.0,&
       &0.0058612,      3*0.0,&
       &0.0, -0.0009803, -0.0016416,        0.0,&
       &-0.0001056,      3*0.0,&
       &-0.0002152, -0.0002567, 2*0.0,&
       &-0.0004163,      3*0.0,&
       &4*0.0,&
       &4*0.0,&
       &-0.0001737,        0.0,        0.0,        0.0,&
       &4*0.0,&
       &4*0.0,&
       &4*0.0,&
       &-0.0006538,      3*0.0,&
       &4*0.0/

  !  DGSASP CONTAINS THE COEFFICIENTS FOR THE SIN(ASPECT)*SLOPE
  !  TERM IN THE DIAMETER GROWTH EQUATION.  DGCASP CONTAINS THE
  !  COEFFICIENTS FOR THE COS(ASPECT)*SLOPE TERM IN THE DIAMETER
  !  GROWTH EQUATION.  DGSLOP CONTAINS THE COEFFICIENTS FOR THE
  !  SLOPE TERM IN THE DIAMETER GROWTH EQUATION.  DGSLSQ CONTAINS
  !  COEFFICIENTS FOR THE (SLOPE)**2 TERM IN THE DIAMETER GROWTH MODELS.
  !  ALL OF THESE ARRAYS ARE SUBSCRIPTED BY SPECIES.

  data DGCASP /&
       &-0.609774, -0.609774, -0.268610,       0.0, -0.698103,&
       &-0.042546, -0.075306, -0.698103,  -0.17839,  0.127311,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.085958, -0.609774,       0.0/

  data DGSASP/&
       &-0.017520, -0.017520,  0.076614,       0.0,  0.102053,&
       &0.332422, -0.036871,  0.102053,  0.052805,  0.076531,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &-0.86398, -0.017520,       0.0/

  data DGSLOP/&
       &-2.057060, -2.057060, -0.711260,       0.0,  1.335928,&
       &-2.43008, -0.129291,  1.335928,  0.784185,  0.024336,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0, -2.057060,       0.0/

  data DGSLSQ/&
       &2.11326,  2.113263,       0.0,       0.0, -1.481349,&
       &0.0,       0.0, -1.481349, -1.504007, -0.781480,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,  2.113263,       0.0/

  data DGEL/&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0, -0.012111,       0.0,       0.0,&
       &-0.075986,       0.0,       0.0/

  data DGEL2/&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.001193,       0.0,       0.0/

  data DGDBAL/&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0, -0.006065,&
       &0.0,       0.0, -0.010222,       0.0,       0.0,&
       &-0.001265,       0.0,       0.0/

  data DGBA/&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &-0.000981,       0.0,       0.0/

  data DGPCCF/&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0, -0.000757,       0.0,       0.0,&
       &0.0,       0.0,       0.0/

  ! SITE BASE SPECIES REFERENCE
  ! 1=WB,LM,LP,OT / 2=DF / 3=ES,AF / 4=-- / 5=AS

  data IDGSIM/&
       &1,1,1,1,1,&
       &1,1,1,1,1,&
       &1,1,1,1,1,&
       &5*1,&
       &1,1,1,2,2,&
       &1,1,1,1,1,&
       &1,1,1,2,2,&
       &1,1,1,2,2,&
       &1,1,1,2,2,&
       &5*1,&
       &5*1,&
       &5*1,&
       &5*1,&
       &5*1,&
       &5*1,&
       &5*1,&
       &1,1,1,1,1,&
       &5*1/

  data DGSIC /&
       &0.001766,    4*0.0,&
       &0.001766,    4*0.0,&
       &0.011597,    4*0.0,&
       &5*0.0,&
       &0.011389, 0.019985, 3*0.0,&
       &0.006460,    4*0.0,&
       &0.009756, 0.014334, 3*0.0,&
       &0.011389, 0.019985, 3*0.0,&
       &0.003955, 0.006310, 3*0.0,&
       &5*0.0,&
       &5*0.0,&
       &5*0.0,&
       &1.965888,    4*0.0,&
       &0.006460,    4*0.0,&
       &5*0.0,&
       &0.227307,    4*0.0,&
       &0.001766,    4*0.0,&
       &5*0.0/

  data ISMAP/ 1, 1, 2, 1, 3, 5, 1, 3, 3, 1,&
       &1, 1, 1, 1, 1, 1, 1, 1/

  !  PONDEROSA PINE FROM CI VARIANT USES HABITAT TYPE. TT MAPPING IS MY
  !  BEST GUESS AT MATCHING THE CI MAPPING.  IF THE NUMBER OF RECOGNIZED
  !  HABITAT TYPES IN TT CHANGES, THEN THIS MAPPING WILL NEED TO BE ADJUSTED.
  !  ged 02/17/10

  data ICHBCL/ 4*1,1,4*1,1,4*1,2*2,5*1,4,18*0,7*1,4*4,& !51
       &222*0,6*1,2,2*3,2*4,4,3,4*0,3*4,12*0,2*0,0,0,0,0,0,5,3*0,& !52-316
       &16*0,2*0,0,3*0,9*0,0,3*0,0,0,0,2*0,3*0,3*0,0/ !317-363

  data DGHAB/&
       &0.0, 0.006074, 0.181590, -0.196098, -0.055780, 0.133907,&
       &0.045857/

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'DGF',3,ICYC)

  !  DEBUG OUTPUT: MODEL COEFFICIENTS.

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

  !  BEGIN SPECIES LOOP.  ASSIGN VARIABLES WHICH ARE SPECIES DEPENDENT

  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (I1.eq.0) go to 20
     I2 = ISCT(ISPC,2)
     CONSPP = DGCON(ISPC) + COR(ISPC) + 0.01*DGCCF(ISPC)*RELDEN
     if (ISPC .eq.10) then
        TEMBA = BA
        if (TEMBA .lt.1)TEMBA = 1.
        CONSPP = CONSPP - 0.257322*ALOG(BA)
     end if

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
          &DGCRS,DGCRS2,DGDSQS,DGPTCC,DGCCF(ISPC),DGPTCC
     9002 format(' DGF ISPC',I2,1X,6(E12.4,1X),3E10.4)

     !  BEGIN TREE LOOP WITHIN SPECIES ISPC.

     do I3 = I1,I2
        I = IND1(I3)
        D = DIAM(I)
        BARK = BRATIO(ISPC,D,HT(I))
        if (D.le.0.0) goto 10

        select case (ISPC)

        case(1:3,5,7:9,17)
           CR = ICR(I) * 0.01
           BAL = (1.0 - (PCT(I)/100.)) * BA100
           DDS = CONSPP + DGLDS*ALOG(D) + DGBALS*BAL + CR*DGCRS+CR*CR*DGCRS2&
                &+DGDSQS*D*D

        case(10)
           CR = ICR(I) * 0.01
           BAL = (1.0 - (PCT(I)/100.)) * BA
           PBAL = (1.0 - (PCT(I)/100.)) * PTBAA(ITRE(I))
           DDS = CONSPP +DGLDS*ALOG(D) + CR*(DGCRS+CR*DGCRS2)&
                &+ DGDSQS*D*D  + DGDBLS*PBAL/(ALOG(D+1.0))

        case(6,14)
           CR = ICR(I)
           call DGFASP(D,ASPDG,CR,BARK,SI,DEBUG)
           DDS = ASPDG + ALOG(COR2(ISPC)) + COR(ISPC)

           ! NOTE: EQNS IN **REGENT** ARE USED FOR ALL SIZED TREES FOR PM, UJ,
           !       RM, BI, AND MC. THIS CODE IS LEFT HERE JUST AS A PLACEHOLDER
           !       IN CASE WE WANT TO CHANGE THIS IN THE FUTURE. CODE HERE
           !       REFLECTS THE SOURCE VARIANT WHERE THE EQNS CAME FROM.

        case(4,11,12)
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

           ! NOTE: EQNS IN **REGENT** ARE USED FOR ALL SIZED TREES FOR PM, UJ,
           !       RM, BI, AND MC. THIS CODE IS LEFT HERE JUST AS A PLACEHOLDER
           !       IN CASE WE WANT TO CHANGE THIS IN THE FUTURE. CODE HERE
           !       REFLECTS THE SOURCE VARIANT WHERE THE EQNS CAME FROM.

        case(13,16)
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

        case(15,18)
           ICLS = IFIX(D + 1.0)
           if (ICLS .gt.41) ICLS = 41
           BAUTBA = BAU(ICLS) / BA
           SI = SITEAR(ISPC)
           DPP = D
           if (DPP .lt.1.0) DPP = 1.0
           BATEM = BA
           if (BATEM .lt.5.0) BATEM = 5.0
           DF = (1.55986 + 1.01825 * DPP - 0.29342 * ALOG(BATEM)&
                &+ 0.00672*SI - 0.00073*BAUTBA)*1.05
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
        write(JOSTND,9003) I,ISPC,D,CR,RELDEN,BA,DDS
        9003 format(' IN DGF, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,&
             &',  CR=',F7.4/&
             &'       RELDEN=',F9.3,',  BA=',F9.3,',   LN(DDS)=',F7.4)
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

  !**   XSITE=SITEAR(ISISP)-ISI*100
  XSITE = SITEAR(ISISP)
  LSI = INT(XSITE/10.)
  if (LSI.lt.2)ISIC = 1
  if (LSI.ge.5)ISIC = 5
  if (LSI.ge.3.and. LSI.lt.4)ISIC = 3
  if (LSI.ge.4.and. LSI.lt.5)ISIC = 4
  if (LSI.ge.2.and. LSI.lt.3)ISIC = 2

  ! ISISP IS THE SPECIES CODE THE SITE WAS TAKEN WITH
  ! ISIC IS THE SITE CLASS 1=0-29,2=30-39,...,5=60+ FOR THE SITE SPECIES

  ISIC2 = INT(SITEAR(ISISP))
  if (ISIC2.gt.59)ISIC2 = 60
  if (ISIC2.lt.20)ISIC2 = 20
  ISC = ISIC2/10

  ! PUT THE SITE IN 10'S INTO A CLASS 1-5

  ISC = ISC-1

  !  ENTER LOOP TO LOAD SPECIES DEPENDENT VECTORS.

  do ISPC = 1,MAXSP
     KSI = IGCCFM(ISC,ISPC)
     DGCCF(ISPC) = DGCCFC(KSI,ISPC)
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

     case(16)
        if (TEMEL.gt.30.)TEMEL = 30.
        XSITE = ALOG(XSITE)
        ASPTEM = ASPECT

     case(13)
        XSITE = ALOG(XSITE)
        ASPTEM = ASPECT

     case(10)
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

     case(10)
        if (ITYPE .gt.0) then
           MAPHAB = ICHBCL(ITYPE) + 1
        else
           MAPHAB = 2
        end if
        DGCON(ISPC) = DGCON(ISPC) + DGHAB(MAPHAB)
        ATTEN(ISPC) = OBSERV(ISPC)

     case(13,16)
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
