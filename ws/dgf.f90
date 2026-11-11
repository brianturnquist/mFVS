!========================================!
!   Compute large tree diameter growth   !
!========================================!
! WS $Id$
subroutine DGF(DIAM)
  implicit none

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
  !             RATIO TERM IN THE DDS MODEL
  !   DGSITE -- ARRAY CONTAINING THE COEFFICIENTS FOR THE SITE
  !             TERM IN THE DDS MODEL
  !    DGBA  -- ARRAY CONTAINING COEFFICIENTS FOR THE BASAL AREA
  !             TERM IN THE DDS MODEL
  !   DGDBA  -- ARRAY CONTAINING COEFFICIENTS FOR THE INTERACTION
  !             BETWEEN BASAL AREA IN LARGER TREES AND LN(DBH)
  !    DGDS  -- ARRAY CONTAINING THE COEFFICIENTS FOR THE DIAMETER
  !             SQUARED TERM IN THE DDS MODEL
  !     DGEL -- ARRAY CONTAINING THE COEFFICIENTS FOR THE ELEVATION TERM.
  !    DGSLP -- ARRAY CONTAINING THE COEFFICIENTS FOR THE SLOPE TERM.
  !    DGSLQ -- ARRAY CONTAINING THE COEFFICIENTS FOR THE SLOPE SQUARED TE
  !    DGLAT -- ARRAY CONTAINING THE COEFFICIENTS FOR THE LATITUDE TERM.

  ! IF SPECIES IS 5 OR 9, USE LEROY'S ORIGINAL WS EQUATIONS WITH SITE.
  ! IF SPECIES IS 7 OR 11, USE NC EQNS FOR BO & TO.
  ! FOR ALL OTHER SPECIES USE EQUATIONS REFIT BY JOHNSON 1992.

  ! IF SPECIES IS 3, 4, 8, OR 9, USE SEPERATE EQUATIONS FOR TREES
  ! SMALLER THAN 10" DBH.

  external RANN
  ! Variables
  logical :: DEBUG
  integer :: OBSERV(MAXSP),MAPLOC(7,MAXSP)
  integer :: ILAT,I,IPCCF,I1,I2,I3,ISPC,J,ITLAT,IWHO
  real    :: DIAM(MAXTRE),DGLD(MAXSP),DGCR(MAXSP),DGCRSQ(MAXSP),DGDBAL(MAXSP),DGBA(MAXSP),&
       &DGHAH(MAXSP),DGPCCF(MAXSP),DGFOR(3,MAXSP),DGSITE(MAXSP),DGELSQ(MAXSP),DGEL(MAXSP),DGCASP(MAXSP),&
       &DGSASP(MAXSP),DGSLOP(MAXSP),DGSLSQ(MAXSP),DGLAT5(5),DGLAT9(5),DGDS(MAXSP)
  real    :: FORCON,CONSJP,CONSPP,DGLDS,DGCRS,DGCRS2,SI
  real    :: DGDBLS,DGPCFS,DGHAHS,DGBAS,D,DPP,HOAVH,CR,ALD,CRID,BAL
  real    :: PBA,PBAL,ALBA,DDS,TDDS,DDSMAX,TSITE,DGDSQS,DGBAL
  real    :: BATEM,DF,DIAGR,BARK,SUMD2,SUMTRE,DGQMD,STDSDI,RELSDI
  real    :: DDSMX1(MAXSP),DDSMX2(MAXSP),BRATIO,TEMEL
  real    :: PRD,DLO,DHI,SDICS,SDICZ,A,B,XMAX,DGLT
  real    :: BRAT,TEMPD1,TEMPD2,DUP
  real    :: ZRD(MAXPLT)

  !     SPECIES LIST FOR WESTERN SIERRAS VARIANT.

  !     1 = SUGAR PINE (SP)                   PINUS LAMBERTIANA
  !     2 = DOUGLAS-FIR (DF)                  PSEUDOTSUGA MENZIESII
  !     3 = WHITE FIR (WF)                    ABIES CONCOLOR
  !     4 = GIANT SEQUOIA (GS)                SEQUOIADENDRON GIGANTEAUM
  !     5 = INCENSE CEDAR (IC)                LIBOCEDRUS DECURRENS
  !     6 = JEFFREY PINE (JP)                 PINUS JEFFREYI
  !     7 = CALIFORNIA RED FIR (RF)           ABIES MAGNIFICA
  !     8 = PONDEROSA PINE (PP)               PINUS PONDEROSA
  !     9 = LODGEPOLE PINE (LP)               PINUS CONTORTA
  !    10 = WHITEBARK PINE (WB)               PINUS ALBICAULIS
  !    11 = WESTERN WHITE PINE (WP)           PINUS MONTICOLA
  !    12 = SINGLELEAF PINYON (PM)            PINUS MONOPHYLLA
  !    13 = PACIFIC SILVER FIR (SF)           ABIES AMABILIS
  !    14 = KNOBCONE PINE (KP)                PINUS ATTENUATA
  !    15 = FOXTAIL PINE (FP)                 PINUS BALFOURIANA
  !    16 = COULTER PINE (CP)                 PINUS COULTERI
  !    17 = LIMBER PINE (LM)                  PINUS FLEXILIS
  !    18 = MONTEREY PINE (MP)                PINUS RADIATA
  !    19 = GRAY PINE (GP)                    PINUS SABINIANA
  !         (OR CALIFORNIA FOOTHILL PINE)
  !    20 = WASHOE PINE (WE)                  PINUS WASHOENSIS
  !    21 = GREAT BASIN BRISTLECONE PINE (GB) PINUS LONGAEVA
  !    22 = BIGCONE DOUGLAS-FIR (BD)          PSEUDOTSUGA MACROCARPA
  !    23 = REDWOOD (RW)                      SEQUOIA SEMPERVIRENS
  !    24 = MOUNTAIN HEMLOCK (MH)             TSUGA MERTENSIANA
  !    25 = WESTERN JUNIPER (WJ)              JUNIPERUS OCIDENTALIS
  !    26 = UTAH JUNIPER (UJ)                 JUNIPERUS OSTEOSPERMA
  !    27 = CALIFORNIA JUNIPER (CJ)           JUNIPERUS CALIFORNICA
  !    28 = CALIFORNIA LIVE OAK (LO)          QUERCUS AGRIFOLIA
  !    29 = CANYON LIVE OAK (CY)              QUERCUS CHRYSOLEPSIS
  !    30 = BLUE OAK (BL)                     QUERCUS DOUGLASII
  !    31 = CALIFORNIA BLACK OAK (BO)         QUERQUS KELLOGGII
  !    32 = VALLEY OAK (VO)                   QUERCUS LOBATA
  !         (OR CALIFORNIA WHITE OAK)
  !    33 = INTERIOR LIVE OAK (IO)            QUERCUS WISLIZENI
  !    34 = TANOAK (TO)                       LITHOCARPUS DENSIFLORUS
  !    35 = GIANT CHINQUAPIN (GC)             CHRYSOLEPIS CHRYSOPHYLLA
  !    36 = QUAKING ASPEN (AS)                POPULUS TREMULOIDES
  !    37 = CALIFORNIA-LAUREL (CL)            UMBELLULARIA CALIFORNICA
  !    38 = PACIFIC MADRONE (MA)              ARBUTUS MENZIESII
  !    39 = PACIFIC DOGWOOD (DG)              CORNUS NUTTALLII
  !    40 = BIGLEAF MAPLE (BM)                ACER MACROPHYLLUM
  !    41 = CURLLEAF MOUNTAIN-MAHOGANY (MC)   CERCOCARPUS LEDIFOLIUS
  !    42 = OTHER SOFTWOODS (OS)
  !    43 = OTHER HARDWOODS (OH)

  !  SURROGATE EQUATION ASSIGNMENT:

  !    FROM EXISTING WS EQUATIONS --
  !      USE 1(SP) FOR 11(WP) AND 24(MH)
  !      USE 2(DF) FOR 22(BD)
  !      USE 3(WF) FOR 13(SF)
  !      USE 8(PP) FOR 18(MP)
  !      USE 34(TO) FOR 35(GC), 36(AS), 37(CL), 38(MA), AND 39(DG)
  !      USE 31(BO) FOR 28(LO), 29(CY), 30(BL), 32(VO), 33(IO), 40(BM), AND
  !                     43(OH)

  !    FROM CA VARIANT --
  !      USE CA11(KP) FOR 12(PM), 14(KP), 15(FP), 16(CP), 17(LM), 19(GP), 20(WE),
  !                       25(WJ), 26(WJ), AND 27(CJ)
  !      USE CA12(LP) FOR 9(LP) AND 10(WB)

  !    FROM SO VARIANT --
  !      USE SO30(MC) FOR 41(MC)

  !    FROM UT VARIANT --
  !      USE UT17(GB) FOR 21(GB)

  !    GS AND RW USE EQUATIONS FROM CASTLE 2019.

  !  DATA STATEMENTS

  data DDSMX1/&
       &-3.91, -3.06, -2.81, -3.91, -2.20,&
       &-3.71, -2.81, -2.07,   0.0,   0.0,&
       &-3.91,   0.0, -2.81,   0.0,   0.0,&
       &0.0,   0.0, -2.07,   0.0,   0.0,&
       &0.0, -3.06, -3.91, -3.91,   0.0,&
       &0.0,   0.0, -2.20, -2.20, -2.20,&
       &-2.20, -2.20, -2.20, -2.20, -2.20,&
       &-2.20, -2.20, -2.20, -2.20, -2.20,&
       &0.0, -3.71, -2.20/

  data DDSMX2/&
       &1.42,  1.27,  1.24,  1.42,  1.08,&
       &1.25,  1.24,  1.09,   0.0,   0.0,&
       &1.42,   0.0,  1.24,   0.0,   0.0,&
       &0.0,   0.0,  1.09,   0.0,   0.0,&
       &0.0,  1.27,  1.42,  1.42,   0.0,&
       &0.0,   0.0,  1.08,  1.08,  1.08,&
       &1.08,  1.08,  1.08,  1.08,  1.08,&
       &1.08,  1.08,  1.08,  1.08,  1.08,&
       &0.0,  1.25,  1.08/

  data DGLD/&
       &1.0857,    0.8641,    1.2854,       0.0,   1.29079,&
       &0.51047,       0.0,   0.96103,  1.218279,  1.218279,&
       &1.0857,  1.077154,    1.2854,  1.077154,  1.077154,&
       &1.077154,  1.077154,   0.96103,  1.077154,  1.077154,&
       &0.0,    0.8641,       0.0,    1.0857,  1.077154,&
       &1.077154,  1.077154,   1.23911,   1.23911,   1.23911,&
       &1.23911,   1.23911,   1.23911,   0.99531,   0.99531,&
       &0.99531,   0.99531,   0.99531,   0.99531,   1.23911,&
       &0.889596,    1.1783,   1.23911/

  data DGCR/&
       &0.3910,    0.4246,   -1.0191,       0.0,   -0.0906,&
       &0.91422,       0.0,    0.4126,  3.167164,  3.167164,&
       &0.3910, -0.276387,   -1.0191, -0.276387, -0.276387,&
       &-0.276387, -0.276387,    0.4126, -0.276387, -0.276387,&
       &0.0,    0.4246,       0.0,    0.3910, -0.276387,&
       &-0.276387, -0.276387,  -1.20841,  -1.20841,  -1.20841,&
       &-1.20841,  -1.20841,  -1.20841,   2.08524,   2.08524,&
       &2.08524,   2.08524,   2.08524,   2.08524,  -1.20841,&
       &1.732535,    0.9492,  -1.20841/

  data DGCRSQ/&
       &0.0,       0.0,    0.9104,       0.0,       0.0,&
       &0.27758,       0.0,       0.0, -1.568333, -1.568333,&
       &0.0,  1.063732,    0.9104,  1.063732,  1.063732,&
       &1.063732,  1.063732,       0.0,  1.063732,  1.063732,&
       &0.0,       0.0,       0.0,       0.0,  1.063732,&
       &1.063732,  1.063732,   2.31782,   2.31782,   2.31782,&
       &2.31782,   2.31782,   2.31782,  -0.98396,  -0.98396,&
       &-0.98396,  -0.98396,  -0.98396,  -0.98396,   2.31782,&
       &0.0,       0.0,   2.31782/

  data DGDBAL/&
       &-0.00579,  -0.01127,  -0.00628,       0.0,  -0.00544,&
       &-0.01282,       0.0,  -0.01265,       0.0,       0.0,&
       &-0.00579,       0.0,  -0.00628,       0.0,       0.0,&
       &0.0,       0.0,  -0.01265,       0.0,       0.0,&
       &0.0,  -0.01127,       0.0,  -0.00579,       0.0,&
       &0.0,       0.0,  -0.00199,  -0.00199,  -0.00199,&
       &-0.00199,  -0.00199,  -0.00199,  -0.00147,  -0.00147,&
       &-0.00147,  -0.00147,  -0.00147,  -0.00147,  -0.00199,&
       &-0.001265,  -0.00016,  -0.00199/

  data DGBA /&
       &-0.1313,       0.0,  -0.21056,       0.0,  -0.23182,&
       &-0.01880,       0.0,   -0.1431, -0.267873, -0.267873,&
       &-0.1313,       0.0,  -0.21056,       0.0,       0.0,&
       &0.0,       0.0,   -0.1431,       0.0,       0.0,&
       &0.0,       0.0,       0.0,   -0.1313,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &-0.000981,   -0.3270,       0.0/

  data DGHAH /&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,   0.50155,   0.50155,&
       &0.50155,   0.50155,   0.50155,   0.50155,       0.0,&
       &0.0,       0.0,       0.0/

  data DGPCCF/&
       &-0.00058,  -0.00018,  -0.00091,       0.0,  -0.00098,&
       &-0.00099,       0.0,  -0.00084, -0.000338, -0.000338,&
       &-0.00058,       0.0,  -0.00091,       0.0,       0.0,&
       &0.0,       0.0,  -0.00084,       0.0,       0.0,&
       &0.0,  -0.00018,       0.0,  -0.00058,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,   -0.0018,   -0.0018,&
       &-0.0018,   -0.0018,   -0.0018,   -0.0018,       0.0,&
       &0.0,       0.0,       0.0/

  !  OBSERV CONTAINS THE NUMBER OF OBSERVATIONS  BY HABITAT CLASS BY
  !  SPECIES FOR THE UNDERLYING MODEL.
  !  (THIS DATA IS ACTUALLY USED BY **DGDRIV** FOR CALIBRATION).

  data  OBSERV/&
       &650,      480,     3301,     8928,     1339,&
       &1114,        9,     1528,      372,      372,&
       &650,       84,     3301,       84,       84,&
       &84,       84,     1528,       84,       84,&
       &1000,      480,     8928,      650,       84,&
       &84,       84,      583,      583,      583,&
       &583,      583,      583,     6504,     6504,&
       &6504,     6504,     6504,     6504,      583,&
       &220,      419,      583/

  ! DGLAT CONTAINS LATITUDE CONSTANTS FOR SPECIES 5 & 9.

  data DGLAT5/ -0.4297, -0.4297, -0.4297, -0.2777, -0.4297/
  data DGLAT9/  0.1434,  0.3191,  0.1434,  0.2246,  0.1434/

  !  DGFOR CONTAINS LOCATION CLASS CONSTANTS FOR EACH SPECIES.
  !  MAPLOC IS AN ARRAY WHICH MAPS FOREST ONTO A LOCATION CLASS.
  !  CURRENTLY FOREST CODE WILL BE 1-6 IN THIS VARIANT.

  data ((MAPLOC(I,J),I = 1,7),J = 1,10)/&
       &1,2,2,2,1,2,1,&
       &1,1,2,1,1,1,1,&
       &1,2,3,3,1,3,1,&
       &1,1,1,1,1,1,1,&
       &1,1,2,3,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,2,2,1,2,1,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1/
  data ((MAPLOC(I,J),I = 1,7),J = 11,20)/&
       &1,2,2,2,1,2,1,&
       &1,1,1,1,1,1,1,&
       &1,2,3,3,1,3,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,2,2,1,2,1,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1/
  data ((MAPLOC(I,J),I = 1,7),J = 21,30)/&
       &1,1,1,1,1,1,1,&
       &1,1,2,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,2,2,2,1,2,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1/
  data ((MAPLOC(I,J),I = 1,7),J = 31,40)/&
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
  data ((MAPLOC(I,J),I = 1,7),J = 41,MAXSP)/&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,2,1,1,&
       &1,1,1,1,1,1,1/

  data ((DGFOR(I,J),I = 1,3),J = 1,10)/&
       &-0.70344, -0.90272,      0.0,&
       &-0.5260,  -0.9842,  -0.7603,&
       &0.0755,  -0.3099,  -0.0440,&
       &-1.69950,      0.0,      0.0,&
       &0.02786,  0.21393, -0.04927,&
       &-1.81306,  -2.3037,      0.0,&
       &0.0,      0.0,      0.0,&
       &-0.8882,  -1.0712,      0.0,&
       &-2.058828,      0.0,      0.0,&
       &-2.058828,      0.0,      0.0/
  data ((DGFOR(I,J),I = 1,3),J = 11,20)/&
       &-0.70344, -0.90272,      0.0,&
       &0.564402,      0.0,      0.0,&
       &0.0755,  -0.3099,  -0.0440,&
       &0.564402,      0.0,      0.0,&
       &0.564402,      0.0,      0.0,&
       &0.564402,      0.0,      0.0,&
       &0.564402,      0.0,      0.0,&
       &-0.8882,  -1.0712,      0.0,&
       &0.564402,      0.0,      0.0,&
       &0.564402,      0.0,      0.0/
  data ((DGFOR(I,J),I = 1,3),J = 21,30)/&
       &0.0,      0.0,      0.0,&
       &-0.5260,  -0.9842,  -0.7603,&
       &-1.69950,      0.0,      0.0,&
       &-0.70344, -0.90272,      0.0,&
       &0.564402,      0.0,      0.0,&
       &0.564402,      0.0,      0.0,&
       &0.564402,      0.0,      0.0,&
       &-2.68349,      0.0,      0.0,&
       &-2.68349,      0.0,      0.0,&
       &-2.68349,      0.0,      0.0/
  data ((DGFOR(I,J),I = 1,3),J = 31,40)/&
       &-2.68349,      0.0,      0.0,&
       &-2.68349,      0.0,      0.0,&
       &-2.68349,      0.0,      0.0,&
       &-0.94563,      0.0,      0.0,&
       &-0.94563,      0.0,      0.0,&
       &-0.94563,      0.0,      0.0,&
       &-0.94563,      0.0,      0.0,&
       &-0.94563,      0.0,      0.0,&
       &-0.94563,      0.0,      0.0,&
       &-2.68349,      0.0,      0.0/
  data ((DGFOR(I,J),I = 1,3),J = 41,MAXSP)/&
       &-0.107648,      0.0,      0.0,&
       &-0.02772,   0.1327,      0.0,&
       &-2.68349,      0.0,      0.0/

  !  DGEL CONTAINS THE COEFFICIENTS FOR THE ELEVATION TERM IN THE
  !  DIAMETER GROWTH EQUATION.
  !  DGSLOP CONTAINS THE COEFFICIENTS FOR THE
  !  SLOPE TERM IN THE DIAMETER GROWTH EQUATION.  DGSLSQ CONTAINS
  !  COEFFICIENTS FOR THE (SLOPE)**2 TERM IN THE DIAMETER GROWTH MODELS.
  !  ALL OF THESE ARRAYS ARE SUBSCRIPTED BY SPECIES.

  data DGCASP/&
       &0.01664,       0.0,   -0.1804,       0.0,       0.0,&
       &-0.16447,   -0.4919,   0.26986,       0.0,       0.0,&
       &0.01664,  0.649870,   -0.1804,  0.649870,  0.649870,&
       &0.649870,  0.649870,   0.26986,  0.649870,  0.649870,&
       &0.0,       0.0,       0.0,   0.01664,  0.649870,&
       &0.649870,  0.649870,   0.08632,   0.08632,   0.08632,&
       &0.08632,   0.08632,   0.08632,  -0.19935,  -0.19935,&
       &-0.19935,  -0.19935,  -0.19935,  -0.19935,   0.08632,&
       &0.085958,       0.0,   0.08632/

  data DGSASP/&
       &-0.00350,       0.0,   -0.1183,  -0.10656,       0.0,&
       &0.05342,       0.0,   0.09668,       0.0,       0.0,&
       &-0.00350,  0.951834,   -0.1183,  0.951834,  0.951834,&
       &0.951834,  0.951834,   0.09668,  0.951834,  0.951834,&
       &0.0,       0.0,  -0.10656,  -0.00350,  0.951834,&
       &0.951834,  0.951834,  -0.11954,  -0.11954,  -0.11954,&
       &-0.11954,  -0.11954,  -0.11954,  -0.03587,  -0.03587,&
       &-0.03587,  -0.03587,  -0.03587,  -0.03587,  -0.11954,&
       &-0.86398,       0.0,  -0.11954/

  data DGSLOP/&
       &0.7603,       0.0,       0.0,  -1.29627,       0.0,&
       &-0.05469,       0.0,   0.90804,       0.0,       0.0,&
       &0.7603,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,   0.90804,       0.0,       0.0,&
       &0.0,       0.0,  -1.29627,    0.7603,       0.0,&
       &0.0,       0.0,   0.85815,   0.85815,   0.85815,&
       &0.85815,   0.85815,   0.85815,   0.73530,   0.73530,&
       &0.73530,   0.73530,   0.73530,   0.73530,   0.85815,&
       &0.0,       0.0,   0.85815/

  data DGSLSQ/&
       &-2.2339,       0.0,       0.0,   0.87335,       0.0,&
       &0.0,       0.0,  -2.04028,       0.0,       0.0,&
       &-2.2339,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,  -2.04028,       0.0,       0.0,&
       &0.0,       0.0,   0.87335,   -2.2339,       0.0,&
       &0.0,       0.0,  -1.17209,  -1.17209,  -1.17209,&
       &-1.17209,  -1.17209,  -1.17209,  -0.99561,  -0.99561,&
       &-0.99561,  -0.99561,  -0.99561,  -0.99561,  -1.17209,&
       &0.0,       0.0,  -1.17209/

  data DGEL/&
       &0.01919,   0.00489,       0.0,       0.0,  -0.00919,&
       &0.00304,       0.0,   0.00323,       0.0,       0.0,&
       &0.01919,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,   0.00323,       0.0,       0.0,&
       &0.0,   0.00489,       0.0,   0.01919,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &-0.075986,       0.0,       0.0/

  data DGELSQ/&
       &-0.00025,       0.0,       0.0,   0.00019,   0.00019,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &-0.00025,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,   0.00019,  -0.00025,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0,       0.0,&
       &0.001193,       0.0,       0.0/

  data DGSITE/&
       &0.5827,    0.5040,    0.5260,       0.0,    0.3737,&
       &0.96412,       0.0,    0.6828,  0.566946,  0.566946,&
       &0.5827,       0.0,    0.5260,       0.0,       0.0,&
       &0.0,       0.0,    0.6828,       0.0,       0.0,&
       &0.0,    0.5040,       0.0,    0.5827,       0.0,&
       &0.0,       0.0,   0.32093,   0.32093,   0.32093,&
       &0.32093,   0.32093,   0.32093,   0.01200,   0.01200,&
       &0.01200,   0.01200,   0.01200,   0.01200,   0.32093,&
       &0.227307,    0.4401,   0.32093/

  data DGDS /&
       &-0.000288, -0.000290, -0.000584,       0.0,  -0.00061,&
       &-0.000222,       0.0, -0.000375,-0.0014178,-0.0014178,&
       &-0.000288,       0.0, -0.000584,       0.0,       0.0,&
       &0.0,       0.0, -0.000375,       0.0,       0.0,&
       &0.0, -0.000290,       0.0, -0.000288,       0.0,&
       &0.0,       0.0, -0.000338, -0.000338, -0.000338,&
       &-0.000338, -0.000338, -0.000338, -0.000373, -0.000373,&
       &-0.000373, -0.000373, -0.000373, -0.000373, -0.000338,&
       &0.0, -0.000660, -0.000338/

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'DGF',3,ICYC)
  if (DEBUG) write(JOSTND,3)ICYC
  3 format(' ENTERING SUBROUTINE DGF  CYCLE =',I5)

  !  DEBUG OUTPUT: MODEL COEFFICIENTS.

  if (DEBUG)write(JOSTND,9000) DGCON
  9000 format(' DGCON =',/,11(1X,F10.5))

  !  CALL SDICAL TO LOAD THE POINT MAX SDI WEIGHTED BY SPECIES ARRAY XMAXPT

  IWHO = 2
  call SDICAL (IWHO, XMAX)

  !  LOAD RELATIVE DENSITY (ZEIDI) FOR INDIVIDUAL POINTS.
  !  ALL SPECIES AND ALL SIZES INCLUDED FOR THIS CALCULATION.

  DLO = 0.0
  DHI = 500.0
  ISPC = 0
  IWHO = 1
  I2 = INT(PI)

  if (DEBUG)write(JOSTND,*) 'IN DGF, CALL SDICLS - POINT COUNT ',I2
  do I1 = 1, I2
     call SDICLS (ISPC,DLO,DHI,IWHO,SDICS,SDICZ,A,B,I1)
     if (DEBUG)write(JOSTND,*) 'IN DGF, POINT ',I1,' SDICZ= ',SDICZ
     ZRD(I1) = SDICZ
  end do

  !  SPECIES USING SURROGATE EQUATIONS FROM THE CR VARIANT HAVE SPECIAL
  !  NEEDS; COMPUTE THOSE HERE.

  ! COMPUTE STAGNATION MULTIPLIER DSTAG. DSTAG INITIALIZED IN GRINIT.

  SUMD2 = 0.
  SUMTRE = 0.
  DGQMD = 0.
  if (ITRN .eq.0) go to 1001
  do I = 1,ITRN
     SUMD2 = SUMD2+DBH(I)*DBH(I)*PROB(I)
     SUMTRE = SUMTRE+PROB(I)
     1000 continue
  end do
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
     call SDICAL(0,SDIMAX)
     if (SDIMAX.gt.0.)RELSDI = STDSDI/SDIMAX
     if (DEBUG)write(JOSTND,*)' STDSDI,SDIMAX,RELSDI= ',&
          &STDSDI,SDIMAX,RELSDI
     if (RELSDI .gt.0.85) RELSDI = 0.85
     DSTAG = 1.0
     if (RELSDI .gt.0.7) DSTAG = 3.33333 * (1.0 - RELSDI)
     if (DEBUG)write(JOSTND,*)' STAGNATION EFFECT= ',DSTAG
     if (DEBUG)write(JOSTND,*)' STAGNATION FLAGS = ',ISTAGF
  end if

  !  END OF SPECIAL NEEDS SECTION

  !  BEGIN SPECIES LOOP.  ASSIGN VARIABLES WHICH ARE SPECIES DEPENDENT

  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (I1.eq.0) go to 20
     I2 = ISCT(ISPC,2)
     FORCON = 0.0
     CONSJP = 0.0
     SI = SITEAR(ISPC)

     !  DEFINE CONSPP. RW AND GS DO NOT GET COR ADJUSTMENT UNTIL AFTER
     !  DIAMETER GROWTH PREDICTION.
     if (ISPC .eq.4.or. ISPC .eq.23) then
        CONSPP = DGCON(ISPC)
     else
        CONSPP = DGCON(ISPC) + COR(ISPC)
     end if
     DGLDS = DGLD(ISPC)
     DGCRS = DGCR(ISPC)
     DGCRS2 = DGCRSQ(ISPC)
     DGDSQS = DGDS(ISPC)
     DGDBLS = DGDBAL(ISPC)
     DGPCFS = DGPCCF(ISPC)
     DGHAHS = DGHAH(ISPC)
     DGBAS = DGBA(ISPC)

     !  LOAD A CONSPP VALUE (CONSJP) FOR SMALL:
     !  2=DF, 3=WF, 6=JP, 7=RF, 13=SF, 22=BD

     select case (ISPC)
     case(6)
        FORCON = 0.74162
        if (IFOR .eq.2) FORCON = 0.59493
        if (IGL .eq.2) FORCON = 0.854515
        CONSJP = COR(ISPC) + 0.40657*ALOG(SI) + 0.56709*SLOPE&
             &- 0.14671*COS(ASPECT)*SLOPE + 0.26785*SIN(ASPECT)*SLOPE&
             &- 0.00164*ELEV + FORCON
        if (LDCOR2.and.COR2(ISPC).gt.0.0)CONSJP = CONSJP+ALOG(COR2(ISPC))
     case(2,3,7,13,22)
        CONSJP = COR(ISPC) + 0.233713*ALOG(SI) + 1.53962
        if (LDCOR2.and.COR2(ISPC).gt.0.0)CONSJP = CONSJP+ALOG(COR2(ISPC))
     end select

     !  BEGIN TREE LOOP.

     do I3 = I1,I2
        I = IND1(I3)
        D = DIAM(I)
        if (D.le.0.0) goto 10
        BARK = BRATIO(ISPC,D,HT(I))
        IPCCF = ITRE(I)
        HOAVH = HT(I)/AVH
        CR = FLOAT(ICR(I))/100.
        ALD = ALOG(D)
        CRID = ((ICR(I)*ICR(I))/(ALOG(D+1.0)))/1000.0
        if (D .lt.2.0)CRID = 1.8
        BAL = (1.0 - (PCT(I)/100.)) * BA
        PBA = PTBAA(ITRE(I))
        PBAL = PBA*(1.0-(PCT(I)/100.))
        if (PBAL .lt.0.0)PBAL = BAL
        if (PBA .le.0.0) then
           if (BA .gt.1.0) then
              PBA = BA
           else
              PBA = 1.0
           end if
        end if
        ALBA = 0.0
        if (BA.gt.0.0)ALBA = ALOG(BA)
        !  RELATIVE DENSITY (ZEIDI) ON THE POINT
        !  CONSTRAIN RD IF NEEDED
        if (XMAXPT(ITRE(I)).le.0.0) then
           PRD = 0.0
        else
           PRD = ZRD(ITRE(I)) / XMAXPT(ITRE(I))
        end if

        select case (ISPC)

           !  SPECIES USING EQUATIONS FROM THE CA VARIANT

        case (9:10,12,14:17,19:20,25:27)
           if (HOAVH .gt.1.5)HOAVH = 1.5
           if (ISPC.eq.9.or. ISPC.eq.10) then
              DGBAL = 0.0
           else
              DGBAL = -0.000893
           end if
           DDS = CONSPP + DGLDS*ALD + DGCRS*CR + DGCRS2*CR*CR&
                &+ DGDSQS*D*D + DGDBLS*BAL/(ALOG(D+1.0))&
                &+ DGPCFS*PCCF(IPCCF) + DGHAHS*HOAVH&
                &+ DGBAS*ALBA + DGBAL*BAL

           !  SPECIES USING EQUATIONS FROM THE SO VARIANT

        case (41)
           if (HOAVH .gt.1.5)HOAVH = 1.5
           DDS = CONSPP + DGLDS*ALD + DGCRS*CR + DGCRS2*CR*CR&
                &+ DGDSQS*D*D + DGDBLS*BAL/(ALOG(D+1.0))&
                &+ DGPCFS*PCCF(IPCCF) + DGHAHS*HOAVH + DGBAS*BA

           !  SPECIES USING EQUATIONS FROM THE CR VARIANT (VIA UT)
           !  USE SPRUCE-FIR MODEL TYPE LOWER BA LIMIT

           !  THIS ESTIMATE IS NOT USED; TREES OF ALL SIZES ARE GROWN WITH THE
           !  EQUATIONS IN **REGENT**.

        case (21)
           DPP = D
           if (DPP .lt.1.0) DPP = 1.0
           BATEM = BA
           if (BATEM .lt.5.0) BATEM = 5.0
           DF = 0.25897 + 1.03129 * DPP - 0.0002025464 * BATEM&
                &+ 0.00177 * SI
           if ((DF-DPP) .gt.1.0) DF = DPP + 1.0
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

           !  RW AND GS EQUATION

        case(4,23)
           if (PBAL .lt.0.) PBAL = BAL
           DGLT = EXP(CONSPP + 0.185911*LOG(D) -0.000073*D*D&
                &-0.001796*PBAL - 0.42078*PRD + 0.589318*LOG(CR*100)&
                &-0.000926*SLOPE*100 - 0.002203*(SLOPE*100)*COS(ASPECT))

           !  CONVERT OUTSIDE BARK DIAMETER INCREMENT TO INSIDE BARK
           !  CHANGE IN SQUARED DIAMETERS AND LOAD INTO WK2 ARRAY.
           BRAT = BRATIO(ISPC,D,HT(I))
           TEMPD1 = D * BRAT ! CURRENT DIB
           DUP = D + DGLT ! WITH GROWTH DOB
           TEMPD2 = DUP * BRAT ! WITH GROWTH DIB

           !  LOG OF CHANGE IN DIB SQUARED FOR THE PERIOD
           DDS = LOG(TEMPD2**2 - TEMPD1**2)

           !  RW/GS DEBUG
           if (DEBUG)write(JOSTND,*)'IN DGF - RW/GS DEBUG ',' D=',D,&
                &' PBAL=',PBAL,' PRD=',PRD,' CR=',CR,' SI=',SITEAR(ISPC),&
                &' SLP=',SLOPE*100,' ASP=',ASPECT, ' BRAT=',BRAT,&
                &' DGLT=', DGLT,' DDS=',DDS

           !  APPLY COR AND COR2 TO DDS
           DDS = DDS + COR(ISPC) + LOG(COR2(ISPC))
           if (DEBUG)write(JOSTND,*)'IN DGF - RW/GS DEBUG ',' DDS=',DDS,&
                &' COR(ISPC)=',COR(ISPC),' COR2(ISPC)=',COR2(ISPC)

           !  SPECIES USING ORIGINAL WS EQUATIONS

        case(7)
           DDS = CONSPP + 1.53339*ALD - 0.47442*D*D/1000.&
                &+ 0.35739*CRID - 0.44256*PBAL/(ALOG(D+1.0))/100.&
                &- 0.12359*ALOG(PBA)

        case default
           DDS = CONSPP + DGLDS*ALD + DGCRS*CR + DGCRS2*CR*CR&
                &+ DGDSQS*D*D + DGDBLS*BAL/(ALOG(D+1.0))&
                &+ DGPCFS*PCCF(IPCCF) + DGHAHS*HOAVH + DGBAS*ALBA
           if (ISPC.eq.3.or. ISPC.eq.13) DDS = DDS - 0.15032

           !  JP,DF,WF,RF  LESS THAN 10" USE SEPERATE EQUATION.

           if (D.lt.10.0) then
              select case (ISPC)
              case(6)
                 DDS = CONSJP + 1.23864*ALD + 0.64311*CR - 0.48754*ALBA&
                      &- 0.00189*BAL/(ALOG(D+1.)) - 0.00096*PCCF(IPCCF)
              case(2,3,7,13,22)
                 DDS = CONSJP - 0.52776*ALBA + 1.64163*ALD&
                      &- 0.00205*BAL/(ALOG(D+1.)) - 0.00105*PCCF(IPCCF)
              end select
           end if

           !  BO & TO ARE 5-YR EQNS, NEED TO ADJUST TO A 10-YEAR ESTIMATE.

           if ((ISPC.ge.28.and. ISPC.le.40) .or. ISPC.eq.43) then
              TDDS = EXP(DDS)
              DDS = ALOG(TDDS*2.0)
           end if
        end select

        !  TEST RUNS SHOW DG TO SMALL, ADJUST TO MATCH GROWTH DATA. 3-24-94 GD
        !  VALUE 0.3 IS LN(1.35) --- 1.35 IS AVERAGE CALIBRATION VALUE.

        if (ISPC.eq.5) then
           DDS = DDS+0.30*(0.80+0.004*(SI-50.))
        end if

        if (DDS.lt.-9.21) DDS = -9.21
        WK2(I) = DDS

        !  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.

        if (DEBUG) then
           write(JOSTND,9001) I,ISPC,D,PBAL,CRID,PBA,CONSPP,DDS
           9001 format(' IN DGF, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,&
                &',  PBAL=',F7.2,',  CRID=',F7.4/&
                &'  PBA=',F9.3,',   CONSPP=',F10.4,',  LN(DDS),=',F7.4)
        end if

        ! CHECK TO SEE THAT PREDICTED DDS DOES NOT EXCEED MAX BOUND

        if (ICR(I).le.0.0) go to 9
        select case (ISPC)
        case (1:3,5:8,11,13,18,22,24,28:33,40,42,43)
           DDSMAX = DDSMX1(ISPC) + DDSMX2(ISPC)*ALOG(ICR(I)*DBH(I))
           if (DDS .gt. DDSMAX) then
              DDS = DDSMAX
              if (DEBUG)write(JOSTND,8)I,ISPC
              8 format(' ******WARNING DDS BOUND BEING INVOKED TREE,SPECIES=',&
                   &2I5)
           end if
        end select
        9 continue
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

  !  ENTER LOOP TO LOAD SPECIES DEPENDENT VECTORS.

  ITLAT = NINT(TLAT)
  if (ITLAT .le.35.) ILAT = 1
  if (ITLAT .eq.36.) ILAT = 2
  if (ITLAT .eq.37.) ILAT = 3
  if (ITLAT .eq.38.) ILAT = 4
  if (ITLAT .ge.39.) ILAT = 5

  do ISPC = 1,MAXSP
     ISPFOR = MAPLOC(IFOR,ISPC)
     TEMEL = ELEV
     if (ISPC.eq.41.and. TEMEL.gt.30)TEMEL = 30.

     select case (ISPC)

     case(4,23)
        DGCON(ISPC) = -3.502444 + 0.415435*LOG(SITEAR(ISPC))

     case(7)
        DGCON(ISPC) = DGLAT9(ILAT) - 0.00700*ELEV&
             &- 0.83400*SLOPE*SLOPE + 0.00734*SITEAR(ISPC)

     case default
        TSITE = SITEAR(ISPC)
        if ((ISPC.ge.28.and. ISPC.le.40) .or. ISPC.eq.43)&
             &TSITE = SITEAR(2)
        DGCON(ISPC) = DGFOR(ISPFOR,ISPC)&
             &+ DGEL(ISPC)*TEMEL&
             &+ DGELSQ(ISPC)*TEMEL*TEMEL&
             &+ DGSASP(ISPC)*SIN(ASPECT)*SLOPE&
             &+ DGCASP(ISPC)*COS(ASPECT)*SLOPE&
             &+ DGSLOP(ISPC)*SLOPE&
             &+ DGSLSQ(ISPC)*SLOPE*SLOPE&
             &+ DGSITE(ISPC)*ALOG(TSITE)

     end select

     ATTEN(ISPC) = OBSERV(ISPC)
     SMCON(ISPC) = 0.

     !  IF READCORD OR REUSCORD WAS SPECIFIED (LDCOR2 IS TRUE) ADD
     !  LN(COR2) TO THE BAI MODEL CONSTANT TERM (DGCON).  COR2 IS
     !  INITIALIZED TO 1.0 IN BLKDATA.
     !  RW AND GS DO NOT HAVE COR2 APPLIED IN THIS CODE SEGMENT
     !  SINCE DDS IS NOT DIRECTLY CALCULATED FOR THESE SPECIES.

     if (LDCOR2.and.COR2(ISPC).gt.0.0) then
        if (ISPC .eq.4.or. ISPC .eq.23) then
           DGCON(ISPC) = DGCON(ISPC)
        else
           DGCON(ISPC) = DGCON(ISPC) + ALOG(COR2(ISPC))
        end if
     end if
     30 continue
  end do

  return
end
