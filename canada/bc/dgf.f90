!========================================!
!   Compute large tree diameter growth   !
!========================================!
! CANADA-BC $Id$
subroutine DGF(DIAM)
  implicit none

  !  THIS SUBROUTINE COMPUTES THE VALUE OF DDS (CHANGE IN DIAMETER)
  !  [NOT SQUARED DIAM, AS IN NI, ETC] FOR EACH TREE RECORD, AND LOADS
  !  IT INTO THE ARRAY WK2.  DDS IS PREDICTED FROM HABITAT TYPE, LOCATION, SLOPE,
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
  include 'PLOT.f90'
  include 'METRIC.f90'
  include 'BCPLOT.f90'

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
  !   DGDBAL -- ARRAY CONTAINING COEFFICIENTS FOR THE INTERACTION
  !             BETWEEN BASAL AREA IN LARGER TREES AND LN(DBH) (ONE
  !             COEFFICIENT PER SPECIES).
  !    DGCCF -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
  !             COMPETITION FACTOR TERM IN THE DDS MODEL (ONE
  !             COEFFICIENT FOR EACH SPECIES, LOADED IN RCON).

  !     P_ZL  - LENGTH OF ZONE AND SITE SERIES STRINGS
  !     P_ZN  - NUMBER OF ZONE MODELS
  !     P_ST  - NUMBER OF SITE SERIES STRINGS PER ZONE OR SIBEC GROUP
  !     P_SS  - NUMBER OF SIBEC GROUPS

  ! Variables
  integer :: P_ZL,P_ZN
  integer :: P_ST,P_SS
  parameter (P_ZL = 15)
  parameter (P_ZN = 28)
  parameter (P_ST = 30)
  parameter (P_SS = 25)

  !     STRUCTURE FOR THE FITTED MODELS. COEFFICIENTS FOR EACH FITTED MODEL
  !     ARE FOUND HERE. ONLY ONE OF DBAL1 OR DBAL2 TERMS CAN HAVE A NON-ZERO
  !     VALUE. THE BIOLOGICAL SCOPE OF THE MD_STR OBJECT IS SOMETIMES THE BEC
  !     ZONE, SOMETIMES ALL BEC ZONES COMBINED, AND SOMETIMES THE SITE SERIES.

  TYPE MD_STR
     integer              :: SPP(MAXSP) ! species list
     character (LEN = P_ZL) :: ZONE(P_ST) ! BEC list
     integer              :: OBSERV ! observations
     real                 :: CON ! constant
     real                 :: CASP ! cos-aspect
     real                 :: SASP ! sin-aspect
     real                 :: EL ! elevation
     real                 :: EL2 ! elevation^2
     real                 :: CCFA ! ccf
     real                 :: LD ! diam
     real                 :: DSQ ! diam^2
     real                 :: DBAL1 ! bal/dbh
     real                 :: DBAL2 ! bal/log(dbh+1)
     real                 :: CR ! crown
     real                 :: BAL ! bal
     real                 :: SIGMAR ! residual error
  end TYPE

  !     THE SURROGATE SPECIES (SP2) IS USED WHEN ONE SPECIES IS A 2ND SPECIES
  !     AS A SURROGATE. THIS OCCURS WHEN THERE IS NO AVAILABLE ESTIMATE FOR
  !     THE 1ST SPP

  TYPE SS_STR
     integer              :: SPP(MAXSP) ! species list
     character (LEN = P_ZL) :: PrettyName(P_ST) ! BEC/SS list
     real                 :: CON ! ss coefficient
  end TYPE

  real    :: DIAM(MAXTRE)

  character(len=15) :: PAT1
  character(len=15) :: PAT2
  integer :: I,J,K,M,I1,I2,I3,IP,JP,ISPC,IPAS,iSeries
  integer :: IPOS(MAXSP),JPOS(MAXSP)
  real    :: D,CR,BAL,BALD1,BALD2,DDS,CONSPP,PELEV,D2
  real    :: BRATIO

  logical :: DEBUG
  logical :: LSPPOK(MAXSP)
  real    :: BECADJ(3),SEICN2(MAXSP),SEISLP(MAXSP),SEISAS(MAXSP),SEICAS(MAXSP),SEIELV(MAXSP),&
       &SEIEL2(MAXSP),SEICCF(MAXSP),DGCON1(MAXSP),DGCON2(MAXSP)
  !     v2
  real    :: BA100,DGLDS,DGBALS,DGCRS,DGCRS2,DGDSQS,DGDBLS,RELDN2,ALD
  real    :: DGLD(MAXSP),DGBAL(MAXSP),DGCR(MAXSP),DGCRSQ(MAXSP),DGDBAL(MAXSP),DGHAB(6,MAXSP),DGFOR(6,&
       &MAXSP),DGDS(4,MAXSP),DGEL(MAXSP),DGEL2(MAXSP),DGSASP(MAXSP),DGCASP(MAXSP),DGSLOP(MAXSP),&
       &DGSLSQ(MAXSP),DGCCFA(5,MAXSP),OBSERV(6,MAXSP)
  integer :: MAPHAB(30,MAXSP),MAPDSQ(11,MAXSP),MAPLOC(11,MAXSP),MAPCCF(30,MAXSP)

  data DGLD/&
       &0.56445, 0.54140, 0.56888, 0.68810, 0.68712, 0.58705,&
       &0.89503, 0.73045, 0.86240, 0.66101,&
       &0.89778, 0.89778, 0.89778, 0.56888, 0.89778/
  data DGCR/&
       &1.08338, 1.03478, 2.06850, 1.93969, 1.64133, 1.29360,&
       &1.85558, 1.54643, 0.52044, 1.31618,&
       &1.28403, 1.28403, 1.28403, 2.06850, 1.28403/
  data DGCRSQ/&
       &0.0,     0.07509,-0.62361, -0.78258,-0.27244,     0.0,&
       &-0.36393,-0.26635, 0.86236,  0.0,&
       &0.0,     0.0,     0.0,     -0.62361, 0.0/
  data DGBAL/&
       &0.42112, 0.43637, 0.50202, 0.45142,     0.0, 0.74596,&
       &-0.03665, 0.25639, 0.0,     0.0,&
       &0.0,     0.0,     0.0,     0.50202,     0.0/
  data DGDBAL/&
       &-2.08272,-2.03256,-2.11590,-1.76812,-0.80918,-2.28375,&
       &-0.43329,-1.18218,-0.51270,-1.25881,&
       &-0.66110,-0.66110,-0.66110,-2.11590,-0.66110/
  data  OBSERV/&
       &1635.0,   76.0,   94.0,    0.0,    0.0,    0.0,&
       &968.0,  900.0,  797.0, 1124.0,   47.0,  809.0,&
       &1079.0, 1682.0, 4014.0,   63.0, 4684.0,    0.0,&
       &816.0,   67.0,  122.0, 3678.0,    0.0,    0.0,&
       &1774.0,    0.0,    0.0,    0.0,    0.0,    0.0,&
       &1176.0,   67.0, 1198.0,    0.0,    0.0,    0.0,&
       &93.0,  940.0,   62.0, 2548.0,  719.0, 2590.0,&
       &907.0,  827.0,   46.0,   35.0,  146.0, 3092.0,&
       &988.0,  177.0,   40.0,  331.0,  332.0, 2380.0,&
       &429.0,   59.0,  605.0,   17.0,  879.0,    0.0,&
       &55.0,  880.0,    0.0,    0.0,    0.0,    0.0,&
       &55.0,  880.0,    0.0,    0.0,    0.0,    0.0,&
       &55.0,  880.0,    0.0,    0.0,    0.0,    0.0,&
       &1079.0, 1682.0, 4014.0,   63.0, 4684.0,    0.0,&
       &55.0,  880.0,    0.0,    0.0,    0.0,    0.0/

  !  DGHAB IS AN ARRAY THAT CONTAINS HABITAT CLASS INTERCEPTS FOR
  !  EACH SPECIES.  MAPHAB IS INDEXED BY ITYPE TO MAP HABITAT
  !  TYPES ONTO HABITAT CLASSES.

  data MAPHAB/&
       &12*2, 7*1, 2,2,1,1, 7*2,&
       &9*5, 1,1,2,1, 3*2, 3,3,2,5,2, 3*1, 5,5,4, 3*5,&
       &7*3, 1, 3*3, 8*1, 3,2, 6*3, 2,3,3,&
       &12*2, 1, 8*2, 1,2,2,1, 5*2,&
       &30*1,&
       &14*2, 1,1, 14*2,&
       &4*5, 1,2,1, 4*5, 2,2, 6*3, 4,4,3,4, 3*5, 4,4,5,5,&
       &11*4, 1,1,4,2,2,4,2,1, 8*4, 3,4,4,&
       &12*6, 1,2,3,3,4,3,1, 7*6, 1,5,5,6,&
       &1,1,2,3,3,2,2, 4*3, 3*2, 4*3, 2, 11*3,&
       &21*2, 1, 8*2,&
       &21*2, 1, 8*2,&
       &21*2, 1, 8*2,&
       &7*3, 1, 3*3, 8*1, 3,2, 6*3, 2,3,3,&
       &21*2, 1, 8*2/

  data DGHAB/&
       &1.15584, 1.05635,     0.0,     0.0,     0.0,     0.0,&
       &0.38335, 0.51291, 0.45377, 0.71322, 0.26835,     0.0,&
       &0.47780, 0.15228, 0.29764,     0.0,     0.0,     0.0,&
       &0.66755, 0.60454,     0.0,     0.0,     0.0,     0.0,&
       &0.45264,     0.0,     0.0,     0.0,     0.0,     0.0,&
       &1.61452, 1.31772,     0.0,     0.0,     0.0,     0.0,&
       &0.77399, 0.67828, 0.64451, 0.37945, 0.54337,     0.0,&
       &-0.58842,-0.21235,-0.71629,-0.53954,     0.0,     0.0,&
       &-0.96389,-0.72415,-0.57308,-0.82218,-1.24093,-1.10746,&
       &1.16233, 0.73408, 0.51417,     0.0,     0.0,     0.0,&
       &-1.68033,-1.52111,     0.0,     0.0,     0.0,     0.0,&
       &-1.68033,-1.52111,     0.0,     0.0,     0.0,     0.0,&
       &-1.68033,-1.52111,     0.0,     0.0,     0.0,     0.0,&
       &0.47780, 0.15228, 0.29764,     0.0,     0.0,     0.0,&
       &-1.68033,-1.52111,     0.0,     0.0,     0.0,     0.0/

  !  DGCCFA CONTAINS COEFFICIENTS FOR THE CCF TERM BY SPECIES BY
  !  HABITAT CLASS.  MAPCCF IS INDEXED BY ITYPE TO MAP HABITAT TYPES
  !  ONTO HABITAT CLASSES.

  data MAPCCF/&
       &12*3, 1, 3*3, 1, 4*3, 2,2, 7*3,&
       &11*3, 1, 6*3, 2,3,1,3,3,1, 6*3,&
       &6*4, 1,2, 3*4, 2,1, 3*4, 3, 3*4, 1,3,2, 4*4, 2,4,4,&
       &12*3, 1, 9*3, 2,2, 6*3,&
       &30*1,&
       &13*3, 1,2,2,3,3,2,3,3,1, 8*3,&
       &4,4,1,4,3,4,2,1,4,1, 3*4, 6*2, 4,1,4,1, 3*4, 1, 3*4,&
       &11*4, 3,3, 3*1, 3,3,1,4,2,4,1, 3*4, 1, 3*4,&
       &12*3, 7*1, 3,1,1,2,2,1,3,1,1,3,3,&
       &2,2,3,1,4,3,3,1,2,4,4,1, 4*4, 1, 13*4,&
       &30*1,&
       &30*1,&
       &30*1,&
       &6*4, 1,2, 3*4, 2,1, 3*4, 3, 3*4, 1,3,2, 4*4, 2,4,4,&
       &30*1/

  data DGCCFA/&
       &-0.02430,-0.24886,-0.01079,     0.0,     0.0,&
       &-0.10144,-0.14793,-0.05438,     0.0,     0.0,&
       &-0.09046,-0.11884,-0.05529,-0.02180,     0.0,&
       &-0.09624,-0.19544,-0.05119,     0.0,     0.0,&
       &0.0,     0.0,     0.0,     0.0,     0.0,&
       &-0.05054,-0.15356,-0.09396,     0.0,     0.0,&
       &-0.05576,-0.14919,-0.40640,-0.11400,     0.0,&
       &-0.01547,-0.38386,-0.05371,-0.15159,     0.0,&
       &-0.01598,-0.04477,-0.07392,     0.0,     0.0,&
       &-0.10416,-0.88809,-0.25938,-0.14726,     0.0,&
       &-0.10744,     0.0,     0.0,     0.0,     0.0,&
       &-0.10744,     0.0,     0.0,     0.0,     0.0,&
       &-0.10744,     0.0,     0.0,     0.0,     0.0,&
       &-0.09046,-0.11884,-0.05529,-0.02180,     0.0,&
       &-0.10744,     0.0,     0.0,     0.0,     0.0/

  !  DGFOR CONTAINS LOCATION CLASS CONSTANTS FOR EACH SPECIES.
  !  MAPLOC IS AN ARRAY WHICH MAPS FOREST ONTO A LOCATION CLASS.

  data MAPLOC/&
       &2,1,2,2,2,2,2,2,2,2,1,&
       &1,1,1,2,3,3,2,5,5,4,1,&
       &5,4,1,2,3,3,2,3,5,1,4,&
       &6,5,1,2,2,3,2,4,6,2,5,&
       &3,2,3,1,3,3,3,3,3,3,2,&
       &4,1,1,1,2,2,3,4,2,1,1,&
       &4,2,1,1,2,4,3,3,4,2,2,&
       &3,1,1,1,3,2,3,3,3,1,1,&
       &4,1,1,2,2,3,3,4,4,2,1,&
       &1,2,2,2,1,4,3,1,4,3,2,&
       &3,2,1,1,3,3,3,3,1,3,2,&
       &3,2,1,1,3,3,3,3,1,3,2,&
       &3,2,1,1,3,3,3,3,1,3,2,&
       &5,4,1,2,3,3,2,3,5,1,4,&
       &3,2,1,1,3,3,3,3,1,3,2/

  data DGFOR/&
       &0.16920,     0.0,     0.0,     0.0,     0.0,     0.0,&
       &0.20004, 0.07656, 0.08188, 0.30379,     0.0,     0.0,&
       &0.50357, 0.34920, 0.21961, 0.61812,     0.0,     0.0,&
       &0.43443, 0.28344,-0.14829, 0.20205, 0.57763,     0.0,&
       &0.10667, 0.44357,     0.0,     0.0,     0.0,     0.0,&
       &0.50070, 0.17647, 0.31745,     0.0,     0.0,     0.0,&
       &0.43735, 0.21113, 0.14808,     0.0,     0.0,     0.0,&
       &0.26262,-0.15871,     0.0,     0.0,     0.0,     0.0,&
       &0.42062, 0.14072,-0.13001,     0.0,     0.0,     0.0,&
       &0.24588, 0.56958, 0.42787,     0.0,     0.0,     0.0,&
       &0.12520, 0.48076,     0.0,     0.0,     0.0,     0.0,&
       &0.12520, 0.48076,     0.0,     0.0,     0.0,     0.0,&
       &0.12520, 0.48076,     0.0,     0.0,     0.0,     0.0,&
       &0.50357, 0.34920, 0.21961, 0.61812,     0.0,     0.0,&
       &0.12520, 0.48076,     0.0,     0.0,     0.0,     0.0/

  !  DGDS CONTAINS COEFFICIENTS FOR THE DIAMETER SQUARED TERMS
  !  IN THE DIAMETER INCREMENT MODELS; ARRAYED BY FOREST BY
  !  SPECIES.  MAPDSQ IS AN ARRAY WHICH MAPS FOREST ONTO A DBH**2
  !  COEFFICIENT.

  data MAPDSQ/&
       &1,2,2,2,2,1,2,1,1,1,2,&
       &1,2,1,1,1,1,1,1,1,1,2,&
       &1,4,2,2,2,3,1,4,1,1,4,&
       &1,1,1,1,1,2,2,3,1,2,1,&
       &1,3,1,2,3,1,1,1,1,1,3,&
       &1,1,2,1,1,1,1,2,1,2,1,&
       &1,1,2,2,1,1,2,3,1,4,1,&
       &1,1,2,1,1,1,3,2,1,1,1,&
       &1,2,2,1,2,1,1,2,1,1,2,&
       &1,2,2,2,2,3,3,2,1,1,2,&
       &1,2,1,1,1,1,1,1,2,1,2,&
       &1,2,1,1,1,1,1,1,2,1,2,&
       &1,2,1,1,1,1,1,1,2,1,2,&
       &1,4,2,2,2,3,1,4,1,1,4,&
       &1,2,1,1,1,1,1,1,2,1,2/

  data DGDS/&
       &-0.000439,-0.000004,      0.0,      0.0,&
       &-0.000310,-0.000566,      0.0,      0.0,&
       &-0.000252,-0.000373,-0.000502,-0.000572,&
       &-0.000274,-0.000089,-0.000643,      0.0,&
       &-0.000225,-0.000216,-0.000429,      0.0,&
       &0.0,      0.0,      0.0,      0.0,&
       &-0.001260,-0.002168,-0.001889,-0.000867,&
       &-0.000132,-0.000294,-0.000427,      0.0,&
       &-0.000283,-0.000780,      0.0,      0.0,&
       &-0.000406,-0.000437,-0.000140,      0.0,&
       &-0.000484,-0.000306,      0.0,      0.0,&
       &-0.000484,-0.000306,      0.0,      0.0,&
       &-0.000484,-0.000306,      0.0,      0.0,&
       &-0.000252,-0.000373,-0.000502,-0.000572,&
       &-0.000484,-0.000306,      0.0,      0.0/

  !  DGEL CONTAINS THE COEFFICIENTS FOR THE ELEVATION TERM IN THE
  !  DIAMETER GROWTH EQUATION.  DGEL2 CONTAINS THE COEFFICIENTS FOR
  !  THE ELEVATION SQUARED TERM IN THE DIAMETER GROWTH EQUATION.
  !  DGSASP CONTAINS THE COEFFICIENTS FOR THE SIN(ASPECT)*SLOPE
  !  TERM IN THE DIAMETER GROWTH EQUATION.  DGCASP CONTAINS THE
  !  COEFFICIENTS FOR THE COS(ASPECT)*SLOPE TERM IN THE DIAMETER
  !  GROWTH EQUATION.  DGSLOP CONTAINS THE COEFFICIENTS FOR THE
  !  SLOPE TERM IN THE DIAMETER GROWTH EQUATION.  DGSLSQ CONTAINS
  !  COEFFICIENTS FOR THE (SLOPE)**2 TERM IN THE DIAMETER GROWTH MODELS.
  !  ALL OF THESE ARRAYS ARE SUBSCRIPTED BY SPECIES.

  data DGCASP/&
       &0.09817,-0.21337,-0.04562,-0.01215, 0.08277,-0.06625,&
       &0.00325,-0.13091,-0.12473,-0.09976,&
       &0.17935, 0.17935, 0.17935,-0.04562,0.17935/
  data DGSASP/&
       &0.03876, 0.03430, 0.06287,-0.04595, 0.10987, 0.05534,&
       &0.12993,-0.06038,-0.06862, 0.01192,&
       &0.13363, 0.13363, 0.13363, 0.06287, 0.13363/
  data DGSLOP/&
       &-0.17888, 0.33523, 0.78176, 1.17025, 0.04966, 0.11931,&
       &0.46546, 0.65622, 0.30070,-0.06637,&
       &0.07628, 0.07628, 0.07628, 0.78176, 0.07628/
  data DGSLSQ/&
       &0.0,    -0.70216,-1.12380,-1.52006, 0.0,     0.0,&
       &-0.58014,-0.90143,-0.62224,-0.43720,&
       &0.0,     0.0,     0.0,    -1.12380, 0.0/
  data DGEL/&
       &0.03517, 0.03730, 0.02591, 0.00917, 0.02863,-0.00175,&
       &-0.00480, 0.06259, 0.06313, 0.03229,&
       &0.08518, 0.08518, 0.08518, 0.02591, 0.08518/
  data DGEL2/&
       &-0.000467,-0.000433,-0.000377,-0.000117,-0.000422,-0.000067,&
       &-0.000058,-0.000709,-0.000676,-0.000422,&
       &-0.000943,-0.000943,-0.000943,-0.000377,-0.000943/

  TYPE (BEC_DESCRIPTION) BEC2
  TYPE (MD_STR) ZNKONST(P_ZN)
  TYPE (SS_STR) SSKONST(P_SS)

  data ZNKONST(1) /&
       &MD_STR (& ! PW - ICH+IDF
       &(/1, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'ICH/all        ', 'IDF/all        ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &200,& ! observations
       &4.9231877,& ! constant
       &0.928152,& ! cos-asp
       &0.0,& ! sin-asp
       &-0.929742,& ! elev
       &0.0482885,& ! elev^2
       &0.0,& ! NO CCF
       &0.0297018,& ! diam
       &-0.00058105,& ! diam^2
       &-0.658208,& ! bal/dbh
       &0.0,& ! NO bal/log(dbh+1)
       &0.0,& ! NO CROWN
       &0.0,& ! NO BAL
       &0.89852& ! sigmar
       &) /

  data ZNKONST(2) /&
       &MD_STR (& ! LW - ICH+IDF
       &(/ 2,0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'ICH/all        ', 'IDF/all        ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &739,& ! observations
       &-0.1021,& ! constant
       &0.0,& ! cos-asp
       &0.6201,& ! sin-asp
       &0.0,& ! elev
       &0.0,& ! elev^2
       &0.0,& ! ccf
       &0.0365,& ! diam
       &-0.00084,& ! diam^2
       &-0.8330,& ! bal/dbh
       &0.0,& ! NO bal/log(dbh+1)
       &1.3530,& ! crown
       &0.0,& ! bal
       &0.55480& ! sigmar
       &) /

  data ZNKONST(3) /&
       &MD_STR (& ! FD,OC - ICH 18+21m 1-30
       &(/ 3,14,0,0,0,0,0,0,0,0,0,0,0,0,0 /),& ! spp
       &(/'ICHdk/02       ', 'ICHdk/04       ',&
       &'ICHmk3/02      ', 'ICHmk3/03      ',&
       &'ICHmk3/06      ', 'ICHwk2/02      ',&
       &'ICHwk2/03      ', 'ICHwk2/04      ',&
       &'ICHwk4/02      ', 'ICHwk4/03      ',&
       &'ICHwk4/04      ', 'ICHwk4/05      ',&
       &'ICHmk1/01      ', 'ICHmk1/01-ys   ',&
       &'ICHmk1/02      ', 'ICHmk1/03      ',&
       &'ICHmk1/04      ', 'ICHmk1/05      ',&
       &'ICHmk2/01      ', 'ICHmk2/02      ',&
       &'ICHmk2/03      ', 'ICHmk2/04      ',&
       &'ICHmk2/05      ', 'ICHmw2/02      ',&
       &'ICHmw2/03      ', 'ICHmw3/01      ',&
       &'ICHmw3/01-y    ', 'ICHmw3/02      ',&
       &'ICHmw3/03      ', 'ICHmw3/04      '/),&
       &1683,& ! observations
       &-0.9496,& ! constant
       &0.0,& ! cos-asp
       &0.0,& ! sin-asp
       &0.0,& ! elev
       &0.0,& ! elev^2
       &0.0,& ! ccf
       &0.0674,& ! diam
       &-0.00095,& ! diam^2
       &0.0,& ! bal/dbh
       &0.0,& ! NO bal/log(dbh+1)
       &1.5869,& ! crown
       &-0.0206,& ! bal
       &0.77801& ! sigmar
       &) /

  data ZNKONST(4) /&
       &MD_STR (& ! FD,OC - ICH 18+21m 31-40
       &(/ 3,14,0,0,0,0,0,0,0,0,0,0,0,0,0 /),& ! spp
       &(/'ICHmw3/05      ', 'ICHwk1/03      ',&
       &'ICHdw/01a      ', 'ICHdw/02       ',&
       &'ICHmw1/02      ', 'ICHmw1/03      ',&
       &'ICHmw1/04      ', 'ICHmw2/03      ',&
       &'ICHmw2/04      ', 'ICHxw/01       ',&
       &'ICH/all        ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &1683,& ! observations
       &-0.9496,& ! constant
       &0.0,& ! cos-asp
       &0.0,& ! sin-asp
       &0.0,& ! elev
       &0.0,& ! elev^2
       &0.0,& ! ccf
       &0.0674,& ! diam
       &-0.00095,& ! diam^2
       &0.0,& ! bal/dbh
       &0.0,& ! NO bal/log(dbh+1)
       &1.5869,& ! crown
       &-0.0206,& ! bal
       &0.77801& ! sigmar
       &) /

  data ZNKONST(5) /&
       &MD_STR (& ! FD,OC - ICH 24m 1-29
       &(/ 3,14,0,0,0,0,0,0,0,0,0,0,0,0,0 /),& ! spp
       &(/'ICHdk/01       ', 'ICHdk/05       ',&
       &'ICHmk3/01      ', 'ICHmk3/04      ',&
       &'ICHwk2/01      ', 'ICHwk2/07      ',&
       &'ICHwk4/01      ', 'ICHwk4/07      ',&
       &'ICHmk1/06      ', 'ICHmw2/01      ',&
       &'ICHmw2/01-ys   ', 'ICHmw2/04      ',&
       &'ICHmw2/05      ', 'ICHmw3/06      ',&
       &'ICHmw3/07      ', 'ICHvk1/01      ',&
       &'ICHvk1/02      ', 'ICHvk1/03      ',&
       &'ICHvk1/04      ', 'ICHwk1/01      ',&
       &'ICHwk1/04      ', 'ICHwk1/05      ',&
       &'ICHdw/01b      ', 'ICHdw/03       ',&
       &'ICHdw/04       ', 'ICHmw1/01      ',&
       &'ICHmw1/05      ', 'ICHmw1/06      ',&
       &'ICHmw2/06      ', '               '/),&
       &1367,& ! observations
       &-1.4219,& ! constant
       &0.0,& ! cos-asp
       &0.0,& ! sin-asp
       &0.0,& ! elev
       &0.0,& ! elev^2
       &0.0,& ! ccf
       &0.0802,& ! diam
       &-0.00104,& ! diam^2
       &0.0,& ! bal/dbh
       &0.0,& ! NO bal/log(dbh+1)
       &2.1392,& ! crown
       &-0.0216,& ! bal
       &0.69721& ! sigmar
       &) /

  data ZNKONST(6) /&
       &MD_STR (& ! FD,OC - IDF 9m+12m SI 1-23 (+4)
       &(/ 3, 14,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'IDFdk3/02      ', 'IDFdk3/03      ',&
       &'IDFdk3/04      ', 'IDFdk4/01      ',&
       &'IDFdk4/02      ', 'IDFdk4/03      ',&
       &'IDFdk4/04      ', 'IDFdk4/05      ',&
       &'IDFdk4/07      ', 'IDFxm/02       ',&
       &'IDFxm/03       ', 'IDFxm/04       ',&
       &'IDFxw/02       ', 'IDFxw/03       ',&
       &'IDFxw/04       ', 'IDFdk1/02      ',&
       &'IDFdk2/02      ', 'IDFdm1/03      ',&
       &'IDFww/02       ', 'IDFxh1/02      ',&
       &'IDFxh1/03      ', 'IDFxh2/02      ',&
       &'IDFxh2/03      ',&
       &'IDFdk2/01      ', 'IDFdk2/03      ',& ! +2 from SI 15m
       &'IDFxh2/01      ', 'IDFxh2/04      ',& ! +2 from SI 15m
       &'IDF/all        ',&
       &'               ', '               '/),&
       &1217,& ! observations
       &0.5596,& ! constant
       &0.0,& ! cos-asp
       &0.0,& ! sin-asp
       &0.0,& ! elev
       &0.0,& ! elev^2
       &-0.198,& ! ccf
       &0.0,& ! diam
       &-0.00034,& ! diam^2
       &0.0,& ! NO bal/dbh
       &-0.1045,& ! bal/log(dbh+1)
       &1.3653,& ! crown
       &0.0,& ! bal
       &0.7339& ! sigmar
       &) /

  data ZNKONST(7) /&
       &MD_STR (& ! FD,OC - IDF 15m SI 1-28 (-4)
       &(/ 3, 14,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'IDFdk3/01      ', 'IDFdk3/05      ',&
       &'IDFdk4/09      ', 'IDFdw/01       ',&
       &'IDFxm/01       ', 'IDFxm/05       ',&
       &'IDFxm/06       ', 'IDFxm/07       ',&
       &'IDFxw/01       ', 'IDFxw/05       ',&
       &'IDFdk1/01      ', 'IDFdk1/03      ',&
       &'IDFdk1/04      ',& ! -2 moved to 12 (AAZ)
       &'IDFdk3/06      ', 'IDFdm1/04      ',&
       &'IDFmw1/02      ', 'IDFmw2/02      ',&
       &'IDFxh1/01      ', 'IDFxh1/04      ',&
       &'IDFxh1/05      ',& ! -2 moved to 12 (AAZ)
       &'IDFxh2/05      ', 'IDFxh2/06      ',&
       &'IDFdm2/01      ', 'IDFdm2/03      ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &1432,& ! observations
       &0.2361,& ! constant
       &0.0,& ! cos-asp
       &0.0,& ! sin-asp
       &0.0,& ! elev
       &0.0,& ! elev^2
       &-0.2418,& ! ccf
       &0.0168,& ! diam
       &-0.00033,& ! diam^2
       &0.0,& ! NO bal/dbh
       &0.0,& ! bal/log(dbh+1)
       &1.5811,& ! crown
       &-0.0191,& ! bal
       &0.88944& ! sigmar
       &) /

  data ZNKONST(8) /&
       &MD_STR (& ! FD,OC - IDF 18+21+24m SI 1-30
       &(/ 3, 14,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'IDFdk3/06      ', 'IDFdk3/07      ',&
       &'IDFdk3/08      ', 'IDFxm/08       ',&
       &'IDFxw/06       ', 'IDFxw/07       ',&
       &'IDFdk1/05      ', 'IDFdk1/06      ',&
       &'IDFdk2/04      ', 'IDFdk2/05      ',&
       &'IDFdk2/06      ', 'IDFdm1/01      ',&
       &'IDFdm1/05      ', 'IDFdm1/06      ',&
       &'IDFdm1/07      ', 'IDFmw1/01      ',&
       &'IDFmw1/01-yc   ', 'IDFmw1/03      ',&
       &'IDFmw1/04      ', 'IDFmw1/05      ',&
       &'IDFmw1/06      ', 'IDFmw2/01      ',&
       &'IDFmw2/03      ', 'IDFmw2/04      ',&
       &'IDFww/01       ', 'IDFww/03       ',&
       &'IDFww/04       ', 'IDFww/05       ',&
       &'IDFww/06       ', 'IDFxh1/06      '/),&
       &2767,& ! observations
       &-0.9981,& ! constant
       &0.0,& ! cos-asp
       &0.0,& ! sin-asp
       &0.0,& ! elev
       &0.0,& ! elev^2
       &-0.0817,& ! ccf
       &0.0435,& ! diam
       &-0.00053,& ! diam^2
       &0.0,& ! NO bal/dbh
       &0.0,& ! bal/log(dbh+1)
       &2.2233,& ! crown
       &-0.0206,& ! bal
       &0.74424& ! sigmar
       &) /

  data ZNKONST(9) /&
       &MD_STR (& ! FD,OC - IDF 18+21m+24m SI 31-37
       &(/ 3, 14,0,0,0,0,0,0,0,0,0,0,0,0,0/),& ! spp
       &(/'IDFxh1/07      ', 'IDFxh1/08      ',&
       &'IDFxh2/07      ', 'IDFxh2/08      ',&
       &'IDFdm2/04      ', 'IDFdm2/05      ',&
       &'IDFdm2/07      ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &2767,& ! observations
       &-0.9981,& ! constant
       &0.0,& ! cos-asp
       &0.0,& ! sin-asp
       &0.0,& ! elev
       &0.0,& ! elev^2
       &-0.0817,& ! ccf
       &0.0435,& ! diam
       &-0.00053,& ! diam^2
       &0.0,& ! NO bal/dbh
       &0.0,& ! bal/log(dbh+1)
       &2.2233,& ! crown
       &-0.0206,& ! bal
       &0.74424& ! sigmar
       &) /

  data ZNKONST(10) /&
       &MD_STR (& ! HW - ICH+IDF
       &(/ 5, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'ICH/all        ', 'IDF/all        ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &959,& ! observations
       &-0.3410,& ! constant
       &0.0,& ! cos-asp
       &0.0,& ! sin-asp
       &0.0,& ! elev
       &0.0,& ! elev^2
       &-0.1307,& ! ccf
       &0.0605,& ! diam
       &-0.00087,& ! diam^2
       &0.0,& ! bal/dbh
       &0.0,& ! NO bal/log(dbh+1)
       &1.5816,& ! crown
       &-0.0119,& ! bal
       &0.88775& ! sigmar
       &) /

  data ZNKONST(11) /&
       &MD_STR (& ! CW - ICH
       &(/ 6, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),& ! spp
       &(/'ICH/all        ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &2756,& ! observations
       &-0.1650,& ! constant
       &0.2763,& ! cos-asp
       &0.0,& ! sin-asp
       &0.0,& ! elev
       &0.0,& ! elev^2
       &-0.1820,& ! ccf
       &0.0372,& ! diam
       &-0.00037,& ! diam^2
       &-0.0884,& ! bal/dbh
       &0.0,& ! NO bal/log(dbh+1)
       &1.4487,& ! crown
       &0.0,& ! bal
       &0.86261& ! sigmar
       &) /

  data ZNKONST(12) /&
       &MD_STR (& ! CW - IDF
       &(/ 6, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'IDF/all        ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &357,& ! observations
       &0.656272,& ! constant
       &-1.027327,& ! cos-asp
       &0.0,& ! sin-asp
       &0.0,& ! elev
       &0.0,& ! elev^2
       &0.0,& ! ccf
       &0.089231,& ! diam
       &-0.0016866,& ! diam^2
       &0.0,& ! NO bal/dbh
       &-0.0432885,& ! bal/log(dbh+1)
       &0.0,& ! crown
       &0.0,& ! NO BAL
       &0.88265& ! sigmar
       &) /

  data ZNKONST(13) /&
       &MD_STR (& ! PL - ICH
       &(/ 7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'ICH/all        ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &1313,& ! observations
       &0.0,& ! constant
       &0.0,& ! cos-asp
       &0.0,& ! sin-asp
       &0.0,& ! elev
       &0.0,& ! elev^2
       &0.0,& ! ccf
       &0.0209,& ! diam
       &-0.00067,& ! diam^2
       &-0.5552,& ! bal/dbh
       &0.0,& ! NO bal/log(dbh+1)
       &1.8153,& ! crown
       &0.0,& ! bal
       &0.72326& ! sigmar
       &) /

  data ZNKONST(14) /&
       &MD_STR (& ! PL - IDF
       &(/ 7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'IDF/all        ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &2009,& ! observations
       &-0.2238,& ! constant
       &0.0,& ! cos-asp
       &0.0,& ! sin-asp
       &0.0,& ! elev
       &0.0,& ! elev^2
       &-0.3124,& ! ccf
       &0.00993,& ! diam
       &-0.00038,& ! diam^2
       &0.0,& ! NO bal/dbh
       &-0.0433,& ! bal/log(dbh+1)
       &1.5667,& ! crown
       &0.0,& ! NO BAL
       &0.6249& ! sigmar
       &) /

  data ZNKONST(15) /&
       &MD_STR (& ! SE - ICH
       &(/ 8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'ICH/all        ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &424,& ! observations
       &-0.5766,& ! constant
       &0.0,& ! cos-asp
       &0.0,& ! sin-asp
       &0.0,& ! elev
       &0.0,& ! elev^2
       &-0.0825,& ! ccf
       &0.0196,& ! diam
       &-0.00028,& ! diam^2
       &0.0,& ! bal/dbh
       &0.0,& ! NO bal/log(dbh+1)
       &1.9456,& ! crown
       &-0.00866,& ! bal
       &0.87288& ! sigmar
       &) /

  data ZNKONST(16) /&
       &MD_STR (& ! SE - IDF
       &(/ 8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'IDF/all        ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &102,& ! observations
       &-0.8694668,& ! constant
       &0.0,& ! cos-asp
       &0.0,& ! sin-asp
       &0.0,& ! elev
       &0.0,& ! elev^2
       &0.0,& ! ccf
       &0.0214183,& ! diam
       &-0.00090472,& ! diam^2
       &0.0,& ! NO bal/dbh
       &-0.1589753,& ! bal/log(dbh+1)
       &3.3882046,& ! crown
       &0.0,& ! NO BAL
       &0.84611& ! sigmar
       &) /

  data ZNKONST(17) /&
       &MD_STR (& ! BL,BG - ICH+IDF
       &(/ 9, 4, 0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'ICH/all        ', 'IDF/all        ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &350,& ! observations
       &0.6265,& ! constant
       &0.0,& ! cos-asp
       &0.0,& ! sin-asp
       &0.0,& ! elev
       &0.0,& ! elev^2
       &-1.0897,& ! ccf
       &0.0515,& ! diam
       &-0.00084,& ! diam^2
       &0.0,& ! bal/dbh
       &0.0,& ! NO bal/log(dbh+1)
       &1.5198,& ! crown
       &0.0,& ! bal
       &0.92423& ! sigmar
       &) /

  data ZNKONST(18) /&
       &MD_STR (& ! PY - IDF+ICH
       &(/ 10, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'ICH/all        ', 'IDF/all        ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &450,& ! observations
       &-0.50298,& ! constant
       &-0.613773,& ! cos-asp
       &0.596886,& ! sin-asp
       &0.0,& ! elev
       &0.0,& ! elev^2
       &0.0,& ! NO CCF
       &0.0862013,& ! diam
       &-0.00115276,& ! diam^2
       &-0.609809,& ! bal/dbh
       &0.0,& ! NO bal/log(dbh+1)
       &0.0,& ! NO CROWN
       &0.0,& ! NO BAL
       &0.86319& ! sigmar
       &) /

  data ZNKONST(19) /&
       &MD_STR (& ! EP,OH - ICH
       &(/11,15, 0,0,0,0,0,0,0,0,0,0,0,0,0 /),& ! spp
       &(/'ICH/all        ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &835,& ! observations
       &-3.6197,& ! constant
       &0.0,& ! cos-asp
       &0.0,& ! sin-asp
       &0.7542,& ! elev
       &-0.0425,& ! elev^2
       &-0.2492,& ! ccf
       &0.0740,& ! diam
       &-0.00116,& ! diam^2
       &0.0,& ! bal/dbh
       &0.0,& ! NO bal/log(dbh+1)
       &1.1239,& ! crown
       &-0.0198,& ! bal
       &0.68942& ! sigmar
       &) /

  data ZNKONST(20) /&
       &MD_STR (& ! EP,OH - IDF
       &(/11,15, 0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'IDF/all        ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &418,& ! observations
       &0.0,& ! constant
       &0.0,& ! cos-asp
       &0.0,& ! sin-asp
       &0.0,& ! elev
       &0.0,& ! elev^2
       &0.0,& ! ccf
       &1.1315,& ! diam
       &-0.0012,& ! diam^2
       &0.3599,& ! additive bal/dbh
       &-0.2306,& ! multiplicative bal/dbh
       &0.0,& ! crown
       &0.0,& ! NO BAL
       &0.78715& ! sigmar
       &) /

  !       ASPEN MODEL HAS DIFFERENT STRUCTURE...

  data ZNKONST(21) /&
       &MD_STR (& ! AT,CT - IDF+ICH
       &(/ 12,13, 0,0,0,0,0,0,0,0,0,0,0,0,0 /),& ! spp
       &(/'ICH/all        ', 'IDF/all        ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &727,& ! observations
       &0.0,& ! constant
       &0.0,& ! cos-asp
       &0.0,& ! sin-asp
       &0.0,& ! elev
       &0.0,& ! elev^2
       &0.0,& ! NO CCF
       &0.1372,& ! diam
       &-0.0015,& ! diam^2
       &1.0768,& ! additive bal/dbh
       &-0.1983,& ! multiplicative bal/dbh
       &0.0,& ! NO CROWN
       &0.0,& ! NO BAL
       &0.78243& ! sigmar
       &) /

  !       SBS and SBPS zones...

  data ZNKONST(22) /&
       &MD_STR (& ! Pl SBS
       &(/7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'SBSdw1/01      ', 'SBSdw2/01      ',&
       &'SBSmc1/01      ', 'SBSmc2/01      ',&
       &'SBSmh/01       ', 'SBSmm/01       ',&
       &'SBSmw/01       ', 'SBSwk1/01      ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &404,& ! observations
       &-1.2421,& ! constant
       &0.0,& ! cos-asp
       &0.00955,& ! sin-asp
       &0.0,& ! elev
       &0.0,& ! elev^2
       &0.0,& ! ccf
       &0.0630,& ! diam                     KB?
       &-0.0012,& ! diam^2
       &0.0,& ! bal/dbh
       &0.0,& ! NO bal/log(dbh+1)
       &1.6043,& ! crown
       &-0.0202,& ! bal
       &0.419& ! sigmar
       &) /

  data ZNKONST(23) /&
       &MD_STR (& ! Pl SBPSdc
       &(/7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'SBPSdc/01      ', 'SBPSxc/01      ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &38,& ! observations
       &-4.1693,& ! constant
       &0.0,& ! cos-asp
       &0.0,& ! sin-asp
       &0.0,& ! elev
       &0.0,& ! elev^2
       &-0.00832,& ! ccf
       &2.4409,& ! diam
       &-0.00565,& ! diam^2
       &0.0,& ! bal/dbh
       &0.0,& ! NO bal/log(dbh+1)
       &1.3229,& ! crown
       &0.0,& ! bal
       &0.893& ! sigmar
       &) /

  data ZNKONST(24) /&
       &MD_STR (& ! Pl SBPSmk
       &(/7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'SBPSmc/01      ', 'SBPSmk/01      ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &28,& ! observations
       &-0.9005,& ! constant
       &0.0,& ! cos-asp
       &0.0,& ! sin-asp
       &0.0,& ! elev
       &0.0,& ! elev^2
       &-0.00414,& ! ccf
       &0.7488,& ! diam
       &-0.00075,& ! diam^2
       &0.0,& ! bal/dbh
       &0.0,& ! NO bal/log(dbh+1)
       &1.3599,& ! crown
       &0.0,& ! bal
       &0.314& ! sigmar
       &) /

  data ZNKONST(25) /&
       &MD_STR (& ! AT - SBPSmk, SBPSdc
       &(/12, 15, 0,0,0,0,0,0,0,0,0,0,0,0,0 /),& ! spp
       &(/'SBPSmc/01      ', 'SBPSmk/01      ',&
       &'SBPSdc/01      ', 'SBPSxc/01      ',&
       &'SBSdw1/01      ', 'SBSdw2/01      ',&
       &'SBSmc1/01      ', 'SBSmc2/01      ',&
       &'SBSmh/01       ', 'SBSmm/01       ',&
       &'SBSmw/01       ', 'SBSwk1/01      ',&
       &'SBS/all        ', 'SBPS/all       ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &27,& ! observations
       &1.1487,& ! constant
       &0.0,& ! cos-asp
       &0.0,& ! sin-asp
       &0.0,& ! elev
       &0.0,& ! elev^2
       &-0.00310,& ! ccf
       &-0.0668,& ! diam
       &0.0,& ! diam^2
       &0.0,& ! additive bal/dbh
       &0.0,& ! multiplicative bal/dbh
       &0.0,& ! crown
       &0.0,& ! bal
       &0.311& ! sigmar
       &) /

  data ZNKONST(26) /&
       &MD_STR (& ! Bl SBS
       &(/9, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'SBPSmc/01      ', 'SBPSmk/01      ',&
       &'SBPSdc/01      ', 'SBPSxc/01      ',&
       &'SBSdw1/01      ', 'SBSdw2/01      ',&
       &'SBSmc1/01      ', 'SBSmc2/01      ',&
       &'SBSmh/01       ', 'SBSmm/01       ',&
       &'SBSmw/01       ', 'SBSwk1/01      ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &456,& ! observations            KB?
       &0.0,& ! constant
       &0.0,& ! cos-asp
       &0.0,& ! sin-asp
       &0.0,& ! elev
       &0.0,& ! elev^2
       &-0.0152,& ! ccf
       &0.0737,& ! diam                    KB?
       &-0.00182,& ! diam^2
       &0.0,& ! bal/dbh
       &0.0,& ! NO bal/log(dbh+1)
       &3.1925,& ! crown
       &0.0,& ! bal
       &0.0& ! sigmar                  KB?
       &) /

  data ZNKONST(27) /&
       &MD_STR (& ! Sx SBSdw1, SBSdw2, SBSmh
       &(/8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'SBPSmc/01      ', 'SBPSmk/01      ',&
       &'SBPSdc/01      ', 'SBPSxc/01      ',&
       &'SBSdw1/01      ', 'SBSdw2/01      ',&
       &'SBSmc1/01      ', 'SBSmc2/01      ',&
       &'SBSmh/01       ', 'SBSmm/01       ',&
       &'SBSmw/01       ', 'SBSwk1/01      ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &501,& ! observations
       &0.9159,& ! constant
       &0.0,& ! cos-asp
       &0.0,& ! sin-asp
       &0.0,& ! elev
       &0.0,& ! elev^2
       &-0.00397,& ! ccf
       &0.00620,& ! diam
       &-0.00009,& ! diam^2
       &0.0,& ! bal/dbh
       &0.0,& ! NO bal/log(dbh+1)
       &0.5586,& ! crown
       &0.0,& ! bal
       &0.786& ! sigmar
       &) /

  data ZNKONST(28) /&
       &MD_STR (& ! Fd SBSdw1, SBSdw2, SBSmh
       &(/3, 14, 0,0,0,0,0,0,0,0,0,0,0,0,0 /),& ! spp
       &(/'SBPSmc/01      ', 'SBPSmk/01      ',&
       &'SBPSdc/01      ', 'SBPSxc/01      ',&
       &'SBSdw1/01      ', 'SBSdw2/01      ',&
       &'SBSmc1/01      ', 'SBSmc2/01      ',&
       &'SBSmh/01       ', 'SBSmm/01       ',&
       &'SBSmw/01       ', 'SBSwk1/01      ',&
       &'SBS/all        ', 'SBPS/all       ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &568,& ! observations
       &1.0901,& ! constant
       &0.0,& ! cos-asp
       &0.0,& ! sin-asp
       &0.0,& ! elev
       &0.0,& ! elev^2
       &-0.00403,& ! ccf
       &0.0144,& ! diam
       &-0.0001,& ! diam^2
       &0.0,& ! bal/dbh
       &0.0,& ! NO bal/log(dbh+1)
       &0.05062,& ! crown
       &-0.00858,& ! bal
       &0.855& ! sigmar
       &) /

  !     SITE SERIES COEFFICIENTS

  data SSKONST(1) /&
       &SS_STR (& ! ICH+IDF PW (zonal model: no site index)
       &(/ 1, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'ICH/all        ', 'IDF/all        ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &0.0&
       &) /

  data SSKONST(2) /&
       &SS_STR (& ! ICH+IDF LW (zonal model; rare in IDF: no site index)
       &(/ 2, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'ICH/all        ', 'IDF/all        ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &0.0&
       &) /

  data SSKONST(3) /&
       &SS_STR (& ! ICH+IDF FD,OC (model coeffs incorporate SIBEC)
       &(/ 3,14,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'ICH/all        ', 'IDF/all        ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &0.0&
       &) /

  data SSKONST(4) /&
       &SS_STR (& ! ICH HW 15m 1-18 (should be absent in IDF)
       &(/ 5, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'ICHmw2/02      ', 'ICHmw2/06      ',&
       &'ICHmw2/07      ', 'ICHmw3/08      ',&
       &'ICHvk1/05      ', 'ICHvk1/06      ',&
       &'ICHwk1/02      ', 'ICHwk1/03      ',&
       &'ICHwk1/06      ', 'ICHwk1/07      ',&
       &'ICHmw1/02      ', 'ICHmw1/03      ',&
       &'ICHmw1/04      ', 'ICHmw1/07      ',&
       &'ICHmw2/03      ', 'ICHmw2/08      ',&
       &'ICHmw3/04      ', 'ICHwk1/08      ',&
       &'ICH/all        ', 'IDF/all        ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &0.0&
       &) /

  data SSKONST(5) /&
       &SS_STR (& ! ICH HW 18m 1-28
       &(/ 5, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'ICHmw2/01      ', 'ICHmw2/01-ys   ',&
       &'ICHmw2/03      ', 'ICHmw2/04      ',&
       &'ICHmw2/05      ', 'ICHmw3/01      ',&
       &'ICHmw3/01-yc   ', 'ICHmw3/05      ',&
       &'ICHmw3/06      ', 'ICHmw3/07      ',&
       &'ICHvk1/01      ', 'ICHvk1/02      ',&
       &'ICHvk1/03      ', 'ICHvk1/04      ',&
       &'ICHwk1/01      ', 'ICHwk1/04      ',&
       &'ICHwk1/05      ', 'ICHdw/01a      ',&
       &'ICHdw/01b      ', 'ICHdw/02       ',&
       &'ICHdw/03       ', 'ICHdw/04       ',&
       &'ICHmw1/01      ', 'ICHmw1/05      ',&
       &'ICHmw1/06      ', 'ICHmw2/05      ',&
       &'ICHmw2/06      ', 'ICHxw/01       ',&
       &'               ', '               '/),&
       &0.0&
       &) /

  data SSKONST(6) /&
       &SS_STR (& ! ICH CW 15m 1-28
       &(/ 6, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'ICHmk1/01      ', 'ICHmk1/01-ys   ',&
       &'ICHmk1/04      ', 'ICHmk1/07      ',&
       &'ICHmk2/01      ', 'ICHmk2/04      ',&
       &'ICHmk2/06      ', 'ICHmw2/02      ',&
       &'ICHmw3/01      ', 'ICHmw3/01-yc   ',&
       &'ICHmw3/03      ', 'ICHmw3/04      ',&
       &'ICHmw3/05      ', 'ICHmw3/08      ',&
       &'ICHvk1/05      ', 'ICHvk1/06      ',&
       &'ICHwk1/02      ', 'ICHwk1/03      ',&
       &'ICHwk1/06      ', 'ICHwk1/07      ',&
       &'ICHdw/01a      ', 'ICHmw1/02      ',&
       &'ICHmw1/03      ', 'ICHmw1/04      ',&
       &'ICHmw1/07      ', 'ICHmw2/03      ',&
       &'ICHwk1/08      ', 'ICHxw/01       ',&
       &'ICH/all        ', '               '/),&
       &0.0&
       &) /

  data SSKONST(7) /&
       &SS_STR (& ! ICH CW 18m 1-25
       &(/ 6, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'ICHmk1/05      ', 'ICHmk1/06      ',&
       &'ICHmk2/05      ', 'ICHmw2/01      ',&
       &'ICHmw2/01-ys   ', 'ICHmw2/03      ',&
       &'ICHmw2/04      ', 'ICHmw2/05      ',&
       &'ICHmw3/06      ', 'ICHmw3/07      ',&
       &'ICHvk1/01      ', 'ICHvk1/03      ',&
       &'ICHvk1/04      ', 'ICHwk1/01      ',&
       &'ICHwk1/04      ', 'ICHwk1/05      ',&
       &'ICHdw/01b      ', 'ICHdw/02       ',&
       &'ICHdw/03       ', 'ICHdw/04       ',&
       &'ICHmw1/01      ', 'ICHmw1/05      ',&
       &'ICHmw1/06      ', 'ICHmw2/04      ',&
       &'ICHmw2/06      ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &0.0466&
       &) /

  data SSKONST(8) /&
       &SS_STR (& ! IDF CW (zonal model: no site index)
       &(/ 6, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'IDF/all        ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &0.0&
       &) /

  data SSKONST(9) /&
       &SS_STR (& ! ICH PL 21m 1-30
       &(/ 7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'ICHdk/02       ', 'ICHdk/03       ',&
       &'ICHdk/04       ', 'ICHmk3/02      ',&
       &'ICHmk3/03      ', 'ICHmk3/05      ',&
       &'ICHmk3/06      ', 'ICHmk3/07      ',&
       &'ICHwk2/02      ', 'ICHwk2/03      ',&
       &'ICHwk2/04      ', 'ICHwk2/05      ',&
       &'ICHwk2/06      ', 'ICHwk2/07      ',&
       &'ICHwk4/02      ', 'ICHwk4/03      ',&
       &'ICHwk4/04      ', 'ICHwk4/05      ',&
       &'ICHwk4/06      ', 'ICHwk4/07      ',&
       &'ICHmk1/01      ', 'ICHmk1/01-ys   ',&
       &'ICHmk1/02      ', 'ICHmk1/03      ',&
       &'ICHmk1/04      ', 'ICHmk1/05      ',&
       &'ICHmk1/07      ', 'ICHmk2/01      ',&
       &'ICHmk2/02      ', 'ICHmk2/03      '/),&
       &0.0&
       &) /

  data SSKONST(10) /&
       &SS_STR (& ! ICH PL 21m 31-50
       &(/ 7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'ICHmk2/04      ', 'ICHmk2/06      ',&
       &'ICHmw2/02      ', 'ICHmw3/01      ',&
       &'ICHmw3/01-yc   ', 'ICHmw3/02      ',&
       &'ICHmw3/03      ', 'ICHmw3/04      ',&
       &'ICHmw3/05      ', 'ICHwk1/02      ',&
       &'ICHwk1/03      ', 'ICHwk1/04      ',&
       &'ICHdw/01a      ', 'ICHdw/02       ',&
       &'ICHmw1/02      ', 'ICHmw1/03      ',&
       &'ICHmw1/04      ', 'ICHmw1/07      ',&
       &'ICHmw2/03      ', 'ICHxw/01       ',&
       &'ICH/all        ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &0.0&
       &) /

  data SSKONST(11) /&
       &SS_STR (& ! ICH PL 24m 1-22
       &(/ 7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'ICHdk/01       ', 'ICHdk/05       ',&
       &'ICHmk3/01      ', 'ICHmk3/04      ',&
       &'ICHwk2/01      ', 'ICHwk4/01      ',&
       &'ICHmk1/06      ', 'ICHmk2/05      ',&
       &'ICHmw2/01      ', 'ICHmw2/01-ys   ',&
       &'ICHmw2/03      ', 'ICHmw2/04      ',&
       &'ICHmw2/05      ', 'ICHmw3/06      ',&
       &'ICHwk1/01      ', 'ICHdw/01b      ',&
       &'ICHdw/03       ', 'ICHdw/04       ',&
       &'ICHmw1/01      ', 'ICHmw1/05      ',&
       &'ICHmw1/06      ', 'ICHmw2/06      ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &0.2625&
       &) /

  data SSKONST(12) /&
       &SS_STR (& ! IDF PL 15m 1-20
       &(/ 7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'IDFdk3/02      ', 'IDFdk3/03      ',&
       &'IDFdk3/05      ', 'IDFdk4/01      ',&
       &'IDFdk4/02      ', 'IDFdk4/05      ',&
       &'IDFdk4/06      ', 'IDFdk4/07      ',&
       &'IDFdk4/08      ', 'IDFdk4/09      ',&
       &'IDFxm/03       ', 'IDFdk1/02      ',&
       &'IDFdk1/03      ', 'IDFdk1/04      ',&
       &'IDFdk3/06      ', 'IDFdm1/03      ',&
       &'IDFdm1/04      ', 'IDFmw1/03      ',&
       &'IDFww/02       ', 'IDFdm2/03      ',&
       &'IDF/all        ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &0.0&
       &) /

  data SSKONST(13) /&
       &SS_STR (& ! IDF PL 18m 1-18
       &(/ 7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'IDFdk3/01      ', 'IDFdk3/06      ',&
       &'IDFdk3/07      ', 'IDFdk3/08      ',&
       &'IDFdk3/09      ', 'IDFdw/01       ',&
       &'IDFdk1/01      ', 'IDFdk2/01      ',&
       &'IDFdk2/03      ', 'IDFdk2/04      ',&
       &'IDFdm1/01      ', 'IDFmw1/04      ',&
       &'IDFmw2/02      ', 'IDFmw2/05      ',&
       &'IDFww/03       ', 'IDFdm2/01      ',&
       &'IDFdm2/04      ', 'IDFdm2/07      ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &0.0&
       &) /

  data SSKONST(14) /&
       &SS_STR (& ! IDF PL 21m 1-17
       &(/ 7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'IDFdk1/05      ', 'IDFdk1/06      ',&
       &'IDFdk2/05      ', 'IDFdk2/06      ',&
       &'IDFdk2/07      ', 'IDFdm1/05      ',&
       &'IDFdm1/06      ', 'IDFdm1/07      ',&
       &'IDFmw1/01      ', 'IDFmw1/01-yc   ',&
       &'IDFmw1/05      ', 'IDFmw1/06      ',&
       &'IDFmw2/01      ', 'IDFmw2/03      ',&
       &'IDFmw2/04      ', 'IDFww/01       ',&
       &'IDFdm2/05      ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &0.3479&
       &) /

  data SSKONST(15) /&
       &SS_STR (& ! ICH SE 18m 1-30
       &(/ 8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'ICHwk2/04      ', 'ICHwk4/04      ',&
       &'ICHwk4/05      ', 'ICHwk4/08      ',&
       &'ICHmk1/01      ', 'ICHmk1/01-ys   ',&
       &'ICHmk1/03      ', 'ICHmk1/04      ',&
       &'ICHmk1/07      ', 'ICHmk2/01      ',&
       &'ICHmk2/03      ', 'ICHmk2/04      ',&
       &'ICHmk2/06      ', 'ICHmw2/01      ',&
       &'ICHmw2/01-ys   ', 'ICHmw2/02      ',&
       &'ICHmw2/03      ', 'ICHmw2/07      ',&
       &'ICHmw3/01      ', 'ICHmw3/01-yc   ',&
       &'ICHmw3/03      ', 'ICHmw3/04      ',&
       &'ICHmw3/05      ', 'ICHmw3/08      ',&
       &'ICHvk1/06      ', 'ICHwk1/03      ',&
       &'ICHwk1/07      ', 'ICHmw1/02      ',&
       &'ICHmw1/03      ', 'ICHmw1/04      '/),&
       &0.0&
       &) /

  data SSKONST(16) /&
       &SS_STR (& ! ICH SE 18m 31-35
       &(/ 8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'ICHmw1/07      ', 'ICHmw2/03      ',&
       &'ICHmw2/04      ', 'ICHmw2/08      ',&
       &'ICHwk1/08      ', 'ICH/all        ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &0.0&
       &) /

  data SSKONST(17) /&
       &SS_STR (& ! ICH SE 21m 1-30
       &(/ 8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'ICHdk/01       ', 'ICHdk/04       ',&
       &'ICHdk/05       ', 'ICHmk3/01      ',&
       &'ICHmk3/04      ', 'ICHmk3/05      ',&
       &'ICHmk3/06      ', 'ICHmk3/07      ',&
       &'ICHwk2/01      ', 'ICHwk2/05      ',&
       &'ICHwk2/06      ', 'ICHwk2/07      ',&
       &'ICHwk2/08      ', 'ICHwk4/01      ',&
       &'ICHwk4/06      ', 'ICHwk4/07      ',&
       &'ICHmk1/05      ', 'ICHmk1/06      ',&
       &'ICHmk2/05      ', 'ICHmw2/04      ',&
       &'ICHmw2/05      ', 'ICHmw2/06      ',&
       &'ICHmw3/06      ', 'ICHmw3/07      ',&
       &'ICHvk1/01      ', 'ICHvk1/02      ',&
       &'ICHvk1/03      ', 'ICHvk1/04      ',&
       &'ICHvk1/05      ', 'ICHwk1/01      '/),&
       &0.1750&
       &) /

  data SSKONST(18) /&
       &SS_STR (& ! ICH SE 21m 31-41
       &(/ 8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'ICHwk1/04      ', 'ICHwk1/05      ',&
       &'ICHwk1/06      ', 'ICHdw/01b      ',&
       &'ICHdw/03       ', 'ICHdw/04       ',&
       &'ICHmw1/01      ', 'ICHmw1/05      ',&
       &'ICHmw1/06      ', 'ICHmw2/07      ',&
       &'ICHwk1/07      ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &0.1750&
       &) /

  data SSKONST(19) /&
       &SS_STR (& ! IDF SE (zonal model: no site index)
       &(/ 8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'IDF/all        ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &0.0&
       &) /

  data SSKONST(20) /&
       &SS_STR (& ! ICH BL,BG 15m 1-9
       &(/ 9, 4 ,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'ICHmk1/03      ', 'ICHmk2/03      ',&
       &'ICHmw2/02      ', 'ICHmw3/03      ',&
       &'ICHmw3/08      ', 'ICHvk1/06      ',&
       &'ICHwk1/02      ', 'ICHmw1/02      ',&
       &'ICHmw2/03      ', 'ICH/all        ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &0.0&
       &) /

  data SSKONST(21) /&
       &SS_STR (& ! ICH BL,BG 18m 1-29
       &(/ 9, 4, 0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'ICHmk1/01      ', 'ICHmk1/01-ys   ',&
       &'ICHmk1/04      ', 'ICHmk1/07      ',&
       &'ICHmk2/01      ', 'ICHmk2/06      ',&
       &'ICHmw2/01      ', 'ICHmw2/01-ys   ',&
       &'ICHmw2/03      ', 'ICHmw2/06      ',&
       &'ICHmw2/07      ', 'ICHmw3/01      ',&
       &'ICHmw3/01-yc   ', 'ICHmw3/05      ',&
       &'ICHvk1/02      ', 'ICHvk1/03      ',&
       &'ICHvk1/05      ', 'ICHwk1/03      ',&
       &'ICHwk1/04      ', 'ICHwk1/06      ',&
       &'ICHwk1/07      ', 'ICHdw/01a      ',&
       &'ICHmw1/01      ', 'ICHmw1/03      ',&
       &'ICHmw1/04      ', 'ICHmw1/07      ',&
       &'ICHmw2/04      ', 'ICHmw2/08      ',&
       &'ICHwk1/08      ', '               '/),&
       &0.0807&
       &) /

  data SSKONST(22) /&
       &SS_STR (& ! ICH BL,BG 21m 1-18
       &(/ 9, 4, 0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'ICHmk1/05      ', 'ICHmk1/06      ',&
       &'ICHmk2/05      ', 'ICHmw2/04      ',&
       &'ICHmw2/05      ', 'ICHmw3/06      ',&
       &'ICHmw3/07      ', 'ICHvk1/01      ',&
       &'ICHvk1/04      ', 'ICHwk1/01      ',&
       &'ICHwk1/05      ', 'ICHdw/01b      ',&
       &'ICHdw/03       ', 'ICHdw/04       ',&
       &'ICHmw1/05      ', 'ICHmw1/06      ',&
       &'ICHmw2/05      ', 'ICHmw2/06      ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &0.2422&
       &) /

  data SSKONST(23) /&
       &SS_STR (& ! IDF BL,BG 1-5 (zonal model: use ICH SI 15)
       &(/ 9, 4, 0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'IDF/all        ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &0.0&
       &) /

  data SSKONST(24) /&
       &SS_STR (& ! ICH+IDF PY 1-30 (zonal model: no site index)
       &(/10, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'IDF/all        ', 'ICH/all        ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &0.0&
       &) /

  data SSKONST(25) /&
       &SS_STR (& ! ICH+IDF EP,OH,AT,AC (zonal model for all hardwoords)
       &(/11, 12, 13, 15, 0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
       &(/'IDF/all        ', 'ICH/all        ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &0.0&
       &) /

  call DBCHK (DEBUG,'DGF',3,ICYC)

  !     V2: SCALE BASAL AREA; FIND SEICCF TERMS
  if (LV2ATV) then
     BA100 = BA/100.
     call SEILTDG (BECADJ,SEICN2,&
          &SEISLP,SEISAS,SEICAS,SEIELV,SEIEL2,SEICCF,LSPPOK)
  end if

  !     BEGIN SPECIES LOOP. ASSIGN VARIABLES WHICH ARE SPECIES DEPENDENT

  do ISPC = 1,MAXSP

     I1 = ISCT(ISPC,1)
     if (I1.eq.0) go to 20
     I2 = ISCT(ISPC,2)

     if (LV2ATV) then

        if (.not.LSPPOK(ISPC)) SEICCF(ISPC) = 0.0
        CONSPP = DGCON(ISPC) + COR(ISPC) +&
             &0.01 * (DGCCF(ISPC) + SEICCF(ISPC)) * RELDEN

        DGLDS = DGLD(ISPC)
        DGBALS = DGBAL(ISPC)
        DGCRS = DGCR(ISPC)
        DGCRS2 = DGCRSQ(ISPC)
        DGDSQS = DGDSQ(ISPC)
        DGDBLS = DGDBAL(ISPC)

     else

        !         HW MODEL IN ICH MUST HAVE AT CCF>=100
        !         BL MODEL IN ICH MUST HAVE AT CCF>=125
        !         EP/OH MODEL IN IDF MUST HAVE AT CCF>=100
        !         PL MODEL IN IDF MUST HAVE AT CCF>=100

        RELDN2 = RELDEN
        select case (ISPC)
        case (5)
           if (INDEX(BEC%Zone,'ICH') .gt.0)&
                &RELDN2 = MAX(100.0, RELDEN)
        case (7)
           if (INDEX(BEC%Zone,'IDF') .gt.0)&
                &RELDN2 = MAX(100.0, RELDEN)
        case (9)
           if (INDEX(BEC%Zone,'ICH') .gt.0)&
                &RELDN2 = MAX(125.0, RELDEN)
        case (11,15)
           if ((INDEX(BEC%Zone,'ICH') .gt.0) .or.&
                &(INDEX(BEC%Zone,'IDF') .gt.0))&
                &RELDN2 = MAX(100.0, RELDEN)
        end select
        CONSPP = DGCON(ISPC) + COR(ISPC) +&
             &DGCCF1(ISPC) * RELDN2 * 0.01

     end if

     !       BEGIN TREE LOOP WITHIN SPECIES ISPC.
     do I3 = I1,I2
        I = IND1(I3)
        if (DIAM(I).le.0.0) goto 10

        CR = ICR(I) * 0.01
        if (LV2ATV) then
           D = DIAM(I)
           ALD = ALOG(D)
           BAL = (1.0 - (PCT(I)/100.)) * BA100
        else
           D = DIAM(I) * INtoCM
           BAL = (1.0 - (PCT(I)/100.)) * BA * FT2pACRtoM2pHA
           BALD1 = BAL/D
           BALD2 = BAL/LOG(D+1.0)

           !           HW MODEL IN ICH MUST HAVE CROWN<=0.80
           !           EP/OH MODEL MUST HAVE CROWN<=0.80
           select case (ISPC)
           case (5)
              if (INDEX(BEC%Zone,'ICH') .gt.0)&
                   &CR = MIN(0.8, CR)
           case (11,15)
              if ((INDEX(BEC%Zone,'ICH') .gt.0.) .or.&
                   &(INDEX(BEC%Zone,'IDF') .gt.0))&
                   &CR = MIN(0.8, CR)
           end select
        end if

        if (LV2ATV) then

           DDS = CONSPP + DGLDS*ALD + DGBALS*BAL + CR*(DGCRS+CR*DGCRS2)&
                &+DGDSQS*D*D  + DGDBLS*BAL/(ALOG(D+1.0))

        else

           select case (ISPC)

              ! new birch IDF model form in IDF; old form in ICH
           case (11,15)
              if (INDEX(BEC%Zone,'IDF') .gt.0) then
                 D2 = MAX(D,1.0) ! CONSTRAIN TO 1 CM
                 BALD1 = BAL/D2
                 DDS = DGLD1(ISPC) * (D2 **&
                      &(DGDBAL1(ISPC) + DGDBAL2(ISPC) * BALD1)) *&
                      &EXP(DGDSQ1(ISPC) * D2 * D2)
              else if (INDEX(BEC%Zone,'ICH') .gt.0) then
                 DDS = CONSPP&
                      &+ DGLD1(ISPC)   * D&
                      &+ DGDSQ1(ISPC)  * D*D&
                      &+ DGBAL1(ISPC)  * BAL&
                      &+ DGDBAL1(ISPC) * BALD1&
                      &+ DGDBAL2(ISPC) * BALD2&
                      &+ DGCR1(ISPC)   * CR
                 DDS = EXP(MAX(-9.21,DDS))
              end if

              ! new aspen model form
           case (12,13)
              D2 = MAX(D,1.0) ! CONSTRAIN TO 1 CM
              BALD1 = BAL/D2
              DDS = DGLD1(ISPC) * (D2 **&
                   &(DGDBAL1(ISPC) + DGDBAL2(ISPC) * BALD1)) *&
                   &EXP(DGDSQ1(ISPC) * D2 * D2)

           case default
              DDS = CONSPP&
                   &+ DGLD1(ISPC)    * D&
                   &+ DGDSQ1(ISPC)   * D*D&
                   &+ DGBAL1(ISPC)   * BAL&
                   &+ DGDBAL1(ISPC) * BALD1&
                   &+ DGDBAL2(ISPC) * BALD2&
                   &+ DGCR1(ISPC)    * CR
              DDS = EXP(MAX(-9.21,DDS))

           end select

           !           CONVERT DG [CM] PREDICTION TO LN(DDS) [IN]
           DDS = (DDS*DDS + 2.0 * DDS * D * BRATIO(ISPC,D,0.0)) *&
                &CMtoIN * CMtoIN

        end if

        DDS = MAX(0.001,DDS) ! temporary: during SBS fit
        DDS = LOG(DDS)

        DDS = MAX(-9.21,DDS)
        WK2(I) = DDS

        !         END OF TREE LOOP. PRINT DEBUG INFO IF DESIRED.
        if (.not.DEBUG) go to 10
        write(JOSTND,9001) I,ISPC,D,BAL,CR,RELDEN,BA,DDS
        9001 format(' IN DGF, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,&
             &',  BAL=',F7.2,',  CR=',F7.4/&
             &'          CCF=',F9.3,',  BA=',F9.3,',   LN(DDS)=',F7.4)
        10 continue
     end do

     !  END OF SPECIES LOOP.

     20 continue
  end do
  return

  entry DGCONS

  !     ENTRY POINT FOR LOADING COEFFICIENTS OF THE DIAMETER INCREMENT
  !     MODEL THAT ARE SITE SPECIFIC AND NEED ONLY BE RESOLVED ONCE.
  !     ITYPE IS THE HABITAT TYPE INDEX AS COMPUTED IN **HABTYP**.
  !     ASPECT IS STAND ASPECT IN RADIANS.  OBSERV CONTAINS THE NUMBER OF
  !     OBSERVATIONS BY HABITAT CLASS BY SPECIES FOR THE UNDERLYING
  !     MODEL (THIS DATA IS ACTUALLY USED BY **DGDRIV** FOR CALIBRATION).

  !     ENTER LOOP TO LOAD SPECIES DEPENDENT VECTORS.

  !     REPLACE NULLS IN COMPILED DATA STRINGS WITH SPACES, SO THAT
  !     STRING LENGTHS WILL BE FIGURED CORRECTLY AND STRING COMPARISONS
  !     WILL WORK PROPERLY

  if (LV2ATV) then

     !       FETCH SEI VARIANT ADJUSTMENT TERMS
     call SEILTDG (BECADJ,SEICN2,&
          &SEISLP,SEISAS,SEICAS,SEIELV,SEIEL2,SEICCF,LSPPOK)

     do I = 1,MAXSP
        ISPHAB = MAPHAB(ITYPE,I)
        ISPFOR = MAPLOC(IFOR,I)
        ISPDSQ = MAPDSQ(IFOR,I)
        ISPCCF = MAPCCF(ITYPE,I)

        !         USE V2 SEI-PARAMETERIZATION IF IT EXISTS
        if (LSPPOK(I)) then
           DGCON1(I) = DGHAB(ISPHAB,I) + DGFOR(ISPFOR,I)&
                !           ADD NI-AVERAGE SLOPE/ASPECT/ELEVATION USED DURING FITTING
                &+  DGEL(I)    * BECADJ(3)&
                &+  DGEL2(I)   * BECADJ(3) * BECADJ(3)&
                &+ (DGSASP(I)  * SIN(BECADJ(2))&
                &+  DGCASP(I)  * COS(BECADJ(2))&
                &+  DGSLOP(I)) * BECADJ(1)&
                &+  DGSLSQ(I)  * BECADJ(1) * BECADJ(1)

           !           USE SEI-DERIVED TERMS
           DGCON2(I) = SEICN2(I)&
                &+  SEIELV(I)  * ELEV&
                &+  SEIEL2(I)  * ELEV * ELEV&
                &+ (SEISAS(I)  * SIN(ASPECT)&
                &+  SEICAS(I)  * COS(ASPECT)&
                &+  SEISLP(I)) * SLOPE

           DGCON(I) = DGCON1(I) + DGCON2(I)
           V2SEICOR(I) = EXP(DGCON2(I))

        else ! OTHERWISE USE THE ORIGINAL NI MODEL
           DGCON(I) = DGHAB(ISPHAB,I)&
                &+  DGFOR(ISPFOR,I)&
                &+  DGEL(I)    * ELEV&
                &+  DGEL2(I)   * ELEV * ELEV&
                &+ (DGSASP(I)  * SIN(ASPECT)&
                &+  DGCASP(I)  * COS(ASPECT)&
                &+  DGSLOP(I)) * SLOPE&
                &+  DGSLSQ(I)  * SLOPE * SLOPE
           V2SEICOR(I) = 1.0
        end if

        DGDSQ(I) = DGDS(ISPDSQ,I)
        DGCCF(I) = DGCCFA(ISPCCF,I)
        ATTEN(I) = OBSERV(ISPHAB,I)
        SMCON(I) = 0. ! error here...

     end do

  else

     do J = 1,P_ZN
        do K = 1,P_ST
           do M = 1,P_ZL
              if (ICHAR(ZNKONST(J)%Zone(K)(M:M)) .eq.0)&
                   &ZNKONST(J)%Zone(K)(M:M) = ' '
           end do
        end do
     end do

     do J = 1,P_SS
        do K = 1,P_ST
           do M = 1,P_ZL
              if (ICHAR(SSKONST(J)%PrettyName(K)(M:M)) .eq.0)&
                   &SSKONST(J)%PrettyName(K)(M:M) = ' '
           end do
        end do
     end do

     !       MAKE TEMPORARY ZONE/SS MAPPING FOR SS IN THE KAMLOOPS ICHmw2.
     !       THESE SS DO NOT NATURALLY CORRESPOND TO THOSE OF THE NELSON REGION,
     !       AND ARE "INCREMENTED" BY ONE

     BEC2 = BEC
     if (INDEX(BEC2%Region,'KAM') .gt.0.and.&
          &INDEX(BEC2%FullName,'ICHmw2') .gt.0.and.&
          &INDEX(BEC2%Series,'-') .eq.0) then

        read (BEC2%Series,'(I4)') iSeries
        select case (iSeries)
        case (2)
           BEC2%PrettyName = "ICHmw2/03"
        case (3)
           BEC2%PrettyName = "ICHmw2/04"
        case (4)
           BEC2%PrettyName = "ICHmw2/05"
        case (5)
           BEC2%PrettyName = "ICHmw2/06"
        case (6)
           BEC2%PrettyName = "ICHmw2/07"
        case (7)
           BEC2%PrettyName = "ICHmw2/08"
        end select
     end if
     PAT2 = BEC2%PrettyName

     !       LOCATE ZONE USING "/all" PATTERN MATCH

     do I = 1,MAXSP
        IPOS(I) = 0
        JPOS(I) = 0
     end do

     !       FIND INDEX TO BECZONE COEFFICIENTS -> IPOS()
     do I = 1,MAXSP
        IPAS = 0
        PAT1 = PAT2
        7 do J = 1,P_ZN
           do K = 1,MAXSP
              if (ZNKONST(J)%SPP(K) .eq.0) exit
              if (ZNKONST(J)%SPP(K) .eq. I) then
                 do M = 1, P_ST
                    if (LEN_TRIM(ZNKONST(J)%Zone(M)) .eq.0) exit
                    if (INDEX(PAT1,ZNKONST(J)%Zone(M)) .gt.0) then
                       IPOS(I) = J
                       goto 8
                    end if
                 end do
                 exit
              end if
           end do
        end do
        if (J .gt. P_ZN .and. IPAS .eq.0) then
           IPAS = 1
           PAT1 = BEC2%Zone(1:LEN_TRIM(BEC%Zone))//'/all '
           goto 7
        end if
        8 if (IPOS(I) .eq.0) goto 5

        !         FIND INDEX TO SITE SERIES COEFFICIENTS -> JPOS()
        IPAS = 0
        PAT1 = PAT2
        9 do J = 1,P_SS
           do K = 1,MAXSP
              if (SSKONST(J)%SPP(K) .eq.0) exit
              if (SSKONST(J)%SPP(K) .eq. I) then
                 do M = 1, P_ST
                    if (LEN_TRIM(SSKONST(J)%PrettyName(M)) .eq.0) exit
                    if (INDEX(PAT1,SSKONST(J)%PrettyName(M)) .gt.0) then
                       JPOS(I) = J
                       goto 5
                    end if
                 end do
                 exit
              end if
           end do
        end do
        if (J .gt. P_SS .and. IPAS .eq.0) then
           IPAS = 1
           PAT1 = BEC2%Zone(1:LEN_TRIM(BEC%Zone))//'/all '
           goto 9
        end if
     5 end do

     !       BUILD EQUATION EXPRESSIONS
     do I = 1,MAXSP
        !ccc          LLTDGOK(I) = .FALSE.
        DGCON(I) = 0.0
        IP = IPOS(I)
        JP = JPOS(I)

        if (IP .lt.1.or. JP .lt.1) goto 11

        !         CONSTRAIN WHITE PINE TO RANGE: 500-1200m; ELEV COEFFS
        !         PARAMETERIZED WITH ELEV(m)/100; OR 5.0-12.0
        PELEV = ELEV * FTtoM
        if (I .eq.1) then
           PELEV = MIN(MAX(PELEV,5.0),12.0)
        end if

        DGCON(I) = SSKONST(JP)%CON&
             &+ ZNKONST(IP)%CON&
             &+ ZNKONST(IP)%EL   * PELEV&
             &+ ZNKONST(IP)%EL2  * (PELEV**2)&
             &+ ZNKONST(IP)%SASP * SIN(ASPECT) * SLOPE&
             &+ ZNKONST(IP)%CASP * COS(ASPECT) * SLOPE

        !         POPULATE ARRAYS FOR OTHER CONSTANTS
        DGCCF1(I) = ZNKONST(IP)%CCFA ! CCF
        DGLD1(I) = ZNKONST(IP)%LD ! DBH
        DGBAL1(I) = ZNKONST(IP)%BAL ! BAL
        DGCR1(I) = ZNKONST(IP)%CR ! CROWN
        DGDSQ1(I) = ZNKONST(IP)%DSQ ! DBH^2
        DGDBAL1(I) = ZNKONST(IP)%DBAL1 ! BAL/DBH
        DGDBAL2(I) = ZNKONST(IP)%DBAL2 ! BAL/LOG(DBH+1)
        ATTEN(I) = ZNKONST(IP)%OBSERV ! OBSERV **coeffs.f77**
        SMCON(I) = 0.
        !        SIGMAR(I)  = ZNKONST(IP)%SIGMAR ! SIGMAR - RES.ERR
        !        SIGMAR(I)  = SIGMAR(I) * 0.5
        !          LLTDGOK(I) = .TRUE.
     11 end do
  end if

  !  IF READCORD OR REUSCORD WAS SPECIFIED (LDCOR2 IS TRUE) ADD
  !  LN(COR2) TO THE BAI MODEL CONSTANT TERM (DGCON).  COR2 IS
  !  INITIALIZED TO 1.0 IN BLKDATA.

  do I = 1,MAXSP
     if (LDCOR2.and.COR2(I).gt.0.0) DGCON(I) = DGCON(I)&
          &+ ALOG(COR2(I))
  end do

  return
end
