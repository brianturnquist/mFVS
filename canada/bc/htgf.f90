!======================================!
!   Compute large tree height growth   !
!======================================!
! CANADA-BC $Id$
subroutine HTGF
  implicit none

  !  THIS SUBROUTINE COMPUTES THE PREDICTED PERIODIC HEIGHT INCREMENT FOR
  !  EACH CYCLE AND LOADS IT INTO THE ARRAY HTG. HEIGHT INCREMENT IS
  !  PREDICTED FROM SPECIES, HABITAT TYPE, HEIGHT, DBH, AND PREDICTED DBH
  !  INCREMENT.  THIS ROUTINE IS CALLED FROM **TREGRO** DURING REGULAR
  !  CYCLING.  ENTRY **HTCONS** IS CALLED FROM **RCON** TO LOAD SITE
  !  DEPENDENT CONSTANTS THAT NEED ONLY BE RESOLVED ONCE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CALCOM.f90'
  include 'ARRAYS.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'MULTCM.f90'
  include 'HTCAL.f90'
  include 'BCPLOT.f90'
  include 'METRIC.f90'

  !   MODEL COEFFICIENTS AND CONSTANTS:

  !    BIAS -- THE AVERAGE RESIDUAL.
  !   HTCON -- AN ARRAY CONTAINING HABITAT TYPE CONSTANTS FOR
  !            HEIGHT GROWTH MODEL (SUBSCRIPTED BY SPECIES)
  !   HDGCOF -- COEFFICIENT FOR DIAMETER GROWTH TERMS.
  !    HGLD -- AN ARRAY, SUBSCRIPTED BY SPECIES, OF THE
  !             COEFFICIENTS FOR THE DIAMETER TERM IN THE HEIGHT
  !             GROWTH MODEL.
  !   H2COF -- COEFFICIENT FOR HEIGHT SQUARED TERMS.
  !    IND2 -- ARRAY OF POINTERS TO SMALL TREES.
  !   SCALE -- TIME FACTOR DERIVED BY DIVIDING FIXED POINT CYCLE
  !            LENGTH BY GROWTH PERIOD LENGTH FOR DATA FROM
  !            WHICH MODELS WERE DEVELOPED.

  !     P_MD  - NUMBER OF MODEL TYPES

  ! Variables
  integer :: P_MD,P_SS
  parameter (P_MD = 57)
  parameter (P_SS = 30)

  integer :: I,ISPC,I1,I2,I3,ITFN,J,K,M,IHT
  integer :: IPOS(MAXSP),IPAS
  real    :: SCALE,XHT,HTI,D,HTNEW,LTSP(MAXSP,3),CON,Y
  real    :: XHT1(MAXSP),XHT2(MAXSP)
  real    :: V3HTG
  character(len=15) :: PAT

  logical :: DEBUG
  !     v2
  integer :: MAPHAB
  real    :: HGLD,HGHC,HGLDD,HGSC,HGH2,BIAS,HGLH,MISHGF
  dimension     HGLD(MAXSP),HGHC(8),HGLDD(8),HGSC(MAXSP),HGH2(8),&
       &MAPHAB(30)

  data HGLD/&
       &-.04935,-.3899,-.4574,-.09775,-.1555,-.1219,&
       &-.2454, -.5720,-.1997,-.5657, -.1219,-.1219,&
       &-.1219,-.4574,-.1219/
  data BIAS/ .4809 /, HGLH/ 0.23315 /

  !     LOCAL HEIGHT-GROWTH DATA STRUCTURE FOR EACH SITE SERIES.

  TYPE LTHG_STR
     integer            :: SPP(MAXSP) ! SPECIES CODE
     character (LEN = 15) :: PRETTYNAME(P_SS) ! BEC/SITE SERIES
     real               :: SI ! SI - Old model
     real               :: B3 ! B3
     real               :: C ! C

     real               :: CN ! CN - New model
     real               :: CNSI ! CNSI
     real               :: DG ! DG
     real               :: DG2 ! DG**2
     real               :: DBH1 ! DBH Additive
     real               :: DBH2 ! DBH Multiplicative
     real               :: HT2 ! HT**2
  end TYPE

  TYPE (LTHG_STR) LTHG(P_MD)

  !     SPECIES-SPECIFIC LARGE TREE COEFFICIENTS:
  !     A1, B1, B2

  data ((LTSP(I,J), J = 1,3), I = 1,MAXSP) /&
       &4.1060,   0.0,      0.0,& ! PW - 1
       &2.2341,  -0.0532,  -0.00222,& ! LW - 2
       &2.6937,  -0.2034,  -0.00387,& ! FD - 3
       &2.0204,   0.0,      0.0,& ! BG - 4 (RARE: USE BL)
       &1.4113,   0.4019,  -0.00760,& ! HW - 5 (use CW)
       &1.4113,   0.4019,  -0.00760,& ! CW - 6
       &0.0   ,   0.0,      0.0,& ! PL - 7 - new model
       &0.0   ,   0.0,      0.0,& ! SE - 8 - new model
       &2.0204,   0.0,      0.0,& ! BL - 9
       &5.1763,  -0.3013,   0.0,& ! PY - 10
       &0.0   ,   0.0,      0.0,& ! EP - 11 - new model
       &0.0   ,   0.0,      0.0,& ! AT - 12 - new model
       &0.0   ,   0.0,      0.0,& ! CT - 13 - new model
       &2.6937,  -0.2034,  -0.00387,& ! OC - 14 [FD]
       &0.0   ,   0.0,      0.0& ! OH - 15 [EP] - new
       &/

  !     SITE SERIES COEFFICIENTS: THESE ARE IN SPECIES ORDER.
  !     NOTE THAT THERE IS A SPECIAL BEC STRING WITH 'all' AS THE
  !     SUBZONE. THIS IS USED (FOR SOME SPECIES) WHEN THERE IS NOT
  !     A PERFECT MATCH TO THE SITE SERIES

  data LTHG(1) /&
       &LTHG_STR(& ! ICH+IDF PW 21m 1-8
       &(/ 1, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'ICHmw3/04      ', 'ICHmw3/05      ',&
       &'ICHdw/01a      ', 'ICHmw1/02      ',&
       &'ICHmw1/03      ', 'ICHmw1/04      ',&
       &'ICHmw1/06      ', 'ICHxw/01       ',&
       &'ICH/all        ', 'IDF/all        ',&
       &'SBPS/all       ', 'SBS/all        ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &0.0,& ! SI
       &0.0429,& ! B3
       &-0.00142,& ! C
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(2) /&
       &LTHG_STR(& ! ICH+IDF PW 24m 1-28
       &(/ 1, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'ICHmw2/01      ', 'ICHmw2/01-ys   ',&
       &'ICHmw2/02      ', 'ICHmw2/03      ',&
       &'ICHmw2/04      ', 'ICHmw2/05      ',&
       &'ICHmw3/01      ', 'ICHmw3/01-yc   ',&
       &'ICHmw3/06      ', 'ICHmw3/07      ',&
       &'ICHvk1/01      ', 'ICHvk1/02      ',&
       &'ICHvk1/03      ', 'ICHvk1/04      ',&
       &'ICHvk1/05      ', 'ICHwk1/01      ',&
       &'ICHwk1/03      ', 'ICHwk1/04      ',&
       &'ICHwk1/05      ', 'ICHdw/01b      ',&
       &'ICHdw/03       ', 'ICHdw/04       ',&
       &'ICHmw1/01      ', 'ICHmw1/05      ',&
       &'ICHmw2/03      ', 'ICHmw2/04      ',&
       &'ICHmw2/06      ', 'ICHwk1/07      ',&
       &'               ', '               '/),&
       &-2.5072,& ! SI
       &0.0944,& ! B3
       &-0.00086,& ! C
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(3) /&
       &LTHG_STR(& ! ICH+IDF LW 15m 1-2
       &(/ 2, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'IDFdm1/04      ', 'IDFdm2/03      ',&
       &'ICH/all        ', 'IDF/all        ',&
       &'SBPS/all       ', 'SBS/all        ',&
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
       &0.0,& ! SI
       &0.0808,& ! B3
       &-0.00115,& ! C
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(4) /&
       &LTHG_STR(& ! ICH+IDF LW 18m 1-3
       &(/ 2, 0,0,0,0,0,0,0,0,0,0,0,0,0,0/),&
       &(/'IDFdm1/01      ', 'IDFdm2/01      ',&
       &'IDFdm2/04      ', '               ',&
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
       &0.0,& ! SI
       &0.1058,& ! B3
       &-0.00123,& ! C
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(5) /&
       &LTHG_STR(& ! ICH+IDF LW 21m 1-12
       &(/ 2, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'ICHmk1/01      ', 'ICHmk1/01-ys   ',&
       &'ICHmk1/03      ', 'ICHmk1/04      ',&
       &'ICHmw3/04      ', 'IDFdm1/05      ',&
       &'IDFdm1/07      ', 'IDFmw1/04      ',&
       &'ICHdw/01a      ', 'ICHdw/02       ',&
       &'ICHxw/01       ', 'IDFdm2/05      ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &0.0,& ! SI
       &0.0802,& ! B3
       &-0.00026,& ! C
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(6) /&
       &LTHG_STR(& ! ICH+IDF LW 24m 1-19
       &(/ 2, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'ICHmk1/05      ', 'ICHmk1/06      ',&
       &'ICHmw2/01      ', 'ICHmw2/01-ys   ',&
       &'ICHmw2/02      ', 'ICHmw2/03      ',&
       &'ICHmw2/04      ', 'ICHmw2/05      ',&
       &'IDFdm1/06      ', 'IDFmw1/01      ',&
       &'IDFmw1/01-yc   ', 'IDFmw1/05      ',&
       &'IDFmw1/06      ', 'ICHdw/01b      ',&
       &'ICHdw/03       ', 'ICHdw/04       ',&
       &'ICHmw2/03      ', 'ICHmw2/04      ',&
       &'ICHmw2/06      ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &0.0,&
       &0.0802,&
       &-0.00026,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(7) /&
       &LTHG_STR(& ! ICH+IDF LW Class 66 (Not in SIBEC table)
       &(/ 2, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'ICHmw3/01      ', '               ',&
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
       &0.0,& ! SI
       &0.0602,& ! B3
       &-0.00026,& ! C
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(8) /&
       &LTHG_STR(& ! ICH+IDF LW Class 77 (Not in SIBEC table)
       &(/ 2, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'IDFmw2/01      ', '               ',&
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
       &0.0,&
       &0.0,&
       &-0.00026,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(9) /&
       &LTHG_STR(& ! ICH+IDF FD,OC 12m 1-23
       &(/ 3, 14, 0,0,0,0,0,0,0,0,0,0,0,0,0/),&
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
       &'IDFxh2/03      ', 'ICH/all        ',&
       &'IDF/all        ', 'SBPS/all       ',&
       &'SBS/all        ', '               ',&
       &'               ', '               '/),&
       &0.0,&
       &0.0673,&
       &-0.00020,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(10) /&
       &LTHG_STR(& ! ICH+IDF FD 15m 1-30 (IDFdk3/01 -> Class 88)
       &(/ 3, 14, 0,0,0,0,0,0,0,0,0,0,0,0,0/),&
       &(/'ICHdk/02       ', 'ICHmk3/02      ',&
       &'ICHwk2/02      ', 'ICHwk2/03      ',&
       &'ICHwk4/02      ', 'ICHwk4/03      ',&
       &'IDFdk3/05      ', 'IDFdk4/09      ',&
       &'IDFdw/01       ', 'IDFxm/01       ',&
       &'IDFxm/05       ', 'IDFxm/06       ',&
       &'IDFxm/07       ', 'IDFxw/01       ',&
       &'IDFxw/05       ', 'ICHmk1/02      ',&
       &'ICHmk2/02      ', 'ICHmw3/02      ',&
       &'IDFdk1/01      ', 'IDFdk1/03      ',&
       &'IDFdk1/04      ', 'IDFdk2/01      ',&
       &'IDFdk2/03      ', 'IDFdk3/06      ',&
       &'IDFdm1/04      ', 'IDFmw1/02      ',&
       &'IDFmw2/02      ', 'IDFxh1/01      ',&
       &'IDFxh1/04      ', '               '/),&
       &0.0,&
       &0.0799,&
       &-0.00020,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /
  data LTHG(11) /&
       &LTHG_STR(& ! ICH+IDF FD 15m 31-38 (IDFxh2/04 & /06 -> Class 77)
       &(/ 3, 14, 0,0,0,0,0,0,0,0,0,0,0,0,0/),&
       &(/'IDFxh1/05      ', 'IDFxh2/01      ',&
       &'IDFxh2/05      ', 'ICHdw/02       ',&
       &'IDFdm2/01      ', 'IDFdm2/03      ',&
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
       &0.0,&
       &0.0799,&
       &-0.00020,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(12) /&
       &LTHG_STR(& ! ICH+IDF FD 18m 1-24
       &(/ 3, 14, 0,0,0,0,0,0,0,0,0,0,0,0,0/),&
       &(/'ICHmk3/03      ', 'ICHwk4/04      ',&
       &'IDFdk3/06      ', 'IDFdk3/07      ',&
       &'IDFdk3/08      ', 'IDFxm/08       ',&
       &'ICHmk1/03      ', 'ICHmk1/04      ',&
       &'ICHmk2/01      ', 'ICHmk2/03      ',&
       &'ICHmk2/04      ', 'ICHmw3/03      ',&
       &'IDFdk1/05      ', 'IDFdk2/04      ',&
       &'IDFdm1/01      ', 'IDFmw1/03      ',&
       &'IDFmw1/04      ', 'IDFmw2/03      ',&
       &'IDFww/03       ', 'IDFxh1/06      ',&
       &'IDFxh1/07      ', 'ICHmw1/02      ',&
       &'IDFdm2/04      ', 'IDFdm2/07      ',&
       &'SBPSmk/01      ', 'SBSdw2/01      ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &0.6750,&
       &0.0647,&
       &-0.00020,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(13) /&
       &LTHG_STR(& ! ICH+IDF FD 21m 1-30 (ICHmw3/04 -> Class 66)
       &(/ 3, 14, 0,0,0,0,0,0,0,0,0,0,0,0,0/),&
       &(/'ICHdk/04       ', 'ICHmk3/06      ',&
       &'ICHwk2/04      ', 'ICHwk4/05      ',&
       &'IDFxw/06       ', 'IDFxw/07       ',&
       &'ICHmk1/01      ', 'ICHmk1/01-ys   ',&
       &'ICHmk1/05      ', 'ICHmk2/05      ',&
       &'ICHmw2/02      ', 'ICHmw2/03      ',&
       &'ICHmw3/01      ', 'ICHmw3/01-yc   ',&
       &'ICHmw3/05      ', 'ICHwk1/03      ',&
       &'IDFdk1/06      ', 'IDFdk2/05      ',&
       &'IDFdk2/06      ', 'IDFdm1/05      ',&
       &'IDFdm1/07      ', 'IDFmw1/01      ',&
       &'IDFmw1/01-yc   ', 'IDFmw1/05      ',&
       &'IDFmw2/01      ', 'IDFmw2/04      ',&
       &'IDFxh1/08      ', 'IDFxh2/07      ',&
       &'IDFxh2/08      ', '               '/),&
       &0.0,&
       &0.0853,&
       &-0.00020,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(14) /&
       &LTHG_STR(& ! ICH+IDF FD 21m 31-39
       &(/ 3, 14, 0,0,0,0,0,0,0,0,0,0,0,0,0/),&
       &(/'ICHdw/01a      ', 'ICHmw1/03      ',&
       &'ICHmw1/04      ', 'ICHmw2/03      ',&
       &'ICHmw2/04      ', 'ICHxw/01       ',&
       &'IDFdm2/05      ', 'SBSdw1/01      ',&
       &'SBSmh/01       ', '               ',&
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
       &0.0,&
       &0.0853,&
       &-0.00020,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(15) /&
       &LTHG_STR(& ! ICH+IDF FD 24m 1-30
       &(/ 3, 14, 0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
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
       &'IDFdm1/06      ', 'IDFmw1/06      ',&
       &'IDFww/01       ', 'IDFww/04       ',&
       &'IDFww/05       ', 'IDFww/06       ',&
       &'ICHdw/01b      ', 'ICHdw/03       '/),&
       &0.6854,&
       &0.0750,&
       &-0.00020,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(16) /&
       &LTHG_STR(& ! ICH+IDF FD 24m 31-35
       &(/ 3, 14, 0,0,0,0,0,0,0,0,0,0,0,0,0/),&
       &(/'ICHdw/04       ', 'ICHmw1/01      ',&
       &'ICHmw1/05      ', 'ICHmw1/06      ',&
       &'ICHmw2/06      ', '               ',&
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
       &0.6854,&
       &0.0750,&
       &-0.00020,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(17) /&
       &LTHG_STR(& ! ICH+IDF FD Class 66
       &(/ 3, 14, 0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'ICHmw3/04      ', '               ',&
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
       &1.8869,&
       &0.0519,&
       &-0.00020,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(18) /&
       &LTHG_STR(& ! ICH+IDF FD Class 77
       &(/ 3, 14, 0,0,0,0,0,0,0,0,0,0,0,0,0/),&
       &(/'IDFxh2/04      ', 'IDFxh2/06      ',&
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
       &-1.4087,&
       &0.0699,&
       &-0.00020,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(19) /&
       &LTHG_STR(& ! ICH+IDF FD Class 88
       &(/ 3, 14, 0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'IDFdk3/01      ', '               ',&
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
       &-1.2464,&
       &0.1298,&
       &-0.00020,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(20) /&
       &LTHG_STR(& ! ICH+IDF BG (use BL 15m 1-9)
       &(/ 4, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
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
       &0.0,& ! SI
       &0.0545,& ! B3
       &-0.00071,& ! C
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /
  data LTHG(21) /&
       &LTHG_STR(& ! ICH+IDF HW 15m 1-18
       &(/ 5, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
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
       &'SBPS/all       ', 'SBS/all        ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &-0.6909,&
       &0.0451,&
       &-0.00022,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(22) /&
       &LTHG_STR(& ! ICH+IDF HW 18m 1-28
       &(/ 5, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
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
       &-0.8108,&
       &0.0692,&
       &-0.00022,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(23) /&
       &LTHG_STR(& ! ICH+IDF HW Class 66 (not in SIBEC table)
       &(/ 5, 0,0,0,0,0,0,0,0,0,0,0,0,0,0/),&
       &(/'ICHmw3/03      ', '               ',&
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
       &-0.6909,& !! use 15m CW
       &0.0451,&
       &-0.00022,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(24) /&
       &LTHG_STR(& ! ICH+IDF CW 12m 1-3
       &(/ 6, 0,0,0,0,0,0,0,0,0,0,0,0,0,0/),&
       &(/'ICHmk1/04      ', 'ICHmk2/04      ',&
       &'ICHmw1/02      ', 'ICH/all        ',&
       &'IDF/all        ', 'SBPS/all       ',&
       &'SBS/all        ', '               ',&
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
       &0.0,&
       &0.0,&
       &-0.00022,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(25) /&
       &LTHG_STR(& ! ICH+IDF CW 15m 1-30
       &(/ 6, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'ICHmk1/01      ', 'ICHmk1/01-ys   ',&
       &'ICHmk1/07      ', 'ICHmk2/01      ',&
       &'ICHmk2/06      ', 'ICHmw2/02      ',&
       &'ICHmw3/01      ', 'ICHmw3/01-yc   ',&
       &'ICHmw3/03      ', 'ICHmw3/04      ',&
       &'ICHmw3/05      ', 'ICHmw3/08      ',&
       &'ICHvk1/05      ', 'ICHvk1/06      ',&
       &'ICHwk1/02      ', 'ICHwk1/03      ',&
       &'ICHwk1/06      ', 'ICHwk1/07      ',&
       &'IDFdk2/07      ', 'IDFmw1/01      ',&
       &'IDFmw1/01-yc   ', 'IDFmw1/04      ',&
       &'IDFmw2/01      ', 'IDFmw2/05      ',&
       &'IDFww/01       ', 'IDFww/04       ',&
       &'IDFww/07       ', 'ICHdw/01a      ',&
       &'ICHmw1/03      ', 'ICHmw1/04      '/),&
       &-0.6909,&
       &0.0451,&
       &-0.00022,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(26) /&
       &LTHG_STR(& ! ICH+IDF CW 15m 31-34
       &(/ 6, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'ICHmw1/07      ', 'ICHmw2/03      ',&
       &'ICHwk1/08      ', 'ICHxw/01       ',&
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
       &-0.6909,&
       &0.0451,&
       &-0.00022,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(27) /&
       &LTHG_STR(& ! ICH+IDF CW 18m 1-30
       &(/ 6, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'ICHmk1/05      ', 'ICHmk1/06      ',&
       &'ICHmk2/05      ', 'ICHmw2/01      ',&
       &'ICHmw2/01-ys   ', 'ICHmw2/03      ',&
       &'ICHmw2/04      ', 'ICHmw2/05      ',&
       &'ICHmw3/06      ', 'ICHmw3/07      ',&
       &'ICHvk1/01      ', 'ICHvk1/03      ',&
       &'ICHvk1/04      ', 'ICHwk1/01      ',&
       &'ICHwk1/04      ', 'ICHwk1/05      ',&
       &'IDFmw1/05      ', 'IDFmw1/06      ',&
       &'IDFmw2/04      ', 'IDFww/05       ',&
       &'IDFww/06       ', 'ICHdw/01b      ',&
       &'ICHdw/02       ', 'ICHdw/03       ',&
       &'ICHdw/04       ', 'ICHmw1/01      ',&
       &'ICHmw1/05      ', 'ICHmw1/06      ',&
       &'ICHmw2/04      ', 'ICHmw2/06      '/),&
       &-0.8108,&
       &0.0692,&
       &-0.00022,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(28) /&
       &LTHG_STR(& ! ICH+IDF CW Class 66  (Not in SIBEC table)
       &(/ 6, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'ICHmk1/03      ', '               ',&
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
       &0.0,&
       &0.0,&
       &-0.00022,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(29) /&
       &LTHG_STR(& ! ICH+IDF PL 15m 1-25
       &(/ 7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'ICHdk/02       ', 'ICHwk2/02      ',&
       &'ICHwk2/03      ', 'ICHwk4/02      ',&
       &'ICHwk4/03      ', 'IDFdk3/02      ',&
       &'IDFdk3/03      ', 'IDFdk3/05      ',&
       &'IDFdk4/01      ', 'IDFdk4/02      ',&
       &'IDFdk4/05      ', 'IDFdk4/06      ',&
       &'IDFdk4/07      ', 'IDFdk4/08      ',&
       &'IDFdk4/09      ', 'IDFxm/03       ',&
       &'IDFdk1/02      ', 'IDFdk1/03      ',&
       &'IDFdk1/04      ', 'IDFdk3/06      ',&
       &'IDFdm1/03      ', 'IDFdm1/04      ',&
       &'IDFmw1/03      ', 'IDFww/02       ',&
       &'IDFdm2/03      ', 'ICH/all        ',&
       &'IDF/all        ', 'SBPS/all       ',&
       &'SBS/all        ', '               '/),&
       &0.0, 0.0, 0.0,&
       &1.7767,& !cn
       &-0.1115,& !cnsi
       &1.4445,& !dg
       &-0.0354,& !dg**2
       &-0.2837,& !dbh additive
       &0.00172,& !dbh multiplicative
       &-0.00063& !ht**2
       &) /

  data LTHG(30) /&
       &LTHG_STR(& ! ICH+IDF PL 18m 1-25 (IDFdk2/01 & /04 -> Class 88)
       !                     (IDFdk3/01 & /06 -> Class 99)
       &(/ 7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'ICHdk/03       ', 'ICHmk3/02      ',&
       &'IDFdk3/07      ', 'IDFdk3/08      ',&
       &'IDFdk3/09      ', 'IDFdw/01       ',&
       &'ICHmk1/02      ', 'ICHmk2/02      ',&
       &'ICHmw3/02      ', 'ICHwk1/02      ',&
       &'IDFdk1/01      ', 'IDFdk2/03      ',&
       &'IDFdm1/01      ', 'IDFmw1/04      ',&
       &'IDFmw2/02      ', 'IDFmw2/05      ',&
       &'IDFww/03       ', 'ICHmw1/02      ',&
       &'IDFdm2/01      ', 'IDFdm2/04      ',&
       &'IDFdm2/07      ', 'SBPSdc/01      ',&
       &'SBPSmk/01      ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &0.0, 0.0, 0.0,&
       &1.7767,& !cn
       &-0.1115,& !cnsi
       &1.4445,& !dg
       &-0.0354,& !dg**2
       &-0.2837,& !dbh additive
       &0.00172,& !dbh multiplicative
       &-0.00063& !ht**2
       &) /

  data LTHG(31) /&
       &LTHG_STR(& ! ICH+IDF PL 21m 1-30 (ICHmk2/03 -> Class 99)
       &(/ 7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'ICHdk/04       ', 'ICHmk3/03      ',&
       &'ICHmk3/05      ', 'ICHmk3/06      ',&
       &'ICHmk3/07      ', 'ICHwk2/04      ',&
       &'ICHwk2/05      ', 'ICHwk2/06      ',&
       &'ICHwk2/07      ', 'ICHwk4/04      ',&
       &'ICHwk4/05      ', 'ICHwk4/06      ',&
       &'ICHwk4/07      ', 'ICHmk1/01      ',&
       &'ICHmk1/01-ys   ', 'ICHmk1/03      ',&
       &'ICHmk1/04      ', 'ICHmk1/05      ',&
       &'ICHmk1/07      ', 'ICHmk2/01      ',&
       &'ICHmk2/04      ', 'ICHmk2/06      ',&
       &'ICHmw2/02      ', 'ICHmw3/01      ',&
       &'ICHmw3/01-yc   ', 'ICHmw3/03      ',&
       &'ICHmw3/04      ', 'ICHmw3/05      ',&
       &'ICHwk1/03      ', '               '/),&
       &0.0, 0.0, 0.0,&
       &1.7767,& !cn
       &0.8675,& !cnsi
       &1.4445,& !dg
       &-0.0354,& !dg**2
       &-0.2837,& !dbh additive
       &0.00172,& !dbh multiplicative
       &-0.00063& !ht**2
       &) /

  data LTHG(32) /&
       &LTHG_STR(& ! ICH+IDF PL 21m 31-55
       &(/ 7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'ICHwk1/04      ', 'IDFdk1/05      ',&
       &'IDFdk1/06      ', 'IDFdk2/05      ',&
       &'IDFdk2/06      ', 'IDFdk2/07      ',&
       &'IDFdm1/05      ', 'IDFdm1/07      ',&
       &'IDFmw1/01      ', 'IDFmw1/01-yc   ',&
       &'IDFmw1/05      ', 'IDFmw1/06      ',&
       &'IDFmw2/01      ', 'IDFmw2/03      ',&
       &'ICHdw/01a      ', 'ICHdw/02       ',&
       &'ICHmw1/03      ', 'ICHmw1/04      ',&
       &'ICHmw1/07      ', 'ICHmw2/03      ',&
       &'ICHxw/01       ', 'IDFdm2/05      ',&
       &'SBSdw1/01      ', 'SBSmh/01       ',&
       &'SBPSmk/01      ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &0.0, 0.0, 0.0,&
       &1.7767,& !cn
       &0.8675,& !cnsi
       &1.4445,& !dg
       &-0.0354,& !dg**2
       &-0.2837,& !dbh additive
       &0.00172,& !dbh multiplicative
       &-0.00063& !ht**2
       &) /

  data LTHG(33) /&
       &LTHG_STR(& ! ICH+IDF PL 24m 1-25
       &(/ 7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'ICHdk/01       ', 'ICHdk/05       ',&
       &'ICHmk3/01      ', 'ICHmk3/04      ',&
       &'ICHwk2/01      ', 'ICHwk4/01      ',&
       &'ICHmk1/06      ', 'ICHmk2/05      ',&
       &'ICHmw2/01      ', 'ICHmw2/01-ys   ',&
       &'ICHmw2/03      ', 'ICHmw2/04      ',&
       &'ICHmw2/05      ', 'ICHmw3/06      ',&
       &'ICHwk1/01      ', 'IDFdm1/06      ',&
       &'IDFmw2/04      ', 'IDFww/01       ',&
       &'ICHdw/01b      ', 'ICHdw/03       ',&
       &'ICHdw/04       ', 'ICHmw1/01      ',&
       &'ICHmw1/05      ', 'ICHmw1/06      ',&
       &'ICHmw2/06      ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &0.0, 0.0, 0.0,&
       &1.7767,& !cn
       &1.1317,& !cnsi
       &1.4445,& !dg
       &-0.0354,& !dg**2
       &-0.2837,& !dbh additive
       &0.00172,& !dbh multiplicative
       &-0.00063& !ht**2
       &) /

  data LTHG(34) /&
       &LTHG_STR(& ! ICH+IDF PL Class 88
       &(/ 7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'IDFdk2/01      ', 'IDFdk2/04      ',&
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
       &0.0, 0.0, 0.0,&
       &1.7767,& !cn
       &0.5698,& !cnsi
       &1.4445,& !dg
       &-0.0354,& !dg**2
       &-0.2837,& !dbh additive
       &0.00172,& !dbh multiplicative
       &-0.00063& !ht**2
       &) /

  data LTHG(35) /&
       &LTHG_STR(& ! ICH+IDF PL Class 99
       &(/ 7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'ICHmk2/03      ', 'IDFdk3/01      ',&
       &'IDFdk3/06      ', '               ',&
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
       &0.0, 0.0, 0.0,&
       &1.7767,& !cn
       &-1.4182,& !cnsi
       &1.4445,& !dg
       &-0.0354,& !dg**2
       &-0.2837,& !dbh additive
       &0.00172,& !dbh multiplicative
       &-0.00063& !ht**2
       &) /

  data LTHG(36) /&
       &LTHG_STR(& ! ICH+IDF SE 15m 1-15
       &(/ 8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'ICHwk2/04      ', 'IDFdk3/09      ',&
       &'IDFdk4/08      ', 'IDFdk4/09      ',&
       &'IDFdk4/10      ', 'IDFdw/01       ',&
       &'ICHmw2/02      ', 'ICHmw3/03      ',&
       &'IDFdk1/01      ', 'IDFdk2/01      ',&
       &'IDFdk2/04      ', 'IDFdm1/01      ',&
       &'ICHmw1/02      ', 'ICHmw2/03      ',&
       &'IDFdm2/07      ', 'ICHmw2/03      ',&
       &'ICH/all        ', 'IDF/all        ',&
       &'SBPSdc/01      ', 'SBPS/all       ',&
       &'SBS/all        ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &0.0, 0.0, 0.0,&
       &0.1717,& !cn
       &0.1475,& !cnsi (use 18m)
       &0.4598,& !dg
       &-0.0159,& !dg**2
       &0.3659,& !dbh additive
       &-0.0060,& !dbh multiplicative
       &-0.00085& !ht**2
       &) /

  data LTHG(37) /&
       &LTHG_STR(& ! ICH+IDF SE 18m 1-30 (ICHmw2/03 -> 15m: AAZ)
       &(/ 8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'ICHwk4/04      ', 'ICHwk4/05      ',&
       &'ICHwk4/08      ', 'IDFdk3/07      ',&
       &'IDFdk3/08      ', 'IDFxm/08       ',&
       &'IDFxm/09       ', 'IDFxw/06       ',&
       &'ICHmk1/01      ', 'ICHmk1/01-ys   ',&
       &'ICHmk1/03      ', 'ICHmk1/04      ',&
       &'ICHmk1/07      ', 'ICHmk2/01      ',&
       &'ICHmk2/03      ', 'ICHmk2/04      ',&
       &'ICHmk2/06      ', 'ICHmw2/01      ',&
       &'ICHmw2/01-ys   ', 'ICHmw2/07      ',&
       &'ICHmw3/01      ', 'ICHmw3/01-yc   ',&
       &'ICHmw3/04      ', 'ICHmw3/05      ',&
       &'ICHmw3/08      ', 'ICHvk1/06      ',&
       &'ICHwk1/03      ', 'ICHwk1/07      ',&
       &'IDFdk1/05      ', '               '/),&
       &0.0, 0.0, 0.0,&
       &0.1717,& !cn
       &0.1475,& !cnsi
       &0.4598,& !dg
       &-0.0159,& !dg**2
       &0.3659,& !dbh additive
       &-0.0060,& !dbh multiplicative
       &-0.00085& !ht**2
       &) /

  data LTHG(38) /&
       &LTHG_STR(& ! ICH+IDF SE 18m 31-47
       &(/ 8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0/),&
       &(/'IDFdk2/05      ', 'IDFdk2/07      ',&
       &'IDFdm1/05      ', 'IDFmw1/01      ',&
       &'IDFmw1/01-yc   ', 'IDFmw2/01      ',&
       &'IDFmw2/05      ', 'IDFxh1/08      ',&
       &'IDFxh2/07      ', 'ICHmw1/03      ',&
       &'ICHmw1/04      ', 'ICHmw1/07      ',&
       &'ICHmw2/04      ', 'ICHmw2/08      ',&
       &'ICHwk1/08      ', 'IDFdm2/04      ',&
       &'SBSdw2/01      ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &0.0, 0.0, 0.0,&
       &0.1717,& !cn
       &0.1475,& !cnsi
       &0.4598,& !dg
       &-0.0159,& !dg**2
       &0.3659,& !dbh additive
       &-0.0060,& !dbh multiplicative
       &-0.00085& !ht**2
       &) /

  data LTHG(39) /&
       &LTHG_STR(& ! ICH+IDF SE 21m 1-30
       &(/ 8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'ICHdk/01       ', 'ICHdk/04       ',&
       &'ICHdk/05       ', 'ICHmk3/01      ',&
       &'ICHmk3/05      ', 'ICHmk3/07      ',&
       &'ICHwk2/01      ', 'ICHwk2/05      ',&
       &'ICHwk2/06      ', 'ICHwk2/08      ',&
       &'ICHwk4/01      ', 'ICHwk4/06      ',&
       &'IDFxw/07       ', 'ICHmk1/05      ',&
       &'ICHmk1/06      ', 'ICHmk2/05      ',&
       &'ICHmw2/06      ', 'ICHmw3/06      ',&
       &'ICHvk1/02      ', 'ICHvk1/03      ',&
       &'ICHvk1/05      ', 'ICHwk1/04      ',&
       &'ICHwk1/06      ', 'IDFdk1/06      ',&
       &'IDFdk2/06      ', 'IDFdm1/07      ',&
       &'IDFmw1/05      ', 'IDFmw2/04      ',&
       &'IDFxh2/08      ', 'ICHdw/01b      '/),&
       &0.0, 0.0, 0.0,&
       &0.1717,& !cn
       &0.2268,& !cnsi
       &0.4598,& !dg
       &-0.0159,& !dg**2
       &0.3659,& !dbh additive
       &-0.0060,& !dbh multiplicative
       &-0.00085& !ht**2
       &) /

  data LTHG(40) /&
       &LTHG_STR(& ! ICH+IDF SE 21m 31-39
       &(/ 8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'ICHmw1/01      ', 'ICHmw1/05      ',&
       &'ICHmw1/06      ', 'ICHmw2/07      ',&
       &'ICHwk1/07      ', 'IDFdm2/05      ',&
       &'SBSdw1/01      ', 'SBSmh/01       ',&
       &'SBSmk/01       ', '               ',&
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
       &0.0, 0.0, 0.0,&
       &0.1717,& !cn
       &0.2268,& !cnsi
       &0.4598,& !dg
       &-0.0159,& !dg**2
       &0.3659,& !dbh additive
       &-0.0060,& !dbh multiplicative
       &-0.00085& !ht**2
       &) /

  data LTHG(41) /&
       &LTHG_STR(& ! ICH+IDF SE 24m 1-16
       &(/ 8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'ICHmk3/04      ', 'ICHmk3/06      ',&
       &'ICHwk2/07      ', 'ICHwk4/07      ',&
       &'ICHmw2/04      ', 'ICHmw2/05      ',&
       &'ICHmw3/07      ', 'ICHvk1/01      ',&
       &'ICHvk1/04      ', 'ICHwk1/01      ',&
       &'ICHwk1/05      ', 'IDFdm1/06      ',&
       &'IDFmw1/06      ', 'ICHdw/03       ',&
       &'ICHdw/04       ', 'ICHmw2/06      ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &0.0, 0.0, 0.0,&
       &0.1717,& !cn
       &0.5203,& !cnsi
       &0.4598,& !dg
       &-0.0159,& !dg**2
       &0.3659,& !dbh additive
       &-0.0060,& !dbh multiplicative
       &-0.00085& !ht**2
       &) /

  data LTHG(42) /&
       &LTHG_STR(& ! ICH+IDF SE Class 66+99 (Not in SIBEC table)
       !         IDFdk3/01 added manually: AAZ
       &(/ 8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'IDFdk3/04      ', 'IDFdk3/01      ',&
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
       &0.0, 0.0, 0.0,&
       &0.1717,& !cn
       &0.0511,& !cnsi
       &0.4598,& !dg
       &-0.0159,& !dg**2
       &0.3659,& !dbh additive
       &-0.0060,& !dbh multiplicative
       &-0.00085& !ht**2
       &) /

  data LTHG(43) /&
       &LTHG_STR(& ! ICH+IDF SE Class 77 & 88 (Not in SIBEC table)
       &(/ 8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'IDFdm2/01      ', 'IDFdm1/04      ',&
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
       &0.0, 0.0, 0.0,&
       &0.1717,& !cn
       &0.0,& !cnsi
       &0.4598,& !dg
       &-0.0159,& !dg**2
       &0.3659,& !dbh additive
       &-0.0060,& !dbh multiplicative
       &-0.00085& !ht**2
       &) /

  data LTHG(44) /&
       &LTHG_STR(& ! ICH+IDF BL 15m 1-9
       &(/ 9, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'ICHmk1/03      ', 'ICHmk2/03      ',&
       &'ICHmw2/02      ', 'ICHmw3/03      ',&
       &'ICHmw3/08      ', 'ICHvk1/06      ',&
       &'ICHwk1/02      ', 'ICHmw1/02      ',&
       &'ICHmw2/03      ', 'ICH/all        ',&
       &'IDF/all        ', 'SBPS/all       ',&
       &'SBS/all        ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &0.0,&
       &0.0545,&
       &-0.00071,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(45) /&
       &LTHG_STR(& ! ICH+IDF BL 18m 1-30
       &(/ 9, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
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
       &'ICHwk1/07      ', 'IDFdk1/05      ',&
       &'IDFdk1/06      ', 'IDFdm1/05      ',&
       &'IDFdm1/07      ', 'ICHdw/01a      ',&
       &'ICHmw1/01      ', 'ICHmw1/03      ',&
       &'ICHmw1/04      ', 'ICHmw1/07      '/),&
       &0.0,&
       &0.0617,&
       &-0.00071,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(46) /&
       &LTHG_STR(& ! ICH+IDF BL 18m 31-35
       &(/ 9, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'ICHmw2/04      ', 'ICHmw2/08      ',&
       &'ICHwk1/08      ', 'SBSdw2/01      ',&
       &'SBSdw1/01      ', '               ',&
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
       &0.0,&
       &0.0617,&
       &-0.00071,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(47) /&
       &LTHG_STR(& ! ICH+IDF BL 21m 1-19
       &(/ 9, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'ICHmk1/05      ', 'ICHmk1/06      ',&
       &'ICHmk2/05      ', 'ICHmw2/04      ',&
       &'ICHmw2/05      ', 'ICHmw3/06      ',&
       &'ICHmw3/07      ', 'ICHvk1/01      ',&
       &'ICHvk1/04      ', 'ICHwk1/01      ',&
       &'ICHwk1/05      ', 'IDFdm1/06      ',&
       &'ICHdw/03       ', 'ICHdw/04       ',&
       &'ICHmw1/05      ', 'ICHmw1/06      ',&
       &'ICHmw2/05      ', 'ICHmw2/06      ',&
       &'SBSmh/01       ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &-0.6005,&
       &0.0857,&
       &-0.00071,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(48) /&
       &LTHG_STR(& ! ICH+IDF BL 24m 1-1
       &(/ 9, 0,0,0,0,0,0,0,0,0,0,0,0,0,0/),&
       &(/'ICHdw/01b      ', '               ',&
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
       &-1.0159,&
       &0.1335,&
       &-0.00071,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(49) /&
       &LTHG_STR(& ! ICH+IDF PY 12m 1-11
       &(/10, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'IDFxw/01       ', 'IDFxw/02       ',&
       &'IDFxw/03       ', 'IDFxw/04       ',&
       &'IDFdk1/02      ', 'IDFdk1/03      ',&
       &'IDFdk2/02      ', 'IDFdk2/03      ',&
       &'IDFxh1/02      ', 'IDFxh2/02      ',&
       &'IDFxh2/03      ', 'ICH/all        ',&
       &'IDF/all        ', 'SBPS/all       ',&
       &'SBS/all        ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &0.0,&
       &0.0,&
       &-0.00030,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(50) /&
       &LTHG_STR(& ! ICH+IDF PY 15m 1-15
       &(/10, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'IDFdk1/04      ', 'IDFdk2/04      ',&
       &'IDFdm1/03      ', 'IDFmw1/02      ',&
       &'IDFmw2/02      ', 'IDFmw2/03      ',&
       &'IDFxh1/03      ', 'IDFxh1/04      ',&
       &'IDFxh1/05      ', 'IDFxh2/01      ',&
       &'IDFxh2/04      ', 'IDFxh2/05      ',&
       &'IDFxh2/06      ', 'ICHmk1/02      ',&
       &'IDFdm2/03      ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &-1.6556,&
       &0.0616,&
       &-0.00030,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(51) /&
       &LTHG_STR(& ! ICH+IDF PY 18m 1-12
       &(/10, 0,0,0,0,0,0,0,0,0,0,0,0,0,0/),&
       &(/'IDFdm1/01      ', 'IDFdm1/04      ',&
       &'IDFmw1/03      ', 'IDFmw1/04      ',&
       &'IDFmw2/01      ', 'IDFww/03       ',&
       &'IDFxh1/01      ', 'IDFxh1/06      ',&
       &'IDFxh1/07      ', 'ICHmk1/03      ',&
       &'IDFdm2/01      ', 'IDFdm2/04      ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               ',&
       &'               ', '               '/),&
       &-1.7829,&
       &0.0748,&
       &-0.00030,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  !     SIBEC 21 absent from AAZs table

  data LTHG(52) /&
       &LTHG_STR(& ! ICH+IDF PY 21m 1-8 -> all use 18m coefficients
       &(/10, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'IDFdm1/05      ', 'IDFmw1/01      ',&
       &'IDFmw1/01-yc   ', 'IDFxh1/08      ',&
       &'IDFxh2/07      ', 'ICHdw/01a      ',&
       &'ICHdw/02       ', 'ICHxw/01       ',&
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
       &-1.7829,&
       &0.0748,&
       &-0.00030,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(53) /&
       &LTHG_STR(& ! ICH+IDF PY 24m 1-3
       &(/10, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'IDFww/01       ', 'IDFww/04       ',&
       &'ICHdw/01b      ', '               ',&
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
       &0.0,&
       &0.0550,&
       &-0.00030,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(54) /&
       &LTHG_STR(& ! ICH+IDF PY Class 66 (Not in SIBEC table)
       &(/10, 0,0,0,0,0,0,0,0,0,0,0,0,0,0/),&
       &(/'IDFdk1/01      ', '               ',&
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
       &0.0,&
       &0.0382,&
       &-0.00030,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(55) /&
       &LTHG_STR(& ! ICH+IDF PY Class 77 (Not in SIBEC table)
       &(/10, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'IDFdk1/05      ', '               ',&
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
       &0.0,&
       &0.0335,&
       &-0.00030,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(56) /&
       &LTHG_STR(& ! ICH+IDF PY Class 88 (Not in SIBEC table)
       &(/10, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'IDFdk2/01      ', '               ',&
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
       &0.0,&
       &0.0,&
       &-0.00030,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0&
       &) /

  data LTHG(57) /&
       &LTHG_STR (& ! ICH+IDF EP,AT,AC,OH all sites - AT model
       &(/11,12,13,15, 0,0,0,0,0,0,0,0,0,0,0 /),&
       &(/'ICH/all        ', 'IDF/all        ',&
       &'SBPSmk/01      ', 'SBSdw2/01      ',&
       &'SBPS/all       ', 'SBS/all        ',&
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
       &0.0, 0.0, 0.0,&
       &0.0,& !cn
       &0.0,& !cnsi
       &2.8176,& !dg
       &-0.2574,& !dg**2
       &-0.2224,& !dbh additive
       &-0.00098,& !dbh multiplicative
       &-0.00085& !ht**2
       &) /

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'HTGF',4,ICYC)
  SCALE = FINT/YR
  ISMALL = 0

  !  GET THE HEIGHT GROWTH MULTIPLIERS.

  call MULTS (2,IY(ICYC),XHMULT)

  !   BEGIN SPECIES LOOP:

  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (I1.eq.0) go to 40
     I2 = ISCT(ISPC,2)
     XHT = 1.0
     XHT = XHMULT(ISPC)

     !   BEGIN TREE LOOP WITHIN SPECIES LOOP

     do I3 = I1,I2
        I = IND1(I3)
        HTI = HT(I)
        D = DBH(I)

        HTG(I) = 0.
        if (PROB(I).le.0.0) then
           if (LTRIP) then
              ITFN = ITRN+2*I-1
              HTG(ITFN) = 0.
              HTG(ITFN+1) = 0.
           end if
           go to 30
        end if

        !       HEIGHT GROWTH EQUATION, EVALUATED FOR EACH TREE EACH CYCLE
        !       MULTIPLIED BY SCALE TO CHANGE FROM A YR. PERIOD TO FINT AND
        !       MULTIPLIED BY XHT TO APPLY USER SUPPLIED GROWTH MULTIPLIERS.

        ITFN = ITRN+2*I-1
        Y = SCALE*XHT

        !    APPLY DWARF MISTLETOE HEIGHT GROWTH IMPACT HERE,
        !    INSTEAD OF AT EACH FUNCTION IF SPECIAL CASES EXIST.

        Y = Y * MISHGF(I,ISPC)
        if (LV2ATV) then

           CON = HTCON(ISPC)+H2COF*HTI*HTI+HGLD(ISPC)*ALOG(D)+&
                &HGLH*ALOG(HTI)
           HTG(I) = (EXP(CON+HDGCOF*ALOG(DG(I)))+BIAS) * Y
           if (LTRIP) then
              HTG(ITFN) = (EXP(CON+HDGCOF*ALOG(DG(ITFN)))  +BIAS) * Y
              HTG(ITFN+1) = (EXP(CON+HDGCOF*ALOG(DG(ITFN+1)))+BIAS) * Y
           end if

        else

           !         NOTE: IF TRIPLING IS SCHEDULED, THE VALUE OF HT(ITFN & ITFN+1) IS
           !         NOT YET DEFINED (CALL TO TRIPLE IS LATER); IN THIS CASE JUST
           !         USE HTI=HT(I); D=DBH(ITFN) AND DG(ITFN) ARE BOTH DEFINED AT THIS
           !         POINT.

           HTG(I) = V3HTG(ISPC,HTI,D,DG(I)) * Y
           if (LTRIP) then
              HTG(ITFN) = V3HTG(ISPC,HTI,D,DG(ITFN)) * Y
              HTG(ITFN+1) = V3HTG(ISPC,HTI,D,DG(ITFN+1)) * Y
           end if

        end if

        !       END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.

        if (DEBUG) then
           HTNEW = HT(I)+HTG(I)
           write (JOSTND,9000) HTG(I),CON,HTCON(ISPC),H2COF,D,&
                &WK1(I),HGLH,HTNEW,HDGCOF,I,ISPC
           9000 format(' 9000 HTGF, HTG=',F8.4,' CON=',F8.4,' HTCON=',F8.4,&
                &' H2COF=',F12.8,' D =',F8.4/' WK1=',F8.4,' HGLH=',F8.4,&
                &' HTNEW=',F8.4,' HDGCOF=',F8.4,' I=',I4,' ISPC=',I2)
        end if

        !       CHECK FOR SIZE CAP COMPLIANCE.

        if ((HT(I)+HTG(I)).gt.SIZCAP(ISPC,4)) then
           HTG(I) = SIZCAP(ISPC,4)-HT(I)
           if (HTG(I) .lt.0.1) HTG(I) = 0.1
        end if
        if (LTRIP) then
           HTG(ITFN) = HTG(I)
           HTG(ITFN+1) = HTG(I)
        end if

        if (DEBUG.and.LTRIP) write(JOSTND,9001) HTG(ITFN),HTG(ITFN+1)
        9001 format( ' UPPER HTG =',F8.4,' LOWER HTG =',F8.4)

        !       END OF TREE LOOP

        30 continue
     end do

     !     END OF SPECIES LOOP

     40 continue
  end do
  return

  !  ENTRY POINT FOR LOADING HEIGHT INCREMENT MODEL COEFFICIENTS THAT ARE
  !  SITE DEPENDENT AND REQUIRE ONE-TIME RESOLUTION.  HGHC CONTAINS
  !  HABITAT TYPE INTERCEPTS, HGLDD CONTAINS HABITAT DEPENDENT
  !  COEFFICIENTS FOR THE DIAMETER INCREMENT TERM, HGH2 CONTAINS HABITAT
  !  DEPENDENT COEFFICIENTS FOR THE HEIGHT-SQUARED TERM, AND HGHC CONTAINS
  !  SPECIES DEPENDENT INTERCEPTS.  HABITAT TYPE IS INDEXED BY ITYPE (SEE
  !  /PLOT/ COMMON AREA).

  !  HEIGHT DUBBING (HT1, HT2) FOR V3 HAPPENS HERE; IN V2 IT HAPPENS IN BECSET
  !  (TO ALLOW COMPATIBILITY WITH PRINTING OF COEFFICIENTS IN MAIN OUTPUT)

  entry HTCONS

  !     V2
  data MAPHAB/&
       &1,1, 7*2, 3,3,4,5,6, 4*7, 4,4,1,4,4, 3*8, 4*1/
  data  HGHC /&
       &2.03035, 1.72222, 1.19728, 1.81759, 2.14781, 1.76998, 2.21104,&
       &1.74090/
  data  HGLDD /&
       &0.62144, 1.02372, 0.85493, 0.75756, 0.46238, 0.49643, 0.37042,&
       &0.34003/
  data  HGH2 /&
       &-13.358E-05, -3.809E-05, -3.715E-05, -2.607E-05, -5.200E-05,&
       &-1.605E-05, -3.631E-05, -4.460E-05/
  data  HGSC /&
       &-.5342,.1433,.1641,-.6458,-.6959,-.9941,-.6004,.2089,&
       &-.5478,.7316,-.9941,-.9941,-.9941,.1641,-.9941 /

  !     V2 - ASSIGN HABITAT DEPENDENT COEFFICIENTS.

  if (LV2ATV) then

     IHT = MAPHAB(ITYPE)
     HGHCH = HGHC(IHT)
     H2COF = HGH2(IHT)
     HDGCOF = HGLDD(IHT)
     SEISHT = 1.0 ! V2 SMALL TREE MODEL

  else

     !       REPLACE NULLS IN COMPILED DATA STRINGS WITH SPACES, SO THAT
     !       STRING LENGTHS WILL BE FIGURED CORRECTLY
     do J = 1,P_MD
        do K = 1,P_SS
           do M = 1,15
              if (ICHAR(LTHG(J)%PrettyName(K)(M:M)) .eq.0)&
                   &LTHG(J)%PrettyName(K)(M:M) = ' '
           end do
        end do
     end do

     !     LOCATE ZONE AND SITE SERIES; IF NO PERFECT MATCH TO SUBZONE
     !     CAN BE FOUND, TRY ".../all" AND LOOP ONE MORE TIME BEFORE
     !     GIVING UP ON A MATCH.

     LTH%PrettyName = BEC%PrettyName
     do I = 1,MAXSP
        IPOS(I) = 0
     end do

     do I = 1,MAXSP

        IPAS = 0
        PAT = BEC%PrettyName

        7 do J = 1,P_MD
           do K = 1,MAXSP
              if (LTHG(J)%SPP(K) .eq.0) exit
              if (LTHG(J)%SPP(K) .eq. I) then
                 do M = 1,P_SS
                    if (LEN_TRIM(LTHG(J)%PrettyName(M))  .eq.0) exit
                    if (INDEX(LTHG(J)%PrettyName(M),PAT) .gt.0) then
                       IPOS(I) = J
                       goto 8
                    end if
                 end do
              end if
           end do
        end do

        if (J .gt. P_MD .and. IPAS .eq.0) then
           IPAS = 1
           PAT = BEC%Zone(1:LEN_TRIM(BEC%Zone))//'/all'
           goto 7
        end if

     8 end do

     !     COPY COEFFICIENTS INTO LTHG STRUCTURE

     do I = 1,MAXSP

        LTH%FIT(I) = .false.
        J = IPOS(I)
        if (J .gt.0) then
           LTH%FIT(I) = .true.

           LTH%A1(I) = LTSP(I,1) ! old v3 model spp constants
           LTH%B1(I) = LTSP(I,2)
           LTH%B2(I) = LTSP(I,3)

           LTH%SI(I) = LTHG(J)%SI ! old v3 model site terms
           LTH%B3(I) = LTHG(J)%B3
           LTH%C(I) = LTHG(J)%C

           LTH%CN(I) = LTHG(J)%CN ! new v3 model
           LTH%CNSI(I) = LTHG(J)%CNSI
           LTH%DG(I) = LTHG(J)%DG
           LTH%DG2(I) = LTHG(J)%DG2
           LTH%DBH1(I) = LTHG(J)%DBH1
           LTH%DBH2(I) = LTHG(J)%DBH2
           LTH%HT2(I) = LTHG(J)%HT2
        end if
     end do
  end if

  !  LOAD OVERALL INTERCEPT MULTIPLIER FOR EACH SPECIES.

  if (LV2ATV) then

     do ISPC = 1,MAXSP
        HTCON(ISPC) = HGHCH + HGSC(ISPC)
        if (LHCOR2.and. HCOR2(ISPC).gt.0.0)&
             &HTCON(ISPC) = HTCON(ISPC) + ALOG(HCOR2(ISPC))
     end do

  else

     do ISPC = 1,MAXSP
        LTH%MLT(ISPC) = 1.0
        if (LTH%FIT(ISPC)) then
           if (LHCOR2.and. HCOR2(ISPC).gt.0.0)&
                &LTH%MLT(ISPC) = HCOR2(ISPC)
        end if
     end do

  end if

  !     STEP 2:
  !     ENTRY POINT FOR REFIT OF HEIGHT DUBBING MODEL FOR VERSION 3
  !     THIS ENTRY POINT IS FOR ICH/IDF ONLY; OTHERS ARE DONE IN BECSET

  !     FIND AND REPLACE HEIGHT COEFFICIENTS HELD IN
  !     ARRAYS HT1() AND HT2() (C_0 AND C_1 OF HT/DIAM EQN)

  !     IF NOT YET DONE, PUT COMPILE-TIME HT1,HT2 INTO ARCHIVE VARIABLES
  !     THIS USES A V2 FLAG BECAUSE HEIGHT DUBBING IS A HOLD-OVER FROM V2
  !     THAT IS BASICALLY UNALTERED IN V3

  if (LV2HTCP) then
     LV2HTCP = .false.
     do I = 1,MAXSP
        XHT1B(I) = HT1(I)
        XHT2B(I) = HT2(I)
     end do
  end if

  !     COPY TEMPORARY VALUES FROM ARCHIVE

  do I = 1,MAXSP
     XHT1(I) = XHT1B(I)
     XHT2(I) = XHT2B(I)
  end do

  !     SEARCH FOR MATCHES AND REPLACE IF FOUND

  do I = 1,MAXSP

     if (I.eq.1) then ! western white pine - PW
        if (INDEX(BEC%Zone,'ICH') .gt.0) then
           if (INDEX(BEC%SubZone,'mw') .gt.0) then
              XHT1(I) = 3.9738
              XHT2(I) = -20.8459
           else if (INDEX(BEC%SubZone,'wk') .gt.0) then
              XHT1(I) = 3.7771
              XHT2(I) = -15.8162
           end if
        end if

     else if (I.eq.2) then ! western larch - LW
        if (INDEX(BEC%Zone,'ICH') .gt.0) then
           if (INDEX(BEC%SubZone,'dw') .gt.0.or.&
                &INDEX(BEC%SubZone,'mk') .gt.0) then
              XHT1(I) = 3.9045
              XHT2(I) = -22.0149
           else if (INDEX(BEC%SubZone,'mw') .gt.0) then
              XHT1(I) = 3.7825
              XHT2(I) = -14.5352
           end if
        else if (INDEX(BEC%Zone,'IDF') .gt.0) then
           if (INDEX(BEC%SubZone,'dm') .gt.0.or.&
                &INDEX(BEC%SubZone,'xh') .gt.0) then
              XHT1(I) = 3.9012
              XHT2(I) = -19.1924
           end if
        end if

     else if (I.eq.3.or. I.eq.14) then ! Douglas-fir - FD (&OC)
        if (INDEX(BEC%Zone,'ICH') .gt.0.or.&
             &INDEX(BEC%PrettyName,'SBSdw1') .gt.0.or.&
             &INDEX(BEC%PrettyName,'SBSmh') .gt.0) then
           if (INDEX(BEC%SubZone,'dk') .gt.0.or.&
                &INDEX(BEC%SubZone,'mk') .gt.0.or.&
                &INDEX(BEC%PrettyName,'SBSdw1') .gt.0.or.&
                &INDEX(BEC%PrettyName,'SBSmh') .gt.0) then
              XHT1(I) = 3.9432
              XHT2(I) = -25.9091
           else if (INDEX(BEC%SubZone,'mw') .gt.0) then
              XHT1(I) = 3.9274
              XHT2(I) = -20.1322
           else if (INDEX(BEC%SubZone,'wk') .gt.0) then
              XHT1(I) = 4.1218
              XHT2(I) = -29.6135
           else if (INDEX(BEC%SubZone,'dw') .gt.0) then
              XHT1(I) = 3.7859
              XHT2(I) = -22.0166
           end if
        else if (INDEX(BEC%Zone,'IDF') .gt.0.or.&
             &INDEX(BEC%Zone,'SBPS') .gt.0.or.&
             &INDEX(BEC%PrettyName,'SBSdw2') .gt.0) then
           if (INDEX(BEC%SubZone,'dk') .gt.0.or.&
                &INDEX(BEC%SubZone,'mw') .gt.0.or.&
                &INDEX(BEC%SubZone,'xm') .gt.0.or.&
                &INDEX(BEC%Zone,'SBPS') .gt.0.or.&
                &INDEX(BEC%PrettyName,'SBSdw2') .gt.0) then
              XHT1(I) = 3.7049
              XHT2(I) = -22.9781
           else if (INDEX(BEC%SubZone,'dm') .gt.0.or.&
                &INDEX(BEC%SubZone,'ww') .gt.0) then
              XHT1(I) = 3.7877
              XHT2(I) = -23.1796
           else if (INDEX(BEC%SubZone,'xh') .gt.0) then
              XHT1(I) = 3.6337
              XHT2(I) = -18.2998
           end if
        end if

     else if (I.eq.5) then ! western hemlock - HW
        if (INDEX(BEC%Zone,'ICH') .gt.0) then
           if (INDEX(BEC%SubZone,'vc') .gt.0.or.&
                &INDEX(BEC%SubZone,'vk') .gt.0) then
              XHT1(I) = 3.9685
              XHT2(I) = -30.4439
           else if (INDEX(BEC%SubZone,'mw') .gt.0.or.&
                &INDEX(BEC%SubZone,'wk') .gt.0) then
              XHT1(I) = 3.9491
              XHT2(I) = -26.1417
           else if (INDEX(BEC%SubZone,'mc') .gt.0) then
              XHT1(I) = 3.8170
              XHT2(I) = -21.3197
           end if
        else if (INDEX(BEC%Zone,'IDF') .gt.0) then
           if (INDEX(BEC%SubZone,'mm') .gt.0) then
              XHT1(I) = 3.2884
              XHT2(I) = -12.9475
           else if (INDEX(BEC%SubZone,'wv') .gt.0.or.&
                &INDEX(BEC%SubZone,'ww') .gt.0.or.&
                &INDEX(BEC%SubZone,'wc') .gt.0) then
              XHT1(I) = 4.0577
              XHT2(I) = -28.1854
           end if
        end if

     else if (I.eq.6) then ! western redcedar - CW
        if (INDEX(BEC%Zone,'ICH') .gt.0) then
           if (INDEX(BEC%SubZone,'mw') .gt.0 ) then
              XHT1(I) = 3.8565
              XHT2(I) = -25.7777
           else if (INDEX(BEC%SubZone,'mm') .gt.0.or.&
                &INDEX(BEC%SubZone,'vk') .gt.0) then
              XHT1(I) = 4.1251
              XHT2(I) = -41.8833
           else if (INDEX(BEC%SubZone,'wk') .gt.0.or.&
                &INDEX(BEC%SubZone,'mc') .gt.0.or.&
                &INDEX(BEC%SubZone,'mk') .gt.0) then
              XHT1(I) = 3.9432
              XHT2(I) = -30.5647
           end if
        else if (INDEX(BEC%Zone,'IDF') .gt.0) then
           if (INDEX(BEC%SubZone,'dm') .gt.0 ) then
              XHT1(I) = 3.2692
              XHT2(I) = -10.1899
           else if (INDEX(BEC%SubZone,'ww') .gt.0) then
              XHT1(I) = 4.0537
              XHT2(I) = -36.7598
           end if
        end if

     else if (I.eq.7) then ! lodgepole pine - PL
        if (INDEX(BEC%Zone,'ICH') .gt.0.or.&
             &INDEX(BEC%PrettyName,'SBSdw1') .gt.0.or.&
             &INDEX(BEC%PrettyName,'SBSmh') .gt.0) then
           if (INDEX(BEC%SubZone,'mk') .gt.0.or.&
                &INDEX(BEC%SubZone,'mw') .gt.0.or.&
                &INDEX(BEC%SubZone,'mc') .gt.0.or.&
                &INDEX(BEC%PrettyName,'SBSdw1') .gt.0.or.&
                &INDEX(BEC%PrettyName,'SBSmh') .gt.0) then
              XHT1(I) = 3.8470
              XHT2(I) = -18.5836
           end if
        else if (INDEX(BEC%Zone,'IDF') .gt.0.or.&
             &INDEX(BEC%Zone,'SBPS') .gt.0.or.&
             &INDEX(BEC%PrettyName,'SBSdw2') .gt.0) then
           if (INDEX(BEC%SubZone,'xh') .gt.0.or.&
                &INDEX(BEC%SubZone,'dm') .gt.0) then
              XHT1(I) = 3.5683
              XHT2(I) = -13.0736
           else if (INDEX(BEC%SubZone,'dk') .gt.0.or.&
                &INDEX(BEC%Zone,'SBPS') .gt.0.or.&
                &INDEX(BEC%PrettyName,'SBSdw2') .gt.0) then
              XHT1(I) = 3.1192
              XHT2(I) = -8.2208
           end if
        end if

     else if (I.eq.8) then ! Engelmann spruce - SE
        if (INDEX(BEC%Zone,'ICH') .gt.0.or.&
             &INDEX(BEC%Zone,'IDF') .gt.0.or.&
             &INDEX(BEC%Zone,'SBPS') .gt.0.or.&
             &INDEX(BEC%Zone,'SBS') .gt.0) then
           if (INDEX(BEC%SubZone,'dk') .gt.0.or.&
                &INDEX(BEC%Zone,'SBPS') .gt.0.or.&
                &INDEX(BEC%PrettyName,'SBSdw2') .gt.0) then
              XHT1(I) = 3.6519
              XHT2(I) = -17.8872
           else if (INDEX(BEC%SubZone,'dm') .gt.0.or.&
                &INDEX(BEC%SubZone,'mk') .gt.0.or.&
                &INDEX(BEC%SubZone,'mc') .gt.0.or.&
                &INDEX(BEC%Zone,'SBS') .gt.0) then
              XHT1(I) = 4.0115
              XHT2(I) = -25.0730
           else if (INDEX(BEC%SubZone,'mw') .gt.0) then
              XHT1(I) = 3.9729
              XHT2(I) = -22.6984
           else if (INDEX(BEC%SubZone,'wk') .gt.0) then
              XHT1(I) = 4.1277
              XHT2(I) = -29.2780
           end if
        end if

     else if (I.eq.9.or. I.eq.4) then ! subalpine fir - BL, BG
        if (INDEX(BEC%Zone,'ICH') .gt.0.or.&
             &INDEX(BEC%Zone,'IDF') .gt.0.or.&
             &INDEX(BEC%Zone,'SBS') .gt.0) then
           if (INDEX(BEC%SubZone,'mk') .gt.0.or.&
                &INDEX(BEC%SubZone,'vk') .gt.0.or.&
                &INDEX(BEC%SubZone,'dk') .gt.0.or.&
                &INDEX(BEC%Zone,'SBS') .gt.0) then
              XHT1(I) = 3.7656
              XHT2(I) = -20.1044
           else if (INDEX(BEC%SubZone,'mw') .gt.0.or.&
                &INDEX(BEC%SubZone,'wk') .gt.0.or.&
                &INDEX(BEC%SubZone,'mc') .gt.0) then
              XHT1(I) = 3.9598
              XHT2(I) = -24.0969
           end if
        end if

     else if (I.eq.10) then ! ponderosa pine - PY
        if (INDEX(BEC%Zone,'ICH') .gt.0) then
           if (INDEX(BEC%SubZone,'dw') .gt.0) then
              XHT1(I) = 4.0178
              XHT2(I) = -27.1633
           else if (INDEX(BEC%SubZone,'mk') .gt.0) then
              XHT1(I) = 3.7798
              XHT2(I) = -29.9875
           end if
        else if (INDEX(BEC%Zone,'IDF') .gt.0) then
           if (INDEX(BEC%SubZone,'xh') .gt.0.or.&
                &INDEX(BEC%SubZone,'dm') .gt.0) then
              XHT1(I) = 3.7948
              XHT2(I) = -26.8113
           else if (INDEX(BEC%SubZone,'dk') .gt.0) then
              XHT1(I) = 3.8113
              XHT2(I) = -29.7283
           end if
        end if
     end if
  end do

  !     COPY TEMPORARY TO PERMANENT ARRAYS

  do I = 1,MAXSP
     HT1(I) = XHT1(I)
     HT2(I) = XHT2(I)
  end do

  !     SET FLAG FOR WHETHER METRIC DUB IS USED, OR NOT

  do I = 1,MAXSP
     if ((HT1(I) .ne. XHT1B(I)) .or.&
          &(HT2(I) .ne. XHT2B(I))) then
        LMHTDUB(I) = .true.
     else
        LMHTDUB(I) = .false.
     end if
  end do

  return
end

  !     V3HTG--SEI
  !     THIS PRIVATE FUNCTION LOCALIZES THE V3 CALCULATIONS FOR HEIGHT GROWTH;
  !     OTHERWISE THEY WOULD BE REPEATED SEVERAL TIMES

  ! Variables
real function V3HTG(ISPC,HT,DBH,DG)
  implicit none

  ! Common blocks
  include 'PRGPRM.f90'
  include 'BCPLOT.f90'
  include 'METRIC.f90'

  integer :: ISPC
  real    :: HT,DBH,DG
  real    :: HTGM,HTM,D,DGM

  V3HTG = 0.0
  if (.not. LTH%FIT(ISPC)) return

  HTM = HT  * FTtoM
  D = DBH * INtoCM
  DGM = DG  * INtoCM

  select case (ISPC)
  case (7,8,11,12,13,15) ! NEW v3 MODEL
     HTGM = (LTH%CN(ISPC) + LTH%CNSI(ISPC) +&
          &(LTH%DG(ISPC) * DGM + LTH%DG2(ISPC) * DGM * DGM)) *&
          &(HTM ** (LTH%DBH1(ISPC) + LTH%DBH2(ISPC) * D)) *&
          &EXP(LTH%HT2(ISPC) * HTM * HTM)
  case default ! OLD V3 MODEL
     HTGM = (LTH%A1(ISPC) + LTH%SI(ISPC)) *&
          &HTM ** (LTH%B1(ISPC) +&
          &(LTH%B2(ISPC) * D) + (LTH%B3(ISPC) * DGM)) *&
          &EXP(LTH%C(ISPC) * HTM * HTM)
  end select
  V3HTG = HTGM * MtoFT * LTH%MLT(ISPC)

  return
end
