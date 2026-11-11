!========================!
!   canada/bc : becset   !
!========================!
! CANADA-BC $Id$
subroutine BECSET
  implicit none

  !     THE PRINCIPAL TASK IS TO RESOLVE SITE AND BEC INFORMATION
  !     PROVIDED BY THE BECINFO KEYWORD, DETERMINE WHICH OF THE
  !     PROGNOSIS (NI-VARIANT) HABITAT CODES IS MATCHED WITH THE
  !     BEC VARIANT, AND PROVIDE SPECIES- AND BEC-SPECIFIC ADJUSTMENT
  !     COEFFICIENTS TO BE APPLIED TO THE LARGE TREE DIAMETER GROWTH
  !     MODEL. COEFFICIENTS ARE BASED ON FITTING A LINEAR REGRESSION
  !     MODEL TO THE DBH SCALING TERM REPORTED BY PROGNOSIS, APPLIED
  !     TO PERMANENT SAMPLE PLOT TREELISTS STRATIFIED BY BEC, IN THE
  !     KAMLOOPS AND NELSON FOREST REGIONS.

  !     THE 'KAMCODE' AND 'NELCODE' STRINGS ARE SET UP SO THAT IT IS
  !     EASY TO FIND THE BEC VARIANT WHEN READING THE SOURCE CODE.
  !     LIKEWISE, THE BEC-SPECIFIC ADJUSTMENTS ARE SET UP IN THE SAME
  !     PATTERN, SO THAT IT IS QUITE EASY TO CHECK THE VALUES AND ADD
  !     NEW OR MODIFIED VALUES IN THE FUTURE. I ALSO KEPT THE NELSON
  !     AND KAMLOOPS COEFFICIENTS SEPARATE, EVEN THOUGH THEY ARE
  !     IDENTICAL AT PRESENT. THIS WILL ALLOW REGION-SPECIFIC
  !     COEFFICIENTS TO BE ADDED IN THE FUTURE, IF THAT IS DEEMED
  !     NECESSARY.

  !     THE KAMCOD2 AND NELCOD2 STRINGS ARE SET UP TO IDENTIFY THE
  !     BEC SUBZONES WHERE PARAMETERIZATION IS VALID. SPECIES THAT
  !     ARE UNPARAMETERIZED FOR A PARTICULAR SUBZONE ARE FLAGGED .TRUE.
  !     OR .FALSE. AND PASSED TO BECGET. THIS IS NECESSARY BECAUSE
  !     SOME ZEROES IN THE NCOEF AND KCOEF ARRAYS ARE TRUE ZEROES --
  !     THE MODEL USES ONLY THE INTERCEPT TERM -- WHILE OTHER ZEROES
  !     ARE PLACEHOLDERS, AND EVEN THE INTERCEPT SHOULD NOT BE USED.

  !     - DON ROBINSON; ESSA TECHNOLOGIES, VANCOUVER. DEC/97; JULY/99
  !     - ABDEL-AZIM ZUMRAWI: MAY 18, 1999: CCF & XBEC7 ARRAYS
  !     - DON ROBINSON:  JULY,        1999: HEIGHT DUBBING COEFFICIENTS
  !                                         FROM T. HAILMARIAM
  !     - DON ROBINSON:  NOVEMBER 23, 1999: ADDED BAMAX CHANGES
  !     - DON ROBINSON:  NOVEMBER 24, 1999: HEIGHT GROWTH ADJUSTMENT
  !     - DON ROBINSON:  DECEMBER  8, 1999: MORTALITY ADJUSTMENT
  !     - ABDEL-AZIM ZUMRAWI: MARCH 15, 2000: IDFdk3 SUBZONE ADDED TO KAM
  !     - DON ROBINSON:  OCTOBER 10, 2000: UPDATE FD,PL LARGE TREE
  !     - DON ROBINSON:  FEBRUARY 12, 2001; ADD IDFdk SMALL TREE HEIGHT GROWTH FROM C. LENCAR

  ! Common blocks
  include  'PRGPRM.f90'
  include  'PLOT.f90'
  include  'COEFFS.f90'
  include  'CONTRL.f90'
  include  'METRIC.f90'
  include  'BCPLOT.f90'

  ! Variables
  integer :: NCNT,KCNT
  integer :: NCNT2,KCNT2,ACNT2

  parameter (NCNT = 20) ! number of explicit BEC/ss Nelson
  parameter (KCNT = 11) ! number of explicit BEC/ss Kamloops

  parameter (NCNT2 = 9) ! number of explicit BEC/szn Nelson
  parameter (KCNT2 = 6) ! number of explicit BEC/szn Kamloops
  parameter (ACNT2 = 12) ! number of explicit BEC/szn Nelson and Kamloops

  !     DECLARE STRUCTURE FOR LARGE TREE HEIGHT GROWTH MODIFIER
  !     AND MORTALITY MODIFIER

  TYPE LTHG_STR
     character (LEN = 7) :: BECszn
     logical           :: OK
     integer           :: Lo(MAXSP)
     integer           :: Hi(MAXSP)
  end TYPE

  !     LBEC8 IS A COPY OF LTDGOK

  logical :: LBEC8(MAXSP),LOK1,LOK2,LBECNEW

  character(len=1) :: CH(MAXSP)
  character(len=44) :: SPLST
  character(len=15) :: COLDBEC

  !      CHARACTER*10 KAMCODE(KCNT),  NELCODE(NCNT)
  !      CHARACTER*7 KAMCOD2(KCNT2), NELCOD2(NCNT2)
  !      INTEGER     KAMINDX(KCNT), NELINDX(NCNT)
  integer :: I,J,IP
  integer :: IREGN,IBEC,INOSPP,IBECX,IBECY

  !     NEED TO REMEMBER THIS BETWEEN CALLS
  integer :: JBECY
  common  /BECREF/  JBECY

  !     ENTRY SEISTHG VARS
  integer :: ISP1
  real    :: X01,X11,X21,X31,X41,X51,X61

  !     ENTRY SEILTHG VARS
  integer :: ISP2
  real    :: X02,XHT22,ALGSLP

  !     ENTRY SEIMORT VARS
  integer :: ISP3
  real    :: X03,XDIA3

  real    :: INTRCP(MAXSP),BECTERM(MAXSP)
  real    :: SEISLP(MAXSP),SEISAS(MAXSP),SEICAS(MAXSP),SEIELV(MAXSP),SEIEL2(MAXSP),SEICCF(MAXSP)
  real    :: XBEC1(MAXSP),XBEC2(MAXSP),XBEC3(MAXSP),XBEC4(MAXSP),XBEC5(MAXSP),XBEC6(MAXSP),XBEC7(MAXSP)
  real    :: BECADJ(3),XBEC0(3)
  real    :: XHT1(MAXSP),XHT2(MAXSP)
  real    :: XLTHG(4),YLTHG(4),XMORT(4),YMORT(4)

  !     LOGICAL ARRAYS FOR SPECIES/REGION. FOR THE LARGE
  !     TREE DIAMETER GROWTH MODEL. IF .FALSE. NO
  !     BEC PARAMETERS EXIST FOR THE SPECIES, EVEN THOUGH
  !     THERE MAY BE PARAMETERS FOR THE BEC

  logical :: LNOK(NCNT2,MAXSP)
  logical :: LNPW(NCNT2),LNLW(NCNT2),LNFD(NCNT2),LNBG(NCNT2),LNHW(NCNT2),LNCW(NCNT2),LNPL(NCNT2),&
       &LNSE(NCNT2),LNBL(NCNT2),LNPY(NCNT2),LNEP(NCNT2),LNAT(NCNT2),LNAC(NCNT2),LNOC(NCNT2),LNOH(NCNT2)
  logical :: LKOK(KCNT2,MAXSP)
  logical :: LKPW(KCNT2),LKLW(KCNT2),LKFD(KCNT2),LKBG(KCNT2),LKHW(KCNT2),LKCW(KCNT2),LKPL(KCNT2),&
       &LKSE(KCNT2),LKBL(KCNT2),LKPY(KCNT2),LKEP(KCNT2),LKAT(KCNT2),LKAC(KCNT2),LKOC(KCNT2),LKOH(KCNT2)

  equivalence (LNPW, LNOK(1, 1)), (LKPW, LKOK(1, 1))
  equivalence (LNLW, LNOK(1, 2)), (LKLW, LKOK(1, 2))
  equivalence (LNFD, LNOK(1, 3)), (LKFD, LKOK(1, 3))
  equivalence (LNBG, LNOK(1, 4)), (LKBG, LKOK(1, 4))
  equivalence (LNHW, LNOK(1, 5)), (LKHW, LKOK(1, 5))
  equivalence (LNCW, LNOK(1, 6)), (LKCW, LKOK(1, 6))
  equivalence (LNPL, LNOK(1, 7)), (LKPL, LKOK(1, 7))
  equivalence (LNSE, LNOK(1, 8)), (LKSE, LKOK(1, 8))
  equivalence (LNBL, LNOK(1, 9)), (LKBL, LKOK(1, 9))
  equivalence (LNPY, LNOK(1,10)), (LKPY, LKOK(1,10))
  equivalence (LNEP, LNOK(1,11)), (LKEP, LKOK(1,11))
  equivalence (LNAT, LNOK(1,12)), (LKAT, LKOK(1,12))
  equivalence (LNAC, LNOK(1,13)), (LKAC, LKOK(1,13))
  equivalence (LNOC, LNOK(1,14)), (LKOC, LKOK(1,14))
  equivalence (LNOH, LNOK(1,15)), (LKOH, LKOK(1,15))

  !     REAL ARRAYS FOR SPECIES/REGION FOR THE LARGE TREE
  !     DIAMETER GROWTH MODEL

  real    :: NCOEF(NCNT,MAXSP)
  real    :: NPW(NCNT),NLW(NCNT),NFD(NCNT),NBG(NCNT),NHW(NCNT),NCW(NCNT),NPL(NCNT),NSE(NCNT),&
       &NBL(NCNT),NPY(NCNT),NEP(NCNT),NAT(NCNT),NAC(NCNT),NOC(NCNT),NOH(NCNT)
  real    :: KCOEF(KCNT,MAXSP)
  real    :: KPW(KCNT),KLW(KCNT),KFD(KCNT),KBG(KCNT),KHW(KCNT),KCW(KCNT),KPL(KCNT),KSE(KCNT),&
       &KBL(KCNT),KPY(KCNT),KEP(KCNT),KAT(KCNT),KAC(KCNT),KOC(KCNT),KOH(KCNT)

  equivalence (NPW, NCOEF(1, 1)), (KPW, KCOEF(1, 1))
  equivalence (NLW, NCOEF(1, 2)), (KLW, KCOEF(1, 2))
  equivalence (NFD, NCOEF(1, 3)), (KFD, KCOEF(1, 3))
  equivalence (NBG, NCOEF(1, 4)), (KBG, KCOEF(1, 4))
  equivalence (NHW, NCOEF(1, 5)), (KHW, KCOEF(1, 5))
  equivalence (NCW, NCOEF(1, 6)), (KCW, KCOEF(1, 6))
  equivalence (NPL, NCOEF(1, 7)), (KPL, KCOEF(1, 7))
  equivalence (NSE, NCOEF(1, 8)), (KSE, KCOEF(1, 8))
  equivalence (NBL, NCOEF(1, 9)), (KBL, KCOEF(1, 9))
  equivalence (NPY, NCOEF(1,10)), (KPY, KCOEF(1,10))
  equivalence (NEP, NCOEF(1,11)), (KEP, KCOEF(1,11))
  equivalence (NAT, NCOEF(1,12)), (KAT, KCOEF(1,12))
  equivalence (NAC, NCOEF(1,13)), (KAC, KCOEF(1,13))
  equivalence (NOC, NCOEF(1,14)), (KOC, KCOEF(1,14))
  equivalence (NOH, NCOEF(1,15)), (KOH, KCOEF(1,15))

  TYPE (LTHG_STR) LTHGMOD(ACNT2)
  TYPE (LTHG_STR) MORTMOD(ACNT2)

  !     PARAMETERS TO SHIFT (SET TO CONSTANT VALUES): THE
  !     SLOPE (37%), ASPECT (225 DEGREES) AND ELEVATION
  !     (1276 METRES) OF THE BASE MODEL. THIS IS REQUIRED
  !     BECAUSE THESE TERMS WERE ALL HELD FIXED AT THE NI-AVERAGE
  !     VALUE WHEN THE ADJUSTMENT MODEL WAS FITTED.

  data BECADJ / 37, 225, 1276 /

  !     PARAMETERS FOR INTERCEPT, BY SPECIES (BOTH REGIONS) FOR
  !     LARGE TREE DIAMETER GROWTH

  data INTRCP / -1.200116,  0.031636, -0.379727, -0.354087,&
       &-0.531580,  0.303820, -0.592774, -1.467097,&
       &-0.655108, -2.015720,  0.0,       0.0,&
       &0.0,      -0.379727,  0.0 /

  !     PARAMETERS FOR SLOPE, SIN ASPECT, COS ASPECT, ELEVATION
  !     ELEVATION**2 AND CCF (BOTH REGIONS) FOR LARGE TREE DIAMETER
  !     GROWTH

  data SEISLP /  0.0     ,  0.0     ,  0.0     ,  0.0     ,&
       &0.0     ,  0.0     ,  0.0     , -0.819996,&
       &0.0     ,  0.0     ,  0.0     ,  0.0,&
       &0.0     ,  0.0     ,  0.0      /

  data SEISAS /  0.0     ,  0.0     ,  0.000728,  0.0     ,&
       &0.0     ,  0.0     ,  0.0     ,  0.0     ,&
       &0.0     , -1.114708,  0.0      ,  0.0,&
       &0.0     ,  0.000728,  0.0      /

  data SEICAS /  0.0     ,  0.0     ,  0.0     , -0.406713,&
       &0.0     ,  0.0     , -0.091611,  0.0     ,&
       &-0.746618,  0.0     ,  0.0      ,  0.0,&
       &0.0     ,  0.0     ,  0.0      /

  data SEIELV /  0.017243,  0.0     ,  0.000369,  0.057385,&
       &0.046852,  0.0     ,  0.010962,  0.072226,&
       &0.071289,  0.061512,  0.0     ,  0.0,&
       &0.0     ,  0.000369,  0.0      /

  data SEIEL2 /  0.0     ,  0.0     ,  0.0     , -0.000847,&
       &-0.000680,  0.0     ,  0.0     , -0.000898,&
       &-0.000987,  0.0     ,  0.0     ,  0.0,&
       &0.0     ,  0.0     ,  0.0      /

  data SEICCF /  0.0     ,  0.0     , -0.064699,  0.0     ,&
       &0.0     ,  0.0     , -0.160717,  0.0     ,&
       &0.0     ,  0.0     ,  0.0     ,  0.0,&
       &0.0     , -0.064699,  0.0      /

  !C     ALLOWABLE BEC VARIANTS FOR KAMLOOPS

  !      DATA KAMCODE /  'PPxh1/01', 'PPxh1/03',  'PPxh1/06',  'PPxh1/07',
  !     &                'PPxh2/03',
  !     &                 'MSdk/01',  'MSdk/03',  'MSdk/04',
  !     &                'MSdm1/04',
  !     &                 'MSxk/05',
  !     &              'ESSFdc1/03' /

  !C     BEC/VARIANT CODES (ONE FOR EACH LINE OF PRECEDING BLOCK)

  !      DATA KAMCOD2 /  'PPxh1',
  !     &                'PPxh2',
  !     &                'MSdk',
  !     &                'MSdm1',
  !     &                'MSxk',
  !     &              'ESSFdc1' /

  !C     CORRESPONDING HABITAT CODES FOR KAMLOOPS

  !      DATA KAMINDX / 130, 130, 310, 310,
  !     &               130,
  !     &               660, 470, 250,
  !     &               250,
  !     &               320,
  !     &               730 /

  !C     ALLOWABLE BEC VARIANTS FOR NELSON

  !      DATA NELCODE /  'PPdh1/01',
  !     &                'PPdh2/01',
  !     &                 'MSdk/01',   'MSdk/03',   'MSdk/04',  'MSdk/05',
  !     &                'MSdm1/01',  'MSdm1/02',  'MSdm1/03', 'MSdm1/04',
  !     &               'ESSFdk/01', 'ESSFdk/04',
  !     &              'ESSFdc1/03',
  !     &               'ESSFwm/01', 'ESSFwm/02', 'ESSFwm/03','ESSFwm/04',
  !     &              'ESSFwc1/02',
  !     &              'ESSFwc4/02','ESSFwc4/03' /

  !C     BEC/VARIANT CODES (ONE FOR EACH LINE OF PRECEDING BLOCK)

  !      DATA NELCOD2 /  'PPdh1',
  !     &                'PPdh2',
  !     &                'MSdk',
  !     &                'MSdm1',
  !     &              'ESSFdk',
  !     &              'ESSFdc1',
  !     &              'ESSFwm',
  !     &              'ESSFwc1',
  !     &              'ESSFwc4' /

  !C     CORRESPONDING HABITAT CODES FOR NELSON

  !      DATA NELINDX / 130,
  !     &               130,
  !     &               660, 470, 250, 420,
  !     &               620, 320, 730, 250,
  !     &               670, 670,
  !     &               730,
  !     &               620, 670, 670, 620,
  !     &               640,
  !     &               670, 670 /

  !     BEC-SPECIFIC PARAMETERS FOR NELSON WHITE PINE (PW)

  data NPW /  0.0,&
       &0.0,&
       &0.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0,&
       &0.0,&
       &0.0,      0.0,      0.0,      0.0,&
       &0.0,&
       &0.0,      0.0 /

  data LNPW / .false.,&
       &.false.,&
       &.false.,&
       &.false.,&
       &.false.,&
       &.false.,&
       &.false.,&
       &.false.,&
       &.true. /

  !     BEC-SPECIFIC PARAMETERS FOR NELSON WESTERN LARCH (LW)

  data NLW /  0.0,&
       &0.0,&
       &-0.477190,-0.477190,-0.134774,-0.134774,&
       &0.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0,&
       &0.0,&
       &0.0,      0.0,      0.0,      0.0,&
       &0.0,&
       &0.0,      0.0 /

  data LNLW / .false.,&
       &.false.,&
       &.true.,&
       &.false.,&
       &.false.,&
       &.false.,&
       &.false.,&
       &.false.,&
       &.false. /

  !     BEC-SPECIFIC PARAMETERS FOR NELSON DOUGLAS-FIR (FD)

  data NFD /  0.0,&
       &0.0,&
       &0.216504, 0.216504, 0.216504, 0.216504,&
       &0.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0,&
       &0.0,&
       &0.0,      0.0,      0.0,      0.0,&
       &0.0,&
       &0.0,      0.0 /

  data LNFD / .false.,&
       &.true.,&
       &.true.,&
       &.true.,&
       &.false.,&
       &.false.,&
       &.false.,&
       &.false.,&
       &.false. /

  !     BEC-SPECIFIC PARAMETERS FOR NELSON GRAND FIR (BG)

  data NBG /  0.0,&
       &0.0,&
       &-0.496461,-0.496461,-0.496461,-0.496461,&
       &0.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0,&
       &0.0,&
       &0.268814, 0.268814, 0.268814, 0.268814,&
       &0.0,&
       &-0.680737,-0.680737 /

  data LNBG / .false.,&
       &.false.,&
       &.true.,&
       &.false.,&
       &.true.,&
       &.false.,&
       &.true.,&
       &.false.,&
       &.true. /

  !     BEC-SPECIFIC PARAMETERS FOR NELSON WESTERN HEMLOCK (HW)

  data NHW /  0.0,&
       &0.0,&
       &0.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0,&
       &0.0,&
       &0.0,      0.0,      0.0,      0.0,&
       &0.0,&
       &0.0,      0.0 /

  data LNHW / .false.,&
       &.false.,&
       &.false.,&
       &.false.,&
       &.false.,&
       &.false.,&
       &.false.,&
       &.false.,&
       &.false. /

  !     BEC-SPECIFIC PARAMETERS FOR NELSON WESTERN REDCEDAR (CW)

  data NCW /  0.0,&
       &0.0,&
       &0.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0,&
       &0.0,&
       &0.0,      0.0,      0.0,      0.0,&
       &0.0,&
       &0.0,      0.0 /

  data LNCW / .false.,&
       &.false.,&
       &.false.,&
       &.false.,&
       &.false.,&
       &.false.,&
       &.false.,&
       &.false.,&
       &.false. /

  !     BEC-SPECIFIC PARAMETERS FOR NELSON LODGEPOLE PINE (PL)

  data NPL /  0.0,&
       &0.0,&
       &0.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0,&
       &0.0,&
       &0.0,      0.0,      0.0,      0.0,&
       &0.0,&
       &0.0,      0.0 /

  data LNPL / .false.,&
       &.false.,&
       &.true.,&
       &.true.,&
       &.true.,&
       &.false.,&
       &.false.,&
       &.false.,&
       &.true. /

  !     BEC-SPECIFIC PARAMETERS FOR NELSON ENGLEMANN SPRUCE (SE)

  data NSE /  0.0,&
       &0.0,&
       &0.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0,      0.0,      0.0,&
       &0.265099, 0.0,&
       &0.0,&
       &0.720272, 0.720272, 0.720272, 0.720272,&
       &0.0,&
       &0.0,      0.0 /

  data LNSE / .false.,&
       &.false.,&
       &.true.,&
       &.true.,&
       &.true.,&
       &.false.,&
       &.true.,&
       &.false.,&
       &.true. /

  !     BEC-SPECIFIC PARAMETERS FOR NELSON SUBALPINE FIR (BL)

  data NBL /  0.0,&
       &0.0,&
       &-0.496461,-0.496461,-0.496461,-0.496461,&
       &0.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0,&
       &0.0,&
       &0.268814, 0.268814, 0.268814, 0.268814,&
       &0.0,&
       &-0.680737,-0.680737 /

  data LNBL / .false.,&
       &.false.,&
       &.true.,&
       &.false.,&
       &.true.,&
       &.false.,&
       &.true.,&
       &.false.,&
       &.true. /

  !     BEC-SPECIFIC PARAMETERS FOR NELSON PONDEROSA PINE (PY)

  data NPY /  0.0,&
       &0.0,&
       &0.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0,&
       &0.0,&
       &0.0,      0.0,      0.0,      0.0,&
       &0.0,&
       &0.0,      0.0 /

  data LNPY / .false.,&
       &.true.,&
       &.false.,&
       &.false.,&
       &.false.,&
       &.false.,&
       &.false.,&
       &.false.,&
       &.false. /

  !     BEC-SPECIFIC PARAMETERS FOR NELSON (EP,AT,AC)

  data NEP  /  NCNT * 0.0 /
  data LNEP / NCNT2 * .false. /

  data NAT  /  NCNT * 0.0 /
  data LNAT / NCNT2 * .false. /

  data NAC  /  NCNT * 0.0 /
  data LNAC / NCNT2 * .false. /

  !     BEC-SPECIFIC PARAMETERS FOR NELSON OTHER CONIFER (OC = FD)

  data NOC /  0.0,&
       &0.0,&
       &0.216504, 0.216504, 0.216504, 0.216504,&
       &0.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0,&
       &0.0,&
       &0.0,      0.0,      0.0,      0.0,&
       &0.0,&
       &0.0,      0.0 /

  data LNOC /  .false.,&
       &.true.,&
       &.true.,&
       &.true.,&
       &.false.,&
       &.false.,&
       &.false.,&
       &.false.,&
       &.false. /

  !     BEC-SPECIFIC PARAMETERS FOR NELSON OTHER HARDWOOD(OH)

  data NOH  /  NCNT * 0.0 /
  data LNOH / NCNT2 * .false. /

  !     BEC-SPECIFIC PARAMETERS FOR KAMLOOPS WHITE PINE (PW)

  data KPW /  0.0,      0.0,      0.0,      0.0,&
       &0.0,&
       &0.0,      0.0,      0.0,&
       &0.0,&
       &0.0,&
       &0.0  /

  data LKPW / .false.,&
       &.false.,&
       &.false.,&
       &.false.,&
       &.false.,&
       &.false. /

  !     BEC-SPECIFIC PARAMETERS FOR KAMLOOPS WESTERN LARCH (LW)

  data KLW /  0.0,      0.0,      0.0,      0.0,&
       &0.0,&
       &-0.477190,-0.477190,-0.134774,&
       &0.0,&
       &0.0,&
       &0.0  /

  data LKLW / .false.,&
       &.false.,&
       &.true.,&
       &.false.,&
       &.false.,&
       &.false. /

  !     BEC-SPECIFIC PARAMETERS FOR KAMLOOPS DOUGLAS-FIR (FD)

  data KFD /  0.0,      0.0,      0.0,      0.0,&
       &0.0,&
       &0.216504,0.216504,0.216504,&
       &0.0,&
       &0.0,&
       &0.0  /

  data LKFD / .false.,&
       &.true.,&
       &.true.,&
       &.true.,&
       &.false.,&
       &.false. /

  !     BEC-SPECIFIC PARAMETERS FOR KAMLOOPS GRAND FIR (BG)

  data KBG /  0.0,      0.0,      0.0,      0.0,&
       &0.0,&
       &-0.496461,-0.496461,-0.496461,&
       &0.0,&
       &0.0,&
       &0.0  /

  data LKBG / .false.,&
       &.false.,&
       &.true.,&
       &.false.,&
       &.false.,&
       &.false. /

  !     BEC-SPECIFIC PARAMETERS FOR KAMLOOPS WESTERN HEMLOCK (HW)

  data KHW /  0.0,      0.0,      0.0,      0.0,&
       &0.0,&
       &0.0,      0.0,      0.0,&
       &0.0,&
       &0.0,&
       &0.0  /

  data LKHW / .false.,&
       &.false.,&
       &.false.,&
       &.false.,&
       &.false.,&
       &.false. /

  !     BEC-SPECIFIC PARAMETERS FOR KAMLOOPS WESTERN REDCEDAR (CW)

  data KCW /  0.0,      0.0,      0.0,      0.0,&
       &0.0,&
       &0.0,      0.0,      0.0,&
       &0.0,&
       &0.0,&
       &0.0  /

  data LKCW / .false.,&
       &.false.,&
       &.false.,&
       &.false.,&
       &.false.,&
       &.false. /

  !     BEC-SPECIFIC PARAMETERS FOR KAMLOOPS LODGEPOLE PINE (PL)

  data KPL /  0.0,      0.0,      0.0,      0.0,&
       &0.0,&
       &0.0,      0.0,      0.0,&
       &0.0,&
       &0.0,&
       &0.0  /

  data LKPL / .false.,&
       &.false.,&
       &.true.,&
       &.true.,&
       &.false.,&
       &.false. /

  !     BEC-SPECIFIC PARAMETERS FOR KAMLOOPS ENGLEMANN SPRUCE (SE)

  data KSE /  0.0,      0.0,      0.0,      0.0,&
       &0.0,&
       &0.0,      0.0,      0.0,&
       &0.0,&
       &0.0,&
       &0.0  /

  data LKSE / .false.,&
       &.false.,&
       &.true.,&
       &.true.,&
       &.false.,&
       &.false. /

  !     BEC-SPECIFIC PARAMETERS FOR KAMLOOPS SUBALPINE FIR (BL)

  data KBL /  0.0,      0.0,      0.0,      0.0,&
       &0.0,&
       &-0.496461,-0.496461,-0.496461,&
       &0.0,&
       &0.0,&
       &0.0  /

  data LKBL / .false.,&
       &.false.,&
       &.true.,&
       &.false.,&
       &.false.,&
       &.false. /

  !     BEC-SPECIFIC PARAMETERS FOR KAMLOOPS PONDEROSA PINE (PY)

  data KPY /  0.0,      0.0,      0.0,      0.0,&
       &-0.597169,&
       &0.0,      0.0,      0.0,&
       &0.0,&
       &0.0,&
       &0.0  /

  data LKPY / .false.,&
       &.true.,&
       &.false.,&
       &.false.,&
       &.false.,&
       &.false. /

  !     BEC-SPECIFIC PARAMETERS FOR KAMLOOPS (EP,AT,AC)

  data KEP  /  KCNT * 0.0 /
  data LKEP /  KCNT2 * .false. /

  data KAT  /  KCNT * 0.0 /
  data LKAT /  KCNT2 * .false. /

  data KAC  /  KCNT * 0.0 /
  data LKAC /  KCNT2 * .false. /

  !     BEC-SPECIFIC PARAMETERS FOR KAMLOOPS OTHER CONIFER (OC=FD)

  data KOC /  0.0,      0.0,      0.0,      0.0,&
       &0.0,&
       &0.216504,0.216504,0.216504,&
       &0.0,&
       &0.0,&
       &0.0  /

  data LKFD / .false.,&
       &.true.,&
       &.true.,&
       &.true.,&
       &.false.,&
       &.false. /

  !     BEC-SPECIFIC PARAMETERS FOR KAMLOOPS OTHER CONIFER (OC=FD)

  data KOH  /  KCNT * 0.0 /
  data LKOH /  KCNT2 * .false. /

  !     THIS STRUCTURE INITIALIZES THE HEIGHT GROWTH REPARAMETERIZATION
  !     THE FIRST ELEMENT IS THE SUBZONE (WITHOUT REFERENCE TO THE KAM
  !     OR NEL REGION); THE 2ND ITEM STATES WHETHER THERE IS A REPARAM-
  !     ETERIZATION WITHIN THE SUBZONE; THE PAIR OF VECTORS CONTAINS
  !     THE BREAKPOINTS (M HEIGHT )FOR THE REPARAMETERIZATION, GIVEN THAT
  !     THE 2ND ITEM IS .TRUE. DECIDUOUS NOT ORIGINALLY INCLUDED IN v2 -
  !     SPP 12,13,15 (AT,AC,OH) - USE THE SPP 11 VALUE (EP); SPP 14 (OC)
  !     NOT IN v2 USES THE SPP 3 VALUE (FD).

  data LTHGMOD /&

       &LTHG_STR ('ESSFdc1', .false., MAXSP * 0, MAXSP * 0),&

       &LTHG_STR ('ESSFdk', .true.,&
       &(/ 0,31,31, 0, 0, 0,25,28,26, 0,25,25,25,31,25/),&
       &(/ 5,35,35, 5, 5, 5,32,37,32, 5,32,32,32,35,32/)),&

       &LTHG_STR ('ESSFwc1', .false., MAXSP * 0, MAXSP * 0),&

       &LTHG_STR ('ESSFwc4', .true.,&
       &(/ 0, 0, 0, 0, 0, 0,23,37,29, 0,23,23,23, 0,23/),&
       &(/ 5, 5, 5, 5, 5, 5,30,49,36, 5,30,30,30, 5,30/)),&

       &LTHG_STR ('ESSFwm', .true.,&
       &(/31,31,32, 0,35, 0,24,35,30, 0,24,24,24,32,24/),&
       &(/35,35,36, 5,41, 5,27,47,41, 5,27,27,27,36,27/)),&

       &LTHG_STR ('MSdk', .true.,&
       &(/ 0,36,27, 0, 0, 0,25,31,31, 0,25,25,25,27,25/),&
       &(/ 5,41,33, 5, 5, 5,30,35,35, 5,30,30,30,33,30/)),&

       &LTHG_STR ('MSdm1', .true.,&
       &(/ 0,36,31, 0, 0, 0,23,31,28, 0,23,23,23,31,23/),&
       &(/ 5,45,37, 5, 5, 5,29,35,33, 5,29,29,29,37,29/)),&

       &LTHG_STR ('MSxk', .false., MAXSP * 0, MAXSP * 0),&

       &LTHG_STR ('PPdh1', .false., MAXSP * 0, MAXSP * 0),&

       &LTHG_STR ('PPdh2', .true.,&
       &(/0,24,24, 0, 0, 0,24,36, 0,25,33,33,33,24,33/),&
       &(/5,24,24, 5, 5, 5,24,46, 5,31,42,42,42,24,42/)),&

       &LTHG_STR ('PPxh1', .false., MAXSP * 0, MAXSP * 0),&

       &LTHG_STR ('PPxh2', .true.,&
       &(/ 0, 0,28, 0, 0, 0, 0, 0, 0,25,33,33,33,28,33/),&
       &(/ 5, 5,34, 5, 5, 5, 5, 5, 5,31,42,42,42,34,42/))/

  !     DEFAULT X AND Y BREAKPOINTS FOR THE LARGE TREE HEIGHT GROWTH
  !     ADJUSTMENT INTERPOLATION

  data XLTHG / 0., 0., 5.,999. /
  data YLTHG / 1., 1., 0.,  0. /

  !     THIS STRUCTURE INITIALIZES THE MORTALITY REPARAMETERIZATION
  !     THE FIRST ELEMENT IS THE SUBZONE (WITHOUT REFERENCE TO THE KAM
  !     OR NEL REGION); THE 2ND ITEM STATES WHETHER THERE IS A REPARAM-
  !     ETERIZATION WITHIN THE SUBZONE; THE PAIR OF VECTORS CONTAINS
  !     THE BREAKPOINTS (CM DIAMETER) FOR THE REPARAMETERIZATION, GIVEN
  !     THAT THE 2ND ITEM IS .TRUE.

  data MORTMOD /&

       &LTHG_STR ('ESSFdc1', .false., MAXSP * 0, MAXSP * 0),&

       &LTHG_STR ('ESSFdk', .true.,&
       &(/  0, 61, 59,  0,  0,  0, 31, 49, 39, 0, 0, 0, 0, 59, 0/),&
       &(/  5, 70,126,  5,  5,  5, 50,113, 57, 5, 5, 5, 5,126, 5/)),&

       &LTHG_STR ('ESSFwc1', .false., MAXSP * 0, MAXSP * 0),&

       &LTHG_STR ('ESSFwc4', .true.,&
       &(/  0,  0,  0,  0,  0,  0, 37, 71, 47, 0, 0, 0, 0, 0, 0/),&
       &(/  5,  5,  5,  5,  5,  5, 50,140, 86, 5, 5, 5, 5, 5, 5/)),&

       &LTHG_STR ('ESSFwm', .true.,&
       &(/ 70, 58, 70,  0, 65,  0, 31, 65, 50,  0, 0, 0, 0,70, 0/),&
       &(/ 90, 73, 90,  5,125,  5, 50,125, 85,  5, 5, 5, 5,90, 5/)),&

       &LTHG_STR ('MSdk', .true.,&
       &(/  0, 63, 60,  0,  0,  0, 31, 53, 38, 0, 0, 0, 0,60, 0/),&
       &(/  5, 94, 67,  5,  5,  5, 51, 74, 54, 5, 5, 5, 5,67, 5/)),&

       &LTHG_STR ('MSdm1', .true.,&
       &(/  0, 70, 57,  0,  0,  0, 28, 48, 42, 0, 0, 0, 0, 57, 0/),&
       &(/  5, 89,102,  5,  5,  5, 51, 66, 73, 5, 5, 5, 5,102, 5/)),&

       &LTHG_STR ('MSxk', .false., MAXSP * 0, MAXSP * 0),&

       &LTHG_STR ('PPdh1', .false., MAXSP * 0, MAXSP * 0),&

       &LTHG_STR ('PPdh2', .true.,&
       &(/  0, 51, 51,  0,  0,  0, 51, 51,  0,62, 0, 0, 0,51, 0/),&
       &(/  5, 65, 65,  5,  5,  5, 65, 65,  5,63, 5, 5, 5,65, 5/)),&

       &LTHG_STR ('PPxh1', .false., MAXSP * 0, MAXSP * 0),&

       &LTHG_STR ('PPxh2', .true.,&
       &(/  0,  0, 65,  0,  0,  0,  0,  0,  0,66, 0, 0, 0,65, 0/),&
       &(/  5,  5, 95,  5,  5,  5,  5,  5,  5,80, 5, 5, 5,95, 5/))/

  !     DEFAULT X AND Y BREAKPOINTS FOR THE LARGE MORTALITY MODEL
  !     ADJUSTMENT INTERPOLATION

  data XMORT / 0., 0., 5.,999. /
  data YMORT / 0., 0., 1.,  1. /

  !     SEE IF THE STAND'S BEC ZONE IS ICH, IDF, SBPS OR SBS; IN WHICH CASE
  !     THE STAND IS SIMULATED WITH THE VERSION 3 CALBIRATION AND THE REST
  !     OF THE ROUTINE IS SKIPPED.

  LV2ATV = .true.
  if (INDEX(BEC%Zone,'ICH') .gt.0.or.&
       &INDEX(BEC%Zone,'IDF') .gt.0.or.&
       &INDEX(BEC%Zone,'SBS') .gt.0.or.&
       &INDEX(BEC%Zone,'SBPS') .gt.0 ) then
     LV2ATV = .false.
     return
  end if

  !     IF LV2HDR IS .TRUE. BECSET HAS ALREADY BEEN CALLED, ALL PARAMETERS ASSIGNED AND OUTPUTS WRITTEN

  if (LV2ATV .and. LV2HDR) return
  LV2HDR = .true.

  IREGN = 0
  IBEC = 0
  IBECX = 0
  IBECY = 0
  LBECNEW = .false.
  COLDBEC = ''

  !     ** LARGE TREE ADJUSTMENT **
  !     IBECX IS THE INDEX TO THE EXISTENCE OF AN ADJUSTMENT PARAMETER AT THE
  !     SITE SERIES. IBEC IS THE HABITAT CODE MATCHING THE STRING CONTAINING
  !     THE SITE SERIES. NO MATCH GIVES IBECX=0, IBEC=0

  if (INDEX(BEC%Region,'KAM') .gt.0.or.&
       &INDEX(BEC%Region,'CAR') .gt.0) then
     IREGN = 1
     select case (BEC%PrettyName)
     case ('PPxh1/01')
        IBEC = 130
        IBECX = 1
        IBECY = 1
     case ('PPxh1/03')
        IBEC = 130
        IBECX = 2
        IBECY = 1
     case ('PPxh1/06')
        IBEC = 310
        IBECX = 3
        IBECY = 1
     case ('PPxh1/07')
        IBEC = 310
        IBECX = 4
        IBECY = 1

     case ('PPxh2/03')
        IBEC = 130
        IBECX = 5
        IBECY = 2

     case ('MSdk/01')
        IBEC = 660
        IBECX = 6
        IBECY = 3
     case ('MSdk/03')
        IBEC = 470
        IBECX = 7
        IBECY = 3
     case ('MSdk/04')
        IBEC = 250
        IBECX = 8
        IBECY = 3

     case ('MSdm1/04')
        IBEC = 250
        IBECX = 9
        IBECY = 4

     case ('MSxk/05')
        IBEC = 320
        IBECX = 10
        IBECY = 5

     case ('ESSFdc1/03')
        IBEC = 730
        IBECX = 11
        IBECY = 6

     case default ! MSdk/01 is default
        IBEC = 660
        IBECX = 6
        IBECY = 3
        COLDBEC = BEC%PrettyName
        BEC%PrettyName = 'MSdk/01'
        BEC%FullName = 'MSdk'
        LBECNEW = .true.

     end select

  else if (INDEX(BEC%Region,'NEL') .gt.0) then
     IREGN = 2
     select case (BEC%PrettyName)
     case ('PPdh1/01')
        IBEC = 130
        IBECX = 1
        IBECY = 1

     case ('PPdh2/01')
        IBEC = 130
        IBECX = 2
        IBECY = 2

     case ('MSdk/01')
        IBEC = 660
        IBECX = 3
        IBECY = 3
     case ('MSdk/03')
        IBEC = 470
        IBECX = 4
        IBECY = 3
     case ('MSdk/04')
        IBEC = 250
        IBECX = 5
        IBECY = 3
     case ('MSdk/05')
        IBEC = 420
        IBECX = 6
        IBECY = 3

     case ('MSdm1/01')
        IBEC = 620
        IBECX = 7
        IBECY = 4
     case ('MSdm1/02')
        IBEC = 320
        IBECX = 8
        IBECY = 4
     case ('MSdm1/03')
        IBEC = 730
        IBECX = 9
        IBECY = 4
     case ('MSdm1/04')
        IBEC = 250
        IBECX = 10
        IBECY = 4

     case ('ESSFdk/01')
        IBEC = 670
        IBECX = 11
        IBECY = 5
     case ('ESSFdk/04')
        IBEC = 670
        IBECX = 12
        IBECY = 5

     case ('ESSFdc1/03')
        IBEC = 730
        IBECX = 13
        IBECY = 6

     case ('ESSFwm/01')
        IBEC = 620
        IBECX = 14
        IBECY = 7
     case ('ESSFwm/02')
        IBEC = 670
        IBECX = 15
        IBECY = 7
     case ('ESSFwm/03')
        IBEC = 670
        IBECX = 16
        IBECY = 7
     case ('ESSFwm/04')
        IBEC = 620
        IBECX = 17
        IBECY = 7

     case ('ESSFwc1/02')
        IBEC = 640
        IBECX = 18
        IBECY = 8

     case ('ESSFwc4/02')
        IBEC = 670
        IBECX = 19
        IBECY = 9
     case ('ESSFwc4/03')
        IBEC = 670
        IBECX = 20
        IBECY = 9

     case default ! MSdk/01 is default; change zones if no match
        IBEC = 660
        IBECX = 3
        IBECY = 3
        COLDBEC = BEC%PrettyName
        BEC%PrettyName = 'MSdk/01'
        BEC%FullName = 'MSdk'
        LBECNEW = .true.
     end select
  end if

  !     ** LARGE TREE ADJUSTMENT **
  !     IBECY IS THE INDEX TO THE ADJUSTMENT PARAMETER AT THE
  !     SUBZONE. NO MATCH GIVES IBECY=0

  !22 IBECY = 0
  !   IF (IREGN .EQ. 1) THEN
  !     DO I = 1, KCNT2
  !       IF (INDEX(BEC%FullName,KAMCOD2(I)) .GT. 0) THEN
  !         IBECY = I
  !         GOTO 23
  !       ENDIF
  !     ENDDO
  !   ELSEIF (IREGN .EQ. 2) THEN
  !     DO I = 1, NCNT2
  !       IF (INDEX(BEC%FullName,NELCOD2(I)) .GT. 0) THEN
  !         IBECY = I
  !         GOTO 23
  !       ENDIF
  !     ENDDO
  !   ENDIF

  !     HEIGHT GROWTH, MORTALITY MODEL ADJUSTMENT
  !     FIND INDEX TO ALL-REGIONS SUBZONE. NO MATCH OR NO
  !     PARAMETERIZATION GIVES JBECY=0. NOTE THAT LTHGMOD IS USED
  !     IN THE TEST, BUT THAT IT APPLIES TO MORTMOD AS WELL, SINCE
  !     THEY ARE IDENTICAL W.R.T. TO BECszn AND OK ELEMENTS.

  JBECY = 0
  do I = 1,ACNT2
     if (INDEX(BEC%FullName,LTHGMOD(I)%BECszn) .gt.0.and.&
          &LTHGMOD(I)%OK) then
        JBECY = I
        goto 24
     end if
  end do

  !     WRITE HEADER INFO

  24 continue
  write (JOSTND, "(/1X,T12,80('='))")
  WRITE (JOSTND, "(                                                 &
       &  /T12,'BC VERSION 2 CALIBRATION OF LARGE-TREE ',                 &
       &   'DIAMETER-GROWTH, LARGE TREE HEIGHT-DIAMETER,',                &
       &  /T12,'SMALL TREE HEIGHT-GROWTH, MAXIMUM BASAL AREA, ',          &
       &   'HEIGHT GROWTH MODIFICATION',                                  &
       &  /T12,'AND MORTALITY MODIFICATION MODELS')")

  !     IF IBEC IS ZERO, THE USER HAS PROVIDED A BEC/SS WHICH CURRENTLY
  !     HAS NO MATCH AND THE DEFAULT HABITAT CODE IS APPLIED. OTHERWISE,
  !     COMPARE WITH EXISTING HABITAT CODE.

  KODTYP = IBEC
  if (LBECNEW) then
     call ERRGRO(.true.,3)
     write (JOSTND, 28) trim(COLDBEC), BEC%Region,&
          &trim(BEC%PrettyName), KODTYP
     28 format (/T12,'THE BEC VARIANT ', A,' HAS NO EXACT LARGE-',&
          &'TREE RECALIBRATION MAPPING IN',/T12, 'THE ', A,&
          &' REGION; SWITCHING TO BEC VARIANT ', A, ' USING DEFAULT ',&
          &'HABITAT TYPE= ', I3)
  else
     write (JOSTND, 30) trim(BEC%PrettyName), KODTYP, BEC%Region
     30 format (/T13,'THE BEC VARIANT ', A, ' MAPS TO HABITAT ',&
          &'TYPE ', I3, ' IN THE ', A, ' REGION.')

     !     need to assign ITYPE here too, this is based on code in /ie/habtyp.f and /ie/blkdat.f

     select case (KODTYP)
     case(130)
        ITYPE = 1
     case(250)
        ITYPE = 4
     case(310)
        ITYPE = 7
     case(320)
        ITYPE = 8
     case(420)
        ITYPE = 10
     case(470)
        ITYPE = 11
     case(620)
        ITYPE = 19
     case(640)
        ITYPE = 20
     case(660)
        ITYPE = 21
     case(670)
        ITYPE = 22
     case(730)
        ITYPE = 27
     case default
        ITYPE = 21
     end select

     !KOTTYP = 130, MTYPE=, JTYP=4,  Itype=  1
     !KOTTYP = 250, MTYPE=, JTYP=15, Itype=  4
     !KOTTYP = 310, MTYPE=, JTYP=18, itype=  7
     !KOTTYP = 320, MTYPE=, JTYP=19, Itype=  8
     !KOTTYP = 420, MTYPE=, JTYP=28, Itype= 10
     !KOTTYP = 470, MTYPE=, JTYP=33, Itype= 11
     !KOTTYP = 620, MTYPE=, JTYP=58, Itype= 19
     !KOTTYP = 640, MTYPE=, JTYP=61, Itype= 20
     !KOTTYP = 660, MTYPE=, JTYP=63, Itype= 21
     !KOTTYP = 670, MTYPE=, JTYP=64, Itype= 22
     !KOTTYP = 730, MTYPE=, JTYP=73, Itype= 27

     !     DATA KTYPE/
     !    &  5*1, 4*2, 4, 3*1, 3,4,5,6,7,8,9,8,8,9,7,3, 3*10, 4,11,20,29,11,      !33
     !    &    11,13,14,17, 4*12, 3*13, 14,15,14,16,14,16, 3*17, 24,12,24,18,     !57
     !    &    19,21,19,20,20,21,22,19,23,19,24,27,25,25,26,27,22,24,27,24,
     !    &    27,28,28,29,28,28, 4*29, 27,9,20,24,21,27,24,30/
     !     DATA MTYPE/
     !    &            130,170,250,260,280,290,310,320,330,420,
     !    &            470,510,520,530,540,550,570,610,620,640,
     !    &            660,670,680,690,710,720,730,830,850,999/
     !     DATA JTYPE / 10,100,110,130,140,160,170,180,190,200,
     !    &            210,220,230,250,260,280,290,310,320,330,
     !    &            340,350,360,370,380,400,410,420,430,440,
     !    &            450,460,470,480,500,501,502,505,506,510,
     !    &            515,516,520,529,530,540,545,550,555,560,
     !    &            565,570,575,579,590,600,610,620,630,635,
     !    &            640,650,660,670,675,680,685,690,700,701,
     !    &            710,720,730,740,750,770,780,790,800,810,
     !    &            820,830,840,850,860,870,890,900,910,920,
     !    &            925,930,940,950,999,27*0 /

     !          DO 100 K=1,95
     !          IF (KODTYP .LT. JTYPE(K)) GO TO 110
     ! 100      CONTINUE
     !          K=96
     ! 110      CONTINUE
     !          ITYPE=KTYPE(K-1)
     !      ENDIF

  end if

  !     LARGE TREE VERSION 2 INTERIM CALIBRATION:

  !     READ FLAG INDICATING IF BEC VARIANT IS VALID FOR SPECIES
  !     IN THE SUBZONE (BUT PERHAPS NOT IN THE SITE SERIES)
  !     READ SITE SERIES TERMS AND ADD TO INTERCEPT

  if (INDEX(BEC%Region,'KAM') .gt.0.or.&
       &INDEX(BEC%Region,'CAR') .gt.0) then
     do I = 1,MAXSP
        LLTDGOK(I) = LKOK(IBECY,I)
        if (IBEC.gt.0) BECTERM(I) = KCOEF(IBECX,I)
        SEICON(I) = INTRCP(I) + BECTERM(I)
     end do
  else if (INDEX(BEC%Region,'NEL') .gt.0) then
     do I = 1,MAXSP
        LLTDGOK(I) = LNOK(IBECY,I)
        if (IBEC.gt.0) BECTERM(I) = NCOEF(IBECX,I)
        SEICON(I) = INTRCP(I) + BECTERM(I)
     end do
  end if

  !     SEE IF THERE ARE ANY SPECIES FOR WHICH A PARAMETERIZATION IS
  !     NOT AVAILABLE.

  do I = 1,44
     SPLST(I:I) = ' '
  end do
  INOSPP = 0
  IP = 1
  do I = 1,MAXSP
     if (.not. LLTDGOK(I)) then
        if (INOSPP .gt.0) then
           SPLST(IP:IP) = ',' ! INSERT COMMAS IF MORE THAN 1 SPP
           IP = IP+1
        end if
        INOSPP = INOSPP + 1
        do J = 1,3
           if (JSP(I)(J:J) .ne. ' ') then
              SPLST(IP:IP) = JSP(I)(J:J)
              IP = IP+1
           end if
        end do
     end if
  end do

  if (INOSPP .gt.0) then
     WRITE(JOSTND, "(/T12,'LARGE-TREE DIAMETER-GROWTH ',             &
          &      'RE-PARAMETERIZATION IS NOT AVAILABLE FOR'/T12,             &
          &      'THE FOLLOWING SPECIES:',2(/1X),                            &
          &      T12,A)") SPLST(1:(IP-1))
  end if

  !      IF THERE ARE SOME SPP W/ REPARAMETERIZATION COEFFICIENTS

  if (INOSPP .lt. MAXSP) then
     write (JOSTND, 31)
     31 format (/1X,T12,'THE FOLLOWING LARGE-TREE FACTOR '&
          &'COEFFICIENT ADJUSTMENTS WILL BE APPLIED:',&
          &2(/1X),T13,'SPP',T17,'INTERCEPT',T27,'   BEC/SS',&
          &/1X,T13,3("-"),T17,9("-"),T27,9("-"))

     do I = 1,MAXSP
        if (LLTDGOK(I))&
             &write (JOSTND, 32) JSP(I), INTRCP(I), BECTERM(I)
        32 format(T13,A3,T17,F9.6,T27,F9.6)
     end do

     write (JOSTND, 34)
     34 format (/1X,T12,'THE FOLLOWING LARGE-TREE CONTINUOUS '&
          &'COEFFICIENT ADJUSTMENTS WILL BE APPLIED:',&
          &2(/1X),T13,'SPP',&
          &T17,'    SLOPE',T27,'  SIN ASP',T37,'  COS ASP',&
          &T47,'     ELEV',T57,'  ELEV**2',T67,'      CCF'/1X,&
          &T13,3("-"), T17,9("-"),T27,9("-"),T37,9("-"),&
          &T47,9("-"),T57,9("-"),T67,9("-"))

     do I = 1,MAXSP
        if (LLTDGOK(I))&
             &write (JOSTND, 35) JSP(I), SEISLP(I),SEISAS(I),SEICAS(I),&
             &SEIELV(I), SEIEL2(I), SEICCF(I)
        35 format(T13,A3,T17,F9.6,T27,F9.6,T37,F9.6,&
             &T47,F9.6,T57,F9.6,T67,F9.6)
     end do
  end if

  !     VERSION 2 - REPARAMETERIZATION OF HEIGHT DUBBING MODEL

  !     FIND AND REPLACE HEIGHT COEFFICIENTS HELD IN PLOT.F77
  !     ARRAYS HT1() AND HT2() (C_0 AND C_1 OF HT/DIAM EQN)
  !     BECAUSE THE ORIGINAL ARRAYS NEED TO KEPT UNALTERED FOR MULTI-
  !     STAND RUNS, AN ARCHIVE (XHT1B,XHT2B) OF THE COMPILE TIME
  !     VARIABLES IS MAINTAINED.
  !     COPY PERMANENT ARRAYS TO ARCHIVE

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

     if (I.eq.2) then ! western larch - LW
        if (INDEX(BEC%Zone,'ESSF') .gt.0.or.&
             &INDEX(BEC%Zone,'MS') .gt.0) then
           if (INDEX(BEC%SubZone,'dk') .gt.0) then
              XHT1(I) = 3.9536
              XHT2(I) = -21.4361
           end if
        end if

     else if (I.eq.3.or. I.eq.14) then ! Douglas-fir - FD; OC
        if (INDEX(BEC%Zone,'ESSF') .gt.0) then
           if (INDEX(BEC%SubZone,'dk') .gt.0.or.&
                &INDEX(BEC%SubZone,'mk') .gt.0) then
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
        else if (INDEX(BEC%Zone,'PP') .gt.0) then
           if (INDEX(BEC%SubZone,'dh') .gt.0.or.&
                &INDEX(BEC%SubZone,'xh') .gt.0) then
              XHT1(I) = 3.5967
              XHT2(I) = -22.0246
           end if
        else if (INDEX(BEC%Zone,'MS') .gt.0) then
           if (INDEX(BEC%SubZone,'dk') .gt.0) then
              XHT1(I) = 3.8840
              XHT2(I) = -25.0499
           end if
        end if

     else if (I.eq.5) then ! western hemlock - HW
        if (INDEX(BEC%Zone,'ESSF') .gt.0) then
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
        if (INDEX(BEC%Zone,'ESSF') .gt.0.or.&
             &INDEX(BEC%Zone,'MS') .gt.0) then
           if (INDEX(BEC%SubZone,'wc') .gt.0) then
              XHT1(I) = 4.0095
              XHT2(I) = -32.5478
           else if (INDEX(BEC%SubZone,'dk') .gt.0.or.&
                &INDEX(BEC%SubZone,'wk') .gt.0) then
              XHT1(I) = 3.8792
              XHT2(I) = -28.1120
           end if
        end if

     else if (I.eq.7) then ! lodgepole pine - PL
        if (INDEX(BEC%Zone,'ESSF') .gt.0.or.&
             &INDEX(BEC%Zone,'MS') .gt.0) then
           if (INDEX(BEC%SubZone,'dk') .gt.0.or.&
                &INDEX(BEC%SubZone,'dc') .gt.0.or.&
                &INDEX(BEC%SubZone,'dm') .gt.0) then
              XHT1(I) = 3.6574
              XHT2(I) = -14.5566
           else if (INDEX(BEC%SubZone,'wk') .gt.0) then
              XHT1(I) = 3.7443
              XHT2(I) = -18.5685
           else if (INDEX(BEC%SubZone,'xv') .gt.0.or.&
                &INDEX(BEC%SubZone,'mv') .gt.0) then
              XHT1(I) = 3.2342
              XHT2(I) = -11.6208
           end if
        end if

     else if (I.eq.8) then ! Engelmann spruce - SE
        if (INDEX(BEC%Zone,'ESSF') .gt.0) then
           if (INDEX(BEC%SubZone,'dk') .gt.0.or.&
                &INDEX(BEC%SubZone,'mw') .gt.0.or.&
                &INDEX(BEC%SubZone,'wc') .gt.0.or.&
                &INDEX(BEC%SubZone,'mv') .gt.0) then
              XHT1(I) = 3.9041
              XHT2(I) = -22.0422
           else if (INDEX(BEC%SubZone,'dc') .gt.0) then
              XHT1(I) = 4.0093
              XHT2(I) = -26.5437
           else if (INDEX(BEC%SubZone,'wk') .gt.0.or.&
                &INDEX(BEC%SubZone,'mc') .gt.0) then
              XHT1(I) = 3.6971
              XHT2(I) = -22.9202
           end if
        else if (INDEX(BEC%Zone,'MS') .gt.0) then
           if (INDEX(BEC%SubZone,'dk') .gt.0) then
              XHT1(I) = 4.0555
              XHT2(I) = -25.5775
           else if (INDEX(BEC%SubZone,'dm') .gt.0) then
              XHT1(I) = 4.1206
              XHT2(I) = -29.9800
           end if
        end if

     else if (I.eq.9.or. I.eq.4) then ! subalpine fir - BL (& BG)
        if (INDEX(BEC%Zone,'ESSF') .gt.0.or.&
             &INDEX(BEC%Zone,'MS') .gt.0) then
           if (INDEX(BEC%SubZone,'dk') .gt.0.or.&
                &INDEX(BEC%SubZone,'wc') .gt.0.or.&
                &INDEX(BEC%SubZone,'mc') .gt.0.or.&
                &INDEX(BEC%SubZone,'mk') .gt.0.or.&
                &INDEX(BEC%SubZone,'mv') .gt.0) then
              XHT1(I) = 3.7405
              XHT2(I) = -21.4266
           else if (INDEX(BEC%SubZone,'mw') .gt.0.or.&
                &INDEX(BEC%SubZone,'dc') .gt.0) then
              XHT1(I) = 4.1376
              XHT2(I) = -32.5929
           else if (INDEX(BEC%SubZone,'wk') .gt.0) then
              XHT1(I) = 3.6544
              XHT2(I) = -22.5426
           end if
        end if

     else if (I.eq.10) then ! ponderosa pine - PY
        if (INDEX(BEC%Zone,'PP') .gt.0) then
           if (INDEX(BEC%SubZone,'dh') .gt.0) then
              XHT1(I) = 3.8878
              XHT2(I) = -34.7046
           else if (INDEX(BEC%SubZone,'xh') .gt.0) then
              XHT1(I) = 3.6298
              XHT2(I) = -31.3677
           end if
        end if
     end if
  end do

  !     MARK COEFFICIENTS THAT ARE DIFFERENT FROM THE
  !     NI VARIANT DEFAULTS AND

  do I = 1,MAXSP
     if ((XHT1(I) .ne. XHT1B(I)) .or. (XHT2(I) .ne. XHT2B(I))) then
        CH(I) = '*'
     else
        CH(I) = ' '
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

  !     PRINT HEIGHT COEFFICIENTS IF DESIRED (NON-BLANK 7TH FIELD OF
  !     BECINFO KEYWORD)

  write (JOSTND,41)
  41 format (/T12,'THE FOLLOWING DIAMETER-HEIGHT MODEL '&
       &'COEFFICIENTS WILL BE USED FOR DUBBING',/T12,&
       &'MISSING HEIGHTS. MARKED COEFFICIENTS FIT A '&
       &'CM/M MODEL; OTHERS AN IN/FT MODEL:',2(/1X),T13,'SPP',&
       &T17,'       C0',T27,'       C1',T37,'NEW'/1X,&
       &T13,3("-"),T17,9("-"),T27,9("-"),T37,3("-"))

  do I = 1,MAXSP
     write (JOSTND, 42) JSP(I), HT1(I), HT2(I), CH(I)
     42 format(T13,A3,T17,F9.4,T27,F9.4,T39,A1)
  end do

  !     REPARAMETERIZATION OF SMALL TREE HEIGHT GROWTH MODEL

  !     IN A MERGED V2/V3 MODEL, ALL THE SMALL TREE HEIGHT GROWTH
  !     MODIFICATIONS WERE FOR SPECIES IN THE IDF. THESE SPECIES
  !     ARE ALL HANDLED UNDER V3

  !     VERSION 2 - REPARAMETERIZATION OF HEIGHT GROWTH MODEL

  if (JBECY .eq.0) then
     WRITE(JOSTND, "(/T12,'LARGE-TREE HEIGHT-GROWTH ',               &
          &    'ADJUSTMENT IS NOT CURRENTLY AVAILABLE FOR'/T12,              &
          &    'THIS BEC/SUBZONE.')")
  else

     !     SEE IF THERE ARE ANY SPECIES FOR WHICH A PARAMETERIZATION IS
     !     NOT AVAILABLE. THESE ALWAYS HAVE LO=0 AND HI=5.

     do I = 1,44
        SPLST(I:I) = ' '
     end do
     INOSPP = 0
     IP = 1
     do I = 1,MAXSP
        LOK2 = ((LTHGMOD(JBECY)%LO(I) .eq.0) .and.&
             &(LTHGMOD(JBECY)%HI(I) .eq.5))
        if (LOK2) then
           if (INOSPP .gt.0) then
              SPLST(IP:IP) = ',' ! INSERT COMMAS IF MORE THAN 1 SPP
              IP = IP+1
           end if
           INOSPP = INOSPP + 1
           do J = 1,3
              if (JSP(I)(J:J) .ne. ' ') then
                 SPLST(IP:IP) = JSP(I)(J:J)
                 IP = IP+1
              end if
           end do
        end if
     end do

     if (INOSPP .gt.0)&
          &  WRITE(JOSTND, "(/T12,'LARGE TREE HEIGHT GROWTH ',               &
          &    'ADJUSTMENT IS NOT AVAILABLE FOR'/T12,                        &
          &    'THE FOLLOWING SPECIES:',2(/1X),T13,A)")&
          &SPLST(1:(IP-1))

     if (INOSPP .lt. MAXSP) then
        write (JOSTND, 61)
        61 format (/1X,T12,'THE FOLLOWING BREAKPOINTS '&
             &'WILL BE USED FOR ADJUSTING HEIGHT GROWTH',&
             &2(/1X),T13,'SPP',T17,'  LOW (M)',T27,' HIGH (M)',&
             &/1X,T13,3("-"),T17,9("-"),T27,9("-"))

        do I = 1,MAXSP
           LOK2 = ((LTHGMOD(JBECY)%LO(I) .eq.0) .and.&
                &(LTHGMOD(JBECY)%HI(I) .eq.5))
           if (.not.LOK2) then
              write (JOSTND, 62) JSP(I),&
                   &LTHGMOD(JBECY)%LO(I),LTHGMOD(JBECY)%HI(I)
              62 format(T13,A3,T17,I9,T27,I9)
           end if
        end do
     end if
  end if

  !     VERSION 2 - REPARAMETERIZATION OF MORTALITY MODEL

  if (JBECY .eq.0) then
     WRITE(JOSTND, "(/T12,'LARGE-TREE HEIGHT-GROWTH ',               &
          &    'ADJUSTMENT IS NOT CURRENTLY AVAILABLE FOR'/T12,              &
          &    'THIS BEC/SUBZONE.')")
  else

     !       SEE IF THERE ARE ANY SPECIES FOR WHICH A PARAMETERIZATION IS
     !       NOT AVAILABLE. THESE ALWAYS HAVE LO=0 AND HI=5.

     do I = 1,44
        SPLST(I:I) = ' '
     end do
     INOSPP = 0
     IP = 1
     do I = 1,MAXSP
        LOK2 = ((MORTMOD(JBECY)%LO(I) .eq.0) .and.&
             &(MORTMOD(JBECY)%HI(I) .eq.5))
        if (LOK2) then
           if (INOSPP .gt.0) then
              SPLST(IP:IP) = ',' ! INSERT COMMAS IF MORE THAN 1 SPP
              IP = IP+1
           end if
           INOSPP = INOSPP + 1
           do J = 1,3
              if (JSP(I)(J:J) .ne. ' ') then
                 SPLST(IP:IP) = JSP(I)(J:J)
                 IP = IP+1
              end if
           end do
        end if
     end do

     if (INOSPP .gt.0)&
          &  WRITE(JOSTND, "(/T12,'MORTALITY ',                              &
          &    'ADJUSTMENT IS NOT AVAILABLE FOR'/T12,                        &
          &    'THE FOLLOWING SPECIES:',2(/1X),T13,A)")&
          &SPLST(1:(IP-1))

     if (INOSPP .lt. MAXSP) then
        write (JOSTND, 71)
        71 format (/T12,'THE FOLLOWING BREAKPOINTS '&
             &'WILL BE USED FOR ADJUSTING MORTALITY',&
             &2(/1X),T13,'SPP',T17,' LOW (CM)',T27,'HIGH (CM)',&
             &/T13,3("-"),T17,9("-"),T27,9("-"))

        do I = 1,MAXSP
           LOK2 = ((MORTMOD(JBECY)%LO(I) .eq.0) .and.&
                &(MORTMOD(JBECY)%HI(I) .eq.5))
           if (.not.LOK2) then
              write (JOSTND, 72) JSP(I),&
                   &MORTMOD(JBECY)%Lo(I),MORTMOD(JBECY)%Hi(I)
              72 format(T13,A3,T17,I9,T27,I9)
           end if
        end do
     end if
  end if

  write (JOSTND, "(/T12,80('='))")

  return

  !     COPY BEC TERMS REQUIRED TO ADJUST LARGE TREE DIAMETER
  !     GROWTH MODEL (CALLED BY SUBROUTINE **DGF**). THE XBEC0 TERMS
  !     ARE TRANSFORMED TO THE INTERNAL UNITS. IF THE SS IS NOT
  !     SUPPORTED, SET LBEC8 TO .FALSE.

  entry SEILTDG (XBEC0,XBEC1,XBEC2,XBEC3,XBEC4,XBEC5,XBEC6,&
       &XBEC7,LBEC8)

  do I = 1,MAXSP
     LBEC8(I) = .false.
  end do

  do I = 1,3
     XBEC0(I) = BECADJ(I)
  end do

  XBEC0(1) = XBEC0(1) / 100.
  XBEC0(2) = XBEC0(2) * 3.14159 / 180.
  XBEC0(3) = XBEC0(3) * MtoFT / 100.

  do I = 1,MAXSP
     XBEC1(I) = SEICON(I)
     XBEC2(I) = SEISLP(I)
     XBEC3(I) = SEISAS(I)
     XBEC4(I) = SEICAS(I)
     XBEC5(I) = SEIELV(I)
     XBEC6(I) = SEIEL2(I)
     XBEC7(I) = SEICCF(I)
     LBEC8(I) = LLTDGOK(I)
  end do

  return

  !     COPY TERMS REQUIRED TO ADJUST SMALL TREE HEIGHT GROWTH MODEL
  !     (**REGENT**). V2 SMALL TREE ADJUSTMENT HAS BEEN SUPERCEDED BY
  !     V3, WHICH MODELS ALL SPECIES IN THE IDF (THE ZONE THAT THE
  !     V2 ADJUSTMENT WAS APPLIED TO).

  entry SEISTHG (ISP1,X01,X11,X21,X31,X41,X51,X61,LOK1)
  LOK1 = .false.
  return

  !     COPY TERMS REQUIRED TO ADJUST LARGE TREE HEIGHT
  !     GROWTH MODEL (CALLED BY SUBROUTINE **UPDATE**). IF THE
  !     SS IS NOT SUPPORTED FOR A SUBZONE, RETURN 1.0 AS THE
  !     MODIFIER; IF THE SPECIES IS NOT SUPPORTED, USE 0,5 M
  !     BREAKPOINTS TO SUPRESS GROWTH. THE BREAKPOINTS FOR
  !     THE YLTHG(4) VECTOR AND THE XLTHG(1,4) ARE CONSTANT
  !     AND ARE DECLARED PREVIOUSLY. THE RETURNED VALUE, X0
  !     IS MULTIPLIED BY THE PREDICTED HEIGHT GROWTH TO GIVE
  !     AN ADJUSTED HEIGHT GROWTH.

  entry SEILTHG (ISP2,XHT22,X02)

  if (JBECY .eq.0.or. ISP2.lt.1.or. ISP2.gt. MAXSP) then
     X02 = 1.
     return
  end if

  XLTHG(2) = LTHGMOD(JBECY)%Lo(ISP2)
  XLTHG(3) = LTHGMOD(JBECY)%Hi(ISP2)

  X02 = ALGSLP((XHT22*FTtoM),XLTHG,YLTHG,4)

  return

  !     COPY TERMS REQUIRED TO ADJUST MORTALITY MODEL
  !     CALLED BY SUBROUTINE *DGDRIV*). IF THE
  !     SS IS NOT SUPPORTED FOR A SUBZONE, RETURN 1.0 AS THE
  !     MODIFIER; IF THE SPECIES IS NOT SUPPORTED, USE 0,5 M
  !     BREAKPOINTS TO SUPRESS GROWTH. THE BREAKPOINTS FOR
  !     THE YMORT(4) VECTOR AND THE XMORT(1,4) ARE CONSTANT
  !     AND ARE DECLARED PREVIOUSLY. THE RETURNED VALUE, X0,
  !     IS COMPARED TO THE MORTALITY PREDICTION, AND THE
  !     GREATER OF THE TWO IS USED.

  entry SEIMORT (ISP3,XDIA3,X03)

  X03 = 1.0
  if (JBECY .eq.0.or. ISP3.lt.1.or. ISP3.gt. MAXSP) return

  XMORT(2) = MORTMOD(JBECY)%Lo(ISP3)
  XMORT(3) = MORTMOD(JBECY)%Hi(ISP3)

  X03 = ALGSLP((XDIA3*INtoCM),XMORT,YMORT,4)

  !     APPLY MORTALITY-EFFECTS THROUGH MULTIPLICATIVE
  !     CHANGE ON DIAMETER GROWTH: THEREFORE A ZERO
  !     MORTALITY BECOMES A 1 FOR DG MULTIPLIER; A 1 FOR
  !     MORTALITY BECOMES A 0 FOR DG MULTIPLIER.
  !     THE VALUE IS KEPT FROM BEING ZERO, SINCE LOG(DG) IS
  !     USED LATER

  X03 = MAX(0.0001, (1.0 - X03))

  return
end
