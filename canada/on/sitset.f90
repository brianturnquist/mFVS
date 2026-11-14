!=====================================!
!   Initialize species site indices   !
!=====================================!
! CANADA-ON $Id$
subroutine SITSET
  implicit none

  ! THIS SUBROUTINE LOADS THE SITELG ARRAY WITH A SITE INDEX FOR EACH
  ! SPECIES WHICH WAS NOT ASSIGNED A SITE INDEX BY KEYWORD.

  ! CALLED BY INITRE

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ESCOMN.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'VOLSTD.f90'
  include 'METRIC.f90'

  ! Variables
  logical :: DEBUG
  real    :: SICOEF1(MAXSP,MAXSP),SICOEF2(MAXSP,MAXSP),BAMAX1(MAXSP)
  integer :: METHB8,METHC8
  real    :: A1(17),A2(17),A3(17),A4(17),A5(17)
  real    :: THT1(17),THT2(17),THT3(17),THT4(17)
  integer :: OSP(MAXSP)

  double precision :: KD
  integer :: J,I,JJ,K,KSP,IEQAG
  character(len=2) :: FORST
  character(len=2) :: DIST
  character(len=2) :: PROD
  character(len=2) :: VAR
  character(len=11) :: VOLEQ
  integer :: IFIASP,ERRFLAG,ISPC,IREGN,KFORST
  real    :: AGEBH,HTM,SIM,SLP,DIFF,SIMIN,AGEDIF
  logical :: LONT

  data BAMAX1/&
       &150.,   150.,   240.,   240.,   240.,   190.,   240.,&
       &240.,   180.,   200.,   200.,   240.,   150.,   150.,&
       &150.,   150.,   130.,   150.,   150.,   200.,   150.,&
       &150.,   150.,   150.,   150.,   150.,   150.,   150.,&
       &150.,   160.,   160.,   160.,   160.,   160.,   160.,&
       &130.,   160.,   160.,   160.,   130.,   130.,   150.,&
       &150.,   150.,   150.,   160.,   150.,   150.,   150.,&
       &150.,   150.,   150.,   150.,   150.,   150.,   150.,&
       &170.,   150.,   140.,   150.,   150.,   150.,   150.,&
       &150.,   150.,   150.,   150.,   150.,&
       &150.,   240.,   190.,   180. /

  data ((SICOEF1(I,J),I = 1,MAXSP),J = 1,MAXSP)/&
       &2*.000,2*-.083,35*.000,3*-4.661,-11.774,29*.000,&
       &2*.000,2*-.083,35*.000,3*-4.661,-11.774,29*.000,&
       &2*.081,2*.0,3.926,2*14.453,11*.0,2*13.601,19*.0,3*-20.194,26*.0,&
       &0.081,3.926,14.453,.0,&
       &2*.081,2*.0,3.926,2*14.453,11*.0,2*13.601,19*.0,3*-20.194,26*.0,&
       &0.081,3.926,14.453,.0,&
       &2*.0,2*-4.094,.0,2*10.9,11*.0,2*5.264,13*.0,3*4.678,3*.000,&
       &3*-9.004,28*.00,10.9,.0,&
       &2*.0,2*-18.249,-13.974,5*.0,-19.486,28*.0,3*-7.893,27*.0,&
       &-13.974,2*.0,&
       &2*.0,2*-18.249,-13.974,5*.0,-19.486,28*.0,3*-7.893,27*.0,&
       &-13.974,2*.0,&
       &8*.0,-7.212,.0,-15.196,28*.0,3*-1.366,12.096,28*.0,-7.212,&
       &7*.000,6.175,.000,2.846,62*.000,&
       &8*.000,-3.700,.000,-12.708,60*.000,-3.700,&
       &5*.000,2*10.904,8.205,.000,6.434,60*.000,10.904,.000,&
       &72*.000,&
       &72*.000,&
       &72*.000,&
       &18*.0,2*8.379,3*-.953,-15.228,.253,3*7.441,4.812,4*.0,&
       &3*-37.214,3*.000,3*-4.346,30*.000,&
       &18*.0,2*8.379,3*-.953,-15.228,.253,3*7.441,4.812,4*.0,&
       &3*-37.214,3*.000,3*-4.346,30*.000,&
       &72*.000,&
       &72*.000,&
       &2*.0,2*-17.873,-5.606,9*.0,2*-9.376,4*.0,3*6.729,-2.119,&
       &-2.628,3*3.4,11.937,4*.0,3*-11.597,3*.0,3*-18.897,-16.538,&
       &26*.0,-5.606,2*.0,&
       &2*.0,2*-17.873,-5.606,9*.0,2*-9.376,4*.0,3*6.729,-2.119,&
       &-2.628,3*3.4,11.937,4*.0,3*-11.597,3*.0,3*-18.897,-16.538,&
       &26*.0,-5.606,2*.0,&
       &14*.0,2*.946,2*.0,2*-7.973,3*.0,-6.136,-5.273,3*-1.384,&
       &3.405,4*.0,3*-9.013,3*.0,3*-.214,-5.084,29*.000,&
       &14*.0,2*.946,2*.0,2*-7.973,3*.0,-6.136,-5.273,3*-1.384,&
       &3.405,4*.0,3*-9.013,3*.0,3*-.214,-5.084,29*.000,&
       &14*.0,2*.946,2*.0,2*-7.973,3*.0,-6.136,-5.273,3*-1.384,&
       &3.405,4*.0,3*-9.013,3*.0,3*-.214,-5.084,29*.000,&
       &14*.0,2*11.436,2*.0,2*2.022,3*5.216,.0,-4.758,3*2.045,1.885,&
       &4*.0,3*-6.197,3*.0,3*-9.885,-10.258,29*.000,&
       &14*.0,2*-.256,2*.0,2*2.652,3*4.856,4.768,.0,3*4.931,9.642,&
       &4*.0,3*-15.079,3*.0,3*3.165,-1.913,29*.000,&
       &14*.0,2*-7.746,2*.0,2*-3.563,3*1.262,-2.119,-4.970,3*.0,&
       &1.006,4*.0,3*-11.199,3*.0,8.187,2*-2.435,-11.199,29*.0,&
       &14*.0,2*-7.746,2*.0,2*-3.563,3*1.262,-2.119,-4.970,3*.0,&
       &1.006,4*.0,3*-11.199,3*.0,8.187,2*-2.435,-11.199,29*.0,&
       &14*.0,2*-7.746,2*.0,2*-3.563,3*1.262,-2.119,-4.970,3*.0,&
       &1.006,4*.0,3*-11.199,3*.0,8.187,2*-2.435,-11.199,29*.0,&
       &14*.0,2*-5.428,2*.0,2*-15.895,3*-3.717,-2.162,-11.645,&
       &3*-1.143,5*.0,3*-34.762,3*.0,3*-23.105,-16.344,29*.000,&
       &33*.000,3*-1.235,36*.0,&
       &33*.000,3*-1.235,36*.0,&
       &33*.000,3*-1.235,36*.0,&
       &33*.000,3*-1.235,36*.0,&
       &4*.0,-5.576,9*.0,2*23.568,2*.0,2*10.41,3*7.934,6.247,12.35,&
       &3*10.199,22.442,4*1.321,6*.0,-8.189,2*-16.187,-16.401,&
       &26*.00,-5.576,2*.0,&
       &4*.0,-5.576,9*.0,2*23.568,2*.0,2*10.41,3*7.934,6.247,12.350,&
       &3*10.199,22.442,4*1.321,6*.0,-8.189,2*-16.187,-16.401,&
       &26*.00,-5.576,2*.0,&
       &4*.0,-5.576,9*.0,2*23.568,2*.0,2*10.41,3*7.934,6.247,12.350,&
       &3*10.199,22.442,4*1.321,6*.0,-8.189,2*-16.187,-16.401,&
       &26*.00,-5.576,2*.0,&
       &72*.0,&
       &72*.0,&
       &72*.0,&
       &2*4.385,2*16.194,8.276,2*7.893,1.605,6*.0,2*4.818,2*.0,&
       &2*16.179,3*.239,9.945,-3.615,3*-11.564,19.238,4*.0,3*7.992,&
       &4*.0,2*2.223,-19.532,25*.000,4.385,8.276,7.893,.0,&
       &2*4.385,2*16.194,8.276,2*7.893,1.605,6*.0,2*4.818,2*.0,&
       &2*16.179,3*.239,9.945,-3.615,3*2.868,19.238,4*.0,3*13.856,&
       &3*.0,-2.271,2*.0,8.500,25*.000,4.385,8.276,7.893,.0,&
       &2*4.385,2*16.194,8.276,2*7.893,1.605,6*.0,2*4.818,2*.0,&
       &2*16.179,3*.239,9.945,-3.615,3*2.868,19.238,4*.0,3*13.856,&
       &3*.0,-2.271,2*.0,8.500,25*.000,4.385,8.276,7.893,.0,&
       &2*9.002,5*.0,-17.109,10*.0,2*13.612,3*4.769,9.696,1.911,&
       &3*10.199,13.342,4*.0,3*13.069,3*.0,13.926,2*-8.947,26*.0,&
       &9.002,3*.0,&
       &72*.0,&
       &72*.0,&
       &72*.0,&
       &72*.0,&
       &72*.0,&
       &72*.0,&
       &72*.0,&
       &72*.0,&
       &72*.0,&
       &72*.0,&
       &72*.0,&
       &72*.0,&
       &72*.0,&
       &72*.0,&
       &72*.0,&
       &72*.0,&
       &72*.0,&
       &72*.0,&
       &72*.0,&
       &72*.0,&
       &72*.0,&
       &72*.0,&
       &72*.0,&
       &72*.0,&
       &72*.0,&
       &2*.000,2*-.083,35*.000,3*-4.661,-11.774,29*.000,&
       &2*.0,2*-4.094,.0,2*10.9,11*.0,2*5.264,13*.0,3*4.678,3*.000,&
       &3*-9.004,28*.00,10.9,.0,&
       &2*.0,2*-18.249,-13.974,5*.0,-19.486,28*.0,3*-7.893,27*.0,&
       &-13.974,2*.0,&
       &7*.000,6.175,.000,2.846,62*.000/
  data ((SICOEF2(I,J),I = 1,MAXSP),J = 1,MAXSP)/&
       &2*.0,2*1.028,35*.0,3*1.063,1.308,29*.0,&
       &2*.0,2*1.028,35*.0,3*1.063,1.308,29*.0,&
       &2*.973,2*.0,.959,2*.792,11*.0,2*.761,19*.0,3*1.247,26*.00,&
       &.973,.959,.792,.0,&
       &2*.973,2*.0,.959,2*.792,11*.0,2*.761,19*.0,3*1.247,26*.00,&
       &.973,.959,.792,.0,&
       &2*0,2*1.043,.0,2*.78,11*.0,2*.939,13*.0,3*.839,3*.0,3*1.088,&
       &28*.000,.78,.0,&
       &2*.0,2*1.263,1.282,5*.0,1.787,28*.0,1.000,29*.000,1.282,2*.0,&
       &2*.0,2*1.263,1.282,5*.0,1.787,28*.0,1.000,29*.000,1.282,2*.0,&
       &8*.0,1.168,.0,1.852,28*.0,3*.851,.707,28*.00,1.168,&
       &7*.0,.856,.0,.769,62*.000,&
       &8*.0,1.300,.0,1.975,60*.0,1.300,&
       &5*.0,2*.560,.540,.000,.506,62*.000,&
       &72*.000,&
       &72*.000,&
       &72*.000,&
       &18*.0,2*.894,3*1.007,1.332,.99,3*.961,.887,4*.0,3*1.579,&
       &3*.0,3*.902,30*.000,&
       &18*.0,2*.894,3*1.007,1.332,.99,3*.961,.887,4*.0,3*1.579,&
       &3*.0,3*.902,30*.000,&
       &72*.0,&
       &72*.0,&
       &2*.0,2*1.314,1.065,9*.0,2*1.119,4*.0,3*.844,1.048,.991,&
       &3*.954,.751,4*.0,3*1.114,3*.0,3*1.168,1.215,26*.0,1.065,2*.0,&
       &2*.0,2*1.314,1.065,9*.0,2*1.119,4*.0,3*.844,1.048,.991,&
       &3*.954,.751,4*.0,3*1.114,3*.0,3*1.168,1.215,26*.0,1.065,2*.0,&
       &14*.0,2*.993,2*.0,2*1.185,3*.0,1.176,1.086,3*1.096,.916,&
       &4*.0,3*1.36,3*.0,3*.896,1.066,29*.000,&
       &14*.0,2*.993,2*.0,2*1.185,3*.0,1.176,1.086,3*1.096,.916,&
       &4*.0,3*1.36,3*.0,3*.896,1.066,29*.000,&
       &14*.0,2*.993,2*.0,2*1.185,3*.0,1.176,1.086,3*1.096,.916,&
       &4*.0,3*1.36,3*.0,3*.896,1.066,29*.000,&
       &14*.0,2*.751,2*.0,2*.954,3*.850,.0,.998,3*.965,.872,4*.0,3*.992,&
       &3*.0,3*.994,1.058,29*.000,&
       &14*.0,2*1.01,2*.0,2*1.009,3*.921,1.002,.0,3*.992,.828,4*.0,&
       &3*1.221,3*.0,3*.876,1.001,29*.000,&
       &14*.0,2*1.041,2*.0,2*1.048,3*.912,1.036,1.008,3*.0,.88,4*.0,&
       &3*1.098,3*.0,.708,2*.849,1.098,29*.000,&
       &14*.0,2*1.041,2*.0,2*1.048,3*.912,1.036,1.008,3*.0,.88,4*.0,&
       &3*1.098,3*.0,.708,2*.849,1.098,29*.000,&
       &14*.0,2*1.041,2*.0,2*1.048,3*.912,1.036,1.008,3*.0,.88,4*.0,&
       &3*1.098,3*.0,.708,2*.849,1.098,29*.000,&
       &14*.0,2*1.128,2*.0,2*1.332,3*1.092,1.147,1.208,3*1.136,5*.0,&
       &3*1.549,3*.0,3*1.201,1.225,29*.000,&
       &33*.000,3*.935,36*.000,&
       &33*.000,3*.935,36*.000,&
       &33*.000,3*.935,36*.000,&
       &33*.000,3*.935,36*.000,&
       &4*.0,1.192,9*.0,2*.633,2*.0,2*.898,3*.88,1.008,.819,3*.911,.646,&
       &4*1.070,.0,2*1.0,3*.0,1.025,2*1.168,1.255,26*.000,1.192,2*.0,&
       &4*.0,1.192,9*.0,2*.633,2*.0,2*.898,3*.88,1.008,.819,3*.911,.646,&
       &4*1.070,1.000,5*.0,1.025,2*1.168,1.255,26*.000,1.192,2*.0,&
       &4*.0,1.192,9*.0,2*.633,2*.0,2*.898,3*.88,1.008,.819,3*.911,.646,&
       &4*1.070,1.000,5*.0,1.025,2*1.168,1.255,26*.000,1.192,2*.0,&
       &72*.000,&
       &72*.000,&
       &72*.000,&
       &2*.941,2*.802,.919,2*1.0,1.175,6*.0,2*1.109,2*.0,2*.856,3*1.116,&
       &1.006,1.142,3*1.412,.833,4*.0,3*.976,4*.0,2*.979,1.403,&
       &25*.0,.941,.919,1.175,.0,&
       &2*.941,2*.802,.919,2*1.0,1.175,6*.0,2*1.109,2*.0,2*.856,&
       &3*1.116,1.006,1.142,3*1.178,.833,4*.0,3*.856,3*.0,1.021,&
       &2*.0,.950,25*.0,.941,.919,1.175,.0,&
       &2*.941,2*.802,.919,2*1.0,1.175,6*.0,2*1.109,2*.0,2*.856,&
       &3*1.116,1.006,1.142,3*1.178,.833,4*.0,3*.856,3*.0,1.021,&
       &2*.0,.950,25*.0,.941,.919,1.175,.0,&
       &2*.756,5*.0,1.414,10*.0,2*.823,3*.938,.945,.999,3*.911,.816,&
       &4*.0,3*.797,3*.0,.713,2*1.053,26*.000,.756,3*.0,&
       &72*.000,&
       &72*.000,&
       &72*.000,&
       &72*.000,&
       &72*.000,&
       &72*.000,&
       &72*.000,&
       &72*.000,&
       &72*.000,&
       &72*.000,&
       &72*.000,&
       &72*.000,&
       &72*.000,&
       &72*.000,&
       &72*.000,&
       &72*.000,&
       &72*.000,&
       &72*.000,&
       &72*.000,&
       &72*.000,&
       &72*.000,&
       &72*.000,&
       &72*.000,&
       &72*.000,&
       &72*.000,&
       &2*.0,2*1.028,35*.0,3*1.063,1.308,29*.0,&
       &2*0,2*1.043,.0,2*.78,11*.0,2*.939,13*.0,3*.839,3*.0,3*1.088,&
       &28*.000,.78,.0,&
       &2*.0,2*1.263,1.282,5*.0,1.787,28*.0,1.000,29*.000,1.282,2*.0,&
       &7*.0,.856,.0,.769,62*.000/

  !  DATA STATEMENTS FOR ONTARIO SITE INDEX EQUATIONS
  !   1=Balsam fir (SP8), 2=Black spruce (SP9,72), 3=Tamarak (SP10)
  !    4=Black ash (SP15), 5=Black cherry (SP20), 6=Elm (SP21)
  !    7=Yellow birch (SP24), 8=Basswood (SP25), 9=Hard maple (SP26)
  !    10=White ash (SP29), 11=Red oak (SP34), 12=White birch (sp43)
  !    13=White spruce (SP6,71),
  !    14=Jack Pine(SP1,69), 15=Red Pine (Sp2&3),
  !    16=White pine (SP5,70), 17=Aspen (SP41)

  data A1/&
       &0.0061, 9.0023, 0.6464, 0.2388, 0.1073, 0.1898,&
       &0.1817, 0.1921, 0.1984, 0.1728, 0.1692, 0.5119,&
       &40.6506,23.7086,22.5992,24.4573,28.0109/
  data A2/&
       &1.3539, 1.4753, 1.000,  1.1583, 1.3455, 1.2186,&
       &1.243,  1.201, 1.2089,  1.256,  1.2648, 1.0229,&
       &5.6605,20.5596,18.7782,18.1841,24.5365/
  data A3/&
       &-0.00019, 0.7996,-0.0225, -0.0102, -0.007, -0.011,&
       &-0.011,  -0.01,  -0.011, -0.011, -0.011, -0.0167,&
       &1.2544, 17.3764,15.1961,12.4653,12.4653/
  data A4/&
       &-1.0286, 0.3976, -1.1129, -1.8455, -3.3034, -2.6865,&
       &-3.0184, -2.3009, -2.4917, -3.3605, -3.4334, -1.0284,&
       &-0.1567,14.2208, 11.5513,  6.7618, 16.9224/
  data A5/&
       &-0.0723, 19.26275, 0.0, -0.1883, -0.3899, -0.2717,&
       &-0.318, -0.2331, -0.2542, -0.3452, -0.3557, -0.0049,&
       &5*0.0/
  data THT1/13*0.0,22.56,23.0,24.42,28.53/
  data THT2/13*0.0,19.46,19.78,19.24,25.02/
  data THT3/13*0.0,16.36,16.56,14.06,21.12/
  data THT4/13*0.0,13.32,13.04,8.3,17.3/

  !  SPECIES MAPPINGS FOR ONTARIO
  !     CODES AS ABOVE.
  data OSP/&
       &14,15,15,15,16,13,13, 1, 2, 3,&
       &2, 9, 2, 2, 4, 4,17, 9, 9, 5,&
       &6, 6, 6, 7, 8, 9, 9, 9,10,11,&
       &11,11,11,11,11,11, 9, 9, 9,17,&
       &17,17,12, 9, 9, 9, 9, 9, 9, 9,&
       &9, 9, 9, 9, 9, 9, 9, 5, 9, 9,&
       &5, 5, 5,17,17,17, 9, 4,14,16,&
       &13, 2/

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'SITSET',6,ICYC)
  ! = pre LS-NEW code that may be relevant

  !        IF (KODFOR .EQ. 910 .OR. KODFOR .EQ. 907 .OR.
  !     &      KODFOR .EQ. 915 .OR. KODFOR .EQ. 916) THEN
  !      XMIN(1)=4.0
  !      XMIN(2)=4.0
  !      XMIN(8)=1.0
  !      XMIN(11)=1.0
  !      XMIN(13)=1.5
  !      XMIN(14)=1.5
  !      DO 20 I=15,68
  !        XMIN(I)=2.0
  !   20 CONTINUE
  !         XMIN(69)=4.0
  !     endif

  !  MINIMUM DBH VALUES, BY FOREST

  do I = 1,MAXSP
     select case(KODFOR)
     case(903)
        if (I.le.14.or.I.ge.68) then
           BFMIND(I) = 7.0
           DBHMIN(I) = 4.0
        else
           DBHMIN(I) = 5.
           if (I.lt.40.or.I.gt.42)BFMIND(I) = 9.
        end if
     case(904,910,913,915,916)
        if (I.gt.14.or.I.ge.68)DBHMIN(I) = 5.
     case(907)
        if (I.gt.14)DBHMIN(I) = 5.
        if (I.ge.40.and.I.le.42)BFMIND(I) = 9.
     end select
  end do

  !     DETERMINE IF WE USE THE NEW ONTARIO EQUATIONS.

  LONT = .false.
  if ((KODFOR .eq.915.or. KODFOR.eq.916) .and.&
       &OSP(ISISP) .gt.0)&
       &LONT = .true.

  !  WRITE ERROR MESSAGE IF NO SITE SPECIES AND/OR SITE CODE HAS
  !  BEEN ENTERED.
  !     NOTE THAT IF USING ONTARIO EQUATIONS, VALUES FOR SITEAR<0
  !     ARE OKAY, BECAUSE THEY ARE ACTUALLY A HEIGHT.

  if (ISISP .le.0.or. SITEAR(ISISP) .eq.0.0.or.&
       &(SITEAR(ISISP) .lt.0.0.and..not. LONT)) then
     write(JOSTND,1)
     1 format(/,3(' ***************'/),' WARNING: SITE SPECIES ',&
          &'AND/OR SITE CODE IS MISSING.  A DEFAULT IS BEING ASSIGNED.',&
          &' THIS PROJECTION SHOULD NOT BE TAKEN SERIOUSLY.',/,&
          &3(' ***************'/),/)
  end if

  !  SET DEFAULT SITE SPECIES OF RED PINE NATURAL AND INDEX OF 60
  !  IF MISSING.

  if (ISISP .le.0) ISISP = 3
  if (SITEAR(ISISP) .eq.0.0.or.&
       &(SITEAR(ISISP) .lt.0.0.and..not. LONT)) SITEAR(ISISP) = 60.

  if (LONT .and. SITEAR(ISISP) .lt.0.0) then

     !  START OF NEW ONTARIO SITE INDEX EQUATIONS
     !        WE ONLY NEED TO USE THESE IF USERS ENTERED A TOP HEIGHT INSTEAD
     !        OF A SITE INDEX.

     !       AGEBH = Age2bh (use 6 for softwoods, 4 for hardwoods)
     !       ieqag = USE AGE OF THE EQUATION, USUALLY 50, OR 56
     !       HTM = USE TOP HEIGHT ENTERED IN THE KEYWORD.

     KSP = OSP(ISISP)
     AGEBH = 6
     if (KSP .ge.3.and. KSP .le.12) AGEBH = 4
     IEQAG = 56
     if (KSP .gt.1.and. KSP .le.11.and. KSP .ne.7) IEQAG = 50
     AGEDIF = real(IEQAG - AGEBH)
     !           remember that if we get here, then SITEAR is negative!
     HTM = -SITEAR(ISISP) * FTtoM

     if (KSP.eq.2) then
        !            BLACK SPRUCE
        !            SIBH50 = a1+0.4396*(topht-1.3)+a2*LN(topht-1.3)-a3*LN(age-age2bh)-a4*(LN(age-age2bh))^2+a5*(topht-1.3)/(age-age2bh)
        if (HTM .le.1.3.or. AGEDIF .le.0.0) then
           SIM = 0.0
        else
           SIM = A1(KSP) + 0.4396*(HTM-1.3) + A2(KSP)*LOG(HTM-1.3) -&
                &A3(KSP)*LOG(AGEDIF) - A4(KSP)*(LOG(AGEDIF))**2.0 +&
                &A5(KSP)*(HTM-1.3)/(AGEDIF)
        end if

     else if (KSP .eq.3) then
        !           TAMARAK
        !           SI = (4.5+A1*(topht*3.28-4.5)*(1-exp(A3*(age-y2bh)))^ (A4))/3.28
        SIM = (4.5 + A1(KSP) * (HTM*3.28-4.5) *&
             &(1.0-exp(A3(KSP)*AGEDIF))**A4(KSP))/3.28

     else if (KSP .eq.1) then
        !           BALSAM FIR
        !          SI = (4.5 + A1*(topht*3.28)^A2*(1-exp(A3*(age-y2bh)))^ (A4*(topht*3.28)^(A5)))/3.28
        SIM = (4.5 + A1(KSP)*(HTM*3.28)**A2(KSP) *&
             &(1.0-exp(A3(KSP)*(AGEDIF)))**&
             &(A4(KSP)*(HTM*3.28)**(A5(KSP))))/3.28

     else if (KSP .eq.13) then
        !          WHITE SPRUCE
        !           SI = 1.3 + (1/(A1*(topht-1.3)^A2*(1-K^((age-age2bh)/Bhindage))^(A3 *(topht-1.3)^(A4))))
        !           K =1-(1/(A1*(topht-1.3)^(A2+1)))^(1/(A3*(topht-1.3)^(A4)))
        !           NOTE: won't work well at heights above 28 m
        KD = 1.- (1./(A1(KSP)*(HTM-1.3)**(A2(KSP)+1)))**&
             &(1.0/(A3(KSP)*(HTM-1.3)**(A4(KSP))))
        SIM = real(1.3 + (1.0/(A1(KSP)*(HTM-1.3)**A2(KSP)*&
             &(1.0-KD**(AGEDIF/50.))**(A3(KSP)*(HTM-1.3)**(A4(KSP))))))

     else if (KSP .lt.14) then
        !          MOST OTHER DEFINED SPECIES
        !          SI = (4.5+a1*(topht*3.28-4.5)^a2*(1-exp(a3*(age-y2bh)))^(a4*(topht*3.28 - 4.5)^(a5)))/3.28
        SIM = (4.5 + A1(KSP)*(HTM*3.28-4.5)**A2(KSP) *&
             &(1.-exp(A3(KSP)*AGEDIF))**(A4(KSP) *&
             &(HTM*3.28 - 4.5)**(A5(KSP))))/3.28

     else if (KSP .ge.14) then
        !          SPECIES FOR WHICH WE DON'T HAVE A CLOSED FORM.
        !           SI HAS BEEN CALCULATED FOR 4 SITE TYPES (I.E., TOP HEIGHTS).
        !          IF THE USER ENTERS A DIFFERENT ONE, THEN WE WILL JUST INTERPOLATE
        !          OR EXTRAPOLATE TO GET THE SI. NOTE THAT THE HEIGHTS ARE IN DECREASING
        !          ORDER.

        if ((HTM .ge. THT4(KSP) .and. HTM .lt. THT3(KSP)) .or.&
             &HTM .lt. THT4(KSP)) then
           SLP = (A3(KSP) - A4(KSP)) / (THT3(KSP) - THT4(KSP))
           DIFF = HTM - THT4(KSP)
           SIMIN = A4(KSP)

        else if (HTM .ge. THT3(KSP) .and. HTM .lt. THT2(KSP)) then
           SLP = (A2(KSP) - A3(KSP)) / (THT2(KSP) - THT3(KSP))
           DIFF = HTM - THT3(KSP)
           SIMIN = A3(KSP)

        else if ((HTM .ge. THT2(KSP) .and. HTM .lt. THT1(KSP)) .or.&
             &HTM .ge. THT1(KSP)) then
           SLP = (A1(KSP) - A2(KSP)) / (THT1(KSP) - THT2(KSP))
           DIFF = HTM - THT2(KSP)
           SIMIN = A2(KSP)
        end if
        SIM = DIFF * SLP + SIMIN

     end if
     if (SIM .gt.0.0) SITEAR(ISISP) = SIM * MtoFT

     !  END OF ONTARIO SECTION

  end if

  !  COMPUTE SPECIES SITE FROM STAND SITE FOR ALL SPECIES WHICH HAVE
  !  COEFFICIENTS IN THE PARAMETER FILE.  WHEN COEFFICIENTS ARE NOT
  !  AVAILABLE FOR A SPECIES, THAT SPECIES' SITE WILL BE ZERO.

  do I = 1,MAXSP
     if (SITEAR(I) .le..0001) SITEAR(I) = SICOEF1(ISISP,I)&
          &+ SICOEF2(ISISP,I)*SITEAR(ISISP)
     5 continue
  end do

  !  FOR SPECIES WHICH DID NOT HAVE SITES FIGURED ABOVE, ATTEMPT A
  !  CONVERSION THRU ASPEN.  IF COEFFICIENTS ARE NOT AVAILABLE, SITES
  !  WILL BE LEFT AS ZERO.

  if (SITEAR(41) .gt..0001) then
     do I = 1,MAXSP
        if (SITEAR(I) .le..0001) SITEAR(I) = SICOEF1(41,I)&
             &+ SICOEF2(41,I)*SITEAR(41)
        10 continue
     end do
  end if

  !  NOW SET ALL UNSET SPECIES SITES TO THE STAND SITE INDEX

  do I = 1,MAXSP
     if (SITEAR(I) .lt..0001) SITEAR(I) = SITEAR(ISISP)
     if (SDIDEF(I) .le.0.) then
        if (BAMAX .gt.0.) then
           SDIDEF(I) = BAMAX/(0.5454154*(PMSDIU/100.))
        else
           SDIDEF(I) = BAMAX1(I)/(0.5454154*(PMSDIU/100.))
        end if
     end if
     15 continue
  end do

  !  LOAD VOLUME DEFAULT MERCH. SPECS.

  do ISPC = 1,MAXSP
     if (DBHMIN(ISPC).le.0.) then !SET **DBHMIN** DEFAULT
        if (ISPC.le.14.or.ISPC.gt.68) then !SOFTWOODS
           DBHMIN(ISPC) = 5.
        else !HARDWOODS
           select case(IFOR)
           case(2)
              DBHMIN(ISPC) = 5.
              if (ISPC.ge.40.and.ISPC.le.42)DBHMIN(ISPC) = 6.
           case(6)
              DBHMIN(ISPC) = 6.
           case default
              DBHMIN(ISPC) = 5.
           end select
        end if
     end if
     if (TOPD(ISPC).le.0.)TOPD(ISPC) = 4. !SET **TOPD** DEFAULT
     if (BFMIND(ISPC).le.0.) then !SET **BFMIND** DEFAULT
        if (ISPC.le.14.or.ISPC.gt.68) then !SOFTWOODS
           BFMIND(ISPC) = 9.
        else !HARDWOODS
           select case(IFOR)
           case(2)
              BFMIND(ISPC) = 9.
              if (ISPC.ge.40.and.ISPC.le.42)BFMIND(ISPC) = 11.
           case(5)
              BFMIND(ISPC) = 11.
              if (ISPC.ge.40.and.ISPC.le.42)BFMIND(ISPC) = 9.
           case default
              BFMIND(ISPC) = 11.
           end select
        end if
     end if
     if (BFTOPD(ISPC).le.0.) then !SET **BFTOPD** DEFAULT
        if (ISPC.le.14.or.ISPC.gt.68) then !SOFTWOODS
           BFTOPD(ISPC) = 7.6
        else !HARDWOODS
           select case(IFOR)
           case(2)
              BFTOPD(ISPC) = 7.6
              if (ISPC.ge.40.and.ISPC.le.42)BFTOPD(ISPC) = 9.6
           case(5)
              BFTOPD(ISPC) = 7.6
              if (ISPC.ge.40.and.ISPC.le.42)BFTOPD(ISPC) = 7.6
           case default
              BFTOPD(ISPC) = 9.6
           end select
        end if
     end if
  end do

  !  LOAD VOLUME EQUATION ARRAYS FOR ALL SPECIES IF USING CLARK
  !  OR GEVORKIANTZ. METHB8 AND METHC8 INDEX THE NUMBER OF SPECIS
  !  USING TWIGS VOLUME EQS. (F7 =8 IN VOLUME AND BFVOLUME KEYWORDS
  !  IF ALL TWIGS EQUATIONS, THEN DON'T PRINT THE NVEL TABLE

  KFORST = KODFOR-900
  IREGN = 9
  write(FORST,'(I2)')KFORST
  if (KFORST.lt.10)FORST(1:1) = '0'
  DIST = '  '
  PROD = '  '
  VAR = 'LS'

  METHB8 = 0
  METHC8 = 0
  do ISPC = 1,MAXSP
     read(FIAJSP(ISPC),'(I4)')IFIASP
     if (((METHC(ISPC).eq.6).or.(METHC(ISPC).eq.9).or.&
          &(METHC(ISPC).eq.5)).and.(VEQNNC(ISPC).eq.'          ')) then
        if (METHC(ISPC).eq.5) then
           VOLEQ(1:7) = '900DVEE'
        else
           VOLEQ(1:7) = '900CLKE'
        end if
        call VOLEQDEF(VAR,IREGN,FORST,DIST,IFIASP,PROD,VOLEQ,ERRFLAG)
        VEQNNC(ISPC) = VOLEQ
        !      WRITE(16,*)' PROD,IFIASP,ISPC,VEQNNC(ISPC)= ',PROD,IFIASP,ISPC,
        !     &VEQNNC(ISPC)
     else if (METHC(ISPC).eq.8) then
        METHC8 = METHC8+1
     end if
     if (((METHB(ISPC).eq.6).or.(METHB(ISPC).eq.9).or.&
          &(METHB(ISPC).eq.5)).and.(VEQNNB(ISPC).eq.'           ')) then
        if (METHB(ISPC).eq.5) then
           VOLEQ(1:7) = '900DVEE'
        else
           VOLEQ(1:7) = '900CLKE'
        end if
        PROD = '01'
        call VOLEQDEF(VAR,IREGN,FORST,DIST,IFIASP,PROD,VOLEQ,ERRFLAG)
        VEQNNB(ISPC) = VOLEQ
     else if (METHC(ISPC).eq.8) then
        METHB8 = METHB8+1
     end if
  end do

  do I = 1,15
     J = (I-1)*10 + 1
     JJ = J+9
     if (JJ.gt.MAXSP)JJ = MAXSP
     write(JOSTND,90)(NSP(K,1)(1:2),K = J,JJ)
     90 format(/' SPECIES ',5X,10(A2,6X))
     write(JOSTND,91)(SDIDEF(K)/ACRtoHA,K = J,JJ )
     91 format(' SDI MAX ',   10F8.0)
     if (JJ .eq. MAXSP)go to 93
     92 continue
  end do
  93 continue

  !  IF FIA CODES WERE IN INPUT DATA, WRITE TRANSLATION TABLE

  if (LFIA) then
     call FIAHEAD(JOSTND)
     write(JOSTND,211) (NSP(I,1)(1:2),FIAJSP(I),I = 1,MAXSP)
     211 format ((T13,8(A3,'=',A6,:,'; '),A,'=',A6))
  end if

  !  WRITE VOLUME EQUATION NUMBER TABLE

  if ((METHC8.ne.MAXSP).and.(METHB8.ne.MAXSP))&
       &call VOLEQHEAD(JOSTND)
  if ((METHC8.ne.MAXSP).and.(METHB8.ne.MAXSP))&
       &write(JOSTND,230)(NSP(J,1)(1:2),VEQNNC(J),VEQNNB(J),J = 1,MAXSP)
  230 format(4(3X,A2,4X,A10,1X,A10))
  return
end
