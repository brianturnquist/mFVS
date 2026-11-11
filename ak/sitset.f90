!=====================================!
!   Initialize species site indices   !
!=====================================!
! AK $Id$
subroutine SITSET
  implicit none

  !  THIS SUBROUTINE LOADS THE SITEAR ARRAY WITH A SITE INDEX FOR EACH
  !  SPECIES WHICH WAS NOT ASSIGNED A SITE INDEX BY KEYWORD, LOADS
  !  THE SDIDEF ARRAY WITH SDI MAXIMUMS FOR EACH SPECIES WHICH WAS NOT
  !  ASSIGNED AN SDI MAXIMUM USING THE SDIMAX KEYWORD, AND LOADS VOLUME
  !  MERCHANTABILITY SPECIFICATIONS AND VOLUME EQUATIONS.

  ! SPECIES LIST FOR ALASKA VARIANT.

  ! Number Code  Common Name         FIA  PLANTS Scientific Name
  !   1     SF   Pacific silver fir  011  ABAM   Abies amabilis
  !   2     AF   subalpine fir       019  ABLA   Abies lasiocarpa
  !   3     YC   Alaska cedar        042  CANO9  Callitropsis nootkatensis
  !   4     TA   tamarack            071  LALA   Larix laricina
  !   5     WS   white spruce        094  PIGL   Picea glauca
  !   6     LS   Lutz's spruce            PILU   Picea lutzii
  !   7     BE   black spruce        095  PIMA   Picea mariana
  !   8     SS   Sitka spruce        098  PISI   Picea sitchensis
  !   9     LP   lodgepole pine      108  PICO   Pinus contorta
  !  10     RC   western redcedar    242  THPL   Thuja plicata
  !  11     WH   western hemlock     263  TSHE   Tsuga heterophylla
  !  12     MH   mountain hemlock    264  TSME   Tsuga mertensiana
  !  13     OS   other softwoods     298  2TN
  !  14     AD   alder species       350  ALNUS  Alnus species
  !  15     RA   red alder           351  ALRU2  Alnus rubra
  !  16     PB   paper birch         375  BEPA   Betula papyrifera
  !  17     AB   Alaska birch        376  BENE4  Betula neoalaskana
  !  18     BA   balsam poplar       741  POBA2  Populus balsamifera
  !  19     AS   quaking aspen       746  POTR5  Populus tremuloides
  !  20     CW   black cottonwood    747  POBAT  Populus trichocarpa
  !  21     WI   willow species      920  SALIX  Salix species
  !  22     SU   Scouler's willow    928  SASC   Salix scouleriana
  !  23     OH   other hardwoods     998  2TB

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'VOLSTD.f90'

  !  DEFINITIONS

  !     DIST --
  !    FORST --
  !     PROD --
  !      VAR --
  !    VOLEQ --
  !  ERRFLAG --
  !        I -- INDEX USED FOR LOOPING ACROSS SPECIES
  !   IFIASP --
  !   INTFOR --
  !    IREGN --
  !     ISPC -- INDEX USED FOR LOOPING ACROSS SPECIES
  !        J -- INDEX USED FOR PRINTING REPORTS
  !       JJ -- INDEX USED FOR PRINTING REPORTS
  !        K -- INDEX USED FOR PRINTING REPORTS
  ! MERCHCDS --
  ! MERCHCAT --
  !      TEM -- DEFAULT SITE INDEX, SET TO 70.
  !   SDICON -- ARRAY CONTAINING SDI MAXIMUM FOR EACH SPECIES
  !               (BASED ON SHAW AND LONG ANALYSIS)
  !      SLO -- ARRAY CONTAINING MINIMUM SITE INDEX VALUES FOR EACH SPECIES
  !             ALL SPECIES OTHER THAN WH, MH, AND SS LT 200 YEARS OLD:
  !               HEGYI, F., JELINEK, J., VISZLAI, J.,CARPENTER, D.,
  !               BRITNEFF, A. 1981.SITE INDEX EQUATIONS AND CURVES
  !               FOR THE MAJOR TREE SPECIES IN BRITISH COLUMBIA.
  !               TABLE 4 (100 YR BASE AGE)
  !             WH, MH, AND SS LESS THAN 200 YEARS OF AGE: PAYANDEH, B.
  !               1974. NONLINEAR SITE INDEX EQUATIONS FOR SEVERAL MAJOR
  !               CANADIAN TIMBER SPECIES. THE FORESTRY CHRONICLE. 194-
  !               196.
  !      SHI -- ARRAY CONTAINING MAXIMUM SITE INDEX VALUES FOR EACH
  !               SPECIES. CITATIONS ARE THE SAME AS SLO.
  ! NOTE: CAP SLO and SHI FOR AD, WI, SU, AND OH BASED ON REDUCTIONS MADE
  !       FOR HEIGHT DUBBING, LARGE TREE DIAMETER GROWTH, AND LARGE TREE
  !       HEIGHT GROWTH.
  !         AD, WI, OH: ASSUME MULTIPLIER OF 0.45
  !         SU: ASSUME MULTIPLIER OF 0.65
  ! NOTE -- BASE AGE 100 USED IN SPRTHT (ESSPRT.F) TO DETERMINE HEIGHT
  !         OF SPROUT RECORDS.

  ! Variables
  character(len=2) :: DIST
  character(len=2) :: FORST
  character(len=2) :: PROD
  character(len=2) :: VAR
  character(len=11) :: VOLEQ

  integer :: ERRFLAG,I,IFIASP,INTFOR,IREGN,ISPC,J,JJ,K,MERCHCDS(2,17),MERCHCAT

  real    :: TEM

  real    :: SDICON(MAXSP),SLO(MAXSP),SHI(MAXSP)

  !  DATA STATEMENTS:

  data SDICON / &
       & 790., 602., 592., 387., 412., 412., 500., 654., 679., 762., &
       & 682., 687., 412., 441., 441., 466., 466., 384., 562., 452., &
       & 447., 447., 452./

  data SLO / &
       & 35., 20., 25., 25., 30., 30., 20., 40., 15., 30., &
       & 35., 20., 30., 20., 75., 35., 45., 45., 45., 45., &
       & 20., 30., 20./

  data SHI / &
       & 105.,  65.,  75.,  55.,  85.,  85.,  55., 125.,  55.,  85., &
       & 105.,  65.,  85., 50., 135., 75., 90., 105.,  85., 105., &
       & 50.,  70.,  50./

  !  LOAD VOLUME DEFAULT MERCH. SPECS.

  !  MERCHCDS CONTAINS MERCH SPEC CATEOGORIES (MERCHCAT) FOR EACH
  !  LOCATION CODE
  data MERCHCDS/ &
       & 713,  1, &
       & 720,  2, &
       & 7400, 2, &
       & 7401, 2, &
       & 7402, 2, &
       & 7403, 2, &
       & 7404, 2, &
       & 7405, 2, &
       & 7406, 2, &
       & 7407, 2, &
       & 7408, 2, &
       & 703,  3, &
       & 1005, 3, &
       & 8134, 3, &
       & 8135, 3, &
       & 8112, 3, &
       & 1004, 4/

  ! IF SITEAR(I) HAS NOT BEEN SET WITH SITECODE KEYWORD, LOAD IT
  ! WITH DEFAULT SITE VALUES.

  TEM = 70.
  if (ISISP .gt. 0) then
     if (SITEAR(ISISP) .gt. 0.0) TEM = SITEAR(ISISP)
  end if
  if (ISISP .eq. 0) ISISP = 11

  ! TRANSLATE SITE SPECIES SITE INDEX TO OTHER SPECIES

  do I = 1,MAXSP
     if (TEM .lt. SLO(ISISP))TEM = SLO(ISISP)
     if (SITEAR(I) .le. 0.0) SITEAR(I) = SLO(I) + &
          & (TEM-SLO(ISISP))/(SHI(ISISP)-SLO(ISISP))* &
          & (SHI(I)-SLO(I))

     10 continue
  end do

  ! LOAD THE SDIDEF ARRAY.

  do I = 1,MAXSP
     if (SDIDEF(I) .gt. 0.0) go to 40
     if (BAMAX .gt. 0.) then
        SDIDEF(I) = BAMAX/(0.5454154*(PMSDIU/100.))
     else
        SDIDEF(I) = SDICON(I)
     end if
     40 continue
  end do

  do I = 1,15
     J = (I-1)*10 + 1
     JJ = J+9
     if (JJ.gt.MAXSP)JJ = MAXSP
     write(JOSTND,90)(NSP(K,1)(1:2),K = J,JJ)
     90 format(/'SPECIES ',5X,10(A2,6X))
     write(JOSTND,91)(NINT(SDIDEF(K)),K = J,JJ )
     91 format('SDI MAX ',   10I8.0)
     if (JJ .eq. MAXSP)go to 93
     92 continue
  end do
  93 continue

  !  LOOP THROUGH MERCHCDS AND SELECT MERCHCAT BASED ON
  !  LOCATION CODE (KODFOR - 17 OPTIONS)

  do I = 1, 17
     if (KODFOR .eq. MERCHCDS(1,I)) then
        MERCHCAT = MERCHCDS(2,I)
        exit
        !  DEFAULT TO TONGASS IF LOCATION CODE IS NOT FOUND IN
        !  MERCHCDS
     else
        MERCHCAT = 3
     end if
  end do

  !  DETERMINE DBHMIN, BFMIND, BFTOPD, AND TOPD BASED ON MERCHCAT

  !  SET DBHMIN DEFAULTS
  do ISPC = 1,MAXSP
     if (DBHMIN(ISPC) .le. 0) then
        select case(MERCHCAT)
        case(1)
           DBHMIN(ISPC) = 6
        case(2)
           DBHMIN(ISPC) = 5
        case(3, 4)
           DBHMIN(ISPC) = 9
        case default
           DBHMIN(ISPC) = 9
        end select
     end if
     !  SET BFMIND DEFAULTS
     if (BFMIND(ISPC) .le. 0) then
        select case(MERCHCAT)
        case(1, 2, 3, 4)
           BFMIND(ISPC) = 9
        case default
           BFMIND(ISPC) = 9
        end select
     end if
     !  SET TOPD DEFAULTS
     if (TOPD(ISPC) .le. 0) then
        select case(MERCHCAT)
        case(1, 2)
           TOPD(ISPC) = 4
        case(3)
           TOPD(ISPC) = 7
        case(4)
           TOPD(ISPC) = 6
        case default
           TOPD(ISPC) = 7
        end select
     end if
     !  SET BFTOPD DEFAULTS
     if (BFTOPD(ISPC) .le. 0) then
        select case(MERCHCAT)
        case(1,2,4)
           BFTOPD(ISPC) = 6
        case(3)
           BFTOPD(ISPC) = 7
        case default
           BFTOPD(ISPC) = 7
        end select
     end if
     !  SET SAWLOG CUBIC DEFAULTS
     if (SCFMIND(ISPC) .le. 0) then
        select case(MERCHCAT)
        case(1, 2, 3, 4)
           SCFMIND(ISPC) = 9
        case default
           SCFMIND(ISPC) = 9
        end select
     end if
     if (SCFTOPD(ISPC) .le. 0) then
        select case(MERCHCAT)
        case(1,2,4)
           SCFTOPD(ISPC) = 6
        case(3)
           SCFTOPD(ISPC) = 7
        case default
           SCFTOPD(ISPC) = 7
        end select
     end if
  end do

  !  LOAD VOLUME EQUATION ARRAYS FOR ALL SPECIES

  INTFOR = KODFOR - (KODFOR/100)*100
  write(FORST,'(I2)')INTFOR
  !     CHANGED CONDITION FROM .LT. TO .NE. IN ORDER TO DEAL WITH NEW
  !     LOCATION CODES FOR STATE, BLM, TCC, AND TRIBAL LANDS
  if (INTFOR.ne.10)FORST(1:1) = '0'
  IREGN = KODFOR/100
  DIST = '  '
  PROD = '  '
  VAR = 'AK'

  !    BRITISH COLUMBIA, MAKAH INDIAN RESERVATION AND IREGN
  !    81 ARE MAPPED TO THE TONGASS

  !    LOCATION CODES 713, 720 AND IREGN 81 ARE MAPPED TO
  !    THE CHUGACH

  if ((KODFOR.eq.713).or.(KODFOR.eq.720).or.(IREGN.eq.74)) then
     IREGN = 10
     FORST = '04'
  else if ((KODFOR.eq.703).or.(IREGN.eq.81)) then
     IREGN = 10
     FORST = '05'
  end if
  ! LOAD VOLUME ARRAYS USING VOLEQDEF
  if (LFIANVB) call NVB_REGION_CHECK
  do ISPC = 1,MAXSP
     read(FIAJSP(ISPC),'(I4)')IFIASP
     VOLEQ = '           '
     if (((METHC(ISPC).eq.6).or.(METHC(ISPC).eq.9)).and. &
          & (VEQNNC(ISPC).eq.'           ')) then
        call VOLEQDEF(VAR,IREGN,FORST,DIST,IFIASP,PROD,VOLEQ,ERRFLAG)
        VOLEQ(11:11) = ' '
        VEQNNC(ISPC) = VOLEQ

     else if ((METHC(ISPC).eq.10).and. &
          & (VEQNNC(ISPC).eq.'          ')) then
        if (JSP(ISPC).eq. 'LS') IFIASP = 94
        call NVBEQDEF(IFIASP,VOLEQ)
        VEQNNC(ISPC) = VOLEQ
     end if
     if (((METHB(ISPC).eq.6).or.(METHB(ISPC).eq.9)).and. &
          & (VEQNNB(ISPC).eq.'           ')) then
        VOLEQ = '           '
        call VOLEQDEF(VAR,IREGN,FORST,DIST,IFIASP,PROD,VOLEQ,ERRFLAG)
        VEQNNB(ISPC) = VOLEQ
     end if
  end do

  !  IF FIA CODES WERE IN INPUT DATA, WRITE TRANSLATION TABLE

  if (LFIA) then
     call FIAHEAD(JOSTND)
     write(JOSTND,211) (NSP(I,1)(1:2),FIAJSP(I),I = 1,MAXSP)
     211 format ((T12,8(A3,'=',A6,:,'; '),A,'=',A6))
  end if

  !  WRITE VOLUME EQUATION NUMBER TABLE

  call VOLEQHEAD(JOSTND)
  write(JOSTND,230)(NSP(J,1)(1:2),VEQNNC(J),VEQNNB(J),J = 1,MAXSP)
  230 format(4(2X,A2,4X,A11,1X,A10,1X))

  return
end subroutine SITSET
