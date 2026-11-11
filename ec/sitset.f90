!=====================================!
!   Initialize species site indices   !
!=====================================!
! EC $Id$
subroutine SITSET
  implicit none

  ! THIS SUBROUTINE LOADS THE SITEAR ARRAY WITH A SITE INDEX FOR EACH
  ! SPECIES, GIVEN A SITE INDEX AND SITE SPECIES, AND LOADS THE SDIDEF
  ! ARRAY WITH SDI MAXIMUMS FOR SPECIES WHICH WERE NOT ASSIGNED A VALUE
  ! USING THE SDIMAX KEYWORD.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'VARCOM.f90'
  include 'VOLSTD.f90'

  ! Variables
  logical :: DEBUG
  character(len=4) :: ASPEC
  character(len=2) :: FORST
  character(len=2) :: DIST
  character(len=2) :: PROD
  character(len=2) :: VAR
  character(len=11) :: VOLEQ
  integer :: IFIASP,ERRFLAG
  integer :: NSISET,NSDSET,I,JSISP,INDEX,KNTECO,NTOHI,ISEQ,NUM
  integer :: ISFLAG,ITOHI,ISPC,K,INTFOR,IREGN,J,JJ
  real    :: FORMAX,RSI,RSDI,AG,SINDX,SI
  real    :: SIAGE(MAXSP),SDICON(MAXSP)

  !  SPECIES LIST FOR EAST CASCADES VARIANT.

  !   1 = WESTERN WHITE PINE      (WP)    PINUS MONTICOLA
  !   2 = WESTERN LARCH           (WL)    LARIX OCCIDENTALIS
  !   3 = DOUGLAS-FIR             (DF)    PSEUDOTSUGA MENZIESII
  !   4 = PACIFIC SILVER FIR      (SF)    ABIES AMABILIS
  !   5 = WESTERN REDCEDAR        (RC)    THUJA PLICATA
  !   6 = GRAND FIR               (GF)    ABIES GRANDIS
  !   7 = LODGEPOLE PINE          (LP)    PINUS CONTORTA
  !   8 = ENGELMANN SPRUCE        (ES)    PICEA ENGELMANNII
  !   9 = SUBALPINE FIR           (AF)    ABIES LASIOCARPA
  !  10 = PONDEROSA PINE          (PP)    PINUS PONDEROSA
  !  11 = WESTERN HEMLOCK         (WH)    TSUGA HETEROPHYLLA
  !  12 = MOUNTAIN HEMLOCK        (MH)    TSUGA MERTENSIANA
  !  13 = PACIFIC YEW             (PY)    TAXUS BREVIFOLIA
  !  14 = WHITEBARK PINE          (WB)    PINUS ALBICAULIS
  !  15 = NOBLE FIR               (NF)    ABIES PROCERA
  !  16 = WHITE FIR               (WF)    ABIES CONCOLOR
  !  17 = SUBALPINE LARCH         (LL)    LARIX LYALLII
  !  18 = ALASKA CEDAR            (YC)    CALLITROPSIS NOOTKATENSIS
  !  19 = WESTERN JUNIPER         (WJ)    JUNIPERUS OCCIDENTALIS
  !  20 = BIGLEAF MAPLE           (BM)    ACER MACROPHYLLUM
  !  21 = VINE MAPLE              (VN)    ACER CIRCINATUM
  !  22 = RED ALDER               (RA)    ALNUS RUBRA
  !  23 = PAPER BIRCH             (PB)    BETULA PAPYRIFERA
  !  24 = GIANT CHINQUAPIN        (GC)    CHRYSOLEPIS CHRYSOPHYLLA
  !  25 = PACIFIC DOGWOOD         (DG)    CORNUS NUTTALLII
  !  26 = QUAKING ASPEN           (AS)    POPULUS TREMULOIDES
  !  27 = BLACK COTTONWOOD        (CW)    POPULUS BALSAMIFERA var. TRICHOCARPA
  !  28 = OREGON WHITE OAK        (WO)    QUERCUS GARRYANA
  !  29 = CHERRY AND PLUM SPECIES (PL)    PRUNUS sp.
  !  30 = WILLOW SPECIES          (WI)    SALIX sp.
  !  31 = OTHER SOFTWOODS         (OS)
  !  32 = OTHER HARDWOODS         (OH)

  !  SURROGATE EQUATION ASSIGNMENT:

  !  FROM THE EC VARIANT:
  !      USE 6(GF) FOR 16(WF)
  !      USE OLD 11(OT) FOR NEW 12(MH) AND 31(OS)

  !  FROM THE WC VARIANT:
  !      USE 19(WH) FOR 11(WH)
  !      USE 33(PY) FOR 13(PY)
  !      USE 31(WB) FOR 14(WB)
  !      USE  7(NF) FOR 15(NF)
  !      USE 30(LL) FOR 17(LL)
  !      USE  8(YC) FOR 18(YC)
  !      USE 29(WJ) FOR 19(WJ)
  !      USE 21(BM) FOR 20(BM) AND 21(VN)
  !      USE 22(RA) FOR 22(RA)
  !      USE 24(PB) FOR 23(PB)
  !      USE 25(GC) FOR 24(GC)
  !      USE 34(DG) FOR 25(DG)
  !      USE 26(AS) FOR 26(AS) AND 32(OH)
  !      USE 27(CW) FOR 27(CW)
  !      USE 28(WO) FOR 28(WO)
  !      USE 36(CH) FOR 29(PL)
  !      USE 37(WI) FOR 30(WI)

  !  DATA STATEMENTS

  data SDICON/&
       &645., 648., 766., 766., 766., 766., 674., 766., 700., 645.,&
       &900., 766., 900., 900., 900., 766., 900., 900., 900., 900.,&
       &900., 900., 900., 900., 900., 900., 900., 900., 900., 900.,&
       &766., 900./

  data FORMAX/900./

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'SITSET',6,ICYC)

  !  DETERMINE HOW MANY SITE VALUES AND SDI VALUES WERE SET VIA KEYWORD.

  NSISET = 0
  NSDSET = 0
  do I = 1,MAXSP
     if (SITEAR(I) .gt.0.) NSISET = NSISET+1
     if (SDIDEF(I) .gt.0.) NSDSET = NSDSET+1
     5 continue
  end do
  if (DEBUG)write(JOSTND,*)'ENTERING SITSET, SITE VALUES SET = ',&
       &NSISET,'  SDI VALUES SET = ',NSDSET

  !  SET SITE SPECIES AND SITE INDEX IF NOT SET BY KEYWORD.
  !  ALSO SET SDI DEFAULTS

  !     REGION 6 FOREST --- CALL **ECOCLS** WITH THE ECOCLASS CODE,
  !     AND GET BACK THE DEFAULT SITE SPECIES, ALL SITE INDICIES,
  !     AND DEFAULT SDI MAXIMUMS ASSOCIATED WITH THE ECOCLASS

  JSISP = 0
  INDEX = 0
  KNTECO = 0
  NTOHI = 0
  10 call ECOCLS (PCOM,ASPEC,RSDI,RSI,ISFLAG,NUM,INDEX,ISEQ)
  KNTECO = KNTECO+1
  if (DEBUG)write(JOSTND,*)'AFTER ECOCLS,PCOM,ASPEC,RSDI,RSI,',&
       &'ISFLAG,NUM,INDEX,ISEQ = ',PCOM,ASPEC,RSDI,RSI,ISFLAG,&
       &NUM,INDEX,ISEQ

  ! IF DEFAULT SDI IS OUT OF BOUNDS, RESET IT.

  ITOHI = 0
  if (RSDI .gt. FORMAX) then
     RSDI = FORMAX
     ITOHI = 1
  end if

  if (ISEQ .eq.0) go to 25
  if (JSISP.eq.0.and. ISFLAG.eq.1) JSISP = ISEQ
  if (ISISP.le.0.and. ISFLAG.eq.1) ISISP = ISEQ
  if (SITEAR(ISEQ).le.0..and. NSISET.eq.0) SITEAR(ISEQ) = RSI
  if (SDIDEF(ISEQ) .le.0.) then
     SDIDEF(ISEQ) = RSDI
     if (ITOHI .gt.0) NTOHI = NTOHI+1
  end if
  if (ISISP.gt.0.and. ISFLAG.eq.1) then
     if (SDIDEF(ISISP) .le.0.) then
        SDIDEF(ISISP) = RSDI
        if (ITOHI .gt.0) NTOHI = NTOHI+1
     end if
  end if
  25 continue
  if (NUM.gt.1.and. KNTECO.lt.NUM) then
     INDEX = INDEX+1
     go to 10
  end if

  !  ON THE CHANCE THAT A SITE SPECIES WAS NOT ENCOUNTERED IN **ECOCLS**
  !  PROVIDE A REGION 6 GLOBAL DEFAULT.

  if (ISISP .le.0) ISISP = 10
  if (SITEAR(ISISP) .le.0.0) SITEAR(ISISP) = 70.

  ! TRANSLATE SITE INDEX TO A REFERENCE AGE FOR EACH SPECIES.

  call SICHG(ISISP,SITEAR(ISISP),SIAGE)

  ! IF SITEAR  HAS NOT BEEN SET WITH SITECODE KEYWORD, LOAD
  ! IT WITH SITE VALUES CALCULATED HERE.

  do ISPC = 1,MAXSP
     if (DEBUG)write(JOSTND,*)'IN SITSET ISISP,ISPC,SITEAR =',&
          &ISISP,ISPC,SITEAR(ISISP)
     if (SITEAR(ISPC) .gt.0.0) go to 30
     AG = SIAGE(ISPC)
     SINDX = SITEAR(ISISP)

     select case (ISPC)

        ! LOGIC FOR SPECIES FROM THE EC VARIANT

     case(1:10,12,16,31)

        call HTCALC(SINDX,ISISP,AG,SI,JOSTND,DEBUG)

        ! CHANGE THE SITE INDEX OF MH AND OS TO METRIC

        if ((ISPC.eq.12.and. ISISP.ne.12) .or.&
             &(ISPC.eq.31.and. ISISP.ne.31)) then
           SI = SI/3.281
           if (SI .gt.28.)SI = 28.
        end if

        ! LOGIC FOR SPECIES FROM THE WC VARIANT

     case(11,13:15,17:30,32)
        SI = 0.
        if (DEBUG)write(JOSTND,*)'CALLING HTCALC SINDX,ISISP,AG,SI=',&
             &SINDX,ISISP,AG,SI
        call HTCALC(SINDX,ISISP,AG,SI,JOSTND,DEBUG)
        if (DEBUG)write(JOSTND,*)'RETURN FROM HTCALC,SINDX,ISISP,AG,SI='&
             &,SINDX,ISISP,AG,SI

        ! MISC. HARDWOODS USE CURTIS SI CURVE CREATED FOR DF. THE
        ! FOLLOWING EQUATIONS ARE USED TO REDUCE THEIR GROWTH UNTIL
        ! SI CURVES ARE FOUND FOR THESE SPECIES. 4/26/01 EES.

        if (ISPC.eq.13.and. ISISP.ne.13)SI = SI*.25
        if (ISPC.eq.14.and. ISISP.ne.14)SI = SI*.70
        if (ISPC.eq.19.and. ISISP.ne.19)SI = SI*.23
        if (ISPC.eq.20.and. ISISP.ne.20)SI = SI*.75
        if (ISPC.eq.21.and. ISISP.ne.21)SI = SI*.75
        if (ISPC.eq.23.and. ISISP.ne.23)SI = SI*1.5
        if (ISPC.eq.24.and. ISISP.ne.24)SI = SI*.70
        if (ISPC.eq.25.and. ISISP.ne.25)SI = SI*.60
        if (ISPC.eq.26.and. ISISP.ne.26)SI = SI*.75
        if (ISPC.eq.27.and. ISISP.ne.27)SI = SI*.85
        if (ISPC.eq.29.and. ISISP.ne.29)SI = SI*.50
        if (ISPC.eq.30.and. ISISP.ne.30)SI = SI*.50
        if (ISPC.eq.32.and. ISISP.ne.32)SI = SI*.75

        ! FOR WHITE OAK USE METHOD DERIVED BY GOULD TO GET ESTIMATE OF MAXIMUM
        ! HEIGHT (THINK OF AS A BASE AGE 300) FROM DF, FROM KING CURVE (BASE AGE 50).

        if (ISPC.eq.28.and. ISISP.ne.28)SI = 114.2*(1-EXP(-0.0266*SI))**2.26

     end select

     SITEAR(ISPC) = SI

     30 continue
  end do

  ! LOAD THE SDIDEF ARRAY

  K = ISISP
  if (SDIDEF(K) .le.0.) K = JSISP
  do I = 1,MAXSP
     if (SDIDEF(I) .gt.0.0) go to 80
     select case (I)
     case(1:10,12,16,31)
        if (BAMAX .gt.0.) then
           SDIDEF(I) = BAMAX/(0.5454154*(PMSDIU/100.))
        else
           SDIDEF(I) = SDIDEF(K) * (SDICON(I)/SDICON(K))
        end if
        if (SDIDEF(I) .gt. FORMAX) then
           SDIDEF(I) = FORMAX
           NTOHI = NTOHI+1
        end if
     case(11,13:15,17:30,32)
        SDIDEF(I) = SDIDEF(K)
     end select
     80 continue
  end do
  if (BAMAX.le.0) BAMAX = SDIDEF(K)*(PMSDIU/100.)*0.54542

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
  if (NTOHI .gt.0)write(JOSTND,102)FORMAX
  102 FORMAT(/'*NOTE -- AT LEAST ONE DEFAULT MAXIMUM SDI EXCEEDED THE FO&
       &REST DEFAULT MAXIMUM. FOREST DEFAULT MAXIMUM OF ',F5.0,' USED.',/&
       &,'          YOU MAY NEED TO SPECIFICALLY RESET VALUES FOR THESE SP&
       &ECIES USING THE SDIMAX KEYWORD.')

  !  LOAD VOLUME EQUATION ARRAYS FOR ALL SPECIES

  INTFOR = KODFOR - (KODFOR/100)*100
  write(FORST,'(I2)')INTFOR
  if (INTFOR.lt.10)FORST(1:1) = '0'
  IREGN = KODFOR/100
  DIST = '  '
  PROD = '  '
  VAR = 'EC'

  if (LFIANVB) call NVB_REGION_CHECK
  do ISPC = 1,MAXSP
     read(FIAJSP(ISPC),'(I3)')IFIASP
     VOLEQ = '           '
     if (((METHC(ISPC).eq.6).or.(METHC(ISPC).eq.9)).and.&
          &(VEQNNC(ISPC).eq.'           ')) then
        call VOLEQDEF(VAR,IREGN,FORST,DIST,IFIASP,PROD,VOLEQ,ERRFLAG)
        VEQNNC(ISPC) = VOLEQ
     else if (METHC(ISPC).eq.10) then
        call NVBEQDEF(IFIASP, VOLEQ)
        VEQNNC(ISPC) = VOLEQ
     end if
     if (VEQNNC(ISPC)(11:11) .eq. CHAR(0)) VEQNNC(ISPC)(11:11) = " "
     if (((METHB(ISPC).eq.6).or.(METHB(ISPC).eq.9)).and.&
          &(VEQNNB(ISPC).eq.'           ')) then
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
end
