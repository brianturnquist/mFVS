!=====================================!
!   Initialize species site indices   !
!=====================================!
! WS $Id$
subroutine SITSET
  implicit none

  ! THIS SUBROUTINE LOADS THE SITEAR ARRAY WITH A SITE INDEX FOR EACH
  ! SPECIES WHICH WAS NOT ASSIGNED A SITE INDEX BY KEYWORD.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'VOLSTD.f90'

  ! Variables
  logical :: DEBUG
  character(len=2) :: FORST
  character(len=2) :: DIST
  character(len=2) :: PROD
  character(len=2) :: VAR
  character(len=11) :: VOLEQ
  integer :: ISPC,I,INTFOR,IREGN,J,JJ,K,IFIASP,ERRFLAG
  real    :: SIAGE(MAXSP),SI(MAXSP),SDICON(MAXSP),AG,SINDX,TEMSI

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
  !      USE 4(GS) FOR 23(RW)
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

  data SDICON/&
       &561., 570., 800., 1052., 576., 365., 1000., 365., 679., 621.,&
       &272., 358., 790., 679., 409., 365., 409., 365., 214., 365.,&
       &409., 570., 1052., 687., 272., 497., 272., 667., 667., 214.,&
       &406., 440., 667., 785., 785., 562., 406., 515., 406., 629.,&
       &501., 365., 406./

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'SITSET',6,ICYC)

  if (ISISP .le.0) ISISP = 8
  if (SITEAR(ISISP) .le.0.0) SITEAR(ISISP) = 100.

  ! TRANSLATE THE SITES AND KEEP

  call SICHG(ISISP,SITEAR(ISISP),SIAGE)

  ! SI IS A VECTOR X MAXSP GIVING EQUIVALENT SITES FOR EACH SPECIES

  if (DEBUG) write(JOSTND,9003)ISISP,SITEAR(ISISP),SIAGE
  9003 format('SITE SPECIES ',I5,' SITE INDEX ',F10.1,&
       &(/,4X,10F6.1))

  do ISPC = 1,MAXSP
     AG = SIAGE(ISPC)
     SINDX = SITEAR(ISISP)

     call HTCALC(IFOR,SINDX,ISISP,AG,SI(ISPC),JOSTND,DEBUG)

     25 continue
  end do
  if (DEBUG) write(JOSTND,9025) SINDX,ISISP,SI
  9025 format('SINDX ',F10.2,'SITE SP ',I5/' TRANSLATED SITES ',&
       &(/,10F6.1))

  ! IF SITEAR() HAS NOT BEEN SET WITH SITECODE KEYWORD, LOAD
  ! IT WITH SITE VALUES TRANSLATED FROM SITE INDEX OF SITE SPECIES.

  ! FOR GB USE SITE RANGE OF 20-60; FROM TOM MARTIN R4
  ! FOR MC USE SITE RANGE OF 5-23; FROM JEPSON MANUAL
  ! THEN TAKE THE SAME PROPORTION OF THE RANGE AS THE RED FIR SITE INDEX IS TO
  ! THE RANGE 10-134 (WHICH IS THE DOLPH RED FIR RANGE IN THE SO VARIANT).

  do I = 1,MAXSP
     if (SITEAR(I) .eq.0.0) then
        select case (I)
        case(21)
           TEMSI = SI(7)
           if (TEMSI .lt.10.0)TEMSI = 10.0
           if (TEMSI .gt.134.0)TEMSI = 134.0
           SITEAR(I) = 20.0+(TEMSI-10.0)/(134.0-10.0)&
                &*(60.0-20.0)
        case(41)
           TEMSI = SI(7)
           if (TEMSI .lt.10.0)TEMSI = 10.0
           if (TEMSI .gt.134.0)TEMSI = 134.0
           SITEAR(I) = 5.0+(TEMSI-10.0)/(134.0-10.0)&
                &*(23.0-5.0)
        case default
           SITEAR(I) = SI(I)
        end select
     end if
     30 continue
  end do

  ! LOAD THE SDIDEF ARRAY

  do I = 1,MAXSP
     if (SDIDEF(I) .gt.0.0) go to 40
     if (BAMAX .gt.0.) then
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

  !  LOAD VOLUME EQUATION ARRAYS FOR ALL SPECIES

  INTFOR = KODFOR - (KODFOR/100)*100
  write(FORST,'(I2)')INTFOR
  if (INTFOR.lt.10)FORST(1:1) = '0'
  IREGN = KODFOR/100
  DIST = '  '
  PROD = '  '
  VAR = 'WS'

  if (LFIANVB) call NVB_REGION_CHECK
  do ISPC = 1,MAXSP
     read(FIAJSP(ISPC),'(I4)')IFIASP
     VOLEQ = '           '
     if (((METHC(ISPC).eq.6).or.(METHC(ISPC).eq.9)).and.&
          &(VEQNNC(ISPC).eq.'           ')) then
        call VOLEQDEF(VAR,IREGN,FORST,DIST,IFIASP,PROD,VOLEQ,ERRFLAG)
        VEQNNC(ISPC) = VOLEQ
     else if (METHC(ISPC).eq.10) then
        call NVBEQDEF(IFIASP,VOLEQ)
        VEQNNC(ISPC) = VOLEQ
     end if
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
