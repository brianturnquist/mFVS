!=========================================!
!   Calibrate tree growth relationships   !
!=========================================!
! VBASE $Id$
subroutine CRATET
  implicit none

  !  THIS SUBROUTINE IS CALLED PRIOR TO PROJECTION.  IT HAS THE
  !  FOLLOWING FUNCTIONS:

  !    1)  CALL **RCON** TO LOAD SITE DEPENDENT MODEL COEFFICIENTS.
  !    2)  REGRESSION TO ESTIMATE COEFFICIENTS OF LOCAL HEIGHT-
  !        DIAMETER RELATIONSHIP.
  !    3)  DUB IN MISSING HEIGHTS.
  !    4)  CALL **DENSE** TO COMPUTE STAND DENSITY.
  !    5)  SCALE CROWN RATIOS AND CALL **CROWN** TO DUB IN ANY MISSING
  !        VALUES.
  !    6)  DEFINE DG BASED ON CALIBRATION CONTROL PARAMETERS AND
  !        CALL **DGDRIV** TO CALIBRATE DIAMETER GROWTH EQUATIONS.
  !    7)  DELETE DEAD TREES FROM INPUT TREE LIST AND REALIGN IND1
  !        AND ISCT.
  !    8)  PRINT A TABLE DESCRIBING CONTROL PARAMETERS AND INPUT
  !        VARIABLES.

  !  SHARED CRATET: ORIGINAL EQUATIONS, INITIALIZATION AND CALL ORDER.
  !  CONSTANT VARIANT GUARDS SELECT EACH LEGACY PATH WITHOUT ADDING
  !  DEPENDENCIES ON OPTIONAL VARIANT ROUTINES IN OTHER EXECUTABLES.
  !  LABELS ARE UNIQUE ACROSS ALTERNATIVES; DIAGNOSTIC TEXT IS UNCHANGED.
  !  REGIONAL NOTES RETAIN THEIR SOURCE CONTEXT. SPECIES INDICES ALWAYS
  !  REFER TO THE SELECTED VARIANT'S BLKDAT ORDER.

  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'
  include 'OUTCOM.f90'
  include 'HTCAL.f90'
  include 'VARCOM.f90'
  include 'GGCOM.f90'
  include 'ORGANON.f90'
  include 'BCPLOT.f90'
  include 'METRIC.f90'

  !  CI/KT KEEP SCALAR AAHT/BBHT SEPARATE FROM THE VARCOM ARRAYS AA/BB.
  !  NATIVE DATA TABLE LENGTHS AND ORGANON LABEL WIDTHS ARE RETAINED.
  !  KT KEEPS THE CONDITIONAL XN ASSIGNMENT; SEE FOUND_BUGS 19.
  logical :: DEBUG
  logical :: MISSCR
  logical :: TKILL
  character(len=4) :: UNDER
  integer :: I
  integer :: J
  integer :: II
  integer :: ISPC
  integer :: IPTR
  integer :: I1
  integer :: I2
  integer :: I3
  integer :: K1
  integer :: K2
  integer :: K3
  integer :: K4
  integer :: JJ
  integer :: IS
  integer :: IM
  integer :: NH
  integer :: KNT2(MAXSP)
  integer :: KNT(MAXSP)
  real :: SPCNT(MAXSP,3)
  real :: D2
  real :: SITAGE
  real :: SITHT
  real :: AGMAX
  real :: HTMAX
  real :: HTMAX2
  real :: AX
  real :: Q
  real :: SUMX
  real :: H
  real :: D
  real :: BX
  real :: XX
  real :: YY
  real :: XN
  real :: HS
  real :: D1
  integer :: IDM1
  real :: TEMCCF
  real :: DM1
  real :: CCFT
  real :: CW
  real :: P
  real :: SI
  integer :: IMR
  real :: P2
  real :: P3
  real :: P4
  real :: YOUNG
  real :: OLD
  integer :: K
  integer :: JCR
  integer :: IICR
  real :: BKPT
  real :: A(6)
  real :: B(6)
  real :: C(6)
  real :: R5SI
  real :: SC
  real :: AHATR5
  real :: BHATR5
  real :: CHATR5
  real :: SIZE
  real :: CONST
  real :: AAHT
  real :: BBHT
  real :: SMHD1(19)
  real :: SMHD2(19)
  real :: IABF
  integer :: IERRCK
  integer :: ISPFL(MAXSP)
  real :: RAD
  real :: SOLN
  real :: COUNTER
  real :: SUMXX
  real :: SUMYY
  real :: SUMXY
  real :: SUMX2
  real :: XY
  real :: X2
  real :: STEP1
  real :: SMA(11)
  real :: SMDBH(11)
  real :: HTKEEP(MAXTRE)
  real :: Z1
  real :: Z2
  real :: AX1
  real :: BX1
  real :: CX1
  real :: XMAX
  character(len=21) :: LABEL_OC(18)
  integer*4 :: IEVEN
  integer*4 :: IRAD
  integer :: NEWCAL(18)
  integer :: ITEM(2000)
  integer :: KG
  integer :: KNTOHT(MAXSP)
  integer :: KNTOCR(MAXSP)
  integer :: NBIG6
  integer :: NVALID
  integer :: NLOAD
  integer :: NUNLOAD
  integer :: ORGFIA
  integer :: IHFLAG
  real*4 :: RADGRO(2000)
  real*4 :: GROWTH(2000)
  real*4 :: TMPCAL(3,18)
  character(len=31) :: LABEL_OP(11)
  real :: HM
  real :: DM

  !  ORIGINAL EM DATA; NATIVE SPECIES ORDER AND PRECISION.

  !  SPECIES ORDER:
  !   1=WB,  2=WL,  3=DF,  4=LM,  5=LL,  6=RM,  7=LP,  8=ES,
  !   9=AF, 10=PP, 11=GA, 12=AS, 13=CW, 14=BA, 15=PW, 16=NC,
  !  17=PB, 18=OS, 19=OH

  !  SPECIES EXPANSION
  !  LM USES IE LM (ORIGINALLY FROM TT VARIANT)
  !  LL USES IE AF (ORIGINALLY FROM NI VARIANT)
  !  RM USES IE JU (ORIGINALLY FROM UT VARIANT)
  !  AS,PB USE IE AS (ORIGINALLY FROM UT VARIANT)
  !  GA,CW,BA,PW,NC,OH USE IE CO (ORIGINALLY FROM CR VARIANT)

  !  INITIALIZE INTERNAL VARIABLES:

  data UNDER/'----'/
  !  ORIGINAL CS SPECIES/DATA NOTE:

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

  !  INITIALIZE INTERNAL VARIABLES:

  !  ORIGINAL BM SPECIES/DATA NOTE:

  !  SPECIES ORDER:
  !   1=WP,  2=WL,  3=DF,  4=GF,  5=MH,  6=WJ,  7=LP,  8=ES,
  !   9=AF, 10=PP, 11=WB, 12=LM, 13=PY, 14=YC, 15=AS, 16=CW,
  !  17=OS, 18=OH

  !  SPECIES EXPANSION:
  !  WJ USES SO JU (ORIGINALLY FROM UT VARIANT; REALLY PP FROM CR VARIANT)
  !  WB USES SO WB (ORIGINALLY FROM TT VARIANT)
  !  LM USES UT LM
  !  PY USES SO PY (ORIGINALLY FROM WC VARIANT)
  !  YC USES WC YC
  !  AS USES SO AS (ORIGINALLY FROM UT VARIANT)
  !  CW USES SO CW (ORIGINALLY FROM WC VARIANT)
  !  OS USES BM PP BARK COEFFICIENT
  !  OH USES SO OH (ORIGINALLY FROM WC VARIANT)

  !  DATA STATEMENTS:

  !  ORIGINAL EC SPECIES/DATA NOTE:

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

  !  INITIALIZE INTERNAL VARIABLES:

  !  ORIGINAL WS DATA; NATIVE SPECIES ORDER AND PRECISION.

  data A/-17.7893,-17.2775,-16.5610,-15.4964,-12.4808,-14.7932/
  !  ORIGINAL WS DATA; NATIVE SPECIES ORDER AND PRECISION.
  data B/ 8.4685, 10.4642, 8.8542, 9.7091, 7.7915, 8.3094/
  !  ORIGINAL WS DATA; NATIVE SPECIES ORDER AND PRECISION.
  data C/ 7.6272, 6.2137, 7.3138, 6.4437, 7.1568, 6.9113/
  !  ORIGINAL CI DATA; NATIVE SPECIES ORDER AND PRECISION.

  !     SPECIES LIST FOR CENTRAL IDAHO VARIANT.

  !     1 = WESTERN WHITE PINE (WP)          PINUS MONTICOLA
  !     2 = WESTERN LARCH (WL)               LARIX OCCIDENTALIS
  !     3 = DOUGLAS-FIR (DF)                 PSEUDOTSUGA MENZIESII
  !     4 = GRAND FIR (GF)                   ABIES GRANDIS
  !     5 = WESTERN HEMLOCK (WH)             TSUGA HETEROPHYLLA
  !     6 = WESTERN REDCEDAR (RC)            THUJA PLICATA
  !     7 = LODGEPOLE PINE (LP)              PINUS CONTORTA
  !     8 = ENGLEMANN SPRUCE (ES)            PICEA ENGELMANNII
  !     9 = SUBALPINE FIR (AF)               ABIES LASIOCARPA
  !    10 = PONDEROSA PINE (PP)              PINUS PONDEROSA
  !    11 = WHITEBARK PINE (WB)              PINUS ALBICAULIS
  !    12 = PACIFIC YEW (PY)                 TAXUS BREVIFOLIA
  !    13 = QUAKING ASPEN (AS)               POPULUS TREMULOIDES
  !    14 = WESTERN JUNIPER (WJ)             JUNIPERUS OCCIDENTALIS
  !    15 = CURLLEAF MOUNTAIN-MAHOGANY (MC)  CERCOCARPUS LEDIFOLIUS
  !    16 = LIMBER PINE (LM)                 PINUS FLEXILIS
  !    17 = BLACK COTTONWOOD (CW)            POPULUS BALSAMIFERA VAR. TRICHOCARPA
  !    18 = OTHER SOFTWOODS (OS)
  !    19 = OTHER HARDWOODS (OH)

  !  SURROGATE EQUATION ASSIGNMENT:

  !  FROM THE IE VARIANT:
  !      USE 17(PY) FOR 12(PY)             (IE17 IS REALLY TT2=LM)
  !      USE 18(AS) FOR 13(AS)             (IE18 IS REALLY UT6=AS)
  !      USE 13(LM) FOR 11(WB) AND 16(LM)  (IE13 IS REALLY TT2=LM)
  !      USE 19(CO) FOR 17(CW) AND 19(OH)  (IE19 IS REALLY CR38=OH)

  !  FROM THE UT VARIANT:
  !      USE 12(WJ) FOR 14(WJ)
  !      USE 20(MC) FOR 15(MC)             (UT20 = SO30=MC, WHICH IS
  !                                                  REALLY WC39=OT)

  !  DATA STATEMENTS

  data SMHD1/&
       &1.74189, 5.30838, 3.05990, 2.77647, 2.77647,&
       &5.30838, 0.74322, 2.88424, 2.74231, 1.74189,&
       &0.,      0.,      0.,      0.,      0.,&
       &0.,      0., 0.74322,      0./
  !  ORIGINAL CI DATA; NATIVE SPECIES ORDER AND PRECISION.
  data SMHD2/&
       &4.17687, 6.41536, 6.42592, 5.59435, 5.59435,&
       &6.41536, 9.23147, 5.39267, 5.35911, 4.17687,&
       &0.,      0.,      0.,      0.,      0.,&
       &0.,      0., 9.23147,      0./
  !  ORIGINAL KT DATA; NATIVE SPECIES ORDER AND PRECISION.
  data SMA/4.43,7.49,5.71,5.00,5.32,4.69,7.49,4.98,4.74,3.83,3.92/
  !  ORIGINAL KT DATA; NATIVE SPECIES ORDER AND PRECISION.
  data SMDBH/4.17,5.84,5.50,4.74,4.74,5.21,2.42,4.96,3.43,5.94,6.17/
  !  ORIGINAL AK SPECIES/DATA NOTE:

  !  SPECIES ORDER:
  !  1   2   3   4   5   6   7   8   9   10  11  12
  !  SF  AF  YC  TA  WS  LS  BE  SS  LP  RC  WH  MH

  !  13  14  15  16  17  18  19  20  21  22  23
  !  OS  AD  RA  PB  AB  BA  AS  CW  WI  SU  OH

  !  DATA STATEMENTS:

  !  ORIGINAL OC DATA; NATIVE SPECIES ORDER AND PRECISION.

  data LABEL_OC/&
       &'         DOUGLAS FIR:',&
       &'     WHITE/GRAND FIR:',&
       &'      PONDEROSA PINE:',&
       &'          SUGAR PINE:',&
       &'       INCENSE-CEDAR:',&
       &'     WESTERN HEMLOCK:',&
       &'    WESTERN REDCEDAR:',&
       &'         PACIFIC YEW:',&
       &'     PACIFIC MADRONE:',&
       &'    GIANT CHINQUAPIN:',&
       &'              TANOAK:',&
       &'     CANYON LIVE OAK:',&
       &'       BIGLEAF MAPLE:',&
       &'    OREGON WHITE OAK:',&
       &'CALIFORNIA BLACK OAK:',&
       &'           RED ALDER:',&
       &'     PACIFIC DOGWOOD:',&
       &'      WILLOW SPECIES:'/
  !  ORIGINAL OP DATA; NATIVE SPECIES ORDER AND PRECISION.

  data LABEL_OP/&
       &'                   DOUGLAS FIR:',&
       &'                     GRAND FIR:',&
       &'               WESTERN HEMLOCK:',&
       &'              WESTERN REDCEDAR:',&
       &'                   PACIFIC YEW:',&
       &' WHITE ALDER / PACIFIC MADRONE:',&
       &'                 BIGLEAF MAPLE:',&
       &'  OREGON WHITE / CAL BLACK OAK:',&
       &'                     RED ALDER:',&
       &'               PACIFIC DOGWOOD:',&
       &'                WILLOW SPECIES:'/

  if (VARIANTCD.eq.'KT') then
     RAD = 0.
     SOLN = 0.
  end if

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'CRATET',6,ICYC)
  if (VARIANTCD.eq.'IE'.or.VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'CI'.or.&
       &VARIANTCD.eq.'BC') then
     !  ORIGINAL IE,BC NOTE:

     !  GROWTH EQUATIONS USED FOR LM AND PY WERE FIT
     !  USING SITE INDEX ADJUSTED TO A 50-YEAR AGE BASE.
     !  ADJUST THE SITE VALUES FOR THESE SPECIES TO THIS BASIS.

     !  FIRST NEED AN ESTIMATE OF STAND CCF

     !  ORIGINAL TT,UT NOTE:
     !  TT: WB, LM, BS, LP, ES, AF, OS; UT ALSO ADJUSTS WF AND PP.
     !  THESE GROWTH EQUATIONS WERE FIT
     !  USING SITE INDEX ADJUSTED TO A 50-YEAR AGE BASE. ADJUST THE SITE VALUES
     !  FOR THESE SPECIES TO THIS BASIS.

     !  FIRST NEED AN ESTIMATE OF STAND CCF

     !  ORIGINAL CI NOTE:

     !  GROWTH EQUATIONS USED FOR WB, LM, AND PY WERE FIT
     !  USING SITE INDEX ADJUSTED TO A 50-YEAR AGE BASE.
     !  ADJUST THE SITE VALUES FOR THESE SPECIES TO THIS BASIS.

     !  FIRST NEED AN ESTIMATE OF STAND CCF

     TEMCCF = 0.
  end if
  if (VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP') then

     !  ORGANON
     !  IF NECESSARY, CALL PREPARE TO FILL IN THE MISSING VALUES
     !  LORGPREP=.TRUE. IF DATA HAS ALREADY BEEN PREPARED/EDITED
     !  IF THIS IS A BARE GROUND PLANT, THEN SKIP THIS SECTION

     do I = 1,18
        TMPCAL(1,I) = 1.0
        TMPCAL(2,I) = 1.0
        TMPCAL(3,I) = 1.0
        NEWCAL(I) = 0
     end do
     if ( DEBUG ) then
        write(JOSTND,124) ICYC,IMODTY,LORGPREP
     end if
     do I = 1,MAXSP
        KNTOHT(I) = 0
        KNTOCR(I) = 0
     end do
     NVALID = 0
     NBIG6 = 0
     if (LORGPREP .or. ITRN.le.0.or. (.not. LORGANON)) go to 93

     !     ASSIGN THE SPECIES SPECIFIC VALUES FOR THE STAND LEVEL VARIABLES
     !     SO THAT ORGANON WILL IMPUTE THE VALUES WHEN IT CALLS PREPARE

     !     ORGANON VARIANT: SWO

     if (DEBUG)write(JOSTND,*)' RVARS(1-5)= ',(RVARS(I),I = 1,5)
     if (DEBUG)write(JOSTND,*)' INDS(1-10)= ',(INDS(I),I = 1,10)
  end if
  if (VARIANTCD.eq.'IE'.or.VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'CI'.or.&
       &VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'BC') then
     !  ORIGINAL OC NOTE:

     !  SET ORGANON TREE INDICATOR (0 = NO, 1 = YES).
     !  VALID TREE:
     !    SPECIES 2=IC,  3=RC,  4=GF/WF, 7=DF,  8=WH, 16=SP, 18=PP, 24=PY,
     !           27=CY, 30=WO, 31=BO,   34=BM, 36=RA, 37=MA, 38=GC, 39=DG,
     !           42=TO, 46=WI
     !    HEIGHT HT > 4.5 FEET
     !    DBH    DBH >= 0.1 INCH

     ! THE CALL TO PREPARE IS TO EDIT THE DATA, DUB MISSING VALUES, AND CALIBRATE THE
     ! ORGANON EQUATIONS TO THE INPUT DATA. WE WILL ONLY PASS VALID ORGANON TREES TO
     ! PREPARE FOR THIS PURPOSE.

     ! ALSO CHECK FOR MAJOR TREE SPECIES; IF NONE, SKIP CALL TO PREPARE AND
     ! USE FVS LOGIC.

     !  ORIGINAL OP NOTE:

     !  SET ORGANON TREE INDICATOR (0 = NO, 1 = YES).
     !  VALID TREE:
     !    SPECIES 3=GF, 16=DF, 18=RC 19=WH, 21=BM, 22=RA, 23=MA, 28=WO,
     !           33=PY, 34=DG, 37=WI
     !    HEIGHT HT > 4.5 FEET
     !    DBH    DBH >= 0.1 INCH

     ! THE CALL TO PREPARE IS TO EDIT THE DATA, DUB MISSING VALUES, AND CALIBRATE THE
     ! ORGANON EQUATIONS TO THE INPUT DATA. WE WILL ONLY PASS VALID ORGANON TREES TO
     ! PREPARE FOR THIS PURPOSE.

     ! ALSO CHECK FOR MAJOR TREE SPECIES; IF NONE, SKIP CALL TO PREPARE AND
     ! USE FVS LOGIC.

     do I = 1,IREC1
        if (VARIANTCD.eq.'IE'.or.VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'CI'.or.&
             &VARIANTCD.eq.'BC') then
           P = PROB(I)
           D = DBH(I)
           DM1 = 0.
           IDM1 = 0
           call CCFCAL(ISP(I),D,DM1,IDM1,P,.false.,CCFT,CW,1)
           TEMCCF = TEMCCF + CCFT
        end if
        if (VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP') then

           ! CHECK TO SEE IF TREE MEETS DIAMETER LOWER LIMITS
           ! TREES WITH MEASURED HEIGHTS NEED TO MEET HEIGHT LOWER LIMITS FOR CALL TO PREPARE
           ! IHFLAG = 1 IF TREE MEETS MEASURED HEIGHT REQUIREMENTS OR THE HEIGHT IS MISSING

           IHFLAG = 0
           if ((HT(I).eq.0.0) .or. (HT(I).gt.4.5)) then
              IHFLAG = 1
           end if
           if (DBH(I) .ge.0.1.and. IHFLAG .eq.1) then
              if (VARIANTCD.eq.'OC') then

                 ! CHECK TO SEE IF THIS IS A BIG6 SPECIES

                 select case (ISP(I))
                 case(2,4,7,16,18)
                    NBIG6 = NBIG6 + 1
                 end select

                 ! SET VALID SPECIES FLAG, IORG(I).

                 select case (ISP(I))
                 case(2,3,4,7,8,16,18,24,27,30,31,34,36,37,38,39,42,46)
                    IORG(I) = 1
                    NVALID = NVALID + 1
                 case default
                    IORG(I) = 0
                 end select
              end if
              if (VARIANTCD.eq.'OP') then

                 ! CHECK TO SEE IF THIS IS A BIG6 SPECIES

                 select case (ISP(I))
                 case(3,16)
                    NBIG6 = NBIG6 + 1
                 end select

                 ! SET VALID SPECIES FLAG, IORG(I).

                 select case (ISP(I))
                 case(3,16,18,19,21,22,23,28,33,34,37)
                    IORG(I) = 1
                    NVALID = NVALID + 1
                 case default
                    IORG(I) = 0
                 end select
              end if
           else
              IORG(I) = 0
           end if
        end if
     end do
  end if
  if (VARIANTCD.eq.'IE'.or.VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'CI'.or.&
       &VARIANTCD.eq.'BC') then
     if (TEMCCF .lt.125.)TEMCCF = 125.
  end if
  if (VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP') then
     if (DEBUG)write(JOSTND,*)' IN CRATET IREC1,NVALID,NBIG6= ',&
          &IREC1,NVALID,NBIG6

     ! IF THERE ARE NO "BIG 6" TREES ORGANON WON'T RUN. FLAG ALL TREES AS
     ! NON-VALID ORGANON TREES AND SKIP CALL TO PREPARE.
     ! ALL TREES WILL GO INTO FVS DUBBING AND CALIBRATION LOGIC

     ! NOTE: EVEN TREES THAT GET DUBBED VALUES IN ORGANON WILL GO INTO THE
     ! FVS DUBBING AND CALIBATION LOGIC. ORGANON DUBBED VALUES WON'T GET
     ! OVERWRITTEN AND INCLUDING THOSE VALUES IN FVS CALIBRATION SHOULD BRING
     ! FVS AND ORGANON EQUATIONS MORE IN SYNC

     if (NBIG6.eq.0) then
        do I = 1,ITRN
           IORG(I) = 0
        end do
        go to 93
     end if

     !  FOR ALL LIVE TREE RECORDS, MOVE THE DATA FROM THE EXISTING FVS TREE
     !  ARRAYS INTO THE FORMAT THAT ORGANON REQUIRES.
     !  ASSIGNMENT OF FVS-VARIABLES TO ORGANON-VARIABLES
     !  ORGANON VARIABLE = FVS VARIABLE

     NLOAD = 0
     do I = 1, IREC1
        TREENO(I) = I ! FVS TREE NUMBER
        PTNO(I) = ITRE(I) ! POINT NUMBER OF THE TREE RECORD
        DBH1(I) = DBH(I) ! DBH
        if (DBH1(I) .lt.0.1) DBH1(I) = 0.1
        HT1OR(I) = HT(I) ! TOTAL HEIGHT AT BEGINNING OF PERIOD
        if (HT(I).gt.0..and. HT1OR(I).lt.4.6) HT1OR(I) = 4.6
        CR1(I) = real(ICR(I)) / 100.0 ! MEASURED/PREDICTED CROWN RATIO
        EXPAN1(I) = PROB(I) * PI ! POINT-LEVEL EXPANSION FACTOR
        RADGRO(I) = DG(I)/2.0 ! INSIDE BARK RADIAL GROWTH
        USER(I) = KUTKOD(I) ! USER CODE AT BEGINNING OF PERIOD
        NLOAD = NLOAD + 1
        ITEM(NLOAD) = I

        ! SET FIA SPECIES CODE (ACTUAL FOR VALID ORGANON SPECIES, SURROGATE
        ! FOR NON-VALID SPECIES.

        call ORGSPC(ISP(I),ORGFIA)
        SPECIES(I) = ORGFIA

        if (DEBUG)write(JOSTND,*)&
             &' I,PTNO,SPECIES,DBH1,HT1OR,CR1,EXPAN1,USER,NLOAD,ITEM= ',&
             &I,PTNO(I),SPECIES(I),DBH1(I),HT1OR(I),CR1(I),EXPAN1(I),&
             &USER(I),NLOAD,ITEM(NLOAD)
     end do
     if (DEBUG)write(JOSTND,*)' NVALID,NLOAD= ',NVALID,NLOAD

     !  TOTAL STAND AGE (EVEN AGED ONLY)

     IEVEN = INDS(4)
     if (INDS(4) .eq.1 ) then
        STAGE = IAGE+IY(ICYC)-IY(1)
        BHAGE = STAGE - 6 ! BREAST HEIGHT AGE
     else
        STAGE = 0
        BHAGE = 0 ! BREAST HEIGHT AGE
     end if
     if (DEBUG)write(JOSTND,*)' STAGE,BHAGE= ',STAGE,BHAGE

     !  CALL PREPARE

     call PREPARE(IMODTY, IPTINV, IREC1, STAGE, BHAGE, SPECIES,&
          &USER, IEVEN, DBH1, HT1OR, CR1, EXPAN1,&
          &RADGRO, RVARS, SERROR, TERROR,&
          &SWARNING, TWARNING, IERROR, IRAD,&
          &GROWTH, TMPCAL )
     LORGPREP = .true.

     !  IF DEBUGGING, REPORT ANY ERRORS TO THE OUTPUT FILE.

     if ( DEBUG .and. IERROR .ne.0 ) then
        write(JOSTND,9125) ICYC, IERROR
        do I = 1, 9
           if ( SWARNING(I) .ne.0 ) then
              write(JOSTND,9127) ICYC, SWARNING(I)
           end if
        end do

        do I = 1, 35
           if ( SERROR(I) .ne.0 ) then

              !  IGNORE THE FOLLOWING ERRORS:
              !     6 -- BHAGE HAS BEEN SET TO 0 FOR AN UNEVEN-AGED STAND
              !     7 -- BHAGE > 0 FOR AN UNEVEN-AGED STAND
              !     8 -- STAGE IS TOO SMALL FOR THE BHAGE
              !     9 --
              !    11 --

              if ( ( I .eq.6  ) .or.&
                   &( I .eq.7  ) .or.&
                   &( I .eq.8  ) .or.&
                   &( I .eq.9  ) .or.&
                   &( I .eq.11 ) ) then
                 write(JOSTND,9136) ICYC, I, SERROR(I)
                 IERROR = 0
                 SERROR(I) = 0
              else

                 write(JOSTND,9126) ICYC, I, SERROR(I)
              end if
           end if
        end do

        do I = 1, 2000
           if ( TWARNING(I) .ne.0 ) then
              write(JOSTND,9128) ICYC, TWARNING(I)
           end if

           do J = 1, 6
              if ( TERROR(I,J) .ne.0 ) then
                 write(JOSTND,9129) ICYC, TERROR(I,J), I, J
              end if
           end do
        end do
     else
        if (DEBUG)write(JOSTND,9125) ICYC, IERROR
     end if

     !  END OF THE CRATET ERROR REPORTING SECTION

     !  NOW RELOAD THE FVS ARRAYS WITH DBH, HT, AND CR FOR ALL VALID
     !  ORGANON TREES
     !  COUNT THE NUMBER OF RECORDS WITH MISSING CR OR HT FOR VALID ORGANON TREES

     NUNLOAD = 0
     KG = 0
     do I = 1, IREC1
        if (IORG(I) .eq.1) then
           NUNLOAD = NUNLOAD + 1
           KG = ITEM(I)
           DBH(I) = DBH1(KG)
           if (HT(I).le.0.0.and. HT1OR(KG).gt.0.0)&
                &KNTOHT(ISP(I)) = KNTOHT(ISP(I))+1
           HT(I) = HT1OR(KG)
           if (ICR(I).le.0.and. CR1(KG).gt.0.0)&
                &KNTOCR(ISP(I)) = KNTOCR(ISP(I))+1
           ICR(I) = NINT( CR1(KG) * 100.0 )
           if (DEBUG)write(JOSTND,*)&
                &' KG,I,ITRE,ISP,DBH,HT,ICR,PROB,ISPECL,NUNLOAD,KNTOHT,',&
                &'KNTOCR= ',KG,I,ITRE(I),ISP(I),DBH(I),HT(I),ICR(I),PROB(I),&
                &ISPECL(I),NUNLOAD,KNTOHT(ISP(I)),KNTOCR(ISP(I))
        end if
     end do
     if (DEBUG)write(JOSTND,*)' NVALID,NUNLOAD= ',NVALID,NUNLOAD
  end if
  if (VARIANTCD.eq.'IE'.or.VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'CI'.or.&
       &VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'BC') then

     if (DEBUG) then
        if (VARIANTCD.eq.'IE'.or.VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'CI'.or.&
             &VARIANTCD.eq.'BC') then
           write(JOSTND,*)'SITEAR BEFORE = '
           write(JOSTND,8998) (NSP(J,1)(1:2),SITEAR(J),J = 1,MAXSP)
        end if
        if (VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP') then
           do I = 1,18
              write(JOSTND,9220) I, ACALIB(1,I),&
                   &I, ACALIB(2,I),&
                   &I, ACALIB(3,I)
              write(JOSTND,9221) I, TMPCAL(1,I),&
                   &I, TMPCAL(2,I),&
                   &I, TMPCAL(3,I), I, NEWCAL(I)
           end do
        end if
     end if
  end if
  if (VARIANTCD.eq.'IE'.or.VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'CI'.or.&
       &VARIANTCD.eq.'BC') then
     do ISPC = 1,MAXSP
        SI = SITEAR(ISPC)
        H = 0.
        if (VARIANTCD.eq.'IE') then
           select case (ISPC)

           ! SPECIES USING ALEXANDER, TACKLE, & DAHMS 1967, RES NOTE RM-29

           case(13,17)
              H = 9.89311 - 0.19177*(50.0) + 0.00124*(50.0**2)&
                   &- 0.00082*(TEMCCF-125.)*SI + 0.01387*(50.0)*SI&
                   &- 0.0000455*(50.0**2)*SI
              SITEAR(ISPC) = H
           end select
        end if
        if (VARIANTCD.eq.'TT') then
           select case (ISPC)

           ! SPECIES USING ALEXANDER, TACKLE, & DAHMS 1967, RES NOTE RM-29

           case(1,2,7,17)
              H = 9.89311 - 0.19177*(50.0) + 0.00124*(50.0**2)&
                   &- 0.00082*(TEMCCF-125.)*SI + 0.01387*(50.0)*SI&
                   &- 0.0000455*(50.0**2)*SI
              SITEAR(ISPC) = H

           ! SPECIES USING ALEXANDER 1967, RES NOTE RM-32

           case(4,5,8,9)
              if (ISPC.ne.4.or.VARACD.eq.'UT') then
                 H = 4.5 + (2.75780*(SI**0.83312)) * (1.0 - EXP(-0.015701*50.))**&
                      &(22.71944*(SI**(-0.63557)))
                 SITEAR(ISPC) = H
              end if

           ! UT PONDEROSA PINE: MEYER 1961 REV., TECH BULL 630.

           case(10)
           end select
        end if
        if (VARIANTCD.eq.'UT') then
           select case (ISPC)

           ! SPECIES USING ALEXANDER, TACKLE, & DAHMS 1967, RES NOTE RM-29

           case(1,2,7,23)
              H = 9.89311 - 0.19177*(50.0) + 0.00124*(50.0**2)&
                   &- 0.00082*(TEMCCF-125.)*SI + 0.01387*(50.0)*SI&
                   &- 0.0000455*(50.0**2)*SI
              SITEAR(ISPC) = H

           ! SPECIES USING ALEXANDER 1967, RES NOTE RM-32

           case(4,5,8,9)
              if (ISPC.ne.4.or.VARACD.eq.'UT') then
                 H = 4.5 + (2.75780*(SI**0.83312)) * (1.0 - EXP(-0.015701*50.))**&
                      &(22.71944*(SI**(-0.63557)))
                 SITEAR(ISPC) = H
              end if

           ! UT PONDEROSA PINE: MEYER 1961 REV., TECH BULL 630.

           case(10)
              H = (3.635794*(SI**0.916307)) / (1.0 + EXP(6.09478 -&
                   &(0.96483*ALOG(50.)) - (0.277025*ALOG(SI))))
              SITEAR(ISPC) = H
           end select
        end if
        if (VARIANTCD.eq.'CI') then
           select case (ISPC)

           ! SPECIES USING ALEXANDER, TACKLE, & DAHMS 1967, RES NOTE RM-29

           case(11,12,16)
              H = 9.89311 - 0.19177*(50.0) + 0.00124*(50.0**2)&
                   &- 0.00082*(TEMCCF-125.)*SI + 0.01387*(50.0)*SI&
                   &- 0.0000455*(50.0**2)*SI
              SITEAR(ISPC) = H
           end select
        end if
        if (VARIANTCD.eq.'IE'.or.VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'CI') then
           if (DEBUG)write(16,*)'ISPC,SI,TEMCCF,H= ',ISPC,SI,TEMCCF,H
        end if
        if (VARIANTCD.eq.'BC') then
           select case (ISPC)

           ! SPECIES USING ALEXANDER, TACKLE, & DAHMS 1967, RES NOTE RM-29

           case(10)
              H = 9.89311 - 0.19177*(50.0) + 0.00124*(50.0**2)&
                   &- 0.00082*(TEMCCF-125.)*SI + 0.01387*(50.0)*SI&
                   &- 0.0000455*(50.0**2)*SI
              SITEAR(ISPC) = H
           end select
           if (DEBUG)write(JOSTND,*)'ISPC,SI,TEMCCF,H= ',ISPC,SI,TEMCCF,H
        end if
     end do
     if (DEBUG) then
        write(JOSTND,*)'SITEAR AFTER = '
        write(JOSTND,8998) (NSP(J,1)(1:2),SITEAR(J),J = 1,MAXSP)
     end if
  end if
  if (VARIANTCD.eq.'SN') then

     !  WRITE SIDMAX, BY SPECIES, TO OUTPUT

     do I = 1,15
        J = (I-1)*10 + 1
        JJ = J+9
        if (JJ.gt.MAXSP)JJ = MAXSP
        write(JOSTND,90)(NSP(K,1)(1:2),K = J,JJ)
        write(JOSTND,91)(SDIDEF(K),K = J,JJ )
        if (JJ .eq. MAXSP)go to 93
        92 continue
     end do
  end if
  if (VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP') then

     !  LOAD ACALIB(I,J) WITH ANY CALIBRATION VALUES COMPUTED IN **PREPARE**
     !  WHICH WERE NOT LOADED BY KEYWORD ENTRY

     do I = 1,18
        do J = 1,3
           if ((ACALIB(J,I) .eq.1.0) .and.&
                &((TMPCAL(J,I) .gt.0.0) .and. (TMPCAL(J,I) .ne.1.0))) then
              ACALIB(J,I) = TMPCAL(J,I)
              NEWCAL(I) = 1
           end if
        end do
     end do
  end if
  if (VARIANTCD.eq.'SN'.or.VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP') then
     93 continue
  end if
  if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
       &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
       &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
       &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
       &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.&
       &VARIANTCD.eq.'BC'.or.VARIANTCD.eq.'ON') then

     !  IF THERE ARE TREE RECORDS, BRANCH TO PREFORM CALIBRATION.

     AX = 0.
  end if
  if (ITRN.gt.0) go to 1
  if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
       &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
       &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
       &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
       &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'AK'.or.&
       &VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'ON') then

     !   CALL MAICAL TO CALCULATE MAI

     call MAICAL
  end if
  call RCON
  ONTREM(7) = 0.
  call DENSE
  call DGDRIV
  call REGENT(.false.,1)
  1 continue
  do I = 1,MAXSP
     if (VARIANTCD.eq.'KT') then
        ISPFL(I) = 0
     end if
     SPCNT(I,1) = 0.0
     SPCNT(I,2) = 0.0
     SPCNT(I,3) = 0.0
     if (ISCT(I,1).eq.0) go to 5
     J = IREF(I)
     IUSED(J) = NSP(I,1)
     5 continue
  end do
  if ((ITRN.le.0).and.(IREC2.ge.MAXTP1))go to 245
  if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
       &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
       &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
       &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
       &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'AK'.or.&
       &VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP') then

     !  PRINT SPECIES LABELS AND NUMBER OF OBSERVATIONS IN CONTROL
     !  TABLE.  THEN, RESET COUNTERS TO ZERO.

     write(JOSTND,'(//''CALIBRATION STATISTICS:''//)')
  end if
  if (VARIANTCD.eq.'BC'.or.VARIANTCD.eq.'ON') then

     !  PRINT SPECIES LABELS AND NUMBER OF OBSERVATIONS IN CONTROL
     !  TABLE.  THEN, RESET COUNTERS TO ZERO.

     write(JOSTND,'(//'' CALIBRATION STATISTICS:''//)')
  end if
  if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
       &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
       &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
       &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
       &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'AK'.or.&
       &VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'BC') then
     write(JOSTND,9000) (IUSED(I), I = 1,NUMSP)
  end if
  if (VARIANTCD.eq.'ON') then
     write(JOSTND,10020) (IUSED(I), I = 1,NUMSP)
  end if
  if (NUMSP .le.11) then
     if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
          &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
          &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
          &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
          &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'OC'.or.&
          &VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'BC') then
        write(JOSTND,9001) (UNDER, I = 1,NUMSP)
     end if
     if (VARIANTCD.eq.'KT') then
        write(JOSTND,10006) (UNDER, I = 1,NUMSP)
     end if
     if (VARIANTCD.eq.'ON') then
        write(JOSTND,10021) (UNDER, I = 1,NUMSP)
     end if
  else
     if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
          &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
          &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
          &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
          &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'OC'.or.&
          &VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'BC') then
        write(JOSTND,9001) (UNDER, I = 1,11)
     end if
     if (VARIANTCD.eq.'KT') then
        write(JOSTND,10006) (UNDER, I = 1,11)
     end if
     if (VARIANTCD.eq.'ON') then
        write(JOSTND,10021) (UNDER, I = 1,11)
     end if
  end if
  if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
       &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
       &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
       &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
       &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'AK'.or.&
       &VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP') then
     write(JOSTND,9002) (KOUNT(I), I = 1,NUMSP)
  end if
  if (VARIANTCD.eq.'BC') then
     write(JOSTND,10009) (KOUNT(I), I = 1,NUMSP)
  end if
  if (VARIANTCD.eq.'ON') then
     write(JOSTND,10022) (KOUNT(I), I = 1,NUMSP)
  end if
  do I = 1,MAXSP
     KNT(I) = 0
     KNT2(I) = 0
     10 continue
  end do

  !   CALL MBACAL TO IDENTIFY SITE SPECIES

  call MBACAL
  if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
       &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
       &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
       &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
       &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'AK'.or.&
       &VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'ON') then

     !   CALL MAICAL TO CALCULATE MAI

     call MAICAL
  end if

  !  CALL **RCON** TO INITIALIZE SITE DEPENDENT MODEL COEFFICIENTS.

  call RCON

  !  CALL **RDPSRT** AND **DENSE** TO COMPUTE INITIAL STAND DENSITY
  !  STATISTICS.  ONTREM(7) IS SET TO ZERO HERE TO ASSURE THAT RELDM1
  !  WILL BE ASSIGNED IN **DENSE** IN THE FIRST CYCLE.

  do I = 1,ITRN
     IND(I) = IND1(I)
     15 continue
  end do
  call RDPSRT(ITRN,DBH,IND,.false.)
  ONTREM(7) = 0.0
  if (VARIANTCD.eq.'KT') then
     do I = 1,MAXTRE
        HTKEEP(I) = 0.
        20 continue
     end do
     call SMHTRG(ISPFL,DEBUG,HTKEEP)
  end if

  !  PREPARE INPUT DATA FOR DIAMETER GROWTH MODEL CALIBRATION.  IF
  !  IDG IS 1, CONVERT THE PAST DIAMETER MEASUREMENT CARRIED IN DG TO
  !  DIAMETER GROWTH.  IF IDG IS 3, CONVERT THE CURRENT DIAMETER
  !  MEASUREMENT CARRIED IN DG TO DIAMETER GROWTH.  ACTUAL DIAMETER
  !  INCREMENT MEASUREMENTS WILL BE CORRECTED FOR BARK GROWTH IN
  !  THE CALIBRATION ROUTINE. (THIS CODE WAS MOVED HERE SO THAT THE
  !  BACKDATING ALGORITHM IN **DENSE**, INVOKED DURING CALIBRATION,
  !  IS CORRECT.)

  Q = 1.0
  if (IDG.eq.3) Q = -1.0
  do II = 1,ITRN
     I = IND1(II)
     if (I.ge.IREC2) go to 230
     if (DG(I).le.0.0) go to 220
     if (IDG.eq.0.or.IDG.eq.2) go to 230
     DG(I) = Q*(DBH(I)-DG(I))
     go to 230
     220 continue
     DG(I) = -1.0
     230 continue
  end do
  if (VARIANTCD.eq.'AK') then

     !  CALL SDICAL TO LOAD THE POINT MAX SDI WEIGHTED BY SPECIES ARRAY XMAXPT()
     !  RECENT DEAD ARE NOT INCLUDED. IF THEY NEED TO BE, SDICAL.F NEEDS MODIFIED
     !  TO DO SO. IWHO, PARAMETER 1, HAS NO AFFECT ON XMAXPT ARRAY VALUES.

     call SDICAL (0, XMAX)
  end if

  !  SET LBKDEN TRUE IF DIAMETERS ARE TO BE BACKDATED FOR DENSITY
  !  CALCULATIONS.  AFTER THIS CALL TO DENSE, INSURE LBKDEN=FALSE.

  LBKDEN = IDG.lt.2
  call DENSE
  LBKDEN = .false.

  !  DELETE NON-PROJECTABLE RECORDS, AND REALIGN POINTERS TO THE
  !  SPECIES ORDERED SORT.

  if (IREC2.eq.MAXTP1) go to 60
  do I = IREC2,MAXTRE
     ISPC = ISP(I)
     IPTR = IREF(ISPC)
     if (IMC(I).eq.7)KNT(IPTR) = KNT(IPTR)+1
     if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
          &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
          &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
          &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
          &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'AK'.or.&
          &VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP') then
        if (DEBUG) write(JOSTND,9003) I,IMC(I),ISPC
     end if
     if (VARIANTCD.eq.'BC'.or.VARIANTCD.eq.'ON') then
        if (DEBUG) write(JOSTND,10010) I,IMC(I),ISPC
     end if
     if (ITRN.gt.0) then
        I1 = ISCT(ISPC,1)
        I2 = ISCT(ISPC,2)
        do I3 = I1,I2
           if (IND1(I3).eq.I) go to 40
           30 continue
        end do
        40 continue
        IND1(I3) = IND1(I2)
        ISCT(ISPC,2) = I2-1
        if (ISCT(ISPC,2).ge.ISCT(ISPC,1)) go to 50
        ISCT(ISPC,1) = 0
        ISCT(ISPC,2) = 0
     end if
     50 continue
  end do

  !  WRITE CALIBRATION TABLE ENTRY FOR NON-PROJECTABLE RECORDS AND RESET
  !  KNT ARRAY TO ZERO.

  if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
       &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
       &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
       &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
       &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'AK'.or.&
       &VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP') then
     write(JOSTND,9004) (KNT(I),I = 1,NUMSP)
  end if
  if (VARIANTCD.eq.'BC') then
     write(JOSTND,10011) (KNT(I),I = 1,NUMSP)
  end if
  if (VARIANTCD.eq.'ON') then
     write(JOSTND,10023) (KNT(I),I = 1,NUMSP)
  end if

  !  RESET TREE RECORD COUNTERS AND SAVE THE NUMBER OF SPECIES.

  ITRN = IREC1
  if ((ITRN.le.0).and.(IREC2.ge.MAXTP1))go to 245
  if (ITRN.le.0)go to 60
  ISPC = NUMSP

  !  MAKE SURE THAT ALL THE SPECIES ORDER SORTS AND THE IND2 ARRAY
  !  ARE IN THE PROPER ORDER. FIRST, SAVE THE SPECIES REFERENCES.

  do I = 1,MAXSP
     KNT(I) = IREF(I)
     51 continue
  end do
  call SPESRT

  !  IF THE NUMBER OF SPECIES HAS CHANGED, WE MUST REWRITE THE
  !  COLUMN HEADINGS.

  if (ISPC.ne.NUMSP) then
     if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
          &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
          &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
          &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
          &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'AK'.or.&
          &VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP') then
        write(JOSTND,52)
     end if
     if (VARIANTCD.eq.'BC'.or.VARIANTCD.eq.'ON') then
        write(JOSTND,10012)
     end if
     do I = 1,MAXSP
        if (ISCT(I,1).eq.0) go to 55
        J = IREF(I)
        IUSED(J) = NSP(I,1)
        55 continue
     end do
     if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
          &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
          &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
          &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
          &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'AK'.or.&
          &VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'BC') then
        write(JOSTND,9000) (IUSED(I), I = 1, NUMSP)
     end if
     if (VARIANTCD.eq.'ON') then
        write(JOSTND,10020) (IUSED(I), I = 1, NUMSP)
     end if
     if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
          &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
          &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
          &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
          &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'OC'.or.&
          &VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'BC') then
        write(JOSTND,9001) (UNDER, I = 1, NUMSP)
     end if
     if (VARIANTCD.eq.'KT') then
        write(JOSTND,10006) (UNDER, I = 1, NUMSP)
     end if
     if (VARIANTCD.eq.'ON') then
        write(JOSTND,10021) (UNDER, I = 1, NUMSP)
     end if
  else

     !        RESET THE REFERENCES.

     do I = 1,MAXSP
        IREF(I) = KNT(I)
        57 continue
     end do
  end if

  !  SORT REMAINING TREE RECORDS IN ORDER OF DESCENDING DIAMETER.
  !  STORE POINTERS TO SORTED ORDER IN IND.

  call RDPSRT(ITRN,DBH,IND,.true.)
  60 continue
  do I = 1,MAXSP
     KNT(I) = 0
     65 continue
  end do

  !  ENTER LOOP TO ADJUST HEIGHT-DBH MODEL FOR LOCAL CONDITIONS.  IF
  !  THERE ARE 3 OR MORE TREES WITH MEASURED HEIGHTS FOR A GIVEN
  !  SPECIES, ADJUST THE INTERCEPT (ASYMPTOTE) IN THE MODEL SO THAT
  !  THE MEAN RESIDUAL FOR THE MEASURED TREES IS ZERO.
  !  IF LHTDRG IS FALSE FOR A GIVEN SPECIES THEN ALL DUBBING IS DONE WITH
  !  DEFAULT VALUES.

  do ISPC = 1,MAXSP
     if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
          &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
          &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
          &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
          &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.&
          &VARIANTCD.eq.'BC'.or.VARIANTCD.eq.'ON') then
        AA(ISPC) = 0.
        BB(ISPC) = 0.
     end if
     I1 = ISCT(ISPC,1)
     if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
          &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
          &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
          &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
          &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.&
          &VARIANTCD.eq.'BC'.or.VARIANTCD.eq.'ON') then
        if (I1.le.0) go to 141
     end if
     if (VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT') then
        if (I1.le.0) then
           AAHT = HT1(ISPC)
           BBHT = HT2(ISPC)
           go to 141
        end if
     end if
     I2 = ISCT(ISPC,2)
     IPTR = IREF(ISPC)
     if (VARIANTCD.eq.'WS') then
        select case (ISPC)
        case(9:10,12,14:17,19:20,25:27)
           SIZE = 5.0
        case default
           SIZE = 3.0
        end select
     end if
     if (VARIANTCD.eq.'KT') then

        !  INITIALIZE SUMS FOR THIS SPECIES.

        COUNTER = 0.0
     end if

     !  INITIALIZE SUMS FOR THIS SPECIES.

     K1 = 0
     K2 = 0
     K3 = 0
     K4 = 0
     if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
          &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
          &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
          &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
          &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'OC'.or.&
          &VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'BC'.or.VARIANTCD.eq.'ON') then
        SUMX = 0.0
     end if
     if (VARIANTCD.eq.'EC') then

        select case (ISPC)

        !  SPECIES USING WC EQUATIONS/LOGIC

        case(11,13:15,17:30,32)
           BKPT = 5.0

        !  SPECIES USING EC EQUATIONS/LOGIC

        case default
           BKPT = 3.0

        end select
     end if
     if (VARIANTCD.eq.'WS') then

        !  DETERMINE SITE CLASS

        R5SI = 0.0
        if (SITEAR(ISPC).ne.0.0)R5SI = SITEAR(ISPC)
        if (R5SI.le.45.0)SC = 5.0
        if (R5SI.gt.45.0.and. R5SI.le.55.0)SC = 4.0
        if (R5SI.gt.55.0.and. R5SI.le.70.0)SC = 3.0
        if (R5SI.gt.70.0.and. R5SI.le.85.0)SC = 2.0
        if (R5SI.gt.85.0.and. R5SI.le.100.0)SC = 1.0
        if (R5SI.gt.100.0)SC = 0.0
     end if
     if (VARIANTCD.eq.'KT') then
        SUMXX = 0.0
        SUMYY = 0.0
        SUMXY = 0.0
        SUMX2 = 0.0
     end if

     !  ENTER TREE LOOP WITHIN SPECIES.

     do I3 = I1,I2
        I = IND1(I3)
        H = HT(I)
        NH = NORMHT(I)
        D = DBH(I)
        if (VARIANTCD.eq.'BC') then

           if (LMHTDUB(ISPC)) then
              HM = H * FTtoM
              DM = D * INtoCM
           end if
        end if
        if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
             &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
             &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
             &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.VARIANTCD.eq.'CR'.or.&
             &VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'BC'.or.&
             &VARIANTCD.eq.'ON') then
           BX = HT2(ISPC)
        end if
        if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
             &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
             &VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'OC'.or.&
             &VARIANTCD.eq.'BC'.or.VARIANTCD.eq.'ON') then

           !  BYPASS SUMS FOR TREES WITH MISSING HEIGHT OR TRUNCATED TOPS.

           if (H.le.4.5.or. NH.lt.0.or. D.lt.3.0) go to 70
        end if
        if (VARIANTCD.eq.'TT') then
           select case(ISPC)
           case (13,16)
              if (H.le.4.5.or. NH.lt.0.or. D.lt.5.0) go to 70
           case default
              if (H.le.4.5.or. NH.lt.0.or. D.lt.3.0) go to 70
           end select
        end if
        if (VARIANTCD.eq.'UT') then
           select case(ISPC)
           case (21,20)
              if (H.le.4.5.or. NH.lt.0.or. D.lt.5.0) go to 70
           case default
              if (H.le.4.5.or. NH.lt.0.or. D.lt.3.0) go to 70
           end select
        end if
        if (VARIANTCD.eq.'BM') then

           ! FOR SPECIES FROM THE WC VARIANT, THE HT1 AND HT2 COEFFICIENTS ONLY
           ! APPLY TO TREES GREATER THAN 5.0 INCHES.

           if ((ISPC.eq.13.or. ISPC.eq.14.or. ISPC.eq.16.or. ISPC.eq.18)&
                &.and. D.lt.5.0)go to 70
        end if
        if (VARIANTCD.eq.'WC'.or.VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'OP') then

           !  BYPASS SUMS FOR TREES WITH MISSING HEIGHT OR TRUNCATED TOPS.
           !  SEPERATE EQUATIONS ARE USED FOR TREES 5" AND LARGER VS TREES
           !  LESS THAN 5" DBH. CALIBRATION OF THE HT-DBH RELATIONSHIP IS
           !  ONLY DONE ON THE EQUATION USED FOR TREES 5" AND LARGER.

           if (H.le.4.5.or. NH.lt.0.or. D.lt.5.0) go to 70
        end if
        if (VARIANTCD.eq.'SO') then
           if (ICR(I).le.0) then
              JCR = 5
           else
              JCR = ((ICR(I)-1)/10)+1
              if (JCR.gt.9)JCR = 9
              if (JCR.le.0)JCR = 5
           end if
           BX = HTT2(ISPC,JCR)

           !  BYPASS SUMS FOR TREES WITH MISSING HEIGHT OR TRUNCATED TOPS.
           !  FOR SPECIES USING LOGIC FROM THE WC VARIANT -
           !  SEPERATE EQUATIONS ARE USED FOR TREES 5" AND LARGER VS TREES
           !  LESS THAN 5" DBH. CALIBRATION OF THE HT-DBH RELATIONSHIP IS
           !  ONLY DONE ON THE EQUATION USED FOR TREES 5" AND LARGER.

           select case(ISPC)
           case (15,19:23,25,26,28:31,33)
              if (H.le.4.5.or. NH.lt.0.or. D.lt.5.0) go to 70
           case default
              if (H.le.4.5.or. NH.lt.0.or. D.lt.3.0) go to 70
           end select
        end if
        if (VARIANTCD.eq.'EC') then

           !  BYPASS SUMS FOR TREES WITH MISSING HEIGHT OR TRUNCATED TOPS.

           !  FOR SPECIES USING EQUATIONS/LOGIC FROM THE WC VARIANT:
           !  SEPERATE EQUATIONS ARE USED FOR TREES 5" AND LARGER VS TREES
           !  LESS THAN 5" DBH. CALIBRATION OF THE HT-DBH RELATIONSHIP IS
           !  ONLY DONE ON THE EQUATION USED FOR TREES 5" AND LARGER.

           if (H.le.4.5.or. NH.lt.0.or. D.lt.BKPT) go to 70
        end if
        if (VARIANTCD.eq.'WS') then

           !  BYPASS SUMS FOR TREES WITH MISSING HEIGHT OR TRUNCATED TOPS.

           if (H.le.4.5.or. NH.lt.0.or. D.lt.SIZE) go to 70
        end if
        if (VARIANTCD.eq.'CI') then
           BBHT = HT2(ISPC)

           !  BYPASS SUMS FOR TREES WITH MISSING HEIGHT OR TRUNCATED TOPS.

           select case(ISPC)
           case (15)
              if (H.le.4.5.or. NH.lt.0.or. D.lt.5.0) go to 70
           case default
              if (H.le.4.5.or. NH.lt.0.or. D.lt.3.0) go to 70
           end select
        end if
        K1 = K1+1
        if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
             &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
             &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
             &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
             &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.&
             &VARIANTCD.eq.'ON') then
           XX = BX/(D+1.)
        end if
        if (VARIANTCD.eq.'CI') then
           XX = BBHT / (D + 1.)
        end if
        if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
             &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
             &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
             &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
             &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'OC'.or.&
             &VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'ON') then
           YY = ALOG(H-4.5)
        end if
        if (VARIANTCD.eq.'BC') then

           if (LMHTDUB(ISPC)) then
              YY = ALOG(HM - 1.3)
              XX = BX / (DM + 1.)
           else
              YY = ALOG(H  - 4.5)
              XX = BX / (D + 1.)
           end if
        end if
        if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
             &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
             &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
             &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
             &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'OC'.or.&
             &VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'BC'.or.VARIANTCD.eq.'ON') then
           SUMX = SUMX+YY-XX
        end if
        if (VARIANTCD.eq.'KT') then
           XX = SQRT(D)
           YY = H
           XY = XX*YY
           X2 = D
           SUMYY = SUMYY + YY
           SUMXX = SUMXX + XX
           SUMXY = SUMXY + XY
           SUMX2 = SUMX2 + X2
           if (DEBUG)write(JOSTND,*)'I=',I,' ISPC=',ISPC,' D=',D,' H=',H,&
                &' SQRTD=',SQRT(D),' SUMYY=',SUMYY,' SUMXX=',SUMXX,' SUMXY='&
                &,SUMXY,' SUMX2=',SUMX2
        end if
        go to 80

        !  COUNT NUMBER OF MISSING HEIGHTS AND BROKEN OR DEAD TOPS.  LOAD THE
        !  ARRAY IND2 WITH SUBSCRIPTS TO THE RECORDS WITH MISSING HEIGHTS.

        70 continue
        if (NH.lt.0) K3 = K3+1
        if (H.gt.0.0.and. NH.eq.0) go to 80
        K2 = K2+1
        IND2(K2) = I
        if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
             &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
             &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
             &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
             &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'OC'.or.&
             &VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'BC'.or.VARIANTCD.eq.'ON') then

           !  END OF SUMMATION LOOP FOR THIS SPECIES.

           !*** ONE LINE FIX FOR GROWTH METHOD 1 PROBLEM. DIXON 11-16-90.
           if (HT(I) .le.0.1) HTG(I) = 0.0
        end if
        80 continue
     end do

     !  IF THERE ARE LESS THAN THREE OBSERVATIONS OR LHTDRG IS FALSE THEN
     !  DUB HEIGHTS USING DEFAULT COEFFICIENTS FOR THIS SPECIES.

     KNT(IPTR) = K3
     if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
          &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
          &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
          &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
          &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'OC'.or.&
          &VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'BC'.or.VARIANTCD.eq.'ON') then
        if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
             &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
             &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
             &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
             &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'OC'.or.&
             &VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'BC') then
           if (K1.lt.3.or..not. LHTDRG(ISPC)) go to 100
        end if
        if (VARIANTCD.eq.'ON') then
           if (K1.lt.3.or..not. LHTDRG(ISPC)) go to 110
        end if
     end if
     if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
          &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
          &VARIANTCD.eq.'WC'.or.VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'CR'.or.&
          &VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'BC'.or.VARIANTCD.eq.'ON') then
        XN = FLOAT(K1)
     end if
     if (VARIANTCD.eq.'AK') then
        if (K1 .lt. 3 .or. .not. LHTDRG(ISPC)) go to 100
     end if
     if (VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'SO'.or.&
          &VARIANTCD.eq.'WS'.or.VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP') then
        XN = real(K1)
     end if
     if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
          &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
          &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
          &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
          &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.&
          &VARIANTCD.eq.'BC'.or.VARIANTCD.eq.'ON') then
        AA(ISPC) = SUMX/XN
     end if
     if (VARIANTCD.eq.'WS') then
        if (DEBUG)write(JOSTND,*)'ISPC,AA,SUMX,XN= ',&
             &ISPC,AA(ISPC),SUMX,XN
     end if
     if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
          &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
          &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
          &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
          &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.&
          &VARIANTCD.eq.'BC'.or.VARIANTCD.eq.'ON') then

        !  IF THE INTERCEPT IS NEGATIVE, USE THE DEFAULT VALUES.

        if (AA(ISPC).ge.0.0) then
           IABFLG(ISPC) = 0
        end if
     end if
     if (VARIANTCD.eq.'CI') then
        AAHT = SUMX / XN

        !  IF THE INTERCEPT IS NEGATIVE, USE THE DEFAULT VALUES.

        IABF = 1
        if (AAHT.ge.0.0) then
           if (ISPC .eq.15)IABF = 0
           go to 110
        end if
     end if
     if (VARIANTCD.eq.'KT') then
        Z1 = XN*SUMXY-SUMXX*SUMYY
        Z2 = XN*SUMX2-SUMXX*SUMXX
        if (Z1.eq.0.0.or. Z2.eq.0.0) then
           BBHT = 0.0
        else
           BBHT = (XN*SUMXY-SUMXX*SUMYY)/(XN*SUMX2-SUMXX*SUMXX)
        end if
        Z1 = SUMX2*SUMYY-SUMXX*SUMXY
        Z2 = XN*SUMX2-SUMXX*SUMXX
        if (Z1.eq.0.0.or. Z2.eq.0.0) then
           AAHT = 0.0
        else
           AAHT = (SUMX2*SUMYY-SUMXX*SUMXY)/(XN*SUMX2-SUMXX*SUMXX)
        end if

        !  IF THE INTERCEPT IS NEGATIVE. USE THE DEFAULT VALUES.

        if (BBHT.ge.0.0) go to 110
     end if
     if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
          &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
          &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
          &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
          &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'AK'.or.&
          &VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'BC') then
        !  ORIGINAL EM,IE,BC NOTE:

        !  DUB IN MISSING HEIGHTS.
        !  A VALUE LESS THAN ZERO STORED IN NORMHT => THAT TREE WAS TOP KILLED.
        !  CONSEQUENTLY, A VALUE OF 80% OF THE PREDICTED HEIGHT IS STORED AS
        !  THE TRUNCATED HEIGHT.

        !  ORIGINAL CA,NC,CS,NE,LS,SN,TT,UT,BM,WC,PN,SO,EC,CR,AK,OC,OP NOTE:

        !  DUB IN MISSING HEIGHTS.
        !  A VALUE LESS THAN ZERO STORED IN 'HT' => THAT TREE WAS TOP KILLED.
        !  CONSEQUENTLY, A VALUE OF 80% OF THE PREDICTED HEIGHT IS STORED AS
        !  THE TRUNCATED HEIGHT.

        100 continue
     end if
     if (VARIANTCD.eq.'WS') then
        AHATR5 = A(IFOR)
        BHATR5 = B(IFOR)
        CHATR5 = C(IFOR)
        if (DEBUG) write(JOSTND,9114)AHATR5,BHATR5,CHATR5
     end if
     if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
          &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
          &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
          &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.VARIANTCD.eq.'CR'.or.&
          &VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'BC') then
        AX = HT1(ISPC)
        BX = HT2(ISPC)
        if (IABFLG(ISPC) .eq.0) AX = AA(ISPC)
     end if
     if (VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT') then
        AAHT = HT1(ISPC)
     end if
     if (VARIANTCD.eq.'KT') then
        BBHT = HT2(ISPC)
        COUNTER = 1.0
     end if
     if (VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'ON') then
        !  ORIGINAL CI,KT NOTE:

        !  DUB IN MISSING HEIGHTS.
        !  A VALUE LESS THAN ZERO STORED IN NORMHT => THAT TREE WAS TOP KILLED.
        !  CONSEQUENTLY, A VALUE OF 80% OF THE PREDICTED HEIGHT IS STORED AS
        !  THE TRUNCATED HEIGHT.

        !  ORIGINAL ON NOTE:

        !  DUB IN MISSING HEIGHTS.
        !  A VALUE LESS THAN ZERO STORED IN 'HT' => THAT TREE WAS TOP KILLED.
        !  CONSEQUENTLY, A VALUE OF 80% OF THE PREDICTED HEIGHT IS STORED AS
        !  THE TRUNCATED HEIGHT.

        110 continue
     end if
     if (K2.eq.0) go to 140
     do JJ = 1,K2
        II = IND2(JJ)
        if (VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'SO') then
           if (ICR(II).le.0) then
              if (VARIANTCD.eq.'BM') then
                 JCR = 4
              end if
              if (VARIANTCD.eq.'SO') then
                 JCR = 5
              end if
           else
              JCR = ((ICR(II)-1)/10)+1
              if (VARIANTCD.eq.'BM') then
                 if (JCR.gt.7)JCR = 7
                 if (JCR.le.0)JCR = 4
              end if
              if (VARIANTCD.eq.'SO') then
                 if (JCR.gt.9)JCR = 9
                 if (JCR.le.0)JCR = 5
              end if
           end if
        end if
        if (VARIANTCD.eq.'EC') then
           JCR = ((ICR(II)-1)/10)+1
        end if
        D = DBH(II)
        if (VARIANTCD.eq.'WC'.or.VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'OP') then
           JCR = ((ICR(II)-1)/10)+1
        end if
        if (VARIANTCD.eq.'SO') then

           !  LOGIC FROM SORNEC 11 (HTT1,HTT2 VARIABLES)

           select case (ISPC)
           case(1:8,10,12,13,32)
              AX = HTT1(ISPC,JCR)
              BX = HTT2(ISPC,JCR)
           case default
              AX = HT1(ISPC)
              BX = HT2(ISPC)
           end select
        end if
        if (VARIANTCD.eq.'ON') then
           AX = HT1(ISPC)
           BX = HT2(ISPC)
        end if
        if (VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'ON') then
           if (IABFLG(ISPC) .eq.0) AX = AA(ISPC)
        end if
        if (VARIANTCD.eq.'BC') then
           DM = DBH(II) * INtoCM
        end if
        if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
             &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
             &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
             &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
             &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'AK'.or.&
             &VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'BC') then
           TKILL = NORMHT(II) .lt.0
        end if
        if (VARIANTCD.eq.'SO') then
           if (DEBUG)write(JOSTND,10000) ISPC,AX,BX,IABFLG(ISPC)
        end if
        if (VARIANTCD.eq.'ON') then

           TKILL = NORMHT(II) .lt.0.
        end if

        if (D .le.0.1) then
           H = 1.01
           if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
                &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
                &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
                &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
                &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.&
                &VARIANTCD.eq.'BC'.or.VARIANTCD.eq.'ON') then
              if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
                   &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
                   &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
                   &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
                   &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI') then
                 go to 10031
              end if
              if (VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'BC'.or.VARIANTCD.eq.'ON') then
                 go to 10030
              end if
           end if
           if (VARIANTCD.eq.'KT') then
              go to 10031
           end if
           if (VARIANTCD.eq.'AK') then
              go to 105
           end if
        end if
        if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
             &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
             &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'OC') then

           H = EXP(AX+BX/(D+1.0))+4.5
        end if
        if (VARIANTCD.eq.'UT') then
           select case(ISPC)

           case (21,20)

              !  USE INVENTORY EQUATIONS IF CALIBRATION OF THE HT-DBH FUNCTION IS TURNED
              !  OFF, OR IF WYKOFF CALIBRATION DID NOT OCCUR.
              !  NOTE: THIS SIMPLIFIES TO IF(IABFLB(ISPC).EQ.1) BUT IS SHOWN IN IT'S
              !        ENTIRITY FOR CLARITY.

              if (.not.LHTDRG(ISPC) .or.&
                   &(LHTDRG(ISPC) .and. IABFLG(ISPC).eq.1)) then
                 if (ISPC .eq.20) then
                    P2 = 1709.7229
                    P3 = 5.8887
                    P4 = -0.2286
                 else
                    P2 = 76.5170
                    P3 = 2.2107
                    P4 = -0.6365
                 end if
                 if (D .ge.3.) then
                    H = 4.5 + P2 * EXP(-1.*P3*D**P4)
                 else
                    H = ((4.5+P2*EXP(-1.*P3*(3.**P4))-4.51)*(D-0.3)/2.7)+4.51
                 end if
              else
                 if (D .lt.5.0) then
                    H = 0.0994 + 4.9767*D
                 else
                    H = EXP(AX+BX/(D+1.0))+4.5
                 end if
              end if

           case default
              H = EXP(AX+BX/(D+1.0))+4.5
           end select
        end if
        if (VARIANTCD.eq.'BM') then

           !  IF D<5.0 AND THE SPECIES IS FROM THE WC VARIANT, USE WC SMALL TREE
           !  DUBBING FUNCTION, OTHRWISE USE THE WYKOFF EQUATION.

           if ((D.lt.5.0) .and. (ISPC.eq.13.or. ISPC.eq.14.or. ISPC.eq.16&
                &.or. ISPC.eq.18)) then

              select case (ISPC)
              case (13,14)
                 H = 1.5907 + 0.3040*D
                 H = EXP(H)
              case (16,18)
                 H = 0.0994 + 4.9767*D
              end select

           else
              H = EXP(AX+BX/(D+1.0))+4.5
           end if
        end if
        if (VARIANTCD.eq.'WC'.or.VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'OP') then

           if (D .lt.5.0.and. ISPC .ne.17) then
              IICR = ICR(II)
              if (IICR.le.0)IICR = 40
              if (VARIANTCD.eq.'WC'.or.VARIANTCD.eq.'PN') then
                 H = HTT1(ISPC,1) + HTT1(ISPC,2)*D&
                      &+ HTT1(ISPC,3)*FLOAT(IICR) + HTT1(ISPC,4)*D*D&
                      &+ HTT1(ISPC,5)
              end if
              if (VARIANTCD.eq.'OP') then
                 H = HTT1(ISPC,1) + HTT1(ISPC,2)*D&
                      &+ HTT1(ISPC,3)*real(IICR) + HTT1(ISPC,4)*D*D&
                      &+ HTT1(ISPC,5)
              end if
              if (ISPC .le.15&
                   &.or. (ISPC.ge.17.and. ISPC.le.20)&
                   &.or. (ISPC.ge.30.and. ISPC.le.33)) H = EXP(H)
           else
              H = EXP(AX+BX/(D+1.0))+4.5
           end if
        end if
        if (VARIANTCD.eq.'SO') then

           !  LOGIC FOR SPECIES FROM WC VARIANT (SEPARATE EQS. FOR SMALL TREES)

           select case(ISPC)
           case (15,19:23,25,26,28:31,33)
              if (D .lt.5.0) then
                 IICR = ICR(II)
                 if (IICR.le.0)IICR = 40
                 H = HTT1(ISPC,1) + HTT1(ISPC,2)*D&
                      &+ HTT1(ISPC,3)*real(IICR) + HTT1(ISPC,4)*D*D&
                      &+ HTT1(ISPC,5)
                 if (ISPC.eq.19.or.ISPC.eq.20)&
                      &H = EXP(H)
              else
                 H = EXP(AX+BX/(D+1.0))+4.5
              end if

           !  ALL OTHER SPECIES

           case default
              H = EXP(AX+BX/(D+1.0))+4.5
           end select
        end if
        if (VARIANTCD.eq.'EC') then

           select case (ISPC)

           !  SPECIES USING EC EQUATIONS/LOGIC

           case(1:10,12,16,31)
              H = EXP(AX+BX/(D+1.0))+4.5

           !  SPECIES USING WC EQUATIONS/LOGIC

           case(11,13:15,17:30,32)
              if (D .lt.5.0) then
                 IICR = ICR(II)
                 if (IICR.le.0)IICR = 40
                 H = HTT1(ISPC,1) + HTT1(ISPC,2)*D&
                      &+ HTT1(ISPC,3)*FLOAT(IICR) + HTT1(ISPC,4)*D*D&
                      &+ HTT1(ISPC,5)
                 select case (ISPC)
                 case(11,13:15,17:18)
                    H = EXP(H)
                 end select
              else
                 H = EXP(AX+BX/(D+1.0))+4.5
              end if

           end select
        end if
        if (VARIANTCD.eq.'ON') then

           !     CALL HTONT ROUTINE TO PROCESS ONTARIO EQUATIONS,

           call HTONT(ISPC,D,RMSQD,BA,H)
        end if
        if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.&
             &VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.&
             &VARIANTCD.eq.'WC'.or.VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.&
             &VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'ON') then
           if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.&
                &VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.&
                &VARIANTCD.eq.'WC'.or.VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.&
                &VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP') then
              if (DEBUG) write(JOSTND,88) AX,BX,D,H
           end if
           if (VARIANTCD.eq.'ON') then
              if (DEBUG) write(JOSTND,10024) AX,BX,D,H
           end if
        end if
        if (VARIANTCD.eq.'BM') then

           ! SMALL PP & OS USE A DIFFERENT LOGIC. RJ 7-26-88

           if ((ISPC.eq.10.or. ISPC.eq.17) .and. D.lt.3.0) then
              H = 8.31485 + 3.03659*D - 0.59200*JCR
           end if

           !  USE INVENTORY EQUATIONS IF CALIBRATION OF THE HT-DBH FUNCTION IS TURNED
           !  OFF, OR IF WYKOFF CALIBRATION DID NOT OCCUR.
           !  NOTE: THIS SIMPLIFIES TO IF(IABFLB(ISPC).EQ.1) BUT IS SHOWN IN IT'S
           !        ENTIRITY FOR CLARITY.

           if (ISPC.eq.6.or. ISPC.eq.11.or. ISPC.eq.12.or. ISPC.eq.15)&
                &go to 10030
        end if
        if (VARIANTCD.eq.'WC'.or.VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'OP') then

           ! SMALL PP USE A DIFFERENT LOGIC. RJ 7-26-88
           ! HARDWIRE CROWN AT 6 AND CHANGE SPLINE POINT TO 4.0" TO GET SMOOTH
           ! TRANSITION BETWEEN CURVES.  GD 3-26-96

           if (ISPC.eq.15.and. D.lt.4.0) then
              JCR = 6
              H = 8.31485 + 3.03659*D - 0.59200*JCR
           end if
        end if
        if (VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC') then

           ! SMALL PP USE A DIFFERENT LOGIC. RJ 7-26-88

           if (ISPC.eq.10.and. D.lt.3.0) then
              if (VARIANTCD.eq.'SO') then
                 if (JCR .gt.7)JCR = 7
              end if
              if (VARIANTCD.eq.'EC') then
                 if (ICR(II).le.0) then
                    JCR = 4
                 else
                    JCR = ((ICR(II)-1)/10)+1
                    if (JCR.gt.7)JCR = 7
                    if (JCR.le.0)JCR = 4
                 end if
              end if
              H = 8.31485 + 3.03659*D - 0.59200*JCR
           end if
        end if
        if (VARIANTCD.eq.'SO') then

           !  USE INVENTORY EQUATIONS IF CALIBRATION OF THE HT-DBH FUNCTION IS TURNED
           !  OFF, OR IF WYKOFF CALIBRATION DID NOT OCCUR.
           !  NOTE: THIS SIMPLIFIES TO IF(IABFLB(ISPC).EQ.1) BUT IS SHOWN IN IT'S
           !        ENTIRITY FOR CLARITY.

           if (ISPC.eq.11.or.ISPC.eq.16.or.ISPC.eq.24)go to 105
        end if
        if (VARIANTCD.eq.'AK') then
           if (DEBUG)write(JOSTND,*) 'WYKOFF EQN DUBBING',' ISPC=',ISPC, &
                & ' AX=',AX,' BX=',BX,' D=',D,' H=',H
        end if
        if (VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.&
             &VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
             &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'AK'.or.&
             &VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'ON') then

           !  USE INVENTORY EQUATIONS IF CALIBRATION OF THE HT-DBH FUNCTION IS TURNED
           !  OFF, OR IF WYKOFF CALIBRATION DID NOT OCCUR.
           !  NOTE: THIS SIMPLIFIES TO IF(IABFLB(ISPC).EQ.1) BUT IS SHOWN IN IT'S
           !        ENTIRITY FOR CLARITY.

           if (.not.LHTDRG(ISPC) .or.&
                &(LHTDRG(ISPC) .and. IABFLG(ISPC).eq.1)) then
              if (VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.&
                   &VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
                   &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'OC'.or.&
                   &VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'ON') then
                 call HTDBH (IFOR,ISPC,D,H,0)
              end if
              if (VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.&
                   &VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
                   &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'OC'.or.&
                   &VARIANTCD.eq.'OP') then
                 if (DEBUG)write(JOSTND,*)'INVENTORY EQN DUBBING IFOR,ISPC,D,H= '&
                      &,IFOR,ISPC,D,H
              end if
              if (VARIANTCD.eq.'AK') then
                 AX1 = HTT11(ISPC)
                 BX1 = HTT12(ISPC)
                 CX1 = HTT13(ISPC)
                 H = 4.5 + (AX1) * (1-EXP(BX1*D))**CX1
                 if (DEBUG) then
                    write(JOSTND,9009) ISPC,AX1,BX1,CX1,D,H
                 end if
              end if
              if (VARIANTCD.eq.'ON') then
                 if (DEBUG)write(JOSTND,*)' INVENTORY EQN DUBBING IFOR,ISPC,D,H= '&
                      &,IFOR,ISPC,D,H
              end if
           end if
        end if
        if (VARIANTCD.eq.'TT') then
           select case(ISPC)

           case (13,16)

              !  USE INVENTORY EQUATIONS IF CALIBRATION OF THE HT-DBH FUNCTION IS TURNED
              !  OFF, OR IF WYKOFF CALIBRATION DID NOT OCCUR.
              !  NOTE: THIS SIMPLIFIES TO IF(IABFLB(ISPC).EQ.1) BUT IS SHOWN IN IT'S
              !        ENTIRITY FOR CLARITY.

              if (.not.LHTDRG(ISPC) .or.&
                   &(LHTDRG(ISPC) .and. IABFLG(ISPC).eq.1)) then
                 if (ISPC .eq.16) then
                    P2 = 1709.7229
                    P3 = 5.8887
                    P4 = -0.2286
                 else
                    P2 = 76.5170
                    P3 = 2.2107
                    P4 = -0.6365
                 end if
                 if (D .ge.3.) then
                    H = 4.5 + P2 * EXP(-1.*P3*D**P4)
                 else
                    H = ((4.5+P2*EXP(-1.*P3*(3.**P4))-4.51)*(D-0.3)/2.7)+4.51
                 end if
              else
                 if (D .lt.5.0) then
                    H = 0.0994 + 4.9767*D
                 else
                    H = EXP(AX+BX/(D+1.0))+4.5
                 end if
              end if

           case default
              if (ISPC.eq.10.and. D.le.3.0) then
                 H = 1.74189 + 4.17687*D
              else
                 H = EXP(AX+BX/(D+1.0))+4.5
              end if
           end select
        end if
        if (VARIANTCD.eq.'WS') then

           select case(ISPC)

           !  SPECIES USING EQUATIONS FROM THE CA VARIANT

           case(4,9:10,12,14:17,19:20,23,25:27)
              H = EXP(AX+BX/(D+1.0))+4.5
              if (DEBUG) write(JOSTND,88) AX,BX,D,H
              if (.not.LHTDRG(ISPC) .or.&
                   &(LHTDRG(ISPC) .and. IABFLG(ISPC).eq.1)) then
                 call HTDBH (IFOR,ISPC,D,H,0)
                 if (DEBUG)write(JOSTND,*)'INVENTORY EQN DUBBING IFOR,ISPC,D,',&
                      &'H= ',IFOR,ISPC,D,H
              end if

           !  LOGIC FOR SPECIES FROM SO VARIANT (SEPARATE EQS. FOR SMALL TREES)

           case (41)
              if (D .lt.5.0) then
                 H = 0.0994 + 4.9767*D
              else
                 H = EXP(AX+BX/(D+1.0))+4.5
              end if
              if (DEBUG) write(JOSTND,88) AX,BX,D,H
              if (.not.LHTDRG(ISPC) .or.&
                   &(LHTDRG(ISPC) .and. IABFLG(ISPC).eq.1)) then
                 call HTDBH (IFOR,ISPC,D,H,0)
                 if (DEBUG)write(JOSTND,*)'INVENTORY EQN DUBBING IFOR,ISPC,D,',&
                      &'H= ',IFOR,ISPC,D,H
              end if

           !  ALL OTHER SPECIES

           case default
              H = EXP(AX+BX/(D+1.0))+4.5
              if (DEBUG) write(JOSTND,88) AX,BX,D,H

           end select

           ! IF USING COMPUTED COEFFS OR NO SITE FOR THIS SPECIES, USE LOGISTIC FN.

           if (IABFLG(ISPC) .eq.0.or. R5SI .le.0.0) go to 105

           ! SPECIAL R5 HEIGHT DUBBING FUNCTION FOR LARGER TREES

           if (DBH(II).gt.3.0) then
              select case (ISPC)
              case(1:3,5:8,11,18,22,24,42)
                 H = AHATR5 + BHATR5*SQRT(D) + CHATR5*SQRT(D)*SQRT(6.0-SC)
              end select
           end if
           if (ISPC .eq.5) H = H * .76
        end if
        if (VARIANTCD.eq.'AK') then

           ! ADJUST HEIGHT IMPUTATION BASED ON SPECIES
           ! AD, WI, OH: ASSUME MULTIPLIER OF 0.45
           ! SU: ASSUME MULTIPLIER OF 0.65
           ! ALL OTHER SPECIES: ASSUME A MULTIPLIER OF 1.00

           select case (ISPC)
           case(14,21,23)
              H = H * 0.45
           case(22)
              H = H * 0.65
           case default
              H = H * 1.00
           end select

           if (H .le. 4.5) H = 4.5
        end if
        if (VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'WS'.or.VARIANTCD.eq.'AK') then
           105 continue
        end if
        if (VARIANTCD.eq.'KT') then

           !  IF THERE ARE LESS THAN THREE OBSERVATIONS FOR A SPECIES OR LHTDRG
           !  IS FALSE OR REGRESSED COEFFICIENTS DO NOT FORMULATE A GOOD EQUATION
           !  THEN DUB HEIGHTS USING DEFAULT COEFFICIENTS FOR THIS SPECIES.

           if (DEBUG)write(JOSTND,*)'K1=',K1,' D=',D,' SMDBH=',SMDBH(ISPC)
           if (K1.lt.3.or..not. LHTDRG(ISPC) .or. COUNTER .eq.1.0) then
              if (D .ge. SMDBH(ISPC)) then
                 H = EXP(AAHT+BBHT/(D+1.0))+4.5
              else
                 H = SMA(ISPC)*D+4.5
              end if

           !  IF THERE ARE ENOUGH SAMPLE TREES, REGRESS STAND DATA USING THE
           !  SQUARE ROOT OF D EQUATION.  FIND THE COMMON SOLUTION BETWEEN THE
           !  SMALL AND LARGE TREE HEIGHT DUBBING EQUATIONS AND PREDICT HEIGHTS
           !  FOR ALL TREES BELOW THAT DIAMETER WITH THE LINEAR EQUATION.

           else
              H = AAHT + BBHT*SQRT(D)
              if (DEBUG)write(JOSTND,*)'I=',II,' AA=',AAHT,' BB=',BBHT,&
                   &'SMA(ISPC)=',SMA(ISPC)
              STEP1 = ((BBHT*BBHT)-4*SMA(ISPC)*(4.5-AAHT))
              !  IERRCK FLAGS THAT THE EQUATIONS FOR THIS SPECIES HAVE NO COMMON
              !  SOLUTION; DEFINE IT ON EVERY EVALUATION (FOUND_BUGS 19).
              IERRCK = 0
              if (STEP1.le.0.0) then
                 IERRCK = 1
                 go to 115
              end if
              RAD = SQRT(STEP1)
              SOLN = ((BBHT-RAD)/(2*SMA(ISPC)))**2
              if (SOLN .le.0.0) SOLN = ((BBHT+RAD)/(2*SMA(ISPC)))**2
              115 continue
              if (D .lt. SOLN .or.(IERRCK .eq.1.and. D .lt. SMDBH(ISPC)))&
                   &H = SMA(ISPC)*D+4.5
           end if
           if (DEBUG)write(JOSTND,*)'d=',D,&
                &' XN=',XN,' AA=',AAHT,' H=',H,' SMA=',SMA(ISPC),' RAD=',&
                &RAD,' SOLN=',SOLN,' ISPC=',ISPC
        end if
        if (VARIANTCD.eq.'BC') then

           if (LMHTDUB(ISPC)) then
              H = (EXP(AX+BX/(DM+1.0))+1.3) * MtoFT
           else
              H = EXP(AX+BX/(D+1.0))+4.5
           end if
        end if
        if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'IE'.or.VARIANTCD.eq.'CS'.or.&
             &VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.VARIANTCD.eq.'TT'.or.&
             &VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'WC'.or.VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.&
             &VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'OC'.or.&
             &VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'BC'.or.VARIANTCD.eq.'ON') then

           if (H .lt.4.5) H = 4.5
        end if
        if (VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'BC'.or.&
             &VARIANTCD.eq.'ON') then
           10030 continue
        end if
        if (VARIANTCD.eq.'CR') then

           ! SPECIAL BLACK HILLS MODIFICATION TO HEIGHT DUBBING LOGIC.
           ! BH DEFAULT EQUATION FOR ALL SPECIES, USE CALIBRATED LOGISTIC EQUATION
           ! IF ENOUGH OBSERVATIONS EXISTED TO CALIBRATE IT

           if (IMODTY.eq.3.and. IABFLG(ISPC).eq.1) then
              if (D.gt.0.5) then
                 H = 32.108633*(SITEAR(ISPC)**0.276926)*&
                      &((1.0-EXP(-0.057766*D))**(0.9844340**(-0.169876)))+4.5
              else
                 SI = SITEAR(ISPC)
                 CONST = 12.41173 + 0.04633*SI - 0.000158*SI*SI
                 H = CONST * D
              end if
           end if
        end if
        if (VARIANTCD.eq.'CI') then

           select case(ISPC)
           case(11,12,13,14,16,17,19)
              H = EXP(AAHT+BBHT/(D+1.0))+4.5

           case (15)

              !  USE INVENTORY EQUATIONS IF CALIBRATION OF THE HT-DBH FUNCTION IS TURNED
              !  OFF, OR IF WYKOFF CALIBRATION DID NOT OCCUR.
              !  NOTE: THIS SIMPLIFIES TO IF(IABF.EQ.1) BUT IS SHOWN IN IT'S
              !        ENTIRITY FOR CLARITY.

              if (.not.LHTDRG(ISPC) .or.&
                   &(LHTDRG(ISPC) .and. IABF.eq.1)) then
                 P2 = 1709.7229
                 P3 = 5.8887
                 P4 = -0.2286
                 if (D .ge.3.) then
                    H = 4.5 + P2 * EXP(-1.*P3*D**P4)
                 else
                    H = ((4.5+P2*EXP(-1.*P3*(3.**P4))-4.51)*(D-0.3)/2.7)+4.51
                 end if
              else
                 if (D .lt.5.0) then
                    H = 0.0994 + 4.9767*D
                 else
                    H = EXP(AAHT+BBHT/(D+1.0))+4.5
                 end if
              end if

           case default
              if (D .le.3.0) then
                 H = SMHD1(ISPC)+SMHD2(ISPC)*D
              else
                 H = EXP(AAHT+BBHT/(D+1.0))+4.5
              end if
           end select
        end if
        if (VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI') then

           if (H .le.4.5) H = 4.5
        end if
        if (VARIANTCD.eq.'KT') then
           if (HTKEEP(II) .lt.4.5)HTKEEP(II) = 4.5
        end if
        if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
             &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
             &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
             &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
             &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT') then
           10031 continue
        end if
        if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
             &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
             &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
             &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
             &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'OC'.or.&
             &VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'BC'.or.VARIANTCD.eq.'ON') then
           if (TKILL) go to 120
        end if
        if (VARIANTCD.eq.'AK') then
           if (TKILL) go to 120
        end if
        if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
             &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
             &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
             &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
             &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'OC'.or.&
             &VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'BC'.or.VARIANTCD.eq.'ON') then
           HT(II) = H
        end if
        if (VARIANTCD.eq.'KT') then
           if (ISPFL(ISPC) .eq.1) then
              HT(II) = HTKEEP(II)
           else
              HT(II) = H
           end if
           if (DEBUG)write(JOSTND,*)'ISPC=',ISPC,' II=',II,' ISPFL=',&
                &ISPFL(ISPC),' HT(II)=',HT(II),' HTKEEP(II)=',HTKEEP(II),&
                &' H=',H
        end if
        K4 = K4+1
        go to 125
        120 continue
        NORMHT(II) = INT(H*100.0 + 0.5)
        if (ITRUNC(II).eq.0) then
           if (HT(II).gt.0.0) then
              ITRUNC(II) = INT(80.0*HT(II) + 0.5)
           else
              ITRUNC(II) = INT(80.0*H + 0.5)
              HT(II) = H
           end if
        else
           if (HT(II).gt.0.0) then
              if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'CS'.or.&
                   &VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.VARIANTCD.eq.'TT'.or.&
                   &VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.VARIANTCD.eq.'PN'.or.&
                   &VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.VARIANTCD.eq.'CR'.or.&
                   &VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'OC'.or.&
                   &VARIANTCD.eq.'OP') then
                 if (HT(II).lt.(real(ITRUNC(II))*0.01))&
                      &HT(II) = real(ITRUNC(II))*0.01
              end if
              if (VARIANTCD.eq.'IE'.or.VARIANTCD.eq.'BC') then
                 if (HT(II).lt.(real(ITRUNC(II))*0.01))&
                      &HT(II) = ITRUNC(II)*0.01
              end if
              if (VARIANTCD.eq.'ON') then
                 if (HT(II).lt.(ITRUNC(II)*0.01)) HT(II) = ITRUNC(II)*0.01
              end if
           else
              if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'CS'.or.&
                   &VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.VARIANTCD.eq.'TT'.or.&
                   &VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.VARIANTCD.eq.'PN'.or.&
                   &VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.VARIANTCD.eq.'CR'.or.&
                   &VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'OC'.or.&
                   &VARIANTCD.eq.'OP') then
                 HT(II) = real(ITRUNC(II))*0.01
              end if
              if (VARIANTCD.eq.'IE'.or.VARIANTCD.eq.'BC'.or.VARIANTCD.eq.'ON') then
                 HT(II) = ITRUNC(II)*0.01
              end if
           end if
        end if
        if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
             &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'TT'.or.&
             &VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'WC'.or.VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.&
             &VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.&
             &VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'BC') then
           if (real(NORMHT(II))*0.01.lt.HT(II)) NORMHT(II) = INT(HT(II)*100.0)
        end if
        if (VARIANTCD.eq.'SN'.or.VARIANTCD.eq.'ON') then
           if (NORMHT(II)*0.01.lt.HT(II)) NORMHT(II) = INT(HT(II)*100.0)
        end if
        if (VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'AK') then
           if (real(NORMHT(II)*0.01).lt.HT(II)) NORMHT(II) = INT(HT(II)*100.0)
        end if
        if (VARIANTCD.eq.'WS') then

           !  IF FOR SOME STRANGE REASON THE TRUNCATED HEIGHT IS GREATER THAN THE
           !  NORMAL HEIGHT, RELY ON THE DATA--RESET HT TO BE EQUAL TO ITRUNC AND
           !  SHUTOFF TRUNCATION LOGIC BY ASSIGNING NORMHT AND ITRUNC TO ZERO.

           if (NORMHT(II).lt.ITRUNC(II)) then
              HT(II) = real(ITRUNC(II))*0.01
              NORMHT(II) = 0
              ITRUNC(II) = 0
           end if
        end if
        125 continue
        130 continue
     end do
     if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
          &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
          &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
          &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
          &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'AK'.or.&
          &VARIANTCD.eq.'BC'.or.VARIANTCD.eq.'ON') then
        KNT2(IPTR) = K4
     end if

     !  END OF SPECIES LOOP.  PRINT HEIGHT-DIAMETER COEFFICIENTS ON
     !  DEBUG UNIT IF DESIRED.

     140 continue
     if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'IE'.or.VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.&
          &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'BC') then
        if (.not.DEBUG) go to 141
     end if
     if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'IE'.or.VARIANTCD.eq.'BC') then
        if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'IE') then
           write(JOSTND,9005) ISPC,AX,BX,HT1(ISPC),HT2(ISPC)
        end if
        if (VARIANTCD.eq.'BC') then
           write(JOSTND,10013) ISPC,AX,BX,HT1(ISPC),HT2(ISPC)
        end if
     end if
     if (VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP') then
        KNT2(IPTR) = K4 + KNTOHT(ISPC)
        if (DEBUG)write(JOSTND,*)' ISPC,IPTR,K4,KNTOHT,KNT2= ',&
             &ISPC,IPTR,K4,KNTOHT(ISPC),KNT2(IPTR)
     end if
     if (VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.&
          &VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
          &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
          &VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'ON') then
        if (DEBUG) then
           if (VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'SN'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
                &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.VARIANTCD.eq.'OC'.or.&
                &VARIANTCD.eq.'OP') then
              if (VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'SN'.or.VARIANTCD.eq.'WC'.or.VARIANTCD.eq.'PN'.or.&
                   &VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP') then
                 write(JOSTND,10000) ISPC,AX,BX,IABFLG(ISPC)
              end if
              if (VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS') then
                 write(JOSTND,10005) ISPC,AX,BX,IABFLG(ISPC)
              end if
           end if
           if (VARIANTCD.eq.'NC') then
              write(JOSTND,10002) ISPC,AX,BX
           end if
           if (VARIANTCD.eq.'CS') then
              write(JOSTND,10003) ISPC,BX,IABFLG(ISPC)
           end if
           if (VARIANTCD.eq.'NE') then
              write(JOSTND,10000) ISPC,AX,BX,IABFLG(ISPC)
           end if
           if (VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'ON') then
              if (VARIANTCD.eq.'LS') then
                 write(JOSTND,10004) ISPC,IABFLG(ISPC)
              end if
              if (VARIANTCD.eq.'ON') then
                 write(JOSTND,10025) ISPC,IABFLG(ISPC)
              end if
           end if
           if (VARIANTCD.eq.'SO') then
              write(JOSTND,*)'END OF HEIGHT DUBBING FOR SPECIES = ',ISPC
           end if
           if (VARIANTCD.eq.'AK') then
              write(JOSTND,10007) ISPC,K2,AX,BX
           end if
        end if
     end if
     if (VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT') then
        write(JOSTND,29005) ISPC,AX,BX,IABFLG(ISPC)
     end if
     if (VARIANTCD.eq.'CR') then
        write(JOSTND,29005) ISPC,AX,BX,IABFLG(ISPC)
     end if
     if (VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT') then
        write(JOSTND,9005) ISPC,AAHT,BBHT,HT1(ISPC),HT2(ISPC)
     end if

     !  LOOP THROUGH DEAD TREES AND DUB MISSING HEIGHTS FOR THIS SPECIES.

     141 continue
     if (IREC2.gt. MAXTRE) go to 150
     do II = IREC2,MAXTRE
        if (ISP(II).ne.ISPC) go to 145
        if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
             &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
             &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
             &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.VARIANTCD.eq.'CR'.or.&
             &VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'BC'.or.VARIANTCD.eq.'ON') then
           AX = HT1(ISPC)
           BX = HT2(ISPC)
        end if
        if (VARIANTCD.eq.'SO') then
           if (ICR(II).le.0) then
              JCR = 5
           else
              JCR = ((ICR(II)-1)/10)+1
              if (JCR.gt.9)JCR = 9
              if (JCR.le.0)JCR = 5
           end if
        end if
        if (VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'BC'.or.VARIANTCD.eq.'ON') then
           if (IABFLG(ISPC) .eq.0) AX = AA(ISPC)
        end if
        if (VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'BC'.or.&
             &VARIANTCD.eq.'ON') then
           D = DBH(II)
        end if
        if (VARIANTCD.eq.'SO') then

           !  LOGIC FROM SORNEC 11 (HTT1,HTT2 VARIABLES)

           select case(ISPC)
           case(1:8,10,12,13,32)
              AX = HTT1(ISPC,JCR)
              BX = HTT2(ISPC,JCR)
           case default
              AX = HT1(ISPC)
              BX = HT2(ISPC)
           end select
        end if
        if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
             &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
             &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
             &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
             &VARIANTCD.eq.'CR') then
           if (IABFLG(ISPC) .eq.0) AX = AA(ISPC)
        end if
        if (VARIANTCD.eq.'BM') then
           if (ICR(II).le.0) then
              JCR = 4
           else
              JCR = ((ICR(II)-1)/10)+1
              if (JCR.gt.7)JCR = 7
              if (JCR.le.0)JCR = 4
           end if
        end if
        if (VARIANTCD.eq.'WC'.or.VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'OP') then
           JCR = ((ICR(II)-1)/10)+1
        end if
        if (VARIANTCD.eq.'WC'.or.VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'OP') then
           IICR = ICR(II)
           if (IICR.le.0)IICR = 40
        end if
        if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
             &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
             &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
             &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.VARIANTCD.eq.'CR'.or.&
             &VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'OP') then
           D = DBH(II)
        end if
        if (VARIANTCD.eq.'AK') then
           H = 0.0
        end if
        if (VARIANTCD.eq.'BC') then
           DM = DBH(II) * INtoCM
        end if
        if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
             &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
             &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
             &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
             &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'AK'.or.&
             &VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'BC') then
           TKILL = NORMHT(II) .lt.0
        end if
        if (VARIANTCD.eq.'ON') then
           TKILL = NORMHT(II) .lt.0.
        end if
        if (HT(II).gt.0..and. TKILL) go to 142
        if (HT(II).gt.0.) go to 146

        if (D .le.0.1) then
           H = 1.01
           go to 144
        end if
        if (VARIANTCD.eq.'AK') then
           AX = HT1(ISPC)
           BX = HT2(ISPC)
           if (IABFLG(ISPC) .eq. 0) AX = AA(ISPC)
        end if
        if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
             &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
             &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'OC') then

           H = EXP(AX+BX/(D+1.0))+4.5
        end if
        if (VARIANTCD.eq.'BM') then

           !  IF D<5.0 AND THE SPECIES IS FROM THE WC VARIANT, USE WC SMALL TREE
           !  DUBBING FUNCTION, OTHRWISE USE THE WYKOFF EQUATION.

           if ((D.lt.5.0) .and. (ISPC.eq.13.or. ISPC.eq.14.or. ISPC.eq.16&
                &.or. ISPC.eq.18)) then

              select case (ISPC)
              case (13,14)
                 H = 1.5907 + 0.3040*D
                 H = EXP(H)
              case (16,18)
                 H = 0.0994 + 4.9767*D
              end select

           else
              H = EXP(AX+BX/(D+1.0))+4.5
           end if

           ! SMALL PP & OS USE A DIFFERENT LOGIC. RJ 7-26-88

           if ((ISPC.eq.10.or. ISPC.eq.17) .and. D.lt.3.0) then
              H = 8.31485 + 3.03659*D - 0.59200*JCR
           end if

           !  USE INVENTORY EQUATIONS IF CALIBRATION OF THE HT-DBH FUNCTION IS TURNED
           !  OFF, OR IF WYKOFF CALIBRATION DID NOT OCCUR.
           !  NOTE: THIS SIMPLIFIES TO IF(IABFLB(ISPC).EQ.1) BUT IS SHOWN IN IT'S
           !        ENTIRITY FOR CLARITY.

           if (ISPC.eq.6.or. ISPC.eq.11.or. ISPC.eq.12.or. ISPC.eq.15)&
                &go to 10032
        end if
        if (VARIANTCD.eq.'WC'.or.VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'OP') then

           if (D .lt.5.0) then
              if (VARIANTCD.eq.'WC'.or.VARIANTCD.eq.'PN') then
                 H = HTT1(ISPC,1) + HTT1(ISPC,2)*D&
                      &+ HTT1(ISPC,3)*FLOAT(IICR) + HTT1(ISPC,4)*D*D&
                      &+ HTT1(ISPC,5)
              end if
              if (VARIANTCD.eq.'OP') then
                 H = HTT1(ISPC,1) + HTT1(ISPC,2)*D&
                      &+ HTT1(ISPC,3)*real(IICR) + HTT1(ISPC,4)*D*D&
                      &+ HTT1(ISPC,5)
              end if
              if (ISPC .le.15&
                   &.or. (ISPC.ge.17.and. ISPC.le.20)&
                   &.or. (ISPC.ge.30.and. ISPC.le.33)) H = EXP(H)
           else
              H = EXP(AX+BX/(D+1.0))+4.5
           end if

           ! SMALL PP USE A DIFFERENT LOGIC. RJ 7-26-88
           ! HARDWIRE CROWN AT 6 AND CHANGE SPLINE POINT TO 4.0" TO GET SMOOTH
           ! TRANSITION BETWEEN CURVES.  GD 3-26-96

           if (ISPC.eq.15.and. D.lt.4.0) then
              JCR = 6
              H = 8.31485 + 3.03659*D - 0.59200*JCR
           end if
        end if
        if (VARIANTCD.eq.'SO') then

           !  LOGIC FOR SPECIES FROM WC VARIANT (SEPARATE EQS. FOR SMALL TREES)

           select case(ISPC)
           case (15,19:23,25,26,28:31,33)
              if (D .lt.5.0) then
                 IICR = ICR(II)
                 if (IICR.le.0)IICR = 40
                 H = HTT1(ISPC,1) + HTT1(ISPC,2)*D&
                      &+ HTT1(ISPC,3)*real(IICR) + HTT1(ISPC,4)*D*D&
                      &+ HTT1(ISPC,5)
                 if (ISPC .le.15&
                      &.or. (ISPC.ge.17.and. ISPC.le.20)&
                      &.or. (ISPC.ge.30.and. ISPC.le.33)) H = EXP(H)
              else
                 H = EXP(AX+BX/(D+1.0))+4.5
              end if

           !  ALL OTHER SPECIES

           case default
              H = EXP(AX+BX/(D+1.0))+4.5
           end select
        end if
        if (VARIANTCD.eq.'EC') then

           select case (ISPC)

           !  SPECIES USING EC EQUATIONS/LOGIC

           case(1:10,12,16,31)
              H = EXP(AX+BX/(D+1.0))+4.5

           !  SPECIES USING WC EQUATIONS/LOGIC

           case(11,13:15,17:30,32)
              if (D .lt.5.0) then
                 IICR = ICR(II)
                 if (IICR.le.0)IICR = 40
                 H = HTT1(ISPC,1) + HTT1(ISPC,2)*D&
                      &+ HTT1(ISPC,3)*FLOAT(IICR) + HTT1(ISPC,4)*D*D&
                      &+ HTT1(ISPC,5)
                 select case (ISPC)
                 case(11,13:15,17:18)
                    H = EXP(H)
                 end select
              else
                 H = EXP(AX+BX/(D+1.0))+4.5
              end if

           end select
        end if
        if (VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC') then

           !  SMALL PP USES A DIFFERENT LOGIC. RJ 7-26-88

           if (ISPC.eq.10.and. D.lt.3.0) then
              if (VARIANTCD.eq.'SO') then
                 if (JCR .gt.7)JCR = 7
              end if
              if (VARIANTCD.eq.'EC') then
                 if (ICR(II).le.0) then
                    JCR = 4
                 else
                    JCR = ((ICR(II)-1)/10)+1
                    if (JCR.gt.7)JCR = 7
                    if (JCR.le.0)JCR = 4
                 end if
              end if
              H = 8.31485 + 3.03659*D - 0.59200*JCR
           end if
        end if
        if (VARIANTCD.eq.'SO') then

           !  USE INVENTORY EQUATIONS IF CALIBRATION OF THE HT-DBH FUNCTION IS TURNED
           !  OFF, OR IF WYKOFF CALIBRATION DID NOT OCCUR.
           !  NOTE: THIS SIMPLIFIES TO IF(IABFLB(ISPC).EQ.1) BUT IS SHOWN IN IT'S
           !        ENTIRITY FOR CLARITY.

           if (ISPC.eq.11.or. ISPC.eq.16.or. ISPC.eq.24) go to 106
        end if
        if (VARIANTCD.eq.'AK') then
           if (DEBUG)write(JOSTND,*) 'WYKOFF EQN DUBBING DEAD',' ISPC=',ISPC, &
                & ' AX=',AX,' BX=',BX,' D=',D,' H=',H
        end if
        if (VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.&
             &VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
             &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'AK'.or.&
             &VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP') then

           !  USE INVENTORY EQUATIONS IF CALIBRATION OF THE HT-DBH FUNCTION IS TURNED
           !  OFF, OR IF WYKOFF CALIBRATION DID NOT OCCUR.
           !  NOTE: THIS SIMPLIFIES TO IF(IABFLB(ISPC).EQ.1) BUT IS SHOWN IN IT'S
           !        ENTIRITY FOR CLARITY.

           if (.not.LHTDRG(ISPC) .or.&
                &(LHTDRG(ISPC) .and. IABFLG(ISPC).eq.1)) then
              if (VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.&
                   &VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
                   &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'OC'.or.&
                   &VARIANTCD.eq.'OP') then
                 call HTDBH (IFOR,ISPC,D,H,0)
                 if (DEBUG)write(JOSTND,*)'INVENTORY EQN DUBBING IFOR,ISPC,D,H= '&
                      &,IFOR,ISPC,D,H
              end if
              if (VARIANTCD.eq.'AK') then
                 AX1 = HTT11(ISPC)
                 BX1 = HTT12(ISPC)
                 CX1 = HTT13(ISPC)
                 H = 4.5 + (AX1) * (1-EXP(BX1*D))**CX1
                 if (DEBUG) then
                    write(JOSTND,9010) ISPC,AX1,BX1,CX1,D,H
                 end if
              end if
           end if
        end if
        if (VARIANTCD.eq.'TT') then
           select case(ISPC)

           case (13,16)

              !  USE INVENTORY EQUATIONS IF CALIBRATION OF THE HT-DBH FUNCTION IS TURNED
              !  OFF, OR IF WYKOFF CALIBRATION DID NOT OCCUR.
              !  NOTE: THIS SIMPLIFIES TO IF(IABFLB(ISPC).EQ.1) BUT IS SHOWN IN IT'S
              !        ENTIRITY FOR CLARITY.

              if (.not.LHTDRG(ISPC) .or.&
                   &(LHTDRG(ISPC) .and. IABFLG(ISPC).eq.1)) then
                 if (ISPC .eq.16) then
                    P2 = 1709.7229
                    P3 = 5.8887
                    P4 = -0.2286
                 else
                    P2 = 76.5170
                    P3 = 2.2107
                    P4 = -0.6365
                 end if
                 if (D .ge.3.) then
                    H = 4.5 + P2 * EXP(-1.*P3*D**P4)
                 else
                    H = ((4.5+P2*EXP(-1.*P3*(3.**P4))-4.51)*(D-0.3)/2.7)+4.51
                 end if
              else
                 if (D .lt.5.0) then
                    H = 0.0994 + 4.9767*D
                 else
                    H = EXP(AX+BX/(D+1.0))+4.5
                 end if
              end if

           case default
              H = EXP(AX+BX/(D+1.0))+4.5
           end select
        end if
        if (VARIANTCD.eq.'UT') then
           select case(ISPC)

           case (21,20)

              !  USE INVENTORY EQUATIONS IF CALIBRATION OF THE HT-DBH FUNCTION IS TURNED
              !  OFF, OR IF WYKOFF CALIBRATION DID NOT OCCUR.
              !  NOTE: THIS SIMPLIFIES TO IF(IABFLB(ISPC).EQ.1) BUT IS SHOWN IN IT'S
              !        ENTIRITY FOR CLARITY.

              if (.not.LHTDRG(ISPC) .or.&
                   &(LHTDRG(ISPC) .and. IABFLG(ISPC).eq.1)) then
                 if (ISPC .eq.20) then
                    P2 = 1709.7229
                    P3 = 5.8887
                    P4 = -0.2286
                 else
                    P2 = 76.5170
                    P3 = 2.2107
                    P4 = -0.6365
                 end if
                 if (D .ge.3.) then
                    H = 4.5 + P2 * EXP(-1.*P3*D**P4)
                 else
                    H = ((4.5+P2*EXP(-1.*P3*(3.**P4))-4.51)*(D-0.3)/2.7)+4.51
                 end if
              else
                 if (D .lt.5.0) then
                    H = 0.0994 + 4.9767*D
                 else
                    H = EXP(AX+BX/(D+1.0))+4.5
                 end if
              end if

           case default
              H = EXP(AX+BX/(D+1.0))+4.5
           end select
        end if
        if (VARIANTCD.eq.'WS') then

           select case(ISPC)

           !  SPECIES USING EQUATIONS FROM THE CA VARIANT

           case(4,9:10,12,14:17,19:20,23,25:27)
              H = EXP(AX+BX/(D+1.0))+4.5
              if (DEBUG) write(JOSTND,88) AX,BX,D,H
              if (.not.LHTDRG(ISPC) .or.&
                   &(LHTDRG(ISPC) .and. IABFLG(ISPC).eq.1)) then
                 call HTDBH (IFOR,ISPC,D,H,0)
                 if (DEBUG)write(JOSTND,*)'INVENTORY EQN DUBBING IFOR,ISPC,D,',&
                      &'H= ',IFOR,ISPC,D,H
              end if

           !  LOGIC FOR SPECIES FROM SO VARIANT (SEPARATE EQS. FOR SMALL TREES)

           case (41)
              if (D .lt.5.0) then
                 H = 0.0994 + 4.9767*D
              else
                 H = EXP(AX+BX/(D+1.0))+4.5
              end if
              if (DEBUG) write(JOSTND,88) AX,BX,D,H
              if (.not.LHTDRG(ISPC) .or.&
                   &(LHTDRG(ISPC) .and. IABFLG(ISPC).eq.1)) then
                 call HTDBH (IFOR,ISPC,D,H,0)
                 if (DEBUG)write(JOSTND,*)'INVENTORY EQN DUBBING IFOR,ISPC,D,',&
                      &'H= ',IFOR,ISPC,D,H
              end if

           !  ALL OTHER SPECIES

           case default
              H = EXP(AX+BX/(D+1.0))+4.5
              if (DEBUG) write(JOSTND,88) AX,BX,D,H

           end select

           ! IF USING COMPUTED COEFFS OR NO SITE FOR THIS SPECIES, USE LOGISTIC FN.

           if (IABFLG(ISPC) .eq.0.or. R5SI .le.0.0) go to 106

           ! SPECIAL R5 HEIGHT DUBBING FUNCTION FOR LARGER TREES

           if (DBH(II).gt.3.0) then
              select case (ISPC)
              case(1:8,11,18,22,24,42)
                 H = AHATR5 + BHATR5*SQRT(D) + CHATR5*SQRT(D)*SQRT(6.0-SC)
              end select
           end if
           if (ISPC .eq.5) H = H * .76
        end if
        if (VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'WS') then

           106 continue
        end if
        if (VARIANTCD.eq.'KT') then

           !  IF THERE ARE LESS THAN THREE OBSERVATIONS FOR A SPECIES OR LHTDRG
           !  IS FALSE OR REGRESSED COEFFICIENTS DO NOT FORMULATE A GOOD EQUATION
           !  THEN DUB HEIGHTS USING DEFAULT COEFFICIENTS FOR THIS SPECIES.

           if (K1.lt.3.or..not. LHTDRG(ISPC) .or. COUNTER .eq.1.0) then
              if (D .ge. SMDBH(ISPC)) then
                 H = EXP(AAHT+BBHT/(D+1.0))+4.5
              else
                 H = SMA(ISPC)*D+4.5
              end if

           !  IF THERE ARE ENOUGH SAMPLE TREES, REGRESS STAND DATA USING THE
           !  SQUARE ROOT OF D EQUATION.  FIND THE COMMON SOLUTION BETWEEN THE
           !  SMALL AND LARGE TREE HEIGHT DUBBING EQUATIONS AND PREDICT HEIGHTS
           !  FOR ALL TREES BELOW THAT DIAMETER WITH THE LINEAR EQUATION.

           else
              H = AAHT + BBHT*SQRT(D)
              STEP1 = ((BBHT*BBHT)-4*SMA(ISPC)*(4.5-AAHT))
              !  IERRCK FLAGS THAT THE EQUATIONS FOR THIS SPECIES HAVE NO COMMON
              !  SOLUTION; DEFINE IT ON EVERY EVALUATION (FOUND_BUGS 19).
              IERRCK = 0
              if (STEP1.le.0.0) then
                 IERRCK = 1
                 go to 143
              end if
              RAD = SQRT(STEP1)
              SOLN = ((BBHT-RAD)/(2*SMA(ISPC)))**2
              if (SOLN .le.0.0) SOLN = ((BBHT+RAD)/(2*SMA(ISPC)))**2
              143 continue
              if (D .lt. SOLN .or.(IERRCK .eq.1.and. D .lt. SMDBH(ISPC)))&
                   &H = SMA(ISPC)*D+4.5
           end if
        end if
        if (VARIANTCD.eq.'BC') then

           if (LMHTDUB(ISPC)) then
              H = (EXP(AX+BX/(DM+1.0))+1.3) * MtoFT
           else
              H = EXP(AX+BX/(D+1.0))+4.5
           end if
        end if
        if (VARIANTCD.eq.'ON') then

           !     CALL HTONT ROUTINE TO PROCESS ONTARIO EQUATIONS,

           call HTONT(ISPC,D,RMSQD,BA,H)
        end if
        if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'IE'.or.VARIANTCD.eq.'CS'.or.&
             &VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.VARIANTCD.eq.'TT'.or.&
             &VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'WC'.or.VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.&
             &VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'OC'.or.&
             &VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'BC'.or.VARIANTCD.eq.'ON') then
           if (H .lt.4.5) H = 4.5
        end if
        if (VARIANTCD.eq.'BM') then

           10032 continue
        end if
        if (VARIANTCD.eq.'CR') then

           ! SPECIAL BLACK HILLS MODIFICATION TO HEIGHT DUBBING LOGIC.
           ! BH DEFAULT EQUATION FOR ALL SPECIES, USE CALIBRATED LOGISTIC EQUATION
           ! IF ENOUGH OBSERVATIONS EXISTED TO CALIBRATE IT

           if (IMODTY.eq.3.and. IABFLG(ISPC).eq.1) then
              if (D.gt.0.5) then
                 H = 32.108633*(SITEAR(ISPC)**0.276926)*&
                      &((1.0-EXP(-0.057766*D))**(0.9844340**(-0.169876)))+4.5
              else
                 SI = SITEAR(ISPC)
                 CONST = 12.41173 + 0.04633*SI - 0.000158*SI*SI
                 H = CONST * D
              end if
           end if
        end if
        if (VARIANTCD.eq.'CI') then

           select case(ISPC)

           case(11,12,13,14,16,17,19)
              H = EXP(AAHT+BBHT/(D+1.0))+4.5

           case (15)

              !  USE INVENTORY EQUATIONS IF CALIBRATION OF THE HT-DBH FUNCTION IS TURNED
              !  OFF, OR IF WYKOFF CALIBRATION DID NOT OCCUR.
              !  NOTE: THIS SIMPLIFIES TO IF(IABF.EQ.1) BUT IS SHOWN IN IT'S
              !        ENTIRITY FOR CLARITY.

              if (.not.LHTDRG(ISPC) .or.&
                   &(LHTDRG(ISPC) .and. IABF.eq.1)) then
                 P2 = 1709.7229
                 P3 = 5.8887
                 P4 = -0.2286
                 if (D .ge.3.) then
                    H = 4.5 + P2 * EXP(-1.*P3*D**P4)
                 else
                    H = ((4.5+P2*EXP(-1.*P3*(3.**P4))-4.51)*(D-0.3)/2.7)+4.51
                 end if
              else
                 if (D .lt.5.0) then
                    H = 0.0994 + 4.9767*D
                 else
                    H = EXP(AAHT+BBHT/(D+1.0))+4.5
                 end if
              end if

           case default
              if (D .le.3.0) then
                 H = SMHD1(ISPC)+SMHD2(ISPC)*D
              else
                 H = EXP(AAHT+BBHT/(D+1.0))+4.5
              end if
           end select
        end if
        if (VARIANTCD.eq.'AK') then

           ! ADJUST HEIGHT IMPUTATION BASED ON SPECIES
           ! AD, WI, OH: ASSUME MULTIPLIER OF 0.45
           ! SU: ASSUME MULTIPLIER OF 0.65
           ! ALL OTHER SPECIES: ASSUME A MULTIPLIER OF 1.00

           select case (ISPC)
           case(14,21,23)
              H = H * 0.45
           case(22)
              H = H * 0.65
           case default
              H = H * 1.00
           end select
        end if
        if (VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.&
             &VARIANTCD.eq.'AK') then

           if (H .le.4.5) H = 4.5
        end if
        if (VARIANTCD.eq.'KT') then
           if (HTKEEP(II).lt.4.5)HTKEEP(II) = 4.5
        end if
        144 continue
        if (TKILL) go to 142
        if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
             &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
             &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
             &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
             &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'OC'.or.&
             &VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'BC'.or.VARIANTCD.eq.'ON') then
           HT(II) = H
        end if
        if (VARIANTCD.eq.'KT') then
           if (ISPFL(ISPC) .eq.1) then
              HT(II) = HTKEEP(II)
           else
              HT(II) = H
           end if
        end if
        go to 146
        142 continue
        if (HT(II) .gt.0.) then
           NORMHT(II) = INT(HT(II)*100.0 + 0.5)
        else
           NORMHT(II) = INT(H*100.0 + 0.5)
        end if
        if (ITRUNC(II).eq.0) then
           if (HT(II).gt.0.0) then
              ITRUNC(II) = INT(80.0*HT(II) + 0.5)
           else
              ITRUNC(II) = INT(80.0*H + 0.5)
              HT(II) = H
           end if
        else
           if (HT(II).gt.0.0) then
              if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'CS'.or.&
                   &VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'SN'.or.VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.&
                   &VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.&
                   &VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.&
                   &VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP') then
                 if (HT(II).lt.(real(ITRUNC(II))*0.01))&
                      &HT(II) = real(ITRUNC(II))*0.01
              end if
              if (VARIANTCD.eq.'IE'.or.VARIANTCD.eq.'BC') then
                 if (HT(II).lt.(real(ITRUNC(II))*0.01)) HT(II) = ITRUNC(II)*0.01
              end if
              if (VARIANTCD.eq.'LS') then
                 if (HT(II).lt.(real(ITRUNC(II)*0.01)))&
                      &HT(II) = real(ITRUNC(II))*0.01
              end if
              if (VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'ON') then
                 if (HT(II).lt.(ITRUNC(II)*0.01)) HT(II) = ITRUNC(II)*0.01
              end if
           else
              if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'CS'.or.&
                   &VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.VARIANTCD.eq.'TT'.or.&
                   &VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.VARIANTCD.eq.'PN'.or.&
                   &VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.VARIANTCD.eq.'CR'.or.&
                   &VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP') then
                 HT(II) = real(ITRUNC(II))*0.01
              end if
              if (VARIANTCD.eq.'IE'.or.VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'BC'.or.VARIANTCD.eq.'ON') then
                 HT(II) = ITRUNC(II)*0.01
              end if
              if (VARIANTCD.eq.'CI') then
                 HT(II) = real(ITRUNC(II)*0.01)
              end if
           end if
        end if
        if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'ON') then
           if (NORMHT(II)*0.01.lt.HT(II)) NORMHT(II) = INT(HT(II)*100.0)
        end if
        if (VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.&
             &VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.&
             &VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.&
             &VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.&
             &VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'BC') then
           if (real(NORMHT(II))*0.01.lt.HT(II)) NORMHT(II) = INT(HT(II)*100.0)
        end if
        if (VARIANTCD.eq.'WS') then

           !  IF FOR SOME STRANGE REASON THE TRUNCATED HEIGHT IS GREATER THAN THE
           !  NORMAL HEIGHT, RELY ON THE DATA--RESET HT TO BE EQUAL TO ITRUNC AND
           !  SHUTOFF TRUNCATION LOGIC BY ASSIGNING NORMHT AND ITRUNC TO ZERO.

           if (NORMHT(II).lt.ITRUNC(II)) then
              HT(II) = real(ITRUNC(II))*0.01
              NORMHT(II) = 0
              ITRUNC(II) = 0
           end if
        end if

        !   CALL FIRE SNAG MODEL TO ADD THE DEAD TREES TO THE
        !   SNAG LIST; DEFLATE PROB(II), WHICH WAS TEMPORARILY
        !   ADJUSTED TO ALLOW BACKDATING FOR CALIBRATION PURPOSES,
        !   IN **NOTRE**

        146 continue
        if (TKILL) then
           if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'SN'.or.VARIANTCD.eq.'WC'.or.&
                &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'ON') then
              HS = ITRUNC(II) * 0.01
           end if
           if (VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.&
                &VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.&
                &VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.VARIANTCD.eq.'CR'.or.&
                &VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'BC') then
              HS = real(ITRUNC(II)) * 0.01
           end if
           if (VARIANTCD.eq.'OP') then
              HS = real(ITRUNC(II) * 0.01)
           end if
        else
           HS = HT(II)
        end if
        call FMSSEE (II,ISPC,D,HS,&
             &(PROB(II)/(FINT/FINTM)),3,.false.,JOSTND)

        145 continue
     end do

     150 continue
  end do
  if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
       &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
       &VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.VARIANTCD.eq.'PN'.or.&
       &VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.VARIANTCD.eq.'CR'.or.&
       &VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'OC'.or.&
       &VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'BC'.or.VARIANTCD.eq.'ON') then

     !  END OF HEIGHT DUBBING SEGMENT.  PRINT CONTROL TABLE ENTRIES FOR
     !  USEABLE HEIGHTS AND MISSING HEIGHTS, AND REINITIALIZE COUNTERS.

     if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
          &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
          &VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'WC'.or.VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.&
          &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'AK'.or.&
          &VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP') then
        write(JOSTND,9006) (KNT2(I),I = 1,NUMSP)
     end if
     if (VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS') then
        write(JOSTND,19006) (KNT2(I),I = 1,NUMSP)
     end if
     if (VARIANTCD.eq.'BC') then
        write(JOSTND,10014) (KNT2(I),I = 1,NUMSP)
     end if
     if (VARIANTCD.eq.'ON') then
        write(JOSTND,10026) (KNT2(I),I = 1,NUMSP)
     end if
     if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
          &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
          &VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'WC'.or.VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.&
          &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'OC'.or.&
          &VARIANTCD.eq.'OP') then
        write(JOSTND,9007) (KNT(I),I = 1,NUMSP)
     end if
     if (VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS') then
        write(JOSTND,19007) (KNT(I),I = 1,NUMSP)
     end if
     if (VARIANTCD.eq.'AK') then
        write(JOSTND,10008) (KNT(I),I = 1,NUMSP)
     end if
     if (VARIANTCD.eq.'BC') then
        write(JOSTND,10015) (KNT(I),I = 1,NUMSP)
     end if
     if (VARIANTCD.eq.'ON') then
        write(JOSTND,10027) (KNT(I),I = 1,NUMSP)
     end if
  end if
  if (VARIANTCD.eq.'TT') then
     write(JOSTND,19006) (KNT2(I),I = 1,NUMSP)
     write(JOSTND,19007) (KNT(I),I = 1,NUMSP)
  end if
  do I = 1,MAXSP
     KNT(I) = 0
     KNT2(I) = 0
     160 continue
  end do
  if (VARIANTCD.eq.'OC') then

     !  ADD THE NUMBER OF MISSING CROWN RATIOS DUBBED BY ORGANON TO THE
     !  NUMBER BEING DUBBED BY FVS.

     do ISPC = 1,MAXSP
        if (IREF(ISPC) .eq.0) cycle
        IPTR = IREF(ISPC)
        KNT2(IPTR) = KNT2(IPTR) + KNTOCR(ISPC)
        if (DEBUG)write(JOSTND,*)' ISPC,IPTR,KNTOCR,KNT2= ',&
             &ISPC,IPTR,KNTOCR(ISPC),KNT2(ISPC)
     end do
  end if

  !  CHECK FOR MISSING CROWNS ON LIVE TREES.
  !  SAVE PCT IN OLDPCT TO RETAIN AN OLD PCTILE VALUE.

  MISSCR = .false.
  do I = 1,ITRN
     OLDPCT(I) = PCT(I)
     if (ICR(I).le.0) then
        MISSCR = .true.
        ISPC = ISP(I)
        IPTR = IREF(ISPC)
        KNT2(IPTR) = KNT2(IPTR)+1
     end if
     if (ITRE(I).le.0) ITRE(I) = 9999
     190 continue
  end do
  if (VARIANTCD.eq.'OP') then

     !  ADD THE NUMBER OF MISSING CROWN RATIOS DUBBED BY ORGANON TO THE
     !  NUMBER BEING DUBBED BY FVS.

     do ISPC = 1,MAXSP
        if (IREF(ISPC) .eq.0) cycle
        IPTR = IREF(ISPC)
        KNT2(IPTR) = KNT2(IPTR) + KNTOCR(ISPC)
        if (DEBUG)write(JOSTND,*)' ISPC,IPTR,KNTOCR,KNT2= ',&
             &ISPC,IPTR,KNTOCR(ISPC),KNT2(ISPC)
     end do
  end if

  !  CHECK FOR MISSING CROWNS ON CYCLE 0 DEAD TREES.

  if (IREC2.gt. MAXTRE) go to 192
  do I = IREC2,MAXTRE
     if (ICR(I).le.0)MISSCR = .true.
     191 continue
  end do
  192 continue

  !  CALL **CROWN** IF ANY CROWN RATIOS ARE MISSING.

  if (MISSCR)call CROWN
  if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
       &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
       &VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.VARIANTCD.eq.'PN'.or.&
       &VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.VARIANTCD.eq.'CR'.or.&
       &VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'OC'.or.&
       &VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'BC'.or.VARIANTCD.eq.'ON') then

     !  PRINT CONTROL TABLE ENTRY FOR MISSING CROWN RATIOS; RESET COUNTERS.

     if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
          &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
          &VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'WC'.or.VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.&
          &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'AK'.or.&
          &VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP') then
        write(JOSTND,9008) (KNT2(I),I = 1,NUMSP)
     end if
     if (VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS') then
        write(JOSTND,19008) (KNT2(I),I = 1,NUMSP)
     end if
     if (VARIANTCD.eq.'BC') then
        write(JOSTND,10016) (KNT2(I),I = 1,NUMSP)
     end if
     if (VARIANTCD.eq.'ON') then
        write(JOSTND,10028) (KNT2(I),I = 1,NUMSP)
     end if
  end if
  if (VARIANTCD.eq.'EM') then
     do I = 1,11
        KNT2(I) = 0
        200 continue
     end do
  end if
  if (VARIANTCD.eq.'TT') then
     write(JOSTND,19008) (KNT2(I),I = 1,NUMSP)
  end if
  if (VARIANTCD.eq.'IE'.or.VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.&
       &VARIANTCD.eq.'WC'.or.VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.&
       &VARIANTCD.eq.'WS'.or.VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT'.or.&
       &VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'BC'.or.&
       &VARIANTCD.eq.'ON') then
     do I = 1,MAXSP
        KNT2(I) = 0
        10033 continue
     end do
  end if
  if (VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
       &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
       &VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'ON') then

     !   CALL AVHT40 TO CALCULATE AVERAGE HEIGHT. THIS CALL IS NEEDED
     !   IN SORNEC BECAUSE DGF ROUTINE USES AVH IN CALCULATION OF DDS.

     call AVHT40
  end if
  if (VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'CR') then
     AGERNG = 1000.
     YOUNG = 1000.
     OLD = 0.
  end if
  if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'IE'.or.VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.&
       &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI') then

     !  ESTIMATE MISSING TOTAL TREE AGE
     !  NOTE: IN THIS VARIANT, FINDAG CALLS POTHTG WHICH SETS ABIRTH(I)

     if (DEBUG)write(JOSTND,*)'IN CRATET, CALLING FINDAG'
  end if
  if (VARIANTCD.eq.'BC') then

     !  ESTIMATE MISSING TOTAL TREE AGES

     if (DEBUG)write(JOSTND,*)' IN CRATET, CALLING FINDAG'
  end if
  if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'IE'.or.VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.&
       &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'BC') then
     do I = 1,ITRN
        if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CI') then
           if (ABIRTH(I) .le.0.) then
              SITAGE = 0.0
              SITHT = 0.0
              AGMAX = 0.0
              HTMAX = 0.0
              HTMAX2 = 0.0
              ISPC = ISP(I)
              if (VARIANTCD.eq.'EM') then
                 D = DBH(I)
              end if
              if (VARIANTCD.eq.'CI') then
                 D1 = DBH(I)
              end if
              H = HT(I)
              D2 = 0.0
              if (VARIANTCD.eq.'EM') then
                 call FINDAG(I,ISPC,D,D2,H,SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,&
                      &DEBUG)
              end if
              if (VARIANTCD.eq.'CI') then
                 call FINDAG(I,ISPC,D1,D2,H,SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,&
                      &DEBUG)
                 if (SITAGE .gt.0.)ABIRTH(I) = SITAGE
              end if
           !       IF(SITAGE .GT. 0.)ABIRTH(I)=SITAGE
           end if
        end if
        if (VARIANTCD.eq.'IE'.or.VARIANTCD.eq.'BC') then
           ISPC = ISP(I)
        end if
        if (VARIANTCD.eq.'IE') then
           select case (ISPC)
           case(18,20,21)
              if (ABIRTH(I) .le.0.) then
                 SITAGE = 0.0
                 SITHT = 0.0
                 AGMAX = 0.0
                 HTMAX = 0.0
                 HTMAX2 = 0.0
                 D = DBH(I)
                 H = HT(I)
                 D2 = 0.0
                 call FINDAG(I,ISPC,D,D2,H,SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,&
                      &DEBUG)
                 if (SITAGE .gt.0.)ABIRTH(I) = SITAGE
              end if
           end select
        end if
        if (VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'CR') then
           H = HT(I)
           if (ABIRTH(I) .le.0.0.or. ABIRTH(I).gt.999.) then
              SITAGE = 0.0
              SITHT = 0.0
              AGMAX = 0.0
              HTMAX = 0.0
              HTMAX2 = 0.0
              ISPC = ISP(I)
              D1 = DBH(I)
              D2 = 0.0
              call FINDAG(I,ISPC,D1,D2,H,SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,&
                   &DEBUG)
              if (SITAGE .gt.0.)ABIRTH(I) = SITAGE
           end if

           !  EXCLUDE SEEDLINGS FROM EA/UEA DETERMINATION. TREES EXACTLY 4.5'
           !  ARE EXCLUDED BECAUSE WE HAVE THEM AS .1 DBH AND THIS WOULD
           !  PROBABLY MESS UP THE CALCUALTION.  DIXON. 5-2-91.

           if (H .gt.4.5) then
              if (ABIRTH(I).le.1.)go to 210
              if (ABIRTH(I) .lt. YOUNG) YOUNG = ABIRTH(I)
              if (ABIRTH(I) .gt. OLD) OLD = ABIRTH(I)
           end if
           210 continue
        end if
        if (VARIANTCD.eq.'BC') then
           select case (ISPC)
           case(11,12,13,15)
              if (ABIRTH(I) .le.0.) then
                 SITAGE = 0.0
                 SITHT = 0.0
                 AGMAX = 0.0
                 HTMAX = 0.0
                 HTMAX2 = 0.0
                 D = DBH(I)
                 H = HT(I)
                 D2 = 0.0
                 call FINDAG(I,ISPC,D,D2,H,SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,&
                      &DEBUG)
                 if (SITAGE .gt.0.)ABIRTH(I) = SITAGE
              end if
           end select
        end if
     end do
  end if
  if (VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.&
       &VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN') then
     do I = 1,MAXSP
        KNT2(I) = 0
        10034 continue
     end do

     !   CALL AVHT40 TO CALCULATE AVERAGE HEIGHT. THIS CALL IS NEEDED
     !   IN SORNEC BECAUSE DGF ROUTINE USES AVH IN CALCULATION OF DDS.

     call AVHT40
  end if
  if (VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'CR') then

     ! COMPUTE AGE RANGE.

     AGERNG = ABS(OLD - YOUNG)
  end if
  if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.&
       &VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'WC'.or.&
       &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
       &VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP') then

     !  CALL DGDRIV TO CALIBRATE DIAMETER GROWTH EQUATIONS.

     if (DEBUG)write(JOSTND,*)'CALL DGDRIV FROM CRATET SECOND TIME'
  end if
  if (VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM') then
     if (DEBUG) write(JOSTND,*)'CALL DGDRIV FROM CRATET SECOND',&
          &' TIME'
  end if
  if (VARIANTCD.eq.'ON') then

     !  CALL DGDRIV TO CALIBRATE DIAMETER GROWTH EQUATIONS.

     if (DEBUG)write(JOSTND,*)' CALL DGDRIV FROM CRATET SECOND TIME'
  end if
  call DGDRIV

  !  PREPARE INPUT DATA FOR HEIGHT GROWTH MODEL CALIBRATION; IT'S DONE
  !  THE SAME AS THE DIAMETER GROWTH MODEL SEEN ABOVE.

  if (IHTG.eq.0.or. IHTG.eq.2) go to 236
  Q = 1.
  if (IHTG.eq.3) Q = -1.
  do I = 1,ITRN
     if (HTG(I).le.0.) go to 233
     HTG(I) = Q * (HT(I)-HTG(I))
     if (HT(I) .le.0.0) HTG(I) = 0.0
     233 continue
  end do
  236 continue
  if (VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.&
       &VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
       &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
       &VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP') then

     !  ESTIMATE MISSING TOTAL TREE AGES

     if (DEBUG)write(JOSTND,*)'IN CRATET, CALLING FINDAG'
  end if
  if (VARIANTCD.eq.'AK') then

     !  ESTIMATE MISSING TOTAL TREE AGE
     !  AK VARIANT DOES NOT ESTIMATE TREE AGE. IF AGE IS PROVIDED ON INPUT
     !  TREE RECORD, IT WILL BE INCREMENTED. PLANTED AND REGEN TREES DO NOT
     !  GET AGE ASSIGNMENTS EITHER.

     if (DEBUG)write(JOSTND,*)'IN CRATET, SET TREE AGE 0 IF LT 0'
  end if
  if (VARIANTCD.eq.'ON') then

     !  ESTIMATE MISSING TREE AGE

     if (DEBUG)write(JOSTND,*)' IN CRATET, CALLING FINDAG'
  end if
  if (VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.&
       &VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
       &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
       &VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.&
       &VARIANTCD.eq.'ON') then
     do I = 1,ITRN
        if (VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.&
             &VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
             &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
             &VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'ON') then
           if (ABIRTH(I) .le.0.) then
              SITAGE = 0.0
              SITHT = 0.0
              AGMAX = 0.0
              HTMAX = 0.0
              HTMAX2 = 0.0
              ISPC = ISP(I)
              if (VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.VARIANTCD.eq.'PN'.or.&
                   &VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP') then
                 D1 = DBH(I)
              end if
              if (VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.&
                   &VARIANTCD.eq.'SN'.or.VARIANTCD.eq.'WS'.or.VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'ON') then
                 D = DBH(I)
              end if
              H = HT(I)
              D2 = 0.0
              if (VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.VARIANTCD.eq.'PN'.or.&
                   &VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP') then
                 call FINDAG(I,ISPC,D1,D2,H,SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,&
                      &DEBUG)
              end if
              if (VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.&
                   &VARIANTCD.eq.'SN'.or.VARIANTCD.eq.'WS'.or.VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'ON') then
                 call FINDAG(I,ISPC,D,D2,H,SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,&
                      &DEBUG)
              end if
              if (SITAGE .gt.0.)ABIRTH(I) = SITAGE
           end if
        end if
        if (VARIANTCD.eq.'AK') then
           if (ABIRTH(I) .lt. 0.) then
              ABIRTH(I) = 0.0
           end if
        end if
     end do
  end if

  !  CALL REGENT TO CALIBRATE THE SMALL TREE HEIGHT INCREMENT MODEL.

  call REGENT(.false.,1)

  !  LOAD SPCNT WITH NUMBER OF TREES PER ACRE BY SPECIES AND TREE
  !  CLASS.

  do I = 1,ITRN
     IS = ISP(I)
     IM = IMC(I)
     SPCNT(IS,IM) = SPCNT(IS,IM)+PROB(I)
     240 continue
  end do

  !  COMPUTE DISTRIBUTION OF TREES PER ACRE AND SPECIES-TREE CLASS
  !  COMPOSITION BY TREES PER ACRE.

  245 continue
  call PCTILE(ITRN,IND,PROB,WK3,ONTCUR(7))
  call DIST(ITRN,ONTCUR,WK3)
  call COMP(OSPCT,IOSPCT,SPCNT)
  if (ITRN.le.0) go to 500
  if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'IE'.or.VARIANTCD.eq.'CS'.or.&
       &VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.VARIANTCD.eq.'TT'.or.&
       &VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.&
       &VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT'.or.&
       &VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'BC'.or.VARIANTCD.eq.'ON') then

     !  CALL **DENSE** TO CALCULATE STAND DENSITY STATISTICS FOR
     !  INITIAL INVENTORY.

     if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'IE'.or.VARIANTCD.eq.'CS'.or.&
          &VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.VARIANTCD.eq.'TT'.or.&
          &VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.&
          &VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT'.or.&
          &VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP') then
        if (DEBUG) write(JOSTND,9013) ICYC
     end if
     if (VARIANTCD.eq.'BC'.or.VARIANTCD.eq.'ON') then
        if (DEBUG) write(JOSTND,10017) ICYC
     end if
  end if
  call DENSE
  if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
       &VARIANTCD.eq.'SN'.or.VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.&
       &VARIANTCD.eq.'WC'.or.VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.&
       &VARIANTCD.eq.'WS'.or.VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT'.or.&
       &VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'BC') then

     !  COUNT AND PRINT NUMBER OF RECORDS WITH MISTLETOE.

     call MISCNT(KNT)
  end if
  if (VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'ON') then
     do I = 1,MAXSP
        KNT(I) = 0
        246 continue
     end do
     do I = 1,ITRN
        call MISGET(I,IMR)
        if (IMR .eq.0)go to 247
        IPTR = IREF(ISP(I))
        KNT(IPTR) = KNT(IPTR) + 1
        247 continue
     end do
  end if
  if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
       &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
       &VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.&
       &VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.&
       &VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.&
       &VARIANTCD.eq.'BC'.or.VARIANTCD.eq.'ON') then
     if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'IE'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'EC'.or.&
          &VARIANTCD.eq.'WS'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT') then
        write(JOSTND,248) (KNT(I),I = 1,NUMSP)
     end if
     if (VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.&
          &VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.VARIANTCD.eq.'WC'.or.VARIANTCD.eq.'PN'.or.&
          &VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'AK'.or.VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP') then
        write(JOSTND,10001) (KNT(I),I = 1,NUMSP)
     end if
     if (VARIANTCD.eq.'CR') then
        write(JOSTND,20248) (KNT(I),I = 1,NUMSP)
     end if
     if (VARIANTCD.eq.'BC') then
        write(JOSTND,10018) (KNT(I),I = 1,NUMSP)
     end if
     if (VARIANTCD.eq.'ON') then
        write(JOSTND,10029) (KNT(I),I = 1,NUMSP)
     end if
  end if
  if (VARIANTCD.eq.'TT') then
     write(JOSTND,248) (KNT(I),I = 1,NUMSP)
  end if
  if (VARIANTCD.eq.'UT') then
     write(JOSTND,20248) (KNT(I),I = 1,NUMSP)
  end if

  !  CALL **SDICHK** TO SEE IF INITIAL STAND SDI IS ABOVE THE SPECIFIED
  !  MAXIMUM SDI.  RESET MAXIMUM SDI IF THIS IS THE CASE.

  call SDICHK
  if (VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP') then

     !  WRITE THE ORGANON SETTINGS TABLE FOR FVS-ORGANON VARIANTS

     call ORGTAB
  end if

  500 continue
  if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'IE'.or.&
       &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'SN'.or.&
       &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'WC'.or.&
       &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'SO'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'WS'.or.&
       &VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'KT'.or.VARIANTCD.eq.'AK'.or.&
       &VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP') then
     if (DEBUG)write(JOSTND,510)ICYC
  end if
  if (VARIANTCD.eq.'BC'.or.VARIANTCD.eq.'ON') then
     if (DEBUG)write(JOSTND,10019)ICYC
  end if

  return

  !  ORIGINAL OUTPUT FORMATS, SHARED ONLY WHEN THEIR LITERALS MATCH.
  9000 format ((T49,11(1X,A2,3X)/))
  9001 format (T49,11(A4,2X))
  9002 format(/,'NUMBER OF RECORDS PER SPECIES',&
       &((T49,11(I4,2X)/)))
  9003 format('IN CRATET: DEAD TREE RECORD:  I=',I4,',  IMC=',I2,&
       &',  SPECIES=',I2,' (9003 CRATET)')
  9004 format(/,'NUMBER OF RECORDS CODED AS RECENT MORTALITY',&
       &((T49,11(I4,2X)/)))
  52 format (/'***** NOTE:  SPECIES HAVE BEEN DROPPED.')
  88 format('CRATET DUBBED HEIGHT: AX,BX,D,H=',4F8.2)
  9005 format('HEIGHT-DIAMETER COEFFICIENTS FOR SPECIES ',I2,&
       &':  INTERCEPT=',F10.6,'  SLOPE=',F10.6,/&
       &'DEFAULT COEFFICIENTS FOR THIS SPECIES',7X,&
       &':  INTERCEPT=',F10.6,'  SLOPE=',F10.6,' (9005 CRATET)')
  9006 format(/,'NUMBER OF RECORDS WITH MISSING HEIGHTS',&
       &((T49,11(I4,2X)/)))
  9007 format(/,'NUMBER OF RECORDS WITH BROKEN OR DEAD TOPS',&
       &((T49,11(I4,2X)/)))
  9008 format(/,'NUMBER OF RECORDS WITH MISSING CROWN RATIOS',&
       &((T49,11(I4,2X)/)))
  9013 format('CALLING DENSE, CYCLE=',I2)
  248 format(/,'NUMBER OF RECORDS WITH MISTLETOE',T49,11(I4,2X))
  510 format('LEAVING SUBROUTINE CRATET  CYCLE =',I5)
  10000 format('HEIGHT-DIAMETER COEFFICIENTS FOR SPECIES ',I2,&
       &':  INTERCEPT=',F10.6,'  SLOPE=',F10.6,'  FLAG=',I3,&
       &' (9005 CRATET)')
  10001 format(/,'NUMBER OF RECORDS WITH MISTLETOE',((T49,11(I4,2X)/)))
  10002 format('HEIGHT-DIAMETER COEFFICIENTS FOR SPECIES ',I2,&
       &':  INTERGEPT=',F10.6,'  SLOPE=',F10.6,' (9005 CRATET)')
  8998 format ((T12,8(A3,'=',F6.0,:,'; '),A,'=',F6.0))
  29005 format('HEIGHT-DIAMETER COEFFICIENTS FOR SPECIES ',I2,&
       &':  INTERCEPT=',F10.6,'  SLOPE=',F10.6,'  IABFLG=',I4,&
       &' (9005 CRATET)')
  10003 format('HEIGHT-DIAMETER COEFFICIENTS FOR SPECIES ',I2,&
       &':  SLOPE=',F10.6,'  FLAG=',I3,&
       &' (9005 CRATET)')
  19006 format(/,'NUMBER OF RECORDS WITH MISSING HEIGHTS',&
       &T49,11(I4,2X))
  19007 format(/,'NUMBER OF RECORDS WITH BROKEN OR DEAD TOPS',&
       &T49,11(I4,2X))
  19008 format(/,'NUMBER OF RECORDS WITH MISSING CROWN RATIOS',&
       &T49,11(I4,2X))
  20248 format(/,'NUMBER OF RECORDS WITH MISTLETOE',T49,11(I4,2X)/)
  10004 format('HEIGHT-DIAMETER COEFFICIENTS FOR SPECIES ',I2,&
       &:  '  FLAG=',I3,&
       &' (9005 CRATET)')
  90 format(/'SPECIES ',5X,10(A2,6X))
  91 format('SDI MAX ',   10F8.0)
  10005 format('HEIGHT-DIAMETER COEFFICIENTS FOR SPECIES ',I2,&
       &':  INTERCEPT=',F10.6,'  SLOPE=',F10.6,'  IABFLG(ISPC)=',I4,&
       &' (9005 CRATET)')
  9114 format('IN CRATET 9114 AHATR5,BHATR5,CHATR5 =',3F11.5)
  10006 format (T47,11(A4,2X))
  9009 format('INVENTORY EQN DUBBING - ISPC ',I2,'  AX1= ',F10.5, &
       & '  BX1=', F7.5,'  CX1=', F7.5,'  D=', F10.5,'  H=', &
       & F10.5)
  10007 format('END OF HT DUBBING FOR SPECIES ',I2,'  K2= ',I4, &
       & ' AX= ',F10.6,' BX= ',F10.6)
  9010 format('INVENTORY EQN DUBBING DEAD - ISPC ',I2,'  AX1= ', &
       & F10.5,'  BX1=', F7.5,'  CX1=', F7.5,'  D=', F10.5, &
       & '  H=', F10.5)
  10008 format(/,'NUMBER OF RECORDS WITH BROKEN OR DEAD TOPS', &
       & ((T549,11(I4,2X)/)))
  124 format(' PREPARE FROM CRATET, CYCLE=',I2,', IMODTY= ',I2,&
       &', LORGPREP= ',L7)
  9125 format(' CRATET ORGANON ERROR CODE, CYCLE= ',I2,' IERROR= ',I2)
  9127 format(' CRATET ORGANON ERROR CODE, CYCLE= ',I2,&
       &' SWARNING= ',I2 )
  9136 format(' CRATET ORGANON ERROR CODE, CYCLE= ',I2,&
       &' IDX= ',I2, ' SERROR= ',I2, ' ERROR IGNORED.')
  9126 format(' CRATET ORGANON ERROR CODE, CYCLE= ',I2,&
       &' IDX= ',I2, ' SERROR= ',I2 )
  9128 format(' CRATET ORGANON ERROR CODE, CYCLE= ',I2,&
       &' TWARNING= ',I2 )
  9129 format(' CRATET ORGANON ERROR CODE, CYCLE=' ,I2,&
       &', TERROR(I,J)= ',I2,&
       &', TREE NUMBER= ',I4,&
       &', ERROR NUMBER= ',I4 )
  9220 format ('   ORGANON ',&
       &' ACALIB(1,',I2,') = ',F9.6,&
       &' ACALIB(2,',I2,') = ',F9.6,&
       &' ACALIB(3,',I2,') = ',F9.6 )
  9221 format ('   ORGANON ',&
       &' TMPCAL(1,',I2,') = ',F9.6,&
       &' TMPCAL(2,',I2,') = ',F9.6,&
       &' TMPCAL(3,',I2,') = ',F9.6,&
       &' NEWCAL(',I2,') = ',I3)
  10009 format(/,' NUMBER OF RECORDS PER SPECIES',&
       &((T49,11(I4,2X)/)))
  10010 format(' IN CRATET: DEAD TREE RECORD:  I=',I4,',  IMC=',I2,&
       &',  SPECIES=',I2,' (9003 CRATET)')
  10011 format(/,' NUMBER OF RECORDS CODED AS RECENT MORTALITY',&
       &((T49,11(I4,2X)/)))
  10012 format (/' ***** NOTE:  SPECIES HAVE BEEN DROPPED.')
  10013 format(' HEIGHT-DIAMETER COEFFICIENTS FOR SPECIES ',I2,&
       &':  INTERCEPT=',F10.6,'  SLOPE=',F10.6,/&
       &' DEFAULT COEFFICIENTS FOR THIS SPECIES',7X,&
       &':  INTERCEPT=',F10.6,'  SLOPE=',F10.6,' (9005 CRATET)')
  10014 format(/,' NUMBER OF RECORDS WITH MISSING HEIGHTS',&
       &((T49,11(I4,2X)/)))
  10015 format(/,' NUMBER OF RECORDS WITH BROKEN OR DEAD TOPS',&
       &((T49,11(I4,2X)/)))
  10016 format(/,' NUMBER OF RECORDS WITH MISSING CROWN RATIOS',&
       &((T49,11(I4,2X)/)))
  10017 format(' CALLING DENSE, CYCLE=',I2)
  10018 format(/,' NUMBER OF RECORDS WITH MISTLETOE',T49,11(I4,2X))
  10019 format(' LEAVING SUBROUTINE CRATET  CYCLE =',I5)
  10020 format((T50,11(1X,A2,3X)/))
  10021 format((T50,11(A4,2X)/))
  10022 format(/,' NUMBER OF RECORDS PER SPECIES',&
       &((T50,11(I4,2X)/)))
  10023 format(/,' NUMBER OF RECORDS CODED AS RECENT MORTALITY',&
       &((T50,11(I4,2X)/)))
  10024 format(' CRATET DUBBED HEIGHT: AX,BX,D,H=',4F8.2)
  10025 format(' HEIGHT-DIAMETER COEFFICIENTS FOR SPECIES ',I2,&
       &:  '  FLAG=',I3,&
       &' (9005 CRATET)')
  10026 format(/,' NUMBER OF RECORDS WITH MISSING HEIGHTS',&
       &((T50,11(I4,2X)/)))
  10027 format(/,' NUMBER OF RECORDS WITH BROKEN OR DEAD TOPS',&
       &((T50,11(I4,2X)/)))
  10028 format(/,' NUMBER OF RECORDS WITH MISSING CROWN RATIOS',&
       &((T50,11(I4,2X)/)))
  10029 format(/,' NUMBER OF RECORDS WITH MISTLETOE',((T50,11(I4,2X)/)))
end subroutine CRATET
