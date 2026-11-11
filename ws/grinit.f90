!=======================================!
!   Initialize growth model variables   !
!=======================================!
! WS $Id$
subroutine GRINIT
  implicit none

  !  INITIALIZE PROGNOSIS MODEL VARIABLES

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'
  include 'OPCOM.f90'
  include 'WORKCM.f90'
  include 'PLOT.f90'
  include 'HTCAL.f90'
  include 'ECON.f90'
  include 'MULTCM.f90'
  include 'SUMTAB.f90'
  include 'VOLSTD.f90'
  include 'VARCOM.f90'
  include 'CWDCOM.f90'
  include 'CALCOM.f90'
  include 'GGCOM.f90'

  ! Variables
  character(len=26) :: DBLK
  character(len=4) :: none
  character(len=2) :: ANINDEX
  integer :: I,J,K

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

  data DBLK/'                          '/
  data none/'NONE'/

  VARACD = 'WS'
  ECOREG = ''
  CFCTYPE = 'F'
  BFCTYPE = 'F'
  call LNKINT
  do I = 1,MAXSP
     SDIDEF(I) = 0.
     IORDER(I) = 0
     COR(I) = 0.0
     XDMULT(I) = 1.
     XHMULT(I) = 1.
     XRDMLT(I) = 1.
     XRHMLT(I) = 1.
     XMMULT(I) = 1.
     XMDIA1(I) = 0.
     XMDIA2(I) = 99999.
     VEQNNB(I) = '           '
     VEQNNC(I) = '           '
     STMP(I) = 1.0
     TOPD(I) = 4.5
     DBHMIN(I) = 7.0
     FRMCLS(I) = 80.0
     METHB(I) = 6
     METHC(I) = 6
     BFSTMP(I) = 1.0
     BFTOPD(I) = 6.0
     BFMIND(I) = 10.0
     SCFSTMP(I) = 1.0
     SCFTOPD(I) = 6.0
     SCFMIND(I) = 10.0
     BFLA0(I) = 0.0
     BFLA1(I) = 1.0
     CFLA0(I) = 0.0
     CFLA1(I) = 1.0
     LDGCAL(I) = .true.
     LHTCAL(I) = .true.

     select case (I)
     case(9:10,12,14:17,19:20,25:27,41)
        LHTDRG(I) = .false.
     case default
        LHTDRG(I) = .true.
     end select

     BARANK(I) = 0.0
     MAXSDI(I) = 0
     SITEAR(I) = 0.0
     IABFLG(I) = 1
     ISTAGF(I) = 0
     LSPCWE(I) = .false.
     CWDS0(I) = 0.
     CWDS1(I) = 0.
     CWDS2(I) = 0.
     CWDS3(I) = 2.
     CWDL0(I) = 0.
     CWDL1(I) = 0.
     CWDL2(I) = 0.
     CWDL3(I) = 2.
     CWTDBH(I) = 0.
     SIZCAP(I,1) = 999.
     SIZCAP(I,2) = 1.
     SIZCAP(I,3) = 0.
     SIZCAP(I,4) = 999.
     JSPIN(I) = 3
     LEAVESP(I) = .false.
     5 continue
  end do
  LFLAGV = .false.
  LBAMAX = .false.
  LZEIDE = .true.
  CALCSDI = ' '
  DSTAG = 1.0
  BAINIT = 0.0
  CFMIN = 0.
  SCFMIN = 0.
  TCFMIN = 0.
  BFMIN = 0.
  BAMIN = 0.
  TCWT = 0.
  SPCLWT = 0.
  PBAWT = 0.
  PCCFWT = 0.
  PTPAWT = 0.
  IREC1 = 0
  RMAI = 0.0
  IREC2 = MAXTP1
  ITHNPI = 1
  ITHNPN = -1
  ITHNPA = 0

  do I = 1,MAXCYC
     IY(I) = -1
     10 continue
  end do
  do K = 1,7
     ITABLE(K) = 0
     11 continue
  end do

  MANAGD = 0
  ISTDORG = 0
  ALPHA = 0.05
  BJPHI = 0.74
  BJTHET = 0.42
  ASPECT = 0.
  LAUTON = .false.
  LFIA = .false.
  LFIANVB = .false.
  AUTMAX = 60.0
  AUTMIN = 45.0
  BAF = 40.
  BAMAX = 0.0
  BRK = 5.0
  DGSD = 2.0
  EFF = 1.0
  ELEV = 45.
  FINT = 10.
  FINTH = 5.
  FINTM = 5.0
  FPA = 300.
  IAGE = 0
  IWORK1(1) = 0

  !  ICL4 IS USED TO IMPLEMENT A NEW OPTION WHICH ALLOWS THE USER TO
  !  CONTROL THE NUMBER OF CYCLES A STAND WILL BE TRIPLED.
  !  ICL5 IS USED TO CARRY THE ACTUAL INPUT HABITAT CODE.

  ICL4 = 2
  ICL5 = 999
  ICL6 = 0
  IDG = 0
  IFINT = 10
  IFINTH = 5
  IFOR = 6
  KODFOR = 0
  IFST = 1
  IGL = 1
  IHTG = 0
  IPTINV = -9999
  ITYPE = 0
  KODTYP = 0
  CPVREF = '          '
  PCOMX = '        '
  IY(1) = 0
  IY(MAXCY1) = -1
  LDCOR2 = .false.
  LDUBDG = .false.
  LEVUSE = .true.
  LFIXSD = .false.
  LHCOR2 = .false.
  LRCOR2 = .false.
  LSUMRY = .false.
  MGMID = none
  MORDAT = .false.
  LSTATS = .false.
  LMORT = .false.
  LBVOLS = .false.
  LCVOLS = .false.
  LFIRE = .false.
  LSITE = .false.
  JOCALB = 0
  GROSPC = -1.0
  NCYC = 0
  NONSTK = -9999
  NOTRIP = .false.
  NPLT = DBLK
  DBCN = ' '
  SAMWT = -1E25
  SLOPE = 5.0
  TLAT = 39.
  TLONG = 120.
  ISTATE = 0
  ICNTY = 0
  TFPA = 0.0
  TRM = 1.
  LBKDEN = .false.
  RELDM1 = 0.
  OLDBA = 0.
  ATAVH = 0.
  ATBA = 0.
  ATCCF = 0.
  ORMSQD = 0.
  SDIMAX = 0.0
  SDIBC = 0.
  SDIAC = 0.
  ISISP = 0
  PMSDIL = 55.
  PMSDIU = 85.
  SLPMRT = 0.0
  CEPMRT = 0.0
  IBASP = 0
  IMODTY = 0
  IPHREG = 0
  IFORTP = 0
  FNMIN = 5.
  NCALHT = 5
  ISILFT = 0
  QMDMSB = 999.
  SLPMSB = 0.
  CEPMSB = 0.
  EFFMSB = 0.90
  DLOMSB = 0.
  DHIMSB = 999.
  MFLMSB = 1
  DBHZEIDE = 0.
  DBHSTAGE = 0.
  DR016 = 0.
  ODR016 = 0.
  ATDR016 = 0.
  DBHSDI = 0.
  JSPINDEF = 0
  CCCOEF = 1.0
  CCCOEF2 = 1.0

  do J = 1,9
     do K = 1,MAXSP
        BFDEFT(J,K) = 0.0
        CFDEFT(J,K) = 0.0
        20 continue
     end do
     30 continue
  end do
  do I = 1,21
     do J = 1,3
        LOGDIA(I,J) = 0.
        35 continue
     end do
     40 continue
  end do
  do I = 1,7
     do J = 1,20
        LOGVOL(I,J) = 0.
        45 continue
     end do
     50 continue
  end do

  !  INITIALIZE ECONOMIC VARIABLES.

  LECBUG = .false.
  LECON = .false.

  !     INITIALIZE SUBROUTINE SPECIFIC DEBUG.

  call DBINIT

  !  INITIALIZE SUMTAB COMMON VARIABLES

  MAIFLG = 0
  NEWSTD = 0
  TOTREM = 0.
  AGELST = 0.
  do I = 1,MAXCY1
     BCYMAI(I) = 0.
     60 continue
  end do

  !     INITIALIZE SPECIES AND POINT GROUPS VARIABLES.

  NPTGRP = 0
  NSPGRP = 0
  do I = 1,30
     do J = 1,52
        ISPGRP(I,J) = 0
        IPTGRP(I,J) = 0
        65 continue
     end do
     70 continue
  end do
  do I = 1,30
     write(ANINDEX,'(I2)') I
     NAMGRP(I) = 'GROUP'//TRIM(ADJUSTL(ANINDEX))
     PTGNAME(I) = 'PTGROUP'//TRIM(ADJUSTL(ANINDEX))
  end do

  !     INITIALIZE SITE TREE ARRAY AND COUNT

  NSITET = 0
  do I = 1,MAXSTR
     do J = 1,6
        SITETR(I,J) = 0.
        74 continue
     end do
     75 continue
  end do

  !  INITIALIZE VARCOM COMMON VARIABLES

  do I = 1,MAXSP
     ISTAGF(I) = 0
  end do

  !     INITIALIZE VARIABLES CARRIED IN GGCOM.

  do I = 1,MAXSP
     TBA(I) = 0.0
     do J = 1,41
        if (I .eq.1) BAU(J) = 0.0
        BCLAS(I,J) = 0.0
        TCLAS(I,J) = 0.0
     end do
  end do
  AGERNG = 0.
  SEEDS = 0.
  BARK1 = 0.
  BARK2 = 0.
  TPAT = 0.
  DSTAG = 0.

  return
end
