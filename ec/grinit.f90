!=======================================!
!   Initialize growth model variables   !
!=======================================!
! EC $Id$
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

  ! Variables
  integer :: I,J,K
  character(len=26) :: DBLK
  character(len=4) :: none
  character(len=2) :: ANINDEX

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

  data DBLK/'                          '/
  data none/'NONE'/

  VARACD = 'EC'
  ECOREG = ''
  CFCTYPE = 'F'
  BFCTYPE = 'F'
  call LNKINT
  do I = 1,MAXSP
     SDIDEF(I) = 0.0
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
     FRMCLS(I) = 0.0
     METHB(I) = 6
     METHC(I) = 6
     BFSTMP(I) = 1.0
     BFTOPD(I) = 4.5
     BFMIND(I) = 7.0
     SCFSTMP(I) = 1.0
     SCFTOPD(I) = 4.5
     SCFMIND(I) = 7.0
     BFLA0(I) = 0.0
     BFLA1(I) = 1.0
     CFLA0(I) = 0.0
     CFLA1(I) = 1.0
     LDGCAL(I) = .true.
     LHTCAL(I) = .true.
     LHTDRG(I) = .false.
     BARANK(I) = 0.0
     MAXSDI(I) = 0
     SITEAR(I) = 0.0
     IABFLG(I) = 1
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
  LZEIDE = .false.
  CALCSDI = ' '
  DBHMIN(7) = 6.0
  BFMIND(7) = 6.0
  SCFMIND(7) = 6.0
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
  RMAI = 50.0
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
  DGSD = 1.7
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
  IFOR = 1
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
  TLAT = 47.
  TLONG = 121.
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

  return
end
