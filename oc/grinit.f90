!=======================================!
!   Initialize growth model variables   !
!=======================================!
! OC $Id$
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
  include 'ORGANON.f90'

  ! Variables
  integer :: I,J,K
  character(len=26) :: DBLK
  character(len=4) :: none
  character(len=2) :: ANINDEX
  data DBLK/'                          '/
  data none/'NONE'/

  VARACD = 'OC'
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
     TOPD(I) = 0.
     DBHMIN(I) = 7.0
     FRMCLS(I) = 0.0
     METHB(I) = 999
     METHC(I) = 999
     BFSTMP(I) = 1.0
     BFTOPD(I) = 0.
     BFMIND(I) = 7.0
     SCFSTMP(I) = 1.0
     SCFTOPD(I) = 0.
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
  DBHMIN(11) = 6.0
  BFMIND(11) = 6.0
  SCFMIND(11) = 6.0
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
     IY(I) = 5
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
  DGSD = 0
  EFF = 1.0
  ELEV = 35.
  FINT = 5.
  FINTH = 5.
  FINTM = 5.0
  FPA = 300.
  IAGE = 0
  IWORK1(1) = 0

  !  ICL4 IS USED TO IMPLEMENT A NEW OPTION WHICH ALLOWS THE USER TO
  !  CONTROL THE NUMBER OF CYCLES A STAND WILL BE TRIPLED.
  !  ICL5 IS USED TO CARRY THE ACTUAL INPUT HABITAT CODE.

  ICL4 = 0
  ICL5 = 999
  ICL6 = 0
  IDG = 0
  IFINT = 5
  IFINTH = 5
  IFOR = 9
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
  TLAT = 42.
  TLONG = 124.
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
  IMODTY = 1
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

  !     INITIALIZE ORGANON VARIABLES

  LORGANON = .true. ! THE GROWTH AND MORTALITY DLL IS ACTIVE
  LORGVOLS = .false. ! THE VOLUME DLL IS NOT BEING USED.
  LORGPREP = .false. ! TREE DATA HAS NOT GONE THROUGH THE PREPARATION PROCESS
  VERSION = 1 ! ORGANON VERSION, DEFAULT SWO
  STAGE = 0 ! total stand age (even aged only)
  BHAGE = 0 ! breast height age
  SITE_1 = 0.0 ! the default site index dependant on variant
  SITE_2 = 0.0 ! the default site index dependant on variant
  MSDI_1 = 0.0
  MSDI_2 = 0.0
  MSDI_2 = 0.0

  ! INITIALIZE ORGANON CONTROL VARIABLE DEFAULTS

  INDS(1) = 1 ! HEIGHT CALIBRATION ON
  INDS(2) = 1 ! HEIGHT-TO-CROWN BASE CALIBRATION ON
  INDS(3) = 1 ! DIAMETER GROWTH CALIBRATION ON
  INDS(4) = 1 ! STAND IS EVEN-AGED
  INDS(5) = 0 ! ORGANON TRIPLING OFF
  INDS(6) = 0 ! STAND HAS NOT BEEN PRUNED
  INDS(7) = 0 ! STAND HAS NOT BEEN THINNED
  INDS(8) = 0 ! STAND HAS NOT BEEN FERTILIZED
  INDS(9) = 1 ! USE SDI-BASED MORTALITY
  INDS(10) = 0 ! WOOD QUALITY VARIABLES ARE NOT BEING COMPUTED.
  INDS(11) = 0 ! OVERSTORY TREES WERE NOT REMOVED AT START OF CURRENT CYCLE
  INDS(12) = 0 ! INGROWTH WAS NOT ADDED AT START OF GROWTH CYCLE
  INDS(13) = 0 ! MAJOR CONIFER TREES WERE NOT CUT AT BEGINNING OF CYCLE
  INDS(14) = 0 ! NOT PLANTED WITH GENETIC IMPROVED STOCK
  INDS(15) = 0 ! STAND IS NOT INFECTED WITH SWISS NEEDLE CAST
  do I = 16,30
     INDS(I) = 0
  end do

  ! INITIALIZE REAL VARIABLE ARRAYS, THESE GET ADJUSTED IN SITSET

  do I = 1,30
     RVARS(I) = 0.0
  end do

  !     FOR NOW, ASSIGN THE DBH, HEIGHT, AND CR CALIBRATIONS TO UNITY.
  !     TODO: THESE GET UPDATED FROM FVS.

  do I = 1,18
     ACALIB(1,I) = 1.0
     ACALIB(2,I) = 1.0
     ACALIB(3,I) = 1.0
  end do

  !     CLEAR OUT THE TREE VARIABLES
  !     THIS SHOULD ALSO BE PERFORMED EACH CYCLE AS THE TREE
  !     RECORDS ARE RE-INITIALIZED FROM THE NATIVE FVS ARRAYS

  do I = 1,2000
     IORG(I) = 0
     TREENO(I) = 0
     PTNO(I) = 0
     SPECIES(I) = 0
     DBH1(I) = 0.0
     HT1OR(I) = 0.0
     CR1(I) = 0.0
     SCR1B(I) = 0.0
     EXPAN1(I) = 0.0
     MGEXP(I) = 0.0
     USER(I) = 0
     TWARNING(I) = 0
  end do

  !     INITIALIZE THE VOLUME VARIABLES NOT NATIVE TO FVS
  !     THESE ONLY APPLY TO THE ORGANON SUBROUTINE VOLCAL_
  !     THE BF DEFAULTS CAN BE CHANGED USING THE ORGVOLS KEYWORD

  CFTD = 0.0 ! SOFTWOODS CF TOP DIAMETER, IN INCHES.
  CFSH = 0.0 ! SOFTWOODS CF STUMP HEIGHT, IN FEET.
  CFTDHW = 0.0 ! HARDWOODS CF TOP DIAMETER, IN INCHES.
  LOGTD = 6.0 ! BF LOG TOP DIAMETER, IN INCHES.
  LOGTA = 8.0 ! BF LOG TRIM, IN INCHES.
  LOGSH = 0.5 ! BF STUMP HEIGHT, IN FEET.
  LOGLL = 32.0 ! BF TARGET LOG LENGTH, IN FEET.
  LOGML = 8.0 ! BF MINIMUM LOG LENGTH, IN FEET.

  !     INITIALIZE THE STOR VARIABLES TO ZERO TO ENSURE
  !     THE VALUES ARE NOT CORRUPT FOR EVERY STAND

  do I = 1,30
     STOR(I) = 0.0
  end do

  !     NULL OUT THE THINNING VARIABLES.
  !     MOVE THE VARIABLES FROM THE PREVIOUS THINNINGS, IF THERE ARE ANY

  do I = 1,5
     YST(I) = 0.0
     BART(I) = 0.0
  end do
  BABT = 0.0

  !     NULL OUT THE FERTILIZATION VARIABLES TOO.

  do I = 1,5
     YSF(I) = 0.0
     PN(I) = 0.0
  end do

  !     SET THE VALUES NEEDED BY GROUP 3 EVENT MONITOR VARIABLES.

  OCC = 0.
  OAHT = 0.

  !  END OF ORGANON SECTION

  return
end
