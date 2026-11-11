!==================!
!   vie : cwcalc   !
!==================!
! VIE $Id$
subroutine CWCALC(ISPC,P,D,H,CR,IICR,CW,IWHO,JSTND2)
  implicit none

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'

  !  THIS ROUTINE CONTAINS A LIBRARY OF CROWN WIDTH EQUATIONS AVAILABLE
  !  FOR USE IN THE WESTERN UNITED STATES.
  !  EQUATIONS ARE GROUPED BY SPECIES ACCORDING TO THEIR FIA CODE.
  !  THIS ROUTINE COMPUTES LARGEST CROWN WIDTH.
  !  IT IS CALLED FROM **CWIDTH** TO PRODUCE CROWTH WIDTH ESTIMATES FOR
  !  OF FOREST GROWN TREES.

  !  DEFINITION OF VARIABLES:
  !        CW = LARGEST CROWN WIDTH
  !      IWHO = 0 IF CALLED FROM CWIDTH
  !      ISPC = FVS SPECIES SEQUENCE NUMBER
  !         P = TREES PER ACRE
  !         D = TREE DBH
  !        CR = CROWN RATIO IN PERCENT (REAL)
  !      IICR = CROWN RATIO IN PERCENT (INTEGER))
  !        CL = CROWN LENGTH
  !         H = TREE HEIGHT
  !     BAREA = BASAL AREA
  !     HILAT = LATITUDE IN DECIMAL DEGREES
  !    HILONG = LONGITUDE (-) IN DECIMAL DEGREES
  !    HIELEV = ELEVATION IN FEET
  !        HI = HOPKINS BIOCLIMATIC INDEX, SEE BECHTOLD 2003.
  !        EL = ELEVATION IN 100's OF FEET
  !       MIND= MIN. DBH FOR BECHTOLD EQN.
  !      OMIND= MIN. DBH FOR OTHER AUTHORS EQN.
  !      CWEQ = CW EQUATION NUMBER (FIA # + EQN #)
  !               BECHTOLD MODEL 1 EQN#      = 01
  !               BECHTOLD MODEL 2 EQN#      = 02
  !               CROOKSTON(R1) EQN#         = 03
  !               CROOKSTON(R6) MODEL 1 EQN# = 04
  !               CROOKSTON(R6) MODEL 2 EQN# = 05
  !               DONNELLY EQN#              = 06
  !               MOEUR EQN#                 = 07
  !               MIKE SHETTLES EQN#           = 08

  !  SOURCES OF FOREST GROWN CROWN WIDTH EQUATIONS:
  !  BECHTOLD, WILLIAM A. 2004. LARGEST-CROWN-DIAMETER PREDICTION MODELS FOR
  !     53 SPECIES IN THE WESTERN UNITED STATES. WJAF. 19(4):245-251.
  !  CROOKSTON, NICHOLAS 2005. DRAFT: ALLOMETRIC CROWN WIDTH EQUATIONS FOR 34
  !     NORTHWEST UNITED STATES TREE SPECIES ESTIMATED USING GENERALIZED LINEAR
  !     MIXED EFFECTS MODELS.
  !  CROOKSTON, NICHOLAS 2003. INTERNAL DOCUMENT ON FILE, MOSCOW IDAHO. DATA
  !     PROVIDED FROM REGION 1.
  !  DONNELLY, DENNIS 1996. INTERNAL DOCUMENT ON FILE, FORT COLLINS, CO. DATA
  !     PROVIDED FROM REGION 6.
  !  MOEUR, MELINDA 1981. CROWN WIDTH AND FOLIAGE WEIGHT OF NORTHERN
  !     ROCKY MOUNTAIN CONIFERS. USDA-FS, INT-183.
  !  CASTLE, MARK 2019. EQUATION DEVELOPED FOR 13 OF 23 NEW/UPDATED SPECIES
  !     FOR NEW AK VARIANT ONLY

  ! Variables
  logical :: DEBUG
  character(len=5) :: CWEQN
  character(len=3) :: FIASP
  character(len=5) :: AKMAP(23)
  character(len=5) :: BMMAP(18)
  character(len=5) :: CAMAP(50)
  character(len=5) :: CIMAP(19)
  character(len=5) :: CRMAP(38)
  character(len=5) :: ECMAP(32)
  character(len=5) :: EMMAP(19)
  character(len=5) :: IEMAP(23)
  character(len=5) :: KTMAP(11)
  character(len=5) :: NCMAP(12)
  character(len=5) :: NIMAP(11)
  character(len=5) :: PNMAP(39)
  character(len=5) :: SOMAP(33)
  character(len=5) :: TTMAP(18)
  character(len=5) :: UTMAP(24)
  character(len=5) :: WCMAP(39)
  character(len=5) :: WSMAP(43)
  character(len=5) :: OCMAP(50)
  character(len=5) :: OPMAP(39)
  character(len=5) :: BCMAP(15)
  integer :: ISPC,IICR,IWHO,JSTND2
  real    :: D,H,CW,HI,HILAT,HILONG,HIELEV,EL,MIND,CR,CL,BAREA
  real    :: BF,P,OMIND
  real    :: RDANUW
  integer :: IDANUW

  !  DATA STATEMENTS

  data MIND/5./,OMIND/1./

  !  MAP EQUATION NUMBERS FOR VARIANT

  !  SOUTHEAST ALASKA

  !                      SF       AF       YC   TAtoBE       WS   LStoWS
  data AKMAP/ '01105', '01905', '04205', '09508', '09408', '09408',&
       !             BE       SS       LP       RC       WH       MH   OStoWS
       &'09508', '09805', '10805', '24205', '26305', '26405', '09408',&
       !         ADtoCW       RA       PB   ABtoPB   BAtoCW       AS       CW
       &'74708', '35106', '37508', '37508', '74708', '74608', '74708',&
       !         WItoCW   SUtoCW   OHtoCW
       &'74708', '74708', '74708'/

  !  BLUE MOUNTAINS

  !                      WP       WL       DF       GF       MH       WJ
  data BMMAP/ '11905', '07303', '20205', '01703', '26403', '06405',&
       !                      LP       ES       AF       PP       WB       LM
       &'10805', '09305', '01905', '12205', '10105', '11301',&
       !                      PY       YC       AS       CW       OS       OH
       &'23104', '04205', '74605', '74705', '12205', '31206'/

  !  INLAND CALIFORNIA

  !                      PC       IC       RC       WF       RF      SH
  data CAMAP/ '04105', '08105', '24205', '01505', '02006', '02105',&
       !             DF       WH       MH       WB       KP       LP   CPtoLP
       &'20205', '26305', '26403', '10105', '10305', '10805', '10805',&
       !             LM       JP       SP       WP       PP   MPtoGP       GP
       &'11301', '11605', '11705', '11905', '12205', '12702', '12702',&
       !             WJ       BR   GStoRW       PY   OStoJP       LO       CY
       &'06405', '09204', '21104', '23104', '11605', '80102', '80502',&
       !             BL   EOtoBL       WO       BO       VO       IO       BM
       &'80702', '80702', '81505', '81802', '82102', '83902', '31206',&
       !         BUtoBM       RA       MA   GCtoTO   DGtoRA   FLtoBM   WNtoBM
       &'31206', '35106', '36102', '63102', '35106', '31206', '31206',&
       !             TO   SYtoTO       AS       CW   WItoBM   CNtoCL       CL
       &'63102', '63102', '74605', '74705', '31206', '98102', '98102',&
       !             OH       RW
       &'31206', '21104'/

  !  CENTRAL IDAHO

  data CIMAP/&
       !            WP       WL       DF       GF       WH
       &'11903',  '07303', '20203', '01703', '26305',&
       !            RC       LP       ES       AF       PP
       &'24203',  '10803', '09303', '01905', '12203',&
       !            WB       PY       AS       WJ       MC
       &'10105',  '23104', '74605', '06405', '47502',&
       !            LM       CW       OS       OH
       &'11301',  '74902', '12205', '74902'/

  !  CENTRAL ROCKIES

  !                      AF       CB       DF       GF       WF       MH
  data CRMAP/ '01905', '01801', '20205', '01703', '01505', '26403',&
       !             RC       WL       BC       LM       LP       PI       PP
       &'24205', '07303', '10201', '11301', '10805', '10602', '12205',&
       !             WB       SW       UJ       BS       ES       WS       AS
       &'10105', '11905', '06602', '09305', '09305', '09305', '74605',&
       !             NC       PW       GO       AW       EM       BK       SO
       &'74902', '74902', '81402', '81402', '81402', '81402', '81402',&
       !             PB       AJ       RM       OJ       ER       PM       PD
       &'74605', '06602', '06602', '06602', '06602', '10602', '10602',&
       !             AZ       CI       OS       OH
       &'10602', '12205', '12205', '74902'/

  !  EAST CASCADES

  data ECMAP/&
       !             WP       WL       DF       SF       RC
       &'11905', '07303', '20205', '01105', '24205',&
       !             GF       LP       ES       AF       PP
       &'01703', '10805', '09305', '01905', '12205',&
       !             WH       MH       PY       WB       NF
       &'26305', '26403', '23104', '10105', '02206',&
       !             WF       LL       YC       WJ       BM
       &'01505', '07204', '04205', '06405', '31206',&
       !             VN       RA       PB   GCtoTO   DGtoRA
       &'32102', '35106', '37506', '63102', '35106',&
       !             AS       CW       WO   PLtoRA   WItoBM
       &'74605', '74705', '81505', '35106', '31206',&
       !             OS       OH
       &'26403', '74605'/

  !  EASTERN MONTANA

  !                      WB       WL       DF       LM       LL       RM
  data EMMAP/ '10105', '07303', '20203', '11301', '07204', '06602',&
       !             LP       ES       AF       PP       GA       AS       CW
       &'10803', '09303', '01903', '12203', '74902', '74605', '74705',&
       !             BA       PW       NC       PB    OS(MH)      OH
       &'74902', '74902', '74902', '37506', '26405', '74902'/

  !  INLAND EMPIRE (23)

  !                      WP       WL       DF       GF       WH       RC
  data IEMAP/ '11903', '07303', '20203', '01703', '26303', '24203',&
       !             LP       ES       AF       PP      MH        WB       LM
       &'10803', '09303', '01903', '12203', '26405', '10105', '11301',&
       !             LL       PI       RM       PY       AS       CO       MM
       &'07204', '10602', '06602', '23104', '74605', '74902', '32102',&
       !             PB       OH       OS
       &'37506', '74902', '12205'/

  !  NORTHERN IDAHO (11)

  !                      WP       WL       DF       GF       WH       RC
  data NIMAP/ '11903', '07303', '20203', '01703', '26303', '24203',&
       !             LP       ES       AF       PP       OT(MH)
       &'10803', '09303', '01903', '12203', '26405'/

  !  BC (15)

  !                      PW       LW       FD       BG       HW       CW
  data BCMAP/ '11903', '07303', '20203', '01703', '26305', '24205',&
       !             PL       SE       BL       PY       EP       AT       AC
       &'10805', '09303', '01905', '12203', '37506', '74605', '74902',&
       !             OC       OH
       &'20203', '37506' /

  !  KOOKANTL

  !                      WP       WL       DF       GF       WH       RC
  data KTMAP/ '11903', '07303', '20203', '01703', '26303', '24203',&
       !             LP       ES       AF       PP       OT(MH)
       &'10803', '09303', '01903', '12203', '26405'/

  !  KLAMATH MOUNTAINS

  !                      OC       SP       DF       WF       MA       IC
  data NCMAP/ '12205', '11705', '20205', '01505', '36102', '08105',&
       !             BO       TO       RF       PP       OH       RW
       &'81802', '63102', '02006', '12205', '81802', '21104'/

  !  PACIFIC COAST

  !                      SF       WF       GF       AF       RF      SS
  data PNMAP/ '01105', '01505', '01703', '01905', '02006', '09805',&
       !             NF       YC       IC       ES       LP       JP       SP
       &'02206', '04205', '08105', '09305', '10805', '11605', '11705',&
       !             WP       PP       DF       RW       RC       WH       MH
       &'11905', '12205', '20205', '21104', '24205', '26305', '26403',&
       !             BM       RA   WAtoBM       PB   GCtoTO       AS       CW
       &'31206', '35106', '31206', '37506', '63102', '74605', '74705',&
       !             WO        J       LL       WB       KP       PY   DGtoRA
       &'81505', '06405', '07204', '10105', '10305', '23104', '35106',&
       !         HTtoRA   CHtoRA   WItoBM       --       OT
       &'35106', '35106', '31206', '12205', '12205'/

  !  SOUTHERN OREGON/ NORTHERN CALIF

  !                      WP       SP       DF       WF       MH       IC
  data SOMAP/ '11905', '11705', '20205', '01505', '26403', '08105',&
       !             LP       ES       SH       PP       WJ       GF       AF
       &'10805', '09305', '02105', '12205', '06405', '01703', '01905',&
       !             SF       NF       WB       WL       RC       WH       PY
       &'01105', '02206', '10105', '07303', '24205', '26305', '23104',&
       !         WAtoBM       RA       BM       AS       CW   CHtoRA       WO
       &'31206', '35106', '31206', '74605', '74705', '35106', '81505',&
       !         WItoBM   GCtoTO       MC   MBtoMC       OS       OH
       &'31206', '63102', '47502', '47502', '12205', '31206'/

  !  TETONS

  !                      WB       LM       DF       PM       BS       AS
  data TTMAP/ '10105', '11301', '20205', '10602', '09305', '74605',&
       !             LP       ES       AF       PP       UJ       RM       BI
       &'10805', '09305', '01905', '12203', '06405', '06405', '31206',&
       !             MM       NC       MC       OS       OH
       &'32102', '74902', '47502', '12205', '74902'/

  !  UTAH

  !                      WB       LM       DF       WF       BS       AS
  data UTMAP/ '10105', '11301', '20205', '01505', '09305', '74605',&
       !             LP       ES       AF       PP       PI       WJ       GO
       &'10805', '09305', '01905', '12205', '10602', '06405', '81402',&
       !             PM       RM       UJ       GB       NC       FC       MC
       &'10602', '06405', '06405', '10201', '74902', '74902', '47502',&
       !             BI       BE       OS       OH
       &'31206', '74902', '12205', '81402'/

  !  WEST CASCADES

  !                      SF       WF       GF       AF       RF      SS
  data WCMAP/ '01105', '01505', '01703', '01905', '02006', '09805',&
       !             NF       YC       IC       ES       LP       JP       SP
       &'02206', '04205', '08105', '09305', '10805', '11605', '11705',&
       !             WP       PP       DF       RW       RC       WH       MH
       &'11905', '12205', '20205', '21104', '24205', '26305', '26403',&
       !             BM       RA   WAtoBM       PB   GCtoTO       AS       CW
       &'31206', '35106', '31206', '37506', '63102', '74605', '74705',&
       !             WO       WJ       LL       WB       KP       PY   DGtoRA
       &'81505', '06405', '07204', '10105', '10305', '23104', '35106',&
       !         HTtoRA   CHtoRA   WItoBM       --       OT
       &'35106', '35106', '31206', '12205', '12205'/

  !  WESTERN SIERRAS

  data WSMAP/&
       !          SP       DF       WF       GS       IC
       &'11705', '20205', '01505', '21104', '08105',&
       !          JP       RF       PP       LP       WB
       &'11605', '02006', '12205', '10805', '10105',&
       !          WP       PM       SF       KP       FP
       &'11905', '10602', '01105', '10305', '10305',&
       !          CP       LM       MP       GP       WE
       &'10805', '11301', '12205', '12702', '10305',&
       !          GB       BD       RW       MH       WJ
       &'10201', '20205', '21104', '26403', '06405',&
       !          UJ       CJ       LO       CY       BL
       &'06405', '06405', '80102', '80502', '80702',&
       !          BO       VO       IO       TO       GC
       &'81802', '82102', '83902', '63102', '63102',&
       !          AS       CL       MA       DG       BM
       &'74605', '98102', '36102', '35106', '31206',&
       !          MC       OS       OH
       &'47502', '12205', '81802'/

  !  FVS-ORGANON SWO  (OC VARIANT)

  !                      PC       IC       RC       GF       RF      SH
  data OCMAP/ '04105', '08105', '24205', '01703', '02006', '02105',&
       !             DF       WH       MH       WB       KP       LP   CPtoLP
       &'20205', '26305', '26403', '10105', '10305', '10805', '10805',&
       !             LM       JP       SP       WP       PP   MPtoGP       GP
       &'11301', '11605', '11705', '11905', '12205', '12702', '12702',&
       !             WJ       BR   GStoRW       PY   OStoJP       LO       CY
       &'06405', '09204', '21104', '23104', '11605', '80102', '80502',&
       !             BL   EOtoBL       WO       BO       VO       IO       BM
       &'80702', '80702', '81505', '81802', '82102', '83902', '31206',&
       !         BUtoBM       RA       MA   GCtoTO   DGtoRA   FLtoBM   WNtoBM
       &'31206', '35106', '36102', '63102', '35106', '31206', '31206',&
       !             TO   SYtoTO       AS       CW   WItoBM   CNtoCL       CL
       &'63102', '63102', '74605', '74705', '31206', '98102', '98102',&
       !             OH       RW
       &'31206', '21104'/

  !  FVS-ORGANON NWO & SMC  (OP VARIANT)

  !                      SF       WF       GF       AF       RF      SS
  data OPMAP/ '01105', '01505', '01703', '01905', '02006', '09805',&
       !             NF       YC       IC       ES       LP       JP       SP
       &'02206', '04205', '08105', '09305', '10805', '11605', '11705',&
       !             WP       PP       DF       RW       RC       WH       MH
       &'11905', '12205', '20205', '21104', '24205', '26305', '26403',&
       !             BM       RA       MA       TO   GCtoTO       AS       CW
       &'31206', '35106', '36102', '63102', '63102', '74605', '74705',&
       !             WO        J       LL       WB       KP       PY   DGtoRA
       &'81505', '06405', '07204', '10105', '10305', '23104', '35106',&
       !         HTtoRA   CHtoRA   WItoBM       --       OT
       &'35106', '35106', '31206', '12205', '12205'/

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  IDANUW = IICR
  IDANUW = IWHO
  RDANUW = P

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'CWCALC',6,ICYC)

  !  SET THE EQUATION NUMBER
  !  OR IF THIS IS AN R5 FOREST BRANCH TO THE R5CRWD ROUTINE

  !  NOTES FOR NON-FS FOREST CODES (ALSO SEE SUBROUTINE **FORKOD**):
  !  SO VARIANT IFOR=8=INDUSTRY IS IN R5
  !             IFOR=10=WARM SPRINGS RESERVATION IS IN R6
  !  NC VARIANT IFOR=5=HOOPA IS IN R5; IFOR=6=SIMPSON TIMBER IS IN R6;
  !                 IFOR=7=BLM COOS BAY IS IN R6
  !  PN&OP VARIANTS IFOR=3=QUINAULT IS IN R6; IFOR=4,5,6=BLM SALEM,EUGENE,COOS BAY
  !                 ARE IN R6
  !  WC VARIANT IFOR=7,8,9,10=BLM SALEM,EUGENE,ROSEBURG,MEDFORD ARE IN R6
  !  CA VARIANT IFOR=8,9,10=BLM ROSEBURG,MEDFORD, COOS BAY ARE IN R6

  !  ALSO BECAUSE OF FOREST CODE MAPPING IN **FORKOD**, IFOR WILL BE:
  !  BM VARIANT  .LE. 4
  !  CA VARIANT  .LE. 10
  !  EC VARIANT  .LE. 4
  !  NC VARIANT  .LE. 7
  !  SO VARIANT  .LE. 10 BUT NOT 9
  !  WC VARIANT  .LE. 10
  !  WS VARIANT  .LE. 6
  !  MAPPING IS ALWAYS WITHIN REGIONAL BOUNDARIES.

  if ((VARACD.eq.'SO').and.(IFOR.ge.4.and. IFOR.le.9)) then
     call R5CRWD(ISPC,D,H,CW)
     go to 9000
  else if (VARACD.eq.'WS') then
     call R5CRWD(ISPC,D,H,CW)
     go to 9000
  else if ((VARACD.eq.'NC').and.((IFOR.le.3).or.(IFOR.eq.5))) then
     call R5CRWD(ISPC,D,H,CW)
     go to 9000
  else if (((VARACD.eq.'CA') .or. (VARACD.eq.'OC')).and.&
       &(IFOR.le.5)) then
     call R5CRWD(ISPC,D,H,CW)
     go to 9000
  end if

  select case (VARACD)
  case('AK')
     CWEQN = AKMAP(ISPC)
  case('BM')
     CWEQN = BMMAP(ISPC)
  case('CA')
     CWEQN = CAMAP(ISPC)
  case('CI')
     CWEQN = CIMAP(ISPC)
  case('CR')
     CWEQN = CRMAP(ISPC)
  case('SM')
     CWEQN = CRMAP(ISPC)
  case('SP')
     CWEQN = CRMAP(ISPC)
  case('BP')
     CWEQN = CRMAP(ISPC)
  case('SF')
     CWEQN = CRMAP(ISPC)
  case('LP')
     CWEQN = CRMAP(ISPC)
  case('EC')
     CWEQN = ECMAP(ISPC)
  case('EM')
     CWEQN = EMMAP(ISPC)
  case('BC')
     CWEQN = BCMAP(ISPC)
  case('IE')
     CWEQN = IEMAP(ISPC)
  case('KT')
     CWEQN = KTMAP(ISPC)
  case('NC')
     CWEQN = NCMAP(ISPC)
  case('NI')
     CWEQN = NIMAP(ISPC)
  case('OC')
     CWEQN = OCMAP(ISPC)
  case('OP')
     CWEQN = OPMAP(ISPC)
  case('PN')
     CWEQN = PNMAP(ISPC)
  case('SO')
     CWEQN = SOMAP(ISPC)
  case('TT')
     CWEQN = TTMAP(ISPC)
  case('UT')
     CWEQN = UTMAP(ISPC)
  case('WC')
     CWEQN = WCMAP(ISPC)
  case('WS')
     CWEQN = WSMAP(ISPC)
  end select

  !      IF(DEBUG)WRITE(JSTND2,*)
  !     &' ENTERING CWCALC: ISPC,CWEQN,VARACD= ',ISPC,CWEQN,VARACD
  !      IF(DEBUG)WRITE(JSTND2,*)' ISPC,P,D,H,CR,IICR,CW,IWHO,JSTND2= ',
  !     &ISPC,P,D,H,CR,IICR,CW,IWHO,JSTND2
  !      IF(DEBUG)WRITE(JSTND2,*)' ISPC,IFOR,KODFOR= ',ISPC,IFOR,KODFOR

  ! SET R6 FOREST SPECIFIC CONSTANTS FOR CROOKSTON(R6) MODELS

  ! NOTES:
  ! CA/OC VARIANTS  710 BLM ROSEBURG USES 610 ROGUE RIVER
  !                 711 BLM MEDFORD USES 610 ROGUE RIVER
  !                 712 BLM COOS BAY USES 611 SISKIYOU
  ! NC VARIANT      712 BLM COOS BAY USES 611 SISKIYOU
  !                 800 SIMPSON TIMBER USES 611 SISKIYOU
  ! PN/OP VARIANTS  708 BLM SALEM USES 606 MT HOOD
  !                 709 BLM EUGENE USES 618 WILLAMETTE
  !                 712 BLM COOS BAY USES 611 SISKIYOU
  ! SO VARIANT      799 WARM SPRINGS RESERVATION USES DESCHUTES
  ! WC VARIANT      708 BLM SALEM USES 606 MT HOOD
  !                 709 BLM EUGENE USES 618 WILLAMETTE
  !                 710 BLM ROSEBURG USES 610 ROGUE RIVER
  !                 711 BLM MEDFORD USES 610 ROGUE RIVER

  FIASP = CWEQN(1:3)
  BF = 1.0

  ! SKIP THIS SECTION FOR FORESTS IN REGIONS 1,2,3,4,5,10
  ! THIS VERSION OF CWCALC IS NOT USED IN REGIONS 8 OR 9
  ! NON-FS AND NON-BLM CODES 7xx WHICH USE **R5CRWD** FALL THROUGH THIS SECTION
  ! THE CODE FOR NC FOREST CODE 800 IS BECAUSE THERE IS ALSO A FOREST CODE 800
  ! IN THE PN AND OP VARIANTS. NC 800 USES SISKIYOU SETTINGS. PN/OP 800 USES
  ! OLYMPIC SETTINGS

  if (KODFOR .lt.601.or. KODFOR.ge.1000)go to 10
  if (VARACD.eq.'NC' .and. KODFOR.eq.800) then
     select case (FIASP) !USE 611=SISKIYOU VALUES
     case('351')
        BF = 0.810
     case('081')
        BF = 0.821
     case('108')
        BF = 0.944
     case('122')
        BF = 0.951
     case('202')
        BF = 0.961
     case('242')
        BF = 0.973
     case('263')
        BF = 1.028
     case('264')
        BF = 0.900
     end select
     go to 10
  end if

  select case (KODFOR)
  case(601, 799) !DESCHUTES
     select case (FIASP)
     case('015')
        BF = 1.044
     case('019')
        BF = 0.936
     case('022')
        BF = 1.301
     case('081')
        BF = 0.837
     case('073')
        BF = 0.818
     case('117')
        BF = 1.048
     case('122')
        BF = 0.918
     case('202')
        BF = 1.055
     case('263')
        BF = 1.097
     end select
  case(602) !FREMONT
     select case (FIASP)
     case('011')
        BF = 1.032
     case('108')
        BF = 1.114
     case('119')
        BF = 1.090
     case('122')
        BF = 0.946
     case('264')
        BF = 1.257
     end select
  case(603) !GIFFORD PINCHOT
     select case (FIASP)
     case('011')
        BF = 1.032
     case('019')
        BF = 0.906
     case('022')
        BF = 1.123
     case('073')
        BF = 0.952
     case('119')
        BF = 1.128
     case('242')
        BF = 0.920
     case('263')
        BF = 1.028
     case('264')
        BF = 1.077
     end select
  case(604) !MALHEUR
     select case (FIASP)
     case('019')
        BF = 1.110
     case('073')
        BF = 0.818
     case('108')
        BF = 1.196
     case('093')
        BF = 1.121
     case('119')
        BF = 1.081
     case('202')
        BF = 1.058
     end select
  case(605) !MT BAKER-SNOQUALMIE
     select case (FIASP)
     case('019')
        BF = 0.886
     case('022')
        BF = 1.075
     case('073')
        BF = 0.907
     case('093')
        BF = 0.949
     case('119')
        BF = 1.081
     case('202')
        BF = 1.019
     case('242')
        BF = 0.973
     end select
  case(606, 708) !MT HOOD, BLM SALEM
     select case (FIASP)
     case('011')
        BF = 1.296
     case('015')
        BF = 1.130
     case('017')
        BF = 1.086
     case('019')
        BF = 1.038
     case('022')
        BF = 1.301
     case('042')
        BF = 1.493
     case('073')
        BF = 0.907
     case('108')
        BF = 0.944
     case('119')
        BF = 1.081
     case('242')
        BF = 1.115
     case('263')
        BF = 1.260
     case('264')
        BF = 1.106
     end select
  case(607) !OCHOCO
     select case (FIASP)
     case('019')
        BF = 1.110
     case('073')
        BF = 0.879
     case('108')
        BF = 1.196
     case('093')
        BF = 1.169
     case('202')
        BF = 1.055
     end select
  case(608) !OKANOGAN
     select case (FIASP)
     case('073')
        BF = 0.952
     case('108')
        BF = 1.114
     case('119')
        BF = 1.081
     case('242')
        BF = 0.905
     case('264')
        BF = 0.900
     end select
  case(609, 800) !OLYMPIC, QUINAULT IR
     select case (FIASP)
     case('011')
        BF = 1.032
     case('108')
        BF = 1.114
     case('098')
        BF = 1.146
     case('242')
        BF = 0.941
     end select
  case(610, 710, 711) !ROGUE RIVER, BLM ROSEBURG AND MEDFORD
     select case (FIASP)
     case('019')
        BF = 0.886
     case('351')
        BF = 0.810
     case('081')
        BF = 0.903
     case('108')
        BF = 0.944
     case('093')
        BF = 0.949
     case('117')
        BF = 1.048
     case('119')
        BF = 1.081
     case('122')
        BF = 0.918
     case('264')
        BF = 0.900
     end select
  case(611, 712) !SISKIYOU, BLM COOS BAY
     select case (FIASP)
     case('351')
        BF = 0.810
     case('081')
        BF = 0.821
     case('108')
        BF = 0.944
     case('122')
        BF = 0.951
     case('202')
        BF = 0.961
     case('242')
        BF = 0.973
     case('263')
        BF = 1.028
     case('264')
        BF = 0.900
     end select
  case(612) !SIUSLAW
     select case (FIASP)
     case('202')
        BF = 0.977
     case('242')
        BF = 0.905
     case('263')
        BF = 0.924
     end select
  case(614) !UMATILLA
     select case (FIASP)
     case('017')
        BF = 1.076
     case('019')
        BF = 1.110
     case('073')
        BF = 0.907
     case('108')
        BF = 1.244
     case('093')
        BF = 1.137
     case('117')
        BF = 1.097
     case('119')
        BF = 1.128
     case('122')
        BF = 1.035
     case('202')
        BF = 1.055
     case('242')
        BF = 1.055
     case('263')
        BF = 1.106
     end select
  case(615) !UMPQUA
     select case (FIASP)
     case('011')
        BF = 1.032
     case('015')
        BF = 1.130
     case('022')
        BF = 1.043
     case('042')
        BF = 1.295
     case('108')
        BF = 1.050
     case('093')
        BF = 1.325
     case('117')
        BF = 1.097
     case('119')
        BF = 1.128
     case('122')
        BF = 1.035
     case('202')
        BF = 1.055
     case('242')
        BF = 1.049
     case('263')
        BF = 1.106
     end select
  case (616) !WALLOWA-WHITMAN
     select case (FIASP)
     case('073')
        BF = 0.818
     case('108')
        BF = 1.114
     case('093')
        BF = 1.070
     case('264')
        BF = 1.077
     end select
  case(617) !WENATCHEE
     select case (FIASP)
     case('017')
        BF = 0.972
     case('019')
        BF = 0.906
     case('073')
        BF = 0.879
     case('108')
        BF = 0.969
     case('093')
        BF = 0.949
     case('117')
        BF = 1.097
     case('122')
        BF = 0.946
     case('202')
        BF = 0.975
     case('242')
        BF = 0.905
     case('263')
        BF = 0.962
     case('264')
        BF = 0.952
     end select
  case(618, 709) !WILLAMETTE, BLM EUGENE
     select case (FIASP)
     case('017')
        BF = 0.972
     case('019')
        BF = 0.936
     case('042')
        BF = 1.127
     case('108')
        BF = 0.903
     case('093')
        BF = 0.857
     case('117')
        BF = 1.097
     case('119')
        BF = 1.081
     case('122')
        BF = 1.070
     case('263')
        BF = 1.087
     end select
  case(620) !WINEMA
     select case (FIASP)
     case('015')
        BF = 1.095
     case('022')
        BF = 1.043
     case('108')
        BF = 1.050
     case('117')
        BF = 1.048
     case('119')
        BF = 1.090
     case('122')
        BF = 0.951
     case('202')
        BF = 1.184
     case('264')
        BF = 1.077
     end select
  case(621) !COLVILLE
     select case (FIASP)
     case('017')
        BF = 1.130
     case('019')
        BF = 1.038
     case('108')
        BF = 1.216
     case('093')
        BF = 1.137
     case('119')
        BF = 1.206
     case('122')
        BF = 1.035
     case('202')
        BF = 1.055
     case('242')
        BF = 0.973
     case('263')
        BF = 1.097
     end select
  end select

  !  INITIALIZE RETURN VARIABLES.

  10 continue
  CW = 0.

  ! SET OTHER VARIABLES FOR CROWN MODELS.

  CL = CR*H*0.01
  BAREA = BA
  if (BAREA .le.1.) BAREA = 1.
  HILAT = TLAT
  HILONG = -1*ABS(TLONG)
  HIELEV = ELEV*100
  EL = ELEV
  !  COMPUTE HOPKINS INDEX
  HI = ((HIELEV-5449.)/100.)*1.0 + (HILAT-42.16)*4.0 +&
       &(-116.39 - HILONG)*1.25

  if (DEBUG)write(JSTND2,*)' IN CWCALC: ISPC,D,VARACD,CWEQN= ',&
       &ISPC,D,VARACD,CWEQN

  !  CALCULATE CROWN WIDTH

  select case (CWEQN)

     !  CASE 01102 BECHTOLD 2004 MODEL 2
     !  011          ABIES AMABILIS                     PACIFIC SILVER FIR
  case('01102')
     if (HI .lt.  -9.) HI = -9.
     if (HI .gt.26.) HI = 26.
     if (D .ge. MIND) then
        CW = 7.7763 + (0.5960*D) + (-0.0705*HI)
     else
        CW = (7.7763 + (0.5960*MIND) + (-0.0705*HI))*(D/MIND)
     end if
     if (CW .gt.33.) CW = 33.

     !  CASE 01105 CROOKSTON (R6) MODEL 2
     !  011          ABIES AMABILIS                     PACIFIC SILVER FIR
  case('01105')
     if (EL .lt.4.) EL = 4.
     if (EL .gt.72.) EL = 72.
     if (D .ge. OMIND) then
        CW = 4.4799*BF*(D**0.45976)*(H**(-0.10425))*(CL**0.11866)*&
             &((BAREA+1.0)**0.06762)*(EXP(EL)**(-0.00715))
     else
        CW = (4.4799*BF*(OMIND**0.45976)*(H**(-0.10425))*&
             &(CL**0.11866)*((BAREA+1.0)**0.06762)*(EXP(EL)**(-0.00715)))&
             &*(D/OMIND)
     end if
     if (CW .gt.33.) CW = 33.

     !  CASE 01502 BECHTOLD 2004 MODEL 2
     !  015          ABIES CONCOLOR                     WHITE FIR
  case('01502')
     if (HI .lt. -40.) HI = -40.
     if (HI .gt.19.) HI = 19.
     if (D .ge. MIND) then
        CW = 2.4789 + (0.9317*D) + (- 0.0128*D*D) + (0.0327*CR) +&
             &(-0.1178*HI)
     else
        CW = (2.4789 + (0.9317*MIND) + (-0.0128*D*D) + (0.0327*CR) +&
             &(-0.1178*HI))*(D/MIND)
     end if
     if (CW .gt.35.) CW = 35.

     !  CASE 01505 CROOKSTON (R6) MODEL 2
     !  015          ABIES CONCOLOR                     WHITE FIR
  case('01505')
     if (EL .lt.2.) EL = 2.
     if (EL .gt.75.) EL = 75.
     if (D .ge. OMIND) then
        CW = 5.0312*BF*(D**0.53680)*(H**(-0.18957))*(CL**0.16199)*&
             &((BAREA+1.0)**0.04385)*(EXP(EL)**(-0.00651))
     else
        CW = (5.0312*BF*(OMIND**0.53680)*(H**(-0.18957))*&
             &(CL**0.16199)*((BAREA+1.0)**0.04385)*(EXP(EL)**(-0.00651)))&
             &*(D/OMIND)
     end if
     if (CW .gt.35.) CW = 35.

     !  CASE 01506 DONNELLY (R6)
     !  015          ABIES CONCOLOR                     WHITE FIR
  case('01506')
     if (D .ge. OMIND) then
        CW = 3.8166*D**0.5229
     else
        CW = (3.8166*OMIND**0.5229)*(D/OMIND)
     end if
     if (CW .gt.35.) CW = 35.

     !  CASE 01702 BECHTOLD 2004 MODEL 2
     !  017          ABIES GRANDIS                      GRAND FIR
  case('01702')
     if (HI .lt. -48.) HI = -48.
     if (HI .gt.20.) HI = 20.
     if (D .ge. MIND) then
        CW = 3.0335 + (0.9752*D) + (-0.0113*D*D) + (0.0548*CR) +&
             &(-0.0597*HI)
     else
        CW = (3.0335 + (0.9752*MIND) + (-0.0113*D*D) + (0.0548*CR) +&
             &(-0.0597*HI))*(D/MIND)
     end if
     if (CW .gt.40.) CW = 40.

     !  CASE 01703 CROOKSTON (R1)
     !  017          ABIES GRANDIS                      GRAND FIR
  case('01703')
     if (D .ge.1.0) then
        CW = 1.0303*EXP(1.14079 + 0.20904*ALOG(CL)+0.38787*ALOG(D))
     else
        CW = (1.0303*EXP(1.14079 + 0.20904*ALOG(CL)+&
             &0.38787*ALOG(1.0))) * (D/1.0)
     end if
     if (CW .gt.40.) CW = 40.

     !  CASE 01705 CROOKSTON (R6) MODEL 2
     !  017          ABIES GRANDIS                      GRAND FIR
  case('01705')
     if (EL .lt.3.) EL = 3.
     if (EL .gt.75.) EL = 75.
     if (D .ge. OMIND) then
        CW = 6.0231*BF*(D**0.54674)*(H**(-0.19451))*(CL**0.15375)&
             &* ((BAREA+1.0)**0.02897)*(EXP(EL)**(-0.00512))
     else
        CW = (6.0231*BF*(OMIND**0.54674)*(H**(-0.19451))*&
             &(CL**0.15375)*((BAREA+1.0)**0.02897)*(EXP(EL)**(-0.00512)))&
             &*(D/OMIND)
     end if
     if (CW .gt.40.) CW = 40.

     !  CASE 01706 DONNELLY (R6)
     !  017          ABIES GRANDIS                      GRAND FIR
  case('01706')
     if (D .ge. OMIND) then
        CW = 4.1870*D**0.5341
     else
        CW = (4.1870*OMIND**0.5341)*(D/OMIND)
     end if
     if (CW .gt.40.) CW = 40.

     !  CASE 01707 MOEUR
     !  017          ABIES GRANDIS                      GRAND FIR
  case('01707')
     CW = EXP(2.20611+ 1.08137*ALOG(D) + (-0.76936)*ALOG(H)&
          &+ 0.29786*ALOG(CL))
     if (CW .gt.40.) CW = 40.

     !  CASE 01801 BECHTOLD 2004 MODEL 1
     !  018          ABIES LASIOCARPA var. ARIZONICA    CORKBARK FIR
  case('01801')
     if (D .ge. MIND) then
        CW = 6.073 + 0.3756*D
     else
        CW = (6.073 + 0.3756*MIND)*(D/MIND)
     end if
     if (CW .gt.15.) CW = 15.

     !  CASE 01901 BECHTOLD 2004 MODEL 2
     !  019          ABIES LASIOCARPA                   SUBALPINE FIR
  case('01901')
     if (HI .lt. -14.) HI = -14.
     if (HI .gt.44.) HI = 44.
     if (D .ge. MIND) then
        CW = 2.6068 + 0.6145*D + 0.0417*CR + (-0.0698)*HI
     else
        CW = (2.6068+0.6145*MIND + 0.0417*CR+(-0.0698*HI))*(D/MIND)
     end if
     if (CW .gt.30.) CW = 30.

     !  CASE 01903 CROOKSTON (R1)
     !  019          ABIES LASIOCARPA                   SUBALPINE FIR
  case('01903')
     if (D .ge.0.1) then
        CW = 1.02886*EXP(1.01255 + 0.30374*ALOG(CL)+0.37093*ALOG(D)&
             &+ (-0.13731*ALOG(H)))
     else
        CW = (1.02886*EXP(1.01255 + 0.30374*ALOG(CL) +&
             &0.37093*ALOG(0.1) + (-0.13731*ALOG(H))))*(D/0.1)
     end if
     if (CW .gt.30.) CW = 30.

     !  CASE 01905 CROOKSTON (R6) MODEL 2
     !  019          ABIES LASIOCARPA                   SUBALPINE FIR
  case('01905')
     if (EL .lt.10.) EL = 10.
     if (EL .gt.85.) EL = 85.
     if (D .ge. OMIND) then
        CW = 5.8827*BF*(D**0.51479)*(H**(-0.21501))*(CL**0.17916)*&
             &((BAREA+1.0)**0.03277)*(EXP(EL)**(-0.00828))
     else
        CW = (5.8827*BF*(OMIND**0.51479)*(H**(-0.21501))*&
             &(CL**0.17916)*((BAREA+1.0)**0.03277)*(EXP(EL)**(-0.00828)))&
             &*(D/OMIND)
     end if
     if (CW .gt.30.) CW = 30.

     !  CASE 01906 DONNELLY (R6)
     !  019          ABIES LASIOCARPA                   SUBALPINE FIR
  case('01906')
     if (D .ge. OMIND) then
        CW = 3.2348*D**0.5179
     else
        CW = (3.2348*OMIND**0.5179)*(D/OMIND)
     end if
     if (CW .gt.30.) CW = 30.

     !  CASE 01907 MOEUR
     !  019          ABIES LASIOCARPA                   SUBALPINE FIR
  case('01907')
     CW = EXP(1.74558 + (1.08137*ALOG(D)) + (-0.73972*ALOG(H))&
          &+ (0.29786*ALOG(CL)))
     if (CW .gt.30.) CW = 30.

     !  CASE 02002 BECHTOLD 2004 MODEL 2
     !  020          ABIES MAGNIFICA VAR. MAGNIFICA   CALIFORNIA RED FIR
  case('02002')
     if (HI .lt. -14.) HI = -14.
     if (HI .gt.44.) HI = 44.
     if (D .ge. MIND) then
        CW = 2.3660 + 0.5472*D + 0.0316*CR + (-0.0702)*HI
     else
        CW = (2.3660+0.5472*MIND+0.0316*CR+(-0.0702*HI))*(D/MIND)
     end if
     if (CW .gt.36.) CW = 36.

     !  CASE 02006 DONNELLY (R6)
     !  020          ABIES MAGNIFICA VAR. MAGNIFICA   CALIFORNIA RED FIR
  case('02006')
     if (D .ge. OMIND) then
        CW = 3.1146*D**0.5780
     else
        CW = (3.1146*OMIND**0.5780)*(D/OMIND)
     end if
     if (CW .gt.65.) CW = 65.

     !  CASE 02101 BECHTOLD 2004 MODEL 2
     !  021          ABIES MAGNIFICA VAR. SHASTENSIS    SHASTA RED FIR
  case('02101')
     if (D .ge. MIND) then
        CW = 4.0524 + 0.6423*D
     else
        CW = (4.0524 + 0.6423*MIND)*(D/MIND)
     end if
     if (CW .gt.26.) CW = 26.

     !  CASE 02105 CROOKSTON (R6) MODEL 2
     !  021          ABIES MAGNIFICA VAR. SHASTENSIS    SHASTA RED FIR
  case('02105')
     if (D .ge. OMIND) then
        CW = 2.3170*BF*(D**0.47880)*(H**(-0.06093))*(CL**0.15482)*&
             &((BAREA+1.0)**0.05182)
     else
        CW = (2.3170*BF*(OMIND**0.47880)*(H**(-0.06093))&
             &*(CL**0.15482)*((BAREA+1.0)**0.05182))*(D/OMIND)
     end if
     if (CW .gt.65.) CW = 65.

     !  CASE 02201 BECHTOLD 2004 MODEL 2
     !  022          ABIES PROCERA                      NOBLE FIR
  case('02201')
     if (HI .lt. -11.) HI = -11.
     if (HI .gt.32.) HI = 32.
     if (D .ge. MIND) then
        CW = 2.7761 + 0.7311*D + 0.0476*CR + (-0.0756*HI)
     else
        CW = (2.7761 + 0.7311*MIND + 0.0476*CR + (-0.0756*HI))*&
             &(D/MIND)
     end if
     if (CW .gt.29.) CW = 29.

     !  CASE 02206 DONNELLY (R6)
     !  022          ABIES PROCERA                      NOBLE FIR
  case('02206')
     if (D .ge. OMIND) then
        CW = 3.0614*D**0.6276
     else
        CW = (3.0614*OMIND**0.6276)*(D/OMIND)
     end if
     if (CW .gt.40.) CW = 40.

     !  CASE 04102 BECHTOLD 2004 MODEL 2
     !  041          CHAMAECYPARIS LAWSONIANA           PORT ORFORD CEDAR
  case('04102')
     if (D .ge. MIND) then
        CW = 1.0365 + (0.7943*D) + (0.0399*CR)
     else
        CW = (1.0365 + (0.7943*MIND) + (0.0399*CR))*(D/MIND)
     end if
     if (CW .gt.22.) CW = 22.

     !  CASE 04105 CROOKSTON (R6) MODEL 2
     !  041          CHAMAECYPARIS LAWSONIANA           PORT ORFORD CEDAR
  case('04105')
     if (EL .lt.2.) EL = 2.
     if (EL .gt.52.) EL = 52.
     if (D .ge. OMIND) then
        CW = 4.6387*BF*(D**0.50874)*(H**(-0.22111))*(CL**0.1755)*&
             &((BAREA+1.0)**0.06447)*(EXP(EL)**(-0.00602))
     else
        CW = (4.6387*BF*(OMIND**0.50874)*(H**(-0.22111))*&
             &(CL**0.1755)*((BAREA+1.0)**0.06447)*(EXP(EL)**(-0.00602)))*&
             &(D/OMIND)
     end if
     if (CW .gt.49.) CW = 49.

     !  CASE 04106 DONNELLY (R6)
     !  041          CHAMAECYPARIS LAWSONIANA           PORT ORFORD CEDAR
  case('04106')
     if (D .ge. OMIND) then
        CW = 5.3864*D**0.4213
     else
        CW = (5.3864*OMIND**0.4213)*(D/OMIND)
     end if
     if (CW .gt.35.) CW = 35.

     !  CASE 04205 CROOKSTON (R6) MODEL 2
     !  042          CHAMAECYPARIS NOOTKATENSIS          ALASKA YELLOW CEDAR
  case('04205')
     if (EL .lt.16.) EL = 16.
     if (EL .gt.62.) EL = 62.
     if (D .ge. OMIND) then
        CW = 3.3756*BF*(D**0.45445)*(H**(-0.11523))*(CL**0.22547)*&
             &((BAREA+1.0)**0.08756)*(EXP(EL)**(-0.00894))
     else
        CW = (3.3756*BF*(OMIND**0.45445)*(H**(-0.11523))*&
             &(CL**0.22547)*((BAREA+1.0)**0.08756)*(EXP(EL)**(-0.00894)))*&
             &(D/OMIND)
     end if
     if (CW .gt.59.) CW = 59.

     !  CASE 04206 DONNELLY (R6)
     !  042          CHAMAECYPARIS NOOTKATENSIS          ALASKA YELLOW CEDAR
  case('04206')
     if (D .ge. OMIND) then
        CW = 3.5341*D**0.5374
     else
        CW = (3.5341*OMIND**0.5374)*(D/OMIND)
     end if
     if (CW .gt.30.) CW = 30.

     !  CASE 06402 BECHTOLD 2004 MODEL 2
     !  064          JUNIPERUS OCCIDENTALIS              WESTERN JUNIPER
  case('06402')
     if (D .ge. MIND) then
        CW = -0.0037 + (1.3526*D) + (-0.0165*D*D)
     else
        CW = (-0.0037+(1.3526*MIND)+(-0.0165*MIND*MIND))*(D/MIND)
     end if
     if (CW .gt.36.) CW = 36.

     !  CASE 06405 CROOKSTON (R6) MODEL 2
     !  064          JUNIPERUS OCCIDENTALIS              WESTERN JUNIPER
  case('06405')
     if (D .ge. OMIND) then
        CW = 5.1486*BF*(D**0.73636)*(H**(-0.46927))*(CL**0.39114)*&
             &((BAREA+1.0)**(-0.05429))
     else
        CW = (5.1486*BF*(OMIND**0.73636)*(H**(-0.46927))*&
             &(CL**0.39114)*((BAREA+1.0)**(-0.05429)))*(D/OMIND)
     end if
     if (CW .gt.36.) CW = 36.

     !  CASE 06601 BECHTOLD 2004 MODEL 1
     !  066          JUNIPERUS SCOPULORUM              ROCKY MOUNTAIN JUNIPER
  case('06601')
     if (D .ge. MIND) then
        CW = 2.1431 + (1.3447*D) + (-0.0228*D*D)
     else
        CW = (2.1431 + (1.3447*MIND) + (-0.0228*MIND*MIND))*(D/MIND)
     end if
     if (CW .gt.29.) CW = 29.

     !  CASE 06602 BECHTOLD 2004 MODEL 2
     !  066          JUNIPERUS SCOPULORUM              ROCKY MOUNTAIN JUNIPER
  case('06602')
     if (HI .lt. -37.) HI = -37.
     if (HI .gt.19.) HI = 19.
     if (D .ge. MIND) then
        CW = -4.1599 +(1.3528*D) + (-0.0233*D*D) + (0.0633*CR)&
             &+ (-0.0423*HI)
     else
        CW = (-4.1599 + (1.3528*MIND)+(-0.0233*MIND*MIND)+&
             &(0.0633*CR) + (-0.0423*HI))*(D/MIND)
     end if
     if (D .ge.25) then
        CW = -4.1599 +(1.3528*25) + (-0.0233*25*25) + (0.0633*CR)&
             &+ (-0.0423*HI)
     end if
     if (CW .gt.29.) CW = 29.

     !  CASE 07204 CROOKSTON (R6) MODEL 1
     !  072          LARIX LYALLII                     SUBALPINE LARCH
  case('07204')
     if (D .ge. OMIND) then
        CW = 2.2586*D**0.68532
     else
        CW = (2.2586*OMIND**0.68532)*(D/OMIND)
     end if
     if (CW .gt.33.) CW = 33.

     !  CASE 07302 BECHTOLD 2004 MODEL 2
     !  073          LARIX OCCIDENTALIS                 WESTERN LARCH
  case('07302')
     if (D .ge. MIND) then
        CW = 1.5995 + 0.7675*D + 0.075*CR
     else
        CW = (1.5995 + 0.7675*MIND + 0.075*CR)*(D/MIND)
     end if
     if (CW .gt.30.) CW = 30.

     !  CASE 07303 CROOKSTON (R1)
     !  073          LARIX OCCIDENTALIS                 WESTERN LARCH
  case('07303')
     if (D .ge.1.0) then
        CW = 1.02478*EXP(0.99889 + 0.19422*ALOG(CL)+0.59423*ALOG(D)&
             &+ (-0.09078*ALOG(H)) + (-0.02341*ALOG(BAREA)))
     else
        CW = (1.02478*EXP(0.99889+0.19422*ALOG(CL)+0.59423*ALOG(1.0)&
             &+ (-0.09078*ALOG(H)) + (-0.02341*ALOG(BAREA))))*(D/1.0)
     end if
     if (CW .gt.40.) CW = 40.

     !  CASE 07305 CROOKSTON (R6) MODEL 2
     !  073          LARIX OCCIDENTALIS                 WESTERN LARCH
  case('07305')
     if (EL .lt.19.) EL = 19.
     if (EL .gt.72.) EL = 72.
     if (D .ge. OMIND) then
        CW = 3.2548*BF*(D**0.60845)*(H**(-0.19146))*(CL**0.21051)*&
             &((BAREA+1.0)**0.00972)*(EXP(EL)**(-0.00313))
     else
        CW = (3.2548*BF*(OMIND**0.60845)*(H**(-0.19146))*&
             &(CL**0.21051)*((BAREA+1.0)**0.00972)*(EXP(EL)**(-0.00313)))&
             &*(D/OMIND)
     end if
     if (CW .gt.40.) CW = 40.

     !  CASE 07306 DONNELLY (R6)
     !  073          LARIX OCCIDENTALIS                 WESTERN LARCH
  case('07306')
     if (D .ge. OMIND) then
        CW = 2.9571*D**0.6081
     else
        CW = (2.9571*D**0.6081)*(D/OMIND)
     end if
     if (CW .gt.40.) CW = 40.

     !  CASE 07307 MOEUR
     !  073          LARIX OCCIDENTALIS                 WESTERN LARCH
  case('07307')
     CW = EXP(2.31359 + 1.08137*ALOG(D) + (-0.80919*ALOG(H))&
          &+ 0.29786*ALOG(CL))
     if (CW .gt.40.) CW = 40.

     !  CASE 08105 CROOKSTON (R6) MODEL 2
     !  081          LIBOCEDRUS DECURRENS                INCENSE CEDAR
  case('08105')
     if (EL .lt.5.) EL = 5.
     if (EL .gt.62.) EL = 62.
     if (D .ge. OMIND) then
        CW = 5.0446*BF*(D**0.47419)*(H**(-0.13917))*(CL**0.14230)&
             &*((BAREA+1.0)**0.04838)*(EXP(EL)**(-0.00616))
     else
        CW = (5.0446*BF*(OMIND**0.47419)*(H**(-0.13917))*&
             &(CL**0.14230)*((BAREA+1.0)**0.04838)*(EXP(EL)**(-0.00616)))&
             &*(D/OMIND)
     end if
     if (CW .gt.78.) CW = 78.

     !  CASE 08106 DONNELLY (R6)
     !  081          LIBOCEDRUS DECURRENS                INCENSE CEDAR
  case('08106')
     if (D .ge. OMIND) then
        CW = 4.0920*D**0.4912
     else
        CW = (4.0920*OMIND**0.4912)*(D/OMIND)
     end if
     if (CW .gt.40.) CW = 40.

     !  CASE 09508 CASTLE AK EQN 08
     !  095          PICEA MARIANA - BE                  BLACK SPRUCE
  case('09508')
     if (D .ge.0.1) then
        CW = 3.391358 * (D**0.638945)*(H**(-0.395285))*(CL**0.264254)
     else
        CW = 0.5
     end if
     if (CW .gt.16.) CW = 16.

     !  CASE 09204 CROOKSTON (R6) MODEL 1
     !  092          PICEA BREWERIANA                    BREWER SPRUCE
  case('09204')
     if (D .ge. OMIND) then
        CW = 2.8232*D**0.66326
     else
        CW = (2.8232*OMIND**0.66326)*(D/OMIND)
     end if
     if (CW .gt.38.) CW = 38.

     !  CASE 09302 BECHTOLD 2004 MODEL 2
     !  093          PICEA ENGELMANNII                  ENGELMANN SPRUCE
  case('09302')
     if (HI .lt. -25.) HI = -25.
     if (HI .gt.44.) HI = 44.
     if (D .ge. MIND) then
        CW = 4.1348 + 0.5694*D + 0.0403*CR + (-0.1014*HI)
     else
        CW = (4.1348+0.5694*MIND+0.0403*CR +(-0.1014*HI))*(D/MIND)
     end if
     if (CW .gt.40.) CW = 40.

     !  CASE 09303 CROOKSTON (R1)
     !  093          PICEA ENGELMANNII                  ENGELMANN SPRUCE
  case('09303')
     if (D .ge.0.1) then
        CW = 1.02687*EXP(1.28027 + 0.2249*ALOG(CL) + 0.47075*ALOG(D)&
             &+ (-0.15911)*ALOG(H))
     else
        CW = (1.02687*EXP(1.28027 + 0.2249*ALOG(CL) +&
             &0.47075*ALOG(0.1)+(-0.15911)*ALOG(H)))*(D/0.1)
     end if
     if (CW .gt.40.) CW = 40.

     !  CASE 09305 CROOKSTON (R6) MODEL 2
     !  093          PICEA ENGELMANNII                  ENGELMANN SPRUCE
  case('09305')
     if (EL .lt.1.) EL = 1.
     if (EL .gt.85.) EL = 85.
     if (D .ge. OMIND) then
        CW = 6.7575*BF*(D**0.55048)*(H**(-0.25204))*(CL**0.19002)&
             &*(EXP(EL)**(-0.00313))
     else
        CW = (6.7575*BF*(OMIND**0.55048)*(H**(-0.25204))*&
             &(CL**0.19002)*(EXP(EL)**(-0.00313)))*(D/OMIND)
     end if
     if (CW .gt.40.) CW = 40.

     !  CASE 09306 DONNELLY (R6)
     !  093          PICEA ENGELMANNII                  ENGELMANN SPRUCE
  case('09306')
     if (D .ge. OMIND) then
        CW = 3.6802*D**0.4940
     else
        CW = (3.6802*OMIND**0.4940)*(D/OMIND)
     end if
     if (CW .gt.40.) CW = 40.

     !  CASE 09307 MOEUR
     !  093          PICEA ENGELMANNII                  ENGELMANN SPRUCE
  case('09307')
     CW = EXP(3.76535 + 1.08137*ALOG(D) + (-1.18257*ALOG(H))&
          &+ 0.29786*ALOG(CL))
     if (CW .gt.40.) CW = 40.

     !  CASE 09802 BECHTOLD 2004 MODEL 2
     !  098          PICEA SITCHENSIS                  SITKA SPRUCE
  case('09802')
     if (D .ge. MIND) then
        CW = 8.8087 + (0.7825*D)
     else
        CW = (8.8087 + 0.7825*MIND)*(D/MIND)
     end if
     if (CW .gt.43.) CW = 43.

     !  CASE 09805 CROOKSTON (R6) MODEL 2
     !  098          PICEA SITCHENSIS                  SITKA SPRUCE
  case('09805')
     if (D .ge. OMIND) then
        CW = 8.48*BF*(D**0.70692)*(H**(-0.38812))*(CL**0.17127)
     else
        CW = (8.48*BF*(OMIND**0.70692)*(H**(-0.38812))&
             &*(CL**0.17127))*(D/OMIND)
     end if
     if (CW .gt.50.) CW = 50.

     !  CASE 09806 DONNELLY (R6)
     !  098          PICEA SITCHENSIS                  SITKA SPRUCE
  case('09806')
     if (D .ge. OMIND) then
        CW = 4.2857*D**0.5940
     else
        CW = (4.2857*OMIND**0.5940)*(D/OMIND)
     end if
     if (CW .gt.60.) CW = 60.

     !  CASE 09408 AK EQN 08
     !  094          PICEA GLAUCA - WS                 WHITE SPRUCE
  case('09408')
     if (EL .lt.1.) EL = 1.
     if (EL .gt.85.) EL = 85.
     if (D .ge.0.1) then
        CW = 8.515316 * (D**0.630576)*(H**(-0.867757))*(CL**0.477791)&
             &* ((BAREA + 1)**0.10021) * (EXP(EL)**(-0.015034))
     else
        CW = 0.5
     end if
     if (CW .gt.40.) CW = 40.

     !  CASE 10102 BECHTOLD 2004 MODEL 2
     !  101          PINUS ALBICAULIS                   WHITEBARK PINE
  case('010102')
     if (HI .lt.6.) HI = 6.
     if (HI .gt.44.) HI = 44.
     if (D .ge. MIND) then
        CW = 0.5223 + 0.7432*D + (0.0829*HI)
     else
        CW = (0.5223 + 0.7432*MIND + (0.0829*HI))*(D/MIND)
     end if
     if (CW .gt.40.) CW = 40.

     !  CASE 10103 CROOKSTON (R1)
     !  101          PINUS ALBICAULIS                   WHITEBARK PINE
  case('10103')
     if (D .ge.1.1) then
        CW = 1.0697*EXP(0.3007 + 0.2400*ALOG(CL) + 0.5696*ALOG(D))
     else
        CW = (1.0697*EXP(0.3007+0.2400*ALOG(CL)+0.5696*ALOG(1.1)))&
             &*(D/1.1)
     end if
     if (CW .gt.40.) CW = 40.

     !  CASE 10105 CROOKSTON (R6) MODEL 2
     !  101          PINUS ALBICAULIS                   WHITEBARK PINE
  case('10105')
     if (D .ge. OMIND) then
        CW = 2.2354*BF*(D**0.66680)*(H**(-0.11658))*(CL**0.16927)
     else
        CW = (2.2354*BF*(OMIND**0.66680)*(H**(-0.11658))*&
             &(CL**0.16927))*(D/OMIND)
     end if
     if (CW .gt.40.) CW = 40.

     !  CASE 10106 DONNELLY (R6)
     !  101          PINUS ALBICAULIS                   WHITEBARK PINE
  case('10106')
     if (D .ge. OMIND) then
        CW = 2.1606*D**0.6897
     else
        CW = (2.1606*OMIND**0.6897)*(D/OMIND)
     end if
     if (CW .gt.40.) CW = 40.

     !  CASE 10107 MOEUR
     !  101          PINUS ALBICAULIS                   WHITEBARK PINE
  case('10107')
     CW = EXP(-.91984 + (1.08137*ALOG(D)) + (-0.07299*ALOG(H))&
          &+ 0.29786*ALOG(CL))
     if (CW .gt.40.) CW = 40.

     !  CASE 10201 BECHTOLD 2004 MODEL 1
     !  102          PINUS ARTISTA                      BRISTLECONE PINE
  case('10201')
     if (D .ge. MIND) then
        CW = (7.4251 + 0.8991*D)
     else
        CW = (7.4251 + 0.8991*MIND)*(D/MIND)
     end if
     if (CW .gt.25.) CW = 25.

     !  CASE 10305 CROOKSTON (R6) MODEL 2
     !  103          PINUS ATTENUATA                     KNOBCONE PINE
  case('10305')
     if (EL .lt.12.) EL = 12.
     if (EL .gt.49.) EL = 49.
     if (D .ge. OMIND) then
        CW = 4.0069*BF*(D**0.84628)*(H**(-0.29035))*(CL**0.13143)*&
             &*(EXP(EL)**(-0.00842))
     else
        CW = (4.0069*BF*(OMIND**0.84628)*(H**(-0.29035))*&
             &(CL**0.13143)*(EXP(EL)**(-0.00842)))*(D/OMIND)
     end if
     if (CW .gt.46.) CW = 46.

     !  CASE 10601 BECHTOLD 2004 MODEL 1
     !  106          PINUS EDULIS                      PINYON PINE (EDULIS)
  case('10601')
     if (D .ge. MIND) then
        CW = -1.2638 + (1.9922*D) + (-0.0410*D*D)
     else
        CW = (-1.2638+(1.9922*MIND)+(-0.0410*MIND*MIND))*(D/MIND)
     end if
     if (CW .gt.25.) CW = 25.

     !  CASE 10602 BECHTOLD 2004 MODEL 2
     !  106          PINUS EDULIS                      PINYON PINE (EDULIS)
  case('10602')
     if (HI .lt. -40.) HI = -40.
     if (HI .gt.11.) HI = 11.
     if (D .ge. MIND) then
        CW = -5.4647 + (1.9660*D) + (-0.0395*D*D) + (0.0427*CR) +&
             &(-0.0259*HI)
     else
        CW = (-5.4647+(1.9660*MIND)+(-0.0395*MIND*MIND)+(0.0427*CR)+&
             &(-0.0259*HI))*(D/MIND)
     end if
     if (D .ge.25) then
        CW = -5.4647 + (1.9660*25) + (-0.0395*25*25) + (0.0427*CR) +&
             &(-0.0259*HI)
     end if
     if (CW .gt.25.) CW = 25.

     !  CASE 10802 BECHTOLD 2004 MODEL 2
     !  108          PINUS CONTORTA                     LODGEPOLE PINE
  case('10202')
     if (D .ge. MIND) then
        CW = -1.5440 + (1.3828*D) + (-0.0200*D*D) + (0.0396*CR) +&
             &(-0.0083*BAREA)
     else
        CW = (-1.5440 + 1.3828*MIND + (-0.0200*D*D) + 0.0396*CR +&
             &(-0.0083*BAREA))*(D/MIND)
     end if
     if (CW .gt.30.) CW = 30.

     !  CASE 10803 CROOKSTON (R1)
     !  108          PINUS CONTORTA                     LODGEPOLE PINE
  case('10803')
     if (D .ge.0.7) then
        CW = 1.03992*EXP(1.58777 + 0.30812*ALOG(CL)+0.64934*ALOG(D)&
             &+ (-0.38964)*ALOG(H))
     else
        CW = (1.03992*EXP(1.58777 + 0.30812*ALOG(CL)+&
             &0.64934*ALOG(0.7)+(-0.38964)*ALOG(H)))*(D/0.7)
     end if
     if (CW .gt.40.) CW = 40.

     !  CASE 10805 CROOKSTON (R6) MODEL 2
     !  108          PINUS CONTORTA                     LODGEPOLE PINE
  case('10805')
     if (EL .lt.1.) EL = 1.
     if (EL .gt.79.) EL = 79.
     if (D .ge. OMIND) then
        CW = 6.6941*BF*(D**0.81980)*(H**(-0.36992))*(CL**0.17722)*&
             &((BAREA+1.0)**(-0.01202))*(EXP(EL)**(-0.00882))
     else
        CW = (6.6941*BF*(OMIND**0.81980)*(H**(-0.36992))*&
             &(CL**0.17722)*((BAREA+1.0)**(-0.01202))*&
             &(EXP(EL)**(-0.00882)))*(D/OMIND)
     end if
     if (CW .gt.40.) CW = 40.

     !  CASE 10806 DONNELLY (R6)
     !  108          PINUS CONTORTA                     LODGEPOLE PINE
  case('10806')
     if (D .ge. OMIND) then
        CW = 2.4132*D**0.6403
     else
        CW = (2.4132*OMIND**0.6403)*(D/OMIND)
     end if
     if (CW .gt.40.) CW = 40.

     !  CASE 10807 MOEUR
     !  108          PINUS CONTORTA                     LODGEPOLE PINE
  case('10807')
     CW = EXP(1.06804 + 1.08137*ALOG(D) + (-0.55987)*ALOG(H)&
          &+ 0.29786*ALOG(CL))
     if (CW .gt.40.) CW = 40.

     !  CASE 11301 BECHTOLD 2004 MODEL 1
     !  113          PINUS FLEXILIS                     LIMBER PINE
  case('11301')
     if (D .ge. MIND) then
        CW = 4.0181 + 0.8528*D
     else
        CW = (4.0181 + 0.8528*MIND)*(D/MIND)
     end if
     if (CW .gt.25.) CW = 25.

     !  CASE 11602 BECHTOLD 2004 MODEL 2
     !  116          PINUS JEFFREYI                    JEFFREY PINE
  case('11602')
     if (HI .lt. -38.) HI = -38.
     if (HI .gt.15.) HI = 15.
     if (D .ge. MIND) then
        CW = 1.2784 + 0.7937*D + 0.0334*CR + (-0.0887*HI)
     else
        CW = (1.2784 + 0.7937*MIND + 0.0334*CR + (-0.0887*HI))*&
             &(D/MIND)
     end if
     if (CW .gt.44.) CW = 44.

     !  CASE 11605 CROOKSTON (R6) MODEL 2
     !  116          PINUS JEFFREYI                    JEFFREY PINE
  case ('11605')
     if (D .ge. OMIND) then
        CW = 4.0217*BF*(D**0.66815)*(H**(-0.11346))*(CL**0.09689)*&
             &((BAREA+1.0)**(-0.06360))
     else
        CW = (4.0217*BF*(OMIND**0.66815)*(H**(-0.11346))*&
             &(CL**0.09689)*((BAREA+1.0)**(-0.06360)))*(D/OMIND)
     end if
     if (CW .gt.39.) CW = 39.

     !  CASE 11606 DONNELLY (R6)
     !  116          PINUS JEFFREYI                    JEFFREY PINE
  case('11606')
     if (D .ge. OMIND) then
        CW = 3.2367*D**0.6247
     else
        CW = (3.2367*OMIND**0.6247)*(D/OMIND)
     end if
     if (CW .gt.30.) CW = 30.

     !  CASE 11702 BECHTOLD 2004 MODEL 2
     !  117          PINUS LAMBERTIANA                  SUGAR PINE
  case('11702')
     if (HI .lt. -47.) HI = -47.
     if (HI .gt.11.) HI = 11.
     if (D .ge. MIND) then
        CW = (3.1052) + (0.8049*D) + (-0.1230*HI)
     else
        CW = (3.1052 + 0.8049*MIND + (-0.1230*HI)) * (D/MIND)
     end if
     if (CW .gt.49.) CW = 49.

     !  CASE 11705 CROOKSTON (R6) MODEL 2
     !  117          PINUS LAMBERTIANA                  SUGAR PINE
  case ('11705')
     if (EL .lt.5.) EL = 5.
     if (EL .gt.75.) EL = 75.
     if (D .ge. OMIND) then
        CW = 3.5930*BF*(D**0.63503)*(H**(-0.22766))*(CL**0.17827)*&
             &((BAREA+1.0)**(0.04267))*(EXP(EL)**(-0.00290))
     else
        CW = (3.5930*BF*(OMIND**0.63503)*(H**(-0.22766))*&
             &(CL**0.17827)*((BAREA+1.0)**(0.04267))*&
             &(EXP(EL)**(-0.00290)))*(D/OMIND)
     end if
     if (CW .gt.56.) CW = 56.

     !  CASE 11706 DONNELLY (R6)
     !  117          PINUS LAMBERTIANA                  SUGAR PINE
  case('11706')
     if (D .ge. OMIND) then
        CW = 3.0610*D**0.6201
     else
        CW = (3.0610*OMIND**0.6201)*(D/OMIND)
     end if
     if (CW .gt.50.) CW = 50.

     !  CASE 11902 BECHTOLD 2004 MODEL 2
     !  119          PINUS MONTICOLA                    WESTERN WHITE PINE
  case('11902')
     if (HI .lt. -25.) HI = -25.
     if (HI .gt.32.) HI = 32.
     if (D .ge. MIND) then
        CW = 4.8643 + 0.6949*D + (-0.0974*HI)
     else
        CW = (4.8643 + 0.6949*MIND + (-0.0974*HI))*(D/MIND)
     end if
     if (CW .gt.35.) CW = 35.

     !  CASE 11903 CROOKSTON (R1)
     !  119          PINUS MONTICOLA                    WESTERN WHITE PINE
  case('11903')
     if (D .ge.1.0) then
        CW = 1.0405*EXP(1.2799 + 0.11941*ALOG(CL) + 0.42745*ALOG(D)&
             &+ (-0.07182*ALOG(BAREA)))
     else
        CW = (1.0405*EXP(1.2799+0.11941*ALOG(CL)+0.42745*ALOG(1.0)&
             &+ (-0.07182*ALOG(BAREA))))*(D/1.0)
     end if
     if (CW .gt.35.) CW = 35.

     !  CASE 11905 CROOKSTON (R6) MODEL 2
     !  119          PINUS MONTICOLA                    WESTERN WHITE PINE
  case('11905')
     if (EL .lt.10.) EL = 10.
     if (EL .gt.75.) EL = 75.
     if (D .ge. OMIND) then
        CW = 5.3822*BF*(D**0.57896)*(H**(-0.19579))*(CL**0.14875)*&
             &(EXP(EL)**(-0.00685))
     else
        CW = (5.3822*BF*(OMIND**0.57896)*(H**(-0.19579))*&
             &(CL**0.14875)*(EXP(EL)**(-0.00685)))*(D/OMIND)
     end if
     if (CW .gt.35.) CW = 35.

     !  CASE 11906 DONNELLY (R6)
     !  119          PINUS MONTICOLA                    WESTERN WHITE PINE
  case('11906')
     if (D .ge. OMIND) then
        CW = 3.4447*D**0.5185
     else
        CW = (3.4447*OMIND**0.5185)*(D/OMIND)
     end if
     if (CW .gt.35.) CW = 35.

     !  CASE 11907 MOEUR
     !  119          PINUS MONTICOLA                    WESTERN WHITE PINE
  case('11907')
     CW = EXP(4.30800 + 1.08137*ALOG(D) + (-1.37265*ALOG(H))&
          &+ 0.29786*ALOG(CL))
     if (CW .gt.35.) CW = 35.

     !  CASE 12202 BECHTOLD 2004 MODEL 2
     !  122          PINUS PONDEROSA                    PONDEROSA PINE
  case('12202')
     if (HI .lt. -56.) HI = -56.
     if (HI .gt.41.) HI = 41.
     if (D .ge. MIND) then
        CW = (-0.3459) + (1.111*D) + (-0.008*D*D) + (0.0566*CR)&
             &+ (-0.0094*BAREA)+(-0.0362*HI)
     else
        CW = ((-0.3459)+(1.111*MIND)+(-0.008*MIND*MIND)+(0.0566*CR)&
             &+ (-0.0094*BAREA) +(-0.0362*HI))*(D/MIND)
     end if
     if (CW .gt.46.) CW = 46.

     !  CASE 12203 CROOKSTON (R1)
     !  122          PINUS PONDEROSA                    PONDEROSA PINE
  case('12203')
     if (D .ge.2.0) then
        CW = 1.02687*EXP(1.49085 + 0.1862*ALOG(CL) + 0.68272*ALOG(D)&
             &+ (-0.28242*ALOG(H)))
     else
        CW = (1.02687*EXP(1.49085 + 0.1862*ALOG(CL) +&
             &0.68272*ALOG(2.0)+ (-0.28242*ALOG(H))))*(D/2.0)
     end if
     if (CW .gt.46.) CW = 46.

     !  CASE 12205 CROOKSTON (R6) MODEL 2
     !  122          PINUS PONDEROSA                    PONDEROSA PINE
  case ('12205')
     if (EL .lt.13.) EL = 13.
     if (EL .gt.75.) EL = 75.
     if (D .ge. OMIND) then
        CW = 4.7762*BF*(D**0.74126)*(H**(-0.28734))*(CL**0.17137)*&
             &((BAREA+1.0)**(-0.00602))*(EXP(EL)**(-0.00209))
     else
        CW = (4.7762*BF*(OMIND**0.74126)*(H**(-0.28734))*&
             &(CL**0.17137)*((BAREA+1.0)**(-0.00602))*&
             &(EXP(EL)**(-0.00209)))*(D/OMIND)
     end if
     if (CW .gt.50.) CW = 50.

     !  CASE 12206 DONNELLY (R6)
     !  122          PINUS PONDEROSA                    PONDEROSA PINE
  case('12206')
     if (D .ge. OMIND) then
        CW = 2.8541*D**0.6400
     else
        CW = (2.8541*OMIND**0.6400)*(D/OMIND)
     end if
     if (CW .gt.50.) CW = 50.

     !  CASE 12207 MOEUR
     !  122          PINUS PONDEROSA                    PONDEROSA PINE
  case('12207')
     CW = EXP(1.62365+ 1.08137*ALOG(D) + (-0.68098*ALOG(H))&
          &+ 0.29786*ALOG(CL))
     if (CW .gt.50.) CW = 50.

     !  CASE 12702 BECHTOLD 2004 MODEL 2
     !  127          PINUS SABINIANA                    GRAY PINE
  case('12702')
     if (HI .lt. -69.) HI = -69.
     if (HI .gt.  -4.) HI = -4.
     if (D .ge. MIND) then
        CW = (-2.4909) + (1.0716*D) + (0.0648*CR)&
             &+(-0.1127*HI)
     else
        CW = ((-2.4909) + (1.0716*MIND) + (0.0648*CR)&
             &+(-0.1127*HI))*(D/MIND)
     end if
     if (CW .gt.54.) CW = 54.

     !  CASE 20202 BECHTOLD 2004 MODEL 2
     !  202          PSEUDOTSUGA MENZIESII              DOUGLAS FIR
  case('20202')
     if (HI .lt. -49.) HI = -49.
     if (HI .gt.67.) HI = 67.
     if (D .ge. MIND) then
        CW = 3.2346 + 1.1158*D + (-0.0112*D*D) + 0.0442*CR +&
             &(-0.0057*BAREA) + (-0.0237*HI)
     else
        CW = (3.2346+1.1158*MIND+(-0.0112*MIND*MIND)+0.0442*CR +&
             &(-0.0057*BAREA) + (-0.0237*HI))*(D/MIND)
     end if
     if (CW .gt.80.) CW = 80.

     !  CASE 20203 CROOKSTON (R1)
     !  202          PSEUDOTSUGA MENZIESII              DOUGLAS FIR
  case('20203')
     if (D .ge.1.0) then
        CW = 1.01685*EXP(1.48372 + 0.27378*ALOG(CL)+0.49646*ALOG(D)&
             &+ (-0.18669*ALOG(H)) + (-0.01509*ALOG(BAREA)))
     else
        CW = (1.01685*EXP(1.48372+0.27378*ALOG(CL)+0.49646*ALOG(1.0)&
             &+ (-0.18669*ALOG(H)) + (-0.01509*ALOG(BAREA))))*(D/1.0)
     end if
     if (CW .gt.80.) CW = 80.

     !  CASE 20205 CROOKSTON (R6) MODEL 2
     !  202          PSEUDOTSUGA MENZIESII              DOUGLAS FIR
  case('20205')
     if (EL .lt.1.) EL = 1.
     if (EL .gt.75.) EL = 75.
     if (D .ge. OMIND) then
        CW = 6.0227*BF*(D**0.54361)*(H**(-0.20669))*(CL**0.20395)*&
             &((BAREA+1.0)**(-0.00644))*(EXP(EL)**(-0.00378))
     else
        CW = (6.0227*1.0*(OMIND**0.54361)*(H**(-0.20669))*&
             &(CL**0.20395)*((BAREA+1.0)**(-0.00644))*&
             &(EXP(EL)**(-0.00378)))*(D/OMIND)
     end if
     if (CW .gt.80.) CW = 80.

     !  CASE 20206 DONNELLY (R6)
     !  202          PSEUDOTSUGA MENZIESII              DOUGLAS FIR
  case('20206')
     if (D .ge. OMIND) then
        CW = 4.4215*D**0.5329
     else
        CW = (4.4215*OMIND**0.5329)*(D/OMIND)
     end if
     if (CW .gt.80.) CW = 80.

     !  CASE 20207 MOEUR
     !  202          PSEUDOTSUGA MENZIESII              DOUGLAS FIR
  case('20207')
     CW = EXP(3.02271+ 1.08137*ALOG(D) + (-1.00486*ALOG(H))&
          &+ 0.29786*ALOG(CL))
     if (CW .gt.80.) CW = 80.

     !  CASE 21104 CROOKSTON (R6) MODEL 2
     !  211          SEQUOIA SEMPERVIRENS               REDWOOD
  case('21104')
     if (D .ge. OMIND) then
        CW = 3.7023*D**0.52618
     else
        CW = (3.7023*OMIND**0.52618)*(D/OMIND)
     end if
     if (CW .gt.39.) CW = 39.

     !  CASE 23104 CROOKSTON (R6) MODEL 2
     !  231          TAXUS PLICATA                      PACIFIC YEW
  case('23104')
     if (D .ge. OMIND) then
        CW = 6.1297*D**0.45424
     else
        CW = (6.1297*OMIND**0.45424)*(D/OMIND)
     end if
     if (CW .gt.30.) CW = 30.

     !  CASE 24202 BECHTOLD 2004 MODEL 2
     !  242          THUJA PLICATA                      WESTERN REDCEDAR
  case('24202')
     if (D .ge. MIND) then
        CW = 5.2911 + 1.0612*D + (-0.0153*D*D) + 0.0469*CR
     else
        CW = (5.2911+1.0612*MIND+(-0.0153*MIND*MIND)+0.0469*CR)*&
             &(D/MIND)
     end if
     if (CW .gt.38.) CW = 38.

     !  CASE 24203 CROOKSTON (R1)
     !  242          THUJA PLICATA                      WESTERN REDCEDAR
  case('24203')
     if (D .ge.1.0) then
        CW = 1.03597*EXP(1.46111 + 0.26289*ALOG(CL)&
             &+ 0.18779*ALOG(D))
     else
        CW = (1.03597*EXP(1.46111 + 0.26289*ALOG(CL)&
             &+ 0.18779*ALOG(1.0)))*(D/1.0)
     end if
     if (CW .gt.45.) CW = 45.

     !  CASE 24205 CROOKSTON (R6) MODEL 2
     !  242          THUJA PLICATA                      WESTERN REDCEDAR
  case('24205')
     if (EL .lt.1.) EL = 1.
     if (EL .gt.72.) EL = 72.
     if (D .ge. OMIND) then
        CW = 6.2382*BF*(D**0.29517)*(H**(-0.10673))*(CL**0.23219)*&
             &((BAREA+1.0)**0.05341)*(EXP(EL)**(-0.00787))
     else
        CW = (6.2382*BF*(OMIND**0.29517)*(H**(-0.10673))*&
             &(CL**0.23219)*((BAREA+1.0)**0.05341)*&
             &(EXP(EL)**(-0.00787)))*(D/OMIND)
     end if
     if (CW .gt.45.) CW = 45.

     !  CASE 24206 DONNELLY (R6)
     !  242          THUJA PLICATA                      WESTERN REDCEDAR
  case('24206')
     if (D .ge. OMIND) then
        CW = 6.2318*D**0.4259
     else
        CW = (6.2318*OMIND**0.4259)*(D/OMIND)
     end if
     if (CW .gt.45.) CW = 45.

     !  CASE 24207 MOEUR
     !  242          THUJA PLICATA                      WESTERN REDCEDAR
  case('24207')
     CW = EXP(2.79784 + 1.08137*ALOG(D) + (-0.89666*ALOG(H))&
          &+ 0.29786*ALOG(CL))
     if (CW .gt.45.) CW = 45.

     !  CASE 26302 BECHTOLD 2004 MODEL 2
     !  263          TSUGA HETERPHYLLA                 WESTERN HEMLOCK
  case('26302')
     if (HI .lt. -34.) HI = -34.
     if (HI .gt.49.) HI = 49.
     if (D .ge. MIND) then
        CW = -0.4624 + 1.0429*D + (-0.0078*D*D)&
             &+ 0.1018*CR + (-0.0271*HI)
     else
        CW = (-0.4624 + 1.0429*MIND + (-0.0078*MIND*MIND)&
             &+ 0.1018*CR + (-0.0271*HI))*(D/MIND)
     end if
     if (CW .gt.54.) CW = 54.

     !  CASE 26303 CROOKSTON (R1)
     !  263          TSUGA HETERPHYLLA                 WESTERN HEMLOCK
  case('26303')
     if (D .ge.0.1) then
        CW = 1.02460*EXP(1.3522 + 0.24844*ALOG(CL)+0.412117*ALOG(D)&
             &+ (-0.104357*ALOG(H)) + (0.03538*ALOG(BAREA)))
     else
        CW = (1.02460*EXP(1.3522+0.24844*ALOG(CL)+0.412117*ALOG(0.1)&
             &+ (-0.104357*ALOG(H)) + (0.03538*ALOG(BAREA))))*(D/0.1)
     end if
     if (CW .gt.54.) CW = 54.

     !  CASE 26305 CROOKSTON (R6) MODEL 2
     !  263          TSUGA HETERPHYLLA                 WESTERN HEMLOCK
  case('26305')
     if (EL .lt.1.) EL = 1.
     if (EL .gt.72.) EL = 72.
     if (D .ge. OMIND) then
        CW = 6.0384*BF*(D**0.51581)*(H**(-0.21349))*(CL**0.17468)*&
             &((BAREA+1.0)**(0.06143))*(EXP(EL)**(-0.00571))
     else
        CW = (6.0384*BF*(OMIND**0.51581)*(H**(-0.21349))*&
             &(CL**0.17468)*((BAREA+1.0)**(0.06143))*&
             &(EXP(EL)**(-0.00571)))*(D/OMIND)
     end if
     if (CW .gt.54.) CW = 54.

     !  CASE 26306 DONNELLY (R6)
     !  263          TSUGA HETERPHYLLA                 WESTERN HEMLOCK
  case('26306')
     if (D .ge. OMIND) then
        CW = 5.4864*D**0.5144
     else
        CW = (5.4864*OMIND**0.5144)*(D/OMIND)
     end if
     if (CW .gt.55.) CW = 55.

     !  CASE 26402 BECHTOLD 2004 MODEL 2
     !  264          TSUGA MERTENSIANA                 MOUNTAIN HEMLOCK
  case('26402')
     if (D .ge. MIND) then
        CW = -0.3362 + 0.7142*D + 0.0414*CR
     else
        CW = (-0.3362 + 0.7142*MIND + 0.0414*CR)*(D/MIND)
     end if
     if (CW .gt.45.) CW = 45.

     !  CASE 26403 CROOKSTON (R1) ***SHOULD THIS BE R6?***
     ! EQUATION HAS BEEN UPDATED TO REFLECT PROPER LOGIC
     !  264          TSUGA MERTENSIANA                 MOUNTAIN HEMLOCK
  case('26403')
     if (H .lt.5) then
        CW = (0.8*H*MAX(0.5,CR*0.01))*(1-(H-5)*0.1)*6.90396&
             &*(D**0.55645)*(H**(-0.28509))*(CL**0.20430)*(H-5)*0.1
     else if (H .ge.15) then
        CW = 6.90396*(D**0.55645)*(H**(-0.28509))*(CL**0.20430)
     else
        CW = .8*H*MAX(0.5,CR*0.01)
     end if
     if (CW .gt.45.) CW = 45.

     !  CASE 26405 CROOKSTON (R6) MODEL 2 ***SHOULD THIS BE R1?***
     !  264          TSUGA MERTENSIANA                 MOUNTAIN HEMLOCK
  case('26405')
     if (EL .lt.10.) EL = 10.
     if (EL .gt.79.) EL = 79.
     if (D .ge. OMIND) then
        CW = 3.7854*BF*(D**0.54684)*(H**(-0.12954))*(CL**0.16151)*&
             &((BAREA+1.0)**(0.03047))*(EXP(EL)**(-0.00561))
     else
        CW = (3.7854*BF*(OMIND**0.54684)*(H**(-0.12954))*&
             &(CL**0.16151)*((BAREA+1.0)**(0.03047))*&
             &(EXP(EL)**(-0.00561)))*(D/OMIND)
     end if
     if (CW .gt.45.) CW = 45.

     !  CASE 26406 DONNELLY (R6)
     !  264          TSUGA MERTENSIANA                 MOUNTAIN HEMLOCK
  case('26406')
     if (D .ge. OMIND) then
        CW = 2.9372*D**0.5878
     else
        CW = (2.9372*D**0.5878)*(D/OMIND)
     end if
     if (CW .gt.45.) CW = 45.

     !  CASE 31202 BECTHOLD 2004 MODEL 2
     !  312          ACER MACROPHYLLUM                   BIGLEAF MAPLE
  case('31202')
     if (HI .lt. -36.) HI = -36.
     if (HI .gt.49.) HI = 49.
     if (D .ge. MIND) then
        CW = -1.9386 + 1.2250*D + 0.1622*CR + (-0.1417*HI)
     else
        CW = (-1.9386+1.2250*MIND+0.1622*CR+(-0.1417*HI))*(D/MIND)
     end if
     if (CW .gt.57.) CW = 57.

     !  CASE 31206 DONNELLY (R6)
     !  312          ACER MACROPHYLLUM                   BIGLEAF MAPLE
  case('31206')
     if (D .ge. OMIND) then
        CW = 7.5183*D**0.4461
     else
        CW = (7.5183*OMIND**0.4461)*(D/OMIND)
     end if
     if (CW .gt.30.) CW = 30.

     !  CASE 32102 BECTHOLD 2004 MODEL 2
     !  321          ACER GLABRUM                       ROCKY MOUNTAIN MAPLE
  case('32102')
     if (D .ge. MIND) then
        CW = 5.9765 + 0.8648*D + 0.0675*CR
     else
        CW = (5.9765 + 0.8648*MIND + 0.0675*CR)*(D/MIND)
     end if
     if (CW .gt.39.) CW = 39.

     !  CASE 35102 BECTHOLD 2004 MODEL 2
     !  351          ALNUS RUBRA                       RED ALDER
  case('35102')
     if (D .ge. MIND) then
        CW = -0.7294 + 1.2885*D + 0.1307*CR
     else
        CW = (-0.7294 + 1.2885*MIND + 0.1307*CR)*(D/MIND)
     end if
     if (CW .gt.50.) CW = 50.

     !  CASE 35105 CROOKSTON (R6) MODEL 2
     !  351          ALNUS RUBRA                       RED ALDER
  case('35105')
     if (D .ge. OMIND) then
        CW = 4.1194*BF*(D**0.46252)*(CL**0.16180)
     else
        CW = (4.1194*BF*(OMIND**0.46252)*(CL**0.16180))*(D/OMIND)
     end if
     if (CW .gt.50.) CW = 50.

     !  CASE 35106 DONNELLY (R6)
     !  351          ALNUS RUBRA                       RED ALDER
  case('35106')
     if (D .ge. OMIND) then
        CW = 7.0806*D**0.4771
     else
        CW = (7.0806*OMIND**0.4771)*(D/OMIND)
     end if
     if (CW .gt.35.) CW = 35.

     !  CASE 35202 BECTHOLD 2004 MODEL 2
     !  352          ALNUS RHOMBIFOLIA                 WHITE ALDER
  case('35202')
     if (D .ge. MIND) then
        CW = 4.6188 + 0.9135*D + 0.1019*CR
     else
        CW = (4.6188 + 0.9135*MIND + 0.1019*CR)*(D/MIND)
     end if
     if (CW .gt.50.) CW = 50.

     !  CASE 36102 BECTHOLD 2004 MODEL 2
     !  361          ARBUTUS MENZIESII                  PACIFIC MADRONE
  case('36102')
     if (HI .lt. -55.) HI = -55.
     if (HI .gt.15.) HI = 15.
     if (D .ge. MIND) then
        CW = 4.9133 + 0.9459*D + 0.0611*CR + (0.0523*HI)
     else
        CW = (4.9133+0.9459*MIND+0.0611*CR+(0.0523*HI))*(D/MIND)
     end if
     if (CW .gt.43.) CW = 43.

     !  CASE 37506 DONNELLY (R6)
     !  375          BETULA PAPYRIFERA                   PAPER BIRCH
  case('37506')
     if (D .ge. OMIND) then
        CW = 5.8980*D**0.4841
     else
        CW = (5.8980*OMIND**0.4841)*(D/OMIND)
     end if
     if (CW .gt.25.) CW = 25.

     !  CASE 37508 AK EQN 08
     !  375          BETULA PAPYRIFERA - PB              PAPER BIRCH
  case('37508')
     if (D .ge.0.1) then
        CW = 2.725006 * (D**0.53601) * (CL**0.196372)&
             &* ((BAREA + 1)**(-0.015305))
     else
        CW = 0.5
     end if
     if (CW .gt.53.) CW = 53.

     !  CASE 47502 BECTHOLD 2004 MODEL 2
     !  475         CERCOCARPUS LEDIFOLIUS       CURLLEAF MOUNATIN MAHOGANY
  case('47502')
     if (HI .lt. -37.) HI = -37.
     if (HI .gt.27.) HI = 27.
     if (D .ge. MIND) then
        CW = 4.0105 + 0.8611*D + (-0.0431*HI)
     else
        CW = (4.0105 + 0.8611*MIND + (-0.0431*HI))*(D/MIND)
     end if
     if (CW .gt.29.) CW = 29.

     !  CASE 63102 BECTHOLD 2004 MODEL 2
     !  631          LITHOCARPUS DENSIFLORUSI           TANOAK
  case('63102')
     if (HI .lt. -55.) HI = -55.
     if (HI .gt.15.) HI = 15.
     if (D .ge. MIND) then
        CW = 3.1150 + 0.7966*D + 0.0745*CR + (-0.0053*BAREA) +&
             &(0.0523*HI)
     else
        CW = (3.1150 + 0.7966*MIND + 0.0745*CR + (-0.0053*BAREA)+&
             &(0.0523*HI))*(D/MIND)
     end if
     if (CW .gt.41.) CW = 41.

     !  CASE 74602 BECTHOLD 2004 MODEL 2
     !  746          POPULUS TREMULOIDES                QUAKING ASPEN
  case('74602')
     if (HI .lt. -9.) HI = -9.
     if (HI .gt.33.) HI = 33.
     if (D .ge. MIND) then
        CW = -0.5095 + 1.2318*D + 0.0744*CR + 0.0233*HI
     else
        CW = (-0.5095+1.2318*MIND+0.0744*CR+0.0233*HI)*(D/MIND)
     end if
     if (CW .gt.45.) CW = 45.

     !  CASE 74603 CROOKSTON (R1)
     !  746          POPULUS TREMULOIDES                QUAKING ASPEN
  case('74603')
     if (D .ge.0.1) then
        CW = 1.0396728*EXP(1.45297 + 0.060132*ALOG(D))
     else
        CW = (1.0396728*EXP(1.45297+0.060132*ALOG(0.1)))*(D/0.1)
     end if
     if (CW .gt.45.) CW = 45.

     !  CASE 74605 CROOKSTON (R6) MODEL 2
     !  746          POPULUS TREMULOIDES                QUAKING ASPEN
  case('74605')
     if (D .ge. OMIND) then
        CW = 4.7961*BF*(D**0.64167)*(H**(-0.18695))*(CL**0.18581)
     else
        CW = (4.7961*BF*(OMIND**0.64167)*(H**(-0.18695))*&
             &(CL**0.18581))*(D/OMIND)
     end if
     if (CW .gt.45.) CW = 45.

     !  CASE 74606 DONNELLY (R6)
     !  746          POPULUS TREMULOIDES                QUAKING ASPEN
  case('74606')
     if (D .ge. OMIND) then
        CW = 4.0910*D**0.5907
     else
        CW = (4.0910*OMIND**0.5907)*(D/OMIND)
     end if
     if (CW .gt.45.) CW = 45.

     !  CASE 74608 AK EQN 08
     !  746          POPULUS TREMULOIDES - AS          QUAKING ASPEN
  case('74608')
     if (D .ge.0.1) then
        CW = 2.386015 * (D**0.63014)*(H**(-0.147121))*(CL**0.274356)
     else
        CW = 0.5
     end if
     if (CW .gt.48.) CW = 48.

     !  CASE 74705 CROOKSTON (R6) MODEL 2
     !  747          POPULUS TRICHOCARPA                BLACK COTTONWOOD
  case('74705')
     if (D .ge. OMIND) then
        CW = 4.4327*BF*(D**0.41505)*(H**(-0.23264))*(CL**0.41477)
     else
        CW = (4.4327*BF*(OMIND**0.41505)*(H**(-0.23264))*&
             &(CL**0.41477))*(D/OMIND)
     end if
     if (CW .gt.56.) CW = 56.

     !  CASE 74708 AK EQN 08
     !  747          POPULUS TRICHOCARPA - CW           BLACK COTTONWOOD
  case('74708')
     if (D .ge.0.1) then
        CW = 0.790658 * (D**0.551987) * (H**0.446434)&
             &* (EXP(EL)**(-0.048415))
     else
        CW = 0.5
     end if
     if (CW .gt.56.) CW = 56.

     !  CASE 74902 BECHTOLD 2004 MODEL 2
     !  749          POPULUS ANGUSTIFOLIA               NARROWLEAF COTTONWOOD
  case('74902')
     if (HI .lt. -26.) HI = -26.
     if (HI .gt.  -2.) HI = -2.
     if (D .ge. MIND) then
        CW = 4.1687 + 1.5355*D + 0.1275*HI
     else
        CW = (4.1687 + 1.5355*MIND + 0.1275*HI)*(D/MIND)
     end if
     if (CW .gt.35.) CW = 35.

     !  CASE 80102 BECHTOLD 2004 MODEL 2
     !  801          QUERCUS AGRIFOLIA                 COASTAL LIVE OAK
  case('80102')
     if (HI .lt. -73.) HI = -73.
     if (HI .gt. -54.) HI = -54.
     if (D .ge. MIND) then
        CW = -16.1696 + 1.7456*D + 0.0925*CR + (-0.1956*HI)
     else
        CW = (-16.1696+1.7456*MIND+0.0925*CR+(-0.1956*HI))*(D/MIND)
     end if
     if (CW .gt.53.) CW = 53.

     !  CASE 80502 BECHTOLD 2004 MODEL 2
     !  805          QUERCUS CHRYSOLEPIS                CANYON LIVE OAK
  case('80502')
     if (HI .lt. -60.) HI = -60.
     if (HI .gt.  -5.) HI = -5.
     if (D .ge. MIND) then
        CW = 0.2738 + 1.0534*D + 0.035*CR + (-0.1385*HI)
     else
        CW = (0.2738+1.0534*MIND+0.035*CR+(-0.1385*HI))*(D/MIND)
     end if
     if (CW .gt.49.) CW = 49.

     !  CASE 80702 BECHTOLD 2004 MODEL 2
     !  807          QUERCUS DOUGLASII                  BLUE OAK
  case('80702')
     if (D .ge. MIND) then
        CW = 2.7110 + 1.5159*D + 0.0415*CR+(-0.0271*BAREA)
     else
        CW = (2.7110+1.5159*MIND+0.0415*CR+(-0.0271*BAREA))*(D/MIND)
     end if
     if (CW .gt.61.) CW = 61.

     !  CASE 81402 BECHTOLD 2004 MODEL 2
     !  814          QUERCUS GAMBELII                   GAMBEL OAK
  case('81402')
     if (D .ge. MIND) then
        CW = 0.3309 + 0.8918*D + 0.0510*CR
     else
        CW = (0.3309 + 0.8918*MIND+ 0.0510*CR)*(D/MIND)
     end if
     if (CW .gt.19.) CW = 19.

     !  CASE 81502 BECTHOLD 2004 MODEL 2
     !  815          QUERCUS GARRYANA                   OREGON WHITE OAK
  case('81502')
     if (D .ge. MIND) then
        CW = -1.3160 + 2.9311*D + (-0.0866*D*D)
     else
        CW = (-1.3160 + 2.9311*MIND+(-0.0866*MIND*MIND))*(D/MIND)
     end if
     if (CW .gt.30.) CW = 30.

     !  CASE 81505 CROOKSTON (R6) MODEL 2
     !  815          QUERCUS GARRYANA                   OREGON WHITE OAK
  case('81505')
     if (D .ge. OMIND) then
        CW = 2.4857*BF*(D**0.70862)*(CL**0.10168)
     else
        CW = (2.4857*BF*(OMIND**0.70862)*(CL**0.10168))*(D/OMIND)
     end if
     if (CW .gt.39.) CW = 39.

     !  CASE 81506 DONNELLY (R6)
     !  815          QUERCUS GARRYANA                   OREGON WHITE OAK
  case('81506')
     if (D .ge. OMIND) then
        CW = 2.4922*D**0.8544
     else
        CW = (2.4922*OMIND**0.8544)*(D/OMIND)
     end if
     if (CW .gt.30.) CW = 30.

     !  CASE 81802 BECTHOLD 2004 MODEL 2
     !  818          QUERCUS KELLOGGII                  CALIFORNIA BLACK OAK
  case('81802')
     if (HI .lt. -47.) HI = -47.
     if (HI .gt.  -8.) HI = -8.
     if (D .ge. MIND) then
        CW = 1.6306 + 0.9867*D + 0.0556*CR + (-0.1199*HI)
     else
        CW = (1.6306+0.9867*MIND+0.0556*CR+(-0.1199*HI))*(D/MIND)
     end if
     if (CW .gt.52.) CW = 52.

     !  CASE 82102 BECTHOLD 2004 MODEL 2
     !  821          QUERCUS LOBATA                  VALLEY OAK
  case('82102')
     if (D .ge. MIND) then
        CW = -2.1068 + 1.9385*D + 0.0860*CR
     else
        CW = (-2.1068 + 1.9385*MIND + 0.0860*CR)*(D/MIND)
     end if
     if (CW .gt.47.) CW = 47.

     !  CASE 83902 BECHTOLD 2004 MODEL 2
     !  839          QUERCUS WISLIZENII                 INTERIOR LIVE OAK
  case('83902')
     if (HI .lt. -60.) HI = -60.
     if (HI .gt.  -5.) HI = - 5.
     if (D .ge. MIND) then
        CW = 0.7146 + 1.5460*D + (-0.1121*HI)
     else
        CW = (0.7146 + 1.5460*MIND + (-0.1121*HI))*(D/MIND)
     end if
     if (CW .gt.37.) CW = 37.

     !  CASE 98102 BECTHOLD 2004 MODEL 2
     !  981           UMBELLULARIA CALIFORNICA          CALIFORNIA LAUREL
  case('98102')
     if (D .ge. MIND) then
        CW = 2.4247 + 1.3174*D + 0.0786*CR
     else
        CW = (2.4247 + 1.3174*MIND + 0.0786*CR)*(D/MIND)
     end if
     if (CW .gt.44.) CW = 44.

  end select
  9000 continue

  !  LIMIT CROWN WIDTH

  if (CW .lt.0.5) CW = 0.5
  if (CW .gt.99.9) CW = 99.9
  return
end
