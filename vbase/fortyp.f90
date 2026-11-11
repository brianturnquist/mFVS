!====================!
!   vbase : fortyp   !
!====================!
! VBASE $Id$
subroutine FORTYP(IXF,SDIFIA)
  implicit none

  !  THIS ROUTINE EXECUTES A DECISION TREE TO ESTIMATE FOREST TYPE
  !  BASED OF STOCKING LEVELS.  THE STOCKING LEVELS ARE CALCULATED
  !  IN THE ROUTINE STKVAL, AND STOCKING VALUES FOR GROUPS OF INITAL
  !  FOREST TYPES ARE CATAGORIZED AND SUMMED.  THIS DECISION
  !  TREE IS BASED ON ARNER 2001.  THIS FORTRAN CODE WAS DEVELOPED
  !  FROM THE FIA ALGORITHM PROTOCOLS.  TO PROVIDE ADEQUATE CODE
  !  DOCUMENTATION, STANDARD FVS CODING FORMAT HAS BEEN RELAXED TO ALLOW
  !  THE USE OF EXCLAMATION MARKS (!) TO PLACE COMMENTS AT THE
  !  END OF VALID FORTRAN STATEMENTS.

  !  VARIABLES
  !  SDIFIA  --  SDIMAX BASED ON FIA FOREST TYPE
  !  IXF     --  SOURCE OF CALL TO FORTYP
  !              = 1 - CALL IS FROM INITRE
  !              = 2 - CALL IS FROM DISPLY, OR LDSUM (PPE)
  !              = 3 - CALL IS FROM CRATET (SN VARIANT ONLY)
  !  IXF         = 4 - CALL IS FROM MORTS  (SN VARIANT ONLY)
  !  FTSDMX  --  FOREST TYPE CODE (FIATYP) AND SN MAXIMUM SDI (SDIFIA)
  !       S  --  THIS ARRAY IS LOADED WITH STOCKING VALUES FROM
  !              THE STKVAL ROUTINE ACCORDING TO INITIAL FOREST TYPE.
  !              DIMENSIONED AS 210 IN ARNER 2001
  !  LFLAGV  --  LOGICAL VARIABLE FOR VARIANT-SPECIFIC USE
  !              SN VARIANT -- USED TO PREVENT (=.T.) RESETING FOREST
  !              TYPE CALCULATION FROM CYCLE TO CYCLE. SET IN IINITRE
  !              AND CARRIED IN PLOT.F77

  !OMMON BLOCKS

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'
  include 'VARCOM.f90'
  include 'ARRAYS.f90'

  !  DECLARATIONS

  ! Variables
  logical :: DEBUG
  logical :: LFORVP
  integer :: IRGN,NINTY5,NINETY
  integer :: IXF,J,I,IFT,JFT,ISNVAR
  real    :: S(210)
  real    :: FTSDMX(141,2)
  real    :: INSCDR,INTMAPLW,JACKPIN,JAPBLKP,JFBCDGFR,JUNIPRW,KNBPIN,LMBRPIN,LNGLFP,LNGLFSLH,LOBP,&
       &LOBSHRTP,LODGPOLE,LRCHPIN,LSPRFIR,LWELASCW,LWOKGMCY,MANGRV,MBB,MELUCA,MESQUITW,MISCWHWD,MISCWSFW,&
       &MNTRYPIN,MPLBASWD,MPLBCHYB,MTNBRSHW,MTNHMLK,MXDHWD,NOBLFIR,NROAK,NWHCDR
  real    :: QOAK2,UPLAND,P1,P2,P3,EIGHTY,FIFTN,TEN,FIVE,HALF,PCOND
  real    :: UPOAKHIC,UPMPBCBR,TOTSTK,OTHSPC,OTHHRD,OTHSFT,OTHALL,PINOAK
  real    :: SWEETGUM,HOLLY,BEECH,BLKGUM,ECTNWD,WHASH,SLVRMPL
  real    :: TWENTY,QUARTR,WCELM,AELM,SROAK,UPLOHWD,UPLOOAK,OTHEXHWD
  real    :: EUCLPT,PALONIA,EXHWDS,PALM,PACMDRN,OTHWHWD,GNTCHNK
  real    :: TROPHWDS,CLFLREL,TANOAK,TANOKLRL,EOAKWDLD,DOAKWDLD,CANYLO
  real    :: COSTLO,GRAYPINE,BLUEOAK,ORGWHO,CLFBO,WSTOAK,BGLFMPL,REDALDR
  real    :: ALDRMAPL,PAPRBRCH,BLSMPOP,ASPEN,ASPBRCH,ELMASHLO,BCWAYP,BLCH
  real    :: ORGNASH,CTNWWILO,SLVMAELM,SGBELMGA,BAELMMAP,HRDWDS,OAKPIN
  real    :: OAKHCK,WHOAK,BUROAK,CHSTOAK,SCRLTOAK,YP,BWALNUT,BLCST
  real    :: REDMAPL,PSTBLJKO,CHBLSC,WOROHK,YPWORO,SCRUBOAK,SWGYP
  real    :: SASFPRSM,OKGMCYP,ATLWCDR,SBSTRM,SCHCHBO,SWGWILO,CYPWTUP
  real    :: OVRCUPWH,ELMASHCW,CTNWD,WILLOW,RBRCHSYC,SYCPCELM,OTHEXSWD
  real    :: SCOTCHP,EXSFTWD,TAMRCK,BLKSPR,WHSPR,REDSPR,BLSMFIR,BFIRSPR
  real    :: USPRFIR,ESPRFIR,PINJUNW,WJUNIPR,RMJUNIPR,ERCEDAR,PNYNJN
  real    :: SPRUCEP,PITCHP,PONDP,TBLMTNP,SANDP,VIRGP,SHRTLFP,SLASHP
  real    :: REDPIN,EHMLCK,EWHPIN,WHPHEM,RWJPIN,EPINE,GNTSEQ,REDWOOD
  real    :: REDWDS,WHBRKPIN,FXTLPIN,BSHPPIN,SWHTPIN,OTHWPIN,STKASPR
  real    :: STKSPHM,SUGRPIN,PORFCDR,PNDROSA,DGFRPIN,WRCEDAR,WLRCH,DGFIR
  real    :: DGFRLR,WWHPIN,AKYLCDR,REDFIR,GRNDFIR,WHTFIR,PSLVRFIR
  real    :: TRUFIR,WHMLCK,WHMLCKS,BLUSPR,ENGSPR,SALPFIR,ENGSAFIR
  real    :: SPSAFIR,TRFIRSPR,DFLRWP,SFTWDS,FIATYP,SOUTH,SDIFIA

  !  BLOCK DATA STATEMENTS
  data ((FTSDMX(I,J),J = 1,1),I = 1,100) /&
       &101, 102, 103, 104, 105, 121, 122, 123, 124, 125,&
       &126, 127, 141, 142, 161, 162, 163, 164, 165, 166,&
       &167, 168, 181, 182, 183, 184, 185, 201, 202, 221,&
       &222, 223, 224, 241, 261, 262, 263, 264, 265, 266,&
       &267, 268, 269, 270, 271, 281, 301, 304, 305, 321,&
       &341, 342, 361, 362, 363, 364, 365, 366, 367, 368,&
       &371, 381, 383, 401, 402, 403, 404, 405, 406, 407,&
       &409, 501, 502, 503, 504, 505, 506, 507, 508, 509,&
       &510, 511, 512, 513, 514, 515, 519, 520, 601, 602,&
       &605, 606, 607, 608, 701, 702, 703, 704, 705, 706/
  data ((FTSDMX(I,J),J = 1,1),I = 101,141) /&
       &707, 708, 709, 722, 801, 802, 803, 805, 807, 809,&
       &901, 902, 904, 911, 912, 921, 922, 923, 924, 925,&
       &926, 931, 932, 941, 942, 943, 951, 952, 953, 954,&
       &955, 981, 982, 991, 992, 993, 995, 996, 997, 998,&
       &999/
  data ((FTSDMX(I,J),J = 2,2),I = 1,100) /&
       &520, 520, 520, 535, 460, 460, 460, 460, 460, 460,&
       &460, 460, 390, 435, 505, 505, 495, 365, 415, 475,&
       &465, 350, 300, 415, 415, 415, 415, 560, 690, 530,&
       &690, 460, 460, 580, 735, 765, 735, 735, 735, 735,&
       &735, 735, 735, 735, 770, 675, 735, 770, 735, 650,&
       &730, 730, 460, 645, 460, 460, 470, 470, 470, 470,&
       &580, 520, 520, 460, 300, 360, 475, 480, 475, 555,&
       &495, 380, 380, 415, 430, 400, 440, 500, 440, 470,&
       &360, 455, 405, 295, 300, 420, 475, 440, 395, 460,&
       &425, 300, 785, 625, 415, 420, 450, 495, 470, 415/
  data ((FTSDMX(I,J),J = 2,2),I = 101,141) /&
       &445, 445, 445, 550, 460, 325, 455, 485, 415, 555,&
       &725, 550, 725, 550, 550, 460, 400, 550, 550, 470,&
       &550, 550, 550, 550, 550, 525, 525, 525, 525, 525,&
       &550, 550, 550, 525, 525, 525, 525, 470, 470, 470,&
       &380/

  !  CHECK FOR DEBUG

  call DBCHK (DEBUG,'FORTYP',6,ICYC)
  if (DEBUG) write (JOSTND,*) ' IN FORTYP_1: ICYC,IXF,IFORTP = ',&
       &ICYC,IXF,IFORTP

  P1 = 0.
  P2 = 0.
  P3 = 0.
  IFT = 0
  JFT = 0
  SOUTH = 0.

  !  THE FIRST CALL TO FORTYP IS FROM SITSET TO SET INITIAL FOREST TYPE
  !  IF IFORTP = 0, IMPLIES NOT SET BY USER, OR INVALID FOREST TYPE.
  !              IFORTP IS SET TO IFORTP= 0 IN CALL FROM SITSET.
  !              THEN CALCULATE FOREST TYPE EVERY CYCLE.
  !  IF FIATYP = VALID NUMBER SET BY USER AND LFLAGV= .F.
  !              THEN INPUT FOREST TYPE FOR FIRST CYCLE AND
  !              RECALCULATE FOR REMAINING CYCLES. SET SDIMAX FOR SN
  !  IF FIATYP = VALID NUMBER SET BY USER AND LFLAGV= .T.
  !              THEN INPUT FOREST TYPE AS CONSTANT USED FOR ALL CYCLES.
  !              SET SDIMAX FOR SN.

  !  ESTABLISH FLAG FOR SN VARIANT

  select case (VARACD)
  case ('SN')
     ISNVAR = 1
  case default
     ISNVAR = 0
  end select

  !  CHECK FOR VALID INPUT FOREST TYPE

  FIATYP = FLOAT(IFORTP)
  if (IXF.eq.1) then ! IXF=1 THEN CALL WAS FROM 'INITRE'
     if (FIATYP.le.0.) go to 9999 ! IF FIATYP IS 0 THEN CALCULATE FT EVERY CYCLE
     LFORVP = .false.
     do I = 1,141
        if (FTSDMX(I,1).eq.FIATYP) LFORVP = .true.
        40 continue
     end do
     if (.not.LFORVP) then ! IF INVALID FOREST TYPE THEN WRITE WARNING MESSAGE
        write (JOSTND,45)
        45 format(/' ********   WARNING, INVALID FOREST TYPE WAS INP',&
             &'UT BY USER, FOREST TYPE WILL BE CALCULATED BY MODEL')
        IFORTP = 0
        FIATYP = 0.
        LFLAGV = .false.
     end if
     go to 9999
  end if
  JFT = IFORTP
  SDIFIA = 0.

  !  INITIALIZE S(210) ARRAY WHICH REPRESENTS ITG GROUPS

  do I = 1,210
     S(I) = 0.
  end do

  !   CALL STKVAL SUBROUTINE TO CALCULATE STOCKING FOR THE STAND

  if (DEBUG) write (JOSTND,*) ' IN FORTYP_2: CALLING STKVAL ...'
  call STKVAL(S)

  if ((ICYC.gt.1).and.LFLAGV)go to 9999

  !     *********************************************************************************************;
  !     * BEGIN -- ASSIGNMENT OF INITIAL TYPES TO COMBINED TYPE GROUPS; (ARNER 2/13/2002 - TABLE 5)
  !     *********************************************************************************************;
  ! --- SOFTWOODS
  SFTWDS = 0. ! A. SOFTWOODS (1-58,60,62-79,161,162)
  do I = 1,58
     SFTWDS = SFTWDS + S(I)
  end do
  SFTWDS = SFTWDS+S(60)
  do I = 62,79
     SFTWDS = SFTWDS + S(I)
  end do
  SFTWDS = SFTWDS+S(161)+S(162)+S(170)
  TRFIRSPR = 0. ! A.B. TRUE FIRS AND SPRUCE (1-5,7,9,14,15,28,34,35)
  do I = 1,5
     TRFIRSPR = TRFIRSPR + S(I)
  end do
  TRFIRSPR = TRFIRSPR+S(7)+S(9)+S(14)+S(15)+S(28)+S(34)+S(35)
  SPSAFIR = S(4)+S(14)+S(15) ! A.B.C. SPRUCE-SUBALPINE FIR (4,14,15)
  ENGSAFIR = S(4)+S(14) ! A.B.C.D. ENGELMANN SPRUCE-SUBALPINE FIR (4,14)
  SALPFIR = S(4) ! A.B.C.D.E. SUBALPINE FIR (4)
  ENGSPR = S(14) ! A.B.C.D.E. ENGLEMAN SPRUCE (14)
  BLUSPR = S(15) ! A.B.C.D. BLUE SPRUCE (15)
  WHMLCKS = S(34)+S(35) ! A.B.C. WESTERN HEMLOCKS (34,35)
  WHMLCK = S(34) ! A.B.C.D. WESTERN HEMLOCK (34)
  MTNHMLK = S(35) ! A.B.C.D. MOUNTAIN HEMLOCK (35)
  TRUFIR = 0. ! A.B.C. TRUE FIRS (1-5,7)
  do I = 1,5
     TRUFIR = TRUFIR+S(I)
  end do
  TRUFIR = TRUFIR+S(7)
  PSLVRFIR = S(1) ! A.B.C.D. PACIFIC SILVER FIR (1)
  WHTFIR = S(2) ! A.B.C.D. WHITE FIR (2)
  GRNDFIR = S(3) ! A.B.C.D. GRAND FIR (3)
  SALPFIR = S(4) ! A.B.C.D. SUBALPINE FIR (4)
  REDFIR = S(5) ! A.B.C.D. RED FIR (5)
  NOBLFIR = S(7) ! A.B.C.D. NOBLE FIR (7)
  AKYLCDR = S(9) ! A.B.C. ALASKA YELLOW CEDAR (9)
  WWHPIN = S(28) ! A.B.C. WESTERN WHITE PINE (28)
  DFLRWP = S(8)+S(10)+S(11)+S(13)+S(23)+S(24)+&
       &S(26)+S(27)+S(30)+S(31)+S(36) ! A.B. DF-LARCH-W.WHITE PINES (8,10,11,13,23,24,26,27,30,31,36)
  DGFRLR = S(11)+S(13)+S(31) ! A.B.C. DF-WESTERN LARCH (11,13,31)
  DGFIR = S(31) ! A.B.C.D. DOUG FIR (31)
  WLRCH = S(13) ! A.B.C.D. WESTERN LARCH (13)
  WRCEDAR = S(11) ! A.B.C.D. WESTERN REDCEDAR (11)
  DGFRPIN = S(8)+S(10)+S(23)+S(24)+S(26)+S(27)+S(30)+S(31)+S(36) ! A.B.C. DF-WESTERN PINES (8,10,23,24,26,27,30,31,36)
  DGFIR = S(31) ! A.B.C.D. DOUG FIR (31)
  PNDROSA = S(26)+S(36) ! A.B.C.D. PONDEROSA PINE (26,36)
  LODGPOLE = S(23) ! A.B.C.D. LODGEPOLE PINE (23)
  PORFCDR = S(8) ! A.B.C.D. PORT ORFORD CEDAR (8)
  SUGRPIN = S(27) ! A.B.C.D. SUGAR PINE (27)
  INSCDR = S(10) ! A.B.C.D. INCENSE CEDAR (10)
  JFBCDGFR = S(24)+S(30)+S(36) ! A.B.C.D. JEFFREY-COULTER PINE-BIGCONE DF (24,30,36)
  LRCHPIN = S(13)+S(23)+S(26)+S(36) ! A.B.C. WESTERN LARCH-PINE (13,23,26,36)
  WLRCH = S(13) ! A.B.C.D. WESTERN LARCH (13)
  PNDROSA = S(26)+S(36) ! A.B.C.D. PONDEROSA PINE (26,36)
  LODGPOLE = S(23) ! A.B.C.D. LODGEPOLE PINE (23)
  STKSPHM = S(11)+S(18)+S(34) ! A.B. SITKA SPRUCE-HEMLOCK (11,18,34)
  WHMLCK = S(34) ! A.B.C. WESTERN HEMLOCK (34)
  STKASPR = S(18) ! A.B.C. SITKA SPRUCE (18)
  WRCEDAR = S(11) ! A.B.C. WESTERN REDCEDAR (11)
  OTHWPIN = S(6)+S(12)+S(19)+S(20)+S(21)+S(22)+S(25)+S(29)+S(40) ! A.B. OTHER WESTERN PINES (6,12,19,20,21,22,25,29,40)
  KNBPIN = S(21) ! A.B.C. KNOBCONE PINE (21)
  SWHTPIN = S(22) ! A.B.C. SOUTHWEST WHITE PINE (22)
  BSHPPIN = S(6) ! A.B.C. BISHOP PINE (6)
  MNTRYPIN = S(29) ! A.B.C. MONTEREY PINE (29)
  FXTLPIN = S(20) ! A.B.C. FOXTAIL-BRISTLECONE PINE (20)
  LMBRPIN = S(25) ! A.B.C. LIMBER PINE (25)
  WHBRKPIN = S(19) ! A.B.C. WHITEBARK PINE (19)
  MISCWSFW = S(12)+S(40) ! A.B.C. MISC. W. SOFTWOODS (12,40)
  REDWDS = S(31)+S(32)+S(33) ! A.B. REDWOODS (31,32,33)
  REDWOOD = S(32) ! A.B.C. REDWOOD (32)
  GNTSEQ = S(33) ! A.B.C. GIANT SEQUOIA (33)
  DGFIR = S(31) ! A.B.C. DOUG FIR (31)
  EPINE = 0. ! A.B. EASTERN PINES (41-54,66)
  do I = 41,54
     EPINE = EPINE+S(I)
  end do
  EPINE = EPINE+S(66)
  RWJPIN = S(41)+S(42)+S(53)+S(66) ! A.B.C. RED-WHITE-JACK PINE (41,42,53,66)
  WHPHEM = S(53)+S(66) ! A.B.C.D. WHITE PINE-HEMLOCK (53,66)
  EWHPIN = S(53) ! A.B.C.D.E. EASTERN WHITE PINE (53)
  EHMLCK = S(66) ! A.B.C.D.E. EASTERN HEMLOCK (66)
  REDPIN = S(42) ! A.B.C.D. RED PINE (42)
  JACKPIN = S(41) ! A.B.C.D. JACK PINE (41)
  LNGLFSLH = S(46)+S(48) ! A.B.C. LONGLEAF - SLASH PINE (46,48)
  LNGLFP = S(48) ! A.B.C.D. LONGLEAF PINE (48)
  SLASHP = S(46) ! A.B.C.D. SLASH PINE (46)
  LOBSHRTP = S(44)+S(45)+S(47)+S(49)+S(50)+S(51)+S(52)+S(54) ! A.B.C. LOBLOLLY-SHORTLEAF PINE (44,45,47,49-52,54)
  LOBP = S(52) ! A.B.C.D. LOBLOLLY PINE (52)
  SHRTLFP = S(45) ! A.B.C.D. SHORTLEAF PINE (45)
  VIRGP = S(54) ! A.B.C.D. VIRGINIA PINE (54)
  SANDP = S(44) ! A.B.C.D. SAND PINE (44)
  TBLMTNP = S(49) ! A.B.C.D. TABLE MOUNTAIN PINE (49)
  PONDP = S(51) ! A.B.C.D. POND PINE (51)
  PITCHP = S(50) ! A.B.C.D. PITCH PINE (50)
  SPRUCEP = S(47) ! A.B.C.D. SPRUCE PINE (47)
  PNYNJN = S(38)+S(63)+S(64)+S(161)+S(162) ! A.B. PINYON-JUNIPER (38,63,64,161,162)
  ERCEDAR = S(64) ! A.B.C. EASTERN REDCEDAR (64)
  RMJUNIPR = S(63) ! A.B.C. ROCKY MOUNTAIN JUNIPER (63)
  WJUNIPR = S(38) ! A.B.C. WESTERN JUNIPER (38)
  JUNIPRW = S(161) ! A.B.C. JUNIPER WOODLAND (161)
  PINJUNW = S(161)+S(162) ! A.B.C. PINYON-JUNIPER WOODLAND (161,162)
  ESPRFIR = S(16)+S(17)+S(55)+S(58)+S(60)+S(65) ! A.B. EASTERN SPRUCE-FIR (16,17,55,58,60,65)
  USPRFIR = S(16)+S(55)+S(58) ! A.B.C. UPLAND SPRUCE-FIR (16,55,58)
  BFIRSPR = S(55)+S(58) ! A.B.C.D. BALSAM FIR - RED SPRUCE (55,58)
  BLSMFIR = S(55) ! A.B.C.D.E. BALSAM FIR (55)
  REDSPR = S(58) ! A.B.C.D.E. RED SPRUCE (58)
  WHSPR = S(16) ! A.B.C.D. WHITE SPRUCE (16)
  LSPRFIR = S(17)+S(60)+S(65) ! A.B.C. LOWLAND SPRUCE-FIR (17,60,65)
  BLKSPR = S(17) ! A.B.C.D. BLACK SPRUCE (17)
  TAMRCK = S(65) ! A.B.C.D. TAMARACK (65)
  NWHCDR = S(60) ! A.B.C.D. NORTHERN WHITE CEDAR (60)
  EXSFTWD = S(70)+S(71)+S(72) ! A.B. EXOTIC SOFTWOODS (70,71,72)
  SCOTCHP = S(71) ! A.B.C. SCOTCH PINE (71)
  JAPBLKP = S(72) ! A.B.C. JAPANESE BLACK PINE (72)
  OTHEXSWD = S(70) ! A.B.C. OTHER EXOTIC SOFTWOODS (70)
  ! --- HARDWOODS
  HRDWDS = 0. ! A. HARDWOODS (59,61,81-153,156-160,163,201-210)
  do I = 81,153
     HRDWDS = HRDWDS+S(I)
  end do
  do I = 156,160
     HRDWDS = HRDWDS+S(I)
  end do
  do I = 201,210
     HRDWDS = HRDWDS+S(I)
  end do
  HRDWDS = HRDWDS+S(59)+S(61)+S(163)+S(180)+S(190)
  OAKPIN = 0. ! A.B. OAK-PINE (41,42,44-54,64)
  do I = 41,54
     OAKPIN = OAKPIN+S(I)
  end do
  OAKPIN = OAKPIN+S(64)
  ERCEDAR = S(64) ! A.B.C. EASTERN REDCEDAR (64)
  SHRTLFP = S(45) ! A.B.C. SHORTLEAF PINE (45)
  EWHPIN = S(53) ! A.B.C. EASTERN WHITE PINE (53)
  LNGLFP = S(48) ! A.B.C. LONGLEAF PINE (48)
  VIRGP = S(54) ! A.B.C. VIRGINIA PINE (54)
  LOBP = S(52) ! A.B.C. LOBLOLLY PINE (52)
  SLASHP = S(46) ! A.B.C. SLASH PINE (46)
  JACKPIN = S(41) ! A.B.C. JACK PINE (41)
  REDPIN = S(42) ! A.B.C. RED PINE (42)
  SANDP = S(44) ! A.B.C. SAND PINE (44)
  SPRUCEP = S(47) ! A.B.C. SPRUCE PINE (47)
  TBLMTNP = S(49) ! A.B.C. TABLE MOUNTAIN PINE (49)
  PITCHP = S(50) ! A.B.C. PITCH PINE (50)
  PONDP = S(51) ! A.B.C. POND PINE (51)
  OAKHCK = 0. ! A.B. OAK-HICKORY (81-86,88,89,92,93,101,108,110,120,122,202,206,207)
  do I = 81,86
     OAKHCK = OAKHCK+S(I)
  end do
  OAKHCK = OAKHCK+S(88)+S(89)+S(92)+S(93)+S(101)+S(108)+S(110)&
       &+S(120)+S(122)+S(202)+S(206)+S(207)
  WHOAK = S(81) ! A.B.C. WHITE OAK (81)
  BUROAK = S(83) ! A.B.C. BUR OAK (83)
  CHSTOAK = S(84) ! A.B.C. CHESTNUT OAK (84)
  NROAK = S(85) ! A.B.C. NORTHERN RED OAK (85)
  SCRLTOAK = S(82) ! A.B.C. SCARLET OAK (82)
  YP = S(110) ! A.B.C. YELLOW POPLAR (110)
  BWALNUT = S(108) ! A.B.C. BLACK WALNUT (108)
  BLCST = S(122) ! A.B.C. BLACK LOCUST (122)
  REDMAPL = S(95) ! A.B.C. RED MAPLE (95)
  PSTBLJKO = S(86)+S(206) ! A.B.C. POST-BLACKJACK OAK (86,206)
  CHBLSC = S(82)+S(84)+S(120) ! A.B.C. CHESTNUT-BLACK/SCARLET OAK (82,84,120)
  WOROHK = S(81)+S(85)+S(92)+S(94)+S(120)+S(207) ! A.B.C. WHITE OAK-RED OAK-HICKORY (81,85,92,94,120,207)
  YPWORO = S(81)+S(85)+S(110) ! A.B.C. YELLOW POPLAR-WH.OAK-RED OAK (81,85,110)
  SCRUBOAK = S(89)+S(203)+S(206) ! A.B.C. SOUTHERN SCRUB OAK (89,203,206)
  SWGYP = S(109)+S(110) ! A.B.C. SWTGUM-YELLOW POPLAR (109,110)
  SASFPRSM = S(93) ! A.B.C. SASSAFRAS-PERSIMMON (93)
  MXDHWD = 0. ! A.B.C. MIXED UPL. HRDWDS (83,88,94,101,106,108,113,122,125,201-204)
  MXDHWD = MXDHWD+S(83)+S(88)+S(94)+S(101)+S(106)+S(108)&
       &+S(113)+S(122)+S(125)
  do I = 201,204
     MXDHWD = MXDHWD+S(I)
  end do
  OKGMCYP = 0. ! A.B. OAK-GUM-CYPRESS (59,61,87,90,111,112,114,127,128,143)
  OKGMCYP = S(59)+S(61)+S(87)+S(90)+S(111)+S(112)+S(114)&
       &+S(127)+S(128)+S(143)
  ATLWCDR = S(59) ! A.B.C. ATLANTIC WHITE CEDAR (59)
  SBSTRM = S(95)+S(111)+S(113)+S(114)+S(127) ! A.B.C. SWEETBAY-SWAMP TUPELO-RED MAPLE (95,111,113,114,127)
  SCHCHBO = S(87) ! A.B.C. SWAMP CHESTNUT-CHERRYBARK OAK (87)
  SWGWILO = S(109)+S(125)+S(143)+S(201)+S(203)+S(204) ! A.B.C. SWTGUM-NUTTALL-WILLOW OAK (109,125,143,201,203,204)
  CYPWTUP = S(61)+S(112) ! A.B.C. CYPRESS-WATER TUPELO (61,112)
  OVRCUPWH = S(90)+S(128) ! A.B.C. OVERCUP OAK-WATER HICKORY (90,128)
  ELMASHCW = 0. ! A.B. ELM-ASH-COTTONWOOD (91,97,100,104,115,116,118,123,129,135,137,208)
  ELMASHCW = S(91)+S(97)+S(100)+S(104)+S(115)+S(116)+S(118)+S(123)&
       &+S(129)+S(135)+S(137)+S(208)
  CTNWD = S(118)+S(137) ! A.B.C. COTTONWOOD (118,137)
  WILLOW = S(123) ! A.B.C. WILLOW (123)
  REDMAPL = S(95) ! A.B.C. RED MAPLE (95)
  RBRCHSYC = S(108)+S(116)+S(123)+S(129) ! A.B.C. RIVER BIRCH-SYCAMORE (108,116,123,129)
  SYCPCELM = S(91)+S(94)+S(109)+S(116) ! A.B.C. SYCAMORE-PECAN-ELM (91,94,109,116)
  BAELMMAP = S(104) ! A.B.C. BLACK ASH-ELM-RED MAPLE (104)
  SGBELMGA = S(94)+S(100)+S(105)+S(115)+S(208)+S(209) ! A.B.C. SUGARBERRY-HACKBERRY-ELM-GREEN ASH (94,100,105,115,208,209)
  SLVMAELM = S(94)+S(97) ! A.B.C. SILVER MAPLE-AMERICAN ELM (94,97)
  CTNWWILO = S(118)+S(123)+S(130)+S(131)+S(137) ! A.B.C. COTTONWOOD-WILLOW (118,123,130,131,137)
  ORGNASH = S(135) ! A.B.C. OREGON ASH (135)
  MBB = S(66)+S(96)+S(98)+S(107)+S(110)+S(122)+S(124) ! A.B. MAPLE-BEECH-BIRCH (66,96,98,107,110,122,124)
  BLCH = S(121) ! A.B.C. BLACK CHERRY (121)
  REDMAPL = S(95) ! A.B.C. RED MAPLE (95)
  BCWAYP = S(103)+S(110)+S(121) ! A.B.C. BLACK CHERRY-WHITE ASH-YELLOW POPLAR (103,110,121)
  MPLBASWD = S(96)+S(124) ! A.B.C. MAPLE-BASSWOOD (96,124)
  ELMASHLO = S(94)+S(105)+S(122) ! A.B.C. ELM-ASH-LOCUST (94,105,122)
  MPLBCHYB = S(66)+S(94)+S(95)+S(96)+S(98)+S(102)+S(105)+S(107)+S(108) ! A.B.C. MAPLE-BEECH-BIRCH (66,94,95,96,98,102,105,107,108)
  ASPBRCH = S(99)+S(117)+S(119) ! A.B. ASPEN-BIRCH (99,117,119)
  ASPEN = S(119) ! A.B.C. ASPEN (119)
  BLSMPOP = S(117) ! A.B.C. BALSAM POPLAR (117)
  PAPRBRCH = S(99) ! A.B.C. PAPER BIRCH (99)
  ALDRMAPL = S(130)+S(131) ! A.B. RED ALDER-MAPLE (130,131)
  REDALDR = S(131) ! A.B.C. RED ALDER (131)
  BGLFMPL = S(130) ! A.B.C. BIGLEAF MAPLE (130)
  WSTOAK = S(134)+S(138)+S(139)+S(140)+S(142)+S(158)+S(163)+S(210) ! A.B. WESTERN OAKS (134,138,139,140,142,158,163,210)
  CLFBO = S(139) ! A.B.C. CALIFORNIA BLACK OAK (139)
  ORGWHO = S(140) ! A.B.C. OREGON WHITE OAK (140)
  BLUEOAK = S(134) ! A.B.C. BLUE OAK (134)
  GRAYPINE = S(163) ! A.B.C. GRAY PINE (163)
  COSTLO = S(138) ! A.B.C. COAST LIVE OAK (138)
  CANYLO = S(142) ! A.B.C. CANYON_INTERIOR LIVE OAK (142)
  DOAKWDLD = S(158) ! A.B.C. DECIDUOUS OAK-WOODLAND (158)
  EOAKWDLD = S(210) ! A.B.C. EVERGREEN OAK-WOODLAND (210)
  TANOKLRL = S(133)+S(136)+S(141) ! A.B. TAN OAK-LAUREL (133,136,141)
  TANOAK = S(136) ! A.B.C. TAN OAK (136)
  CLFLREL = S(141) ! A.B.C. CALIFORNIA LAUREL (141)
  GNTCHNK = S(133) ! A.B.C. GIANT CHINKAPIN (133)
  OTHWHWD = S(132)+S(156)+S(157)+S(159)+S(160) ! A.B. OTHER WESTERN HARDWOODS (132,156,157,159,160)
  PACMDRN = S(132) ! A.B.C. PACIFIC MADRONE (132)
  MESQUITW = S(157) ! A.B.C. MESQUITE WOODLAND (157)
  MTNBRSHW = S(156) ! A.B.C. MOUNTAIN BRUSH WOODLAND (156)
  INTMAPLW = S(159) ! A.B.C. INTERMOUNTAIN MAPLE WOODLAND (159)
  MISCWHWD = S(160) ! A.B.C. MISC. WESTERN HARDWOODS (160)
  TROPHWDS = S(147)+S(149) ! A.B. TROPICAL HARDWOODS (147,149)
  PALM = S(147) ! A.B.C. SABAL PALM (147)
  MANGRV = S(149) ! A.B.C. MANGROVE (149)
  EXHWDS = S(144)+S(145)+S(146)+S(148) ! A.B. EXOTIC HARDWOODS (144,145,146,148)
  PALONIA = S(144) ! A.B.C. ROYAL PAULOWNIA (144)
  MELUCA = S(145) ! A.B.C. MELALUCA (145)
  EUCLPT = S(148) ! A.B.C. EUCALYPTUS (148)
  OTHEXHWD = S(146) ! A.B.C. OTHER EXOTIC HARDWOODS (146)
  ! --- SPECIAL GROUPS
  UPLOOAK = S(125)+S(201)+S(203)+S(204) ! A. UPLAND-LOWLAND OAKS (125,201,203,204)
  UPLOHWD = S(95)+S(103)+S(105) ! A. UPLAND-LOWLAND HARDWOODS (95,103,105)
  SROAK = S(88) ! A. SOUTHERN RED OAK (88)
  AELM = S(94) ! A. AMERICAN/SLIPPERY/ROCK ELM (94)
  WCELM = S(209) ! A. WINGED/CEDAR ELM (209)
  SLVRMPL = S(97) ! A. SILVER MAPLE (97)
  WHASH = S(103) ! A. WHITE ASH (103)
  ECTNWD = S(118) ! A. EASTERN COTTONWOOD (118)
  BLCH = S(121) ! A. BLACK CHERRY (121)
  BLKGUM = S(113) ! A. BLACK GUM (113)
  BEECH = S(102) ! A. AMERICAN BEECH (102)
  HOLLY = S(106) ! A. AMERICAN HOLLY (106)
  SWEETGUM = S(109) ! A. SWEETGUM (109)
  PINOAK = S(205) ! A. PIN OAK (205)
  ! --- SPECIAL FVS GROUPS
  OTHALL = S(170)+S(180)+S(190) ! A. ALL OTHER SPECIES
  OTHSFT = S(170) ! A. OTHER SOFTWOODS
  OTHHRD = S(180) ! A. OTHER HARDWOODS
  OTHSPC = S(190) ! A. OTHER SPECIES
  ! --- TOTAL ALL ITG GROUPS
  TOTSTK = 0. ! TOTAL FOREST STOCKING (1-210)
  do I = 1,210
     TOTSTK = TOTSTK+S(I)
  end do
  !     *********************************************************************************************;
  !     * END -- ASSIGNMENT OF INITIAL TYPES TO COMBINED TYPE GROUPS;
  !     *********************************************************************************************;

  !  DEFINE SPECIAL CATETORIES FOR LATER USE IN TYPING

  !    FIA DETERMINES 'UPLAND' OR 'LOWLAND' SITE FROM PHYSIOGRAPHIC
  !    CODES.  IN FVS THAT INFO. IS NOT AVAILABLE.  FOR FVS USE,
  !    DETERMINE UPLAND/LOWLAND BY SUMMING THE ELIGIBLE STOCKING
  !    OF SPECIES THAT NORMALLY OCCUR IN THOSE SETTINGS.  THE TOTALS
  !    COMPUTED HERE ARE USED FOR E. HARDWOOD FOR. TYPE DETERMINATION.

  UPMPBCBR = 0. ! UPLAND - MAPLE-BEECH-BIRCH
  UPMPBCBR = S(66)+S(85)+S(94)+S(95)+S(96)+S(98)+S(102)+S(103)+&
       &S(105)+S(107)+S(108)+S(110)+S(121)+S(122)+S(124)
  UPOAKHIC = 0. ! UPLAND - OAK-HICKORY
  UPOAKHIC = S(81)+S(82)+S(83)+S(84)+S(85)+S(86)+S(88)+S(89)+S(92)+&
       &S(93)+S(94)+S(95)+S(101)+S(102)+S(103)+S(105)+S(106)+&
       &S(108)+S(109)+S(110)+S(113)+S(120)+S(121)+S(122)+S(125)+&
       &S(201)+S(202)+S(203)+S(204)+S(206)+S(207)+S(209)
  LWELASCW = 0. ! LOWLAND - ELM-ASH-COTTONWOOD
  LWELASCW = S(91)+S(94)+S(95)+S(97)+S(100)+S(103)+S(104)+S(105)+&
       &S(108)+S(109)+S(115)+S(116)+S(118)+S(123)+S(129)+&
       &S(130)+S(131)+S(135)+S(137)+S(208)+S(209)
  LWOKGMCY = 0. ! LOWLAND - OAK-GUM-CYPRESS
  LWOKGMCY = S(59)+S(61)+S(87)+S(90)+S(94)+S(95)+S(102)+S(103)+&
       &S(105)+S(106)+S(109)+S(111)+S(112)+S(113)+S(114)+&
       &S(125)+S(127)+S(128)+S(143)+S(201)+S(203)+S(204)+S(205)

  !  DEFINE PROPORTION-OF-TOTAL-STOCKING VARIABLES

  PCOND = 1.00
  HALF = 0.50 * TOTSTK
  QUARTR = 0.25 * TOTSTK
  FIVE = 0.05 * TOTSTK
  TEN = 0.10 * TOTSTK
  FIFTN = 0.15 * TOTSTK
  TWENTY = 0.20 * TOTSTK
  EIGHTY = 0.80 * TOTSTK
  NINETY = INT(0.90 * TOTSTK)
  NINTY5 = INT(0.95 * TOTSTK)

  !  DETERMINE FOREST SERVICE REGION NUMBER

  IRGN = 0
  if (KODFOR/10000000.ge.1) then
     IRGN = KODFOR/10000000
  else if (KODFOR/10000.ge.1) then
     IRGN = KODFOR/10000
  else if (KODFOR/100.ge.1) then
     IRGN = KODFOR/100
  end if

  !  BEGIN FOREST TYPE ALGORITHM

  IFT = 0
  if (TOTSTK/PCOND.lt.10.) then
     IFT = 999 ! 999 = NONSTOCKED
     ISZCL = 5
     ISTCL = 5
     go to 999
  end if

  !  START SECTION TO DETERMINE FOREST TYPE BASED ON COMPUTED
  !  AND TOTAL STOCKING VALUES FROM ABOVE.

  if (DEBUG) write (JOSTND,*) ' IN FORTYP_3: SFTWDS,HRDWDS,',&
       &'ISZCL,ISTCL = ', SFTWDS,HRDWDS,ISZCL,ISTCL

  if (SFTWDS.ge.HRDWDS) then ! A. SOFTWOODS
     IFT = 996 ! FVS OTHER SOFTWOODS(996)
     if (DEBUG) write (JOSTND,*) ' IN FORTYP_4: ICYC,TRFIRSPR,',&
          &'STKSPHM,REDWDS,DFLRWP,OTHWPIN,EPINE,ESPRFIR,PNYNJN,EXSFTWD,',&
          &'OTHSFT = '
     if (DEBUG)write(JOSTND,*)ICYC,TRFIRSPR,STKSPHM,REDWDS,DFLRWP,&
          &OTHWPIN,EPINE,ESPRFIR,PNYNJN,EXSFTWD,OTHSFT
     P1 = MAX(TRFIRSPR,STKSPHM,REDWDS,DFLRWP,OTHWPIN,&
          &EPINE,ESPRFIR,PNYNJN,EXSFTWD,OTHSFT)-.001
     if (P1.gt.0.) then
        if (P1.lt.TRFIRSPR) then ! A.B. TRUE FIRS AND SPRUCE
           P2 = MAX(SPSAFIR,WHMLCKS,TRUFIR,AKYLCDR,WWHPIN)-.001
           if (P2.gt.0.) then
              if (P2.lt.SPSAFIR) then
                 if (ENGSAFIR.ge.BLUSPR) then
                    if ((SALPFIR.ge.FIVE.and.SALPFIR.lt.HALF).and.&
                         &(ENGSPR.ge.FIVE.and.ENGSPR.lt.HALF)) then
                       IFT = 266 ! ENGELMANN SPRUCE-SUBALPINE(266)
                    else if (ENGSPR.ge.SALPFIR) then
                       IFT = 265 ! ENGELMANN SPRUCE(265)
                    else if (SALPFIR.gt.0.) then
                       IFT = 268 ! SUBALPINE FIR(268)
                    end if
                 else if (BLUSPR.gt.0.) then
                    IFT = 269 ! BLUE SPRUCE(269)
                 end if
              else if (P2.lt.WHMLCKS) then
                 if (WHMLCK.ge.MTNHMLK) then
                    IFT = 301 ! WESTERN HEMLOCK(301)
                 else if (MTNHMLK.gt.0.) then
                    IFT = 270 ! MOUNTAIN HEMLOCK(270)
                 end if
              else if (P2.lt.TRUFIR) then
                 P3 = MAX(PSLVRFIR,WHTFIR,GRNDFIR,SALPFIR,&
                      &REDFIR,NOBLFIR)-.001
                 if (P3.gt.0.) then
                    if (P3.lt.PSLVRFIR) then
                       IFT = 264 ! PACIFIC SILVER FIR(264)
                    else if (P3.lt.WHTFIR) then
                       IFT = 261 ! WHITE FIR(261)
                    else if (P3.lt.GRNDFIR) then
                       IFT = 267 ! GRAND FIR(267)
                    else if (P3.lt.SALPFIR) then
                       IFT = 268 ! SUBALPINE FIR(268)
                    else if (P3.lt.REDFIR) then
                       IFT = 262 ! RED FIR(262)
                    else if (NOBLFIR.gt.0.) then
                       IFT = 263 ! NOBLE FIR(263)
                    end if
                 end if
              else if (P2.lt.AKYLCDR) then
                 IFT = 271 ! ALASKA YELLOW CEDAR(271)
              else if (WWHPIN.gt.0.) then
                 IFT = 241 ! WESTERN WHITE PINE(241)
              end if
           end if
        else if (P1.lt.STKSPHM) then ! A.B. SITKA SPRUCE-HEMLOCK
           P2 = MAX(WHMLCK,STKASPR,WRCEDAR)-.001
           if (P2.gt.0.) then
              if (P2.lt.WHMLCK) then
                 IFT = 301 ! WESTERN HEMLOCK(301)
              else if (P2.lt.STKASPR) then
                 IFT = 305 ! SITKA SPRUCE(305)
              else if (WRCEDAR.gt.0.) then
                 IFT = 304 ! WESTERN REDCEDAR(304)
              end if
           end if
        else if (P1.lt.REDWDS) then ! A.B. REDWOODS
           P2 = MAX(REDWOOD,GNTSEQ,DGFIR)-.001
           if (P2.gt.0.) then
              if (P2.lt.REDWOOD) then
                 IFT = 341 ! REDWOOD(341)
              else if (P2.lt.GNTSEQ) then
                 IFT = 342 ! GIANT SEQUOIA(342)
              else if (DGFIR.gt.0.) then
                 IFT = 201 ! DOUGLAS-FIR(201)
              end if
           end if
        else if (P1.lt.DFLRWP) then ! A.B. DF-LARCH-W.WHITE PINES
           P2 = MAX(DGFRLR,DGFRPIN,LRCHPIN)-.001
           if (P2.gt.0.) then
              if (P2.lt.DGFRLR) then
                 P3 = MAX(DGFIR,WLRCH,WRCEDAR)-.001
                 if (P3.gt.0.) then
                    if (P3.lt.DGFIR) then
                       IFT = 201 ! DOUGLAS-FIR(201)
                    else if (P3.lt.WLRCH) then
                       IFT = 321 ! WESTERN LARCH(321)
                    else if (WRCEDAR.gt.0.) then
                       IFT = 304 ! WESTERN REDCEDAR(304)
                    end if
                 end if
              else if (P2.lt.DGFRPIN) then
                 P3 = MAX(DGFIR,PNDROSA,LODGPOLE,PORFCDR,&
                      &SUGRPIN,INSCDR,JFBCDGFR)-.001
                 if (P3.gt.0.) then
                    if (P3.lt.DGFIR) then
                       IFT = 201 ! DOUGLAS-FIR(201)
                    else if (P3.lt.PNDROSA) then
                       IFT = 221 ! PONDEROSA PINE(221)
                    else if (P3.lt.LODGPOLE) then
                       IFT = 281 ! LODGEPOLE PINE (281)
                    else if (P3.lt.PORFCDR) then
                       IFT = 202 ! PORT ORFORD CEDAR(202)
                    else if (P3.lt.SUGRPIN) then
                       IFT = 224 ! SUGAR PINE(224)
                    else if (P3.lt.INSCDR) then
                       IFT = 222 ! INCENSE CEDAR(222)
                    else if (JFBCDGFR.gt.0.) then
                       IFT = 223 ! JEFREY-COULTER PINE-BIGCONE DF(223)
                    end if
                 end if
              else if (P2.lt.LRCHPIN) then
                 P3 = MAX(WLRCH,PNDROSA,LODGPOLE)-.001
                 if (P3.gt.0.) then
                    if (P3.lt.WLRCH) then
                       IFT = 321 ! WESTERN LARCH(321)
                    else if (P3.lt.PNDROSA) then
                       IFT = 221 ! PONDEROSA PINE(221)
                    else if (LODGPOLE.gt.0.) then
                       IFT = 281 ! LODGEPOLE PINE(281)
                    end if
                 end if
              end if
           end if
        else if (P1.lt.OTHWPIN) then ! A.B. OTHER WESTERN PINES
           P2 = MAX(KNBPIN,SWHTPIN,BSHPPIN,MNTRYPIN,&
                &FXTLPIN,LMBRPIN,WHBRKPIN,MISCWSFW)-.001
           if (P2.gt.0.) then
              if (P2.lt.KNBPIN) then
                 IFT = 361 ! KNOBCONE PINE(361)
              else if (P2.lt.SWHTPIN) then
                 IFT = 362 ! SW WHITE PINE(362)
              else if (P2.lt.BSHPPIN) then
                 IFT = 363 ! BISHOP PINE(363)
              else if (P2.lt.MNTRYPIN) then
                 IFT = 364 ! MONTEREY PINE(364)
              else if (P2.lt.FXTLPIN) then
                 IFT = 365 ! FOXTAIL-BRISTLECONE PINE(365)
              else if (P2.lt.LMBRPIN) then
                 IFT = 366 ! LIMBER PINE(366)
              else if (P2.lt.WHBRKPIN) then
                 IFT = 367 ! WHITEBARK PINE(367)
              else if (MISCWSFW.gt.0.) then
                 IFT = 368 ! MISC. WESTERN SOFTWOODS(368)
              end if
           end if
        else if (P1.lt.EPINE) then ! A.B. EASTERN PINES
           P2 = MAX(RWJPIN,LNGLFSLH,LOBSHRTP)-.001
           if (P2.gt.0.) then
              if (P2.lt.RWJPIN) then
                 if ((WHPHEM.ge.HALF).and.&
                      &(EWHPIN.ge.FIVE.and.EWHPIN.lt.HALF).and.&
                      &(EHMLCK.ge.FIVE.and.EHMLCK.lt.HALF)) then
                    IFT = 104 ! E.WHITE PINE-HEMLOCK(104)
                 else
                    P3 = MAX(EWHPIN,REDPIN,JACKPIN,EHMLCK)-0.001
                 end if
                 if (P3.gt.0.) then
                    if (P3.lt.EWHPIN) then
                       IFT = 103 ! E.WHITE PINE(103)
                    else if (P3.lt.REDPIN) then
                       IFT = 102 ! RED PINE(102)
                    else if (P3.lt.JACKPIN) then
                       IFT = 101 ! JACK PINE(101)
                    else if (EHMLCK.gt.0.) then
                       IFT = 105 ! EASTERN HEMLOCK(105)
                    end if
                 end if
              else if (P2.lt.LNGLFSLH) then
                 if (LNGLFP.ge.SLASHP) then
                    IFT = 141 ! LONGLEAF PINE(141)
                 else if (SLASHP.gt.0.) then
                    IFT = 142 ! SLASH PINE(142)
                 end if
              else if (LOBSHRTP.gt.0.) then
                 P3 = MAX(LOBP,SHRTLFP,VIRGP,SANDP,&
                      &TBLMTNP,PONDP,PITCHP,SPRUCEP)-.001
                 if (P3.gt.0.) then
                    if (P3.lt.LOBP) then
                       IFT = 161 ! LOBLOLLY PINE(161)
                    else if (P3.lt.SHRTLFP) then
                       IFT = 162 ! SHORTLEAF PINE(162)
                    else if (P3.lt.VIRGP) then
                       IFT = 163 ! VIRGINIA PINE(163)
                    else if (P3.lt.SANDP) then
                       IFT = 164 ! SAND PINE(164)
                    else if (P3.lt.TBLMTNP) then
                       IFT = 165 ! TABLE MOUNTAIN PINE(165)
                    else if (P3.lt.PONDP) then
                       IFT = 166 ! POND PINE(166)
                    else if (P3.lt.PITCHP) then
                       IFT = 167 ! PITCH PINE(167)
                    else if (SPRUCEP.gt.0.) then
                       IFT = 168 ! SPRUCE PINE(168)
                    end if
                 end if
              end if
           end if
        else if (P1.lt.ESPRFIR) then ! A.B. EASTERN SPRUCE-FIR
           if (USPRFIR.ge.LSPRFIR) then ! A.B.C. UPLAND E. SPRUCE-FIR
              if ((BFIRSPR.ge.HALF).and.&
                   &(BLSMFIR.ge.FIVE.and.BLSMFIR.lt.HALF).and.&
                   &(REDSPR.ge.FIVE.and.REDSPR.lt.HALF)) then
                 IFT = 124 ! RED SPRUCE-BALSAM FIR(124)
              else
                 P2 = MAX(BLSMFIR,WHSPR,REDSPR)-.001
                 if (P2.gt.0.) then
                    if (P2.lt.BLSMFIR) then
                       IFT = 121 ! BALSAM FIR(121)
                    else if (P2.lt.WHSPR) then
                       IFT = 122 ! WHITE SPRUCE(122)
                    else if (REDSPR.gt.0.) then
                       IFT = 123 ! RED SPRUCE(123)
                    end if
                 end if
              end if
           else ! A.B.C. LOWLAND E. SPRUCE-FIR
              P2 = MAX(BLKSPR,TAMRCK,NWHCDR)-.001
              if (P2.gt.0.) then
                 if (P2.lt.BLKSPR) then
                    IFT = 125 ! BLACK SPRUCE(125)
                 else if (P2.lt.TAMRCK) then
                    IFT = 126 ! TAMARACK(126)
                 else if (NWHCDR.gt.0.) then
                    IFT = 127 ! NORTHERN WHITE-CEDAR(127)
                 end if
              end if
           end if
        else if (P1.lt.PNYNJN) then ! A.B. PINYON-JUNIPER
           P2 = MAX(ERCEDAR,RMJUNIPR,WJUNIPR,JUNIPRW,PINJUNW)-.001
           if (P2.gt.0.) then
              if (P2.lt.ERCEDAR) then
                 IFT = 181 ! EASTERN REDCEDAR(181)
              else if (P2.lt.RMJUNIPR) then
                 IFT = 182 ! ROCKY MTN. JUNIPER(182)
              else if (P2.lt.WJUNIPR) then
                 IFT = 183 ! WESTERN JUNIPER(183)
              else if (P2.lt.JUNIPRW) then
                 IFT = 184 ! JUNIPER WOODLAND(184)
              else if (PINJUNW.gt.0.) then
                 IFT = 185 ! PINYON-JUNIPER WOODLAND(185)
              end if
           end if
        else if (P1.lt.EXSFTWD) then ! A.B. EXOTIC SOFTWOODS
           P2 = MAX(SCOTCHP,JAPBLKP,OTHEXSWD)-.001
           if (P2.gt.0.) then
              if (P2.lt.SCOTCHP) then
                 IFT = 381 ! SCOTCH PINE(381)
              else if (P2.lt.JAPBLKP) then
                 IFT = 383 ! JAPANESE BLACK PINE(382)/(383)
              else if (OTHEXSWD.gt.0.) then
                 IFT = 383 ! OTHER EXOTIC SOFTWOODS(383)
              end if
           end if
        else if (P1.lt.OTHSFT) then ! A.B. FVS - OTHER SOFTWOODS
           P2 = MAX(OTHSFT,0.)-.001
           if (P2.gt.0.) then
              if (OTHSFT.gt.0.) then
                 IFT = 996 ! FVS - OTHER SOFTWOODS(996)
              end if
           end if
        end if
     end if
  else ! HARDWOODS
     IFT = 997 ! FVS OTHER HARDWOODS(997)
     if (DEBUG) write (JOSTND,*) ' IN FORTYP_5: ICYC,OAKPIN = ',&
          &ICYC,OAKPIN
     if (OAKPIN.ge.QUARTR) then ! A.B. OAK-PINE
        P1 = MAX(ERCEDAR,SHRTLFP,EWHPIN,LNGLFP,VIRGP,LOBP,SLASHP,&
             &JACKPIN,REDPIN,SANDP,TBLMTNP,PITCHP,PONDP,SPRUCEP)-.001
        if (P1.gt.0.) then
           if (P1.lt.ERCEDAR) then
              IFT = 402 ! E. REDCEDAR-HARDWOOD(402)
           else if (P1.lt.SHRTLFP) then
              IFT = 404 ! SHORTLEAF PINE-OAK(404)
           else if (P1.lt.EWHPIN) then
              IFT = 401 ! E. WHITE PINE-RED OAK-WHITE ASH(401)
           else if (P1.lt.LNGLFP) then
              IFT = 403 ! LONGLEAF PINE-OAK(403)
           else if (P1.lt.VIRGP) then
              IFT = 405 ! VIRGINIA PINE-S. RED OAK(405)
           else if (P1.lt.LOBP) then
              IFT = 406 ! LOBLOLLY PINE-HARDWOOD((406)
           else if (P1.lt.SLASHP) then
              IFT = 407 ! SLASH PINE-HARDWOOD(407)
           else if (JACKPIN+REDPIN+SANDP+TBLMTNP&
                &+PITCHP+PONDP+SPRUCEP.gt.0) then
              IFT = 409 ! OTHER PINE-HARDWOOD(409)
           end if
        end if
     else
        if (PSTBLJKO.gt.0.01) PSTBLJKO = PSTBLJKO+SROAK
        if (CHSTOAK.gt.0.01) CHSTOAK = CHSTOAK+SROAK
        if (BAELMMAP.gt.0.01) BAELMMAP = BAELMMAP+AELM+REDMAPL&
             &+SLVRMPL+WHASH+ECTNWD
        UPLAND = 0.0 ! SET FLAG FOR UPLAND SITE
        P1 = MAX(LWELASCW,LWOKGMCY,UPMPBCBR,UPOAKHIC)-.001
        if (P1.gt.0) then
           if (P1.lt.LWELASCW) then
              UPLAND = 0.0
           else if (P1.lt.LWOKGMCY) then
              UPLAND = 0.0
           else if (P1.lt.UPMPBCBR) then
              UPLAND = 1.0
           else if (UPOAKHIC.gt.0.) then
              UPLAND = 1.0
           end if
        end if
        if (UPLAND.eq.1.0) then
           OAKHCK = OAKHCK+SWEETGUM+UPLOOAK+AELM+WCELM+HOLLY+BLKGUM
           select case (VARACD)
           case ('SN')
              SOUTH = 1.0
           case default
              SOUTH = 0.0
           end select
           if (SOUTH.eq.1.0) then
              OAKHCK = OAKHCK+UPLOHWD+BEECH+BLCH
           else
              if (REDMAPL.ge.HALF) then
                 OAKHCK = OAKHCK+UPLOHWD+BEECH+BLCH
              else if ((UPLOHWD+BEECH+BLCH).lt.HALF) then
                 if (OAKHCK.gt.FIVE) then
                    OAKHCK = OAKHCK+UPLOHWD+BEECH+BLCH
                 end if
              end if
              MBB = MBB+UPLOHWD+BEECH+BLCH+AELM
              if (MBB.gt.FIVE) then
                 if ((NROAK+YP+BWALNUT).lt.HALF) then
                    MBB = MBB+NROAK+YP+BWALNUT
                 end if
              end if
           end if
        else
           UPLAND = 0.0 ! SET FLAG FOR LOWLAND SITE
           OKGMCYP = OKGMCYP+UPLOOAK
           if (OKGMCYP.gt.0.01) then
              OKGMCYP = OKGMCYP+UPLOHWD+SWEETGUM+AELM&
                   &+BEECH+HOLLY+BLKGUM+PINOAK
           end if
           ELMASHCW = ELMASHCW+AELM+WCELM
           if (ELMASHCW.gt.0.01) then
              ELMASHCW = ELMASHCW+UPLOHWD+SWEETGUM+BWALNUT
           else if (OKGMCYP.lt.0.01) then
              if (OAKHCK.gt.MBB) then
                 OAKHCK = OAKHCK+UPLOHWD+BLCH+BEECH
              else
                 MBB = MBB+UPLOHWD+BLCH+BEECH
              end if
           end if
        end if
        if (DEBUG) write (JOSTND,*) ' IN FORTYP_6: ICYC,UPLAND,',&
             &'SOUTH = ',ICYC,UPLAND,SOUTH
        if (DEBUG) write (JOSTND,*) ' IN FORTYP_7: ICYC,',&
             &'MBB,ELMASHCW,OAKHCK,OKGMCYP,ASPBRCH,ALDRMAPL,',&
             &'WSTOAK,TANOKLRL,OTHWHWD,TROPHWDS,EXHWDS,',&
             &'OTHHRD,OTHSPC = '
        if (DEBUG)write(JOSTND,*)ICYC,MBB,ELMASHCW,OAKHCK,OKGMCYP,&
             &ASPBRCH,ALDRMAPL,WSTOAK,TANOKLRL,OTHWHWD,TROPHWDS,EXHWDS,&
             &OTHHRD,OTHSPC
        P1 = MAX(MBB,ELMASHCW,OAKHCK,OKGMCYP,ASPBRCH,ALDRMAPL,&
             &WSTOAK,TANOKLRL,OTHWHWD,TROPHWDS,EXHWDS,&
             &OTHHRD,OTHSPC)-.001
        if (P1.gt.0.) then
           if (P1.lt. MBB) then ! A.B. MAPLE-BEECH-BIRCH
              if (BLCH.ge. HALF) then
                 IFT = 802 ! BLACK CHERRY(802)
              else if (REDMAPL.ge.HALF) then
                 IFT = 809 ! RED MAPLE-UPLAND(809)
              else
                 P2 = MAX(BCWAYP,MPLBASWD,ELMASHLO,MPLBCHYB)-.001
                 if (P2.gt.0.) then
                    if (P2.lt.BCWAYP) then
                       IFT = 803 ! B.CHERRY-W.ASH-Y.POPLAR(803)
                    else if (P2.lt.MPLBASWD) then
                       IFT = 805 ! HARD MAPLE-BASSWOOD(805)
                    else if (P2.lt.ELMASHLO) then
                       IFT = 807 ! ELM-ASH-LOCUST(807)
                    else if (MPLBCHYB.gt.0.) then
                       IFT = 801 ! MAPLE-BEECH-YELLOW BIRCH(801)
                    end if
                 end if
              end if
           else if (P1.lt.ELMASHCW) then ! A.B. ELM-ASH-COTTONWOOD
              if (CTNWD.ge.HALF) then
                 IFT = 703 ! COTTONWOOD(703)
              else if (WILLOW.ge.HALF) then
                 IFT = 704 ! WILLOW(704)
              else if (REDMAPL.ge.HALF) then
                 IFT = 708 ! RED MAPLE-LOWLAND(708)
              else
                 P2 = MAX(RBRCHSYC,SYCPCELM,BAELMMAP,SGBELMGA,&
                      &SLVMAELM,CTNWWILO,ORGNASH)-.001
                 if (P2.gt.0.) then
                    if (P2.lt.RBRCHSYC) then
                       IFT = 702 ! RIVER BIRCH-SYCAMORE(702)
                    else if (P2.lt.SYCPCELM) then
                       IFT = 705 ! SYCAMORE-PECAN-ELM(705)
                    else if (P2.lt.BAELMMAP) then
                       IFT = 701 ! B.ASH-ELM-MAPLE(701)
                    else if (P2.lt.SGBELMGA) then
                       IFT = 706 ! SUGARBERRY-HACKBERRY-ELM-GREEN ASH(706)
                    else if (P2.lt.SLVMAELM) then
                       IFT = 707 ! SILVER MAPLE-ELM(707)
                    else if (P2.lt.CTNWWILO) then
                       IFT = 709 ! COTTONWOOD-WILLOW(709)
                    else if (ORGNASH.gt.0.) then
                       IFT = 722 ! OREGON ASH(722)
                    end if
                 end if
              end if
           else if (P1.lt.OAKHCK) then ! A.B. OAK-HICKORY
              if (WHOAK.ge.HALF) then
                 IFT = 504 ! WHITE OAK(504)
              else if (BUROAK.ge.HALF) then
                 IFT = 509 ! BUR OAK(509)
              else if (CHSTOAK.ge.HALF) then
                 IFT = 502 ! CHESTNUT OAK(502)
              else if (NROAK.ge.HALF) then
                 IFT = 505 ! N.RED OAK(505)
              else if (SCRLTOAK.ge.HALF) then
                 IFT = 510 ! SCARLET OAK(510)
              else if (YP.ge.HALF) then
                 IFT = 511 ! YELLOW POPLAR(511)
              else if (BWALNUT.ge.HALF) then
                 IFT = 512 ! BLACK WALNUT(512)
              else if (BLCST.ge.HALF) then
                 IFT = 513 ! BLACK LOCUST(513)
              else if (REDMAPL.ge.HALF) then
                 IFT = 519 ! RED MAPLE-OAK(519)
              else
                 QOAK2 = .25*OAKHCK
                 P2 = MAX(PSTBLJKO,CHBLSC,WOROHK,YPWORO,&
                      &SCRUBOAK,SWGYP,SASFPRSM,MXDHWD)
                 if (P2.ge.QOAK2) then
                    P2 = P2-.001
                    if (P2.lt.PSTBLJKO) then
                       IFT = 501 ! POST-BLACKJACK OAK(501)
                    else if (P2.lt.CHBLSC) then
                       IFT = 515 ! CHSTNUT-BLCK-SCRLET OAK(515)
                    else if (P2.lt.WOROHK) then
                       IFT = 503 ! W.OAK-R.OAK-HICKORY(503)
                    else if (P2.lt.YPWORO) then
                       IFT = 506 ! Y.POPLAR-W.OAK-RED OAK(506)
                    else if (P2.lt.SCRUBOAK) then
                       IFT = 514 ! SOUTHERN SCRUB-OAK(514)
                    else if (P2.lt.SWGYP) then
                       IFT = 508 ! SWEETGUM-Y.POP(508)
                    else if (P2.lt.SASFPRSM) then
                       IFT = 507 ! SASAFRAS-PERSIMMON(507)
                    else if (MXDHWD.gt.0.) then
                       IFT = 520 ! MIXED UPLAND HARDWOODS(520)
                    end if
                 else
                    if (MXDHWD.gt.0.) then
                       IFT = 520 ! MIXED UPLAND HARDWOODS(520)
                    end if
                 end if
              end if
           else if (P1.lt.OKGMCYP) then ! A.B. OAK-GUM-CYPRESS
              if (ATLWCDR.ge.HALF) then
                 IFT = 606 ! ATLANTIC WHITE CEDAR(606)
              else
                 P2 = MAX(SBSTRM,SCHCHBO,SWGWILO,&
                      &CYPWTUP,OVRCUPWH)-.001
                 if (P2.gt.0.) then
                    if (P2.lt.SBSTRM) then
                       IFT = 608 ! SWEETBAY,SWAMP TPLO,RED MPL(608)
                    else if (P2.lt.SCHCHBO) then
                       IFT = 601 ! SWAMP CHESTNUT-CHERRY.BRK OAK(601)
                    else if (P2.lt.SWGWILO) then
                       IFT = 602 ! SWEETGUM, NUTTALL-WILLOW-OAK(602)
                    else if (P2.lt.CYPWTUP) then
                       IFT = 607 ! CYPRESS-WATER TUPELO(607)
                    else if (OVRCUPWH.gt.0.) then
                       IFT = 605 ! OVRCUP,WATER OAK HICKORY(605)
                    end if
                 end if
              end if
           else if (P1.lt.ASPBRCH) then ! A.B. ASPEN-BIRCH
              P2 = MAX(ASPEN,BLSMPOP,PAPRBRCH)-.001
              if (P2.gt.0.) then
                 if (P2.lt.ASPEN) then
                    IFT = 901 ! ASPEN(901)
                 else if (P2.lt.BLSMPOP) then
                    IFT = 904 ! BALSAM POPLAR(904)
                 else if (PAPRBRCH.gt.0.) then
                    IFT = 902 ! PAPER BIRCH(902)
                 end if
              end if
           else if (P1.lt.ALDRMAPL) then ! A.B. RED ALDER-MAPLE
              P2 = MAX(REDALDR,BGLFMPL)-.001
              if (P2.lt.REDALDR) then
                 IFT = 911 ! RED ALDER(911)
              else if (BGLFMPL.gt.0.) then
                 IFT = 912 ! BIGLEAF MAPLE(912)
              end if
           else if (P1.lt.WSTOAK) then ! A.B. WESTERN OAKS
              P2 = MAX(CLFBO,ORGWHO,BLUEOAK,GRAYPINE,&
                   &COSTLO,CANYLO,DOAKWDLD,EOAKWDLD)-.001
              if (P2.gt.0.) then
                 if (P2.lt.CLFBO) then
                    IFT = 922 ! CALIFORNIA BLACK OAK(922)
                 else if (P2.lt.ORGWHO) then
                    IFT = 923 ! OREGON WHITE OAK(923)
                 else if (P2.lt.BLUEOAK) then
                    IFT = 924 ! BLUE OAK(924)
                 else if (P2.lt.GRAYPINE) then
                    IFT = 921 ! GRAY PINE(921)
                 else if (P2.lt.COSTLO) then
                    IFT = 931 ! COAST LIVE OAK(931)
                 else if (P2.lt.CANYLO) then
                    IFT = 932 ! CANYON-INTERIOR LIVE OAK(932)
                 else if (P2.lt.DOAKWDLD) then
                    IFT = 925 ! DECIDUOUS OAK WOODLAND(925)
                 else if (EOAKWDLD.gt.0.) then
                    IFT = 926 ! EVERGREEN OAK WOODLAND(926)
                 end if
              end if
           else if (P1.lt.TANOKLRL) then ! A.B. TAN OAK-LAUREL
              P2 = MAX(TANOAK,CLFLREL,GNTCHNK)-.001
              if (P2.gt.0.) then
                 if (P2.lt.TANOAK) then
                    IFT = 941 ! TAN OAK(941)
                 else if (P2.lt.CLFLREL) then
                    IFT = 942 ! CALIFORNIA LAUREL(942)
                 else if (GNTCHNK.gt.0.) then
                    IFT = 943 ! GIANT CHINKAPIN(943)
                 end if
              end if
           else if (P1.lt.OTHWHWD) then ! A.B. OTHER WESTERN HARDWOODS
              P2 = MAX(PACMDRN,MESQUITW,MTNBRSHW,&
                   &INTMAPLW,MISCWHWD)-.001
              if (P2.gt.0.) then
                 if (P2.lt.PACMDRN) then
                    IFT = 951 ! PACIFIC MADRONE(951)
                 else if (P2.lt.MESQUITW) then
                    IFT = 952 ! MESQUITE-WOODLAND(952)
                 else if (P2.lt.MTNBRSHW) then
                    IFT = 953 ! MTN. BRUSH WOODLAND(953)
                 else if (P2.lt.INTMAPLW) then
                    IFT = 954 ! INT. MTN. MAPLE WOODLAND(954)
                 else if (MISCWHWD.gt.0.) then
                    IFT = 955 ! MISC. W. HARDWOODS(955)
                 end if
              end if
           else if (P1.lt.TROPHWDS) then ! A.B. TROPICAL HARDWOODS
              P2 = MAX(PALM,MANGRV)-.001
              if (P2.gt.0.) then
                 if (P2.lt.PALM) then
                    IFT = 981 ! SABAL PALM(981)
                 else if (MANGRV.gt.0.) then
                    IFT = 982 ! MANGROVE(982)
                 end if
              end if
           else if (P1.lt.EXHWDS) then ! A.B. EXOTIC HARDWOODS
              P2 = MAX(PALONIA,MELUCA,EUCLPT,OTHEXHWD)-.001
              if (P2.gt.0.) then
                 if (P2.lt.PALONIA) then
                    IFT = 991 ! ROYAL PAULOWNIA(991)
                 else if (P2.lt.MELUCA) then
                    IFT = 992 ! MELALUCA(992)
                 else if (P2.lt.EUCLPT) then
                    IFT = 993 ! EUCALYPTUS(993)
                 else if (OTHEXHWD.gt.0.) then
                    IFT = 995 ! OTHER EXOTIC HARDWOODS(995)
                 end if
              end if
           else if (P1.lt.OTHHRD) then ! A.B. FVS - OTHER HARDWOODS
              P2 = MAX(OTHHRD,0.)-.001
              if (P2.gt.0.) then
                 if (OTHHRD.gt.0.) then
                    IFT = 997 ! FVS - OTHER HARDWOODS(997)
                 end if
              end if
           else if (P1.lt.OTHSPC) then ! A.B. FVS - ALL OTHER SPECIES
              P2 = MAX(OTHSPC,0.)-.001
              if (P2.gt.0.) then
                 if (OTHSPC.gt.0.) then
                    IFT = 998 ! FVS - ALL OTHER SPECIES(998)
                 end if
              end if
           end if
        end if
     end if
  end if

  ! --- SPECIAL MIXED CONIFER TEST FOR CERTAIN COUNTIES IN CALIFORNIA;
  if (DEBUG) write(JOSTND,*)' IN FORTYP_8: KODFOR,IRGN,ISTATE,',&
       &'ICNTY,TLAT,TLONG = ',KODFOR,IRGN,ISTATE,ICNTY,TLAT,TLONG
  if (ISTATE.eq.06.or.IRGN.eq.5) then ! STATE IS CALIFORNIAT
     if (ICNTY.ne.000.and.&
          &ICNTY.ne.015.and.ICNTY.ne.023.and.ICNTY.ne.041.and.&
          &ICNTY.ne.045.and.ICNTY.ne.055.and.ICNTY.ne.081.and.&
          &ICNTY.ne.085.and.ICNTY.ne.087.and.ICNTY.ne.097) then ! N. CA. COUNTIES
        if (IFT.eq.201) IFT = 371 ! DOUGLAS-FIR NOT ALONG N. CA COAST
     else if (TLAT.gt.0.and.TLONG.gt.0) then
        if (.not.((TLAT.ge.37.and.TLAT.le.42).and.&
             &(TLONG.ge.123.and.TLONG.le.125))) then ! APPROXIMATION FOR N. CA. COUNTIES
           if (IFT.eq.201) IFT = 371 ! DOUGLAS-FIR NOT ALONG N. CA COAST
        end if
     end if
     if (IFT.eq.224.or.IFT.eq.222) IFT = 371 ! SUGAR PINE/INCENSE CEDAR
     if (IFT.eq.221.or.IFT.eq.223) then ! PONDEROSA/JEFFREY PINE
        if (PNDROSA.lt.EIGHTY) IFT = 371
     end if
     if (IFT.eq.261.or.IFT.eq.262) then ! WHITE FIR/RED FIR
        if (TRUFIR.lt.EIGHTY) IFT = 371
     end if
  end if

  999 continue
  if (DEBUG) write (JOSTND,*) ' IN FORTYP_9: IFT,JFT,LSTART = ',&
       &IFT,JFT,LSTART
  if (IFT.eq.0) IFT = 998 ! OTHER OR UNKNOWN
  if (LSTART) then
     if (JFT.gt.0.and.JFT.ne.IFT) then
        write (JOSTND,'(/10A)')&
             &' ********   WARNING, INITIAL USER INPUT FOREST TYPE ',&
             &'DOES NOT MATCH FOREST TYPE CALCULATED BY MODEL'
        IFT = JFT
     end if
  end if
  if (LFLAGV) IFT = JFT ! USER SET VALID FOREST TYPE FOR ALL CYCLES
  FIATYP = FLOAT(IFT)

  if (DEBUG) write (JOSTND,*) ' IN FORTYP_10: ICYC,IFT = ',&
       &ICYC,IFT

  !  SET FOREST TYPE VARIABLE (IFORTP) CARRIED IN PLOT.F77
  !  INVENTORY FOREST TYPE IS SAVED FOR POTENTIAL USE IN CYCLES WHERE
  !  FOREST TYPE CANNOT BE DETERMINED BY CALCULATION.

  IFORTP = INT(FIATYP)
  if (ICYC .eq.0) IIFORTP = IFORTP

  9999 continue

  if (DEBUG) write (JOSTND,*)' IN FORTYP_10: FIATYP,SDIFIA,',&
       &'ISZCL,ISTCL,IIFORTP = ',FIATYP,SDIFIA,ISZCL,ISTCL,IIFORTP

  return
end
