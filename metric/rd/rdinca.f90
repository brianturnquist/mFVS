!========================!
!   metric/rd : rdinca   !
!========================!
! METRIC-RD $Id$
subroutine RDIN(PASKEY,ARRAY,LNOTBK,LKECHO)

  !  Purpose :
  !     This is the master routine for reading in root disease keywords.

  !     Format statements that handle I/O based on number of species (MAXSP)
  !     must be changed from 11 to 49 for Inland CA S. CAscades variant.
  !     Those format statements are:  606, 711, 1321, 2921, 3963

  !  Called By :
  !     INITRE  [PROGNOSIS]

  !  Calls :
  !     DBCHK   (SUBROUTINE)   [PROGNOSIS]
  !     KEYRDR  (SUBROUTINE)   [PROGNOSIS]
  !     ERRGRO  (SUBROUTINE)   [PROGNOSIS]
  !     FNDKEY  (SUBROUTINE)   [PROGNOSIS]
  !     OPNEW   (SUBROUTINE)   [PROGNOSIS]
  !     RCDSET  (SUBROUTINE)   [PROGNOSIS]
  !     MYOPEN  (SUBROUTINE)   [PROGNOSIS]
  !     SPDECD  (SUBROUTINE)   [PROGNOSIS]
  !     RDROOT  (SUBROUTINE)   [ROOT DISEASE]
  !     RDSSIZ  (SUBROUTINE)   [ROOT DISEASE]
  !     RDSTP   (SUBROUTINE)   [ROOT DISEASE]

  !  Entry Points :
  !     RDKEY

  !  Arguments :
  !     PASKEY - RETURNS TO THE CALLING ROUTINE THE KEYWORD FROM ARRAY
  !              TABLE FOR INDEX VALUE IN PARAMETER KEY OF ENTRY POINT
  !              RDKEY.
  !     ARRAY  - ARRAY OF 7 ELEMENTS THAT HOLDS THE FIELD VALUES READ IN
  !              FROM THE KEYWORD
  !     LNOTBK - ELEMENT IS TRUE IF FIELD IS NOT BLANK

  !  Local Variables :

  !  Common Block Variables Used :

  !  Revision History :
  !     05/01/97 - Matthew K. Thompson (FHTET)
  !                Fixed an error with setting the disease type in the
  !                PLOTINF keyword.
  !     05/28/97 - Matthew K. Thompson (FHTET)
  !                Removed keyword DATELIST and call to subroutine RDTLS.
  !     05/26/98 - Robert Nathan Havis (FHTET)
  !                Put in warning message print to output file when RRTYPE
  !                is not used to specify disease and default disease type,
  !                annosus is used. new logical (LRTYPE)
  !     12/18/98 - Matt Oberle (MJO DEC98) (FHTET)
  !                Added 6 new species names to the RRJSP data table.
  !     01/18/00 - Lance David (FHTET)
  !                Inserted a keyword spaceholder in position 27 of array
  !                TABLE and increased the array size to 42. Removal of the
  !                keyword that occupied this space and reducing the array
  !                size (possibly mod dated 05/38/97) caused the FVS option
  !                processor to report the wrong keyword in the Activity
  !                Schedule and Summary reports for all Root Disease activity
  !                keywords in positions 27-42 of array TABLE.
  !     03/20/00 - Lance David (FHTET)
  !                Fixed additional format statments to handle I/O for 49
  !                species. Keywords affected were RRHOSTS and INFKILL.
  !                Replaced the literals in option processor calls with
  !                references to array MYACT.
  !     08/03/00 - Nick Crookston (RMRS)
  !                Fixed up some format statements. (Who coded the old
  !                H edit discriptor in 907?).
  !  16-MAY-01 Lance R. David (FHTET)
  !     Added use of FVS subroutine UPCASE to eliminate case sensitivity in
  !    COMMENT's "END" keyword processing.
  !  06-JUL-01 Lance R. David (FHTET)
  !     Moved several local variables (IDOBB, LBBON and LRTYPE) to RDCOM
  !     and their initialization to RDINIT.
  !     The purpose of this change is to allow RD keywords to occur
  !     in multiple sets/blocks within the FVS keyword set. That means the
  !     that certain variables cannot be re-initialized each time this
  !     routine is entered which is triggered by the RDIN keyword.
  !  06-AUG-01 Lance R. David (FHTET)
  !     Handling of tree specie code, number and alpha, (ISPC and KARD(2)) for
  !     BBTYPEx keywords were not set when field 2 of keyword was blank. This
  !     was a problem (undefined variable) for the keyword reporting statements.
  !  13-AUG-02 Lance R. David (FHTET)
  !     Added call to FVS routine GETSED in processing of RSEED keyword
  !     for random seeding of the random number generator when field 1 is 0.
  !  10-NOV-2003 Lance R. David (FHTET)
  !     Added LFLAG to KEYRDR call statement argument list.
  !  04-JAN-2006 Lance R. David (FHTET)
  !     Changed error message and process for END keyword so that FVS
  !     simulation continues instead of terminating when initialization
  !     instructions are incomplete.
  !..........................................................................

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'

  !.... Common include files.

  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'RDCOM.f90'
  include 'RDARRY.f90'
  include 'ARRAYS.f90'
  include 'RDADD.f90'
  include 'METRIC.f90'

  !.... Local variable declarations.

  ! Variables
  logical :: DEBUG
  logical :: LNOTBK(7),LINIT,LKECHO

  character(len=16) :: RRJSP(ITOTSP)
  character(len=12) :: DISTYP(0:ITOTRR)
  character(len=10) :: KARD(7)
  character(len=10) :: T1
  character(len=10) :: T2
  character(len=10) :: T3
  character(len=8) :: TABLE(42)
  character(len=8) :: KEYWRD
  character(len=8) :: PASKEY
  character(len=4) :: C4TMP
  character(len=80) :: RECORD

  integer :: MYACT(9)

  dimension    ARRAY(7), PRMS(10), IHST(MAXSP)

  !.... Data Statements.

  !.... 1-SPREAD  2-PSTUMP 3-WINDTHR 4-BBTYPE1`5-BBTYPE2
  !.... 6-BBTYPE3 7-BORATE 8-SPORE   9-BBTYPE4

  data MYACT / 2401, 2403, 2414, 2415, 2416,&
       &2417, 2430, 2431, 2432 /

  data DISTYP /'NON-HOST    ','P-TYPE ANOS.','S-TYPE ANOS.',&
       &'ARMILLARIA  ','PHELLINUS   '/

  data RRJSP /'      WHITE PINE', '   WESTERN LARCH',&
       &'     DOUGLAS-FIR', '       GRAND FIR',&
       &' WESTERN HEMLOCK', '     W. R. CEDAR',&
       &'  LODGEPOLE PINE', '       E. SPRUCE',&
       &'   SUBALPINE FIR', '  PONDEROSA PINE',&
       &'MOUNTAIN HEMLOCK', '      SUGAR PINE',&
       &'       WHITE FIR', '   INCENSE-CEDAR',&
       &'         RED FIR', '   P. SILVER FIR',&
       &'  OTHER SOFTWOOD', '  OTHER HARDWOOD',&
       &'           ASPEN', '     BLUE SPRUCE',&
       &'        CORKBARK', ' WHITE BARK PINE',&
       &'     LIMBER PINE', '      COTTONWOOD',&
       &'    WHITE SPRUCE', '         JUNIPER',&
       &'  OTHER CONIFERS', '   GIANT SEQUOIA',&
       &'       BLACK OAK', '           OTHER',&
       &'    JEFFERY PINE', 'TANOAK/CHINKAPIN',&
       &'          PINYON', '    YELLOW CEDAR',&
       &'        RED WOOD', ' SUBALPINE LARCH',&
       &'  KNOBECONE PINE', '     PACIFIC YEW',&
       &'       NOBLE FIR', '        NON-HOST',&
       &'PRT ORFORD CEDAR', '  SHASTA RED FIR',&
       &'    COULTER PINE', '   MONTEREY PINE',&
       &'     DIGGER PINE', '   BREWER SPRUCE' /

  data TABLE&
       &/'SPREAD',   'CARRY',    'PSTUMP',   'OPEN',    'RRINIT',&
       &'TTDMULT',  'INFMULT',  'INFSIMS',  'END',     'SAREA',&
       &'COMMENT',  'RRDOUT',   'INFKILL',  'WINDTHR', 'BBTYPE1',&
       &'BBTYPE2',  'BBTYPE3',  'INOCSPAN', 'RRCOMP',  'RRECHO',&
       &'STREAD',   'TDISTN',   'RRJUMP',   'RRMINK',  'RSEED',&
       &'PLREAD',   '--RDv3--', 'RRTREIN',  'INFCOLO', 'BORATE',&
       &'SPORE',    'BBTYPE4',  'DNSCALC',  'SMCOUT',  'BBOUT',&
       &'PLOTINF',  'SDIRMULT', 'TIMEDEAD', 'RRHOSTS', 'INOCLIFE',&
       &'BBCLEAR',  'RRTYPE' /

  !     DATA TABLE, alphabetically by keyword

  !     keyword  array index

  !     --RDv3--  27    Put in as a place holder. If removed, indexing in FVS
  !                     option processor routines must also be corrected.
  !     BBCLEAR   41
  !     BBOUT     35
  !     BBTYPE1   15
  !     BBTYPE2   16
  !     BBTYPE3   17
  !     BBTYPE4   32
  !     BORATE    30
  !     CARRY      2     Not used in Annosus model
  !     COMMENT   11
  !     DNSCALC   33
  !     END        9
  !     INFCOLO   29
  !     INFKILL   13
  !     INFMULT    7
  !     INFSIMS    8
  !     INOCLIFE  40
  !     INOCSPAN  18
  !     OPEN       4
  !     PLOTINF   36
  !     PLREAD    26
  !     PSTUMP     3
  !     RRCOMP    19
  !     RRDOUT    12
  !     RRECHO    20
  !     RRHOSTS   39
  !     RRINIT     5
  !     RRJUMP    23
  !     RRMINK    24
  !     RRTREIN   28
  !     RRTYPE    42
  !     RSEED     25
  !     SAREA     10
  !     SDIRMULT  37
  !     SMCOUT    34
  !     SPREAD     1
  !     SPORE     31
  !     STREAD    21
  !     TDISTN    22
  !     TIMEDEAD  38
  !     TTDMULT    6
  !     WINDTHR   14

  IROOT = 1
  ISIZE = 42

  !.... Load the passed keyword into KEYWRD.

  KEYWRD = PASKEY

  !.... See if we need to do some debug.

  call DBCHK(DEBUG,'RDIN',4,ICYC)

  90 continue

  !.... Call the keyword reader.

  call KEYRDR (IREAD,JOSTND,DEBUG,KEYWRD,&
       &LNOTBK,ARRAY,IRECNT,KODE,KARD,LFLAG)

  !.... PROCESS ERRORS
  ! ... RETURN CODES 0=NO ERROR 1=COLUMN 1 BLANK 2=EOF

  if (KODE .eq.0) goto 80
  if (KODE .eq.2) call ERRGRO (.false.,2)
  call ERRGRO (.true.,6)
  goto 90

  80 continue
  call FNDKEY (NUMBER,KEYWRD,TABLE,ISIZE,KODE,DEBUG,JOSTND)

  !.... RETURN CODES 0=NO ERROR 1=KEYWORD NOT FOUND 2=MISSPELLING

  if (KODE .eq.0) goto 51
  if (KODE .eq.1) then
     call ERRGRO (.true.,1)
     goto 90
  end if

  51 continue

  !.... PROCESS THE KEYWORD

  goto (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,&
       &22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,&
       &41,42), NUMBER

  1 continue

  !  ==========  OPTION NUMBER 1: SPREAD ===============================

  !     ARG 1 = YR OF CHANGE TO THESE PARAMETERS
  !     ARG 2 = 0, STATIC SPREAD MODEL
  !     ARG 2 = 1, DYNAMIC SPREAD MODEL
  !     ARG 2 = 2, DYNAMIC SPREAD MODEL WITH RATE AVERAGED
  !           EVALUATED ONLY IF ARG 2 = 0
  !     ARG 3 = X, SPREAD RATE FOR P TYPE CENTERS OR CURRENT ROOT
  !           DISEASE CENTERS
  !     ARG 4 = X, SPREAD RATE FOR S TYPE CENTERS
  !           ONLY USED IF ARG 2 > 0
  !     ARG 5 = Y  NUMBER OF MONTE CARLO SIMULATIONS FOR SPREAD
  !     ARG 6 = DISEASE TYPE :     1 = P-TYPE ANNOSUS
  !                                2 = S-TYPE ANNOSUS
  !                                3 = ARMILLARIA
  !                                4 = PHELLINUS

  IDT = 1
  IPOINT = MINRR

  if (LNOTBK(1)) IDT = INT(ARRAY(1))

  !.... SET WHICH ROOT DISEASE SPECIES SHOULD BE MODIFIED

  if (LNOTBK(6)) IPOINT = INT(ARRAY(6))

  IRMIN = MINRR
  IRMAX = MAXRR

  if (IPOINT .gt.2) then
     IRMIN = IPOINT
     IRMAX = IPOINT
  else if (IPOINT .eq.0) then
     IRMIN = 1
     IRMAX = 2
  end if

  PRMS(1) = IRSPTY
  PRMS(2) = RRRSET(IRMIN)
  PRMS(3) = RRRSET(IRMAX)
  PRMS(4) = NMONT

  if (LNOTBK(5)) PRMS(4) = ARRAY(5)
  if (PRMS(4) .lt.0.0) PRMS(4) = 0.0
  if (PRMS(4) .gt.50.0) PRMS(4) = 50.0

  if (LNOTBK(2)) PRMS(1) = ARRAY(2)
  if (ARRAY(2) .eq.1.0) goto 150
  if (ARRAY(2) .eq.2.0) goto 175

  !.... STATIC SPREAD MODEL

  IRSPTY = 0

  if (LMTRIC) then
     if (LNOTBK(3)) PRMS(2) = ARRAY(3) * MTOFT
     if (LNOTBK(4)) PRMS(3) = ARRAY(4) * MTOFT
  else
     if (LNOTBK(3)) PRMS(2) = ARRAY(3)
     if (LNOTBK(4)) PRMS(3) = ARRAY(4)
  end if

  if (LKECHO)write(JOSTND,100) KEYWRD, IDT
  100 format(/1X,A8,'   STARTING IN YEAR: ',I5,'   USE THE STATIC ',&
       &'SPREAD MODEL')

  if (LMTRIC) then
     if (IRMIN .eq. IRMAX) then
        if (LKECHO)write(JOSTND,113) DISTYP(IRMIN),PRMS(2) * FTTOM
        113 format(/T13, 'SPREAD FOR ',A12,F4.2,' M/YEAR')
     else if (IRMAX .eq. IRMIN + 1) then
        if (LKECHO)write(JOSTND,114) DISTYP(IRMIN),PRMS(2) * FTTOM,&
             &DISTYP(IRMAX),PRMS(3) * FTTOM
        114 format(/T13, 'SPREAD FOR ',A12,F4.2,' M/YEAR' /&
             &T13, 'SPREAD FOR ',A12,F4.2,' M/YEAR')
     end if
  else
     if (IRMIN .eq. IRMAX) then
        if (LKECHO)write(JOSTND,103) DISTYP(IRMIN),PRMS(2)
        103 format(/T13, 'SPREAD FOR ',A12,F4.2,' FEET/YEAR')
     else if (IRMAX .eq. IRMIN + 1) then
        if (LKECHO)write(JOSTND,104) DISTYP(IRMIN),PRMS(2),&
             &DISTYP(IRMAX),PRMS(3)
        104 format(/T13, 'SPREAD FOR ',A12,F4.2,' FEET/YEAR' /&
             &T13, 'SPREAD FOR ',A12,F4.2,' FEET/YEAR')
     end if
  end if
  goto 180

  150 continue

  !.... DYNAMIC SPREAD MODEL WITH A VARIETY OF SPREAD RATES USED

  IRSPTY = 1

  if (LKECHO)write(JOSTND,101) KEYWRD,IDT,INT(PRMS(4))
  101 format (/1X,A8,'   STARTING IN YEAR: ',I5,'  USE THE DYNAMIC ',&
       &'SPREAD MODEL (VARIABLE RATES).',/T13,'DO ',&
       &I4,'  SPREAD SIMULATIONS')
  goto 180

  175 continue

  !.... DYNAMIC SPREAD MODEL WITH AVERAGE SPREAD RATE APPLIED TO ALL
  !.... CENTERS

  IRSPTY = 2

  if (LKECHO)write(JOSTND,102) KEYWRD,IDT,INT(PRMS(4))
  102 format (/1X,A8,'   STARTING IN YEAR: ',I5,'  USE THE DYNAMIC ',&
       &'SPREAD MODEL (AVERAGE RATES).',/T13,'DO ',&
       &I4,'  SPREAD SIMULATIONS')
  180 continue

  call OPNEW (KODE,IDT,MYACT(1),4,PRMS)
  if (KODE .gt.0) goto 90

  goto 90

  2 continue

  !  ==========  OPTION NUMBER 2: CARRY ================================

  !     OPTION PROCESSING LOGIC FOR CARRYOVER MODEL IS CONTAINED IN
  !     SUBROUTINE DISEAS. DEFAULT IS STATIC CARRY MODEL.

  !     ****NOT YET METRIFIED.  IF DECIDE TO USE, MUST ADD METRIC OPTIONS*

  !     IF(LKECHO)WRITE(JOSTND,*) ' '
  !     IF(LKECHO)WRITE(JOSTND,*) 'CARRY      KEYWORD NOT USED IN GENERAL ROOT ',
  !    &                 'DISEASE MODEL'
  !     GOTO 90

  !     ICRRY = 0
  !     IF (ARRAY(2) .GT. 0.0) IRGEN(1) = INT(ARRAY(2))
  !     IF (ARRAY(1) .GT. 0) GOTO 202

  !     STATIC CARRYOVER MODEL, LOOK FOR PINSET, CHECK THAT IT'S IN BOUNDS

  !     IFRRC = 0
  !     IRFLAG = 1
  !     IF ((LNOTBK(4)) .AND. ARRAY(4) .GE. 0.0 .AND. ARRAY(4) .LE. 100.0)
  !    >    NNCENT = INT(ARRAY(4))
  !     IF (ARRAY(3) .LT. 0.0 .OR. ARRAY(3) .GT. 1.0) GOTO 203
  !     IF (LNOTBK(3)) PINSET = ARRAY(3)
  !     GOTO 204

  ! 203 CONTINUE
  !     ICRRY = 1

  ! 204 CONTINUE
  !     GOTO 205

  ! 202 CONTINUE

  !     DYNAMIC CARRYOVER MODEL, CHECK FOR DEFINITION OF MINIMUM
  !     SPREAD RATE IN ARRAY(3)

  !     IFRRC = 1
  !     IRFLAG = 0
  !     IF (ARRAY(3) .GT. 0.0) RRGEN(1,7) = ARRAY(3)

  !     OUTPUT FOR CARRY KEYWORD

  ! 205 CONTINUE
  !     IF (IFRRC .EQ. 1) GOTO 206
  !     IF (ICRRY .EQ. 1) GOTO 207

  !     IF(LKECHO)WRITE(JOSTND,208) KEYWRD, PINSET,NNCENT,IRGEN(1)
  ! 208 FORMAT(/1X ,A8, '   STATIC CARRYOVER MODEL',
  !    >     2X,'; PROB. OF NEW INFECTION CENTERS BEING FORMED=',F4.2,
  !    >      ';  NEW CENTERS = ',I4,/
  !    >        T13,'MODEL CALLED ',I4,' CYCLES AFTER MANAGEMENT')
  !     GOTO 90

  ! 207 CONTINUE
  !     IF(LKECHO)WRITE(JOSTND,210) KEYWRD,ARRAY(2),PINSET
  ! 210 FORMAT (/1X,A8,'*** CARRYOVER MODEL KEYWORD, PROBABILITY',
  !    > ' OF INFECTION',/,
  !    >      T13, '***** ASSIGNED ',F4.2,' OUT OF BOUNDS, SET TO ',F4.2)
  !     GOTO 90

  ! 206 CONTINUE
  !     IF(LKECHO)WRITE(JOSTND,211) KEYWRD,RRGEN(1,7),IRGEN(1)
  ! 211 FORMAT (/1X,A8,'   DYNAMIC CARRYOVER MODEL, MINIMUM SPREAD= ',
  !    >      F5.2, ' FT/YR  MODEL CALLED ',I4,' CYCLES AFTER MANAGEMENT')

  goto 90

  3 continue

  !  ==========  OPTION NUMBER 3: PSTUMP ===============================

  !     ARG 1 - YEAR OF STUMP PUSHING
  !     ARG 2 - STUMP PUSHING EFFICIENCY.  THIS IS THE PROPORTION OF
  !             STUMPS THAT WILL BE REMOVED BY THE STUMP-PUSHING ACTIVITY.
  !             DEFAULT EFFICIENCY IS 1.0
  !     ARG 3 - MINIMUM STUMP DIAMETER TO BE REMOVED.  ALL STUMPS OF
  !             THIS DIAMETER AND GREATER WILL BE REMOVED WITH THE
  !             EFFICIENCY STATED IN FIELD 2.  IF LEFT BLANK, MINIMUM
  !             DBH IS SET TO 0.

  IDT = 1
  IRRPSH = 0

  if (LNOTBK(1)) IDT = INT(ARRAY(1))

  PRMS(1) = PPUSH
  PRMS(2) = PRREM

  if (ARRAY(2) .le.1.0.and. ARRAY(2) .gt.0.0) then
     IRRPSH = 1
     PRMS(1) = ARRAY(2)
  end if

  if (ARRAY(3) .gt.0.0.and. ARRAY(3) .le.999.0) then
     if (LMTRIC) PRMS(2) = ARRAY(3) * CMTOIN
     if (.not. LMTRIC) PRMS(2) = ARRAY(3)
  end if

  call OPNEW (KODE,IDT,MYACT(2),2,PRMS)
  if (KODE .gt.0) goto 90

  !.... OUTPUT FOR PSTUMP KEYWORD

  if (IRRPSH .eq.1) goto 301
  write(JOSTND,302) KEYWRD, ARRAY(2), PPUSH
  302 format (/1X,A8,'   ***** ',F4.2,' STUMP-PUSHING EFFICIENCY ',&
       &'OUT OF BOUNDS',/,T13,'***** SET TO ',F4.2)
  goto 90

  301 continue
  if (LMTRIC) then
     if (LKECHO)write(JOSTND,304) KEYWRD, IDT, PRMS(1), PRMS(2) * INT
     304 format(/1X,A8,'   DATE/CYCLE=',I5,';  EFFICIENCY = ',F4.2,&
          &';   MIN SIZE = ',F6.2,' CMS DBH')
  else
     if (LKECHO)write(JOSTND,303) KEYWRD, IDT, PRMS(1), PRMS(2)
     303 format(/1X,A8,'   DATE/CYCLE=',I5,';  EFFICIENCY = ',F4.2,&
          &';   MIN SIZE = ',F6.2,' INCHES DBH')
  end if

  go to 90

  4 continue

  !  ==========  OPTION NUMBER 4: OPEN    ==============================

  !     OPEN FILE.  USED WITH KEYWORD RRECHO, BBOUT, SMCOUT, INFSIMS.
  !     DEFAULT VALUES ARE: UNIT 24 (RRECHO DEFAULT)
  !     SPACES TREATED AS ZEROS, STATUS FRESH, REC LENGTH 132,
  !     FILE NAME MUST BE SUPPLIED AS A SUPPLEMENTAL RECORD.

  if (.not. LNOTBK(1)) ARRAY(1) = 24
  if (.not. LNOTBK(2)) ARRAY(2) = 0
  if (.not. LNOTBK(3)) ARRAY(3) = 3
  if (.not. LNOTBK(4)) ARRAY(4) = 132

  !.... CALL PROGNOSIS SUBROUTINE KEYOPN FOR OPEN KEYWORD.

  call KEYOPN (IREAD,RECORD,JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
  goto 90

  5 continue

  !  ==========  OPTION NUMBER 5: RRINIT ===============================

  !     THIS KEYWORD IS USED WHEN ROOT DISEASE INITIALIZATION IS
  !     PERFORMED 'MANUALLY' IE THE INITIALIZATION INFORMATION IS SUPPLIED
  !     BY THE USER.

  !     CENTERS CAN BE EITHER READ IN OR SET RANDOMLY

  !     FIELD 7 CONTAINS THE DISEASE TYPE.  1 = P TYPE ANNOSUS
  !                                         2 = S TYPE ANNOSUS
  !                                         3 = ARMILLARIA
  !                                         4 = PHELLINUS

  IDI = MINRR
  if (LNOTBK(7)) IDI = INT(ARRAY(7))

  !     Modification April 17, 1998 (RNH APR98) to correct setting of
  !     root disease type and number of centers (ie. RRMAX and RRMIN
  !     when RRINIT keyword is used without the RRTYPE keyword.

  !      MAXRR= IDI

  !     end modification (RNH APR98)

  RRMAN = .true.
  if (ARRAY(1) .lt.1.0) goto 520

  !.... THE ATTRIBUTES OF EACH CENTER ARE READ IN
  !.... AND ARRAY(2) CONTAINS THE NUMBER OF CENTERS TO READ
  !.... IN.  CHECK THAT ONLY 100, AT MOST CENTERS ARE READ IN

  if (LNOTBK(2)) NCENTS(IDI) = INT(ARRAY(2))
  if (ARRAY(2) .le.100.0) goto 502
  write(JOSTND,503)
  503 format(T13,'**** TOO MANY CENTERS TO READ IN, SET TO 100')
  NCENTS(IDI) = 100

  502 continue
  do IC = 1,NCENTS(IDI)
     read(IREAD,505) PCENTS(IDI,IC,1), PCENTS(IDI,IC,2),&
          &PCENTS(IDI,IC,3)
     505 format (3F7.1)

     if (LMTRIC) then
        PCENTS(IDI,IC,1) = PCENTS(IDI,IC,1) * MTOFT
        PCENTS(IDI,IC,2) = PCENTS(IDI,IC,2) * MTOFT
        PCENTS(IDI,IC,3) = PCENTS(IDI,IC,3) * MTOFT
     end if

     if (PCENTS(IDI,IC,1) .le. DIMEN) goto 506

     write(JOSTND,507) IC, DIMEN
     507 format (/T13,'***** ERROR IN PATCH ',I4,&
          &' X-COORD SET TO ',F7.0)
     PCENTS(IDI,IC,1) = DIMEN

     506 continue
     if (PCENTS(IDI,IC,2) .le. DIMEN) goto 504

     write(JOSTND,508) IC,DIMEN
     508 format (/T13,'***** ERROR IN PATCH ',I4,&
          &' Y-COORD SET TO ',F7.0)
     PCENTS(IDI,IC,2) = DIMEN

     504 continue
  end do

  !.... SET PAREA(IDI) TO -1 TO INDICATE THAT CENTERS WERE READ IN
  !.... MANUALLY AND SET LPAREA TO TRUE SO PAREA WONT BE RESET AT END
  !.... OF PROCESSING.

  PAREA(IDI) = -1.0
  LPAREA(IDI) = .true.

  IPCFLG(IDI) = 1
  goto 530

  520 continue

  !.... THE ATTRIBUTES OF THE CENTERS ARE ASSIGNED RANDOMLY
  !.... THE USER MUST SET THE TOTAL NUMBER OF CENTERS IN
  !.... ARRAY(2) AND THE TOTAL DISEASED AREA IN ARRAY(6)

  if (LNOTBK(2)) NCENTS(IDI) = INT(ARRAY(2))
  if (ARRAY(2) .le.100.0) goto 525

  write(JOSTND,521)
  521 format(T13,'**** TOO MANY CENTERS TO ASSIGN, SET TO 100')

  NCENTS(IDI) = 100

  525 continue
  if (LMTRIC) then
     if (LNOTBK(6)) PAREA(IDI) = ARRAY(6) * HATOAC
  else
     if (LNOTBK(6)) PAREA(IDI) = ARRAY(6)
  end if

  if (LNOTBK(6)) LPAREA(IDI) = .true.
  IPCFLG(IDI) = 0

  !.... IF THERE IS ONLY ONE CENTER AND THE REQUESTED AREA IS THE STAND
  !.... AREA THEN RUN THE STAND AS ONE CENTER

  if (NCENTS(IDI) .eq.1.and. PAREA(IDI) .eq. SAREA)&
       &LONECT(IDI) = 1

  530 continue

  !.... SET UP OTHER PARAMETERS

  if (LNOTBK(3) .and. LNOTBK(4)) then
     if (LMTRIC) then
        if (ARRAY(3) .ge.0.0) PRKILL(IDI) = ARRAY(3) / HATOAC
        if (ARRAY(4) .ge.0.0) PRUN(IDI) = ARRAY(4) / HATOAC
     else
        if (ARRAY(3) .ge.0.0) PRKILL(IDI) = ARRAY(3)
        if (ARRAY(4) .ge.0.0) PRUN(IDI) = ARRAY(4)
     end if
     LINIT = .true.
  else
     LINIT = .false.
  end if

  if (LNOTBK(5) .and. ARRAY(5) .ge.0.0.and. ARRAY(5) .le.1.0)&
       &RRINCS(IDI) = ARRAY(5)

  !.... OUTPUT FOR RRINIT KEYWORD

  if (IPCFLG(IDI) .eq.1) goto 550

  if (LKECHO)write(JOSTND,541) KEYWRD, DISTYP(IDI)
  541 format (/1X,A8,'   ROOT DISEASE IS ', A12 /&
       &T13,'ROOT DISEASE INITIALIZATION KEYWORD; ',&
       &'ROOT DISEASE CENTERS ASSIGNED RANDOMLY '/&
       &T13,'INITIALIZATION WILL BE WITH USER-SUPPLIED DATA',&
       &' UNLESS THE RRTREIN KEYWORD IS USED IN THE',&
       &' RUNSTREAM')

  if (LINIT) then
     if (LMTRIC) then
        if (LKECHO)write(JOSTND,572) NCENTS(IDI), PRKILL(IDI)/ACTOHA,&
             &PRUN(IDI)/ACTOHA
        572 format (T13,'NUMBER OF CENTERS=',I4/&
             &T13,'INFECTED TREES/HA IN DISEASED AREAS   = ',F7.2/&
             &T13,'UNINFECTED TREES/HA IN DISEASED AREAS = ',F7.2)
     else
        if (LKECHO)write(JOSTND,542) NCENTS(IDI), PRKILL(IDI), PRUN(I
        542 format (T13,'NUMBER OF CENTERS=',I4/&
             &T13,'INFECTED TREES/ACRE IN DISEASED AREAS   = ',F7.2/&
             &T13,'UNINFECTED TREES/ACRE IN DISEASED AREAS = ',F7.2)
     end if

  else
     if (LKECHO)write(JOSTND,543) NCENTS(IDI)
     543 format (T13,'NUMBER OF CENTERS=',I4)

     if (LMTRIC) then
        if (LKECHO)write(JOSTND,575)
        575 format (T13,'50% OF TREES/HA IN THE DISEASED AREAS ARE',&
             &' INFECTED'/&
             &T13,'50% OF TREES/HA IN THE DISEASED AREAS ARE',&
             &' UNINFECTED')
     else
        if (LKECHO)write(JOSTND,576)
        576 format (T13,'50% OF TREES/ACRE IN THE DISEASED AREAS ARE',&
             &' INFECTED'/&
             &T13,'50% OF TREES/ACRE IN THE DISEASED AREAS ARE',&
             &' UNINFECTED')
     end if

     if (LNOTBK(3) .or. LNOTBK(4)) write(JOSTND,544)
     544 format (T10,'** EITHER FIELD 3 OR FIELD 4 WAS NOT SPECIFIED.',&
          &' DEFAULTS WERE USED IN BOTH FIELDS')
  end if

  if (PAREA(IDI) .lt.0.0) then
     if (LKECHO)write(JOSTND,571) RRINCS(IDI)
     571 format (T13,'PROPORTION ROOTS INFECTED=',F4.2,&
          &/T13,'ROOT DISEASE AREA WILL BE CALCULATED FROM THE ',&
          &'PROPORTION OF INFECTED PLOTS IN THE STAND.')

  else
     if (LMTRIC) then
        if (LKECHO)write(JOSTND,585) RRINCS(IDI), PAREA(IDI) * ACTOHA
        585 format (T13,'PROPORTION ROOTS INFECTED=',F4.2,&
             &' ;DISEASE AREA = ',F6.2,' HA')
     else
        if (LKECHO)write(JOSTND,545) RRINCS(IDI), PAREA(IDI)
        545 format (T13,'PROPORTION ROOTS INFECTED=',F4.2,&
             &' ;DISEASE AREA = ',F6.2,' ACRES')
     end if
  end if

  if (LONECT(IDI) .eq.1.0) then
     if (LKECHO)write(JOSTND,547)
     547 format (T13,'THE STAND WILL BE MODELLED AS ONE DISEASE CENTER')
  end if

  goto 590

  550 continue
  if (LKECHO)write(JOSTND,551) KEYWRD, DISTYP(IDI)
  551 format (/1X,A8,'   ROOT DISEASE IS ', A12 /&
       &T13,'ROOT DISEASE INITIALIZATION KEYWORD; ',&
       &' ROOT DISEASE CENTERS READ IN '/&
       &T13,'INITIALIZATION WILL BE WITH USER-SUPPLIED DATA',&
       &' UNLESS THE RRTREIN KEYWORD IS USED IN THE',&
       &' RUNSTREAM')

  if (LINIT) then
     if (LMTRIC) then
        if (LKECHO)write(JOSTND,572) NCENTS(IDI), PRKILL(IDI)/ACTOHA,&
             &PRUN(IDI)/ACTOHA
     else
        if (LKECHO)write(JOSTND,542) NCENTS(IDI), PRKILL(IDI), PRUN(I
     end if
  else
     if (LKECHO)write(JOSTND,543) NCENTS(IDI)
     if (LMTRIC) if (LKECHO)write(JOSTND,575)
     if (.not. LMTRIC) if (LKECHO)write(JOSTND,576)
     if (LNOTBK(3) .or. LNOTBK(4)) if (LKECHO)write(JOSTND,544)
  end if

  if (LKECHO)write(JOSTND,552) RRINCS(IDI)
  552 format (T13,'PROPORTION ROOTS INFECTED=',F4.2)

  if (LKECHO)write(JOSTND,553)
  553 format (//,T13,'X-COORD    Y-COORD   RADIUS')
  if (LKECHO)write(JOSTND,554)
  554 format (T13,'------------------------------')

  do IC = 1,NCENTS(IDI)
     if (LMTRIC) then
        if (LKECHO)write(JOSTND,555) PCENTS(IDI,IC,1) * FTTOM,&
             &PCENTS(IDI,IC,2) * FTTOM,&
             &PCENTS(IDI,IC,3) * FTTOM
     else
        if (LKECHO)write(JOSTND,555) PCENTS(IDI,IC,1), PCENTS(IDI,IC,&
             &PCENTS(IDI,IC,3)
     end if
     555 format (T13,F7.1,4X,F7.1,2X,F7.1)
     556 continue
  end do

  590 continue
  go to 90

  6 continue

  !  ==========  OPTION NUMBER 6: TTDMULT ==============================

  !     TIME TO DEATH MULTIPLIERS
  !     CHECK THAT ROOT DISEASE TYPE IS ACCEPTABLE

  IPOINT = 0
  if (LNOTBK(2)) IPOINT = INT(ARRAY(2))
  if (ARRAY(1) .gt.0.0) IRGEN(8) = INT(ARRAY(1))
  IHB = 1
  if (IRGEN(8) .ne.3000) IHB = 2
  if (INT(ARRAY(2)) .le. ITOTRR) goto 601

  write(JOSTND,602)
  602 format (/, ' ********   ERROR IN DISEASE TYPE, TTDMULT KEYWORD')
  write(JOSTND,603)
  603 format (' ********   KEYWORD WILL AFFECT ALL SPECIFIED DISEASE ',&
       &'TYPES')
  IPOINT = 0

  601 continue

  !.... SET WHICH ROOT DISEASE SPECIES SHOULD BE MODIFIED

  IRMIN = MINRR
  IRMAX = MAXRR
  if (IPOINT .gt.0) then
     IRMIN = IPOINT
     IRMAX = IPOINT
  end if

  !.... CALL SPDECD TO GET SPECIES CODE (ALPHA OR NUMERIC).

  call SPDECD(3,ISPC,NSP(1,1),MAXSP,JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)

  !.... NOW, CHECK WHETHER ONLY ONE VALUE TO BE CHANGED, OR ALL

  if (ISPC .ge.1.and. ISPC .le. MAXSP) then

     !....    ONLY ONE TREE SPECIES VALUE TO BE CHANGED
     !....    (FOR ONE OR MORE RR TYPES)

     do IDI = IRMIN,IRMAX
        HABFAC(IRTSPC(ISPC),IDI,IHB) = ARRAY(4)
        670 continue
     end do

  else if (ISPC .eq.0) then

     !....    ALL SPECIES GET CHANGED

     read (IREAD,606) (HABFAC(IRTSPC(KSP),IRMIN,IHB),KSP = 1,MAXSP)
     do IDI = IRMIN+1,IRMAX
        do KSP = 1,MAXSP
           HABFAC(IRTSPC(KSP),IDI,IHB) = &
                &HABFAC(IRTSPC(KSP),IRMIN,IHB)
           671 continue
        end do
        672 continue
     end do
     606 format (49F5.2)

  else

     !....    ERROR IN READING IN SPECIES CODE

     write(JOSTND,610) KEYWRD
     610 format (' ********   ERROR IN KEYWORD ', A8, ', SPECIES ',&
          &'CODE NOT VALID.  KEYWORD IGNORED.')
     goto 90

  end if

  !.... OUTPUT FOR TTDMULT KEYWORD

  if (LKECHO)write(JOSTND,620) KEYWRD
  620 format (/1X,A8,'   TIME TO DEATH MULTIPLIERS ')
  if (LKECHO)write(JOSTND,621)
  621 format (//T13,'MULTIPLIERS ARE RELATIVE TO INTERIOR',&
       &' DOUGLAS-FIR')
  if (LKECHO)write(JOSTND,622)
  622 format (T13,'ON DOUGLAS-FIR HABITAT')
  if (LKECHO)write(JOSTND,623)
  623 format (/T13,5X,'SPECIES',14X,'MULTIPLIER')
  if (LKECHO)write(JOSTND,624)
  624 format (T13,41('-'))

  do IDI = IRMIN,IRMAX
     if (LKECHO)write(JOSTND,680) DISTYP(IDI)
     if (LKECHO)write(JOSTND,681)

     do KSP = 1,MAXSP
        if (LKECHO)write(JOSTND,631) RRJSP(IRTSPC(KSP)),&
             &HABFAC(IRTSPC(KSP),IDI,1)
        630 continue
     end do
     675 continue
  end do

  680 format (T13,A16)
  681 format (T13,16('-'))
  631 format (T13,A16,13X,F5.2)

  if (IRGEN(8) .eq.3000) goto 640
  if (LKECHO)write(JOSTND,641) IRGEN(8)
  641 format (/T13,'TIME TO DEATH MULTIPLIERS AFTER ',I4)
  if (LKECHO)write(JOSTND,623)
  if (LKECHO)write(JOSTND,624)

  do IDI = IRMIN,IRMAX
     if (LKECHO)write(JOSTND,680) DISTYP(IDI)
     if (LKECHO)write(JOSTND,681)

     do KSP = 1,MAXSP
        if (LKECHO)write(JOSTND,631) RRJSP(IRTSPC(KSP)),&
             &HABFAC(IRTSPC(KSP),IDI,2)
        642 continue
     end do
     676 continue
  end do

  640 continue
  goto 90

  7 continue

  !  ==========  OPTION NUMBER 7: INFMULT ==============================

  !     PROBABILITY OF INFECTION MULTIPLIERS
  !     CHECK FOR YEAR THAT MULTIPLIERS ARE TO TAKE EFFECT
  !     CHECK THAT ROOT DISEASE TYPE IS ACCEPTABLE

  IPOINT = 0
  if (ARRAY(1) .gt.0.0) IRGEN(9) = INT(ARRAY(1))
  if (LNOTBK(2)) IPOINT = INT(ARRAY(2))

  !.... CHECK TO MAKE SURE ROOT DISEASE TYPE IS ACCEPTABLE

  if (INT(ARRAY(2)) .gt. ITOTRR) then
     write(JOSTND,702)
     write(JOSTND,703)
     IPOINT = 0
  end if

  702 format (/,' ********   ERROR IN DISEASE TYPE, INFMULT KEYWORD')
  703 format (' ********   CHANGE WILL AFFECT ALL ACTIVE DISEASE ',&
       &'TYPES')

  !.... SET THE ROOT DISEASE TYPE TO BE MODIFIED

  IRMIN = MINRR
  IRMAX = MAXRR
  if (IPOINT .gt.0) then
     IRMIN = IPOINT
     IRMAX = IPOINT
  end if

  !.... CALL SPDECD TO GET SPECIES CODE (ALPHA OR NUMERIC).

  call SPDECD(3,ISPC,NSP(1,1),MAXSP,JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)

  !.... NOW, CHECK WHETHER ONLY ONE VALUE TO BE CHANGED, OR ALL

  if (ISPC .ge.1.and. ISPC .le. MAXSP) then

     !....    ONLY ONE VALUE TO BE CHANGED

     do IDI = IRMIN,IRMAX
        if (IRGEN(9) .eq.3000) then
           PNINF(IRTSPC(ISPC),IDI) = ARRAY(4)
        else
           SSSFAC(IRTSPC(ISPC),IDI) = &
                &ARRAY(4) / PNINF(IRTSPC(ISPC),IDI)
        end if
        770 continue
     end do

  else if (ISPC .eq.0) then

     !....    ALL SPECIES GET CHANGED

     if (IRGEN(9) .eq.3000) then
        read (IREAD,711) (PNINF(IRTSPC(KSP),IRMIN),KSP = 1,MAXSP)
     else
        read (IREAD,711) (SSSFAC(IRTSPC(KSP),IRMIN),KSP = 1,MAXSP)
     end if
     711 format (49F5.2)

     do IDI = IRMIN,IRMAX
        do KSP = 1,MAXSP
           if (IDI .gt. IRMIN) then
              if (IRGEN(9) .eq.3000) then
                 PNINF(IRTSPC(KSP),IDI) = PNINF(IRTSPC(KSP),IRMIN)
              else
                 SSSFAC(IRTSPC(KSP),IDI) = SSSFAC(IRTSPC(KSP),IRMIN)
              end if
           end if
           if (PNINF(IRTSPC(KSP),IDI) .gt.0.0) then
              SSSFAC(IRTSPC(KSP),IDI) = SSSFAC(IRTSPC(KSP),IDI) /&
                   &PNINF(IRTSPC(KSP),IDI)
           end if
           712 continue
        end do
        772 continue
     end do

  else

     !....    ERROR IN READING IN SPECIES CODE

     if (LKECHO)write(JOSTND,610) KEYWRD
     goto 90
  end if

  !.... OUTPUT FOR INFMULT KEYWORD

  720 continue
  if (LKECHO)write(JOSTND,721) KEYWRD
  721 format (/1X,A8,'   PROBABILITY OF INFECTION FOR ROOT DISEASE')
  if (LKECHO)write(JOSTND,722)
  722 format (/T13,5X,'SPECIES',14X,'PROBABILITY')
  if (LKECHO)write(JOSTND,723)
  723 format (T13,41('-'))

  do IDI = IRMIN,IRMAX
     if (LKECHO)write(JOSTND,780) DISTYP(IDI)
     if (LKECHO)write(JOSTND,781)

     do KSP = 1,MAXSP
        if (LKECHO)write(JOSTND,725) RRJSP(IRTSPC(KSP)), PNINF(IRTSPC
        724 continue
     end do
     775 continue
  end do

  780 format (T13,A16)
  781 format (T13,16('-'))
  725 format(T13,A16,13X,F5.2)

  if (IRGEN(9) .eq.3000) goto 740
  if (LKECHO)write(JOSTND,726) IRGEN(9)
  726 format (/T13,'PROBABILITY OF INFECTION AFTER ',I4)
  if (LKECHO)write(JOSTND,722)
  if (LKECHO)write(JOSTND,723)

  do IDI = IRMIN,IRMAX
     if (LKECHO)write(JOSTND,780) DISTYP(IDI)
     if (LKECHO)write(JOSTND,781)

     do KSP = 1,MAXSP
        TT = PNINF(IRTSPC(KSP),IDI) * SSSFAC(IRTSPC(KSP),IDI)
        if (LKECHO)write(JOSTND,725) RRJSP(IRTSPC(KSP)), TT
        730 continue
     end do
     785 continue
  end do

  740 continue
  goto 90

  8 continue

  !  ==========  OPTION NUMBER 8: INFSIMS ==============================

  !     SET NUMBER OF SIMULATIONS TO DO THE INSIDE CENTER INFECTION
  !     SIMULATION ALSO, CAN REQUEST OUTPUT FROM THE INSIDE CENTER
  !     INFECTION SIMULATION (IN REGULAR TABLE FORMAT, WITH HEADINGS)
  !     (TO PRINT OUTPUT, FIELD 2 MUST CONTAIN SOME CHARATER, AND
  !     FIELD 3 MUST CONTAIN THE FILE NUMBER. IF FIELD 2 IS NOT BLANK
  !     AND FIELD 3 IS BLANK, THE DEFAULT FILE NUMBER WILL BE USED)

  if (LNOTBK(1)) NINSIM = INT(ARRAY(1))
  if (NINSIM .gt.50) NINSIM = 50

  ISDOUT = 0
  if (LNOTBK(2)) then
     ISDOUT = 25
     if (ARRAY(3) .gt.0.0) ISDOUT = INT(ARRAY(3))
  end if

  !.... OUTPUT FOR INFSIMS KEYWORD

  if (LKECHO)write(JOSTND,801) KEYWRD, NINSIM
  801 format(/1X,A8,'   NUMBER OF TIMES TO SIMULATE THE INSIDE ',&
       &'INFECTION DYNAMICS IS: ',I4)

  if (ISDOUT .gt.0) then
     if (LKECHO)write(JOSTND,802) ISDOUT
     802 format (/T13,'   INSIDE CENTER INFECTION OUTPUT ON LOGICAL',&
          &' UNIT ',I4)
  end if

  goto 90

  9 continue

  !  ==========  OPTION NUMBER 9: END ==================================

  !     END KEYWORD. END THE ROOT DISEASE KEYWORD PROCESSING.
  !     OPEN ROOT DISEASE FILES HERE. CALL IS PUT HERE TO INSURE
  !     FILES ARE OPENED AFTER ANY DEFAULT UNITS HAVE BEEN MODIFIED BY
  !     KEYWORD.

  if (LKECHO)write(JOSTND,901) KEYWRD
  901 format (/1X,A8,'   END OF ROOT DISEASE KEYWORDS')

  !.... IF NEITHER INITIALIZATION METHOD SPECIFIED,THE SIMULATION
  !.... CONTINUES WITHOUT ROOT DISEASE IMPACTS.

  if (.not. RRTINV .and..not. RRMAN) then
     write(JOSTND,905)

     905 format (/' ***** FVSRD ERROR: NEITHER TREELIST OR MANUAL ',&
          &'INITIALIZATION SPECIFIED. ROOT DISEASE NOT ',&
          &'ACTIVATED.*****')
     call ERRGRO (.true.,4)
     goto 9000
  end if

  !.... IF TREE RECORD DATA HAS BEEN READ BEFORE ROOT DISEASE KEYWORDS
  !.... HAVE BEEN PROCESSED WRITE ERROR MESSAGE AND TERMINATE RUN. THIS IS
  !.... NECESSARY BECAUSE METHOD OF ROOT DISEASE INITIALIZATION MUST BE
  !.... SPECIFIED IN ORDER FOR INTREE TO PROPERLY PROCESS TREE RECORDS.

  if (IREC1.eq.0) goto 910

  write(JOSTND,906)
  906 format (/' ****** WARNING ****** TREE RECORDS HAVE BEEN ',&
       &'PROCESSED BEFORE METHOD OF ROOT DISEASE INITIALIZATION HAS ',&
       &'BEEN  SPECIFIED.',/,T23,'RUN IS TERMINATED.')
  call RCDSET (2,.false.)

  910 continue

  !     If the RRTYPE keword has not be used the disease type for the
  !     simulation will be the defalt - annosus disease. The following
  !     statement checks for the use of RRTYPE and writes warning message
  !     to the output file (RNH May98)

  if (.not. LRTYPE) write(JOSTND, 907)

  907 format (/' ****** WARNING: THE "RRTYPE" KEYWORD WAS NOT USED,',&
       &'>> ANNOSUS ROOT DISEASE <<, WILL BE SIMULATED.')

  !.... IF NO BARK BEETLES ARE SCHEDULED, AND THE USER HAS NOT SPECIFIED
  !.... THAT THE CHOICE OF NO BARK BEETLES WAS INTENTIONAL, THEN
  !.... TURN ON ALL BARK BEETLES USING THEIR DEFAULT VALUES.

  if (IDOBB .le.0.and. LBBON) then
     write(JOSTND,911)
     911 format (/' ******* WARNING: NO BARK BEETLES WERE SPECIFIED ',&
          &'AND THE BBCLEAR KEYWORD WAS NOT USED.',/T18,&
          &'BARK BEETLES ARE ON USING THEIR DEFAULT VALUES AS FOLLOWS')

     !....    TYPE 1 BARK BEETLES

     IDOBB = IDOBB + 1
     PRMS(1) = RROBTS(1)
     PRMS(2) = RROBSC(1)
     PRMS(3) = RROBOL(1)
     PRMS(4) = RROBMR(1)
     PRMS(5) = 0.0

     call OPNEW (KODE,1,MYACT(4),5,PRMS)
     if (KODE .gt.0) goto 913

     if (LMTRIC) then
        if (LKECHO)write(JOSTND, 952) PRMS(1), PRMS(2)*INTOCM,&
             &PRMS(3)/ACTOHA, PRMS(4)
     else
        if (LKECHO)write(JOSTND, 912) (PRMS(I), I = 1,4)
     end if

     912 format (/1X,'BBTYPE1    TYPE 1 BARK BEETLE IS ON EVERY YEAR.',&
          &' SPECIES =',F3.0,';  MINIMUM DBH=',F5.2,' INCHES',&
          &/T13,'MINIMUM ELIGIBLE STEMS FOR EVENT =',F7.0,&
          &' STEMS/ACRE;   MORTALITY RATE = ',F5.3)
     952 format (/1X,'BBTYPE1    TYPE 1 BARK BEETLE IS ON EVERY YEAR.',&
          &' SPECIES =',F3.0,';  MINIMUM DBH=',F5.2,' CMS',&
          &/T13,'MINIMUM ELIGIBLE STEMS FOR EVENT =',F7.0,&
          &' STEMS/HA;   MORTALITY RATE = ',F5.3)

     913 continue

     !....    TYPE 3 BARK BEETLES

     IDOBB = IDOBB + 1
     IDT = 1

     PRMS(1) = RROBTS(3)
     PRMS(2) = RROBSC(3)
     PRMS(3) = RROBOL(3)
     PRMS(4) = RROBMR(3)
     PRMS(5) = RROBRD(3)
     PRMS(6) = 0.0

     call OPNEW (KODE,1,MYACT(6),6,PRMS)
     if (KODE .gt.0) goto 915

     if (LMTRIC) then
        if (LKECHO)write(JOSTND, 954) PRMS(1), PRMS(2)*INTOCM,&
             &PRMS(3)/ACTOHA, PRMS(4), PRMS(5)
     else
        if (LKECHO)write(JOSTND, 914) (PRMS(I), I = 1,5)
     end if

     914 format(/1X,'BBTYPE3     TYPE 3 BARK BEETLE IS ON EVERY YEAR',&
          &' SPECIES =', F3.0, ';  MINIMUM DBH=', F5.2, '  INCHES',&
          &/T13, 'MINIMUM ELIGIBLE STEMS FOR EVENT= ', F7.0,&
          &' STEMS', '/ACRE   ;  MORTALITY RATE = ', F4.2,&
          &/T13, 'MINIMUM ROOT DISEASE INFECTION= ', F4.2)
     954 format(/1X,'BBTYPE3     TYPE 3 BARK BEETLE IS ON EVERY YEAR',&
          &' SPECIES =', F3.0, ';  MINIMUM DBH=', F5.2, '  CMS',&
          &/T13, 'MINIMUM ELIGIBLE STEMS FOR EVENT= ', F7.0,&
          &' STEMS', '/HA   ;  MORTALITY RATE = ', F4.2,&
          &/T13, 'MINIMUM ROOT DISEASE INFECTION= ', F4.2)

     915 continue

     !....    TYPE 4 BARK BEETLES

     IDOBB = IDOBB + 1
     PRMS(1) = RROBTS(4)
     PRMS(2) = RROBSC(4)
     PRMS(3) = RROBOL(4)
     PRMS(4) = RROBMR(4)
     PRMS(5) = RROBRD(4)
     PRMS(6) = 0.3
     PRMS(7) = 0.15
     PRMS(8) = 0.01
     PRMS(9) = 0.0

     call OPNEW (KODE,1,MYACT(9),9,PRMS)
     if (KODE .gt.0) goto 917

     if (LMTRIC) then
        if (LKECHO)write(JOSTND, 956) PRMS(1), PRMS(2)*INTOCM,&
             &PRMS(3)/ACTOHA, (PRMS(I),I = 4,8)
     else
        if (LKECHO)write(JOSTND,  916) (PRMS(I), I = 1,8)
     end if

     916 format(/1X,'BBTYPE4    TYPE 4 BARK BEETLE IS ON EVERY YEAR',&
          &' SPECIES =',F3.0,';  MINIMUM DBH=',F5.2,'  INCHES',&
          &/T13,'MINIMUM ELIGIBLE STEMS FOR EVENT= ',F7.0,' STEMS',&
          &'/ACRE ; INFECTED MORTALITY= ',F5.3,&
          &/T13,'MINIMUM ROOT DISEASE INFECTION= ',F5.2,&
          &' UNINFECTED-WITHIN-PATCH MORTALITY= ',F5.2,&
          &/T13,'UNINFECTED-FRINGE MORTALITY=',F5.2,&
          &' UNINFECTED-NON-PATCH MORTALITY= ',F5.2)
     956 format(/1X,'BBTYPE4    TYPE 4 BARK BEETLE IS ON EVERY YEAR',&
          &' SPECIES =',F3.0,';  MINIMUM DBH=',F5.2,'  CMS',&
          &/T13,'MINIMUM ELIGIBLE STEMS FOR EVENT= ',F7.0,' STEMS',&
          &'/HA ; INFECTED MORTALITY= ',F5.3,&
          &/T13,'MINIMUM ROOT DISEASE INFECTION= ',F5.2,&
          &' UNINFECTED-WITHIN-PATCH MORTALITY= ',F5.2,&
          &/T13,'UNINFECTED-FRINGE MORTALITY=',F5.2,&
          &' UNINFECTED-NON-PATCH MORTALITY= ',F5.2)

     917 continue
  end if

  !.... IF BBTYPE II SPECIFIED BUT NOT A WINDTHROW EVENT WRITE ERROR
  !.... MESSAGE.

  if (BB2GO .and..not. WINGO) then
     write(JOSTND,920)
     920 format(/T13,'***** WARNING, TYPE II BARK BEETLE EVENT CALLED',&
          &' FOR BUT WINDTHROW EVENT NOT CALLED'/,&
          &T13,'TYPE II BARK BEETLE OUTBREAK CANNOT HAPPEN *****')
  end if

  !.... IF THERE MAY BE TOO MANY BARK BEETLE EVENTS SCHEDULED THEN
  !.... PRINT A WARNING MESSAGE THAT SOME BARK BEETLES MAY NOT BE DOME

  if (IDOBB .gt.3*MAXSP) then
     write(JOSTND,919)
     919 format(/T13,'***** WARNING, TOO MANY BARK BEETLE EVENTS HAVE',&
          &' BEEN SCHEDULED.'/T13,'IF ALL OF THEM ARE ACTIVE IN',&
          &' THE SAME YEAR NOT ALL EVENTS WILL HAPPEN *****')
  end if

  !.... IF REINEKE SPECIFIED BUT BBTYPE 1 NOT PRESENT

  if (REINGO .and..not. BB1GO) then
     write(JOSTND,921)
     921 format(/T13,'***** WARNING, REINEKE METHOD CALLED',&
          &' WITHOUT BBTYPE 1 EVENT:'/,&
          &T13,'REINEKE WILL HAVE NO EFFECT *****')
  end if

  do IDI = MINRR,MAXRR

     !....    IF PAREA HAS NOT BEEN ENTERED BY KEYWORD THEN SET THE DEFAULT
     !....    FOR PAREA TO 25% OF SAREA.

     if (.not. LPAREA(IDI)) PAREA(IDI) = 0.25 * SAREA

     !....    If the LONECT flag has not been set and if there is only one
     !....    center and the requested root disease area is equal to the
     !....    stand area then run the stand as one center.
     !....    This code is here to catch if the SAREA keyword comes before
     !....    the RRINIT keyword.

     if (LONECT(IDI) .eq.0.and. NCENTS(IDI) .eq.1.and.&
          &PAREA(IDI) .eq. SAREA) then
        LONECT(IDI) = 1
        if (LKECHO)write(JOSTND,*)
        if (LKECHO)write(JOSTND,547)
     end if

     923 continue
  end do

  !.... OPEN DISK FILE FOR UNIT IRUNIT.

  call MYOPEN (IRUNIT, '   ', 4, 133, 0, 1, 1, 0, KODE)

  !.... PRINT MESSAGE IF ERROR OCCURED IN OPEN.

  if (KODE .eq.1) then
     write(JOSTND,*) ' ERROR WHILE OPENING UNIT IRUNIT !!!!!!!'
  end if

  !.... OPEN DISK FILE FOR UNIT IOUNIT.

  call MYOPEN (IOUNIT, '   ', 4, 133, 0, 1, 1, 0, KODE)

  !.... PRINT MESSAGE IF ERROR OCCURED IN OPEN.

  if (KODE .eq.1) then
     write(JOSTND,*) ' ERROR WHILE OPENING UNIT IOUNIT !!!!!!!'
  end if

  !.... OPEN DISK FILE FOR RRECHO (UNIT IRGEN(2)).

  !      CALL MYOPEN (IRGEN(2), 'RRECHO', 1, 133, 0, 1, 1, 0, KODE)

  goto 9000

  10 continue

  !  ==========  OPTION NUMBER 10: SAREA ================================

  !     SET THE STAND AREA IN ACRES
  !        (IF ENTERED AS HA, CHANGE TO ACRES)
  !     CHECK THAT A POSITIVE VALUE HAS BEEN ENTERED

  if (ARRAY(1) .gt.0.0) goto 1010

  if (LMTRIC) write(JOSTND,1011) SAREA * ACTOHA
  1011 format (/T13,'***** ERROR IN STAND AREA INPUT ',F5.2,&
       &' HECTARES ASSUMED')

  if (.not. LMTRIC) write(JOSTND,1001) SAREA
  1001 format (/T13,'***** ERROR IN STAND AREA INPUT ',F5.2,&
       &' ACRES ASSUMED')
  goto 90

  1010 continue
  if (.not. LMTRIC) SAREA = ARRAY(1)
  if (LMTRIC) SAREA = ARRAY(1) * HATOAC
  DIMEN = SQRT(SAREA) * 208.7

  !.... OUTPUT FOR SAREA KEYWORD

  if (LMTRIC) then
     if (LKECHO)write(JOSTND,1012) KEYWRD, SAREA * ACTOHA
     1012 format(/1X, A8, '   STAND AREA =', F8.2, ' HA')
  else
     if (LKECHO)write(JOSTND,1002) KEYWRD, SAREA
     1002 format(/1X, A8, '   STAND AREA =', F8.2, ' ACRES')
  end if
  goto 90

  11 continue

  !  ==========  OPTION NUMBER 11: COMMENT ==============================

  !     COMMENT KEYWORD.  THIS ENABLES THE USER TO WRITE OUT
  !     A DESCRIPTION OF THE PARTICULAR ROOT DISEASE SCENARIO

  if (LKECHO)write(JOSTND,1101) KEYWRD
  1101 format (/1X,A8)

  !.... READ IN COMMENT STATEMENTS UNTIL 'END' IS ENCOUNTERED

  1102 continue
  read (IREAD,1103,end = 40) RECORD
  1103 format (A80)
  IRECNT = IRECNT + 1
  C4TMP = RECORD(1:4)
  call UPCASE (C4TMP(1:1))
  call UPCASE (C4TMP(2:2))
  call UPCASE (C4TMP(3:3))
  if (C4TMP .eq. 'END ') then
     if (LKECHO)write(JOSTND,1105) RECORD(1:4)
     1105 format (/' ',A4)
     goto 90
  else
     1104 if (LKECHO)write(JOSTND,1106) RECORD
     1106 format (T13,A80)
  end if
  goto 1102

  12 continue

  !  ==========  OPTION NUMBER 12: RRDOUT ===============================

  !     DETAILED OUTPUT KEYWORD

  IRDOUT = 1

  !.... OUTPUT FOR RRDOUT KEYWORD

  if (LKECHO)write(JOSTND,1201) KEYWRD
  1201 format (/1X,A8,'   DETAILED ROOT DISEASE OUTPUT WILL BE PRINTED')
  goto 90

  13 continue

  !  ==========  OPTION NUMBER 13: INFKILL ==============================

  !     KEYWORD FOR INFECTION AT DEATH
  !     CHECK THAT ROOT DISEASE TYPE IS ACCEPTABLE

  IPOINT = 0
  if (LNOTBK(1)) IPOINT = INT(ARRAY(1))

  !.... IF ROOT DISEASE TYPE IS NOT VALID THEN SET VALUES FOR ALL
  !.... ROOT DISEASE TYPES.

  if (INT(ARRAY(1)) .gt. ITOTRR) then
     write(JOSTND,1302) ITOTRR,ARRAY(1)
     write(JOSTND,1303)
     IPOINT = 0
  end if

  1302 format (/,' ********   ERROR IN DISEASE TYPE, INFKILL KEYWORD.',&
       &' VALUE MUST BE LESS THAN ',I2,' VALUE ON KEYWORD=',F4.0)
  1303 format (' ********   CHANGE WILL AFFECT ALL ACTIVE DISEASE ',&
       &'TYPES')

  !.... SET ROOT DISEASE TYPE TO BE MODIFIED.

  IRMIN = MINRR
  IRMAX = MAXRR
  if (IPOINT .gt.0) then
     IRMIN = IPOINT
     IRMAX = IPOINT
  end if

  !.... CALL SPDECD TO GET SPECIES CODE (ALPHA OR NUMERIC).

  call SPDECD(2,ISPC,NSP(1,1),MAXSP,JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)

  !.... CHECK WHETHER ONLY ONE VALUE TO BE CHANGED, OR ALL

  if (ISPC .ge.1.and. ISPC .le. MAXSP) then

     !....    ONLY ONE VALUE TO BE CHANGED

     do IDI = IRMIN,IRMAX
        if (LNOTBK(3)) PKILLS(IRTSPC(ISPC),IDI) = ARRAY(3)
        1350 continue
     end do

  else if (ISPC .eq.0) then

     !....    ALL SPECIES GET CHANGED

     read (IREAD,1321) (PKILLS(IRTSPC(KSP),IRMIN),KSP = 1,MAXSP)

     do IDI = IRMIN+1,IRMAX
        do KSP = 1,MAXSP
           PKILLS(IRTSPC(KSP),IDI) = PKILLS(IRTSPC(KSP),IRMIN)
           1355 continue
        end do
        1357 continue
     end do
     1321 format (49F5.2)
  else

     !....    ERROR IN READING IN SPECIES CODE

     write(JOSTND,610) KEYWRD
     goto 90
  end if

  !.... OUTPUT FOR INFKILL KEYWORD

  1330 continue

  if (LKECHO)write(JOSTND,1331) KEYWRD
  1331 format (/1X,A8,'   PROPORTION ROOT INFECTION AT DEATH')
  if (LKECHO)write(JOSTND,1332)
  1332 format (/T13,5X,'SPECIES',8X,'PROPORTION INFECTED')
  if (LKECHO)write(JOSTND,1333)
  1333 format (T13,41('-'))

  do IDI = IRMIN,IRMAX
     if (LKECHO)write(JOSTND,1365) DISTYP(IDI)
     if (LKECHO)write(JOSTND,1366)

     do KSP = 1,MAXSP
        if (LKECHO)write(JOSTND,1335) RRJSP(IRTSPC(KSP)),&
             &PKILLS(IRTSPC(KSP),IDI)
        1334 continue
     end do
     1360 continue
  end do

  1365 format (T13,A16)
  1366 format (T13,16('-'))
  1335 format (T13,A16,13X,F5.2)

  goto 90

  14 continue

  !  ==========  OPTION NUMBER 14: WINDTHR ===============================

  !     WINDTHROW KEYWORD

  WINGO = .true.
  IDT = 1
  if (LNOTBK(1)) IDT = INT(ARRAY(1))
  PRMS(1) = ROWIND
  PRMS(2) = ROWMIN

  if (LNOTBK(2)) PRMS(1) = ARRAY(2)
  if (LNOTBK(3)) PRMS(2) = ARRAY(3)
  call OPNEW (KODE,IDT,MYACT(3),2,PRMS)
  if (KODE .gt.0) goto 90

  !.... OUTPUT FOR WINDTHR KEYWORD

  if (LKECHO)write(JOSTND,1401) KEYWRD, IDT,PRMS(1),PRMS(2)
  1401 format(/1X,A8,'   WINDTHROW EVENT TO BE ATTEMPTED IN DATE/CYCLE=',&
       &I4, ';   PROPORTION TO WINDTHROW =', F4.2,&
       &/T13, 'MINIMUM ELIGIBLE STEMS FOR EVENT =', F7.0)
  goto 90

  15 continue

  !  ==========  OPTION NUMBER 15: BBTYPE1 ===============================

  !     BARK BEETLE TYPE # 1

  BB1GO = .true.
  IDOBB = IDOBB + 1

  IDT = 1
  if (LNOTBK(1)) IDT = INT(ARRAY(1))

  !.... SET THE DEFAULTS FOR SPECIES, MINIMUM DBH, MIN ELIGIBLE TPA,
  !.... MORTALITY RATE, AND NUMBER OF OUTBREAKS.

  PRMS(1) = RROBTS(1)
  PRMS(2) = RROBSC(1)
  PRMS(3) = RROBOL(1)
  PRMS(4) = RROBMR(1)
  PRMS(5) = 0.0

  if (LNOTBK(2)) then

     !....    CALL SPDECD TO GET SPECIES CODE (ALPHA OR NUMERIC).

     call SPDECD(2,ISPC,NSP(1,1),MAXSP,JOSTND,IRECNT,KEYWRD,&
          &ARRAY,KARD)

     if (ISPC .ge.1.and. ISPC .le. MAXSP) then

        !....       VALID SPECIES CODE.

        PRMS(1) = ARRAY(2)

     else

        !....       INVALID SPECIES CODE.

        write(JOSTND,1510) KARD(2), ISPC
        1510 format (' ********   SPECIES = ', A3, ' (',I2,') ',&
             &'NOT VALID. DEFAULT WILL BE USED.')

        ISPC = INT(RROBTS(1))
        KARD(2)(1:2) = NSP(ISPC,1)(1:2)
        KARD(2)(3:3) = ' '
     end if
  else
     ISPC = INT(RROBTS(1))
     KARD(2)(1:2) = NSP(ISPC,1)(1:2)
     KARD(2)(3:3) = ' '
  end if

  if (LNOTBK(5)) PRMS(4) = ARRAY(5)
  if (LNOTBK(6)) PRMS(5) = ARRAY(6)

  if (LMTRIC) then
     if (LNOTBK(3)) PRMS(2) = ARRAY(3) * CMTOIN
     if (LNOTBK(4)) PRMS(3) = ARRAY(4) / HATOAC
  else
     if (LNOTBK(3)) PRMS(2) = ARRAY(3)
     if (LNOTBK(4)) PRMS(3) = ARRAY(4)
  end if

  call OPNEW (KODE,IDT,MYACT(4),5,PRMS)
  if (KODE .gt.0) goto 90

  !.... DIAGNOSTIC OUTPUT FOR BBTYPE1 KEYWORD: STEMS/ACRE METHOD

  if (.not. REINGO) then
     if (LMTRIC) then
        if (LKECHO)write(JOSTND, 1530) KEYWRD, IDT, KARD(2), ISPC,&
             &PRMS(2) * INTOCM, PRMS(3) / ACTOHA, PRMS(4)
     else
        if (LKECHO)write(JOSTND, 1520) KEYWRD, IDT, KARD(2), ISPC,&
             &(PRMS(I), I = 2,4)
     end if

     1520 format (/1X,A8,'   TYPE 1 BARK BEETLE TO BE ATTEMPTED IN DATE',&
          &'/CYCLE=', I5, ';   SPECIES =', A3, '(', I2, '); ',&
          &'MINIMUM DBH=', F5.2, ' INCHES',&
          &/T13, 'MINIMUM ELIGIBLE STEMS FOR EVENT =', F7.2,&
          &' STEMS/ACRE;   MORTALITY RATE = ', F5.3)

     1530 format (/1X,A8,'   TYPE 1 BARK BEETLE TO BE ATTEMPTED IN DATE',&
          &'/CYCLE=', I5, ';   SPECIES =', A3, '(', I2, '); ',&
          &'MINIMUM DBH=', F5.2, ' CMS',&
          &/T13, 'MINIMUM ELIGIBLE STEMS FOR EVENT =', F7.2,&
          &' STEMS/HA;   MORTALITY RATE = ', F5.3)

  end if

  !.... DIAGNOSTIC OUTPUT FOR BBTYPE1 KEYWORD: REINEKE METHOD

  if (REINGO) then
     if (LMTRIC) then
        if (LKECHO)write(JOSTND,1550) KEYWRD, IDT, KARD(2), ISPC,&
             &PRMS(2) * INTOCM, PRMS(3) / ACTOHA, PRMS(4)
     else
        if (LKECHO)write(JOSTND,1540) KEYWRD, IDT, KARD(2), ISPC,&
             &(PRMS(I), I = 2,4)
     end if

     1540 format (/1X,A8,'   TYPE 1 BARK BEETLE TO BE ATTEMPTED IN DATE',&
          &'/CYCLE=', I5, ';   SPECIES =', A3, '(', I2, '); ',&
          &'MINIMUM DBH=', F5.2, ' INCHES',&
          &/T13, 'MINIMUM REINEKE SDI=', F5.3,&
          &'  MORTALITY RATE = ', F5.3)

     1550 format (/1X,A8,'   TYPE 1 BARK BEETLE TO BE ATTEMPTED IN DATE',&
          &'/CYCLE=', I5, ';   SPECIES =', A3, '(', I2, '); ',&
          &'MINIMUM DBH=', F5.2, ' CMS',&
          &/T13, 'MINIMUM REINEKE SDI=', F5.3,&
          &'  MORTALITY RATE = ', F5.3)

  end if

  if (PRMS(5) .eq.1.0) then
     if (LKECHO)write(JOSTND,1560)
  else
     if (LKECHO)write(JOSTND,1570)
  end if

  1560 format (9X, '   ONLY ONE OUTBREAK WILL BE SCHEDULED FOR THIS ',&
       &'BARK BEETLE.')

  1570 format (9X, '   MULTIPLE OUTBREAKS WILL BE SCHEDULED FOR THIS ',&
       &'BARK BEETLE.')

  goto 90

  16 continue

  !  ==========  OPTION NUMBER 16: BBTYPE2 =============================

  !     TYPE 2 BARK BEETLES

  BB2GO = .true.
  IDOBB = IDOBB + 1

  IDT = 1
  if (LNOTBK(1)) IDT = INT(ARRAY(1))

  !.... SET THE DEFAULTS FOR SPECIES, MINIMUM DBH, MIN TPA WINDTHROWN,
  !.... MORTALITY RATE, AND NUMBER OF OUTBREAKS.

  PRMS(1) = RROBTS(2)
  PRMS(2) = RROBSC(2)
  PRMS(3) = RROBOL(2)
  PRMS(4) = RROBMR(2)
  PRMS(5) = 0.0

  if (LNOTBK(2)) then

     !....    CALL SPDECD TO GET SPECIES CODE (ALPHA OR NUMERIC).

     call SPDECD(2,ISPC,NSP(1,1),MAXSP,JOSTND,IRECNT,KEYWRD,&
          &ARRAY,KARD)

     if (ISPC .ge.1.and. ISPC .le. MAXSP) then

        !....       VALID SPECIES CODE.

        PRMS(1) = ARRAY(2)

     else

        !....       INVALID SPECIES CODE.

        write(JOSTND, 1510) KARD(2), ISPC
        ISPC = INT(RROBTS(2))
        KARD(2)(1:2) = NSP(ISPC,1)(1:2)
        KARD(2)(3:3) = ' '
     end if
  else
     ISPC = INT(RROBTS(2))
     KARD(2)(1:2) = NSP(ISPC,1)(1:2)
     KARD(2)(3:3) = ' '
  end if

  if (LNOTBK(5)) PRMS(4) = ARRAY(5)
  if (LNOTBK(6)) PRMS(5) = ARRAY(6)

  if (LMTRIC) then
     if (LNOTBK(3)) PRMS(2) = ARRAY(3) * CMTOIN
     if (LNOTBK(4)) PRMS(3) = ARRAY(4) / HATOAC
  else
     if (LNOTBK(3)) PRMS(2) = ARRAY(3)
     if (LNOTBK(4)) PRMS(3) = ARRAY(4)
  end if

  call OPNEW (KODE,IDT,MYACT(5),5,PRMS)
  if (KODE .gt.0) goto 90

  !.... OUTPUT FOR BBTYPE2 KEYWORD

  if (LMTRIC) then

     if (LKECHO)write(JOSTND,1611) KEYWRD, IDT, KARD(2), ISPC,&
          &PRMS(2) * INTOCM,&
          &PRMS(3) / ACTOHA, PRMS(4)
     1611 format (/1X,A8,'   TYPE 2 BARK BEETLE TO BE ATTEMPTED IN DATE',&
          &'/CYCLE=', I5, ';   SPECIES =', A3, '(', I2, '); ',&
          &'MINIMUM DBH=', F5.2, '  CMS',&
          &/T13, 'MINIMUM ELIGIBLE STEMS FOR EVENT =', F7.2,&
          &' STEMS/HA ;   MORTALITY RATE = ', F4.2)

  else
     if (LKECHO)write(JOSTND, 1601) KEYWRD, IDT, KARD(2), ISPC,&
          &(PRMS(I), I = 2,4)
     1601 format (/1X,A8,'   TYPE 2 BARK BEETLE TO BE ATTEMPTED IN DATE',&
          &'/CYCLE=', I5, ';   SPECIES =', A3, '(', I2, '); ',&
          &'MINIMUM DBH=', F5.2, '  INCHES',&
          &/T13, 'MINIMUM ELIGIBLE STEMS FOR EVENT =', F7.2,&
          &' STEMS/ACRE ;   MORTALITY RATE = ', F4.2)

  end if

  if (PRMS(5) .eq.1.0) then
     if (LKECHO)write(JOSTND,1560)
  else
     if (LKECHO)write(JOSTND,1570)
  end if

  goto 90

  17 continue

  !  ==========  OPTION NUMBER 17: BBTYPE3 ==============================

  !     TYPE 3 BARK BEETLES

  !     Changed order of paramter fields: 10/94

  IDOBB = IDOBB + 1

  IDT = 1
  if (LNOTBK(1)) IDT = INT(ARRAY(1))

  !.... SET THE DEFAULTS FOR SPECIES, MINIMUM DBH, MINIMUM ELIGIBLE TPA,
  !.... MORTALITY RATE, MINIMUM PROPORTION ROOTS INFECTED, AND
  !.... NUMBER OF OUTBREAKS.

  PRMS(1) = RROBTS(3)
  PRMS(2) = RROBSC(3)
  PRMS(3) = RROBOL(3)
  PRMS(4) = RROBMR(3)
  PRMS(5) = RROBRD(3)
  PRMS(6) = 0.0

  if (LNOTBK(2)) then

     !....    CALL SPDECD TO GET SPECIES CODE (ALPHA OR NUMERIC).

     call SPDECD(2,ISPC,NSP(1,1),MAXSP,JOSTND,IRECNT,KEYWRD,&
          &ARRAY,KARD)

     if (ISPC .ge.1.and. ISPC .le. MAXSP) then

        !....       VALID SPECIES CODE.

        PRMS(1) = ARRAY(2)

     else

        !....       INVALID SPECIES CODE.

        write(JOSTND,1510) KARD(2), ISPC
        ISPC = INT(RROBTS(3))
        KARD(2)(1:2) = NSP(ISPC,1)(1:2)
        KARD(2)(3:3) = ' '
     end if
  else
     ISPC = INT(RROBTS(3))
     KARD(2)(1:2) = NSP(ISPC,1)(1:2)
     KARD(2)(3:3) = ' '
  end if

  if (LMTRIC) then
     if (LNOTBK(3)) PRMS(2) = ARRAY(3) * CMTOIN
     if (LNOTBK(4)) PRMS(3) = ARRAY(4) / HATOAC
  else
     if (LNOTBK(3)) PRMS(2) = ARRAY(3)
     if (LNOTBK(4)) PRMS(3) = ARRAY(4)
  end if

  if (LNOTBK(5)) PRMS(5) = ARRAY(5)
  if (LNOTBK(6)) PRMS(6) = ARRAY(6)
  if (LNOTBK(7)) PRMS(4) = ARRAY(7)

  call OPNEW (KODE,IDT,MYACT(6),6,PRMS)
  if (KODE .gt.0) goto 90

  !.... OUTPUT FOR BBTYPE3 KEYWORD

  if (LMTRIC) then

     if (LKECHO)write(JOSTND,1711) KEYWRD, IDT, KARD(2), ISPC, PRMS(2&
          &PRMS(3)/ACTOHA, PRMS(4), PRMS(5)
     1711 format(/1X,A8,'   TYPE 3 BARK BEETLE TO BE ATTEMPTED IN DATE',&
          &'/CYCLE=', I5, ';   SPECIES =', A3, '(', I2, '); ',&
          &'MINIMUM DBH=', F5.2, '  CMS',&
          &/T13, 'MINIMUM ELIGIBLE STEMS FOR EVENT= ', F7.2,&
          &' STEMS/HA   ;  MORTALITY RATE = ', F4.2,&
          &/T13, 'MINIMUM ROOT DISEASE INFECTION= ', F4.2)

  else
     if (LKECHO)write(JOSTND, 1701) KEYWRD, IDT, KARD(2), ISPC,&
          &(PRMS(I), I = 2,5)
     1701 format(/1X,A8,'   TYPE 3 BARK BEETLE TO BE ATTEMPTED IN DATE',&
          &'/CYCLE=', I5, ';   SPECIES =', A3, '(', I2, '); ',&
          &'MINIMUM DBH=', F5.2, '  INCHES',&
          &/T13, 'MINIMUM ELIGIBLE STEMS FOR EVENT= ', F7.2,&
          &' STEMS/ACRE   ;  MORTALITY RATE = ', F4.2,&
          &/T13, 'MINIMUM ROOT DISEASE INFECTION= ', F4.2)

  end if

  if (PRMS(6) .eq.1.0) then
     if (LKECHO)write(JOSTND,1560)
  else
     if (LKECHO)write(JOSTND,1570)
  end if

  go to 90

  18 continue

  !  ==========  OPTION NUMBER 18: INOCSPAN =============================

  !     Minimum life span of inoculum

  !.... For each disease type, if a value was entered, set the XMINLF
  !.... array to the user defined value and then print out the value.

  do IDI = 1, ITOTRR
     if (LNOTBK(IDI)) then
        XMINLF(IDI) = ARRAY(IDI)
        if (LKECHO)write(JOSTND,1830) KEYWRD, DISTYP(IDI), XMINLF(IDI
     end if
     1820 continue
  end do

  1830 format (/1X,A8,'   MINIMUM LIFE SPAN FOR ',A12,' INOCULUM IS',&
       &F4.0, ' YEARS')

  goto 90

  19 continue

  !  ==========  OPTION NUMBER 19: RRCOMP  ==============================

  if (ARRAY(1) .gt.400.0.or. ARRAY(1) .le.0.0) goto 1910
  IRCOMP = INT(ARRAY(1))
  goto 1920

  1910 continue
  write(JOSTND,1911) ARRAY(1)
  1911 format (/1X,A8,'   *** ERROR, COMPRESSION TARGET READ AS ',F5.0,&
       &'.', /, T13, 'VALUE IS OUT OF BOUNDS. SET TO 400.')
  IRCOMP = 400

  goto 1930

  !.... OUTPUT FOR RRCOMP KEYWORD

  1920 continue
  if (LKECHO)write(JOSTND,1921) KEYWRD, IRCOMP
  1921 format (/1X,A8,'   COMPRESSION TARGET=',I5,' RECORDS')

  1930 continue
  goto 90

  20 continue

  !  ==========  OPTION NUMBER 20: RRECHO ===============================

  !     MACHINE READABLE SUMMARY OUTPUT KEYWORD.
  !     IRGEN(7) IS SET TO 1 IF RRECHO IS TO BE PRINTED.

  if (ARRAY(1) .gt.0.0) IRGEN(2) = INT(ARRAY(1))
  IRGEN(7) = 1

  !M    IF (ARRAY(2) .GT. 0.0) IRGEN(7) = INT(ARRAY(2))

  !.... OUTPUT FOR RRECHO KEYWORD

  if (LKECHO)write(JOSTND,2001) KEYWRD, IRGEN(2)
  2001 format (/1X,A8,'   MACHINE READABLE OUTPUT ON LOGICAL UNIT ',I4)

  !M    IF (IRGEN(7) .NE. 0) IF(LKECHO)WRITE(JOSTND,2002)
  !M  2002 FORMAT (T13,'OUTPUT WILL BE IN LOTUS READABLE FORMAT')

  goto 90

  21 continue

  !  ==========  OPTION NUMBER 21: STREAD ===============================

  !     Modified 10/94 to use multiple keywords instead of supplemental
  !     records.

  IRGEN(3) = 1

  !.... TEST TO MAKE SURE NONE OF THE FIRST FOUR FIELDS ARE BLANK.

  do I = 1,4
     if (.not.LNOTBK(I)) then
        write(JOSTND,2101)
        2101 format(/,' **** BLANK FIELD IN STREAD.  KEYWORD IGNORED.')
        goto 90
     end if
     2110 continue
  end do

  !.... CALL SPDECD TO GET SPECIES CODE (ALPHA OR NUMERIC).

  call SPDECD(1,ISPC,NSP(1,1),MAXSP,JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)

  if (ISPC .lt.1.or. ISPC .gt. MAXSP) then

     !....    INVALID SPECIES CODE.

     write(JOSTND,2102) KARD(1), ISPC
     2102 format (/,' **** STREAD: INVALID SPECIES. ',&
          &'SPECIES = ', A3, ' (', I2, ').  KEYWORD IGNORED.')
     goto 90
  end if

  ISL = INT(ARRAY(2))
  if (ISL .lt.1.or. ISL .gt.5) then
     write(JOSTND,2103) ISL
     2103 format (/,' **** STREAD: STUMP CLASS=',I3, ' OUT OF RANGE.',&
          &' KEYWORD IGNORED.')
     goto 90
  end if

  DEN = ARRAY(3)
  if (DEN .lt.0.0) then
     write(JOSTND,2104) DEN
     2104 format (/,' **** STREAD: NEGATIVE NUMBER OF STUMPS=',F6.1,&
          &' KEYWORD IGNORED.')
     goto 90
  end if

  IRRSP = INT(ARRAY(4))
  if (IRRSP .lt.1.or. IRRSP .gt. ITOTRR) then
     write(JOSTND,2105) IRRSP
     2105 format (/,' **** STREAD: DISEASE TYPE=',I2, ' OUT OF RANGE.',&
          &' KEYWORD IGNORED.')
     goto 90
  end if

  if (LNOTBK(5)) then
     JAGE = INT(ARRAY(5))
  else
     JAGE = 0
  end if

  if (JAGE .lt.0) then
     write(JOSTND,2106) JAGE
     2106 format (/,' **** STREAD: NEGATIVE YEARS SINCE DEATH=',I4,&
          &' KEYWORD IGNORED.')
     goto 90
  end if

  ISTFLG = 1

  !.... CONVERT STUMP CLASS TO DIAMETER (INCHES)

  DIAM = STCUT(ISL) + 6.0

  !.... ESTIMATE ROOT RADIUS

  call RDROOT(ISPC,DIAM,ANS,PROOT(IRTSPC(ISPC)),RSLOP(IRTSPC(ISPC)),&
       &0.0)
  RTD = ANS

  !.... ADD TO STUMP ARRAY

  call RDSTP (ISL,ISPC,DEN,DIAM,RTD)

  !     IF(LKECHO)WRITE(JOSTND,2159) 'AFTER', ISL, ISPC, DEN, DIAM, RTD, IRRSP
  !2159 FORMAT (1X,A10,' ISL=',I4, ' ISPC=',I4, '   DEN=',F8.2,
  !    &        '   DIAM=',F8.2, '   RTD=',F8.2,
  !    &        '   IRRSP=',I4)

  !.... RESET AGE TO USER INPUT, IF DIFFERENT FROM THAT SET IN RDSTP

  if (JAGE .ne. IYEAR) then
     JRAGED(IRRSP,ISPS(IRTSPC(ISPC)),ISL,1) = -JAGE
  end if
  ISTFLG = 0

  !.... WRITE OUTPUT FOR STREAD KEYWORD.

  if (LKECHO)write(JOSTND,2131) KEYWRD, KARD(1), ISPC, ISL, DEN,&
       &DISTYP(IRRSP), JAGE
  2131 format (/, 1X, A8, '   STUMPS ENTERED WITH FOLLOWING ATTRIBUTES:',&
       &/, T13, 'SPECIES = ', A3, '(', I2, ')   STUMP CLASS = ',&
       &I2, '   NUMBER OF STUMPS = ', F6.1, '   DISEASE TYPE = ',&
       &A12, '   YEARS SINCE DEATH = ', I3)

  goto 90

  22 continue

  !  ==========  OPTION NUMBER 22: TDISTN ===============================

  !     TREE DISTRIBUTION FOR SPREAD MODEL

  if (ARRAY(1) .gt.0.0) IRGEN(4) = INT(ARRAY(1))
  if (ARRAY(2) .eq.0.0.or. ARRAY(2) .eq.1.0)&
       &IRSTYP = INT(ARRAY(2))
  if (LNOTBK(3)) RRSFRN = ARRAY(3)

  !.... OUPUT FOR TDISTN KEYWORD

  if ((IRSTYP .eq.0).and.LKECHO)write(JOSTND,2200) KEYWRD
  if ((IRSTYP .eq.1).and.LKECHO)write(JOSTND,2201) KEYWRD
  2200 format (/,1X,A8,'   TREE DISTRIBUTION IS RANDOM')
  2201 format (/,1X,A8,'   TREE DISTRIBUTION IS LATTICE')

  if (IRGEN(4) .eq.3000) goto 2230

  if (LKECHO)write(JOSTND,2202) IRGEN(4)
  2202 format (T13,'SPATIAL DISTRIBUTION WILL CHANGE',&
       &' TO OTHER FORM IN ',I4)

  2230 continue
  IRG = IRGEN(4)
  if (((IRSTYP.eq.1).or.(IRG .ne.3000))&
       &.and.LKECHO)write(JOSTND,2231) RRSFRN
  2231 format (T13,'STANDARD DEVIATION OF LATTICE DISTRIBUTION IS ',F7.3)

  goto 90

  23 continue

  !  ==========  OPTION NUMBER 23: RRJUMP ==============================

  !     SPECIFIES THE EXTENT TO WHICH ROOT DISEASE CENTERS WILL EXPAND
  !     AFTER A CUT.

  !.... WRITE OUT KEYWORD INFO.

  if (LKECHO)write(JOSTND,2301) KEYWRD
  2301 format (/,1X,A8,'   NUMBER OF RADII THE AREA IN SPECIFIED ROOT ',&
       &'DISEASE WILL JUMP OUT')

  !.... FOR EACH DISEASE TYPE, IF A VALUE WAS ENTERED, SET THE TNJUMP
  !.... ARRAY TO THE USER DEFINED VALUE AND THEN PRINT OUT THE VALUE.

  do IDI = 1,ITOTRR
     if (LNOTBK(IDI)) then

        !....       A VALUE WAS ENTERED FOR THIS DISEASE TYPE (IDI), SET
        !....       TNJUMP ARRAY.

        TNJUMP(IDI) = ARRAY(IDI)

        !....       OUTPUT VALUE FOR DISEASE TYPE (IDI)

        if (LKECHO)write(JOSTND,2312) DISTYP(IDI), TNJUMP(IDI)
     end if
     2305 continue
  end do

  2312 format (T13,1X,A12,5X,F5.2)

  goto 90

  24 continue

  !  ==========  OPTION NUMBER 24: RRMINK ================================

  !     MINIMUM YEARS TO KILL TREES

  do IDI = 1,ITOTRR
     if (LNOTBK(IDI)) then
        XMINKL(IDI) = ARRAY(IDI)

        !....       OUTPUT FOR RRMINK KEYWORD

        if (LKECHO)write(JOSTND,2401) KEYWRD, XMINKL(IDI), DISTYP(IDI
     end if
     2420 continue
  end do

  2401 format (/1X,A8,'   NO HABITAT INFLUENCE BELOW ',F4.0,&
       &' YEARS, ',A12 )

  goto 90

  25 continue

  !  ==========  OPTION NUMBER 25: RSEED ================================

  !     KEYWORD FOR REINITIALIZING ROOT DISEASE RANDOM NUMBER GENERATOR

  if (LNOTBK(1).and.ARRAY(1).eq.0.0) call GETSED (ARRAY(1))
  if (LNOTBK(1)) DSEED = ARRAY(1)

  if (LKECHO)write(JOSTND,2501) KEYWRD, DSEED
  2501 format (/1X,A8,'   SEED TO REINITIALIZE RANDOM NUMBER GENERATOR=',&
       &F10.2)

  goto 90

  26 continue

  !  ==========  OPTION NUMBER 26: PLREAD ===============================

  !     KEYWORD TO READ IN SUB-PLOT ID'S

  IDI = MINRR
  if (LNOTBK(1)) IDI = INT(ARRAY(1))

  K = 0
  LONECT(IDI) = 2

  2601 read (IREAD,2602) IDSPLT
  2602 format (I4)

  if (IDSPLT .eq. -999) goto 2610
  K = K + 1
  IRDPLT(IDI,K) = IDSPLT
  goto 2601

  2610 continue

  !.... WRITE KEYWORD, DISEASE SUB-PLOTS

  if (K .le.25) M = K
  if (K .gt.25) M = 25

  if (LKECHO)write(JOSTND,2611) KEYWRD, DISTYP(IDI),&
       &(IRDPLT(IDI,J),J = 1,M)
  2611 format (/1X,A8,'   ROOT DISEASE IS ', A12 /&
       &T13, 'DISEASE SUBPLOTS READ IN'/&
       &T13, 'DISEASED PLOTS=', 25(I4))

  if (K .le.25) goto 90

  if (LKECHO)write(JOSTND,2612)
  2612 format (/)
  NEXT = M + 1

  if (LKECHO)write(JOSTND,2613) (IRDPLT(IDI,J),J = NEXT,K)
  2613 format (T27,25I4)
  goto 90

  27 continue

  !  ==========  OPTION NUMBER 27: --RDv3-- =============================

  !     THIS IS NOT AN ACTUAL KEYWORD. IT IS JUST A PLACEHOLDER OCCUPYING
  !     THE SPACE OF A KEYWORD THAT WAS REMOVED.

  !.... OUTPUT

  if (LKECHO)write(JOSTND,2710) KEYWRD
  2710 format (/1X,A8,'   THIS RD KEYWORD ARRAY POSITION IS NOT USED.')

  goto 90

  28 continue

  !  ==========  OPTION NUMBER 28: RRTREIN  =============================

  !     THIS KEYWORD USED WHEN ROOT DISEASE INITIALIZATION IS TAKEN
  !     DIRECTLY FROM THE INPUT TREELIST.

  RRTINV = .true.

  !.... OUTPUT FOR RRTREIN KEYWORD

  if (LKECHO)write(JOSTND,2856) KEYWRD
  2856 format (/1X,A8,'   ROOT DISEASE INITIALIZATION IS FROM TREELIST')

  goto 90

  29 continue

  !  ==========  OPTION NUMBER 29: INFCOLO  =============================

  !     USED TO CHANGE THE PROPORTION OF ROOT SYSTEMS COLONIZED AFTER
  !     DEATH.

  IPOINT = 0
  if (LNOTBK(3)) IPOINT = INT(ARRAY(3))

  if (INT(ARRAY(3)) .gt. ITOTRR) then
     if (LKECHO)write(JOSTND,2960) KEYWRD, ARRAY(3),ITOTRR
     IPOINT = 0
  end if

  2960 format (/T13,A8,'**** INVALID DISEASE TYPE. VALUE ENTERED: ',&
       &F3.0,' VALUE MUST BE LESS THAN ',I2,&
       &/T13,8X, 'CHANGE WILL AFFECT ALL DISEASE TYPES')

  !.... SET ROOT DISEASE TYPE TO BE MODIFIED.

  IRMIN = MINRR
  IRMAX = MAXRR
  if (IPOINT .gt.0) then
     IRMIN = IPOINT
     IRMAX = IPOINT
  end if

  !.... CALL SPDECD TO GET SPECIES CODE (ALPHA OR NUMERIC).

  call SPDECD(1,ISPC,NSP(1,1),MAXSP,JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)

  if (ISPC .ge.1.and. ISPC .le. MAXSP) then

     !....    ONLY ONE SPECIES TO BE CHANGED.

     do IDI = IRMIN,IRMAX
        if (LNOTBK(2)) PCOLO(IRTSPC(ISPC),IDI) = ARRAY(2)
        2965 continue
     end do

  else

     !....    ALL SPECIES GET CHANGED.

     read (IREAD,2921) (PCOLO(IRTSPC(KSP),IRMIN), KSP = 1,MAXSP)
     2921 format (49F5.2)

     do IDI = IRMIN+1,IRMAX
        do KSP = 1,MAXSP
           PCOLO(IRTSPC(KSP),IDI) = PCOLO(IRTSPC(KSP),IRMIN)
           2966 continue
        end do
        2967 continue
     end do
  end if

  !.... OUTPUT FOR INFCOLO KEYWORD

  2930 continue
  if (LKECHO)write(JOSTND,2931) KEYWRD
  2931 format (/1X,A8,'   PROPORTION ROOT COLONIZED AT DEATH')
  if (LKECHO)write(JOSTND,2932)
  2932 format (/T13,5X,'SPECIES',8X,'PROPORTION COLONIZED')
  if (LKECHO)write(JOSTND,2933)
  2933 format (T13,41('-'))

  do IDI = IRMIN,IRMAX
     if (LKECHO)write(JOSTND,2971) DISTYP(IDI)
     if (LKECHO)write(JOSTND,2972)
     do KSP = 1,MAXSP
        if (LKECHO)write(JOSTND,2935) RRJSP(IRTSPC(KSP)),&
             &PCOLO(IRTSPC(KSP),IDI)
        2934 continue
     end do
     2970 continue
  end do

  2935 format (T13,A16,13X,F5.2)
  2971 format (T13,A16)
  2972 format (T13,16('-'))

  goto 90

  30 continue

  !  ==========  OPTION NUMBER 30: BORATE  =============================

  if (MINRR .gt.2.and. MAXRR .gt.2) then
     write(JOSTND,3002)
     3002 format(/,' **** BORATE: INVALID DISEASE TYPE ACTIVE. ',&
          &'BORATE APPLIES ONLY TO ANNOSUS.',&
          &/T13,'KEYWORD WILL BE IGNORED.')
     goto 90
  end if

  IDT = 1
  if (LNOTBK(1)) IDT = INT(ARRAY(1))

  PRMS(1) = BOTRT
  PRMS(2) = BODBH

  if (LNOTBK(2)) PRMS(1) = ARRAY(2)
  if (LNOTBK(3)) PRMS(2) = ARRAY(3)

  if (LMTRIC .and. LNOTBK(3)) PRMS(2) = ARRAY(3) * CMTOIN

  call OPNEW (KODE,IDT,MYACT(7),2,PRMS)
  if (KODE .gt.0) goto 90

  LBORAX(1) = .true.

  if (LMTRIC) then
     if (LKECHO)write(JOSTND,3010) KEYWRD,IDT,PRMS(1),PRMS(2)*INTOCM
  else
     if (LKECHO)write(JOSTND,3010) KEYWRD,IDT, (PRMS(I), I = 1,2)
  end if

  3010 format (/1X,A8,'   BORATE WILL BE APPLIED IN DATE/CYCLE=',I5,&
       &'     PROP TREAT:', F4.2, '   MIN DBH:', F6.2)

  !      IF (LNOTBK(1) .AND. ARRAY(1) .GE. 0.0 .AND. ARRAY(1) .LE. 1.0)
  !     &   BOTRT = ARRAY(1)
  !      IF (LNOTBK(2)) BODBH = ARRAY(2)

  !      IF(LKECHO)WRITE(JOSTND,3000) BOTRT, BODBH
  ! 3000 FORMAT (/1X,'BORAX      PROP TREAT:', F4.2, '   MIN DBH:', F6.2)

  goto 90

  31 continue

  !  ==========  OPTION NUMBER 31: SPORE    =============================

  !     FIELD 5 CONTAINS THE DISEASE TYPE.  1 = P TYPE.  2 = S TYPE.

  IDT = 1
  if (LNOTBK(1)) IDT = INT(ARRAY(1))

  IDI = INT(ARRAY(6))
  if (IDI .ne.2) IDI = 1

  PRMS(1) = SPINF(IDI)
  PRMS(2) = SPDBH(IDI)
  PRMS(3) = SPYTK(IDI)
  PRMS(4) = SPTRAN(IDI)
  PRMS(5) = IDI

  if (LNOTBK(2)) PRMS(1) = ARRAY(2)
  if (LNOTBK(3)) PRMS(2) = ARRAY(3)
  if (LNOTBK(4)) PRMS(3) = ARRAY(4)
  if (LNOTBK(5)) PRMS(4) = ARRAY(5)

  if (LMTRIC .and. LNOTBK(3)) PRMS(2) = ARRAY(3) * CMTOIN

  call OPNEW (KODE,IDT,MYACT(8),5,PRMS)
  if (KODE .gt.0) goto 90

  if (LMTRIC) then
     if (LKECHO)write(JOSTND,3100) KEYWRD,IDT,PRMS(1),PRMS(2)*INTOCM,&
          &PRMS(3),PRMS(4),PRMS(5)
  else
     if (LKECHO)write(JOSTND,3100) KEYWRD,IDT,(PRMS(I), I = 1,5)
  end if

  3100 format (/1X,A8,'   IN DATE/CYCLE : ',I4,' VALUES WILL BE ',&
       &'CHANGED TO :',&
       &/T13, 'PROPORTION OF STUMPS TO INFECT :',F4.2,&
       &/T13, 'MINIMUM STUMP DIAMETER TO FORM NEW CENTERS :',F6.2,&
       &/T13, 'MULTIPLIER FOR TIME-TO-DEATH :',F7.2,&
       &/T13, 'MULTIPLIER FOR PROBABILITY OF INFECTION :', F6.2,&
       &/T13, 'DISEASE TYPE :',F3.0)

  goto 90

  32 continue

  !  ==========  OPTION NUMBER 32: BBTYPE4 ==============================

  !     TYPE 4 BARK BEETLES

  !     Changed order of parameter fields: 10/94

  IDOBB = IDOBB + 1

  IDT = 1
  if (LNOTBK(1)) IDT = INT(ARRAY(1))

  !.... SET THE DEFAULTS FOR SPECIES, MINIMUM DBH, MINIMUM ELIGIBLE TPA,
  !.... MORTALITY RATE, AND MINIMUM PROPORTION ROOTS INFECTED.

  PRMS(1) = RROBTS(4)
  PRMS(2) = RROBSC(4)
  PRMS(3) = RROBOL(4)
  PRMS(4) = RROBMR(4)
  PRMS(5) = RROBRD(4)

  !.... SET THE DEFAULTS FOR MORTALITY RATES AND NUMBER OF OUTBREAKS.

  PRMS(6) = 0.3
  PRMS(7) = 0.15
  PRMS(8) = 0.01
  PRMS(9) = 0.0

  if (LNOTBK(2)) then

     !....    CALL SPDECD TO GET SPECIES CODE (ALPHA OR NUMERIC).

     call SPDECD(2,ISPC,NSP(1,1),MAXSP,JOSTND,IRECNT,KEYWRD,&
          &ARRAY,KARD)

     if (ISPC .ge.1.and. ISPC .le. MAXSP) then

        !....       VALID SPECIES CODE.

        PRMS(1) = ARRAY(2)

     else

        !....       INVALID SPECIES CODE.

        if (LKECHO)write(JOSTND,1510) KARD(2), ISPC
        ISPC = INT(RROBTS(4))
        KARD(2)(1:2) = NSP(ISPC,1)(1:2)
        KARD(2)(3:3) = ' '
     end if
  else
     ISPC = INT(RROBTS(4))
     KARD(2)(1:2) = NSP(ISPC,1)(1:2)
     KARD(2)(3:3) = ' '
  end if

  if (LMTRIC) then
     if (LNOTBK(3)) PRMS(2) = ARRAY(3) * CMTOIN
     if (LNOTBK(4)) PRMS(3) = ARRAY(4) / HATOAC
  else
     if (LNOTBK(3)) PRMS(2) = ARRAY(3)
     if (LNOTBK(4)) PRMS(3) = ARRAY(4)
  end if

  if (LNOTBK(5)) PRMS(5) = ARRAY(5)
  if (LNOTBK(6)) PRMS(9) = ARRAY(6)
  if (LNOTBK(7)) PRMS(4) = ARRAY(7)

  3201 read (IREAD,3202) T1, T2, T3
  3202 format (A10,A10,A10)

  if (T1.ne.'          ') read (T1,'(G10.0)') PRMS(6)
  if (T2.ne.'          ') read (T2,'(G10.0)') PRMS(7)
  if (T3.ne.'          ') read (T3,'(G10.0)') PRMS(8)

  call OPNEW (KODE,IDT,MYACT(9),9,PRMS)
  if (KODE .gt.0) goto 90

  !.... OUTPUT FOR BBTYPE4 KEYWORD

  if (LMTRIC) then
     if (LKECHO)write(JOSTND,3213) KEYWRD, IDT, KARD(2), ISPC,&
          &PRMS(2)*INTOCM,&
          &PRMS(3)/ACTOHA, PRMS(5), PRMS(4),&
          &(PRMS(I),I = 6,8)
  else
     if (LKECHO)write(JOSTND,3203) KEYWRD,IDT,KARD(2),ISPC,PRMS(2),&
          &PRMS(3), PRMS(5), PRMS(4), (PRMS(I),I = 6,8)
  end if

  3203 format(/1X, A8, '   TYPE 4 BARK BEETLE TO BE ATTEMPTED IN DATE',&
       &'/CYCLE=', I5, ';   SPECIES =', A3, '(', I2, ');  ',&
       &'MINIMUM DBH=', F5.2, '  INCHES',&
       &/T13, 'MINIMUM ELIGIBLE STEMS FOR EVENT= ', F7.2,&
       &' STEMS/ACRE ; MINIMUM ROOT DISEASE INFECTION= ', F5.2,&
       &/T13, 'INFECTED MORTALITY= ', F5.3,&
       &' UNINFECTED-WITHIN-PATCH MORTALITY= ', F5.2,&
       &/T13, 'UNINFECTED-FRINGE MORTALITY=', F5.2,&
       &' UNINFECTED-NON-PATCH MORTALITY= ', F5.2)

  3213 format(/1X, A8, '   TYPE 4 BARK BEETLE TO BE ATTEMPTED IN DATE',&
       &'/CYCLE=', I5, ';   SPECIES =', A3, '(', I2, ');  ',&
       &'MINIMUM DBH=', F5.2, '  CMS',&
       &/T13, 'MINIMUM ELIGIBLE STEMS FOR EVENT= ', F7.2,&
       &' STEMS/HA ; MINIMUM ROOT DISEASE INFECTION= ', F5.2,&
       &/T13, 'INFECTED MORTALITY= ', F5.3,&
       &' UNINFECTED-WITHIN-PATCH MORTALITY= ', F5.2,&
       &/T13, 'UNINFECTED-FRINGE MORTALITY=', F5.2,&
       &' UNINFECTED-NON-PATCH MORTALITY= ', F5.2)

  if (PRMS(9) .eq.1.0) then
     if (LKECHO)write(JOSTND,1560)
  else
     if (LKECHO)write(JOSTND,1570)
  end if

  go to 90

  33 continue

  !  ==========  OPTION NUMBER 33: DNSCALC ==============================

  !     PROCESS THE ARGUMENTS TO IMPLEMENT THE REINEKE STAND DENSITY
  !     INDEX METHOD FOR EVALUATING THE THRESHOLD FOR A BARK BEETLE
  !     TYPE 1 OUTBREAK.
  !     ASSIGN REINEK(4) IF THE ARGUMENT IS { -2.5 <= arg <= -0.5 }

  REINGO = .true.

  do I = 1,3
     if (LNOTBK(I)) REINEK(I) = ARRAY(I)
     if (REINEK(I) .ne.0) REINEK(I) = 1
     3300 continue
  end do

  if (LMTRIC) ARRAY(4) = (ARRAY(4) / HATOAC) / CMTOIN
  if (LNOTBK(4) .and. ARRAY(4) .le. -0.5.and. ARRAY(4) .ge. -2.5)&
       &REINEK(4) = ARRAY(4)

  !.... OUTPUT FOR DNSCALC KEYWORD

  if (LKECHO)write(JOSTND, 3302) KEYWRD

  if (REINEK(1) .eq.0) then
     if (LKECHO)write(JOSTND, 3303) 'TREES/ACRE'
  else
     if (LKECHO)&
          &write(JOSTND, 3303) 'REINEKE STAND DENSITY INDEX (SDI)'
  end if

  if (REINEK(2) .eq.0) then
     if (LKECHO)write(JOSTND, 3303) 'ENTIRE STAND'
  else
     if (LKECHO)&
          &write(JOSTND, 3303) 'ONLY CLEAN (OUTSIDE DISEASE PATCHES)'
  end if

  if (REINEK(3) .eq.0) then
     if (LKECHO)write(JOSTND, 3303) 'ONLY LIVING TREES'
  else
     if (LKECHO)&
          &write(JOSTND, 3303) 'ALL TREES (LIVING AND STANDING DEAD)'
  end if

  if (REINEK(1) .ne.0) then
     if (LMTRIC) then
        if (LKECHO)write(JOSTND, 3306) (REINEK(4) / ACTOHA) / INTOCM
     else
        if (LKECHO)write(JOSTND, 3306) REINEK(4)
     end if
  end if

  3302 format (/1X,A8, '  DNSCALC (REINEKE) OPTIONS:')
  3303 format (1X, A40, ' USED TO CALCULATE OUTBREAK DENSITY THRESHOLD')
  3306 format (1X, ' SLOPE OF SDI FUNCTION = ', F5.3)

  go to 90

  34 continue

  !  ==========  OPTION NUMBER 34: SMCOUT ===============================

  !     OUTPUT FROM THE SPREAD RATE MONTE CARLO SIMULATION
  !     (IN REGULAR TABLE FORMAT, WITH HEADINGS)

  IMCOUT = 26
  if (ARRAY(1) .gt.0.0) IMCOUT = INT(ARRAY(1))

  !.... OUTPUT FOR SMCOUT KEYWORD

  if (LKECHO)write(JOSTND,3401) KEYWRD, IMCOUT
  3401 format (/1X,A8,'   SPREAD RATE MONTE CARLO OUTPUT ON LOGICAL',&
       &' UNIT ',I4)

  goto 90

  35 continue

  !  ==========  OPTION NUMBER 35: BBOUT ===============================

  !     OUTPUT DETAILING BARK BEETLE MORTALITY BY SIZE CLASS
  !     (IN REGULAR TABLE FORMAT, WITH HEADINGS)

  IBBOUT = 27
  if (ARRAY(1) .gt.0.0) IBBOUT = INT(ARRAY(1))

  !.... OUTPUT FOR BBOUT KEYWORD

  if (LKECHO)write(JOSTND,3501) KEYWRD, IBBOUT
  3501 format (/1X,A8,'   DETAILED BARK BEETLE MORTALITY OUTPUT ON ',&
       &'LOGICAL UNIT ',I4)

  goto 90

  36 continue

  !  ==========  OPTION NUMBER 36: PLOTINF ===============================

  !     Set the percent infected in each plot. If a plot is not mentioned,
  !     assume there is no infection and that all trees in that plot are
  !     outside centers.

  !.... Set the root disease type from field 1.

  IDI = MINRR
  if (LNOTBK(1)) IDI = INT(ARRAY(1))

  !.... Set the LONECT flag to show that the model is being run with
  !.... multiple plots and tell the model that PLOTINF is being used.

  LONECT(IDI) = 2
  LPLINF = .true.

  K = 0

  !.... Start loop to read supplemental records.

  3601 read (IREAD,3602) IDSPLT,RDPRP
  3602 format (I4,F5.2)

  if (IDSPLT .eq. -999.or. K .eq.50) goto 3610

  K = K + 1
  IANPLT(IDI,K) = IDSPLT
  PLPROP(IDI,K) = RDPRP
  goto 3601

  3610 continue

  !.... Set M to print out sub-plot info.  If there are more then 25
  !.... sub-plots then more then one record needs to be written.

  if (K .le.25) M = K
  if (K .gt.25) M = 25

  !.... Write keyword.

  if (LKECHO)write(JOSTND,3611) KEYWRD, DISTYP(IDI),&
       &(IANPLT(IDI,J),J = 1,M)

  3611 format (/1X,A8,'   ROOT DISEASE IS ', A12 /&
       &T13, 'DISEASE SUBPLOTS READ IN'/&
       &T13, 'DISEASED PLOTS=', 25(I5))

  if (K .le.25) goto 3655

  !.... Write a new line.

  if (LKECHO)write(JOSTND,3612)
  3612 format (/)

  !.... Set NEXT to print sub-plot numbers 25 to 50.

  NEXT = M + 1

  if (LKECHO)write(JOSTND,3613) (IANPLT(IDI,J),J = NEXT,K)
  3613 format (T27,25(I5))

  3655 continue

  !.... Print out the proportion of root disease in each sub-plot.

  if (LKECHO)write(JOSTND,3621) (PLPROP(IDI,J),J = 1,M)
  3621 format (T13, 'DISEASE PROPS =',25(F5.2))

  if (K .le.25) goto 90

  !.... Write a new line.

  if (LKECHO)write(JOSTND,3622)
  3622 format (/)

  !.... Set NEXT to print sub-plot numbers 25 to 50.

  NEXT = M + 1

  if (LKECHO)write(JOSTND,3633) (PLPROP(IDI,J),J = NEXT,K)
  3633 format (T27,25(F5.2))

  goto 90

  37 continue

  !  ==========  OPTION NUMBER 37: SDIRMULT ===========================

  !     ROOT RADIUS MULTIPLIER BASED ON SDI. USER SPECIFIES A SLOPE
  !     AND THE SDI AT WHICH THE MULTIPLIER SHOULD BE ONE. (SLOPE
  !     SHOULD NORMALLY BE NEGATIVE). DEFAULT IS NO MULTIPLER

  SDNORM = 369.0
  SDISLP = -0.0033

  if (LMTRIC) then
     if (ARRAY(1) .gt.0.0) SDNORM = ARRAY(1) / HATOAC
     if (LNOTBK(2)) SDISLP = ARRAY(2) * HATOAC
  else
     if (ARRAY(1) .gt.0.0) SDNORM = ARRAY(1)
     if (LNOTBK(2)) SDISLP = ARRAY(2)
  end if

  !.... OUTPUT FOR SMCOUT KEYWORD

  if (LMTRIC) then
     if (LKECHO)write(JOSTND,3701)KEYWRD,SDNORM/ACTOHA,SDISLP*ACTOHA
  else
     if (LKECHO)write(JOSTND,3701) KEYWRD, SDNORM, SDISLP
  end if

  3701 format (/1X,A8,'   ROOT RADIUS MULTIPLER IS ONE AT SDI= ',F6.1,&
       &' WITH SLOPE = ',F8.4)

  goto 90

  38 continue

  !  ==========  OPTION NUMBER 38: TIMEDEAD ==============================

  !     TIME SINCE DEATH FOR DEAD STUFF IN THE INVENTORY.  USED TO REDUCE
  !     THE AMOUNT OF INOCULUM PRESENT INITIALLY IN THE STAND BY DECAYING
  !     THEDEAD STUFF TO THE PRESENT TIME.  USER CAN SPECIFY DIFFERENT
  !     VALUES FOR EACH SIZE CLASS.  FIELD 6 (ONE VALUE FOR ALL CLASSES)
  !     ELIMINATED 10/94.

  if (LNOTBK(1)) DEDAGE(1) = INT(ARRAY(1))
  if (LNOTBK(2)) DEDAGE(2) = INT(ARRAY(2))
  if (LNOTBK(3)) DEDAGE(3) = INT(ARRAY(3))
  if (LNOTBK(4)) DEDAGE(4) = INT(ARRAY(4))
  if (LNOTBK(5)) DEDAGE(5) = INT(ARRAY(5))

  !.... OUTPUT FOR TIMEDEAD KEYWORD

  if (LMTRIC) then
     if (LKECHO)write(JOSTND,3811) KEYWRD, (DEDAGE(J),J = 1,5)
  else
     if (LKECHO)write(JOSTND,3801) KEYWRD, (DEDAGE(J),J = 1,5)
  end if

  3801 format (/1X, A8, '   TIME SINCE DEATH OF TREES IN INVENTORY IS:',&
       &/T13, ' 0- 12":', I4, '  12-24":', I4, '  24-48":', I4,&
       &/T13, '48-100":', I4, '   >100":', I4)
  3811 format (/1X, A8, '   TIME SINCE DEATH OF TREES IN INVENTORY IS:',&
       &/T13, '  0- 30CM:', I4, '  30-61CM:', I4,'  61-122CM:',I4,&
       &/T13, '122-254CM:', I4, '   >254CM:', I4)

  goto 90

  39 continue

  !  ==========  OPTION NUMBER 39: RRHOSTS ==============================

  !     THIS KEYWORD CHANGES THE HOST TREES FOR EACH OF THE ROOT DISEASE
  !     TYPES SO THAT USERS CAN RUN THE MODEL THINKING ABOUT ROOT DISEASE
  !     AS SOME OTHER MORTALITY AGENT

  ISPC = 0
  IDI = 1

  if (LNOTBK(1)) then

     !....    CALL SPDECD TO GET SPECIES CODE (ALPHA OR NUMERIC).

     call SPDECD(1,ISPC,NSP(1,1),MAXSP,JOSTND,IRECNT,KEYWRD,&
          &ARRAY,KARD)

     if (ISPC .ge.1.and. ISPC .le. MAXSP) then

        !....       Change only one species

        if (LNOTBK(2)) IDI = INT(ARRAY(2))

        if (IDI .lt.0.or. IDI .gt. ITOTRR) then

           !....          INVALID DISEASE TYPE

           write(JOSTND,3965) RRJSP(IRTSPC(ISPC)), IDI
           IDI = MAXRR
           if (MAXRR .lt.3) IDI = IDITYP(IRTSPC(ISPC))
           write(JOSTND,3966) IDI, DISTYP(IDI)
        else
           if (MAXRR .lt.3) IDITYP(IRTSPC(ISPC)) = IDI
        end if

     else if (ISPC .eq.0) then

        !....       Change all species

        read(IREAD,3963) (IHST(KSP), KSP = 1,MAXSP)
        3963 format(49I5)

        do KSP = 1,MAXSP
           IDI = IHST(KSP)

           if (IDI .lt.0.or. IDI .gt. ITOTRR) then

              !....             INVALID DISEASE TYPE

              write(JOSTND,3965) RRJSP(IRTSPC(KSP)), IDI
              IDI = IDITYP(IRTSPC(KSP))
              write(JOSTND,3966) IDI, DISTYP(IDI)
           else

              !....             VALID DISEASE TYPE

              IDITYP(IRTSPC(KSP)) = IDI
           end if
           3910 continue
        end do

     else

        !....       IF SPECIES OUT OF RANGE. PRINT MESSAGE AND IGNORE THE
        !....       KEYWORD.

        write(JOSTND,3961) KEYWRD, KARD(1), INT(ARRAY(1))
        3961 format(1X, A8, '   ERROR!!!!    INVALID SPECIES : ', A3,&
             &' (', I2, ');  KEYWORD IGNORED!!! ')
     end if
  end if

  3965 format(/1X,'********   ERROR IN RRHOSTS, DISEASE TYPE OUT OF ',&
       &'RANGE (0-2)', /, T13, 'SPECIES: ', A16,&
       &',        DISEASE TYPE: ',I5)
  3966 format(T13, 'DEFAULT TYPE USED:  ', I5, '    -    ', A12)

  !.... Output for RRHOSTS

  if (LKECHO)write(JOSTND,3971) KEYWRD
  if (LKECHO)write(JOSTND,3972)
  if (LKECHO)write(JOSTND,3973)

  do KSP = 1,MAXSP
     if (MAXRR .lt.3) then
        if (LKECHO)write(JOSTND,3975) RRJSP(IRTSPC(KSP)),&
             &DISTYP(IDITYP(IRTSPC(KSP)))
     else
        if (LKECHO)write(JOSTND,3975) RRJSP(IRTSPC(KSP)),&
             &DISTYP(MAXRR)
     end if
     3920 continue
  end do

  3971 format(/1X, A8, '   HOST TREE SPECIES')
  3972 format(/T13, 5X, 'SPECIES', 14X, 'DISEASE TYPE')
  3973 format(T13, 41('-'))
  3975 format(T13, A16, 9X, A12)

  goto 90

  40 continue

  !  ==========  OPTION NUMBER 40: INOCLIFE =============================

  !     THIS KEYWORD CHANGES THE LIFESPAN AND DECAY PATTERN OF INOCULUM
  !     IN THE FORM OF INFECTED ROOTS ON DEAD TREES AND STUMPS.
  !     CHANGES AFFECT ALL SIZE CLASSES

  IPOINT = 0

  if (LNOTBK(7)) IPOINT = INT(ARRAY(7))

  !.... CHECK TO MAKE SURE ROOT DISEASE TYPE IS ACCEPTABLE

  if (INT(ARRAY(7)) .gt. ITOTRR) then
     write(JOSTND,4010)
     write(JOSTND,4020)
     IPOINT = 0
  end if

  4010 format (/,' ********   ERROR IN DISEASE TYPE, INOCLIFE KEYWORD')
  4020 format (' ********   CHANGE WILL AFFECT ALL ACTIVE DISEASE ',&
       &'TYPES')

  !.... SET THE ROOT DISEASE TYPE TO BE MODIFIED

  IRMIN = MINRR
  IRMAX = MAXRR

  if (IPOINT .gt.0) then
     IRMIN = IPOINT
     IRMAX = IPOINT
  end if

  do IDI = IRMIN,IRMAX

     if (LMTRIC) then
        if (LNOTBK(1)) DECFN(IDI,1,1) = ARRAY(1)
        if (LNOTBK(2)) DECFN(IDI,2,1) = ARRAY(2) * MTOFT
        if (LNOTBK(3)) RSITFN(IDI,1) = ARRAY(3) * MTOFT / CMTOIN
        if (LNOTBK(4)) RSITFN(IDI,2) = ARRAY(4) * MTOFT
        if (LNOTBK(5)) YRSITF(IDI,1,1) = ARRAY(5) / CMTOIN
        if (LNOTBK(6)) YRSITF(IDI,2,1) = ARRAY(6) * MTOFT

        !....       ALSO ASSIGN THESE VARIABLES TO THE LARGER SIZE CLASS,
        !....       IF THEY WERE CHANGED

        if (LNOTBK(1)) DECFN(IDI,1,2) = ARRAY(1)
        if (LNOTBK(2)) DECFN(IDI,2,2) = ARRAY(2) * MTOFT
        if (LNOTBK(5)) YRSITF(IDI,1,2) = ARRAY(5) / CMTOIN
        if (LNOTBK(6)) YRSITF(IDI,2,2) = ARRAY(6) * MTOFT
     else
        if (LNOTBK(1)) DECFN(IDI,1,1) = ARRAY(1)
        if (LNOTBK(2)) DECFN(IDI,2,1) = ARRAY(2)
        if (LNOTBK(3)) RSITFN(IDI,1) = ARRAY(3)
        if (LNOTBK(4)) RSITFN(IDI,2) = ARRAY(4)
        if (LNOTBK(5)) YRSITF(IDI,1,1) = ARRAY(5)
        if (LNOTBK(6)) YRSITF(IDI,2,1) = ARRAY(6)

        !....       ALSO ASSIGN THESE VARIABLES TO THE LARGER SIZE CLASS,
        !....       IF THEY WERE CHANGED

        if (LNOTBK(1)) DECFN(IDI,1,2) = ARRAY(1)
        if (LNOTBK(2)) DECFN(IDI,2,2) = ARRAY(2)
        if (LNOTBK(5)) YRSITF(IDI,1,2) = ARRAY(5)
        if (LNOTBK(6)) YRSITF(IDI,2,2) = ARRAY(6)
     end if
     4030 continue
  end do

  !.... OUTPUT FOR INOCLIFE KEYWORD

  do IDI = IRMIN,IRMAX

     if (LMTRIC) then
        if (LKECHO)write(JOSTND,4050) KEYWRD, DISTYP(IDI), DECFN(IDI,&
             &DECFN(IDI,2,2)*FTTOM,&
             &RSITFN(IDI,1)*FTTOM/INTOCM,&
             &RSITFN(IDI,2)*FTTOM,&
             &YRSITF(IDI,1,2)/CMTOIN,&
             &YRSITF(IDI,2,2)*FTTOM
     else
        if (LKECHO)write(JOSTND,4050) KEYWRD, DISTYP(IDI), DECFN(IDI,&
             &DECFN(IDI,2,2), RSITFN(IDI,1),&
             &RSITFN(IDI,2), YRSITF(IDI,1,2),&
             &YRSITF(IDI,2,2)
     end if
     4040 continue
  end do

  4050 format (/1X, A8, '   FOR ', A12, '   DECAY FUNCTION SLOPE IS ',&
       &F7.5, ' AND INTERCEPT IS ', F7.5,&
       &/T13, 'FUNCTION DESCRIBING THE ROOT RADIUS AT WHICH TO ',&
       &' STOP DECAYING HAS SLOPE ', F7.5, ' AND INTERCEPT ',F7.5,&
       &/T13, 'FUNCTION DESCRIBING THE NUMBER OF YEARS TO NOT ',&
       &' DECAY HAS SLOPE ', F7.4, ' AND INTERCEPT ', F7.4)

  goto 90

  41 continue

  !  ==========  OPTION NUMBER 41: BBCLEAR ==============================

  !     THIS KEYWORD ENSURES THAT NO BARK BEETLES WILL BE TURNED ON
  !     IN THIS SIMULATION

  LBBON = .false.

  if (LKECHO)write(JOSTND,4101) KEYWRD
  4101 format (/1X,A8,'   NO BARK BEETLES WILL BE ACTIVE')

  goto 90

  42 continue

  !  ==========  OPTION NUMBER 42: RRTYPE ==============================

  !     THIS KEYWORD TELLS THE MODEL WHICH DISEASE TYPES ARE ACTIVE IN
  !     THE STAND.
  !     FOR NOW, ASSUME THAT WE CAN HAVE ONLY ONE TYPE (1,2=ANNOSUS,
  !     3=ARMILLARIA, 4=PHELLINUS)

  LRTYPE = .true.

  MINRR = 0
  MAXRR = 0

  do IDI = 1,ITOTRR
     if (LNOTBK(IDI)) then
        IPOINT = INT(ARRAY(IDI))
        if (IPOINT .lt. MINRR .or. MINRR .eq.0) MINRR = IPOINT
        if (IPOINT .gt. MAXRR) MAXRR = IPOINT
     end if
     4205 continue
  end do

  if (MINRR .le.0) MINRR = 1
  if (MINRR .gt. ITOTRR) MINRR = ITOTRR
  if (MAXRR .le.0) MAXRR = 1
  if (MAXRR .gt. ITOTRR) MAXRR = ITOTRR
  if (MAXRR .lt. MINRR) MAXRR = MINRR

  !.... THIS NEXT STATEMENT IS TO ENSURE THAT CAN ONLY HAVE ONE TYPE
  !.... IN THE STAND

  if (MAXRR .ne. MINRR .and. (MINRR .ne.1.and. MAXRR .ne.2)) then
     MAXRR = MINRR

     write(JOSTND,4209)
     4209 format (/T13, '***ERROR IN RRTYPE KEYWORD. ONLY ONE DISEASE ',&
          &'CAN BE SIMULATED IN THE STAND.')
  end if

  if (MINRR .eq. MAXRR) then
     if (LKECHO)write(JOSTND,4210) KEYWRD, DISTYP(MINRR)
     4210 format (/1X,A8,3X,A15,' WILL BE SIMULATED.')
  else if (MAXRR .eq. MINRR+1) then
     if (LKECHO)write(JOSTND,4215) KEYWRD, DISTYP(MINRR),DISTYP(MAXRR
     4215 format (/1X,A8,3X,A15,' AND ',A15,' WILL BE SIMULATED.')
  end if

  goto 90

  ! ================ ENTRY POINT RDKEY =============================

  !     SPECIAL ENTRY TO RETREIVE KEYWORDS

  entry RDKEY(KEY,PASKEY)
  PASKEY = TABLE(KEY)

  9000 return
end
