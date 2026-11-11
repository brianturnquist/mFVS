!===================!
!   wsbwe : bwein   !
!===================!
! WSBWE $Id$
subroutine BWEIN(LKECHO)
  implicit none

  !  **BWEIN                  DATE OF LAST REVISION:  03/23/17

  !     OPTION PROCESSOR FOR BUDWORM MODEL.

  !     PART OF THE MULTI-STAND WESTERN SPRUCE BUDWORM MODEL.
  !     CONVERT TO F77, N.L. CROOKSTON 12/88
  !     N.L.CROOKSTON, W.P. KEMP, P.W. THOMAS, UNIV OF IDAHO,
  !     FORESTRY SCIENCES LAB, MOSCOW, IDAHO. APRIL 10, 1984.

  !     MAJOR REVISION: SERIAL LINKAGE TO STAND PROGNOSIS MODEL.
  !     MAJOR REVISION: LINKAGE TO PARALLEL PROCESSING EXTENSION.
  !     major revision: delete links to BW pop.dyn. model
  !       K.A.Sheehan USDA-FS,R6-FID  July 1996
  !     MAJOR REVISION: CONVERT TO MENU-DRIVEN INPUT. KAS 9/96
  !     Revised to remove menu system -- k.sheehan 12/21/98

  !     CALLED FROM :

  !       INITRE - PROGNOSIS KEYWORD PROCESSOR.

  !     SUBROUTINES CALLED :

  !     BWEERR - PROCESS ERRORS.
  !     BWEFDK - FIND A KEYWORD IN A LOOKUP TABLE.
  !     BWEKRD - READ A KEYWORD.
  !     BWEMNU - HANDLE BUDWORM MENU OPTIONS
  !     BWEOB  - SCHEDULE THE NEXT BUDWORM OUTBREAK
  !     BWERSD - RESEED RANDOM NUMBER GENERATOR.
  !     BWEUCA
  !     MYOPEN
  !     OPNEW  - SCHEDULE NEW OPTION.

  !  PARAMETERS :

  !   IBWCHK - =1 IF BUDLITE IS CALLED, ELSE = 0 (LOCAL VARIABLE)
  !   IOBACT - IS MOST RECENT OUTBREAK STILL ACTIVE? 1=YES, 2=NO
  !   IREAD  - LOGICAL UNIT NUMBER FOR BUDWORM KEYWORD INPUT.
  !   IRECNT - NUMBER OF RECORDS READ FROM IREAD.
  !   JOSTND - LOGICAL UNIT NUMBER FOR KEYWORD REPORT.
  !   MOPT   - 1=USE MENU SYSTEM, 0=USE FVS KEYWORD FILE FOR BW OPTIONS,
  !            2=USE BUDWORM KEYWORD FILE FOR BW OPTIONS
  !   JOWE   - FILE NUMBER FOR WEATHER STATION DATA [BWEBOX]
  !   WSTEA  - NAME OF FILE CONTAINING WEATHER STATION DATA [LOCAL]
  !   BWEATH(10,4) - WEATHER PARAMETER ARRAY [BWECM2]
  !   MGMIDB - MANAGEMENT ID TAG FOR BW MODEL [BWEBOX]
  !   IYROBL - YEAR THAT THE LAST (MOST RECENT) OUTBREAK STARTED [BWECM2]
  !   LBUDL  - FLAG SET TO TRUE IF A BW OUTBREAK IS (WILL BE) ACTIVE [BWECM2]
  !   IOBLOC - LOCATION INDEX FOR OUTBREAK STATISTICS [BWECM2]
  !   IWOPT  - WEATHER OPTION (1= INCORP. VARIATION, 2= USE MEANS ONLY) [BWECM2]
  !   NEMULT(4,3) - MULTIPLIERS FOR NATURAL ENEMY EFFECTS [BWEBOX]
  !   LP1-7  - BW OUTPUT FLAGS (TRUE=PRINT, FALSE=NO PRINT) [BWEBOX]

  !  Revision History:
  !    02-MAY-00 Lance David (FHTET)
  !      .Added debug handling.
  !      .The variable LSTRT is initialized to false to suppress the
  !       printing of the heading within the body of the FVS keyword
  !       report.
  !    04-MAY-00 Lance David (FHTET)
  !      .Replaced variable IFILE with direct usage of FVS variable IREAD
  !       for logical unit number of keyword file.
  !    12-MAY-00 Lance David (FHTET)
  !      .Changed variable ISIZE to TABSZ.
  !    16-MAY-00 Lance David (FHTET)
  !      .Removed TEMPS1(1) as character storage for IYROBL year of last
  !       outbreak and added ILOBYR to store initial value.
  !    08-JUN-00 Lance David (FHTET)
  !      .Changed to BWOUTPUT keyword section to only process fields
  !       that are actually used (fields 1-7, variables LP1-LP7)
  !    16-JUN-00 Lance David (FHTET)
  !      .Reading of the weather station data file (STATIONS.DAT) moved
  !       from BWEINT to BWEIN so that the process will only occur if
  !       budworm keywords are present. Subroutine BWEINT is called by
  !       FVS subroutine INITRE regardless.
  !    19-JUN-00 Lance David (FHTET)
  !      .Added information to what is written when keyword is processed
  !       for keywords: recovery, outbrloc, bwoutput, obsched.
  !      .Print control variables LP1-LP7 changed from integer to logical.
  !    02-AUG-00 Nick Crookston (RMRS)
  !       Added a missing comma to statement 2540. Modified MYOPEN calls
  !       for the weather data files.
  !    02-OCT-00 Lance David (FHTET)
  !      .Reading of the weather station data file (STATIONS.DAT) moved
  !       to the location where the WEAHTER keyword is processed so that
  !       this file is only processed when the specific option is requested
  !       that utilizes the WSLOOK array, use model's internal weather
  !       paramters (field 4 = 1).
  !    01-DEC-00 Lance David (FHTET)
  !      .Variable IDEFPR associated with the generation of DFESUM.TAB
  !       was not set along with LP3. Kathy originally had two options
  !       on the generation of this table, but now only option 1 is
  !       active. The section of code for this table was originally in
  !       the menu subroutine. I don't think it was properly placed when
  !       put in this subroutine. It still does not function properly,
  !       only a table of zeros is generated. This is not a critical
  !       problem, so will have to be addressed at a later date.
  !    05-DEC-00 Lance David (FHTET)
  !      .Removed a couple do loops that data arrays STAGE and TREESP.
  !      .In BWSPRAY keyword processing:
  !        - variable KSPRAY changed to ISPRAY
  !        - % defoliation trigger variable TRIGGR had not been set.
  !    16-AUG-01 Lance R. David (FHTET)
  !       Keyword RANNSEED was lost in the smoke of reworking the model
  !       and is now back in operation.
  !       Minor modification in the reporting of Weather keyword parameters.
  !    21-NOV-2002 Lance R. David (FHTET)
  !       Changed variable ISTATE to ISTNUM.
  !    30-OCT-2003 Lance R. David (FHTET)
  !       Replaced real literal value (1.) with real "ARRAY" in GenDefol
  !       keyword processing call to OPNEW.
  !    05-MAY-2005 Lance R. David (FHTET)
  !       Removed carriage control column from format statements used
  !       to write to JOSTND do to change in file type to list/text.
  !    12-DEC-2005 Lance R. David (FHTET)
  !       Put carriage control column back into format statements. Seems
  !       this FVS update has been tabled or canceled.
  !    28-DEC-2005 Lance R. David (FHTET)
  !       Increased weather file name to 40 characters.
  !       Modified the WEATHER keyword reporting for clarity and set
  !       the file name when users select to use weather parameter files
  !       provided with the model to be located in 'c:/fvsbin/' directory.
  !    22-SEP-2006 Lance R. DAvid (FHTET)
  !       Change random number initialization and seed handling for
  !       damage model, weather model and outbreak scheduling.
  !    29-MAY-2009 Lance R. David (FMSC)
  !       Added character variable TMPNAM. Prepended keyword file name
  !       to output file names at BWOUTPUT keyword processing.
  !    14-JUL-2010 Lance R. David (FMSC)
  !       Added IMPLICIT NONE and declared variables as needed.
  !    04-SEP-2013 Lance R. David (FMSC)
  !       Added RAWS weather year range as supplemental record on
  !       WEATHER keyword.
  !    23-MAR-2013 Lance R. David (FMSC)
  !       Modified DEFOL and SETPRBIO to process numeric and alpha specie codes.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'BWESTD.f90'
  include 'BWECOM.f90'
  include 'BWECM2.f90'
  include 'BWEBOX.f90'

  !     INTERNAL STORAGE

  ! Variables
  character(len=40) :: TEMPNM
  character(len=40) :: TMPNAM
  character(len=4) :: CFOUR
  character(len=3) :: TREESP(5)
  character(len=8) :: KEYWRD
  character(len=7) :: CSTAT
  character(len=10) :: KARD(12)
  character(len=6) :: STAGE(3)
  character(len=20) :: WSTEA
  character(len=25) :: CWTYP
  character(len=40) :: STR40
  character(len=80) :: RECORD

  integer :: I,IACTK,IBWCHK,IDT,IEND,IM,IOB,IP,IS,ISKIP,ISTAGE,ISTART,ISTLNB,IUNIT,IZNUL,J,K,KODE,N,&
       &NEN,NSKIP,NUMBR

  real    :: ARRAY(12),RECNT,X

  logical :: DEBUG,LKECHO
  logical :: LNOTBK(12),LSTRT,LADDS,LOUTP

  !LRD  EQUIVALENCE (KARD,RECORD)

  data STAGE/'FOURTH','FIFTH','SIXTH'/
  data TREESP/' WF',' DF',' GF','SAF',' ES'/

  !.... Initialize local variables.

  DEBUG = .false.
  LSTRT = .false.
  LADDS = .false.
  LOUTP = .false.

  TEMPNM = ' '
  CFOUR = ' '
  KEYWRD = ' '
  CSTAT = ' '
  WSTEA = ' '
  STR40 = ' '
  RECORD = ' '
  do I = 1,12
     KARD(I) = ' '
  end do
  do I = 1,12
     LNOTBK(I) = .false.
     ARRAY(I) = 0.0
  end do
  NUMBR = 0

  !.... Check for DEBUG

  call DBCHK(DEBUG,'BWEIN',5,ICYC)
  if (DEBUG) write(JOSTND,*) 'ENTER BWEIN: ICYC = ',ICYC

  !     OPEN BUDWORM STANDARD OUTPUT DEVICE AND INITIALIZE CONTROL VARIABLES.

  IBWCHK = 0
  STR40 = ' '
  call MYOPEN (JOWSBW,STR40,4,133,1,1,1,0,KODE)
  LOUTP = .false.
  LSTRT = .false.
  LADDS = .false.
  10 continue
  call BWEKRD (IREAD,JOSTND,KEYWRD,LNOTBK,ARRAY,&
       &IRECNT,KODE,KARD)

  !  RETURN KODES 0=NO ERROR,1=COLUMN 1 BLANK OR ANOTHER ERROR,2=EOF
  !               LESS THAN ZERO...USE OF PARMS STATEMENT IS PRESENT.

  if (KODE .eq.0) goto 30
  if (KODE .eq.2) call BWEERR(IREAD,IRECNT,JOSTND,ICCODE,&
       &.false.,2)
  call BWEERR (IREAD,IRECNT,JOSTND,ICCODE,.true.,6)
  goto 10
  30 continue

  if (DEBUG) write(JOSTND,*) 'IN BWEIN: >>>BEFORE CALL BWEFDK'
  if (DEBUG) write(JOSTND,*) 'IN BWEIN: NUMBR =', NUMBR
  if (DEBUG) write(JOSTND,*) 'IN BWEIN: KEYWRD =', KEYWRD
  if (DEBUG) write(JOSTND,*) 'IN BWEIN: TABLE  =', TABLE
  if (DEBUG) write(JOSTND,*) 'IN BWEIN: TABSZ  =', TABSZ
  if (DEBUG) write(JOSTND,*) 'IN BWEIN: KODE   =', KODE
  call BWEFDK (NUMBR,KEYWRD,TABLE,TABSZ,KODE,JOSTND)
  if (DEBUG) write(JOSTND,*) 'IN BWEIN: >>>>AFTER CALL BWEFDK'
  if (DEBUG) write(JOSTND,*) 'IN BWEIN: NUMBR =', NUMBR
  if (DEBUG) write(JOSTND,*) 'IN BWEIN: KEYWRD =', KEYWRD
  if (DEBUG) write(JOSTND,*) 'IN BWEIN: TABLE  =', TABLE
  if (DEBUG) write(JOSTND,*) 'IN BWEIN: TABSZ  =', TABSZ
  if (DEBUG) write(JOSTND,*) 'IN BWEIN: KODE   =', KODE

  !     RETURN KODES 0=NO ERROR,1=KEYWORD NOT FOUND.

  if (KODE .eq.0) then
     goto 95
  else
     call BWEERR (IREAD,IRECNT,JOSTND,ICCODE,.true.,1)
     goto 10
  end if

  !     SPECIAL END-OF-FILE TARGET (USED IF YOU READ PARAMETER CARDS).

  80 continue
  call BWEERR (IREAD,IRECNT,JOSTND,ICCODE,.false.,2)

  !     TARGET USED TO WRITE ILLEGAL PARAMETERS STATEMENT.

  90 continue
  call BWEERR (IREAD,IRECNT,JOSTND,ICCODE,.true.,4)
  goto 10
  95 continue

  !     WRITE KEYWORD TABLE HEADING.

  if (.not.LSTRT) goto 99
  if (LKECHO) write(JOSTND,98)
  98 format (/,130('-'),//,T49,'OPTIONS SELECTED BY INPUT',//,&
       &130('-'),/,'KEYWORD    PARAMETERS:',/,&
       &' --------   ',119('-'))
  LSTRT = .false.
  99 continue

  !     PROCESS OPTIONS

  if (DEBUG) write(JOSTND,*) 'IN BWEIN: KEYWRD= ',KEYWRD
  if (DEBUG) write(JOSTND,*) 'IN BWEIN: ARRAY = ',ARRAY
  if (DEBUG) write(JOSTND,*) 'IN BWEIN: LNOTBK= ',LNOTBK

  goto (200,300,400,500,600,700,800,900,1000&
       &,1100,1200,1300,1400,1500,1600,1700,1800,1900&
       &,2000,2100,2200,2300,2400,2500,2600), NUMBR

  !--------------------- OPTION NUMBER 2 -- END

  !     THE END KEYWORD SIGNALS THE END OF THE MAIN BUDWORM KEYWORD SET.

  200 continue
  if (LKECHO) write(JOSTND,220) KEYWRD
  220 format (/,A8,'   END OF BUDWORM OPTIONS.')

  ! SKIP THE FOLLOWING SECTION IF MANUAL DEFOLIATION IS SUPPLIED

  if (LDEFOL) goto 9000 ! exit subroutine

  !  READ WEATHER DATA NOW IF BUDLITE HAS BEEN SELECTED
  !  IWSRC: 1 = Weather parameters provided with model
  !         2 = Weather parameter file provided by user
  !         3 = RAWS daily weather data to be used

  if (IBWCHK .eq.1) then
     if (IWSRC .eq.1) then
        call MYOPEN (JOWE,TEMPNM,3,133,1,1,1,0,KODE)
        if (ITEMP(5).ne.1) then
           NSKIP = (ITEMP(5)-1)*11
           do ISKIP = 1,NSKIP
              read (JOWE,240)
              240 format (1X)
              250 continue
           end do
        end if
     else if (IWSRC .eq.2) then
        call MYOPEN (JOWE,WFNAME,3,133,1,1,1,0,KODE)
     else
        call BWERAWS
     end if
     if (IWSRC .eq.1.or. IWSRC .eq.2) then
        read (JOWE,260) WSTEA
        260 format (A20)
        do I = 1,10
           read (JOWE,265) (BWEATH(I,J),J = 1,4)
           265 format (4F8.3)
           270 continue
        end do

        ! This is an old section of code from before weather source option
        ! of using RAWS daily weather was added. At this point in the code,
        ! the RAWS data has already been processed. BUT we are still missing
        ! means and Std Dev values for hot Fall and precipitation during key
        ! periods for small and large larvae, pupae and L2 emergence.
        ! Lance David 07/20/2011

        !         ELSE
        !            READ (JOWE,275) WHOTM,WHOTSD,(RAINM(I),RAINS(I),I=1,3),
        !     >          RAINDM,RAINDS
        !  275       FORMAT (/10F7.3)
        !            READ (JOWE,280) IYEAR
        !  280       FORMAT (I4)
        !            IF (IYEAR.EQ.IY(1)) THEN
        !               REWIND (JOWE)
        !            ELSE
        !               ICOUNT=0
        !  285          READ (JOWE,280,END=295,IOSTAT=IOS) IYEAR
        !               ICOUNT=ICOUNT+1
        !               IF (IYEAR.NE.IY(ICYC)) GOTO 285
        !               REWIND (JOWE)
        !               NCOUNT=ICOUNT-1
        !               DO 290 I=1,NCOUNT
        !                  READ (JOWE,280,IOSTAT=IOS) IYEAR
        !  290          CONTINUE
        !               ICOUNT=0
        !  295          IF (ICOUNT.NE.0) REWIND (JOWE)
        !            ENDIF

     end if
  end if

  !  SET UP THE TIMING OF THE NEXT OUTBREAK

  call BWEOB
  KRECVR = 0

  !  INIT. THE SPECIAL EVENTS TABLE & COUNTER, PRINT SUMMARY INFO.

  if (LP4.and. IBWCHK.eq.1) call BWEP4(1)

  goto 9000 ! end of keywords and initialization, exit subroutine

  !--------------------- OPTION NUMBER 3 -- OPEN

  300 continue
  read (IREAD,'(A80)',end = 80) RECORD
  IRECNT = IRECNT+1
  if (ARRAY(1).gt.0.0) goto 310
  call BWEERR(IREAD,IRECNT,JOSTND,ICCODE,.true.,4)
  goto 10
  310 continue
  IUNIT = IFIX(ARRAY(1))
  if (ARRAY(2).eq.0) then
     IZNUL = 0
     CFOUR = 'ZERO'
  else
     IZNUL = 1
     CFOUR = 'NULL'
  end if
  IS = INT(ARRAY(3))+1
  if (IS.ge.0.or.IS.le.3) goto 320
  call BWEERR(IREAD,IRECNT,JOSTND,ICCODE,.true.,4)
  goto 10
  320 continue
  if (IS.eq.1) CSTAT = 'UNKNOWN'
  if (IS.eq.2) CSTAT = 'NEW'
  if (IS.eq.3) CSTAT = 'OLD'
  if (IS.eq.4) then
     IS = 5
     CSTAT = 'FRESH'
  end if
  IM = 80
  if (ARRAY(4).gt.0) IM = INT(ARRAY(4))

  !     CALL MYOPEN TO OPEN THE FILE.  MACHINE SPECIFIC CODE IS IN MYOPEN

  call MYOPEN (IUNIT,RECORD(1:40),IS,IM,IZNUL,1,1,0,KODE)

  if (LKECHO) write(JOSTND,340) KEYWRD,IUNIT,CFOUR,CSTAT,IM,RECORD
  340 format (/,A8,'   DATA SET REFERENCE NUMBER = ',I5,'; BLANK=',A4,&
       &'; STATUS=',A7,/,T12,&
       &'MAXIMUM RECORD LENGTH (IGNORED ON SOME MACHINES) = ',I4,&
       &/,T12,'DATA SET NAME = ',A)
  if (KODE.eq.1) write(JOSTND,350)
  350 format (/,T12,'********   OPEN FAILED   ********')

  goto 10

  !--------------------- OPTION NUMBER 4 -- CLOSE

  400 continue
  if (ARRAY(1).le.0.0) then
     call BWEERR(IREAD,IRECNT,JOSTND,ICCODE,.true.,4)
  else
     I = INT(ARRAY(1))
     if (LKECHO) write(JOSTND,420) KEYWRD,I
     420 format (/,A8,'   DATA SET REFERENCE NUMBER = ',I5)
     close (UNIT = I)
  end if
  goto 10

  !--------------------- OPTION NUMBER 5 -- MGMTID

  500 continue
  read (IREAD,510,end = 80) MGMIDB
  510 format (A4)
  IRECNT = IRECNT+1
  if (LKECHO)write(JOSTND,520) KEYWRD,MGMIDB
  520 format (/,A8,'   MANAGEMENT ID= ',A4)
  goto 10

  !--------------------- OPTION NUMBER 6 -- RANNSEED

  !     THE RANNSEED KEYWORD RESEEDS THE BUDWORM DAMAGE MODEL RANDOM
  !     NUMBER GENERATOR.
  !     IF FIELD 1 IS ZERO OR BLANK, THE CURRENT SEED IS RETRIEVED AND
  !     REPORTED.

  600 continue
  if (.not. LNOTBK(1) .or. ARRAY(1).eq.0.0) then
     call BWERGT (DSEEDD)
     DSEEDR = real(DSEEDD)
     if (LKECHO)write(JOSTND,610) KEYWRD, DSEEDR
  else if (LNOTBK(1) .and. ARRAY(1).le.10000.0) then
     if (LKECHO)write(JOSTND,612) KEYWRD, ARRAY(1)
  else
     DSEEDR = ARRAY(1)
     !         DSEEDD = DBLE(DSEEDR)
     call BWERSD(LNOTBK(1), DSEEDR)
     call BWERGT(DSEEDD)
     if (LKECHO)write(JOSTND,614) KEYWRD, DSEEDR
  end if

  610 format (/,A8,'   DAMAGE MODEL RANDOM NUMBER SEED IS:',&
       &F10.0)
  612 format (/,A8,'   ********   ERROR ********',/,11X,&
       &'RANDOM SEED VALUE MUST BE A 5-DIGIT ODD NUMBER. ',&
       &'VALUE PROVIDED WAS:',F10.0,/,11X,&
       &'RESEEDING OF RANDOM NUMBER GENERATOR DID NOT OCCUR.')
  614 format (/,A8,'   DAMAGE MODEL RANDOM NUMBER SEED RESET TO:',&
       &F10.0)
  goto 10

  !--------------------- OPTION NUMBER 7 -- STARTYR

  !     THE STARTYR KEYWORD SELECTS THE STARTING YEAR OF A
  !     BUDWORM MODEL RUN, IN STAND ALONE MODE.

  700 continue
  IBWYR1 = 1
  if (LNOTBK(1)) IBWYR1 = IFIX(ARRAY(1))
  if (LKECHO) write(JOSTND,710) KEYWRD,IBWYR1
  710 format (/,A8,'   FIRST YEAR OF PROJECTION IS',I5,'.')
  goto 10

  !--------------------- OPTION NUMBER 8 -- COMMENT

  800 continue
  if (LKECHO) write(JOSTND,'(/1X,A8)') KEYWRD
  810 continue
  read (IREAD,'(A80)',end = 80) RECORD
  IRECNT = IRECNT+1
  CFOUR = RECORD (1:4)
  do I = 1,4
     call BWEUCA (CFOUR(I:I))
     820 continue
  end do
  if (CFOUR.eq.'END ') then
     if (LKECHO)write(JOSTND,'(/'' END'')')
     goto 10
  else
     if (LKECHO)write(JOSTND,'(T13,A80)') RECORD
     goto 810
  end if
  goto 10

  !--------------------- OPTION NUMBER 9 -- DEFOL

  !     THE DEFOL KEYWORD SCHEDULES MANUAL DEFOLIATION IN THE LINKED
  !     VERSION OF THE BUDWORM MODEL.
  !     SPECIES IS DECODED AND SET TO APPROPIATE FVS SPECIES NUMBERIC CODE
  !     BLANK DEFOLIATION FIELDS ARE SET TO ZERO.
  900 continue
  LDEFOL = .true.
  IACTK = 2151
  X = 0.
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  call SPDECD (2,IS,JSP,JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (IS .eq. -999.or. IS .eq.0) then
     ARRAY(2) = 0.0
  else
     if (IBWSPM(IS) .eq.7) then
        !         NON-HOST SPECIES SPECIFIED
        if (LKECHO) write(JOSTND,905) KEYWRD,IDT,JSP(IS)
        905 format (/,A8,'   DATE/CYCLE=',I5,'; SPECIES= ',A4,&
             &'; NON-HOST - NOT SCHEDULED.')
        goto 10
     else
        ARRAY(2) = IS
     end if
  end if

  if (ARRAY(3).gt.15.) ARRAY(3) = 0.0
  do I = 4,7
     if (.not.LNOTBK(I)) ARRAY(I) = X
     910 continue
  end do
  call OPNEW (KODE,IDT,IACTK,6,ARRAY(2))
  if (KODE.gt.0) goto 10
  if (LKECHO) write(JOSTND,920)&
       &KEYWRD,IDT,KARD(2)(1:4),(ARRAY(I),I = 3,7)
  920 format (/,A8,'   DATE/CYCLE=',I5,'; SPECIES= ',A4,&
       &'; CROWN CODE=',F3.0,'; NEW=',F7.2,'; 1-YR=',F7.2,&
       &'; 2-YR=',F7.2,'; REMAINING=',F7.2)
  goto 10

  !--------------------- OPTION NUMBER 10 -- SETPRBIO

  !     THE SETPRBIO KEYWORD ALLOWS THE USER TO SCHEDULE RECOMPUTATION
  !     OF THE PROPORTION OF RETAINED BIOMASS ARRAYS IN THE LINKED VERSION
  !     OF THE BUDWORM MODEL.

  1000 continue
  IACTK = 2153
  X = 1.0
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  call SPDECD (2,IS,JSP,JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (IS .eq. -999.or. IS .eq.0) then
     ARRAY(2) = 0.0
  else
     if (IBWSPM(IS) .eq.7) then
        !         NON-HOST SPECIES SPECIFIED
        if (LKECHO) write(JOSTND,1005) KEYWRD,IDT,JSP(IS)
        1005 format (/,A8,'   DATE/CYCLE=',I5,'; SPECIES= ',A4,&
             &'; NON-HOST - NOT SCHEDULED.')
        goto 10
     else
        ARRAY(2) = IS
     end if
  end if
  if (ARRAY(3).gt.15.) ARRAY(3) = 0.0
  do I = 4,6
     if (.not.LNOTBK(I)) ARRAY(I) = X
     1010 continue
  end do
  call OPNEW (KODE,IDT,IACTK,5,ARRAY(2))
  if (KODE.gt.0) goto 10
  if (LKECHO) write(JOSTND,1020)&
       &KEYWRD,IDT,KARD(2)(1:4),(ARRAY(I),I = 3,6)
  1020 format (/,A8,'   DATE/CYCLE=',I5,'; SPECIES= ',A4,&
       &'; CROWN CODE=',F3.0,'; 1-YR=',F7.2,&
       &'; 2-YR=',F7.2,'; REMAINING=',F7.2)
  goto 10

  !--------------------- OPTION NUMBER 11 -- DAMAGE

  !     THE DAMAGE KEYWORD ACTIVATES DAMAGE OUTPUT.

  1100 continue
  LBWDAM = .true.
  if (LKECHO)write(JOSTND,1110) KEYWRD
  1110 format (/,A8,'   PRINT BUDWORM-CAUSED DAMAGE',&
       &' OUTPUT.')
  goto 10

  !--------------------- OPTION NUMBER 12 -- NODAMAGE

  !     THE NODAMAGE KEYWORD DEACTIVATES DAMAGE OUTPUT.

  1200 continue
  LBWDAM = .false.
  if (LKECHO)write(JOSTND,1210) KEYWRD
  1210 format (/,A8,'   DO NOT PRINT BUDWORM-CAUSED DAMAGE',&
       &' OUTPUT.')
  goto 10

  !--------------------- OPTION NUMBER 13 -- PERDAM

  1300 continue
  LBWPDM = .true.
  if (LKECHO)write(JOSTND,1310) KEYWRD
  1310 format (/,A8,'   PRINT BUDWORM-CAUSED PERIODIC DAMAGE',&
       &' OUTPUT.')
  goto 10

  !--------------------- OPTION NUMBER 14 -- NOPERDAM

  1400 continue
  LBWPDM = .false.
  if (LKECHO)write(JOSTND,1410) KEYWRD
  1410 format (/,A8,'   DO NOT PRINT BUDWORM-CAUSED PERIODIC DAMAGE',&
       &' OUTPUT.')
  goto 10

  ! -----------------------OPTION NUMBER 15 -- GENDEFOL

  1500 continue
  IACTK = 2150
  IBWCHK = 1
  IOBACT = 0
  if (LNOTBK(1)) IOBACT = IFIX(ARRAY(1))
  if (IOBACT.eq.1) then
     LTEMP1(1) = .true.
  else
     LTEMP1(1) = .false.
  end if
  IYROBL = ILOBYR
  if (LNOTBK(2)) then
     IYROBL = IFIX(ARRAY(2))
     ILOBYR = IYROBL
  end if
  LBUDL = .true.
  call OPNEW(KODE,IOBACT,IACTK,0,ARRAY)
  if (LKECHO)write(JOSTND,1550) KEYWRD,IOBACT,IYROBL
  1550 format (/,A8,'   CURRENT OUTBREAK (1=Y)=',I2,&
       &'; LAST OUTBREAK STARTED',&
       &' IN ',I5, '.')
  goto 10

  ! -----------------------OPTION NUMBER 16 -- OUTBRLOC

  !   SET THE GEOGRAPHIC LOCATION INDEX FOR OUTBREAKS
  !     (1=SOUTHWEST, 2=NORTHWEST, 3=MONTANA)

  1600 continue
  IOBLOC = 2
  OBSEER = real(OBSEED)
  if (LNOTBK(1)) IOBLOC = IFIX(ARRAY(1))
  ITEMP(2) = IOBLOC
  if (LNOTBK(2)) then
     OBSEER = real(ARRAY(2))
     call BWERSD(LNOTBK(2), OBSEER)
     call BWERGT(OBSEED)
  end if
  if (LKECHO)write(JOSTND,1620) KEYWRD,IOBLOC,TEMPS2(IOBLOC),OBSEER
  1620 format (/,A8,3X,'GEOGRAPHIC LOCATION FOR OUTBREAK.',/,11X,&
       &'LOCATION OPTIONS ARE: 1=SOUTHWEST, 2=NORTHWEST, 3=MONTANA',&
       &/,11X,'OUTBREAK LOCATION SPECIFIED=',I3,' (',A20,')',&
       &/,11X,'RANDOM NUMBER SEED FOR OUTBREAKS=',F8.1)
  goto 10

  ! -----------------------OPTION NUMBER 17 -- RECOVERY

  1700 continue

  if (LKECHO)write(JOSTND,1720) KEYWRD
  1720 format (/,A8,3X,'THIS OPTION IS CURRENTLY UNDER DEVELOPMENT ',&
       &'AND HAS NO AFFECT.')

  !     IACTK= 2154
  !     CALL OPNEW(KODE,0,IACTK,0,ARRAY)
  goto 10

  ! -----------------------OPTION NUMBER 18 -- WEATHER

  1800 continue
  IWOPT = 1
  JOWE = 40
  WSEEDR = real(WSEED)
  if (LNOTBK(1)) IWOPT = IFIX(ARRAY(1))
  if (LNOTBK(2)) JOWE = IFIX(ARRAY(2))
  if (LNOTBK(4)) IWSRC = IFIX(ARRAY(4))
  if (LNOTBK(5)) ITEMP(5) = IFIX(ARRAY(5))
  if (LNOTBK(6)) ISTNUM = IFIX(ARRAY(6))
  if (LNOTBK(3)) then
     WSEEDR = real(ARRAY(3))
     call BWERSD(LNOTBK(3), WSEEDR)
     call BWERGT(WSEED)
  end if
  ITEMP(4) = ISTNUM
  IWYR = IY(1)
  if (DEBUG) write(JOSTND,*) 'IN BWEIN: WSEED=', WSEED

  if (IWSRC .eq.1) then

     !        User has chosen weather data source as that provided within
     !        the model; therefor, we need to process the weather stations
     !        file containing state and station names.

     !        ***** This code is not active in production versions *****
     !        ***** of the executable.                             *****
     !        ***** WSLOOK array is loaded using data statements   *****
     !        ***** in bweblk the block data program unit.         *****

     !        READ WEATHER STATION NAMES
     !        This process expects file stations.dat to contain 10 states
     !        with 100 stations for each state.

     !.       IF (DEBUG) IF(LKECHO)WRITE(JOSTND,*) 'IN BWEIN: STNAME=',STNAME
     !.       CLOSE (JOWE)
     !.       CALL MYOPEN (JOWE,STNAME,3,133,1,1,1,0,KODE)
     !.
     !.       DO JSTATE=1,10
     !.         READ (JOWE,4) NUMSTN(JSTATE)
     !.  4      FORMAT (11X,I3)
     !.         READ (JOWE,5) (WSLOOK(JSTN,JSTATE),JSTN=1,100)
     !.  5      FORMAT (5A16,19(/5A16))
     !.       END DO
     !        Write weather station lookup table to help create block data
     !        statements in bweblk?? files. List of names with propper
     !        formatting is written to the main FVS output file.
     !        If it is desired to perform processing of the stations.dat
     !        file at runtime, do activate the following 3 lines.

     !.       IF(LKECHO)WRITE(JOSTND,6) WSLOOK
     !.  6    FORMAT ((33(/,'     >"',A16,'","',A16,'","',A16,'",'),
     !.   >               /,'     >"',A16,'",'))

     !        ***** End of weather station name file processing.   *****

     !        Skip the 2 supplemental records for the weather data file name
     !        and year range because they are not utilized with this option.

     read(IREAD,1830)
     1830 format (1X,/,1X)

  else if (IWSRC .eq.2.or. IWSRC .eq.3) then
     read (IREAD,1850) WFNAME
     1850 format (A40)
  end if

  !     If using RAWS data, a year range may be specified on the
  !     following record. Four-digit starting and ending years

  if (IWSRC .eq.3) then
     read (IREAD,*) IYRNG(1), IYRNG(2)
  end if

  if (IWOPT .eq.1) then
     CWTYP = 'INCLUDES RANDOM VARIATION'
  else
     CWTYP = 'BASED ON AVERAGE VALUES'
  end if

  if (IWSRC .eq.1) then
     TEMPNM = 'c:/fvsbin/'//STATES(ISTNUM,2)
     if (LKECHO) write (JOSTND,1819) KEYWRD,CWTYP,ITEMP(5),&
          &WSLOOK(ITEMP(5),ITEMP(4)),STATES(ISTNUM,1),TEMPNM,WSEEDR
     1819 format (/,A8,'   WEATHER ',A,';  WEATHER',&
          &' STATION = (',I3,') ',A,' IN ',A,&
          &/,11X,'WEATHER DATA FILE IS ',A,&
          &/,11X,'RANDOM NUMBER SEED FOR WEATHER= ',F8.1)

  else if (IWSRC .eq.2) then
     if (LKECHO) write(JOSTND,1820) KEYWRD,CWTYP,JOWE,WFNAME,WSEEDR
     1820 format (/,A8,'   WEATHER ',A,';  WEATHER DATA',&
          &' FILE (NO.) NAME= (',I3') ',A,/,11X,'RANDOM NUMBER',&
          &' SEED FOR WEATHER= ',F8.1)

  else if (IWSRC .eq.3) then
     if (LKECHO) write(JOSTND,1822) KEYWRD,JOWE,WFNAME
     1822 format (/,A8,'   WEATHER IS ACTUAL RAWS DATA;  WEATHER DATA',&
          &' FILE (NO.), NAME= (',I3') ',A)
     if (IYRNG(1) .ge.1900.and. IYRNG(2) .ge.1900) then
        if (LKECHO) write(JOSTND,1823) IYRNG(1), IYRNG(2)
        1823 format (/,11X,'RANGE OF WEATHER YEARS SPECIFIED: ',&
             &I4,' TO ',I4)
     else
        if (LKECHO) write(JOSTND,1824)
        1824 format (/,11X,'ALL WEATHER YEARS WILL BE USED.')
     end if
  end if
  goto 10

  ! -----------------------OPTION NUMBER 19 -- NEMULT

  1900 continue
  NEN = -1
  if (LNOTBK(1)) then
     NEN = IFIX(ARRAY(1))
     if (LNOTBK(2)) NEMULT(NEN,1) = ARRAY(2)
     if (LNOTBK(3)) NEMULT(NEN,2) = ARRAY(3)
     if (LNOTBK(4)) NEMULT(NEN,3) = ARRAY(4)
  end if
  if (NEN.lt.1.or. NEN.gt.4) then
     write (JOSTND,1910) KEYWRD,NEN
     1910 format (/,A8,'   NATURAL ENEMY KEYWORD CALLED -- ERROR! ',&
          &'NEN = ',I3)
  else
     if (LKECHO) write(JOSTND,1920) KEYWRD,NEN,&
          &(NEMULT(NEN,ISTAGE),ISTAGE = 1,3)
     1920 format (/,A8,'   NATURAL ENEMY KEYWORD; NEN = ',I3,&
          &'  MULTIPLIERS= ',3F7.2)
  end if
  goto 10

  ! -----------------------OPTION NUMBER 20 -- BWOUTPUT

  !     The output options are and associated file names are:
  !     FIELD 1) WITHIN.TAB, UNIT 41
  !     FIELD 2) CANOPY.TAB, UNIT 42
  !     FIELD 3) DEFSUM.TAB, UNIT 43
  !     FIELD 4) PARAMS.TAB, UNIT 44
  !     FIELD 5) ANNUAL.TAB, UNIT 45
  !     FIELD 6) DEFOL.TAB,  UNIT 46
  !     FIELD 7) DYNAMICS.TAB, UNIT 47
  !           8) EFFECTS.TAB, UNIT 48   -- not developed ?

  2000 continue
  if (LNOTBK(1) .and. ARRAY(1) .eq.1.0) LP1 = .true.
  if (LNOTBK(2) .and. ARRAY(2) .eq.1.0) LP2 = .true.
  if (LNOTBK(3) .and. ARRAY(3) .eq.1.0) LP3 = .true.
  if (LNOTBK(4) .and. ARRAY(4) .eq.1.0) LP4 = .true.
  if (LNOTBK(5) .and. ARRAY(5) .eq.1.0) LP5 = .true.
  if (LNOTBK(6) .and. ARRAY(6) .eq.1.0) LP6 = .true.
  if (LNOTBK(7) .and. ARRAY(7) .eq.1.0) LP7 = .true.
  if (LKECHO)write(JOSTND,2005) KEYWRD,(ARRAY(I),I = 1,7)
  2005 format (/,A8,3X,'BUDWORM OUTPUT OPTIONS:',7F4.0,/,36X,&
       &'(1) (2) (3) (4) (5) (6) (7)',/,11X,&
       &'REPORTS, SUMMARIES AND TABLES TO BE PRODUCED ARE:')

  if (LP1) then
     TMPNAM = KWDFIL(1:ISTLNB(KWDFIL))//'_'//OUTNAM(1)
     if (LKECHO) write(JOSTND,2011) TMPNAM
     2011 format (11X,'(1) ',A30,&
          &'-- WITHIN-YEAR POPULATION DYNAMICS SUMMARY')
     call MYOPEN(JOBWP1,TMPNAM,5,133,1,1,1,0,KODE)
  end if
  if (LP2) then
     TMPNAM = KWDFIL(1:ISTLNB(KWDFIL))//'_'//OUTNAM(2)
     if (LKECHO) write(JOSTND,2012) TMPNAM
     2012 format (11X,'(2) ',A30,'-- ANNUAL DEFOLIATION BY SPECIES TABLE')
     call MYOPEN(JOBWP2,TMPNAM,5,133,1,1,1,0,KODE)
  end if
  if (LP3) then
     TMPNAM = KWDFIL(1:ISTLNB(KWDFIL))//'_'//OUTNAM(3)
     if (LKECHO) write(JOSTND,2013) TMPNAM
     2013 format (11X,'(3) ',A30,'-- DEFOLIATION SUMMARY')
     IDEFPR = 1
     IBUDYR = 0
     do I = 1,100
        do J = 1,5
           IDEF(I,J) = 0
           2025 continue
        end do
     end do
     if (IDEFPR.eq.1) then
        call MYOPEN (JOBWP3,TMPNAM,5,133,1,1,1,0,KODE)
        do IP = 1,5
           DLABS(IP) = '        '
           if (IDEFSP(IP).eq.1) DLABS(IP) = TREESP(IP)
           2030 continue
        end do
        NUMCOL = 5
        write (JOBWP3,2035) IDEFPR,NUMCOL,(DLABS(I),I = 1,5)
        2035 format (I4,4X,I4,/,8X,5A8)
     else if (IDEFPR.eq.2) then
        call MYOPEN(JOBWP3,TMPNAM,3,133,1,1,1,0,KODE)
        if (KODE.eq.0) then
           read (JOBWP3,2040) IDEFPR,NUMCOL,(DLABS(N),N = 1,NUMCOL)
           2040 format (I4,4X,I4,/,8X,5(A8,2X))
           if (IDEFPR.eq.2) then
              do J = 1,100
                 if (NUMCOL.eq.1) read (JOBWP3,2045,end = 2052) IDEF(J,1)
                 if (NUMCOL.eq.2) read (JOBWP3,2045,end = 2052)&
                      &(IDEF(J,I),I = 1,2)
                 if (NUMCOL.eq.3) read (JOBWP3,2045,end = 2052)&
                      &(IDEF(J,I),I = 1,3)
                 if (NUMCOL.ge.4) read (JOBWP3,2045,end = 2052)&
                      &(IDEF(J,I),I = 1,4)
                 2045 format(8X,4(I4,6X))
                 2050 continue
              end do
              2052 NUMCOL = NUMCOL+1
              if (NUMCOL.gt.5) then
                 write (*,2055)
                 2055 format('********   ERROR - WSBW: TOO MANY COLUMNS ',&
                      &'FOR DEFOLIATION TABLE -- WILL WRITE OVER ',&
                      &'PREVIOUS COLUMN ')
                 NUMCOL = 5
              end if
              DLABS(NUMCOL) = DEFLAB
              close (JOBWP3)
              call MYOPEN(JOBWP3,TMPNAM,5,133,1,1,1,0,KODE)
              write (JOBWP3,2040) IDEFPR,NUMCOL,(DLABS(I),I = 1,NUMCOL)
           else
              close (JOBWP3)
              call MYOPEN (JOBWP3,TMPNAM,5,133,1,1,1,0,KODE)
              DLABS(1) = DEFLAB
              do IP = 2,5
                 DLABS(IP) = '        '
                 2060 continue
              end do
              NUMCOL = 1
              IDEFPR = 2
              write (JOBWP3,2040) IDEFPR,NUMCOL,DLABS(1)
           end if
        else
           call MYOPEN (JOBWP3,TMPNAM,5,133,1,1,1,0,KODE)
           DLABS(1) = DEFLAB
           do IP = 2,5
              DLABS(IP) = '        '
              2065 continue
           end do
           NUMCOL = 1
           write (JOBWP3,2040) IDEFPR,NUMCOL,DLABS(1)
        end if
        !        ELSE
        !           WRITE (*,2070) IDEFPR
        ! 2070      FORMAT ('********   ERROR - WSBW: IDEFPR = ',I6)
     end if
  end if
  if (LP4) then
     TMPNAM = KWDFIL(1:ISTLNB(KWDFIL))//'_'//OUTNAM(4)
     if (LKECHO)write(JOSTND,2014) TMPNAM
     2014 format (11X,'(4) ',A30,'-- PARAMETERS AND KEY EVENTS SUMMARY')
     call MYOPEN(JOBWP4,TMPNAM,5,133,1,1,1,0,KODE)
  end if
  if (LP5) then
     TMPNAM = KWDFIL(1:ISTLNB(KWDFIL))//'_'//OUTNAM(5)
     if (LKECHO)write(JOSTND,2015) TMPNAM
     2015 format (11X,'(5) ',A30,'-- BRIEF ANNUAL SUMMARY')
     call MYOPEN(JOBWP5,TMPNAM,5,133,1,1,1,0,KODE)
  end if
  if (LP6) then
     TMPNAM = KWDFIL(1:ISTLNB(KWDFIL))//'_'//OUTNAM(6)
     if (LKECHO) write(JOSTND,2016) TMPNAM
     2016 format (11X,'(6) ',A30,'-- ANNUAL DEFOLIATION SUMMARY')
     call MYOPEN(JOBWP6,TMPNAM,5,133,1,1,1,0,KODE)
  end if
  if (LP7) then
     TMPNAM = KWDFIL(1:ISTLNB(KWDFIL))//'_'//OUTNAM(7)
     if (LKECHO) write(JOSTND,2017) TMPNAM
     2017 format (11X,'(7) ',A30,'-- BUDWORM DYNAMICS SUMMARY')
     call MYOPEN(JOBWP7,TMPNAM,5,133,1,1,1,0,KODE)
  end if
  !     IF (LP8) THEN
  !       CALL MYOPEN(JOBWP8,OUTNAM(8),5,133,1,1,1,0,KODE)
  !     ENDIF

  goto 10

  ! -----------------------OPTION NUMBER 21 -- PARASITE

  2100 continue
  IPARA = 1
  LTEMP1(2) = .true.
  if (LNOTBK(1)) IPARA = IFIX(ARRAY(1))
  if (IPARA.eq.2) then
     LTEMP1(2) = .false.
     OBPHAS(1,2) = OBPHAS(1,1)
     OBPHAS(2,2) = OBPHAS(2,1)
     OBPHAS(3,2) = OBPHAS(3,1)
     if (LKECHO)write(JOSTND,2120) KEYWRD
     2120 format (/,A8,'   PARASITISM RATES DO NOT CHANGE DURING THE ',&
          &'OUTBREAK')
  else
     if (LKECHO)write(JOSTND,2140) KEYWRD
     2140 format (/,A8,'   PARASITISM RATES INCREASE DURING THE ',&
          &'OUTBREAK')
  end if
  goto 10

  ! -----------------------OPTION NUMBER 22 -- FQUALDEV

  2200 continue
  IQUALD = 1
  LTEMP1(3) = .true.
  if (LNOTBK(1)) IQUALD = IFIX(ARRAY(1))
  if (IQUALD.eq.2) then
     LTEMP1(3) = .false.
     FOLDVY(3) = FOLDVY(1)
     FOLDVY(4) = FOLDVY(1)
     if (LKECHO)write(JOSTND,2220) KEYWRD
     2220 format (/,A8,'   LARVAL DEVELOPMENT TIMES DO NOT ',&
          &'CHANGE DURING THE OUTBREAK')
  else
     if (LKECHO) write(JOSTND,2240) KEYWRD
     2240 format (/,A8,'   LARVAL DEVELOPMENT TIMES INCREASE',&
          &' DURING THE OUTBREAK')

  end if
  goto 10

  ! -----------------------OPTION NUMBER 23 -- FQUALWT

  2300 continue
  IQUALW = 1
  LTEMP1(4) = .true.
  if (LNOTBK(1)) IQUALW = IFIX(ARRAY(1))
  if (IQUALW.eq.2) then
     LTEMP1(4) = .false.
     FOLWTY(3) = FOLWTY(1)
     FOLWTY(4) = FOLWTY(1)
     if (LKECHO)write(JOSTND,2320) KEYWRD
     2320 format (/,A8,'   PUPAL WEIGHTS DO NOT ',&
          &'CHANGE DURING THE OUTBREAK')
  else
     if (LKECHO) write(JOSTND,2340) KEYWRD
     2340 format (/,A8,'   PUPAL WEIGHTS DECREASE',&
          &' DURING THE OUTBREAK')

  end if
  goto 10

  ! -----------------------OPTION NUMBER 24 -- TITLE

  2400 continue
  read (IREAD,2410,end = 80) ITITLB
  2410 format (A72)
  RECNT = RECNT+1
  if (LKECHO) write(JOSTND,2420) KEYWRD,ITITLB
  2420 format (/,A8,'   BUDWORM TITLE= ',A72)
  goto 10

  ! -----------------------OPTION NUMBER 25 -- BWSPRAY

  2500 continue
  if (LNOTBK(1)) ISPRAY = IFIX(ARRAY(1))
  if (LNOTBK(2)) ISPVAR = IFIX(ARRAY(2))
  if (LNOTBK(3)) INSTSP = IFIX(ARRAY(3))
  if (LNOTBK(4)) SPEFF = ARRAY(4)
  if (LNOTBK(5)) LIMITS = IFIX(ARRAY(5))

  if (ISPRAY.eq.1) then
     IACTK = 2157
     IDT = ISPVAR
     call OPNEW(KODE,IDT,IACTK,2,ARRAY(3))
     if (KODE.gt.0) goto 10
     if (LKECHO)write(JOSTND,2520) KEYWRD,&
          &ISPVAR,STAGE(INSTSP),SPEFF
     2520 format (/,A8,'   INSECTICIDE TO BE APPLIED IN YEAR ',I4,&
          &/,11X,'AT PEAK ',A6,' INSTAR, RESULTING IN ',F5.1,&
          &' PERCENT MORTALITY')
  else if (ISPRAY.eq.2) then
     IACTK = 2159
     IDT = 0
     TRIGGR = ISPVAR
     call OPNEW(KODE,IDT,IACTK,2,ARRAY(3))
     if (KODE.gt.0) goto 10
     if (LKECHO)&
          &write(JOSTND,2540) KEYWRD,ISPVAR,STAGE(INSTSP),SPEFF
     2540 format (/,A8,'   INSECTICIDE TO BE APPLIED WHEN ',&
          &'% DEFOLIATION FIRST RISES ABOVE ',I4,/,&
          &11X,'AT PEAK ',A6,' INSTAR, RESULTING IN ',F5.1,&
          &' PERCENT MORTALITY')
     if ((LIMITS.eq.1).and.LKECHO)write(JOSTND,2550)
     2550 format (11X,'ONLY ONE SPRAY APPLICATION ALLOWED PER OUTBREAK')
  end if
  goto 10

  ! -----------------------OPTION NUMBER 26 -- OBSCHED

  2600 continue
  if (LNOTBK(1)) IOBOPT = IFIX(ARRAY(1))

  if (IOBOPT.eq.1) then
     if (LKECHO) write(JOSTND,2671) KEYWRD
     2671 format (/,A8,'   STARTING DATES GENERATED FROM FREQUENCY',&
          &' STATISTICS;',/,11X,'OUTBREAKS END WHEN % NEW DEFOLIATION',&
          &' IS < 10% FOR'/,11X,'3 CONSECUTIVE YEARS (5 YEAR MIN.)')

  else if (IOBOPT.eq.2) then
     if (LKECHO)write(JOSTND,2672) KEYWRD
     2672 format (/,A8,'   BOTH STARTING AND ',&
          &'ENDING DATES GENERATED FROM STATISTICS')

  else if (IOBOPT.eq.3) then
     if (LKECHO)write(JOSTND,2673) KEYWRD
     2673 format (/,A8,'   USER HAS SELECTED OUTBREAK',&
          &' STARTING AND ENDING YEARS:')
     NOBSCH = 0
     K = 0
     do IOB = 1,3
        ISTART = 0
        IEND = 0
        K = K+2
        if (LNOTBK(K)) ISTART = IFIX(ARRAY(K))
        if (LNOTBK(K+1)) IEND = IFIX(ARRAY(K+1))
        if (ISTART.ne.0.and.IEND.ne.0) then
           NOBSCH = NOBSCH+1
           IOBSCH(NOBSCH,1) = ISTART
           IOBSCH(NOBSCH,2) = IEND
           if (LKECHO) write(JOSTND,2675) NOBSCH,ISTART,IEND
           2675 format (14X,'OUTBREAK NO. ',I1,':  START= ',I4,&
                &', END= ',I4)
        end if
        2620 continue
     end do

  else if (IOBOPT.eq.4) then
     if (LKECHO) write(JOSTND,2676) KEYWRD
     2676 format (/,A8,'   FIRST OUTBREAK SCHEDULED, THEN ',&
          &'GENERAL DEFOLIATOR RUNS ',/,11X,&
          &'UNTIL THE END OF THE SIMULATION.')

  else

     !       INVALID OPTION FOR OUTBREAK SCHEDULING.

     write(JOSTND,2678) KEYWRD
     2678 format (/,A8,'   INVALID OUTBREAK SCHEDULING OPTION. FIELD ',&
          &'1 VALUE MUST BE 1, 2, 3 OR 4.')
  end if

  goto 10

  9000 continue
  if (DEBUG) write(JOSTND,*) 'EXIT  BWEIN: ICYC = ',ICYC
  return
end
