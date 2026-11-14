!===================!
!   wwpb : bmppin   !
!===================!
! WWPB $Id$
subroutine BMPPIN (IREADx,IRECNTx,IJOPPR)

  !  **BMPPIN  DATE OF LAST REVISION: DECEMBER 13, 1994
  !  **BMPPIN  DATE OF LAST REVISION: May 6, 1998 (RNH, May98)
  !     Revised OPTION 29 -- SANITIZE to read user input stand lists
  !                          similar to the way OPTION 28 -- SALVAGE
  !                          works.  Also modified the BMSANI module to
  !                          work like BMSALV
  !  **BMPPIN  DATE OF LAST REVISION: May 27, 1998 (MJO May98)(MJO July98)
  !     Added OPTION 38 -- BMFDBK beetle negative feedback keyword

  !  **BMPPIN  DATE OF LAST REVISION Aug 26, 1999 (AJM)  Modified ATTRACT
  !            section (Option 14) so that conversions are done appropriately.

  !   Revised 3/21/00 AJM;  replaced all calls to CH8* sort subroutines to
  !     C26* (ie c26srt & c26bsr). Passed arguments are now 26 characters.
  !     NOTE: I also changed dimensions of NSSTDS and BMSTDS in BMCOM.f77.

  !...Revised 3/28/00 AJM.  Modified BMHIST, WETSCORE, RPHERO, APHERO, SPRAY,
  !     SPRAY, SANITIZE, SALVAGE, SLASHMGT, and NONSTOCK, so that supplemental
  !     records can be 26 characters long.

  !   Revised: 7/00 Fixed logicals in QRROT and QSRUST.  Because they were
  !     changed (in BMINIT?) to be OFF by default, the logicals herein
  !     need to be switched ON if the keyword is encountered.

  !   Revised 4/01 AJM. Because I added CONTRL.F77 as common (so we can use
  !   variable KWDFIL in BMTOTALS [et al.]), I am renaming the IREAD &
  !   IRECNT arguments in this SUBROUTINE's argument list--so that they
  !   won't be used here as passed arguments (i.e. they are now included
  !   via common block.  Passed arguments are now dummy.)

  !   Revised 9/19/01. ajm.  changed 8th argument in calls to MYOPEN from
  !     1 to 0. (With a 1, format of output files was altered...1st column
  !     was omitted, which resulted in errors in the ArcView Display tools.)

  !   Revised 08/16/02 Lance R. David (FHTET)
  !   Added option number 39, COMMENT keyword, processing.

  !   10-NOV-2003 - Lance R. David (FHTET)
  !      Added LFLAG to KEYRDR call statement argument list.

  !   Revised 9/05 AJM (FHTET)
  !    Adding new keyword BMOUT.  It contains 2 fields.
  !    Field 1 invokes writing of *NEW* "cycle" output file (*.bmc)
  !    Field 2 invokes writing of *NEW* landscape average output file (*.bml).
  !    See new BMOUT routine for details.  This change concurrent with the
  !    addition to the WWPBM of a new stand level option processor: BMIN,
  !    used for processing ** stand level ** output generating keywords.
  !    Keywords BMTOTALS, BMDETAIL, and BMDVOUT are now *OBSOLETE*!

  !     WWPB - West Wide Pine Beetle
  !            [MOUNTAIN PINE BEETLE/WESTERN PINE BEETLE/IPS]
  !     OPTION PROCESSOR FOR THE MULTI-STAND MOUNTAIN PINE BEETLE MODEL.
  !     THIS IS AN EXTENSION TO THE PARALLEL PROCESSING SYSTEM.

  !     CALLED FROM: PPIN

  ! PARAMETER DEFINITIONS
  !     IREAD  - logical unit number for the keyword file [I]               ! SEE 4/01 REVISION NOTE
  !     IRECNT - count for number of records read from the keyword file [O] ! DITTO
  !     IJOPPR - logical unit number for standard output file.
  !             (this is a dummmy variable for this routine because the
  !              variable JOPPRT is in the PPE common space defined in the
  !              include file PPCNTL.F77 which means JOPPRT can not be used
  !              in the parameter list. The parameter is necessary because the
  !              call from PPE routine PPIN needs to identify an output unit
  !              on which to write a message if the WWPB model is not attached.
  !              look in the WWPB stub routine EXBM.F77 entry point BMPPIN.)

  ! VARIABLE DEFINITIONS
  !     DEFYRS - THE DEFAULT NUMBER OF YEARS AN OPTION WILL BE "ON" AT THE
  !              GIVEN PARAMETER VALUES.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PPEPRM.f90'
  include 'PPCNTL.f90'

  ! Adding CONTRL for varialbe KWDFIL used with BMPPIN AJM 4/01

  include 'CONTRL.f90'
  include 'BMPRM.f90'
  include 'BMCOM.f90'
  include 'BMRRCM.f90'
  include 'BMPCOM.f90'

  parameter (KWCNT = 40)

  !...TEST.  CURRENTLY, SUPPLEMENTAL STAND IDs ARE READ AS VARIABLE
  !   "KEYWRD", WHICH DOESN'T WORK ANYMORE SINCE STAND IDs ARE NOW
  !   CHARACTER*26.  SO I'M CREATING A NEW VARIABLE NAME, DIMENSIONED
  !   CHARACTER*26, WHICH WILL BE THE SUPPLEMENTAL RECORDS FOR SANITIZE
  !   ie THE LIST OF STAND IDs TO SANITIZE (WHICH NEED TO BE PASSED TO
  !   SUBROUTINE C26BSR AS CHARACTER*26).

  ! Variables
  character(len=4) :: C4TMP
  character(len=8) :: TABLE
  character(len=8) :: KEYWRD
  character(len=8) :: PASKEY
  character(len=26) :: SUPLRECS
  character(len=80) :: RECORD
  character(len=10) :: KARD(12)
  character(len=250) :: OUTNAM
  logical :: LNOTBK,LKECHO
  integer :: MINSZ,NPARMS,ISIZ,IDT,IJOPPR,HISTSZ
  integer :: BSPEC
  integer :: MYLST1(MXSTND),MYLST2(MXSTND)
  real    :: DEFYRS,ARRAY(12),HISTNUM
  dimension PRMS(8)
  dimension TABLE(KWCNT),LNOTBK(12)
  data DEFYRS/ 20/
  data ISIZE / KWCNT/

  ! *** Added BMFDBK to the end of table below. MJO May98
  !Changing BMTOTALS to BMOUT.  Changing LBMVOL to LBMCYC

  data TABLE /&
       &'DISPERSE','RVDENSE ','BMTOTALS','BMDETAIL','DEBUG   ',&
       &'NODEBUG ','RANNSEED','END     ','NOTOTAL ','BMDVOUT',&
       &'BMHIST  ','BMPARM  ','VARYRAIN','ATTRACT ','WETSCORE',&
       &'REPRODN ','BKPKILL ','LIGHTN  ','IPSDV   ','QWINDTH ',&
       &'QRROT   ','QSRUST  ','OTHERBB ','QMORT   ','RPHERO  ',&
       &'APHERO  ','SPRAY   ','SALVAGE ','SANITIZE','OWVALUES',&
       &'OWTYPE  ','OWIPSDV ','QFIRE   ','QDEFOL  ','SLASHMGT',&
       &'NONSTOCK','BMDAMAGE','BMFDBK  ','COMMENT ','BMOUT   '/

  !     **********          EXECUTION BEGINS          **********

  !     Initialization is carried out only the first time that BMPPIN is called
  !     (ie, when LBMINT=True). This should avoid the problem of values that were
  !     changed by a keyword being reset when the management keywords are added.

  if (LBMINT) call BMINIT (JOPPRT)

  LKECHO = .true.

  10 continue
  call KEYRDR (IREAD,JOPPRT,LBMDEB,KEYWRD,LNOTBK,&
       &ARRAY,IRECNT,KODE,KARD,LFLAG,LKECHO)

  !     RETURN KODES 0=NO ERROR,1=COLUMN 1 BLANK,2=EOF

  if (KODE .eq.0) go to 30
  if (KODE .eq.2) call ERRGRO(.false.,2)
  call ERRGRO (.true.,6)
  goto 10
  30 continue
  call FNDKEY (NUMBER,KEYWRD,TABLE,ISIZE,KODE,LBMDEB,JOPPRT)

  !     RETURN KODES 0=NO ERROR,1=KEYWORD NOT FOUND,2=MISSPELLING.

  if (KODE .eq.0) goto 90
  if (KODE .eq.1) then
     call ERRGRO (.true.,1)
     goto 10
  end if
  goto 90

  !     SPECIAL END-OF-FILE TARGET

  !* 80 CONTINUE
  !*    CALL ERRGRO (.FALSE.,2)
  90 continue

  !     PROCESS OPTIONS *** Added 4800 to list below. MJO May98

  go to( 1000,1200,1300,1400,1500,1600,1700,1800,1900,2000&
       &,2100,2200,2300,2400,2500,2600,2700,2800,2900,3000&
       &,3100,3200,3300,3400,3500,3600,3700,3800,3900,4000&
       &,4100,4200,4300,4400,4500,4600,4700,4800,4900,5000)NUMBER

  1000 continue
  !                        OPTION 1 -- DISPERSE
  !      IF (LNOTBK(1)) GOTO 1005
  !      CALL KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
  !      CALL ERRGRO (.TRUE.,4)
  !      GOTO 10
  ! 1005 CONTINUE
  if (LNOTBK(1)) then
     I = IFIX(ARRAY(1))
  else
     I = 1
  end if
  if (.not.LNOTBK(2)) ARRAY(2) = 50.
  call GPNEW (KODE,I,301,1,ARRAY(2))
  if (KODE.gt.0) then
     call KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
  else
     if (LKECHO)write(JOPPRT,1010) KEYWRD,I,IFIX(ARRAY(2))
     1010 format (/1X,A8,'   ALL STANDS WILL BE INVOLVED IN ',&
          &'THE WWPB DISPERSAL AREA.'/T13,'SIMULATION IS SCHEDULED',&
          &' TO START IN ',I4,' WITH A MAXIMUM DURATION OF ',I4,' YEARS.')

  end if
  goto 10
  1200 continue
  !                        OPTION NUMBER 2 -- RVDENSE
  LCDENS = .false.
  if (LKECHO)write(JOPPRT,1210) KEYWRD
  1210 format (/1X,A8,'    STAND DENSITY WILL NOT AFFECT THE RATING ',&
       &'VALUE OF TREES')
  goto 10
  1300 continue
  !                        OPTION NUMBER 3 -- BMTOTALS

  ! THIS IS NOW OBSOLETE.  ISSUE WARNING

  write(JOPPRT,1310) KEYWRD
  1310 format (/1X,A8,'   WWPB KEYWORD BMTOTALS IS NO LONGER SUPPORTED.'/&
       &,T13,'Keyword BMTOTALS has been replaced by keyword "BMOUT";',&
       &' PLEASE REVISE YOUR KEYWORD SETS.'//,&
       &T13,'! * !   THIS PROGRAM WILL REDIRECT YOUR BMTOTALS OUTPUT ',&
       &'REQUEST TO KEYWORD BMOUT (see below).      ! * !' )

  !  GO TO BMOUT PROCESSING.  SET LNOTBKs TO FALSE TO INVOKE THE WRITING
  !  OF BOTH OF BMOUT's OUTPUT FILES
  !  (THESE SHOULD ALREADY BE FALSE ANYWAY; THIS ENSURES IT SO.)

  KEYWRD = 'BMOUT   '
  LNOTBK(1) = .false.
  LNOTBK(2) = .false.

  go to 5000 ! GO TO BMOUT

  ! COMMENT OUT ALL THE REST  AJM SEPT 5005

  !C          LBMVOL=.TRUE.
  !C
  !C     Set flag to write annual stand output to unit JBMSPY (28)
  !C     and landscape annual data to unit JBMLPY (29)
  !C     (RNH July98)
  !C
  !      LBMSPY= .TRUE.
  !      LBMLPY= .TRUE.
  !C
  !      IF (LNOTBK(1)) JBMVOL=IFIX(ARRAY(1))
  !      IF(LKECHO)WRITE(JOPPRT,1310) KEYWRD,JBMVOL
  ! 1310 FORMAT (/1X,A8,'   WWPB MAIN OUTPUT STATISTICS WILL BE ',
  !     >       'WRITTEN TO DATA SET REFERENCE NUMBER ',I4)

  !C ADDING CODE THAT WILL (1) OPEN THE OUTPUT FILES; (2) NAME THE OUTPUT FILES
  !C INVOKED BY THIS KEYWORD.  Now, the output files are not named by the user;
  !C instead, they are named the same name as the simulation filename, with extensions
  !C *.bmt, *.bml, *.bms for the MAIN, LANDSCAPE-PER-YEAR and STAND-PER-YEAR
  !C output files respectively.  Before this change, users would have to OPEN and name
  !C these files separately and independently via the OPEN keyword.  NOW, if keyword
  !C BMTOTALS is used, all three files will be written, opened, and named.  AJM 4/01

  !      OUTNAM=KWDFIL(:ISTLNB(KWDFIL))//'.bmt'
  !C      CALL MYOPEN (JBMVOL,OUTNAM,1,133,0,1,1,1,KODE)
  !      CALL MYOPEN (JBMVOL,OUTNAM,1,133,0,1,1,0,KODE)
  !      IF (KODE.GT.0) THEN
  !         WRITE (*,'('' OPEN FAILED FOR '')') OUTNAM
  !      ENDIF

  !      OUTNAM=KWDFIL(:ISTLNB(KWDFIL))//'.bml'
  !C      CALL MYOPEN (JBMLPY,OUTNAM,1,133,0,1,1,1,KODE)
  !      CALL MYOPEN (JBMLPY,OUTNAM,1,133,0,1,1,0,KODE)
  !      IF (KODE.GT.0) THEN
  !         WRITE (*,'('' OPEN FAILED FOR '')') OUTNAM
  !      ENDIF

  !      OUTNAM=KWDFIL(:ISTLNB(KWDFIL))//'.bms'
  !C      CALL MYOPEN (JBMSPY,OUTNAM,1,133,0,1,1,1,KODE)
  !      CALL MYOPEN (JBMSPY,OUTNAM,1,133,0,1,1,0,KODE)
  !      IF (KODE.GT.0) THEN
  !         WRITE (*,'('' OPEN FAILED FOR '')') OUTNAM
  !      ENDIF

  goto 10

  1400 continue
  !                        OPTION NUMBER 4 -- BMDETAIL

  ! This keyword is now obsolete! AJM  8/05

  ! stand-level detailed output is now available via BMIN option processor

  !      LBMDET=.TRUE.
  !      IF (LNOTBK(1)) JBMDBH=IFIX(ARRAY(1))

  !     ISSUE OBSOLETE WARNING AJM 9/05

  write(JOPPRT,1410) KEYWRD
  1410 format (/1X,A8,'   WWPB MODEL KEYWORD BMDETAIL IS NOW OBSOLETE!',/&
       &T12,' Detailed output may be obtained via new * STAND-LEVEL *',&
       &' WWPB Model keywords: MAINOUT, TREEOUT, BKPOUT, &',&
       &' VOLOUT,',/T13,'which, if used, must be attached between',&
       &' a "BMIN" & "END" set of keywords,',/T12,&
       &' AND must be attached to each stand for which output ',&
       &' is desired.',/T13,'(i.e. between each stand`s pair of',&
       &'"ADDSTAND" & "PROCESS" keywords.)')

  !      IF(LKECHO)WRITE(JOPPRT,1410) KEYWRD,JBMDBH
  ! 1410 FORMAT (/1X,A8,'   WWPB DETAILED SIZE CLASS STATISTICS ',
  !     >       'WILL BE WRITTEN TO DATA SET REFERENCE NUMBER ',I4)

  ! Adding code so that if this keyword is used (if detailed output file requested)
  ! then the file will be already opened and named.  User now no longer needs to explicilty
  ! name the file, nor use OPEN keyword.  Name given will be the simulation (keyword)
  ! filename, suffixed with "bmd".

  !      OUTNAM=KWDFIL(:ISTLNB(KWDFIL))//'.bmd'
  !C      CALL MYOPEN (JBMDBH,OUTNAM,1,133,0,1,1,1,KODE)
  !      CALL MYOPEN (JBMDBH,OUTNAM,1,133,0,1,1,0,KODE)
  !      IF (KODE.GT.0) THEN
  !         WRITE (*,'('' OPEN FAILED FOR '')') OUTNAM
  !      ENDIF

  goto 10
  1500 continue
  !                        OPTION NUMBER 5 -- DEBUG
  LBMDEB = .true.
  if (LKECHO)write(JOPPRT,'(/1X,A8)') KEYWRD
  goto 10
  1600 continue
  !                        OPTION NUMBER 6 -- NODEBUG
  LBMDEB = .false.
  if (LKECHO)write(JOPPRT,'(/1X,A8)') KEYWRD
  goto 10
  1700 continue
  !                        OPTION NUMBER 7 -- RANNSEED
  call BMRNSD (LNOTBK(1),ARRAY(1))
  if (LKECHO)write(JOPPRT,1710) KEYWRD,ARRAY(1)
  1710 format (/1X,A8,'   RANDOM SEED IS:',F10.0)
  ! define new variable for identifying this specific run in a series
  ! of monte carlo simulations.  the randome number seed, unique to
  ! each simulation, is the identifier.  A new ouput file is written from
  ! bmoutm, this variable included.  this variable added to common block.
  !      DREWTEST = ARRAY(1)
  goto 10
  1800 continue
  !                        OPTION NUMBER 8 -- END
  if (LKECHO)write(JOPPRT,1810) KEYWRD
  1810 format (/1X,A8,'   END OF MULTI-STAND WWPB OPTIONS.')
  goto 900
  1900 continue
  !                        OPTION NUMBER 9 -- NOTOTAL

  LBMVOL = .false.
  if (LKECHO)write(JOPPRT,1910) KEYWRD
  1910 format (/1X,A8,'    WWPB MAIN OUTPUT FILE WILL NOT BE WRITTEN.')
  goto 10
  2000 continue
  !                        OPTION NUMBER 10 -- BMDVOUT

  ! This keyword is now obsolete! AJM  8/05

  ! stand-level detailed output is now available via BMIN option processor

  !      LBMDVO=.TRUE.
  !      IF (LNOTBK(1)) JBMDV=IFIX(ARRAY(1))

  !     ISSUE OBSOLETE WARNING AJM 9/05

  write(JOPPRT,2010) KEYWRD
  2010 format (/1X,A8,'   WWPB MODEL KEYWORD BMDVOUT IS NOW OBSOLETE!',/&
       &T12,' Driving variable output may be obtained via a new',&
       &' *STAND-LEVEL* WWPB Model keyword: BKPOUT,'/T12,&
       &' which, if used, must be attached between a "BMIN" & "END"',&
       &' set of keywords,'/T13,'AND must be attached to each',&
       &' stand for which output is desired.',/T12,'(i.e. between',&
       &' each stand`s pair of "ADDSTAND" & "PROCESS" keywords.)')

  !      IF(LKECHO)WRITE(JOPPRT,2010) KEYWRD, JBMDV
  ! 2010 FORMAT (/1X,A8,'   WWPB DRIVING VARIABLES OUTPUT FILE',
  !     >       ' WILL BE WRITTEN TO DATA SET REFERENCE NUMBER ',I4)
  !C
  !C Adding code so that if this keyword is used (if driving variable output file requested)
  !C then the file will be already opened and named.  User now no longer needs to explicilty
  !C name the file, nor use OPEN keyword.  Name given will be the simulation (keyword)
  !C filename, suffixed with "bmv".

  !      OUTNAM=KWDFIL(:ISTLNB(KWDFIL))//'.bmv'
  !C      CALL MYOPEN (JBMDV,OUTNAM,1,133,0,1,1,1,KODE)
  !      CALL MYOPEN (JBMDV,OUTNAM,1,133,0,1,1,0,KODE)
  !      IF (KODE.GT.0) THEN
  !         WRITE (*,'('' OPEN FAILED FOR '')') OUTNAM
  !      ENDIF

  goto 10
  2100 continue
  !                        OPTION NUMBER 11 -- BMHIST

  !     Initial conditions for WWPB infestation
  HISTNUM = 0
  IUNIT = IREAD
  BSPEC = PBSPEC
  if (LNOTBK(1)) IUNIT = IFIX(ARRAY(1))
  if (LNOTBK(2)) BSPEC = IFIX(ARRAY(2))

  if (LKECHO)write(JOPPRT,2110) KEYWRD,IUNIT,BSPEC
  2110 format (/1X,A8,'   BEETLE-KILLED TREES WILL BE READ FROM ',&
       &'DATA REFERENCE NUMBER',I4,' FOR BEETLE SPECIES ',I3)

  BMCNT = 0
  2120 continue

  !...Changes below for 26-character supplemental records.

  !   This line is original code...READ (IUNIT,'(A8,T11,7F10.0)',END=2130) KEYWRD,ARRAY

  !      The line below works, but I'm changing ARRAY.
  !      READ (IUNIT,'(A26,T31,2F10.2,5F1.0)',END=2130) SUPLRECS,ARRAY
  !      IF (INDEX(KEYWRD,'-999').GT.0) GOTO 2130

  !   I've defined 2 new local variables--HISTSZ and HISTNUM--to substitute
  !   for ARRAY(1) and ARRAY(2) parameters.

  read(IUNIT,'(A26,T31,I10,F10.0)',end = 2130)SUPLRECS,HISTSZ,HISTNUM
  if (SUPLRECS(1:1) .eq. '*') goto 2120
  if (INDEX(SUPLRECS,'-999').gt.0) goto 2130
  BMCNT = BMCNT+1
  if (BMCNT.gt. MXSTND) then
     !     NOTE AJM 12/05.
     !     SOMETIME IN THE FUTURE, WE MIGHT WANT TO CHANGE THE WAY THIS IS DONE.
     !     BECAUSE WE TEMPORARILY "CO-OPT" ARRAY BMSTDS() HERE, WE NECESSARILY
     !     CONSTRAIN BMCNT TO BE NO GREATER THAN MXSTND.  BUT BMCNT SHOULD BE ABLE
     !     TO BE GREATER THAN MXSTND.  FOR EXAMPLE, USERS CURRENTLY MAY ENTER FOR
     !     EACH STAND MORE THAN ONE RECORD IN THE BMHIST SUPPLEMENTAL RECORD LIST.
     !     IDEALLY, BMHIST STAND DATA OUGHT TO BE KEPT IN ITS OWN ARRAY(S) DIMENSIONED
     !     MXSTND*10. I AM LEAVING THIS AS IS FOR NOW, BECAUSE THERE IS NO IMMEDIATE
     !     NEED TO ENABLE MORE THAN 10,000 (MXSTND) # OF BMHIST SUPPLEMETAL RECORDS.
     !     NOTE HOWEVER THAT THE CONSTRAINT THAT THE NUMBER OF BMHIST RECORDS (BMCNT)
     !     BE NO GREATER THAN MXSTND IS THEORETICALLY AN UNREASONABLE CONSTRAINT.
     BMCNT = MXSTND
     WRITE(JOPPRT,'(/T13,''MAX STANDS EXCEEDED; DATA '',             &
          !     >         ''IGNORED: '',A8,2X,7F9.2)') KEYWRD,ARRAY
          &''IGNORED: '',A26,4X,I4,F6.2)') SUPLRECS,HISTSZ,HISTNUM
     if (IUNIT.eq.IREAD) IRECNT = IRECNT + 1
     goto 2120
  end if
  !      BMSTDS(BMCNT)= KEYWRD
  BMSTDS(BMCNT) = SUPLRECS
  !     J= IFIX(ARRAY(1))
  J = HISTSZ
  if ((J .ge.1) .and. (J .le. NSCL)) then
     BMINI(BMCNT,BSPEC,1) = real(J)
     BMINI(BMCNT,BSPEC,2) = AMAX1(HISTNUM,0.0)
  else
     BMINI(BMCNT,BSPEC,1) = -999.0
     BMINI(BMCNT,BSPEC,2) = -999.0
  end if
  goto 2120

  2130 continue

  if (IUNIT.eq.IREAD) IRECNT = IRECNT+BMCNT+1
  if (LKECHO)write(JOPPRT,'(/T13,''RECORDS PROCESSED= '',I4)') BMCNT

  goto 10

  2200 continue
  !                        OPTION NUMBER 12 -- BMPARM
  !     Initialize many of the parameters used for the beetle model, such as species,
  !     and number of generations
  !     Minimum diam for attractiveness and distance divisor are now initialized in
  !     ATTRACT

  if (LNOTBK(1)) PBSPEC = IFIX(ARRAY(1))
  if (PBSPEC .le.1) PBSPEC = 1
  if (PBSPEC .gt.4) PBSPEC = 4
  if (LNOTBK(2)) NBGEN = IFIX(ARRAY(2))
  if (NBGEN .le.1) NBGEN = 1
  if (NBGEN .ge.5) NBGEN = 5
  if (PBSPEC .eq.3) then
     NIBGEN = NBGEN
  end if

  if (LNOTBK(3)) HSPEC(PBSPEC,7) = 0

  do I = 3,7
     if (LNOTBK(I)) then
        JSP = IFIX(ARRAY(I))
        HSPEC(PBSPEC,JSP) = 1
     end if
     2205 continue
  end do

  !    Turn off Ips as a driving variable if IPSDV was used before BMPARM and the main
  !    beetle species is Ips

  if (IPSON .and. PBSPEC .eq.3) IPSON = .false.

  if (LKECHO)write(JOPPRT,2210) KEYWRD,PBSPEC,NBGEN
  2210 format (/1X,A8,'   BEETLE SPECIES CODE IS:',I3,&
       &'     NUMBER OF GENERATIONS IS:',I3)
  do I = 1,MAXSP
     if (HSPEC(PBSPEC,I) .eq.1) then
        if (LKECHO)write(JOPPRT,2220) I
        2220 format ('            HOST TREE SPECIES WILL BE:',I4)
     end if
     2225 continue
  end do

  goto 10

  2300 continue
  !                        OPTION NUMBER 13 -- VARYRAIN
  ! Landscape level, ON in year 'IDT'; stays ON until turned off by user.

  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  NPARMS = 6
  PRMS(1) = 0.0
  if (LNOTBK(2)) PRMS(1) = ARRAY(2)
  PRMS(2) = 3.0
  !      PRMS(2)= 0.3  ..CHANGED THIS 3/31/00 AJM
  if (PRMS(1) .eq.0.0) then
     if (LNOTBK(3)) PRMS(2) = MAX(0.01, ARRAY(3))
  else
     if (LNOTBK(3)) PRMS(2) = ARRAY(3)
  end if
  PRMS(3) = 0.0
  if (LNOTBK(4)) PRMS(3) = ARRAY(4)
  PRMS(4) = 0.0
  if (LNOTBK(5)) PRMS(4) = ARRAY(5)
  PRMS(5) = -1.0
  if (LNOTBK(6)) PRMS(5) = ARRAY(6)
  PRMS(6) = 0.0
  if (LNOTBK(7)) PRMS(6) = ARRAY(7)

  call GPNEW (KODE,IDT,309,NPARMS,PRMS(1))
  if (KODE.gt.0) then
     call KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
     write(JOPPRT,2305) KEYWRD
     2305 format(/1X,A8,' Unable to set options')
     goto 2330
  end if

  if (PRMS(1) .eq. -1.0) then
     if (LKECHO)write(JOPPRT, 2321) KEYWRD, IDT
     2321 format(/1X,A8,'   CLIMATIC ACTIVITY WILL BE TURNED OFF ',&
          &'IN DATE/CYCLE ', I4)
  else
     if (LKECHO)write(JOPPRT, 2310) KEYWRD, IDT
     2310 format(/1X,A8,'   SPECIFIED CLIMATIC ACTIVITY WILL BEGIN ',&
          &'IN DATE/CYCLE ', I4,' AND REMAIN ACTIVE UNTIL TURNED OFF')

     if (PRMS(1) .eq.0.0) then
        if (LKECHO)write(JOPPRT, 2311) PRMS(2)
     else
        if (LKECHO)write(JOPPRT, 2312) PRMS(2)
     end if
     2311 format(12X,'CLIMATIC ACTIVITY WILL BE RANDOM WITH SD=',F7.3)
     2312 format(12X,'THE CHANGE IN CLIMATIC ACTIVITY WILL BE ',F7.3)

     if (LKECHO)write(JOPPRT, 2313) (PRMS(I),I = 3,6)
     2313 format(12X,'THE COEFFICIENTS OF INFLUENCE ARE: DBH= ',F5.2,&
          &' ELEVATION= ',F5.2,' SDI= ',F5.2,' OTHER= ',F5.2)
  end if

  ! Note the use of a special stand id 'MXSTND' to hold landscape level global
  ! parms. The special ID is picked up in the relevant routine.

  call GPADSD(MXSTND)
  call GPCLOS(309)

  2330 continue

  goto 10

  2400 continue
  !                        OPTION NUMBER 14 -- ATTRACT
  !     These are the user-controlled parameters in the between stand
  !     attractiveness equation

  IBSP = PBSPEC
  if (LNOTBK(1)) IBSP = INT(ARRAY(1))
  if (IBSP .lt.1) IBSP = 1
  if (IBSP .gt.3) IBSP = 3

  !     the second field contains the minimum dbh for attractiveness
  if (LNOTBK(2)) then
     MINSZ = IFIX(ARRAY(2))
     do I = 1,NSCL
        if (MINSZ .le. UPSIZ(I)) then
           ISCMIN(IBSP) = I
           goto 2402
        end if
        if (I .eq. NSCL) ISCMIN(IBSP) = I
        2401 continue
     end do
     2402 continue
  end if

  !     The third field contains the special tree multiplier

  if (LNOTBK(3)) USERA(IBSP) = ARRAY(3)

  ! This makes the 'old' DDWT=.1 give identical results to the new 'DDWT=.1'
  ! allowing for meters (old) and miles(new) and [1/ddwt**2] (old) and [ddwt] (new)
  ! Print the DDWT as the meters divisor (DDWTM) instead of the miles one
  !      DDWTM = USERC(IBSP)
  if (LNOTBK(4)) then
     USERC(IBSP) = AMAX1(1.0E-5,ARRAY(4))
  end if
  DDWTM = USERC(IBSP)
  USERC(IBSP) = USERC(IBSP) * USERC(IBSP) * 259.0 * 10000.0
  !         USERC(IBSP) = USERC(IBSP) * USERC(IBSP)
  !      ENDIF

  !     The fifth field controls the "n" in the actractiveness equation
  !     "n" is assumed to be in square meters, so it will be multiplied
  !     here to be in the same units as ("c" * distance)^2
  !     Note that "n" cannot be zero since we take the log of it later

  !      OLDN = SELFA(IBSP)
  if (LNOTBK(5)) then
     SELFA(IBSP) = AMAX1(1.0E-5,ARRAY(5))
  end if
  OLDN = SELFA(IBSP)
  !       SELFA(IBSP)= USERC(IBSP) * USERC(IBSP) * SELFA(IBSP)

  !       SELFA(IBSP) = SELFA(IBSP) * 259.0 * 10000.0

  !...Above CHANGED 8/24/99 (ajm).

  !      ENDIF

  !...Moved the last two "END IF" statements up so that the conversion
  !   will be performed on the default values of C and N, and not just
  !   to those values input in the ATTRACT keyword. (AJM 8/24/99)

  !     The last field contains Rmax, the value used in determining
  !     the extent of outside world and also how far beetles can travel.

  if (LNOTBK(6)) URMAX(IBSP) = ARRAY(6)
  URMAX(IBSP) = MAX(URMAX(IBSP),0.1)

  if (ISCMIN(IBSP) .eq.1) then
     MINSIZ = 0
  else
     MINSIZ = UPSIZ(ISCMIN(IBSP)-1)
  end if

  if (LKECHO)write(JOPPRT,2450) KEYWRD,IBSP,INT(MINSIZ),&
       &USERA(IBSP),DDWTM,OLDN,URMAX(IBSP)
  !     >                    USERA(IBSP),USERC(IBSP),OLDN,URMAX(IBSP)

  2450 format (/1X,A8,'   FOR BEETLE SPECIES TYPE ',I4,&
       &'  MINIMUM ATTRACTIVE DIAMETER WILL BE ',I4,' INCHES;',&
       &'  SPECIAL TREE MULTIPLIER IS ',F6.3,/T13,&
       &'DISTANCE WILL BE MEASURED IN METERS TIMES ',F8.4,&
       &' AND "N" WILL BE ',F8.4,' SQUARE METERS.',&
       &/T13,'BEETLES CAN "SEE" OR TRAVEL ',F7.1,' MILES (RMAX).')

  goto 10

  2500 continue

  !                        OPTION NUMBER 15 -- WETSCORE

  IUNIT = IREAD
  if (LNOTBK(1)) IUNIT = IFIX(ARRAY(1))

  if (LKECHO)write(JOPPRT,2510) KEYWRD,IUNIT
  2510 format (/1X,A8,'   SITE WETNESS INDICES WILL BE ',&
       &'READ FROM DATA REFERENCE NUMBER ',I4)

  WTCNT = 0
  2520 continue
  !      READ (IUNIT,'(A8,T11,7F10.0)',END=2530) KEYWRD,ARRAY
  !      IF (INDEX(KEYWRD,'-999').GT.0) GOTO 2530
  !      WTCNT=WTCNT+1
  !      IF (WTCNT.GT.MXSTND) THEN
  !        WTCNT=MXSTND
  !        IF(LKECHO)WRITE(JOPPRT,'(/T13,''MAX STANDS EXCEEDED; DATA '',
  !     >         ''IGNORED: '',A8,2X,7F9.2)') KEYWRD,ARRAY
  !        IF (IUNIT.EQ.IREAD) IRECNT= IRECNT + 1
  !        GOTO 2520
  !        ENDIF

  !      WTSTDS(WTCNT)= KEYWRD
  !      WTSC(WTCNT)= ARRAY(1)

  !...Fix for 26-character supplemental records.  1/28/00.
  !...this first attempt may not work--let's see if SUPLRECS
  !   and ARRAY can be space delimited...

  !...SPACE DELIMITING DID NOT WORK THIS WAY...
  !...SO, I'll put variable ARRAY in columns 31-40

  !      READ (IUNIT,*,END=2530) SUPLRECS,ARRAY
  read (IUNIT,'(A26,T31,F10.3)',end = 2530) SUPLRECS,ARRAY
  if (SUPLRECS(1:1) .eq. '*') goto 2520
  if (INDEX(SUPLRECS,'-999').gt.0) goto 2530
  WTCNT = WTCNT+1
  if (WTCNT.gt.MXSTND) then
     WTCNT = MXSTND
     WRITE(JOPPRT,'(/T13,''MAX STANDS EXCEEDED; DATA '',             &
          &         ''IGNORED: '',A26,2X,7F9.2)') SUPLRECS,ARRAY
     if (IUNIT.eq.IREAD) IRECNT = IRECNT + 1
     goto 2520
  end if

  WTSTDS(WTCNT) = SUPLRECS
  WTSC(WTCNT) = ARRAY(1)
  goto 2520

  2530 continue

  if (IUNIT.eq.IREAD) IRECNT = IRECNT+WTCNT+1
  if (LKECHO)write(JOPPRT,'(/T13,''RECORDS PROCESSED= '',I4)') WTCNT

  goto 10

  2600 continue
  !                        OPTION NUMBER 16 -- REPRODN

  !     This keyword sets the reproduction rate for BKP, used when beetles are
  !     emerging from killed (or strip-killed) trees.

  IBSP = PBSPEC
  if (LNOTBK(1)) IBSP = INT(ARRAY(1))
  if (LNOTBK(2)) REPLAC = ARRAY(2)
  if (LNOTBK(3)) REPMAX = ARRAY(3)
  if (LNOTBK(4)) DBHMAX = ARRAY(4)

  RSLOPE = (REPMAX - 1.0) / (DBHMAX - REPLAC)
  B = 1.0 - (REPLAC * RSLOPE)

  LOW = 0
  do I = 1,NSCL
     if (I .gt.1) LOW = UPSIZ(I-1)
     DBHMID = (UPSIZ(I) + LOW) / 2.0

     if (DBHMID .lt. DBHMAX) then
        INC(IBSP,I) = (RSLOPE * DBHMID) + B
     else
        INC(IBSP,I) = REPMAX
     end if

     !     CHECK FOR NEGATIVE VALUES (WHICH CAN HAPPEN IN SMALL SCs) AJM 9/05

     if (INC(IBSP,I).lt.0.01) INC(IBSP,I) = 0.01

     2610 continue
  end do

  if (LKECHO)write(JOPPRT, 2650) KEYWRD,IBSP,REPLAC,RSLOPE,&
       &REPMAX,DBHMAX
  2650 format(/1X,A8,'   FOR BEETLE TYPE ',I4,' REPRODUCTION WILL ',&
       &'OCCUR WITH BMULT=1 AT DBH=',F6.2,/T13,'SLOPE=',F6.3,&
       &' AND MAXIMUM AT BMULT=',F5.2,' AND DBH=',F6.2)

  goto 10

  2700 continue

  !                        OPTION NUMBER 17 -- BKPKILL

  !     This keyword is used to invoke climatic events which affect
  !     how the beetle reproduces once inside the tree. It affects
  !     the amount of beetles emerging from the tree and the amount of
  !     beetles in the outside world.

  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  NPARMS = 4
  PRMS(1) = 1.0
  PRMS(2) = PBSPEC
  if (PBSPEC .lt.3) PRMS(3) = 0.35
  if (PBSPEC .eq.3) PRMS(3) = 0.035
  PRMS(4) = 0.035
  if (LNOTBK(2)) PRMS(1) = ARRAY(2)
  if (LNOTBK(3)) PRMS(2) = ARRAY(3)
  if (LNOTBK(4)) PRMS(3) = ARRAY(4)
  if (LNOTBK(5)) PRMS(4) = ARRAY(5)

  call GPNEW (KODE,IDT,317,NPARMS,PRMS)
  if (KODE.gt.0) then
     call KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
     write(JOPPRT,2705) KEYWRD
     2705 format(/1X,A8,' Unable to set options')
     goto 2780
  end if

  ! Note the use of a special stand id 'MXSTND' to hold landscape level global
  ! parms. The special ID is picked up in the relevant routine.

  call GPADSD(MXSTND)
  call GPCLOS(317)

  if (LKECHO)write(JOPPRT, 2750) KEYWRD,IDT,INT(PRMS(1)),&
       &INT(PRMS(2)),PRMS(3), PRMS(4)
  2750 format(/1X,A8,'   IN YEAR/CYCLE ',I4,' FOR ',I4,' YEARS',&
       &' AND FOR BEETLE TYPE ',I4,&
       &' REPRODUCTION WILL BE ',F6.3,' OR (IF TYPE 4)',F6.3)

  2780 continue

  goto 10

  2800 continue
  !                        OPTION NUMBER 18 -- LIGHTN (Lightning)

  !     Landscape level, Lightning is always on unless user specifies a
  !     strike densiyt=0.  Keyword allows users to change strike density
  !     in different years (or to turn lightning off)

  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  NPARMS = 2
  PRMS(1) = 1.0
  PRMS(2) = DENS

  if (LNOTBK(2)) PRMS(2) = MAX(0.0, ARRAY(2))

  call GPNEW (KODE,IDT,306,NPARMS,PRMS(1))
  if (KODE.gt.0) then
     call KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
     write(JOPPRT,2805) KEYWRD
     2805 format(/1X,A8,' Unable to set options')
     goto 2830
  end if

  ! Note the use of a special stand id 'MXSTND' to hold landscape level global
  ! parms. The special ID is picked up in the relevant routine.

  call GPADSD(MXSTND)
  call GPCLOS(306)

  if (LKECHO)write(JOPPRT, 2810) KEYWRD, IDT, IFIX(PRMS(1)), PRMS(2)
  2810 format(/1X,A8,'   LIGHTNING STRIKE DENSITY WILL CHANGE IN ',&
       &'DATE/CYCLE ',I4, ' END AFTER ',I4, ' YEARS,',&
       &/T13,'AND HAVE A STRIKE DENSITY OF ',F8.5,' TREES/ACRE.')

  2830 continue

  goto 10
  2900 continue
  !                        OPTION NUMBER 19 -- IPSDV

  !     Turn Ips on as a driving variable (IPSON=true) and set the min and max
  !     size classes for killing. Size classes can be set here even if Ips is the
  !     main beetle species. Also, set the number of generations for Ips

  if (PBSPEC .ne.3) IPSON = .true.

  if (LNOTBK(1)) then
     MINSZ = IFIX(ARRAY(1))
     do I = 1,NSCL
        if (MINSZ .lt. UPSIZ(I)) then
           IPSMIN = I
           goto 2902
        end if
        if (I .eq. NSCL) IPSMIN = NSCL
        2901 continue
     end do
     2902 continue
  end if

  if (LNOTBK(2)) then
     MAXSZ = IFIX(ARRAY(2))
     do I = 1,NSCL
        if (MAXSZ .lt. UPSIZ(I)) then
           IPSMAX = I
           goto 2904
        end if
        if (IPSMAX .eq.0) IPSMAX = NSCL
        2903 continue
     end do
     2904 continue
  end if

  if (LNOTBK(3)) NIBGEN = IFIX(ARRAY(3))
  if (LNOTBK(4)) PFSLSH = ARRAY(4)

  if (IPSON) then
     if (LKECHO)write(JOPPRT,2910) KEYWRD
     2910 format (/1X,A8,'   IPS IS A DRIVING VARIABLE',I5)
  end if
  if (LKECHO)write(JOPPRT,2915)KEYWRD,IPSMIN,IPSMAX,NIBGEN,PFSLSH
  2915 format (/1X,A8,'   MINIMUM SIZE CLASS IPS ATTACKS WILL BE',I5,&
       &'   MAXIMUM SIZE CLASS IPS CAN KILL WILL BE',I5,&
       &'   NUMBER OF IPS GENERATIONS WILL BE', I5,&
       &'   PROPORTION OF SLASH TO FILL FIRST IS ', F4.2)

  goto 10
  3000 continue
  !                        OPTION NUMBER 20 -- WINDTHROW (QWINDTH)

  !     Landscape level, ON in year 'IDT'; stays ON for specified duration

  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  NPARMS = 3
  PRMS(1) = DEFYRS
  PRMS(2) = 8.0
  PRMS(3) = WTHRSH
  PRMS(4) = CRASH
  if (LNOTBK(2)) PRMS(1) = ARRAY(2)
  if (LNOTBK(3)) PRMS(2) = ARRAY(3)
  if (LNOTBK(4)) PRMS(3) = ARRAY(4)
  if (LNOTBK(5)) PRMS(4) = ARRAY(5)
  PRMS(4) = AMIN1(PRMS(4),1.0)

  call GPNEW (KODE,IDT,305,NPARMS,PRMS)
  if (KODE.gt.0) then
     call KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
     write(JOPPRT,3005) KEYWRD
     3005 format(/1X,A8,' Unable to set options')
     goto 3030
  end if

  if (LKECHO)write(JOPPRT,3010) KEYWRD, IDT, (PRMS(I), I = 1,4)
  3010 format(/1X,A8,'   Landscape windthrow to be attempted ',&
       &'beginning in date/cycle:', I4,' and lasting', F4.0,&
       &' years.',/T13,'Minimum height to be thrown:',F6.1,&
       &' feet. Minimum number of eligible stems/acre:',&
       &F6.0,/T13,'Proportion to throw: ',F4.2)

  ! Note the use of stand ID MXSTND, which is a dummy for the use of the keyword for
  ! the entire landscape. Any 1:MXSTND value could be used.

  call GPADSD(MXSTND)
  call GPCLOS(305)

  3030 continue
  goto 10

  !                        OPTION NUMBER 21 -- QRROT

  !     Read in the parameters associated with quick root disease model

  3100 continue
  if (LNOTBK(1)) BMRRSR = ARRAY(1)
  if (BMRRSR .lt.0.0) BMRRSR = 0.0
  if (BMRRSR .gt.1.0) BMRRSR = 1.0
  !      IF (BMRRSR .EQ. 0.0) LRRON = .FALSE.
  if (BMRRSR .gt.0) LRRON = .true.
  if (LKECHO)write(JOPPRT,3150) KEYWRD, BMRRSR
  3150 format (/1X,A8,'   QUICK ROOT DISEASE: ANNUAL SPREAD RATE',&
       &'(0-1) FOR TREES INFECTED AT INVENTORY WILL BE', F4.2)
  goto 10

  !                        OPTION NUMBER 22 -- QSRUST

  !     Read in the parameters associated with quick stem rust model

  3200 continue
  if (LNOTBK(1)) BMSRSR = ARRAY(1)
  if (BMSRSR .lt.0.0) BMSRSR = 0.0
  if (BMSRSR .gt.1.0) BMSRSR = 1.0
  !      IF (BMSRSR .EQ. 0.0) LSRON = .FALSE.
  if (BMSRSR .gt.0) LSRON = .true.

  if (LKECHO)write(JOPPRT,3250) KEYWRD, BMSRSR
  3250 format (/1X,A8,'   QUICK STEM RUST: ANNUAL SPREAD RATE',&
       &'(0-1) FOR TREES INFECTED AT INVENTORY WILL BE', F4.2)
  goto 10

  !                        OPTION NUMBER 23 -- OTHERBB

  !     This is loosely based on the Type 3 bark beetles in the root disease models.
  !     Parameters are all initialized here (maybe should be done in a common block?)
  !     and are somewhat based on the defaults used in the Annosus model.

  !     On in year specified, for specified number of years, but attacks only occur if
  !     conditions are right.

  3300 continue
  IDT = 1
  if (LNOTBK(1)) IDT = INT(ARRAY(1))

  NPARMS = 6
  PRMS(1) = DEFYRS
  PRMS(2) = 4.0
  PRMS(3) = 10.0
  PRMS(4) = 0.75
  PRMS(5) = 0.7

  if (LNOTBK(2)) PRMS(1) = ARRAY(2)
  if (LNOTBK(3)) then
     TEMP = ARRAY(3)
     PRMS(2) = 0.0
  end if
  if (LNOTBK(4)) PRMS(3) = ARRAY(4)
  if (LNOTBK(5)) PRMS(4) = ARRAY(5)
  if (LNOTBK(6)) PRMS(5) = ARRAY(6)

  ISIZ = 0
  3310 continue
  if ((PRMS(2) .eq.0.0) .and. (ISIZ .lt. NSCL)) then
     ISIZ = ISIZ + 1
     if (TEMP .le. UPSIZ(ISIZ)) PRMS(2) = ISIZ
     goto 3310
  else
     if (PRMS(2) .eq.0.0) PRMS(2) = NSCL
     goto 3315
  end if
  3315 continue

  call GPNEW (KODE,IDT,323,NPARMS,PRMS)
  if (KODE.gt.0) then
     call KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
     write(JOPPRT,3320) KEYWRD
     3320 format(/1X,A8,' Unable to set options')
     goto 3330
  end if

  if (LKECHO)write(JOPPRT, 3340) KEYWRD,IDT,INT(PRMS(1)),&
       &(PRMS(I),I = 2,5)
  3340 format(/1X,A8,'   OTHER BARK BEETLES TO BE ATTEMPTED IN DATE',&
       &'/CYCLE=',I5,' AND LASTING', I4, ' YEARS; MIN SIZE CLASS=',F4.0,&
       &/T13,'MINIMUM ELIGIBLE STEMS FOR EVENT= ',F7.1, ' STEMS/ACRE; ',&
       &'ATTACK RATE= ',F5.3, /T13,'MAXIMUM RV= ',F4.2)

  ! Note the use of stand ID MXSTND, which is a dummy for the use of the keyword for
  ! the entire landscape. Any 1:MXSTND value could be used.

  call GPADSD(MXSTND)
  call GPCLOS(323)

  3330 continue
  goto 10

  3400 continue
  !                        OPTION NUMBER 24 -- "Quick" Mortality model (QMORT)

  !     Keyword that causes increasing mortality on host or non-host trees.

  IDT = 1
  if (LNOTBK(1)) IDT = INT(ARRAY(1))

  NPARMS = 6
  PRMS(1) = DEFYRS
  PRMS(2) = 2.0
  PRMS(3) = 0.0
  PRMS(4) = 0.0
  PRMS(5) = 0.5
  TEMP = 0.0
  TEMP1 = 100.0
  if (LNOTBK(2)) PRMS(1) = ARRAY(2)
  if (LNOTBK(3)) PRMS(2) = ARRAY(3)
  if (LNOTBK(4)) TEMP = ARRAY(4)
  if (LNOTBK(5)) TEMP1 = ARRAY(5)
  if (LNOTBK(6)) PRMS(5) = ARRAY(6)
  PRMS(6) = PRMS(1)

  ISIZ = 0
  3410 continue
  if (((PRMS(3) .eq.0.0) .or. (PRMS(4) .eq.0.0))&
       &.and. (ISIZ .lt. NSCL)) then
     ISIZ = ISIZ + 1
     if (TEMP .le. UPSIZ(ISIZ) .and. PRMS(3) .eq.0.0)&
          &PRMS(3) = ISIZ
     if (TEMP1.le. UPSIZ(ISIZ) .and. PRMS(4) .eq.0.0)&
          &PRMS(4) = ISIZ
     goto 3410
  else
     if (PRMS(3) .eq.0.0) PRMS(3) = NSCL
     if (PRMS(4) .eq.0.0) PRMS(4) = NSCL
     goto 3415
  end if

  3415 continue

  call GPNEW (KODE,IDT,307,NPARMS,PRMS)
  if (KODE.gt.0) then
     call KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
     write(JOPPRT,3406) KEYWRD
     3406 format(/1X,A8,'WARNING: Unable to set options')
     goto 3470
  end if
  ! Note the use of stand ID MXSTND, which is a dummy for the use of the keyword for
  ! the entire landscape. Any 1:MXSTND value could be used.

  call GPADSD(MXSTND)
  call GPCLOS(307)

  if (LKECHO)write(JOPPRT, 3450) KEYWRD, IDT, (PRMS(I), I = 1,5)
  3450 format(/1X,A8,'   Extra mortality to start in date/cycle ', I5,&
       &' and will last ',F4.0,' years;  Species =',F3.0,&
       &';  Min siz class=',F4.0,/T13,'Max size class= ',&
       &F4.0,' Total mortality= ',F5.2)

  3470 continue

  goto 10
  3500 continue
  !                        OPTION NUMBER 25 -- RPHERO (Repelling pheromone)

  !     Apply repelling pheromone in year 'IDT' to stand 'NNN'.

  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  IUNIT = IREAD
  if (LNOTBK(2)) IUNIT = IFIX(ARRAY(2))

  NPARMS = 1
  PRMS(1) = 75
  if (LNOTBK(3)) PRMS(1) = ARRAY(3)
  PRMS(1) = AMIN1(PRMS(1),100.0)
  PRMS(1) = AMAX1(PRMS(1),0.0)

  if (NOSTND.le.0) then
     write(JOPPRT,3504) KEYWRD
     3504 format(/1X,A8,' Stands not yet read; ',&
          &'Keyword MUST follow inventory')
     goto 3540
  end if

  if (LKECHO)write(JOPPRT,3510) KEYWRD, IDT, PRMS(1), IUNIT
  3510 format(/1X,A8,'   REPELLING pheromone applied in year ', I5,&
       &'; with efficiency =',F5.2,'%. Stands read from logical ',&
       &'unit ',I4)

  call C26SRT (NOSTND,STDIDS,MYLST1,.true.)

  !     Read the input stand IDs, find their location in BMSTDS, and assign
  !     that position to MYLST2.

  J = 0

  ! 3520 CONTINUE
  !      READ(IUNIT,'(A8)',END=3530) KEYWRD
  !      IF (IUNIT.EQ.IREAD) IRECNT= IRECNT + 1
  !      IF (INDEX(KEYWRD,'-999') .GT. 0) GOTO 3530
  !      CALL C26BSR (NOSTND, STDIDS, MYLST1, KEYWRD, IP)
  !      IF (IP .GT. 0) THEN
  !        J = J + 1
  !        IF (J .LE. MXSTND) THEN
  !          MYLST2(J) = IP
  !          IF(LKECHO)WRITE(JOPPRT,3525) KEYWRD, IP
  ! 3525     FORMAT(T13,'STAND ID: ',A8,'   position ',I4)

  !...Fix for 26-character supplemental records.  AJM 3/28/00

  3520 continue
  read(IUNIT,*,end = 3530) SUPLRECS
  if (SUPLRECS(1:1) .eq. '*') goto 3520
  if (INDEX(SUPLRECS,'-999') .gt.0) goto 3530
  if (IUNIT.eq.IREAD) IRECNT = IRECNT + 1
  call C26BSR (NOSTND, STDIDS, MYLST1, SUPLRECS, IP)
  if (IP .gt.0) then
     J = J + 1
     if (J .le. MXSTND) then
        MYLST2(J) = IP
        if (LKECHO)write(JOPPRT,3525) SUPLRECS, IP
        3525 format(T13,'STAND ID: ',A26,'   position ',I4)
     end if
  end if
  goto 3520
  3530 continue

  !     Apply the list to the global option list.

  call GPADD (KODE, IDT, 302, NPARMS, PRMS(1), J, MYLST2)
  if (KODE.gt.0) then
     call KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
     write(JOPPRT,3505) KEYWRD
     3505 format(/1X,A8,' No room to set option.')
     goto 3540
  end if

  call GPCLOS(302)
  3540 continue

  goto 10

  3600 continue
  !                        OPTION NUMBER 26 -- APHERO (Attracting pheromone)

  !     Apply attracting pheromone in year 'IDT' to stand 'NNN'.

  if (NOSTND .eq.0) then
     write(JOPPRT,3604) KEYWRD
     3604 format(/1X,A8,' Stands not yet read; ',&
          &'Keyword MUST follow inventory.')
     goto 3640
  end if

  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  IUNIT = IREAD
  if (LNOTBK(2)) IUNIT = IFIX(ARRAY(2))

  NPARMS = 1
  PRMS(1) = 75
  if (LNOTBK(3)) PRMS(1) = ARRAY(3)
  PRMS(1) = AMIN1(PRMS(1),100.0)
  PRMS(1) = AMAX1(PRMS(1),0.0)

  if (LKECHO)write(JOPPRT,3610) KEYWRD, IDT, PRMS(1), IUNIT
  3610 format(/1X,A8,'   ATTRACTING pheromone applied in year ', I5,&
       &'; with efficiency =',F5.2,'%. Stands read from logical ',&
       &'unit ',I4)

  !     Sort the STDIDS vector and return MYLST1 as its sorted order.

  call C26SRT (NOSTND,STDIDS,MYLST1,.true.)

  !     Read the input stand IDs, find their location in BMSTDS, and assign
  !     that position to MYLST2.

  J = 0

  ! 3620 CONTINUE
  !      READ(IUNIT,'(A8)',END=3630) KEYWRD
  !      IF (IUNIT.EQ.IREAD) IRECNT= IRECNT + 1
  !      IF (INDEX(KEYWRD,'-999') .GT. 0) GOTO 3630
  !      CALL C26BSR (NOSTND, STDIDS, MYLST1, KEYWRD, IP)
  !      IF (IP .GT. 0) THEN
  !        J = J + 1
  !        IF (J .LE. MXSTND) THEN
  !          MYLST2(J) = IP
  !          IF(LKECHO)WRITE(JOPPRT,3625) KEYWRD, IP
  ! 3625     FORMAT(T13,'STAND ID: ',A8,'   position ',I4)

  !...Fix for 26-character supplemental records.  AJM 3/28/00

  3620 continue
  read(IUNIT,*,end = 3630) SUPLRECS
  if (SUPLRECS(1:1) .eq. '*') goto 3620
  if (INDEX(SUPLRECS,'-999') .gt.0) goto 3630
  if (IUNIT.eq.IREAD) IRECNT = IRECNT + 1
  call C26BSR (NOSTND, STDIDS, MYLST1, SUPLRECS, IP)
  if (IP .gt.0) then
     J = J + 1
     if (J .le. MXSTND) then
        MYLST2(J) = IP
        if (LKECHO)write(JOPPRT,3625) SUPLRECS, IP
        3625 format(T13,'STAND ID: ',A26,'   position ',I4)
     end if
  end if
  goto 3620
  3630 continue

  !     Apply the list to the global option list.

  call GPADD (KODE, IDT, 303, NPARMS, PRMS(1), J, MYLST2)
  if (KODE.gt.0) then
     call KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
     write(JOPPRT,3605) KEYWRD
     3605 format(/1X,A8,' No room to set option.')
     goto 3640
  end if

  call GPCLOS(303)

  3640 continue

  goto 10

  3700 continue
  !                        OPTION NUMBER 27 -- SPRAY (Pesticides)

  !     Apply pesticide in year 'IDT' to stands listed by the user.

  if (NOSTND .eq.0) then
     write(JOPPRT,3704) KEYWRD
     3704 format(/1X,A8,' Stands not yet read; ',&
          &'Keyword MUST follow inventory.')
     goto 3640
  end if

  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
  IUNIT = IREAD
  if (LNOTBK(2)) IUNIT = IFIX(ARRAY(2))

  NPARMS = 1
  if (LNOTBK(3)) PRMS(1) = ARRAY(3)

  if (LKECHO)write(JOPPRT, 3750) KEYWRD,IDT,PRMS(1),IUNIT
  3750 format(/1X,A8,'   Pesticide will be applied in date/cycle ', I5,&
       &'  Prop of trees in stand sprayed =',F4.2,/T13,'Stands read from',&
       &' logical unit ',I4)

  call C26SRT (NOSTND,STDIDS,MYLST1,.true.)

  J = 0
  3720 continue

  !      READ(IUNIT,'(A8)',END=3730) KEYWRD
  !      IF (IUNIT.EQ.IREAD) IRECNT= IRECNT + 1
  !      IF (INDEX(KEYWRD,'-999') .GT. 0) GOTO 3730
  !      CALL C26BSR (NOSTND,STDIDS,MYLST1,KEYWRD,IP)
  !      IF (IP .GT. 0) THEN
  !        J = J + 1
  !        IF (J .LE. MXSTND) THEN
  !          MYLST2(J) = IP
  !          IF(LKECHO)WRITE(JOPPRT,3725) KEYWRD, IP
  ! 3725     FORMAT(T13,'STAND ID: ',A8,'   position ',I4)

  !...Fix for 26-character supplemental records.  AJM 3/28/00

  read(IUNIT,*,end = 3730) KEYWRD
  if (SUPLRECS(1:1) .eq. '*') goto 3720
  if (INDEX(SUPLRECS,'-999') .gt.0) goto 3730
  if (IUNIT.eq.IREAD) IRECNT = IRECNT + 1
  call C26BSR (NOSTND,STDIDS,MYLST1,SUPLRECS,IP)
  if (IP .gt.0) then
     J = J + 1
     if (J .le. MXSTND) then
        MYLST2(J) = IP
        if (LKECHO)write(JOPPRT,3725) SUPLRECS, IP
        3725 format(T13,'STAND ID: ',A26,'   position ',I4)
     end if
  end if
  goto 3720
  3730 continue

  call GPADD (KODE, IDT, 304, NPARMS, PRMS(1), J, MYLST2)
  if (KODE.gt.0) then
     call KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
     write(JOPPRT,3705) KEYWRD
     3705 format(/1X,A8,' No room to set option.')
     goto 3740
  end if

  call GPCLOS(304)
  3740 continue

  goto 10

  3800 continue
  !                        OPTION NUMBER 28 -- SALVAGE

  !     Salvage removes dead standing wood. User must specify the stand(s) that will be salvaged.

  !     Modified the keyword input array to correspond with the SANITIZE
  !     keyword and to consider minimum volume for action on a stand
  !     (RNH May98)

  if (NOSTND .eq.0) then
     write(JOPPRT,3804) KEYWRD
     3804 format(/1X,A8,' Stands not yet read; ',&
          &'Keyword MUST follow inventory.')
     goto 3840
  end if

  !     Set Defaults

  IDT = 1
  IUNIT = IREAD
  !      NPARMS= 4

  !     With Min. Vol. of Salvage number of Parameters is 5 (RNH May 98)
  NPARMS = 5
  TEMP = 10.0
  TEMP1 = 60.0
  TEMP2 = 2.0

  !     Default Min. Vol. for Salvage set to 10.0 (cf/acre) (RNH May98)

  PRMS(4) = 10.0

  !     Efficiency in now 5th parameter in field 7 (RNH May 98)
  !      PRMS(4)= 0.8

  PRMS(5) = 0.8

  !     Set user defined variables

  if (LNOTBK(1)) IDT = INT(ARRAY(1))
  if (LNOTBK(2)) IUNIT = INT(ARRAY(2))
  if (LNOTBK(3)) TEMP = ARRAY(3) !  Min. DBH
  if (LNOTBK(4)) TEMP1 = ARRAY(4) !  Max. DBH
  if (LNOTBK(5)) TEMP2 = ARRAY(5) !  Max. age, dead wood

  !     Min. Vol. is set to 4th parameter and Field 6 (RNH May 98)
  if (LNOTBK(6)) PRMS(4) = ARRAY(6)

  !     Efficiency is 5th parameter inField 7 (RNH May 98)
  if (LNOTBK(7)) PRMS(5) = ARRAY(7)

  PRMS(1) = 0.0
  PRMS(2) = 0.0
  PRMS(3) = 0.0

  !     DETERMINE WHICH SIZE CLASS CONTAINS THE MAX AND MIN DBH
  !         classes are (for SDWP): <3, <10, <20, <60
  if (TEMP .le.3.0) PRMS(1) = 1
  if (TEMP1.le.3.0) PRMS(2) = 1

  ISIZ = 0
  3810 continue
  if (((PRMS(1) .eq.0.0) .or. (PRMS(2) .eq.0.0))&
       &.and. (ISIZ .lt. MXDWHZ)) then
     ISIZ = ISIZ + 1
     if (TEMP .le. WPSIZ(ISIZ) .and. PRMS(1) .eq.0.0)&
          &PRMS(1) = ISIZ + 1
     if (TEMP1.le. WPSIZ(ISIZ) .and. PRMS(2) .eq.0.0)&
          &PRMS(2) = ISIZ + 1
     goto 3810
  else
     if (PRMS(1) .eq.0.0) PRMS(1) = MXDWHZ + 1
     if (PRMS(2) .eq.0.0) PRMS(2) = MXDWHZ + 1
     goto 3815
  end if
  3815 continue

  !     DETERMINE WHICH AGE CLASS CONTAINS THE MAXIMUM AGE
  !         classes are: <1, 1-2, 2-5, 5-10, >10
  if (TEMP2.lt.1.0) then
     PRMS(3) = 1
  else if (TEMP2.lt.2.0) then
     PRMS(3) = 2
  else if (TEMP2.lt.5.0) then
     PRMS(3) = 3
  else if (TEMP2.lt.10.0) then
     PRMS(3) = 4
  else
     PRMS(3) = 5

  end if

  !     Change input echo to correspond to new input data fields (RNH May 98)

  if (LKECHO)write(JOPPRT, 3850) KEYWRD,IDT,TEMP,TEMP1,TEMP2,&
       &PRMS(4),PRMS(5)
  3850 format(/1X,A8,'   SALVAGE WILL OCCUR IN', I5,&
       &' MINIMUM DBH FOR SALVAGE=', F6.2,&
       &' MAXIMUM DBH=',F6.2,T13,'MAX AGE FOR SALVAGE= ',F8.2,&
       &/T13,'MIN VOL. FOR SALVAGE=',F8.0,' EFFICIENCY =',F4.2)

  !     Sort the STDIDS vector and return MYLST1 as its sorted order.

  call C26SRT (NOSTND,STDIDS,MYLST1,.true.)

  !     Read the input stand IDs, find their location in BMSTDS, and assign
  !     that position to MYLST2.

  J = 0

  ! 3820 CONTINUE
  !      READ(IUNIT,'(A8)',END=3830) KEYWRD
  !      IF (IUNIT.EQ.IREAD) IRECNT= IRECNT + 1
  !      IF (INDEX(KEYWRD,'-999') .GT. 0) GOTO 3830
  !      CALL C26BSR (NOSTND, STDIDS, MYLST1, KEYWRD, IP)
  !      IF (IP .GT. 0) THEN
  !        J = J + 1
  !        IF (J .LE. MXSTND) THEN
  !          MYLST2(J) = IP
  !          IF(LKECHO)WRITE(JOPPRT,3825) KEYWRD, IP
  ! 3825     FORMAT(T13,'STAND ID: ',A8,'   position ',I4)

  !...Fix for 26-character supplemental records.  AJM 3/28/00

  3820 continue
  read(IUNIT,*,end = 3830) SUPLRECS
  if (SUPLRECS(1:1) .eq. '*') goto 3820
  if (INDEX(SUPLRECS,'-999') .gt.0) goto 3830
  if (IUNIT.eq.IREAD) IRECNT = IRECNT + 1
  call C26BSR (NOSTND, STDIDS, MYLST1, SUPLRECS, IP)
  if (IP .gt.0) then
     J = J + 1
     if (J .le. MXSTND) then
        MYLST2(J) = IP
        if (LKECHO)write(JOPPRT,3825) SUPLRECS, IP
        3825 format(T13,'STAND ID: ',A26,'   position ',I4)
     end if
  end if
  goto 3820
  3830 continue

  !     Apply the list to the global option list.

  call GPADD (KODE, IDT, 318, NPARMS, PRMS(1), J, MYLST2)
  if (KODE.gt.0) then
     call KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
     write(JOPPRT,3805) KEYWRD
     3805 format(/1X,A8,' No room to set option.')
     goto 3840
  end if

  call GPCLOS(318)

  3840 continue

  goto 10
  3900 continue
  !                        OPTION NUMBER 29 -- SANITIZE

  !     Currently this is landscape level (like the driving variables) so the user
  !     specifies a year and threshold and any stands that meet the threshold criteria
  !     in that year will have a sanitation cut performed.  We may want to change this
  !     to be like the pheromones and have the user specify a stand to cut (instead of
  !     a threshold).

  !     Changed SANITIZE keyword to stand level (RNH May98)

  !     From SALVAGE

  if (NOSTND .eq.0) then
     write(JOPPRT,3814) KEYWRD
     3814 format(/1X,A8,' Stands not yet read; ',&
          &'Keyword MUST follow inventory.')
     goto 3841
  end if

  !     End from SALVAGE
  !     Changed the keyword read such that SALVAGE and SANITIZE have
  !     consistent keyword fields.  Added IUNIT=IREAD (RNH (May 98)

  !     Default variable values

  IDT = 1
  IUNIT = IREAD
  NPARMS = 5
  TEMP = 6.0
  TEMP1 = 999.
  PRMS(3) = 0.0
  !      PRMS(3)= 0.8
  PRMS(4) = 10.0
  PRMS(5) = 0.95

  !     From Salvage (RNH May 98)
  !     User defined variable values

  if (LNOTBK(1)) IDT = INT(ARRAY(1))
  if (LNOTBK(2)) IUNIT = INT(ARRAY(2)) !  Optional input file for stand list

  !     Moved keyword parameter read over one field (RNH May 98)

  if (LNOTBK(3)) TEMP = ARRAY(3) !  Min. DBH
  if (LNOTBK(4)) TEMP1 = ARRAY(4) !  Max. DBH
  if (LNOTBK(5)) PRMS(3) = ARRAY(5) !  Max. rating value (RV)
  if (LNOTBK(6)) PRMS(4) = ARRAY(6) !  Min. elibible Vol.
  if (LNOTBK(7)) PRMS(5) = ARRAY(7) !  Efficiency of Sanitation

  !     Number of parameters remains the same as in old Sanitize keyword

  PRMS(1) = 0.0
  PRMS(2) = 0.0
  if (TEMP .le.3.0) PRMS(1) = 1
  if (TEMP1.le.3.0) PRMS(2) = 1

  !     From SALVAGE

  !     DETERMINE WHICH SIZE CLASS CONTAINS THE MAX AND MIN DBH
  !         classes are (for SDWP): <3, <10, <20, <60
  if (TEMP .le.3.0) PRMS(1) = 1
  if (TEMP1.le.3.0) PRMS(2) = 1

  !     End from SALVAGE

  ISIZ = 0
  3910 continue
  if (((PRMS(1) .eq.0.0) .or. (PRMS(2) .eq.0.0))&
       &.and. (ISIZ .lt. NSCL)) then
     ISIZ = ISIZ + 1
     if (TEMP .le. UPSIZ(ISIZ) .and. PRMS(1) .eq.0.0)&
          &PRMS(1) = ISIZ
     if (TEMP1.le. UPSIZ(ISIZ) .and. PRMS(2) .eq.0.0)&
          &PRMS(2) = ISIZ
     goto 3910
  else
     if (PRMS(1) .eq.0.0) PRMS(1) = NSCL
     if (PRMS(2) .eq.0.0) PRMS(2) = NSCL
     goto 3915
  end if
  3915 continue

  !     From SALVAGE

  !      IF(LKECHO)WRITE(JOPPRT, 3950) KEYWRD,IDT,TEMP,TEMP1,(PRMS(I),I=3,5)
  ! 3950 FORMAT(/1X,A8,' SANITATION WILL OCCUR AT ', I5,
  !     >  ' MINIMUM DBH FOR SANITATION= ', F6.2,
  !     >  ' MAXIMUM DBH= ',F6.2,' MAX. RV TO REMOVE= ',F5.2,
  !     >  /T13,'MIN VOL. FOR SANITATION=',F8.0,' EFFICIENCY =',F4.2)

  !     Change input echo to correspond to new input data fields (RNH May 98)

  if (LKECHO)write(JOPPRT, 3950) KEYWRD,IDT,TEMP,TEMP1,&
       &(PRMS(I),I = 3,5)
  3950 format(/1X,A8,' SANITATION WILL OCCUR AT ', I5,&
       &' MINIMUM DBH FOR SANITATION= ', F6.2,&
       &' MAXIMUM DBH= ',F6.2,' MAX. RV TO REMOVE= ',F5.2,&
       &/T13,'MIN VOL. FOR SANITATION=',F8.0,' EFFICIENCY =',F4.2)

  !     Sort the STDIDS vector and return MYLST1 as its sorted order.

  call C26SRT (NOSTND,STDIDS,MYLST1,.true.)

  !     Read the input stand IDs, find their location in BMSTDS, and assign
  !     that position to MYLST2.

  J = 0
  !...Fix for 26-character supplemental records.  AJM 3/28/00

  3821 continue
  !      READ(IUNIT,'(A8)',END=3831) KEYWRD
  read(IUNIT,*,end = 3831) SUPLRECS
  !      READ(IUNIT,'(A8)',END=3831) SUPLRECS
  if (SUPLRECS(1:1) .eq. '*') goto 3821
  if (INDEX(SUPLRECS,'-999') .gt.0) goto 3831
  if (IUNIT.eq.IREAD) IRECNT = IRECNT + 1
  !      IF (INDEX(KEYWRD,'-999') .GT. 0) GOTO 3831

  !      CALL C26BSR (NOSTND, STDIDS, MYLST1, KEYWRD, IP)
  call C26BSR (NOSTND, STDIDS, MYLST1, SUPLRECS, IP)
  if (IP .gt.0) then
     J = J + 1
     if (J .le. MXSTND) then
        MYLST2(J) = IP
        !          IF(LKECHO)WRITE(JOPPRT,3826) KEYWRD, IP
        if (LKECHO)write(JOPPRT,3826) SUPLRECS, IP
        3826 format(T13,'STAND ID: ',A26,'   position ',I4)
     end if
  end if
  goto 3821
  3831 continue

  !     Apply the list to the global option list.

  !     Change glaobal list code to 308 for SANI

  call GPADD (KODE, IDT, 308, NPARMS, PRMS(1), J, MYLST2)
  if (KODE.gt.0) then
     call KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
     write(JOPPRT,3816) KEYWRD
     3816 format(/1X,A8,' No room to set option.')
     goto 3841
  end if

  !     End from SALVAGE
  !     Comment out teh following (RNH MAy98)

  !      CALL GPNEW (KODE,IDT,308,NPARMS,PRMS)
  !      IF (KODE.GT.0) THEN
  !        CALL KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
  !        IF(LKECHO)WRITE(JOPPRT,3906) KEYWRD
  ! 3906   FORMAT(/1X,A8,'WARNING: Unable to set options')
  !        GOTO 3960
  !      ENDIF

  !     End RNH

  !     Comment out GPADSD (RNH, may98)

  !      CALL GPADSD(MXSTND)
  call GPCLOS(308)

  3960 continue

  !     Loop out for SANI (RNH, MAy98)

  3841 continue
  goto 10
  4000 continue
  !                        OPTION NUMBER 30 -- OWVALUES: Outside World

  ! This keyword now sets all values of the outside world that can vary during a
  ! simulation (such as BAhost, BAspecial, BAtotal, BKP, RV, #special). This may
  ! also be used with the keyword WORLD which sets the type of outside world.
  ! If any fields are blank, the average initial conditions will be used.

  NPARMS = 6
  do I = 1,NPARMS
     !         Set PRMS to -1 to indicate that the user did not specify values.
     PRMS(I) = -1
     4005 continue
  end do

  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  !     total bkp
  if (LNOTBK(2)) PRMS(1) = ARRAY(2)
  !     basal area of host above min size
  if (LNOTBK(3)) PRMS(2) = ARRAY(3)
  !     basal area of special trees
  if (LNOTBK(4)) PRMS(3) = ARRAY(4)
  !     density of special trees
  if (LNOTBK(5)) PRMS(4) = ARRAY(5)
  !     total basal area
  if (LNOTBK(6)) PRMS(5) = ARRAY(6)
  !     rating value
  if (LNOTBK(7)) PRMS(6) = ARRAY(7)

  call GPNEW (KODE,IDT,310,NPARMS,PRMS)
  if (KODE.gt.0) then
     call KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
     write(JOPPRT,4006) KEYWRD
     4006 format(/1X,A8,'WARNING: Unable to set options')
     goto 4050
  end if

  if (LKECHO)write(JOPPRT,4010) KEYWRD, IDT, (PRMS(I), I = 1,6)
  4010 format(/1X,A8,'   Outside world parameters will change in ',I5,&
       &' BKP outside= ',F6.2,' sqft/acre.',/T13,&
       &'BA host= ',F7.2,' sqft/acre; Special trees: BA= ',F7.2,&
       &' sqft/acre and density= ',F7.2,/T13,'trees/acre. Total BA= ',&
       &F7.2,' sqft/acre; Rating value= ',F5.2,/T13,'NOTE: ',&
       &'-1 means that landscape average or previous year value used')

  call GPADSD(MXSTND)
  call GPCLOS(310)
  4050 continue

  goto 10
  4100 continue
  !                        OPTION NUMBER 31 -- OWTYPE (More outside world info)

  ! This keyword sets whether the outside world should be used, the type of
  ! outside world (floating or constant) and the % of the outside world that
  ! is non-stoackable.

  if (LNOTBK(1) .and. (IFIX(ARRAY(1)).eq.0)) OUTOFF = .true.
  if (LNOTBK(2)) UFLOAT = IFIX(ARRAY(2))
  if (LNOTBK(3)) STOCKO = 100.0 - ARRAY(3)
  if (STOCKO .lt.1.0) STOCKO = 1.0

  if (.not. OUTOFF) then
     if (UFLOAT .eq. -1) then
        if (LKECHO)write(JOPPRT,4110) KEYWRD, STOCKO
        4110 format(/1X,A8,'   Outside world values are always the ',&
             &'average of the landscape.  The outside is ',F4.0,&
             &'% stockable.')
     else
        if (LKECHO)write(JOPPRT,4115) KEYWRD, STOCKO
        4115 format(/1X,A8,'   Outside world values are always ',&
             &'constant (users can change them).  The outside is ',F4.0,&
             &'% stockable.')
     end if

     !       Change STOCKO into a proportion because that is what is used later
     STOCKO = STOCKO / 100.0
  else
     if (LKECHO)write(JOPPRT,4120) KEYWRD
     4120 format(/1X,A8,'   The outside world is NOT being simulated.')
  end if

  goto 10
  4200 continue
  !                        OPTION NUMBER 32 -- OWIPSDV: More Outside World

  !     This keyword supplements OWVALUES. It is used only for Ips as a driving
  !     variable, and not as a main beetle species.  Any blanks are assumed to
  !     be landscape averages as with OWVALUES. Note that there are fewer
  !     fields since we don't want the user re-specifiying values that can't
  !     differ (RV and total BA in the outside world).

  NPARMS = 4
  do I = 1,NPARMS
     !         Set PRMS to -1 to indicate that the user did not specify values.
     PRMS(I) = -1
     4205 continue
  end do

  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  if (LNOTBK(2)) PRMS(1) = ARRAY(2)
  if (LNOTBK(3)) PRMS(2) = ARRAY(3)
  if (LNOTBK(4)) PRMS(3) = ARRAY(4)
  if (LNOTBK(5)) PRMS(4) = ARRAY(5)

  call GPNEW (KODE,IDT,311,NPARMS,PRMS)
  if (KODE.gt.0) then
     call KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
     write(JOPPRT,4206) KEYWRD
     4206 format(/1X,A8,'WARNING: Unable to set options')
     goto 4250
  end if

  if (LKECHO)write(JOPPRT,4210) KEYWRD, IDT, (PRMS(I), I = 1,6)
  4210 format(/1X,A8,'   Ips outside world parameters will change in ',&
       &I5,' BKP outside= ',F6.2,' sqft/acre; BA host= ',F7.2,&
       &' sqft/acre;',/T13,'Special trees: BA= ',F7.2,&
       &' sqft/acre and density= ',F7.2,' trees/acre.',/T13,'NOTE: ',&
       &'-1 means that landscape average or previous year value used')

  if (.not. IPSON) write(JOPPRT,4211) KEYWRD
  4211 format(/1X,A8,'   WARNING: Ips may not be on as a driving ',&
       &'variable. Requested options will not be used.')

  call GPADSD(MXSTND)
  call GPCLOS(311)
  4250 continue

  goto 10
  4300 continue
  !                        OPTION NUMBER 33 -- QFIRE: Fire model.

  NPARMS = 2
  PRMS(1) = DEFYRS
  PRMS(2) = 3.0

  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  if (LNOTBK(2)) PRMS(1) = ARRAY(2)
  if (LNOTBK(3)) PRMS(2) = ARRAY(3)
  PRMS(2) = AMIN1(PRMS(2),4.0)
  PRMS(2) = AMAX1(PRMS(2),1.0)

  call GPNEW (KODE,IDT,313,NPARMS,PRMS)
  if (KODE.gt.0) then
     call KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
     write(JOPPRT,4306) KEYWRD
     4306 format(/1X,A8,'WARNING: Unable to set options')
     goto 4350
  end if

  if (LKECHO)write(JOPPRT,4310) KEYWRD,IDT,INT(PRMS(1)),INT(PRMS(2))
  4310 format(/1X,A8,'   Fire will be active in year/cycle ',I5,&
       &' with a period of ',I4,' years, using fuel ',&
       &' moisture type ',I4)

  call GPADSD(MXSTND)
  call GPCLOS(313)
  4350 continue

  goto 10
  4400 continue
  !                        OPTION NUMBER 34 -- QDEFOL: Defoliator model

  NPARMS = 4
  PRMS(1) = DEFYRS
  PRMS(2) = 0.25
  PRMS(3) = 0.0
  PRMS(4) = 0.0
  TEMP = 8.0
  TEMP1 = 30.0

  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  if (LNOTBK(2)) PRMS(1) = ARRAY(2)
  if (LNOTBK(3)) PRMS(2) = ARRAY(3)
  PRMS(2) = AMIN1(PRMS(2),1.0)
  PRMS(2) = AMAX1(PRMS(2),0.0)

  if (LNOTBK(4)) TEMP = ARRAY(4)
  if (LNOTBK(5)) TEMP1 = ARRAY(5)

  ISIZ = 0
  4410 continue
  if (((PRMS(3) .eq.0.0) .or. (PRMS(4) .eq.0.0))&
       &.and. (ISIZ .lt. NSCL)) then
     ISIZ = ISIZ + 1
     if (TEMP .le. UPSIZ(ISIZ) .and. PRMS(3) .eq.0.0)&
          &PRMS(3) = ISIZ
     if (TEMP1.le. UPSIZ(ISIZ) .and. PRMS(4) .eq.0.0)&
          &PRMS(4) = ISIZ
     goto 4410
  else
     if (PRMS(3) .eq.0.0) PRMS(3) = NSCL
     if (PRMS(4) .eq.0.0) PRMS(4) = NSCL
     goto 4415
  end if

  4415 continue

  call GPNEW (KODE,IDT,314,NPARMS,PRMS)
  if (KODE.gt.0) then
     call KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
     write(JOPPRT,4406) KEYWRD
     4406 format(/1X,A8,'WARNING: Unable to set options')
     goto 4450
  end if

  if (LKECHO)write(JOPPRT,4430) KEYWRD,IDT,INT(PRMS(1)),PRMS(2),&
       &INT(PRMS(3)),INT(PRMS(4))
  4430 format(/1X,A8,'   DEFOLIATORS WILL BE ACTIVE IN YEAR/CYCLE',I5,&
       &' FOR ',I4,' YEARS.',F6.2,' PROPORTION OF HOST TREES BETWEEN '&
       &/T13,'SIZE CLASS',I3,' AND',I3,' ARE MORE THAN 70% AFFECTED.')

  call GPADSD(MXSTND)
  call GPCLOS(314)
  4450 continue

  goto 10

  4500 continue
  !                        OPTION NUMBER 35 -- SLASHMGT

  !     Remove NEW downed dead wood in specified stands in specified years.

  if (NOSTND .eq.0) then
     write(JOPPRT,4504) KEYWRD
     4504 format(/1X,A8,' Stands not yet read; ',&
          &'Keyword MUST follow inventory.')
     goto 4540
  end if

  IDT = 1
  IUNIT = IREAD
  NPARMS = 2
  PRMS(1) = 0.0
  PRMS(2) = 0.8
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
  if (LNOTBK(2)) IUNIT = IFIX(ARRAY(2))
  if (LNOTBK(3)) PRMS(1) = ARRAY(3)
  if (LNOTBK(4)) PRMS(2) = ARRAY(4)
  PRMS(1) = AMIN1(PRMS(1),1.0)
  PRMS(1) = AMAX1(PRMS(1),0.0)
  PRMS(2) = AMIN1(PRMS(2),1.0)
  PRMS(2) = AMAX1(PRMS(2),0.0)

  if (LKECHO)write(JOPPRT,4510) KEYWRD, IDT, PRMS(1),PRMS(2),IUNIT
  4510 format(/1X,A8,'    Fresh slash to be removed in year', I5,&
       &'; Prop slash < 3 in diam. to remove =',F5.3,/T13,&
       &'Prop. >= 3in diam. =',F5.3,' Stands read from logical ',&
       &'unit ',I4)

  call C26SRT (NOSTND,STDIDS,MYLST1,.true.)

  J = 0
  4520 continue

  !      READ(IUNIT,'(A8)',END=4530) KEYWRD
  !      IF (IUNIT.EQ.IREAD) IRECNT= IRECNT + 1
  !      IF (INDEX(KEYWRD,'-999') .GT. 0) GOTO 4530
  !      CALL C26BSR (NOSTND, STDIDS, MYLST1, KEYWRD, IP)
  !      IF (IP .GT. 0) THEN
  !        J = J + 1
  !        IF (J .LE. MXSTND) THEN
  !          MYLST2(J) = IP
  !          IF(LKECHO)WRITE(JOPPRT,4525) KEYWRD, IP
  ! 4525     FORMAT(T13,'STAND ID: ',A8,'   position ',I4)

  !...Fix for 26-character supplemental records.  AJM 3/28/00.

  read(IUNIT,*,end = 4530) SUPLRECS
  if (SUPLRECS(1:1) .eq. '*') goto 4520
  if (INDEX(SUPLRECS,'-999') .gt.0) goto 4530
  if (IUNIT.eq.IREAD) IRECNT = IRECNT + 1
  call C26BSR (NOSTND, STDIDS, MYLST1, SUPLRECS, IP)
  if (IP .gt.0) then
     J = J + 1
     if (J .le. MXSTND) then
        MYLST2(J) = IP
        if (LKECHO)write(JOPPRT,4525) SUPLRECS, IP
        4525 format(T13,'STAND ID: ',A26,'   position ',I4)
     end if
  end if
  goto 4520
  4530 continue

  !     Apply the list to the global option list.

  call GPADD (KODE, IDT, 315, NPARMS, PRMS(1), J, MYLST2)
  if (KODE.gt.0) then
     call KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
     write(JOPPRT,4505) KEYWRD
     4505 format(/1X,A8,' No room to set option.')
     goto 4540
  end if

  call GPCLOS(315)

  4540 continue

  goto 10
  !                        OPTION NUMBER 36 -- NONSTOCK (nonstockable stands)

  4600 continue

  IUNIT = IREAD
  if (LNOTBK(1)) IUNIT = IFIX(ARRAY(1))

  if (LKECHO)write(JOPPRT,4610) KEYWRD, IUNIT
  4610 format(/1X,A8,' Nonstockable stands read from logical unit ',I4)

  NSSTND = 0

  ! 4620 CONTINUE
  !      READ(IUNIT,'(A8)',END=4630) KEYWRD
  !      IF (IUNIT.EQ.IREAD) IRECNT= IRECNT + 1
  !      IF (INDEX(KEYWRD,'-999') .GT. 0) GOTO 4630
  !      NSSTND = NSSTND + 1
  !      IF (NSSTND .LE. MXSTND) THEN
  !        NSSTDS(NSSTND) = KEYWRD
  !        IF(LKECHO)WRITE(JOPPRT,4625) KEYWRD
  ! 4625   FORMAT(T13,'NONSTOCKABLE STAND ID: ',A8)

  !...Fix for 26-character supplemental records.  AJM 3/28/00

  4620 continue
  read(IUNIT,*,end = 4630) SUPLRECS
  if (SUPLRECS(1:1) .eq. '*') goto 4620
  if (IUNIT.eq.IREAD) IRECNT = IRECNT + 1
  if (INDEX(SUPLRECS,'-999') .gt.0) goto 4630
  NSSTND = NSSTND + 1
  if (NSSTND .le. MXSTND) then
     NSSTDS(NSSTND) = SUPLRECS
     if (LKECHO)write(JOPPRT,4625) SUPLRECS
     4625 format(T13,'NONSTOCKABLE STAND ID: ',A26)
  end if
  goto 4620
  4630 continue

  goto 10

  4700 continue
  !                        OPTION NUMBER 37 -- BMDAMAGE

  !     This controls the number of years prior to the first master cycle, from which
  !     any inventoried attack of MPB will be included in the initial conditions for
  !     the dispersal. If the keyword is ABSENT, inventory will *not* be used. If it
  !     is present with no arguments, the default value of 2 years will be applied. If
  !     there is an argument in field 1, that number will be applied.

  LINVON = .true.
  if (LNOTBK(1)) DEDYRS = MAX(0, INT(ARRAY(1)))

  if (LKECHO)write(JOPPRT,4710) KEYWRD, DEDYRS
  4710 format(/1X,A8,' Inventoried beetle attacks (bole only) will',&
       &' be included when',/T13, 'inventory is within ',&
       &I4,' years of the first master cycle year.')

  goto 10

  4800 continue
  !                  OPTION NUMBER 38 -- BMFDBK (Beetle Negative Feedback) MJO May98

  !     This keyword parameterizes the functions in the BMFDBK.F subroutine, which
  !     monitors the landscape-level beetle population and applies a negative feedback
  !     event when triggered.

  TFDBK = 50.0
  !     Added the flag and 6th keyword field below (MJO July98).
  LFDBK = .true.

  if (LNOTBK(1)) TFDBK = ARRAY(1)

  if (LKECHO)write(JOPPRT,4810) KEYWRD
  4810 format (/1X,A8,'   NEGATIVE FEEDBACK INITIALIZED:')
  if (LKECHO)write(JOPPRT,4820) TFDBK
  4820 FORMAT ('               Mean stand-level BKP to trigger feedback =&
       &',F7.3)
  !      IF(LKECHO)WRITE(JOPPRT,4830) SFDBK
  ! 4830 FORMAT ('               Feedback severity = ',F5.3)
  !      IF(LKECHO)WRITE(JOPPRT,4840) LSBKPA,LSBKPB
  ! 4840 FORMAT ('               BKP(t-1) = ',F6.3,' and BKP(t-2) = ',F6.3)
  !      IF(LKECHO)WRITE(JOPPRT,4850) LAG
  ! 4850 FORMAT ('               Feedback Lag = ',I1,' year(s)')

  goto 10

  ! ORIGINAL LANDSCAPE-LEVEL CODE BELOW...SAVED FOR POSTERITY
  !      TFDBK = 0.3
  !      SFDBK = 1.0
  !      LSBKPA = 0.2
  !      LSBKPB = 0.1
  !      LAG = 1
  !     Added the flag and 6th keyword field below (MJO July98).
  !      LFDBK = .TRUE.

  !      IF (LNOTBK(1)) TFDBK = ARRAY(1)
  !      IF (LNOTBK(2)) SFDBK = ARRAY(2)
  !      IF (LNOTBK(3)) LSBKPA = ARRAY(3)
  !      IF (LNOTBK(4)) LSBKPB = ARRAY(4)
  !      IF (LNOTBK(5)) LAG = ARRAY(5)

  !      IF(LKECHO)WRITE(JOPPRT,4810) KEYWRD
  ! 4810 FORMAT (/1X,A8,'   NEGATIVE FEEDBACK INITIALIZED:')
  !      IF(LKECHO)WRITE(JOPPRT,4820) TFDBK
  ! 4820 FORMAT ('               Mean landscape BKP to trigger feedback =
  !     &',F7.3)
  !      IF(LKECHO)WRITE(JOPPRT,4830) SFDBK
  ! 4830 FORMAT ('               Feedback severity = ',F5.3)
  !      IF(LKECHO)WRITE(JOPPRT,4840) LSBKPA,LSBKPB
  ! 4840 FORMAT ('               BKP(t-1) = ',F6.3,' and BKP(t-2) = ',F6.3)
  !      IF(LKECHO)WRITE(JOPPRT,4850) LAG
  ! 4850 FORMAT ('               Feedback Lag = ',I1,' year(s)')

  !      GOTO 10

  4900 continue

  !  ==========  OPTION NUMBER 39: COMMENT ==============================

  !     COMMENT KEYWORD.  THIS ENABLES THE USER TO INCLUDE
  !     A DESCRIPTIONS WITHIN THE KEYWORD BLOCK.

  if (LKECHO)write(JOPPRT,4901) KEYWRD
  4901 format (/1X,A8)

  !.... READ IN COMMENT STATEMENTS UNTIL 'END' IS ENCOUNTERED

  4902 continue
  read (IREAD,4903,end = 10) RECORD
  4903 format (A80)
  IRECNT = IRECNT + 1
  C4TMP = RECORD(1:4)
  call UPCASE (C4TMP(1:1))
  call UPCASE (C4TMP(2:2))
  call UPCASE (C4TMP(3:3))
  if (C4TMP .eq. 'END ') then
     if (LKECHO)write(JOPPRT,4905) RECORD(1:4)
     4905 format (/' ',A4)
     goto 10
  else
     4904 if (LKECHO)write(JOPPRT,4906) RECORD
     4906 format (T13,A80)
  end if
  goto 4902

  5000 continue
  !                        OPTION NUMBER 40 -- BMOUT (REPLACES BMTOTALS)

  !     IF FIELD ONE IS BLANK, NAME & OPEN THE CYCLE OUTPUT FILE (*.bmc)

  if (.not. LNOTBK(1).or.ARRAY(1).eq.0) then
     LBMCYC = .true.

     OUTNAM = KWDFIL(:ISTLNB(KWDFIL))//'.bmc'

     if (LKECHO)write(JOPPRT,5010) KEYWRD,OUTNAM,JBMCYC
     5010 format (/1X,A8,'   WWPB MODEL "CYCLE" OUTPUT DATA WILL BE ',&
          &'WRITTEN AT CYCLE BOUNDARIES TO FILE: ',A,/&
          &T12,' (DATA SET REFERENCE NUMBER:',I4,')')

     call MYOPEN (JBMCYC,OUTNAM,1,133,0,1,1,0,KODE)
     if (KODE.gt.0) then
        write (*,'('' OPEN FAILED FOR '')') OUTNAM
     end if
  end if

  ! IF FIELD 2 IS BLANK, NAME & OPEN THE LANDSCAPE PER YR OUTPUT FILE (*.bml)

  if (.not. LNOTBK(2).or.ARRAY(2).eq.0) then
     LBMLPY = .true.

     OUTNAM = KWDFIL(:ISTLNB(KWDFIL))//'.bml'

     if (LKECHO)write(JOPPRT,5020) KEYWRD,OUTNAM,JBMLPY
     5020 format (/1X,A8,'   WWPB MODEL LANDSCAPE AVERAGE OUTPUT ',&
          &'STATISTICS WILL BE WRITTEN ANNUALLY TO FILE: ',A,/&
          &T12,' (DATA SET REFERENCE NUMBER:',I4,')')

     call MYOPEN (JBMLPY,OUTNAM,1,133,0,1,1,0,KODE)
     if (KODE.gt.0) then
        write (*,'('' OPEN FAILED FOR '')') OUTNAM
     end if
  end if

  goto 10

  ! AFTER END KEYWORD, EXIT

  900 return

  !.... Special entry to retrieve keywords.
  ! commenting out ajm 8/05.
  ! this is being moved to the new sytand-level option processor BMIN.
  ! We do not need this here anyway, hence our ability to "co-opt" it , rather than duplicate it.

  !      ENTRY BMKEY (KEY,PASKEY)
  !      PASKEY= TABLE(KEY)
  !      RETURN

end
