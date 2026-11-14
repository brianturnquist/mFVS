!==========================!
!   metric/ppbase : ppin   !
!==========================!
! METRIC-PPBASE $Id$
subroutine PPIN
  implicit none

  !     OPTION PROCESSOR FOR THE PARALLEL PROCESSING
  !     EXTENSION TO THE STAND PROGNOSIS SYSTEM.

  !     NICHOLAS L. CROOKSTON          DEC 1979 AND FEB 1982
  !     INT-FORESTRY SCIENCES LABORATORY-MOSCOW, IDAHO

  !     CALLED FROM : PPEMAIN.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PPEPRM.f90'
  include 'OPCOM.f90'
  include 'PPHVCM.f90'
  include 'HVDNCM.f90'
  include 'CONTRL.f90'
  include 'PPCNTL.f90'
  include 'METRIC.f90'

  ! Variables
  integer :: ISIZE
  parameter (ISIZE = 72)

  integer :: KODE,NUMBER,I,I2,J,ISAV,INFILE,IRC,IRPS
  real    :: XWT,DMULT,AMULT,SNEI
  character(len=10) :: KARD(7)
  character(len=10) :: CNEWID
  character(len=80) :: RECORD
  character(len=8) :: TABLE(ISIZE)
  character(len=8) :: KEYWRD
  character(len=26) :: SRD1
  character(len=26) :: SRD2
  character(len=7) :: CSTAT
  character(len=4) :: MRD1
  character(len=4) :: MRD2
  character(len=4) :: CFOUR
  logical :: LNOTBK(7),LALL,LPRT,LKECHO
  real    :: ARRAY(7)
  data TABLE /&
       &'REWIND','ADDSTAND','    ','DEBUG','PROJECT'&
       &,'MSTRINT','MSTRCYCS','STARTYR','NODEBUG'&
       &,'ENDFILE','READHIST','SELISN','SELSTAND','SELMGMID'&
       &,'NEWEIGHT','COMPID','COMPOSIT','NOCOMPOS','SUMMARY'&
       &,'NOSUM','COPYSUM','NOCOPSUM','ACTSUM','NOACTSUM','STNDTABS'&
       &,'YIELDS','SNDCMP','NOSNDCMP','SMPTRE','NOSMPTRE'&
       &,'CVOUTPUT','MXCLRCUT','PPSINIT','INTERACT','PAUSE','RESTORE'&
       &,'NBDENEFF','MSPOLICY','EXACT','NOTEXACT','DEFCCUT','WSBWALL'&
       &,'TMPLINIT','TEMPLATE','AREALOCS','NEIGHBOR','    '&
       &,'OPEN','CLOSE','AUTOID','NOAUTOID','SELSPLAB','SELMSPLB'&
       &,'BYSTAND','BYMGMID','BYSPLAB','BYMSPLB','NOSELECT'&
       &,'NOBYGRP','COPYCOMP','NOCOPCMP','LABWTS','NOLABWTS'&
       &,'LABWTSID','RDACCESS','CMUNIT','NOCMUNIT','LDMGMTID'&
       &,'COMMENT','ADDFILE','BMPPIN','SYSTEM'/

  !     **********          EXECUTION BEGINS          **********

  !     IF VARIABLES ARE INITIALIZED, THEN: SKIP INITIALIZATON.

  5 continue
  if (.not.LIN1ST) go to 10
  LIN1ST = .false.
  call PPINIT
  10 continue
  LKECHO = .true.
  call KEYRDR (IREAD,JOPPRT,PDEBUG,KEYWRD,LNOTBK,&
       &ARRAY,IRECNT,KODE,KARD,LFLAG,LKECHO)

  !     RETURN KODES 0=NO ERROR,1=COLUMN 1 BLANK,2=EOF

  if (KODE.le.0) goto 30
  if (KODE.ne.2) goto 20
  if (JADFIL.eq.0) call ERRGRO(.false.,2)
  IREAD = JADFIL
  JADFIL = 0
  goto 10
  20 continue
  call ERRGRO (.true.,6)
  goto 10
  30 continue
  call FNDKEY (NUMBER,KEYWRD,TABLE,ISIZE,KODE,PDEBUG,JOPPRT)

  !     RETURN KODES 0=NO ERROR,1=KEYWORD NOT FOUND.

  if (KODE .eq.0) go to 40
  if (KODE .eq.1) then
     call ERRGRO (.true.,1)
     goto 10
  end if
  40 continue
  go to 90

  !     SPECIAL END-OF-FILE TARGET

  80 continue
  if (JADFIL .gt.0) then
     IREAD = JADFIL
     JADFIL = 0
     go to 10
  end if
  call ERRGRO (.false.,2)
  90 continue

  !     PROCESS OPTIONS

  go to( 1000,1200,1300,1400,1500,1600,1700,1800,1900,2000&
       &,2100,2200,2300,2400,2500,2600,2700,2800,2900,3000&
       &,3100,3200,3300,3400,3500,3600,3700,3800,3900,4000&
       &,4100,4200,4300,4400,4500,4600,4700,4800,4900,5000&
       &,5100,5200,5300,5400,5500,5600,5700,5800,5900,6000&
       &,6100,6200,6300,6400,6500,6600,6700,6800,6900,7000&
       &,7100,7200,7300,7400,7500,7600,7700,7800,7900,8000&
       &,8100,8200),  NUMBER
  1000 continue
  !                        OPTION NUMBER 1 REWIND
  if (LNOTBK(1)) goto 1005
  call KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
  call ERRGRO (.true.,4)
  goto 10
  1005 continue
  I = IFIX(ARRAY(1))
  write (JOPPRT,1010) KEYWRD,I
  1010 format (/1X,A8,'   DATA SET REFERENCE NUMBER=',I3)
  rewind I
  goto 10
  1200 continue
  !                        OPTION NUMBER 2 ADDSTAND

  !     IF RESTARTING, THIS COMMAND IS INVALID.

  if (LRESTR) then
     call KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
     call PPERRP (.true.,7)
     goto 10
  end if

  !     IF THIS IS THE FIRST STAND, INITIALIZE THE TEMPLATE.
  !     (THIS CALL IS HERE, RATHER IN PPINIT, SO IT WILL NOT
  !     INTERFER WITH PAUSE/RESTART.)

  if (NOSTND.le.0.and..not.(LTMPAC.or.LTMPAD)) call MSTRIT

  write (JOPPRT,1220) KEYWRD
  1220 format (/1X,A8,'   ADD ONE STAND TO THE DATA BASE.')
  NAURPS = 1
  if (ARRAY(1).gt.1) then
     NAURPS = IFIX(ARRAY(1))
     write (JOPPRT,1230) NAURPS
     1230 format (T13,'AUTOMATICALLY CREATE ',I3,' REPLICATIONS ',&
          &'OF THIS STAND AND GIVE EACH A UNIQUE RANDOM NUMBER ',&
          &'SEQUENCE.')
  end if
  return
  1300 continue
  !                        OPTION NUMBER 3
  goto 10

  1400 continue
  !                        OPTION NUMBER 4 DEBUG
  if (LNOTBK(1)) then
     JOPPRT = IFIX(ARRAY(1))
     JOSTND = JOPPRT
  end if
  write (JOPPRT,1410) KEYWRD
  1410 format (/1X,A8,'   DEBUG PARALLEL PROCESSING EXTENSION.')
  PDEBUG = .true.
  goto 10
  1500 continue
  !                        OPTION NUMBER 5 PROJECT

  !     IF END-OF-RUN IS ON, THEN: THE COMMAND IS IN ERROR.

  if (.not.LEORUN) goto 1502
  write (JOPPRT,1501) KEYWRD
  1501 format (/1X,A8,'   END OF PROJECTION ALREADY OCCURRED.')
  call PPERRP (.true.,8)
  goto 10
  1502 continue

  !     IF HISTRY FILE IS OPEN FOR INPUT, REWRITE IT.

  if (LHIST) call WRTHST (JOHIST,JOHIST)

  !     IF RESTARTING, BRANCH TO SPECIAL WRITE

  if (LRESTR) goto 1540
  if (ISTND.gt.0) goto 1520
  write (JOPPRT,1510) KEYWRD,ISTND
  1510 format (/1X,A8,'   STAND COUNTER=',I3)
  call PPERRP (.true.,8)
  goto 10
  1520 continue
  write (JOPPRT,1530) KEYWRD,ISTND
  1530 format (/1X,A8,'   THERE ARE',I5,' STANDS TO PROJECT.')
  LASTIN = .true.
  return
  1540 continue
  write (JOPPRT,1550) KEYWRD
  1550 format (/1X,A8,'   CONTINUE PROJECTION.')
  return
  1600 continue
  !                        OPTION NUMBER 6 MSTRINT
  if (ISTND.le.0) goto 1605
  call PPERRP (.true.,9)
  goto 10
  1605 continue
  I2 = 10
  if (LNOTBK(2)) I2 = IFIX(ARRAY(2))
  if (.not.LNOTBK(1)) goto 1620
  I = IABS(IFIX(ARRAY(1))) + 1
  if (I.eq.1) goto 1620
  if (I.ge.MAXCY1) then
     call KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,4)
     goto 10
  end if
  LMINT = .true.
  MIY(I) = I2
  I = I-1
  write (JOPPRT,1615) KEYWRD,I,I2
  1615 format(/1X,A8,3X,'MASTER CYCLE=',I3,'; PERIOD LENGTH=',I3)
  goto 10
  1620 continue
  LMINT = .true.
  do I = 2,MAXCY1
     MIY(I) = I2
     1630 continue
  end do
  write (JOPPRT,1635) KEYWRD,I2
  1635 format(/1X,A8,3X,'ALL MASTER CYCLES;  PERIOD LENGTH=',I3)
  goto 10
  1700 continue
  !                        OPTION NUMBER 7 MSTRCYCS
  if (ISTND.le.0) goto 1710
  call PPERRP (.true.,9)
  goto 10
  1710 continue
  LMCYC = .true.
  if (ARRAY(1).ge.1..and. ARRAY(1).le.MAXCYC) goto 1720
  call KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
  call ERRGRO (.true.,4)
  goto 10
  1720 continue
  MNCYC = IFIX(ARRAY(1))
  LMCYC = .true.
  write (JOPPRT,1730) KEYWRD,MNCYC
  1730 format (/1X,A8,'   NUMBER OF MASTER CYCLES=',I3)
  goto 10
  1800 continue
  !                        OPTION NUMBER 8 STARTYR
  if (ISTND.le.0) goto 1805
  call PPERRP(.true.,9)
  goto 10
  1805 continue
  MIY(1) = IFIX(ARRAY(1))
  LSTYR = .true.
  write (JOPPRT,1810) KEYWRD,MIY(1)
  1810 format (/1X,A8,'   MASTER STARTING YEAR=',I5)
  goto 10
  1900 continue
  !                        OPTION NUMBER 9 NODEBUG
  PDEBUG = .false.
  write (JOPPRT,1910) KEYWRD
  1910 format (/1X,A)
  goto 10
  2000 continue
  !                        OPTION NUMBER 10 ENDFILE
  I = IFIX(ARRAY(1))
  if (I.gt.0) goto 2010
  call KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
  call ERRGRO (.true.,4)
  goto 10
  2010 continue
  write (JOPPRT,1010) KEYWRD,I
  end FILE I
  goto 10
  2100 continue
  !                        OPTION NUMBER 11 READHIST
  if (LNOTBK(1)) JOHIST = IFIX(ARRAY(1))
  write (JOPPRT,2110) KEYWRD,JOHIST
  2110 format (/1X,A8,'   READ SIMULATION HISTORY FILE USING DATA ',&
       &'SET REFERENCE NUMBER',I3)
  call HISTRD (JOHIST)
  goto 10
  2200 continue
  !                        OPTION NUMBER 12 SELISN
  CSISN = '-1'
  CLISN = '-1'
  read (IREAD,2210,end = 80) RECORD
  2210 format (A80)
  IRECNT = IRECNT+1
  if (RECORD( 1:20).ne. ' ') CSISN = RECORD( 1:20)
  if (RECORD(21:31).ne. ' ') CLISN = RECORD(21:40)
  if (CLISN.ne.'-1') goto 2250
  if (CSISN.ne.'-1') goto 2230
  write (JOPPRT,2220) KEYWRD
  2220 format (/1X,A8,'   NO SELECTION CRITERIA SPECIFIED.')
  goto 10
  2230 continue
  do I = 5,11
     if (CSISN(I:I).eq.' ') CSISN(I:I) = '0'
     2235 continue
  end do
  write (JOPPRT,2240) KEYWRD,CSISN
  2240 format (/1X,A8,'   STAND WITH INTERNAL STAND NUMBER= ',A11,&
       &' WILL BE PROCESSED FOR OUTPUT.')
  goto 10
  2250 continue
  if (CSISN.ne.'-1') goto 2270
  write (JOPPRT,2260) KEYWRD
  2260 format (/1X,A8,'   SECOND ISN SPECIFIED WITHOUT THE FIRST.')
  call ERRGRO (.true.,4)
  goto 10
  2270 continue
  do I = 5,11
     if (CSISN(I:I).eq.' ') CSISN(I:I) = '0'
     if (CLISN(I:I).eq.' ') CLISN(I:I) = '0'
     2275 continue
  end do
  write (JOPPRT,2280) KEYWRD,CSISN,CLISN
  2280 format (/1X,A8,'   STANDS WITH INTERNAL STAND NUMBERS GREATER ',&
       &'OR EQUAL TO ',A11/T13,'AND LESS THAN OR EQUAL TO ',A11,&
       &' WILL BE PROCESSED FOR OUTPUT.')
  goto 10
  2300 continue
  !                        OPTION NUMBER 13 SELSTAND
  SID1 = NOTSET
  SID2 = NOTSET
  read (IREAD,2310,end = 80) SRD1,SRD2
  2310 format (A26,2X,A26)
  IRECNT = IRECNT+1
  if (SRD1.ne.' ') SID1 = SRD1
  if (SRD2.ne.' ') SID2 = SRD2
  if (SID2.ne.NOTSET) goto 2350
  if (SID1.ne.NOTSET) goto 2330
  write (JOPPRT,2220) KEYWRD
  goto 10
  2330 continue
  write (JOPPRT,2340) KEYWRD,SID1
  2340 format (/1X,A8,'   STAND WITH STAND IDENTIFICATION= ''',A8,&
       &''' WILL BE PROCESSED FOR OUTPUT.')
  goto 10
  2350 continue
  if (SID1.ne.NOTSET) goto 2370
  write (JOPPRT,2360) KEYWRD
  2360 format (/1X,A8,'   SECOND STAND ID SPECIFIED WITHOUT THE FIRST.')
  call ERRGRO (.true.,4)
  goto 10
  2370 continue
  write (JOPPRT,2380) KEYWRD,SID1,SID2
  2380 format (/1X,A8,'   STANDS WITH STAND IDENTIFICATIONS ',&
       &'GREATER OR EQUAL TO ''',A8,''''/T13,&
       &'AND LESS THAN OR EQUAL TO ''',A8,&
       &''' WILL BE PROCESSED FOR OUTPUT.')
  goto 10
  2400 continue
  !                        OPTION NUMBER 14 SELMGMID
  MGM1 = NSET
  MGM2 = NSET
  read (IREAD,2410,end = 80) MRD1,MRD2
  2410 format (A4,T11,A4)
  IRECNT = IRECNT+1
  if (MRD1.ne.' ') MGM1 = MRD1
  if (MRD2.ne.' ') MGM2 = MRD2
  if (MGM2.ne.NSET) goto 2450
  if (MGM1.ne.NSET) goto 2430
  write (JOPPRT,2220) KEYWRD
  goto 10
  2430 continue
  write (JOPPRT,2440) KEYWRD,MGM1
  2440 format (/1X,A8,'   STANDS WITH MANAGEMENT IDENTIFICATION ''',A4,&
       &''' WILL BE PROCESSED FOR OUTPUT.')
  goto 10
  2450 continue
  if (MGM1.ne.NSET) goto 2470
  write (JOPPRT,2460) KEYWRD
  2460 format (/1X,A8,'   SECOND MANAGEMENT ID SPECIFIED WITHOUT ',&
       &'THE FIRST.')
  call ERRGRO (.true.,4)
  goto 10
  2470 continue
  write (JOPPRT,2480) KEYWRD,MGM1,MGM2
  2480 format (/1X,A8,'   STANDS WITH MANAGEMENT IDENTIFICATIONS ',&
       &'GREATER OR EQUAL TO ''',A4,''''/T13,&
       &'AND LESS THAN OR EQUAL TO ''',A4,&
       &''' WILL BE PROCESSED FOR OUTPUT.')
  goto 10
  2500 continue
  !                        OPTION NUMBER 15 NEWEIGHT
  read (IREAD,2510,end = 80) CNEWID,XWT
  2510 format (A11,T21,F10.0)
  IRECNT = IRECNT+1

  !     IF THE VALUE OF NOSTND IS 0, THEN: EXIT VIA "NO DATA ERROR"

  if (NOSTND.le.0) then
     write (JOPPRT,2540) KEYWRD,CNEWID
     call PPERRP (.true.,8)
  else

     !        ELSE: FIND THE VALUE OF CNEWID IN THE ARRAY CISNUM

     do I = 1,NOSTND
        if (CNEWID.ne.CISNUM(I)) goto 2530
        J = I
        goto 2550
        2530 continue
     end do

     !        IF DO-LOOP EXITS (THE ISN WANTED CAN'T BE FOUND);
     !        THEN: WRITE ERROR MSG, AND GO READ ANOTHER KEYWORD.

     write (JOPPRT,2540) KEYWRD,CNEWID
     2540 format (/1X,A8,'   ATTEMPT TO FIND ISN= ',A11,' FAILED.')
     call ERRGRO (.true.,4)
     goto 10
     2550 continue

     !        ISN WAS FOUND:  WRITE MSG AND ASSIGN THE NEW WEIGHT.

     write (JOPPRT,2560) KEYWRD,CNEWID,BRCHWT(J),XWT
     2560 format (/1X,A8,'   INTERNAL STAND NUMBER= ',A11,&
          &'; OLD BRANCH WEIGHT=',&
          &F15.7,'; NEW WEIGHT=',F15.7)
     BRCHWT(J) = XWT
  end if
  goto 10
  2600 continue
  !                        OPTION NUMBER 16 COMPID
  read (IREAD,'(A4)',end = 80) CMPID
  IRECNT = IRECNT+1
  write (JOPPRT,2620) KEYWRD,CMPID
  2620 format (/1X,A8,'   COMPOSITE YIELD TABLES ARE IDENTIFIED BY ',&
       &'THE FOLLOWING: ',A4)
  goto 10
  2700 continue
  !                        OPTION NUMBER 17 COMPOSIT
  LCMPS = .true.
  write (JOPPRT,2710) KEYWRD
  2710 format (/1X,A8,'   COMPOSITE YIELD TABLES WILL BE GENERATED.')
  goto 10
  2800 continue
  !                        OPTION NUMBER 18 NOCOMPOS
  LCMPS = .false.
  write (JOPPRT,2810) KEYWRD
  2810 format(/1X,A8,'   COMPOSITE YIELD TABLES WILL NOT BE GENERATED.')
  goto 10
  2900 continue
  !                        OPTION NUMBER 19 SUMMARY
  LSUM = .true.
  write (JOPPRT,2910) KEYWRD
  2910 format (/1X,A8,'   SUMMARY DATA WILL BE PRINTED.')
  goto 10
  3000 continue
  !                        OPTION NUMBER 20 NOSUM
  LSUM = .false.
  write (JOPPRT,3010) KEYWRD
  3010 format (/1X,A8,'   SUMMARY DATA WILL NOT BE PRINTED.')
  goto 10
  3100 continue
  !                        OPTION NUMBER 21 COPYSUM
  JCPSUM = JOCOPY
  if (LNOTBK(1)) JCPSUM = IFIX(ARRAY(1))
  write (JOPPRT,3110) KEYWRD,JOCOPY
  3110 format (/1X,A8,'   SUMMARY YIELD TABLES WILL ',&
       &'BE COPIED, WITHOUT HEADINGS, ',&
       &'TO THE DATA SET REFERENCED BY ',I2)
  goto 10
  3200 continue
  !                        OPTION NUMBER 22 NOCOPSUM
  JCPSUM = 0
  write (JOPPRT,3210) KEYWRD
  3210 format (/1X,A8,'   NO SUMMARY YIELD TABLES WILL',&
       &' BE WRITTEN WITHOUT HEADINGS.')
  goto 10
  3300 continue
  !                        OPTION NUMBER 23 ACTSUM
  LACTS = .true.
  write (JOPPRT,3310) KEYWRD
  3310 format (/1X,A8,'   ACTIVITY SUMMARIES WILL BE PRINTED.')
  goto 10
  3400 continue
  !                        OPTION NUMBER 24 NOACTSUM
  LACTS = .false.
  write (JOPPRT,3410) KEYWRD
  3410 format (/1X,A8,'   ACTIVITY SUMMARIES WILL NOT BE PRINTED.')
  goto 10
  3500 continue
  !                        OPTION NUMBER 25 STNDTABS
  write (JOPPRT,3510) KEYWRD
  3510 format (/1X,A8,'   PRINT STAND TABLES.')
  if (LSNDCP.or.LSMPT) goto 3525
  write (JOPPRT,3520) TABLE(28),TABLE(30)
  3520 format (T13,'''',A8,''' AND ''',A8,&
       &''' ARE IN EFFECT.')
  call PPERRP (.true.,8)
  goto 10
  3525 continue
  call HISTRD (JOHIST)
  if (NMPRTD.gt.0) goto 3530
  call PPERRP (.true.,8)
  goto 10
  3530 continue
  ISAV = ISTND
  call SNDTBS (KODE)
  ISTND = ISAV
  goto 10
  3600 continue
  !                        OPTION NUMBER 26 YIELDS
  write (JOPPRT,3610) KEYWRD
  3610 format (/1X,A8,'   PROCESS YIELD TABLES.')
  if (LSUM .or. LCMPS .or. JCPCMP.gt.0&
       &.or. JCPSUM.gt.0.or. LACTS) goto 3625
  write (JOPPRT,3620) (TABLE(I),I = 18,24,2),TABLE(61)
  3620 format (T13,4('''',A8,''','),' AND ''',A8,''' ARE IN EFFECT.')
  call PPERRP (.true.,8)
  goto 10
  3625 continue
  if (NOSTND.gt.0) goto 3630
  call HISTRD (JOHIST)
  if (NOSTND.gt.0) goto 3630
  call PPERRP (.true.,8)
  goto 10
  3630 continue
  call YIELDS
  goto 10
  3700 continue
  !                        OPTION NUMBER 27 SNDCMP
  LSNDCP = .true.
  write (JOPPRT,3710) KEYWRD
  3710 format (/1X,A8,'   STAND COMPOSITON TABLES WILL BE PRINTED.')
  goto 10
  3800 continue
  !                        OPTION NUMBER 28 NOSNDCMP
  LSNDCP = .false.
  write (JOPPRT,3810) KEYWRD
  3810 format (/1X,A8,'   STAND COMPOSTION TABLES WILL NOT BE PRINTED.')
  goto 10
  3900 continue
  !                        OPTION NUMBER 29 SMPTRE
  LSMPT = .true.
  write (JOPPRT,3910) KEYWRD
  3910 format (/1X,A8,'   SAMPLE TREE AND STAND ATTRIBUTE TABLES WILL',&
       &' BE PRINTED.')
  goto 10
  4000 continue
  !                        OPTION NUMBER 30 NOSMPTRE
  LSMPT = .false.
  write (JOPPRT,4010) KEYWRD
  4010 format (/1X,A8,'   SAMPLE TREE AND STAND ARRTIBUTE TABLES WILL',&
       &' NOT BE PRINTED.')
  goto 10
  4100 continue
  !                        OPTION NUMBER 31 CVOUTPUT
  write (JOPPRT,4110) KEYWRD
  4110 format (/1X,A8,'   PROCESS COVER MODEL OUTPUT.')
  call PPCVUT (LNOTBK,ARRAY)
  goto 10
  4200 continue
  !                        OPTION NUMBER 32 MXCLRCUT
  if (LNOTBK(1)) HVMXCC = ARRAY(1) * HAtoACR
  LHVMXC = .true.
  write (JOPPRT,4210) KEYWRD,HVMXCC * ACRtoHA
  4210 format (/1X,A8,'   MULTISTAND POLICY SELECTION WILL LIMIT ',&
       &'CONTIGUOUS CLEAR CUTS TO ',F6.2,' HA FOR SELECTED STANDS.'/&
       &T13,'STANDS WILL NOT BE SPLIT RESULTING IN ',&
       &'TARGETS NOT BEING "EXACTLY" REACHED.')
  LPRTCT = .false.
  goto 10
  4300 continue
  !                        OPTION NUMBER 33 PPSINIT
  LFLAG = .true.
  LIN1ST = .true.
  ICCODE = 0
  write (JOPPRT,4310) KEYWRD
  4310 format (/1X,A8,'   REINITIALIZE PARALLEL PROCESSING EXTENSION.'//&
       &1X,132('-'))
  goto 5
  4400 continue
  !                        OPTION NUMBER 34 INTERACT
  I = IFIX(ARRAY(1))
  4410 continue
  call GPNEW (KODE,I,100,0,ARRAY)
  if (KODE.gt.0) then
     call KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
     write (JOPPRT,4415)
     4415 format (/' ERROR WHILE PROCESSING INTERACT KEYWORD.')
     goto 10
  end if
  write (JOPPRT,4420) KEYWRD,I
  4420 format (/1X,A8,'   MASTER CYCLE SCHEDULED FOR ',I4,&
       &' WILL BE PROCESSED USING INTERSTAND'/&
       &T13,'CONTROL FOR SUBSEQUENT STANDS.')
  goto 10
  4500 continue
  !                        OPTION NUMBER 35 PAUSE
  if (.not.LNOTBK(1)) then
     write (JOPPRT,4510) KEYWRD
     4510 format (/1X,A8)
     call PPAUSE
     call GRSTOP
  else
     I = IFIX(ARRAY(1))
     if (I.le.0.or. I.ge.MAXCYC) then
        call KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO (.true.,4)
     else
        LPAUSE(I) = .true.
        write (JOPPRT,4580) KEYWRD,I
        4580 format (/1X,A8,'   A PROCESSING PAUSE WILL OCCUR AT',&
             &' THE END OF MASTER CYCLE',I3)
     end if
  end if
  goto 10
  4600 continue
  !                        OPTION NUMBER 36 RESTORE
  write (JOPPRT,4620) KEYWRD
  4620 format (/1X,A8,'   RESTORE PARALLEL PROCESSING MEMORY.')
  call PPEGET
  goto 10
  4700 continue
  !                        OPTION NUMBER 37 NBDENEFF
  LNEDNG = .true.
  if (LNOTBK(1)) HXSIZE = ARRAY(1) * HAtoACR
  write (JOPPRT,4710) KEYWRD,HXSIZE * ACRtoHA
  4710 format (/1X,A8,'   THE GROWTH OF EACH STAND IS PARTIALLY ',&
       &'DEPENDENT ON THE STAND DENSITY OF IT''S NEIGHBORS.',&
       &/T13,'CALCULATIONS ASSUME ALL STANDS ARE HEXAGONS OF ',&
       &F6.1,' HECTARES.')
  if (LPRTCT) then
     write (JOPPRT,4720)
     4720 format (T13,'THE "EXACT" OPTION HAS BEEN TURNED OFF.')
     LPRTCT = .false.
  end if
  goto 10
  4800 continue
  !                        OPTION NUMBER 38 MSPOLICY

  call HVIN (RECORD,KEYWRD,ARRAY,LNOTBK,IREAD,IRECNT)
  goto 10
  4900 continue
  !                        OPTION NUMBER 39 EXACT
  LPRTCT = .true.
  write (JOPPRT,4910) KEYWRD
  4910 format (/1X,A8,'   STANDS WILL BE SPLIT AS NECESSARY TO INSURE',&
       &' MULTISTAND POLICY TARGETS ARE "EXACTLY" REACHED.')
  if (LHVMXC) then
     write (JOPPRT,4920)
     4920 format (T13,'MAX CONTIGUOUS CLEAR CUT OPTION HAS BEEN TURNED',&
          &'OFF.')
     LHVMXC = .false.
  end if
  if (LNEDNG) then
     write (JOPPRT,4930)
     4930 format (T13,'NEIGHBORING DENSITY EFFECTS HAVE BEEN TURNED',&
          &'OFF.')
     LNEDNG = .false.
  end if
  goto 10
  5000 continue
  !                        OPTION NUMBER 40 NOTEXACT
  LPRTCT = .false.
  write (JOPPRT,5010) KEYWRD
  5010 format (/1X,A8,'   STANDS WILL NOT BE SPLIT RESULTING IN ',&
       &'MULTISTAND POLICY TARGETS NOT BEING "EXACTLY" REACHED.')
  goto 10
  5100 continue
  !                        OPTION NUMBER 41 DEFCCUT
  if (LNOTBK(1)) DCTOP = ARRAY(1) * MtoFT
  if (LNOTBK(2)) DCTPA = ARRAY(2) / HAtoACR
  write (JOPPRT,5110) KEYWRD,DCTOP * FTtoM,DCTPA * HAtoACR
  5110 format (/1X,A8,'   A STAND IS CONSIDERED CLEAR CUT WHEN ',&
       &'THE TOP HEIGHT IS LESS THAN ',F6.1,' AND '/T13,&
       &'THE DENSTIY IS IS LESS THAN ',F6.1,' TREES PER HA')
  goto 10
  5200 continue
  !                        OPTION NUMBER 42 WSBWALL

  LTMPAC = .true.
  LTMPAD = .true.
  write (JOPPRT,5210) KEYWRD
  5210 format (/1X,A8,'   MULTI-STAND WESTERN SPRUCE BUDWORM OPTIONS',&
       &' NOT AVAILABLE.')
  goto 10
  5300 continue
  !                        OPTION NUMBER 43 TMPLINIT
  call MSTRIT
  write (JOPPRT,5310) KEYWRD
  5310 format (/1X,A8,'   TEMPLATE RESET TO DEFAULT (NO OPTIONS), ',&
       &'OPTION TEMPLATING SUSPENDED.')
  goto 10
  5400 continue
  !                        OPTION NUMBER 44  TEMPLATE
  call MSTRIT
  LTMPAC = .true.
  LTMPAD = .true.
  write (JOPPRT,5410) KEYWRD
  5410 format (/1X,A8,'   OPTION TEMPLATING ACTIVATED.')
  goto 10
  5500 continue
  !                        OPTION NUMBER 45 AREALOCS
  call SPLAIT
  INFILE = IREAD
  if (LNOTBK(1)) INFILE = IFIX(ARRAY(1))
  DMULT = 1.0
  if (LNOTBK(2)) DMULT = ARRAY(2)
  AMULT = 1.0
  if (LNOTBK(3)) AMULT = ARRAY(3)
  write (JOPPRT,5510) KEYWRD,INFILE,DMULT,AMULT
  5510 format (/1X,A8,'   READ THE SIZE AND LOCATION FILE FROM DATA SET',&
       &' REFERENCE NUMBER = ',I4/T13,'LOCATIONS ARE SCALED ',&
       &'BY ',E13.6,'; AREA DATA BY ',E13.6)
  call SPLARD (DMULT,AMULT,INFILE,I,IRC)
  write (JOPPRT,5520) I
  5520 format (T13,I5,' STAND LOCATION RECORDS WERE READ.')
  if (INFILE.eq.IREAD) then
     IRECNT = IRECNT+I
     if (IRC.eq.1) goto 80
  end if

  !     SIGNAL THAT THE AREAS ARRAY IS NOW NOT DEFINED

  LAREAS = .false.
  goto 10
  5600 continue
  !                        OPTION NUMBER 46 NEIGHBOR

  !     READ THE NEIGHBOR DATA FILE

  call SPNBIT
  INFILE = IREAD
  if (LNOTBK(1)) INFILE = IFIX(ARRAY(1))
  SNEI = 1.0
  if (LNOTBK(2)) SNEI = ARRAY(2)
  write (JOPPRT,5610) KEYWRD, INFILE, SNEI
  5610 format (/1X,A8,'   READ THE NEIGHBOR DATA FILE FROM DATA SET ',&
       &'REFERENCE NUMBER = ',I4/T13,'NEIGHBOR DATA SCALED BY ',&
       &E13.6)
  call SPNBRD (SNEI,INFILE,I,IRC)
  write (JOPPRT,5620) I
  5620 format (T13,I5,' RECORDS OF NEIGHBOR DATA READ.')
  if (INFILE.eq.IREAD) then
     IRECNT = IRECNT+I
     if (IRC.eq.1) goto 80
  end if
  goto 10
  5700 continue
  !                        OPTION NUMBER 47 ** free spot **
  5800 continue
  !                        OPTION NUMBER 48  OPEN
  call KEYOPN (IREAD,RECORD,JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
  goto 10
  5900 continue
  !                        OPTION NUMBER 49  CLOSE
  if (ARRAY(1).le.0.0) then
     call KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,4)
  else
     I = ARRAY(1)
     write (JOPPRT,5920) KEYWRD,I
     5920 format (/1X,A8,'   DATA SET REFERENCE NUMBER = ',I5)
     close (UNIT = I)
  end if
  goto 10
  6000 continue
  !                        OPTION NUMBER 50  AUTOID
  NAUTID = 1
  if (ARRAY(1).lt.0) then
     read (IREAD,'(A4)',end = 80) CAUTID
     IRECNT = IRECNT+1
  else
     write (CAUTID,'(I4.4)') IFIX(ARRAY(1))
  end if
  write (JOPPRT,6010) KEYWRD,CAUTID
  6010 format (/1X,A8,'   MGMTID FIELD IS AUTOMATICALLY GENERATED ',&
       &'STARTING WITH "',A4,'"')
  goto 10
  6100 continue
  !                        OPTION NUMBER 51  NOAUTOID
  NAUTID = 0
  write (JOPPRT,6110) KEYWRD
  6110 format (/1X,A8,'   MGMTID FIELD IS NOT AUTOMATICALLY GENERATED.')
  goto 10
  6200 continue
  !                        OPTION NUMBER 52  SELSPLAB

  !     READ A STAND POLICY LABEL FOR "SELECT" CRITERIA.

  call LBSTRD (IREAD,LNSSPL,SLSPLB,IRECNT,RECORD,KODE,&
       &WKSTR1,WKSTR2)
  if (KODE.gt.1) goto 80

  if (LNSSPL.gt.90) then
     write (JOPPRT,6210) KEYWRD,SLSPLB(1:LNSSPL)
     6210 format (/1X,A8,'   IF ANY MEMBER OF: '/T13,A)
  else
     write (JOPPRT,6211) KEYWRD,SLSPLB(1:LNSSPL)
     6211 format (/1X,A8,'   IF ANY MEMBER OF: ',A)
  end if
  write (JOPPRT,6212)
  6212 format (T13,'IS ALSO IN A STAND LABEL, THE STAND IS SELECTED.')
  if (KODE.eq.1) then
     write (JOPPRT,6220)
     6220 format (/' ********   WARNING: THIS LABEL SET IS SHORTER ',&
          &'THAN THE ONE YOU SPECIFIED.')
     call RCDSET (1,.true.)
  end if
  goto 10
  6300 continue
  !                        OPTION NUMBER 53  SELMSPLB

  !     READ A MULTISTAND POLICY LABEL FOR "SELECT" CRITERIA.

  IRPS = 0
  call LBGET1 (IREAD,IRECNT,RECORD,IRPS,LNSHPL,WKSTR2,1,KODE)
  if (KODE.eq.2) goto 80
  SLHPLB = ' '
  SLHPLB(1:LNSHPL) = WKSTR2(1:LNSHPL)

  if (LNSHPL.le.0) then
     write (JOPPRT,'(/1X,A8,''   OPTION HAS BEEN TURNED OFF.'')')&
          &KEYWRD
  else
     write (JOPPRT,6310) KEYWRD,SLHPLB(1:LNSHPL)
     6310 format (/1X,A8,'   STANDS THAT APPLY TO MULTISTAND POLICY "',&
          &A,'" ARE SELECTED FOR OUTPUT.')
     if (KODE.eq.1) then
        write (JOPPRT,6320)
        6320 format (/' ********   WARNING: THIS LABEL IS SHORTER ',&
             &'THAN THE ONE YOU SPECIFIED.')
        call RCDSET (1,.true.)
     end if
  end if
  goto 10
  6400 continue
  !                        OPTION NUMBER 54  BYSTAND
  NBYGRP = 1
  write (JOPPRT,6410) KEYWRD
  6410 format (/1X,A8,'   YIELDS PROCESSING IS GROUPED BY STAND.')
  goto 10
  6500 continue
  !                        OPTION NUMBER 55  BYMGMID
  NBYGRP = 2
  write (JOPPRT,6510) KEYWRD
  6510 format (/1X,A8,'   YIELDS PROCESSING IS GROUPED BY ',&
       &'MANAGEMENT IDENTIFICATION CODES.')
  goto 10
  6600 continue
  !                        OPTION NUMBER 56  BYSPLAB
  NBYGRP = 3
  write (JOPPRT,6610) KEYWRD
  6610 format (/1X,A8,'   YIELDS PROCESSING IS GROUPED BY ',&
       &'STAND MANAGEMENT POLICIES.')
  goto 10
  6700 continue
  !                        OPTION NUMBER 57  BYMSPLB
  NBYGRP = 4
  write (JOPPRT,6710) KEYWRD
  6710 format (/1X,A8,'   YIELDS PROCESSING IS GROUPED BY ',&
       &'MULTISTAND POLICIES.')
  goto 10
  6800 continue
  !                        OPTION NUMBER 58  NOSELECT
  call SELINT
  write (JOPPRT,6810) KEYWRD
  6810 format (/1X,A8,'   "SELECT" OPTIONS HAVE BEEN TURNED OFF.')
  goto 10
  6900 continue
  !                        OPTION NUMBER 59  NOBYGRP
  NBYGRP = 0
  write (JOPPRT,6910) KEYWRD
  6910 format (/1X,A8,'   "BYGROUP" OPTIONS HAVE BEEN TURNED OFF.')
  goto 10
  7000 continue
  !                        OPTION NUMBER 60 COPYCOMP
  JCPCMP = JOCOPY
  if (LNOTBK(1)) JCPCMP = IFIX(ARRAY(1))
  write (JOPPRT,7010) KEYWRD,JCPCMP
  7010 format (/1X,A8,'   COMPOSITE YIELD TABLES WILL ',&
       &'BE COPIED, WITHOUT HEADINGS, ',&
       &'TO THE DATA SET REFERENCED BY ',I2)
  goto 10
  7100 continue
  !                        OPTION NUMBER 61 NOCOPCMP
  JCPCMP = 0
  write (JOPPRT,7110) KEYWRD
  7110 format (/1X,A8,'   NO COMPOSITE YIELD TABLES WILL',&
       &' BE WRITTEN WITHOUT HEADINGS.')
  goto 10
  7200 continue
  !                        OPTION NUMBER 62  LABWTS
  LBWTS = .true.
  if (ARRAY(1).gt.0) JLBWTS = ARRAY(1)
  write (JOPPRT,7210) KEYWRD,JLBWTS
  7210 format (/1X,A8,'   THE LABELS AND WEIGHTS FILE WILL',&
       &' BE WRITTEN TO DATA SET REFERENCE NUMBER ',I3)
  goto 10
  7300 continue
  !                        OPTION NUMBER 63  NOLABWTS
  LBWTS = .false.
  write (JOPPRT,7310) KEYWRD
  7310 format (/1X,A8,'   THE LABELS AND WEIGHTS FILE WILL NOT',&
       &' BE WRITTEN.')
  goto 10
  7400 continue
  !                        OPTION NUMBER 64  LABWTSID
  read (IREAD,'(A4)',end = 80) CLBWID
  IRECNT = IRECNT+1
  write (JOPPRT,7420) KEYWRD,CLBWID
  7420 format (/1X,A8,'   THE LABELS AND WEIGHTS RECORDS ARE ',&
       &'IDENTIFIED BY THE FOLLOWING: ',A4)
  goto 10
  7500 continue
  !                        OPTION NUMBER 65 RDACCESS
  call SPRDIT
  INFILE = IREAD
  if (LNOTBK(1)) INFILE = IFIX(ARRAY(1))
  DMULT = 1.0
  if (LNOTBK(2)) DMULT = ARRAY(2)
  write (JOPPRT,7510) KEYWRD,INFILE,DMULT
  7510 format (/1X,A8,'   READ THE ROAD DISTANCE FILE FROM DATA SET',&
       &' REFERENCE NUMBER = ',I4/T13,'DISTANCES ARE SCALED ',&
       &'BY ',E13.6)
  call SPRDRD (DMULT,INFILE,I,IRC)
  write (JOPPRT,7520) I
  7520 format (T13,I5,' STAND LOCATION RECORDS WERE READ.')
  if (INFILE.eq.IREAD) then
     IRECNT = IRECNT+I
     if (IRC.eq.1) goto 80
  end if
  goto 10
  7600 continue
  !                        OPTION NUMBER 66 CMUNIT
  LHVUNT = .true.
  write (JOPPRT,7610) KEYWRD
  7610 format (/1X,A8,'   STANDS WITH THE SAME MANAGEMENT ',&
       &'IDENTIFICATIONS ARE SELECTED AS A UNIT.')
  goto 10
  7700 continue
  !                        OPTION NUMBER 67 NOCMUNIT
  LHVUNT = .false.
  write (JOPPRT,7710) KEYWRD
  7710 format (/1X,A8,'   STANDS ARE UNIQUE UNITS.')
  goto 10
  7800 continue
  !                        OPTION NUMBER 68 LDMGMTID

  !     REDEFINE THE STORED MGMTID'S FOR ALL OF THE STANDS.

  INFILE = IREAD
  if (LNOTBK(1)) INFILE = IFIX(ARRAY(1))
  write (JOPPRT,7810) KEYWRD,INFILE
  7810 format (/1X,A8,'   READ STAND AND NEW MGMTID PAIRS FROM DATA SET',&
       &' REFERENCE NUMBER = ',I4)
  I2 = 0
  J = 0
  ISAV = 0
  7820 continue
  read (INFILE,'(A)',end = 7830) RECORD
  if (INDEX(RECORD,'-999').gt.0) goto 7840
  ISAV = ISAV+1
  if (J.eq.0) then
     if (NOSTND.gt.0) then
        J = 1
        call C26SRT (NOSTND,STDIDS,ISDWK1,.true.)
     else
        J = -1
        goto 7820
     end if
  else
     if (J.eq.-1) goto 7820
  end if
  call C26BSR (NOSTND,STDIDS,ISDWK1,RECORD(1:8),I)
  if (I.gt.0) then
     I2 = I2+1
     MGMIDS(I) = RECORD(10:13)
  end if
  goto 7820
  7830 continue
  if (INFILE.eq.IREAD) then
     IRECNT = IRECNT+ISAV
     goto 80
  end if
  7840 continue
  if (INFILE.eq.IREAD) IRECNT = IRECNT+ISAV+1
  write (JOPPRT,7850) ISAV,I2
  7850 format (T13,'RECORDS READ=',I5,'; MGMTIDS CHANGED=',I4)
  if (J.eq.-1) call ERRGRO (.true.,16)
  goto 10
  7900 continue
  !                        OPTION NUMBER 69 COMMENT
  write (JOPPRT,1910) KEYWRD
  7910 continue
  read (IREAD,'(A)',end = 80) RECORD
  IRECNT = IRECNT+1
  CFOUR = RECORD (1:4)
  do I = 1,4
     call UPCASE (CFOUR(I:I))
     7915 continue
  end do
  if (CFOUR.eq.'END ') then
     write (JOPPRT,1910) CFOUR
     goto 10
  else
     write (JOPPRT,'(T13,A)') RECORD
     goto 7910
  end if
  8000 continue
  !                        OPTION NUMBER 70 ADDFILE
  if (LNOTBK(1)) goto 8010
  call KEYDMP (JOPPRT,IRECNT,KEYWRD,ARRAY,KARD)
  call ERRGRO (.true.,4)
  goto 10
  8010 continue
  JADFIL = IREAD
  IREAD = IFIX(ARRAY(1))
  write (JOPPRT,1010) KEYWRD,IREAD
  go to 10
  !                        OPTION NUMBER 71 -- BMPPIN
  8100 continue
  write (JOPPRT,8110) KEYWRD
  8110 format (/1X,A8,'   MULTI-STAND WESTWIDE PINE BEETLE OPTIONS:')
  call BMPPIN (IREAD,IRECNT,JOPPRT)
  goto 10
  !                        OPTION NUMBER 72 -- SYSTEM

  8200 continue

  !     READ THE AND PROCESS THE SYSTEM COMMAND.

  read(IREAD,'(A)',end = 80) RECORD
  IRECNT = IRECNT+1
  write (JOSTND,8210) KEYWRD,TRIM(RECORD)
  8210 format (/,1X,A8,'   SYSTEM CALL:',A)
  call SYSTEM(RECORD)
  goto 10

end
