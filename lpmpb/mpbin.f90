!===================!
!   lpmpb : mpbin   !
!===================!
! LPMPB $Id$
subroutine MPBIN (PASKEY,ARRAY,LNOTBK,LKECHO)
  implicit none

  !     OPTION PROCESSOR FOR MOUNTAIN PINE BEETLE MODEL

  ! Revision History:
  !  Previous revision date noted was 03/18/89.
  !  11/10/03 - Lance R. David (FHTET)
  !    Added LFLAG to KEYRDR call statement argument list.
  !  07/27/07 - Lance R. David (FHTET)
  !    Improved text written for the INVMORT keyword.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.
  !   08/22/14 Lance R. David (FMSC)
  !     Function name was used as variable name.
  !     changed variable INT to INCRS

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'MPBCOM.f90'
  include 'COLCOM.f90'

  !.... INTERNAL STORAGE

  ! Variables
  character(len=8) :: PASKEY
  character(len=8) :: TABLE(40)
  character(len=8) :: KEYWRD
  character(len=10) :: KARD(12)
  character(len=80) :: PNAME

  logical :: LRANSD,LNOTBK(12),LACSRF,LKECHO

  integer :: ISIZE,KODE,NUMBER,I,I1,I2,IG,J,K,KEY

  real    :: ARRAY(12)

  data ISIZE/ 40 /

  data TABLE /&
       &'END','PSFOUND','DCFOUND','INVMORT','MANSTART','MPBSTART',&
       &'AGGPHERM','REPPHERM','DEBUG','PRBSCALE','RANNSEED',&
       &'BETTER','RANSTART','NUMCLASS','AMP','ACTSRF','CRITAD',&
       &'MPBECHO','BEETLES','HABSUIT','MAXYEARS','STNDSIZE',&
       &'GENOTYPE','STRONG','EMERINC','POPDYN','NOPOPDYN',&
       &'AGGTHRES','CURRMORT','EPIPROB','MPBGRF','NODEBUG',&
       &'QVALUES ','INITMORT','NOMPBGRF','PSDBHLIM',&
       &'PSPKILL','DCPKILL','LATITUDE','PARTIAL' /

  !     Set flag for warning note concerning use of ??X.exe models

  call TXNOTE (LXNOTE)

  KEYWRD = PASKEY

  10 continue
  call KEYRDR (IREAD,JOSTND,DEBUIN,KEYWRD,LNOTBK,&
       &ARRAY,IRECNT,KODE,KARD,LFLAG,LKECHO)

  !  RETURN KODES 0=NO ERROR,1=COLUMN 1 BLANK OR ANOTHER ERROR,2=EOF
  !               LESS THAN ZERO...USE OF PARMS STATEMENT IS PRESENT.

  !*    IF (KODE.LT.0) THEN
  !*       IPRMPT=-KODE
  !*    ELSE
  !*       IPRMPT=0
  !*    ENDIF

  if (KODE .eq.0) goto 30
  if (KODE .eq.2) call ERRGRO(.false.,2)
  call ERRGRO (.true.,6)
  goto 10

  30 continue
  call FNDKEY (NUMBER,KEYWRD,TABLE,ISIZE,KODE,DEBUIN,JOSTND)

  !.... RETURN KODES 0=NO ERROR,1=KEYWORD NOT FOUND,2=MISSPELLING.

  if (KODE .eq.0) go to 40
  if (KODE .eq.1) call ERRGRO (.false.,1)
  call ERRGRO (.true.,5)
  40 continue

  !.... IF VARIABLES ARE INITIALIZED, THEN: BRANCH TO OPTION PROCESSING.

  if (LMPB1) go to 90

  !.... ELSE: INITIALIZE VARIABLES

  LACSRF = .false.
  LMPB1 = .true.
  LRANSD = .false.
  go to 90

  !.... SPECIAL END-OF-FILE TARGET

  80 continue
  call ERRGRO (.false.,2)
  90 continue

  !.... PROCESS OPTIONS

  go to( 1000,1200,1300,1400,1500,1500,1700,1800,1900,2000&
       &,2100,2200,2300,2400,2500,2600,2700,2800,2900,3000&
       &,3100,3200,3300,3400,3500,3600,3700,3800,3900,4000&
       &,4100,4200,4300,4400,4500,4600,4700,4800,4900,5000&
       &),  NUMBER

  1000 continue

  !     =================  OPTION NUMBER 1  -- END =============

  if (LKECHO) write(JOSTND,1020) KEYWRD
  1020 format (/,A8,'   END OF MOUNTAIN PINE BEETLE OPTIONS.')
  return

  1200 continue

  !     =================  OPTION NUMBER 2  -- PSFOUND =========

  LPS = .true.
  if (ARRAY(1) .le.0.) go to 10
  if (LKECHO) write(JOSTND,1220) KEYWRD
  1220 format (/,A8,'   PROPORTION OF SURFACE TARGETED FOR SPRAYING ',&
       &'THAT WAS ACTUALLY FOUND AND SPRAYED (BY YEAR):',/)

  J = IFIX(ARRAY(1))
  do I = 1,J
     I1 = (I - 1) * 8 + 1
     I2 = I1 + 7
     if (I2.gt.30) I2 = 30

     read (IREAD,1725,end = 80) (PSPF(K),K = I1,I2)
     if (LKECHO) write(JOSTND,1727) (PSPF(K),K = I1,I2)
     IRECNT = IRECNT + 1
     1250 continue
  end do
  go to 10

  1300 continue

  !     =================  OPTION NUMBER 3  -- DCFOUND =========

  LDC = .true.
  if (ARRAY(1) .le.0.) go to 10
  if (LKECHO) write(JOSTND,1320) KEYWRD
  1320 format (/,A8,'   PROPORTION OF ATTACKED TREES SPRAYED ',&
       &'(BY YEAR):',/)

  J = IFIX(ARRAY(1))
  do I = 1,J
     I1 = (I - 1) * 8 + 1
     I2 = I1 + 7
     if (I2.gt.30) I2 = 30
     read (IREAD,1725,end = 80) (DCPF(K),K = I1,I2)
     if (LKECHO) write(JOSTND,1727) (DCPF(K),K = I1,I2)
     IRECNT = IRECNT + 1
     1350 continue
  end do
  go to 10

  1400 continue

  !     =================  OPTION NUMBER 4  -- INVMORT  ========

  LINVMR = .true.
  LCURMR = .false.
  if (LNOTBK(1)) LDAM = .false.

  if (LKECHO) write(JOSTND,1410) KEYWRD
  1410 format (/,A8,'   USE INVENTORIED CURRENT MPB ATTACK DATA '&
       &'(FVS TREE DATA DAMAGE/SEVERITY CODES).')

  if (LKECHO .and. LDAM) write(JOSTND,1411)
  1411 format (11X,'MPB DAMAGE CODE TABLE WILL BE WRITTEN.')

  if (LKECHO .and..not. LDAM) write(JOSTND,1412)
  1412 format (11X,'MPB DAMAGE CODE TABLE WILL NOT BE WRITTEN.')
  go to 10

  1500 continue

  !     =================  OPTION NUMBER 5  -- MANSTART  =======

  !....  NOTE: BOTH MPBSTART AND MANSTART KEYWORDS GET YOU HERE !!!!!!!!

  MPBON = 1
  I = 1
  if (LNOTBK(1)) I = IFIX(ARRAY(1))

  call OPNEW (KODE,I,555,0,ARRAY(2))
  if (KODE .gt.0) go to 10

  if (LKECHO) write(JOSTND,1540) KEYWRD,I
  1540 format (/,A8,'   OUTBREAK MANUALLY SCHEDULED FOR CYCLE/YEAR ',&
       &I4)

  go to 10

  !     =================  OPTION NUMBER 6  -- MPBSTART  =======

  !.... NOTE: THIS KEYWORD BEING READ IN CAUSES CONTROL TO BE TRANSFERRED
  !....       TO LINE 1500 IF THE CODE FOR MANSTART.

  1700 continue

  !     =================  OPTION NUMBER 7  -- AGGPHERM  =======

  LAGG = .true.
  if (ARRAY(1) .le.0.) go to 10

  if (LKECHO) write(JOSTND,1720) KEYWRD
  1720 format (/,A8,'   POPULATION MULTIPLIERS USED TO ',&
       &'SIMULATE THE ARTIFICIAL APPLICATION OF AGGREGATION PHEREMONES',&
       &' (BY YEAR):',/)

  J = IFIX(ARRAY(1))
  do I = 1,J
     I1 = (I - 1) * 8 + 1
     I2 = I1 + 7
     if (I2.gt.30) I2 = 30
     read (IREAD,1725,end = 80) (AGGPH(K),K = I1,I2)
     if (LKECHO) write(JOSTND,1727) (AGGPH(K),K = I1,I2)
     1725 format (8F10.0)
     1727 format (T12,8F10.2)

     IRECNT = IRECNT + 1
     1750 continue
  end do
  go to 10

  1800 continue

  !     =================  OPTION NUMBER 8  -- REPPHERM  =======

  LREP = .true.
  if (ARRAY(1) .le.0.) go to 10

  if (LKECHO) write(JOSTND,1820) KEYWRD
  1820 format (/,A8,'   POPULATION MULTIPLIERS USED TO ',&
       &'SIMULATE THE ARTIFICIAL APPLICATION OF REPELLING PHEREMONES',&
       &' (BY YEAR):',/)

  J = IFIX(ARRAY(1))
  do I = 1,J
     I1 = (I - 1) * 8 + 1
     I2 = I1 + 7
     if (I2.gt.30) I2 = 30

     read (IREAD,1725,end = 80) (REPL(K),K = I1,I2)
     if (LKECHO) write(JOSTND,1727) (REPL(K),K = I1,I2)

     IRECNT = IRECNT + 1
     1850 continue
  end do
  go to 10

  1900 continue

  !     =================  OPTION NUMBER 9  -- DEBUG  ==========

  if (LKECHO) write(JOSTND,1920) KEYWRD
  1920 format (/,A8,'   MOUNTAIN PINE BEETLE DEBUG ACTIVATED.')

  JOMPB = JOSTND
  DEBUIN = .true.
  go to 10

  2000 continue

  !     =================  OPTION NUMBER 10 -- PRBSCALE  =======

  if ( LNOTBK(1) ) PRBSCL = ARRAY(1)
  if (LKECHO) write(JOSTND,2040) KEYWRD,PRBSCL
  2040 format (/,A8,'   OUTBREAK PROBABILITY SCALING FACTOR = ',F11.3)
  go to 10

  2100 continue

  !     =================  OPTION NUMBER 11 -- RANNSEED  =======

  call MPRNSD (LNOTBK(1),ARRAY(1))
  if (LKECHO) write(JOSTND,2110) KEYWRD,ARRAY(1)
  2110 format (/,A8,'   RANDOM SEED IS:',F10.0)
  goto 10

  2200 continue

  !     =================  OPTION NUMBER 12 -- BETTER  =========

  do I = 1,4
     if ( LNOTBK(I) ) BETTER(I) = ARRAY(I)
     2250 continue
  end do

  if (LKECHO) write(JOSTND,2260) KEYWRD,(BETTER(I),I = 1,NATR)
  2260 format (/,A8,'   CLASSIFICATION IMPORTANCE VALUES',&
       &' (ONE PER ATTRIBUTE) =',8F6.1)
  go to 10

  2300 continue

  !     =================  OPTION NUMBER 13 -- RANSTART  =======

  LRANST = .true.
  if (LNOTBK(1)) ISTDT = ARRAY(1)

  if (LKECHO) write(JOSTND,2310) KEYWRD, ISTDT
  2310 format (/,A8,'   MOUNTAIN PINE BEETLE OUTBREAKS WILL BE ',&
       &'SCHEDULED RANDOMLY STARTING AT CYCLE/YEAR ',I4,'.')
  go to 10

  2400 continue

  !     =================  OPTION NUMBER 14 -- NUMCLASS  =======

  if ( ARRAY(1) .ge.1.0.and. ARRAY(1) .le.30.0)&
       &NCLASS = IFIX (ARRAY(1))

  if (LKECHO) write(JOSTND,2440) KEYWRD,NCLASS
  2440 format (/,A8,'   NUMBER OF TREE CLASSES REPRESENTED = ',I5)
  go to 10

  2500 continue

  !     =================  OPTION NUMBER 15 -- AMP  ============

  if ( LNOTBK(1) ) AMP1 = ARRAY(1)
  if ( LNOTBK(2) ) AMP2 = ARRAY(2)

  if (LKECHO) write(JOSTND,2540) KEYWRD,AMP1, AMP2
  2540 format (/,A8,'   AGGREGATING TREES HAVE ',F8.1,&
       &' TIMES MORE SURFACE.  PARM TWO =',F8.1)
  go to 10

  2600 continue

  !     =================  OPTION NUMBER 16 -- ACTSURF  ========

  LACSRF = .true.
  if (ARRAY(1) .le.0.) go to 10

  if (LKECHO) write(JOSTND,2620) KEYWRD
  2620 format (/,A8,'   ACTUAL SURFACE KILLED PER YEAR : ',/)

  J = IFIX(ARRAY(1))
  do I = 1,J
     I1 = (I - 1) * 8 + 1
     I2 = I1 + 7
     if (I2.gt.30) I2 = 30

     read (IREAD,1725,end = 80) (ACTSRF(K),K = I1,I2)
     if (LKECHO) write(JOSTND,1727) (ACTSRF(K),K = I1,I2)
     IRECNT = IRECNT + 1
     2650 continue
  end do
  go to 10

  2700 continue

  !     =================  OPTION NUMBER 17 -- CRITAD  =========

  if ( LNOTBK(1) ) CRITAD = ARRAY(1)

  if (LKECHO) write(JOSTND,2760) KEYWRD,CRITAD
  2760 format (/,A8,'   BEETLES PER UNIT SURFACE NEEDED TO CAUSE ',&
       &'TREE DEATH = ',F11.3)
  go to 10

  2800 continue

  !     =================  OPTION NUMBER 18 -- MPBECHO  ========

  !.... SET JOMPBX TO THE UNIT NUMBER FOR THE POST PROCESSOR OUTPUT.

  JOMPBX = 29

  !.... READ IN THE FILE NAME FROM THE SUPPLEMENTAL RECORD.

  read (IREAD,2810) PNAME
  2810 format (A)
  if (PNAME .eq. ' ') PNAME = 'MPBOUT'

  !.... TRY TO OPEN THE FILE.

  call MYOPEN (JOMPBX, PNAME, 1, 133, 0, 1, 1, 0, KODE)

  !.... PRINT OUT KEYWORD MESSAGE BASED ON IF THE FILE OPENS OR NOT.

  if (KODE .eq.1) then
     write(JOSTND,2820) KEYWRD, PNAME
     2820 format (/,A8,'   POST PROCESSOR OUTPUT FILE ',A80,/,&
          &11X,'WAS NOT OPENED DUE TO AN ERROR!!!!')
     JOMPBX = 0
  else
     if (LKECHO) write(JOSTND,2830) KEYWRD, PNAME
     2830 format (/,A8,'   POST PROCESSOR OUTPUT FILE ',A80,/,&
          &11X,'WAS OPENED.')
  end if
  goto 10

  2900 continue

  !     =================  OPTION NUMBER 19 -- BEETLES  ========

  if ( LNOTBK(1) ) STRBUG = ARRAY(1)

  if (LKECHO) write(JOSTND,2940) KEYWRD, STRBUG
  2940 format (/,A8,'   STARTING BEETLE POPULATION PER ACRE = ',F11.3)
  go to 10

  3000 continue

  !     =================  OPTION NUMBER 20 -- HABSUIT  ========

  if ( LNOTBK(1) ) HS = ARRAY(1)

  if (LKECHO) write(JOSTND,3040) KEYWRD, HS
  3040 format (/,A8,'   HABITAT SUITABILITY = ',F11.3)
  go to 10

  3100 continue

  !     =================  OPTION NUMBER 21 -- MAXYEARS  =======

  if ( ARRAY(1) .ge.1.0.and. ARRAY(1) .le.30.0 )&
       &MPMXYR = IFIX (ARRAY(1))

  if (LKECHO) write(JOSTND,3140) KEYWRD, MPMXYR
  3140 format (/,A8,'   MAXIMUM YEARS IN MPB PROJECTION = ',I4)
  go to 10

  3200 continue

  !     =================  OPTION NUMBER 22 -- STNDSIZE  =======

  if ( LNOTBK(1) ) EXCON = ARRAY(1)

  if (LKECHO) write(JOSTND,3240) KEYWRD, EXCON
  3240 format (/,A8,'   STAND SIZE (ACRES) = ',F11.3)
  go to 10

  3300 continue

  !     =================  OPTION NUMBER 23 -- GENOTYPE  =======

  if ( ARRAY(1) .ge.1.0.and. ARRAY(1) .le.3.0 )&
       &NG = IFIX ( ARRAY(1) )
  if ( LNOTBK(2) ) DST(1) = ARRAY(2)
  if ( LNOTBK(3) ) DST(2) = ARRAY(3)
  if ( LNOTBK(4) ) DST(3) = ARRAY(4)

  if (LKECHO) write(JOSTND,3340) KEYWRD,NG,(DST(IG),IG = 1,NG)
  3340 format (/,A8,'   NUMBER OF GENOTYPES = ',I1,'.  DISTANCE EACH ',&
       &'ONE CAN FLY (FEET) IS ',3F8.1)
  go to 10

  3400 continue

  !     =================  OPTION NUMBER 24 -- STRONG  =========

  if ( LNOTBK(1) )  STRP = ARRAY(1)

  if (LKECHO) write(JOSTND,3440) KEYWRD,STRP
  3440 format (/,A8,'   PROPORTION BEETLES IN GENOTYPE ONE (STRONG) =',&
       &F8.7)
  go to 10

  3500 continue

  !     =================  OPTION NUMBER 25 -- EMERINC  ========

  if (LNOTBK(1)) INCRS = IFIX (ARRAY(1))

  if (LKECHO) write(JOSTND,3540) KEYWRD,INCRS
  3540 format (/,A8,'   NUMBER OF EMERGENCE INCREMENTS = ',I4)
  go to 10

  3600 continue

  !     =================  OPTION NUMBER 26 -- POPDYN  =========

  if (LKECHO) write(JOSTND,3640) KEYWRD
  3640 format (/,A8,'   THE MOUNTAIN PINE BEETLE POPULATION DYNAMICS',&
       &' MODEL WILL BE USED.')
  LPOPDY = .true.
  go to 10

  3700 continue

  !     =================  OPTION NUMBER 27 -- NOPOPDYN  =======

  if (LKECHO) write(JOSTND,3740) KEYWRD
  3740 format (/,A8,'   THE MOUNTAIN PINE BEETLE RATE OF LOSS MODEL',&
       &' WILL BE USED.')

  if (LNOTBK(1)) IBOUSE = IFIX(ARRAY(1))

  if (IBOUSE.ge.1) then
     IBOUSE = 1
     if (LKECHO) write(JOSTND,3741)
     3741 format (T12,'BOUSFIELD''S ADJUSTMENTS ARE BEING USED.')
  else
     IBOUSE = 0
     if (LKECHO) write(JOSTND,3742)
     3742 format (T12,'BOUSFIELD''S ADJUSTMENTS ARE NOT BEING USED.')
  end if

  LPOPDY = .false.
  go to 10

  3800 continue

  !     =================  OPTION NUMBER 28 -- AGGTHRES  =======

  LCRES = .not. LNOTBK(1)
  if (LNOTBK(1)) TAFAC = ARRAY(1)
  if (LNOTBK(2)) TAMAX = ARRAY(2)
  if (LNOTBK(3)) TAMIN = ARRAY(3)
  if (LNOTBK(4)) TATOL = ARRAY(4)

  if (LKECHO) write(JOSTND,3840) KEYWRD,TAFAC,TAMAX,TAMIN,TATOL
  3840 format (/,A8,'   THRESHOLD OF AGGREGATION = ',F11.3,/,&
       &T12,'THRESHOLD MAXIMUM = ',F11.3,/,&
       &T12,'THRESHOLD MINIMUM = ',F11.3,/,&
       &T12,'CALIBRATION TOLERANCE = ',F11.3)
  go to 10

  3900 continue

  !     =================  OPTION NUMBER 29 -- CURRMORT  =======

  do I = 1,7
     if (LNOTBK(I)) CURRMR(I) = ARRAY(I)
     3910 continue
  end do

  read (IREAD,'(3F10.0,T1,3A10)',end = 80) (ARRAY(I),I = 1,3),&
       &(KARD(I),I = 1,3)
  IRECNT = IRECNT + 1

  do I = 1,3
     if (KARD(I) .ne. ' ') CURRMR(7+I) = ARRAY(I)
     3915 continue
  end do

  if (LKECHO) write(JOSTND,3920) KEYWRD,CURRMR
  3920 format (/,A8,'   CURRENT MORTALITY BY DBH CLASS=',10F8.3)
  LCURMR = .true.
  LINVMR = .false.

  !.... Call the Event Monitor and schedule a manual outbreak in
  !.... cycle 1.

  call OPNEW (KODE,1,555,0,ARRAY(4))

  go to 10

  4000 continue

  !     =================  OPTION NUMBER 30 -- EPIPROB  ========

  if (ARRAY(1) .lt.0.0.or. ARRAY(1) .gt.1.0) go to 10
  LEPI = .true.
  EPIPRB = ARRAY(1)

  if (LKECHO) write(JOSTND,4040) KEYWRD,EPIPRB
  4040 format (/,A8,'   SPECIFIED OUTBREAK PROBABILITY = ',F11.3)
  go to 10

  4100 continue

  !     =================  OPTION NUMBER 31 -- MPBGRF  =========

  if (LKECHO) write(JOSTND,4140) KEYWRD
  4140 format (/,A8,'   MOUNTAIN PINE BEETLE GRAPHICS OUTPUT',&
       &' MODEL WILL BE PRINTED.')
  MPBGRF = .true.
  go to 10

  4200 continue

  !     =================  OPTION NUMBER 32 -- NODEBUG  ========

  if (LKECHO) write(JOSTND,4240) KEYWRD
  4240 format (/,A8,'   TURN OFF DEBUG OUTPUT.')
  DEBUIN = .false.
  go to 10

  4300 continue

  !     =================  OPTION NUMBER 33 -- QVALUES  ========

  do I = 1,7
     if (LNOTBK(I)) PRNOIN(I) = ARRAY(I)
     4310 continue
  end do

  read (IREAD,'(3F10.0,T1,3A10)',end = 80) (ARRAY(I),I = 1,3),&
       &(KARD(I),I = 1,3)
  IRECNT = IRECNT + 1

  do I = 1,3
     if (KARD(I).ne.' ') PRNOIN(7+I) = ARRAY(I)
     4315 continue
  end do

  if (LKECHO) write(JOSTND,4320) KEYWRD,PRNOIN
  4320 format (/,A8,'   Q VALUES BY DBH CLASS=',10F8.3)
  go to 10

  4400 continue

  !     =================  OPTION NUMBER 34 -- INITMORT  =======

  do I = 1,7
     if (LNOTBK(I)) ZINMOR(I) = ARRAY(I)
     4410 continue
  end do

  read (IREAD,'(3F10.0,T1,3A10)',end = 80) (ARRAY(I),I = 1,3),&
       &(KARD(I),I = 1,3)
  IRECNT = IRECNT + 1

  do I = 1,3
     if (KARD(I).ne.' ') ZINMOR(7+I) = ARRAY(I)
     4415 continue
  end do

  if (LKECHO) write(JOSTND,4420) KEYWRD,ZINMOR
  4420 format (/,A8,'   INITIAL MORTALITY BY DBH CLASS=',10F8.3)
  go to 10

  4500 continue

  !     =================  OPTION NUMBER 35 -- NOMPBGRF  =======

  if (LKECHO) write(JOSTND,4540) KEYWRD
  4540 format (/,A8,'   MOUNTAIN PINE BEETLE GRAPHICS OUTPUT',&
       &' MODEL WILL NOT BE PRINTED.')
  MPBGRF = .false.
  go to 10

  4600 continue

  !     =================  OPTION NUMBER 36 -- PSDBHLIM  =======

  LPS = .true.
  if (ARRAY(1) .le.0.) go to 10

  if (LKECHO) write(JOSTND,4620) KEYWRD
  4620 format (/,A8,'   SMALLEST SIZE TREE (DBH) TREATED WITH ',&
       &'PREVENTIVE SPRAY (BY YEAR):',/)

  J = IFIX(ARRAY(1))
  do I = 1,J
     I1 = (I - 1) * 8 + 1
     I2 = I1 + 7
     if (I2.gt.30) I2 = 30

     read (IREAD,1725,end = 80) (PSDL(K),K = I1,I2)
     if (LKECHO) write(JOSTND,1727) (PSDL(K),K = I1,I2)
     IRECNT = IRECNT + 1
     4650 continue
  end do
  go to 10

  4700 continue

  !     =================  OPTION NUMBER 37 -- PSPKILL  ========

  LPS = .true.
  if (ARRAY(1) .le.0.) go to 10

  if (LKECHO) write(JOSTND,4720) KEYWRD
  4720 format (/,A8,'   PROPORTION OF BEETLES KILLED LANDING ON ',&
       &'TREES TREATED WITH PREVENTIVE SPRAY (BY YEAR):',/)

  J = IFIX(ARRAY(1))
  do I = 1,J
     I1 = (I - 1) * 8 + 1
     I2 = I1 + 7
     if (I2.gt.30) I2 = 30

     read (IREAD,1725,end = 80) (PSPK(K),K = I1,I2)
     if (LKECHO) write(JOSTND,1727) (PSPK(K),K = I1,I2)
     IRECNT = IRECNT + 1
     4750 continue
  end do
  go to 10

  4800 continue

  !     =================  OPTION NUMBER 38 -- DCPKILL  ========

  LDC = .true.
  if (ARRAY(1) .le.0.) go to 10

  if (LKECHO) write(JOSTND,4820) KEYWRD
  4820 format (/,A8,'   PROPORTION OF BEETLES KILLED WITH DIRECT ',&
       &'CONTROL (BY YEAR):',/)

  J = IFIX(ARRAY(1))
  do I = 1,J
     I1 = (I - 1) * 8 + 1
     I2 = I1 + 7
     if (I2.gt.30) I2 = 30

     read (IREAD,1725,end = 80) (DCPK(K),K = I1,I2)
     if (LKECHO) write(JOSTND,1727) (DCPK(K),K = I1,I2)
     IRECNT = IRECNT + 1
     4850 continue
  end do
  go to 10

  4900 continue

  !     =================  OPTION NUMBER 39  -- LATITUDE  ======

  if ( LNOTBK(1)) FORLAT = ARRAY(1)
  if (LKECHO) write(JOSTND,4940) KEYWRD,FORLAT
  4940 format (/,A8,'   FOREST LATITUDE = ',F4.1,' DEGREES.')
  go to 10

  5000 continue

  !     =================  OPTION NUMBER 40  --  PARTIAL  ======

  if (LNOTBK(1)) NEPIYR = IFIX(ARRAY(1))
  LSADLP = NEPIYR .gt.0.and.LNOTBK(2)
  if ( LSADLP ) SADLPP = ARRAY(2)

  if (LKECHO) write(JOSTND,5040) KEYWRD,NEPIYR
  5040 format (/,A8,'   CURRENT EPIDEMIC IS IN YEAR ',I4,', ')

  if (LSADLP.and.LKECHO) write(JOSTND,5060) SADLPP
  5060 format (T12,'SURFACE AREA OF DEAD LODGEPOLE =',F11.3)

  if ((.not.LSADLP).and.LKECHO) write(JOSTND,5080)
  5080 format (T12,'SURFACE AREA OF DEAD LODGEPOLE ',&
       &'WILL BE COMPUTED BY MODEL.')
  go to 10

  !.... This entry point executes mountain pine beetle operations
  !.... which can only be executed after all Prognosis keywords
  !.... have been read, but before the Prognosis model enters its cycle
  !.... loop.

  entry MPBOPS

  if (.not. LMPB1) return

  !.... Request diameter growth dubbing & save the past DG measurement
  !.... period...

  NPYR = IFINT
  LDUBDG = .true.
  return

  !.... Entry point used to find the keyword in the table of keywords.

  entry MPKEY (KEY,PASKEY)
  PASKEY = TABLE(KEY)

  return
end
