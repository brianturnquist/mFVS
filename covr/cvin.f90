!=================!
!   covr : cvin   !
!=================!
! COVR $Id$
subroutine CVIN (PASKEY,ARRAY,LNOTBK,LKECHO)
  implicit none

  !  PROCESSES OPTIONS AND WRITES OPTION TABLE FOR COVER ROUTINES.
  !  CALLED FROM **INITRE**.

  ! FIELD       ** (OPTION NUMBER) KEYWORD **

  !             ** (1) END **

  !             ** (2) CANOPY **

  !       -- LCNOP -- LOGICAL FLAG FOR CANOPY OPTION
  ! 1     -- COVOPT -- FOLIAGE BIOMASS EQUATION OPTION.  PRESENTLY NOT
  !                    USED, BECAUSE TREE AGE NOT CARRIED BY PROGNOSIS.

  !             ** (3) SHRUBS **

  !       -- LBROW -- LOGICAL FLAG FOR SHRUBS OPTION
  ! 1     -- SAGE -- TIME SINCE DISTURBANCE
  ! 2     -- IHTYPE -- HABITAT TYPE CODE FOR SHRUBS OPTION ONLY
  ! 3     -- IPHYS -- PHYSIOGRAPHIC POSITION CODE
  ! 4     -- IDIST -- TYPE OF DISTURBANCE CODE

  !             ** (4) SHRBLAYR **

  !       -- LCAL1 -- LOGICAL FLAG FOR CALIBRATION BY SHRUB LAYER
  !       -- LCALIB -- LOGICAL FLAG FOR CALIBRATION
  !       -- NKLASS -- NUMBER OF OBSERVED SHRUB LAYERS
  !       -- SUMCVR -- TOTAL OBSERVED SHRUB COVER
  ! 1,3,5 -- AVGBHT(3) -- OBSERVED AVERAGE HEIGHT OF UP TO 3 SHRUB LAYERS
  ! 2,4,6 -- AVGBPC(3) -- OBSERVED AVERAGE COVER OF UP TO 3 SHRUB LAYERS

  !             ** (5) SHRUBHT **

  !       -- LCAL2 -- LOGICAL FLAG FOR CALIBRATION BY SPECIES
  !       -- LCALIB -- LOGICAL FLAG FOR CALIBRATION
  ! 1-31  -- SHRBHT(31) -- OBSERVED HEIGHTS BY SPECIES

  !             ** (6) SHRUBPC **

  !       -- LCAL2 -- LOGICAL FLAG FOR CALIBRATION BY SPECIES
  !       -- LCALIB -- LOGICAL FLAG FOR CALIBRATION
  ! 1-31  -- SHRBPC(31) -- OBSERVED COVER BY SPECIES

  !             ** (8) NOCOVOUT **

  !       -- LCOVER -- LOGICAL FLAG FOR TURNING OFF CANOPY COVER DISPLAY

  !             ** (9) NOSHBOUT **

  !       -- LSHRUB -- LOGICAL FLAG FOR TURNING OFF SHRUB COVER DISPLAY

  !             ** (10) NOSUMOUT **

  !       -- LCVSUM -- LOGICAL FLAG FOR TURNING OFF SUMMARY DISPLAY

  !             ** (12) COVER **

  !       -- IDT -- CYCLE OR DATE TO TURN ON COVER OPTIONS
  !       -- JOSHRB -- LOGICAL UNIT NUMBER FOR COVER OUTPUT

  !             ** (13) SHOWSHRB **

  !       -- LSHOW -- LOGICAL FLAG FOR SHOWSHRB OPTION
  !       -- NSHOW -- NUMBER OF SPECIES FOR DISPLAY
  ! 1-6   -- ISHOW(6)-- SUBSCRIPTS OF SPECIES FOR DISPLAY

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'CVCOM.f90'

  !  INTERNAL STORAGE

  ! Variables
  logical :: DEBUG,LKECHO
  logical :: LNOTBK(12),LSORT
  real    :: ARRAY(12),ARRAY2(8)
  integer :: KNDEX(33)
  integer :: KSIZE,ISIZE,KODE,NUMBER,I,J,K,IDT,KEY,IRTNCD
  real    :: TEMPHT,TEMPPC
  character(len=10) :: KARD(12)
  character(len=8) :: TABLE(20)
  character(len=8) :: KEYWRD
  character(len=8) :: PASKEY
  character(len=4) :: SNAME(33)
  character(len=4) :: SHRBSP(8)
  data ISIZE/20/, KSIZE/33/
  data TABLE /'END','CANOPY','SHRUBS','SHRBLAYR','SHRUBHT',&
       &'SHRUBPC','DEBUG','NOCOVOUT','NOSHBOUT','NOSUMOUT',&
       &'        ','COVER','SHOWSHRB','CVNOHEAD',6*' ' /
  data SNAME /&
       &'ARUV','BERB','LIBO','PAMY','SPBE','VASC','CARX','LONI',&
       &'MEFE','PHMA','RIBE','ROSA','RUPA','SHCA','SYMP','VAME',&
       &'XETE','FERN','COMB','ACGL','ALSI','AMAL','CESA','CEVE',&
       &'COST','HODI','PREM','PRVI','SALX','SAMB','SORB',&
       &'    ','-999'/

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'CVIN',4,0)

  !  TURN ON SWITCH TO CALL DIAMETER DUBBING SUBROUTINE.

  if (DEBUG) write(JOSTND,4000)
  4000 format ('**CALLING CVIN')
  LDUBDG = .true.

  !     SORT SHRUB NAMES.

  call CH4SRT (KSIZE,SNAME,KNDEX,.true.)

  !  SET KEYWORD = PASSED KEYWORD, BRANCH TO FIND KEYWORD IN THE TABLE.

  KEYWRD = PASKEY
  go to 30

  !  TO READ ANOTHER COVER KEYWORD, BRANCH BACK HERE.

  10 continue
  call KEYRDR (IREAD,JOSTND,DEBUG,KEYWRD,&
       &LNOTBK,ARRAY,IRECNT,KODE,KARD,LFLAG,LKECHO)

  !  RETURN KODES  0=NO ERROR; 1=COLUMN 1 BLANK; 2=EOF

  if (KODE .eq.0) go to 30
  if (KODE .eq.2) call ERRGRO(.false.,2)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  call ERRGRO(.true.,6)
  go to 10
  30 continue

  !  CALL FNDKEY TO FIND KEYWORD IN THE TABLE

  call FNDKEY (NUMBER,KEYWRD,TABLE,ISIZE,KODE,DEBUG,JOSTND)

  !  RETURN KODES  0=NO ERROR; 1=KEYWORD NOT FOUND.

  if (KODE .eq.0) go to 40
  if (KODE .eq.1) then
     call ERRGRO (.true.,1)
     go to 10
  end if
  40 continue

  !  PROCESS OPTIONS.

  go to (1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,&
       &2000,2100,2200,2300,2400,2500,2600,2700,2800,2900), NUMBER
  1000 continue
  !==========  OPTION NUMBER  1: END       ====================
  if (LKECHO)write(JOSTND,8090) KEYWRD
  8090 format (/,A8,'   END COVER OPTIONS')
  return

  1100 continue
  !========== OPTION NUMBER  2: CANOPY     ====================
  LCNOP = .true.
  COVOPT = 2
  if (LKECHO)write(JOSTND,9000) KEYWRD
  9000 format (/,A8,'   CANOPY MODEL CALCULATIONS: TREE CROWN ',&
       &'WIDTH, CROWN SHAPE, AND FOLIAGE BIOMASS')
  if (.not. LNOTBK(1)) go to 1150
  COVOPT = IFIX(ARRAY(1))
  if (LKECHO)write(JOSTND,9002)
  9002 format (T12,'FOLIAGE BIOMASS EQUATION REQUIRES TREE AGES')
  1150 continue
  go to 10
  1200 continue
  !========== OPTION NUMBER  3: SHRUBS    ====================
  LBROW = .true.
  SAGE = -1.0
  if (LKECHO)write(JOSTND,9004) KEYWRD
  9004 format (/,A8,'   SHRUB MODEL OPTIONS:  ')
  if (.not. LNOTBK(1)) go to 1220
  SAGE = ARRAY(1)
  if (LKECHO)write(JOSTND,9104) SAGE
  9104 format (T12,'TIME SINCE DISTURBANCE = ', F5.1,' YEARS')
  go to 1230
  1220 continue
  if ((IAGE.le.0).and.LKECHO)write(JOSTND,9005)
  9005 format (T12,'WARNING: SHRUB MODELS REQUIRE TIME SINCE',&
       &' DISTURBANCE ON THE SHRUBS CARD OR' / T12,'STAND AGE ',&
       &'ON THE STDINFO CARD.  INITIAL TIME SINCE DISTURBANCE ',&
       &'WILL BE SET TO 3 YEARS')
  if ((IAGE.gt.0).and.LKECHO)write(JOSTND,9205) IAGE
  9205 format (T12,'TIME SINCE DISTURBANCE WILL BE SET TO STAND AGE=',&
       &I5,' YEARS')
  1230 continue

  IHTYPE = 0
  if (.not. LNOTBK(2)) go to 1250
  IHTYPE = INT(ARRAY(2))
  if (LKECHO)write(JOSTND,9003) IHTYPE
  9003 format (T12,'HABITAT TYPE = ',I3,' SELECTED FOR PROCESSING ',&
       &'SHRUBS OPTIONS')
  1250 continue

  IPHYS = 2
  if (.not. LNOTBK(3)) go to 1260
  IPHYS = INT(ARRAY(3))
  if ((IPHYS.eq.1).and.LKECHO)write(JOSTND,4001) IPHYS
  4001 format (T12,'PHYSIOGRAPHY TYPE = ',I3,' (BOTTOM)')
  if ((IPHYS.eq.2).and.LKECHO)write(JOSTND,4002) IPHYS
  4002 format (T12,'PHYSIOGRAPHY TYPE = ',I3,' (LOWER SLOPE)')
  if ((IPHYS.eq.3).and.LKECHO)write(JOSTND,4003) IPHYS
  4003 format (T12,'PHYSIOGRAPHY TYPE = ',I3,' (MIDSLOPE)')
  if ((IPHYS.eq.4).and.LKECHO)write(JOSTND,4004) IPHYS
  4004 format (T12,'PHYSIOGRAPHY TYPE = ',I3,' (UPPER SLOPE)')
  if ((IPHYS.eq.5).and.LKECHO)write(JOSTND,4005) IPHYS
  4005 format (T12,'PHYSIOGRAPHY TYPE = ',I3,' (RIDGE)')
  1260 continue

  IDIST = 1
  if (.not. LNOTBK(4)) go to 1270
  IDIST = INT(ARRAY(4))
  if ((IDIST.eq.1).and.LKECHO)write(JOSTND,4011) IDIST
  4011 format (T12,'DISTURBANCE TYPE = ',I3,' (NONE)')
  if ((IDIST.eq.2).and.LKECHO)write(JOSTND,4012) IDIST
  4012 format (T12,'DISTURBANCE TYPE = ',I3,' (MECHANICAL)')
  if ((IDIST.eq.3).and.LKECHO)write(JOSTND,4013) IDIST
  4013 format (T12,'DISTURBANCE TYPE = ',I3,' (BURN)')
  if ((IDIST.eq.4).and.LKECHO)write(JOSTND,4014) IDIST
  4014 format (T12,'DISTURBANCE TYPE = ',I3,' (ROAD)')
  1270 continue
  go to 10
  1300 continue
  !========== OPTION NUMBER  4: SHRBLAYR ====================
  if (LKECHO)write(JOSTND,9011) KEYWRD
  9011 format (/,A8,'   SHRUB MODEL CALIBRATION BY LAYER')
  LCAL1 = .true.
  LCALIB = .true.
  AVGBHT(1) = ARRAY(1)
  AVGBPC(1) = ARRAY(2)
  AVGBHT(2) = ARRAY(3)
  AVGBPC(2) = ARRAY(4)
  AVGBHT(3) = ARRAY(5)
  AVGBPC(3) = ARRAY(6)

  !  COUNT THE NUMBER OF CLASSES INPUT, AND ADD UP TOTAL COVER.

  NKLASS = 0
  SUMCVR = 0.0
  do I = 2,6,2
     if (LNOTBK(I)) NKLASS = NKLASS+1
     SUMCVR = SUMCVR + ARRAY(I)
     110 continue
  end do

  !  BUBBLE-SORT THE % COVER AND HEIGHT FIGURES BY DECREASING HEIGHT.

  120 continue
  LSORT = .false.
  do I = 1,2
     J = I + 1
     if ( AVGBHT(I) .ge. AVGBHT(J) ) go to 130
     LSORT = .true.
     TEMPHT = AVGBHT(I)
     TEMPPC = AVGBPC(I)
     AVGBHT(I) = AVGBHT(J)
     AVGBPC(I) = AVGBPC(J)
     AVGBHT(J) = TEMPHT
     AVGBPC(J) = TEMPPC
     130 continue
  end do
  if ( LSORT ) go to 120

  !  WRITE SORTED INPUT CALIBRATION VALUES.

  if (LKECHO)write(JOSTND,9012)
  9012 format (T12,'LAYER  HEIGHT  COVER'/&
       &T12,'-----  ------  -----')
  do I = 1,NKLASS
     if (LKECHO)write(JOSTND,9013) I,AVGBHT(I),AVGBPC(I)
     9013 format (T12,I3,F9.1,F8.1)
     140 continue
  end do
  go to 10
  1400 continue
  !========== OPTION NUMBER  5: SHRUBHT  ====================
  if (LKECHO)write(JOSTND,9014) KEYWRD
  9014 format (/,A8,'   SHRUB MODEL CALIBRATION BY SPECIES',&
       &' HEIGHT')
  LCAL2 = .true.
  LCALIB = .true.
  do K = 1,4
     read (IREAD,8000,end = 600) (SHRBSP(I),ARRAY2(I),I = 1,8)
     8000 format (8(A4,F6.1))

     !  WRITE INPUT VALUES.

     if (LKECHO)write(JOSTND,8099) (SHRBSP(I),ARRAY2(I),I = 1,8)
     8099 format (T12,8(A4,F6.1))
     IRECNT = IRECNT + 1

     !  CALL **CH4BSR** TO FIND THE SHRUB SPECIES ABBREVIATIONS.
     !  "NUMBER" WILL RETURN THE SHRUB SPECIES INDEX NUMBER.

     do I = 1,8
        call CH4BSR(KSIZE,SNAME,KNDEX,SHRBSP(I),NUMBER)

        !  RETURN CODES:  0=NOT FOUND, >0=KEYWORD POSITION

        if ( NUMBER.gt.0 ) go to 220
        call ERRGRO (.true.,1)
        go to 230
        220 continue

        !  '-999' ENCOUNTERED: END OF SHRUB DATA.

        if (NUMBER .eq.33) go to 10

        !  IF NO INFORMATION WAS INPUT FOR A GIVEN SHRUB SPECIES (THE
        !  PARAMETER FIELD WAS BLANK & NUMBER=32) DO NOT ERASE THE DUMMY
        !  VALUE OF -99999.0 WHICH WAS ASSIGNED TO SHRBHT(I) IN **CVINIT**.

        if ( NUMBER .eq.32 ) go to 230
        SHRBHT(NUMBER) = ARRAY2(I)
        230 continue
     end do
     210 continue
  end do
  1500 continue
  !========== OPTION NUMBER  6: SHRUBPC   ====================
  if (LKECHO)write(JOSTND,9022) KEYWRD
  9022 format (/,A8,'   SHRUB MODEL CALIBRATION BY SPECIES',&
       &' PERCENT COVER')
  LCAL2 = .true.
  LCALIB = .true.
  do K = 1,4
     read (IREAD,8000,end = 600) (SHRBSP(I),ARRAY2(I),I = 1,8)

     !  WRITE INPUT VALUES.

     if (LKECHO)write(JOSTND,8099) (SHRBSP(I),ARRAY2(I),I = 1,8)
     IRECNT = IRECNT + 1

     !  CALL **CH4BSR** TO FIND THE SHRUB SPECIES ABBREVIATIONS.
     !  "NUMBER" WILL RETURN THE SHRUB SPECIES INDEX NUMBER.

     do I = 1,8
        call CH4BSR(KSIZE,SNAME,KNDEX,SHRBSP(I),NUMBER)

        !  RETURN CODES:  0=NOT FOUND, >0=KEYWORD POSITION

        if ( NUMBER.gt.0 ) go to 320
        call ERRGRO (.true.,1)
        go to 330
        320 continue

        !  '-999' ENCOUNTERED: END OF SHRUB DATA.

        if (NUMBER .eq.33) go to 10

        !  IF NO INFORMATION WAS INPUT FOR A GIVEN SHRUB SPECIES (THE
        !  PARAMETER FIELD WAS BLANK & NUMBER=32) DO NOT ERASE THE DUMMY
        !  VALUE OF -99999.0 WHICH WAS ASSIGNED TO SHRBPC(I) IN **CVINIT**.

        if ( NUMBER .eq.32 ) go to 330
        SHRBPC(NUMBER) = ARRAY2(I)
        330 continue
     end do
     310 continue
  end do

  !  ENTER HERE IF END-OF-FILE ENCOUNTERED WHILE READING
  !  SHRUB CALIBRATION DATA.

  600 continue
  call ERRGRO(.false.,2)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return
  1600 continue
  !========== OPTION NUMBER  7: DEBUG    ====================
  if (LKECHO)write(JOSTND,9027)
  9027 format(/,'DEBUG',6X,'IS NO LONGER PROCESSED AS A COVER OPTION;'/&
       &11X,'USE STANDARD PROGNOSIS DEBUG PROCEDURES.')
  go to 10
  1700 continue
  !========== OPTION NUMBER  8: NOCOVOUT ====================
  LCOVER = .false.
  if (LKECHO)write(JOSTND,9028) KEYWRD
  9028 format (/,A8,'   CANOPY COVER STATISTICS DISPLAY WILL NOT ',&
       &'BE WRITTEN')
  go to 10
  1800 continue
  !========== OPTION NUMBER  9: NOSHBOUT===================
  LSHRUB = .false.
  if (LKECHO)write(JOSTND,9030) KEYWRD
  9030 format (/,A8,'   SHRUB STATISTICS DISPLAY WILL NOT BE WRITTEN')
  go to 10
  1900 continue
  !========== OPTION NUMBER 10: NOSUMOUT ====================
  LCVSUM = .false.
  if (LKECHO)write(JOSTND,9032) KEYWRD
  9032 format (/,A8,'   CANOPY AND SHRUBS SUMMARY DISPLAY WILL NOT ',&
       &'BE WRITTEN')
  go to 10
  2000 continue
  !========== OPTION NUMBER 11:          ====================
  go to 10
  2100 continue
  !========== OPTION NUMBER 12: COVER    ====================
  IDT = 1
  if (ARRAY(1) .gt.0.0) IDT = IFIX(ARRAY(1))
  if (LKECHO)write(JOSTND,9035) KEYWRD,IDT
  9035 format (/,A8,'   COVER OPTIONS:  ',&
       &/T12,'DATE/CYCLE=',I5)
  call OPNEW (KODE,IDT,900,0,ARRAY)
  if (KODE .gt.0) go to 10
  if (LNOTBK(2)) JOSHRB = IFIX(ARRAY(2))
  if (LKECHO)write(JOSTND,9036) JOSHRB
  9036 format (T12,'DATA SET REFERENCE NUMBER =',I3)
  go to 10
  2200 continue
  !========== OPTION NUMBER 13: SHOWSHRB ====================
  if (LKECHO)write(JOSTND,9037) KEYWRD
  9037 format (/,A8,'   SELECT SHRUB SPECIES FOR OUTPUT')
  IRECNT = IRECNT + 1
  LSHOW = .true.
  NSHOW = 0
  read (IREAD,8001,end = 600) (SHRBSP(I),I = 1,6)
  8001 format (6(6X,A4))

  !  CALL **CH4BSR** TO FIND THE SHRUB SPECIES ABBREVIATIONS.

  do I = 1,6
     call CH4BSR(KSIZE,SNAME,KNDEX,SHRBSP(I),NUMBER)
     if ( NUMBER.gt.0 ) go to 420
     call ERRGRO (.true.,1)
     go to 430
     420 continue
     if ( NUMBER .lt.32 ) NSHOW = NSHOW + 1
     ISHOW(I) = NUMBER
     430 continue
  end do

  !  WRITE INPUT VALUES.

  if (LKECHO)write(JOSTND,8091) (SHRBSP(I),I = 1,NSHOW)
  8091 format (T12,6(6X,A4))
  go to 10
  2300 continue
  !========== OPTION NUMBER 14: CVNOHEAD ====================
  if (LNOTBK(1)) JCVNOH = IFIX(ARRAY(1))
  LCVNOH = .true.
  if (LKECHO)write(JOSTND,2310) KEYWRD,JCVNOH
  2310 format (/,A8,'   WRITE COVER DATA WITHOUT A HEADING TO',&
       &' DATA SET REFERENCE NUMBER ',I2)
  goto 10
  2400 continue
  !========== OPTION NUMBER 15:          ====================
  go to 10
  2500 continue
  !========== OPTION NUMBER 16:          ====================
  go to 10
  2600 continue
  !========== OPTION NUMBER 17:          ====================
  go to 10
  2700 continue
  !========== OPTION NUMBER 18:          ====================
  go to 10
  2800 continue
  !========== OPTION NUMBER 19:          ====================
  go to 10
  2900 continue
  !========== OPTION NUMBER 20:          ====================
  go to 10

  entry CVKEY (KEY,PASKEY)
  PASKEY = TABLE(KEY)
  return
end
