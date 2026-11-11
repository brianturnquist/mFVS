!==============================!
!   Write activity summaries   !
!==============================!
! BASE $Id$
subroutine OPLIST (LFIRST,NPLT,MGMID,ITITLE)
  implicit none

  !     OPTION PROCESSING ROUTINE - NL CROOKSTON - JAN 1981 - MOSCOW

  !     OPLIST WRITES A SUMMARY OF THE ACTIVITIES.
  !     IF LFIRST IS TRUE, THE INITIAL ACTIVITY SCHEDULE IS WRITTEN;
  !     AND IF LFIRST IS FALSE, THE FINAL SCHEDULE, INCLUDING THE
  !     DISPOSITION OF ACTIVITIES, IS WRITTEN.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'OPCOM.f90'
  include 'KEYCOM.f90'

  ! Variables
  integer :: NTRSLT,I1,I,ICY,I2,II,IACTK,IDT,KEY,LOC,J1,J2,J,ID,K
  character(len=26) :: NPLT
  character(len=4) :: MGMID
  character(len=72) :: ITITLE
  character(len=20) :: IDISPO(2)
  logical :: LINE,LFIRST
  character(len=8) :: KEYWRD
  character(len=8) :: UNKNOW
  character(len=8) :: TITL
  character(len=8) :: SCHED
  character(len=8) :: SUMMR
  character(len=4) :: TAB2(17)
  parameter (NTRSLT = 148)
  integer :: ITRSL1(NTRSLT),ITRSL2(NTRSLT)
  integer :: IOPCYHLD(MAXACT) ! Used for temp hold of IOPCYC array
  data ITRSL1/ &
       & 33,   80,   81,   82,   90,   91,   92,   93,   94,   95, &
       & 96,   97,   98,   99,  100,  101,  102,  110,  111,  120, &
       & 198,  199,  200,  201,  202,  203,  204,  205,  206,  215, &
       & 216,  217,  218,  222,  223,  224,  225,  226,  227,  228, &
       & 229,  230,  231,  232,  233,  234,  235,  236,  237,  248, &
       & 249,  250,  260,  427,  428,  429,  430,  431,  432,  440, &
       & 442,  443,  444,  445,  450,  491,  492,  493,  555,  810, &
       & 811,  900, 1001, 1002, 1003, 1004, 1005, 1006, 1007, 1008, &
       & 1009, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, &
       & 2150, 2151, 2152, 2153, 2154, 2155, 2156, 2157, 2158, 2159, &
       & 2160, 2208, 2209, 2210, 2320, 2401, 2402, 2403, 2414, 2415, &
       & 2416, 2417, 2430, 2431, 2432, 2433, 2501, 2505, 2506, 2507, &
       & 2512, 2520, 2521, 2522, 2523, 2525, 2529, 2530, 2538, 2539, &
       & 2548, 2549, 2550, 2553, 2554, 2605, 2606, 2607, 2608, 2609, &
       & 2701, 2702, 2703, 2704, 2801, 2802, 2803, 2804/

  data ITRSL2/ &
       & 33,   17,   96,  102,    3,   58,   62,   70,   59,  207, &
       & 45,   88,  110,  111,  118,  616,  603,   38,   37,  138, &
       & 135,   92,   48,   49,   71,  103,  107,  120,  144,   41, &
       & 42,   43,   44,   23,   24,   25,   26,   27,   28,   29, &
       & 30,  112,  115,   35,  122,  124,  129,  136,  141,  128, &
       & 108,   78,   34,  216,  211,  212,  202,  203,  229,  213, &
       & 215,   93,  145,  146,  226,  204,  230,  205,  406,  306, &
       & 336,  512,  901,  902,  903,  904,  905,  906,  910,  911, &
       & 916, 1001, 1004, 1005, 1002, 1007, 1010, 1011, 1023, 1024, &
       & 1114, 1108, 1114, 1109, 1116, 1114, 1114, 1124, 1114, 1124, &
       & 1114, 1208, 1209, 1210, 1320, 1401, 1402, 1403, 1414, 1415, &
       & 1416, 1417, 1430, 1431, 1432, 1433, 1501, 1505, 1506, 1507, &
       & 1512, 1520, 1521, 1522, 1523, 1525, 1533, 1534, 1538, 1539, &
       & 1548, 1549, 1550, 1553, 1554, 1601, 1602, 1603, 1604, 1605, &
       & 1301, 1302, 1303, 1304,  703,  704,  705,  706/

  data TAB2/'CMPU','BASE','ESTB','DFTM','MPB', 'COVR', &
       & 'DBS ','CLIM','    ','RUST','MIST','WSBE','DFB', &
       & 'WPBM','RDIS','FIRE','ECON'/
  data UNKNOW/'*UNKNOWN'/,SCHED/'SCHEDULE'/,SUMMR/'SUMMARY '/
  data IDISPO/'DELETED OR CANCELED ', &
       & 'NOT DONE            '/

  !     ITRSL1= TRANSLATION TABLE, PART ONE. CONTAINS AN ORDERED LIST
  !             OF VALID ACTIVITY CODES.
  !     ITRSL2= TRANSLATION TABLE, PART TWO. CONTAINS A SUBSCRIPT TO
  !             THE POSITION IN THE KEYWORD TABLES THAT CONTAINS THE
  !             KEYWORD CORRESPONDING TO THE ACTIVITY.
  !             IF THE SUBSCRIPT IS UNDER 200, THEN THE KEYWORD IS
  !             PART OF THE BASE AND FOUND IN 'TABLE'.  IF IT IS OVER
  !             199, THEN THE KEYWORD IS FROM AS EXTENSION AND NUMBERED
  !             AS FOLLOWS:
  !                     200-299  ESTAB
  !                     300-399  DFTM
  !                     400-499  MPB
  !                     500-599  COVER
  !                     600-699  DBS  (DATABASE KEYWORDS)
  !                     700-799  CLIMATE-FVS
  !                     800-899  ** free spot **
  !                     900-999  BRUST
  !                    1000-1099 MIST
  !                    1100-1199 WSBWE (GenDefol)
  !                    1200-1299 DFB
  !                    1300-1399 WPBM--STAND LEVEL ACTIVITIES.
  !                    1400-1499 WRD VER. 3.0
  !                    1500-1599 FIRE
  !                    1600-1699 ECON

  !     NTRSLT= THE LENGTH OF THE TRANSLATION TABLES.

  !     IF THERE ARE NO ACTIVITIES, AND THIS IS THE SECOND CALL,
  !     DO NOT PRINT THE ACTIVITY SCHEDULE.

  if (.not.LFIRST  .and. IMGL.eq.1) return

  !     WRITE HEADING

  if (.not.LFIRST) write (JOSTND,15)
  if (.not.LFIRST) write (JOSTND,15)
  TITL = SUMMR
  if (LFIRST) TITL = SCHED
  write (JOSTND,10) TITL,NPLT,MGMID,ITITLE
  10 format (//T54,'ACTIVITY ',A8//'STAND ID= ',A26, &
       & '    MGMT ID= ',A4,4X,A72//130('-'))
  if (LFIRST) write (JOSTND,11)
  11 format(/'CYCLE  DATE  EXTENSION  KEYWORD   DATE  PARAMETERS:'/ &
       & '-----  ----  ---------  --------  ----  ',90('-'))
  if (.not.LFIRST) write (JOSTND,12)
  12 format(/'CYCLE  DATE  EXTENSION  KEYWORD   DATE  ', &
       & 'ACTIVITY DISPOSITION  PARAMETERS:'/ &
       & '-----  ----  ---------  --------  ----  ',20('-'), &
       & 2X,68('-'))

  !     IF THIS THE SECOND CALL (LFIRST=FALSE) MAKE SURE THAT THE DATE
  !     AN ACTIVITY WAS ACCOMPLISHED IS USED TO ASSIGN THE ACTIVITY
  !     TO THE CYCLE RATHER THAN THE DATE THE ACTIVITY WAS SCHEDULED.

  !     (1) SAVE THE DATE ARRAY (IDATE) IN IOPCYC AND LOAD IDATE WITH
  !         THE DATE THE ACTIVITY WAS ACCOMPLISHED.

  if (LFIRST) goto 16
  I1 = IMGL-1
  do I = 1,I1
     IOPCYHLD(I) = IOPCYC(I)
     IOPCYC(I) = IDATE(I)

     if (IACT(I,4).gt.0) IDATE(I) = IACT(I,4)
     13 continue
  end do

  !     (2) REESTABLISH THE ASSIGNMENT OF ACTIVITIES TO CYCLES.

  call OPSORT(I1,IDATE,ISEQ,IOPSRT,.true.)
  call OPCYCL(NCYC,IY)

  !     CREATE AND POPULATE COMPUTE DATABASE TABLE IF REQUIRED

  call DBSCMPU

  !     (3) RETURN THE VALUES OF IDATE

  do I = 1,I1
     IDATE(I) = IOPCYC(I)
     IOPCYC(I) = IOPCYHLD(I)
     14 continue
  end do
  16 continue

  !     DO FOR ALL CYCLES

  LINE = .true.
  do ICY = 1,NCYC
     I1 = IMGPTS(ICY,1)
     if (I1.gt.0) goto 30
     if (LINE) write (JOSTND,15)
     15 format ()
     LINE = .false.
     write (JOSTND,20) ICY,IY(ICY)
     20 format (I4,I7)
     goto 160
     30 continue
     LINE = .true.
     write (JOSTND,35) ICY,IY(ICY)
     35 format (/I4,I7)

     !     IF THERE ARE NO ACTIVITIES DURING THE CYCLE;
     !     THEN: BRANCH TO END CYCLE.

     I2 = IMGPTS(ICY,2)

     !     DO FOR ALL ACTIVITIES WITHIN THE CYCLE.

     do II = I1,I2
        I = IOPSRT(II)
        IACTK = IACT(I,1)
        IDT = IDATE(I)

        !     FIND THE ACTIVITY KODE (IACTK) IN THE TRANSLTAION TABLE.

        call OPBISR (NTRSLT,ITRSL1,IACTK,KEY)

        !     IF THE ACTIVITY KODE IS NOT IN THE TRANSLATION TABLE;
        !     THEN: LOAD THE KEYWRD WITH '*UNKNOWN' AND BRANCH TO WRITE.

        if (KEY.le.0) goto 36
        KEY = ITRSL2(KEY)
        goto 40
        36 continue
        KEYWRD = UNKNOW
        LOC = 2
        goto 61
        40 continue

        !     ELSE: LOAD KEYWRD WITH THE ACTUAL KEYWORD AND LOC WITH LOCATION
        !     POINTER.

        LOC = (KEY/100)+1
        if (KEY .lt. 200) then
           KEY = MOD(KEY,200)
        else
           KEY = MOD(KEY,100)
        end if
        goto (42,42,43,44,45,46,47,48,49,52,53,54,55,56,57,58,59),LOC
        42 continue

        !     IF THE ACTIVITY CODE IS FOR A COMPUTE, THEN SET THE KEYWORD UP
        !     TO BE THE VARIABLE NAME.

        if (IACTK.eq.33) then
           LOC = IFIX(PARMS(IACT(I,2)+1))
           if (LOC.gt.500) LOC = LOC-500
           KEYWRD = CTSTV5(LOC)
           LOC = 1
        else
           KEYWRD = TABLE(KEY)
           LOC = 2
        end if
        goto 61
        43 continue
        call ESKEY(KEY,KEYWRD)
        goto 61
        44 continue
        call TMKEY(KEY,KEYWRD)
        goto 61
        45 continue
        call MPKEY(KEY,KEYWRD)
        goto 61
        46 continue
        call CVKEY(KEY,KEYWRD)
        goto 61
        47 continue
        call DBSKEY(KEY,KEYWRD)
        goto 61
        48 continue
        call CLKEY(KEY,KEYWRD)
        go to 61
        49 continue
        !  ** free spot **
        go to 61
        52 continue
        call BRKEY (KEY,KEYWRD)
        go to 61
        53 continue
        call MISKEY (KEY,KEYWRD)
        go to 61
        54 continue
        call BWEKEY (KEY,KEYWRD)
        go to 61
        55 continue
        call DFBKEY (KEY,KEYWRD)
        go to 61
        56 continue
        call BMKEY (KEY,KEYWRD)
        go to 61
        57 continue
        call RDKEY (KEY,KEYWRD)
        go to 61
        58 continue
        call FMKEY (KEY,KEYWRD)
        goto 61
        59 continue
        call ECKEY (KEY,KEYWRD)
        61 continue
        J1 = IACT(I,2)

        !     IF THIS IS THE SECOND CALL (LFIRST=FALSE), BRANCH ALTERNATIVE
        !     LOGIC TO WRITE ACTIVITY DISPOSITION.

        if (.not.LFIRST) goto 100

        !     J1 AND J2 POINT TO THE PARAMETERS FOR THE ACTIVITY.
        !     IF THERE ARE PARAMETERS;
        !     THEN: WRITE THE PARAMETERS ALONG WITH THE KEYWORD.

        if (J1.le.0) goto 90
        J2 = IACT(I,3)

        !     IF WRITTING THE COMPUTE'S, THEN ONLY WRITE 1 PARM.

        if (LOC.eq.1) J2 = J1
        write (JOSTND,60) TAB2(LOC),KEYWRD,IDT,(PARMS(J),J = J1,J2)
        60 format (T16,A4,T25,A8,I6,((T41,8F11.4)))
        goto 150
        90 continue

        !     ELSE: WRITE WITHOUT THE PARAMETERS.

        write (JOSTND,60) TAB2(LOC),KEYWRD,IDT
        goto 150
        100 continue

        !     WRITE DISPOSITION ALONG WITH ACTIVITY.

        ID = IACT(I,4)
        K = 0
        if (ID.le.0) K = ID+2
        if (J1.le.0) goto 95
        J2 = IACT(I,3)
        if (LOC.eq.1) J2 = J1
        if (K.eq.0) write(JOSTND,91) TAB2(LOC),KEYWRD,IDT,ID, &
             & (PARMS(J),J = J1,J2)
        91 format (T16,A4,T25,A8,I6,'  DONE IN',I5,((T63,6F11.4)))
        if (K.gt.0) write(JOSTND,92) TAB2(LOC),KEYWRD,IDT, &
             & IDISPO(K),(PARMS(J),J = J1,J2)
        92 format (T16,A4,T25,A8,I6,2X,A20,((T63,6F11.4)))
        goto 140
        95 continue
        if (K.eq.0) write(JOSTND,91) TAB2(LOC),KEYWRD,IDT,ID
        if (K.gt.0) write(JOSTND,92) TAB2(LOC),KEYWRD,IDT, &
             & IDISPO(K)
        140 continue

        !     WRITE OUTPUT FOR CHEAPO

        call ECOPLS (KEYWRD,IACT(I,4),PARMS,J1,J2)
        150 continue
     end do
     160 continue
  end do

  !     WRITE END-OF-TABLE

  write (JOSTND,180)
  180 format (130('-'))
  return
end subroutine OPLIST
