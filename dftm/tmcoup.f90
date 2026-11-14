!===================!
!   dftm : tmcoup   !
!===================!
! DFTM $Id$
subroutine TMCOUP
  implicit none

  !     THIS SUBROUTINE CALCULATES THE EFFECTS OF A DFTM OUTBREAK
  !     AND ACTS AS THE INTERFACE ROUTINE TO THE STAND
  !     PROGNOSIS MODEL.

  !     MOST RECENT REVISION: NOV 1981 BY R.A. MONSERUD AND
  !     AND N.L. CROOKSTON

  !     CALLING SEQUENCE ASSUMPTIONS:   NORMAL DIAMETER
  !        AND HEIGHT GROWTH AND NORMAL MORTALITY HAVE BEEN
  !        CALCULATED FOR THIS PERIOD, BUT HAVE NOT BEEN
  !        USED TO UPDATE HT, DBH, OR PROB YET.
  !        NORMAL MORTALITY LOSSES ARE STORED IN WK2.

  ! Revision History:
  !   23-DEC-99 Lance R. David (FHTET)
  !         Updated for expansion of FVS stand id (variable NPLT)
  !         from 8 to 26 characters.
  !   07-AUG-01 Lance R. David (FHTET)
  !         Initialization of arrays ISC and ITABSV.
  !   01-APR-2013 Lance R. David (FMSC)
  !      A few variables defined locally were already defined
  !      in a common block. Local declaration removed.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'TMCOM1.f90'
  include 'BIOMAS.f90'
  include 'TMEGGS.f90'
  include 'UPPER.f90'
  include 'LOWER.f90'
  include 'DFOL.f90'
  include 'ICOND.f90'

  ! TMCOUP internal variable descriptions.

  !   AMORT  - Real (14,9).  Holds percentages and probabilities for
  !            mortality and top-kill calculations and conditions.
  !            Remember that with the data statement, the array is loaded
  !            in a column major order, not row major.  The DATA statement
  !            loads the array in the following order (1,1) (2,1) (3,1)
  !            (4,1). . .(1,2) (2,2) (3,2). . .(14,9).

  !                                        rows
  !                             Tree Percent Defoliation Classes
  !            |
  !            |----------DF Classes 1-7---------|--------GF Classes 1-7-------|
  ! columns    |_1____2____3____4____5____6____7____8____9__10__11__12__13__14__
  !          1 |
  !          2 |
  !          3 |
  !          4 |
  !          5 |
  !          6 |
  !          7 |
  !          8 |
  !          9 |

  !              col 1   -- Primary Mortality Percent
  !              col 2   -- Secondary Mortality Percent
  !              col 3   -- used in condition test with a random probability
  !                         to determine if top-kill is to be applied to a
  !                         a tree defoliation class
  !              col 4   -- not used
  !              col 5-9 -- probabilities used in a accumulative manner
  !                         to determine the top-kill class (1-5) the
  !                         tree will be in when top-kill has occured
  !                         based on its tree defoliation class.
  !                         Actual top-kill percentage is calculated from
  !                         values in arrays TKBOT and TKTOP.

  !   DGLOSS - Real.  Holds the calculated value of diameter growth loss.

  !   DPMAX  - Real.  The maximum branch defoliation percentage for the
  !            current tree class.  (from Phase II and Phase III defoliation
  !            percentages stored in common variable DPCENT).

  !   G19MAX - Real.  The maximum number of days for which food demands were
  !            not fully satisfied for the current tree class.  (from Phase II
  !            or Phase III values stored in common variable G19OUT).

  !   ITABSV - Integer.  Holds the row index to the AMORT table.  This
  !            value identifies the tree defoliation class (1-7 for DF
  !            and 8-14 for GF).

  !   RGLOSS - Real (14).  Holds the static percentages used for calculating
  !            the diameter growth loss for each tree defoliation class.
  !            (1)-(7)  -- DF values
  !            (8)-(14) -- GF values

  !   TKBOT  - Real (5).  Bottom threshold percentages for top-kill
  !            calculations of top-kill classes 1-5.

  !   TKTOP  - Real (5).  Top threshold percentages for top-kill
  !            calculations of top-kill classes 1-5.

  ! Variables
  character(len=4) :: KSPP(3)

  integer :: I,I1,I2,I7,ICRI,IFIN,II,IJUMP,IP,IPT(MAXTRE),ISC(100,2),ISPLIT,ITAB,ITABSV(100),ITWO,J,&
       &JCLAS2(100),JTRUNK,K,KEY(9),KODE,LEFT,MAXCLS,NACL,NRECS,NUM,NUMCLS(3)

  real    :: A3(1),A4(1),A5(1),A6(1),A7(1),A8(1),ACBASE,ACR,ADBH,ADG,ADGLOS,AHT,AHTG,AHTLOS,AMORT(14,&
       &9),APCTKL,ATKILL,CBASE,CLAS(100,7),CR,CROWN,DELTA,DFMEAN,DGLOSS,DPMAX,EGGS(100),G19MAX,GFMEAN,&
       &HTGLOS,HTRUNC
  real    :: P1,P2,P3,PCKILL,PRMORT,PRMS(2),PRNORM,PRSUM,PRTOPK,RANDOM,RGLOSS(14),SALKOD,SPROB,T,&
       &TKBOT(5),TKILL,TKTOP(5),TMBCHL,TSAVE(100),WBDEF(3),WCBASE(3),WCR(3),WDBH(3),WDG(3),WDGLOS(3),&
       &WEGGS(3),WHT(3),WHTG(3),WHTLOS(3),WPCTKL(3),WPMORT(3),WRECS(3),WTDEF(3),WTKILL(3),WZ2(3),WZ3(3),&
       &WZ4(3),ZZ1,ZZ2

  equivalence (CLAS(1,1), Z4(1)), (JCLASS(1), JCLAS2(1))
  equivalence (IPT(1), WK6(1))

  data AMORT/0.0000, 0.0000, 0.0090, 0.0280, 0.1730, 0.4770, 0.9230,&
       &0.0000, 0.0000, 0.0090, 0.0280, 0.1730, 0.4770, 0.9230,&
       &0.0760, 0.0760, 0.0760, 0.1075, 0.1999, 0.1144, 0.0168,&
       &0.0442, 0.0428, 0.0452, 0.0587, 0.0926, 0.0778, 0.0114,&
       &0.1158, 0.2081, 0.4163, 0.4820, 0.2888, 0.2005, 0.0295,&
       &0.0559, 0.1271, 0.2703, 0.3838, 0.3441, 0.2149, 0.0316,&
       &0.7570, 0.6562, 0.4736, 0.3825, 0.3383, 0.2081, 0.0306,&
       &0.9000, 0.8301, 0.6755, 0.5295, 0.3903, 0.2303, 0.0339,&
       &0.0694, 0.1360, 0.1800, 0.2376, 0.1376, 0.1532, 0.0226,&
       &0.0341, 0.0877, 0.1332, 0.1733, 0.1933, 0.0963, 0.0142,&
       &0.0464, 0.0696, 0.2125, 0.1606, 0.0599, 0.0384, 0.0057,&
       &0.0123, 0.0293, 0.0891, 0.1021, 0.0587, 0.0401, 0.0059,&
       &0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000,&
       &0.0039, 0.0078, 0.0317, 0.0434, 0.0225, 0.0274, 0.0040,&
       &0.0000, 0.0000, 0.0238, 0.0838, 0.0775, 0.0000, 0.0000,&
       &0.0038, 0.0019, 0.0122, 0.0479, 0.0157, 0.0000, 0.0000,&
       &0.0000, 0.0026, 0.0000, 0.0000, 0.0138, 0.0089, 0.0013,&
       &0.0018, 0.0005, 0.0040, 0.0172, 0.0539, 0.0511, 0.0075/

  data RGLOSS /0.846, 0.716, 0.648, 0.610, 0.569, 0.388, 0.388,&
       &0.785, 0.693, 0.628, 0.596, 0.559, 0.455, 0.455 /

  data TKBOT /0.0, 0.0, 0.10, 0.25, 0.50/
  data TKTOP /0.0, 0.10, 0.25, 0.50, 0.99/

  data MAXCLS / 100 /,  ITWO / 2 /
  data ISPLIT / 3 /

  !     SET PARAMETERS FOR CROWN DEFOLIATION EQUATION

  data P1 / -22.579678 /, P2/ -22.996253 /, P3/ -598181961.0 /

  data KEY / 3, 4, 6*0, 1  /
  data KSPP / '    ', '    ', 'HOST' /

  !     INITIALIZE KSPP.

  KSPP(1)(1:2) = NSP(IDFCOD,1)
  KSPP(2)(1:2) = NSP(IGFCOD,1)

  do I = 1,100
     ISC(I,1) = 0
     ISC(I,2) = 0
     ITABSV(I) = 0
     5 continue
  end do

  !     WRITE A HEADER

  write(JODFTM,10) NPLT, MGMID
  10 format (//,19('-'),'  DFTM DEFOLIATION STATISTICS FOR STAND ',&
       &A26,'; MANAGEMENT ID= ',A4,2X,23('-')/)

  !     WRITE A WARNING IF IFINT IS NE TO 5

  if (IFINT .ne.5) write (JODFTM,20) IFINT
  20 format (/,'***** WARNING:  THE CALCULATIONS OF TUSSOCK',&
       &' MOTH IMPACT ON TREE GROWTH ASSUME THAT A 5 YEAR',&
       &' PROJECTION CYCLE IS BEING USED.',/,&
       &T17,'YOU ARE USING A ',I2,' YEAR CYCLE.')

  write (JODFTM,30) TMYRS(ICYC), ICYC, IY(ICYC), IY(ICYC+1)
  30 format ('THE FIRST YEAR OF THIS SIMULATED TUSSOCK MOTH ',&
       &'OUTBREAK IS YEAR ',I5,'.  IT IS BEING SIMULATED',/,&
       &'AS PART OF CYCLE',I3,' WHICH REPRESENTS YEARS',&
       &I5,' TO',I5,' IN THIS STANDS DEVELOPMENT.')

  !     IF POST PROCESSOR OUTPUT THEN PRINT THE OUTBREAK YEAR.

  if (JODFEC .gt.0) write (JODFEC,35) TMYRS(ICYC)
  35 format(I4)

  if (TMDEBU) then
     write (JODFTM,40) CNTDF, CNTGF
     40 format (//, 9('*'),'  TMCOUP . . .',/,&
          &'CNTDF = ',F10.4,' CNTGF = ',F10.4)

     write (JODFTM,50)
     50 format ('PROB . . .'//)

     write (JODFTM,60) (PROB(I),I = 1,ITRN)
     60 format (10F13.3)
  end if

  !     LOAD ARRAY 'IPT'

  do I = 1,ITRN
     IPT(I) = IND1(I)
     70 continue
  end do

  IFIN = NACLAS(1) + NACLAS(2)
  NUMCLS(1) = NACLAS(1)
  NUMCLS(2) = NACLAS(2)
  NUMCLS(3) = IFIN

  if (LDF) then

     !        ** LOAD DOUGLAS FIR

     if (TMDEBU) write (JODFTM,80)
     80 format ('CALLING GARBEL (DF)')

     !        CALL DATA COMPRESSION ROUTINE

     I = ISCT(IDFCOD,1)
     I7 = 7

     call GARBEL (IPT(I), (ISCT(IDFCOD,2)-I+1),&
          &KEY, WEIGHT, I7, NACLAS(1), TMPN1, CLAS, ISC, WK3, PCNEWF,&
          &FBIOMS, A3, A4, A5, A6, A7, A8, PROB, KODE, MAXCLS, ITWO)

     if (KODE .ne.0) write (JODFTM,90)
     90 format ('COMPRESSION ROUTINE FAILED FOR DOUGLAS FIR')

     !        ADD THE PROPER OFFSET TO SECTOR POINTERS

     I2 = NACLAS(1)
     do I = 1, I2
        ISC(I,1) = ISC(I,1) + ISCT(IDFCOD,1) - 1
        ISC(I,2) = ISC(I,2) + ISCT(IDFCOD,1) - 1

        !           SET SPECIES CODE

        IZ6(I) = 1
        100 continue
     end do
  end if

  if (LGF) then

     !        ** LOAD GRAND FIR

     if (TMDEBU) write (JODFTM,110)
     110 format ('CALLING GARBEL (GF)')

     !        CALL DATA COMPRESSION ROUTINE

     I1 = NACLAS(1) + 1
     I = ISCT(IGFCOD,1)
     I7 = 7
     call GARBEL (IPT(I), (ISCT(IGFCOD,2)-I+1), KEY, WEIGHT,&
          &I7, NACLAS(2), TMPN1, CLAS(I1,1), ISC(I1,1), WK3, PCNEWF,&
          &FBIOMS, A3, A4, A5, A6, A7, A8, PROB, KODE, MAXCLS, ITWO)

     if (KODE .ne.0) write (JODFTM,120)
     120 format ('COMPRESSION ROUTINE FAILED FOR GRAND FIR')

     !        ADD THE PROPER OFFSET TO SECTOR POINTERS

     do I = I1, IFIN
        ISC(I,1) = ISC(I,1) + ISCT(IGFCOD,1) - 1
        ISC(I,2) = ISC(I,2) + ISCT(IGFCOD,1) - 1

        !           SET SPECIES CODE

        IZ6(I) = 2
        130 continue
     end do
  end if

  !     CREATE 'JCLAS2' AS A SORTED ORDER OF THE TREE CLASSES
  !     IN DESENDING AVERAGE CLASS DIAMETER.

  !     STEP1: COMPUTE AVERAGE DBH FOR EACH TREE CLASS (STORE IN WK3)

  do I = 1,IFIN
     I1 = ISC(I,1)
     I2 = ISC(I,2)
     ADBH = 0.0
     SPROB = 0.0

     do J = I1,I2
        IP = IPT(J)
        ADBH = ADBH + DBH(IP) * PROB(IP)
        SPROB = SPROB + PROB(IP)
        140 continue
     end do

     WK3(I) = ADBH / SPROB
     JCLAS2(I) = I
     150 continue
  end do

  !     STEP2: SORT JCLAS2 ON WK3

  call RDPSRT (NACLAS(1), WK3, JCLAS2, .false.)
  J = NACLAS(1) + 1
  call RDPSRT (NACLAS(2), WK3, JCLAS2(J), .false.)

  DFMEAN = DFREGG(1)

  if (DFREGG(3) .ge.0.001) then

     !        RANDOMLY CHOOSE DF MEAN LARVAL DENSITY FOR EACH OUTBREAK
     !        IF THE BETWEEN-OUTBREAK STANDARD DEVIATION IS POSITIVE.

     160 ZZ1 = TMBCHL(0.0,1.0)
     DFMEAN = DFREGG(1) + DFREGG(3) * ZZ1
     if (DFMEAN .lt.0.001) goto 160
  end if

  GFMEAN = GFREGG(1)

  if (GFREGG(3) .ge.0.001) then

     !        RANDOMLY CHOOSE GF MEAN LARVAL DENSITY FOR EACH OUTBREAK
     !        IF THE BETWEEN-OUTBREAK STANDARD DEVIATION IS POSITIVE.

     !        THE DEVIATES FOR BOTH HOST SPECIES SHOULD BE HIGHLY CORRELATED

     ZZ2 = TMBCHL(0.0, 0.1)
     GFMEAN = GFREGG(1) + GFREGG(3) * (ZZ1 + ZZ2)
     if (GFMEAN .lt.0.001) GFMEAN = 0.0
  end if

  !     FILL IN THE REST OF THE 'ICOND' COMMON AREA

  I1 = 0
  I2 = 0

  do II = 1,IFIN
     I = JCLAS2(II)
     X5(I) = Z2(I) * Z3(I) / 100.0
     X6(I) = Z3(I) - X5(I)
     Z5(I) = 1.0

     !       ASSIGN EGGS TO TREE CLASSES

     if (IEGTYP .eq.2) then

        !          DETERMINISTICALLY ALLOCATE EGGS BY CROWN (ACTUALLY DBH) CLASS
        !          (NOTE THAT THE TREE CLASSES ARE IN DECENDING ORDER OF DBH,
        !          SINCE CLASSIFICATION HAS BEEN ON A MONOTONIC FUNCTION OF DBH)

        NACL = NACLAS(1)
        if (IZ6(I) .eq.2) NACL = NACLAS(2)
        LEFT = II
        if (IZ6(I) .eq.2) LEFT = II - NACLAS(1)
        NUM = ((LEFT * ISPLIT - 1) / NACL) + 1
        if (IZ6(I) .eq.1) X7(I) = DFEGG(NUM)
        if (IZ6(I) .eq.2) X7(I) = GFEGG(NUM)
     else

        !          RANDOMLY ALLOCATE EGGS TO TREE CLASSES IF IEGTYP = 1

        if (IZ6(I) .eq.1) X7(I) = TMBCHL(DFMEAN, DFREGG(2))
        if (IZ6(I) .eq.2) X7(I) = TMBCHL(GFMEAN, GFREGG(2))
        if (X7(I) .lt.0.0) X7(I) = 0.0
     end if

     !       SAVE THE EGG COUNTS FOR PRINTING

     EGGS(I) = X7(I)
     170 continue
  end do

  if (TMDEBU) then

     !        WRITE DEBUG OUTPUT

     write (JODFTM,180) (IPT(I),I = 1,ITRN)
     180 format (//,'IPT . . .',//,135(10I13,/))

     write (JODFTM,190) ((ISC(I1,I2),I2 = 1,2),I1 = 1,IFIN)
     190 format (//,'CLASS SECTOR POINTERS (ISC) . . .',//,10(10I13,/))

     write (JODFTM,200) IFIN,NACLAS
     200 format (//,'IFIN= ',I6,'  NUMBER ACTUAL CLASSES (NACLAS)=',&
          &2I9,/,'DUMP OF COMMON AREA ''ICON''. . . ',//,&
          &T9,'IZ6',10X,'Z4',11X,'Z5',11X,'Z2',11X,'Z3',&
          &11X,'X5',11X,'X6',11X,'X7',6X,'JCLASS',/,&
          &5X,'SPECIES       PROB  BIOMAS/TREE       PCNEWF',&
          &7X,'FBIOMS     NEW BMAS     OLD BMAS',9X,'EGGS',/)

     do I = 1,IFIN
        write (JODFTM,220) IZ6(I), Z4(I), Z5(I), Z2(I), Z3(I),&
             &X5(I), X6(I), X7(I), I
        210 continue
     end do
     220 format (1X,I10,7F13.3,I10)
  end if

  if (LPUNCH) then

     !        WRITE DFTMOD INPUT TO DISK FOR LATER RETRIEVAL

     write (JOTMDK,230)
     230 format ('LIST OF INPUT TO DFTMOD:')

     do I = 1,IFIN
        write (JOTMDK,250) IZ6(I), Z4(I), Z5(I), Z2(I), Z3(I),&
             &X5(I), X6(I), X7(I), I, NPLT
        240 continue
     end do
     250 format (I2,7F9.4,I3,4X,A4,1X,A26)
  end if

  if (TMDEBU) write (JODFTM,260)
  260 format (//,9('*'),' CALLING DFTMOD')

  !     ** CALCULATE % BRANCH DEFOLIATION
  !     Array DPCENT is loaded

  call DFTMOD (1,IFIN)

  if (TMDEBU) write (JODFTM,270)
  270 format (//,9('*'),' RETURN FROM DFTMOD')

  !     IF ( LPUNCH ) WRITE (JOTMDK,280)
  ! 280 FORMAT (' LIST OF DEFOLIATION PERCENTS BY TREE CLASS:')

  do I = 1,IFIN
     I1 = ISC(I,1)
     I2 = ISC(I,2)

     !        COMPUTE MAXIMUM BRANCH DEFOLIATION (EITHER PHASE II OR III)

     DPMAX = DPCENT(I,1)
     if (DPCENT(I,2) .gt. DPMAX) DPMAX = DPCENT(I,2)

     !        COMPUTE CROWN DEFOLIATION AND FIND ASSOCIATED LINE
     !        OF MORTALITY AND TOP-KILL TABLE (AMORT)

     G19MAX = G19OUT(I,1)
     if (G19OUT(I,2) .gt. G19MAX) G19MAX = G19OUT(I,2)

     if (G19MAX .gt.0.001) goto 290

     !        T = PERCENT TREE DEFOLIATION

     T = (P1 / (P2 + P3 * EXP(P1 * DPMAX / 100.0))) * 100.0
     goto 300

     290 T = 97.7911 + (G19MAX / 5.0) * 2.2089
     if (G19MAX .ge.5.0) T = 100.0

     300 if (T .le.15.0) ITAB = 1

     !*  For Debug purposes only          *

     !*       WRITE(*,*)
     !*   & ' DPMAX (Branch % defol) = ',DPMAX,'   T (Tree % Defol) = ',T

     !        Determine Tree Defoliation Class for references to AMORT table.

     if (T .gt.15.0.and. T .le.35.0) ITAB = 2
     if (T .gt.35.0.and. T .le.65.0) ITAB = 3
     if (T .gt.65.0.and. T .le.85.0) ITAB = 4
     if (T .gt.85.0.and. T .le.95.0) ITAB = 5
     if (T .gt.95.0.and. T .le.99.5) ITAB = 6
     if (T .gt.99.5) ITAB = 7
     if (IZ6(I) .eq.2) ITAB = ITAB + 7

     !        IZ6 IS SPECIES CODE IN THE DFTM BRANCH MODEL
     !        ( DFIR IF IZ6 = 1 AND GFIR IF IZ6 = 2 )

     !        ITAB  WILL NOW BE THE PROPER ROW FOR TREE CLASS ICLASS
     !        IN THE AMORT TABLE.

     ITABSV(I) = ITAB
     TSAVE(I) = T

     do J = I1,I2
        IP = IPT(J)

        !           NOW CALCULATE PR(MORTALITY) FROM THE AMORT TABLE
        !           NOTE THAT THIS INCLUDES DIRECT AND SECONDARY MORTALITY

        PRMORT = AMORT(ITAB,1) + AMORT(ITAB,2)

        !           CALCULATE THE NORMAL MORTALITY RATE (PRNORM) APPLICABLE
        !           IF THERE HAD BEEN NO DFTM OUTBREAK.

        PRNORM = WK2(IP) / PROB(IP)

        if (PRMORT .lt. PRNORM) PRMORT = PRNORM

        if (ITAB .eq.1.or. ITAB .eq.8) then

           !              Use maximum branch defoliation to interpolate between
           !              the normal mortality rate and the AMORT mortality rate
           !              when defoliation is very light.

           DELTA = (PRMORT - PRNORM) * (1.0 - (DPMAX / 68.03))
           if (DELTA .ge.0.0) PRMORT = PRMORT - DELTA
        end if

        !           Save DFTM mortality losses in WK2.

        WK2(IP) = PROB(IP) * PRMORT

        310 continue
     end do
     320 continue
  end do

  if (ITMSLV .eq.1) then

     !        IF SALVAGE METHOD (ITMSLV) IS 1 THEN:
     !        SET SALVAGE OPTION FOR ICYC+1
     !        THE SALVAGE CODE (KUTKOD FOR THINPRSC) IS 73

     SALKOD = 73

     PRMS(1) = EFF
     PRMS(2) = SALKOD
     call OPADD (IY(ICYC+1),229,0,2,PRMS,K)
     if (K .ne.0) ITMSLV = 0
     call OPINCR (IY, ICYC, NCYC)
  end if

  if (ITMREP .gt.1) write (JODFTM,340)
  340 format(//44('-'),'  SUMMARY OF TREE CLASS CHARACTERISTICS  ',&
       &46('-')//23('-'),' BEFORE OUTBREAK ',30('-'),2X,22('-'),&
       &' AFTER OUTBREAK ',21('-')/T48,'MIDCROWN SAMPLE BRANCH',&
       &T74,'PRECENTAGE',T95,'--- 5 YEAR CHANGE IN: ---'/T14,&
       &'RECORDS',T48,23('-'),3X,'DEFOLIATION    FIVE   DIAMETER',&
       &7X,'HEIGHT',6X,'AMOUNT OF'/'TREE',T14,'PER',4X,'TREES',&
       &T42,'LIVE   PERCENT  FOLIAGE FIRST',T73,13('-'),'   YEAR  ',&
       &10('-'),3X,12('-'),'   TOP-KILL'/'CLASS  HOST  TREE   ',&
       &'PER    DBH  HEIGHT  CROWN    NEW    BIOMASS INSTAR  BRANCH ',&
       &'  TREE   MORT  NET   LOSS    NET    LOSS  ',10('-')/&
       &'NO.    SPP.  CLASS  ACRE   (IN)   (FT)  RATIO  FOLIAGE',&
       &'  (GRAMS) LARVAE    (%)    (%)    RATE  (IN)  (IN)    ',&
       &'(FT)   (FT)  (FT)   (%)'/70('-'),2X,59('-'))

  !     CALCULATE GROWTH LOSSES AND TOP-KILL DAMAGES DUE TO DFTM

  if (ITMREP .gt.1) then

     !        INITIALIZE WEIGHTED AVERAGES FOR SPECIES AND HOST SUMMARIES
     !        ( USE PREFIX W )

     !        ( I=1: DF   I=2: GF     I=3: ALL HOST )

     do I = 1,3

        WDBH(I) = 0.0
        WHTG(I) = 0.0
        WHT(I) = 0.0
        WDG(I) = 0.0
        WCR(I) = 0.0
        WCBASE(I) = 0.0
        WHTLOS(I) = 0.0
        WDGLOS(I) = 0.0
        WPCTKL(I) = 0.0
        WTKILL(I) = 0.0
        WEGGS(I) = 0.0
        WRECS(I) = 0.0
        WZ4(I) = 0.0
        WZ2(I) = 0.0
        WZ3(I) = 0.0
        WBDEF(I) = 0.0
        WTDEF(I) = 0.0
        WPMORT(I) = 0.0
        350 continue
     end do
  end if

  do I = 1,IFIN
     I1 = ISC(I,1)
     I2 = ISC(I,2)

     if (ITMREP .gt.1) then

        !          INITIALIZE WEIGHTED AVERAGES FOR TREE CLASS SUMMARY STATISTICS

        ADBH = 0.0
        AHTG = 0.0
        AHT = 0.0
        ADG = 0.0
        ACR = 0.0
        ACBASE = 0.0
        AHTLOS = 0.0
        ADGLOS = 0.0
        APCTKL = 0.0
        ATKILL = 0.0
     end if

     !       COMPUTE MAXIMUM BRANCH DEFOLIATION (EITHER PHASE II OR III)

     DPMAX = DPCENT(I,1)
     if (DPCENT(I,2) .gt. DPMAX) DPMAX = DPCENT(I,2)

     !       Set AMORT table tree defoliation class index (ITAB)
     !       and percent total tree defoliation for the tree class (T)

     ITAB = ITABSV(I)
     T = TSAVE(I)

     !       ALLOCATE DAMAGE TO ALL TREES IN CLASS I

     do J = I1,I2
        IP = IPT(J)
        NRECS = I2 - I1 + 1
        K = 0
        PCKILL = 0.0
        HTGLOS = 0.0
        TKILL = 0.0
        DGLOSS = 0.0
        PRSUM = 0.0

        !         CALCULATE HEIGHT TO CROWN BASE BEFORE ANY HEIGHT REDUCTION.

        ICRI = ICR(IP)
        if (ICRI .lt.0) ICRI = -ICRI
        CBASE = HT(IP) * (100 - ICRI) / 100.0
        CROWN = HT(IP) - CBASE
        CR = ICRI / 100.0

        !         RECONSTRUCT PRMORT

        PRMORT = WK2(IP) / PROB(IP)

        ACBASE = ACBASE + CBASE * PROB(IP)
        ADBH = ADBH + DBH(IP) * PROB(IP)
        AHT = AHT + HT(IP) * PROB(IP)
        ACR = ACR + CR * PROB(IP)

        !         DETERMINE TOP-KILL DAMAGE

        call TMRANN(PRTOPK)

        if (PRTOPK .lt. AMORT(ITAB,3) .and. DPMAX .gt.0.0001) then

           !            Top-Kill has occured.  DETERMINE THE TOP-KILL CLASS: K

           PRSUM = 0.0

           do K = 1,5

              !              Accumulate top-kill probabilities from AMORT table until
              !              the sum is greater than the probability returned by the
              !              random number generator.  The loop (1 thru 5) during
              !              which this happens determines the top-kill class and the
              !              loop is exited.

              PRSUM = PRSUM + AMORT(ITAB,4+K)

              if (PRTOPK .le. PRSUM) then

                 !                 Process the tree for the current top-kill class (K)
                 !                 and exit the loop.

                 if (K .eq.1) then

                    !                    Kill one year of leader growth and exit loop

                    HTGLOS = HTG(IP) * (1.0 / FINT)
                    HTG(IP) = HTG(IP) - HTGLOS
                 else

                    !                    Use random linear interpolation to compute percent of
                    !                    total crown top-killed

                    call TMRANN(RANDOM)
                    PCKILL = TKBOT(K) + RANDOM * (TKTOP(K) - TKBOT(K))

                    !                    NORMHT CONTAINS THE HEIGHT OF THE TREE JUST BEFORE
                    !                    TRUNCATION (TOP-KILL), AND ITRUNC CONTAINS THE HEIGHT
                    !                    OF TRUNCATION.  THESE VARIABLES MUST BE USED
                    !                    IN ALL SUBSEQUENT VOLUME CALCULATIONS SO THAT THE
                    !                    PROPER FORM OF THE TREE IS MAINTAINED. TO REMOVE
                    !                    THIS CULL AT THE TOP, CALCULATE VOLUME OF A BEHRE
                    !                    HYPERBOLOID WITH TOTAL HEIGHT, NORMHT, AND TRUNCATION
                    !                    HEIGHT AT ITRUNC.  NORMHT AND ITRUNC ARE NON-ZERO ONLY
                    !                    FOR TRUNCATED TREES; THUS NORMHT AND ITRUNC ARE ZEROED
                    !                    OUT AT RUN INITIATION.  THESE TWO VARIABLES ARE CHANGED
                    !                    ONLY IF JTRUNK = 1.  SEE VOLS FOR FURTHER DESCRIPTION.

                    HTGLOS = CROWN * PCKILL
                    TKILL = HTGLOS
                    HTRUNC = HT(IP) - HTGLOS
                    JTRUNK = 0

                    if (HTGLOS .gt.5.0.and. HT(IP) .gt.20.0) then
                       JTRUNK = 1
                    end if

                    !                    TEST TO SEE IF THE TREE HAS BEEN PREVIOUSLY TOP-KILLED
                    !                    BELOW THE CURRENT HEIGHT OF TRUNCATION.

                    if ((ITRUNC(IP) / 100.0) .lt. HTRUNC&
                         &.and. ITRUNC(IP) .gt.0) JTRUNK = 0

                    if (JTRUNK .eq.1) then

                       !                       Truncate the top-killed trees.

                       NORMHT(IP) = IFIX(HT(IP) * 100.0 + 0.5)
                       ITRUNC(IP) = IFIX(HTRUNC * 100.0 + 0.5)
                    end if

                    !                    Adjust height and height growth to reflect truncation.

                    HT(IP) = HTRUNC
                    HTG(IP) = HTG(IP) - HTGLOS
                 end if

                 !                 Exit loop

                 goto 370
              end if

              !            End of DO loop to determine top-kill class

              360 continue
           end do
           370 continue
        else

           !            No top-kill has occured.
           !            Calculate annual height reduction.

           HTGLOS = HTG(IP) * DPMAX / 100.0 * (1.0 / FINT)
           HTG(IP) = HTG(IP) - HTGLOS
        end if

        !         REDUCE DIAMETER GROWTH

        !         NOTE THAT DBH HAS NOT BEEN UPDATED IN TREGRO YET, SO
        !         DO NOT SUBTRACT DGLOSS FROM DBH.

        DGLOSS = DG(IP) * (1.0 - RGLOSS(ITAB))
        if (DGLOSS .gt. DG(IP)) DGLOSS = DG(IP)
        DG(IP) = DG(IP) - DGLOSS

        !         MODIFY CROWN RATIO FOR THE REVISED HT.

        CR = (1.0 - (CBASE / HT(IP))) * 100.0
        ICRI = IFIX(CR + 0.5)

        !         CLASSIFY TREES AS LIVE CULLS IF CROWN RATIO IS < 5% OR
        !         IF THE HEIGHT OF TRUNCATION IS LESS THAN 17 FT.

        if (ITRUNC(IP) .gt.0.and. ITRUNC(IP) .lt.1700) IMC(IP) = 3
        if (ICRI .lt.5) IMC(IP) = 3
        if (ICRI .lt.5) ICRI = 5
        ICR(IP) = ICRI

        !         SET ICR NEGATIVE IF TREE HAS BEEN TOP-KILLED;
        !         THIS SIGNALS CROWN NOT TO CALCULATE A CHANGE
        !         IN CROWN RATIO.

        if (JTRUNK .eq.1.and. ICR(IP) .gt.0) ICR(IP) = -ICR(IP)

        if (ITMREP .lt.2) then
           continue
        else

           !            Accumulate weighted averages

           ADG = ADG + DG(IP) * PROB(IP)
           AHTG = AHTG + HTG(IP) * PROB(IP)
           ADGLOS = ADGLOS + DGLOSS * PROB(IP)
           AHTLOS = AHTLOS + HTGLOS * PROB(IP)
           APCTKL = APCTKL + PCKILL * PROB(IP)
           ATKILL = ATKILL + TKILL * PROB(IP)
        end if

        if (HTG(IP) .lt.0.0) HTG(IP) = 0.0

        !         IF A SALVAGE CUT IS TO BE PERFORMED (ITMSLV = 1) AND
        !         DEFOLIATION IS ABOVE THE MINIMUM SPECIFIED (T >= TMDEFL)
        !         THEN SET KUTKOD TO THE SALVAGE CODE (SALKOD)

        if (ITMSLV .eq.1.and. T .ge. TMDEFL) then
           KUTKOD(IP) = SALKOD
        end if

        !       End of DO loop to allocate damage to trees.

        380 continue
     end do

     if (ITMREP .gt.1) then

        K = IZ6(I)

        390 continue

        !          Accumulate weighted averages for species and host summaries
        !          (1) DF  (2) GF  (3) All Hosts

        WDBH(K) = WDBH(K) + ADBH
        WHTG(K) = WHTG(K) + AHTG
        WHT(K) = WHT(K) + AHT
        WDG(K) = WDG(K) + ADG
        WCR(K) = WCR(K) + ACR
        WCBASE(K) = WCBASE(K) + ACBASE
        WDGLOS(K) = WDGLOS(K) + ADGLOS
        WHTLOS(K) = WHTLOS(K) + AHTLOS
        WPCTKL(K) = WPCTKL(K) + APCTKL
        WTKILL(K) = WTKILL(K) + ATKILL
        WRECS(K) = WRECS(K) + NRECS
        WZ4(K) = WZ4(K) + Z4(I)
        WZ2(K) = WZ2(K) + Z2(I) * Z4(I)
        WZ3(K) = WZ3(K) + Z3(I) * Z4(I)
        WEGGS(K) = WEGGS(K) + EGGS(I) * Z4(I)
        WBDEF(K) = WBDEF(K) + DPMAX * Z4(I)
        WTDEF(K) = WTDEF(K) + T * Z4(I)
        WPMORT(K) = WPMORT(K) + PRMORT * Z4(I)

        if (K .lt.3) then

           !             Set array index for ALL HOSTS and accumulate the values.

           IJUMP = 1
           K = 3
           goto 390
        else
           IJUMP = 0
        end if

        !          NOTE: Z4(I) = SUM(OVER TREE RECORDS IN CLASS I) OF PROB(IP) =
        !                        NUMBER OF T/A IN CLASS I.

        ADBH = ADBH / Z4(I)
        AHTG = AHTG / Z4(I)
        AHT = AHT / Z4(I)
        ADG = ADG / Z4(I)
        ACR = ACR / Z4(I)
        ACBASE = ACBASE / Z4(I)
        ADGLOS = ADGLOS / Z4(I)
        AHTLOS = AHTLOS / Z4(I)
        APCTKL = 100.0 * APCTKL / Z4(I)
        ATKILL = ATKILL / Z4(I)

        !          All trees in class I have been assessed for DFTM damage

        !          SKIP A LINE IF A NEW SPECIES IS ENCOUNTERED.

        if (I .gt.1.and. IZ6(I) .ne. IZ6(I-1)) write(JODFTM,*)

        410 format (I4)
        write(JODFTM,420) I,KSPP(IZ6(I)),NRECS,Z4(I),ADBH,AHT,ACR,&
             &Z2(I),Z3(I),EGGS(I),DPMAX,T,PRMORT,ADG,ADGLOS,&
             &AHTG,AHTLOS,ATKILL,APCTKL
        if (JODFEC .gt.0) write (JODFEC,420) I,KSPP(IZ6(I)),NRECS,&
             &Z4(I),ADBH,AHT,ACR,Z2(I),Z3(I),EGGS(I),DPMAX,T,PRMORT,&
             &ADG,ADGLOS,AHTG,AHTLOS,ATKILL,APCTKL
        420 format(I4,4X,A2,I7,F7.1,2F7.1,F7.2,F9.1,F8.1,F7.1,F9.2,&
             &F7.2,F7.3,2F6.2,F8.2,F7.2,2F6.1)
     end if

     430 continue
  end do

  if (ITMREP .gt.1) then

     !        Compute DF, GF and ALL HOST grand averages.

     write (JODFTM,*)
     if (JODFEC .gt.0) write (JODFEC,*)

     do K = 1,3
        if (K .eq.1.and..not. LDF) goto 450
        if (K .eq.2.and..not. LGF) goto 450
        if (K .eq.3.and..not. (LDF .and. LGF)) goto 450
        WDBH(K) = WDBH(K) / WZ4(K)
        WHTG(K) = WHTG(K) / WZ4(K)
        WHT(K) = WHT(K) / WZ4(K)
        WDG(K) = WDG(K) / WZ4(K)
        WCR(K) = WCR(K) / WZ4(K)
        WCBASE(K) = WCBASE(K) / WZ4(K)
        WDGLOS(K) = WDGLOS(K) / WZ4(K)
        WHTLOS(K) = WHTLOS(K) / WZ4(K)
        WPCTKL(K) = WPCTKL(K) / WZ4(K) * 100.0
        WTKILL(K) = WTKILL(K) / WZ4(K)
        WRECS(K) = WRECS(K) / NUMCLS(K)
        WZ2(K) = WZ2(K) / WZ4(K)
        WZ3(K) = WZ3(K) / WZ4(K)
        WEGGS(K) = WEGGS(K) / WZ4(K)
        WBDEF(K) = WBDEF(K) / WZ4(K)
        WTDEF(K) = WTDEF(K) / WZ4(K)
        WPMORT(K) = WPMORT(K) / WZ4(K)
        WZ4(K) = WZ4(K) / NUMCLS(K)

        write (JODFTM,440) KSPP(K), WRECS(K), WZ4(K), WDBH(K),&
             &WHT(K), WCR(K), WZ2(K), WZ3(K), WEGGS(K), WBDEF(K),&
             &WTDEF(K), WPMORT(K), WDG(K), WDGLOS(K), WHTG(K),&
             &WHTLOS(K), WTKILL(K), WPCTKL(K)
        if (JODFEC .gt.0) write (JODFEC,440) KSPP(K), WRECS(K),&
             &WZ4(K), WDBH(K), WHT(K), WCR(K), WZ2(K), WZ3(K),&
             &WEGGS(K), WBDEF(K), WTDEF(K), WPMORT(K), WDG(K),&
             &WDGLOS(K), WHTG(K), WHTLOS(K), WTKILL(K), WPCTKL(K)
        440 format('AVERAGE ',A4,F5.1,F7.1,2F7.1,F7.2,F9.1,F8.1,&
             &F7.1,F9.2,F7.2,F7.3,2F6.2,F8.2,F7.2,2F6.1)

        450 continue
     end do
     if (JODFEC .gt.0) write (JODFEC,*)
  end if

  !     WRITE OUTPUT

  if (LPUNCH) write (JOTMDK,460)
  460 format ('END OF PUNCH OUTPUT')

  if (TMDTRE) then
     write (JODFTM,470)
     470 format ('DUMP OF INDIVIDUAL TREE RECORDS:'//,'  IPT IMC '&
          &,'SPECIES',5X,'WK2',8X,'PROB',11X,'HT',10X,'HTG',10X,'DBH',&
          &11X,'DG',4X,'ICR',4X,'NORMHT',4X,'ITRUNC KUTKOD',//)

     do II = 1, ITRN
        I = IPT(II)
        write (JODFTM,480) I,IMC(I),ISP(I),WK2(I),PROB(I),HT(I),&
             &HTG(I),DBH(I),DG(I),ICR(I),NORMHT(I),ITRUNC(I),KUTKOD(I)
        480 format (I5,2I4,6F13.3,I4,2I10,I3)
        490 continue
     end do
  end if

  return
end
