!============================================!
!   Compute crown ratios and crown changes   !
!============================================!
! AK $Id$
subroutine CROWN
  implicit none

  !  THIS ROUTINE IS CALLED FROM **CRATET** TO DUB
  !  MISSING VALUES, AND BY **TREGRO** TO COMPUTE CHANGE DURING
  !  REGULAR CYCLING.  ENTRY **CRCONS** IS CALLED BY **RCON** TO
  !  LOAD MODEL CONSTANTS THAT ARE SITE DEPENDENT AND NEED ONLY
  !  BE RESOLVED ONCE.  A CALL TO **DUBSCR** IS ISSUED TO DUB
  !  CROWN RATIO FOR DEAD TREES.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.F77'
  include 'COEFFS.F77'
  include 'CONTRL.F77'
  include 'OUTCOM.F77'
  include 'PDEN.F77'
  include 'PLOT.F77'
  include 'VARCOM.F77'
  include 'CRCOEF.f90'

  ! Variables
  logical :: DEBUG

  integer :: I,I1,I2,I3,IACTK,ICRI,IDATE,IDT,IG,IGRP,IGSP,IITRE,ISPC
  integer :: ISPCC,IULIM,J1,JJ,NP,NTODO,IWHO
  integer :: ISORT(MAXTRE),MYACTS(1)

  real    :: CHG,CL,CR,CRLN,CRMAX,D,H,HD,HN,PDIFPY
  real    :: PRD,DBHLO,DBHHI,SDICS,SDICZ,A,B,X,B1,B2,B3,B4
  real    :: CRNEW(MAXTRE),PRM(5),ZRD(MAXPLT),XMAX
  real    :: BAPLT,TPAPLT,QMDPLT

  !  DATA STATEMENTS:

  data MYACTS/81/

  !  SPECIES ORDER:
  !  1   2   3   4   5   6   7   8   9   10  11  12
  !  SF  AF  YC  TA  WS  LS  BE  SS  LP  RC  WH  MH

  !  13  14  15  16  17  18  19  20  21  22  23
  !  OS  AD  RA  PB  AB  BA  AS  CW  WI  SU  OH

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'CROWN',5,ICYC)

  if (DEBUG) write(JOSTND,3)ICYC
  3 format(' ENTERING SUBROUTINE CROWN  CYCLE =',I5)

  !  CALL SDICAL TO LOAD THE POINT MAX SDI WEIGHTED BY SPECIES ARRAY XMAXPT()
  !  RECENT DEAD ARE NOT INCLUDED. IF THEY NEED TO BE, SDICAL.F NEEDS MODIFIED
  !  TO DO SO. IWHO PARAMETER HAS NO AFFECT ON XMAXPT ARRAY VALUES.

  IWHO = 2
  call SDICAL (IWHO, XMAX)

  !  LOAD SDI (ZEIDI) FOR INDIVIDUAL POINTS.
  !  ALL SPECIES AND ALL SIZES INCLUDED FOR THIS CALCULATION.
  !  SDI USED IN CR CALCULATION

  DBHLO = 0.0
  DBHHI = 500.0
  ISPC = 0
  IWHO = 1
  I2 = INT(PI)

  do I1 = 1, I2
     call SDICLS (ISPC,DBHLO,DBHHI,IWHO,SDICS,SDICZ,A,B,I1)
     ZRD(I1) = SDICZ
  end do

  ! INITIALIZE CROWN VARIABLES TO BEGINNING OF CYCLE VALUES.

  if (LSTART) then
     do JJ = 1,MAXTRE
        CRNEW(JJ) = 0.0
        ISORT(JJ) = 0
        10 continue
     end do
  end if

  !  GO TO DUB CROWNS ON DEAD TREES IF NO LIVE TREES IN INVENTORY

  if ((ITRN.le.0).and.(IREC2.lt.MAXTP1))go to 74

  ! IF THERE ARE NO TREE RECORDS, THEN RETURN

  if (ITRN.eq.0) then
     return
  else if (TPROB.le.0.0) then
     do I = 1,ITRN
        ICR(I) = ABS(ICR(I))
     end do
     return
  end if

  !  PROCESS CRNMULT KEYWORD.

  call OPFIND(1,MYACTS,NTODO)
  if (NTODO .eq. 0)go to 25
  do I = 1,NTODO
     call OPGET(I,5,IDATE,IACTK,NP,PRM)
     IDT = IDATE
     call OPDONE(I,IDT)
     ISPCC = INT(PRM(1))

     !  ISPCC<0 CHANGE FOR ALL SPECIES IN THE SPECIES GROUP
     !  ISPCC=0 CHANGE FOR ALL SPEICES
     !  ISPCC>0 CHANGE THE INDICATED SPECIES

     if (ISPCC .lt. 0) then
        IGRP = -ISPCC
        IULIM = ISPGRP(IGRP,1)+1
        do IG = 2,IULIM
           IGSP = ISPGRP(IGRP,IG)
           if (PRM(2) .ge. 0.0)CRNMLT(IGSP) = PRM(2)
           if (PRM(3) .gt. 0.0)DLOW(IGSP) = PRM(3)
           if (PRM(4) .gt. 0.0)DHI(IGSP) = PRM(4)
           if (PRM(5) .gt. 0.0)ICFLG(IGSP) = 1
           21 continue
        end do
     else if (ISPCC .eq. 0) then
        do ISPCC = 1,MAXSP
           if (PRM(2) .ge. 0.0)CRNMLT(ISPCC) = PRM(2)
           if (PRM(3) .gt. 0.0)DLOW(ISPCC) = PRM(3)
           if (PRM(4) .gt. 0.0)DHI(ISPCC) = PRM(4)
           if (PRM(5) .gt. 0.0)ICFLG(ISPCC) = 1
           22 continue
        end do
     else
        if (PRM(2) .ge. 0.0)CRNMLT(ISPCC) = PRM(2)
        if (PRM(3) .gt. 0.0)DLOW(ISPCC) = PRM(3)
        if (PRM(4) .gt. 0.0)DHI(ISPCC) = PRM(4)
        if (PRM(5) .gt. 0.0)ICFLG(ISPCC) = 1
     end if
     24 continue
  end do
  25 continue

  !  DETERMINE IF DEBUG IS NEEDED
  if (DEBUG)write(JOSTND,9024)ICYC,CRNMLT
  9024 format(/' IN CROWN 9024 ICYC,CRNMLT= ', &
       & I5/((1X,23F6.2)/))

  ! LOAD ISORT ARRAY WITH DIAMETER DISTRIBUTION RANKS.  IF
  ! ISORT(K) = 10 THEN TREE NUMBER K IS THE 10TH TREE FROM
  ! THE BOTTOM IN THE DIAMETER RANKING  (1=SMALL, ITRN=LARGE)

  do JJ = 1,ITRN
     J1 = ITRN - JJ + 1
     ISORT(IND(JJ)) = J1
     11 continue
  end do
  !  DETERMINE IF DEBUG IS NEEDED
  if (DEBUG) then
     write(JOSTND,7900)ITRN,(IND(JJ),JJ = 1,ITRN)
     7900 format(' IN CROWN 7900 ITRN,IND =',I6,/,86(1X,32I4,/))
     write(JOSTND,7901)ITRN,(ISORT(JJ),JJ = 1,ITRN)
     7901 format(' IN CROWN 7900 ITRN,ISORT =',I6,/,86(1X,32I4,/))
  end if

  !  CROWN LOOPS OVER SPECIES AND THEN LOOPS OVER TREE RECORDS
  !  FOR EACH SPECIES

  !  BEGIN OUTER LOOP OVER SPECIES
  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (I1 .eq. 0) go to 70
     I2 = ISCT(ISPC,2)

     !  LOAD SPECIES (ISPC) COEFFCIENTS:
     !  INTERCEPT(B1), HDR(B2), RD(B3), DQMD(B4)
     B1 = CRINT(ISPC)
     B2 = CRHDR(ISPC)
     B3 = CRRD(ISPC)
     B4 = CRDQMD(ISPC)

     !  DETERMINE IF DEBUG IS NEEDED
     if (DEBUG) write(JOSTND,9001) ISPC, &
          & B1,B2,B3,B4
     9001 format(' IN CROWN 9001 ISPC,', &
          & 'B1,B2,B3,B4 = ',/1X,I5,5F10.4)

     !  BEGIN INNER DO LOOP OVER TREE RECORDS OF SPECIES
     do I3 = I1,I2
        I = IND1(I3)
        IITRE = ITRE(I)

        !  IF THIS IS THE INITIAL ENTRY TO 'CROWN' AND THE TREE IN QUESTION
        !  HAS A CROWN RATIO ASCRIBED TO IT, THE WHOLE PROCESS IS BYPASSED.

        if (LSTART .and. ICR(I).gt.0)goto 60

        !  IF ICR(I) IS NEGATIVE, CROWN RATIO CHANGE WAS COMPUTED IN A
        !  PEST DYNAMICS EXTENSION.  SWITCH THE SIGN ON ICR(I) AND BYPASS
        !  CHANGE CALCULATIONS.

        if (LSTART) goto 40
        if (ICR(I).ge.0) go to 40
        ICR(I) = -ICR(I)

        !  DETERMINE IF DEBUG IS NEEDED
        if (DEBUG) write (JOSTND,35) I,ICR(I)
        35 format (' ICR(',I4,') WAS CALCULATED ELSEWHERE AND IS ',I4)
        goto 60
        40 continue
        D = DBH(I)
        H = HT(I)

        !  CALCULATE PLOT LEVEL QMD
        !  IF THERE ARE NO LIVE TREES ON INVENTORY POINT, SET QMD OF
        !  INVENTORY POINT TO 1. THE VALUE OF 1 SHOULD ONLY EVER BE ASSUMED
        !  HERE IF THERE ARE NO LIVE TREES LEFT IN THE STAND.
        BAPLT = PTBAA(ITRE(I))
        TPAPLT = PTPA(ITRE(I))
        if (TPAPLT .gt. 0.0) then
           QMDPLT = SQRT((BAPLT/TPAPLT)/0.005454)
        else
           QMDPLT = 1.0
        end if

        !  ENSURE THAT QMDPLT IS AT LEAST 1. VERY LARGE CROWN RATIO VALUES CAN
        !  BE PRODUCED WHEN QMDPLT IS LESS THAN 1. THIS BEHAVIOR IS RELATED TO QMD
        !  VALUES OF 1 BEING A RARE OBSERVATION IN THE DATASET USED TO FIT CROWN RATIO
        !  RELATIONSHIPS.
        if (QMDPLT .le. 1.0) QMDPLT = 1.0

        !  CALCULATE RELATIVE DENSITY OF INVENTORY POINT
        !  IF SDI MAX OF INVENTORY POINT IS EVER LE 0, SET POINT RELATIVE DENSITY
        !  TO 0.
        if (XMAXPT(ITRE(I)).le.0.0) then
           PRD = 0.0
        else
           PRD = ZRD(ITRE(I)) / XMAXPT(ITRE(I))
        end if

        !  DETERMINE IF DEBUG IS NEEDED
        if (DEBUG) write(JOSTND,*) 'IN CROWN',' ISPC=', ISPC,' IITRE=', &
             & IITRE,' D=',D,' H=',H,' PRD=',PRD,' BAPLT=',BAPLT,' TPAPLT=', &
             & TPAPLT,' QMDPLT=',QMDPLT,' ZRD=',ZRD(ITRE(I)),' XMAXPT=', &
             & XMAXPT(ITRE(I))

        !  BRANCH TO STATEMENT 58 TO HANDLE TREES WITH DBH LESS THAN 1 IN.

        !  CALCULATE SMALL TREE CR IF DBH < 1" FOR ALL SPECIES
        if (D.lt.1.0 .and. LSTART) go to 58

        !  CALCULATE CROWN RATIO USING THE FOLLOWING LOGISITC EQUATION
        !  FORM:

        !  CR = 1/(1 + EXP(X)))
        !  X = B1 + B2*HDR + B3*PRD + B4*DBH/QMDPLT

        !  WHERE
        !  CR: CROWN RATIO
        !  HDR: HEIGHT DIAMETER RATIO OF TREE RECORD
        !  PRD: POINT LEVEL RELATIVE DENSITY
        !  DBH: DIAMETER AT BREAST HEIGHT
        !  QMDPLT: POINT LEVEL QUADRATIC MEAN DIAMETER.

        !  SAME RELATIONSHIP IS USED TO CALCULATE CROWN RATIO FOR DEAD
        !  TREES.

        !  CALCULATE X
        X = B1 &
             & + B2 * LOG(H*12/D) &
             & + B3 * PRD &
             & + B4 * (D/QMDPLT)

        !  CALCULATE CROWN RATIO FROM X
        X = 1.0/(1.0+EXP(X))

        !  DETERMINE IF CROWN RATIO (X) PREDICTION NEEDS TO BE CONSTRAINED
        !  (0.05 > X < 0.95)
        if (X .lt. 0.05) X = 0.05
        if (X .gt. 0.95) X = 0.95

        !  LOAD PREDICTED CR VALUE INTO CRNEW ARRAY
        CRNEW(I) = INT((X * 100) + 0.5)

        !  WRITE DEBUG INFO IF DESIRED

        if (DEBUG)write(JOSTND,9002) I,X,CRNEW(I),ICR(I)
        9002 format(' IN CROWN 9002 WRITE I,X,CRNEW,ICR = ',I5,2F10.5,I5)

        !  COMPUTE THE CHANGE IN CROWN RATIO
        !  CALC THE DIFFERENCE BETWEEN THE MODEL AND THE OLD(OBS)
        !  LIMIT CHANGE TO 1% PER YEAR DECREASE AND 3% PER YEAR INCREASE
        !  (3% BECAUSE OF HIGH GROWTH RATES IN THE AK VARIANT)

        if (LSTART .or. ICR(I).eq.0) go to 9052
        CHG = CRNEW(I) - real(ICR(I))
        PDIFPY = CHG/real(ICR(I))/FINT
        if (PDIFPY.gt.0.03)CHG = real(ICR(I))*(0.03)*FINT
        if (PDIFPY.lt.-0.01)CHG = real(ICR(I))*(-0.01)*FINT

        !  DETERMINE IF DEBUG IS NEEDED
        if (DEBUG)write(JOSTND,9020)I,CRNEW(I),ICR(I),PDIFPY,CHG
        9020 format(/'  IN CROWN 9020 I,CRNEW,ICR,PDIFPY,CHG =', &
             & I5,F10.3,I5,3F10.3)

        if (DBH(I) .ge. DLOW(ISPC) .and. DBH(I) .le. DHI(ISPC)) then
           CRNEW(I) = real(ICR(I)) + CHG * CRNMLT(ISPC)
        else
           CRNEW(I) = real(ICR(I)) + CHG
        end if
        9052 continue
        ICRI = INT(CRNEW(I)+0.5)
        if (LSTART .or. ICR(I) .eq. 0) then
           if (DBH(I).ge.DLOW(ISPC) .and. DBH(I).le.DHI(ISPC)) then
              ICRI = INT(real(ICRI) * CRNMLT(ISPC))
           end if
        end if

        ! CALC CROWN LENGTH NOW

        if (LSTART .or. ICR(I) .eq. 0)go to 55
        CRLN = HT(I)*real(ICR(I))/100.

        ! CALC CROWN LENGTH MAX POSSIBLE IF ALL HTG GOES TO NEW CROWN

        CRMAX = (CRLN+HTG(I))/(HT(I)+HTG(I))*100.0
        if (DEBUG)write(JOSTND,9004)CRMAX,CRLN,ICRI,I,CRNEW(I),CHG
        9004 format('  CRMAX=',F10.2,' CRLN=',F10.2, &
             & ' ICRI=',I10,' I=',I5,' CRNEW=',F10.2,' CHG=',F10.3)

        ! IF NEW CROWN EXCEEDS MAX POSSIBLE LIMIT IT TO MAX POSSIBLE

        if (real(ICRI).gt.CRMAX) ICRI = INT(CRMAX+0.5)
        if (ICRI.lt.10 .and. CRNMLT(ISPC).eq.1.0) ICRI = INT(CRMAX+0.5)

        !  REDUCE CROWNS OF TREES  FLAGGED AS TOP-KILLED ON INVENTORY

        55 if (.not.LSTART .or. ITRUNC(I).eq.0) go to 59
        HN = real(NORMHT(I))/100.0
        HD = HN-real(ITRUNC(I))/100.0
        CL = (real(ICRI)/100.)*HN-HD
        ICRI = INT((CL*100./HN)+.5)

        !  DETERMINE IF DEBUG IS NEEDED
        if (DEBUG)write(JOSTND,9030)I,ITRUNC(I),NORMHT(I),HN,HD,ICRI,CL
        9030 format('  IN CROWN 9030 I,ITRUNC,NORMHT,HN,HD,ICRI,CL = ', &
             & 3I5,2F10.3,I5,F10.3)
        go to 59

        !  CROWNS FOR TREES WITH DBH LT 1 INCH ARE DUBBED HERE.  NO CHANGE
        !  IS CALCULATED UNTIL THE TREE ATTAINS A DBH OF 1 INCH.

        58 continue
        if (ICR(I) .ne. 0) go to 60
        call DUBSCR(ISPC,D,H,PRD,QMDPLT,CR)

        if (DEBUG) write(JOSTND,*) 'RETURN FROM DUBSCR IN CROWN'
        ICRI = INT(CR*100.0+0.5)
        if (DBH(I).ge.DLOW(ISPC) .and. DBH(I).le.DHI(ISPC)) &
             & ICRI = INT(real(ICRI) * CRNMLT(ISPC))

        !  BALANCING ACT BETWEEN TWO CROWN MODELS OCCURS HERE
        !  END OF CROWN RATIO CALCULATION LOOP. BOUND CR ESTIMATE AND FILL
        !  THE ICR VECTOR.

        59 continue
        !  CONSTRAIN CROWN RATIO IF NECCESARY
        if (ICRI.gt.95) ICRI = 95
        if (ICRI .lt. 10 .and. CRNMLT(ISPC).eq.1) ICRI = 10
        if (ICRI.lt.1) ICRI = 1
        ICR(I) = ICRI

        !  END OF TREE RECORD LOOP
        60 continue
     end do
     if (LSTART .and. ICFLG(ISPC).eq.1) then
        CRNMLT(ISPC) = 1.0
        ICFLG(ISPC) = 0
     end if

     !  END OF SPECIES LOOP
     70 continue
  end do
  74 continue

  !  DUB MISSING CROWNS ON CYCLE 0 DEAD TREES.

  !  DETERMINE IF CR NEEDS TO BE CALCULATED FOR DEAD TREES
  if (IREC2 .gt. MAXTRE) go to 80

  !  LOOP THROUGH DEAD TREE RECORDS
  do I = IREC2,MAXTRE
     if (ICR(I) .gt. 0) go to 79
     ISPC = ISP(I)
     D = DBH(I)
     H = HT(I)

     !  CALCULATE PLOT LEVEL QMD
     !  IF THERE ARE NO LIVE TREES ON INVENTORY POINT, SET QMD OF
     !  INVENTORY POINT TO 1. THE VALUE OF 1 SHOULD ONLY EVER BE ASSUMED
     !  HERE IF THERE ARE NO LIVE TREES IN THE STAND.
     BAPLT = PTBAA(ITRE(I))
     TPAPLT = PTPA(ITRE(I))
     if (TPAPLT .gt. 0.0) then
        QMDPLT = SQRT((BAPLT/TPAPLT)/0.005454)
     else
        QMDPLT = 1.0
     end if

     !  ENSURE THAT QMDPLT IS AT LEAST 1. VERY LARGE CROWN RATIO VALUES CAN
     !  BE PRODUCED WHEN QMDPLT IS LESS THAN 1. THIS BEHAVIOR IS RELATED TO QMD
     !  VALUES OF 1 BEING A RARE OBSERVATION IN THE DATASET USED TO FIT CROWN RATIO
     !  RELATIONSHIPS.
     if (QMDPLT .le. 1.0) QMDPLT = 1.0

     !  CALCULATE RELATIVE DENSITY OF INVENTORY POINT
     !  IF SDI MAX OF INVENTORY POINT IS EVER LE 0, SET POINT RELATIVE DENSITY
     !  TO 0.
     if (XMAXPT(ITRE(I)).le.0.0) then
        PRD = 0.0
     else
        PRD = ZRD(ITRE(I)) / XMAXPT(ITRE(I))
     end if

     !  CALL DUBSCR
     call DUBSCR(ISPC,D,H,PRD,QMDPLT,CR)
     ICRI = INT(CR*100.0 + 0.5)
     if (ITRUNC(I).eq.0) go to 78
     HN = real(NORMHT(I))/100.0
     HD = HN-real(ITRUNC(I))/100.0
     CL = (real(ICRI)/100.)*HN-HD
     ICRI = INT((CL*100./HN)+.5)
     78 continue
     if (ICRI.gt.95) ICRI = 95
     if (ICRI .lt. 10) ICRI = 10
     ICR(I) = ICRI
     79 continue
  end do
  !  END OF DEAD TREE LOOP
  80 continue

  !  DETERMINE IF DEBUG IS NEEDED
  if (DEBUG)write(JOSTND,9010)ITRN,(ICR(JJ),JJ = 1,ITRN)
  9010 format(' LEAVING CROWN 9010 FORMAT ITRN,ICR= ',I10,/, &
       & 43(' ',32I4,/))
  if (DEBUG)write(JOSTND,90)ICYC
  90 format(' LEAVING SUBROUTINE CROWN  CYCLE =',I5)
  return

  entry CRCONS

  !  ENTRY POINT FOR LOADING CROWN RATIO MODEL COEFFICIENTS
  !  AND CRNMULT ARRAYS

  !  SPECIES ORDER:
  !  1   2   3   4   5   6   7   8   9   10  11  12
  !  SF  AF  YC  TA  WS  LS  BE  SS  LP  RC  WH  MH

  !  13  14  15  16  17  18  19  20  21  22  23
  !  OS  AD  RA  PB  AB  BA  AS  CW  WI  SU  OH

  !  CROWN RATIO INTERCEPT COEFFICIENTS
  data CRINT/ &
       & -2.818788, -2.818788,  0.655733,  6.469047, -4.02647, &
       & -4.02647,  -2.781735, -2.818788, -0.425173, -0.449694, &
       & -0.364665, -0.47458,  -4.026470,  0.977291, -2.799049, &
       & -1.268650, -1.268650,  0.977291,  0.802511,  0.977291, &
       & -2.406592, -2.406592,  0.977291/

  !  CROWN RATIO HT/DBH RATIO COEFFICIENTS
  data CRHDR/ &
       & 0.623633,  0.623633, -0.064716, -1.495754,  0.655230, &
       & 0.655230,  0.438826,  0.623633,  0.295262,  0.104368, &
       & 0.116994,  0.141604,  0.655230, -0.314026,  0.660530, &
       & 0.147556,  0.147556, -0.314026, -0.143680, -0.314026, &
       & 0.347417,  0.347417, -0.314026/

  !  CROWN RATIO RELATIVE DENSITY COEFFICIENTS
  data CRRD/ &
       & 0.952948, 0.952948, 0.439411, 0.913909, 0.914077, &
       & 0.914077, 0.970251, 0.952948, 0.732675, 0.557638, &
       & 0.573079, 0.590263, 0.914077, 0.848483, 0.987503, &
       & 1.092390, 1.092390, 0.848483, 1.023227, 0.848483, &
       & 0.952753, 0.952753, 0.848483/

  !  CROWN RATIO DQMD COEFFICIENTS - CONSISTENT ACROSS ALL SPECIES
  data CRDQMD/ &
       & -0.174337, -0.174337, -0.174337, -0.051268, -0.051268, &
       & -0.051268, -0.051268, -0.174337, -0.174337, -0.174337, &
       & -0.174337, -0.174337, -0.051268, -0.051268, -0.174337, &
       & -0.051268, -0.051268, -0.051268, -0.051268, -0.051268, &
       & -0.051268, -0.051268, -0.051268/

  !  CRNMULT PARAMETERS
  data CRNMLT/MAXSP*1.0/
  data ICFLG/MAXSP*0/
  data DLOW/MAXSP*0.0/
  data DHI/MAXSP*999.0/

  return
end subroutine CROWN
