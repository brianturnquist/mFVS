!============================================!
!   Compute crown ratios and crown changes   !
!============================================!
! NC $Id$
subroutine CROWN
  implicit none

  !  THIS SUBROUTINE IS USED TO DUB MISSING CROWN RATIOS AND
  !  COMPUTE CROWN RATIO CHANGES FOR TREES THAT ARE GREATER THAN
  !  3 INCHES DBH.  THE EQUATION USED PREDICTS CROWN RATIO FROM
  !  HABITAT TYPE, BASAL AREA, CROWN COMPETITION FACTOR, DBH, TREE
  !  HEIGHT, AND PERCENTILE IN THE BASAL AREA DISTRIBUTION.  WHEN
  !  THE EQUATION IS USED TO PREDICT CROWN RATIO CHANGE, VALUES
  !  OF THE PREDICTOR VARIABLES FROM THE START OF THE CYCLE ARE USED
  !  TO PREDICT OLD CROWN RATIO, VALUES FROM THE END OF THE CYCLE
  !  ARE USED TO PREDICT NEW CROWN RATIO, AND THE CHANGE IS
  !  COMPUTED BY SUBTRACTION.  THE CHANGE IS APPLIED TO ACTUAL
  !  CROWN RATIO.  THIS ROUTINE IS CALLED FROM **CRATET** TO DUB
  !  MISSING VALUES, AND BY **TREGRO** TO COMPUTE CHANGE DURING
  !  REGULAR CYCLING.  ENTRY **CRCONS** IS CALLED BY **RCON** TO
  !  LOAD MODEL CONSTANTS THAT ARE SITE DEPENDENT AND NEED ONLY
  !  BE RESOLVED ONCE.  A CALL TO **DUBSCR** IS ISSUED TO DUB
  !  CROWN RATIO WHEN DBH IS LESS THAN 3 INCHES.  PROCESSING OF
  !  CROWN CHANGE FOR SMALL TREES IS CONTROLLED BY **REGENT**.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'
  include 'OUTCOM.f90'
  include 'PLOT.f90'
  include 'PDEN.f90'
  include 'VARCOM.f90'

  ! Variables
  real    :: CRNEW(MAXTRE),WEIBA(12),WEIBB0(12),WEIBB1(12),WEIBC0(12),WEIBC1(12),C0(12),C1(12),PRM(5),&
       &CRNMLT(12),DLOW(12),DHI(12)
  integer :: MYACTS(1),ICFLG(12),ISORT(MAXTRE)
  integer :: JJ,NTODO,I,NP,IACTK,IDATE,IDT,ISPCC,IGRP,IULIM
  integer :: IG,IGSP,J1,ISPC,I1,I2,I3,IITRE,ICRI
  real    :: RELSDI,ACRNEW,A,B,C,D,H,SCALE,X,RNUMB
  real    :: CHG,PDIFPY,CRLN,CRMAX,HN,HD,CL,TPCT,TPCCF,CR
  data MYACTS/81/
  real    :: BAPLT,QMDPLT,TPAPLT,PRD,DBHLO,DBHHI,SDICS,SDICZ,XMAX
  real    :: ZRD(MAXPLT),HDR
  integer :: IWHO

  !  SPECIES ORDER 1=OC, 2=SP, 3=DF, 4=WF, 5=M , 6=IC, 7=BO, 8=TO,
  !                9=RF, 10=PP, 11=OH, 12 = RW

  logical :: DEBUG

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'CROWN',5,ICYC)
  if (DEBUG) write(JOSTND,3)ICYC
  3 format(' ENTERING SUBROUTINE CROWN CYCLE =',I5)

  !  CALL SDICAL TO LOAD THE POINT MAX SDI WEIGHTED BY SPECIES ARRAY XMAXPT()
  !  RECENT DEAD ARE NOT INCLUDED. IF THEY NEED TO BE, SDICAL.F NEEDS MODIFIED
  !  TO DO SO. IWHO PARAMETER HAS NO AFFECT ON XMAXPT ARRAY VALUES.

  IWHO = 2
  call SDICAL (IWHO, XMAX)

  !  LOAD SDI (ZEIDI) FOR INDIVIDUAL POINTS.
  !  ALL SPECIES AND ALL SIZES INCLUDED FOR THIS CALCULATION.
  !  SDI USED IN CR CALCULATION
  !  POINT SDI IS USED IN CR CALCULATION FOR RW/GS

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

  !  DUB CROWNS ON DEAD TREES IF NO LIVE TREES IN INVENTORY

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
  if (NTODO .eq.0)go to 25
  do I = 1,NTODO
     call OPGET(I,5,IDATE,IACTK,NP,PRM)
     IDT = IDATE
     call OPDONE(I,IDT)
     ISPCC = INT(PRM(1))

     !  ISPCC<0 CHANGE FOR ALL SPECIES IN THE SPECIES GROUP
     !  ISPCC=0 CHANGE FOR ALL SPEICES
     !  ISPCC>0 CHANGE THE INDICATED SPECIES

     if (ISPCC .lt.0) then
        IGRP = -ISPCC
        IULIM = ISPGRP(IGRP,1)+1
        do IG = 2,IULIM
           IGSP = ISPGRP(IGRP,IG)
           if (PRM(2) .ge.0.0)CRNMLT(IGSP) = PRM(2)
           if (PRM(3) .gt.0.0)DLOW(IGSP) = PRM(3)
           if (PRM(4) .gt.0.0)DHI(IGSP) = PRM(4)
           if (PRM(5) .gt.0.0)ICFLG(IGSP) = 1
           21 continue
        end do
     else if (ISPCC .eq.0) then
        do ISPCC = 1,MAXSP
           if (PRM(2) .ge.0.0)CRNMLT(ISPCC) = PRM(2)
           if (PRM(3) .gt.0.0)DLOW(ISPCC) = PRM(3)
           if (PRM(4) .gt.0.0)DHI(ISPCC) = PRM(4)
           if (PRM(5) .gt.0.0)ICFLG(ISPCC) = 1
           22 continue
        end do
     else
        if (PRM(2) .ge.0.0)CRNMLT(ISPCC) = PRM(2)
        if (PRM(3) .gt.0.0)DLOW(ISPCC) = PRM(3)
        if (PRM(4) .gt.0.0)DHI(ISPCC) = PRM(4)
        if (PRM(5) .gt.0.0)ICFLG(ISPCC) = 1
     end if
     24 continue
  end do
  25 continue
  if (DEBUG)write(JOSTND,9024)ICYC,CRNMLT
  9024 format(/' IN CROWN 9024 ICYC,CRNMLT= ',&
       &I5/((1X,11F6.2)/))

  ! LOAD ISORT ARRAY WITH DIAMETER DISTRIBUTION RANKS.  IF
  ! ISORT(K) = 10 THEN TREE NUMBER K IS THE 10TH TREE FROM
  ! THE BOTTOM IN THE DIAMETER RANKING  (1=SMALL, ITRN=LARGE)

  do JJ = 1,ITRN
     J1 = ITRN - JJ + 1
     ISORT(IND(JJ)) = J1
     11 continue
  end do
  if (DEBUG) then
     write(JOSTND,7900)ITRN,(IND(JJ),JJ = 1,ITRN)
     7900 format(' IN CROWN 7900 ITRN,IND =',I6,/,86(1H ,32I4,/))
     write(JOSTND,7901)ITRN,(ISORT(JJ),JJ = 1,ITRN)
     7901 format(' IN CROWN 7900 ITRN,ISORT =',I6,/,86(1H ,32I4,/))
  end if

  !  ENTER THE LOOP FOR SPECIES DEPENDENT VARIABLES

  do ISPC = 1,12
     I1 = ISCT(ISPC,1)
     if (I1.eq.0) go to 70
     I2 = ISCT(ISPC,2)

     ! ESTIMATE MEAN CROWN RATIO FROM SDI, AND ESTIMATE WEIBULL PARAMETERS

     if (SDIDEF(ISPC) .gt.0.) then
        RELSDI = SDIAC / SDIDEF(ISPC)
     else
        RELSDI = 1.0
     end if
     if (RELSDI .gt.1.5)RELSDI = 1.5
     ACRNEW = C0(ISPC) + C1(ISPC) * RELSDI*100.0
     A = WEIBA(ISPC)
     B = WEIBB0(ISPC) + WEIBB1(ISPC) * ACRNEW
     C = WEIBC0(ISPC) + WEIBC1(ISPC)*ACRNEW
     if (B .lt.3.0) B = 3.0
     if (C .lt.2.0) C = 2.0

     if (DEBUG) write(JOSTND,9001) ISPC,SDIAC,ORMSQD,RELSDI,&
          &ACRNEW,A,B,C,SDIDEF(ISPC)
     9001 FORMAT(' IN CROWN 9001 ISPC,SDIAC,ORMSQD,RELSDI,ACRNEW,A,B,       &
          &C,SDIDEF = ',/,1H ,I5,F8.2,F8.4,F8.2,F8.2,4F10.4)
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
        if (DEBUG) write (JOSTND,35) I,ICR(I)
        35 format (' ICR(',I4,') WAS CALCULATED ELSEWHERE AND IS ',I4)
        goto 60
        40 continue
        D = DBH(I)
        H = HT(I)

        !  CALCULATE HEIGHT DIEMTER RATIO
        HDR = (H*12)/D

        !  CALCULATE PLOT LEVEL QMD - USED IN RW/GS CR CALCULATION
        !  IF THERE ARE NO LIVE TREES ON INVENTORY POINT, SET QMD OF INVENTORY
        !  POINT TO 1. THE VALUE OF 1 SHOULD ONLY EVER BE ASSUMED HERE IF THERE
        !  ARE NO LIVE TREES IN THE STAND.
        BAPLT = PTBAA(ITRE(I))
        TPAPLT = PTPA(ITRE(I))
        if (TPAPLT .gt.0.0) then
           QMDPLT = SQRT((BAPLT/TPAPLT)/0.005454)
        else
           QMDPLT = 1.0
        end if

        !  ENSURE THAT QMDPLT IS AT LEAST 1. VERY LARGE CROWN RATIO VALUES CAN
        !  BE PRODUCED WHEN QMDPLT IS LESS THAN 1. THIS BEHAVIOR IS RELATED TO QMD
        !  VALUES OF 1 BEING A RARE OBSERVATION IN THE DATASET USED TO FIT CROWN RATIO
        !  RELATIONSHIPS.
        if (QMDPLT .le.1.0) QMDPLT = 1.0

        !  CALCULATE RELATIVE DENSITY OF INVENTORY POINT - USED IN RW/GS CR CALCULATION.
        !  IF SDI MAX OF INVENTORY POINT IS EVER LE 0, SET POINT RELATIVE DENSITY
        !  TO 0.
        if (XMAXPT(ITRE(I)).le.0.0) then
           PRD = 0.0
        else
           PRD = ZRD(ITRE(I)) / XMAXPT(ITRE(I))
        end if

        !  BRANCH TO STATEMENT 58 TO HANDLE TREES WITH DBH LESS THAN 1 IN.

        if (D.lt.1.0.and. LSTART) go to 58

        !  CALCULATE THE PREDICTED CURRENT CROWN RATIO

        select case(ISPC)

           ! BRANCH FOR RW

        case(12)
           X = -1.021064 + 0.309296*LOG(HDR) +&
                &0.869720*PRD - 0.116274*(D/QMDPLT)
           X = 1.0/(1.0+EXP(X))

           if (DEBUG)write(JOSTND,*)' IN CROWN - RW DEBUG', ' D=',D,&
                &' H=',H,' PRD=',PRD,' QMDPLT=',QMDPLT,' HDR=',HDR,' CR=',X

           ! ALL OTHER SPECIES

        case default
           SCALE = 1.5-RELSDI
           if (SCALE .gt.1.0) SCALE = 1.0
           if (SCALE .lt.0.30) SCALE = 0.30
           if (DBH(I) .gt.0.0) then
              X = (real(ISORT(I)) / real(ITRN)) * SCALE
           else
              call RANN(RNUMB)
              X = RNUMB * SCALE
           end if
        end select

        !  CONSTRAIN CR IF NEEDED
        if (X .lt..05) X = .05
        if (X .gt..95) X = .95

        ! DETERMINE CRNEW BASED ON SPECIES
        if (ISPC .eq.12) then
           CRNEW(I) = X * 10
        else
           CRNEW(I) = A + B*((-1.0*ALOG(1-X))**(1.0/C))
        end if

        !  WRITE DEBUG INFO IF DESIRED

        if (DEBUG)write(JOSTND,9002) I,X,CRNEW(I),ICR(I),LSTART
        9002 format(' IN CROWN 9002 I,X,CRNEW,ICR,LSTART = ',I5,2F10.5,&
             &I5,L2)
        CRNEW(I) = CRNEW(I)*10.0

        !  COMPUTE THE CHANGE IN CROWN RATIO
        !  CALC THE DIFFERENCE BETWEEN THE MODEL AND THE OLD(OBS)
        !  LIMIT CHANGE TO 1% PER YEAR

        if (LSTART .or. ICR(I).eq.0) go to 9052
        CHG = CRNEW(I) - real(ICR(I))
        PDIFPY = CHG/real(ICR(I))/FINT
        if (PDIFPY.gt.0.01)CHG = real(ICR(I))*(0.01)*FINT
        if (PDIFPY.lt.-0.01)CHG = real(ICR(I))*(-0.01)*FINT
        if (DEBUG)write(JOSTND,9020)I,CRNEW(I),ICR(I),PDIFPY,CHG
        9020 format(/'  IN CROWN 9020 I,CRNEW,ICR,PDIFPY,CHG =',&
             &I5,F10.3,I5,3F10.3)
        if (DBH(I) .ge. DLOW(ISPC) .and. DBH(I) .le. DHI(ISPC)) then
           CRNEW(I) = real(ICR(I)) + CHG * CRNMLT(ISPC)
        else
           CRNEW(I) = real(ICR(I)) + CHG
        end if
        9052 ICRI = INT(CRNEW(I)+0.5)
        if (LSTART .or. ICR(I).eq.0) then
           if (DBH(I).ge.DLOW(ISPC) .and. DBH(I).le.DHI(ISPC))&
                &ICRI = INT(real(ICRI) * CRNMLT(ISPC))
        end if

        ! CALC CROWN LENGTH NOW

        if (LSTART .or. ICR(I).eq.0)go to 55
        CRLN = HT(I)*real(ICR(I))/100.

        ! CALC CROWN LENGTH MAX POSSIBLE IF ALL HTG GOES TO NEW CROWN

        CRMAX = (CRLN+HTG(I))/(HT(I)+HTG(I))*100.0
        if (DEBUG)write(JOSTND,9004)CRMAX,CRLN,ICRI,I,CRNEW(I),&
             &CHG
        9004 format(' CRMAX=',F10.2,' CRLN=',F10.2,&
             &' ICRI=',I10,' I=',I5,' CRNEW=',F10.2,' CHG=',F10.3)

        ! IF NEW CROWN EXCEEDS MAX POSSIBLE LIMIT IT TO MAX POSSIBLE

        if (real(ICRI).gt.CRMAX) ICRI = INT(CRMAX+0.5)
        if (ICRI.lt.10.and. CRNMLT(ISPC).eq.1.0)ICRI = INT(CRMAX+0.5)

        !  REDUCE CROWNS OF TREES  FLAGGED AS TOP-KILLED ON INVENTORY

        55 if (.not.LSTART .or. ITRUNC(I).eq.0) go to 59
        HN = real(NORMHT(I))/100.0
        HD = HN-real(ITRUNC(I))/100.0
        CL = (real(ICRI)/100.)*HN-HD
        ICRI = INT((CL*100./HN)+.5)
        if (DEBUG)write(JOSTND,9030)I,ITRUNC(I),NORMHT(I),HN,HD,&
             &ICRI,CL
        9030 format(' IN CROWN 9030 I,ITRUNC,NORMHT,HN,HD,ICRI,CL = ',&
             &3I5,2F10.3,I5,F10.3)
        go to 59

        !  CROWNS FOR TREES WITH DBH LT 3.0 IN ARE DUBBED HERE.  NO CHANGE
        !  IS CALCULATED UNTIL THE TREE ATTAINS A DBH OF 3 INCHES.

        58 continue
        if (ICR(I).ne.0) go to 60
        TPCT = PCT(I)
        TPCCF = PCCF(IITRE)
        call DUBSCR(ISPC,D,H,PRD,QMDPLT,CR,TPCT,TPCCF)
        if (DEBUG) write(JOSTND,*) 'RETURN FORM DUBSCR IN CROWN'
        ICRI = INT(CR*100.0+0.5)
        if (DBH(I).ge.DLOW(ISPC) .and. DBH(I).le.DHI(ISPC)) then
           ICRI = INT(real(ICRI) * CRNMLT(ISPC))
        end if

        !  BALANCING ACT BETWEEN TWO CROWN MODELS OCCURS HERE
        !  END OF CROWN RATIO CALCULATION LOOP.  BOUND CR ESTIMATE AND FILL
        !  THE ICR VECTOR.

        59 continue
        if (ICRI.gt.95) ICRI = 95
        if (ICRI .lt.10.and. CRNMLT(ISPC).eq.1) ICRI = 10
        if (ICRI.lt.1)ICRI = 1
        ICR(I) = ICRI
        60 continue
     end do
     if (LSTART .and. ICFLG(ISPC).eq.1) then
        CRNMLT(ISPC) = 1.0
        ICFLG(ISPC) = 0
     end if
     70 continue
  end do
  74 continue

  !  DUB MISSING CROWNS ON CYCLE 0 DEAD TREES.

  if (IREC2.gt. MAXTRE) go to 80
  do I = IREC2,MAXTRE
     if (ICR(I) .gt.0) go to 79
     ISPC = ISP(I)
     D = DBH(I)
     H = HT(I)
     TPCT = PCT(I)
     IITRE = ITRE(I)
     TPCCF = PCCF(IITRE)

     !  CALCULATE PLOT LEVEL QMD - USED IN RW/GS CR CALCULATION
     !  IF THERE ARE NO LIVE TREES ON INVENTORY POINT, SET QMD OF INVENTORY
     !  POINT TO 1. THE VALUE OF 1 SHOULD ONLY EVER BE ASSUMED HERE IF THERE
     !  ARE NO LIVE TREES IN THE STAND.
     BAPLT = PTBAA(IITRE)
     TPAPLT = PTPA(IITRE)
     if (TPAPLT .gt.0.0) then
        QMDPLT = SQRT((BAPLT/TPAPLT)/0.005454)
     else
        QMDPLT = 1.0
     end if

     !  ENSURE THAT QMDPLT IS AT LEAST 1. VERY LARGE CROWN RATIO VALUES CAN
     !  BE PRODUCED WHEN QMDPLT IS LESS THAN 1. THIS BEHAVIOR IS RELATED TO QMD
     !  VALUES OF 1 BEING A RARE OBSERVATION IN THE DATASET USED TO FIT CROWN RATIO
     !  RELATIONSHIPS.
     if (QMDPLT .le.1.0) QMDPLT = 1.0

     !  CALCULATE RELATIVE DENSITY OF INVENTORY POINT - USED IN RW/GS CR CALCULATION.
     !  IF SDI MAX OF INVENTORY POINT IS EVER LE 0, SET POINT RELATIVE DENSITY
     !  TO 0.
     if (XMAXPT(IITRE).le.0.0) then
        PRD = 0.0
     else
        PRD = ZRD(IITRE) / XMAXPT(IITRE)
     end if

     !  CALL DUBSCR
     call DUBSCR (ISPC,D,H,PRD,QMDPLT,CR,TPCT,TPCCF)
     ICRI = INT(CR*100.0 + 0.5)
     if (ITRUNC(I).eq.0) go to 78
     HN = real(NORMHT(I))/100.0
     HD = HN-real(ITRUNC(I))/100.0
     CL = (real(ICRI)/100.)*HN-HD
     ICRI = INT((CL*100./HN)+.5)
     78 continue
     if (ICRI.gt.95) ICRI = 95
     if (ICRI .lt.10) ICRI = 10
     ICR(I) = ICRI
     79 continue
  end do

  80 continue
  if (DEBUG)write(JOSTND,9010)ITRN,(ICR(JJ),JJ = 1,ITRN)
  9010 format(' LEAVING CROWN 9010 ITRN,ICR= ',I10,/,&
       &43(1H ,32I4,/))
  if (DEBUG)write(JOSTND,90)ICYC
  90 format(' LEAVING SUBROUTINE CROWN CYCLE =',I5)
  return
  entry CRCONS

  !  ENTRY POINT FOR LOADING CROWN RATIO MODEL COEFFICIENTS

  ! SPECIES ORDER
  !  1=OC,  2=SP,  3=DF,  4=WF,  5=M ,  6=IC,  7=BO,
  !  8=TO,  9=RF, 10=PP, 11=OH,  12 = RW

  ! LOAD WEIBULL 'A' PARAMETER BY SPECIES

  data WEIBA/ 12*0.0/

  ! LOAD WEIBULL 'B' PARAMETER EQUATION CONSTANT COEFFICIENT

  data WEIBB0/  0.52909,  0.25115,  0.52909,  0.48464,  0.08402,&
       &0.29964,  0.06607,  0.25667,  0.16601,  0.03685,&
       &0.25667,  0.00000/

  ! LOAD WEIBULL 'B' PARAMETER EQUATION SLOPE COEFFICIENT

  data WEIBB1/  1.00677, 1.05987,  1.00677,  1.01272,  1.10297,&
       &1.05398, 1.10705,  1.06474,  1.08150,  1.09499,&
       &1.06474, 0.00000/

  ! LOAD WEIBULL 'C' PARAMETER EQUATION CONSTANT COEFFICIENT

  data WEIBC0/ -3.48211,  0.33383, -3.48211, -2.78353,  0.91078,&
       &-1.09270,  2.04714,  0.11729,  0.91420,  4.01340,&
       &0.11729,  0.00000/

  ! LOAD WEIBULL 'C' PARAMETER EQUATION SLOPE COEFFICIENT

  data WEIBC1/  1.38780,  0.63833,  1.38780,  1.27283,  0.45819,&
       &0.80687,  0.15070,  0.61681,  0.45768,  0.04946,&
       &0.61681,  0.00000/

  ! LOAD CR=F(SDI) EQUATION CONSTANT COEFFICIENT

  data C0/7.48846, 6.92893, 7.48846,  7.44422,  3.64292,&
       &5.12357, 6.82187, 5.95912,  6.14578,  6.04928,&
       &5.95912, 0.00000/

  ! LOAD CR=F(SDI) EQUATION SLOPE COEFFICIENT

  data C1/-0.02899, -0.04053, -0.02899, -0.04779, -0.00317,&
       &-0.01042, -0.02247, -0.01812, -0.02781, -0.01091,&
       &-0.01812,  0.00000/

  data CRNMLT/12*1.0/
  data ICFLG/12*0/
  data DLOW/12*0.0/
  data DHI/12*99.0/
  return
end
