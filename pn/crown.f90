!============================================!
!   Compute crown ratios and crown changes   !
!============================================!
! PN $Id$
subroutine CROWN
  implicit none

  !  THIS SUBROUTINE IS USED TO DUB MISSING CROWN RATIOS AND
  !  COMPUTE CROWN RATIO CHANGES FOR TREES THAT ARE GREATER THAN
  !  1 INCH DBH.

  !  THIS ROUTINE IS CALLED FROM **CRATET** TO DUB
  !  MISSING VALUES, AND BY **TREGRO** TO COMPUTE CHANGE DURING
  !  REGULAR CYCLING.  ENTRY **CRCONS** IS CALLED BY **RCON** TO
  !  LOAD MODEL CONSTANTS THAT ARE SITE DEPENDENT AND NEED ONLY
  !  BE RESOLVED ONCE.  A CALL TO **DUBSCR** IS ISSUED TO DUB
  !  CROWN RATIO WHEN DBH IS LESS THAN 1 INCH.  PROCESSING OF
  !  CROWN CHANGE FOR SMALL TREES IS CONTROLLED BY **REGENT**.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'ARRAYS.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'
  include 'OUTCOM.f90'
  include 'PDEN.f90'
  include 'VARCOM.f90'

  ! Variables
  logical :: DEBUG
  real    :: CRNEW(MAXTRE),WEIBA(17),WEIBB0(17),WEIBB1(17),WEIBC0(17)
  real    :: WEIBC1(17),C0(17),C1(17)
  integer :: IMAP(MAXSP),ISORT(MAXTRE)
  real    :: CRNMLT(MAXSP),DLOW(MAXSP),DHI(MAXSP)
  integer :: MYACTS(1)
  real    :: PRM(5)
  integer :: ICFLG(MAXSP)
  integer :: JJ,NTODO,I,NP,IACTK,IDATE,IDT,ISPCC,IGRP,IULIM,IG,IGSP
  integer :: J1,ISPC,I1,I2,JSPC,I3,IITRE,ICRI
  real    :: RELSDI,ACRNEW,A,B,C,D,H,SCALE,X,RNUMB
  real    :: CHG,PDIFPY,CRLN,CRMAX,HN,HD,CL,TPCT,TPCCF,CR

  integer :: IWHO
  real    :: BAPLT,DBHHI,DBHLO,HDR,PRD,QMDPLT,SDICS,SDICZ,TPAPLT,XMAX
  real    :: ZRD(MAXPLT)

  data MYACTS/81/
  data IMAP/1,2*2,2*3,17,4,15,2*11,16,6,2*5,6,7,11,&
       &8,9,10,12,13,7*14,4*11,6*14/

  !  SPECIES ORDER 1=SF,  2=WF,  3=GF,  4=AF,  5=RF,  6=SS,  7=NF,  8=YC,
  !                9=IC, 10=ES, 11=LP, 12=JP, 13=SP, 14=WP, 15=PP, 16=DF,
  !               17=RW, 18=RC, 19=WH, 20=MH, 21=BM, 22=RA, 23=WA, 24=PB,
  !               25=GC, 26=AS, 27=CW, 28=WO, 29=J , 30=LL, 31=WB, 32=KP,
  !               33=PY, 34=DG, 35=HT, 36=CH, 37=WI, 38=  , 39=OT

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'CROWN',5,ICYC)

  if (DEBUG) write(JOSTND,3)ICYC
  3 format(' ENTERING SUBROUTINE CROWN  CYCLE =',I5)

  !  START OF JUNE 2021 EDITS
  !  CALL SDICAL TO LOAD THE POINT MAX SDI WEIGHTED BY SPECIES ARRAY XMAXPT()
  !  RECENT DEAD ARE NOT INCLUDED. IF THEY NEED TO BE, SDICAL.F NEEDS MODIFIED
  !  TO DO SO. IWHO PARAMETER HAS NO AFFECT ON XMAXPT ARRAY VALUES.

  IWHO = 2
  call SDICAL (IWHO, XMAX)

  !  LOAD SDI (ZEIDI) FOR INDIVIDUAL POINTS.
  !  ALL SPECIES AND ALL SIZES INCLUDED FOR THIS CALCULATION.
  !  SDI USED IN CR CALCULATION
  !  POINT SDI IS USED IN CR CALCULATION FOR RW

  DBHLO = 0.0
  DBHHI = 500.0
  ISPC = 0
  IWHO = 1
  I2 = INT(PI)

  do I1 = 1, I2
     call SDICLS (ISPC,DBHLO,DBHHI,IWHO,SDICS,SDICZ,A,B,I1)
     ZRD(I1) = SDICZ
  end do

  !  END JUNE 2021 EDITS

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

  !      IF(DEBUG)WRITE(JOSTND,9023)ICYC,CRNMLT
  ! 9023 FORMAT(/' IN CROWN 9023 ICYC,CRNMLT= ',
  !     & I5/((1X,11F6.2)/))

  call OPFIND(1,MYACTS,NTODO)
  if (NTODO .eq.0)go to 25
  do I = 1,NTODO
     call OPGET(I,5,IDATE,IACTK,NP,PRM)
     IDT = IDATE
     call OPDONE(I,IDT)
     ISPCC = INT(PRM(1))
     !      IF(DEBUG)WRITE(JOSTND,*)' PRM= ',PRM

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

  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (I1.eq.0) go to 70
     I2 = ISCT(ISPC,2)
     JSPC = IMAP(ISPC)

     ! ESTIMATE MEAN CROWN RATIO FROM SDI, AND ESTIMATE WEIBULL PARAMETERS

     if (SDIDEF(ISPC) .gt.0.) then
        RELSDI = SDIAC / SDIDEF(ISPC)
     else
        RELSDI = 1.0
     end if
     if (RELSDI .gt.1.5)RELSDI = 1.5
     ACRNEW = C0(JSPC) + C1(JSPC) * RELSDI*100.0
     A = WEIBA(JSPC)
     B = WEIBB0(JSPC) + WEIBB1(JSPC) * ACRNEW
     C = WEIBC0(JSPC) + WEIBC1(JSPC)*ACRNEW
     if (B .lt.3.0) B = 3.0
     if (C .lt.2.0) C = 2.0
     if (DEBUG) write(JOSTND,9001) ISPC,SDIAC,ORMSQD,RELSDI,&
          &ACRNEW,A,B,C,SDIDEF(ISPC)
     9001 format(' IN CROWN 9001 ISPC,SDIAC,ORMSQD,RELSDI,ACRNEW,A,B,',&
          &'C,SDIDEF = '/1X,I5,F8.2,F8.4,F8.2,F8.2,4F10.4)
     do I3 = I1,I2
        I = IND1(I3)
        IITRE = ITRE(I)

        !  IF THIS IS THE INITIAL ENTRY TO 'CROWN' AND THE TREE IN QUESTION
        !  HAS A CROWN RATIO ASCRIBED TO IT, THE WHOLE PROCESS IS BYPASSED.

        if (LSTART .and. ICR(I).gt.0)goto 60

        !  IF ICR(I) IS NEGATIVE, CROWN RATIO CHANGE WAS COMPUTED IN A
        !  PEST DYNAMICS EXTENSION.  SWITCH THE SIGN ON ICR(I) AND BYPASS
        !  CHANGE CALCULATIONS.

        if (LSTART) go to 40
        if (ICR(I).ge.0) go to 40
        ICR(I) = -ICR(I)
        if (DEBUG) write (JOSTND,35) I,ICR(I)
        35 format (' ICR(',I4,') WAS CALCULATED ELSEWHERE AND IS ',I4)
        goto 60
        40 continue
        D = DBH(I)
        H = HT(I)

        !  2021 ADDITION BEGIN

        !  CALCULATE HEIGHT DIAMETER RATIO
        HDR = (H*12)/D

        !  CALCULATE PLOT LEVEL QMD - USED IN REDWOOD CR CALCULTION.
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

        !  CALCULATE RELATIVE DENSITY OF INVENTORY POINT - USED IN RW CR CALCULATION.
        !  IF SDI MAX OF INVENTORY POINT IS EVER LE 0, SET POINT RELATIVE DENSITY
        !  TO 0.
        if (XMAXPT(ITRE(I)).le.0.0) then
           PRD = 0.0
        else
           PRD = ZRD(ITRE(I)) / XMAXPT(ITRE(I))
        end if

        !  2021 ADDITION END

        !  BRANCH TO STATEMENT 58 TO HANDLE TREES WITH DBH LESS THAN 1 IN.

        if (D.lt.1.0.and. LSTART) go to 58

        !  CALCULATE THE PREDICTED CURRENT CROWN RATIO
        !    USING SWITCH CASE FOR RW BRANCH

        select case(ISPC)
        case(17)
           X = -1.021064 + 0.309296*LOG(HDR) +&
                &0.869720*PRD - 0.116274*(D/QMDPLT)
           X = 1.0/(1.0+EXP(X))

           if (DEBUG)write(JOSTND,*)' IN CROWN - RW DEBUG', ' D=',D,&
                &' H=',H,' PRD=',PRD,' QMDPLT=',QMDPLT,' HDR=',HDR,' CR=',X

        case default
           SCALE = (1.0 - .00167 * (RELDEN-100.0))
           if (SCALE .gt.1.0) SCALE = 1.0
           if (SCALE .lt.0.30) SCALE = 0.30
           if (DBH(I) .gt.0.0) then
              X = (real(ISORT(I)) / real(ITRN)) * SCALE
           else
              call RANN(RNUMB)
              X = RNUMB * SCALE
           end if
        end select

        !  CONSTRAIN CR
        if (X .lt..05) X = .05
        if (X .gt..95) X = .95

        !  DETERMINE CRNEW BASED ON SPECIES
        if (ISPC .eq.17) then
           CRNEW(I) = X * 10
        else
           CRNEW(I) = A + B*((-1.0*ALOG(1-X))**(1.0/C))
        end if

        !  WRITE DEBUG INFO IF DESIRED

        if (DEBUG)write(JOSTND,9002) I,X,CRNEW(I),ICR(I)
        9002 format(' IN CROWN 9002 WRITE I,X,CRNEW,ICR = ',I5,2F10.5,I5)
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
        if (DBH(I).ge.DLOW(ISPC) .and. DBH(I).le.DHI(ISPC)) then
           CRNEW(I) = real(ICR(I)) + CHG * CRNMLT(ISPC)
        else
           CRNEW(I) = real(ICR(I)) + CHG
        end if
        9052 ICRI = INT(CRNEW(I) + 0.5)
        if (LSTART .or. ICR(I).eq.0) then
           if (DBH(I).ge.DLOW(ISPC) .and. DBH(I).le.DHI(ISPC)) then
              ICRI = INT(real(ICRI) * CRNMLT(ISPC))
           end if
        end if
        !      IF(DEBUG)WRITE(JOSTND,*)' 9052,ISPC,I,ICFLG,CRNMLT,ICRI= ',ISPC,
        !     &I,ICFLG(ISPC),CRNMLT(ISPC),ICRI

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

        if (ICRI.gt.CRMAX) ICRI = INT(CRMAX + 0.5)
        if (ICRI.lt.10.and. CRNMLT(ISPC).eq.1.0)ICRI = INT(CRMAX + 0.5)

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

        !  CROWNS FOR TREES WITH DBH LT 1 INCH ARE DUBBED HERE.  NO CHANGE
        !  IS CALCULATED UNTIL THE TREE ATTAINS A DBH OF 1 INCH.

        58 continue
        if (ICR(I) .ne.0) go to 60
        TPCT = PCT(I)
        TPCCF = PCCF(IITRE)
        call DUBSCR(ISPC,D,H,PRD,QMDPLT,CR,TPCT,TPCCF)
        if (DEBUG) write(JOSTND,*) 'RETURN FORM DUBSCR IN CROWN'
        ICRI = INT(CR*100.0 + 0.5)
        if (DBH(I).ge.DLOW(ISPC) .and. DBH(I).le.DHI(ISPC))&
             &ICRI = INT(real(ICRI) * CRNMLT(ISPC))

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
        !      IF(DEBUG)WRITE(JOSTND,*)' 60-70-ISPC,LSTART,ICFLG(ISPC)= ',
        !     &ISPC,LSTART,ICFLG(ISPC)
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

     !  CALCULATE PLOT LEVEL QMD - USED IN RW CR CALCULATION
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

     !  CALCULATE RELATIVE DENSITY OF INVENTORY POINT - USED IN RW CR CALCULATION.
     !  IF SDI MAX OF INVENTORY POINT IS EVER LE 0, SET POINT RELATIVE DENSITY
     !  TO 0.
     if (XMAXPT(IITRE).le.0.0) then
        PRD = 0.0
     else
        PRD = ZRD(IITRE) / XMAXPT(IITRE)
     end if

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
  9010 format(' LEAVING CROWN 9010 FORMAT ITRN,ICR= ',I10,/,&
       &43(1H ,32I4,/))
  if (DEBUG)write(JOSTND,90)ICYC
  90 format(' LEAVING SUBROUTINE CROWN  CYCLE =',I5)
  return
  entry CRCONS

  !  ENTRY POINT FOR LOADING CROWN RATIO MODEL COEFFICIENTS

  !  COEFF ORDER 1=SF,  2=WF,  3=RF,  4=NF,  5=WP,  6=PP,  7=DF,
  !              8=RC,  9=WH, 10=MH, 11=RW, 12=BM, 13=RA, 14=OH,
  !             15=IC, 16=LP, 17=SS

  ! LOAD WEIBULL 'A' PARAMETER BY SPECIES

  data WEIBA/ 2*0.0, 1.0, 8*0.0, 1.0, 2*0.0, 1.0, 2*0.0/

  ! LOAD WEIBULL 'B' PARAMETER EQUATION CONSTANT COEFFICIENT

  data WEIBB0/ -0.171680,  0.130939, -0.981113, -0.135807, 0.019948,&
       &-0.036696, -0.012061, -0.062693,  0.073435,  0.162672, 0.196054,&
       &-0.818809,  0.035786, -0.238295, -0.811424, -0.131210,-0.107413/

  ! LOAD WEIBULL 'B' PARAMETER EQUATION SLOPE COEFFICIENT

  data WEIBB1/ 1.161549, 1.093406, 1.092273, 1.147712, 1.108738,&
       &1.132792, 1.119712, 1.139657, 1.107183, 1.073404, 1.073909,&
       &1.054176, 1.121389, 1.180163, 1.056190, 1.159760, 1.140775/

  ! LOAD WEIBULL 'C' PARAMETER EQUATION CONSTANT COEFFICIENT

  data WEIBC0/ 2.8263  , 1.355139,  1.326047, 3.017494, 2.621230,&
       &2.876094, 3.2126  , 1.7664  ,  2.6237  , 3.288501, 0.345647,&
       &-2.366108, 2.0408  , 3.044134, -3.831124, 2.598238, 3.0712  /

  ! LOAD WEIBULL 'C' PARAMETER EQUATION SLOPE COEFFICIENT

  data WEIBC1 / 0.0     , 0.350472, 0.318386, 0.0     , 0.186734,&
       &0.0     , 0.0     , 0.0     , 0.0     , 0.0     , 0.620145,&
       &1.202413, 0.0     , 0.0     , 1.401938, 0.0     , 0.0     /

  ! LOAD CR=F(SDI) EQUATION CONSTANT COEFFICIENT

  data C0/    5.073342, 5.212394, 4.860467, 5.568864, 4.279655,&
       &5.073273, 5.666442, 4.481330, 5.671345, 6.484942, 5.417431,&
       &4.420000, 4.656659, 4.625125, 5.200550, 4.890318, 5.812879/

  ! LOAD CR=F(SDI) EQUATION SLOPE COEFFICIENT

  data C1/   -0.011430, -0.011623, -0.006173, -0.021293, -0.002484,&
       &-0.020988, -0.025199, -0.018092, -0.023463, -0.023248, -0.011608,&
       &-0.010660, -0.022612, -0.016042, -0.014890, -0.018837, -0.028504/

  data CRNMLT/MAXSP*1.0/
  data ICFLG/MAXSP*0/
  data DLOW/MAXSP*0.0/
  data DHI/MAXSP*99.0/
  return
end
