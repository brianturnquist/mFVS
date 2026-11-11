!============================================!
!   Compute crown ratios and crown changes   !
!============================================!
! SO $Id$
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
  include 'PLOT.f90'
  include 'ARRAYS.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'
  include 'OUTCOM.f90'
  include 'PDEN.f90'
  include 'VARCOM.f90'

  ! Variables
  logical :: DEBUG
  real    :: CRNEW(MAXTRE),WEIBA(MAXSP),WEIBB0(MAXSP)
  real    :: WEIBB1(MAXSP),WEIBC0(MAXSP),WEIBC1(MAXSP),C0(MAXSP),C1(MAXSP)
  integer :: ISORT(MAXTRE)
  real    :: CRNMLT(MAXSP),DLOW(MAXSP),DHI(MAXSP)
  integer :: MYACTS(1)
  real    :: PRM(5)
  integer :: ICFLG(MAXSP)
  integer :: JJ,NTODO,I,NP,IACTK,IDATE,IDT,ISPCC,IGRP,J1,ISPC,I1,I2
  integer :: I3,IITRE,IULIM,IG,IGSP,ICRI
  real    :: RELSDI,ACRNEW,A,B,C,D,H,SCALE,X,RNUMB
  real    :: PDIFPY,CHG,CRLN,CRMAX,HN,HD,CL,TPCT,TPCCF,CR
  data MYACTS/81/

  !  SPECIES ORDER:
  !  1=WP,  2=SP,  3=DF,  4=WF,  5=MH,  6=IC,  7=LP,  8=ES,  9=SH,  10=PP,
  ! 11=JU, 12=GF, 13=AF, 14=SF, 15=NF, 16=WB, 17=WL, 18=RC, 19=WH,  20=PY,
  ! 21=WA, 22=RA, 23=BM, 24=AS, 25=CW, 26=CH, 27=WO, 28=WI, 29=GC,  30=MC,
  ! 31=MB, 32=OS, 33=OH

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'CROWN',5,ICYC)

  if (DEBUG) write(JOSTND,3)ICYC
  3 format(' ENTERING SUBROUTINE CROWN  CYCLE =',I5)

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

  do ISPC = 1,MAXSP
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

        !  BRANCH TO STATEMENT 58 TO HANDLE TREES WITH DBH LESS THAN 1 IN.

        if (D.lt.1.0.and. LSTART) go to 58

        !  CALCULATE THE PREDICTED CURRENT CROWN RATIO

        SCALE = (1.0 - .00167 * (RELDEN-100.0))
        if (SCALE .gt.1.0) SCALE = 1.0
        if (SCALE .lt.0.30) SCALE = 0.30
        if (DBH(I) .gt.0.0) then
           X = (real(ISORT(I)) / real(ITRN)) * SCALE
        else
           call RANN(RNUMB)
           X = RNUMB * SCALE
        end if
        if (X .lt..05) X = .05
        if (X .gt..95) X = .95
        CRNEW(I) = A + B*((-1.0*ALOG(1-X))**(1.0/C))

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
        9052 ICRI = INT(CRNEW(I)+0.5)
        if (LSTART .or. ICR(I).eq.0) then
           if (DBH(I).ge.DLOW(ISPC) .and. DBH(I).le.DHI(ISPC)) then
              ICRI = INT(real(ICRI) * CRNMLT(ISPC))
           end if
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
        if (ICR(I) .ne.0) go to 60
        TPCT = PCT(I)
        TPCCF = PCCF(IITRE)
        call DUBSCR(ISPC,D,H,CR,TPCT,TPCCF)
        if (DEBUG) write(JOSTND,*) 'RETURN FORM DUBSCR IN CROWN'
        ICRI = INT(CR*100.0+0.5)
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
     call DUBSCR (ISPC,D,H,CR,TPCT,TPCCF)
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

  !  SPECIES ORDER:
  !  1=WP,  2=SP,  3=DF,  4=WF,  5=MH,  6=IC,  7=LP,  8=ES,  9=SH,  10=PP,
  ! 11=JU, 12=GF, 13=AF, 14=SF, 15=NF, 16=WB, 17=WL, 18=RC, 19=WH,  20=PY,
  ! 21=WA, 22=RA, 23=BM, 24=AS, 25=CW, 26=CH, 27=WO, 28=WI, 29=GC,  30=MC,
  ! 31=MB, 32=OS, 33=OH

  ! LOAD WEIBULL 'A' PARAMETER BY SPECIES

  data WEIBA/ 2.0, 2.0, 1.0, 0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 0.0,&
       &0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0,&
       &0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,&
       &0.0, 1.0, 0.0/

  ! LOAD WEIBULL 'B' PARAMETER EQUATION CONSTANT COEFFICIENT

  data WEIBB0/&
       &-2.12713, -2.27296, -1.19297,  0.06593, -0.94138,&
       &-1.38636,  0.07609, -0.91567,  0.16601,  0.24916,&
       &0.07609,  0.06593, -0.91567, -0.09734, -0.13581,&
       &-0.82631,  0.00603, -0.01129,  0.49085,  0.19605,&
       &-0.23830, -1.11274, -0.23830, -0.08414, -0.23830,&
       &-0.23830,  0.06607, -0.23830, -0.23830, -0.23830,&
       &-0.23830, -1.19297, -0.23830/

  ! LOAD WEIBULL 'B' PARAMETER EQUATION SLOPE COEFFICIENT

  data WEIBB1/&
       &1.10526,  1.12434,  1.12928,  1.09624,  1.08256,&
       &1.16801,  1.10184,  1.06469,  1.08150,  1.04831,&
       &1.10184,  1.09624,  1.06469,  1.14675,  1.14771,&
       &1.06217,  1.12276,  1.11665,  1.01414,  1.07391,&
       &1.18016,  1.12314,  1.18016,  1.14765,  1.18016,&
       &1.18016,  1.10705,  1.18016,  1.18016,  1.18016,&
       &1.18016,  1.12928,  1.18016/

  ! LOAD WEIBULL 'C' PARAMETER EQUATION CONSTANT COEFFICIENT

  data WEIBC0/&
       &2.77,    3.34,  3.42,   3.71, 3.47,&
       &3.02,    3.01,  3.50, 0.9142, 4.36,&
       &3.01,    3.71,  3.50,  2.716, 3.02,&
       &3.31429,   2.734, 3.355,   3.16, 0.35,&
       &3.04,    2.53,  3.04,  2.775, 3.04,&
       &3.04, 2.04714,  3.04,   3.04, 3.04,&
       &3.04,    3.42,  3.04/

  ! LOAD WEIBULL 'C' PARAMETER EQUATION SLOPE COEFFICIENT

  data WEIBC1 /&
       &0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
       &0.00000, 0.00000, 0.00000, 0.45768, 0.00000,&
       &0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
       &0.00000, 0.00000, 0.00000, 0.00000, 0.62015,&
       &0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
       &0.00000, 0.15070, 0.00000, 0.00000, 0.00000,&
       &0.00000, 0.00000, 0.00000/

  ! LOAD CR=F(SDI) EQUATION CONSTANT COEFFICIENT

  data C0/&
       &7.16846, 6.59700, 5.52653, 6.61291, 7.45097,&
       &6.17373, 5.50719, 6.77400, 6.14578, 6.41166,&
       &7.23800, 6.61291, 6.12779, 4.79981, 5.56886,&
       &6.19911, 4.98675, 5.74915, 5.48853, 5.41743,&
       &4.62512, 4.12048, 4.62512, 4.01678, 4.62512,&
       &4.62512, 6.82187, 4.62512, 4.62512, 4.62512,&
       &4.62512, 5.52653, 4.62512/

  ! LOAD CR=F(SDI) EQUATION SLOPE COEFFICIENT

  data C1/&
       &-0.02375, -0.01954,  0.00000, -0.02182, -0.02406,&
       &-0.01795, -0.01833,  0.00000, -0.02781, -0.02041,&
       &0.00000, -0.02182, -0.01269, -0.00653, -0.02129,&
       &-.02216, -0.02466, -0.01090, -0.00717, -0.01161,&
       &-0.01604, -0.00636, -0.01604, -0.01516, -0.01604,&
       &-0.01604, -0.02247, -0.01604, -0.01604, -0.01604,&
       &-0.01604,  0.00000, -0.01604/

  data CRNMLT/MAXSP*1.0/
  data ICFLG/MAXSP*0/
  data DLOW/MAXSP*0.0/
  data DHI/MAXSP*99.0/
  return
end
