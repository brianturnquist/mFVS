!============================================!
!   Compute crown ratios and crown changes   !
!============================================!
! IE $Id$
subroutine CROWN
  implicit none

  !  THIS SUBROUTINE IS USED TO DUB MISSING CROWN RATIOS AND COMPUTE CROWN
  !  RATIO CHANGES FOR TREES THAT ARE GREATER THAN 3 INCHES DBH.  CROWN
  !  RATIO IS PREDICTED FROM HABITAT TYPE, BASAL AREA, CROWN COMPETITION
  !  FACTOR, DBH, TREE HEIGHT, AND PERCENTILE IN THE BASAL AREA
  !  DISTRIBUTION.  WHEN THE EQUATION IS USED TO PREDICT CROWN RATIO
  !  CHANGE, VALUES OF THE PREDICTOR VARIABLES FROM THE START OF THE CYCLE
  !  ARE USED TO PREDICT OLD CROWN RATIO, VALUES FROM THE END OF THE CYCLE
  !  ARE USED TO PREDICT NEW CROWN RATIO, AND THE CHANGE IS COMPUTED BY
  !  SUBTRACTION.  THE CHANGE IS APPLIED TO ACTUAL CROWN RATIO.  THIS
  !   ROUTINE IS CALLED FROM **CRATET** TO DUB MISSING VALUES, AND BY
  !  **TREGRO** TO COMPUTE CHANGE DURING REGULAR CYCLING.  ENTRY
  !  **CRCONS** IS CALLED BY **RCON** TO LOAD MODEL CONSTANTS THAT ARE
  !  SITE DEPENDENT AND NEED ONLY BE RESOLVED ONCE.  A CALL TO **DUBSCR**
  !  IS ISSUED TO DUB CROWN RATIO WHEN DBH IS LESS THAN 3 INCHES.
  !  PROCESSING OF CROWN CHANGE FOR SMALL TREES IS CONTROLLED BY
  !  **REGENT**.

  !  SPECIES EXPANSION
  !  WB USE COEFFICIENTS FOR L
  !  LM AND PY USE COEFFICIENTS FROM TT FOR LM
  !  LL USE COEFFICIENTS FOR AF
  !  AS, MM, PB USE COEFFIECIENTS FOR AS FROM UT
  !  CO, OH USE COEFFIECIENTS FOR OH FROM CR
  !  OS USE COEFFEICIENTS FOR MH
  !  PI, JU USE COEFFICIENTS FROM UT (WHICH ARE FROM CR SO,
  !         USE CR LOGIC)

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CALCOM.f90'
  include 'ARRAYS.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'
  include 'OUTCOM.f90'
  include 'PLOT.f90'
  include 'PDEN.f90'
  include 'VARCOM.f90'
  include 'GGCOM.f90'

  !  DECLARATIONS AND DIMENSIONS FOR INTERNAL VARIABLES:

  !  DCRCON -- CONSTANT TERM FOR THE CROWN RATIO MODEL BASED ON
  !            PAST TREE STAND ATTRIBUTES
  !  XCRCON -- CONSTANT TERM FOR THE CROWN RATIO MODEL BASED ON
  !            CURRENT TREE STAND ATTRIBUTES
  !    PARM -- ARRAY OF COEFFICIENTS FOR TERMS INVOLVING TREE
  !            AND STAND ATTRIBUTES

  external RANN
  ! Variables
  logical :: DEBUG,NIVAR,CRVAR,UTTVAR,LPIJU
  real    :: PARM(MAXSP,14)
  integer :: MAPHAB(30,MAXSP),IERR
  real    :: CRHAB(14,MAXSP),RMAIAS,RMAILM,TEMMAI,ADJMAI
  real    :: CRNEW(MAXTRE),WEIBA(MAXSP),WEIBB0(MAXSP),WEIBB1(MAXSP),WEIBC0(MAXSP),WEIBC1(MAXSP),&
       &C0(MAXSP),C1(MAXSP),CRNMLT(MAXSP),DLOW(MAXSP),DHI(MAXSP),PRM(5)
  integer :: ISORT(MAXTRE),MYACTS(1),ICFLG(MAXSP)
  real    :: OBA,XCRCON,DCRCON,B7,B8,B9,B10,B11,B12,B13,B14,RELSDI
  real    :: ACRNEW,A,B,C,D,H,BARK,HF,CL,CR,SCALE,X,RNUMB
  integer :: ISPC,I1,I2,I3,IITRE,J,I,JJ,NTODO,NP,IACTK
  integer :: IDATE,IDT,ISPCC,IGRP,IULIM,IG,IGSP,J1,ICRI
  real    :: CRSD,X1,X2,RDM1,BRATIO,P,PCR,EXPPCR,EXPDCR
  real    :: DCR,CHG,PDIFPY,XCR,BACHLO,CRLN,CRMAX,HN,HD,TPCCF

  data MYACTS/81/

  !  DATA STATEMENTS FROM NI VARIANT

  data PARM/&
       &0.0,-0.00204,0.0,-0.00183,3*0.0,-0.00203,-0.00190,-0.002165,&
       &-0.00264,-.00204,0.0,-0.00190,8*0.0,-.00264,&
       &4*0.0,-0.000001902,6*0.0,12*0.0,&
       &-0.34566,4*0.0,0.17479,5*0.0,12*0.0,&
       &5*0.0,-0.00183,5*0.0,12*0.0,&
       &10*0.0,0.000005116,11*0.0,0.000005116,&
       &2*0.0,-0.15334,3*0.0,-0.18555,4*0.0,12*0.0,&
       &0.03882,3*0.0,0.03027,-0.0056,5*0.0,12*0.0,&
       &-0.0007,3*0.0,-0.00055,6*0.0,12*0.0,&
       &.0,0.30066,0.33840,0.24293,2*0.0,0.53172,0.29699,0.23372,&
       &0.26558,0.0,0.30066,0.0,0.23372,9*0.0,&
       &6*0.0,-0.02989,4*0.0,12*0.0,&
       &6*0.0,0.00011,4*0.0,12*0.0,&
       &-0.21217,-0.59302,-0.59685,-0.25601,-0.25776,2*0.0,-0.38334,&
       &-0.28433,-0.31555,-0.25138,-0.59302,0.0,-0.28433,8*0.0,-0.25138,&
       &0.00301,5*0.0,0.0042,0.0,0.001903,4*0.0,0.001903,9*0.0,&
       &0.0,0.19558,0.16488,0.07260,0.06887,0.1105,0.0,0.09918,0.0,&
       &0.16072,0.05140,0.19558,10*0.0,0.05140/
  data ((MAPHAB(I,J),I = 1,30), J = 1,5)/&
       &12*2,3,3*4,3*5,6,6,1,6,1,7,1,4*6,&
       &7*2,3,2,4,4,5,6,3*7,8,8,4,1,10,9,10,1,11,4*1,2,&
       &3*2,3*4,6,7,4,8,8,5,9,3*10,2*11,8,1,2*12,13,1,3,1,3,3*1,&
       &9*2,1,1,2,3,3*4,5,5,6,1,3*7,1,8,1,7,3*1,&
       &30*1/
  data ((MAPHAB(I,J),I = 1,30), J = 6,MAXSP)/&
       &16*1,3*2,11*1,&
       &6*2,4,5,5,2,2,6,7,3*8,9,9,10,2*11,12,11,1,13,1,14,3,3,11,&
       &7*2,3,2,1,1,2,4,3*5,6,6,7,8,8,9,8,10,11,1,1,2*12,8,&
       &13*2,3,4*4,5,6,6,7,6,1,8,1,9,2*10,6,&
       &2,2,4,3*1,5,6,3*1,8,7,3*9,3*3,11*1,&
       &12*1,4*2,3,3,4,3*1,5,1,6,5*1,&
       &7*2,3,2,4,4,5,6,3*7,8,8,4,1,10,9,10,1,11,4*1,2,&
       &30*1,&
       &13*2,3,4*4,5,6,6,7,6,1,8,1,9,2*10,6,&
       &240*1,&
       &12*1,4*2,3,3,4,3*1,5,1,6,5*1/
  data CRHAB/&
       &0.8884, 0.7309, 0.9347, 0.9888, 0.9945, 1.1126, 1.0263, 7*0.0,&
       &0.06533, 0.03441, 0.2307, 0.1661, -0.1253, -0.05018, 0.11005,&
       &0.08113, 0.1782, 0.03919, 0.2107, 3*0.0,&
       &0.8643, 0.7271, 0.9840, 0.8127, 0.8874, 0.7055, 0.7708, 0.7849,&
       &0.8038, 0.8742, 0.8232, 0.8415, 0.9759, 0.0,&
       &-0.2304,-0.5421,-0.4343,-0.3759,-0.4129,-0.4879,-0.2674,&
       &-0.1941,6*0.0,&
       &-0.2413,13*0.0,&
       &-1.6053, -1.7128, 12*0.0,&
       &-0.3785, -0.4142, -0.3985, -0.2987, -0.3810, -0.4087, -0.3577,&
       &-0.2994, -0.2486,-0.2863,-0.1968,-0.4931,-0.2676,-0.5625,&
       &0.05351, -0.05031, 0.1075, -0.1872, 0.01729, 0.03667, 0.01885,&
       &0.09102, 0.1371, 0.08368, 0.1230, -0.02365, 2*0.0,&
       &0.09453, -0.07740, 0.07113, 0.2039, 0.06176, 0.1513, 0.09086,&
       &0.1580, 0.09229, 0.01551, 4*0.0,&
       &-0.9436, -0.8654, -0.8849, -0.9067, -0.8783, -1.0103, -1.0268,&
       &-1.0050, -1.0301, 5*0.0,&
       &0.4649, 0.3211, 0.1970, 0.2295, 0.3383, 0.3450, 8*0.0,&
       &0.06533, 0.03441, 0.2307, 0.1661, -0.1253, -0.05018, 0.11005,& !START SPEC EXPAND
       &0.08113, 0.1782, 0.03919, 0.2107, 3*0.0,&
       &14*0.0,&
       &0.09453, -0.07740, 0.07113, 0.2039, 0.06176, 0.1513, 0.09086,&
       &0.1580, 0.09229, 0.01551, 4*0.0,&
       &112*0.0,&
       &0.4649, 0.3211, 0.1970, 0.2295, 0.3383, 0.3450, 8*0.0/

  !     DATA CRSD/12.69/
  !  THIS IS A POPULATION ESTIMATE AND TOO LARGE FOR WITHIN STAND VARIATION.
  !  CUT IT IN HALF UNTIL SOMETHING BETTER COMES ALONG.   DIXON  4/19/99

  data CRSD/6.35/

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'CROWN',5,ICYC)

  !  CALCULATE THE MAI VALUES FOR THE AS/MM/PB AND LM/PY SPECIES GROUPS.
  !  THIS WILL BE USED LATER IN CROWN DUBBING FOR THESE SPECIES INSTEAD
  !  OF THE RMAI VALUE COMPUTED IN CRATET. THESE ARE THE ONLY SPECIES
  !  WHICH HAVE AN MAI TERM IN THE ESTIMATION OF CROWN RATIO.

  RMAIAS = ADJMAI(746,SITEAR(18),10.0,IERR)
  if (RMAIAS .gt.128.0)RMAIAS = 128.0
  RMAILM = ADJMAI(101,SITEAR(13),10.0,IERR)
  if (RMAILM .gt.128.0)RMAI = 128.0
  if (DEBUG)write(JOSTND,*)' SITEAS,RMAIAS,SITELM,RMAILM= ',&
       &SITEAR(18),RMAIAS,SITEAR(13),RMAILM

  !  WRITE DEBUG INFO (STAND DENSITY TERMS) IF DESIRED

  if (DEBUG) write(JOSTND,9000) BA, RELDEN, RELDM1
  9000 format(' IN CROWN, BA=',F7.3,'  RELDEN=',F7.3,'  RELDM1=',F7.3)

  !  DUB CROWNS ON DEAD TREES IF NO LIVE TREES IN INVENTORY

  if ((ITRN.le.0).and.(IREC2.lt.MAXTP1))go to 74

  !  IF THERE ARE NO TREE RECORDS, THEN RETURN.

  if (ITRN.eq.0) then
     return
  else if (TPROB.le.0.0) then
     do I = 1,ITRN
        ICR(I) = ABS(ICR(I))
     end do
     return
  end if

  !  ADDED FOR SPECIES EXPANSION
  !  INITIALIZE CROWN VARIABLES TO BEGINNING OF CYCLE

  if (LSTART) then
     do JJ = 1,MAXTRE
        CRNEW(JJ) = 0.0
        ISORT(JJ) = 0
        10 continue
     end do
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

  !  IF CCF LAST CYCLE WAS LESS THAN 100.0, THERE IS ASSUMED TO BE
  !  NO DENSITY IMPACT ON CROWN RATIOS.

  X1 = 0.
  X2 = 0.
  RDM1 = RELDM1
  OBA = OLDBA
  if (RELDM1.ge.100.0) go to 20
  OBA = BA
  RDM1 = RELDEN
  20 continue
  if (LSTART) goto 26
  if (OBA.gt.0.)X1 = ALOG(OBA)
  if (RDM1.gt.0.)X2 = ALOG(RDM1)
  26 continue

  !  ENTER THE LOOP FOR SPECIES DEPENDENT VARIABLES

  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (I1.eq.0)goto 70
     I2 = ISCT(ISPC,2)

     !  SET UP LOGICAL VARIABLES DEPENDING UPON VARIANT SPECIES CAME FROM

     NIVAR = .false.
     CRVAR = .false.
     UTTVAR = .false.
     LPIJU = .false.
     if (ISPC.le.12.or. ISPC.eq.14.or. ISPC.eq.23) then
        NIVAR = .true.
     else if (ISPC.eq.19.or. ISPC.eq.22) then
        CRVAR = .true.
     else if (ISPC.eq.15.or. ISPC.eq.16) then
        LPIJU = .true.
     else
        UTTVAR = .true.
     end if
     if (NIVAR) then
        XCRCON = CRCON(ISPC) + PARM(ISPC,1)*BA + PARM(ISPC,2)*BA*BA +&
             &PARM(ISPC,3)*ALOG(BA) + PARM(ISPC,4)*RELDEN + PARM(ISPC,5)&
             &*RELDEN*RELDEN + PARM(ISPC,6)*ALOG(RELDEN)
        DCRCON = 0.0
     end if

     !  IF DUBBING MISSING CROWN RATIOS, BYPASS ASSIGNMENT OF DCRCON.

     if (LSTART) go to 30
     if (NIVAR) then
        DCRCON = CRCON(ISPC) + PARM(ISPC,1)*OBA + PARM(ISPC,2)*OBA*OBA +&
             &PARM(ISPC,3)*X1 + PARM(ISPC,4)*RDM1 + PARM(ISPC,5)*RDM1&
             &*RDM1 + PARM(ISPC,6)*X2
     end if

     !  WRITE DEBUG INFO (CONSTANT TERMS) IF DESIRED

     30 continue
     if (DEBUG) write(JOSTND,9001) ISPC, XCRCON, DCRCON
     9001 format(' SPECIES: ',I2,' - XCRCON = ',F10.3,', DCRCON = ',F10.3)

     !  VARIABLES B7, B8, ..., B14 ARE SET UP TO ELIMINATE THE
     !  2-DIMENSIONAL ARRAY ACCESSES WHEN PROCESSING RECORDS OF THE SAME
     !  SPECIES.

     B7 = PARM(ISPC,7)
     B8 = PARM(ISPC,8)
     B9 = PARM(ISPC,9)
     B10 = PARM(ISPC,10)
     B11 = PARM(ISPC,11)
     B12 = PARM(ISPC,12)
     B13 = PARM(ISPC,13)
     B14 = PARM(ISPC,14)

     !  FOLLOWING ADDED FOR SPECIES EXPANSION FROM TT,UT
     !  ESTIMATE MEAN CROWN RATIO FROM SDI, AND ESTIMATE WEIBULL PARAMETERS

     if (SDIDEF(ISPC) .gt.0.) then
        RELSDI = SDIAC / SDIDEF(ISPC)
     else
        RELSDI = 1.0
     end if
     if (RELSDI .gt.1.5) RELSDI = 1.5
     ACRNEW = C0(ISPC) + C1(ISPC) * RELSDI*100.0
     A = WEIBA(ISPC)
     B = WEIBB0(ISPC) + WEIBB1(ISPC) * ACRNEW
     C = WEIBC0(ISPC) + WEIBC1(ISPC)*ACRNEW
     if (B .lt.1.0) B = 1.0
     if (C .lt.2.0) C = 2.0
     if (DEBUG) write(JOSTND,9801) ISPC,SDIAC,ORMSQD,RELSDI,&
          &ACRNEW,A,B,C,SDIDEF(ISPC)
     9801 FORMAT(/' IN CROWN 9001 WRITE ISPC,SDIAC,ORMSQD,RELSDI,ACRNEW,A,B &
          &C,SDIDEF = ',/,1H ,I5,F8.2,F8.4,F8.2,F8.2,4F10.4)

     !  ENTER LOOP FOR TREE DEPENDENT VARIABLES

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
        if (ICR(I).gt.0) go to 40
        ICR(I) = -ICR(I)
        if (DEBUG) write (JOSTND,35) I,ICR(I)
        35 format (' ICR(',I4,') WAS CALCULATED ELSEWHERE AND IS ',I4)
        goto 60
        40 continue
        D = DBH(I)
        H = HT(I)
        BARK = BRATIO(ISPC,D,H)

        !  FOLLOWING ADDED FOR SPECIES EXPANSION FROM CR

        if (CRVAR .or. LPIJU) then
           HF = H+HTG(I)
           if (CRVAR) then
              CL = 5.17281 + 0.32552 * HF - 0.01675 * BA
           else if (LPIJU) then
              CL = -0.59373 + 0.67703 * HF
           end if
           if (CL .lt.1.0) CL = 1.0
           if (CL .gt. HF) CL = HF
           CR = CL / HF
           CRNEW(I) = CR*100.
           goto 53
        end if

        !  BRANCH TO STATEMENT 58 TO HANDLE TREES WITH DBH LT SOME THRESHOLD
        !  FOR TT AND UT SPECIES EXPANSION TREES, BRANCH TO 58 IF D < 1 IN.
        !  "ELSE" PORTION APPLIES IF NIVAR IS .TRUE., BRANCH IF D < 3 IN.
        !  CASE WHERE CRVAR OR LPIJU IS TRUE BRANCH TO STMT 53 ABOVE.

        if (UTTVAR) then
           if (D.lt.1.0.and.LSTART) go to 58
        else
           if (D.lt.3.0) go to 58
        end if

        !  THE FOLLOWING LINES UNTIL STATEMENT 51 HAS BEEN ADDED
        !  FOR UT AND TT SPECIES (EXCEPT PINYON & JUNIPER)

        if (UTTVAR) then

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
           9002 format(/' IN CROWN 9002 WRITE I,X,CRNEW,ICR = ',&
                &I5,2F10.5,I5)
           CRNEW(I) = CRNEW(I)*10.0

           !  END OF LOOP FOR UT, TT SPECIES

        end if

        !  THE FOLLOWING LOGIC IS USED ONLY BY ORIGINAL NI SPECIES
        !  ALL OTHER SPECIES SKIP TO LINE

        if (NIVAR) then

           !  BYPASS CROWN CHANGE CALCULATION IF CROWN RATIO WAS ASSIGNED
           !  THIS CYCLE IN REGENT.

           if (.not.LSTART .and. D-DG(I)/BARK .lt.3.0) go to 60
           P = PCT(I)
           if (P.lt.0.01)P = 0.01

           !  CALCULATE THE PREDICTED CURRENT CROWN RATIO

           PCR = XCRCON + B7*D + B8*D*D + B9*ALOG(D) + B10*H + B11*H*H +&
                &B12*ALOG(H) + B13*P + B14*ALOG(P)
           EXPPCR = EXP(PCR)

           !  SET DIFFERENTIAL TO 0.0 AND BRANCH TO 50 IF DUBBING

           EXPDCR = 0.0
           if (LSTART) go to 50

           !  BACKDATE TREE ATTRIBUTES SO THAT CROWN RATIO AT THE BEGINNING
           !  OF THE CYCLE CAN BE PREDICTED.

           D = D-(DG(I)/BARK)
           if (D.le.0.0) D = DBH(I)
           H = H-HTG(I)
           if (H.le.0.0) H = HT(I)
           if ((OLDPCT(I).gt.PCT(I).and.ONTREM(7).gt.0.0).or.&
                &OLDPCT(I).le.0.0) OLDPCT(I) = PCT(I)
           P = OLDPCT(I)
           if (P.lt.0.01)P = 0.01

           !  CALCULATE THE PREDICTED CROWN RATIO AT THE START OF THE CYCLE.

           DCR = DCRCON + B7*D + B8*D*D + B9*ALOG(D) + B10*H + B11*H*H +&
                &B12*ALOG(H) + B13*P + B14*ALOG(P)
           EXPDCR = EXP(DCR)

           !  WRITE DEBUG INFO IF DESIRED

           50 continue
           if (DEBUG)write(JOSTND,*)' ISPC,NIVAR= ',ISPC,NIVAR
           if (DEBUG) write(JOSTND,9802) I,ICR(I),EXPPCR,EXPDCR,D,H,PCT(I),&
                &OLDPCT(I)
           9802 format(' ICR(',I4,')=',I3,'  EXPPCR=',F10.3,' EXPDCR=',F10.3,&
                &' D=',F7.3,' H=',F7.3,' P=',F7.3,' OLDP=',F7.3)

           !  COMPUTE THE PREDICTED CROWN RATIO AND
           !  BOUND CROWN CHANGE TO 1% PER YEAR

           CHG = EXPPCR-EXPDCR
           if (.not.LSTART.or.(ICR(I).gt.0)) then
              PDIFPY = CHG/real(ICR(I))/FINT*100.
              if (PDIFPY.gt.0.01)CHG = real(ICR(I))*(0.01)*FINT/100.
              if (PDIFPY.lt.-0.01)CHG = real(ICR(I))*(-0.01)*FINT/100.
           end if

           !  APPLY CRNMULT KEYWORD ADJUSTMENTS

           if (DBH(I).ge.DLOW(ISPC) .and. DBH(I).lt.DHI(ISPC)) then
              ICRI = INT(real(ICR(I))+CRNMLT(ISPC)*CHG*100.0+0.50005)
           else
              ICRI = INT(real(ICR(I))+CHG*100.0+0.50005)
           end if
           if ((DGSD.lt.1.0).or.(.not.LSTART)) go to 51
           XCR = ICRI
           ICRI = INT(BACHLO(XCR,CRSD,RANN))
           51 continue

           !  END OF NI BLOCK

           go to 55
        end if

        !  COMPUTE THE CHANGE IN CROWN RATIO
        !  CALC THE DIFFERENCE BETWEEN THE MODEL AND THE OLD(OBS)
        !  THIS SECTION ADDED FOR SPECIES EXPANSION
        !  BOUND CROWN CHANGE TO 1% PER YEAR

        53 continue
        CHG = CRNEW(I)-real(ICR(I))
        if (.not.LSTART.or.(ICR(I).gt.0)) then
           PDIFPY = CHG/real(ICR(I))/FINT
           if (PDIFPY.gt.0.01)CHG = real(ICR(I))*(0.01)*FINT
           if (PDIFPY.lt.-0.01)CHG = real(ICR(I))*(-0.01)*FINT
           if (DEBUG)write(JOSTND,9020)I,CRNEW(I),ICR(I),PDIFPY,CHG
           9020 format(/'  IN CROWN 9020 I,CRNEW,ICR,PDIFPY,CHG =',&
                &I5,F10.3,I5,3F10.3)
           if (DBH(I).ge.DLOW(ISPC).and.DBH(I).lt.DHI(ISPC)) then
              CRNEW(I) = real(ICR(I)) + CHG * CRNMLT(ISPC)
           else
              CRNEW(I) = real(ICR(I)) + CHG
           end if
           ICRI = INT(CRNEW(I)+0.5)
        else
           !  DUB CROWN
           ICRI = INT(CRNEW(I)+0.5)
           if (DBH(I).ge.DLOW(ISPC).and.DBH(I).lt.DHI(ISPC))&
                &ICRI = INT(real(ICRI)*CRNMLT(ISPC))
        end if

        ! CALC CROWN LENGTH NOW FOR CR, UT, TT VARIANTS

        if (LSTART .or. ICR(I).eq.0)go to 55
        CRLN = HT(I)*real(ICR(I))/100.

        ! CALC CROWN LENGTH MAX POSSIBLE IF ALL HTG GOES TO NEW CROWN

        CRMAX = (CRLN+HTG(I))/(HT(I)+HTG(I))*100.0
        if (DEBUG)write(JOSTND,9004)CRMAX,CRLN,ICRI,I,CRNEW(I),CHG
        9004 format(/' CRMAX=',F10.2,' CRLN=',F10.2,&
             &' ICRI=',I10,' I=',I5,' CRNEW=',F10.2,' CHG=',F10.3)

        ! IF NEW CROWN EXCEEDS MAX POSSIBLE LIMIT IT TO MAX POSSIBLE

        if (ICRI.lt.10.and.CRNMLT(ISPC).eq.1.0)ICRI = INT(CRMAX+0.5)
        if (ICRI.gt.CRMAX) ICRI = INT(CRMAX+0.5)

        !  REDUCE CROWNS OF TREES  FLAGGED AS TOP-KILLED ON INVENTORY

        55 continue
        if (.not.LSTART .or. ITRUNC(I).eq.0) go to 59
        HN = real(NORMHT(I))/100.0
        HD = HN-real(ITRUNC(I))/100.0
        CL = (real(ICRI)/100.)*HN-HD
        ICRI = INT((CL*100./HN)+.5)
        if (DEBUG)write(JOSTND,9030)I,ITRUNC(I),NORMHT(I),HN,HD,&
             &ICRI,CL
        9030 format(/'  IN CROWN 9030 I,ITRUNC,NORMHT,HN,HD,ICRI,CL = ',&
             &3I5,2F10.3,I5,F10.3)
        go to 59

        !  CROWNS FOR TREES WITH DBH LT 3.0 (NI), 1.0 (UT & TT) ARE DUBBED HERE.
        !  NO CHANGE IS CALCULATED UNTIL THE TREE ATTAINS THESE LIMITS.
        !  NOT USED BY CR SPECIES

        58 continue
        if (CRVAR)goto 59
        if (.not.LSTART.and. NIVAR) go to 60
        if (UTTVAR .and. ICR(I).ne.0)goto 60
        TPCCF = PCCF(IITRE)
        select case (ISPC)
        case(13,17)
           TEMMAI = RMAILM
        case(18,20,21)
           TEMMAI = RMAIAS
        case default
           TEMMAI = RMAI
        end select
        call DUBSCR(ISPC,D,H,BA,CR,TPCCF,AVH,TEMMAI)
        ICRI = INT(CR*100.0+0.5)
        if (DEBUG)write(JOSTND,*)' AFTER DUBSCR I,ISPC,D,H,BA,CR,ICRI= ',&
             &I,ISPC,D,H,BA,CR,ICRI

        !  ADDED FOR CRNMULT

        if (DBH(I).ge.DLOW(ISPC) .and. DBH(I).lt.DHI(ISPC))&
             &ICRI = INT(real(ICRI) * CRNMLT(ISPC))

        !  END OF CROWN RATIO CALCULATION LOOP.  BOUND CR ESTIMATE AND FILL
        !  THE ICR VECTOR.

        59 continue
        if (ICRI.gt.95) ICRI = 95
        if (.not.NIVAR) then
           if (ICRI .lt.10.and.CRNMLT(ISPC).eq.1.0) ICRI = 10
           if (ICRI .lt.1)ICRI = 1
        else
           if (ICRI .lt.5.and.(CRNMLT(ISPC).eq.1)) ICRI = 5
        end if
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

  if (IREC2.gt. MAXTRE) go to 75
  do I = IREC2,MAXTRE
     if (ICR(I) .gt.0) go to 79
     ISPC = ISP(I)
     D = DBH(I)
     H = HT(I)

     !  SET UP LOGICAL VARIABLES DEPENDING UPON VARIANT SPECIES CAME FROM

     NIVAR = .false.
     CRVAR = .false.
     UTTVAR = .false.
     LPIJU = .false.
     if (ISPC.le.12.or. ISPC.eq.14.or. ISPC.eq.23) then
        NIVAR = .true.
     else if (ISPC.eq.19.or. ISPC.eq.22) then
        CRVAR = .true.
     else if (ISPC.eq.15.or. ISPC.eq.16) then
        LPIJU = .true.
     else
        UTTVAR = .true.
     end if

     !  ADDED FROM UT, TT FOR SPECIES EXPANSION

     IITRE = ITRE(I)
     TPCCF = PCCF(IITRE)
     if (CRVAR .or. LPIJU) then
        if (CRVAR) then
           CL = 5.17281 + 0.32552 * H - 0.01675 * BA
        else if (LPIJU) then
           CL = -0.59373 + 0.67703 * H
        end if
        if (CL .lt.1.0) CL = 1.0
        if (CL .gt. H) CL = H
        CR = CL / H
        ICRI = INT(CR*100.)
     else
        select case (ISPC)
        case(13,17)
           TEMMAI = RMAILM
        case(18,20,21)
           TEMMAI = RMAIAS
        case default
           TEMMAI = RMAI
        end select
        call DUBSCR (ISPC,D,H,BA,CR,TPCCF,AVH,TEMMAI)
        ICRI = INT(CR*100.0 + 0.5)
     end if

     if (ITRUNC(I).eq.0) go to 78
     HN = NORMHT(I)/100.0
     HD = HN-ITRUNC(I)/100.0
     CL = (real(ICRI)/100.)*HN-HD
     ICRI = INT((CL*100./HN)+.5)
     78 continue
     if (ICRI.gt.95) ICRI = 95
     if (ICRI .lt.10) ICRI = 10
     ICR(I) = ICRI
     79 continue
  end do

  75 continue
  if (DEBUG)write(JOSTND,9010)ITRN,(ICR(JJ),JJ = 1,ITRN)
  9010 format(/' LEAVING CROWN 9010 FORMAT ITRN,ICR= ',I10,/,&
       &43(1H ,32I4,/))
  if (DEBUG)write(JOSTND,90)ICYC
  90 format(' LEAVING SUBROUTINE CROWN  CYCLE =',I5)
  return

  entry CRCONS

  !  ENTRY POINT FOR LOADING CROWN RATIO MODEL COEFFICIENTS THAT ARE
  !  SITE DEPENDENT AND REQUIRE ONE TIME RESOLUTION.  ITYPE INDEXES
  !  HABITAT TYPE (CARRIED IN /PLOT/ COMMON AREA), CRHAB CONTAINS HABITAT
  !  INTERCEPTS BY HABITAT TYPE BY SPECIES, AND MAPCR MAPS HABITAT TYPE
  !  ONTO HABITAT CLASS FOR EACH SPECIES.

  !  DATA STATEMENTS FOR WEIBULL PAREMETERS ADDED FOR SPECIES EXPANSION

  ! LOAD WEIBULL 'A' PARAMETER BY SPECIES

  data WEIBA/12*0.0,1.0,3*0.0,1.0,6*0.0/

  ! LOAD WEIBULL 'B' PARAMETER EQUATION CONSTANT COEFFICIENT

  data WEIBB0/12*0.0,-0.82631,3*0.0,-0.82631,-0.08414,0.0,&
       &-0.08414,-0.08414,0.0,0.0/

  ! LOAD WEIBULL 'B' PARAMETER EQUATION SLOPE COEFFICIENT

  data WEIBB1/12*0.0,1.06217,3*0.0,1.06217,1.14765,0.0,&
       &2*1.14765,0.0,0.0/

  ! LOAD WEIBULL 'C' PARAMETER EQUATION CONSTANT COEFFICIENT

  data WEIBC0/12*0.0,3.31429,3*0.0,3.31429,2.77500,0.0,&
       &2*2.77500,0.0,0.0/

  ! LOAD WEIBULL 'C' PARAMETER EQUATION SLOPE COEFFICIENT

  data WEIBC1/23*0.0/

  ! LOAD CR=F(SDI) EQUATION CONSTANT COEFFICIENT

  data C0/12*0.0,6.19911,3*0.0,6.19911,4.01678,0.0,4.01678,&
       &4.01678,0.0,0.0/

  ! LOAD CR=F(SDI) EQUATION SLOPE COEFFICIENT

  data C1/12*0.0,-.02216,0.0,0.0,0.0,-.02216,-.01516,0.0,&
       &-.01516,-.01516,0.0,0.0/

  ! LOAD OTHER MISCELLANEOUS CONSTANTS

  data CRNMLT/MAXSP*1.0/
  data ICFLG/MAXSP*0/
  data DLOW/MAXSP*0.0/
  data DHI/MAXSP*99.0/

  do ISPC = 1,MAXSP
     ICRHAB = MAPHAB(ITYPE,ISPC)
     CRCON(ISPC) = CRHAB(ICRHAB,ISPC)
     80 continue
  end do
  return
end
