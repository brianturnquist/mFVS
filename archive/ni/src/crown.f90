!============================================!
!   Compute crown ratios and crown changes   !
!============================================!
! NI $Id$
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

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CALCOM.f90'
  include 'ARRAYS.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'
  include 'OUTCOM.f90'
  include 'PLOT.f90'

  !  DECLARATIONS AND DIMENSIONS FOR INTERNAL VARIABLES:

  !  DCRCON -- CONSTANT TERM FOR THE CROWN RATIO MODEL BASED ON
  !            PAST TREE STAND ATTRIBUTES
  !  XCRCON -- CONSTANT TERM FOR THE CROWN RATIO MODEL BASED ON
  !            CURRENT TREE STAND ATTRIBUTES
  !    PARM -- ARRAY OF COEFFICIENTS FOR TERMS INVOLVING TREE
  !            AND STAND ATTRIBUTES

  external RANN
  ! Variables
  logical :: DEBUG
  real    :: PARM(MAXSP,14)
  integer :: MAPHAB(30,MAXSP)
  real    :: CRHAB(14,MAXSP)
  real    :: CRNMLT(MAXSP),DLOW(MAXSP),DHI(MAXSP),PRM(5),CHG,PDIFPY
  integer :: MYACTS(1),ICFLG(MAXSP)
  integer :: J,I,NTODO,NP,IACTK,IDATE,IDT,ISPCC,IGRP,IULIM,IG,IGSP,ISPC
  integer :: I1,I2,I3,ICRI
  real    :: HD,HN,BACHLO,XCR,DCR,EXPDCR,EXPPCR,PCR,P,BRATIO,BARK,H,D
  real    :: B7,B8,B9,B10,B11,B12,B13,B14,DCRCON,XCRCON,X1,X2,OBA,RDM1
  real    :: CRSD,CL,CR

  ! Data statements
  data MYACTS/81/
  data PARM/&
       &0.0,-0.00204,0.0,-0.00183,3*0.0,-0.00203,-0.00190,-0.002165,&
       &-0.00264,&
       &4*0.0,-0.000001902,6*0.0,&
       &-0.34566,4*0.0,0.17479,5*0.0,&
       &5*0.0,-0.00183,5*0.0,&
       &10*0.0,0.000005116,&
       &2*0.0,-0.15334,3*0.0,-0.18555,4*0.0,&
       &0.03882,3*0.0,0.03027,-0.0056,5*0.0,&
       &-0.0007,3*0.0,-0.00055,6*0.0,&
       &.0,0.30066,0.33840,0.24293,2*0.0,0.53172,0.29699,0.23372,&
       &0.26558,0.0,&
       &6*0.0,-0.02989,4*0.0,&
       &6*0.0,0.00011,4*0.0,&
       &-0.21217,-0.59302,-0.59685,-0.25601,-0.25776,2*0.0,-0.38334,&
       &-0.28433,-0.31555,-0.25138,&
       &0.00301,5*0.0,0.0042,0.0,0.001903,2*0.0,&
       &0.0,0.19558,0.16488,0.07260,0.06887,0.1105,0.0,0.09918,0.0,&
       &0.16072,0.05140/
  data ((MAPHAB(I,J),I = 1,30), J = 1,5)/&
       &12*2,3,3*4,3*5,6,6,1,6,1,7,1,4*6,&
       &7*2,3,2,4,4,5,6,3*7,8,8,4,1,10,9,10,1,11,4*1,2,&
       &3*2,3*4,6,7,4,8,8,5,9,3*10,2*11,8,1,2*12,13,1,3,1,3,3*1,&
       &9*2,1,1,2,3,3*4,5,5,6,1,3*7,1,8,1,7,3*1,&
       &30*1/
  data ((MAPHAB(I,J),I = 1,30), J = 6,11)/&
       &16*1,3*2,11*1,&
       &6*2,4,5,5,2,2,6,7,3*8,9,9,10,2*11,12,11,1,13,1,14,3,3,11,&
       &7*2,3,2,1,1,2,4,3*5,6,6,7,8,8,9,8,10,11,1,1,2*12,8,&
       &13*2,3,4*4,5,6,6,7,6,1,8,1,9,2*10,6,&
       &2,2,4,3*1,5,6,3*1,8,7,3*9,3*3,11*1,&
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
       &0.4649, 0.3211, 0.1970, 0.2295, 0.3383, 0.3450, 8*0.0/

  !     DATA CRSD/12.69/
  !  THIS IS A POPULATION ESTIMATE AND TOO LARGE FOR WITHIN STAND VARIATION.
  !  CUT IT IN HALF UNTIL SOMETHING BETTER COMES ALONG.   DIXON  4/19/99

  data CRSD/6.35/

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'CROWN',5,ICYC)

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

  !  PROCESS CRNMULT KEYWORD.

  call OPFIND(1,MYACTS,NTODO)
  if (NTODO .eq.0)go to 25
  do I = 1,NTODO
     call OPGET(I,5,IDATE,IACTK,NP,PRM)
     IDT = IDATE
     call OPDONE(I,IDT)
     ISPCC = INT(PRM(1))
     if (DEBUG)write(JOSTND,*)' PRM1= ',PRM

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
           15 continue
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
  if (DEBUG)write(JOSTND,*)' PRM2= ',PRM
  25 continue
  if (DEBUG)write(JOSTND,9024)ICYC,CRNMLT
  9024 format(/' IN CROWN 9024 ICYC,CRNMLT= ',&
       &I5/((1X,11F6.2)/))

  !  IF CCF LAST CYCLE WAS LESS THAN 100.0, THERE IS ASSUMED TO BE
  !  NO DENSITY IMPACT ON CROWN RATIOS.

  RDM1 = RELDM1
  OBA = OLDBA
  if (RELDM1.ge.100.0) go to 20
  OBA = BA
  RDM1 = RELDEN
  20 continue
  if (LSTART) goto 21
  X1 = ALOG(OBA)
  X2 = ALOG(RDM1)
  21 continue

  !  ENTER THE LOOP FOR SPECIES DEPENDENT VARIABLES

  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (I1.eq.0)goto 70
     I2 = ISCT(ISPC,2)
     XCRCON = CRCON(ISPC) + PARM(ISPC,1)*BA + PARM(ISPC,2)*BA*BA +&
          &PARM(ISPC,3)*ALOG(BA) + PARM(ISPC,4)*RELDEN + PARM(ISPC,5)&
          &*RELDEN*RELDEN + PARM(ISPC,6)*ALOG(RELDEN)
     DCRCON = 0.0

     !  IF DUBBING MISSING CROWN RATIOS, BYPASS ASSIGNMENT OF DCRCON.

     if (LSTART) go to 30
     DCRCON = CRCON(ISPC) + PARM(ISPC,1)*OBA + PARM(ISPC,2)*OBA*OBA +&
          &PARM(ISPC,3)*X1 + PARM(ISPC,4)*RDM1 + PARM(ISPC,5)*RDM1&
          &*RDM1 + PARM(ISPC,6)*X2

     !  WRITE DEBUG INFO (CONSTANT TERMS) IF DESIRED

     30 continue
     if (DEBUG) write(JOSTND,9001) ISPC, XCRCON, DCRCON
     9001 format(' SPECIES: ',I2,' - XCRCON = ',F10.3,', DCRCON = ',F10.3)

     !  VARIABLES B1, B2, ..., B14 ARE SET UP TO ELIMINATE THE
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

     !  ENTER LOOP FOR TREE DEPENDENT VARIABLES

     do I3 = I1,I2
        I = IND1(I3)

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

        !  BRANCH TO STATEMENT 58 TO HANDLE TREES WITH DBH LESS THAN 3 IN.

        if (D.lt.3.0) go to 58

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
        if ((OLDPCT(I).gt.PCT(I).and.ONTREM(7).gt.0.0).or.OLDPCT(I).le.0.0)&
             &OLDPCT(I) = PCT(I)
        P = OLDPCT(I)
        if (P.lt.0.01)P = 0.01

        !  CALCULATE THE PREDICTED CROWN RATIO AT THE START OF THE CYCLE.

        DCR = DCRCON + B7*D + B8*D*D + B9*ALOG(D) + B10*H + B11*H*H +&
             &B12*ALOG(H) + B13*P + B14*ALOG(P)
        EXPDCR = EXP(DCR)

        !  WRITE DEBUG INFO IF DESIRED

        50 continue
        if (DEBUG) write(JOSTND,9002) I,ICR(I),EXPPCR,EXPDCR,D,H,PCT(I),&
             &OLDPCT(I)
        9002 format(' ICR(',I4,')=',I3,'  EXPPCR=',F10.3,' EXPDCR=',F10.3,&
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

        !  REDUCE CROWNS OF TREES  FLAGGED AS TOP-KILLED ON INVENTORY

        if (.not.LSTART .or. ITRUNC(I).eq.0) go to 59
        HN = real(NORMHT(I))/100.0
        HD = HN-real(ITRUNC(I))/100.0
        CL = (real(ICRI)/100.)*HN-HD
        ICRI = INT((CL*100./HN)+.5)
        go to 59

        !  CROWNS FOR TREES WITH DBH LT 3.0 IN ARE DUBBED HERE.  NO CHANGE
        !  IS CALCULATED UNTIL THE TREE ATTAINS A DBH OF 3 INCHES.

        58 continue
        if (.not.LSTART) go to 60
        call DUBSCR(ISPC,D,H,BA,CR)
        ICRI = INT(CR*100.0+0.5)
        if (DEBUG)write(JOSTND,*)' AFTER DUBSCR I,ISPC,D,H,BA,CR,ICRI= ',&
             &I,ISPC,D,H,BA,CR,ICRI

        !  ADDED FOR CRNMLT ADJUSTMENT OF DUBBED VALUES

        if (DBH(I).ge.DLOW(ISPC) .and. DBH(I).lt.DHI(ISPC))&
             &ICRI = INT(real(ICRI) * CRNMLT(ISPC))

        !  END OF CROWN RATIO CALCULATION LOOP.  BOUND CR ESTIMATE AND FILL
        !  THE ICR VECTOR.

        59 continue
        if (ICRI.gt.95) ICRI = 95
        if ((ICRI.lt.5).and.(CRNMLT(ISPC).eq.1)) ICRI = 5
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
     call DUBSCR (ISPC,D,H,BA,CR)
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

  75 continue
  return
  entry CRCONS

  !  ENTRY POINT FOR LOADING CROWN RATIO MODEL COEFFICIENTS THAT ARE
  !  SITE DEPENDENT AND REQUIRE ONE TIME RESOLUTION.  ITYPE INDEXES
  !  HABITAT TYPE (CARRIED IN /PLOT/ COMMON AREA), CRHAB CONTAINS HABITAT
  !  INTERCEPTS BY HABITAT TYPE BY SPECIES, AND MAPCR MAPS HABITAT TYPE
  !  ONTO HABITAT CLASS FOR EACH SPECIES.

  do ISPC = 1,MAXSP
     ICRHAB = MAPHAB(ITYPE,ISPC)
     CRCON(ISPC) = CRHAB(ICRHAB,ISPC)
     80 continue
  end do
  data CRNMLT/MAXSP*1.0/
  data ICFLG/MAXSP*0/
  data DLOW/MAXSP*0.0/
  data DHI/MAXSP*99.0/
  return
end
