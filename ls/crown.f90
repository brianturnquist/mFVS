!============================================!
!   Compute crown ratios and crown changes   !
!============================================!
! LS $Id$
subroutine CROWN
  implicit none

  !  THIS SUBROUTINE IS USED TO DUB MISSING CROWN RATIOS AND
  !  COMPUTE CROWN RATIO CHANGES FOR TREES.  THE EQUATION USED
  !  PREDICTS CROWN RATIO FROM BASAL AREA AND TREE DIAMETER.
  !  THIS EQUATION AND THE COEFFICIENTS ARE FROM 'A GUIDE TO
  !  THE TWIGS PROGRAM FOR THE NORTH CENTRAL U.S.', GENERAL
  !  TECHNICAL REPORT, NC-125.  THIS ROUTINE IS CALLED FROM
  !  **CRATET** TO DUB MISSING VALUES, AND BY **TREGRO** TO
  !  COMPUTE CHANGE DURING REGULAR CYCLING.  ENTRY **CRCONS**
  !  IS CALLED BY **RCON** TO LOAD MODEL CONSTANTS THAT ARE SITE
  !  DEPENDENT AND NEED ONLYBE RESOLVED ONCE.  PROCESSING OF
  !  CROWN CHANGE FOR SMALL TREES IS CONTROLLED BY **REGENT**.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'
  include 'OUTCOM.f90'
  include 'PDEN.f90'
  include 'VARCOM.f90'

  ! Variables
  logical :: DEBUG
  real    :: CRNEW(MAXTRE),BCR1(MAXSP),BCR2(MAXSP),BCR3(MAXSP),BCR4(MAXSP),CRNMLT(MAXSP),DLOW(MAXSP),&
       &DHI(MAXSP)
  integer :: MYACTS(1)
  real    :: PRM(5)
  integer :: ICFLG(MAXSP)
  integer :: JJ,NTODO,I,NP,IACTK,IDATE,IDT,ISPC,IGRP,IULIM,IG,IGSP
  integer :: I1,I2,I3,ICRI,ISPCC
  real    :: D,DEN,CHG,PDIFPY,CRLN,CRMAX,HN,HD,CL,CR
  data MYACTS/81/

  ! Species specific coefficients
  data BCR1/&
       &2*6.640, 2*5.350, 6.790,2*7.840, 5.630, 5.540, 6.000, 5.710,&
       &3*5.710, 2*4.500, 4.350, 4.850,2*4.350,3*4.400, 4.180,&
       &4.440, 3*3.400, 4.490,4*5.840, 4.200,2*5.060,3*6.210, 4.110,&
       &2*4.000, 5.000, 25*4.000/
  data BCR2/&
       &2*.0135, 2*.0053, .0058,2*.0057, .0047, .0072, .0053, .0077,&
       &.0077,2*.0077,2*.0032, .0046, .0050,2*.0046,4*.0025,&
       &.0037,3*.0066, .0029,4*.0082, .0016,2*.0033,3*.0073, .0054,&
       &2*.0024,.0066, 25*.0024/
  data BCR3/&
       &2*3.200, 2*1.528, 7.590,2*1.272, 3.523, 4.200, 0.431, 2.290,&
       &2.290,2*2.290,2*0.795, 1.820, 9.810,2*1.820,3*1.000, 1.410,&
       &2.090,3*2.870, 1.210,4*3.260, 2.760,2*1.730,3*9.990,1.650,&
       &2*-2.830, 4.920,25*-2.830/
  data BCR4/&
       &2*-.0518,2*-.0330,-.0103,2*-.1420,-.0689,-.0530,-.0012,-.2530,&
       &3*-.2530,2*-.1050,-.2740,-.0099,2*-.2740,3*-.0940,-.5120,&
       &-.0650,3*-.4340,-.0650,4*-.0490,-.0250,2*-.0610,3*-.01,-.1100,&
       &2*.0210,-.0263,25*.0210/

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'CROWN',5,ICYC)
  if (DEBUG) write(JOSTND,3)ICYC
  3 format(' ENTERING SUBROUTINE CROWN  CYCLE =',I5)

  ! INITIALIZE CROWN VARIABLES TO BEGINNING OF CYCLE VALUES.

  if (LSTART) then
     do JJ = 1,MAXTRE
        CRNEW(JJ) = 0.0
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

  !  ENTER THE LOOP FOR SPECIES DEPENDENT VARIABLES

  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (I1.eq.0) go to 70
     I2 = ISCT(ISPC,2)
     do I3 = I1,I2
        I = IND1(I3)

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
        DEN = 1+BCR2(ISPC)*BA
        CRNEW(I) = 10*(BCR1(ISPC)/DEN+BCR3(ISPC)*(1-EXP(BCR4(ISPC)*D)))

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
        if (DEBUG)write(JOSTND,*)'DEN= ',DEN,' BA=',BA

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
     DEN = 1+BCR2(ISPC)*BA
     CR = 10*(BCR1(ISPC)/DEN+BCR3(ISPC)*(1-EXP(BCR4(ISPC)*D)))
     ICRI = INT(CR + 0.5)
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
  data CRNMLT/MAXSP*1.0/
  data ICFLG/MAXSP*0/
  data DLOW/MAXSP*0.0/
  data DHI/MAXSP*99.0/
  return
end
