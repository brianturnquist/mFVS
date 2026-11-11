!============================================!
!   Compute crown ratios and crown changes   !
!============================================!
! ACD $Id$
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
  real    :: CRNEW(MAXTRE),BCR1(MAXSP),BCR2(MAXSP),BCR3(MAXSP),BCR4(MAXSP),PRM(5),CRNMLT(MAXSP), &
       & DLOW(MAXSP),DHI(MAXSP)
  integer :: MYACTS(1),ICFLG(MAXSP)
  integer :: JJ,NTODO,I,NP,IACTK,IDATE,IDT,ISPCC,IGRP,IULIM,IG,IGSP
  integer :: ISPC,I1,I2,I3,ICRI
  real    :: CR,CL,HD,HN,CRMAX,CRLN,PDIFPY,CHG,DEN,D
  data MYACTS/81/

  ! Species specific coefficients
  data BCR1/ &
       & 5.630,6.000,7.840,4*5.540,5.540, 6.640, 5.350, 6.790, &
       & 4*5.710,2*5.710,8*4.350,3.400,3*4.180,3*4.180,2*5.000, &
       & 5*4.000,6.210,5*3.733,3*3.733,5*4.500,3.733,5*4.110,4*4.000, &
       & 3*3.733,2*5.840,2*4.200,27*4.000,11*4.000/
  data BCR2/ &
       & .0047,.0053,.0057,4*.0072,.0072, .0135, .0053, .0058, &
       & 4*.0077,2*.0077,8*.0046,.0066,3*.0025,3*.0025,2*.0066, &
       & 5*.0024,.0073,5*.0040,3*.0040,5*.0032,.0040,5*.0054,4*.0024, &
       & 3*.0040,2*.0082,2*.0016,27*.0024,11*.0024/
  data BCR3/ &
       & 3.523,0.431,1.272,4*4.200, 4.200, 3.200, 1.528, 7.590, &
       & 4*2.290,2*2.290,8*1.820,2.870,3*1.410,3*1.410,2*4.920, &
       & 5*-2.830,9.990,5*3.632,3*3.632,5*0.795,3.632,5*1.650, &
       & 4*-2.830,3*3.632,2*3.260,2*2.760,27*-2.830,11*-2.830/
  data BCR4/ &
       & -.0689,-.0012,-.1420,4*-.0530,-.0530,-.0518,-.0330,-.0103, &
       & 4*-.2530,2*-.2530,8*-.2740,-.4340,3*-.5120,3*-.5120,2*-.0263, &
       & 5*.0210,-.0100,5*-.0412,3*-.0412,5*-.1050,-.0412,5*-.1100, &
       & 4*.0210,3*-.0412,2*-.0490,2*-.0250,27*.0210,11*.0210/

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
  if (NTODO .eq. 0)go to 25
  do I = 1,NTODO
     call OPGET(I,5,IDATE,IACTK,NP,PRM)
     IDT = IDATE
     call OPDONE(I,IDT)
     ISPCC = IFIX(PRM(1))

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
  if (DEBUG)write(JOSTND,9024)ICYC,CRNMLT
  9024 format(/' IN CROWN 9024 ICYC,CRNMLT= ', &
       & I5/((1X,11F6.2)/))

  !  ENTER THE LOOP FOR SPECIES DEPENDENT VARIABLES

  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (I1 .eq. 0) go to 70
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
        CHG = CRNEW(I) - ICR(I)
        PDIFPY = CHG/ICR(I)/FINT
        if (PDIFPY.gt.0.01)CHG = ICR(I)*(0.01)*FINT
        if (PDIFPY.lt.-0.01)CHG = ICR(I)*(-0.01)*FINT
        if (DEBUG)write(JOSTND,9020)I,CRNEW(I),ICR(I),PDIFPY,CHG
        9020 format(/'  IN CROWN 9020 I,CRNEW,ICR,PDIFPY,CHG =', &
             & I5,F10.3,I5,3F10.3)
        if (DBH(I).ge.DLOW(ISPC) .and. DBH(I).le.DHI(ISPC)) then
           CRNEW(I) = ICR(I) + CHG * CRNMLT(ISPC)
        else
           CRNEW(I) = ICR(I) + CHG
        end if
        9052 ICRI = CRNEW(I)+0.5
        if (LSTART .or. ICR(I).eq.0) then
           if (DBH(I).ge.DLOW(ISPC) .and. DBH(I).le.DHI(ISPC)) &
                & ICRI = ICRI * CRNMLT(ISPC)
        end if

        ! CALC CROWN LENGTH NOW

        if (LSTART .or. ICR(I).eq.0)go to 55
        CRLN = HT(I)*ICR(I)/100.

        ! CALC CROWN LENGTH MAX POSSIBLE IF ALL HTG GOES TO NEW CROWN

        CRMAX = (CRLN+HTG(I))/(HT(I)+HTG(I))*100.0
        if (DEBUG)write(JOSTND,9004)CRMAX,CRLN,ICRI,I,CRNEW(I), &
             & CHG
        9004 format(' CRMAX=',F10.2,' CRLN=',F10.2, &
             & ' ICRI=',I10,' I=',I5,' CRNEW=',F10.2,' CHG=',F10.3)

        ! IF NEW CROWN EXCEEDS MAX POSSIBLE LIMIT IT TO MAX POSSIBLE

        if (ICRI.gt.CRMAX) ICRI = CRMAX+0.5
        if (ICRI.lt.10 .and. CRNMLT(ISPC).eq.1.0)ICRI = CRMAX+0.5

        !  REDUCE CROWNS OF TREES  FLAGGED AS TOP-KILLED ON INVENTORY

        55 if (.not.LSTART .or. ITRUNC(I).eq.0) go to 59
        HN = NORMHT(I)/100.0
        HD = HN-ITRUNC(I)/100.0
        CL = (FLOAT(ICRI)/100.)*HN-HD
        ICRI = IFIX((CL*100./HN)+.5)
        if (DEBUG)write(JOSTND,9030)I,ITRUNC(I),NORMHT(I),HN,HD, &
             & ICRI,CL
        9030 format(' IN CROWN 9030 I,ITRUNC,NORMHT,HN,HD,ICRI,CL = ', &
             & 3I5,2F10.3,I5,F10.3)
        go to 59

        !  BALANCING ACT BETWEEN TWO CROWN MODELS OCCURS HERE
        !  END OF CROWN RATIO CALCULATION LOOP.  BOUND CR ESTIMATE AND FILL
        !  THE ICR VECTOR.

        59 continue
        if (ICRI.gt.95) ICRI = 95
        if (ICRI .lt. 10 .and. CRNMLT(ISPC).eq.1) ICRI = 10
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

  if (IREC2 .gt. MAXTRE) go to 80
  do I = IREC2,MAXTRE
     if (ICR(I) .gt. 0) go to 79
     ISPC = ISP(I)
     D = DBH(I)
     DEN = 1+BCR2(ISPC)*BA
     CR = 10*(BCR1(ISPC)/DEN+BCR3(ISPC)*(1-EXP(BCR4(ISPC)*D)))
     ICRI = CR + 0.5
     if (ITRUNC(I).eq.0) go to 78
     HN = NORMHT(I)/100.0
     HD = HN-ITRUNC(I)/100.0
     CL = (FLOAT(ICRI)/100.)*HN-HD
     ICRI = IFIX((CL*100./HN)+.5)
     78 continue
     if (ICRI.gt.95) ICRI = 95
     if (ICRI .lt. 10) ICRI = 10
     ICR(I) = ICRI
     79 continue
  end do

  80 continue
  if (DEBUG)write(JOSTND,9010)ITRN,(ICR(JJ),JJ = 1,ITRN)
  9010 format(' LEAVING CROWN 9010 FORMAT ITRN,ICR= ',I10,/, &
       & 43(1H ,32I4,/))
  if (DEBUG)write(JOSTND,90)ICYC
  90 format(' LEAVING SUBROUTINE CROWN  CYCLE =',I5)
  return
  entry CRCONS
  data CRNMLT/MAXSP*1.0/
  data ICFLG/MAXSP*0/
  data DLOW/MAXSP*0.0/
  data DHI/MAXSP*99.0/
  return
end subroutine CROWN
