!============================================!
!   Compute crown ratios and crown changes   !
!============================================!
! EM $Id$
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
  !  ROUTINE IS CALLED FROM **CRATET** TO DUB MISSING VALUES, AND BY
  !  **TREGRO** TO COMPUTE CHANGE DURING REGULAR CYCLING.  ENTRY
  !  **CRCONS** IS CALLED BY **RCON** TO LOAD MODEL CONSTANTS THAT ARE
  !  SITE DEPENDENT AND NEED ONLY BE RESOLVED ONCE.  A CALL TO **DUBSCR**
  !  IS ISSUED TO DUB CROWN RATIO WHEN DBH IS LESS THAN 3 INCHES.
  !  PROCESSING OF CROWN CHANGE FOR SMALL TREES IS CONTROLLED BY
  !  **REGENT**.

  !  WHITEBARK PINE (WB) USES COEFFICIENTS FROM WESTERN LARCH
  !  OTHER SOFTWOODS (OS) ARE REALLY COEFFEICIENTS FOR MOUNTAIN HEMLOCK

  !  SPECIES EXPANSION:
  !  LIMBER PINE (LM) USES IE-LM COEFFICIENTS WHICH ARE ORIGINALLY FROM TT
  !  SUBALPINE LARCH (LL) USES IE-AF COEFFICIENTS
  !  ROCKY MTN JUNIPER (RM) USES IE-JU COEFFICIENTS WHICH ARE ORIGINALLY FROM CR
  !  GREEN ASH (GA), BLACK COTTONWOOD (CW), BALSAM POPLAR (BA), PLAINS COTTONWOOD
  !    (PW), NARROWLEAF COTTONWOOD (NC), AND OTHER HARDWOODS (OH) USE IE-CO
  !    COEFFICIENTS WHICH ARE ORIGINALLY FROM CR
  !  QUAKING ASPEN (AS) AND PAPER BIRCH (PB) USE IE-AS WHICH ARE ORIGINALLY FROM UT

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

  external RANN
  ! Variables
  logical :: DEBUG,NIVAR,CRVAR,UTTVAR,LPIJU,EMVAR
  real    :: PARM(14)
  integer :: MAPHAB(30)
  real    :: CRHAB(14)
  real    :: CRNEW(MAXTRE),WEIBA(MAXSP),WEIBB0(MAXSP),WEIBB1(MAXSP),WEIBC0(MAXSP),WEIBC1(MAXSP),&
       &C0(MAXSP),C1(MAXSP),CRNMLT(MAXSP),DLOW(MAXSP),DHI(MAXSP),PRM(5)
  integer :: ISORT(MAXTRE),MYACTS(1),ICFLG(MAXSP)
  real    :: OBA,XCRCON,DCRCON,RELSDI
  real    :: ACRNEW,A,B,C,D,H,BARK,HF,CL,CR,SCALE,X,RNUMB
  integer :: ISPC,I1,I2,I3,IITRE,I,JJ,NTODO,NP,IACTK
  integer :: IDATE,IDT,ISPCC,IGRP,IULIM,IG,IGSP,J1,ICRI
  real    :: CRSD,X1,X2,RDM1,BRATIO,P,PCR,EXPPCR,EXPDCR
  real    :: DCR,CHG,PDIFPY,XCR,BACHLO,CRLN,CRMAX,HN,HD,TPCCF,TPCT

  !  SPECIES ORDER:
  !   1=WB,  2=WL,  3=DF,  4=LM,  5=LL,  6=RM,  7=LP,  8=ES,
  !   9=AF, 10=PP, 11=GA, 12=AS, 13=CW, 14=BA, 15=PW, 16=NC,
  !  17=PB, 18=OS, 19=OH

  !  SPECIES EXPANSION
  !  LM USES IE LM (ORIGINALLY FROM TT VARIANT)
  !  LL USES IE AF (ORIGINALLY FROM NI VARIANT)
  !  RM USES IE JU (ORIGINALLY FROM UT VARIANT)
  !  AS,PB USE IE AS (ORIGINALLY FROM UT VARIANT)
  !  GA,CW,BA,PW,NC,OH USE IE CO (ORIGINALLY FROM CR VARIANT)

  data MYACTS/81/

  !  DATA STATEMENTS FROM IE/NI VARIANT SPECIES 9

  data PARM/&
       &-0.00190, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.23372, 0.0, 0.0,&
       &-0.28433, 0.001903, 0.0/

  data MAPHAB/13*2,3,4*4,5,6,6,7,6,1,8,1,9,2*10,6/

  data CRHAB/&
       &0.09453, -0.07740, 0.07113, 0.2039, 0.06176, 0.1513, 0.09086,&
       &0.1580, 0.09229, 0.01551, 4*0.0/

  !     DATA CRSD/12.69/
  !  THIS IS A POPULATION ESTIMATE AND TOO LARGE FOR WITHIN STAND VARIATION.
  !  CUT IT IN HALF UNTIL SOMETHING BETTER COMES ALONG.   DIXON  4/19/99

  data CRSD/6.35/

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'CROWN',5,ICYC)

  if (DEBUG) write(JOSTND,3)ICYC
  3 format(' ENTERING SUBROUTINE CROWN  CYCLE =',I5)
  if (DEBUG) write(JOSTND,9000) BA, RELDEN, RELDM1
  9000 format(' IN CROWN, BA=',F7.3,'  RELDEN=',F7.3,'  RELDM1=',F7.3)

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
     ISPCC = IFIX(PRM(1))

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

     EMVAR = .false.
     NIVAR = .false.
     CRVAR = .false.
     UTTVAR = .false.
     LPIJU = .false.
     if (ISPC.eq.5) then
        NIVAR = .true.
     else if (ISPC.eq.11.or. (ISPC.ge.13.and. ISPC.le.16) .or.&
          &ISPC.eq.19) then
        CRVAR = .true.
     else if (ISPC.eq.6) then
        LPIJU = .true.
     else if (ISPC.eq.4.or. ISPC.eq.12.or. ISPC.eq.17) then
        UTTVAR = .true.
     else
        EMVAR = .true.
     end if

     XCRCON = 0.
     DCRCON = 0.
     if (NIVAR) then
        XCRCON = CRCON(ISPC) + PARM(1)*BA + PARM(2)*BA*BA +&
             &PARM(3)*ALOG(BA) + PARM(4)*RELDEN + PARM(5)&
             &*RELDEN*RELDEN + PARM(6)*ALOG(RELDEN)
     end if

     !  IF DUBBING MISSING CROWN RATIOS, BYPASS ASSIGNMENT OF DCRCON.

     if (LSTART) go to 30
     if (NIVAR) then
        DCRCON = CRCON(ISPC) + PARM(1)*OBA + PARM(2)*OBA*OBA +&
             &PARM(3)*X1 + PARM(4)*RDM1 + PARM(5)*RDM1&
             &*RDM1 + PARM(6)*X2
     end if

     !  WRITE DEBUG INFO (CONSTANT TERMS) IF DESIRED

     30 continue
     if (DEBUG) write(JOSTND,9001) ISPC, XCRCON, DCRCON
     9001 format(' SPECIES: ',I2,' - XCRCON = ',F10.3,', DCRCON = ',F10.3)

     ! ESTIMATE MEAN CROWN RATIO FROM SDI, AND ESTIMATE WEIBULL PARAMETERS

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
        !  FOR EM, AND TT AND UT SPECIES, BRANCH TO 58 IF D < 1 IN.
        !  "ELSE" PORTION APPLIES IF NIVAR IS .TRUE., BRANCH IF D < 3 IN.
        !  CASE WHERE CRVAR OR LPIJU IS TRUE BRANCH TO STMT 53 ABOVE.

        if (EMVAR .or. UTTVAR) then
           if (D.lt.1.0.and.LSTART) go to 58
        else
           if (D.lt.3.0) go to 58
        end if

        !  CALCULATE THE PREDICTED CURRENT CROWN RATIO

        if (EMVAR .or. UTTVAR) then
           SCALE = (1.0 - .00167 * (RELDEN-100.0))
           if (SCALE .gt.1.0) SCALE = 1.0
           if (SCALE .lt.0.30) SCALE = 0.30
           if (DBH(I) .gt.0.0) then
              X = (FLOAT(ISORT(I)) / FLOAT(ITRN)) * SCALE
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
        end if

        !  THE FOLLOWING LOGIC IS USED ONLY BY ORIGINAL NI SPECIES
        !  BYPASS CROWN CHANGE CALCULATION IF CROWN RATIO WAS ASSIGNED
        !  THIS CYCLE IN REGENT.

        if (NIVAR) then
           if (.not.LSTART .and. D-DG(I)/BARK .lt.3.0) go to 60
           P = PCT(I)
           if (P.lt.0.01)P = 0.01

           !  CALCULATE THE PREDICTED CURRENT CROWN RATIO

           PCR = XCRCON + PARM(7)*D + PARM(8)*D*D + PARM(9)*ALOG(D) +&
                &PARM(10)*H + PARM(11)*H*H + PARM(12)*ALOG(H) +&
                &PARM(13)*P + PARM(14)*ALOG(P)
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

           DCR = DCRCON + PARM(7)*D + PARM(8)*D*D + PARM(9)*ALOG(D) +&
                &PARM(10)*H + PARM(11)*H*H + PARM(12)*ALOG(H) +&
                &PARM(13)*P + PARM(14)*ALOG(P)
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
           ICRI = INT(CRNEW(I) + 0.5)
        else
           !  DUB CROWN
           ICRI = INT(CRNEW(I) + 0.5)
           if (DBH(I).ge.DLOW(ISPC).and.DBH(I).lt.DHI(ISPC))&
                &ICRI = INT(real(ICRI)*CRNMLT(ISPC))
        end if

        ! CALC CROWN LENGTH NOW

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
        CL = (FLOAT(ICRI)/100.)*HN-HD
        ICRI = IFIX((CL*100./HN)+.5)
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
        if ((EMVAR .or. UTTVAR) .and. ICR(I).ne.0)goto 60
        TPCT = PCT(I)
        TPCCF = PCCF(IITRE)
        call DUBSCR(ISPC,D,H,CR,TPCT,TPCCF)
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
        if (NIVAR) then
           if ((ICRI.lt.5).and.(CRNMLT(ISPC).eq.1)) ICRI = 5
        else
           if (ICRI .lt.10.and.CRNMLT(ISPC).eq.1.0) ICRI = 10
           if (ICRI .lt.1)ICRI = 1
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
     TPCT = PCT(I)
     IITRE = ITRE(I)
     TPCCF = PCCF(IITRE)

     !  SET UP LOGICAL VARIABLES DEPENDING UPON VARIANT SPECIES CAME FROM

     EMVAR = .false.
     NIVAR = .false.
     CRVAR = .false.
     UTTVAR = .false.
     LPIJU = .false.
     if (ISPC.eq.5) then
        NIVAR = .true.
     else if (ISPC.eq.11.or. (ISPC.ge.13.and. ISPC.le.16) .or.&
          &ISPC.eq.19) then
        CRVAR = .true.
     else if (ISPC.eq.6) then
        LPIJU = .true.
     else if (ISPC.eq.4.or. ISPC.eq.12.or. ISPC.eq.17) then
        UTTVAR = .true.
     else
        EMVAR = .true.
     end if

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
        call DUBSCR(ISPC,D,H,CR,TPCT,TPCCF)
        ICRI = INT(CR*100.0 + 0.5)
     end if

     if (ITRUNC(I).eq.0) go to 78
     HN = real(NORMHT(I))/100.0
     HD = HN-real(ITRUNC(I))/100.0
     CL = (FLOAT(ICRI)/100.)*HN-HD
     ICRI = IFIX((CL*100./HN)+.5)
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

  ! SPECIES ORDER
  !   1=WB,  2=WL,  3=DF,  4=LM,  5=LL,  6=RM,  7=LP,  8=ES,
  !   9=AF, 10=PP, 11=GA, 12=AS, 13=CW, 14=BA, 15=PW, 16=NC,
  !  17=PB, 18=OS, 19=OH

  ! LOAD WEIBULL 'A' PARAMETER BY SPECIES

  data WEIBA/3*0.0, 1.0, 15*0.0/

  ! LOAD WEIBULL 'B' PARAMETER EQUATION CONSTANT COEFFICIENT

  data WEIBB0/  0.11035,  0.11035,  0.14652, -0.82631,      0.0,&
       &0.0, -0.00359,  0.67059,  0.73693,  0.02663,      0.0,&
       &-0.08414,      0.0,      0.0,      0.0,      0.0, -0.08414,&
       &0.11035,      0.0/

  ! LOAD WEIBULL 'B' PARAMETER EQUATION SLOPE COEFFICIENT

  data WEIBB1/  1.10085,  1.10085,  1.09052,  1.06217,      0.0,&
       &0.0,  1.12728,  0.99349,  0.98414,  1.11477,      0.0,&
       &1.14765,      0.0,      0.0,      0.0,      0.0,  1.14765,&
       &1.10085,      0.0/

  ! LOAD WEIBULL 'C' PARAMETER EQUATION CONSTANT COEFFICIENT

  data WEIBC0/  0.02774,  0.02774,  1.04746,  3.31429,      0.0,&
       &0.0,  2.60377, -4.25938, -4.16681,  2.95048,      0.0,&
       &2.77500,      0.0,      0.0,      0.0,      0.0,  2.77500,&
       &0.02774,      0.0/

  ! LOAD WEIBULL 'C' PARAMETER EQUATION SLOPE COEFFICIENT

  data WEIBC1/  0.35524,  0.35524,  0.39752,      0.0,      0.0,&
       &0.0,      0.0,  1.35687,  1.33779,      0.0,      0.0,&
       &0.0,      0.0,      0.0,      0.0,      0.0,      0.0,&
       &0.35524,      0.0/

  ! LOAD CR=F(SDI) EQUATION CONSTANT COEFFICIENT

  data C0/      5.68625,  5.68625,  5.92714,  6.19911,      0.0,&
       &0.0,  5.05870,  7.41093,  7.36476,  5.61047,      0.0,&
       &4.01678,      0.0,      0.0,      0.0,      0.0,  4.01678,&
       &5.68625,      0.0/

  ! LOAD CR=F(SDI) EQUATION SLOPE COEFFICIENT

  data C1/     -0.04470, -0.04470, -0.03346, -0.02216,      0.0,&
       &0.0, -0.03307, -0.03467, -0.03761, -0.03557,      0.0,&
       &-0.01516,      0.0,      0.0,      0.0,      0.0, -0.01516,&
       &-0.04470,      0.0/

  ! LOAD OTHER MISCELLANEOUS CONSTANTS

  data CRNMLT/MAXSP*1.0/
  data ICFLG/MAXSP*0/
  data DLOW/MAXSP*0.0/
  data DHI/MAXSP*99.0/

  ICRHAB = MAPHAB(ITYPE)
  do ISPC = 1,MAXSP
     if (ISPC .eq.5) then
        CRCON(ISPC) = CRHAB(ICRHAB)
     else
        CRCON(ISPC) = 0.
     end if
     80 continue
  end do

  return
end
