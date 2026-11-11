!============================================!
!   Compute crown ratios and crown changes   !
!============================================!
! WS $Id$
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
  !  CROWN RATIO WHEN DBH IS LESS THAN 1 INCH.

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
  logical :: DEBUG
  integer :: MYACTS(1),ICFLG(MAXSP),ISORT(MAXTRE)
  integer :: JJ,NTODO,I,NP,IACTK,IDATE,IDT,ISPCC,IGRP,IULIM,IG,IGSP,J1
  integer :: ISPC,I1,I2,I3,IITRE,ICRI
  real    :: CRNEW(MAXTRE),WEIBA(MAXSP),WEIBB0(MAXSP),WEIBB1(MAXSP),WEIBC0(MAXSP),WEIBC1(MAXSP),&
       &C0(MAXSP),C1(MAXSP),CRNMLT(MAXSP),DLOW(MAXSP),DHI(MAXSP),PRM(5)
  real    :: RELSDI,ACRNEW,A,B,C,D,H,SCALE,X,RNUMB,CHG
  real    :: PDIFPY,CRLN,CRMAX,HN,HD,CL,TPCT,TPCCF,CR,HF
  real    :: BAPLT,QMDPLT,TPAPLT,PRD,DBHLO,DBHHI,SDICS,SDICZ,XMAX
  real    :: ZRD(MAXPLT),HDR
  integer :: IWHO

  !     SPECIES LIST FOR WESTERN SIERRAS VARIANT.

  !     1 = SUGAR PINE (SP)                   PINUS LAMBERTIANA
  !     2 = DOUGLAS-FIR (DF)                  PSEUDOTSUGA MENZIESII
  !     3 = WHITE FIR (WF)                    ABIES CONCOLOR
  !     4 = GIANT SEQUOIA (GS)                SEQUOIADENDRON GIGANTEAUM
  !     5 = INCENSE CEDAR (IC)                LIBOCEDRUS DECURRENS
  !     6 = JEFFREY PINE (JP)                 PINUS JEFFREYI
  !     7 = CALIFORNIA RED FIR (RF)           ABIES MAGNIFICA
  !     8 = PONDEROSA PINE (PP)               PINUS PONDEROSA
  !     9 = LODGEPOLE PINE (LP)               PINUS CONTORTA
  !    10 = WHITEBARK PINE (WB)               PINUS ALBICAULIS
  !    11 = WESTERN WHITE PINE (WP)           PINUS MONTICOLA
  !    12 = SINGLELEAF PINYON (PM)            PINUS MONOPHYLLA
  !    13 = PACIFIC SILVER FIR (SF)           ABIES AMABILIS
  !    14 = KNOBCONE PINE (KP)                PINUS ATTENUATA
  !    15 = FOXTAIL PINE (FP)                 PINUS BALFOURIANA
  !    16 = COULTER PINE (CP)                 PINUS COULTERI
  !    17 = LIMBER PINE (LM)                  PINUS FLEXILIS
  !    18 = MONTEREY PINE (MP)                PINUS RADIATA
  !    19 = GRAY PINE (GP)                    PINUS SABINIANA
  !         (OR CALIFORNIA FOOTHILL PINE)
  !    20 = WASHOE PINE (WE)                  PINUS WASHOENSIS
  !    21 = GREAT BASIN BRISTLECONE PINE (GB) PINUS LONGAEVA
  !    22 = BIGCONE DOUGLAS-FIR (BD)          PSEUDOTSUGA MACROCARPA
  !    23 = REDWOOD (RW)                      SEQUOIA SEMPERVIRENS
  !    24 = MOUNTAIN HEMLOCK (MH)             TSUGA MERTENSIANA
  !    25 = WESTERN JUNIPER (WJ)              JUNIPERUS OCIDENTALIS
  !    26 = UTAH JUNIPER (UJ)                 JUNIPERUS OSTEOSPERMA
  !    27 = CALIFORNIA JUNIPER (CJ)           JUNIPERUS CALIFORNICA
  !    28 = CALIFORNIA LIVE OAK (LO)          QUERCUS AGRIFOLIA
  !    29 = CANYON LIVE OAK (CY)              QUERCUS CHRYSOLEPSIS
  !    30 = BLUE OAK (BL)                     QUERCUS DOUGLASII
  !    31 = CALIFORNIA BLACK OAK (BO)         QUERQUS KELLOGGII
  !    32 = VALLEY OAK (VO)                   QUERCUS LOBATA
  !         (OR CALIFORNIA WHITE OAK)
  !    33 = INTERIOR LIVE OAK (IO)            QUERCUS WISLIZENI
  !    34 = TANOAK (TO)                       LITHOCARPUS DENSIFLORUS
  !    35 = GIANT CHINQUAPIN (GC)             CHRYSOLEPIS CHRYSOPHYLLA
  !    36 = QUAKING ASPEN (AS)                POPULUS TREMULOIDES
  !    37 = CALIFORNIA-LAUREL (CL)            UMBELLULARIA CALIFORNICA
  !    38 = PACIFIC MADRONE (MA)              ARBUTUS MENZIESII
  !    39 = PACIFIC DOGWOOD (DG)              CORNUS NUTTALLII
  !    40 = BIGLEAF MAPLE (BM)                ACER MACROPHYLLUM
  !    41 = CURLLEAF MOUNTAIN-MAHOGANY (MC)   CERCOCARPUS LEDIFOLIUS
  !    42 = OTHER SOFTWOODS (OS)
  !    43 = OTHER HARDWOODS (OH)

  !  SURROGATE EQUATION ASSIGNMENT:

  !    FROM EXISTING WS EQUATIONS --
  !      USE 1(SP) FOR 11(WP) AND 24(MH)
  !      USE 2(DF) FOR 22(BD)
  !      USE 3(WF) FOR 13(SF)
  !      USE 4(GS) FOR 23(RW)
  !      USE 8(PP) FOR 18(MP)
  !      USE 34(TO) FOR 35(GC), 36(AS), 37(CL), 38(MA), AND 39(DG)
  !      USE 31(BO) FOR 28(LO), 29(CY), 30(BL), 32(VO), 33(IO), 40(BM), AND
  !                     43(OH)

  !    FROM CA VARIANT --
  !      USE CA11(KP) FOR 12(PM), 14(KP), 15(FP), 16(CP), 17(LM), 19(GP), 20(WE),
  !                       25(WJ), 26(WJ), AND 27(CJ)
  !      USE CA12(LP) FOR 9(LP) AND 10(WB)

  !    FROM SO VARIANT --
  !      USE SO30(MC) FOR 41(MC)

  !    FROM UT VARIANT --
  !      USE UT17(GB) FOR 21(GB)

  data MYACTS/81/

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'CROWN',5,ICYC)
  if (DEBUG) write(JOSTND,3)ICYC
  3 format(' ENTERING SUBROUTINE CROWN  CYCLE =',I5)

  !  CALL SDICAL TO LOAD THE POINT MAX SDI WEIGHTED BY SPECIES ARRAY XMAXPT()
  !  RECENT DEAD ARE NOT INCLUDED. IF THEY NEED TO BE, SDICAL.F NEEDS MODIFIED
  !  TO DO SO. IWHO PARAMETER HAS NO AFFECT ON XMAXPT ARRAY VALUES.
  !  MARK CASTLE: MAY NEED TO ADD XMAXPT ARRAY TO COMMONS

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
     9001 FORMAT(' IN CROWN 9001 WRITE ISPC,SDIAC,ORMSQD,RELSDI,ACRNEW,A,B, &
          &C,SDIDEF = ',/1X,I5,F8.2,F8.4,F8.2,F8.2,4F10.4)
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
        35 format (/' ICR(',I4,') WAS CALCULATED ELSEWHERE AND IS ',I4)
        goto 60
        40 continue
        D = DBH(I)
        H = HT(I)

        !     CALCULATE HEIGHT DIAMETER RATIO
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

        select case(ISPC)

           !  GREAT BASIN BRISTLECONE PINE FROM THE UT VARIANT

        case(21)
           HF = H + HTG(I)
           CL = -0.59373 + 0.67703 * HF
           if (CL .lt.1.0) CL = 1.0
           if (CL .gt. HF) CL = HF
           CRNEW(I) = (CL/HF)*100.
           if (DEBUG)write(JOSTND,*)' I,HF,BA,CL,CRNEW= ',&
                &I,HF,BA,CL,CRNEW(I)

           ! GS AND RW

        case(4,23)
           X = -1.021064 + 0.309296*LOG(HDR) +&
                &0.869720*PRD - 0.116274*(D/QMDPLT)
           X = 1.0/(1.0+EXP(X))

           ! CONSTRAIN CR
           if (X .lt..05) X = .05
           if (X .gt..95) X = .95

           if (DEBUG)write(JOSTND,*)' IN CROWN - RW/GS DEBUG', ' D=',D,&
                &' H=',H,' PRD=',PRD,' QMDPLT=',QMDPLT,' HDR=',HDR,' CR=',X

           ! SET CRNEW
           CRNEW(I) = X * 100

        case default

           !  CALCULATE THE PREDICTED CURRENT CROWN RATIO

           select case (ISPC)
           case(9:10,12,14:17,19:20,25:27)
              SCALE = 1.5-RELSDI
           case(1:8,11,13,18,22:24,28:40,42:43)
              SCALE = (1.0 - .00333 * (RELDEN-50.0))
           case(41)
              SCALE = (1.0 - .00167 * (RELDEN-100.0))
           end select
           if (SCALE .gt.1.0) SCALE = 1.0
           if (SCALE .lt.0.30) SCALE = 0.30
           if (DBH(I) .gt.0.0) then
              X = (real(ISORT(I)) / real(ITRN)) * SCALE
           else
              call RANN(RNUMB)
              X = RNUMB * SCALE
           end if

           ! CONSTRAIN CR
           if (X .lt..05) X = .05
           if (X .gt..95) X = .95

           ! DETERMINE CRNEW
           CRNEW(I) = A + B*((-1.0*ALOG(1-X))**(1.0/C))

           !  WRITE DEBUG INFO IF DESIRED

           if (DEBUG)write(JOSTND,9002) I,X,CRNEW(I),ICR(I)
           9002 format(/' IN CROWN 9002 WRITE I,X,CRNEW,ICR = ',&
                &I5,2F10.5,I5)
           CRNEW(I) = CRNEW(I)*10.0

        end select

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
        9004 format(/' CRMAX=',F10.2,' CRLN=',F10.2,&
             &' ICRI=',I10,' I=',I5,' CRNEW=',F10.2,' CHG=',F10.3)

        ! IF NEW CROWN EXCEEDS MAX POSSIBLE LIMIT IT TO MAX POSSIBLE

        if (ICRI.lt.10.and. CRNMLT(ISPC).eq.1.0)ICRI = INT(CRMAX+0.5)
        if (real(ICRI).gt.CRMAX) ICRI = INT(CRMAX+0.5)

        !  REDUCE CROWNS OF TREES  FLAGGED AS TOP-KILLED ON INVENTORY

        55 if (.not.LSTART .or. ITRUNC(I).eq.0) go to 59
        HN = real(NORMHT(I))/100.0
        HD = HN-real(ITRUNC(I))/100.0
        CL = (real(ICRI)/100.)*HN-HD
        ICRI = INT((CL*100./HN)+.5)
        if (DEBUG)write(JOSTND,9030)I,ITRUNC(I),NORMHT(I),HN,HD,&
             &ICRI,CL
        9030 format(/'  IN CROWN 9030 I,ITRUNC,NORMHT,HN,HD,ICRI,CL = ',&
             &3I5,2F10.3,I5,F10.3)
        go to 59

        !  CROWNS FOR TREES WITH DBH LT 1.0 IN ARE DUBBED HERE.  NO CHANGE
        !  IS CALCULATED UNTIL THE TREE ATTAINS A DBH OF 1 INCHES.

        58 continue
        if (ICR(I) .ne.0) go to 60

        select case(ISPC)

        case(21)
           HF = H + HTG(I)
           CL = -0.59373 + 0.67703 * HF
           if (CL .lt.1.0) CL = 1.0
           if (CL .gt. HF) CL = HF
           ICRI = INT((CL/HF)*100. + 0.5)
           if (DEBUG)write(JOSTND,*)' I,HF,BA,CL,CRNEW= ',&
                &I,HF,BA,CL,CRNEW(I)

        case default
           TPCT = PCT(I)
           TPCCF = PCCF(IITRE)
           call DUBSCR(ISPC,D,H,PRD,QMDPLT,CR,TPCT,TPCCF)
           if (DEBUG) write(JOSTND,*) 'RETURN FROM DUBSCR IN CROWN'
           ICRI = INT(CR*100.0+0.5)

        end select

        if (DBH(I).ge.DLOW(ISPC) .and. DBH(I).le.DHI(ISPC))&
             &ICRI = INT(real(ICRI) * CRNMLT(ISPC))

        !  BALANCING ACT BETWEEN TWO CROWN MODELS OCCURS HERE
        !  END OF CROWN RATIO CALCULATION LOOP.  BOUND CR ESTIMATE AND FILL
        !  THE ICR VECTOR.

        59 continue
        if (ICRI.gt.95) ICRI = 95
        if (ICRI.lt.10.and. CRNMLT(ISPC).eq.1.0) ICRI = 10
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
     IITRE = ITRE(I)

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

     select case(ISPC)

     case(21)
        CL = -0.59373 + 0.67703 * H
        if (CL .lt.1.0) CL = 1.0
        if (CL .gt. H) CL = H
        ICRI = INT((CL/H)*100. + 0.5)
        if (DEBUG)write(JOSTND,*)' I,H,BA,CL,ICRI= ',&
             &I,HF,BA,CL,ICRI

     case default
        TPCT = PCT(I)
        TPCCF = PCCF(IITRE)
        call DUBSCR (ISPC,D,H,PRD,QMDPLT,CR,TPCT,TPCCF)
        ICRI = INT(CR*100.0 + 0.5)
     end select

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
  9010 format(/' LEAVING CROWN 9010 FORMAT ITRN,ICR= ',I10,/,&
       &43(1H ,32I4,/))
  if (DEBUG)write(JOSTND,90)ICYC
  90 format(' LEAVING SUBROUTINE CROWN  CYCLE =',I5)
  return

  entry CRCONS

  !  ENTRY POINT FOR LOADING CROWN RATIO MODEL COEFFICIENTS

  !     SPECIES LIST FOR WESTERN SIERRAS VARIANT.
  !  1=SP,  2=DF,  3=WF,  4=GS,  5=IC,  6=JP,  7=RF,  8=PP,  9=LP, 10=WB,
  ! 11=WP, 12=PM, 13=SF, 14=KP, 15=FP, 16=CP, 17=LM, 18=MP, 19=GP, 20=WE,
  ! 21=GB, 22=BD, 23=RW, 24=MH, 25=WJ, 26=UJ, 27=CJ, 28=LO, 29=CY, 30=BL,
  ! 31=BO, 32=VO, 33=IO, 34=TO, 35=GC, 36=AS, 37=CL, 38=MA, 39=DG, 40=BM,
  ! 41=MC, 42=OS, 43=OH

  !  SURROGATE EQUATION ASSIGNMENT:

  !    FROM EXISTING WS EQUATIONS --
  !      USE 1(SP) FOR 11(WP) AND 24(MH)
  !      USE 2(DF) FOR 22(BD)
  !      USE 3(WF) FOR 13(SF)
  !      USE 4(GS) FOR 23(RW)
  !      USE 8(PP) FOR 18(MP)
  !      USE 34(TO) FOR 35(GC), 36(AS), 37(CL), 38(MA), AND 39(DG)
  !      USE 31(BO) FOR 28(LO), 29(CY), 30(BL), 32(VO), 33(IO), 40(BM), AND
  !                     43(OH)

  !    FROM CA VARIANT --
  !      USE CA11(KP) FOR 12(PM), 14(KP), 15(FP), 16(CP), 17(LM), 19(GP), 20(WE),
  !                       25(WJ), 26(WJ), AND 27(CJ)
  !      USE CA12(LP) FOR 9(LP) AND 10(WB)

  !    FROM SO VARIANT --
  !      USE SO30(MC) FOR 41(MC)

  !    FROM UT VARIANT --
  !      USE UT17(GB) FOR 21(GB)

  ! LOAD WEIBULL 'A' PARAMETER BY SPECIES

  data WEIBA/&
       &0.0,      0.0,      0.0,      0.0,      0.0,&
       &2.0,      0.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0,      0.0/

  ! LOAD WEIBULL 'B' PARAMETER EQUATION CONSTANT COEFFICIENT

  data WEIBB0/&
       &0.32957,  0.39996,  0.17606,  0.32957,  0.15500,&
       &-1.24580,  0.16601,  0.20199, -0.13121, -0.13121,&
       &0.32957,  0.16267,  0.17606,  0.16267,  0.16267,&
       &0.16267,  0.16267,  0.20199,  0.16267,  0.16267,&
       &0.0,  0.39996,  0.32957,  0.32957,  0.16267,&
       &0.16267,  0.16267, -0.14217, -0.14217, -0.14217,&
       &-0.14217, -0.14217, -0.14217, -0.14217, -0.14217,&
       &-0.14217, -0.14217, -0.14217, -0.14217, -0.14217,&
       &-0.23830, -0.09800, -0.14217/

  ! LOAD WEIBULL 'B' PARAMETER EQUATION SLOPE COEFFICIENT

  data WEIBB1/&
       &1.03916,  1.03150,  1.07984,  1.03916,  1.08747,&
       &0.94476,  1.08150,  1.07198,  1.15976,  1.15976,&
       &1.03916,  1.07340,  1.07984,  1.07340,  1.07340,&
       &1.07340,  1.07340,  1.07198,  1.07340,  1.07340,&
       &0.0,  1.03150,  1.03916,  1.03916,  1.07340,&
       &1.07340,  1.07340,  1.15448,  1.15448,  1.15448,&
       &1.15448,  1.15448,  1.15448,  1.15448,  1.15448,&
       &1.15448,  1.15448,  1.15448,  1.15448,  1.15448,&
       &1.18016,  1.11809,  1.15448/

  ! LOAD WEIBULL 'C' PARAMETER EQUATION CONSTANT COEFFICIENT

  data WEIBC0/&
       &-0.83314, -0.98287, -0.89140, -0.83314,  0.85877,&
       &-10.54490,  0.91420,  0.75409,  2.59824,  2.59824,&
       &-0.83314,  3.28850, -0.89140,  3.28850,  3.28850,&
       &3.28850,  3.28850,  0.75409,  3.28850,  3.28850,&
       &0.0, -0.98287, -0.83314, -0.83314,  3.28850,&
       &3.28850,  3.28850,  0.59185,  0.59185,  0.59185,&
       &0.59185,  0.59185,  0.59185,  0.59185,  0.59185,&
       &0.59185,  0.59185,  0.59185,  0.59185,  0.59185,&
       &3.04,  4.05181,  0.59185/

  ! LOAD WEIBULL 'C' PARAMETER EQUATION SLOPE COEFFICIENT

  data WEIBC1/&
       &0.91493,  0.88449,  0.76518,  0.91493,  0.40125,&
       &2.45822,  0.45768,  0.52191,      0.0,      0.0,&
       &0.91493,      0.0,  0.76518,      0.0,      0.0,&
       &0.0,      0.0,  0.52191,      0.0,      0.0,&
       &0.0,  0.88449,  0.91493,  0.91493,      0.0,&
       &0.0,      0.0,  0.37245,  0.37245,  0.37245,&
       &0.37245,  0.37245,  0.37245,  0.37245,  0.37245,&
       &0.37245,  0.37245,  0.37245,  0.37245,  0.37245,&
       &0.0,      0.0,  0.37245/

  ! LOAD CR=F(SDI) EQUATION CONSTANT COEFFICIENT

  data C0/&
       &7.12189,  5.91609,  6.86237,  7.12189,  6.32336,&
       &7.33055,  6.14578,  6.15172,  4.89032,  4.89032,&
       &7.12189,  6.48494,  6.86237,  6.48494,  6.48494,&
       &6.48494,  6.48494,  6.15172,  6.48494,  6.48494,&
       &0.0,  5.91609,  7.12189,  7.12189,  6.48494,&
       &6.48494,  6.48494,  4.00579,  4.00579,  4.00579,&
       &4.00579,  4.00579,  4.00579,  4.00579,  4.00579,&
       &4.00579,  4.00579,  4.00579,  4.00579,  4.00579,&
       &4.62512,  6.35669,  4.00579/

  ! LOAD CR=F(SDI) EQUATION SLOPE COEFFICIENT

  data C1/&
       &-0.02817, -0.00943, -0.03278, -0.02817, -0.02987,&
       &-0.01539, -0.02781, -0.01994, -0.01884, -0.01884,&
       &-0.02817, -0.02325, -0.03278, -0.02325, -0.02325,&
       &-0.02325, -0.02325, -0.01994, -0.02325, -0.02325,&
       &0.0, -0.00943, -0.02817, -0.02817, -0.02325,&
       &-0.02325, -0.02325, -0.00522, -0.00522, -0.00522,&
       &-0.00522, -0.00522, -0.00522, -0.00522, -0.00522,&
       &-0.00522, -0.00522, -0.00522, -0.00522, -0.00522,&
       &-0.01604, -0.00846, -0.00522/

  ! LOAD COEFFICIENTS FOR GIANT SEQUIOA (GS) CR EQUATION

  data CRNMLT/MAXSP*1.0/
  data ICFLG/MAXSP*0/
  data DLOW/MAXSP*0.0/
  data DHI/MAXSP*99.0/

  return
end
