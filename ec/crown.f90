!============================================!
!   Compute crown ratios and crown changes   !
!============================================!
! EC $Id$
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
  logical :: DEBUG
  integer :: ISORT(MAXTRE),MYACTS(1),ICFLG(MAXSP)
  integer :: I,I1,I2,I3,IACTK,ICRI,IDATE,IDT,IG,IGSP,IITRE,ISPCC,IGRP
  integer :: IULIM,ISPC
  integer :: J1,JJ,NP,NTODO
  real    :: A,ACRNEW,B,C,CHG,CL,CR,CRLN,CRMAX,D,H,HD,HN
  real    :: PDIFPY,RELSDI,RNUMB,SCALE,TPCCF,TPCT,X
  real    :: C0(MAXSP),C1(MAXSP),CRNEW(MAXTRE),CRNMLT(MAXSP),DHI(MAXSP),DLOW(MAXSP),WEIBA(MAXSP),&
       &WEIBB0(MAXSP),WEIBB1(MAXSP),WEIBC0(MAXSP),WEIBC1(MAXSP),PRM(5)

  data MYACTS/81/

  !  SPECIES LIST FOR EAST CASCADES VARIANT.

  !   1 = WESTERN WHITE PINE      (WP)    PINUS MONTICOLA
  !   2 = WESTERN LARCH           (WL)    LARIX OCCIDENTALIS
  !   3 = DOUGLAS-FIR             (DF)    PSEUDOTSUGA MENZIESII
  !   4 = PACIFIC SILVER FIR      (SF)    ABIES AMABILIS
  !   5 = WESTERN REDCEDAR        (RC)    THUJA PLICATA
  !   6 = GRAND FIR               (GF)    ABIES GRANDIS
  !   7 = LODGEPOLE PINE          (LP)    PINUS CONTORTA
  !   8 = ENGELMANN SPRUCE        (ES)    PICEA ENGELMANNII
  !   9 = SUBALPINE FIR           (AF)    ABIES LASIOCARPA
  !  10 = PONDEROSA PINE          (PP)    PINUS PONDEROSA
  !  11 = WESTERN HEMLOCK         (WH)    TSUGA HETEROPHYLLA
  !  12 = MOUNTAIN HEMLOCK        (MH)    TSUGA MERTENSIANA
  !  13 = PACIFIC YEW             (PY)    TAXUS BREVIFOLIA
  !  14 = WHITEBARK PINE          (WB)    PINUS ALBICAULIS
  !  15 = NOBLE FIR               (NF)    ABIES PROCERA
  !  16 = WHITE FIR               (WF)    ABIES CONCOLOR
  !  17 = SUBALPINE LARCH         (LL)    LARIX LYALLII
  !  18 = ALASKA CEDAR            (YC)    CALLITROPSIS NOOTKATENSIS
  !  19 = WESTERN JUNIPER         (WJ)    JUNIPERUS OCCIDENTALIS
  !  20 = BIGLEAF MAPLE           (BM)    ACER MACROPHYLLUM
  !  21 = VINE MAPLE              (VN)    ACER CIRCINATUM
  !  22 = RED ALDER               (RA)    ALNUS RUBRA
  !  23 = PAPER BIRCH             (PB)    BETULA PAPYRIFERA
  !  24 = GIANT CHINQUAPIN        (GC)    CHRYSOLEPIS CHRYSOPHYLLA
  !  25 = PACIFIC DOGWOOD         (DG)    CORNUS NUTTALLII
  !  26 = QUAKING ASPEN           (AS)    POPULUS TREMULOIDES
  !  27 = BLACK COTTONWOOD        (CW)    POPULUS BALSAMIFERA var. TRICHOCARPA
  !  28 = OREGON WHITE OAK        (WO)    QUERCUS GARRYANA
  !  29 = CHERRY AND PLUM SPECIES (PL)    PRUNUS sp.
  !  30 = WILLOW SPECIES          (WI)    SALIX sp.
  !  31 = OTHER SOFTWOODS         (OS)
  !  32 = OTHER HARDWOODS         (OH)

  !  SURROGATE EQUATION ASSIGNMENT:

  !  FROM THE EC VARIANT:
  !      USE 6(GF) FOR 16(WF)
  !      USE OLD 11(OT) FOR NEW 12(MH) AND 31(OS)

  !  FROM THE WC VARIANT:
  !      USE 19(WH) FOR 11(WH)
  !      USE 33(PY) FOR 13(PY)
  !      USE 31(WB) FOR 14(WB)
  !      USE  7(NF) FOR 15(NF)
  !      USE 30(LL) FOR 17(LL)
  !      USE  8(YC) FOR 18(YC)
  !      USE 29(WJ) FOR 19(WJ)
  !      USE 21(BM) FOR 20(BM) AND 21(VN)
  !      USE 22(RA) FOR 22(RA)
  !      USE 24(PB) FOR 23(PB)
  !      USE 25(GC) FOR 24(GC)
  !      USE 34(DG) FOR 25(DG)
  !      USE 26(AS) FOR 26(AS) AND 32(OH)
  !      USE 27(CW) FOR 27(CW)
  !      USE 28(WO) FOR 28(WO)
  !      USE 36(CH) FOR 29(PL)
  !      USE 37(WI) FOR 30(WI)

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
     9001 FORMAT(' IN CROWN 9001 WRITE ISPC,SDIAC,ORMSQD,RELSDI,ACRNEW,A,B, &
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
        35 format ('  ICR(',I4,') WAS CALCULATED ELSEWHERE AND IS ',I4)
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
        if (DEBUG)write(JOSTND,9004)CRMAX,CRLN,ICRI,I,CRNEW(I),CHG
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

        !  CROWNS FOR TREES WITH DBH LT 1.0 IN ARE DUBBED HERE.  NO CHANGE
        !  IS CALCULATED UNTIL THE TREE ATTAINS A DBH OF 1 INCHES.

        58 continue
        if (ICR(I) .ne.0) go to 60
        TPCT = PCT(I)
        TPCCF = PCCF(IITRE)
        call DUBSCR(ISPC,D,H,CR,TPCT,TPCCF)
        if (DEBUG) write(JOSTND,*) 'RETURN FROM DUBSCR IN CROWN'
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

  !  LOAD WEIBULL 'A' PARAMETER BY SPECIES

  data WEIBA/&
       &0.0,      0.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0,      1.0,      0.0,      1.0,&
       &1.0,      1.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0/

  ! LOAD WEIBULL 'B' PARAMETER EQUATION CONSTANT COEFFICIENT

  data WEIBB0/&
       &0.08106,  0.00603, -0.28295, -0.09734, -0.01129,&
       &-0.09734, -0.00047, -0.15678,  0.08247,  0.08106,&
       &0.490848, -0.01129, 0.196054, 0.196054,-0.135807,&
       &-0.09734, 0.196054,-0.811424,-0.238295,-0.818809,&
       &-0.818809,-1.112738,-0.238295,-0.238295,-0.238295,&
       &-0.238295,-0.238295,-0.238295,-0.238295,-0.238295,&
       &-0.01129,-0.238295/

  ! LOAD WEIBULL 'B' PARAMETER EQUATION SLOPE COEFFICIENT

  data WEIBB1/&
       &1.10253,  1.12276,  1.18232,  1.14675,  1.11665,&
       &1.14675,  1.13172,  1.14894,  1.10804,  1.10253,&
       &1.014138,  1.11665, 1.073909, 1.073909, 1.147712,&
       &1.14675, 1.073909, 1.056190, 1.180163, 1.054176,&
       &1.054176, 1.123138, 1.180163, 1.180163, 1.180163,&
       &1.180163, 1.180163, 1.180163, 1.180163, 1.180163,&
       &1.11665, 1.180163/

  ! LOAD WEIBULL 'C' PARAMETER EQUATION CONSTANT COEFFICIENT

  data WEIBC0/&
       &1.04477,  2.73400,  3.03400,  2.71600,  3.35500,&
       &2.71600,  2.22700,  3.05300,  1.45931,  1.04477,&
       &3.164558,  3.35500, 0.345647, 0.345647, 3.017494,&
       &2.71600, 0.345647,-3.831124, 3.044134,-2.366108,&
       &-2.366108, 2.533158, 3.044134, 3.044134, 3.044134,&
       &3.044134, 3.044134, 3.044134, 3.044134, 3.044134,&
       &3.35500, 3.044134/

  ! LOAD WEIBULL 'C' PARAMETER EQUATION SLOPE COEFFICIENT

  data WEIBC1/&
       &0.42828,      0.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0,      0.0,  0.25495,  0.42828,&
       &0.0,      0.0, 0.620145, 0.620145,      0.0,&
       &0.0, 0.620145, 1.401938,      0.0, 1.202413,&
       &1.202413,      0.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0/

  ! LOAD CR=F(SDI) EQUATION CONSTANT COEFFICIENT

  data C0/&
       &5.23986,  4.98675,  4.99727,  4.79981,  5.74915,&
       &4.79981,  3.85379,  6.04394,  6.00795,  5.23986,&
       &5.488532,  5.74915, 5.417431, 5.417431, 5.568864,&
       &4.79981, 5.417431, 5.200550, 4.625125, 4.420000,&
       &4.420000, 4.120478, 4.625125, 4.625125, 4.625125,&
       &4.625125, 4.625125, 4.625125, 4.625125, 4.625125,&
       &5.74915, 4.625125/

  ! LOAD CR=F(SDI) EQUATION SLOPE COEFFICIENT

  data C1/&
       &-0.02569, -0.02466, -0.01043, -0.00653, -0.01090,&
       &-0.00653, -0.00795, -0.01825, -0.02301, -0.02569,&
       &-0.007173, -0.01090,-0.011608,-0.011608,-0.021293,&
       &-0.00653,-0.011608,-0.014890,-0.016042,-0.010660,&
       &-0.010660,-0.006357,-0.016042,-0.016042,-0.016042,&
       &-0.016042,-0.016042,-0.016042,-0.016042,-0.016042,&
       &-0.01090,-0.016042/

  data CRNMLT/MAXSP*1.0/
  data ICFLG/MAXSP*0/
  data DLOW/MAXSP*0.0/
  data DHI/MAXSP*99.0/

  return
end
