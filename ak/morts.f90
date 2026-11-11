!============================!
!   Compute tree mortality   !
!============================!
! AK $Id$
subroutine MORTS
  implicit none

  ! VWC $Id$

  !  THIS SUBROUTINE COMPUTES PERIODIC MORTALITY RATES FOR EACH TREE
  !  RECORD AND THEN REDUCES THE NUMBER OF TREES/ACRE REPRESENTED BY THE
  !  TREE RECORD.  MORTALITY RATE FOR TREES IS PREDICTED FROM DBH
  !  AND BASAL AREA IN TREES LARGER (BAL). IF STAND GETS ABOVE MAXSDI OR BAMAX,
  !  MORTALITY IS REPEATEDLY APPLIED UNTIL STAND IS BELOW MAX. IF THIS ROUTINE
  !  IS CALLED FROM **TREGRO** WHEN CYCLING FOR GROWTH PREDICTION. ENTRY
  !  **MORCON** IS ACCESSED TO LOAD SITE DEPENDENT CONSTANTS.

  ! SPECIES LIST FOR ALASKA VARIANT.

  ! Number Code  Common Name         FIA  PLANTS Scientific Name
  !   1     SF   Pacific silver fir  011  ABAM   Abies amabilis
  !   2     AF   subalpine fir       019  ABLA   Abies lasiocarpa
  !   3     YC   Alaska cedar        042  CANO9  Callitropsis nootkatensis
  !   4     TA   tamarack            071  LALA   Larix laricina
  !   5     WS   white spruce        094  PIGL   Picea glauca
  !   6     LS   Lutz�s spruce            PILU   Picea lutzii
  !   7     BE   black spruce        095  PIMA   Picea mariana
  !   8     SS   Sitka spruce        098  PISI   Picea sitchensis
  !   9     LP   lodgepole pine      108  PICO   Pinus contorta
  !  10     RC   western redcedar    242  THPL   Thuja plicata
  !  11     WH   western hemlock     263  TSHE   Tsuga heterophylla
  !  12     MH   mountain hemlock    264  TSME   Tsuga mertensiana
  !  13     OS   other softwoods     298  2TN
  !  14     AD   alder species       350  ALNUS  Alnus species
  !  15     RA   red alder           351  ALRU2  Alnus rubra
  !  16     PB   paper birch         375  BEPA   Betula papyrifera
  !  17     AB   Alaska birch        376  BENE4  Betula neoalaskana
  !  18     BA   balsam poplar       741  POBA2  Populus balsamifera
  !  19     AS   quaking aspen       746  POTR5  Populus tremuloides
  !  20     CW   black cottonwood    747  POBAT  Populus trichocarpa
  !  21     WI   willow species      920  SALIX  Salix species
  !  22     SU   Scouler�s willow    928  SASC   Salix scouleriana
  !  23     OH   other hardwoods     998  2TB

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.F77'
  include 'PLOT.F77'
  include 'CALCOM.F77'
  include 'CONTRL.F77'
  include 'COEFFS.F77'
  include 'ESTREE.F77'
  include 'MULTCM.F77'
  include 'PDEN.F77'
  include 'WORKCM.F77'
  include 'VARCOM.F77'

  !  DEFINITIONS:
  !      BM1 --  ARRAY CONTAINING INTERCEPT COEFFICIENTS FOR SURVIVAL EQUATION
  !      BM2 --  ARRAY CONTAINING DBH COEFFICIENTS FOR SURVIVAL EQUATION
  !      BM3 --  ARRAY CONTAINING DBH^2 COEFFICIENTS FOR SURVIVAL EQUATION
  !      BM4 --  ARRAY CONTAINING BAL COEFFICIENTS FOR SURVIVAL EQUATION
  !      BM5 --  ARRAY CONTAINING BAL/LN(DBH + 1) COEFFICIENTS FOR SURVIVAL EQUATION
  !    DSURV --  MINIMUM DIAMETER LIMITS USED FOR SURVIVAL CALCULATIONS
  !        D --  TREE DIAMETER
  !    PTBAL --  POINT BASAL AREA
  !      RIP --  ESTIMATED ANNUAL MORTALITY RATE
  !        P --  NUMBER OF TREES PER ACRE REPRESENTED BY A TREE.
  !      WKI --  SCALAR USED FOR CALCULATION AND TEMPORARY STORAGE
  !              OF TREE MORTALITY (TREES/ACRE).

  ! Variables
  logical :: DEBUG,LINCL
  real    :: PRM(6)
  real    :: BM1(MAXSP),BM2(MAXSP),BM3(MAXSP),BM4(MAXSP),BM5(MAXSP)
  real    :: CREDIT,TEMP,XMORE,VLOS,PRES,XCHECK,X,RIP
  real    :: WKI,D1,D2,XMORT
  real    :: DQ10,G,BRATIO,BARK,CIOBDS,D,P,SD2SQ,T,SUMDR10
  real    :: WKIT2,G1(MAXTRE)
  real    :: BAA,TA,SD2SQA,SUMDR10A,DQ10A,SDIA,CIOBDS1(MAXTRE)
  real    :: WKII(MAXTRE),PTBAL
  integer :: MYACTS(2),PASS
  integer :: NTODO,I,NP,IACTK,IDATE,ISPCC,IS,ISPC,I1,I2,I3,IP
  integer :: IDMFLG,ITODO,KPOINT,KBIG,IGRP,IULIM,IG,IX,J
  real    :: DSURV(MAXSP),DTEMP,SDIUPR

  ! Data statements
  data MYACTS/94,97/

  !     SURVIVAL INTERCEPT COEFFICIENTS
  data BM1/ &
       & 5.106086, 5.106086, 5.334283, 5.129246, 4.585373, &
       & 4.585373, 5.129246, 5.106086, 4.935148, 6.090056, &
       & 5.120483, 5.440943, 4.585373, 2.651991, 2.987803, &
       & 3.255821, 3.255821, 2.651991, 2.833541, 2.651991, &
       & 2.651991, 2.651991, 2.651991/

  !     SURVIVAL DBH COEFFICIENTS
  data BM2/ &
       & 0.036136, 0.036136, 0.036136, 0.402707, 0.402707, &
       & 0.402707, 0.402707, 0.036136, 0.036136, 0.036136, &
       & 0.036136, 0.036136, 0.402707, 0.402707, 0.402707, &
       & 0.402707, 0.402707, 0.402707, 0.402707, 0.402707, &
       & 0.402207, 0.402207, 0.402207/

  !     SURVIVAL DBH^2 COEFFICIENTS
  data BM3/ &
       & -0.000634, -0.000634, -0.001034, -0.051774, -0.020537, &
       & -0.020537, -0.051774, -0.000634, -0.000850, -0.000745, &
       & -0.00105,  -0.000942, -0.020537, -0.011288, -0.014038, &
       & -0.018658, -0.018658, -0.011288, -0.015719, -0.011288, &
       & -0.011288, -0.011288, -0.011288/

  !     SURVIVAL BAL COEFFICIENTS
  !     NOT USED (ZERO VALUE) FOR SF, AF, YC, SS, LP, RC, WH, MH
  data BM4/ &
       & 0.0,       0.0,       0.0,      -0.004365, -0.004414, &
       & -0.004414, -0.004365,  0.0,       0.0,       0.0, &
       & 0.0,       0.0,      -0.004414, -0.002557, -0.005462, &
       & -0.007307, -0.007307, -0.002557, -0.010755, -0.002557, &
       & -0.002557, -0.002557, -0.002557/

  !     SURVIVAL BAL/DBH COEFFICIENTS
  !     NOT USED (ZERO VALUE) FOR TA, WS, LS, BE, OS, AD, RA, PB, AB,
  !     BA, AS, BC, WI, SU, OH
  data BM5/ &
       & -0.010747, -0.010747, -0.004540,  0.0,       0.0, &
       & 0.0,       0.0,      -0.010747, -0.008488, -0.005608, &
       & -0.005131, -0.005375,  0.0,       0.0,       0.0, &
       & 0.0,       0.0,       0.0,       0.0,       0.0, &
       & 0.0,       0.0,       0.0/

  !     MINIMUM DIAMETER USED FOR SURVIVAL CALCULATIONS
  data DSURV/ &
       & 0.50, 0.50, 0.50, 0.10, 0.10, 0.10, 0.10, 0.50, 0.50, 0.50, &
       & 0.50, 0.50, 0.10, 0.10, 0.10, 0.10, 0.10, 0.10, 0.10, 0.10, &
       & 0.10, 0.10, 0.10/

  WKIT2 = 0.0
  do I = 1,6
     PRM(I) = 0.0
  end do

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'MORTS',5,ICYC)

  !  IF THERE ARE NO TREE RECORDS, SIMPLY PROCESS THE MORTALITY
  !  MULTIPLIERS AND BRANCH TO END (FOR THE METHOD USED TO GET
  !  MULTIPLIERS, SEE SUBROUTINE MULTS).

  call OPFIND (1,MYACTS(1),NTODO)
  if (NTODO.gt.0) then
     do I = 1,NTODO
        call OPGET(I,4,IDATE,IACTK,NP,PRM)
        if (IACTK.lt.0) goto 9
        call OPDONE(I,IY(ICYC))
        ISPCC = IFIX(PRM(1))
        if (ISPCC.eq.0) goto 2
        XMMULT(ISPCC) = PRM(2)
        XMDIA1(ISPCC) = PRM(3)
        XMDIA2(ISPCC) = PRM(4)
        goto 9
        2 continue
        do ISPCC = 1,MAXSP
           XMMULT(ISPCC) = PRM(2)
           XMDIA1(ISPCC) = PRM(3)
           XMDIA2(ISPCC) = PRM(4)
        end do
        9 continue
     end do
  end if

  ! IF BARE GROUND PLANT, LIMITS WERE NOT ADJUSTED FROM A PERCENT TO A
  ! PROPORTION IN CRATET, ADJUST THEM HERE.

  if (PMSDIL .gt. 1.0)PMSDIL = PMSDIL/100.
  if (PMSDIU .gt. 1.0)PMSDIU = PMSDIU/100.

  if (ITRN.le.0) goto 100

  ! SDIMAX IS USED HERE TO CARRY WEIGHTED SDI MAXIMUM. IF A USER-DEFINED
  ! BASAL AREA MAXIMUM HAS NOT BEEN ENTERED, THEN BASAL AREA MAXIMUM
  ! WILL BE RESET TO BE CONSISTENT WITH THE SDI MAXIMUM. THIS IS NEEDED
  ! FOR MODELING CLIMATE CHANGE.

  call SDICAL(0,SDIMAX)
  if (DEBUG)write(JOSTND,*)'IN MORTS CYCLE= ',ICYC,'  BAMAX= ', &
       & BAMAX,'  SDIMAX= ',SDIMAX

  ! ADJUST SDIMAX BASED ON PMSDIU. SDIUPR VARIABLE WILL LATER BE USED
  ! TO DETERMINE NUMBER OF PASSES IN MORTALITY PROCESSING.

  SDIUPR = SDIMAX * PMSDIU
  if (DEBUG)write(JOSTND,*)'IN MORTS CYCLE= ',ICYC,' SDIMAX= ', &
       & SDIMAX,' PMSDIU=',PMSDIU,' SDIUPR=',SDIUPR

  !  ESTIMATE QUADRATIC MEAN DIAMETER 10 YEARS HENCE.

  T = 0.0
  SD2SQ = 0.0
  SUMDR10 = 0.0
  DQ10 = 0.0
  do I = 1,ITRN
     P = PROB(I)
     IS = ISP(I)
     D = DBH(I)
     BARK = BRATIO(IS,D,HT(I))
     G = DG(I)/BARK
     G1(I) = G
     if (LZEIDE.and.(D.lt.DBHZEIDE)) goto 20 ! BRANCH IF D IS LT MIN DBH
     if (.not.LZEIDE.and.(D.lt.DBHSTAGE)) goto 20
     CIOBDS = (2.0*D*G+G*G)
     CIOBDS1(I) = CIOBDS
     SD2SQ = SD2SQ+P*(D*D+CIOBDS)
     if (LZEIDE) then
        SUMDR10 = SUMDR10+P*(D+G)**1.605
     end if
     T = T+P
     20 continue
  end do
  if (T.eq. 0.0) T = 0.0001
  DQ10 = SQRT(SD2SQ/T)
  if (LZEIDE) then
     DQ10 = (SUMDR10/T)**(1/1.605)
  end if
  if (DEBUG)write(JOSTND,*)'SD2SQ,SUMDR10,DQ10= ', &
       & SD2SQ,SUMDR10,DQ10

  !  START LOOP TO ESTIMATE MORTALITY RATE. TREES ARE PROCESSED
  !  ONE AT A TIME WITHIN A SPECIES.

  !  START SPECIES LOOP
  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (I1.le.0) goto 50
     I2 = ISCT(ISPC,2)
     XMORT = XMMULT(ISPC)
     D1 = XMDIA1(ISPC)
     D2 = XMDIA2(ISPC)

     !  START TREE LOOP WITHIN SPECIES.

     TA = 0.0
     SD2SQA = 0.0
     SUMDR10A = 0.0
     PASS = 1
     do I3 = I1,I2

        !  INITIALIZE FOR NEXT TREE.

        I = IND1(I3)
        P = PROB(I)
        RIP = 0.0
        WKI = 0.0
        WK2(I) = 0.0
        D = DBH(I)
        BARK = BRATIO(ISPC,D,HT(I))
        PTBAL = PTBALT(I)
        if (P.le.0.0) goto 40

        !  COMPUTE SURVIVAL RATE

        !  INDIVIDUAL TREE PROBABLITY OF SURVIVAL (ANNUAL) IS CALCULATED USING A MODIFIED
        !  LOGISTIC EQUATION FORM:

        !  PR(SURV) = EXP(X)/(1 + EXP(X))
        !  X = B1 + B2*DBH + B3*DBH^2 + B4*BAL + B5*BAL/DBH

        !  WHERE
        !  PR(SURV): IS ANNUAL PROBABILITY OF SURVIVAL
        !  DBH: DIAMETER AT BREAST HEIGHT
        !  BAL: POINT BASAL AREA IN TREES LARGER THAN SUBJECT TREE

        ! DETERMINE DIAMETER TO USE FOR SURVIVAL CALCULATION
        ! VALUE IN DSURV ARRAY IS USED WHEN DBH OF RECORD IS BELOW THE DSURV VALUE.
        ! THESE VALUES ARE USED TO AVOID MASS SEEDLING MORTALITY.
        DTEMP = D
        if (DTEMP .lt. DSURV(ISPC)) DTEMP = DSURV(ISPC)

        ! CALCULATE SURVIVAL
        RIP = BM1(ISPC) + BM2(ISPC)*DTEMP + BM3(ISPC)*DTEMP*DTEMP + &
             & BM4(ISPC)*PTBAL + BM5(ISPC)*PTBAL/DTEMP

        RIP = EXP(RIP)/(1 + EXP(RIP))

        ! DETERMINE IF DEBUG IS NEEDED
        if (DEBUG)write(JOSTND,*)'IN MORTS',' I=',I,' ISPC=', ISPC, &
             & ' D=',D,' DTEMP=',DTEMP,' PTBAL=',PTBAL,' RIP=',RIP

        !  APPLY MORTALITY MULTIPLIERS WITHIN USER-PROVIDED DBH LIMITS
        !  AND STORE TREES PER ACRE DYING IN THE ARRAY WK2.

        X = 1.0
        if (D.ge.D1 .and. D.lt.D2) X = XMORT

        !  FROM HERE TO STATEMENT 29 IS ESTABLISHMENT MODEL CODE.
        !  "BEST" TREES NOT SUBJECT TO MORTALITY FOR 20 YRS
        !  AFTER DISTURBANCE DATE.

        if (IESTAT(I).le.0) goto 29
        if (IY(ICYC).ge.IESTAT(I)) IESTAT(I) = 0
        XCHECK = FLOAT(IESTAT(I)-IY(ICYC))/FINT
        if (XCHECK.gt.1.0) XCHECK = 1.0
        if (XCHECK.lt.0.0) XCHECK = 0.0
        X = X* (1.0-XCHECK)
        29 continue

        ! CONSTRAIN SURVIVAL PREDICTION
        if (RIP.gt.0.99999) RIP = 0.99999
        if (RIP.lt.0.001) RIP = 0.0

        ! PROBABILITY OF SURVIVAL IS CONVERTED TO PROBABILITY OF MORTALITY ADJUSTED BY
        ! CYCLE LENGTH THROUGH THE FOLLOWING: 1 - PR(SURV)**FINT
        WKI = P*(1.0 - (RIP**FINT))*X

        ! DETERMINE IF DEBUG IS NEEDED
        if (DEBUG) &
             & write(JOSTND,*)'IN MORTS EQ I,X,P,RIP,FINT,WKI= ', &
             & I,X,P,RIP,FINT,WKI

        ! DETERMINE IF PREDICTED MORTALITY IS GREATER THAN SIZE CAP IMPOSED
        ! MORTALITY RATE
        BARK = BRATIO(IS,D,HT(I))
        G = (DG(I)/BARK) * (FINT/10.0)
        IDMFLG = IFIX(SIZCAP(ISPC,3))
        if ((D+G).ge.SIZCAP(ISPC,1) .and. IDMFLG.ne.1) then
           WKI = AMAX1(WKI,(P*SIZCAP(ISPC,2)*FINT/10.0))
           if (DEBUG)write(JOSTND,*)'SIZE CAP RESTRICTION IMPOSED, ', &
                & 'I,ISPC,D,P,SIZCAP 1-3,WKI = ', &
                & I,ISPC,D,P,SIZCAP(ISPC,1),SIZCAP(ISPC,2),SIZCAP(ISPC,3),WKI
        end if
        if (WKI.gt.P) WKI = P

        !  IF SDIMAX IS LESS THAN 5, ASSUME CLIMATE HAS CHANGED ENOUGH THAT THE
        !  SITE WILL NO LONGER SUPPORT TREES, AND KILL ALL EXISTING TREES.

        if (SDIMAX .lt. 5) then
           WKI = P
        end if

        !  RECALCULATE VALUES AFTER MORTALITY HAS BEEN APPLIED

        TA = TA+(PROB(I)-WKI)
        WK2(I) = WKI
        if (DEBUG)write(JOSTND,*)'IN MORTS INITIAL PASS: IXX WKI WK2=', &
             & I,WKI,WK2(I)
        40 continue
     end do
     50 continue
  end do
  goto 59

  !  RECALCULATE SDI AND BA FOR THE STAND AND MAKE SURE THEY ARE BELOW THE MAXIMUM

  55 continue
  DQ10A = SQRT(SD2SQA/TA)
  BAA = .005454154*DQ10A*DQ10A*TA
  if (LZEIDE) then
     DQ10A = (SUMDR10A/TA)**(1./1.605)
  end if
  SDIA = TA*(DQ10A/10)**1.605
  if (DEBUG)write(JOSTND,*)'IN MORTS:PASS,TA,SD2SQA,SUMDR10A,DQ10A', &
       & ',BAA,SDIA=',PASS,TA,SD2SQA,SUMDR10A,DQ10A,BAA,SDIA

  !  LOOP BACK THROUGH TREES RECORDS IF STAND IS OVER MAXIMUM SDI.
  !  BECAUSE BA CAN CREEP UP TO UNREALISTIC VALUES EVEN WHILE
  !  HOLDING SDI BELOW MAXIMUM, ANOTHER PASS IS MADE IF A STAND'S
  !  BASAL AREA EXCEEDS THE BASAL AREA MAXIMUM OF THE STAND

  if (DEBUG)write(JOSTND,*)' IN MORTS',' SDIA=',SDIA,' SDIUPR=', &
       & SDIUPR,' BAA=',BAA,' BAMAX=',BAMAX,' PASS=',PASS
  if (((SDIA.lt.SDIUPR).and.(BAA.lt.BAMAX)).or.(PASS.gt.100)) goto 70
  PASS = PASS+1
  SD2SQA = 0
  SUMDR10A = 0
  TA = 0
  59 continue
  do I = 1,ITRN
     WKI = WK2(I)*(PASS)
     if (WKI.gt.PROB(I)) WKI = PROB(I)
     WKIT2 = WKI+WKIT2
     WKII(I) = WKI
     if (LZEIDE.and.(DBH(I).lt.DBHZEIDE)) goto 60 ! BRANCH IF D IS LT MIN DBH
     if (.not.LZEIDE.and.(DBH(I).lt.DBHSTAGE)) goto 60
     SD2SQA = SD2SQA+(PROB(I)-WKI)*(DBH(I)*DBH(I)+CIOBDS1(I))
     if (LZEIDE) then
        SUMDR10A = SUMDR10A+(PROB(I)-WKI)*(DBH(I)+G1(I))**1.605
     end if
     TA = TA+(PROB(I)-WKI)
     IF (DEBUG) WRITE(JOSTND,*)'IN MORT PASS,I,DBH,WKI,WKIT2,SD2SQA,   &
          &  SUMDR10A,TA=',PASS,I,DBH(I),WKI,WKIT2,SD2SQA,SUMDR10A,TA
     60 continue
  end do
  goto 55

  !  END OF TREE LOOPING.  PRINT DEBUG INFO IF DESIRED.

  70 continue
  do I = 1,ITRN
     if (PASS.gt.1) WK2(I) = WKII(I)
     if (WK2(I).gt.PROB(I)) WK2(I) = PROB(I)
     if (DEBUG) then
        PRES = PROB(I)-WK2(I)
        VLOS = WK2(I)*CFV(I)/FINT
        write(JOSTND,9000) I,ISP(I),DBH(I),G1(I),BA,PROB(I), &
             & WK2(I),PRES,VLOS
        9000 format('IN MORTS, I=',I4,',  ISPC=',I3,',  DBH=',F7.2, &
             & ',  DBH INCREMENT=',F7.4,',  BA=',F7.2, &
             & ',  INIT PROB=',F9.3/ &
             & ' TREES DYING=',F9.3,'  RES PROB=',F9.3, &
             & ',  VOL LOST=',F9.3)
     end if
  end do
  if (DEBUG) write(JOSTND,*) 'IN MORTS,  ISPC=', ISPC
  100 continue

  !  COMPUTE THE CLIMATE-PREDICTED MORTALITY RATES BY SPECIES

  call CLMORTS

  !  BEGIN FIXMORT ACTIVITY PROCESS IF SCHEDULED
  !  COMPUTE THE FIXMORT OPTION.  LOOP OVER ALL SCHEDULED FIXMORT'S
  !  LINCL IS USED TO INDICATE WHETHER A TREE GETS AFFECTED OR NOT

  call OPFIND (1,MYACTS(2),NTODO)
  if (NTODO.gt.0) then
     if (DEBUG) write(JOSTND,*)'FIXMORT PROCESSING, ITODO= ',ITODO
     do ITODO = 1,NTODO
        call OPGET (ITODO,6,IDATE,IACTK,NP,PRM)
        if (IACTK.lt.0) goto 300
        call OPDONE(ITODO,IY(ICYC))
        ISPCC = IFIX(PRM(1))
        if (NP .le. 4) then
           if (PRM(2).gt. 1.0) PRM(2) = 1.0
        end if
        if (PRM(3).lt. 0.0) PRM(3) = 0.0
        if (PRM(4).le. 0.0) PRM(4) = 999.
        IP = 1
        if (NP.gt.4) then
           if (PRM(5).lt.3.) then
              if (PRM(2).gt. 1.0) PRM(2) = 1.0
              if (PRM(2).lt. 0.0) PRM(2) = 0.0
           end if
           if (PRM(5).eq.1.0) then
              IP = 2
           else if (PRM(5).eq.2.0) then
              IP = 3
           else if (PRM(5).eq.3.) then
              IP = 4
           end if
        end if

        !  SET FLAG FOR POINT MORTALITY, OR KILLING FROM ABOVE
        !    PRM(6)    POINT      SIZE   KBIG     KILL DIRECTION
        !      0         NO        NO     0                       DEFAULT CONDITION
        !      1         YES       NO     0
        !     10         NO        YES    1       BOTTOM UP
        !     11         YES       YES    1       BOTTOM UP
        !     20         NO        YES    2       TOP DOWN
        !     21         YES       YES    2       TOP DOWN

        KPOINT = 0
        KBIG = 0
        if (PRM(6).gt.0.) then
           if (PRM(6) .eq. 1) then
              KPOINT = 1
           else if (PRM(6) .eq. 10) then
              KBIG = 1
           else if (PRM(6) .eq. 11) then
              KPOINT = 1
              KBIG = 1
           else if (PRM(6) .eq. 20) then
              KBIG = 2
           else if (PRM(6) .eq. 21) then
              KPOINT = 1
              KBIG = 2
           end if
        end if
     end do
  end if

  ! END OF FIXMORT ACTIVITY PROCESS

  if (ITRN.gt.0 .and. (KBIG .ne.0 .or. KPOINT.ne. 0)) then

     ! IF CONCENTRATING MORTALITY ON A POINT, AND/OR BY SIZE TREES IS IN
     ! EFFECT, DETERMINE EFFECT OF THIS FIXMORT AND REALLOCATE BY POINT:
     !   REALLOCATE ALL MORTALITY IF REPLACE OPTION OR MULTIPLY OPTION
     !   ARE IN EFFECT.
     !   ONLY REALLOCATE ADDITIONAL MORTALITY IF "ADD" OPTION IS IN EFFECT
     !   ONLY REALLOCATE ADDITIONAL MORTALITY IF "MAX" OPTION IS IN EFFECT
     !   (I.E. MORTALITY OVER AND ABOVE WHAT WAS PREVIOUSLY PREDICTED.

     if (KBIG.ge.1 .or. (KPOINT.eq.1 .and. IPTINV.gt.1)) then
        XMORE = 0.0
        do I = 1,ITRN
           LINCL = .false.
           if (ISPCC.eq.0 .or. ISPCC.eq.ISP(I)) then
              LINCL = .true.
           else if (ISPCC.lt.0) then
              IGRP = -ISPCC
              IULIM = ISPGRP(IGRP,1)+1
              do IG = 2,IULIM
                 if (ISP(I) .eq. ISPGRP(IGRP,IG)) then
                    LINCL = .true.
                    goto 91
                 end if
              end do
           end if
           91 continue
           if (LINCL .and. &
                & (PRM(3).le.DBH(I) .and. DBH(I).lt.PRM(4))) then

              select case (IP)
              case (1)
                 XMORE = XMORE+PROB(I)*PRM(2)
                 WK2(I) = 0.
              case (2)
                 XMORE = XMORE+(AMAX1(0.0,PROB(I)-WK2(I))*PRM(2))
              case (3)
                 TEMP = AMAX1(WK2(I),(PROB(I)*PRM(2)))
                 if (TEMP .gt. WK2(I)) then
                    XMORE = XMORE+TEMP-WK2(I)
                 end if
              case (4)
                 XMORE = XMORE+WK2(I)*PRM(2)
                 WK2(I) = 0.
              end select
           end if

        end do
        if (DEBUG) write(JOSTND,*)'KPOINT,KBIG,ITRN,XMORE= ', &
             & KPOINT,KBIG,ITRN,XMORE
        CREDIT = 0.
        do I = 1,ITRN
           IWORK1(I) = IND1(I)
           if (KBIG .eq. 1) then
              WORK3(I) = (-1.0)* &
                   & (DBH(I)+DG(I)/BRATIO(ISP(I),DBH(I),HT(I)))
           else
              WORK3(I) = DBH(I)+DG(I)/BRATIO(ISP(I),DBH(I),HT(I))
           end if
        end do
        call RDPSRT(ITRN,WORK3,IWORK1,.false.)
        if (DEBUG) then
           write(JOSTND,*)'DBH= ',(DBH(IG),IG = 1,ITRN)
           write(JOSTND,*)'IWORK1= ',(IWORK1(IG),IG = 1,ITRN)
           write(JOSTND,*)'WK2= ',(WK2(IG),IG = 1,ITRN)
        end if

        if (KBIG.ge.1 .and. KPOINT.eq.0) then

           !           CONCENTRATION BY SIZE ONLY

           do I = 1,ITRN
              IX = IWORK1(I)
              LINCL = .false.
              if (ISPCC.eq.0 .or. ISPCC.eq.ISP(IX)) then
                 LINCL = .true.
              else if (ISPCC.lt.0) then
                 IGRP = -ISPCC
                 IULIM = ISPGRP(IGRP,1)+1
                 do IG = 2,IULIM
                    if (ISP(IX) .eq. ISPGRP(IGRP,IG)) then
                       LINCL = .true.
                       goto 93
                    end if
                 end do
              end if
              93 continue
              if (LINCL .and. &
                   & (PRM(3).le.DBH(IX) .and. DBH(IX).lt.PRM(4))) then
                 TEMP = CREDIT+PROB(IX)-WK2(IX)
                 if ((TEMP .le. XMORE).or. &
                      & (ABS(TEMP-XMORE).lt.0.0001)) then
                    CREDIT = CREDIT+PROB(IX)-WK2(IX)
                    WK2(IX) = PROB(IX)
                 else
                    WK2(IX) = WK2(IX)+XMORE-CREDIT
                    CREDIT = XMORE
                    goto 295
                 end if
              end if
           end do
           goto 295

        else if (KPOINT.eq.1 .and. KBIG.eq.0) then

           !           CONCENTRATION ON POINTS ONLY

           do J = 1,IPTINV
              do I = 1,ITRN
                 if (ITRE(I) .ne. J) goto 204
                 LINCL = .false.
                 if (ISPCC.eq.0 .or. ISPCC.eq.ISP(I)) then
                    LINCL = .true.
                 else if (ISPCC.lt.0) then
                    IGRP = -ISPCC
                    IULIM = ISPGRP(IGRP,1)+1
                    do IG = 2,IULIM
                       if (ISP(I) .eq. ISPGRP(IGRP,IG)) then
                          LINCL = .true.
                          goto 95
                       end if
                    end do
                 end if
                 95 continue
                 if (LINCL .and. &
                      & (PRM(3).le.DBH(I) .and. DBH(I).lt.PRM(4))) then
                    TEMP = CREDIT+PROB(I)-WK2(I)
                    if ((TEMP .le. XMORE).or. &
                         & (ABS(TEMP-XMORE).lt.0.0001)) then
                       CREDIT = CREDIT+PROB(I)-WK2(I)
                       WK2(I) = PROB(I)
                    else
                       WK2(I) = WK2(I)+XMORE-CREDIT
                       CREDIT = XMORE
                       goto 295
                    end if
                 end if
                 204 continue
              end do
           end do
           goto 295

        else

           !           CONCENTRATION BY SIZE ON POINTS (POINTS HAVE PRIORITY, SO TREES
           !           WILL BE KILLED BY SIZE ON ONE POINT BEFORE MOVING TO THE NEXT
           !           POINT TO START WITH THE BIGGEST/SMALLEST TREES ON THAT POINT.

           do J = 1,IPTINV
              do I = 1,ITRN
                 IX = IWORK1(I)
                 if (ITRE(IX) .ne. J) goto 311
                 LINCL = .false.
                 if (ISPCC.eq.0 .or. ISPCC.eq.ISP(IX)) then
                    LINCL = .true.
                 else if (ISPCC.lt.0) then
                    IGRP = -ISPCC
                    IULIM = ISPGRP(IGRP,1)+1
                    do IG = 2,IULIM
                       if (ISP(IX) .eq. ISPGRP(IGRP,IG)) then
                          LINCL = .true.
                          goto 97
                       end if
                    end do
                    97 continue
                 end if
                 if (LINCL .and. &
                      & (PRM(3).le.DBH(IX) .and. DBH(IX).lt.PRM(4))) then
                    TEMP = CREDIT+PROB(IX)-WK2(IX)
                    if ((TEMP .le. XMORE).or. &
                         & (ABS(TEMP-XMORE).lt.0.0001)) then
                       CREDIT = CREDIT+PROB(IX)-WK2(IX)
                       WK2(IX) = PROB(IX)
                    else
                       WK2(IX) = WK2(IX)+XMORE-CREDIT
                       CREDIT = XMORE
                       goto 295
                    end if
                 end if
                 311 continue
              end do
           end do
           goto 295
        end if

     end if
  else if (ITRN .gt. 0 .and. KBIG .eq. 0 .and. KPOINT .eq. 0) then

     !  NORMAL FIXMORT PROCESSING WHEN POINT OR SIZE CONCENTRATION
     !  IS NOT IN EFFECT.

     do I = 1,ITRN
        LINCL = .false.
        if (ISPCC.eq.0 .or. ISPCC.eq.ISP(I)) then
           LINCL = .true.
        else if (ISPCC.lt.0) then
           IGRP = -ISPCC
           IULIM = ISPGRP(IGRP,1)+1
           do IG = 2,IULIM
              if (ISP(I) .eq. ISPGRP(IGRP,IG)) then
                 LINCL = .true.
                 goto 99
              end if
           end do
        end if
        99 continue
        if (LINCL .and. &
             & (PRM(3).le.DBH(I) .and. DBH(I).lt.PRM(4))) then

           select case (IP)
           case (1)
              WK2(I) = PROB(I)*PRM(2)
           case (2)
              WK2(I) = WK2(I)+(AMAX1(0.0,PROB(I)-WK2(I))*PRM(2))
           case (3)
              WK2(I) = AMAX1(WK2(I),(PROB(I)*PRM(2)))
           case (4)
              WK2(I) = AMIN1(PROB(I),WK2(I)*PRM(2))
           end select
        end if
     end do
  end if
  295 continue
  300 continue
  if (DEBUG)write(JOSTND,*) &
       & 'ITODO,WK2= ',ITODO,(WK2(IG),IG = 1,ITRN)

  return

  entry MORCON
  return
end subroutine MORTS
