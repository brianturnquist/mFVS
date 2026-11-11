!============================!
!   Compute tree mortality   !
!============================!
! OP $Id$
subroutine MORTS
  implicit none

  !  THIS SUBROUTINE COMPUTES PERIODIC MORTALITY RATES FOR
  !  EACH TREE RECORD AND THEN REDUCES THE NUMBER OF TREES/ACRE
  !  REPRESENTED BY THE TREE RECORD.
  !  THIS ROUTINE IS CALLED FROM **GRINCR** WHEN CYCLING FOR GROWTH
  !  PREDICTION.  ENTRY **MORCON** IS CALLED TO LOAD SITE DEPENDENT
  !  CONSTANTS.

  !  IF THERE ARE SOME VALID ORGANON TREES IN THE STAND, THEN
  !  ORGANON MORTALITY RATES FOR EACH TREE RECORD (REGARDLESS OF IORG(i)
  !  VALUE) WERE LOADED IN THE CALL TO **EXECUTE** FROM **DGDRIV**.
  !  USE ORGANON RATES FOR ALL TREES; APPLY USER ADJUSTMENTS.

  !  IF THERE ARE NO VALID ORGANON TREES, THEN ORGANON MORTALITY RATES HAVE
  !  NOT BEEN CALCULATED (**EXECUTE** WAS NOT CALLED); USE FVS MORTALITY LOGIC

  !  SF , WF, GF, AF, RF,SS, NF, ES:     USE GF FROM NW ORGANON
  !  YC , IC:                            USE IC FROM SW ORGANON
  !  LP, JP, SP, WP, PP, WJ, LL, WB, KP: USE PP FROM SW ORGANON
  !  DF, OT:                             USE DF FROM NW ORGANON
  !  WH, MH, RC:                         USE WH FROM NW ORGANON
  !  BM:                                 USE BM FROM NW ORGANON
  !  RA, WA, PB, AS, CW, WI:             USE RA FROM NW ORGANON
  !  GC:                                 USE GC FROM SW ORGANON
  !  WO:                                 GOULD&HARRINGTON
  !  PY:                                 USE PY FROM SW ORGANON
  !  DG, HT, CH:                         USE DG FROM NW ORGANON
  !  RW                                  USE CASTLE 2021 EQUATION

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'
  include 'CALCOM.f90'
  include 'CONTRL.f90'
  include 'COEFFS.f90'
  include 'ESTREE.f90'
  include 'MULTCM.f90'
  include 'PDEN.f90'
  include 'WORKCM.f90'
  include 'VARCOM.f90'
  include 'ORGANON.f90'

  !  DEFINITIONS:

  !     PMSC -- CONSTANT TERMS FOR EACH SPECIES FOR THE MORTALITY
  !             RATE EQUATION.
  !        I -- TREE SUBSCRIPT.
  !        D -- TREE DIAMETER.
  !      RIP -- ESTIMATED ANNUAL MORTALITY RATE BASED ON ORGANON EQNS
  !             MODEL.
  !        P -- NUMBER OF TREES PER ACRE REPRESENTED BY A TREE.
  !      WKI -- SCALAR USED FOR CALCULATION AND TEMPORARY STORAGE
  !             OF TREE MORTALITY (TREES/ACRE).
  !    MRTEQ -- MORTALITY EQUATION NUMBER

  ! Variables
  logical :: DEBUG,LINCL
  real    :: PMSC(MAXSP),PRM(6),PSP(MAXSP),BM0(MAXSP)
  real    :: BM1(MAXSP),BM2(MAXSP),BM3(MAXSP),BM4(MAXSP),BM5(MAXSP)
  integer :: MYACTS(2),MORTMAP(39),PASS,MCLASS(MAXSP),ACLASS
  real    :: CREDIT,TEMP,XMORE,VLOS,PRES,XCHECK,X,RIP
  real    :: DGT,RELDBH,WKI,D1,D2,XMORT,AVED,WPROB,DSUM
  real    :: DQ10,G,BRATIO,BARK,CIOBDS,D,P,SD2SQ,T,CRADJ,SUMDR10
  real    :: RELHT,XSITE1,XSITE2,BAL,CR,WKIT2,RELDBH1(MAXTRE),G1(MAXTRE)
  real    :: BAA,TA,SD2SQA,SUMDR10A,DQ10A,SDIA,CIOBDS1(MAXTRE)
  real    :: WKII(MAXTRE),MVALUES(5),AVALUE,ALPHA(3),BETA(MAXSP)
  real    :: DBHA,HBH,PTBAL
  real    :: SMORMT,RBM0,RBM1,RBM2,RBM3,RBM4,RBM5
  integer :: NTODO,I,NP,IACTK,IDATE,ISPCC,IS,ISPC,I1,I2,I3,IP
  integer :: IDMFLG,ITODO,KPOINT,KBIG,IGRP,IULIM,IG,IX,J
  integer :: IMRTMP

  ! Data statements
  data ALPHA/ -4.4384, 0.0053, -0.6001/
  data MVALUES/ 1.00, 1.500, 2.250, 3.375, 5.062/
  data MCLASS/ 1, 2, 2, 2, 2, 2, 3, 2, 3, 2, 4, 4, 3,&
       &3, 4, 3, 1, 1, 1, 1, 1, 4, 3, 4, 3, 5, 5, 5, 5, 4,&
       &4, 5, 1, 1, 3, 3, 5, 1, 5/

  data BETA /&
       &0.247354, 0.217481, 0.179705, 0.205647, 0.216823,&
       &0.216823, 0.282203, 0.216823, 0.281542, 0.170425,&
       &0.168227, 0.216823, 0.216823, 0.216823, 0.236925,&
       &0.163506, 0.216823, 0.182940, 0.172690, 0.302866,&
       &0.216823, 0.216823, 0.216823, 0.216823, 0.216823,&
       &0.216823, 0.216823, 0.216823, 0.216823, 0.216823,&
       &0.216823, 0.216823, 0.216823, 0.216823, 0.216823,&
       &0.216823, 0.216823, 0.216823, 0.216823/

  data MYACTS/94,97/
  data PSP/ 10*0.0, 6*1.0, 14*0.0, 2*1.0, 7*0.0 /
  data PMSC/ 39*.317888 /

  ! THESE SETTINGS ARE FOR NWO MODEL TYPE; IF SMC MODEL TYPE THEN
  ! CHANGE SOME OF THE SETTINGS FOR DF. THIS IS DONE BELOW.

  data MORTMAP/&
       &2,2,2,2,2,2,2,4,4,2,&
       &4,4,4,4,4,1,6,3,3,3,& ! DF NWO
       ! SMC     & 4,4,4,4,4,4,1,3,3,3,                         ! DF SMC
       &4,4,4,4,4,4,4,5,4,4,&
       &4,4,4,4,4,4,4,1,1/

  data BM0/&
       &-7.60159    ,-7.60159    ,-7.60159    ,-7.60159    ,-7.60159   ,&
       &-7.60159    ,-7.60159    ,-1.922689902,-1.922689902,-7.60159   ,&
       &-1.050000682,-1.050000682,-1.050000682,-1.050000682,-1.050000682,&
       &-4.13142    ,-4.13142    ,-0.761609   ,-0.761609   ,-0.761609  ,&
       ! SMC     & -3.12161659 ,-4.13142    ,-0.761609   ,-0.761609   ,-0.761609  ,
       &-2.976822456,-2.0        ,-2.0        ,-2.0        ,-4.317549852,&
       &-2.0        ,-2.0        ,-0.0        ,-1.050000682,-1.050000682,&
       &-1.050000682,-1.050000682,-4.072781265,-3.020345211,-3.020345211,&
       &-3.020345211,-2.0,        -4.13142    ,-4.13412/

  data BM1/&
       &-0.200523   ,-0.200523   ,-0.200523   ,-0.200523   ,-0.200523  ,&
       &-0.200523   ,-0.200523   ,-0.136081990,-0.136081990,-0.200523  ,&
       &-.194363402 ,-.194363402 ,-.194363402 ,-.194363402 ,-.194363402,&
       &-1.13736    ,-1.13736    ,-0.529366   ,-0.529366   ,-0.529366  ,&
       ! SMC     & -0.44724396 ,-1.13736    ,-0.529366   ,-0.529366   ,-0.529366  ,
       &0.0         ,-0.5        ,-0.5        ,-0.5        ,-0.057696253,&
       &-0.5        ,-0.5        ,-0.0        ,-.194363402 ,-.194363402,&
       &-.194363402 ,-.194363402 ,-0.176433475,0.0         ,0.0        ,&
       &0.0         ,-0.5        ,-1.13736    ,-1.13736/

  data BM2/&
       &0.0         ,0.0         ,0.0         ,0.0         ,0.0        ,&
       &0.0         ,0.0         ,0.002479863 ,0.002479863 ,0.0        ,&
       &0.003803100 ,0.003803100 ,0.003803100 ,0.003803100 ,0.003803100,&
       &0.0         ,0.0         ,0.0         ,0.0         ,0.0        ,&
       &0.0         ,0.015       ,0.015       ,0.015       ,0.0        ,&
       &0.015       ,0.015       ,0.0         ,0.003803100 ,0.003803100,&
       &0.003803100 ,0.003803100 ,0.0         ,0.0         ,0.0        ,&
       &0.0         ,0.015       ,0.0         ,0.0/

  data BM3/&
       &0.0         ,0.0         ,0.0         ,0.0         ,0.0       ,&
       &0.0         ,0.0         ,-3.178123293,-3.178123293,0.0       ,&
       &-3.557300286,-3.557300286,-3.557300286,-3.557300286,-3.557300286,&
       &-0.823305   ,-0.823305   ,-4.74019    ,-4.74019    ,-4.74019   ,&
       ! SMC     & -2.48387172 ,-0.823305   ,-4.74019    ,-4.74019    ,-4.74019   ,
       &-6.223250962,-3.0        ,-3.0        ,-3.0        ,0.0        ,&
       &-3.0        ,-3.0        ,0.0         ,-3.557300286,-3.557300286,&
       &-3.557300286,-3.557300286,-1.729453975,-8.467882343,-8.467882343,&
       &-8.467882343,-3.0        ,-0.823305   ,-0.823305/

  data BM4/&
       &0.0441333   ,0.0441333   ,0.0441333   ,0.0441333   ,0.0441333  ,&
       &0.0441333   ,0.0441333   ,0.0         ,0.0         ,0.0441333  ,&
       &0.003971638 ,0.003971638 ,0.003971638 ,0.003971638 ,0.003971638,&
       &0.0307749   ,0.0307749   ,0.0119587   ,0.0119587   ,0.0119587  ,&
       ! SMC     & 0.01843137  ,0.0307749   ,0.0119587   ,0.0119587   ,0.0119587  ,
       &0.0         ,0.015       ,0.015       ,0.015       ,0.004861355,&
       &0.015       ,0.015       ,0.0         ,0.003971638 ,0.003971638,&
       &0.003971638 ,0.003971638 ,0.0         ,0.013966388 ,0.013966388,&
       &0.013966388 ,0.015       ,0.0307749   ,0.0307749/

  data BM5/&
       &0.00063849  ,0.00063849  ,0.00063849  ,0.00063849  ,0.00063849  ,&
       &0.00063849  ,0.00063849  ,0.004684133 ,0.004684133 ,0.00063849  ,&
       &0.005573601 ,0.005573601 ,0.005573601 ,0.005573601 ,0.005573601,&
       &0.00991005  ,0.00991005  ,0.00756365  ,0.00756365  ,0.00756365  ,&
       ! SMC     & 0.01353918  ,0.00991005  ,0.00756365  ,0.00756365  ,0.00756365  ,
       &0.0         ,0.01        ,0.01        ,0.01        ,0.00998129,&
       &0.01        ,0.01        ,0.0         ,0.005573601 ,0.005573601,&
       &0.005573601 ,0.005573601 ,0.012525642 ,0.009461545 ,0.009461545,&
       &0.009461545 ,0.01        ,0.00991005  ,0.00991005/

  WKIT2 = 0.0

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'MORTS',5,ICYC)

  !  IF THERE ARE NO TREE RECORDS, SIMPLY PROCESS THE MORTALITY
  !  MULTIPLIERS AND BRANCH TO END (FOR THE METHOD USED TO GET
  !  MULTIPLIERS, SEE SUBROUTINE MULTS).

  call OPFIND (1,MYACTS(1),NTODO)
  if (NTODO.eq.0) goto 12
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
        5 continue
     end do
     9 continue
  end do
  12 continue

  ! IF BARE GROUND PLANT, LIMITS WERE NOT ADJUSTED FROM A PERCENT TO A
  ! PROPORTION IN CRATET, ADJUST THEM HERE.

  if (PMSDIL .gt.1.0)PMSDIL = PMSDIL/100.
  if (PMSDIU .gt.1.0)PMSDIU = PMSDIU/100.

  if (ITRN.le.0) goto 100

  ! SDIMAX IS USED HERE TO CARRY WEIGHTED SDI MAXIMUM. IF A USER-DEFINED
  ! BASAL AREA MAXIMUM HAS NOT BEEN ENTERED, THEN BASAL AREA MAXIMUM
  ! WILL BE RESET TO BE CONSISTENT WITH THE SDI MAXIMUM. THIS IS NEEDED
  ! FOR MODELING CLIMATE CHANGE.

  call SDICAL(0,SDIMAX)
  if (DEBUG)write(JOSTND,*)'IN MORTS CYCLE= ',ICYC,'  BAMAX= ',&
       &BAMAX,'  SDIMAX= ',SDIMAX

  !  ESTIMATE QUADRATIC MEAN DIAMETER 5 YEARS HENCE.

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
     if (LZEIDE.and.(D.lt.DBHZEIDE))go to 20 ! BRANCH IF D IS LT MIN DBH
     if (.not.LZEIDE.and.(D.lt.DBHSTAGE))go to 20
     CIOBDS = (2.0*D*G+G*G)
     CIOBDS1(I) = CIOBDS
     SD2SQ = SD2SQ+P*(D*D+CIOBDS)
     if (LZEIDE) then
        SUMDR10 = SUMDR10+P*(D+G)**1.605
     end if
     T = T+P
     20 continue
  end do
  if (T.eq.0.0) T = 0.0001
  DQ10 = SQRT(SD2SQ/T)
  if (LZEIDE) then
     DQ10 = (SUMDR10/T)**(1/1.605)
  end if
  if (DEBUG)write(JOSTND,*)'SD2SQ,SUMDR10,DQ10= ',&
       &SD2SQ,SUMDR10,DQ10

  !  CALCULATE AVERAGE DBH USED IN INDIV. MORT EQUATION BELOW

  DSUM = 0.0
  WPROB = 0.0
  do I = 1,ITRN
     WPROB = WPROB+PROB(I)
     DSUM = DSUM+DBH(I)*PROB(I)
  end do
  AVED = DSUM/WPROB
  if (DEBUG)write(JOSTND,*)'IN MORTS DSUM,WPROB,AVED= ',DSUM,WPROB,&
       &AVED

  !  SEE IF ORGANON MORTALITY HAS BEEN ESTIMATED. IF YES, USE ORGANON
  !  RATES FOR ALL TREE RECORDS. IF NO, THEN ESTIMATE FVS MORTALITY.

  SMORMT = 0.0
  do I = 1,ITRN
     SMORMT = SMORMT + MORTEXP(I)
  end do
  if (DEBUG)write(JOSTND,*)' IN MORTS SMORMT= ',SMORMT

  !  START LOOP TO ESTIMATE MORTALITY RATE.  TREES ARE PROCESSED
  !  ONE AT A TIME WITHIN A SPECIES.

  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (I1.le.0) go to 50
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
        if (P.le.0.0) go to 40

        !  IF ORGANON MORTALITY HAS BEEN ESTIMATED, USE IT

        if (LORGANON .and. (SMORMT .gt.0.)) then
           WK2(I) = MORTEXP(I)*(FINT/5.)
           WKI = MORTEXP(I)*(FINT/5.)
           if (DEBUG) write(JOSTND,*) ' IN MORTS I,ISPC,D,WK2,IORG= ',&
                &I,ISPC,D,WK2(I),IORG(I)
           go to 39
        end if

        RELDBH = D/AVED
        RELDBH1(I) = RELDBH
        if (D.le.0.5)D = 0.5
        DGT = WK1(I)/OLDFNT
        if (D.le.1.0.and. DGT.lt.0.05) DGT = 0.05
        if (D.le.5.0.and. D.gt.1.0.and. DGT.lt.0.05)&
             &DGT = 0.05*(5.0-D)/4.0
        G = WK1(I)/(BARK*OLDFNT)
        if (WK1(I)/OLDFNT .lt. DGT)G = DGT/BARK

        !  COMPUTE MORTALITY RATE

        ! WC AND PN MORTALITY FUNCTIONS WERE REPLACED WITH ORGANON NWO AND A
        ! FEW FROM ORGANON SWO BECAUSE OF ISSUES THAT WERE FOUND THAT THE
        ! OLD EQUATIONS WERE APPLYING MORTALITY TOO EQUALLY BY SIZE, TREE DBH
        ! COEFFICIENT IS MORE SIGNIFICANT IN THE ORGANON EQUATIONS AND
        ! WORK BETTER. ESM07/10/2014
        ! IF USERS WANT TO TEST SMC EQUATION FOR DF SEE COMMENTED OUT
        ! CODE IN DATA STATEMETNS.
        ! OREGON WHITE OAK USES A DIFFERENT EQUATION BUILT
        ! BY HARRINGTON AND GOULD.
        ! SMALL TREES USE EQUATIONS BY GOULD AND HARRINGTON

        CR = ICR(I)*0.01
        BAL = (1.0 - (PCT(I)/100.)) * BA
        PTBAL = PTBALT(I)
        CRADJ = 1.0
        if (CR .le.0.17) CRADJ = 1.0-EXP(-(25.0*CR)**2.0)
        XSITE2 = SITEAR(19)
        XSITE1 = SITEAR(16)

        ! IF SMC MODEL TYPE USE MORTMAP(26)=4 INSTEAD OF 1

        IMRTMP = MORTMAP(ISPC)
        RBM0 = BM0(ISPC)
        RBM1 = BM1(ISPC)
        RBM2 = BM2(ISPC)
        RBM3 = BM3(ISPC)
        RBM4 = BM4(ISPC)
        RBM5 = BM5(ISPC)
        if (IMODTY .eq.3.and. ISPC .eq.16) then
           IMRTMP = 4
           RBM0 = -3.12161659
           RBM1 = -0.44724396
           RBM3 = -2.48387172
           RBM4 = 0.01843137
           RBM5 = 0.01353918
        end if

        select case(IMRTMP)
        case(1)
           RIP = RBM0 + RBM1*D**.5 + RBM3*CR**.25 +&
                &RBM4*(XSITE1+4.5) + RBM5*BAL
           RIP = (1.0/(1.0+EXP(-(RIP)))) !5 yr RIP
           RIP = (1.0-RIP)**(0.2) !annual survival
           RIP = 1.0-RIP*CRADJ !annual RIP
        case(2)
           RIP = RBM0 + RBM1*D + RBM4*(XSITE1+4.5) +  RBM5*(BAL/D)
           RIP = (1.0/(1.0+EXP(-(RIP)))) !5 yr RIP
           RIP = (1.0-RIP)**(0.2) !annual survival
           RIP = 1.0-RIP*CRADJ !annual RIP
        case(3)
           RIP = RBM0 + RBM1*D + RBM2*D**2 + RBM3*CR +&
                &RBM4*(XSITE2+4.5) + RBM5*BAL
           RIP = (1.0/(1.0+EXP(-(RIP)))) !5 yr RIP
           RIP = (1.0-RIP)**(0.2) !annual survival
           RIP = 1.0-RIP*CRADJ !annual RIP
        case(4)
           RIP = RBM0 + RBM1*D + RBM2*D**2 + RBM3*CR +&
                &RBM4*(XSITE1+4.5) + RBM5*BAL
           RIP = (1.0/(1.0+EXP(-(RIP)))) !5 yr RIP
           RIP = (1.0-RIP)**(0.2) !annual survival
           RIP = 1.0-RIP*CRADJ !annual RIP
        case(5)
           RELHT = 0.0
           if (AVH .gt.0.0) RELHT = HT(I)/AVH
           if (RELHT .gt.1.5)RELHT = 1.5
           RIP = -6.6707 + 0.5105*ALOG(5+BA) - 1.3183*RELHT
           RIP = (1.0/(1.0+EXP(RIP)))
           RIP = 1.0-RIP !annual RIP
           ! NEW REDWOOD EQUATION
        case(6)
           RIP = 2.901447 + (0.578694*D) + (-0.001793*PTBAL)
           RIP = 1.0/(1.0 + EXP(RIP)) !annual RIP
           if (DEBUG) write(JOSTND,*)'IN MORTS RW DEBUG',' DBH=',D,&
                &' PTBAL=',PTBAL,' RIP=',RIP
        end select

        ! SMALL-TREE MORTALITY MODEL DEVELOPED BY GOULD AND HARRINGTON
        ! SMALL-TREE LOGIC IS BYPASSED FOR REDWOOD

        if (D .lt.3.0.and. ISPC .ne.17) then
           RELHT = 0.0
           if (AVH .gt.0.0) RELHT = HT(I)/AVH
           if (RELHT .gt.1.5)RELHT = 1.5
           HBH = HT(I)
           if (HBH .ge.4.5) HBH = 4.5
           DBHA = D+BETA(ISPC)*HBH
           ACLASS = MCLASS(ISPC)
           AVALUE = MVALUES(ACLASS)
           RIP = PTBAL*AVALUE/SQRT(DBHA+1)
           RIP = ALPHA(1) + ALPHA(2)*RIP
           RIP = RIP + ALPHA(3)*RELHT
           RIP = (1.0/(1.0+EXP(RIP)))
           RIP = 1.0-RIP
           IF(DEBUG)WRITE(JOSTND,*)'IN MORTS SMALL EQNS: I,RIP,HBH,        &
                &  DBHA,PTBAL,ACLASS,AVALUE,ALPHA1,ALPHA2,ALPHA3 =   ',I,RIP,HBH,&
                &DBHA,PTBAL,ACLASS,AVALUE,ALPHA(1),ALPHA(2),ALPHA(3)
        end if

        IF(DEBUG)WRITE(JOSTND,*)'IN MORTS EQNS IMRTMP,I,ISPC,RIP,D,CR,    &
             &XSITE1,XSITE2,BAL,CRDJ=',IMRTMP,I,ISPC,RIP,D,CR,XSITE1,&
             &XSITE2,BAL,CRADJ

        !  APPLY MORTALITY MULTIPLIERS WITHIN USER-PROVIDED DBH LIMITS
        !  AND STORE TREES PER ACRE DYING IN THE ARRAY WK2.

        X = 1.0
        if (D.ge.D1.and. D.lt.D2) X = XMORT

        !  FROM HERE TO STATEMENT 29 IS ESTABLISHMENT MODEL CODE.
        !  "BEST" TREES NOT SUBJECT TO MORTALITY FOR 20 YRS
        !  AFTER DISTURBANCE DATE.

        if (IESTAT(I).le.0) go to 29
        if (IY(ICYC).ge.IESTAT(I)) IESTAT(I) = 0
        XCHECK = FLOAT(IESTAT(I)-IY(ICYC))/FINT
        if (XCHECK.gt.1.0) XCHECK = 1.0
        if (XCHECK.lt.0.0) XCHECK = 0.0
        X = X* (1.0-XCHECK)
        29 continue
        !     CHANGED MINIMUM ANNUAL MORTALITY RATE (RIP) TO 0.001 TO ENSURE
        !     THAT STANDS DO NOT EXCEED MAXIMUM BA AND SDI. THIS CONSTRAINT WILL
        !     ONLY INFLUENCE NON-VALID ORGANON TREE RECORDS.
        !      IF (RIP.LT.0.00001) RIP=0.00001
        if (RIP.lt.0.001) RIP = 0.001
        WKI = P*(1.0-(1.0-RIP)**FINT)*X
        if (DEBUG)write(JOSTND,*)'IN MORTS EQ I,XCHECK,X,P,RIP,FINT,WKI= ',&
             &I,XCHECK,X,P,RIP,FINT,WKI

        BARK = BRATIO(IS,D,HT(I))
        G = (DG(I)/BARK) * (FINT/5.0)
        IDMFLG = IFIX(SIZCAP(ISPC,3))
        if ((D+G).ge.SIZCAP(ISPC,1) .and. IDMFLG.ne.1) then
           WKI = AMAX1(WKI,(P*SIZCAP(ISPC,2)*FINT/5.0))
           if (DEBUG)write(JOSTND,*)'SIZE CAP RESTRICTION IMPOSED, ',&
                &'I,ISPC,D,P,SIZCAP 1-3,WKI = ',&
                &I,ISPC,D,P,SIZCAP(ISPC,1),SIZCAP(ISPC,2),SIZCAP(ISPC,3),WKI
        end if
        39 continue
        if (WKI.gt.P) WKI = P

        !  IF SDIMAX IS LESS THAN 5, ASSUME CLIMATE HAS CHANGED ENOUGH THAT THE
        !  SITE WILL NO LONGER SUPPORT TREES, AND KILL ALL EXISTING TREES.

        if (SDIMAX .lt.5) then
           WKI = P
        end if

        !  RECALCULATE VALUES AFTER MORTALITY HAS BEEN APPLIED

        TA = TA+(PROB(I)-WKI)
        WK2(I) = WKI
        if (DEBUG)write(JOSTND,*)'IN MORTS INITIAL PASS: IXX WKI WK2=',&
             &I,WKI,WK2(I)
        40 continue
     end do
     50 continue
  end do
  goto 59

  !     RECALCULATE SDI AND BA FOR THE STAND AND MAKE SURE THEY ARE BELOW THE MAXIMUM

  55 continue
  DQ10A = SQRT(SD2SQA/TA)
  BAA = .005454154*DQ10A*DQ10A*TA
  if (LZEIDE) then
     DQ10A = (SUMDR10A/TA)**(1./1.605)
  end if
  SDIA = TA*(DQ10A/10)**1.605
  IF(DEBUG)WRITE(JOSTND,*)'IN MORTS:PASS,TA,SD2SQA,SUMDR10A,DQ10A   &
       &,BAA,SDIA=',PASS,TA,SD2SQA,SUMDR10A,DQ10A,BAA,SDIA

  !     LOOP BACK THROUGH TREES RECORDS IF STAND IF OVER MAXIMUM SDI.
  !     BUT BECAUSE BA CAN CREEP UP TO UNREALISTIC VALUES EVEN WHILE
  !     HOLDING SDI BELOW MAXIMUM AN ADDITIONAL MAXIMUM OF 550FT2/ACRE.
  !     THIS WAS SET BASED ON MAXIMUM BA FOUND ON 400 CVS PLOTS ON WESTSIDE

  if (((SDIA.lt.SDIMAX).and.(BAA.lt.550)).or.(PASS.gt.100)) go to 70
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
     if (LZEIDE.and.(DBH(I).lt.DBHZEIDE))go to 60 ! BRANCH IF D IS LT MIN DBH
     if (.not.LZEIDE.and.(DBH(I).lt.DBHSTAGE))go to 60
     SD2SQA = SD2SQA+(PROB(I)-WKI)*(DBH(I)*DBH(I)+CIOBDS1(I))
     if (LZEIDE) then
        SUMDR10A = SUMDR10A+(PROB(I)-WKI)*(DBH(I)+G1(I))**1.605
     end if
     TA = TA+(PROB(I)-WKI)
     IF(DEBUG)WRITE(JOSTND,*)'IN MORT PASS,I,DBH,WKI,WKIT2,SD2SQA,     &
          &SUMDR10A,TA=',PASS,I,DBH(I),WKI,WKIT2,SD2SQA,SUMDR10A,TA
     60 continue
  end do
  goto 55

  !  END OF TREE LOOPING.  PRINT DEBUG INFO IF DESIRED.

  70 continue
  do I = 1,ITRN
     if (PASS.gt.1) WK2(I) = WKII(I)
     if (WK2(I).gt.PROB(I)) WK2(I) = PROB(I)
     if (.not.DEBUG) go to 80
     PRES = PROB(I)-WK2(I)
     VLOS = WK2(I)*CFV(I)/FINT
     write(JOSTND,9000) I,ISPC,DBH(I),G1(I),RELDBH1(I),BA,PROB(I),&
          &WK2(I),PRES,VLOS
     9000 format('IN MORTS, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,&
          &',  DBH INCREMENT=',F7.4,',  RELDBH=',F7.4,',  BA=',F7.2,&
          &',  INIT PROB=',F9.3/&
          &', TREES DYING=',F9.3,'  RES PROB=',F9.3,&
          &',  VOL LOST=',F9.3)
     80 continue
  end do
  if (.not.DEBUG) go to 100
  write(JOSTND,9001)  ISPC
  9001 format('IN MORTS,  ISPC=',I3,&
       &11X)
  100 continue

  !  COMPUTE THE CLIMATE-PREDICTED MORTALITY RATES BY SPECIES

  call CLMORTS

  !  COMPUTE THE FIXMORT OPTION.  LOOP OVER ALL SCHEDULED FIXMORTS
  !  LINCL IS USED TO INDICATE WHETHER A TREE GETS AFFECTED OR NOT

  call OPFIND (1,MYACTS(2),NTODO)
  if (NTODO.gt.0) then
     if (DEBUG)write(JOSTND,*)'FIXMORT PROCESSING, ITODO= ',ITODO
     do ITODO = 1,NTODO
        call OPGET (ITODO,6,IDATE,IACTK,NP,PRM)
        if (IACTK.lt.0) goto 300
        call OPDONE(ITODO,IY(ICYC))
        ISPCC = IFIX(PRM(1))
        if (NP .le.4) then
           if (PRM(2).gt.1.0)PRM(2) = 1.0
        end if
        if (PRM(3).lt.0.0)PRM(3) = 0.0
        if (PRM(4).le.0.0)PRM(4) = 999.
        IP = 1
        if (NP.gt.4) then
           if (PRM(5).lt.3.) then
              if (PRM(2).gt.1.0)PRM(2) = 1.0
              if (PRM(2).lt.0.0)PRM(2) = 0.0
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
           if (PRM(6) .eq.1) then
              KPOINT = 1
           else if (PRM(6) .eq.10) then
              KBIG = 1
           else if (PRM(6) .eq.11) then
              KPOINT = 1
              KBIG = 1
           else if (PRM(6) .eq.20) then
              KBIG = 2
           else if (PRM(6) .eq.21) then
              KPOINT = 1
              KBIG = 2
           end if
        end if
        if (ITRN.gt.0) then

           ! IF CONCENTRATING MORTALITY ON A POINT, AND/OR BY SIZE TREES IS IN
           ! EFFECT, DETERMINE EFFECT OF THIS FIXMORT AND REALLOCATE BY POINT:
           !   REALLOCATE ALL MORTALITY IF REPLACE OPTION OR MULTIPLY OPTION
           !   ARE IN EFFECT.
           !   ONLY REALLOCATE ADDITIONAL MORTALITY IF "ADD" OPTION IS IN EFFECT
           !   ONLY REALLOCATE ADDITIONAL MORTALITY IF "MAX" OPTION IS IN EFFECT
           !   (I.E. MORTALITY OVER AND ABOVE WHAT WAS PREVIOUSLY PREDICTED.

           if (KBIG.ge.1.or. (KPOINT.eq.1.and. IPTINV.gt.1)) then
              XMORE = 0.
              do I = 1,ITRN
                 LINCL = .false.
                 if (ISPCC.eq.0.or. ISPCC.eq.ISP(I)) then
                    LINCL = .true.
                 else if (ISPCC.lt.0) then
                    IGRP = -ISPCC
                    IULIM = ISPGRP(IGRP,1)+1
                    do IG = 2,IULIM
                       if (ISP(I) .eq. ISPGRP(IGRP,IG)) then
                          LINCL = .true.
                          go to 91
                       end if
                       90 continue
                    end do
                 end if
                 91 continue
                 if (LINCL .and.&
                      &(PRM(3).le.DBH(I) .and. DBH(I).lt.PRM(4))) then
                    goto (191,192,193,194),IP
                    191 continue
                    XMORE = XMORE+PROB(I)*PRM(2)
                    WK2(I) = 0.
                    goto 199
                    192 continue
                    XMORE = XMORE+(AMAX1(0.0,PROB(I)-WK2(I))*PRM(2))
                    goto 199
                    193 continue
                    TEMP = AMAX1(WK2(I),(PROB(I)*PRM(2)))
                    if (TEMP .gt. WK2(I)) then
                       XMORE = XMORE+TEMP-WK2(I)
                    end if
                    goto 199
                    194 continue
                    XMORE = XMORE+WK2(I)*PRM(2)
                    WK2(I) = 0.
                    goto 199
                 end if
                 199 continue
              end do
              if (DEBUG)write(JOSTND,*)'KPOINT,KBIG,ITRN,XMORE= ',&
                   &KPOINT,KBIG,ITRN,XMORE
              CREDIT = 0.
              do I = 1,ITRN
                 IWORK1(I) = IND1(I)
                 if (KBIG .eq.1) then
                    WORK3(I) = (-1.0)*&
                         &(DBH(I)+DG(I)/BRATIO(ISP(I),DBH(I),HT(I)))
                 else
                    WORK3(I) = DBH(I)+DG(I)/BRATIO(ISP(I),DBH(I),HT(I))
                 end if
                 201 continue
              end do
              call RDPSRT(ITRN,WORK3,IWORK1,.false.)
              if (DEBUG)write(JOSTND,*)'DBH= ',(DBH(IG),IG = 1,ITRN)
              if (DEBUG)write(JOSTND,*)'IWORK1= ',(IWORK1(IG),IG = 1,ITRN)
              if (DEBUG)write(JOSTND,*)'WK2= ',(WK2(IG),IG = 1,ITRN)

              if (KBIG.ge.1.and. KPOINT.eq.0) then

                 !  CONCENTRATION BY SIZE ONLY

                 do I = 1,ITRN
                    IX = IWORK1(I)
                    LINCL = .false.
                    if (ISPCC.eq.0.or. ISPCC.eq.ISP(IX)) then
                       LINCL = .true.
                    else if (ISPCC.lt.0) then
                       IGRP = -ISPCC
                       IULIM = ISPGRP(IGRP,1)+1
                       do IG = 2,IULIM
                          if (ISP(IX) .eq. ISPGRP(IGRP,IG)) then
                             LINCL = .true.
                             go to 93
                          end if
                          92 continue
                       end do
                    end if
                    93 continue
                    if (LINCL .and.&
                         &(PRM(3).le.DBH(IX) .and. DBH(IX).lt.PRM(4))) then
                       TEMP = CREDIT+PROB(IX)-WK2(IX)
                       if ((TEMP .le. XMORE).or.&
                            &(ABS(TEMP-XMORE).lt.0.0001)) then
                          CREDIT = CREDIT+PROB(IX)-WK2(IX)
                          WK2(IX) = PROB(IX)
                       else
                          WK2(IX) = WK2(IX)+XMORE-CREDIT
                          CREDIT = XMORE
                          go to 295
                       end if
                    end if
                    310 continue
                 end do
                 go to 295

              else if (KPOINT.eq.1.and. KBIG.eq.0) then

                 !  CONCENTRATION ON POINTS ONLY

                 do J = 1,IPTINV
                    do I = 1,ITRN
                       if (ITRE(I) .ne. J)go to 204
                       LINCL = .false.
                       if (ISPCC.eq.0.or. ISPCC.eq.ISP(I)) then
                          LINCL = .true.
                       else if (ISPCC.lt.0) then
                          IGRP = -ISPCC
                          IULIM = ISPGRP(IGRP,1)+1
                          do IG = 2,IULIM
                             if (ISP(I) .eq. ISPGRP(IGRP,IG)) then
                                LINCL = .true.
                                go to 95
                             end if
                             94 continue
                          end do
                       end if
                       95 continue
                       if (LINCL .and.&
                            &(PRM(3).le.DBH(I) .and. DBH(I).lt.PRM(4))) then
                          TEMP = CREDIT+PROB(I)-WK2(I)
                          if ((TEMP .le. XMORE).or.&
                               &(ABS(TEMP-XMORE).lt.0.0001)) then
                             CREDIT = CREDIT+PROB(I)-WK2(I)
                             WK2(I) = PROB(I)
                          else
                             WK2(I) = WK2(I)+XMORE-CREDIT
                             CREDIT = XMORE
                             go to 295
                          end if
                       end if
                       204 continue
                    end do
                    205 continue
                 end do
                 go to 295

                 !  CONCENTRATION BY SIZE ON POINTS (POINTS HAVE PRIORITY, SO TREES
                 !  WILL BE KILLED BY SIZE ON ONE POINT BEFORE MOVING TO THE NEXT
                 !  POINT TO START WITH THE BIGGEST/SMALLEST TREES ON THAT POINT.
              else
                 do J = 1,IPTINV
                    do I = 1,ITRN
                       IX = IWORK1(I)
                       if (ITRE(IX) .ne. J)go to 311
                       LINCL = .false.
                       if (ISPCC.eq.0.or. ISPCC.eq.ISP(IX)) then
                          LINCL = .true.
                       else if (ISPCC.lt.0) then
                          IGRP = -ISPCC
                          IULIM = ISPGRP(IGRP,1)+1
                          do IG = 2,IULIM
                             if (ISP(IX) .eq. ISPGRP(IGRP,IG)) then
                                LINCL = .true.
                                go to 97
                             end if
                             96 continue
                          end do
                       end if
                       97 continue
                       if (LINCL .and.&
                            &(PRM(3).le.DBH(IX) .and. DBH(IX).lt.PRM(4))) then
                          TEMP = CREDIT+PROB(IX)-WK2(IX)
                          if ((TEMP .le. XMORE).or.&
                               &(ABS(TEMP-XMORE).lt.0.0001)) then
                             CREDIT = CREDIT+PROB(IX)-WK2(IX)
                             WK2(IX) = PROB(IX)
                          else
                             WK2(IX) = WK2(IX)+XMORE-CREDIT
                             CREDIT = XMORE
                             go to 295
                          end if
                       end if
                       311 continue
                    end do
                    312 continue
                 end do
                 go to 295
              end if

           end if

           !  NORMAL FIXMORT PROCESSING WHEN POINT OR SIZE CONCENTRATION
           !  IS NOT IN EFFECT.

           do I = 1,ITRN
              LINCL = .false.
              if (ISPCC.eq.0.or. ISPCC.eq.ISP(I)) then
                 LINCL = .true.
              else if (ISPCC.lt.0) then
                 IGRP = -ISPCC
                 IULIM = ISPGRP(IGRP,1)+1
                 do IG = 2,IULIM
                    if (ISP(I) .eq. ISPGRP(IGRP,IG)) then
                       LINCL = .true.
                       go to 99
                    end if
                    98 continue
                 end do
              end if
              99 continue
              if (LINCL .and.&
                   &(PRM(3).le.DBH(I) .and. DBH(I).lt.PRM(4))) then
                 goto (610,620,630,640),IP
                 610 continue
                 WK2(I) = PROB(I)*PRM(2)
                 goto 290
                 620 continue
                 WK2(I) = WK2(I)+(AMAX1(0.0,PROB(I)-WK2(I))*PRM(2))
                 goto 290
                 630 continue
                 WK2(I) = AMAX1(WK2(I),(PROB(I)*PRM(2)))
                 goto 290
                 640 continue
                 WK2(I) = AMIN1(PROB(I),WK2(I)*PRM(2))
                 goto 290
              end if
              290 continue
           end do
           295 continue
           if (DEBUG)write(JOSTND,*)'ITODO,WK2= ',&
                &ITODO,(WK2(IG),IG = 1,ITRN)
        end if
        300 continue
     end do
  end if
  return

  entry MORCON
  return
end
