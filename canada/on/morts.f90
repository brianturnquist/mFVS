!============================!
!   Compute tree mortality   !
!============================!
! CANADA-ON $Id: morts.f 3998 2022-05-18 18:50:56Z donrobinson $
subroutine MORTS
  implicit none

  !  THIS SUBROUTINE COMPUTES PERIODIC MORTALITY RATES FOR
  !  EACH TREE RECORD AND THEN REDUCES THE NUMBER OF TREES/ACRE
  !  REPRESENTED BY THE TREE RECORD.
  !  THIS ROUTINE IS CALLED FROM **TREGRO** WHEN CYCLING FOR GROWTH
  !  PREDICTION.  ENTRY **MORCON** IS CALLED TO LOAD SITE DEPENDENT
  !  CONSTANTS.

  !  SDI BASED MORTALITY USES MAXIMUM SDI EQUATIONS FROM:
  !  PENNER, MARGARET. 3/31/2010. FVS_ONTARIO MORTALITY EQUATIONS.
  !  REPORT PREPARED FOR JOHN PARTON OMNR. EQUATION (9).

  !  EQUATIONS FOR MATURE STAND BOUNDARY WERE DEVELOPED BY GARY DIXON AND
  !  APPROXIMATE PENNERS EQUATION (10) IN THE SAME REPORT.

  !  IF A BASAL AREA MAXIMUM HAS BEEN SET BY THE USER, A CHECK IS MADE TO
  !  MAKE SURE BASAL AREA HAS NOT EXCEEDED THE SPECIFIED LIMIT.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'OUTCOM.f90'
  include 'PLOT.f90'
  include 'COEFFS.f90'
  include 'ESTREE.f90'
  include 'MULTCM.f90'
  include 'PDEN.f90'
  include 'VARCOM.f90'
  include 'WORKCM.f90'
  include 'METRIC.f90'

  !  DEFINITIONS OF LOCAL VARIABLES:

  !    BKG_MAP = MAPPING FUNCTION RELATING ONTARIO SPECIES TO ONE OF
  !              THE 4 BACKGROUND MORTALITY COEFFICIENT SETS
  !    C0 -- CONSTANT TERM FOR SDI RELATIONSHIP
  !             LN(T) = LN(C0)-1.605 *LN(QMD)
  !    D -- TREE DIAMETER.
  !    DQ0     = START OF CYCLE MEAN SQUARE DIAMETER
  !    DQ10    = QUADRATIC DBH AT END OF CYCLE IF TREES/ACRE IS HELD CONSTANT
  !    DR0     = START OF CYCLE REINEKES DIAMETER
  !    DR10    = REINEKES DBH AT END OF CYCLE IF TREES/ACRE IS HELD CONSTANT
  !    I -- TREE SUBSCRIPT.
  !    INDX_BA = FVS SEQUENCE NUMBER OF THE SPECIES WITH THE MOST BA
  !    MATURE STAND BOUNDARY LINE:  LN(TPH)=A+B*LN(QMDcm)
  !    GARY DIXONS APPROXIMATION OF MARGARET PENNERS EQUATION 10 SPLINED
  !    TO MEET THE SDI MAXIMUM LINE AT THE BREAKPOINT DBH
  !      MSB_DBH = BREAKPOINT LN(QMDcm) AT WHICH STAND DENSITY CONTROL
  !                SWITCHES FROM THE MAXIMUM SDI LINE TO THE MATURE
  !                STAND BOUNDARY LINE
  !      MSB_INT = INTERCEPT FOR THE MATURE STAND BOUNDARY LINE
  !      MSB_SLP = SLOPE OF THE MATURE STAND BOUNDARY LINE
  !    MSB_MAP = MAPPING FUNCTION RELATING ONTARIO SPECIES TO ONE OF
  !              PENNERS 13 MAXIMUM SDI EQUATIONS
  !    P -- NUMBER OF TREES PER ACRE REPRESENTED BY A TREE.
  !    PMSC -- CONSTANT TERMS FOR EACH SPECIES FOR THE BACKGROUND
  !             MORTALITY RATE EQUATION (B0).
  !    PMD -- COEFFICIENTS FOR EACH SPECIES FOR THE DIAMETER TERM
  !             IN THE BACKGROUND MORTALITY RATE EQUATION (B1).
  !    RI -- ESTIMATED ANNUAL BACKGROUND MORTALITY RATE
  !    RIP -- WEIGHTED AVERAGE MORTALITY RATE BASED ON TREE DBH, RI,
  !           AND RN.
  !    RN -- MORTALITY RATE THAT WILL MATCH THE TREND IN TREES PER
  !          ACRE THAT IS PREDICTED FROM THE SDI RELATIONSHIP
  !          TREES/ACRE LOSS EXPRESSED AS PER YEAR LOSS
  !   MARGARET PENNERS MAXIMUM SDI EQUATION: LN(QMDcm)=A+B*LN(TPH)
  !     WHEN USED THIS EQUATION IS SOLVED: LN(TPH)=(-A/B)+(1/B)*LN(QMDcm)
  !     WHERE SDIINT = (-A/B), AND SDISLP = (1/B)
  !     SDI_INT = MARGARET PENNERS INTERCEPT FOR THE MAXIMUM SDI LINE
  !     SDI_SLP = MARGARET PENNERS SLOPE FOR THE MAXIMUM SDI LINE
  !    T       = TREES/ACRE AT START OF CYCLE
  !    TN10    = PREDICTED TREES/ACRE AT THE END OF THE CYCLE
  !    WKI -- SCALAR USED FOR CALCULATION AND TEMPORARY STORAGE
  !           OF TREE MORTALITY (TREES/ACRE).

  ! Variables
  logical :: DEBUG,LINCL

  integer :: I,IACTK,IDATE,IDMFLG,IG,IGRP,INDX,INDX_BA
  integer :: IP,IPASS,IPATH,IS,ISPC,ISPCC,ITODO,IULIM,IX,I1,I2,I3
  integer :: J,KBIG,KNT,KNT2,KPOINT
  integer :: NTODO,NP

  integer :: BKG_MAP(MAXSP),MSB_MAP(MAXSP)
  integer :: MYACTS(2)

  real    :: ADJFAC,BADEAD,BANEW,BARK,BRATIO,B0,B1
  real    :: CEPT,CIOBDS,CONST,CREDIT
  real    :: D,DBHEND,DIA0,DIFF,DR0,DR10,DR10N,DQ0,DQ10
  real    :: DQ10N,D1,D2,D10,D10N,D55M,D85M,G,P,PRES,QMDNEW
  real    :: RI,RIP,RN
  real    :: SDIINT,SDISLP,SDQ0,SD2SQ,SD2SQN,SLP
  real    :: SUMBA,SUMDR0,SUMDR10,SUMDR10N,SUMKIL,SUMTRE
  real    :: T,TBAM1,TEM,TEMQMD,TEMEFF,TEMINT,TEMP,TEMSLP,TMD0,TMD10
  real    :: TMMSB,TMORE,TN,TNEW,TN10,TPACLS,TPHMAX
  real    :: TPRIME,TREEIT,T55D0,T55D10,T55M,T85D0,T85D10,T85M,T85MSB
  real    :: VLOS,WKI,X,XMORE,XMORT

  real    :: BASP(MAXSP)
  real    :: MSB_DBH(13),MSB_INT(13),MSB_SLP(13)
  real    :: PMSC(4),PMD(4),PRM(6)
  real    :: SDI_INT(13),SDI_SLP(13)

  ! Data statements
  data MYACTS/94,97/

  !     MAX SDI MORTALITY COEFFICIENTS FROM MARGARET PENNER
  !     Source: FVS_Mortality_Report.docx, Table 6; Eqn (9)
  !     SDI_INT,SDI_SLP MAPPED BY MSB_MAP

  data SDI_INT /&
       &7.77029,  6.25277,  5.98521,  5.85622, 4.85176,&
       &8.99134,  4.49037,  6.90979,  6.97823, 6.32553,&
       &7.40806,  8.26319,  6.69982/
  data SDI_SLP /&
       &-0.61566, -0.40357, -0.38783,  -0.3828, -0.25916,&
       &-0.8304, -0.20807, -0.49564, -0.54518, -0.46181,&
       &-0.58078, -0.74894, -0.51312 /

  !     MSB COEFFICIENTS
  !     Source: Gary Dixon
  !     APPROXIMATE MARGARET PENNER
  !     FVS_Mortality_Report.docx, Table 7; Eqn (10)
  !     MSB_DBH, MSB_INT, MSB_SLP; MAPPED BY MSB_MAP

  data MSB_DBH /&
       &3.00,     3.30,     2.00,     2.90,     2.75,&
       &3.70,     2.80,     3.10,     3.50,     2.90,&
       &3.10,     3.30,     2.40/
  data MSB_INT /&
       &14.64825, 20.51662, 16.27566, 21.06262, 32.03489,&
       &15.99204, 29.68404, 17.91661, 13.23997, 20.46762,&
       &15.78771, 18.28047, 13.65976/
  data MSB_SLP /&
       &-2.30,    -4.00,    -3.00,    -4.60,    -8.70,&
       &-2.60,    -7.70,    -3.30,    -1.96,    -4.50,&
       &-2.70,    -3.60,    -2.20/

  !     MAP EACH SPECIES TO ONE OF 13 FITTED EQUATIONS
  !     MSB = MAXIMUM STAND BOUNDARY; THE SAME MAPPING
  !     IS USED FOR SDI-BASED MORTALITY: SDI_INT & SDI_SLP

  data MSB_MAP /&
       &3, 2, 2, 2, 1, 4, 4, 6, 5, 3,& ! 10
       &8, 7, 2, 5,11,11,13,10,10,11,&
       &10,10,10,12,10, 9,10, 9,11,12,& ! 30
       &12,12,12,12,12,12,11,12,12,13,&
       &13,13,11,12,12,12,10,13,12,10,& ! 50
       &10,10,10,10,12,10,11,11,10,12,&
       &11,11,11,12,13,13,11,12, 3, 1,& ! 70
       &4, 5 /

  ! BACKGROUND MORTALITY CONSTANTS
  ! MAPPED BY BKG_MAP

  data PMSC/5.1676998,   9.6942997,   5.5876999,   5.9617000/
  data PMD/-0.0077681,  -0.0127328,  -0.0053480,  -0.0340128/
  data BKG_MAP/&
       &1, 3, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 3, 3, 4, 1, 4, 1, 1, 4,&
       &1, 1, 1, 4, 1, 1, 1, 1, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,&
       &4, 4, 4, 1, 4, 4, 1, 1, 4, 4, 4, 4, 1, 4, 4, 1, 4, 4, 1, 4,&
       &4, 4, 4, 1, 1, 4, 1, 4, 1, 3, 1, 1/

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'MORTS',5,ICYC)
  if (DEBUG)write(JOSTND,9000)ICYC
  9000 format(' ENTERING SUBROUTINE MORTS  CYCLE =',I4)

  !     INITIALIZATIONS

  TREEIT = 0.
  KNT = 0
  TPHMAX = 35000.0*HAtoACR

  !     PROCESS MORTMULT KEYWORD.

  call OPFIND(1,MYACTS(1),NTODO)
  if (NTODO .eq.0)go to 25
  do I = 1,NTODO
     call OPGET(I,4,IDATE,IACTK,NP,PRM)
     call OPDONE(I,IY(ICYC))
     ISPCC = IFIX(PRM(1))
     if (ISPCC .eq.0)go to 21
     XMMULT(ISPCC) = PRM(2)
     XMDIA1(ISPCC) = PRM(3)
     XMDIA2(ISPCC) = PRM(4)
     go to 24
     21 continue
     do ISPCC = 1,MAXSP
        XMMULT(ISPCC) = PRM(2)
        XMDIA1(ISPCC) = PRM(3)
        XMDIA2(ISPCC) = PRM(4)
        22 continue
     end do
     24 continue
  end do
  25 continue
  if (DEBUG)write(JOSTND,9010)ICYC,RMSQD
  9010 format(1H0,'IN MORTS 9010 ICYC,RMSQD= ',&
       &I5,5X,F10.5)
  if (RMSQD .eq.0.) then
     CEPMRT = 0.
     SLPMRT = 0.
  end if

  !     IF BARE GROUND PLANT, LIMITS WERE NOT ADJUSTED FROM A PERCENT TO A
  !     PROPORTION IN CRATET, ADJUST THEM HERE.

  if (PMSDIL .gt.1.0)PMSDIL = PMSDIL/100.
  if (PMSDIU .gt.1.0)PMSDIU = PMSDIU/100.

  !     INITIALIZE STARTING VALUES.
  !     DQ0, CALCULATED BELOW, IS THE SAME AS RMSQD THAT IS CALCULATED IN DENSE
  !     WHEN DBHSDI=0.

  IPASS = 0
  SUMKIL = 0.0

  !     COMPUTE QUADRATIC MEAN DIAMETER AT THE BEGINNING OF THE CYCLE.
  !     COMPUTE INITIAL ESTIMATES OF QMD AND BASAL AREA BY SPECIES AT THE END
  !     OF THE CYCLE USING END-CYCLE DIAMETER AND START-CYCLE PROB. THESE
  !     END-OF-CYCLE QMD WILL BE ADJUSTED AS TREES ARE KILLED UNTIL
  !     A SOLUTION IS REACHED WITHIN SPECIFIED LIMITS.

  T = 0.0
  DQ0 = 0.0
  SDQ0 = 0.0
  SD2SQ = 0.0
  SUMDR0 = 0.0
  SUMDR10 = 0.0
  DR0 = 0.0
  DR10 = 0.0
  SUMBA = 0.0
  do ISPC = 1,MAXSP
     BASP(ISPC) = 0.0
     I1 = ISCT(ISPC,1)
     if (I1.le.0)go to 20
     I2 = ISCT(ISPC,2)
     do I3 = I1,I2
        I = IND1(I3)
        P = PROB(I)
        IS = ISP(I)
        WK2(I) = 0.0
        D = DBH(I)
        if (D.lt.DBHSDI)go to 12 ! BRANCH IF D IS LT USER'S DBHSDI
        BARK = BRATIO(IS,D,HT(I))
        G = (DG(I)/BARK) * (FINT/10.0)
        CIOBDS = (2.0*D*G+G*G)
        TEM = P*(D*D+CIOBDS)
        SD2SQ = SD2SQ+TEM
        SDQ0 = SDQ0+P*(D)**2.
        if (LZEIDE) then
           SUMDR10 = SUMDR10+P*(D+G)**1.605
           SUMDR0 = SUMDR0+P*(D)**1.605
        end if
        T = T+P
        BASP(ISPC) = BASP(ISPC) + TEM*0.0054542
        SUMBA = SUMBA + TEM*0.0054542
        12 continue
     end do
     20 continue
  end do
  if (DEBUG)write(JOSTND,*)'SDQ0,SD2SQ,SUMDR0,SUMDR10= ',&
       &SDQ0,SD2SQ,SUMDR0,SUMDR10
  if (DEBUG)write(JOSTND,9002)SUMBA,BASP
  9002 format(' SUMBA,BASP= ',F10.5,(/10(F10.5)))

  !  DETERMINE SPECIES WITH THE MOST BA; THIS WILL DETERMINE WHICH
  !  MAXIMUM SDI OR MSB RELATIONSHIP IS CONTROLLING STAND DENSITY

  INDX_BA = 1
  TEM = BASP(1)
  do ISPC = 1,MAXSP
     if (BASP(ISPC).gt. TEM) then
        INDX_BA = ISPC
        TEM = BASP(ISPC)
     end if
  end do
  if (DEBUG)write(JOSTND,*)' INDX_BA,IBASP= ',INDX_BA,IBASP
  if (INDX_BA .ne. IBASP) then
     IBASP = INDX_BA
     CEPMRT = 0.
     SLPMRT = 0.
  end if

  !     SOME EVENTS CAN CHANGE THE TRAJECTORY OF A STAND. FOR EXAMPLE,
  !     REGENERATION, THINNING, USER MORTALITY, FIRE, I&P EFFECTS.
  !     IF THE TRAJECTORY IS CHANGED,
  !     RESET SLOPE AND INTERCEPT FOR CALCULATION.
  !     (THE VALUE OF 1. IS TO ALLOW FOR ROUNDING ERROR)

  if (ICYC.gt.1.and. ABS(T-TPAMRT).gt.1.) then
     CEPMRT = 0.
     SLPMRT = 0.
     if (DEBUG)write(JOSTND,*)' RESETTING SLOPE,INTERCEPT T,TPAMRT= ',&
          &T,TPAMRT
  end if

  if (T .lt.1.0) go to 45
  DQ0 = SQRT(SDQ0/T)
  DQ10 = SQRT(SD2SQ/T)
  if (LZEIDE) then
     DR0 = (SUMDR0/T)**(1./1.605)
     DR10 = (SUMDR10/T)**(1./1.605)
  end if
  if (DEBUG)write(JOSTND,*)' IN MORTS, T,ISISP,LZEIDE= ',&
       &T,ISISP,LZEIDE
  if (LZEIDE) then
     DIA0 = DR0
     D10 = DR10
  else
     DIA0 = DQ0
     D10 = DQ10
  end if
  10 continue
  if (DEBUG) write(JOSTND,9020) DQ0,DQ10,T,SD2SQ,DR0,DR10
  9020 format(1H ,'IN MORTS  DQ0 = ',F10.5,' DQ10 = ',F10.2,&
       &' T = ',F8.2,' SD2SQ = ',F10.2,' DR0= ',F10.2,' DR10= ',F10.2)

  !     IF D IS TOO LOW, NUMERICAL PROBLEMS OCCUR. RESET IF NECESSARY.

  if (DIA0.lt.0.3) then
     D10 = 0.3 + D10 - DIA0
     DIA0 = 0.3
     if (DEBUG) write(JOSTND,*)' RESETTING DIA0,D10= ',DIA0,D10
  end if

  ! THIS SECTION OF CODE COMPUTES MORTALITY IN TERMS OF THE RELATIONSHIP
  ! T = A*(D**B)  WHERE T IS THE TOTAL NUMBER OF TREES AND D IS THE
  ! QUADRATIC MEAN DIAMETER OR REINEKE DIAMATER. THIS IS THE REINEKE
  ! SDI RELATIONSHIP WHEN B = -1.605. ON A LN-LN SCALE THIS BECOMES
  ! LN(T) = A + B*LN(D). FOR THE ONTARIO VARIANT, WE USE PENNERS SDI
  ! MAXIMUM EQUATION SOLVED FOR LN(T) = A + B*LN(D)

  ! AT A SPECIFIED BREAKPOINT LN(DBH), THE MAXIMUM STAND DENSITY SWITCHES FROM
  ! BEING CONTROLLED BY THE SDI MAXIMUM EQUATION TO A MATURE STAND BOUNDARY
  ! (MSB) EQUATION. FOR THE ONTARIO VARIANT, PENNER GIVES EQUATIONS FOR THE
  ! MATURE STAND BOUNDARY FUNCTION. THE MAXIMUM SDI AND MSB EQUATIONS NEED
  ! TO BE SEAMLESSLY SPLINED AT THE BREAKPOINT LN(DBH). GARY DIXON ESTIMATED
  ! COEFFICIENTS TO APPROXIMATE PENNERS MSB FUNCTION THAT FIT THIS CRITERIA.

  ! DEFAULT LIMITS: LO = 55% MAX SDI (PMSDIL); UP = 85% MAX SDI (PMSDIU)
  ! THAT IS TO SAY, DENSITY RELATED MORTALITY WILL BE INVOKED WHEN STAND
  ! DENSITY REACHES THE LOWER LIMIT, AND WILL BE CAPPED AT THE UPPER LIMIT.
  ! THESE LIMITS ARE UNDER USER CONTROL USING THE SDIMAX KEYWORD.

  ! FROM RELATIONSHIPS BETWEEN LN(TREES PER ACRE) VS
  ! LN(DIAMETER), THE MAXIMUM LINE, 85 PERCENT OF MAXIMUM LINE, AND
  ! 55 PERCENT OF MAXIMUM LINE, ARE DETERMINED. WHEN STAND DENSITY IS BELOW
  ! THE LOWER THRESHOLD TREES ARE ONLY SUBJECT TO BACKGROUND MORTALITY. AS
  ! SUCH, TREES PER ACRE DECREASES ONLY SLIGHTLY UNTIL THE LOWER LIMIT IS
  ! REACHED. ONCE THE LOWER LIMIT IS REACHED TREES PER ACRE STARTS DECREASING
  ! ACCORDING TO AN INTERNALLY COMPUTED LINEAR FN (ON THE LN-LN SCALE) UNTIL
  ! IT REACHES THE UPPER LIMIT.  FROM THIS POINT ON, TREES PER ACRE FOLLOWS
  ! DOWN THE UPPER LIMIT WHICH IS BASED ON EITHER THE MAXIMUM SDI OR MSB
  ! RELATIONSHIPS DEPENDING ON STAND QMD AND SPECIES.  FOR STANDS WHERE THE
  ! INITIAL NUMBER OF TREES EXCEEDS THE UPPER LIMIT, THE NUMBER OF TREES IS
  ! DECREASED TO THE UPPER LIMIT THE FIRST CYCLE.

  ! THIS SECTION OF CODE WAS DEVELOPED BY GARY DIXON FMSC FT COLLINS, CO.
  ! DEFINITION OF VARIABLES IMPORTANT TO THIS SECTION -

  !     T      = TOTAL NUMBER OF TREES
  !     ISISP  = INDEX OF SPECIES WITH MAXIMUM BASAL AREA
  !     TMD0   = MAXIMUM NUMBER OF TREES FOR A GIVEN DQ0 OR DRO
  !     T85D0  = 85 PERCENT LEVEL OF TMD0
  !     T55D0  = 55 PERCENT LEVEL OF TMD0
  !     TMD10  = MAXIMUM NUMBER OF TREES FOR A GIVEN DQ10 OR DR10
  !     T85D10 = 85 PERCENT LEVEL OF TMD10
  !     T55D10 = 55 PERCENT LEVEL OF TMD10
  !     D55M   = DIAMETER VALUE CORRESPONDING TO LN(T) ON THE 55 PERCENT L
  !     D85M   = DIAMETER VALUE CORRESPONDING TO THE POINT WHERE THE LINEA
  !              INTERSECTS THE 85 PERCENT LINE
  !     SLPMRT = SLOPE COEFFICIENT FOR THE LINEAR FN
  !     CEPMRT = INTERCEPT COEFFICIENT FOR THE LINEAR FN
  !     KNT    = COUNTER FOR NUMBER OF ITERATIONS IN DETERMINING COEFFICIE
  !     TREEIT = NUMBER OF TREES VALUE USED IN ITERATION PROCESS
  !     TEM    = TEMPORARY STORAGE VARIABLE
  !     IPATH  = PATH THROUGH LINEAR FN COEFFICIENT SECTION -
  !              1 IF ITERATIVE DETERMINATION
  !              2 IF STRAIGHT COMPUTATION
  !     TMMSB  = MAXIMUM NUMBER OF TREES FOR A GIVEN DQ10 OR DR10
  !              ACCORDING TO THE MATURE STAND BOUNDARY FUNCTION
  !     T85MSB = 85 PERCENT LEVEL OF TMMSB

  !  SDIMAX IS USED HERE TO CARRY WEIGHTED SDI MAXIMUM
  !  THIS IS COMPUTED USING REINEKES EQUATIONS AND IS ONLY USED
  !  IN THIS VARIANT FOR THE CLIMATE CHANGE CHECK.

  call SDICAL(0,SDIMAX)
  CONST = SDIMAX / (10**(-1.605))
  if (DEBUG)write(JOSTND,*)' SDIMAX,CONST,BAMAX= ',&
       &SDIMAX,CONST,BAMAX

  ! IF SDIMAX IS LESS THAN 5, ASSUME CLIMATE HAS CHANGED ENOUGH THAT THE
  ! SITE WILL NO LONGER SUPPORT TREES, AND KILL ALL EXISTING TREES.

  if (SDIMAX .lt.5) then
     TN10 = 0.
     go to 271
  end if

  IPATH = 0
  if (T .gt. TPHMAX) T = TPHMAX

  !     GIVEN DQ0, DETERMINE MAXIMUM TREES, 85 PERCENT LEVEL AND
  !     55 PERCENT LEVEL

  INDX = MSB_MAP(INDX_BA)
  SDIINT = -SDI_INT(INDX)/SDI_SLP(INDX)
  SDISLP = 1.0 /SDI_SLP(INDX)
  CONST = EXP(SDIINT)
  if (DEBUG)write(JOSTND,*)' INDX,SDIINT,SDISLP,CONST= ',&
       &INDX,SDIINT,SDISLP,CONST
  TMD0 = ACRtoHA*EXP(SDIINT+SDISLP*ALOG(DIA0*INtoCM))
  if (TMD0.gt. TPHMAX) TMD0 = TPHMAX
  T55D0 = TMD0 * PMSDIL
  T85D0 = TMD0 * PMSDIU
  if (DEBUG)write(JOSTND,*)' INDX_BA,INDX,DIA0,D10,CONST= ',&
       &INDX_BA,INDX,DIA0,D10,CONST
  if (DEBUG)write(JOSTND,*)' TMD0,PMSDIU,PMSDIL,T85D0,T55D0= ',&
       &TMD0,PMSDIU,PMSDIL,T85D0,T55D0

  !     GIVEN DQ10 OR DR10 DETERMINE MAXIMUM TREES, 85 PERCENT LEVEL
  !     AND 55 PERCENT L

  TMD10 = ACRtoHA*EXP(SDIINT+SDISLP*ALOG(D10*INtoCM))
  if (TMD10.gt. TPHMAX) TMD10 = TPHMAX
  T55D10 = TMD10 * PMSDIL
  T85D10 = TMD10 * PMSDIU
  if (DEBUG)write(JOSTND,*)'TMD10,PMSDIU,PMSDIL,T85D10,T55D10=',&
       &TMD10,PMSDIU,PMSDIL,T85D10,T55D10
  if (DEBUG)write(JOSTND,9040)ICYC,SDIMAX
  9040 format(' IN MORTS ICYC,MAX SDI =',I5,F10.1)

  ! IF A USER-SPECIFIED (MORTMSB KEYWORD) MSB IS IN EFFECT,
  ! THEN COMPUTE THE MSB EQUATION INTERCEPT.
  ! ASSUMES QMDMSB IS IN INCHES AT THIS POINT
  ! SLPMSB IS CARRIED IN COMMON AND IS THE USER-ENTERED SLOPE
  ! SDISLP IS THE SLOPE OF PENNERS SDI EQN SOLVED LN(T)=A+B*LN(QMDcm)
  ! CONST IS THE CONSTANT OF PENNERS SDI EQN T=C*QMDcm**(-D)

  if (SLPMSB .ne.0.) then
     TEMP = INtoCM*QMDMSB
     CEPMSB = ALOG(CONST*(TEMP**(SDISLP)))-SLPMSB*ALOG(TEMP)
  end if
  if (DEBUG)write(JOSTND,*)' MATURE STAND BOUNDARY SETTINGS, QMDMSB,'&
       &,'CEPMSB,SLPMSB,D10= ',QMDMSB,CEPMSB,SLPMSB,D10

  !     IF TOTAL NUMBER OF TREES EXCEEDS 85 PERCENT OF THE COMPUTED MAXIMUM,
  !     KILL BACK TO 85 PERCENT LEVEL.

  if (T .le. T85D0) go to 210
  TN10 = T85D10
  go to 270
  210 continue

  !     IF TOTAL NUMBER OF TREES IS BETWEEN 55 PERCENT AND 85 PERCENT, FIND
  !     LINEAR FN COEFFICIENTS (ITERATIVE METHOD) AND KILL ACCORDING TO
  !     LINEAR FN.

  if (T .le. T55D0) go to 240

  !       SPECIAL CASE WHERE T IS CLOSE TO THE 85% LINE AT DIA0

  if (ABS(T85D0-T).le.5.) then
     TN10 = T85D10
     go to 270
  end if

  KNT = 1
  TREEIT = T + 0.1 * T
  IPATH = 1
  220 continue
  TEM = TREEIT
  if (IPATH .eq.2) TEM = T
  if (DEBUG)write(JOSTND,*)' MORTS 220,TEM,CONST',TEM,CONST
  TEMP = TEM*HAtoACR
  D55M = (ALOG(TEMP) - ALOG(PMSDIL*CONST)) / (SDISLP)
  T55M = ALOG(TEMP)
  D85M = D55M * 1.25
  if (DEBUG)write(JOSTND,*)' D55M,T55M,D85M= ',D55M,T55M,D85M
  221 if (D85M .gt.5.0) D85M = 5.0
  if (D85M .lt.0.125)D85M = 0.125
  T85M = ALOG(CONST * (EXP(D85M) ** (SDISLP)) * PMSDIU)
  SLP = (T85M-T55M) / (D85M - D55M)
  if (DEBUG)write(JOSTND,*)' D55M,D85M,T55M,T85M,SLP= ',&
       &D55M,D85M,T55M,T85M,SLP
  if (SLP .gt. -0.5.and. D85M .lt.5.0) then
     D85M = D85M + .1
     go to 221
  end if
  CEPT = T55M - SLP * D55M
  if (T .le. T55D0) go to 230
  if (DEBUG)write(JOSTND,*)' MORTS,359,DIA0',DIA0
  TPRIME = CEPT + SLP * ALOG(DIA0*INtoCM)
  DIFF = T - EXP(TPRIME)*ACRtoHA
  if (DEBUG)write(JOSTND,9050) DIA0,D10,T,TREEIT,TEM,D55M,&
       &T55M,D85M,T85M,SLP,CEPT,TPRIME,DIFF,KNT
  9050 format(1H0,'MORTS 9050',13F9.3,I4)
  if (DIFF .le.5.0.and. DIFF .ge. -5.0) go to 230
  TREEIT = TREEIT + 0.5 * DIFF
  KNT = KNT + 1
  if (KNT .le.100) go to 220
  230 continue
  if (SLPMRT .eq.0.) SLPMRT = SLP
  if (CEPMRT .eq.0.) CEPMRT = CEPT
  if (DEBUG)write(JOSTND,*)' D10,CEPMRT,SLPMRT= ',D10,CEPMRT,SLPMRT
  TEM = ALOG(D10*INtoCM)
  TN10 = CEPMRT + SLPMRT*TEM
  TN10 = EXP(TN10)*ACRtoHA
  if (TN10.ge. T85D10)  TN10 = T85D10
  go to 270
  240 continue

  ! IF TOTAL NUMBER OF TREES IS LESS THAN 55 PERCENT AT DIA0 BUT GREATER TH
  ! 55 PERCENT AT D10, FIND LINEAR FN (STRAIGHT COMPUTATION) AND KILL
  ! ACCORDING TO LINEAR FN.
  ! IF TOTAL NUMBER OF TREES IS LESS THAN 55 PERCENT AT DIA0 AND D10,
  ! HOLD NUMBER OF TREES CONSTANT.

  if (T .le. T55D10) then
     TN10 = T
     go to 270
  else
     IPATH = 2
     go to 220
  end if
  270 continue

  if (DEBUG)write(JOSTND,9060)DIA0,D10,T,TN10,TREEIT,&
       &CONST,KNT
  9060 format(1H0,'MORTS 9060',6(F10.3,3X),I4)

  !   BOUND TN10 (THE NUMBER OF TREES REMAINING IN THE STAND AFTER MORTALITY)
  !   IF TN10 IS GREATER THAN T, SET TN10 = T
  !   IF TN10 IS SMALL, JUST KILL ALL THE TREES

  271 continue
  if (TN10.gt. T)TN10 = T
  if (TN10.lt.0.1)TN10 = 0.

  !     RN IS SDI-DRIVEN ANNUAL MORTALITY
  !     PREDICTED AT THE STAND LEVEL

  RN = 1.0-(1.0-((T-TN10)/T))**(1./FINT)

  if (DEBUG) write(JOSTND,*) 'TESTMORTS, RN=',RN,&
       &'T=',T,'TN10=',TN10

  !  START LOOP TO ESTIMATE SDI BASED MORTALITY RATE.
  !  TREES ARE PROCESSED ONE AT A TIME WITHIN A SPECIES.

  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (I1.le.0) go to 50
     I2 = ISCT(ISPC,2)
     XMORT = XMMULT(ISPC)
     D1 = XMDIA1(ISPC)
     D2 = XMDIA2(ISPC)
     B0 = PMSC(BKG_MAP(ISPC))
     B1 = PMD(BKG_MAP(ISPC))

     !  START TREE LOOP WITHIN SPECIES.
     do I3 = I1,I2

        !  INITIALIZE FOR NEXT TREE.

        I = IND1(I3)
        P = PROB(I)
        WKI = 0.0
        WK2(I) = 0.0
        if (P.le.0.0) go to 40
        D = DBH(I)

        !     COMPUTE BACKGROUND MORTALITY RATE RI

        RI = (1.0/(1.0+EXP(B0+B1*D)))

        ! TEST RUNS SHOW BACKGROUND MORTALITY RATE IS HIGH, CUT IT IN HALF.
        RI = 0.5 * RI

        ! MERGE ESTIMATES OF RI AND RN
        RIP = RI

        ! IF SDI NOT IN EFFECT YET SET RIP TO BACKGROUND MORTALITY RATE.
        ! OTHERWISE SET TO SDI MORTALITY RATE.

        RIP = RN
        TEM = CONST*((D10*INtoCM)**(SDISLP))
        if (TEM .gt. TPHMAX)TEM = TPHMAX
        TEM = TEM*PMSDIL*ACRtoHA
        if (T .le. TEM .or. RN .le.0.0)RIP = RI
        if (RIP.gt.1.0) RIP = 1.0
        X = 1.0
        if (D .ge. D1.and. D .lt. D2)X = XMORT

        !     APPLY MORTALITY MULTIPLIER ONLY TO BACKGROUND RATE

        if (RIP .eq. RN) X = 1.0
        WKI = P*(1.0-(1.0-RIP)**FINT)*X
        if (WKI.gt.P) WKI = P
        if (DEBUG) write(JOSTND,9070) I,D,RI,RN,RIP
        9070 format(' MORTALITY RATE ESTIMATES FOR TREE ',I4,', DBH = ',F6.2/&
             &'  RI = ',F7.5,' RN = ',F7.5,' RIP = ',F7.5)
        WK2(I) = WKI

        !  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.

        if (DEBUG) then
           PRES = P-WKI
           VLOS = WKI*CFV(I)/FINT
           write(JOSTND,9080) I,ISPC,D,P,WKI,PRES,VLOS
           9080 format(' IN MORTS, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,&
                &',  INIT PROB=',F7.3,&
                &',  TREES DYING=',F7.3,'RES PROB=',F7.3,&
                &',  VOL LOST=',F9.3)
        end if
        40 continue
     end do

     !  END OF SPECIES LOOP.  PRINT DEBUG INFO IF DESIRED.

     50 continue
  end do

  ! DISTRIBUTE MORTALITY BY VARIANT SPECIFIC METHOD
  ! IF ALL TREES ARE BEING KILLED, WE DONT NEED TO DO THIS

  SUMTRE = 0.0
  if (RIP .eq. RN)SUMTRE = T-TN10
  if (SUMTRE .lt.0.0)SUMTRE = 0.0
  if (TN10.ge.0.1) then
     call VARMRT(SUMTRE,DEBUG,SUMKIL,D10)
  end if

  ! ESTIMATE NEW QUADRATIC MEAN DIAMETER AND SEE HOW BAD OUR
  ! INITIAL ESTIMATE WAS.

  TN = 0.0
  SD2SQN = 0.0
  SUMDR10N = 0.
  DR10N = 0.
  IPASS = IPASS+1
  do I = 1,ITRN
     P = PROB(I)-WK2(I)
     IS = ISP(I)
     D = DBH(I)
     if (D.lt.DBHSDI)go to 30 ! BRANCH IF D IS LT USER'S DBHSDI
     BARK = BRATIO(IS,D,HT(I))
     G = (DG(I)/BARK) * (FINT/10.0)
     CIOBDS = (2.0*D*G+G*G)
     SD2SQN = SD2SQN+P*(D*D+CIOBDS)
     if (LZEIDE) then
        SUMDR10N = SUMDR10N+P*(D+G)**(1.605)
     end if
     TN = TN+P
     30 continue
  end do
  if (TN .eq.0.0)go to 35
  DQ10N = SQRT(SD2SQN/TN)
  if (LZEIDE) then
     DR10N = (SUMDR10N/TN)**(1./1.605)
  end if
  if (DEBUG)write(JOSTND,*)' MORTS CHECK DIA. IPASS,DQ10,DR10,',&
       &'DQ10N,DR10N= ',IPASS,DQ10,DR10,DQ10N,DR10N
  if (LZEIDE) then
     D10N = DR10N
  else
     D10N = DQ10N
  end if
  if (IPASS .eq.10)go to 35
  DIFF = ABS(D10-D10N)
  if (DEBUG)write(JOSTND,*)' DIFFS. DQ10,D10,D10N',&
       &'DIFF= ',DQ10,D10,DIFF,D10N

  if (DIFF .gt.0.2) then

     ! SOMETIMES SELECTIVE KILLING CAN DECREASE QMD.  IF SO, TAKE
     ! CALCULATED MORTALITY AND RECALIBRATE NEXT CYCLE.
     if (DEBUG)write(JOSTND,*)' D10,DIA0,DQ10,D10N= ',&
          &D10,DIA0,DQ10,D10N
     if (D10N .le. DIA0) then
        IPATH = 0
        go to 35
     end if
     D10 = D10N
     go to 10
  end if
  35 continue

  ! IF ALTERNATE MORTALITY IS IN EFFECT, THEN COMPUTE NECESSARY PARAMETERS
  ! AND KILL ADDITIONAL TREES TO SIMULATE STAND BREAK-UP. QMD WILL CHANGE
  ! SO SET FLAG TO RECALIBRATE NEXT CYCLE.

  ! IF USER HAS ENTERED THEIR OWN MSB RELATIONSHIP USING THE MORTMSB KEYWORD,
  ! THEN USE IT; USE GARY DIXON'S APPROXIMATION TO MARGARET PENNER'S EQN 10
  ! AS THE DEFALUT MSB LINE.

  TMMSB = 0.
  T85MSB = 0.
  if (SLPMSB .ne.0.) then
     TEMINT = CEPMSB
     TEMSLP = SLPMSB
     TEMQMD = ALOG(INtoCM*QMDMSB)
  else
     TEMINT = MSB_INT(INDX)
     TEMSLP = MSB_SLP(INDX)
     TEMQMD = MSB_DBH(INDX)
  end if
  TEMP = ALOG(INtoCM*D10)
  if (DEBUG)write(JOSTND,*)' TEMP,TEMQMD,TN= ',TEMP,TEMQMD,TN
  if (TEMP.gt.TEMQMD .and. TN.gt.0.) then
     TMMSB = EXP(TEMINT+TEMSLP*TEMP)
     if (DEBUG)write(JOSTND,*)' TEMINT,TEMSLP,TEMP,TMMSB= ',&
          &TEMINT,TEMSLP,TEMP,TMMSB
     T85MSB = TMMSB*PMSDIU
     if (DEBUG)write(JOSTND,*)' TMMSB,PMSDIU,T85MSB= ',&
          &TMMSB,PMSDIU,T85MSB
     TMORE = TN-T85MSB*ACRtoHA
     if (DEBUG)write(JOSTND,*)' TN,T85MSB,ACRtoHA,TMORE= ',&
          &TN,T85MSB,ACRtoHA,TMORE
     if (TMORE .lt.0.)TMORE = 0.
     if (DEBUG)write(JOSTND,*)' ALTERNATE MORTALITY LOGIC, D10,TN,',&
          &'TEMQMD,TEMINT,TEMSLP,TMMSB,T85MSB,PMSDIU,TMORE= '
     if (DEBUG)write(JOSTND,*)&
          &D10,TN,TEMQMD,TEMINT,TEMSLP,TMMSB,T85MSB,PMSDIU,TMORE

     ! MAKE SURE MSB EFFICIENCY IS SET HIGH ENOUGH; SINCE MORTALITY IS
     ! CONCENTRATED IN A DBH RANGE, FIRST COMPUTE THE TPA LEFT IN THE DBH
     ! RANGE AFTER DENSITY RELATED MORTALITY HAS BEEN ACCOUNTED FOR.
     ! CALL SUBROUTINE MSBMRT TO KILL TMORE TREES AND SET RECALIBRATE FLAG.

     ! IF THERE ARENT ENOUGH TREES LEFT IN THE CLASS TO ACHIEVE THE
     ! ALTERNATE MORTALITY LEVEL EVEN IF THEY ARE ALL KILLED, THEN CANCEL
     ! THE ALTERNATE MORTALITY LOGIC; IF THE EFFICIENCY IS SET TO LOW TO
     ! ACHIEVE THE ALTERNATE MORTALITY LEVEL THEN RECOMPUTE IT AND CONTINUE
     ! WITH THE LOGIC.

     TPACLS = 0.
     do I = 1,ITRN
        BARK = BRATIO(ISP(I),DBH(I),HT(I))
        DBHEND = DBH(I)+(DG(I)/BARK)*(FINT/10.0)
        if (DBHEND.ge.DLOMSB .and. DBHEND.lt.DHIMSB) then
           TPACLS = TPACLS+PROB(I)-WK2(I)
        end if
     end do
     if (DEBUG)write(JOSTND,*)' ALT MORT LOGIC DLOMSB,DHIMSB,TPACLS',&
          &' = ',DLOMSB,DHIMSB,TPACLS

     if (TMORE .gt. TPACLS) then
        write(JOSTND,351)TPACLS,TMORE
        351 format(/,2(' ***************'/),' WARNING: FOR ALTERNATE ',&
             &'MORTALITY, TPA IN DBH CLASS OF ',F8.1,&
             &' TREES/ACRE IS LESS THAN THE ADDITIONAL MORTALITY TPA',&
             &/,10X,' OF ',F8.1,' TREES/ACRE.  '&
             &'ALTERNATE MORTALITY CANCELLED.',/2(' ***************'/),/)
        go to 353
     end if

     TEMEFF = EFFMSB
     if (MFLMSB .eq.3) then
        TEMEFF = TMORE/TPACLS
     else
        if (TPACLS*TEMEFF .lt. TMORE) then
           TEMEFF = TMORE/TPACLS
           write(JOSTND,352)EFFMSB,TEMEFF
           352 format(/,2(' ***************'/),' WARNING: FOR ALTERNATE ',&
                &'MORTALITY, MORTALITY EFFICIENCY OF ',F8.4,&
                &' IS TOO LOW TO REACH THE ADDITIONAL MORTALITY LEVEL. ',&
                &/,10X,'MORTALITY EFFICIENCY RESET TO ',F8.4,&
                &' FOR FURTHER PROCESSING.',&
                &/2(' ***************'/),/)
        end if
     end if
     call MSBMRT(TEMEFF,TMORE,DLOMSB,DHIMSB,MFLMSB,DEBUG)
     IPATH = 0
  end if
  353 continue

  !  LOOP THROUGH TREES AND CHECK FOR SIZE (aka AGE) CAP RESTRICTIONS

  do I = 1,ITRN
     IS = ISP(I)
     D = DBH(I)
     P = PROB(I)
     BARK = BRATIO(IS,D,HT(I))
     G = (DG(I)/BARK) * (FINT/10.0)
     IDMFLG = IFIX(SIZCAP(IS,3))
     if ((D+G).ge.SIZCAP(IS,1) .and. IDMFLG.ne.1) then
        WK2(I) = AMAX1(WK2(I),(P*SIZCAP(IS,2)*FINT/10.0))
        if (WK2(I) .gt. P)WK2(I) = P
        if (DEBUG)write(JOSTND,*)' SIZE CAP RESTRICTION IMPOSED, ',&
             &'I,IS,D,P,SIZCAP 1-3,WK2 = ',&
             &I,IS,D,P,SIZCAP(IS,1),SIZCAP(IS,2),SIZCAP(IS,3),WK2(I)
     end if
     354 continue
  end do

  !  CHECK TO SEE IF BA IS STILL WITHIN LIMITS. IF OUT OF BOUNDS,
  !  ADJUST THE MORTALITY VALUES PROPORTIONATELY ACROSS ALL TREE RECORDS.

  KNT2 = 0
  9001 continue
  TNEW = 0.
  BANEW = 0.
  QMDNEW = 0.
  BADEAD = 0.
  do I = 1,ITRN
     P = PROB(I)-WK2(I)
     D = DBH(I)
     BARK = BRATIO(ISP(I),D,HT(I))
     G = (DG(I)/BARK) * (FINT/10.0)
     TNEW = TNEW+P
     BANEW = BANEW+(0.0054542*(D+G)**2.)*P
     BADEAD = BADEAD+(0.0054542*(D+G)**2.)*WK2(I)
     QMDNEW = QMDNEW + ((D+G)**2.)*P
     if (DEBUG)write(JOSTND,*)' I,P,D,G,TNEW,BANEW,QMDNEW,BADEAD= ',&
          &I,P,D,G,TNEW,BANEW,QMDNEW,BADEAD
     36 continue
  end do
  if (TNEW .gt.0.) then
     QMDNEW = SQRT(QMDNEW/TNEW)
  else
     QMDNEW = 0.
  end if

  if ((BANEW-BAMAX) .gt.1.) then

     !       CALCULATE ADJUSTMENT FACTOR NEEDED TO GET RESIDUAL BA WITHIN
     !       THE BA MAXIMUM LIMIT. INCREASE IT BY 10% AND PLACE A LOWER
     !       BOUND ON THE FACTOR TO SPEED UP ITERATION.

     ADJFAC = ((BANEW-BAMAX)/BADEAD)
     if (DEBUG)write(JOSTND,*)' BANEW,TBAM1,ADJFAC= ',&
          &BANEW,TBAM1,ADJFAC

     !       LOOP THROUGH THE TREE LIST AND ADJUST THE MORTALITY VALUES.

     TNEW = 0.
     do I = 1,ITRN
        P = PROB(I)
        WKI = WK2(I)*(1+ADJFAC)
        if (WKI.gt.P) WKI = P
        WK2(I) = WKI
        if (DEBUG)write(JOSTND,*)' ADJUSTING FOR BAMAX I,P,WKI= ',&
             &I,P,WKI

        !       PRINT DEBUG INFO IF DESIRED.

        TNEW = TNEW+P-WKI
        if (.not.DEBUG) go to 1500
        PRES = P-WKI
        VLOS = WKI*CFV(I)/FINT
        write(JOSTND,9080) I,ISPC,D,P,WKI,PRES,VLOS
        1500 continue
     end do

     ! LOOP BACK AND SEE IF THE BAMAX TARGET HAS BEEN ACHIEVED YET.
     ! (I.E. IF THE COMPUTED MORTALITY RATE EXCEEDED THE PROB, AND WE
     !  HAD TO LIMIT MORTALITY TO THE PROB VALUE FOR SOME TREE RECORDS,
     !  THEN WE MAY NOT REACH THE BA LIMIT IN ONE PASS.)

     KNT2 = KNT2+1
     if (KNT2.lt.100)go to 9001

     IPATH = 0
     if (DEBUG)write(JOSTND,*)' AFTER BA ADJUSTMENT RESIDUAL TPA = ',&
          &TNEW
  end if
  TPAMRT = TNEW
  45 continue

  !  COMPUTE THE FIXMORT OPTION.  LOOP OVER ALL SCHEDULED FIXMORTS
  !  LINCL IS USED TO INDICATE WHETHER A TREE GETS AFFECTED OR NOT

  call OPFIND (1,MYACTS(2),NTODO)
  if (NTODO.gt.0) then
     if (DEBUG)write(JOSTND,*)' FIXMORT PROCESSING, ITODO= ',ITODO
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
              if (DEBUG)write(JOSTND,*)' KPOINT,KBIG,ITRN,XMORE= ',&
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
              if (DEBUG)write(JOSTND,*)' DBH= ',(DBH(IG),IG = 1,ITRN)
              if (DEBUG)write(JOSTND,*)' IWORK1= ',(IWORK1(IG),IG = 1,ITRN)
              if (DEBUG)write(JOSTND,*)' WK2= ',(WK2(IG),IG = 1,ITRN)

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
           if (DEBUG)write(JOSTND,*)' ITODO,WK2= ',&
                &ITODO,(WK2(IG),IG = 1,ITRN)
        end if
        300 continue
     end do
  end if
  return

  entry MORCON

  !  ENTRY POINT FOR LOADING MORTALITY MODEL CONSTANTS THAT
  !  REQUIRE ONE-TIME RESOLUTION.

  CEPMRT = 0.
  SLPMRT = 0.
  TPAMRT = 0.
  return
end
