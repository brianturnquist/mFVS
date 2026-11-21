!============================!
!   Compute tree mortality   !
!============================!
! VBASE $Id$
subroutine MORTS
  use VARCFG_MOD, only: CFG_MAXSP
  implicit none

  !  THIS SUBROUTINE COMPUTES PERIODIC MORTALITY RATES FOR EACH TREE
  !  RECORD AND THEN REDUCES THE NUMBER OF TREES/ACRE REPRESENTED BY THE
  !  TREE RECORD.  IT IS CALLED FROM **TREGRO** WHEN CYCLING FOR GROWTH
  !  PREDICTION.  ENTRY **MORCON** IS CALLED TO LOAD SITE DEPENDENT
  !  CONSTANTS.

  !  SHARED BY 23 BUILT VARIANTS (AK RETAINS ITS NATIVE MORTS).
  !  VARIANTCD SELECTS ONE FAMILY KERNEL AT COMPILE TIME:
  !    MORSDI -- SDI-BASED MORTALITY (GARY DIXON): BM, CA, CR, CS, EC, EM,
  !              LS, NC, NE, OC, SN, SO, TT, UT AND WS.  EM AND TT USE THE
  !              PROGNOSIS EQUATION (PRGTRE) FOR SOME SPECIES.
  !    MORPRG -- PROGNOSIS MORTALITY (HAMILTON): CI, IE AND KT.
  !    MORPAC -- ORGANON-BASED EQUATIONS: OP, PN AND WC.
  !    MORBC, MORON -- THE BC AND ON MODELS, MOVED UNCHANGED.
  !  ALL SHARE THE FIXMORT PROCESSING IN FIXMRT, AND ALL BUT BC SHARE
  !  THE MORTMULT PROCESSING IN MMULT.  WORKING VARIABLES THAT THE
  !  KERNELS SHARE WITH THESE HELPERS ARE DECLARED HERE, SO THEY BEHAVE AS
  !  THE LOCALS OF THE FORMER SINGLE ROUTINES DID.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CALCOM.f90'
  include 'CONTRL.f90'
  include 'COEFFS.f90'
  include 'ESTREE.f90'
  include 'MULTCM.f90'
  include 'OUTCOM.f90'
  include 'PDEN.f90'
  include 'PLOT.f90'
  include 'VARCOM.f90'
  include 'WORKCM.f90'
  include 'ORGANON.f90'
  include 'METRIC.f90'
  include 'BCPLOT.f90'

  ! Variables shared by the kernels and helpers
  logical :: DEBUG
  integer :: MYACTS(2),I,ISPC,IP,KSPC
  real    :: PRM(6),D,BARK,G,DGT,RELDBH,RIP,POTENT,RIPP,X,XCHECK
  real    :: D1,D2,XMORT,AVED,RZ,BA10,TB,TTB,DSUM,WPROB,T,DELTBA
  real    :: B0,B1,B2,B5,BRATIO
  logical :: LCON

  ! Data statements
  data MYACTS/94,97/

  LCON = .false.
  go to 1

  entry MORCON

  !  ENTRY POINT FOR LOADING MORTALITY MODEL CONSTANTS THAT REQUIRE
  !  ONE-TIME RESOLUTION.

  LCON = .true.
  1 continue
  if (VARIANTCD.eq.'BC') call MORBC(LCON)
  if (VARIANTCD.eq.'ON') call MORON(LCON)
  if (VARIANTCD.eq.'CI'.or.VARIANTCD.eq.'IE'.or.VARIANTCD.eq.'KT')&
       &call MORPRG(LCON)
  if (VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'WC')&
       &call MORPAC(LCON)
  if (VARIANTCD.eq.'BM'.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'CR'.or.&
       &VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'EM'.or.&
       &VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'NE'.or.&
       &VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'SN'.or.VARIANTCD.eq.'SO'.or.&
       &VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT'.or.VARIANTCD.eq.'WS')&
       &call MORSDI(LCON)
  if (.not.LCON) call FIXMRT
  return

contains

  subroutine MORSDI(LCON)
    implicit none

    !  SDI-BASED MORTALITY.  SDI BASED MORTALITY IS ALWAYS USED. ADDITIONAL
    !  MORTALITY OCCURS WHEN STAND BA EXCEEDS THE SPECIFIED MAXIMUM BA FOR
    !  THE SITE.  IF NOT SET BY THE USER, BAMAX IS DETERMINED FROM MAX SDI
    !  AT 10 INCH DBH.

    !  VARIANT DIFFERENCES KEPT FROM THE FORMER NATIVE COPIES:
    !    - BACKGROUND RATE COEFFICIENTS (MORCOF): BY SPECIES, OR THROUGH A
    !      SPECIES MAP IN CA/OC, CR AND CS/LS/NE.  EM ADDS A DBH**2 TERM.
    !    - CA, OC, CS, LS AND NE INCLUDE TREES FROM DBHSDI IN THE QMD
    !      ESTIMATES; THE OTHERS USE THE ZEIDE/STAGE DBH LIMITS.
    !    - NC, OC AND SN SCALE DG BY FINT/5 (DGPER); THE OTHERS BY FINT/10.
    !    - SN DOES NOT HALVE THE BACKGROUND RATE.  CA/OC (RW, GS), WS (RW,
    !      GS) AND NC (RW) FLOOR IT AT 0.0001.
    !    - EM (SPECIES OTHER THAN 1-3, 7-10, 18) AND TT (SPECIES 10) USE THE
    !      PROGNOSIS EQUATION.  OC USES ORGANON MORTALITY WHEN AVAILABLE.
    !    - THE VARIANT-SPECIFIC DISTRIBUTION ROUTINE (EM HAS NONE); SN, CS,
    !      LS AND NE DO NOT CALL CLMORTS.  DIAGNOSTIC FORMATS DIFFER.

    !  DEFINITIONS:

    !     PMSC -- CONSTANT TERMS FOR EACH SPECIES FOR THE BACKGROUND
    !             MORTALITY
    !             RATE EQUATION (B0).
    !      PMD -- COEFFICIENTS FOR EACH SPECIES FOR THE DIAMETER TERM
    !             IN THE BACKGROUND MORTALITY RATE EQUATION (B1).
    !    PMDSQ -- COEFFICIENTS FOR EACH SPECIES FOR THE DIAMETER SQUARED
    !             TERM IN THE BACKGROUND MORTALITY RATE EQUATION (B2).
    !       C0 -- CONSTANT TERM FOR SDI RELATIONSHIP
    !             LN(T) = LN(C0)-1.605 *LN(QMD)
    !        I -- TREE SUBSCRIPT.
    !        D -- TREE DIAMETER.
    !       RI -- ESTIMATED ANNUAL BACKGROUND MORTALITY RATE BASED ON
    !             HAMILTONS
    !       RN -- MORTALITY RATE THAT WILL MATCH THE TREND IN TREES PER
    !             ACRE THAT IS PREDICTED FROM THE SDI RELATIONSHIP
    !      RIP -- WEIGHTED AVERAGE MORTALITY RATE BASED ON TREE DBH, RI,
    !             AND RN.
    !        P -- NUMBER OF TREES PER ACRE REPRESENTED BY A TREE.
    !      WKI -- SCALAR USED FOR CALCULATION AND TEMPORARY STORAGE
    !             OF TREE MORTALITY (TREES/ACRE).

    logical :: LCON,LVLS,LDBHSD,LPRG
    integer :: IDMFLG,KNT,IPASS,I1,I2,I3,IS,IPATH,KNT2
    real    :: DQ0,SUMKIL,SD2SQ,P,CIOBDS,DQ10,CONST,TMD0,TNEW,BANEW,QMDNEW
    real    :: ADJFAC,T85D0,T55D0,TMD10,T85D10,T55D10,TN10,TEM,D55M,T55M
    real    :: D85M,T85M,SLP,CEPT,TPRIME,DIFF,RN,WKI,RI,PRES,VLOS,SUMTRE
    real    :: TN,SD2SQN,DQ10N,TREEIT,TMMSB,T85MSB,TMORE,TEMEFF,TPACLS
    real    :: DBHEND,BADEAD,DIA0,D10,DR0,DR10,SUMDR0,SUMDR10,SUMDR10N
    real    :: DR10N,D10N,SDQ0,SMORMT,DGPER,XMLT

    if (LCON) then
       if (VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'TT') call PRGCON
       CEPMRT = 0.
       SLPMRT = 0.
       TPAMRT = 0.
       return
    end if

    LVLS = VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'NE'
    LDBHSD = LVLS.or.VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'OC'
    DGPER = 10.0
    if (VARIANTCD.eq.'NC'.or.VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'SN')&
         &DGPER = 5.0

    ! Check whether debugging is enabled
    call DBCHK (DEBUG,'MORTS',5,ICYC)
    if (DEBUG)write(JOSTND,9000)ICYC
    9000 format('ENTERING SUBROUTINE MORTS  CYCLE =',I4)

    !  INITIALIZATIONS

    TREEIT = 0.
    KNT = 0

    !  PROCESS MORTMULT KEYWORD.

    call MMULT(VARIANTCD.eq.'EM'.or.VARIANTCD.eq.'TT')
    if (DEBUG) then
       if (VARIANTCD.eq.'SN') then
          write(JOSTND,9011)ICYC,RMSQD
       else if (LVLS) then
          write(JOSTND,9012)ICYC,RMSQD
       else
          write(JOSTND,9010)ICYC,RMSQD
       end if
    end if
    9010 format('IN MORTS 9010 ICYC,RMSQD= ',&
         &I5,5X,F6.2)
    9011 format('IN MORTS 9010 ICYC,RMSQD= ',I5,2F10.2)
    9012 format('IN MORTS 9010 ICYC,RMSQD= ',I5,5X,F10.5)

    if (RMSQD .eq.0.) then
       CEPMRT = 0.
       SLPMRT = 0.
    end if

    ! IF BARE GROUND PLANT, LIMITS WERE NOT ADJUSTED FROM A PERCENT TO A
    ! PROPORTION IN CRATET, ADJUST THEM HERE.

    if (PMSDIL .gt.1.0)PMSDIL = PMSDIL/100.
    if (PMSDIU .gt.1.0)PMSDIU = PMSDIU/100.

    ! INITIALIZE STARTING VALUES.
    ! DQ0, CALCULATED BELOW, IS THE SAME AS RMSQD THAT IS CALCULATED IN DENSE
    ! WHEN DBHSDI=0.

    IPASS = 0
    SUMKIL = 0.0

    !  ESTIMATE QUADRATIC MEAN DIAMETER 10 YEARS HENCE.

    T = 0.0
    DQ0 = 0.
    SDQ0 = 0.
    SD2SQ = 0.
    SUMDR0 = 0.
    SUMDR10 = 0.
    DR0 = 0.
    DR10 = 0.
    do ISPC = 1,CFG_MAXSP
       I1 = ISCT(ISPC,1)
       if (I1.le.0)go to 20
       I2 = ISCT(ISPC,2)
       do I3 = I1,I2
          I = IND1(I3)
          P = PROB(I)
          IS = ISP(I)
          WK2(I) = 0.0
          D = DBH(I)
          if (LDBHSD) then
             if (D.lt.DBHSDI)go to 12
          else
             if (LZEIDE.and.(D.lt.DBHZEIDE))go to 12 ! BRANCH IF D IS LT MIN DBH
             if (.not.LZEIDE.and.(D.lt.DBHSTAGE))go to 12
          end if
          BARK = BRATIO(IS,D,HT(I))
          G = (DG(I)/BARK) * (FINT/DGPER)
          CIOBDS = (2.0*D*G+G*G)
          SD2SQ = SD2SQ+P*(D*D+CIOBDS)
          SDQ0 = SDQ0+P*(D)**2
          if (LZEIDE) then
             SUMDR10 = SUMDR10+P*(D+G)**1.605
             SUMDR0 = SUMDR0+P*(D)**1.605
          end if
          T = T+P
          12 continue
       end do
       20 continue
    end do
    if (DEBUG) then
       if (VARIANTCD.eq.'SN') then
          write(JOSTND,*)'SDQ0,SD2SQ,SUMDR0,SUMDR10,T= ',&
               &SDQ0,SD2SQ,SUMDR0,SUMDR10,T
       else
          write(JOSTND,*)'SDQ0,SD2SQ,SUMDR0,SUMDR10= ',&
               &SDQ0,SD2SQ,SUMDR0,SUMDR10
       end if
    end if

    !  SOME EVENTS CAN CHANGE THE TRAJECTORY OF A STAND. FOR EXAMPLE,
    !  REGENERATION, THINNING, USER MORTALITY, FIRE, I&P EFFECTS.
    !  IF THE TRAJECTORY IS CHANGED,
    !  RESET SLOPE AND INTERCEPT FOR CALCULATION.
    !  (THE VALUE OF 1. IS TO ALLOW FOR ROUNDING ERROR)

    if (ICYC.gt.1.and. ABS(T-TPAMRT).gt.1.) then
       CEPMRT = 0.
       SLPMRT = 0.
       if (DEBUG)write(JOSTND,*)'RESETTING SLOPE,INTERCEPT T,TPAMRT= ',&
            &T,TPAMRT
    end if

    if (T .lt.1.0) go to 45
    DQ0 = SQRT(SDQ0/T)
    DQ10 = SQRT(SD2SQ/T)
    if (VARIANTCD.eq.'TT') then
       if (DEBUG)write(JOSTND,*)'TESTMORTS,249,T,ISISP',T,ISISP
    end if
    if (LZEIDE) then
       DR0 = (SUMDR0/T)**(1./1.605)
       DR10 = (SUMDR10/T)**(1./1.605)
    end if
    if (DEBUG)write(JOSTND,*)'IN MORTS, T,ISISP,LZEIDE= ',&
         &T,ISISP,LZEIDE
    if (LZEIDE) then
       DIA0 = DR0
       D10 = DR10
    else
       DIA0 = DQ0
       D10 = DQ10
    end if
    10 continue
    DELTBA = .005454154*D10*D10*T-BA

    if (DEBUG) then
       if (LVLS) then
          write(JOSTND,9021) DQ0,DQ10,T,SD2SQ,DR0,DR10
       else if (VARIANTCD.eq.'TT') then
          write(JOSTND,9022) DQ0,DQ10,T,SD2SQ,DR0,DR10
       else
          write(JOSTND,9020) DQ0,DQ10,T,SD2SQ,DR0,DR10
       end if
    end if
    9020 format('IN MORTS  DQ0 = ',F8.2,' DQ10 = ',F10.2,&
         &' T = ',F8.2,' SD2SQ = ',F10.2,' DR0= ',F10.2,' DR10= ',F10.2)
    9021 format('IN MORTS  DQ0 = ',F10.5,' DQ10 = ',F10.2,&
         &' T = ',F8.2,' SD2SQ = ',F10.2,' DR0= ',F10.2,' DR10= ',F10.2)
    9022 format('IN MORTS  DQ0 = ',F8.2,' DQ10 = ',F10.2,&
         &' T = ',F8.2,' SD2SQ = ',F10.2)

    ! TT PREPARES ITS PROGNOSIS TERMS HERE, BEFORE THE DIA0 RESET; EM AFTER IT.

    if (VARIANTCD.eq.'TT') call PRGAVE(D10)

    !  IF D IS TOO LOW, NUMERICAL PROBLEMS OCCUR. RESET IF NECESSARY.

    if (DIA0.lt.0.3) then
       D10 = 0.3 + D10 - DIA0
       DIA0 = 0.3
       if (DEBUG) write(JOSTND,*)'RESETTING DIA0,D10= ',DIA0,D10
    end if

    !  THIS SECTION OF CODE IS NEEDED FOR SPECIES USING EC VARIANT LOGIC:

    ! THIS SECTION OF CODE COMPUTES MORTALITY IN TERMS OF THE RELATIONSHIP
    ! T = CONSTANT*(D**-1.605)  WHERE T IS THE TOTAL NUMBER OF TREES
    ! AND D IS THE QUADRATIC MEAN DIAMETER OR REINEKE DIAMATER.

    ! DEFAULT LIMITS: LO = 55% MAX SDI (PMSDIL); UP = 85% MAX SDI (PMSDIU)

    ! FROM RELATIONSHIPS BETWEEN LN(TREES PER ACRE) VS
    ! LN(DIAMETER), THE MAXIMUM LINE, 85 PERCENT OF MAXIMUM
    ! LINE, AND 55 PERCENT OF MAXIMUM LINE, WERE DETERMINED.  TREES PER ACRE
    ! IS HELD CONSTANT AS LN(D) INCREASES UNTIL THE 55 PERCENT LINE IS
    ! REACHED.  THEN TREES PER ACRE STARTS DECREASING ACCORDING TO A COMPUTE
    ! LINEAR FN UNTIL IT REACHES THE 85 PERCENT LINE.  FROM THIS POINT ON,
    ! TREES PER ACRE FOLLOWS DOWN THE 85 PERCENT LINE.  FOR STANDS WHERE THE
    ! INITIAL NUMBER OF TREES EXCEEDS THE MAXIMUM LINE, NUMBER OF TREES IS
    ! DECREASED TO THE MAXIMUM LINE THE FIRST CYCLE, AND TO THE 85 PERCENT L
    ! THIS SECOND CYCLE.  FOR STANDS WITH AN INITIAL NUMBER OF TREES BETWEEN
    ! 85 PERCENT LINE AND MAXIMUM LINES, THE NUMBER OF TREES IS DECREASED
    ! TO THE 85 PERCENT LINE THE FIRST CYCLE.

    ! THIS SECTION OF CODE WAS DEVELOPED BY GARY DIXON FMSC FT COLLINS, CO.

    ! DEFINITION OF VARIABLES IMPORTANT TO THIS SECTION -

    !     T      = TOTAL NUMBER OF TREES
    !     ISISP = INDEX OF SPECIES WITH MAXIMUM BASAL AREA
    !     TMD0   = MAXIMUM NUMBER OF TREES FOR A GIVEN DQ0 OR DR0
    !     T85D0  = 85 PERCENT LEVEL OF TMD0
    !     T55D0  = 55 PERCENT LEVEL OF TMD0
    !     TMD10  = MAXIMUM NUMBER OF TREES FOR A GIVEN DQ10 OR DR10
    !     T85D10 = 85 PERCENT LEVEL OF TMD10
    !     T55D10 = 55 PERCENT LEVEL OF TMD10
    !     D55M   = DIAMETER VALUE CORRESPONDING TO LN(T) ON THE 55 PERCENT L
    !     D85M   = DIAMETER VALUE CORRESPONDING TO THE POINT WHERE THE LINEA
    !              INTERSECTS THE 85 PERCENT LINE
    !   SLPMRT   = SLOPE COEFFICIENT FOR THE LINEAR FN
    !   CEPMRT   = INTERCEPT COEFFICIENT FOR THE LINEAR FN
    !     KNT    = COUNTER FOR NUMBER OF ITERATIONS IN DETERMINING COEFFICIE
    !     TREEIT = NUMBER OF TREES VALUE USED IN ITERATION PROCESS
    !     TEM    = TEMPORARY STORAGE VARIABLE
    !     IPATH  = PATH THROUGH LINEAR FN COEFFICIENT SECTION -
    !              1 IF ITERATIVE DETERMINATION
    !              2 IF STRAIGHT COMPUTATION
    !      TMMSB = MAXIMUM NUMBER OF TREES FOR A GIVEN DQ10 OR DR10
    !               ACCORDING TO THE MATURE STAND BOUNDARY FUNCTION
    !     T85MSB = 85 PERCENT LEVEL OF TMMSB

    ! SDIMAX IS USED HERE TO CARRY WEIGHTED SDI MAXIMUM. IF A USER-DEFINED
    ! BASAL AREA MAXIMUM HAS NOT BEEN ENTERED, THEN BASAL AREA MAXIMUM
    ! WILL BE RESET TO BE CONSISTENT WITH THE SDI MAXIMUM. THIS IS NEEDED
    ! FOR MODELING CLIMATE CHANGE.

    if (VARIANTCD.eq.'EM') call PRGAVE(D10)
    call SDICAL(0,SDIMAX)
    if (VARIANTCD.eq.'EC') then
       if (DEBUG)write(JOSTND,*)'SDIMAX,BAMAX= ',&
            &SDIMAX,BAMAX
    end if
    CONST = SDIMAX / 0.02483133
    if (DEBUG)write(JOSTND,*)'SDIMAX,CONST,BAMAX= ',&
         &SDIMAX,CONST,BAMAX

    ! IF SDIMAX IS LESS THAN 5, ASSUME CLIMATE HAS CHANGED ENOUGH THAT THE
    ! SITE WILL NO LONGER SUPPORT TREES, AND KILL ALL EXISTING TREES.

    if (SDIMAX .lt.5) then
       TN10 = 0.
       go to 271
    end if

    IPATH = 0
    if (T .gt.35000.)T = 35000.

    ! GIVEN DQ0, DETERMINE MAXIMUM TREES, 85 PERCENT LEVEL AND 55 PERCENT LE

    TMD0 = CONST * (DIA0 ** (-1.605))
    if (TMD0.gt.35000.0) TMD0 = 35000.0
    T85D0 = TMD0 * PMSDIU
    T55D0 = PMSDIL * TMD0
    if (DEBUG)write(JOSTND,*)'TMD0,PMSDIU,PMSDIL,T85D0,T55D0= ',&
         &TMD0,PMSDIU,PMSDIL,T85D0,T55D0

    ! GIVEN DQ10 OR DR10 DETERMINE MAXIMUM TREES, 85 PERCENT LEVEL,
    ! AND 55 PERCENT L

    TMD10 = CONST * (D10**(-1.605))
    if (TMD10.gt.35000.0) TMD10 = 35000.0
    T85D10 = TMD10 * PMSDIU
    T55D10 = PMSDIL * TMD10
    if (DEBUG)write(JOSTND,*)'TMD10,PMSDIU,PMSDIL,T85D10,T55D10= ',&
         &TMD10,PMSDIU,PMSDIL,T85D10,T55D10
    if (DEBUG) then
       if (VARIANTCD.eq.'BM') then
          write(JOSTND,9041)ICYC,SDIMAX,BAMAX
       else
          write(JOSTND,9040)ICYC,SDIMAX
       end if
    end if
    9040 format('IN MORTS ICYC,MAX SDI =',I5,F10.1)
    9041 format('IN MORTS ICYC,MAX SDI, BAMAX  =',I5,2F10.1)

    ! IF MSB IS IN EFFECT, THEN COMPUTE NECESSARY MSB PARAMETERS

    if (SLPMSB .ne.0.) then
       CEPMSB = ALOG(CONST*(QMDMSB**(-1.605)))-SLPMSB*ALOG(QMDMSB)
    end if
    if (DEBUG)write(JOSTND,*)'MATURE STAND BOUNDARY SETTINGS, QMDMSB,'&
         &,'CEPMSB,SLPMSB,D10= ',QMDMSB,CEPMSB,SLPMSB,D10

    ! IF TOTAL NUMBER OF TREES IS BETWEEN 85 PERCENT AND COMPUTED MAXIMUM,
    ! KILL BACK TO 85 PERCENT LEVEL.

    if (T .le. T85D0) go to 210
    TN10 = T85D10
    go to 270
    210 continue

    ! IF TOTAL NUMBER OF TREES IS BETWEEN 55 PERCENT AND 85 PERCENT, FIND
    ! LINEAR FN COEFFICIENTS (ITERATIVE METHOD) AND KILL ACCORDING TO
    ! LINEAR FN.

    if (T .le. T55D0) go to 240

    ! SPECIAL CASE WHERE T IS CLOSE TO THE 85% LINE AT DIA0

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
    if (DEBUG)write(JOSTND,*)'MORTS 220,TEM,CONST',TEM,CONST
    D55M = (ALOG(TEM) - ALOG(PMSDIL*CONST)) / (-1.605)
    T55M = ALOG(TEM)
    D85M = D55M * 1.25
    if (DEBUG)write(JOSTND,*)'D55M,T55M,D85M= ',D55M,T55M,D85M
    221 if (D85M .gt.5.0) D85M = 5.0
    if (D85M .lt.0.125)D85M = 0.125
    T85M = ALOG(CONST * (EXP(D85M) ** (-1.605)) * PMSDIU)
    SLP = (T85M-T55M) / (D85M - D55M)
    if (DEBUG)write(JOSTND,*)'D55M,D85M,T55M,T85M,SLP= ',&
         &D55M,D85M,T55M,T85M,SLP
    if (SLP .gt. -0.5.and. D85M .lt.5.0) then
       D85M = D85M + .1
       go to 221
    end if
    CEPT = T55M - SLP * D55M
    if (T .le. T55D0) go to 230
    if (DEBUG)write(JOSTND,*)'MORTS,359,DIA0',DIA0
    TPRIME = CEPT + SLP * ALOG(DIA0)
    DIFF = T - EXP(TPRIME)
    if (DEBUG)write(JOSTND,9050) DIA0,D10,T,TREEIT,TEM,D55M,&
         &T55M,D85M,T85M,SLP,CEPT,TPRIME,DIFF,KNT
    9050 format('MORTS 9050',13F9.3,I4)
    if (DIFF .le.5.0.and. DIFF .ge. -5.0) go to 230
    TREEIT = TREEIT + 0.5 * DIFF
    KNT = KNT + 1
    if (KNT .le.100) go to 220
    230 continue
    if (SLPMRT .eq.0.) SLPMRT = SLP
    if (CEPMRT .eq.0.) CEPMRT = CEPT
    if (DEBUG)write(JOSTND,*)'D10,CEPMRT,SLPMRT= ',D10,CEPMRT,SLPMRT
    TEM = ALOG(D10)
    TN10 = CEPMRT + SLPMRT*TEM
    TN10 = EXP(TN10)
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
    9060 format('MORTS 9060',6(F10.3,3X),I4)

    ! BOUND TN10 (THE NUMBER OF TREES REMAINING IN THE STAND AFTER MORTALITY)
    !   IF TN10 IS GREATER THAN T, SET TN10 = T
    !   IF TN10 IS SMALL, JUST KILL ALL THE TREES

    271 continue
    if (TN10.gt. T)TN10 = T
    if (TN10.lt.0.1)TN10 = 0.

    RN = 1.0-(1.0-((T-TN10)/T))**(1./FINT)
    if (DEBUG) write(JOSTND,*) 'TESTMORTS, RN=',RN,&
         &'T=',T,'TN10=',TN10

    !  OC: SEE IF ORGANON MORTALITY HAS BEEN ESTIMATED. IF YES, USE ORGANON
    !  RATES FOR ALL TREE RECORDS. IF NO, THEN ESTIMATE FVS MORTALITY.

    if (VARIANTCD.eq.'OC') then
       SMORMT = 0.0
       do I = 1,ITRN
          SMORMT = SMORMT + MORTEXP(I)
       end do
       if (DEBUG)write(JOSTND,*)' IN MORTS SMORMT= ',SMORMT
    end if

    !  START LOOP TO ESTIMATE SDI BASED MORTALITY RATE.
    !  TREES ARE PROCESSED ONE AT A TIME WITHIN A SPECIES.

    do ISPC = 1,CFG_MAXSP
       I1 = ISCT(ISPC,1)
       if (I1.le.0) go to 50
       I2 = ISCT(ISPC,2)
       XMORT = XMMULT(ISPC)
       D1 = XMDIA1(ISPC)
       D2 = XMDIA2(ISPC)
       call MORCOF

       !  START TREE LOOP WITHIN SPECIES.

       do I3 = I1,I2

          !  INITIALIZE FOR NEXT TREE.

          I = IND1(I3)
          P = PROB(I)
          WKI = 0.0
          WK2(I) = 0.0
          D = DBH(I)
          if (VARIANTCD.eq.'EM') BARK = BRATIO(ISPC,D,HT(I))
          if (P.le.0.0) go to 40

          !  OC: IF ORGANON MORTALITY HAS BEEN ESTIMATED, USE IT

          if (VARIANTCD.eq.'OC') then
             if (LORGANON .and. (SMORMT .gt.0.)) then
                WK2(I) = MORTEXP(I)*(FINT/5.)
                WKI = MORTEXP(I)*(FINT/5.)
                if (WKI.gt.P) WKI = P
                if (DEBUG) write(JOSTND,*) ' IN MORTS I,ISPC,D,WK2,IORG= ',&
                     &I,ISPC,D,WK2(I),IORG(I)
                go to 39
             end if
          end if

          !  EM (ALL BUT SPECIES 1-3, 7-10 AND 18) AND TT (SPECIES 10) USE THE
          !  PROGNOSIS EQUATION.  EM CLAMPS ITS LOGIT AT 70 AND SCALES THE RATE
          !  BY SPECIES; TT CLAMPS AT 88.5, KILLS ALL TREES WHEN SDIMAX < 5 AND
          !  PRINTS ITS OWN DIAGNOSTIC.

          LPRG = .false.
          if (VARIANTCD.eq.'EM') LPRG = .not.(ISPC.le.3.or.&
               &(ISPC.ge.7.and. ISPC.le.10) .or. ISPC.eq.18)
          if (VARIANTCD.eq.'TT') LPRG = ISPC.eq.10
          if (LPRG) then
             if (VARIANTCD.eq.'EM') then
                call PRGTRE(B0,70.0)
                XMLT = 0.6
                if (ISPC.eq.5) XMLT = 1.0
                if (ISPC.eq.6) XMLT = 0.2
                WKI = P*(1.0-(1.0-RIPP)**FINT)*X*XMLT
                if (WKI.gt.P) WKI = P
                WK2(I) = WKI
                go to 39
             end if
             BARK = BRATIO(ISPC,D,HT(I))
             call PRGTRE(B0,88.5)
             WKI = P*(1.0-(1.0-RIPP)**FINT)*X
             if (WKI.gt.P) WKI = P
             if (SDIMAX .lt.5) then
                WKI = P
             end if
             WK2(I) = WKI
             if (.not.DEBUG) go to 40
             PRES = P-WKI
             VLOS = WKI*CFV(I)/FINT
             write(JOSTND,9002) I,ISPC,D,G,RELDBH,BA,P,WKI,PRES,VLOS
             9002 format('IN MORTS, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,&
                  &',  DBH INCREMENT=',F7.4,',  RELDBH=',F7.4,',  BA=',F7.2,&
                  &',  INIT PROB=',F7.3/&
                  &', TREES DYING=',F7.3,'RES PROB=',F7.3,&
                  &',  VOL LOST=',F9.3)
             go to 40
          end if

          !  COMPUTE BACKGROUND MORTALITY RATE RI

          if (VARIANTCD.eq.'EM') then
             RI = (1.0/(1.0+EXP(B0+B1*D+B2*D*D)))
          else
             RI = (1.0/(1.0+EXP(B0+B1*D)))
          end if

          ! CONSTRAIN RW AND GS MORTALITY IF NEEDED

          if (((VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'OC').and.&
               &(ISPC.eq.23.or. ISPC.eq.50)) .or.&
               &(VARIANTCD.eq.'WS'.and.(ISPC.eq.4.or. ISPC.eq.23))) then
             if (RI .lt.0.0001) RI = 0.0001
             if (DEBUG)write(JOSTND,*)' IN MORTS - RW/GS DEBUG', ' ISPC=',&
                  &ISPC,' D=',D, ' RI=',RI
          end if
          if (VARIANTCD.eq.'NC'.and. ISPC.eq.12) then
             if (RI .lt.0.0001) RI = 0.0001
             if (DEBUG)write(JOSTND,*)' IN MORTS - RW DEBUG', ' ISPC=',&
                  &ISPC,' D=',D, ' RI=',RI
          end if

          ! TEST RUNS SHOW BACKGROUND MORTALITY RATE IS HIGH, CUT IT IN HALF
          ! (EXCEPT IN SN).

          if (VARIANTCD.ne.'SN') RI = 0.5 * RI

          !  MERGE ESTIMATES OF RI AND RN.

          RIP = RI

          ! IF SDI NOT IN EFFECT YET SET RIP TO BACKGROUND MORTALITY RATE.
          ! OTHERWISE SET TO SDI MORTALITY RATE.

          RIP = RN
          TEM = CONST*(D10**(-1.605))
          if (TEM .gt.35000.0)TEM = 35000.0
          TEM = TEM*PMSDIL
          if (T .le. TEM .or. RN .le.0.0)RIP = RI
          if (RIP.gt.1.0) RIP = 1.0
          X = 1.0
          if (D .ge. D1.and. D .lt. D2)X = XMORT

          ! APPLY MORTALITY MULTIPLIER ONLY TO BACKGROUND RATE

          if (RIP .eq. RN) X = 1.0
          WKI = P*(1.0-(1.0-RIP)**FINT)*X
          if (WKI.gt.P) WKI = P
          if (DEBUG) write(JOSTND,9070) I,D,RI,RN,RIP
          9070 format('MORTALITY RATE ESTIMATES FOR TREE ',I4,', DBH = ',F6.2/&
               &' RI = ',F7.5,' RN = ',F7.5,' RIP = ',F7.5)
          WK2(I) = WKI

          !  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.

          39 continue
          if (DEBUG) then
             PRES = P-WKI
             VLOS = WKI*CFV(I)/FINT
             if (VARIANTCD.eq.'EM') then
                write(JOSTND,9081) I,ISPC,D,G,RELDBH,BA,P,WKI,PRES,VLOS
             else
                write(JOSTND,9080) I,ISPC,D,P,WKI,PRES,VLOS
             end if
             9080 format('IN MORTS, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,&
                  &',  INIT PROB=',F9.3,&
                  &',  TREES DYING=',F9.3,'  RES PROB=',F9.3,&
                  &',  VOL LOST=',F9.3)
             9081 format('IN MORTS, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,&
                  &',  DBH INCREMENT=',F7.4,',  RELDBH=',F7.4,',  BA=',F7.2,&
                  &',  INIT PROB=',F9.3/&
                  &', TREES DYING=',F9.3,'  RES PROB=',F9.3,&
                  &',  VOL LOST=',F9.3)
          end if
          40 continue
       end do

       !  END OF SPECIES LOOP.  PRINT DEBUG INFO IF DESIRED.

       if (DEBUG .and. .not.LVLS) then
          write(JOSTND,9090)  ISPC,B0,B1
          9090 format('IN MORTS,  ISPC=',I3,&
               &11X,'B0=',F8.6,',  B1=',F8.6)
       end if
       50 continue
    end do

    ! DISTRIBUTE MORTALITY BY VARIANT SPECIFIC METHOD
    ! IF ALL TREES ARE BEING KILLED, WE DONT NEED TO DO THIS

    !  OC: IF USING ORGANON MORTALITY, SKIP THIS SECTION.  EM HAS NO VARIANT
    !  SPECIFIC DISTRIBUTION.

    if (VARIANTCD.eq.'OC') then
       if (LORGANON .and. (SMORMT .gt.0.))go to 353
    end if
    if (VARIANTCD.ne.'EM') then
       SUMTRE = 0.0
       if (RIP .eq. RN)SUMTRE = T-TN10
       if (SUMTRE .lt.0.0)SUMTRE = 0.0
       if (TN10.ge.0.1) then
          if (VARIANTCD.eq.'BM') call BMTMRT(SUMTRE,DEBUG,SUMKIL)
          if (VARIANTCD.eq.'NC') call NWCMRT(SUMTRE,DEBUG,SUMKIL)
          if (VARIANTCD.eq.'SO') call SCOMRT(SUMTRE,DEBUG,SUMKIL)
          if (VARIANTCD.eq.'TT') call TTMRT(SUMTRE,DEBUG,SUMKIL)
          if (VARIANTCD.eq.'UT') call UTMRT(SUMTRE,DEBUG,SUMKIL)
          if (VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'CR'.or.VARIANTCD.eq.'CS'.or.&
               &VARIANTCD.eq.'EC'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'NE'.or.&
               &VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'SN'.or.VARIANTCD.eq.'WS')&
               &call VARMRT(SUMTRE,DEBUG,SUMKIL)
       end if
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
       if (LDBHSD) then
          if (D.lt.DBHSDI)go to 30
       else
          if (LZEIDE.and.(D.lt.DBHZEIDE))go to 30 ! BRANCH IF D IS LT MIN DBH
          if (.not.LZEIDE.and.(D.lt.DBHSTAGE))go to 30
       end if
       BARK = BRATIO(IS,D,HT(I))
       G = (DG(I)/BARK) * (FINT/DGPER)
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
    if (DEBUG)write(JOSTND,*)'MORTS CHECK DIA. IPASS,DQ10,DR10',&
         &'DQ10N,DR10N= ',IPASS,DQ10,DR10,DQ10N,DR10N
    if (LZEIDE) then
       D10N = DR10N
    else
       D10N = DQ10N
    end if
    if (IPASS .eq.10)go to 35
    DIFF = ABS(D10-D10N)
    if (DIFF .gt.0.1) then

       ! SOMETIMES SELECTIVE KILLING CAN DECREASE QMD.  IF SO, TAKE
       ! CALCULATED MORTALITY AND RECALIBRATE NEXT CYCLE.

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

    TMMSB = 0.
    T85MSB = 0.
    if (D10.gt.QMDMSB .and. TN.gt.0.) then
       TMMSB = EXP(CEPMSB+SLPMSB*ALOG(D10))
       T85MSB = TMMSB*PMSDIU
       TMORE = TN-T85MSB
       if (TMORE .lt.0.)TMORE = 0.
       if (DEBUG)write(JOSTND,*)'ALTERNATE MORTALITY LOGIC, D10,TN,',&
            &'QMDMSB,CEPMSB,SLPMSB,TMMSB,T85MSB,PMSDIU,TMORE= '
       if (DEBUG)write(JOSTND,*)&
            &D10,TN,QMDMSB,CEPMSB,SLPMSB,TMMSB,T85MSB,PMSDIU,TMORE

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
          DBHEND = DBH(I)+(DG(I)/BARK)*(FINT/DGPER)
          if (DBHEND.ge.DLOMSB .and. DBHEND.lt.DHIMSB) then
             TPACLS = TPACLS+PROB(I)-WK2(I)
          end if
       end do
       if (DEBUG)write(JOSTND,*)'ALT MORT LOGIC DLOMSB,DHIMSB,TPACLS',&
            &' = ',DLOMSB,DHIMSB,TPACLS

       if (TMORE .gt. TPACLS) then
          write(JOSTND,351)TPACLS,TMORE
          351 format(/,2('***************'/),'WARNING: FOR ALTERNATE ',&
               &'MORTALITY, TPA IN DBH CLASS OF ',F8.1,&
               &' TREES/ACRE IS LESS THAN THE ADDITIONAL MORTALITY TPA',&
               &/,9X,' OF ',F8.1,' TREES/ACRE.  ',&
               &'ALTERNATE MORTALITY CANCELLED.',/2('***************'/),/)
          go to 353
       end if

       TEMEFF = EFFMSB
       if (MFLMSB .eq.3) then
          TEMEFF = TMORE/TPACLS
       else
          if (TPACLS*TEMEFF .lt. TMORE) then
             TEMEFF = TMORE/TPACLS
             write(JOSTND,352)EFFMSB,TEMEFF
             352 format(/,2('***************'/),'WARNING: FOR ALTERNATE ',&
                  &'MORTALITY, MORTALITY EFFICIENCY OF ',F8.4,&
                  &' IS TOO LOW TO REACH THE ADDITIONAL MORTALITY LEVEL. ',&
                  &/,9X,'MORTALITY EFFICIENCY RESET TO ',F8.4,&
                  &' FOR FURTHER PROCESSING.',&
                  &/2('***************'/),/)
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
       G = (DG(I)/BARK) * (FINT/DGPER)
       IDMFLG = IFIX(SIZCAP(IS,3))
       if ((D+G).ge.SIZCAP(IS,1) .and. IDMFLG.ne.1) then
          WK2(I) = AMAX1(WK2(I),(P*SIZCAP(IS,2)*FINT/DGPER))
          if (WK2(I) .gt. P)WK2(I) = P
          if (DEBUG)write(JOSTND,*)'SIZE CAP RESTRICTION IMPOSED, ',&
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
       G = (DG(I)/BARK) * (FINT/DGPER)
       if (VARIANTCD.eq.'SN') then
          if (DEBUG)write(JOSTND,*)'I,DG,BARK,FINT,G= ',I,DG(I),BARK,FINT,G
       end if
       if (VARIANTCD.eq.'TT') then
          if (DEBUG)write(JOSTND,*)'I,D,DG,BARK,PROB,WK2,P= ',&
               &I,D,DG(I),BARK,PROB(I),WK2(I),P
       end if
       TNEW = TNEW+P
       BANEW = BANEW+(0.0054542*(D+G)**2)*P
       BADEAD = BADEAD+(0.0054542*(D+G)**2)*WK2(I)
       QMDNEW = QMDNEW + ((D+G)**2)*P
       if (DEBUG)write(JOSTND,*)'I,P,D,G,TNEW,BANEW,QMDNEW,BADEAD= ',&
            &I,P,D,G,TNEW,BANEW,QMDNEW,BADEAD
       36 continue
    end do
    if (TNEW .gt.0.) then
       QMDNEW = SQRT(QMDNEW/TNEW)
    else
       QMDNEW = 0.
    end if
    if (DEBUG)write(JOSTND,*)'ICYC,BANEW,BAMAX,TNEW,QMDNEW,KNT2= ',&
         &ICYC,BANEW,BAMAX,TNEW,QMDNEW,KNT2
    !  OC: IF USING ORGANON MORTALITY, SKIP THIS SECTION

    if (VARIANTCD.eq.'OC') then
       if (SMORMT .gt.0.)go to 45
    end if
    if ((BANEW-BAMAX) .gt.1.) then

       !       CALCULATE ADJUSTMENT FACTOR NEEDED TO GET RESIDUAL BA WITHIN
       !       THE BA MAXIMUM LIMIT. INCREASE IT BY 10% AND PLACE A LOWER
       !       BOUND ON THE FACTOR TO SPEED UP ITERATION.

       ADJFAC = ((BANEW-BAMAX)/BADEAD)
       if (DEBUG)write(JOSTND,*)'BANEW,BAMAX,ADJFAC= ',&
            &BANEW,BAMAX,ADJFAC

       !       LOOP THROUGH THE TREE LIST AND ADJUST THE MORTALITY VALUES.

       TNEW = 0.
       do I = 1,ITRN
          P = PROB(I)
          WKI = WK2(I)*(1+ADJFAC)
          if (WKI.gt.P) WKI = P
          WK2(I) = WKI
          if (DEBUG)write(JOSTND,*)'ADJUSTING FOR BAMAX I,P,WKI= ',&
               &I,P,WKI

          !       PRINT DEBUG INFO IF DESIRED.

          TNEW = TNEW+P-WKI
          if (.not.DEBUG) go to 1500
          PRES = P-WKI
          VLOS = WKI*CFV(I)/FINT
          if (VARIANTCD.eq.'EM') then
             write(JOSTND,9081) I,ISPC,D,P,WKI,PRES,VLOS
          else
             write(JOSTND,9080) I,ISPC,D,P,WKI,PRES,VLOS
          end if
          1500 continue
       end do

       ! LOOP BACK AND SEE IF THE BAMAX TARGET HAS BEEN ACHIEVED YET.
       ! (I.E. IF THE COMPUTED MORTALITY RATE EXCEEDED THE PROB, AND WE
       !  HAD TO LIMIT MORTALITY TO THE PROB VALUE FOR SOME TREE RECORDS,
       !  THEN WE MAY NOT REACH THE BA LIMIT IN ONE PASS.)

       KNT2 = KNT2+1
       if (KNT2.lt.100)go to 9001

       IPATH = 0
       if (DEBUG)write(JOSTND,*)'AFTER BA ADJUSTMENT RESIDUAL TPA = ',&
            &TNEW
    end if
    TPAMRT = TNEW
    45 continue

    !  COMPUTE THE CLIMATE-PREDICTED MORTALITY RATES BY SPECIES

    if (VARIANTCD.ne.'SN'.and.VARIANTCD.ne.'CS'.and.VARIANTCD.ne.'LS'.and.&
         &VARIANTCD.ne.'NE') call CLMORTS

  end subroutine MORSDI

  subroutine MORPRG(LCON)
    implicit none

    !  PROGNOSIS MORTALITY (CI, IE, KT).  MORTALITY RATE IS PREDICTED FROM
    !  DBH, DBH INCREMENT, RELATIVE DIAMETER, AND STAND BASAL AREA. AN
    !  ADJUSTMENT IS MADE TO INCREASE MORTALITY RATE AS THE STAND BASAL AREA
    !  APPROACHES THE THEORETICAL MAXIMUM THAT THE HABITAT TYPE CAN SUPPORT.
    !  THE TREE EQUATION IS IN PRGTRE.  IE AND CI SCALE THE RATE BY SPECIES
    !  (XMLT); CI CLAMPS THE LOGIT AT 88.5 FOR ITS NATIVE SPECIES AND 70 FOR
    !  THE UT SURROGATES (11-17, 19).  CI'S PMD TABLE (ALL ZERO) WAS NEVER
    !  READ AND IS NOT KEPT.

    logical :: LCON
    integer :: I1,I2,I3,IS,IDMFLG
    real    :: P,SD2SQ,CIOBDS,DQ10,WKI,PRES,VLOS,CLAMP,XMLT

    if (LCON) then
       call PRGCON
       return
    end if

    call DBCHK (DEBUG,'MORTS',5,ICYC)
    if (VARIANTCD.eq.'CI') then
       if (DEBUG)write(JOSTND,9002)ICYC
       9002 format('ENTERING SUBROUTINE MORTS  CYCLE =',I4)
    end if

    !  IF THERE ARE NO TREE RECORDS, SIMPLY PROCESS THE MORTALITY
    !  MULTIPLIERS AND BRANCH TO END (FOR THE METHOD USED TO GET
    !  MULTIPLIERS, SEE SUBROUTINE MULTS).

    call MMULT(.true.)

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

    !  ESTIMATE QUADRATIC MEAN DIAMETER 10 YEARS HENCE.

    T = 0.0
    SD2SQ = 0.0
    do I = 1,ITRN
       P = PROB(I)
       IS = ISP(I)
       D = DBH(I)
       BARK = BRATIO(IS,D,HT(I))
       G = DG(I)/BARK
       CIOBDS = (2.0*D*G+G*G)
       SD2SQ = SD2SQ+P*(D*D+CIOBDS)
       T = T+P
       20 continue
    end do
    DQ10 = SQRT(SD2SQ/T)
    DELTBA = .005454154*DQ10*DQ10*T-BA

    !  PROJECT BASAL AREA FORWARD 10 YEARS ASSUMING THAT BA/BAMAX
    !  PROPORTION OF THE PREDICTED BASAL AREA INCREMENT WILL BE LOST TO
    !  MORTALITY.  AT BA=BAMAX, MORTALITY WILL ABSORB ALL INCREMENT.

    call PRGAVE(DQ10)

    !  START LOOP TO ESTIMATE MORTALITY RATE.  TREES ARE PROCESSED
    !  ONE AT A TIME WITHIN A SPECIES.

    do ISPC = 1,CFG_MAXSP
       I1 = ISCT(ISPC,1)
       if (I1.le.0) go to 50
       I2 = ISCT(ISPC,2)
       XMORT = XMMULT(ISPC)
       D1 = XMDIA1(ISPC)
       D2 = XMDIA2(ISPC)
       call MORCOF

       !  START TREE LOOP WITHIN SPECIES.

       do I3 = I1,I2

          !  INITIALIZE FOR NEXT TREE.

          I = IND1(I3)
          P = PROB(I)
          WKI = 0.0
          WK2(I) = 0.0
          D = DBH(I)
          BARK = BRATIO(ISPC,D,HT(I))
          if (P.le.0.0) go to 40
          CLAMP = 70.0
          if (VARIANTCD.eq.'CI') then
             select case (ISPC)
             case (11:17,19)
             case default
                CLAMP = 88.5
             end select
          end if
          call PRGTRE(B5,CLAMP)

          !  APPLY 60 PERCENT OF NI RATE FOR LM,PY,AS,CO,MM,PB,OH.
          !  APPLY 20 PERCENT OF NI RATE FOR PI,JU.

          XMLT = 1.0
          if (VARIANTCD.eq.'IE') then
             if (ISPC.le.12.or. ISPC.eq.14.or. ISPC.eq.23) then
                XMLT = 1.0
             else if (ISPC.eq.15.or. ISPC.eq.16) then
                XMLT = 0.2
             else
                XMLT = 0.6
             end if
          else if (VARIANTCD.eq.'CI') then
             select case (ISPC)
             case (11:13,15:17,19)
                XMLT = 0.6
             case (14)
                XMLT = 0.2
             end select
          end if
          WKI = P*(1.0-(1.0-RIPP)**FINT)*X*XMLT

          BARK = BRATIO(ISPC,D,HT(I))
          G = (DG(I)/BARK) * (FINT/10.0)
          IDMFLG = IFIX(SIZCAP(ISPC,3))
          if ((D+G).ge.SIZCAP(ISPC,1) .and. IDMFLG.ne.1) then
             WKI = AMAX1(WKI,(P*SIZCAP(ISPC,2)*FINT/10.0))
             if (DEBUG)write(JOSTND,*)'SIZE CAP RESTRICTION IMPOSED, ',&
                  &'I,ISPC,D,P,SIZCAP 1-3,WKI = ',&
                  &I,ISPC,D,P,SIZCAP(ISPC,1),SIZCAP(ISPC,2),SIZCAP(ISPC,3),WKI
          end if
          if (WKI.gt.P) WKI = P

          ! IF SDIMAX IS LESS THAN 5, ASSUME CLIMATE HAS CHANGED ENOUGH THAT THE
          ! SITE WILL NO LONGER SUPPORT TREES, AND KILL ALL EXISTING TREES.

          if (SDIMAX .lt.5) then
             WKI = P
          end if
          WK2(I) = WKI

          !  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.

          if (.not.DEBUG) go to 40
          PRES = P-WKI
          VLOS = WKI*CFV(I)/FINT
          if (VARIANTCD.eq.'CI') then
             write(JOSTND,9003) I,ISPC,D,G,RELDBH,BA,P,WKI,PRES,VLOS
          else
             write(JOSTND,9000) I,ISPC,D,G,RELDBH,BA,P,WKI,PRES,VLOS
          end if
          9000 format('IN MORTS, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,&
               &',  DBH INCREMENT=',F7.4,',  RELDBH=',F7.4,',  BA=',F7.2,&
               &',  INIT PROB=',F9.3/&
               &', TREES DYING=',F9.3,'  RES PROB=',F9.3,&
               &',  VOL LOST=',F9.3)
          9003 format('IN MORTS, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,&
               &',  DBH INCREMENT=',F7.4,',  RELDBH=',F7.4,',  BA=',F7.2,&
               &',  INIT PROB=',F7.3/&
               &',  TREES DYING=',F7.3,'RES PROB=',F7.3,&
               &',  VOL LOST=',F9.3)
          40 continue
       end do

       !  END OF SPECIES LOOP.  PRINT DEBUG INFO IF DESIRED.

       if (.not.DEBUG) go to 50
       write(JOSTND,9001)  ISPC,B5
       9001 format('IN MORTS,  ISPC=',I3,&
            &11X,'B5=',F8.6)
       50 continue
    end do

    100 continue

    !  COMPUTE THE CLIMATE-PREDICTED MORTALITY RATES BY SPECIES

    call CLMORTS

  end subroutine MORPRG

  subroutine MORPAC(LCON)
    implicit none

    !  OP, PN AND WC MORTALITY: ORGANON NWO/SWO EQUATIONS, SMALL-TREE
    !  EQUATIONS (GOULD AND HARRINGTON), THEN AN SDI/BA ADJUSTMENT PASS.
    !  OP ALSO USES ORGANON MORTALITY WHEN AVAILABLE, SWITCHES DF TO THE SMC
    !  EQUATION FOR MODEL TYPE 3 AND SCALES DG BY FINT/5; PN AND WC SCALE BY
    !  FINT/10, BOUND RIP AWAY FROM 1 AND WC ADJUSTS ITS SITE INDEX.  THE
    !  FORMER PMSC AND PSP TABLES WERE NEVER READ AND ARE NOT KEPT.

    logical :: LCON
    integer :: MORTMAP(39),PASS,MCLASS(39),ACLASS,I1,I2,I3,IS,IDMFLG,IMRTMP
    real    :: BM0(39),BM1(39),BM2(39),BM3(39),BM4(39),BM5(39)
    real    :: VLOS,PRES,WKI,DQ10,CIOBDS,P,SD2SQ,CRADJ,SUMDR10
    real    :: RELHT,XSITE1,XSITE2,BAL,CR,WKIT2,RELDBH1(MAXTRE),G1(MAXTRE)
    real    :: BAA,TA,SD2SQA,SUMDR10A,DQ10A,SDIA,CIOBDS1(MAXTRE)
    real    :: WKII(MAXTRE),MVALUES(5),AVALUE,ALPHA(3),BETA(39)
    real    :: DBHA,HBH,PTBAL,SMORMT,RBM0,RBM1,RBM2,RBM3,RBM4,RBM5,DGPER

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

    if (LCON) return
    DGPER = 10.0
    if (VARIANTCD.eq.'OP') DGPER = 5.0

    WKIT2 = 0.0

    !  CHECK FOR DEBUG.

    call DBCHK (DEBUG,'MORTS',5,ICYC)

    !  IF THERE ARE NO TREE RECORDS, SIMPLY PROCESS THE MORTALITY
    !  MULTIPLIERS AND BRANCH TO END (FOR THE METHOD USED TO GET
    !  MULTIPLIERS, SEE SUBROUTINE MULTS).

    call MMULT(.true.)

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

    !  OP: SEE IF ORGANON MORTALITY HAS BEEN ESTIMATED. IF YES, USE ORGANON
    !  RATES FOR ALL TREE RECORDS. IF NO, THEN ESTIMATE FVS MORTALITY.

    if (VARIANTCD.eq.'OP') then
       SMORMT = 0.0
       do I = 1,ITRN
          SMORMT = SMORMT + MORTEXP(I)
       end do
       if (DEBUG)write(JOSTND,*)' IN MORTS SMORMT= ',SMORMT
    end if

    !  START LOOP TO ESTIMATE MORTALITY RATE.  TREES ARE PROCESSED
    !  ONE AT A TIME WITHIN A SPECIES.

    do ISPC = 1,CFG_MAXSP
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

          !  OP: IF ORGANON MORTALITY HAS BEEN ESTIMATED, USE IT

          if (VARIANTCD.eq.'OP') then
             if (LORGANON .and. (SMORMT .gt.0.)) then
                WK2(I) = MORTEXP(I)*(FINT/5.)
                WKI = MORTEXP(I)*(FINT/5.)
                if (DEBUG) write(JOSTND,*) ' IN MORTS I,ISPC,D,WK2,IORG= ',&
                     &I,ISPC,D,WK2(I),IORG(I)
                go to 39
             end if
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
          if (CR .le.0.17) CRADJ = 1.0-EXP(-(25.0*CR)**2)
          XSITE2 = SITEAR(19)
          XSITE1 = SITEAR(16)
          if (VARIANTCD.eq.'WC') XSITE1 = 5.21486+0.66486*SITEAR(16)

          ! OP: IF SMC MODEL TYPE USE MORTMAP(26)=4 INSTEAD OF 1

          IMRTMP = MORTMAP(ISPC)
          RBM0 = BM0(ISPC)
          RBM1 = BM1(ISPC)
          RBM2 = BM2(ISPC)
          RBM3 = BM3(ISPC)
          RBM4 = BM4(ISPC)
          RBM5 = BM5(ISPC)
          if (VARIANTCD.eq.'OP'.and. IMODTY .eq.3.and. ISPC .eq.16) then
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

          if (VARIANTCD.eq.'OP') then
             IF(DEBUG)WRITE(JOSTND,*)'IN MORTS EQNS IMRTMP,I,ISPC,RIP,D,CR,    &
                  &XSITE1,XSITE2,BAL,CRDJ=',IMRTMP,I,ISPC,RIP,D,CR,XSITE1,&
                  &XSITE2,BAL,CRADJ
          else
             IF(DEBUG)WRITE(JOSTND,*)'IN MORTS EQNS MORTMAP,I,ISPC,RIP,D,CR,   &
                  &XSITE1,XSITE2,BAL,CRDJ=',MORTMAP(ISPC),I,ISPC,RIP,D,CR,XSITE1,&
                  &XSITE2,BAL,CRADJ
          end if

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
          if (VARIANTCD.ne.'OP') then
             if ( (1.0-RIP) .lt.0.001 ) RIP = 0.001
          end if
          WKI = P*(1.0-(1.0-RIP)**FINT)*X
          if (DEBUG)write(JOSTND,*)'IN MORTS EQ I,XCHECK,X,P,RIP,FINT,WKI= ',&
               &I,XCHECK,X,P,RIP,FINT,WKI

          BARK = BRATIO(IS,D,HT(I))
          G = (DG(I)/BARK) * (FINT/DGPER)
          IDMFLG = IFIX(SIZCAP(ISPC,3))
          if ((D+G).ge.SIZCAP(ISPC,1) .and. IDMFLG.ne.1) then
             WKI = AMAX1(WKI,(P*SIZCAP(ISPC,2)*FINT/DGPER))
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

  end subroutine MORPAC

  subroutine MORBC(LCON)
    implicit none

    !  BC: MOVED UNCHANGED EXCEPT THAT FIXMORT IS THE SHARED FIXMRT.

    logical :: LCON

    !  THIS SUBROUTINE COMPUTES PERIODIC MORTALITY RATES FOR EACH TREE
    !  RECORDS AND THEN REDUCES THE NUMBER OF TREES/ACRE REPRESENTED BY THE
    !  TREE RECORD.  MORTALITY RATE IS PREDICTED FROM DBH, DBH INCREMENT,
    !  RELATIVE DIAMETER, AND STAND BASAL AREA. AN ADJUSTMENT IS MADE TO
    !  INCREASE MORTALITY RATE AS THE STAND BASAL AREA APPROACHES THE
    !  THEORETICAL MAXIMUM THAT THE HABITAT TYPE CAN SUPPORT.  THIS ROUTINE
    !  IS CALLED FROM **TREGRO** WHEN CYCLING FOR GROWTH PREDICTION.  ENTRY
    !  **MORCON** IS ACCESSED TO LOAD SITE DEPENDENT CONSTANTS.

    ! Common blocks

    !  DEFINITIONS:

    !        I -- TREE SUBSCRIPT.
    !        D -- TREE DIAMETER.
    !      RIP -- ESTIMATED ANNUAL MORTALITY RATE BASED ON HAMILTON'S
    !             MODEL.
    !       RZ -- RATE SUCH THAT BASAL AREA IN 10 YEARS IS EQUAL TO BA
    !             PLUS (BAMAX-BA)/BAMAX PROPORTION OF THE PREDICTED BA
    !             INCREMENT.
    !     RIPP -- WEIGHTED AVERAGE MORTALITY RATE BASED ON BA, BAMAX,
    !             RIP, AND RZ.  THIS IS THE RATE THAT IS USED TO PREDICT
    !             TREE MORTALITY.  IT IS AN ANNUAL RATE THAT WILL BE
    !             COMPOUNDED TO OBTAIN A FINT-YEAR RATE.
    !        P -- NUMBER OF TREES PER ACRE REPRESENTED BY A TREE.
    !      WKI -- SCALAR USED FOR CALCULATION AND TEMPORARY STORAGE
    !             OF TREE MORTALITY (TREES/ACRE).
    !     FMRT -- 1,2: BEC: 1= ICH, 2= IDF
    !             1,6: SPP: 1= PW, 2=LW, 3= FD, 4= PL, 5= SE, 6=PY
    !             1,5: BA:  1= <20, 2=20-30, 3=30-40, 4=40-50, 5=50+ m**2/ha
    !             1,4: DBH: 1= <10, 2=10-25, 3=25-40, 4=40+ cm DBH
    !             THESE ARE MAPPED TO TMRT (SIMILAR DIMS) IN MORCON

    !     P_MD  - NUMBER OF SETS OF SITE SERIES COEFFICIENTS
    !     P_SS  - MAXIMUM NUMBER OF SITE SERIES PER COEFFICIENT SET

    ! Variables
    integer :: P_MD,P_SS
    parameter (P_MD = 1)
    parameter (P_SS = 30)


    character(len=15) :: PAT
    integer :: NTODO,I,IDATE,IACTK,NP,ISPCC,IS,ISPC
    integer :: I1,I2,I3,IP,J,K,M
    integer :: IPOS(15),IPAS
    integer :: MRTCLS,DBHCLS,BACLS,BECCLS,IDMFLG
    real    :: T,SD2SQ,P,D,G,CIOBDS,DQ10,DELTBA,BA10,TB
    real    :: TTB,RZ,BARK,XMORT,DX,BAX
    real    :: D1,D2,WKI,RIP,RIPP
    real    :: X,XCHECK,PRES,VLOS
    real    :: BAL,SPH
    real    :: BRATIO
    real    :: FMRT(MRT_IDF,MRT_PY,MRT_BA_UB,MRT_DBH_UB)
    real    :: DBHBRK(MRT_DBH_UB-1),BABRK(MRT_BA_UB-1)


    !     V2
    real    :: PMSC(15),POT(54)
    real    :: DSUM,WPROB,AVED,B5,RELDBH,DGT,POTENT
    integer :: IPDG(30,15),IPDG2(30,15),IPT,IPT2

    !     LOCAL MORTALITY DATA STRUCTURE FOR EACH SITE SERIES.

    TYPE LMRT_STR
       integer            :: SPP(15) ! SPECIES CODE
       character (LEN = 15) :: PrettyName(P_SS) ! BEC/SITE SERIES
       real               :: COEF(8) ! COEFFICIENTS
    end TYPE

    ! Data statements

    !     V2
    data PMSC/&
         &0.0,-.17603,2*0.317888,0.607725,1.57976,-.12057,.94019,&
         &2*.21180,0.0,0.0,0.0,0.317888,0.0/

    data ((IPDG(I,J),I = 1,30),J = 1,2)/&
         &7, 5*6, 5,6,5, 4*6, 8, 5*7, 5,3,4,3,4,4,6,5,1,1,6,&
         &4*15, 14,14,12,14,13,14,14,15,15,17,15,15,16,15,15,11,10,11,9,&
         &12,12,15,12,7,10,15/
    data ((IPDG(I,J),I = 1,30),J = 3,5)/&
         &4*14, 13,13,11,14,12,14, 3*14, 17,14,14, 3*15, 11,9,10,8,11,11,&
         &15,11,5,10,15,&
         &4*12, 11,11, 3*10, 11,11,12,12, 3*14, 3*13, 9,6,8,7,9,10,12,10,&
         &4,8,12,&
         &4*10, 9,9,8,9,8,9, 3*10, 12, 5*11, 8,6,7,5,7,7,10,8,6,7,10/
    data ((IPDG(I,J),I = 1,30),J = 6,7)/&
         &6*9, 7, 3*8, 3*9, 11, 4*10, 11,7,5,6,4,7,7,9,8,3,6,9,&
         &13, 5*12, 10,12, 3*11, 12,12,15,13,13,14,13,13,9,8,9,7,10,10,&
         &12,10,4,8,12/
    data ((IPDG(I,J),I = 1,30),J = 8,9)/&
         &4*9, 8,9,7,8,7, 3*8, 9,11, 5*10, 7,5,6,5,7,7,9,8,3,6,9,&
         &4*7, 6,7,5,6,7,6,6,7,7,9,9,8,9,8,8,5,3,5,4,5,5,7,5,2,4,7/
    data ((IPDG(I,J),I = 1,30),J = 10,10)/&
         &4*15, 14,14,9,13,13,14,14,13,14,18, 5*14, 10,9,9,8,11,11,15,9,&
         &6,8,15/
    data ((IPDG(I,J),I = 1,30),J = 11,15)/& !EP,AC,AT,OC(FD),OH
         &4*15, 14,14,9,13,13,14,14,13,14,18, 5*14, 10,9,9,8,11,11,15,9,&
         &6,8,15,&
         &4*15, 14,14,12,14,13,14,14,15,15,17,15,15,16,15,15,11,10,11,9,&
         &12,12,15,12,7,10,15,&
         &4*15, 14,14,9,13,13,14,14,13,14,18, 5*14, 10,9,9,8,11,11,15,9,&
         &6,8,15,&
         &4*14, 13,13,11,14,12,14, 3*14, 17,14,14, 3*15, 11,9,10,8,11,11,&
         &15,11,5,10,15,&
         &4*15, 14,14,12,14,13,14,14,15,15,17,15,15,16,15,15,11,10,11,9,&
         &12,12,15,12,7,10,15/

    data ((IPDG2(I,J),I = 1,30),J = 1,2)/&
         &30, 3*29, 28,28,27,31,27,27,28,31,32,32,31,31,32,31,31,25,23,24,&
         &23,24,24,27,26,18,16,27,&
         &50, 3*49, 48,48,47,52,46,47,48,53,54,54,52,52,54,50,51,41,39,44,&
         &43,44,44,50,53,36,37,50/
    data ((IPDG2(I,J),I = 1,30),J = 3,5)/&
         &4*45, 44,44,43,48,43,43,44,49,50,50, 3*49, 47,47,39,36,40,39,40,&
         &39,45,41,33,34,45,&
         &4*41, 40,40,39,42,39,39,40,44, 5*45, 43,44,36,33,36,36,37,38,41,&
         &38,30,31,41,&
         &38, 3*37, 36,37,37,40,35,36,37,41,41,42,40,40,41,40,40,33,30,33,&
         &31,32,31,36,34,32,27,36/
    data ((IPDG2(I,J),I = 1,30),J = 6,7)/&
         &38, 3*37, 3*36, 38,36,35,37, 3*41, 40,40,41,40,40,33, 3*32, 34,&
         &33,38,35,26,28,38,&
         &4*41, 40,40,39,45,37,38,39,43,44,46,43,43,45,43,43,35,33,35,35,&
         &36,36,41,37,30,32,41/
    data ((IPDG2(I,J),I = 1,30),J = 8,9)/&
         &38, 6*37, 41,33,37,37,41,41,42,39,39,41,40,40,33,32,33,33,34,33,&
         &38,35,28,32,38,&
         &31,29,30,31,29,30,29,33,31,30,29,34,34,35,34,34,35,33,33,27,23,&
         &26,26,27,26,31,26,19,23,31/
    data ((IPDG2(I,J),I = 1,30),J = 10,10)/&
         &4*45, 44,44,43,48,41,42,43,47,48,50,47,47,46,44,44,36,34,37,37,&
         &38,37,43,34,30,33,43/
    data ((IPDG2(I,J),I = 1,30),J = 11,15)/& !EP,AC,AT,OC(FD),OH
         &50, 3*49, 48,48,47,52,46,47,48,53,54,54,52,52,54,50,51,41,39,44,&
         &43,44,44,50,53,36,37,50,&
         &50, 3*49, 48,48,47,52,46,47,48,53,54,54,52,52,54,50,51,41,39,44,&
         &43,44,44,50,53,36,37,50,&
         &50, 3*49, 48,48,47,52,46,47,48,53,54,54,52,52,54,50,51,41,39,44,&
         &43,44,44,50,53,36,37,50,&
         &4*45, 44,44,43,48,43,43,44,49,50,50, 3*49, 47,47,39,36,40,39,40,&
         &39,45,41,33,34,45,&
         &50, 3*49, 48,48,47,52,46,47,48,53,54,54,52,52,54,50,51,41,39,44,&
         &43,44,44,50,53,36,37,50/

    data POT/&
         &.25,.30,.35,.40,.45,.50,.55,.60,.65,.70,.75,.80,.85,.90,.95,1.00,&
         &1.05,1.10,1.15,1.20,1.25,1.30,1.35,1.40,1.45,1.50,1.55,1.60,1.65,&
         &1.70,1.75,1.80,1.85,1.90,1.95,2.00,2.05,2.10,2.15,2.20,2.25,2.30,&
         &2.35,2.40,2.45,2.50,2.55,2.60,2.65,2.70,2.75,2.80,2.85,2.90/

    !     V3
    !     THESE ARE THE COEFFICIENTS OF THE TABULAR MORTALITY MODEL USED
    !     FOR FD AND PL; GROUPED BY ICH AND IDF ONLY. THE VALUES ARE
    !     ANNUAL PERCENT MORTALITY: 0.153 = 0.153%/YR = 0.00153/YR

    data DBHBRK / 10.0, 25.0, 40.0 /
    data BABRK  / 20.0, 30.0, 40.0, 50.0 /

    !     EACH ROW IS A BA CLASS; COLUMNS ARE DBH CLASS

    !     ICH, PW (1)
    data ((FMRT(MRT_ICH,MRT_PW,I,J),J = 1,MRT_DBH_UB),I = 1,MRT_BA_UB)/&
         &0.153, 0.141, 0.088, 0.062,&
         &0.305, 0.281, 0.176, 0.124,&
         &0.305, 0.281, 0.176, 0.124,&
         &0.364, 0.266, 0.188, 0.124,&
         &0.462, 0.266, 0.188, 0.124 /

    !     IDF, PW - use ICH
    data ((FMRT(MRT_IDF,MRT_PW,I,J),J = 1,MRT_DBH_UB),I = 1,MRT_BA_UB)/&
         &0.153, 0.141, 0.088, 0.062,&
         &0.305, 0.281, 0.176, 0.124,&
         &0.305, 0.281, 0.176, 0.124,&
         &0.364, 0.266, 0.188, 0.124,&
         &0.462, 0.266, 0.188, 0.124 /

    !     ICH, LW  (2)
    data ((FMRT(MRT_ICH,MRT_LW,I,J),J = 1,MRT_DBH_UB),I = 1,MRT_BA_UB)/&
         &0.196, 0.185, 0.131, 0.058,&
         &0.393, 0.370, 0.263, 0.115,&
         &0.393, 0.350, 0.263, 0.115,&
         &0.456, 0.334, 0.263, 0.115,&
         &0.456, 0.334, 0.263, 0.115 /

    !     IDF, LW - use ICH
    data ((FMRT(MRT_IDF,MRT_LW,I,J),J = 1,MRT_DBH_UB),I = 1,MRT_BA_UB)/&
         &0.196, 0.185, 0.131, 0.058,&
         &0.393, 0.370, 0.263, 0.115,&
         &0.393, 0.350, 0.263, 0.115,&
         &0.456, 0.334, 0.263, 0.115,&
         &0.456, 0.334, 0.263, 0.115 /

    !     ICH, FD (3)
    data ((FMRT(MRT_ICH,MRT_FD,I,J),J = 1,MRT_DBH_UB),I = 1,MRT_BA_UB)/&
         &0.281, 0.072, 0.136, 0.123,&
         &0.563, 0.145, 0.272, 0.245,&
         &0.592, 0.297, 0.272, 0.245,&
         &0.400, 0.326, 0.181, 0.326,&
         &0.159, 0.385, 0.233, 0.252 /

    !     IDF, FD
    data ((FMRT(MRT_IDF,MRT_FD,I,J),J = 1,MRT_DBH_UB),I = 1,MRT_BA_UB)/&
         &0.265, 0.259, 0.305, 0.213,&
         &0.529, 0.518, 0.609, 0.427,&
         &0.515, 0.443, 0.332, 0.358,&
         &0.409, 0.647, 0.689, 0.163,&
         &0.409, 0.510, 0.247, 0.163 /

    !     ICH, BG (BL) (4,9)
    data ((FMRT(MRT_ICH,MRT_BG,I,J),J = 1,MRT_DBH_UB),I = 1,MRT_BA_UB)/&
         &0.103, 0.108, 0.064, 0.020,&
         &0.205, 0.216, 0.127, 0.039,&
         &0.480, 0.392, 0.216, 0.039,&
         &0.510, 0.223, 0.187, 0.079,&
         &0.202, 0.092, 0.070, 0.070 /

    !     IDF, BG (BL) - use ICH(4,9)
    data ((FMRT(MRT_IDF,MRT_BG,I,J),J = 1,MRT_DBH_UB),I = 1,MRT_BA_UB)/&
         &0.103, 0.108, 0.064, 0.020,&
         &0.205, 0.216, 0.127, 0.039,&
         &0.480, 0.392, 0.216, 0.039,&
         &0.510, 0.223, 0.187, 0.079,&
         &0.202, 0.092, 0.070, 0.070 /

    !     ICH, HW (5)
    data ((FMRT(MRT_ICH,MRT_HW,I,J),J = 1,MRT_DBH_UB),I = 1,MRT_BA_UB)/&
         &0.125, 0.101, 0.076, 0.045,&
         &0.251, 0.202, 0.153, 0.089,&
         &0.198, 0.330, 0.210, 0.089,&
         &0.288, 0.330, 0.210, 0.089,&
         &0.394, 0.200, 0.151, 0.103 /

    !     IDF, HW - use ICH
    data ((FMRT(MRT_IDF,MRT_HW,I,J),J = 1,MRT_DBH_UB),I = 1,MRT_BA_UB)/&
         &0.125, 0.101, 0.076, 0.045,&
         &0.251, 0.202, 0.153, 0.089,&
         &0.198, 0.330, 0.210, 0.089,&
         &0.288, 0.330, 0.210, 0.089,&
         &0.394, 0.200, 0.151, 0.103 /

    !     ICH, CW (6)
    data ((FMRT(MRT_ICH,MRT_CW,I,J),J = 1,MRT_DBH_UB),I = 1,MRT_BA_UB)/&
         &0.078, 0.068, 0.065, 0.059,&
         &0.157, 0.136, 0.130, 0.119,&
         &0.225, 0.142, 0.136, 0.130,&
         &0.295, 0.223, 0.151, 0.151,&
         &0.202, 0.092, 0.070, 0.049 /

    !     IDF, CW - use ICH
    data ((FMRT(MRT_IDF,MRT_CW,I,J),J = 1,MRT_DBH_UB),I = 1,MRT_BA_UB)/&
         &0.078, 0.068, 0.065, 0.059,&
         &0.157, 0.136, 0.130, 0.119,&
         &0.225, 0.142, 0.136, 0.130,&
         &0.295, 0.223, 0.151, 0.151,&
         &0.202, 0.092, 0.070, 0.049 /

    !     ICH, PL (7)
    data ((FMRT(MRT_ICH,MRT_PL,I,J),J = 1,MRT_DBH_UB),I = 1,MRT_BA_UB)/&
         &0.218, 0.317, 0.196, 0.116,&
         &0.435, 0.633, 0.391, 0.232,&
         &0.457, 0.607, 0.276, 0.232,&
         &0.391, 0.468, 0.405, 0.232,&
         &0.256, 0.388, 0.128, 0.232 /

    !     IDF, PL
    data ((FMRT(MRT_IDF,MRT_PL,I,J),J = 1,MRT_DBH_UB),I = 1,MRT_BA_UB)/&
         &0.282, 0.226, 0.130, 0.130,&
         &0.563, 0.452, 0.260, 0.260,&
         &0.493, 0.445, 0.195, 0.195,&
         &0.413, 0.592, 0.245, 0.245,&
         &0.413, 0.287, 0.433, 0.245 /

    !     ICH, SE (combined ICH+IDF) (8)
    data ((FMRT(MRT_ICH,MRT_SE,I,J),J = 1,MRT_DBH_UB),I = 1,MRT_BA_UB)/&
         &0.126, 0.101, 0.122, 0.058,&
         &0.253, 0.202, 0.244, 0.116,&
         &0.253, 0.202, 0.244, 0.116,&
         &0.253, 0.242, 0.242, 0.116,&
         &0.219, 0.182, 0.152, 0.116 /

    !     IDF, SE (combined ICH+IDF)
    data ((FMRT(MRT_IDF,MRT_SE,I,J),J = 1,MRT_DBH_UB),I = 1,MRT_BA_UB)/&
         &0.126, 0.101, 0.122, 0.058,&
         &0.253, 0.202, 0.244, 0.116,&
         &0.253, 0.202, 0.244, 0.116,&
         &0.253, 0.242, 0.242, 0.116,&
         &0.219, 0.182, 0.152, 0.116 /

    !     ICH, PY (same as IDF) (10)
    data ((FMRT(MRT_ICH,MRT_PY,I,J),J = 1,MRT_DBH_UB),I = 1,MRT_BA_UB)/&
         &0.168, 0.116, 0.089, 0.063,&
         &0.335, 0.231, 0.179, 0.127,&
         &0.320, 0.222, 0.092, 0.046,&
         &0.662, 0.434, 0.206, 0.220,&
         &0.662, 0.434, 0.206, 0.220 /

    !     IDF, PY
    data ((FMRT(MRT_IDF,MRT_PY,I,J),J = 1,MRT_DBH_UB),I = 1,MRT_BA_UB)/&
         &0.168, 0.116, 0.089, 0.063,&
         &0.335, 0.231, 0.179, 0.127,&
         &0.320, 0.222, 0.092, 0.046,&
         &0.662, 0.434, 0.206, 0.220,&
         &0.662, 0.434, 0.206, 0.220 /

    !     THESE ARE THE COEFFICIENTS OF A LOGISTIC SURVIVAL RELATIONSHIP
    !     FITTED BY TEMESGEN HAILEMARIAN, (UBC FORESTRY); DECEMBER 8,
    !     2001.

    TYPE (LMRT_STR) LMRT(P_MD)

    !     SITE SERIES COEFFICIENTS: NOTE THAT THERE IS A SPECIAL
    !     BEC STRING WITH 'all' AS THE SUBZONE. THIS IS USED
    !     WHEN THERE IS NOT A PERFECT MATCH TO THE SITE SERIES.

    data LMRT(1) /&
         &LMRT_STR (&
         &(/11,12,13,15, 0,0,0,0,0,0,0,0,0,0,0/),& ! EP,AT,CT,OH
         &(/'ICH/all        ', 'IDF/all        ',&
         &'SBPS/all       ', 'SBS/all        ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &(/-2.774444,  0.684626, -0.180211,  0.004550, -0.634960,&
         &0.063153, -0.276905,  0.000100 /))&
         &/

    !  CHECK FOR DEBUG.

    if (LCON) go to 9990
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
       do ISPCC = 1,CFG_MAXSP
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
    if (DEBUG)write(JOSTND,*)' IN MORTS CYCLE= ',ICYC,'  BAMAX= ',&
         &BAMAX,'  SDIMAX= ',SDIMAX

    !     ESTIMATE QUADRATIC MEAN DIAMETER 10 YEARS HENCE.

    T = 0.0
    SD2SQ = 0.0
    do I = 1,ITRN
       P = PROB(I)
       IS = ISP(I)
       D = DBH(I)
       BARK = BRATIO(IS,D,HT(I))
       G = DG(I)/BARK
       CIOBDS = (2.0*D*G+G*G)
       SD2SQ = SD2SQ + P *(D*D+CIOBDS)
       T = T + P
       20 continue
    end do
    DQ10 = SQRT(SD2SQ/T)
    DELTBA = .005454154*DQ10*DQ10*T-BA

    !     PROJECT BASAL AREA FORWARD 10 YEARS ASSUMING THAT BA/BAMAX
    !     PROPORTION OF THE PREDICTED BASAL AREA INCREMENT WILL BE LOST TO
    !     MORTALITY.  AT BA=BAMAX, MORTALITY WILL ABSORB ALL INCREMENT.

    BA10 = BA+((BAMAX-BA)/BAMAX)*DELTBA
    TB = BA10/(.005454154*DQ10*DQ10)

    !     NOW CONVERT TO AN ESTIMATE ON ANNUAL TPA MORTALITY RATE.

    TTB = (T-TB)/T
    if (TTB.gt.0.9999) TTB = 0.9999
    RZ = 1.0-(1.0-(TTB))**0.1

    !     CALCULATE AVERAGE DBH

    DSUM = 0.0
    WPROB = 0.0
    do I = 1,ITRN
       WPROB = WPROB+PROB(I)
       DSUM = DSUM+DBH(I)*PROB(I)
    end do
    AVED = DSUM/WPROB

    !     METRIC BA & SPH

    BAX = BA * FT2pACRtoM2pHA
    SPH = T  * HAtoACR

    !     GET STAND BA-CLASS FOR SPECIAL FD AND PL MODELS
    !       1 =   0 - 20
    !       2 = >20 - 30
    !       3 = >30 - 40
    !       4 = >40 - 50
    !       5 = >50+

    BACLS = MRT_BA_UB
    do I = 1, (MRT_BA_UB - 1)
       if (BAX .le. BABRK(I)) then
          BACLS = I
          exit
       end if
    end do

    !  START LOOP TO ESTIMATE MORTALITY RATE.  TREES ARE PROCESSED
    !  ONE AT A TIME WITHIN A SPECIES.

    do ISPC = 1,CFG_MAXSP

       I1 = ISCT(ISPC,1)
       if (I1.le.0) go to 50
       I2 = ISCT(ISPC,2)
       if (LV2ATV) then
          GMULT(1) = GMULT_C(1,ISPC)
          GMULT(2) = GMULT_C(2,ISPC)
          REIN(1) = REIN_C(1,ISPC)
          REIN(2) = REIN_C(2,ISPC)
          B5 = PMSC(ISPC)
       else
          !         FIND AND SET INDEX FOR TABULAR MORTALITY MODELS
          MRTCLS = 0
          B5 = 0.0
          if (ISPC .eq.1) then
             MRTCLS = MRT_PW
          else if (ISPC .eq.2) then
             MRTCLS = MRT_LW
          else if (ISPC .eq.3.or. ISPC .eq.14) then
             MRTCLS = MRT_FD
          else if (ISPC .eq.4.or. ISPC .eq.9) then
             MRTCLS = MRT_BG
          else if (ISPC .eq.5) then
             MRTCLS = MRT_HW
          else if (ISPC .eq.6) then
             MRTCLS = MRT_CW
          else if (ISPC .eq.7) then
             MRTCLS = MRT_PL
          else if (ISPC .eq.8) then
             MRTCLS = MRT_SE
          else if (ISPC .eq.10) then
             MRTCLS = MRT_PY
          end if
       end if

       XMORT = XMMULT(ISPC)
       D1 = XMDIA1(ISPC)
       D2 = XMDIA2(ISPC)

       !       START TREE LOOP WITHIN SPECIES.

       do I3 = I1,I2

          !         INITIALIZE FOR NEXT TREE.

          if (DEBUG) write(JOSTND,*)'MORTS: 40 loop'
          if (DEBUG) write(JOSTND,*)'MORTS: I3,I1,I2', I3,I1,I2
          I = IND1(I3)
          P = PROB(I)
          WKI = 0.0
          WK2(I) = 0.0
          if (P.le.0.0) go to 40
          D = DBH(I)
          BARK = BRATIO(ISPC,D,HT(I))
          if (DEBUG) write(JOSTND,*)'MORTS: I,P,D,BARK', I3,I1,I2,BARK

          if (LV2ATV) then

             RELDBH = D/AVED
             if (D.le.0.5)D = 0.5
             DGT = WK1(I)/OLDFNT
             if (D.le.1.0.and. DGT.lt.0.05) DGT = 0.05
             if (D.le.5.0.and. D.gt.1.0.and. DGT.lt.0.05)&
                  &DGT = 0.05*(5.0-D)/4.0
             G = WK1(I)/(BARK*OLDFNT)
             if (WK1(I)/OLDFNT .lt. DGT)G = DGT/BARK

             !           COMPUTE MORTALITY RATE

             if ((ICYC.eq.1.or. WK1(I).eq.0.0) .and. DG(I).gt.0.5)&
                  &G = DG(I)/(BARK*10.0)
             IP = 1
             if (D.le.5.0)IP = 2
             G = G*GMULT(IP)
             RIP = 2.76253+0.222310*SQRT(D)-0.0460508*SQRT(BA)+&
                  &11.2007*G-0.554421/D+B5+0.246301*RELDBH+6.07129*G/D
             if (RIP.gt.70.0) RIP = 70.0
             if (RIP.lt.-70.0) RIP = -70.0
             RIP = (1.0/(1.0+EXP(RIP)))
             POTENT = REIN(IP)
             RIP = RIP*POTENT

          else

             ! DBH is at least 2.5cm; important for HW mortality
             DX = MAX(2.5, (D*INtoCM))
             RELDBH = DX/MAX(2.5,AVED*INtoCM)

             if (MRTCLS .gt.0) then

                !             GET DBH-CLASS FOR TABULAR MODELS
                !               1 =   0 - 10
                !               2 = >10 - 25
                !               3 = >25 - 40
                !               4 = >40

                DBHCLS = MRT_DBH_UB
                do J = 1,(MRT_DBH_UB - 1)
                   if (DX .le. DBHBRK(J)) then
                      DBHCLS = J
                      exit
                   end if
                end do
                RIP = TMRT(MRTCLS,BACLS,DBHCLS)

             else

                !             COMPUTE MORTALITY RATE; SPECIES WITH NO AVAILABLE ESTIMATE
                !             HAVE COMPLETE MORTALITY. NOTE THAT THE MORT%() EXPRESSION
                !             INITIALLY COMPUTES SURVIVAL. AFTER EXPONENTIATION IT IS
                !             CONVERTED TO MORTALITY: (1.0 - SURVIVAL)

                BAL = (1.0 - (PCT(I)/100.)) * BAX
                !             IF (MORT%FIT(ISPC) .AND. LLTDGOK(ISPC)) THEN
                if (MORT%FIT(ISPC)) then
                   RIP = MORT%CON(ISPC)&
                        &+ (MORT%INVDBH(ISPC)  / DX)&
                        &+ (MORT%DBH(ISPC)     * DX)&
                        &+ (MORT%DBHSQ(ISPC)   * DX*DX)&
                        &+ (MORT%RDBH(ISPC)    * RELDBH)&
                        &+ (MORT%BAL(ISPC)     * BAL)&
                        &+ (MORT%SQRTBA(ISPC)  * SQRT(BAX))&
                        &+ (MORT%SPH(ISPC)     * SPH)
                else
                   RIP = 99.0
                end if
                RIP = MAX(-70.0, MIN(70.0, RIP))
                RIP = 1.0 - (1.0 / (1.0 + EXP(RIP)))
             end if

          end if

          !         MAKE ADJUSTMENT FOR APPROACH TO MAXIMUM BASAL AREA.

          RIPP = BA*RZ
          if (BA.gt.BAMAX) goto 27
          RIPP = RIPP+(BAMAX-BA)*RIP
          27 RIPP = RIPP/BAMAX
          if (RIPP.lt.RIP) RIPP = RIP
          if (RIPP.gt.1.0) RIPP = 1.0

          !  APPLY MORTALITY MULTIPLIERS WITHIN USER-PROVIDED DBH LIMITS
          !  AND STORE TREES PER ACRE DYING IN THE ARRAY WK2.

          X = 1.0
          if (D.ge.D1.and.D.lt.D2) X = XMORT

          !  FROM HERE TO STATEMENT 29 IS ESTABLISHMENT MODEL CODE.
          !  "BEST" TREES NOT SUBJECT TO MORTALITY FOR 20 YRS
          !  AFTER DISTURBANCE DATE.

          if (IESTAT(I).le.0) go to 29
          if (IY(ICYC).ge.IESTAT(I)) IESTAT(I) = 0
          XCHECK = FLOAT(IESTAT(I)-IY(ICYC))/FINT
          if (XCHECK.gt.1.0) XCHECK = 1.0
          if (XCHECK.lt.0.0) XCHECK = 0.0
          X = X*(1.0-XCHECK)
          29 continue

          !  APPLY 60 PERCENT OF NI RATE FOR LM,PY,AS,CO,MM,PB,OH. - New in NI
          !  DO NOT APPLY TO BC

          WKI = P*(1.0-(1.0- RIPP)**FINT)*X !!!*0.6

          BARK = BRATIO(ISPC,D,HT(I))
          G = (DG(I)/BARK) * (FINT/10.0)
          IDMFLG = IFIX(SIZCAP(ISPC,3))
          if ((D+G).ge.SIZCAP(ISPC,1) .and. IDMFLG.ne.1) then
             WKI = AMAX1(WKI,(P*SIZCAP(ISPC,2)*FINT/10.0))
             if (DEBUG)write(JOSTND,*)' SIZE CAP RESTRICTION IMPOSED, ',&
                  &'I,ISPC,D,P,SIZCAP 1-3,WKI = ',&
                  &I,ISPC,D,P,SIZCAP(ISPC,1),SIZCAP(ISPC,2),SIZCAP(ISPC,3),WKI
          end if
          if (WKI .gt. P) WKI = P

          ! IF SDIMAX IS LESS THAN 5, ASSUME CLIMATE HAS CHANGED ENOUGH THAT THE
          ! SITE WILL NO LONGER SUPPORT TREES, AND KILL ALL EXISTING TREES.

          if (SDIMAX .lt.5) then
             WKI = P
          end if
          WK2(I) = WKI

          !  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.

          if (.not.DEBUG) go to 40
          PRES = P - WKI
          VLOS = WKI * CFV(I) / FINT
          write(JOSTND,9000) I,ISPC,D,G,RELDBH,BA,P,WKI,PRES,VLOS
          9000 format(' IN MORTS, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,&
               &',  DBH INCREMENT=',F7.4,',  RELDBH=',F7.4,',  BA=',F7.2,&
               &',  INIT PROB=',F9.3/&
               &', TREES DYING=',F9.3,'  RES PROB=',F9.3,&
               &',  VOL LOST=',F9.3)
          40 continue
       end do

       !  END OF SPECIES LOOP.  PRINT DEBUG INFO IF DESIRED.

       if (DEBUG) write(JOSTND,*)'MORTS: ISPC', ISPC
       if (DEBUG) write(JOSTND,*)'MORTS: B5', B5 !!! Not defined

       if (.not.DEBUG) go to 50
       write(JOSTND,9001)  ISPC,B5
       9001 format('IN MORTS,  ISPC=',I3,&
            &11X,'B5=',F8.6)
       50 continue
    end do

    100 continue

    !  COMPUTE THE CLIMATE-PREDICTED MORTALITY RATES BY SPECIES

    if (DEBUG) write(JOSTND,*)'IN MORTS: CALLING CLMORTS'
    call CLMORTS
    if (DEBUG) write(JOSTND,*)'IN MORTS: RETURNED CLMORTS'

    return

    9990 continue

    !  ENTRY POINT FOR LOADING MORTALITY MODEL CONSTANTS THAT REQUIRE ONE-
    !  TIME RESOLUTION.

    if (LV2ATV) then

       !  TIME RESOLUTION.  ITYPE IS A HABITAT INDEX THAT IS ASSIGNED IN
       !  **BECSET** .

       IPT = IPDG(ITYPE,IFOR)
       POTEN = POT(IPT)
       do ISPC = 1,CFG_MAXSP
          if (LLTDGOK(ISPC)) then
             GMULT_C(1,ISPC) = .90/(POTEN*V2SEICOR(ISPC))
             REIN_C(1,ISPC) = &
                  &(1-(POTEN*V2SEICOR(ISPC)/20.+1.)**(-1.605))/.06821
          else
             GMULT_C(1,ISPC) = .90/POTEN
             REIN_C(1,ISPC) = (1-(POTEN/20.+1.)**(-1.605))/.06821
          end if
       end do

       IPT2 = IPDG2(ITYPE,IFOR)
       POTEN = POT(IPT2)
       do ISPC = 1,CFG_MAXSP
          if (LLTDGOK(ISPC)) then
             GMULT_C(2,ISPC) = 2.50/(POTEN*V2SEICOR(ISPC))
             REIN_C(2,ISPC) = &
                  &(1-(POTEN*V2SEICOR(ISPC)+1.)**(-1.605))/.86610
          else
             GMULT_C(2,ISPC) = 2.50/POTEN
             REIN_C(2,ISPC) = (1-(POTEN+1.)**(-1.605))/.86610
          end if
       end do

    else

       !       REPLACE NULLS IN COMPILED DATA STRINGS WITH SPACES, SO THAT
       !       STRING LENGTHS WILL BE FIGURED CORRECTLY
       do J = 1,P_MD
          do K = 1,P_SS
             do M = 1,15
                if (ICHAR(LMRT(J)%PrettyName(K)(M:M)) .eq.0)&
                     &LMRT(J)%PrettyName(K)(M:M) = ' '
             end do
          end do
       end do

       !       LOCATE ZONE AND SITE SERIES; IF NO PERFECT MATCH TO SUBZONE
       !       CAN BE FOUND, TRY ".../all" AND LOOP ONE MORE TIME BEFORE
       !       GIVING UP ON A MATCH.
       MORT%PrettyName = BEC%PrettyName
       do I = 1,CFG_MAXSP
          IPOS(I) = 0
       end do
       do I = 1,CFG_MAXSP
          IPAS = 0
          PAT = BEC%PrettyName
          7 do J = 1, P_MD
             do K = 1,CFG_MAXSP
                if (LMRT(J)%SPP(K) .eq.0) exit
                if (LMRT(J)%SPP(K) .eq. I) then
                   do M = 1, P_SS
                      if (LEN_TRIM(LMRT(J)%PrettyName(M)) .eq.0) exit
                      if (INDEX(LMRT(J)%PrettyName(M),PAT) .gt.0) then
                         IPOS(I) = J
                         goto 8
                      end if
                   end do
                end if
             end do
          end do

          if (J .gt. P_MD .and. IPAS .eq.0) then
             IPAS = 1
             PAT = BEC%Zone(1:LEN_TRIM(BEC%Zone))//'/all '
             goto 7
          end if
       8 end do

       !       COPY COEFFICIENTS INTO MORT STRUCTURE
       do I = 1,CFG_MAXSP
          MORT%FIT(I) = .false.
          J = IPOS(I)
          if (J .gt.0) then
             MORT%FIT(I) = .true.
             MORT%CON(I) = LMRT(J)%COEF(1)
             MORT%INVDBH(I) = LMRT(J)%COEF(2)
             MORT%DBH(I) = LMRT(J)%COEF(3)
             MORT%DBHSQ(I) = LMRT(J)%COEF(4)
             MORT%RDBH(I) = LMRT(J)%COEF(5)
             MORT%BAL(I) = LMRT(J)%COEF(6)
             MORT%SQRTBA(I) = LMRT(J)%COEF(7)
             MORT%SPH(I) = LMRT(J)%COEF(8)
          end if
       end do

       !       COPY COEFFICIENTS FOR TABULAR MORTALITY MODELS IN ICH- AND IDF-ZONES
       !       CONVERSION: FMRT(...)/100 CONVERTS ANNUAL % MORTALITY (FROM TEMESGEN'S
       !       TABLE) TO ANNUAL PROPORTION MORTALITY.

       BECCLS = 1
       if (INDEX(BEC%Zone,'ICH') .gt.0) BECCLS = 1
       if (INDEX(BEC%Zone,'IDF') .gt.0) BECCLS = 2
       !           MAP SBPS TO IDF
       if (INDEX(BEC%Zone,'SBPS') .gt.0) BECCLS = 2
       !           MAP SBS TO ICH, but ONE subzone will be mapped differently
       if (INDEX(BEC%Zone,'SBS') .gt.0) BECCLS = 1
       if (INDEX(BEC%PrettyName,'SBSdw2') .gt.0) BECCLS = 2

       do I = 1,MRT_PY
          do J = 1,MRT_BA_UB
             do K = 1,MRT_DBH_UB
                !!              TMRT(I,J,K) =
                !!     >          EXP(LOG(1.0 - (FMRT(BECCLS,I,J,K) * 0.1)) / 10.0)
                TMRT(I,J,K) = FMRT(BECCLS,I,J,K)/100.0
             end do
          end do
       end do

    end if

    return

  end subroutine MORBC

  subroutine MORON(LCON)
    implicit none

    ! ON'S PUBLIC VARMRT TAKES DQ10 AS A FOURTH ARGUMENT. THIS LOCAL
    ! INTERFACE KEEPS ITS CONTRACT DISTINCT FROM MORSDI'S VARMRT CALLS.
    interface
       subroutine VARMRT(TOKILL,DEBUG,SUMKIL,DQ10)
         real :: TOKILL,SUMKIL,DQ10
         logical :: DEBUG
       end subroutine VARMRT
    end interface

    !  ON: MOVED UNCHANGED EXCEPT THAT MORTMULT AND FIXMORT ARE THE SHARED
    !  MMULT AND FIXMRT.

    logical :: LCON

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

    integer :: I,IDMFLG,INDX,INDX_BA
    integer :: IPASS,IPATH,IS,ISPC,I1,I2,I3
    integer :: KNT,KNT2

    integer :: BKG_MAP(72),MSB_MAP(72)

    real    :: ADJFAC,BADEAD,BANEW,BARK,BRATIO,B0,B1
    real    :: CEPT,CIOBDS,CONST
    real    :: D,DBHEND,DIA0,DIFF,DR0,DR10,DR10N,DQ0,DQ10
    real    :: DQ10N,D1,D2,D10,D10N,D55M,D85M,G,P,PRES,QMDNEW
    real    :: RI,RIP,RN
    real    :: SDIINT,SDISLP,SDQ0,SD2SQ,SD2SQN,SLP
    real    :: SUMBA,SUMDR0,SUMDR10,SUMDR10N,SUMKIL,SUMTRE
    real    :: T,TBAM1,TEM,TEMQMD,TEMEFF,TEMINT,TEMP,TEMSLP,TMD0,TMD10
    real    :: TMMSB,TMORE,TN,TNEW,TN10,TPACLS,TPHMAX
    real    :: TPRIME,TREEIT,T55D0,T55D10,T55M,T85D0,T85D10,T85M,T85MSB
    real    :: VLOS,WKI,X,XMORT

    real    :: BASP(72)
    real    :: MSB_DBH(13),MSB_INT(13),MSB_SLP(13)
    real    :: PMSC(4),PMD(4)
    real    :: SDI_INT(13),SDI_SLP(13)

    ! Data statements

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
    if (LCON) go to 9990
    call DBCHK (DEBUG,'MORTS',5,ICYC)
    if (DEBUG)write(JOSTND,9000)ICYC
    9000 format(' ENTERING SUBROUTINE MORTS  CYCLE =',I4)

    !     INITIALIZATIONS

    TREEIT = 0.
    KNT = 0
    TPHMAX = 35000.0*HAtoACR

    !     PROCESS MORTMULT KEYWORD.

    call MMULT(.false.)
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
    do ISPC = 1,CFG_MAXSP
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
          SDQ0 = SDQ0+P*(D)**2
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
    do ISPC = 1,CFG_MAXSP
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

    do ISPC = 1,CFG_MAXSP
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
               &/,10X,' OF ',F8.1,' TREES/ACRE.  ',&
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
       BANEW = BANEW+(0.0054542*(D+G)**2)*P
       BADEAD = BADEAD+(0.0054542*(D+G)**2)*WK2(I)
       QMDNEW = QMDNEW + ((D+G)**2)*P
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

    return

    9990 continue

    !  ENTRY POINT FOR LOADING MORTALITY MODEL CONSTANTS THAT
    !  REQUIRE ONE-TIME RESOLUTION.

    CEPMRT = 0.
    SLPMRT = 0.
    TPAMRT = 0.
    return

  end subroutine MORON

  subroutine MMULT(LSKIP)
    implicit none

    !  PROCESS MORTMULT KEYWORD.  LSKIP IS TRUE FOR THE VARIANTS (CI, EM,
    !  IE, KT, OP, PN, TT, WC) THAT SKIP ACTIVITIES WITH A NEGATIVE CODE.

    logical :: LSKIP
    integer :: NTODO,IDATE,IACTK,NP,ISPCC

    call OPFIND(1,MYACTS(1),NTODO)
    if (NTODO .eq.0) return
    do I = 1,NTODO
       call OPGET(I,4,IDATE,IACTK,NP,PRM)
       if (LSKIP) then
          if (IACTK.lt.0) cycle
       end if
       call OPDONE(I,IY(ICYC))
       ISPCC = IFIX(PRM(1))
       if (ISPCC .eq.0) then
          do ISPCC = 1,CFG_MAXSP
             XMMULT(ISPCC) = PRM(2)
             XMDIA1(ISPCC) = PRM(3)
             XMDIA2(ISPCC) = PRM(4)
          end do
       else
          XMMULT(ISPCC) = PRM(2)
          XMDIA1(ISPCC) = PRM(3)
          XMDIA2(ISPCC) = PRM(4)
       end if
    end do
  end subroutine MMULT

  subroutine PRGAVE(DX)
    implicit none

    !  STAND TERMS FOR THE PROGNOSIS EQUATION.  DX IS THE END-OF-PERIOD
    !  QUADRATIC MEAN DIAMETER (D10 IN EM AND TT, DQ10 IN CI, IE AND KT).

    real    :: DX

    BA10 = BA+((BAMAX-BA)/BAMAX)*DELTBA
    TB = BA10/(.005454154*DX*DX)

    !  NOW CONVERT TO AN ESTIMATE ON ANNUAL TPA MORTALITY RATE.

    TTB = (T-TB)/T
    if (TTB.gt.0.9999) TTB = 0.9999
    RZ = 1.0-(1.0-(TTB))**0.1

    !  CALCULATE AVERAGE DBH

    DSUM = 0.0
    WPROB = 0.0
    do I = 1,ITRN
       WPROB = WPROB+PROB(I)
       DSUM = DSUM+DBH(I)*PROB(I)
    end do
    AVED = DSUM/WPROB

  end subroutine PRGAVE

  subroutine PRGTRE(B,CLAMP)
    implicit none

    !  PROGNOSIS (HAMILTON) ANNUAL MORTALITY RATE RIPP AND MULTIPLIER X FOR
    !  TREE I OF SPECIES ISPC, WITH SPECIES CONSTANT B.  THE LOGIT IS CLAMPED
    !  AT +/-CLAMP.  D MAY BE RAISED TO 0.5 AND G IS LEFT AS THE ADJUSTED DBH
    !  INCREMENT, AS THE CALLERS' DIAGNOSTICS EXPECT.

    real    :: B,CLAMP

    RELDBH = D/AVED
    if (D.le.0.5)D = 0.5
    DGT = WK1(I)/OLDFNT
    if (D.le.1.0.and. DGT.lt.0.05) DGT = 0.05
    if (D.le.5.0.and. D.gt.1.0.and. DGT.lt.0.05)&
         &DGT = 0.05*(5.0-D)/4.0
    G = WK1(I)/(BARK*OLDFNT)
    if (WK1(I)/OLDFNT .lt. DGT)G = DGT/BARK

    !  COMPUTE MORTALITY RATE

    if ((ICYC.eq.1.or. WK1(I).eq.0.0) .and. DG(I).gt.0.5)&
         &G = DG(I)/(BARK*10.0)
    IP = 1
    if (D.le.5.0)IP = 2
    G = G*GMULT(IP)
    RIP = 2.76253+0.222310*SQRT(D)-0.0460508*SQRT(BA)+&
         &11.2007*G-0.554421/D+B+0.246301*RELDBH+6.07129*G/D
    if (RIP.gt.CLAMP) RIP = CLAMP
    if (RIP.lt.-CLAMP) RIP = -CLAMP
    RIP = (1.0/(1.0+EXP(RIP)))
    POTENT = REIN(IP)
    RIP = RIP*POTENT
    RIPP = BA*RZ

    !  MAKE ADJUSTMENT FOR APPROACH TO MAXIMUM BASAL AREA.

    if (BA.gt.BAMAX)go to 27
    RIPP = RIPP+(BAMAX-BA)*RIP
    27 RIPP = RIPP/BAMAX
    if (RIPP.lt.RIP)RIPP = RIP
    if (RIPP.gt.1.0)RIPP = 1.0

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
  end subroutine PRGTRE

  subroutine MORCOF
    implicit none

    !  BACKGROUND MORTALITY COEFFICIENTS FOR SPECIES ISPC: B0, B1 (AND B2 IN
    !  EM) FOR MORSDI, B5 FOR MORPRG.  NATIVE TABLES OF THE FORMER VARIANT
    !  COPIES; CA/OC, CR AND CS/LS/NE MAP SPECIES TO SHADE-TOLERANCE GROUPS.

    real    :: PMSCBM(18),PMDBM(18),PMSCCA(7),PMDCA(7),PMSCCR(6),PMDCR(6)
    real    :: PMSCEC(32),PMDEC(32),PMSCEM(19),PMDEM(19),PMDSQEM(19)
    real    :: PMSCNC(12),PMDNC(12),PMSCSN(90),PMDSN(90),PMSCSO(33),PMDSO(33)
    real    :: PMSCTT(18),PMDTT(18),PMSCUT(24),PMDUT(24),PMSCLS(4),PMDLS(4)
    real    :: PMSCWS(43),PMDWS(43),PMSCIE(23),PMSCKT(11),PMSCCI(19)
    integer :: IBGMCA(50),IBGMCR(38),IMAPCS(96),IMAPLS(68),IMAPNE(108)

    ! BM
    data PMSCBM/6.5112, 6.5112, 7.2985, 5.1677, 9.6943,&
         &5.1677, 5.9617, 9.6943, 5.1677, 5.5877,&
         &6.5112, 6.5112, 5.5877, 5.5877, 5.1677,&
         &5.5877, 5.5877, 5.9617/
    data PMDBM/-.0052485, -.0052485, -.0129121, -.0077681, -.0127328,&
         &-.0077681, -.0340128, -.0127328, -.0077681,  -.005348,&
         &-.0052485, -.0052485,  -.005348,  -.005348, -.0077681,&
         &-0.005348,  -.005348, -.0340128/
    ! CA AND OC
    data PMSCCA/ 6.5112, 7.2985, 5.1677, 9.6943, 5.9617, 5.5877,&
         &2.596805/
    data PMDCA/ -.0052485, -.0129121, -.0077681, -.0127328,&
         &-.0340128, -.005348, 0.512610 /
    data IBGMCA/ 3, 3, 3, 3, 3, 3, 2, 4, 4, 1,&
         &1, 5, 1, 1, 6, 1, 1, 6, 6, 6,&
         &3, 4, 7, 3, 3, 24*3, 7 /
    ! CR
    data PMSCCR/ 6.5112, 7.2985, 5.1677, 9.6943, 5.9617, 5.5877 /
    data PMDCR/ -.0052485, -.0129121, -.0077681, -.0127328,&
         &-.0340128, -.005348 /
    data IBGMCR/ 3, 3, 2, 3, 3, 4, 3, 1, 3, 3,&
         &5, 3, 6, 3, 1, 3, 4, 4, 4, 5,&
         &5, 5, 5, 5, 5, 5, 5, 5, 3, 3,&
         &3, 3, 3, 3, 3, 6, 3, 5/
    ! EC
    data PMSCEC/&
         &6.5112,    6.5112,    7.2985,    5.1677,    9.6943,&
         &5.1677,    5.9617,    9.6943,    5.1677,    5.5877,&
         &5.1677,    5.1677,    9.6943,    6.5112,    6.5112,&
         &5.1677,    5.9617,    5.1677,    5.5877,    5.9617,&
         &5.9617,    5.5877,    5.5877,    6.5112,    9.6943,&
         &5.9617,    5.9617,    6.5112,    5.5877,    5.9617,&
         &5.1677,    5.9617/
    data PMDEC/&
         &-.0052485, -.0052485, -.0129121, -.0077681, -.0127328,&
         &-.0077681, -.0340128, -.0127328, -.0077681,  -.005348,&
         &-.0077681, -.0077681, -.0127328, -.0052485, -.0052485,&
         &-.0077681, -.0340128, -.0077681,  -.005348, -.0340128,&
         &-.0340128,  -.005348,  -.005348, -.0052485, -.0127328,&
         &-.0340128, -.0340128, -.0052485,  -.005348, -.0340128,&
         &-.0077681, -.0340128/
    ! EM
    data PMSCEM/&
         &5.45676,    5.26043,    5.55086,        0.0,    0.21180,&
         &0.0,    3.87794,    6.41265,    5.88697,    5.58766,&
         &0.0,        0.0,        0.0,        0.0,        0.0,&
         &0.0,        0.0,    7.47709,        0.0/
    data PMDEM/&
         &-.0118233, -0.0097092, -0.0129121,        0.0,        0.0,&
         &0.0,      .3078, -0.0127328, -0.0333752, -0.0052485,&
         &0.0,        0.0,        0.0,        0.0,        0.0,&
         &0.0,        0.0, -0.0395156,        0.0/
    data PMDSQEM/&
         &0.0,        0.0,        0.0,        0.0,        0.0,&
         &0.0,    -0.0174,        0.0,        0.0,        0.0,&
         &0.0,        0.0,        0.0,        0.0,        0.0,&
         &0.0,        0.0,        0.0,        0.0/
    ! NC
    data PMSCNC/6.5112,6.5112,7.2985,5.1677,9.6943,5.1677,&
         &5.9617,9.6943,5.1677,5.5877,5.1677,2.59680/
    data PMDNC/-.0052485,-.0052485,-.0129121,-.0077681,&
         &-.0127328,-.0077681,-.0340128,-.0127328,&
         &-.0077681,-.005348,-.0077681, 0.51261/
    ! SN
    data PMSCSN /&
         &5.1676998,   9.6942997,   5.1676998,   5.5876999,   5.5876999,&
         &5.5876999,   5.1676998,   5.5876999,   5.5876999,   5.5876999,&
         &5.5876999,   5.5876999,   5.5876999,   5.5876999,   5.5876999,&
         &5.5876999,   5.1676998,   5.1676998,   5.1676998,   5.1676998,&
         &5.1676998,   5.1676998,   5.1676998,   5.9617000,   5.1676998,&
         &5.1676998,   5.9617000,   5.9617000,   5.9617000,   5.1676998,&
         &5.1676998,   5.1676998,   5.1676998,   5.1676998,   5.9617000,&
         &5.9617000,   5.1676998,   5.9617000,   5.1676998,   5.1676998,&
         &5.1676998,   5.9617000,   5.9617000,   5.9617000,   5.9617000,&
         &5.1676998,   5.9617000,   5.1676998,   5.9617000,   5.1676998,&
         &5.9617000,   5.1676998,   5.9617000,   5.1676998,   5.9617000,&
         &5.1676998,   5.1676998,   5.1676998,   5.9617000,   5.9617000,&
         &5.9617000,   5.9617000,   5.9617000,   5.9617000,   5.9617000,&
         &5.9617000,   5.9617000,   5.1676998,   5.9617000,   5.9617000,&
         &5.9617000,   5.9617000,   5.9617000,   5.9617000,   5.9617000,&
         &5.9617000,   5.9617000,   5.9617000,   5.9617000,   5.1676998,&
         &5.1676998,   5.1676998,   5.1676998,   5.1676998,   5.1676998,&
         &5.1676998,   5.1676998,   5.5876999,   5.9617000,   5.9617000/
    data PMDSN /&
         &-0.0077681,  -0.0127328,  -0.0077681,  -0.0053480,  -0.0053480,&
         &-0.0053480,  -0.0077681,  -0.0053480,  -0.0053480,  -0.0053480,&
         &-0.0053480,  -0.0053480,  -0.0053480,  -0.0053480,  -0.0053480,&
         &-0.0053480,  -0.0077681,  -0.0077681,  -0.0077681,  -0.0077681,&
         &-0.0077681,  -0.0077681,  -0.0077681,  -0.0340128,  -0.0077681,&
         &-0.0077681,  -0.0340128,  -0.0340128,  -0.0340128,  -0.0077681,&
         &-0.0077681,  -0.0077681,  -0.0077681,  -0.0077681,  -0.0340128,&
         &-0.0340128,  -0.0077681,  -0.0340128,  -0.0077681,  -0.0077681,&
         &-0.0077681,  -0.0340128,  -0.0340128,  -0.0340128,  -0.0340128,&
         &-0.0077681,  -0.0340128,  -0.0077681,  -0.0340128,  -0.0077681,&
         &-0.0340128,  -0.0077681,  -0.0340128,  -0.0077681,  -0.0340128,&
         &-0.0077681,  -0.0077681,  -0.0077681,  -0.0340128,  -0.0340128,&
         &-0.0340128,  -0.0340128,  -0.0340128,  -0.0340128,  -0.0340128,&
         &-0.0340128,  -0.0340128,  -0.0077681,  -0.0340128,  -0.0340128,&
         &-0.0340128,  -0.0340128,  -0.0340128,  -0.0340128,  -0.0340128,&
         &-0.0340128,  -0.0340128,  -0.0340128,  -0.0340128,  -0.0077681,&
         &-0.0077681,  -0.0077681,  -0.0077681,  -0.0077681,  -0.0077681,&
         &-0.0077681,  -0.0077681,  -0.0053480,  -0.0340128,  -0.0340128/
    ! SO
    data PMSCSO/&
         &6.5112, 6.5112, 7.2985, 5.1677, 9.6943,&
         &5.1677, 5.9617, 9.6943, 5.1677, 5.5877,&
         &5.1677, 5.1677, 5.1677, 5.1677, 5.1677,&
         &6.5112, 5.9617, 5.1677, 7.2985, 5.5877,&
         &5.9617, 5.9617, 5.5877, 5.1677, 5.5877,&
         &5.9617, 5.9617, 5.9617, 5.5877, 5.9617,&
         &5.9617, 7.2985, 5.9617/
    data PMDSO/&
         &-0.0052485, -0.0052485, -0.0129121, -0.0077681, -0.0127328,&
         &-0.0077681, -0.0340128, -0.0127328, -0.0077681,  -0.005348,&
         &-0.0077681, -0.0077681, -0.0077681, -0.0077681, -0.0077681,&
         &-0.0052485, -0.0340128, -0.0077681, -0.0129121,  -0.005348,&
         &-0.0340128, -0.0340128,  -0.005348, -0.0077681,  -0.005348,&
         &-0.0340128, -0.0340128, -0.0340128,  -0.005348, -0.0340128,&
         &-0.0340128, -0.0129121, -0.0340128/
    ! TT
    data PMSCTT/&
         &6.5112, 6.5112, 7.2985, 5.1677, 9.6943,&
         &5.1677, 5.9617, 9.6943, 5.1677, .21180,&
         &5.1677, 5.1677, 5.5877, 5.1677, 5.9617,&
         &5.9617, 5.1677, 5.9617/
    data PMDTT/&
         &-.0052485, -.0052485, -.0129121, -.0077681, -.0127328,&
         &-.0077681, -.0340128, -.0127328, -.0077681,        0.,&
         &-.0077681, -.0077681, -0.005348, -.0077681, -.0052485,&
         &-.0340128, -.0077681, -.0052485/
    ! UT
    data PMSCUT/&
         &6.5112,    6.5112,    7.2985,    5.1677,    9.6943,&
         &5.1677,    5.9617,    9.6943,    5.1677,    5.5877,&
         &5.1677,    5.1677,    5.1677,    5.1677,    5.1677,&
         &5.1677,    5.1677,    5.9617,    5.9617,    5.9617,&
         &5.5877,    5.9617,    5.1677,    5.1677/
    data PMDUT/&
         &-.0052485, -.0052485, -.0129121, -.0077681, -.0127328,&
         &-.0077681, -.0340128, -.0127328, -.0077681,  -.005348,&
         &-.0077681, -.0077681, -.0077681, -.0077681, -.0077681,&
         &-.0077681, -.0077681, -.0052485, -.0052485,-0.0340128,&
         &-0.005348, -.0052485, -.0077681, -.0077681/
    ! CS, LS AND NE (SHADE-TOLERANCE GROUPS)
    data PMSCLS/5.1676998,   9.6942997,   5.5876999,   5.9617000/
    data PMDLS/-0.0077681,  -0.0127328,  -0.0053480,  -0.0340128/
    data IMAPCS/&
         &3, 2, 3, 3, 3, 2, 3, 4, 4, 1, 4, 4, 1, 4, 4, 4, 4, 4, 4, 4,&
         &4, 4, 4, 1, 4, 1, 1, 4, 1, 4, 1, 4, 1, 4, 4, 1, 1, 1, 1, 1,&
         &4, 1, 1, 1, 4, 1, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,&
         &4, 4, 4, 4, 4, 4, 4, 4, 1, 1, 4, 4, 4, 4, 4, 4, 1, 1, 4, 3,&
         &4, 4, 1, 1, 4, 1, 1, 1, 4, 1, 1, 4, 4, 1, 1, 1/
    data IMAPLS/&
         &1, 3, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 3, 3, 4, 1, 4, 1, 1, 4,&
         &1, 1, 1, 4, 1, 1, 1, 1, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,&
         &4, 4, 4, 1, 4, 4, 1, 1, 4, 4, 4, 4, 1, 4, 4, 1, 4, 4, 1, 4,&
         &4, 4, 4, 1, 1, 4, 1, 4/
    data IMAPNE/&
         &1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 1, 1, 3, 3, 1, 1, 3, 1, 3,&
         &3, 3, 3, 3, 2, 1, 1, 1, 1, 4, 1, 4, 4, 4, 4, 4, 4, 4, 4, 1,&
         &1, 4, 4, 1, 1, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,&
         &4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 1, 1, 4, 4, 4, 4, 4, 4, 4,&
         &1, 4, 4, 4, 1, 1, 4, 4, 4, 1, 1, 1, 1, 1, 1, 1, 1, 4, 4, 4,&
         &4, 4, 1, 1, 4, 1, 4, 4/
    ! WS
    data PMSCWS/&
         &6.5112,    7.2985,    5.1677,    2.5968,    5.1677,&
         &9.6943,    5.1677,    5.5877,    5.9617,    5.9617,&
         &6.5112,    6.5112,    5.1677,    6.5112,    6.5112,&
         &6.5112,    6.5112,    5.5877,    6.5112,    6.5112,&
         &5.1677,    7.2985,    2.5968,    6.5112,    6.5112,&
         &6.5112,    6.5112,    5.9617,    5.9617,    5.9617,&
         &5.9617,    5.9617,    5.9617,    5.1677,    5.1677,&
         &5.1677,    5.1677,    5.1677,    5.1677,    5.9617,&
         &5.9617,    6.5112,    5.9617/
    data PMDWS/&
         &-.0052485, -.0129121, -.0077681,   .512610, -.0077681,&
         &-.0127328, -.0077681,  -.005348, -.0340128, -.0340128,&
         &-.0052485, -.0052485, -.0077681, -.0052485, -.0052485,&
         &-.0052485, -.0052485,  -.005348, -.0052485, -.0052485,&
         &-.0077681, -.0129121,  .512610, -.0052485, -.0052485,&
         &-.0052485, -.0052485, -.0340128, -.0340128, -.0340128,&
         &-.0340128, -.0340128, -.0340128, -.0077681, -.0077681,&
         &-.0077681, -.0077681, -.0077681, -.0077681, -.0340128,&
         &-.0340128, -.0052485, -.0340128/
    ! IE, KT AND CI (PROGNOSIS CONSTANT B5)
    data PMSCIE/&
         &0.0,-.17603,2*0.317888,0.607725,1.57976,-.12057,.94019,&
         &2*.21180,0.0,-.17603,0.0,.21180,9*0.0/
    data PMSCKT/&
         &0.0,-.17603,2*0.317888,0.607725,1.57976,-.12057,.94019,&
         &2*.21180,0.0/
    data PMSCCI/&
         &0.0,  -.17603, 0.317888, 0.317888, 0.607725,&
         &1.57976,  -.12057,   .94019,   .21180,   .21180,&
         &0.0,      0.0,      0.0,      0.0,      0.0,&
         &0.0,      0.0,      0.0,      0.0/

    select case (VARIANTCD)
    case ('BM')
       B0 = PMSCBM(ISPC)
       B1 = PMDBM(ISPC)
    case ('CA','OC')
       KSPC = IBGMCA(ISPC)
       B0 = PMSCCA(KSPC)
       B1 = PMDCA(KSPC)
    case ('CR')
       KSPC = IBGMCR(ISPC)
       B0 = PMSCCR(KSPC)
       B1 = PMDCR(KSPC)
    case ('CS','LS','NE')
       if (VARIANTCD.eq.'CS') KSPC = IMAPCS(ISPC)
       if (VARIANTCD.eq.'LS') KSPC = IMAPLS(ISPC)
       if (VARIANTCD.eq.'NE') KSPC = IMAPNE(ISPC)
       B0 = PMSCLS(KSPC)
       B1 = PMDLS(KSPC)
    case ('EC')
       B0 = PMSCEC(ISPC)
       B1 = PMDEC(ISPC)
    case ('EM')
       B0 = PMSCEM(ISPC)
       B1 = PMDEM(ISPC)
       B2 = PMDSQEM(ISPC)
    case ('NC')
       B0 = PMSCNC(ISPC)
       B1 = PMDNC(ISPC)
    case ('SN')
       B0 = PMSCSN(ISPC)
       B1 = PMDSN(ISPC)
    case ('SO')
       B0 = PMSCSO(ISPC)
       B1 = PMDSO(ISPC)
    case ('TT')
       B0 = PMSCTT(ISPC)
       B1 = PMDTT(ISPC)
    case ('UT')
       B0 = PMSCUT(ISPC)
       B1 = PMDUT(ISPC)
    case ('WS')
       B0 = PMSCWS(ISPC)
       B1 = PMDWS(ISPC)
    case ('IE')
       B5 = PMSCIE(ISPC)
    case ('KT')
       B5 = PMSCKT(ISPC)
    case ('CI')
       B5 = PMSCCI(ISPC)
    end select
  end subroutine MORCOF

  subroutine PRGCON
    implicit none

    !  GMULT AND REIN FOR THE PROGNOSIS EQUATION (CI, EM, IE, KT, TT).
    !  ITYPE IS A HABITAT INDEX; CI FIRST MAPS ITS FORESTS (MAPFOR), AND TT
    !  USES FIXED CLASSES 12 AND 41.

    real    :: POT(54)
    integer :: IPDGIE(30,11),IPDG2IE(30,11),IPDGCI(30,11),IPDG2CI(30,11)
    integer :: IPDGEM(30,6),IPDG2EM(30,6),MAPFOR(6),J,IPT,IPT2,MIFOR

    data POT/&
         &.25,.30,.35,.40,.45,.50,.55,.60,.65,.70,.75,.80,.85,.90,.95,1.00,&
         &1.05,1.10,1.15,1.20,1.25,1.30,1.35,1.40,1.45,1.50,1.55,1.60,1.65,&
         &1.70,1.75,1.80,1.85,1.90,1.95,2.00,2.05,2.10,2.15,2.20,2.25,2.30,&
         &2.35,2.40,2.45,2.50,2.55,2.60,2.65,2.70,2.75,2.80,2.85,2.90/
    ! IE AND KT
    data ((IPDGIE(I,J),I = 1,30),J = 1,2)/&
         &7, 5*6, 5,6,5, 4*6, 8, 5*7, 5,3,4,3,4,4,6,5,1,1,6,&
         &4*15, 14,14,12,14,13,14,14,15,15,17,15,15,16,15,15,11,10,11,9,&
         &12,12,15,12,7,10,15/
    data ((IPDGIE(I,J),I = 1,30),J = 3,5)/&
         &4*14, 13,13,11,14,12,14, 3*14, 17,14,14, 3*15, 11,9,10,8,11,11,&
         &15,11,5,10,15,&
         &4*12, 11,11, 3*10, 11,11,12,12, 3*14, 3*13, 9,6,8,7,9,10,12,10,&
         &4,8,12,&
         &4*10, 9,9,8,9,8,9, 3*10, 12, 5*11, 8,6,7,5,7,7,10,8,6,7,10/
    data ((IPDGIE(I,J),I = 1,30),J = 6,7)/&
         &6*9, 7, 3*8, 3*9, 11, 4*10, 11,7,5,6,4,7,7,9,8,3,6,9,&
         &13, 5*12, 10,12, 3*11, 12,12,15,13,13,14,13,13,9,8,9,7,10,10,&
         &12,10,4,8,12/
    data ((IPDGIE(I,J),I = 1,30),J = 8,9)/&
         &4*9, 8,9,7,8,7, 3*8, 9,11, 5*10, 7,5,6,5,7,7,9,8,3,6,9,&
         &4*7, 6,7,5,6,7,6,6,7,7,9,9,8,9,8,8,5,3,5,4,5,5,7,5,2,4,7/
    data ((IPDGIE(I,J),I = 1,30),J = 10,11)/&
         &4*15, 14,14,9,13,13,14,14,13,14,18, 5*14, 10,9,9,8,11,11,15,9,&
         &6,8,15,&
         &4*15, 14,14,12,14,13,14,14,15,15,17,15,15,16,15,15,11,10,11,9,&
         &12,12,15,12,7,10,15/
    data ((IPDG2IE(I,J),I = 1,30),J = 1,2)/&
         &30, 3*29, 28,28,27,31,27,27,28,31,32,32,31,31,32,31,31,25,23,24,&
         &23,24,24,27,26,18,16,27,&
         &50, 3*49, 48,48,47,52,46,47,48,53,54,54,52,52,54,50,51,41,39,44,&
         &43,44,44,50,53,36,37,50/
    data ((IPDG2IE(I,J),I = 1,30),J = 3,5)/&
         &4*45, 44,44,43,48,43,43,44,49,50,50, 3*49, 47,47,39,36,40,39,40,&
         &39,45,41,33,34,45,&
         &4*41, 40,40,39,42,39,39,40,44, 5*45, 43,44,36,33,36,36,37,38,41,&
         &38,30,31,41,&
         &38, 3*37, 36,37,37,40,35,36,37,41,41,42,40,40,41,40,40,33,30,33,&
         &31,32,31,36,34,32,27,36/
    data ((IPDG2IE(I,J),I = 1,30),J = 6,7)/&
         &38, 3*37, 3*36, 38,36,35,37, 3*41, 40,40,41,40,40,33, 3*32, 34,&
         &33,38,35,26,28,38,&
         &4*41, 40,40,39,45,37,38,39,43,44,46,43,43,45,43,43,35,33,35,35,&
         &36,36,41,37,30,32,41/
    data ((IPDG2IE(I,J),I = 1,30),J = 8,9)/&
         &38, 6*37, 41,33,37,37,41,41,42,39,39,41,40,40,33,32,33,33,34,33,&
         &38,35,28,32,38,&
         &31,29,30,31,29,30,29,33,31,30,29,34,34,35,34,34,35,33,33,27,23,&
         &26,26,27,26,31,26,19,23,31/
    data ((IPDG2IE(I,J),I = 1,30),J = 10,11)/&
         &4*45, 44,44,43,48,41,42,43,47,48,50,47,47,46,44,44,36,34,37,37,&
         &38,37,43,34,30,33,43,&
         &50, 3*49, 48,48,47,52,46,47,48,53,54,54,52,52,54,50,51,41,39,44,&
         &43,44,44,50,53,36,37,50/
    ! CI
    data ((IPDGCI(I,J),I = 1,30),J = 1,2)/&
         &7, 5*6, 5,6,5, 4*6, 8, 5*7, 5,3,4,3,4,4,6,5,1,1,6,&
         &4*15, 14,14,12,14,13,14,14,15,15,17,15,15,16,15,15,11,10,11,9,&
         &12,12,15,12,7,10,15/
    data ((IPDGCI(I,J),I = 1,30),J = 3,5)/&
         &4*14, 13,13,11,14,12,14, 3*14, 17,14,14, 3*15, 11,9,10,8,11,11,&
         &15,11,5,10,15,&
         &4*12, 11,11, 3*10, 11,11,12,12, 3*14, 3*13, 9,6,8,7,9,10,12,10,&
         &4,8,12,&
         &4*10, 9,9,8,9,8,9, 3*10, 12, 5*11, 8,6,7,5,7,7,10,8,6,7,10/
    data ((IPDGCI(I,J),I = 1,30),J = 6,7)/&
         &6*9, 7, 3*8, 3*9, 11, 4*10, 11,7,5,6,4,7,7,9,8,3,6,9,&
         &13, 5*12, 10,12, 3*11, 12,12,15,13,13,14,13,13,9,8,9,7,10,10,&
         &12,10,4,8,12/
    data ((IPDGCI(I,J),I = 1,30),J = 8,9)/&
         &4*9, 8,9,7,8,7, 3*8, 9,11, 5*10, 7,5,6,5,7,7,9,8,3,6,9,&
         &4*7, 6,7,5,6,7,6,6,7,7,9,9,8,9,8,8,5,3,5,4,5,5,7,5,2,4,7/
    data ((IPDGCI(I,J),I = 1,30),J = 10,11)/&
         &4*15, 14,14,9,13,13,14,14,13,14,18, 5*14, 10,9,9,8,11,11,15,9,&
         &6,8,15,&
         &4*15, 14,14,12,14,13,14,14,15,15,17,15,15,16,15,15,11,10,11,9,&
         &12,12,15,12,7,10,15/
    data ((IPDG2CI(I,J),I = 1,30),J = 1,2)/&
         &30, 3*29, 28,28,27,31,27,27,28,31,32,32,31,31,32,31,31,25,23,24,&
         &23,24,24,27,26,18,16,27,&
         &50, 3*49, 48,48,47,52,46,47,48,53,54,54,52,52,54,50,51,41,39,44,&
         &43,44,44,50,53,36,37,50/
    data ((IPDG2CI(I,J),I = 1,30),J = 3,5)/&
         &4*45, 44,44,43,48,43,43,44,49,50,50, 3*49, 47,47,39,36,40,39,40,&
         &39,45,41,33,34,45,&
         &4*41, 40,40,39,42,39,39,40,44, 5*45, 43,44,36,33,36,36,37,38,41,&
         &38,30,31,41,&
         &38, 3*37, 36,37,37,40,35,36,37,41,41,42,40,40,41,40,40,33,30,33,&
         &31,32,31,36,34,32,27,36/
    data ((IPDG2CI(I,J),I = 1,30),J = 6,7)/&
         &38, 3*37, 3*36, 38,36,35,37, 3*41, 40,40,41,40,40,33, 3*32, 34,&
         &33,38,35,26,28,38,&
         &4*41, 40,40,39,45,37,38,39,43,44,46,43,43,45,43,43,35,33,35,35,&
         &36,36,41,37,30,32,41/
    data ((IPDG2CI(I,J),I = 1,30),J = 8,9)/&
         &38, 6*37, 41,33,37,37,41,41,42,39,39,41,40,40,33,32,33,33,34,33,&
         &38,35,28,32,38,&
         &31,29,30,31,29,30,29,33,31,30,29,34,34,35,34,34,35,33,33,27,23,&
         &26,26,27,26,31,26,19,23,31/
    data ((IPDG2CI(I,J),I = 1,30),J = 10,11)/&
         &4*45, 44,44,43,48,41,42,43,47,48,50,47,47,46,44,44,36,34,37,37,&
         &38,37,43,34,30,33,43,&
         &50, 3*49, 48,48,47,52,46,47,48,53,54,54,52,52,54,50,51,41,39,44,&
         &43,44,44,50,53,36,37,50/
    data MAPFOR/ 10,  1,  1, 10,  1,  1/
    ! EM
    data ((IPDGEM(I,J),I = 1,30),J = 1,6)/&
         ! BITTERROOT MAPPING USED FOR THE BEAVERHEAD
         &7, 5*6, 5,6,5, 4*6, 8, 5*7, 5,3,4,3,4,4,6,5,1,1,6,&
         ! BITTERROOT MAPPING USED FOR THE CUSTER
         &7, 5*6, 5,6,5, 4*6, 8, 5*7, 5,3,4,3,4,4,6,5,1,1,6,&
         ! BITTERROOT MAPPING USED FOR THE DEERLODGE
         &7, 5*6, 5,6,5, 4*6, 8, 5*7, 5,3,4,3,4,4,6,5,1,1,6,&
         ! BITTERROOT MAPPING USED FOR THE GALLATIN
         &7, 5*6, 5,6,5, 4*6, 8, 5*7, 5,3,4,3,4,4,6,5,1,1,6,&
         ! LOLO       MAPPING USED FOR THE HELENA
         &4*7, 6,7,5,6,7,6,6,7,7,9,9,8,9,8,8,5,3,5,4,5,5,7,5,2,4,7,&
         ! LOLO       MAPPING USED FOR THE LEWIS AND CLARK
         &4*7, 6,7,5,6,7,6,6,7,7,9,9,8,9,8,8,5,3,5,4,5,5,7,5,2,4,7/
    data ((IPDG2EM(I,J),I = 1,30),J = 1,6)/&
         ! BITTERROOT MAPPING USED FOR THE BEAVERHEAD
         &30, 3*29, 28,28,27,31,27,27,28,31,32,32,31,31,32,31,31,25,23,24,&
         &23,24,24,27,26,18,16,27,&
         ! BITTERROOT MAPPING USED FOR THE CUSTER
         &30, 3*29, 28,28,27,31,27,27,28,31,32,32,31,31,32,31,31,25,23,24,&
         &23,24,24,27,26,18,16,27,&
         ! BITTERROOT MAPPING USED FOR THE DEERLODGE
         &30, 3*29, 28,28,27,31,27,27,28,31,32,32,31,31,32,31,31,25,23,24,&
         &23,24,24,27,26,18,16,27,&
         ! BITTERROOT MAPPING USED FOR THE GALLATIN
         &30, 3*29, 28,28,27,31,27,27,28,31,32,32,31,31,32,31,31,25,23,24,&
         &23,24,24,27,26,18,16,27,&
         ! LOLO       MAPPING USED FOR THE HELENA
         &31,29,30,31,29,30,29,33,31,30,29,34,34,35,34,34,35,33,33,27,23,&
         &26,26,27,26,31,26,19,23,31,&
         ! LOLO       MAPPING USED FOR THE LEWIS AND CLARK
         &31,29,30,31,29,30,29,33,31,30,29,34,34,35,34,34,35,33,33,27,23,&
         &26,26,27,26,31,26,19,23,31/

    select case (VARIANTCD)
    case ('IE','KT')
       IPT = IPDGIE(ITYPE,IFOR)
       IPT2 = IPDG2IE(ITYPE,IFOR)
    case ('CI')
       MIFOR = MAPFOR(IFOR)
       IPT = IPDGCI(ITYPE,MIFOR)
       IPT2 = IPDG2CI(ITYPE,MIFOR)
    case ('EM')
       IPT = IPDGEM(ITYPE,IFOR)
       IPT2 = IPDG2EM(ITYPE,IFOR)
    case default
       IPT = 12
       IPT2 = 41
    end select
    POTEN = POT(IPT)
    GMULT(1) = .90/POTEN
    REIN(1) = (1-(POTEN/20.+1.)**(-1.605))/.06821
    POTEN = POT(IPT2)
    GMULT(2) = 2.50/POTEN
    REIN(2) = (1-(POTEN+1.)**(-1.605))/.86610
  end subroutine PRGCON

  subroutine FIXMRT
    implicit none

    !  FIXMORT PROCESSING, SHARED BY ALL 23 CONSUMERS.  BC AND ON PRINT
    !  THESE DIAGNOSTICS WITH A LEADING BLANK (BL(1:LBL)) AND CR PRINTS
    !  NTODO; BC'S DEFAULT UPPER DBH IS 999 INCHES IN CENTIMETRES.  THE
    !  FIRST DIAGNOSTIC PRINTS ITODO BEFORE IT IS SET (FOUND_BUGS.md).

    logical :: LINCL
    character(len=1) :: BL
    integer :: LBL,NTODO,ITODO,IDATE,IACTK,NP,ISPCC,KPOINT,KBIG
    integer :: IGRP,IULIM,IG,IX,J
    real    :: XMORE,TEMP,CREDIT

    BL = ' '
    LBL = 0
    if (VARIANTCD.eq.'BC'.or.VARIANTCD.eq.'ON') LBL = 1

    !  COMPUTE THE FIXMORT OPTION.  LOOP OVER ALL SCHEDULED FIXMORTS
    !  LINCL IS USED TO INDICATE WHETHER A TREE GETS AFFECTED OR NOT

    call OPFIND (1,MYACTS(2),NTODO)
    if (NTODO.gt.0) then
       if (DEBUG) then
          if (VARIANTCD.eq.'CR') then
             write(JOSTND,*)'FIXMORT PROCESSING, NTODO= ',NTODO
          else
             write(JOSTND,*)BL(1:LBL)//'FIXMORT PROCESSING, ITODO= ',ITODO
          end if
       end if
       do ITODO = 1,NTODO
          call OPGET (ITODO,6,IDATE,IACTK,NP,PRM)
          if (IACTK.lt.0) goto 300
          call OPDONE(ITODO,IY(ICYC))
          ISPCC = IFIX(PRM(1))
          if (NP .le.4) then
             if (PRM(2).gt.1.0)PRM(2) = 1.0
          end if
          if (PRM(3).lt.0.0)PRM(3) = 0.0
          if (PRM(4).le.0.0) then
             if (VARIANTCD.eq.'BC') then
                PRM(4) = 999.*INTOCM
             else
                PRM(4) = 999.
             end if
          end if
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
                if (DEBUG)write(JOSTND,*)BL(1:LBL)//'KPOINT,KBIG,ITRN,XMORE= ',&
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
                if (DEBUG)write(JOSTND,*)BL(1:LBL)//'DBH= ',(DBH(IG),IG = 1,ITRN)
                if (DEBUG)write(JOSTND,*)BL(1:LBL)//'IWORK1= ',(IWORK1(IG),IG = 1,ITRN)
                if (DEBUG)write(JOSTND,*)BL(1:LBL)//'WK2= ',(WK2(IG),IG = 1,ITRN)

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
             if (DEBUG)write(JOSTND,*)BL(1:LBL)//'ITODO,WK2= ',&
                  &ITODO,(WK2(IG),IG = 1,ITRN)
          end if
          300 continue
       end do
    end if

  end subroutine FIXMRT

end subroutine MORTS
