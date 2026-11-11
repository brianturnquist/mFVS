!========================================!
!   Compute large tree diameter growth   !
!========================================!
! AK $Id$
subroutine DGF(DIAM)
  implicit none

  !  THIS SUBROUTINE COMPUTES THE VALUE OF DGPRED (CHANGE IN
  !  DIAMETER) FOR EACH TREE RECORD, AND LOADS IT INTO THE ARRAY
  !  WK2. THE SET OF TREE DIAMETERS TO BE USED IS PASSED AS THE
  !  ARGUMENT DIAM.  THE PROGRAM THUS HAS THE FLEXIBILITY TO
  !  PROCESS DIFFERENT CALIBRATION OPTIONS.  THIS ROUTINE IS CALLED
  !  BY **DGDRIV** DURING CALIBRATION AND WHILE CYCLING FOR GROWTH
  !  PREDICTION.  ENTRY **DGCONS** IS CALLED BY **RCON** TO LOAD SITE
  !  DEPENDENT COEFFICIENTS THAT NEED ONLY BE RESOLVED ONCE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.F77'
  include 'CALCOM.F77'
  include 'COEFFS.F77'
  include 'CONTRL.F77'
  include 'OUTCOM.F77'
  include 'PLOT.F77'
  include 'VARCOM.F77'

  !  VARIABLE DEFINITIONS:

  !   DIAM   -- ARRAY LOADED WITH TREE DIAMETERS
  !             (PASSED AS AN ARGUEMENT).

  !  ** FOR THE FOLOWING, ARRAYS HOLD SPECIES SPECIFIC VALUES FOR THE  **
  !  ** EQUATIONS AND THE SCALAR COEFFICIENTS ARE NOT SPECIES SPECIFIC.**

  !   DGBAL  -- ARRAY, COEFFICIENTS FOR THE BASAL AREA IN
  !             LARGER TREES TERM IN THE DIAMETER GROWTH EQUATION
  !   DGDBAL -- ARRAY, COEFFICIENTS FOR THE INTERACTION
  !             BETWEEN POINT BASAL AREA IN LARGER TREES AND LN(DBH)
  !   DGDSQ  -- ARRAY, THE COEFFICIENTS FOR THE DIAMETER
  !             SQUARED TERM IN THE DIAMETER GROWTH EQUATION
  !   DGEL   -- ARRAY, COEFFICIENTS FOR THE ELEVATION TERM IN THE
  !             DIAMETER GROWTH EQUATION.
  !   DGLD   -- ARRAY, COEFFICIENTS FOR THE LOG(DIAMETER)
  !             TERM IN THE DIAMETER GROWTH EQUATION
  !   DGLNCR -- ARRAY, COEFFICIENTS FOR THE LOG(CROWN
  !             RATIO) TERM IN THE DIAMETER GROWTH EQUATION
  !   DGLNSI -- ARRAY, COEFFICIENTS FOR THE LOG(SITE
  !             INDEX) TERM IN THE DIAMETER GROWTH EQUATION
  !   DGRD   -- ARRAY, COEFFICIENTS FOR POINT RELATIVE DENSITY
  !             (PLOT ZEIDE SDI / PLOT MAX SDI � DECIMAL)
  !   DGSASP -- ARRAY, COEFFICIENT FOR SLOPE*COS(ASPECT) TERM
  !   DGSLOP -- ARRAY, COEFFICIENT FOR SLOPE PERCENT TERM
  !   OBSERV -- CONTAINS THE NUMBER OF OBSERVATIONS BY SPECIES FOR THE
  !             GROWTH MODEL (THIS DATA IS ACTUALLY USED BY **DGDRIV**
  !             FOR CALIBRATION).
  !   PFCON  -- ARRAY, PERMAFROST INTERCEPT COEFFICIENT (b1)
  !   PFDBAL -- ARRAY, PERMAFROST POINT BASAL AREA LARGER COEFFIENT (b4)
  !   PFDSQ  -- SCALAR, PERMAFROST DBH SQUARED COEFFIENT (b2)
  !   PFEL   -- SCALAR, PERMAFROST ELEVATION COEFFIENT (b7)
  !   PFLD   -- ARRAY, PERMAFROST NAT LOG OF DBH COEFFIENT (b3)
  !   PFLNCR -- SCALAR, PERMAFROST NAT LOG OF CROWN RATIO COEFFIENT (b6)
  !   PFPRES -- SCALAR, PERMAFROST PRESENCE/ABSENCE FACTOR (b01)
  !   PFRD   -- ARRAY, PERMAFROST ZEIDI POINT RELATIVE DENSITY COEFFIENT (b5)
  !   PFSASP -- SCALAR, PERMAFROST SLOPE*cos(ASPECT) COEFFIENT (b9)
  !   PFSLOP -- SCALAR, PERMAFROST SLOPE COEFFIENT (b8)

  ! SPECIES LIST FOR ALASKA VARIANT.

  !     PERMAFROST
  !      AFFECTED
  !         |
  ! Number  V  Code  Common Name         FIA  PLANTS Scientific Name
  !   1        SF   Pacific silver fir  011  ABAM   Abies amabilis
  !   2        AF   subalpine fir       019  ABLA   Abies lasiocarpa
  !   3        YC   Alaska cedar        042  CANO9  Callitropsis nootkatensis
  !   4     P  TA   tamarack            071  LALA   Larix laricina
  !   5     P  WS   white spruce        094  PIGL   Picea glauca
  !   6     P  LS   Lutz�s spruce            PILU   Picea lutzii
  !   7     P  BE   black spruce        095  PIMA   Picea mariana
  !   8        SS   Sitka spruce        098  PISI   Picea sitchensis
  !   9        LP   lodgepole pine      108  PICO   Pinus contorta
  !  10        RC   western redcedar    242  THPL   Thuja plicata
  !  11        WH   western hemlock     263  TSHE   Tsuga heterophylla
  !  12        MH   mountain hemlock    264  TSME   Tsuga mertensiana
  !  13     P  OS   other softwoods     298  2TN
  !  14        AD   alder species       350  ALNUS  Alnus species
  !  15        RA   red alder           351  ALRU2  Alnus rubra
  !  16     P  PB   paper birch         375  BEPA   Betula papyrifera
  !  17     P  AB   Alaska birch        376  BENE4  Betula neoalaskana
  !  18     P  BA   balsam poplar       741  POBA2  Populus balsamifera
  !  19     P  AS   quaking aspen       746  POTR5  Populus tremuloides
  !  20     P  CW   black cottonwood    747  POBAT  Populus trichocarpa
  !  21     P  WI   willow species      920  SALIX  Salix species
  !  22     P  SU   Scouler�s willow    928  SASC   Salix scouleriana
  !  23     P  OH   other hardwoods     998  2TB

  ! Variables
  logical :: DEBUG
  integer :: I ,I1,I2,I3,ISPC,IWHO

  real    :: PBAL,BRAT,BRATIO,CR,D,XMAX

  real    :: SSITE,TEMEL

  real    :: DGCONB1(MAXSP),DGDBAL(MAXSP),DGDISQ(MAXSP),DGEL(MAXSP),DGLD(MAXSP),DGLNCR(MAXSP), &
       & DGLNSI(MAXSP),DGRD(MAXSP),DGSASP(MAXSP),DGSLOP(MAXSP)
  real    :: A,B,D2,DGCOMP1,DGCOMP2,DGPRED,DHI,DLO,PRD,SDICS,SDICZ,TEMSLP,TEMSASP,ZRD(MAXPLT),DDS

  real    :: PFPRES,PFCON(MAXSP),PFDBAL(MAXSP),PFDSQ,PFEL,PFLD(MAXSP),PFLNCR,PFRD(MAXSP),PFSASP,PFSLOP
  real    :: PFCOMP1,PFCOMP2,PFMOD,BASEDG

  real    :: DUP,TEMPD1,TEMPD2

  real    :: DIAM(MAXTRE),OBSERV(MAXSP)
  !     , DGRAT(MAXSP)

  !  DATA STATEMENTS:

  ! *** BEGIN DIAMETER GROWTH EQUATION COEFFICIENTS ***
  ! DESCRIBED IN DOCUMENTATION AS
  ! INTERCEPT -- b1
  data DGCONB1 / &
       & -8.580747, -8.580747, -9.485113, -6.585228, -6.315766, &
       & -6.315766, -6.585228, -8.580747, -9.297786, -9.357119, &
       & -9.458059, -9.155381, -6.315766, -5.646338, -5.195889, &
       & -6.015696, -6.015696, -5.646338, -6.704925, -5.646338, &
       & -5.646338, -5.646338, -5.646338 /

  ! DESCRIBED IN DOCUMENTATION AS COEFFIENT FOR
  ! DBH^2 -- b2
  data DGDISQ / &
       & -0.000439, -0.000439, -0.000439, -0.002279, -0.002279, &
       & -0.002279, -0.002279, -0.000439, -0.000439, -0.000439, &
       & -0.000439, -0.000439, -0.002279, -0.002279, -0.002279, &
       & -0.002279, -0.002279, -0.002279, -0.002279, -0.002279, &
       & -0.002279, -0.002279, -0.002279 /

  ! DESCRIBED IN DOCUMENTATION AS COEFFIENT FOR
  ! ln(DBH) -- b3
  data DGLD / &
       & 0.033655,  0.033655,  0.081005,  0.152853,  0.480982, &
       & 0.480982,  0.152853,  0.033655, -0.000862,  0.243224, &
       & 0.175997,  0.044554,  0.480982,  0.212906,  0.425682, &
       & 0.353350,  0.353350,  0.212906,  0.669461,  0.212906, &
       & 0.212906,  0.212906,  0.212906 /

  ! DESCRIBED IN DOCUMENTATION AS COEFFIENT FOR
  ! PBAL -- b4
  data DGDBAL / &
       & -0.002283, -0.002283, -0.000598, -0.001513, -0.000057, &
       & -0.000057, -0.001513, -0.002283, -0.000598, -0.000598, &
       & -0.000595, -0.000580, -0.000057, -0.000722, -0.001166, &
       & -0.007165, -0.007165, -0.000722, -0.000557, -0.000722, &
       & -0.000722, -0.000722, -0.000722 /

  ! DESCRIBED IN DOCUMENTATION AS COEFFIENT FOR
  ! PRD -- b5
  data DGRD / &
       & 0.0,       0.0,       0.0,      -0.353197, -0.455502, &
       & -0.455502, -0.353197,  0.0,       0.0,       0.0, &
       & 0.0,       0.0,      -0.455502, -0.763633, -0.623822, &
       & -0.353197, -0.353197, -0.763633, -0.480135, -0.763633, &
       & -0.763633, -0.763633, -0.763633 /

  ! DESCRIBED IN DOCUMENTATION AS COEFFIENT FOR
  ! ln(CR) -- b6
  data DGLNCR / &
       & 0.819909, 0.819909, 0.819909, 0.727631, 0.727631, &
       & 0.727631, 0.727631, 0.819909, 0.819909, 0.819909, &
       & 0.819909, 0.819909, 0.727631, 0.727631, 0.727631, &
       & 0.727631, 0.727631, 0.727631, 0.727631, 0.727631, &
       & 0.727631, 0.727631, 0.727631 /

  ! DESCRIBED IN DOCUMENTATION AS COEFFIENT FOR
  ! ELEV-- b7
  data DGEL / &
       & 0.0,       0.0,       0.0,      -0.000175, -0.000175, &
       & -0.000175, -0.000175,  0.0,       0.0,       0.0, &
       & 0.0,       0.0,      -0.000175, -0.000175, -0.000175, &
       & -0.000175, -0.000175, -0.000175, -0.000175, -0.000175, &
       & -0.000175, -0.000175, -0.000175 /

  ! DESCRIBED IN DOCUMENTATION AS COEFFIENT FOR
  ! SLOPE -- b8
  data DGSLOP / &
       & 0.001604, 0.001604, 0.001604, 0.001225, 0.001225, &
       & 0.001225, 0.001225, 0.001604, 0.001604, 0.001604, &
       & 0.001604, 0.001604, 0.001225, 0.001225, 0.001225, &
       & 0.001225, 0.001225, 0.001225, 0.001225, 0.001225, &
       & 0.001225, 0.001225, 0.001225 /

  ! DESCRIBED IN DOCUMENTATION AS COEFFIENT FOR
  ! SLOPE*cos(ASPECT) -- b9
  data DGSASP / &
       & 0.000020,  0.000020,  0.000020, -0.002424, -0.002424, &
       & -0.002424, -0.002424,  0.000020,  0.000020,  0.000020, &
       & 0.000020,  0.000020, -0.002424, -0.002424, -0.002424, &
       & -0.002424, -0.002424, -0.002424, -0.002424, -0.002424, &
       & -0.002424, -0.002424, -0.002424 /

  ! DESCRIBED IN DOCUMENTATION AS COEFFIENT FOR
  ! ln(SI) -- b10
  data DGLNSI / &
       & 0.712498, 0.712498, 0.712498, 0.0,      0.0, &
       & 0.0,      0.0,      0.712498, 0.712498, 0.712498, &
       & 0.712498, 0.712498, 0.0,      0.0,      0.0, &
       & 0.0,      0.0,      0.0,      0.0,      0.0, &
       & 0.0,      0.0,      0.0 /

  !C *** END DIAMETER GROWTH EQUATION COEFFICIENTS ***

  ! *** BEGIN PERMAFROST DIAMETER GROWTH MODIFIER COEFFICIENTS ***
  ! DESCRIBED IN DOCUMENTATION AS PERMAFROST PRESENCE/ABSENCE FACTOR
  ! IN THE CALCULATION OF THE MODIFIER.
  ! ADDITIVE FACTOR -- b01
  data PFPRES / -0.349215 /

  ! DESCRIBED IN DOCUMENTATION AS PERMAFROST COEFFICIENT FOR
  ! INTERCEPT -- b1
  data PFCON / &
       & 0.0,       0.0,       0.0, -6.529717, -6.274336, &
       & -6.274336, -6.529717,       0.0,       0.0,       0.0, &
       & 0.0,       0.0, -6.274336,       0.0,       0.0, &
       & -6.093543, -6.093543, -5.527782, -6.861172, -5.527782, &
       & -5.527782, -5.527782, -5.527782/

  ! DESCRIBED IN DOCUMENTATION AS PERMAFROST COEFFIENT FOR
  ! DBH^2 -- b2
  data PFDSQ / -0.002711 /

  ! DESCRIBED IN DOCUMENTATION AS PERMAFROST COEFFIENT FOR
  ! ln(DBH) -- b3
  data PFLD / &
       & 0.0,      0.0,      0.0, 0.161256, 0.476859, &
       & 0.476859, 0.161256,      0.0,      0.0,      0.0, &
       & 0.0,      0.0, 0.476859,      0.0,      0.0, &
       & 0.373362, 0.373362, 0.082885, 0.712862, 0.082885, &
       & 0.082885, 0.082885, 0.082885/

  ! DESCRIBED IN DOCUMENTATION AS PERMAFROST COEFFIENT FOR
  ! PBAL -- b4
  data PFDBAL / &
       & 0.0,       0.0,       0.0, -0.001871, -0.000531, &
       & -0.000531, -0.001871,       0.0,       0.0,       0.0, &
       & 0.0,       0.0, -0.000531,       0.0,       0.0, &
       & -0.007347, -0.007347, -0.001321, -0.000533, -0.001321, &
       & -0.001321, -0.001321, -0.001321 /

  ! DESCRIBED IN DOCUMENTATION AS PERMAFROST COEFFIENT FOR
  ! PRD -- b5
  data PFRD / &
       & 0.0,       0.0,       0.0,  -0.02879, -0.482094, &
       & -0.482094, -0.02879,       0.0,        0.0,       0.0, &
       & 0.0,       0.0, -0.482094,       0.0,       0.0, &
       & -0.330313, -0.330313, -0.819852, -0.462575, -0.819852, &
       & -0.819852, -0.819852, -0.819852 /

  ! DESCRIBED IN DOCUMENTATION AS PERMAFROST COEFFIENT FOR
  ! ln(CR) -- b6
  data PFLNCR / 0.736013 /

  ! DESCRIBED IN DOCUMENTATION AS PERMAFROST COEFFIENT FOR
  ! ELEV-- b7
  data PFEL / -0.000137 /

  ! DESCRIBED IN DOCUMENTATION AS PERMAFROST COEFFIENT FOR
  ! SLOPE -- b8
  data PFSLOP / 0.001702 /

  ! DESCRIBED IN DOCUMENTATION AS PERMAFROST COEFFIENT FOR
  ! SLOPE*cos(ASPECT) -- b9
  data PFSASP / -0.001819 /

  ! *** END PERMAFROST DIAMETER GROWTH MODIFIER COEFFICIENTS ***

  !  NUMBER OF DATA OBSERVATIONS BY SPECIES FOR GROWTH MODEL

  data OBSERV / &
       & 4346.0,  4346.0,  2781.0, 29501.0, 70550.0, &
       & 70550.0, 29501.0,  4346.0,   879.0,  1546.0, &
       & 10395.0,  5872.0, 70550.0,  4727.0,   202.0, &
       & 32332.0, 32332.0,  4727.0, 21380.0,  4727.0, &
       & 4727.0,  4727.0,  4727.0/

  !  DATAFRAME OF DG MODIFIERS USED TO CORRECT FOR ANNUALIZED DG
  !      DATA DGRAT/
  !     &  0.99, 0.99, 1.00, 0.99, 1.01, 1.01, 0.99, 0.99, 1.00, 1.00,
  !     &  1.00, 1.00, 1.01, 0.98, 0.98, 0.97, 0.97, 0.98, 1.00, 0.98,
  !     &  0.98, 0.98, 0.98/

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'DGF',3,ICYC)
  if (DEBUG) write(JOSTND,2)ICYC
  2 format(' ENTERING DGF  CYCLE =',I5)

  !  DEBUG OUTPUT: MODEL COEFFICIENTS.

  if (DEBUG)write(JOSTND,*) 'IN DGF,HTCON=',HTCON
  if (DEBUG)write(JOSTND,*)'RMAI=',RMAI,'ELEV=',ELEV,'RELDEN=',RELDEN
  if (DEBUG) &
       & write(JOSTND,9000) DGCON,DGDSQ,DGLD,DGLNCR,DGDBAL
  9000 format(/11(1X,F10.5))

  !  DIAMETER GROWTH EQUATION

  !  THIS IS OUTSIDE BARK ANNUAL DIAMETER INCREMENT THAT WILL BE
  !  CONVERTED TO INSIDE BARK BASAL AREA (DDS) INCREMENT WHEN
  !  LOADED INTO WK2 ARRAY AT END OF TREE LOOP.

  !  ADI = exp(X)

  !  X = b1 + b2 * DBH^2 + b3 * ln(DBH) + b4 * PBAL+ b5 * PRD
  !    + b6 * ln(CR) + b7 * ELEV + b8 * SLOPE
  !    + b9 * SLOPE * cos(ASPECT) + b10*ln(SI)

  !  Example:
  !  X = b1 + b2*(10.0)^2 + b3*ln(10.0) + b4*(100.0) + b5*(0.35)
  !    + b6*ln(45) + b7*1500 + b8*50
  !    + b9*50*cos(3.14159) + b10*ln(75)

  !  WHERE:
  !  ADI = Annual diameter increment (in year-1)
  !  DBH = Diameter at breast height (in)
  !  PBAL = Plot level basal area in larger trees (ft2 * acre-1)
  !  PRD = Plot Relative density (plot Zeide SDI / plot Max SDI � decimal)
  !  CR = Crown ratio (%)
  !  ELEV = Elevation of plot (ft)
  !  SLOPE = Slope of plot (%)
  !  ASPECT = Aspect of plot (radians)
  !  SI = site index (ft)

  !  CALL SDICAL TO LOAD THE POINT MAX SDI WEIGHTED BY SPECIES ARRAY XMAXPT()
  !  RECENT DEAD ARE NOT INCLUDED. IF THEY NEED TO BE, SDICAL.F NEEDS MODIFIED
  !  TO DO SO. IWHO PARAMETER HAS NO AFFECT ON XMAXPT ARRAY VALUES.

  IWHO = 2
  call SDICAL (IWHO, XMAX)

  !  LOAD RELATIVE DENSITY (ZEIDI) FOR INDIVIDUAL POINTS.
  !  ALL SPECIES AND ALL SIZES INCLUDED FOR THIS CALCULATION.

  DLO = 0.0
  DHI = 500.0
  ISPC = 0
  IWHO = 1
  I2 = INT(PI)

  if (DEBUG)write(JOSTND,*) 'IN DGF, CALL SDICLS - POINT COUNT ',I2
  do I1 = 1, I2
     call SDICLS (ISPC,DLO,DHI,IWHO,SDICS,SDICZ,A,B,I1)
     if (DEBUG)write(JOSTND,*) ' POINT ',I1,' SDICZ= ',SDICZ
     ZRD(I1) = SDICZ
  end do

  !     SET ELEVATION, SLOPE, AND ASPECT VALUES
  TEMEL = ELEV * 100.0
  TEMSLP = SLOPE * 100.0
  TEMSASP = TEMSLP * COS(ASPECT)

  !  BEGIN SPECIES LOOP.  ASSIGN VARIABLES WHICH ARE SPECIES DEPENDENT

  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (DEBUG)write(JOSTND,*) &
          & ' TOP OF SPECIES LOOP, ISPC= ',ISPC,' ISCT= ',I1
     if (I1.eq.0) go to 20
     I2 = ISCT(ISPC,2)
     SSITE = SITEAR(ISPC)

     !       ELEVATION, SLOPE, ASPECT AND SITE INDEX COMPONENT OF
     !       DIAMETER GROWTH EQUATION
     !       THESE ARE THE COMPONENTS OF THE EQUATION THAT ARE NOT
     !       TREE SPECIFIC AND ARE LOADED IN DGCONS ENTRY AT END OF
     !       THIS ROUTINE.

     DGCOMP1 = DGEL(ISPC) * TEMEL &
          & + DGSLOP(ISPC) * TEMSLP &
          & + DGSASP(ISPC) * TEMSASP &
          & + DGLNSI(ISPC) * LOG(SSITE)

     !       IF PRESENCE OF PERMAFROST IS TRUE, SET UP FIRST COMPONENTS
     !       OF MODIFIER VARIABLES FOR PERMAFROST AFFECTED SPECIES.
     !       THESE ARE NOT POINT OR TREE SPECIFIC: ELEVATION, SLOPE, ASPECT

     select case (ISPC)
     case (4:7, 13, 16:23)
        PFCOMP1 = PFEL * TEMEL &
             & + PFSLOP * TEMSLP &
             & + PFSASP * TEMSASP
     case default
        PFCOMP1 = 0.0
     end select

     if (DEBUG)write(JOSTND,*) &
          & '  SPECIES VARIABLES, TEMEL=',TEMEL,' TEMSLP=',TEMSLP, &
          & ' ASPECT=',ASPECT,' TEMSASP=',TEMSASP,' SSITE=',SSITE, &
          & ' LOG(SSITE)=',LOG(SSITE)

     !  BEGIN TREE LOOP WITHIN SPECIES ISPC.

     do I3 = I1,I2
        I = IND1(I3)
        D = DIAM(I)
        if (DEBUG)write(JOSTND,*) &
             & ' TOP OF TREE LOOP, I= ',I,' D= ',D
        if (D.le.0.0) goto 10
        D2 = D*D
        CR = real(ICR(I))

        !         BASAL AREA IN LARGER TREES ON THE POINT
        PBAL = PTBALT(I)

        !         RELATIVE DENSITY (ZEIDI) ON THE POINT
        !         CONSTRAIN RD IF NEEDED
        if (XMAXPT(ITRE(I)).le.0.0) then
           PRD = 0.0
        else
           PRD = ZRD(ITRE(I)) / XMAXPT(ITRE(I))
        end if

        !         DIAMETER, PBAL, RELATIVE DENSITY (ZEIDI) AND CROWN RATIO
        !         COMPONENTS OF DIAMETER GROWTH EQUATION.
        !         THESE COMPONENTS USE POINT OR TREE SPECIFIC VLUES.
        DGCOMP2 = DGDISQ(ISPC) * D2 &
             & + DGLD(ISPC) * LOG(D) &
             & + DGDBAL(ISPC) * PBAL &
             & + DGRD(ISPC) * PRD &
             & + DGLNCR(ISPC) * LOG(CR)
        if (DEBUG)write(JOSTND,*) &
             & '  TREE VARIABLES, D^2=',D2,' LOG(D)=',LOG(D), &
             & ' PBAL=',PBAL,' PRD=',PRD,' CR=',CR,' LOG(CR)=',LOG(CR)

        !         ANNUAL DIAMETER GROWTH = exp(X)
        !         X = b1 + b2 * DBH^2 + b3 * ln(DBH) + b4 * PBAL + b5 * PRD
        !             + b6 * ln(CR) + b7 * ELEV + b8 * SLOPE
        !             + b9 * SLOPE * cos(ASPECT) + b10*ln(SI)

        !         DGCOMP2 INCLUDES: b2, b3, b4, b5, b6
        !         DGCOMP1 INCLUDES: b7, b8, b9, b10
        BASEDG = (EXP(DGCONB1(ISPC) + DGCOMP2 + DGCOMP1))

        if (DEBUG)write(JOSTND,*) &
             & '  BASEDG EQN, DGCONB1=',DGCONB1(ISPC),' DGCOMP1=',DGCOMP1, &
             & ' DGCOMP2=', DGCOMP2,' BASEDG=',BASEDG

        !         IF PRESENCE OF PERMAFROST IS TRUE, SET UP SECOND COMPONENTS
        !         OF MODIFIER VARIABLES FOR PERMAFROST AFFECTED SPECIES.
        !         THESE ARE POINT AND TREE SPECIFIC:
        !         DIAMETER, POINT BAL, POINT RELATIVE DENSITY (ZEIDI) AND
        !         CROWN RATIO

        !         COMPUTE PERMAFROST MODIFIER BY COMPUTING THE NUMERATOR WITH
        !         THE EQUATION BELOW. NUMERATOR INCLUDES PERM FACTOR WHEN
        !         PERMAFROST IS TURNED ON FOR AFFECTED SPECIES. NUMERATOR
        !         DOES NOT INCLUDE PERM FACTOR WHEN TURNED OFF FOR AFFECTED
        !         SPECIES.

        !         ADI = exp(X)

        !         X = b1 + PERM + b2 * DBH2 + b3 * ln(DBH) + b4 * PBAL
        !             + b5 * PRD + b6 * ln(CR) + b7 * ELEV + b8 * SLOPE
        !             + b9 * SLOPE * cos(ASPECT)
        !         WHERE:
        !         ADI = Annual diameter increment (in year-1)
        !         PERM = permafrost presence variable
        !         DBH = Diameter at breast height (in)
        !         PBAL = Plot level basal area in larger trees (ft2 * acre-1)
        !         PRD = Relative density (plot Zeide SDI / plot Max SDI - decimal)
        !         CR = Crown ratio (%)
        !         ELEV = Elevation of plot (ft)
        !         SLOPE = Slope of plot (%)
        !         ASPECT = Aspect of plot (radians)
        select case (ISPC)
        case (4:7, 13, 16:23)
           PFCOMP2 = PFDSQ * D2 &
                & + PFLD(ISPC) * LOG(D) &
                & + PFDBAL(ISPC) * PBAL &
                & + PFRD(ISPC) * PRD &
                & + PFLNCR * LOG(CR)
           if (LPERM) then
              PFMOD = EXP(PFCON(ISPC) + PFPRES + PFCOMP2 + &
                   & PFCOMP1)/BASEDG
              if (PFMOD.gt.1.0)PFMOD = 1.0
           else
              PFMOD = EXP(PFCON(ISPC) + PFCOMP2 + &
                   & PFCOMP1)/BASEDG
              if (PFMOD.lt.1.0)PFMOD = 1.0
           end if
        case default
           PFMOD = 1.0
        end select
        if (DEBUG)write(JOSTND,*) &
             & '  PF MODIFIER, PFCON=',PFCON(ISPC),' PFPRES=',PFPRES, &
             & ' PFCOMP1',PFCOMP1,' PFCOMP2',PFCOMP2,' PFMOD= ', PFMOD

        !         ANNUAL DIAMETER GROWTH WITH PERMAFROST MODIFIER
        !         EXPANDED TO PERIOD.
        DGPRED = YR * BASEDG * PFMOD

        !         ADJUST DIAMETER GROWTH BASED ON SPECIES
        !         AD, WI, OH: ASSUME MULTIPLIER OF 0.45
        !         SU: ASSUME MULTIPLIER OF 0.65
        !         ALL OTHER SPECIES: ASSUME A MULTIPLIER OF 1.00
        select case (ISPC)
        case(14,21,23)
           DGPRED = DGPRED * 0.45
        case(22)
           DGPRED = DGPRED * 0.65
        case default
           DGPRED = DGPRED * 1.00
        end select

        if (DEBUG)write(JOSTND,*) &
             & '  YR=',YR,' DGPRED=',DGPRED

        !         CONVERT OUTSIDE BARK DIAMETER INCREMENT TO INSIDE BARK
        !         CHANGE IN SQUARED DIAMETERS AND LOAD INTO WK2 ARRAY.

        BRAT = BRATIO(ISPC,D,HT(I))
        TEMPD1 = D * BRAT ! CURRENT DIB
        DUP = D + DGPRED ! WITH GROWTH DOB
        TEMPD2 = DUP * BRAT ! WITH GROWTH DIB

        !         LOG OF CHANGE IN DIB SQUARED FOR THE PERIOD WITH CALIBRATION
        !         (COR) AND READCORD ADJUSTMENT FROM DGCON.
        DDS = LOG(TEMPD2**2 - TEMPD1**2)+COR(ISPC)+DGCON(ISPC) ! CHANGE IN DIB
        if (DDS.lt.-9.21) DDS = -9.21
        WK2(I) = DDS

        !  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.

        if (DEBUG)write(JOSTND,*) &
             & ' END OF TREE LOOP, BRAT=',BRAT,' TEMPD1=',TEMPD1,' DUP=',DUP, &
             & ' TEMPD2=',TEMPD2,' COR=',COR(ISPC),' DGCON(ISPC)',DGCON(ISPC)
        if (DEBUG)write(JOSTND,*)' DDS=',DDS,'WK2(I)=',WK2(I)
        10 continue
     end do

     !  END OF SPECIES LOOP.

     20 continue
  end do
  if (DEBUG)write(JOSTND,9002)ICYC
  9002 format(' LEAVING SUBROUTINE DGF  CYCLE =',I5)
  return

  entry DGCONS

  !  ENTRY POINT FOR LOADING COEFFICIENTS OF THE DIAMETER INCREMENT
  !  MODEL THAT ARE SITE SPECIFIC AND NEED ONLY BE RESOLVED ONCE.

  !  LOAD ELEVATION, SLOPE AND ASPEC.

  !  ENTER LOOP TO LOAD SPECIES DEPENDENT VECTORS.

  !  SOME CALIBRATION RELATED VALUES MAY BE PROCESSED OR LOADED HERE,
  !  BUT THAT HAS NOT BEEN WORKED OUT YET.

  call DBCHK (DEBUG,'DGCONS',6,ICYC)
  if (DEBUG)write(JOSTND,*) 'ENTER DGCONS'

  do ISPC = 1,MAXSP
     ATTEN(ISPC) = OBSERV(ISPC)

     !  IF READCORD OR REUSCORD WAS SPECIFIED (LDCOR2 IS TRUE) ADD
     !  LN(COR2) TO THE BAI MODEL CONSTANT TERM (DGCON).  COR2 IS
     !  INITIALIZED TO 1.0 IN BLKDATA.
     !  CHANGE TEMSASP BACK TO ASPECT

     if (LDCOR2.and.COR2(ISPC).gt.0.0) then
        DGCON(ISPC) = DGCON(ISPC) + ALOG(COR2(ISPC))
     end if
     !        IF(DEBUG) WRITE(JOSTND,40) TEMEL, TEMSASP, TEMSLP, ISPC,
     !     &  SITEAR(ISPC), DGCON(ISPC), COR2(ISPC)
     !   40   FORMAT(' IN DGF-DGCONS: TEMEL, TEMSASP, TEMSLP, ISPC, ',
     !     &  'SITEAR(ISPC), DGCON(ISPC), COR2(ISPC): ',
     !     &  3F7.1,I5,F7.1,2F10.3)

     if (DEBUG) write(JOSTND,40) ISPC, &
          & SITEAR(ISPC), DGCON(ISPC), COR2(ISPC)
     40 format(' IN DGF-DGCONS: ISPC, SITEAR(ISPC), DGCON(ISPC),', &
          & 'COR2(ISPC): ', I5,F7.1,2F10.3)
  end do

  return
end subroutine DGF
