!========================================!
!   Compute large tree diameter growth   !
!========================================!
! CS $Id$
subroutine DGF(DIAM)
  implicit none

  !  THIS SUBROUTINE COMPUTES THE VALUE OF DDS (CHANGE IN SQUARED
  !  DIAMETER) FOR EACH TREE RECORD, AND LOADS IT INTO THE ARRAY
  !  WK2.  DIAMETER GROWTH IS PREDICTED FROM DBH, SITE INDEX,
  !  CROWN RATIO, AND BASAL AREA PERCENTILES AND QMD FOR TREES
  !  GREATER THAN OR EQUAL TO 5 INCHED DBH. THE
  !  SET OF TREE DIAMETERS TO BE USED IS PASSED AS THE ARGUMENT TO
  !  DIAM.  THE PROGRAM THUS HAS THE FLEXIBILITY TO PROCESS DIFFERENT
  !  CALIBRATION OPTIONS.  THIS ROUTINE IS CALLED BY **DGDRIV** DURING
  !  CALIBRATION AND WHILE CYCLING FOR GROWTH PREDICTION.  ENTRY
  !  **DGCONS** IS CALLED BY **RCON** TO LOAD SITE DEPENDENT COEFFICIENTS
  !  THAT NEED ONLY BE RESOLVED ONCE.

  !  DIAMETER GROWTH EQUATIONS WERE DEVELOPED THROUGH COST SHARE AGREEMENT NO.
  !  10-CS-11132425-258 WITH MICHIGAN TECHNOLOGICAL UNIVERSITY, OCTOBER 2012.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CALCOM.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'
  include 'OUTCOM.f90'
  include 'PDEN.f90'
  include 'PLOT.f90'

  !  DEFINITIONS OF LOCAL VARIABLES.

  !    BAGE5 -- STAND BASAL AREA IN TREES 5.0" DBH AND LARGER.
  !      BAL -- STAND BASAL AREA IN TREES WITH DIAMETER >= THE DIAMETER
  !             OF THE SUBJECT TREE.
  !     BALC -- ARRAY CONTAINING COEFFICIENTS FOR THE BASAL AREA IN
  !             LARGER TREES TERM IN THE DDS MODEL.
  !     BARK -- BARK RATIO
  !   BRATIO -- BARK RATIO FUNCTION CONTAINED IN BRATIO.F
  !   CONSPP -- CONSTANT TERM IN THE DIAMETER GROWTH EQUATION.
  !       CR -- PERCENT CROWN RATIO FOR A TREE
  !    CRSQC -- ARRAY CONTAINING THE COEFFICIENTS FOR THE PERCENT CROWN
  !             RATIO SQUARED TERM IN THE DDS MODEL.
  !    CRWNC -- ARRAY CONTAINING THE COEFFICIENTS FOR THE PERCENT CROWN
  !             RATIO TERM IN THE DDS MODEL.
  !        D -- DIAMETER AT BREAST HEIGHT FOR A TREE
  !     DBHC -- ARRAY CONTAINING COEFFICIENTS FOR THE DIAMETER TERM IN
  !             THE DDS MODEL
  !    DBH2C -- ARRAY CONTAINING COEFFICIENTS FOR THE DIAMETER SQUARED
  !             TERM IN THE DDS MODEL
  !      DDS -- PREDICTED INSIDE-BARK BASAL AREA INCREMENT FOR A TREE
  !    DEBUG -- LOGICAL VARIABLE USED TO CONTROL DEBUG OUTPUT
  !   DIAGRI -- PREDICTED INSIDE-BARK DIAMETER GROWTH FOR A TREE
  !   DIAGRO -- PREDICTED OUTSIDE-BARK DIAMETER GROWTH FOR A TREE
  !     DIAM -- ARRAY LOADED WITH TREE DIAMETERS
  ! D1 - D11 -- EQUATION COEFFICIENTS FOR A DEBUG WRITE STATEMENT
  !     I,IK -- ARRAY INDICIES
  !   INTERC -- DDS REGRESSION EQUATION INTERCEPT
  !     ISPC -- FVS SPECIES SEQUENCE NUMBER
  ! I1,I2,I3 -- ARRAY INDICIES
  !   OBSERV -- SAMPLE SIZE USED TO FIT THE EQUATION
  !        P -- TREES-PER-ACRE FOR A TREE RECORD
  !   QMDGE5 -- QUADRATIC MEAN DIAMETER FOR TREES 5.0" DBH AND LARGER
  !    RDBHC -- ARRAY CONTAINING COEFFICIENTS FOR DIAM/QMDGE5
  !             TERM IN THE DDS MODEL.
  !  RDBHSQC -- ARRAY CONTAINING COEFFICIENTS FOR THE DIAM*DIAM/QMDGE5
  !             TERM IN THE DDS MODEL.
  !   RELDBH -- RELATIVE TREE DIAMETER (D/QMDGE5)
  ! RELDBHSQ -- RELATIVE TREE DIAMTER SQUARED ((D**2)/QMDGE5)
  !     SBAC -- ARRAY CONTAINING THE COEFFICIENTS FOR STAND BASAL AREA
  !             IN TREES 5.0" DBH AND LARGER TERM IN THE DDS MODEL.
  !   SDQGE5 -- SUM OF TREE DIAMETERS SQUARED FOR THE STAND
  !    SITEC -- ARRAY CONTAINING COEFFICIENTS FOR SITE INDEX IN THE DDS MODEL.
  !        T -- TOTAL STAND TREES-PER-ACRE
  !    VDBHC -- ARRAY CONTAINING COEFFICIENTS FOR THE INVERSE OF DIAMETER
  !             TERM IN THE DDS MODEL.

  !  SPECIES AND COEFFICIENT ORDER

  !  NOTE -- other softwood (6=OS) uses eastern white pine (7=WP)
  !          as surrogate for all coefficients.

  !    1=RC   2=JU    3=SP   4=VP   5=LP
  !    6=OS   7=WP    8=WN   9=BN  10=TL
  !   11=TS  12=WT   13=BG  14=HS  15=SH
  !   16=SL  17=MH   18=PH  19=HI  20=WH
  !   21=BH  22=PE   23=BI  24=AB  25=BA
  !   26=PA  27=UA   28=EC  29=RM  30=BE
  !   31=SV  32=BC   33=AE  34=SG  35=HK
  !   36=WE  37=EL   38=SI  39=RL  40=RE
  !   41=YP  42=BW   43=SM  44=AS  45=WA
  !   46=GA  47=WO   48=RO  49=SK  50=BO
  !   51=SO  52=BJ   53=CK  54=SW  55=BR
  !   56=SN  57=PO   58=DO  59=CO  60=PN
  !   61=CB  62=QI   63=OV  64=WK  65=NK
  !   66=WL  67=QS   68=UH  69=SS  70=OB
  !   71=CA  72=PS   73=HL  74=BP  75=BT
  !   76=QA  77=BK   78=OL  79=SY  80=BY
  !   81=RB  82=SU   83=WI  84=BL  85=NC
  !   86=AH  87=RD   88=DW  89=HT  90=KC
  !   91=OO  92=CT   93=MV  94=MB  95=HH
  !   96=SD

  ! Variables
  logical :: DEBUG

  integer :: I,IK,ISPC,I1,I2,I3

  real    :: BAGE5,BAL,BARK,BRATIO,CONSPP,CR,D,DDS,DIAGRO,DIAGRI
  real    :: D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11
  real    :: P,QMDGE5,RELDBH,RELDBHSQ,SDQGE5,T

  real    :: BALC(MAXSP),CRSQC(MAXSP),CRWNC(MAXSP),DBHC(MAXSP),DBH2C(MAXSP),DIAM(MAXTRE),&
       &INTERC(MAXSP),OBSERV(MAXSP),RDBHC(MAXSP),RDBHSQC(MAXSP),SBAC(MAXSP),SITEC(MAXSP),VDBHC(MAXSP)

  !  DATA STATEMENTS:

  !  COEFFICIENTS FOR DDS MODEL

  data INTERC/&
       &3.09820,  3.09820,  0.64882,  2.76330,  2.76330,&
       &-0.7079,  -0.7079,  2.95910,  2.95910, -0.28233,&
       &-0.28233, -0.28233, -0.28233,  1.08270,  1.08270,&
       &1.08270,  1.08270, -0.64747, -0.64747, -0.64747,&
       &-0.64747, -0.64747, -0.64747, -0.07129,  0.89302,&
       &0.89302,  0.89302,  1.81510,  2.23020,  2.23020,&
       &2.23020,  3.23650,  1.23520,  1.23520,  1.23520,&
       &1.23520,  1.23520,  1.23520,  1.23520,  1.23520,&
       &3.4264,  3.71790,  2.97130, -0.29734, -0.29734,&
       &-0.29734,  1.09010,  0.02098,  0.02098,  1.89700,&
       &2.0352,  3.86780,  0.93769,  0.93769,  0.93769,&
       &0.93769,  1.36920,  1.36920, -1.73610,  3.98500,&
       &3.98500,  3.98500,  0.93769,  3.98500,  3.98500,&
       &3.98500,  3.98500,  3.02200,  3.38410,  3.02200,&
       &3.02200,  3.02200,  3.21620,  3.02200,  3.02200,&
       &3.02200,  3.02200,  3.86690,  3.86690,  3.86690,&
       &3.86690,  3.86690,  3.86690,  3.86690,  3.03080,&
       &3.03080,  3.03080,  3.03080,  3.03080,  2.95910,&
       &-0.18599,  3.03080,  3.03080,  3.03080,  3.03080,&
       &3.03080/

  data VDBHC/&
       &-8.58440,  -8.58440,       0.0, -10.45000, -10.45000,&
       &0.0,       0.0,  -9.51990,  -9.51990,       0.0,&
       &0.0,       0.0,       0.0,  -5.46950,  -5.46950,&
       &-5.46950,  -5.46950,       0.0,       0.0,       0.0,&
       &0.0,       0.0,       0.0,       0.0, -14.16700,&
       &-14.16700, -14.16700,       0.0,  -6.99620,  -6.99620,&
       &-6.99620, -10.26800,  -5.21440,  -5.21440,  -5.21440,&
       &-5.21440,  -5.21440,  -5.21440,  -5.21440,  -5.21440,&
       &-7.8659,  -8.96970, -10.11500,       0.0,       0.0,&
       &0.0,  -3.68390,       0.0,       0.0,  -5.72030,&
       &-5.2884, -11.27600,  -5.11930,  -5.11930,  -5.11930,&
       &-5.11930,  -5.03520,  -5.03520,       0.0, -13.51900,&
       &-13.51900, -13.51900,  -5.11930, -13.51900, -13.51900,&
       &-13.51900, -13.51900, -13.62700, -10.28200, -13.62700,&
       &-13.62700, -13.62700,  -8.59700, -13.62700, -13.62700,&
       &-13.62700 ,-13.62700,  -8.87037,  -8.87037,  -8.87037,&
       &-8.87037,  -8.87037,  -8.87037,  -8.87037,  -8.10590,&
       &-8.10590,  -8.10590,  -8.10590,  -8.10590,  -9.51990,&
       &0.0,  -8.10590,  -8.10590,  -8.10590,  -8.10590,&
       &-8.10590/

  data DBHC/&
       &0.0,     0.0,     0.0,     0.0,      0.0,&
       &0.07533, 0.07533,     0.0,     0.0,      0.0,&
       &0.0,     0.0,     0.0, 0.12337,  0.12337,&
       &0.12337, 0.12337, 0.26917, 0.26917,  0.26917,&
       &0.26917, 0.26917, 0.26917, 0.14458,      0.0,&
       &0.0,     0.0, 0.09153, 0.11324,  0.11324,&
       &0.11324,     0.0, 0.18300, 0.18300,  0.18300,&
       &0.18300, 0.18300, 0.18300, 0.18300,  0.18300,&
       &0.057705,     0.0,     0.0, 0.20494,  0.20494,&
       &0.20494, 0.11623, 0.14061, 0.14061,      0.0,&
       &0.0,     0.0, 0.09743, 0.09743,  0.09743,&
       &0.09743, 0.12165, 0.12165, 0.26087,      0.0,&
       &0.0,     0.0, 0.09743,     0.0,      0.0,&
       &0.0,     0.0,     0.0,     0.0,      0.0,&
       &0.0,     0.0,     0.0,     0.0,      0.0,&
       &0.0,     0.0,     0.0,     0.0,      0.0,&
       &0.0,     0.0,     0.0,     0.0,      0.0,&
       &0.0,     0.0,     0.0,     0.0,      0.0,&
       &0.20178,     0.0,     0.0,     0.0,      0.0,&
       &0.0/

  data DBH2C/&
       &0.0,       0.0,  0.01234,      0.0,      0.0,&
       &0.0,       0.0,      0.0,      0.0,  0.01581,&
       &0.01581,   0.01581,  0.01581,      0.0,      0.0,&
       &0.0,       0.0, -0.00342, -0.00342, -0.00342,&
       &-0.00342,  -0.00342, -0.00342,      0.0,      0.0,&
       &0.0,       0.0,      0.0, -0.00186, -0.00186,&
       &-0.00186,       0.0, -0.00314, -0.00314, -0.00314,&
       &-0.00314,  -0.00314, -0.00314, -0.00314, -0.00314,&
       &0.0,       0.0,      0.0,      0.0,      0.0,&
       &0.0,       0.0,      0.0,      0.0,  0.00346,&
       &0.0051307,       0.0,      0.0,      0.0,      0.0,&
       &0.0,       0.0,      0.0,      0.0,      0.0,&
       &0.0,       0.0,      0.0,      0.0,      0.0,&
       &0.0,       0.0,      0.0,      0.0,      0.0,&
       &0.0,       0.0,      0.0,      0.0,      0.0,&
       &0.0,       0.0,      0.0,      0.0,      0.0,&
       &0.0,       0.0,      0.0,      0.0,      0.0,&
       &0.0,       0.0,      0.0,      0.0,      0.0,&
       &0.0,       0.0,      0.0,      0.0,      0.0,&
       &0.0/

  data RDBHC/&
       &0.0,     0.0,  2.87386,      0.0,      0.0,&
       &2.12877, 2.12877,      0.0,      0.0,  3.37463,&
       &3.37463, 3.37463,  3.37463,  0.40029,  0.40029,&
       &0.40029, 0.40029,  0.48856,  0.48856,  0.48856,&
       &0.48856, 0.48856,  0.48856,  1.85880,      0.0,&
       &0.0,     0.0,  1.02090,      0.0,      0.0,&
       &0.0,     0.0,      0.0,      0.0,      0.0,&
       &0.0,     0.0,      0.0,      0.0,      0.0,&
       &0.0,     0.0,      0.0,  1.03840,  1.03840,&
       &1.03840, 0.70038,  1.21010,  1.21010,  0.93903,&
       &1.1685,     0.0,  0.47749,  0.47749,  0.47749,&
       &0.47749,     0.0,      0.0,  1.76660,      0.0,&
       &0.0,     0.0,  0.47749,      0.0,      0.0,&
       &0.0,     0.0,      0.0,      0.0,      0.0,&
       &0.0,     0.0,      0.0,      0.0,      0.0,&
       &0.0,     0.0,      0.0,      0.0,      0.0,&
       &0.0,     0.0,      0.0,      0.0,      0.0,&
       &0.0,     0.0,      0.0,      0.0,      0.0,&
       &0.54649,     0.0,      0.0,      0.0,      0.0,&
       &0.0/

  data RDBHSQC/&
       &0.0,       0.0,  -0.20916,      0.0,      0.0,&
       &-0.05909,  -0.05909,       0.0,      0.0, -0.26631,&
       &-0.26631,  -0.26631,  -0.26631, -0.03136, -0.03136,&
       &-0.03136,  -0.03136,  -0.03745, -0.03745, -0.03745,&
       &-0.03745,  -0.03745,  -0.03745, -0.08029,      0.0,&
       &0.0,       0.0,  -0.03235,      0.0,      0.0,&
       &0.0,       0.0,       0.0,      0.0,      0.0,&
       &0.0,       0.0,       0.0,      0.0,      0.0,&
       &-0.019995,       0.0,       0.0, -0.08002, -0.08002,&
       &-0.08002,  -0.04289,  -0.05494, -0.05494, -0.05092,&
       &-0.071643,       0.0,  -0.02443, -0.02443, -0.02443,&
       &-0.02443,  -0.03038,  -0.03038, -0.11006,      0.0,&
       &0.0,       0.0,  -0.02443,      0.0,      0.0,&
       &0.0,       0.0,       0.0,      0.0,      0.0,&
       &0.0,       0.0,       0.0,      0.0,      0.0,&
       &0.0,       0.0,       0.0,      0.0,      0.0,&
       &0.0,       0.0,       0.0,      0.0,      0.0,&
       &0.0,       0.0,       0.0,      0.0,      0.0,&
       &-0.04667,       0.0,       0.0,      0.0,      0.0,&
       &0.0/

  data SBAC/&
       &-0.00386,  -0.00386, -0.00162,  -0.00324,  -0.00324,&
       &0.0,       0.0,      0.0,       0.0,       0.0,&
       &0.0,       0.0,      0.0,  -0.00160,  -0.00160,&
       &-0.00160,  -0.00160, -0.00230,  -0.00230,  -0.00230,&
       &-0.00230,  -0.00230, -0.00230,       0.0,       0.0,&
       &0.0,       0.0,      0.0,       0.0,       0.0,&
       &0.0,       0.0, -0.00439,  -0.00439,  -0.00439,&
       &-0.00439,  -0.00439, -0.00439,  -0.00439,  -0.00439,&
       &0.0,       0.0,      0.0,       0.0,       0.0,&
       &0.0,       0.0,      0.0,       0.0,  -0.00176,&
       &0.00240,  -0.00537,      0.0,       0.0,       0.0,&
       &0.0,       0.0,      0.0,       0.0,       0.0,&
       &0.0,       0.0,      0.0,       0.0,       0.0,&
       &0.0,       0.0, -0.00288,       0.0,  -0.00288,&
       &-0.00288,  -0.00288,      0.0,  -0.00288,  -0.00288,&
       &-0.00288,  -0.00288,      0.0,       0.0,       0.0,&
       &0.0,       0.0,      0.0,       0.0,  -0.00179,&
       &-0.00179,  -0.00179, -0.00179,  -0.00179,       0.0,&
       &-0.00704,  -0.00179, -0.00179,  -0.00179,  -0.00179,&
       &-0.00179/

  data BALC/&
       &0.0,        0.0,  -0.00318,       0.0,       0.0,&
       &0.0,        0.0,  -0.00169,  -0.00169,       0.0,&
       &0.0,        0.0,       0.0,       0.0,       0.0,&
       &0.0,        0.0,       0.0,       0.0,       0.0,&
       &0.0,        0.0,       0.0,       0.0,       0.0,&
       &0.0,        0.0,  -0.00428,  -0.00171,  -0.00171,&
       &-0.00171,   -0.00150,   0.00331,   0.00331,   0.00331,&
       &0.00331,    0.00331,   0.00331,   0.00331,   0.00331,&
       &-0.0052144,   -0.00278,  -0.00303,  -0.00195,  -0.00195,&
       &-0.00195,   -0.00302,       0.0,       0.0,       0.0,&
       &-0.0013941,        0.0,  -0.00092,  -0.00092,  -0.00092,&
       &-0.00092,   -0.00351,  -0.00351,       0.0,  -0.00233,&
       &-0.00233,   -0.00233,  -0.00092,  -0.00233,  -0.00233,&
       &-0.00233,   -0.00233,       0.0,  -0.00170,       0.0,&
       &0.0,        0.0,  -0.00261,       0.0,       0.0,&
       &0.0,        0.0,  -0.00535,  -0.00535,  -0.00535,&
       &-0.00535,   -0.00535,  -0.00535,  -0.00535,       0.0,&
       &0.0,        0.0,       0.0,       0.0,  -0.00169,&
       &0.00554,        0.0,       0.0,       0.0,       0.0,&
       &0.0/

  data CRWNC/&
       &0.01628, 0.01628,  0.02252,  0.05754,  0.05754,&
       &0.06590, 0.06590,  0.03012,  0.03012,  0.01650,&
       &0.01650, 0.01650,  0.01650,  0.02491,  0.02491,&
       &0.02491, 0.02491,  0.02673,  0.02673,  0.02673,&
       &0.02673, 0.02673,  0.02673,  0.01373,  0.15110,&
       &0.15110, 0.15110,      0.0,  0.03114,  0.03114,&
       &0.03114, 0.02457,  0.02814,  0.02814,  0.02814,&
       &0.02814, 0.02814,  0.02814,  0.02814,  0.02814,&
       &0.04625, 0.01517,  0.02898,  0.02569,  0.02569,&
       &0.02569, 0.02757,  0.01366,  0.01366,  0.03088,&
       &0.03738, 0.00904,  0.01140,  0.01140,  0.01140,&
       &0.01140, 0.01313,  0.01313,  0.01933,  0.03135,&
       &0.03135, 0.03135,  0.01140,  0.03135,  0.03135,&
       &0.03135, 0.03135,  0.04569,  0.03201,  0.04569,&
       &0.04569, 0.04569,  0.01869,  0.04569,  0.04569,&
       &0.04569, 0.04569,  0.02034,  0.02034,  0.02034,&
       &0.02034, 0.02034,  0.02034,  0.02034,  0.00489,&
       &0.00489, 0.00489,  0.00489,  0.00489,  0.03012,&
       &0.04276, 0.00489,  0.00489,  0.00489,  0.00489,&
       &0.00489/

  data CRSQC/&
       &0.0,      0.0,      0.0, -0.00041, -0.00041,&
       &-0.00046, -0.00046,      0.0,      0.0,      0.0,&
       &0.0,      0.0,      0.0, -0.00017, -0.00017,&
       &-0.00017, -0.00017, -0.00021, -0.00021, -0.00021,&
       &-0.00021, -0.00021, -0.00021,      0.0, -0.00141,&
       &-0.00141, -0.00141,  0.00031, -0.00014, -0.00014,&
       &-0.00014,      0.0, -0.00011, -0.00011, -0.00011,&
       &-0.00011, -0.00011, -0.00011, -0.00011, -0.00011,&
       &-0.000227,      0.0, -0.00013,      0.0,      0.0,&
       &0.0, -0.00021,      0.0,      0.0, -0.00016,&
       &-0.000312,      0.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0,      0.0,      0.0, -0.00019,&
       &-0.00019, -0.00019,      0.0, -0.00019, -0.00019,&
       &-0.00019, -0.00019, -0.00037, -0.00026, -0.00037,&
       &-0.00037, -0.00037,      0.0, -0.00037, -0.00037,&
       &-0.00037, -0.00037,      0.0,      0.0,      0.0,&
       &0.0,      0.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0,      0.0,      0.0,      0.0,&
       &-0.00024,      0.0,      0.0,      0.0,      0.0,&
       &0.0/

  data SITEC/&
       &0.0,      0.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0,  0.00356,  0.00356,      0.0,&
       &0.0,      0.0,      0.0,  0.00543,  0.00543,&
       &0.00543,  0.00543,  0.00744,  0.00744,  0.00744,&
       &0.00744,  0.00744,  0.00744,      0.0,      0.0,&
       &0.0,      0.0,      0.0,      0.0,      0.0,&
       &0.0,  0.00235,  0.00447,  0.00447,  0.00447,&
       &0.00447,  0.00447,  0.00447,  0.00447,  0.00447,&
       &-0.00431,     0.0,  0.00383,  0.00424,  0.00424,&
       &0.00424,  0.00351,  0.00955,  0.00955,  0.00540,&
       &0.0,      0.0,  0.01240,  0.01240,  0.01240,&
       &0.01240,  0.00915,  0.00915,  0.00774,      0.0,&
       &0.0,      0.0,  0.01240,      0.0,      0.0,&
       &0.0,      0.0,  0.00766,      0.0,  0.00766,&
       &0.00766,  0.00766,  0.00599,  0.00766,  0.00766,&
       &0.00766,  0.00766,      0.0,      0.0,      0.0,&
       &0.0,      0.0,      0.0,      0.0,      0.0,&
       &0.0,      0.0,      0.0,      0.0,  0.00356,&
       &0.0,      0.0,      0.0,      0.0,      0.0,&
       &0.0/

  data OBSERV/&
       &4111.0,  4111.0, 2864.0,  216.0,  216.0,&
       &283.0,   283.0, 2298.0, 2298.0,  677.0,&
       &677.0,   677.0,  677.0, 3872.0, 3872.0,&
       &3872.0,  3872.0, 3980.0, 3980.0, 3980.0,&
       &3980.0,  3980.0, 3980.0,  326.0,   75.0,&
       &75.0,    75.0,  416.0, 3300.0, 3300.0,&
       &3300.0,  1686.0, 5969.0, 5969.0, 5969.0,&
       &5969.0,  5969.0, 5969.0, 5969.0, 5969.0,&
       &1113.0,   576.0, 2962.0, 2958.0, 2958.0,&
       &2958.0, 10044.0, 2370.0, 2370.0, 6259.0,&
       &1368.0,   580.0, 1818.0, 1818.0, 1818.0,&
       &1818.0,  5289.0, 5289.0,  213.0, 1453.0,&
       &1453.0,  1453.0, 1818.0, 1453.0, 1453.0,&
       &1453.0,  1453.0,  982.0, 1028.0,  982.0,&
       &982.0,   982.0, 1035.0,  982.0,  982.0,&
       &982.0,   982.0, 1422.0, 1422.0, 1422.0,&
       &1422.0,  1422.0, 1422.0, 1422.0,  589.0,&
       &589.0,   589.0,  589.0,  589.0, 2298.0,&
       &945.0,   589.0,  589.0,  589.0,  589.0,&
       &589.0/

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'DGF',3,ICYC)
  if (DEBUG) write(JOSTND,5)ICYC
  5 format(' ENTERING SUBROUTINE DGF  CYCLE =',I5)

  !  INITIALIZE VARIABLES FOR DBH >= 5 IN

  SDQGE5 = 0.
  T = 0.
  BAGE5 = 0.
  QMDGE5 = 0.

  !  COMPUTE BASAL AREA IN TREES >= 5.0" DBH,
  !  AND QMD FOR TREES >= 5.0" DBH

  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (I1.le.0)go to 20
     I2 = ISCT(ISPC,2)
     do I3 = I1,I2
        I = IND1(I3)
        P = PROB(I)
        D = DIAM(I)
        if (D.lt.5.)go to 10
        SDQGE5 = SDQGE5+P*(D)**2.
        T = T+P
        BAGE5 = BAGE5+D*D*P*0.005454154
        10 continue
     end do
     20 continue
  end do
  if (SDQGE5.gt.0.)QMDGE5 = SQRT(SDQGE5/T)
  if (BAGE5.le.0.) BAGE5 = 10.

  !  BEGIN SPECIES LOOP.

  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (I1.eq.0) go to 200
     I2 = ISCT(ISPC,2)
     CONSPP = DGCON(ISPC) + COR(ISPC)

     !  BEGIN TREE LOOP WITHIN SPECIES ISPC.

     do I3 = I1,I2
        I = IND1(I3)
        D = DIAM(I)
        CR = ICR(I)
        if (CR .le.0.) CR = 10.
        BAL = (1.0 - (PCT(I)/100.)) * BA

        if (D.le.0.0) go to 100

        !  BOUND QMD FOR SOME SPECIES

        select case (ISPC)
        case(50) ! BO
           if (QMDGE5.gt.12.) QMDGE5 = 12.
        case(3,10:13) ! SP,TL,TS,WT,BG
           if (QMDGE5.gt.13.) QMDGE5 = 13.
        case(14:17,28,53:56) ! HS,SH,SL,MH,EC,CK,SW,BR,SN
           if (QMDGE5.gt.25.) QMDGE5 = 25.
        case(24) ! AB
           if (QMDGE5.gt.40.) QMDGE5 = 40.
        case(51) ! SO
           if (QMDGE5.gt.11.) QMDGE5 = 11.
        case(44:46, 59) ! AS,WA,GA,CO
           if (QMDGE5.gt.20.) QMDGE5 = 20.
        case(48:49) ! RO,SK
           if (QMDGE5.gt.30.) QMDGE5 = 30.
        case(91) ! OO
           if (QMDGE5.gt.17.) QMDGE5 = 17.
        case default
        end select

        !  BOUND CROWN RATIO FOR SOME SPECIES

        select case (ISPC)
        case(7) ! WP
           if (CR .gt.50.) CR = 50.
        case(8:10) ! WN,BN,TL
           if (CR.gt.75.) CR = 75.
        case(28,41) ! EC,YP
           if (CR .gt.60.) CR = 60.
        case(44:46) ! AS,WA,GA
           if (CR .gt.80.) CR = 80.
        case(32,78:84) ! BC,SY,BY,RB,SU,WI,BL
           if (CR .gt.85.) CR = 85.
        case default
        end select

        !  RELATIVE DBH

        RELDBH = 0.0
        RELDBHSQ = 0.0
        if (QMDGE5.gt.0.0) RELDBH = D/QMDGE5
        if (QMDGE5.gt.0.0) RELDBHSQ = D*D/QMDGE5

        if (DEBUG) write(JOSTND,*) 'IN DGF-I= ',I,' D=',D,&
             &' RELDBH= ',RELDBH,' SI= ',SITEAR(ISPC),&
             &'BAGE5=',BAGE5,'CR=',CR,' QMDGE5= ',QMDGE5,&
             &' BAL= ',BAL,' CONSPP= ',CONSPP

        !  CALCULATION OF DDS FOR CENTRAL STATES VARIANT

        DDS = CONSPP&
             &+ INTERC(ISPC)&
             &+ VDBHC(ISPC) * 1./D&
             &+ DBHC(ISPC) * D&
             &+ DBH2C(ISPC) * D * D&
             &+ RDBHC(ISPC) * RELDBH&
             &+ RDBHSQC(ISPC) * RELDBHSQ&
             &+ CRWNC(ISPC) * CR&
             &+ CRSQC(ISPC) * CR*CR&
             &+ SBAC(ISPC) * BAGE5&
             &+ BALC(ISPC) * BAL&
             &+ SITEC(ISPC) * SITEAR(ISPC)

        !  IF DEBUGGING, OUTPUT TERMS IN THE EQUATION

        if (DEBUG) then
           D1 = INTERC(ISPC)
           D2 = VDBHC(ISPC) * 1./D
           D3 = DBHC(ISPC) * D
           D4 = DBH2C(ISPC) * D * D
           D5 = RDBHC(ISPC) * RELDBH
           D6 = RDBHSQC(ISPC) * RELDBHSQ
           D7 = CRWNC(ISPC) * CR
           D8 = CRSQC(ISPC) * CR*CR
           D9 = SBAC(ISPC) * BAGE5
           D10 = BALC(ISPC) * BAL
           D11 = SITEC(ISPC) * SITEAR(ISPC)

           write(JOSTND,9090)D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11,DDS
           9090 format(' IN DGF ,D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11,DDSO=',&
                &/,15F9.3)
        end if

        !  CALCULATION OF DDS FOR CENTRAL STATES VARIANT WAS BUILT
        !  DDS OUTSIDE BARK, CONVERT TO INSIDE BARK

        if (DDS.lt.-9.21) DDS = -9.21
        DIAGRO = SQRT(DBH(I)*DBH(I)+EXP(DDS))-DBH(I)
        BARK = BRATIO(ISPC,DBH(I),HT(I))
        DIAGRI = DIAGRO*BARK
        DDS = ALOG(((DBH(I)*BARK+DIAGRI)**2.0)-(DBH(I)*BARK)**2.0)
        if (DEBUG) write(JOSTND,*) 'IN DGF-I= ',I,' D=',D,&
             &' DIAGRO= ',DIAGRO,' DIAGRI= ',DIAGRI,' BARK= ',BARK,&
             &' DDS= ',DDS

        if (DDS.lt.-9.21) DDS = -9.21
        WK2(I) = DDS

        !  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.

        if (DEBUG) then
           write(JOSTND,9095) I,ISPC,DDS
           9095 format(' IN DGF, I=',I4,',  ISPC=',I3,',  DDS=',F7.4)
           write(JOSTND,*)' IN DGF, DG= ',(DG(IK), IK = 1,50)
        end if

        100 continue
     end do

     !  END OF SPECIES LOOP.

     200 continue
  end do
  if (DEBUG) write(JOSTND,9100)ICYC
  9100 format(' LEAVING SUBROUTINE DGF  CYCLE =',I5)

  return

  entry DGCONS

  !  ENTRY POINT FOR LOADING COEFFICIENTS OF THE DIAMETER INCREMENT
  !  MODEL THAT ARE SITE SPECIFIC AND NEED ONLY BE RESOLVED ONCE.
  !  OBSERV CONTAINS THE NUMBER OF
  !  OBSERVATIONS BY SPECIES FOR THE UNDERLYING MODEL
  !  (THIS DATA IS USED BY **DGDRIV** FOR CALIBRATION).

  !  ENTER LOOP TO LOAD SPECIES DEPENDENT VECTORS.

  do ISPC = 1,MAXSP
     DGCON(ISPC) = 0.
     ATTEN(ISPC) = OBSERV(ISPC)
     SMCON(ISPC) = 0.

     !  IF READCORD OR REUSCORD WAS SPECIFIED (LDCOR2 IS TRUE) ADD
     !  LN(COR2) TO THE BAI MODEL CONSTANT TERM (DGCON).  COR2 IS
     !  INITIALIZED TO 1.0 IN BLKDATA.

     if (LDCOR2.and.COR2(ISPC).gt.0.0) DGCON(ISPC) = DGCON(ISPC)&
          &+ ALOG(COR2(ISPC))
  end do

  return
end
