!========================================!
!   Compute large tree diameter growth   !
!========================================!
! VBASE $Id$
subroutine DGF(DIAM)
  implicit none

  !  THIS SUBROUTINE COMPUTES THE VALUE OF DDS (CHANGE IN SQUARED
  !  DIAMETER) FOR EACH TREE RECORD, AND LOADS IT INTO THE ARRAY
  !  WK2.  DDS IS PREDICTED FROM SITE INDEX, LOCATION, SLOPE,
  !  ASPECT, ELEVATION, DBH, CROWN RATIO, BASAL AREA IN LARGER TREES,
  !  AND CCF.  THE SET OF TREE DIAMETERS TO BE USED IS PASSED AS THE
  !  ARGUEMENT DIAM.  THE PROGRAM THUS HAS THE FLEXIBILITY TO
  !  PROCESS DIFFERENT CALIBRATION OPTIONS.  THIS ROUTINE IS CALLED
  !  BY **DGDRIV** DURING CALIBRATION AND WHILE CYCLING FOR GROWTH
  !  PREDICTION.  ENTRY **DGCONS** IS CALLED BY **RCON** TO LOAD SITE
  !  DEPENDENT COEFFICIENTS THAT NEED ONLY BE RESOLVED ONCE.

  !  ONE SHARED DGF FOR THE VARIANTS THAT SELECT vbase/dgf.f90.  EACH
  !  EQUATION FAMILY IS AN INTERNAL PROCEDURE HOLDING ITS VARIANTS'
  !  ORIGINAL EQUATIONS, COEFFICIENT BANKS, DIAGNOSTICS AND PERIOD
  !  CONVERSION; CONSTANT VARIANT GUARDS SELECT THE FAMILY, SO A FAMILY'S
  !  CALLS ARE COMPILED ONLY INTO ITS OWN VARIANTS.  SPECIES INDICES
  !  ALWAYS REFER TO THE SELECTED VARIANT'S BLKDAT ORDER.
  !  THE DGCONS ENTRY IS SELECTED BY THE BUILD: EM'S **RCON** PASSES THE
  !  HABITAT TYPE INDEX IDTYPE (em/common/DGCONS_ENTRY.f90); ALL OTHER
  !  VARIANTS USE common/DGCONS_ENTRY.f90 WITHOUT AN ARGUMENT.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CALCOM.f90'
  include 'ARRAYS.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'
  include 'OUTCOM.f90'
  include 'PLOT.f90'
  include 'PDEN.f90'
  include 'GGCOM.f90'
  include 'VARCOM.f90'
  include 'CICOM.f90'
  include 'KOTCOM.f90'

  !     DIAM -- ARRAY LOADED WITH TREE DIAMETERS (PASSED AS AN
  !             ARGUEMENT).
  !   IDTYPE -- EM HABITAT TYPE INDEX PASSED TO **DGCONS** (EM ONLY).
  !    LCONS -- TRUE WHEN ENTERED THROUGH **DGCONS**.
  real    :: DIAM(MAXTRE)
  integer :: IDTYPE
  logical :: LCONS

  LCONS = .false.
  go to 1
  include 'DGCONS_ENTRY.f90'
  LCONS = .true.
  1 continue
  if (VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP'.or.&
       &VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'WC') call DGFPAC(LCONS)
  if (VARIANTCD.eq.'TT'.or.VARIANTCD.eq.'UT') call DGFTU(LCONS)
  if (VARIANTCD.eq.'IE'.or.VARIANTCD.eq.'EM') call DGFIE(LCONS)
  if (VARIANTCD.eq.'CI') call DGFCI(LCONS)
  if (VARIANTCD.eq.'KT') call DGFKT(LCONS)
  return

contains

  subroutine DGFPAC(LCONS)
    implicit none

    !  PACIFIC FAMILY (CA, OC, OP, PN, WC).  COEFFICIENT BANK IB:
    !    1 = CA/OC (13 EQUATIONS), 2 = OP/PN (20), 3 = WC (19).
    !  OC AND OP PREDICT 10-YEAR GROWTH AND HALVE IT IN REAL SCALE.
    !  CA/OC INDEX DGDSQ AND ATTEN BY SPECIES; OP/PN/WC INDEX THEM BY
    !  EQUATION (FOUND_BUGS 33).  ONLY OP/PN/WC SET ISPDSQ (CALCOM).

    ! SPECIES ORDER IN CA AND OC (OC CALLS SPECIES 4 GF):
    !  1=PC  2=IC  3=RC  4=WF  5=RF  6=SH  7=DF  8=WH  9=MH 10=WB
    ! 11=KP 12=LP 13=CP 14=LM 15=JP 16=SP 17=WP 18=PP 19=MP 20=GP
    ! 21=JU 22=BR 23=GS 24=PY 25=OS 26=LO 27=CY 28=BL 29=EO 30=WO
    ! 31=BO 32=VO 33=IO 34=BM 35=BU 36=RA 37=MA 38=GC 39=DG 40=FL
    ! 41=WN 42=TO 43=SY 44=AS 45=CW 46=WI 47=CN 48=CL 49=OH 50=RW

    ! CA/OC DIAMETER GROWTH EQUATIONS:
    !  1=IC    ALSO USED FOR PC & RC
    !  2=WF    ALSO USED FOR BR
    !  3=RF/SH
    !  4=DF
    !  5=KP    ALSO USED FOR CP, LM, GP, JU, PY, & CN
    !  6=LP    ALSO USED FOR WB
    !  7=SP    ALSO USED FOR WH & MH
    !  8=WP
    !  9=PP    ALSO USED FOR JP, MP, & OS
    ! 10=OAKS (LO/CY/BL/EO/WO/BO/VO/IO)
    !          ALSO USED FOR BM, BU, RA, OA, WN, SY, AS, CW, WI, CL, OH
    ! 11=MA    ALSO USED FOR GC & DG
    ! 12=GS/RW NEW EQUATIONS FIT BY CASTLE IN 2019
    ! 13=TO    BORROWED FROM NC VARIANT

    !  OP/PN/WC SPECIES ORDER (23,24 ARE MA,TO IN OP AND WA,PB IN PN/WC;
    !  WC LEAVES 6 BLANK):
    !                1=SF,  2=WF,  3=GF,  4=AF,  5=RF,  6=SS,  7=NF,  8=YC,
    !                9=IC, 10=ES, 11=LP, 12=JP, 13=SP, 14=WP, 15=PP, 16=DF,
    !               17=RW, 18=RC, 19=WH, 20=MH, 21=BM, 22=RA, 23=MA, 24=TO,
    !               25=GC, 26=AS, 27=CW, 28=WO, 29=J , 30=LL, 31=WB, 32=KP,
    !               33=PY, 34=DG, 35=HT, 36=CH, 37=WI, 38=  , 39=OT

    ! OP/PN COEFFICIENT ORDER
    ! 1=SF, 2=WF/GF,  3=RF,  4=NF,  5=SP/WP,  6=JP/PP,  7=DF,  8=RC,
    ! 9=WH,  10=MH,  11=IC/ES/LL/WB/KP/PY,  12=BM, 13=RA,
    ! 14=MA/TO/GC/AS/CW/J/DG/HT/CH/WI,  15=YC,  16=LP,  17=AF,  18=SS,  19=WO
    ! 20=RW
    ! WC USES THE SAME ORDER WITHOUT SS: 18=WO, 19=RW.

    !  ** OP/PN REFIT CHANGES 060396 DMD.  CHANGES TO MOST DATA ARRAYS FOR
    !     SS,DF,RC,WH.
    !  ** REPLACED OREGON WHITE OAK EQUATION CREATED BY GOULD AND HARRINGTON
    !     FROM PNW STATION DATE 04/19/10 ESM
    !  OP/PN RED FIR EQUATION IS BAD (RESULTS IN 3" TREES 150' TALL). OBTAINED
    !  RED FIR DATA FROM TOMMY GREGG FOR R6, AND PLOTTED HT VS DBH. GROWING
    !  RED FIR WITH THE NOBLE FIR EQUATION MATCHES VERY WELL. 2/15/02 GED.
    !  THE OP/PN/WC COEFFICIENTS FOR JP/PP ARE FROM THE CA VARIANT. THE
    !  EQUATION DEVELOPED FROM THE VERY LIMITED DATA SET AVAILABLE FROM THE
    !  WC AREA DID NOT PERFORM VERY WELL.  GED 1/29/03

    logical :: LCONS
    !  DEFINITIONS OF INTERNAL VARIABLES.

    !    DGFOR -- ARRAY LOADED WITH FOREST SPECIFIC CONSTANTS
    !     DGDS -- ARRAY LOADED WITH DIAMETER SQUARED COEFFICIENTS
    !     DGLD -- ARRAY CONTAINING COEFFICIENTS FOR THE LOG(DIAMETER)
    !             TERM IN THE DDS MODEL.
    !     DGCR -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
    !             RATIO TERM IN THE DDS MODEL.
    !   DGCRSQ -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
    !             RATIO SQUARED TERM IN THE DDS MODEL.
    !    DGBAL -- ARRAY CONTAINING COEFFICIENTS FOR THE BASAL AREA IN
    !             LARGER TREES TERM IN THE DDS MODEL.
    !   DGDBAL -- ARRAY CONTAINING COEFFICIENTS FOR THE INTERACTION
    !             BETWEEN BASAL AREA IN LARGER TREES AND LN(DBH).
    !    DGLBA -- ARRAY OF COEFFICIENTS FOR LOG(BASAL AREA).
    !     DGBA -- ARRAY OF COEFFICIENTS OF BASAL AREA (OP/PN/WC).
    !   DGSITE -- ARRAY OF COEFFICIENTS OF LOG(SITE).
    !     DGEL -- ARRAY OF COEFFICIENTS OF ELEVATION.
    !    DGEL2 -- ELEVATION SQUARED COEFFICIENTS.
    !   DGCASP -- ARRAY OF COEFFICIENTS OF COS(ASPECT)*SLOPE.
    !   DGSASP -- ARRAY OF COEFFICIENTS FOR SIN(ASPECT)*SLOPE.
    !   DGSLOP -- ARRAY OF COEFFICIENTS FOR SLOPE.
    !   DGSLSQ -- ARRAY OF COEFFICIENTS FOR SLOPE**2.
    !   DGPCCF -- ARRAY OF COEFFICIENTS FOR POINT CROWN COMPETITION FACTOR.
    !    DGHAH -- ARRAY OF COEFFICIENTS FOR THE RELATIVE HEIGHT TERM.
    !   MAPSPC -- ARRAY MAPPING EACH SPECIES TO ONE OF THE GROWTH EQUATIONS
    !   MAPLOC -- ARRAY MAPPING THE FOREST AND EQUATION TO THE
    !             LOCATION CONSTANT
    !   MAPDSQ -- ARRAY MAPPING FOREST AND EQUATION TO A DBH**2
    !             COEFFICIENT (OP/PN/WC; CA/OC HAVE ONE PER EQUATION).
    !   OBSERV -- NUMBER OF OBSERVATIONS IN DEVELOPMENT OF THE GROWTH
    !             EQUATION (THIS DATA IS ACTUALLY USED BY **DGDRIV** FOR
    !             CALIBRATION).

    integer, parameter :: IB = merge(1, merge(2, 3, VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'PN'),&
         &VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'OC')
    integer, parameter :: KRWGSB(3) = (/12,20,19/), KWOB(3) = (/0,19,18/)
    integer, parameter :: KRWGS = KRWGSB(IB), KWO = KWOB(IB)

    ! Variables
    integer :: ISPC,I1,I2,JSPC,I3,I,IPCCF,IWHO,JFOR,KSP,IDSQ,J,K
    real    :: CONSPP,D,CR,BAL,RELHT,PBAL,DDS,TDDS,SASP,XSITE,TEMEL
    real    :: PRD,DLO,DHI,SDICS,SDICZ,A,B,XMAX,BARK,BRATIO,DIAGR
    real    :: ZRD(MAXPLT)
    real    :: DGLD(20,3),DGLBA(20,3),DGCR(20,3),DGCRSQ(20,3),DGDBAL(20,3)
    real    :: DGBAL(20,3),DGFOR(6,20,3),DGDS(3,20,3),DGEL(20,3),DGEL2(20,3)
    real    :: DGSASP(20,3),DGCASP(20,3),DGSLOP(20,3),DGSLSQ(20,3)
    real    :: DGBA(20,3),DGSITE(20,3),DGPCCF(20,3),DGHAH(20,3)
    integer :: MAPSPC(50,3),MAPLOC(10,20,3),MAPDSQ(6,20,3),OBSERV(20,3)
    logical :: DEBUG

    !  DGFOR CONTAINS LOCATION CLASS CONSTANTS FOR EACH SPECIES.
    !  MAPLOC IS AN ARRAY WHICH MAPS FOREST ONTO A LOCATION CLASS.
    !  DGDS CONTAINS COEFFICIENTS FOR THE DIAMETER SQUARED TERMS
    !  IN THE DIAMETER INCREMENT MODELS    ARRAYED BY FOREST BY
    !  SPECIES.  DGEL, DGEL2, DGSASP (SIN(ASPECT)*SLOPE), DGCASP
    !  (COS(ASPECT)*SLOPE), DGSLOP AND DGSLSQ ARE SITE TERMS USED BY
    !  **DGCONS**.

    data (MAPSPC(J,1),J=1,50)/&
         &1,  1,  1,  2,  3,  3,  4,  7,  7,  6,&
         &5,  6,  5,  5,  9,  7,  8,  9,  9,  5,&
         &5,  2, 12,  5,  9, 10, 10, 10, 10, 10,&
         &10, 10, 10, 10, 10, 10, 11, 11, 11, 10,&
         &10, 13, 10, 10, 10, 10,  5, 10, 10, 12/
    data (MAPSPC(J,2),J=1,39)/&
         &1,2,2,3, 4,18,4,15,11,11,16,6,5,5,6,7,20,8,9,10,12,&
         &13,14,14,14,14,14,19,14,11,11,11,11,14,14,14,14,14,14/
    data (MAPSPC(J,3),J=1,39)/&
         &1,2,2,3,17,17,4,15,11,11,16,6,5,5,6,7,19,8,9,10,12,&
         &13,14,14,14,14,14,18,14,11,11,11,11,14,14,14,14,14,14/

    data (DGLD(J,1),J=1,13)/&
         &0.950418, 1.182104, 1.186676, 0.716226, 1.077154,&
         &1.218279, 0.886150, 0.825682, 0.738750, 1.310111,&
         &0.955569, 0.0     , 0.99531 /
    data (DGLD(J,2),J=1,20)/&
         &0.919402, 0.905119, 0.993986, 0.904253, 0.844690, 0.738750,&
         &0.802905, 0.744005, 0.641956, 0.857131, 0.879338, 1.024186,&
         &0.511442, 0.889596, 0.816880, 0.478504, 0.949631, 1.049845,&
         &1.66609 , 0.0/
    data (DGLD(J,3),J=1,19)/&
         &0.527758, 0.905119, 0.993986, 0.904253, 0.844690, 0.738750,&
         &0.534138, 0.843013, 0.722462, 0.857131, 0.879338, 1.024186,&
         &0.0, 0.889596, 0.816880, 0.478504, 0.949631, 1.66609,&
         &0.0/

    data (DGCR(J,1),J=1,13)/&
         &1.815305, 2.856578, 2.763519, 3.272451,-0.276387,&
         &3.167164, 1.478650, 1.675208, 3.454857, 0.271183,&
         &0.0     , 0.0     , 2.08524 /
    data (DGCR(J,2),J=1,20)/&
         &1.290568, 1.754811, 1.522401, 4.123101, 1.597250, 3.454857,&
         &1.936912, 0.771395, 1.471926, 1.505513, 1.970052, 0.459387,&
         &0.623093, 1.732535, 2.471226, 1.905011, 1.826879, 1.632468,&
         &0.0     , 0.0/
    data (DGCR(J,3),J=1,19)/&
         &2.982807, 1.754811, 1.522401, 4.123101, 1.597250, 3.454857,&
         &1.636854, 2.878032, 2.160348, 1.505513, 1.970052, 0.459387,&
         &0.0     , 1.732535, 2.471226, 1.905011, 1.826879, 0.0,&
         &0.0/

    data (DGCRSQ(J,1),J=1,13)/&
         &0.0     ,-1.093354,-0.871061,-1.642904, 1.063732,&
         &-1.568333, 0.0     , 0.0     ,-1.773805, 0.0     ,&
         &0.0     , 0.0     ,-0.98396 /
    data (DGCRSQ(J,2),J=1,20)/&
         &0.125823, 0.0     , 0.0     ,-2.689340, 0.0     ,-1.773805,&
         &0.0     , 0.0     , 0.0     , 0.0     , 0.0     , 0.0     ,&
         &0.0     , 0.0     , 0.0     , 0.0     , 0.0     , 0.0     ,&
         &0.0     , 0.0/
    data (DGCRSQ(J,3),J=1,19)/&
         &-1.331331, 2*0.0, -2.689340, 0.0, -1.773805, -0.045578,&
         &-1.631418, -0.834196, 10*0.0/

    data (DGSITE(J,1),J=1,13)/&
         &0.820451, 0.365679, 0.492695, 0.759305, 0.0     ,&
         &0.566946, 0.963375, 0.724300, 1.011504, 0.213526,&
         &1.334008, 0.0     , 0.00659 /
    data (DGSITE(J,2),J=1,20)/&
         &0.541881, 0.318254, 0.349888, 0.684939, 0.404010, 1.011504,&
         &0.495162, 0.708166, 0.634098, 0.208040, 0.252853, 1.965888,&
         &0.237269, 0.227307, 0.244694, 0.391327, 0.375175, 0.0     ,&
         &0.14995 , 0.0/
    data (DGSITE(J,3),J=1,19)/&
         &0.534255, 0.318254, 0.349888, 0.684939, 0.404010, 1.011504,&
         &1.020863, 0.139734, 0.380416, 0.208040, 0.252853, 1.965888,&
         &0.0     , 0.227307, 0.244694, 0.391327, 0.375175, 0.14995,&
         &0.0/

    data (DGDBAL(J,1),J=1,13)/&
         &-0.005433,-0.005992,-0.003728,-0.008787, 0.0     ,&
         &0.0     ,-0.006263,-0.002133,-0.013091, 0.0     ,&
         &-0.005893, 0.0     ,-0.00147 /
    data (DGDBAL(J,2),J=1,20)/&
         &-0.002133, -0.005355, -0.002979, -0.006368, -0.003726, -0.013091,&
         &-0.001827, -0.016240, -0.012589, -0.004101, -0.004215, -0.010222,&
         &-0.027074, -0.001265, -0.005950, -0.004706, -0.005350, -0.000086,&
         &0.0      ,  0.0/
    data (DGDBAL(J,3),J=1,19)/&
         &-0.011247, -0.005355, -0.002979, -0.006368, -0.003726, -0.013091,&
         &-0.009363, -0.003923, -0.004065, -0.004101, -0.004215, -0.010222,&
         &0.0     , -0.001265, -0.005950, -0.004706, -0.005350,  0.0,&
         &0.0/

    data (DGLBA(J,1),J=1,13)/&
         &-0.000016,-0.058039,-0.122905,-0.028564, 0.0     ,&
         &-0.267873,-0.129146,-0.203636,-0.131185, 0.0     ,&
         &-0.408462, 0.0     , 0.0     /
    data (DGLBA(J,2),J=1,20)/&
         &-0.136818,  0.0     ,  0.0     ,  0.0     ,  0.0     , -0.131185,&
         &-0.129474, -0.130036, -0.085525,  0.0     ,  0.0     ,  0.0     ,&
         &-0.481983,  0.0     ,  0.0     ,  0.0     ,  0.0     , -0.198636,&
         &0.0      ,  0.0/
    data (DGLBA(J,3),J=1,19)/&
         &-0.030730, 4*0.0, -0.131185, 13*0.0/

    data (DGBA(J,2),J=1,20)/&
         &0.0     ,  0.0     , -0.000137,  0.0     ,  0.0     ,  0.0     ,&
         &0.0     ,  0.0     ,  0.0     ,  0.0     , -0.000173,  0.0     ,&
         &0.0     , -0.000981, -0.000147, -0.000114,  0.000040,  0.0     ,&
         &-0.00204 ,  0.0/
    data (DGBA(J,3),J=1,19)/&
         &2*0.0, -0.000137, 3*0.0, -0.000215, 3*0.0, -0.000173,&
         &2*0.0, -0.000981, -0.000147, -0.000114, 0.000040,-0.00204,&
         &0.0/

    data (DGBAL(J,1),J=1,13)/&
         &0.0     , 0.0     , 0.0     , 0.0     ,-0.000893,&
         &0.0     , 0.0     , 0.0     , 0.0     , 0.0     ,&
         &0.0     , 0.0     , 0.0     /
    data (DGBAL(J,2),J=1,20)/&
         &0.0     ,  0.0     ,  0.0     ,  0.0     ,  0.0     ,  0.0     ,&
         &-0.001639,  0.003883,  0.002385,  0.0     ,  0.0     ,  0.0     ,&
         &0.008903,  0.0     ,  0.0     ,  0.0     ,  0.0     , -0.002319,&
         &-0.00326 ,  0.0/
    data (DGBAL(J,3),J=1,19)/&
         &0.002839, 16*0.0, -0.00326, 0.0/

    data (DGPCCF(J,1),J=1,13)/&
         &-0.000779,-0.001014, 0.0     ,-0.000224, 0.0     ,&
         &-0.000338, 0.0     , 0.0     ,-0.000593,-0.000473,&
         &0.0     , 0.0     ,-0.00018 /
    data (DGPCCF(J,2),J=1,20)/&
         &0.0     ,  0.0     ,  0.0     , -0.000471, -0.000257, -0.000593,&
         &0.0     ,  0.0     ,  0.0     , -0.000201,  0.0     , -0.000757,&
         &0.0     ,  0.0     ,  0.0     ,  0.0     ,  0.0     ,  0.0     ,&
         &0.0     ,  0.0/
    data (DGPCCF(J,3),J=1,19)/&
         &3*0.0, -0.000471, -0.000257, -0.000593, 0.0, -0.000552,&
         &0.0, -0.000201, 0.0, -0.000757, 7*0.0/

    data (DGHAH(J,1),J=1,13)/&
         &0.0     , 0.0     , 0.0     , 0.0     , 0.0     ,&
         &0.0     , 0.0     , 0.0     , 0.0     , 0.0     ,&
         &0.0     , 0.0     , 0.50155 /
    data (DGHAH(J,2),J=1,20)/&
         &0.0     , -0.000661,  0.0     ,  0.0     ,  0.0     ,  0.0     ,&
         &0.0     ,  0.0     ,  0.0     ,  0.0     ,  0.0     ,  0.0     ,&
         &0.0     ,  0.0     ,  0.0     ,  0.0     ,  0.0     ,  0.0     ,&
         &0.0     ,  0.0/
    data (DGHAH(J,3),J=1,19)/&
         &0.0, -0.000661, 6*0.0, -0.000358, 10*0.0/

    data (OBSERV(J,1),J=1,13)/&
         &613, 3759, 2062, 5400,   84,&
         &372,  561,  253, 2482,  306,&
         &336, 8928, 6504/
    data (OBSERV(J,2),J=1,20)/&
         &622, 1487, 747, 1467,  596, 2482, 11563, 1192, 4293,&
         &2848,  475,  78, 1369,  220,  112,   759,  542,  502,&
         &2144, 8928/
    data (OBSERV(J,3),J=1,19)/&
         &3664, 1487, 747, 1467, 596, 2482, 14999, 1309,&
         &4836, 2848, 475,   78, 125,  220,     0,  759, 542, 2144,&
         &8928/

    data ((MAPLOC(K,J,1),K=1,10),J=1,13)/&
         &1,1,1,1,2,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,2,1,1,1,1,1,1,&
         &1,2,2,1,3,4,5,4,4,5,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,2,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1/
    data ((MAPLOC(K,J,2),K=1,6),J=1,20)/&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,2,1,2,2,2,&
         &1,2,1,2,2,2,&
         &1,1,1,1,1,1,&
         &1,2,2,2,2,2,&
         &1,2,1,2,2,2,&
         &1,2,1,2,2,2,&
         &1,1,1,1,1,1,&
         &1,2,1,2,2,2,&
         &1,2,1,2,2,2,&
         &1,2,3,3,3,3,&
         &1,2,1,2,2,2,&
         &1,2,1,2,2,2,&
         &1,2,1,2,2,2,&
         &1,1,1,1,1,1,&
         &1,2,1,2,2,2,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1/
    data ((MAPLOC(K,J,3),K=1,6),J=1,19)/&
         &1,2,3,4,5,6,&
         &1,1,1,1,1,1,&
         &1,1,1,2,1,1,&
         &1,1,2,2,2,2,&
         &1,1,1,1,1,2,&
         &1,1,1,1,1,1,&
         &1,2,3,4,5,6,&
         &1,1,2,1,1,1,&
         &1,1,2,3,2,1,&
         &1,1,2,2,1,1,&
         &1,1,1,1,1,2,&
         &1,1,1,1,1,2,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,2,&
         &1,1,1,1,1,2,&
         &1,1,1,2,2,2,&
         &1,1,1,1,2,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1/

    data ((DGFOR(K,J,1),K=1,5),J=1,13)/&
         &-3.428338, -3.966547,  0.0     ,  0.0     ,  0.0     ,&
         &-2.108357,  0.0     ,  0.0     ,  0.0     ,  0.0     ,&
         &-2.073942, -1.943608,  0.0     ,  0.0     ,  0.0     ,&
         &-1.877695, -2.099646, -2.211587, -1.955301, -2.078432,&
         &0.564402,  0.0     ,  0.0     ,  0.0     ,  0.0     ,&
         &-2.058828, -1.596998,  0.0     ,  0.0     ,  0.0     ,&
         &-2.397678,  0.0     ,  0.0     ,  0.0     ,  0.0     ,&
         &-1.626879,  0.0     ,  0.0     ,  0.0     ,  0.0     ,&
         &-2.922255,  0.0     ,  0.0     ,  0.0     ,  0.0     ,&
         &-1.958189,  0.0     ,  0.0     ,  0.0     ,  0.0     ,&
         &-3.344700,  0.0     ,  0.0     ,  0.0     ,  0.0     ,&
         &0.0     ,  0.0     ,  0.0     ,  0.0     ,  0.0     ,&
         &-0.94563 ,  0.0     ,  0.0     ,  0.0     ,  0.0     /
    data ((DGFOR(K,J,2),K=1,3),J=1,20)/&
         &-0.627531,  0.0     ,  0.0     ,&
         &-0.643920,  0.0     ,  0.0     ,&
         &-1.888949, -1.276180,  0.0     ,&
         &-1.401865, -1.127977,  0.0     ,&
         &-0.589570, -0.909553,  0.0     ,&
         &-2.922255,  0.0     ,  0.0     ,&
         &-0.739354, -0.199200,  0.0     ,&
         &-0.688250, -0.405590,  0.0     ,&
         &-0.594460, -0.522658,  0.0     ,&
         &-1.052161, -0.793945,  0.0     ,&
         &-1.310067, -1.432659,  0.0     ,&
         &-7.753469, -8.279266,  0.0     ,&
         &4.253807,  3.913250,  3.507520,&
         &-0.107648, -0.098335,  0.0     ,&
         &-1.277664, -1.178041,  0.0     ,&
         &-0.524624, -0.803095,  0.0     ,&
         &-9.211184, -9.800653,  0.0     ,&
         &2.075598,  2.100904,  0.0     ,&
         &-1.33299 ,  0.0     ,  0.0     ,&
         &0.0     ,  0.0     ,  0.0     /
    data ((DGFOR(K,J,3),K=1,6),J=1,19)/&
         &-0.619069, -0.479015, -0.291244, 0.0, -0.420228, -0.746419,&
         &-0.643920, 5*0.0,&
         &-1.888949, -1.276180, 4*0.0,&
         &-1.401865, -1.127977, 4*0.0,&
         &-0.589570, -0.909553, 4*0.0,&
         &-2.922255, 5*0.0,&
         &-2.750874, -2.787499, -2.672664, -2.533437, -2.693964, -2.718852,&
         &0.412763, 0.645645, 4*0.0,&
         &-0.298310, -0.147675, -0.006413, 3*0.0,&
         &-1.052161, -0.793945, 4*0.0,&
         &-1.310067, -1.432659, 4*0.0,&
         &-7.753469, -8.279266, 4*0.0,&
         &6*0.0,&
         &-0.107648, -0.098335, 4*0.0,&
         &-1.277664, -1.178041, 4*0.0,&
         &-0.524624, -0.803095, 4*0.0,&
         &-9.211184, -9.800653, 4*0.0,&
         &-1.33299 , 5*0.0,&
         &6*0.0/

    data ((MAPDSQ(K,J,2),K=1,6),J=1,20)/&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,2,1,2,2,2,&
         &1,2,1,2,2,2,&
         &1,2,1,2,2,2,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,2,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,2,1,2,2,2,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1/
    data ((MAPDSQ(K,J,3),K=1,6),J=1,19)/&
         &1,1,1,2,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1/

    data (DGDS(1,J,1),J=1,13)/&
         &-0.0002385,&
         &-0.0006362,&
         &-0.0004572,&
         &-0.0002723,&
         &0.0      ,&
         &-0.0014178,&
         &-0.0002528,&
         &-0.0000731,&
         &-0.0004708,&
         &-0.0003048,&
         &0.0      ,&
         &0.0      ,&
         &-0.000373 /
    data ((DGDS(K,J,2),K=1,3),J=1,20)/&
         &-0.0002641,  0.0      ,  0.0      ,&
         &-0.0003137,  0.0      ,  0.0      ,&
         &-0.0002621,  0.0      ,  0.0      ,&
         &-0.0003996,  0.0      ,  0.0      ,&
         &-0.0000596,  0.0      ,  0.0      ,&
         &-0.0004708,  0.0      ,  0.0      ,&
         &-0.0000896, -0.0000641,  0.0      ,&
         &-0.0000572, -0.0000862,  0.0      ,&
         &-0.0001736, -0.0001040,  0.0      ,&
         &-0.0002214,  0.0      ,  0.0      ,&
         &-0.0001323,  0.0      ,  0.0      ,&
         &-0.0001737,  0.0      ,  0.0      ,&
         &-0.0005099,  0.0      ,  0.0      ,&
         &0.0      ,  0.0      ,  0.0      ,&
         &-0.0002536,  0.0      ,  0.0      ,&
         &0.0      ,  0.0      ,  0.0      ,&
         &-0.0003552,  0.0      ,  0.0      ,&
         &-0.0002123, -0.0001361,  0.0      ,&
         &-0.00154  ,  0.0      ,  0.0      ,&
         &0.0      ,  0.0      ,  0.0      /
    data ((DGDS(K,J,3),K=1,2),J=1,19)/&
         &-0.0001983, 0.0,&
         &-0.0003137, 0.0,&
         &-0.0002621, 0.0,&
         &-0.0003996, 0.0,&
         &-0.0000596, 0.0,&
         &-0.0004708, 0.0,&
         &-0.0001039, 0.0,&
         &-0.0000644, 0.0,&
         &-0.0001546, 0.0,&
         &-0.0002214, 0.0,&
         &-0.0001323, 0.0,&
         &-0.0001737, 0.0,&
         &2*0.0,&
         ! + DSQ TERM CAUSING BLOWUPS, SET TO 0 FOR NOW. DIXON 4-29-93.
         !    &  0.0001809, 0.0,
         &2*0.0,&
         &-0.0002536, 0.0,&
         &2*0.0,&
         &-0.0003552, 0.0,&
         &-0.00154  , 0.0,&
         &0.0      , 0.0/

    data (DGCASP(J,1),J=1,13)/&
         &0.0     ,-0.315227,-0.444594,-0.151727, 0.649870,&
         &0.0     ,-0.280294,-0.179510, 0.0     , 0.0     ,&
         &0.0     , 0.0     ,-0.19935 /
    data (DGCASP(J,2),J=1,20)/&
         &-0.217205,  0.0     , -0.782418, -0.374512,  0.0     ,  0.0     ,&
         &0.014165, -0.106936, -0.056608, -0.104495,  0.0     ,  0.0     ,&
         &0.022254,  0.085958, -0.023186,  0.207853, -0.935870, -0.221095,&
         &0.0     ,  0.0/
    data (DGCASP(J,3),J=1,19)/&
         &2*0.0, -0.782418, -0.374512, 0.0, 0.0, -0.080943, 2*0.0,&
         &-0.104495, 3*0.0, 0.085958, -0.023186, 0.207853, -0.935870,&
         &0.0, 0.0/

    data (DGSASP(J,1),J=1,13)/&
         &0.0     , 0.097350, 0.139180, 0.018681, 0.951834,&
         &0.0     ,-0.014463,-0.562259, 0.0     , 0.0     ,&
         &0.0     , 0.0     ,-0.03587 /
    data (DGSASP(J,2),J=1,20)/&
         &0.096326,  0.0     ,  0.022160, -0.207659,  0.0     ,  0.0     ,&
         &0.003263, -0.106020,  0.061254, -0.126130,  0.0     ,  0.0     ,&
         &-0.085538, -0.863980,  0.679903,  0.378860,  0.202507,  0.100081,&
         &0.0     ,  0.0/
    data (DGSASP(J,3),J=1,19)/&
         &2*0.0, 0.022160, -0.207659, 0.0, 0.0, -0.038992, 2*0.0,&
         &-0.126130, 3*0.0, -0.863980, 0.679903, 0.378860, 0.202507,&
         &0.0, 0.0/

    data (DGSLOP(J,1),J=1,13)/&
         &0.0     ,-0.206267, 0.0     ,-0.339369, 0.0     ,&
         &0.0     ,-0.581722,-0.544867, 0.0     , 0.0     ,&
         &0.0     , 0.0     , 0.73530 /
    data (DGSLOP(J,2),J=1,20)/&
         &-0.265612,  0.0     ,  0.319956,  0.400223,  0.0     ,  0.0     ,&
         &-0.340401, -0.303490,  0.736143,  0.411602,  0.0     ,  0.0     ,&
         &0.0     ,  0.0     ,  0.0     , -0.066440,  0.0     , -0.169141,&
         &0.0     ,  0.0/
    data (DGSLOP(J,3),J=1,19)/&
         &0.245548, 0.0, 0.319956, 0.400223, 0.0, 0.0, 0.077787,&
         &0.0, 0.421486, 0.411602, 5*0.0, -0.066440, 0.0, 0.0,&
         &0.0/

    data (DGSLSQ(J,1),J=1,13)/&
         &0.0     , 0.0     , 0.0     , 0.0     , 0.0     ,&
         &0.0     , 0.0     , 0.0     , 0.0     , 0.0     ,&
         &0.0     , 0.0     ,-0.99561 /
    data (DGSLSQ(J,2),J=1,20)/&
         &0.0     ,  0.0     ,  0.0     ,  0.0     ,  0.0     ,  0.0     ,&
         &0.0     ,  0.0     , -1.082191,  0.0     ,  0.0     ,  0.0     ,&
         &0.0     ,  0.0     ,  0.0     ,  0.0     ,  0.0     ,  0.0     ,&
         &0.0     ,  0.0/
    data (DGSLSQ(J,3),J=1,19)/&
         &5*0.0, 0.0, -0.215778, 0.0, -0.693610, 10*0.0/

    data (DGEL(J,1),J=1,13)/&
         &0.0     , 0.0301  , 0.0248,-0.0141  , 0.0     ,&
         &0.0     , 0.0     , 0.0   ,-0.003784, 0.0049  ,&
         &0.0     , 0.0     , 0.0     /
    data (DGEL(J,2),J=1,20)/&
         &-0.023858, -0.003051, -0.003773, -0.069045, -0.023376, -0.003784,&
         &-0.009845, -0.009564, -0.018444, -0.003809,  0.0     , -0.012111,&
         &0.0     , -0.075986,  0.0     , -0.005414,  0.323546,  0.007009,&
         &0.0     ,  0.0/
    data (DGEL(J,3),J=1,19)/&
         &-0.048852, -0.003051, -0.003773, -0.069045, -0.023376,-0.003784,&
         &-0.037591, -0.050081, -0.040067, -0.003809, 0.0, -0.012111,&
         &0.0,      -0.075986,  0.0,      -0.005414, 0.323546, 0.0,&
         &0.0/

    data (DGEL2(J,1),J=1,13)/&
         &0.0     ,-0.00030732,-0.00033429, 0.00024083, 0.0       ,&
         &0.0     , 0.0       , 0.0       , 0.00006660,-0.00008781,&
         &0.0     , 0.0       , 0.0     /
    data (DGEL2(J,2),J=1,20)/&
         &0.0     ,  0.0     ,  0.0     ,  0.000608,  0.0     , 0.0000666,&
         &0.0     ,  0.0     ,  0.0     ,  0.0     ,  0.0     ,  0.0     ,&
         &0.0     ,  0.001193,  0.0     ,  0.0     , -0.003130,  0.0     ,&
         &0.0     ,  0.0/
    data (DGEL2(J,3),J=1,19)/&
         &0.000478, 2*0.0, 0.000608, 0.0, 0.00006660, 0.000549, 0.000660,&
         &0.000395, 4*0.0, 0.001193, 2*0.0, -0.003130, 0.0, 0.0/


    if (LCONS) go to 1000

    !  CHECK FOR DEBUG.

    call DBCHK (DEBUG,'DGF',3,ICYC)
    if (DEBUG) write(JOSTND,3)ICYC
    3 format(' ENTERING SUBROUTINE DGF  CYCLE =',I5)

    !  DEBUG OUTPUT: MODEL COEFFICIENTS.

    if (DEBUG) write(JOSTND,*) 'IN DGF,HTCON=',HTCON,&
         &'ELEV=',ELEV,'RELDEN=',RELDEN
    if (DEBUG)&
         &write(JOSTND,9000) DGCON,DGDSQ
    9000 format(/11(1X,F10.5))

    !  CALL SDICAL TO LOAD THE POINT MAX SDI WEIGHTED BY SPECIES ARRAY XMAXPT

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
       if (DEBUG)write(JOSTND,*) 'IN DGF, POINT ',I1,' SDICZ= ',SDICZ
       ZRD(I1) = SDICZ
    end do

    !  BEGIN SPECIES LOOP.  ASSIGN VARIABLES WHICH ARE SPECIES
    !  DEPENDENT

    do ISPC = 1,MAXSP
       I1 = ISCT(ISPC,1)
       if (I1.eq.0) go to 20
       I2 = ISCT(ISPC,2)
       JSPC = MAPSPC(ISPC,IB)
       KSP = JSPC
       if (IB.eq.1) KSP = ISPC

       !  DEFINE CONSPP. RW AND GS DO NOT GET COR ADJUSTMENT UNTIL AFTER
       !  DIAMETER GROWTH PREDICTION.
       if (JSPC.eq.KRWGS) then
          CONSPP = DGCON(ISPC)
       else
          CONSPP = DGCON(ISPC) + COR(ISPC)
       end if

       !  BEGIN TREE LOOP WITHIN SPECIES ISPC.

       do I3 = I1,I2
          I = IND1(I3)
          D = DIAM(I)
          if (D.le.0.0) goto 10

          !  OP/PN/WC RED ALDER USES A DIFFERENT EQUATION AND SKIPS THE
          !  TREE VARIABLES BELOW.

          if (IB.ne.1.and.JSPC.eq.13) then
             BARK = BRATIO(22,D,HT(I))
             call DGFRA(ISPC,D,BARK,DIAGR,DDS)
             go to 5
          end if

          CR = ICR(I)*0.01
          BAL = (1.0 - (PCT(I)/100.)) * BA
          IPCCF = ITRE(I)
          RELHT = 0.0
          if (AVH .gt.0.0) RELHT = HT(I)/AVH
          if (RELHT .gt.1.5)RELHT = 1.5

          !  RELATIVE DENSITY (ZEIDI) ON THE POINT
          !  CONSTRAIN RD IF NEEDED
          if (XMAXPT(ITRE(I)).le.0.0) then
             PRD = 0.0
          else
             PRD = ZRD(ITRE(I)) / XMAXPT(ITRE(I))
          end if

          !  GIANT SEQUOIA AND COASTAL REDWOOD USE DIFFERENT FUNCTIONAL FORM

          if (JSPC.eq.KRWGS) then
             call DGFRWGS(I,ISPC,D,CR,BAL,PRD,CONSPP,DEBUG,PBAL,DDS)
          else

             !  THIS FUNCTION OCCASIONALLY GIVES UNDERFLOW ERROR ON PC. SPLITTING
             !  IT UP INTO TWO PARTS IS A TEMPORARY SOLUTION WHICH WORKS. GD 2/20/97

             DDS = CONSPP + DGLD(JSPC,IB)*ALOG(D)&
                  &+ CR*(DGCR(JSPC,IB) + CR*DGCRSQ(JSPC,IB))&
                  &+ DGDSQ(KSP)*D*D  + DGDBAL(JSPC,IB)*BAL/(ALOG(D+1.0))
             if (IB.eq.1) then
                DDS = DDS + DGPCCF(JSPC,IB)*PCCF(IPCCF) + DGHAH(JSPC,IB)*RELHT&
                     &+ DGLBA(JSPC,IB)*ALOG(BA) + DGBAL(JSPC,IB)*BAL
             else
                DDS = DDS + DGPCCF(JSPC,IB)*PCCF(IPCCF) + DGHAH(JSPC,IB)*RELHT&
                     &+ DGLBA(JSPC,IB)*ALOG(BA)&
                     &+ DGBAL(JSPC,IB)*BAL + DGBA(JSPC,IB)*BA
             end if
          end if

          !  CA/OC TANOAK EQN IS A 5-YR ESTIMATE, ADJUST TO 10-YR BASIS.

          if (IB.eq.1.and.ISPC .eq.42) then
             TDDS = EXP(DDS)
             DDS = ALOG(TDDS*2.)
          end if

          5 continue
          if (DEBUG) write(JOSTND,8000)&
               &I,ISPC,CONSPP,D,BA,CR,BAL,PCCF(IPCCF),RELDEN,HT(I),AVH
          8000 format(1H0,'IN DGF 8000F',2I5,9F11.4)

          ! OC/OP EQUATIONS ARE PREDICTING 10-YEAR DIAMETER GROWTH, CONVERT
          ! THIS TO A 5-YR ESTIMATE IN REAL SCALE AND THEN LOAD INTO WK2

          if (VARIANTCD.eq.'OC'.or.VARIANTCD.eq.'OP') then
             TDDS = EXP(DDS)
             DDS = ALOG(TDDS/2.0)
          end if
          if (DDS.lt.-9.21) DDS = -9.21
          WK2(I) = DDS

          !  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.

          if (DEBUG) then
             write(JOSTND,9001) I,ISPC,DDS
             9001 format(' IN DGF, I=',I4,',  ISPC=',I3,',  LN(DDS)=',F7.4)
          end if
          10 continue
       end do

       !  END OF SPECIES LOOP.

       20 continue
    end do
    if (DEBUG) write(JOSTND,100)ICYC
    100 format(' LEAVING SUBROUTINE DGF  CYCLE =',I5)
    return

    !  ENTRY **DGCONS**
    1000 continue

    !  ENTRY POINT FOR LOADING COEFFICIENTS OF THE DIAMETER INCREMENT
    !  MODEL THAT ARE SITE SPECIFIC AND NEED ONLY BE RESOLVED ONCE.
    !  IDTYPE IS A HABITAT TYPE INDEX THAT IS COMPUTED IN **RCON**.
    !  ASPECT IS STAND ASPECT.

    !  CHECK FOR DEBUG.

    call DBCHK (DEBUG,'DGF',3,ICYC)

    !  WC ALIGNS BLM ADMINISTRATIVE UNIT WITH CLOSEST NF:
    !  SALEM -- MT HOOD       EUGENE -- WILLAMETTE
    !  ROSEBURG -- UMPQUA     MEDFORD -- ROGUE RIVER

    JFOR = IFOR
    if (IB.eq.3) then
       if (IFOR .le.6) then
          JFOR = IFOR
       else if (IFOR .eq.7) then
          JFOR = 3
       else if (IFOR .eq.8) then
          JFOR = 6
       else if (IFOR .eq.9) then
          JFOR = 5
       else if (IFOR .eq.10) then
          JFOR = 4
       else
          JFOR = 3
       end if
    end if

    !  ENTER LOOP TO LOAD SPECIES DEPENDENT VECTORS.
    !  OP/PN/WC: CONSTRAIN ELEVATION TERM FOR MODEL 14 TO BE LE 30;
    !  WO USES KINGS SI FOR DF IN DDS EQUATION, SO HAS TO BE TRANSLATED

    do ISPC = 1,MAXSP
       JSPC = MAPSPC(ISPC,IB)
       KSP = JSPC
       if (IB.eq.1) KSP = ISPC
       ISPFOR = MAPLOC(JFOR,JSPC,IB)
       IDSQ = 1
       if (IB.ne.1) then
          ISPDSQ = MAPDSQ(JFOR,JSPC,IB)
          IDSQ = ISPDSQ
       end if
       SASP = &
            &+(DGSASP(JSPC,IB) * SIN(ASPECT)&
            &+ DGCASP(JSPC,IB) * COS(ASPECT)&
            &+ DGSLOP(JSPC,IB)) * SLOPE&
            &+ DGSLSQ(JSPC,IB) * SLOPE * SLOPE
       XSITE = SITEAR(ISPC)
       TEMEL = ELEV
       if (IB.ne.1) then
          if (JSPC.eq.10)XSITE = XSITE*3.281
          if (JSPC.eq.KWO)XSITE = -37.60812*ALOG(1-(XSITE/114.24569)**.4444)
          if (JSPC.eq.14.and. TEMEL.gt.30.)TEMEL = 30.
       end if

       !  PROCESS RW AND GS AND THEN ALL OTHER SPECIES

       if (JSPC.eq.KRWGS) then
          DGCON(ISPC) = -3.502444 + 0.415435*LOG(SITEAR(ISPC))
          DGDSQ(KSP) = 0.
       else
          DGCON(ISPC) = &
               &DGFOR(ISPFOR,JSPC,IB)&
               &+ DGEL(JSPC,IB) * TEMEL&
               &+ DGEL2(JSPC,IB) * TEMEL * TEMEL&
               &+ DGSITE(JSPC,IB)*ALOG(XSITE)&
               &+ SASP
          DGDSQ(KSP) = DGDS(IDSQ,JSPC,IB)
       end if
       ATTEN(KSP) = OBSERV(JSPC,IB)
       SMCON(ISPC) = 0.

       if (DEBUG) then
          if (IB.eq.1) then
             write(JOSTND,9030)DGFOR(ISPFOR,JSPC,IB),&
                  &DGEL(JSPC,IB),ELEV,DGSASP(JSPC,IB),ASPECT,&
                  &DGCASP(JSPC,IB),DGSLOP(JSPC,IB),SLOPE,DGSITE(JSPC,IB),&
                  &XSITE,DGCON(ISPC),SASP
          else if (IB.eq.2) then
             write(JOSTND,9030)DGFOR(ISPFOR,JSPC,IB),&
                  &DGEL(JSPC,IB),ELEV,DGEL2(JSPC,IB),DGSASP(JSPC,IB),ASPECT,&
                  &DGCASP(JSPC,IB),DGSLOP(JSPC,IB),SLOPE,DGSITE(JSPC,IB),&
                  &SITEAR(ISPC),DGCON(ISPC),SASP,XSITE
          else
             write(JOSTND,9031)DGFOR(ISPFOR,JSPC,IB),&
                  &DGEL(JSPC,IB),ELEV,DGEL2(JSPC,IB),DGSASP(JSPC,IB),ASPECT,&
                  &DGCASP(JSPC,IB),DGSLOP(JSPC,IB),SLOPE,DGSITE(JSPC,IB),&
                  &SITEAR(ISPC),DGCON(ISPC),SASP,XSITE
          end if
       end if
       9030 format(' IN DGF 9030',13F9.5)
       9031 format(' IN DGF 9030',14F9.5)

       !  IF READCORD OR REUSCORD WAS SPECIFIED (LDCOR2 IS TRUE) ADD
       !  LN(COR2) TO THE BAI MODEL CONSTANT TERM (DGCON).  COR2 IS
       !  INITIALIZED TO 1.0 IN BLKDATA.
       !  RW AND GS DO NOT HAVE COR2 APPLIED IN THIS CODE SEGMENT
       !  SINCE DDS IS NOT DIRECTLY CALCULATED FOR THESE SPECIES.

       if (LDCOR2.and.COR2(ISPC).gt.0.0) then
          if (JSPC.ne.KRWGS) DGCON(ISPC) = DGCON(ISPC) + ALOG(COR2(ISPC))
       end if
    end do
    return
  end subroutine DGFPAC

  subroutine DGFTU(LCONS)
    implicit none

    !  TETONS (TT) AND UTAH (UT).  COEFFICIENT BANK IB: 1 = TT (18 SPECIES),
    !  2 = UT (24).  DDS IS PREDICTED FROM SITE, LOCATION, SLOPE, ASPECT,
    !  ELEVATION, DBH, CROWN RATIO, BASAL AREA IN LARGER TREES AND CCF.
    !  KGR GROUPS THE NATIVE SPECIES BY EQUATION:
    !    1 = TT/UT CONIFERS (UT ADDS POINT CCF; UT SPECIES 5 x 0.95)
    !    2 = TT PONDEROSA PINE (POINT BAL, LN(BA) IN THE CONSTANT, HABITAT)
    !    3 = ASPEN (**DGFASP**)
    !    4 = JUNIPER/PINYON; EQNS IN **REGENT** ARE USED FOR ALL SIZED
    !        TREES FOR PM, UJ, RM, BI, AND MC.  THIS CODE IS LEFT AS A
    !        PLACEHOLDER IN CASE WE WANT TO CHANGE THIS IN THE FUTURE.
    !    5 = MIXED HARDWOODS
    !    6 = SURROGATE EQUATIONS FROM THE CR VARIANT, WITH THE SPRUCE-FIR
    !        MODEL TYPE LOWER BA LIMIT AND STAGNATION (UT 17 USES THE
    !        JUNIPER FORM)
    !  ONLY UT RESETS DGCCF(8) FOR HIGH SITE CLASSES DURING **DGF**; ONLY
    !  TT SELECTS DGCCF BY SITE CLASS (IGCCFM/DGCCFC) IN **DGCONS**.

    !     DGLD -- COEFFICIENTS FOR THE LOG(DIAMETER) TERM.
    !     DGCR -- COEFFICIENTS FOR THE CROWN RATIO TERM.
    !   DGCRSQ -- COEFFICIENTS FOR THE CROWN RATIO SQUARED TERM.
    !    DGBAL -- COEFFICIENTS FOR THE BASAL AREA IN LARGER TREES TERM.
    !    DGCCF -- CROWN COMPETITION FACTOR COEFFICIENTS (COEFFS), LOADED
    !             IN **DGCONS**.

    logical :: LCONS
    integer, parameter :: IB = merge(1, 2, VARIANTCD.eq.'TT')
    integer, parameter :: KHWEL = merge(16, 20, VARIANTCD.eq.'TT')
    logical :: DEBUG
    integer :: ICLS,MAPHAB,II,ISI,ISC,ISPC,I1,I2,KSI,I3,I,LSI,ISIC,ISIC2,J,K
    real    :: BA100,CONSPP,DGLDS,DGBALS,DGCRS,DGCRS2,DGDSQS,SI,D,CR
    real    :: ASPDG,BAL,DDS,XSITE,YOUNG,OLD,SUMD2,SUMTRE,DPROB,DGDBLS
    real    :: DGQMD,STDSDI,RELSDI,ALD,DGLCCF,DGPTCC,BARK,BRATIO,DPP,BATEM
    real    :: DF,DIAGR,BAUTBA,ASPTEM,TEMEL,TEMBA,PBAL,DGFCOB,DGFJUN
    integer :: KGR(24,2),ISMAP(24,2),IBSERV(5,24,2),MAPLOC(6,24,2)
    integer :: MAPDSQ(6,24,2),IDGSIM(7,24,2)
    integer :: IGCCFM(5,18),ICHBCL(363)
    real    :: DGLD(24,2),DGCR(24,2),DGCRSQ(24,2),DGBAL(24,2),DGPCCF(24,2)
    real    :: DGBA(24,2),DGDBAL(24,2),OBSERV(24,2),DGFOR(5,24,2)
    real    :: DGDS(4,24,2),DGCASP(24,2),DGSASP(24,2),DGSLOP(24,2)
    real    :: DGSLSQ(24,2),DGEL(24,2),DGEL2(24,2),DGSIC(5,24,2)
    real    :: DGCCFC(5,18),DGHAB(7),DGCCFA(24)

    !          TT: 1-3,5,7-9,17 CONIFERS; 4,11,12 JUNIPER; 6,14 ASPEN;
    !              10 PP; 13,16 MIXED HARDWOODS; 15,18 CR SURROGATES.
    data (KGR(J,1),J=1,18)/1,1,1,4,1,3,1,1,1,2,4,4,5,3,6,5,1,6/
    !          UT: 1-5,7-10,23 CONIFERS; 6 ASPEN; 11-16,24 JUNIPER;
    !              20,21 MIXED HARDWOODS; 17-19,22 CR SURROGATES.
    data (KGR(J,2),J=1,24)/1,1,1,1,1,3,1,1,1,1,4,4,4,4,4,4,6,6,6,5,5,6,1,4/

    data (DGLD(J,1),J=1,18)/&
         &0.213947,  0.213947,  0.533965,       0.0,  0.378802,&
         &-0.368391,  0.563751,  0.378802,  0.648535,  0.822203,&
         &0.0,       0.0,  1.024186,       0.0,       0.0,&
         &0.889596,  0.213947,       0.0/
    data (DGLD(J,2),J=1,24)/&
         &0.213947,  0.213947,  0.479631,  0.827167, 0.587579,&
         &0.0,  0.587503,  0.587579,  0.833096, 0.733305,&
         &0.0,       0.0,       0.0,       0.0,      0.0,&
         &0.0,       0.0,       0.0,       0.0, 0.889596,&
         &1.024186,       0.0,  0.213947,       0.0/

    data (DGCR(J,1),J=1,18)/&
         &1.523464,  1.523464,  1.931900,       0.0,  1.098353,&
         &4.034753,  2.164346,  1.098353,  0.137638,  1.768935,&
         &0.0,       0.0,  0.459387,       0.0,       0.0,&
         &1.732535,  1.523464,       0.0/
    data (DGCR(J,2),J=1,24)/&
         &1.523464,  1.523464,  3.182592, -0.207507, 0.331129,&
         &0.0,  2.148640,  0.331129,  1.422919, 1.315804,&
         &0.0,       0.0,       0.0,       0.0,      0.0,&
         &0.0,       0.0,       0.0,       0.0, 1.732535,&
         &0.459387,       0.0,  1.523464,       0.0/

    data (DGCRSQ(J,1),J=1,18)/&
         &0.0,       0.0, -0.894368,       0.0,       0.0,&
         &-5.617552, -0.625799,       0.0, 1.0665429, -0.176164,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0/
    data (DGCRSQ(J,2),J=1,24)/&
         &0.0,       0.0, -1.310144,  1.578941, 0.816301,&
         &0.0, -0.598897,  0.816301,  0.225676, 0.238917,&
         &0.0,       0.0,       0.0,       0.0,      0.0,&
         &0.0,       0.0,       0.0,       0.0,      0.0,&
         &0.0,       0.0,       0.0,       0.0/

    data (DGBAL(J,1),J=1,18)/&
         &-0.358634, -0.358634, -0.574858,       0.0, -0.490005,&
         &-0.704392, -0.469671, -0.490005, -0.312129,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0, -0.358634,       0.0/
    data (DGBAL(J,2),J=1,24)/&
         &-0.358634, -0.358634, -0.707380, -0.010478, -0.399357,&
         &0.0, -0.192073, -0.399357, -0.182808, -0.320124,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0, -0.358634,       0.0/

    data (DGPCCF(J,1),J=1,18)/&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0, -0.000757,       0.0,       0.0,&
         &0.0,       0.0,       0.0/
    data (DGPCCF(J,2),J=1,24)/&
         &0.0,       0.0, -0.001613, -0.000428,      0.0,&
         &0.0,  -.000467,       0.0,  -.000200, -.002576,&
         &0.0,       0.0,       0.0,       0.0,      0.0,&
         &0.0,       0.0,       0.0,       0.0,      0.0,&
         &-0.000757,       0.0,       0.0,       0.0/

    data (DGBA(J,1),J=1,18)/&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &-0.000981,       0.0,       0.0/
    data (DGBA(J,2),J=1,24)/&
         &0.0,       0.0,       0.0,       0.0,      0.0,&
         &0.0,       0.0,       0.0,       0.0,      0.0,&
         &0.0,       0.0,       0.0,       0.0,      0.0,&
         &0.0,       0.0,       0.0,       0.0,-0.000981,&
         &0.0,       0.0,       0.0,       0.0/

    data (DGDBAL(J,1),J=1,18)/&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0, -0.006065,&
         &0.0,       0.0, -0.010222,       0.0,       0.0,&
         &-0.001265,       0.0,       0.0/
    data (DGDBAL(J,2),J=1,24)/&
         &0.0,       0.0,       0.0,       0.0,      0.0,&
         &0.0,       0.0,       0.0,       0.0,      0.0,&
         &0.0,       0.0,       0.0,       0.0,      0.0,&
         &0.0,       0.0,       0.0,       0.0,-0.001265,&
         &-0.010222,       0.0,       0.0,       0.0/

    data (ISMAP(J,1),J=1,18)/1, 1, 2, 1, 3, 5, 1, 3, 3, 1,&
         &1, 1, 1, 1, 1, 1, 1, 1/
    data (ISMAP(J,2),J=1,24)/1, 1, 2, 6, 4, 5, 1, 4, 3, 7,&
         &1, 1, 1, 1, 1, 1, 1, 1, 1, 1,&
         &1, 1, 7, 1/

    data ((IBSERV(K,J,1),K=1,5),J=1,18)/&
         &27,  70, 123, 101,  33,&
         &27,  70, 123, 101,  33,&
         &2, 125, 387,1008,1075,&
         &5*1000,&
         &3, 111, 242, 281, 334,&
         &184, 429, 356, 162,  74,&
         &0, 112,1023,1208, 295,&
         &3, 111, 242, 281, 334,&
         &1, 140, 798, 819, 530,&
         &5*0,&
         &5*1000,&
         &5*1000,&
         &5*0,&
         &184, 429, 356, 162,  74,&
         &5*1000,&
         &5*0,&
         &27,  70, 123, 101,  33,&
         &5*1000/
    data ((IBSERV(K,J,2),K=1,5),J=1,24)/&
         &27,70,123,101,33,&
         &27,70,123,101,33,&
         &202,541,323,103,42,&
         &283,45,49,24,12,&
         &586,544,272,95,32,&
         &184,429,356,162,74,&
         &865,1023,372,10,1,&
         &586,544,272,95,32,&
         &203,432,397,126,84,&
         &289,241,41,24,5,&
         &5*1000,&
         &5*1000,&
         &5*1000,&
         &5*1000,&
         &5*1000,&
         &5*1000,&
         &5*1000,&
         &5*1000,&
         &5*1000,&
         &5*1000,&
         &5*1000,&
         &5*1000,&
         &27,70,123,101,33,&
         &5*1000/

    data (OBSERV(J,1),J=1,18)/&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,    8420.0,&
         &0.0,       0.0,      78.0,       0.0,       0.0,&
         &220.0,       0.0,       0.0/
    data (OBSERV(J,2),J=1,24)/&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,     220.0,&
         &78.0,       0.0,       0.0,       0.0/

    data ((MAPLOC(K,J,1),K=1,4),J=1,18)/&
         &1,1,2,3,&
         &1,1,2,3,&
         &1,1,2,3,&
         &4*1,&
         &1,1,2,3,&
         &1,1,2,1,&
         &1,1,2,2,&
         &1,1,2,3,&
         &1,1,2,3,&
         &4*1,&
         &4*1,&
         &4*1,&
         &4*1,&
         &4*1,&
         &4*1,&
         &4*1,&
         &1,1,2,3,&
         &4*1/
    data ((MAPLOC(K,J,2),K=1,6),J=1,24)/&
         &6*1,&
         &6*1,&
         &1,2,2,2,3,4,&
         &1,2,3,3,4,4,&
         &1,1,1,2,3,4,&
         &1,1,1,1,1,1,&
         &1,1,1,1,2,3,&
         &1,1,1,2,3,4,&
         &1,2,1,1,1,3,&
         &1,2,2,3,3,3,&
         &6*1,&
         &6*1,&
         &6*1,&
         &6*1,&
         &6*1,&
         &6*1,&
         &6*1,&
         &6*1,&
         &6*1,&
         &6*1,&
         &6*1,&
         &6*1,&
         &6*1,&
         &6*1/

    data ((DGFOR(K,J,1),K=1,5),J=1,18)/&
         &1.911884, 1.568742, 2.001195,      0.0,     0.0,&
         &1.911884, 1.568742, 2.001195,      0.0,     0.0,&
         &1.084994, 0.796640, 1.042871,      0.0,     0.0,&
         &5*0.0,&
         &1.543251, 0.943003, 0.792165,      0.0,     0.0,&
         &1.543622, 1.643733,      0.0,      0.0,     0.0,&
         &0.494205, 0.502908,      0.0,      0.0,     0.0,&
         &1.543251, 0.943003, 0.792165,      0.0,     0.0,&
         &0.921658, 0.807282, 0.914279,      0.0,     0.0,&
         &1.879603,     4*0.,&
         &5*0.0,&
         &5*0.0,&
         &-7.753469,      0.0,      0.0,      0.0,     0.0,&
         &5*0.0,&
         &5*0.0,&
         &-0.107648,      0.0,      0.0,      0.0,     0.0,&
         &1.911884, 1.568742, 2.001195,      0.0,     0.0,&
         &5*0.0/
    data ((DGFOR(K,J,2),K=1,5),J=1,24)/&
         &1.911884,       0.0,       0.0,      0.0,      0.0,&
         &1.911884,       0.0,       0.0,      0.0,      0.0,&
         &0.192136, -0.064516,  0.477698, 0.589169,      0.0,&
         &-0.061856, -0.130667, -0.314746, 0.421806,      0.0,&
         &0.011943,  0.265071, -0.094861, 0.796948,      0.0,&
         &0.0,       0.0,       0.0,      0.0,      0.0,&
         &-0.256987, -0.425846,  0.530457,      0.0,      0.0,&
         &0.011943,  0.265071, -0.094861, 0.796948,      0.0,&
         &-0.467188, -0.638653,  0.116430,      0.0,      0.0,&
         &-0.13235, -0.460129, -0.302309,      0.0,      0.0,&
         &0.0,       0.0,       0.0,      0.0,      0.0,&
         &0.0,       0.0,       0.0,      0.0,      0.0,&
         &0.0,       0.0,       0.0,      0.0,      0.0,&
         &0.0,       0.0,       0.0,      0.0,      0.0,&
         &0.0,       0.0,       0.0,      0.0,      0.0,&
         &0.0,       0.0,       0.0,      0.0,      0.0,&
         &0.0,       0.0,       0.0,      0.0,      0.0,&
         &0.0,       0.0,       0.0,      0.0,      0.0,&
         &0.0,       0.0,       0.0,      0.0,      0.0,&
         &-0.107648,       0.0,       0.0,      0.0,      0.0,&
         &-7.753469,       0.0,       0.0,      0.0,      0.0,&
         &0.0,       0.0,       0.0,      0.0,      0.0,&
         &1.911884,       0.0,       0.0,      0.0,      0.0,&
         &0.0,       0.0,       0.0,      0.0,      0.0/

    data ((MAPDSQ(K,J,1),K=1,4),J=1,18)/&
         &1,1,1,1,&
         &1,1,1,1,&
         &1,1,1,1,&
         &4*1,&
         &1,1,1,1,&
         &1,1,1,1,&
         &1,1,2,3,&
         &1,1,1,1,&
         &1,1,1,2,&
         &4*1,&
         &4*1,&
         &4*1,&
         &4*1,&
         &4*1,&
         &4*1,&
         &4*1,&
         &1,1,1,1,&
         &4*1/
    data ((MAPDSQ(K,J,2),K=1,6),J=1,24)/&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,2,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1,&
         &1,1,1,1,1,1/

    data ((DGDS(K,J,1),K=1,4),J=1,18)/&
         &-0.0006538,      3*0.0,&
         &-0.0006538,      3*0.0,&
         &-0.0001997,      3*0.0,&
         &4*0.0,&
         &-0.0001056,      3*0.0,&
         &0.0058612,      3*0.0,&
         &0.0, -0.0009803, -0.0016416,        0.0,&
         &-0.0001056,      3*0.0,&
         &-0.0002152, -0.0002567, 2*0.0,&
         &-0.0004163,      3*0.0,&
         &4*0.0,&
         &4*0.0,&
         &-0.0001737,        0.0,        0.0,        0.0,&
         &4*0.0,&
         &4*0.0,&
         &4*0.0,&
         &-0.0006538,      3*0.0,&
         &4*0.0/
    data ((DGDS(K,J,2),K=1,4),J=1,24)/&
         &-0.0006538,        0.0,        0.0,        0.0,&
         &-0.0006538,        0.0,        0.0,        0.0,&
         &0.0,        0.0,        0.0,        0.0,&
         &-0.000018,        0.0,        0.0,        0.0,&
         &0.0,        0.0,        0.0,        0.0,&
         &0.0,        0.0,        0.0,        0.0,&
         &0.0,        0.0,        0.0,        0.0,&
         &0.0,        0.0,        0.0,        0.0,&
         &-0.0001672,        0.0,        0.0,        0.0,&
         &-0.0005345, -0.0006363,        0.0,        0.0,&
         &0.0,        0.0,        0.0,        0.0,&
         &0.0,        0.0,        0.0,        0.0,&
         &0.0,        0.0,        0.0,        0.0,&
         &0.0,        0.0,        0.0,        0.0,&
         &0.0,        0.0,        0.0,        0.0,&
         &0.0,        0.0,        0.0,        0.0,&
         &0.0,        0.0,        0.0,        0.0,&
         &0.0,        0.0,        0.0,        0.0,&
         &0.0,        0.0,        0.0,        0.0,&
         &0.0,        0.0,        0.0,        0.0,&
         &-0.0001737,        0.0,        0.0,        0.0,&
         &0.0,        0.0,        0.0,        0.0,&
         &-0.0006538,        0.0,        0.0,        0.0,&
         &0.0,        0.0,        0.0,        0.0/

    data (DGCASP(J,1),J=1,18)/&
         &-0.609774, -0.609774, -0.268610,       0.0, -0.698103,&
         &-0.042546, -0.075306, -0.698103,  -0.17839,  0.127311,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.085958, -0.609774,       0.0/
    data (DGCASP(J,2),J=1,24)/&
         &-0.609774, -0.609774,  0.015235,  0.012704, -0.198194,&
         &0.0, -0.168522, -0.198194, -0.232267, -0.411292,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,  0.085958,&
         &0.0,       0.0, -0.609774,       0.0/

    data (DGSASP(J,1),J=1,18)/&
         &-0.017520, -0.017520,  0.076614,       0.0,  0.102053,&
         &0.332422, -0.036871,  0.102053,  0.052805,  0.076531,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &-0.86398, -0.017520,       0.0/
    data (DGSASP(J,2),J=1,24)/&
         &-0.017520, -0.017520,  0.022753, -0.082731, -0.122483,&
         &0.0,  0.128610, -0.122483, -0.192975, -0.287654,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,  -0.86398,&
         &0.0,       0.0, -0.017520,       0.0/

    data (DGSLOP(J,1),J=1,18)/&
         &-2.057060, -2.057060, -0.711260,       0.0,  1.335928,&
         &-2.43008, -0.129291,  1.335928,  0.784185,  0.024336,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0, -2.057060,       0.0/
    data (DGSLOP(J,2),J=1,24)/&
         &-2.057060, -2.057060, -0.532905, -1.133123,  0.240433,&
         &0.0,  0.120589,  0.240433,  0.383578,  0.016965,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0, -2.057060,       0.0/

    data (DGSLSQ(J,1),J=1,18)/&
         &2.11326,  2.113263,       0.0,       0.0, -1.481349,&
         &0.0,       0.0, -1.481349, -1.504007, -0.781480,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,  2.113263,       0.0/
    data (DGSLSQ(J,2),J=1,24)/&
         &2.113263,  2.113263, -0.086518,  1.931351,       0.0,&
         &0.0,  -.266226,       0.0, -0.333955,  2.282665,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,  2.113263,       0.0/

    data (DGEL(J,1),J=1,18)/&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0, -0.012111,       0.0,       0.0,&
         &-0.075986,       0.0,       0.0/
    data (DGEL(J,2),J=1,24)/&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0, -0.075986,&
         &-0.012111,       0.0,       0.0,       0.0/

    data (DGEL2(J,1),J=1,18)/&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.001193,       0.0,       0.0/
    data (DGEL2(J,2),J=1,24)/&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,  0.001193,&
         &0.0,       0.0,       0.0,       0.0/

    data ((IDGSIM(K,J,1),K=1,5),J=1,18)/&
         &1,1,1,1,1,&
         &1,1,1,1,1,&
         &1,1,1,1,1,&
         &5*1,&
         &1,1,1,2,2,&
         &1,1,1,1,1,&
         &1,1,1,2,2,&
         &1,1,1,2,2,&
         &1,1,1,2,2,&
         &5*1,&
         &5*1,&
         &5*1,&
         &5*1,&
         &5*1,&
         &5*1,&
         &5*1,&
         &1,1,1,1,1,&
         &5*1/
    data ((IDGSIM(K,J,2),K=1,7),J=1,24)/&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,2,1,2,1,1,1,&
         &1,2,1,1,1,1,2,&
         &1,2,2,2,2,2,2,&
         &1,1,1,1,1,1,1,&
         &1,1,1,2,2,2,2,&
         &1,2,2,2,2,2,2,&
         &1,1,2,2,3,2,2,&
         &1,1,1,1,1,2,3,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1/

    data ((DGSIC(K,J,1),K=1,5),J=1,18)/&
         &0.001766,    4*0.0,&
         &0.001766,    4*0.0,&
         &0.011597,    4*0.0,&
         &5*0.0,&
         &0.011389, 0.019985, 3*0.0,&
         &0.006460,    4*0.0,&
         &0.009756, 0.014334, 3*0.0,&
         &0.011389, 0.019985, 3*0.0,&
         &0.003955, 0.006310, 3*0.0,&
         &5*0.0,&
         &5*0.0,&
         &5*0.0,&
         &1.965888,    4*0.0,&
         &0.006460,    4*0.0,&
         &5*0.0,&
         &0.227307,    4*0.0,&
         &0.001766,    4*0.0,&
         &5*0.0/
    data ((DGSIC(K,J,2),K=1,5),J=1,24)/&
         &0.001766,      0.0,       0.0,      0.0,      0.0,&
         &0.001766,      0.0,       0.0,      0.0,      0.0,&
         &0.010968, 0.006827,  0.199457,      0.0,      0.0,&
         &0.017663, 0.005327,       0.0,      0.0,      0.0,&
         &0.015133, 0.021085,       0.0,      0.0,      0.0,&
         &0.006460,      0.0,       0.0,      0.0,      0.0,&
         &0.021764, 0.027956,       0.0,      0.0,      0.0,&
         &0.015133, 0.021085,       0.0,      0.0,      0.0,&
         &0.004468, 0.008147, -0.015283,      0.0,      0.0,&
         &0.019282, 0.049804,   0.02943,      0.0,      0.0,&
         &0.0,      0.0,       0.0,      0.0,      0.0,&
         &0.0,      0.0,       0.0,      0.0,      0.0,&
         &0.0,      0.0,       0.0,      0.0,      0.0,&
         &0.0,      0.0,       0.0,      0.0,      0.0,&
         &0.0,      0.0,       0.0,      0.0,      0.0,&
         &0.0,      0.0,       0.0,      0.0,      0.0,&
         &0.0,      0.0,       0.0,      0.0,      0.0,&
         &0.0,      0.0,       0.0,      0.0,      0.0,&
         &0.0,      0.0,       0.0,      0.0,      0.0,&
         &0.227307,      0.0,       0.0,      0.0,      0.0,&
         &1.965888,      0.0,       0.0,      0.0,      0.0,&
         &0.0,      0.0,       0.0,      0.0,      0.0,&
         &0.001766,      0.0,       0.0,      0.0,      0.0,&
         &0.0,      0.0,       0.0,      0.0,      0.0/

    data IGCCFM/&
         &5*1,&
         &5*1,&
         &1,2,2,2,2,&
         &5*1,&
         &1,1,2,3,3,&
         &5*1,&
         &5*1,&
         &1,1,2,3,3,&
         &1,1,1,2,2,&
         &5*1,&
         &5*1,&
         &5*1,&
         &5*1,&
         &5*1,&
         &5*1,&
         &5*1,&
         &5*1,&
         &5*1/

    data DGCCFC/&
         &-0.199592,    4*0.0,&
         &-0.199592,    4*0.0,&
         &-0.641932,-0.141370,    3*0.0,&
         &5*0.,&
         &-0.045495,-0.204852,-0.311383,    2*0.0,&
         &0.472247,    4*0.0,&
         &-0.206752,    4*0.0,&
         &-0.045495,-0.204852,-0.311383,    2*0.0,&
         &-0.186614,-0.023236,    3*0.0,&
         &5*0.,&
         &5*0.,&
         &5*0.,&
         &5*0.,&
         &5*0.,&
         &5*0.,&
         &5*0.,&
         &-0.199592,    4*0.0,&
         &5*0./

    data ICHBCL/4*1,1,4*1,1,4*1,2*2,5*1,4,18*0,7*1,4*4,& !51
         &222*0,6*1,2,2*3,2*4,4,3,4*0,3*4,12*0,2*0,0,0,0,0,0,5,3*0,& !52-316
         &16*0,2*0,0,3*0,9*0,0,3*0,0,0,0,2*0,3*0,3*0,0/ !317-363

    data DGHAB/&
         &0.0, 0.006074, 0.181590, -0.196098, -0.055780, 0.133907,&
         &0.045857/

    data DGCCFA/&
         &-0.199592, -0.199592,       0.0, -0.098821, -0.043414,&
         &0.0, -0.407523, -0.043414,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0, -0.199592,       0.0/


    if (LCONS) go to 1000

    ! Check whether debugging is enabled
    call DBCHK (DEBUG,'DGF',3,ICYC)

    !  DEBUG OUTPUT: MODEL COEFFICIENTS.

    if (DEBUG) write(JOSTND,3)ICYC
    3 format(' ENTERING SUBROUTINE DGF  CYCLE =',I5)
    if (DEBUG)write(JOSTND,*) ' DGCON= ', DGCON

    !  SPECIES USING SURROGATE EQUATIONS FROM THE CR VARIANT HAVE SPECIAL
    !  NEEDS; COMPUTE THOSE HERE.

    AGERNG = 1000.
    YOUNG = 1000.
    OLD = 0.0

    ! CALL ARANGE AND BADIST

    call BADIST(DEBUG)

    ! FIND AGE DISTRIBUTION

    do I = 1,ITRN
       if (ABIRTH(I).le.1.)go to 5
       if (HT(I) .le.4.5) go to 5
       if (ABIRTH(I) .lt. YOUNG) YOUNG = ABIRTH(I)
       if (ABIRTH(I) .gt. OLD) OLD = ABIRTH(I)
       5 continue

       ! COMPUTE AGE RANGE.

       AGERNG = OLD - YOUNG
       AGERNG = ABS(AGERNG)
    end do

    ! COMPUTE STAGNATION MULTIPLIER DSTAG. DSTAG INITIALIZED IN GRINIT.
    ! IF THIS PASS THROUGH **DGF** IS DURING CALIBRATION (LSTART = .TRUE.)
    ! INCLUDE RECENT MORTALITY TREES (IMC(I)=7) IN THE CALCULATION.

    DSTAG = 1.0
    SUMD2 = 0.
    SUMTRE = 0.
    DGQMD = 0.
    if (ITRN .eq.0) go to 1001
    do II = 1,ITRN
       I = IND1(II)
       if (I.ge.IREC2) go to 1002
       SUMD2 = SUMD2+DBH(I)*DBH(I)*PROB(I)
       SUMTRE = SUMTRE+PROB(I)
       if (DEBUG)write(JOSTND,*)' DGF II,I,DBH,PROB,SUMD2,',&
            &'SUMTRE= ',II,I,DBH(I),PROB(I),SUMD2,SUMTRE
       1002 continue
    end do

    ! ADD IN RECENT MORTALITY TREES IF CALIBRATING

    if (LSTART) then
       do II = IREC2,MAXTRE
          ISPC = ISP(II)
          DPROB = PROB(II)/(FINT/FINTM)
          if (DEBUG)write(JOSTND,*)' II,ISPC,IMC,DBH,DPROB= ',&
               &II,ISPC,IMC(II),DBH(II),DPROB
          if (IMC(II) .eq.7) then
             SUMD2 = SUMD2+DBH(II)*DBH(II)*DPROB
             SUMTRE = SUMTRE+DPROB
             if (DEBUG)write(JOSTND,*)' DGF II,ISPC,DBH,DPROB,SUMD2,',&
                  &'SUMTRE= ',II,ISPC,DBH(II),DPROB,SUMD2,SUMTRE
          end if
       end do
    end if

    if (DEBUG)write(JOSTND,*)' SUMD2,SUMTRE= ',SUMD2,SUMTRE
    DGQMD = SQRT(SUMD2/SUMTRE)
    1001 continue
    if (ICYC.gt.0.and. BA.gt.0.0) then
       if (DGQMD.gt.0.) then
          STDSDI = SUMTRE*(DGQMD/10.0)**1.605
       else
          STDSDI = 0.
       end if
       RELSDI = 0.
       call SDICAL(1,SDIMAX)
       if (SDIMAX.gt.0.)RELSDI = STDSDI/SDIMAX
       if (DEBUG)write(JOSTND,*)' STDSDI,SDIMAX,RELSDI= ',&
            &STDSDI,SDIMAX,RELSDI
       if (RELSDI .gt.0.85) RELSDI = 0.85
       if (RELSDI .gt.0.7) DSTAG = 3.33333 * (1.0 - RELSDI)
       if (DEBUG)write(JOSTND,*)' STAGNATION EFFECT= ',DSTAG
       if (DEBUG)write(JOSTND,*)' STAGNATION FLAGS = ',ISTAGF
    end if

    !  END OF SPECIAL NEEDS SECTION

    !  SCALE BASAL AREA.

    BA100 = BA/100.
    if (IB.eq.2) then
       ISI = NINT(SITEAR(ISISP))
       if (ISI.gt.59)ISI = 60
       if (ISI.lt.20)ISI = 20
       ISC = INT(real(ISI)/10.)

       ! PUT THE SITE IN 10'S INTO A CLASS 1-5

       ISC = ISC-1
    end if

    !  BEGIN SPECIES LOOP.  ASSIGN VARIABLES WHICH ARE SPECIES DEPENDENT

    do ISPC = 1,MAXSP
       I1 = ISCT(ISPC,1)
       if (I1.eq.0) go to 20
       I2 = ISCT(ISPC,2)
       if (IB.eq.2) then
          if (ISC.ge.4.and. ISPC.eq.8) DGCCF(8) = -0.154870
       end if
       CONSPP = DGCON(ISPC) + COR(ISPC) + 0.01*DGCCF(ISPC)*RELDEN
       if (KGR(ISPC,IB).eq.2) then
          TEMBA = BA
          if (TEMBA .lt.1)TEMBA = 1.
          CONSPP = CONSPP - 0.257322*ALOG(BA)
       end if

       if (DEBUG)write(JOSTND,9001)ISPC,DGCON(ISPC),COR(ISPC),&
            &DGCCF(ISPC),RELDEN,CONSPP
       9001 format(' IN DGF 9001 ISPC,DGCON,COR,DGCCF,RELDEN,CONSPP=',&
            &I5,5F10.5)
       DGLDS = DGLD(ISPC,IB)
       DGBALS = DGBAL(ISPC,IB)
       DGCRS = DGCR(ISPC,IB)
       DGCRS2 = DGCRSQ(ISPC,IB)
       DGDSQS = DGDSQ(ISPC)
       DGPTCC = DGPCCF(ISPC,IB)
       DGDBLS = DGDBAL(ISPC,IB)
       SI = SITEAR(ISPC)

       if (DEBUG) then
          if (IB.eq.1) then
             write(JOSTND,9002)ISPC,CONSPP,DGLDS,DGBALS,&
                  &DGCRS,DGCRS2,DGDSQS,DGPTCC,DGCCF(ISPC),DGPTCC
          else
             write(JOSTND,9005)ISPC,CONSPP,DGLDS,DGBALS,&
                  &DGCRS,DGCRS2,DGDSQS,ISC,DGPTCC,DGCCF(ISPC),DGPTCC
          end if
       end if
       9002 format(' DGF ISPC',I2,1X,6(E12.4,1X),3E10.4)
       9005 format(' DGF ISPC',I2,1X,6(E12.4,1X),I5,3E10.4)

       !  BEGIN TREE LOOP WITHIN SPECIES ISPC.

       do I3 = I1,I2
          if (IB.eq.2) then
             BAL = 0.0
             CR = 0.0
          end if
          I = IND1(I3)
          D = DIAM(I)
          BARK = BRATIO(ISPC,D,HT(I))
          if (D.le.0.0) goto 10

          select case (KGR(ISPC,IB))

          case(1)
             CR = ICR(I) * 0.01
             BAL = (1.0 - (PCT(I)/100.)) * BA100
             if (IB.eq.1) then
                DDS = CONSPP + DGLDS*ALOG(D) + DGBALS*BAL + CR*DGCRS+CR*CR*DGCRS2&
                     &+DGDSQS*D*D
             else
                DDS = CONSPP + DGLDS*ALOG(D) + DGBALS*BAL + CR*DGCRS+CR*CR*DGCRS2&
                     &+DGDSQS*D*D + DGPTCC*PCCF(ITRE(I))
                if (ISPC.eq.5)DDS = DDS*0.95
             end if

          case(2)
             CR = ICR(I) * 0.01
             BAL = (1.0 - (PCT(I)/100.)) * BA
             PBAL = (1.0 - (PCT(I)/100.)) * PTBAA(ITRE(I))
             DDS = CONSPP +DGLDS*ALOG(D) + CR*(DGCRS+CR*DGCRS2)&
                  &+ DGDSQS*D*D  + DGDBLS*PBAL/(ALOG(D+1.0))

          case(3)
             CR = ICR(I)
             call DGFASP(D,ASPDG,CR,BARK,SI,DEBUG)
             DDS = ASPDG + ALOG(COR2(ISPC)) + COR(ISPC)

          case(4)
             DPP = D
             if (DPP .lt.1.0) DPP = 1.0
             BATEM = BA
             if (BATEM .lt.1.0) BATEM = 1.0
             DF = DGFJUN(DPP,BATEM,SI)
             !CCC        IF(DF .GT. DBHMAX(IS)) DF = DBHMAX(IS)
             if (DF .lt. DPP) DF = DPP
             DIAGR = (DF - DPP) * BARK
             if (DIAGR .le.0.) then
                DDS = -9.21
             else
                DDS = ALOG( (DIAGR * (2.0 * DPP * BARK + DIAGR)) ) + CONSPP
                if (DDS .lt. -9.21) DDS = -9.21
             end if

          case(5)
             ALD = ALOG(D)
             CR = ICR(I)*0.01
             BAL = (1.0 - (PCT(I)/100.)) * BA
             if (DEBUG)write(JOSTND,*)' BAL,BA,PCT(I)= ',BAL,BA,PCT(I)
             DGLCCF = 0.0
             DDS = CONSPP +DGLDS*ALD + DGBALS*BAL + CR*(DGCRS+CR*DGCRS2)&
                  &+DGDSQS*D*D  + DGDBLS*BAL/(ALOG(D+1.0))&
                  &+DGPTCC*PCCF(ITRE(I))&
                  &+DGBA(ISPC,IB)*BA

          case(6)
             ICLS = IFIX(D + 1.0)
             if (ICLS .gt.41) ICLS = 41
             BAUTBA = BAU(ICLS) / BA
             SI = SITEAR(ISPC)
             DPP = D
             if (DPP .lt.1.0) DPP = 1.0
             BATEM = BA
             if (BATEM .lt.5.0) BATEM = 5.0
             if (IB.eq.2.and. ISPC.eq.17) then
                DF = DGFJUN(DPP,BATEM,SI)
             else
                DF = DGFCOB(DPP,BATEM,SI,BAUTBA)*1.05
             end if
             !CCC        IF(DF .GT. DBHMAX(ISPC)) DF = DBHMAX(ISPC)
             if (DF .lt. DPP) DF = DPP
             DIAGR = (DF - DPP) * BARK
             if (ISTAGF(ISPC).ne.0) DIAGR = DIAGR*DSTAG
             if (DIAGR .le.0.) then
                DDS = -9.21
             else
                DDS = ALOG( (DIAGR * (2.0 * DPP * BARK + DIAGR)) )&
                     &+ COR(ISPC) + DGCON(ISPC)
                if (DDS .lt. -9.21) DDS = -9.21
             end if

          end select

          if (DDS.lt.-9.21) DDS = -9.21
          WK2(I) = DDS

          !  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.

          if (.not.DEBUG) go to 10
          if (IB.eq.1) then
             write(JOSTND,9003) I,ISPC,D,CR,RELDEN,BA,DDS
          else
             write(JOSTND,9004) I,ISPC,D,BAL,CR,BA,DDS
          end if
          9003 format(' IN DGF, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,&
               &',  CR=',F7.4/&
               &'       RELDEN=',F9.3,',  BA=',F9.3,',   LN(DDS)=',F7.4)
          9004 format(' IN DGF, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,&
               &',  BAL=',F7.2,',  CR=',F7.4,&
               &',  BA=',F9.3,',   LN(DDS),=',F7.4)
          10 continue
       end do

       !  END OF SPECIES LOOP.

       20 continue
    end do

    return

    !  ENTRY **DGCONS**: LOAD COEFFICIENTS OF THE DIAMETER INCREMENT
    !  MODEL THAT ARE SITE SPECIFIC AND NEED ONLY BE RESOLVED ONCE.

    1000 continue

    !  PUT SITE INTO ONE OF 5 CLASSES

    XSITE = SITEAR(ISISP)
    LSI = INT(XSITE/10.)
    if (LSI.lt.2)ISIC = 1
    if (LSI.ge.5)ISIC = 5
    if (LSI.ge.3.and. LSI.lt.4)ISIC = 3
    if (LSI.ge.4.and. LSI.lt.5)ISIC = 4
    if (LSI.ge.2.and. LSI.lt.3)ISIC = 2

    ! ISISP IS THE SPECIES CODE THE SITE WAS TAKEN WITH
    ! ISIC IS THE SITE CLASS 1=0-29,2=30-39,...,5=60+ FOR THE SITE SPECIES

    if (IB.eq.1) then
       ISIC2 = INT(SITEAR(ISISP))
       if (ISIC2.gt.59)ISIC2 = 60
       if (ISIC2.lt.20)ISIC2 = 20
       ISC = ISIC2/10

       ! PUT THE SITE IN 10'S INTO A CLASS 1-5

       ISC = ISC-1
    end if

    !  ENTER LOOP TO LOAD SPECIES DEPENDENT VECTORS.

    do ISPC = 1,MAXSP
       if (IB.eq.1) then
          KSI = IGCCFM(ISC,ISPC)
          DGCCF(ISPC) = DGCCFC(KSI,ISPC)
       else
          DGCCF(ISPC) = DGCCFA(ISPC)
       end if
       ISPFOR = MAPLOC(IFOR,ISPC,IB)
       ISPDSQ = MAPDSQ(IFOR,ISPC,IB)

       !  LOAD DGCON, DGDSQ,  AND ATTEN.   SHIFT ASPECT 45 DEGREES
       !  FOR THE DIAMETER INCREMENT MODEL INTERCEPT CALCULATIONS
       !  (NOT FOR MIXED HARDWOODS OR TT PP).

       !  NOTE: 0.7854 IS 45 DEGREES IN RADIANS. THIS CORRECTION IS
       !        PROBABLY DUE TO A PROBLEM IN THE WAY ASPECT WAS RECORDED
       !        IN THE DATA SET USED TO FIT THE MODEL.  GED 12-20-94.

       ISI = ISMAP(ISISP,IB)
       TEMEL = ELEV
       XSITE = SITEAR(ISISP)

       select case(KGR(ISPC,IB))

       case(5)
          if (ISPC.eq.KHWEL.and. TEMEL.gt.30.)TEMEL = 30.
          XSITE = ALOG(XSITE)
          ASPTEM = ASPECT

       case(2)
          ASPTEM = ASPECT

       case default
          ASPTEM = ASPECT-0.7854

       end select

       DGCON(ISPC) = DGSIC(IDGSIM(ISI,ISPC,IB),ISPC,IB) *XSITE&
            &+ DGFOR(ISPFOR,ISPC,IB)&
            &+(DGSASP(ISPC,IB) * SIN(ASPTEM)&
            &+ DGCASP(ISPC,IB) * COS(ASPTEM)&
            &+ DGSLOP(ISPC,IB)) * SLOPE&
            &+ DGSLSQ(ISPC,IB) * SLOPE * SLOPE&
            &+ DGEL(ISPC,IB) * TEMEL&
            &+ DGEL2(ISPC,IB) * TEMEL * TEMEL
       DGDSQ(ISPC) = DGDS(ISPDSQ,ISPC,IB)
       SMCON(ISPC) = 0.

       select case (KGR(ISPC,IB))

       case(2)
          if (ITYPE .gt.0) then
             MAPHAB = ICHBCL(ITYPE) + 1
          else
             MAPHAB = 2
          end if
          DGCON(ISPC) = DGCON(ISPC) + DGHAB(MAPHAB)
          ATTEN(ISPC) = OBSERV(ISPC,IB)

       case(5)
          ATTEN(ISPC) = OBSERV(ISPC,IB)

       case default
          ATTEN(ISPC) = IBSERV(ISIC,ISPC,IB)

       end select

       !  IF READCORD OR REUSCORD WAS SPECIFIED (LDCOR2 IS TRUE) ADD
       !  LN(COR2) TO THE BAI MODEL CONSTANT TERM (DGCON).  COR2 IS
       !  INITIALIZED TO 1.0 IN BLKDATA.

       if (LDCOR2.and.COR2(ISPC).gt.0.0) DGCON(ISPC) = DGCON(ISPC)&
            &+ ALOG(COR2(ISPC))
    end do

    return
  end subroutine DGFTU

  subroutine DGFIE(LCONS)
    implicit none

    !  INLAND EMPIRE (IE) AND EASTERN MONTANA (EM).  COEFFICIENT BANK IB:
    !  1 = IE (23 SPECIES), 2 = EM (19).  DDS IS PREDICTED FROM HABITAT
    !  TYPE, LOCATION, SLOPE, ASPECT, ELEVATION, DBH, CROWN RATIO, BASAL
    !  AREA IN LARGER TREES, AND CCF.  KGR GROUPS THE NATIVE SPECIES:
    !    1 = ORIGINAL NI EQUATION (IE 1-14,17,23; EM 4,5)
    !    2 = ORIGINAL EM EQUATION WITH MANAGED/UNMANAGED POINT CCF TERMS
    !        (EM 1-3,7-10,18)
    !    3 = PI/JU LOGIC FROM UT (IE 15,16; EM 6)
    !    4 = ASPEN LOGIC FROM UTAH (IE 18,20,21; EM 12,17, ALSO USED FOR
    !        PAPER BIRCH)
    !    5 = CO LOGIC FROM CR (IE 19,22; EM 11,13-16,19)
    !  IE SPECIES EXPANSION:
    !  WB USE COEFFICIENTS FOR L
    !  LM AND PY USE COEFFICIENTS FROM TT FOR LM
    !  LL USE COEFFICIENTS FOR AF
    !  AS, MM, PB USE COEFFIECIENTS FOR AS FROM UT
    !  CO, OH USE COEFFIECIENTS FOR OH FROM CR
    !  OS USE COEFFEICIENTS FOR MH
    !  PI, JU USE COEFFICIENTS FROM UT
    !  MAPSITE -- SITE INDEX ASSIGNED BY SPECIES IS DEPENDENT UPON HABITAT TYPE
    !  EM: CONSPP IS DIFFERENT FOR LM (4); EM'S **RCON** PASSES IDTYPE, AND
    !  INDICES ABOVE 117 USE HABITAT 30.

    !     DGLD -- COEFFICIENTS FOR THE LOG(DIAMETER) TERM.
    !     DGCR -- COEFFICIENTS FOR THE CROWN RATIO TERM.
    !   DGCRSQ -- COEFFICIENTS FOR THE CROWN RATIO SQUARED TERM.
    !    DGBAL -- COEFFICIENTS FOR THE BASAL AREA IN LARGER TREES TERM.
    !   DGDBAL -- COEFFICIENTS FOR THE INTERACTION BETWEEN BASAL AREA IN
    !             LARGER TREES AND LN(DBH).
    !    DGCCF -- CROWN COMPETITION FACTOR COEFFICIENTS (COEFFS), LOADED
    !             IN **DGCONS**.

    logical :: LCONS
    integer, parameter :: IB = merge(1, 2, VARIANTCD.eq.'IE')
    logical :: DEBUG
    integer :: I,I1,I2,I3,ISIC,ICLS,ISPC,LSI,IPCCF,JDTYPE,J,K,L
    real    :: YOUNG,OLD,BA100,CONSPP,DGLDS,DGBALS,DGCRS,DGCRS2,DGDSQS
    real    :: DGDBLS,SI,D,ALD,BARK,BRATIO,DPP,BATEM,DF,DIAGR,DDS,CR
    real    :: ASPDG,BAL,XSITE,TMPASP,ALCCF,DUM1,DUM2,XSLOPE,DGFCOA,DGFJUN
    integer :: KGR(23,2),MAPHAB(117,23,2),MAPLOC(11,23,2),MAPDSQ(11,23,2)
    integer :: MAPCCF(30,23),MAPCCE(30)
    real    :: DGLD(23,2),DGCR(23,2),DGCRSQ(23,2),DGBAL(23,2),DGDBAL(23,2)
    real    :: OBSERV(6,23,2),DGHAB(8,23,2),DGFOR(6,23,2),DGDS(4,23,2)
    real    :: DGCASP(23,2),DGSASP(23,2),DGSLOP(23,2),DGSLSQ(23,2)
    real    :: DGEL(23,2),DGEL2(23,2),DGCCFA(5,23),DGCCFE(3)
    real    :: DGLCCF(19),DGPCC1(19),DGPCC2(19),DGPCCF(19)

    data (KGR(J,1),J=1,23)/14*1,3,3,1,4,5,4,4,5,1/
    data (KGR(J,2),J=1,19)/2,2,2,1,1,3,2,2,2,2,5,4,5,5,5,5,4,2,5/

    data (DGLD(J,1),J=1,23)/&
         &0.56445, 0.54140, 0.56888, 0.68810, 0.68712, 0.58705,&
         &0.89503, 0.73045, 0.86240, 0.66101, 0.89778,&
         &0.54140, 0.213947,0.86240,     0.0,     0.0, 0.213947,&
         &0.0,      0.0,    0.0,     0.0,     0.0, 0.89778/
    data (DGLD(J,2),J=1,19)/&
         &0.80110,  0.80110,  0.82617,  0.213947,   0.86240,        0.,&
         &0.75719,  0.83323,  0.89571,   0.58932,        0.,        0.,&
         &0.,       0.,       0.,        0.,        0.,   0.80110,&
         &0./

    data (DGCR(J,1),J=1,23)/&
         &1.08338, 1.03478, 2.06850, 1.93969, 1.64133, 1.29360,&
         &1.85558, 1.54643, 0.52044, 1.31618, 1.28403,&
         &1.03478, 1.523464,0.52044,     0.0,     0.0, 1.523464,&
         &0.0,     0.0,     0.0,     0.0,     0.0, 1.28403/
    data (DGCR(J,2),J=1,19)/&
         &1.02878,  1.02878,  1.41015,  1.523464,   0.52044,        0.,&
         &1.43611,  1.10040,  1.30934,   1.94874,        0.,        0.,&
         &0.,       0.,       0.,        0.,        0.,   1.02878,&
         &0./

    data (DGCRSQ(J,1),J=1,23)/&
         &0.0, 0.07509,-0.62361,-0.78258,-0.27244,     0.0,&
         &-0.36393,-0.26635, 0.86236,     0.0,     0.0,&
         &0.07509,     0.0, 0.86236,     0.0,     0.0,     0.0,&
         &0.0,     0.0,     0.0,     0.0,     0.0,     0.0/
    data (DGCRSQ(J,2),J=1,19)/&
         &-0.45448, -0.45448, -0.55362,        0.,   0.86236,        0.,&
         &-0.44926,       0., -0.17348,  -0.88761,        0.,        0.,&
         &0.,       0.,       0.,        0.,        0.,  -0.45448,&
         &0./

    data (DGBAL(J,1),J=1,23)/&
         &0.42112, 0.43637, 0.50202, 0.45142,     0.0, 0.74596,&
         &-0.03665, 0.25639,     0.0,     0.0,     0.0,&
         &0.43637,-0.358634,    0.0,     0.0,     0.0,-.358634,&
         &0.0,      0.0,    0.0,     0.0,     0.0,     0.0/
    data (DGBAL(J,2),J=1,19)/&
         &0.00064,  0.00064,  0.00216, -0.358634,        0.,        0.,&
         &0.00556,  0.00443,  0.00350,   0.00710,        0.,        0.,&
         &0.,       0.,       0.,        0.,        0.,   0.00064,&
         &0./

    data (DGDBAL(J,1),J=1,23)/&
         &-2.08272,-2.03256,-2.11590,-1.76812,-0.80918,-2.28375,&
         &-0.43329,-1.18218,-0.51270,-1.25881,-0.66110,&
         &-2.03256,     0.0,-0.51270,     0.0,     0.0,     0.0,&
         &0.0,     0.0,     0.0,     0.0,     0.0,-0.66110/
    data (DGDBAL(J,2),J=1,19)/&
         &-0.00328, -0.00328, -0.00889,        0.,  -0.51270,        0.,&
         &-0.01282, -0.01281, -0.00764,  -0.02316,        0.,        0.,&
         &0.,       0.,       0.,        0.,        0.,  -0.00328,&
         &0./

    data ((OBSERV(K,J,1),K=1,6),J=1,23)/&
         &1635.0,   76.0,   94.0,    0.0,    0.0,    0.0,&
         &968.0,  900.0,  797.0, 1124.0,   47.0,  809.0,&
         &1079.0, 1682.0, 4014.0,   63.0, 4684.0,    0.0,&
         &816.0,   67.0,  122.0, 3678.0,    0.0,    0.0,&
         &1774.0,    0.0,    0.0,    0.0,    0.0,    0.0,&
         &1176.0,   67.0, 1198.0,    0.0,    0.0,    0.0,&
         &93.0,  940.0,   62.0, 2548.0,  719.0, 2590.0,&
         &907.0,  827.0,   46.0,   35.0,  146.0, 3092.0,&
         &988.0,  177.0,   40.0,  331.0,  332.0, 2380.0,&
         &429.0,   59.0,  605.0,   17.0,  879.0,    0.0,&
         &55.0,  880.0,    0.0,    0.0,    0.0,    0.0,&
         &968.0,  900.0,  797.0, 1124.0,   47.0,  809.0,&
         &27.,    70.,   123.,   101.,    33.,     0.,&
         &988.0,  177.0,   40.0,  331.0,  332.0, 2380.0,&
         &1000.,  1000.,  1000.,  1000.,  1000.,  1000.,&
         &1000.,  1000.,  1000.,  1000.,  1000.,  1000.,&
         &27.,    70.,   123.,   101.,    33.,     0.,&
         &184.,   429.,   356.,   162.,    74.,     0.,&
         &1000.,  1000.,  1000.,  1000.,  1000.,  1000.,&
         &184.,   429.,   356.,   162.,    74.,     0.,&
         &184.,   429.,   356.,   162.,    74.,     0.,&
         &1000.,  1000.,  1000.,  1000.,  1000.,  1000.,&
         &55.0,  880.0,    0.0,    0.0,    0.0,    0.0/
    data ((OBSERV(K,J,2),K=1,6),J=1,19)/&
         &256.,    0.,    0.,    0.,    0.,    0.,&
         &306.,    0.,    0.,    0.,    0.,    0.,&
         &697.,    0.,    0.,    0.,    0.,    0.,&
         &27.,   70.,  123.,  101.,   33.,    0.,&
         &988.,  177.,   40.,  331.,  332., 2380.,&
         &1000.,    0.,    0.,    0.,    0.,    0.,&
         &3208.,    0.,    0.,    0.,    0.,    0.,&
         &0.,    0.,    0.,    0.,    0.,    0.,&
         &1198.,    0.,    0.,    0.,    0.,    0.,&
         &7837.,    0.,    0.,    0.,    0.,    0.,&
         &1000.,    0.,    0.,    0.,    0.,    0.,&
         &184.,  429.,  356.,  162.,   74.,    0.,&
         &1000.,    0.,    0.,    0.,    0.,    0.,&
         &1000.,    0.,    0.,    0.,    0.,    0.,&
         &1000.,    0.,    0.,    0.,    0.,    0.,&
         &1000.,    0.,    0.,    0.,    0.,    0.,&
         &184.,  429.,  356.,  162.,   74.,    0.,&
         &22.,    0.,    0.,    0.,    0.,    0.,&
         &1000.,    0.,    0.,    0.,    0.,    0./

    data ((MAPHAB(K,J,1),K=1,30),J=1,23)/&
         &12*2, 7*1, 2,2,1,1, 7*2,&
         &9*5, 1,1,2,1, 3*2, 3,3,2,5,2, 3*1, 5,5,4, 3*5,&
         &7*3, 1, 3*3, 8*1, 3,2, 6*3, 2,3,3,&
         &12*2, 1, 8*2, 1,2,2,1, 5*2,&
         &30*1,&
         &14*2, 1,1, 14*2,&
         &4*5, 1,2,1, 4*5, 2,2, 6*3, 4,4,3,4, 3*5, 4,4,5,5,&
         &11*4, 1,1,4,2,2,4,2,1, 8*4, 3,4,4,&
         &12*6, 1,2,3,3,4,3,1, 7*6, 1,5,5,6,&
         &1,1,2,3,3,2,2, 4*3, 3*2, 4*3, 2, 11*3,&
         &21*2, 1, 8*2,&
         &9*5, 1,1,2,1, 3*2, 3,3,2,5,2, 3*1, 5,5,4, 3*5,&
         &30*1,&
         &12*6, 1,2,3,3,4,3,1, 7*6, 1,5,5,6,&
         &30*1,&
         &30*1,&
         &30*1,&
         &30*1,&
         &30*1,&
         &30*1,&
         &30*1,&
         &30*1,&
         &21*2, 1, 8*2/
    data ((MAPHAB(L,K,2),L = 1,117),K = 1,3)/&
         ! SPECIES 1=WB
         &2*4,4*3,16*2,5*4,2*3,2*4,4*3,4,9*3,4,2*3,4*4,2*3,2*2,&
         &3*3,2,2*3,2,13*3,3*2,2*3,2*2,3*3,1,8*3,2,5*3,4,3,2,3,&
         &2,3,2,3,2*2,5*3,4,&
         ! SPECIES 2=WL
         &2*4,4*3,16*2,5*4,2*3,2*4,4*3,4,9*3,4,2*3,4*4,2*3,2*2,&
         &3*3,2,2*3,2,13*3,3*2,2*3,2*2,3*3,1,8*3,2,5*3,4,3,2,3,&
         &2,3,2,3,2*2,5*3,4,&
         ! SPECIES 3=DF
         &2*5,7,2*4,2*7,3,3*5,6,12*4,2,4,2*5,2*3,4,2*5,3,7,3*5,7,5,&
         &2*4,2*5,2,6,5,5*3,3*7,2*5,4*6,3*5,3,7,2*3,2*6,5*3,5,2*6,&
         &4*5,4,3,2*5,3*7,5,2*6,5*5,4,6,1,5,2*3,8*5,6*4/
    data ((MAPHAB(L,K,2),L = 1,117),K = 4,6)/&
         ! SPECIES 4=LM
         &30*1,87*0,&
         ! SPECIES 5=LL
         &12*6, 1,2,3,3,4,3,1, 7*6, 1,5,5,6,87*0,&
         ! SPECIES 6=RM
         &30*1,87*0/
    data ((MAPHAB(L,K,2),L = 1,117),K = 7,9)/&
         ! SPECIES 7=LP
         &1,7,6,1,2,2*6,8,7,2*1,8,3,8,2,1,8,1,3,3*1,2*8,2*3,2,7,3,7,&
         &2*1,2,3,1,2*3,2*1,7,3,7,2*1,2,2*3,1,3*2,3,7,6,2*1,2,7,3,2*8,&
         &3*7,8,6,8,6,1,7,3,6,2,3,4*2,7,2,3,1,7,3*2,5,1,2,7,8,2,4,2,5,&
         &2,1,2*3,3*1,3,1,4*6,2,3*6,5,3,6,3,7,&
         ! SPECIES 8=ES
         &24*4,5,1,4,5,2*6,3,2*4,2*5,4*4,3*3,3*4,2,5,6,3*4,&
         &5,5*4,3*5,4,5,2*4,2*2,1,2*4,1,2,4,3*5,3,4,2*2,4,2,&
         &2*4,2,3,3*4,3,3*4,3,2*4,3*2,6,5,2*4,3,5,4,3,5*4,6*6,&
         ! SPECIES 9=AF
         &3,27*2,3*5,8*2,13*5,2,3*5,10*4,5*3,1,3,5*2,4,2,2*4,&
         &2*3,5*2,2*3,4,2,3,2,3,4,5*3,5,3,7*2,6*5/
    data ((MAPHAB(L,K,2),L = 1,117),K = 10,12)/&
         ! SPECIES 10=PP
         &12*1,3*2,3,3,2,3,4,5,4,1,3*2,6,3,3,10*4,9*5,6*7,&
         &3,3,61*7,&
         ! SPECIES 11=GA
         &30*1,87*0,&
         ! SPECIES 12=AS
         &30*1,87*0/
    data ((MAPHAB(L,K,2),L = 1,117),K = 13,15)/&
         ! SPECIES 13=CW
         &30*1,87*0,&
         ! SPECIES 14=BA
         &30*1,87*0,&
         ! SPECIES 15=PW
         &30*1,87*0/
    data ((MAPHAB(L,K,2),L = 1,117),K = 16,19)/&
         ! SPECIES 16=NC
         &30*1,87*0,&
         ! SPECIES 17=PB
         &30*1,87*0,&
         ! SPECIES 18=OS
         &2*4,4*3,16*2,5*4,2*3,2*4,4*3,4,9*3,4,2*3,4*4,2*3,2*2,&
         &3*3,2,2*3,2,13*3,3*2,2*3,2*2,3*3,1,8*3,2,5*3,4,3,2,3,&
         &2,3,2,3,2*2,5*3,4,&
         ! SPECIES 19=OH
         &30*1,87*0/

    data ((DGHAB(K,J,1),K=1,6),J=1,23)/&
         &1.15584, 1.05635,     0.0,     0.0,     0.0,     0.0,&
         &0.38335, 0.51291, 0.45377, 0.71322, 0.26835,     0.0,&
         &0.47780, 0.15228, 0.29764,     0.0,     0.0,     0.0,&
         &0.66755, 0.60454,     0.0,     0.0,     0.0,     0.0,&
         &0.45264,     0.0,     0.0,     0.0,     0.0,     0.0,&
         &1.61452, 1.31772,     0.0,     0.0,     0.0,     0.0,&
         &0.77399, 0.67828, 0.64451, 0.37945, 0.54337,     0.0,&
         &-0.58842,-0.21235,-0.71629,-0.53954,     0.0,     0.0,&
         &-0.96389,-0.72415,-0.57308,-0.82218,-1.24093,-1.10746,&
         &1.16233, 0.73408, 0.51417,     0.0,     0.0,     0.0,&
         &-1.68033,-1.52111,     0.0,     0.0,     0.0,     0.0,&
         &0.38335, 0.51291, 0.45377, 0.71322, 0.26835,     0.0,&
         &6*0.0,&
         &-0.96389,-0.72415,-0.57308,-0.82218,-1.24093,-1.10746,&
         &6*0.0,&
         &6*0.0,&
         &6*0.0,&
         &6*0.0,&
         &6*0.0,&
         &6*0.0,&
         &6*0.0,&
         &6*0.0,&
         &-1.68033,-1.52111,     0.0,     0.0,     0.0,     0.0/
    data ((DGHAB(L,K,2),L = 1,8),K = 1,10)/&
         ! SPECIES 1=WB
         &0.,-0.20842, 0.01545, 0.29742,      0.,      0.,      0.,     0.,&
         ! SPECIES 2=WL
         &0.,-0.20842, 0.01545, 0.29742,      0.,      0.,      0.,     0.,&
         ! SPECIES 3=DF
         &0.,-0.20240,-0.03260, 0.05559,-0.12847,-0.07100,-0.31860,     0.,&
         ! SPECIES 4=LM
         &0.,      0.,      0.,      0.,      0.,      0.,      0.,     0.,&
         ! SPECIES 5=LL
         &-0.96389,-0.72415,-0.57308,-0.82218,-1.24093,-1.10746,  0.,    0.,&
         ! SPECIES 6=RM
         &0.,      0.,      0.,      0.,      0.,      0.,      0.,     0.,&
         ! SPECIES 7=LP
         &0.,-0.06686, 0.00921,-0.17011,-0.11806,-0.22504, 0.06528,0.14340,&
         ! SPECIES 8=ES
         &0., 0.23303,-0.01746, 0.11129, 0.19065, 0.40362,      0.,     0.,&
         ! SPECIES 9=AF
         &0., 0.12967, 0.33768, 0.25940, 0.48657,      0.,      0.,     0.,&
         ! SPECIES 10=PP
         &0.,-0.19356,-0.07999, 0.03500,-0.13880,-1.04929,-0.32378,     0./
    data ((DGHAB(L,K,2),L = 1,8),K = 11,19)/&
         ! SPECIES 11=GA
         &0.,      0.,      0.,      0.,      0.,      0.,      0.,     0.,&
         ! SPECIES 12=AS
         &0.,      0.,      0.,      0.,      0.,      0.,      0.,     0.,&
         ! SPECIES 13=CW
         &0.,      0.,      0.,      0.,      0.,      0.,      0.,     0.,&
         ! SPECIES 14=BA
         &0.,      0.,      0.,      0.,      0.,      0.,      0.,     0.,&
         ! SPECIES 15=PW
         &0.,      0.,      0.,      0.,      0.,      0.,      0.,     0.,&
         ! SPECIES 16=NC
         &0.,      0.,      0.,      0.,      0.,      0.,      0.,     0.,&
         ! SPECIES 17=PB
         &0.,      0.,      0.,      0.,      0.,      0.,      0.,     0.,&
         ! SPECIES 18=OS
         &0.,-0.20842, 0.01545, 0.29742,      0.,      0.,      0.,     0.,&
         ! SPECIES 19=OH
         &0.,      0.,      0.,      0.,      0.,      0.,      0.,     0./

    data ((MAPLOC(K,J,1),K=1,11),J=1,23)/&
         &2,1,2,2,2,2,2,2,2,2,1,&
         &1,1,1,2,3,3,2,5,5,4,1,&
         &5,4,1,2,3,3,2,3,5,1,4,&
         &6,5,1,2,2,3,2,4,6,2,5,&
         &3,2,3,1,3,3,3,3,3,3,2,&
         &4,1,1,1,2,2,3,4,2,1,1,&
         &4,2,1,1,2,4,3,3,4,2,2,&
         &3,1,1,1,3,2,3,3,3,1,1,&
         &4,1,1,2,2,3,3,4,4,2,1,&
         &1,2,2,2,1,4,3,1,4,3,2,&
         &3,2,1,1,3,3,3,3,1,3,2,&
         &1,1,1,2,3,3,2,5,5,4,1,&
         &11*1,&
         &4,1,1,2,2,3,3,4,4,2,1,&
         &11*1,&
         &11*1,&
         &11*1,&
         &11*1,&
         &11*1,&
         &11*1,&
         &11*1,&
         &11*1,&
         &3,2,1,1,3,3,3,3,1,3,2/
    data ((MAPLOC(L,K,2),L = 1,7),K = 1,10)/&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,2,3,4,3,1,5,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,2,3,2,4,5,6,&
         &1,2,3,2,3,4,1,&
         &1,2,3,4,5,6,1,&
         &1,2,3,3,2,2,1/
    data ((MAPLOC(L,K,2),L = 1,7),K = 11,19)/&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1/

    data ((DGFOR(K,J,1),K=1,6),J=1,23)/&
         &0.16920,     0.0,     0.0,     0.0,     0.0,     0.0,&
         &0.20004, 0.07656, 0.08188, 0.30379,     0.0,     0.0,&
         &0.50357, 0.34920, 0.21961, 0.61812,     0.0,     0.0,&
         &0.43443, 0.28344,-0.14829, 0.20205, 0.57763,     0.0,&
         &0.10667, 0.44357,     0.0,     0.0,     0.0,     0.0,&
         &0.50070, 0.17647, 0.31745,     0.0,     0.0,     0.0,&
         &0.43735, 0.21113, 0.14808,     0.0,     0.0,     0.0,&
         &0.26262,-0.15871,     0.0,     0.0,     0.0,     0.0,&
         &0.42062, 0.14072,-0.13001,     0.0,     0.0,     0.0,&
         &0.24588, 0.56958, 0.42787,     0.0,     0.0,     0.0,&
         &0.12520, 0.48076,     0.0,     0.0,     0.0,     0.0,&
         &0.20004, 0.07656, 0.08188, 0.30379,     0.0,     0.0,&
         &1.568742,    0.0,     0.0,     0.0,     0.0,     0.0,&
         &0.42062, 0.14072,-0.13001,     0.0,     0.0,     0.0,&
         &0.0,     0.0,     0.0,     0.0,     0.0,     0.0,&
         &0.0,     0.0,     0.0,     0.0,     0.0,     0.0,&
         &1.568742,    0.0,     0.0,     0.0,     0.0,     0.0,&
         &0.0,     0.0,     0.0,     0.0,     0.0,     0.0,&
         &0.0,     0.0,     0.0,     0.0,     0.0,     0.0,&
         &0.0,     0.0,     0.0,     0.0,     0.0,     0.0,&
         &0.0,     0.0,     0.0,     0.0,     0.0,     0.0,&
         &0.0,     0.0,     0.0,     0.0,     0.0,     0.0,&
         &0.12520, 0.48076,     0.0,     0.0,     0.0,     0.0/
    data ((DGFOR(L,K,2),L = 1,6),K = 1,10)/&
         &1.5675,      0.,      0.,      0.,      0.,      0.,&
         &1.5675,      0.,      0.,      0.,      0.,      0.,&
         &1.10349, 1.55392, 1.04495, 1.27679,      0.,      0.,&
         &1.568742,      0.,      0.,      0.,      0.,      0.,&
         &0.,      0.,      0.,      0.,      0.,      0.,&
         &0.,      0.,      0.,      0.,      0.,      0.,&
         &1.75497, 1.89439, 1.63227, 1.65474, 1.51043,      0.,&
         &-2.45844,-2.14138,-2.55288,-2.42650,      0.,      0.,&
         &0.46110,-0.16517, 0.35319, 0.64719, 0.40140, 0.33757,&
         &3.57069, 3.45044, 3.67551,      0.,      0.,      0./
    data ((DGFOR(L,K,2),L = 1,6),K = 11,19)/&
         &0.,      0.,      0.,      0.,      0.,      0.,&
         &0.,      0.,      0.,      0.,      0.,      0.,&
         &0.,      0.,      0.,      0.,      0.,      0.,&
         &0.,      0.,      0.,      0.,      0.,      0.,&
         &0.,      0.,      0.,      0.,      0.,      0.,&
         &0.,      0.,      0.,      0.,      0.,      0.,&
         &0.,      0.,      0.,      0.,      0.,      0.,&
         &1.5675,      0.,      0.,      0.,      0.,      0.,&
         &0.,      0.,      0.,      0.,      0.,      0./

    data ((MAPDSQ(K,J,1),K=1,11),J=1,23)/&
         &1,2,2,2,2,1,2,1,1,1,2,&
         &1,2,1,1,1,1,1,1,1,1,2,&
         &1,4,2,2,2,3,1,4,1,1,4,&
         &1,1,1,1,1,2,2,3,1,2,1,&
         &1,3,1,2,3,1,1,1,1,1,3,&
         &1,1,2,1,1,1,1,2,1,2,1,&
         &1,1,2,2,1,1,2,3,1,4,1,&
         &1,1,2,1,1,1,3,2,1,1,1,&
         &1,2,2,1,2,1,1,2,1,1,2,&
         &1,2,2,2,2,3,3,2,1,1,2,&
         &1,2,1,1,1,1,1,1,2,1,2,&
         &1,2,1,1,1,1,1,1,1,1,2,&
         &11*1,&
         &1,2,2,1,2,1,1,2,1,1,2,&
         &11*1,&
         &11*1,&
         &11*1,&
         &11*1,&
         &11*1,&
         &11*1,&
         &11*1,&
         &11*1,&
         &1,2,1,1,1,1,1,1,2,1,2/
    data ((MAPDSQ(L,K,2),L = 1,7),K = 1,10)/&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,3,2,1,3,2,3,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,2,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,2,1,1,1,1,1/
    data ((MAPDSQ(L,K,2),L = 1,7),K = 11,19)/&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1/

    data ((DGDS(K,J,1),K=1,4),J=1,23)/&
         &-0.000439,-0.000004,      0.0,      0.0,&
         &-0.000310,-0.000566,      0.0,      0.0,&
         &-0.000252,-0.000373,-0.000502,-0.000572,&
         &-0.000274,-0.000089,-0.000643,      0.0,&
         &-0.000225,-0.000216,-0.000429,      0.0,&
         &0.0,      0.0,      0.0,      0.0,&
         &-0.001260,-0.002168,-0.001889,-0.000867,&
         &-0.000132,-0.000294,-0.000427,      0.0,&
         &-0.000283,-0.000780,      0.0,      0.0,&
         &-0.000406,-0.000437,-0.000140,      0.0,&
         &-0.000484,-0.000306,      0.0,      0.0,&
         &-0.000310,-0.000566,      0.0,      0.0,&
         &4*-0.0006538,&
         &-0.000283,-0.000780,      0.0,      0.0,&
         &4*0.0,&
         &4*0.0,&
         &4*-0.0006538,&
         &4*0.0,&
         &4*0.0,&
         &4*0.0,&
         &4*0.0,&
         &4*0.0,&
         &-0.000484,-0.000306,      0.0,      0.0/
    data ((DGDS(L,K,2),L = 1,4),K = 1,10)/&
         &0.,        0.,            0.,       0.,&
         &0.,        0.,            0.,       0.,&
         &-0.000251, -0.000412,            0.,       0.,&
         &-0.0006538,        0.,            0.,       0.,&
         &-0.000283,        0.,            0.,       0.,&
         &0.,        0.,            0.,       0.,&
         &-0.000402,        0.,            0.,       0.,&
         &-0.000034,        0.,            0.,       0.,&
         &0.,        0.,            0.,       0.,&
         &-0.000248, -0.000991,            0.,       0./
    data ((DGDS(L,K,2),L = 1,4),K = 11,19)/&
         &0.,        0.,            0.,       0.,&
         &0.,        0.,            0.,       0.,&
         &0.,        0.,            0.,       0.,&
         &0.,        0.,            0.,       0.,&
         &0.,        0.,            0.,       0.,&
         &0.,        0.,            0.,       0.,&
         &0.,        0.,            0.,       0.,&
         &0.,        0.,            0.,       0.,&
         &0.,        0.,            0.,       0./

    data (DGCASP(J,1),J=1,23)/&
         &0.09817,-0.21337,-0.04562,-0.01215, 0.08277,-0.06625,&
         &0.00325,-0.13091,-0.12473,-0.09976, 0.17935,&
         &-0.21337,-0.609774,-.12473,     0.0,     0.0,-0.609774,&
         &0.0,     0.0,      0.0,     0.0,   0.0, 0.17935/
    data (DGCASP(J,2),J=1,19)/&
         &0.00270,  0.00270, -0.04290, -0.609774,  -0.12473,        0.,&
         &-0.02355, -0.15989, -0.20925,   0.06497,        0.,        0.,&
         &0.,       0.,       0.,        0.,        0.,   0.00270,&
         &0./

    data (DGSASP(J,1),J=1,23)/&
         &0.03876, 0.03430, 0.06287,-0.04595, 0.10987, 0.05534,&
         &0.12993,-0.06038,-0.06862, 0.01192, 0.13363,&
         &0.03430,-0.01752,-0.06862,     0.0,     0.0,-0.017520,&
         &0.0,    0.0,      0.0,     0.0,    0.0, 0.13363/
    data (DGSASP(J,2),J=1,19)/&
         &-0.01606, -0.01606,  0.06124,  -0.01752,  -0.06862,        0.,&
         &-0.00393,  0.03982,  0.01463,   0.20328,        0.,        0.,&
         &0.,       0.,       0.,        0.,        0.,  -0.01606,&
         &0./

    data (DGSLOP(J,1),J=1,23)/&
         &-0.17888, 0.33523, 0.78176, 1.17025, 0.04966, 0.11931,&
         &0.46546, 0.65622, 0.30070,-0.06637, 0.07628,&
         &0.33523,-2.05706, 0.30070,     0.0,     0.0,-2.05706,&
         &0.0,      0.0,     0.0,     0.0,    0.0, 0.07628/
    data (DGSLOP(J,2),J=1,19)/&
         &-0.20011, -0.20011, -0.39500,  -2.05706,   0.30070,        0.,&
         &-0.21964,  0.06898, -0.03350,  -0.95238,        0.,        0.,&
         &0.,       0.,       0.,        0.,        0.,  -0.20011,&
         &0./

    data (DGSLSQ(J,1),J=1,23)/&
         &0.0,-0.70216,-1.12380,-1.52006,     0.0,     0.0,&
         &-0.58014,-0.90143,-0.62224,-0.43720,     0.0,&
         &-0.70216, 2.113263,-0.62224,    0.0,     0.0, 2.113263,&
         &0.0,      0.0,     0.0,    0.0,     0.0,      0.0/
    data (DGSLSQ(J,2),J=1,19)/&
         &0.,       0.,       0.,  2.113263,  -0.62224,        0.,&
         &0., -0.34251, -0.11164,   0.61813,        0.,        0.,&
         &0.,       0.,       0.,        0.,        0.,        0.,&
         &0./

    data (DGEL(J,1),J=1,23)/&
         &0.03517, 0.03730, 0.02591, 0.00917, 0.02863,-0.00175,&
         &-0.00480, 0.06259, 0.06313, 0.03229, 0.08518,&
         &0.03730,     0.0, 0.06313,     0.0,     0.0,     0.0,&
         &0.0,     0.0,     0.0,     0.0,     0.0, 0.08518/
    data (DGEL(J,2),J=1,19)/&
         &-0.00565, -0.00565, -0.00196,        0.,   0.06313,        0.,&
         &-0.01234,  0.08677, -0.00885,  -0.07453,        0.,        0.,&
         &0.,       0.,       0.,        0.,        0.,  -0.00565,&
         &0./

    data (DGEL2(J,1),J=1,23)/&
         &-0.000467,-0.000433,-0.000377,-0.000117,-0.000422,-0.000067,&
         &-0.000058,-0.000709,-0.000676,-0.000422,-0.000943,&
         &-0.000433,      0.0,-0.000676,      0.0,       0.0,     0.0,&
         &0.0,      0.0,      0.0,      0.0,       0.0,-0.000943/
    data (DGEL2(J,2),J=1,19)/&
         &0.,       0.,       0.,        0., -0.000676,        0.,&
         &0., -0.00072, -0.00002,   0.00060,        0.,        0.,&
         &0.,       0.,       0.,        0.,        0.,        0.,&
         &0./

    data MAPCCF/&
         &12*3, 1, 3*3, 1, 4*3, 2,2, 7*3,&
         &11*3, 1, 6*3, 2,3,1,3,3,1, 6*3,&
         &6*4, 1,2, 3*4, 2,1, 3*4, 3, 3*4, 1,3,2, 4*4, 2,4,4,&
         &12*3, 1, 9*3, 2,2, 6*3,&
         &30*1,&
         &13*3, 1,2,2,3,3,2,3,3,1, 8*3,&
         &4,4,1,4,3,4,2,1,4,1, 3*4, 6*2, 4,1,4,1, 3*4, 1, 3*4,&
         &11*4, 3,3, 3*1, 3,3,1,4,2,4,1, 3*4, 1, 3*4,&
         &12*3, 7*1, 3,1,1,2,2,1,3,1,1,3,3,&
         &2,2,3,1,4,3,3,1,2,4,4,1, 4*4, 1, 13*4,&
         &30*1,&
         &11*3, 1, 6*3, 2,3,1,3,3,1, 6*3,&
         &30*1,&
         &12*3, 7*1, 3,1,1,2,2,1,3,1,1,3,3,&
         &30*1,&
         &30*1,&
         &30*1,&
         &30*1,&
         &30*1,&
         &30*1,&
         &30*1,&
         &30*1,&
         &30*1/

    data DGCCFA/&
         &-0.02430,-0.24886,-0.01079,     0.0,     0.0,&
         &-0.10144,-0.14793,-0.05438,     0.0,     0.0,&
         &-0.09046,-0.11884,-0.05529,-0.02180,     0.0,&
         &-0.09624,-0.19544,-0.05119,     0.0,     0.0,&
         &0.0,     0.0,     0.0,     0.0,     0.0,&
         &-0.05054,-0.15356,-0.09396,     0.0,     0.0,&
         &-0.05576,-0.14919,-0.40640,-0.11400,     0.0,&
         &-0.01547,-0.38386,-0.05371,-0.15159,     0.0,&
         &-0.01598,-0.04477,-0.07392,     0.0,     0.0,&
         &-0.10416,-0.88809,-0.25938,-0.14726,     0.0,&
         &-0.10744,     0.0,     0.0,     0.0,     0.0,&
         &-0.10144,-0.14793,-0.05438,     0.0,     0.0,&
         &-0.199592,    0.0,     0.0,     0.0,     0.0,&
         &-0.01598,-0.04477,-0.07392,     0.0,     0.0,&
         &0.0,     0.0,     0.0,     0.0,     0.0,&
         &0.0,     0.0,     0.0,     0.0,     0.0,&
         &-0.199592,    0.0,     0.0,     0.0,     0.0,&
         &0.0,     0.0,     0.0,     0.0,     0.0,&
         &0.0,     0.0,     0.0,     0.0,     0.0,&
         &0.0,     0.0,     0.0,     0.0,     0.0,&
         &0.0,     0.0,     0.0,     0.0,     0.0,&
         &0.0,     0.0,     0.0,     0.0,     0.0,&
         &-0.10744,     0.0,     0.0,     0.0,     0.0/

    data MAPCCE/12*3, 7*1, 3,1,1,2,2,1,3,1,1,3,3/

    data DGCCFE/-0.01598, -0.04477, -0.07392/

    data DGLCCF/&
         &-0.25717, -0.25717, -0.11974,        0.,        0.,        0.,&
         &-0.12332,       0.,       0.,        0.,        0.,        0.,&
         &0.,       0.,       0.,        0.,        0.,  -0.25717,&
         &0./

    data DGPCC1/&
         &0.,        0., -0.00211,        0.,        0.,        0.,&
         &-0.00120,  -0.00155, -0.00164,  -0.00297,        0.,        0.,&
         &0.,        0.,       0.,        0.,        0.,        0.,&
         &0./

    data DGPCC2/&
         &0.,        0., -0.00232,        0.,        0.,        0.,&
         &-0.00145,  -0.00123, -0.00232,  -0.00254,        0.,        0.,&
         &0.,        0.,       0.,        0.,        0.,        0.,&
         &0./

    data DGPCCF/&
         &0.,       0.,  0.000003,       0.,       0.,       0.,&
         &0.000002, 0.000002, -0.000003, 0.000005,       0.,       0.,&
         &0.,       0.,        0.,       0.,       0.,       0.,&
         &0./


    if (LCONS) go to 1000

    ! Check whether debugging is enabled
    call DBCHK (DEBUG,'DGF',3,ICYC)
    if (IB.eq.2) then
       if (DEBUG) write(JOSTND,3)ICYC
    end if
    3 format(' ENTERING SUBROUTINE DGF  CYCLE =',I5)

    !  DEBUG OUTPUT: MODEL COEFFICIENTS.

    if (DEBUG) then
       if (IB.eq.1) then
          write(JOSTND,9000) DGCON,DGDSQ,DGLD(1:MAXSP,IB),DGCR(1:MAXSP,IB),&
               &DGCRSQ(1:MAXSP,IB),DGCCF,DGBAL(1:MAXSP,IB)
       else
          write(JOSTND,9002) DGCON,DGDSQ,DGLD(1:MAXSP,IB),DGCR(1:MAXSP,IB),&
               &DGCRSQ(1:MAXSP,IB),DGCCF,DGBAL(1:MAXSP,IB)
       end if
    end if
    9000 format(/,' DGCON',/12(1X,F9.5),/11(1X,F9.5),/,' DGDSQ',/12(1X,F9.5&
         &),/11(1X,F9.5),/,' DGLD',/12(1X,F9.5),/11(1X,F9.5),/,' DGCR',&
         &/12(1X,F9.5),/11(1X,F9.5),/,' DGCRSQ',/12(1X,F9.5),/11(1X,F9.5),&
         &/,' DGCCF',/12(1X,F9.5),/11(1X,F9.5),/,' DGBAL',/12(1X,F9.5),&
         &/11(1X,F9.5))
    9002 format(/,' DGCON',/12(1X,F9.5),/7(1X,F9.5),/,' DGDSQ',/12(1X,F9.5)&
         &,/7(1X,F9.5),/,' DGLD',/12(1X,F9.5),/7(1X,F9.5),/,' DGCR',&
         &/12(1X,F9.5),/7(1X,F9.5),/,' DGCRSQ',/12(1X,F9.5),/7(1X,F9.5),&
         &/,' DGCCF',/12(1X,F9.5),/7(1X,F9.5),/,' DGBAL',/12(1X,F9.5),&
         &/7(1X,F9.5))

    ! CALL BADIST AND COMPUTE THE AGE RANGE

    AGERNG = 1000.
    YOUNG = 1000.
    OLD = 0.0
    call BADIST(DEBUG)

    do I = 1,ITRN
       if (ABIRTH(I).le.1.)go to 5
       if (HT(I) .le.4.5) go to 5
       if (ABIRTH(I) .lt. YOUNG) YOUNG = ABIRTH(I)
       if (ABIRTH(I) .gt. OLD) OLD = ABIRTH(I)
       5 continue
       AGERNG = OLD - YOUNG
       AGERNG = ABS(AGERNG)
    end do

    call SDICAL(0,SDIMAX)

    !  SCALE BASAL AREA.

    BA100 = BA/100.

    !  BEGIN SPECIES LOOP.  ASSIGN VARIABLES WHICH ARE SPECIES DEPENDENT

    if (IB.eq.2) then
       ALCCF = 0.0
       if (RELDEN .gt.0.0)ALCCF = ALOG(RELDEN)
    end if
    do ISPC = 1,MAXSP
       I1 = ISCT(ISPC,1)
       if (I1.eq.0) go to 20
       I2 = ISCT(ISPC,2)
       CONSPP = DGCON(ISPC) + COR(ISPC) + 0.01*DGCCF(ISPC)*RELDEN

       !  EM SPECIES EXPANSION-CONSPP IS DIFFERENT FOR LM

       if (IB.eq.2.and. ISPC.eq.4)CONSPP = CONSPP + .01*(-.199592)*RELDEN

       DGLDS = DGLD(ISPC,IB)
       DGBALS = DGBAL(ISPC,IB)
       DGCRS = DGCR(ISPC,IB)
       DGCRS2 = DGCRSQ(ISPC,IB)
       DGDSQS = DGDSQ(ISPC)
       DGDBLS = DGDBAL(ISPC,IB)

       !  SPECIES EXPANSION-SITE INDEX ASSIGNED IS DEPENDENT UPON HABITAT TYPE
       !  IN SUBROUTINE **SITSET**

       SI = SITEAR(ISPC)

       !  BEGIN TREE LOOP WITHIN SPECIES ISPC.

       do I3 = I1,I2
          I = IND1(I3)
          D = DIAM(I)
          if (D.le.0.0) goto 10
          ALD = ALOG(D)
          BARK = BRATIO(ISPC,D,HT(I))
          if (IB.eq.2) then
             CR = ICR(I)*0.01
             BAL = (1.0 - (PCT(I)/100.)) * BA
             IPCCF = ITRE(I)
          end if

          select case (KGR(ISPC,IB))

          case(3)

             !  PI/JU LOGIC FROM UT

             DPP = D
             if (DPP .lt.1.0) DPP = 1.0
             BATEM = BA
             if (BATEM .lt.1.0) BATEM = 1.0
             DF = DGFJUN(DPP,BATEM,SI)
             if (DF .lt. DPP) DF = DPP
             DIAGR = (DF - DPP) * BARK
             if (DIAGR .le.0.) then
                DDS = -9.21
             else
                DDS = ALOG( (DIAGR * (2.0 * DPP * BARK + DIAGR)) ) + CONSPP
             end if

          case(4)

             !  ASPEN LOGIC FROM UTAH

             CR = ICR(I)
             call DGFASP(D,ASPDG,CR,BARK,SI,DEBUG)
             DDS = ASPDG + ALOG(COR2(ISPC)) + COR(ISPC)

          case(5)

             !  CO LOGIC FROM CR
             !--DBHMAX MOVED FROM SITSET

             ICLS = IFIX(D + 1.0)
             if (ICLS .gt.41) ICLS = 41
             if (IB.eq.1) CR = ICR(I)*0.01
             DPP = D
             if (DPP .lt.1.0) DPP = 1.0
             DF = DGFCOA(DPP,BA,SI)
             if (DF .gt.36.) DF = 36.
             if (DF .lt. DPP) DF = DPP
             DIAGR = (DF - DPP) * BARK
             if (DIAGR .le.0.) then
                DDS = -9.21
             else
                DDS = ALOG( (DIAGR * (2.0 * DPP * BARK + DIAGR)) )
                if (DDS .lt. -9.21) DDS = -9.21
             end if
             DDS = DDS+COR(ISPC)+DGCON(ISPC)

          case(1)

             ! ORIGINAL NI SECTION

             if (IB.eq.1) CR = ICR(I)*0.01
             BAL = (1.0-(PCT(I)/100.))*BA100
             DDS = CONSPP + DGLDS*ALD + DGBALS*BAL + CR*(DGCRS+CR*DGCRS2)&
                  &+DGDSQS*D*D  + DGDBLS*BAL/(ALOG(D+1.0))

          case(2)

             ! ORIGINAL EM SECTION
             ! DUM1 AND DUM2 ARE RELDEN DUMMYS.  THEY ARE 0 OR 1 DEPENDING ON WHETHER
             ! OR NOT THE STAND IS MANAGED.

             DUM1 = 0.0
             DUM2 = 1.0
             if (MANAGD .eq.1) then
                DUM1 = 1.0
                DUM2 = 0.0
             end if
             DDS = CONSPP + DGLDS*ALD + DGBALS*BAL + CR*(DGCRS+CR*DGCRS2)&
                  &+ DGDSQS*D*D  + DGDBLS*BAL/(ALOG(D+1.0))&
                  &+ DGPCCF(ISPC)*(RELDEN**2)&
                  &+ DGLCCF(ISPC) * ALCCF + DGPCC1(ISPC)*DUM1*PCCF(IPCCF)&
                  &+ DGPCC2(ISPC)*DUM2*PCCF(IPCCF)

             if (DEBUG) write(JOSTND,8000) DGCON(ISPC),COR(ISPC),&
                  &DGCCF(ISPC),RELDEN,DGLCCF(ISPC),RMAI,CONSPP,DGLDS,ALD,&
                  &DGBALS,BAL,CR,DGCRS,DGCRS2,DGDSQS,D,DGDBLS,&
                  &DGPCCF(ISPC),PCCF(IPCCF),DDS,DGLCCF(ISPC),ALCCF
             8000 format(/,' IN DGF AT LINE 178',9F12.5,/,1H ,8F12.5,/,&
                  &1H ,5F12.5,//)
          end select

          if (DDS.lt.-9.21) DDS = -9.21
          WK2(I) = DDS

          !  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.

          if (DEBUG) then
             if (IB.eq.1) then
                write(JOSTND,9001) I,ISPC,D,BAL,CR,RELDEN,BA,DDS
             else
                write(JOSTND,9003) I,ISPC,D,BAL,CR,RELDEN,BA,DDS
             end if
          end if
          9001 format(' IN DGF, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,&
               &',  BAL=',F7.2,',  CR=',F7.4/&
               &'          CCF=',F9.3,',  BA=',F9.3,',   LN(DDS)=',F7.4)
          9003 format(' IN DGF, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,&
               &',  BAL=',F7.2,',  CR=',F7.4/&
               &'       RELDEN=',F9.3,',  BA=',F9.3,',   LN(DDS)=',F7.4)
          10 continue
       end do

       !  END OF SPECIES LOOP.

       20 continue
    end do
    if (IB.eq.2) then
       if (DEBUG)write(JOSTND,7000)ICYC
    end if
    7000 format(' LEAVING SUBROUTING DGF  CYCLE =',I5)
    return

    !  ENTRY **DGCONS**: LOAD COEFFICIENTS OF THE DIAMETER INCREMENT
    !  MODEL THAT ARE SITE SPECIFIC AND NEED ONLY BE RESOLVED ONCE.
    !  ITYPE IS THE HABITAT TYPE INDEX AS COMPUTED IN **HABTYP**.
    !  ASPECT IS STAND ASPECT IN RADIANS.  OBSERV CONTAINS THE NUMBER OF
    !  OBSERVATIONS BY HABITAT CLASS BY SPECIES FOR THE UNDERLYING
    !  MODEL (THIS DATA IS ACTUALLY USED BY **DGDRIV** FOR CALIBRATION).

    1000 continue
    if (IB.eq.2) then
       JDTYPE = IDTYPE
       if (IDTYPE .gt.117) JDTYPE = 30
    end if

    !  ENTER LOOP TO LOAD SPECIES DEPENDENT VECTORS.

    do ISPC = 1,MAXSP

       !  PUT SITE INTO ONE OF 5 CLASSES

       XSITE = SITEAR(ISPC)
       LSI = INT(XSITE/10.)
       if (LSI.lt.2)ISIC = 1
       if (LSI.ge.5)ISIC = 5
       if (LSI.ge.3.and. LSI.lt.4)ISIC = 3
       if (LSI.ge.4.and. LSI.lt.5)ISIC = 4
       if (LSI.ge.2.and. LSI.lt.3)ISIC = 2

       !  EM'S ORIGINAL EQUATION SPECIES USE THE DETAILED HABITAT INDEX AND
       !  SLOPE IN PERCENT/10 (NULLIFIED FOR OS BECAUSE OF POOR EXTRAPOLATION).

       XSLOPE = SLOPE
       if (IB.eq.2.and. KGR(ISPC,IB).eq.2) then
          ISPHAB = MAPHAB(JDTYPE,ISPC,IB)
          XSLOPE = SLOPE / 10.0
          if (ISPC .eq.18)XSLOPE = 0.0
       else
          ISPHAB = MAPHAB(ITYPE,ISPC,IB)
       end if
       ISPFOR = MAPLOC(IFOR,ISPC,IB)
       ISPDSQ = MAPDSQ(IFOR,ISPC,IB)
       TMPASP = ASPECT
       if (IB.eq.1) then
          ISPCCF = MAPCCF(ITYPE,ISPC)
          if (ISPC.eq.13.or.ISPC.ge.15.and.ISPC.le.18.or.ISPC.eq.20&
               &.or.ISPC.eq.21)TMPASP = TMPASP-.7854
       else
          ISPCCF = MAPCCE(ITYPE)
          if (ISPC.eq.4.or. ISPC.eq.6.or. ISPC.eq.12&
               &.or.ISPC.eq.17)TMPASP = TMPASP-.7854
       end if
       DGCON(ISPC) = DGHAB(ISPHAB,ISPC,IB)&
            &+ DGFOR(ISPFOR,ISPC,IB)&
            &+ DGEL(ISPC,IB) * ELEV&
            &+ DGEL2(ISPC,IB) * ELEV * ELEV&
            &+(DGSASP(ISPC,IB) * SIN(TMPASP)&
            &+ DGCASP(ISPC,IB) * COS(TMPASP)&
            &+ DGSLOP(ISPC,IB)) * XSLOPE&
            &+ DGSLSQ(ISPC,IB) * XSLOPE * XSLOPE
       DGDSQ(ISPC) = DGDS(ISPDSQ,ISPC,IB)
       if (IB.eq.1) then
          DGCCF(ISPC) = DGCCFA(ISPCCF,ISPC)
          if (ISPC .le.12.or. ISPC .eq.14.or. ISPC .eq.23) then
             ATTEN(ISPC) = OBSERV(ISPHAB,ISPC,IB)
          else
             ATTEN(ISPC) = OBSERV(ISIC,ISPC,IB)
             if (ISPC.eq.13.or.ISPC.eq.17) then
                DGCON(ISPC) = DGCON(ISPC)+.001766*XSITE
             else if (ISPC.eq.18.or.ISPC.eq.20.or.ISPC.eq.21) then
                DGCON(ISPC) = DGCON(ISPC)+.006460*XSITE
             end if
          end if
       else
          DGCCF(ISPC) = 0.0
          if (ISPC.eq.4) then
             DGCCF(ISPC) = -0.199592
             ATTEN(ISPC) = OBSERV(ISIC,ISPC,IB)
          else if (ISPC.eq.5) then
             DGCCF(ISPC) = DGCCFE(ISPCCF)
             ATTEN(ISPC) = OBSERV(ISPHAB,ISPC,IB)
          else if (ISPC.eq.12.or. ISPC.eq.17) then
             ATTEN(ISPC) = OBSERV(ISIC,ISPC,IB)
          else
             ATTEN(ISPC) = OBSERV(1,ISPC,IB)
          end if
          if (ISPC.eq.4) then
             DGCON(ISPC) = DGCON(ISPC)+.001766*XSITE
          else if (ISPC.eq.12.or. ISPC.eq.17) then
             DGCON(ISPC) = DGCON(ISPC)+.006460*XSITE
          end if
       end if
       SMCON(ISPC) = 0.

       !  IF READCORD OR REUSCORD WAS SPECIFIED (LDCOR2 IS TRUE) ADD
       !  LN(COR2) TO THE BAI MODEL CONSTANT TERM (DGCON).  COR2 IS
       !  INITIALIZED TO 1.0 IN BLKDATA.

       if (LDCOR2.and.COR2(ISPC).gt.0.0) DGCON(ISPC) = DGCON(ISPC)&
            &+ ALOG(COR2(ISPC))
    end do
    return
  end subroutine DGFIE

  subroutine DGFCI(LCONS)
    implicit none

    !  CI: ITS OWN NATIVE EQUATIONS, BANKS AND DIAGNOSTICS.

    logical :: LCONS
    !  DIMENSIONS FOR INTERNAL VARIABLES.

    !     DIAM -- ARRAY OF TREE DIAMETERS (PASSED AS AN ARGUMENT).
    !    DGLBA -- COEFFICIENTS FOR LOG(BASAL AREA)
    !     DGBA -- COEFFICIENTS FOR BASAL AREA
    !    DGBAL -- COEFFICIENTS FOR BASAL AREA IN LARGER TREES
    !     DGLD -- COEFFICIENTS FOR LOG(DIAMETER)
    !     DGCR -- COEFFICIENTS FOR CROWN RATIO
    !   DGCRSQ -- COEFFICIENTS FOR CROWN RATIO SQUARED
    !     DGEL -- COEFFICIENTS FOR ELEVATION
    !    DGEL2 -- COEFFICIENTS FOR ELEVATION SQUARED
    !   DGCASP -- COEFFICIENTS FOR COSINE(ASPECT)*SLOPE
    !   DGSASP -- COEFFICIENTS FOR SINE(ASPECT)*SLOPE
    !   DGSLOP -- COEFFICIENTS FOR SLOPE
    !   DGSLSQ -- COEFFICIENTS FOR SLOPE SQUARED
    !   DGDBAL -- COEFFICIENTS FOR INTERACTION
    !             BETWEEN BASAL AREA IN LARGER TREES AND LN(DBH+1)
    !   DGPCCF -- COEFFICIENTS FOR POINT CROWN COMPETETION FACTOR
    !   DGCCFA -- COEFFICIENTS FOR STAND CROWN COMPETETION FACTOR
    !   DGHAB  -- CONSTANTS FOR HABITAT TYPE
    !   DGFOR  -- CONSTANTS FOR FOREST LOCATION
    !    DGDS  -- COEFFICIENTS FOR DIAMETER SQUARED
    !  MAPLOC  -- MAPPING FUNCTION FOREST --> FOREST LOCATION CONSTANT
    !  OBSERV  -- NUMBER OF OBSERVATIONS USED TO DEVELOP THE SPECIES
    !             SPECIFIC EQUATION.
    !  ICHBCL  -- MAPPING FUNCTION HABITAT TYPE --> HAB TYPE CONSTANT

    ! Variables
    logical :: DEBUG
    integer :: MAPLOC(6,19),OBSERV(19),ICHBCL(130,19)
    integer :: I,I1,I2,I3,IPCCF,MAPHAB,ISPC
    integer :: IBSERV(6,3),LSI,ISIC,ICLS
    real    :: DGLD(19),DGCR(19),DGCRSQ(19),DGDBAL(19),DGHAB(12,19),DGFOR(3,&
         &19),DGDS(19),DGEL2(19),DGSASP(19),DGCASP(19),DGSLOP(19),DGLBA(19),DGBA(19),&
         &DGPCCF(19),DGEL(19),DGSLSQ(19),DGBAL(19),DGCCFA(19)
    real    :: CONSPP,DGLDS,DGCRS,DGDSQS,DGDBLS,DGLBAS,DGPCFS,DGBAS
    real    :: D,CR,BAL,PBAL,DDS,DHAB,DGCRS2,BRATIO
    real    :: TMPASP,XSITE,TEMEL,DGBALS,BARK,SI,ASPDG,DPP,DF,DIAGR,BATEM
    real    :: DGFCOA,DGFJUN

    !     SPECIES LIST FOR CENTRAL IDAHO VARIANT.

    !     1 = WESTERN WHITE PINE (WP)          PINUS MONTICOLA
    !     2 = WESTERN LARCH (WL)               LARIX OCCIDENTALIS
    !     3 = DOUGLAS-FIR (DF)                 PSEUDOTSUGA MENZIESII
    !     4 = GRAND FIR (GF)                   ABIES GRANDIS
    !     5 = WESTERN HEMLOCK (WH)             TSUGA HETEROPHYLLA
    !     6 = WESTERN REDCEDAR (RC)            THUJA PLICATA
    !     7 = LODGEPOLE PINE (LP)              PINUS CONTORTA
    !     8 = ENGLEMANN SPRUCE (ES)            PICEA ENGELMANNII
    !     9 = SUBALPINE FIR (AF)               ABIES LASIOCARPA
    !    10 = PONDEROSA PINE (PP)              PINUS PONDEROSA
    !    11 = WHITEBARK PINE (WB)              PINUS ALBICAULIS
    !    12 = PACIFIC YEW (PY)                 TAXUS BREVIFOLIA
    !    13 = QUAKING ASPEN (AS)               POPULUS TREMULOIDES
    !    14 = WESTERN JUNIPER (WJ)             JUNIPERUS OCCIDENTALIS
    !    15 = CURLLEAF MOUNTAIN-MAHOGANY (MC)  CERCOCARPUS LEDIFOLIUS
    !    16 = LIMBER PINE (LM)                 PINUS FLEXILIS
    !    17 = BLACK COTTONWOOD (CW)            POPULUS BALSAMIFERA VAR. TRICHOCARPA
    !    18 = OTHER SOFTWOODS (OS)
    !    19 = OTHER HARDWOODS (OH)

    !  SURROGATE EQUATION ASSIGNMENT:

    !  FROM THE IE VARIANT:
    !      USE 17(PY) FOR 12(PY)             (IE17 IS REALLY TT2=LM)
    !      USE 18(AS) FOR 13(AS)             (IE18 IS REALLY UT6=AS)
    !      USE 13(LM) FOR 11(WB) AND 16(LM)  (IE13 IS REALLY TT2=LM)
    !      USE 19(CO) FOR 17(CW) AND 19(OH)  (IE19 IS REALLY CR38=OH)

    !  FROM THE UT VARIANT:
    !      USE 12(WJ) FOR 14(WJ)
    !      USE 20(MC) FOR 15(MC)             (UT20 = SO30=MC, WHICH IS
    !                                                  REALLY WC39=OT)

    data (ICHBCL(I,1),I = 1,130)/&
         &12*1,2*2,5*3,5*4,2*3,4,10*0,5*4,18*0,5*5,6*6,4*4,55*0/
    data (ICHBCL(I,2),I = 1,130)/&
         &69*1,61*0/
    data (ICHBCL(I,3),I = 1,130)/&
         &37*1,9*2,14*3,2*2,2*1,3,1,5,2*3,2*6,4*8,27*7,28*0/
    data (ICHBCL(I,4),I = 1,130)/&
         &62*0,2*1,0,3,4,1,3,2*5,2*3,2*6,55*0/
    data (ICHBCL(I,5),I = 1,130)/&
         &62*0,2*1,0,3,4,1,3,2*5,2*3,2*6,55*0/
    data (ICHBCL(I,6),I = 1,130)/&
         &69*1,61*0/
    data (ICHBCL(I,7),I = 1,130)/&
         &60*1,4*10,2,5,3,6*5,11*6,14*7,2*8,9,8,5*11,3*9,20*0/
    data (ICHBCL(I,8),I = 1,130)/&
         &61*0,4*1,2,3,2*4,6*5,5*3,4*5,11*3,35*0/
    data (ICHBCL(I,9),I = 1,130)/&
         &65*1,4*2,6*1,5*0,6*1,9*0,2*1,0,1,0,4,1,9*4,2*1,2*0,3*1,12*0/
    data (ICHBCL(I,10),I = 1,130)/&
         &12*1,2*2,5*3,5*4,2*3,4,10*0,5*4,18*0,5*5,6*6,4*4,55*0/
    data (ICHBCL(I,12),I = 1,130)/&
         &130*0/
    data (ICHBCL(I,13),I = 1,130)/&
         &130*0/
    data (ICHBCL(I,14),I = 1,130)/&
         &130*0/
    data (ICHBCL(I,15),I = 1,130)/&
         &130*0/
    data (ICHBCL(I,16),I = 1,130)/&
         &130*0/
    data (ICHBCL(I,17),I = 1,130)/&
         &130*0/
    data (ICHBCL(I,18),I = 1,130)/&
         &60*1,4*10,2,5,3,6*5,11*6,14*7,2*8,9,8,5*11,3*9,20*0/
    data (ICHBCL(I,19),I = 1,130)/&
         &130*0/

    data DGLBA/&
         &-.257322,  -.121785,  -.196025,  -.217923,  -.217923,&
         &-.121785,  -.178038,  -.204602,  -.113511,  -.257322,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,  -.178038,        0./

    data DGBA/&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,  -.000926,       0.0,&
         &0.0,       0.0,       0.0,       0.0, -0.001265,&
         &0.0,       0.0,       0.0,       0.0/

    data DGLD/&
         &.822203,   .725693,  1.045093,  1.286963,  1.286963,&
         &.725693,   .900528,  1.241561,   .968212,   .822203,&
         &0.213947,  0.213947,       0.0,       0.0,  0.889596,&
         &0.213947,       0.0,   .900528,       0.0/

    data DGCR/&
         &1.768935,  2.362259,  1.658968,  1.175105,  1.175105,&
         &2.362259,  1.913105,   .506551,   .626180,  1.768935,&
         &1.523464,  1.523464,       0.0,       0.0,  1.732535,&
         &1.523464,       0.0,  1.913105,       0.0/

    data DGCRSQ/&
         &-.176164, -1.719086,  -.361716,   .219013,   .219013,&
         &-1.719086,  -.258206,   .762026,   .688924,  -.176164,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,  -.258206,       0.0/

    data DGEL/&
         &0.0,  -.000064,   .003527,   .005045,   .005045,&
         &-.000064,   .006343,       0.0,   .010618,       0.0,&
         &0.0,       0.0,       0.0,       0.0, -0.075986,&
         &0.0,       0.0,   .006343,       0.0/

    data DGEL2/&
         &0.0,  -.000012,  -.000095,  -.000093,  -.000093,&
         &-0.000012,  -.000096,       0.0,  -.000194,       0.0,&
         &0.0,       0.0,       0.0,       0.0,  0.001193,&
         &0.0,       0.0,  -.000096,       0.0/

    data DGCASP/&
         &.127311,  -.120816,   .059683,  -.004469,  -.004469,&
         &-.120816,   .081123,   .013843,  -.137792,   .127311,&
         &-0.609774, -0.609774,       0.0,       0.0,  0.085958,&
         &-0.609774,       0.0,   .081123,       0.0/

    data DGSASP/&
         &.076531,   .008765,   .023297,   .009335,   .009335,&
         &.008765,   .066831,  -.157633,  -.073830,   .076531,&
         &-0.01752,  -0.01752,       0.0,       0.0,  -0.86398,&
         &-0.01752,       0.0,   .066831,       0.0/

    data DGSLOP/&
         &.024336,  -.293028,  -.133723,  -.033374,  -.033374,&
         &-.293028,   .000452,  1.271265,   .161578,   .024336,&
         &-2.05706,  -2.05706,       0.0,       0.0,       0.0,&
         &-2.05706,       0.0,   .000452,       0.0/

    data DGSLSQ/&
         &-.781480,       0.0,  -.484626,  -.418343,  -.418343,&
         &0.0,  -.207088, -1.842644,  -.050890,  -.781480,&
         &2.113263,  2.113263,       0.0,       0.0,       0.0,&
         &2.113263,       0.0,  -.207088,       0.0/

    data DGDBAL/&
         &-.006065,  -.003403,  -.002579,  -.000578,  -.000578,&
         &-.003403,  -.001641,  -.000638,       0.0,  -.006065,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,  -.001641,       0.0/

    data DGPCCF/&
         &0.0,  -.000521,  -.000422,  -.000512,  -.000512,&
         &-.000521,       0.0,  -.000210,  -.000601,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0/

    data DGBAL/&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &-0.358634, -0.358634,       0.0,       0.0,       0.0,&
         &-0.358634,       0.0,       0.0,       0.0/

    data DGCCFA/&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &-0.199592, -0.199592,       0.0,       0.0,       0.0,&
         &-0.199592,       0.0,       0.0,       0.0/

    data  OBSERV/&
         &8420,     3091,    16232,    21277,    21277,&
         &3091,    20986,     5517,     6533,     8420,&
         &0,        0,        0,        0,      220,&
         &0,        0,    20986,        0/

    data IBSERV/&
         ! ROW 1 USED FOR: 11=WB, 12=PY, 16=LM
         &27,    70,   123,   101,    33,     0,&
         ! ROW 2 USED FOR: 13=AS
         &184,   429,   356,   162,    74,     0,&
         ! ROW 3 USED FOR: 14=WJ, 17=CW AND 19=OH
         &1000,  1000,  1000,  1000,  1000,  1000/

    !  SPECIES 1
    data (DGHAB(I,1),I = 1,12)/&
         &0.0,  0.006074,  0.181590, -0.196098, -0.055780,&
         &0.133907,  0.045857,     5*0.0/
    !  SPECIES 2
    data (DGHAB(I,2),I = 1,12)/&
         &0.0,  0.098727, -0.066114,  0.174716,     8*0.0/
    !  SPECIES 3
    data (DGHAB(I,3),I = 1,12)/&
         &0.0,  0.143953,  0.054519,  0.216254, -0.160600,&
         &0.085439,  0.337503, -0.041782,  0.016502,     3*0.0/
    !  SPECIES 4
    data (DGHAB(I,4),I = 1,12)/&
         &0.0, -0.091074,  0.051189, -0.057982, -0.157537,&
         &0.094401, -0.198203,     5*0.0/
    !  SPECIES 5
    data (DGHAB(I,5),I = 1,12)/&
         &0.0, -0.091074,  0.051189, -0.057982, -0.157537,&
         &0.094401, -0.198203,     5*0.0/
    !  SPECIES 6
    data (DGHAB(I,6),I = 1,12)/&
         &0.0,  0.098727, -0.066114,  0.174716,     8*0.0/
    !  SPECIES 7
    data (DGHAB(I,7),I = 1,12)/&
         &0.0,  0.155479,  0.255922,  0.543851,  0.468293,&
         &0.412304,  0.367085,  0.171232,  0.262057,  0.048266,&
         &0.301184,  0.542920/
    !  SPECIES 8
    data (DGHAB(I,8),I = 1,12)/&
         &0.0,  0.108200,  0.332189,  0.161025,  0.376800,&
         &0.229605,     6*0.0/
    !  SPECIES 9
    data (DGHAB(I,9),I = 1,12)/&
         &0.0,  0.151514,  0.092675,  0.318444, -0.038935,&
         &7*0.0/
    !  SPECIES 10
    data (DGHAB(I,10),I = 1,12)/&
         &0.0,  0.006074,  0.181590, -0.196098, -0.055780,&
         &0.133907,  0.045857,     5*0.0/
    !  SPECIES 11
    data (DGHAB(I,11),I = 1,12)/&
         &12*0.0/
    !  SPECIES 12
    data (DGHAB(I,12),I = 1,12)/&
         &12*0.0/
    !  SPECIES 13
    data (DGHAB(I,13),I = 1,12)/&
         &12*0.0/
    !  SPECIES 14
    data (DGHAB(I,14),I = 1,12)/&
         &12*0.0/
    !  SPECIES 15
    data (DGHAB(I,15),I = 1,12)/&
         &12*0.0/
    !  SPECIES 16
    data (DGHAB(I,16),I = 1,12)/&
         &12*0.0/
    !  SPECIES 17
    data (DGHAB(I,17),I = 1,12)/&
         &12*0.0/
    !  SPECIES 18
    data (DGHAB(I,18),I = 1,12)/&
         &0.0,  0.155479,  0.255922,  0.543851,  0.468293,&
         &0.412304,  0.367085,  0.171232,  0.262057,  0.048266,&
         &0.301184,  0.542920/
    !  SPECIES 19
    data (DGHAB(I,19),I = 1,12)/&
         &12*0.0/

    !  DGFOR CONTAINS LOCATION CLASS CONSTANTS FOR EACH SPECIES.
    !  MAPLOC IS AN ARRAY WHICH MAPS FOREST ONTO A LOCATION CLASS.

    data MAPLOC/&
         &1, 2, 2, 3, 2, 2,&
         &1, 2, 2, 2, 2, 2,&
         &1, 2, 2, 3, 2, 2,&
         &1, 2, 2, 1, 2, 2,&
         &1, 2, 2, 1, 2, 2,&
         &1, 2, 2, 2, 2, 2,&
         &1, 2, 2, 3, 2, 2,&
         &1, 1, 1, 1, 1, 1,&
         &1, 2, 2, 3, 2, 2,&
         &1, 2, 2, 3, 2, 2,&
         &1, 1, 1, 1, 1, 1,&
         &1, 1, 1, 1, 1, 1,&
         &1, 1, 1, 1, 1, 1,&
         &1, 1, 1, 1, 1, 1,&
         &1, 1, 1, 1, 1, 1,&
         &1, 1, 1, 1, 1, 1,&
         &1, 1, 1, 1, 1, 1,&
         &1, 2, 2, 3, 2, 2,&
         &1, 1, 1, 1, 1, 1/
    data DGFOR/&
         &2.017817, 1.851178, 1.879603,&
         &1.445729, 1.550427, 0.0,&
         &1.116401, 1.004272, 1.035798,&
         &0.691340, 0.977362, 0.0,&
         &0.691340, 0.977362, 0.0,&
         &1.445729, 1.550427, 0.0,&
         &0.473751, 0.425722, 0.523818,&
         &0.255416, 0.0,      0.0,&
         &0.442641, 0.475575, 0.593639,&
         &2.017817, 1.851178, 1.879603,&
         &1.568742,      0.0,      0.0,&
         &1.568742,      0.0,      0.0,&
         &0.0,      0.0,      0.0,&
         &0.0,      0.0,      0.0,&
         &-0.107648,      0.0,      0.0,&
         &1.568742,      0.0,      0.0,&
         &0.0,      0.0,      0.0,&
         &0.473751, 0.425722, 0.523818,&
         &0.0,      0.0,      0.0/

    !  DGDS CONTAINS COEFFICIENTS FOR THE DIAMETER SQUARED TERMS
    !  IN THE DIAMETER INCREMENT MODELS; ARRAYED BY SPECIES.

    data DGDS/&
         &-0.0004163, -0.0003576, -0.0004562, -0.0004408, -0.0004408,&
         &-0.0003576, -0.0009134, -0.0003958, -0.0002569, -0.0004163,&
         &-0.0006538, -0.0006538,        0.0,        0.0,        0.0,&
         &-0.0006538,        0.0, -0.0009134,        0.0/

    if (LCONS) go to 1000

    ! Check whether debugging is enabled
    call DBCHK (DEBUG,'DGF',3,ICYC)

    !  BEGIN SPECIES LOOP.  ASSIGN VARIABLES WHICH ARE SPECIES DEPENDENT

    do ISPC = 1,MAXSP
       I1 = ISCT(ISPC,1)
       if (I1.eq.0) go to 20
       I2 = ISCT(ISPC,2)
       CONSPP = DGCON(ISPC) + COR(ISPC) + 0.01*DGCCFA(ISPC)*RELDEN
       DGLDS = DGLD(ISPC)
       DGBALS = DGBAL(ISPC)
       DGCRS = DGCR(ISPC)
       DGCRS2 = DGCRSQ(ISPC)
       DGDSQS = DGDSQ(ISPC)
       DGDBLS = DGDBAL(ISPC)
       DGLBAS = DGLBA(ISPC)
       DGPCFS = DGPCCF(ISPC)
       DGBAS = DGBA(ISPC)
       SI = SITEAR(ISPC)

       !  BEGIN TREE LOOP WITHIN SPECIES ISPC.

       do I3 = I1,I2
          I = IND1(I3)
          IPCCF = ITRE(I)
          D = DIAM(I)
          if (D.le.0.0) goto 10
          CR = ICR(I)*0.01
          select case (ISPC)
          case(11,12,16)
             BAL = (1.0 - (PCT(I)/100.)) * BA/100.
          case default
             BAL = (1.0 - (PCT(I)/100.)) * BA
          end select
          PBAL = (1.0 - (PCT(I)/100.)) * PTBAA(ITRE(I))
          BARK = BRATIO(ISPC,D,HT(I))

          select case (ISPC)

             !  13=ASPEN; FROM IE(18)=UT(6)

          case (13)
             CR = ICR(I)
             call DGFASP(D,ASPDG,CR,BARK,SI,DEBUG)
             DDS = ASPDG + ALOG(COR2(ISPC)) + COR(ISPC)

             !  14=WJ; FROM UT(12)

          case (14)
             DPP = D
             if (DPP .lt.1.0) DPP = 1.0
             BATEM = BA
             if (BATEM .lt.1.0) BATEM = 1.0
             DF = DGFJUN(DPP,BATEM,SI)
             if (DF .lt. DPP) DF = DPP
             DIAGR = (DF - DPP) * BARK
             if (DIAGR .le.0.) then
                DDS = -9.21
             else
                DDS = ALOG( (DIAGR * (2.0 * DPP * BARK + DIAGR)) ) + CONSPP
                if (DDS .lt. -9.21) DDS = -9.21
             end if

             !  15=MC; FROM UT(20)=SO(30)=WC(39)

          case (15)
             if (DEBUG)write(JOSTND,*)' BAL,BA,PCT(I)= ',BAL,BA,PCT(I)
             DDS = CONSPP +DGLDS*ALOG(D) + DGBALS*BAL + CR*(DGCRS+CR*DGCRS2)&
                  &+DGDSQS*D*D  + DGBAS*BAL/(ALOG(D+1.0))&
                  &-0.000981*BA

             !  17=CW AND 19=OH; FROM IE(19)=CR(38)

          case(17,19)
             ICLS = IFIX(D + 1.0)
             if (ICLS .gt.41) ICLS = 41
             CR = ICR(I)*0.01
             DPP = D
             if (DPP .lt.1.0) DPP = 1.0
             DF = DGFCOA(DPP,BA,SI)
             if (DF .gt.36.) DF = 36.
             if (DF .lt. DPP) DF = DPP
             DIAGR = (DF - DPP) * BARK
             if (DIAGR .le.0.) then
                DDS = -9.21
             else
                DDS = ALOG( (DIAGR * (2.0 * DPP * BARK + DIAGR)) )
                if (DDS .lt. -9.21) DDS = -9.21
             end if
             DDS = DDS+COR(ISPC)+DGCON(ISPC)

             !  SPECIES USING ORIGINAL CI EQNS, PLUS THOSE FROM IE THAT HAVE THE
             !  SAME FUNCTIONAL FORM: 11=WB, 12=PY, AND 16=LM,

          case default
             DDS = CONSPP +DGLDS*ALOG(D) + DGLBAS*ALOG(BA)&
                  &+ DGDSQS*D*D + DGDBLS*PBAL/(ALOG(D+1.0))&
                  &+ DGBAS*BAL/(ALOG(D+1.0)) + DGPCFS*PCCF(IPCCF)&
                  &+ CR*(DGCRS+CR*DGCRS2)+ DGBALS*BAL
          end select

          if (DDS.lt.-9.21) DDS = -9.21
          WK2(I) = DDS

          !  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.

          if (.not.DEBUG) go to 10
          write(JOSTND,9001) I,ISPC,D,BAL,CR,PCCF(IPCCF),BA,&
               &PBAL,DDS
          9001 format(' IN DGF, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,&
               &',  BAL=',F7.2,',  CR=',F7.4/&
               &'     PCCF=',F9.3,',  BA=',F9.3,',  PBAL=',F9.3,&
               &',   LN(DDS)=',F7.4)
          10 continue
       end do

       !  END OF SPECIES LOOP.

       20 continue
    end do
    return

    1000 continue

    !  ENTRY POINT FOR LOADING COEFFICIENTS OF THE DIAMETER INCREMENT
    !  MODEL THAT ARE SITE SPECIFIC AND NEED ONLY BE RESOLVED ONCE.
    !  ICINDX IS A CI HABITAT TYPE INDEX COMPUTED IN **HABTYP**.
    !  ASPECT IS STAND ASPECT.  OBSERV CONTAINS THE NUMBER OF
    !  OBSERVATIONS BY SPECIES FOR THE UNDERLYING
    !  MODEL (THIS DATA IS ACTUALLY USED BY **DGDRIV** FOR CALIBRATION).

    ! CHECK FOR DEBUG.

    call DBCHK(DEBUG,'DGF',3,ICYC)
    if (DEBUG)&
         &write(JOSTND,*)' ICINDX',ICINDX,'SLOPE',SLOPE,'ELEV',ELEV,'IFOR',&
         &IFOR,'ASP',ASPECT

    !  ENTER LOOP TO LOAD SPECIES DEPENDENT VECTORS.

    do ISPC = 1,MAXSP

       !  PUT SITE INTO ONE OF 5 CLASSES

       XSITE = SITEAR(ISPC)
       LSI = INT(XSITE/10.)
       if (LSI.lt.2)ISIC = 1
       if (LSI.ge.5)ISIC = 5
       if (LSI.ge.3.and. LSI.lt.4)ISIC = 3
       if (LSI.ge.4.and. LSI.lt.5)ISIC = 4
       if (LSI.ge.2.and. LSI.lt.3)ISIC = 2

       MAPHAB = ICHBCL(ICINDX,ISPC) + 1
       DHAB = DGHAB(MAPHAB,ISPC)
       ISPFOR = MAPLOC(IFOR,ISPC)

       select case (ISPC)
       case(11,12,13,16)
          TMPASP = ASPECT - 0.7854
       case default
          TMPASP = ASPECT
       end select

       select case (ISPC)
       case(15)
          TEMEL = ELEV
          if (TEMEL.gt.30.)TEMEL = 30.
       case default
          TEMEL = ELEV
       end select

       DGCON(ISPC) = DHAB&
            &+ DGFOR(ISPFOR,ISPC)&
            &+ DGEL(ISPC) * TEMEL&
            &+ DGEL2(ISPC) * TEMEL * TEMEL&
            &+(DGSASP(ISPC) * SIN(TMPASP)&
            &+ DGCASP(ISPC) * COS(TMPASP)&
            &+ DGSLOP(ISPC)) * SLOPE&
            &+ DGSLSQ(ISPC) * SLOPE * SLOPE

       select case (ISPC)
       case(11,12,16)
          DGCON(ISPC) = DGCON(ISPC)+.001766*XSITE
       case(13)
          DGCON(ISPC) = DGCON(ISPC)+.006460*XSITE
       case(15)
          DGCON(ISPC) = DGCON(ISPC)+0.227307*ALOG(XSITE)
       end select

       if (DEBUG)&
            &write(JOSTND,*)' CONSTANT COMPONENTS ISPC = ',ISPC,&
            &'DGCON',DGCON(ISPC),'DHAB',DHAB,'DHAB',DHAB,'DGFOR',&
            &DGFOR(ISPFOR,ISPC),'DGEL',DGEL(ISPC),'DGEL2',DGEL2(ISPC),&
            &' SINASP',DGSASP(ISPC),'COSASP',DGCASP(ISPC),&
            &'DGSLOP',DGSLOP(ISPC),'DGSLOSQ',DGSLSQ(ISPC),&
            &'XSITE',XSITE

       select case (ISPC)
       case(11,12,16)
          ATTEN(ISPC) = IBSERV(ISIC,1)
       case(13)
          ATTEN(ISPC) = IBSERV(ISIC,2)
       case(14,17,19)
          ATTEN(ISPC) = IBSERV(ISIC,3)
       case default
          ATTEN(ISPC) = OBSERV(ISPC)
       end select

       DGDSQ(ISPC) = DGDS(ISPC)
       SMCON(ISPC) = 0.

       !  IF READCORD OR REUSCORD WAS SPECIFIED (LDCOR2 IS TRUE) ADD
       !  LN(COR2) TO THE BAI MODEL CONSTANT TERM (DGCON).  COR2 IS
       !  INITIALIZED TO 1.0 IN BLKDATA.

       if (LDCOR2.and.COR2(ISPC).gt.0.0) DGCON(ISPC) = DGCON(ISPC)&
            &+ ALOG(COR2(ISPC))
       30 continue
    end do
    return
  end subroutine DGFCI

  subroutine DGFKT(LCONS)
    implicit none

    !  KT: ITS OWN NATIVE EQUATIONS, BANKS AND DIAGNOSTICS.

    logical :: LCONS
    !  DIMENSIONS FOR INTERNAL VARIABLES.

    !     DGLD -- ARRAY CONTAINING COEFFICIENTS FOR THE LOG(DIAMETER)
    !             TERM IN THE DDS MODEL (ONE COEFFICIENT FOR EACH
    !             SPECIES).
    !     DGCR -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
    !             RATIO TERM IN THE DDS MODEL (ONE COEFFICIENT FOR
    !             EACH SPECIES).
    !   DGCRSQ -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
    !             RATIO SQUARED TERM IN THE DDS MODEL (ONE
    !             COEFFICIENT FOR EACH SPECIES).
    !    DGBAL -- ARRAY CONTAINING COEFFICIENTS FOR THE BASAL AREA IN
    !             LARGER TREES TERM IN THE DDS MODEL. NOT USED IN THIS
    !             VARIANT (ONE COEFFICIENT FOR EACH SPECIES).
    !   DGDBAL -- ARRAY CONTAINING COEFFICIENTS FOR THE INTERACTION
    !             BETWEEN BASAL AREA IN LARGER TREES AND LN(DBH) (ONE
    !             COEFFICIENT PER SPECIES).
    !    DGCCF -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
    !             COMPETITION FACTOR TERM IN THE DDS MODEL (ONE
    !             COEFFICIENT FOR EACH SPECIES, LOADED IN RCON).
    !    DGLBA -- ARRAY CONTAINING COEFFICIENTS FOR THE LOG(BASAL AREA)
    !             TERM IN THE DDS MODEL (ONE COEFFICIENT FOR EACH SPECIES).
    !    DGPCC1 - ARRAY CONTAINING THE POINT CROWN COMPETITION FACTOR
    !             FOR MANAGED STANDS (ONE COEFFICIENT FOR EACH SPECIES).
    !    DGPCC2 - ARRAY CONTAINING THE POINT CROWN COMPETITION FACTOR
    !             FOR UNMANAGED STANDS (ONE COEFFICIENT FOR EACH SPECIES).
    !    DGCCFSQ- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
    !             COMPETITION FACTOR SQUARED TERM IN THE DDS MODEL (ONE
    !             COEFFICIENT FOR EACH SPECIES).
    !    DBHCH -- ARRAY FOR THE DIAMETER LIMIT UNTIL CROWN RATIO IS LIMITED
    !    ICRLIM - MAXIMUM CROWN RATIO ALLOWED FOR TREES WITH DIAMETER
    !             GREATER THAN OR EQUAL TO DBHCH

    ! Variables
    logical :: DEBUG
    real    :: DGLD(11),DGCR(11),DBHCH(11)
    real    :: DGCRSQ(11),DGDBAL(11),DGHAB(9,11),DGFOR(7,11)
    real    :: DGDS(11),DGEL(11),DGEL2(11),DGSASP(11)
    real    :: DGCASP(11),DGSLOP(11),DGSLSQ(11)
    real    :: DGLBA(11),DGPCC1(11),DGPCC2(11),CCFSQ(11)
    real    :: DGCCFA(11),DGD2,CRMAX
    integer :: ICRLIM(11),MAPHAB(175,11),MAPLOC(10,11)
    integer :: OBSERV(9,11)
    integer :: ISPC,I1,I2,I3,I,IPCCF
    real    :: CONSPP,DGLDS,DGCRS,DGCRS2,DGDBLS,DGCCF2,DGLBAS,D,DUM1,DUM2
    real    :: DGPC1,DGPC2,PCCF1,CCF2,ALD,CR,BAL,DDS
    data DGLD/&
         &0.89068, 0.71363, 0.91484, 1.24554, 1.03000, 1.00243,&
         &0.90678, 0.98560, 1.06324, 0.85286, 0.89778/
    data DGCR/&
         &1.33383, 1.57307, 2.32586, 1.21753, 1.04060, 1.62559,&
         &1.79329, 1.35330, 0.75412, 2.71069, 1.28403/
    data DGCRSQ/&
         &-0.46964,-0.56399,-1.02799, 0.00000, 0.00000,-0.25015,&
         &-0.40740,-0.18967, 0.46269,-1.27309,0.000000/
    data DGDBAL/&
         &-0.00706,-0.01026,-0.00697,-0.00483,-0.00404,-0.00430,&
         &-0.00479,-0.00614,-0.00443,-0.00527,-0.66110/
    data CCFSQ/&
         &0.000000, 0.000008, 0.000007, 0.000003, 0.000000, 0.000006,&
         &0.000000, 0.000004, 0.000006, 0.000000, 0.000000/
    data DGPCC1/&
         &0.000072, 0.000000,-0.000413,-0.001050,-0.000558,-0.001027,&
         &0.000770,-0.000565,-0.000050,-0.000113, 0.000000/
    data DGPCC2/&
         &-0.000810, 0.000000,-0.001173,-0.001232,-0.000925,-0.000958,&
         &-0.000697,-0.001045,-0.001134,-0.000981, 0.000000/
    data DGLBA/&
         &-0.26312, 0.00000, 0.00000, 0.00000,-0.00308, 0.09764,-0.07759,&
         &-0.14396,-0.11870,-0.07669, 0.00000/
    data DBHCH/&
         &6*25.0,20.0,3*25.0,99.0/
    data ICRLIM/&
         &7,6,8,8,8,8,7,9,9,7,9/
    data  OBSERV/&
         &82,   758,  3280,   322,     0,     0,     0,     0,     0,&
         &7811,  5028,  2546,  3735,  5002, 15814,  3443,     0,     0,&
         &69,  3306,  5075, 10390,  4735,  3137,  1543,     0,     0,&
         &6744,  1428,   483,    54,     0,     0,     0,     0,     0,&
         &1019,   145,  6295,     0,     0,     0,     0,     0,     0,&
         &2990,   345,  1664,  1398,  3085,     0,     0,     0,     0,&
         &939,  4622,  2717,  8448, 16688,  2869,  6849,  4891,   676,&
         &224,  3217,   759,  4674,  3538,   192,   145,  3031,  4189,&
         &926,  5333,  2140,  6219,   298,  9150,     0,     0,     0,&
         &1188,  1558,  2721,   486,  1719,   405,     0,     0,     0,&
         &55,   880,     0,     0,     0,     0,     0,     0,     0/

    !  DGHAB IS AN ARRAY THAT CONTAINS HABITAT CLASS INTERCEPTS FOR
    !  EACH SPECIES.  MAPHAB IS INDEXED BY ITYPE TO MAP HABITAT
    !  TYPES ONTO HABITAT CLASSES.

    data MAPHAB/&
         &23*1,2,3*1,3*2,3*1,2,5*1,2*2,4*1,2,2*1,2,22*1,4*2,1,2*2,1,&
         &4*2,2*1,7*2,1,2*2,3*3,2,3,2,1,3,2,3,1,4*3,4,2*3,2*4,1,&
         &3,3*1,3,6*1,3,2*1,2*3,2*4,3*1,2*3,4,3,3*1,4,1,4,1,4,4*1,&
         &4,21*1,&
         &11*1,2,1,2,2*1,2,7*1,2,1,2,2*1,2*2,1,2*2,3*1,2,3*1,2*2,4*1,&
         &2*2,1,2,2*1,2*2,1,2,2*1,4,1,7,6,2*1,7,1,2*4,2*1,2*3,2*1,2*3,&
         &4,1,5,2*3,1,3,1,3*5,4,1,3,2*4,3,5,3*6,5,3,4,2*1,5,2*1,3*3,&
         &2*4,6,7,4,6,4,7,3*1,7,5*1,4,6,2*1,6,7,1,7,2*1,6,4,7,2*6,3*1,&
         &4,1,6,4,6,2*1,2*4,9*1,6,2*1,4,9*1,&
         &11*1,2,1,2,2*1,2,2*1,2,3*1,2,3,1,2,4,2*3,2,3,5,2,3,2,2*3,2*5,&
         &2*3,5,3,1,2,3,2*5,4,3,5,3,2*2,4*1,4,3*6,7,1,2,1,4,6,1,4,2,1,&
         &9*4,2,18*4,1,2*4,1,2*4,2,4,2*6,4,6,4,3*6,1,6,7,6,2*1,7,6,&
         &2*1,7,1,2*7,6,7,6,2*7,6,1,3*6,2*4,1,6,1,2*6,4,2*7,1,6,2*7,&
         &5*1,7,4*1,7,6,8*1,&
         &23*1,2,2*1,3*2,1,4*2,1,2*2,2*1,2*2,4*1,3*2,23*1,3,2,6*1,2,6*1,3,&
         &3*1,2*3,1,4,3*1,2*2,1,2,1,2,5*1,2,1,6*2,4*1,2,8*1,5*2,41*1,&
         &72*1,3,21*1,2,6*3,1,3*2,70*1,&
         &11*1,2,11*1,4,3*1,2*2,3*4,1,4,3*2,1,3*4,1,2*4,1,2,1,2*4,2,10*1,&
         &2,1,2,5*1,2,2*1,2*3,4*2,1,6*2,1,3,4,7*3,2*1,5,2*4,5,3*4,5,&
         &2*1,11*3,2*1,2*3,2*1,2*3,4*1,5*3,3*1,7,1,2,5,2,2*1,5,2*1,&
         &3*5,2*1,2*5,6*1,5,14*1,&
         &13*1,2,2*1,3,6*1,4,2,1,2,2*3,2*4,3,5*2,4,2*2,2*3,2*4,1,2*3,2*2,&
         &3,4,1,4,2*2,1,2,2*1,4*4,5,1,4,5,3*4,1,3*6,1,6,5,6,1,2*6,7,&
         &5,6,1,6,2*7,5,2*7,5,1,7,5,7,5,7,6,2*5,1,2*5,2*1,6,7,5,2*4,8,&
         &5,4*4,2*1,2*4,2*1,5,8,2*4,5,1,2*4,5,8,5,3*1,3*4,2*5,4,1,2*4,&
         &5,4,9,2*1,2*5,4,5,5*1,5,2*1,2,4,9*1,4,&
         &27*1,2*3,1,3,2*1,2*2,2*3,2,2*1,3,2*1,2,1,2,1,2,3,10*1,2*3,3*2,1,&
         &2,1,2*3,2*1,2*5,3*1,2,2*1,2,4,2,2*4,1,3,5,3,2*5,4*6,5,2*4,5,&
         &2*4,5,3*4,2*1,2*2,4,7,5,8,5,2*9,2,5,1,7,2*5,2*1,7,2*1,7,8,1,&
         &9,5,9,8,9,2*8,5,7,9,2*2,9,2,1,5,7,5,8,9,2*8,1,2*2,6*1,8,1,9,&
         &2*1,2,2*9,7*1,&
         &27*1,2*2,2*3,4,3,2,3*3,2,3,2*2,1,3,2,1,2,5*3,3*1,2,1,3,2*1,4*2,&
         &2*1,2,1,2*2,2*1,2,4,3*1,4,2*1,2*4,3,4,2*1,4*2,3*5,1,5,10*4,&
         &2*1,3*3,5,2,6,2,2*4,2*2,1,5,4,2,2*1,5,3,2*1,4,1,3,2*6,3,6,1,&
         &2,6,2*4,1,2*6,4,1,5,1,2*3,1,2*3,1,4,6,6*1,3,3*1,5,2,5,4,5*1,&
         &4,1,&
         &13*1,2,1,2*2,2*1,2,2*1,2,2*4,1,4,5,1,2*3,5,4*2,2*3,2*2,5,3*2,1,&
         &3,4,2,3,4,2,2*1,2,5,6*1,2*3,5*1,3,1,2*6,5,6,1,3*6,1,5*5,1,&
         &3*6,5*1,6,1,4*5,3*1,5,3*1,3*3,1,6*3,4*1,3,8*1,5*3,5*1,3*3,&
         &2*1,3,1,2*3,4*1,2*3,21*1,&
         &133*2,5*1,37*2/
    data DGHAB/&
         &0.00000, 0.46877, 0.36827, 0.25380, 0.00000, 0.00000, 0.00000,&
         &0.00000, 0.00000,&
         &0.00000,-0.14413, 0.09463, 0.25452, 0.09901, 0.15839, 0.18736,&
         &0.00000, 0.00000,&
         &0.00000, 0.32102, 0.26245, 0.36975, 0.20189, 0.41958, 0.21150,&
         &0.00000, 0.00000,&
         &0.00000, 0.04934,-0.02840, 0.30154, 0.00000, 0.00000, 0.00000,&
         &0.00000, 0.00000,&
         &0.00000,-0.18536,-0.03054, 0.00000, 0.00000, 0.00000, 0.00000,&
         &0.00000, 0.00000,&
         &0.00000, 0.22308, 0.05241,-0.03541,-0.14473, 0.00000, 0.00000,&
         &0.00000, 0.00000,&
         &0.00000,-0.02517, 0.14033, 0.09364, 0.06392, 0.17607, 0.12434,&
         &0.03253,-0.04246,&
         &0.00000, 0.25430, 0.35826, 0.26282, 0.33856, 0.66511, 0.47824,&
         &0.08994, 0.14762,&
         &0.00000, 0.19307,-0.02105, 0.13296, 0.33120, 0.03795, 0.00000,&
         &0.00000, 0.00000,&
         &0.00000,-0.11238,-0.02699,-.266678, .0483982,.1757643,0.00000,&
         &0.00000, 0.00000,&
         &-1.68033,-1.52111, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
         &0.00000, 0.00000/

    !  DGCCFA CONTAINS COEFFICIENTS FOR THE CCF TERM BY SPECIES

    data DGCCFA/&
         &0.00250,-0.00270,-0.00178,-0.00047, 00000.0,&
         &-0.00225, 0.00000, 0.00000,-0.00055,0.000000,-0.10744/

    !  DGFOR CONTAINS LOCATION CLASS CONSTANTS FOR EACH SPECIES.  MAPLOC
    !  IS AN ARRAY WHICH MAPS GEOGRAPHIC ARES ONTO A LOCATION CLASS.

    data MAPLOC/&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,2,2,3,2,2,3,4,5,3,&
         &1,1,2,3,2,4,2,5,6,7,&
         &1,2,2,2,2,1,1,3,4,1,&
         &1,1,1,2,1,1,1,3,1,1,&
         &1,1,2,3,4,5,6,5,5,5,&
         &1,2,2,3,2,1,4,5,1,4,&
         &1,1,1,2,3,1,3,4,1,1,&
         &1,1,2,1,2,2,2,3,4,4,&
         &1,2,2,2,2,1,2,2,1,2,&
         &1,1,1,1,1,1,1,1,1,1/
    data DGFOR/&
         &1.76061 ,    0.0,     0.0,     0.0,     0.0,     0.0,     0.0,&
         &0.90538, 1.05703, 0.92987, 1.21864, 0.85925,     0.0,     0.0,&
         &0.28515, 0.47449, 0.22243, 0.38244, 0.77000, 0.25263, 0.41726,&
         &-0.16279, 0.01201, 0.21551,-0.45254,     0.0,     0.0,     0.0,&
         &0.58335, 0.05082, 0.76325,     0.0,     0.0,     0.0,     0.0,&
         &0.60677, 0.25734, 0.41586, 0.84058, 0.87222, 0.51366,     0.0,&
         &0.73315, 0.80566, 0.68869, 0.78334, 1.00490,     0.0,     0.0,&
         &0.73648, 0.63397, 0.86675, 1.01332,     0.0,     0.0,     0.0,&
         &0.28606, 0.42202, 0.59260, 0.35594,     0.0,     0.0,     0.0,&
         &1.27975, 1.46166,     0.0,     0.0,     0.0,     0.0,     0.0,&
         &0.0,     0.0,     0.0,     0.0,     0.0,     0.0,     0.0/

    !  DGDS CONTAINS COEFFICIENTS FOR THE DIAMETER SQUARED TERMS
    !  IN THE DIAMETER INCREMENT MODELS.

    data DGDS/&
         &-0.000358,-0.000553,-0.000818,-0.000983,&
         &-0.000546,-0.000252,-0.001237,-0.000551,&
         &-0.001140,-0.000711,-0.000484/

    !  DGEL CONTAINS THE COEFFICIENTS FOR THE ELEVATION TERM IN THE
    !  DIAMETER GROWTH EQUATION.  DGEL2 CONTAINS THE COEFFICIENTS FOR
    !  THE ELEVATION SQUARED TERM IN THE DIAMETER GROWTH EQUATION.
    !  DGSASP CONTAINS THE COEFFICIENTS FOR THE SIN(ASPECT)*SLOPE
    !  TERM IN THE DIAMETER GROWTH EQUATION.  DGCASP CONTAINS THE
    !  COEFFICIENTS FOR THE COS(ASPECT)*SLOPE TERM IN THE DIAMETER
    !  GROWTH EQUATION.  DGSLOP CONTAINS THE COEFFICIENTS FOR THE
    !  SLOPE TERM IN THE DIAMETER GROWTH EQUATION.  DGSLSQ CONTAINS
    !  COEFFICIENTS FOR THE (SLOPE)**2 TERM IN THE DIAMETER GROWTH MODELS.
    !  ALL OF THESE ARRAYS ARE SUBSCRIPTED BY SPECIES.

    data DGCASP/&
         &-0.06355,-0.13645,-0.02412,-0.01643,-0.03073, 0.00779,&
         &-0.07271,-0.18211,-0.10512,-0.18077, 0.17935/
    data DGSASP/&
         &-0.02975,-0.02172, 0.06558, 0.05178,-0.00462, 0.00659,&
         &0.06870, 0.07099,-0.01060,-0.00704, 0.13363/
    data DGSLOP/&
         &0.05705, 0.15248, 0.03603, 0.17354,-0.02011,-0.05252,&
         &-0.31034, 0.22033, 0.26053,-0.61544, 0.07628/
    data DGSLSQ/&
         &-0.30165,-0.40132,-0.50543,-0.35182, 0.16420, 0.18516,&
         &0.00000,-0.45491,-0.47935, 0.00000,0.000000/
    data DGEL/&
         &-0.00600,-0.00740,-0.00563, 0.00159,-0.00430,-0.02275,&
         &0.00281, 0.00634, 0.01539,-0.00515, 0.08518/
    data DGEL2/&
         &0.000000, 0.000069, 0.000000, 0.000000, 0.000000, 0.000281,&
         &-0.000112,-0.000106,-0.000214,-0.000120,-0.000943/

    if (LCONS) go to 1000

    ! Check whether debugging is enabled
    call DBCHK (DEBUG,'DGF',3,ICYC)

    !  DEBUG OUTPUT: MODEL COEFFICIENTS.

    if (DEBUG)&
         &write(JOSTND,9000) DGCON,DGLD,DGCR,DGCRSQ,DGCCF
    9000 format(/11(1X,F10.5))

    !  BEGIN SPECIES LOOP.  ASSIGN VARIABLES WHICH ARE SPECIES DEPENDENT

    do ISPC = 1,MAXSP
       I1 = ISCT(ISPC,1)
       if (I1.eq.0) go to 20
       I2 = ISCT(ISPC,2)
       if (DEBUG)write(JOSTND,*)' DGCON=',DGCON(ISPC),' COR=',COR(ISPC),&
            &'DGCCF=',DGCCF(ISPC),' RELDEN=',RELDEN
       CONSPP = DGCON(ISPC) + COR(ISPC) + DGCCF(ISPC)*RELDEN
       if (ISPC.eq.11) CONSPP = DGCON(ISPC)+COR(ISPC)+.01*DGCCF(ISPC)*RELDEN
       DGLDS = DGLD(ISPC)
       DGCRS = DGCR(ISPC)
       DGCRS2 = DGCRSQ(ISPC)
       DGDBLS = DGDBAL(ISPC)
       DGCCF2 = CCFSQ(ISPC)
       DGLBAS = DGLBA(ISPC)
       DGD2 = DGDS(ISPC)
       CRMAX = (ICRLIM(ISPC)*10.0-5.0)/100

       !  BEGIN TREE LOOP WITHIN SPECIES ISPC.

       do I3 = I1,I2
          I = IND1(I3)
          D = DIAM(I)

          !  THE FOLLOWING CODE SEGMENT THROUGH ENDIF SETS DUM1 TO UNITY FOR A
          !  MANAGED STAND WITH DUM2 ZERO.  VICE VERSA FOR AN UNMANAGED STAND.

          DUM1 = 0.0
          DUM2 = 1.0
          if (MANAGD .eq.1) then
             DUM1 = 1.0
             DUM2 = 0.0
          end if
          DGPC1 = DGPCC1(ISPC)*DUM1
          DGPC2 = DGPCC2(ISPC)*DUM2
          IPCCF = ITRE(I)
          PCCF1 = PCCF(IPCCF)
          CCF2 = RELDEN*RELDEN
          if (D.le.0.0) goto 10
          ALD = ALOG(D)
          CR = ICR(I)*0.01

          !  IF TREES DBH IS GREATER THAN OR EQUAL TO DBHCH, CHECK TREES CROWN
          !  RATIO.  IF CROWN RATIO IS GREATER THAN ICRLIM, SET CROWN RATIO TO
          !  CRLIM FOR THE COMPUTATION OF DIAMETER GROWTH.  THIS BOUNDING OF
          !  CROWN RATIO IS DUE TO THE DIAMETER GROWTH EQUATION BEING HIGHLY
          !  DEPENDENT UPON CROWN RATIO. LARGE TREES WITH LARGE CROWNS WERE
          !  GROWING TOO MUCH.

          !     IF (D .GT. DBHCH(ISPC)) THEN
          !        IF(CR .GT. CRMAX) CR=CRMAX
          !     ENDIF
          BAL = (1.0 - (PCT(I)/100.)) * BA
          if (ISPC .eq.11)BAL = (1.0-(PCT(I)/100.))*BA/100
          DDS = CONSPP + DGLDS*ALD + CR*(DGCRS+CR*DGCRS2)&
               &+ DGDBLS*BAL/(ALOG(D+1.0))+DGCCF2*CCF2+DGD2*D*D+&
               &DGLBAS*ALOG(BA) + DGPC1*PCCF1 + DGPC2*PCCF1

          if (DDS.lt.-9.21) DDS = -9.21
          WK2(I) = DDS
          if (DEBUG)write(JOSTND,*)' I=',I
          if (DEBUG)write(JOSTND,*)' CONSPP=',CONSPP,' DGLDS=',DGLDS,&
               &' ALD=',ALD,' D=',D,' CR=',CR,' DGCRS=',DGCRS,&
               &' DGCRS2=',DGCRS2,' DGDBLS=',DGDBLS,' BAL=',BAL,' DGCCF2=',&
               &DGCCF2,' CCF2=',CCF2,' DGD2=',DGD2,' DGLBAS=',DGLBAS,' BA=',&
               &BA,' DGPC1=',DGPC1,' PCCF1=',PCCF1,' DGPC2=',DGPC2,' DDS=',DDS

          !  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.

          if (.not.DEBUG) go to 10
          write(JOSTND,9001) I,ISPC,D,BAL,CR,RELDEN,BA,DDS
          9001 format(' IN DGF, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,&
               &',  BAL=',F7.2,',  CR=',F7.4/&
               &'          CCF=',F9.3,',  BA=',F9.3,',   LN(DDS)=',F7.4)
          write(JOSTND,*)' CONSPP=',CONSPP,' PCT=',PCT(I),&
               &' DGDBLS=',DGDBLS,' DGCCF2=',DGCCF2,&
               &' DGPC1=',DGPC1,' DGPC2=',DGPC2,'CRMAX=',CRMAX,&
               &' DBHCH=',DBHCH(ISPC)
          10 continue
       end do

       !  END OF SPECIES LOOP.

       20 continue
    end do
    return
    1000 continue

    !  ENTRY POINT FOR LOADING COEFFICIENTS OF THE DIAMETER INCREMENT
    !  MODEL THAT ARE SITE SPECIFIC AND NEED ONLY BE RESOLVED ONCE.
    !  ITYPE IS THE HABITAT TYPE INDEX AS COMPUTED IN **HABTYP**.
    !  ASPECT IS STAND ASPECT IN RADIANS.  OBSERV CONTAINS THE NUMBER OF
    !  OBSERVATIONS BY HABITAT CLASS BY SPECIES FOR THE UNDERLYING
    !  MODEL (THIS DATA IS ACTUALLY USED BY **DGDRIV** FOR CALIBRATION).

    !  ENTER LOOP TO LOAD SPECIES DEPENDENT VECTORS.

    do ISPC = 1,MAXSP
       ISPHAB = MAPHAB(KKTYPE,ISPC)
       ISPFOR = MAPLOC(KOTFOR,ISPC)
       DGCCF(ISPC) = DGCCFA(ISPC)
       DGCON(ISPC) = DGHAB(ISPHAB,ISPC)&
            &+ DGFOR(ISPFOR,ISPC)&
            &+ DGEL(ISPC) * ELEV&
            &+ DGEL2(ISPC) * ELEV * ELEV&
            &+(DGSASP(ISPC) * SIN(ASPECT)&
            &+ DGCASP(ISPC) * COS(ASPECT)&
            &+ DGSLOP(ISPC)) * SLOPE&
            &+ DGSLSQ(ISPC) * SLOPE * SLOPE
       ATTEN(ISPC) = OBSERV(ISPHAB,ISPC)
       SMCON(ISPC) = 0.

       !  IF READCORD OR REUSCORD WAS SPECIFIED (LDCOR2 IS TRUE) ADD
       !  LN(COR2) TO THE BAI MODEL CONSTANT TERM (DGCON).  COR2 IS
       !  INITIALIZED TO 1.0 IN BLKDATA.

       if (LDCOR2.and.COR2(ISPC).gt.0.0) DGCON(ISPC) = DGCON(ISPC)&
            &+ ALOG(COR2(ISPC))
       30 continue
    end do
    return
  end subroutine DGFKT

end subroutine DGF
