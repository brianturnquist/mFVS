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

  !  ONE SHARED DGF FOR ALL VARIANTS.  EACH
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
  include 'METRIC.f90'
  include 'BCPLOT.f90'

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
  if (VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'LS') call DGFCLS(LCONS)
  if (VARIANTCD.eq.'EC') call DGFEC(LCONS)
  if (VARIANTCD.eq.'SO') call DGFSO(LCONS)
  if (VARIANTCD.eq.'NC') call DGFNC(LCONS)
  if (VARIANTCD.eq.'WS') call DGFWS(LCONS)
  if (VARIANTCD.eq.'AK') call DGFAK(LCONS)
  if (VARIANTCD.eq.'SN') call DGFSN(LCONS)
  if (VARIANTCD.eq.'NE') call DGFNE(LCONS)
  if (VARIANTCD.eq.'ON') call DGFON(LCONS)
  if (VARIANTCD.eq.'CR') call DGFCR(LCONS)
  if (VARIANTCD.eq.'BC') call DGFBC(LCONS)
  if (VARIANTCD.eq.'BM') call DGFBM(LCONS)
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
    integer :: ICLS,MAPHAB,ISI,ISC,ISPC,I1,I2,KSI,I3,I,LSI,ISIC,ISIC2,J,K
    real    :: BA100,CONSPP,DGLDS,DGBALS,DGCRS,DGCRS2,DGDSQS,SI,D,CR
    real    :: ASPDG,BAL,DDS,XSITE,DGDBLS
    real    :: ALD,DGLCCF,DGPTCC,BARK,BRATIO,DPP,BATEM
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
    !  NEEDS: AGE RANGE AND STAGNATION, SHARED WITH CR.

    call DGFSTG(DEBUG)

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

  subroutine DGFSTG(DEBUG)
    implicit none

    !  CR, TT AND UT: AGE RANGE (AGERNG) AND STAGNATION MULTIPLIER (DSTAG)
    !  FOR THE CR-STYLE EQUATIONS.  ONLY THESE VARIANTS CALL IT.

    logical :: DEBUG
    integer :: I,II,ISPC
    real    :: YOUNG,OLD,SUMD2,SUMTRE,DPROB,DGQMD,STDSDI,RELSDI

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
  end subroutine DGFSTG

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

  subroutine DGFCLS(LCONS)
    implicit none

    !  CS AND LS: ONE EQUATION FORM WITH EACH VARIANT'S OWN COEFFICIENT
    !  BANK (IB = 1 FOR CS, 96 SPECIES; IB = 2 FOR LS, 68 SPECIES) AND ITS
    !  OWN QMD AND CROWN-RATIO CAPS.  DIAMETER GROWTH IS PREDICTED FROM
    !  DBH, SITE INDEX, CROWN RATIO, AND BASAL AREA PERCENTILES AND QMD
    !  FOR TREES GREATER THAN OR EQUAL TO 5 INCHES DBH.  THE QMD CAPS
    !  CHANGE QMDGE5 IN PLACE, SO LATER SPECIES SEE EARLIER CAPS.

    !  DIAMETER GROWTH EQUATIONS WERE DEVELOPED THROUGH COST SHARE AGREEMENT NO.
    !  10-CS-11132425-258 WITH MICHIGAN TECHNOLOGICAL UNIVERSITY, OCTOBER 2012.

    logical :: LCONS
    integer, parameter :: IB = merge(1, 2, VARIANTCD.eq.'CS')

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

    !  SPECIES AND COEFFICIENT ORDER, CS (IB = 1):

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

    !  SPECIES AND COEFFICIENT ORDER, LS (IB = 2):

    !  NOTE -- commercial hardwood (44=CH) uses black walnut (46=WN)
    !          as surrogate for all coefficients.

    !      1=JP   2=SC    3=RN    4=RP    5=WP
    !      6=WS   7=NS    8=BF    9=BS   10=TA
    !     11=WC  12=EH   13=OS   14=RC   15=BA
    !     16=GA  17=EC   18=SV   19=RM   20=BC
    !     21=AE  22=RL   23=RE   24=YB   25=BW
    !     26=SM  27=BM   28=AB   29=WA   30=WO
    !     31=SW  32=BR   33=CK   34=RO   35=BO
    !     36=NP  37=BH   38=PH   39=SH   40=BT
    !     41=QA  42=BP   43=PB   44=CH   45=BN
    !     46=WN  47=HH   48=BK   49=NC   50=BE
    !     51=ST  52=MM   53=AH   54=AC   55=HK
    !     56=DW  57=HT   58=AP   59=BG   60=SY
    !     61=PR  62=CC   63=PL   64=WI   65=BL
    !     66=DM  67=SS   68=MA

    ! Variables
    logical :: DEBUG

    integer :: I,IK,ISPC,I1,I2,I3,J

    real    :: BAGE5,BAL,BARK,BRATIO,CONSPP,CR,D,DDS,DIAGRO,DIAGRI
    real    :: D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11
    real    :: P,QMDGE5,RELDBH,RELDBHSQ,SDQGE5,T

    real    :: BALC(96,2),CRSQC(96,2),CRWNC(96,2),DBHC(96,2),DBH2C(96,2),&
         &INTERC(96,2),OBSERV(96,2),RDBHC(96,2),RDBHSQC(96,2),SBAC(96,2),SITEC(96,2),VDBHC(96,2)

    !  DATA STATEMENTS:

    !  COEFFICIENTS FOR DDS MODEL

    data (INTERC(J,1),J=1,96)/&
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
    data (INTERC(J,2),J=1,68)/&
         &1.395,     1.395,  0.21748,   0.21748,   4.1821,&
         &3.9303,    3.9303,    3.458,    3.4206,   -0.461,&
         &-0.74827,    3.2309,  0.21748,    3.0982,   2.5313,&
         &2.5313,    1.8151,   2.2302,   -0.3258,  -0.3258,&
         &3.2435,    3.2435,   3.2435,      2.54,   3.6912,&
         &3.2946,    3.2946,   2.7284,   2.47033,   1.9019,&
         &1.9019,    1.9019,   1.9019,    0.4452,  0.49086,&
         &0.49086,  0.058969, 0.058969,  0.058969,   3.4483,&
         &3.3904,    3.3904,   2.6884,    2.9591,   2.9591,&
         &2.9591,    5.3879,    3.022,    3.1876,   3.1876,&
         &3.1876,    3.1876,   3.0308,    3.0308,   1.2352,&
         &3.0308,    3.0308,   3.0308,  -0.28233,   3.8669,&
         &3.0308,    3.0308,   3.0308,    3.8669,   3.8669,&
         &3.8669,    3.3841,   3.0308/

    data (VDBHC(J,1),J=1,96)/&
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
    data (VDBHC(J,2),J=1,68)/&
         &-3.9398,  -3.9398,      0.0,      0.0,  -13.911,&
         &-9.3995,  -9.3995,  -10.103,  -9.4927,      0.0,&
         &0.0,  -11.816,      0.0,  -8.5844,  -10.146,&
         &-10.146,      0.0,  -6.9962,      0.0,      0.0,&
         &-10.737,  -10.737,  -10.737,   -7.304,  -11.035,&
         &-12.691,  -12.691,  -12.059,  -9.9428,  -5.6950,&
         &-5.6950,  -5.6950,  -5.6950,      0.0,      0.0,&
         &0.0,      0.0,      0.0,      0.0,  -8.1878,&
         &-7.1815,  -7.1815,  -7.3181,  -9.5199,  -9.5199,&
         &-9.5199,  -13.259,  -13.627,  -10.287,  -10.287,&
         &-10.287,  -10.287,  -8.1059,  -8.1059,  -5.2144,&
         &-8.1059,  -8.1059,  -8.1059,      0.0,  -8.8704,&
         &-8.1059,  -8.1059,  -8.1059,  -8.8704,  -8.8704,&
         &-8.8704,  -10.282,  -8.1059/

    data (DBHC(J,1),J=1,96)/&
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
    data (DBHC(J,2),J=1,68)/&
         &0.0,      0.0,  0.30253,  0.30253,      0.0,&
         &0.0,      0.0,      0.0,      0.0,  0.42230,&
         &0.33025,      0.0,  0.30253,      0.0,      0.0,&
         &0.0, 0.091526,  0.11324,  0.33549,  0.33549,&
         &0.0,      0.0,      0.0,      0.0,      0.0,&
         &0.0,      0.0,      0.0,      0.0, 0.096247,&
         &0.096247, 0.096247, 0.096247,  0.22472,  0.23430,&
         &0.23430,      0.0,      0.0,      0.0,      0.0,&
         &0.0,      0.0,      0.0,      0.0,      0.0,&
         &0.0,      0.0,      0.0,      0.0,      0.0,&
         &0.0,      0.0,      0.0,      0.0,  0.18300,&
         &0.0,      0.0,      0.0,      0.0,      0.0,&
         &0.0,      0.0,      0.0,      0.0,      0.0,&
         &0.0,      0.0,      0.0/

    data (DBH2C(J,1),J=1,96)/&
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
    data (DBH2C(J,2),J=1,68)/&
         &0.01213,  0.01213, -0.00885, -0.00885,       0.0,&
         &0.0,      0.0,      0.0,      0.0,       0.0,&
         &0.0,      0.0, -0.00885,      0.0,       0.0,&
         &0.0,      0.0, -0.00186, -0.00703,  -0.00703,&
         &0.0,      0.0,      0.0,      0.0,       0.0,&
         &0.0,      0.0,      0.0,      0.0,       0.0,&
         &0.0,      0.0,      0.0, -0.00397,  -0.00371,&
         &-0.00371,  0.01986,  0.01986,  0.01986,       0.0,&
         &0.0,      0.0,      0.0,      0.0,       0.0,&
         &0.0,      0.0,      0.0,      0.0,       0.0,&
         &0.0,      0.0,      0.0,      0.0,  -0.00314,&
         &0.0,      0.0,      0.0,  0.01581,       0.0,&
         &0.0,      0.0,      0.0,      0.0,       0.0,&
         &0.0,      0.0,      0.0/

    data (RDBHC(J,1),J=1,96)/&
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
    data (RDBHC(J,2),J=1,68)/&
         &1.37270,  1.37270,      0.0,      0.0, -0.33396,&
         &-0.58596, -0.58596, -0.63924, -1.43830,      0.0,&
         &0.0,      0.0,      0.0,      0.0,      0.0,&
         &0.0,  1.02090,      0.0, -0.53201, -0.53201,&
         &0.0,      0.0,      0.0,      0.0, -0.34599,&
         &0.0,      0.0,      0.0,      0.0,      0.0,&
         &0.0,      0.0,      0.0,      0.0,      0.0,&
         &0.0,  4.09180,  4.09180,  4.09180,      0.0,&
         &-0.16910, -0.16910,      0.0,      0.0,      0.0,&
         &0.0, -2.44892,      0.0,      0.0,      0.0,&
         &0.0,      0.0,      0.0,      0.0,      0.0,&
         &0.0,      0.0,      0.0,  3.37463,      0.0,&
         &0.0,      0.0,      0.0,      0.0,      0.0,&
         &0.0,      0.0,      0.0/

    data (RDBHSQC(J,1),J=1,96)/&
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
    data (RDBHSQC(J,2),J=1,68)/&
         &-0.17735, -0.17735,      0.0,      0.0,      0.0,&
         &0.0,      0.0,      0.0,      0.0, -0.11080,&
         &-0.08958,      0.0,      0.0,      0.0,      0.0,&
         &0.0, -0.03235,      0.0,      0.0,      0.0,&
         &0.0,      0.0,      0.0,      0.0,      0.0,&
         &0.0,      0.0,      0.0,      0.0, -0.01611,&
         &-0.01611, -0.01611, -0.01611,      0.0,      0.0,&
         &0.0, -0.33415, -0.33415, -0.33415,      0.0,&
         &0.0,      0.0,      0.0,      0.0,      0.0,&
         &0.0,      0.0,      0.0,      0.0,      0.0,&
         &0.0,      0.0,      0.0,      0.0,      0.0,&
         &0.0,      0.0,      0.0, -0.26631,      0.0,&
         &0.0,      0.0,      0.0,      0.0,      0.0,&
         &0.0,      0.0,      0.0/

    data (SBAC(J,1),J=1,96)/&
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
    data (SBAC(J,2),J=1,68)/&
         &0.0,      0.0, -0.00114,  -0.00114,      0.0,&
         &0.0,      0.0, -0.00192, 0.0049528,      0.0,&
         &-0.00118, -0.00166, -0.00114,  -0.00386, -0.00095,&
         &-0.00095,      0.0,      0.0,  -0.00203, -0.00203,&
         &0.0,      0.0,      0.0,       0.0,      0.0,&
         &-0.00210, -0.00210,      0.0,  -0.00228,      0.0,&
         &0.0,      0.0,      0.0,       0.0, -0.00186,&
         &-0.00186,      0.0,      0.0,       0.0,      0.0,&
         &0.0,      0.0,      0.0,       0.0,      0.0,&
         &0.0,      0.0, -0.00288,       0.0,      0.0,&
         &0.0,      0.0, -0.00179,  -0.00179, -0.00439,&
         &-0.00179, -0.00179, -0.00179,       0.0,      0.0,&
         &-0.00179, -0.00179, -0.00179,       0.0,      0.0,&
         &0.0,      0.0, -0.00179/

    data (BALC(J,1),J=1,96)/&
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
    data (BALC(J,2),J=1,68)/&
         &-0.00428, -0.00428, -0.00306,   -0.00306, -0.00412,&
         &-0.00375, -0.00375,      0.0, -0.0067966,      0.0,&
         &0.0,      0.0, -0.00306,        0.0,      0.0,&
         &0.0, -0.00428, -0.00171,        0.0,      0.0,&
         &0.0,      0.0,      0.0,   -0.00223, -0.00259,&
         &0.0,      0.0,      0.0,        0.0, -0.00367,&
         &-0.00367, -0.00367, -0.00367,   -0.00148,      0.0,&
         &0.0,  0.00184,  0.00184,    0.00184, -0.00134,&
         &-0.00311, -0.00311,      0.0,   -0.00169, -0.00169,&
         &-0.00169, -0.00353,      0.0,        0.0,      0.0,&
         &0.0,      0.0,      0.0,        0.0,  0.00331,&
         &0.0,      0.0,      0.0,        0.0, -0.00535,&
         &0.0,      0.0,      0.0,   -0.00535, -0.00535,&
         &-0.00535, -0.00170,      0.0/

    data (CRWNC(J,1),J=1,96)/&
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
    data (CRWNC(J,2),J=1,68)/&
         &0.02997, 0.02997, 0.01947,  0.01947, 0.03382,&
         &0.01635, 0.01635, 0.02677, 0.018393,     0.0,&
         &0.02758, 0.01535, 0.01947,  0.01628, 0.01677,&
         &0.01677,     0.0, 0.03114,  0.03314, 0.03314,&
         &0.05258, 0.05258, 0.05258,  0.01369, 0.01932,&
         &0.01787, 0.01787, 0.01843,  0.04226, 0.03021,&
         &0.03021, 0.03021, 0.03021,      0.0, 0.00770,&
         &0.00770,     0.0,     0.0,      0.0, 0.02367,&
         &0.01543, 0.01543,     0.0,  0.03012, 0.03012,&
         &0.03012,     0.0, 0.04569,  0.04919, 0.04919,&
         &0.04919, 0.04919, 0.00489,  0.00489, 0.02814,&
         &0.00489, 0.00489, 0.00489,  0.01650, 0.02034,&
         &0.00489, 0.00489, 0.00489,  0.02034, 0.02034,&
         &0.02034, 0.03201, 0.00489/

    data (CRSQC(J,1),J=1,96)/&
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
    data (CRSQC(J,2),J=1,68)/&
         &-0.00015, -0.00015,      0.0,      0.0, -0.00012,&
         &0.0,      0.0, -0.00009,      0.0,  0.00012,&
         &-0.00015,      0.0,      0.0,      0.0,      0.0,&
         &0.0,  0.00031, -0.00014, -0.00016, -0.00016,&
         &-0.00038, -0.00038, -0.00038,      0.0,      0.0,&
         &0.0,      0.0,      0.0, -0.00029, -0.00022,&
         &-0.00022, -0.00022, -0.00022,  0.00009,      0.0,&
         &0.0,  0.00008,  0.00008,  0.00008,      0.0,&
         &0.0,      0.0,  0.00020,      0.0,      0.0,&
         &0.0,      0.0, -0.00037, -0.00045, -0.00045,&
         &-0.00045, -0.00045,      0.0,      0.0, -0.00011,&
         &0.0,      0.0,      0.0,      0.0,      0.0,&
         &0.0,      0.0,      0.0,      0.0,      0.0,&
         &0.0, -0.00026,      0.0/

    data (SITEC(J,1),J=1,96)/&
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
    data (SITEC(J,2),J=1,68)/&
         &0.00385,  0.00385,  0.00581,   0.00581,       0.0,&
         &0.0,      0.0,  0.00317, 0.0074198,       0.0,&
         &0.00537,  0.00676,  0.00581,       0.0,   0.01285,&
         &0.01285,      0.0,      0.0,   0.00520,   0.00520,&
         &0.0,      0.0,      0.0,   0.00647,       0.0,&
         &0.00496,  0.00496,  0.00856,   0.01152,       0.0,&
         &0.0,      0.0,      0.0,   0.01305,   0.00724,&
         &0.00724,      0.0,      0.0,       0.0,       0.0,&
         &0.00619,  0.00619,  0.00350,   0.00356,   0.00356,&
         &0.00356,      0.0,  0.00766,       0.0,       0.0,&
         &0.0,      0.0,      0.0,       0.0,   0.00447,&
         &0.0,      0.0,      0.0,       0.0,       0.0,&
         &0.0,      0.0,      0.0,       0.0,       0.0,&
         &0.0,      0.0,      0.0/

    data (OBSERV(J,1),J=1,96)/&
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
    data (OBSERV(J,2),J=1,68)/&
         &1337.0, 1337.0, 3409.0, 3409.0, 1343.0,&
         &986.0,  986.0, 2498.0, 2365.0, 1524.0,&
         &5618.0,  756.0, 3409.0, 4111.0, 3285.0,&
         &3285.0,  416.0, 3300.0, 5133.0, 5133.0,&
         &604.0,  604.0,  604.0,  563.0, 1934.0,&
         &4470.0, 4470.0,  150.0,  303.0, 1848.0,&
         &1848.0, 1848.0, 1848.0, 1605.0,  596.0,&
         &596.0,  333.0,  333.0,  333.0,  899.0,&
         &5660.0, 5660.0, 2649.0, 2298.0, 2298.0,&
         &2298.0,  209.0,  982.0,  309.0,  309.0,&
         &309.0,  309.0,  589.0,  589.0, 5969.0,&
         &589.0,  589.0,  589.0,  677.0, 1422.0,&
         &589.0,  589.0,  589.0, 1422.0, 1422.0,&
         &1422.0, 1028.0,  589.0/

    if (LCONS) go to 1000

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
          SDQGE5 = SDQGE5+P*(D)**2
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

          !  BOUND QMD AND THEN CROWN RATIO FOR SOME SPECIES.

          if (VARIANTCD.eq.'CS') then
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
          else
             select case (ISPC)
             case(1,2,10,37:39,59) ! SC,JP,TA,BH,PH,SH,BG
                if (QMDGE5.gt.13.) QMDGE5 = 13.
             case(11) ! WC
                if (QMDGE5.gt.15.) QMDGE5 = 15.
             case(17,30:33) ! EC,WO,SW,BR,CK
                if (QMDGE5.gt.25.) QMDGE5 = 25.
             case default
             end select

             select case (ISPC)
             case(17) !EC
                if (CR .gt.60.) CR = 60.
             case(60,65) !SY,BL
                if (CR .gt.85.) CR = 85.
             case default
             end select
          end if

          !  RELATIVE DBH

          RELDBH = 0.0
          RELDBHSQ = 0.0
          if (QMDGE5.gt.0.0) RELDBH = D/QMDGE5
          if (QMDGE5.gt.0.0) RELDBHSQ = D*D/QMDGE5

          if (DEBUG) write(JOSTND,*) 'IN DGF-I= ',I,' D=',D,&
               &' RELDBH= ',RELDBH,' SI= ',SITEAR(ISPC),&
               &'BAGE5=',BAGE5,'CR=',CR,' QMDGE5= ',QMDGE5,&
               &' BAL= ',BAL,' CONSPP= ',CONSPP

          !  CALCULATION OF DDS FOR CENTRAL AND LAKE STATES VARIANTS

          DDS = CONSPP&
               &+ INTERC(ISPC,IB)&
               &+ VDBHC(ISPC,IB) * 1./D&
               &+ DBHC(ISPC,IB) * D&
               &+ DBH2C(ISPC,IB) * D * D&
               &+ RDBHC(ISPC,IB) * RELDBH&
               &+ RDBHSQC(ISPC,IB) * RELDBHSQ&
               &+ CRWNC(ISPC,IB) * CR&
               &+ CRSQC(ISPC,IB) * CR*CR&
               &+ SBAC(ISPC,IB) * BAGE5&
               &+ BALC(ISPC,IB) * BAL&
               &+ SITEC(ISPC,IB) * SITEAR(ISPC)

          !  IF DEBUGGING, OUTPUT TERMS IN THE EQUATION

          if (DEBUG) then
             D1 = INTERC(ISPC,IB)
             D2 = VDBHC(ISPC,IB) * 1./D
             D3 = DBHC(ISPC,IB) * D
             D4 = DBH2C(ISPC,IB) * D * D
             D5 = RDBHC(ISPC,IB) * RELDBH
             D6 = RDBHSQC(ISPC,IB) * RELDBHSQ
             D7 = CRWNC(ISPC,IB) * CR
             D8 = CRSQC(ISPC,IB) * CR*CR
             D9 = SBAC(ISPC,IB) * BAGE5
             D10 = BALC(ISPC,IB) * BAL
             D11 = SITEC(ISPC,IB) * SITEAR(ISPC)

             write(JOSTND,9090)D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11,DDS
             9090 format(' IN DGF ,D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11,DDSO=',&
                  &/,15F9.3)
          end if

          !  CALCULATION OF DDS FOR CENTRAL AND LAKE STATES VARIANTS WAS BUILT
          !  DDS OUTSIDE BARK, CONVERT TO INSIDE BARK

          if (DDS.lt.-9.21) DDS = -9.21
          DIAGRO = SQRT(DBH(I)*DBH(I)+EXP(DDS))-DBH(I)
          BARK = BRATIO(ISPC,DBH(I),HT(I))
          DIAGRI = DIAGRO*BARK
          DDS = ALOG(((DBH(I)*BARK+DIAGRI)**2)-(DBH(I)*BARK)**2)
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

    1000 continue

    !  ENTRY POINT FOR LOADING COEFFICIENTS OF THE DIAMETER INCREMENT
    !  MODEL THAT ARE SITE SPECIFIC AND NEED ONLY BE RESOLVED ONCE.
    !  OBSERV CONTAINS THE NUMBER OF
    !  OBSERVATIONS BY SPECIES FOR THE UNDERLYING MODEL
    !  (THIS DATA IS USED BY **DGDRIV** FOR CALIBRATION).

    !  ENTER LOOP TO LOAD SPECIES DEPENDENT VECTORS.

    do ISPC = 1,MAXSP
       DGCON(ISPC) = 0.
       ATTEN(ISPC) = OBSERV(ISPC,IB)
       SMCON(ISPC) = 0.

       !  IF READCORD OR REUSCORD WAS SPECIFIED (LDCOR2 IS TRUE) ADD
       !  LN(COR2) TO THE BAI MODEL CONSTANT TERM (DGCON).  COR2 IS
       !  INITIALIZED TO 1.0 IN BLKDATA.

       if (LDCOR2.and.COR2(ISPC).gt.0.0) DGCON(ISPC) = DGCON(ISPC)&
            &+ ALOG(COR2(ISPC))
    end do

    return
  end subroutine DGFCLS

  subroutine DGFEC(LCONS)
    implicit none

    !  EC: ORIGINAL EC CONIFER EQUATIONS AND EQUATIONS BORROWED FROM WC,
    !  WITH RED ALDER FROM **DGFRA**.

    logical :: LCONS
    !  DIMENSIONS FOR INTERNAL VARIABLES.

    !     DIAM -- ARRAY LOADED WITH TREE DIAMETERS (PASSED AS AN
    !             ARGUEMENT).
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
    !             LARGER TREES TERM IN THE DDS MODEL
    !             (ONE COEFFICIENT FOR EACH SPECIES).
    !   DGDBAL -- ARRAY CONTAINING COEFFICIENTS FOR THE INTERACTION
    !             BETWEEN BASAL AREA IN LARGER TREES AND LN(DBH) (ONE
    !             COEFFICIENT PER SPECIES).
    !    DGCCF -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
    !             COMPETITION FACTOR TERM IN THE DDS MODEL (ONE
    !             COEFFICIENT FOR EACH SPECIES, LOADED IN RCON).

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

    ! Variables
    logical :: DEBUG
    integer :: ISPC,I1,I2,I3,I,IPCCF
    integer :: MAPDSQ(7,32),MAPLOC(7,32)
    real    :: BARK,BRATIO,DIAGR
    real    :: CONSPP,D,CR,BAL,DUMMY,RELHT,DDS,SASP,XSITE,TEMEL
    real    :: DGLD(32),DGDUM(32),DGCR(32),DGCRSQ(32),DGDBAL(32),&
         &DGHCCF(32),DGFOR(6,32),DGDS(4,32),DGEL(32),DGEL2(32),DGSASP(32),DGCASP(32),&
         &DGSLOP(32),DGSLSQ(32),DGCCFA(32),OBSERV(32),DGSITE(32),DGPCCF(32),SL0DUM(32),&
         &DGBA(32)

    data DGLD/&
         &1.32610,  0.609098,  0.855516,  0.980383,   0.58705,&
         &1.042583,  0.554261,  0.823082,  0.816917,  0.665401,&
         &0.722462,  0.580156,  0.879338,  0.879338,  0.904253,&
         &1.042583,  0.879338,  0.816880,  0.889596,  1.024186,&
         &1.024186,       0.0,  0.889596,  0.889596,  0.889596,&
         &0.889596,  0.889596,   1.66609,  0.889596,  0.889596,&
         &0.580156,  0.889596/

    data DGCR/&
         &1.29730,  1.158355,  2.009866,  1.709846,   1.29360,&
         &2.182084,  1.423849,  1.263610,  1.119493,  1.671186,&
         &2.160348,  1.212069,  1.970052,  1.970052,  4.123101,&
         &2.182084,  1.970052,  2.471226,  1.732535,  0.459387,&
         &0.459387,       0.0,  1.732535,  1.732535,  1.732535,&
         &1.732535,  1.732535,       0.0,  1.732535,  1.732535,&
         &1.212069,  1.732535/

    data DGCRSQ/&
         &0.0,       0.0,  -0.44082,       0.0,       0.0,&
         &-0.843518,       0.0,       0.0,       0.0,       0.0,&
         &-0.834196,       0.0,       0.0,       0.0, -2.689340,&
         &-0.843518,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0/

    data DGSITE/&
         &0.86756,  0.351929,  1.119725,  0.323625,       0.0,&
         &0.782092,  0.458662,  0.290959,  0.231960,  0.921987,&
         &0.380416,  0.346907,  0.252853,  0.252853,  0.684939,&
         &0.782092,  0.252853,  0.244694,  0.227307,  1.965888,&
         &1.965888,       0.0,  0.227307,  0.227307,  0.227307,&
         &0.227307,  0.227307,   0.14995,  0.227307,  0.227307,&
         &0.346907,  0.227307/

    data DGDBAL/&
         &-0.00239, -0.004253, -0.003075, -0.000261,  -0.02284,&
         &-0.001323, -0.004803, -0.005163, -0.000702, -0.008065,&
         &-0.004065,       0.0, -0.004215, -0.004215, -0.006368,&
         &-0.001323, -0.004215, -0.005950, -0.001265, -0.010222,&
         &-0.010222,       0.0, -0.001265, -0.001265, -0.001265,&
         &-0.001265, -0.001265,       0.0, -0.001265, -0.001265,&
         &0.0, -0.001265/

    data DGDUM/&
         &0.0,       0.0,       0.0, -0.799079,       0.0,&
         &0.522079,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.522079,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0/

    data DGHCCF/&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,  0.156459,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.156459,       0.0/

    data DGPCCF/&
         &-0.00044, -0.000568, -0.000441, -0.000643,  -0.00094,&
         &-0.001574, -0.000627, -0.000883, -0.001102,   0.00112,&
         &0.0, -0.001221,       0.0,       0.0, -0.000471,&
         &-0.001574,       0.0,       0.0,       0.0, -0.000757,&
         &-0.000757,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &-0.001221,       0.0/

    !  IDTYPE IS A HABITAT TYPE INDEX THAT IS COMPUTED IN **RCON**.
    !  ASPECT IS STAND ASPECT.  OBSERV CONTAINS THE NUMBER OF
    !  OBSERVATIONS BY HABITAT CLASS BY SPECIES FOR THE UNDERLYING
    !  MODEL (THIS DATA IS ACTUALLY USED BY **DGDRIV** FOR
    !  CALIBRATION).

    data  OBSERV/&
         &1185.,      591.,     6249.,     1210.,      100.,&
         &1950.,     1478.,      652.,      723.,     4021.,&
         &4836.,     1370.,      475.,      475.,     1467.,&
         &1950.,      475.,       0.0,      220.,       78.,&
         &78.,      125.,      220.,      220.,      220.,&
         &220.,      220.,     2144.,      220.,      220.,&
         &1370.,      220./

    !  DGCCFA CONTAINS COEFFICIENTS FOR THE CCF TERM BY SPECIES BY
    !  HABITAT CLASS.

    data DGCCFA/&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0, -0.003183,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0/

    !  DGFOR CONTAINS LOCATION CLASS CONSTANTS FOR EACH SPECIES.
    !  MAPLOC IS AN ARRAY WHICH MAPS FOREST ONTO A LOCATION CLASS.

    data MAPLOC/&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,2,1,2,1,1,1,&
         &1,2,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,2,1,1,1,&
         &1,1,2,1,1,1,1,&
         &1,1,1,2,2,1,1,&
         &1,1,1,2,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,2,2,2,1,1,1,&
         &1,2,3,3,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,2,2,2,1,1,1,&
         &1,1,1,2,1,1,1,&
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
         &1,1,1,1,1,1,1,&
         &1,2,3,3,1,1,1,&
         &1,1,1,1,1,1,1/

    data DGFOR/&
         &-4.64535,       0.0,       0.0,       0.0,       0.0,       0.0,&
         &-0.605649,       0.0,       0.0,       0.0,       0.0,       0.0,&
         &-4.081038, -3.965956,       0.0,       0.0,       0.0,       0.0,&
         &-0.441408, -0.538987,       0.0,       0.0,       0.0,       0.0,&
         &1.49419,       0.0,       0.0,       0.0,       0.0,       0.0,&
         &-3.811100, -3.673109,       0.0,       0.0,       0.0,       0.0,&
         &-1.084679, -1.172470,       0.0,       0.0,       0.0,       0.0,&
         &-0.098284,  0.117987,       0.0,       0.0,       0.0,       0.0,&
         &-0.420205, -0.312955,       0.0,       0.0,       0.0,       0.0,&
         &-3.102028,       0.0,       0.0,       0.0,       0.0,       0.0,&
         &-0.147675, -0.298310,       0.0,       0.0,       0.0,       0.0,&
         &-1.407548, -1.131934, -1.539078,       0.0,       0.0,       0.0,&
         &-1.310067,       0.0,       0.0,       0.0,       0.0,       0.0,&
         &-1.310067,       0.0,       0.0,       0.0,       0.0,       0.0,&
         &-1.127977, -1.401865,       0.0,       0.0,       0.0,       0.0,&
         &-3.811100, -3.673109,       0.0,       0.0,       0.0,       0.0,&
         &-1.310067,       0.0,       0.0,       0.0,       0.0,       0.0,&
         &-1.277664,       0.0,       0.0,       0.0,       0.0,       0.0,&
         &-0.107648,       0.0,       0.0,       0.0,       0.0,       0.0,&
         &-7.753469,       0.0,       0.0,       0.0,       0.0,       0.0,&
         &-7.753469,       0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,       0.0,&
         &-0.107648,       0.0,       0.0,       0.0,       0.0,       0.0,&
         &-0.107648,       0.0,       0.0,       0.0,       0.0,       0.0,&
         &-0.107648,       0.0,       0.0,       0.0,       0.0,       0.0,&
         &-0.107648,       0.0,       0.0,       0.0,       0.0,       0.0,&
         &-0.107648,       0.0,       0.0,       0.0,       0.0,       0.0,&
         &-1.33299,       0.0,       0.0,       0.0,       0.0,       0.0,&
         &-0.107648,       0.0,       0.0,       0.0,       0.0,       0.0,&
         &-0.107648,       0.0,       0.0,       0.0,       0.0,       0.0,&
         &-1.407548, -1.131934, -1.539078,       0.0,       0.0,       0.0,&
         &-0.107648,       0.0,       0.0,       0.0,       0.0,       0.0/

    !  DGDS CONTAINS COEFFICIENTS FOR THE DIAMETER SQUARED TERMS
    !  IN THE DIAMETER INCREMENT MODELS    ARRAYED BY FOREST BY
    !  SPECIES.  MAPDSQ IS AN ARRAY WHICH MAPS FOREST ONTO A DBH**2
    !  COEFFICIENT.

    data MAPDSQ/&
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
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1/

    data DGDS/&
         &0.0,       0.0,       0.0,       0.0,&
         &-0.0001683,       0.0,       0.0,       0.0,&
         &-0.000261,       0.0,       0.0,       0.0,&
         &-0.0002189,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,&
         &-0.0003694,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,&
         &-0.0002039,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,&
         &-0.0002468,       0.0,       0.0,       0.0,&
         &-0.0001546,       0.0,       0.0,       0.0,&
         &-0.0000192,       0.0,       0.0,       0.0,&
         &-0.0001323,       0.0,       0.0,       0.0,&
         &-0.0001323,       0.0,       0.0,       0.0,&
         &-0.0003996,       0.0,       0.0,       0.0,&
         &-0.0003694,       0.0,       0.0,       0.0,&
         &-0.0001323,       0.0,       0.0,       0.0,&
         &-0.0002536,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,&
         &-0.0001737,       0.0,       0.0,       0.0,&
         &-0.0001737,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,&
         &-0.00154,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,&
         &-0.0000192,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0/

    !  DGEL CONTAINS THE COEFFICIENTS FOR THE ELEVATION TERM IN THE
    !  DIAMETER GROWTH EQUATION.  DGEL2 CONTAINS THE COEFFICIENTS FOR
    !  THE ELEVATION SQUARED TERM IN THE DIAMETER GROWTH EQUATION.
    !  DGSASP CONTAINS THE COEFFICIENTS FOR THE SIN(ASPECT)*SLOPE
    !  TERM IN THE DIAMETER GROWTH EQUATION.  DGCASP CONTAINS THE
    !  COEFFICIENTS FOR THE COS(ASPECT)*SLOPE TERM IN THE DIAMETER
    !  GROWTH EQUATION.  DGSLOP CONTAINS THE COEFFICIENTS FOR THE
    !  SLOPE TERM IN THE DIAMETER GROWTH EQUATION.  DGSLSQ CONTAINS
    !  COEFFICIENTS FOR THE (SLOPE)**2 TERM IN THE DIAMETER GROWTH
    !  MODELS.  ALL OF THESE ARRAYS ARE SUBSCRIPTED BY SPECIES.

    data DGCASP/&
         &0.38002, -0.156235, -0.092151, -0.059062,  -0.06625,&
         &-0.239156, -0.064328, -0.055587, -0.049761, -0.181022,&
         &0.0, -0.097288,       0.0,       0.0, -0.374512,&
         &-0.239156,       0.0, -0.023186,  0.085958,       0.0,&
         &0.0,       0.0,  0.085958,  0.085958,  0.085958,&
         &0.085958,  0.085958,       0.0,  0.085958,  0.085958,&
         &-0.097288,  0.085958/

    data DGSASP/&
         &-0.17911,  0.258712,  0.029947, -0.128126,   0.05534,&
         &-0.185520, -0.142328,  0.216231,  0.002810, -0.149848,&
         &0.0,  0.037062,       0.0,       0.0, -0.207659,&
         &-0.185520,       0.0,  0.679903, -0.863980,       0.0,&
         &0.0,       0.0, -0.863980, -0.863980, -0.863980,&
         &-0.863980, -0.863980,       0.0, -0.863980, -0.863980,&
         &0.037062, -0.863980/

    data DGSLOP/&
         &-0.81780, -0.635704, -0.309511,  0.240178,   0.11931,&
         &1.466089, -0.097297, -0.000577,  1.160345, -0.252705,&
         &0.421486,  0.089774,       0.0,       0.0,  0.400223,&
         &1.466089,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.089774,       0.0/

    data DGSLSQ/&
         &0.84368,       0.0,       0.0,  0.131356,       0.0,&
         &-1.817050,  0.094464,       0.0, -1.740114,       0.0,&
         &-0.693610,       0.0,       0.0,       0.0,       0.0,&
         &-1.817050,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0/

    data DGEL/&
         &0.0,  0.004379, -0.021091, -0.015087,  -0.00175,&
         &0.023020, -0.001124, -0.014944, -0.009430, -0.005345,&
         &-0.040067,  0.012082,       0.0,       0.0, -0.069045,&
         &0.023020,       0.0,       0.0, -0.075986, -0.012111,&
         &-0.012111,       0.0, -0.075986, -0.075986, -0.075986,&
         &-0.075986, -0.075986,       0.0, -0.075986, -0.075986,&
         &0.012082, -0.075986/

    data DGEL2/&
         &0.0,       0.0,  0.000225,       0.0, -0.000067,&
         &-0.000364,       0.0,       0.0,       0.0,       0.0,&
         &0.000395,       0.0,       0.0,       0.0,  0.000608,&
         &-0.000364,       0.0,       0.0,  0.001193,       0.0,&
         &0.0,       0.0,  0.001193,  0.001193,  0.001193,&
         &0.001193,  0.001193,       0.0,  0.001193,  0.001193,&
         &0.0,  0.001193/

    ! DUMMY FOR SLOPE EQ 0

    data SL0DUM/&
         &0.0, -0.290174,       0.0, -0.174404,       0.0,&
         &-0.360203,       0.0,       0.0, -0.278601,       0.0,&
         &0.0, -0.099908,       0.0,       0.0,       0.0,&
         &-0.360203,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &-0.099908,       0.0/

    !  BA COEFFICIENT FOR SPECIES USING EQNS FROM THE WC VARIANT

    data DGBA/&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0, -0.000173, -0.000173,       0.0,&
         &0.0, -0.000173, -0.000147, -0.000981,       0.0,&
         &0.0,       0.0, -0.000981, -0.000981, -0.000981,&
         &-0.000981, -0.000981,  -0.00204, -0.000981, -0.000981,&
         &0.0, -0.000981/

    !  CHECK FOR DEBUG.

    if (LCONS) go to 1000

    call DBCHK (DEBUG,'DGF',3,ICYC)
    if (DEBUG) write(JOSTND,3)ICYC
    3 format(' ENTERING SUBROUTINE DGF  CYCLE =',I5)

    !  DEBUG OUTPUT: MODEL COEFFICIENTS.

    if (DEBUG) write(JOSTND,*) 'IN DGF,HTCON=',HTCON,&
         &'ELEV=',ELEV,'RELDEN=',RELDEN
    if (DEBUG)&
         &write(JOSTND,9000) DGCON,DGDSQ
    9000 format(/11(1X,F10.5))

    !  BEGIN SPECIES LOOP.  ASSIGN VARIABLES WHICH ARE SPECIES
    !  DEPENDENT

    do ISPC = 1,MAXSP
       I1 = ISCT(ISPC,1)
       if (I1.eq.0) go to 20
       I2 = ISCT(ISPC,2)
       CONSPP = DGCON(ISPC) + COR(ISPC)
       if (ISPC.eq.12.or. ISPC.eq.31)CONSPP = CONSPP-0.000021*RMAI*RELDEN
       select case (ISPC)
       case(4,6,16)
          DUMMY = 1.0
       case default
          DUMMY = 0.0
       end select

       !  BEGIN TREE LOOP WITHIN SPECIES ISPC.

       do I3 = I1,I2
          I = IND1(I3)
          D = DIAM(I)
          if (D.le.0.0) goto 10
          CR = ICR(I)*0.01
          BAL = (1.0 - (PCT(I)/100.)) * BA
          IPCCF = ITRE(I)
          RELHT = 0.0
          if (AVH .gt.0.0) RELHT = HT(I)/AVH
          if (RELHT .gt.1.5)RELHT = 1.5

          select case (ISPC)

             !  SPECIES USING EQUATIONS FROM THE EC VARIANT

          case(1:10,12,16,31)
             DDS = CONSPP + DGLD(ISPC)*ALOG(D)&
                  &+ DGDUM(ISPC)*DUMMY + CR*(DGCR(ISPC) + CR*DGCRSQ(ISPC))&
                  &+ DGDSQ(ISPC)*D*D  + DGDBAL(ISPC)*BAL/(ALOG(D+1.0))&
                  &+ DGPCCF(ISPC)*PCCF(IPCCF)&
                  &+ DGHCCF(ISPC)*RELHT*PCCF(IPCCF)/100.0&
                  &+ DGCCFA(ISPC) *PCCF(IPCCF)*PCCF(IPCCF)/1000.0
             if (ISPC.eq.1)DDS = DDS+0.49649*RELHT

             !  SPECIES USING EQUATIONS FROM THE WC VARIANT

          case(11,13:15,17:21,23:30,32)

             !  THIS FUNCTION OCCASIONALLY GIVES AN UNDERFLOW ERROR. SPLITTING
             !  IT UP INTO TWO PARTS IS A TEMPORARY SOLUTION WHICH WORKS. GD 2/20/97

             DDS = CONSPP + DGLD(ISPC)*ALOG(D)&
                  &+ CR*(DGCR(ISPC) + CR*DGCRSQ(ISPC))&
                  &+ DGDSQ(ISPC)*D*D  + DGDBAL(ISPC)*BAL/(ALOG(D+1.0))&
                  &+ DGPCCF(ISPC)*PCCF(IPCCF) + DGBA(ISPC)*BA
             if (ISPC.eq.11) DDS = DDS - 0.000358*RELHT
             if (ISPC.eq.28) DDS = DDS - 0.00326*BAL

             !  RED ALDER
             !  FUNCTION BOTTOMS OUT AT D=18. DECREASE LINERALY AFTER THAT TO
             !  DG=0 AT D=28, AND LIMIT TO .1 ON LOWER END.  GED 4-15-93.

          case(22)
             BARK = BRATIO(ISPC,D,HT(I))
             call DGFRA(ISPC,D,BARK,DIAGR,DDS)

          end select

          if (DEBUG) write(JOSTND,8000)&
               &I,ISPC,CONSPP,D,BA,CR,BAL,PCCF(IPCCF),RELDEN,HT(I),AVH,DDS
          8000 format(1H0,'IN DGF 8000F',2I5,10F11.4)

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

    1000 continue

    !  ENTRY POINT FOR LOADING COEFFICIENTS OF THE DIAMETER INCREMENT
    !  MODEL THAT ARE SITE SPECIFIC AND NEED ONLY BE RESOLVED ONCE.

    !  CHECK FOR DEBUG.

    call DBCHK (DEBUG,'DGF',3,ICYC)

    !  ENTER LOOP TO LOAD SPECIES DEPENDENT VECTORS.

    do ISPC = 1,MAXSP
       ISPFOR = MAPLOC(IFOR,ISPC)
       ISPDSQ = MAPDSQ(IFOR,ISPC)
       SASP = &
            &(DGSASP(ISPC) * SIN(ASPECT)&
            &+ DGCASP(ISPC) * COS(ASPECT)&
            &+ DGSLOP(ISPC)) * SLOPE&
            &+ DGSLSQ(ISPC) * SLOPE * SLOPE

       select case (ISPC)
       case(1:10,12,16,31)
          if (SLOPE .eq.0.0)SASP = SL0DUM(ISPC)
       end select

       XSITE = SITEAR(ISPC)
       select case (ISPC)
       case (12)
          XSITE = XSITE*3.281
       case (28)
          XSITE = 1-(XSITE/114.24569)**.4444
          if (XSITE.le.0) then
             XSITE = 125.
          else
             XSITE = -37.60812*ALOG(XSITE)
          end if
       case (31)
          XSITE = XSITE*3.281
       end select

       TEMEL = ELEV
       select case (ISPC)
       case (19,23:27,29,30,32)
          if (TEMEL.gt.30.)TEMEL = 30.
       end select

       DGCON(ISPC) = &
            &DGFOR(ISPFOR,ISPC)&
            &+ DGEL(ISPC) * TEMEL&
            &+ DGEL2(ISPC) * TEMEL * TEMEL&
            &+ DGSITE(ISPC)*ALOG(XSITE)&
            &+ SASP

       ! DUMMY FOR SUBALPINE FIR

       if (ISPC .eq.9) DGCON(ISPC) = DGCON(ISPC) + 0.3835

       DGDSQ(ISPC) = DGDS(ISPDSQ,ISPC)
       ATTEN(ISPC) = OBSERV(ISPC)
       SMCON(ISPC) = 0.
       if (DEBUG)write(JOSTND,9030)DGFOR(ISPFOR,ISPC),&
            &DGEL(ISPC),ELEV,DGEL2(ISPC),DGSASP(ISPC),ASPECT,&
            &DGCASP(ISPC),DGSLOP(ISPC),SLOPE,DGSITE(ISPC),&
            &SITEAR(ISPC),DGCON(ISPC),SASP,XSITE
       9030 format(' IN DGF 9030',14F9.5)

       !  IF READCORD OR REUSCORD WAS SPECIFIED (LDCOR2 IS TRUE) ADD
       !  LN(COR2) TO THE BAI MODEL CONSTANT TERM (DGCON).  COR2 IS
       !  INITIALIZED TO 1.0 IN BLKDATA.

       if (LDCOR2.and.COR2(ISPC).gt.0.0) DGCON(ISPC) = DGCON(ISPC)&
            &+ ALOG(COR2(ISPC))
       30 continue
    end do

    return

  end subroutine DGFEC

  subroutine DGFSO(LCONS)
    implicit none

    !  SO: NATIVE GENERAL EQUATION WITH IFOR=10 OVERRIDES; JUNIPER, ASPEN
    !  AND RED ALDER FROM THE SHARED HELPERS.

    ! ES DIAMETER GROWTH EQUATIONS ARE FROM BM VARIANT   6/5/89

    logical :: LCONS
    !  DIMENSIONS FOR INTERNAL VARIABLES.

    !     DIAM -- ARRAY LOADED WITH TREE DIAMETERS (PASSED AS AN
    !             ARGUEMENT).
    !     DGLD -- ARRAY CONTAINING COEFFICIENTS FOR THE LOG(DIAMETER)
    !             TERM IN THE DDS MODEL (ONE COEFFICIENT FOR EACH
    !             SPECIES).
    !     DGCR -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
    !             RATIO TERM IN THE DDS MODEL (ONE COEFFICIENT FOR
    !             EACH SPECIES).
    !   DGCRSQ -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
    !             RATIO SQUARED TERM IN THE DDS MODEL (ONE
    !             COEFFICIENT FOR EACH SPECIES).
    !    DGBA  -- ARRAY CONTAINING COEFFICIENTS FOR BASAL AREA
    !             TERM IN THE DDS MODEL
    !             (ONE COEFFICIENT FOR EACH SPECIES).
    !    DGBAL -- ARRAY CONTAINING COEFFICIENTS FOR THE BASAL AREA IN
    !             LARGER TREES TERM IN THE DDS MODEL
    !             (ONE COEFFICIENT FOR EACH SPECIES).
    !   DGDBAL -- ARRAY CONTAINING COEFFICIENTS FOR THE INTERACTION
    !             BETWEEN BASAL AREA IN LARGER TREES AND LN(DBH) (ONE
    !             COEFFICIENT PER SPECIES).
    !    DGCCF -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
    !             COMPETITION FACTOR TERM IN THE DDS MODEL (ONE
    !             COEFFICIENT FOR EACH SPECIES, LOADED IN RCON).
    !  SPECIES ORDER:
    !  1=WP,  2=SP,  3=DF,  4=WF,  5=MH,  6=IC,  7=LP,  8=ES,  9=SH,  10=PP,
    ! 11=JU, 12=GF, 13=AF, 14=SF, 15=NF, 16=WB, 17=WL, 18=RC, 19=WH,  20=PY,
    ! 21=WA, 22=RA, 23=BM, 24=AS, 25=CW, 26=CH, 27=WO, 28=WI, 29=GC,  30=MC,
    ! 31=MB, 32=OS, 33=OH

    ! Variables
    logical :: DEBUG

    real    :: DGLD(33),DGBAL(33),DGCR(33),DGCRSQ(33),DGDBAL(33),DGFOR(6,&
         &33),DGDS(4,33),DGEL(33),DGEL2(33),DGSASP(33),DGCASP(33),DGSLOP(33),&
         &DGSLSQ(33),DGSIC(33),DGMAI(33),DGMACC(33),DGPCCF(33),DGCCFA(33),SLODUM(33),&
         &DGHAH(33),DGSITE(33),DGLBA(33),DGBA(33)
    integer :: MAPDSQ(10,33),MAPLOC(10,33)
    integer :: IBSERV(5,33),I,J,ISPC,I1,I2,I3,IPCCF,ISIC
    real    :: ALCCF,CONSPP,DGLDS,DGBALS,DGCRS,DGCRS2,DGDSQS,DGDBLS,SI,D
    real    :: BARK,BRATIO,RELHT,DPP,BATEM,DF,DIAGR,DDS,CR,ASPDG
    real    :: DGFJUN
    real    :: DUMMY,ALD,BAL,HOAVH,DGLCCF,DGPCF2,XSITE,LSI
    real    :: XSLOPE,ASPTEM,TEMEL,SASP,OBSERV(33)
    data DGLD/&
         &0.77889,  1.05245,  0.33160,  0.89887,  0.99324,&
         &0.72947,  0.63249,  1.32610, 1.186676,  0.53028,&
         &0.0,  0.89887,  0.91259, 0.980383,0.9042530,&
         &0.213947, 0.609098,  0.58705,0.7224620,0.8793380,&
         &0.8895960,      0.0,1.0241860,      0.0,0.8895960,&
         &0.8895960, 1.310111,0.8895960,0.8895960,0.8895960,&
         &0.8895960,  0.33160,0.8895960/
    data DGCR/&
         &3.36606,  2.89738,  4.43317,  3.11044,  1.73837,&
         &1.57139,  2.35065,  1.29730, 2.763519,  3.43377,&
         &0.0,  3.11044,  2.44945, 1.709846,4.1231012,&
         &1.523464, 1.158355,  1.29360,2.1603479,1.9700520,&
         &1.7325350,      0.0,0.4593870,      0.0,1.7325350,&
         &1.7325350, 0.271183,1.7325350,1.7325350,1.7325350,&
         &1.7325350,  4.43317,1.7325350/
    data DGCRSQ/&
         &-1.80146, -0.95430, -2.82894, -1.13806, -0.12161,&
         &0.00000, -0.63173,  0.00000,-0.871061, -1.84300,&
         &0.0, -1.13806, -0.72173,      0.0,-2.6893401,&
         &0.0,      0.0,      0.0,-0.8341960,     0.0,&
         &0.0,      0.0,      0.0,      0.0,      0.0,&
         &0.0,      0.0,      0.0,      0.0,      0.0,&
         &0.0, -2.82894,      0.0/
    data DGBA/&
         &0.00000,  0.00000,  0.00000,  0.00000,  0.00000,&
         &0.00000,  0.00000,  0.00000,  0.00000,  0.00000,&
         &0.00000,  0.00000,  0.00000,  0.00000,      0.0,&
         &0.00000,  0.00000,  0.00000,      0.0,-0.0001730,&
         &-0.000981,      0.0,      0.0,  0.00000,-0.0009810,&
         &-0.000981,  0.00000,-0.000981,-0.000981,-0.0009810,&
         &-0.000981,  0.00000,-0.000981/
    data DGBAL/&
         &0.00121,  0.00020,  0.00479,  0.00106, -0.00087,&
         &0.00483,  0.00169,  0.00000,      0.0,  0.00325,&
         &0.0,  0.00106,  0.00380,      0.0,      0.0,&
         &-0.358634,      0.0,      0.0,      0.0,      0.0,&
         &0.0,      0.0,      0.0,      0.0,      0.0,&
         &0.0,      0.0,      0.0,      0.0,      0.0,&
         &0.0,  0.00479,      0.0/
    data DGDBAL/&
         &-0.00897, -0.00437, -0.01918, -0.00707, -0.00105,&
         &-0.01497, -0.00647, -0.00239,-0.003728, -0.01362,&
         &0.0, -0.00707, -0.01620,-0.000261,-0.006368,&
         &0.0,-0.004253, -0.02284,-0.004065,-0.004215,&
         &-0.001265,     0.0, -0.010222,      0.0,-0.001265,&
         &-0.001265,      0.0,-0.001265,-0.001265,-0.001265,&
         &-0.001265, -0.01918,-0.001265/
    data DGMAI/&
         &0.00000,  0.00375,  0.00044,  0.00114,  0.00000,&
         &0.00246,  0.00778,  0.00000,      0.0,  0.00360,&
         &0.0,  0.00114,  0.00753,      0.0,      0.0,&
         &0.0,      0.0,      0.0,      0.0,      0.0,&
         &0.0,      0.0,      0.0,      0.0,      0.0,&
         &0.0,      0.0,      0.0,      0.0,      0.0,&
         &0.0,  0.00044,      0.0/
    data DGMACC/&
         &0.00001, -0.00001,  0.00001,  0.00001,  0.00001,&
         &0.00000, -0.00003,  0.00000,      0.0,  0.00000,&
         &0.0,  0.00001, -0.00002,      0.0,      0.0,&
         &0.0,      0.0,      0.0,      0.0,      0.0,&
         &0.0,      0.0,      0.0,      0.0,      0.0,&
         &0.0,      0.0,      0.0,      0.0,      0.0,&
         &0.0,  0.00001,      0.0/
    data DGPCCF/&
         &0.00000, -0.00060, -0.00041, -0.00027,  0.00000,&
         &-0.00053,  0.00000, -0.00044,      0.0,  0.00000,&
         &0.0, -0.00027, -0.00064,-0.000643,-0.0004710,&
         &0.0,-0.000568, -0.00094,      0.0,      0.0,&
         &0.0,      0.0,-0.000757,      0.0,      0.0,&
         &0.0,-0.000473,      0.0,      0.0,      0.0,&
         &0.0, -0.00041,      0.0/

    !  IBSERV IS THE NUMBER OF OBSERVATIONS IN THE DIAMETER GROWTH
    !  MODEL BY SPECIES AND SITE CLASS.

    data  ((IBSERV(I,J),I = 1,5),J = 1,15)/&
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
         &5*1000,&
         &5*1000,&
         &5*1000/
    data  ((IBSERV(I,J),I = 1,5),J = 16,33)/&
         &27,70,123,101,33,&
         &5*1000,&
         &5*1000,&
         &5*1000,&
         &5*1000,&
         &5*1000,&
         &5*1000,&
         &5*1000,&
         &184,429,356,162,74,&
         &5*1000,&
         &5*1000,&
         &5*1000,&
         &5*1000,&
         &5*1000,&
         &5*1000,&
         &5*1000,&
         &5*1000,&
         &5*1000/

    !  OBSERV CONTAINS THE NUMBER OF OBSERVATIONS BY HABITAT CLASS
    !  BY SPECIES FOR THE UNDERLYING MODEL (THIS DATA IS ACTUALLY
    !  USED BY **DGDRIV** FOR CALIBRATION.

    data  OBSERV/&
         &256.0,  306.0,  697.0,  5513.0,  779.0,&
         &501.0, 3208.0, 1185.0,   2062., 7837.0,&
         &22.0, 5513.0, 1198.0,   1210.,  1467.,&
         &22.0,   591.,  100.0,   4836.,   475.,&
         &220.0,  125.,    78.0,    22.0,  220.,&
         &220.,   306.,    220.,    220., 220.,&
         &220.,  697.0,  220./

    !  DGCCFA CONTAINS COEFFICIENTS FOR THE CCF TERM BY SPECIES BY
    !  HABITAT CLASS.

    data DGCCFA/&
         &-0.00016,  0.00157, -0.00138, -0.00024, -0.00027,&
         &0.00000,  0.00109,  0.00000,      0.0,  0.00000,&
         &0.00000, -0.00024,  0.00201,      0.0,      0.0,&
         &-0.199592,      0.0,      0.0,      0.0,      0.0,&
         &0.0,      0.0,      0.0,  0.00000,      0.0,&
         &0.0,      0.0,      0.0,      0.0,      0.0,&
         &0.0, -0.00138,      0.0/

    !  DGFOR CONTAINS LOCATION CLASS CONSTANTS FOR EACH SPECIES.
    !  MAPLOC IS AN ARRAY WHICH MAPS FOREST ONTO A LOCATION CLASS.

    data ((MAPLOC(I,J),I = 1,10),J = 1,15)/&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,2,3,4,4,4,4,1,1,1,&
         &1,1,2,3,4,5,4,5,1,1,&
         &1,1,2,2,3,3,3,4,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,2,2,3,4,5,4,6,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,2,2,3,3,4,3,2,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,2,2,3,3,3,4,1,1,&
         &1,2,2,3,4,5,4,5,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1/
    data ((MAPLOC(I,J),I = 1,10),J = 16,33)/&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,2,3,4,5,4,5,1,1,&
         &1,1,1,1,1,1,1,1,1,1/

    data ((DGFOR(I,J),I = 1,6),J = 1,15)/&
         &-0.23185, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
         &-1.09124,-1.04630,-.048564,-0.47130, 0.00000, 0.00000,&
         &0.83941, 0.97742, 0.67207, 1.17949, 0.83941, 0.00000,&
         &-0.16674,-0.04804, 0.02336, 0.47563, 0.00000, 0.00000,&
         &-0.25368, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
         &0.23477, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
         &-0.03997,-0.17367, 0.80024, 0.40611, 0.17307, 0.35268,&
         &-4.64535, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
         &-2.073942, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
         &1.48355, 1.34282, 1.46037, 1.54377, 0.00000, 0.00000,&
         &0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
         &-0.16674,-0.04804, 0.02336, 0.47563, 0.00000, 0.00000,&
         &-1.68772,-0.96240,-1.38122,-1.03440,-0.76742, 0.00000,&
         &-.441408, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
         &-1.127977, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000/
    data ((DGFOR(I,J),I = 1,6),J = 16,33)/&
         &1.911884, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
         &-.605649, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
         &1.49419, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
         &-0.147675,0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
         &-1.3100671,0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
         &-0.107648,0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
         &0.0,0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
         &-7.753469,0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
         &0.0, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
         &-0.107648, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
         &-0.107648, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
         &-1.958189, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
         &-0.107648, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
         &-0.107648, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
         &-0.107648, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
         &-0.107648, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
         &0.83941, 0.97742, 0.67207, 1.17949, 0.83941, 0.00000,&
         &-0.107648, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000/

    !  DGDS CONTAINS COEFFICIENTS FOR THE DIAMETER SQUARED TERMS
    !  IN THE DIAMETER INCREMENT MODELS; ARRAYED BY FOREST BY
    !  SPECIES.  MAPDSQ IS AN ARRAY WHICH MAPS FOREST ONTO A DBH**2
    !  COEFFICIENT.

    data ((MAPDSQ(I,J),I = 1,10),J = 1,15)/&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,2,3,2,3,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,2,1,2,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,2,3,3,3,3,3,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,2,3,2,3,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1/
    data ((MAPDSQ(I,J),I = 1,10),J = 16,33)/&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,1,1,1/

    data ((DGDS(I,J),I = 1,4),J = 1,15)/&
         &-0.000091, 0.000000,      0.0,      0.0,&
         &-0.000295, 0.000000,      0.0,      0.0,&
         &-0.000066, 0.000000, 0.000000, 0.000000,&
         &-0.000414,-0.000239,-0.000325,      0.0,&
         &-0.000252, 0.000000, 0.000000,      0.0,&
         &-0.000251,-0.000133,      0.0,      0.0,&
         &-0.000099, 0.000000, 0.000000, 0.000000,&
         &0.000000, 0.000000, 0.000000,      0.0,&
         &-0.0004572, 0.000000,      0.0,      0.0,&
         &-0.000324,-0.000356,-0.000120,      0.0,&
         &0.0,      0.0,      0.0,      0.0,&
         &-0.000414,-0.000239,-0.000325,      0.0,&
         &-0.000468, 0.000000,      0.0,      0.0,&
         &-0.0002189, 0.000000,      0.0,      0.0,&
         &-0.0003996, 0.000000,      0.0,      0.0/
    data ((DGDS(I,J),I = 1,4),J = 16,33)/&
         &-0.0006538, 0.000000,      0.0,      0.0,&
         &-0.0001683, 0.000000,      0.0,      0.0,&
         &0.0, 0.000000,      0.0,      0.0,&
         &-0.0001546, 0.000000,      0.0,      0.0,&
         &-0.0001323, 0.000000,      0.0,      0.0,&
         &0.0, 0.000000,      0.0,      0.0,&
         &0.0, 0.000000,      0.0,      0.0,&
         &-0.0001737, 0.000000,      0.0,      0.0,&
         &0.0, 0.000000,      0.0,      0.0,&
         &0.0, 0.000000,      0.0,      0.0,&
         &0.0, 0.000000,      0.0,      0.0,&
         &-0.0003048, 0.000000,      0.0,      0.0,&
         &0.0, 0.000000,      0.0,      0.0,&
         &0.0, 0.000000,      0.0,      0.0,&
         &0.0, 0.000000,      0.0,      0.0,&
         &0.0, 0.000000,      0.0,      0.0,&
         &-0.000066, 0.000000, 0.000000, 0.000000,&
         &0.0, 0.000000,      0.0,      0.0/

    !  DGEL CONTAINS THE COEFFICIENTS FOR THE ELEVATION TERM IN THE
    !  DIAMETER GROWTH EQUATION.  DGEL2 CONTAINS THE COEFFICIENTS FOR
    !  THE ELEVATION SQUARED TERM IN THE DIAMETER GROWTH EQUATION.
    !  DGSASP CONTAINS THE COEFFICIENTS FOR THE SIN(ASPECT)*SLOPE
    !  TERM IN THE DIAMETER GROWTH EQUATION.  DGCASP CONTAINS THE
    !  COEFFICIENTS FOR THE COS(ASPECT)*SLOPE TERM IN THE DIAMETER
    !  GROWTH EQUATION.  DGSLOP CONTAINS THE COEFFICIENTS FOR THE
    !  SLOPE TERM IN THE DIAMETER GROWTH EQUATION.  DGSLSQ CONTAINS
    !  COEFFICIENTS FOR THE (SLOPE)**2 TERM IN THE DIAMETER GROWTH
    !  MODELS. ALL OF THESE ARRAYS ARE SUBSCRIPTED BY SPECIES.

    data DGCASP/&
         &0.12915,  0.08831, -0.17389, -0.14234, -0.29385,&
         &-0.04156, -0.39893,  0.38002,-0.444594, -0.05217,&
         &0.0, -0.14234, -0.65502,-0.059062,-0.3745120,&
         &-0.609774,-0.156235, -0.06625,      0.0,      0.0,&
         &0.0859580,      0.0,      0.0,      0.0,0.0859580,&
         &0.0859580,      0.0,0.0859580,0.0859580,0.0859580,&
         &0.0859580, -0.17389,0.0859580/

    data DGSASP/&
         &-0.19278,  0.56358, -0.40153, -0.17130,  0.33069,&
         &-0.43281, -0.25658, -0.17911, 0.139180, -0.14076,&
         &0.0, -0.17130, -0.28887,-0.128126,-0.2076590,&
         &-0.017520, 0.258712,  0.05534,      0.0,      0.0,&
         &-0.8639800,      0.0,     0.0,      0.0,-0.86398,&
         &-0.86398,      0.0,-0.86398, -0.86398, -0.86398,&
         &-0.86398, -0.40153,-0.86398/

    data DGSLOP/&
         &0.77922, -1.50252, -0.18923,  0.02912, -0.59628,&
         &-0.90318,  0.19253, -0.81780,      0.0, -0.29407,&
         &0.0,  0.02912,  0.64936, 0.240178,0.4002230,&
         &-2.057060,-0.635704,  0.11931,0.4214860,      0.0,&
         &0.0,      0.0,      0.0,      0.0,      0.0,&
         &0.0,      0.0,      0.0,      0.0,      0.0,&
         &0.0, -0.18923,      0.0/

    data DGSLSQ/&
         &-0.93813,  1.21439, -0.49057, -0.29671,  1.07549,&
         &1.36603,  0.00000,  0.84368,      0.0,  0.16735,&
         &0.0, -0.29671, -0.37153, 0.131356,      0.0,&
         &2.11326,      0.0,      0.0,-0.6936100,     0.0,&
         &0.0,      0.0,      0.0,      0.0,      0.0,&
         &0.0,      0.0,      0.0,      0.0,      0.0,&
         &0.0, -0.49057,      0.0/

    data DGEL/&
         &0.00279,  0.00466,  0.01544,  0.00362, -0.03036,&
         &-0.00637,  0.01012,  0.00000,   0.0248, -0.00331,&
         &0.0,  0.00362,  0.02956,-0.015087,-0.0690450,&
         &0.0, 0.004379, -0.00175,-0.0400670,      0.0,&
         &-0.075986,      0.0,-0.012111,      0.0,-0.0759860,&
         &-0.075986,   0.0049,-0.075986,-0.075986,-0.075986,&
         &-0.075986,  0.01544,-0.075986/

    data DGEL2/&
         &-0.00001,  0.00001, -0.00010, -0.00006,  0.00037,&
         &0.00014, -0.00020,  0.00000,-0.00033429,  0.00006,&
         &0.0, -0.00006, -0.00033,      0.0, 0.0006080,&
         &0.0,      0.0,-0.000067,0.0003950,      0.0,&
         &0.0011930,      0.0,      0.0,      0.0, 0.001193,&
         &0.001193,-0.00008781,0.001193,0.001193,0.001193,&
         &0.001193, -0.00010,0.001193/

    data DGSIC /&
         &0.000000, 0.000000, 0.000000, 0.000000, 0.000000,&
         &0.000000, 0.000000, 0.000000, 0.000000, 0.000000,&
         &0.0, 0.000000, 0.000000,      0.0,      0.0,&
         &0.001766,      0.0,      0.0,      0.0,      0.0,&
         &0.0,      0.0, 0.000000,      0.0, 0.000000,&
         &0.000000, 0.000000, 0.000000, 0.000000, 0.000000,&
         &0.000000, 0.000000, 0.000000/

    data DGSITE /&
         &0.000000, 0.000000, 0.000000, 0.000000, 0.000000,&
         &0.000000, 0.000000, 0.000000, 0.492695, 0.000000,&
         &0.000000, 0.000000, 0.000000, 0.323625,0.6849390,&
         &0.0,0.351929,       0.0,0.3804160,0.2528530,&
         &0.2273070,      0.0, 1.965888, 0.000000, 0.227307,&
         &0.227307, 0.213526, 0.227307, 0.227307, 0.227307,&
         &0.227307, 0.000000, 0.227307/

    data DGLBA /&
         &0.000000, 0.000000, 0.000000, 0.000000, 0.000000,&
         &0.000000, 0.000000, 0.000000,-0.122905, 0.000000,&
         &0.000000, 0.000000, 0.000000, 0.000000,      0.0,&
         &0.000000, 0.000000, 0.000000,      0.0,      0.0,&
         &0.0,      0.0,      0.0, 0.000000,      0.0,&
         &0.0,      0.0,      0.0,      0.0,      0.0,&
         &0.0, 0.000000,      0.0/

    data DGHAH /&
         &0.000000, 0.000000, 0.000000, 0.000000, 0.000000,&
         &0.000000, 0.000000, 0.000000,      0.0, 0.000000,&
         &0.000000, 0.000000, 0.000000, 0.000000,      0.0,&
         &0.000000, 0.000000, 0.000000,-0.0003580,     0.0,&
         &0.0,      0.0,      0.0, 0.000000,      0.0,&
         &0.0,      0.0, 0.000000,      0.0,      0.0,&
         &0.0, 0.000000,      0.0/

    ! DUMMY FOR SLOPE EQ 0

    data SLODUM /&
         &0.000000, 0.000000, 0.000000, 0.000000, 0.000000,&
         &0.000000, 0.000000, 0.000000, 0.000000, 0.000000,&
         &0.000000, 0.000000, 0.000000,-0.174404,      0.0,&
         &0.0,-0.290174, 0.000000,      0.0,      0.0,&
         &0.0, 0.000000, 0.000000, 0.000000, 0.000000,&
         &0.000000, 0.000000, 0.000000, 0.000000, 0.000000,&
         &0.000000, 0.000000, 0.000000/

    !  CHECK FOR DEBUG.

    if (LCONS) go to 1000

    call DBCHK (DEBUG,'DGF',3,ICYC)
    if (DEBUG) write(JOSTND,3)ICYC
    3 format(' ENTERING SUBROUTINE DGF  CYCLE =',I5)

    !  DEBUG OUTPUT: MODEL COEFFICIENTS.

    if (DEBUG) write(JOSTND,*) ' IN DGF,HTCON=',HTCON
    if (DEBUG)&
         &write(JOSTND,*) ' DGCON= ', DGCON
    !     & WRITE(JOSTND,9000) DGCON,DGDSQ,DGLD,DGCR,DGCRSQ,DGCCF,DGBAL
    ! 9000 FORMAT (/,11(1X,F10.5))

    !  BEGIN SPECIES LOOP.  ASSIGN VARIABLES WHICH ARE SPECIES
    !  DEPENDENT

    ALCCF = 0.0
    if (RELDEN .gt.0.0)ALCCF = ALOG(RELDEN)
    do ISPC = 1,MAXSP
       I1 = ISCT(ISPC,1)
       if (I1.eq.0) go to 20
       I2 = ISCT(ISPC,2)

       !  WARM SPRINGS RESERVATION USES EQUATIONS FROM EC OR WC VARIANTS
       !  FOR SOME SPECIES

       if (IFOR .eq.10) then
          select case (ISPC)
          case(1,2,6,7,18,32)
             CONSPP = DGCON(ISPC) + COR(ISPC)
             go to 5
          end select
       end if

       !  FORESTS OTHER THAN WARM SPRINGS, AND WARM SPRINGS SPECIES THAT USE
       !  EQUATIONS FROM THE SO VARIANT

       CONSPP = DGCON(ISPC) + COR(ISPC) + 0.01*DGCCF(ISPC)*RELDEN&
            &+0.01*DGMACC(ISPC)*RMAI*RELDEN

       5 continue
       DGLDS = DGLD(ISPC)
       DGBALS = DGBAL(ISPC)
       DGCRS = DGCR(ISPC)
       DGCRS2 = DGCRSQ(ISPC)
       DGDSQS = DGDSQ(ISPC)
       DGDBLS = DGDBAL(ISPC)
       SI = SITEAR(ISPC)

       if (DEBUG)write(JOSTND,9002)ISPC,DGCON(ISPC),COR(ISPC),&
            &DGCCF(ISPC),DGMACC(ISPC),RELDEN,RMAI,CONSPP
       9002 format(' IN DGF 9002 ISPC,DGCON,COR,DGCCF,DGMACC,RELDEN,RMAI,',&
            &'CONSPP=',I5,10F10.5)

       !  BEGIN TREE LOOP WITHIN SPECIES ISPC.

       do I3 = I1,I2
          I = IND1(I3)
          D = DIAM(I)
          WK2(I) = 0.0
          IPCCF = ITRE(I)
          BARK = BRATIO(ISPC,D,HT(I))
          if (D.le.0.0) go to 10
          RELHT = 0.0
          if (AVH .gt.0.0) RELHT = HT(I)/AVH
          if (RELHT .gt.1.5)RELHT = 1.5

          !  THE FOLLOWING IS JU(11) FROM THE UT VARIANT

          if (ISPC.eq.11) then
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
             if (DEBUG)write(JOSTND,*)' ISPC,D,DIAGR,DPP,BA,BARK,CONSPP= '
             if (DEBUG)write(JOSTND,*)ISPC,D,DIAGR,DPP,BA,BARK,CONSPP
             go to 8
          else if (ISPC.eq.24) then

             !  AS(24) IS FROM THE UT VARIANT

             CR = ICR(I)
             call DGFASP(D,ASPDG,CR,BARK,SI,DEBUG)
             if (DEBUG)write(JOSTND,*)' D,ASPDG,CR,BARK,SI= ',&
                  &D,ASPDG,CR,BARK,SI
             if (DEBUG)write(JOSTND,*)' COR2, COR= ',COR2(ISPC),COR(ISPC)
             DDS = ASPDG + ALOG(COR2(ISPC)) + COR(ISPC)
             go to 8

             !  RED ALDER USES A DIFFERENT EQUATION.
             !  FUNCTION BOTTOMS OUT AT D=18. DECREASE LINERALY AFTER THAT TO
             !  DG=0 AT D=28, AND LIMIT TO .1 ON LOWER END.  GED 4-15-93.

          else if (ISPC .eq.22) then
             BARK = BRATIO(22,D,HT(I))
             call DGFRA(ISPC,D,BARK,DIAGR,DDS)
             if (DEBUG)write(JOSTND,*)' RA-BARK,D,BA,DIAGR,COR2,COR,DDS= '
             if (DEBUG)write(JOSTND,*)BARK,D,BA,DIAGR,COR2(ISPC),COR(ISPC),DDS

             go to 8
          end if

          !  WARM SPRINGS RESERVATION USES EQUATIONS FROM EC OR WC VARIANTS
          !  FOR SOME SPECIES

          if (DEBUG)write(JOSTND,*)' I,IFOR,ISPC,ICR,PCT,BA= ',&
               &I,IFOR,ISPC,ICR(I),PCT(I),BA
          if (IFOR .eq.10) then
             CR = ICR(I)*0.01
             BAL = (1.0 - (PCT(I)/100.)) * BA
             select case (ISPC)

                !       USE WHITE PINE AND SUGAR PINE EQN FROM WC VARIANT

             case (1,2)
                DDS = CONSPP + 0.844690*ALOG(D) + 1.597250*CR&
                     &- 0.0000596*D*D  - 0.003726*BAL/(ALOG(D+1.0))&
                     &- 0.000257*PCCF(IPCCF)
                if (DEBUG)write(JOSTND,*)' WSPR WP&SP: I,CONSPP,D,CR,BAL,',&
                     &'PCCF,DDS= ',I,CONSPP,D,CR,BAL,PCCF(IPCCF),DDS
                go to 8

                !       USE INCENSE CEDAR EQN FROM WC VARIANT

             case (6)
                DDS = CONSPP + 0.879338*ALOG(D)&
                     &+ 1.970052*CR  - 0.0001323*D*D&
                     &- 0.004215*BAL/(ALOG(D+1.0)) - 0.000173*BA
                if (DEBUG)write(JOSTND,*)' WSPR IC: I,CONSPP,D,CR,BAL,BA,',&
                     &'DDS= ',I,CONSPP,D,CR,BAL,BA,DDS
                go to 8

                !       USE LODGEPOLE PINE EQN FROM EC VARIANT

             case (7)
                DDS = CONSPP + 0.554261*ALOG(D) + 1.423849*CR&
                     &- 0.004803*BAL/(ALOG(D+1.0)) - 0.000627*PCCF(IPCCF)
                if (DEBUG)write(JOSTND,*)' WSPR LP: I,CONSPP,D,CR,BAL,PCCF,',&
                     &'DDS= ',I,CONSPP,D,CR,BAL,PCCF(IPCCF),DDS
                go to 8

                !       USE REDCEDAR EQN FROM WC VARIANT

             case (18)
                DDS = CONSPP + 0.843013*ALOG(D)&
                     &+ CR*(2.878032 - 1.631418*CR)&
                     &- 0.0000644*D*D  - 0.003923*BAL/(ALOG(D+1.0))&
                     &- 0.000552*PCCF(IPCCF)
                if (DEBUG)write(JOSTND,*)' WSPR RC: I,CONSPP,D,CR,BAL,PCCF,',&
                     &'DDS= ',I,CONSPP,D,CR,BAL,PCCF(IPCCF),DDS
                go to 8

                !       OTHER SOFTWOODS USE ALASKA YELLOW CEDAR EQN FROM WC VARIANT

             case (32)
                DDS = CONSPP + 0.816880*ALOG(D)&
                     &+ 2.471226*CR - 0.0002536*D*D&
                     &- 0.005950*BAL/(ALOG(D+1.0)) - 0.000147*BA
                if (DEBUG)write(JOSTND,*)' WSPR OS: I,CONSPP,D,CR,BAL,BA,',&
                     &'DDS= ',I,CONSPP,D,CR,BAL,BA,DDS
                go to 8

             end select
          end if

          !  FORESTS OTHER THAN WARM SPRINGS, AND WARM SPRINGS SPECIES THAT USE
          !  EQUATIONS FROM THE SO VARIANT

          !  SF(14) FROM EC NEEDS ADDITIONAL VARIABLE (DUMMY)

          if (ISPC.eq.14) then
             DUMMY = -0.799079
          else
             DUMMY = 0.
          end if

          !  SORNEC11 CODE FOLLOWS

          ALD = ALOG(D)
          CR = ICR(I)*0.01
          BAL = (1.0 - (PCT(I)/100.)) * BA
          if (DEBUG)write(JOSTND,*)' BAL,BA,PCT(I)= ',BAL,BA,PCT(I)

          !  SPECIES 16 TT(1) AND 11 UT(12) USE SCALED BAL

          if (ISPC.eq.11.or.ISPC.eq.16)BAL = BAL/100.

          HOAVH = HT(I)/AVH
          if (HOAVH .gt.1.5)HOAVH = 1.5
          DGLCCF = 0.0
          DGPCF2 = 0.0
          if (ISPC .eq.10) then
             DGLCCF = (-0.20488)
             DGPCF2 = .000001
             if (PCCF(IPCCF) .gt.400) DGPCF2 = 0.0
          end if
          DDS = CONSPP +DGLDS*ALD + DGBALS*BAL + CR*(DGCRS+CR*DGCRS2)&
               &+DGDSQS*D*D  + DGDBLS*BAL/(ALOG(D+1.0))&
               &+DGPCCF(ISPC)*PCCF(IPCCF)&
               &+DGLCCF * ALCCF + DGPCF2 * (PCCF(IPCCF)**2)&
               &+DGHAH(ISPC)*RELHT&
               &+DGLBA(ISPC)*ALOG(BA)&
               &+DGBA(ISPC)*BA + DUMMY
          if (ISPC.eq.8)DDS = DDS+0.49649*HOAVH

          !      IF(DEBUG)THEN
          !      WRITE(JOSTND,*)' DGLDS*ALD= ',DGLDS*ALD
          !      WRITE(JOSTND,*)' DGBALS*BAL= ', DGBALS*BAL
          !      WRITE(JOSTND,*)' CR*DGCRS+= ',CR*DGCRS
          !      WRITE(JOSTND,*)' CR*CR*DGCRS2= ',CR*CR*DGCRS2
          !      WRITE(JOSTND,*)' DGDSQS*D*D= ',DGDSQS*D*D
          !      WRITE(JOSTND,*)' DGDBLS*BAL/(ALOG(D)= ',DGDBLS*BAL/(ALOG(D+1.0))
          !      WRITE(JOSTND,*)' DGPCCF(ISPC)*PCCF(I= ',DGPCCF(ISPC)*PCCF(IPCCF)
          !      WRITE(JOSTND,*)' IPCCF,PCCF(IPCCF) ',IPCCF,PCCF(IPCCF)
          !      WRITE(JOSTND,*)' DGLCCF * ALCCF= ',DGLCCF * ALCCF
          !      WRITE(JOSTND,*)' DGPCF2 * (PCCF**2)= ',DGPCF2 * (PCCF(IPCCF)**2)
          !      WRITE(JOSTND,*)' DGHAH(ISPC)*RELHT= ',DGHAH(ISPC)*RELHT
          !      WRITE(JOSTND,*)' DGLBA(ISPC)*ALOG(BA)= ',DGLBA(ISPC)*ALOG(BA)
          !      WRITE(JOSTND,*)' DGBA(ISPC)*BA + DUMMY= ',DGBA(ISPC)*BA + DUMMY
          !      WRITE(JOSTND,*)' DDS= ', DDS
          !      ENDIF

          if (DEBUG)write(JOSTND,*)' DGCON,COR,DGCCF,RELDEN,DGMACC,RMAI,',&
               &'CONSPP,DGLDS,ALD,DGBALS,BAL,CR,DGCRS,DGCRS2,DGDSQS,D,DGDBLS,',&
               &'DGPCCF,PCCF,DDS,DGLCCF,ALCCF,DGPCF2'
          if (DEBUG) write(JOSTND,8000) DGCON(ISPC),COR(ISPC),&
               &DGCCF(ISPC),RELDEN,DGMACC(ISPC),RMAI,CONSPP,DGLDS,ALD,&
               &DGBALS,BAL,CR,DGCRS,DGCRS2,DGDSQS,D,DGDBLS,&
               &DGPCCF(ISPC),PCCF(IPCCF),DDS,DGLCCF,ALCCF,DGPCF2
          8000 format(' IN DGF AT LINE 622',9F12.5/1X,8F12.5/&
               &1X,6F12.5/)

          !     CALL PPDGF TO GET A MODIFICATION VALUE FOR DDS THAT ACCOUNTS
          !     FOR THE DENSITY OF NEIGHBORING STANDS.

          8 continue
          if (DDS.lt.-9.21) DDS = -9.21
          WK2(I) = DDS

          !  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.

          if (DEBUG) then
             write(JOSTND,9001) I,ISPC,D,BAL,CR,RELDEN,BA,DDS
             9001 format(' IN DGF, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,&
                  &',  BAL=',F7.2,',  CR=',F7.4/&
                  &'       RELDEN=',F9.3,',  BA=',F9.3,', LN(DDS)=',F7.4)
          end if
          10 continue
       end do

       !  END OF SPECIES LOOP.

       20 continue
    end do
    if (DEBUG)write(JOSTND,100)ICYC
    100 format(' LEAVING SUBROUTINE DGF  CYCLE =',I5)
    return

    1000 continue

    !  ENTRY POINT FOR LOADING COEFFICIENTS OF THE DIAMETER INCREMENT
    !  MODEL THAT ARE SITE SPECIFIC AND NEED ONLY BE RESOLVED ONCE.
    !  IDTYPE IS A HABITAT TYPE INDEX THAT IS COMPUTED IN **RCON**.
    !  CHECK FOR DEBUG.

    call DBCHK (DEBUG,'DGF',3,ICYC)

    !  NOTE: FOR UT SPCIES 11(JU), 24(AS) AND TT 16(WB) 0.7854 IS 45 DEGREES
    !        IN RADIANS. THIS CORRECTION WAS
    !        PROBABLY DUE TO A PROBLEM IN THE WAY ASPECT WAS RECORDED
    !        IN THE DATA SET USED TO FIT THE MODEL.  GED 12-20-94.
    !  PUT SITE INTO ONE OF 5 CLASSES FOR UT SPCIES 11(JU) AND 24(AS)

    XSITE = SITEAR(ISISP)
    LSI = XSITE/10.
    if (LSI.lt.2)ISIC = 1
    if (LSI.ge.5)ISIC = 5
    if (LSI.ge.3.and. LSI.lt.4)ISIC = 3
    if (LSI.ge.4.and. LSI.lt.5)ISIC = 4
    if (LSI.ge.2.and. LSI.lt.3)ISIC = 2

    !  ENTER LOOP TO LOAD SPECIES DEPENDENT VECTORS.

    ISPHAB = 1
    ISPCCF = 1
    if (DEBUG)write(JOSTND,*)' IFOR= ',IFOR
    do ISPC = 1,MAXSP
       ISPFOR = MAPLOC(IFOR,ISPC)
       ISPDSQ = MAPDSQ(IFOR,ISPC)
       DGCCF(ISPC) = DGCCFA(ISPC)
       DGDSQ(ISPC) = DGDS(ISPDSQ,ISPC)

       !  WARM SPRINGS RESERVATION USES EQUATIONS FROM EC OR WC VARIANTS
       !  FOR SOME SPECIES

       if (IFOR .eq.10) then
          select case (ISPC)

             !       USE WHITE PINE AND SUGAR PINE EQN FROM WC VARIANT
             !       FOR WHITE PINE TRANSLATE SITE FROM BRICKELL (T50) TO CURTIS (B100)
             !       FOR SUGAR PINE TRANSLATE SITE FROM BARRETT (B100) TO CURTIS (B100)
             !       SITE TRANSLATION EQNS FROM ERIN SMITH-MATEJA, FMSC
             !       USE MT HOOD LOCATION CONSTANTS

          case (1,2)
             SASP = 0.
             if (ISPC .eq.1) then
                XSITE = 1.96 * SITEAR(ISPC)
             else
                XSITE = SITEAR(ISPC)
             end if
             DGCON(ISPC) = &
                  &- 0.589570&
                  &- 0.023376 * ELEV&
                  &+ 0.404010 * ALOG(XSITE)&
                  &+ SASP
             ATTEN(ISPC) = 596.
             SMCON(ISPC) = 0.
             if (DEBUG)write(JOSTND,*)' WSPR WP,SP: DGCON,SLOPE,ASPECT,',&
                  &'ELEV,SITEAR,XSITE,SASP= ',DGCON(ISPC),SLOPE,ASPECT,&
                  &ELEV,SITEAR(ISPC),XSITE,SASP
             go to 28

             !       USE INCENSE CEDAR EQN FROM WC VARIANT
             !       TRANSLATE SITE FROM COCHRAN (B50) TO BARRETT (B100)
             !       SITE TRANSLATION EQN FROM ERIN SMITH-MATEJA, FMSC
             !       USE MT HOOD LOCATION CONSTANTS

          case (6)
             SASP = 0.
             XSITE = 25.0 + 1.2 * SITEAR(ISPC)
             DGCON(ISPC) = &
                  &- 1.310067&
                  &+ 0.252853 * ALOG(XSITE)&
                  &+ SASP
             ATTEN(ISPC) = 475.
             SMCON(ISPC) = 0.
             if (DEBUG)write(JOSTND,*)' WSPR IC: DGCON,SLOPE,ASPECT,ELEV,',&
                  &'SITEAR,XSITE,SASP= ',DGCON(ISPC),SLOPE,ASPECT,ELEV,&
                  &SITEAR(ISPC),XSITE,SASP
             go to 28

             !       USE LODGEPOLE PINE EQN FROM EC VARIANT
             !       TRANSLATE SITE FROM DAHMS (T50) TO ALEXANDER (T100)
             !       SITE TRANSLATION EQN FROM ERIN SMITH-MATEJA, FMSC
             !       USE MT HOOD LOCATION CONSTANTS

          case (7)
             SASP = &
                  &(-0.142328 * SIN(ASPECT)&
                  &- 0.064328 * COS(ASPECT)&
                  &- 0.097297) * SLOPE&
                  &+ 0.094464 * SLOPE * SLOPE
             XSITE = 12.25 + 1.325 * SITEAR(ISPC)
             DGCON(ISPC) = &
                  &- 1.084679&
                  &- 0.001124 * ELEV&
                  &+ 0.458662 * ALOG(XSITE)&
                  &+ SASP
             ATTEN(ISPC) = 1478.
             SMCON(ISPC) = 0.
             if (DEBUG)write(JOSTND,*)' WSPR LP: DGCON,SLOPE,ASPECT,ELEV,',&
                  &'SITEAR,XSITE,SASP= ',DGCON(ISPC),SLOPE,ASPECT,ELEV,&
                  &SITEAR(ISPC),XSITE,SASP
             go to 28

             !       USE REDCEDAR EQN FROM WC VARIANT
             !       TRANSLATE SITE FROM HEGYI (T100) TO CURTIS (B100)
             !       SITE TRANSLATION EQN FROM ERIN SMITH-MATEJA, FMSC
             !       USE MT HOOD LOCATION CONSTANTS

          case (18)
             SASP = 0.
             XSITE = SITEAR(ISPC)
             DGCON(ISPC) = &
                  &0.645645&
                  &- 0.050081 * ELEV&
                  &+ 0.000660 * ELEV * ELEV&
                  &+ 0.139734 * ALOG(XSITE)&
                  &+ SASP
             ATTEN(ISPC) = 1309.
             SMCON(ISPC) = 0.
             if (DEBUG)write(JOSTND,*)' WSPR RC: DGCON,SLOPE,ASPECT,ELEV,',&
                  &'SITEAR,XSITE,SASP= ',DGCON(ISPC),SLOPE,ASPECT,ELEV,&
                  &SITEAR(ISPC),XSITE,SASP
             go to 28

             !       OTHER SOFTWOODS USE ALASKA YELLOW CEDAR EQN FROM WC VARIANT
             !       TRANSLATE SITE FROM COCHRAN (B50) TO CURTIS (B100)
             !       SITE TRANSLATION EQN FROM ERIN SMITH-MATEJA, FMSC
             !       USE MT HOOD LOCATION CONSTANTS

          case (32)
             SASP = &
                  &(0.679903 * SIN(ASPECT)&
                  &- 0.023186 * COS(ASPECT)) * SLOPE
             XSITE = 3.5 + 1.45 * SITEAR(ISPC)
             DGCON(ISPC) = &
                  &- 1.277664&
                  &+ 0.244694 * ALOG(XSITE)&
                  &+ SASP
             ATTEN(ISPC) = 0.
             SMCON(ISPC) = 0.
             if (DEBUG)write(JOSTND,*)' WSPR OS: DGCON,SLOPE,ASPECT,ELEV,',&
                  &'SITEAR,XSITE,SASP= ',DGCON(ISPC),SLOPE,ASPECT,ELEV,&
                  &SITEAR(ISPC),XSITE,SASP
             go to 28

          end select
       end if

       !  FORESTS OTHER THAN WARM SPRINGS, AND WARM SPRINGS SPECIES THAT USE
       !  EQUATIONS FROM THE SO VARIANT

       XSITE = SITEAR(ISISP)
       XSLOPE = SLOPE
       if (ISPC.eq.11.or.ISPC.eq.16.or.ISPC.eq.24) then
          ASPTEM = ASPECT-0.7854
       else
          ASPTEM = ASPECT
       end if
       TEMEL = ELEV
       select case(ISPC)
       case(21,25,26,28,29,30,31,33)
          if (TEMEL.gt.30.)TEMEL = 30.
       end select

       !  NULLIFY THE SLOPE COEF FOR JUNIPER BECAUSE OF POOR EXTRAPOLATION
       !  IF(ISPC .EQ. 11)XSLOPE=0.0 USED IN SO11 FOR OTHER

       SASP = &
            &+(DGSASP(ISPC) * SIN(ASPTEM)&
            &+ DGCASP(ISPC) * COS(ASPTEM)&
            &+ DGSLOP(ISPC)) * XSLOPE&
            &+ DGSLSQ(ISPC) * XSLOPE * XSLOPE

       if ((SLOPE .le.0.0).and.((ISPC.eq.14).or.(ISPC.eq.17)))&
            &SASP = SLODUM(ISPC) !FROM EC VARIANT

       DGCON(ISPC) = &
            &DGFOR(ISPFOR,ISPC)&
            &+ DGEL(ISPC) * TEMEL&
            &+ DGEL2(ISPC) * TEMEL * TEMEL&
            &+ DGMAI(ISPC)*RMAI&
            &+ DGSIC(ISPC)*XSITE&
            &+ DGSITE(ISPC)*ALOG(SITEAR(ISPC))&
            &+ SASP

       !      IF(DEBUG)THEN
       !      WRITE(JOSTND,*)' DGFOR(ISPFOR,ISPC)= ',       DGFOR(ISPFOR,ISPC)
       !      WRITE(JOSTND,*)' DGEL(ISPC) * TEMEL= ',        DGEL(ISPC) * TEMEL
       !      WRITE(JOSTND,*)' DGEL2(ISPC)*TEMEL*TEMEL= ',DGEL2(ISPC)*TEMEL*TEMEL
       !      WRITE(JOSTND,*)' DGMAI(ISPC)*RMAI= ',         DGMAI(ISPC)*RMAI
       !      WRITE(JOSTND,*)' DGSIC(ISPC)*XSITE= ',        DGSIC(ISPC)*XSITE
       !      WRITE(JOSTND,*)' DGSITE*ALOG(SI)=',DGSITE(ISPC)*ALOG(SITEAR(ISPC))
       !      WRITE(JOSTND,*)' SASP         = ',                SASP
       !      WRITE(JOSTND,*)'DGSASP(ISPC) * SIN= ', DGSASP(ISPC) * SIN(ASPTEM)
       !      WRITE(JOSTND,*)'DGCASP(ISPC) * COS= ', DGCASP(ISPC) * COS(ASPTEM)
       !      WRITE(JOSTND,*)'DGSLOP(ISPC)) * XSLOPE= ',DGSLOP(ISPC) * XSLOPE
       !      WRITE(JOSTND,*)'DGSLSQ*XSLOPE2= ',DGSLSQ(ISPC) * XSLOPE * XSLOPE
       !      ENDIF

       if (ISPC.eq.8)&
            &DGCON(ISPC) = DGCON(ISPC)+.86756*ALOG(SITEAR(ISPC))
       if (ISPC.eq.11.or.ISPC.eq.16.or.ISPC.eq.24) then
          ATTEN(ISPC) = IBSERV(ISIC,ISPC)
       else
          ATTEN(ISPC) = OBSERV(ISPC)
       end if
       SMCON(ISPC) = 0.

       if (DEBUG)write(JOSTND,9030)&
            &DGFOR(ISPFOR,ISPC),DGEL(ISPC),ELEV,DGEL2(ISPC),DGSASP(ISPC),&
            &ASPECT,DGCASP(ISPC),DGSLOP(ISPC),XSLOPE,DGSLSQ(ISPC),&
            &DGMAI(ISPC),RMAI,DGSITE(ISPC),ALOG(XSITE),SASP,DGCON(ISPC)
       9030 format(' IN DGF 9030',20F10.5)

       !  IF READCORD OR REUSCORD WAS SPECIFIED (LDCOR2 IS TRUE) ADD
       !  LN(COR2) TO THE BAI MODEL CONSTANT TERM (DGCON).  COR2 IS
       !  INITIALIZED TO 1.0 IN BLKDATA.

       28 continue
       if (LDCOR2.and.COR2(ISPC).gt.0.0) DGCON(ISPC) = DGCON(ISPC)&
            &+ ALOG(COR2(ISPC))
       30 continue
    end do
    return
  end subroutine DGFSO

  subroutine DGFNC(LCONS)
    implicit none

    !  NC: ORDINARY AND REVISED POINT-DENSITY EQUATIONS; REDWOOD THROUGH
    !  **RWGSDG**.  ROWS ALREADY ON THE FIVE-YEAR BASIS ARE NOT HALVED.

    logical :: LCONS
    !  DIMENSIONS FOR INTERNAL VARIABLES.

    !     DIAM -- ARRAY LOADED WITH TREE DIAMETERS (PASSED AS AN
    !             ARGUEMENT).
    !     DGLD -- ARRAY CONTAINING COEFFICIENTS FOR THE LOG(DIAMETER)
    !             TERM IN THE DDS MODEL (ONE COEFFICIENT FOR EACH
    !             SPECIES).
    !     DGCR -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
    !             RATIO TERM IN THE DDS MODEL (ONE COEFFICIENT FOR
    !             EACH SPECIES).
    !   DGCRSQ -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
    !             RATIO SQUARED TERM IN THE DDS MODEL (ONE
    !             COEFFICIENT FOR EACH SPECIES).
    !   DGDBAL -- ARRAY CONTAINING COEFFICIENTS FOR THE INTERACTION
    !             BETWEEN BASAL AREA IN LARGER TREES AND LN(DBH) (ONE
    !             COEFFICIENT PER SPECIES).
    !    DGCCFA -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
    !             COMPETITION FACTOR TERM IN THE DDS MODEL (ONE
    !             COEFFICIENT FOR EACH SPECIES, LOADED IN RCON).

    ! Variables
    logical :: DEBUG
    real    :: DGLD(12),DGCR(12),DGCRSQ(12),DGCCFA(12)
    real    :: DGDBAL(12),DGFOR(6,12),DGDS(4,12),DGEL2(12)
    real    :: DGSASP(12),DGCASP(12),DGSLOP(12),DGSLSQ(12),DGSLQ2(12)
    integer :: MAPDSQ(7,12),MAPLOC(7,12)
    real    :: DGSLP2(12),DGSITE(12)
    integer :: OBSERV(12)
    real    :: DGBA(12),DGHAH(12),DGPCCF(12),DGLAT2(5,12)
    real    :: DGDBA2,DGBA2,DGLD2,DGDSQ2,DGCR2,ALBA,ALRD,CONSPP,DGLDS
    real    :: DGCRS,DGCRS2,DGDSQS,DGDBLS,D,ALD,CRID,CR,BAL,HOAVH,PBA,PBAL
    real    :: ALPBA,DDS,TDDS
    integer :: ISPC,I1,I2,I3,I,IPCCF,ILAT,IWHO
    real    :: PRD,DLO,DHI,SDICS,SDICZ,A,B,XMAX,DGLT
    real    :: BRAT,BRATIO
    real    :: ZRD(MAXPLT)

    ! COEFS FOR SPECIES 2,6,9 ARE FROM THE WESSIN VARIANT (DOLPH)

    dimension DGCR2(12),DGDSQ2(12),DGLD2(12),DGBA2(12),DGDBA2(12)

    data DGLD/&
         &0.88425, 0.0    , 0.86990, 1.01718, 1.14082, 0.0    ,&
         &1.23911, 0.99531, 0.0    , 0.96865, 0.99531, 0.0/

    data DGCR/&
         &2.83271, 0.0    , 2.96040, 3.01884, 2.82796, 0.0    ,&
         &-1.20841, 2.08524, 0.0    , 1.5466 , 2.08524, 0.0/

    data DGCRSQ/&
         &-0.84141, 0.0    ,-1.08219,-1.12464,-2.14739, 0.0    ,&
         &2.31782,-0.98396, 0.0    , 0.07152,-0.98396, 0.0/

    data DGDBAL/&
         &-0.00358, 0.0    ,-0.00443,-0.00257,-0.00126, 0.0    ,&
         &-0.00199,-0.00147, 0.0    ,-0.00408,-0.00147, 0.0/

    data DGBA /&
         &0.0    , 0.0    ,-0.01744,-0.16596, 0.0    , 0.0    ,&
         &0.0    , 0.0    , 0.0    , 0.0    , 0.0    , 0.0/

    data DGHAH /&
         &0.0    , 0.0    , 0.0    , 0.0    , 0.56348, 0.0    ,&
         &0.0    , 0.50155, 0.0    , 0.0    , 0.50155, 0.0/

    data DGPCCF/&
         &0.0    , 0.0    , 0.0    , 0.0    , 0.0    , 0.0    ,&
         &0.0    ,-0.00018, 0.0    ,-0.00002,-0.0018,  0.0/

    data DGLD2/&
         &1.52284, 1.26883, 1.92426, 1.53339, 1.26883, 1.41389,&
         &1.41389, 1.52284, 1.53339, 1.63568, 1.41389, 0.0/

    data DGCR2/&
         &0.51033, 0.27986, 0.40047, 0.35739, 0.27986, 0.32660,&
         &0.32660, 0.51033, 0.35739, 0.27516, 0.32660, 0.0/

    data DGDSQ2/&
         &-0.26590,-0.35325,-0.44612,-0.47442,-0.35325,-0.48938,&
         &-0.48938,-0.26590,-0.47442,-0.54162,-0.48938, 0.0/

    data DGBA2/&
         &-0.35579, 0.00000,-0.24596,-0.12359, 0.00000,-0.25287,&
         &-0.25287,-0.35579,-0.12359,-0.20902,-0.25287, 0.0/

    data DGDBA2/&
         &0.00000,-0.79922, 0.00000,-0.44256,-0.79922,-0.16000,&
         &-0.16000, 0.00000,-0.44256,-0.32497,-0.16000, 0.0/

    !  IDTYPE IS A HABITAT TYPE INDEX THAT IS COMPUTED IN **RCON**.
    !  ASPECT IS STAND ASPECT.  OBSERV CONTAINS THE NUMBER OF
    !  OBSERVATIONS BY HABITAT CLASS BY SPECIES FOR THE UNDERLYING
    !  MODEL (THIS DATA IS ACTUALLY USED BY **DGDRIV** FOR CALIBRATION).

    data  OBSERV/&
         &7019,  138, 7019,  395, 1762,  125,  583,&
         &6504,    9,  403, 6504, 8928/

    !  DGCCFA CONTAINS COEFFICIENTS FOR THE CCF TERM BY SPECIES

    data DGCCFA/&
         &-0.06784, 0.0    , 0.0    , 0.0    , 0.0    , 0.0    ,&
         &0.0    , 0.0    , 0.0    , 0.0    , 0.0    , 0.0/

    !  DGFOR CONTAINS LOCATION CLASS CONSTANTS FOR EACH SPECIES.
    !  MAPLOC IS AN ARRAY WHICH MAPS FOREST ONTO A LOCATION CLASS.

    data MAPLOC/&
         &1,1,1,2,3,3,2,&
         &1,1,1,1,1,1,1,&
         &1,1,1,2,3,3,2,&
         &1,1,1,2,3,3,2,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1/

    data DGFOR/&
         &-2.00201,-2.19449,-1.84083, 0.00000, 0.00000, 0.00000,&
         &0.0    , 0.0    , 0.0    , 0.0    , 0.00000, 0.00000,&
         &-2.54402,-2.41928,-2.75656, 0.0    , 0.0    , 0.00000,&
         &-1.88042,-2.06853,-1.69815, 0.0    , 0.00000, 0.00000,&
         &-1.69950, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
         &0.0    , 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
         &-2.68349, 0.0    , 0.0    , 0.0    , 0.0    , 0.0    ,&
         &-0.94563, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
         &0.0    , 0.0    , 0.0    , 0.0    , 0.0    , 0.00000,&
         &-4.6744 , 0.0    , 0.0    , 0.0    , 0.00000, 0.00000,&
         &-0.94563, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
         &0.0    , 0.0    , 0.0    , 0.0    , 0.00000, 0.00000/

    data DGLAT2/&
         &0.1630,  0.1630,  0.1630,  0.1630,  0.1630,&
         &-0.4297, -0.4297, -0.4297, -0.2777, -0.4297,&
         &-0.1043, -0.1043, -0.1043, -0.1043, -0.1043,&
         &0.1434,  0.3191,  0.1434,  0.2246,  0.1434,&
         &-0.4297, -0.4297, -0.4297, -0.2777, -0.4297,&
         &0.0540, -0.1232,  0.0540,  0.0540,  0.0540,&
         &0.0540, -0.1232,  0.0540,  0.0540,  0.0540,&
         &0.1630,  0.1630,  0.1630,  0.1630,  0.1630,&
         &0.1434,  0.3191,  0.1434,  0.2246,  0.1434,&
         &0.1035, -0.3955, -0.3955, -0.3955, -0.3995,&
         &0.0540, -0.1232,  0.0540,  0.0540,  0.0540,&
         &0.0000,  0.0000,  0.0000,  0.0000,  0.0000/

    !  DGSLOP CONTAINS THE COEFFICIENTS FOR THE
    !  SLOPE TERM IN THE DIAMETER GROWTH EQUATION.  DGSLSQ CONTAINS
    !  COEFFICIENTS FOR THE (SLOPE)**2 TERM IN THE DIAMETER GROWTH MODELS.
    !  ALL OF THESE ARRAYS ARE SUBSCRIPTED BY SPECIES.

    data DGSLP2/&
         &0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
         &0.00000, 0.00000, 0.00000, 0.80370, 0.00000, 0.00000/
    data DGSLQ2/&
         &0.00000, 0.00000, 0.00000,-0.83400, 0.00000, 0.00000,&
         &0.00000, 0.00000,-0.83400, 0.00000, 0.00000, 0.00000/
    data DGEL2/&
         &0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
         &0.00000, 0.00000,-0.00700, 0.00000, 0.00000, 0.00000/
    data DGSITE/&
         &0.47932, 0.01401, 0.56356, 0.47360, 0.20189, 0.01200,&
         &0.32093, 0.00659, 0.00734, 1.10842, 0.00659, 0.00000/

    !  DGDS CONTAINS COEFFICIENTS FOR THE DIAMETER SQUARED TERMS
    !  IN THE DIAMETER INCREMENT MODELS; ARRAYED BY FOREST BY
    !  SPECIES.  MAPDSQ IS AN ARRAY WHICH MAPS FOREST ONTO A DBH**2
    !  COEFFICIENT.
    data MAPDSQ/&
         &1,1,1,1,2,2,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,2,2,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1/
    data DGDS/&
         &-0.000328,-0.000248,      0.0,      0.0,&
         &0.0     , 0.000000,      0.0,      0.0,&
         &-0.000313, 0.0     , 0.000000, 0.000000,&
         &-0.000356,-0.000268, 0.0     ,      0.0,&
         &-0.000875, 0.000000, 0.000000,      0.0,&
         &0.0     , 0.0     ,      0.0,      0.0,&
         &-0.000338, 0.000000, 0.000000, 0.000000,&
         &-0.000373, 0.000000, 0.000000,      0.0,&
         &0.0     , 0.000000,      0.0,      0.0,&
         &-0.000728, 0.0     , 0.0     ,      0.0,&
         &-0.000373, 0.000000,      0.0,      0.0,&
         &0.0     , 0.0     ,      0.0,      0.0/

    !  DGEL2 CONTAINS THE COEFFICIENTS FOR
    !  THE ELEVATION SQUARED TERM IN THE DIAMETER GROWTH EQUATION.
    !  DGSASP CONTAINS THE COEFFICIENTS FOR THE SIN(ASPECT)*SLOPE
    !  TERM IN THE DIAMETER GROWTH EQUATION.  DGCASP CONTAINS THE
    !  COEFFICIENTS FOR THE COS(ASPECT)*SLOPE TERM IN THE DIAMETER
    !  GROWTH EQUATION.  DGSLOP CONTAINS THE COEFFICIENTS FOR THE
    !  SLOPE TERM IN THE DIAMETER GROWTH EQUATION.  DGSLSQ CONTAINS
    !  COEFFICIENTS FOR THE (SLOPE)**2 TERM IN THE DIAMETER GROWTH MODELS.
    !  ALL OF THESE ARRAYS ARE SUBSCRIPTED BY SPECIES.

    data DGCASP/&
         &-0.14319, 0.0    ,-0.16836,-0.15630,-0.19174, 0.0    ,&
         &0.08632,-0.19935, 0.0    , 0.0    ,-0.19935, 0.0/
    data DGSASP/&
         &-0.02884, 0.0    ,-0.040708,-0.01560,-0.10656, 0.0    ,&
         &-0.11954,-0.03587, 0.0    , 0.0    ,-0.03587,  0.0/
    data DGSLOP/&
         &0.63500, 0.0    , 0.46468, 0.58937,-1.29627, 0.0    ,&
         &0.85815, 0.73530, 0.0    , 0.0    , 0.73530, 0.0/
    data DGSLSQ/&
         &-1.09400, 0.0    ,-0.87145,-1.05045, 0.87335, 0.0    ,&
         &-1.17209,-0.99561, 0.0    , 0.0    ,-0.99561, 0.0/

    ! Check whether debugging is enabled
    if (LCONS) go to 1000

    call DBCHK (DEBUG,'DGF',3,ICYC)
    if (DEBUG) write(JOSTND,3)ICYC
    3 format(' ENTERING SUBROUTINE DGF  CYCLE =',I5)

    !  CALL SDICAL TO LOAD THE POINT MAX SDI WEIGHTED BY SPECIES ARRAY XMAXPT

    IWHO = 2
    call SDICAL (IWHO, XMAX)

    !  LOAD RELATIVE DENSITY (ZEIDI) FOR INDIVIDUAL POINTS.
    !  ALL SPECIES AND ALL SIZES INCLUDED FOR THIS CALCULATION.
    !  POINT RELATIVE DENSITY IS USED FOR RW DG PREDICTIONS.

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

    !  DEBUG OUTPUT: MODEL COEFFICIENTS.

    if (DEBUG)write(JOSTND,*) 'IN DGF,HTCON=',HTCON,&
         &'RMAI=',RMAI,'ELEV=',ELEV,'RELDEN=',RELDEN
    if (DEBUG)&
         &write(JOSTND,9000) DGCON,DGDSQ,DGLD,DGCR,DGCRSQ,DGCCFA
    9000 format(/11(1X,F10.5))

    !  ASSIGN VARIABLES WHICH ARE STAND DEPENDENT.

    ALBA = 0.0
    if (BA.gt.0.0)ALBA = ALOG(BA)
    ALRD = 0.0
    if (RELDEN.gt.0.0)ALRD = ALOG(RELDEN)

    !  BEGIN SPECIES LOOP.  ASSIGN VARIABLES WHICH ARE SPECIES DEPENDENT

    do ISPC = 1,12
       I1 = ISCT(ISPC,1)
       if (I1.eq.0) go to 20
       I2 = ISCT(ISPC,2)

       !  DETERMINE CONSPP BASED ON SPECIES
       !  REDWOOD
       if (ISPC.eq.12) then
          CONSPP = DGCON(ISPC)
          !  ALL OTHER SPECIES
       else
          CONSPP = DGCON(ISPC) + COR(ISPC) + DGCCFA(ISPC)*ALRD&
               &+ DGBA(ISPC)*ALBA
       end if

       DGLDS = DGLD(ISPC)
       DGCRS = DGCR(ISPC)
       DGCRS2 = DGCRSQ(ISPC)
       DGDSQS = DGDSQ(ISPC)
       DGDBLS = DGDBAL(ISPC)

       !  BEGIN TREE LOOP WITHIN SPECIES ISPC.

       do I3 = I1,I2
          I = IND1(I3)
          D = DIAM(I)
          if (D.le.0.0) goto 10
          ALD = ALOG(D)
          CRID = ((ICR(I)*ICR(I))/(ALOG(D+1.0)))/1000.0
          if (D .lt.2.0)CRID = 1.8
          CR = ICR(I)*0.01
          BAL = (1.0 - (PCT(I)/100.)) * BA
          HOAVH = HT(I)/AVH
          if (HOAVH .gt.1.5)HOAVH = 1.5
          PBA = PTBAA(ITRE(I))
          PBAL = PBA*(1.0-(PCT(I)/100.))
          if (PBAL.le.0.0)PBAL = BAL
          if (PBA.le.0.0 )PBA = BA
          ALPBA = 0.0
          if (PBA .gt.0.0)ALPBA = ALOG(PBA)
          IPCCF = ITRE(I)

          !  RELATIVE DENSITY (ZEIDI) ON THE POINT
          !  CONSTRAIN RD IF NEEDED
          if (XMAXPT(ITRE(I)).le.0.0) then
             PRD = 0.0
          else
             PRD = ZRD(ITRE(I)) / XMAXPT(ITRE(I))
          end if

          if (DEBUG) write(JOSTND,333) PBA,D,BA,PBAL,PRD
          333 format(' DGF PBA,D,BA,PBAL,PRD =',4F12.3)

          !   DIFFERENT CALCULATION OF DG FOR SPECIES 2, 6 QND 9. IF NOT THESE
          !   SPECIES SKIP TO CALCULATION FOR OTHER SPECIES.

          !  PROCESS REDWOOD
          select case(ISPC)
          case(12)
             call RWGSDG(ISPC,D,HT(I),CR,PBAL,PRD,CONSPP,DGLT,BRAT,DDS)

             !  RW DEBUG
             if (DEBUG)write(JOSTND,*)'IN DGF - RW DEBUG',' D=',D,&
                  &' PBAL=',PBAL,' PRD=',PRD,' CR=',CR,' SI=',SITEAR(ISPC),&
                  &' SLP=',SLOPE*100,' ASP=',ASPECT, ' DGLT=', DGLT,&
                  &' BRAT=', BRAT,' DDS=',DDS

             !  APPLY COR AND COR2 TO DDS AND THEN CONVERT TO 5-YEAR DDS
             DDS = DDS + COR(ISPC) + LOG(COR2(ISPC))
             TDDS = EXP(DDS)
             DDS = ALOG(TDDS/2.0)

             if (DEBUG)write(JOSTND,*)'IN DGF - RW DEBUG ',' TDDS=',TDDS,&
                  &' DDS=',DDS,' COR(ISPC)=',COR(ISPC),' COR2(ISPC)=',&
                  &COR2(ISPC)

             !  PROCESS SUGAR PINE, INCENSE CEDAR, AND RED FIR
          case(2,6,9)
             DDS = CONSPP + DGLD2(ISPC)*ALD + DGDSQ2(ISPC)*D*D/1000.0&
                  &+ DGCR2(ISPC)*CRID + DGDBA2(ISPC)*PBAL/(ALOG(D+1.0))/100.0&
                  &+ DGBA2(ISPC)*ALPBA

             if (DEBUG) write(JOSTND,334) DDS,CONSPP,ALD,ALPBA,CRID
             334 format(' DGF DDS,CONSPP,ALD,ALPBA,CRID =',5F12.3)

             ! CONVERT TO 5 YEAR RATE IN REAL SCALE

             if (DDS .lt. -8.52) DDS = -8.52
             TDDS = EXP(DDS)
             DDS = ALOG(TDDS/2.0)

             !  PROCESS ALL OTHER SPECIES
          case default
             DDS = CONSPP + DGLDS*ALD  + CR*(DGCRS+CR*DGCRS2)&
                  &+ DGDSQS*D*D  + DGDBLS*BAL/(ALOG(D+1.0))&
                  &+ DGPCCF(ISPC)*PCCF(IPCCF) + DGHAH(ISPC)*HOAVH
             if (ISPC .eq.4)DDS = DDS - 0.15032
          end select

          ! THIS IS A SPECIES DUMMY TO ADJUST THE OTHERWISE DF COEFFICIENTS

          if (DDS.lt.-9.21) DDS = -9.21
          WK2(I) = DDS

          !  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.

          if (DEBUG) then
             write(JOSTND,9001) I,ISPC,D,BAL,CR,RELDEN,BA,DDS
             9001 format(' IN DGF, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,&
                  &',  BAL=',F7.2,',  CR=',F7.4/&
                  &'       RELDEN=',F9.3,',  BA=',F9.3,',   LN(DDS)=',F7.4)
          end if
          10 continue
       end do

       !  END OF SPECIES LOOP.

       20 continue
    end do
    if (DEBUG)write(JOSTND,100)ICYC
    100 format(' LEAVING SUBROUTINE DGF  CYCLE =',I5)
    return
    1000 continue
    call DBCHK (DEBUG,'DGCONS',6,ICYC)

    !  ENTRY POINT FOR LOADING COEFFICIENTS OF THE DIAMETER INCREMENT
    !  MODEL THAT ARE SITE SPECIFIC AND NEED ONLY BE RESOLVED ONCE.

    !  ENTER LOOP TO LOAD SPECIES DEPENDENT VECTORS.

    do ISPC = 1,12
       ILAT = 5
       ISPFOR = MAPLOC(IFOR,ISPC)
       ISPDSQ = MAPDSQ(IFOR,ISPC)

       !   DIFFERENT CALCULATION FOR SPECIES 2,6 AND 9. SKIP SPECIAL
       !   CODE IF NOT ONE OF THESE SPECIES.

       select case(ISPC)

          !  PROCESS REDWOOD
       case(12)
          DGCON(ISPC) = -3.502444 + 0.415435*LOG(SITEAR(ISPC))

          !  PROCESS SUGAR PINE, INCENSE CEDAR, AND RED FIR
       case(2,6,9)
          DGCON(ISPC) = DGLAT2(ILAT,ISPC) + DGEL2(ISPC)*ELEV&
               &+ DGSLP2(ISPC)*SLOPE + DGSLQ2(ISPC)*SLOPE*SLOPE&
               &+ DGSITE(ISPC)*SITEAR(ISPC)

          !  PROCESS ALL OTHER SPECIES
       case default
          DGCON(ISPC) = DGFOR(ISPFOR,ISPC)&
               &+ DGEL2(ISPC) * ELEV * ELEV&
               &+(DGSASP(ISPC) * SIN(ASPECT)&
               &+ DGCASP(ISPC) * COS(ASPECT)&
               &+ DGSLOP(ISPC)) * SLOPE&
               &+ DGSLSQ(ISPC) * SLOPE * SLOPE&
               &+ DGSITE(ISPC)*ALOG(SITEAR(3))
       end select

       DGDSQ(ISPC) = DGDS(ISPDSQ,ISPC)
       ATTEN(ISPC) = OBSERV(ISPC)
       SMCON(ISPC) = 0.

       if (DEBUG)write(JOSTND,9030)&
            &DGFOR(ISPFOR,ISPC),ELEV,DGEL2(ISPC),DGSASP(ISPC),&
            &ASPECT,DGCASP(ISPC),DGSLOP(ISPC),SLOPE,DGSLSQ(ISPC),&
            &RMAI,DGCON(ISPC),SITEAR(ISPC),DGSITE(ISPC)
       9030 format(' IN DGF 9030',7F10.5,/,1H ,11X,7F10.5)

       !  IF READCORD OR REUSCORD WAS SPECIFIED (LDCOR2 IS TRUE) ADD
       !  LN(COR2) TO THE BAI MODEL CONSTANT TERM (DGCON).  COR2 IS
       !  INITIALIZED TO 1.0 IN BLKDATA.
       !  RW DOES NOT HAVE COR2 APPLIED IN THIS CODE SEGMENT SINCE DDS IS
       !  NOT DIRECTLY CALCULATED FOR THIS SPECIES.

       if (LDCOR2.and.COR2(ISPC).gt.0.0) then
          if (ISPC .eq.12) then
             DGCON(ISPC) = DGCON(ISPC)
          else
             DGCON(ISPC) = DGCON(ISPC) + ALOG(COR2(ISPC))
          end if
       end if
       30 continue
    end do
    return
  end subroutine DGFNC

  subroutine DGFWS(LCONS)
    implicit none

    !  WS: CALIFORNIA, DEFAULT AND SPECIES-SPECIFIC EQUATIONS; GS/RW
    !  THROUGH **DGFRWGS** AND GB THROUGH **DGFJUN**.

    logical :: LCONS
    !  DIMENSIONS FOR INTERNAL VARIABLES.

    !     DIAM -- ARRAY LOADED WITH TREE DIAMETERS (PASSED AS AN
    !             ARGUEMENT).
    !     DGLD -- ARRAY CONTAINING COEFFICIENTS FOR THE LOG(DIAMETER)
    !             TERM IN THE DDS MODEL (ONE COEFFICIENT FOR EACH
    !             SPECIES).
    !     DGCR -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
    !             RATIO TERM IN THE DDS MODEL
    !   DGSITE -- ARRAY CONTAINING THE COEFFICIENTS FOR THE SITE
    !             TERM IN THE DDS MODEL
    !    DGBA  -- ARRAY CONTAINING COEFFICIENTS FOR THE BASAL AREA
    !             TERM IN THE DDS MODEL
    !   DGDBA  -- ARRAY CONTAINING COEFFICIENTS FOR THE INTERACTION
    !             BETWEEN BASAL AREA IN LARGER TREES AND LN(DBH)
    !    DGDS  -- ARRAY CONTAINING THE COEFFICIENTS FOR THE DIAMETER
    !             SQUARED TERM IN THE DDS MODEL
    !     DGEL -- ARRAY CONTAINING THE COEFFICIENTS FOR THE ELEVATION TERM.
    !    DGSLP -- ARRAY CONTAINING THE COEFFICIENTS FOR THE SLOPE TERM.
    !    DGSLQ -- ARRAY CONTAINING THE COEFFICIENTS FOR THE SLOPE SQUARED TE
    !    DGLAT -- ARRAY CONTAINING THE COEFFICIENTS FOR THE LATITUDE TERM.

    ! IF SPECIES IS 5 OR 9, USE LEROY'S ORIGINAL WS EQUATIONS WITH SITE.
    ! IF SPECIES IS 7 OR 11, USE NC EQNS FOR BO & TO.
    ! FOR ALL OTHER SPECIES USE EQUATIONS REFIT BY JOHNSON 1992.

    ! IF SPECIES IS 3, 4, 8, OR 9, USE SEPERATE EQUATIONS FOR TREES
    ! SMALLER THAN 10" DBH.

    external RANN
    ! Variables
    logical :: DEBUG
    integer :: OBSERV(43),MAPLOC(7,43)
    integer :: ILAT,I,IPCCF,I1,I2,I3,ISPC,J,ITLAT,IWHO
    real    :: DGLD(43),DGCR(43),DGCRSQ(43),DGDBAL(43),DGBA(43),&
         &DGHAH(43),DGPCCF(43),DGFOR(3,43),DGSITE(43),DGELSQ(43),DGEL(43),DGCASP(43),&
         &DGSASP(43),DGSLOP(43),DGSLSQ(43),DGLAT5(5),DGLAT9(5),DGDS(43)
    real    :: FORCON,CONSJP,CONSPP,DGLDS,DGCRS,DGCRS2,SI
    real    :: DGDBLS,DGPCFS,DGHAHS,DGBAS,D,DPP,HOAVH,CR,ALD,CRID,BAL
    real    :: PBA,PBAL,ALBA,DDS,TDDS,DDSMAX,TSITE,DGDSQS,DGBAL
    real    :: BATEM,DF,DIAGR,BARK,SUMD2,SUMTRE,DGQMD,STDSDI,RELSDI
    real    :: DGFJUN
    real    :: DDSMX1(43),DDSMX2(43),BRATIO,TEMEL
    real    :: PRD,DLO,DHI,SDICS,SDICZ,A,B,XMAX
    real    :: ZRD(MAXPLT)

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

    !    GS AND RW USE EQUATIONS FROM CASTLE 2019.

    !  DATA STATEMENTS

    data DDSMX1/&
         &-3.91, -3.06, -2.81, -3.91, -2.20,&
         &-3.71, -2.81, -2.07,   0.0,   0.0,&
         &-3.91,   0.0, -2.81,   0.0,   0.0,&
         &0.0,   0.0, -2.07,   0.0,   0.0,&
         &0.0, -3.06, -3.91, -3.91,   0.0,&
         &0.0,   0.0, -2.20, -2.20, -2.20,&
         &-2.20, -2.20, -2.20, -2.20, -2.20,&
         &-2.20, -2.20, -2.20, -2.20, -2.20,&
         &0.0, -3.71, -2.20/

    data DDSMX2/&
         &1.42,  1.27,  1.24,  1.42,  1.08,&
         &1.25,  1.24,  1.09,   0.0,   0.0,&
         &1.42,   0.0,  1.24,   0.0,   0.0,&
         &0.0,   0.0,  1.09,   0.0,   0.0,&
         &0.0,  1.27,  1.42,  1.42,   0.0,&
         &0.0,   0.0,  1.08,  1.08,  1.08,&
         &1.08,  1.08,  1.08,  1.08,  1.08,&
         &1.08,  1.08,  1.08,  1.08,  1.08,&
         &0.0,  1.25,  1.08/

    data DGLD/&
         &1.0857,    0.8641,    1.2854,       0.0,   1.29079,&
         &0.51047,       0.0,   0.96103,  1.218279,  1.218279,&
         &1.0857,  1.077154,    1.2854,  1.077154,  1.077154,&
         &1.077154,  1.077154,   0.96103,  1.077154,  1.077154,&
         &0.0,    0.8641,       0.0,    1.0857,  1.077154,&
         &1.077154,  1.077154,   1.23911,   1.23911,   1.23911,&
         &1.23911,   1.23911,   1.23911,   0.99531,   0.99531,&
         &0.99531,   0.99531,   0.99531,   0.99531,   1.23911,&
         &0.889596,    1.1783,   1.23911/

    data DGCR/&
         &0.3910,    0.4246,   -1.0191,       0.0,   -0.0906,&
         &0.91422,       0.0,    0.4126,  3.167164,  3.167164,&
         &0.3910, -0.276387,   -1.0191, -0.276387, -0.276387,&
         &-0.276387, -0.276387,    0.4126, -0.276387, -0.276387,&
         &0.0,    0.4246,       0.0,    0.3910, -0.276387,&
         &-0.276387, -0.276387,  -1.20841,  -1.20841,  -1.20841,&
         &-1.20841,  -1.20841,  -1.20841,   2.08524,   2.08524,&
         &2.08524,   2.08524,   2.08524,   2.08524,  -1.20841,&
         &1.732535,    0.9492,  -1.20841/

    data DGCRSQ/&
         &0.0,       0.0,    0.9104,       0.0,       0.0,&
         &0.27758,       0.0,       0.0, -1.568333, -1.568333,&
         &0.0,  1.063732,    0.9104,  1.063732,  1.063732,&
         &1.063732,  1.063732,       0.0,  1.063732,  1.063732,&
         &0.0,       0.0,       0.0,       0.0,  1.063732,&
         &1.063732,  1.063732,   2.31782,   2.31782,   2.31782,&
         &2.31782,   2.31782,   2.31782,  -0.98396,  -0.98396,&
         &-0.98396,  -0.98396,  -0.98396,  -0.98396,   2.31782,&
         &0.0,       0.0,   2.31782/

    data DGDBAL/&
         &-0.00579,  -0.01127,  -0.00628,       0.0,  -0.00544,&
         &-0.01282,       0.0,  -0.01265,       0.0,       0.0,&
         &-0.00579,       0.0,  -0.00628,       0.0,       0.0,&
         &0.0,       0.0,  -0.01265,       0.0,       0.0,&
         &0.0,  -0.01127,       0.0,  -0.00579,       0.0,&
         &0.0,       0.0,  -0.00199,  -0.00199,  -0.00199,&
         &-0.00199,  -0.00199,  -0.00199,  -0.00147,  -0.00147,&
         &-0.00147,  -0.00147,  -0.00147,  -0.00147,  -0.00199,&
         &-0.001265,  -0.00016,  -0.00199/

    data DGBA /&
         &-0.1313,       0.0,  -0.21056,       0.0,  -0.23182,&
         &-0.01880,       0.0,   -0.1431, -0.267873, -0.267873,&
         &-0.1313,       0.0,  -0.21056,       0.0,       0.0,&
         &0.0,       0.0,   -0.1431,       0.0,       0.0,&
         &0.0,       0.0,       0.0,   -0.1313,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &-0.000981,   -0.3270,       0.0/

    data DGHAH /&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,   0.50155,   0.50155,&
         &0.50155,   0.50155,   0.50155,   0.50155,       0.0,&
         &0.0,       0.0,       0.0/

    data DGPCCF/&
         &-0.00058,  -0.00018,  -0.00091,       0.0,  -0.00098,&
         &-0.00099,       0.0,  -0.00084, -0.000338, -0.000338,&
         &-0.00058,       0.0,  -0.00091,       0.0,       0.0,&
         &0.0,       0.0,  -0.00084,       0.0,       0.0,&
         &0.0,  -0.00018,       0.0,  -0.00058,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,   -0.0018,   -0.0018,&
         &-0.0018,   -0.0018,   -0.0018,   -0.0018,       0.0,&
         &0.0,       0.0,       0.0/

    !  OBSERV CONTAINS THE NUMBER OF OBSERVATIONS  BY HABITAT CLASS BY
    !  SPECIES FOR THE UNDERLYING MODEL.
    !  (THIS DATA IS ACTUALLY USED BY **DGDRIV** FOR CALIBRATION).

    data  OBSERV/&
         &650,      480,     3301,     8928,     1339,&
         &1114,        9,     1528,      372,      372,&
         &650,       84,     3301,       84,       84,&
         &84,       84,     1528,       84,       84,&
         &1000,      480,     8928,      650,       84,&
         &84,       84,      583,      583,      583,&
         &583,      583,      583,     6504,     6504,&
         &6504,     6504,     6504,     6504,      583,&
         &220,      419,      583/

    ! DGLAT CONTAINS LATITUDE CONSTANTS FOR SPECIES 5 & 9.

    data DGLAT5/ -0.4297, -0.4297, -0.4297, -0.2777, -0.4297/
    data DGLAT9/  0.1434,  0.3191,  0.1434,  0.2246,  0.1434/

    !  DGFOR CONTAINS LOCATION CLASS CONSTANTS FOR EACH SPECIES.
    !  MAPLOC IS AN ARRAY WHICH MAPS FOREST ONTO A LOCATION CLASS.
    !  CURRENTLY FOREST CODE WILL BE 1-6 IN THIS VARIANT.

    data ((MAPLOC(I,J),I = 1,7),J = 1,10)/&
         &1,2,2,2,1,2,1,&
         &1,1,2,1,1,1,1,&
         &1,2,3,3,1,3,1,&
         &1,1,1,1,1,1,1,&
         &1,1,2,3,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,2,2,1,2,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1/
    data ((MAPLOC(I,J),I = 1,7),J = 11,20)/&
         &1,2,2,2,1,2,1,&
         &1,1,1,1,1,1,1,&
         &1,2,3,3,1,3,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,2,2,1,2,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1/
    data ((MAPLOC(I,J),I = 1,7),J = 21,30)/&
         &1,1,1,1,1,1,1,&
         &1,1,2,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,2,2,2,1,2,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,1,1,1/
    data ((MAPLOC(I,J),I = 1,7),J = 31,40)/&
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
    data ((MAPLOC(I,J),I = 1,7),J = 41,43)/&
         &1,1,1,1,1,1,1,&
         &1,1,1,1,2,1,1,&
         &1,1,1,1,1,1,1/

    data ((DGFOR(I,J),I = 1,3),J = 1,10)/&
         &-0.70344, -0.90272,      0.0,&
         &-0.5260,  -0.9842,  -0.7603,&
         &0.0755,  -0.3099,  -0.0440,&
         &-1.69950,      0.0,      0.0,&
         &0.02786,  0.21393, -0.04927,&
         &-1.81306,  -2.3037,      0.0,&
         &0.0,      0.0,      0.0,&
         &-0.8882,  -1.0712,      0.0,&
         &-2.058828,      0.0,      0.0,&
         &-2.058828,      0.0,      0.0/
    data ((DGFOR(I,J),I = 1,3),J = 11,20)/&
         &-0.70344, -0.90272,      0.0,&
         &0.564402,      0.0,      0.0,&
         &0.0755,  -0.3099,  -0.0440,&
         &0.564402,      0.0,      0.0,&
         &0.564402,      0.0,      0.0,&
         &0.564402,      0.0,      0.0,&
         &0.564402,      0.0,      0.0,&
         &-0.8882,  -1.0712,      0.0,&
         &0.564402,      0.0,      0.0,&
         &0.564402,      0.0,      0.0/
    data ((DGFOR(I,J),I = 1,3),J = 21,30)/&
         &0.0,      0.0,      0.0,&
         &-0.5260,  -0.9842,  -0.7603,&
         &-1.69950,      0.0,      0.0,&
         &-0.70344, -0.90272,      0.0,&
         &0.564402,      0.0,      0.0,&
         &0.564402,      0.0,      0.0,&
         &0.564402,      0.0,      0.0,&
         &-2.68349,      0.0,      0.0,&
         &-2.68349,      0.0,      0.0,&
         &-2.68349,      0.0,      0.0/
    data ((DGFOR(I,J),I = 1,3),J = 31,40)/&
         &-2.68349,      0.0,      0.0,&
         &-2.68349,      0.0,      0.0,&
         &-2.68349,      0.0,      0.0,&
         &-0.94563,      0.0,      0.0,&
         &-0.94563,      0.0,      0.0,&
         &-0.94563,      0.0,      0.0,&
         &-0.94563,      0.0,      0.0,&
         &-0.94563,      0.0,      0.0,&
         &-0.94563,      0.0,      0.0,&
         &-2.68349,      0.0,      0.0/
    data ((DGFOR(I,J),I = 1,3),J = 41,43)/&
         &-0.107648,      0.0,      0.0,&
         &-0.02772,   0.1327,      0.0,&
         &-2.68349,      0.0,      0.0/

    !  DGEL CONTAINS THE COEFFICIENTS FOR THE ELEVATION TERM IN THE
    !  DIAMETER GROWTH EQUATION.
    !  DGSLOP CONTAINS THE COEFFICIENTS FOR THE
    !  SLOPE TERM IN THE DIAMETER GROWTH EQUATION.  DGSLSQ CONTAINS
    !  COEFFICIENTS FOR THE (SLOPE)**2 TERM IN THE DIAMETER GROWTH MODELS.
    !  ALL OF THESE ARRAYS ARE SUBSCRIPTED BY SPECIES.

    data DGCASP/&
         &0.01664,       0.0,   -0.1804,       0.0,       0.0,&
         &-0.16447,   -0.4919,   0.26986,       0.0,       0.0,&
         &0.01664,  0.649870,   -0.1804,  0.649870,  0.649870,&
         &0.649870,  0.649870,   0.26986,  0.649870,  0.649870,&
         &0.0,       0.0,       0.0,   0.01664,  0.649870,&
         &0.649870,  0.649870,   0.08632,   0.08632,   0.08632,&
         &0.08632,   0.08632,   0.08632,  -0.19935,  -0.19935,&
         &-0.19935,  -0.19935,  -0.19935,  -0.19935,   0.08632,&
         &0.085958,       0.0,   0.08632/

    data DGSASP/&
         &-0.00350,       0.0,   -0.1183,  -0.10656,       0.0,&
         &0.05342,       0.0,   0.09668,       0.0,       0.0,&
         &-0.00350,  0.951834,   -0.1183,  0.951834,  0.951834,&
         &0.951834,  0.951834,   0.09668,  0.951834,  0.951834,&
         &0.0,       0.0,  -0.10656,  -0.00350,  0.951834,&
         &0.951834,  0.951834,  -0.11954,  -0.11954,  -0.11954,&
         &-0.11954,  -0.11954,  -0.11954,  -0.03587,  -0.03587,&
         &-0.03587,  -0.03587,  -0.03587,  -0.03587,  -0.11954,&
         &-0.86398,       0.0,  -0.11954/

    data DGSLOP/&
         &0.7603,       0.0,       0.0,  -1.29627,       0.0,&
         &-0.05469,       0.0,   0.90804,       0.0,       0.0,&
         &0.7603,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,   0.90804,       0.0,       0.0,&
         &0.0,       0.0,  -1.29627,    0.7603,       0.0,&
         &0.0,       0.0,   0.85815,   0.85815,   0.85815,&
         &0.85815,   0.85815,   0.85815,   0.73530,   0.73530,&
         &0.73530,   0.73530,   0.73530,   0.73530,   0.85815,&
         &0.0,       0.0,   0.85815/

    data DGSLSQ/&
         &-2.2339,       0.0,       0.0,   0.87335,       0.0,&
         &0.0,       0.0,  -2.04028,       0.0,       0.0,&
         &-2.2339,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,  -2.04028,       0.0,       0.0,&
         &0.0,       0.0,   0.87335,   -2.2339,       0.0,&
         &0.0,       0.0,  -1.17209,  -1.17209,  -1.17209,&
         &-1.17209,  -1.17209,  -1.17209,  -0.99561,  -0.99561,&
         &-0.99561,  -0.99561,  -0.99561,  -0.99561,  -1.17209,&
         &0.0,       0.0,  -1.17209/

    data DGEL/&
         &0.01919,   0.00489,       0.0,       0.0,  -0.00919,&
         &0.00304,       0.0,   0.00323,       0.0,       0.0,&
         &0.01919,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,   0.00323,       0.0,       0.0,&
         &0.0,   0.00489,       0.0,   0.01919,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &-0.075986,       0.0,       0.0/

    data DGELSQ/&
         &-0.00025,       0.0,       0.0,   0.00019,   0.00019,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &-0.00025,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,   0.00019,  -0.00025,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.001193,       0.0,       0.0/

    data DGSITE/&
         &0.5827,    0.5040,    0.5260,       0.0,    0.3737,&
         &0.96412,       0.0,    0.6828,  0.566946,  0.566946,&
         &0.5827,       0.0,    0.5260,       0.0,       0.0,&
         &0.0,       0.0,    0.6828,       0.0,       0.0,&
         &0.0,    0.5040,       0.0,    0.5827,       0.0,&
         &0.0,       0.0,   0.32093,   0.32093,   0.32093,&
         &0.32093,   0.32093,   0.32093,   0.01200,   0.01200,&
         &0.01200,   0.01200,   0.01200,   0.01200,   0.32093,&
         &0.227307,    0.4401,   0.32093/

    data DGDS /&
         &-0.000288, -0.000290, -0.000584,       0.0,  -0.00061,&
         &-0.000222,       0.0, -0.000375,-0.0014178,-0.0014178,&
         &-0.000288,       0.0, -0.000584,       0.0,       0.0,&
         &0.0,       0.0, -0.000375,       0.0,       0.0,&
         &0.0, -0.000290,       0.0, -0.000288,       0.0,&
         &0.0,       0.0, -0.000338, -0.000338, -0.000338,&
         &-0.000338, -0.000338, -0.000338, -0.000373, -0.000373,&
         &-0.000373, -0.000373, -0.000373, -0.000373, -0.000338,&
         &0.0, -0.000660, -0.000338/

    !  CHECK FOR DEBUG.

    if (LCONS) go to 1100

    call DBCHK (DEBUG,'DGF',3,ICYC)
    if (DEBUG) write(JOSTND,3)ICYC
    3 format(' ENTERING SUBROUTINE DGF  CYCLE =',I5)

    !  DEBUG OUTPUT: MODEL COEFFICIENTS.

    if (DEBUG)write(JOSTND,9000) DGCON
    9000 format(' DGCON =',/,11(1X,F10.5))

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

    !  SPECIES USING SURROGATE EQUATIONS FROM THE CR VARIANT HAVE SPECIAL
    !  NEEDS; COMPUTE THOSE HERE.

    ! COMPUTE STAGNATION MULTIPLIER DSTAG. DSTAG INITIALIZED IN GRINIT.

    SUMD2 = 0.
    SUMTRE = 0.
    DGQMD = 0.
    if (ITRN .eq.0) go to 1001
    do I = 1,ITRN
       SUMD2 = SUMD2+DBH(I)*DBH(I)*PROB(I)
       SUMTRE = SUMTRE+PROB(I)
       1000 continue
    end do
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
       call SDICAL(0,SDIMAX)
       if (SDIMAX.gt.0.)RELSDI = STDSDI/SDIMAX
       if (DEBUG)write(JOSTND,*)' STDSDI,SDIMAX,RELSDI= ',&
            &STDSDI,SDIMAX,RELSDI
       if (RELSDI .gt.0.85) RELSDI = 0.85
       DSTAG = 1.0
       if (RELSDI .gt.0.7) DSTAG = 3.33333 * (1.0 - RELSDI)
       if (DEBUG)write(JOSTND,*)' STAGNATION EFFECT= ',DSTAG
       if (DEBUG)write(JOSTND,*)' STAGNATION FLAGS = ',ISTAGF
    end if

    !  END OF SPECIAL NEEDS SECTION

    !  BEGIN SPECIES LOOP.  ASSIGN VARIABLES WHICH ARE SPECIES DEPENDENT

    do ISPC = 1,MAXSP
       I1 = ISCT(ISPC,1)
       if (I1.eq.0) go to 20
       I2 = ISCT(ISPC,2)
       FORCON = 0.0
       CONSJP = 0.0
       SI = SITEAR(ISPC)

       !  DEFINE CONSPP. RW AND GS DO NOT GET COR ADJUSTMENT UNTIL AFTER
       !  DIAMETER GROWTH PREDICTION.
       if (ISPC .eq.4.or. ISPC .eq.23) then
          CONSPP = DGCON(ISPC)
       else
          CONSPP = DGCON(ISPC) + COR(ISPC)
       end if
       DGLDS = DGLD(ISPC)
       DGCRS = DGCR(ISPC)
       DGCRS2 = DGCRSQ(ISPC)
       DGDSQS = DGDS(ISPC)
       DGDBLS = DGDBAL(ISPC)
       DGPCFS = DGPCCF(ISPC)
       DGHAHS = DGHAH(ISPC)
       DGBAS = DGBA(ISPC)

       !  LOAD A CONSPP VALUE (CONSJP) FOR SMALL:
       !  2=DF, 3=WF, 6=JP, 7=RF, 13=SF, 22=BD

       select case (ISPC)
       case(6)
          FORCON = 0.74162
          if (IFOR .eq.2) FORCON = 0.59493
          if (IGL .eq.2) FORCON = 0.854515
          CONSJP = COR(ISPC) + 0.40657*ALOG(SI) + 0.56709*SLOPE&
               &- 0.14671*COS(ASPECT)*SLOPE + 0.26785*SIN(ASPECT)*SLOPE&
               &- 0.00164*ELEV + FORCON
          if (LDCOR2.and.COR2(ISPC).gt.0.0)CONSJP = CONSJP+ALOG(COR2(ISPC))
       case(2,3,7,13,22)
          CONSJP = COR(ISPC) + 0.233713*ALOG(SI) + 1.53962
          if (LDCOR2.and.COR2(ISPC).gt.0.0)CONSJP = CONSJP+ALOG(COR2(ISPC))
       end select

       !  BEGIN TREE LOOP.

       do I3 = I1,I2
          I = IND1(I3)
          D = DIAM(I)
          if (D.le.0.0) goto 10
          BARK = BRATIO(ISPC,D,HT(I))
          IPCCF = ITRE(I)
          HOAVH = HT(I)/AVH
          CR = FLOAT(ICR(I))/100.
          ALD = ALOG(D)
          CRID = ((ICR(I)*ICR(I))/(ALOG(D+1.0)))/1000.0
          if (D .lt.2.0)CRID = 1.8
          BAL = (1.0 - (PCT(I)/100.)) * BA
          PBA = PTBAA(ITRE(I))
          PBAL = PBA*(1.0-(PCT(I)/100.))
          if (PBAL .lt.0.0)PBAL = BAL
          if (PBA .le.0.0) then
             if (BA .gt.1.0) then
                PBA = BA
             else
                PBA = 1.0
             end if
          end if
          ALBA = 0.0
          if (BA.gt.0.0)ALBA = ALOG(BA)
          !  RELATIVE DENSITY (ZEIDI) ON THE POINT
          !  CONSTRAIN RD IF NEEDED
          if (XMAXPT(ITRE(I)).le.0.0) then
             PRD = 0.0
          else
             PRD = ZRD(ITRE(I)) / XMAXPT(ITRE(I))
          end if

          select case (ISPC)

             !  SPECIES USING EQUATIONS FROM THE CA VARIANT

          case (9:10,12,14:17,19:20,25:27)
             if (HOAVH .gt.1.5)HOAVH = 1.5
             if (ISPC.eq.9.or. ISPC.eq.10) then
                DGBAL = 0.0
             else
                DGBAL = -0.000893
             end if
             DDS = CONSPP + DGLDS*ALD + DGCRS*CR + DGCRS2*CR*CR&
                  &+ DGDSQS*D*D + DGDBLS*BAL/(ALOG(D+1.0))&
                  &+ DGPCFS*PCCF(IPCCF) + DGHAHS*HOAVH&
                  &+ DGBAS*ALBA + DGBAL*BAL

             !  SPECIES USING EQUATIONS FROM THE SO VARIANT

          case (41)
             if (HOAVH .gt.1.5)HOAVH = 1.5
             DDS = CONSPP + DGLDS*ALD + DGCRS*CR + DGCRS2*CR*CR&
                  &+ DGDSQS*D*D + DGDBLS*BAL/(ALOG(D+1.0))&
                  &+ DGPCFS*PCCF(IPCCF) + DGHAHS*HOAVH + DGBAS*BA

             !  SPECIES USING EQUATIONS FROM THE CR VARIANT (VIA UT)
             !  USE SPRUCE-FIR MODEL TYPE LOWER BA LIMIT

             !  THIS ESTIMATE IS NOT USED; TREES OF ALL SIZES ARE GROWN WITH THE
             !  EQUATIONS IN **REGENT**.

          case (21)
             DPP = D
             if (DPP .lt.1.0) DPP = 1.0
             BATEM = BA
             if (BATEM .lt.5.0) BATEM = 5.0
             DF = DGFJUN(DPP,BATEM,SI)
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

             !  RW AND GS EQUATION

          case(4,23)
             call DGFRWGS(I,ISPC,D,CR,BAL,PRD,CONSPP,DEBUG,PBAL,DDS)

             !  SPECIES USING ORIGINAL WS EQUATIONS

          case(7)
             DDS = CONSPP + 1.53339*ALD - 0.47442*D*D/1000.&
                  &+ 0.35739*CRID - 0.44256*PBAL/(ALOG(D+1.0))/100.&
                  &- 0.12359*ALOG(PBA)

          case default
             DDS = CONSPP + DGLDS*ALD + DGCRS*CR + DGCRS2*CR*CR&
                  &+ DGDSQS*D*D + DGDBLS*BAL/(ALOG(D+1.0))&
                  &+ DGPCFS*PCCF(IPCCF) + DGHAHS*HOAVH + DGBAS*ALBA
             if (ISPC.eq.3.or. ISPC.eq.13) DDS = DDS - 0.15032

             !  JP,DF,WF,RF  LESS THAN 10" USE SEPERATE EQUATION.

             if (D.lt.10.0) then
                select case (ISPC)
                case(6)
                   DDS = CONSJP + 1.23864*ALD + 0.64311*CR - 0.48754*ALBA&
                        &- 0.00189*BAL/(ALOG(D+1.)) - 0.00096*PCCF(IPCCF)
                case(2,3,7,13,22)
                   DDS = CONSJP - 0.52776*ALBA + 1.64163*ALD&
                        &- 0.00205*BAL/(ALOG(D+1.)) - 0.00105*PCCF(IPCCF)
                end select
             end if

             !  BO & TO ARE 5-YR EQNS, NEED TO ADJUST TO A 10-YEAR ESTIMATE.

             if ((ISPC.ge.28.and. ISPC.le.40) .or. ISPC.eq.43) then
                TDDS = EXP(DDS)
                DDS = ALOG(TDDS*2.0)
             end if
          end select

          !  TEST RUNS SHOW DG TO SMALL, ADJUST TO MATCH GROWTH DATA. 3-24-94 GD
          !  VALUE 0.3 IS LN(1.35) --- 1.35 IS AVERAGE CALIBRATION VALUE.

          if (ISPC.eq.5) then
             DDS = DDS+0.30*(0.80+0.004*(SI-50.))
          end if

          if (DDS.lt.-9.21) DDS = -9.21
          WK2(I) = DDS

          !  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.

          if (DEBUG) then
             write(JOSTND,9001) I,ISPC,D,PBAL,CRID,PBA,CONSPP,DDS
             9001 format(' IN DGF, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,&
                  &',  PBAL=',F7.2,',  CRID=',F7.4/&
                  &'  PBA=',F9.3,',   CONSPP=',F10.4,',  LN(DDS),=',F7.4)
          end if

          ! CHECK TO SEE THAT PREDICTED DDS DOES NOT EXCEED MAX BOUND

          if (ICR(I).le.0.0) go to 9
          select case (ISPC)
          case (1:3,5:8,11,13,18,22,24,28:33,40,42,43)
             DDSMAX = DDSMX1(ISPC) + DDSMX2(ISPC)*ALOG(ICR(I)*DBH(I))
             if (DDS .gt. DDSMAX) then
                DDS = DDSMAX
                if (DEBUG)write(JOSTND,8)I,ISPC
                8 format(' ******WARNING DDS BOUND BEING INVOKED TREE,SPECIES=',&
                     &2I5)
             end if
          end select
          9 continue
          10 continue
       end do

       !  END OF SPECIES LOOP.

       20 continue
    end do
    if (DEBUG)write(JOSTND,100)ICYC
    100 format(' LEAVING SUBROUTINE DGF  CYCLE =',I5)
    return

    1100 continue

    !  ENTRY POINT FOR LOADING COEFFICIENTS OF THE DIAMETER INCREMENT
    !  MODEL THAT ARE SITE SPECIFIC AND NEED ONLY BE RESOLVED ONCE.
    !  IDTYPE IS A HABITAT TYPE INDEX THAT IS COMPUTED IN **RCON**.

    !  ENTER LOOP TO LOAD SPECIES DEPENDENT VECTORS.

    ITLAT = NINT(TLAT)
    if (ITLAT .le.35.) ILAT = 1
    if (ITLAT .eq.36.) ILAT = 2
    if (ITLAT .eq.37.) ILAT = 3
    if (ITLAT .eq.38.) ILAT = 4
    if (ITLAT .ge.39.) ILAT = 5

    do ISPC = 1,MAXSP
       ISPFOR = MAPLOC(IFOR,ISPC)
       TEMEL = ELEV
       if (ISPC.eq.41.and. TEMEL.gt.30)TEMEL = 30.

       select case (ISPC)

       case(4,23)
          DGCON(ISPC) = -3.502444 + 0.415435*LOG(SITEAR(ISPC))

       case(7)
          DGCON(ISPC) = DGLAT9(ILAT) - 0.00700*ELEV&
               &- 0.83400*SLOPE*SLOPE + 0.00734*SITEAR(ISPC)

       case default
          TSITE = SITEAR(ISPC)
          if ((ISPC.ge.28.and. ISPC.le.40) .or. ISPC.eq.43)&
               &TSITE = SITEAR(2)
          DGCON(ISPC) = DGFOR(ISPFOR,ISPC)&
               &+ DGEL(ISPC)*TEMEL&
               &+ DGELSQ(ISPC)*TEMEL*TEMEL&
               &+ DGSASP(ISPC)*SIN(ASPECT)*SLOPE&
               &+ DGCASP(ISPC)*COS(ASPECT)*SLOPE&
               &+ DGSLOP(ISPC)*SLOPE&
               &+ DGSLSQ(ISPC)*SLOPE*SLOPE&
               &+ DGSITE(ISPC)*ALOG(TSITE)

       end select

       ATTEN(ISPC) = OBSERV(ISPC)
       SMCON(ISPC) = 0.

       !  IF READCORD OR REUSCORD WAS SPECIFIED (LDCOR2 IS TRUE) ADD
       !  LN(COR2) TO THE BAI MODEL CONSTANT TERM (DGCON).  COR2 IS
       !  INITIALIZED TO 1.0 IN BLKDATA.
       !  RW AND GS DO NOT HAVE COR2 APPLIED IN THIS CODE SEGMENT
       !  SINCE DDS IS NOT DIRECTLY CALCULATED FOR THESE SPECIES.

       if (LDCOR2.and.COR2(ISPC).gt.0.0) then
          if (ISPC .eq.4.or. ISPC .eq.23) then
             DGCON(ISPC) = DGCON(ISPC)
          else
             DGCON(ISPC) = DGCON(ISPC) + ALOG(COR2(ISPC))
          end if
       end if
       30 continue
    end do

    return
  end subroutine DGFWS

  subroutine DGFAK(LCONS)
    implicit none

    !  AK: ANNUAL OUTSIDE-BARK DIAMETER INCREMENT (DGPRED, CHANGE IN
    !  DIAMETER) WITH PERMAFROST MODIFIERS, EXPANDED TO THE PERIOD AND
    !  CONVERTED TO LN(DDS) INSIDE BARK.  ITS DGCONS SETS ATTEN AND ADDS
    !  LN(COR2) TO THE INCOMING DGCON; IT DOES NOT RESET DGCON OR SMCON.

    logical :: LCONS
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

    real    :: DGCONB1(23),DGDBAL(23),DGDISQ(23),DGEL(23),DGLD(23),DGLNCR(23), &
         & DGLNSI(23),DGRD(23),DGSASP(23),DGSLOP(23)
    real    :: A,B,D2,DGCOMP1,DGCOMP2,DGPRED,DHI,DLO,PRD,SDICS,SDICZ,TEMSLP,TEMSASP,ZRD(MAXPLT),DDS

    real    :: PFPRES,PFCON(23),PFDBAL(23),PFDSQ,PFEL,PFLD(23),PFLNCR,PFRD(23),PFSASP,PFSLOP
    real    :: PFCOMP1,PFCOMP2,PFMOD,BASEDG

    real    :: DUP,TEMPD1,TEMPD2

    real    :: OBSERV(23)
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
    if (LCONS) go to 1000

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

    1000 continue

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
  end subroutine DGFAK

  subroutine DGFSN(LCONS)
    implicit none

    !  SN: NATIVE SPECIES EQUATIONS WITH FOREST-TYPE, PLANTATION AND
    !  ECOLOGICAL-UNIT TERMS; IFOR=20 OVERRIDES SPECIES 8 AND 13.

    logical :: LCONS
    !     EUT  -- ECOLOGICAL UNIT TYPE IS A USER INPUT VARIABLE IN THE
    !             STAND LIST FILE (EUT=1 MOUNTAIN, EUT=0 NON MOUNTAIN)
    !     DIAM -- ARRAY LOADED WITH TREE DIAMETERS (PASSED AS AN
    !             ARGUEMENT).

    !  DIMENSIONS FOR INTERNAL VARIABLES.

    !     DBHC -- ARRAY CONTAINING COEFFICIENTS FOR THE DIAMETER TERM IN
    !             THE DDS MODEL
    !   INTERC -- DDS REGRESSION EQUATION INTERCEPT
    !    DBHC2 -- ARRAY CONTAINING COEFFICIENTS FOR THE DIAMETER SQUARED
    !             TERM IN THE DDS MODEL
    !     LDBH -- ARRAY CONTAINING COEFFICIENTS FOR THE LOG(DIAMETER)
    !             TERM IN THE DDS MODEL.
    !     CRWN -- ARRAY CONTAINING THE COEFFICIENTS FOR THE PERCENT CROWN
    !             RATIO TERM IN THE DDS MODEL.
    !     CRSQ -- ARRAY CONTAINING THE COEFFICIENTS FOR THE PERCENT CROWN
    !             RATIO SQUARED TERM IN THE DDS MODEL.
    !     BALP -- ARRAY CONTAINING THE COEFFICIENTS FOR COMPETITION
    !             RELATIVE TO TREE SIZE
    !     PLTB -- ARRAY CONTAINING COEFFICIENTS FOR THE BASAL AREA IN
    !             LARGER TREES TERM IN THE DDS MODEL.
    !     PLTT -- ARRAY OF COEFFICIENTS OF BASAL AREA, PLOT BASIS
    !     TANS -- ARRAY OF COEFFICIENTS OF TAN(SLOPE)
    !     FCOS -- ARRAY OF COEFFICIENTS OF COS(ASPECT IN DEG.)
    !     FSIN -- ARRAY OF COEFFICIENTS FOR SIN(ASPECT)

    !  THE FOLOWING ARE ECOLOGICAL UNIT QUALITATIVE COEFFICIENTS

    !    EICEC -- REGRESSION INTERCEPT MODIFIER.
    !             MEANINGFULL WHEN EUT=1 (IE. MOUNTAINOUS SITE)
    !   DBHCEC -- ARRAY CONTAINING COEFFICIENTS FOR THE DIAMETER TERM IN
    !             THE DDS MODEL
    !             MEANINGFULL WHEN EUT=1 (IE. MOUNTAINOUS SITE)
    !   DBH2EC -- ARRAY CONTAINING COEFFICIENTS FOR THE DIAMETER SQUARED
    !             TERM IN THE DDS MODEL
    !             MEANINGFULL WHEN EUT=1 (IE. MOUNTAINOUS SITE)
    !   LDBHEC -- ARRAY CONTAINING COEFFICIENTS FOR THE LOG(DIAMETER)
    !             TERM IN THE DDS MODEL.
    !             MEANINGFULL WHEN EUT=1 (IE. MOUNTAINOUS SITE)
    !   CRWNEC -- ARRAY CONTAINING THE COEFFICIENTS FOR THE PERCENT CROWN
    !             RATIO TERM IN THE DDS MODEL.
    !             MEANINGFULL WHEN EUT=1 (IE. MOUNTAINOUS SITE)
    !   CRSQEC --ARRAY CONTAINING THE COEFFICIENTS FOR THE PERCENT CROWN
    !              RATIO SQUARED TERM IN THE DDS MODEL.
    !             MEANINGFULL WHEN EUT=1 (IE. MOUNTAINOUS SITE)
    !   BALPEC -- ARRAY CONTAINING THE COEFFICIENTS FOR COMPETITION
    !             RELATIVE TO TREE SIZE
    !             MEANINGFULL WHEN EUT=1 (IE. MOUNTAINOUS SITE)
    !   PLTBEC -- ARRAY CONTAINING COEFFICIENTS FOR THE BASAL AREA IN
    !             LARGER TREES TERM IN THE DDS MODEL.
    !             MEANINGFULL WHEN EUT=1 (IE. MOUNTAINOUS SITE)
    !   PLTTEC -- ARRAY OF COEFFICIENTS OF BASAL AREA, PLOT BASIS
    !             MEANINGFULL WHEN EUT=1 (IE. MOUNTAINOUS SITE)
    !   TANSEC -- ARRAY OF COEFFICIENTS OF TAN(SLOPE)
    !             MEANINGFULL WHEN EUT=1 (IE. MOUNTAINOUS SITE)
    !   FCOSEC -- ARRAY OF COEFFICIENTS OF COS(ASPECT IN DEG.)
    !             MEANINGFULL WHEN EUT=1 (IE. MOUNTAINOUS SITE)
    !   FSINEC -- ARRAY OF COEFFICIENTS FOR SIN(ASPECT)
    !             MEANINGFULL WHEN EUT=1 (IE. MOUNTAINOUS SITE)

    !  SPECIES AND COEFFICIENT ORDER

    !         1=FR,   2=JU,   3=PI,   4=PU,   5=SP,   6=SA
    !         7=SR,   8=LL,   9=TM,   10=PP,  11=PD,  12=WP
    !         13=LP,  14=VP,  15=BY,  16=PC,  17=HM,  18=FM
    !         19=BE,  20=RM,  21=SV,  22=SM,  23=BU,  24=BB
    !         25=SB,  26=AH,  27=HI,  28=CA,  29=HB,  30=RD
    !         31=DW,  32=PS,  33=AB,  34=AS,  35=WA,  36=BA
    !         37=GA,  38=HL,  39=LB,  40=HA,  41=HY,  42=BN
    !         43=WN,  44=SU,  45=YP,  46=MG,  47=CT,  48=MS
    !         49=MV,  50=ML,  51=AP,  52=MB,  53=WT,  54=BG
    !         55=TS,  56=HH,  57=SD,  58=RA,  59=SY,  60=CW
    !         61=BT,  62=BC,  63=WO,  64=SO,  65=SK,  66=CB
    !         67=TO,  68=LK,  69=OV,  70=BJ,  71=SN,  72=CK
    !         73=WK,  74=CO,  75=RO,  76=QS,  77=PO,  78=BO
    !         79=LO,  80=BK,  81=WI,  82=SS,  83=BW,  84=EL
    !         85=WE,  86=AE,  87=RL,  88=OS,  89=OH,  90=OT

    ! Variables
    real    :: INTERC(90),LDBH(90),DBH2(90),LCRWN(90),HREL(90),ISIO(90),&
         &OBSERV(90),PLTB(90),PNTBL(90),TANS(90),FCOS(90),FSIN(90)
    real    :: D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11
    real    :: DDS,CONSPP,D,RELHT,BAL,PBA,PBAL,BARK,BRATIO,DG5
    integer :: KLOHD,KNOHD,KOKPN,KSFHP,KUPHD,KUPOK,KYLPN,KPLANT
    integer :: ISPC,I1,I2,I3,I,IK

    !  ECOLOGICAL UNIT CATEGORICAL VARIABLES

    real    :: PM221(90),PM222(90),PM231(90),P221(90),P222(90),S231L(90),S231T(90),&
         &P232(90),P234(90),P255(90),P411(90),FTLOHD(90),FTNOHD(90),FTOKPN(90),&
         &FTSFHP(90),FTUPHD(90),FTUPOK(90),FTYLPN(90),PLANT(90)

    logical :: DEBUG

    integer :: KPM221,KPM222,KPM231,KP221,KP222,KS231T,KS231L,KP232,KP234,KP255,KP411

    !  CHECK FOR DEBUG.

    if (LCONS) go to 1000

    call DBCHK (DEBUG,'DGF',3,ICYC)
    if (DEBUG) write(JOSTND,3)ICYC,&
         &ISCT(22,1),ISCT(33,1),ISCT(90,1)
    3 format(' ENTERING SUBROUTINE DGF  CYCLE =',I5,10I5)

    !  DEBUG OUTPUT: MODEL COEFFICIENTS.

    if (DEBUG) write(JOSTND,*) 'IN DGF,HTCON=',HTCON,&
         &'ELEV=',ELEV,'RELDEN=',RELDEN,&
         &' MANAGD= ',MANAGD

    !  INITIALISZE COEFFICIENTS THAT WHICH ARE APPLIED TO THE FOREST TYPE
    !  GROUP CATEGORICAL VARIABLES

    KLOHD = 0
    KNOHD = 0
    KOKPN = 0
    KSFHP = 0
    KUPHD = 0
    KUPOK = 0
    KYLPN = 0

    !  SET COEF FOR MANAGED KEYWORD

    KPLANT = 0
    if (MANAGD .gt.0) KPLANT = 1

    !  SEARCHG FOR FOREST TYPE GROUP FOR THE
    !  APPLICATION OF ASSOCIATED CATEGORICAL VARIABLE

    if ((IFORTP .eq.701) .or. (IFORTP .eq.801) .or.&
         &(IFORTP .eq.805)) then

       KNOHD = 1

    else if ((IFORTP .eq.104) .or. (IFORTP .eq.105) .or.&
         &(IFORTP .eq.121) .or. (IFORTP .eq.124)) then

       KSFHP = 1

    else if ((IFORTP .eq.141) .or. (IFORTP .eq.142) .or.&
         &(IFORTP .eq.161) .or. (IFORTP .eq.162) .or.&
         &(IFORTP .eq.163) .or. (IFORTP .eq.164) .or.&
         &(IFORTP .eq.166)) then

       KYLPN = 1

    else   if ((IFORTP .eq.165) .or. (IFORTP .eq.403) .or.&
         &(IFORTP .eq.404) .or. (IFORTP .eq.405) .or.&
         &(IFORTP .eq.406) .or. (IFORTP .eq.407) .or.&
         &(IFORTP .eq.409)) then

       KOKPN = 1

    else   if ((IFORTP .eq.501) .or. (IFORTP .eq.502) .or.&
         &(IFORTP .eq.503) .or. (IFORTP .eq.504) .or.&
         &(IFORTP .eq.505) .or. (IFORTP .eq.510) .or.&
         &(IFORTP .eq.514) .or. (IFORTP .eq.515)) then

       KUPOK = 1

    else if ((IFORTP .eq.168) .or. (IFORTP .eq.508) .or.&
         &(IFORTP .eq.601) .or. (IFORTP .eq.602) .or.&
         &(IFORTP .eq.605) .or. (IFORTP .eq.606) .or.&
         &(IFORTP .eq.607) .or. (IFORTP .eq.608) .or.&
         &(IFORTP .eq.702) .or. (IFORTP .eq.703) .or.&
         &(IFORTP .eq.704) .or. (IFORTP .eq.705) .or.&
         &(IFORTP .eq.706) .or. (IFORTP .eq.708)) then

       KLOHD = 1

    else if ((IFORTP .eq.103) .or. (IFORTP .eq.167) .or.&
         &(IFORTP .eq.181) .or. (IFORTP .eq.401) .or.&
         &(IFORTP .eq.402) .or. (IFORTP .eq.506) .or.&
         &(IFORTP .eq.507) .or. (IFORTP .eq.511) .or.&
         &(IFORTP .eq.512) .or. (IFORTP .eq.513) .or.&
         &(IFORTP .eq.519) .or. (IFORTP .eq.520) .or.&
         &(IFORTP .eq.802) .or. (IFORTP .eq.803) .or.&
         &(IFORTP .eq.807) .or. (IFORTP .eq.809)) then

       KUPHD = 1

    end if

    DG5 = 0

    !  BEGIN SPECIES LOOP.  ASSIGN VARIABLES WHICH ARE SPECIES
    !  DEPENDENT

    do ISPC = 1,MAXSP
       I1 = ISCT(ISPC,1)
       if (I1.eq.0) go to 20
       I2 = ISCT(ISPC,2)
       CONSPP = DGCON(ISPC) + COR(ISPC)

       !  BEGIN TREE LOOP WITHIN SPECIES ISPC.

       do I3 = I1,I2
          I = IND1(I3)
          D = DIAM(I)

          if (D.le.0.0) goto 10

          !  RELATIVE HEIGHT

          RELHT = 0.0
          if (AVH .gt.0.0) RELHT = HT(I)/AVH
          if (RELHT .gt.1.5)RELHT = 1.5

          !  SET BA AND BAL TO NONZERO SMALL NUMBERS

          if (BA .le.0.) BA = 25.
          if (ICR(I) .le.0) ICR(I) = 25
          BAL = (1.0 - (PCT(I)/100.)) * BA

          if (DEBUG) write(JOSTND,*) 'IN DGF,D=',D,&
               &'RELDEN=',RELDEN,' SI= ',&
               &SITEAR(ISPC),' ISIO= ',ISIO(ISPC)
          if (DEBUG) write(JOSTND,*) 'IN DGF1,BA=',BA,&
               &'ICR=',ICR(I),'SLOPE=',SLOPE,' ASPECT=',ASPECT

          !  POINT BASEAL AREA LARGER

          PBA = PTBAA(ITRE(I))
          if (PBA.le.0.0 )PBA = BA
          PBAL = PBA*(1.0-(PCT(I)/100.))
          if (PBAL.le.0.0)PBAL = BAL

          !  CALCULATION OF DDS FOR SOUTHERN VARIENT
          !  THE FIRST GROUP OF TERMS REFLECT INDIVIDUALTREE AND STAND CONDITIONS

          DDS = CONSPP + INTERC(ISPC)&
               &+ LDBH(ISPC) * ALOG(D)&
               &+ DBH2(ISPC) * D * D&
               &+ LCRWN(ISPC) * ALOG(FLOAT(ICR(I)))&
               &+ HREL(ISPC) * RELHT&
               &+ PLTB(ISPC) * BA&
               &+ PNTBL(ISPC) * PBAL

          if (DEBUG) then
             D1 = CONSPP + INTERC(ISPC)
             D2 = LDBH(ISPC) * ALOG(D)
             D3 = DBH2(ISPC) * D * D
             D4 = LCRWN(ISPC) * ALOG(FLOAT(ICR(I)))
             D5 = HREL(ISPC) * RELHT
             D6 = ISIO(ISPC) * SITEAR(ISPC)
             D7 = PLTB(ISPC) * BA
             D8 = PNTBL(ISPC) * PBAL
             D9 = TANS(ISPC) * SLOPE
             D10 = FCOS(ISPC) * SLOPE * COS(ASPECT)
             D11 = FSIN(ISPC) * SLOPE * SIN(ASPECT)

             write(JOSTND,222)D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11
             222 format(' IN DGF ,D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11=',&
                  &/,15F7.3)
          end if

          !  THE THIRD GROUP OF TERMS
          !  ARE BASED ON MODEL CALCULATED FOREST TYPE

          DDS = DDS&
               &+ FTLOHD(ISPC) * KLOHD&
               &+ FTNOHD(ISPC) * KNOHD&
               &+ FTOKPN(ISPC) * KOKPN&
               &+ FTSFHP(ISPC) * KSFHP&
               &+ FTUPHD(ISPC) * KUPHD&
               &+ FTUPOK(ISPC) * KUPOK&
               &+ FTYLPN(ISPC) * KYLPN&
               &+ PLANT(ISPC)  * KPLANT

          if (DEBUG) then
             D1 = FTLOHD(ISPC) * KLOHD
             D2 = FTNOHD(ISPC) * KNOHD
             D3 = FTOKPN(ISPC) * KOKPN
             D4 = FTSFHP(ISPC) * KSFHP
             D5 = FTUPHD(ISPC) * KUPHD
             D6 = FTUPOK(ISPC) * KUPOK
             D7 = FTYLPN(ISPC) * KYLPN
             D8 = PLANT(ISPC)  * KPLANT

             write(JOSTND,224)D1,D2,D3,D4,D5,D6,D7, D8
             224 format(' FOREST TYPE D1,D2,D3,D4,D5,D6,D7, D8 =',&
                  &/,15F7.3)
          end if

          !  FORT BRAGG 8=LONGLEAF(121); 13=LOBLOLLY(131)

          if (IFOR .eq.20.and. (ISPC .eq.8.or. ISPC .eq.13)) then
             BARK = BRATIO(ISPC,D,HT(I))
             if (ISPC .eq.8) then
                DG5 = (D*BARK)*((-0.4553*(0.09737-EXP(-0.2428*D)))&
                     &+ 0.05574*(FLOAT(ICR(I))/100.) - 0.0002965*BA&
                     &- 0.00002481*PBA - 0.001192*((PCT(I)/100.)**(-0.9663))&
                     &+ 0.0010110*SITEAR(ISPC) - 0.007711*RELHT)
             else if (ISPC .eq.13) then
                DG5 = (D*BARK)*((-0.3428*(-0.1741-EXP(-0.1328*D)))&
                     &+ 0.1145*(FLOAT(ICR(I))/100.) - 0.0001682*BA&
                     &- 0.00003978*PBA - 0.159400*((PCT(I)/100.)**(-0.1299))&
                     &+ 0.0006204*SITEAR(ISPC) + 0.02474*RELHT)
             end if
             if (DG5.lt.0.01)DG5 = 0.01
             DDS = ALOG((DG5*(2.0*D*BARK+DG5)))
             if (DEBUG)write(JOSTND,220)ISPC,I,D,HT(I),BARK,ICR(I),BA,PBA,&
                  &PCT(I),SITEAR(ISPC),RELHT,DG5,DDS
             220 format(' ISPC,I,D,H,BARK,ICR,BA,PBA,PCT,SI,',&
                  &'RELHT,DG5,DDS= ',/,2I5,2F8.2,F8.5,I5,6F10.4,F10.6)
          end if

          if (DDS.lt.-9.21) DDS = -9.21

          WK2(I) = DDS

          !  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.

          if (DEBUG) then
             write(JOSTND,9001) I,ISPC,DDS
             9001 format(' IN DGF, I=',I4,',  ISPC=',I3,',  DDS=',F7.4)
             write(JOSTND,*)' IN DGF, DG= ',(DG(IK), IK = 1,50)
          end if
          10 continue
       end do

       !  END OF SPECIES LOOP.

       20 continue
    end do
    if (DEBUG) write(JOSTND,100)ICYC
    100 format(' LEAVING SUBROUTINE DGF  CYCLE =',I5)
    return
    1000 continue

    !  ENTRY POINT FOR LOADING COEFFICIENTS OF THE DIAMETER INCREMENT
    !  MODEL THAT ARE SITE SPECIFIC AND NEED ONLY BE RESOLVED ONCE.
    !  OBSERV CONTAINS THE NUMBER OF
    !  OBSERVATIONS BY SPECIES FOR THE UNDERLYING
    !  MODEL (THIS DATA IS ACTUALLY USED BY **DGDRIV** FOR CALIBRATION).

    !  CHECK FOR DEBUG.

    call DBCHK (DEBUG,'DGF',3,ICYC)

    data INTERC/&
         &-2.267851,  -1.864431,  -2.267851,  -3.791466,  -0.008942,&
         &-1.641698,  -2.431165,  -1.331052,  -2.600803,  -3.639059,&
         &-2.353114,  -3.497764,   0.222214,  -2.600803,  -1.735969,&
         &-4.224977,  -2.267851,  -1.685778,  -0.871047,  -2.260482,&
         &-2.260482,  -2.313444,  -1.876225,  -1.092055,  -1.092055,&
         &-1.281144,  -2.728289,  -1.068980,  -0.833167,  -1.062539,&
         &-2.540719,  -2.524455,  -1.251887,  -2.954457,  -1.315283,&
         &-0.897707,  -0.897707,  -0.314922,  -2.514589,  -2.352258,&
         &-1.981934,  -2.354190,  -2.354190,  -1.324147,  -2.513351,&
         &-2.516823,  -1.239592,  -1.477929,  -2.516823,  -1.477929,&
         &-1.746231,  -1.746231,  -2.721782,  -1.508549,  -2.555720,&
         &-1.431973,  -3.180040,  -2.096616,  -1.012995,  -1.068980,&
         &-1.068980,  -2.610023,  -1.608339,  -2.284302,  -0.783581,&
         &-0.295485,  -2.698130,  -1.561284,  -0.947174,  -1.948938,&
         &-1.321662,  -2.223515,  -0.845477,  -2.900655,  -2.732646,&
         &-0.328678,  -1.430321,  -2.345845,  -3.640660,  -1.307911,&
         &-1.109398,  -1.745126,  -1.848106,  -2.356235,  -0.790138,&
         &-0.510736,  -0.229212,  -1.864431,  -1.431973,  -1.645961/

    data LDBH/&
         &1.442529,   1.403065,   1.442529,   1.796179,   1.238170,&
         &1.461093,   1.691731,   1.098112,   1.525435,   1.397394,&
         &1.425614,   1.339503,   1.163040,   1.525435,   1.505649,&
         &1.831739,   1.442529,   1.454506,   1.217898,   1.449834,&
         &1.449834,   1.350084,   1.197048,   1.024946,   1.024946,&
         &1.335625,   1.548449,   1.164191,   1.190567,   1.174050,&
         &1.293125,   1.479865,   1.349337,   1.461691,   1.216264,&
         &1.243091,   1.243091,   0.927191,   1.459672,   1.746852,&
         &1.456263,   1.050171,   1.050171,   1.395884,   1.495351,&
         &1.454173,   1.063360,   1.126474,   1.454173,   1.126474,&
         &1.234133,   1.234133,   1.599221,   1.306362,   1.303035,&
         &1.452294,   1.355790,   1.254708,   1.272885,   1.164191,&
         &1.164191,   1.220310,   1.468589,   1.569893,   1.432483,&
         &1.239946,   1.622081,   1.335456,   1.376350,   1.611438,&
         &1.640507,   0.937359,   1.488444,   1.347350,   1.499450,&
         &1.282494,   1.293728,   1.450330,   1.448503,   0.963269,&
         &1.187096,   1.313340,   1.424209,   1.479429,   0.949707,&
         &1.164789,   1.060275,   1.403065,   1.452294,   1.447657/

    data DBH2/&
         &-0.000548,  -0.001237,  -0.000548,  -0.005109,  -0.001170,&
         &-0.002530,  -0.000945,  -0.001834,  -0.003519,  -0.001670,&
         &-0.001694,  -0.000961,  -0.000863,  -0.003519,  -0.000054,&
         &-0.000595,  -0.000548,  -0.000818,  -0.000105,  -0.000931,&
         &-0.000931,  -0.000816,  -0.000778,  -0.000653,  -0.000653,&
         &0.000000,  -0.000761,   0.000000,   0.000000,   0.000000,&
         &-0.000856,  -0.001512,  -0.000447,   0.000000,  -0.000080,&
         &0.000000,   0.000000,   0.000000,  -0.001317,   0.000000,&
         &-0.002061,  -0.000154,  -0.000154,  -0.000490,  -0.000756,&
         &-0.000925,  -0.000092,  -0.000267,  -0.000925,  -0.000267,&
         &-0.000017,  -0.000017,  -0.000162,  -0.000576,   0.000000,&
         &-0.001475,  -0.000784,   0.000000,  -0.000238,   0.000000,&
         &0.000000,  -0.000234,  -0.000778,  -0.000632,  -0.000412,&
         &-0.000163,  -0.001703,  -0.000412,  -0.000482,  -0.000844,&
         &-0.000285,   0.000212,  -0.000286,  -0.000533,  -0.000729,&
         &-0.000351,  -0.000452,  -0.000674,  -0.000210,   0.000000,&
         &0.000000,  -0.000179,  -0.001488,  -0.000517,   0.000000,&
         &0.000000,   0.000000,  -0.001237,  -0.001475,  -0.002158/

    data LCRWN/&
         &0.568468,   0.273616,   0.568468,   0.902185,   0.053076,&
         &0.265872,   0.588558,   0.184512,   0.615731,   0.739443,&
         &0.455833,   0.759060,   0.028483,   0.615731,   0.132441,&
         &0.446234,   0.568468,   0.242436,   0.240101,   0.361311,&
         &0.361311,   0.394806,   0.183857,   0.206770,   0.206770,&
         &0.111128,   0.203837,   0.084279,   0.193368,   0.239942,&
         &0.368481,   0.289171,   0.193148,   0.377819,   0.087907,&
         &0.090158,   0.090158,   0.103234,   0.654209,   0.291502,&
         &0.215249,   0.425328,   0.425328,   0.145539,   0.530123,&
         &0.252335,   0.243097,   0.134257,   0.252335,   0.134257,&
         &0.285511,   0.285511,   0.351271,   0.112403,   0.319301,&
         &0.061077,   0.532236,   0.344067,   0.234530,   0.084279,&
         &0.084279,   0.520739,   0.139456,   0.272422,   0.044460,&
         &0.020622,   0.354924,   0.246163,   0.099697,   0.135696,&
         &0.038193,   0.286311,   0.053292,   0.300133,   0.344764,&
         &0.071625,   0.047937,   0.251441,   0.549199,   0.268621,&
         &0.202056,   0.201048,   0.289244,   0.425367,   0.100266,&
         &0.127893,   0.116801,   0.273616,   0.061077,   0.241038/

    data ISIO/&
         &-0.001151,  -0.000374,  -0.001151,  -0.002009,   0.004723,&
         &0.006851,   0.000109,   0.008774,   0.001033,   0.008731,&
         &0.007876,   0.004214,   0.005018,   0.001033,   0.003996,&
         &0.005975,  -0.001151,   0.004360,  -0.000022,   0.003444,&
         &0.003444,  -0.000542,   0.010254,   0.002354,   0.002354,&
         &0.005347,   0.004399,   0.009700,  -0.000056,  -0.005893,&
         &0.004257,   0.003369,  -0.000287,   0.007104,   0.003424,&
         &-0.000465,  -0.000465,   0.003038,  -0.003582,   0.003239,&
         &0.004240,   0.001348,   0.001348,   0.001993,   0.000746,&
         &0.007223,  -0.003084,   0.005884,   0.007223,   0.005884,&
         &0.002596,   0.002596,   0.001783,   0.003356,   0.006498,&
         &0.004856,   0.005170,   0.002656,   0.000406,   0.009700,&
         &0.009700,   0.003897,   0.004530,   0.005065,   0.003012,&
         &0.003830,   0.000682,   0.004714,   0.001280,   0.002740,&
         &0.005844,   0.008621,   0.005330,   0.008415,   0.004632,&
         &-0.001173,   0.005466,   0.005921,   0.009377,   0.003713,&
         &0.006303,   0.004206,   0.003499,   0.002767,   0.004360,&
         &0.000450,  -0.001806,  -0.000374,   0.004856,  -0.003250/

    data TANS/&
         &-0.241408,  -0.231988,  -0.241408,   0.501409,  -0.704687,&
         &-0.018479,  -0.324278,   0.225213,  -0.217771,  -0.317178,&
         &0.685018,  -0.372738,  -0.759347,  -0.217771,  -0.560585,&
         &2.356739,  -0.241408,  -0.339807,  -0.613177,  -0.097604,&
         &-0.097604,  -0.032482,  -0.178079,  -0.192793,  -0.192793,&
         &-0.224489,  -0.245761,   0.075733,  -0.179044,  -0.391784,&
         &-0.282662,  -0.327799,  -0.380400,  -0.619659,   0.018297,&
         &-0.158064,  -0.158064,  -0.107755,  -3.469125,   0.335703,&
         &-0.303627,   0.133415,   0.133415,  -0.502977,  -0.321777,&
         &-0.025555,  -0.308453,   0.405735,  -0.025555,   0.405735,&
         &-0.872861,  -0.872861,  -0.023349,  -0.296628,  -0.130105,&
         &-0.422675,  -0.292680,  -1.762191,  -0.092471,   0.075733,&
         &0.075733,  -0.071719,  -0.223273,  -0.204324,  -0.312121,&
         &-0.105862,  -1.084846,  -0.704548,  -0.666854,  -0.071837,&
         &-1.251051,  -0.065269,  -0.151128,  -0.133852,  -0.174932,&
         &-0.692617,  -0.408878,  -0.307814,  -0.099727,  -0.335364,&
         &-0.135482,  -0.013340,  -0.580068,  -1.000956,  -0.439374,&
         &-0.446461,   0.158824,  -0.231988,  -0.422675,  -1.080004/

    data FCOS/&
         &0.066508,   0.077542,   0.066508,  -1.788107,   0.127667,&
         &-0.193157,   0.526867,   0.086883,   0.018819,   0.083538,&
         &-2.907934,  -0.085193,   0.185360,   0.018819,  -0.428140,&
         &-8.639114,   0.066508,  -0.204878,   0.315909,  -0.069753,&
         &-0.069753,  -0.009543,   0.187157,  -0.112449,  -0.112449,&
         &0.031993,   0.055798,  -0.601088,   0.039673,   0.039121,&
         &-0.084011,  -0.200696,   0.160386,   0.020132,  -0.001091,&
         &-0.138225,  -0.138225,   0.958763, -10.149549,  -0.657751,&
         &0.210525,  -0.209729,  -0.209729,   0.141477,  -0.001645,&
         &0.149606,  -0.071496,  -0.063579,   0.149606,  -0.063579,&
         &-0.078948,  -0.078948,   2.073647,  -0.131968,  -0.053688,&
         &-0.236535,   0.050523,   1.202555,   0.494599,  -0.601088,&
         &-0.601088,  -0.028196,   0.008526,  -0.004871,   0.051221,&
         &0.191286,  -0.196932,   0.421803,  -0.977891,  -0.147407,&
         &-0.342161,  -0.008808,   0.073469,  -0.056050,  -0.042528,&
         &0.176391,   0.009165,  -0.033568,  -0.174200,  -0.084806,&
         &-0.083002,  -0.021681,   0.235707,  -0.064524,   0.272172,&
         &0.228712,  -0.216082,   0.077542,  -0.236535,   0.112883/

    data FSIN/&
         &0.086959,  -0.025792,   0.086959,  -1.231519,   0.028391,&
         &-0.251016,   0.009866,   0.107445,  -0.052142,   0.150686,&
         &1.683401,  -0.035582,  -0.072842,  -0.052142,  -0.739509,&
         &5.615465,   0.086959,  -0.147110,  -0.237088,   0.094162,&
         &0.094162,   0.005581,  -0.108544,   0.113349,   0.113349,&
         &-0.160702,   0.080648,  -0.757088,  -0.071628,  -0.038350,&
         &0.104748,  -0.145939,  -0.088382,  -0.020785,  -0.021565,&
         &-0.081197,  -0.081197,   0.823135,   1.404412,   0.585839,&
         &-0.159362,   0.014948,   0.014948,   0.003549,   0.064815,&
         &0.032438,   0.265688,   0.017095,   0.032438,   0.017095,&
         &-0.023609,  -0.023609,  -2.213173,  -0.031596,   0.149549,&
         &0.201149,  -0.008687,   0.383865,  -0.367667,  -0.757088,&
         &-0.757088,  -0.180664,  -0.032889,   0.048758,   0.109366,&
         &-0.324254,   0.514630,   0.296509,  -0.311322,   0.241882,&
         &0.077775,   0.471906,  -0.165679,   0.003947,   0.110336,&
         &0.069174,   0.054469,   0.054762,  -0.102828,   0.127896,&
         &0.059529,  -0.069965,  -0.055862,  -0.146557,   0.248287,&
         &0.069806,   0.037837,  -0.025792,   0.201149,   0.158354/

    data HREL/&
         &-0.403762,   0.177408,  -0.403762,   0.000000,   0.040334,&
         &0.069104,  -0.326169,   0.388018,   0.059646,  -0.193198,&
         &-0.198222,   0.605201,   0.006935,   0.059646,  -0.119572,&
         &-0.125847,  -0.403762,  -0.140837,   0.071213,   0.282436,&
         &0.282436,   0.631803,   0.547747,   0.489441,   0.489441,&
         &-0.244632,   0.570012,   0.501307,   0.508738,   0.411945,&
         &-0.611245,   0.243575,   0.279322,   0.185353,   0.487191,&
         &0.496594,   0.496594,   0.538379,   0.106808,  -1.771604,&
         &-0.414064,   0.616257,   0.616257,   0.256765,   0.161718,&
         &0.243666,   0.533202,   0.539000,   0.243666,   0.539000,&
         &0.180572,   0.180572,   0.133939,   0.121169,  -0.109064,&
         &-0.208915,   0.077021,   0.176829,   0.332494,   0.501307,&
         &0.501307,   0.181418,   0.358369,   0.206868,   0.241383,&
         &0.431515,  -0.033854,   0.381808,   0.427943,   0.056739,&
         &0.086499,   0.733279,   0.130856,   0.597032,   0.466082,&
         &0.455600,   0.514612,   0.527504,   0.073672,   0.396699,&
         &0.093966,   0.542693,   0.253525,  -0.083383,   0.348136,&
         &0.516517,   0.430393,   0.177408,  -0.208915,   0.719652/

    data PLTB/&
         &-0.000824,  -0.003620,  -0.000824,  -0.005287,  -0.004394,&
         &-0.002939,  -0.001847,  -0.002182,  -0.002304,  -0.002257,&
         &-0.003001,  -0.000865,  -0.003408,  -0.002304,  -0.000502,&
         &-0.000191,  -0.000824,  -0.003268,  -0.001242,  -0.002133,&
         &-0.002133,  -0.001413,  -0.004219,  -0.002005,  -0.002005,&
         &-0.003852,  -0.003339,  -0.001041,  -0.001846,  -0.001141,&
         &-0.003144,  -0.001319,  -0.002547,  -0.002027,  -0.000807,&
         &-0.000001,  -0.000001,  -0.001181,  -0.000311,   0.000000,&
         &-0.003096,  -0.000564,  -0.000564,  -0.002978,  -0.001839,&
         &-0.001259,   0.000680,  -0.002740,  -0.001259,  -0.002740,&
         &-0.001709,  -0.001709,  -0.000370,  -0.002482,  -0.001087,&
         &-0.003145,  -0.002389,  -0.002625,  -0.000252,  -0.001041,&
         &-0.001041,  -0.002494,  -0.002807,  -0.001157,  -0.003263,&
         &-0.002257,  -0.002773,  -0.001526,  -0.001416,  -0.003558,&
         &-0.004171,  -0.000299,  -0.003479,  -0.001112,  -0.000908,&
         &-0.000482,  -0.003306,  -0.002235,  -0.003217,  -0.001858,&
         &0.000000,  -0.002593,  -0.001236,  -0.003132,  -0.002975,&
         &-0.003114,  -0.001330,  -0.003620,  -0.003145,   0.000000/

    data PNTBL/&
         &-0.002503,  -0.001890,  -0.002503,  -0.003203,  -0.003271,&
         &-0.004873,  -0.001394,  -0.002898,  -0.002716,  -0.002188,&
         &-0.004510,  -0.004065,  -0.004184,  -0.002716,  -0.000768,&
         &-0.000600,  -0.002503,  -0.001923,  -0.000826,  -0.001383,&
         &-0.001383,  -0.001527,  -0.000586,  -0.001848,  -0.001848,&
         &-0.000644,  -0.001029,  -0.001349,  -0.001776,  -0.002822,&
         &-0.001235,  -0.002784,  -0.001176,  -0.000779,  -0.001262,&
         &-0.001450,  -0.001450,  -0.002489,  -0.002192,  -0.000770,&
         &-0.000394,  -0.000588,  -0.000588,  -0.001940,  -0.002217,&
         &-0.001111,  -0.001643,  -0.000886,  -0.001111,  -0.000886,&
         &-0.000099,  -0.000099,  -0.000262,  -0.001013,  -0.001185,&
         &0.000000,  -0.000797,  -0.000811,  -0.001608,  -0.001349,&
         &-0.001349,  -0.001904,  -0.002269,  -0.001315,  -0.001913,&
         &-0.001826,  -0.002901,  -0.002253,  -0.002636,  -0.001825,&
         &-0.001962,  -0.001711,  -0.001641,  -0.001536,  -0.001131,&
         &-0.002125,  -0.001375,  -0.000862,  -0.000779,  -0.001674,&
         &-0.001075,  -0.000345,  -0.002875,  -0.001352,  -0.001030,&
         &-0.001524,  -0.002098,  -0.001890,   0.000000,  -0.002468/

    data PM221/&
         &0.000000,   0.131771,   0.000000,   0.000000,  -0.569409,&
         &0.000000,   0.000000,   0.000000,  -0.157516,   0.000000,&
         &0.000000,   0.000000,  -0.069716,  -0.157516,   0.000000,&
         &0.000000,   0.000000,   0.000000,  -0.202911,  -0.010003,&
         &-0.010003,  -0.074887,   0.000000,   0.000000,   0.000000,&
         &-0.260665,   0.034070,   0.000000,  -0.435211,   0.147356,&
         &0.101205,   0.111933,  -0.191377,   0.424837,  -0.082465,&
         &0.146900,   0.146900,  -0.256637,   0.000000,   0.000000,&
         &-0.126229,  -0.071073,  -0.071073,   0.214847,  -0.035012,&
         &-1.721210,   0.000000,  -0.184013,  -1.721210,  -0.184013,&
         &0.352576,   0.352576,   0.000000,  -0.014903,   0.000000,&
         &-0.041534,   0.012432,   0.000000,  -0.308437,   0.000000,&
         &0.000000,   0.290593,  -0.191164,  -0.258781,  -0.294531,&
         &-0.303022,   0.000000,   0.000000,   0.000000,  -0.172582,&
         &-0.252410,  -0.039862,   0.197053,   0.000000,   0.000000,&
         &-0.854641,  -0.042633,  -0.122266,   0.000000,   0.000000,&
         &-0.316154,  -0.109616,   0.000000,   0.340206,   0.117770,&
         &-0.312702,  -0.167102,   0.131771,  -0.041534,   0.000000/

    data PM222/&
         &0.000000,   0.217904,   0.000000,   0.000000,  -0.252741,&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000,&
         &0.000000,   0.000000,   0.581967,   0.000000,   0.000000,&
         &0.000000,   0.000000,   0.044974,  -0.354429,  -0.157820,&
         &-0.157820,  -0.216260,   0.000000,   0.379877,   0.379877,&
         &-0.170424,  -0.221662,   0.000000,  -0.117993,   0.184427,&
         &0.352912,  -0.023333,  -0.370162,   0.000000,  -0.149061,&
         &-0.443396,  -0.443396,  -0.149473,   0.000000,   0.000000,&
         &0.000000,   0.374367,   0.374367,  -0.027123,   0.000000,&
         &0.000000,  -0.333954,  -0.708021,   0.000000,  -0.708021,&
         &0.128982,   0.128982,   0.000000,  -0.266804,   0.000000,&
         &0.130420,   0.000000,   0.000000,  -0.468897,   0.000000,&
         &0.000000,   0.163528,  -0.155148,   0.000000,  -0.267829,&
         &-0.568197,   0.000000,   0.000000,   0.000000,   0.107373,&
         &0.000000,  -0.110433,   0.108061,   0.000000,   0.023769,&
         &-0.590369,  -0.252560,  -0.144853,   0.000000,  -0.174431,&
         &0.000000,   0.068664,   0.022312,  -0.330585,  -0.235842,&
         &-0.097041,  -0.292978,   0.217904,   0.130420,   0.000000/

    data PM231/&
         &0.000000,   0.436986,   0.000000,   0.000000,  -0.265699,&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000,&
         &0.000000,   0.000000,   0.790149,   0.000000,   0.000000,&
         &0.000000,   0.000000,  -0.121719,   0.371150,  -0.046780,&
         &-0.046780,   0.000000,   0.000000,  -0.528365,  -0.528365,&
         &-0.314804,  -0.172720,   0.000000,  -0.495339,  -0.070292,&
         &0.166296,   0.289483,  -0.181571,   0.000000,  -0.241677,&
         &-0.448423,  -0.448423,   0.047390,   0.000000,   0.000000,&
         &0.000000,  -0.058790,  -0.058790,  -0.098745,   0.000000,&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000,&
         &0.243569,   0.243569,   0.000000,  -0.251634,   0.000000,&
         &0.212883,   0.000000,   0.000000,  -0.573635,   0.000000,&
         &0.000000,  -0.111387,  -0.154994,   0.000000,  -0.113549,&
         &-0.001804,   0.000000,   0.000000,  -0.506825,  -0.084775,&
         &-0.278470,  -0.170239,  -0.061415,   0.000000,   0.058308,&
         &-0.160338,  -0.243734,  -0.163130,   0.000000,   0.170467,&
         &0.000000,  -0.981901,   0.378775,   0.261703,  -0.435920,&
         &-0.361568,   0.084842,   0.436986,   0.212883,   0.000000/

    data P221/&
         &-0.121082,   0.083896,  -0.121082,   0.000000,  -0.694484,&
         &0.000000,   0.000000,   0.000000,  -0.107642,   0.020105,&
         &0.000000,  -0.065630,  -0.584818,  -0.107642,   0.000000,&
         &0.000000,  -0.121082,   0.000000,  -0.282925,   0.171661,&
         &0.171661,   0.064792,   0.168179,   0.306760,   0.306760,&
         &-0.114680,   0.042510,   0.000000,  -0.347813,   0.281057,&
         &0.233779,   0.480859,  -0.105676,  -0.181787,  -0.146698,&
         &-0.133024,  -0.133024,  -0.341690,   0.000000,   0.000000,&
         &-0.278119,  -0.072556,  -0.072556,   0.311384,   0.114831,&
         &-0.438669,   0.326227,  -0.947356,  -0.438669,  -0.947356,&
         &0.269060,   0.269060,   0.000000,  -0.000477,   0.000000,&
         &0.110701,   0.160384,   0.000000,  -0.163987,   0.000000,&
         &0.000000,   0.060201,  -0.063321,  -0.063421,  -0.146202,&
         &-0.301395,   0.000000,   0.000000,   0.000000,   0.127193,&
         &-0.201638,   0.024645,  -0.343207,   0.151380,   0.043448,&
         &-0.190715,  -0.162999,   0.021869,   0.000000,   0.016664,&
         &-0.360580,  -0.066394,   0.195305,   0.431384,  -0.036156,&
         &-0.127373,  -0.229308,   0.083896,   0.110701,   0.000000/

    data P222/&
         &0.000000,  -0.005539,   0.000000,   0.000000,  -0.285112,&
         &0.000000,   0.000000,   0.000000,  -0.034553,  -0.297952,&
         &0.000000,  -0.450665,  -0.364073,  -0.034553,   0.230225,&
         &0.000000,   0.000000,   0.289979,  -0.250799,   0.170429,&
         &0.170429,   0.000000,   0.024888,   0.167634,   0.167634,&
         &0.043550,  -0.012725,  -0.010636,  -0.336521,   0.322736,&
         &0.104553,   0.215139,   0.093229,   0.000000,   0.000000,&
         &-0.079056,  -0.079056,  -0.215198,   0.000000,   0.000000,&
         &-0.179796,   0.000000,   0.000000,   0.203471,   0.255257,&
         &0.000000,  -0.158005,   0.000000,   0.000000,   0.000000,&
         &0.000219,   0.000219,   0.186240,   0.058798,   0.000000,&
         &-0.130543,   0.079502,   0.000000,  -0.291934,  -0.010636,&
         &-0.010636,   0.089278,  -0.031876,  -0.023554,  -0.078697,&
         &-0.106435,   0.000000,   0.000000,   0.074902,  -0.066699,&
         &0.222377,   0.000000,  -0.056293,   0.200026,   0.036206,&
         &-0.329148,  -0.160463,   0.000000,   0.000000,   0.035772,&
         &0.031358,   0.000000,   0.106462,  -0.107188,  -0.229106,&
         &-0.293653,  -0.287173,  -0.005539,  -0.130543,   0.000000/

    data S231T/&
         &0.022498,   0.000000,   0.022498,   0.000000,  -0.504565,&
         &-0.025549,   0.000000,  -0.175073,   0.000000,   0.236656,&
         &0.000000,   0.102041,  -0.183317,   0.000000,   0.457755,&
         &0.000000,   0.022498,   0.000000,  -0.351211,  -0.031739,&
         &-0.031739,  -0.207182,   0.029753,   0.178981,   0.178981,&
         &-0.090864,   0.000000,  -0.380297,  -0.325780,   0.000000,&
         &0.000000,  -0.049742,   0.006941,   0.073246,  -0.208643,&
         &-0.119819,  -0.119819,  -0.396963,   0.000000,  -0.211742,&
         &0.019636,  -0.015672,  -0.015672,  -0.034773,   0.000000,&
         &-0.316872,  -0.089037,  -0.256347,  -0.316872,  -0.256347,&
         &-0.253490,  -0.253490,  -1.048379,  -0.116789,   0.018150,&
         &-0.025850,   0.000000,   0.000000,  -0.312825,  -0.380297,&
         &-0.380297,   0.000000,   0.000000,   0.000000,  -0.142921,&
         &-0.139157,  -0.141504,  -0.184740,  -0.083491,  -0.052485,&
         &-0.053375,   0.126865,  -0.113493,   0.204279,   0.132129,&
         &-0.237106,  -0.048463,  -0.048649,   0.000000,   0.047888,&
         &0.151013,  -0.153195,  -0.145798,   0.000000,   0.125704,&
         &-0.413743,  -0.014656,   0.000000,  -0.025850,   0.000000/

    data S231L/&
         &-0.097721,   0.490740,  -0.097721,   0.000000,   0.000000,&
         &0.324111,  -0.155764,  -0.067793,   0.025073,   0.000000,&
         &0.000000,   0.000000,   0.256273,   0.025073,   0.154525,&
         &0.000000,  -0.097721,   0.416909,  -0.156560,   0.111637,&
         &0.111637,  -0.145892,   0.000000,   0.365419,   0.365419,&
         &0.087924,   0.116846,  -0.091346,  -0.130359,   0.452072,&
         &0.359721,   0.249350,  -0.003611,   0.000000,  -0.015617,&
         &-0.116966,  -0.116966,   0.095929,   0.000000,   0.000000,&
         &0.163182,   0.349608,   0.349608,   0.115389,   0.095383,&
         &0.014928,   0.587498,  -0.181586,   0.014928,  -0.181586,&
         &0.000000,   0.000000,   0.031801,   0.138867,   0.688179,&
         &0.087497,   0.295282,   0.712083,  -0.204430,  -0.091346,&
         &-0.091346,   0.239920,   0.164710,   0.067643,   0.000000,&
         &0.000000,   0.987526,   0.183278,   0.045341,   0.178459,&
         &0.006612,  -0.010615,   0.076125,   0.295079,   0.163239,&
         &0.000000,   0.000000,   0.151382,   0.000000,   0.371885,&
         &-0.120265,   0.075592,  -0.155027,   0.045265,   0.000000,&
         &-0.127535,   0.000000,   0.490740,   0.087497,   0.000000/

    data P232/&
         &0.000000,   0.399497,   0.000000,   0.000000,  -0.113258,&
         &0.000000,   0.000000,   0.000000,  -0.150946,   0.000000,&
         &0.000000,   0.000000,   0.000000,  -0.150946,   0.000000,&
         &0.000000,   0.000000,   0.163106,  -0.233523,   0.000000,&
         &0.000000,   0.707850,   0.000000,   0.236959,   0.236959,&
         &0.000000,   0.113816,  -0.145951,  -0.212093,   0.552309,&
         &0.093627,   0.000000,   0.000000,   0.000000,  -0.004213,&
         &-0.075746,  -0.075746,  -0.078848,   0.000000,   0.000000,&
         &0.000000,   0.255119,   0.255119,   0.000000,   0.113058,&
         &0.000000,   0.575938,   0.000000,   0.000000,   0.000000,&
         &-0.119399,  -0.119399,   0.000000,   0.000000,   0.000000,&
         &0.000000,   0.057285,   0.000000,  -0.167438,  -0.145951,&
         &-0.145951,   0.125890,   0.031862,   0.087098,  -0.137069,&
         &0.000058,   0.000000,   0.000000,   0.062981,   0.000000,&
         &0.000000,   0.755389,   0.000000,   0.085617,   0.205401,&
         &-0.089086,  -0.081158,   0.084829,   0.000000,   0.171124,&
         &-0.059706,  -0.040417,  -0.071987,   0.036098,   0.070072,&
         &-0.043112,  -0.029868,   0.399497,   0.000000,   0.000000/

    data P234/&
         &0.000000,   0.938548,   0.000000,   0.000000,   0.114097,&
         &0.306793,  -0.112223,   0.123262,   0.000000,   0.000000,&
         &0.000000,   0.000000,   0.281790,   0.000000,   0.021935,&
         &0.000000,   0.000000,   0.451765,   0.000000,   0.283893,&
         &0.283893,   0.289780,   0.000000,   0.417591,   0.417591,&
         &0.215406,   0.183886,   0.000000,   0.000000,   0.255957,&
         &0.488202,   0.363787,  -0.031277,   0.000000,   0.151449,&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000,&
         &-0.283774,   0.259162,   0.259162,   0.129382,   0.111540,&
         &0.161649,   0.572974,   0.493509,   0.161649,   0.493509,&
         &0.032415,   0.032415,  -0.054588,  -0.031436,   0.202859,&
         &0.018385,   0.424776,   0.489307,   0.000000,   0.000000,&
         &0.000000,   0.264833,   0.089972,   0.000000,   0.044499,&
         &-0.035715,   1.109686,   0.238162,   0.000000,  -0.037201,&
         &0.007600,   0.471921,   0.122936,   0.080911,   0.433254,&
         &-0.190850,   0.159575,   0.369759,   0.268416,   0.240479,&
         &0.000000,   0.333962,   0.920288,  -0.318940,  -0.022479,&
         &0.000000,   0.210046,   0.938548,   0.018385,   0.000000/

    data P255/&
         &0.000000,   1.088152,   0.000000,   0.000000,   0.092458,&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000,&
         &0.000000,   0.000000,   0.274618,   0.000000,   0.288209,&
         &0.000000,   0.000000,   0.000000,  -0.011673,   0.000000,&
         &0.000000,   0.000000,   0.000000,   1.151813,   1.151813,&
         &0.000000,   0.441744,  -0.227224,  -0.208957,   0.576422,&
         &0.000000,   0.317870,   0.000000,   0.000000,  -0.056292,&
         &-0.003469,  -0.003469,   0.115297,   0.000000,   0.000000,&
         &0.941544,   0.352483,   0.352483,   0.492438,   0.000000,&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000,&
         &0.498573,   0.498573,   0.000000,   0.375820,   0.000000,&
         &0.255056,   0.000000,   0.000000,   0.122950,  -0.227224,&
         &-0.227224,   1.901340,   0.448682,   0.000000,   0.189435,&
         &-0.198361,   1.446041,   0.768121,  -0.065887,   0.380348,&
         &0.000000,   0.606309,   0.249718,   0.585065,   0.000000,&
         &-0.036830,   0.171401,  -0.051983,   0.000000,   0.444606,&
         &-0.203095,   0.393517,   0.644926,   0.270984,   0.235360,&
         &0.119860,  -0.360256,   1.088152,   0.255056,   0.000000/

    data P411/&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000,&
         &-0.342293,   0.000000,   0.000000,   0.000000,   0.000000,&
         &-0.205608,   0.000000,   0.000000,   0.000000,  -0.033047,&
         &0.156623,   0.000000,   0.000000,   0.000000,  -0.340066,&
         &-0.340066,   0.000000,   0.000000,   0.000000,   0.000000,&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000,&
         &0.000000,   0.000000,   0.000000,  -0.795646,   0.000000,&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000,&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000,&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000,&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000,&
         &0.000000,   0.000000,  -0.147600,   0.000000,   0.000000,&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000,&
         &0.000000,   0.000000,  -1.328540,   0.000000,  -0.741911,&
         &0.000000,   0.000000,  -0.261750,   0.000000,   0.000000,&
         &0.000000,   0.000000,   0.000000,  -0.639461,   0.000000,&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000,&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000/

    data FTLOHD/&
         &0.000000,   0.128828,   0.000000,   1.214007,   0.106418,&
         &0.325861,   0.000000,   0.048216,  -0.059007,   0.000000,&
         &0.187724,  -0.585211,   0.126441,  -0.059007,   0.000000,&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000,&
         &0.000000,   0.177698,   0.276485,   0.000000,   0.000000,&
         &0.000000,   0.307092,   0.000000,   0.000000,  -0.131762,&
         &0.193678,   0.140335,   0.139891,   0.000000,   0.112081,&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000,&
         &0.000000,   0.163749,   0.163749,   0.000000,   0.083904,&
         &0.000000,  -0.295834,   0.000000,   0.000000,   0.000000,&
         &0.000000,   0.000000,   0.000000,   0.114656,   0.000000,&
         &0.000000,   0.076657,   0.000000,   0.000000,   0.000000,&
         &0.000000,   0.181563,   0.214921,   0.081994,   0.148989,&
         &0.000000,  -0.049627,   0.000000,   0.000000,  -0.123167,&
         &0.000000,   0.086112,   0.000000,   0.237228,   0.103731,&
         &0.139953,   0.268975,   0.049693,   0.000000,   0.138241,&
         &0.000000,  -0.058475,   0.067203,   0.000000,   0.211919,&
         &0.000000,   0.000000,   0.128828,   0.000000,   0.000000/

    data FTNOHD/&
         &-0.023264,  -0.020186,  -0.023264,   0.000000,   0.455020,&
         &0.000000,   0.000000,   0.000000,   0.325781,  -0.110161,&
         &0.000000,  -0.062163,  -0.122163,   0.325781,  -0.050765,&
         &0.000000,  -0.023264,  -0.581137,   0.197314,  -0.008575,&
         &-0.008575,   0.081088,   0.000000,  -0.090106,  -0.090106,&
         &0.107053,   0.127121,   0.000000,  -0.020917,  -0.159125,&
         &0.080191,   0.008246,   0.094979,   0.065051,   0.244086,&
         &0.003456,   0.003456,   0.000000,   0.000000,  -0.063207,&
         &-0.003095,  -0.176854,  -0.176854,   0.057604,   0.057388,&
         &-0.326815,   0.049636,  -0.153348,  -0.326815,  -0.153348,&
         &-0.357484,  -0.357484,   0.000000,   0.063092,   0.580807,&
         &-0.154634,   0.352409,  -1.199357,  -0.019656,   0.000000,&
         &0.000000,   0.303382,   0.196181,   0.174080,   0.201455,&
         &0.130043,   0.000000,  -0.127737,  -0.924265,   0.000000,&
         &-0.286340,   0.035396,   0.044529,   0.136033,   0.207951,&
         &0.207603,   0.482744,   0.272007,   0.000000,   0.117354,&
         &0.000000,   0.013995,   0.000000,  -0.005493,   0.112610,&
         &-0.221068,  -0.232430,  -0.020186,  -0.154634,   0.000000/

    data FTOKPN/&
         &0.294886,   0.054362,   0.294886,   0.751229,   0.017518,&
         &0.116235,  -0.098950,   0.088872,   0.045370,  -0.010394,&
         &0.044416,  -0.073668,   0.050835,   0.045370,  -0.201498,&
         &-0.196837,   0.294886,  -0.049388,  -0.002307,  -0.091712,&
         &-0.091712,  -0.007830,   0.000000,  -0.211483,  -0.211483,&
         &-0.184779,   0.075082,  -0.122639,  -0.227013,   0.081533,&
         &0.227760,  -0.030083,   0.112394,  -0.006241,  -0.042491,&
         &-0.108803,  -0.108803,  -0.228299,  -0.063217,   0.000000,&
         &-0.007689,  -0.164651,  -0.164651,  -0.090836,  -0.055234,&
         &-0.017682,  -0.353219,   0.183663,  -0.017682,   0.183663,&
         &0.010020,   0.010020,  -0.203627,   0.076702,  -0.116496,&
         &-0.087145,   0.079881,  -0.022774,  -0.262507,  -0.122639,&
         &-0.122639,   0.162524,   0.106951,   0.073767,   0.055742,&
         &-0.065943,   0.167384,   0.029504,  -0.183514,   0.000279,&
         &0.076760,   0.021275,  -0.042926,   0.030761,   0.010721,&
         &0.009285,   0.042786,   0.034224,   0.074099,  -0.097572,&
         &0.001536,  -0.139506,   0.008236,  -0.263842,   0.174149,&
         &-0.019557,  -0.086137,   0.054362,  -0.087145,   0.000000/

    data FTSFHP/&
         &-0.271743,   0.000000,  -0.271743,   0.000000,   0.000000,&
         &0.000000,   0.000000,   0.000000,   0.091999,   0.043707,&
         &0.000000,  -0.198969,   0.000000,   0.091999,   0.000000,&
         &0.000000,  -0.271743,   0.000000,  -0.361488,  -0.226500,&
         &-0.226500,   0.268289,   0.000000,  -0.112850,  -0.112850,&
         &0.000000,   0.163686,   0.000000,   0.000000,   0.000000,&
         &-0.207255,   0.000000,   0.191476,   0.243386,   0.000000,&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000,&
         &0.000000,  -0.557302,  -0.557302,   0.491153,  -0.499954,&
         &0.000000,   0.000000,  -0.024673,   0.000000,  -0.024673,&
         &0.000000,   0.000000,   0.000000,  -0.447981,   0.000000,&
         &0.000000,  -0.131538,   0.000000,   0.000000,   0.000000,&
         &0.000000,   0.000000,   0.210088,  -0.126888,   0.000000,&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000,&
         &0.000000,   0.000000,   0.000000,  -0.238190,  -0.023986,&
         &0.000000,   0.000000,  -0.104624,   0.000000,   0.000000,&
         &0.000000,   0.414214,   0.000000,   0.000000,   0.000000,&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000/

    data FTUPHD/&
         &-0.004304,   0.079239,  -0.004304,   1.139953,   0.066811,&
         &0.162020,   0.072308,   0.086720,  -0.004333,   0.000000,&
         &0.482241,   0.000000,   0.063669,  -0.004333,   0.194880,&
         &0.000000,  -0.004304,  -0.021913,   0.213336,  -0.115718,&
         &-0.115718,   0.018085,  -0.158379,  -0.150287,  -0.150287,&
         &-0.144979,   0.145813,  -0.227215,  -0.046600,   0.000000,&
         &0.123444,   0.064726,   0.098473,   0.009788,   0.199271,&
         &-0.018640,  -0.018640,  -0.170453,  -0.297261,  -0.111645,&
         &-0.082245,   0.000000,   0.000000,  -0.155894,   0.000000,&
         &-0.029526,   0.008351,   0.055907,  -0.029526,   0.055907,&
         &-0.023979,  -0.023979,   0.066993,   0.111483,  -0.110740,&
         &-0.071133,  -0.023003,   0.060206,  -0.150643,  -0.227215,&
         &-0.227215,   0.146052,   0.100081,   0.089055,   0.068460,&
         &-0.066026,   0.099290,  -0.053091,  -0.083244,   0.170335,&
         &0.037457,   0.111528,  -0.088049,   0.096751,   0.138142,&
         &0.132520,   0.090618,   0.071804,   0.061718,   0.000000,&
         &0.019648,   0.000000,  -0.033491,  -0.054579,   0.168458,&
         &-0.210607,  -0.073558,   0.079239,  -0.071133,   0.000000/

    data FTUPOK/&
         &0.000000,   0.000000,   0.000000,   0.239480,  -0.040181,&
         &0.410684,  -0.055071,   0.106061,  -0.067779,  -0.315855,&
         &0.296549,   0.022013,  -0.016885,  -0.067779,   0.081554,&
         &0.000000,   0.000000,  -0.323458,  -0.003385,  -0.233899,&
         &-0.233899,   0.000000,  -0.066001,  -0.229603,  -0.229603,&
         &-0.125931,   0.000000,   0.428371,  -0.142184,  -0.091293,&
         &0.000000,  -0.063738,   0.000000,  -0.041769,   0.000000,&
         &-0.085510,  -0.085510,  -0.178680,   0.000000,   0.000000,&
         &-0.187742,  -0.156488,  -0.156488,  -0.168272,  -0.090655,&
         &-0.096994,   0.000000,   0.109578,  -0.096994,   0.109578,&
         &-0.024950,  -0.024950,   0.696768,   0.000000,  -0.195302,&
         &-0.204189,   0.000000,   0.390875,  -0.161551,   0.428371,&
         &0.428371,   0.000000,   0.000000,   0.000000,   0.000000,&
         &-0.114309,   0.000000,   0.012219,  -0.057771,   0.000000,&
         &0.010126,   0.000000,  -0.085910,   0.000000,   0.000000,&
         &0.000000,   0.000000,   0.000000,   0.229807,  -0.083039,&
         &0.165022,  -0.166983,   0.012197,  -0.202033,   0.000000,&
         &-0.237209,  -0.242294,   0.000000,  -0.204189,   0.000000/

    data FTYLPN/&
         &0.039012,   0.144288,   0.039012,   0.000000,   0.000000,&
         &0.000000,  -0.236871,   0.000000,   0.000000,   0.116814,&
         &0.000000,   0.046063,   0.000000,   0.000000,  -0.324291,&
         &-0.241760,   0.039012,   0.304165,  -0.252234,  -0.000042,&
         &-0.000042,  -0.969059,   0.000000,  -0.122245,  -0.122245,&
         &-0.023984,   0.091028,  -0.127654,  -0.373160,   0.005318,&
         &0.395749,   0.000000,   0.301892,  -0.049053,   0.256243,&
         &-0.279440,  -0.279440,  -0.343244,   0.065035,   0.000000,&
         &-0.008297,  -0.232630,  -0.232630,   0.058458,   0.053935,&
         &-0.031043,  -0.627226,   0.052422,  -0.031043,   0.052422,&
         &0.272524,   0.272524,   0.045531,   0.229180,   0.006188,&
         &0.120468,   0.300435,  -0.045807,  -0.239422,  -0.127654,&
         &-0.127654,   0.203538,   0.154886,   0.015718,   0.047989,&
         &-0.023093,   0.184647,   0.030690,  -0.076588,  -0.096186,&
         &0.088233,  -0.518898,  -0.068603,   0.053589,   0.052338,&
         &0.219845,   0.056597,  -0.044373,  -0.043042,  -0.046524,&
         &-0.001371,  -0.018172,   0.148520,   0.071996,   0.364012,&
         &0.137175,   0.177563,   0.144288,   0.120468,   0.000000/

    data PLANT/&
         &0.000000,   0.000000,   0.000000,   0.173758,   0.000000,&
         &0.227572,   0.000000,   0.110751,   0.000000,   0.000000,&
         &0.000000,   0.098090,   0.245669,   0.000000,   0.000000,&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000,&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000,&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000,&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000,&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000,&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000,&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000,&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000,&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000,&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000,&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000,&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000,&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000,&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000,&
         &0.000000,   0.000000,   0.000000,   0.000000,   0.000000/

    data OBSERV /&
         &1141,     4132,     1141,      820,    26008,&
         &18736,      884,     9190,     9860,      735,&
         &2590,     2602,    92313,     9860,     6428,&
         &5578,     1141,      455,     1426,    28402,&
         &28402,     2763,      277,     2107,     2107,&
         &3513,    24404,      972,     4485,      482,&
         &7775,     1219,     4953,     5463,     1989,&
         &3958,     3958,      588,     1483,       89,&
         &3417,      941,      941,    38847,    19207,&
         &6729,      376,      938,     6729,      938,&
         &539,      539,     6104,    10328,    17098,&
         &840,     5689,     1953,     1892,      972,&
         &972,     2944,    27487,     6009,    13442,&
         &4239,     2764,     8105,     2960,     3605,&
         &1504,      831,    24356,    12188,     8401,&
         &505,    14841,    10119,     2916,     1990,&
         &2134,     1577,      763,     3230,     2294,&
         &2024,      791,     4132,      840,      639/

    !  ECOLOGICAL UNITS:
    !  SET THE COEFFICIENTS THAT ARE APPLIED TO THE CATEGORICAL VARIABLES AT
    !  THE PROVINCIAL LEVEL (INDICATED BY P IN THE SECOND CARACTER) AND
    !  SECTION LEVEL (INDICATED BY S IN THE SECOND CARACTER)

    data KPM221/0/,KPM222/0/,KPM231/0/,KP221/0/,KP222/0/,KS231T/0/,&
         &KS231L/0/,KP232/0/,KP234/0/,KP255/0/,KP411/0/

    KPM221 = 0
    KPM222 = 0
    KPM231 = 0
    KP221 = 0
    KP222 = 0
    KS231T = 0
    KS231L = 0
    KP232 = 0
    KP234 = 0
    KP255 = 0
    KP411 = 0

    !  EXAMINE THE STAND ECOLOGICAL UNIT VARIABLE (PASSED IN PCOM) AND
    !  ASSIGN THE COEFFICIENT '1' IF APPROPRIATE. THE ORIGINAL
    !  ASSIGNMENT WAS UPDATED TO INCLUDE 1995 AND 2007 ECOUNIT CODES

    select case (PCOM(1:1))

       !  STAND IS IN MOUNTAINOUS TERRAIN

    case('M')
       select case (PCOM(2:4))
       case('221')
          KPM221 = 1
       case('222','223')
          KPM222 = 1
       case('231')
          KPM231 = 1
       end select

       !  STAND IS IN COASTAL PLAINS, PIEDMONT, LOW PLATTEAU, GULF PRAIRIE ...

    case default
       select case (PCOM(1:3))
       case('221')
          select case (PCOM(4:5))
          case('DD','DE')
             KS231T = 1
          case('EJ','EG','EN')
             KP222 = 1
          case default
             KP221 = 1
          end select
       case('222','223')
          KP222 = 1

          !  PROVINCE 231 IS DIVIDED INTO TWO SECTION LEVEL AREAS.

       case('231')
          select case (PCOM(4:4))
          case('A','C','D','I')
             KS231T = 1
          case('B','E','F','G')
             KS231L = 1
          case('H')
             select case (PCOM(5:5))
             case('A','B','C')
                KS231L = 1
             case('D','E','F')
                KP222 = 1
             case('H','I')
                KP232 = 1
             end select
          end select
       case('232')
          select case (PCOM(4:5))
          case('FF')
             KP234 = 1
          case default
             KP232 = 1
          end select
       case('234')
          KP234 = 1
       case('251')
          KP255 = 1
       case('255')
          select case (PCOM(4:5))
          case('CH')
             KS231L = 1
          case default
             KP255 = 1
          end select
       case('411')
          KP411 = 1
       end select
    end select

    !  ENTER LOOP TO LOAD SPECIES DEPENDENT VECTORS.

    do ISPC = 1,MAXSP
       ATTEN(ISPC) = OBSERV(ISPC)

       !  FORT BRAGG

       if (IFOR.eq.20.and. ISPC.eq.8)ATTEN(8) = 2056
       if (IFOR.eq.20.and. ISPC.eq.13)ATTEN(13) = 689

       DGCON(ISPC) = ISIO(ISPC) * SITEAR(ISPC)&
            &+ TANS(ISPC) * SLOPE&
            &+ FCOS(ISPC) * SLOPE * COS(ASPECT)&
            &+ FSIN(ISPC) * SLOPE * SIN(ASPECT)

       !  THE SECOND GROUP OF TERMS
       !  ARE BASED ON ECOLOGICAL UNIT, CATEGORICAL VARIABLES

       DGCON(ISPC) = DGCON(ISPC)&
            &+ PM221(ISPC) * KPM221&
            &+ PM222(ISPC) * KPM222&
            &+ PM231(ISPC) * KPM231&
            &+ P221(ISPC) *  KP221&
            &+ P222(ISPC) *  KP222&
            &+ S231T(ISPC) * KS231T&
            &+ S231L(ISPC) * KS231L&
            &+ P232(ISPC) *  KP232&
            &+ P234(ISPC) *  KP234&
            &+ P255(ISPC) *  KP255&
            &+ P411(ISPC) *  KP411

       if (DEBUG) then
          D1 = PM221(ISPC) * KPM221
          D2 = PM222(ISPC) * KPM222
          D3 = PM231(ISPC) * KPM231
          D4 = P221(ISPC) *  KP221
          D5 = P222(ISPC) *  KP222
          D6 = S231T(ISPC) * KS231T
          D7 = S231L(ISPC) * KS231L
          D8 = P232(ISPC) *  KP232
          D9 = P234(ISPC) *  KP234
          D10 = P255(ISPC) *  KP255
          D11 = P411(ISPC) *  KP411

          write(JOSTND,223)D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11
          223 format(' EUT ,D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11 =',&
               &/,15F7.3)

       end if

       !  IF READCORD OR REUSCORD WAS SPECIFIED (LDCOR2 IS TRUE) ADD
       !  LN(COR2) TO THE BAI MODEL CONSTANT TERM (DGCON).  COR2 IS
       !  INITIALIZED TO 1.0 IN BLKDATA.

       if (LDCOR2.and.COR2(ISPC).gt.0.0) DGCON(ISPC) = DGCON(ISPC)&
            &+ ALOG(COR2(ISPC))
       999 continue
    end do
    return
  end subroutine DGFSN

  subroutine DGFNE(LCONS)
    implicit none

    !  NE: TEN ANNUAL POTENTIAL BASAL-AREA ITERATIONS PER TREE, MODIFIED BY
    !  **BALMOD**; DDS IS PREDICTED FROM DBH, SITE INDEX, AND BASAL AREA IN
    !  LARGER TREES.

    !  DIAMETER GROWTH EQUATIONS ARE FROM NE-TWIGS VERSION 3.01
    !  OR SEE 'INDIVIDUAL-TREE DIAMETER GROWTH MODEL FOR THE NORTHEASTERN
    !  UNITED STATES' TECK & HILT. RESEARCH PAPER NE-649

    logical :: LCONS
    !  VARIABLES DEFINED:
    !  POTBAG--POTENTIAL BASAL AREA GROWTH (PER YEAR)
    !  BAG   --INDIVIDUAL TREE BASAL-AREA GROWTH RATE (PER YEAR)

    !  DIMENSIONS FOR INTERNAL VARIABLES.

    !       B1 -- ARRAY CONTAINING THE COEFFICIENTS FOR THE SITE
    !       INDEX FACTOR TERM IN THE BASAL AREA GROWTH MODEL
    !       FOR THE NORTHEASTERN UNITED STATES (ONE COEFFICIENT
    !       PER SPECIES).
    ! B2 -- ARRAY CONTAINING THE COEFFICIENTS FOR THE DBH TERM
    !       IN THE BASAL AREA GROWTH MODEL FOR THE NORTHEASTERN
    !       UNITED STATES (ONE COEFFICIENT PER SPECIES).
    ! B3 -- ARRAY CONTAINING THE COEFFICIENTS FOR THE BAL TERM
    !       IN THE BASAL AREA GROWTH MODEL FOR THE NORTHEASTERN
    !       UNITED STATES (ONE COEFFICIENT PER SPECIES).
    !             THESE COEFFICIENTS WERE MOVED TO SUBROUTINE **BALMOD**
    !             SO THE SAME EFFECT COULD BE INCORPORATED INTO SUBROUTINES
    !             **HTGF** AND **RGNTHW**. OTHERWISE, YOU ENDED UP WITH
    !             TALL SKINNY TREES SINCE DG WAS REDUCED AND HTG KEPT ON
    !             GOING.

    ! Variables
    logical :: DEBUG

    real    :: B1(108),B2(108),TEMD(MAXTRE)
    integer :: I,ISPC,I1,I2,I3,ILOOP
    real    :: DGB1,DGB2,D,POTBAG,BAGMOD,DELD,QTRBA,QDBH,BARK,BRATIO
    real    :: DIAGR,DDS
    data B1/&
         &.0008829,.0009933,.0008721,4*.0008236, 0.0009252,&
         &.0011303,.0009252,.0006634,4*.0009050,2*.0008737,&
         &8*.0006634, 0.0007906,3*.0007439,3*.0006668,2*.0009766,&
         &5*.0007993, 0.0006911,5*.0008992,3*.0008815,5*.0011885,&
         &0.0007929,5*.0007417,4*.0008769,3*.0008238,2*.0008920,&
         &2*.0008550,27*.0009567,11*.0003604/
    data B2/&
         &0.0602785, 0.0816995, 0.0578650,4*.0549439, 0.1134195,&
         &0.0934796, 0.1134195, 0.1083470,4*.0517297,2*.0940538,&
         &8*.1083470, 0.0651982,3*.0706905,3*.0768212,2*.0832328,&
         &5*.0779654, 0.0730441,5*.0925395,3*.1419212,5*.0920050,&
         &0.1568904,5*.0867535,4*.0866621,3*.0790660,2*.0979702,&
         &2*.0957964,27*.1038458,11*.0328767/

    ! Check whether debugging is enabled
    if (LCONS) go to 1100

    call DBCHK (DEBUG,'DGF',3,ICYC)
    if (DEBUG) write(JOSTND,3)ICYC
    3 format(' ENTERING SUBROUTINE DGF  CYCLE =',I5)

    !  STORE DIAMETERS FOR ITERATIVE PROCESSING

    do I = 1,MAXTRE
       TEMD(I) = DIAM(I)
       4 continue
    end do

    !  COMPUTE BAL FOR ALL THE TREES

    call BADIST(DEBUG)

    !  BEGIN SPECIES LOOP.  ASSIGN VARIABLES WHICH ARE SPECIES DEPENDENT

    do ISPC = 1,MAXSP
       I1 = ISCT(ISPC,1)
       if (I1.eq.0) go to 20
       I2 = ISCT(ISPC,2)
       DGB1 = B1(ISPC)
       DGB2 = B2(ISPC)

       !  BEGIN TREE LOOP WITHIN SPECIES ISPC.

       do I3 = I1,I2
          I = IND1(I3)

          !  ITERATE 10 TIMES, SINCE DG EQUATION IS AN ANNUAL BASIS

          do ILOOP = 1,10
             D = TEMD(I)
             WK2(I) = 0.0
             if (D .le.0.0) go to 10
             POTBAG = DGB1*SITEAR(ISPC)*(1.0-EXP(-(DGB2*D)))
             POTBAG = POTBAG *0.7
             if (DEBUG)&
                  &write(16,*)' ILOOP,I,ISPC,SITE,D,DGB1,DGB2,POTBAG= ',&
                  &ILOOP,I,ISPC,SITEAR(ISPC),D,DGB1,DGB2,POTBAG

             !  GET BAL MODIFIER AND ADJUST THE DIAMETER GROWTH ESTIMATE.

             call BALMOD(ISPC,D,BAGMOD)
             if (DEBUG) write(JOSTND,*)'I=',I,' POTBAG=',POTBAG,' SITEAR=',&
                  &SITEAR(ISPC), ' D=',D,' ISPC=',ISPC,' BAGMOD=',&
                  &BAGMOD,' PCT(I)=',PCT(I)
             if (DEBUG)write(JOSTND,*)' AFTER BALMOD BAGMOD,HT,AVH= ',&
                  &BAGMOD,HT(I),AVH
             DELD = POTBAG*BAGMOD
             if (DEBUG)&
                  &write(16,*)' I,POTBAG,BAGMOD,DELD= ',I,POTBAG,BAGMOD,DELD
             QTRBA = DELD+(D*D*.0054542)
             QDBH = (QTRBA/.0054542)**.5
             TEMD(I) = QDBH
             1000 continue
          end do

          BARK = BRATIO(ISPC,TEMD(I),HT(I))
          DIAGR = (TEMD(I)-DIAM(I))*BARK
          if (DEBUG)&
               &write(JOSTND,*)' I,TEMD,DIAM,BARK,DIAGR= ',&
               &I,TEMD(I),DIAM(I),BARK,DIAGR
          if (LDCOR2.and. COR2(ISPC) .gt.0.0) DIAGR = DIAGR*COR2(ISPC)
          if (DIAGR.le..0001) DIAGR = .0001
          if (DEBUG)write(16,*)' I,ISPC,COR2,DIAGR,LDCOR2= ',&
               &I,ISPC,COR2(ISPC),DIAGR,LDCOR2
          DDS = DIAGR*(2.0*DIAM(I)*BARK+DIAGR)
          if (DEBUG)write(16,*)' I,DIAGR,DIAM(I),BARK,DDS= ',&
               &I,DIAGR,DIAM(I),BARK,DDS

          if (DEBUG) write(JOSTND,*)' DDS=',DDS,&
               &BAGMOD,'  DDS=',DDS,' DIAGR=',DIAGR

          WK2(I) = ALOG(DDS)+COR(ISPC)
          if (DEBUG)write(16,*)' I,DDS,COR,WK2= ',&
               &I,DDS,COR(ISPC),WK2(I)

          !  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.

          if (DEBUG) then
             write(JOSTND,9001) I,ISPC,D,BA,DDS
             9001 format(' IN DGF, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,&
                  &',  BA=',F9.3,',  LOG(DDS)=',F7.4)
          end if
          10 continue
       end do

       !  END OF SPECIES LOOP.

       20 continue
    end do
    if (DEBUG)write(JOSTND,100)ICYC
    100 format(' LEAVING SUBROUTINE DGF  CYCLE =',I5)
    return
    1100 continue

    !  ENTRY POINT FOR LOADING COEFFICIENTS OF THE DIAMETER INCREMENT
    !  MODEL THAT ARE SITE SPECIFIC AND NEED ONLY BE RESOLVED ONCE.

    do ISPC = 1,MAXSP
       DGCON(ISPC) = 0.
       ATTEN(ISPC) = 1000.
       SMCON(ISPC) = 0.
       30 continue
    end do
    return
  end subroutine DGFNE

  subroutine DGFON(LCONS)
    implicit none

    !  ON: IF SPECIES HAVE BEEN CALIBRATED FOR ONTARIO (FORKOD 915)
    !  THEN DIAMETER GROWTH IS PREDICTED FROM NEW EQUATIONS USING
    !  DBH, SITE INDEX, STAND AVERAGE DBH, AND BASAL AREA. NOTE THAT ALL
    !  MODEL EQUATION PARAMETERS ARE METRIC, AND MUST BE CONVERTE
    !  BEFORE AND AFTER THE CALCULATIONS.

    !  02/23/06 - ONTARIO DIAMETER GROWTH EQUATIONS ARE FROM MARGARET PENNER

    logical :: LCONS
    !  VARIABLES DEFINED:
    !  DELD  -- TREE'S DIAMETER GROWTH FOR ONE YEAR
    !  COEFFICIENTS DEFINED:
    !  OSPMAP   -- SPECIES MAPPING FOR ONTARIO EQUATIONS
    !  DM, SIM, B, Q -- DBH, SI, BA, RMSQD IN METRIC
    !  DGLN     -- LN(DBH_GROWTH) (METRIC)

    !     NUMBER OF LARGE TREE DG EQUATIONS

    ! Variables
    integer :: MAXEQ
    parameter (MAXEQ = 35)

    logical :: DEBUG,LQUAL(72)
    logical :: LHYBRID,LMBLEAD
    real    :: TEMD(MAXTRE),BAMSP(72,2)
    real    :: B0(MAXEQ),B1(MAXEQ),B2(MAXEQ)
    real    :: BBAL(MAXEQ),BHT(MAXEQ),BSI(MAXEQ),BBA(MAXEQ)
    real    :: BDBHQ(MAXEQ),BAGS(MAXEQ),B95(MAXEQ)
    real    :: BARK,D,SI,DELD,DGLN,DIAGR,DDS
    real    :: DBHM,SIM,BAM,QMDM,BALM,HTM
    real    :: AGSBAM,X,A1,A2,BAM2,BAM3,BB
    real    :: PHWBAM
    real    :: R,A,B,C,X1,X2,Y
    integer :: I,J,I1,I2,I3,ISPC,KSP,ILOOP,AGS,ITCR,ITCR2,ICUT,JI
    integer :: OSPMAP(72),OBSERV(MAXEQ)

    real    :: BRATIO

    !     MAPPING OF FVS SPECIES INTO ONTARIO GROWTH SPECIES.

    data OSPMAP/&
         &5,  4,  3,  4,  1,  7,  8, 12,  9, 14,& !10
         &13, 11,  9,  9, 26, 28, 35, 17, 17, 33,& !20
         &17, 17, 17, 19, 29, 15, 15, 24, 28, 24,& !30
         &17, 24, 15, 22, 26, 26, 33, 33, 17, 35,& !40
         &35, 35, 21, 15, 31, 31, 32, 31, 21, 17,& !50
         &17, 17, 32, 17, 17, 17, 17, 17, 17, 17,& !60
         &17, 17, 17, 17, 17, 17, 17, 17,  6,  2,& !70
         &8, 10/

    !     THESE 8 FVS SPECIES HAVE A QUALITY CODE. THE SPECIES
    !     RECORDED BY OSPMAP IS THE FIRST OF THE 2 PAIRS; IF THE
    !     QUALITY CODE REQUIRES IT (IMC = 1; AGS EQN) THE EQUATION
    !     NUMBER IS +1, OTHER LOGIC IS USED TO FETCH THE NECESSARY
    !     COEFFICIENTS

    !     SPECIES            FVS SPP  DG EQN

    !     BLACK ASH          15       26/27
    !     SOFT MAPLE         19       17/18
    !     BLACK CHERRY       20       33/34
    !     YELLOW BIRCH       24       19/20
    !     BASSWOOD           25       29/30
    !     SUGAR MAPLE        26       15/16
    !     AMERICAN BEECH     28       24/25
    !     RED OAK            34       22/23

    data LQUAL /&
         &14*.false.,&
         &.true.,& ! FVS SPP 15 -> 26/27
         &3*.false.,&
         &.true.,& ! FVS SPP 19 -> 17/18
         &.true.,& ! FVS SPP 20 -> 33/34
         &3*.false.,&
         &.true.,& ! FVS SPP 24 -> 19/20
         &.true.,& ! FVS SPP 25 -> 29/30
         &.true.,& ! FVS SPP 26 -> 15/16
         &1*.false.,&
         &.true.,& ! FVS SPP 28 -> 24/25
         &5*.false.,&
         &.true.,& ! FVS SPP 34 -> 22/23
         &38*.false. /

    data B0 /&
         &3.691,   1.8979,  5.451,   1.1499,  4.184,&
         &2.1184,  1.3936,  1.5596,  2.8199,  1.0315,& !10
         &1.4697,  1.4792,  2.3705,  3.3822,  2.256,&
         &2.0375,  1.5782,  1.626,   0.8673,  1.5838,& !20
         &2.6442,  1.4321,  2.2574,  1.0566,  1.2195,&
         &0.,      0.,      2.9716,  3.2426,  3.388,& !30
         &18.6647,  1.6447,  2.5493,  1.1281,  1.5392 /

    data B1 /&
         &1.0989,  0.2965,  1.5444,  0.,      1.0302,&
         &0.6541,  0.0648,  0.1237,  0.,      0.,& !10
         &0.1872,  0.0138,  0.2704,  0.6565,  0.6342,&
         &0.4857,  0.0787,  0.0892,  0.196,   0.3696,& !20
         &0.3003,  0.,      0.3295,  0.286,   0.2808,&
         &0.5888,  0.6024,  0.8478,  1.1018,  1.1824,& !30
         &7.7658,  0.0835,  0.6732,  0.,      0.038 /

    data B2 /&
         &0.0396,  0.0045,  0.0766,  0.0282,  0.074,&
         &0.1046,  0.0158,  0.0525,  0.0147,  0.0803,&
         &0.00433, 0.000774,0.,      0.00127, 0.026,&
         &0.0215,  0.,      0.,      0.0189,  0.0322,&
         &0.,      0.00166, 0.,      0.0168,  0.0148,&
         &0.0692,  0.0644,  0.0234,  0.0323,  0.0372,&
         &0.297,   0.00941, 0.0186,  0.00176, 0. /

    data BBAL /&
         &0.0477,  0.0305,  0.0205,  0.0266,  0.0426,&
         &0.0552,  0.0356,  0.0661,  0.0217,  0.0468,& !10
         &0.0225,  0.000676,0.,      0.0111,  0.0155,&
         &0.0185,  0.00224, 0.00211, 0.0302,  0.0325,& !20
         &0.0209,  0.0486,  0.015,   0.00708, 0.00837,&
         &0.0295,  0.07,    0.0131,  0.00381, 0.00655,& !30
         &0.0545,  0.0327,  0.0045,  0.0151,  0.0267 /

    data BHT /&
         &0.,      0.,      0.,      0.,      0.,&
         &0.,      0.,      0.,      0.,      0.,& !10
         &0.,      0.,      0.,      0.,      0.,&
         &0.,      0.,      0.,      0.0132,  0.,& !20
         &0.,      0.,      0.,      0.,      0.,&
         &0.,      0.,      0.,      0.,      0.,& !30
         &0.,      0.,      0.,      0.,      0. /

    data BSI /&
         &-0.0117,  0.,     -0.0351, -0.0836, -0.0624,&
         &-0.0379, -0.018,  -0.0596, -0.093,  -0.0665,& !10
         &0.,      0.00166, 0.,      0.,      0.,&
         &0.,      0.,      0.,      0.,      0.,& !20
         &0.0101, -0.025,   0.,      0.,      0.,&
         &0.,      0.,      0.,      0.,      0.,& !30
         &0.,      0.,      0.,      0.,     -0.0211 /

    data BBA /&
         &0.,      0.0041,  0.,      0.0257,  0.,&
         &0.,      0.,      0.,      0.,      0.,& !10
         &0.0226,  0.,      0.00875, 0.0244,  0.00705,&
         &0.00292, 0.,      0.,      0.,      0.,& !20
         &0.,      0.,      0.,      0.0181,  0.0145,&
         &0.1151,  0.1273,  0.,      0.0215,  0.0205,& !30
         &0.,      0.,      0.022,   0.0111,  0.00405 /

    data BDBHQ /&
         &-0.015,  -0.0089, -0.0343,  0.,       0.,&
         &0.,      0.,      0.,      0.,       0.,& !10
         &-0.0201,  0.,      0.,      0.,       0.,&
         &0.,      0.,      0.,      0.000716,-0.012,& !20
         &0.,      0.,      0.,      0.,       0.,&
         &0.,      0.,      0.,      0.,       0.,& !30
         &0.,      0.,      0.,      0.,       0. /

    data BAGS /&
         &0.,      0.,      0.,      0.,      0.,&
         &0.,      0.,      0.,      0.,      0.,& !10
         &0.,      0.,      0.,      0.,      0.,&
         &-0.2502,  0.,     -0.0622,  0.,     -0.1575,& !20
         &0.,      0.,      0.0612,  0.,     -0.199,&
         &0.,     -1.6033,  0.,      0.,     -0.158,& !30
         &0.,      0.,      0.,     -0.5982,  0. /

    data B95 /&
         &0.56,    0.51667, 0.52,    0.46228, 0.24,&
         &0.48,    0.39,    0.557,   0.22,    0.517,& !10
         &0.56,    0.422,   0.32,    0.2,     0.55,&
         &0.55,    0.52,    0.52,    0.514,   0.514,& !20
         &0.241,   0.46,    0.46,    0.575,   0.575,&
         &0.48,    0.48,    0.614,   0.57,    0.57,& !30
         &0.3,     0.28,    0.46,    0.46,    0.38 /

    !  THE NUMBER OF OBSERVATIONS BY EQUATION COEFFICIENT.
    !  THERE ARE SOME CASES WHERE THERE IS A TREE-QUALITY TERM
    !  FOR A SPECIES; AND BECAUSE OF THEY THE FITS WERE DONE,
    !  THERE ARE 2 EQUATIONS. IN THESE CASES THE FIRST (NO
    !  QUALITY) SAMPLE SIZE IS USED.

    data  OBSERV/&
         &13692, 5314, 4954,92648,31572,&
         &4879, 1883, 4591,56172, 9139,& !10
         &830, 6281,  601,  613,26739,&
         &26739,  741,  741,  508, 1453,& !20
         &4394,  595,  740, 3971, 3971,&
         &269,  269,  284,  934,  934,& !30
         &115,  259,  274,  274,11183/

    ! Check whether debugging is enabled
    if (LCONS) go to 1000

    call DBCHK (DEBUG,'DGF',3,ICYC)
    if (DEBUG) write(JOSTND,3)ICYC
    3 format(' ENTERING SUBROUTINE DGF  CYCLE =',I5)

    !     STORE DIAMETERS FOR ITERATIVE PROCESSING

    do I = 1,MAXTRE
       TEMD(I) = DIAM(I)
       4 continue
    end do

    !     METRIC STAND-LEVEL PARAMETERS
    BAM = BA    * FT2pACRtoM2pHA
    QMDM = RMSQD * INtoCM
    AGSBAM = 0.

    !     NUMBER OF LOOPS TO ITERATE THE ANNUAL DG GROWTH MODEL
    !     FOR THE TIMESTEP SIZE
    !     J = 10  ! ASSUME A 10 YEAR CYCLE AND ADJUST LATER

    !     CALCULATE BAMSP (M**2/HA) FOR EACH SPECIES. THOSE
    !     WITH IMC=1 ARE PUT IN AN "AGS" SUBGROUP (JI=1); OTHERWISE
    !     INTO A "UGS" SUBGROUP (JI=2). THOSE ASSIGNMENTS ARE ONLY
    !     MEANINGFUL FOR A SUBSET OF COMMERCIAL SPECIES
    do ISPC = 1,MAXSP
       BAMSP(ISPC,1) = 0.
       BAMSP(ISPC,2) = 0.
       I1 = ISCT(ISPC,1)
       if (I1.ne.0) then
          I2 = ISCT(ISPC,2)
          do I3 = I1,I2
             I = IND1(I3)
             DBHM = DIAM(I) * INtoCM
             if (IMC(I) .eq.1) then
                JI = 1
             else
                JI = 2
             end if
             BAMSP(ISPC,JI) = BAMSP(ISPC,JI) +&
                  &((3.141593*(DBHM*0.5)**2)*PROB(I))
          end do
          BAMSP(ISPC,1) = (BAMSP(ISPC,1)/10000.)/ACRtoHA
          BAMSP(ISPC,2) = (BAMSP(ISPC,2)/10000.)/ACRtoHA
       end if
    end do

    !     BA IN AGS HARDWOODS USED BY PENNER (MAPLES, BEECH, IRONWOOD, BASSWOOD)
    !     THIS MIGHT ACTUALLY NEED TO BE ALL AGS SPECIES, NOT JUST PENNER 2020

    AGSBAM = BAMSP(18,1) + BAMSP(19,1) + BAMSP(25,1) + BAMSP(26,1) +&
         &BAMSP(27,1) + BAMSP(28,1) + BAMSP(47,1)

    !     SET FLAG IF BEECH (28) OR MAPLE (18,19,26,27) ARE LEADING BY BA
    LMBLEAD = .false.
    X = 0.
    I = -1
    do ISPC = 1,MAXSP
       Y = BAMSP(ISPC,1) + BAMSP(ISPC,2)
       if (Y .gt. X) then
          X = Y
          I = ISPC
       end if
    end do
    if ((I.eq.18) .or. (I.eq.19) .or. (I.eq.26)&
         &.or. (I.eq.27) .or. (I.eq.28)) then
       LMBLEAD = .true.
    end if

    !     BEGIN SPECIES LOOP.  ASSIGN VARIABLES WHICH ARE SPECIES DEPENDENT

    do ISPC = 1,MAXSP
       I1 = ISCT(ISPC,1)
       if (I1.eq.0) go to 20
       I2 = ISCT(ISPC,2)
       SI = SITEAR(ISPC)
       SIM = SI*FTtoM

       !     DETERMINE IF WE USE THE NEW ONTARIO EQUATIONS.
       !     NOTE, WHEN WE GET THE LAKE STATES PARAMETERS, WE WILL MODIFY
       !     THESE LINES TO DETERMINE WHICH SET OF EQUATIONS GETS USED.

       if (OSPMAP(ISPC) .eq.0) goto 20

       !       BEGIN TREE LOOP WITHIN SPECIES ISPC.

       do I3 = I1,I2
          I = IND1(I3)

          WK2(I) = 0.0
          if (DIAM(I) .le.0.0) go to 10

          !     IF THIS IS AN ISPC SPECIES FOR WHICH A QUALITY EQUATION
          !     EXISTS AND THE INDIVIDUAL IS A QUALITY TREE (IMC=1), SELECT
          !     COEFFICIENTS OFFSET BY +1; SIGN OF AGS COEFFS IS <0 BECAUSE
          !     THE EQUATION IS CODED WITH SUBTRACTION, SO <0 GIVES A POSITIVE
          !     IMPROVEMENT TO ANNUAL GROWTH RTE

          if (LQUAL(ISPC).and.IMC(I).eq.1) then
             KSP = OSPMAP(ISPC)+1
             AGS = 1
          else
             KSP = OSPMAP(ISPC)
             AGS = 0
          end if

          !     ITERATE 10 TIMES AT 1000, SINCE DG EQUATION IS AN ANNUAL BASIS
          !     NOTE THAT THERE IS NO UPDATING OF BAM, BALM, QMDM, OR HTM
          !     SO THE ITERATION IS PEDANTIC... JUST TO BE THE SAME ACROSS
          !     ALL FVS VARIANTS.

          BALM = (1.0 - (PCT(I)/100.)) * BAM
          HTM = HT(I) * FTtoM
          DBHM = DIAM(I) * INtoCM

          !     THE ENTIRE DG EQUATION IS BROKEN INTO 2 PIECES; FIRST
          !     PIECE IS CONSTANT OVER THE ITERATIONS; 2ND PIECE
          !     INVOLVES DBH AND IS UPDATED

          X1 = - (B0(KSP))&
               !    >         + (B1(KSP)    * LOG(DBHM))
               !    >         - (B2(KSP)    * DBHM)
               &- (BBAL(KSP)  * BALM)&
               &- (BHT(KSP)   * HTM)&
               &- (BSI(KSP)   * SIM)&
               &- (BBA(KSP)   * BAM)&
               &- (BDBHQ(KSP) * QMDM)&
               &- (BAGS(KSP)  * real(AGS))

          !         LN CM/YR DIAMETER GROWTH STANDARD 10 YEAR PROJECTION
          do ILOOP = 1,10
             DGLN = X1&
                  &+ (B1(KSP) * LOG(DBHM))&
                  &- (B2(KSP) * DBHM)

             !           JUST TO BE SAFE, CONSTRAIN PREDICTED DG TO +/- 5 BEFORE
             !           EXPONENTIATION; THEN CONSTRAIN THE ANNUAL GROWTH TO
             !           0.001 - 95TH% FROM B95()
             DELD = MIN(MAX(DGLN,-5.0),5.0)
             DELD = EXP(DELD)
             DELD = MIN(MAX(DELD,0.0001),B95(KSP))
             DBHM = DBHM + DELD
          end do

          D = DBHM * CMtoIN
          BARK = BRATIO(ISPC,D,HT(I))
          DIAGR = (D-DIAM(I))*BARK
          TEMD(I) = DIAGR

          !     FINAL ASSIGNMENT OF DDS AND WK2() IS NOW DONE AT THE END, TO ALLOW
          !     FOR MODIFICATION OF DIAMETER GROWTH PREDCITION (DIAGR) FOR THE HYBRID
          !     HARDWOOD GROWTH MODEL.

          !          IF(DEBUG)
          !     &      WRITE(JOSTND,*)' I,DIAM,DIAGR,BARK= ',
          !     &      I,DIAM(I),DIAGR,BARK

          !          IF(LDCOR2 .AND. COR2(ISPC) .GT. 0.0)DIAGR= DIAGR*COR2(ISPC)
          !          IF(DIAGR.LE. .0001) DIAGR=.0001
          !          TEMD(I) = DIAGR

          !          IF(DEBUG)WRITE(JOSTND,*)' I,ISPC,COR2,DIAGR,LDCOR2= ',
          !     &      I,ISPC,COR2(ISPC),DIAGR,LDCOR2

          !          DDS=DIAGR*(2.0*DIAM(I)*BARK+DIAGR)
          !          IF(DEBUG)WRITE(JOSTND,*)' I,DIAGR,DIAM(I),BARK,DDS= ',
          !     &      I,DIAGR,DIAM(I),BARK,DDS

          !          IF (DDS .GT. 0.0) WK2(I)=ALOG(DDS)+COR(ISPC)

          10 continue
       end do
       20 continue ! END OF SPECIES LOOP.
    end do

    !     MODIFY THE DIAMETER GROWTH PREDICTION USING PENNER-ADJUSTMENT IF THERE IS
    !     (A) LIVE BA, (B) AT LEAST 50% OF BA IN SHADE-TOLERANT SPECIES AND
    !     (C) MAPLE OR BEECH ARE THE LEADING SPECIES
    LHYBRID = .false.
    if ((BAM .gt.0.) .and. (AGSBAM/BAM .gt.0.50) .and. LMBLEAD) then
       LHYBRID = .true.
    end if

    !CCCCCCCCCCCCCCCCCCCCCCCCCCCC

    !     Temporarily disable the hybrid model for BlueSource 09-Mar-2022

    LHYBRID = .false.

    !CCCCCCCCCCCCCCCCCCCCCCCCCCCC

    if (LHYBRID) then

       !       STAND-LEVEL ADJUSTMENT OF GROWTH FOR HARDWOODS BASED ON PENNER.
       !       (2020) "HARDWOOD SELECTION GROWTH RATES".
       !       BEECH, SUGAR MAPLE, RED MAPLE, SILVER MAPLE, IRONWOOD, BLACK MAPLE
       !       THE CALCULATIONS USE ENTIRE STAND BA, DQ, ETC., AND
       !       NOT JUST THE HARDWOOD TREES THAT ARE THE TARGET FOR THE ADJUSTMENT

       !       TIMBER CAPABILITY RATING (TCR) FROM PENNER (2020) TABLE 5.
       !       DO-LOOP BELOW USES TVOL ACCRETION IN THE PREVIOUS TIMESTEP
       !       (BEST WE CAN DO) IN IMPERIAL UNITS NATIVE TO IOSUM().

       !       TCR   FT3/AC/YR   M3/HA/YR
       !         0          NA         NA
       !         1        >110       >7.7
       !         2      91-110    6.3-7.7
       !         3      71-90     4.9-6.3
       !         4      51-70     3.5-4.9
       !         5      31-50     2.1-3.5
       !         6      11-30     0.7-2.1
       !         7        <11        <0.7

       !       THE 15TH ENTRY IN IOSUM() IS TOTAL VOLUME ACCRETION FT3/AC/YR
       !       IN A CYCLE. THIS SHOULD FIND ACCRETION FOR THE PREVIOUS CYCLE
       !       DEFAULT IN THE FIRST CYCLE IS ITCR2=5

       ITCR = 0
       if (ICYC .gt.1) then
          ITCR = IOSUM(15,ICYC-1)
       end if

       ITCR2 = 5
       if (ITCR .eq.0) then
          ITCR2 = 5
       else if (ITCR .gt.110) then
          ITCR2 = 1
       else if (ITCR .gt.91.and. ITCR .le.110) then
          ITCR2 = 2
       else if (ITCR .gt.71.and. ITCR .le.91) then
          ITCR2 = 3
       else if (ITCR .gt.51.and. ITCR .le.71) then
          ITCR2 = 4
       else if (ITCR .gt.31.and. ITCR .le.51) then
          ITCR2 = 5
       else if (ITCR .gt.11.and. ITCR .le.31) then
          ITCR2 = 6
       else
          ITCR2 = 7
       end if

       !       THE 7TH ENTRY IN IOSUM() IS TOTAL TPA REMOVED IN THE CYCLE
       !       FIRST CHECK REMOVALS IN THE CURRENT CYCLE, THEN CHECK IF
       !       ANYTHING HAS BEEN REMOVED IN THE LAST 5 YEARS, SET ICUT=1
       ICUT = 0
       if (ICYC .eq.1) then
          if (ONTREM(7) .gt.0.0) then
             ICUT = 1
          end if
       else
          do I = ICYC-1, 1, -1
             if (IY(ICYC)-IY(I) .le.5) then
                if (IOSUM(7,I) .gt.0) then
                   ICUT = 1
                   exit
                end if
             else
                exit
             end if
          end do
       end if

       X = 0.07292&
            &- (0.00178 * BAM)&
            &+ (0.01464 * AGSBAM/BAM)&
            &+ (0.00217 * real(ITCR2))&
            &- (0.00093 * QMDM)&
            &+ (0.00404 * -1.0)& ! SET Q = -1.0
            &- (0.00974 * real(ICUT))

       X = X / BAM

       !       BAM3 IS THE PREDICTED BA CHANGE FOR THE CYCLE BASED ON PENNER STAND
       !       MODEL, CONVERTED TO FT2/AC USED INTERNALLY. MULTIPLY BY 10 FOR 10-YEAR
       !       STANDARD PROJECTION ADJUSTED LATER FOR ACTUAL CYCLE
       BAM3 = X * BAM * 10. * M2pHAtoFT2pACR

       !       BAM2 IS THE PREDICTED BA CHANGE FOR THE CYCLE BASED ON THE ORIGINAL
       !       MODEL, in FT2/AC
       BAM2 = 0.0
       do ISPC = 1,MAXSP
          I1 = ISCT(ISPC,1)
          if (I1.eq.0) go to 201
          I2 = ISCT(ISPC,2)
          do I3 = I1,I2
             I = IND1(I3)
             A2 = ((DIAM(I)+TEMD(I))/24.0)**2
             A1 = (DIAM(I)/24.0)**2
             BAM2 = BAM2 + PROB(I) * 3.141596 * (A2-A1)
          202 end do
       201 end do

       !       ADJUSTMENT TO DIAMETER CHANGE BASED ON ADJUSTMENT OF THE BASAL
       !       AREA GROWTH "RING"

       !       BB * PI * (R**2 + 2*R*X2 + X2**2 - R**2) = PI * (R**2 + 2*R*X1 + X1**2 - R**2)
       !       0 = X1**2 + 2*R*X1 + (-BB * (2*R*X2 + X2**2))

       !       WHERE:
       !       BB = BAM3/BAM2 (I.E. PENNER/FVS) RATIO OF THE BASAL AREA CHANGE FROM THE TWO MODELS
       !       R  = CURRENT RADIUS BASED ON DIAMETER DIAM(I)/2
       !       X2 = RADIUS CHANGE PREDICTED BY DIAMETER CHANGE BASED ON TEMDG(I)/2 IN ORIGINAL MODEL
       !       X1 = RADIUS CHANGE FOLLOWING ADJUSTMENT CALCULATION (POSITIVE ROOT OF QUADRATIC)

       A = 1.0
       BB = BAM3/BAM2
       do ISPC = 1,MAXSP
          I1 = ISCT(ISPC,1)
          if (I1.eq.0) go to 203
          I2 = ISCT(ISPC,2)
          do I3 = I1,I2
             I = IND1(I3)
             B = DIAM(I)
             R = B * 0.5
             X2 = TEMD(I) * 0.5
             C = -BB * ((2.0 * R * X2) + (X2 * X2))
             X1 = (-B + SQRT(B * B - (4.0 * A * C))) * 0.5
             TEMD(I) = X1 * 2.0
          204 end do
       203 end do

       if (DEBUG) then
          write(JOSTND,*)' BAM,AGSBAM,ITCR2,QMDM,ICUT=',&
               &BAM,AGSBAM,ITCR2,QMDM,ICUT
          write(JOSTND,*)' BB,BAM3,BAM2,A,B,C=',&
               &BB,BAM3,BAM2,A,B,C
       end if

    end if ! END OF LHYBRID IF/ENDIF

    do ISPC = 1,MAXSP
       I1 = ISCT(ISPC,1)
       if (I1.eq.0) go to 303
       I2 = ISCT(ISPC,2)
       do I3 = I1,I2
          I = IND1(I3)
          DIAGR = TEMD(I)
          if (DEBUG)&
               &write(JOSTND,*)' I,DIAM,DIAGR,BARK= ',&
               &I,DIAM(I),DIAGR,BARK

          if (LDCOR2.and. COR2(ISPC) .gt.0.0)DIAGR = DIAGR*COR2(ISPC)
          if (DIAGR.le..0001) DIAGR = .0001

          if (DEBUG)write(JOSTND,*)' I,ISPC,COR2,DIAGR,LDCOR2= ',&
               &I,ISPC,COR2(ISPC),DIAGR,LDCOR2

          DDS = DIAGR*(2.0*DIAM(I)*BARK+DIAGR)
          if (DEBUG)write(JOSTND,*)' I,DIAGR,DIAM(I),BARK,DDS= ',&
               &I,DIAGR,DIAM(I),BARK,DDS

          if (DDS .gt.0.0) WK2(I) = ALOG(DDS)+COR(ISPC)
       304 end do
    303 end do

    if (DEBUG)write(JOSTND,100)ICYC
    100 format(' LEAVING SUBROUTINE DGF  CYCLE =',I5)
    return

    !     ENTRY DGCONS

    1000 continue
    do ISPC = 1,MAXSP
       if (OSPMAP(ISPC) .gt.0) then
          J = OBSERV(OSPMAP(ISPC))
       else
          J = 1
       end if
       DGCON(ISPC) = 0.
       ATTEN(ISPC) = real(J)
       SMCON(ISPC) = 0.
       120 continue
    end do
    return
  end subroutine DGFON

  subroutine DGFCR(LCONS)
    implicit none

    !  CR: **GEMDG** HOLDS THE EQUATIONS AND COEFFICIENTS FOR EVERY SPECIES
    !  AND MODEL TYPE (IMODTY); THIS PART PREPARES THE STAND AND POINT
    !  VARIABLES, CALLS IT FOR EACH TREE AND APPLIES COR AND DGCON.

    logical :: LCONS
    ! Variables
    logical :: DEBUG
    integer :: I,ISPC,I1,I2,I3,IPCCF,ICLS
    real    :: SSITE
    real    :: D,BARK,BRATIO,CR,BAUTBA,SPBA,PBAL,BAL,DDS

    if (LCONS) go to 1000

    !  CHECK FOR DEBUG.

    call DBCHK (DEBUG,'DGF',3,ICYC)
    if (DEBUG) write(JOSTND,*)' ENTERING DGF CYCLE=',ICYC

    !  AGE RANGE AND STAGNATION MULTIPLIER (SHARED WITH TT AND UT).

    call DGFSTG(DEBUG)

    !  BEGIN SPECIES LOOP.  ASSIGN VARIABLES WHICH ARE SPECIES DEPENDENT

    do ISPC = 1,MAXSP
       SSITE = SITEAR(ISPC)
       I1 = ISCT(ISPC,1)
       if (I1.eq.0) go to 20
       I2 = ISCT(ISPC,2)

       !   PROCESS TREES WITHIN SPECIES.

       do I3 = I1,I2
          I = IND1(I3)
          D = DIAM(I)
          ICLS = IFIX(D + 1.0)
          if (ICLS .gt.41) ICLS = 41
          BARK = BRATIO(ISPC,D,HT(I))
          WK2(I) = 0.0
          if (D .le.0.0) go to 10
          CR = ICR(I)*0.01
          BAUTBA = BAU(ICLS) / BA
          SPBA = TBA(ISPC)
          IPCCF = ITRE(I)
          PBAL = (1.0-(PCT(I)/100.))*PTBAA(IPCCF)
          BAL = (1.0-(PCT(I)/100.))*BA

          if (DEBUG)write(JOSTND,*)' BEFORE CALL TO GEMDG',I,IMODTY,ISPC,&
               &BAUTBA,SPBA,SSITE,D,BA,BARK,CR,SLOPE,ASPECT,ELEV,PBAL,&
               &PCCF(IPCCF),RELDEN,BAL

          call GEMDG(DDS,IMODTY,ISPC,BAUTBA,SPBA,SSITE,D,BA,BARK,&
               &CR,SLOPE,ASPECT,ELEV,PBAL,PCCF(IPCCF),RELDEN,BAL)

          WK2(I) = DDS + COR(ISPC) + DGCON(ISPC)

          !  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.

          if (.not.DEBUG) go to 10
          write(JOSTND,*)' AFTER CALL TO GEMDG',I,ISPC,DDS,COR(ISPC),&
               &DGCON(ISPC),WK2(I)
          10 continue
       end do

       !  END OF SPECIES LOOP.

       20 continue
    end do
    return
    1000 continue

    !  ENTRY POINT FOR LOADING COEFFICIENTS OF THE DIAMETER INCREMENT
    !  MODEL THAT ARE SITE SPECIFIC AND NEED ONLY BE RESOLVED ONCE.
    !  ASPECT IS STAND ASPECT.  OBSERV CONTAINS THE NUMBER OF
    !  OBSERVATIONS BY HABITAT CLASS BY SPECIES FOR THE UNDERLYING
    !  MODEL (THIS DATA IS ACTUALLY USED BY **DGDRIV** FOR CALIBRATION).

    !  OBSERV IS THE NUMBER OF OBSERVATIONS IN THE DIAMETER GROWTH
    !  MODEL BY SPECIES AND SITE CLASS.

    do ISPC = 1,MAXSP
       DGCON(ISPC) = 0.
       SMCON(ISPC) = 0.

       !  IF READCORD OR REUSCORD WAS SPECIFIED (LDCOR2 IS TRUE) ADD
       !  LN(COR2) TO THE BAI MODEL CONSTANT TERM (DGCON).  COR2 IS
       !  INITIALIZED TO 1.0 IN BLKDATA.

       if (LDCOR2.and.COR2(ISPC).gt.0.0) DGCON(ISPC) = DGCON(ISPC)&
            &+ ALOG(COR2(ISPC))
       ATTEN(ISPC) = 1000.
       30 continue
    end do
    ATTEN(4) = 21277.
    ATTEN(6) = 880.
    ATTEN(7) = 1176.
    ATTEN(8) = 900.
    ATTEN(10) = 123.
    ATTEN(14) = 306.
    return
  end subroutine DGFCR

  subroutine DGFBC(LCONS)
    implicit none

    !  BC: COMPUTES THE VALUE OF DDS (CHANGE IN DIAMETER) [NOT SQUARED
    !  DIAM, AS IN NI, ETC] FROM HABITAT TYPE, LOCATION, SLOPE, ASPECT,
    !  ELEVATION, DBH, CROWN RATIO, BASAL AREA IN LARGER TREES AND CCF.
    !  LV2ATV SELECTS THE LEGACY (LV2) TABLES WITH **SEILTDG** ADJUSTMENTS;
    !  OTHERWISE THE V3 ZONE/SITE-SERIES MODEL IS USED.

    logical :: LCONS
    !  DIMENSIONS FOR INTERNAL VARIABLES.

    !     DIAM -- ARRAY LOADED WITH TREE DIAMETERS (PASSED AS AN
    !             ARGUEMENT).
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
    !             LARGER TREES TERM IN THE DDS MODEL
    !             (ONE COEFFICIENT FOR EACH SPECIES).
    !   DGDBAL -- ARRAY CONTAINING COEFFICIENTS FOR THE INTERACTION
    !             BETWEEN BASAL AREA IN LARGER TREES AND LN(DBH) (ONE
    !             COEFFICIENT PER SPECIES).
    !    DGCCF -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
    !             COMPETITION FACTOR TERM IN THE DDS MODEL (ONE
    !             COEFFICIENT FOR EACH SPECIES, LOADED IN RCON).

    !     P_ZL  - LENGTH OF ZONE AND SITE SERIES STRINGS
    !     P_ZN  - NUMBER OF ZONE MODELS
    !     P_ST  - NUMBER OF SITE SERIES STRINGS PER ZONE OR SIBEC GROUP
    !     P_SS  - NUMBER OF SIBEC GROUPS

    ! Variables
    integer :: P_ZL,P_ZN
    integer :: P_ST,P_SS
    parameter (P_ZL = 15)
    parameter (P_ZN = 28)
    parameter (P_ST = 30)
    parameter (P_SS = 25)

    !     STRUCTURE FOR THE FITTED MODELS. COEFFICIENTS FOR EACH FITTED MODEL
    !     ARE FOUND HERE. ONLY ONE OF DBAL1 OR DBAL2 TERMS CAN HAVE A NON-ZERO
    !     VALUE. THE BIOLOGICAL SCOPE OF THE MD_STR OBJECT IS SOMETIMES THE BEC
    !     ZONE, SOMETIMES ALL BEC ZONES COMBINED, AND SOMETIMES THE SITE SERIES.

    TYPE MD_STR
       integer              :: SPP(15) ! species list
       character (LEN = P_ZL) :: ZONE(P_ST) ! BEC list
       integer              :: OBSERV ! observations
       real                 :: CON ! constant
       real                 :: CASP ! cos-aspect
       real                 :: SASP ! sin-aspect
       real                 :: EL ! elevation
       real                 :: EL2 ! elevation^2
       real                 :: CCFA ! ccf
       real                 :: LD ! diam
       real                 :: DSQ ! diam^2
       real                 :: DBAL1 ! bal/dbh
       real                 :: DBAL2 ! bal/log(dbh+1)
       real                 :: CR ! crown
       real                 :: BAL ! bal
       real                 :: SIGMAR ! residual error
    end TYPE

    !     THE SURROGATE SPECIES (SP2) IS USED WHEN ONE SPECIES IS A 2ND SPECIES
    !     AS A SURROGATE. THIS OCCURS WHEN THERE IS NO AVAILABLE ESTIMATE FOR
    !     THE 1ST SPP

    TYPE SS_STR
       integer              :: SPP(15) ! species list
       character (LEN = P_ZL) :: PrettyName(P_ST) ! BEC/SS list
       real                 :: CON ! ss coefficient
    end TYPE

    character(len=15) :: PAT1
    character(len=15) :: PAT2
    integer :: I,J,K,M,I1,I2,I3,IP,JP,ISPC,IPAS,iSeries
    integer :: IPOS(15),JPOS(15)
    real    :: D,CR,BAL,BALD1,BALD2,DDS,CONSPP,PELEV,D2
    real    :: BRATIO

    logical :: DEBUG
    logical :: LSPPOK(15)
    real    :: BECADJ(3),SEICN2(15),SEISLP(15),SEISAS(15),SEICAS(15),SEIELV(15),&
         &SEIEL2(15),SEICCF(15),DGCON1(15),DGCON2(15)
    !     v2
    real    :: BA100,DGLDS,DGBALS,DGCRS,DGCRS2,DGDSQS,DGDBLS,RELDN2,ALD
    real    :: DGLD(15),DGBAL(15),DGCR(15),DGCRSQ(15),DGDBAL(15),DGHAB(6,15),DGFOR(6,&
         &15),DGDS(4,15),DGEL(15),DGEL2(15),DGSASP(15),DGCASP(15),DGSLOP(15),&
         &DGSLSQ(15),DGCCFA(5,15),OBSERV(6,15)
    integer :: MAPHAB(30,15),MAPDSQ(11,15),MAPLOC(11,15),MAPCCF(30,15)

    data DGLD/&
         &0.56445, 0.54140, 0.56888, 0.68810, 0.68712, 0.58705,&
         &0.89503, 0.73045, 0.86240, 0.66101,&
         &0.89778, 0.89778, 0.89778, 0.56888, 0.89778/
    data DGCR/&
         &1.08338, 1.03478, 2.06850, 1.93969, 1.64133, 1.29360,&
         &1.85558, 1.54643, 0.52044, 1.31618,&
         &1.28403, 1.28403, 1.28403, 2.06850, 1.28403/
    data DGCRSQ/&
         &0.0,     0.07509,-0.62361, -0.78258,-0.27244,     0.0,&
         &-0.36393,-0.26635, 0.86236,  0.0,&
         &0.0,     0.0,     0.0,     -0.62361, 0.0/
    data DGBAL/&
         &0.42112, 0.43637, 0.50202, 0.45142,     0.0, 0.74596,&
         &-0.03665, 0.25639, 0.0,     0.0,&
         &0.0,     0.0,     0.0,     0.50202,     0.0/
    data DGDBAL/&
         &-2.08272,-2.03256,-2.11590,-1.76812,-0.80918,-2.28375,&
         &-0.43329,-1.18218,-0.51270,-1.25881,&
         &-0.66110,-0.66110,-0.66110,-2.11590,-0.66110/
    data  OBSERV/&
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
         &55.0,  880.0,    0.0,    0.0,    0.0,    0.0,&
         &55.0,  880.0,    0.0,    0.0,    0.0,    0.0,&
         &1079.0, 1682.0, 4014.0,   63.0, 4684.0,    0.0,&
         &55.0,  880.0,    0.0,    0.0,    0.0,    0.0/

    !  DGHAB IS AN ARRAY THAT CONTAINS HABITAT CLASS INTERCEPTS FOR
    !  EACH SPECIES.  MAPHAB IS INDEXED BY ITYPE TO MAP HABITAT
    !  TYPES ONTO HABITAT CLASSES.

    data MAPHAB/&
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
         &21*2, 1, 8*2,&
         &21*2, 1, 8*2,&
         &7*3, 1, 3*3, 8*1, 3,2, 6*3, 2,3,3,&
         &21*2, 1, 8*2/

    data DGHAB/&
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
         &-1.68033,-1.52111,     0.0,     0.0,     0.0,     0.0,&
         &-1.68033,-1.52111,     0.0,     0.0,     0.0,     0.0,&
         &0.47780, 0.15228, 0.29764,     0.0,     0.0,     0.0,&
         &-1.68033,-1.52111,     0.0,     0.0,     0.0,     0.0/

    !  DGCCFA CONTAINS COEFFICIENTS FOR THE CCF TERM BY SPECIES BY
    !  HABITAT CLASS.  MAPCCF IS INDEXED BY ITYPE TO MAP HABITAT TYPES
    !  ONTO HABITAT CLASSES.

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
         &30*1,&
         &30*1,&
         &6*4, 1,2, 3*4, 2,1, 3*4, 3, 3*4, 1,3,2, 4*4, 2,4,4,&
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
         &-0.10744,     0.0,     0.0,     0.0,     0.0,&
         &-0.10744,     0.0,     0.0,     0.0,     0.0,&
         &-0.09046,-0.11884,-0.05529,-0.02180,     0.0,&
         &-0.10744,     0.0,     0.0,     0.0,     0.0/

    !  DGFOR CONTAINS LOCATION CLASS CONSTANTS FOR EACH SPECIES.
    !  MAPLOC IS AN ARRAY WHICH MAPS FOREST ONTO A LOCATION CLASS.

    data MAPLOC/&
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
         &3,2,1,1,3,3,3,3,1,3,2,&
         &3,2,1,1,3,3,3,3,1,3,2,&
         &5,4,1,2,3,3,2,3,5,1,4,&
         &3,2,1,1,3,3,3,3,1,3,2/

    data DGFOR/&
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
         &0.12520, 0.48076,     0.0,     0.0,     0.0,     0.0,&
         &0.12520, 0.48076,     0.0,     0.0,     0.0,     0.0,&
         &0.50357, 0.34920, 0.21961, 0.61812,     0.0,     0.0,&
         &0.12520, 0.48076,     0.0,     0.0,     0.0,     0.0/

    !  DGDS CONTAINS COEFFICIENTS FOR THE DIAMETER SQUARED TERMS
    !  IN THE DIAMETER INCREMENT MODELS; ARRAYED BY FOREST BY
    !  SPECIES.  MAPDSQ IS AN ARRAY WHICH MAPS FOREST ONTO A DBH**2
    !  COEFFICIENT.

    data MAPDSQ/&
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
         &1,2,1,1,1,1,1,1,2,1,2,&
         &1,2,1,1,1,1,1,1,2,1,2,&
         &1,4,2,2,2,3,1,4,1,1,4,&
         &1,2,1,1,1,1,1,1,2,1,2/

    data DGDS/&
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
         &-0.000484,-0.000306,      0.0,      0.0,&
         &-0.000484,-0.000306,      0.0,      0.0,&
         &-0.000252,-0.000373,-0.000502,-0.000572,&
         &-0.000484,-0.000306,      0.0,      0.0/

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
         &0.09817,-0.21337,-0.04562,-0.01215, 0.08277,-0.06625,&
         &0.00325,-0.13091,-0.12473,-0.09976,&
         &0.17935, 0.17935, 0.17935,-0.04562,0.17935/
    data DGSASP/&
         &0.03876, 0.03430, 0.06287,-0.04595, 0.10987, 0.05534,&
         &0.12993,-0.06038,-0.06862, 0.01192,&
         &0.13363, 0.13363, 0.13363, 0.06287, 0.13363/
    data DGSLOP/&
         &-0.17888, 0.33523, 0.78176, 1.17025, 0.04966, 0.11931,&
         &0.46546, 0.65622, 0.30070,-0.06637,&
         &0.07628, 0.07628, 0.07628, 0.78176, 0.07628/
    data DGSLSQ/&
         &0.0,    -0.70216,-1.12380,-1.52006, 0.0,     0.0,&
         &-0.58014,-0.90143,-0.62224,-0.43720,&
         &0.0,     0.0,     0.0,    -1.12380, 0.0/
    data DGEL/&
         &0.03517, 0.03730, 0.02591, 0.00917, 0.02863,-0.00175,&
         &-0.00480, 0.06259, 0.06313, 0.03229,&
         &0.08518, 0.08518, 0.08518, 0.02591, 0.08518/
    data DGEL2/&
         &-0.000467,-0.000433,-0.000377,-0.000117,-0.000422,-0.000067,&
         &-0.000058,-0.000709,-0.000676,-0.000422,&
         &-0.000943,-0.000943,-0.000943,-0.000377,-0.000943/

    TYPE (BEC_DESCRIPTION) BEC2
    TYPE (MD_STR) ZNKONST(P_ZN)
    TYPE (SS_STR) SSKONST(P_SS)

    data ZNKONST(1) /&
         &MD_STR (& ! PW - ICH+IDF
         &(/1, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
         &(/'ICH/all        ', 'IDF/all        ',&
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
         &'               ', '               ',&
         &'               ', '               '/),&
         &200,& ! observations
         &4.9231877,& ! constant
         &0.928152,& ! cos-asp
         &0.0,& ! sin-asp
         &-0.929742,& ! elev
         &0.0482885,& ! elev^2
         &0.0,& ! NO CCF
         &0.0297018,& ! diam
         &-0.00058105,& ! diam^2
         &-0.658208,& ! bal/dbh
         &0.0,& ! NO bal/log(dbh+1)
         &0.0,& ! NO CROWN
         &0.0,& ! NO BAL
         &0.89852& ! sigmar
         &) /

    data ZNKONST(2) /&
         &MD_STR (& ! LW - ICH+IDF
         &(/ 2,0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),&
         &(/'ICH/all        ', 'IDF/all        ',&
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
         &'               ', '               ',&
         &'               ', '               '/),&
         &739,& ! observations
         &-0.1021,& ! constant
         &0.0,& ! cos-asp
         &0.6201,& ! sin-asp
         &0.0,& ! elev
         &0.0,& ! elev^2
         &0.0,& ! ccf
         &0.0365,& ! diam
         &-0.00084,& ! diam^2
         &-0.8330,& ! bal/dbh
         &0.0,& ! NO bal/log(dbh+1)
         &1.3530,& ! crown
         &0.0,& ! bal
         &0.55480& ! sigmar
         &) /

    data ZNKONST(3) /&
         &MD_STR (& ! FD,OC - ICH 18+21m 1-30
         &(/ 3,14,0,0,0,0,0,0,0,0,0,0,0,0,0 /),& ! spp
         &(/'ICHdk/02       ', 'ICHdk/04       ',&
         &'ICHmk3/02      ', 'ICHmk3/03      ',&
         &'ICHmk3/06      ', 'ICHwk2/02      ',&
         &'ICHwk2/03      ', 'ICHwk2/04      ',&
         &'ICHwk4/02      ', 'ICHwk4/03      ',&
         &'ICHwk4/04      ', 'ICHwk4/05      ',&
         &'ICHmk1/01      ', 'ICHmk1/01-ys   ',&
         &'ICHmk1/02      ', 'ICHmk1/03      ',&
         &'ICHmk1/04      ', 'ICHmk1/05      ',&
         &'ICHmk2/01      ', 'ICHmk2/02      ',&
         &'ICHmk2/03      ', 'ICHmk2/04      ',&
         &'ICHmk2/05      ', 'ICHmw2/02      ',&
         &'ICHmw2/03      ', 'ICHmw3/01      ',&
         &'ICHmw3/01-y    ', 'ICHmw3/02      ',&
         &'ICHmw3/03      ', 'ICHmw3/04      '/),&
         &1683,& ! observations
         &-0.9496,& ! constant
         &0.0,& ! cos-asp
         &0.0,& ! sin-asp
         &0.0,& ! elev
         &0.0,& ! elev^2
         &0.0,& ! ccf
         &0.0674,& ! diam
         &-0.00095,& ! diam^2
         &0.0,& ! bal/dbh
         &0.0,& ! NO bal/log(dbh+1)
         &1.5869,& ! crown
         &-0.0206,& ! bal
         &0.77801& ! sigmar
         &) /

    data ZNKONST(4) /&
         &MD_STR (& ! FD,OC - ICH 18+21m 31-40
         &(/ 3,14,0,0,0,0,0,0,0,0,0,0,0,0,0 /),& ! spp
         &(/'ICHmw3/05      ', 'ICHwk1/03      ',&
         &'ICHdw/01a      ', 'ICHdw/02       ',&
         &'ICHmw1/02      ', 'ICHmw1/03      ',&
         &'ICHmw1/04      ', 'ICHmw2/03      ',&
         &'ICHmw2/04      ', 'ICHxw/01       ',&
         &'ICH/all        ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &1683,& ! observations
         &-0.9496,& ! constant
         &0.0,& ! cos-asp
         &0.0,& ! sin-asp
         &0.0,& ! elev
         &0.0,& ! elev^2
         &0.0,& ! ccf
         &0.0674,& ! diam
         &-0.00095,& ! diam^2
         &0.0,& ! bal/dbh
         &0.0,& ! NO bal/log(dbh+1)
         &1.5869,& ! crown
         &-0.0206,& ! bal
         &0.77801& ! sigmar
         &) /

    data ZNKONST(5) /&
         &MD_STR (& ! FD,OC - ICH 24m 1-29
         &(/ 3,14,0,0,0,0,0,0,0,0,0,0,0,0,0 /),& ! spp
         &(/'ICHdk/01       ', 'ICHdk/05       ',&
         &'ICHmk3/01      ', 'ICHmk3/04      ',&
         &'ICHwk2/01      ', 'ICHwk2/07      ',&
         &'ICHwk4/01      ', 'ICHwk4/07      ',&
         &'ICHmk1/06      ', 'ICHmw2/01      ',&
         &'ICHmw2/01-ys   ', 'ICHmw2/04      ',&
         &'ICHmw2/05      ', 'ICHmw3/06      ',&
         &'ICHmw3/07      ', 'ICHvk1/01      ',&
         &'ICHvk1/02      ', 'ICHvk1/03      ',&
         &'ICHvk1/04      ', 'ICHwk1/01      ',&
         &'ICHwk1/04      ', 'ICHwk1/05      ',&
         &'ICHdw/01b      ', 'ICHdw/03       ',&
         &'ICHdw/04       ', 'ICHmw1/01      ',&
         &'ICHmw1/05      ', 'ICHmw1/06      ',&
         &'ICHmw2/06      ', '               '/),&
         &1367,& ! observations
         &-1.4219,& ! constant
         &0.0,& ! cos-asp
         &0.0,& ! sin-asp
         &0.0,& ! elev
         &0.0,& ! elev^2
         &0.0,& ! ccf
         &0.0802,& ! diam
         &-0.00104,& ! diam^2
         &0.0,& ! bal/dbh
         &0.0,& ! NO bal/log(dbh+1)
         &2.1392,& ! crown
         &-0.0216,& ! bal
         &0.69721& ! sigmar
         &) /

    data ZNKONST(6) /&
         &MD_STR (& ! FD,OC - IDF 9m+12m SI 1-23 (+4)
         &(/ 3, 14,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'IDFdk3/02      ', 'IDFdk3/03      ',&
         &'IDFdk3/04      ', 'IDFdk4/01      ',&
         &'IDFdk4/02      ', 'IDFdk4/03      ',&
         &'IDFdk4/04      ', 'IDFdk4/05      ',&
         &'IDFdk4/07      ', 'IDFxm/02       ',&
         &'IDFxm/03       ', 'IDFxm/04       ',&
         &'IDFxw/02       ', 'IDFxw/03       ',&
         &'IDFxw/04       ', 'IDFdk1/02      ',&
         &'IDFdk2/02      ', 'IDFdm1/03      ',&
         &'IDFww/02       ', 'IDFxh1/02      ',&
         &'IDFxh1/03      ', 'IDFxh2/02      ',&
         &'IDFxh2/03      ',&
         &'IDFdk2/01      ', 'IDFdk2/03      ',& ! +2 from SI 15m
         &'IDFxh2/01      ', 'IDFxh2/04      ',& ! +2 from SI 15m
         &'IDF/all        ',&
         &'               ', '               '/),&
         &1217,& ! observations
         &0.5596,& ! constant
         &0.0,& ! cos-asp
         &0.0,& ! sin-asp
         &0.0,& ! elev
         &0.0,& ! elev^2
         &-0.198,& ! ccf
         &0.0,& ! diam
         &-0.00034,& ! diam^2
         &0.0,& ! NO bal/dbh
         &-0.1045,& ! bal/log(dbh+1)
         &1.3653,& ! crown
         &0.0,& ! bal
         &0.7339& ! sigmar
         &) /

    data ZNKONST(7) /&
         &MD_STR (& ! FD,OC - IDF 15m SI 1-28 (-4)
         &(/ 3, 14,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'IDFdk3/01      ', 'IDFdk3/05      ',&
         &'IDFdk4/09      ', 'IDFdw/01       ',&
         &'IDFxm/01       ', 'IDFxm/05       ',&
         &'IDFxm/06       ', 'IDFxm/07       ',&
         &'IDFxw/01       ', 'IDFxw/05       ',&
         &'IDFdk1/01      ', 'IDFdk1/03      ',&
         &'IDFdk1/04      ',& ! -2 moved to 12 (AAZ)
         &'IDFdk3/06      ', 'IDFdm1/04      ',&
         &'IDFmw1/02      ', 'IDFmw2/02      ',&
         &'IDFxh1/01      ', 'IDFxh1/04      ',&
         &'IDFxh1/05      ',& ! -2 moved to 12 (AAZ)
         &'IDFxh2/05      ', 'IDFxh2/06      ',&
         &'IDFdm2/01      ', 'IDFdm2/03      ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &1432,& ! observations
         &0.2361,& ! constant
         &0.0,& ! cos-asp
         &0.0,& ! sin-asp
         &0.0,& ! elev
         &0.0,& ! elev^2
         &-0.2418,& ! ccf
         &0.0168,& ! diam
         &-0.00033,& ! diam^2
         &0.0,& ! NO bal/dbh
         &0.0,& ! bal/log(dbh+1)
         &1.5811,& ! crown
         &-0.0191,& ! bal
         &0.88944& ! sigmar
         &) /

    data ZNKONST(8) /&
         &MD_STR (& ! FD,OC - IDF 18+21+24m SI 1-30
         &(/ 3, 14,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'IDFdk3/06      ', 'IDFdk3/07      ',&
         &'IDFdk3/08      ', 'IDFxm/08       ',&
         &'IDFxw/06       ', 'IDFxw/07       ',&
         &'IDFdk1/05      ', 'IDFdk1/06      ',&
         &'IDFdk2/04      ', 'IDFdk2/05      ',&
         &'IDFdk2/06      ', 'IDFdm1/01      ',&
         &'IDFdm1/05      ', 'IDFdm1/06      ',&
         &'IDFdm1/07      ', 'IDFmw1/01      ',&
         &'IDFmw1/01-yc   ', 'IDFmw1/03      ',&
         &'IDFmw1/04      ', 'IDFmw1/05      ',&
         &'IDFmw1/06      ', 'IDFmw2/01      ',&
         &'IDFmw2/03      ', 'IDFmw2/04      ',&
         &'IDFww/01       ', 'IDFww/03       ',&
         &'IDFww/04       ', 'IDFww/05       ',&
         &'IDFww/06       ', 'IDFxh1/06      '/),&
         &2767,& ! observations
         &-0.9981,& ! constant
         &0.0,& ! cos-asp
         &0.0,& ! sin-asp
         &0.0,& ! elev
         &0.0,& ! elev^2
         &-0.0817,& ! ccf
         &0.0435,& ! diam
         &-0.00053,& ! diam^2
         &0.0,& ! NO bal/dbh
         &0.0,& ! bal/log(dbh+1)
         &2.2233,& ! crown
         &-0.0206,& ! bal
         &0.74424& ! sigmar
         &) /

    data ZNKONST(9) /&
         &MD_STR (& ! FD,OC - IDF 18+21m+24m SI 31-37
         &(/ 3, 14,0,0,0,0,0,0,0,0,0,0,0,0,0/),& ! spp
         &(/'IDFxh1/07      ', 'IDFxh1/08      ',&
         &'IDFxh2/07      ', 'IDFxh2/08      ',&
         &'IDFdm2/04      ', 'IDFdm2/05      ',&
         &'IDFdm2/07      ', '               ',&
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
         &2767,& ! observations
         &-0.9981,& ! constant
         &0.0,& ! cos-asp
         &0.0,& ! sin-asp
         &0.0,& ! elev
         &0.0,& ! elev^2
         &-0.0817,& ! ccf
         &0.0435,& ! diam
         &-0.00053,& ! diam^2
         &0.0,& ! NO bal/dbh
         &0.0,& ! bal/log(dbh+1)
         &2.2233,& ! crown
         &-0.0206,& ! bal
         &0.74424& ! sigmar
         &) /

    data ZNKONST(10) /&
         &MD_STR (& ! HW - ICH+IDF
         &(/ 5, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'ICH/all        ', 'IDF/all        ',&
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
         &'               ', '               ',&
         &'               ', '               '/),&
         &959,& ! observations
         &-0.3410,& ! constant
         &0.0,& ! cos-asp
         &0.0,& ! sin-asp
         &0.0,& ! elev
         &0.0,& ! elev^2
         &-0.1307,& ! ccf
         &0.0605,& ! diam
         &-0.00087,& ! diam^2
         &0.0,& ! bal/dbh
         &0.0,& ! NO bal/log(dbh+1)
         &1.5816,& ! crown
         &-0.0119,& ! bal
         &0.88775& ! sigmar
         &) /

    data ZNKONST(11) /&
         &MD_STR (& ! CW - ICH
         &(/ 6, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),& ! spp
         &(/'ICH/all        ', '               ',&
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
         &'               ', '               ',&
         &'               ', '               '/),&
         &2756,& ! observations
         &-0.1650,& ! constant
         &0.2763,& ! cos-asp
         &0.0,& ! sin-asp
         &0.0,& ! elev
         &0.0,& ! elev^2
         &-0.1820,& ! ccf
         &0.0372,& ! diam
         &-0.00037,& ! diam^2
         &-0.0884,& ! bal/dbh
         &0.0,& ! NO bal/log(dbh+1)
         &1.4487,& ! crown
         &0.0,& ! bal
         &0.86261& ! sigmar
         &) /

    data ZNKONST(12) /&
         &MD_STR (& ! CW - IDF
         &(/ 6, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'IDF/all        ', '               ',&
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
         &'               ', '               ',&
         &'               ', '               '/),&
         &357,& ! observations
         &0.656272,& ! constant
         &-1.027327,& ! cos-asp
         &0.0,& ! sin-asp
         &0.0,& ! elev
         &0.0,& ! elev^2
         &0.0,& ! ccf
         &0.089231,& ! diam
         &-0.0016866,& ! diam^2
         &0.0,& ! NO bal/dbh
         &-0.0432885,& ! bal/log(dbh+1)
         &0.0,& ! crown
         &0.0,& ! NO BAL
         &0.88265& ! sigmar
         &) /

    data ZNKONST(13) /&
         &MD_STR (& ! PL - ICH
         &(/ 7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'ICH/all        ', '               ',&
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
         &'               ', '               ',&
         &'               ', '               '/),&
         &1313,& ! observations
         &0.0,& ! constant
         &0.0,& ! cos-asp
         &0.0,& ! sin-asp
         &0.0,& ! elev
         &0.0,& ! elev^2
         &0.0,& ! ccf
         &0.0209,& ! diam
         &-0.00067,& ! diam^2
         &-0.5552,& ! bal/dbh
         &0.0,& ! NO bal/log(dbh+1)
         &1.8153,& ! crown
         &0.0,& ! bal
         &0.72326& ! sigmar
         &) /

    data ZNKONST(14) /&
         &MD_STR (& ! PL - IDF
         &(/ 7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'IDF/all        ', '               ',&
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
         &'               ', '               ',&
         &'               ', '               '/),&
         &2009,& ! observations
         &-0.2238,& ! constant
         &0.0,& ! cos-asp
         &0.0,& ! sin-asp
         &0.0,& ! elev
         &0.0,& ! elev^2
         &-0.3124,& ! ccf
         &0.00993,& ! diam
         &-0.00038,& ! diam^2
         &0.0,& ! NO bal/dbh
         &-0.0433,& ! bal/log(dbh+1)
         &1.5667,& ! crown
         &0.0,& ! NO BAL
         &0.6249& ! sigmar
         &) /

    data ZNKONST(15) /&
         &MD_STR (& ! SE - ICH
         &(/ 8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'ICH/all        ', '               ',&
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
         &'               ', '               ',&
         &'               ', '               '/),&
         &424,& ! observations
         &-0.5766,& ! constant
         &0.0,& ! cos-asp
         &0.0,& ! sin-asp
         &0.0,& ! elev
         &0.0,& ! elev^2
         &-0.0825,& ! ccf
         &0.0196,& ! diam
         &-0.00028,& ! diam^2
         &0.0,& ! bal/dbh
         &0.0,& ! NO bal/log(dbh+1)
         &1.9456,& ! crown
         &-0.00866,& ! bal
         &0.87288& ! sigmar
         &) /

    data ZNKONST(16) /&
         &MD_STR (& ! SE - IDF
         &(/ 8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'IDF/all        ', '               ',&
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
         &'               ', '               ',&
         &'               ', '               '/),&
         &102,& ! observations
         &-0.8694668,& ! constant
         &0.0,& ! cos-asp
         &0.0,& ! sin-asp
         &0.0,& ! elev
         &0.0,& ! elev^2
         &0.0,& ! ccf
         &0.0214183,& ! diam
         &-0.00090472,& ! diam^2
         &0.0,& ! NO bal/dbh
         &-0.1589753,& ! bal/log(dbh+1)
         &3.3882046,& ! crown
         &0.0,& ! NO BAL
         &0.84611& ! sigmar
         &) /

    data ZNKONST(17) /&
         &MD_STR (& ! BL,BG - ICH+IDF
         &(/ 9, 4, 0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'ICH/all        ', 'IDF/all        ',&
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
         &'               ', '               ',&
         &'               ', '               '/),&
         &350,& ! observations
         &0.6265,& ! constant
         &0.0,& ! cos-asp
         &0.0,& ! sin-asp
         &0.0,& ! elev
         &0.0,& ! elev^2
         &-1.0897,& ! ccf
         &0.0515,& ! diam
         &-0.00084,& ! diam^2
         &0.0,& ! bal/dbh
         &0.0,& ! NO bal/log(dbh+1)
         &1.5198,& ! crown
         &0.0,& ! bal
         &0.92423& ! sigmar
         &) /

    data ZNKONST(18) /&
         &MD_STR (& ! PY - IDF+ICH
         &(/ 10, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'ICH/all        ', 'IDF/all        ',&
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
         &'               ', '               ',&
         &'               ', '               '/),&
         &450,& ! observations
         &-0.50298,& ! constant
         &-0.613773,& ! cos-asp
         &0.596886,& ! sin-asp
         &0.0,& ! elev
         &0.0,& ! elev^2
         &0.0,& ! NO CCF
         &0.0862013,& ! diam
         &-0.00115276,& ! diam^2
         &-0.609809,& ! bal/dbh
         &0.0,& ! NO bal/log(dbh+1)
         &0.0,& ! NO CROWN
         &0.0,& ! NO BAL
         &0.86319& ! sigmar
         &) /

    data ZNKONST(19) /&
         &MD_STR (& ! EP,OH - ICH
         &(/11,15, 0,0,0,0,0,0,0,0,0,0,0,0,0 /),& ! spp
         &(/'ICH/all        ', '               ',&
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
         &'               ', '               ',&
         &'               ', '               '/),&
         &835,& ! observations
         &-3.6197,& ! constant
         &0.0,& ! cos-asp
         &0.0,& ! sin-asp
         &0.7542,& ! elev
         &-0.0425,& ! elev^2
         &-0.2492,& ! ccf
         &0.0740,& ! diam
         &-0.00116,& ! diam^2
         &0.0,& ! bal/dbh
         &0.0,& ! NO bal/log(dbh+1)
         &1.1239,& ! crown
         &-0.0198,& ! bal
         &0.68942& ! sigmar
         &) /

    data ZNKONST(20) /&
         &MD_STR (& ! EP,OH - IDF
         &(/11,15, 0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'IDF/all        ', '               ',&
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
         &'               ', '               ',&
         &'               ', '               '/),&
         &418,& ! observations
         &0.0,& ! constant
         &0.0,& ! cos-asp
         &0.0,& ! sin-asp
         &0.0,& ! elev
         &0.0,& ! elev^2
         &0.0,& ! ccf
         &1.1315,& ! diam
         &-0.0012,& ! diam^2
         &0.3599,& ! additive bal/dbh
         &-0.2306,& ! multiplicative bal/dbh
         &0.0,& ! crown
         &0.0,& ! NO BAL
         &0.78715& ! sigmar
         &) /

    !       ASPEN MODEL HAS DIFFERENT STRUCTURE...

    data ZNKONST(21) /&
         &MD_STR (& ! AT,CT - IDF+ICH
         &(/ 12,13, 0,0,0,0,0,0,0,0,0,0,0,0,0 /),& ! spp
         &(/'ICH/all        ', 'IDF/all        ',&
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
         &'               ', '               ',&
         &'               ', '               '/),&
         &727,& ! observations
         &0.0,& ! constant
         &0.0,& ! cos-asp
         &0.0,& ! sin-asp
         &0.0,& ! elev
         &0.0,& ! elev^2
         &0.0,& ! NO CCF
         &0.1372,& ! diam
         &-0.0015,& ! diam^2
         &1.0768,& ! additive bal/dbh
         &-0.1983,& ! multiplicative bal/dbh
         &0.0,& ! NO CROWN
         &0.0,& ! NO BAL
         &0.78243& ! sigmar
         &) /

    !       SBS and SBPS zones...

    data ZNKONST(22) /&
         &MD_STR (& ! Pl SBS
         &(/7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'SBSdw1/01      ', 'SBSdw2/01      ',&
         &'SBSmc1/01      ', 'SBSmc2/01      ',&
         &'SBSmh/01       ', 'SBSmm/01       ',&
         &'SBSmw/01       ', 'SBSwk1/01      ',&
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
         &404,& ! observations
         &-1.2421,& ! constant
         &0.0,& ! cos-asp
         &0.00955,& ! sin-asp
         &0.0,& ! elev
         &0.0,& ! elev^2
         &0.0,& ! ccf
         &0.0630,& ! diam                     KB?
         &-0.0012,& ! diam^2
         &0.0,& ! bal/dbh
         &0.0,& ! NO bal/log(dbh+1)
         &1.6043,& ! crown
         &-0.0202,& ! bal
         &0.419& ! sigmar
         &) /

    data ZNKONST(23) /&
         &MD_STR (& ! Pl SBPSdc
         &(/7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'SBPSdc/01      ', 'SBPSxc/01      ',&
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
         &'               ', '               ',&
         &'               ', '               '/),&
         &38,& ! observations
         &-4.1693,& ! constant
         &0.0,& ! cos-asp
         &0.0,& ! sin-asp
         &0.0,& ! elev
         &0.0,& ! elev^2
         &-0.00832,& ! ccf
         &2.4409,& ! diam
         &-0.00565,& ! diam^2
         &0.0,& ! bal/dbh
         &0.0,& ! NO bal/log(dbh+1)
         &1.3229,& ! crown
         &0.0,& ! bal
         &0.893& ! sigmar
         &) /

    data ZNKONST(24) /&
         &MD_STR (& ! Pl SBPSmk
         &(/7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'SBPSmc/01      ', 'SBPSmk/01      ',&
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
         &'               ', '               ',&
         &'               ', '               '/),&
         &28,& ! observations
         &-0.9005,& ! constant
         &0.0,& ! cos-asp
         &0.0,& ! sin-asp
         &0.0,& ! elev
         &0.0,& ! elev^2
         &-0.00414,& ! ccf
         &0.7488,& ! diam
         &-0.00075,& ! diam^2
         &0.0,& ! bal/dbh
         &0.0,& ! NO bal/log(dbh+1)
         &1.3599,& ! crown
         &0.0,& ! bal
         &0.314& ! sigmar
         &) /

    data ZNKONST(25) /&
         &MD_STR (& ! AT - SBPSmk, SBPSdc
         &(/12, 15, 0,0,0,0,0,0,0,0,0,0,0,0,0 /),& ! spp
         &(/'SBPSmc/01      ', 'SBPSmk/01      ',&
         &'SBPSdc/01      ', 'SBPSxc/01      ',&
         &'SBSdw1/01      ', 'SBSdw2/01      ',&
         &'SBSmc1/01      ', 'SBSmc2/01      ',&
         &'SBSmh/01       ', 'SBSmm/01       ',&
         &'SBSmw/01       ', 'SBSwk1/01      ',&
         &'SBS/all        ', 'SBPS/all       ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &27,& ! observations
         &1.1487,& ! constant
         &0.0,& ! cos-asp
         &0.0,& ! sin-asp
         &0.0,& ! elev
         &0.0,& ! elev^2
         &-0.00310,& ! ccf
         &-0.0668,& ! diam
         &0.0,& ! diam^2
         &0.0,& ! additive bal/dbh
         &0.0,& ! multiplicative bal/dbh
         &0.0,& ! crown
         &0.0,& ! bal
         &0.311& ! sigmar
         &) /

    data ZNKONST(26) /&
         &MD_STR (& ! Bl SBS
         &(/9, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'SBPSmc/01      ', 'SBPSmk/01      ',&
         &'SBPSdc/01      ', 'SBPSxc/01      ',&
         &'SBSdw1/01      ', 'SBSdw2/01      ',&
         &'SBSmc1/01      ', 'SBSmc2/01      ',&
         &'SBSmh/01       ', 'SBSmm/01       ',&
         &'SBSmw/01       ', 'SBSwk1/01      ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &456,& ! observations            KB?
         &0.0,& ! constant
         &0.0,& ! cos-asp
         &0.0,& ! sin-asp
         &0.0,& ! elev
         &0.0,& ! elev^2
         &-0.0152,& ! ccf
         &0.0737,& ! diam                    KB?
         &-0.00182,& ! diam^2
         &0.0,& ! bal/dbh
         &0.0,& ! NO bal/log(dbh+1)
         &3.1925,& ! crown
         &0.0,& ! bal
         &0.0& ! sigmar                  KB?
         &) /

    data ZNKONST(27) /&
         &MD_STR (& ! Sx SBSdw1, SBSdw2, SBSmh
         &(/8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'SBPSmc/01      ', 'SBPSmk/01      ',&
         &'SBPSdc/01      ', 'SBPSxc/01      ',&
         &'SBSdw1/01      ', 'SBSdw2/01      ',&
         &'SBSmc1/01      ', 'SBSmc2/01      ',&
         &'SBSmh/01       ', 'SBSmm/01       ',&
         &'SBSmw/01       ', 'SBSwk1/01      ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &501,& ! observations
         &0.9159,& ! constant
         &0.0,& ! cos-asp
         &0.0,& ! sin-asp
         &0.0,& ! elev
         &0.0,& ! elev^2
         &-0.00397,& ! ccf
         &0.00620,& ! diam
         &-0.00009,& ! diam^2
         &0.0,& ! bal/dbh
         &0.0,& ! NO bal/log(dbh+1)
         &0.5586,& ! crown
         &0.0,& ! bal
         &0.786& ! sigmar
         &) /

    data ZNKONST(28) /&
         &MD_STR (& ! Fd SBSdw1, SBSdw2, SBSmh
         &(/3, 14, 0,0,0,0,0,0,0,0,0,0,0,0,0 /),& ! spp
         &(/'SBPSmc/01      ', 'SBPSmk/01      ',&
         &'SBPSdc/01      ', 'SBPSxc/01      ',&
         &'SBSdw1/01      ', 'SBSdw2/01      ',&
         &'SBSmc1/01      ', 'SBSmc2/01      ',&
         &'SBSmh/01       ', 'SBSmm/01       ',&
         &'SBSmw/01       ', 'SBSwk1/01      ',&
         &'SBS/all        ', 'SBPS/all       ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &568,& ! observations
         &1.0901,& ! constant
         &0.0,& ! cos-asp
         &0.0,& ! sin-asp
         &0.0,& ! elev
         &0.0,& ! elev^2
         &-0.00403,& ! ccf
         &0.0144,& ! diam
         &-0.0001,& ! diam^2
         &0.0,& ! bal/dbh
         &0.0,& ! NO bal/log(dbh+1)
         &0.05062,& ! crown
         &-0.00858,& ! bal
         &0.855& ! sigmar
         &) /

    !     SITE SERIES COEFFICIENTS

    data SSKONST(1) /&
         &SS_STR (& ! ICH+IDF PW (zonal model: no site index)
         &(/ 1, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'ICH/all        ', 'IDF/all        ',&
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
         &'               ', '               ',&
         &'               ', '               '/),&
         &0.0&
         &) /

    data SSKONST(2) /&
         &SS_STR (& ! ICH+IDF LW (zonal model; rare in IDF: no site index)
         &(/ 2, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'ICH/all        ', 'IDF/all        ',&
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
         &'               ', '               ',&
         &'               ', '               '/),&
         &0.0&
         &) /

    data SSKONST(3) /&
         &SS_STR (& ! ICH+IDF FD,OC (model coeffs incorporate SIBEC)
         &(/ 3,14,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'ICH/all        ', 'IDF/all        ',&
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
         &'               ', '               ',&
         &'               ', '               '/),&
         &0.0&
         &) /

    data SSKONST(4) /&
         &SS_STR (& ! ICH HW 15m 1-18 (should be absent in IDF)
         &(/ 5, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'ICHmw2/02      ', 'ICHmw2/06      ',&
         &'ICHmw2/07      ', 'ICHmw3/08      ',&
         &'ICHvk1/05      ', 'ICHvk1/06      ',&
         &'ICHwk1/02      ', 'ICHwk1/03      ',&
         &'ICHwk1/06      ', 'ICHwk1/07      ',&
         &'ICHmw1/02      ', 'ICHmw1/03      ',&
         &'ICHmw1/04      ', 'ICHmw1/07      ',&
         &'ICHmw2/03      ', 'ICHmw2/08      ',&
         &'ICHmw3/04      ', 'ICHwk1/08      ',&
         &'ICH/all        ', 'IDF/all        ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &0.0&
         &) /

    data SSKONST(5) /&
         &SS_STR (& ! ICH HW 18m 1-28
         &(/ 5, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'ICHmw2/01      ', 'ICHmw2/01-ys   ',&
         &'ICHmw2/03      ', 'ICHmw2/04      ',&
         &'ICHmw2/05      ', 'ICHmw3/01      ',&
         &'ICHmw3/01-yc   ', 'ICHmw3/05      ',&
         &'ICHmw3/06      ', 'ICHmw3/07      ',&
         &'ICHvk1/01      ', 'ICHvk1/02      ',&
         &'ICHvk1/03      ', 'ICHvk1/04      ',&
         &'ICHwk1/01      ', 'ICHwk1/04      ',&
         &'ICHwk1/05      ', 'ICHdw/01a      ',&
         &'ICHdw/01b      ', 'ICHdw/02       ',&
         &'ICHdw/03       ', 'ICHdw/04       ',&
         &'ICHmw1/01      ', 'ICHmw1/05      ',&
         &'ICHmw1/06      ', 'ICHmw2/05      ',&
         &'ICHmw2/06      ', 'ICHxw/01       ',&
         &'               ', '               '/),&
         &0.0&
         &) /

    data SSKONST(6) /&
         &SS_STR (& ! ICH CW 15m 1-28
         &(/ 6, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'ICHmk1/01      ', 'ICHmk1/01-ys   ',&
         &'ICHmk1/04      ', 'ICHmk1/07      ',&
         &'ICHmk2/01      ', 'ICHmk2/04      ',&
         &'ICHmk2/06      ', 'ICHmw2/02      ',&
         &'ICHmw3/01      ', 'ICHmw3/01-yc   ',&
         &'ICHmw3/03      ', 'ICHmw3/04      ',&
         &'ICHmw3/05      ', 'ICHmw3/08      ',&
         &'ICHvk1/05      ', 'ICHvk1/06      ',&
         &'ICHwk1/02      ', 'ICHwk1/03      ',&
         &'ICHwk1/06      ', 'ICHwk1/07      ',&
         &'ICHdw/01a      ', 'ICHmw1/02      ',&
         &'ICHmw1/03      ', 'ICHmw1/04      ',&
         &'ICHmw1/07      ', 'ICHmw2/03      ',&
         &'ICHwk1/08      ', 'ICHxw/01       ',&
         &'ICH/all        ', '               '/),&
         &0.0&
         &) /

    data SSKONST(7) /&
         &SS_STR (& ! ICH CW 18m 1-25
         &(/ 6, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'ICHmk1/05      ', 'ICHmk1/06      ',&
         &'ICHmk2/05      ', 'ICHmw2/01      ',&
         &'ICHmw2/01-ys   ', 'ICHmw2/03      ',&
         &'ICHmw2/04      ', 'ICHmw2/05      ',&
         &'ICHmw3/06      ', 'ICHmw3/07      ',&
         &'ICHvk1/01      ', 'ICHvk1/03      ',&
         &'ICHvk1/04      ', 'ICHwk1/01      ',&
         &'ICHwk1/04      ', 'ICHwk1/05      ',&
         &'ICHdw/01b      ', 'ICHdw/02       ',&
         &'ICHdw/03       ', 'ICHdw/04       ',&
         &'ICHmw1/01      ', 'ICHmw1/05      ',&
         &'ICHmw1/06      ', 'ICHmw2/04      ',&
         &'ICHmw2/06      ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &0.0466&
         &) /

    data SSKONST(8) /&
         &SS_STR (& ! IDF CW (zonal model: no site index)
         &(/ 6, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'IDF/all        ', '               ',&
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
         &'               ', '               ',&
         &'               ', '               '/),&
         &0.0&
         &) /

    data SSKONST(9) /&
         &SS_STR (& ! ICH PL 21m 1-30
         &(/ 7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'ICHdk/02       ', 'ICHdk/03       ',&
         &'ICHdk/04       ', 'ICHmk3/02      ',&
         &'ICHmk3/03      ', 'ICHmk3/05      ',&
         &'ICHmk3/06      ', 'ICHmk3/07      ',&
         &'ICHwk2/02      ', 'ICHwk2/03      ',&
         &'ICHwk2/04      ', 'ICHwk2/05      ',&
         &'ICHwk2/06      ', 'ICHwk2/07      ',&
         &'ICHwk4/02      ', 'ICHwk4/03      ',&
         &'ICHwk4/04      ', 'ICHwk4/05      ',&
         &'ICHwk4/06      ', 'ICHwk4/07      ',&
         &'ICHmk1/01      ', 'ICHmk1/01-ys   ',&
         &'ICHmk1/02      ', 'ICHmk1/03      ',&
         &'ICHmk1/04      ', 'ICHmk1/05      ',&
         &'ICHmk1/07      ', 'ICHmk2/01      ',&
         &'ICHmk2/02      ', 'ICHmk2/03      '/),&
         &0.0&
         &) /

    data SSKONST(10) /&
         &SS_STR (& ! ICH PL 21m 31-50
         &(/ 7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'ICHmk2/04      ', 'ICHmk2/06      ',&
         &'ICHmw2/02      ', 'ICHmw3/01      ',&
         &'ICHmw3/01-yc   ', 'ICHmw3/02      ',&
         &'ICHmw3/03      ', 'ICHmw3/04      ',&
         &'ICHmw3/05      ', 'ICHwk1/02      ',&
         &'ICHwk1/03      ', 'ICHwk1/04      ',&
         &'ICHdw/01a      ', 'ICHdw/02       ',&
         &'ICHmw1/02      ', 'ICHmw1/03      ',&
         &'ICHmw1/04      ', 'ICHmw1/07      ',&
         &'ICHmw2/03      ', 'ICHxw/01       ',&
         &'ICH/all        ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &0.0&
         &) /

    data SSKONST(11) /&
         &SS_STR (& ! ICH PL 24m 1-22
         &(/ 7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'ICHdk/01       ', 'ICHdk/05       ',&
         &'ICHmk3/01      ', 'ICHmk3/04      ',&
         &'ICHwk2/01      ', 'ICHwk4/01      ',&
         &'ICHmk1/06      ', 'ICHmk2/05      ',&
         &'ICHmw2/01      ', 'ICHmw2/01-ys   ',&
         &'ICHmw2/03      ', 'ICHmw2/04      ',&
         &'ICHmw2/05      ', 'ICHmw3/06      ',&
         &'ICHwk1/01      ', 'ICHdw/01b      ',&
         &'ICHdw/03       ', 'ICHdw/04       ',&
         &'ICHmw1/01      ', 'ICHmw1/05      ',&
         &'ICHmw1/06      ', 'ICHmw2/06      ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &0.2625&
         &) /

    data SSKONST(12) /&
         &SS_STR (& ! IDF PL 15m 1-20
         &(/ 7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'IDFdk3/02      ', 'IDFdk3/03      ',&
         &'IDFdk3/05      ', 'IDFdk4/01      ',&
         &'IDFdk4/02      ', 'IDFdk4/05      ',&
         &'IDFdk4/06      ', 'IDFdk4/07      ',&
         &'IDFdk4/08      ', 'IDFdk4/09      ',&
         &'IDFxm/03       ', 'IDFdk1/02      ',&
         &'IDFdk1/03      ', 'IDFdk1/04      ',&
         &'IDFdk3/06      ', 'IDFdm1/03      ',&
         &'IDFdm1/04      ', 'IDFmw1/03      ',&
         &'IDFww/02       ', 'IDFdm2/03      ',&
         &'IDF/all        ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &0.0&
         &) /

    data SSKONST(13) /&
         &SS_STR (& ! IDF PL 18m 1-18
         &(/ 7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'IDFdk3/01      ', 'IDFdk3/06      ',&
         &'IDFdk3/07      ', 'IDFdk3/08      ',&
         &'IDFdk3/09      ', 'IDFdw/01       ',&
         &'IDFdk1/01      ', 'IDFdk2/01      ',&
         &'IDFdk2/03      ', 'IDFdk2/04      ',&
         &'IDFdm1/01      ', 'IDFmw1/04      ',&
         &'IDFmw2/02      ', 'IDFmw2/05      ',&
         &'IDFww/03       ', 'IDFdm2/01      ',&
         &'IDFdm2/04      ', 'IDFdm2/07      ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &0.0&
         &) /

    data SSKONST(14) /&
         &SS_STR (& ! IDF PL 21m 1-17
         &(/ 7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'IDFdk1/05      ', 'IDFdk1/06      ',&
         &'IDFdk2/05      ', 'IDFdk2/06      ',&
         &'IDFdk2/07      ', 'IDFdm1/05      ',&
         &'IDFdm1/06      ', 'IDFdm1/07      ',&
         &'IDFmw1/01      ', 'IDFmw1/01-yc   ',&
         &'IDFmw1/05      ', 'IDFmw1/06      ',&
         &'IDFmw2/01      ', 'IDFmw2/03      ',&
         &'IDFmw2/04      ', 'IDFww/01       ',&
         &'IDFdm2/05      ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &0.3479&
         &) /

    data SSKONST(15) /&
         &SS_STR (& ! ICH SE 18m 1-30
         &(/ 8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'ICHwk2/04      ', 'ICHwk4/04      ',&
         &'ICHwk4/05      ', 'ICHwk4/08      ',&
         &'ICHmk1/01      ', 'ICHmk1/01-ys   ',&
         &'ICHmk1/03      ', 'ICHmk1/04      ',&
         &'ICHmk1/07      ', 'ICHmk2/01      ',&
         &'ICHmk2/03      ', 'ICHmk2/04      ',&
         &'ICHmk2/06      ', 'ICHmw2/01      ',&
         &'ICHmw2/01-ys   ', 'ICHmw2/02      ',&
         &'ICHmw2/03      ', 'ICHmw2/07      ',&
         &'ICHmw3/01      ', 'ICHmw3/01-yc   ',&
         &'ICHmw3/03      ', 'ICHmw3/04      ',&
         &'ICHmw3/05      ', 'ICHmw3/08      ',&
         &'ICHvk1/06      ', 'ICHwk1/03      ',&
         &'ICHwk1/07      ', 'ICHmw1/02      ',&
         &'ICHmw1/03      ', 'ICHmw1/04      '/),&
         &0.0&
         &) /

    data SSKONST(16) /&
         &SS_STR (& ! ICH SE 18m 31-35
         &(/ 8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'ICHmw1/07      ', 'ICHmw2/03      ',&
         &'ICHmw2/04      ', 'ICHmw2/08      ',&
         &'ICHwk1/08      ', 'ICH/all        ',&
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
         &0.0&
         &) /

    data SSKONST(17) /&
         &SS_STR (& ! ICH SE 21m 1-30
         &(/ 8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'ICHdk/01       ', 'ICHdk/04       ',&
         &'ICHdk/05       ', 'ICHmk3/01      ',&
         &'ICHmk3/04      ', 'ICHmk3/05      ',&
         &'ICHmk3/06      ', 'ICHmk3/07      ',&
         &'ICHwk2/01      ', 'ICHwk2/05      ',&
         &'ICHwk2/06      ', 'ICHwk2/07      ',&
         &'ICHwk2/08      ', 'ICHwk4/01      ',&
         &'ICHwk4/06      ', 'ICHwk4/07      ',&
         &'ICHmk1/05      ', 'ICHmk1/06      ',&
         &'ICHmk2/05      ', 'ICHmw2/04      ',&
         &'ICHmw2/05      ', 'ICHmw2/06      ',&
         &'ICHmw3/06      ', 'ICHmw3/07      ',&
         &'ICHvk1/01      ', 'ICHvk1/02      ',&
         &'ICHvk1/03      ', 'ICHvk1/04      ',&
         &'ICHvk1/05      ', 'ICHwk1/01      '/),&
         &0.1750&
         &) /

    data SSKONST(18) /&
         &SS_STR (& ! ICH SE 21m 31-41
         &(/ 8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'ICHwk1/04      ', 'ICHwk1/05      ',&
         &'ICHwk1/06      ', 'ICHdw/01b      ',&
         &'ICHdw/03       ', 'ICHdw/04       ',&
         &'ICHmw1/01      ', 'ICHmw1/05      ',&
         &'ICHmw1/06      ', 'ICHmw2/07      ',&
         &'ICHwk1/07      ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &0.1750&
         &) /

    data SSKONST(19) /&
         &SS_STR (& ! IDF SE (zonal model: no site index)
         &(/ 8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'IDF/all        ', '               ',&
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
         &'               ', '               ',&
         &'               ', '               '/),&
         &0.0&
         &) /

    data SSKONST(20) /&
         &SS_STR (& ! ICH BL,BG 15m 1-9
         &(/ 9, 4 ,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'ICHmk1/03      ', 'ICHmk2/03      ',&
         &'ICHmw2/02      ', 'ICHmw3/03      ',&
         &'ICHmw3/08      ', 'ICHvk1/06      ',&
         &'ICHwk1/02      ', 'ICHmw1/02      ',&
         &'ICHmw2/03      ', 'ICH/all        ',&
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
         &0.0&
         &) /

    data SSKONST(21) /&
         &SS_STR (& ! ICH BL,BG 18m 1-29
         &(/ 9, 4, 0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'ICHmk1/01      ', 'ICHmk1/01-ys   ',&
         &'ICHmk1/04      ', 'ICHmk1/07      ',&
         &'ICHmk2/01      ', 'ICHmk2/06      ',&
         &'ICHmw2/01      ', 'ICHmw2/01-ys   ',&
         &'ICHmw2/03      ', 'ICHmw2/06      ',&
         &'ICHmw2/07      ', 'ICHmw3/01      ',&
         &'ICHmw3/01-yc   ', 'ICHmw3/05      ',&
         &'ICHvk1/02      ', 'ICHvk1/03      ',&
         &'ICHvk1/05      ', 'ICHwk1/03      ',&
         &'ICHwk1/04      ', 'ICHwk1/06      ',&
         &'ICHwk1/07      ', 'ICHdw/01a      ',&
         &'ICHmw1/01      ', 'ICHmw1/03      ',&
         &'ICHmw1/04      ', 'ICHmw1/07      ',&
         &'ICHmw2/04      ', 'ICHmw2/08      ',&
         &'ICHwk1/08      ', '               '/),&
         &0.0807&
         &) /

    data SSKONST(22) /&
         &SS_STR (& ! ICH BL,BG 21m 1-18
         &(/ 9, 4, 0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'ICHmk1/05      ', 'ICHmk1/06      ',&
         &'ICHmk2/05      ', 'ICHmw2/04      ',&
         &'ICHmw2/05      ', 'ICHmw3/06      ',&
         &'ICHmw3/07      ', 'ICHvk1/01      ',&
         &'ICHvk1/04      ', 'ICHwk1/01      ',&
         &'ICHwk1/05      ', 'ICHdw/01b      ',&
         &'ICHdw/03       ', 'ICHdw/04       ',&
         &'ICHmw1/05      ', 'ICHmw1/06      ',&
         &'ICHmw2/05      ', 'ICHmw2/06      ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &0.2422&
         &) /

    data SSKONST(23) /&
         &SS_STR (& ! IDF BL,BG 1-5 (zonal model: use ICH SI 15)
         &(/ 9, 4, 0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'IDF/all        ', '               ',&
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
         &'               ', '               ',&
         &'               ', '               '/),&
         &0.0&
         &) /

    data SSKONST(24) /&
         &SS_STR (& ! ICH+IDF PY 1-30 (zonal model: no site index)
         &(/10, 0,0,0,0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'IDF/all        ', 'ICH/all        ',&
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
         &'               ', '               ',&
         &'               ', '               '/),&
         &0.0&
         &) /

    data SSKONST(25) /&
         &SS_STR (& ! ICH+IDF EP,OH,AT,AC (zonal model for all hardwoords)
         &(/11, 12, 13, 15, 0,0,0,0,0,0,0,0,0,0,0  /),& ! spp
         &(/'IDF/all        ', 'ICH/all        ',&
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
         &'               ', '               ',&
         &'               ', '               '/),&
         &0.0&
         &) /

    if (LCONS) go to 1000

    call DBCHK (DEBUG,'DGF',3,ICYC)

    !     V2: SCALE BASAL AREA; FIND SEICCF TERMS
    if (LV2ATV) then
       BA100 = BA/100.
       call SEILTDG (BECADJ,SEICN2,&
            &SEISLP,SEISAS,SEICAS,SEIELV,SEIEL2,SEICCF,LSPPOK)
    end if

    !     BEGIN SPECIES LOOP. ASSIGN VARIABLES WHICH ARE SPECIES DEPENDENT

    do ISPC = 1,MAXSP

       I1 = ISCT(ISPC,1)
       if (I1.eq.0) go to 20
       I2 = ISCT(ISPC,2)

       if (LV2ATV) then

          if (.not.LSPPOK(ISPC)) SEICCF(ISPC) = 0.0
          CONSPP = DGCON(ISPC) + COR(ISPC) +&
               &0.01 * (DGCCF(ISPC) + SEICCF(ISPC)) * RELDEN

          DGLDS = DGLD(ISPC)
          DGBALS = DGBAL(ISPC)
          DGCRS = DGCR(ISPC)
          DGCRS2 = DGCRSQ(ISPC)
          DGDSQS = DGDSQ(ISPC)
          DGDBLS = DGDBAL(ISPC)

       else

          !         HW MODEL IN ICH MUST HAVE AT CCF>=100
          !         BL MODEL IN ICH MUST HAVE AT CCF>=125
          !         EP/OH MODEL IN IDF MUST HAVE AT CCF>=100
          !         PL MODEL IN IDF MUST HAVE AT CCF>=100

          RELDN2 = RELDEN
          select case (ISPC)
          case (5)
             if (INDEX(BEC%Zone,'ICH') .gt.0)&
                  &RELDN2 = MAX(100.0, RELDEN)
          case (7)
             if (INDEX(BEC%Zone,'IDF') .gt.0)&
                  &RELDN2 = MAX(100.0, RELDEN)
          case (9)
             if (INDEX(BEC%Zone,'ICH') .gt.0)&
                  &RELDN2 = MAX(125.0, RELDEN)
          case (11,15)
             if ((INDEX(BEC%Zone,'ICH') .gt.0) .or.&
                  &(INDEX(BEC%Zone,'IDF') .gt.0))&
                  &RELDN2 = MAX(100.0, RELDEN)
          end select
          CONSPP = DGCON(ISPC) + COR(ISPC) +&
               &DGCCF1(ISPC) * RELDN2 * 0.01

       end if

       !       BEGIN TREE LOOP WITHIN SPECIES ISPC.
       do I3 = I1,I2
          I = IND1(I3)
          if (DIAM(I).le.0.0) goto 10

          CR = ICR(I) * 0.01
          if (LV2ATV) then
             D = DIAM(I)
             ALD = ALOG(D)
             BAL = (1.0 - (PCT(I)/100.)) * BA100
          else
             D = DIAM(I) * INtoCM
             BAL = (1.0 - (PCT(I)/100.)) * BA * FT2pACRtoM2pHA
             BALD1 = BAL/D
             BALD2 = BAL/LOG(D+1.0)

             !           HW MODEL IN ICH MUST HAVE CROWN<=0.80
             !           EP/OH MODEL MUST HAVE CROWN<=0.80
             select case (ISPC)
             case (5)
                if (INDEX(BEC%Zone,'ICH') .gt.0)&
                     &CR = MIN(0.8, CR)
             case (11,15)
                if ((INDEX(BEC%Zone,'ICH') .gt.0.) .or.&
                     &(INDEX(BEC%Zone,'IDF') .gt.0))&
                     &CR = MIN(0.8, CR)
             end select
          end if

          if (LV2ATV) then

             DDS = CONSPP + DGLDS*ALD + DGBALS*BAL + CR*(DGCRS+CR*DGCRS2)&
                  &+DGDSQS*D*D  + DGDBLS*BAL/(ALOG(D+1.0))

          else

             select case (ISPC)

                ! new birch IDF model form in IDF; old form in ICH
             case (11,15)
                if (INDEX(BEC%Zone,'IDF') .gt.0) then
                   D2 = MAX(D,1.0) ! CONSTRAIN TO 1 CM
                   BALD1 = BAL/D2
                   DDS = DGLD1(ISPC) * (D2 **&
                        &(DGDBAL1(ISPC) + DGDBAL2(ISPC) * BALD1)) *&
                        &EXP(DGDSQ1(ISPC) * D2 * D2)
                else if (INDEX(BEC%Zone,'ICH') .gt.0) then
                   DDS = CONSPP&
                        &+ DGLD1(ISPC)   * D&
                        &+ DGDSQ1(ISPC)  * D*D&
                        &+ DGBAL1(ISPC)  * BAL&
                        &+ DGDBAL1(ISPC) * BALD1&
                        &+ DGDBAL2(ISPC) * BALD2&
                        &+ DGCR1(ISPC)   * CR
                   DDS = EXP(MAX(-9.21,DDS))
                end if

                ! new aspen model form
             case (12,13)
                D2 = MAX(D,1.0) ! CONSTRAIN TO 1 CM
                BALD1 = BAL/D2
                DDS = DGLD1(ISPC) * (D2 **&
                     &(DGDBAL1(ISPC) + DGDBAL2(ISPC) * BALD1)) *&
                     &EXP(DGDSQ1(ISPC) * D2 * D2)

             case default
                DDS = CONSPP&
                     &+ DGLD1(ISPC)    * D&
                     &+ DGDSQ1(ISPC)   * D*D&
                     &+ DGBAL1(ISPC)   * BAL&
                     &+ DGDBAL1(ISPC) * BALD1&
                     &+ DGDBAL2(ISPC) * BALD2&
                     &+ DGCR1(ISPC)    * CR
                DDS = EXP(MAX(-9.21,DDS))

             end select

             !           CONVERT DG [CM] PREDICTION TO LN(DDS) [IN]
             DDS = (DDS*DDS + 2.0 * DDS * D * BRATIO(ISPC,D,0.0)) *&
                  &CMtoIN * CMtoIN

          end if

          DDS = MAX(0.001,DDS) ! temporary: during SBS fit
          DDS = LOG(DDS)

          DDS = MAX(-9.21,DDS)
          WK2(I) = DDS

          !         END OF TREE LOOP. PRINT DEBUG INFO IF DESIRED.
          if (.not.DEBUG) go to 10
          write(JOSTND,9001) I,ISPC,D,BAL,CR,RELDEN,BA,DDS
          9001 format(' IN DGF, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,&
               &',  BAL=',F7.2,',  CR=',F7.4/&
               &'          CCF=',F9.3,',  BA=',F9.3,',   LN(DDS)=',F7.4)
          10 continue
       end do

       !  END OF SPECIES LOOP.

       20 continue
    end do
    return

    1000 continue

    !     ENTRY POINT FOR LOADING COEFFICIENTS OF THE DIAMETER INCREMENT
    !     MODEL THAT ARE SITE SPECIFIC AND NEED ONLY BE RESOLVED ONCE.
    !     ITYPE IS THE HABITAT TYPE INDEX AS COMPUTED IN **HABTYP**.
    !     ASPECT IS STAND ASPECT IN RADIANS.  OBSERV CONTAINS THE NUMBER OF
    !     OBSERVATIONS BY HABITAT CLASS BY SPECIES FOR THE UNDERLYING
    !     MODEL (THIS DATA IS ACTUALLY USED BY **DGDRIV** FOR CALIBRATION).

    !     ENTER LOOP TO LOAD SPECIES DEPENDENT VECTORS.

    !     REPLACE NULLS IN COMPILED DATA STRINGS WITH SPACES, SO THAT
    !     STRING LENGTHS WILL BE FIGURED CORRECTLY AND STRING COMPARISONS
    !     WILL WORK PROPERLY

    if (LV2ATV) then

       !       FETCH SEI VARIANT ADJUSTMENT TERMS
       call SEILTDG (BECADJ,SEICN2,&
            &SEISLP,SEISAS,SEICAS,SEIELV,SEIEL2,SEICCF,LSPPOK)

       do I = 1,MAXSP
          ISPHAB = MAPHAB(ITYPE,I)
          ISPFOR = MAPLOC(IFOR,I)
          ISPDSQ = MAPDSQ(IFOR,I)
          ISPCCF = MAPCCF(ITYPE,I)

          !         USE V2 SEI-PARAMETERIZATION IF IT EXISTS
          if (LSPPOK(I)) then
             DGCON1(I) = DGHAB(ISPHAB,I) + DGFOR(ISPFOR,I)&
                  !           ADD NI-AVERAGE SLOPE/ASPECT/ELEVATION USED DURING FITTING
                  &+  DGEL(I)    * BECADJ(3)&
                  &+  DGEL2(I)   * BECADJ(3) * BECADJ(3)&
                  &+ (DGSASP(I)  * SIN(BECADJ(2))&
                  &+  DGCASP(I)  * COS(BECADJ(2))&
                  &+  DGSLOP(I)) * BECADJ(1)&
                  &+  DGSLSQ(I)  * BECADJ(1) * BECADJ(1)

             !           USE SEI-DERIVED TERMS
             DGCON2(I) = SEICN2(I)&
                  &+  SEIELV(I)  * ELEV&
                  &+  SEIEL2(I)  * ELEV * ELEV&
                  &+ (SEISAS(I)  * SIN(ASPECT)&
                  &+  SEICAS(I)  * COS(ASPECT)&
                  &+  SEISLP(I)) * SLOPE

             DGCON(I) = DGCON1(I) + DGCON2(I)
             V2SEICOR(I) = EXP(DGCON2(I))

          else ! OTHERWISE USE THE ORIGINAL NI MODEL
             DGCON(I) = DGHAB(ISPHAB,I)&
                  &+  DGFOR(ISPFOR,I)&
                  &+  DGEL(I)    * ELEV&
                  &+  DGEL2(I)   * ELEV * ELEV&
                  &+ (DGSASP(I)  * SIN(ASPECT)&
                  &+  DGCASP(I)  * COS(ASPECT)&
                  &+  DGSLOP(I)) * SLOPE&
                  &+  DGSLSQ(I)  * SLOPE * SLOPE
             V2SEICOR(I) = 1.0
          end if

          DGDSQ(I) = DGDS(ISPDSQ,I)
          DGCCF(I) = DGCCFA(ISPCCF,I)
          ATTEN(I) = OBSERV(ISPHAB,I)
          SMCON(I) = 0. ! error here...

       end do

    else

       do J = 1,P_ZN
          do K = 1,P_ST
             do M = 1,P_ZL
                if (ICHAR(ZNKONST(J)%Zone(K)(M:M)) .eq.0)&
                     &ZNKONST(J)%Zone(K)(M:M) = ' '
             end do
          end do
       end do

       do J = 1,P_SS
          do K = 1,P_ST
             do M = 1,P_ZL
                if (ICHAR(SSKONST(J)%PrettyName(K)(M:M)) .eq.0)&
                     &SSKONST(J)%PrettyName(K)(M:M) = ' '
             end do
          end do
       end do

       !       MAKE TEMPORARY ZONE/SS MAPPING FOR SS IN THE KAMLOOPS ICHmw2.
       !       THESE SS DO NOT NATURALLY CORRESPOND TO THOSE OF THE NELSON REGION,
       !       AND ARE "INCREMENTED" BY ONE

       BEC2 = BEC
       if (INDEX(BEC2%Region,'KAM') .gt.0.and.&
            &INDEX(BEC2%FullName,'ICHmw2') .gt.0.and.&
            &INDEX(BEC2%Series,'-') .eq.0) then

          read (BEC2%Series,'(I4)') iSeries
          select case (iSeries)
          case (2)
             BEC2%PrettyName = "ICHmw2/03"
          case (3)
             BEC2%PrettyName = "ICHmw2/04"
          case (4)
             BEC2%PrettyName = "ICHmw2/05"
          case (5)
             BEC2%PrettyName = "ICHmw2/06"
          case (6)
             BEC2%PrettyName = "ICHmw2/07"
          case (7)
             BEC2%PrettyName = "ICHmw2/08"
          end select
       end if
       PAT2 = BEC2%PrettyName

       !       LOCATE ZONE USING "/all" PATTERN MATCH

       do I = 1,MAXSP
          IPOS(I) = 0
          JPOS(I) = 0
       end do

       !       FIND INDEX TO BECZONE COEFFICIENTS -> IPOS()
       do I = 1,MAXSP
          IPAS = 0
          PAT1 = PAT2
          7 do J = 1,P_ZN
             do K = 1,MAXSP
                if (ZNKONST(J)%SPP(K) .eq.0) exit
                if (ZNKONST(J)%SPP(K) .eq. I) then
                   do M = 1, P_ST
                      if (LEN_TRIM(ZNKONST(J)%Zone(M)) .eq.0) exit
                      if (INDEX(PAT1,ZNKONST(J)%Zone(M)) .gt.0) then
                         IPOS(I) = J
                         goto 8
                      end if
                   end do
                   exit
                end if
             end do
          end do
          if (J .gt. P_ZN .and. IPAS .eq.0) then
             IPAS = 1
             PAT1 = BEC2%Zone(1:LEN_TRIM(BEC%Zone))//'/all '
             goto 7
          end if
          8 if (IPOS(I) .eq.0) goto 5

          !         FIND INDEX TO SITE SERIES COEFFICIENTS -> JPOS()
          IPAS = 0
          PAT1 = PAT2
          9 do J = 1,P_SS
             do K = 1,MAXSP
                if (SSKONST(J)%SPP(K) .eq.0) exit
                if (SSKONST(J)%SPP(K) .eq. I) then
                   do M = 1, P_ST
                      if (LEN_TRIM(SSKONST(J)%PrettyName(M)) .eq.0) exit
                      if (INDEX(PAT1,SSKONST(J)%PrettyName(M)) .gt.0) then
                         JPOS(I) = J
                         goto 5
                      end if
                   end do
                   exit
                end if
             end do
          end do
          if (J .gt. P_SS .and. IPAS .eq.0) then
             IPAS = 1
             PAT1 = BEC2%Zone(1:LEN_TRIM(BEC%Zone))//'/all '
             goto 9
          end if
       5 end do

       !       BUILD EQUATION EXPRESSIONS
       do I = 1,MAXSP
          !ccc          LLTDGOK(I) = .FALSE.
          DGCON(I) = 0.0
          IP = IPOS(I)
          JP = JPOS(I)

          if (IP .lt.1.or. JP .lt.1) goto 11

          !         CONSTRAIN WHITE PINE TO RANGE: 500-1200m; ELEV COEFFS
          !         PARAMETERIZED WITH ELEV(m)/100; OR 5.0-12.0
          PELEV = ELEV * FTtoM
          if (I .eq.1) then
             PELEV = MIN(MAX(PELEV,5.0),12.0)
          end if

          DGCON(I) = SSKONST(JP)%CON&
               &+ ZNKONST(IP)%CON&
               &+ ZNKONST(IP)%EL   * PELEV&
               &+ ZNKONST(IP)%EL2  * (PELEV**2)&
               &+ ZNKONST(IP)%SASP * SIN(ASPECT) * SLOPE&
               &+ ZNKONST(IP)%CASP * COS(ASPECT) * SLOPE

          !         POPULATE ARRAYS FOR OTHER CONSTANTS
          DGCCF1(I) = ZNKONST(IP)%CCFA ! CCF
          DGLD1(I) = ZNKONST(IP)%LD ! DBH
          DGBAL1(I) = ZNKONST(IP)%BAL ! BAL
          DGCR1(I) = ZNKONST(IP)%CR ! CROWN
          DGDSQ1(I) = ZNKONST(IP)%DSQ ! DBH^2
          DGDBAL1(I) = ZNKONST(IP)%DBAL1 ! BAL/DBH
          DGDBAL2(I) = ZNKONST(IP)%DBAL2 ! BAL/LOG(DBH+1)
          ATTEN(I) = ZNKONST(IP)%OBSERV ! OBSERV **coeffs.f77**
          SMCON(I) = 0.
          !        SIGMAR(I)  = ZNKONST(IP)%SIGMAR ! SIGMAR - RES.ERR
          !        SIGMAR(I)  = SIGMAR(I) * 0.5
          !          LLTDGOK(I) = .TRUE.
       11 end do
    end if

    !  IF READCORD OR REUSCORD WAS SPECIFIED (LDCOR2 IS TRUE) ADD
    !  LN(COR2) TO THE BAI MODEL CONSTANT TERM (DGCON).  COR2 IS
    !  INITIALIZED TO 1.0 IN BLKDATA.

    do I = 1,MAXSP
       if (LDCOR2.and.COR2(I).gt.0.0) DGCON(I) = DGCON(I)&
            &+ ALOG(COR2(I))
    end do

    return
  end subroutine DGFBC

  subroutine DGFBM(LCONS)
    implicit none

    !  BM: LARGE-TREE EQUATIONS, BLENDED WITH THE SMALL-TREE EQUATIONS
    !  BELOW 10 INCHES FOR THE SPECIES THAT HAVE THEM.

    !  THE SEPERATE EQUATIONS FOR TREES LESS THAN 10" DBH INCLUDED
    !  MANAGED STAND SURVEY (MSS) DATA.  THE EQUATION FOR LODGEPOLE PINE
    !  WAS REFIT TO INCLUDE TERMS FOR CCF AND BA EFFECTS.
    !  ALSO, SPLINE FOR DIAMETER ESTIMATES BASED ON MSS DATA, AND FOR
    !  ESTIMATES BASED ON LARGER TREES NOW COVERS WHOLE RANGE 3-10" DBH,
    !  INSTEAD OF ONLY 7-10".                    DENNIS DONNELLY 11/30/95

    !  Earlier revisions include coefficients for the following species:
    !  WL, DF, GF, LP, ES, AF, & PP.  The coefficients for WP, & OT are
    !  unchanged.  Species #4 is now GF (no WF in model) and Species #6
    !  is now 'blank'.  Species #5 changed from 'WH' to 'MH'.

    !  SEPERATE EQUATIONS ADDED FOR WL,DF,GF,LP,& PP LESS THAN 10" DBH
    !  DIXON 5/16/94. OTHER SPECIES ARE PROXIED

    !  REFIT EQUATIONS FOR TREES LESS THAN 10" DBH LOADED 9/9/99. THESE
    !  WERE REFIT IN 1996 BY RALPH JOHNSON.  DIXON 9/9/99

    logical :: LCONS
    !  VARIABLE DEFINITIONS:

    !     DIAM -- ARRAY LOADED WITH TREE DIAMETERS (PASSED AS AN
    !             ARGUEMENT).
    !     DGLD -- ARRAY CONTAINING COEFFICIENTS FOR THE LOG(DIAMETER)
    !             TERM IN THE DDS MODEL (ONE COEFFICIENT FOR EACH
    !             SPECIES).
    !     DGCR -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
    !             RATIO TERM IN THE DDS MODEL (ONE COEFFICIENT FOR
    !             EACH SPECIES).
    !   DGCRSQ -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
    !             RATIO SQUARED TERM IN THE DDS MODEL (ONE
    !             COEFFICIENT FOR EACH SPECIES).
    !   DGDBAL -- ARRAY CONTAINING COEFFICIENTS FOR THE INTERACTION
    !             BETWEEN BASAL AREA IN LARGER TREES AND LN(DBH) (ONE
    !             COEFFICIENT PER SPECIES).

    ! Variables
    logical :: DEBUG

    integer :: I,I1,I2,I3,INDXH,INDXS,IPCCF,ISIC,ISPC,J,LSI

    integer :: IBSERV(5,3),SMMAPH(92,5),SMMAPS(18)

    real    :: ALBA,ASPDG,ASPTEM,BAL,BARK,BATEM,BRATIO,CCR,CCR2
    real    :: CDBAL,CDSQ,CLBA,CLD,CONSPP,CPCCF,CR,D,DDS,DDSL,DDSS,DF
    real    :: DGFJUN
    real    :: DIAGR,DPP,DSQ,DSQNEW,HOAVH,SI,SMCONS,TEMDG,TEMEL,TSITE
    real    :: XSITE,XWT

    real    :: DGBA(18),DGBAL(18),DGCASP(18),DGCCFA(18)
    real    :: DGCR(18),DGCRSQ(18),DGDBAL(18),DGDS(18)
    real    :: DGEL(18),DGEL2(18),DGFOR(4,18),DGLBA(18)
    real    :: DGLD(18),DGPCCF(18),DGSASP(18),DGSIC(18)
    real    :: DGSITE(18),DGSLOP(18),DGSLSQ(18)
    real    :: OBSERV(18),SMCASP(5),SMCR(5),SMCRSQ(5),SMDBAL(5),SMDS(5)
    real    :: SMEL(5),SMEL2(5),SMFOR(4,5),SMHAB(5,5),SMLBA(5),SMLD(5)
    real    :: SMPCCF(5),SMSASP(5),SMSLOP(5)

    !  DATA STATEMENTS:

    !  SPECIES ORDER:
    !   1=WP,  2=WL,  3=DF,  4=GF,  5=MH,  6=WJ,  7=LP,  8=ES,
    !   9=AF, 10=PP, 11=WB, 12=LM, 13=PY, 14=YC, 15=AS, 16=CW,
    !  17=OS, 18=OH

    !  SPECIES EXPANSION:
    !  WJ USES SO JU (ORIGINALLY FROM UT VARIANT; REALLY PP FROM CR VARIANT)
    !  WB USES SO WB (ORIGINALLY FROM TT VARIANT)
    !  LM USES UT LM
    !  PY USES SO PY (ORIGINALLY FROM WC VARIANT)
    !  YC USES WC YC
    !  AS USES SO AS (ORIGINALLY FROM UT VARIANT)
    !  CW USES SO CW (ORIGINALLY FROM WC VARIANT)
    !  OS USES BM PP BARK COEFFICIENT
    !  OH USES SO OH (ORIGINALLY FROM WC VARIANT)

    ! FOR BM ORIGINAL SPECIES: 1-5, 6-10, AND 17:
    ! COEFFICIENTS FOR TREES SMALLER THAN 10 INCHES.
    ! SMHAB IS DIMENSIONED BY (COEFF INDEX, SPECIES INDEX).
    ! SMMAPH IS DIMENSIONED BY (PA INDEX, SPECIES INDEX).

    data SMLD/&
         &1.20856, 1.12948, 1.52803, 1.00488, 1.04225/

    data SMCR/&
         &1.73596, 1.54957, 0.66664, 2.47118, 2.31970/

    data SMCRSQ/&
         &0.00000, 0.00000, 1.20070,-0.99894,-0.43073/

    data SMDBAL/&
         &-0.00066,-0.00223,-0.00199,-0.00358,-0.00105/

    data SMLBA/&
         &-0.24782,-0.15369,-0.13405,-0.24135,-0.24965/

    data SMPCCF/&
         &0.00000,-0.00003,-0.00167,      0., 0.00000/

    data SMFOR/&
         &-0.00991,-0.00991, 0.24298,-0.00991,&
         &0.12927, 0.12927, 0.42841, 0.31221,&
         &1.31341, 1.53206, 1.78409, 1.73754,&
         &-0.21988,-0.21988, 0.22239,-0.47388,&
         &1.61313, 1.75654, 1.90894, 1.75744/

    data SMMAPS/ 1,1,2,3,4,0,4,3,3,5,&
         &0,0,0,0,0,0,5,0/

    data SMHAB/&
         ! WESTERN LARCH
         &0., -0.131277, -0.328134,  0.      ,  0.      ,&
         ! DOUGLAS-FIR
         &0., -0.336855, -1.004248, -0.195972, -0.092403,&
         ! GRAND FIR
         &0., -0.137259,  0.282528,  0.      ,  0.      ,&
         ! LODGEPOLE PINE
         &0.,  0.119324,  0.425094,  0.      ,  0.      ,&
         ! PONDEROSA PINE
         &0.,  0.482619,  0.173487, -0.087731,  0.      /

    data SMMAPH /&
         ! WESTERN LARCH
         &27*0, 2, 0, 2, 31*0, 2, 7*0, 2*1, 5*0, 13*1, 3*0,&
         ! DOUGLAS-FIR
         &1, 1, 8*3, 2*4, 3, 7*0, 2*1, 0, 2*1, 0, 1, 2, 0, 2, 16*3,&
         &2*0, 8*3, 2*0, 3, 0, 3, 2, 24*0, 2*3, 0, 3*3,&
         ! GRAND FIR
         &85*0, 2, 2*1, 2, 3*0,&
         ! LODGEPOLE PINE
         &0, 0, 8*2, 2*0, 2, 17*0, 16*2, 6*0, 2*2, 15*0, 2*1,&
         &5*0, 5*1, 0, 3*1, 4*0, 3*2,&
         ! PONDEROSA PINE
         &30*0, 16*3, 2*0, 8*3, 2*0, 3, 0, 3, 6*0, 2*1, 2*2, 5*1,&
         &4*0, 2, 1, 2*0, 2, 4*0, 3*3/

    data SMDS/&
         &-0.000571,-0.000023,-0.000951,-0.000643,-0.000157/

    data SMCASP/&
         &-0.06358,-0.11174,-0.18548,-0.46361,-0.08695/

    data SMSASP/&
         &0.12754, 0.05022,-0.11202, 0.35696,-0.13976/

    data SMSLOP/&
         &-0.41366,-0.36252,-0.16110, 0.45733,-0.24248/

    data SMEL/&
         &0.00000,-0.00823,-0.09472, 0.00912,-0.07547/

    data SMEL2/&
         &0.00000, 0.00000, 0.00092, 0.00000, 0.00087/

    ! FOR BM ORIGINAL SPECIES: 1-5, 6-10, AND 17:
    !   COEFFICIENTS FOR TREES LARGER THAN 10 INCHES;
    ! FOR ALL OTHER SPECIES, USE FOR ALL TREES.

    data DGLD/&
         &0.77889,  0.41802,  0.57990,  1.01031,    0.8978,      0.0,&
         &0.70429,  1.12805,  0.83642,  0.44675,  0.213947, 0.213947,&
         &0.879338, 0.816880,      0.0, 0.889596,   0.44675, 0.889596/

    data DGCR/&
         &3.36606,  2.15440,  2.13121,  2.56530,    1.2840,      0.0,&
         &3.00236,  3.22770,  1.60755,  1.70901,  1.523464, 1.523464,&
         &1.970052, 2.471226,      0.0, 1.732535,   1.70901, 1.732535/

    data DGCRSQ/&
         &-1.80146, -1.03088, -0.40173, -0.91846,       0.0,      0.0,&
         &-1.24947, -1.13951,      0.0,      0.0,       0.0,      0.0,&
         &0.0,      0.0,      0.0,      0.0,       0.0,      0.0/

    data DGBA/&
         &0.0,      0.0,      0.0,      0.0,       0.0,      0.0,&
         &0.0,      0.0,      0.0,      0.0,       0.0,      0.0,&
         &-0.000173,-0.000147,      0.0,-0.000981,       0.0,-0.000981/

    data DGBAL/&
         &0.0,      0.0,      0.0,      0.0,       0.0,       0.0,&
         &0.0,      0.0,      0.0,      0.0, -0.358634, -0.358634,&
         &0.0,      0.0,      0.0,      0.0,       0.0,       0.0/

    data DGCCFA/&
         &0.0,      0.0,      0.0,      0.0,       0.0,       0.0,&
         &0.0,      0.0,      0.0,      0.0, -0.199592, -0.199592,&
         &0.0,      0.0,      0.0,      0.0,       0.0,       0.0/

    data DGSIC/&
         &0.0,      0.0,      0.0,      0.0,       0.0,       0.0,&
         &0.0,      0.0,      0.0,      0.0,  0.001766,  0.001766,&
         &0.0,      0.0,      0.0,      0.0,       0.0,       0.0/

    data DGSITE/&
         &0.0,  0.47469,  0.76217,  0.58666,      0.0,      0.0,&
         &0.34450,  0.34406,  0.51754,  0.73067,      0.0,      0.0,&
         &0.2528530, 0.244694,      0.0, 0.227307,  0.73067, 0.227307/

    data DGDBAL/&
         &-0.00897, -0.00801, -0.00886, -0.00557,-0.006611,      0.0,&
         &-0.00251, -0.00156, -0.00091, -0.01184,      0.0,      0.0,&
         &-0.004215,-0.005950,      0.0,-0.001265, -0.01184,-0.001265/

    data DGLBA/&
         &0.0,      0.0, -0.06574, -0.15658,      0.0,      0.0,&
         &-0.17037,      0.0, -0.18969, -0.10675,      0.0,      0.0,&
         &0.0,      0.0,      0.0,      0.0, -0.10675,      0.0/

    data DGPCCF/&
         &0.0,      0.0, -0.00034,      0.0,-0.001074,      0.0,&
         &-0.00032, -0.00014, -0.00038, -0.00057,      0.0,      0.0,&
         &0.0,      0.0,      0.0,      0.0, -0.00057,      0.0/

    data  OBSERV/&
         &256., 1209., 3478., 2963., 1000.,   0.0,&
         &1117.,  596.,  599., 6577.,   0.0,   0.0,&
         &475.,    0.,   0.0,  220., 6577.,  220./

    !  IBSERV IS THE NUMBER OF OBSERVATIONS IN THE DIAMETER GROWTH
    !  MODEL BY SPECIES AND SITE CLASS; USED FOR WJ, WB, LM, AND AS

    data  ((IBSERV(I,J),I = 1,5),J = 1,3)/&
         &5*1000,& !WJ
         &27,  70, 123, 101,  33,& !WB & LM
         &184, 429, 356, 162,  74/ !AS

    !  DGFOR CONTAINS LOCATION CLASS CONSTANTS FOR EACH SPECIES.

    data DGFOR/&
         &-0.23185, -0.23185, -0.23185, -0.23185,&
         &-0.56061, -0.56061, -0.56061, -0.56061,&
         &-1.69223, -1.69223, -1.69223, -1.78978,&
         &-1.16884, -1.16884, -1.16884, -1.16884,&
         &-1.6803,  -1.6803,  -1.6803,  -1.6803,&
         &0.0,      0.0,      0.0,      0.0,& !WJ
         &1.59448,  1.59448,  1.59448,  1.49879,&
         &-2.38952, -2.38952, -2.38952, -2.38952,&
         &-0.48027, -0.48027, -0.48027, -0.48027,&
         &0.05217, -0.04456,  0.05217,  0.11197,&
         &1.911884, 1.911884, 1.911884, 1.911884,& !WB USE TT BRIDGER NF
         &1.911884, 1.911884, 1.911884, 1.911884,& !LM
         &-1.310067,-1.310067,-1.310067,-1.310067,& !PY
         &-1.178041,-1.178041,-1.178041,-1.178041,& !YC
         &0.,       0.,       0.,       0.,& !AS
         &-0.107648,-0.107648,-0.107648,-0.107648,& !CW
         &0.05217, -0.04456,  0.05217,  0.11197,&
         &-0.107648,-0.107648,-0.107648,-0.107648/ !OH

    !  DGDS CONTAINS COEFFICIENTS FOR THE DIAMETER SQUARED TERMS
    !  IN THE DIAMETER INCREMENT MODELS; ARRAYED BY FOREST BY
    !  SPECIES.

    data DGDS/&
         &-0.000091,       0.0,-0.000380,-0.000541, -0.000484,       0.0,&
         &0.0, -0.000289, 0.000088,-0.000214,-0.0006538,-0.0006538,&
         &-0.0001323,-0.0002536,      0.0,      0.0, -0.000214,       0.0/

    !  DGEL CONTAINS THE COEFFICIENTS FOR THE ELEVATION TERM IN THE
    !  DIAMETER GROWTH EQUATION.  DGEL2 CONTAINS THE COEFFICIENTS FOR
    !  THE ELEVATION SQUARED TERM IN THE DIAMETER GROWTH EQUATION.
    !  DGSASP CONTAINS THE COEFFICIENTS FOR THE SIN(ASPECT)*SLOPE
    !  TERM IN THE DIAMETER GROWTH EQUATION.  DGCASP CONTAINS THE
    !  COEFFICIENTS FOR THE COS(ASPECT)*SLOPE TERM IN THE DIAMETER
    !  GROWTH EQUATION.  DGSLOP CONTAINS THE COEFFICIENTS FOR THE
    !  SLOPE TERM IN THE DIAMETER GROWTH EQUATION.  DGSLSQ CONTAINS
    !  COEFFICIENTS FOR THE (SLOPE)**2 TERM IN THE DIAMETER GROWTH
    !  MODELS ALL OF THESE ARRAYS ARE SUBSCRIPTED BY SPECIES.

    data DGCASP/&
         &0.12915,      0.0, -0.15167, -0.16504,    0.1794,       0.0,&
         &-0.37870, -0.11989, -0.44759, -0.02280, -0.609774, -0.609774,&
         &0.0,-0.023186,      0.0, 0.085958,  -0.02280,  0.085958/

    data DGSASP/&
         &-0.19278,      0.0, -0.11862, -0.19627,    0.1336,       0.0,&
         &0.09760,  0.35781, -0.27729, -0.12480, -0.017520, -0.017520,&
         &0.0, 0.679903,      0.0, -0.86398,  -0.12480,  -0.86398/

    data DGSLOP/&
         &0.77922,      0.0, -0.28123, -0.67496,    0.0763,       0.0,&
         &0.03990,      0.0,  0.35402, -0.16402, -2.057060, -2.057060,&
         &0.0,      0.0,      0.0,      0.0,  -0.16402,       0.0/

    data DGSLSQ/&
         &-0.93813,      0.0,      0.0,  0.76704,       0.0,       0.0,&
         &0.0,      0.0,      0.0,      0.0,   2.11326,  2.113263,&
         &0.0,      0.0,      0.0,      0.0,       0.0,       0.0/

    data DGEL/&
         &0.00279,      0.0,  0.00371, -0.00633,    0.0852,       0.0,&
         &-0.06908,      0.0, -0.01423, -0.05796,       0.0,       0.0,&
         &0.0,      0.0,      0.0,-0.075986,  -0.05796, -0.075986/

    data DGEL2/&
         &-0.00001,      0.0,      0.0,      0.0,  -0.00094,       0.0,&
         &0.00062,      0.0,      0.0,  0.00060,       0.0,       0.0,&
         &0.0,      0.0,      0.0, 0.001193,   0.00060,  0.001193/

    !  CHECK FOR DEBUG.

    if (LCONS) go to 1000

    call DBCHK (DEBUG,'DGF',3,ICYC)
    if (DEBUG) write(JOSTND,3)ICYC
    3 format(' ENTERING SUBROUTINE DGF  CYCLE =',I5)

    !  DEBUG OUTPUT: MODEL COEFFICIENTS.

    if (DEBUG) write(JOSTND,*) ' IN DGF,HTCON=',HTCON
    if (DEBUG)&
         &write(JOSTND,*) ' DGCON= ', DGCON
    !     IF(DEBUG)
    !    & WRITE(JOSTND,9000) DGCON,DGDSQ,DGLD,DGCR,DGCRSQ,DGPCCF,DGDBAL
    !9000 FORMAT(/,' DGCON',/12(1X,F9.5),/7(1X,F9.5),/,' DGDSQ',/12(1X,F9.5)
    !    & ,/7(1X,F9.5),/,' DGLD',/12(1X,F9.5),/7(1X,F9.5),/,' DGCR',
    !    & /12(1X,F9.5),/7(1X,F9.5),/,' DGCRSQ',/12(1X,F9.5),/7(1X,F9.5),
    !    & /,' DGPCCF',/12(1X,F9.5),/7(1X,F9.5),/,' DGDBAL',/12(1X,F9.5),
    !    & /7(1X,F9.5))

    !  BEGIN SPECIES LOOP.  ASSIGN VARIABLES WHICH ARE SPECIES
    !  DEPENDENT.

    ALBA = 0.0
    if (BA.gt.0.0)ALBA = ALOG(BA)
    do ISPC = 1,MAXSP
       I1 = ISCT(ISPC,1)
       if (I1.eq.0) go to 20
       I2 = ISCT(ISPC,2)
       CONSPP = DGCON(ISPC) + COR(ISPC) + 0.01*DGCCF(ISPC)*RELDEN
       SMCONS = SMCON(ISPC) + COR(ISPC)
       INDXS = SMMAPS(ISPC)
       CLD = DGLD(ISPC)
       CLBA = DGLBA(ISPC)
       CCR = DGCR(ISPC)
       CCR2 = DGCRSQ(ISPC)
       CDSQ = DGDSQ(ISPC)
       CDBAL = DGDBAL(ISPC)
       CPCCF = DGPCCF(ISPC)
       SI = SITEAR(ISPC)
       if (DEBUG)write(JOSTND,9002)ISPC,DGCON(ISPC),COR(ISPC),&
            &DGCCF(ISPC),RELDEN,CONSPP,SMCON(ISPC),SMCONS,INDXS
       9002 format(' IN DGF 9002 ISPC,DGCON,COR,DGCCF,RELDEN,CONSPP,',&
            &'SMCON,SMCONS,INDXS=',I5,7F10.5,I5)

       !  BEGIN TREE LOOP WITHIN SPECIES ISPC.

       do I3 = I1,I2
          I = IND1(I3)
          D = DIAM(I)
          if (D.le.0.0) goto 10

          select case (ISPC)

             !  ORIGINAL BM SPECIES

          case(1:5,7:10,17)
             CR = ICR(I)*0.01
             BAL = (1.0 - (PCT(I)/100.)) * BA
             IPCCF = ITRE(I)
             HOAVH = HT(I)/AVH
             if (HOAVH .gt.1.5)HOAVH = 1.5
             if (IPCCF .lt.100) HOAVH = 1.0
             DDSL = CONSPP + CLD*ALOG(D)&
                  &+ CLBA*ALBA + CR*(CCR + CR*CCR2)&
                  &+ CDSQ*D*D  + CDBAL*BAL/(ALOG(D+1.0))&
                  &+ CPCCF*PCCF(IPCCF)

             !  EXTRA VARIABLES FOR INDIVIDUAL SPECIES

             if (ISPC .eq.1) DDSL = DDSL+ 0.00121 * BAL&
                  &+ .00001 * .01 * RMAI * RELDEN&
                  &- .0000016 * RELDEN
             if (ISPC .eq.2) DDSL = DDSL- 0.000695 * BA

             !  DEBUG OUTPUT: DDS FOR TREES >= 10-INCHES

             if (DEBUG) write(JOSTND,9010) ISPC, IPCCF,&
                  &CONSPP, CLD, CDSQ, D, CLBA, ALBA, CCR, CCR2, CR,&
                  &CDBAL, BAL, CPCCF, PCCF(IPCCF), RELDEN
             9010 format(1H0, 'DGF-9010F', 2I5, 14F8.4)

             !  EQUATION FOR TREES < 10" DBH.

             DDSS = 0.
             if (D.lt.10.) then
                DDSS = SMCONS + SMLD(INDXS)*ALOG(D)&
                     &+ SMLBA(INDXS)*ALBA + CR*(SMCR(INDXS) + CR*SMCRSQ(INDXS))&
                     &+ SMDS(INDXS)*D*D  + SMDBAL(INDXS)*BAL/(ALOG(D+1.0))&
                     &+ SMPCCF(INDXS)*PCCF(IPCCF)

                !  DEBUG OUTPUT: DDS FOR TREES < 10-INCHES

                if (DEBUG) write(JOSTND,9020) INDXS, SMCONS,&
                     &SMLD(INDXS), SMDS(INDXS), D,  SMLBA(INDXS), ALBA,&
                     &SMCR(INDXS), SMCRSQ(INDXS), CR,  SMDBAL(INDXS), BAL,&
                     &SMPCCF(INDXS), PCCF(IPCCF), IPCCF
                9020 format(' DGF-9020F', I5, 13F8.4, I5)

                !  EQN WAS FIT TO 5 YEAR DIAMETER GROWTHS, ADJUST TO 10 YEAR ESTIMATE.

                DSQ = EXP(DDSS)
                TEMDG = (SQRT(D*D+DSQ)-D)*2.
                DSQNEW = (TEMDG+D)**2 - D*D
                if (DSQNEW .le.0.) then
                   DDSS = 0.
                else
                   DDSS = ALOG(DSQNEW)
                end if
             end if

             !   SPLINE L & S ESTIMATES IF D AT CYCLE START IS IN RANGE 3-10 IN.
             !   TREES 3-10 IN. SIMULATED BASED ON MSS DATA IN ADDITION TO
             !   ORIGINAL SIMULATION.  COMBINE ESTIMATES OVER DIAM. RANGE 3-10 IN
             !   SO MSS DATA INFLUENCE PREDOMINATES FOR SMALLER TREES IN 3-10.
             !   GRADUALLY INCREASE INFLUENCE OF PREVIOUS ESTIMATES AS TREE SIZE
             !   APPROACHES 10 IN. WITH ONLY PREVIOUS ESTIMATES FROM LARGER TREES
             !   IN FORCE FOR DIAMETERS GREATER THAN 10 IN.   DMD   11/30/95

             XWT = 1.
             if (D.ge.3)XWT = (10.-D)/7.
             if (XWT.lt.0.)XWT = 0.
             DDS = XWT*DDSS + (1.-XWT)*DDSL

             if (DEBUG) write(JOSTND,9025) D, XWT, DDSS, DDSL, DDS
             9025 format(' DGF-9025F--D, XWT, DDSS, DDSL, DDS:', 5F10.5)
             if (DEBUG) write(JOSTND,9030)&
                  &I,ISPC,CONSPP,D,BA,CR,BAL,PCCF(IPCCF),RELDEN,HT(I),AVH,SMCONS
             9030 format(' DGF-9030F',2I5,10F11.4)

             !  SPECIES FROM THE UT & TT VARIANTS

          case(6,11,12,15)
             BARK = BRATIO(ISPC,D,HT(I))
             if (ISPC.eq.6) then
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
                WK2(I) = DDS
                if (DEBUG)write(JOSTND,*)' ISPC,D,DIAGR,DPP,BA,BARK,CONSPP= '
                if (DEBUG)write(JOSTND,*)ISPC,D,DIAGR,DPP,BA,BARK,CONSPP
             else if (ISPC .eq.15) then
                CR = ICR(I)
                call DGFASP(D,ASPDG,CR,BARK,SI,DEBUG)
                DDS = ASPDG + ALOG(COR2(ISPC)) + COR(ISPC)
                WK2(I) = DDS
                if (DEBUG)write(JOSTND,*)' D,ASPDG,CR,BARK,SI= ',D,ASPDG,CR,&
                     &BARK,SI
                if (DEBUG)write(JOSTND,*)' COR2, COR= ',COR2(ISPC),COR(ISPC)
             else
                CR = ICR(I) * 0.01
                BAL = (1.0 - (PCT(I)/100.)) * BA/100.
                DDS = CONSPP + DGLD(ISPC)*ALOG(D) + DGBAL(ISPC)*BAL +&
                     &DGCR(ISPC)*CR + CR*CR*DGCRSQ(ISPC) + DGDSQ(ISPC)*D*D +&
                     &DGPCCF(ISPC)*PCCF(ITRE(I))
                WK2(I) = DDS
             end if

             !  SPECIES FROM THE WC VARIANT

          case(13,14,16,18)
             CR = ICR(I)*0.01
             BAL = (1.0 - (PCT(I)/100.)) * BA
             IPCCF = ITRE(I)
             DDS = CONSPP + DGLD(ISPC)*ALOG(D)&
                  &+ CR*(DGCR(ISPC) + CR*DGCRSQ(ISPC))&
                  &+ DGDSQ(ISPC)*D*D  + DGDBAL(ISPC)*BAL/(ALOG(D+1.0))
             DDS = DDS + DGPCCF(ISPC)*PCCF(IPCCF)&
                  &+ DGLBA(ISPC)*ALOG(BA)&
                  &+ DGBAL(ISPC)*BAL + DGBA(ISPC)*BA
             if (DEBUG) write(JOSTND,8000)&
                  &I,ISPC,CONSPP,D,BA,CR,BAL,PCCF(IPCCF),RELDEN,HT(I),AVH
             8000 format(1H0,'IN DGF 8000F',2I5,9F11.4)

          end select
          if (DDS.lt.-9.21) DDS = -9.21
          WK2(I) = DDS

          !  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.

          if (DEBUG) then
             write(JOSTND,9040) I,ISPC,DDS
             9040 format(' DGF-9040F, I=',I4,',  ISPC=',I3,',  DDS=LN(DDS)=',F7.4)
          end if
          10 continue
       end do

       !  END OF SPECIES LOOP.

       20 continue
    end do
    if (DEBUG)write(JOSTND,100)ICYC
    100 format(' LEAVING SUBROUTINE DGF  CYCLE =',I5)
    return

    1000 continue

    !  ENTRY POINT FOR LOADING COEFFICIENTS OF THE DIAMETER INCREMENT
    !  MODEL THAT ARE SITE SPECIFIC AND NEED ONLY BE RESOLVED ONCE.
    !  IDTYPE IS A HABITAT TYPE INDEX THAT IS COMPUTED IN **RCON**.
    !  ASPECT IS STAND ASPECT.

    !  CHECK FOR DEBUG.

    call DBCHK (DEBUG,'DGF',3,ICYC)

    !  NOTE: FOR UT SPECIES 6(WJ), 12(LM), 15(AS) AND TT 11(WB) 0.7854 IS
    !        45 DEGREES IN RADIANS. THIS CORRECTION WAS
    !        PROBABLY DUE TO A PROBLEM IN THE WAY ASPECT WAS RECORDED
    !        IN THE DATA SET USED TO FIT THE MODEL.  GED 12-20-94.
    !  PUT SITE INTO ONE OF 5 CLASSES FOR UT SPECIES 6(WJ), 11(WB) AND 15(AS)

    XSITE = SITEAR(ISISP)
    LSI = INT(XSITE/10.)
    if (LSI.lt.2)ISIC = 1
    if (LSI.ge.5)ISIC = 5
    if (LSI.ge.3.and. LSI.lt.4)ISIC = 3
    if (LSI.ge.4.and. LSI.lt.5)ISIC = 4
    if (LSI.ge.2.and. LSI.lt.3)ISIC = 2

    !  ENTER LOOP TO LOAD SPECIES DEPENDENT VECTORS.

    do ISPC = 1,MAXSP
       DGCCF(ISPC) = DGCCFA(ISPC)

       !  COMPUTE CONSTANT TERM: LARGE TREES >= 10 INCHES.

       if (ISPC.eq.6.or. ISPC.eq.11.or. ISPC.eq.12.or. ISPC.eq.15) then
          ASPTEM = ASPECT-0.7854
       else
          ASPTEM = ASPECT
       end if
       TSITE = SITEAR(ISPC)
       if (ISPC.eq.7) then
          TSITE = -43.78 + 2.16 * TSITE
          if (TSITE.le.1.) TSITE = 1.
       end if
       if (ISPC.eq.5) TSITE = TSITE * 3.28
       TEMEL = ELEV
       if (ISPC.eq.16.or. ISPC.eq.18) then
          if (TEMEL .gt.30.)TEMEL = 30.
       end if
       DGCON(ISPC) = DGSIC(ISPC)*XSITE&
            &+ DGFOR(IFOR,ISPC)&
            &+ DGEL(ISPC) * TEMEL&
            &+ DGEL2(ISPC) * TEMEL * TEMEL&
            &+(DGSASP(ISPC) * SIN(ASPTEM)&
            &+ DGCASP(ISPC) * COS(ASPTEM)&
            &+ DGSLOP(ISPC)) * SLOPE&
            &+ DGSLSQ(ISPC) * SLOPE * SLOPE&
            &+ DGSITE(ISPC)*ALOG(TSITE)
       if (DEBUG) write(JOSTND,9050) IFOR, ISPC,    DGCON(ISPC),&
            &DGFOR(IFOR,ISPC), DGEL(ISPC), DGEL2(ISPC), TEMEL,&
            &DGSASP(ISPC), DGCASP(ISPC), ASPTEM,&
            &DGSLOP(ISPC), DGSLSQ(ISPC), SLOPE,&
            &DGSITE(ISPC), SITEAR(ISPC), TSITE
       9050 format(' DGF-9050F', 2I5, 14F8.4)

       !  COMPUTE CONSTANT TERM:  LARGE TREES < 10 IN.

       select case (ISPC)
       case (1:5,7:10,17)
          INDXS = SMMAPS(ISPC)
          INDXH = SMMAPH(ICL5,INDXS) + 1
          SMCON(ISPC) = SMHAB(INDXH,INDXS)&
               &+ SMFOR(IFOR,INDXS)&
               &+ SMEL(INDXS) * ELEV&
               &+ SMEL2(INDXS) * ELEV * ELEV&
               &+(SMSASP(INDXS) * SIN(ASPTEM)&
               &+ SMCASP(INDXS) * COS(ASPTEM)&
               &+ SMSLOP(INDXS)) * SLOPE
       case (6,11:16,18)
          SMCON(ISPC) = 0.0
       end select

       select case (ISPC)
       case (6)
          ATTEN(ISPC) = IBSERV(ISIC,1)
       case (11,12)
          ATTEN(ISPC) = IBSERV(ISIC,2)
       case (15)
          ATTEN(ISPC) = IBSERV(ISIC,3)
       case default
          ATTEN(ISPC) = OBSERV(ISPC)
       end select

       DGDSQ(ISPC) = DGDS(ISPC)
       if (DEBUG) write(JOSTND,9060) IFOR, ISPC, INDXS, ICL5, INDXH,&
            &SMCON(ISPC), SMHAB(INDXH,INDXS), SMFOR(IFOR,INDXS),&
            &SMEL(INDXS), SMEL2(INDXS), ELEV,&
            &SMSASP(INDXS), SMCASP(INDXS), ASPTEM,&
            &SMSLOP(INDXS), SLOPE,&
            &DGDSQ(ISPC), ATTEN(ISPC)
       9060 format(' DGF-9060F', 5I5, /' ', 12F8.4,F8.0)

       !  IF READCORD OR REUSCORD WAS SPECIFIED (LDCOR2 IS TRUE) ADD
       !  LN(COR2) TO THE BAI MODEL CONSTANT TERM (DGCON).  COR2 IS
       !  INITIALIZED TO 1.0 IN BLKDATA.

       if (LDCOR2.and.COR2(ISPC).gt.0.0) then
          DGCON(ISPC) = DGCON(ISPC)+ALOG(COR2(ISPC))

          select case (ISPC)
          case (1:5,7:10,17)
             SMCON(ISPC) = SMCON(ISPC)+ALOG(COR2(ISPC))
          end select

       end if
       30 continue
    end do
    return
  end subroutine DGFBM

end subroutine DGF
