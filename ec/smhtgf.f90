!=================!
!   ec : smhtgf   !
!=================!
! EC $Id$
subroutine SMHTGF (MODE,ICYC,ISPC,H,DTIME,HHT,JOSTND,DEBUG)
  implicit none

  !  THIS ROUTINE CALCULATES THE HEIGHT GROWTH OF SMALL TREES (D<3.0 IN).
  !  WHEN CALLED FROM ESSUBH (MODE=0), DTIME= TOTAL TREE AGE.
  !  IF CALLED FROM REGENT (MODE=1), DTIME= FINT AND THE
  !  RETURN VARIABLE (HHT) IS THE HEIGHT INCREMENT.
  !  CALLED FROM **ESSUBH
  !  CALLED FROM **REGENT

  !  INPUT VARIABLES
  !  ISPC   - SPECIES SEQUENCE NUMBER
  !  H      - BEGINING HEIGHT
  !  SI     - SITE INDEX FOR SPECIES
  !  MODE   - = 0 IF CALL IS FROM ESSUBH, = 1 IF CALL IS FROM REGENT
  !  DTIME  - DTIME=TOTAL SEEDLING AGE (MODE= 0), DTIME=FINT (MODE= 1)

  !  RETURN VARIABLES
  !  HHT    - HEIGHT GROWTH OVER TIME INCREMENT DTIME (REGENT)
  !  HHT    - HEIGHT 5 YEARS INTO CYCLE, OR END OF CYCLE FINT<5 (ESSUBH)

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'

  ! Variables
  logical :: DEBUG
  integer :: ISPC,ICYC,JOSTND,MODE
  real    :: AGEPDT,C1,C2,C3,C4,EFFAGE,DTIME,H,HHT,SI,HHT1,HHT2

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

  !  IF NO GROWTH IS TO BE CALCULATED ON ESTAB TREES, I.E. PLANTING
  !  IS MORE THAN 5 YEARS INTO THE SIMULATION, THEN RETURN AND
  !  LEAVE THE SMALL TREE GROWTH, DURING THE PLANTING CYCLE, TO
  !  THE REGENT ROUTINE.

  if (DTIME .le.0.0) go to 120

  SI = SITEAR(ISPC)

  select case (ISPC)

     !  HEIGHT OF TALLEST SUBSEQUENT SPECIES 1 (WP) **********

  case(1)

     !       FOR NON-LINEAR HEIGHT GROWTH MODEL SOLVE FIRST FOR EFFECTIVE
     !       AGE BASED ON INPUT HEIGHT.

     C1 = 0.375045
     C2 = 0.92503
     C3 = -0.020796
     C4 = 2.48811

     if (MODE .eq.0) then
        EFFAGE = 0.
        AGEPDT = EFFAGE + DTIME
     else if (MODE .eq.1) then
        EFFAGE = ALOG( (1.0 - (C1/SI * H)**(1/C4))/C2 ) / C3
        AGEPDT = EFFAGE + DTIME
     end if

     HHT1 = (SI/C1)*(1.0-C2*EXP(C3*EFFAGE))**C4
     HHT2 = (SI/C1)*(1.0-C2*EXP(C3*AGEPDT))**C4
     HHT = HHT2 - HHT1

     !  HEIGHT OF TALLEST SUBS. SPECIES 2 (WL) **********

  case(2,17)
     HHT = ((-3.97245 + 0.50995*SI)/(28.11668-0.05661*SI))*DTIME

     !  HT OF TALLEST SUBS. SPECIES 3 (DF) **********

  case(3)
     HHT = ((2.0 + 0.420*SI)/(28.5 - 0.05*SI))*DTIME

     !  HT OF TALLEST SUBS. SPECIES 4 (SF) **********

  case(4)
     HHT = ((-0.6667 + 0.4333*SI)/(28.5 - 0.05*SI))*DTIME

     !  HT OF TALLEST SUBS. SPECIES 5 (RC) **********

  case(5)
     C1 = 0.752842
     C2 = 1.0
     C3 = -0.0174
     C4 = 1.4711
     if (MODE .eq.0) then
        EFFAGE = 0.
        AGEPDT = EFFAGE + DTIME
     else if (MODE .eq.1) then
        EFFAGE = (1.0 - (C1/SI * H)**(1/C4))/C2
        if (EFFAGE.gt.0) then
           EFFAGE = ALOG( EFFAGE ) / C3
        else
           EFFAGE = 100.
        end if
        AGEPDT = EFFAGE + DTIME
     end if

     HHT1 = (SI/C1)*(1.0-C2*EXP(C3*EFFAGE))**C4
     HHT2 = (SI/C1)*(1.0-C2*EXP(C3*AGEPDT))**C4
     HHT = HHT2 - HHT1

     !  HT OF TALLEST SUBS. SPECIES  6 (GF) **********
     !  HT OF TALLEST SUBS. SPECIES 16 (WF) **********

  case(6,16)
     HHT = ((-1.0470 + 0.4220*SI)/(28.7739 - 0.0597*SI))*DTIME

     !  HT OF TALLEST SUBS. SPECIES 7 (LP) **********

  case(7)
     HHT = (0.3277 + 0.01296*SI)*DTIME

     !  HT OF TALLEST SUBS. SPECIES 8 (ES) **********

  case(8)
     HHT = ((-8.0 + 0.35*SI)/(53.72545 - 0.274509*SI))*DTIME

     !  HT OF TALLEST SUBS. SPECIES 9 (AF) **********

  case(9)
     HHT = ((6.0 + 0.14*SI)/(33.882 - 0.06588*SI))*DTIME

     !  HEIGHT OF TALLEST SUBS. SPECIES 10 (PP) **********

  case(10)
     HHT = ((-1.0 + 0.32857*SI)/(28.0 - 0.042857*SI))*DTIME

     !  HEIGHT OF TALLEST SUBS. SPECIES 11 (WH) **********

  case(11)
     HHT = ((-5.74874 + 0.54576*SI)/(26.15767 - 0.03596*SI))*DTIME

     !  HEIGHT OF TALLEST SUBS. SPECIES 12 (MH) **********
     !  HEIGHT OF TALLEST SUBS. SPECIES 31 (OS) **********
     !  IN METERS CONVERTED TO FEET

  case(12,31)
     HHT = ((0.965758 + 0.082969*SI)/(55.249612 - 1.288852*SI))*DTIME&
          &*3.280833

     !  HEIGHT OF TALLEST SUBS. SPECIES 13 (PY) **********
     !  HEIGHT OF TALLEST SUBS. SPECIES 14 (WB) **********
     !  HEIGHT OF TALLEST SUBS. SPECIES 18 (YC) **********
     !  HEIGHT OF TALLEST SUBS. SPECIES 19 (WJ) **********
     !  HEIGHT OF TALLEST SUBS. SPECIES 20 (BM) **********
     !  HEIGHT OF TALLEST SUBS. SPECIES 21 (VN) **********
     !  HEIGHT OF TALLEST SUBS. SPECIES 23 (PB) **********
     !  HEIGHT OF TALLEST SUBS. SPECIES 24 (GC) **********
     !  HEIGHT OF TALLEST SUBS. SPECIES 25 (DG) **********
     !  HEIGHT OF TALLEST SUBS. SPECIES 26 (AS) **********
     !  HEIGHT OF TALLEST SUBS. SPECIES 27 (CW) **********
     !  HEIGHT OF TALLEST SUBS. SPECIES 29 (PL) **********
     !  HEIGHT OF TALLEST SUBS. SPECIES 30 (WI) **********
     !  HEIGHT OF TALLEST SUBS. SPECIES 32 (OH) **********

  case(13,14,18:21,23:27,29,30,32)
     HHT = ((1.47043 + 0.23317*SI)/(31.56252 - 0.05586*SI))*DTIME

     !  HEIGHT OF TALLEST SUBS. SPECIES 15 (NF) **********

  case(15)
     HHT = ((11.26677 + 0.12027*SI)/(27.93806 - 0.02873*SI))*DTIME

     !  HEIGHT OF TALLEST SUBS. SPECIES 22 (RA) **********

  case(22)
     HHT = (-0.007025 + 0.056794*SI)*DTIME

     !  HEIGHT OF TALLEST SUBS. SPECIES 28 (WO) **********

  case(28)
     HHT = (((-37.60812*ALOG(1.0-(SI/114.24569)**0.44444))*0.01)-0.1)&
          &*DTIME

  end select

  120 continue
  if (DEBUG)&
       &write(JOSTND,*)' LEAVING SMHTGF, ICYC,ISPC,HHT,DTIME,SI= ',&
       &ICYC,ISPC,HHT,DTIME,SI

  return
end
