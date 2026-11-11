!=========================!
!   Compute bark ratios   !
!=========================!
! EC $Id$
function BRATIO(IS,D,H)
  implicit none

  ! FUNCTION TO COMPUTE BARK RATIOS AS A FUNCTION OF DIAMETER AND SPECIES.
  ! REPLACES ARRAY BKRAT IN BLKDAT.

  ! Common blocks
  include 'PRGPRM.f90'

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
  real    :: BARK1(MAXSP),BARK2(MAXSP),H,D,BRATIO,DIB
  integer :: IS
  real    :: RDANUW

  data BARK1/&
       &0.964,     0.851,     0.844,     0.903,     0.950,&
       &0.903,     0.963,     0.956,     0.903,     0.889,&
       &0.93371,     0.934,   0.93329,   0.93329,  0.904973,&
       &0.903,       0.9,  0.837291,   0.94967,    0.0836,&
       &0.0836,  0.075256,    0.0836,   0.15565,  0.075256,&
       &0.075256,  0.075256,    0.8558,  0.075256,  0.075256,&
       &0.934,  0.075256/

  data BARK2/&
       &1.,        1.,        1.,        1.,        1.,&
       &1.,        1.,        1.,        1.,        1.,&
       &1.,        1.,        1.,        1.,        1.,&
       &1.,        1.,        1.,        1.,   0.94782,&
       &0.94782,   0.94967,   0.94782,   0.90182,   0.94967,&
       &0.94967,   0.94967,    1.0213,   0.94967,   0.94967,&
       &1.,   0.94967/

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = H

  select case (IS)

     !  ORIGINAL EC VARIANT SPECIES (WP,WL,DF,SF,RC,GF,LP,ES,AF,PP,OS)
     !  THOSE SPECIES USING EC COEFFICIENTS (WF,VN)

  case(1:10,12,16,31)
     BRATIO = BARK1(IS)

     !  THOSE SPECIES USING WC COEFFICIENTS
     !  WH, MH, PY, WB, NF, LL, YC, WJ

  case(11,13:15,17:19)
     BRATIO = BARK1(IS)

     !  THOSE SPECIES USING WC COEFFICIENTS; DIB = a + b*DOB
     !  BM, VN, RA, PB, GC, DG, AS, CW, PL, WI, OH

  case(20:27,29:30,32)
     if (D .gt.0) then
        DIB = BARK1(IS) + BARK2(IS)*D
        BRATIO = DIB/D
     else
        BRATIO = 0.99
     end if
     if (BRATIO .gt.0.99) BRATIO = 0.99
     if (BRATIO .lt.0.80) BRATIO = 0.80

     !  THOSE SPECIES USING WC COEFFICIENTS; DIB = a * DOB ** b
     !  WO

  case(28)
     if (D .gt.0) then
        DIB = BARK1(IS)*D**BARK2(IS)
        BRATIO = DIB/D
     else
        BRATIO = 0.99
     end if
     if (BRATIO .gt.0.99) BRATIO = 0.99
     if (BRATIO .lt.0.80) BRATIO = 0.80
  end select

  return
end
