!=========================!
!   Compute bark ratios   !
!=========================!
! CI $Id$
function BRATIO(IS,D,H)
  implicit none

  ! FUNCTION TO COMPUTE BARK RATIOS AS A FUNCTION OF DIAMETER AND SPECIES.
  ! REPLACES ARRAY BKRAT IN BASE MODEL.

  ! Common blocks
  include 'PRGPRM.f90'

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

  ! Variables
  real    :: BARK1(MAXSP),BARK2(MAXSP),H,D,BRATIO,DIB,TEMD
  integer :: IS
  real    :: RDANUW

  !  DATA STATEMENTS

  data BARK1/&
       &.859045, .900000, .903563, .904973, .903563,&
       &.837291, .900000, .900000, .903563, .809427,&
       &0.969,   0.969,   0.950,     0.0,     0.9,&
       &0.969,   0.892, .900000,   0.892/
  data BARK2/&
       &0.0,     0.0, .989388,     0.0, .989388,&
       &0.0,     0.0,     0.0, .989388,1.016866,&
       &0.0,     0.0,     0.0,     0.0,     0.0,&
       &0.0,  -0.086,     0.0,  -0.086/

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = H

  select case (IS)

  case(3,5,9,10)
     DIB = BARK1(IS)*D**BARK2(IS)
     if (D .gt.0.) then
        BRATIO = DIB/D
     else
        BRATIO = 0.97
     end if
     if (BRATIO .gt.0.97) BRATIO = 0.97

  case(14)
     TEMD = D
     if (TEMD.lt.1.)TEMD = 1.
     if (TEMD.gt.19.)TEMD = 19.
     BRATIO = 0.9002 - 0.3089*(1/TEMD)
     if (BRATIO .gt.0.99) BRATIO = 0.99
     if (BRATIO .lt.0.80) BRATIO = 0.80

  case(17,19)
     TEMD = D
     if (TEMD.lt.1.)TEMD = 1.
     BRATIO = BARK1(IS)+BARK2(IS)*(1.0/TEMD)
     if (BRATIO .gt.0.99) BRATIO = 0.99
     if (BRATIO .lt.0.80) BRATIO = 0.80

  case default
     BRATIO = BARK1(IS)

  end select

  return
end
