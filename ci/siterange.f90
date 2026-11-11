!====================!
!   ci : siterange   !
!====================!
! CI $Id$
subroutine SITERANGE(IWHO,IS,SILOW,SIHIGH)
  implicit none

  ! SUBROUTINE TO RETURN THE SITE INDEX RANGE FOR A SPECIES.

  ! THIS SUBROUTINE IS CALLED FROM **HTGF**,**REGENT**, AND **SITSET**.

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

  ! Common blocks
  include 'PRGPRM.f90'

  ! Variables
  real    :: SITELO(MAXSP),SITEHI(MAXSP)
  real    :: SILOW,SIHIGH
  integer :: IS,IWHO

  !  DEFINITION OF VARIABLES:

  !      IS -- FVS SPECIES INDEX NUMBER
  !    IWHO -- 1 IF CALLED FROM SUBROUTINE **SITSET**
  !            2 IF CALLED FROM SUBROUTINE **REGENT**
  !  SITEHI -- UPPER SITE INDEX VALUES
  !  SITELO -- LOWER SITE INDEX VALUES

  !  DATA STATEMENTS:

  !  THESE VALUES SHOULD BE BASED ON THE BASE-AGE OF THE SITE CURVE
  !  BEING USED FOR THAT SPECIES.

  !  SITE INDEX NOT USED FOR THESE SPECIES BUT VALUES ASSIGNED FOR
  !  TRANSLATION TO SPECIES THAT DO USE SITE INDEX:
  !  FROM BM: WP, WL, GF, CW
  !  FROM TT: DF, LP, ES, AF, PP
  !  FROM SO: WH, RC
  !  OS USES DF, OH USES CW

  data SITELO/&
       &20.,  50.,  30.,  50.,   6.,  29.,  20.,  40.,  40.,  40.,&
       &25.,  25.,  30.,   5.,   5.,  25.,  30.,  30.,  30./

  data SITEHI/&
       &80., 110.,  70., 110., 203., 152., 100., 100.,  90.,  80.,&
       &50.,  50.,  70.,  15.,  15.,  50., 120.,  70., 120./

  select case (IWHO)

     !  CALLED FROM SUBROUTINES **SITSET** AND **REGENT**

  case (1,2)
     SILOW = SITELO(IS)
     SIHIGH = SITEHI(IS)

     !  SPACE FOR ADDITIONAL CALLS

  case default
     SILOW = 0.
     SIHIGH = 999.

  end select

  return
end
