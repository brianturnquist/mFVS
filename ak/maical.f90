!=========================================!
!   Compute stand mean annual increment   !
!=========================================!
! AK $Id$
subroutine MAICAL
  implicit none

  !  THIS SUBROUTINE CALCULATES THE MAI FOR THE STAND.
  !  CALLED FROM CRATET.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.F77'
  include 'COEFFS.F77'
  include 'CONTRL.F77'
  include 'HTCAL.F77'
  include 'OUTCOM.F77'
  include 'PLOT.F77'

  ! Variables
  integer :: IERR,ISICD

  integer :: ISPNUM(MAXSP)

  real    :: ADJMAI,SSSI

  ! SPECIES LIST FOR ALASKA VARIANT.

  ! Number Code  Common Name         FIA  PLANTS Scientific Name
  !   1     SF   Pacific silver fir  011  ABAM   Abies amabilis
  !   2     AF   subalpine fir       019  ABLA   Abies lasiocarpa
  !   3     YC   Alaska cedar        042  CANO9  Callitropsis nootkatensis
  !   4     TA   tamarack            071  LALA   Larix laricina
  !   5     WS   white spruce        094  PIGL   Picea glauca
  !   6     LS   Lutz�s spruce            PILU   Picea lutzii
  !   7     BE   black spruce        095  PIMA   Picea mariana
  !   8     SS   Sitka spruce        098  PISI   Picea sitchensis
  !   9     LP   lodgepole pine      108  PICO   Pinus contorta
  !  10     RC   western redcedar    242  THPL   Thuja plicata
  !  11     WH   western hemlock     263  TSHE   Tsuga heterophylla
  !  12     MH   mountain hemlock    264  TSME   Tsuga mertensiana
  !  13     OS   other softwoods     298  2TN
  !  14     AD   alder species       350  ALNUS  Alnus species
  !  15     RA   red alder           351  ALRU2  Alnus rubra
  !  16     PB   paper birch         375  BEPA   Betula papyrifera
  !  17     AB   Alaska birch        376  BENE4  Betula neoalaskana
  !  18     BA   balsam poplar       741  POBA2  Populus balsamifera
  !  19     AS   quaking aspen       746  POTR5  Populus tremuloides
  !  20     CW   black cottonwood    747  POBAT  Populus trichocarpa
  !  21     WI   willow species      920  SALIX  Salix species
  !  22     SU   Scouler�s willow    928  SASC   Salix scouleriana
  !  23     OH   other hardwoods     998  2TB

  !  DATA STATEMENTS:

  !  INITIALIZE INTERNAL VARIABLES:
  !  (ISPNUM CONTAINS THE FIA CODE FOR THE PROXY EQUATION FOR THE CALL TO
  !   SUBROUTINE **ADJMAI**)

  data ISPNUM/ 011, 019, 042, 071, 094, 094, 095, 098, 108, 242, &
       & 263, 264, 298, 350, 351, 375, 376, 741, 746, 747, &
       & 920, 928, 998/

  !     CALCULATE ADJUSTED MAI AND LIMIT TO 128 MAX.
  !     DEFAULT SITE SPECIES IS WESTERN HEMLOCK.

  if (ISISP .eq. 0) ISISP = 11
  SSSI = SITEAR(ISISP)
  if (SSSI .eq. 0.) SSSI = 80.0
  ISICD = ISPNUM(ISISP)
  RMAI = ADJMAI(ISICD,SSSI,10.0,IERR)
  if (RMAI .gt. 128.0)RMAI = 128.0
  return
end subroutine MAICAL
