!======================================================!
!   Distribute mortality by percentile and tolerance   !
!======================================================!
! AK $Id$
subroutine SEAMRT(TOKILL,DEBUG,SUMKIL)
  implicit none

  ! SUBROUTINE TO DISTRIBUTE MORTALITY ACCORDING TO PERCENTILE AND
  ! SPECIES TOLERANCE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'
  include 'ESTREE.f90'
  include 'MULTCM.f90'
  include 'OUTCOM.f90'
  include 'PDEN.f90'
  include 'PLOT.f90'

  ! Variables
  logical :: DEBUG

  integer :: I,IG,ISWTCH,JPASS,JSPC,MINSTP,NPASS

  real    :: ADJUST,CRI,OTEM2,PASS1,PEFF,SHORT,SUMKIL,TEMKIL,TEMSUM
  real    :: TOKILL,TPALFT,XKILL

  real    :: EFFTR(MAXTRE),TEMWK2(MAXTRE),VARADJ(MAXSP)

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

  data VARADJ / 0.300, 0.100, 0.300, 0.500, 0.500, &
       & 0.500, 0.500, 0.300, 0.700, 0.100, &
       & 0.100, 0.300, 0.700, 0.500, 0.500, &
       & 0.500, 0.500, 0.900, 0.900, 0.900, &
       & 0.500, 0.500, 0.500 /

  !  VARIABLE DEFINITIONS:
  !   ADJUST = FINAL SCALAR ADJUSTMENT NEEDED TO SCALE MORTALITY VALUES
  !            TO ACHIEVE THE DESIRED MORTALITY LEVEL
  !    SHORT = TPA SHORT OF REACHING THE DESIRED MORTALITY LEVEL (DUE TO
  !            ALL THE TREE'S PROB BEING ATTRIBUTED TO MORTALITY BEFORE THE
  !            DESIRED STAND LEVEL MORTALITY LEVEL IS REACHED)
  !   TOKILL = NUMBER OF TREES TO KILL THIS CYCLE
  !   SUMKIL = RUNNING TOTAL OF NUMBER OF TREES KILLED SO FAR
  !    PASS1 = NUMBER OF TREES KILLED IN ONE PASS THROUGH THE TREELIST
  !            WITH THE SPECIFIED TREE LEVEL MORTALITY EFFICIENCIES
  !   VARADJ = TREE SHADE TOLERANCE (1.0 = MOST INTOLERANT)
  !    JPASS = VARIABLE TO TRACK THE NUMBER OF PASSES THROUGH THE LOGIC
  !   TPALFT = DIFFERENCE BETWEEN INITIAL TPA AND MORTALITY TPA

  if (DEBUG)write(JOSTND,20)ICYC,TOKILL
  20 format('0ENTERING VARMRT CYCLE =',I3,' DENSITY RELATED TOKILL = ', &
       & F6.1)

  ! DETERMINE TOTAL NUMBER OF TREES TO KILL IF BACKGROUND MORTALITY
  ! IS STILL IN EFFECT

  if (TOKILL .eq. 0.0) then
     do I = 1,ITRN
        TOKILL = TOKILL+WK2(I)
     end do
     if (DEBUG)write(JOSTND,*)' BACKGROUND TOKILL = ',TOKILL
  end if

  ! INITIALIZE SCALARS AND ARRAYS

  TEMKIL = TOKILL
  JPASS = 0
  PASS1 = 0.
  SUMKIL = 0.
  do I = 1,ITRN
     WK2(I) = 0.
     TEMWK2(I) = 0.
     EFFTR(I) = 0.
  end do

  ! PASS THROUGH THE TREELIST AND LOAD MORTALITY EFFICIENCY VALUES FOR
  ! EACH TREE RECORD.
  ! CALCULATE HOW MANY TPA WILL BE KILLED IN ONE PASS THROUGH THE TREELIST
  ! WITH EFFICIENCY VALUES SET AT THIS LEVEL.

  do I = 1,ITRN
     JSPC = ISP(I)
     CRI = real(ICR(I))
     PEFF = 0.84525 - 0.01074*PCT(I) + 0.0000002*PCT(I)**3.0
     if (PEFF .gt. 1.0) PEFF = 1.0
     if (PEFF .lt. 0.01) PEFF = 0.01
     EFFTR(I) = PEFF * ((100.-CRI)/100.) * VARADJ(JSPC) * 0.01
     PASS1 = PASS1 + PROB(I)*EFFTR(I)
  end do
  if (DEBUG)write(JOSTND,30)ITRN,(EFFTR(IG),IG = 1,ITRN)
  30 format(' MORTALITY EFFICIENCY VALUES, ITRN = ',I7, &
       & (/10F10.5))
  if (DEBUG)write(JOSTND,*)' TREES KILLED IN ONE PASS = ',PASS1

  !  CALCULATE THE APPROXIMATE NUMBER OF PASSES NEEDED TO ACHIEVE THE
  !  DESIRED MORTALITY

  NPASS = INT(TOKILL/PASS1)+1
  if (DEBUG)write(JOSTND,*)' APPROXIMATE NUMBER OF PASSES NEEDED = ', &
       & NPASS

  !  LOOP THROUGH THE TREES AND LOAD THE FIRST GUESS AT TREE RECORD
  !  MORTALITY BASED ON THE MORTALITY EFFICIENCY (r) AND APPROXIMATE NUMBER
  !  OF PASSES NEEDED (n). THIS IS A GEOMETRIC PROGRESSION WHERE THE RATE
  !  IS THE MORTALITY EFFICIENCY (r) AND THE STARTING VALUE IS THE INITIAL
  !  PROB VALUE (a). THE Nth TERM IN THE PROGRESSION IS a*r*(1-r)**(n-1)
  !  AND THE SUM OF N TERMS IS -a*((1-r)**n - 1)
  !  ACCUMULATE THE MORTALITY ACHIEVED WITH THIS FIRST PASS

  100 continue
  JPASS = JPASS+1
  if (JPASS .gt. 1)TEMKIL = SHORT
  ISWTCH = 0
  105 continue
  TEMSUM = 0.
  do I = 1,ITRN
     TPALFT = PROB(I)-WK2(I)
     if (TPALFT .gt. 0.) then
        OTEM2 = TEMWK2(I)
        TEMWK2(I) = (-TPALFT*((1.0-EFFTR(I))**NPASS - 1.0))
        if (DEBUG)write(JOSTND,*)' I,PROB,WK2,TPALFT,EFFTR,TEMWK2,', &
             & 'OTEM2= ',I,PROB(I),WK2(I),TPALFT,EFFTR(I),TEMWK2(I),OTEM2
        TEMSUM = TEMSUM+TEMWK2(I)
     end if
  end do
  if (DEBUG)write(JOSTND,*)' AFTER GUESS ',JPASS,' TEMSUM= ',TEMSUM, &
       & '  TOKILL= ',TOKILL

  ! ADJUST MORTALITY VALUES TO ACHIEVE DESIRED MORTALITY
  ! IF AN ENTIRE TREE RECORD PROB GETS KILLED, ADJUST PASS1 VALUE FOR
  ! THE NEXT PASS

  if (NPASS .gt. 50) then
     MINSTP = 5
  else if (NPASS .gt. 20) then
     MINSTP = 2
  else
     MINSTP = 1
  end if
  ADJUST = TEMKIL/TEMSUM
  if (ADJUST .lt. 0.8) then
     if (ISWTCH .eq. 2) go to 110
     if (DEBUG)write(JOSTND,*)' TEMKIL,TEMSUM,PASS1,NPASS= ', &
          & TEMKIL,TEMSUM,PASS1,NPASS
     NPASS = NPASS-MAX(MINSTP,INT((TEMSUM-TEMKIL)/PASS1))
     if (DEBUG)write(JOSTND,*)' ADJUST= ',ADJUST,'  IS TO SMALL,', &
          & ' MIN STEP= ',MINSTP,' NEW NPASS= ',NPASS
     ISWTCH = 1
     if (NPASS .le. 0)go to 110
     go to 105
  else if (ADJUST .gt. 1.2) then
     if (ISWTCH .eq. 1) go to 110
     NPASS = NPASS+MAX(MINSTP,INT((TEMKIL-TEMSUM)/PASS1))
     if (DEBUG)write(JOSTND,*)' ADJUST= ',ADJUST,'  IS TO BIG,', &
          & ' MIN STEP= ',MINSTP,' NEW NPASS= ',NPASS
     ISWTCH = 2
     go to 105
  end if
  110 continue
  SHORT = 0.
  if (DEBUG)write(JOSTND,*)' TEMKIL= ',TEMKIL,'  TEMSUM= ', &
       & TEMSUM,'  ADJUSTMENT= ',ADJUST
  do I = 1,ITRN
     TPALFT = PROB(I)-WK2(I)
     if (TPALFT .lt. 0.00001)go to 150
     XKILL = TEMWK2(I)*ADJUST
     if ((PROB(I)-WK2(I)-XKILL) .le. 0.00001) then
        TEMWK2(I) = PROB(I)-WK2(I)
        if (DEBUG)write(JOSTND,*)' SHORT,I,XKILL,PROB,WK2= ', &
             & SHORT,I,XKILL,PROB(I),WK2(I)
        SHORT = SHORT+(XKILL-PROB(I)+WK2(I))
        if (DEBUG)write(JOSTND,*)' SHORT= ',SHORT
        PASS1 = PASS1-EFFTR(I)
        go to 140
     end if
     TEMWK2(I) = XKILL
     140 continue
     WK2(I) = WK2(I)+TEMWK2(I)
     SUMKIL = SUMKIL+TEMWK2(I)
     150 continue
  end do
  if (DEBUG)write(JOSTND,23)ITRN,(WK2(IG),IG = 1,ITRN)
  23 format(' ADJUSTED MORTALITY VALUES, ITRN = ',I7, &
       & (/10F10.5))
  if (DEBUG)write(JOSTND,*)' SHORT = ',SHORT
  if (SHORT .gt. 0.) then
     NPASS = INT(SHORT/PASS1)+1
     if (DEBUG)write(JOSTND,*)' SHORT,PASS1, ADJUSTED PASSES NEEDED', &
          & '= ',SHORT,PASS1,NPASS
     go to 100
  end if

  return
end subroutine SEAMRT
