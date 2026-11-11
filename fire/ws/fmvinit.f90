!=======================!
!   fire/ws : fmvinit   !
!=======================!
! FIRE-WS $Id$
subroutine FMVINIT
  implicit none

  !  Purpose:
  !      Initialize variant-specific variables for the Fire Model

  !  Called from: INITRE

  !  Call list definitions:

  !  Local variable definitions:

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'FMCOM.f90'
  include 'FMFCOM.f90'

  ! Variables
  integer :: I,J

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
  !    35 = GIANT CHINKAPIN (GC)              CHRYSOLEPIS CHRYSOPHYLLA
  !    36 = QUAKING ASPEN (AS)                POPULUS TREMULOIDES
  !    37 = CALIFORNIA-LAUREL (CL)            UMBELLULARIA CALIFORNICA
  !    38 = PACIFIC MADRONE (MA)              ARBUTUS MENZIESII
  !    39 = PACIFIC DOGWOOD (DG)              CORNUS NUTTALLII
  !    40 = BIGLEAF MAPLE (BM)                ACER MACROPHYLLUM
  !    41 = CURLLEAF MOUNTAIN-MAHOGANY (MC)   CERCOCARPUS LEDIFOLIUS
  !    42 = OTHER SOFTWOODS (OS)
  !    43 = OTHER HARDWOODS (OH)

  LVWEST = .true. ! WESTERN VARIANT

  CANCLS(1) = 5.0
  CANCLS(2) = 17.5
  CANCLS(3) = 37.5
  CANCLS(4) = 75.0

  CORFAC(1) = 0.5
  CORFAC(2) = 0.3
  CORFAC(3) = 0.2
  CORFAC(4) = 0.1

  SNPRCL(1) = 0
  SNPRCL(2) = 12
  SNPRCL(3) = 18
  SNPRCL(4) = 24
  SNPRCL(5) = 30
  SNPRCL(6) = 36

  LOWDBH(1) = 0.0
  LOWDBH(2) = 5.0
  LOWDBH(3) = 10.0
  LOWDBH(4) = 20.0
  LOWDBH(5) = 30.0
  LOWDBH(6) = 40.0
  LOWDBH(7) = 50.0

  !  SET POTENTIAL FIRE TEMPERATURES AND WINDSPEEDS

  PREWND(1) = 20.
  PREWND(2) = 6.
  POTEMP(1) = 70.
  POTEMP(2) = 70.

  !     WS DECAY RATES BASED ON WS DESIGN DOCUMENT (BEUKEMA, 2000)
  !     MODIFIED AT CA-REVIEW MEETING (FEBRUARY 2003) EACH VARIANT
  !     MAY USE DIFFERENT RATES AND CITE DIFFERENT DATA SOURCES

  !     DECAY RATES IN THE WS ARE SUBSEQUENTLY MODIFIED BY A
  !     MULTIPLIER BASED ON DUNNING-CODE (SEE WS **FMCBA**)

  DKR(1,1) = 0.025 ! < 0.25"
  DKR(2,1) = 0.025 ! 0.25 - 1"
  DKR(3,1) = 0.025 ! 1 - 3"
  DKR(4,1) = 0.0125 ! 3 - 6"
  DKR(5,1) = 0.0125 ! 6 - 12"
  DKR(6,1) = 0.0125 ! 12 - 20"
  DKR(7,1) = 0.0125 ! 20 - 35"
  DKR(8,1) = 0.0125 ! 35 - 50"
  DKR(9,1) = 0.0125 !  > 50"

  do I = 1,9
     do J = 2,4
        DKR(I,J) = DKR(I,1)
     end do
  end do

  !     LITTER LOSS/YR (10) AND DUFF LOSS/YR (11)

  do J = 1,4
     DKR(10,J) = 0.5
     DKR(11,J) = 0.002
  end do

  !     Duff production rates 'PRDUFF' are a proportion of the overall
  !     decay rate: 'DKR'.

  do I = 1,MXFLCL
     do J = 1,4
        PRDUFF(I,J) = 0.02
        TODUFF(I,J) = DKR(I,J) * PRDUFF(I,J)
     end do
  end do

  !     SET ALL THE SNAG PARAMETERS HERE (THOSE WHICH ARE UNDER USER CONTROL).
  !     ALSO SET LIMBRK.  NZERO COULD BE UNDER USER-CONTROL ONE DAY.

  !     VALUE OF HTXSFT BASED ON CONVERTING PL HEIGHT-LOSS RATE TO DF (19.5)
  !     AND PP RATE TO ~DF/SPRUCE (10.2, 2.1); USE 10.0

  NZERO = 0.01
  LIMBRK = 0.01
  HTXSFT = 10.0
  do I = 1,MAXSP
     PSOFT(I) = 0.0
  end do

  !     VALUE OF HTR1 BASED ON 50% HEIGHT LOSS IN 20 YEARS; ALL SPECIES
  !     VALUE OF HTR2 BASED ON HEIGHT LOSS CEASING AFTER 50% LOSS
  !        NOTE: HTR2 CAN NO LONGER BE 0 (FEB 2002) SO WAS RESET TO DEFAULT 0.01
  !              THE CEASING OF HEIGHT-LOSS AFTER 50% IS NOW GIVEN BY HTX (BELOW)

  HTR1 = 0.03406
  HTR2 = 0.01

  !     ** SPECIES-LEVEL VARIABLE ASSIGNMENT **

  !     [TO CONVERT G/CM**3 TO LB/FT**3, MULTIPLY 'X' G/CM**3 * 62.372]

  !     LEAFLF() - LIFETIME (YRS) OF LEAVES/NEEDLES

  !     TFALL() - TIME TO FALL FOR DEAD CROWN COMPONENTS. THIS VARIABLE
  !     IS NOT UNDER USER-CONTROL BUT MIGHT BE SOMEDAY.
  !     [LITTERFALL AND SMALL TWIG FALL VALUES CHANGED 2/97. SB&ER]
  !     TFALL INDEXING USES CROWNW() DIMENSIONS, I.E.

  !     0 :  FOLIAGE
  !     1 : <0.25"
  !     2 :  0.25" -   1"
  !     3 :  1"    -   3"
  !     4 :  3"    -   6"
  !     5 :  6"    -  12"

  !     IF THE VALUE OF TFALL(I,-) IS LARGER THAN 20, PARAMETER TFMAX IN
  !     **FMPARM.F77** MUST BE ADJUSTED TO EQUAL THE NEW VALUE, AND LOOPS
  !     INVOLVING THE VALUE (SEE FMSCRO) MUST BE RE-EXAMINED TO INSURE
  !     THAT THEY BEHAVE PROPERLY.

  !     ALLDWN() - YEARS DEAD AT WHICH ALL SNAGS WILL BE FALLEN
  !     DECAYX() - DECAY RATE MULTIPLIER       (PP = 1.0 in NI var)
  !     FALLX()  - FALL RATE MULTIPLIER        (PP = 1.0 in NI var)
  !     HTX()    - HEIGHT-LOSS RATE MULTIPLIER (PP = 1.0 in NI var)

  !     ** NOTE THAT SNAG RATE PARAMETERS ARE ALL RELATIVE TO PP **

  !     DKRCLS() - DECAY RATE CLASS 1 (V.SLOW) TO 4 (FAST). MODEL USERS
  !     CAN USE THE FUELDCAY KEYWORD TO REASSIGN RATES WITHIN THE 4
  !     CLASSES, AND THE FUELPOOL KEYWORD TO REASSIGN CLASS

  !     FALL RATE MULTIPLIER FALLX() = 1.79 FOR ALL PINES. THIS GIVES 7%/YR
  !     FOR DBH=15"; FALLX() = 1.02 FOR ALL OTHERS; THIS GIVE 4%/YR FOR
  !     DBH=15"

  !     DECAYX() MULTIPLIER = 999.0; INITIALLY HARD SNAGS NEVER BECOME SOFT

  do I = 1,MAXSP

     !  FOR SPECIES USING EQNS FROM THE WS VARIANT:
     !       HARD SNAGS NEVER BECOME SOFT
     !       HEIGHT LOSS IS THE SAME FOR ALL SPP; SEE 'HTR1' (ABOVE)
     !       HEIGHT LOSS CEASES FOR THE LAST 50% (USED TO BE SET BY HTR2, CHANGED FEB 2002)

     DECAYX(I) = 999.0
     HTX(I,1) = 1.0
     HTX(I,3) = 1.0
     HTX(I,2) = 0.0
     HTX(I,4) = 0.0

     select case (I)

        !         sugar pine

     case (1)
        V2T(I) = 21.2
        LEAFLF(I) = 3.0
        TFALL(I,0) = 3.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 100.0
        FALLX(I) = 1.235
        DKRCLS(I) = 4
        LSW(I) = .true.

        !         Douglas-fir

     case (2)
        V2T(I) = 28.7 ! interior west DF in Wood Handbook
        LEAFLF(I) = 5.0
        TFALL(I,0) = 3.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 100.0
        FALLX(I) = 0.882
        DKRCLS(I) = 3
        LSW(I) = .true.

        !         white fir

     case (3)
        V2T(I) = 23.1
        LEAFLF(I) = 7.0
        TFALL(I,0) = 3.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 100.0
        FALLX(I) = 0.882
        DKRCLS(I) = 4
        LSW(I) = .true.

        !         giant sequoia

     case (4)
        V2T(I) = 21.2 ! use redwood
        LEAFLF(I) = 5.0
        TFALL(I,0) = 3.0
        TFALL(I,3) = 20.0
        ALLDWN(I) = 150.0 ! Nate Stevenson, 'a century or more'
        FALLX(I) = 0.687
        DKRCLS(I) = 2
        LSW(I) = .true.

        !         incense cedar

     case (5)
        V2T(I) = 21.8
        LEAFLF(I) = 5.0
        TFALL(I,0) = 1.0
        TFALL(I,3) = 20.0
        ALLDWN(I) = 100.0
        FALLX(I) = 0.687
        DKRCLS(I) = 2
        LSW(I) = .true.

        !         jeffrey pine

     case (6)
        V2T(I) = 21.2
        LEAFLF(I) = 3.0
        TFALL(I,0) = 3.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 100.0
        FALLX(I) = 1.235
        DKRCLS(I) = 4
        LSW(I) = .true.

        !         red fir

     case (7)
        V2T(I) = 22.5
        LEAFLF(I) = 7.0
        TFALL(I,0) = 3.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 100.0
        FALLX(I) = 0.882
        DKRCLS(I) = 4
        LSW(I) = .true.

        !         ponderosa pine

     case (8)
        V2T(I) = 23.7
        LEAFLF(I) = 3.0
        TFALL(I,0) = 3.0
        TFALL(I,3) = 10.0
        ALLDWN(I) = 100.0
        FALLX(I) = 1.235
        DKRCLS(I) = 4
        LSW(I) = .true.

        !         lodgepole pine

     case (9)
        V2T(I) = 23.7 ! lodgepole/ponderosa
        LEAFLF(I) = 3.0
        TFALL(I,0) = 3.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 100.0
        FALLX(I) = 1.235
        DKRCLS(I) = 4
        LSW(I) = .true.

        !         whitebark pine

     case (10)
        V2T(I) = 22.5 ! white
        LEAFLF(I) = 3.0
        TFALL(I,0) = 3.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 100.0
        FALLX(I) = 1.235
        DKRCLS(I) = 4
        LSW(I) = .true.

        !         western white pine

     case (11)
        V2T(I) = 22.5
        LEAFLF(I) = 3.0
        TFALL(I,0) = 3.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 100.0
        FALLX(I) = 1.235
        DKRCLS(I) = 4
        LSW(I) = .true.

        !         singleleaf pinyon

     case (12)
        V2T(I) = 31.8
        LEAFLF(I) = 3.0
        TFALL(I,0) = 2.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 150.0
        DECAYX(I) = 0.9
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 0.0978
        end do
        DKRCLS(I) = 4
        LSW(I) = .true.

        !         pacific silver fir

     case (13)
        V2T(I) = 24.9
        LEAFLF(I) = 7.0
        TFALL(I,0) = 3.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 100.0
        FALLX(I) = 0.882
        DKRCLS(I) = 4
        LSW(I) = .true.

        !         knobcone pine

     case (14)
        V2T(I) = 23.7 ! lodgepole/ponderosa
        LEAFLF(I) = 4.0
        TFALL(I,0) = 3.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 100.0
        FALLX(I) = 1.235
        DKRCLS(I) = 4
        LSW(I) = .true.

        !         foxtail pine

     case (15)
        V2T(I) = 23.7 ! lodgepole/ponderosa
        LEAFLF(I) = 4.0
        TFALL(I,0) = 3.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 100.0
        FALLX(I) = 1.235
        DKRCLS(I) = 4
        LSW(I) = .true.

        !         Coulter pine

     case (16)
        V2T(I) = 23.7 ! lodgepole/ponderosa
        LEAFLF(I) = 3.0
        TFALL(I,0) = 3.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 100.0
        FALLX(I) = 1.235
        DKRCLS(I) = 4
        LSW(I) = .true.

        !         limber pine

     case (17)
        V2T(I) = 22.5 ! white
        LEAFLF(I) = 3.0
        TFALL(I,0) = 3.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 100.0
        FALLX(I) = 1.235
        DKRCLS(I) = 4
        LSW(I) = .true.

        !         monterey pine

     case (18)
        V2T(I) = 23.7
        LEAFLF(I) = 3.0
        TFALL(I,0) = 3.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 100.0
        FALLX(I) = 1.235
        DKRCLS(I) = 4
        LSW(I) = .true.

        !         gray or California foothill pine

     case (19)
        V2T(I) = 23.7 ! lodgepole/ponderosa
        LEAFLF(I) = 3.0
        TFALL(I,0) = 3.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 100.0
        FALLX(I) = 1.235
        DKRCLS(I) = 4
        LSW(I) = .true.

        !         washoe pine

     case (20)
        V2T(I) = 23.7 ! lodgepole/ponderosa
        LEAFLF(I) = 4.0
        TFALL(I,0) = 3.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 100.0
        FALLX(I) = 1.235
        DKRCLS(I) = 4
        LSW(I) = .true.

        !         Great Basin bristlecone pine

     case (21)
        TFALL(I,0) = 3.0
        DKRCLS(I) = 4
        V2T(I) = 23.7
        LEAFLF(I) = 3.0
        TFALL(I,3) = 20.0
        ALLDWN(I) = 999.0
        DECAYX(I) = 0.9
        FALLX(I) = 0.001
        do J = 1,4
           HTX(I,J) = 0.0462
        end do
        LSW(I) = .true.

        !         bigcone Douglas-fir

     case (22)
        V2T(I) = 28.7 ! interior west DF in Wood Handbook
        LEAFLF(I) = 5.0
        TFALL(I,0) = 3.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 100.0
        FALLX(I) = 0.882
        DKRCLS(I) = 3
        LSW(I) = .true.

        !         redwood

     case (23)
        V2T(I) = 21.2 ! use redwood
        LEAFLF(I) = 5.0
        TFALL(I,0) = 3.0
        TFALL(I,3) = 20.0
        ALLDWN(I) = 150.0 ! Nate Stevenson, 'a century or more'
        FALLX(I) = 0.687
        DKRCLS(I) = 2
        LSW(I) = .true.

        !         mountain hemlock

     case (24)
        V2T(I) = 26.2
        LEAFLF(I) = 4.0
        TFALL(I,0) = 3.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 100.0
        FALLX(I) = 1.235
        DKRCLS(I) = 4
        LSW(I) = .true.

        !         western juniper, Utah juniper, California juniper

     case (25:27)
        V2T(I) = 34.9
        LEAFLF(I) = 4.0
        TFALL(I,0) = 3.0
        TFALL(I,3) = 20.0
        ALLDWN(I) = 150.0
        FALLX(I) = 0.687
        DKRCLS(I) = 2
        LSW(I) = .true.

        !         California live oak, canyon live oak, blue oak, California black oak,
        !         California white/valley oak, interior live oak, bigleaf maple,
        !         other hardwoods

     case (28:33,40,43)
        select case (I)
        case (28,29,33)
           V2T(I) = 49.9 ! live oak
        case (30,32)
           V2T(I) = 37.4 ! white oak
        case (31,43)
           V2T(I) = 34.9 ! black oak
        case (40)
           V2T(I) = 27.4
        end select
        LEAFLF(I) = 1.0
        TFALL(I,0) = 1.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 50.0
        FALLX(I) = 1.545
        DKRCLS(I) = 2
        LSW(I) = .false.

        !         tanoak, giant chinkapin, quaking aspen, California-laurel,
        !         pacific madrone, pacific dogwood

     case (34:39)
        select case (I)
        case(34,35,37,38)
           V2T(I) = 36.2
        case(36)
           V2T(I) = 21.8
        case(39)
           V2T(I) = 27.4 ! bigleaf maple
        end select
        LEAFLF(I) = 1.0
        TFALL(I,0) = 1.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 50.0
        FALLX(I) = 1.545
        select case (I)
        case(34:36,39)
           DKRCLS(I) = 4
        case(37)
           DKRCLS(I) = 2
        case(38)
           DKRCLS(I) = 3
        end select
        LSW(I) = .false.

        !         curlleaf mountain-mahogany

     case (41)
        V2T(I) = 21.8
        LEAFLF(I) = 1.0
        TFALL(I,0) = 1.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 90.0
        FALLX(I) = 1.0
        DKRCLS(I) = 4
        LSW(I) = .false.
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        DECAYX(I) = 1.0

        !         other softwoods: use LP

     case (42)
        V2T(I) = 23.7
        LEAFLF(I) = 3.0
        TFALL(I,0) = 3.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 100.0
        FALLX(I) = 1.235
        DKRCLS(I) = 4
        LSW(I) = .true.

     end select

     !       TIME-TO-FALL FOR OTHER CROWN CATEGORIES

     TFALL(I,1) = 10.0
     TFALL(I,2) = 15.0
     TFALL(I,4) = TFALL(I,3)
     TFALL(I,5) = TFALL(I,3)

     !       DEAD LEAF FALL CANNOT BE > LIVE

     TFALL(I,0) = MIN(TFALL(I,0), LEAFLF(I))

     !       TFALL(I,3) CANNOT BE < TFALL(I,2)

     if (TFALL(I,2) .gt. TFALL(I,3)) then
        TFALL(I,2) = TFALL(I,3)
     end if

     !       CONVERT LB/FT**3 TO TONS/FT**3

     V2T(I) = V2T(I) / 2000.0

  end do

  !     parameters for post-burn snag fall rates:

  PBSCOR = 0.0
  PBSOFT = 1.0
  PBSMAL = 0.9
  PBSIZE = 12.0
  PBTIME = 7.0

  !     PARAMETERS FOR FUEL MODEL SELECTION

  !     THE LODGEPOLE PINE COVER TYPE METAGROUP **FMCFMD**

  OLDICT = 0

  !     DROUGHT START AND END YEARS

  IDRYB = 0
  IDRYE = 0

  !     CRITICAL % CHANGE REQUIRED TO TRIGGER ACTIVITY FUELS

  SLCRIT = 10.0

  !     CRITICAL % CHANGE IN %CC REQUIRED TO TRIGGER SHRUB MODEL DELAY

  CCCRIT = 10.0

  return
end
