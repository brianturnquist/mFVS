!=======================!
!   fire/oc : fmvinit   !
!=======================!
! FIRE-OC $Id$
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

  !     SET DECAY VARIABLES TO AN UNSET STATE

  !     IN CA-FFE VARIANT DECAY RATE DEFAULTS ARE ASSIGNED IN CYCLE 1
  !     IN **FMCBA**

  do I = 1,MXFLCL
     do J = 1,4
        DKR(I,J) = -1.
     end do
  end do
  do I = 1,MXFLCL
     do J = 1,4
        PRDUFF(I,J) = -1.
        TODUFF(I,J) = 0.
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

  !     V2T() - UNITS ARE LB/CUFT BY SPECIES FROM NI/SO VARIANTS,
  !     AND 'WOOD HANDBOOK' USDA FOREST PRODUCTS AG. HANDBOOK 72.
  !     1974. DENSITY OF PINYON, JUNIPER, GAMBEL FROM CHOJNACKY 1992.

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

  !     DECAYX() MULTIPLIER = 999.0; INITIALLY HARD SNAGS NEVER BECOME SOFT

  do I = 1,MAXSP

     select case (I)

        !         Port Orford cedar (borrows from western redcedar)
     case (1)
        V2T(I) = 24.3
        LEAFLF(I) = 4.0
        TFALL(I,0) = 3.0
        TFALL(I,3) = 20.0
        ALLDWN(I) = 150.0
        FALLX(I) = 1.235 ! 20", 95% down @ 25 yrs
        LSW(I) = .true.

        !         incense-cedar
     case (2)
        V2T(I) = 21.8
        LEAFLF(I) = 5.0
        TFALL(I,0) = 1.0
        TFALL(I,3) = 20.0
        ALLDWN(I) = 100.0
        FALLX(I) = 0.687
        LSW(I) = .true.

        !         western redcedar (uses DF for some RC attributes)
     case (3)
        V2T(I) = 19.3
        LEAFLF(I) = 5.0
        TFALL(I,0) = 3.0
        TFALL(I,3) = 20.0
        ALLDWN(I) = 150.0
        FALLX(I) = 1.235
        LSW(I) = .true.

        !         grand fir (uses SO California grand fir settings)
     case (4)
        V2T(I) = 21.8
        LEAFLF(I) = 7.0
        TFALL(I,0) = 3.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 100.0
        FALLX(I) = 0.882
        LSW(I) = .true.

        !         California red fir, Shasta red fir
     case (5,6)
        V2T(I) = 22.5
        LEAFLF(I) = 7.0
        TFALL(I,0) = 3.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 100.0
        FALLX(I) = 0.882
        LSW(I) = .true.

        !         Douglas-fir
     case (7)
        V2T(I) = 28.7 ! interior west DF in Wood Handbook
        LEAFLF(I) = 5.0
        TFALL(I,0) = 3.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 100.0
        FALLX(I) = 0.882
        LSW(I) = .true.

        !         western hemlock
     case (8)
        V2T(I) = 26.2
        LEAFLF(I) = 5.0
        TFALL(I,0) = 3.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 100.0
        FALLX(I) = 1.235
        LSW(I) = .true.

        !         mountain hemlock
     case (9)
        V2T(I) = 26.2
        LEAFLF(I) = 4.0
        TFALL(I,0) = 3.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 100.0
        FALLX(I) = 1.235
        LSW(I) = .true.

        !         pines: whitebark (10), knobcone (11), lodgepole (12),
        !         Coulter (13), limber (14), Jeffrey (15), sugar (16),
        !         western white (17), ponderosa (18), Monterey (19),
        !         gray (20), other softwoods (25)
     case (10:20,25)
        select case (I)
        case (10,14,17)
           V2T(I) = 22.5 ! white
        case (11,12,13,18,19,20,25)
           V2T(I) = 23.7 ! lodgepole/ponderosa
        case (15,16)
           V2T(I) = 21.2 ! sugar
        end select
        select case (I)
        case (11)
           LEAFLF(I) = 4.0
        case default
           LEAFLF(I) = 3.0
        end select
        TFALL(I,0) = 3.0
        select case (I)
        case (18,25)
           TFALL(I,3) = 10.0
        case default
           TFALL(I,3) = 15.0
        end select
        ALLDWN(I) = 100.0
        FALLX(I) = 1.235
        LSW(I) = .true.

        !         western juniper
     case (21)
        V2T(I) = 34.9
        LEAFLF(I) = 4.0
        TFALL(I,0) = 3.0
        TFALL(I,3) = 20.0
        ALLDWN(I) = 150.0
        FALLX(I) = 0.687
        LSW(I) = .true.

        !         Brewer spruce
     case (22)
        V2T(I) = 20.6 ! Engelmann
        LEAFLF(I) = 8.0
        TFALL(I,0) = 3.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 100.0
        FALLX(I) = 0.687
        LSW(I) = .true.

        !         giant sequoia and redwood
     case (23,50)
        V2T(I) = 21.2 ! use redwood
        LEAFLF(I) = 5.0
        TFALL(I,0) = 3.0
        TFALL(I,3) = 20.0
        ALLDWN(I) = 150.0
        FALLX(I) = 0.687
        LSW(I) = .true.

        !         Pacific yew
     case (24)
        V2T(I) = 26.2 ! use baldcypress
        LEAFLF(I) = 7.0
        TFALL(I,0) = 3.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 100.0
        FALLX(I) = 0.687
        LSW(I) = .true.

        !         oaks: coast live oak (26), canyon live oak (27),
        !         blue oak (28), Engelmann oak (29), Oregon white
        !         oak (30), California black oak (31), valley white
        !         oak (32), interior live oak (33), other hardwoods (49)

     case (26:33,49)
        select case (I)
        case (26,27,33)
           V2T(I) = 49.9 ! live oak
        case (28,29,30,32)
           V2T(I) = 37.4 ! white oak
        case (31,49)
           V2T(I) = 34.9 ! black oak
        end select
        LEAFLF(I) = 1.0
        TFALL(I,0) = 1.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 50.0
        FALLX(I) = 1.545
        LSW(I) = .false.

        !         bigleaf maple
     case (34)
        V2T(I) = 27.4
        LEAFLF(I) = 1.0
        TFALL(I,0) = 1.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 50.0
        FALLX(I) = 1.545
        LSW(I) = .false.

        !         California buckeye
     case (35)
        V2T(I) = 37.4 ! use white oak
        LEAFLF(I) = 1.0
        TFALL(I,0) = 1.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 50.0
        FALLX(I) = 1.545
        LSW(I) = .false.

        !         red alder
     case (36)
        V2T(I) = 23.1
        LEAFLF(I) = 1.0
        TFALL(I,0) = 1.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 50.0
        FALLX(I) = 1.545
        LSW(I) = .false.

        !         Pacific madrone (37)
     case (37)
        V2T(I) = 36.2 ! tanoak
        LEAFLF(I) = 1.0
        TFALL(I,0) = 1.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 50.0
        FALLX(I) = 1.545
        LSW(I) = .false.

        !         Golden chinkapin (38), tanoak (42)
     case (38,42)
        V2T(I) = 36.2 ! tanoak
        LEAFLF(I) = 1.0
        TFALL(I,0) = 1.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 50.0
        FALLX(I) = 1.545
        LSW(I) = .false.

        !         Pacific dogwood
     case (39)
        V2T(I) = 27.4 ! bigleaf maple
        LEAFLF(I) = 1.0
        TFALL(I,0) = 1.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 50.0
        FALLX(I) = 1.545
        LSW(I) = .false.

        !         Oregon ash
     case (40)
        V2T(I) = 31.2
        LEAFLF(I) = 1.0
        TFALL(I,0) = 1.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 50.0
        FALLX(I) = 1.545
        LSW(I) = .false.

        !         walnut
     case (41)
        V2T(I) = 31.8
        LEAFLF(I) = 1.0
        TFALL(I,0) = 1.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 50.0
        FALLX(I) = 1.545
        LSW(I) = .false.

        !         California sycamore
     case (43)
        V2T(I) = 28.7 ! American sycamore
        LEAFLF(I) = 1.0
        TFALL(I,0) = 1.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 50.0
        FALLX(I) = 1.545
        LSW(I) = .false.

        !         quaking aspen
     case (44)
        V2T(I) = 21.8
        LEAFLF(I) = 1.0
        TFALL(I,0) = 1.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 50.0
        FALLX(I) = 1.545
        LSW(I) = .false.

        !         black cottonwood
     case (45)
        V2T(I) = 19.3
        LEAFLF(I) = 1.0
        TFALL(I,0) = 1.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 50.0
        FALLX(I) = 1.545
        LSW(I) = .false.

        !         willow
     case (46)
        V2T(I) = 22.5 ! black willow
        LEAFLF(I) = 1.0
        TFALL(I,0) = 1.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 50.0
        FALLX(I) = 1.545
        LSW(I) = .false.

        !         California nutmeg
     case (47)
        V2T(I) = 34.9 ! hickory-nutmeg
        LEAFLF(I) = 1.0
        TFALL(I,0) = 1.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 50.0
        FALLX(I) = 1.545
        LSW(I) = .false.

        !         California laurel
     case (48)
        V2T(I) = 36.2 ! tanoak
        LEAFLF(I) = 1.0
        TFALL(I,0) = 1.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 50.0
        FALLX(I) = 1.545
        LSW(I) = .false.

     end select

     !       SET THE DECAY RATE CLASS (DKRCLS)
     !       DKRCLS() - DECAY RATE CLASS 1 (V.SLOW) TO 4 (FAST). MODEL USERS
     !       CAN USE THE FUELDCAY KEYWORD TO REASSIGN RATES WITHIN THE 4
     !       CLASSES, AND THE FUELPOOL KEYWORD TO REASSIGN CLASS

     select case (I)

        !         some pines, doug-fir, cedars
     case (1:3,7,10,14,16,17,21,23,24,50)
        DKRCLS(I) = 1

        !         lodgepole, spruce, hemlock
     case (8,9,11,12,19,22)
        DKRCLS(I) = 2

        !         firs, some pines, oak
     case (4:6,13,15,18,20,25:33,37,38,42,49)
        DKRCLS(I) = 3

        !         aspen, cottonwood, other hardwoods
     case (34:36,39:41,43:48)
        DKRCLS(I) = 4

     end select

     !       HARD SNAGS NEVER BECOME SOFT
     !       HEIGHT LOSS IS THE SAME FOR ALL SPP; SEE 'HTR1' (ABOVE)
     !       HEIGHT LOSS CEASES FOR THE LAST 50% (USED TO BE SET BY HTR2, CHANGED FEB 2002)

     DECAYX(I) = 999.0
     HTX(I,1) = 1.0 ! 20 yrs for all species
     HTX(I,3) = 1.0
     HTX(I,2) = 0.0
     HTX(I,4) = 0.0

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
