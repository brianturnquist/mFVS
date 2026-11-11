!=======================!
!   fire/bm : fmvinit   !
!=======================!
! FIRE-BM $Id$
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

  !     DECAY RATES BASED ON WORKSHOP RESULTS FOR KIM MELLEN-MCLEAN'S CWD MODEL
  !     FIRST BASE RATES ARE SET (BY DECAY RATE CLASS) AND THEN THEY ARE ADJUSTED
  !     BASED ON HABITAT TYPE (TEMPERATURE AND MOISTURE CATEGORY)

  DKR(1,1) = 0.076 ! < 0.25"
  DKR(2,1) = 0.076 ! 0.25 - 1"
  DKR(3,1) = 0.076 ! 1 - 3"
  DKR(4,1) = 0.019 ! 3 - 6"
  DKR(5,1) = 0.019 ! 6 - 12"
  DKR(6,1) = 0.019 ! 12 - 20"
  DKR(7,1) = 0.019 ! 20 - 35"
  DKR(8,1) = 0.019 ! 35 - 50"
  DKR(9,1) = 0.019 !  > 50"

  DKR(1,2) = 0.081 ! < 0.25"
  DKR(2,2) = 0.081 ! 0.25 - 1"
  DKR(3,2) = 0.081 ! 1 - 3"
  DKR(4,2) = 0.025 ! 3 - 6"
  DKR(5,2) = 0.025 ! 6 - 12"
  DKR(6,2) = 0.025 ! 12 - 20"
  DKR(7,2) = 0.025 ! 20 - 35"
  DKR(8,2) = 0.025 ! 35 - 50"
  DKR(9,2) = 0.025 !  > 50"

  DKR(1,3) = 0.090 ! < 0.25"
  DKR(2,3) = 0.090 ! 0.25 - 1"
  DKR(3,3) = 0.090 ! 1 - 3"
  DKR(4,3) = 0.033 ! 3 - 6"
  DKR(5,3) = 0.033 ! 6 - 12"
  DKR(6,3) = 0.033 ! 12 - 20"
  DKR(7,3) = 0.033 ! 20 - 35"
  DKR(8,3) = 0.033 ! 35 - 50"
  DKR(9,3) = 0.033 !  > 50"

  DKR(1,4) = 0.113 ! < 0.25"
  DKR(2,4) = 0.113 ! 0.25 - 1"
  DKR(3,4) = 0.113 ! 1 - 3"
  DKR(4,4) = 0.058 ! 3 - 6"
  DKR(5,4) = 0.058 ! 6 - 12"
  DKR(6,4) = 0.058 ! 12 - 20"
  DKR(7,4) = 0.058 ! 20 - 35"
  DKR(8,4) = 0.058 ! 35 - 50"
  DKR(9,4) = 0.058 !  > 50"

  !  LITTER LOSS/YR (10) AND DUFF LOSS/YR (11)

  do J = 1,4
     DKR(10,J) = 0.65
     DKR(11,J) = 0.002
  end do

  !     DUFF PRODUCTION RATES 'PRDUFF' ARE A PROPORTION OF THE OVERALL
  !     DECAY RATE: 'DKR'.

  do I = 1,MXFLCL
     do J = 1,4
        PRDUFF(I,J) = 0.02
        TODUFF(I,J) = DKR(I,J) * PRDUFF(I,J)
     end do
  end do

  !     SET ALL THE SNAG PARAMETERS HERE (THOSE WHICH ARE UNDER USER CONTROL).
  !     ALSO SET LIMBRK.  NZERO COULD BE UNDER USER-CONTROL ONE DAY.

  NZERO = 0.01
  LIMBRK = 0.01
  HTXSFT = 1.0
  do I = 1,MAXSP
     PSOFT(I) = 0.0
  end do

  !     HTR1 and HTR2 are set below, but are not currently used in this variant,
  !     unless a user modifies the snag height loss rates thru the SNAGBRK keyword.
  !     See fmsnag for details.

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

  !     DKRCLS() - DECAY RATE CLASS 1 (V.SLOW) TO 4 (FAST). MODEL USERS
  !     CAN USE THE FUELDCAY KEYWORD TO REASSIGN RATES WITHIN THE 4
  !     CLASSES, AND THE FUELPOOL KEYWORD TO REASSIGN CLASS

  !     FALL RATE MULTIPLIER FALLX() = 1.79 FOR ALL PINES. THIS GIVES 7%/YR
  !     FOR DBH=15"; FALLX() = 1.02 FOR ALL OTHERS; THIS GIVE 4%/YR FOR
  !     DBH=15"

  !     DECAYX() MULTIPLIER = 999.0; INITIALLY HARD SNAGS NEVER BECOME SOFT

  do I = 1,MAXSP

     select case (I)

        !  WESTERN WHITE PINE (1 = WP) (uses PP for some WP attributes)

     case (1)
        V2T(I) = 22.5
        LEAFLF(I) = 4.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 75.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  WESTERN LARCH (2 = WL) (uses similarity to PP for some WL attributes)

     case (2)
        V2T(I) = 29.9
        LEAFLF(I) = 1.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 100.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  DOUGLAS-FIR (3 = DF)

     case (3)
        V2T(I) = 28.7 ! west DF in Wood Handbook
        LEAFLF(I) = 5.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 75.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  GRAND FIR (4 = GF)

     case (4)
        V2T(I) = 21.8
        LEAFLF(I) = 7.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 60.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  MOUNTAIN HEMLOCK (5 = MH) (mostly from western hemlock)

     case (5)
        V2T(I) = 26.2
        LEAFLF(I) = 5.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 70.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  WESTERN JUNIPER (6 = WJ) -- USE SO WJ

     case (6)
        V2T(I) = 34.9
        LEAFLF(I) = 4.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 100.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  LODGEPOLE PINE (7 = LP)

     case (7)
        V2T(I) = 23.7
        LEAFLF(I) = 3.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 50.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  ENGELMANN SPRUCE (8 = ES)

     case (8)
        V2T(I) = 20.6
        LEAFLF(I) = 6.0
        TFALL(I,3) = 10.0
        ALLDWN(I) = 75.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  SUBALPINE FIR (9 = AF)

     case (9)
        V2T(I) = 19.3
        LEAFLF(I) = 7.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 60.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  PONDEROSA PINE (10 = PP)

     case (10)
        V2T(I) = 23.7
        LEAFLF(I) = 4.0
        TFALL(I,3) = 10.0
        ALLDWN(I) = 80.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  WHITEBARK PINE (11 = WB) USE SO WB VALUES

     case (11)
        V2T(I) = 22.5 ! from white pine
        LEAFLF(I) = 3.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 110.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  LIMBER PINE (12 = LM) USE SO WB VALUES

     case (12)
        V2T(I) = 22.5 ! from white pine
        LEAFLF(I) = 3.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 110.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  PACIFIC YEW (13 = PY) USE SO PY VALUES

     case (13)
        V2T(I) = 26.2 ! baldcypress
        LEAFLF(I) = 7.0
        TFALL(I,3) = 20.0
        ALLDWN(I) = 90.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  ALASKA CEDAR (14 = YC) USE WC YC VALUES

     case (14)
        V2T(I) = 26.2
        LEAFLF(I) = 5.0
        TFALL(I,3) = 20.0
        ALLDWN(I) = 300.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  QUAKING ASPEN (15 = AS) USE SO AS VALUES

     case (15)
        V2T(I) = 21.8
        LEAFLF(I) = 1.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 90.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .false.

        !  BLACK COTTONWOOD (16 = CW) USE SO CW VALUES

     case (16)
        V2T(I) = 19.3
        LEAFLF(I) = 1.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 90.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .false.

        !  OTHER SOFTWOODS (17 = OS) (USE BM PONDEROSA PINE)

     case (17)
        V2T(I) = 23.7
        LEAFLF(I) = 4.0
        TFALL(I,3) = 10.0
        ALLDWN(I) = 80.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  OTHER HARDWOODS (18 = OH) USE SO OH VALUES

     case (18)
        V2T(I) = 21.8
        LEAFLF(I) = 1.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 90.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .false.

     end select

     !       TIME-TO-FALL FOR OTHER CROWN CATEGORIES

     select case (I)
     case(6,11:16,18)
        TFALL(I,0) = 2.0
        TFALL(I,1) = 5.0
        TFALL(I,2) = 5.0
     case default
        TFALL(I,0) = 3.0
        TFALL(I,1) = 10.0
        TFALL(I,2) = 15.0
     end select

     TFALL(I,4) = TFALL(I,3)
     TFALL(I,5) = TFALL(I,3)

     !       DEAD LEAF FALL CANNOT BE > LIVE

     TFALL(I,0) = MIN(3.0, LEAFLF(I))

     !       TFALL(I,3) CANNOT BE < TFALL(I,2)

     if (TFALL(I,2) .gt. TFALL(I,3)) then
        TFALL(I,2) = TFALL(I,3)
     end if

     !       CONVERT LB/FT**3 TO TONS/FT**3

     V2T(I) = V2T(I) / 2000.0

     !       SET THE DECAY RATE CLASS (DKRCLS)

     select case (I)

        !         some pines, doug-fir, cedars
     case (1:3,6,11:14)
        DKRCLS(I) = 1

        !         lodgepole, spruce, hemlock
     case (5,7,8)
        DKRCLS(I) = 2

        !         firs, some pines, oak
     case (4,9,10,17)
        DKRCLS(I) = 3

        !         aspen, cottonwood, other hardwoods
     case (15,16,18)
        DKRCLS(I) = 4

     end select

  end do

  !     PARAMETERS FOR POST-BURN SNAG FALL RATES:

  PBSCOR = 0.0
  PBSOFT = 0.0
  PBSMAL = 0.0
  PBSIZE = 12.0
  PBTIME = 7.0

  !     THE LODGEPOLE PINE COVER TYPE METAGROUP (NOT USED IN BM-FFE)

  OLDICT = 0

  !     DROUGHT START AND END YEARS

  IDRYB = 0
  IDRYE = 0

  !     CRITICAL % CHANGE REQUIRED TO TRIGGER ACTIVITY FUELS

  SLCRIT = 10.0

  return
end
