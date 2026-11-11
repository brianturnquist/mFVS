!=======================!
!   fire/ec : fmvinit   !
!=======================!
! FIRE-EC $Id$
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
     DKR(10,J) = 0.50
     DKR(11,J) = 0.002
  end do

  !  DUFF PRODUCTION RATES 'PRDUFF' ARE A PROPORTION OF THE OVERALL
  !  DECAY RATE: 'DKR'.

  do I = 1,MXFLCL
     do J = 1,4
        PRDUFF(I,J) = 0.02
        TODUFF(I,J) = DKR(I,J) * PRDUFF(I,J)
     end do
  end do

  !  SET ALL THE SNAG PARAMETERS HERE (THOSE WHICH ARE UNDER USER CONTROL).
  !  ALSO SET LIMBRK.  NZERO COULD BE UNDER USER-CONTROL ONE DAY.

  !  HTR1 and HTR2 are set below, but are not currently used in this variant,
  !  unless a user modifies the snag height loss rates thru the SNAGBRK keyword.
  !  See fmsnag for details.

  NZERO = 0.01
  LIMBRK = 0.01
  HTXSFT = 1.0
  HTR1 = 0.0228
  HTR2 = 0.01
  do I = 1,MAXSP
     PSOFT(I) = 0.0
  end do

  !  ** SPECIES-LEVEL VARIABLE ASSIGNMENT **

  !  V2T() - UNITS ARE LB/CUFT BY SPECIES FROM NI/SO VARIANTS,
  !  AND 'WOOD HANDBOOK' USDA FOREST PRODUCTS AG. HANDBOOK 72.
  !  1974. DENSITY OF PINYON, JUNIPER, GAMBEL FROM CHOJNACKY 1992.

  !  [TO CONVERT G/CM**3 TO LB/FT**3, MULTIPLY 'X' G/CM**3 * 62.372]

  !  LEAFLF() - LIFETIME (YRS) OF LEAVES/NEEDLES

  !  TFALL() - TIME TO FALL FOR DEAD CROWN COMPONENTS. THIS VARIABLE
  !  IS NOT UNDER USER-CONTROL BUT MIGHT BE SOMEDAY.
  !  [LITTERFALL AND SMALL TWIG FALL VALUES CHANGED 2/97. SB&ER]
  !  TFALL INDEXING USES CROWNW() DIMENSIONS, I.E.

  !     0 :  FOLIAGE
  !     1 : <0.25"
  !     2 :  0.25" -   1"
  !     3 :  1"    -   3"
  !     4 :  3"    -   6"
  !     5 :  6"    -  12"

  !  IF THE VALUE OF TFALL(I,-) IS LARGER THAN 20, PARAMETER TFMAX IN
  !  **FMPARM.F77** MUST BE ADJUSTED TO EQUAL THE NEW VALUE, AND LOOPS
  !  INVOLVING THE VALUE (SEE FMSCRO) MUST BE RE-EXAMINED TO INSURE
  !  THAT THEY BEHAVE PROPERLY.

  !  ALLDWN() - YEARS DEAD AT WHICH ALL SNAGS WILL BE FALLEN
  !  DECAYX() - DECAY RATE MULTIPLIER       (PP = 1.0 in NI var)
  !  FALLX()  - FALL RATE MULTIPLIER        (PP = 1.0 in NI var)
  !  HTX()    - HEIGHT-LOSS RATE MULTIPLIER (PP = 1.0 in NI var)

  !  ** NOTE THAT SNAG RATE PARAMETERS ARE ALL RELATIVE TO PP **

  !  DKRCLS() - DECAY RATE CLASS 1 (V.SLOW) TO 4 (FAST). MODEL USERS
  !  CAN USE THE FUELDCAY KEYWORD TO REASSIGN RATES WITHIN THE 4
  !  CLASSES, AND THE FUELPOOL KEYWORD TO REASSIGN CLASS

  do I = 1,MAXSP

     select case (I)

        !  white pine (NI)

     case (1)
        V2T(I) = 22.5
        LEAFLF(I) = 4.0
        TFALL(I,0) = 2.0
        TFALL(I,1) = 5.0
        TFALL(I,2) = 5.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 110.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  western larch (NI)

     case (2)
        V2T(I) = 29.9
        LEAFLF(I) = 1.0
        TFALL(I,0) = 1.0
        TFALL(I,1) = 5.0
        TFALL(I,2) = 5.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 150.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  Douglas-fir (NI)

     case (3)
        V2T(I) = 28.7 ! west DF in Wood Handbook
        LEAFLF(I) = 5.0
        TFALL(I,0) = 2.0
        TFALL(I,1) = 5.0
        TFALL(I,2) = 5.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 75.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  Pacific silver fir (based in part on GF-NI)

     case (4)
        V2T(I) = 24.9
        LEAFLF(I) = 7.0
        TFALL(I,0) = 2.0
        TFALL(I,1) = 5.0
        TFALL(I,2) = 5.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 30.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  western redcedar (NI)

     case (5)
        V2T(I) = 19.3
        LEAFLF(I) = 5.0
        TFALL(I,0) = 2.0
        TFALL(I,1) = 5.0
        TFALL(I,2) = 5.0
        TFALL(I,3) = 20.0
        ALLDWN(I) = 300.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  grand fir (NI)

     case (6)
        V2T(I) = 21.8
        LEAFLF(I) = 7.0
        TFALL(I,0) = 2.0
        TFALL(I,1) = 5.0
        TFALL(I,2) = 5.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 90.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  lodgepole pine (NI)

     case (7)
        V2T(I) = 23.7
        LEAFLF(I) = 3.0
        TFALL(I,0) = 2.0
        TFALL(I,1) = 5.0
        TFALL(I,2) = 5.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 35.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  Engelmann spruce (NI)

     case (8)
        V2T(I) = 20.6
        LEAFLF(I) = 6.0
        TFALL(I,0) = 2.0
        TFALL(I,1) = 5.0
        TFALL(I,2) = 5.0
        TFALL(I,3) = 10.0
        ALLDWN(I) = 100.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  subalpine fir (NI)

     case (9)
        V2T(I) = 19.3
        LEAFLF(I) = 7.0
        TFALL(I,0) = 2.0
        TFALL(I,1) = 5.0
        TFALL(I,2) = 5.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 40.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  ponderosa pine (NI)

     case (10)
        V2T(I) = 23.7
        LEAFLF(I) = 4.0
        TFALL(I,0) = 2.0
        TFALL(I,1) = 5.0
        TFALL(I,2) = 5.0
        TFALL(I,3) = 10.0
        ALLDWN(I) = 100.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  western hemlock; from WC

     case (11)
        V2T(I) = 26.2
        LEAFLF(I) = 5.0
        TFALL(I,0) = 3.0
        TFALL(I,1) = 10.0
        TFALL(I,2) = 15.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 100.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  mountain hemlock; from SO via WC

     case (12)
        V2T(I) = 26.2
        LEAFLF(I) = 4.0
        TFALL(I,0) = 2.0
        TFALL(I,1) = 5.0
        TFALL(I,2) = 5.0
        TFALL(I,3) = 10.0
        ALLDWN(I) = 30.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  Pacific yew; from CA via WC

     case (13)
        V2T(I) = 26.2 ! baldcypress
        LEAFLF(I) = 7.0
        TFALL(I,0) = 3.0
        TFALL(I,1) = 10.0
        TFALL(I,2) = 15.0
        TFALL(I,3) = 20.0
        ALLDWN(I) = 100.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  whitebark pine; from CA via WC

     case (14)
        V2T(I) = 22.5 ! white pine
        LEAFLF(I) = 3.0
        TFALL(I,0) = 3.0
        TFALL(I,1) = 10.0
        TFALL(I,2) = 15.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 100.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  noble fir; DF from SO, via WC

     case (15)
        V2T(I) = 23.1
        LEAFLF(I) = 7.0
        TFALL(I,0) = 2.0
        TFALL(I,1) = 5.0
        TFALL(I,2) = 5.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 90.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  white fir; use WC grand fir

     case (16)
        V2T(I) = 21.8
        LEAFLF(I) = 7.0
        TFALL(I,0) = 2.0
        TFALL(I,1) = 5.0
        TFALL(I,2) = 5.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 90.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  subalpine larch; WL from EC, via WC

     case (17)
        V2T(I) = 29.9 ! western larch
        LEAFLF(I) = 1.0
        TFALL(I,0) = 1.0
        TFALL(I,1) = 5.0
        TFALL(I,2) = 5.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 150.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  Alaska cedar; RC from EC, via WC

     case (18)
        V2T(I) = 26.2
        LEAFLF(I) = 5.0
        TFALL(I,0) = 2.0
        TFALL(I,1) = 5.0
        TFALL(I,2) = 5.0
        TFALL(I,3) = 20.0
        ALLDWN(I) = 300.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  western juniper; from SO via WC

     case (19)
        V2T(I) = 34.9
        LEAFLF(I) = 4.0
        TFALL(I,0) = 2.0
        TFALL(I,1) = 5.0
        TFALL(I,2) = 5.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 100.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  bigleaf maple; from CA via WC

     case (20)
        V2T(I) = 27.4
        LEAFLF(I) = 1.0
        TFALL(I,0) = 1.0
        TFALL(I,1) = 10.0
        TFALL(I,2) = 15.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 50.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .false.

        !  vine maple; use bigleaf maple

     case (21)
        V2T(I) = 27.4
        LEAFLF(I) = 1.0
        TFALL(I,0) = 1.0
        TFALL(I,1) = 10.0
        TFALL(I,2) = 15.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 50.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .false.

        !  red alder; from CA via WC

     case (22)
        V2T(I) = 23.1
        LEAFLF(I) = 1.0
        TFALL(I,0) = 1.0
        TFALL(I,1) = 10.0
        TFALL(I,2) = 15.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 50.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .false.

        !  paper birch; from WC

     case (23)
        V2T(I) = 29.9
        LEAFLF(I) = 1.0
        TFALL(I,0) = 1.0
        TFALL(I,1) = 10.0
        TFALL(I,2) = 15.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 50.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .false.

        !  golden chinkapin; from CA via WC

     case (24)
        V2T(I) = 36.2
        LEAFLF(I) = 1.0
        TFALL(I,0) = 1.0
        TFALL(I,1) = 10.0
        TFALL(I,2) = 15.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 50.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .false.

        !  Pacific dogwood; from CA via WC

     case (25)
        V2T(I) = 27.4 ! bigleaf maple
        LEAFLF(I) = 1.0
        TFALL(I,0) = 1.0
        TFALL(I,1) = 10.0
        TFALL(I,2) = 15.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 50.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .false.

        !  quaking aspen; from CA via WC

     case (26)
        V2T(I) = 21.8
        LEAFLF(I) = 1.0
        TFALL(I,0) = 1.0
        TFALL(I,1) = 10.0
        TFALL(I,2) = 15.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 50.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .false.

        !  black cottonwood; from CA via WC

     case (27)
        V2T(I) = 19.3
        LEAFLF(I) = 1.0
        TFALL(I,0) = 1.0
        TFALL(I,1) = 10.0
        TFALL(I,2) = 15.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 50.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .false.

        !  Oregon white oak; from CA via WC

     case (28)
        V2T(I) = 37.4 ! Or white oak
        LEAFLF(I) = 1.0
        TFALL(I,0) = 1.0
        TFALL(I,1) = 10.0
        TFALL(I,2) = 15.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 50.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .false.

        !  cherry and plum species; BC from WC

     case (29)
        V2T(I) = 29.3 ! black cherry
        LEAFLF(I) = 1.0
        TFALL(I,0) = 1.0
        TFALL(I,1) = 10.0
        TFALL(I,2) = 15.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 50.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .false.

        !  willow species; from CA via WC

     case (30)
        V2T(I) = 22.5
        LEAFLF(I) = 1.0
        TFALL(I,0) = 1.0
        TFALL(I,1) = 10.0
        TFALL(I,2) = 15.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 50.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .false.

        !  other softwoods (HM - based in part on PP-NI)

     case (31)
        V2T(I) = 26.2
        LEAFLF(I) = 4.0
        TFALL(I,0) = 2.0
        TFALL(I,1) = 5.0
        TFALL(I,2) = 5.0
        TFALL(I,3) = 10.0
        ALLDWN(I) = 30.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .true.

        !  other hardwoods; use aspen

     case (32)
        V2T(I) = 21.8
        LEAFLF(I) = 1.0
        TFALL(I,0) = 1.0
        TFALL(I,1) = 10.0
        TFALL(I,2) = 15.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 50.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        LSW(I) = .false.

     end select

     !  TIME-TO-FALL FOR OTHER CROWN CATEGORIES (NI)

     TFALL(I,4) = TFALL(I,3)
     TFALL(I,5) = TFALL(I,3)

     !  DEAD LEAF FALL CANNOT BE > LIVE

     TFALL(I,0) = MIN(TFALL(I,0), LEAFLF(I))

     !  TFALL(I,3) CANNOT BE < TFALL(I,2)

     if (TFALL(I,2) .gt. TFALL(I,3)) then
        TFALL(I,2) = TFALL(I,3)
     end if

     !  CONVERT LB/FT**3 TO TONS/FT**3

     V2T(I) = V2T(I) / 2000.0

     !       SET THE DECAY RATE CLASS (DKRCLS)

     select case (I)

        !         some pines, doug-fir, cedars
     case (1:3,5,13,14,17:19)
        DKRCLS(I) = 1

        !         lodgepole, spruce, hemlock
     case (7,8,11,12,31)
        DKRCLS(I) = 2

        !         firs, some pines, oak
     case (4,6,9,10,15,16,24,28)
        DKRCLS(I) = 3

        !         aspen, cottonwood, other hardwoods
     case (20:23,25:27,29,30,32)
        DKRCLS(I) = 4

     end select

  end do

  !  parameters for post-burn snag fall rates:

  PBSCOR = 0.0
  PBSOFT = 0.0
  PBSMAL = 0.0
  PBSIZE = 12.0
  PBTIME = 7.0

  !  PARAMETERS FOR FUEL MODEL SELECTION

  !  THE DOUGLAS-FIR COVER TYPE METAGROUP **FMCFMD**

  OLDICT = 1

  !  DROUGHT START AND END YEARS

  IDRYB = 0
  IDRYE = 0

  !  CRITICAL % CHANGE REQUIRED TO TRIGGER ACTIVITY FUELS

  SLCRIT = 10.0

  return
end
