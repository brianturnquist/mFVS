!=======================!
!   fire/ls : fmvinit   !
!=======================!
! FIRE-LS $Id$
subroutine FMVINIT
  implicit none

  !  PURPOSE:
  !      INITIALIZE VARIANT-SPECIFIC VARIABLES FOR THE FIRE MODEL

  !  CALLED FROM: FMINIT

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'CONTRL.f90'
  include 'FMCOM.f90'
  include 'FMFCOM.f90'

  ! Variables
  integer :: I,J,TFALLCLS(MAXSP),SNAGCLS(MAXSP)

  LVWEST = .false. ! EASTERN VARIANT

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

  PREWND(1) = 25.
  PREWND(2) = 15.
  POTEMP(1) = 80.
  POTEMP(2) = 50.

  !  DECAY RATES - FROM ABBOTT AND CROSSLEY, ECOLOGY, 1982
  !                     ALBAN AND PASTOR, CAN J FOR RES, 1993
  !                     TYRRELL AND CROW, CAN J FOR RES, 1994

  DKR(1,1) = 0.11
  DKR(2,1) = 0.11
  DKR(3,1) = 0.09
  DKR(4,1) = 0.06
  DKR(5,1) = 0.06
  DKR(6,1) = 0.02
  DKR(7,1) = 0.02
  DKR(8,1) = 0.02
  DKR(9,1) = 0.02

  do I = 1,9
     do J = 2,4
        DKR(I,J) = DKR(I,1)
     end do
  end do

  !  LITTER LOSS/YR (10) AND DUFF LOSS/YR (11)
  !  FOR LITTER DECAY - PERALA AND ALBAN SUGGESTS 0.18, BUT THIS INCLUDES
  !  TWIGS, SO USED INFO FROM MELILLO (NH) INSTEAD.  THIS IS CONSISTENT WITH
  !  DISCUSSIONS WITH COELI HOOVER.

  do J = 1,4
     DKR(10,J) = 0.31
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

  NZERO = 0.01
  LIMBRK = 0.01
  HTXSFT = 2.0
  HTR1 = 0.1 ! based on LS workshop info
  HTR2 = 0.01
  do I = 1,MAXSP
     PSOFT(I) = 0.0
  end do

  !  ** SPECIES-LEVEL VARIABLE ASSIGNMENT **

  !  V2T() - DERIVED BY USING THE SPECIFIC GRAVITY VALUES IN TABLE 4-3A IN
  !  THE 'WOOD HANDBOOK' USDA FOREST PRODUCTS LAB. 1999.  FPL-GTR-113.

  !  LEAFLF() - LIFETIME (YRS) OF LEAVES/NEEDLES
  !  LEAFLF WAS SET WITH INFO FROM HARLOW AND HARRAH'S TEXTBOOK
  !  OF DENDROLOGY, 9TH EDITION (EASTERN REDCEDAR AND NORTHERN WHITE CEDAR
  !  FOUND IN MICHIGAN TREES, BARNES AND WAGNER)

  !  TFALL() - TIME TO FALL FOR CROWN COMPONENTS;
  !  CATEGORIES ARE
  !  0 -  FOLIAGE
  !  1 -   0 - 0.25 IN
  !  2 -   0.25 - 1.00 IN
  !  3 -   1.00 - 3.00 IN
  !  4 -   3.00 - 6.00 IN
  !  5 -   6.00 - 12.00 IN

  !  TFALL IS ASSIGNED FOR 4 DIFFERENT TFALLCLS GROUPS.  THIS INFO IS FROM
  !  THE SN VARIANT, SINCE WE FORGOT TO DISCUSS IT AT THE LS WORKSHOP.  SOME
  !  CHANGES WERE MADE BASED ON THE RELATIVE FALL RATES OF VARIOUS SPECIES
  !  AND GENERAL LS WORKSHOP NOTES.

  !  THE SNAG VARIABLES DESCRIBED BELOW ARE SET BY FIRST DEFINING A
  !  SNAGCLS (1 - 6) FOR EACH SPECIES, AND THEN SETTING THE
  !  VARIABLES FOR THE 6 SNAG CLASSES.  SNAGCLS IS DEFINED AS FOLLOWS:
  !  1 - ASPEN, BIRCH, SPRUCE, FIR, POPLAR, BASSWOOD (FASTEST FALLERS)
  !  2 - JACK PINE
  !  3 - WHITE PINE
  !  4 - RED PINE
  !  5 - ASH, MAPLE, BEECH, ELM
  !  6 - CEDAR, TAMARACK, OAK, HICKORY, HEMLOCK (SLOWEST FALLERS)
  !  SPECIES WERE PUT IN SNAGCLS 5 BY DEFAULT
  !  SNAGCLS IS USED TO SET THE FOLLOWING VARIABLES:
  !  ALLDWN() - YEARS DEAD AT WHICH ALL SNAGS WILL BE FALLEN
  !  DECAYX() - DECAY RATE MULTIPLIER
  !  FALLX()  - FALL RATE MULTIPLIER
  !  HTX()    - HEIGHT-LOSS RATE MULTIPLIER

  !  DKRCLS() - DECAY RATE CLASS 1 (V.SLOW) TO 4 (FAST). MODEL USERS
  !  CAN USE THE FUELDCAY KEYWORD TO REASSIGN RATES WITHIN THE 4
  !  CLASSES, AND THE FUELPOOL KEYWORD TO REASSIGN CLASS

  !  DKRCLS WAS ASSIGNED FROM INFO IN THE WOOD HANDBOOK:
  !  HTTP://WWW.FPL.FS.FED.US/DOCUMNTS/FPLGTR/FPLGTR113/FPLGTR113.HTM
  !  WHEN SPECIES WERE CLASSIFIED DIFFERENTLY FOR YOUNG OR OLD GROWTH,
  !  YOUNG GROWTH WAS ASSUMED.
  !  SPECIES NOT LISTED WERE CLASSED AS 4 IF NOT IN
  !  WOOD HANDBOOK

  do I = 1,MAXSP

     select case (I)

        !  JACK PINE

     case (1)
        V2T(I) = 24.9
        TFALLCLS(I) = 4
        LEAFLF(I) = 2.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  SCOTCH PINE

     case (2)
        V2T(I) = 25.6
        TFALLCLS(I) = 4
        LEAFLF(I) = 3.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 4

        !  RED PINE (NATURAL)

     case (3)
        V2T(I) = 25.6
        TFALLCLS(I) = 4
        LEAFLF(I) = 4.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 4

        !  RED PINE PLANTATION

     case (4)
        V2T(I) = 25.6
        TFALLCLS(I) = 4
        LEAFLF(I) = 4.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 4

        !  EASTERN WHITE PINE

     case (5)
        V2T(I) = 21.2
        TFALLCLS(I) = 4
        LEAFLF(I) = 2.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 3

        !  WHITE SPRUCE

     case (6)
        V2T(I) = 23.1
        TFALLCLS(I) = 4
        LEAFLF(I) = 8.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  NORWAY SPRUCE

     case (7)
        V2T(I) = 23.1
        TFALLCLS(I) = 4
        LEAFLF(I) = 8.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  BALSAM FIR

     case (8)
        V2T(I) = 20.6
        TFALLCLS(I) = 4
        LEAFLF(I) = 8.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  BLACK SPRUCE

     case (9)
        V2T(I) = 23.7
        TFALLCLS(I) = 4
        LEAFLF(I) = 8.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  TAMARACK

     case (10)
        V2T(I) = 30.6
        TFALLCLS(I) = 1
        LEAFLF(I) = 1.0
        DKRCLS(I) = 3
        SNAGCLS(I) = 6

        !  NORTHERN WHITE-CEDAR

     case (11)
        V2T(I) = 18.1
        TFALLCLS(I) = 1
        LEAFLF(I) = 2.0
        DKRCLS(I) = 2
        SNAGCLS(I) = 6

        !  EASTERN HEMLOCK

     case (12)
        V2T(I) = 23.7
        TFALLCLS(I) = 1
        LEAFLF(I) = 3.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 6

        !  OTHER SOFTWOOD

     case (13)
        V2T(I) = 27.4
        TFALLCLS(I) = 1
        LEAFLF(I) = 5.0 !used eastern redcedar
        DKRCLS(I) = 2 !used eastern redcedar
        SNAGCLS(I) = 6 !used eastern redcedar

        !  EASTERN REDCEDAR

     case (14)
        V2T(I) = 27.4
        TFALLCLS(I) = 1
        LEAFLF(I) = 5.0
        DKRCLS(I) = 2
        SNAGCLS(I) = 6

        !  BLACK ASH

     case (15)
        V2T(I) = 28.1
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 5

        !  GREEN ASH

     case (16)
        V2T(I) = 33.1
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 5

        !  EASTERN COTTONWOOD

     case (17)
        V2T(I) = 23.1
        TFALLCLS(I) = 4
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  SILVER MAPLE

     case (18)
        V2T(I) = 27.4
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 5

        !  RED MAPLE

     case (19)
        V2T(I) = 30.6
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 5

        !  BLACK CHERRY

     case (20)
        V2T(I) = 29.3
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 2
        SNAGCLS(I) = 5

        !  AMERICAN ELM

     case (21)
        V2T(I) = 28.7
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 5

        !  SLIPPERY ELM

     case (22)
        V2T(I) = 29.9
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 5

        !  ROCK ELM

     case (23)
        V2T(I) = 35.6
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 5

        !  YELLOW BIRCH

     case (24)
        V2T(I) = 34.3
        TFALLCLS(I) = 4
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  AMERICAN BASSWOOD

     case (25)
        V2T(I) = 20.0
        TFALLCLS(I) = 4
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  SUGAR MAPLE

     case (26)
        V2T(I) = 34.9
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 5

        !  BLACK MAPLE

     case (27)
        V2T(I) = 32.4
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 5

        !  AMERICAN BEECH

     case (28)
        V2T(I) = 34.9
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 5

        !  WHITE ASH

     case (29)
        V2T(I) = 34.3
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 5

        !  WHITE OAK

     case (30)
        V2T(I) = 37.4
        TFALLCLS(I) = 1
        LEAFLF(I) = 1.0
        DKRCLS(I) = 2
        SNAGCLS(I) = 6

        !  SWAMP WHITE OAK

     case (31)
        V2T(I) = 39.9
        TFALLCLS(I) = 1
        LEAFLF(I) = 1.0
        DKRCLS(I) = 2
        SNAGCLS(I) = 6

        !  BUR OAK

     case (32)
        V2T(I) = 36.2
        TFALLCLS(I) = 1
        LEAFLF(I) = 1.0
        DKRCLS(I) = 2
        SNAGCLS(I) = 6

        !  CHINKAPIN OAK

     case (33)
        V2T(I) = 37.4
        TFALLCLS(I) = 1
        LEAFLF(I) = 1.0
        DKRCLS(I) = 2
        SNAGCLS(I) = 6

        !  NORTHERN RED OAK

     case (34)
        V2T(I) = 34.9
        TFALLCLS(I) = 2
        LEAFLF(I) = 1.0
        DKRCLS(I) = 2 ! used white oak
        SNAGCLS(I) = 6

        !  BLACK OAK

     case (35)
        V2T(I) = 34.9
        TFALLCLS(I) = 2
        LEAFLF(I) = 1.0
        DKRCLS(I) = 2 ! used white oak
        SNAGCLS(I) = 6

        !  NORTHERN PIN OAK

     case (36)
        V2T(I) = 36.2
        TFALLCLS(I) = 2
        LEAFLF(I) = 1.0
        DKRCLS(I) = 2 ! used white oak
        SNAGCLS(I) = 6

        !  BITTERNUT HICKORY

     case (37)
        V2T(I) = 37.4
        TFALLCLS(I) = 1
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 6

        !  PIGNUT HICKORY

     case (38)
        V2T(I) = 41.2
        TFALLCLS(I) = 1
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 6

        !  SHAGBARK HICKORY

     case (39)
        V2T(I) = 39.9
        TFALLCLS(I) = 1
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 6

        !  BIGTOOTH ASPEN

     case (40)
        V2T(I) = 22.5
        TFALLCLS(I) = 4
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  QUAKING ASPEN

     case (41)
        V2T(I) = 21.8
        TFALLCLS(I) = 4
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  BALSAM POPLAR

     case (42)
        V2T(I) = 19.3
        TFALLCLS(I) = 4
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  PAPER BIRCH

     case (43)
        V2T(I) = 29.9
        TFALLCLS(I) = 4
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  COMMERICAL HARDWOOD

     case (44)
        V2T(I) = 31.8
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 5

        !  BUTTERNUT

     case (45)
        V2T(I) = 22.5
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 5

        !  BLACK WALNUT

     case (46)
        V2T(I) = 31.8
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 2
        SNAGCLS(I) = 5

        !  EASTERN HOPHORNBEAM

     case (47)
        V2T(I) = 31.8
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 5

        !  BLACK LOCUST

     case (48)
        V2T(I) = 41.2
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 1
        SNAGCLS(I) = 5

        !  NONCOMMERCIAL HARDWOOD

     case (49)
        V2T(I) = 28.7
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 5

        !  BOXELDER

     case (50)
        V2T(I) = 30.6
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 5

        !  STRIPED MAPLE

     case (51)
        V2T(I) = 30.6
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 5

        !  MOUNTAIN MAPLE

     case (52)
        V2T(I) = 30.6
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 5

        !  AMERICAN HORNBEAM / MUSCLEWOOD

     case (53)
        V2T(I) = 28.7
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 5

        !  AMERICAN CHESTNUT

     case (54)
        V2T(I) = 28.7
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 2
        SNAGCLS(I) = 5

        !  HACKBERRY

     case (55)
        V2T(I) = 30.6
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 5

        !  FLOWERING DOGWOOD

     case (56)
        V2T(I) = 28.7
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 5

        !  HAWTHORN

     case (57)
        V2T(I) = 28.7
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 5

        !  APPLE

     case (58)
        V2T(I) = 29.3
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 2 ! used black cherry
        SNAGCLS(I) = 5

        !  BLACKGUM

     case (59)
        V2T(I) = 28.7
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 5

        !  AMERICAN SYCAMORE

     case (60)
        V2T(I) = 28.7
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 5

        !  PIN CHERRY

     case (61)
        V2T(I) = 29.3
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 2 ! used black cherry
        SNAGCLS(I) = 5

        !  COMMON CHOKECHERRY

     case (62)
        V2T(I) = 29.3
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 2 ! used black cherry
        SNAGCLS(I) = 5

        !  CHERRY AND PLUM SPECIES

     case (63)
        V2T(I) = 29.3
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 2 ! used black cherry
        SNAGCLS(I) = 5

        !  WILLOW

     case (64)
        V2T(I) = 22.5
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 5

        !  BLACK WILLOW

     case (65)
        V2T(I) = 22.5
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 5

        !  DIAMOND WILLOW

     case (66)
        V2T(I) = 22.5
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 5

        !  SASSAFRAS

     case (67)
        V2T(I) = 26.2
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 2
        SNAGCLS(I) = 5

        !  AMERICAN MOUNTAIN ASH

     case (68)
        V2T(I) = 28.7
        TFALLCLS(I) = 3
        LEAFLF(I) = 1.0
        DKRCLS(I) = 4
        SNAGCLS(I) = 5

     end select

     select case (I)
     case (1:14)
        LSW(I) = .true.
     case default
        LSW(I) = .false.
     end select

     select case (TFALLCLS(I))
     case (1) ! hickory/white oak/hemlock/cedar/tamarack group
        TFALL(I,1) = 2.0
        TFALL(I,3) = 5.0
        TFALL(I,4) = 10.0

     case (2) ! red oak group
        TFALL(I,1) = 1.0
        TFALL(I,3) = 4.0
        TFALL(I,4) = 8.0

     case (3) ! ash/elm/maple/beech/other group
        TFALL(I,1) = 1.0
        TFALL(I,3) = 3.0
        TFALL(I,4) = 6.0

     case (4) ! pine/spruce/fir/aspen/poplar/birch/basswood
        TFALL(I,1) = 1.0
        TFALL(I,3) = 2.0
        TFALL(I,4) = 4.0

     end select

     TFALL(I,0) = 1.0
     TFALL(I,2) = TFALL(I,1)
     TFALL(I,5) = TFALL(I,4)

     select case (SNAGCLS(I))

     case (1) ! aspen, birch, spruce, fir, poplar, basswood
        ! this group decays and falls really fast
        DECAYX(I) = 0.4
        FALLX(I) = 1.66
        ALLDWN(I) = 10
        do J = 1,4
           HTX(I,J) = 3.0
        end do

     case (2) ! jack pine
        DECAYX(I) = 0.8
        FALLX(I) = 1.33
        ALLDWN(I) = 30
        do J = 1,4
           HTX(I,J) = 1.0
        end do

     case (3) ! white pine
        DECAYX(I) = 1.0
        FALLX(I) = 1.16
        ALLDWN(I) = 50
        do J = 1,4
           HTX(I,J) = 0.0 ! no height loss
        end do

     case (4) ! red pine
        DECAYX(I) = 1.2
        FALLX(I) = 1.0
        ALLDWN(I) = 50
        do J = 1,4
           HTX(I,J) = 0.0 ! no height loss
        end do

     case (5) ! ash, maple, beech, elm
        DECAYX(I) = 1.5
        FALLX(I) = .83
        ALLDWN(I) = 50
        do J = 1,4
           HTX(I,J) = 0.65
        end do

     case (6) ! cedar, tamarack, oak, hickory, hemlock
        ! this group decays and falls very slowly
        DECAYX(I) = 2.3
        FALLX(I) = .53
        ALLDWN(I) = 50
        do J = 1,4
           HTX(I,J) = 0.45
           if (I .eq.12) HTX(I,J) = 0.0 ! hemlock gets no ht loss
        end do
     end select

     !  CONVERT LB/FT**3 TO TONS/FT**3

     V2T(I) = V2T(I) / 2000.0

  end do

  !  PARAMETERS FOR POST-BURN SNAG FALL RATES

  PBSCOR = 0.0
  PBSOFT = 1.0
  PBSMAL = 0.9
  PBSIZE = 12.0
  PBTIME = 7.0 !may need to make this shorter??

  !  PARAMETERS FOR FUEL MODEL SELECTION
  !  COVER TYPE

  OLDICT = 3

  !  DROUGHT START AND END YEARS

  IDRYB = 0
  IDRYE = 0

  !  CRITICAL % CHANGE REQUIERED TO TRIGGER ACTIVITY FUELS

  SLCRIT = 10.0

  return
end
