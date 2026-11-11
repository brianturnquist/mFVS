!=======================!
!   fire/ne : fmvinit   !
!=======================!
! FIRE-NE $Id$
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

  !  DECAY RATES - FROM FOSTER AND LANG, CAN J FOR RES, 1982
  !                     ARTHUR, TRITTON, AND FAHEY, CAN J FOR RES, 1993
  !                     FAHEY, HUGHES, PU, AND ARTHUR, FOREST SCIENCE, 1988
  !  THIS IS BASED ON DISCUSSIONS WITH COELI HOOVER.

  DKR(1,1) = 0.19 ! Fahey et. al.
  DKR(2,1) = 0.19 ! Fahey et. al.
  DKR(3,1) = 0.11 ! Arthur et. al. and Fahey et. al.
  DKR(4,1) = 0.07 ! gradient
  DKR(5,1) = 0.03 ! Foster and Lang
  DKR(6,1) = 0.03 ! Foster and Lang
  DKR(7,1) = 0.03 ! Foster and Lang
  DKR(8,1) = 0.03 ! Foster and Lang
  DKR(9,1) = 0.03 ! Foster and Lang

  do I = 1,9
     do J = 2,4
        DKR(I,J) = DKR(I,1)
     end do
  end do

  !  LITTER LOSS/YR (10) AND DUFF LOSS/YR (11)
  !  FOR LITTER DECAY - USED INFO FROM MELILLO, ECOLOGY, 1982.  THIS IS
  !  BASED ON DISCUSSIONS WITH COELI HOOVER.
  !  *** MAY 2006 - I BUMPED UP THE LITTER DECAY RATE SINCE TOO MUCH LITTER WAS
  !  ACCUMULATING OVER TIME - SAR

  do J = 1,4
     DKR(10,J) = 0.40
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
  HTR1 = 0.015 ! ht loss info from Coeli Hoover & Linda Heath
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

  !  TFALL IS ASSIGNED FOR 6 DIFFERENT TFALLCLS GROUPS.  THIS INFO IS BASED
  !  ON THE SN AND LS VARIANTS, SINCE I FORGOT TO DISCUSS IT AT THE MEETING
  !  WITH COELI HOOVER AND LINDA HEATH.

  !  THE SNAG DECAY RATE IS SET BY FIRST DEFINING A
  !  SNAGCLS (1, 2, OR 3) FOR EACH SPECIES, AND THEN SETTING THE DECAY RATE
  !  VARIABLE FOR THE 3 SNAG CLASSES.  SNAGCLS IS DEFINED AS FOLLOWS:
  !  1 - SPECIES DECAYS FASTER THAN AVERAGE
  !  2 - SPECIES DECAYS AT AN AVERAGE RATE
  !  3 - SPECIES DECAYS SLOWER THAN AVERAGE
  !  SNAGCLS IS USED TO SET THE FOLLOWING VARIABLE:
  !  DECAYX() - DECAY RATE MULTIPLIER
  !  THESE CLASSES AND THE DECAY RATES WERE TAKEN FROM THE SN-FFE (WITH SOME
  !  MODIFICATION) BECAUSE LINDA THOUGHT THEY SEEMED REASONABLE.

  !  THE FOLLOWING VARIABLES ARE ALSO SET, BUT ARE NOT SPECIES-SPECIFIC
  !  ALLDWN() - YEARS DEAD AT WHICH ALL SNAGS WILL BE FALLEN
  !  FALLX()  - FALL RATE MULTIPLIER
  !  HTX()    - HEIGHT-LOSS RATE MULTIPLIER
  !  THESE WERE BASED ON DISCUSSIONS WITH COELI HOOVER AND LINDA
  !  HEATH AND A PAPER BY YAMASAKI AND LEAK (IN PRESS, NJAF)

  !  DKRCLS() - DECAY RATE CLASS 1 (V.SLOW) TO 4 (FAST). MODEL USERS
  !  CAN USE THE FUELDCAY KEYWORD TO REASSIGN RATES WITHIN THE 4
  !  CLASSES, AND THE FUELPOOL KEYWORD TO REASSIGN CLASS

  !  DKRCLS WAS ASSIGNED FROM INFO IN THE WOOD HANDBOOK:
  !  HTTP://WWW.FPL.FS.FED.US/DOCUMNTS/FPLGTR/FPLGTR113/FPLGTR113.HTM
  !  WHEN SPECIES WERE CLASSIFIED DIFFERENTLY FOR YOUNG OR OLD GROWTH,
  !  YOUNG GROWTH WAS ASSUMED.
  !  SPECIES NOT LISTED WERE CLASSED AS 4 IF NOT IN THE WOOD HANDBOOK

  do I = 1,MAXSP

     select case (I)

        !  BALSAM FIR

     case (1)
        V2T(I) = 20.6
        TFALLCLS(I) = 6
        LEAFLF(I) = 8
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  TAMARACK

     case (2)
        V2T(I) = 30.6
        TFALLCLS(I) = 1
        LEAFLF(I) = 1
        DKRCLS(I) = 3
        SNAGCLS(I) = 3

        !  WHITE SPRUCE

     case (3)
        V2T(I) = 23.1
        TFALLCLS(I) = 6
        LEAFLF(I) = 8
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  RED SPRUCE

     case (4)
        V2T(I) = 23.1
        TFALLCLS(I) = 6
        LEAFLF(I) = 8
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  NORWAY SPRUCE

     case (5)
        V2T(I) = 23.1
        TFALLCLS(I) = 6
        LEAFLF(I) = 8
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  BLACK SPRUCE

     case (6)
        V2T(I) = 23.7
        TFALLCLS(I) = 6
        LEAFLF(I) = 8
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  OTHER SPRUCE

     case (7)
        V2T(I) = 23.1
        TFALLCLS(I) = 6
        LEAFLF(I) = 8
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  RED PINE

     case (8)
        V2T(I) = 25.6
        TFALLCLS(I) = 6
        LEAFLF(I) = 4
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  EASTERN WHITE PINE

     case (9)
        V2T(I) = 21.2
        TFALLCLS(I) = 6
        LEAFLF(I) = 2
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  LOBLOLLY PINE

     case (10)
        V2T(I) = 29.3
        TFALLCLS(I) = 6
        LEAFLF(I) = 3
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  VIRGINIA PINE

     case (11)
        V2T(I) = 28.1
        TFALLCLS(I) = 6
        LEAFLF(I) = 3
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  NORTHERN WHITE CEDAR

     case (12)
        V2T(I) = 18.1
        TFALLCLS(I) = 1
        LEAFLF(I) = 2
        DKRCLS(I) = 2
        SNAGCLS(I) = 3

        !  ATLANTIC WHITE CEDAR

     case (13)
        V2T(I) = 19.3
        TFALLCLS(I) = 1
        LEAFLF(I) = 3
        DKRCLS(I) = 2
        SNAGCLS(I) = 3

        !  EASTERN REDCEDAR

     case (14)
        V2T(I) = 27.4
        TFALLCLS(I) = 1
        LEAFLF(I) = 5
        DKRCLS(I) = 2
        SNAGCLS(I) = 3

        !  JUNIPER SPECIES

     case (15)
        V2T(I) = 27.4
        TFALLCLS(I) = 1
        LEAFLF(I) = 5
        DKRCLS(I) = 2
        SNAGCLS(I) = 3

        !  EASTERN HEMLOCK

     case (16)
        V2T(I) = 23.7
        TFALLCLS(I) = 3
        LEAFLF(I) = 3
        DKRCLS(I) = 4
        SNAGCLS(I) = 3

        !  HEMLOCK SPECIES

     case (17)
        V2T(I) = 26.2
        TFALLCLS(I) = 3
        LEAFLF(I) = 3
        DKRCLS(I) = 4
        SNAGCLS(I) = 3

        !  OTHER PINE

     case (18)
        V2T(I) = 25.6
        TFALLCLS(I) = 6
        LEAFLF(I) = 2
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  JACK PINE

     case (19)
        V2T(I) = 24.9
        TFALLCLS(I) = 6
        LEAFLF(I) = 2
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  SHORTLEAF PINE

     case (20)
        V2T(I) = 29.3
        TFALLCLS(I) = 6
        LEAFLF(I) = 4
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        ! TABLE MOUNTAIN PINE

     case (21)
        V2T(I) = 28.1
        TFALLCLS(I) = 6
        LEAFLF(I) = 3
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  PITCH PINE

     case (22)
        V2T(I) = 29.3
        TFALLCLS(I) = 6
        LEAFLF(I) = 2
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  POND PINE

     case (23)
        V2T(I) = 31.8
        TFALLCLS(I) = 6
        LEAFLF(I) = 2
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  SCOTCH PINE

     case (24)
        V2T(I) = 25.6
        TFALLCLS(I) = 6
        LEAFLF(I) = 3
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  OTHER SOFTWOOD SPECIES

     case (25)
        V2T(I) = 25.6
        TFALLCLS(I) = 6
        LEAFLF(I) = 2
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  RED MAPLE

     case (26)
        V2T(I) = 30.6
        TFALLCLS(I) = 5
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  SUGAR MAPLE

     case (27)
        V2T(I) = 34.9
        TFALLCLS(I) = 5
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  BLACK MAPLE

     case (28)
        V2T(I) = 32.4
        TFALLCLS(I) = 5
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  SILVER MAPLE

     case (29)
        V2T(I) = 27.4
        TFALLCLS(I) = 5
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  YELLOW BIRCH

     case (30)
        V2T(I) = 34.3
        TFALLCLS(I) = 6
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  SWEET BIRCH

     case (31)
        V2T(I) = 37.4
        TFALLCLS(I) = 6
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  RIVER BIRCH

     case (32)
        V2T(I) = 29.9
        TFALLCLS(I) = 6
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  PAPER BIRCH

     case (33)
        V2T(I) = 29.9
        TFALLCLS(I) = 6
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  GRAY BIRCH

     case (34)
        V2T(I) = 29.9
        TFALLCLS(I) = 6
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  HICKORY SPECIES

     case (35)
        V2T(I) = 39.9
        TFALLCLS(I) = 2
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 3

        !  PIGNUT HICKORY

     case (36)
        V2T(I) = 41.2
        TFALLCLS(I) = 2
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 3

        !  SHELLBARK HICKORY

     case (37)
        V2T(I) = 38.7
        TFALLCLS(I) = 2
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 3

        !  SHAGBARK HICKORY

     case (38)
        V2T(I) = 39.9
        TFALLCLS(I) = 2
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 3

        !  MOCKERNUT HICKORY

     case (39)
        V2T(I) = 39.9
        TFALLCLS(I) = 2
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 3

        !  AMERICAN BEECH

     case (40)
        V2T(I) = 34.9
        TFALLCLS(I) = 4
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  ASH SPECIES

     case (41)
        V2T(I) = 33.1
        TFALLCLS(I) = 5
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  WHITE ASH

     case (42)
        V2T(I) = 34.3
        TFALLCLS(I) = 5
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  BLACK ASH

     case (43)
        V2T(I) = 28.1
        TFALLCLS(I) = 5
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  GREEN ASH

     case (44)
        V2T(I) = 33.1
        TFALLCLS(I) = 5
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  PUMPKIN ASH

     case (45)
        V2T(I) = 33.1
        TFALLCLS(I) = 5
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  YELLOW-POPLAR

     case (46)
        V2T(I) = 24.9
        TFALLCLS(I) = 4
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  SWEETGUM

     case (47)
        V2T(I) = 28.7
        TFALLCLS(I) = 5
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  CUCUMBERTREE

     case (48)
        V2T(I) = 27.4
        TFALLCLS(I) = 4
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  QUAKING ASPEN

     case (49)
        V2T(I) = 21.8
        TFALLCLS(I) = 6
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  BALSAM POPLAR

     case (50)
        V2T(I) = 19.3
        TFALLCLS(I) = 6
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  EASTERN COTTONWOOD

     case (51)
        V2T(I) = 23.1
        TFALLCLS(I) = 6
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  BIGTOOTH ASPEN

     case (52)
        V2T(I) = 22.5
        TFALLCLS(I) = 6
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  SWAMP COTTONWOOD

     case (53)
        V2T(I) = 23.1
        TFALLCLS(I) = 6
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  BLACK CHERRY

     case (54)
        V2T(I) = 29.3
        TFALLCLS(I) = 4
        LEAFLF(I) = 1
        DKRCLS(I) = 2
        SNAGCLS(I) = 2

        !  WHITE OAK

     case (55)
        V2T(I) = 37.4
        TFALLCLS(I) = 3
        LEAFLF(I) = 1
        DKRCLS(I) = 2
        SNAGCLS(I) = 3

        !  BUR OAK

     case (56)
        V2T(I) = 36.2
        TFALLCLS(I) = 3
        LEAFLF(I) = 1
        DKRCLS(I) = 2
        SNAGCLS(I) = 3

        !  CHINKAPIN OAK

     case (57)
        V2T(I) = 37.4
        TFALLCLS(I) = 3
        LEAFLF(I) = 1
        DKRCLS(I) = 2
        SNAGCLS(I) = 3

        !  POST OAK

     case (58)
        V2T(I) = 37.4
        TFALLCLS(I) = 3
        LEAFLF(I) = 1
        DKRCLS(I) = 2
        SNAGCLS(I) = 3

        !  OAK SPECIES

     case (59)
        V2T(I) = 37.4
        TFALLCLS(I) = 3
        LEAFLF(I) = 1
        DKRCLS(I) = 2
        SNAGCLS(I) = 3

        !  SCARLET OAK

     case (60)
        V2T(I) = 37.4
        TFALLCLS(I) = 4
        LEAFLF(I) = 1
        DKRCLS(I) = 2
        SNAGCLS(I) = 2

        !  SHINGLE OAK

     case (61)
        V2T(I) = 34.9
        TFALLCLS(I) = 4
        LEAFLF(I) = 1
        DKRCLS(I) = 2
        SNAGCLS(I) = 2

        !  WATER OAK

     case (62)
        V2T(I) = 34.9
        TFALLCLS(I) = 4
        LEAFLF(I) = 1
        DKRCLS(I) = 2
        SNAGCLS(I) = 2

        !  NORTHERN PIN OAK

     case (63)
        V2T(I) = 36.2
        TFALLCLS(I) = 4
        LEAFLF(I) = 1
        DKRCLS(I) = 2
        SNAGCLS(I) = 2

        !  CHESTNUT OAK

     case (64)
        V2T(I) = 35.6
        TFALLCLS(I) = 3
        LEAFLF(I) = 1
        DKRCLS(I) = 2
        SNAGCLS(I) = 3

        !  SWAMP WHITE OAK

     case (65)
        V2T(I) = 39.9
        TFALLCLS(I) = 3
        LEAFLF(I) = 1
        DKRCLS(I) = 2
        SNAGCLS(I) = 3

        !  SWAMP CHESTNUT OAK

     case (66)
        V2T(I) = 37.4
        TFALLCLS(I) = 3
        LEAFLF(I) = 1
        DKRCLS(I) = 2
        SNAGCLS(I) = 3

        !  NORTHERN RED OAK

     case (67)
        V2T(I) = 34.9
        TFALLCLS(I) = 4
        LEAFLF(I) = 1
        DKRCLS(I) = 2
        SNAGCLS(I) = 2

        !  SOUTHERN RED OAK

     case (68)
        V2T(I) = 32.4
        TFALLCLS(I) = 4
        LEAFLF(I) = 1
        DKRCLS(I) = 2
        SNAGCLS(I) = 2

        !  BLACK OAK

     case (69)
        V2T(I) = 34.9
        TFALLCLS(I) = 4
        LEAFLF(I) = 1
        DKRCLS(I) = 2
        SNAGCLS(I) = 2

        !  CHERRYBARK OAK

     case (70)
        V2T(I) = 38.0
        TFALLCLS(I) = 4
        LEAFLF(I) = 1
        DKRCLS(I) = 2
        SNAGCLS(I) = 2

        !  OTHER HARDWOODS

     case (71)
        V2T(I) = 28.7
        TFALLCLS(I) = 5
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  BUCKEYE SPECIES

     case (72)
        V2T(I) = 28.7
        TFALLCLS(I) = 5
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  YELLOW BUCKEYE

     case (73)
        V2T(I) = 28.7
        TFALLCLS(I) = 5
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  WATER BIRCH

     case (74)
        V2T(I) = 29.9
        TFALLCLS(I) = 6
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  COMMON HACKBERRY

     case (75)
        V2T(I) = 30.6
        TFALLCLS(I) = 4
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  COMMON PERSIMMON

     case (76)
        V2T(I) = 28.7
        TFALLCLS(I) = 4
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 3

        !  AMERICAN HOLLY

     case (77)
        V2T(I) = 28.7
        TFALLCLS(I) = 4
        LEAFLF(I) = 3
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  BUTTERNUT

     case (78)
        V2T(I) = 22.5
        TFALLCLS(I) = 4
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  BLACK WALNUT

     case (79)
        V2T(I) = 31.8
        TFALLCLS(I) = 4
        LEAFLF(I) = 1
        DKRCLS(I) = 2
        SNAGCLS(I) = 2

        !  OSAGE-ORANGE

     case (80)
        V2T(I) = 28.7
        TFALLCLS(I) = 5
        LEAFLF(I) = 1
        DKRCLS(I) = 1
        SNAGCLS(I) = 2

        !  MAGNOLIA

     case (81)
        V2T(I) = 28.7
        TFALLCLS(I) = 4
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  SWEETBAY

     case (82)
        V2T(I) = 28.7
        TFALLCLS(I) = 4
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  APPLE SPECIES

     case (83)
        V2T(I) = 29.3
        TFALLCLS(I) = 4
        LEAFLF(I) = 1
        DKRCLS(I) = 2
        SNAGCLS(I) = 2

        !  WATER TUPELO

     case (84)
        V2T(I) = 28.7
        TFALLCLS(I) = 3
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 3

        !  BLACK GUM

     case (85)
        V2T(I) = 28.7
        TFALLCLS(I) = 3
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 3

        !  SOURWOOD

     case (86)
        V2T(I) = 28.7
        TFALLCLS(I) = 5
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  PAULOWNIA

     case (87)
        V2T(I) = 28.7
        TFALLCLS(I) = 5
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  SYCAMORE

     case (88)
        V2T(I) = 28.7
        TFALLCLS(I) = 5
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  WILLOW OAK

     case (89)
        V2T(I) = 34.9
        TFALLCLS(I) = 4
        LEAFLF(I) = 1
        DKRCLS(I) = 2
        SNAGCLS(I) = 2

        !  BLACK LOCUST

     case (90)
        V2T(I) = 41.2
        TFALLCLS(I) = 2
        LEAFLF(I) = 1
        DKRCLS(I) = 1
        SNAGCLS(I) = 3

        !  BLACK WILLOW

     case (91)
        V2T(I) = 22.5
        TFALLCLS(I) = 6
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  SASSAFRAS

     case (92)
        V2T(I) = 26.2
        TFALLCLS(I) = 4
        LEAFLF(I) = 1
        DKRCLS(I) = 2
        SNAGCLS(I) = 2

        !  AMERICAN BASSWOOD

     case (93)
        V2T(I) = 20.0
        TFALLCLS(I) = 6
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  WHITE BASSWOOD

     case (94)
        V2T(I) = 20.0
        TFALLCLS(I) = 6
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  ELM SPECIES

     case (95)
        V2T(I) = 28.7
        TFALLCLS(I) = 5
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  AMERICAN ELM

     case (96)
        V2T(I) = 28.7
        TFALLCLS(I) = 5
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  SLIPPERY ELM

     case (97)
        V2T(I) = 29.9
        TFALLCLS(I) = 5
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  NON-COMMERCIAL HARDWOODS

     case (98)
        V2T(I) = 28.7
        TFALLCLS(I) = 5
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  BOX ELDER

     case (99)
        V2T(I) = 30.6
        TFALLCLS(I) = 5
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  STRIPED MAPLE

     case (100)
        V2T(I) = 30.6
        TFALLCLS(I) = 5
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  TREE OF HEAVEN - AILANTHUS

     case (101)
        V2T(I) = 28.7
        TFALLCLS(I) = 5
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  SERVICEBERRY

     case (102)
        V2T(I) = 28.7
        TFALLCLS(I) = 5
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  AMERICAN HORNBEAM

     case (103)
        V2T(I) = 28.7
        TFALLCLS(I) = 4
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  FLOWERING DOGWOOD

     case (104)
        V2T(I) = 28.7
        TFALLCLS(I) = 5
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  HAWTHORN SPECIES

     case (105)
        V2T(I) = 28.7
        TFALLCLS(I) = 5
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  HOPHORNBEAM

     case (106)
        V2T(I) = 28.7
        TFALLCLS(I) = 4
        LEAFLF(I) = 1
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  PLUM SPECIES

     case (107)
        V2T(I) = 29.3
        TFALLCLS(I) = 4
        LEAFLF(I) = 1
        DKRCLS(I) = 2
        SNAGCLS(I) = 2

        !  PIN CHERRY

     case (108)
        V2T(I) = 29.3
        TFALLCLS(I) = 4
        LEAFLF(I) = 1
        DKRCLS(I) = 2
        SNAGCLS(I) = 2

     end select

     select case (I)
     case (1:25)
        LSW(I) = .true.
     case default
        LSW(I) = .false.
     end select

     if ((I .ge.12) .and. (I .le.15)) then !cedars
        TFALL(I,0) = 3.0 ! foliage
     else
        TFALL(I,0) = 1.0
     end if

     select case (TFALLCLS(I))

     case (1) ! cedar / tamarack group
        TFALL(I,1) = 5.0
        TFALL(I,3) = 10.0
        TFALL(I,4) = 25.0

     case (2) ! hickory / black locust group
        TFALL(I,1) = 3.0
        TFALL(I,3) = 6.0
        TFALL(I,4) = 12.0

     case (3) ! white oak group / hemlock / other
        TFALL(I,1) = 2.0
        TFALL(I,3) = 5.0
        TFALL(I,4) = 10.0

     case (4) ! red oak group / other
        TFALL(I,1) = 1.0
        TFALL(I,3) = 4.0
        TFALL(I,4) = 8.0

     case (5) ! ash/elm/maple/other group
        TFALL(I,1) = 1.0
        TFALL(I,3) = 3.0
        TFALL(I,4) = 6.0

     case (6) ! pine/spruce/fir/aspen/poplar/birch/basswood
        TFALL(I,1) = 1.0
        TFALL(I,3) = 2.0
        TFALL(I,4) = 4.0

     end select

     TFALL(I,2) = TFALL(I,1)
     TFALL(I,5) = TFALL(I,4)

     FALLX(I) = 1.0
     ALLDWN(I) = 50.0

     select case (SNAGCLS(I))

     case (1) ! pines (except white), fir, spruce and others
        ! this group decays faster than average
        DECAYX(I) = 0.07 ! 12 inch tree is soft in 2 years

     case (2) ! black oak and others
        ! this group decays at an average rate
        DECAYX(I) = 0.21 ! 12 inch tree is soft in 6 years

     case (3) ! white oak, redcedar, and others
        ! this group decays slower than average
        DECAYX(I) = 0.35 ! 12 inch tree is soft in 10 years
     end select

     do J = 1,4
        HTX(I,J) = 1.0 ! all species get base rate of
        ! 1.5% ht. loss/year
     end do

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
  !  COVER TYPE  - CURRENTLY NOT USED IN NE-FFE

  OLDICT = 0

  !  DROUGHT START AND END YEARS - CURRENTLY NOT USED IN NE-FFE

  IDRYB = 0
  IDRYE = 0

  !  CRITICAL % CHANGE REQUIERED TO TRIGGER ACTIVITY FUELS

  SLCRIT = 10.0

  return
end
