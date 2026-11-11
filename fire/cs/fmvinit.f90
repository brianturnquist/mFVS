!=======================!
!   fire/cs : fmvinit   !
!=======================!
! FIRE-CS $Id$
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

  PREWND(1) = 20.
  PREWND(2) = 8.
  POTEMP(1) = 70.
  POTEMP(2) = 60.

  !  DECAY RATES FROM ABBOTT AND CROSSLEY / BARBER AND VANLEAR

  DKR(1,1) = 0.11
  DKR(2,1) = 0.11
  DKR(3,1) = 0.09
  DKR(4,1) = 0.07
  DKR(5,1) = 0.07
  DKR(6,1) = 0.07
  DKR(7,1) = 0.07
  DKR(8,1) = 0.07
  DKR(9,1) = 0.07

  do I = 1,9
     do J = 2,4
        DKR(I,J) = DKR(I,1)
     end do
  end do

  !  LITTER LOSS/YR (10) AND DUFF LOSS/YR (11)
  !  LITTER RATES FROM SHARPE ET AL. AND WITKAMP

  do J = 1,4
     DKR(10,J) = 0.65
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

  !  NO SNAG HEIGHT LOSS

  NZERO = 0.01
  LIMBRK = 0.01
  HTXSFT = 2.0
  HTR1 = 0.01 !from bm fmvinit, the way to get 0 ht. loss is
  HTR2 = 0.01 !to set these low (can't be 0) and set htx to 0
  do I = 1,MAXSP
     PSOFT(I) = 0.0
  end do

  !  ** SPECIES-LEVEL VARIABLE ASSIGNMENT **

  !  V2T() - DERIVED BY USING TABLE 4-3A OF THE
  !  'WOOD HANDBOOK' USDA FOREST PRODUCTS LAB. 1999.  FPL-GTR-113.
  !  AS WELL AS 'HARDWOODS OF NORTH AMERICA' 1995. FPL-GTR-83 AND
  !  JENKINS ET. AL. 2004. GTR-NE-319.

  !  LEAFLF() - LIFETIME (YRS) OF LEAVES/NEEDLES - TAKEN FROM SN-FFE
  !  LEAFLF WAS SET WITH INFO FROM HARLOW AND HARRAH'S TEXTBOOK
  !  OF DENDROLOGY, 9TH EDITION
  !  EXCEPTIONS:
  !    EASTERN REDCEDAR FOUND IN MICHIGAN TREES, BARNES AND WAGNER

  !  TFALL() - TIME TO FALL FOR CROWN COMPONENTS;
  !  CATEGORIES ARE
  !  0 -  FOLIAGE
  !  1 -   0 - 0.25 IN
  !  2 -   0.25 - 1.00 IN
  !  3 -   1.00 - 3.00 IN
  !  4 -   3.00 - 6.00 IN
  !  5 -   6.00 - 12.00 IN

  !  TFALL IS ASSIGNED FOR 6 DIFFERENT TFALLCLS GROUPS, AS DEFINED
  !  AT THE SN-FFE WORKSHOP.  SPECIES NOT IN SN-FFE WERE CLASSES AS 5,
  !  EXCEPT OSAGE-ORANGE, WHICH WAS SET TO 4.

  !  NOTE: IF ANY TFALL VALUE IS GT 25, YOU SHOULD RE-DIM TFMAX IN FMPARM.

  !  THE SNAG VARIABLES DESCRIBED BELOW ARE SET BY FIRST DEFINING A
  !  SNAGCLS (1, 2, OR 3) FOR EACH SPECIES, AND THEN SETTING THE
  !  VARIABLES FOR THE 3 SNAG CLASSES.  SNAGCLS IS DEFINED AS FOLLOWS:
  !  1 - SPECIES DECAYS AND FALLS FASTER THAN AVERAGE
  !  2 - SPECIES DECAYS AND FALLS AT AN AVERAGE RATE
  !  3 - SPECIES DECAYS AND FALLS SLOWER THAN AVERAGE
  !  SNAGCLS WAS TAKEN FROM SN-FFE.
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
  !  SPECIES NOT LISTED IN THE BOOK WERE ASSIGNED BASED ON INFO
  !  PROVIDED AT THE SN WORKSHOP

  do I = 1,MAXSP

     select case (I)

        !  EASTERN REDCEDAR

     case (1)
        V2T(I) = 27.4
        TFALLCLS(I) = 1
        DKRCLS(I) = 2
        SNAGCLS(I) = 3

        !  JUNIPER SPECIES

     case (2)
        V2T(I) = 27.4
        TFALLCLS(I) = 1
        DKRCLS(I) = 2
        SNAGCLS(I) = 3

        !  SHORTLEAF PINE

     case (3)
        V2T(I) = 29.3
        TFALLCLS(I) = 6
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  VIRGINIA PINE

     case (4)
        V2T(I) = 28.1
        TFALLCLS(I) = 6
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  LOBLOLLY PINE

     case (5)
        V2T(I) = 29.3
        TFALLCLS(I) = 6
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  OTHER SOFTWOOD SPECIES

     case (6)
        V2T(I) = 21.2
        TFALLCLS(I) = 6
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  EASTERN WHITE PINE

     case (7)
        V2T(I) = 21.2
        TFALLCLS(I) = 6
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  BLACK WALNUT

     case (8)
        V2T(I) = 31.8
        TFALLCLS(I) = 4
        DKRCLS(I) = 2
        SNAGCLS(I) = 2

        !  BUTTERNUT

     case (9)
        V2T(I) = 22.5
        TFALLCLS(I) = 4
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  TUPELO SPECIES

     case (10)
        V2T(I) = 28.7
        TFALLCLS(I) = 3
        DKRCLS(I) = 2
        SNAGCLS(I) = 3

        !  SWAMP TUPELO

     case (11)
        V2T(I) = 28.7
        TFALLCLS(I) = 3
        DKRCLS(I) = 2
        SNAGCLS(I) = 3

        !  WATER TUPELO

     case (12)
        V2T(I) = 28.7
        TFALLCLS(I) = 3
        DKRCLS(I) = 2
        SNAGCLS(I) = 3

        !  BLACKGUM / BLACK TUPELO

     case (13)
        V2T(I) = 28.7
        TFALLCLS(I) = 3
        DKRCLS(I) = 2
        SNAGCLS(I) = 3

        !  SELECT HICKORY

     case (14)
        V2T(I) = 39.9
        TFALLCLS(I) = 2
        DKRCLS(I) = 4
        SNAGCLS(I) = 3

        !  SHAGBARK HICKORY

     case (15)
        V2T(I) = 39.9
        TFALLCLS(I) = 2
        DKRCLS(I) = 4
        SNAGCLS(I) = 3

        !  SHELLBARK HICKORY

     case (16)
        V2T(I) = 38.7
        TFALLCLS(I) = 2
        DKRCLS(I) = 4
        SNAGCLS(I) = 3

        !  MOCKERNUT HICKORY

     case (17)
        V2T(I) = 39.9
        TFALLCLS(I) = 2
        DKRCLS(I) = 4
        SNAGCLS(I) = 3

        !  PIGNUT HICKORY

     case (18)
        V2T(I) = 41.2
        TFALLCLS(I) = 2
        DKRCLS(I) = 4
        SNAGCLS(I) = 3

        !  HICKORY SPECIES

     case (19)
        V2T(I) = 39.9
        TFALLCLS(I) = 2
        DKRCLS(I) = 4
        SNAGCLS(I) = 3

        !  WATER HICKORY

     case (20)
        V2T(I) = 38.0
        TFALLCLS(I) = 2
        DKRCLS(I) = 4
        SNAGCLS(I) = 3

        !  BITTERNUT HICKORY

     case (21)
        V2T(I) = 37.4
        TFALLCLS(I) = 2
        DKRCLS(I) = 4
        SNAGCLS(I) = 3

        !  PECAN

     case (22)
        V2T(I) = 37.4
        TFALLCLS(I) = 2
        DKRCLS(I) = 4
        SNAGCLS(I) = 3

        !  BLACK HICKORY

     case (23)
        V2T(I) = 39.9
        TFALLCLS(I) = 2
        DKRCLS(I) = 4
        SNAGCLS(I) = 3

        !  AMERICAN BEECH

     case (24)
        V2T(I) = 34.9
        TFALLCLS(I) = 4
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  BLACK ASH

     case (25)
        V2T(I) = 28.1
        TFALLCLS(I) = 5
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  PUMPKIN ASH

     case (26)
        V2T(I) = 29.9
        TFALLCLS(I) = 5
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  BLUE ASH

     case (27)
        V2T(I) = 33.1
        TFALLCLS(I) = 5
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  EASTERN COTTONWOOD

     case (28)
        V2T(I) = 23.1
        TFALLCLS(I) = 5
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  RED MAPLE

     case (29)
        V2T(I) = 30.6
        TFALLCLS(I) = 5
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  BOXELDER

     case (30)
        V2T(I) = 25.9
        TFALLCLS(I) = 5
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  SILVER MAPLE

     case (31)
        V2T(I) = 27.4
        TFALLCLS(I) = 5
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  BLACK CHERRY

     case (32)
        V2T(I) = 29.3
        TFALLCLS(I) = 4
        DKRCLS(I) = 2
        SNAGCLS(I) = 2

        !  AMERICAN ELM

     case (33)
        V2T(I) = 28.7
        TFALLCLS(I) = 5
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  SUGARBERRY

     case (34)
        V2T(I) = 30.6
        TFALLCLS(I) = 4
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  HACKBERRY

     case (35)
        V2T(I) = 30.6
        TFALLCLS(I) = 4
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  WINGED ELM

     case (36)
        V2T(I) = 37.4
        TFALLCLS(I) = 5
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  ELM SPECIES

     case (37)
        V2T(I) = 28.7
        TFALLCLS(I) = 5
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  SIBERIAN ELM

     case (38)
        V2T(I) = 28.7
        TFALLCLS(I) = 5
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  SLIPPERY ELM

     case (39)
        V2T(I) = 29.9
        TFALLCLS(I) = 5
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  ROCK ELM

     case (40)
        V2T(I) = 35.6
        TFALLCLS(I) = 5
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  YELLOW-POPLAR

     case (41)
        V2T(I) = 24.9
        TFALLCLS(I) = 5
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  AMERICAN BASSWOOD

     case (42)
        V2T(I) = 20.0
        TFALLCLS(I) = 5
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  SUGAR MAPLE

     case (43)
        V2T(I) = 34.9
        TFALLCLS(I) = 5
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  ASH SPECIES

     case (44)
        V2T(I) = 33.1
        TFALLCLS(I) = 5
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  WHITE ASH

     case (45)
        V2T(I) = 34.3
        TFALLCLS(I) = 5
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  GREEN ASH

     case (46)
        V2T(I) = 33.1
        TFALLCLS(I) = 5
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  WHITE OAK

     case (47)
        V2T(I) = 37.4
        TFALLCLS(I) = 3
        DKRCLS(I) = 2
        SNAGCLS(I) = 3

        !  NORTHERN RED OAK

     case (48)
        V2T(I) = 34.9
        TFALLCLS(I) = 4
        DKRCLS(I) = 3
        SNAGCLS(I) = 2

        !  SOUTHERN RED OAK

     case (49)
        V2T(I) = 32.4
        TFALLCLS(I) = 4
        DKRCLS(I) = 3
        SNAGCLS(I) = 2

        !  BLACK OAK

     case (50)
        V2T(I) = 34.9
        TFALLCLS(I) = 4
        DKRCLS(I) = 3
        SNAGCLS(I) = 2

        !  SCARLET OAK

     case (51)
        V2T(I) = 37.4
        TFALLCLS(I) = 4
        DKRCLS(I) = 3
        SNAGCLS(I) = 2

        !  BLACKJACK OAK

     case (52)
        V2T(I) = 34.9
        TFALLCLS(I) = 2
        DKRCLS(I) = 2
        SNAGCLS(I) = 3

        !  CHINKAPIN OAK

     case (53)
        V2T(I) = 37.4
        TFALLCLS(I) = 3
        DKRCLS(I) = 2
        SNAGCLS(I) = 3

        !  SWAMP WHITE OAK

     case (54)
        V2T(I) = 39.9
        TFALLCLS(I) = 3
        DKRCLS(I) = 2
        SNAGCLS(I) = 3

        !  BUR OAK

     case (55)
        V2T(I) = 36.2
        TFALLCLS(I) = 3
        DKRCLS(I) = 2
        SNAGCLS(I) = 3

        !  SWAMP CHESTNUT OAK

     case (56)
        V2T(I) = 37.4
        TFALLCLS(I) = 3
        DKRCLS(I) = 3
        SNAGCLS(I) = 2

        !  POST OAK

     case (57)
        V2T(I) = 37.4
        TFALLCLS(I) = 3
        DKRCLS(I) = 2
        SNAGCLS(I) = 3

        !  DELTA POST OAK

     case (58)
        V2T(I) = 37.4
        TFALLCLS(I) = 3
        DKRCLS(I) = 2
        SNAGCLS(I) = 3

        !  CHESTNUT OAK

     case (59)
        V2T(I) = 35.6
        TFALLCLS(I) = 3
        DKRCLS(I) = 3
        SNAGCLS(I) = 2

        !  PIN OAK

     case (60)
        V2T(I) = 36.2
        TFALLCLS(I) = 4
        DKRCLS(I) = 3
        SNAGCLS(I) = 2

        !  CHERRYBARK OAK

     case (61)
        V2T(I) = 38.0
        TFALLCLS(I) = 4
        DKRCLS(I) = 3
        SNAGCLS(I) = 2

        !  SHINGLE OAK

     case (62)
        V2T(I) = 34.9
        TFALLCLS(I) = 4
        DKRCLS(I) = 2
        SNAGCLS(I) = 2

        !  OVERCUP OAK

     case (63)
        V2T(I) = 35.6
        TFALLCLS(I) = 3
        DKRCLS(I) = 3
        SNAGCLS(I) = 2

        !  WATER OAK

     case (64)
        V2T(I) = 34.9
        TFALLCLS(I) = 3
        DKRCLS(I) = 2
        SNAGCLS(I) = 3

        !  NUTTALL OAK

     case (65)
        V2T(I) = 34.9
        TFALLCLS(I) = 4
        DKRCLS(I) = 3
        SNAGCLS(I) = 2

        !  WILLOW OAK

     case (66)
        V2T(I) = 34.9
        TFALLCLS(I) = 4
        DKRCLS(I) = 2
        SNAGCLS(I) = 2

        !  SHUMARD OAK

     case (67)
        V2T(I) = 34.9
        TFALLCLS(I) = 4
        DKRCLS(I) = 3
        SNAGCLS(I) = 2

        !  OTHER UPLAND HARDWOODS

     case (68)
        V2T(I) = 37.4
        TFALLCLS(I) = 2
        DKRCLS(I) = 2
        SNAGCLS(I) = 3

        !  SASSAFRAS

     case (69)
        V2T(I) = 26.2
        TFALLCLS(I) = 4
        DKRCLS(I) = 2
        SNAGCLS(I) = 2

        !  OHIO BUCKEYE

     case (70)
        V2T(I) = 20.6
        TFALLCLS(I) = 5
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  CATALPA

     case (71)
        V2T(I) = 23.7
        TFALLCLS(I) = 4
        DKRCLS(I) = 2
        SNAGCLS(I) = 2

        !  COMMON PERSIMMON

     case (72)
        V2T(I) = 39.9
        TFALLCLS(I) = 4
        DKRCLS(I) = 2
        SNAGCLS(I) = 3

        !  HONEYLOCUST

     case (73)
        V2T(I) = 37.4
        TFALLCLS(I) = 2
        DKRCLS(I) = 2
        SNAGCLS(I) = 3

        !  BALSAM POPLAR

     case (74)
        V2T(I) = 19.3
        TFALLCLS(I) = 5
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  BIGTOOTH ASPEN

     case (75)
        V2T(I) = 22.5
        TFALLCLS(I) = 5
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  QUAKING ASPEN

     case (76)
        V2T(I) = 21.8
        TFALLCLS(I) = 5
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  BLACK LOCUST

     case (77)
        V2T(I) = 41.2
        TFALLCLS(I) = 2
        DKRCLS(I) = 1
        SNAGCLS(I) = 3

        !  OTHER LOWLAND SPECIES

     case (78)
        V2T(I) = 28.7
        TFALLCLS(I) = 5
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  SYCAMORE

     case (79)
        V2T(I) = 28.7
        TFALLCLS(I) = 5
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  BALDCYPRESS

     case (80)
        V2T(I) = 26.2
        TFALLCLS(I) = 1
        DKRCLS(I) = 3
        SNAGCLS(I) = 3

        !  RIVER BIRCH

     case (81)
        V2T(I) = 30.6
        TFALLCLS(I) = 5
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  SWEETGUM

     case (82)
        V2T(I) = 28.7
        TFALLCLS(I) = 5
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  WILLOW SPECIES

     case (83)
        V2T(I) = 22.5
        TFALLCLS(I) = 6
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  BLACK WILLOW

     case (84)
        V2T(I) = 22.5
        TFALLCLS(I) = 6
        DKRCLS(I) = 4
        SNAGCLS(I) = 1

        !  NON-COMMERCIAL HARDWOODS

     case (85)
        V2T(I) = 39.9
        TFALLCLS(I) = 5
        DKRCLS(I) = 3
        SNAGCLS(I) = 2

        !  AMERICAN HORNBEAM

     case (86)
        V2T(I) = 36.2
        TFALLCLS(I) = 4
        DKRCLS(I) = 3
        SNAGCLS(I) = 2

        !  EASTERN REDBUD

     case (87)
        V2T(I) = 36.2
        TFALLCLS(I) = 5
        DKRCLS(I) = 3
        SNAGCLS(I) = 2

        !  FLOWERING DOGWOOD

     case (88)
        V2T(I) = 39.9
        TFALLCLS(I) = 5
        DKRCLS(I) = 3
        SNAGCLS(I) = 2

        !  HAWTHORN SPECIES

     case (89)
        V2T(I) = 38.7
        TFALLCLS(I) = 5
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  KENTUCKY COFFEETREE

     case (90)
        V2T(I) = 33.1
        TFALLCLS(I) = 5
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  OSAGE-ORANGE

     case (91)
        V2T(I) = 47.4
        TFALLCLS(I) = 4
        DKRCLS(I) = 1
        SNAGCLS(I) = 2

        !  CUCUMBERTREE

     case (92)
        V2T(I) = 27.4
        TFALLCLS(I) = 4
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  SWEETBAY

     case (93)
        V2T(I) = 26.2
        TFALLCLS(I) = 4
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

        !  MULBERRY SPECIES

     case (94)
        V2T(I) = 36.8
        TFALLCLS(I) = 5
        DKRCLS(I) = 1
        SNAGCLS(I) = 2

        !  EASTERN HOPHORNBEAM

     case (95)
        V2T(I) = 39.3
        TFALLCLS(I) = 4
        DKRCLS(I) = 3
        SNAGCLS(I) = 2

        !  SOURWOOD

     case (96)
        V2T(I) = 31.2
        TFALLCLS(I) = 5
        DKRCLS(I) = 4
        SNAGCLS(I) = 2

     end select

     select case (I)
     case (1:7)
        LSW(I) = .true.
     case default
        LSW(I) = .false.
     end select

     select case (I)
     case (1,2)
        LEAFLF(I) = 5
     case (3)
        LEAFLF(I) = 4
     case (4,5)
        LEAFLF(I) = 3
     case (6,7)
        LEAFLF(I) = 2
     case default
        LEAFLF(I) = 1
     end select

     if (I .le.2) then !redcedar /juniper species
        TFALL(I,0) = 3.0 ! foliage
     else
        TFALL(I,0) = 1.0
     end if

     select case (TFALLCLS(I))

     case (1) ! baldcypress/redcedar group
        TFALL(I,1) = 5.0
        TFALL(I,3) = 10.0
        TFALL(I,4) = 25.0

     case (2) ! hickory/blackjack oak group
        TFALL(I,1) = 3.0
        TFALL(I,3) = 6.0
        TFALL(I,4) = 12.0

     case (3) ! white oak group
        TFALL(I,1) = 2.0
        TFALL(I,3) = 5.0
        TFALL(I,4) = 10.0

     case (4) ! red oak group
        TFALL(I,1) = 1.0
        TFALL(I,3) = 4.0
        TFALL(I,4) = 8.0

     case (5) ! ash/elm/cottonwood group
        TFALL(I,1) = 1.0
        TFALL(I,3) = 3.0
        TFALL(I,4) = 6.0

     case (6) ! pines
        TFALL(I,1) = 1.0
        TFALL(I,3) = 2.0
        TFALL(I,4) = 4.0

     end select

     TFALL(I,2) = TFALL(I,1)
     TFALL(I,5) = TFALL(I,4)

     select case (SNAGCLS(I))

     case (1) ! pines and others
        ! this group decays and falls faster than average
        DECAYX(I) = 0.07 ! 12 inch tree is soft in 2 years
        FALLX(I) = 7.17 ! 95% of 12-inchers are down in 3 years
        ALLDWN(I) = 6.0
        if ((I .ge.3) .and. (I .le.7)) then ! pines
           ALLDWN(I) = 50.0
        end if

     case (2) ! black oak and others
        ! this group decays and falls at average rate
        DECAYX(I) = 0.21 ! 12 inch tree is soft in 6 years
        FALLX(I) = 3.07 ! 95% of 12-inchers are down in 7 years
        ALLDWN(I) = 15.0

     case (3) ! white oak, redcedar, and others
        ! this group decays and falls slower than average
        DECAYX(I) = 0.35 ! 12 inch tree is soft in 10 years
        FALLX(I) = 1.96 ! 95% of 12-inchers are down in 11 years
        ALLDWN(I) = 25.0
        if (I .le.2) then ! redcedar
           ALLDWN(I) = 100.0
        end if

     end select

     do J = 1,4
        HTX(I,J) = 0.0 ! no height loss
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
  !  COVER TYPE

  OLDICT = 0

  !  DROUGHT START AND END YEARS

  IDRYB = 0
  IDRYE = 0

  !  CRITICAL % CHANGE REQUIERED TO TRIGGER ACTIVITY FUELS

  SLCRIT = 10.0

  return
end
