!=======================!
!   fire/ak : fmvinit   !
!=======================!
! FIRE-AK $Id$
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

  PREWND(1) = 20.0
  PREWND(2) = 6.0
  POTEMP(1) = 70.0
  POTEMP(2) = 70.0

  !     DECAY RATES BASED ON PN DECAY INFO SENT BY KIM MELLEN IN R6,
  !     ASSUMING COLD/WET PN HABITAT TYPE.

  DKR(1,1) = 0.052 ! < 0.25"
  DKR(2,1) = 0.052 ! 0.25 - 1"
  DKR(3,1) = 0.052 ! 1 - 3"
  DKR(4,1) = 0.012 ! 3 - 6"
  DKR(5,1) = 0.012 ! 6 - 12"
  DKR(6,1) = 0.009 ! 12 - 20"
  DKR(7,1) = 0.009 ! 20 - 35"
  DKR(8,1) = 0.009 ! 35 - 50"
  DKR(9,1) = 0.009 ! > 50"
  DKR(10,1) = 0.35 ! litter
  DKR(11,1) = 0.002 ! duff

  DKR(1,2) = 0.061 ! < 0.25"
  DKR(2,2) = 0.061 ! 0.25 - 1"
  DKR(3,2) = 0.061 ! 1 - 3"
  DKR(4,2) = 0.025 ! 3 - 6"
  DKR(5,2) = 0.025 ! 6 - 12"
  DKR(6,2) = 0.018 ! 12 - 20"
  DKR(7,2) = 0.018 ! 20 - 35"
  DKR(8,2) = 0.018 ! 35 - 50"
  DKR(9,2) = 0.018 ! > 50"
  DKR(10,2) = 0.4 ! litter
  DKR(11,2) = 0.002 ! duff

  DKR(1,3) = 0.073 ! < 0.25"
  DKR(2,3) = 0.073 ! 0.25 - 1"
  DKR(3,3) = 0.073 ! 1 - 3"
  DKR(4,3) = 0.041 ! 3 - 6"
  DKR(5,3) = 0.041 ! 6 - 12"
  DKR(6,3) = 0.031 ! 12 - 20"
  DKR(7,3) = 0.031 ! 20 - 35"
  DKR(8,3) = 0.031 ! 35 - 50"
  DKR(9,3) = 0.031 ! > 50"
  DKR(10,3) = 0.45 ! litter
  DKR(11,3) = 0.003 ! duff

  DKR(1,4) = 0.098 ! < 0.25"
  DKR(2,4) = 0.098 ! 0.25 - 1"
  DKR(3,4) = 0.098 ! 1 - 3"
  DKR(4,4) = 0.077 ! 3 - 6"
  DKR(5,4) = 0.077 ! 6 - 12"
  DKR(6,4) = 0.058 ! 12 - 20"
  DKR(7,4) = 0.058 ! 20 - 35"
  DKR(8,4) = 0.058 ! 35 - 50"
  DKR(9,4) = 0.058 ! > 50"
  DKR(10,4) = 0.5 ! litter
  DKR(11,4) = 0.003 ! duff

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

  NZERO = 0.01
  LIMBRK = 0.01
  HTXSFT = 2.0
  do I = 1,MAXSP
     PSOFT(I) = 0.0
  end do

  !     HTR1 and HTR2 are set below, and used for most species (where htx = 1)
  !     for cedar, htx = 0, so no snag height loss is modelled.
  !     2% a year height loss is based on Hennon and Loopstra (1991) who found that
  !     WH snags (ave dbh 23") were 30 ft or shorter after 38 years.

  HTR1 = 0.02
  HTR2 = 0.02

  !     ** SPECIES-LEVEL VARIABLE ASSIGNMENT **

  !     V2T() - UNITS ARE LB/CUFT BY SPECIES - FROM THE
  !      'WOOD HANDBOOK' USDA FOREST PRODUCTS LAB. 1999.  FPL-GTR-113.

  !     [TO CONVERT G/CM**3 TO LB/FT**3, MULTIPLY 'X' G/CM**3 * 62.372]

  !     LEAFLF() - LIFETIME (YRS) OF LEAVES/NEEDLES

  !     TFALL() - TIME TO FALL FOR DEAD CROWN COMPONENTS. THIS VARIABLE
  !     IS NOT UNDER USER-CONTROL BUT MIGHT BE SOMEDAY.
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

  !     ALLDWN() - YEARS DEAD AT WHICH ALL SNAGS WILL BE FALLEN (NOT USED IN AK-FFE)
  !     DECAYX() - DECAY RATE MULTIPLIER
  !     FALLX()  - FALL RATE MULTIPLIER
  !     HTX()    - HEIGHT-LOSS RATE MULTIPLIER

  !     DKRCLS() - DECAY RATE CLASS 1 (V.SLOW) TO 4 (FAST). MODEL USERS
  !     CAN USE THE FUELDCAY KEYWORD TO REASSIGN RATES WITHIN THE 4
  !     CLASSES, AND THE FUELPOOL KEYWORD TO REASSIGN CLASS

  !     ORIGINAL MEMBERS OF GROUPS PRIOR TO AK 23-SPECIES EXPANSION IS
  !     IS MARKED WITH (ORIGINAL) IN THE FOLLOWING 2 CASE STATMENTS.

  do I = 1,MAXSP

     select case (I)

        !         pacific silver fir (ORIGINAL)
     case (1)
        V2T(I) = 24.9
        LEAFLF(I) = 7.0
        ALLDWN(I) = 30.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        DKRCLS(I) = 3
        LSW(I) = .true.

        !         subalpine fir (ORIGINAL)
     case (2)
        V2T(I) = 19.3
        LEAFLF(I) = 7.0
        ALLDWN(I) = 90.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        DKRCLS(I) = 3
        LSW(I) = .true.

        !         Alaska-cedar (ORIGINAL)
     case (3)
        V2T(I) = 26.2
        LEAFLF(I) = 5.0
        ALLDWN(I) = 300.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 0.0
        end do
        DKRCLS(I) = 1
        LSW(I) = .true.

        !         tamarack (subalpine larch in PN)
     case (4)
        V2T(I) = 29.9
        LEAFLF(I) = 1.0
        ALLDWN(I) = 150.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        DKRCLS(I) = 1
        LSW(I) = .true.

        !         white spruce (ORIGINAL)
        !         Lutz's spruce
        !         black spruce
        !         other softwoods (ORIGINAL)
     case (5,6,7,13)
        V2T(I) = 23.1
        LEAFLF(I) = 6.0
        ALLDWN(I) = 90.0 !pn engelmann spruce
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        DKRCLS(I) = 2
        LSW(I) = .true.

        !         sitka spruce (ORIGINAL)
     case (8)
        V2T(I) = 20.6
        LEAFLF(I) = 5.0
        ALLDWN(I) = 110.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        DKRCLS(I) = 2
        LSW(I) = .true.

        !         lodgepole pine (ORIGINAL)
     case (9)
        V2T(I) = 23.7
        LEAFLF(I) = 3.0
        ALLDWN(I) = 90.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        DKRCLS(I) = 2
        LSW(I) = .true.

        !         western redcedar (ORIGINAL)
     case (10)
        V2T(I) = 19.3
        LEAFLF(I) = 5.0
        ALLDWN(I) = 300.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 0.0
        end do
        DKRCLS(I) = 1
        LSW(I) = .true.

        !         western hemlock (ORIGINAL)
     case (11)
        V2T(I) = 26.2
        LEAFLF(I) = 5.0
        ALLDWN(I) = 100.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        DKRCLS(I) = 2
        LSW(I) = .true.

        !         mountain hemlock (ORIGINAL)
     case (12)
        V2T(I) = 26.2
        LEAFLF(I) = 4.0
        ALLDWN(I) = 90.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        DKRCLS(I) = 2
        LSW(I) = .true.

        !         alder species
        !         red alder (ORIGINAL)
     case (14,15)
        V2T(I) = 23.1
        LEAFLF(I) = 1.0
        ALLDWN(I) = 50.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        DKRCLS(I) = 4
        LSW(I) = .false.

        !         paper birch (paper birch in PN)
        !         Alaska birch
     case (16,17)
        V2T(I) = 29.9
        LEAFLF(I) = 1.0
        ALLDWN(I) = 50.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        DKRCLS(I) = 4
        LSW(I) = .false.

        !         balsam poplar
        !         black cottonwood (ORIGINAL)
        !         other hardwoods (ORIGINAL)
     case (18,20,23)
        V2T(I) = 19.3
        LEAFLF(I) = 1.0
        ALLDWN(I) = 50.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        DKRCLS(I) = 4
        LSW(I) = .false.

        !         quaking aspen (quaking aspen in PN)
     case (19)
        V2T(I) = 21.8
        LEAFLF(I) = 1.0
        ALLDWN(I) = 50.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        DKRCLS(I) = 4
        LSW(I) = .false.

        !         willow species (willow in PN)
        !         Scouler's willow
     case (21,22)
        V2T(I) = 22.5
        LEAFLF(I) = 1.0
        ALLDWN(I) = 50.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        DKRCLS(I) = 4
        LSW(I) = .false.

     end select

     !       TIME-TO-FALL VALUES

     select case (I)

        !       white spruce (ORIGINAL)
        !       Lutz's spruce
        !       black spruce
        !       other softwood (ORIGINAL)
     case (5,6,7,13)
        TFALL(I,0) = 2
        TFALL(I,1) = 5
        TFALL(I,2) = 5
        TFALL(I,3) = 10
        TFALL(I,4) = 50

        !       Alaska-cedar (ORIGINAL)
        !       western redcedar (ORIGINAL)
     case (3,10)
        TFALL(I,0) = 5
        TFALL(I,1) = 15
        TFALL(I,2) = 15
        TFALL(I,3) = 30
        TFALL(I,4) = 55

        !       western hemlock (ORIGINAL)
        !       mountain hemlock (ORIGINAL)
        !       tamarack
     case (4,11,12)
        TFALL(I,0) = 1
        TFALL(I,1) = 5
        TFALL(I,2) = 5
        TFALL(I,3) = 15
        TFALL(I,4) = 50

        !       pacific silver fir (ORIGINAL)
        !       subalpine fir (ORIGINAL)
        !       sitka spriuce (ORIGINAL)
        !       lodgpole pine (ORIGINAL)
     case (1,2,8,9)
        TFALL(I,0) = 2
        TFALL(I,1) = 5
        TFALL(I,2) = 5
        TFALL(I,3) = 15
        TFALL(I,4) = 50

        !       alder species
        !       red alder (ORIGINAL)
        !       paper birch
        !       Alaska birch
        !       balsam poplar
        !       quaking aspen
        !       black cottonwood (ORIGINAL)
        !       willow species
        !       Scouler's willow
        !       other hardwoods (ORIGINAL)
     case (14:23)
        TFALL(I,0) = 1
        TFALL(I,1) = 10
        TFALL(I,2) = 15
        TFALL(I,3) = 15
        TFALL(I,4) = 50
     end select

     TFALL(I,5) = TFALL(I,4)

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
  PBSOFT = 0.0
  PBSMAL = 0.0
  PBSIZE = 12.0
  PBTIME = 7.0

  !     PARAMETERS FOR FUEL MODEL SELECTION

  !     THE DEFAULT COVER GROUP **FMCFMD** (not used in AK-FFE)
  OLDICT = 3
  OLDICT2 = 0
  OLDICTWT(1) = 1.0
  OLDICTWT(2) = 0.0

  !     DROUGHT START AND END YEARS (not used in AK-FFE)
  IDRYB = 0
  IDRYE = 0

  !     CRITICAL % CHANGE REQUIRED TO TRIGGER ACTIVITY FUELS
  SLCRIT = 10.0

  !     CRITICAL % CHANGE IN %CC REQUIRED TO TRIGGER SHRUB MODEL DELAY (not used in AK-FFE)
  CCCRIT = 10.0

  return
end
