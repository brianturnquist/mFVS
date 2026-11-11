!=======================!
!   fire/kt : fmvinit   !
!=======================!
! FIRE-KT $Id$
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

  DKR(1,1) = 0.12
  DKR(2,1) = 0.12
  DKR(3,1) = 0.09
  DKR(4,1) = 0.015
  DKR(5,1) = 0.015
  DKR(6,1) = 0.015
  DKR(7,1) = 0.015
  DKR(8,1) = 0.015
  DKR(9,1) = 0.015

  do I = 1,9
     do J = 2,4
        DKR(I,J) = DKR(I,1)
     end do
  end do

  !  LITTER LOSS/YR (10) AND DUFF LOSS/YR (11)

  do J = 1,4
     DKR(10,J) = 0.5
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

  !     VALUE OF HTXSFT BASED ON CONVERTING PL HEIGHT-LOSS RATE TO DF (19.5)
  !     AND PP RATE TO ~DF/SPRUCE (10.2, 2.1); USE 10.0

  NZERO = 0.01
  LIMBRK = 0.01
  HTXSFT = 2.0
  HTR1 = 0.0228
  HTR2 = 0.01
  do I = 1,MAXSP
     PSOFT(I) = 0.0
  end do

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
  !     DECAYX() - DECAY RATE MULTIPLIER       (PP = 1.0)
  !     FALLX()  - FALL RATE MULTIPLIER        (PP = 1.0)
  !     HTX()    - HEIGHT-LOSS RATE MULTIPLIER (PP = 1.0)

  !     ** NOTE THAT SNAG RATE PARAMETERS ARE ALL RELATIVE TO PP **

  !     DKRCLS() - DECAY RATE CLASS 1 (V.SLOW) TO 4 (FAST). MODEL USERS
  !     CAN USE THE FUELDCAY KEYWORD TO REASSIGN RATES WITHIN THE 4
  !     CLASSES, AND THE FUELPOOL KEYWORD TO REASSIGN CLASS

  do I = 1,MAXSP

     select case (I)

        !         western white pine (uses PP for most WP attributes)
     case (1)
        V2T(I) = 22.5
        LEAFLF(I) = 4.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 110.0
        DECAYX(I) = 1.1
        FALLX(I) = 0.9
        do J = 1,4
           HTX(I,J) = 0.9
        end do
        DKRCLS(I) = 4
        LSW(I) = .true.

        !         western larch (uses similarity to PP for some WL attributes)
     case (2)
        V2T(I) = 29.9
        LEAFLF(I) = 1.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 110.0
        DECAYX(I) = 1.1
        FALLX(I) = 0.9
        do J = 1,4
           HTX(I,J) = 0.9
        end do
        DKRCLS(I) = 3
        LSW(I) = .true.

        !         Douglas-fir
     case (3)
        V2T(I) = 28.1
        LEAFLF(I) = 5.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 110.0
        DECAYX(I) = 1.1
        FALLX(I) = 0.9
        do J = 1,4
           HTX(I,J) = 0.9
        end do
        DKRCLS(I) = 3
        LSW(I) = .true.

        !         grand fir
     case (4)
        V2T(I) = 21.8
        LEAFLF(I) = 7.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 90.0
        DECAYX(I) = 0.9
        FALLX(I) = 1.1
        do J = 1,4
           HTX(I,J) = 1.1
        end do
        DKRCLS(I) = 4
        LSW(I) = .true.

        !         western hemlock
     case (5)
        V2T(I) = 26.2
        LEAFLF(I) = 5.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 90.0
        DECAYX(I) = 0.9
        FALLX(I) = 1.1
        do J = 1,4
           HTX(I,J) = 1.1
        end do
        DKRCLS(I) = 4
        LSW(I) = .true.

        !         western redcedar (uses DF for some RC attributes)
     case (6)
        V2T(I) = 19.3
        LEAFLF(I) = 5.0
        TFALL(I,3) = 20.0
        ALLDWN(I) = 90.0
        DECAYX(I) = 0.9
        FALLX(I) = 1.1
        do J = 1,4
           HTX(I,J) = 1.1
        end do
        DKRCLS(I) = 2
        LSW(I) = .true.

        !         lodgepole pine
     case (7)
        V2T(I) = 23.7
        LEAFLF(I) = 3.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 90.0
        DECAYX(I) = 0.9
        FALLX(I) = 1.1
        do J = 1,4
           HTX(I,J) = 1.1
        end do
        DKRCLS(I) = 4
        LSW(I) = .true.

        !         Engelmann spruce
     case (8)
        V2T(I) = 20.6
        LEAFLF(I) = 6.0
        TFALL(I,3) = 10.0
        ALLDWN(I) = 90.0
        DECAYX(I) = 0.9
        FALLX(I) = 1.1
        do J = 1,4
           HTX(I,J) = 1.1
        end do
        DKRCLS(I) = 4
        LSW(I) = .true.

        !         subalpine fir
     case (9)
        V2T(I) = 19.3
        LEAFLF(I) = 7.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 90.0
        DECAYX(I) = 0.9
        FALLX(I) = 1.1
        do J = 1,4
           HTX(I,J) = 1.1
        end do
        DKRCLS(I) = 4
        LSW(I) = .true.

        !         ponderosa pine
     case (10)
        V2T(I) = 23.7
        LEAFLF(I) = 4.0
        TFALL(I,3) = 10.0
        ALLDWN(I) = 100.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        DKRCLS(I) = 4
        LSW(I) = .true.

        !         mountain hemlock
     case (11)
        V2T(I) = 26.2
        LEAFLF(I) = 4.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 100.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        DKRCLS(I) = 4
        LSW(I) = .true.

     end select

     TFALL(I,1) = 5.0
     TFALL(I,2) = TFALL(I,1)
     TFALL(I,4) = TFALL(I,3)
     TFALL(I,5) = TFALL(I,3)

     !       DEAD LEAF FALL CANNOT BE > LIVE

     TFALL(I,0) = MIN(2.0, LEAFLF(I))

     !       TFALL(I,3) CANNOT BE < TFALL(I,2)

     if (TFALL(I,2) .gt. TFALL(I,3)) then
        TFALL(I,2) = TFALL(I,3)
     end if

     !       CONVERT LB/FT**3 TO TONS/FT**3

     V2T(I) = V2T(I) / 2000.0

  end do

  !     PARAMETERS FOR POST-BURN SNAG FALL RATES:

  PBSCOR = 0.0
  PBSOFT = 1.0
  PBSMAL = 0.9
  PBSIZE = 12.0
  PBTIME = 7.0

  !     THE LODGEPOLE PINE COVER TYPE METAGROUP **FMCFMD**

  OLDICT = 0

  !     DROUGHT START AND END YEARS

  IDRYB = 0
  IDRYE = 0

  !     CRITICAL % CHANGE REQUIERED TO TRIGGER ACTIVITY FUELS

  SLCRIT = 10.0

  return
end
