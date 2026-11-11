!=======================!
!   fire/ci : fmvinit   !
!=======================!
! FIRE-CI $Id$
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

  !     SPECIES LIST FOR CENTRAL IDAHO VARIANT.

  !     1 = WESTERN WHITE PINE (WP)          PINUS MONTICOLA
  !     2 = WESTERN LARCH (WL)               LARIX OCCIDENTALIS
  !     3 = DOUGLAS-FIR (DF)                 PSEUDOTSUGA MENZIESII
  !     4 = GRAND FIR (GF)                   ABIES GRANDIS
  !     5 = WESTERN HEMLOCK (WH)             TSUGA HETEROPHYLLA
  !     6 = WESTERN REDCEDAR (RC)            THUJA PLICATA
  !     7 = LODGEPOLE PINE (LP)              PINUS CONTORTA
  !     8 = ENGLEMANN SPRUCE (ES)            PICEA ENGELMANNII
  !     9 = SUBALPINE FIR (AF)               ABIES LASIOCARPA
  !    10 = PONDEROSA PINE (PP)              PINUS PONDEROSA
  !    11 = WHITEBARK PINE (WB)              PINUS ALBICAULIS
  !    12 = PACIFIC YEW (PY)                 TAXUS BREVIFOLIA
  !    13 = QUAKING ASPEN (AS)               POPULUS TREMULOIDES
  !    14 = WESTERN JUNIPER (WJ)             JUNIPERUS OCCIDENTALIS
  !    15 = CURLLEAF MOUNTAIN-MAHOGANY (MC)  CERCOCARPUS LEDIFOLIUS
  !    16 = LIMBER PINE (LM)                 PINUS FLEXILIS
  !    17 = BLACK COTTONWOOD (CW)            POPULUS BALSAMIFERA VAR. TRICHOCARPA
  !    18 = OTHER SOFTWOODS (OS)
  !    19 = OTHER HARDWOODS (OH)

  !  SURROGATE EQUATION ASSIGNMENT:

  !  FROM THE IE VARIANT:
  !      USE 17(PY) FOR 12(PY)             (IE17 IS REALLY TT2=LM)
  !      USE 18(AS) FOR 13(AS)             (IE18 IS REALLY UT6=AS)
  !      USE 13(LM) FOR 11(WB) AND 16(LM)  (IE13 IS REALLY TT2=LM)
  !      USE 19(CO) FOR 17(CW) AND 19(OH)  (IE19 IS REALLY CR38=OH)

  !  FROM THE UT VARIANT:
  !      USE 12(WJ) FOR 14(WJ)
  !      USE 20(MC) FOR 15(MC)             (UT20 = SO30=MC, WHICH IS
  !                                                  REALLY WC39=OT)

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

  !  DUFF PRODUCTION RATES 'PRDUFF' ARE A PROPORTION OF THE OVERALL
  !  DECAY RATE: 'DKR'.

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
  !     1974.

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

  do I = 1,MAXSP

     select case (I)

        !  1=WP, western white pine (NI uses PP for most WP attributes)

     case (1)
        V2T(I) = 22.5
        LEAFLF(I) = 4.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 110.0
        DECAYX(I) = 1.1
        FALLX(I) = 0.9 ! NI default
        do J = 1,4
           HTX(I,J) = 0.4 ! 25% loss 30 yr -> 0.42
        end do
        DKRCLS(I) = 4
        LSW(I) = .true.

        !  2=WL, western larch (NI)

     case (2)
        V2T(I) = 29.9
        LEAFLF(I) = 1.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 150.0
        DECAYX(I) = 1.1
        FALLX(I) = 1.0 ! 95% down 15"/25yr -> 1.0
        do J = 1,4
           HTX(I,J) = 0.001 ! no height loss
        end do
        DKRCLS(I) = 3
        LSW(I) = .true.

        !  3=DF, Douglas-fir (NI)

     case (3)
        V2T(I) = 28.1 ! north DF in Wood Handbook
        LEAFLF(I) = 5.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 75.0
        DECAYX(I) = 1.1
        FALLX(I) = 0.9 ! 95% down 18"/30yr -> 0.9
        do J = 1,4
           HTX(I,J) = 1.0 ! 50% loss 30 yr -> 1.0
        end do
        DKRCLS(I) = 3
        LSW(I) = .true.

        !  4=GF, grand fir

     case (4)
        V2T(I) = 21.8
        LEAFLF(I) = 7.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 90.0
        DECAYX(I) = 0.9
        FALLX(I) = 1.1
        do J = 1,4
           HTX(I,J) = 1.5 ! 50% loss 20 yr -> 1.52
        end do
        DKRCLS(I) = 4
        LSW(I) = .true.

        !  5=WH, western hemlock

     case (5)
        V2T(I) = 26.2
        LEAFLF(I) = 5.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 150.0
        DECAYX(I) = 0.9
        FALLX(I) = 0.9 ! 95% down 18"/28 yr -> 0.87
        do J = 1,4
           HTX(I,J) = 0.9 ! 50% loss 35 yr -> 0.87
        end do
        DKRCLS(I) = 4
        LSW(I) = .true.

        !  6=RC, western redcedar

     case (6)
        V2T(I) = 19.3
        LEAFLF(I) = 5.0
        TFALL(I,3) = 20.0
        ALLDWN(I) = 300.0
        DECAYX(I) = 0.9
        FALLX(I) = 0.3 ! 95% down 18"/100yr -> 0.27
        do J = 1,4
           HTX(I,J) = 0.3 ! 25% loss 50 yr -> 0.25
        end do
        DKRCLS(I) = 2
        LSW(I) = .true.

        !  7=LP, lodgepole pine

     case (7)
        V2T(I) = 23.7
        LEAFLF(I) = 3.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 35.0
        DECAYX(I) = 0.9
        FALLX(I) = 1.6 ! 95% down 15"/15yr -> 1.6
        do J = 1,4
           HTX(I,J) = 0.001 ! no height loss
        end do
        DKRCLS(I) = 4
        LSW(I) = .true.

        !  8=ES, Engelmann spruce
        !  special handling of FALLX(8) in **FMSNAG**

     case (8)
        V2T(I) = 20.6
        LEAFLF(I) = 6.0
        TFALL(I,3) = 10.0
        ALLDWN(I) = 100.0
        DECAYX(I) = 0.9
        FALLX(I) = 1.2 ! 95% 15"/20yr -> 1.2; 24"/95yr -> 0.4
        do J = 1,4
           HTX(I,J) = 0.001 ! no height loss
        end do
        DKRCLS(I) = 4
        LSW(I) = .true.

        !  9=AF, subalpine fir

     case (9)
        V2T(I) = 19.3
        LEAFLF(I) = 7.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 40.0
        DECAYX(I) = 0.9
        FALLX(I) = 0.8 ! 95% 15"/30yr -> 0.8
        do J = 1,4
           HTX(I,J) = 0.001 ! no height loss
        end do
        DKRCLS(I) = 4
        LSW(I) = .true.

        !  10=PP, ponderosa pine (NI)

     case (10)
        V2T(I) = 23.7
        LEAFLF(I) = 4.0
        TFALL(I,3) = 10.0
        ALLDWN(I) = 90.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0 ! NI default
        do J = 1,4
           HTX(I,J) = 0.001 ! no height loss
        end do
        DKRCLS(I) = 4
        LSW(I) = .true.

        ! 11=WB, whitebark pine
        ! 16=LM, limber pine

     case (11,16)
        V2T(I) = 22.5 !w. white pine
        LEAFLF(I) = 3.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 90.0
        DECAYX(I) = 0.9
        FALLX(I) = 0.41
        do J = 1,4
           HTX(I,J) = 0.001 ! no height loss
        end do
        DKRCLS(I) = 4
        LSW(I) = .true.

        ! 12=PY, Pacific yew

     case (12)
        V2T(I) = 26.2 ! bald cypress
        LEAFLF(I) = 7.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 100.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        DKRCLS(I) = 1
        LSW(I) = .true.

        ! 13=AS, quaking aspen

     case (13)
        V2T(I) = 21.8
        LEAFLF(I) = 1.0
        TFALL(I,3) = 10.0
        ALLDWN(I) = 5.0
        DECAYX(I) = 0.9
        FALLX(I) = 4.0
        do J = 1,4
           HTX(I,J) = 0.001 ! no height loss
        end do
        DKRCLS(I) = 4
        LSW(I) = .false.

        ! 14=WJ, western juniper

     case (14)
        V2T(I) = 34.9
        LEAFLF(I) = 4.0
        TFALL(I,3) = 20.0
        ALLDWN(I) = 150.0
        DECAYX(I) = 0.9
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 0.0978
        end do
        DKRCLS(I) = 2
        LSW(I) = .true.

        ! 15=MC, curlleaf mtn-mahogany

     case (15)
        V2T(I) = 21.8
        LEAFLF(I) = 1.0
        TFALL(I,0) = 1.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 90.0
        DECAYX(I) = 1.0
        FALLX(I) = 1.0
        do J = 1,4
           HTX(I,J) = 1.0
        end do
        DKRCLS(I) = 4
        LSW(I) = .false.

        ! 17=CW, black cottonwood
        ! 19=OH, other hardwoods

     case (17,19)
        V2T(I) = 19.3 !black cottonwood
        LEAFLF(I) = 1.0
        TFALL(I,3) = 10.0
        ALLDWN(I) = 5.0
        DECAYX(I) = 0.9
        FALLX(I) = 4.0
        do J = 1,4
           HTX(I,J) = 0.001 ! no height loss
        end do
        DKRCLS(I) = 4
        LSW(I) = .false.

        ! 18=OS, other softwoods (mountain hemlock (snag params from EC-PSF))

     case (18)
        V2T(I) = 26.2
        LEAFLF(I) = 4.0
        TFALL(I,3) = 15.0
        ALLDWN(I) = 30.0
        DECAYX(I) = 0.9
        FALLX(I) = 0.9 ! 95% 18"/25yr -> 0.9
        do J = 1,4
           HTX(I,J) = 1.5 ! 50% 20 yr -> 1.52
        end do
        DKRCLS(I) = 4
        LSW(I) = .true.

     end select

     select case (I)
        ! 14=WJ
     case (14)
        TFALL(I,1) = 10.0
        TFALL(I,2) = 15.0
        TFALL(I,4) = TFALL(I,3)
        TFALL(I,5) = TFALL(I,3)

     case default
        TFALL(I,1) = 5.0
        TFALL(I,2) = TFALL(I,1)
        TFALL(I,4) = TFALL(I,3)
        TFALL(I,5) = TFALL(I,3)
     end select

     !  DEAD LEAF FALL CANNOT BE > LIVE

     TFALL(I,0) = MIN(2.0, LEAFLF(I))

     !  TFALL(I,3) CANNOT BE < TFALL(I,2)

     if (TFALL(I,2) .gt. TFALL(I,3)) then
        TFALL(I,2) = TFALL(I,3)
     end if

     !  CONVERT LB/FT**3 TO TONS/FT**3

     V2T(I) = V2T(I) / 2000.0

  end do

  !  PARAMETERS FOR POST-BURN SNAG FALL RATES:

  PBSCOR = 0.0
  PBSOFT = 1.0
  PBSMAL = 0.9
  PBSIZE = 12.0
  PBTIME = 7.0

  !  THE LODGEPOLE PINE COVER TYPE METAGROUP **FMCFMD**

  OLDICT = 0

  !  DROUGHT START AND END YEARS

  IDRYB = 0
  IDRYE = 0

  !  CRITICAL % CHANGE REQUIRED TO TRIGGER ACTIVITY FUELS

  SLCRIT = 10.0

  return
end
