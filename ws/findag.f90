!=======================================!
!   Estimate tree age and site height   !
!=======================================!
! WS $Id$
subroutine FINDAG(I,ISPC,D1,D2,H,SITAGE,SITHT,AGMAX,HTMAX1,&
     &HTMAX2,DEBUG)
  implicit none

  !  CALLED FROM **COMCUP
  !  CALLED FROM **CRATET** AND **HTGF**

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'
  include 'GGCOM.f90'

  !  LOCAL VARIABLE DEFINITIONS:

  !  INCRNG = HAS A VALUE OF 1 WHEN THE SITE CURVE HAS BEEN MONOTONICALLY
  !           INCREASING IN PREVIOUS ITERATIONS.
  !   OLDHG = HEIGHT GUESS FROM PREVIOUS ITERATION.

  ! Variables
  logical :: DEBUG
  integer :: I,ISPC,ICLS,INCRNG
  real    :: D1,D2,H,SITAGE,SITHT,AGMAX,HTMAX1,HTMAX2,BAUTBA,SITE
  real    :: AGEMAX(MAXSP),HTMAX(MAXSP),AG,DIFF,HGUESS,SINDX,TOLER
  real    :: TOL,AP,AGETEM,HH,RATIO,TAGE,TSITE,OLDHG
  real    :: RDANUW

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
  !    35 = GIANT CHINQUAPIN (GC)             CHRYSOLEPIS CHRYSOPHYLLA
  !    36 = QUAKING ASPEN (AS)                POPULUS TREMULOIDES
  !    37 = CALIFORNIA-LAUREL (CL)            UMBELLULARIA CALIFORNICA
  !    38 = PACIFIC MADRONE (MA)              ARBUTUS MENZIESII
  !    39 = PACIFIC DOGWOOD (DG)              CORNUS NUTTALLII
  !    40 = BIGLEAF MAPLE (BM)                ACER MACROPHYLLUM
  !    41 = CURLLEAF MOUNTAIN-MAHOGANY (MC)   CERCOCARPUS LEDIFOLIUS
  !    42 = OTHER SOFTWOODS (OS)
  !    43 = OTHER HARDWOODS (OH)

  !  SURROGATE EQUATION ASSIGNMENT:

  !    FROM EXISTING WS EQUATIONS --
  !      USE 1(SP) FOR 11(WP) AND 24(MH)
  !      USE 2(DF) FOR 22(BD)
  !      USE 3(WF) FOR 13(SF)
  !      USE 4(GS) FOR 23(RW)
  !      USE 8(PP) FOR 18(MP)
  !      USE 34(TO) FOR 35(GC), 36(AS), 37(CL), 38(MA), AND 39(DG)
  !      USE 31(BO) FOR 28(LO), 29(CY), 30(BL), 32(VO), 33(IO), 40(BM), AND
  !                     43(OH)

  !    FROM CA VARIANT --
  !      USE CA11(KP) FOR 12(PM), 14(KP), 15(FP), 16(CP), 17(LM), 19(GP), 20(WE),
  !                       25(WJ), 26(WJ), AND 27(CJ)
  !      USE CA12(LP) FOR 9(LP) AND 10(WB)

  !    FROM SO VARIANT --
  !      USE SO30(MC) FOR 41(MC)

  !    FROM UT VARIANT --
  !      USE UT17(GB) FOR 21(GB)

  !  DATA STATEMENTS

  data AGEMAX/&
       &0.,   0.,   0.,   0.,   0.,   0.,   0.,   0., 400., 400.,&
       &0., 400.,   0., 400., 400., 400., 400.,   0., 400., 400.,&
       &210.,   0.,   0.,   0., 400., 400., 400.,   0.,   0.,   0.,&
       &0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,&
       &400.,   0.,   0./

  data HTMAX/&
       &0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,&
       &0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,&
       &0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,&
       &0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,&
       &20.,   0.,   0./

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = D2
  RDANUW = HTMAX2

  !  INITIALIZATIONS

  TOLER = 2.0
  SINDX = SITEAR(ISPC)
  AGMAX = AGEMAX(ISPC)
  HTMAX1 = HTMAX(ISPC)
  AG = 2.0

  !  FOR SPECIES FROM THE SO VARIANT: (41=MC)
  !  CRATET CALLS FINDAG AT THE BEGINING OF THE SIMULATION TO
  !  CALCULATE THE AGE OF INCOMING TREES.  AT THIS POINT ABIRTH(I)=0.
  !  THE AGE OF INCOMING TREES HAVING H>=HTMAX1 IS CALCULATED BY
  !  ASSUMING A GROWTH RATE OF 0.10FT/YEAR FOR THE INTERVAL H-HTMAX1.
  !  TREES REACHING HTMAX1 DURING THE SIMULATION ARE IDENTIFIED IN HTGF.

  if (ISPC.eq.41.and. H.ge.HTMAX1) then
     SITAGE = AGMAX + (H - HTMAX1)/0.10
     SITHT = H
     if (DEBUG)write(JOSTND,*)' ISPC,AGMAX,H,HTMAX1= ',ISPC,&
          &AGMAX,H,HTMAX1
     go to 30
  end if

  !  SPECIES THAT DON'T REQUIRE ITERATION OR HAVE THEIR OWN ITERATION
  !  21=GB FROM THE CR VARIANT VIA UT VARIANT

  if (ISPC .eq.21) then
     ICLS = IFIX(D1+1.0)
     if (ICLS .gt.41) ICLS = 41
     BAUTBA = BAU(ICLS)/BA
     if (BAUTBA.lt.0.0)BAUTBA = 0.0
     SITE = SITEAR(ISPC)

     TOL = 2.0
     AP = 10.0
     100 continue
     AGETEM = AP
     if (AGETEM .lt.30.0) AGETEM = 30.
     HH = (2.75780*SITE**0.83312) * ((1.0-EXP(-0.015701*AGETEM))&
          &**(22.71944*SITE**(-0.63557))) + 4.5
     RATIO = 1.0 - BAUTBA
     if (RATIO .lt.0.728) RATIO = 0.728
     HH = HH * RATIO
     DIFF = ABS(HH - H)
     if (DIFF .lt. TOL) go to 9990
     if (HH .gt. H) go to 9990
     AP = AP + 5.0
     if (AP .gt. AGEMAX(ISPC)) then
        TAGE = AGEMAX(ISPC)
        go to 9995
     end if
     go to 100
     9990 continue
     TAGE = AGETEM
     TSITE = SITE
     if (TSITE.lt.20.)TSITE = 20.
     TAGE = TAGE+4.5/(-0.22+0.0155*TSITE)
     9995 continue

     if (DEBUG)write(JOSTND,*)' IN FINDAG TAGE,SITE,H,',&
          &'BAUTBA= ',TAGE,SITE,H,BAUTBA
     SITAGE = TAGE
     if (SITAGE .le.0.0) SITAGE = 1.0
     go to 30
  end if

  ! R5 USE DUNNING/LEVITAN SITE CURVE.
  ! R6 USE **HTCALC** SITE CURVES.
  ! SPECIES DIFFERENCES ARE ARE ACCOUNTED FOR BY THE SPECIES
  ! SPECIFIC SITE INDEX VALUES WHICH ARE SET AFTER KEYWORD PROCESSING.

  INCRNG = 0
  HGUESS = 0.
  75 continue
  OLDHG = HGUESS

  call HTCALC(IFOR,SINDX,ISPC,AG,HGUESS,JOSTND,DEBUG)

  if (DEBUG)write(JOSTND,91200)I,IFOR,AG,HGUESS,H
  91200 format(' IN GUESS AN AGE--I,IFOR,AGE,HGUESS,H ',2I5,3F10.2)

  !  AVOID NEGATIVE PREDICTED HEIGHTS AT SMALL AGES FROM SOME SI CURVES
  !  GED 4/20/18

  if (HGUESS .lt.1.)go to 175

  DIFF = ABS(HGUESS-H)
  if (DIFF .le. TOLER .or. H .lt. HGUESS) then
     SITAGE = AG
     SITHT = HGUESS
     if (DEBUG) then
        write(JOSTND, *)' DIFF,TOLER,H,HGUESS,AG,SITAGE,SITHT= ',&
             &DIFF,TOLER,H,HGUESS,AG,SITAGE,SITHT
     end if
     go to 30
  end if

  !  SOME SITE CURVES DECREASE AT THE START BEFORE INCREASING. IF DECREASING,
  !  KEEP GOING; IF SITE CURVE WAS INCREASING, BUT NOW HAS FLATTENED OFF, STOP
  !  THE ITERATION GED 04/19/18

  DIFF = (HGUESS-OLDHG)
  if (OLDHG.ne.0.0.and. DIFF.ge.0.05) INCRNG = 1
  if (DEBUG)write(JOSTND,*)' IN FINDAG OLDHG,DIFF,INCRNG= ',&
       &OLDHG,DIFF,INCRNG
  if (INCRNG.eq.1.and. DIFF .lt.0.05) then
     SITAGE = AG
     SITHT = HGUESS
     if (DEBUG) then
        write(JOSTND, *)' SITE CURVE FLAT OLDHG,AG,HGUESS,SITAGE,',&
             &'SITHT= ',OLDHG,AG,HGUESS,SITAGE,SITHT
     end if
     go to 30
  end if

  175 continue
  AG = AG + 2.

  if (AG .gt. AGMAX) then

     !  H IS TOO GREAT AND MAX AGE IS EXCEEDED

     SITAGE = AGMAX
     SITHT = H
     go to 30
  else
     go to 75
  end if

  30 continue
  if (DEBUG)write(JOSTND,50)I,SITAGE,SITHT
  50 format(' LEAVING SUBROUTINE FINDAG  I,SITAGE,SITHT =',&
       &I5,2F10.3)

  return
end
  !**END OF CODE SEGMENT
