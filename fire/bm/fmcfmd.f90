!======================!
!   fire/bm : fmcfmd   !
!======================!
! FIRE-BM $Id$
subroutine FMCFMD (IYR, FMD)
  implicit none

  !     SINGLE-STAND VERSION
  !     CALLED FROM: FMBURN
  !  PURPOSE:
  !     THIS SUBROUTINE RETURNS TWO TYPES OF INFORMATION: THE FUEL MODEL
  !     THAT WOULD BE USED IF THE STATIC FUEL MODEL OPTION IS SELECTED
  !     (STORED AS IFMD(1), WITH A WEIGHT OF FWT(1)=1.0 AND THE CLOSEST
  !     FUEL MODELS (UP TO 4) AND THEIR WEIGHTINGS FOR USE BY THE DYNAMIC
  !     FUEL MODEL

  !  CALL LIST DEFINITIONS:
  !     FMD:     FUEL MODEL NUMBER

  !  COMMON BLOCK VARIABLES AND PARAMETERS:
  !     SMALL:   SMALL FUELS FROM DYNAMIC FUEL MODEL
  !     LARGE:   LARGE FUELS FROM DYNAMIC FUEL MODEL

  !.... PARAMETER INCLUDE FILES.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'

  !.... COMMON INCLUDE FILES.

  include 'FMFCOM.f90'
  include 'FMCOM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'ARRAYS.f90'
  include 'SSTGMC.f90'

  !     LOCAL VARIABLE DECLARATIONS

  !     NUMBER OF POTENTIAL FIRE MODELS

  ! Variables
  integer :: ICLSS
  parameter(ICLSS = 14)

  integer :: IYR,FMD

  integer :: I,J,K
  integer :: IPTR(ICLSS),ITYP(ICLSS)
  real    :: XPTS(ICLSS,2),EQWT(ICLSS),WT1(2),WT2(4),WT3(2),WT4(2)
  real    :: STNDBA,X(2),Y(2),COVA,COVB,AFWT
  real    :: ALGSLP,SZCLS,WD(13),SCLAB(13)

  real    :: PRDF,PRPP
  logical :: DEBUG,VL1(MAXTRE)

  !     FIXED VALUES FOR INTERPOLATION FUNCTION

  data     Y / 0.0, 1.0 /

  !     SIZE CLASS LABELS (AS REAL) FOR THE BLUE MOUNTAIN

  data SCLAB /1.0, 2.0, 3.0, 4.0, 5.0,&
       &6.0, 6.5, 7.0, 7.5, 8.0,&
       &9.0,10.0,11.0/

  !     These are the integer tags associated with each fire model
  !     class. They are returned with the weight

  data IPTR / 1,2,3,4,5,6,7,8,9,10,11,12,13,14 /

  !     These are 0 for regular lines, -1 for horizontal and 1 for
  !     vertical lines. If any of the lines defined by xpts() are of
  !     an unusual variety, this must be entered here so that
  !     special logic can be invoked.  In this case, all the line
  !     segments have a |slope| that is > 0 and less than inif.

  data ITYP / ICLSS * 0 /

  !     XPTS: FIRST COLUMN ARE THE SMALL FUEL VALUES FOR EACH FIRE MODEL
  !       WHEN LARGE FUEL= 0 (I.E. THE X-INTERCEPT OF THE LINE). SECOND
  !       COLUMN CONTAINS THE LARGE FUEL VALUE FOR EACH FIRE MODEL WHEN
  !       SMALL FUEL=0 (I.E. THE Y-INTERCEPT OF THE LINE).

  data ((XPTS(I,J), J = 1,2), I = 1,ICLSS) /&
       &5., 15.,& ! FMD   1
       &5., 15.,& ! FMD   2
       &5., 15.,& ! FMD   3
       &5., 15.,& ! FMD   4
       &5., 15.,& ! FMD   5
       &5., 15.,& ! FMD   6
       &5., 15.,& ! FMD   7
       &5., 15.,& ! FMD   8
       &5., 15.,& ! FMD   9
       &15., 30.,& ! FMD  10 ! shares with 11
       &15., 30.,& ! FMD  11
       &30., 60.,& ! FMD  12 ! shares with 14
       &45.,100.,& ! FMD  13
       &30., 60./ ! FMD  14

  !     INITIALLY SET ALL MODELS OFF; NO TWO CANDIDATE MODELS ARE COLINEAR, AND COLINEARITY
  !     WEIGHTS ARE ZERO. IF TWO CANDIDATE MODELS ARE COLINEAR, THE WEIGHTS MUST BE SET, AND
  !     MUST SUM TO 1, WRT EACH OTHER. FMDYN WILL RESCALE COLINEAR WEIGHTS SO THAT THEY SUM
  !     TO ONE, IF THEY DON'T ALREADY!

  do I = 1,ICLSS
     EQWT(I) = 0.0
  end do

  !     BEGIN ROUTINE

  call DBCHK (DEBUG,'FMCFMD',6,ICYC)

  if (DEBUG) write(JOSTND,1) ICYC,IYR,LUSRFM
  1 format(' FMCFMD CYCLE= ',I2,' IYR=',I5,' LUSRFM=',L5)

  !     IF USER-SPECIFIED FM DEFINITIONS, THEN WE ARE DONE.

  if (LUSRFM) return

  if (DEBUG) write(JOSTND,7) ICYC,IYR,HARVYR,LDYNFM,FMKOD,&
       &SMALL,LARGE
  7 format(' FMCFMD CYCLE= ',I2,' IYR=',I5,' HARVYR=',I5,&
       &' LDYNFM=',L2,' FMKOD=',I4,&
       &' SMALL=',F7.2,' LARGE=',F7.2)

  !     COMPUTE TOTAL STAND BASAL AREA AND PROPORTION IN PONDEROSA
  !     PLUS DOUGLAS-FIR

  STNDBA = 0.0
  do I = 1,MAXSP
     STNDBA = STNDBA + FMTBA(I)
  end do

  PRDF = 0.0
  PRPP = 0.0
  if (STNDBA .gt.0.01) then
     PRDF = FMTBA(3) /STNDBA
     PRPP = FMTBA(10)/STNDBA
  end if

  !     3 - RESOLVE DYNAMIC FMD
  !     RULES ARE BASED ON A TABLE GIVEN BY LES HOLSAPPLE. DON ROBINSON AT
  !     ESSA DERIVED THIS LOGIC BASED ON THOSE VALUES. TRANSITION CUTPOINTS
  !     WERE MODIFIED AS FOLLOWS: %CC [ PERCOV ] CUTPOINTS WERE ADJUSTED SO
  !     THAT THEY BEGIN TO CHANGE -5% FROM THE CUTPOINT AND FINISH CHANGING
  !     +5% ABOVE THE CUTPOINT.

  X(1) = 0.40
  X(2) = 0.60
  J = 2

  WT1(J) = ALGSLP(MAX(PRDF,PRPP),X,Y,2)
  WT1(J-1) = 1.0 - WT1(J)

  !     NEITHER PONDEROSA NOR DOUGLAS-FIR DOMINATES

  if (WT1(1) .gt.0.0) then

     !     CALL A SIMPLIFIED VERSION OF /BASE/SRC/SSTAGE (01/16/02 VERSION)
     !     TO COMPUTE COVER IN THE UPPER (COVA) AND LOWER (COVB) CANOPY (ALL
     !     IN UPPER IF <2 CANOPY LAYERS IDENTIFIED. LOGICAL VECTOR VL1()
     !     IDENTIFIES MEMBERS OF THE UPPER CANOPY, TO BE USED IN FINDINGING
     !     THE STAND SIZE CLASS.

     call BMSTAGE(COVA,COVB,VL1)
     call BMSZCLS(DBH,FMPROB,VL1,ITRN,WD)

     !       SZCLS - SIZE CLASS SYSTEM USED IN BM (13 GROUPS IN ALL)
     !       COVA  - % CANOPY COVER IN UPPER STRATUM
     !       COVB  - % CANOPY COVER IN LOWER STRATUM

     do K = 1,13
        if (WD(K) .gt.0.0) then
           SZCLS = SCLAB(K)

           if (SZCLS .le.3.0) then

              EQWT(5) = EQWT(5) + WT1(1) * WD(K)

           else if (SZCLS .gt.3.0.and. SZCLS .lt.7.0) then

              X(1) = 25.0
              X(2) = 35.0
              J = 2

              WT2(J) = ALGSLP(PERCOV,X,Y,2)
              WT2(J-1) = 1.0 - WT2(J)

              if (WT2(1) .gt.0.0) then
                 EQWT(5) = EQWT(5) + WT1(1) * WT2(1) * WD(K)
              end if

              if (WT2(2) .gt.0.0) then
                 EQWT(8) = EQWT(8) + WT1(1) * WT2(2) * WD(K)
              end if

           else !SZCLS >7

              X(1) = 25.0
              X(2) = 35.0
              J = 2
              WT2(J) = ALGSLP(PERCOV,X,Y,2)
              WT2(J-1) = 1.0 - WT2(J)

              if (WT2(1) .gt.0.0) then
                 EQWT(5) = EQWT(5) + WT1(1) * WT2(1) * WD(K)
              end if

              if (WT2(2) .gt.0.0) then

                 X(1) = 25.0
                 X(2) = 35.0
                 J = 2
                 WT3(J) = ALGSLP(COVA,X,Y,2)
                 WT3(J-1) = 1.0 - WT3(J)

                 X(1) = 10.0
                 X(2) = 20.0
                 J = 2
                 WT4(J) = ALGSLP(COVB,X,Y,2)
                 WT4(J-1) = 1.0 - WT4(J)

                 EQWT(8) = EQWT(8 ) + WT1(1) * WT2(2) * WT3(1) * WT4(1)&
                      &* WD(K)
                 EQWT(8) = EQWT(8 ) + WT1(1) * WT2(2) * WT3(1) * WT4(2)&
                      &* WD(K)
                 EQWT(8) = EQWT(8 ) + WT1(1) * WT2(2) * WT3(2) * WT4(1)&
                      &* WD(K)

                 EQWT(10) = EQWT(10) + WT1(1) * WT2(2) * WT3(2) * WT4(2)&
                      &* WD(K)

              end if
           end if
        end if
     end do
  end if

  !     CASE 2: DOUGLAS-FIR OR PONDEROSA DOMINATES BASAL AREA

  if (WT1(2) .gt.0.0) then

     do I = 1,4
        WT2(I) = 0.0
     end do

     if (PERCOV .lt.15.0) then
        X(1) = 5.0
        X(2) = 15.0
        J = 2
     else if (PERCOV .ge.15.0.and. PERCOV .lt.35.0) then
        X(1) = 25.0
        X(2) = 35.0
        J = 3
     else if (PERCOV .ge.35.0) then
        X(1) = 45.0
        X(2) = 55.0
        J = 4
     end if
     WT2(J) = ALGSLP(PERCOV,X,Y,2)
     WT2(J-1) = 1.0 - WT2(J)

     if (WT2(1) .gt.0.0) then
        EQWT(1) = EQWT(1) + WT1(2) * WT2(1)
     end if

     if (WT2(2) .gt.0.0) then
        EQWT(2) = EQWT(2) + WT1(2) * WT2(2)
     end if

     if (WT2(3) .gt.0.0) then

        X(1) = 0.40
        X(2) = 0.60
        J = 2
        WT3(J) = ALGSLP((PRPP/(PRPP+PRDF)),X,Y,2)
        WT3(J-1) = 1.0 - WT3(J)

        if (WT3(1) .gt.0.0) then
           EQWT(10) = EQWT(10) + WT1(2) * WT2(3) * WT3(1)
        end if

        if (WT3(2) .gt.0.0) then
           EQWT(2) = EQWT(2)  + WT1(2) * WT2(3) * WT3(2)
        end if

     end if

     if (WT2(4) .gt.0.0) then
        EQWT(9) = EQWT(9) + WT1(2) * WT2(4)
     end if

  end if

  !     END OF DETAILED LOW FUEL MODEL SELECTION

  !     DURING THE 5 YEARS AFTER AN ENTRY, AND ASSUMING THAT SMALL+LARGE
  !     ACTIVIVITY FUELS HAVE JUMPED BY 10%, THEN MODEL 11 AND 14 ARE
  !     CANDIDATE MODELS, SHARING WITH 10 AND 12 RESPECTIVELY. THE
  !     WEIGHT OF THE SHARED RELATIONSHIP DECLINES FROM PURE 11 INITIALLY,
  !     TO PURE 10 AFTER THE PERIOD EXPIRES. SIMILARLY, COMPUTE WEIGHT FOR
  !     MODEL 14 ACTIVITY FUELS, SHARED WITH CURRENT MODEL 12. THE
  !     RELATIONSHIP CHANGES IN THE SAME WAS AS THE 10/11 FUELS.

  AFWT = MAX(0.0, 1.0 - (IYR - HARVYR) / 5.0)
  if (SLCHNG .ge. SLCRIT .or. LATFUEL) then
     LATFUEL = .true.
     EQWT(11) = AFWT
     EQWT(14) = AFWT
     if (AFWT .le.0.0) LATFUEL = .false.
  end if
  if (.not. LATFUEL) AFWT = 0.0

  !     MODELS 10,12,13 ARE ALWAYS CANDIDATE MODELS FOR NATURAL FUELS
  !     OTHER MODELS ARE ALSO CANDIDATES, DEPENDING ON COVER TYPE, ETC

  EQWT(10) = 1.0 - AFWT
  EQWT(12) = 1.0 - AFWT
  EQWT(13) = 1.0

  !     CALL FMDYN TO RESOLVE WEIGHTS, SORT THE WEIGHTED FUEL MODELS
  !     FROM THE HIGHEST TO LOWEST, SET FMD (USING THE HIGHEST WEIGHT)

  call FMDYN(SMALL,LARGE,ITYP,XPTS,EQWT,IPTR,ICLSS,LDYNFM,FMD)

  if (DEBUG) write (JOSTND,8) FMD
  8 format (' FMCFMD, FMD=',I4)

  return
end

  !     THIS IS CRIBBED FROM SSTAGE, AND STRIPPED SO THAT IT YIELDS ONLY
  !     THE FIRST CANOPY BREAKPOINT, IF ANY.

subroutine BMSTAGE(CCA,CCB,LA)
  implicit none

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'

  !     INDEX: HT-ORDERED LIST OF TREE INDEX
  !     LA:    INDEX OF MEMBERS IN LAYER "A"

  ! Variables
  integer :: INDEX(MAXTRE)
  logical :: LA(MAXTRE)

  equivalence (WK5,INDEX)

  real    :: CCA,CCB

  real    :: X,DIFF,DIFF1,SUMPRB,XHT,SPROB
  real    :: CRS1,CRS2
  integer :: II,NSTR
  integer :: ID1I1,ID1I2,ID2I1,ID2I2
  integer :: IS1I1,IS1I2,IS2I1,IS2I2
  integer :: IILG,ILARGE,ISMALL
  integer :: IS1OK,IS2OK

  integer :: I,NTREES

  !     INITIALIZE

  XHT = 0.0
  CRS1 = 0.0
  CRS2 = 0.0
  do I = 1,MAXTRE
     LA(I) = .false.
  end do

  !     LOAD THE INDEX WITH TREES THAT REPRESENT OVER A LIMITED
  !     TREES/ACRE

  NTREES = 0
  SPROB = 0.0
  do I = 1,ITRN
     SPROB = SPROB + PROB(I)
     if (PROB(I) .gt.0.00001) then
        NTREES = NTREES + 1
        INDEX(NTREES) = I
     end if
  end do
  if (NTREES .eq.0) goto 100

  !     1-RECORD SPECIAL CASE

  if (NTREES .le.1) then
     I = INDEX(1)
     WK6(I) = CRWDTH(I)
     CRS1 = WK6(I) * WK6(I) * PROB(I) * 0.785398 / 43560.0
     IS1I1 = 1
     IS1I2 = 1
     goto 80
  end if

  call RDPSRT (NTREES,HT,INDEX,.false.)

  !     CALCULATE THE CCF-BASED "CROWN WIDTH" FOR EACH TREE...AND
  !     CONVERT THIS TO CROWN COVER.

  do II = 1,NTREES
     I = INDEX(II)
     WK6(I) = CRWDTH(I)
     WK6(I) = WK6(I) * WK6(I) * PROB(I) * 0.785398
  end do

  !     FIND THE LARGEST DIFFERENCE: DIFF1

  !     THE FOLLOWING POINTERS POINT TO THE FIRST AND THE LAST TREE
  !     RECORD IN A GAP.  THAT IS, A GAP MAY SPAN MORE THAN ONE TREE
  !     RECORD (IN THE CASE THAT WE FIND INSIGNIFICANT "LADDER TREES").

  DIFF1 = -1E20
  ID1I1 = 0
  ID1I2 = 0
  ID2I1 = 0
  ID2I2 = 0

  IILG = 1
  ILARGE = INDEX(IILG)
  SUMPRB = 0.0

  do II = 2,NTREES
     ISMALL = INDEX(II)

     !       IF THE SMALLER TREE CREATES A GAP...
     !       (A GAP MUST BE AT LEAST 10 FEET or 30% OF THE HEIGHT)...
     !       THE CONTRIBUTION OF THE TREE MUST BE AT LEAST 2% BEFORE
     !       THE GAP IS COUNTED

     X = MAX(10.0, HT(ILARGE) * 30.0 * 0.01)
     if (HT(ISMALL) .lt. HT(ILARGE)-X) then
        if (PROB(ISMALL) + SUMPRB .lt.2.0) then
           SUMPRB = SUMPRB + PROB(ISMALL)
        else
           DIFF = HT(ILARGE) - HT(ISMALL)
           if (DIFF .gt. DIFF1) then
              DIFF1 = DIFF
              ID1I1 = IILG
              ID1I2 = II
           end if
           ILARGE = ISMALL
           IILG = II
           SUMPRB = 0.0
        end if
     else

        !       IF THE SMALLER TREE DOES NOT CREATE A GAP...THEN SEE IF
        !       IF IT IS AN INSIGNIFICANT "LADDER" TREE. IF IT IS AN
        !       INSIGNIFICANT LADDER...AND NO GAP WAS FOUND. RESET THE
        !       SUMMATION AND MOVE THE "TOP" TREE DOWN.

        if (PROB(ISMALL) + SUMPRB .lt.2.0) then
           SUMPRB = SUMPRB + PROB(ISMALL)
        else
           ILARGE = ISMALL
           IILG = II
           SUMPRB = 0.0
        end if
     end if
  end do

  !     THESE POINTERS POINT TO THE TOP AND BOTTOM OF EACH STRATUM,
  !     IF ID1I1 HAS BEEN SET, THEN THERE ARE 2 POTENTIAL STRATA (NSTR)

  NSTR = 1
  IS1I1 = 1
  IS1I2 = NTREES
  IS2I1 = 0
  IS2I2 = 0
  if (ID1I1.gt.0) then
     NSTR = 2
     IS1I2 = ID1I1
     IS2I1 = ID1I2
     IS2I2 = NTREES
  end if

  !     THE FIRST POTENTIAL STRATUM CONTAINS TREES FROM 1 TO ID1I1
  !     THE SECOND CONTAINS TREES FROM ID1I2 TO ID2I1
  !     COMPUTE AND SAVE THE COVER IN EACH POTENTIAL STRATUM AND
  !     CHECK FOR MIN COVER.

  !     CHECK THE FIRST (UPPER) LAYER
  !     FOR THE PURPOSES OF COMPUTING THE COVER, INCLUDE THE TREES THAT
  !     ARE WITHIN THE GAP IN THE UPPER STRATUM.

  IS1OK = 0
  CRS1 = 0.0
  IS1I2 = MAX(IS1I2,IS2I1-1)
  call COVOLP (.false.,JOSTND,IS1I2-IS1I1+1,INDEX(IS1I1),WK6,CRS1,&
       &CCCOEF)
  if (CRS1.gt.5.0) IS1OK = 1

  !     CHECK THE SECOND (LOWER) LAYER

  IS2OK = 0
  CRS2 = 0.0
  if (NSTR .ge.2) then
     call COVOLP (.false.,JOSTND,IS2I2-IS2I1+1,INDEX(IS2I1),WK6,CRS2,&
          &CCCOEF)
     if (CRS2.gt.5.0) IS2OK = 1
  end if

  !     COUNT THE VALID (>5%) STRATA
  !     IF THERE ARE NOT VALID STRATA AND IF THE NUMBER OF TREES ARE
  !     UNDER 200, THEN WE ARE DONE.  OTHERWISE, FORM ONE STRATUM OF ALL
  !     THE TREES AND RECOMPUTE COVER.

  NSTR = IS1OK + IS2OK
  if (NSTR.eq.0) then
     if (SPROB .lt.200.0) goto 80
     CRS2 = 0.0
     IS1I1 = 1
     IS1I2 = NTREES
     call COVOLP (.false.,JOSTND,IS1I2,INDEX(IS1I1),WK6,CRS1,&
          &CCCOEF)
  end if

  80 CCA = CRS1
  CCB = CRS2

  do I = IS1I1,IS1I2
     LA(INDEX(I)) = .true.
  end do

  100 continue

  return
end

  !     COMPUTE THE SIZE CLASS OF TREES IN THE UPPER CANOPY

  !     D  = REAL VECTOR OF DBH
  !     P  = REAL VECTOR OF STEMS/ACRE
  !     VL = LOGICAL VECTOR OF MEMEBERSHIP IN TOP LAYER
  !     I  = INTEGER LENGTH OF D() AND VL() VECTORS
  !     S  = WT (BA-BASE) OF EACH SIZE CLASS
  !     C  = COMPLEXITY OF SIZE CLASS (1=SIMPLE, 2=COMPLEX)
  !     WDOM = WEIGHTED DOMINANT CATEGORY [ RETURNED ]

subroutine BMSZCLS(D,P,VL,N,WDOM)
  implicit none

  ! Variables
  real    :: D,P
  logical :: VL
  integer :: N
  real    :: WDOM(13)
  dimension D(N),P(N),VL(N)

  integer :: I,J,ICLS,CCLS,C(13),PASS
  real    :: BA,DT,S(13),SWT,XRAN,JITTER
  double precision :: SAVESO

  data PASS /50/, JITTER /0.2/

  !     ZERO BINS TO WEIGHTED DOMINANT CATEGORY AND COMPLEXITY INDEX

  do I = 1,13
     WDOM(I) = 0.0
     C(I) = 1
  end do

  !     POPULATE BASIC SIZE CATEGORIES USING UNIFORM RANDOM JITTER.
  !     THE IDEA HERE IS TO SAMPLE DIAMETERS JITTERED
  !     BY 20%, THEN RECOMPUTE A WEIGHTED DOMINANT CATEGORY.

  call RANNGET(SAVESO)

  do J = 1,PASS

     !     ZERO BINS TO HOLD BA-BASED WEIGHTS FOR THE BLUE MOUNTAINS
     !     SIZE CATEGORIES

     do I = 1,13
        S(I) = 0.0
     end do

     do I = 1,N
        if (VL(I)) then
           call RANN(XRAN)
           DT = D(I) * (1.0 + JITTER * ((XRAN * 2.0) - 1.0))
           BA = DT * DT * P(I)
           if (DT .ge.0.0.and. DT .lt.1.0) then ! SEEDLING    [1]
              S(1) = S(1) + BA
              C(1) = 1
           else if (DT .ge.1.0.and. DT .lt.5.0) then ! SAPLING     [2]
              S(2) = S(2) + BA
              C(2) = 1
           else if (DT .ge.5.0.and. DT .lt.9.0) then ! POLE        [3]
              S(3) = S(3) + BA
              C(3) = 1
           else if (DT .ge.9.0.and. DT .lt.15.0) then ! SMALLTREE:9-15  [4]
              S(4) = S(4) + BA
              C(4) = 1
           else if (DT .ge.15.0.and. DT .lt.21.0) then ! SMALLTREE:15-21 [5]
              S(5) = S(5) + BA
              C(5) = 1
           else if (DT .ge.21.0.and. DT .lt.32.0) then ! MEDIUMTREE  [6]
              S(6) = S(6) + BA
              C(6) = 1
           else ! LARGETREE   [7]
              S(7) = S(7) + BA
              C(7) = 1
           end if
        end if
     end do

     !     CREATE COMPOSITE SIZE CATEGORIES

     S(8) = S(1)  + S(2) ! SEEDLING+SAPLING                        [8]
     C(8) = 2

     S(9) = S(2)  + S(3) ! SAPLING+POLE                            [9]
     C(9) = 2

     S(10) = S(4)  + S(5) ! SMALLTREE(9-21)                        [10]
     C(10) = 2

     S(11) = S(3)  + S(10) ! POLE+SMALLTREE(9-21)                   [11]
     C(11) = 3

     S(12) = S(10) + S(6) ! SMALLTREE(9-21)+MEDIUM                 [12]
     C(12) = 3

     S(13) = S(6)  + S(7) ! MEDIUM+LARGE                           [13]
     C(13) = 2

     ICLS = 0
     CCLS = 4
     SWT = -1.0
     do I = 1,13
        if (S(I) .eq. SWT) then
           if (C(I) .lt. CCLS) then
              CCLS = C(I)
              ICLS = I
           end if
        else
           if (S(I) .gt. SWT) then
              SWT = S(I)
              CCLS = C(I)
              ICLS = I
           end if
        end if
     end do

     WDOM(ICLS) = WDOM(ICLS) + 1.0/FLOAT(PASS)

  end do

  call RANNPUT(SAVESO)

  return
end
