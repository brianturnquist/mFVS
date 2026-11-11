!======================!
!   fire/ci : fmcfmd   !
!======================!
! FIRE-CI $Id$
subroutine FMCFMD (IYR, FMD)
  implicit none

  !     SINGLE-STAND VERSION
  !     CALLED FROM: FMBURN
  !  PURPOSE:
  !     THIS SUBROUTINE RETURNS TWO TYPES OF INFORMATION: THE FUEL MODEL
  !     THAT WOULD BE USED IF THE STATIC FUEL MODEL OPTION IS SELECTED
  !     (STORED AS IFMD(1), WITH A WEIGTH OF FWT(1)=1.0 AND THE CLOSEST
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

  integer :: I,J,K,II,ICT
  integer :: IPTR(ICLSS),ITYP(ICLSS)
  real    :: XPTS(ICLSS,2),EQWT(ICLSS),WT1(4),WT2(4)
  real    :: STNDBA,X(2),Y(2),AFWT
  real    :: ALGSLP
  real    :: PRLONG,CCGF,CRGF,FMTPA
  real    :: CAREA,CWIDTH,TOTCRA
  logical :: DEBUG,LCRGF

  !     FIXED VALUES FOR INTERPOLATION FUNCTION

  data     Y / 0.0, 1.0 /

  !     THESE ARE THE INTEGER TAGS ASSOCIATED WITH EACH FIRE MODEL
  !     CLASS. THEY ARE RETURNED WITH THE WEIGHT

  data IPTR / 1,2,3,4,5,6,7,8,9,10,11,12,13,14 /

  !     THESE ARE 0 FOR REGULAR LINES, -1 FOR HORIZONTAL AND 1 FOR
  !     VERTICAL LINES. IF ANY OF THE LINES DEFINED BY XPTS() ARE OF
  !     AN UNUSUAL VARIETY, THIS MUST BE ENTERED HERE SO THAT
  !     SPECIAL LOGIC CAN BE INVOKED.  IN THIS CASE, ALL THE LINE
  !     SEGMENTS HAVE A |SLOPE| THAT IS > 0 AND LESS THAN INIF.

  data ITYP / ICLSS * 0 /

  !     XPTS: FIRST COLUMN ARE THE SMALL FUEL VALUES FOR EACH FIRE MODEL
  !     WHEN LARGE FUEL= 0 (I.E. THE X-INTERCEPT OF THE LINE). SECOND
  !     COLUMN CONTAINS THE LARGE FUEL VALUE FOR EACH FIRE MODEL WHEN
  !     SMALL FUEL=0 (I.E. THE Y-INTERCEPT OF THE LINE).

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
  !     MUST SUM TO 1, WRT EACH OTHER

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

  !     COMPUTE TOTAL STAND BASAL AREA AND PROPORTION IN LONG-NEEDLE
  !     SPECIES: WHITE PINE AND PONDEROSA PINE

  STNDBA = 0.0
  PRLONG = 0.0
  do I = 1, MAXSP
     STNDBA = STNDBA + FMTBA(I)
     select case (I)
     case (1,10)
        PRLONG = PRLONG + FMTBA(I)
     end select
  end do

  if (PRLONG .gt.0.01.and. STNDBA .gt.0.01) then
     PRLONG = PRLONG / STNDBA
  else
     PRLONG = 0.0
  end if

  !     3 - RESOLVE DYNAMIC FMD

  do I = 1,4
     WT1(I) = 0.0
  end do

  !     FIND POTENTIAL VEGETATION GROUP (PVG) **FMCMBA**
  !     **NOTE** % SHORT VS %LONG: 100% SHORT -> 8; 100% LONG -> 9
  !     IS FREQUENTLY USED TO WEIGHT THE SELECTION OF MODEL 8/9

  call CIPVG(ICT)
  select case (ICT)

     !       1 = DRY PONDEROSA PINE - XERIC DOUGLAS-FIR,
     !       2 = WARM/DRY DOUGLAS-FIR - MOIST PONDEROSA PINE,
     !       3 = COOL MOIST DOUGLAS-FIR
     !       4 = COOL DRY DOUGLAS-FIR

  case (1:4)

     select case (ICT)
     case (1)
        K = 1
     case (2)
        call CIS9B(I) ! IS THIS NINEBARK OR SNOWBERRY? **FMCBA**
        if (I .eq.1) then
           K = 5
        else
           K = 2
        end if
     case (3)
        K = 5
     case (4)
        K = 2
     end select

     X(1) = 30.0
     X(2) = 50.0
     J = 2

     WT1(J) = ALGSLP(PERCOV,X,Y,2)
     WT1(J-1) = 1.0 - WT1(J)

     if (WT1(1) .gt.0.0) then
        EQWT(K) = EQWT(K) + WT1(1)
     end if

     if (WT1(2) .gt.0.0) then
        EQWT(9) = EQWT(9) + WT1(2) * PRLONG
        EQWT(8) = EQWT(8) + WT1(2) * (1.0 - PRLONG)
     end if

     !       5 = DRY GRAND FIR
     !       6 = WET GRAND FIR

  case (5:6)

     select case (ICT)
     case (5)
        K = 2
     case (6)
        K = 5
     end select

     !         COMPUTE DETAILS ON GF < 3" DBH: DO THEY HAVE, ON
     !         AVERAGE, >75% CROWN AND WHAT %CC DO THEY FORM?

     LCRGF = .false.
     CCGF = 0.0
     if (ISCT(4,1) .gt.0) then
        CRGF = 0.0
        FMTPA = 0.0
        TOTCRA = 0.0
        do II = ISCT(4,1),ISCT(4,2)
           I = IND1(II)
           if (DBH(I) .le.3.0) then
              CRGF = CRGF + (FMPROB(I) * ICR(I))
              FMTPA = FMTPA + FMPROB(I)
              CWIDTH = CRWDTH(I)
              CAREA = 3.1415927 * CWIDTH * CWIDTH / 4.0
              TOTCRA = TOTCRA + CAREA * FMPROB(I)
           end if
        end do
        if (FMTPA .le.0.) then
           CRGF = 0.
        else
           CRGF = CRGF / FMTPA
        end if
        LCRGF = (CRGF .ge.75.0)
        CCGF = 100.0 * (1.0 - EXP(-TOTCRA/43560.))
     end if

     if (LCRGF) then

        X(1) = 50.0
        X(2) = 70.0
        J = 2

        WT1(J) = ALGSLP(CCGF,X,Y,2)
        WT1(J-1) = 1.0 - WT1(J)

        if (WT1(1) .gt.0.0) then

           X(1) = 40.0
           X(2) = 60.0
           J = 2

           WT2(J) = ALGSLP(PERCOV,X,Y,2)
           WT2(J-1) = 1.0 - WT2(J)

           if (WT2(1) .gt.0.0) then
              EQWT(K) = EQWT(K) + WT1(1) * WT2(1)
           end if

           if (WT2(2) .gt.0.0) then
              EQWT(9) = EQWT(9) + WT1(1) * WT2(2) * PRLONG
              EQWT(8) = EQWT(8) + WT1(1) * WT2(2) * (1.0 - PRLONG)
           end if

        end if

        !           NOTE DISCONTINUITY: MODEL SWITCHES COMPLETELY TO 5 WHEN WT1(2) = 1.0
        !           WHILE ITS APPROACH TO THIS LIMIT IS ALONG THE 9/8 TRAJECTORY. PLUS
        !           CA CHANGE.

        if (WT1(2) .gt.0.0) then

           if (WT1(2) .eq.1.0) then

              EQWT(5) = EQWT(5) + WT1(2)

           else

              X(1) = 40.0
              X(2) = 60.0
              J = 2

              WT2(J) = ALGSLP(PERCOV,X,Y,2)
              WT2(J-1) = 1.0 - WT2(J)

              if (WT2(1) .gt.0.0) then
                 EQWT(5) = EQWT(5) + WT1(2) * WT2(1)
              end if

              if (WT2(2) .gt.0.0) then
                 EQWT(9) = EQWT(9) + WT1(2) * WT2(2) * PRLONG
                 EQWT(8) = EQWT(8) + WT1(2) * WT2(2) * (1.0 - PRLONG)
              end if

           end if
        end if

     else ! NO SIG LONG-CROWN GF UNDERSTOREY

        X(1) = 40.0
        X(2) = 60.0
        J = 2

        WT1(J) = ALGSLP(PERCOV,X,Y,2)
        WT1(J-1) = 1.0 - WT1(J)

        if (WT1(1) .gt.0.0) then
           EQWT(K) = EQWT(K) + WT1(1)
        end if

        if (WT1(2) .gt.0.0) then
           EQWT(9) = EQWT(9) + WT1(2) * PRLONG
           EQWT(8) = EQWT(8) + WT1(2) * (1.0 - PRLONG)
        end if

     end if

     !       7  = WARM DRY SUBALPINE FIR
     !       8  = WET SUBALPINE FIR
     !       9  = HIGH WATER TABLE SUBALPINE FIR
     !       10 = PERSISTENT LODGEPOLE PINE
     !       11 = HIGH ELEVATION SUBALPINE FIR W/ WHITEBARK PINE

  case (7:11)

     EQWT(8) = 1.0

  end select

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
