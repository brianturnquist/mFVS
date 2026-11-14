!======================!
!   fire/cs : fmcfmd   !
!======================!
! FIRE-CS $Id$
subroutine FMCFMD (IYR, FMD)
  implicit none

  !  SINGLE-STAND VERSION
  !  CALLED FROM: FMBURN
  !  PURPOSE:
  !     THIS SUBROUTINE RETURNS TWO TYPES OF INFORMATION: THE FUEL MODEL
  !     THAT WOULD BE USED IF THE STATIC FUEL MODEL OPTION IS SELECTED
  !     (STORED AS IFMD(1), WITH A WEIGTH OF FWT(1)=1.0 AND THE CLOSEST
  !     THE CLOSEST FUEL MODELS (UP TO 4) AND THEIR WEIGHTINGS FOR USE
  !     BY THE DYNAMIC FUEL MODEL OPTION

  !  CALL LIST DEFINITIONS:
  !     FMD:     FUEL MODEL NUMBER

  !  COMMON BLOCK VARIABLES AND PARAMETERS:
  !     SMALL:   SMALL FUELS FROM DYNAMIC FUEL MODEL
  !     LARGE:   LARGE FUELS FROM DYNAMIC FUEL MODEL

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'FMFCOM.f90'
  include 'FMCOM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90' ! since using FIA algorithm info

  !     LOCAL VARIABLE DECLARATIONS

  ! Variables
  integer :: ICLSS
  parameter(ICLSS = 14)

  integer :: IYR,FMD
  integer :: IPTR(ICLSS),ITYP(ICLSS)
  integer :: J,I,IFFEFT

  real    :: XPTS(ICLSS,2),EQWT(ICLSS)
  real    :: X,Y,RCHT,RCTPA,MOISWT(ICLSS)
  logical :: DEBUG

  !  THESE ARE THE INTEGER TAGS ASSOCIATED WITH EACH FIRE MODEL
  !  CLASS. THEY ARE RETURNED WITH THE WEIGHT

  data IPTR / 1,2,3,4,5,6,7,8,9,10,11,12,13,14 /

  !  THESE ARE 0 FOR REGULAR LINES, -1 FOR HORIZONTAL AND 1 FOR
  !  VERTICAL LINES. IF ANY OF THE LINES DEFINED BY XPTS() ARE OF
  !  AN UNUSUAL VARIETY, THIS MUST BE ENTERED HERE SO THAT
  !  SPECIAL LOGIC CAN BE INVOKED.  IN THIS CASE, ALL THE LINE
  !  SEGMENTS HAVE A |SLOPE| THAT IS > 0 AND LESS THAN INIF.

  data ITYP / ICLSS * 0 /

  !  XPTS: FIRST COLUMN ARE THE SMALL FUEL VALUES FOR EACH FIRE MODEL
  !  WHEN LARGE FUEL= 0 (I.E. THE X-INTERCEPT OF THE LINE). SECOND
  !  COLUMN CONTAINS THE LARGE FUEL VALUE FOR EACH FIRE MODEL WHEN
  !  SMALL FUEL=0 (I.E. THE Y-INTERCEPT OF THE LINE).

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
       &10., 30.,& ! FMD  10 ! moved line based on SN workshop input
       &15., 30.,& ! FMD  11
       &30., 60.,& ! FMD  12
       &45.,100.,& ! FMD  13
       &30., 60./ ! FMD  14

  !  INITIALLY SET ALL MODELS OFF; NO TWO CANDIDATE MODELS ARE
  !  COLINEAR, AND COLINEARITY WEIGHTS ARE ZERO. IF TWO CANDIDATE
  !  MODELS ARE COLINEAR, THE WEIGHTS MUST BE SET, AND
  !  MUST SUM TO 1, WRT EACH OTHER

  do I = 1,ICLSS
     EQWT(I) = 0.0
  end do

  !  BEGIN ROUTINE

  call DBCHK (DEBUG,'FMCFMD',6,ICYC)

  if (DEBUG) write(JOSTND,1) ICYC,IYR,LUSRFM
  1 format(' FMCFMD CYCLE= ',I2,' IYR=',I5,' LUSRFM=',L5)

  !  IF USER-SPECIFIED FM DEFINITIONS, THEN WE ARE DONE.

  if (LUSRFM) return

  if (DEBUG) write(JOSTND,7) ICYC,IYR,HARVYR,LDYNFM,PERCOV,FMKOD,&
       &SMALL,LARGE
  7 format(' FMCFMD CYCLE= ',I2,' IYR=',I5,' HARVYR=',I5,&
       &' LDYNFM=',L2,' PERCOV=',F7.2,' FMKOD=',I4,&
       &' SMALL=',F7.2,' LARGE=',F7.2)

  !  DETERMINE FFE FOREST TYPE (1 OF 8 CATEGORIES) FROM FIA FOR. TYPE

  call FMSNFT(IFFEFT)

  !  LOW FUEL MODEL SELECTION
  !  FIRST, WE NEED TO SEE WHICH FUEL MODELS SHOULD BE SELECTED
  !  BASED ON THE ABOVE IFFEFT.
  !  THE AMOUNT OF SMALL AND LARGE FUEL PRESENT CAN MODIFY THE CHOICE
  !  OF MODEL.  (SEE CODE BELOW)

  select case (IFFEFT)
  case (1, 2, 3) ! hardwood, hardwood/pine, or pine/hardwood
     if (SMALL .gt.6) then
        EQWT(5) = 1.0
     else
        if (MOIS(1,4) .le.0.15) then
           MOISWT(9) = 1.0
        else if (MOIS(1,4) .gt.0.25) then
           MOISWT(8) = 1.0
        else
           MOISWT(9) = 1.0 - (MOIS(1,4) - 0.15)/0.1
           MOISWT(8) = 1.0 - (0.25 - MOIS(1,4))/0.1
        end if

        if (SMALL .le.4) then
           EQWT(8) = MOISWT(8)
           EQWT(9) = MOISWT(9)
        else
           EQWT(8) = (1.0 - (SMALL - 4)/2)*MOISWT(8)
           EQWT(9) = (1.0 - (SMALL - 4)/2)*MOISWT(9)
           EQWT(5) = 1.0 - (6 - SMALL)/2
        end if
     end if

  case (4, 8) ! pine or saint francis
     if (MOIS(1,4) .le.0.15) then
        EQWT(9) = 1.0
     else if (MOIS(1,4) .gt.0.25) then
        EQWT(8) = 1.0
     else
        EQWT(9) = 1.0 - (MOIS(1,4) - 0.15)/0.1
        EQWT(8) = 1.0 - (0.25 - MOIS(1,4))/0.1
     end if

  case (5, 6) ! pine bluestem or oak savannah
     EQWT (2) = 1.0

  case (7) ! eastern redcedar
     RCHT = 0
     RCTPA = 0
     do I = 1,ITRN
        X = HT(I)
        Y = FMPROB(I)
        select case (ISP(I))
        case (1,2) ! redcedar
           RCHT = RCHT + X
           RCTPA = RCTPA + Y
        end select
     end do

     if (RCTPA .gt.0) RCHT = RCHT/RCTPA

     if (RCHT .gt.7.5) then
        EQWT(4) = 1.0
     else if (RCHT .le.4.5) then
        EQWT(6) = 1.0
     else
        EQWT(6) = 1.0 - (RCHT - 4.5)/3
        EQWT(4) = 1.0 - (7.5 - RCHT)/3
     end if

  case (9)
     EQWT(6) = 1.0
  end select

  !  END OF DETAILED LOW FUEL MODEL SELECTION

  !  MODELS 10,12 ARE ALWAYS CANDIDATE MODELS FOR NATURAL FUELS
  !  OTHER MODELS ARE ALSO CANDIDATES, DEPENDING ON COVER TYPE, ETC
  !  MODELS 11, 13, AND 14 ARE NOT CANDIDATES IN THE OZARK FFE

  EQWT(10) = 1.0
  EQWT(12) = 1.0
  EQWT(11) = 0.0
  EQWT(13) = 0.0
  EQWT(14) = 0.0

  !  CALL FMDYN TO RESOLVE WEIGHTS, SORT THE WEIGHTED FUEL MODELS
  !  FROM THE HIGHEST TO LOWEST, SET FMD (USING THE HIGHEST WEIGHT)

  call FMDYN(SMALL,LARGE,ITYP,XPTS,EQWT,IPTR,ICLSS,LDYNFM,FMD)

  if (DEBUG) write (JOSTND,8) FMD,LDYNFM
  8 format (' FMCFMD, FMD=',I4,' LDYNFM=',L2)

  return
end
