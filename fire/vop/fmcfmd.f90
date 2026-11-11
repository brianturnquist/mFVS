!=======================!
!   fire/vop : fmcfmd   !
!=======================!
! FIRE-VOP $Id$
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

  !     PARAMETER INCLUDE FILES.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'

  !     COMMON INCLUDE FILES.

  include 'FMFCOM.f90'
  include 'FMCOM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'ARRAYS.f90'
  include 'SSTGMC.f90'

  !     LOCAL VARIABLE DECLARATIONS

  !     NUMBER OF HABITAT TYPES IN **HABTYP*

  ! Variables
  integer :: MXR6CODE
  parameter (MXR6CODE = 75)

  !     NUMBER OF POTENTIAL FIRE MODELS

  integer :: ICLSS
  parameter(ICLSS = 13)

  !     COVER METAGROUP ENUMS

  integer :: SFCT,DFCT,MHCT,RACT,LPCT,WOCT,UBCT

  parameter(&
       &SFCT = 1,& ! SF,WH,SS,RC
       &DFCT = 2,& ! DF,GF,WP
       &MHCT = 3,& ! MH,AF,WB
       &RACT = 4,& ! RA
       &LPCT = 5,& ! LP
       &WOCT = 6,& ! WO,GC,TO
       &UBCT = 6) ! UPPER BOUND

  integer :: FMD
  integer :: IYR,I,J,K,ICT(2),ICOV,IWET
  integer :: IPTR(ICLSS),ITYP(ICLSS),INDX(UBCT),IXS(MAXTRE)
  real    :: XPTS(ICLSS,2),EQWT(ICLSS),WT1(2),WT2(4),WT3(3)
  real    :: ICTWT(2),TARGBA,SUMBA,LTREES,TREEBA,SNG1
  real    :: CTBA(UBCT),STNDBA,X(2),Y(2),X1,QMD80,AFWT
  real    :: ALGSLP
  logical :: DEBUG,LQMD,LGRASS,LFORB,LSHRUB,LWET

  !     FIXED VALUES FOR INTERPOLATION FUNCTION

  data     Y / 0.0, 1.0 /

  !     These are the integer tags associated with each fire model
  !     class. They are returned with the weight

  data IPTR / 1,2,3,4,5,6,7,8,9,10,11,12,13 /

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
       &30., 60.,& ! FMD  12
       &45.,100./ ! FMD  13

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

  if (DEBUG) write(JOSTND,7) ICYC,IYR,HARVYR,LDYNFM,PERCOV,FMKOD,&
       &SMALL,LARGE
  7 format(' FMCFMD CYCLE= ',I2,' IYR=',I5,' HARVYR=',I5,&
       &' LDYNFM=',L2,' PERCOV=',F7.2,' FMKOD=',I4,&
       &' SMALL=',F7.2,' LARGE=',F7.2)

  !     DETERMINE WHETHER HABITAT IS FORB, GRASS OR SHRUB; INVALID MAPPING
  !     DEFAULTS TO GRASS

  ICOV = 0
  call PNFGS(ICOV)
  LGRASS = .false.
  LFORB = .false.
  LSHRUB = .false.
  select case (ICOV)
  case (1)
     LFORB = .true.
  case (0,2)
     LGRASS = .true.
  case (3)
     LSHRUB = .true.
  end select

  !     DETERMINE WHETHER HABITAT IS MOIST, AFFECTS LPCT RULES
  !     DEFAULTS TO .FALSE. ENTRY POINT IN **FMCBA**

  call PNWET(IWET)
  LWET = .false.
  if (IWET .eq.2) LWET = .true.

  !     ZERO OUT THE COVER GROUPS AND COMPUTE TOTAL STAND BASAL AREA

  do I = 1,UBCT
     CTBA(I) = 0.0
  end do
  STNDBA = 0.0
  do I = 1,MAXSP
     STNDBA = STNDBA + FMTBA(I)
  end do

  !     FIND BA IN EACH COVER TYPE GROUP; ANYTHING NOT SPECIFIED IS
  !     PUT WITH DOUGLAS-FIR; IN THE WC-FFE SITKA SPRUCE IS POOLED WITH
  !     ENGELMANN = 10. CASE-STATEMENT VALUES CAN'T BE VARIABLES, SO
  !     THE WHOLE THING IS DUPLICATED, CHANGING THE 10 AND 6 FOR THE
  !     FIRST CASE...

  if (VARACD .eq. 'WC') then
     do I = 1,MAXSP
        select case (I)
        case (1,19,10,18)
           CTBA(SFCT) = CTBA(SFCT) + FMTBA(I)
        case (20,4,31)
           CTBA(MHCT) = CTBA(MHCT) + FMTBA(I)
        case (22)
           CTBA(RACT) = CTBA(RACT) + FMTBA(22)
        case (11)
           CTBA(LPCT) = FMTBA(11)
        case (28,25)
           CTBA(WOCT) = FMTBA(28) + FMTBA(25)
        case default ! 16,3,14 + minor
           CTBA(DFCT) = CTBA(DFCT) + FMTBA(I)
        end select
     end do
  else
     do I = 1,MAXSP
        select case (I)
        case (1,19,6,18)
           CTBA(SFCT) = CTBA(SFCT) + FMTBA(I)
        case (20,4,31)
           CTBA(MHCT) = CTBA(MHCT) + FMTBA(I)
        case (22)
           CTBA(RACT) = CTBA(RACT) + FMTBA(22)
        case (11)
           CTBA(LPCT) = FMTBA(11)
        case (28,24,25)
           CTBA(WOCT) = FMTBA(28) + FMTBA(24) + FMTBA(25)
        case default ! 16,3,14 + minor
           CTBA(DFCT) = CTBA(DFCT) + FMTBA(I)
        end select
     end do
  end if

  !     2 - FIND AND SAVE THE TWO DOMINANT GROUPS; RESCALING
  !     SO THEY SUM TO 1.0

  if ((ITRN .gt.0) .and. (STNDBA.gt.0.001)) then
     call RDPSRT (UBCT,CTBA,INDX,.true.)
     X1 = 0.0
     do I = 1,2
        ICT(I) = INDX(I)
        ICTWT(I) = CTBA(INDX(I))
        X1 = X1 + ICTWT(I)
     end do
     do I = 1,2
        ICTWT(I) = ICTWT(I)/X1
     end do
  else
     ICT(1) = OLDICT
     ICT(2) = OLDICT2
     ICTWT(1) = OLDICTWT(1)
     ICTWT(2) = OLDICTWT(2)
  end if
  OLDICT = ICT(1)
  OLDICT2 = ICT(2)
  OLDICTWT(1) = ICTWT(1)
  OLDICTWT(2) = ICTWT(2)

  !     COMPUTE QMD OF LOWER 80% OF BA TREES IF EITHER SFCT OR DFCT IS A
  !     COVER TYPE (THIS ALGORITHM BASED ON WS-FFE **CWHR**)

  LQMD = .false.
  do I = 1,2
     if ((ICT(I) .eq. SFCT) .or. (ICT(I) .eq. DFCT)) then
        LQMD = .true.
     end if
  end do
  QMD80 = 0.0
  if (LQMD) then
     TARGBA = STNDBA * 0.80
     SUMBA = 0.0
     LTREES = 0.0
     if (ITRN .gt.0) call RDPSRT(ITRN,DBH,IXS,.true.)
     do J = ITRN,1,-1
        I = IXS(J)
        TREEBA = FMPROB(I) * DBH(I) * DBH(I) * 0.0054542
        if ((SUMBA + TREEBA) .lt. TARGBA) then
           SUMBA = SUMBA + TREEBA
           QMD80 = QMD80 + (FMPROB(I) * DBH(I) * DBH(I))
           LTREES = LTREES + FMPROB(I)
        else
           if (TREEBA .gt.0) then
              SNG1 = (TARGBA - SUMBA) / TREEBA
           else
              SNG1 = 0
           end if
           LTREES = LTREES + (FMPROB(I) * SNG1)
           QMD80 = QMD80 + (DBH(I) * DBH(I) * FMPROB(I) * SNG1)
           goto 100
        end if
     end do
     100 continue
     if (LTREES .gt.0.0) QMD80 = SQRT(QMD80/LTREES)
  end if

  !     3 - RESOLVE DYNAMIC FMD
  !     RULES ARE BASED ON DIAGRAMS DEVELOPED AT PN/WC FFE WORKSHOP. TRANSITION
  !     CUTPOINTS  WERE MODIFIED AS FOLLOWS: %CC [ PERCOV ] CUTPOINTS WERE
  !     ADJUSTED SO THAT THEY BEGIN TO CHANGE -10% FROM THE CUTPOINT AND FINISH
  !     CHANGING +10% ABOVE THE CUTPOINT. SIZE [ QMD ] CUTPOINTS ARE ADJUSTED
  !     USING -1" AND +1" ABOUT THE CUTPOINT. THESE TRANSITIONS INSURE THAT THE
  !     MODEL BEHAVIOUR DOES NOT (USUALLY) EXHIBIT EXTREME JUMPS DURING STAND
  !     DEVELOPMENT

  !     WT1 - WEIGHT ASSIGNED TO THE COVER TYPE (ONLY TOP 2)
  do I = 1,2
     WT1(I) = ICTWT(I)
  end do

  do I = 1,2
     do J = 1,UBCT
        if (ICT(I) .eq. J .and. WT1(I) .gt.0.0) then

           select case (J)

              ! SF,WH,SS,RC
           case (SFCT)

              X(1) = 3.0
              X(2) = 5.0
              K = 2
              WT2(K) = ALGSLP(QMD80,X,Y,2)
              WT2(K-1) = 1.0 - WT2(K)

              if (WT2(1) .gt.0.0) then
                 EQWT(5) = EQWT(5) + WT1(I) * WT2(1)
              end if

              if (WT2(2) .gt.0.0) then

                 do K = 1,3
                    WT3(K) = 0.0
                 end do

                 if (PERCOV .lt.60.0) then
                    X(1) = 40.0
                    X(2) = 60.0
                    K = 2
                 else if (PERCOV .ge.60.0 ) then
                    X(1) = 70.0
                    X(2) = 90.0
                    K = 3
                 end if
                 WT3(K) = ALGSLP(PERCOV,X,Y,2)
                 WT3(K-1) = 1.0 - WT3(K)

                 if (WT3(1) .gt.0.0) then
                    EQWT(5) = EQWT(5) + WT1(I) * WT2(2) * WT3(1)
                 end if

                 if (WT3(2) .gt.0.0) then
                    if (LSHRUB) then
                       EQWT(5) = EQWT(5) + WT1(I) * WT2(2) * WT3(2)
                    else
                       EQWT(8) = EQWT(8) + WT1(I) * WT2(2) * WT3(2)
                    end if
                 end if

                 if (WT3(3) .gt.0.0) then
                    EQWT(8) = EQWT(8) + WT1(I) * WT2(2) * WT3(3)
                 end if

              end if

              ! DF,GF,WP
           case (DFCT)

              X(1) = 3.0
              X(2) = 5.0
              K = 2
              WT2(K) = ALGSLP(QMD80,X,Y,2)
              WT2(K-1) = 1.0 - WT2(K)

              if (WT2(1) .gt.0.0) then
                 EQWT(5) = EQWT(5) + WT1(I) * WT2(1)
              end if

              if (WT2(2) .gt.0.0) then

                 do K = 1,3
                    WT3(K) = 0.0
                 end do

                 X(1) = 70.0
                 X(2) = 90.0
                 K = 2
                 WT3(K) = ALGSLP(PERCOV,X,Y,2)
                 WT3(K-1) = 1.0 - WT3(K)

                 if (WT3(1) .gt.0.0) then
                    if (LGRASS) then
                       EQWT(2) = EQWT(2) + WT1(I) * WT2(2) * WT3(1)
                    else if (LSHRUB) then
                       EQWT(5) = EQWT(5) + WT1(I) * WT2(2) * WT3(1)
                    else if (LFORB) then
                       EQWT(8) = EQWT(8) + WT1(I) * WT2(2) * WT3(1)
                    end if
                 end if

                 if (WT3(2) .gt.0.0) then
                    EQWT(8) = EQWT(8) + WT1(I) * WT2(2) * WT3(2)
                 end if

              end if

              ! MH,AF,WB
           case (MHCT)

              do K = 1,2
                 WT2(K) = 0.0
              end do

              X(1) = 70.0
              X(2) = 90.0
              K = 2
              WT2(K) = ALGSLP(PERCOV,X,Y,2)
              WT2(K-1) = 1.0 - WT2(K)

              if (WT2(1) .gt.0.0) then
                 if (LSHRUB) then
                    EQWT(5) = EQWT(5) + WT1(I) * WT2(1)
                 else
                    EQWT(8) = EQWT(8) + WT1(I) * WT2(1)
                 end if
              end if

              if (WT2(2) .gt.0.0) then
                 EQWT(8) = EQWT(8) + WT1(I) * WT2(2)
              end if

              ! RA
           case (RACT)

              do K = 1,2
                 WT2(K) = 0.0
              end do

              X(1) = 40.0
              X(2) = 60.0
              K = 2
              WT2(K) = ALGSLP(PERCOV,X,Y,2)
              WT2(K-1) = 1.0 - WT2(K)

              if (WT2(1) .gt.0.0) then
                 EQWT(5) = EQWT(5) + WT1(I) * WT2(1)
              end if

              if (WT2(2) .gt.0.0) then
                 EQWT(9) = EQWT(9) + WT1(I) * WT2(2)
              end if

              ! LP
           case (LPCT)

              if (LWET) then
                 do K = 1,2
                    WT2(K) = 0.0
                 end do

                 X(1) = 40.0
                 X(2) = 60.0
                 K = 2
                 WT2(K) = ALGSLP(PERCOV,X,Y,2)
                 WT2(K-1) = 1.0 - WT2(K)

                 if (WT2(1) .gt.0.0) then
                    EQWT(5) = EQWT(5) + WT1(I) * WT2(1)
                 end if

                 if (WT2(2) .gt.0.0) then
                    EQWT(8) = EQWT(8) + WT1(I) * WT2(2)
                 end if

              else
                 EQWT(5) = EQWT(5) + WT1(I)
              end if

              ! WO,TO,GC
           case (WOCT)

              do K = 1,2
                 WT2(K) = 0.0
              end do

              X(1) = 40.0
              X(2) = 60.0
              K = 2
              WT2(K) = ALGSLP(PERCOV,X,Y,2)
              WT2(K-1) = 1.0 - WT2(K)

              if (WT2(1) .gt.0.0) then
                 if (LSHRUB) then
                    EQWT(2) = EQWT(2) + WT1(I) * WT2(1)
                 else
                    EQWT(1) = EQWT(1) + WT1(I) * WT2(1)
                 end if
              end if

              if (WT2(2) .gt.0.0) then
                 if (LSHRUB) then
                    EQWT(5) = EQWT(5) + WT1(I) * WT2(2)
                 else if (LGRASS) then
                    EQWT(2) = EQWT(2) + WT1(I) * WT2(2)
                 else if (LFORB) then
                    EQWT(8) = EQWT(8) + WT1(I) * WT2(2)
                 end if
              end if

           end select

        end if ! NONZERO WTS
     end do ! LOOP OVER COVER TYPES
  end do ! LOOP OVER TWO MAJOR COVER TYPES

  !     END OF DETAILED LOW FUEL MODEL SELECTION

  !     DURING THE 5 YEARS AFTER AN ENTRY, AND ASSUMING THAT SMALL+LARGE
  !     ACTIVIVITY FUELS HAVE JUMPED BY 10%, THEN MODEL 11 IS A
  !     CANDIDATE MODEL, SHARING WITH 10. THE WEIGHT OF THE SHARED
  !     RELATIONSHIP DECLINES FROM PURE 11 INITIALLY TO PURE 10 AFTER
  !     THE PERIOD EXPIRES.

  AFWT = MAX(0.0, 1.0 - (IYR - HARVYR) / 5.0)
  if (SLCHNG .ge. SLCRIT .or. LATFUEL) then
     LATFUEL = .true.
     EQWT(11) = AFWT
     if (AFWT .le.0.0) LATFUEL = .false.
  end if
  if (.not. LATFUEL) AFWT = 0.0

  !     MODELS 10,12,13 ARE ALWAYS CANDIDATE MODELS FOR NATURAL FUELS
  !     OTHER MODELS ARE ALSO CANDIDATES, DEPENDING ON COVER TYPE, ETC

  EQWT(10) = 1.0 - AFWT
  EQWT(12) = 1.0
  EQWT(13) = 1.0

  !     CALL FMDYN TO RESOLVE WEIGHTS, SORT THE WEIGHTED FUEL MODELS
  !     FROM THE HIGHEST TO LOWEST, SET FMD (USING THE HIGHEST WEIGHT)

  call FMDYN(SMALL,LARGE,ITYP,XPTS,EQWT,IPTR,ICLSS,LDYNFM,FMD)

  if (DEBUG) write (JOSTND,8) FMD
  8 format (' FMCFMD, FMD=',I4)

  return
end
