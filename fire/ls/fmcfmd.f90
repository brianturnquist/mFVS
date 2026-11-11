!======================!
!   fire/ls : fmcfmd   !
!======================!
! FIRE-LS $Id$
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
  include 'PLOT.f90'

  !     LOCAL VARIABLE DECLARATIONS

  ! Variables
  integer :: ICLSS
  parameter(ICLSS = 22)

  !     COVER METAGROUP ENUMS

  !     JPCT  =  1  ! JACK PINE COVER TYPE
  !     NHCT  =  2  ! NORTHERN HARDWOOD
  !     RPCT  =  3  ! RED AND WHITE PINE
  !     MWCT  =  4  ! MIXED WOOD
  !     OACT  =  5  ! OAK
  !     POCT  =  6  ! PINE OAK
  !     ABCT  =  7  ! ASPEN BIRCH !! NOTE THAT SUBSEQUENT FOR-LOOPS USE THIS U-BOUND

  integer :: JPCT,NHCT,RPCT,MWCT,OACT,POCT,ABCT
  parameter(&
       &JPCT = 1,&
       &NHCT = 2,&
       &RPCT = 3,&
       &MWCT = 4,&
       &OACT = 5,&
       &POCT = 6,&
       &ABCT = 7)

  integer :: IYR,FMD,ICT
  integer :: IPTR(ICLSS),ITYP(ICLSS)
  integer :: J,I,K,L,FINDMOD
  real    :: CAREA,HEMCRA,CWIDTH,HEMCOV,CONCOV,OVERCOV,OVERCRA
  real    :: CONCRA,BIRBA,ASPBA,RPBA,WPBA,PINECRA,PINECOV
  real    :: FMAVH,HARDBA,BAMOST
  real    :: XPTS(ICLSS,2),EQWT(ICLSS),AFWT,BFWPCNT
  real    :: X,CTBA(ABCT),STNDBA,BFCOUNT,SMCOUNT,CONCOUNT
  real    :: MAPBASBA,OAKBA,ASBIBA,HEMBA,LT3
  logical :: DEBUG,LBFUNDER,LSMTREES,LCNUNDER,LBFWPUND,LDRY

  !  THESE ARE THE INTEGER TAGS ASSOCIATED WITH EACH FIRE MODEL
  !  CLASS. THEY ARE RETURNED WITH THE WEIGHT

  data IPTR / 1,2,3,4,5,6,7,8,9,10,11,12,13,&
       &105,142,143,146,161,162,164,186,189 /

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
       &15., 30.,& ! FMD  10
       &15., 30.,& ! FMD  11
       &30., 60.,& ! FMD  12
       &45.,100.,& ! FMD  13
       &5., 15.,& ! FMD 105
       &5., 15.,& ! FMD 142
       &5., 15.,& ! FMD 143
       &5., 15.,& ! FMD 146
       &5., 15.,& ! FMD 161
       &5., 15.,& ! FMD 162
       &5., 15.,& ! FMD 164
       &5., 15.,& ! FMD 186
       &5., 15./ ! FMD 189

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

  if (DEBUG) write(JOSTND,6) ICYC,IYR,HARVYR,LDYNFM,PERCOV,FMKOD,&
       &SMALL,LARGE
  6 format(' FMCFMD CYCLE= ',I2,' IYR=',I5,' HARVYR=',I5,&
       &' LDYNFM=',L2,' PERCOV=',F7.2,' FMKOD=',I4,&
       &' SMALL=',F7.2,' LARGE=',F7.2)

  !  LOW FUEL MODEL SELECTION

  FMAVH = ATAVH
  if (DEBUG) write(JOSTND,8) FMAVH
  8 format(' FMCFMD FMAVH= ',F4.0)

  !  DETERMINE IF THERE IS A BALSAM FIR UNDERSTORY (ATLEAST 500 BF PER
  !  ACRE 1 - 3").  DETERMINE IF THERE ARE MANY SMALL TREES (ATLEAST 500
  !  TPA 0 - 2").  DETERMINE IF THERE IS A BALSAM FIR / WHITE PINE
  !  UNDERSTORY (ATLEAST 500 BF OR WP 1 - 3").  DETERMINE IF THERE IS A
  !  CONIFER UNDERSTORY (ATLEAST 500 CONIFERS 1 - 3").

  LBFUNDER = .false.
  LSMTREES = .false.
  LCNUNDER = .false.
  LBFWPUND = .false.
  BFCOUNT = 0
  SMCOUNT = 0
  CONCOUNT = 0
  BFWPCNT = 0
  do I = 1,ITRN
     if ((ISP(I) .eq.8) .and. (DBH(I) .ge.1) .and.&
          &(DBH(I) .le.3)) BFCOUNT = BFCOUNT + FMPROB(I)
     if (DBH(I) .le.2) SMCOUNT = SMCOUNT + FMPROB(I)
     if ((ISP(I) .le.14) .and. (DBH(I) .ge.1) .and.&
          &(DBH(I) .le.3)) CONCOUNT = CONCOUNT + FMPROB(I)
     if ((ISP(I) .eq.8) .or. (ISP(I) .eq.5)) then
        if ((DBH(I) .ge.1) .and. (DBH(I) .le.3)) then
           BFWPCNT = BFWPCNT + FMPROB(I)
        end if
     end if
  end do
  if (BFCOUNT .ge.500) LBFUNDER = .true.
  if (SMCOUNT .ge.500) LSMTREES = .true.
  if (CONCOUNT .ge.500) LCNUNDER = .true.
  if (BFWPCNT .ge.500) LBFWPUND = .true.

  !  CHECK TO SEE IF THERE IS A DROUGHT

  LDRY = .false.
  if (IYR .ge. IDRYB .and. IYR .le. IDRYE) LDRY = .true.

  if (DEBUG) write (JOSTND,9) LDRY,LBFUNDER,LSMTREES
  9 format (' FMCFMD, LDRY=',L2,' LBFUNDER=',L2,' LSMTREES=',L2)

  !  FIND THE TONS/ACRE IN DEAD SURFACE FUEL 0 - 3"

  LT3 = 0.0
  do I = 1,2
     do K = 1,2
        do L = 1,4
           do J = 1,3
              LT3 = LT3 + CWD(I,J,K,L)
           end do
        end do
     end do
  end do

  !  FIND THE BASAL AREA IN EACH COVER TYPE

  do I = 1,ABCT ! ZERO OUT THE COVER SUPER-GROUPS
     CTBA(I) = 0.
  end do

  STNDBA = 0.0

  do I = 1,ITRN
     X = FMPROB(I) * DBH(I) * DBH(I) * 0.0054542
     select case (ISP(I))
     case (1) ! JACK PINE
        CTBA(JPCT) = CTBA(JPCT) + X
        STNDBA = STNDBA + X
     case (12,18,19,25,26,27,28) ! NORTHERN HARDWOOD
        CTBA(NHCT) = CTBA(NHCT) + X
        STNDBA = STNDBA + X
     case (3,4,5) ! RED AND WHITE PINE
        CTBA(RPCT) = CTBA(RPCT) + X
        STNDBA = STNDBA + X
     case (24,40,41,43) ! MIXED WOOD AND ASPEN BIRCH
        CTBA(MWCT) = CTBA(MWCT) + X
        CTBA(ABCT) = CTBA(ABCT) + X
        STNDBA = STNDBA + X
     case (6,7,8,9) ! MIXED WOOD HAS LARGE SPRUCE AND FIR
        ! ASPEN BIRCH HAS SMALL SPRUCE AND FIR
        if (DBH(I) .ge.5) then
           CTBA(MWCT) = CTBA(MWCT) + X
        else
           CTBA(ABCT) = CTBA(ABCT) + X
        end if
        STNDBA = STNDBA + X
     case (30:33,35,36) ! OAK
        CTBA(OACT) = CTBA(OACT) + X
        STNDBA = STNDBA + X
     case (34) ! RED OAK GOES IN BOTH OAK AND NORTHERN HARDWOODS
        CTBA(NHCT) = CTBA(NHCT) + X
        CTBA(OACT) = CTBA(OACT) + X
        STNDBA = STNDBA + X
     end select
  end do

  !  FIND COVER TYPE METAGROUP *ICT* - FIRST LOOK FOR GROUP WITH THE MOST
  !  BASAL AREA; IF OAK AND PINE COMBINED ARE HIGHER, USE OAK OR PINE
  !  COVER TYPES.  IF THERE ARE NO TREES USED THE METAGROUP OF
  !  THE PREVIOUS CYCLE, INITIALIZED TO *RPCT* IN **FMVINIT**

  ICT = 0
  if ((ITRN .gt.0) .and. (STNDBA.gt.0.001)) then
     BAMOST = 0
     do I = 1,ABCT
        if (CTBA(I) .gt. BAMOST) then
           BAMOST = CTBA(I)
           ICT = I
        end if
     end do
     CTBA(POCT) = CTBA(OACT) + CTBA(RPCT)
     if (CTBA(POCT) .gt. BAMOST) then
        ICT = OACT
        if (CTBA(RPCT) .gt. CTBA(OACT)) ICT = RPCT
     end if
  else
     ICT = OLDICT
  end if
  OLDICT = ICT

  select case (ICT)
  case (JPCT) !  jack pine
     if (PERCOV .le.70) then
        if (FMAVH .le.25) then
           EQWT(4) = 1.0
        else ! stand height gt 25
           if (LBFUNDER) then ! balsam fir understory
              if (BURNSEAS .le.2) then ! before greenup
                 EQWT(10) = 1.0
              else ! after greenup
                 EQWT(FINDMOD(162,IPTR,ICLSS)) = 1.0
              end if
           else if ((ITYPE .eq.6) .or. (ITYPE .eq.7)) then !grass understory, FDc12 or FDc23
              EQWT(2) = 1.0
           else ! must be deciduous shrubs
              if (BURNSEAS .le.2) then ! before greenup
                 EQWT(10) = 1.0
              else ! after greenup
                 EQWT(FINDMOD(161,IPTR,ICLSS)) = 1.0
              end if
           end if
        end if
     else ! percov gt 70%
        if (FMAVH .le.15) then
           EQWT(4) = 1.0
        else !stand height gt 15
           if (FWIND .le.4) then
              EQWT(8) = 1.0
           else
              EQWT(10) = 1.0
           end if
        end if
     end if

  case (NHCT) ! northern hardwoods
     STNDBA = 0.0
     MAPBASBA = 0.0
     OAKBA = 0.0
     ASBIBA = 0.0
     HEMBA = 0.0
     HEMCRA = 0.0

     do I = 1,ITRN
        X = FMPROB(I) * DBH(I) * DBH(I) * 0.0054542
        select case (ISP(I))
        case (18,19,25,26,27) ! maple and basswood
           MAPBASBA = MAPBASBA + X
           STNDBA = STNDBA + X
        case (30:36) ! oak
           OAKBA = OAKBA + X
           STNDBA = STNDBA + X
        case (24,40,41,43) ! aspen and birch
           ASBIBA = ASBIBA + X
           STNDBA = STNDBA + X
        case (12) ! hemlock
           HEMBA = HEMBA + X
           STNDBA = STNDBA + X
           CWIDTH = CRWDTH(I)
           CAREA = 3.1415927*CWIDTH*CWIDTH/4.0
           HEMCRA = HEMCRA + CAREA*FMPROB(I)
        case (28) ! beech
           STNDBA = STNDBA + X
        end select
     end do
     HEMCOV = 1.0 - EXP(-HEMCRA/43560.)
     HEMCOV = HEMCOV * 100.0
     if ((HEMCOV .ge.30) .or. (ASBIBA .gt.0)) then
        EQWT(8) = 1.0
     else if (STNDBA .gt.0) then
        if ((MAPBASBA/STNDBA) .ge.0.5) then
           EQWT(FINDMOD(186,IPTR,ICLSS)) = 1.0
        else
           if (BURNSEAS .eq.4) then ! FALL
              EQWT(9) = 1.0
           else
              EQWT(8) = 1.0
           end if
        end if
     else
        EQWT(8) = 1.0
     end if

  case (RPCT) ! red and white pine (two slight difference for white pine)
     STNDBA = 0.0
     RPBA = 0.0
     WPBA = 0.0
     PINECRA = 0.0
     HARDBA = 0.0

     do I = 1,ITRN
        X = FMPROB(I) * DBH(I) * DBH(I) * 0.0054542
        STNDBA = STNDBA + X
        if ((ISP(I) .ge.3) .and. (ISP(I) .le.5)) then
           CWIDTH = CRWDTH(I)
           CAREA = 3.1415927*CWIDTH*CWIDTH/4.0
           PINECRA = PINECRA + CAREA*FMPROB(I)
        end if
        select case (ISP(I))
        case (3,4) ! red pine
           RPBA = RPBA + X
        case (5) ! white pine
           WPBA = WPBA + X
        case (15:68) ! hardwoods
           HARDBA = HARDBA + X
        end select
     end do
     PINECOV = 1.0 - EXP(-PINECRA/43560.)
     PINECOV = PINECOV * 100.0

     if (LT3.ge.5) then
        EQWT(10) = 1.0
     else if (PERCOV .le.0) then
        EQWT(8) = 1.0
     else if (((PINECOV/PERCOV) .lt.0.5) .and. (HARDBA .gt.0)) then
        EQWT(8) = 1.0
     else if ((ITYPE .eq.4) .or. (ITYPE .eq.10)) then
        ! FDn33 or FDc34, hazel underbrush is present
        if (LDRY) EQWT(FINDMOD(146,IPTR,ICLSS)) = 1.0
        if (.not. LDRY) EQWT(FINDMOD(143,IPTR,ICLSS)) = 1.0
     else if ((LBFUNDER) .or. (LBFWPUND)) then
        if (FWIND .le.4) then
           EQWT(10) = 1.0
        else
           EQWT(FINDMOD(146,IPTR,ICLSS)) = 1.0
        end if
     else if (PERCOV .ge.50) then
        EQWT(9) = 1.0
     else if ((PERCOV .le.30) .and. (RPBA .gt. WPBA)) then
        EQWT(2) = 1.0 ! this grass type only occurs with young red pine
        ! plantations or red pine savannas
     else
        EQWT(9) = 1.0 ! not sure in this case
     end if

  case (MWCT) ! mixed wood
     STNDBA = 0.0
     BIRBA = 0.0
     CONCRA = 0.0
     OVERCRA = 0.0

     do I = 1,ITRN
        X = FMPROB(I) * DBH(I) * DBH(I) * 0.0054542
        if (DBH(I) .ge.5) then
           CWIDTH = CRWDTH(I)
           CAREA = 3.1415927*CWIDTH*CWIDTH/4.0
           OVERCRA = OVERCRA + CAREA*FMPROB(I)
        end if
        select case (ISP(I))
        case (24,43) ! birch
           BIRBA = BIRBA + X
           STNDBA = STNDBA + X
        case (1:14) ! conifers
           STNDBA = STNDBA + X
           if (DBH(I) .ge.5) then
              CWIDTH = CRWDTH(I)
              CAREA = 3.1415927*CWIDTH*CWIDTH/4.0
              CONCRA = CONCRA + CAREA*FMPROB(I)
           end if
        case (40:41) ! aspen
           STNDBA = STNDBA + X
        end select
     end do
     OVERCOV = 1.0 - EXP(-OVERCRA/43560.)
     OVERCOV = OVERCOV * 100.0
     CONCOV = 1.0 - EXP(-CONCRA/43560.)
     CONCOV = CONCOV * 100.0
     if (STNDBA .le.0) then
        EQWT(8) = 1.0
     else if ((BIRBA/STNDBA) .ge.0.50) then
        EQWT(9) = 1.0
     else if (OVERCOV .gt.0) then
        if ((CONCOV/OVERCOV) .ge.0.30) then
           EQWT(10) = 1.0
        else
           EQWT(8) = 1.0
        end if
     else
        EQWT(8) = 1.0
     end if

  case (OACT) ! oak
     STNDBA = 0.0
     OAKBA = 0.0
     OVERCRA = 0.0

     do I = 1,ITRN
        X = FMPROB(I) * DBH(I) * DBH(I) * 0.0054542
        STNDBA = STNDBA + X
        if (DBH(I) .ge.5) then
           CWIDTH = CRWDTH(I)
           CAREA = 3.1415927*CWIDTH*CWIDTH/4.0
           OVERCRA = OVERCRA + CAREA*FMPROB(I)
        end if
        if ((ISP(I) .eq.30) .or. (ISP(I) .eq.35)) then
           ! white oak and black oak
           OAKBA = OAKBA + X
        end if
     end do
     OVERCOV = 1.0 - EXP(-OVERCRA/43560.)
     OVERCOV = OVERCOV * 100.0

     if (OVERCOV .ge.45) then
        if (BURNSEAS .eq.1) then ! early spring
           if (MOIS(1,1) .lt.0.08) then
              EQWT(FINDMOD(186,IPTR,ICLSS)) = 1.0
           else
              EQWT(8) = 1.0
           end if
        else ! not early spring
           if (STNDBA .le.0) then
              EQWT(9) = 1.0
           else if ((OAKBA/STNDBA) .ge.0.30) then
              EQWT(FINDMOD(189,IPTR,ICLSS)) = 1.0
           else
              EQWT(9) = 1.0
           end if
        end if
     else if (OVERCOV .ge.15) then
        if (.not. LSMTREES) EQWT(2) = 1.0
        if (LSMTREES) EQWT(FINDMOD(142,IPTR,ICLSS)) = 1.0
     else
        if (.not. LSMTREES) EQWT(FINDMOD(105,IPTR,ICLSS)) = 1.0
        if (LSMTREES) EQWT(FINDMOD(142,IPTR,ICLSS)) = 1.0
     end if

  case (ABCT) ! aspen birch
     ASPBA = 0.0
     BIRBA = 0.0

     do I = 1,ITRN
        X = FMPROB(I) * DBH(I) * DBH(I) * 0.0054542
        if ((ISP(I) .eq.40) .or. (ISP(I) .eq.41)) ASPBA = ASPBA + X
        if ((ISP(I) .eq.24) .or. (ISP(I) .eq.43)) BIRBA = BIRBA + X
     end do

     if (LT3.ge.5) then
        EQWT(10) = 1.0
     else
        if (.not. LCNUNDER) then
           if (BIRBA .gt. ASPBA) then
              EQWT(9) = 1.0
           else
              EQWT(8) = 1.0
           end if
        else
           if (FWIND .gt.4) then
              EQWT(FINDMOD(164,IPTR,ICLSS)) = 1.0
           else
              if (BIRBA .gt. ASPBA) then
                 EQWT(9) = 1.0
              else
                 EQWT(8) = 1.0
              end if
           end if
        end if
     end if

  end select

  !  END OF DETAILED LOW FUEL MODEL SELECTION

  select case (ICT)
  case (JPCT, MWCT, OACT)

     !        DURING THE 5 YEARS AFTER AN ENTRY, AND ASSUMING THAT SMALL+LARGE
     !        ACTIVITY FUELS HAVE JUMPED BY 10%, THEN MODEL 11 IS A
     !        CANDIDATE MODEL, SHARING WITH 10. THE WEIGHT OF THE SHARED
     !        RELATIONSHIP DECLINES FROM PURE 11 INITIALLY, TO PURE 10 AFTER
     !        THE PERIOD EXPIRES.

     AFWT = MAX(0.0, 1.0 - (IYR - HARVYR) / 5.0)
     if (SLCHNG .ge. SLCRIT .or. LATFUEL) then
        LATFUEL = .true.
        EQWT(11) = AFWT
        if (AFWT .le.0.0) LATFUEL = .false.
     end if
     if (.not. LATFUEL) AFWT = 0.0

     !        MODELS 10,12, AND 13 ARE CANDIDATE MODELS FOR NATURAL FUELS

     EQWT(10) = 1.0 - AFWT
     EQWT(12) = 1.0
     EQWT(13) = 1.0
     ! oak never gets 12 or 13
     if (ICT .eq. OACT) then
        EQWT(12) = 0.0
        EQWT(13) = 0.0
     end if

  case (NHCT) ! northern hardwoods never get fm 10, 12, or 13, and
     ! only gets fm 11 if there is activity fuel
     AFWT = MAX(0.0, 1.0 - (IYR - HARVYR) / 5.0)
     if (SLCHNG .ge. SLCRIT .or. LATFUEL) then
        LATFUEL = .true.
        if (AFWT .le.0.0) LATFUEL = .false.
     end if

     if (LATFUEL) EQWT(11) = 1.0
     if (.not. LATFUEL) EQWT(11) = 0.0

     EQWT(10) = 0.0
     EQWT(12) = 0.0
     EQWT(13) = 0.0

  case (RPCT) ! red and white pine
     ! with natural fuels, fm 10,12,13 possible
     ! with activity fuels, fm 12 used if red slash and fm 11 used if older slash
     AFWT = MAX(0.0, 1.0 - (IYR - HARVYR) / 5.0)
     if (SLCHNG .ge. SLCRIT .or. LATFUEL) then
        LATFUEL = .true.
        if ((IYR - HARVYR) .le.2) then
           EQWT(12) = 1.0
           EQWT(11) = 0.0
           EQWT(10) = 0.0
           EQWT(13) = 0.0
        else if (((IYR - HARVYR) .gt.2)&
             &.and. ((IYR - HARVYR) .le.5)) then
           EQWT(11) = 1.0
           EQWT(12) = 0.0
           EQWT(10) = 0.0
           EQWT(13) = 0.0
        else
           EQWT(10) = 1.0
           EQWT(11) = 0.0
           EQWT(12) = 1.0
           EQWT(13) = 1.0
        end if
        if (AFWT .le.0.0) LATFUEL = .false.
     else
        EQWT(10) = 1.0
        EQWT(11) = 0.0
        EQWT(12) = 1.0
        EQWT(13) = 1.0
     end if

  case (ABCT) ! aspen birch never gets fm 11, 12, or 13
     EQWT(10) = 1.0
     EQWT(11) = 0.0
     EQWT(12) = 0.0
     EQWT(13) = 0.0
  end select

  !  CALL FMDYN TO RESOLVE WEIGHTS, SORT THE WEIGHTED FUEL MODELS
  !  FROM THE HIGHEST TO LOWEST, SET FMD (USING THE HIGHEST WEIGHT)

  call FMDYN(SMALL,LARGE,ITYP,XPTS,EQWT,IPTR,ICLSS,LDYNFM,FMD)

  if (DEBUG) write (JOSTND,10) FMD,LDYNFM,ICT,ITYPE,FWIND
  10 format (' FMCFMD, FMD=',I4,' LDYNFM=',L2,' ICT=',I4,' ITYPE='&
       &,I4,' FWIND=',F4.1)

  return
end

  !     "LOCAL SUBROUTINE"
  !     FIND FUEL MODEL NUMBER (FOR LABELLING PURPOSES USUALLY,
  !     GIVEN THE INDEX POSITION IN THE IPTR VECTOR. THIS NEED
  !     ARISES WHEN FUEL MODEL LABELS DO NOT MATCH THEIR INDEX
  !     E.G. MODELS 25 & 26, WHICH ARE IN POSITIONS 14 & 15 IN
  !     THIS VARIANT. 8 IS RETURNED IF THE MODEL LABEL IS NOT FOUND,

  ! Variables
integer function FINDMOD(JMOD,IPTR,ICLSS)

  integer :: JMOD,K,IPTR
  dimension IPTR(ICLSS)

  do K = 1,ICLSS
     if (JMOD .eq. IPTR(K)) then
        goto 11
     end if
  end do
  K = 8
  11 continue
  FINDMOD = K

  return
end
