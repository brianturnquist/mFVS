!=======================!
!   fire/vut : fmcfmd   !
!=======================!
! FIRE-VUT $Id$
subroutine FMCFMD (IYR, FMD)
  implicit none

  !  SINGLE-STAND VERSION
  !  CALLED FROM: FMBURN
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

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'FMFCOM.f90'
  include 'FMCOM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'ARRAYS.f90'
  include 'SSTGMC.f90'

  !  LOCAL VARIABLE DECLARATIONS

  !  NUMBER OF POTENTIAL FIRE MODELS

  ! Variables
  integer :: ICLSS
  parameter(ICLSS = 12)

  !  COVER METAGROUP ENUMS:

  !     OBCT  =  1  ! OAK BRUSH COVER TYPE
  !     PJCT  =  2  ! PINYON JUNIPER
  !     PPCT  =  3  ! PONDEROSA PINE
  !     WSCT  =  4  ! WHITE SPRUCE
  !     SFCT  =  5  ! SPRUCE FIR
  !     LPCT  =  6  ! LODGEPOLE PINE
  !     MCCT  =  7  ! MIXED CONIFER
  !     ASCT  =  8  ! ASPEN !! NOTE THAT SUBSEQUENT FOR-LOOPS USE THIS U-BOUND

  integer :: OBCT,PJCT,PPCT,WSCT,SFCT,LPCT,MCCT,ASCT
  parameter(&
       &OBCT = 1,&
       &PJCT = 2,&
       &PPCT = 3,&
       &WSCT = 4,&
       &SFCT = 5,&
       &LPCT = 6,&
       &MCCT = 7,&
       &ASCT = 8)

  !  LOCAL VARIABLE DECLARATIONS:

  integer :: FMD,ICT,USCT,SIFM,IS,II
  integer :: IYR,I,J,J2,I1,I2
  integer :: IPTR(ICLSS),ITYP(ICLSS)
  integer :: HPOINT(MAXTRE),IDUM(1),IFMST
  real    :: CTBA(ASCT),USBA(ASCT),STNDBA,XW,CVR10,AFWT
  real    :: SSUMN,P,FMAVH,XWID(MAXTRE),SIBRK
  real    :: XPTS(ICLSS,2)
  real    :: X,Y,X3,Y1,Y2,Y3,Y4,Y5,Y6
  real    :: BL,BD,SNVIH,SNVIS,VT,XVEC(2),YVEC(2),ALGSLP
  real    :: PSUM,USHT,TINY
  real    :: EQWT(ICLSS),XNEG1
  logical :: DEBUG,LDRY,LUNDER,LCUNDR
  logical :: LWFOK,LPPDOM,LMERCH

  !  DUMMY ENTRY FOR **COVOLP**

  data     IDUM /0/

  !  These are the integer tags associated with each fire model
  !  class. They are returned with the weight

  data IPTR / 1,2,3,4,5,6,7,8,9,10,11,12 /

  !  These are 0 for regular lines, -1 for horizontal and 1 for
  !  vertical lines. If any of the lines defined by xpts() are of
  !  an unusual variety, this must be entered here so that
  !  special logic can be invoked.  In this case, all the line
  !  segments have a |slope| that is > 0 and less than inif.

  data ITYP / ICLSS * 0 /

  !  XPTS: FIRST COLUMN ARE THE SMALL FUEL VALUES FOR EACH FIRE MODEL
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
       &30., 60./ ! FMD  12

  !  INITIALLY SET ALL MODELS OFF; NO TWO CANDIDATE MODELS ARE COLINEAR, AND COLINEARITY
  !  WEIGHTS ARE ZERO. IF TWO CANDIDATE MODELS ARE COLINEAR, THE WEIGHTS MUST BE SET, AND
  !  MUST SUM TO 1, WRT EACH OTHER

  do I = 1,ICLSS
     EQWT(I) = 0.0
  end do

  !  ** END OF GENERAL FUEL MODEL SETUP **

  !  BEGIN ROUTINE

  call DBCHK (DEBUG,'FMCFMD',6,ICYC)
  if (DEBUG) write(JOSTND,1) ICYC,IYR,LUSRFM
  1 format(' FMCFMD CYCLE= ',I2,' IYR=',I5,' LUSRFM=',L5)

  !  IF THE USER SPECIFIED THE FM DEFINITIONS, THEN WE ARE DONE.

  if (LUSRFM) return

  if (DEBUG) write(JOSTND,7) ICYC,IYR,HARVYR,LDYNFM,PERCOV,FMKOD,&
       &SMALL,LARGE
  7 format(' FMCFMD CYCLE= ',I2,' IYR=',I5,' HARVYR=',I5,&
       &' LDYNFM=',L2,' PERCOV=',F7.2,' FMKOD=',I4,&
       &' SMALL=',F7.2,' LARGE=',F7.2)

  LDRY = .false.
  if (IYR .ge. IDRYB .and. IYR .le. IDRYE) LDRY = .true.

  !  FIND DESCENDING SORT ORDER WRT HEIGHT, THEN (COPIED
  !  FROM **DENSE**) STEP FROM THE LARGEST DOWNWARD TO GET
  !  THE 40/ACRE TALLEST THEN AVERAGE TO GET FM- TOPHEIGHT

  if (ITRN .gt.0) call RDPSRT(ITRN,HT,HPOINT,.true.)
  SSUMN = 0.0
  FMAVH = 0.0
  do I = 1,ITRN
     II = HPOINT(I)
     P = FMPROB(II)
     if ((SSUMN + P) .gt.40.0) P = 40. - SSUMN
     SSUMN = SSUMN + P
     FMAVH = FMAVH + (HT(II) * P)
     if (SSUMN .ge.40.0) go to 65
  end do
  65 continue
  if (SSUMN .gt.0.0) FMAVH = FMAVH / SSUMN

  !  UNDERSTORY HEIGHT IS HALF THE CURRENT TOP HEIGHT

  USHT = 0.5 * FMAVH

  !  STEPS:

  !  1 - FIND BASAL AREA IN EACH 'COVER TYPE' - BECAUSE OF SPECIES
  !      POOLING, THIS CAN BE DIFFERENT FROM HIGHEST BA SPECIES
  !      AT THE SAME TIME FIND THE >UNDERSTORY< BA FOR THE SAME
  !      GROUPINGS; REQUIRED FOR A NUMBER OF COVER TYPE FUEL MODELS.

  do I = 1,ASCT ! ZERO OUT THE COVER SUPER-GROUPS
     CTBA(I) = 0.
     USBA(I) = 0.
  end do

  select case (VARACD)

     !  TETONS VARIANT

  case ('TT')
     do I = 1,ITRN
        X = FMPROB(I) * DBH(I) * DBH(I) * 0.0054542
        LUNDER = HT(I) .le. USHT

        select case (ISP(I))

        case (4,11,12) ! PINYON JUNIPER
           CTBA(PJCT) = CTBA(PJCT) + X
           if (LUNDER) USBA(PJCT) = USBA(PJCT) + X

        case (10) ! PONDEROSA PINE
           CTBA(PPCT) = CTBA(PPCT) + X
           if (LUNDER) USBA(PPCT) = USBA(PPCT) + X

        case (5,8,9) ! SPRUCE FIR
           CTBA(SFCT) = CTBA(SFCT) + X
           if (LUNDER) USBA(SFCT) = USBA(SFCT) + X

        case (7) ! LODGEPOLE PINE
           CTBA(LPCT) = CTBA(LPCT) + X
           if (LUNDER) USBA(LPCT) = USBA(LPCT) + X

        case (1,2,3,17) ! MIXED CONIFER
           CTBA(MCCT) = CTBA(MCCT) + X
           if (LUNDER) USBA(MCCT) = USBA(MCCT) + X

        case (6,13:16,18) ! ASPEN, COTTONWOOD, OTHER HARDWOODS
           CTBA(ASCT) = CTBA(ASCT) + X
           if (LUNDER) USBA(ASCT) = USBA(ASCT) + X

        end select
     end do

     !  UTAH VARIANT

  case ('UT')
     do I = 1,ITRN
        X = FMPROB(I) * DBH(I) * DBH(I) * 0.0054542
        LUNDER = HT(I) .le. USHT

        select case (ISP(I))

        case (13) ! OAK BRUSH
           CTBA(OBCT) = CTBA(OBCT) + X
           if (LUNDER) USBA(OBCT) = USBA(OBCT) + X

        case (11,12,14:16) ! PINYON JUNIPER
           CTBA(PJCT) = CTBA(PJCT) + X
           if (LUNDER) USBA(PJCT) = USBA(PJCT) + X

        case (10) ! PONDEROSA PINE
           CTBA(PPCT) = CTBA(PPCT) + X
           if (LUNDER) USBA(PPCT) = USBA(PPCT) + X

        case (5,8,9) ! SPRUCE FIR
           CTBA(SFCT) = CTBA(SFCT) + X
           if (LUNDER) USBA(SFCT) = USBA(SFCT) + X

        case (7) ! LODGEPOLE PINE
           CTBA(LPCT) = CTBA(LPCT) + X
           if (LUNDER) USBA(LPCT) = USBA(LPCT) + X

        case (1:4,17,23) ! MIXED CONIFER
           CTBA(MCCT) = CTBA(MCCT) + X
           if (LUNDER) USBA(MCCT) = USBA(MCCT) + X

        case (6,18:22,24) ! ASPEN, COTTONWOOD, OTHER HARDWOODS
           CTBA(ASCT) = CTBA(ASCT) + X
           if (LUNDER) USBA(ASCT) = USBA(ASCT) + X

        end select
     end do

     !  CENTRAL ROCKIES (CR) VARIANT

  case ('CR')
     do I = 1,ITRN
        X = FMPROB(I) * DBH(I) * DBH(I) * 0.0054542
        LUNDER = HT(I) .le. USHT

        select case (ISP(I))

        case (23:27) ! OAK BRUSH
           CTBA(OBCT) = CTBA(OBCT) + X
           if (LUNDER) USBA(OBCT) = USBA(OBCT) + X

        case (12,16,29:35) ! PINYON JUNIPER
           CTBA(PJCT) = CTBA(PJCT) + X
           if (LUNDER) USBA(PJCT) = USBA(PJCT) + X

        case (13,36) ! PONDEROSA PINE, CHIHUAHUA PINE
           CTBA(PPCT) = CTBA(PPCT) + X
           if (LUNDER) USBA(PPCT) = USBA(PPCT) + X

        case (19) ! WHITE SPRUCE
           CTBA(WSCT) = CTBA(WSCT) + X
           if (LUNDER) USBA(WSCT) = USBA(WSCT) + X

        case (1,17,18) ! SPRUCE FIR
           CTBA(SFCT) = CTBA(SFCT) + X
           if (LUNDER) USBA(SFCT) = USBA(SFCT) + X

        case (11) ! LODGEPOLE PINE
           CTBA(LPCT) = CTBA(LPCT) + X
           if (LUNDER) USBA(LPCT) = USBA(LPCT) + X

        case (2:10,14,15,37) ! MIXED CONIFER
           CTBA(MCCT) = CTBA(MCCT) + X
           if (LUNDER) USBA(MCCT) = USBA(MCCT) + X

        case (20:22,28,38) ! ASPEN, COTTONWOOD, PAPER BIRCH,
           CTBA(ASCT) = CTBA(ASCT) + X
           if (LUNDER) USBA(ASCT) = USBA(ASCT) + X

        end select
     end do

  case default

  end select

  !  2 - FIND COVER TYPE METAGROUP *ICT* - FIRST LOOK FOR >50% OF
  !      BASAL AREA IN METAGROUP; FAILING THAT ASSIGN AS MIXED
  !      CONIFER *MCCT* - IF THERE ARE NO TREES USED THE METAGROUP OF
  !      THE PREVIOUS CYCLE, INITIALIZED TO *LPCT* IN **FMVINIT**

  STNDBA = 0.0
  do I = 1,ASCT
     STNDBA = STNDBA + CTBA(I)
  end do
  ICT = 0
  if ((ITRN .gt.0) .and. (STNDBA.gt.0.001)) then
     do I = 1,ASCT
        if ((CTBA(I)/STNDBA) .gt.0.50) then
           ICT = I
           goto 10
        end if
     end do
     ICT = MCCT
     goto 10
  else
     ICT = OLDICT
  end if
  10 OLDICT = ICT

  !  3 - USING THE UNDERTSTORY COVER TYPE *USBA* SELECT THE
  !      DOMINANT UNDERSTORY SPECIES GROUP *USCT*

  X = 0.0
  do I = 1,ASCT ! SUM FOR UNDERSTORY BA
     X = X + USBA(I)
  end do
  USCT = 0
  Y = -1. ! FIND MAX GROUP
  if (X .gt.0.0) then
     do I = 1,ASCT
        if (USBA(I) .gt. Y) then
           USCT = I
           Y = USBA(I)
        end if
     end do
  else
     USCT = ICT
  end if

  !  SET FLAG FOR CONIFEROUS UNDERSTOREY PRESENT

  LCUNDR = ((USBA(PPCT)+USBA(WSCT)+USBA(SFCT)+&
       &USBA(LPCT)+USBA(MCCT)) .gt.1.)

  if (DEBUG) write (JOSTND,11) ICT,LCUNDR,CTBA,USBA
  11 format (' IN FMCFMD, ICT,LCUNDR=',I3,L2,' CTBA=',8F6.2/&
       &' USBA=',8F6.2)
  TINY = 0.0

  !  4 - DETERMINE THE STRUCTURE CLASS.  DIFFERENT PARAMETERS ARE USED
  !      FOR THE CR VARIANT (VALIDATION WORKSHOP OCT. 04).

  !  UTAH AND TETONS VARIANTS

  if ((VARACD .eq. 'UT') .or. (VARACD .eq. 'TT')) then
     call FMSSTAGE(TPAMIN,CCMIN,PCTSMX,SAWDBH,SSDBH,GAPPCT,IFMST,X3,&
          &FMPROB,FMICR)

     !  CENTRAL ROCKIES VARIANT

  else
     y1 = 200
     y2 = 5
     y3 = 30
     if (ICT .eq.6) then ! lodgepole
        y4 = 12
     else
        y4 = 18
     end if
     y5 = 5
     y6 = 20
     call FMSSTAGE(y1,y2,y3,y4,y5,y6,IFMST,X3,FMPROB,FMICR)
  end if

  if (DEBUG) write (JOSTND,*) 'IFMST=',IFMST

  !  5 - USING THE COVER TYPE METAGROUP *ICT* SELECT THE FUEL MODEL
  !      SOME COVER TYPE MODELS CAN SOMETIMES LOOP BACK TO THIS POINT TO
  !      USE ALTERNATIVE COVER-TYPE RULES

  111 continue

  select case (ICT)

     !  OAK BRUSH COVER TYPE

  case(OBCT)
     if (FWIND .le.7.0) then
        EQWT(8) = 1.0
     else
        IS = MAXSP
        if (VARACD .eq. 'UT') IS = 13
        if ((VARACD.ne.'UT') .and. (VARACD.ne.'TT')) IS = 23
        if (CTBA(OBCT) .eq.0) then
           EQWT(5) = 1.0
        else

           !  AVERAGE HT OF OAK *X*

           X = 0.0
           PSUM = 0.0

           !  UTAH VARIANT

           if (VARACD .eq. 'UT') then
              IS = 13
              if (ISCT(IS,1) .gt.0) then
                 do J = ISCT(IS,1), ISCT(IS,2)
                    I = IND1(J)
                    X = X + HT(I) * FMPROB(I)
                    PSUM = PSUM + FMPROB(I)
                 end do
              end if

              !  CENTRAL ROCKIES VARIANT

           else if (VARACD .eq. 'CR') then
              do J = 1,MAXSP
                 I1 = ISCT(J,1)
                 if (I1.eq.0) go to 59
                 I2 = ISCT(J,2)
                 if (J.ge.23.and. J.le.27) then
                    do J2 = I1,I2
                       I = IND1(J)
                       X = X + HT(I) * FMPROB(I)
                       PSUM = PSUM + FMPROB(I)
                    end do
                 end if
                 59 continue
              end do

              !  TETONS VARIANT -- DOESN'T HAVE OAK AS A SPECIES

           else
              go to 60
           end if

           60 continue
           if (PSUM .gt.1.0E-6) X = X / PSUM

           !  DEAD BIOMASS: *Y*

           !  LIVE BIOMASS INCLUDES CROWNS & BOLES
           !  DEAD BIOMASS INCLUDES SNAGS & CWD

           BL = 0.0
           do I = 1,ITRN
              BL = BL + (CROWNW(I,0) * FMPROB(I) * P2T)
              do J = 1,5
                 BL = BL + (CROWNW(I,J) + OLDCRW(I,J)) *&
                      &P2T * FMPROB(I)
              end do
              XNEG1 = -1.0
              LMERCH = .false.
              call FMSVL2(ISP(I),DBH(I),HT(I),XNEG1,VT,ICR(I),&
                   &'L',.false.,.false.,JOSTND)
              BL = BL + (FMPROB(I) * V2T(ISP(I)) * VT)
           end do
           BD = 0.0
           do I = 1,NSNAG
              SNVIS = 0.0
              SNVIH = 0.0
              if (DENIH(I) .gt.0.0) then
                 call FMSVOL (I, HTIH(I), SNVIH, DEBUG, JOSTND)
                 SNVIH = SNVIH * DENIH(I)
              end if
              if (DENIS(I) .gt.0.0) then
                 call FMSVOL (I, HTIS(I), SNVIS, DEBUG, JOSTND)
                 SNVIS = SNVIS * DENIS(I)
              end if
              BD = BD + (SNVIS + SNVIH) * V2T(SPS(I))
           end do
           BD = BD + LARGE + SMALL
           Y = 0.
           if ((BD+BL) .gt.1.0E-6) Y = 100. * (BD/(BD+BL))

           !  NOW THAT AVG HT AND % DEAD BIOMASS ARE KNOWN,
           !  CHOOSE THE OAK COVER FIRE MODEL. IN THE 'ELSE'
           !  CASE, MODEL 4/5 BLENDING IS DONE AND FMD=5

           if (X .le.2.0.or. Y .le.50.0) then
              EQWT(5) = 1.0
           else if (X .gt.6.0.and. Y .gt.50.0) then
              EQWT(4) = 1.0
           else
              XVEC(1) = 2.0
              XVEC(2) = 6.0
              YVEC(1) = 0.0
              YVEC(2) = 1.0
              EQWT(4) = ALGSLP(X,XVEC,YVEC,2)
              EQWT(5) = 1.0 - EQWT(4)
           end if
        end if
     end if

     !  PINYON JUNIPER COVER TYPE
     !  NOTE *UNUSUAL* 'NO FIRE' OUTCOME IN UT DOCUMENT
     !  AND ADDITION OF TINY WHEN CALLED FROM OTHER TYPES
     !  FOR CR VARIANT, THIS WAS CHANGED AFTER VALIDATION MEETING (OCT 04)

  case(PJCT)

     !  CENTRAL ROCKIES VARIANT

     if ((VARACD.ne.'TT') .and. (VARACD.ne.'UT')) then
        if (PERCOV .le.25) then
           EQWT(2) = 1.0
        else if (PERCOV .le.35) then
           EQWT(2) = 1 - (PERCOV - 25)/10
           EQWT(5) = 1 - (35 - PERCOV)/10
        else if (PERCOV .le.45) then
           EQWT(5) = 1.0
        else if (PERCOV .le.55) then
           EQWT(5) = 1 - (PERCOV - 45)/10
           EQWT(6) = 1 - (55 - PERCOV)/10
        else
           EQWT(6) = 1.0
        end if

        !  UTAH AND TETONS VARIANTS

     else
        if ((SMALL+LARGE+TINY) .gt.0.0.and.&
             &FWIND .gt.7.0.and. PERCOV .gt.20.0) then
           if (LDRY) then
              EQWT(6) = 1.0
           else
              EQWT(5) = 1.0
           end if
        else
           EQWT(8) = 1.0
        end if
     end if

     !  PONDEROSA PINE COVER TYPE

  case(PPCT)

     !  DEFINE UNDERSTORY CHARACTERISTICS
     !  DEFINE UNDERSTORY AS TREES/SNAGS WITH HEIGHT
     !  < HALF OF THE STAND TOP HEIGHT.

     !  LIVE UNDERSTORY BIOMASS: CROWNS & BOLES

     BL = 0.0
     do I = 1,ITRN
        if (HT(I) .le. USHT) then
           BL = BL + (CROWNW(I,0) * FMPROB(I) * P2T)
           do J = 1,5
              BL = BL + (CROWNW(I,J) + OLDCRW(I,J)) *&
                   &P2T * FMPROB(I)
           end do
           XNEG1 = -1.0
           LMERCH = .false.
           call FMSVL2(ISP(I),DBH(I),HT(I),XNEG1,VT,ICR(I),&
                &'L',LMERCH,DEBUG,JOSTND)
           BL = BL + (FMPROB(I) * V2T(ISP(I)) * VT)
        end if
     end do

     !  DEAD UNDERSTORY BIOMASS: SNAGS ONLY

     BD = 0.0
     do I = 1,NSNAG
        SNVIS = 0.0
        SNVIH = 0.0
        if (DENIH(I) .gt.0.0.and. HTIH(I) .le. USHT) then
           call FMSVOL (I, HTIH(I), SNVIH, DEBUG, JOSTND)
           SNVIH = SNVIH * DENIH(I)
        end if
        if (DENIS(I) .gt.0.0.and. HTIS(I) .le. USHT) then
           call FMSVOL (I, HTIS(I), SNVIS, DEBUG, JOSTND)
           SNVIS = SNVIS * DENIS(I)
        end if
        BD = BD + (SNVIS + SNVIH) * V2T(SPS(I))
     end do

     !  Y IS PERCENT OF TOTAL BIOMASS THAT IS DEAD

     Y = 0.0
     if ((BD+BL) .gt.1.0E-6)  Y = 100. * (BD/(BD+BL))

     !  NOW CHOOSE FUEL MODEL

     if (PERCOV .le.60.0) then

        !  LIVE+DEAD UNDERSTORY PRESENT

        if ((BD+BL) .gt.0.0) then
           if (FWIND .gt.7.0) then
              if (Y .le.50.0) then

                 !  IF OA OR PJ, LOOP BACK AND USE OA OR PJ
                 !  (SURFACE FUELS) RULES

                 if (USCT .eq. OBCT .or. USCT .eq. PJCT) then
                    ICT = USCT
                    if (SMALL .le.0.0) TINY = 1.0E-3
                    goto 111
                 else
                    EQWT(5) = 1.0
                 end if
              else
                 EQWT(6) = 1.0
              end if
           else
              if ((VARACD .eq. 'UT') .or.&
                   &(VARACD .eq. 'TT')) then
                 EQWT(8) = 1.0
              else
                 EQWT(5) = 1.0
              end if
           end if
        else
           EQWT(2) = 1.0
        end if

     else

        !  PERCOV > 60

        if (FWIND .gt.7.0) then
           if ((CTBA(PJCT)/MAX(1.0E-3,STNDBA)) .gt.0.2) then
              if (LDRY) then
                 EQWT(6) = 1.0
              else
                 EQWT(5) = 1.0
              end if
           else if (IFMST .eq.6) then
              if (LCUNDR) then
                 EQWT(5) = 1.0
              else
                 EQWT(2) = 1.0
              end if
           else
              EQWT(9) = 1.0
           end if
        else
           EQWT(9) = 1.0
        end if
     end if

     !  WHITE SPRUCE COVER TYPE
     !  ONLY OCCURS IN THE CENTRAL ROCKIES VARIANT

  case(WSCT)
     if (PERCOV .le.40.0) then
        EQWT(2) = 1.0
     else

        !  FIND AVERAGE DIAMETER

        X = 0.0
        PSUM = 0.0
        do I = 1,ITRN
           X = X + (FMPROB(I) * DBH(I))
           PSUM = PSUM + FMPROB(I)
        end do
        if (PSUM .gt.1.0E-6) X = X / PSUM
        if (IFMST .ge.4.and. X .gt.12.0) then ! ge 3
           EQWT(10) = 1.0
        else
           EQWT(8) = 1.0
        end if
     end if

     !  SPRUCE FIR COVER TYPE
     !  THIS WAS CHANGED FOR CR VARIANT AFTER VALIDATION WORKSHOP (OCT 04)

  case(SFCT)

     !  CENTRAL ROCKIES VARIANT

     if ((VARACD.ne.'TT') .and. (VARACD.ne.'UT')) then

        select case (IFMST)

        case(0)
           EQWT(2) = 1.0

        case(1)
           ! CALCULATE QMD
           X = 0.0
           PSUM = 0.0
           do I = 1,ITRN
              X = X + (FMPROB(I) * DBH(I)*DBH(I))
              PSUM = PSUM + FMPROB(I)
           end do
           if (PSUM .gt.1.0E-6) X = SQRT(X / PSUM)
           if (X .gt.1) then
              EQWT(5) = 1.0
           else
              EQWT(2) = 1.0
           end if

        case(2)
           EQWT(8) = 1.0

        case(3:6)
           EQWT(10) = 1.0

        end select

        !  UTAH AND TETONS VARIANTS

     else
        if (IFMST .le.2) then
           EQWT(2) = 1.0
        else
           EQWT(8) = 1.0
        end if
     end if

     !  LODGEPOLE PINE COVER TYPE
     !  THIS WAS CHANGED FOR CR VARIANT AFTER VALIDATION WORKSHOP (OCT 04)

  case(LPCT)

     !  CENTRAL ROCKIES VARIANT

     if ((VARACD .ne. 'TT') .and. (VARACD .ne. 'UT')) then

        select case (IFMST)

        case(0,1,5)
           EQWT(5) = 1.0

        case(2)
           EQWT(8) = 1.0

        case(3,4,6)
           EQWT(10) = 1.0

        end select

        !  UTAH AND TETONS VARIANTS

     else

        !  COMPUTE STAND TPA

        X = 0.0
        do I = 1,ITRN
           X = X + FMPROB(I)
        end do

        !  LP SITE INDEX

        SIFM = INT(SITEAR(7))
        SIBRK = 20

        if (SIFM .lt. SIBRK) then
           EQWT(5) = 1.0
        else if (X .gt.1000.0) then
           if (FMAVH .le.10.0) then
              EQWT(5) = 1.0
           else
              if (FWIND .gt.7.0) then
                 EQWT(5) = 1.0
              else
                 EQWT(8) = 1.0
              end if
           end if
        else if (IFMST .le.2) then
           EQWT(2) = 1.0
        else
           EQWT(8) = 1.0
        end if
     end if

     !  MIXED CONIFER COVER TYPE
     !  THIS WAS CHANGED FOR CR VARIANT AFTER VALIDATION WORKSHOP (OCT 04)

  case(MCCT)

     !  DETERMINE IF PP IS HIGHEST RANK SPECIES WRT BA.

     if (VARACD .eq. 'TT') then
        LPPDOM = .true.
        J = 10
        do I = 1, MAXSP
           if ((I .ne. J) .and. (FMTBA(I) .gt. FMTBA(J))) then
              LPPDOM = .false.
           end if
        end do
     else
        LPPDOM = .true.
        if (VARACD .eq. 'UT') then
           J = 10
        else
           J = 13
        end if
        do I = 1, MAXSP
           if ((I .ne. J) .and. (FMTBA(I) .gt. FMTBA(J))) then
              LPPDOM = .false.
           end if
        end do
     end if

     !  CENTRAL ROCKIES VARIANT

     if ((VARACD.ne.'TT') .and. (VARACD.ne.'UT')) then
        if (LPPDOM) then
           EQWT(9) = 1.0
        else

           select case (IFMST)

           case(0)
              if (LDRY) then
                 EQWT(1) = 1.0
              else
                 EQWT(2) = 1.0
              end if

           case(1)
              if (LDRY) then
                 EQWT(6) = 1.0
              else
                 EQWT(5) = 1.0
              end if

           case(2)
              if (LDRY) then
                 EQWT(6) = 1.0
              else
                 EQWT(8) = 1.0
              end if

           case(3,4,6)
              EQWT(10) = 1.0

           case(5)
              if (PERCOV .ge.55) then
                 EQWT(8) = 1.0
              else if (PERCOV .lt.45) then
                 EQWT(2) = 1.0
              else
                 EQWT(2) = 1 - (PERCOV - 45)/10
                 EQWT(8) = 1 - (55 - PERCOV)/10
              end if

           end select
        end if

        !  UTAH AND TETONS VARIANTS

     else

        !  IS WHITE FIR PRESENT? ONLY PERTAINS TO UT VARIANT;
        !  WHITE FIR IS NOT A VALID SPECIES IN TT VARIANT

        LWFOK = .false.
        PSUM = 0.0
        if (VARACD.eq.'UT' .and. ISCT(4,1).gt.0) then
           do J = ISCT(4,1), ISCT(4,2)
              PSUM = PSUM + FMPROB(IND1(J))
           end do
        end if
        if (PSUM .gt.0.0) LWFOK = .true.

        !  STAND TPA

        X = 0.0
        do I = 1,ITRN
           X = X + FMPROB(I)
        end do

        if (PERCOV .le.50.0) then

           if (X .gt.1000.0) then
              if (LDRY) then
                 EQWT(2) = 1.0
              else
                 if (FMAVH .le.10.0) then
                    EQWT(5) = 1.0
                 else
                    EQWT(8) = 1.0
                 end if
              end if
           else ! < 1000
              if ((VARACD .eq. 'UT') .and.&
                   &(CTBA(PJCT) .gt.0.0) .and.&
                   &((CTBA(SFCT) .gt.0.0) .or. LWFOK)) then
                 if (LDRY) then
                    EQWT(6) = 1.0
                 else
                    EQWT(5) = 1.0
                 end if
              else
                 if (LDRY) then
                    EQWT(2) = 1.0
                 else
                    EQWT(8) = 1.0
                 end if
              end if
           end if ! end of > 1000

        else ! PERCOV > 50%

           if (LPPDOM) then
              EQWT(9) = 1.0
           else
              EQWT(8) = 1.0
           end if
        end if

     end if

     !  ASPEN COVER TYPE

  case(ASCT)
     if ((CTBA(ASCT)/MAX(1.0E-3,STNDBA)) .gt.0.80) then
        if ((VARACD .ne. 'UT') .and.&
             &(VARACD .ne. 'TT') .and. (IMODTY .eq.1)) then
           EQWT(2) = 1.0
        else
           EQWT(5) = 1.0
        end if

     else if (LCUNDR) then
        J = 0
        if (VARACD .eq. 'UT') then
           do I = 1,ITRN

              select case (ISP(I))

              case(6,11:16,18:22,24)
                 J = J

              case default
                 if (HT(I) .gt.10.0) then
                    J = J + 1
                    XW = CRWDTH(I)
                    XWID(J) = XW*XW*FMPROB(I)*0.785398
                 end if

              end select
           end do

           !  TETONS VARIANT

        else if (VARACD .eq. 'TT') then
           do I = 1,ITRN

              select case (ISP(I))

              case(4,6,11:16,18)
                 J = J

              case default
                 if (HT(I) .gt.10.0) then
                    J = J + 1
                    XW = CRWDTH(I)
                    XWID(J) = XW*XW*FMPROB(I)*0.785398
                 end if

              end select
           end do

           !  CENTRAL ROCKIES VARIANT

        else
           do I = 1,ITRN
              select case (ISP(I))
              case(12,16,20:35,38)
                 J = J
              case default
                 if (HT(I) .gt.10.0) then
                    J = J + 1
                    XW = CRWDTH(I)
                    XWID(J) = XW*XW*FMPROB(I)*0.785398
                 end if
              end select
           end do
        end if
        call COVOLP (DEBUG,JOSTND,J,IDUM,XWID,CVR10,CCCOEF)

        if (CVR10.gt.40.0) then
           EQWT(8) = 1.0
        else
           EQWT(2) = 1.0
        end if

     else if ((CTBA(PPCT) + CTBA(SFCT) + CTBA(LPCT)&
          &+ CTBA(MCCT) + CTBA(WSCT)) .gt.1.0) then
        ICT = MCCT
        goto 111
     else if (CTBA(OBCT) .gt.1.0) then
        ICT = OBCT
        goto 111
     else if (CTBA(PJCT) .gt.1.0) then
        ICT = PJCT
        if (SMALL .le.0.0) TINY = 1.0E-3
        goto 111
     end if

  end select

  !  END OF DETAILED LOW FUEL MODEL SELECTION

  !  DURING THE 5 YEARS AFTER AN ENTRY, AND ASSUMING THAT SMALL+LARGE
  !  ACTIVIVITY FUELS HAVE JUMPED BY 10%, THEN MODEL 11 IS A
  !  CANDIDATE MODEL, SHARING WITH 10. THE WEIGHT OF THE SHARED
  !  RELATIONSHIP DECLINES FROM PURE 11 INITIALLY, TO PURE 10 AFTER
  !  THE PERIOD EXPIRES.

  AFWT = MAX(0.0, 1.0 - (IYR - HARVYR) / 5.0)
  if (SLCHNG .ge. SLCRIT .or. LATFUEL) then
     LATFUEL = .true.
     EQWT(11) = AFWT
     if (AFWT .le.0.0) LATFUEL = .false.
  end if
  if (.not. LATFUEL) AFWT = 0.0

  !  MODELS 10,12 ARE ALWAYS CANDIDATE MODELS FOR NATURAL FUELS
  !  OTHER MODELS ARE ALSO CANDIDATES, DEPENDING ON COVER TYPE, ETC
  !  NO MODEL 13/14 IN UT

  EQWT(10) = 1.0 - AFWT
  EQWT(12) = 1.0

  !  CALL FMDYN TO RESOLVE WEIGHTS, SORT THE WEIGHTED FUEL MODELS
  !  FROM THE HIGHEST TO LOWEST, SET FMD (USING THE HIGHEST WEIGHT)

  call FMDYN(SMALL,LARGE,ITYP,XPTS,EQWT,IPTR,ICLSS,LDYNFM,FMD)

  if (DEBUG) write (JOSTND,8) FMD
  8 format (' FMCFMD, FMD=',I4)

  return
end
