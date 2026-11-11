!======================!
!   fire/ec : fmcfmd   !
!======================!
! FIRE-EC $Id$
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

  !  NUMBER OF HABITAT TYPES IN **HABTYP*

  ! Variables
  integer :: NPA
  parameter (NPA = 155)

  !  NUMBER OF POTENTIAL FIRE MODELS

  integer :: ICLSS
  parameter(ICLSS = 13)

  !  COVER METAGROUP ENUMS

  integer*2 :: DFCT,LPCT,SAFCT,MHCT,WPCT,ESCT,PSFCT,WLCT,PPCT,DFGFCT,PPDFCT,LPWLCT,SAFMCT,MMIXCT,&
       &DMIXCT,UBCT

  parameter(&
       !----------
       !  PURE (1 SPECIES > 50% BA)
       !----------
       &DFCT = 1,& ! Douglas-fir
       &LPCT = 2,& ! lodgepole pine and whitebark pine
       &SAFCT = 3,& ! subalpine fir, noble fir, and white fir
       &MHCT = 4,& ! mountain hemlock and western hemlock
       &WPCT = 5,& ! white pine
       &ESCT = 6,& ! Engelmann spruce
       &PSFCT = 7,& ! pacific silver fir
       &WLCT = 8,& ! western larch and subalpine larch
       &PPCT = 9,& ! ponderosa pine
       !----------
       !  MIXTURES (2 SPECIES > 50% BA)
       !----------
       &DFGFCT = 10,& ! Douglas-fir/grand fir
       &PPDFCT = 11,& ! ponderosa pine/Douglas-fir
       &LPWLCT = 12,& ! lodgepole pine/whitebark pine/
       !                      western larch/subalpine larch
       !----------
       !  DOG'S BREAKFAST
       !----------
       &SAFMCT = 13,& ! SAF leading but not > 50%
       &MMIXCT = 14,& ! moist habitat mixed conifer
       &DMIXCT = 15,& ! dry habitat mixed conifer
       &UBCT = 15) ! UPPER BOUND

  integer :: FMD
  integer :: IYR,I,J,ICT,IFMST
  integer :: IPTR(ICLSS),ITYP(ICLSS)
  real    :: XPTS(ICLSS,2),EQWT(ICLSS),WT1(4),WT2(4)
  real    :: CTBA(UBCT),STNDBA,X(2),Y(2),X1,Y1,QMD,AFWT
  real    :: ALGSLP,X3
  logical :: DEBUG,LSAFD,LSNGL

  !  FIXED VALUES FOR INTERPOLATION FUNCTION

  data     Y / 0.0, 1.0 /

  !  These are the integer tags associated with each fire model
  !  class. They are returned with the weight

  data IPTR / 1,2,3,4,5,6,7,8,9,10,11,12,13 /

  !  These are 0 for regular lines, -1 for horizontal and 1 for
  !  vertical lines. If any of the lines defined by xpts() are of
  !  an unusual variety, this must be entered here so that
  !  special logic can be invoked.  In this case, all the line
  !  segments have a |slope| that is > 0 and less than inif.

  data ITYP / ICLSS * 0 /

  !  XPTS: FIRST COLUMN ARE THE SMALL FUEL VALUES FOR EACH FIRE MODEL
  !    WHEN LARGE FUEL= 0 (I.E. THE X-INTERCEPT OF THE LINE). SECOND
  !    COLUMN CONTAINS THE LARGE FUEL VALUE FOR EACH FIRE MODEL WHEN
  !    SMALL FUEL=0 (I.E. THE Y-INTERCEPT OF THE LINE).

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

  !  INITIALLY SET ALL MODELS OFF; NO TWO CANDIDATE MODELS ARE COLINEAR, AND COLINEARITY
  !  WEIGHTS ARE ZERO. IF TWO CANDIDATE MODELS ARE COLINEAR, THE WEIGHTS MUST BE SET, AND
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

  !  COMPUTE QMD

  X1 = 0.0
  Y1 = 0.0
  do J = 1,ITRN
     X1 = X1 + FMPROB(I) * DBH(I) * DBH(I)
     Y1 = Y1 + FMPROB(I)
  end do
  QMD = 0.0
  if (Y1.gt.0.0) QMD = SQRT(X1/Y1)

  !  COMPUTE NUMBER OF LAYERS, USING STRUCTURE CLASS

  !    0 = BG = BARE GROUND
  !    1 = SI = STAND INITIATION
  !    2 = SE = STEM EXCLUSION
  !    3 = UR = UNDERSTORY REINITIATION
  !    4 = YM = YOUNG FOREST MULTISTRATA
  !    5 = OS = OLD FOREST SINGLE STRATUM
  !    6 = OM = OLD FOREST MULTISTRATA

  call FMSSTAGE(TPAMIN,CCMIN,PCTSMX,SAWDBH,SSDBH,GAPPCT,IFMST,X3,&
       &FMPROB,FMICR)

  select case (IFMST)
  case (0,1,2,5)
     LSNGL = .true.
  case (3,4,6)
     LSNGL = .false.
  case default
     LSNGL = .false.
  end select

  !  ZERO OUT THE COVER GROUPS AND COMPUTE TOTAL STAND BASAL AREA

  do I = 1,UBCT
     CTBA(I) = 0.
  end do
  STNDBA = 0.
  do I = 1,MAXSP
     STNDBA = STNDBA + FMTBA(I)
  end do

  CTBA(DFCT) = FMTBA(3)
  CTBA(LPCT) = FMTBA(7) + FMTBA(14)
  CTBA(SAFCT) = FMTBA(9) + FMTBA(15) + FMTBA(16)
  CTBA(MHCT) = FMTBA(12) + FMTBA(11)
  CTBA(WPCT) = FMTBA(1)
  CTBA(ESCT) = FMTBA(8)
  CTBA(PSFCT) = FMTBA(4)
  CTBA(WLCT) = FMTBA(2) + FMTBA(17)
  CTBA(PPCT) = FMTBA(10)
  CTBA(DFGFCT) = FMTBA(3)  + FMTBA(6)
  CTBA(PPDFCT) = FMTBA(10) + FMTBA(3)
  CTBA(LPWLCT) = FMTBA(7)  + FMTBA(14) + FMTBA(2) + FMTBA(17)

  !  FIND IF MOISTURE REGIME IS MOIST OR DRY **FMCMBA**

  call ECMOIST(I)
  if (I .eq.1) then
     CTBA(MMIXCT) = STNDBA
  else
     CTBA(DMIXCT) = STNDBA
  end if

  !  2 - FIND COVER TYPE METAGROUP *ICT* - (1) LOOK FOR >50% OF
  !      BASAL AREA IN A SINGLE-SPECIES GROUP; (2) LOOK FOR
  !      TWO-SPECIES GROUPS; (3) LOOK FOR SAF AS THE LARGEST SINGLE
  !      COMPONENT; (4) USE MOIST/DRY ASSIGNMENT. IF
  !      THERE ARE NO TREES, USE THE METAGROUP OF THE PREVIOUS CYCLE,
  !      INITIALIZED TO *LPCT* IN **FMVINIT**

  ICT = 0
  if ((ITRN .gt.0) .and. (STNDBA.gt.0.001)) then

     do I = DFCT,PPCT ! (1) - ONE SPP DOMINANT
        if ((CTBA(I)/STNDBA) .gt.0.50) then
           ICT = I
           goto 10
        end if
     end do

     do I = DFGFCT,LPWLCT ! (2) - TWO SPP DOMINANT
        if ((CTBA(I)/STNDBA) .gt.0.50) then
           ICT = I
           goto 10
        end if
     end do

     LSAFD = .true. ! (3) - SAF LEADING
     do I = 1, MAXSP
        if (I .ne.9.and. FMTBA(I) .gt. CTBA(SAFCT)) then
           LSAFD = .false.
           goto 11
        end if
     end do
     11 if (LSAFD) then
        ICT = SAFMCT
        goto 10
     end if

     do I = MMIXCT,DMIXCT ! (4) - MOIST/DRY
        if ((CTBA(I)/STNDBA) .gt.0.50) then
           ICT = I
           goto 10
        end if
     end do
     goto 10
  else
     ICT = OLDICT
  end if
  10 OLDICT = ICT

  !  3 - RESOLVE DYNAMIC FMD
  !  RULES ARE BASED ON A TABLE GIVEN BY TOM L. SARAH BEUKEMA AT
  !  ESSA DERIVED THIS LOGIC BASED ON THOSE VALUES. TRANSITION CUTPOINTS
  !  WERE MODIFIED AS FOLLOWS: %CC [ PERCOV ] CUTPOINTS WERE ADJUSTED SO
  !  THAT THEY BEGIN TO CHANGE -10% FROM THE CUTPOINT AND FINISH CHANGING
  !  +10% ABOVE THE CUTPOINT. SIZE [ QMD ] CUTPOINTS ARE ADJUSTED USING
  !  -1" AND +1" ABOUT THE CUTPOINT. THESE TRANSITIONS INSURE THAT THE
  !  MODEL BEHAVIOUR DOES NOT (USUALLY) EXHIBIT EXTREME JUMPS DURING STAND
  !  DEVELOPMENT

  do I = 1,4
     WT1(I) = 0.0
  end do

  select case (ICT)

     !  FIR, LODGEPOLE PINE, AND MIXED

  case (DFCT,DFGFCT,LPCT,LPWLCT,MHCT,PSFCT,&
       &SAFCT,WPCT,SAFMCT,MMIXCT)

     if (PERCOV .lt.30.0) then
        X(1) = 10.0
        X(2) = 30.0
        J = 2
     else if (PERCOV .ge.30.0.and. PERCOV .lt.60.0) then
        X(1) = 40.0
        X(2) = 60.0
        J = 3
     else if (PERCOV .ge.60.0) then
        X(1) = 70.0
        X(2) = 90.0
        J = 4
     end if
     WT1(J) = ALGSLP(PERCOV,X,Y,2)
     WT1(J-1) = 1.0 - WT1(J)

     if (WT1(1) .gt.0.0) then

        if (LSNGL) then
           if (ICT .eq. MMIXCT) then

              do I = 1,2
                 WT2(I) = 0.0
              end do

              if (QMD .le.9.0) then
                 X(1) = 7.0
                 X(2) = 9.0
                 J = 2
              else
                 X(1) = 19.0
                 X(2) = 21.0
                 J = 3
              end if
              WT2(J) = ALGSLP(QMD,X,Y,2)
              WT2(J-1) = 1.0 - WT2(J)

              EQWT(1) = EQWT(1) + WT1(1) * (WT2(1) + WT2(3))
              EQWT(5) = EQWT(5) + WT1(1) * WT2(2)

           else
              EQWT(1) = EQWT(1) + WT1(1)
           end if
        else
           EQWT(5) = EQWT(5) + WT1(1)
        end if
     end if

     if (WT1(2) .gt.0.0) then

        if (ICT .eq. LPCT   .or.&
             &ICT .eq. LPWLCT .or.&
             &ICT .eq. WPCT   .or.&
             &ICT .eq. DFCT) then

           X(1) = 2.0
           X(2) = 4.0
           J = 2
           WT2(J) = ALGSLP(QMD,X,Y,2)
           WT2(J-1) = 1.0 - WT2(J)

           if (ICT .eq. DFCT) then
              if (LSNGL) then
                 EQWT(5) = EQWT(5) + WT1(2)
              else
                 EQWT(8) = EQWT(8) + WT1(2)
              end if
           else
              EQWT(1) = EQWT(1) + WT1(2) * WT2(1)
              if (LSNGL) then
                 EQWT(5) = EQWT(5) + WT1(2) * WT2(2)
              else
                 EQWT(8) = EQWT(8) + WT1(2) * WT2(2)
              end if
           end if

        else if (ICT .eq. DFGFCT .or.&
             &ICT .eq. MHCT   .or.&
             &ICT .eq. PSFCT  .or.&
             &ICT .eq. MMIXCT) then

           if (LSNGL) then

              do I = 1,3
                 WT2(I) = 0.0
              end do

              if (QMD .le.4.0) then
                 X(1) = 2.0
                 X(2) = 4.0
                 J = 2
              else
                 X(1) = 19.0
                 X(2) = 21.0
                 J = 3
              end if
              WT2(J) = ALGSLP(QMD,X,Y,2)
              WT2(J-1) = 1.0 - WT2(J)

              EQWT(5) = EQWT(5) + WT1(2) * (WT2(1)+WT2(3))
              EQWT(8) = EQWT(8) + WT1(2) * WT2(2)

           else
              EQWT(8) = EQWT(8) + WT1(2)
           end if

        else if (ICT .eq. SAFCT .or.&
             &ICT .eq. SAFMCT) then

           if (LSNGL) then
              X(1) = 2.0
              X(2) = 4.0
              J = 2
              WT2(J) = ALGSLP(QMD,X,Y,2)
              WT2(J-1) = 1.0 - WT2(J)

              EQWT(5) = EQWT(5) + WT1(2) * WT2(1)
              EQWT(8) = EQWT(8) + WT1(2) * WT2(2)
           else
              EQWT(8) = EQWT(8) + WT1(2)
           end if

        end if
     end if

     if (WT1(3) .gt.0.0) then

        X(1) = 2.0
        X(2) = 4.0
        J = 2
        WT2(J) = ALGSLP(QMD,X,Y,2)
        WT2(J-1) = 1.0 - WT2(J)

        EQWT(5) = EQWT(5) + WT1(3) * WT2(1)
        EQWT(8) = EQWT(8) + WT1(3) * WT2(2)

     end if

     if (WT1(4) .gt.0.0) then

        if (LSNGL .and.&
             &(ICT .eq. LPCT .or.&
             &ICT .eq. WPCT .or.&
             &ICT .eq. LPWLCT)) then

           X(1) = 2.0
           X(2) = 4.0
           J = 2
           WT2(J) = ALGSLP(QMD,X,Y,2)
           WT2(J-1) = 1.0 - WT2(J)

           EQWT(5) = EQWT(5) + WT1(4) * WT2(1)
           EQWT(8) = EQWT(8) + WT1(4) * WT2(2)
        else
           EQWT(8) = EQWT(8) + WT1(4)
        end if

     end if

     !  ENGELMANN SPRUCE, WESTERN LARCH

  case (ESCT,WLCT)

     if (QMD .le.4.0) then
        X(1) = 2.0
        X(2) = 4.0
        J = 2
     else if (QMD .gt.4.0.and. QMD .lt.9.0) then
        X(1) = 7.0
        X(2) = 9.0
        J = 3
     else if (QMD .ge.9.0) then
        X(1) = 19.0
        X(2) = 21.0
        J = 4
     end if
     WT1(J) = ALGSLP(QMD,X,Y,2)
     WT1(J-1) = 1.0 - WT1(J)

     if (WT1(1) .gt.0.0) then

        do I = 1,3
           WT2(I) = 0.0
        end do

        if (PERCOV .le.30.0) then
           X(1) = 10.0
           X(2) = 30.0
           J = 2
        else
           X(1) = 70.0
           X(2) = 90.0
           J = 3
        end if
        WT2(J) = ALGSLP(PERCOV,X,Y,2)
        WT2(J-1) = 1.0 - WT2(J)

        EQWT(1) = EQWT(1) + WT1(1) * WT2(1)
        EQWT(5) = EQWT(5) + WT1(1) * WT2(2)
        EQWT(8) = EQWT(8) + WT1(1) * WT2(3)

     end if

     if (WT1(2) .gt.0.0) then

        do I = 1,2
           WT2(I) = 0.0
        end do

        X(1) = 10.0
        X(2) = 30.0
        J = 2

        WT2(J) = ALGSLP(PERCOV,X,Y,2)
        WT2(J-1) = 1.0 - WT2(J)

        if (LSNGL) then
           EQWT(1) = EQWT(1) + WT1(2) * WT2(1)
        else
           EQWT(5) = EQWT(5) + WT1(2) * WT2(1)
        end if
        EQWT(8) = EQWT(8) + WT1(2) * WT2(2)

     end if

     if (WT1(3) .gt.0.0) then

        do I = 1,2
           WT2(I) = 0.0
        end do

        X(1) = 10.0
        X(2) = 30.0
        J = 2

        WT2(J) = ALGSLP(PERCOV,X,Y,2)
        WT2(J-1) = 1.0 - WT2(J)

        if ((ICT .eq. ESCT) .or.&
             &((ICT .eq. WLCT) .and. LSNGL)) then
           EQWT(5) = EQWT(5) + WT1(3) * WT2(1)
           EQWT(8) = EQWT(8) + WT1(3) * WT2(2)
        else
           EQWT(8) = EQWT(8) + WT1(3)
        end if

     end if

     if (WT1(4) .gt.0.0) then

        do I = 1,2
           WT2(I) = 0.0
        end do

        X(1) = 10.0
        X(2) = 30.0
        J = 2

        WT2(J) = ALGSLP(PERCOV,X,Y,2)
        WT2(J-1) = 1.0 - WT2(J)

        if (((ICT .eq. ESCT) .and. LSNGL) .or.&
             &((ICT .eq. WLCT) .and..not.LSNGL)) then
           EQWT(5) = EQWT(5) + WT1(4) * WT2(1)
        else
           EQWT(1) = EQWT(1) + WT1(4) * WT2(1)
        end if
        EQWT(8) = EQWT(8) + WT1(4) * WT2(2)

     end if

     !  PONDEROSA PINE

  case (PPCT)

     if (PERCOV .lt.30.0) then
        X(1) = 10.0
        X(2) = 30.0
        J = 2
     else if (PERCOV .ge.30.0.and. PERCOV .lt.60.0) then
        X(1) = 40.0
        X(2) = 60.0
        J = 3
     else if (PERCOV .ge.60.0) then
        X(1) = 70.0
        X(2) = 90.0
        J = 4
     end if
     WT1(J) = ALGSLP(PERCOV,X,Y,2)
     WT1(J-1) = 1.0 - WT1(J)

     if (WT1(1) .gt.0.0) then
        if (LSNGL) then
           EQWT(2) = EQWT(2) + WT1(1)
        else
           EQWT(6) = EQWT(6) + WT1(1)
        end if
     end if

     if (WT1(2) .gt.0.0) then
        if (LSNGL) then
           EQWT(6) = EQWT(6) + WT1(2)
        else
           EQWT(9) = EQWT(9) + WT1(2)
        end if
     end if

     if (WT1(3) .gt.0.0) then

        do I = 1,2
           WT2(I) = 0.0
        end do

        X(1) = 2.0
        X(2) = 4.0
        J = 2

        WT2(J) = ALGSLP(QMD,X,Y,2)
        WT2(J-1) = 1.0 - WT2(J)

        EQWT(6) = EQWT(6) + WT1(3) * WT2(1)
        EQWT(9) = EQWT(9) + WT1(3) * WT2(2)

     end if

     if (WT1(4) .gt.0.0) then
        EQWT(9) = EQWT(9) + WT1(4)
     end if

     !  DRY MIXED

  case (DMIXCT)

     if (PERCOV .lt.30.0) then
        X(1) = 10.0
        X(2) = 30.0
        J = 2
     else if (PERCOV .ge.30.0.and. PERCOV .lt.60.0) then
        X(1) = 40.0
        X(2) = 60.0
        J = 3
     else if (PERCOV .ge.60.0) then
        X(1) = 70.0
        X(2) = 90.0
        J = 4
     end if
     WT1(J) = ALGSLP(PERCOV,X,Y,2)
     WT1(J-1) = 1.0 - WT1(J)

     if (WT1(1) .gt.0.0) then
        if (LSNGL) then
           EQWT(2) = EQWT(2) + WT1(1)
        else
           EQWT(6) = EQWT(6) + WT1(1)
        end if
     end if

     if (WT1(2) .gt.0.0) then
        if (LSNGL) then
           EQWT(6) = EQWT(6) + WT1(2)
        else
           EQWT(8) = EQWT(8) + WT1(2)
        end if
     end if

     if (WT1(3) .gt.0.0) then

        do I = 1,3
           WT2(I) = 0.0
        end do

        if (QMD .le.4.0) then
           X(1) = 2.0
           X(2) = 4.0
           J = 2
        else
           X(1) = 19.0
           X(2) = 21.0
           J = 3
        end if
        WT2(J) = ALGSLP(QMD,X,Y,2)
        WT2(J-1) = 1.0 - WT2(J)

        EQWT(6) = EQWT(6) + WT1(3) * WT2(1)
        if (LSNGL) then
           EQWT(9) = EQWT(9) + WT1(3) * WT2(2)
        else
           EQWT(8) = EQWT(8) + WT1(3) * WT2(2)
        end if
        EQWT(8) = EQWT(8) + WT1(3) * WT2(3)

     end if

     if (WT1(4) .gt.0.0) then
        EQWT(8) = EQWT(8) + WT1(4)
     end if

     !  PONDEROSA PINE - DOUGLAS-FIR

  case (PPDFCT)

     if (PERCOV .lt.30.0) then
        X(1) = 10.0
        X(2) = 30.0
        J = 2
     else if (PERCOV .ge.30.0.and. PERCOV .lt.60.0) then
        X(1) = 40.0
        X(2) = 60.0
        J = 3
     else if (PERCOV .ge.60.0) then
        X(1) = 70.0
        X(2) = 90.0
        J = 4
     end if
     WT1(J) = ALGSLP(PERCOV,X,Y,2)
     WT1(J-1) = 1.0 - WT1(J)

     if (WT1(1) .gt.0.0) then
        if (LSNGL) then
           EQWT(2) = EQWT(2) + WT1(1)
        else
           EQWT(6) = EQWT(6) + WT1(1)
        end if
     end if

     if (WT1(2) .gt.0.0) then

        if (LSNGL) then

           do I = 1,3
              WT2(I) = 0.0
           end do

           if (QMD .le.4.0) then
              X(1) = 2.0
              X(2) = 4.0
              J = 2
           else
              X(1) = 19.0
              X(2) = 21.0
              J = 3
           end if
           WT2(J) = ALGSLP(QMD,X,Y,2)
           WT2(J-1) = 1.0 - WT2(J)

           EQWT(6) = EQWT(6) + WT1(2) * (WT2(1) + WT2(3))
           EQWT(8) = EQWT(8) + WT1(2) *  WT2(2)
        else
           EQWT(8) = EQWT(8) + WT1(2)
        end if

     end if

     if (WT1(3) .gt.0.0) then

        if (LSNGL) then
           do I = 1,2
              WT2(I) = 0.0
           end do

           X(1) = 2.0
           X(2) = 4.0
           J = 2
           WT2(J) = ALGSLP(QMD,X,Y,2)
           WT2(J-1) = 1.0 - WT2(J)

           EQWT(6) = EQWT(6) + WT1(3) * WT2(1)
           EQWT(9) = EQWT(9) + WT1(3) * WT2(2)

        else
           EQWT(8) = EQWT(8) + WT1(3)
        end if

     end if

     if (WT1(4) .gt.0.0) then

        if (LSNGL) then
           do I = 1,3
              WT2(I) = 0.0
           end do

           X(1) = 2.0
           X(2) = 4.0
           J = 2
           WT2(J) = ALGSLP(QMD,X,Y,2)
           WT2(J-1) = 1.0 - WT2(J)

           EQWT(8) = EQWT(8) + WT1(3) * WT2(1)
           EQWT(9) = EQWT(9) + WT1(3) * WT2(2)

        else
           EQWT(8) = EQWT(8) + WT1(4)
        end if
     end if

  end select

  !  END OF DETAILED LOW FUEL MODEL SELECTION

  !  DURING THE 5 YEARS AFTER AN ENTRY, AND ASSUMING THAT SMALL+LARGE
  !  ACTIVIVITY FUELS HAVE JUMPED BY 10%, THEN MODEL 11 IS A
  !  CANDIDATE MODEL, SHARING WITH 10. THE WEIGHT OF THE SHARED
  !  RELATIONSHIP DECLINES FROM PURE 11 INITIALLY TO PURE 10 AFTER
  !  THE PERIOD EXPIRES.

  AFWT = MAX(0.0, 1.0 - (IYR - HARVYR) / 5.0)
  if (SLCHNG .ge. SLCRIT .or. LATFUEL) then
     LATFUEL = .true.
     EQWT(11) = AFWT
     if (AFWT .le.0.0) LATFUEL = .false.
  end if
  if (.not. LATFUEL) AFWT = 0.0

  !  MODELS 10,12,13 ARE ALWAYS CANDIDATE MODELS FOR NATURAL FUELS
  !  OTHER MODELS ARE ALSO CANDIDATES, DEPENDING ON COVER TYPE, ETC

  EQWT(10) = 1.0 - AFWT
  EQWT(12) = 1.0
  EQWT(13) = 1.0

  !  CALL FMDYN TO RESOLVE WEIGHTS, SORT THE WEIGHTED FUEL MODELS
  !  FROM THE HIGHEST TO LOWEST, SET FMD (USING THE HIGHEST WEIGHT)

  call FMDYN(SMALL,LARGE,ITYP,XPTS,EQWT,IPTR,ICLSS,LDYNFM,FMD)

  if (DEBUG) write (JOSTND,8) FMD
  8 format (' FMCFMD, FMD=',I4)

  return
end
