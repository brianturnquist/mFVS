!======================!
!   fire/ca : fmcfmd   !
!======================!
! FIRE-CA $Id$
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
  !     THIS VARIANT IS VERY SIMILAR TO THE NC-FFE VERSION

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

  !     LOCAL VARIABLE DECLARATIONS

  ! Variables
  integer :: ICLSS,CWHRR,CWHRC
  parameter(ICLSS = 13) ! NUMBER OF FUEL MODELS
  parameter(CWHRR = 11) ! ROWS IN CWHR MATRIX
  parameter(CWHRC = 18) ! COLS IN CWHR MATRIXC

  character(len=3) :: SZDN
  character(len=2) :: FTLABL(0:CWHRR)
  character(len=2) :: SSLABL(0:CWHRC)
  character(len=3) :: SDTMP

  integer :: FMD,IFT,JSS
  integer :: IYR,I,J
  integer :: IPTR(ICLSS),ITYP(ICLSS)
  integer :: CWHR_MOD(4),IMD(2)
  integer :: FMD_R5(CWHRR,CWHRC),FMD_R6(CWHRR,CWHRC)
  !      INTEGER, DIMENSION (:,:), POINTER :: CWHRFMD
  integer :: CWHRFMD(CWHRR,CWHRC)
  real    :: XPTS(ICLSS,2),EQWT(ICLSS)
  real    :: BAPCT(MAXSP),STNDBA,AFWT,XMCON,XHARD
  real    :: CWHR_WT(4),CWXPTS(4,2),CCBP(3),DBHBP(4)
  logical :: DEBUG,LDRY

  integer :: FINDJSS,FINDMOD

  !     LABELS FOR DEBUG OUTPUT: FOREST TYPE CODES AND CWHR
  !     STRUCTURAL STAGES

  data     FTLABL /&
       &"? ", "P ","R ","WE","WW","D ","J ","H ","X ","MP","MC",&
       &"OS" /
  data     SSLABL /&
       &"? ",&
       &"1 ",&
       &"2S", "2P", "2M", "2D",&
       &"3S", "3P", "3M", "3D",&
       &"4S", "4P", "4M", "4D",&
       &"5S", "5P", "5M", "5D",&
       &"6 " /

  !     OREGON (R5) CWHR FUEL MODEL CLASSIFICATION: ROWS ARE FOREST TYPE; COLUMNS ARE
  !     STRUCTURAL STAGE DENSITY GROUPS:

  !     Row labels: "P","R","WE","WW","D","J","H","X","MP","MC","OS"

  data ((FMD_R5(I,J), J = 1,CWHRC), I = 1,CWHRR) /&
       &5, 6, 6, 6, 6, 2, 2, 9, 9, 2, 2, 2, 9, 2, 2, 9, 9,10,& ! 1 pine
       &5, 5, 5, 8, 8,11,11, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,10,& ! 2 red fir
       &5, 5, 5, 8, 8,11,11,11, 8, 8, 8, 8, 8, 8, 8, 8, 8,10,& ! 3 wh fir E
       &5, 5, 5, 8, 8,11,11, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,10,& ! 4 wh fir W
       &5, 5, 5, 6, 6, 6, 6, 8, 8,11,11, 9, 8,11,11, 9, 8,10,& ! 5 Douglas-fir
       &5, 2, 2, 6, 6, 2, 2, 2, 9, 2, 2, 2, 9, 2, 2, 2, 9,10,& ! 6 Jeffrey pine
       &5, 5, 5, 6, 6,11,11,11, 9, 9, 9, 9, 9, 9, 9, 9, 9,10,& ! 7 hardwoods
       &8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,10,& ! 8 lodgepole pine
       &5, 5, 5, 6, 6, 6, 6, 6, 9, 9, 9, 8, 8, 8, 8, 8, 8,10,& ! 9 mixed pine
       &5, 5, 5, 6, 6, 6, 6, 6, 8, 6, 6, 8, 8, 6, 6, 8, 8,10,& !10 mixed conifer
       &5, 5, 5, 6, 6, 6, 6, 6, 8, 6, 6, 8, 8, 6, 6, 8, 8,10 / !11 other soft

  !     CALIFORNIA (R6) CWHR FUEL MODEL CLASSIFICATION: ROWS ARE FOREST TYPE; COLUMNS ARE
  !     STRUCTURAL STAGE DENSITY GROUPS:

  !     Row labels: "P","R","WE","WW","D","J","H","X","MP","MC","OS"

  data ((FMD_R6(I,J), J = 1,CWHRC), I = 1,CWHRR) /&
       &5, 6, 6, 6, 6, 2, 2, 9, 9, 2, 2, 2, 9, 2, 2, 9, 9,10,& ! 1 pine
       &5, 5, 5, 8, 8,11,11, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,10,& ! 2 red fir
       &5, 5, 5, 8, 8,11,11,11, 8, 8, 8, 8, 8, 8, 8, 8, 8,10,& ! 3 wh fir E
       &5, 5, 5, 8, 8,11,11, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,10,& ! 4 wh fir W
       &5, 5, 5, 6, 6, 6, 6, 8, 8,11,11, 9, 8,11,11, 9, 8,10,& ! 5 Douglas-fir
       &5, 2, 2, 6, 6, 2, 2, 2, 9, 2, 2, 2, 9, 2, 2, 2, 9,10,& ! 6 Jeffrey pine
       &5, 5, 5, 6, 6,11,11,11, 9, 9, 9, 9, 9, 9, 9, 9, 9,10,& ! 7 hardwoods
       &8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,10,& ! 8 lodgepole pine
       &5, 5, 5, 6, 6, 6, 6, 6, 9, 9, 9, 8, 8, 8, 8, 8, 8,10,& ! 9 mixed pine
       &5, 5, 5, 6, 6, 6, 6, 6, 8, 6, 6, 8, 8, 6, 6, 8, 8,10,& !10 mixed conifer
       &5, 5, 5, 6, 6, 6, 6, 6, 8, 6, 6, 8, 8, 6, 6, 8, 8,10 / !11 other soft

  !     CWXPTS, CCBP & DBHBP ARE ALL USED BY **CWHR** AND ARE NOT UNDER
  !     USER CONTROL

  !     VALUES OF X-INTERCEPT AND Y-INTERCEPT FOR THE 4 CWHR MODELS
  !     X-AXIS IS % CANOPY COVER; Y-AXIS IS % CORRECTED CANOPY COVER
  !     THESE CORRESPOND TO SETTING THE HOCKEY-STICK FUNCTION
  !     BREAKPOINTS TO 10, 40 & 70%

  data (CWXPTS(J,1),J = 1,4) /  10.3,  45.0,  88.3, 185.0/
  data (CWXPTS(J,2),J = 1,4) / 195.0, 225.0, 265.0, 185.0/
  data CCBP  / 10.0, 40.0, 70.0 /
  data DBHBP / 1.0, 5.0, 11.0, 21.0 /

  !     THESE ARE THE INTEGER TAGS ASSOCIATED WITH EACH FIRE MODEL
  !     CLASS. THEY ARE RETURNED WITH THE WEIGHT

  data IPTR / 1,2,3,4,5,6,7,8,9,10,11,12,13/

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
       &30., 60.,& ! FMD  12
       &45.,100./ ! FMD  13

  !     INITIALLY SET ALL MODELS OFF; NO TWO CANDIDATE MODELS ARE COLINEAR, AND COLINEARITY
  !     WEIGHTS ARE ZERO. IF TWO CANDIDATE MODELS ARE COLINEAR, THE WEIGHTS MUST BE SET, AND
  !     MUST SUM TO 1, WRT EACH OTHER

  do I = 1,ICLSS
     EQWT(I) = 0.0
  end do

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

  !     ** BEGIN ROUTINE **

  LDRY = .false.
  if (IYR .ge. IDRYB .and. IYR .le. IDRYE) LDRY = .true.

  !     COPY OREGON- (R5) OR CAL-SPECIFIC (R6) FUEL MODEL MATRIX TO MATRIX
  !     THAT WILL ACTUALLY BE USED

  if ((KODFOR .ge.500.and. KODFOR .lt.600)&
       &.or. KODFOR .ge.700) then
     do I = 1,CWHRR
        do J = 1,CWHRC
           CWHRFMD(I,J) = FMD_R5(I,J)
        end do
     end do
  else
     do I = 1,CWHRR
        do J = 1,CWHRC
           CWHRFMD(I,J) = FMD_R6(I,J)
        end do
     end do
  end if

  !      IF (IFOR .GE. 6) THEN
  !        CWHRFMD => LOC(FMD_R6)
  !      ELSE
  !        CWHRFMD => LOC(FMD_R5)
  !      ENDIF

  !     COMPUTE TOTAL STAND BASAL AREA AND PERCENTAGE IN EACH SPECIES

  STNDBA = 0.0
  do I = 1, MAXSP
     BAPCT(I) = 0.0
     STNDBA = STNDBA + FMTBA(I)
  end do
  if (STNDBA .gt.0.001) then
     do I = 1, MAXSP
        BAPCT(I) = 100.0 * (FMTBA(I)/STNDBA)
     end do
  end if

  !     CALL CALIFORNIA WILDLIFE HABITAT RATING LOGIC TO CALCULATE
  !     STRUCTURAL STAGE/DENSITY CLASSIFICATION. THE RETURNED STRING
  !     CODES ARE CONVERTED TO GIVE THE COLUMN (JSS) FOR THE MAPPING
  !     TABLE

  call CWHR(ISP,DBH,HT,FMICR,FMPROB,CRWDTH,ITRN,&
       &CWXPTS,CCBP,DBHBP,&
       &SZDN,CWHR_MOD,CWHR_WT)

  JSS = FINDJSS(SZDN)

  !     FIND THE FOREST TYPE ROWS (IFT) FOR THE FIRE BEHAVIOUR MODEL TABLE

  XMCON = 0.0 ! mixed conifers
  do I = 1,3
     XMCON = XMCON + BAPCT(I)
  end do
  do I = 8,11
     XMCON = XMCON + BAPCT(I)
  end do
  do I = 13,14
     XMCON = XMCON + BAPCT(I)
  end do
  do I = 16,17
     XMCON = XMCON + BAPCT(I)
  end do
  do I = 19,25
     XMCON = XMCON + BAPCT(I)
  end do

  XHARD = 0.0 ! total hardwoods
  do I = 26,49
     XHARD = XHARD + BAPCT(I)
  end do

  IFT = 0
  if (BAPCT(18) .ge.80.0) then
     IFT = 1 ! pine
  else
     if ((BAPCT(4)+BAPCT(5)+BAPCT(6)) .ge.80.0) then
        if ((BAPCT(5)+BAPCT(6)) .ge. BAPCT(4)) then
           IFT = 2 ! red fir
        else
           if (SITEAR(ISISP) .lt.55.0) then
              IFT = 3 ! wh fir E
           else
              IFT = 4 ! wh fir W
           end if
        end if
     else
        if (BAPCT(7)      .ge.80.0) then ! Douglas-fir
           IFT = 5
        else if (BAPCT(15) .ge.80.0) then ! Jeffrey pine
           IFT = 6
        else if (XHARD     .ge.80.0) then ! hardwoods
           IFT = 7
        else if (BAPCT(12) .ge.80.0) then ! lodgepole pine
           IFT = 8
        else if (XMCON     .ge.80.0) then ! mixed conifer
           IFT = 10
        else
           if (BAPCT(18) .gt. BAPCT(4)) then ! mixed pine
              IFT = 9
           else
              IFT = 11 ! other softwood
           end if
        end if
     end if
  end if

  if (DEBUG) write(JOSTND,2) ICYC,IYR,&
       &FTLABL(IFT),IFT,SSLABL(JSS),JSS
  2 format(' FMCFMD CYCLE= ',I2,' IYR=',I4, ' IFT= ',A3,'(',I2,')'&
       &,  ' JSS= ',A3,'(',I2,')')

  !     TAKE FUEL MODEL FROM ROW (IFT) AND COLUMN (JSS) INDEX
  !     FAILURE TO FIND A VALID ENTRY: SET FMD=8 AND EXIT CODE

  if (IFT .gt.0.and. IFT .le. CWHRR .and.&
       &JSS .gt.0.and. JSS .le. CWHRC) then
     EQWT(FINDMOD(CWHRFMD(IFT, JSS),IPTR,ICLSS)) = 1.0
  else
     call RCDSET (2,.true.)
  end if

  !     RESOLVE DYNAMIC FMD ASSIGNMENTS FROM MODIFIED CWHR MODEL
  !     CWHR_WT:  WEIGHT OF MODEL TYPE (AT MOST 2 WILL BE RETURNED)
  !     CWHR_MOD: DYNAMIC ASSIGNMENT OF CWHR MODEL TYPE
  !               10=S-TYPE; 20=P-TYPE; 30=M-TYPE; 40=D-TYPE

  if (LDYNFM) then
     select case(SZDN(1:1))
     case ("2","3","4","5") ! BLENDING ONLY OCCURS WITH SS 2-5

        !           FIND CWHR MODEL COLUMNS FOR THE BLENDED MODEL GROUPS

        do I = 1,2
           IMD(I) = 0
           if (CWHR_WT(I) .gt.0.0) then
              SDTMP(1:2) = SZDN(1:2)
              select case(CWHR_MOD(I))
              case(10) ! S MODEL TYPES
                 SDTMP(3:3) = "S"
              case(20) ! P MODEL TYPES
                 SDTMP(3:3) = "P"
              case(30) ! M MODEL TYPE
                 SDTMP(3:3) = "M"
              case(40) ! D MODEL TYPE
                 SDTMP(3:3) = "D"
              end select
              IMD(I) = FINDMOD(CWHRFMD(IFT,FINDJSS(SDTMP)),IPTR,ICLSS)
           end if
        end do

        !           SOME EXTRA CARE IS REQUIRED WHEN FMD>13; THESE
        !           DON'T HAVE A DIRECT MAPPING INTO THE ARRAY;
        !           RATHER: 111 -> 14; 25 -> 15; 26 -> 16

        if (IMD(1) .eq.0.and. IMD(2) .eq.0) then
           EQWT(8) = 1.0
           call RCDSET (2,.true.)
        else if (IMD(1) .ne.0.and. IMD(2) .ne.0.and.&
             &(IMD(1) .ne. IMD(2)) ) then
           if (IMD(1) .ne. IMD(2)) then
              do I = 1,2
                 EQWT(IMD(I)) = CWHR_WT(I)
              end do
           end if
        end if
     end select
  end if

  !     END OF DETAILED LOW FUEL MODEL SELECTION

  !     IN CA-VARIANTS (WS,NC,IC) FM11 CAN BE PRESENT WITHOUT ACTIVITIES,
  !     VIA THE CWHR TABLE. WHEN THIS OCCURS, FM10/FM11 SHARING IS BYPASSED.

  if (EQWT(11) .eq.0.0) then

     !     DURING THE 5 YEARS AFTER AN ENTRY, AND ASSUMING THAT SMALL+LARGE
     !     ACTIVIVITY FUELS HAVE JUMPED BY 10%, THEN MODEL 11 IS A
     !     CANDIDATE MODEL, SHARING WITH 10. THE WEIGHT OF THE SHARED
     !     RELATIONSHIP DECLINES FROM PURE 11 INITIALLY, TO PURE 10 AFTER
     !     THE PERIOD EXPIRES.

     AFWT = MAX(0.0, 1.0 - (IYR - HARVYR) / 5.0)
     if (SLCHNG .ge. SLCRIT .or. LATFUEL) then
        LATFUEL = .true.
        EQWT(11) = AFWT
        if (AFWT .le.0.0) LATFUEL = .false.
     end if
     if (.not. LATFUEL) AFWT = 0.0
     EQWT(10) = 1.0 - AFWT
  else
     EQWT(10) = 0.0
  end if

  EQWT(12) = 1.0
  EQWT(13) = 1.0

  !     CALL FMDYN TO RESOLVE WEIGHTS, SORT THE WEIGHTED FUEL MODELS
  !     FROM THE HIGHEST TO LOWEST, SET FMD (USING THE HIGHEST WEIGHT)

  call FMDYN(SMALL,LARGE,ITYP,XPTS,EQWT,IPTR,ICLSS,LDYNFM,FMD)

  !     DELAY SHRUB DELAY IN CALIFORNIA VARIANTS

  call FMSHRUB (IYR,FMD)

  if (DEBUG) write (JOSTND,8) FMD
  8 format (' FMCFMD, FMD=',I4)

  return
end
