!======================!
!   fire/ws : fmcfmd   !
!======================!
! FIRE-WS $Id$
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

  !     LOCAL VARIABLE DECLARATIONS

  !     NUMBER OF POTENTIAL FIRE MODELS

  ! Variables
  integer :: ICLSS,CWHRR,CWHRC
  parameter(ICLSS = 15)
  parameter(CWHRR = 12) ! ROWS IN CWHR MATRIX
  parameter(CWHRC = 18) ! COLS IN CWHR MATRIX
  character(LEN = 3) :: SZDN,SDTMP
  character(LEN = 2),dimension(0:CWHRR) :: FTLABL
  character(LEN = 2),dimension(0:CWHRC) :: SSLABL
  logical :: DEBUG,LDRY
  integer :: FMD,IFT,JSS
  integer :: IYR,I,J
  integer :: IPTR(ICLSS),ITYP(ICLSS)
  integer :: CWHR_MOD(4),IMD(2)
  integer :: CWHRFMD(CWHRR,CWHRC)
  integer :: FINDJSS,FINDMOD
  real    :: XPTS(ICLSS,2),EQWT(ICLSS)
  real    :: BAPCT(MAXSP),STNDBA,AFWT,XMCON,XHARD
  real    :: CWHR_WT(4),CWXPTS(4,2),CCBP(3),DBHBP(4)

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
  !    35 = GIANT CHINKAPIN (GC)              CHRYSOLEPIS CHRYSOPHYLLA
  !    36 = QUAKING ASPEN (AS)                POPULUS TREMULOIDES
  !    37 = CALIFORNIA-LAUREL (CL)            UMBELLULARIA CALIFORNICA
  !    38 = PACIFIC MADRONE (MA)              ARBUTUS MENZIESII
  !    39 = PACIFIC DOGWOOD (DG)              CORNUS NUTTALLII
  !    40 = BIGLEAF MAPLE (BM)                ACER MACROPHYLLUM
  !    41 = CURLLEAF MOUNTAIN-MAHOGANY (MC)   CERCOCARPUS LEDIFOLIUS
  !    42 = OTHER SOFTWOODS (OS)
  !    43 = OTHER HARDWOODS (OH)

  !  DATA STATEMENTS

  !     LABELS FOR DEBUG OUTPUT: FOREST TYPE CODES AND CWHR
  !     STRUCTURAL STAGES

  data     FTLABL /&
       &"? ", "E ","P ","R ","W ","F ","D ","G ","J ",&
       &"H ","X ","MP","MC" /
  data     SSLABL /&
       &"? ",&
       &"1 ",&
       &"2S", "2P", "2M", "2D",&
       &"3S", "3P", "3M", "3D",&
       &"4S", "4P", "4M", "4D",&
       &"5S", "5P", "5M", "5D",&
       &"6 " /

  !     CWHR FUEL MODEL CLASSIFICATION: ROWS ARE FOREST TYPE; COLUMNS ARE STRUCTURAL
  !     STAGE DENSITY GROUPS:

  !     Row labels: "E","P","R","W","F","D","G","J","H","X", "MP", "MC"
  !     Col labels: modified from original WS-FFE workshop. Now expanded
  !                 so that there is some duplication

  data ((CWHRFMD(I,J), J = 1,CWHRC), I = 1,CWHRR) /&
       &9, 2, 2, 9, 9, 2, 2, 2, 9, 2, 2, 8, 8, 2, 2, 8, 8,10,& ! 1 pine E
       &9, 5, 5, 9, 9,26,26,25, 9,26,26, 8, 8,26,26, 8, 8,10,& ! 2 pine W
       &8, 8, 8, 8, 8,11,11, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,10,& ! 3 red fir
       &8, 8, 8, 8, 8,11,11,11, 8, 8, 8, 8, 8, 8, 8, 8, 8,10,& ! 4 wh fir E
       &8, 5, 5, 8, 8,11,11, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,10,& ! 5 wh fir W
       &8, 5, 5, 8, 8, 5, 5, 8, 8,11,11, 9, 8,11,11, 9, 8,10,& ! 6 Douglas-fir
       &8, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,10,& ! 7 giant sequoia
       &9, 9, 9, 9, 9, 2, 2, 2, 9, 2, 2, 2, 9, 2, 2, 2, 9,10,& ! 8 Jeffrey pine
       &8, 5, 5, 9, 9,11,11,11, 9, 9, 9, 9, 9, 9, 9, 9, 9,10,& ! 9 hardwoods
       &8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,10,& !10 lodgepole pine
       &9, 5, 5, 9, 9,26,26,25, 9,26,26, 8, 8,26,26, 8, 8,10,& !11 mixed pine
       &8, 9, 9, 8, 8,26,26,11, 8, 5, 5, 8, 8, 5, 5, 8, 8,10/ !12 mixed conifer

  !     CWXPTS, CCBP & DBHBP ARE ALL USED BY **CWHR** AND ARE NOT UNDER
  !     USER CONTROL

  !     VALUES OF X-INTERCEPT AND Y-INTERCEPT FOR THE 4 CWHR MODELS
  !     X-AXIS IS % CANOPY COVER; Y-AXIS IS % CORRECTED CANOPY COVER
  !     THESE CORRESPOND TO SETTING THE HOCKEY-STICK FUNCTION
  !     BREAKPOINTS TO 25, 40 & 60%

  !     VALUES OF X-INTERCEPT AND Y-INTERCEPT FOR THE 4 MODELS

  data (CWXPTS(J,1),J = 1,4) /  26.8,  42.2,  70.0, 180.0/
  data (CWXPTS(J,2),J = 1,4) / 187.5, 435.8, 350.0, 180.0/
  data CCBP  / 25.0, 40.0, 60.0 /
  data DBHBP / 1.0, 6.0, 11.0, 24.0 /

  !     THESE ARE THE INTEGER TAGS ASSOCIATED WITH EACH FIRE MODEL
  !     CLASS. THEY ARE RETURNED WITH THE WEIGHT

  data IPTR / 1,2,3,4,5,6,7,8,9,10,11,12,13,25,26 /

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
       &45.,100.,& ! FMD  13
       &5., 15.,& ! FMD  25
       &5., 15. / ! FMD  26

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

  !  TALLY UP THE BASAL AREA IN MIXED CONIFERS AND HARDWOODS
  !  SOFTWOODS CONSIDERED OUTSIDE MIXED CONIFERS ARE:
  !    2=DF, 3=WF, 4=GS, 6=JP, 7=RF, 8=PP, 9=LP
  !    (NOT SURE IF 22=BD SHOULD BE IN WITH DF, OR 23=RW SHOULD BE IN WITH GS
  !     THESE ARE BOTH GOING TO MIXED CONIFERS RIGHT NOW)
  !  OLD BLACK OAK AND TANOAK/CHINKAPIN GROUPS ARE NOW IN HARDWOODS

  XMCON = 0.0 ! mixed conifers
  XHARD = 0.0 ! total hardwoods
  do I = 1,MAXSP
     select case (I)
     case(1,5,10:27,42)
        XMCON = XMCON + BAPCT(I)
     case(28:41,43)
        XHARD = XHARD + BAPCT(I)
     end select
  end do

  !     FIND THE FOREST TYPE ROWS (IFT) FOR THE FIRE BEHAVIOUR MODEL TABLE

  IFT = 0
  if (BAPCT(8) .ge.80.0) then
     if (SITEAR(ISISP) .lt.55.0) then
        IFT = 1 ! E
     else
        IFT = 2 ! P
     end if
  else
     if ((BAPCT(7)+BAPCT(3)) .ge.80.0) then
        if (BAPCT(7) .ge. BAPCT(3)) then
           IFT = 3 ! R
        else
           if (SITEAR(ISISP) .lt.55.0) then
              IFT = 4 ! W
           else
              IFT = 5 ! F
           end if
        end if
     else
        if (BAPCT(2) .ge.80.0) then
           IFT = 6 ! D
        else if (BAPCT(4) .ge.80.0) then
           IFT = 7 ! G
        else if (BAPCT(6) .ge.80.0) then
           IFT = 8 ! J
        else if (XHARD .ge.80.0) then
           IFT = 9 ! H
        else if (BAPCT(9) .ge.80.0) then
           IFT = 10 ! X
        else if (XMCON .ge.80.0) then
           IFT = 12 ! MC
        else
           if (BAPCT(8) .ge. BAPCT(3)) then
              IFT = 11 ! MP
           else
              IFT = 12 ! MC
           end if
        end if
     end if
  end if

  if (DEBUG) write(JOSTND,2) ICYC,IYR,&
       &FTLABL(IFT),IFT,SSLABL(JSS),JSS
  2 format(' FMCFMD CYCLE= ',I2,' IYR=',I4, ' IFT= ',A3,'(',I2,')',&
       &' JSS= ',A3,'(',I2,')')

  !     TAKE FUEL MODEL FROM ROW (IFT) AND COLUMN (JSS) INDEX
  !     FAILURE TO FIND A VALID ENTRY: SET FMD=8 AND EXIT CODE

  if (IFT .gt.0.and. IFT .le. CWHRR.and.&
       &JSS .gt.0.and. JSS .le. CWHRC) then
     EQWT(FINDMOD(CWHRFMD(IFT, JSS),IPTR,ICLSS)) = 1.0
  else
     EQWT(8) = 1.0
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
