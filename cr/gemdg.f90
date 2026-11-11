!================!
!   cr : gemdg   !
!================!
! CR $Id$
subroutine GEMDG(DDS,IMODTY,IS,BAUTBA,SPBA,SI,DP,BAT,BARK,&
     &CR,SLOPE,ASPECT,ELEV,PBAL,PCCFI,RELDEN,BAL)
  implicit none

  !   THIS SUBROUTINE CALCULATES DDS. IT IS CALLED FROM DGF.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'GGCOM.f90'
  include 'VARCOM.f90'

  ! Variables
  real    :: BAL,RELDEN,PCCFI,PBAL,ELEV,SLOPE,CR,BARK,BAT,DP
  real    :: SI,SPBA,BAUTBA,DDS,DPP,BATEM,BGTTBA,BAL100
  integer :: IS,IMODTY,IDDS
  real    :: ASPECT,DF,CON1,CON2,DFMCPP,DFSWPP,ADJB,BAPP,BARAT,ADJR
  real    :: ADJ,SIMINR,XSLOPE,DIAGR
  real    :: RDANUW

  !  SPECIES ORDER:
  !   1=AF,  2=CB,  3=DF,  4=GF,  5=WF,  6=MH,  7=RC,  8=WL,  9=BC, 10=LM,
  !  11=LP, 12=PI, 13=PP, 14=WB, 15=SW, 16=UJ, 17=BS, 18=ES, 19=WS, 20=AS,
  !  21=NC, 22=PW, 23=GO, 24=AW, 25=EM, 26=BK, 27=SO, 28=PB, 29=AJ, 30=RM,
  !  31=OJ, 32=ER, 33=PM, 34=PD, 35=AZ, 36=CI, 37=OS, 38=OH

  !  SPECIES EXPANSION:
  !  UJ,AJ,RM,OJ,ER USE CR JU
  !  NC,PW USE CR CO
  !  GO,AW,EM,BK,SO USE CR OA
  !  PB USES CR AS
  !  PM,PD,AZ USE CR PI
  !  CI USES CR PP

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = ELEV

  !  SET APPROPRIATE BOUNDS BY MODEL TYPE.

  DPP = DP
  if (DPP .lt.1.0) DPP = 1.0
  BATEM = BAT
  BGTTBA = BAUTBA
  IDDS = 0
  BAL100 = BAL/100.

  ! SOUTHWEST MIXED CONIFERS TYPE

  if (IMODTY .eq.1) then
     if (BATEM .lt.65.0) BGTTBA = 0.0
     if (BATEM .lt.1.0) BATEM = 1.0

     ! SOUTHWEST PONDEROSA PINE TYPE

  else if (IMODTY .eq.2) then
     if (BATEM .lt.21.0) BATEM = 21.0

     ! BLACK HILLS PONDEROSA PINE TYPE

  else if (IMODTY .eq.3) then
     if (BATEM .lt.5.0) BATEM = 5.0

     ! SPRUCE-FIR TYPE

  else if (IMODTY .eq.4) then
     if (BATEM .lt.5.0) BATEM = 5.0

     !  LODGEPOLE PINE TYPE

  else if (IMODTY .eq.5) then
     if (BATEM .lt.14.0) BATEM = 14.0
  end if

  !  BRANCH TO EQUATIONS FOR APPROPRIATE SPECIES.

  select case (IS)

     !  SUB-ALPINE FIR
     !  CORKBARK FIR

     !  SWMC,SWPP USE CORKBARK EQN FOR GENGYM SWMC
     !  OTHER MODEL TYPES USE AF/CB EQN FROM GENGYM S-F & LP

  case(1:2)
     if (IMODTY .le.2) then
        DF = 0.50283 + 0.93999 * DPP - 0.0016822 * BAT + 0.00791&
             &* SI + 0.16235 * (ALOG(DPP))**2.0
     else
        DF = 2.19459 + 0.94659 * DPP - 0.20410 * BGTTBA&
             &- 0.35970 * ALOG(BATEM) + 0.11326 * ((ALOG(DPP))**2.0)&
             &+ 0.00615 * SI
     end if

     !  DOUGLAS-FIR

  case(3)
     DF = 0.58234 + 0.94542 * DPP - 0.0025092 * BAT&
          &+ 0.0070474 * SI - 0.14044 * BGTTBA + 0.15738 *&
          &(ALOG(DPP))**2.0

     !  GRAND FIR
     !  EQN FROM CENTRAL IDAHO VARIANT, HABITAT TYPE 520 (ABGR/CLUN)
     !  SAWTOOTH NATIONAL FOREST, ELEV=6000 FT

  case(4)
     DDS = -0.057982 + 0.977362 + 0.005045*60.&
          &- 0.000093*60. *60.  + 0.009335*SIN(ASPECT)*SLOPE&
          &- 0.004469*COS(ASPECT)*SLOPE - 0.033374*SLOPE&
          &- 0.418343*SLOPE*SLOPE&
          &+ 1.286963*ALOG(DPP) - 0.217923*ALOG(BAT)&
          &+ 1.175105*CR + 0.219013*CR*CR - 0.0004408*DPP*DPP&
          &- 0.000578*PBAL/ALOG(DPP+1) - 0.000512*PCCFI
     IDDS = 1

     !  WHITE FIR

  case(5)
     DF = 0.46172 + 1.04089 * DPP - 0.0019984 * BAT + 0.0065947&
          &* SI - 0.09197 * BGTTBA - 0.0011355 * DPP * DPP

     !  MOUNTAIN HEMLOCK
     !  MH EQUATION FROM NORTH IDAHO VARIANT, HABITAT TYPE 710 (TSME/XETE),
     !  BITTERROOT NATIONAL FOREST, ELEV=6000 FT

  case(6)
     DDS = -1.52111 + 0.08518*60.&
          &- 0.000943*60. *60.  + 0.13363*SIN(ASPECT)*SLOPE&
          &+ 0.17935*COS(ASPECT)*SLOPE + 0.07628*SLOPE&
          &+ 0.89778*ALOG(DPP) - 0.10744*0.01*RELDEN&
          &+ 1.28403*CR - 0.000484*DPP*DPP&
          &- 0.66110*BAL100/ALOG(DPP+1)
     IDDS = 1

     !  WESTERN RED CEDAR
     !  EQN FROM NI VARIANT, HABITAT TYPE 550 (THPL/OPHO)
     !  ELEVATION = 6000 FT

  case(7)
     DDS = 1.61452 - 0.00175*60.&
          &- 0.000067*60. *60.  + 0.05534*SIN(ASPECT)*SLOPE&
          &- 0.06625*COS(ASPECT)*SLOPE + 0.11931*SLOPE&
          &+ 0.58705*ALOG(DPP) - 0.15356*0.01*RELDEN&
          &+ 0.74596 * BAL100&
          &+ 1.29360*CR&
          &- 2.28375*BAL100/ALOG(DPP+1)
     IDDS = 1

     !  WESTERN LARCH
     !  EQN FROM NI VARIANT, 260 HABITAT TYPE, BITTERROOT NF
     !  ELEV = 6000 FT

  case(8)
     DDS = 0.51291 + 0.20004 + 0.03730*60.&
          &- 0.000433*60. *60.  + 0.03430*SIN(ASPECT)*SLOPE&
          &- 0.21337*COS(ASPECT)*SLOPE + 0.33523*SLOPE&
          &- 0.70216*SLOPE*SLOPE - 0.05438*0.01*RELDEN&
          &+ 0.54140*ALOG(DPP) + 0.43637 * BAL100&
          &+ 1.03478*CR + 0.07509*CR*CR - 0.000310*DPP*DPP&
          &- 2.03256*BAL100/ALOG(DPP+1)
     IDDS = 1

     !  LIMBER PINE
     !  EQN FROM UT VARIANT
     !  ORIGINAL EQN HAD + D-SQUARED TERM, CHANGED TO - TO AVOID
     !  RUNAWAY DIAMETER GROWTH  GD 01/11/00

  case(10)
     CON1 = SIN(ASPECT-0.7854)*SLOPE*(-0.01752)
     CON2 = COS(ASPECT-0.7854)*SLOPE*(-0.609774)
     DDS = 1.911884 + 0.001766*SI&
          &+ CON1&
          &+ CON2 - 2.05706*SLOPE&
          &+ 2.113263*SLOPE*SLOPE - 0.199592*0.01*RELDEN&
          &+ 0.213947*ALOG(DPP) - 0.358634 * BAL100&
          &+ 1.523464*CR - 0.0006538*DPP*DPP
     IDDS = 1

     !  LODGEPOLE PINE

  case(11)
     DF = 1.32652 + 0.96279 * DPP - 0.17138 * BGTTBA&
          &- 0.21827 * ALOG(BATEM) + 0.06298 * ((ALOG(DPP))**2.0)&
          &+ 0.00590 * SI

     !  BRISTLECONE PINE
     !  LIMBER PINE
     !  PINYON PINE, SINGLELEAF PINYON, BORDER PINYON, ARIZONA PINYON PINE
     !  UTAH JUNIPER, ALLIGATOR JUNIPER, ROCKY MTN JUNIPER, ONESEED JUNIPER
     !  EASTERN REDCEDAR
     !  GAMBEL OAK, ARIZONA WHITE OAK, EMORY OAK, BUR OAK, SILVERLEAF OAK
     !  OTHER SOFTWOODS

  case(9,12,16,23:27,29:35,37)
     DF = 0.25897 + 1.03129 * DPP - 0.0002025464 * BATEM&
          &+ 0.00177 * SI
     if ((DF-DPP) .gt.1.0) DF = DPP + 1.0

     !  PONDEROSA PINE, CHIHUAHUA PINE

  case(13,36)

     ! PONDEROSA PINE   --- SWMC TYPE

     if (IMODTY .eq.1) then
        DFMCPP = 0.26265 + 0.94001 * DPP - 0.0016405 * BAT&
             &+ 0.0060074 * SI + 0.16328 * (ALOG(DPP))**2.0
        BGTTBA = BAUTBA
        BATEM = BAT
        if (BATEM .lt.21.0) BATEM = 21.0
        DFSWPP = 4.10552 + 0.88872 * DPP&
             &-0.83531 * ALOG(BATEM) + 0.013781 * SI - 0.52784 * BGTTBA&
             &+ 0.23834 * (ALOG(DP))**2.0
        ADJB = 0.0
        if (BAT .gt.60.0.and. BAT .lt.100.0) then
           ADJB = -0.025 * BAT + 2.5
        else if (BAT .lt.60.1) then
           ADJB = 1.0
        end if
        BAPP = SPBA
        if (BAPP .lt.1.0) BAPP = 1.0
        BARAT = BAPP / BAT
        ADJR = 0.0
        if (BARAT .gt.0.3.and. BARAT .lt.0.5) then
           ADJR = 5.0 * BARAT - 1.5
        else if (BARAT .gt.0.499) then
           ADJR = 1.0
        end if
        ADJ = AMAX1(ADJB,ADJR)
        DF = (1.0 - ADJ) * DFMCPP + ADJ * DFSWPP

        ! PONDEROSA PINE  --- SWPP TYPE

     else if (IMODTY .eq.2) then
        DF = 4.10552 + 0.88872 * DPP&
             &- 0.83531 * ALOG(BATEM) + 0.013781 * SI&
             &- 0.52784 * BGTTBA + 0.23834 * (ALOG(DPP))**2.0

        ! PONDEROSA PINE -- BLACK HILLS PONDEROSA PINE TYPE

     else if (IMODTY .eq.3) then
        DF = 2.50428 + 0.94573 * DPP - 0.45765 * ALOG(BATEM)&
             &+ 0.01354*SI - 0.46350*BGTTBA + 0.05940*(ALOG(DPP))**2.0
        if (AGERNG.gt.40..and. BGTTBA.gt.0.3) then
           SIMINR = 0.57593 + 0.92561*SI
           if (BATEM .lt.21.) BATEM = 21.
           DFSWPP = 4.10552 + 0.88872 * DPP&
                &- 0.83531 * ALOG(BATEM) + 0.013781 * SIMINR&
                &- 0.52784 * BGTTBA + 0.23834 * (ALOG(DPP))**2.0
           if (DF .gt. DFSWPP) then
              if (BGTTBA .ge.0.5) then
                 DF = DFSWPP
                 if (DF.lt.(DPP+.1))DF = DPP+.1
              else
                 DF = (2.5 - 5.0 * BGTTBA) * DF&
                      &+ (-1.5 + 5.0 * BGTTBA) * DFSWPP
              end if
           end if
        end if

        !  PONDEROSA PINE  --- LODGEPOLE PINE TYPE
        !  ALSO USED FOR SPRUCE-FIR MODEL TYPE

     else
        DF = 2.50428 + 0.94573 * DPP - 0.45765 * ALOG(BATEM)&
             &+ 0.01354*SI - 0.46350*BGTTBA + 0.05940*(ALOG(DPP))**2.0
     end if

     !  WHITEBARK PINE
     !  EQN FROM EM VARIANT, HABITAT TYPE 850
     !  ELEV = 8000 FEET

  case(14)
     XSLOPE = SLOPE/10.
     DDS = 0.01545 + 1.5675 - 0.00565*80.&
          &- 0.01606*SIN(ASPECT)*XSLOPE&
          &+ 0.00270*COS(ASPECT)*XSLOPE - 0.20011*XSLOPE&
          &+ 0.80110*ALOG(DPP) + 0.00064 * BAL&
          &+ 1.02878*CR - 0.45448*CR*CR&
          &- 0.00328*BAL/ALOG(DPP+1) - 0.25717*ALOG(RELDEN)
     IDDS = 1

     !  WESTERN WHITE PINE

  case(15)
     DF = 0.89451 + 0.95435 * DPP - 0.0012183 * BAT + 0.00159&
          &* SI - 0.20815 * BGTTBA + 0.09987 * (ALOG(DPP))**2.0

     !  BLUE SPRUCE

  case(17)
     DF = 0.50941 + 1.02697 * DPP - 0.0035783 * BAT&
          &+ 0.01022 * SI - 0.00098825 * DPP * DPP

     !  ENGELMANN SPRUCE
     !  SWMC & SWPP USE SWMC EQN
     !  ALL OTHER MODEL TYPES USE S-F EQN

  case(18)
     if (IMODTY .le.2) then
        DF = 0.67074 + 1.03068 * DPP - 0.0020055 * BAT&
             &+ 0.00633 * SI - 0.0010875 * DPP * DPP
     else
        DF = 2.28652 + 0.94475 * DPP - 0.48056 * BGTTBA&
             &- 0.41632 * ALOG(BATEM) + 0.10778 * ((ALOG(DPP))**2.0)&
             &+ 0.01295 * SI
     end if

     !  WHITE SPRUCE

  case(19)
     DF = 2.94734 + 0.88423 * DPP - 0.64449 * ALOG(BATEM)&
          &+ 0.01207 * SI + 0.25363 * (ALOG(DPP))**2.0

     !  ASPEN, PAPER BIRCH
     !  NARROWLEAF COTTONWOOD, PLAINS COTTONWOOD
     !  OTHER HARDWOODS

     !  SWMC AND SWPP USE SWMC EQN
     !  ALL OTHER MODEL TYPES USE ASPEN MODEL TYPE EQN

  case(20,21:22,28,38)
     if (IMODTY .le.2) then
        DF = 0.24506 + 1.01291 * DPP - 0.00084659 * BAT&
             &+ 0.00631*SI
     else
        DF = 1.55986 + 1.01825 * DPP - 0.29342 * ALOG(BATEM)&
             &+ 0.00672*SI - 0.00073*BGTTBA
     end if

     ! DIAMETER GROWTH BEING UNDERESTIMATED, INCREASE ACCORDINGLY.

     DF = DF * 1.05

     !  PLACE FOR OTHER SPECIES

  case default
     DDS = -9.21
     IDDS = 1

  end select

  !  IF DIRECT LN(DDS) ESTIMATE, FIRST CONVERT TO DG ESTIMATE,
  !  THEN APPLY DSTAG REDUCTION IF CALLED FOR.  IF DIRECT DG
  !  ESTIMATE, APPLY DSTAG REDUCTION IF CALLED FOR. IN BOTH CASES,
  !  CONVERT TO LN(DDS) SCALE BEFORE EXITING.

  if (IDDS .gt.0) then
     DIAGR = SQRT(EXP(DDS)+((DPP*BARK)**2.0))-DPP*BARK
  else
     if (DF .gt. DBHMAX(IS)) DF = DBHMAX(IS)
     if (DF .lt. DPP) DF = DPP
     DIAGR = (DF - DPP) * BARK
  end if
  if (ISTAGF(IS).ne.0) DIAGR = DIAGR*DSTAG

  !  TEST RUNS SHOW BH PP GROWTH TO HIGH, REDUCE.  DIXON 3-10-93.

  if (IMODTY.eq.3.and. (IS.eq.13.or. IS.eq.36)) DIAGR = DIAGR*0.80

  if (DIAGR .le.0.) then
     DDS = -9.21
  else
     DDS = ALOG( (DIAGR * (2.0 * DPP * BARK + DIAGR)) )
     if (DDS .lt. -9.21) DDS = -9.21
  end if
  return
end
