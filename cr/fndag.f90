!================!
!   cr : fndag   !
!================!
! CR $Id$
subroutine FNDAG(I,TAGE,SITE,RHT,BAUTBA,ISPC,DEBUG)
  implicit none

  ! THIS ROUTINE USES THE LOGIC FROM GENGYM THAT CALCULATES A HEIGHT
  ! GIVEN A SITE TO FIND AN EFFECTIVE AGE.
  ! EVEN-AGED LOGIC FROM GEMHT

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'

  ! Variables
  logical :: DEBUG
  real    :: BAUTBA,RHT,SITE,TAGE,AGETEM,TOL,AP,AGEMAX,HG,HL
  real    :: HH,RATIO,HTMAX,CCFTEM,DIFF,TSITE
  integer :: ISPC,I

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

  !  USE SHEPPARDS EQN FOR ASPEN AND SURROGATE SPECIES.
  !  ALL MODEL TYPES: WB, AS, PB, OH
  !  ALSO FOR MODEL TYPE 3: NC, PW, UJ, AJ, RM, OJ, ER, OS

  if (ISPC.eq.20.or. ISPC.eq.28.or. ISPC.eq.38.or.&
       &ISPC.eq.14) go to 50
  if (IMODTY.eq.3.and. (ISPC.eq.21.or. ISPC.eq.22.or.&
       &ISPC.eq.16.or. (ISPC.ge.29.and. ISPC.le.32) .or.&
       &ISPC.eq.37)) go to 50

  go to 90

  ! THIS IS A BREAST HIGH AGE CURVE, ADJUST TO TOTAL AGE

  50 continue
  AGETEM = (RHT*12.0*2.54/26.9825)**0.8509
  AGETEM = AGETEM+(4.5/(0.1+SITE/50.))
  TAGE = AGETEM
  go to 9995

  90 continue
  TOL = 2.0
  AP = 10.0

  AGEMAX = 210.
  if (IMODTY .eq.3) AGEMAX = 165.
  if (IMODTY .eq.5) AGEMAX = 200.

  100 continue
  AGETEM = AP

  go to (1000,2000,3000,4000,5000,6000,6000,6000), IMODTY

  ! SOUTHWESTERN MIXED CONIFER  --- MODEL TYPE 1

  1000 continue
  !     IF(SITE .LT. 80.0) AGETEM = AMAX1(AGETEM,(110.0 - SITE))
  if (AGETEM .gt. AGEMAX) AGETEM = AGEMAX
  if (DEBUG)write(JOSTND,*)' IN FNDAG1 I,SITE,RHT,BAUTBA,ISPC,',&
       &'AGETEM= ',I,SITE,RHT,BAUTBA,ISPC,AGETEM
  HG = 109.559129*(1.0-0.975884*EXP(-0.014377*AGETEM))**1.289266&
       &+ 4.5
  HL = 72.512644*(1.0-0.876961*EXP(-0.020066*AGETEM))**2.016632
  HH = -((HG - HL) * ((82.488 - SITE) / 26.279)) + HG
  !     IF(SITE .LT. 80.0) THEN
  !       IF(AP .LT. AGETEM) HH = ((HH - 4.5) / AGETEM) * AP + 4.5
  !     ELSE
  !       IF(AP .LT. 20.0) HH = (0.02348 * SITE - 0.93429) * AP + 4.5
  !     ENDIF
  RATIO = 1.0 - BAUTBA
  if (RATIO .lt.0.768) RATIO = 0.768
  HH = HH * RATIO
  go to 9900

  ! SOUTHWESTERN PONDEROSA PINE -- MODEL TYPE 2

  2000 continue
  !     IF(SITE .LT. 80.0) AGETEM = AMAX1(AGETEM,(120.0 - SITE))
  if (AGETEM .gt. AGEMAX) AGETEM = AGEMAX
  if (DEBUG)write(JOSTND,*)' IN FNDAG2 I,SITE,RHT,BAUTBA,ISPC,',&
       &'AGETEM= ',I,SITE,RHT,BAUTBA,ISPC,AGETEM
  HG = 106.493954*(1.0-0.938775*EXP(-0.016066*AGETEM))**1.550720&
       &+ 4.5
  HL = 78.078735*(1.0-0.843715*EXP(-0.020412*AGETEM))**2.280435
  HH = -((HG - HL) * ((81.5585 - SITE) / 21.7149)) + HG
  !     IF(SITE .LT. 80.0) THEN
  !       IF(AP .LT. AGETEM) HH = ((HH - 4.5) / AGETEM) * AP + 4.5
  !     ELSE
  !       IF(AP .LT. 20.0) HH = (0.02463 * SITE - 1.1025) * AP + 4.5
  !     ENDIF
  RATIO = 1.0 - BAUTBA
  if (RATIO .lt.0.768) RATIO = 0.768
  HH = HH * RATIO
  go to 9900

  ! BLACK HILLS PONDEROSA PINE -- MODEL TYPE 3

  3000 continue

  ! CARL'S AMAX FUNCTION BLOWS UP AT AGE 180. THE CONSTANT
  ! 1.2999886 IS CARL'S FUNCTION EVALUATED AT AGE 179.
  ! SITE CURVES FLATTEN OFF AT AGE 180.

  HTMAX = (SITE + 0.3846) * 1.2999886
  if (RHT .ge. HTMAX) then
     TAGE = 165.
     go to 9995
  end if

  HH = (SITE + 0.3846) * (-0.5234 + 1.8234 *&
       &EXP(-(1.0989 - 0.006105 * AGETEM) ** 2.35))
  RATIO = 1.0 - BAUTBA
  if (RATIO .lt.0.793) RATIO = 0.793
  HH = HH * RATIO
  go to 9900

  ! SPRUCE-FIR -- MODEL TYPE 4

  4000 continue
  if (AGETEM .lt.30.0) AGETEM = 30.
  HH = (2.75780*SITE**0.83312) * ((1.0-EXP(-0.015701*AGETEM))&
       &**(22.71944*SITE**(-0.63557))) + 4.5
  !     IF(AP .LT. AGETEM) HH = ((HH-4.5) / AGETEM) * AP + 4.5
  RATIO = 1.0 - BAUTBA
  if (RATIO .lt.0.728) RATIO = 0.728
  HH = HH * RATIO
  go to 9900

  ! LODGEPOLE PINE -- MODEL TYPE 5

  5000 continue
  if (AGETEM .lt.30.0) AGETEM = 30.
  CCFTEM = RELDEN - 125.0
  if (CCFTEM .lt.0.0) CCFTEM = 0.0
  HH = 9.89331 - 0.19177*AGETEM + 0.00124*AGETEM*AGETEM&
       &- 0.00082*CCFTEM*SITE + 0.01387*AGETEM*SITE&
       &- 0.0000455*AGETEM*AGETEM*SITE
  !     IF(AP .LE. 30.0) HH = (HH / AGETEM) * AP
  RATIO = 1.0 - BAUTBA
  if (RATIO .lt.0.742) RATIO = 0.742
  HH = HH * RATIO
  go to 9900

  ! SPACE FOR FUTURE MODEL TYPES INCLUDING ASPEN.

  6000 continue
  go to 1000

  9900 continue
  DIFF = ABS(HH - RHT)
  if (DIFF .lt. TOL) go to 9990
  if (HH .gt. RHT) go to 9990
  AP = AP + 5.0
  if (AP .gt. AGEMAX) then
     TAGE = AGEMAX
     go to 9995
  end if
  go to 100
  9990 continue
  TAGE = AGETEM

  ! CURVES FOR MODEL TYPES 1,2, & 4 ARE BREAST HEIGHT AGE CURVES. ADD AGE
  ! TO BREAST HEIGHT TO CALCULATED AGE TO GET TOTAL AGE.

  if (IMODTY.eq.1) then
     TSITE = SITE
     if (TSITE.lt.30.)TSITE = 30.
     TAGE = TAGE+4.5/(-0.642+0.02285*TSITE)
  else if (IMODTY.eq.2) then
     TAGE = TAGE+4.5/(0.25+0.00467*SITE)
  else if (IMODTY.eq.4) then
     TSITE = SITE
     if (TSITE.lt.20.)TSITE = 20.
     TAGE = TAGE+4.5/(-0.22+0.0155*TSITE)
  end if
  9995 continue
  return
end
