!=======================!
!   organon : voleqns   !
!=======================!
! ORGANON $Id$

subroutine CF(VERSION,SPP,SPGRP,DBH,HT,CR,CFTD,CFSH,VALU)
  !     CALCULATE ENDING OR STARTING CF VOLUME

  !     VALU = CF VOLUME TO BE CALCULATED

  !  04/25/2014 - THERE ARE COMMON SUBROUTINE NAMES IN THE SOURCE CODE
  !               USED TO BUILD THE ORGANON DLLS. IN ORDER TO LINK THE
  !               ORGANON SOURCE CODE WITH THE FVS SOURCE CODE WE CHANGED
  !               THE DUPLICATED SUBROUTINE NAMES TO MAKE THEM UNIQUE.

  !  CHANGED THE NAME OF SUBROUTINE - SCRIB TO SCRIB_VOL

  implicit none
  ! Variables
  integer*4 :: VERSION,SPP,SPGRP
  real*4  :: DBH,HT,CR,CFTD,CFSH,DIB,DIB1FT,VALU

  select case(VERSION)
  case(1) ! Southwest Oregon
     call SWO_DIB(SPGRP,DBH,CR,DIB)
     call SWO_DIB1FT(SPGRP,DBH,CR,DIB1FT)
  case(2,3) ! Western Willamette Valley, SMC
     call NWO_DIB(SPGRP,DBH,CR,DIB)
     call NWO_DIB1FT(SPGRP,DBH,CR,DIB1FT)
  case(4) ! Red Alder Plantations
     call RAP_DIB(SPGRP,DBH,DIB)
     call RAP_DIB1FT(SPGRP,DBH,CR,DIB1FT)
  end select
  if (SPP .le.300) then
     select case(VERSION)
     case(1) ! Southwest Oregon
        call SWO_CCFV(SPGRP,DBH,HT,CR,DIB,DIB1FT,CFTD,CFSH,VALU)
     case(2,3) ! Western Willamette Valley, SMC
        call NWO_CCFV(SPGRP,DBH,HT,CR,DIB,DIB1FT,CFTD,CFSH,VALU)
     case(4) ! Red Alder Plantations
        call RAP_CCFV(SPGRP,DBH,HT,CR,DIB,DIB1FT,CFTD,CFSH,VALU)
     end select
  else
     select case(VERSION)
     case(1) ! Southwest Oregon
        call SWO_HCFV(VERSION,SPGRP,DBH,HT,DIB,DIB1FT,CFTD,CFSH,&
             &VALU)
     case(2,3) ! Western Willamette Valley, SMC
        call NWO_HCFV(VERSION,SPGRP,DBH,HT,DIB,DIB1FT,CFTD,CFSH,&
             &VALU)
     case(4) ! Red Alder Plantations
        call RAP_HCFV(VERSION,SPGRP,DBH,HT,CR,DIB,DIB1FT,CFTD,&
             &CFSH,VALU)
     end select
  end if
  return
end

subroutine SWO_DIB(SPGRP,DBH,CR,DIB)
  implicit none
  ! Variables
  integer*4 :: SPGRP
  real*4  :: DBH,CR,DIB,BTPAR(18,3),E1,E2,E3

  !  BARK THICKNESS (3 parameters - all species)

  data BTPAR/&
       &0.92443655,  0.92162494,  0.80860026,  0.85897904,  0.87875535,& !  DF,GW,PP,SP,IC,
       &0.933707  ,  0.9497    ,  0.97      ,  0.96317   ,  0.94448   ,& !  WH,RC,PY,MD,GC,
       &0.859151  ,  0.910499  ,  0.97059   ,  0.878457  ,  0.889703  ,& !  TA,CL,BL,WO,BO,
       &0.947     ,  0.94448   ,  0.94448   ,& !  RA,PD,WI

       &0.98886654,  1.0       ,  1.01742589,  1.0       ,  1.0       ,& !  DF,GW,PP,SP,IC,
       &1.0       ,  1.0       ,  1.0       ,  1.0       ,  0.9875170 ,& !  WH,RC,PY,MD,GC,
       &1.0178109 ,  1.01475   ,  0.993585  ,  1.02393   ,  1.0104062 ,& !  TA,CL,BL,WO,BO,
       &1.0       ,  0.9875170 ,  0.9875170 ,& !  RA,PD,WI

       &-0.03414550, -0.03415396,  0.0       , 0.0        , -0.07696055,& !  DF,GW,PP,SP,IC,
       &0.0       ,  0.0       ,  0.0       , 0.0        ,  0.0       ,& !  WH,RC,PY,MD,GC,
       &0.0       ,  0.0       ,  0.0       , 0.0        ,  0.0       ,& !  TA,CL,BL,WO,BO,
       &0.0       ,  0.0       ,  0.0       / !  RA,PD,WI
  E1 = BTPAR(SPGRP,1)
  E2 = BTPAR(SPGRP,2)
  E3 = BTPAR(SPGRP,3)
  DIB = E1*(DBH**E2)*EXP(E3*(1.0-CR)**0.5)
  return
end

subroutine NWO_DIB(SPGRP,DBH,CR,DIB)
  implicit none
  ! Variables
  integer*4 :: SPGRP
  real*4  :: DBH,CR,DIB,BTPAR(11,3),E1,E2,E3

  !  BARK THICKNESS (3 parameters - all species)

  data BTPAR/&
       &0.971330, 0.92162494 , 0.933707, 0.9497  , 0.97    ,& !  DF,GF,WH,RC,PY
       &0.96317 , 0.97059    , 0.878457, 0.947   , 0.94448 ,& !  MD,BL,WO,RA,PD
       &0.94448 ,& !  WI

       &0.966365, 1.0        , 1.0     , 1.0     , 1.0     ,& !  DF,GF,WH,RC,PY
       &1.0     , 0.993585   , 1.02393 , 1.0     , 0.987517,& !  MD,BL,WO,RA,PD
       &0.987517,& !  WI

       &0.0     , -0.03415396, 0.0     , 0.0     , 0.0     ,& !  DF,GF,WH,RC,PY
       &0.0     ,  0.0       , 0.0     , 0.0     , 0.0     ,& !  MD,BL,WO,RA,PD
       &0.0     / !  WI
  if (SPGRP .eq.1) then
     E1 = 0.903563
     E2 = 0.989388
     E3 = 0.0
  else
     E1 = BTPAR(SPGRP,1)
     E2 = BTPAR(SPGRP,2)
     E3 = BTPAR(SPGRP,3)
  end if
  DIB = E1*DBH**E2*EXP(E3*(1.0-CR)**0.5)
  return
end

subroutine RAP_DIB(SPGRP,DBH,DIB)
  implicit none
  ! Variables
  integer*4 :: SPGRP
  real*4  :: DBH,DIB,BTPAR(7,2),E1,E2

  !  BARK THICKNESS (2 parameters - all species)

  data BTPAR/&
       &0.947   , 0.971330, 0.933707, 0.9497  , 0.97059 ,& !  RA,DF,WH,RC,BL
       &0.94448 , 0.94448 ,& !  PD,WI

       &1.0     , 0.966365, 1.0     , 1.0     , 0.993585,& !  RA,DF,WH,RC,BL
       &0.987517, 0.987517/ !  PD,WI

  E1 = BTPAR(SPGRP,1)
  E2 = BTPAR(SPGRP,2)
  DIB = E1*DBH**E2
  return
end

subroutine SWO_DIB1FT(SPGRP,DBH,CR,DIB1FT)
  implicit none
  ! Variables
  integer*4 :: SPGRP
  real*4  :: DBH,CR,DIB1FT,SDPAR(18,5),G0,G1,G2,G3,G4

  !  STUMP DIAMETER (4 parameters - all species)

  data SDPAR/&
       &0.149809111 , 0.393048214, 0.0        , 0.0       , 0.451569966 ,& !  DF,GW,PP,SP,IC,
       &0.0         , 0.451569966, 0.451569966, 0.0       , 0.0         ,& !  WH,RC,PY,MD,GC,
       &0.0         , 0.0        , 0.0        , 0.0       , 0.0         ,& !  TA,CL,BL,WO,BO,
       &0.0         , 0.0        , 0.0        ,& !  RA,PD,WI

       &0.900790279 , 0.729932627, 1.0        , 1.04030514, 0.831752493 ,& !  DF,GW,PP,SP,IC,
       &0.989819    , 0.831752493, 0.831752493, 1.0       , 1.0         ,& !  WH,RC,PY,MD,GC,
       &1.0         , 1.0        , 1.0        , 1.0       , 1.0         ,& !  TA,CL,BL,WO,BO,
       &1.0         , 1.0        , 1.0        ,& !  RA,PD,WI

       &0.133648456 , 0.120814754, 0.0        , 0.0       , 0.216216295 ,& !  DF,GW,PP,SP,IC,
       &0.0         , 0.216216295, 0.216216295, 0.0       , 0.0         ,& !  WH,RC,PY,MD,GC,
       &0.0         , 0.0        , 0.0        , 0.0       , 0.0         ,& !  TA,CL,BL,WO,BO,
       &0.0         , 0.0        , 0.0        ,& !  RA,PD,WI

       &3.67532829  , 1.0        , 1.0        , 1.0       , 7.00446878  ,& !  DF,GW,PP,SP,IC,
       &1.0         , 7.00446878 , 7.00446878 , 1.0       , 1.0         ,& !  WH,RC,PY,MD,GC,
       &1.0         , 1.0        , 1.0        , 1.0       , 1.0         ,& !  TA,CL,BL,WO,BO,
       &1.0         , 1.0        , 1.0        ,& !  RA,PD,WI

       &1.0213663112, 1.097851010, 1.0        , 1.0       , 1.0560026859,& !  DF,GW,PP,SP,IC,
       &1.0         , 1.056002686, 1.056002686, 1.0       , 1.0         ,& !  WH,RC,PY,MD,GC,
       &1.0         , 1.0        , 1.0        , 1.0       , 1.0         ,& !  TA,CL,BL,WO,BO,
       &1.0         , 1.0        , 1.0      / !  RA,PD,WI

  G0 = SDPAR(SPGRP,1)
  G1 = SDPAR(SPGRP,2)
  G2 = SDPAR(SPGRP,3)
  G3 = SDPAR(SPGRP,4)
  G4 = SDPAR(SPGRP,5)
  DIB1FT = G0+G1*EXP(G2*CR**G3)*DBH**G4
  return
end

subroutine NWO_DIB1FT(SPGRP,DBH,CR,DIB1FT)
  implicit none
  ! Variables
  integer*4 :: SPGRP
  real*4  :: DBH,CR,DIB1FT,SDPAR(11,5),G0,G1,G2,G3,G4

  !  STUMP DIAMETER (5 parameters - all species)

  data SDPAR/&
       &0.149809111 , 0.393048214, 0.0     , 0.451569966 , 0.451569966,& ! DF,GF,WH,RC,PY
       &0.0         , 0.0        , 0.0     , 0.0         , 0.0        ,& ! MD,BL,WO,RA,PD
       &0.0         ,& ! WI

       &0.900790279 , 0.729932627, 0.989819, 0.831752493 , 0.831752493,& ! DF,GF,WH,RC,PY
       &1.0         , 1.0        , 1.0     , 1.0         , 1.0        ,& ! MD,BL,WO,RA,PD
       &1.0         ,& ! WI

       &0.133648456 , 0.120814754, 0.0     , 0.216216295 , 0.216216295,& ! DF,GF,WH,RC,PY
       &0.0         , 0.0        , 0.0     , 0.0         , 0.0        ,& ! MD,BL,WO,RA,PD
       &0.0     ,& ! WI

       &3.67532829  , 1.0        , 1.0     , 7.00446878  , 7.00446878 ,& ! DF,GF,WH,RC,PY
       &1.0         , 1.0        , 1.0     , 1.0         , 1.0        ,& ! MD,BL,WO,RA,PD
       &1.0         ,& ! WI

       &1.0213663112, 1.097851010, 1.0     , 1.0560026859, 1.0560026859,& ! DF,GF,WH,RC,PY
       &1.0         , 1.0        , 1.0     , 1.0         , 1.0         ,& ! MD,BL,WO,RA,PD
       &1.0         / ! WI

  G0 = SDPAR(SPGRP,1)
  G1 = SDPAR(SPGRP,2)
  G2 = SDPAR(SPGRP,3)
  G3 = SDPAR(SPGRP,4)
  G4 = SDPAR(SPGRP,5)
  DIB1FT = G0+G1*EXP(G2*CR**G3)*DBH**G4
  return
end

subroutine RAP_DIB1FT(SPGRP,DBH,CR,DIB1FT)
  implicit none
  ! Variables
  integer*4 :: SPGRP
  real*4  :: DBH,CR,DIB1FT,SDPAR(7,5),G0,G1,G2,G3,G4

  !  STUMP DIAMETER (5 parameters - all species)

  data SDPAR/&
       &0.0     , 0.149809111 , 0.0         , 0.451569966 , 0.0     ,& !  RA,DF,WH,RC,BL
       &0.0     , 0.0         ,& !  PD,WI

       &1.0     , 0.900790279 , 0.989819    , 0.831752493 , 1.0     ,& !  RA,DF,WH,RC,BL
       &1.0     , 1.0         ,& !  PD,WI

       &0.0     , 0.133648456 , 0.0         , 0.216216295 , 0.0     ,& !  RA,DF,WH,RC,BL
       &0.0     , 0.0         ,& !  PD,WI

       &1.0     , 3.67532829  , 1.0         , 7.00446878  , 1.0     ,& !  RA,DF,WH,RC,BL
       &1.0     , 1.0         ,& !  PD,WI

       &1.0     , 1.0213663112, 1.0         , 1.0560026859, 1.0     ,& !  RA,DF,WH,RC,BL
       &1.0     , 1.0         / !  PD,WI

  G0 = SDPAR(SPGRP,1)
  G1 = SDPAR(SPGRP,2)
  G2 = SDPAR(SPGRP,3)
  G3 = SDPAR(SPGRP,4)
  G4 = SDPAR(SPGRP,5)
  DIB1FT = G0+G1*EXP(G2*CR**G3)*DBH**G4
  return
end

subroutine SWO_CCFV(SPGRP,DBH,HT,CR,DIB,DIB1FT,CFTD,CFSH,VALU)
  implicit none
  ! Variables
  integer*4 :: SPGRP
  real*4  :: DBH,HT,CR,DIB,DIB1FT,CFTD,CFSH,VALU,CCVPAR(8,8),CMVPAR(8,6),A1,A2,A3,A4,A5,EQNO1,B1,B2,&
       &B3,B4,B5,B6,B7,EQNO2,X1,X2,X3,VABH,VBBH,PI,HCB,CRABH,RAT,DIBD,SDIB

  !  CUBIC VOLUME - CONIFERS (8 parameters)

  data CCVPAR/&
       &1.24485628 ,  1.33397259 , 1.27677676 , 0.855844369, 0.90763281,& !  DF,GW,PP,SP,IC
       &1.168      ,  0.887      , 0.887      ,& !  WH,RC,PY

       &0.346490193,  0.357808283, 0.162198194, 0.388366991, 0.34284673,& !  DF,GW,PP,SP,IC
       &0.265430   ,  0.367622   , 0.367622   ,& !  WH,RC,PY

       &-0.56574969 , -0.755355039, 0.0        , 0.0     ,   -0.63865388,& !  DF,GW,PP,SP,IC
       &0.0        ,  0.0        , 0.0        ,& !  WH,RC,PY

       &0.632239239,  0.5        , 0.0        , 0.0     ,    1.5857204 ,& !  DF,GW,PP,SP,IC
       &0.0        ,  0.0        , 0.0        ,& !  WH,RC,PY

       &-0.152406551, -0.261766125, 0.0        , 0.0     ,    0.0       ,& !  DF,GW,PP,SP,IC
       &0.0        ,  0.0        , 0.0        ,& !  WH,RC,PY

       &4.55802463 ,  1.0        , 1.0        , 1.0     ,    1.0       ,& !  DF,GW,PP,SP,IC
       &1.0        ,  1.0        , 1.0        ,& !  WH,RC,PY

       &-0.051186711,  0.0        , 0.0        , 0.0     ,    0.0       ,& !  DF,GW,PP,SP,IC
       &0.0        ,  0.0        , 0.0        ,& !  WH,RC,PY

       &1.0        ,  1.0        , 0.0        , 0.0     ,    1.0       ,& !  DF,GW,PP,SP,IC
       &0.0        ,  0.0        , 0.0        / !  WH,RC,PY

  !  CONIFER CUBIC MERCH VOLUME - CONIFERS (6 parameters)

  data CMVPAR/&
       &-3.39101798 , -0.76519904, -4.87435933, -4.87435933, -3.75729892,& !  DF,GW,PP,SP,IC
       &0.930057   , 0.885038   , 0.885038   ,& !  WH,RC,PY

       &0.918583494, 0.25       , 1.19484691 ,  1.27588884,  1.23328561,& !  DF,GW,PP,SP,IC
       &3.74152    , 3.29655    , 3.29655    ,& !  WH,RC,PY

       &1.3330217  , 3.80136398 , 0.634341265,  0.63434126,  1.17859869,& !  DF,GW,PP,SP,IC
       &0.0        , 0.0        , 0.0        ,& !  WH,RC,PY

       &-0.935974246, -1.7902001 , 0.0        ,  0.0       , -0.45135743,& !  DF,GW,PP,SP,IC
       &0.0        , 0.0        , 0.0        ,& !  WH,RC,PY

       &3.0        , 1.0        , 1.0        ,  1.0       ,  2.0       ,& !  DF,GW,PP,SP,IC
       &0.0        , 0.0        , 0.0        ,& !  WH,RC,PY

       &1.0        , 1.0        , 1.0        ,  1.0       ,  1.0       ,& !  DF,GW,PP,SP,IC
       &0.0        , 0.0        , 0.0        / !  WH,RC,PY

  A1 = CMVPAR(SPGRP,1)
  A2 = CMVPAR(SPGRP,2)
  A3 = CMVPAR(SPGRP,3)
  A4 = CMVPAR(SPGRP,4)
  A5 = CMVPAR(SPGRP,5)
  EQNO1 = CMVPAR(SPGRP,6)
  B1 = CCVPAR(SPGRP,1)/1000.0
  B2 = CCVPAR(SPGRP,2)
  B3 = CCVPAR(SPGRP,3)
  B4 = CCVPAR(SPGRP,4)
  B5 = CCVPAR(SPGRP,5)
  B6 = CCVPAR(SPGRP,6)
  B7 = CCVPAR(SPGRP,7)
  EQNO2 = CCVPAR(SPGRP,8)
  VABH = 0.0
  VBBH = 0.0
  PI = 3.14159265

  !     CALCULATE CROWN RATIO ABOVE BREAST HEIGHT

  HCB = HT-CR*HT
  if (HCB .gt.4.5) then
     CRABH = (HT-HCB)/(HT-4.5)
  else
     CRABH = 1.0
  end if

  !     CALCULATE VOLUME ABOVE BREAST HEIGHT

  if (DIB.le.CFTD) go to 20
  if (EQNO1.lt.0.5) then
     RAT = 1.0-A1*(CFTD/DIB)**A2
  else
     RAT = 1.0-EXP(A1*(1.0-CFTD/DIB)**A2)*(CFTD/DIB)**(A3+A4*CR**A5)
  end if
  X1 = ((HT-4.5)/DBH)**(B2*(1.0-EXP(B3*DBH**B4))**EQNO2)
  X2 = EXP(B5*CRABH**B6)
  X3 = DBH**B7
  VABH = RAT*(B1*X1*X2*X3*DBH**2*(HT-4.5))

  !     VOLUME BELOW BREAST HEIGHT

  20 DIBD = (DIB/DIB1FT)**(2./3.)
  SDIB = (((4.5-DIBD-CFSH*(1.0-DIBD))/3.5)**1.5)*DIB1FT
  if (SDIB.le.CFTD) go to 10
  VBBH = (.25*PI*DIB1FT**2)*((1.0/43904.)*(729.0+81.0*DIBD+&
       &297.0*(DIB/DIB1FT)**(4./3.)+265.0*(DIB/DIB1FT)**2)-&
       &(1.0/6174)*((4.5-DIBD)**3*CFSH-&
       &1.5*(4.5-DIBD)**2*(1.0-DIBD)*CFSH**2+&
       &(4.5-DIBD)*(1.0-DIBD)**2*CFSH**3-&
       &(.25)*(1.0-DIBD)**3*CFSH**4))

  !     DETERMINE TREE'S VOLUME

  10 VALU = (VABH+VBBH)
  if (VALU .lt.0.0)VALU = 0.0
  return
end

subroutine NWO_CCFV(SPGRP,DBH,HT,CR,DIB,DIB1FT,CFTD,CFSH,VALU)
  implicit none
  ! Variables
  integer*4 :: SPGRP
  real*4  :: DBH,HT,CR,DIB,DIB1FT,CFTD,CFSH,VALU,CCVPAR(5,8),CMVPAR(5,6),A1,A2,A3,A4,A5,EQNO1,B1,B2,&
       &B3,B4,B5,B6,B7,EQNO2,X1,X2,X3,VABH,VBBH,PI,HCB,CRABH,RAT,DIBD,SDIB

  !  CUBIC VOLUME - CONIFERS (8 parameters)

  data CCVPAR/&
       &1.24485628 ,  1.33397259 , 1.168   ,  0.90763281,  0.90763281,& !  DF,GF,WH,RC,PY

       &0.346490193,  0.357808283, 0.265430,  0.34284673,  0.34284673,& !  DF,GF,WH,RC,PY

       &-0.56574969 , -0.755355039, 0.0     , -0.63865388, -0.63865388,& !  DF,GF,WH,RC,PY

       &0.632239239,  0.5        , 0.0     ,  1.5857204 ,  1.5857204 ,& !  DF,GF,WH,RC,PY

       &-0.152406551, -0.261766125, 0.0     ,  0.0       ,  0.0       ,& !  DF,GF,WH,RC,PY

       &4.55802463 ,  1.0        , 1.0     ,  1.0       ,  1.0       ,& !  DF,GF,WH,RC,PY

       &-0.051186711,  0.0        , 0.0     ,  0.0       ,  0.0       ,& !  DF,GF,WH,RC,PY

       &1.0        ,  1.0        , 0.0     ,  1.0       ,  1.0       / !  DF,GF,WH,RC,PY

  !  CONIFER CUBIC MERCH VOLUME - CONIFERS (6 parameters)

  data CMVPAR/&
       &-3.39101798 , -0.76519904, 0.930057, -3.75729892, -3.75729892,& !  DF,GF,WH,RC,PY

       &0.918583494,  0.25      , 3.74152 ,  1.23328561,  1.23328561,& !  DF,GF,WH,RC,PY

       &1.3330217  ,  3.80136398, 0.0     ,  1.17859869,  1.17859869,& !  DF,GF,WH,RC,PY

       &-0.935974246, -1.7902001 , 0.0     , -0.45135743, -0.45135743,& !  DF,GF,WH,RC,PY

       &3.0        ,  1.0       , 0.0     ,  2.0       ,  2.0       ,& !  DF,GF,WH,RC,PY

       &1.0        ,  1.0       , 0.0     ,  1.0       ,  1.0       / !  DF,GF,WH,RC,PY

  A1 = CMVPAR(SPGRP,1)
  A2 = CMVPAR(SPGRP,2)
  A3 = CMVPAR(SPGRP,3)
  A4 = CMVPAR(SPGRP,4)
  A5 = CMVPAR(SPGRP,5)
  EQNO1 = CMVPAR(SPGRP,6)
  B1 = CCVPAR(SPGRP,1)/1000.0
  B2 = CCVPAR(SPGRP,2)
  B3 = CCVPAR(SPGRP,3)
  B4 = CCVPAR(SPGRP,4)
  B5 = CCVPAR(SPGRP,5)
  B6 = CCVPAR(SPGRP,6)
  B7 = CCVPAR(SPGRP,7)
  EQNO2 = CCVPAR(SPGRP,8)
  VABH = 0.0
  VBBH = 0.0
  PI = 3.14159265

  !     CALCULATE CROWN RATIO ABOVE BREAST HEIGHT

  HCB = HT-CR*HT
  if (HCB .gt.4.5) then
     CRABH = (HT-HCB)/(HT-4.5)
  else
     CRABH = 1.0
  end if

  !     CALCULATE VOLUME ABOVE BREAST HEIGHT

  if (DIB.le.CFTD) go to 20
  if (EQNO1.lt.0.5) then
     RAT = 1.0-A1*(CFTD/DIB)**A2
  else
     RAT = 1.0-EXP(A1*(1.0-CFTD/DIB)**A2)*(CFTD/DIB)**(A3+A4*CR**A5)
  end if
  X1 = ((HT-4.5)/DBH)**(B2*(1.0-EXP(B3*DBH**B4))**EQNO2)
  X2 = EXP(B5*CRABH**B6)
  X3 = DBH**B7
  VABH = RAT*(B1*X1*X2*X3*DBH**2*(HT-4.5))

  !     VOLUME BELOW BREAST HEIGHT

  20 DIBD = (DIB/DIB1FT)**(2./3.)
  SDIB = (((4.5-DIBD-CFSH*(1.0-DIBD))/3.5)**1.5)*DIB1FT
  if (SDIB.le.CFTD) go to 10
  VBBH = (.25*PI*DIB1FT**2)*((1.0/43904.)*(729.0+81.0*DIBD+&
       &297.0*(DIB/DIB1FT)**(4./3.)+265.0*(DIB/DIB1FT)**2)-&
       &(1.0/6174)*((4.5-DIBD)**3*CFSH-&
       &1.5*(4.5-DIBD)**2*(1.0-DIBD)*CFSH**2+&
       &(4.5-DIBD)*(1.0-DIBD)**2*CFSH**3-&
       &(.25)*(1.0-DIBD)**3*CFSH**4))

  !     DETERMINE TREE'S VOLUME

  10 VALU = (VABH+VBBH)
  if (VALU .lt.0.0)VALU = 0.0
  return
end

subroutine RAP_CCFV(SPGRP,DBH,HT,CR,DIB,DIB1FT,CFTD,CFSH,VALU)
  implicit none
  ! Variables
  integer*4 :: SPGRP,ISPGRP
  real*4  :: DBH,HT,CR,DIB,DIB1FT,CFTD,CFSH,VALU,CCVPAR(3,8),CMVPAR(3,6),A1,A2,A3,A4,A5,EQNO1,B1,B2,&
       &B3,B4,B5,B6,B7,EQNO2,X1,X2,X3,VABH,VBBH,PI,HCB,CRABH,RAT,DIBD,SDIB

  !  CUBIC VOLUME - CONIFERS (3 parameters)

  data CCVPAR/&
       &1.24485628 , 1.168   ,  0.90763281,& !  DF,WH,RC

       &0.346490193, 0.265430,  0.34284673,& !  DF,WH,RC

       &-0.56574969 , 0.0     , -0.63865388,& !  DF,WH,RC

       &0.632239239, 0.0     ,  1.5857204 ,& !  DF,WH,RC

       &-0.152406551, 0.0     ,  0.0       ,& !  DF,WH,RC

       &4.55802463 , 1.0     ,  1.0       ,& !  DF,WH,RC

       &-0.051186711, 0.0     ,  0.0       ,& !  DF,WH,RC

       &1.0        , 0.0     ,  1.0       / !  DF,WH,RC

  !  CONIFER CUBIC MERCH VOLUME - CONIFERS (2 parameters)

  data CMVPAR/&
       &-3.39101798 , 0.930057, -3.75729892,& !  DF,WH,RC

       &0.918583494, 3.74152 ,  1.23328561,& !  DF,WH,RC

       &1.3330217  , 0.0     ,  1.17859869,& !  DF,WH,RC

       &-0.935974246, 0.0     , -0.45135743,& !  DF,WH,RC

       &3.0        , 1.0     ,  2.0       ,& !  DF,WH,RC

       &1.0        , 0.0     ,  1.0       / !  DF,WH,RC

  ISPGRP = SPGRP-1
  A1 = CMVPAR(SPGRP,1)
  A2 = CMVPAR(SPGRP,2)
  A3 = CMVPAR(SPGRP,3)
  A4 = CMVPAR(SPGRP,4)
  A5 = CMVPAR(SPGRP,5)
  EQNO1 = CMVPAR(SPGRP,6)
  B1 = CCVPAR(SPGRP,1)/1000.0
  B2 = CCVPAR(SPGRP,2)
  B3 = CCVPAR(SPGRP,3)
  B4 = CCVPAR(SPGRP,4)
  B5 = CCVPAR(SPGRP,5)
  B6 = CCVPAR(SPGRP,6)
  B7 = CCVPAR(SPGRP,7)
  EQNO2 = CCVPAR(SPGRP,8)
  VABH = 0.0
  VBBH = 0.0
  PI = 3.14159265

  !     CALCULATE CROWN RATIO ABOVE BREAST HEIGHT

  HCB = HT-CR*HT
  if (HCB .gt.4.5) then
     CRABH = (HT-HCB)/(HT-4.5)
  else
     CRABH = 1.0
  end if

  !     CALCULATE VOLUME ABOVE BREAST HEIGHT

  if (DIB.le.CFTD) go to 20
  if (EQNO1.lt.0.5) then
     RAT = 1.0-A1*(CFTD/DIB)**A2
  else
     RAT = 1.0-EXP(A1*(1.0-CFTD/DIB)**A2)*(CFTD/DIB)**(A3+A4*CR**A5)
  end if
  X1 = ((HT-4.5)/DBH)**(B2*(1.0-EXP(B3*DBH**B4))**EQNO2)
  X2 = EXP(B5*CRABH**B6)
  X3 = DBH**B7
  VABH = RAT*(B1*X1*X2*X3*DBH**2*(HT-4.5))

  !     VOLUME BELOW BREAST HEIGHT

  20 DIBD = (DIB/DIB1FT)**(2./3.)
  SDIB = (((4.5-DIBD-CFSH*(1.0-DIBD))/3.5)**1.5)*DIB1FT
  if (SDIB.le.CFTD) go to 10
  VBBH = (.25*PI*DIB1FT**2)*((1.0/43904.)*(729.0+81.0*DIBD+&
       &297.0*(DIB/DIB1FT)**(4./3.)+265.0*(DIB/DIB1FT)**2)-&
       &(1.0/6174)*((4.5-DIBD)**3*CFSH-&
       &1.5*(4.5-DIBD)**2*(1.0-DIBD)*CFSH**2+&
       &(4.5-DIBD)*(1.0-DIBD)**2*CFSH**3-&
       &(.25)*(1.0-DIBD)**3*CFSH**4))

  !     DETERMINE TREE'S VOLUME

  10 VALU = (VABH+VBBH)
  if (VALU .lt.0.0)VALU = 0.0
  return
end

subroutine SWO_HCFV(VERSION,SPGRP,DBH,HT,DIB,DIB1FT,CFTD,CFSH,&
     &VALU)
  implicit none
  ! Variables
  integer*4 :: VERSION,SPGRP,INDSPG
  real*4  :: DBH,HT,DIB,DIB1FT,CFTD,CFSH,VALU,HCVPAR(10,2),HMVPAR(10,3),A1,A2,A3,B0,B1,CVTS,CVM,CVS,&
       &PI,DIBD,SDIB
  integer*4 :: IDANUW

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  IDANUW = VERSION
  !                                                                      !  WH,RC,PY
  !  CUBIC VOLUME - HARDWOODS (2 parameters)

  data HCVPAR/&
       &0.09515994, 0.04969879, 0.06345756, 0.06345756, 0.06345756,& !  MD,GC,TA,CL,BL
       &0.06345756, 0.06345756, 0.04969879, 0.04969879, 0.04969879,& !  WO,BO,RA,PD,WI

       &0.00247940, 0.00247940, 0.00208240, 0.00208240, 0.00208240,& !  MD,GC,TA,CL,BL
       &0.00208240, 0.00208240, 0.00247940, 0.00247940, 0.00247940/ !  WO,BO,RA,PD,WI

  !  CUBIC MERCH VOLUME - HARDWODS (3 parameters)

  data HMVPAR/&
       &-0.2391, -0.3741, -0.2792, -0.3741, -0.4270, -0.3741, -0.3741,& ! MD,GC,TA,CL,BL,WO,BO
       &-0.4280, -0.3741, -0.3741,& !  RA,PD,WI

       &2.951 ,  3.642 ,  3.038 ,  3.642 ,  2.348 ,  3.642 ,  3.642 ,& ! MD,GC,TA,CL,BL,WO,BO
       &3.465 ,  3.642 ,  3.642 ,& !  RA,PD,WI

       &-2.512 , -3.406 , -2.603 , -3.406 , -2.276 , -3.406 , -3.406 ,& ! MD,GC,TA,CL,BL,WO,BO
       &-3.269 , -3.406 , -3.406/ !  RA,PD,WI

  !     CALCULATES CUBIC FOOT VOLUMES OF HARDWOODS USING
  !          EQUATIONS DEVELOPED FROM THE DATA IN J.A. SNELL AND S.N. LITTLE.
  !          1983.  PREDICTING CROWN WEIGHT AND BOLE VOLUME OF FIVE WESTERN
  !          HARDWOODS.  GENERAL TECHNICAL REPORT PNW-151.

  PI = 3.14159265
  INDSPG = SPGRP-8
  A1 = HMVPAR(INDSPG,1)
  A2 = HMVPAR(INDSPG,2)
  A3 = HMVPAR(INDSPG,3)
  B0 = HCVPAR(INDSPG,1)
  B1 = HCVPAR(INDSPG,2)

  !     CALCULATE DIB AND TREE TOTAL CUBIC FOOT VOLUME

  CVTS = B0+B1*DBH**2*HT

  !     CALCULATE OTHER MERCH. TOPS USING THE EQUATIONS IN SNELL AND LITTLE

  CVM = CVTS*(1.0+A1*CFTD**A2*DBH**A3)
  if (CVM .lt.0.0) CVM = 0.0

  !     CALCUATE STUMP VOLUME

  CVS = 0.0
  DIBD = (DIB/DIB1FT)**(2./3.)
  SDIB = (((4.5-DIBD-CFSH*(1.0-DIBD))/3.5)**1.5)*DIB1FT
  if (SDIB.le.CFTD) then
     VALU = 0.0
     return
  end if
  CVS = (.25*PI*DIB1FT**2)*((1.0/6174)*((4.5-DIBD)**3*CFSH-&
       &1.5*(4.5-DIBD)**2*(1.0-DIBD)*CFSH**2+&
       &(4.5-DIBD)*(1.0-DIBD)**2*CFSH**3-&
       &(.25)*(1.0-DIBD)**3*CFSH**4))

  !     CALCUATE TREE'S VOLUME

  VALU = CVM-CVS
  if (VALU .lt.0.0)VALU = 0.0
  return
end

subroutine NWO_HCFV(VERSION,SPGRP,DBH,HT,DIB,DIB1FT,CFTD,CFSH,&
     &VALU)
  implicit none
  ! Variables
  integer*4 :: VERSION,SPGRP,INDSPG
  real*4  :: DBH,HT,DIB,DIB1FT,CFTD,CFSH,VALU,HCVPAR(6,2),HMVPAR(6,3),A1,A2,A3,B0,B1,CVTS,CVM,CVS,PI,&
       &DIBD,SDIB
  integer*4 :: IDANUW

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  IDANUW = VERSION

  !  CUBIC VOLUME - HARDWOODS (2 parameters)

  data HCVPAR/&
       &0.09515994, 0.06345756, 0.06345756, 0.04969879, 0.04969879,& !  MD,BL,WO,RA,PD
       &0.04969879,& !  WI

       &0.00247940, 0.00208240, 0.00208240, 0.00247940, 0.00247940,& !  MD,BL,WO,RA,PD
       &0.00247940/ !  WI

  !  CUBIC MERCH VOLUME - HARDWODS (3 parameters)

  data HMVPAR/&
       &-0.2391, -0.4270, -0.3741, -0.4280, -0.3741, -0.3741,& !  MD,BL,WO,RA,PD,WI

       &2.951 ,  2.348 ,  3.642 ,  3.465 ,  3.642 ,  3.642 ,& !  MD,BL,WO,RA,PD,WI

       &-2.512 , -2.276 , -3.406 , -3.269 , -3.406 , -3.406/ !  MD,BL,WO,RA,PD,WI

  !     CALCULATES CUBIC FOOT VOLUMES OF HARDWOODS USING
  !          EQUATIONS DEVELOPED FROM THE DATA IN J.A. SNELL AND S.N. LITTLE.
  !          1983.  PREDICTING CROWN WEIGHT AND BOLE VOLUME OF FIVE WESTERN
  !          HARDWOODS.  GENERAL TECHNICAL REPORT PNW-151.

  PI = 3.14159265
  INDSPG = SPGRP-5
  A1 = HMVPAR(INDSPG,1)
  A2 = HMVPAR(INDSPG,2)
  A3 = HMVPAR(INDSPG,3)
  B0 = HCVPAR(INDSPG,1)
  B1 = HCVPAR(INDSPG,2)

  !     CALCULATE DIB AND TREE TOTAL CUBIC FOOT VOLUME

  CVTS = B0+B1*DBH**2*HT

  !     CALCULATE OTHER MERCH. TOPS USING THE EQUATIONS IN SNELL AND LITTLE

  CVM = CVTS*(1.0+A1*CFTD**A2*DBH**A3)
  if (CVM .lt.0.0) CVM = 0.0

  !     CALCUATE STUMP VOLUME

  CVS = 0.0
  DIBD = (DIB/DIB1FT)**(2./3.)
  SDIB = (((4.5-DIBD-CFSH*(1.0-DIBD))/3.5)**1.5)*DIB1FT
  if (SDIB.le.CFTD) then
     VALU = 0.0
     return
  end if
  CVS = (.25*PI*DIB1FT**2)*((1.0/6174)*((4.5-DIBD)**3*CFSH-&
       &1.5*(4.5-DIBD)**2*(1.0-DIBD)*CFSH**2+&
       &(4.5-DIBD)*(1.0-DIBD)**2*CFSH**3-&
       &(.25)*(1.0-DIBD)**3*CFSH**4))

  !     CALCUATE TREE'S VOLUME

  VALU = CVM-CVS
  if (VALU .lt.0.0)VALU = 0.0
  return
end

subroutine RAP_HCFV(VERSION,SPGRP,DBH,HT,CR,DIB,DIB1FT,CFTD,CFSH,&
     &VALU)
  implicit none
  ! Variables
  integer*4 :: VERSION,SPGRP,INDSPG
  real*4  :: DBH,HT,CR,DIB,DIB1FT,CFTD,CFSH,VALU,HCVPAR(3,2),HMVPAR(3,3),A1,A2,A3,B0,B1,CVTS,CVM,CVS,&
       &PI,DIBD,SDIB
  real*4  :: RDANUW
  integer*4 :: IDANUW

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  IDANUW = VERSION
  RDANUW = CR

  !  CUBIC VOLUME - HARDWOODS (2 parameters)

  data HCVPAR/&
       &0.06345756, 0.04969879, 0.04969879,& !  BL,PD,WI

       &0.00208240, 0.00247940, 0.00247940/ !  BL,PD,WI

  !  CUBIC MERCH VOLUME - HARDWODS (3 parameters)

  data HMVPAR/&
       &-0.4270, -0.3741, -0.3741,& !  BL,PD,WI

       &2.348 ,  3.642 ,  3.642 ,& !  BL,PD,WI

       &-2.276 , -3.406 , -3.406/ !  BL,PD,WI

  !     CALCULATES CUBIC FOOT VOLUME OF RED ALDER USING THE TAPER EQUATION OF
  !         HIBBS, BLUHM, AND GARBER (2007)
  if (SPGRP .eq.1) then
     !         CALL RA_CFV(DBH,HT,CR,MH,CFSH,CFTD,VALU)
     VALU = 0.04969879+0.00247940*DBH**2*HT
     return
  end if

  !     CALCULATES CUBIC FOOT VOLUMES OF HARDWOODS USING
  !          EQUATIONS DEVELOPED FROM THE DATA IN J.A. SNELL AND S.N. LITTLE.
  !          1983.  PREDICTING CROWN WEIGHT AND BOLE VOLUME OF FIVE WESTERN
  !          HARDWOODS.  GENERAL TECHNICAL REPORT PNW-151.

  PI = 3.14159265
  INDSPG = SPGRP-4
  A1 = HMVPAR(INDSPG,1)
  A2 = HMVPAR(INDSPG,2)
  A3 = HMVPAR(INDSPG,3)
  B0 = HCVPAR(INDSPG,1)
  B1 = HCVPAR(INDSPG,2)

  !     CALCULATE DIB AND TREE TOTAL CUBIC FOOT VOLUME

  CVTS = B0+B1*DBH**2*HT

  !     CALCULATE OTHER MERCH. TOPS USING THE EQUATIONS IN SNELL AND LITTLE

  CVM = CVTS*(1.0+A1*CFTD**A2*DBH**A3)
  if (CVM .lt.0.0) CVM = 0.0

  !     CALCUATE STUMP VOLUME

  CVS = 0.0
  DIBD = (DIB/DIB1FT)**(2./3.)
  SDIB = (((4.5-DIBD-CFSH*(1.0-DIBD))/3.5)**1.5)*DIB1FT
  if (SDIB.le.CFTD) then
     VALU = 0.0
     return
  end if
  CVS = (.25*PI*DIB1FT**2)*((1.0/6174)*((4.5-DIBD)**3*CFSH-&
       &1.5*(4.5-DIBD)**2*(1.0-DIBD)*CFSH**2+&
       &(4.5-DIBD)*(1.0-DIBD)**2*CFSH**3-&
       &(.25)*(1.0-DIBD)**3*CFSH**4))

  !     CALCUATE TREE'S VOLUME

  VALU = CVM-CVS
  if (VALU .lt.0.0)VALU = 0.0
  return
end

subroutine SCRIB_VOL(VERSION,ISP,SPGRP,SVOL,LOGLL,LOGTD,LOGSH,&
     &LOGTA,LOGML,DOB,HT,CR,VALU)

  !     VALUE = SCRIBNER VOLUME CALCULATED

  implicit none
  ! Variables
  integer*4 :: VERSION,ISP,SPGRP,SVOL,LOGLL
  integer*4 :: NDI,NLOGTD,NW,I,J,II
  real*4  :: LOGTD,LOGSH,LOGTA,LOGML,DOB,HT,CR,VALU
  real*4  :: D,TLL,EX,HCB,HABH,PDIB,AA1,AA2,A3,A4,ALP,WLT,PP1,PP2,H,DI,VOLG,NL(40,4),LVOL(40,4),&
       &TOTS(2,4),A,B,C,ROOT,MH,HM1,HM2

  D = 0.0
  TLL = 0.0
  VALU = 0.0
  do I = 1,2
     do J = 1,4
        TOTS(I,J) = 0.
     end do
  end do
  if (VERSION .eq.4.and. ISP .eq.351) then
     call RA_SCRIB(SVOL,LOGLL,LOGTD,LOGSH,LOGTA,LOGML,DOB,HT,CR,&
          &VALU)
     return
  end if
  if (ISP .gt.300) return
  EX = 1.0
  HCB = (1.0-CR)*HT
  HABH = HT-4.5

  select case(VERSION)
  case(1) ! Southwest Oregon
     call SWO_DIB(SPGRP,DOB,CR,PDIB)
     call SWO_TAPER(SPGRP,AA1,AA2,A3,A4,ALP)
  case(2,3) ! Western Willamette Valley, SMC
     call NWO_DIB(SPGRP,DOB,CR,PDIB)
     call NWO_TAPER(SPGRP,AA1,AA2,A3,A4,ALP)
  case(4) !Red Alder Plantations
     call RAP_DIB(SPGRP,DOB,PDIB)
     call RAP_TAPER(SPGRP,AA1,AA2,A3,A4,ALP)
  end select
  WLT = (ALP*HCB-4.5)/HABH
  PP1 = AA1+AA2*EXP(A3*(HABH/DOB)**2)
  PP2 = A4
  if (PDIB.le.LOGTD) go to 10

  !     COMPUTE MERCHANTABLE HEIGHT

  H = ALP*HCB
  call LOGVOL(1,SVOL,LOGLL,WLT,H,HABH,PP1,PP2,PDIB,D,TLL,EX,DI,&
       &VOLG,NL,LVOL,TOTS)
  if (WLT .le.0.0) then
     A = -(PP1+1.0)/HABH**2
     B = PP1/HABH
     C = 1.0-LOGTD/PDIB
  else if (WLT .gt.0..and. DI .le. LOGTD) then
     A = PP2/HABH**2
     B = PP1/HABH
     C = 1.0-LOGTD/PDIB
  else
     A = (PP2*WLT**2-PP1-2*PP2*WLT-1.0)/(HABH**2*(WLT-1.0)**2)
     B = ((2*WLT-1.0+PP2*WLT**2+PP1*WLT**2)-(PP2*WLT**2-PP1-&
          &2*PP2*WLT-1.0))/(HABH*(WLT-1.0)**2)
     C = -(LOGTD/PDIB+(2*WLT-1.0+PP2*WLT**2+PP1*WLT**2)/&
          &(WLT-1.0)**2)
  end if
  ROOT = B**2-4*A*C
  if (ROOT .lt.0.0) then
     MH = 0.
     go to 21
  end if
  HM1 = (-B+SQRT(ROOT))/(2*A)
  HM2 = (-B-SQRT(ROOT))/(2*A)
  if (HM1.gt.0.0.and. HM1.le. HABH) then
     H = HM1+4.5
     call LOGVOL(4,SVOL,LOGLL,WLT,H,HABH,PP1,PP2,PDIB,D,TLL,EX,DI,&
          &VOLG,NL,LVOL,TOTS)
     NDI = NINT(DI*10.0)
     NLOGTD = NINT(LOGTD*10.0)
     if (NDI .ne. NLOGTD) HM1 = 0.0
  else
     HM1 = 0.0
  end if
  if (HM2.gt.0.0.and. HM2.le. HABH) then
     H = HM2+4.5
     call LOGVOL(4,SVOL,LOGLL,WLT,H,HABH,PP1,PP2,PDIB,D,TLL,EX,DI,&
          &VOLG,NL,LVOL,TOTS)
     NDI = NINT(DI*10.0)
     NLOGTD = NINT(LOGTD*10.0)
     if (NDI .ne. NLOGTD) HM2 = 0.0
  else
     HM2 = 0.0
  end if
  if (HM1.le.0..or. HM1.gt. HABH) then
     if (HM2.le.0..or. HM2.gt. HABH) then
        MH = 0.
        go to 21
     else
        MH = HM2
     end if
  else if (HM2.lt.0..or. HM2.gt. HABH) then
     MH = HM1
  else
     MH = MAX(HM1,HM2)
  end if
  MH = MH+4.5
  21 continue

  !     CALCULATE LOG VOLUMES

  NW = INT((MH-LOGSH)/(FLOAT(LOGLL)+LOGTA))
  if (NW .lt.0) NW = 0
  TLL = MH-LOGSH-FLOAT(NW)*(FLOAT(LOGLL)+LOGTA)

  H = LOGSH
  do II = 1,NW
     H = H+FLOAT(LOGLL)+LOGTA
     call LOGVOL(3,SVOL,LOGLL,WLT,H,HABH,PP1,PP2,PDIB,1.0,TLL,EX,DI,&
          &VOLG,NL,LVOL,TOTS)
     VALU = VALU+VOLG
  end do

  !     COMPUTE VOLUME OF TOP LOG

  if (TLL .ge. (LOGML+LOGTA)) then
     J = INT(TLL-LOGTA)
     TLL = FLOAT(J)+LOGTA
     D = TLL/FLOAT(LOGLL)
     H = H+TLL
     call LOGVOL(3,SVOL,LOGLL,WLT,H,HABH,PP1,PP2,PDIB,D,TLL,EX,DI,&
          &VOLG,NL,LVOL,TOTS)
     VALU = VALU+VOLG
  end if
  10 continue
  if (VALU .lt.0.)VALU = 0.
  return
end

subroutine LOGVOL(N,SVOL,LOGLL,WLT,H,HABH,PP1,PP2,PDIB,D,TLL,EX,&
     &DI,V,NL,LVOL,TOTS)
  implicit none
  !     SUBROUTINE LOGVOL(N,SVOL,EX,D,V)
  !     ROUTINE TO CALCULATE LOG VOLUME AND ADD TO APPROPRIATE CELL

  !     N = TYPE OF CALCULATION
  !       = 1  MERCHANTABLE HEIGHT
  !       = 2  LOG VOLUME
  !       = 3  TREE VOLUME
  !       = 4  TOP DIAMETER CHECK

  !     SVOL = SPECIES GROUP FOR LOG REPORT

  !     EX = TREE RESIDUAL OR CUT EXPANSION FACTOR

  !     D = RATIO OF LOG LENGTH TO SPECIFIED LOG LENGTH

  ! Variables
  integer*4 :: N,SVOL,LOGLL
  integer*4 :: DII,LEN
  real*4  :: WLT,H,HABH,PP1,PP2,PDIB,D,TLL,EX,DI,NL(40,4),LVOL(40,4),TOTS(2,4),V
  real*4  :: I1,I2,RH,JP1,JP2,A,B,C,SVTBL(80),SVTBL16(6),SVTBL32(6)
  data SVTBL/0., .143, .39, .676, 1.07, 1.160, 1.4, 1.501, 2.084,&
       &3.126, 3.749, 4.9, 6.043, 7.14, 8.88, 10., 11.528,&
       &13.29, 14.99, 17.499, 18.99, 20.88, 23.51, 25.218,&
       &28.677, 31.249, 34.22, 36.376, 38.04, 41.06, 44.376,&
       &45.975, 48.99, 50., 54.688, 57.66, 64.319, 66.73, 70.,&
       &75.24, 79.48, 83.91, 87.19, 92.501, 94.99, 99.075,&
       &103.501, 107.97, 112.292, 116.99, 121.65, 126.525,&
       &131.51, 136.51, 141.61, 146.912, 152.21, 157.71,&
       &163.288, 168.99, 174.85, 180.749, 186.623, 193.17,&
       &199.12, 205.685, 211.81, 218.501, 225.685, 232.499,&
       &239.317, 246.615, 254.04, 261.525, 269.04, 276.63,&
       &284.26, 292.501, 300.655,308.97/
  data SVTBL16/1.249,1.608,1.854,2.410,3.542,4.167/
  data SVTBL32/1.57,1.8,2.2,2.9,3.815,4.499/

  !     USE TAPER EQUATION TO DETERMINE DIAMETER AT TOP OF LOG

  V = 0.0
  if (N .eq.1) then
     RH = WLT
  else
     RH = (H-4.5)/HABH
  end if
  if (WLT .le.0.0) then
     I2 = 0.0
  else
     I2 = 1.0
  end if
  if (RH .ge.0.0.and. RH .le. WLT) then
     I1 = 0.0
  else
     I1 = 1.0
  end if
  JP1 = (RH-1.0)/(WLT-1.0)
  JP2 = (WLT-RH)/(WLT-1.0)
  A = 1.0-RH+I2*(RH+I1*(JP1*(1.0+JP2)-1.0))-(RH-1.0)*(RH-I2*RH)
  B = PP1*(I2*(RH+I1*(JP1*(RH+WLT*JP2)-RH))-(RH-1.0)*(RH-I2*RH))
  C = PP2*I2*((RH**2)+I1*(JP1*WLT*(2.0*RH-WLT+WLT*JP2)-RH**2))
  DI = PDIB*(A+B+C)
  if (N.eq.1.or.N.eq.4)return

  !     EXTRACT VOLUME FROM VOLUME TABLES

  if (D .lt.1.0.or. D .gt.1.0) then
     LEN = INT(TLL)
  else
     LEN = LOGLL
  end if
  DII = INT(DI)
  if (DII .ge.6.and. DII .le.11) then
     if (LEN .ge.16.and. LEN .le.31) then
        V = (SVTBL16(DII-5)*FLOAT(LEN))*EX
     else if (LEN .ge.32.and. LEN .le.40) then
        V = (SVTBL32(DII-5)*FLOAT(LEN))*EX
     else
        V = (SVTBL(DII)*FLOAT(LEN))*EX
     end if
  else
     V = (SVTBL(DII)*FLOAT(LEN))*EX
  end if
  if (N .eq.3) return
  DII = DII/2
  NL(DII,SVOL) = NL(DII,SVOL)+EX
  LVOL(DII,SVOL) = LVOL(DII,SVOL)+V
  TOTS(2,SVOL) = TOTS(2,SVOL)+V
  return
end

subroutine SWO_TAPER(SPGRP,AA1,AA2,A3,A4,ALP)
  implicit none
  ! Variables
  integer*4 :: SPGRP
  real*4  :: AA1,AA2,A3,A4,ALP,TPAR(8,5)

  !  TAPER (5 parameters - conifer only)

  data TPAR/&
       &-0.55029801,-0.342017552, -0.595823501 ,-0.6       ,-0.596278066,& !  DF,GW,PP,SP,IC
       &-0.55029801,-0.596278066, -0.596278066 ,& !  WH,RC,PY

       &-0.69479837,-0.777574201, -1.25803662  ,-0.48435806,-0.83987883 ,& !  DF,GW,PP,SP,IC
       &-0.69479837,-0.83987883 , -0.83987883  ,& !  WH,RC,PY

       &-6.13100423,-4.33569876 ,-13.867406    ,-3.3249206 ,-6.85768402 ,& !  DF,GW,PP,SP,IC
       &-6.13100423,-6.85768402 , -6.85768402  ,& !  WH,RC,PY

       &0.35697451, 0.672963393,  0.0998711245, 0.10862035, 0.134178717,& !  DF,GW,PP,SP,IC
       &0.35697451, 0.134178717,  0.134178717 ,& !  WH,RC,PY

       &0.5       , 0.33       ,  0.6         , 0.74      , 0.71       ,& !  DF,GW,PP,SP,IC
       &0.5       , 0.71       ,  0.71/ !  WH,RC,PY

  AA1 = TPAR(SPGRP,1)
  AA2 = TPAR(SPGRP,2)
  A3 = TPAR(SPGRP,3)/100.0
  A4 = TPAR(SPGRP,4)
  ALP = TPAR(SPGRP,5)
  return
end

subroutine NWO_TAPER(SPGRP,AA1,AA2,A3,A4,ALP)
  implicit none
  ! Variables
  integer*4 :: SPGRP
  real*4  :: AA1,AA2,A3,A4,ALP,TPAR(5,5)

  !  TAPER (5 parameters - conifer only)

  data TPAR/&
       &-0.55029801,-0.342017552,-0.55029801,-0.596278066,-0.596278066,& ! DF,GF,WH,RC,PY

       &-0.69479837,-0.777574201,-0.69479837,-0.83987883 ,-0.83987883 ,& ! DF,GF,WH,RC,PY

       &-6.13100423,-4.33569876 ,-6.13100423,-6.85768402 ,-6.85768402 ,& ! DF,GF,WH,RC,PY

       &0.35697451, 0.672963393, 0.35697451, 0.134178717, 0.134178717,& ! DF,GF,WH,RC,PY

       &0.5       ,  0.33      ,  0.5      ,  0.71      ,  0.71      / ! DF,GF,WH,RC,PY

  AA1 = TPAR(SPGRP,1)
  AA2 = TPAR(SPGRP,2)
  A3 = TPAR(SPGRP,3)
  A4 = TPAR(SPGRP,4)
  ALP = TPAR(SPGRP,5)
  return
end

subroutine RAP_TAPER(SPGRP,AA1,AA2,A3,A4,ALP)
  implicit none
  ! Variables
  integer*4 :: SPGRP,ISPGRP
  real*4  :: AA1,AA2,A3,A4,ALP,TPAR(3,5)

  !  TAPER (5 parameters - conifer only)

  data TPAR/&
       &-0.55029801, -0.55029801, -0.596278066,& ! DF,WH,RC

       &-0.69479837, -0.69479837, -0.83987883 ,& ! DF,WH,RC

       &-6.13100423, -6.13100423, -6.85768402 ,& ! DF,WH,RC

       &0.35697451,  0.35697451,  0.134178717,& ! DF,WH,RC

       &0.5       ,  0.5       ,  0.71       / ! DF,WH,RC

  ISPGRP = SPGRP-1
  AA1 = TPAR(ISPGRP,1)
  AA2 = TPAR(ISPGRP,2)
  A3 = TPAR(ISPGRP,3)
  A4 = TPAR(ISPGRP,4)
  ALP = TPAR(ISPGRP,5)
  return
end

subroutine TAPER_RA(DBH,HT,CR,HI,DI)
  implicit none
  ! Variables
  real*4  :: DBH,HT,CR,HI,X,Z,P,A1,A2,A3,A4,A5,A6,A7,C,DI,D140
  A1 = 0.9113
  A2 = 1.0160
  A3 = 0.2623
  A4 = -18.7695
  A5 = 3.1931
  A6 = 0.1631
  A7 = 0.4180
  D140 = 0.000585+0.997212*DBH
  Z = HI/HT
  P = 4.5/HT
  X = (1.0-Z**0.5)/(1.0-P**0.5)
  C = A3*(1.364409*D140**0.3333333*EXP(A4*Z)&
       &+EXP(A5*CR**A6*(D140/HT)**A7*Z))
  DI = A1*D140**A2*X**C
  return
end

subroutine RA_MH(DBH,HT,CR,TD,MH)
  implicit none
  ! Variables
  integer*4 :: IHT,I
  real*4  :: DBH,HT,CR,TD,MH,D0,DI,HI
  if (TD .le.0.0) then
     MH = HT
     return
  end if
  call TAPER_RA(DBH,HT,CR,0.0,D0)
  if (D0.le. TD) then
     MH = 0.0
  else
     MH = 0.0
     IHT = INT(ANINT(10.0*HT))-1
     do I = 1,IHT
        HI = HT-0.1*FLOAT(I)
        call TAPER_RA(DBH,HT,CR,HI,DI)
        if (DI .ge. TD) then
           MH = HI
           exit
        end if
     end do
  end if
  return
end

subroutine RA_CFV(DBH,HT,CR,MH,SH,TD,CFV)
  implicit none
  ! Variables
  integer*4 :: NSEC,NSECDIFF,NSEC2,I
  real*4  :: DBH,HT,CR,MH,SH,TD,FC,HI,D1,D2,D3,LOGV,CFV,DIFF,REMAIN,LOGL
  FC = 0.005454154
  if (TD .le.0.0) then
     MH = HT
  else
     call RA_MH(DBH,HT,CR,TD,MH)
  end if
  if (MH .eq.0.0.or. MH .le. SH) then
     CFV = 0.0
     return
  end if
  DIFF = MH-SH
  REMAIN = MOD(DIFF,1.0)
  NSEC = INT(ANINT(DIFF-REMAIN))
  if (TD .le.0.0.and. DIFF .le.0.0) then
     NSEC = NSEC-1
  end if
  NSECDIFF = MOD(NSEC,2)
  NSEC2 = (NSEC-NSECDIFF)/2
  CFV = 0.0
  HI = SH
  call TAPER_RA(DBH,HT,CR,HI,D1)
  do I = 1,NSEC2
     HI = HI+1.0
     call TAPER_RA(DBH,HT,CR,HI,D2)
     HI = HI+1.0
     call TAPER_RA(DBH,HT,CR,HI,D3)
     LOGL = 2.0
     LOGV = LOGL*FC*((D1**2+4.0*D2**2+D3**2)/6.0)
     CFV = CFV+LOGV
     D1 = D3
  end do
  if (NSECDIFF .gt.0) then
     HI = HI+1.0
     call TAPER_RA(DBH,HT,CR,HI,D2)
     LOGL = 1.0
     LOGV = LOGL*FC*((D1**2+D2**2)/2.0)
     CFV = CFV+LOGV
     D1 = D2
  end if
  if (TD .le.0.0) then
     if (REMAIN .le.0.0) then
        LOGL = 1.0
     else
        LOGL = REMAIN
     end if
     LOGV = LOGL*FC*(D1**2)/3.0
     CFV = CFV+LOGV
  else
     if (REMAIN .gt.0.0) then
        call TAPER_RA(DBH,HT,CR,MH,D2)
        LOGL = REMAIN
        LOGV = LOGL*FC*((D1**2+D2**2)/2.0)
        CFV = CFV+LOGV
     end if
  end if
  return
end

subroutine RA_SCRIB(SVOL,LOGLL,LOGTD,LOGSH,LOGTA,LOGML,DOB,HT,CR,&
     &VALU)

  !     VALUE = SCRIBNER VOLUME CALCULATED

  implicit none
  ! Variables
  integer*4 :: SVOL,LOGLL
  integer*4 :: NW,I,J,II
  real*4  :: LOGTD,LOGSH,LOGTA,LOGML,DOB,HT,CR,VALU
  real*4  :: D,TLL,EX,H,DI,VOLG,NL(40,4),LVOL(40,4),TOTS(2,4),MH

  D = 0.0
  TLL = 0.0
  VALU = 0.0
  do I = 1,2
     do J = 1,4
        TOTS(I,J) = 0.
     end do
  end do
  EX = 1.0

  !     COMPUTE MERCHANTABLE HEIGHT

  if (LOGTD .le.0.0) then
     MH = HT
  else
     call RA_MH(DOB,HT,CR,LOGTD,MH)
  end if
  if (MH .eq.0.0.or. MH .le. LOGSH) then
     VALU = 0.0
     return
  end if

  !     CALCULATE LOG VOLUMES

  NW = INT((MH-LOGSH)/(FLOAT(LOGLL)+LOGTA))
  if (NW .lt.0) NW = 0
  TLL = MH-LOGSH-FLOAT(NW)*(FLOAT(LOGLL)+LOGTA)
  D = 1.0

  H = LOGSH
  do II = 1,NW
     H = H+FLOAT(LOGLL)+LOGTA
     call RA_LOGVOL(3,DOB,HT,CR,SVOL,LOGLL,H,D,TLL,EX,DI,VOLG,NL,&
          &LVOL,TOTS)
     !         CALL RA_LOGVOL(3,DOB,HT,LOGLL,H,D,TLL,EX,DI,VOLG,NL,LVOL,TOTS)
     VALU = VALU+VOLG
  end do

  !     COMPUTE VOLUME OF TOP LOG

  if (TLL .ge. (LOGML+LOGTA)) then
     J = INT(TLL-LOGTA)
     TLL = FLOAT(J)+LOGTA
     D = TLL/FLOAT(LOGLL)
     H = H+TLL
     call RA_LOGVOL(3,DOB,HT,CR,SVOL,LOGLL,H,D,TLL,EX,DI,VOLG,NL,&
          &LVOL,TOTS)
     !         CALL RA_LOGVOL(3,DOB,HT,LOGLL,H,D,TLL,EX,DI,VOLG,NL,LVOL,TOTS)
     VALU = VALU+VOLG
  end if
  if (VALU .lt.0.)VALU = 0.
  return
end

subroutine RA_LOGVOL(N,DBH,HT,CR,SVOL,LOGLL,HI,D,TLL,EX,DI,V,NL,&
     &LVOL,TOTS)
  !      SUBROUTINE RA_LOGVOL(N,DBH,HT,LOGLL,HI,D,TLL,EX,DI,V,NL,LVOL,TOTS)
  implicit none
  !     ROUTINE TO CALCULATE LOG VOLUME AND ADD TO APPROPRIATE CELL

  !     N = TYPE OF CALCULATION
  !       = 1  MERCHANTABLE HEIGHT
  !       = 2  LOG VOLUME
  !       = 3  TREE VOLUME
  !       = 4  TOP DIAMETER CHECK

  !     SVOL = SPECIES GROUP FOR LOG REPORT

  !     EX = TREE RESIDUAL OR CUT EXPANSION FACTOR

  !     D = RATIO OF LOG LENGTH TO SPECIFIED LOG LENGTH

  ! Variables
  integer*4 :: N,SVOL,LOGLL
  integer*4 :: DII,LEN
  real*4  :: DBH,HT,CR,HI,D,TLL,EX,DI,V,NL(40,4),LVOL(40,4),TOTS(2,4)
  real*4  :: SVTBL(80),SVTBL16(6),SVTBL32(6)
  data SVTBL/0., .143, .39, .676, 1.07, 1.160, 1.4, 1.501, 2.084,&
       &3.126, 3.749, 4.9, 6.043, 7.14, 8.88, 10., 11.528,&
       &13.29, 14.99, 17.499, 18.99, 20.88, 23.51, 25.218,&
       &28.677, 31.249, 34.22, 36.376, 38.04, 41.06, 44.376,&
       &45.975, 48.99, 50., 54.688, 57.66, 64.319, 66.73, 70.,&
       &75.24, 79.48, 83.91, 87.19, 92.501, 94.99, 99.075,&
       &103.501, 107.97, 112.292, 116.99, 121.65, 126.525,&
       &131.51, 136.51, 141.61, 146.912, 152.21, 157.71,&
       &163.288, 168.99, 174.85, 180.749, 186.623, 193.17,&
       &199.12, 205.685, 211.81, 218.501, 225.685, 232.499,&
       &239.317, 246.615, 254.04, 261.525, 269.04, 276.63,&
       &284.26, 292.501, 300.655,308.97/
  data SVTBL16/1.249,1.608,1.854,2.410,3.542,4.167/
  data SVTBL32/1.57,1.8,2.2,2.9,3.815,4.499/

  !     USE TAPER EQUATION TO DETERMINE DIAMETER AT TOP OF LOG

  V = 0.0
  call TAPER_RA(DBH,HT,CR,HI,DI)

  if (N.eq.1.or. N.eq.4)return

  !     EXTRACT VOLUME FROM VOLUME TABLES

  if (D .lt.1.0.or. D .gt.1.0) then
     LEN = INT(TLL)
  else
     LEN = LOGLL
  end if
  DII = INT(DI)
  if (DII .ge.6.and. DII .le.11) then
     if (LEN .ge.16.and. LEN .le.31) then
        V = (SVTBL16(DII-5)*FLOAT(LEN))*EX
     else if (LEN .ge.32.and. LEN .le.40) then
        V = (SVTBL32(DII-5)*FLOAT(LEN))*EX
     else
        V = (SVTBL(DII)*FLOAT(LEN))*EX
     end if
  else
     V = (SVTBL(DII)*FLOAT(LEN))*EX
  end if
  if (N .eq.3) return
  DII = DII/2
  NL(DII,SVOL) = NL(DII,SVOL)+EX
  LVOL(DII,SVOL) = LVOL(DII,SVOL)+V
  TOTS(2,SVOL) = TOTS(2,SVOL)+V
  return
end
