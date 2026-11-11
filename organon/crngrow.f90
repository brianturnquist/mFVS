!=======================!
!   organon : crngrow   !
!=======================!
! ORGANON $Id$

  !     ORGANON GROWTH AND YIELD MODEL
  !     SUBROUTINES INCLUDED:
  !         CROWGRO
  !         HCB_SWO
  !         HCB_NWO
  !         HCB_SMC
  !         HCB_RAP
  !         MCW_SWO
  !         MCW_NWO
  !         MCW_SMC
  !         MCW_RAP
  !         LCW_SWO
  !         LCW_NWO
  !         LCW_SMC
  !         LCW_RAP
  !         HLCW_SWO
  !         HLCW_NWO
  !         HLCW_SMC
  !         HLCW_RAP
  !         CALC_CC
  !         CW_SWO
  !         CW_NWO
  !         CW_SMC
  !         CW_RAP
  !         CRNCLO
  !         GET_CCFL
  !         MAXHCB_SWO
  !         MAXHCB_NWO
  !         MAXHCB_SMC
  !         MAXHCB_RAP

subroutine CROWGRO(VERSION,CYCLG,NTREES,IB,TDATAI,TDATAR,&
     &SCR,GROWTH,MGEXP,DEADEXP,CCFLL1,CCFL1,CCFLL2,&
     &CCFL2,SBA1,SBA2,SI_1,SI_2,CALIB,CCH)
  !     DETERMINE 5-YR CROWN RECESSION

  implicit none
  ! Variables
  integer*4 :: VERSION,CYCLG,NTREES,IB,TDATAI(2000,3),I,ISPGRP,J
  real*4  :: TDATAR(2000,8),SCR(2000,3),GROWTH(2000,4),MGEXP(2000),DEADEXP(2000),CCFLL1(51),&
       &CCFL1(500),CCFLL2(51),CCFL2(500),SBA1,SBA2,SI_1,SI_2,CALIB(6,18),CCH(41),OG1,OG2,PHT,PDBH,SCCFL1,&
       &HCB1,PCR1,PHCB1,HT,DBH,SCCFL2,HCB2,PCR2,PHCB2,HCBG,AHCB1,SHCB1,AHCB2,SHCB2,MAXHCB,CC2
  call OLDGRO(NTREES,IB,TDATAI,TDATAR,GROWTH,DEADEXP,-1.0,OG1)
  call OLDGRO(NTREES,IB,TDATAI,TDATAR,GROWTH,DEADEXP,0.0,OG2)
  do I = 1,NTREES
     if (TDATAI(I,1).eq.0)cycle
     if (CYCLG .eq.0)TDATAR(I,6) = TDATAR(I,3)

     !        CALCULATE HCB START OF GROWTH

     !        CALCULATE STARTING HEIGHT

     PHT = TDATAR(I,2)-GROWTH(I,1)

     !        CALCULATE STARTING DBH

     PDBH = TDATAR(I,1)-GROWTH(I,2)
     ISPGRP = TDATAI(I,2)
     call GET_CCFL(PDBH,CCFLL1,CCFL1,SCCFL1)
     select case(VERSION)
     case(1)
        call HCB_SWO(ISPGRP,PHT,PDBH,SCCFL1,SBA1,SI_1,SI_2,OG1,&
             &HCB1)
        PCR1 = 1.0-HCB1/PHT
     case(2)
        call HCB_NWO(ISPGRP,PHT,PDBH,SCCFL1,SBA1,SI_1,SI_2,OG1,&
             &HCB1)
        PCR1 = CALIB(2,ISPGRP)*(1.0-HCB1/PHT)
     case(3)
        call HCB_SMC(ISPGRP,PHT,PDBH,SCCFL1,SBA1,SI_1,SI_2,OG1,&
             &HCB1)
        PCR1 = 1.0-HCB1/PHT
     case(4)
        call HCB_RAP(ISPGRP,PHT,PDBH,SCCFL1,SBA1,SI_1,SI_2,OG1,&
             &HCB1)
        PCR1 = 1.0-HCB1/PHT
     end select
     PHCB1 = (1.0-PCR1)*PHT

     !        CALCULATE HCB END OF GROWTH

     HT = TDATAR(I,2)
     DBH = TDATAR(I,1)
     call GET_CCFL(DBH,CCFLL2,CCFL2,SCCFL2)
     select case(VERSION)
     case(1)
        call HCB_SWO(ISPGRP,HT,DBH,SCCFL2,SBA2,SI_1,SI_2,OG2,&
             &HCB2)
        call MAXHCB_SWO(ISPGRP,HT,SCCFL2,MAXHCB)
        PCR2 = 1.0-HCB2/HT
     case(2)
        call HCB_NWO(ISPGRP,HT,DBH,SCCFL2,SBA2,SI_1,SI_2,OG2,&
             &HCB2)
        call MAXHCB_NWO(ISPGRP,HT,SCCFL2,MAXHCB)
        PCR2 = CALIB(2,ISPGRP)*(1.0-HCB2/HT)
     case(3)
        call HCB_SMC(ISPGRP,HT,DBH,SCCFL2,SBA2,SI_1,SI_2,OG2,&
             &HCB2)
        call MAXHCB_SMC(ISPGRP,HT,SCCFL2,MAXHCB)
        PCR2 = 1.0-HCB2/HT
     case(4)
        call HCB_RAP(ISPGRP,HT,DBH,SCCFL2,SBA2,SI_1,SI_2,OG2,&
             &HCB2)
        call MAXHCB_RAP(ISPGRP,HT,SCCFL2,MAXHCB)
        PCR2 = 1.0-HCB2/HT
     end select
     PHCB2 = (1.0-PCR2)*HT

     !        DETERMINE CROWN GROWTH

     HCBG = PHCB2-PHCB1
     if (HCBG.lt.0.0) then
        HCBG = 0.0
     end if
     AHCB1 = (1.0-TDATAR(I,3))*PHT
     SHCB1 = (1.0-SCR(I,1))*PHT
     AHCB2 = AHCB1+HCBG
     SHCB2 = SHCB1+HCBG
     if (AHCB1.gt. SHCB1) then
        if (AHCB1.gt. SHCB2) then
           if (SHCB2.ge. MAXHCB) then
              SCR(I,1) = 1.0-MAXHCB/HT
              TDATAR(I,3) = 1.0-AHCB1/HT
           else
              SCR(I,1) = 1.0-SHCB2/HT
              TDATAR(I,3) = 1.0-AHCB1/HT
           end if
        else
           TDATAR(I,3) = 1.0-SHCB2/HT
           do J = 1,3
              SCR(I,J) = 0.0
           end do
        end if
     else
        if (AHCB1.ge. MAXHCB) then
           TDATAR(I,3) = 1.0-AHCB1/HT
        else if (AHCB2.ge. MAXHCB) then
           TDATAR(I,3) = 1.0-MAXHCB/HT
        else
           TDATAR(I,3) = 1.0-AHCB2/HT
        end if
     end if
  end do
  call CRNCLO(0,0.0,VERSION,NTREES,TDATAI,TDATAR,SCR,MGEXP,&
       &CCH,CC2)
  return
end

subroutine HCB_SWO(ISPGRP,HT,DBH,CCFL,BA,SI_1,SI_2,OG,HCB)
  implicit none
  ! Variables
  integer*4 :: ISPGRP
  real*4  :: HT,DBH,CCFL,BA,SI_1,SI_2,OG,HCB,HCBPAR(18,7),B0,B1,B2,B3,B4,B5,B6

  !  NEW HEIGHT TO CROWN BASE FOR UNDAMAGED TREES ONLY
  !     (7 parameters - all species)

  !     DF Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
  !     GW Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
  !     PP Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
  !     SP Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
  !     IC Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
  !     WH Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
  !     RC Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #2
  !     PY Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
  !     MD Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
  !     GC Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
  !     TA Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
  !     CL Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
  !     BL Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
  !     WO Coefficients from Gould, Marshall, and Harrington (2008) West. J. Appl. For. 23: 26-33
  !     BO Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
  !     RA Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #1
  !     PD Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
  !     WI Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29

  data HCBPAR/&
       &1.797136911,  3.451045887,  1.656364063,  3.785155749,& !  DF,GW,PP,SP
       &2.428285297,  0.0        ,  4.49102006 ,  0.0        ,& !  IC,WH,RC,PY
       &2.955339267,  0.544237656,  0.833006499,  0.5376600543,& !  MD,GC,TA,CL
       &0.9411395642, 1.05786632 ,  2.60140655 ,  0.56713781  ,& !  BL,WO,BO,RA
       &0.0         , 0.0        ,& !  PD,WI

       &-0.010188791, -0.005985239, -0.002755463, -0.009012547,& !  DF,GW,PP,SP
       &-0.006882851,  0.0        ,  0.0        ,  0.0        ,& !  IC,WH,RC,PY
       &0.0        , -0.020571754, -0.012984204, -0.018632397,& !  MD,GC,TA,CL
       &-0.00768402 ,  0.0        ,  0.0        , -0.010377976,& !  BL,WO,BO,RA
       &0.0        ,  0.0        ,& !  PD,WI

       &-0.003346230, -0.003211194,  0.0        , -0.003318574,& !  DF,GW,PP,SP
       &-0.002612590,  0.0        , -0.00132412 ,  0.0        ,& !  IC,WH,RC,PY
       &0.0        , -0.004317523, -0.002704717,  0.0        ,& !  MD,GC,TA,CL
       &-0.005476131, -0.00183283 , -0.002273616, -0.002066036,& !  BL,WO,BO,RA
       &-0.005666559, -0.005666559,& !  PD,WI

       &-0.412217810, -0.671479750, -0.568302547, -0.670270058,& !  DF,GW,PP,SP
       &-0.572782216,  0.0        , -1.01460531 ,  0.0        ,& !  IC,WH,RC,PY
       &-0.798610738,  0.0        ,  0.0        ,  0.0        ,& !  MD,GC,TA,CL
       &0.0        , -0.28644547 , -0.554980629,  0.0        ,& !  BL,WO,BO,RA
       &-0.745540494, -0.745540494,& !  PD,WI

       &3.958656001,  3.931095518,  6.730693919,  2.758645081,& !  DF,GW,PP,SP
       &2.113378338,  4.801329946,  0.0        ,  2.030940382,& !  IC,WH,RC,PY
       &3.095269471,  3.132713612,  0.0        ,  0.0        ,& !  MD,GC,TA,CL
       &0.0        ,  0.0        ,  0.0        ,  1.39796223 ,& !  BL,WO,BO,RA
       &0.0        ,  0.0        ,& !  PD,WI

       &0.008526562,  0.003115567,  0.001852526,  0.0        ,& !  DF,GW,PP,SP
       &0.008480754,  0.0        ,  0.01340624 ,  0.0        ,& !  IC,WH,RC,PY
       &0.0        ,  0.0        ,  0.0        ,  0.0        ,& !  MD,GC,TA,CL
       &0.0        ,  0.0        ,  0.0        ,  0.0        ,& !  BL,WO,BO,RA
       &0.038476613,  0.038476613,& !  PD,WI

       &0.448909636,  0.516180892,  0.0        ,  0.841525071,& !  DF,GW,PP,SP
       &0.506226895,  0.0        ,  0.0        ,  0.0        ,& !  IC,WH,RC,PY
       &0.700465646,  0.483748898,  0.2491242765, 0.0        ,& !  MD,GC,TA,CL
       &0.0        ,  0.0        ,  0.0        ,  0.0        ,& !  BL,WO,BO,RA
       &0.0        ,  0.0/ !  PD,WI

  B0 = HCBPAR(ISPGRP,1)
  B1 = HCBPAR(ISPGRP,2)
  B2 = HCBPAR(ISPGRP,3)
  B3 = HCBPAR(ISPGRP,4)
  B4 = HCBPAR(ISPGRP,5)
  B5 = HCBPAR(ISPGRP,6)
  B6 = HCBPAR(ISPGRP,7)
  if (ISPGRP .eq.3) then
     HCB = HT/(1.0+EXP(B0+B1*HT+B2*CCFL+B3*ALOG(BA)+B4*(DBH/HT)&
          &+B5*SI_2+B6*OG**2))
  else
     HCB = HT/(1.0+EXP(B0+B1*HT+B2*CCFL+B3*ALOG(BA)+B4*(DBH/HT)&
          &+B5*SI_1+B6*OG**2))
  end if
  return
end

subroutine HCB_NWO(ISPGRP,HT,DBH,CCFL,BA,SI_1,SI_2,OG,HCB)
  implicit none
  ! Variables
  integer*4 :: ISPGRP
  real*4  :: HT,DBH,CCFL,BA,SI_1,SI_2,OG,HCB,HCBPAR(11,7),B0,B1,B2,B3,B4,B5,B6

  !  HEIGHT TO CROWN BASE (7 parameters - all species)

  !     DF Coefficients from Zumrawi and Hann (1989) FRL Research Paper 52
  !     GF Coefficients from Zumrawi and Hann (1989) FRL Research Paper 52
  !     WH Coefficients from Johnson (2002) Willamette Industries Report
  !     RC Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #2
  !     PY Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
  !     MD Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
  !     BL Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
  !     WO Coefficients from Gould, Marshall, and Harrington (2008) West. J. Appl. For. 23: 26-33
  !     RA Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #1
  !     PD Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
  !     WI Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29

  data HCBPAR/&
       &1.94093    ,  1.04746    ,  1.92682    ,  4.49102006 ,& !  DF,GF,WH,RC
       &0.0        ,  2.955339267,  0.9411395642, 1.05786632 ,& !  PY,MD,BL,WO
       &0.56713781 ,  0.0        ,  0.0        ,& !  RA,PD,WI

       &-0.0065029  , -0.0066643  , -0.00280478 ,  0.0        ,& !  DF,GF,WH,RC
       &0.0        ,  0.0        , -0.00768402 ,  0.0        ,& !  PY,MD,BL,WO
       &-0.010377976,  0.0        ,  0.0        ,& !  RA,PD,WI

       &-0.0048737  , -0.0067129  , -0.0011939  , -0.00132412 ,& !  DF,GF,WH,RC
       &0.0        ,  0.0        , -0.005476131, -0.00183283 ,& !  PY,MD,BL,WO
       &-0.002066036, -0.005666559, -0.005666559,& !  RA,PD,WI

       &-0.261573   ,  0.0        , -0.513134   , -1.01460531 ,& !  DF,GF,WH,RC
       &0.0        , -0.798610738,  0.0        , -0.28644547 ,& !  PY,MD,BL,WO
       &0.0        , -0.745540494, -0.745540494,& !  RA,PD,WI

       &1.08785    ,  0.0        ,  3.68901    ,  0.0        ,& !  DF,GF,WH,RC
       &2.030940382,  3.095269471,  0.0        ,  0.0        ,& !  PY,MD,BL,WO
       &1.39796223 ,  0.0        ,  0.0        ,& !  RA,PD,WI

       &0.0        ,  0.0        ,  0.00742219 ,  0.01340624 ,& !  DF,GF,WH,RC
       &0.0        ,  0.0        ,  0.0        ,  0.0        ,& !  PY,MD,BL,WO
       &0.0        ,  0.038476613,  0.038476613,& !  RA,PD,WI

       &0.0        ,  0.0        ,  0.0        ,  0.0        ,& !  DF,GF,WH,RC
       &0.0        ,  0.700465646,  0.0        ,  0.0        ,& !  PY,MD,BL,WO
       &0.0        ,  0.0        ,  0.0/ !  RA,PD,WI

  B0 = HCBPAR(ISPGRP,1)
  B1 = HCBPAR(ISPGRP,2)
  B2 = HCBPAR(ISPGRP,3)
  B3 = HCBPAR(ISPGRP,4)
  B4 = HCBPAR(ISPGRP,5)
  B5 = HCBPAR(ISPGRP,6)
  B6 = HCBPAR(ISPGRP,7)
  if (ISPGRP .eq.3) then
     HCB = HT/(1.0+EXP(B0+B1*HT+B2*CCFL+B3*ALOG(BA)+B4*(DBH/HT)&
          &+B5*SI_2+B6*OG**2))
  else
     HCB = HT/(1.0+EXP(B0+B1*HT+B2*CCFL+B3*ALOG(BA)+B4*(DBH/HT)&
          &+B5*SI_1+B6*OG**2))
  end if
  return
end

subroutine HCB_SMC(ISPGRP,HT,DBH,CCFL,BA,SI_1,SI_2,OG,HCB)
  implicit none
  ! Variables
  integer*4 :: ISPGRP
  real*4  :: HT,DBH,CCFL,BA,SI_1,SI_2,OG,HCB,HCBPAR(11,7),B0,B1,B2,B3,B4,B5,B6

  !  HEIGHT TO CROWN BASE (7 parameters - all species)

  !     DF Coefficients from Hann and Hanus (2004) FS 34: 1193-2003
  !     GF Coefficients from Zumrawi and Hann (1989) FRL Research Paper 52
  !     WH Coefficients from Johnson (2002) Willamette Industries Report
  !     RC Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #2
  !     PY Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
  !     MD Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
  !     BL Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
  !     WO Coefficients from Gould, Marshall, and Harrington (2008) West. J. Appl. For. 23: 26-33
  !     RA Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #1
  !     PD Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
  !     WI Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29

  data HCBPAR/& !
       &6.18464679 ,  1.04746    ,  1.92682    ,  4.49102006 ,& !  DF,GF,WH,RC
       &0.0        ,  2.955339267,  0.9411395642, 1.05786632 ,& !  PY,MD,BL,WO
       &0.56713781 ,  0.0        ,  0.0        ,& !  RA,PD,WI

       &-0.00328764 , -0.0066643  , -0.00280478 ,  0.0        ,& !  DF,GF,WH,RC
       &0.0        ,  0.0        , -0.00768402 ,  0.0        ,& !  PY,MD,BL,WO
       &-0.010377976,  0.0        ,  0.0        ,& !  RA,PD,WI

       &-0.00136555 , -0.0067129  , -0.0011939  , -0.00132412 ,& !  DF,GF,WH,RC
       &0.0        ,  0.0        , -0.005476131, -0.00183283 ,& !  PY,MD,BL,WO
       &-0.002066036, -0.005666559, -0.005666559,& !  RA,PD,WI

       &-1.19702220 ,  0.0        , -0.513134   , -1.01460531 ,& !  DF,GF,WH,RC
       &0.0        , -0.798610738,  0.0        , -0.28644547 ,& !  PY,MD,BL,WO
       &0.0        , -0.745540494, -0.745540494,& !  RA,PD,WI

       &3.17028263 ,  0.0        ,  3.68901    ,  0.0        ,& !  DF,GF,WH,RC
       &2.030940382,  3.095269471,  0.0        ,  0.0        ,& !  PY,MD,BL,WO
       &1.39796223 ,  0.0        ,  0.0        ,& !  RA,PD,WI

       &0.0        ,  0.0        ,  0.00742219 ,  0.01340624 ,& !  DF,GF,WH,RC
       &0.0        ,  0.0        ,  0.0        ,  0.0        ,& !  PY,MD,BL,WO
       &0.0        ,  0.038476613,  0.038476613,& !  RA,PD,WI

       &0.0        ,  0.0        ,  0.0        ,  0.0        ,& !  DF,GF,WH,RC
       &0.0        ,  0.700465646,  0.0        ,  0.0        ,& !  PY,MD,BL,WO
       &0.0        ,  0.0        ,  0.0/ !  RA,PD,WI

  B0 = HCBPAR(ISPGRP,1)
  B1 = HCBPAR(ISPGRP,2)
  B2 = HCBPAR(ISPGRP,3)
  B3 = HCBPAR(ISPGRP,4)
  B4 = HCBPAR(ISPGRP,5)
  B5 = HCBPAR(ISPGRP,6)
  B6 = HCBPAR(ISPGRP,7)
  if (ISPGRP .eq.3) then
     HCB = HT/(1.0+EXP(B0+B1*HT+B2*CCFL+B3*ALOG(BA)+B4*(DBH/HT)&
          &+B5*SI_2+B6*OG**2))
  else
     HCB = HT/(1.0+EXP(B0+B1*HT+B2*CCFL+B3*ALOG(BA)+B4*(DBH/HT)&
          &+B5*SI_1+B6*OG**2))
  end if
  return
end

subroutine HCB_RAP(ISPGRP,HT,DBH,CCFL,BA,SI_1,SI_2,OG,HCB)
  implicit none
  ! Variables
  integer*4 :: ISPGRP
  real*4  :: HT,DBH,CCFL,BA,SI_1,SI_2,OG,HCB,HCBPAR(7,8),B0,B1,B2,B3,B4,B5,B6,K,SITE

  !  HEIGHT TO CROWN BASE (7 parameters - all species)

  !     RA Coefficients from Hann, Bluhm, and Hibbs Red Alder Plantation Analysis
  !     DF Coefficients from Hann and Hanus (2004) FS 34: 1193-2003
  !     WH Coefficients from Johnson (2002) Willamette Industries Report
  !     RC Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #2
  !     BL Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
  !     PD Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
  !     WI Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29

  data HCBPAR/& !
       &3.73113020  ,  6.18464679 ,  1.92682    ,  4.49102006 ,& !  RA,DF,WH,RC
       &0.9411395642,  0.0        ,  0.0        ,& !  BL,PD,WI

       &-0.021546486 , -0.00328764 , -0.00280478 ,  0.0        ,& !  RA,DF,WH,RC
       &-0.00768402  ,  0.0        ,  0.0        ,& !  BM,PD,WI

       &-0.0016572840, -0.00136555 , -0.0011939  , -0.00132412 ,& !  RA,DF,WH,RC
       &-0.005476131 , -0.005666559, -0.005666559,& !  BL,PD,WI

       &-1.0649544   , -1.19702220 , -0.513134   , -1.01460531 ,& !  RA,DF.WH,RC
       &0.0         , -0.745540494, -0.745540494,& !  BL,PD,WI

       &7.47699601  ,  3.17028263 ,  3.68901    ,  0.0        ,& !  RA,DF,WH,RC
       &0.0         ,  0.0        ,  0.0        ,& !  BL,PD,WI

       &0.0252953320,  0.0        ,  0.00742219 ,  0.01340624 ,& !  RA,DF,WH,RC
       &0.0        ,   0.038476613,  0.038476613,& !  BL,PD,WI

       &0.0        ,   0.0        ,  0.0        ,  0.0        ,& !  RA,DF,WH,RC
       &0.0        ,   0.0        ,  0.0        ,& !  BL,PD,WI

       &1.6        ,   0.0        ,  0.0        ,  0.0        ,& !  RA,DF,WH,RC
       &0.0        ,   0.0        ,  0.0/ !  BL,PD,WI

  B0 = HCBPAR(ISPGRP,1)
  B1 = HCBPAR(ISPGRP,2)
  B2 = HCBPAR(ISPGRP,3)
  B3 = HCBPAR(ISPGRP,4)
  B4 = HCBPAR(ISPGRP,5)
  B5 = HCBPAR(ISPGRP,6)
  B6 = HCBPAR(ISPGRP,7)
  K = HCBPAR(ISPGRP,8)
  if (ISPGRP .eq.1) then
     HCB = (HT-K)/(1.0+EXP(B0+B1*HT+B2*CCFL+B3*ALOG(BA)+B4*(DBH/HT)&
          &+B5*SI_1+B6*OG**2))+K
  else
     SITE = SI_2
     if (ISPGRP .eq.3) then
        SITE = 0.480 +( 1.110 * (SI_2+4.5))
     end if
     HCB = HT/(1.0+EXP(B0+B1*HT+B2*CCFL+B3*ALOG(BA)+B4*(DBH/HT)&
          &+B5*SITE+B6*OG**2))
  end if
  return
end

subroutine MCW_SWO(ISPGRP,D,H,MCW)
  !     DETERMINE MCW FOR EACH TREE

  !     MCW = MCW VALUE

  implicit none
  ! Variables
  integer*4 :: ISPGRP
  real*4  :: D,H,MCW,MCWPAR(18,4),B0,B1,B2,DBH,HT,PKDBH

  !  MAXIMUM CROWN WIDTH (4 parameters - all species)

  !     DF Coefficients from Paine and Hann (1982) FRL Research Paper 46
  !     GW Coefficients from Paine and Hann (1982) FRL Research Paper 46
  !     PP Coefficients from Paine and Hann (1982) FRL Research Paper 46
  !     SP Coefficients from Paine and Hann (1982) FRL Research Paper 46
  !     IC Coefficients from Paine and Hann (1982) FRL Research Paper 46
  !     WH Coefficients from Paine and Hann (1982) FRL Research Paper 46
  !     RC Coefficients from Smith (1966) Proc. 6th World Forestry Conference
  !     PY Coefficients from WH
  !     MD Coefficients from Paine and Hann (1982) FRL Research Paper 46
  !     GC Coefficients from Paine and Hann (1982) FRL Research Paper 46
  !     TA Coefficients from Paine and Hann (1982) FRL Research Paper 46
  !     CL Coefficients from TA
  !     BL Coefficients from Ek (1974) School of Natural Res., U. Wisc., Forestry Res. Notes.
  !     WO Coefficients from Paine and Hann (1982) FRL Research Paper 46
  !     BO Coefficients from Paine and Hann (1982) FRL Research Paper 46
  !     RA Coefficients from Smith (1966) Proc. 6th World Forestry Conference
  !     PD Coefficients from GC
  !     WI Coefficients from GC

  data MCWPAR/&
       &4.6366    ,   6.1880    ,   3.4835    ,   4.6600546 ,& ! DF,GW,PP,SP
       &3.2837    ,   4.5652    ,   4.0       ,   4.5652    ,& ! IC,WH,RC,PY
       &3.4298629 ,   2.9793895 ,   4.4443    ,   4.4443    ,& ! MD,GC,TA,CL
       &4.0953    ,   3.0785639 ,   3.3625    ,   8.0       ,& ! BL,WO,BO,RA
       &2.9793895 ,   2.9793895 ,& !  PD,WI

       &1.6078    ,   1.0069    ,   1.343     ,   1.0701859 ,& ! DF,GW,PP,SP
       &1.2031    ,   1.4147    ,   1.65      ,   1.4147    ,& ! IC,WH,RC,PY
       &1.3532302 ,   1.5512443 ,   1.7040    ,   1.7040    ,& ! MD,GC,TA,CL
       &2.3849    ,   1.9242211 ,   2.0303    ,   1.53      ,& ! BL,WO,BO,RA
       &1.5512443 ,   1.5512443 ,& !  PD,WI

       &-0.009625  ,   0.0       ,  -0.0082544 ,   0.0       ,& ! DF,GW,PP,SP
       &-0.0071858 ,   0.0       ,   0.0       ,   0.0       ,& ! IC,WH,RC,PY
       &0.0       ,  -0.01416129,  0.0        ,   0.0       ,& ! MD,GC,TA,CL
       &-0.011630  ,   0.0       ,  -0.0073307 ,   0.0       ,& ! BL,WO,BO,RA
       &-0.01416129,  -0.01416129,& !  PD,WI

       &88.52      , 999.99      ,  81.35      , 999.99      ,& ! DF,GW,PP,SP
       &83.71      , 999.99      , 999.99      , 999.99      ,& ! IC,WH,RC,PY
       &999.99      ,  54.77      , 999.99      , 999.99      ,& ! MD,GC,TA,CL
       &102.53      , 999.99      , 138.93      , 999.99      ,& ! BL,WO,BO,RA
       &54.77      ,  54.77/ !  PD,WI

  DBH = D
  HT = H
  B0 = MCWPAR(ISPGRP,1)
  B1 = MCWPAR(ISPGRP,2)
  B2 = MCWPAR(ISPGRP,3)
  PKDBH = MCWPAR(ISPGRP,4)
  if (DBH .gt. PKDBH) then
     DBH = PKDBH
  end if
  if (HT .lt.4.501) then
     MCW = HT/4.5*B0
  else
     MCW = B0+B1*DBH+B2*DBH**2
  end if
  return
end

subroutine MCW_NWO(ISPGRP,D,H,MCW)
  !     DETERMINE MCW FOR EACH TREE

  !     MCW = MCW VALUE

  implicit none
  ! Variables
  integer*4 :: ISPGRP
  real*4  :: D,H,MCW,MCWPAR(11,4),B0,B1,B2,DBH,HT,PKDBH

  !  MAXIMUM CROWN WIDTH (4 parameters - all species)

  !     DF Coefficients from Paine and Hann (1982) FRL Research Paper 46
  !     GF Coefficients from Paine and Hann (1982) FRL Research Paper 46
  !     WH Coefficients from Johnson (2002) Willamette Industries Report
  !     RC Coefficients from Smith (1966) Proc. 6th World Forestry Conference
  !     PY Coefficients from WH of Paine and Hann (1982) FRL Research Paper 46
  !     MD Coefficients from Paine and Hann (1982) FRL Research Paper 46
  !     BL Coefficients from Ek (1974) School of Natural Res., U. Wisc., Forestry Res. Notes.
  !     WO Coefficients from Paine and Hann (1982) FRL Research Paper 46
  !     RA Coefficients from Smith (1966) Proc. 6th World Forestry Conference
  !     PD Coefficients from GC of Paine and Hann (1982) FRL Research Paper 46
  !     WI Coefficients from GC of Paine and Hann (1982) FRL Research Paper 46

  data MCWPAR/&
       &4.6198    ,   6.1880    ,   4.3586    ,   4.0       ,& ! DF,GF,WH,RC
       &4.5652    ,   3.4298629 ,   4.0953    ,   3.0785639 ,& ! PY,MD,BL,WO
       &8.0       ,   2.9793895 ,   2.9793895 ,& !  RA,PD,WI

       &1.8426    ,   1.0069    ,   1.57458   ,   1.65      ,& ! DF,GF,WH,RC
       &1.4147    ,   1.3532302 ,   2.3849    ,   1.9242211 ,& ! PY,MD,BL,WO
       &1.53      ,   1.5512443 ,   1.5512443 ,& !  RA,PD,WI

       &-0.011311  ,   0.0       ,   0.0       ,   0.0       ,& ! DF,GF,WH,RC
       &0.0       ,   0.0       ,  -0.0102651 ,   0.0       ,& ! PY,MD,BL,WO
       &0.0       ,  -0.01416129,  -0.01416129,& !  RA,PD,WI

       &81.45      , 999.99      ,  76.70      , 999.99      ,& ! DF,GF,WH,RC
       &999.99      , 999.99      , 102.53      , 999.99      ,& ! PY,MD,BL,WO
       &999.99      ,  54.77      ,  54.77/ !  RA,PD,WI

  DBH = D
  HT = H
  B0 = MCWPAR(ISPGRP,1)
  B1 = MCWPAR(ISPGRP,2)
  B2 = MCWPAR(ISPGRP,3)
  PKDBH = MCWPAR(ISPGRP,4)
  if (DBH .gt. PKDBH) then
     DBH = PKDBH
  end if
  if (HT .lt.4.501) then
     MCW = HT/4.5*B0
  else
     MCW = B0+B1*DBH+B2*DBH**2
  end if
  return
end

subroutine MCW_SMC(ISPGRP,D,H,MCW)
  !     DETERMINE MCW FOR EACH TREE

  !     MCW = MCW VALUE

  implicit none
  ! Variables
  integer*4 :: ISPGRP
  real*4  :: D,H,MCW,MCWPAR(11,4),B0,B1,B2,DBH,HT,PKDBH

  !  MAXIMUM CROWN WIDTH (4 parameters - all species)

  !     DF Coefficients from Paine and Hann (1982) FRL Research Paper 46
  !     GF Coefficients from Paine and Hann (1982) FRL Research Paper 46
  !     WH Coefficients from Paine and Hann (1982) FRL Research Paper 46
  !     RC Coefficients from Smith (1966) Proc. 6th World Forestry Conference
  !     PY Coefficients from WH of Paine and Hann (1982) FRL Research Paper 46
  !     MD Coefficients from Paine and Hann (1982) FRL Research Paper 46
  !     BL Coefficients from Ek (1974) School of Natural Res., U. Wisc., Forestry Res. Notes.
  !     WO Coefficients from Paine and Hann (1982) FRL Research Paper 46
  !     RA Coefficients from Smith (1966) Proc. 6th World Forestry Conference
  !     PD Coefficients from GC of Paine and Hann (1982) FRL Research Paper 46
  !     WI Coefficients from GC of Paine and Hann (1982) FRL Research Paper 46

  data MCWPAR/&
       &4.6198    ,   6.1880    ,   4.5652    ,   4.0       ,& ! DF,GF,WH,RC
       &4.5652    ,   3.4298629 ,   4.0953    ,   3.0785639 ,& ! PY,MD,BL,WO
       &8.0       ,   2.9793895 ,   2.9793895 ,& ! RA,PD,WI

       &1.8426    ,   1.0069    ,   1.4147    ,   1.65      ,& ! DF,GF,WH,RC
       &1.4147    ,   1.3532302 ,   2.3849    ,   1.9242211 ,& ! PY,MD,BL,WO
       &1.53      ,   1.5512443 ,   1.5512443 ,& ! RA,PD,WI

       &-0.011311  ,   0.0       ,   0.0       ,   0.0       ,& ! DF,GF,WH,RC
       &0.0       ,   0.0       ,  -0.011630  ,   0.0       ,& ! PY,MD,BL,WO
       &0.0       ,  -0.01416129,  -0.01416129,& ! RA,PD,WI

       &81.45      , 999.99      , 999.99      , 999.99      ,& ! DF,GF,WH,RC
       &999.99      , 999.99      , 102.53      , 999.99      ,& ! PY,MD,BL,WO
       &999.99      ,  54.77      ,  54.77/ ! RA,PD,WI

  DBH = D
  HT = H
  B0 = MCWPAR(ISPGRP,1)
  B1 = MCWPAR(ISPGRP,2)
  B2 = MCWPAR(ISPGRP,3)
  PKDBH = MCWPAR(ISPGRP,4)
  if (DBH .gt. PKDBH) then
     DBH = PKDBH
  end if
  if (HT .lt.4.501) then
     MCW = HT/4.5*B0
  else
     MCW = B0+B1*DBH+B2*DBH**2
  end if
  return
end

subroutine MCW_RAP(ISPGRP,D,H,MCW)
  !     DETERMINE MCW FOR EACH TREE

  !     MCW = MCW VALUE

  implicit none
  ! Variables
  integer*4 :: ISPGRP
  real*4  :: D,H,MCW,MCWPAR(7,5),B0,B1,B2,K,DBH,HT,PKDBH

  !  MAXIMUM CROWN WIDTH (4 parameters - all species)

  !     RA Coefficients from Hann, Bluhm, and Hibbs Red Alder Plantation Analysis
  !     DF Coefficients from Paine and Hann (1982) FRL Research Paper 46
  !     WH Coefficients from Paine and Hann (1982) FRL Research Paper 46
  !     RC Coefficients from Smith (1966) Proc. 6th World Forestry Conference
  !     BL Coefficients from Ek (1974) School of Natural Res., U. Wisc., Forestry Res. Notes.
  !     PD Coefficients from GC of Paine and Hann (1982) FRL Research Paper 46
  !     WI Coefficients from GC of Paine and Hann (1982) FRL Research Paper 46

  data MCWPAR/&
       &2.320746348,   4.6198    ,   4.5652    ,   4.0       ,& ! RA,DF,WH,RC
       &4.0953     ,   2.9793895 ,   2.9793895 ,& ! BL,PD,WI

       &6.661401926,   1.8426    ,   1.4147    ,   1.65      ,& ! RA,DF,WH,RC
       &2.3849     ,   1.5512443 ,   1.5512443 ,& ! BL,PD,WI

       &0.0        ,  -0.011311  ,   0.0       ,   0.0       ,& ! RA,DF,WH,RC
       &-0.011630   ,  -0.01416129,  -0.01416129,& ! BL,PD,WI

       &0.6        ,   1.0       ,   1.0       ,   1.0       ,& ! RA,DF,WH,RC
       &1.0        ,   1.0       ,   1.0       ,& ! BL,PD,WI

       &999.99       ,  81.45      , 999.99      , 999.99      ,& ! RA,DF,WH,RC
       &102.53       ,  54.77      ,  54.77/ ! BL,PD,WI

  DBH = D
  HT = H
  B0 = MCWPAR(ISPGRP,1)
  B1 = MCWPAR(ISPGRP,2)
  B2 = MCWPAR(ISPGRP,3)
  K = MCWPAR(ISPGRP,4)
  PKDBH = MCWPAR(ISPGRP,5)
  if (DBH .gt. PKDBH) then
     DBH = PKDBH
  end if
  if (HT .lt.4.501) then
     MCW = HT/4.5*B0
  else
     MCW = B0+B1*DBH**K+B2*DBH**2
  end if
  return
end

subroutine LCW_SWO(ISPGRP,MCW,CR,SCR,DBH,HT,LCW)
  implicit none
  ! Variables
  integer*4 :: ISPGRP
  real*4  :: MCW,CR,SCR,DBH,HT,CL,LCW,LCWPAR(18,3),B1,B2,B3

  !  LARGEST CROWN WIDTH (3 parameters - all species)

  !     DF Coefficients from Hann (1997) FRL Research Contribution 17
  !     GW Coefficients from Hann (1997) FRL Research Contribution 17
  !     PP Coefficients from Hann (1997) FRL Research Contribution 17
  !     SP Coefficients from Hann (1997) FRL Research Contribution 17
  !     IC Coefficients from Hann (1997) FRL Research Contribution 17
  !     WH Coefficients from Hann (1997) FRL Research Contribution 17
  !     RC Coefficients from IC
  !     PY Coefficients from WH
  !     MD Coefficients from Hann (1997) FRL Research Contribution 17
  !     GC Coefficients from Hann (1997) FRL Research Contribution 17
  !     TA Coefficients from Hann (1997) FRL Research Contribution 17
  !     CL Coefficients from Hann (1997) FRL Research Contribution 17
  !     BL Coefficients from Hann (1997) FRL Research Contribution 17
  !     WO Coefficients from Hann (1997) FRL Research Contribution 17
  !     BO Coefficients from Hann (1997) FRL Research Contribution 17
  !     RA Coefficients from Hann (1997) FRL Research Contribution 17
  !     PD Coefficients from GC
  !     WI Coefficients from GC

  data LCWPAR/&
       &0.0       ,  0.0       ,  0.355532  ,  0.0       ,& !  DF,GW,PP,SP
       &-0.251389  ,  0.0       , -0.251389  ,  0.0       ,& !  IC,WH,RC,PY
       &0.118621  ,  0.0       ,  0.0       ,  0.0       ,& !  MD,GC,TA,CL
       &0.0       ,  0.364811  ,  0.0       ,  0.3227140 ,& !  BL,WO,BO,RA
       &0.0       ,  0.0       ,& !  PD,WI

       &0.00371834,  0.00308402, 0.0        ,  0.00339675,& !  DF,GW,PP,SP
       &0.00692512,  0.0       , 0.00692512 ,  0.0       ,& !  IC,WH,RC,PY
       &0.00384872,  0.0       , 0.0111972  ,  0.0207676 ,& !  MD,GC,TA,CL
       &0.0       ,  0.0       , 0.0        ,  0.0       ,& !  BL,WO,BO,RA
       &0.0       ,  0.0       ,& !  PD,WI

       &0.808121  ,  0.0       , 0.0        ,  0.532418  ,& !  DF,GW,PP,SP
       &0.985922  ,  0.0       , 0.985922   ,  0.0       ,& !  IC,WH,RC,PY
       &0.0       ,  1.161440  , 0.0        ,  0.0       ,& !  MD,GC,TA,CL
       &1.47018   ,  0.0       , 1.27196    ,  0.0       ,& !  BL,WO,BO,RA
       &1.161440  ,  1.161440/ !  PD,WI

  B1 = LCWPAR(ISPGRP,1)
  B2 = LCWPAR(ISPGRP,2)
  B3 = LCWPAR(ISPGRP,3)
  if (SCR .gt. CR) then
     CL = SCR*HT
     LCW = MCW*SCR**(B1+B2*CL+B3*(DBH/HT))
  else
     CL = CR*HT
     LCW = MCW*CR**(B1+B2*CL+B3*(DBH/HT))
  end if
  return
end

subroutine LCW_NWO(ISPGRP,MCW,CR,SCR,DBH,HT,LCW)
  implicit none
  ! Variables
  integer*4 :: ISPGRP
  real*4  :: MCW,CR,SCR,DBH,HT,CL,LCW,LCWPAR(11,3),B1,B2,B3

  !  LARGEST CROWN WIDTH (3 parameters - all species)

  !     DF Coefficients from Hann (1997) FRL Research Contribution 17
  !     GF Coefficients from Hann (1997) FRL Research Contribution 17
  !     WH Coefficients from Johnson (2002) Willamette Industries Report
  !     RC Coefficients from IC of Hann (1997) FRL Research Contribution 17
  !     PY Coefficients from WH of Hann (1997) FRL Research Contribution 17
  !     MD Coefficients from Hann (1997) FRL Research Contribution 17
  !     BL Coefficients from Hann (1997) FRL Research Contribution 17
  !     WO Coefficients from Hann (1997) FRL Research Contribution 17
  !     RA Coefficients from Hann (1997) FRL Research Contribution 17
  !     PD Coefficients from GC of Hann (1997) FRL Research Contribution 17
  !     WI Coefficients from GC of Hann (1997) FRL Research Contribution 17

  data LCWPAR/&
       &0.0       ,  0.0       ,  0.105590  , -0.2513890 ,& !  DF,GF,WH,RC
       &0.0       ,  0.118621  ,  0.0       ,  0.3648110 ,& !  PY,MD,BL,WO
       &0.3227140 ,  0.0       ,  0.0       ,& !  RA,PD,WI

       &0.00436324,  0.00308402,  0.0035662 ,  0.006925120,& !  DF,GF,WH,RC
       &0.0       ,  0.00384872,  0.0       ,  0.0       ,& !  PY,MD,BL,WO
       &0.0       ,  0.0       ,  0.0       ,& !  RA,PD,WI

       &0.6020020 ,  0.0       ,  0.0       ,  0.985922   ,& !  DF,GF,WH,RC
       &0.0       ,  0.0       ,  1.470180  ,  0.0        ,& !  PY,MD,BL,WO
       &0.0       ,  1.61440   ,  1.61440/ !  RA,PD,WI

  B1 = LCWPAR(ISPGRP,1)
  B2 = LCWPAR(ISPGRP,2)
  B3 = LCWPAR(ISPGRP,3)
  if (SCR .gt. CR) then
     CL = SCR*HT
     LCW = MCW*SCR**(B1+B2*CL+B3*(DBH/HT))
  else
     CL = CR*HT
     LCW = MCW*CR**(B1+B2*CL+B3*(DBH/HT))
  end if
  return
end

subroutine LCW_SMC(ISPGRP,MCW,CR,SCR,DBH,HT,LCW)
  implicit none
  ! Variables
  integer*4 :: ISPGRP
  real*4  :: MCW,CR,SCR,DBH,HT,CL,LCW,LCWPAR(11,3),B1,B2,B3

  !  LARGEST CROWN WIDTH (3 parameters - all species)

  !     DF Coefficients from Hann (1997) FRL Research Contribution 17
  !     GF Coefficients from Hann (1997) FRL Research Contribution 17
  !     WH Coefficients from Hann (1997) FRL Research Contribution 17
  !     RC Coefficients from IC of Hann (1997) FRL Research Contribution 17
  !     PY Coefficients from WH of Hann (1997) FRL Research Contribution 17
  !     MD Coefficients from Hann (1997) FRL Research Contribution 17
  !     BL Coefficients from Hann (1997) FRL Research Contribution 17
  !     WO Coefficients from Hann (1997) FRL Research Contribution 17
  !     RA Coefficients from Hann (1997) FRL Research Contribution 17
  !     PD Coefficients from GC of Hann (1997) FRL Research Contribution 17
  !     WI Coefficients from GC of Hann (1997) FRL Research Contribution 17

  data LCWPAR/&
       &0.0       ,  0.0       ,  0.0       , -0.2513890 ,& !  DF,GF,WH,RC
       &0.0       ,  0.118621  ,  0.0       ,  0.3648110 ,& !  PY,MD,BL,WO
       &0.3227140 ,  0.0       ,  0.0       ,& !  RA,PD,WI

       &0.00436324,  0.00308402,  0.0       ,  0.006925120,& !  DF,GF,WH,RC
       &0.0       ,  0.00384872,  0.0       ,  0.0        ,& !  PY,MD,BL,WO
       &0.0       ,  0.0       ,  0.0       ,& !  RA,PD,WI

       &0.6020020 ,  0.0       ,  0.0       ,  0.985922   ,& !  DF,GF,WH,RC
       &0.0       ,  0.0       ,  1.470180  ,  0.0        ,& !  PY,MD,BL,WO
       &0.0       ,  1.61440   ,  1.61440/ !  RA,PD,WI

  B1 = LCWPAR(ISPGRP,1)
  B2 = LCWPAR(ISPGRP,2)
  B3 = LCWPAR(ISPGRP,3)
  if (SCR .gt. CR) then
     CL = SCR*HT
     LCW = MCW*SCR**(B1+B2*CL+B3*(DBH/HT))
  else
     CL = CR*HT
     LCW = MCW*CR**(B1+B2*CL+B3*(DBH/HT))
  end if
  return
end

subroutine LCW_RAP(ISPGRP,MCW,CR,SCR,DBH,HT,LCW)
  implicit none
  ! Variables
  integer*4 :: ISPGRP
  real*4  :: MCW,CR,SCR,DBH,HT,CL,LCW,LCWPAR(7,4),B0,B1,B2,B3

  !  LARGEST CROWN WIDTH (3 parameters - all species)

  !     RA Coefficients from Hann, Bluhm, and Hibbs Red Alder Plantation Analysis
  !     DF Coefficients from Hann (1997) FRL Research Contribution 17
  !     WH Coefficients from Hann (1997) FRL Research Contribution 17
  !     RC Coefficients from IC of Hann (1997) FRL Research Contribution 17
  !     BL Coefficients from Hann (1997) FRL Research Contribution 17
  !     PD Coefficients from GC of Hann (1997) FRL Research Contribution 17
  !     WI Coefficients from GC of Hann (1997) FRL Research Contribution 17

  data LCWPAR/&
       &0.78160725,  1.0       ,  1.0       ,  1.0        ,& !  RA,DF,WH,RC
       &1.0       ,  1.0       ,  1.0       ,& !  BL,PD,WI

       &0.44092737,  0.0       ,  0.0       , -0.2513890 ,& !  RA,DF,WH,RC
       &0.0       ,  0.0       ,  0.0       ,& !  BM,PD,WI

       &0.0       ,  0.00436324,  0.0       ,  0.006925120,& !  RA,DF,WH,RC
       &0.0       ,  0.0       ,  0.0       ,& !  BL,PD,WI

       &0.0       ,  0.6020020 ,  0.0       ,  0.985922   ,& !  RA,DF,WH,RC
       &1.470180  ,  1.61440   ,  1.61440/ !  BL,PD,WI

  B0 = LCWPAR(ISPGRP,1)
  B1 = LCWPAR(ISPGRP,2)
  B2 = LCWPAR(ISPGRP,3)
  B3 = LCWPAR(ISPGRP,4)
  if (SCR .gt. CR) then
     CL = SCR*HT
     LCW = B0*MCW*SCR**(B1+B2*CL+B3*(DBH/HT))
  else
     CL = CR*HT
     LCW = B0*MCW*CR**(B1+B2*CL+B3*(DBH/HT))
  end if
  return
end

subroutine HLCW_SWO(ISPGRP,HT,CR,SCR,HLCW)
  implicit none
  ! Variables
  integer*4 :: ISPGRP
  real*4  :: HT,CR,SCR,CL,HLCW,DACBPAR(18),B1

  !  DISTANCE ABOVE CROWN BASE TO LARGEST CROWN WIDTH (1 parameter - all species)

  !     DF Coefficients from Hann (1999) FS 45: 217-225
  !     GW Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     PP Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     SP Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     IC Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     WH Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     RC Coefficients from IC
  !     PY Coefficients from WH
  !     MD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     GC Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     TA Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     CL Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     BL Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     WO Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     BO Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     RA Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     PD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     WI Coefficients from Hann and Hanus (2001) FRL Research Contribution 34

  data DACBPAR/&
       &0.062000, 0.028454, 0.05    , 0.05    , 0.20    ,& !  DF,GW,PP,SP,IC,
       &0.209806, 0.20    , 0.209806, 0.0     , 0.0     ,& !  WH,RC,PY,MD,GC,
       &0.0     , 0.0     , 0.0     , 0.0     , 0.0     ,& !  TA,CL,BL,WO,BO,
       &0.0     , 0.0     , 0.0/ !  RA,PD,WI

  B1 = DACBPAR(ISPGRP)
  if (SCR .gt. CR) then
     CL = SCR*HT
  else
     CL = CR*HT
  end if
  HLCW = HT-(1.0-B1)*CL
  return
end

subroutine HLCW_NWO(ISPGRP,HT,CR,SCR,HLCW)
  implicit none
  ! Variables
  integer*4 :: ISPGRP
  real*4  :: HT,CR,SCR,CL,HLCW,DACBPAR(11),B1

  !  DISTANCE ABOVE CROWN BASE TO LARGEST CROWN WIDTH (1 parameter - all species)

  !     DF Coefficients from Hann (1999) FS 45: 217-225
  !     GF Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     WH Coefficients from Marshall, Johnson, and Hann (2003) CJFR 33: 2059-2066
  !     RC Coefficients from WH of Hann and Hanus (2001) FRL Research Contribution 34
  !     PY Coefficients from WH of Hann and Hanus (2001) FRL Research Contribution 34
  !     MD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     BL Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     WO Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     RA Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     PD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     WI Coefficients from Hann and Hanus (2001) FRL Research Contribution 34

  data DACBPAR/&
       &0.062000, 0.028454, 0.355270, 0.209806, 0.209806,& !  DF,GF,WH,RC,PY
       &0.0     , 0.0     , 0.0     , 0.0     , 0.0     ,& !  MD,BL,WO,RA,PD
       &0.0/ !  WI

  B1 = DACBPAR(ISPGRP)
  if (SCR .gt. CR) then
     CL = SCR*HT
  else
     CL = CR*HT
  end if
  HLCW = HT-(1.0-B1)*CL
  return
end

subroutine HLCW_SMC(ISPGRP,HT,CR,SCR,HLCW)
  implicit none
  ! Variables
  integer*4 :: ISPGRP
  real*4  :: HT,CR,SCR,CL,HLCW,DACBPAR(11),B1

  !  DISTANCE ABOVE CROWN BASE TO LARGEST CROWN WIDTH (1 parameter - all species)

  !     DF Coefficients from Hann (1999) FS 45: 217-225
  !     GF Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     WH Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     RC Coefficients from WH
  !     PY Coefficients from WH
  !     MD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     BL Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     WO Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     RA Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     PD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     WI Coefficients from Hann and Hanus (2001) FRL Research Contribution 34

  data DACBPAR/&
       &0.062000, 0.028454, 0.209806, 0.209806, 0.209806,& !  DF,GF,WH,RC,PY
       &0.0     , 0.0     , 0.0     , 0.0     , 0.0     ,& !  MD,BL,WO,RA,PD
       &0.0/ !  WI

  B1 = DACBPAR(ISPGRP)
  if (SCR .gt. CR) then
     CL = SCR*HT
  else
     CL = CR*HT
  end if
  HLCW = HT-(1.0-B1)*CL
  return
end

subroutine HLCW_RAP(ISPGRP,HT,CR,SCR,HLCW)
  implicit none
  ! Variables
  integer*4 :: ISPGRP
  real*4  :: HT,CR,SCR,CL,HLCW,DACBPAR(7,2),B1,B2

  !  DISTANCE ABOVE CROWN BASE TO LARGEST CROWN WIDTH (1 parameter - all species)

  !     RA Coefficients from Hann, Bluhm, and Hibbs Red Alder Plantation Analysis
  !     DF Coefficients from Hann (1999) FS 45: 217-225
  !     WH Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     RC Coefficients from WH
  !     BL Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     PD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     WI Coefficients from Hann and Hanus (2001) FRL Research Contribution 34

  data DACBPAR/&
       &0.63619616, 0.062000, 0.209806, 0.209806, 0.0     ,& !  RA,DF,WH,RC,BL
       &0.0       , 0.0     ,& !  PD,WI

       &-1.2180562 , 0.0     , 0.0     , 0.0     , 0.0     ,& !  RA,DF,WH,RC,BL
       &0.0       , 0.0/ !  PD,WI

  B1 = DACBPAR(ISPGRP,1)
  B2 = DACBPAR(ISPGRP,2)
  if (SCR .gt. CR) then
     CL = SCR*HT
  else
     CL = CR*HT
  end if
  HLCW = HT-(1.0-B1*EXP(B2*(1.0-HT/140.0)**3))*CL
  return
end

subroutine CALC_CC(VERSION,ISPGRP,HLCW,LCW,HT,DBH,HCB,EXPAN,CCH)
  implicit none
  ! Variables
  integer*4 :: VERSION,ISPGRP,II,L
  real*4  :: HLCW,LCW,HT,DBH,HCB,EXPAN,CCH(41),XL,XHLCW,XLCW,CW,CA
  if (HCB .gt. HLCW) then
     XHLCW = HCB
     select case(VERSION)
     case(1)
        call CW_SWO(ISPGRP,HLCW,LCW,HT,DBH,XHLCW,XLCW)
     case(2)
        call CW_NWO(ISPGRP,HLCW,LCW,HT,DBH,XHLCW,XLCW)
     case(3)
        call CW_SMC(ISPGRP,HLCW,LCW,HT,DBH,XHLCW,XLCW)
     case(4)
        call CW_RAP(ISPGRP,HLCW,LCW,HT,DBH,XHLCW,XLCW)
     end select
  else
     XHLCW = HLCW
     XLCW = LCW
  end if
  do II = 40,1,-1
     L = II-1
     XL = FLOAT(L)*(CCH(41)/40.0)
     if (XL.le.XHLCW) then
        CW = XLCW
     else if (XL .gt. XHLCW .and. XL .lt. HT) then
        select case(VERSION)
        case(1)
           call CW_SWO(ISPGRP,HLCW,LCW,HT,DBH,XL,CW)
        case(2)
           call CW_NWO(ISPGRP,HLCW,LCW,HT,DBH,XL,CW)
        case(3)
           call CW_SMC(ISPGRP,HLCW,LCW,HT,DBH,XL,CW)
        case(4)
           call CW_RAP(ISPGRP,HLCW,LCW,HT,DBH,XL,CW)
        end select
     else
        CW = 0.0
     end if
     CA = (CW**2)*(.001803*EXPAN)
     CCH(II) = CCH(II)+CA
  end do
  return
end

subroutine CW_SWO(ISPGRP,HLCW,LCW,HT,DBH,XL,CW)
  implicit none
  ! Variables
  integer*4 :: ISPGRP
  real*4  :: HLCW,LCW,HT,DBH,XL,CW,CWAPAR(18,3),B1,B2,B3,RP,RATIO

  !  CROWN WIDTH ABOVE LARGEST CROWN WIDTH (3 parameters - all species)

  !     DF Coefficients from Hann (1999) FS 45: 217-225
  !     GW Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     PP Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     SP Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     IC Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     WH Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     RC Coefficients from IC
  !     PY Coefficients from WH
  !     MD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     GC Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     TA Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     CL Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     BL Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     WO Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     BO Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     RA Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     PD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     WI Coefficients from Hann and Hanus (2001) FRL Research Contribution 34

  data CWAPAR/&
       &0.929973,   0.999291,   0.755583,   0.755583,  0.629785,& !  DF,GW,PP,SP,IC,
       &0.629785,   0.629785,   0.629785,   0.5     ,  0.5     ,& !  WH,RC,PY,MD,GC,
       &0.5     ,   0.5     ,   0.5     ,   0.5     ,  0.5     ,& !  TA,CL,BL,WO,BO,
       &0.5     ,   0.5     ,   0.5     ,& !  RA,PD,WI

       &-0.135212,   0.0     ,   0.0     ,   0.0     ,   0.0     ,& !  DF,GW,PP,SP,IC,
       &0.0     ,   0.0     ,   0.0     ,   0.0     ,   0.0     ,& !  WH,RC,PY,MD,GC,
       &0.0     ,   0.0     ,   0.0     ,   0.0     ,   0.0     ,& !  TA,CL,BL,WO,BO,
       &0.0     ,   0.0     ,   0.0     ,& !  RA,PD,WI

       &-0.0157579, -0.0314603,   0.0    ,   0.0     ,   0.0     ,& !  DF,GW,PP,SP,IC,
       &0.0      ,  0.0      ,   0.0    ,   0.0     ,   0.0     ,& !  WH,RC,PY,MD,GC,
       &0.0      ,  0.0      ,   0.0    ,   0.0     ,   0.0     ,& !  TA,CL,BL,WO,BO,
       &0.0      ,  0.0      ,   0.0/ !  RA,PD,WI

  B1 = CWAPAR(ISPGRP,1)
  B2 = CWAPAR(ISPGRP,2)
  B3 = CWAPAR(ISPGRP,3)
  RP = (HT-XL)/(HT-HLCW)
  RATIO = HT/DBH
  if (ISPGRP .eq.1) then
     if (RATIO .gt.50.0) RATIO = 50.0
  else if (ISPGRP .eq.2) then
     if (RATIO .gt.31.0) RATIO = 31.0
  end if
  CW = LCW*RP**(B1+B2*SQRT(RP)+B3*(RATIO))
  return
end

subroutine CW_NWO(ISPGRP,HLCW,LCW,HT,DBH,XL,CW)
  implicit none
  ! Variables
  integer*4 :: ISPGRP
  real*4  :: HLCW,LCW,HT,DBH,XL,CW,CWAPAR(11,3),B1,B2,B3,RP,RATIO

  !  CROWN WIDTH ABOVE LARGEST CROWN WIDTH (3 parameters - all species)

  !     DF Coefficients from Hann (1999) FS 45: 217-225
  !     GF Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     WH Coefficients from Marshall, Johnson, and Hann (2003) CJFR 33: 2059-2066
  !     RC Coefficients from WH Hann and Hanus (2001) FRL Research Contribution 34
  !     PY Coefficients from WH Hann and Hanus (2001) FRL Research Contribution 34
  !     MD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     BL Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     WO Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     RA Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     PD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     WI Coefficients from Hann and Hanus (2001) FRL Research Contribution 34

  data CWAPAR/&
       &0.929973 ,  0.999291 ,  0.461782 ,   0.629785,& !  DF,GF,WH,RC
       &0.629785,   0.5      ,  0.5      ,   0.5     ,& !  PY,MD,BL,WO
       &0.5      ,  0.5      ,  0.5      ,& !  RA,PD,WI

       &-0.135212 ,  0.0      ,  0.552011 ,   0.0     ,& !  DF,GF,WH,RC
       &0.0      ,  0.0      ,  0.0      ,   0.0     ,& !  PY,MD,BL,WO
       &0.0      ,  0.0      ,  0.0      ,& !  RA,PD,WI

       &-0.0157579, -0.0314603,  0.0      ,   0.0     ,& !  DF,GF,WH,RC
       &0.0      ,  0.0      ,  0.0      ,   0.0     ,& !  PY,MD,BL,WO
       &0.0      ,  0.0      ,  0.0/ !  RA,PD,WI

  B1 = CWAPAR(ISPGRP,1)
  B2 = CWAPAR(ISPGRP,2)
  B3 = CWAPAR(ISPGRP,3)
  RP = (HT-XL)/(HT-HLCW)
  RATIO = HT/DBH
  if (ISPGRP .eq.1) then
     if (RATIO .gt.50.0) RATIO = 50.0
  else if (ISPGRP .eq.2) then
     if (RATIO .gt.31.0) RATIO = 31.0
  end if
  CW = LCW*RP**(B1+B2*SQRT(RP)+B3*(RATIO))
  return
end

subroutine CW_SMC(ISPGRP,HLCW,LCW,HT,DBH,XL,CW)
  implicit none
  ! Variables
  integer*4 :: ISPGRP
  real*4  :: HLCW,LCW,HT,DBH,XL,CW,CWAPAR(11,3),B1,B2,B3,RP,RATIO

  !  CROWN WIDTH ABOVE LARGEST CROWN WIDTH (3 parameters - all species)

  !     DF Coefficients from Hann (1999) FS 45: 217-225
  !     GF Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     WH Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     RC Coefficients from WH
  !     PY Coefficients from WH
  !     MD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     BL Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     WO Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     RA Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     PD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     WI Coefficients from Hann and Hanus (2001) FRL Research Contribution 34

  data CWAPAR/&
       &0.929973 ,  0.999291 ,  0.629785 ,   0.629785,& !  DF,GF,WH,RC
       &0.629785,   0.5      ,  0.5      ,   0.5     ,& !  PY,MD,BL,WO
       &0.5      ,  0.5      ,  0.5      ,& !  RA,PD,WI

       &-0.135212 ,  0.0      ,  0.0      ,   0.0     ,& !  DF,GF,WH,RC
       &0.0      ,  0.0      ,  0.0      ,   0.0     ,& !  PY,MD,BL,WO
       &0.0      ,  0.0      ,  0.0      ,& !  RA,PD,WI

       &-0.0157579, -0.0314603,  0.0      ,   0.0     ,& !  DF,GF,WH,RC
       &0.0      ,  0.0      ,  0.0      ,   0.0     ,& !  PY,MD,BL,WO
       &0.0      ,  0.0      ,  0.0/ !  RA,PD,WI

  B1 = CWAPAR(ISPGRP,1)
  B2 = CWAPAR(ISPGRP,2)
  B3 = CWAPAR(ISPGRP,3)
  RP = (HT-XL)/(HT-HLCW)
  RATIO = HT/DBH
  if (ISPGRP .eq.1) then
     if (RATIO .gt.50.0) RATIO = 50.0
  else if (ISPGRP .eq.2) then
     if (RATIO .gt.31.0) RATIO = 31.0
  end if
  CW = LCW*RP**(B1+B2*SQRT(RP)+B3*(RATIO))
  return
end

subroutine CW_RAP(ISPGRP,HLCW,LCW,HT,DBH,XL,CW)
  implicit none
  ! Variables
  integer*4 :: ISPGRP
  real*4  :: HLCW,LCW,HT,DBH,XL,CW,CWAPAR(7,3),B1,B2,B3,RP,RATIO

  !  CROWN WIDTH ABOVE LARGEST CROWN WIDTH (3 parameters - all species)

  !     RA Coefficients from Hann, Bluhm, and Hibbs Red Alder Plantation Analysis
  !     DF Coefficients from Hann (1999) FS 45: 217-225
  !     WH Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     RC Coefficients from WH
  !     BL Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     PD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     WI Coefficients from Hann and Hanus (2001) FRL Research Contribution 34

  data CWAPAR/&
       &0.63420194,  0.929973 ,  0.629785 ,   0.629785,& !  RA,DF,WH,RC
       &0.5       ,   0.5     ,  0.5      ,& !  BL,PD,WI

       &0.17649614, -0.135212 ,  0.0      ,   0.0     ,& !  RA,DF,WH,RC
       &0.0       ,  0.0      ,  0.0      ,& !  BL,PD,WI

       &-0.02315018, -0.0157579,  0.0      ,   0.0     ,& !  RA,DF,WH,RC
       &0.0       ,  0.0      ,  0.0/ !  BL,PD,WI

  B1 = CWAPAR(ISPGRP,1)
  B2 = CWAPAR(ISPGRP,2)
  B3 = CWAPAR(ISPGRP,3)
  RP = (HT-XL)/(HT-HLCW)
  RATIO = HT/DBH
  if (ISPGRP .eq.2) then
     if (RATIO .gt.50.0) RATIO = 50.0
  end if
  CW = LCW*RP**(B1+B2*SQRT(RP)+B3*(RATIO))
  return
end

subroutine CRNCLO(IND,CTMUL,VERSION,NTREES,TDATAI,TDATAR,&
     &SCR,MGEXP,CCH,CC)
  !     DETERMINE CROWN CLOSURE

  !          IND = 0 COMPUTE CROWN CLOSURES WITHOUT SHADOW CROWN RATIOS
  !              = 1 COMPUTE CROWN CLOSURES WITH SHADOW CROWN RATIOS
  !          CTMUL = CUT TREE MULTIPLIER
  !                =  0.0 TO NOT ADD OR SUBTRACT CUT TREES
  !                =  1.0 TO ADD CUT TREES
  !                = -1.0 TO SUBTRACT CUT TREES
  implicit none
  ! Variables
  integer*4 :: IND,VERSION,NTREES,TDATAI(2000,3),L,I,ISPGRP
  real*4  :: CTMUL,TDATAR(2000,8),SCR(2000,3),MGEXP(2000),CCH(41),CC,DBH,HT,CR,CL,HCB,EXPAN,SCR1,MCW,&
       &LCW,HLCW

  do L = 1,40
     CCH(L) = 0.
  end do
  CCH(41) = TDATAR(1,2)
  do I = 2,NTREES
     if (TDATAR(I,2).gt.CCH(41)) CCH(41) = TDATAR(I,2)
  end do
  do I = 1,NTREES
     ISPGRP = TDATAI(I,2)
     DBH = TDATAR(I,1)
     HT = TDATAR(I,2)
     if (IND .eq.1.and. SCR(I,1) .gt.0.0) then
        CR = SCR(I,1)
     else
        CR = TDATAR(I,3)
     end if
     SCR1 = SCR(I,1)
     CL = CR*HT
     HCB = HT-CL
     EXPAN = TDATAR(I,4)+CTMUL*MGEXP(I)
     select case(VERSION)
     case(1)
        call MCW_SWO(ISPGRP,DBH,HT,MCW)
        call LCW_SWO(ISPGRP,MCW,CR,SCR1,DBH,HT,LCW)
        call HLCW_SWO(ISPGRP,HT,CR,SCR1,HLCW)
        call CALC_CC(VERSION,ISPGRP,HLCW,LCW,HT,DBH,HCB,EXPAN,&
             &CCH)
     case(2)
        call MCW_NWO(ISPGRP,DBH,HT,MCW)
        call LCW_NWO(ISPGRP,MCW,CR,SCR1,DBH,HT,LCW)
        call HLCW_NWO(ISPGRP,HT,CR,SCR1,HLCW)
        call CALC_CC(VERSION,ISPGRP,HLCW,LCW,HT,DBH,HCB,EXPAN,&
             &CCH)
     case(3)
        call MCW_SMC(ISPGRP,DBH,HT,MCW)
        call LCW_SMC(ISPGRP,MCW,CR,SCR1,DBH,HT,LCW)
        call HLCW_SMC(ISPGRP,HT,CR,SCR1,HLCW)
        call CALC_CC(VERSION,ISPGRP,HLCW,LCW,HT,DBH,HCB,EXPAN,&
             &CCH)
     case(4)
        call MCW_RAP(ISPGRP,DBH,HT,MCW)
        call LCW_RAP(ISPGRP,MCW,CR,SCR1,DBH,HT,LCW)
        call HLCW_RAP(ISPGRP,HT,CR,SCR1,HLCW)
        call CALC_CC(VERSION,ISPGRP,HLCW,LCW,HT,DBH,HCB,EXPAN,&
             &CCH)
     end select
  end do
  CC = CCH(1)
  return
end

subroutine GET_CCFL(DBH,CCFLL1,CCFL1,CCFL)
  implicit none
  ! Variables
  integer*4 :: K
  real*4  :: DBH,CCFLL1(51),CCFL1(500),CCFL
  if (DBH .gt.100.0) then
     CCFL = 0.0
  else if (DBH .gt.50.0) then
     K = INT(DBH-49.0)
     CCFL = CCFLL1(K)
  else
     K = INT(DBH*10.0+0.5)
     CCFL = CCFL1(K)
  end if
  return
end

subroutine MAXHCB_SWO(ISPGRP,HT,CCFL,MAXHCB)
  implicit none
  ! Variables
  integer*4 :: ISPGRP
  real*4  :: HT,CCFL,MAXHCB
  real*4  :: MAXPAR(18,5),B0,B1,B2,B3,LIMIT,MAXBR

  !  MAXIMUM HEIGHT TO CROWN BASE
  !     (5 parameters - all species)

  data MAXPAR/&
       &0.96       ,  0.96       ,  1.01       ,  1.02       ,& !  DF,GW,PP,SP
       &0.97       ,  1.01       ,  0.96       ,  0.85       ,& !  IC,WH,RC,PY
       &0.981      ,  1.0        ,  0.98       ,  1.0         ,& !  MD,GC,TA,CL
       &1.0         , 1.0        ,  1.0        ,  0.93        ,& !  BL,WO,BO,RA
       &1.0         , 0.985      ,& !  PD,WI

       &0.26       ,  0.31       ,  0.36       ,  0.27       ,& !  DF,GW,PP,SP
       &0.22       ,  0.36       ,  0.31       ,  0.35       ,& !  IC,WH,RC,PY
       &0.161      ,  0.45       ,  0.33       ,  0.45       ,& !  MD,GC,TA,CL
       &0.45       ,  0.3        ,  0.2        ,  0.18       ,& !  BL,WO,BO,RA
       &0.45       ,  0.285      ,& !  PD,WI

       &-0.987864873, -2.450718394, -1.041915784, -0.922718593,& !  DF,GW,PP,SP
       &-0.002612590, -0.944528054, -1.059636222, -0.922868139,& !  IC,WH,RC,PY
       &-1.73666044 , -1.219919284, -0.911341687, -0.922025464,& !  MD,GC,TA,CL
       &-1.020016685, -0.95634399 , -1.053892465, -0.928243505,& !  BL,WO,BO,RA
       &-1.020016685, -0.969750805,& !  PD,WI

       &1.0        ,  1.0        ,  0.6        ,  0.4        ,& !  DF,GW,PP,SP
       &1.0        ,  0.6        ,  1.0        ,  0.8        ,& !  IC,WH,RC,PY
       &1.0        ,  1.2        ,  1.0        ,  1.0        ,& !  MD,GC,TA,CL
       &1.0        ,  1.1        ,  1.0        ,  1.0        ,& !  BL,WO,BO,RA
       &1.0        ,  0.9        ,& !  PD,WI

       &0.95       ,  0.95       ,  0.95       ,  0.96       ,& !  DF,GW,PP,SP
       &0.95       ,  0.96       ,  0.95       ,  0.80       ,& !  IC,WH,RC,PY
       &0.98       ,  0.98       ,  0.97       ,  0.98       ,& !  MD,GC,TA,CL
       &0.95       ,  0.98       ,  0.98       ,  0.92       ,& !  BL,WO,BO,RA
       &0.95       ,  0.98/ !  PD,WI

  B0 = MAXPAR(ISPGRP,1)
  B1 = MAXPAR(ISPGRP,2)
  B2 = MAXPAR(ISPGRP,3)
  B3 = MAXPAR(ISPGRP,4)
  LIMIT = MAXPAR(ISPGRP,5)
  MAXBR = B0-B1*EXP(B2*(CCFL/100.0)**B3)
  if (MAXBR .gt. LIMIT) MAXBR = LIMIT
  MAXHCB = MAXBR*HT
  return
end

subroutine MAXHCB_NWO(ISPGRP,HT,CCFL,MAXHCB)
  implicit none
  ! Variables
  integer*4 :: ISPGRP
  real*4  :: HT,CCFL,MAXHCB
  real*4  :: MAXPAR(11,5),B0,B1,B2,B3,LIMIT,MAXBR

  !  MAXIMUM HEIGHT TO CROWN BASE
  !     (5 parameters - all species)

  data MAXPAR/&
       &0.96       ,  0.96       ,  1.01       ,  0.96       ,& !  DF,GF,WH,RC
       &0.85       ,  0.981      ,  1.0        ,  1.0        ,& !  PY,MD,BL,WO
       &0.93       ,  1.0        ,  0.985      ,& !  RA,PD,WI

       &0.26       ,  0.31       ,  0.36       ,  0.31       ,& !  DF,GF,WH,RC
       &0.35       ,  0.161      ,  0.45       ,  0.3        ,& !  PY,MD,BL,WO
       &0.18       ,  0.45       ,  0.285      ,& !  RA,PD,WI

       &-0.900721383, -2.450718394, -0.944528054, -1.059636222,& !  DF,GW,WH,RC
       &-0.922868139, -1.73666044 , -1.020016685, -0.95634399 ,& !  PY,MD,BL,WO
       &-0.928243505, -1.020016685, -0.969750805,& !  RA,PD,WI

       &1.0        ,  1.0        ,  0.6        ,  1.0        ,& !  DF,GW,WH,RC
       &0.8        ,  1.0        ,  1.0        ,  1.1        ,& !  PY,MD,BL,WO
       &1.0        ,  1.0        ,  0.9        ,& !  RA,PD,WI

       &0.95       ,  0.95       ,  0.96       ,  0.95       ,& !  DF,GW,WH,RC
       &0.80       ,  0.98       ,  0.95       ,  0.98       ,& !  PY,MD,BL,WO
       &0.92       ,  0.95       ,  0.98/ !  RA,PD,WI

  B0 = MAXPAR(ISPGRP,1)
  B1 = MAXPAR(ISPGRP,2)
  B2 = MAXPAR(ISPGRP,3)
  B3 = MAXPAR(ISPGRP,4)
  LIMIT = MAXPAR(ISPGRP,5)
  MAXBR = B0-B1*EXP(B2*(CCFL/100.0)**B3)
  if (MAXBR .gt. LIMIT) MAXBR = LIMIT
  MAXHCB = MAXBR*HT
  return
end

subroutine MAXHCB_SMC(ISPGRP,HT,CCFL,MAXHCB)
  implicit none
  ! Variables
  integer*4 :: ISPGRP
  real*4  :: HT,CCFL,MAXHCB
  real*4  :: MAXPAR(11,5),B0,B1,B2,B3,LIMIT,MAXBR

  !  MAXIMUM HEIGHT TO CROWN BASE
  !     (5 parameters - all species)

  data MAXPAR/&
       &0.96       ,  0.96       ,  1.01       ,  0.96       ,& !  DF,GF,WH,RC
       &0.85       ,  0.981      ,  1.0        ,  1.0        ,& !  PY,MD,BL,WO
       &0.93       ,  1.0        ,  0.985      ,& !  RA,PD,WI

       &0.26       ,  0.31       ,  0.36       ,  0.31       ,& !  DF,GF,WH,RC
       &0.35       ,  0.161      ,  0.45       ,  0.3        ,& !  PY,MD,BL,WO
       &0.18       ,  0.45       ,  0.285      ,& !  RA,PD,WI

       &-0.34758    , -2.450718394, -0.944528054, -1.059636222,& !  DF,GW,WH,RC
       &-0.922868139, -1.73666044 , -1.020016685, -0.95634399 ,& !  PY,MD,BL,WO
       &-0.928243505, -1.020016685, -0.969750805,& !  RA,PD,WI

       &1.5        ,  1.0        ,  0.6        ,  1.0        ,& !  DF,GW,WH,RC
       &0.8        ,  1.0        ,  1.0        ,  1.1        ,& !  PY,MD,BL,WO
       &1.0        ,  1.0        ,  0.9        ,& !  RA,PD,WI

       &0.95       ,  0.95       ,  0.96       ,  0.95       ,& !  DF,GW,WH,RC
       &0.80       ,  0.98       ,  0.95       ,  0.98       ,& !  PY,MD,BL,WO
       &0.92       ,  0.95       ,  0.98/ !  RA,PD,WI

  B0 = MAXPAR(ISPGRP,1)
  B1 = MAXPAR(ISPGRP,2)
  B2 = MAXPAR(ISPGRP,3)
  B3 = MAXPAR(ISPGRP,4)
  LIMIT = MAXPAR(ISPGRP,5)
  MAXBR = B0-B1*EXP(B2*(CCFL/100.0)**B3)
  if (MAXBR .gt. LIMIT) MAXBR = LIMIT
  MAXHCB = MAXBR*HT
  return
end

subroutine MAXHCB_RAP(ISPGRP,HT,CCFL,MAXHCB)
  implicit none
  ! Variables
  integer*4 :: ISPGRP
  real*4  :: HT,CCFL,MAXHCB
  real*4  :: MAXPAR(7,5),B0,B1,B2,B3,LIMIT,MAXBR

  !  MAXIMUM HEIGHT TO CROWN BASE
  !     (5 parameters - all species)

  data MAXPAR/&
       &0.93       ,  0.96       ,  1.01       ,  0.96       ,& !  RA,DF,WH,RC
       &1.0        ,  1.0        ,  0.985      ,& !  BL,PD,WI

       &0.18       ,  0.26       ,  0.36       ,  0.31       ,& !  RA,DF,WH,RC
       &0.45       ,  0.45       ,  0.285      ,& !  BL,PD,WI

       &-0.928243505, -0.34758    , -0.944528054, -1.059636222,& !  RA,DF,WH,RC
       &-1.020016685, -1.020016685, -0.969750805,& !  BL,PD,WI

       &1.0        ,  1.5        ,  0.6        ,  1.0        ,& !  RA,DF,WH,RC
       &1.0        ,  1.0        ,  0.9        ,& !  BL,PD,WI

       &0.92       ,  0.95       ,  0.96       ,  0.95       ,& !  RA,DF,WH,RC
       &0.95       ,  0.95       ,  0.98/ !  BL,PD,WI

  B0 = MAXPAR(ISPGRP,1)
  B1 = MAXPAR(ISPGRP,2)
  B2 = MAXPAR(ISPGRP,3)
  B3 = MAXPAR(ISPGRP,4)
  LIMIT = MAXPAR(ISPGRP,5)
  MAXBR = B0-B1*EXP(B2*(CCFL/100.0)**B3)
  if (MAXBR .gt. LIMIT) MAXBR = LIMIT
  MAXHCB = MAXBR*HT
  return
end
