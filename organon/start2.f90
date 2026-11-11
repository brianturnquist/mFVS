!======================!
!   organon : start2   !
!======================!
! ORGANON $Id$

  !     ORGANON GROWTH AND YIELD MODEL
  !     SUBROUTINES INCLUDED:
  !        HDCALIB
  !        PRDHT
  !        A_HD_SWO
  !        A_HD_RAP
  !        CRCALIB
  !        PRDCR
  !        A_HCB_SWO
  !        A_HCB_RAP
  !        HS_H40
  !        B_H40
  !        F_H40
  !C        NCH40
  !        WHHLB_SI_UC
  !        WHHLB_H40
  !        SITECV_F
  !        SITEF_C
  !        SITEF_SI
  !        DFORTY
  !        SPMIX
  !        HD40_SWO
  !        HD40_NWO
  !        HD40_SMC
  !        HD40_RAP
  !        GET_CCFL
  !        CALTST

  !  04/25/2014 - THERE ARE COMMON SUBROUTINE NAMES IN THE SOURCE CODE
  !               USED TO BUILD THE ORGANON DLLS. IN ORDER TO LINK THE
  !               ORGANON SOURCE CODE WITH THE FVS SOURCE CODE WE CHANGED
  !               THE DUPLICATED SUBROUTINE NAMES TO MAKE THEM UNIQUE.

  !  CHANGED THE NAME OF SUBROUTINE/SUBROUTINE CALL - GET_CCFL TO GET_CCFL_EDIT
  !  CHANGED THE NAME OF SUBROUTINE/SUBROUTINE CALL - SITECV_F TO SITECV_F_EDIT
  !  CHANGED THE NAME OF SUBROUTINE/SUBROUTINE CALL - SITEF_C TO SITEF_C_EDIT
  !  CHANGED THE NAME OF SUBROUTINE/SUBROUTINE CALL - SITEF_SI TO SITEF_SI_EDIT
  !  CHANGED THE NAME OF SUBROUTINE/SUBROUTINE CALL - WHHLB_SI_UC TO WHHLB_SI_UC_EDIT

subroutine HDCALIB(VERSION,IB,NSPN,NTREES,NPTS,TAGE,BHAGE,TDATAI,&
     &EVEN,SI_1,SI_2,PDEN,TDATAR,D40,HT40,PDF,PTF,PPP,PWH,&
     &PRA,ENTHT,ENTDBH,PTRHT,CALIB)
  implicit none

  !     ROUTINE TO CALIBRATE HEIGHT/DIAMETER EQUATIONS

  ! Variables
  integer*4 :: TAGE,BHAGE,ENTHT(18),ENTDBH(18),IB,NSPN,NTREES,NPTS,TDATAI(2000,3),VERSION
  real*4  :: CALIB(3,18),SI_1,SI_2,PTRHT(2000),TDATAR(2000,4)
  logical*2 :: EVEN
  integer*4 :: I,ISPGRP
  real*4  :: X,YXS(18),XSS(18),Y,WT,YSS(18),BETA,PDF,PTF,PPP,PWH,PRA,HT40,D40,AGE1,DBH,PDEN,SI_UC,AGE2
  !      INTEGER*4 IANS,IYN
  !      IF (J .EQ. 1) THEN
  !         WRITE(*,*) ' GROWTH = ',GROWTH(J),' PDG = ',PDG
  !C         WRITE(*,*) ' BAL1(1) = ',BAL1(1)
  !         WRITE(*,1600)
  ! 1600    FORMAT(1X,' '\)
  !         IANS = IYN(2)
  !      ENDIF

  !     DETERMINE CALIBRATION RATIO FOR SPECIES GROUPS

  do I = 1,18
     ENTHT(I) = 0
     ENTDBH(I) = 0
     CALIB(1,I) = 1.0
     YXS(I) = 0.0
     XSS(I) = 0.0
     YSS(I) = 0.0
  end do
  D40 = 0.0
  HT40 = 0.0
  PDF = 0.0
  PTF = 0.0
  PPP = 0.0
  PWH = 0.0
  PRA = 0.0
  if (EVEN) then
     AGE1 = FLOAT(BHAGE)
     AGE2 = FLOAT(TAGE)
     call SPMIX(NTREES,NPTS,TDATAI,TDATAR,PDF,PTF,PPP,PWH,PRA)
     call DFORTY(VERSION,NTREES,NPTS,IB,TDATAI,TDATAR,D40)
     select case(VERSION)
     case(1)
        if (PDF .ge.0.80) then
           call HS_H40(HT40,1,AGE1,SI_1)
        else if (PTF .ge.0.80) then
           call HS_H40(HT40,1,AGE1,SI_1)
        else if (PPP .ge.0.80) then
           call HS_H40(HT40,2,AGE1,SI_2)
        end if
     case(2,3)
        if (PDF .ge.0.80) then
           call B_H40(HT40,AGE1,SI_1)
        else if (PWH .ge.0.80) then
           call F_H40(HT40,AGE1,SI_2)
        end if
     case(4)
        if (PRA .ge.0.80) then
           call WHHLB_SI_UC_EDIT(SI_1,PDEN,SI_UC)
           call WHHLB_H40_EDIT(SI_UC,20.0,AGE2,HT40)
        end if
     end select
  end if
  do I = 1,NTREES
     ISPGRP = TDATAI(I,2)
     DBH = TDATAR(I,1)
     ENTDBH(ISPGRP) = ENTDBH(ISPGRP)+1
     if (TDATAR(I,2) .le.0.0) cycle

     !        CALCULATE TREE HEIGHT

     select case(VERSION)
     case(1)
        if (PDF .ge.0.80.and. ISPGRP .eq.1) then
           call HD40_SWO(1,HT40,D40,DBH,PTRHT(I))
        else if (PTF .ge.0.80.and. ISPGRP .eq.2) then
           call HD40_SWO(2,HT40,D40,DBH,PTRHT(I))
        else if (PPP .ge.0.80.and. ISPGRP .eq.3) then
           call HD40_SWO(3,HT40,D40,DBH,PTRHT(I))
        else
           call A_HD_SWO(ISPGRP,DBH,PTRHT(I))
        end if
     case(2)
        if (PDF .ge.0.80.and. ISPGRP .eq.1) then
           call HD40_NWO(1,HT40,D40,DBH,PTRHT(I))
        else if (PWH .ge.0.80.and. ISPGRP .eq.3) then
           call HD40_NWO(2,HT40,D40,DBH,PTRHT(I))
        else
           call HD_NWO(ISPGRP,DBH,PTRHT(I))
        end if
     case(3)
        if (PDF .ge.0.80.and. ISPGRP .eq.1) then
           call HD40_SMC(1,HT40,D40,DBH,PTRHT(I))
        else if (PWH .ge.0.80.and. ISPGRP .eq.3) then
           call HD40_SMC(2,HT40,D40,DBH,PTRHT(I))
        else
           call HD_SMC(ISPGRP,DBH,PTRHT(I))
        end if
     case(4)
        if (PRA .ge.0.80.and. ISPGRP .eq.1) then
           call HD40_RAP(1,HT40,D40,DBH,PTRHT(I))
        else
           call A_HD_RAP(ISPGRP,DBH,PTRHT(I))
        end if
     end select
     Y = TDATAR(I,2)-4.5
     X = PTRHT(I)-4.5
     WT = TDATAR(I,1)
     YXS(ISPGRP) = YXS(ISPGRP)+Y*X/WT
     XSS(ISPGRP) = XSS(ISPGRP)+X*X/WT
     YSS(ISPGRP) = YSS(ISPGRP)+Y*Y/WT
     ENTHT(ISPGRP) = ENTHT(ISPGRP)+1
  end do
  do I = 1, NSPN
     if (ENTDBH(I) .eq.0) cycle
     if (ENTHT(I) .lt.2) cycle
     call CALTST(YXS(I),XSS(I),YSS(I),ENTHT(I),BETA)
     CALIB(1,I) = BETA
  end do
  return
end

subroutine PRDHT(VERSION,NTREES,TDATAI,ENT,TDATAR,D40,HT40,&
     &PDF,PTF,PPP,PWH,PRA,CALIB,PTRHT)
  implicit none

  !     ROUTINE TO PREDICT MISSING TREE HEIGHTS

  ! Variables
  integer*4 :: NTREES,TDATAI(2000,3),VERSION
  real*4  :: CALIB(3,18),PTRHT(2000),TDATAR(2000,4)
  integer*4 :: ENT,I,ISPGRP
  real*4  :: PDF,PTF,PPP,PWH,PRA,HT40,D40,DBH
  ENT = 0
  do I = 1,NTREES
     ISPGRP = TDATAI(I,2)
     DBH = TDATAR(I,1)
     if (DBH .le.0.0) cycle
     if (TDATAR(I,2) .gt.0.0) cycle

     !        CALCULATE TREE HEIGHT

     select case(VERSION)
     case(1)
        if (PDF .ge.0.80.and. ISPGRP .eq.1) then
           call HD40_SWO(1,HT40,D40,DBH,PTRHT(I))
        else if (PTF .ge.0.80.and. ISPGRP .eq.2) then
           call HD40_SWO(2,HT40,D40,DBH,PTRHT(I))
        else if (PPP .ge.0.80.and. ISPGRP .eq.3) then
           call HD40_SWO(3,HT40,D40,DBH,PTRHT(I))
        else
           call A_HD_SWO(ISPGRP,DBH,PTRHT(I))
        end if
     case(2)
        if (PDF .ge.0.80.and. ISPGRP .eq.1) then
           call HD40_NWO(1,HT40,D40,DBH,PTRHT(I))
        else if (PWH .ge.0.80.and. ISPGRP .eq.3) then
           call HD40_NWO(2,HT40,D40,DBH,PTRHT(I))
        else
           call HD_NWO(ISPGRP,DBH,PTRHT(I))
        end if
     case(3)
        if (PDF .ge.0.80.and. ISPGRP .eq.1) then
           call HD40_SMC(1,HT40,D40,DBH,PTRHT(I))
        else if (PWH .ge.0.80.and. ISPGRP .eq.3) then
           call HD40_SMC(2,HT40,D40,DBH,PTRHT(I))
        else
           call HD_SMC(ISPGRP,DBH,PTRHT(I))
        end if
     case(4)
        if (PRA .ge.0.80.and. ISPGRP .eq.1) then
           call HD40_RAP(1,HT40,D40,DBH,PTRHT(I))
        else
           call A_HD_RAP(ISPGRP,DBH,PTRHT(I))
        end if
     end select
     ENT = ENT+1
     TDATAR(I,2) = 4.5+CALIB(1,ISPGRP)*(PTRHT(I)-4.5)
     if (TDATAR(I,2) .lt.4.6)TDATAR(I,2) = 4.6
  end do
  return
end

subroutine A_HD_SWO(ISPGRP,DBH,PRDHT)
  implicit none
  ! Variables
  real*4  :: DBH,PRDHT,HDPAR(18,3),B0,B1,B2
  integer*4 :: ISPGRP

  !  NEW HEIGHT/DIAMETER PARAMETERS FOR ALL (UNDAMAGED AND DAMAGED) TREES
  !      (3 parameters - all species)

  !     DF Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
  !     GW Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
  !     PP Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
  !     SP Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
  !     IC Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
  !     WH Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
  !     RC Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #2
  !     PY Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
  !     MD Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
  !     GC Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
  !     TA Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
  !     CL Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
  !     BL Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
  !     WO Coefficients from Gould, Marshall, and Harrington (2008) West. J. Appl. For. 23: 26-33
  !     BO Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
  !     RA Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #1
  !     PD Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
  !     WI Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27

  data HDPAR/&
       &7.153156143,  6.638003799, 7.181264435, 6.345116767,& !  DF,GW,PP,SP
       &8.776627288,  6.58804    , 6.14817441 , 6.402691396,& !  IC,WH,RC,PY
       &5.42457261 ,  9.21600278 , 7.398142262, 7.762149257,& !  MD,GC,TA,CL
       &5.02002617 ,  4.69753118 , 4.907340242, 5.59759126 ,& !  BL,WO,BO,RA
       &5.252315215,  3.862132151,& !  PD,WI

       &-5.36900835 , -5.44399465 ,-5.90709219 ,-5.30026188 ,& !  DF,GW,PP,SP
       &-7.4383668  , -5.35325461 ,-5.40092761 ,-4.79802411 ,& !  IC,WH,RC,PY
       &-3.56317104 , -7.63409138 ,-5.5099273  ,-6.04759773 ,& !  MD,GC,TA,CL
       &-2.51228202 , -3.51586969 ,-3.18017969 ,-3.19942952 ,& !  BL,WO,BO,RA
       &-3.13509983 , -1.5294776  ,& !  PD,WI

       &-0.25832512 , -0.33929196 ,-0.27533719 ,-0.35264183 ,& !  DF,GW,PP,SP
       &-0.16906224 , -0.31897786 ,-0.38922036 ,-0.16317997 ,& !  IC,WH,RC,PY
       &-0.36177689 , -0.15346440 ,-0.19080702 ,-0.16308399 ,& !  MD,GC,TA,CL
       &-0.42256497 , -0.57665068 ,-0.46654227 ,-0.38783403 ,& !  BL,WO,BO,RA
       &-0.26979750 , -0.62476287 / !  PD,WI

  B0 = HDPAR(ISPGRP,1)
  B1 = HDPAR(ISPGRP,2)
  B2 = HDPAR(ISPGRP,3)
  PRDHT = 4.5+EXP(B0+B1*DBH**B2)
  return
end

subroutine A_HD_RAP(ISPGRP,DBH,PRDHT)
  implicit none
  ! Variables
  real*4  :: DBH,PRDHT,HDPAR(7,3),B0,B1,B2
  integer*4 :: ISPGRP

  !  HEIGHT/DIAMETER PARAMETERS (UNDAMAGED AND DAMAGED) TREES
  !     (3 parameters - all species)

  !     RA Coefficients from Hann, Bluhm, and Hibbs (2011) Forest Biometrics Research Paper 1
  !     DF Coefficients from Hanus, Marshall, and Hann (1999) FRL Research Contribution 25
  !     WH Coefficients from Hanus, Marshall, and Hann (1999) FRL Research Contribution 25
  !     RC Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #2
  !     BL Coefficients from Wang and Hann (1988) FRL Research Paper 51
  !     PD Coefficients from Wang and Hann (1988) FRL Research Paper 51
  !     WI Coefficients from Wang and Hann (1988) FRL Research Paper 51

  data HDPAR/&
       &6.76804202 ,  7.262195456,  6.555344622,  6.14817441,& !  RA,DF,WH,RC
       &5.21462    ,  4.49727    ,  4.88361    ,& !  BL,PD,WI

       &-4.6370303  , -5.899759104, -5.137174162, -5.40092761,& !  RA,DF,WH,RC
       &-2.70252    , -2.07667    , -2.47605    ,& !  BL,PD,WI

       &-0.23108894 , -0.287207389, -0.364550800, -0.38922036,& !  RA,DF,WH,RC
       &-0.354756   ,  -0.388650  , -0.309050  / !  BL,PD,WI

  B0 = HDPAR(ISPGRP,1)
  B1 = HDPAR(ISPGRP,2)
  B2 = HDPAR(ISPGRP,3)
  PRDHT = 4.5+EXP(B0+B1*DBH**B2)
  return
end

subroutine CRCALIB(VERSION,IB,NSPN,NTREES,NPTS,TDATAI,&
     &ENTDBH,SI_1,SI_2,TDATAR,STDATAR,BAL,BALL,CCFL,CCFLL,&
     &SBA,OG,ENTCR,PCR,CALIB)
  implicit none

  !     ROUTINE TO CALIBRATE CROWN RATIOS

  ! Variables
  integer*4 :: VERSION,IB,NSPN,NTREES,NPTS,TDATAI(2000,3),ENTDBH(18),ENTCR(18)
  real*4  :: SI_1,SI_2,TDATAR(2000,4),STDATAR(2000,4),BAL(500),BALL(51),CCFL(500),CCFLL(51),SBA,OG,&
       &PCR(2000),CALIB(3,18)
  integer*4 :: I,ISPGRP
  real*4  :: YSS(18),X,Y,XSS(18),YXS(18),DBH,HT,XSI_1,XSI_2,SCCFL,PHCB,BETA
  !      OPEN(44,FILE='TEMP.DAT',STATUS='UNKNOWN')

  do I = 1,18
     ENTCR(I) = 0
     CALIB(2,I) = 1.0
     YSS(I) = 0.0
     XSS(I) = 0.0
     YXS(I) = 0.0
  end do

  call SSUM(2,VERSION,NPTS,NTREES,TDATAI,TDATAR,STDATAR,SBA,&
       &BAL,BALL,CCFL,CCFLL)
  call OLDGROWTH(NPTS,NTREES,IB,TDATAI,TDATAR,OG)
  do I = 1,NTREES
     if (TDATAR(I,3) .le.0.0) cycle
     ISPGRP = TDATAI(I,2)
     DBH = TDATAR(I,1)
     HT = TDATAR(I,2)
     XSI_1 = SI_1-4.5
     XSI_2 = SI_2-4.5
     call GET_CCFL_EDIT(DBH,CCFLL,CCFL,SCCFL)
     !         WRITE(44,2002) HT,DBH,SCCFL,SBA,SI_1
     ! 2002    FORMAT(F16.6,1X,F16.6,1X,F16.6,1X,F16.6,1X,F16.6)
     select case(VERSION)
     case(1)
        call A_HCB_SWO(ISPGRP,HT,DBH,SCCFL,SBA,XSI_1,XSI_2,OG,&
             &PHCB)
        PCR(I) = 1.0-PHCB/HT
     case(2)
        call HCB_NWO(ISPGRP,HT,DBH,SCCFL,SBA,XSI_1,XSI_2,OG,&
             &PHCB)
        PCR(I) = 1.0-PHCB/HT
     case(3)
        call HCB_SMC(ISPGRP,HT,DBH,SCCFL,SBA,XSI_1,XSI_2,OG,&
             &PHCB)
        PCR(I) = 1.0-PHCB/HT
     case(4)
        call A_HCB_RAP(ISPGRP,HT,DBH,SCCFL,SBA,XSI_1,XSI_2,OG,&
             &PHCB)
        PCR(I) = 1.0-PHCB/HT
     end select
     X = PCR(I)
     Y = TDATAR(I,3)
     !         WRITE(44,2000) X,Y
     ! 2000    FORMAT(F8.4,1X,F8.4)
     YXS(ISPGRP) = YXS(ISPGRP)+X*Y
     XSS(ISPGRP) = XSS(ISPGRP)+X*X
     YSS(ISPGRP) = YSS(ISPGRP)+Y*Y
     ENTCR(ISPGRP) = ENTCR(ISPGRP)+1
  end do
  do I = 1,NSPN
     if (ENTDBH(I) .eq.0) cycle
     if (ENTCR(I) .lt.2)cycle
     call CALTST(YXS(I),XSS(I),YSS(I),ENTCR(I),BETA)
     CALIB(2,I) = BETA
  end do
  !      WRITE(44,2001) YXS(1),XSS(1),YSS(1),BETA
  ! 2001 FORMAT(F16.6,1X,F16.6,1X,F16.6,1X,F16.6)
  !      CLOSE(44)
  return
end

subroutine PRDCR(VERSION,IB,NSPN,NTREES,NPTS,TDATAI,SI_1,&
     &SI_2,TDATAR,CCFL,CCFLL,SBA,OG,ENT,PCR,CALIB)
  implicit none

  !     ROUTINE TO CALCULATE MISSING CROWN RATIOS

  ! Variables
  integer*4 :: VERSION,IB,NSPN,NTREES,NPTS,TDATAI(2000,3),ENT
  real*4  :: SI_1,SI_2,TDATAR(2000,4),CCFL(500),CCFLL(51),SBA,OG,PCR(2000),CALIB(3,18)
  integer*4 :: I,ISPGRP
  real*4  :: DBH,HT,XSI_1,XSI_2,SCCFL,PHCB
  integer*4 :: IDANUW

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  IDANUW = IB
  IDANUW = NPTS
  IDANUW = NSPN

  ENT = 0
  do I = 1,NTREES
     if (TDATAR(I,3) .gt.0.0) cycle

     !        CALCULATE  CROWN RATIO

     ENT = ENT+1
     ISPGRP = TDATAI(I,2)
     DBH = TDATAR(I,1)
     if (DBH .le.0.0) cycle
     HT = TDATAR(I,2)
     XSI_1 = SI_1-4.5
     XSI_2 = SI_2-4.5
     call GET_CCFL_EDIT(DBH,CCFLL,CCFL,SCCFL)
     select case(VERSION)
     case(1)
        call A_HCB_SWO(ISPGRP,HT,DBH,SCCFL,SBA,XSI_1,XSI_2,OG,&
             &PHCB)
        PCR(I) = 1.0-PHCB/HT
     case(2)
        call HCB_NWO(ISPGRP,HT,DBH,SCCFL,SBA,XSI_1,XSI_2,OG,&
             &PHCB)
        PCR(I) = 1.0-PHCB/HT
     case(3)
        call HCB_SMC(ISPGRP,HT,DBH,SCCFL,SBA,XSI_1,XSI_2,OG,&
             &PHCB)
        PCR(I) = 1.0-PHCB/HT
     case(4)
        call A_HCB_RAP(ISPGRP,HT,DBH,SCCFL,SBA,XSI_1,XSI_2,OG,&
             &PHCB)
        PCR(I) = 1.0-PHCB/HT
     end select
     TDATAR(I,3) = PCR(I)*CALIB(2,TDATAI(I,2))
     if (TDATAR(I,3) .gt.1.0) TDATAR(I,3) = 1.0
     if (TDATAR(I,3) .lt.0.05) TDATAR(I,3) = 0.05
  end do
  return
end

subroutine A_HCB_SWO(ISPGRP,HT,DBH,CCFL,BA,SI_1,SI_2,OG,HCB)
  implicit none
  ! Variables
  integer*4 :: ISPGRP
  real*4  :: HT,DBH,CCFL,BA,SI_1,SI_2,OG,HCB,HCBPAR(18,7),B0,B1,B2,B3,B4,B5,B6

  !  NEW HEIGHT TO CROWN BASE FOR ALL (UNDAMAGED AND DAMAGED) TREES
  !        (7 PARameters - all species)

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
       &1.990155033,  4.800089990,  2.024723585 ,  3.582314301,& !  DF,GW,PP,SP
       &3.127730861,  0.0        ,  4.49102006  ,  0.0        ,& !  IC,WH,RC,PY
       &3.271130882,  0.387912505,  0.4488479442,  1.285465907,& !  MD,GC,TA,CL
       &1.000364090,  1.05786632 ,  2.672850866 ,  0.56713781 ,& !  BL,WO,BO,RA
       &0.0        ,  0.0        ,& !  PD,WI

       &-0.008180786,  0.0        , -0.001953589 , -0.003256792,& !  DF,GW,PP,SP
       &-0.004386780,  0.0        ,  0.0         ,  0.0        ,& !  IC,WH,RC,PY
       &0.0        , -0.015000868, -0.009375810 , -0.024459278,& !  MD,GC,TA,CL
       &-0.010636441,  0.0        ,  0.0         , -0.010377976,& !  BL,WO,BO,RA
       &0.0        ,  0.0        ,& !  PD,WI

       &-0.004696095, -0.003268539, -0.001837480 ,  0.0        ,& !  DF,GW,PP,SP
       &-0.003557122,  0.0        , -0.00132412  ,  0.0        ,& !  IC,WH,RC,PY
       &0.0        , -0.004098099, -0.001822050 , -0.003992574,& !  MD,GC,TA,CL
       &-0.005950398, -0.00183283 , -0.001400851 , -0.002066036,& !  BL,WO,BO,RA
       &-0.004842962, -0.004842962,& !  PD,WI

       &-0.392033240, -0.858744969, -0.568909853 , -0.765250973,& !  DF,GW,PP,SP
       &-0.637929879,  0.0        , -1.01460531  ,  0.0        ,& !  IC,WH,RC,PY
       &-0.841331291,  0.0        ,  0.0         ,  0.0        ,& !  MD,GC,TA,CL
       &0.0        , -0.28644547 , -0.605971926 ,  0.0        ,& !  BL,WO,BO,RA
       &-0.567987126, -0.567987126,& !  PD,WI

       &1.945708371,  0.0        ,  4.831886553 ,  3.043845568,& !  DF,GW,PP,SP
       &0.977816058,  3.246352823,  0.0         ,  1.225564582,& !  IC,WH,RC,PY
       &1.791699815,  2.104871164,  0.0         ,  0.0        ,& !  MD,GC,TA,CL
       &0.0        ,  0.0        ,  0.0         ,  1.39796223 ,& !  BL,WO,BO,RA
       &0.0        ,  0.0        ,& !  PD,WI

       &0.007854260,  0.0        ,  0.001653030 ,  0.0        ,& !  DF,GW,PP,SP
       &0.005850321,  0.0        ,  0.01340624  ,  0.0        ,& !  IC,WH,RC,PY
       &0.0        ,  0.0        ,  0.0         ,  0.0        ,& !  MD,GC,TA,CL
       &0.0        ,  0.0        ,  0.0         ,  0.0        ,& !  BL,WO,BO,RA
       &0.0281315332, 0.0281315332,& !  PD,WI

       &0.295593583,  0.275679490,  0.0         ,  0.0        ,& !  DF,GW,PP,SP
       &0.257070387,  0.0        ,  0.0         ,  0.0        ,& !  IC,WH,RC,PY
       &0.927163029,  0.352773356,  0.233233237 ,  0.0        ,& !  MD,GC,TA,CL
       &0.310672769,  0.0        ,  0.430988703 ,  0.0        ,& !  BL,WO,BO,RA
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

subroutine A_HCB_RAP(ISPGRP,HT,DBH,CCFL,BA,SI_1,SI_2,OG,HCB)
  implicit none
  ! Variables
  integer*4 :: ISPGRP
  real*4  :: HT,DBH,CCFL,BA,SI_1,SI_2,OG,HCB,HCBPAR(7,8),B0,B1,B2,B3,B4,B5,B6,K,SITE

  !  HEIGHT TO CROWN BASE (UNDAMAGED AND DAMAGED) TREES
  !     (7 parameters - all species)

  !     RA Coefficients from Hann, Bluhm, and Hibbs Red Alder Plantation Analysis
  !     DF Coefficients from Hann and Hanus (2004) FS 34: 1193-2003
  !     WH Coefficients from Johnson (2002) Willamette Industries Report
  !     RC Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #2
  !     BL Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
  !     PD Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29
  !     WI Coefficients from Hanus, Hann, and Marshall (2000) FRL Research Contribution 29

  data HCBPAR/& !
       &3.98915507  ,  6.18464679 ,  1.92682    ,  4.49102006 ,& !  RA,DF,WH,RC
       &0.9411395642,  0.0        ,  0.0        ,& !  BL,PD,WI

       &-0.019280895 , -0.00328764 , -0.00280478 ,  0.0        ,& !  RA,DF,WH,RC
       &-0.00768402  ,  0.0        ,  0.0        ,& !  BM,PD,WI

       &-0.0017632543, -0.00136555 , -0.0011939  , -0.00132412 ,& !  RA,DF,WH,RC
       &-0.005476131 , -0.005666559, -0.005666559,& !  BL,PD,WI

       &-1.1178816   , -1.19702220 , -0.513134   , -1.01460531 ,& !  RA,DF.WH,RC
       &0.0         , -0.745540494, -0.745540494,& !  BL,PD,WI

       &7.12804469  ,  3.17028263 ,  3.68901    ,  0.0        ,& !  RA,DF,WH,RC
       &0.0         ,  0.0        ,  0.0        ,& !  BL,PD,WI

       &0.0240273988,  0.0        ,  0.00742219 ,  0.01340624 ,& !  RA,DF,WH,RC
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
        SITE = (0.480 +( 1.110 * (SI_2+4.5)))-4.5
     end if
     HCB = HT/(1.0+EXP(B0+B1*HT+B2*CCFL+B3*ALOG(BA)+B4*(DBH/HT)&
          &+B5*SITE+B6*OG**2))
  end if
  return
end

subroutine HS_H40(HT40,ISP,AGE,SI)

  !     Hann, D.W. and J.A. Scrivani.  1987.  Dominanat-height-growth and
  !     site-index equations for Douglas-fir and ponderosa pine in
  !     Southwest Oregon.  Oregon State University, Forest Research
  !     Laboratory.  Research Bulletin 59.  13p.   Douglas-fir

  implicit none
  ! Variables
  real*4  :: AGE,HT40,X1,X2,SI,S
  integer*4 :: ISP

  S = SI-4.5
  if (ISP .eq.1) then
     X1 = 1.0-EXP(-EXP(-6.21693 + 0.281176*ALOG(S)&
          &+ 1.14354*ALOG(AGE)))
     X2 = 1.0-EXP(-EXP(-6.21693 + 0.281176*ALOG(S)&
          &+ 1.14354*ALOG(50.0)))
  else if (ISP .eq.2) then
     X1 = 1.0-EXP(-EXP(-6.54707 + 0.288169*ALOG(S)&
          &+ 1.21297*ALOG(AGE)))
     X2 = 1.0-EXP(-EXP(-6.54707 + 0.288169*ALOG(S)&
          &+ 1.21297*ALOG(50.0)))
  end if
  HT40 = 4.5 + S*(X1/X2)

  return
end

subroutine B_H40(HT40,AGE,SI)

  !     Bruce, D.  1981.  Consistent height-growth and growth-rate
  !     estimates for remeasured plots.  Forest Science 27:711-725.

  implicit none
  ! Variables
  real*4  :: AGE,HT40,SI,A1,A2

  A2 = -0.447762 - 0.894427*SI/100.0 + 0.793548*(SI/100.0)**2&
       &-0.17166*(SI/100.0)**3
  A1 = LOG(4.5/SI)/((13.25 - (SI/20.0))**A2 - (63.25 -&
       &(SI/20.0))**A2)
  HT40 = SI*EXP(A1*((AGE + 13.25 - (SI/20.0))**A2&
       &- (63.25 - (SI/20.0))**A2))

  return
end

subroutine F_H40(HT40,AGE,SI)

  !     Top height (option 2) and site curves (option 1) for western
  !     hemlock by J. Flewelling (unpublished).  Note, these are metric
  !     curves.  This subroutine requires the following subroutines:
  !       SITECV_F   computes top height from site and age
  !       SITEF_C    computes model parameters
  !       SITEF_SI   calculates an approximate psi for a given site

  implicit none
  ! Variables
  real*4  :: AGE,SI,HT40,HTOP,XSI
  real*4  :: ASPEC,AEQN,ATYPE,BAGE,MINA,MAXA,MINSI,MAXSI,REGION

  common /INFO/ASPEC,AEQN,ATYPE,BAGE,MINA,MAXA,MINSI,MAXSI,REGION

  XSI = SI*0.3048
  call SITECV_F_EDIT(XSI,AGE,HTOP)
  HT40 = HTOP*3.2808
  return
end

  !      SUBROUTINE NCH40(H40,AGE,SI)
  !C
  !C     NIGH AND COURTIN (1998) RED ALDER
  !C
  !      IMPLICIT NONE
  !      REAL*4 SI,AGE,H40,MSI,MH40
  !      MSI=SI/3.28
  !      MH40=1.3+((1.693*(MSI-1.3))/(1.0+EXP(3.600-1.240*ALOG(AGE-0.5))))
  !      H40=3.28*MH40
  !      RETURN
  !      END

subroutine WHHLB_SI_UC_EDIT(SI_C,PDEN,SI_UC)

  !     UNCORRECTS THE DENSITY INPACT UPON THE WEISKITTEL, HANN, HIBBS, LAM, AND BLUHN
  !          SITE INDEX FOR RED ALDER

  implicit none
  ! Variables
  real*4  :: SI_C,PDEN,SI_UC

  !     SITE INDEX UNCORRECTED FOR DENSITY EFFECT

  SI_UC = SI_C*(1.0-0.36979789*EXP(-0.00042264*PDEN**1.5))
  return
end

subroutine WHHLB_H40_EDIT(H40M,TAGEM,TAGEP,PH40P)

  !     WEISKITTEL, HANN, HIBBS, LAM, AND BLUHM DOMINANT HEIGHT GROWTH EQUATION FOR
  !     RED ALDER

  implicit none
  ! Variables
  real*4  :: H40M,TAGEM,TAGEP,PH40P
  real*4  :: B1,B2
  B1 = -4.481266
  B2 = -0.658884
  PH40P = H40M*EXP(B1*(TAGEP**B2-TAGEM**B2))
  return
end

subroutine SITECV_F_EDIT(si,age,htop)

  !     Purpose:  Implements new height-increment methods (F)

  !     Current Date: FEB 2, 1994    J. FLEWELLING

  !     SI    IN     R*4    Site index (m) (basis is BH AGE 50)
  !     AGE   IN     R*4    Breast height age (.= 1.0)
  !     HTOP  OUT    R*4    Top height (== site height) (m)

  ! Variables
  real*4  :: SI,AGE,HTOP,OLD_SI,SI_2,yk,alpha,beta,c,b1,xk,psi,x,z
  integer*4 :: h1

  save OLD_SI

  common /sitefprm/xk, b1, c , beta,alpha, h1, yk, SI_2

  !                           determine if coefficients for this SI are
  !                          already in siteprm. If not, get them.

  if (SI.ne. old_si) then
     old_si = si
     call sitef_si_EDIT( si, psi)
     call sitef_C_EDIT ( psi)
  end if

  !                                         apply height-age equation.
  x = age-1
  if (x.lt.xk) then
     htop = h1 + SI_2*x +&
          &(1-b1)*SI_2*xk/(c+1) * (((xk-x)/xk)**(c+1)-1)
  else
     z = x -xk
     htop = yk&
          &+alpha*(1-exp(-beta*z))
  end if
  return
end

subroutine SITEF_C_EDIT(psi)

  !     Purpose:  For a specified psi, calculates all of the height-age
  !               model parameters, and stores them in /sitefprm/

  !     Current Date: FEB 2, 1994    J. FLEWELLING

  !     psi     input   REAL     productivity index (m/yr)

  ! Variables
  real*4  :: psi,fp,SI_2,yk,alpha,beta,c,b1,xk
  integer*4 :: i,h1
  dimension fp(10)
  common /sitefprm/xk, b1, c ,  beta,alpha, h1, yk, SI_2
  !                                                   R24 Coefficients
  data (fp(i),i = 1,5),fp(9)  /    128.326    ,    -2.54871 ,&
       &5.33208, -9.00622 , 1.2, 52.7948 /

  SI_2 = psi
  xk = fp(1) * exp( fp(2) * psi)
  b1 = 0.2 + 0.8/ ( 1 + exp(fp(3) + fp(4)*psi))
  c = 1.0 + fp(5)* psi
  alpha = fp(9)*psi
  h1 = INT(1.3 + (b1*psi)/2.)
  yk = h1 + psi * xk* ( 1.0 - (1.-b1)/(c+1))

  beta = psi/alpha

  return
end

subroutine SITEF_SI_EDIT( SI, PSI)

  !     Purpose:  Calculates an approximate psi for a given site index
  !               Ref 'Model Fitting: top height increment', Feb 2, 1994.

  !     Current Date: FEB 2, 1994    J. FLEWELLING and A. ZUMRAWI

  !          si      input  r*4    site index (top height at BH age 50)
  !          psi     output r*4    site productivity parameter.

  ! Variables
  real*4  :: SI,PSI,b,si_piv,x
  integer*4 :: i,j
  dimension b(6,2)
  data si_piv  / 32.25953/
  data (b(i,1),i = 1,6)/ .299720, .116875, .074866, .032348,&
       &.006984, .000339 /
  data (b(i,2),i = 1,6)/ .290737, .129665, -.058777,&
       &-.000669, .006003, -.001060 /

  if (si.le. si_piv) then
     J = 1
  else
     J = 2
  end if

  x = (si - si_piv)/10.0

  psi = 0.75 + x*( b(1,j) + x*( b(2,j)  + x*( b(3,j)&
       &+ x*( b(4,j) + x*( b(5,j)  + x*b(6,j)   )))))
  return
end

subroutine DFORTY(VERSION,NTREES,NPTS,IB,TDATAI,TDATAR,D40)
  !     DETERMINE DIAMETER OF THE FORTY LARGEST BIG-6 TREES PER ACRE

  implicit none
  ! Variables
  integer*4 :: VERSION,NTREES,NPTS,IB,TDATAI(2000,3),I,ID,IIB
  real*4  :: TDATAR(2000,4),TOTD,TOTTR,DCL(1000),TRCL(1000),D40,TRDIFF,EXPAN

  TOTD = 0.0
  TOTTR = 0.0
  do I = 1,1000
     DCL(I) = 0.0
     TRCL(I) = 0.0
  end do
  IIB = IB
  if (VERSION .ge.4) IIB = 1
  do I = 1,NTREES
     if (TDATAR(I,1) .le.0.0) cycle
     if (TDATAI(I,2) .le. IIB) then
        ID = IFIX(TDATAR(I,1)*10.0)
        if (ID.gt.1000) ID = 1000
        EXPAN = TDATAR(I,4)/FLOAT(NPTS)
        DCL(ID) = DCL(ID)+TDATAR(I,1)*EXPAN
        TRCL(ID) = TRCL(ID)+EXPAN
     end if
  end do
  do I = 1000,1,-1
     TOTD = TOTD+DCL(I)
     TOTTR = TOTTR+TRCL(I)
     if (TOTTR.gt.40.0) then
        TRDIFF = TRCL(I)-(TOTTR-40.0)
        TOTD = TOTD-DCL(I)+((DCL(I)/TRCL(I))*TRDIFF)
        TOTTR = 40.0
        exit
     end if
  end do
  if (TOTTR.gt.0.0) then
     D40 = TOTD/TOTTR
  else
     D40 = 0.0
  end if
  return
end

subroutine SPMIX(NTREES,NPTS,TDATAI,TDATAR,PDF,PTF,PPP,PWH,PRA)
  implicit none
  ! Variables
  integer*4 :: NTREES,NPTS,TDATAI(2000,3),I
  real*4  :: TDATAR(2000,4),BA,BATOT,BADF,BATF,BAPP,BAWH,BARA,PDF,PTF,PPP,PWH,PRA
  BATOT = 0.0
  BADF = 0.0
  BATF = 0.0
  BAPP = 0.0
  BAWH = 0.0
  BARA = 0.0
  PDF = 0.0
  PTF = 0.0
  PPP = 0.0
  PWH = 0.0
  PRA = 0.0
  do I = 1,NTREES
     BA = (TDATAR(I,1)**2*TDATAR(I,4))*0.005454154/NPTS
     BATOT = BATOT+BA
     if (TDATAI(I,1) .eq.15.or. TDATAI(I,1) .eq.17) then
        BATF = BATF+BA
     else if (TDATAI(I,1) .eq.122) then
        BAPP = BAPP+BA
     else if (TDATAI(I,1) .eq.202) then
        BADF = BADF+BA
     else if (TDATAI(I,1) .eq.263) then
        BAWH = BAWH+BA
     else if (TDATAI(I,1) .eq.351) then
        BARA = BARA+BA
     end if
  end do
  if (BATOT .gt.0.0) then
     PDF = BADF/BATOT
     PTF = BATF/BATOT
     PPP = BAPP/BATOT
     PWH = BAWH/BATOT
     PRA = BARA/BATOT
  end if
  return
end

subroutine HD40_SWO(IEQ,HT40,D40,DBH,PTRHT)
  implicit none
  ! Variables
  integer*4 :: IEQ
  real*4  :: HT40,D40,DBH,PTRHT,B0,B1,B2,EXD,EXD40,HD40PAR(3,3)

  !    HEIGHT/DIAMETER USING D40 (3PARameters - Douglas-fir, true firs,
  !                               ponderosa pine)
  !     DF Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
  !     GW Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
  !     PP Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
  !C
  data HD40PAR/&
       &-3.485635287, -4.376160718, -4.047994965,& !  DF,GW,PP
       &-0.255712209, -0.231693907, -0.135864020,& !  DF,GW,PP
       &-0.001555149, -0.001334070, -0.005647510/ !  DF,GW,PP

  B0 = HD40PAR(IEQ,1)
  B1 = HD40PAR(IEQ,2)
  B2 = HD40PAR(IEQ,3)
  EXD = EXP(B0*DBH**(B1+B2*(HT40-4.5)))
  EXD40 = EXP(B0*D40**(B1+B2*(HT40-4.5)))
  PTRHT = 4.5+(HT40-4.5)*(EXD/EXD40)
  return
end

subroutine HD40_NWO(IEQ,HT40,D40,DBH,PTRHT)
  implicit none
  ! Variables
  integer*4 :: IEQ
  real*4  :: HT40,D40,DBH,PTRHT,B0,B1,B2,EXD,EXD40,HD40PAR(2,3)

  !    HEIGHT/DIAMETER USING D40 (3PARameters - Douglas-fir and Western Hemlock)

  !     DF Coefficients from Hanus, Marshall, and Hann (1999) FRL Research Contribution 25
  !     WH Coefficients from Hanus, Marshall, and Hann (1999) FRL Research Contribution 25

  data HD40PAR/&
       &-2.857232223, -2.790360488,& !  DF,WH
       &-0.393885195, -0.235470605,& !  DF,WH
       &-0.000521583, -0.002374673/ !  DF,WH

  B0 = HD40PAR(IEQ,1)
  B1 = HD40PAR(IEQ,2)
  B2 = HD40PAR(IEQ,3)
  EXD = EXP(B0*DBH**(B1+B2*(HT40-4.5)))
  EXD40 = EXP(B0*D40**(B1+B2*(HT40-4.5)))
  PTRHT = 4.5+(HT40-4.5)*(EXD/EXD40)
  return
end

subroutine HD40_SMC(IEQ,HT40,D40,DBH,PTRHT)
  implicit none
  ! Variables
  integer*4 :: IEQ
  real*4  :: HT40,D40,DBH,PTRHT,B0,B1,B2,EXD,EXD40,HD40PAR(2,3)

  !    HEIGHT/DIAMETER USING D40 (3PARameters - Douglas-fir and western hemlock)

  !     DF Coefficients from Hanus, Marshall, and Hann (1999) FRL Research Contribution 25
  !     WH Coefficients from Hanus, Marshall, and Hann (1999) FRL Research Contribution 25

  data HD40PAR/&
       &-2.857232223, -2.790360488,& !  DF,WH
       &-0.393885195, -0.235470605,& !  DF,WH
       &-0.000521583, -0.002374673/ !  DF,WH

  B0 = HD40PAR(IEQ,1)
  B1 = HD40PAR(IEQ,2)
  B2 = HD40PAR(IEQ,3)
  EXD = EXP(B0*DBH**(B1+B2*(HT40-4.5)))
  EXD40 = EXP(B0*D40**(B1+B2*(HT40-4.5)))
  PTRHT = 4.5+(HT40-4.5)*(EXD/EXD40)
  return
end

subroutine HD40_RAP(IEQ,HT40,D40,DBH,PTRHT)
  implicit none
  ! Variables
  integer*4 :: IEQ
  real*4  :: HT40,D40,DBH,PTRHT,B0,B1,B2,EXD,EXD40,HD40PAR(3,3)

  !     HEIGHT/DIAMETER USING D40 (3PARameters - Red Alder, Douglas-fir and
  !     western hemlock)

  !     RA Coefficients from Hann, Bluhm, and Hibbs (2011) Forest Biometrics Research Paper 1
  !     DF Coefficients from Hanus, Marshall, and Hann (1999) FRL Research Contribution 25
  !     WH Coefficients from Hanus, Marshall, and Hann (1999) FRL Research Contribution 25

  data HD40PAR/&
       &-1.7477875  , -2.857232223, -2.790360488,& !  RA,DF,WH
       &-0.40004105 , -0.393885195, -0.235470605,& !  RA,DF,WH
       &-0.00497111 , -0.000521583, -0.002374673/ !  RA,DF,WH

  B0 = HD40PAR(IEQ,1)
  B1 = HD40PAR(IEQ,2)
  B2 = HD40PAR(IEQ,3)
  EXD = EXP(B0*DBH**(B1+B2*(HT40-4.5)))
  EXD40 = EXP(B0*D40**(B1+B2*(HT40-4.5)))
  PTRHT = 4.5+(HT40-4.5)*(EXD/EXD40)
  return
end

subroutine GET_CCFL_EDIT(DBH,CCFLL1,CCFL1,CCFL)
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

subroutine CALTST(YXS,XSS,YSS,N,BETA)

  !     COMPUTE CALIBRATION BETA AND DETERMINE IF IT IS SIGNIFICANT FROM
  !     ONE

  implicit none
  ! Variables
  integer*4 :: N,IDEGF
  real*4  :: BETA,VARBETA,DEGF,MSE,TTEST,YXS,XSS,YSS,TVAL(34),CRITVAL
  data TVAL/63.657, 9.925, 5.841, 4.604, 4.032, 3.707, 3.499, 3.355,&
       &3.250, 3.169, 3.106, 3.055, 3.012, 2.977, 2.947, 2.921,&
       &2.898, 2.878, 2.861, 2.845, 2.831, 2.819, 2.807, 2.797,&
       &2.787, 2.779, 2.771, 2.763, 2.756, 2.750, 2.704, 2.660,&
       &2.617, 2.576/
  BETA = YXS/XSS
  IDEGF = N-1
  DEGF = FLOAT(IDEGF)
  MSE = (YSS-2.0*BETA*YXS+BETA**2*XSS)/DEGF
  VARBETA = MSE/XSS
  if (VARBETA.gt.0.0) then
     TTEST = ABS(BETA-1.0)/SQRT(VARBETA)
     if (IDEGF.le.30) then
        CRITVAL = TVAL(IDEGF)
     else if (IDEGF.le.40) then
        CRITVAL = TVAL(30)+(TVAL(31)-TVAL(30))*((DEGF-30.0)/10.0)
     else if (IDEGF.le.60) then
        CRITVAL = TVAL(31)+(TVAL(32)-TVAL(31))*((DEGF-40.0)/20.0)
     else if (IDEGF.le.120) then
        CRITVAL = TVAL(32)+(TVAL(33)-TVAL(32))*((DEGF-60.0)/60.0)
     else if (IDEGF.le.1000) then
        CRITVAL = TVAL(33)+(TVAL(34)-TVAL(33))*((DEGF-120.0)/880.0)
     else
        CRITVAL = TVAL(34)
     end if
     if (TTEST.lt.CRITVAL) then
        BETA = 1.0
     else
        if (BETA .gt.2.0) then
           BETA = 2.0
        else if (BETA .lt.0.5) then
           BETA = 0.5
        end if
     end if
  else
     if (BETA .gt.2.0) then
        BETA = 2.0
     else if (BETA .lt.0.5) then
        BETA = 0.5
     end if
  end if
  return
end
