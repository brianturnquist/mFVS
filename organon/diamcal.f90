!=======================!
!   organon : diamcal   !
!=======================!
! ORGANON $Id$

  !      ORGANON GROWTH AND YIELD MODEL
  !     SUBROUTINES INCLUDED:
  !        DGCALIB
  !        DIAMGRO
  !        PHTS
  !        MORTAL
  !        DIB_SWO
  !        DIB_NWO
  !        DIB_SMC
  !        DIB_RAP
  !        DOB_SWO
  !        DOB_NWO
  !        DOB_SMC
  !        DOB_RAP
  !        SSUM
  !        OLDGROWTH
  !        GET_BAL

  !  04/25/2014 - THERE ARE COMMON SUBROUTINE NAMES IN THE SOURCE CODE
  !               USED TO BUILD THE ORGANON DLLS. IN ORDER TO LINK THE
  !               ORGANON SOURCE CODE WITH THE FVS SOURCE CODE WE CHANGED
  !               THE DUPLICATED SUBROUTINE NAMES TO MAKE THEM UNIQUE.

  !  CHANGED THE NAME OF SUBROUTINE CALL - DG_NWO DG_NWO_RUN, DG_NWO_RUN IS
  !                                        IN THE DIAGRO ROUTINE, DG_NWO WAS IN
  !                                        EDITDLL\COMFILES.FOR

subroutine DGCALIB(VERSION,IB,NSPN,NPTS,NTREES,RADIN,TDATAI,&
     &RAD,BAL2,BALL2,OG,TDATAR,GROWTH,SI_1,SI_2,&
     &RADGRO,SBA2,STDATAR,PDG,CALIB)

  !     CALCULATION OF DIAMETER GROWTH CALIBRATION

  implicit none
  ! Variables
  integer*4 :: IB,NSPN,NTREES,NPTS,RADIN(18),TDATAI(2000,3),VERSION

  real*4  :: BAL2(500),BALL2(51),CALIB(3,18),SI_1,GROWTH(2000),OG,PDG(2000),SI_2,RADGRO(2000),SBA2,&
       &STDATAR(2000,4),TDATAR(2000,4)
  real*4  :: AVRAT,BETA,DG,DBH1,DBH2,DIB(2000),DIBCK,PM,RATIO(18),SDG,SPDG,SUM1,SUM2,SUM3,SUM4,YSS,&
       &POW(2000),XPM,PS
  real*4  :: XSI_1,XSI_2,PMK(2000),BAL1(500),BALL1(51),CCFL1(500),CCFLL1(51),SBA1
  integer*4 :: I,J,ISPGRP
  logical*2 :: RAD
  AVRAT = 0.
  SUM3 = 0.0
  SUM4 = 0.0

  !     PRESET ALL DIAMETER CALIBRATIONS TO 1.0

  do I = 1,18
     CALIB(3,I) = 1.0
  end do
  XSI_1 = SI_1-4.5
  XSI_2 = SI_2-4.5

  !     IF NO RADIAL GROWTHS -- SKIP CALIB
  if (.not. RAD) return

  !     PREDICT PREVIOUS 5-YR DIAMETER

  do I = 1,NTREES
     call DIAMGRO(VERSION,I,TDATAI,TDATAR,XSI_1,XSI_2,SBA2,&
          &BALL2,BAL2,PDG)
  end do
  do I = 1,18
     RATIO(I) = 1.0
     if (RADIN(I).le.0) cycle
     SUM1 = 0.0
     SUM2 = 0.0
     do J = 1,NTREES
        if (TDATAI(J,2) .ne. I) cycle
        DBH2 = TDATAR(J,1)
        ISPGRP = TDATAI(J,2)
        select case(VERSION)
        case(1)
           call DIB_SWO(ISPGRP,DBH2,DIB(J))
        case(2)
           call DIB_NWO(ISPGRP,DBH2,DIB(J))
        case(3)
           call DIB_NWO(ISPGRP,DBH2,DIB(J))
        case(4)
           call DIB_RAP(ISPGRP,DBH2,DIB(J))
        end select
        if (RADGRO(J) .gt.0.) then
           DIBCK = DIB(J)-2.0*RADGRO(J)
           if (DIBCK.gt.0.0) then
              select case(VERSION)
              case(1)
                 call DOB_SWO(ISPGRP,DIBCK,DBH1)
              case(2)
                 call DOB_NWO(ISPGRP,DIBCK,DBH1)
              case(3)
                 call DOB_NWO(ISPGRP,DIBCK,DBH1)
              case(4)
                 call DOB_RAP(ISPGRP,DIBCK,DBH1)
              end select
              if (DBH1.lt.0.1) DBH1 = 0.1
           else
              DBH1 = 0.1
           end if
           DG = DBH2-DBH1
           SUM1 = SUM1+DG
           SUM2 = SUM2+PDG(J)
           STDATAR(J,1) = DBH2-DG
           if (STDATAR(J,1) .lt.0.1)STDATAR(J,1) = 0.1
           GROWTH(J) = DG
        end if
     end do
     RATIO(I) = SUM1/SUM2
     SUM3 = SUM3+SUM1
     SUM4 = SUM4+SUM2
  end do
  AVRAT = SUM3/SUM4
  do I = 1,18
     if (RADIN(I) .le.5) then
        RATIO(I) = AVRAT
     end if
  end do
  do J = 1,NTREES
     I = TDATAI(J,2)
     if (RADGRO(J) .le.0.0) then
        STDATAR(J,1) = TDATAR(J,1)-RATIO(I)*PDG(J)
        if (STDATAR(J,1) .lt.0.1)STDATAR(J,1) = 0.1
        GROWTH(J) = TDATAR(J,1)-STDATAR(J,1)
     end if
  end do

  !     PREDICT PREVIOUS 5-YR HEIGHTS

  call PHTS(VERSION,NTREES,TDATAI,TDATAR,STDATAR,CALIB)

  !        PREDICT PREVIOUS 5-YR CROWN RATIO

  do I = 1,NTREES
     STDATAR(I,3) = (TDATAR(I,3)*TDATAR(I,2))/STDATAR(I,2)
     if (STDATAR(I,3) .gt.1.0) STDATAR(I,3) = 1.0
  end do

  !     CALCULATE PREVIOUS 5-YR STAND AND TREE STATISTICS

  !     PREDICT PREVIOUS 5-YR MORTALITY AND EXPANSION FACTOR

  call MORTAL(VERSION,NPTS,NTREES,IB,TDATAI,TDATAR,BALL2,BAL2,&
       &OG,XSI_1,XSI_2,POW,PMK)
  do I = 1,NTREES
     XPM = 1.0/(1.0+EXP(-PMK(I)))
     PS = (1.0-XPM)**POW(I)
     PM = 1.0-PS
     if (PM .gt.0.5)PM = 0.5
     STDATAR(I,4) = TDATAR(I,4)/(1.0-PM)
  end do

  !     GROW TREES WITH RADIAL GROWTH INPUT GIVING PREDICTED & ACTUAL
  !     TO DETERMINE CALIBRATION

  call SSUM(1,VERSION,NPTS,NTREES,TDATAI,TDATAR,STDATAR,SBA1,&
       &BAL1,BALL1,CCFL1,CCFLL1)
  do I = 1,NSPN
     SDG = 0.0
     SPDG = 0.0
     YSS = 0.0
     do J = 1,NTREES
        if (TDATAI(J,2) .ne. I .or. RADGRO(J) .le.0.0) cycle
        call DIAMGRO(VERSION,J,TDATAI,STDATAR,XSI_1,XSI_2,&
             &SBA1,BALL1,BAL1,PDG)
        SDG = SDG+GROWTH(J)
        SPDG = SPDG+PDG(J)
        YSS = YSS+(GROWTH(J)**2/PDG(J))
     end do
     if (RADIN(I) .lt.2) cycle
     call CALTST(SDG,SPDG,YSS,RADIN(I),BETA)
     CALIB(3,I) = BETA
  end do
  return
end

subroutine DIAMGRO(VERSION,K,TDATAI,XTDATAR,SI_1,SI_2,SBA,&
     &BALL,BAL,PDG)
  implicit none

  !     CALCULATES FIVE-YEAR DIAMETER GROWTH RATE OF THE K-TH TREE

  ! Variables
  integer*4 :: VERSION,K,TDATAI(2000,3),ISP,ISPGRP
  real*4  :: XTDATAR(2000,4),SI_1,SI_2,SBA,BALL(51),BAL(500),PDG(2000),DBH,CR,DG,SBAL,SITE

  !     CALCULATE BASAL AREA IN LARGER TREES

  DBH = XTDATAR(K,1)
  CR = XTDATAR(K,3)
  ISP = TDATAI(K,1)
  ISPGRP = TDATAI(K,2)
  call GET_BAL(DBH,BALL,BAL,SBAL)
  select case(VERSION)
  case(1)
     SITE = SI_1
  case(2,3)
     if (ISP .eq.263) then
        SITE = SI_2
     else
        SITE = SI_1
     end if
  case(4)
     if (ISP .eq.351) then
        SITE = SI_1
     else
        SITE = SI_2
     end if
  end select

  !     CALCULATE DIAMETER GROWTH RATE FOR UNTREATED TREES

  select case(VERSION)
  case(1)
     call DG_SWO(ISPGRP,DBH,CR,SITE,SBAL,SBA,DG)
  case(2)
     call DG_NWO_RUN(ISPGRP,DBH,CR,SITE,SBAL,SBA,DG)
  case(3)
     call DG_SMC(ISPGRP,DBH,CR,SITE,SBAL,SBA,DG)
  case(4)
     call DG_RAP(ISPGRP,DBH,CR,SITE,SBAL,SBA,DG)
  end select
  PDG(K) = DG
  return
end

subroutine PHTS(VERSION,NTREES,TDATAI,TDATAR,STDATAR,CALIB)
  implicit none
  ! Variables
  integer*4 :: VERSION,NTREES,TDATAI(2000,3),I,ISPGRP
  real*4  :: TDATAR(2000,4),STDATAR(2000,4),CALIB(3,18),PRSH,PRH,CPRSH,CPRH
  do I = 1,NTREES
     ISPGRP = TDATAI(I,2)
     select case(VERSION)
     case(1)
        call A_HD_SWO(ISPGRP,STDATAR(I,1),PRSH)
        call A_HD_SWO(ISPGRP,TDATAR(I,1),PRH)
     case(2)
        call HD_NWO(ISPGRP,STDATAR(I,1),PRSH)
        call HD_NWO(ISPGRP,TDATAR(I,1),PRH)
     case(3)
        call HD_SMC(ISPGRP,STDATAR(I,1),PRSH)
        call HD_SMC(ISPGRP,TDATAR(I,1),PRH)
     case(4)
        call A_HD_RAP(ISPGRP,STDATAR(I,1),PRSH)
        call A_HD_RAP(ISPGRP,TDATAR(I,1),PRH)
     end select
     CPRSH = 4.5+CALIB(1,ISPGRP)*(PRSH-4.5)
     CPRH = 4.5+CALIB(1,ISPGRP)*(PRH-4.5)
     STDATAR(I,2) = (CPRSH/CPRH)*TDATAR(I,2)
     if (STDATAR(I,2) .lt.4.5)STDATAR(I,2) = 4.5
  end do
  return
end

subroutine MORTAL(VERSION,NPTS,NTREES,IB,TDATAI,TDATAR,BALL,&
     &BAL,OG,SI_1,SI_2,POW,PMK)
  !     ROUTINE FOR SETTING TREE MORTALITY

  implicit none
  ! Variables
  integer*4 :: VERSION,NPTS,NTREES,IB,TDATAI(2000,3),I,ISPGRP
  real*4  :: TDATAR(2000,4),BALL(51),OG,BAL(500),SI_1,SI_2,PMK(2000),DBH,HT,SBAL,CR,POW(2000)
  integer :: IDANUW

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  IDANUW = IB
  IDANUW = NPTS

  do I = 1,NTREES
     PMK(I) = 0.0
  end do

  !  INDIVIDUAL TREE MORTALITY EQUATIONS

  do I = 1,NTREES
     if (TDATAR(I,4) .le.0.) cycle
     ISPGRP = TDATAI(I,2)
     DBH = TDATAR(I,1)
     HT = TDATAR(I,2)
     CR = TDATAR(I,3)
     call GET_BAL(DBH,BALL,BAL,SBAL)
     select case(VERSION)
     case(1)
        call PM_SWO(ISPGRP,DBH,CR,SI_1,SBAL,OG,POW(I),PMK(I))
     case(2)
        call PM_NWO(ISPGRP,DBH,CR,SI_1,SI_2,SBAL,POW(I),PMK(I))
     case(3)
        call PM_SMC(ISPGRP,DBH,CR,SI_1,SI_2,SBAL,POW(I),PMK(I))
     case(4)
        call PM_RAP(ISPGRP,DBH,CR,SI_1,SI_2,SBAL,POW(I),PMK(I))
     end select
  end do
  return
end

subroutine DIB_SWO(ISPGRP,DOB,DIB)
  implicit none
  ! Variables
  real*4  :: DOB,DIBPAR(18,2),B0,B1,DIB
  integer*4 :: ISPGRP

  !  DIAMETER INSIDE BARK (2 parameters - all species)

  data DIBPAR/&
       &0.903563 , 0.904973 , 0.809427 , 0.859045 , 0.837291 ,& !  DF,GW,PP,SP,IC,
       &0.933707 , 0.9497   , 0.97     , 0.96317  , 0.94448  ,& !  WH,RC,PY,MD,GC,
       &0.859151 , 0.910499 , 0.97059  , 0.878457 , 0.889703 ,& !  TA,CL,BL,WO,BO,
       &0.947    , 0.94448  , 0.94448  ,& !  RA,PD,WI

       &0.989388 , 1.0      , 1.016866 , 1.0      , 1.0      ,& !  DF,GW,PP,SP,IC,
       &1.0      , 1.0      , 1.0      , 1.0      , 0.9875170,& !  WH,RC,PY,MD,GC,
       &1.0178109, 1.01475  , 0.993585 , 1.02393  , 1.0104062,& !  TA,CL,BL,WO,BO,
       &1.0      , 0.9875170, 0.9875170/ !  RA,PD,WI

  B0 = DIBPAR(ISPGRP,1)
  B1 = DIBPAR(ISPGRP,2)
  DIB = B0*DOB**B1
  return
end

subroutine DIB_NWO(ISPGRP,DOB,DIB)
  !     THE SMC VERSION USES THESE SAME EQUATIONS, SO IT ALSO CALLS THIS ROUTINE.
  implicit none
  ! Variables
  real*4  :: DOB,DIBPAR(11,2),B0,B1,DIB
  integer*4 :: ISPGRP

  !  DIAMETER INSIDE BARK (2 parameters - all species)

  data DIBPAR/&
       &0.971330, 0.904973, 0.933707, 0.9497  , 0.97    ,& !  DF,GF,WH,RC,PY
       &0.96317 , 0.97059 , 0.878457, 0.947   , 0.94448 ,& !  MD,BL,WO,RA,PD
       &0.94448 ,& !  WI

       &0.966365, 1.0     , 1.0     , 1.0     , 1.0     ,& !  DF,GF,WH,RC,PY
       &1.0     , 0.993585, 1.02393 , 1.0     , 0.987517,& !  MD,BL,WO,RA,PD
       &0.987517/ !  WI

  B0 = DIBPAR(ISPGRP,1)
  B1 = DIBPAR(ISPGRP,2)
  DIB = B0*DOB**B1
  return
end


subroutine DIB_RAP(ISPGRP,DOB,DIB)
  implicit none
  ! Variables
  real*4  :: DOB,DIBPAR(7,2),B0,B1,DIB
  integer*4 :: ISPGRP

  !  DIAMETER INSIDE BARK (2 parameters - all species)

  data DIBPAR/&
       &0.947   , 0.971330, 0.933707, 0.9497  , 0.97059 ,& !  RA,DF,WH,RC,BL
       &0.94448 , 0.94448 ,& !  PD,WI

       &1.0     , 0.966365, 1.0     , 1.0     , 0.993585,& !  RA,DF,WH,RC,BL
       &0.987517, 0.987517/ !  PD,WI

  B0 = DIBPAR(ISPGRP,1)
  B1 = DIBPAR(ISPGRP,2)
  DIB = B0*DOB**B1
  return
end

subroutine DOB_SWO(ISPGRP,DIB,DOB)
  implicit none
  ! Variables
  real*4  :: DIB,DIBPAR(18,2),B0,B1,DOB
  integer*4 :: ISPGRP

  !  DIAMETER INSIDE BARK (2 parameters - all species)

  data DIBPAR/&
       &0.903563 , 0.904973 , 0.809427 , 0.859045 , 0.837291 ,& !  DF,GW,PP,SP,IC,
       &0.933707 , 0.9497   , 0.97     , 0.96317  , 0.94448  ,& !  WH,RC,PY,MD,GC,
       &0.859151 , 0.910499 , 0.97059  , 0.878457 , 0.889703 ,& !  TA,CL,BL,WO,BO,
       &0.947    , 0.94448  , 0.94448  ,& !  RA,PD,WI

       &0.989388 , 1.0      , 1.016866 , 1.0      , 1.0      ,& !  DF,GW,PP,SP,IC,
       &1.0      , 1.0      , 1.0      , 1.0      , 0.9875170,& !  WH,RC,PY,MD,GC,
       &1.0178109, 1.01475  , 0.993585 , 1.02393  , 1.0104062,& !  TA,CL,BL,WO,BO,
       &1.0      , 0.9875170, 0.9875170/ !  RA,PD,WI

  B0 = DIBPAR(ISPGRP,1)
  B1 = DIBPAR(ISPGRP,2)
  DOB = (DIB/B0)**(1.0/B1)
  return
end

subroutine DOB_NWO(ISPGRP,DIB,DOB)
  !     THE SMC VERSION USES THESE SAME EQUATIONS, SO IT ALSO CALLS THIS ROUTINE.
  implicit none
  ! Variables
  real*4  :: DIB,DIBPAR(11,2),B0,B1,DOB
  integer*4 :: ISPGRP

  !  DIAMETER INSIDE BARK (2 parameters - all species)

  data DIBPAR/&
       &0.971330, 0.904973, 0.933707, 0.9497  , 0.97    ,& !  DF,GF,WH,RC,PY
       &0.96317 , 0.97059 , 0.878457, 0.947   , 0.94448 ,& !  MD,BL,WO,RA,PD
       &0.94448 ,& !  WI

       &0.966365, 1.0     , 1.0     , 1.0     , 1.0     ,& !  DF,GF,WH,RC,PY
       &1.0     , 0.993585, 1.02393 , 1.0     , 0.987517,& !  MD,BL,WO,RA,PD
       &0.987517/ !  WI

  B0 = DIBPAR(ISPGRP,1)
  B1 = DIBPAR(ISPGRP,2)
  DOB = (DIB/B0)**(1.0/B1)
  return
end


subroutine DOB_RAP(ISPGRP,DIB,DOB)
  implicit none
  ! Variables
  real*4  :: DIB,DIBPAR(7,2),B0,B1,DOB
  integer*4 :: ISPGRP

  !  DIAMETER INSIDE BARK (2 parameters - all species)

  data DIBPAR/&
       &0.947   , 0.971330, 0.933707, 0.9497  , 0.97059 ,& !  RA,DF,WH,RC,BL
       &0.94448 , 0.94448 ,& !  PD,WI

       &1.0     , 0.966365, 1.0     , 1.0     , 0.993585,& !  DF,DF,WH,RC,BL
       &0.987517, 0.987517/ !  PD,WI

  B0 = DIBPAR(ISPGRP,1)
  B1 = DIBPAR(ISPGRP,2)
  DOB = (DIB/B0)**(1.0/B1)
  return
end

subroutine SSUM(N,VERSION,NPTS,NTREES,TDATAI,TDATAR,STDATAR,&
     &SBA,BAL,BALL,CCFL,CCFLL)
  implicit none
  !     CALCULATE BEGINNING OR ENDING STAND STATISTICS

  !     N = START OR ENDING STATISTICS
  !       1 = START
  !       2 = END

  ! Variables
  integer*4 :: N,VERSION,NPTS,NTREES,TDATAI(2000,3)

  real*4  :: BAL(500),BALL(51),SBA,CCFL(500),CCFLL(51),STDATAR(2000,4),TDATAR(2000,4)

  real*4  :: HT,DBH,EX,BA,CCF,MCW
  integer*4 :: I,K,L,ISPGRP

  !     CALCULATE BAL, SBA AND CCFL

  SBA = 0.
  do I = 1,500
     CCFL(I) = 0.
     BAL(I) = 0.
  end do
  do I = 1,51
     CCFLL(I) = 0.
     BALL(I) = 0.
  end do
  do I = 1,NTREES
     ISPGRP = TDATAI(I,2)
     if (N .eq.1) then
        HT = STDATAR(I,2)
        DBH = STDATAR(I,1)
        EX = STDATAR(I,4)/FLOAT(NPTS)
     else
        HT = TDATAR(I,2)
        DBH = TDATAR(I,1)
        EX = TDATAR(I,4)/FLOAT(NPTS)
     end if
     if (EX .le.0.0) cycle
     BA = (DBH**2*EX)*0.005454154
     SBA = SBA+BA
     select case(VERSION)
     case(1)
        call MCW_SWO(ISPGRP,DBH,HT,MCW)
     case(2)
        call MCW_NWO(ISPGRP,DBH,HT,MCW)
     case(3)
        call MCW_SMC(ISPGRP,DBH,HT,MCW)
     case(4)
        call MCW_RAP(ISPGRP,DBH,HT,MCW)
     end select
     CCF = 0.001803*MCW**2*TDATAR(I,4)/NPTS
     if (DBH .gt.50.0) then
        L = INT(DBH-49.0)
        if (L.gt.52) L = 52
        do K = 1,500
           CCFL(K) = CCFL(K)+CCF
           BAL(K) = BAL(K)+BA
        end do
        do K = 1,L-1
           CCFLL(K) = CCFLL(K)+CCF
           BALL(K) = BALL(K)+BA
        end do
     else
        L = INT(DBH*10.+.5)
        do K = 1,L-1
           CCFL(K) = CCFL(K)+CCF
           BAL(K) = BAL(K)+BA
        end do
     end if
  end do
  return
end

subroutine OLDGROWTH(NPTS,NTREES,IB,TDATAI,TDATAR,OG)
  !     DETERMINE THE OLD GROWTH INDICATOR "OG"

  implicit none
  ! Variables
  integer*4 :: NPTS,IB,NTREES,TDATAI(2000,3)

  real*4  :: TDATAR(2000,4)

  integer*4 :: I,ID
  real*4  :: TOTHT,TOTD,TOTTR,HTCL(100),DCL(100),TRCL(100),HT5,DBH5,TRDIFF,HT,DBH,EXPAN,OG

  TOTHT = 0.0
  TOTD = 0.0
  TOTTR = 0.0
  do I = 1,100
     HTCL(I) = 0.0
     DCL(I) = 0.0
     TRCL(I) = 0.0
  end do
  do I = 1,NTREES
     if (TDATAI(I,2).le.IB) then
        HT = TDATAR(I,2)
        DBH = TDATAR(I,1)
        EXPAN = TDATAR(I,4)/FLOAT(NPTS)
        ID = IFIX(DBH)+1
        if (ID.gt.100) ID = 100
        HTCL(ID) = HTCL(ID)+HT*EXPAN
        DCL(ID) = DCL(ID)+DBH*EXPAN
        TRCL(ID) = TRCL(ID)+EXPAN
     end if
  end do
  do I = 100,1,-1
     TOTHT = TOTHT+HTCL(I)
     TOTD = TOTD+DCL(I)
     TOTTR = TOTTR+TRCL(I)
     if (TOTTR .gt.5.0) then
        TRDIFF = TRCL(I)-(TOTTR-5.0)
        TOTHT = TOTHT-HTCL(I)+((HTCL(I)/TRCL(I))*TRDIFF)
        TOTD = TOTD-DCL(I)+((DCL(I)/TRCL(I))*TRDIFF)
        TOTTR = 5.0
        exit
     end if
  end do
  if (TOTTR.gt.0.0) then
     HT5 = TOTHT/TOTTR
     DBH5 = TOTD/TOTTR
     OG = DBH5*HT5/10000.0
  else
     OG = 0.0
  end if
  return
end

subroutine GET_BAL(DBH,BALL1,BAL1,BAL)
  !     SHARED BY THE RUN (DIAGRO) AND DIAMCAL CODE.
  implicit none
  ! Variables
  integer*4 :: K
  real*4  :: DBH,BALL1(51),BAL1(500),BAL
  if (DBH .gt.100.0) then
     BAL = 0.0
  else if (DBH .gt.50.0) then
     K = INT(DBH-49.0)
     BAL = BALL1(K)
  else
     K = INT(DBH*10.0+0.5)
     BAL = BAL1(K)
  end if
  return
end
