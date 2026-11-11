!=========================!
!   organon : mortality   !
!=========================!
! ORGANON $Id$

  !     ORGANON GROWTH AND YIELD MODEL
  !     SUBROUTINES INCLUDED:
  !         MORTAL
  !         FUNCTION QUAD
  !         RAMORT
  !         PM_FERT
  !         PM_SWO
  !         PM_NWO
  !         PM_SMC
  !         PM_RAP
  !         OLDGRO

  !  04/25/2014 - THERE ARE COMMON SUBROUTINE NAMES IN THE SOURCE CODE
  !               USED TO BUILD THE ORGANON DLLS. IN ORDER TO LINK THE
  !               ORGANON SOURCE CODE WITH THE FVS SOURCE CODE WE CHANGED
  !               THE DUPLICATED SUBROUTINE NAMES TO MAKE THEM UNIQUE.

  !  CHANGED THE NAME OF SUBROUTINE - MORTAL TO MORTAL_RUN

subroutine MORTAL_RUN(VERSION,CYCLG,NTREES,IB,TDATAI,POST,MORT,&
     &TDATAR,SCR,GROWTH,MGEXP,DEADEXP,BALL1,BAL1,SI_1,&
     &SI_2,PN,YF,A1,A2,A1MAX,PA1MAX,NO,RD0,RAAGE,PDEN)
  implicit none
  !     ROUTINE FOR SETTING TREE MORTALITY

  ! Variables
  integer*4 :: VERSION,CYCLG,NTREES,IB,TDATAI(2000,3),I,ISPGRP,IND,KK
  real*4  :: TDATAR(2000,8),SCR(2000,3),GROWTH(2000,4),MGEXP(2000),DEADEXP(2000),BALL1(51),BAL1(500),&
       &SI_1,SI_2,PN(5),YF(5),A1,A2,RAAGE,A3,RDCC,KB,KR1,STBA,STN,PMK(2000),RAN,SQMDM,SQMDA,RD,RD0,BAA,NA,&
       &OG1,DBH,HT,FERTADJ,SBAL1,CR,PM,PS,QMDA,RDA,A1MAX,PA1MAX,XA3,NO,QMDP,CRADJ,NK,NAA,BAAA,QUAD1,QUAD2,&
       &POW(2000),XPM,PDEN
  logical*2 :: POST,MORT
  !      INTEGER*4 IANS,IYN
  do I = 1,NTREES
     POW(I) = 1.0
     if (VERSION .eq.4.and. TDATAI(I,1) .ne.351) then
        POW(I) = 0.2
     end if
  end do
  if (VERSION .le.3) then
     A3 = 14.39533971
  else
     A3 = 3.88
  end if
  if (VERSION .le.3) then
     RDCC = 0.60
  else
     RDCC = 0.5211
  end if
  KB = 0.005454154
  KR1 = 1.0
  STBA = 0.0
  STN = 0.0
  RAN = 0.0
  do I = 1,NTREES
     STBA = STBA+TDATAR(I,1)**2*KB*TDATAR(I,4)
     STN = STN+TDATAR(I,4)
     if (TDATAI(I,1) .eq.351.and. VERSION .le.3) then
        RAN = RAN+TDATAR(I,4)
     end if
     if (CYCLG.eq.0.and. POST) then
        STBA = STBA+TDATAR(I,1)**2*KB*MGEXP(I)
        STN = STN+MGEXP(I)
     end if
     PMK(I) = 0.0
     DEADEXP(I) = 0.0
  end do
  if (RAN .le.0.0001) then
     RAAGE = 0.0
  end if
  SQMDM = EXP(A1-A2*LOG(STN))
  SQMDA = SQRT(STBA/(KB*STN))
  RD = STN/EXP(A1/A2-LOG(SQMDA)/A2)
  if (CYCLG.eq.0) then
     RD0 = RD
     NO = 0.0
     A1MAX = A1
  end if
  BAA = 0.0
  NA = 0.0
  call OLDGRO(NTREES,IB,TDATAI,TDATAR,GROWTH,DEADEXP,0.0,OG1)

  !  INDIVIDUAL TREE MORTALITY EQUATIONS

  do I = 1,NTREES
     if (TDATAR(I,4) .le.0.) cycle
     ISPGRP = TDATAI(I,2)
     DBH = TDATAR(I,1)
     HT = TDATAR(I,2)
     call PM_FERT(ISPGRP,VERSION,CYCLG,PN,YF,FERTADJ)
     call GET_BAL(DBH,BALL1,BAL1,SBAL1)
     if (SCR(I,1) .gt. TDATAR(I,3)) then
        CR = SCR(I,1)
     else
        CR = TDATAR(I,3)
     end if

     !      IF (I .EQ. 15) THEN
     !         WRITE(*,*) ' SBAL1 = ',SBAL1,' BAL1(1) = ',BAL1(1)
     !         WRITE(*,1600)
     ! 1600    FORMAT(1X,' '\)
     !         IANS = IYN(2)
     !      ENDIF
     select case(VERSION)
     case(1)
        call PM_SWO(ISPGRP,DBH,CR,SI_1,SBAL1,OG1,POW(I),PMK(I))
     case(2)
        call PM_NWO(ISPGRP,DBH,CR,SI_1,SI_2,SBAL1,POW(I),PMK(I))
     case(3)
        call PM_SMC(ISPGRP,DBH,CR,SI_1,SI_2,SBAL1,POW(I),PMK(I))
     case(4)
        call PM_RAP(ISPGRP,DBH,CR,SI_1,SI_2,SBAL1,POW(I),PMK(I))
     end select
     PMK(I) = PMK(I)+FERTADJ
  end do
  if (VERSION .le.3) then
     if (RAAGE .ge.55.0) then
        call RAMORT(NTREES,TDATAI,RAAGE,TDATAR,RAN,PMK)
     end if
     RAAGE = RAAGE+5.0
  end if
  do I = 1,NTREES
     if (SCR(I,1) .gt. TDATAR(I,3)) then
        CR = SCR(I,1)
     else
        CR = TDATAR(I,3)
     end if
     !         CRADJ=1.0-EXP(-(10.0*CR)**5.0)
     CRADJ = 1.0
     if (CR.le.0.17) CRADJ = 1.0-EXP(-(25.0*CR)**2.0)
     XPM = 1.0/(1.0+EXP(-PMK(I)))
     PS = (1.0-XPM)**POW(I)
     PM = 1.0-PS*CRADJ
     NA = NA+TDATAR(I,4)*(1.0-PM)
     BAA = BAA+KB*(TDATAR(I,1)+GROWTH(I,2))**2*TDATAR(I,4)*(1.0-PM)
  end do

  !  DETERMINE IF ADDITIONAL MORTALITY MUST BE TAKEN

  if (MORT) then
     QMDA = SQRT(BAA/(KB*NA))
     RDA = NA/EXP(A1/A2-LOG(QMDA)/A2)
     if (CYCLG .eq.0) then

        !  INITALIZATIONS FOR FIRST GROWTH CYCLE

        if (RD .ge.1.0) then
           if (RDA .gt. RD) then
              A1MAX = LOG(SQMDA)+A2*LOG(STN)
           else
              A1MAX = LOG(QMDA)+A2*LOG(NA)
           end if
           IND = 1
           if (A1MAX .lt. A1) A1MAX = A1
           PA1MAX = A1MAX
        else
           IND = 0
           if (VERSION .le.3) then
              if (RD .gt. RDCC) then
                 XA3 = -1.0/A3
                 NO = STN*(LOG(RD)/LOG(RDCC))**XA3
              end if
           else
              NO = PDEN
           end if
        end if

        !  INITIALIZATIONS FOR SUBSEQUENT GROWTH CYCLES

     else
        if (RD0.ge.1.0) then
           IND = 1
           A1MAX = LOG(QMDA)+A2*LOG(NA)
           if (A1MAX .gt. PA1MAX) A1MAX = PA1MAX
           if (A1MAX .lt. A1) A1MAX = A1
           PA1MAX = A1MAX
        else if (RD .ge.1.0.and. NO .le.0.0) then
           if (RDA .gt. RD) then
              A1MAX = LOG(SQMDA)+A2*LOG(STN)
           else
              A1MAX = LOG(QMDA)+A2*LOG(NA)
           end if
           IND = 1
           if (A1MAX .lt. A1) A1MAX = A1
           PA1MAX = A1MAX
        else
           IND = 0
           if (VERSION .le.3) then
              if (RD .gt. RDCC .and. NO .le.0.0) then
                 XA3 = -1.0/A3
                 NO = STN*(LOG(RD)/LOG(RDCC))**XA3
              end if
           else
              NO = PDEN
           end if
        end if
     end if

     !  COMPUTATION OF ADDITIONAL MORTALITY IF NECESSARY

     if (IND .eq.0.and.NO .gt.0.0) then
        if (VERSION .le.3) then
           QMDP = QUAD1(NA,NO,RDCC,A1)
        else
           QMDP = QUAD2(NA,NO,RDCC,A1)
        end if
     else
        QMDP = EXP(A1MAX-A2*LOG(NA))
     end if

     !          NO ADDITIONAL MORTALITY NECESSARY

     if (RD .le. RDCC .or. QMDP .gt. QMDA) then
        do I = 1,NTREES
           if (SCR(I,1) .gt. TDATAR(I,3)) then
              CR = SCR(I,1)
           else
              CR = TDATAR(I,3)
           end if
           !                   CRADJ=1.0-EXP(-(10.0*CR)**5.0)
           CRADJ = 1.0
           if (CR.le.0.17) CRADJ = 1.0-EXP(-(25.0*CR)**2.0)
           XPM = 1.0/(1.0+EXP(-PMK(I)))
           PS = (1.0-XPM)**POW(I)
           PM = 1.0-PS*CRADJ
           DEADEXP(I) = TDATAR(I,4)*PM
           TDATAR(I,4) = TDATAR(I,4)*(1.0-PM)
        end do

        !          ADJUSTMENT TO MORTALITY NECESSARY

     else
        KR1 = 0.0
        do KK = 1,7
           NK = 10.0/10.0**KK
           82 KR1 = KR1+NK
           NAA = 0.0
           BAAA = 0.0
           do I = 1,NTREES
              if (TDATAR(I,4).lt.0.001) cycle
              if (SCR(I,1) .gt. TDATAR(I,3)) then
                 CR = SCR(I,1)
              else
                 CR = TDATAR(I,3)
              end if
              !                      CRADJ=1.0-EXP(-(10.0*CR)**5.0)
              CRADJ = 1.0
              if (CR.le.0.17) CRADJ = 1.0-EXP(-(25.0*CR)**2.0)
              XPM = 1.0/(1.0+EXP(-(KR1+PMK(I))))
              PS = (1.0-XPM)**POW(I)
              PM = 1.0-PS*CRADJ
              NAA = NAA+TDATAR(I,4)*(1.0-PM)
              BAAA = BAAA+KB*(TDATAR(I,1)+GROWTH(I,2))**2.0*&
                   &TDATAR(I,4)*(1.0-PM)
           end do
           QMDA = SQRT(BAAA/(KB*NAA))
           if (IND .eq.0) then
              if (VERSION .le.3) then
                 QMDP = QUAD1(NAA,NO,RDCC,A1)
              else
                 QMDP = QUAD2(NAA,NO,RDCC,A1)
              end if
           else
              QMDP = EXP(A1MAX-A2*LOG(NAA))
           end if
           if (QMDP.ge.QMDA) then
              KR1 = KR1-NK
           else
              go to 82
           end if
        end do
        do I = 1,NTREES
           if (TDATAR(I,4).le.0.0) then
              DEADEXP(I) = 0.0
              TDATAR(I,4) = 0.0
           else
              if (SCR(I,1) .gt. TDATAR(I,3)) then
                 CR = SCR(I,1)
              else
                 CR = TDATAR(I,3)
              end if
              !                     CRADJ=1.0-EXP(-(10.0*CR)**5.0)
              CRADJ = 1.0
              if (CR.le.0.17) CRADJ = 1.0-EXP(-(25.0*CR)**2.0)
              XPM = 1.0/(1.0+EXP(-(KR1+PMK(I))))
              PS = (1.0-XPM)**POW(I)
              PM = 1.0-PS*CRADJ
              DEADEXP(I) = TDATAR(I,4)*PM
              TDATAR(I,4) = TDATAR(I,4)*(1.0-PM)
           end if
        end do
     end if
  else
     do I = 1,NTREES
        if (SCR(I,1) .gt. TDATAR(I,3)) then
           CR = SCR(I,1)
        else
           CR = TDATAR(I,3)
        end if
        !                   CRADJ=1.0-EXP(-(10.0*CR)**5.0)
        CRADJ = 1.0
        if (CR.le.0.17) CRADJ = 1.0-EXP(-(25.0*CR)**2.0)
        XPM = 1.0/(1.0+EXP(-PMK(I)))
        PS = (1.0-XPM)**POW(I)
        PM = 1.0-PS*CRADJ
        DEADEXP(I) = TDATAR(I,4)*PM
        TDATAR(I,4) = TDATAR(I,4)*(1.0-PM)
     end do
  end if
  do I = 1,NTREES
     if (TDATAR(I,4) .lt.0.00001)TDATAR(I,4) = 0.0
  end do
  return
end

function QUAD1(NI,NO,RDCC,A1)

  implicit none
  ! Variables
  real*4  :: NI,NO,QUAD1,A1,A2,A3,A4,X,RDCC
  A2 = 0.62305
  A3 = 14.39533971
  A4 = -(LOG(RDCC)*A2/A1)
  X = A1-A2*LOG(NI)-(A1*A4)*EXP(-A3*(LOG(NO)-LOG(NI)))
  QUAD1 = EXP(X)
  return
end

function QUAD2(NI,NO,RDCC,A1)

  implicit none
  ! Variables
  real*4  :: NI,NO,QUAD2,A1,A2,A3,A4,X,RDCC
  real*4  :: RDANUW

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = RDCC

  A2 = 0.64
  A3 = 3.88
  A4 = 0.07
  X = A1-A2*LOG(NI)-(A1*A4)*EXP(-A3*(LOG(NO)-LOG(NI)))
  QUAD2 = EXP(X)
  return
end

subroutine RAMORT(NTREES,TDATAI,RAAGE,TDATAR,RAN,PMK)
  implicit none
  ! Variables
  real*4  :: RAAGE,TDATAR(2000,8),RAN,PMK(2000),PM,RAMORT1,RAMORT2,RAQMDN1,RAQMDN2,RABAN1,RABAN2,&
       &RATPAN1,RATPAN2,KB,KR1,NK
  integer*4 :: NTREES,TDATAI(2000,3),I,KK
  KB = 0.005454154
  RAMORT1 = 0.0
  do I = 1,NTREES
     if (TDATAI(I,1) .eq.351) then
        PM = 1.0/(1.0+EXP(-PMK(I)))
        RAMORT1 = RAMORT1+TDATAR(I,4)*PM
     end if
  end do
  RAQMDN1 = 3.313+0.18769*RAAGE-0.000198*RAAGE*RAAGE
  RABAN1 = -26.1467+5.31482*RAAGE-0.037466*RAAGE*RAAGE
  RAQMDN2 = 3.313+0.18769*(RAAGE+5.0)-0.000198*(RAAGE+5.0)**2
  RABAN2 = -26.1467+5.31482*(RAAGE+5.0)-0.037466*(RAAGE+5.0)**2
  RATPAN1 = RABAN1/(KB*RAQMDN1*RAQMDN1)
  RATPAN2 = RABAN2/(KB*RAQMDN2*RAQMDN2)
  if (RATPAN1.gt.0.0.and. RATPAN2.gt.0.0) then
     RAMORT2 = RAN*(1.0-RATPAN2/RATPAN1)
  else
     do I = 1,NTREES
        if (TDATAI(I,1) .eq.351) then
           PMK(I) = 1000.0
        end if
     end do
     return
  end if
  if (RAMORT1.lt. RAMORT2) then
     KR1 = 0.0
     do KK = 1,7
        NK = 10.0/10.0**KK
        30 KR1 = KR1+NK
        RAMORT1 = 0.0
        do I = 1,NTREES
           if (TDATAI(I,1) .eq.351) then
              PM = 1.0/(1.0+EXP(-(KR1+PMK(I))))
              RAMORT1 = RAMORT1+TDATAR(I,4)*PM
           end if
        end do
        if (RAMORT1.gt. RAMORT2) then
           KR1 = KR1-NK
        else
           go to 30
        end if
     end do
     do I = 1,NTREES
        if (TDATAI(I,1) .eq.351) then
           PMK(I) = KR1+PMK(I)
        end if
     end do
  end if
  return
end

subroutine PM_FERT(ISPGRP,VERSION,CYCLG,PN,YF,FERTADJ)
  implicit none
  ! Variables
  integer*4 :: ISPGRP,VERSION,CYCLG,II
  real*4  :: PN(5),YF(5),FERTADJ,PF1,PF2,PF3,XTIME,FERTX1,FERTX2
  if (VERSION .le.3) then
     if (ISPGRP .eq.1) then
        PF1 = 0.0000552859
        PF2 = 1.5
        PF3 = -0.5
     else
        PF1 = 0.0
        PF2 = 1.0
        PF3 = 0.0
     end if
  else
     PF1 = 0.0
     PF2 = 1.0
     PF3 = 0.0
  end if
  XTIME = FLOAT(CYCLG)*5.0
  FERTX1 = 0.0
  do II = 2,5
     FERTX1 = FERTX1+PN(II)*EXP((PF3/PF2)*(YF(1)-YF(II)))
  end do
  FERTX2 = EXP(PF3*(XTIME-YF(1)))
  FERTADJ = PF1*(PN(1)+FERTX1)**PF2*FERTX2
  return
end

subroutine PM_SWO(ISPGRP,DBH,CR,SI_1,BAL,OG,POW,PM)
  implicit none
  ! Variables
  integer*4 :: ISPGRP
  real*4  :: DBH,CR,SI_1,BAL,OG,POW,PM,B0,B1,B2,B3,B4,B5,B6,B7,MPAR(18,9)

  !  NEW SWO MORTALITY WITH REVISED CLO PARAMETERS (8 parameters - all species)

  !     DF Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     GW Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     PP Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     SP Coefficients from Unpublished Equation on File at OSU Dept. Forest Resources
  !     IC Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     WH Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     RC Coefficients from WH of Hann, Marshall, Hanus (2003) FRL Research Contribution 40
  !     PY Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     MD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     GC Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     TA Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     CL Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     BL Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     WO Coefficients from Gould, Marshall, and Harrington (2008) West. J. Appl. For. 23: 26-33
  !     BO Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     RA Coefficients from Best Guess
  !     PD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     WI Coefficients from Best Guess

  data MPAR/&
       &-4.648483270, -2.215777201, -1.050000682, -1.531051304,& !  DF,GW,PP,SP
       &-1.922689902, -1.166211991, -0.761609   , -4.072781265,& !  IC,WH,RC,PY
       &-6.089598985, -4.317549852, -2.410756914, -2.990451960,& !  MD,GC,TA,CL
       &-2.976822456, -6.00031085 , -3.108619921, -2.0        ,& !  BL,WO,BO,RA
       &-3.020345211, -1.386294361,& !  PD,WI

       &-0.266558690, -0.162895666, -0.194363402,  0.0        ,& !  DF,GW,PP,SP
       &-0.136081990,  0.0        , -0.529366   , -0.176433475,& !  IC,WH,RC,PY
       &-0.245615070, -0.057696253,  0.0        ,  0.0        ,& !  MD,GC,TA,CL
       &0.0        , -0.10490823 , -0.570366764, -0.5        ,& !  BL,WO,BO,RA
       &0.0        ,  0.0        ,& !  PD,WI

       &0.003699110,  0.003317290,  0.003803100,  0.0        ,& !  DF,GW,PP,SP
       &0.002479863,  0.0        ,  0.0        ,  0.0        ,& !  IC,WH,RC,PY
       &0.0        ,  0.0        ,  0.0        ,  0.0        ,& !  MD,GC,TA,CL
       &0.0        ,  0.0        ,  0.018205398,  0.015      ,& !  BL,WO,BO,RA
       &0.0        ,  0.0        ,& !  PD,WI

       &-2.118026640, -3.561438261, -3.557300286,  0.0        ,& !  DF,GW,PP,SP
       &-3.178123293, -4.602668157, -4.74019    , -1.729453975,& !  IC,WH,RC,PY
       &-3.208265570,  0.0        , -1.049353753,  0.0        ,& !  MD,GC,TA,CL
       &-6.223250962, -0.99541909 , -4.584655216, -3.0        ,& !  BL,WO,BO,RA
       &-8.467882343,  0.0        ,& !  PD,WI

       &0.025499430,  0.014644689,  0.003971638,  0.0        ,& !  DF,GW,PP,SP
       &0.0        ,  0.0        ,  0.0119587  ,  0.0        ,& !  IC,WH,RC,PY
       &0.033348079,  0.004861355,  0.008845583,  0.0        ,& !  MD,GC,TA,CL
       &0.0        ,  0.00912739 ,  0.014926170,  0.015      ,& !  BL,WO,BO,RA
       &0.013966388,  0.0        ,& !  PD,WI

       &0.003361340,  0.0        ,  0.005573601,  0.0        ,& !  DF,GW,PP,SP
       &0.004684133,  0.0        ,  0.00756365 ,  0.012525642,& !  IC,WH,RC,PY
       &0.013571319,  0.00998129 ,  0.0        ,  0.002884840,& !  MD,GC,TA,CL
       &0.0        ,  0.87115652 ,  0.012419026,  0.01       ,& !  BL,WO,BO,RA
       &0.009461545,  0.0        ,& !  PD,WI

       &0.013553950,  0.0        ,  0.0        ,  0.0        ,& !  DF,GW,PP,SP
       &0.0        ,  0.0        ,  0.0        ,  0.0        ,& !  IC,WH,RC,PY
       &0.0        ,  0.0        ,  0.0        ,  0.0        ,& !  MD,GC,TA,CL
       &0.0        ,  0.0        ,  0.0        ,  0.0        ,& !  BL,WO,BO,RA
       &0.0        ,  0.0        ,& !  PD,WI

       &-2.723470950,  0.0        ,  0.0        ,  0.0        ,& !  DF,GW,PP,SP
       &0.0        ,  0.0        ,  0.0        ,  0.0        ,& !  IC,WH,RC,PY
       &0.0        ,  0.0        ,  0.0        ,  0.0        ,& !  MD,GC,TA,CL
       &0.0        ,  0.0        ,  0.0        ,  0.0        ,& !  BL,WO,BO,RA
       &0.0        ,  0.0        ,& !  PD,WI

       &1.0        ,  1.0        ,  1.0        ,  1.0        ,& !  DF,GW,PP,SP
       &1.0        ,  1.0        ,  1.0        ,  1.0        ,& !  IC,WH,RC,PY
       &1.0        ,  1.0        ,  1.0        ,  1.0        ,& !  MD,GC,TA,CL
       &1.0        ,  1.0        ,  1.0        ,  1.0        ,& !  BL,WO,BO,RA
       &1.0        ,  1.0/ !  PD,WI

  B0 = MPAR(ISPGRP,1)
  B1 = MPAR(ISPGRP,2)
  B2 = MPAR(ISPGRP,3)
  B3 = MPAR(ISPGRP,4)
  B4 = MPAR(ISPGRP,5)
  B5 = MPAR(ISPGRP,6)
  B6 = MPAR(ISPGRP,7)
  B7 = MPAR(ISPGRP,8)
  POW = MPAR(ISPGRP,9)
  if (ISPGRP .eq.14) then ! Oregon White Oak
     PM = B0+B1*DBH+B2*DBH**2+B3*CR+B4*(SI_1+4.5)+B5*ALOG(BAL+5.0)
  else
     PM = B0+B1*DBH+B2*DBH**2+B3*CR+B4*(SI_1+4.5)+B5*BAL&
          &+B6*BAL*EXP(B7*OG)
  end if
  return
end

subroutine PM_NWO(ISPGRP,DBH,CR,SI_1,SI_2,BAL,POW,PM)
  implicit none
  ! Variables
  integer*4 :: ISPGRP
  real*4  :: DBH,CR,SI_1,SI_2,BAL,POW,PM,B0,B1,B2,B3,B4,B5,MPAR(11,7),SQDBH,CR25

  !  NWO MORTALITY (6 parameters - all species)

  !     DF Coefficients from Unpublished Equation on File at OSU Dept. Forest Resources
  !     GF Coefficients from Unpublished Equation on File at OSU Dept. Forest Resources
  !     WH Coefficients from Hann, Marshall, Hanus (2003) FRL Research Contribution 40
  !     RC Coefficients from WH of Hann, Marshall, Hanus (2003) FRL Research Contribution 40
  !     PY Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     MD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     BL Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     WO Coefficients from Gould, Marshall, and Harrington (2008) West. J. Appl. For. 23: 26-33
  !     RA Coefficients from Best Guess
  !     PD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     WI Coefficients from Best Guess

  data MPAR/&
       &-4.13142    , -7.60159    , -0.761609   , -0.761609   ,& !  DF,GF,WH,RC
       &-4.072781265, -6.089598985, -2.976822456, -6.00031085 ,& !  PY,MD,BL,WO
       &-2.0        , -3.020345211, -1.386294361,& !  RA,PD,WI

       &-1.13736    , -0.200523   , -0.529366   , -0.529366   ,& !  DF,GF,WH,RC
       &-0.176433475, -0.245615070,  0.0        , -0.10490823 ,& !  PY,MD,BL,WO
       &-0.5        ,  0.0        ,  0.0        ,& !  RA,PD,WI

       &0.0        ,  0.0        ,  0.0        ,  0.0        ,& !  DF,GF,WH,RC
       &0.0        ,  0.0        ,  0.0        ,  0.0        ,& !  PY,MD,BL,WO
       &0.015      ,  0.0        ,  0.0        ,& !  RA,PD,WI

       &-0.823305   ,  0.0        , -4.74019    , -4.74019    ,& !  DF,GF,WH,RC
       &-1.729453975, -3.208265570, -6.223250962, -0.99541909 ,& !  PY,MD,BL,WO
       &-3.0        , -8.467882343,  0.0        ,& !  RA,PD,WI

       &0.0307749  ,  0.0441333  ,  0.0119587  ,  0.0119587  ,& !  DF,GF,WH,RC
       &0.0        ,  0.033348079,  0.0        ,  0.00912739 ,& !  PY,MD,BL,WO
       &0.015      ,  0.013966388,  0.0        ,& !  RA,PD,WI

       &0.00991005 ,  0.00063849 ,  0.00756365 ,  0.00756365 ,& !  DF,GF,WH,RC
       &0.012525642,  0.013571319,  0.0        ,  0.87115652 ,& !  PY,MD,BL,WO
       &0.01       ,  0.009461545,  0.0        ,& !  RA,PD,WI

       &1.0        ,  1.0        ,  1.0        ,  1.0        ,& !  DF,GF,WH,RC
       &1.0        ,  1.0        ,  1.0        ,  1.0        ,& !  PY,MD,BL,WO
       &1.0        ,  1.0        ,  1.0/ !  RA,PD,WI

  B0 = MPAR(ISPGRP,1)
  B1 = MPAR(ISPGRP,2)
  B2 = MPAR(ISPGRP,3)
  B3 = MPAR(ISPGRP,4)
  B4 = MPAR(ISPGRP,5)
  B5 = MPAR(ISPGRP,6)
  POW = MPAR(ISPGRP,7)
  SQDBH = DBH**0.5
  CR25 = CR**0.25
  if (ISPGRP .eq.1) then ! Douglas fir
     PM = B0+B1*SQDBH+B3*CR25+B4*(SI_1+4.5)+B5*BAL
  else if (ISPGRP .eq.2) then ! Grand Fir
     !      IF(ISPGRP .EQ. 2)THEN                          ! Grand Fir
     PM = B0+B1*DBH+B4*(SI_1+4.5)+B5*(BAL/DBH)
  else if (ISPGRP .eq.3.or. ISPGRP .eq.4) then ! Western Hemlock and Western Red Cedar
     PM = B0+B1*DBH+B2*DBH**2+B3*CR+B4*(SI_2+4.5)+B5*BAL
  else if (ISPGRP .eq.8) then ! Oregon White Oak
     PM = B0+B1*DBH+B2*DBH**2+B3*CR+B4*(SI_1+4.5)+B5*ALOG(BAL+5.0)
  else
     PM = B0+B1*DBH+B2*DBH**2+B3*CR+B4*(SI_1+4.5)+B5*BAL
  end if
  return
end

subroutine PM_SMC(ISPGRP,DBH,CR,SI_1,SI_2,BAL,POW,PM)
  implicit none
  ! Variables
  integer*4 :: ISPGRP
  real*4  :: DBH,CR,SI_1,SI_2,BAL,POW,PM,B0,B1,B2,B3,B4,B5,MPAR(11,7)

  !  SMC MORTALITY (6 parameters - all species)

  !     DF Coefficients from Hann, Marshall, and Hanus (2006) FRL Research Contribution 49
  !     GF Coefficients from Unpublished Equation on File at OSU Dept. Forest Resources
  !     WH Coefficients from Hann, Marshall, Hanus (2003) FRL Research Contribution 40
  !     RC Coefficients from WH of Hann, Marshall, Hanus (2003) FRL Research Contribution 40
  !     PY Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     MD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     BL Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     WO Coefficients from Gould, Marshall, and Harrington (2008) West. J. Appl. For. 23: 26-33
  !     RA Coefficients from Best Guess
  !     PD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     WI Coefficients from Best Guess

  data MPAR/&
       &-3.12161659 , -7.60159    , -0.761609   , -0.761609   ,& !  DF,GF,WH,RC
       &-4.072781265, -6.089598985, -2.976822456, -6.00031085 ,& !  PY,MD,BL,WO
       &-2.0        , -3.020345211, -1.386294361,& !  RA,PD,WI

       &-0.44724396 , -0.200523   , -0.529366   , -0.529366   ,& !  DF,GF,WH,RC
       &-0.176433475, -0.245615070,  0.0        , -0.10490823 ,& !  PY,MD,BL,WO
       &-0.5        ,  0.0        ,  0.0        ,& !  RA,PD,WI

       &0.0        ,  0.0        ,  0.0        ,  0.0        ,& !  DF,GF,WH,RC
       &0.0        ,  0.0        ,  0.0        ,  0.0        ,& !  PY,MD,BL,WO
       &0.015      ,  0.0        ,  0.0        ,& !  RA,PD,WI

       &-2.48387172 ,  0.0        , -4.74019    , -4.74019    ,& !  DF,GF,WH,RC
       &-1.729453975, -3.208265570, -6.223250962, -0.99541909 ,& !  PY,MD,BL,WO
       &-3.0        , -8.467882343,  0.0        ,& !  RA,PD,WI

       &0.01843137 ,  0.0441333  ,  0.0119587  ,  0.0119587  ,& !  DF,GF,WH,RC
       &0.0        ,  0.033348079,  0.0        ,  0.00912739 ,& !  PY,MD,BL,WO
       &0.015      ,  0.013966388,  0.0        ,& !  RA,PD,WI

       &0.01353918 ,  0.00063849 ,  0.00756365 ,  0.00756365 ,& !  DF,GF,WH,RC
       &0.012525642,  0.013571319,  0.0        ,  0.87115652 ,& !  PY,MD,BL,WO
       &0.01       ,  0.009461545,  0.0        ,& !  RA,PD,WI

       &1.0        ,  1.0        ,  1.0        ,  1.0        ,& !  DF,GF,WH,RC
       &1.0        ,  1.0        ,  1.0        ,  1.0        ,& !  PY,MD,BL,WO
       &1.0        ,  1.0        ,  1.0/ !  RA,PD,WI

  B0 = MPAR(ISPGRP,1)
  B1 = MPAR(ISPGRP,2)
  B2 = MPAR(ISPGRP,3)
  B3 = MPAR(ISPGRP,4)
  B4 = MPAR(ISPGRP,5)
  B5 = MPAR(ISPGRP,6)
  POW = MPAR(ISPGRP,7)
  if (ISPGRP .eq.2) then ! Grand Fir
     PM = B0+B1*DBH+B4*(SI_1+4.5)+B5*(BAL/DBH)
  else if (ISPGRP .eq.3.or. ISPGRP .eq.4) then ! Western Hemlock and Western Red Cedar
     PM = B0+B1*DBH+B2*DBH**2+B3*CR+B4*(SI_2+4.5)+B5*BAL
  else if (ISPGRP .eq.8) then ! Oregon White Oak
     PM = B0+B1*DBH+B2*DBH**2+B3*CR+B4*(SI_1+4.5)+B5*ALOG(BAL+5.0)
  else
     PM = B0+B1*DBH+B2*DBH**2+B3*CR+B4*(SI_1+4.5)+B5*BAL
  end if
  return
end

subroutine PM_RAP(ISPGRP,DBH,CR,SI_1,SI_2,BAL,POW,PM)
  implicit none
  ! Variables
  integer*4 :: ISPGRP
  real*4  :: DBH,CR,SI_1,SI_2,BAL,POW,PM,B0,B1,B2,B3,B4,B5,MPAR(7,7),SITE

  !  RAP MORTALITY (6 parameters - all species)

  !     RA Coefficients from Hann, Bluhm, and Hibbs New Red Alder Equation
  !     DF Coefficients from Hann, Marshall, and Hanus (2006) FRL Research Contribution 49
  !     WH Coefficients from Hann, Marshall, Hanus (2003) FRL Research Contribution 40
  !     RC Coefficients from WH of Hann, Marshall, Hanus (2003) FRL Research Contribution 40
  !     BL Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     PD Coefficients from Hann and Hanus (2001) FRL Research Contribution 34
  !     WI Coefficients from Best Guess

  data MPAR/&
       &-4.333150734 , -3.12161659 , -0.761609   , -0.761609   ,& !  RA,DF,WH,RC
       &-2.976822456 , -3.020345211, -1.386294361,& !  BL,PD,WI

       &-0.9856713799, -0.44724396 , -0.529366   , -0.529366   ,& !  RA,DF,WH,RC
       &0.0         ,  0.0        ,  0.0        ,& !  BL,PD,WI,WO

       &0.0         ,  0.0        ,  0.0        ,  0.0        ,& !  RA,DF,WH,RC
       &0.0         ,  0.0        ,  0.0        ,& !  BL,PD,WI

       &-2.583317081 , -2.48387172 , -4.74019    , -4.74019    ,& !  RA,DF,WH,RC
       &-6.223250962 , -8.467882343,  0.0        ,& !  BL,PD,WI

       &0.0369852164,  0.01843137 ,  0.0119587  ,  0.0119587  ,& !  RA,DF,WH,RC
       &0.0         ,  0.013966388,  0.0        ,& !  BL,PD,WI

       &0.0394546978,  0.01353918 ,  0.00756365 ,  0.00756365 ,& !  RA,DF,WH,RC
       &0.0         ,  0.009461545,  0.0        ,& !  BL,PD,WI

       &1.0         ,  0.2        ,  0.2        ,  0.2        ,& !  RA,DF,WH,RC
       &0.2         ,  0.2        ,  0.2        / !  BL,PD,WI

  B0 = MPAR(ISPGRP,1)
  B1 = MPAR(ISPGRP,2)
  B2 = MPAR(ISPGRP,3)
  B3 = MPAR(ISPGRP,4)
  B4 = MPAR(ISPGRP,5)
  B5 = MPAR(ISPGRP,6)
  POW = MPAR(ISPGRP,7)
  if (ISPGRP .eq.1) then
     SITE = SI_1+4.5
  else if (ISPGRP .eq.2.or. ISPGRP .gt.4) then
     SITE = SI_2+4.5
  else if (ISPGRP .eq.3.or. ISPGRP .eq.4) then
     SITE = -0.432 + 0.899 * (SI_2+4.5)
  end if
  PM = B0+B1*DBH+B2*DBH**2+B3*CR+B4*SITE+B5*BAL
  return
end

subroutine OLDGRO(NTREES,IB,TDATAI,TDATAR,GROWTH,DEADEXP,&
     &XIND,OG)
  !     DETERMINE THE OLD GROWTH INDICATOR "OG"

  !          XIND =  0.0, DO NOT ADD GROWTH VALUES OR MORTALITY VALUES
  !               = -1.0, SUBTRACT GROWTH VALUES AND ADD MORTALITY VALUES
  !               =  1.0, ADD GROWTH VALUES AND SUBTRACT MORTALITY VALUES
  implicit none
  ! Variables
  integer*4 :: NTREES,IB,TDATAI(2000,3),I,ID
  real*4  :: TDATAR(2000,8),GROWTH(2000,4),DEADEXP(2000),TOTHT,TOTD,TOTTR,HTCL(100),DCL(100),&
       &TRCL(100),HT5,DBH5,TRDIFF,HT,DBH,EXPAN,XIND,OG
  !      INTEGER*4 IANS,IYN

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
        HT = TDATAR(I,2)+XIND*GROWTH(I,1)
        DBH = TDATAR(I,1)+XIND*GROWTH(I,2)
        EXPAN = TDATAR(I,4)-XIND*DEADEXP(I)
        !      WRITE(*,*) ' HG= ',GROWTH(I,1),' DG= ',GROWTH(I,2)
        !      WRITE(*,*) ' DEAD= ',DEADEXP(I),' I= ',I
        !      WRITE(*,1600)
        ! 1600 FORMAT(1X,' '\)
        !      IANS = IYN(2)
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
        go to 20
     end if
  end do
  20 if (TOTTR.gt.0.0) then
     !      WRITE(*,*) ' TOTHT= ',TOTHT,' TOTD= ',TOTD,' TOTTR= ',TOTTR
     !      WRITE(*,1600)
     ! 1600 FORMAT(1X,' '\)
     !      IANS = IYN(2)
     HT5 = TOTHT/TOTTR
     DBH5 = TOTD/TOTTR
     OG = DBH5*HT5/10000.0
  else
     OG = 0.0
  end if
  return
end
