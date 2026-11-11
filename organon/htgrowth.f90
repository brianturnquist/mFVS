!========================!
!   organon : htgrowth   !
!========================!
! ORGANON $Id$

  !     ORGANON GROWTH AND YIELD MODEL
  !     SUBROUTINES INCLUDED:
  !         HTGRO1
  !         HTGRO2
  !         HS_HG
  !         B_HG
  !         F_HG
  !C         NCHG
  !         WHHLB_HG
  !         WHHLB_SI_UC
  !         WHHLB_GEA
  !         WHHLB_H40
  !         HG_SWO
  !         HG_NWO
  !         HG_SMC
  !         HG_RAP
  !         LIMIT
  !         HD_NWO
  !         HD_SMC
  !         HD_RAP
  !         HG_FERT
  !         HG_THIN
  !         RAGEA
  !         RAH40

subroutine HTGRO1(K,M,ON,VERSION,CYCLG,IB,TDATAI,TDATAR,SI_1,SI_2,&
     &CCH,CALIB,PN,YF,BABT,BART,YT,OLD,PDEN,GROWTH)
  implicit none
  !     CALCULATE 5-YEAR HEIGHT GROWTH

  !     K = TREE NUMBER TO GROW HEIGHT

  !     M = METHOD OF TRIPLING
  !       0 = NO TRIPLING
  !       1 = TRIPLE EVERY TREE
  !       2 = TRIPLE EVERY OTHER TREE
  !       3 = RANDOM ERROR

  !     ON = EVERY OTHER TREE TRIPLING DESIGNATOR
  !        1 = TRIPLE THIS TREE
  !        0 = DON'T TRIPLE THIS TREE

  ! Variables
  integer*4 :: K,M,ON,VERSION,CYCLG,IB,TDATAI(2000,3),ISPGRP,I,ISISP
  real*4  :: TDATAR(2000,8),SI_1,SI_2,CCH(41),CALIB(6,18),PN(5),YF(5),BABT,BART(5),YT(5),OLD,&
       &GROWTH(2000,4),CR,DBH,H,XI,XXI,TCCH,SITE,GEAGE,GP,PHTGRO,IDXAGE,HG,FERTADJ,THINADJ,PDEN
  !      INTEGER*4 IANS,IYN
  real*4  :: RDANUW

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = CALIB(1,1)

  !     CALCULATE HEIGHT GROWTH

  CR = TDATAR(K,3)
  DBH = TDATAR(K,1)
  H = TDATAR(K,2)
  ISPGRP = TDATAI(K,2)

  !     FOR MAJOR SPECIES

  if (TDATAI(K,2) .le. IB) then
     XI = 40.0*(TDATAR(K,2)/CCH(41))
     I = INT(XI)+2
     XXI = FLOAT(I)-1.0
     if (TDATAR(K,2) .ge. CCH(41)) then
        TCCH = 0.0
     else if (I.eq.41) then
        TCCH = CCH(40)*(40.0-XI)
     else
        TCCH = CCH(I)+(CCH(I-1)-CCH(I))*(XXI-XI)
     end if

     !     COMPUTE HEIGHT GROWTH OF UNTREATED TREES

     select case(VERSION)
     case(1)

        !                POTENTIAL HEIGHT GROWTH FROM HANN AND SCRIVANI'S (1987) DOMINANT
        !                HEIGHT GROWTH EQUATION

        if (TDATAI(K,1) .eq.122) then
           SITE = SI_2
           ISISP = 2
        else
           SITE = SI_1
           if (TDATAI(K,1) .eq.81)SITE = (SI_1+4.5)*0.66-4.5
           ISISP = 1
        end if
        call HS_HG(ISISP,SITE,TDATAR(K,2),GEAGE,PHTGRO)
        IDXAGE = 500.0
        call HG_SWO(ISPGRP,PHTGRO,CR,TCCH,HG)
     case(2)
        GP = 5.0
        if (TDATAI(K,2) .eq.3) then

           !                    POTENTIAL HEIGHT GROWTH FROM FLEWELLING'S WESTERN HEMLOCK
           !                    DOMINANT HEIGHT GROWTH

           SITE = SI_2+4.5
           call F_HG(SITE,TDATAR(K,2),GP,GEAGE,PHTGRO)
        else

           !                    POTENTIAL HEIGHT GROWTH FROM BRUCE'S (1981) DOMINANT HEIGHT
           !                    GROWTH FOR DOUGLAS-FIR
           !                    AND GRAND FIR

           SITE = SI_1+4.5
           call B_HG(SITE,TDATAR(K,2),GP,GEAGE,PHTGRO)
        end if
        IDXAGE = 120.0
        call HG_NWO(ISPGRP,PHTGRO,CR,TCCH,HG)
     case(3)
        GP = 5.0
        if (TDATAI(K,2) .eq.3) then

           !                    POTENTIAL HEIGHT GROWTH FROM FLEWELLING'S WESTERN HEMLOCK
           !                    DOMINANT HEIGHT GROWTH

           SITE = SI_2+4.5
           call F_HG(SITE,TDATAR(K,2),GP,GEAGE,PHTGRO)
        else

           !                    POTENTIAL HEIGHT GROWTH FROM BRUCE'S (1981) DOMINANT HEIGHT
           !                    GROWTH FOR DOUGLAS-FIR
           !                    AND GRAND FIR

           SITE = SI_1+4.5
           call B_HG(SITE,TDATAR(K,2),GP,GEAGE,PHTGRO)
        end if
        IDXAGE = 120.0
        call HG_SMC(ISPGRP,PHTGRO,CR,TCCH,HG)
     case(4)
        GP = 1.0
        if (TDATAI(K,2) .eq.1) then
           !                    POTENTIAL HEIGHT GROWTH FROM WEISKITTEL, HANN, HIBBS, LAM,
           !                    AND BLUHM (2009) RED ALDER TOP HEIGHT GROWTH

           SITE = SI_1+4.5
           call WHHLB_HG(SITE,PDEN,TDATAR(K,2),GP,GEAGE,&
                &PHTGRO)
           !                 WRITE(60,1000) SITE,PDEN,TDATAR(K,2),GP,GEAGE,PHTGRO,
           !     1                          DBH,H,CR,TCCH
           ! 1000            FORMAT(10F12.6)
        else if (TDATAI(K,2) .eq.3) then

           !                    POTENTIAL HEIGHT GROWTH FROM FLEWELLING'S WESTERN HEMLOCK
           !                    DOMINANT HEIGHT GROWTH

           SITE = -0.432 + 0.899 * (SI_2+4.5)
           call F_HG(SITE,TDATAR(K,2),GP,GEAGE,PHTGRO)
        else

           !                    POTENTIAL HEIGHT GROWTH FROM BRUCE'S (1981) DOMINANT HEIGHT
           !                    GROWTH FOR DOUGLAS-FIR
           !                    AND GRAND FIR

           SITE = SI_2+4.5
           call B_HG(SITE,TDATAR(K,2),GP,GEAGE,PHTGRO)
        end if
        IDXAGE = 30.0
        call HG_RAP(ISPGRP,PHTGRO,CR,TCCH,HG)
        !                 WRITE(60,1000) SITE,GEAGE,PHTGRO,DBH,H,CR,TCCH,HG
        ! 1000            FORMAT(2F7.2,F10.7,2F10.5,2F10.6,F10.7)
     end select
     if (TDATAI(K,2).le.IB.and.GEAGE.gt.IDXAGE) then
        OLD = OLD+1.
        if (M .eq.1.or. (M .eq.2.and. ON .eq.1))OLD = OLD+2.
     end if
     call HG_FERT(CYCLG,VERSION,ISPGRP,SI_1,PN,YF,FERTADJ)
     call HG_THIN(CYCLG,VERSION,ISPGRP,BABT,BART,YT,THINADJ)
     !      IF(K.EQ.161) THEN
     !        WRITE(*,*) ' PHTGRO = ',PHTGRO
     !        WRITE(*,*) ' HG=',HG,' THINADJ=',THINADJ,' FERTADJ=',FERTADJ
     !        WRITE(*,*) ' CR=',CR,' TCCH=',TCCH
     !        WRITE(*,1600)
     ! 1600   FORMAT(1X,' '\)
     !        IANS = IYN(2)
     !      ENDIF
     GROWTH(K,1) = HG*THINADJ*FERTADJ
     !           GROWTH(K,1)=HG
     call LIMIT(VERSION,TDATAI(K,1),TDATAR(K,1),TDATAR(K,2),&
          &GROWTH(K,2),GROWTH(K,1))
     !           GROWTH(K,3)=GROWTH(K,3)+GROWTH(K,1)
  end if
  return
end

subroutine HTGRO2(K,VERSION,IB,TDATAI,TDATAR,RASI,CALIB,&
     &GROWTH)
  implicit none
  !     CALCULATE 5-YEAR HEIGHT GROWTH

  !     K = TREE NUMBER TO GROW HEIGHT

  ! Variables
  integer*4 :: K,VERSION,IB,TDATAI(2000,3),ISPGRP
  !      INTEGER*4 IANS,IYN
  real*4  :: TDATAR(2000,8),RASI,CALIB(6,18),GROWTH(2000,4),CR,DBH,PDBH,PRDHT1,PRDHT2,PRDHT,GEARA,&
       &RAH1,RAH2,RAHG

  !     CALCULATE HEIGHT GROWTH FOR MINOR SPECIES

  CR = TDATAR(K,3)
  DBH = TDATAR(K,1)
  ISPGRP = TDATAI(K,2)
  !      IF(K .GE. 50) THEN
  !      WRITE(*,*) ' ISPGRP = ',ISPGRP,' RASI = ',RASI,' GEARA = ',GEARA
  !      WRITE(*,*) ' CR = ',CR,' DBH = ',DBH,' EXPAN = ',TDATAR(K,4)
  !      WRITE(*,1600)
  ! 1600 FORMAT(1X,' '\)
  !      IANS = IYN(2)
  !      ENDIF
  if (TDATAI(K,2) .gt. IB) then
     PDBH = DBH-GROWTH(K,2)
     select case(VERSION)
     case(1)
        call HD_SWO(ISPGRP,DBH,PRDHT2)
        call HD_SWO(ISPGRP,PDBH,PRDHT1)
     case(2)
        call HD_NWO(ISPGRP,DBH,PRDHT2)
        call HD_NWO(ISPGRP,PDBH,PRDHT1)
     case(3)
        call HD_SMC(ISPGRP,DBH,PRDHT2)
        call HD_SMC(ISPGRP,PDBH,PRDHT1)
     case(4)
        call HD_RAP(ISPGRP,DBH,PRDHT2)
        call HD_RAP(ISPGRP,PDBH,PRDHT1)
     end select
     PRDHT1 = 4.5+CALIB(1,ISPGRP)*(PRDHT1-4.5)
     PRDHT2 = 4.5+CALIB(1,ISPGRP)*(PRDHT2-4.5)
     PRDHT = (PRDHT2/PRDHT1)*TDATAR(K,2)

     !          RED ALDER HEIGHT GROWTH - ADDED 12/13/02

     if (TDATAI(K,1) .eq.351.and. VERSION .le.3) then
        call RAGEA(TDATAR(K,2),RASI,GEARA)
        !      WRITE(*,*) ' HT = ',TDATAR(K,2),' RASI = ',RASI,' GEARA = ',GEARA
        !      WRITE(*,*) ' HG = ',HG,' THINADJ = ',THINADJ,' FERTADJ = ',FERTADJ
        !      WRITE(*,1600)
        ! 1600 FORMAT(1X,' '\)
        !       IANS = IYN(2)
        if (GEARA .le.0.0) then
           GROWTH(K,1) = 0.0
        else
           call RAH40(GEARA,RASI,RAH1)
           call RAH40(GEARA+5.0,RASI,RAH2)
           RAHG = RAH2-RAH1
           GROWTH(K,1) = RAHG
        end if
     else
        GROWTH(K,1) = PRDHT-TDATAR(K,2)
     end if
     GROWTH(K,3) = GROWTH(K,3)+GROWTH(K,1)
  end if
  return
end

subroutine HS_HG(ISP,SI,HT,GEAGE,PHTGRO)
  implicit none
  ! Variables
  integer*4 :: ISP
  real*4  :: SI,GEAGE,HT,PHTGRO,B0,B1,B2,BBC,X50,A1A,XAI,XAI5

  !     Hann and Scrivani (1987) FRL Research Bulletin 59

  if (ISP .eq.1) then
     B0 = -6.21693
     B1 = 0.281176
     B2 = 1.14354
  else
     B0 = -6.54707
     B1 = 0.288169
     B2 = 1.21297
  end if
  BBC = B0+B1*LOG(SI)
  X50 = 1.0-EXP((-1.)*EXP(BBC+B2*3.912023))
  A1A = 1.0-(HT-4.5)*(X50/SI)
  if (A1A .le.0.) then
     GEAGE = 500.
     PHTGRO = 0.
  else
     GEAGE = ((-1.0*LOG(A1A))/(EXP(B0)*SI**B1))**(1.0/B2)
     XAI = 1.0-EXP(-1.0*EXP(BBC+B2*LOG(GEAGE)))
     XAI5 = 1.0-EXP((-1.)*EXP(BBC+B2*LOG(GEAGE+5.)))
     PHTGRO = (4.5+(HT-4.5)*(XAI5/XAI))-HT
  end if
  return
end

subroutine B_HG(SI,HT,GP,GEAGE,PHTGRO)

  !     Bruce (1981) Forest Science 27: 711-725

  implicit none
  ! Variables
  real*4  :: SI,HT,X1,X2,B1,B2,GP,GEAGE,PHT,XX1,PHTGRO
  X1 = 13.25-SI/20.0
  X2 = 63.25-SI/20.0
  B2 = -0.447762-0.894427*SI/100.0+0.793548*(SI/100.0)**2&
       &-0.171666*(SI/100.0)**3
  B1 = ALOG(4.5/SI)/(X1**B2-X2**B2)
  XX1 = ALOG(HT/SI)/B1+X2**B2
  if (XX1.gt.0.0) then
     GEAGE = XX1**(1.0/B2)-X1
  else
     GEAGE = 500.0
  end if
  PHT = SI*EXP(B1*((GEAGE+GP+X1)**B2-X2**B2))
  PHTGRO = PHT-HT
  return
end

subroutine F_HG(SI,HT,GP,GEAGE,PHTGRO)
  !     For Western Hemlock compute Growth Effective Age and 5-year potential
  !     or 1-year height growth using the western hemlock top height curves of
  !     Flewelling.  These subroutines are required:
  !       SITECV_F   computes top height from site and age
  !       SITEF_C    computes model parameters
  !       SITEF_SI   calculates an approximate psi for a given site
  !       Note: Flewelling's curves are metric.
  !             Site Index is not adjusted for stump height.

  implicit none
  ! Variables
  integer*4 :: I
  real*4  :: SI,GEAGE,HT,GP,PHTGRO,SIM,HTM,AGE,HTOP,PHT,XHTOP1,XHTOP2
  SIM = SI * 0.3048
  HTM = HT * 0.3048

  !     Compute growth effective age

  AGE = 1.0
  do I = 1,4
     5 AGE = AGE + 100./10.**I
     if (AGE .gt.500.0) then
        GEAGE = 500.0
        call SITECV_F(SIM,GEAGE,XHTOP1)
        call SITECV_F(SIM,GEAGE+GP,XHTOP2)
        PHTGRO = 3.2808*(XHTOP2-XHTOP1)
        return
     end if
     call SITECV_F(SIM,AGE,HTOP)
     if (HTOP .lt. HTM) go to 5
     AGE = AGE - 100./10.**I
  end do
  GEAGE = AGE

  !     Compute top height and potential height growth

  call SITECV_F(SIM,GEAGE+GP,HTOP)
  PHT = HTOP*3.2808
  PHTGRO = PHT - HT
  return
end

  !      SUBROUTINE NC_HG(SI,HT,GEAGE,PHTGRO)
  !C
  !C     NIGH AND COURTIN (1998) RED ALDER
  !C
  !      IMPLICIT NONE
  !      REAL*4 SI,HT,GEAGE,PHTGRO,MSI,MHT,X1,X2,AGE,FMHT
  !      MSI=SI/3.28
  !      MHT=HT/3.28
  !      X1=1.693*(MSI-1.3)/(MHT-1.3)
  !      IF(X1 .LT. 1.0001) X1=1.0001
  !      X2=(3.600-ALOG(X1-1.0))/1.240
  !      GEAGE=EXP(X2)+0.5
  !      IF(GEAGE .GT. 150.0) GEAGE=150.0
  !      AGE=GEAGE+1.0
  !      FMHT=1.3+((1.693*(MSI-1.3))/(1.0+EXP(3.600-1.240*ALOG(AGE-0.5))))
  !      PHTGRO=3.28*(FMHT-MHT)
  !      RETURN
  !      END

subroutine WHHLB_HG(SI_C,PDEN,HT,GP,GEA,POTHGRO)

  !     WEISKITTEL, HANN, HIBBS, LAM, AND BLUHM DOMINANT HEIGHT GROWTH INCREMENT
  !     EQUATION FOR RED ALDER

  implicit none
  ! Variables
  real*4  :: SI_C,PDEN,HT,GP,GEA,POTHGRO,SI_UC,PHT,A
  call WHHLB_SI_UC_RUN(SI_C,PDEN,SI_UC)
  call WHHLB_GEA_RUN(HT,SI_UC,GEA)
  A = GEA+GP
  call WHHLB_H40_RUN(HT,GEA,A,PHT)
  POTHGRO = PHT-HT
  return
end

subroutine WHHLB_SI_UC_RUN(SI_C,PDEN,SI_UC)

  !     UNCORRECTS THE DENSITY INPACT UPON THE WEISKITTEL, HANN, HIBBS, LAM, AND BLUHN
  !          SITE INDEX FOR RED ALDER

  implicit none
  ! Variables
  real*4  :: SI_C,PDEN,SI_UC

  !     SITE INDEX UNCORRECTED FOR DENSITY EFFECT

  SI_UC = SI_C*(1.0-0.326480904*EXP(-0.000400268678*PDEN**1.5))
  return
end

subroutine WHHLB_GEA_RUN(H,SI_UC,GEA)

  !     RED ALDER GROWTH EFFECTIVE AGE EQUATION BASED ON H40 EQUATION FROM
  !         THE WEISKITTEL, HANN, HIBBS, LAM, AND BLUHM DOMINANT HEIGHT GROWTH
  !         EQUATION

  implicit none
  ! Variables
  real*4  :: H,SI_UC,GEA,B1,B2,X
  B1 = -4.481266
  B2 = -0.658884
  X = (1.0/B1)*ALOG(H/SI_UC)+20.0**B2
  if (X .lt.0.03) X = 0.03
  GEA = X**(1.0/B2)
  return
end

subroutine WHHLB_H40_RUN(H40M,TAGEM,TAGEP,PH40P)

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

subroutine HG_SWO(ISPGRP,PHTGRO,CR,TCCH,HG)
  implicit none
  ! Variables
  real*4  :: PHTGRO,CR,TCCH,HG,HGPAR(5,8),P1,P2,P3,P4,P5,P6,P7,P8,FCR,B0,B1,MODIFER,CRADJ
  integer*4 :: ISPGRP

  !  HEIGHT GROWTH PARAMETERS (8 parameters - big 5 conifers only)

  !     DF Coefficients from Hann and Hanus (2002) FRL Research Contribution 41
  !     GW Coefficients from Hann and Hanus (2002) FRL Research Contribution 41
  !     PP Coefficients from Hann and Hanus (2002) FRL Research Contribution 41
  !     SP Coefficients from Hann and Hanus (2002) FRL Research Contribution 41
  !     IC Coefficients from Hann and Hanus (2002) FRL Research Contribution 41

  data HGPAR/&
       &1.0       ,  1.0       ,  1.0       ,  1.0       ,  1.0       ,& ! DF,GW,PP,SP,IC

       &-0.02457621, -0.01453250, -0.14889850, -0.14889850, -0.01453250,& ! DF,GW,PP,SP,IC

       &-0.00407303, -0.00407303, -0.00322752, -0.00678955, -0.00637434,& ! DF,GW,PP,SP,IC

       &1.0       ,  1.0       ,  1.0       ,  1.0       ,  1.0       ,& ! DF,GW,PP,SP,IC

       &2.89556338,  7.69023575,  0.92071847,  0.92071847,  1.27228638,& ! DF,GW,PP,SP,IC

       &2.0       ,  2.0       ,  2.0       ,  2.0       ,  2.0       ,& ! DF,GW,PP,SP,IC

       &0.0       ,  0.0       ,  0.0       ,  0.0       ,  0.0       ,& ! DF,GW,PP,SP,IC

       &1.0       ,  1.0       ,  1.0       ,  1.0       ,  1.0/ ! DF,GW,PP,SP,IC

  P1 = HGPAR(ISPGRP,1)
  P2 = HGPAR(ISPGRP,2)
  P3 = HGPAR(ISPGRP,3)
  P4 = HGPAR(ISPGRP,4)
  P5 = HGPAR(ISPGRP,5)
  P6 = HGPAR(ISPGRP,6)
  P7 = HGPAR(ISPGRP,7)
  P8 = HGPAR(ISPGRP,8)
  FCR = (-P5*(1.0-CR)**P6)*EXP(P7*TCCH**0.5)
  B0 = P1*EXP(P2*TCCH)
  B1 = EXP(P3*TCCH**P4)
  if (FCR .lt. -20.) then
     MODIFER = P8*B0
  else
     MODIFER = P8*(B0+(B1-B0)*EXP(FCR))
  end if
  CRADJ = 1.0
  if (CR .le.0.17) CRADJ = 1.0-EXP(-(25.0*CR)**2.0)
  HG = PHTGRO*MODIFER*CRADJ
  return
end

subroutine HG_NWO(ISPGRP,PHTGRO,CR,TCCH,HG)
  implicit none
  ! Variables
  real*4  :: PHTGRO,CR,TCCH,HG,HGPAR(3,8),P1,P2,P3,P4,P5,P6,P7,P8,FCR,B0,B1,MODIFER,CRADJ
  integer*4 :: ISPGRP

  !  HEIGHT GROWTH PARAMETERS (8 parameters - big 3 conifers only)

  !     DF Coefficients from Hann, Marshall, and Hanus (2006) FRL Research Contribution ??
  !     GF Coefficients from Ritchie and Hann (1990) FRL Research Paper 54
  !     WH Coefficients from Johnson (2002) Willamette Industries Report

  data HGPAR/&
       &0.655258886 ,  1.0         ,  1.0         ,& !  DF,GF,WH

       &-0.006322913 , -0.0328142   , -0.0384415   ,& !  DF,GF,WH

       &-0.039409636 , -0.0127851   , -0.0144139   ,& !  DF,GF,WH

       &0.5         ,  1.0         ,  0.5         ,& !  DF,GF,WH

       &0.597617316 ,  6.19784     ,  1.04409     ,& !  DF,GF,WH

       &2.0         ,  2.0         ,  2.0         ,& !  DF,GF,WH

       &0.631643636 ,  0.0         ,  0.0         ,& !  DF,GF,WH

       &1.010018427 ,  1.01        ,  1.03        / !  DF,GF,WH

  P1 = HGPAR(ISPGRP,1)
  P2 = HGPAR(ISPGRP,2)
  P3 = HGPAR(ISPGRP,3)
  P4 = HGPAR(ISPGRP,4)
  P5 = HGPAR(ISPGRP,5)
  P6 = HGPAR(ISPGRP,6)
  P7 = HGPAR(ISPGRP,7)
  P8 = HGPAR(ISPGRP,8)
  FCR = (-P5*(1.0-CR)**P6)*EXP(P7*TCCH**0.5)
  B0 = P1*EXP(P2*TCCH)
  B1 = EXP(P3*TCCH**P4)
  if (FCR .lt. -20.) then
     MODIFER = P8*B0
  else
     MODIFER = P8*(B0+(B1-B0)*EXP(FCR))
  end if
  CRADJ = 1.0
  if (CR.le.0.17) CRADJ = 1.0-EXP(-(25.0*CR)**2.0)
  HG = PHTGRO*MODIFER*CRADJ
  return
end

subroutine HG_SMC(ISPGRP,PHTGRO,CR,TCCH,HG)
  implicit none
  ! Variables
  real*4  :: PHTGRO,CR,TCCH,HG,HGPAR(3,8),P1,P2,P3,P4,P5,P6,P7,P8,FCR,B0,B1,MODIFER,CRADJ
  integer*4 :: ISPGRP
  !C
  !C  HEIGHT GROWTH PARAMETERS (8 parameters - big 3 conifers only)
  !C
  !C     DF Coefficients from Hann, Marshall, and Hanus (2006) FRL Research Contribution ??
  !C     GF Coefficients from Ritchie and Hann (1990) FRL Research Paper 54
  !C     WH Coefficients from Hann, Marshall, and Hanus (2003) FRL Research Contribution 40
  !C
  !      DATA HGPAR/
  !     1           0.655258886,  1.0      ,  1.0         ,               !  DF,GF,WH
  !C
  !     2          -0.006322913, -0.0328142, -0.0056949357,               !  DF,GF,WH
  !C
  !     3          -0.039409636, -0.0127851, -0.0018047267,               !  DF,GF,WH
  !C
  !     4           0.5        ,  1.0      ,  0.5         ,               !  DF,GF,WH
  !C
  !     5           0.597617316,  6.19784  ,  6.1978      ,               !  DF,GF,WH
  !C
  !     6           2.0        ,  2.0      ,  2.0         ,               !  DF,GF,WH
  !C
  !     7           0.631643636,  0.0      ,  0.0         ,               !  DF,GF,WH
  !C
  !     8           1.010018427,  1.01     ,  1.03        /               !  DF,GF,WH

  !  HEIGHT GROWTH PARAMETERS (8 parameters - big 3 conifers only)

  !     DF Coefficients from Hann, Marshall, and Hanus (2006) FRL Research Contribution ??
  !     GF Coefficients from Ritchie and Hann (1990) FRL Research Paper 54
  !     WH Coefficients from Johnson (2002) Willamette Industries Report

  data HGPAR/&
       &0.655258886 ,  1.0         ,  1.0         ,& !  DF,GF,WH

       &-0.006322913 , -0.0328142   , -0.0384415   ,& !  DF,GF,WH

       &-0.039409636 , -0.0127851   , -0.0144139   ,& !  DF,GF,WH

       &0.5         ,  1.0         ,  0.5         ,& !  DF,GF,WH

       &0.597617316 ,  6.19784     ,  1.04409     ,& !  DF,GF,WH

       &2.0         ,  2.0         ,  2.0         ,& !  DF,GF,WH

       &0.631643636 ,  0.0         ,  0.0         ,& !  DF,GF,WH

       &1.010018427 ,  1.01        ,  1.03        / !  DF,GF,WH

  P1 = HGPAR(ISPGRP,1)
  P2 = HGPAR(ISPGRP,2)
  P3 = HGPAR(ISPGRP,3)
  P4 = HGPAR(ISPGRP,4)
  P5 = HGPAR(ISPGRP,5)
  P6 = HGPAR(ISPGRP,6)
  P7 = HGPAR(ISPGRP,7)
  P8 = HGPAR(ISPGRP,8)
  FCR = (-P5*(1.0-CR)**P6)*EXP(P7*TCCH**0.5)
  B0 = P1*EXP(P2*TCCH)
  B1 = EXP(P3*TCCH**P4)
  if (FCR .lt. -20.) then
     MODIFER = P8*B0
  else
     MODIFER = P8*(B0+(B1-B0)*EXP(FCR))
  end if
  CRADJ = 1.0
  if (CR.le.0.17) CRADJ = 1.0-EXP(-(25.0*CR)**2.0)
  HG = PHTGRO*MODIFER*CRADJ
  return
end

subroutine HG_RAP(ISPGRP,PHTGRO,CR,TCCH,HG)
  implicit none
  ! Variables
  real*4  :: PHTGRO,CR,TCCH,HG,HGPAR(3,8),P1,P2,P3,P4,P5,P6,P7,P8,FCR,B0,B1,MODIFER,CRADJ
  integer*4 :: ISPGRP
  !C
  !C  HEIGHT GROWTH PARAMETERS (8 parameters - 3 species only)
  !C
  !     RA Coefficients from Hann, Bluhm, and Hibbs Red Alder Plantation Analysis
  !     DF Coefficients from Hann, Marshall, and Hanus (2006) FRL Research Contribution ??
  !     WH Coefficients from Johnson (2002) Willamette Industries Report

  ! WEIGHTED SUMMATION PROCEDURE PARAMETERS FOR RED ALDER

  data HGPAR/&
       &0.809837005 ,  0.655258886 ,  1.0         ,& !  RA,DF,WH

       &-0.0134163653, -0.006322913 , -0.0384415   ,& !  RA,DF,WH

       &-0.0609398629, -0.039409636 , -0.0144139   ,& !  RA,DF,WH

       &0.5         ,  0.5         ,  0.5         ,& !  RA,DF,WH

       &1.0         ,  0.597617316 ,  1.04409     ,& !  RA,DF,WH

       &2.0         ,  2.0         ,  2.0         ,& !  RA,DF,WH

       &0.1469442410,  0.631643636 ,  0.0         ,& !  RA,DF,WH

       &1.0476380753,  1.010018427 ,  1.03        / !  RA,DF,WH

  !C
  !C WEIGHTED CENTRAL PAI PROCEDURE PARAMETERS FOR RED ALDER
  !C
  !      DATA HGPAR/
  !     1       0.775118127 ,  0.655258886 ,  1.0         ,   !  RA,DF,WH
  !C
  !     2      -0.0128743358, -0.006322913 , -0.0384415   ,   !  RA,DF,WH
  !C
  !     3      -0.070294082 , -0.039409636 , -0.0144139   ,   !  RA,DF,WH
  !C
  !     4       0.5         ,  0.5         ,  0.5         ,   !  RA,DF,WH
  !C
  !     5       1.0         ,  0.597617316 ,  1.04409     ,   !  RA,DF,WH
  !C
  !     6       2.0         ,  2.0         ,  2.0         ,   !  RA,DF,WH
  !C
  !     7       0.120539836 ,  0.631643636 ,  0.0         ,   !  RA,DF,WH
  !C
  !     8       1.07563185  ,  1.010018427 ,  1.03        /   !  RA,DF,WH
  !C
  P1 = HGPAR(ISPGRP,1)
  P2 = HGPAR(ISPGRP,2)
  P3 = HGPAR(ISPGRP,3)
  P4 = HGPAR(ISPGRP,4)
  P5 = HGPAR(ISPGRP,5)
  P6 = HGPAR(ISPGRP,6)
  P7 = HGPAR(ISPGRP,7)
  P8 = HGPAR(ISPGRP,8)
  FCR = (-P5*(1.0-CR)**P6)*EXP(P7*TCCH**0.5)
  B0 = P1*EXP(P2*TCCH)
  B1 = EXP(P3*TCCH**P4)
  if (FCR .lt. -20.) then
     MODIFER = P8*B0
  else
     MODIFER = P8*(B0+(B1-B0)*EXP(FCR))
  end if
  CRADJ = 1.0
  if (CR.le.0.17) CRADJ = 1.0-EXP(-(25.0*CR)**2.0)
  HG = PHTGRO*MODIFER*CRADJ
  return
end

subroutine LIMIT(VERSION,ISP,DBH,HT,DG,HG)
  implicit none
  ! Variables
  real*4  :: DBH,HT,DG,HG,A0,A1,A2,HT1,HT2,HT3,DBH1,DBH2,DBH3,PHT1,PHT2,PHT3,PHGR1,PHGR2
  integer*4 :: VERSION,ISP,JSP
  JSP = ISP
  if (ISP .eq.263.and. VERSION .eq.1) JSP = 15
  select case(JSP)
  case(202,263)
     A0 = 19.04942539
     A1 = -0.04484724
     A2 = 1.0
  case(17,15)
     A0 = 16.26279948
     A1 = -0.04484724
     A2 = 1.0
  case(122)
     A0 = 17.11482201
     A1 = -0.04484724
     A2 = 1.0
  case(117)
     A0 = 14.29011403
     A1 = -0.04484724
     A2 = 1.0
  case(351)
     A0 = 60.619859
     A1 = -1.59138564
     A2 = 0.496705997
  case default
     A0 = 15.80319194
     A1 = -0.04484724
     A2 = 1.0
  end select
  HT1 = HT-4.5
  HT2 = HT1+HG
  HT3 = HT2+HG
  DBH1 = DBH
  DBH2 = DBH1+DG
  DBH3 = DBH2+DG
  PHT1 = A0*DBH1/(1.0-A1*DBH1**A2)
  PHT2 = A0*DBH2/(1.0-A1*DBH2**A2)
  PHT3 = A0*DBH3/(1.0-A1*DBH3**A2)
  PHGR1 = (PHT2-PHT1+HG)/2.0
  PHGR2 = PHT2-HT1
  if (HT2.gt.PHT2) then
     if (PHGR1.lt.PHGR2) then
        HG = PHGR1
     else
        HG = PHGR2
     end if
  else if (HT3.gt.PHT3) then
     HG = PHGR1
  end if
  if (HG.lt.0.0) then
     HG = 0.0
  end if
  return
end

subroutine HD_SWO(ISPGRP,DBH,PRDHT)
  implicit none
  ! Variables
  real*4  :: DBH,PRDHT,HDPAR(18,3),B0,B1,B2
  integer*4 :: ISPGRP

  !  NEW HEIGHT/DIAMETER PARAMETERS FOR UNDAMAGED TREES. EXCEPT RC, WO, AND RA (3 parameters - all species)

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
  !     BL Coefficients Hanus, Hann and Marshall (1999) FRL Research Contribution 27
  !     WO Coefficients from Gould, Marshall, and Harrington (2008) West. J. Appl. For. 23: 26-33
  !     BO Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
  !     RA Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #1
  !     PD Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27
  !     WI Coefficients from Hanus, Hann and Marshall (1999) FRL Research Contribution 27

  data HDPAR/&
       &7.133682298,  6.75286569,  6.27233557,  5.81876360,& !  DF,GW,PP,SP
       &10.04621768 ,  6.58804   ,  6.14817441,  5.10707208,& !  IC,WH,RC,PY
       &6.53558288 ,  9.2251518 ,  8.49655416,  9.01612971,& !  MD,GC,TA,CL
       &5.20018445 ,  4.69753118,  5.04832439,  5.59759126,& !  BL,WO,BO,RA
       &7.49095931 ,  3.26840527,& !  PD,WI

       &-5.433744897, -5.52614439, -5.57306985, -5.31082668,& !  DF,GW,PP,SP
       &-8.72915115,  -5.25312496, -5.40092761, -3.28638769,& !  IC,WH,RC,PY
       &-4.69059053,  -7.65310387, -6.68904033, -7.34813829,& !  MD,GC,TA,CL
       &-2.86671078,  -3.51586969, -3.32715915, -3.19942952,& !  BL,WO,BO,RA
       &-5.40872209,  -0.95270859,& !  PD,WI

       &-0.266398088, -0.33012156, -0.40384171, -0.47349388,& !  DF,GW,PP,SP
       &-0.14040106,  -0.31895401, -0.38922036, -0.24016101,& !  IC,WH,RC,PY
       &-0.24934807,  -0.15480725, -0.16105112, -0.134025626,& !  MD,GC,TA,CL
       &-0.42255220,  -0.57665068, -0.43456034, -0.38783403,& !  BL,WO,BO,RA
       &-0.16874962,  -0.98015696/ !  PD,WI

  B0 = HDPAR(ISPGRP,1)
  B1 = HDPAR(ISPGRP,2)
  B2 = HDPAR(ISPGRP,3)
  PRDHT = 4.5+EXP(B0+B1*DBH**B2)
  return
end

subroutine HD_NWO(ISPGRP,DBH,PRDHT)
  implicit none
  ! Variables
  real*4  :: DBH,PRDHT,HDPAR(11,3),B0,B1,B2
  integer*4 :: ISPGRP

  !  HEIGHT/DIAMETER PARAMETERS (3 parameters - all species)

  !     DF Coefficients from Wang and Hann (1988) FRL Research Paper 51
  !     GF Coefficients from Wang and Hann (1988) FRL Research Paper 51
  !     WH Coefficients from Johnson (2000) Willamette Industries Report
  !     RC Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #2
  !     PY Coefficients from Wang and Hann (1988) FRL Research Paper 51
  !     MD Coefficients from Wang and Hann (1988) FRL Research Paper 51
  !     BL Coefficients from Wang and Hann (1988) FRL Research Paper 51
  !     WO Coefficients from Gould, Marshall, and Harrington (2008) West. J. Appl. For. 23: 26-33
  !     RA Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #1
  !     PD Coefficients from Wang and Hann (1988) FRL Research Paper 51
  !     WI Coefficients from Wang and Hann (1988) FRL Research Paper 51

  data HDPAR/&
       &7.04524    ,  7.42808    ,  5.93792    ,  6.14817441,& !  DF,GF,WH,RC
       &9.30172    ,  5.84487    ,  5.21462    ,  4.69753118,& !  PY,MD,BL,WO
       &5.59759126 ,  4.49727    ,  4.88361    ,& !  RA,PD,WI

       &-5.16836    , -5.80832    , -4.43822    , -5.40092761,& !  DF,GF,WH,RC
       &-7.50951    , -3.84795    , -2.70252    , -3.51586969,& !  PY,MD,BL,WO
       &-3.19942952 , -2.07667    , -2.47605    ,& !  RA,PD,WI

       &-0.253869   ,  -0.240317  , -0.411373   , -0.38922036,& !  DF,GF,WH,RC
       &-0.100000   ,  -0.289213  , -0.354756   , -0.57665068,& !  PY,MD,BL,WO
       &-0.38783403 ,  -0.388650  , -0.309050  / !  RA,PD,WI

  B0 = HDPAR(ISPGRP,1)
  B1 = HDPAR(ISPGRP,2)
  B2 = HDPAR(ISPGRP,3)
  PRDHT = 4.5+EXP(B0+B1*DBH**B2)
  return
end

subroutine HD_SMC(ISPGRP,DBH,PRDHT)
  implicit none
  ! Variables
  real*4  :: DBH,PRDHT,HDPAR(11,3),B0,B1,B2
  integer*4 :: ISPGRP

  !  HEIGHT/DIAMETER PARAMETERS (3 parameters - all species)

  !     DF Coefficients from Hanus, Marshall, and Hann (1999) FRL Research Contribution 25
  !     GF Coefficients from Wang and Hann (1988) FRL Research Paper 51
  !     WH Coefficients from Hanus, Marshall, and Hann (1999) FRL Research Contribution 25
  !     RC Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #2
  !     PY Coefficients from Wang and Hann (1988) FRL Research Paper 51
  !     MD Coefficients from Wang and Hann (1988) FRL Research Paper 51
  !     BL Coefficients from Wang and Hann (1988) FRL Research Paper 51
  !     WO Coefficients from Gould, Marshall, and Harrington (2008) West. J. Appl. For. 23: 26-33
  !     RA Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #1
  !     PD Coefficients from Wang and Hann (1988) FRL Research Paper 51
  !     WI Coefficients from Wang and Hann (1988) FRL Research Paper 51

  data HDPAR/&
       &7.262195456,  7.42808    ,  6.555344622,  6.14817441,& !  DF,GF,WH,RC
       &9.30172    ,  5.84487    ,  5.21462    ,  4.69753118,& !  PY,MD,BL,WO
       &5.59759126 ,  4.49727    ,  4.88361    ,& !  RA,PD,WI

       &-5.899759104, -5.80832    , -5.137174162, -5.40092761,& !  DF,GF,WH,RC
       &-7.50951    , -3.84795    , -2.70252    , -3.51586969,& !  PY,MD,BL,WO
       &-3.19942952 , -2.07667    , -2.47605    ,& !  RA,PD,WI

       &-0.287207389, -0.240317   , -0.364550800, -0.38922036,& !  DF,GF,WH,RC
       &-0.100000   ,  -0.289213  , -0.354756   , -0.57665068,& !  PY,MD,BL,WO
       &-0.38783403 ,  -0.388650  , -0.309050  / !  RA,PD,WI

  B0 = HDPAR(ISPGRP,1)
  B1 = HDPAR(ISPGRP,2)
  B2 = HDPAR(ISPGRP,3)
  PRDHT = 4.5+EXP(B0+B1*DBH**B2)
  return
end

subroutine HD_RAP(ISPGRP,DBH,PRDHT)
  implicit none
  ! Variables
  real*4  :: DBH,PRDHT,HDPAR(7,3),B0,B1,B2
  integer*4 :: ISPGRP

  !  HEIGHT/DIAMETER PARAMETERS (3 parameters - all species)

  !     RA Coefficients from Hann, Bluhm, and Hibbs (2011) Forest Biometrics Research Paper 1
  !     DF Coefficients from Hanus, Marshall, and Hann (1999) FRL Research Contribution 25
  !     WH Coefficients from Hanus, Marshall, and Hann (1999) FRL Research Contribution 25
  !     RC Coefficients from Hann and Hanus (2002) OSU Department of Forest Management Internal Report #2
  !     BL Coefficients from Wang and Hann (1988) FRL Research Paper 51
  !     PD Coefficients from Wang and Hann (1988) FRL Research Paper 51
  !     WI Coefficients from Wang and Hann (1988) FRL Research Paper 51

  data HDPAR/&
       &6.75650139 ,  7.262195456,  6.555344622,  6.14817441,& !  RA,DF,WH,RC
       &5.21462    ,  4.49727    ,  4.88361    ,& !  BL,PD,WI

       &-4.6252377  , -5.899759104, -5.137174162, -5.40092761,& !  RA,DF,WH,RC
       &-2.70252    , -2.07667    , -2.47605    ,& !  BL,PD,WI

       &-0.23208200 , -0.287207389, -0.364550800, -0.38922036,& !  RA,DF,WH,RC
       &-0.354756   ,  -0.388650  , -0.309050  / !  BL,PD,WI

  B0 = HDPAR(ISPGRP,1)
  B1 = HDPAR(ISPGRP,2)
  B2 = HDPAR(ISPGRP,3)
  PRDHT = 4.5+EXP(B0+B1*DBH**B2)
  return
end

subroutine HG_FERT(CYCLG,VERSION,ISPGRP,SI_1,PN,YF,FERTADJ)
  implicit none
  ! Variables
  real*4  :: SI_1,PN(5),YF(5),FERTADJ,PF1,PF2,PF3,PF4,PF5,FALDWN,XTIME,FERTX1,FERTX2,TEM1,TEM2
  integer*4 :: CYCLG,VERSION,ISPGRP,I
  if (VERSION .le.3) then
     if (ISPGRP .eq.1) then
        PF1 = 1.0
        PF2 = 0.333333333
        PF3 = -1.107409443
        PF4 = -2.133334346
        PF5 = 1.5
     else
        PF1 = 0.0
        PF2 = 1.0
        PF3 = 0.0
        PF4 = 0.0
        PF5 = 1.0
     end if
  else
     PF1 = 0.0
     PF2 = 1.0
     PF3 = 0.0
     PF4 = 0.0
     PF5 = 1.0
  end if
  FALDWN = 1.0
  XTIME = FLOAT(CYCLG)*5.0
  FERTX1 = 0.0
  do I = 2,5
     FERTX1 = FERTX1+(PN(I)/800.0)*EXP((PF3/PF2)*(YF(1)-YF(I)))
  end do

  !     FERTX2=EXP(PF3*(XTIME-YF(1))+PF4*(SI_1/100.0)**PF5)
  !     Break apart equation to limit value used in EXP function.
  !     Value less than -86.0 will cause underflow error with single
  !     precision real variable. 7/12/22 LD

  TEM1 = (PF3*(XTIME-YF(1))+PF4*(SI_1/100.0)**PF5)
  TEM2 = MAX(-86.0,TEM1)
  FERTX2 = EXP(TEM2)

  FERTADJ = 1.0+(PF1*((PN(1)/800.0)+FERTX1)**PF2*FERTX2)*FALDWN
  return
end

subroutine HG_THIN(CYCLG,VERSION,ISPGRP,BABT,BART,YT,THINADJ)
  implicit none
  ! Variables
  real*4  :: BABT,BART(5),YT(5),THINADJ,PT1,PT2,PT3,XTIME,THINX1,THINX2,THINX3,PREM,GP
  integer*4 :: CYCLG,VERSION,ISPGRP,I
  if (VERSION .le.3) then
     if (ISPGRP .eq.1) then
        PT1 = -0.3197415492
        PT2 = 0.7528887377
        PT3 = -0.2268800162
     else
        PT1 = 0.0
        PT2 = 1.0
        PT3 = 0.0
     end if
     GP = 5.0
  else
     if (ISPGRP .eq.1) then
        PT1 = -0.613313694
        PT2 = 1.0
        PT3 = -0.443824038
     else if (ISPGRP .eq.2) then
        !            PT1=-0.3197415492
        PT1 = -0.47842928 ! ANNUALIZED PARAMETER
        PT2 = 0.7528887377
        PT3 = -0.2268800162
     else
        PT1 = 0.0
        PT2 = 1.0
        PT3 = 0.0
     end if
     GP = 1.0
  end if
  XTIME = FLOAT(CYCLG)*GP
  THINX1 = 0.0
  do I = 2,5
     THINX1 = THINX1+BART(I)*EXP((PT3/PT2)*(YT(1)-YT(I)))
  end do
  THINX2 = THINX1+BART(1)
  THINX3 = THINX1+BABT
  if (THINX3.le.0.0) then
     PREM = 0.0
  else
     PREM = THINX2/THINX3
  end if
  if (PREM .gt.0.75) PREM = 0.75
  THINADJ = 1.0+PT1*PREM**PT2*EXP(PT3*(XTIME-YT(1)))
  return
end

subroutine RAGEA(H,SI,GEA)

  !     RED ALDER GROWTH EFFECTIVE AGE EQUATION BASED ON H40 EQUATION FROM
  !         WORTHINGTON, JOHNSON, STAEBLER AND LLOYD (1960) PNW RESEARCH
  !         PAPER 36

  implicit none
  ! Variables
  real*4  :: H,SI,GEA
  GEA = 19.538*H/(SI-0.60924*H)
  return
end

subroutine RAH40(A,SI,H)

  !     RED ALDER H40 EQUATION FROM FROM WORTHINGTON, JOHNSON, STAEBLER
  !         AND LLOYD (1960) PNW RESEARCH PAPER 36

  implicit none
  ! Variables
  real*4  :: A,SI,H
  H = SI/(0.60924+19.538/A)
  return
end
