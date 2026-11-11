!======================!
!   organon : submax   !
!======================!
! ORGANON $Id$
subroutine SUBMAX(TRIAL,VERSION,NTREES,TDATAI,TDATAR,MGEXP,&
     &MSDI_1,MSDI_2,MSDI_3,A1,A2)

  !     CALCULATE THE MAXIMUM SIZE-DENISTY LINE

  implicit none
  ! Variables
  real*4  :: TDATAR(2000,8),MGEXP(2000),MSDI_1,MSDI_2,MSDI_3,A1,A2,KB,TEMPA1,BAGRP(18),TOTBA,PDF,PTF,&
       &PPP,PWH,A1MOD,TFMOD,OCMOD,DBH,EX1,PRA
  integer*4 :: VERSION,NTREES,TDATAI(2000,3),ISPGRP,I
  logical*2 :: TRIAL
  select case(VERSION)
  case(1,2,3)
     !         REINEKE (1933)
     A2 = 0.62305
  case(4)
     !         PUETTMANN ET AL. (1993)
     A2 = 0.64
  end select
  KB = 0.005454154
  if (MSDI_1.gt.0.0) then
     TEMPA1 = LOG(10.0)+A2*LOG(MSDI_1)
  else
     select case(VERSION)
     case(1)
        !            ORIGINAL SWO-ORGANON - Max. SDI = 530.2
        TEMPA1 = 6.21113
     case(2)
        !            ORIGINAL WWV-ORGANON - Max. SDI = 520.5
        TEMPA1 = 6.19958
     case(3)
        !            ORIGINAL WWV-ORGANON
        TEMPA1 = 6.19958
     case(4)
        !            PUETTMANN ET AL. (1993)
        TEMPA1 = 5.96
     end select
  end if
  do I = 1,18
     BAGRP(I) = 0.0
  end do
  do I = 1,NTREES
     ISPGRP = TDATAI(I,2)
     DBH = TDATAR(I,1)
     if (TRIAL) then
        EX1 = TDATAR(I,4)-MGEXP(I)
     else
        EX1 = TDATAR(I,4)
     end if
     BAGRP(ISPGRP) = BAGRP(ISPGRP)+KB*DBH**2*EX1
  end do
  TOTBA = 0.0
  do I = 1,3
     TOTBA = TOTBA+BAGRP(I)
  end do
  if (TOTBA .gt.0.0) then
     if (VERSION .le.3) then
        PDF = BAGRP(1)/TOTBA
        PTF = BAGRP(2)/TOTBA
     else
        PRA = BAGRP(1)/TOTBA
        PDF = BAGRP(2)/TOTBA
     end if
  else
     if (VERSION .le.3) then
        PDF = 0.0
        PTF = 0.0
     else
        PRA = 0.0
        PDF = 0.0
     end if
  end if
  !     Modified for both 10/28/94
  select case(VERSION)
  case(1)
     if (MSDI_2.gt.0.0) then
        TFMOD = (LOG(10.0)+A2*LOG(MSDI_2))/TEMPA1
     else
        TFMOD = 1.03481817
     end if
     if (MSDI_3.gt.0.0) then
        OCMOD = (LOG(10.0)+A2*LOG(MSDI_3))/TEMPA1
     else
        OCMOD = 0.9943501
     end if
     if (TOTBA .gt.0.0) then
        PPP = BAGRP(3)/TOTBA
     else
        PPP = 0.0
     end if
     if (PDF.ge.0.5) then
        A1MOD = 1.0
     else if (PTF.ge.0.6666667) then
        A1MOD = TFMOD
     else if (PPP.ge.0.6666667) then
        A1MOD = OCMOD
     else
        A1MOD = PDF+TFMOD*PTF+OCMOD*PPP
     end if
  case(2,3)
     if (MSDI_2.gt.0.0) then
        TFMOD = (LOG(10.0)+A2*LOG(MSDI_2))/TEMPA1
     else
        TFMOD = 1.03481817
     end if
     if (MSDI_3.gt.0.0) then
        OCMOD = (LOG(10.0)+A2*LOG(MSDI_3))/TEMPA1
     else
        !            Based on Johnson's (2000) analysis of Max. SDI for western hemlock
        OCMOD = 1.014293245
     end if
     if (TOTBA .gt.0.0) then
        PWH = BAGRP(3)/TOTBA
     else
        PWH = 0.0
     end if
     if (PDF.ge.0.5) then
        A1MOD = 1.0
     else if (PWH.ge.0.5) then
        A1MOD = OCMOD
     else if (PTF.ge.0.6666667) then
        A1MOD = TFMOD
     else
        A1MOD = PDF+OCMOD*PWH+TFMOD*PTF
     end if
  case(4)
     if (TOTBA .gt.0.0) then
        PWH = BAGRP(3)/TOTBA
     else
        PWH = 0.0
     end if
     A1MOD = 1.0
  end select
  if (A1MOD.le.0.0) A1MOD = 1.0
  A1 = TEMPA1*A1MOD
  return
end
