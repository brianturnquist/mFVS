!========================!
!   organon : statsorg   !
!========================!
! ORGANON $Id$

subroutine SSTATS(VERSION,NTREES,TDATAI,TDATAR,SBA,TPA,SCCF,&
     &BAL,BALL,CCFL,CCFLL)
  implicit none

  !     ROUTINE TO CALCULATE STAND LEVEL,BAL AND CCFL STATISTICS

  ! Variables
  integer*4 :: VERSION,NTREES,TDATAI(2000,3),I,ISPGRP,L,K
  real*4  :: TDATAR(2000,8),SBA,TPA,SCCF,BAL(500),BALL(51),CCFL(500),CCFLL(51),DBH,HT,EXPAN,BA,MCW,CCF

  do I = 1,500
     CCFL(I) = 0.0
     BAL(I) = 0.0
  end do
  do I = 1,51
     CCFLL(I) = 0.0
     BALL(I) = 0.0
  end do
  SBA = 0.0
  SCCF = 0.0
  TPA = 0.0
  do I = 1,NTREES
     if (TDATAR(I,4) .lt.0.0001) cycle
     ISPGRP = TDATAI(I,2)
     DBH = TDATAR(I,1)
     HT = TDATAR(I,2)
     EXPAN = TDATAR(I,4)
     BA = DBH*DBH*EXPAN*0.005454154
     SBA = SBA+BA
     TPA = TPA+EXPAN
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
     CCF = 0.001803*MCW**2*EXPAN
     SCCF = SCCF+CCF
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
        L = INT(DBH*10.0+.5)
        do K = 1,L-1
           CCFL(K) = CCFL(K)+CCF
           BAL(K) = BAL(K)+BA
        end do
     end if
  end do
  return
end

subroutine HTFORTY(CTMUL,VERSION,IB,NTREES,TDATAI,TDATAR,MGEXP,&
     &HT40)
  implicit none

  !     DETERMINE HEIGHT OF THE FORTY LARGEST BIG-6 TREES PER ACRE

  !          CTMULT = CUT TREE MULTIPLIER
  !                 =  0.0 TO NOT ADD OR SUBTRACT CUT TREES
  !                 =  1.0 TO ADD CUT TREES
  !                 = -1.0 TO SUBTRACT CUT TREES
  ! Variables
  integer*4 :: VERSION,IB,NTREES,TDATAI(2000,3),I,ID,IIB
  real*4  :: CTMUL,TDATAR(2000,8),MGEXP(2000),HT40,TOTHT,TOTTR,HTCL(100),TRCL(100),EXPAN,TRDIFF

  TOTHT = 0.0
  TOTTR = 0.0
  do I = 1,100
     HTCL(I) = 0.0
     TRCL(I) = 0.0
  end do
  IIB = IB
  if (VERSION .eq.4) IIB = 1
  do I = 1,NTREES
     if (TDATAI(I,2).le.IIB) then
        ID = IFIX(TDATAR(I,1))+1
        if (ID.gt.100) ID = 100
        EXPAN = TDATAR(I,4)+CTMUL*MGEXP(I)
        HTCL(ID) = HTCL(ID)+TDATAR(I,2)*EXPAN
        TRCL(ID) = TRCL(ID)+EXPAN
     end if
  end do
  do I = 100,1,-1
     TOTHT = TOTHT+HTCL(I)
     TOTTR = TOTTR+TRCL(I)
     if (TOTTR .gt.40.0) then
        TRDIFF = TRCL(I)-(TOTTR-40.0)
        TOTHT = TOTHT-HTCL(I)+((HTCL(I)/TRCL(I))*TRDIFF)
        TOTTR = 40.0
        go to 20
     end if
  end do
  20 if (TOTTR.gt.0.0) then
     HT40 = TOTHT/TOTTR
  else
     HT40 = 0.0
  end if
  return
end

subroutine RASITE(H,A,SI)
  implicit none

  !     RED ALDER SITE INDEX EQUATION FROM WORTHINGTON, JOHNSON, STAEBLER
  !         AND LLOYD (1960) PNW RESEARCH PAPER 36

  ! Variables
  real*4  :: H,A,SI
  SI = (0.60924+19.538/A)*H
  return
end

function CON_RASI(SI_1)
  implicit none
  ! Variables
  real*4  :: CON_RASI,SI_1
  CON_RASI = 9.73+0.64516*(SI_1+4.5)
  return
end
