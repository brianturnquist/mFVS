!======================!
!   organon : orgvol   !
!======================!
! ORGANON $Id$

subroutine VOLCAL(VERSION,SPP,CFTD,CFSH,LOGLL,LOGML,LOGTD,LOGSH,&
     &LOGTA,DBH,HT,CR,VERROR,TERROR,VWARNING,&
     &TWARNING,IERROR,CFVOL,BFVOL)

  !  04/25/2014 - THERE ARE COMMON SUBROUTINE NAMES IN THE SOURCE CODE
  !               USED TO BUILD THE ORGANON DLLS. IN ORDER TO LINK THE
  !               ORGANON SOURCE CODE WITH THE FVS SOURCE CODE WE CHANGED
  !               THE DUPLICATED SUBROUTINE NAMES TO MAKE THEM UNIQUE.

  !  CHANGED THE NAME OF SUBROUTINE/SUBROUTINE CALL - EDIT TO EDIT_VOL
  !  CHANGED THE NAME OF SUBROUTINE/SUBROUTINE CALL - SPGROUP TO SPGROUP_VOL

  implicit none
  ! Variables
  integer*4 :: VERSION,IERROR,SPP
  integer*4 :: SPGRP,VERROR(5),TERROR(4),VWARNING(5),TWARNING,ILOGLL,VERS,SVOL
  real*4  :: CFTD,CFSH,LOGLL,LOGML,LOGTD,LOGSH,LOGTA,DBH,HT,CR,XLOGTA,CFVOL,BFVOL
  logical*2 :: ERROR
  if (CFTD .lt.0.0) CFTD = 0.0
  if (CFSH .lt.0.0) CFSH = 0.0
  if (LOGTD .le.0.0) LOGTD = 6.0
  if (LOGSH .le.0.0) LOGSH = 0.5
  if (LOGTA .le.0.0) LOGTA = 8.0
  if (LOGLL .le.0.0) LOGLL = 32.0
  if (LOGML .le.0.0) LOGML = 8.0
  VERS = VERSION
  if (VERS .lt.1.or. VERS .gt.4) VERS = 1
  call EDIT_VOL(VERS,SPP,CFTD,CFSH,LOGLL,LOGML,LOGTD,LOGSH,LOGTA,&
       &DBH,HT,CR,SPGRP,VERROR,TERROR,VWARNING,TWARNING,ERROR)
  IERROR = 0
  if (ERROR) then
     IERROR = 1
     return
  end if

  !        CALCULATE CF VOLUME FOR TREE

  call CF(VERS,SPP,SPGRP,DBH,HT,CR,CFTD,CFSH,CFVOL)
  SVOL = 1
  ILOGLL = INT(LOGLL)
  XLOGTA = LOGTA/12.0
  call SCRIB_VOL(VERS,SPP,SPGRP,SVOL,ILOGLL,LOGTD,LOGSH,XLOGTA,&
       &LOGML,DBH,HT,CR,BFVOL)
  return
end

subroutine EDIT_VOL(VERS,SPP,CFTD,CFSH,LOGLL,LOGML,LOGTD,LOGSH,&
     &LOGTA,DBH,HT,CR,SPGRP,VERROR,TERROR,VWARNING,&
     &TWARNING,ERROR)
  implicit none
  ! Variables
  integer*4 :: VERS,SPP
  integer*4 :: I,SPGRP,VERROR(5),TERROR(4),VWARNING(5),TWARNING,TEMP
  real*4  :: CFTD,CFSH,LOGLL,LOGML,LOGTD,LOGSH,LOGTA,DBH,HT,CR
  real*4  :: B0,B1,PHT
  logical*2 :: ERROR
  ERROR = .false.
  do I = 1,4
     TERROR(I) = 0
  end do
  TWARNING = 0
  do I = 1,5
     VERROR(I) = 0
     VWARNING(I) = 0
  end do

  !     EDIT VOLUME SPECIFICATIONS FOR ERRORS

  if (CFSH .gt.4.5) then
     VERROR(1) = 1
  end if
  if (LOGML .gt. LOGLL) then
     VERROR(2) = 1
  end if
  if (LOGTD .gt.0.0.and. LOGTD .lt.1.0) then
     VERROR(3) = 1
  end if
  if (LOGSH .gt.4.5) then
     VERROR(4) = 1
  end if
  if (LOGTA .gt.0.0.and. LOGTA .lt.1.0) then
     VERROR(5) = 1
  end if
  do I = 1,5
     if (VERROR(I) .eq.1) then
        ERROR = .true.
        return
     end if
  end do

  !     EDIT TREE ATTRIBUTES FOR ERRORS

  call CKSP(VERS,SPP,TEMP)
  TERROR(1) = TEMP
  if (DBH .le.0.09) then
     TERROR(2) = 1
  end if
  if (HT .le.4.5) then
     TERROR(3) = 1
  end if
  if (CR .lt.0.0.or. CR .gt.1.0) then
     TERROR(4) = 1
  end if
  do I = 1,4
     if (TERROR(I) .eq.1) then
        ERROR = .true.
        return
     end if
  end do
  call SPGROUP_VOL(VERS,SPP,SPGRP)

  !     DETERMINE WARNINGS (IF ANY)

  !     EDIT VOLUME SPECIFICATIONS FOR ERRORS

  if (CFTD .gt.12.0) then
     VWARNING(1) = 1
  end if
  if (LOGLL .lt.8.0.or. LOGLL .gt.40.0) then
     VWARNING(2) = 1
  end if
  if (LOGML .lt.8.0.or. LOGML .gt.40.0) then
     VWARNING(3) = 1
  end if
  if (LOGTD .gt.12.0) then
     VWARNING(4) = 1
  end if
  if (LOGTA .gt.12.0) then
     VWARNING(5) = 1
  end if
  B1 = -0.04484724
  select case(SPP)
  case(202)
     B0 = 19.04942539
  case(263)
     if (VERS .ge.2)B0 = 19.04942539
  case(17,15)
     B0 = 16.26279948
  case(122)
     B0 = 17.11482201
  case(117)
     B0 = 14.29011403
  case default
     B0 = 15.80319194
  end select
  if (HT .gt.4.5) then
     PHT = 4.5+B0*DBH/(1.0-B1*DBH)
     if (HT .gt. PHT) then
        TWARNING = 1
     end if
  end if
  return
end

subroutine SPGROUP_VOL(VERSION,SPECIES,SPGRP)
  !     DETERMINE SPECIES GROUP THE TREE

  !     I = TREE INDICATOR

  implicit none
  ! Variables
  integer*4 :: VERSION,SPECIES,SPGRP,ISX,J

  integer*4 :: SCODE1(19) = (/            202,15,17,122,117,81,263,242,231,361,431,631,805,312,815,&
       &818,351,492,920 /)

  integer*4 :: SCODE2(11) = (/            202,17,263,242,231,361,312,815,351,492,920 /)

  integer*4 :: SCODE3(7) = (/            351,202,263,242,312,492,920 /)
  ISX = -9999
  select case (VERSION)
  case(1)
     do J = 1, 19
        if (SPECIES .eq. SCODE1(J)) then
           ISX = J
           if (ISX .gt.2) ISX = ISX-1
           exit
        end if
     end do
  case(2,3)
     do J = 1, 11
        if (SPECIES .eq. SCODE2(J)) then
           ISX = J
           exit
        end if
     end do
  case(4)
     do J = 1, 7
        if (SPECIES .eq. SCODE3(J)) then
           ISX = J
           exit
        end if
     end do
  end select
  SPGRP = ISX
  return
end

subroutine CKSP(VERSION,SPECIES,TEMP)
  !     CHECK SPECIES CODE FOR THE TREE

  implicit none
  ! Variables
  integer*4 :: VERSION,SPECIES,J,TEMP
  logical*2 :: BAD

  integer*4 :: SCODE1(19) = (/            202,15,17,122,117,81,263,242,231,361,431,631,805,312,815,&
       &818,351,492,920 /)

  integer*4 :: SCODE2(11) = (/            202,17,263,242,231,361,312,815,351,492,920 /)

  integer*4 :: SCODE3(7) = (/            351,202,263,242,312,492,920 /)
  BAD = .true.
  TEMP = 0
  select case (VERSION)
  case(1)
     do J = 1, 19
        if (SPECIES .eq. SCODE1(J)) then
           BAD = .false.
           exit
        end if
     end do
  case(2,3)
     do J = 1, 11
        if (SPECIES .eq. SCODE2(J)) then
           BAD = .false.
        end if
     end do
  case(4)
     do J = 1, 7
        if (SPECIES .eq. SCODE3(J)) then
           BAD = .false.
        end if
     end do
  end select
  if (BAD) then
     TEMP = 1
  end if
  return
end

subroutine GET_ORGVOL_EDITION(EDITION)
  ! Variables
  real*4  :: EDITION
  EDITION = 9.1
  return
end

subroutine LOG_TABLE(VERSION,NTREES,XLOGLL,LOGML,LOGTD,LOGSH,&
     &XLOGTA,SPP,DBH,XHT,XCR,EXPAN,NL,LVOL)
  implicit none
  ! Variables
  real*4  :: XLOGLL,LOGML,LOGTD,LOGSH,XLOGTA,DBH(2000),XHT(2000),XCR(2000),EXPAN(2000)
  real*4  :: NL(40,4),LVOL(40,4),H,WLT,PP1,PP2,PDIB,DI,X,TOTS(2,4),TLL,DOB,HT,CR,HCB,LOGTA
  integer*4 :: VERSION,NTREES,SPP(2000)
  integer*4 :: ISP,ISPGRP,LOGLL
  integer*4 :: II,SVOL,I,J,NW,NDI,NLOGTD
  real*4  :: EX,MH,AA1,AA2,A3,A4,A,B,C,V,ALP,D,ROOT,HM1,HM2

  LOGTA = XLOGTA/12.0
  LOGLL = INT(XLOGLL)
  do I = 1,40
     do J = 1,4
        NL(I,J) = 0.
        LVOL(I,J) = 0.
     end do
  end do

  do I = 1,NTREES

     !        GET LOG TABLE SPECIES GROUP AND EXP FACTOR

     if (DBH(I) .le. LOGTD) cycle
     if (VERSION .le.3) then
        if (SPP(I) .le.300) then
           if (SPP(I) .eq.202) then
              SVOL = 1
           else if (SPP(I) .eq.15.or. SPP(I) .eq.17) then
              SVOL = 2
           else if ((SPP(I) .eq.117.or. SPP(I) .eq.122)&
                &.and. VERSION .eq.1) then
              SVOL = 3
           else if (SPP(I) .eq.263.and. VERSION .ge.2) then
              SVOL = 3
           else
              SVOL = 4
           end if
        else
           cycle
        end if
     else
        if (SPP(I) .le.300.or. SPP(I) .eq.351) then
           if (SPP(I) .eq.351) then
              SVOL = 1
           else if (SPP(I) .eq.202) then
              SVOL = 2
           else if (SPP(I) .eq.263) then
              SVOL = 3
           else
              SVOL = 4
           end if
        else
           cycle
        end if
     end if

     DOB = DBH(I)
     HT = XHT(I)
     CR = XCR(I)
     EX = EXPAN(I)
     ISP = SPP(I)
     call SPGROUP_VOL(VERSION,ISP,ISPGRP)
     X = HT-4.5
     HCB = (1.0-CR)*HT

     select case(VERSION)
     case(1) ! Southwest Oregon
        call SWO_DIB(ISPGRP,DOB,CR,PDIB)
        call SWO_TAPER(ISPGRP,AA1,AA2,A3,A4,ALP)
     case(2,3) ! Northwest Oregon, SMC
        call NWO_DIB(ISPGRP,DOB,CR,PDIB)
        call NWO_TAPER(ISPGRP,AA1,AA2,A3,A4,ALP)
     case(4) ! Red Alder Plantations
        call RAP_DIB(ISPGRP,DOB,PDIB)
        call RAP_TAPER(ISPGRP,AA1,AA2,A3,A4,ALP)
     end select
     WLT = (ALP*HCB-4.5)/X
     PP1 = AA1+AA2*(X/DOB)+A3*(X/DOB)**2
     PP2 = A4
     if (PDIB .le. LOGTD) cycle

     !        COMPUTE MERCHANTABLE HEIGHT

     if (VERSION .eq.4.and. ISP .eq.351) then
        call RA_MH(DOB,HT,CR,LOGTD,MH)
        go to 21
     end if
     D = 0.0
     H = ALP*HCB
     call LOGVOL(1,SVOL,LOGLL,WLT,H,X,PP1,PP2,PDIB,D,TLL,EX,DI,V,&
          &NL,LVOL,TOTS)
     if (WLT .le.0.0) then
        A = -(PP1+1.0)/X**2
        B = PP1/X
        C = 1.0-LOGTD/PDIB
     else if (WLT .gt.0..and. DI .le. LOGTD) then
        A = PP2/X**2
        B = PP1/X
        C = 1.0-LOGTD/PDIB
     else
        A = (PP2*WLT**2-PP1-2*PP2*WLT-1.0)/(X**2*(WLT-1.0)**2)
        B = ((2*WLT-1.0+PP2*WLT**2+PP1*WLT**2)-(PP2*WLT**2-PP1-&
             &2*PP2*WLT-1.0))/(X*(WLT-1.0)**2)
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

     !        CHECK MERCHANTABLE LOG DIAMETERS

     if (HM1.gt.0.0) then
        D = 1.0
        H = HM1+4.5

        call LOGVOL(4,SVOL,LOGLL,WLT,H,X,PP1,PP2,PDIB,D,TLL,EX,DI,&
             &V,NL,LVOL,TOTS)
        NDI = NINT(DI*10.0)
        NLOGTD = NINT(LOGTD*10.0)
        if (NDI .ne. NLOGTD) HM1 = 0.0
     else
        HM1 = 0.0
     end if
     if (HM2.gt.0.0) then
        D = 1.0
        H = HM2+4.5
        call LOGVOL(4,SVOL,LOGLL,WLT,H,X,PP1,PP2,PDIB,D,TLL,EX,DI,&
             &V,NL,LVOL,TOTS)
        NDI = NINT(DI*10.0)
        NLOGTD = NINT(LOGTD*10.0)
        if (NDI .ne. NLOGTD) HM2 = 0.0
     else
        HM2 = 0.0
     end if
     if (HM1.lt.0..or. HM1.gt.X) then
        if (HM2.lt.0..or. HM2.gt.X) then
           MH = 0.
           go to 21
        else
           MH = HM2
        end if
     else if (HM2.lt.0..or. HM2.gt.X) then
        MH = HM1
     else
        MH = MAX(HM1,HM2)
     end if
     MH = MH+4.5
     21 continue

     !        CALCULATE LOG VOLUMES

     NW = INT((MH-LOGSH)/(FLOAT(LOGLL)+LOGTA))
     if (NW .lt.0) NW = 0
     TLL = MH-LOGSH-FLOAT(NW)*(FLOAT(LOGLL)+LOGTA)

     H = LOGSH
     do II = 1,NW
        D = 1.0
        H = H+FLOAT(LOGLL)+LOGTA
        if (VERSION .eq.4.and. ISP .eq.351) then
           call RA_LOGVOL(2,DOB,HT,CR,SVOL,LOGLL,H,D,TLL,EX,DI,V,NL,&
                &LVOL,TOTS)
        else
           call LOGVOL(2,SVOL,LOGLL,WLT,H,X,PP1,PP2,PDIB,D,TLL,EX,&
                &DI,V,NL,LVOL,TOTS)
        end if
     end do

     !        COMPUTE VOLUME OF TOP LOG

     if (TLL .ge. (LOGML+LOGTA)) then
        J = INT(TLL-LOGTA)
        TLL = FLOAT(J)+LOGTA
        D = TLL/FLOAT(LOGLL)
        H = H+TLL
        if (VERSION .eq.4.and. ISP .eq.351) then
           call RA_LOGVOL(2,DOB,HT,CR,SVOL,LOGLL,H,D,TLL,EX,DI,V,NL,&
                &LVOL,TOTS)
        else
           call LOGVOL(2,SVOL,LOGLL,WLT,H,X,PP1,PP2,PDIB,D,TLL,EX,&
                &DI,V,NL,LVOL,TOTS)
        end if
     end if
  end do
  return
end
