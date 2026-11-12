!=======================!
!   organon : prepare   !
!=======================!
! ORGANON $Id$

  !     ORGANON GROWTH AND YIELD MODEL
  !     SUBROUTINES INCLUDED:
  !        PREPARE
  !        EDIT
  !        SPGROUP
  !        CKSP
  !        CKAGE
  !        HSHG
  !        BHG
  !        FHG
  !C        NCHG
  !        WHHLB_GEA
  !        GET_ORGEDIT_EDITION

  !  04/25/2014 - THERE ARE COMMON SUBROUTINE NAMES IN THE SOURCE CODE
  !               USED TO BUILD THE ORGANON DLLS. IN ORDER TO LINK THE
  !               ORGANON SOURCE CODE WITH THE FVS SOURCE CODE WE CHANGED
  !               THE DUPLICATED SUBROUTINE NAMES TO MAKE THEM UNIQUE.

  !  CHANGED THE NAME OF SUBROUTINE B_HG TO B_HG_EDIT
  !  CHANGED THE NAME OF SUBROUTINE CKAGE TO CKAGE_EDIT
  !  CHANGED THE NAME OF SUBROUTINE F_HG TO F_HG_EDIT
  !  CHANGED THE NAME OF SUBROUTINE HS_HG TO HS_HG_EDIT
  !  CHANGED THE NAME OF SUBROUTINE CALL SITECV_F TO SITECV_F_EDIT
  !  CHANGED THE NAME OF SUBROUTINE CALL SPGROUP TO SPGROUP_EDIT

subroutine PREPARE(VERSION,NPTS,NTREES,STAGE,BHAGE,SPECIES,&
     &USER,IEVEN,DBH,HT,CR,EXPAN,RADGRO,RVARS,&
     &SERROR,TERROR,SWARNING,TWARNING,IERROR,IRAD,&
     &GROWTH,ACALIB)
  !      SUBROUTINE PREPARE(VERSION,NPTS,NTREES,STAGE,BHAGE,SPECIES,
  !     1                   USER,IEVEN,DBH,HT,CR,EXPAN,RADGRO,SITE_1,
  !     2                   SITE_2,MSDI_1,MSDI_2,MSDI_3,PDEN,SERROR,TERROR,
  !     3                   SWARNING,TWARNING,IERROR,IRAD,GROWTH,ACALIB)
  implicit none
  ! Variables
  integer*4 :: VERSION,NPTS,NTREES,STAGE,BHAGE,SPECIES(2000),USER(2000),SERROR(13),TERROR(2000,6),&
       &SWARNING(8),TWARNING(2000)
  integer*4 :: IB,NSPN,TDATAI(2000,3),SPGRP(2000),RADIN(18),ENTDBH(18),ENTHT(18),ENTCR(18),ENT
  integer*4 :: IEVEN,IERROR,IRAD
  real*4  :: DBH(2000),HT(2000),CR(2000),EXPAN(2000),RADGRO(2000),RVARS(30)
  real*4  :: SITE_1,SITE_2,MSDI_1,MSDI_2,MSDI_3,PDEN
  real*4  :: TDATAR(2000,4),SBA,BALL(51),BAL(500),GROWTH(2000),PDG(2000),PTRHT(2000),PCR(2000),&
       &CCFLL(51),CCFL(500),STDATAR(2000,4),OG,ACALIB(3,18),HT40,D40,PDF,PTF,PPP,PWH,PRA
  logical*2 :: EVEN,ERROR,MISSHT,MISSCR,RAD
  integer*4 :: I,IDXAGE,IDXCYC
  !      INTEGER*4 IANS,IYN

  EVEN = .false.
  if (IEVEN .eq.1) then
     EVEN = .true.
  end if
  SITE_1 = RVARS(1)
  SITE_2 = RVARS(2)
  MSDI_1 = RVARS(3)
  MSDI_2 = RVARS(4)
  MSDI_3 = RVARS(5)
  PDEN = RVARS(9)
  call EDIT(VERSION,NPTS,NTREES,STAGE,BHAGE,SPECIES,EVEN,DBH,&
       &HT,CR,EXPAN,RADGRO,SITE_1,SITE_2,MSDI_1,MSDI_2,MSDI_3,&
       &PDEN,IB,NSPN,SPGRP,RADIN,SERROR,TERROR,SWARNING,&
       &TWARNING,ERROR,MISSHT,MISSCR,RAD)
  IERROR = 0
  if (ERROR) then
     IERROR = 1
     return
  end if
  if (VERSION .eq.1) then
     IDXAGE = 500
     IDXCYC = 100
  else if (VERSION .eq.2.or. VERSION .eq.3) then
     IDXAGE = 120
     IDXCYC = 24
  else
     IDXAGE = 30
     IDXCYC = 30
  end if
  do I = 1,2000
     TDATAI(I,1) = 0
     TDATAI(I,2) = 0
     TDATAI(I,3) = 0
     TDATAR(I,1) = 0.0
     TDATAR(I,2) = 0.0
     TDATAR(I,3) = 0.0
     TDATAR(I,4) = 0.0
     STDATAR(I,1) = 0.0
     STDATAR(I,2) = 0.0
     STDATAR(I,3) = 0.0
     STDATAR(I,4) = 0.0
     PCR(I) = 0.0
     PTRHT(I) = 0.0
     GROWTH(I) = 0.0
  end do
  do I = 1,NTREES
     TDATAI(I,1) = SPECIES(I)
     TDATAI(I,2) = SPGRP(I)
     TDATAI(I,3) = USER(I)
     TDATAR(I,1) = DBH(I)
     TDATAR(I,2) = HT(I)
     TDATAR(I,3) = CR(I)
     TDATAR(I,4) = EXPAN(I)
  end do
  do I = 1,18
     ACALIB(1,I) = 0.0
     ACALIB(2,I) = 0.0
     ACALIB(3,I) = 0.0
     ENTDBH(I) = 0
     ENTHT(I) = 0
     ENTCR(I) = 0
  end do
  call HDCALIB(VERSION,IB,NSPN,NTREES,NPTS,STAGE,BHAGE,TDATAI,EVEN,&
       &SITE_1,SITE_2,PDEN,TDATAR,D40,HT40,PDF,PTF,PPP,PWH,PRA,ENTHT,&
       &ENTDBH,PTRHT,ACALIB)
  if (MISSHT) then
     call PRDHT(VERSION,NTREES,TDATAI,ENT,TDATAR,D40,HT40,PDF,PTF,&
          &PPP,PWH,PRA,ACALIB,PTRHT)
  end if
  call CRCALIB(VERSION,IB,NSPN,NTREES,NPTS,TDATAI,ENTDBH,&
       &SITE_1,SITE_2,TDATAR,STDATAR,BAL,BALL,CCFL,CCFLL,SBA,OG,&
       &ENTCR,PCR,ACALIB)
  if (MISSCR) then
     call PRDCR(VERSION,IB,NSPN,NTREES,NPTS,TDATAI,SITE_1,&
          &SITE_2,TDATAR,CCFL,CCFLL,SBA,OG,ENT,PCR,ACALIB)
  end if
  if (RAD) then
     call DGCALIB(VERSION,IB,NSPN,NPTS,NTREES,RADIN,TDATAI,&
          &RAD,BAL,BALL,OG,TDATAR,GROWTH,SITE_1,SITE_2,&
          &RADGRO,SBA,STDATAR,PDG,ACALIB)
  else
     do I = 1,18
        ACALIB(3,I) = 1.0
     end do
  end if
  IRAD = 0
  if (RAD) then
     IRAD = 1
  end if
  do I = 1,NTREES
     SPECIES(I) = TDATAI(I,1)
     USER(I) = TDATAI(I,3)
     DBH(I) = TDATAR(I,1)
     HT(I) = TDATAR(I,2)
     CR(I) = TDATAR(I,3)
     EXPAN(I) = TDATAR(I,4)
  end do
  return
end

subroutine EDIT(VERSION,NPTS,NTREES,STAGE,BHAGE,SPECIES,&
     &EVEN,DBH,HT,CR,EXPAN,RADGRO,SI_1,SI_2,MSDI_1,MSDI_2,&
     &MSDI_3,PDEN,IB,NSPN,SPGRP,RADIN,SERROR,TERROR,SWARNING,&
     &TWARNING,ERROR,MISSHT,MISSCR,RAD)

  implicit none
  ! Variables
  integer*4 :: VERSION,NPTS,NTREES,STAGE,BHAGE,SPECIES(2000),IB,NSPN,IIB
  integer*4 :: I,J,SPGRP(2000),RADIN(18),BIG6,BNXT,SERROR(13),TERROR(2000,6),SWARNING(8),TWARNING(2000)
  real*4  :: DBH(2000),HT(2000),CR(2000),EXPAN(2000),RADGRO(2000),SI_1,SI_2,MSDI_1,MSDI_2,MSDI_3,&
       &MAXDF,MAXGF,MAXWH,MAXPP,MAXIC,MAXRA,MAXHT,ICSI,PDEN,PRA
  real*4  :: BA,SBA,B6SBA,HWSBA,B0,B1,PHT,OLD,Z50,X
  logical*2 :: EVEN,ERROR,MISSHT,MISSCR,RAD
  ERROR = .false.
  RAD = .false.
  MISSHT = .false.
  MISSCR = .false.
  BIG6 = 0
  BNXT = 0
  MAXGF = 0.0
  MAXDF = 0.0
  MAXWH = 0.0
  MAXPP = 0.0
  MAXIC = 0.0
  MAXRA = 0.0
  do I = 1,18
     RADIN(I) = 0
  end do
  do I = 1,2000
     do J = 1,6
        TERROR(I,J) = 0
     end do
     TWARNING(I) = 0
  end do
  do I = 1,13
     SERROR(I) = 0
  end do
  do I = 1,8
     SWARNING(I) = 0
  end do
  if (NTREES .lt.1.or. NTREES .gt.2000) then
     SERROR(1) = 1
  end if
  if (VERSION .lt.1.or. VERSION .gt.4) then
     SERROR(2) = 1
  end if
  if (NPTS .le.0) then
     SERROR(3) = 1
  end if
  if (SI_1.le.0.0.and. SI_2.le.0.0) then
     SERROR(4) = 1
  end if
  if (EVEN .and. BHAGE .le.0) then
     SERROR(6) = 1
  end if
  if (.not. EVEN .and. BHAGE .gt.0) then
     SERROR(7) = 1
  end if
  if (EVEN .and. (STAGE-BHAGE) .le.1) then
     SERROR(8) = 1
  end if
  if (MSDI_1.gt.1000.0.or. MSDI_2.gt.1000.0.or. MSDI_3.gt.&
       &1000.0) then
     SERROR(9) = 1
  end if
  if (VERSION .ge.4.and. SI_1.le.0.0) then
     SERROR(10) = 1
  end if
  if (VERSION .ge.4.and. PDEN .le.0.0) then
     SERROR(11) = 1
  end if
  if (.not. EVEN .and. VERSION .ge.4) then
     SERROR(12) = 1
  end if
  do I = 1,13
     if (SERROR(I) .eq.1) then
        ERROR = .true.
        return
     end if
  end do
  select case(VERSION)
  case(1)
     IB = 5
     NSPN = 18
  case(2,3)
     IB = 3
     NSPN = 11
  case(4)
     IB = 3
     NSPN = 7
  end select

  !     EDIT TREE RECORDS FOR ERRORS

  do I = 1,NTREES
     call CKSP_RUN(VERSION,I,SPECIES,TERROR)
     if (DBH(I) .le.0.09) then
        TERROR(I,2) = 1
     end if
     if (HT(I) .le.0.0) then
        MISSHT = .true.
     else if (HT(I) .le.4.5) then
        TERROR(I,3) = 1
     end if
     if (CR(I) .le.0.0) then
        MISSCR = .true.
     else if (CR(I) .gt.1.0) then
        TERROR(I,4) = 1
     end if
     if (EXPAN(I) .lt.0.0) then
        TERROR(I,5) = 1
     end if
  end do
  do I = 1,NTREES
     do J = 1,6
        if (TERROR(I,J) .eq.1) then
           ERROR = .true.
           return
        end if
     end do
  end do
  do I = 1,NTREES
     select case(VERSION)
     case(1) !  SWO  BIG SIX
        if (SPECIES(I) .eq.122.and. HT(I) .gt. MAXPP) then
           MAXPP = HT(I)
        else if (SPECIES(I) .eq.81.and. HT(I) .gt. MAXIC) then
           MAXIC = HT(I)
        else if (SPECIES(I) .eq.202.and. HT(I) .gt. MAXDF) then
           MAXDF = HT(I)
        else if (SPECIES(I) .eq.15.and. HT(I) .gt. MAXDF) then
           MAXDF = HT(I)
        else if (SPECIES(I) .eq.17.and. HT(I) .gt. MAXDF) then
           MAXDF = HT(I)
        else if (SPECIES(I) .eq.117.and. HT(I) .gt. MAXDF) then
           MAXDF = HT(I)
        end if
     case(2,3)
        if (SPECIES(I) .eq.17.and. HT(I) .gt. MAXGF) then
           MAXGF = HT(I)
        else if (SPECIES(I).eq.202.and. HT(I) .gt. MAXDF) then
           MAXDF = HT(I)
        else if (SPECIES(I) .eq.263.and. HT(I) .gt. MAXWH) then
           MAXWH = HT(I)
        end if
     case(4)
        if (SPECIES(I) .eq.351.and. HT(I) .gt. MAXRA) then
           MAXRA = HT(I)
        end if
     end select
     call SPGROUP_RUN(VERSION,I,SPECIES,SPGRP)
     IIB = IB
     if (VERSION .ge.4) IIB = 1
     if (SPGRP(I).le.IIB) then
        BIG6 = BIG6+1
        if (EXPAN(I) .le.0.0) then
           BNXT = BNXT+1
        end if
     end if
  end do

  !     DETERMINE IF SPECIES MIX CORRECT FOR STAND AGE

  SBA = 0.0
  B6SBA = 0.0
  HWSBA = 0.0
  do I = 1,NTREES
     if (EXPAN(I) .le.0.0) cycle
     BA = DBH(I)**2*EXPAN(I)
     SBA = SBA+BA
     if (SPGRP(I) .le. IIB) B6SBA = B6SBA+BA
     if (VERSION .eq.1) then
        if (SPECIES(I) .eq.361.or. SPECIES(I) .eq.431.or.&
             &SPECIES(I) .eq.818) then
           HWSBA = HWSBA+BA
        end if
     end if
  end do
  SBA = SBA*.005454154/FLOAT(NPTS)
  B6SBA = B6SBA*.005454154/FLOAT(NPTS)
  if (B6SBA .le.0) then
     SERROR(5) = 1
     ERROR = .true.
     return
  end if
  if (VERSION .ge.4) then
     if (SBA .gt.0.0) then
        PRA = B6SBA/SBA
     else
        PRA = 0.0
     end if
     if (PRA .lt.0.9) then
        SERROR(13) = 1
        ERROR = .true.
        return
     end if
  end if
  if (VERSION .eq.1) then
     if (SI_1.le.0.0.and. SI_2.gt.0.0) then
        SI_1 = 1.062934*SI_2
     else if (SI_2.le.0.0) then
        SI_2 = 0.940792*SI_1
     end if
  else if (VERSION .eq.2.or. VERSION .eq.3) then
     !  Site index conversion equation from Nigh (1995, Forest Science 41:84-98)
     if (SI_1.le.0.0.and. SI_2.gt.0.0) then
        SI_1 = 0.480 +( 1.110 * SI_2)
     else if (SI_2.le.0.0) then
        SI_2 = -0.432 +( 0.899 * SI_1)
     end if
  else
     if (SI_2.le.0.0) then
        SI_2 = 4.776377*SI_1**0.763530587
     end if
  end if

  !     DETERMINE WARNINGS (IF ANY)

  select case (VERSION)
  case(1)
     if ((SI_1.gt.0.0).and.(SI_1.lt.40.0.or. SI_1.gt.150.0)) then
        SWARNING(1) = 1
     end if
     if ((SI_2.gt.0.0).and.(SI_2.lt.50.0.or. SI_2.gt.140.0)) then
        SWARNING(2) = 1
     end if
  case(2,3)
     if ((SI_1.gt.0.0).and.(SI_1.lt.90.0.or. SI_1.gt.142.0)) then
        SWARNING(1) = 1
     end if
     if ((SI_2.gt.0.0).and.(SI_2.lt.90.0.or. SI_2.gt.142.0)) then
        SWARNING(2) = 1
     end if
  case(4)
     if (SI_1.lt.20.0.or. SI_1.gt.90.0) then
        SWARNING(1) = 1
     end if
     if ((SI_2.gt.0.0).and.(SI_2.lt.70.0.or. SI_2.gt.160.0)) then
        SWARNING(2) = 1
     end if
  end select
  select case(VERSION)
  case(1)
     if (MAXPP .gt.0.0) then
        MAXHT = (SI_2-4.5)*(1.0/(1.-EXP(-0.164985*(SI_2-4.5)&
             &**0.288169)))+4.5
        if (MAXPP .gt. MAXHT) SWARNING(3) = 1
     end if
     if (MAXIC .gt.0.0) then
        ICSI = (0.66*SI_1)-4.5
        MAXHT = ICSI*(1.0/(1.-EXP(-0.174929*ICSI**0.281176)))+4.5
        if (MAXIC .gt. MAXHT) SWARNING(3) = 1
     end if
     if (MAXDF .gt.0.0) then
        MAXHT = (SI_1-4.5)*(1.0/(1.0 -&
             &EXP(-0.174929*(SI_1-4.5)**0.281176)))+4.5
        if (MAXDF .gt. MAXHT) SWARNING(3) = 1
     end if
  case(2,3)
     if (MAXDF .gt.0.0) then
        Z50 = 2500.0/(SI_1-4.5)
        MAXHT = 4.5+1.0/(-0.000733819+0.000197693*Z50)
        if (MAXDF .gt. MAXHT) SWARNING(3) = 1
     end if
     if (MAXGF .gt.0.0) then
        Z50 = 2500.0/(SI_1-4.5)
        MAXHT = 4.5+1.0/(-0.000733819+0.000197693*Z50)
        if (MAXGF .gt. MAXHT) SWARNING(3) = 1
     end if
     if (MAXWH .gt.0.0) then
        Z50 = 2500.0/(SI_2-4.5)
        MAXHT = 4.5+1.0/(0.00192+0.00007*Z50)
        if (MAXWH .gt. MAXHT) SWARNING(3) = 1
     end if
  case(4)
     if (MAXRA .gt.0.0) then
        call WHHLB_H40_RUN(SI_1,20.0,150.0,MAXHT)
        if (MAXRA .gt. MAXHT) SWARNING(3) = 1
     end if
  end select
  if (EVEN .and. VERSION .le.3.and. BHAGE .lt.10) then
     SWARNING(4) = 1
  end if
  if ((VERSION .eq.1.and. (B6SBA + HWSBA) .lt. SBA*.2).or.&
       &(VERSION .eq.2.and. (B6SBA + HWSBA) .lt. SBA*.5).or.&
       &(VERSION .eq.3.and. (B6SBA + HWSBA) .lt. SBA*.5).or.&
       &(VERSION .eq.4.and. (B6SBA + HWSBA) .lt. SBA*.8)) then
     SWARNING(5) = 1
  end if
  if (NTREES .lt.50) then
     SWARNING(6) = 1
  end if
  call CKAGE_EDIT(VERSION,NTREES,IB,SPGRP,SI_1,SI_2,PDEN,HT,OLD)
  X = 100.0*(OLD/(BIG6-BNXT))
  if (X .ge.50.0) SWARNING(7) = 1
  if (VERSION .eq.1) then
     if (EVEN .and. BHAGE .gt.500.) SWARNING(8) = 1
  else if (VERSION .eq.2.or. VERSION .eq.3) then
     if (EVEN .and. BHAGE .gt.120.) SWARNING(8) = 1
  else
     if (EVEN .and. STAGE .gt.30.) SWARNING(8) = 1
  end if
  B1 = -0.04484724
  do I = 1,NTREES
     select case(SPECIES(I))
     case(202)
        B0 = 19.04942539
     case(263)
        if (VERSION .eq.2.or. VERSION .eq.3)B0 = 19.04942539
        if (VERSION .eq.4)B0 = 19.04942539
     case(17,15)
        B0 = 16.26279948
     case(122)
        B0 = 17.11482201
     case(117)
        B0 = 14.29011403
     case default
        B0 = 15.80319194
     end select
     if (HT(I) .gt.4.5) then
        PHT = 4.5+B0*DBH(I)/(1.0-B1*DBH(I))
        if (HT(I) .gt. PHT) then
           TWARNING(I) = 1
        end if
     end if
     if (RADGRO(I) .gt.0.0) then
        RAD = .true.
        RADIN(SPGRP(I)) = RADIN(SPGRP(I))+1
     end if
  end do
  return
end



subroutine CKAGE_EDIT(VERSION,NTREES,IB,SPGRP,SI_1,SI_2,&
     &PDEN,HT,OLD)
  implicit none
  ! Variables
  integer*4 :: VERSION,NTREES,IB,SPGRP(2000),K,ISISP
  real*4  :: SITE,SI_1,SI_2,HT(2000),OLD,GEAGE,PHTGRO,IDXAGE,SI_UC,PDEN
  OLD = 0.0
  do K = 1,NTREES
     if (HT(K) .le.4.5) cycle
     if (SPGRP(K).gt.IB) cycle
     select case(VERSION)
     case(1)

        !        GROWTH EFFECTIVE AGE FROM HANN AND SCRIVANI'S (1987) DOMINANT
        !        HEIGHT GROWTH EQUATION

        if (SPGRP(K) .eq.3) then
           SITE = SI_2-4.5
           ISISP = 2
        else
           SITE = SI_1-4.5
           !               IF(SPGRP(K) .EQ. 5) SITE=SI_1*0.66-4.5
           if (SPGRP(K) .eq.5) SITE = SI_1*0.66-4.5
           ISISP = 1
        end if
        call HS_HG(ISISP,SITE,HT(K),GEAGE,PHTGRO)
        IDXAGE = 500.0
     case(2)
        if (SPGRP(K) .eq.3) then

           !           GROWTH EFFECTIVE AGE FROM FLEWELLING'S WESTERN HEMLOCK
           !           DOMINANT HEIGHT GROWTH EQUATION

           SITE = SI_2
           call F_HG_EDIT(SITE,HT(K),GEAGE,PHTGRO)
        else

           !        GROWTH EFFECTIVE AGE FROM BRUCE'S (1981) DOMINANT HEIGHT
           !        GROWTH EQUATION FOR DOUGLAS-FIRAND GRAND FIR

           SITE = SI_1
           call B_HG_EDIT(SITE,HT(K),GEAGE,PHTGRO)
        end if
        IDXAGE = 120.0
     case(3)
        if (SPGRP(K) .eq.3) then

           !           GROWTH EFFECTIVE AGE FROM FLEWELLING'S WESTERN HEMLOCK
           !           DOMINANT HEIGHT GROWTH EQUATION

           SITE = SI_2
           call F_HG_EDIT(SITE,HT(K),GEAGE,PHTGRO)
        else

           !           GROWTH EFFECTIVE AGE FROM BRUCE'S (1981) DOMINANT HEIGHT
           !           GROWTH EQUATION FOR DOUGLAS-FIR AND GRAND FIR

           SITE = SI_1
           call B_HG_EDIT(SITE,HT(K),GEAGE,PHTGRO)
        end if
        IDXAGE = 120.0
     case(4)
        if (SPGRP(K) .eq.1) then

           !           GROWTH EFFECTIVE AGE FROM WEISKITTEL ET AL. (2009) RED ALDER
           !           DOMINANT HEIGHT GROWTH EQUATION

           SITE = SI_1
           call WHHLB_SI_UC_EDIT(SITE,PDEN,SI_UC)
           call WHHLB_GEA_RUN(HT(K),SI_UC,GEAGE)
        end if
        IDXAGE = 30.0
     end select
     if (GEAGE.gt.IDXAGE) then
        OLD = OLD+1.0
     end if
  end do
  return
end


subroutine B_HG_EDIT(SI,HT,GEAGE,PHTGRO)
  implicit none
  ! Variables
  real*4  :: SI,HT,X1,X2,B1,B2,GEAGE,PHT,XX1,PHTGRO
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
  PHT = SI*EXP(B1*((GEAGE+5.0+X1)**B2-X2**B2))
  PHTGRO = PHT-HT
  return
end

subroutine F_HG_EDIT(SI,HT,GEAGE,PHTGRO)
  implicit none
  !     For Western Hemlock compute Growth Effective Age and 5-year potential
  !     height growth using the western hemlock top height curves of
  !     Flewelling.  These subroutines are required:
  !       SITECV_F   computes top height from site and age
  !       SITEF_C    computes model parameters
  !       SITEF_SI   calculates an approximate psi for a given site
  !       Note: Flewelling's curves are metric.
  !             Site Index is not adjusted for stump height.

  ! Variables
  real*4  :: SI,GEAGE,HT,PHTGRO,SIM,HTM,AGE,HTOP,PHT,XHTOP1,XHTOP2
  integer*4 :: I
  SIM = SI * 0.3048
  HTM = HT * 0.3048

  !     Compute growth effective age

  AGE = 1.0
  do I = 1,4
     5 AGE = AGE + 100./10.**I
     if (AGE .gt.500.0) then
        GEAGE = 500.0
        call SITECV_F_EDIT(SIM,GEAGE,XHTOP1)
        call SITECV_F_EDIT(SIM,GEAGE+5,XHTOP2)
        PHTGRO = 3.2808*(XHTOP2-XHTOP1)
        return
     end if
     call SITECV_F_EDIT(SIM,AGE,HTOP)
     if (HTOP .lt. HTM) go to 5
     AGE = AGE - 100./10.**I
  end do
  GEAGE = AGE

  !     Compute top height and potential height growth

  call SITECV_F_EDIT(SIM,GEAGE+5,HTOP)
  PHT = HTOP*3.2808
  PHTGRO = PHT - HT
  return
end


subroutine GET_ORGEDIT_EDITION(EDITION)
  implicit none
  ! Variables
  real*4  :: EDITION
  EDITION = 9.1
  return
end
