!========================!
!   organon : woodqual   !
!========================!
! ORGANON $Id$

subroutine WOODQUAL(IJCALC,IEVEN,IFINAL,ACTION,BHAGE,STAGE,NINGRO,&
     &NPTS,NTREES,NWQT,VERSION,SPECIES,SITE_1,&
     &SITE_2,PDEN,DBH,HT,CR,SCRX,EXPAN,MGEXP,DGRO,&
     &HGRO,CRCHNG,SCRCHNG,BRCNT,BRDIA,BRHT,JCORE,&
     &IDIB)
  !  04/25/2014 - THERE ARE COMMON SUBROUTINE NAMES IN THE SOURCE CODE
  !               USED TO BUILD THE ORGANON DLLS. IN ORDER TO LINK THE
  !               ORGANON SOURCE CODE WITH THE FVS SOURCE CODE WE CHANGED
  !               THE DUPLICATED SUBROUTINE NAMES TO MAKE THEM UNIQUE.

  !  CHANGED THE NAME OF SUBROUTINE/SUBROUTINE CALL - SPGROUP TO SPGROUP_WOOD
  !  CHANGED THE NAME OF SUBROUTINE - TAPER_RA TO TAPER_RA_WOOD

  implicit none
  ! Variables
  integer*4 :: IJCALC,IEVEN,IFINAL,ACTION,BHAGE,STAGE,NINGRO,NPTS,NTREES,NWQT,VERSION,SPECIES(2000)
  real*4  :: SITE_1,SITE_2,PDEN,DBH(2000),HT(2000),CR(2000),SCRX(2000),EXPAN(2000),MGEXP(2000),&
       &DGRO(2000),HGRO(2000),CRCHNG(2000),SCRCHNG(2000)
  integer*4 :: TDATAI(2000,3),SPGRP(2000),IB2,NEXT,WQINIT,BRCNT(2000,3),BRDIA(2000,40),BRHT(2000,40),&
       &JCORE(2000,40),IDIB(2000,40)
  real*4  :: TDATAR(2000,8),GROWTH(2000,4),SCR(2000,3),SI_1,SI_2
  logical*2 :: JCALC,EVEN,FINAL
  integer*4 :: I,J
  JCALC = .false.
  EVEN = .false.
  FINAL = .false.
  if (IJCALC .eq.1) then
     JCALC = .true.
  end if
  if (IEVEN .eq.1) then
     EVEN = .true.
  end if
  if (IFINAL .eq.1) then
     FINAL = .true.
  end if
  select case(VERSION)
  case(1)
     IB2 = 5
  case(2,3)
     IB2 = 2
  case(4)
     IB2 = 1
  end select
  SI_1 = SITE_1
  SI_2 = SITE_2
  do I = 1,2000
     GROWTH(I,3) = 0.0
     GROWTH(I,4) = 0.0
     TDATAI(I,1) = 0
     TDATAI(I,2) = 0
     TDATAI(I,3) = 0
     TDATAR(I,1) = 0.0
     TDATAR(I,2) = 0.0
     TDATAR(I,3) = 0.0
     TDATAR(I,4) = 0.0
     TDATAR(I,5) = 0.0
     TDATAR(I,6) = 0.0
     TDATAR(I,7) = 0.0
     TDATAR(I,8) = 0.0
     SCR(I,1) = 0.0
     SCR(I,2) = 0.0
     SCR(I,3) = 0.0
  end do
  do I = 1,NTREES
     GROWTH(I,1) = HGRO(I)
     GROWTH(I,2) = DGRO(I)
     TDATAI(I,1) = SPECIES(I)
     call SPGROUP_WOOD(VERSION,I,SPECIES,SPGRP)
     TDATAI(I,2) = SPGRP(I)
     TDATAR(I,1) = DBH(I)
     TDATAR(I,2) = HT(I)
     TDATAR(I,3) = CR(I)
     TDATAR(I,4) = EXPAN(I)
     SCR(I,1) = SCRX(I)
  end do
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
     if (SI_2.le.0.0) SI_2 = 125.0
  end if

  !     CHANGE TO EXPANSION FACTOR FOR STAND--NOT SAMPLE

  do J = 1,NTREES
     TDATAR(J,4) = TDATAR(J,4)/FLOAT(NPTS)
     MGEXP(J) = MGEXP(J)/FLOAT(NPTS)
     TDATAR(J,5) = TDATAR(J,4)
     if (SCR(J,1) .gt.0.0) then
        TDATAR(J,6) = SCR(J,1)
        SCR(J,2) = SCR(J,1)
     else
        TDATAR(J,6) = TDATAR(J,3)
     end if
     SCR(J,3) = SCR(J,1)-SCRCHNG(J)
     TDATAR(J,7) = TDATAR(J,3)-CRCHNG(J)
     if (FINAL) then
        TDATAR(J,8) = MGEXP(J)
     else
        TDATAR(J,8) = TDATAR(J,4)
     end if
  end do
  SI_1 = SI_1-4.5
  SI_2 = SI_2-4.5
  select case(ACTION)
  case(1)
     WQINIT = -1
     call WQ_FILL(JCALC,EVEN,BHAGE,STAGE,IB2,NTREES,NWQT,TDATAI,&
          &VERSION,WQINIT,SI_1,SI_2,PDEN,TDATAR,SCR,BRCNT,&
          &BRDIA,BRHT,JCORE)
  case(2)
     WQINIT = NINGRO
     call WQ_FILL(JCALC,EVEN,BHAGE,STAGE,IB2,NTREES,NWQT,TDATAI,&
          &VERSION,WQINIT,SI_1,SI_2,PDEN,TDATAR,SCR,BRCNT,&
          &BRDIA,BRHT,JCORE)
  case(3)
     call WQ_CALC(JCALC,EVEN,BHAGE,STAGE,IB2,NTREES,VERSION,&
          &TDATAI,GROWTH,TDATAR,SCR,BRCNT,BRDIA,BRHT,&
          &JCORE)
  case(4)
     NEXT = 4
     call WQ_END(FINAL,JCALC,NEXT,IB2,NTREES,VERSION,&
          &TDATAI,MGEXP,TDATAR,SCR,BRCNT,BRDIA,BRHT,JCORE,&
          &IDIB)
  case(5)
     NEXT = 1
     call WQ_END(FINAL,JCALC,NEXT,IB2,NTREES,VERSION,&
          &TDATAI,MGEXP,TDATAR,SCR,BRCNT,BRDIA,BRHT,JCORE,&
          &IDIB)
  end select
  return
end

subroutine SPGROUP_WOOD(VERSION,I,SPECIES,SPGRP)
  !     DETERMINE SPECIES GROUP FOR EACH TREE IN TREE LIST

  !     I = TREE INDICATOR

  implicit none
  ! Variables
  integer*4 :: VERSION,I,SPECIES(2000),SPGRP(2000),ISX,J

  integer*4 :: SCODE1(19) = (/            202,15,17,122,117,81,263,242,231,361,431,631,805,312,815,&
       &818,351,492,920 /)

  integer*4 :: SCODE2(11) = (/            202,17,263,242,231,361,312,815,351,492,920 /)

  integer*4 :: SCODE3(7) = (/            351,202,263,242,312,492,920 /)
  ISX = -9999
  select case (VERSION)
  case(1)
     do J = 1, 19
        if (SPECIES(I).eq. SCODE1(J)) then
           ISX = J
           if (ISX .gt.2) ISX = ISX-1
           exit
        end if
     end do
  case(2,3)
     do J = 1, 11
        if (SPECIES(I).eq. SCODE2(J)) then
           ISX = J
           exit
        end if
     end do
  case(4)
     do J = 1, 11
        if (SPECIES(I).eq. SCODE3(J)) then
           ISX = J
           exit
        end if
     end do
  end select
  SPGRP(I) = ISX
  return
end

subroutine WHHLB_SI_UC_WOOD(SI_C,PDEN,SI_UC)

  !     UNCORRECTS THE DENSITY INPACT UPON THE WEISKITTEL, HANN, HIBBS, LAM, AND BLUHN
  !          SITE INDEX FOR RED ALDER

  implicit none
  ! Variables
  real*4  :: SI_C,PDEN,SI_UC

  !     SITE INDEX UNCORRECTED FOR DENSITY EFFECT

  SI_UC = SI_C*(1.0-0.326480904*EXP(-0.000400268678*PDEN**1.5))
  return
end

subroutine TAPER_RA_WOOD(DBH,HT,CR,HI,DI)
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
