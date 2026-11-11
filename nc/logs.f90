!===============================!
!   Compute board foot volume   !
!===============================!
! NC $Id$
subroutine LOGS&
     &(DBH,HT,ICCD,IBCD,CUMIN,BDMIN,ISP,CSTMP,BV,JOSTND)
  implicit none

  !  TAPER EQUATIONS BY G.S.BIGING
  !  BY K.STUMPF, ADAPTED BY B.KRUMLAND, P.J.DAUGHERTY

  ! Variables
  real    :: XPS(9),TPCF(2,9)
  integer :: IPRGSP(12),IMAP(12)
  real    :: SF(4,2),TBV,QTLN
  integer :: JOSTND,ISP,ICCD,I,JSP,ILN,IMAX,NX,IH,J
  integer :: IBCD,M,IC,NLOGS,ITEM,IDU,N,LNGTH
  real    :: BV,CSTMP,BDMIN,CUMIN,HT,DBH,X,D,B1,B2,HI,DIBAHI,TRIM
  real    :: SLN,CTP,BTP,DTOP,Z1,HTM,DL,TV,DU,FRAC,QLOGS,S1,AH
  data TPCF/&
       &1.01959  ,.33567 ,1.06932  ,.41563 ,1.07134  ,.47216&
       &,1.02929  ,.33401 ,1.09262  ,.36530 ,1.07588  ,.35378&
       &,1.01959  ,.33567 ,1.06932  ,.41563 ,1.01959  ,.33567/
  data IPRGSP/10,2,6,3,4,9,7,1,8,11,5,12/
  data IMAP  / 1,2,3,4,5,6,7,9,9,10,11,4/

  !  SCRIBNER VOL. FACTORS FOR SMALL LOGS

  data SF/1.249,1.608,1.854,2.410,1.160,1.400,1.501,2.084/

  !  STATEMENT FUNCTION TO CALC. DIB AT H(I)

  DIBAHI(HI,HT,B1,B2,D,X) = D*(B1+B2*ALOG(1.-X*(HI/HT)**(1./3.)))

  !   FIND VALID SPECIES CODE AND BRANCH TO APPROPRIATE STATEMENT TO
  !   COMPUTE BOARD FOOT VOLUME. SPECIES 1 AND 8 USE THE SAME VOLUME
  !   COEFFICIENTS.

  do I = 1,12
     if (ISP .eq. IPRGSP(I)) go to 9
     7 continue
  end do
  write(JOSTND,907) ISP
  907 format(' IN SIERRA VOLS NO MATCH FOR SPECIES',I4,&
       &'.  SPECIES 10 USED.')
  I = 10
  9 continue
  JSP = IMAP(I)

  !   SPECIES 5, 7, 8 AND 11 ARE CALCULATED DIFFERENTLY FROM THE
  !   OTHER SPECIES.

  if (ISP .eq.5) go to 15
  if (ISP .eq.7) go to 16
  if (ISP .eq.8.or. ISP .eq.11) go to 17
  go to 18

  !   MADRONNE USE PNW 414. VOL IS REALLY SAW LOG CUBIC.

  15 continue
  BV = .0006182*DBH**1.726*HT**1.265
  BV = BV*5.0
  go to 100

  ! BLACK OAK EQ FROM PNW 414
  ! EQN IS IN CUBIC FEET, APPLY BF/CF RATIO   R.JOHNSON

  16 continue
  BV = .00124 * DBH**2.68 * HT**0.424
  BV = BV * 5.0
  go to 100

  !  TANOAK AND OTHER HARDWOODS

  17 continue
  BV = .000252*DBH**2.309*HT**1.211
  BV = BV*5.0
  go to 100

  !   CALCULATE VOLUME HERE FOR ALL OTHER SPECIES.
  !   DEFINE NOMINAL SCALING STANDARDS

  18 continue
  ILN = 16
  BV = 0.0
  TRIM = .3
  SLN = ILN+TRIM
  IMAX = 0
  do M = 1,9
     XPS(M) = 1. - EXP(-TPCF(1,M)/TPCF(2,M))
     5 continue
  end do
  CTP = ICCD
  BTP = IBCD

  !  CUBIC VOLUME

  NX = 1
  if (DBH.le.(ICCD))goto 50
  if (DBH.lt.CUMIN)goto 50
  DTOP = CTP

  !  FIND MERCH TOP

  20 Z1 = EXP((DTOP/DBH - TPCF(1,JSP))/TPCF(2,JSP)) - 1.
  HTM = (Z1/(-XPS(JSP)))**3*HT
  if (HTM.le.(.5*SLN))goto 100
  if (NX.eq.2)goto 65
  IH = INT(HTM+.49999999)
  DL = DIBAHI(CSTMP,HT,TPCF(1,JSP),TPCF(2,JSP),DBH,XPS(JSP))
  TV = 0.0
  J = 0

  !   SCALE THE TREE

  do I = 2,IH
     HI = FLOAT(I)
     DU = DIBAHI(HI,HT,TPCF(1,JSP),TPCF(2,JSP),DBH,XPS(JSP))
     TV = TV+.005454*((DU+DL)/2.)**2
     DL = DU
     LNGTH = MOD(I,ILN)
     if (I.eq.IH)goto 24
     if (LNGTH.ne.0)goto 30
     24 continue
     if (LNGTH.eq.0)LNGTH = ILN
     FRAC = FLOAT(LNGTH)/FLOAT(ILN)
     IC = INT((DU-3.)/2.)
     IC = IC+1
     if (IC.lt.1)IC = 1
     if (IC.gt.25)IC = 25
     if (IC.gt.IMAX)IMAX = IC
     TV = 0.0
     30 continue
  end do

  !   BOARD VOLUMES

  50 if (DBH.le.(IBCD))goto 100
  if (DBH.lt.BDMIN)goto 100
  DTOP = BTP
  NX = 2
  goto 20
  65 QLOGS = (HTM-1.)/SLN
  NLOGS = INT(QLOGS)
  NLOGS = NLOGS-1
  if (NLOGS.le.0)goto 80
  S1 = SLN
  AH = 0.0
  BV = 0.0
  ITEM = 1
  68 do J = 1,NLOGS
     HI = FLOAT(J)*S1+AH
     DU = DIBAHI(HI,HT,TPCF(1,JSP),TPCF(2,JSP),DBH,XPS(JSP))
     FRAC = ((S1-TRIM)/FLOAT(ILN))
     if (DU.le.9.0) then
        IDU = INT(DU + .5)
        IDU = IDU - 5
        N = 1
        if (S1.lt.SLN)N = 2
        TBV = SF(IDU,N) * S1
     else
        TBV = ((.79*DU -2)*DU -4.)*FRAC
     end if
     TBV = INT(TBV/10. + .5) * 10.
     IC = INT((DU-3.)/2.)
     IC = IC+1
     if (IC.lt.1)IC = 1
     if (IC.gt.25)IC = 25
     if (IC.gt.IMAX)IMAX = IC
     BV = BV+TBV
     70 continue
  end do
  if (ITEM.eq.2)goto 100

  !  TOP LOGS FOR BOARD FOOT

  80 ITEM = 2
  AH = NLOGS*SLN
  if (AH.le.0)AH = 0.0
  QTLN = HTM-AH-1.
  NLOGS = 2
  if (QTLN.lt.(1.5*SLN))NLOGS = 1
  S1 = QTLN/FLOAT(NLOGS)
  goto 68
  100 continue
  return
end
