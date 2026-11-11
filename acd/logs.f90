!===============================!
!   Compute board foot volume   !
!===============================!
! ACD $Id$
subroutine LOGS(DBH,HT,IBCD,BDMIN,ISP,STMP,BV)
  implicit none

  !  REGION 5 BOARD FOOT VOLUME MODELS.
  !  TAPER EQUATIONS BY G.S.BIGING
  !  BY K.STUMPF, ADAPTED BY B.KRUMLAND, P.J.DAUGHERTY

  ! Common blocks
  include 'PRGPRM.F77'

  ! Variables
  real    :: XPS(6),TPCF1(6),TPCF2(6),SF(4,2)
  integer :: IMAP(MAXSP)
  real    :: QTLN,TBV,FRAC,DU,AH,S1,HTM,Z1,DTOP,BVL,DIBAHI,HI,B1,B2,D
  real    :: X,SLN,TRIM,DBH,HT,BDMIN,STMP,BV
  integer :: ISP,IBCD,JSP,NLOGS,N,IDU,ITEM,J
  data TPCF1/ &
       & 1.01959, 1.06932, 1.07134, 1.02929, 1.09262, 1.07588/
  data TPCF2/ &
       & 0.33567, 0.41563, 0.47216, 0.33401, 0.36530, 0.35378/
  data XPS/ &
       & 0.95204, 0.92368, 0.89659, 0.95411, 0.94976, 0.95222/
  data IMAP  / 1, 2, 4,4*5, 1, 3, 1, 1,4*6,2*1,8*3,83*1/

  !  DEFINE NOMINAL SCALING STANDARDS: SLN=STANDARD LOG LENGTH;
  !  TRIM=KERF.

  data SLN,TRIM/16.3,0.3/

  !  SCRIBNER VOL. FACTORS FOR SMALL LOGS

  data SF/1.249,1.608,1.854,2.410,1.160,1.400,1.501,2.084/

  !  STATEMENT FUNCTION TO CALC. DIB AT H(I)

  DIBAHI(HI,HT,B1,B2,D,X) = D*(B1+B2*ALOG(1.-X*(HI/HT)**(1./3.)))

  ! Initialize the volume estimate
  BV = 0.0

  !   VOLUMES FOR SPECIES 5 AND 7 ARE CALCULATED FROM FORMULA.

  if (ISP .eq. 5) then

     !       GIANT SEQUOIA FROM WENSEL AND SCHOENHEIDE

     BVL = -5.66831 + 2.21035*ALOG(DBH) + 0.91921*ALOG(HT)
     BV = EXP(BVL)
     return
  end if
  if (ISP .eq. 7) then

     !       BLACK OAK EQ FROM PNW 414
     !       EQN IS IN CUBIC FEET, APPLY BF/CF RATIO   R.JOHNSON

     BV = .00124 * DBH**2.68 * HT**0.424
     BV = BV * 5.0
     return
  end if

  !  VOLUME FOR OTHER SPECIES IS COMPUTED FROM LOG RULE.  JSP MAPS
  !  SPECIES ONTO A TAPER MODEL.

  JSP = IMAP(ISP)
  DTOP = IBCD

  !  CHECK FOR MERCHANTABILITY LIMITS.

  if (DBH.le.DTOP .or. DBH.lt.BDMIN) go to 100
  Z1 = EXP((DTOP/DBH - TPCF1(JSP))/TPCF2(JSP)) - 1.

  !  HTM IS TOTAL MERCHANTABLE LENGTH ADJUSTED FOR STUMP HEIGHT.
  !  RETURN IF LESS THAN 1/2 LOG.

  HTM = (Z1/(-XPS(JSP)))**3*HT - STMP
  if (HTM.le.(.5*SLN)) go to 100

  !  NLOGS IS THE TOTAL NUMBER OF LOGS EXCLUDING THE TOP FULL LOG AND ANY
  !  PARTIAL LOG ABOVE IT.

  NLOGS = HTM/SLN -1
  if (NLOGS.le.0) go to 80
  S1 = SLN
  AH = 0.0
  BV = 0.0
  N = 0
  IDU = 0
  ITEM = 1
  60 continue
  do J = 1,NLOGS
     HI = FLOAT(J)*S1+AH
     DU = DIBAHI(HI,HT,TPCF1(JSP),TPCF2(JSP),DBH,XPS(JSP))
     FRAC = ((S1-TRIM)/16.0)
     if (DU.le.9.0) then
        IDU = DU+.5 -5
        if (IDU.le.0) IDU = 1
        N = 1
        if (S1.lt.SLN)N = 2
        TBV = SF(IDU,N) * S1
     else
        TBV = ((.79*DU -2.0)*DU -4.)*FRAC
     end if
     TBV = INT(TBV/10. + .5) * 10.
     BV = BV+TBV
     70 continue
  end do
  if (ITEM.eq.2) go to 100

  !  RETURN TO STATEMENT 60 TO COMPUTE VOLUME FOR TOP LOGS.

  80 ITEM = 2
  AH = NLOGS*SLN
  if (AH.le.0)AH = 0.0
  QTLN = HTM-AH
  NLOGS = 2
  if (QTLN.lt.(1.5*SLN))NLOGS = 1
  S1 = QTLN/FLOAT(NLOGS)
  go to 60
  100 continue
  return
end subroutine LOGS
