!======================================!
!   Compute stand density statistics   !
!======================================!
! BASE $Id$
subroutine DENSE
  implicit none

  !   THIS SUBROUTINE CALCULATES THE AVERAGE HEIGHT OF THE STAND,
  !   THE CROWN COMPETITION FACTOR BACKDATED TO THE START OF THE
  !   PERIOD OF OBSERVED GROWTH, THE RELATIVE DENSITY BY SPECIES,
  !   MAINTAINS AN INGROWTH ACCUMULATOR, MAKES THE CORRECTION
  !   TO RELATIVE DENSITY FOR NON-STOCKABLE POINTS, AND LOADS PCT

  ! Common blocks
  include 'PRGPRM.F77'
  include 'PLOT.F77'
  include 'CALCOM.F77'
  include 'ARRAYS.F77'
  include 'CONTRL.F77'
  include 'OUTCOM.F77'
  include 'HTCAL.F77'
  include 'PDEN.F77'
  include 'VARCOM.F77'

  ! Variables
  integer :: I,IS,IP,ISPC,I1,I2,I3,II
  real    :: SSUMN,TEMP1,TEMP2,RAT,TOTAL,CCFT,CW,BATREE,DP,P,BAT,RELDT
  real    :: TSUMD2,R,BRATIO,D,G,SN,BAGR
  real    :: SUMDR0
  logical :: LREDO
  logical :: DEBUG

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'DENSE',5,ICYC)

  !  WK3 IS LOADED WITH DBH TO FACILITATE BACKDATING DURING
  !  CALIBRATION.  WHEN CALIBRATING, BA AND RELDEN ARE DENSITY
  !  ESTIMATES FOR THE START OF THE DIAMETER INCREMENT MEASUREMENT
  !  PERIOD AND RELDM1 AND OLDBA ARE DENSITY ESTIMATES FOR THE START
  !  OF THE HEIGHT INCREMENT MEASUREMENT PERIOD.

  LREDO = .false.
  if (ITRN.le.0) goto 6
  do I = 1,IREC1
     WK3(I) = DBH(I)
     1 continue
  end do

  !  IF DIAMETERS NEED TO BE BACK DATED (LBKDEN=TRUE) PROCEED WITH
  !  BACKDATING:  CALCULATE THE AVERAGE RATIO
  !  OF DIAMETER SQUARED AT START OF GROWTH PERIOD TO DIAMETER
  !  SQUARED AT THE END OF THE GROWTH PERIOD (BASAL AREA GROWTH
  !  RATIO), FOR ALL GROWTH SAMPLE TREES.

  if (DEBUG) write(JOSTND,9006) IDG,LBKDEN
  9006 format(' IN DENSE;  IDG = ',I2,';  LBKDEN = ',L1)
  if (.not.LBKDEN) go to 6
  LREDO = .true.

  !  NOW LOAD WK3 WITH DBH FOR THOSE TREES THAT DIED WITHIN
  !  THE LAST FIVE YEARS (TREE HISTORY 6,7 = IMC() OF 7)
  !  OLDER DEAD (TREE HISTORY 8,9 = IMC() OF 9) LOAD DBH OF 0.

  if (IREC2.gt.MAXTRE) go to 12
  do I = IREC2,MAXTRE
     WK3(I) = DBH(I)
     if (IMC(I) .eq. 9) WK3(I) = 0.
     11 continue
  end do
  12 continue
  BAGR = 0.0
  SN = 0.0
  do I = 1,IREC1
     G = DG(I)
     D = DBH(I)

     !  DG IS SET TO -1.0 IN **CRATET** WHEN THE INPUT VALUE IS ZERO.
     !  THESE OBSERVATIONS ARE NOT INCLUDED IN THE CALCULATION OF THE
     !  BASAL AREA GROWTH RATIO.  WHEN GROWTH IS GREATER THAN THE CURRENT
     !  DIAMETER, THE RECORD IS ALSO EXCLUDED FROM COMPUTING BAGR.

     if (G.lt.0.0) go to 3
     if (IDG.eq.1) go to 2
     IS = ISP(I)
     G = G/BRATIO(IS,D,HT(I))
     2 continue
     if (G.gt.D) go to 3
     BAGR = BAGR+(1.0-(2.0*D*G-G*G)/(D*D))
     SN = SN+1.0
     3 continue
  end do

  !  IF THERE ARE NO GROWTH SAMPLE TREES, BACKDATING CANNOT BE DONE.

  if (SN.le.0.0) go to 6

  !  COMPUTE AVERAGE BASAL AREA GROWTH RATIO, AND BACKDATE DIAMETERS,
  !  LOADING THEM INTO WK3.

  BAGR = BAGR/SN
  do I = 1,IREC1
     IS = ISP(I)
     G = DG(I)
     D = DBH(I)
     if (IDG.eq.1) go to 4
     G = G/BRATIO(IS,DBH(I),HT(I))
     if (G.gt.D)G = D
     4 continue
     R = 1.0-(2.0*D*G-G*G)/(D*D)
     if (G.lt.0.0.or.R.le.0.0) R = BAGR
     WK3(I) = SQRT(D*D*R)
     if (DEBUG) write(JOSTND,9005) I,IS,G,R,D,WK3(I)
     9005 format(' IN DENSE: I = ',I4,';  IS = ',I2,';  G = ', &
          & F6.2,';  R = ',F8.5,';  D = ',F6.2,';  WK3 = ',F6.2)
     5 continue
  end do
  6 continue

  !  INITIALIZE.  IF BACKDATING DENSITY AND THIS IS THE SECOND PASS,
  !  DO NOT WIPE OUT THE POINT DENSITY STATS.

  if (LBKDEN.and..not.LREDO) go to 15
  do IP = 1,MAXPLT
     BAAA(IP) = 0.0
     PCCF(IP) = 0.0
     PTPA(IP) = 0.0
     PTPAA(IP) = 0.0
     PRDA(IP) = 0.0
     do IS = 1,MAXSP
        OVER(IS,IP) = 0.0
        13 continue
     end do
     14 continue
  end do
  15 continue
  do IS = 1,MAXSP
     RELDSP(IS) = 0.0
     16 continue
  end do
  TSUMD2 = 0.0
  RELDT = 0.
  TPROB = 0.
  BAT = 0.
  BA = 0.

  !   CALL MBACAL TO IDENTIFY THE SITE SPECIES IE SPECIES WITH MOST
  !   BASAL AREA.

  call MBACAL

  !   BEGIN SPECIES LOOP.   ACCUMULATE STAND SUMS.  LOAD WK5.

  SUMDR0 = 0.
  DR016 = 0.
  do ISPC = 1, MAXSP
     I2 = ISCT(ISPC,2)
     if ( I2 .le. 0 ) go to  50
     I1 = ISCT(ISPC,1)

     !  CCF FOR TREES GREATER THAN 10.0 IN. DBH IS CALCULATED WITH A
     !  QUADRATIC CROWN AREA - DBH RELATIONSHIP.  FOR SMALLER TREES,
     !  AN EXPONENTIAL CROWN AREA - DBH RELATIONSHIP IS USED.  IN
     !  BOTH CASES, THERE IS A LINEAR RELATIONSHIP ON THE LOG - LOG
     !  SCALE.

     do I3 = I1,I2
        I = IND1(I3)
        P = PROB(I)
        TPROB = TPROB+P
        D = DBH(I)
        if (LBKDEN.and.LREDO)  D = WK3(I)
        if (D.ge.DBHSDI)SUMDR0 = SUMDR0+P*(D)**1.605 ! ADD ZEIDE SUMS IF DIA. >=DBHSDI
        DP = D*P
        WK5(I) = D*DP
        TSUMD2 = TSUMD2+WK5(I)
        IP = ITRE(I)
        BATREE = 0.005454154*WK5(I)
        BAT = BAT+BATREE

        !    CALL CCFCAL TO CALCULATE CROWN COMPETITION

        call CCFCAL(ISPC,D,HT(I),ICR(I),P,.false.,CCFT,CW,1)

        if (DEBUG) write(JOSTND,501) CCFT,P
        501 format(' DENSE: CCFT PROB =',2F10.2)
        RELDSP(ISPC) = RELDSP(ISPC)+CCFT

        !  IF BACKDATING DENSITY AND THIS IS THE SECOND PASS, DO NOT
        !  RECOMPUTE THE POINT DENSITY STATISTICS.

        if (DEBUG) write (JOSTND,500) LBKDEN,LREDO,D,REGNBK
        500 format (' IN DENSE: LBKDEN,LREDO,D,REGNBK = ',2L2,2E15.7)
        if (LBKDEN.and..not.LREDO) go to 10
        PCCF(IP) = PCCF(IP)+CCFT*PI/GROSPC
        PTPA(IP) = PTPA(IP)+P*PI/GROSPC

        !  REGNBK IS THE MINIMUM DBH OF AN OVERSTORY TREE.

        if (D.lt.REGNBK) go to 10
        BAAA(IP) = BAAA(IP)+BATREE*PI/GROSPC
        OVER(ISPC,IP) = OVER(ISPC,IP)+BATREE*PI/GROSPC
        PTPAA(IP) = PTPAA(IP)+P*PI/GROSPC
        if (XMAXPT(IP) .gt. 0.0) then
           PRDA(IP) = PRDA(IP)+(P*(D/10.)**1.605*PI/GROSPC)/XMAXPT(IP)
        end if
        10 continue
     end do

     !   CALCULATE RELATIVE DENSITY FOR THE STAND.

     RELDT = RELDT + RELDSP(ISPC)

     !   END OF CROWN COMPETITION FACTOR LOOP.  PRINT DEBUG INFO IF
     !      DESIRED.

     if (.not.DEBUG) go to 50
     write (JOSTND,9000) ISPC,RELDSP(ISPC)
     9000 format(' CCF FOR SPECIES',I3,'=',F10.3)
     50 continue
  end do
  if (.not.LREDO) go to 53
  LREDO = .false.
  if (IREC2.gt.MAXTRE) goto 49
  do I = IREC2,MAXTRE
     WK3(I) = 0.01
     52 continue
  end do
  49 continue
  RELDM1 = RELDT
  OLDBA = BAT

  !  WHEN BACKDATING, PCT DISTRIBUTION MUST BE COMPUTED HERE.

  call PCTILE(ITRN,IND,WK5,PCT,TOTAL)
  go to 6
  53 continue
  RELDEN = RELDT
  RMSQD = 0.
  if (ITRN.gt.0.and.TPROB.gt.0.0) then
     RMSQD = SQRT (TSUMD2 / TPROB)
     DR016 = (SUMDR0/TPROB)**(1./1.605)
  end if
  BA = BAT

  !  IF PREPARING DENSITY ESTIMATES FOR CALIBRATION, FINISH THE JOB.

  if (.not.LBKDEN) go to 54
  RAT = FINTH/FINT
  TEMP1 = (RELDEN-RELDM1)*RAT+RELDM1
  TEMP2 = (BA-OLDBA)*RAT+OLDBA
  RELDEN = RELDM1
  RELDM1 = TEMP1
  BA = OLDBA
  OLDBA = TEMP2
  if (DEBUG) write(JOSTND,9003) BA,OLDBA,RELDEN,RELDM1
  9003 format(' IN DENSE:  BA = ',F8.2,', OLDBA = ',F8.2, &
       & ', RELDEN = ',F8.2,', RELDM1 = ',F8.2/)
  54 continue

  !   IF NOT BACKDATING, COMPUTE BASAL AREA PCT DISTRIBUTION.

  if (LBKDEN) go to 55
  call PCTILE(ITRN,IND,WK5,PCT,TOTAL)
  if (LSTART) RELDM1 = RELDEN
  55 continue

  !   COMPUTE THE POINT BASAL AREA IN LARGER TREES...PTBAL USES WK5
  !   AS IT IS COMPUTED ABOVE: D*D*P, AND WK6 FOR A SCRATCH VECTOR.

  call PTBAL

  !     CALCULATE TOP HEIGHT: AVERAGE HEIGHT OF THE 40 TREES PER ACRE
  !     WITH THE LARGEST DBH'S.

  AVH = 0.
  if (ITRN.le.0) goto 70
  SSUMN = 0.
  do I = 1,ITRN
     II = IND(I)
     P = PROB(II)
     if (SSUMN+P.gt.40.0) P = 40.0-SSUMN
     SSUMN = SSUMN+P
     AVH = AVH+HT(II)*P
     if (SSUMN.ge.40.0) go to 65
     60 continue
  end do
  65 continue
  if (SSUMN .gt. 0.) AVH = AVH/SSUMN
  70 continue
  if (DEBUG) write(JOSTND,9002) GROSPC,TPROB,TSUMD2,RELDEN,BA, &
       & RMSQD,AVH,DR016
  9002 format(' IN DENSE, GROSPC = ',F10.4,', TPROB = ',F10.4, &
       & ', TSUMD2 = ',F10.2,', RELDEN = ',F10.4,', BA = ', &
       & F10.4/'     RMSQD = ',F10.4,', AVH = ',F10.4, &
       & ' DR016= ',F10.4)
  return
end subroutine DENSE
