!====================!
!   vbase : evpred   !
!====================!
! VBASE $Id$
subroutine EVPRED
  implicit none

  !  THIS SUBROUTINE COMPUTES VALUES OF PRE-DEFINED EVENT MONITOR
  !  VARIABLES, WHERE THE COMPUTATION IS VERY INVOLVED.

  !  THIS ROUTINE IS CALLED FROM **EVTSTV**.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'

  ! Variables
  integer :: I,ISPC,INDEX(MAXTRE),ISRTI
  real    :: FINDX,TEMP,CCPCT,BASM,ADHW,DMAX,DSNMAX,D,SUMTPA,P
  real    :: X1,X2,X3,X4,X5,SUMPIN,HTMAX
  logical :: DEBUG,LFIRE2

  entry FISHER (FINDX)

  !  THIS ENTRY COMPUTES A VALUE FOR THE FISHER RESTING HABITAT
  !  SUITABILITY

  !  REF: ZIELINSKI, WILLIAM J.; TRUEX, RICHARD L.; DUNK, JEFFREY R.;
  !       GAMAN, TOM. 2006. USING FOREST INVENTORY DATA TO ASSESS FISHER
  !       RESTING HABITAT SUITABILITY IN CALIFORNIA. ECOLOGICAL
  !       APPLICATIONS 16(3). PP 1010-1025.

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'FISHER',6,ICYC)
  if (DEBUG)write(JOSTND,10)ICYC
  10 format(' ENTERING SUBROUTINE EVPDEF, ENTRY FISHER, CYCLE =',I4)

  ! Initialize variables
  TEMP = 0.
  FINDX = 0.
  BASM = 0.
  CCPCT = 0.
  ADHW = 0.
  DMAX = 0.
  DSNMAX = 0.

  !  IF FIRE MODEL IS NOT ACTIVE, FOR SNAG PROCESSING, RETURN A ZERO.

  call FMATV(LFIRE2)
  if (.not. LFIRE2)go to 500

  !  THIS IS CURRENTLY ONLY ALLOWED IN THE CALIFORNIA VARIANTS AND
  !  ONLY FOR R5 FORESTS (I.E. BASED ON R5 CROWN WIDTH EQNS).

  if (VARACD.eq.'NC' .and. IFOR.ne.4.and. IFOR.ne.5) then
     go to 20
  else if ((VARACD.eq.'CA'.or.VARACD.eq.'OC').and.IFOR.lt.6) then
     go to 20
  else if (VARACD.eq.'SO' .and. (IFOR.gt.3.and. IFOR.lt.10)) then
     go to 20
  else if (VARACD.eq.'WS') then
     go to 20
  else
     go to 500
  end if
  20 continue
  if (ITRN .le.0) go to 500

  !  CANOPY COVER PERCENT IS ONLY FOR PREDOMINANT, DOMINANT, AND CODOMINENT
  !  TREES. PROXY THIS BY TAKING ALL TREES WHICH ARE AT LEAST AS TALL AS
  !  50% OF THE HEIGHT OF THE 90TH %-TILE TREE IN THE HEIGHT DISTRIBUTION

  !  EXCLUDE TREES WITH CROWN RATIO LESS THAN 30% FROM THE CALCULATION.
  !  EXCLUDE TREES WITH DBH LT 1 INCH FROM THE CALCULATION
  !  FIRST SORT BY HT. NEXT LOOP THROUGH THE TREES AND COMPUTE PERCENT
  !  CANOPY COVER.

  do I = 1,MAXTRE
     if (I .le. ITRN) then
        INDEX(I) = I
     else
        INDEX(I) = 0
     end if
     25 continue
  end do
  call RDPSRT(ITRN,HT,INDEX,.false.)
  if (DEBUG)write(JOSTND,*)' INDEX = ',(INDEX(I),I = 1,ITRN)

  SUMPIN = 0.
  HTMAX = 0.
  do I = 1,ITRN
     ISRTI = INDEX(I)
     P = PROB(ISRTI)
     IF(DEBUG)WRITE(JOSTND,*)' IN FISHER CCPCT1, SUMPIN,ISRTI,P,HT,HTMA&
          &X,TPROB= ',SUMPIN,ISRTI,P,HT(ISRTI),HTMAX,TPROB
     if (DBH(ISRTI) .lt.1.) goto 30
     if (ICR(ISRTI) .lt.31) goto 30
     if (HT(ISRTI) .ge. HTMAX) then
        CCPCT = CCPCT + P*CRWDTH(ISRTI)**2.0
        SUMPIN = SUMPIN + P
        if (DEBUG)write(JOSTND,*)' ISRTI,P,CRWDTH,CCPCT,SUMPIN= ',&
             &ISRTI,P,CRWDTH(ISRTI),CCPCT,SUMPIN
     end if
     if (SUMPIN.gt.TPROB*0.10.and. HTMAX.eq.0.)HTMAX = HT(ISRTI)*.50
     30 continue
  end do
  CCPCT = 100.0*CCPCT*0.785398/43560.

  !  LOOP THROUGH TREES AND CALCULATE:
  !  1) BASAL AREA IN SMALL TREES, 5-51 CENTIMETERS DBH
  !     (BASM: SQUARE METERS/HA)
  !  2) PERCENT CROWN COVER OF DOMINANT AND CODOMINANT TREES
  !     (CCPCT: PERCENT, ALLOW FOR OVERLAP)
  !  3) ARITHMETIC AVERAGE DIAMETER OF ALL HARDWOODS
  !     (ADHW: CENTIMETERS)
  !  4) DIAMETER OF THE LARGEST LIVE TREE IN THE STAND
  !     (DMAX: CENTIMETERS)
  !  5) DIAMETER OF THE LARGEST CONIFER SNAG IN THE STAND
  !     (DSNMAX: CENTIMETERS)

  SUMTPA = 0.
  do I = 1,ITRN
     ISPC = ISP(I)
     D = DBH(I)
     P = PROB(I)
     if (D .gt. DMAX)DMAX = D

     !     NOTE: IN THE DIAMETER SCREEN BELOW, WE ARE USING 5 INCHES AS THE LOWER
     !           LIMIT INSTEAD OF 5 CM (1.9685 INCHES) TO BE CONSISTENT WITH
     !           THE WAY ANDREW GRAY (PNW-FIA) SAYS IT NEEDS TO BE CALCULATED
     !           WHICH IS CONTRARY TO THE PUBLICATION. THIS MAY NEED TO BE
     !           CHANGED.
     !***     IF(D.GE.1.9685 .AND. D.LT.20.0787)THEN

     if (D.ge.5.0.and. D.lt.20.0787) then
        BASM = BASM + 0.0054542*P*D*D
     end if
     if (VARACD.eq.'WS' .and. (ISPC.eq.7.or. ISPC.eq.11)) then
        ADHW = ADHW + D*P
        SUMTPA = SUMTPA + P
     else if (VARACD.eq.'NC' .and. (ISPC.eq.5.or. ISPC.eq.7.or.&
          &ISPC.eq.8.or. ISPC.eq.11)) then
        ADHW = ADHW + D*P
        SUMTPA = SUMTPA + P
     else if ((VARACD.eq.'CA' .or. VARACD.eq.'OC').and.ISPC.ge.26) then
        ADHW = ADHW + D*P
        SUMTPA = SUMTPA + P
     else if (VARACD.eq.'SO' .and. ((ISPC.ge.21.and. ISPC.le.31) .or.&
          &ISPC.eq.33)) then
        ADHW = ADHW + D*P
        SUMTPA = SUMTPA + P
     end if
     90 continue
  end do
  if (DEBUG)write(JOSTND,*)' IN FISHER DMAX,ADHS,SUMTPA,BASM,CCPCT= '&
       &,DMAX,ADHW,SUMTPA,BASM,CCPCT

  !  CALCULATE:
  !  5) DIAMETER OF THE LARGEST CONIFER SNAG IN THE STAND
  !     (DSNMAX: CENTIMETERS)

  call FMEVMSN(DSNMAX)
  if (DEBUG)write(JOSTND,*)' IN FISHER DSNMAX= ',DSNMAX

  !  CONVERT ENGLISH UNITS TO METRIC UNITS WHERE APPROPRIATE
  !  PERCENT CROWN COVER SHOULD BE EQUIVALENT CALCULATED EITHER WAY
  !  CALCULATE THE FISHER HABITAT SUITABILITY INDEX

  DMAX = DMAX*2.54
  DSNMAX = DSNMAX*2.54
  if (SUMTPA .gt.0.) then
     ADHW = (ADHW/SUMTPA)*2.54
  else
     ADHW = 0.
  end if
  BASM = BASM*0.2295643
  if (DEBUG)write(JOSTND,*)' IN FISHER, CCPCT,BASM,ADHW,DMAX,SLOPE,',&
       &'DSNMAX= ',CCPCT,BASM,ADHW,DMAX,SLOPE,DSNMAX

  if (CCPCT .gt.0.) then
     X1 = ALOG10(CCPCT)
  else
     X1 = 0.
  end if
  if (BASM .gt.0.) then
     X2 = ALOG10(BASM)
  else
     X2 = 0.
  end if
  if (ADHW .gt.0.) then
     X3 = ALOG10(ADHW)
  else
     X3 = 0.
  end if
  if (DMAX .gt.0.) then
     X4 = ALOG10(DMAX)
  else
     X4 = 0.
  end if
  if (SLOPE .gt.0.) then
     X5 = ALOG10(SLOPE*100.)
  else
     X5 = 0.
  end if

  TEMP = -22.1217941 + 2.461062*X1 + 2.15615937*X2 + 0.47133361*X3&
       &+ 4.55271635*X4 + 2.16130549*X5 + 0.00793579*DSNMAX
  FINDX = EXP(TEMP)/(1+EXP(TEMP))

  500 continue
  if (DEBUG)write(JOSTND,*)' LEAVING FISHER, TEMP,FINDX= ',TEMP,FINDX
  return

  !      ENTRY NEXT
  !      RETURN

  !      ENTRY NEXT
  !      RETURN

end
