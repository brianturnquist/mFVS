!======================================!
!   Compute large tree height growth   !
!======================================!
! CANADA-ON $Id$
subroutine HTGF
  implicit none

  !  THIS SUBROUTINE COMPUTES THE PREDICTED PERIODIC HEIGHT
  !  INCREMENT FOR EACH CYCLE AND LOADS IT INTO THE ARRAY HTG.
  !  HEIGHT INCREMENT IS PREDICTED FROM SPECIES, SITE INDEX,
  !  BASAL AREA/ACRE, AND DBH.  THIS ROUTINE IS CALLED FROM
  !  **TREGRO** DURING REGULAR CYCLING.  ENTRY **HTCONS**
  !  IS CALLED FROM **RCON** TO LOAD SITE DEPENDENT CONSTANTS
  !  THAT NEED ONLY BE RESOLVED ONCE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'COEFFS.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'
  include 'MULTCM.f90'
  include 'PDEN.f90'
  include 'HTCAL.f90'
  include 'METRIC.f90'

  ! Variables
  logical :: DEBUG
  integer :: I,ISPC,I1,I2,I3,IS,ITFN
  real    :: SCALE,BA10,BARK,BRATIO,DBH10,XHT,XHT2,HTNOW,HT10,GM,TEMHTG
  real    :: XTREES,SUMDBHSQ,X,QMD10

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'HTGF',4,ICYC)
  if (DEBUG) write(JOSTND,3)ICYC
  3 format(' ENTERING SUBROUTINE HTGF CYCLE =',I5)
  if (DEBUG)write(JOSTND,*) 'IN HTGF AT BEGINNING, HTCON=',&
       &HTCON,'RMAI=',RMAI,'ELEV=',ELEV
  SCALE = FINT/YR

  !  GET THE HEIGHT GROWTH MULTIPLIERS.

  call MULTS (2,IY(ICYC),XHMULT)
  BA10 = 0.
  XTREES = 0.
  SUMDBHSQ = 0.

  !  DETERMINE BA 10 YEARS FROM NOW

  do I = 1,ITRN
     BARK = BRATIO(ISP(I),DBH(I),HT(I))
     DBH10 = DBH(I)+DG(I)/BARK
     X = PROB(I)*DBH10**2
     BA10 = BA10 + 0.0054542 * X
     XTREES = XTREES + PROB(I)
     SUMDBHSQ = SUMDBHSQ + X
     20 continue
  end do
  QMD10 = 0.0
  if (XTREES .gt.0.0) QMD10 = SQRT(SUMDBHSQ/XTREES)

  if (DEBUG) write(JOSTND,*)'  BA,BA10= ',BA,BA10

  !  FOR CALCULATION OF HEIGHT GROWTH, IF BA10 IS LESS THAN BA (E.G. AS
  !  WOULD HAPPEN IN A THIN) SET BA10 TO BA TO AVOID SHRINKING TREES

  if (BA10.lt. BA) BA10 = BA

  !  BEGIN SPECIES LOOP.

  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (I1.eq.0) go to 40
     I2 = ISCT(ISPC,2)
     XHT = XHMULT(ISPC)
     XHT2 = HCOR2(ISPC)

     !  BEGIN TREE LOOP WITHIN SPECIES LOOP

     do I3 = I1,I2
        I = IND1(I3)
        IS = ISP(I)
        BARK = BRATIO(IS,DBH(I),HT(I))
        HTG(I) = 0.
        DBH10 = DBH(I)+DG(I)/BARK

        if (DEBUG) write(JOSTND,*) 'I=',I,'  ISPC=',ISPC,'  DBH=',DBH(I),&
             &' SITEAR(ISPC)=',SITEAR(ISPC)

        ! BEGIN SECTION FOR CALCULATING USING THE ONTARIO EQUATIONS.
        !     NOTE THAT ALL CALCS ARE NOW DONE IN A SUBROUTINE.

        call HTONT(ISPC,DBH(I),RMSQD,BA,HTNOW)
        call HTONT(ISPC,DBH10,QMD10,BA10,HT10)

        !  HEIGHT GROWTH EQUATION EVALUATED FOR EACH TREE EACH CYCLE
        !  MULTIPLIED BY SCALE TO CHANGE FROM A YR. PERIOD TO FINT
        !  AND MULTIPLIED BY XHT TO APPLY USER SUPPLIED GROWTH MULTIPLIERS.

        HTG(I) = HT10-HTNOW

        !  GET MODIFIER BORROWED FROM THE DIAMETER GROWTH EQUATIONS AND
        !  ADJUST THE HEIGHT GROWTH ESTIMATE.

        call BALMOD(ISPC,DBH(I),BA,RMSQD,GM,DEBUG)
        HTG(I) = HTG(I)*GM

        if (HTG(I) .lt.0.1) HTG(I) = 0.1
        if (DEBUG) write(JOSTND,*) 'I=',I,'  HT10=',HT10,'  HTNOW=',&
             &HTNOW, '  HTG(I)=',HTG(I),' XHT=',XHT,' XHT2=',XHT2
        HTG(I) = SCALE*XHT*HTG(I)*XHT2
        TEMHTG = HTG(I)

        ! CHECK FOR SIZE CAP COMPLIANCE.

        if ((HT(I)+HTG(I)).gt.SIZCAP(ISPC,4)) then
           HTG(I) = SIZCAP(ISPC,4)-HT(I)
           if (HTG(I) .lt.0.1) HTG(I) = 0.1
        end if

        if (.not.LTRIP) go to 30
        ITFN = ITRN+2*I-1
        HTG(ITFN) = TEMHTG

        ! CHECK FOR SIZE CAP COMPLIANCE.

        if ((HT(ITFN)+HTG(ITFN)).gt.SIZCAP(ISPC,4)) then
           HTG(ITFN) = SIZCAP(ISPC,4)-HT(ITFN)
           if (HTG(ITFN) .lt.0.1) HTG(ITFN) = 0.1
        end if

        HTG(ITFN+1) = TEMHTG

        ! CHECK FOR SIZE CAP COMPLIANCE.

        if ((HT(ITFN+1)+HTG(ITFN+1)).gt.SIZCAP(ISPC,4)) then
           HTG(ITFN+1) = SIZCAP(ISPC,4)-HT(ITFN+1)
           if (HTG(ITFN+1) .lt.0.1) HTG(ITFN+1) = 0.1
        end if

        if (DEBUG) write(JOSTND,9001) HTG(ITFN),HTG(ITFN+1)
        9001 format( ' UPPER HTG =',F8.4,'LOWER HTG =',F8.4)

        !  END OF TREE LOOP

        30 continue
     end do

     !  END OF SPECIES LOOP

     40 continue
  end do

  return

  entry HTCONS

  !  LOAD OVERALL INTERCEPT FOR EACH SPECIES

  do ISPC = 1,MAXSP
     HTCON(ISPC) = 0.0
     60 continue
  end do
  return
end
