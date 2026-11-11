!======================================!
!   Compute large tree height growth   !
!======================================!
! NC $Id$
subroutine HTGF
  implicit none

  !  THIS SUBROUTINE COMPUTES THE PREDICTED PERIODIC HEIGHT
  !  INCREMENT FOR EACH CYCLE AND LOADS IT INTO THE ARRAY HTG.
  !  HEIGHT INCREMENT IS PREDICTED FROM SPECIES, HABITAT TYPE,
  !  HEIGHT, DBH, AND PREDICTED DBH INCREMENT.  THIS ROUTINE
  !  IS CALLED FROM **TREGRO** DURING REGULAR CYCLING.  ENTRY
  !  **HTCONS** IS CALLED FROM **RCON** TO LOAD SITE DEPENDENT
  !  CONSTANTS THAT NEED ONLY BE RESOLVED ONCE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'
  include 'OUTCOM.f90'
  include 'PLOT.f90'
  include 'MULTCM.f90'
  include 'HTCAL.f90'
  include 'PDEN.f90'
  include 'VARCOM.f90'

  ! Variables
  logical :: DEBUG
  integer :: I,ISPC,I1,I2,I3,ITFN
  real    :: AGP05,HGUESS,SCALE,P1,P2,P3,P4,SINDX,XHT,H,POTHTG
  real    :: XMOD,RELHT,CR,TEMHTG
  real    :: HD1(MAXSP),HD2(MAXSP),HD3(MAXSP),HD4(MAXSP)
  real    :: D1,D2,SITAGE,SITHT,AGMAX,HTMAX,HTMAX2
  real    :: MISHGF,PBAL,LTHTG,CRATIO,DGLT
  real    :: BRAT,BRATIO,DG10,HGBND

  !  DATA STATEMENTS

  data HD1/4*0.0,4.4666 ,0.0, 4.80758, 4.9684, 0.0,0.0,4.9684,&
       &0.0/

  data HD2/4*0.0,-0.00179,0.0,-0.00224,-0.004057,2*0.0,-0.004057,&
       &0.0/

  data HD3/4*0.0,0.002048,0.0,-0.000513,0.000924,2*0.0,0.000924,&
       &0.0/

  data HD4/4*0.0,-7.9428,0.0,-7.729644,-10.45158,2*0.0,-10.45158,&
       &0.0/

  !   MODEL COEFFICIENTS AND CONSTANTS:

  !    IND2 -- ARRAY OF POINTERS TO SMALL TREES.

  !   SCALE -- TIME FACTOR DERIVED BY DIVIDING FIXED POINT CYCLE
  !            LENGTH BY GROWTH PERIOD LENGTH FOR DATA FROM
  !            WHICH MODELS WERE DEVELOPED.

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'HTGF',4,ICYC)
  if (DEBUG) write(JOSTND,3)ICYC
  3 format(' ENTERING SUBROUTINE HTGF  CYCLE =',I5)
  if (DEBUG)write(JOSTND,*) 'IN HTGF AT BEGINNING,HTCON=',&
       &HTCON,'RMAI=',RMAI,'ELEV=',ELEV

  AGP05 = 0.0
  SCALE = FINT/YR

  !  GET THE HEIGHT GROWTH MULTIPLIERS.

  call MULTS (2,IY(ICYC),XHMULT)

  !   BEGIN SPECIES LOOP. LOAD BETA COEFFICIENTS.

  do ISPC = 1,MAXSP

     ! FOLLOWING ARE COEFS FOR THE HT DIA BASED HTG FCN

     P1 = HD1(ISPC)
     P2 = HD2(ISPC)
     P3 = HD3(ISPC)
     P4 = HD4(ISPC)
     SINDX = SITEAR(ISPC)
     XHT = 1.0
     XHT = XHMULT(ISPC)

     if (DEBUG)write(JOSTND,*)'HTDIACOF',ISPC,P1,P2,P3,P4

     ! END OF BETS COEFS

     I1 = ISCT(ISPC,1)
     if (I1.eq.0) go to 4000
     I2 = ISCT(ISPC,2)

     !   BEGIN TREE LOOP WITHIN SPECIES LOOP

     do I3 = I1,I2
        I = IND1(I3)
        HTG(I) = 0.
        if (PROB(I) .le.0.0)go to 2600
        H = HT(I)

        HGUESS = 0.0
        SITAGE = 0.0
        SITHT = 0.0
        AGMAX = 0.0
        HTMAX = 0.0
        HTMAX2 = 0.0
        D1 = DBH(I)
        D2 = 0.0
        DGLT = DG(I)
        CRATIO = ICR(I)
        PBAL = PTBAA(ITRE(I))*(1.0-(PCT(I)/100.))

        ! BEGIN PROCESSING SPECIES
        ! DETERMINE HTG CALCULATION METHOD BASED ON SPECIES
        select case(ISPC)

           ! BRANCH FOR RW
        case(12)

           !  CALCULATE HTG FOR REDWOOD USING THE FOLLOWING FUNCTIONAL FORM

           !  HI = EXP(X)
           !  X = B1 + D1^2 + LOG(D1) + LOG(SI) + LOG(DGLT) + LOG(H)

           !  WHERE
           !  HI = ANNUAL HEIGHT INCREMENT
           !  D1 = DIAMETER AT BREAST HEIGHT
           !  SI = SITE INDEX (BASE AGE 50)
           !  DGLT = 10-YEAR OUTSIDE BARK DIAMETER GROWTH
           !  H = TOTAL TREE HEIGHT

           ! SCALE DGLT TO 10 YEAR DIAMETER GROWTH
           DGLT = DGLT * 2.0

           ! CONVERT DGLT TO OUTSIDE BARK DIAMETER GROWTH
           BRAT = BRATIO(ISPC,D1,H)
           DG10 = DGLT/BRAT

           ! APPLY CONSTRAINTS IF H OF INCOMING TREE IS LESS THAN 4.5 FT
           ! HERE DG10 IS ASSUMED TO BE 0.1
           if (H .lt.4.5) DG10 = 0.1

           if (DEBUG)write(JOSTND,*)' IN HTGF - RW DEBUG',' I=',I,&
                &' ISPC=',ISPC,' D=',D1,' BRAT=',BRAT,' DGLT=', DGLT,&
                &' DG10=',DG10

           LTHTG = EXP(1.412947 - 0.000204*D1**2 + 0.31971*LOG(D1) +&
                &0.394005*LOG(SINDX) + 0.399888*LOG(DG10) - 0.451708*LOG(H))

           ! SCALE LTHTG TO 5 YEARS
           LTHTG = LTHTG * 0.5

           ! BOUND HEIGHT GROWTH BASED ON THE HEIGHT OF RECORD. BOUNDING IS
           ! APPLIED TO AVOID HAVING TREES REACH UNREALISTIC HEIGHTS.THE HEIGHT
           ! GROWTH BOUNDING FUNCTION PROPORTIONALLY ADJUSTS HEIGHT GROWTH VALUES
           ! SO HEIGHTS OF A RECORD WILL EVENTUALLY CONVERGE TO THE UPPER HEIGHT
           ! BOUNDING VALUE. LOWER BOUNDING VALUE (217 FT) IS BASED ON MAXIMUM TREE
           ! HEIGHT FOUND IN THE DATASET USED TO FIT THE RW HEIGHT GROWTH EQUATION.
           ! UPPER BOUNDING VALUE FOUND IN THE DATASET IS BASED ON CURRENT HEIGHT
           ! MAXIMUM FOUND IN NATURE: HYPERION REDWOOD (~380 FT).

           ! BOUNDING LOGIC:
           ! 1) IF HT IS BETWEEN 217 FT AND 380 FT, THEN HEIGHT GROWTH IS
           !    BOUND.
           ! 2) IF THE HT IS BELOW 217 FT THEN HEIGHT GROWTH IS NOT
           !    BOUND.
           ! 3) IF THE HT IS ABOVE 380 FT, THEN HEIGHT GROWTH BOUNDING
           !    VALUE IS SET TO 0.1.
           if (H .ge.217.0.and. H .lt.380.0) then
              HGBND = 1.0 - ((H - 217.0)/(380.0 - 217.0))
              if (HGBND .lt.0.1) HGBND = 0.1
           else if (H .lt.217.0) then
              HGBND = 1.0
           else
              HGBND = 0.1
           end if

           if (DEBUG)write(JOSTND,*)'IN HTGF - RW DEBUG',' H=',H,&
                &' HGBND=',HGBND,' LTHTG=',LTHTG

           HTG(I) = LTHTG * HGBND

           ! DEBUG
           if (DEBUG)write(JOSTND,*)'IN HTGF - RW DEBUG',' D=', D1,&
                &' H=',H,' SI=',SINDX, ' DG10=', DG10,' HTG5YR=',HTG(I)

           !  BEGIN PROCESSING OF ALL OTHER SPECIES IN NC
        case default
           if (DEBUG)write(JOSTND,*)' IN HTGF, CALLING FINDAG I= ',I
           call FINDAG(I,ISPC,D1,D2,H,SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,&
                &DEBUG)

           !   CALCULATE THE HTG VIA SITE AND MODIFER TECHNIQUE

           if (H .ge. HTMAX) then
              HTG(I) = 0.1
              HTG(I) = SCALE*XHT*HTG(I)*EXP(HTCON(ISPC))
              go to 2600
           end if

           !  NORMAL HEIGHT INCREMENT CALCULATON BASED ON TREE AGE
           !  FIRST CHECK FOR MAXIMUM TREE AGE

           if (SITAGE .ge. AGMAX) then
              POTHTG = 0.10
              go to 140
           else
              AGP05 = SITAGE + 5.0
           end if

           !  CALL HTCALC FOR NORMAL CYCLING

           call HTCALC(SINDX,ISPC,AGP05,HGUESS,JOSTND,DEBUG)
           POTHTG = HGUESS - SITHT
           if (DEBUG)write(JOSTND,*)' AGP05, HGUESS, H= ',AGP05,HGUESS,H

           ! ASSIGN A POTENTIAL HTG FOR THE ASYMPTOTIC AGE

           140 continue
           XMOD = 1.0
           if (PCCF(ITRE(I)) .lt.50.0)go to 170
           RELHT = H/AVH

           ! LESSEN THE IMPACT OF RELHT FOR MODERATELY STOCKED STANDS

           if (PCCF(ITRE(I)) .lt.100.0) RELHT = (RELHT + 1.0) / 2.0
           if (RELHT .gt.1.0)RELHT = 1.0
           CR = ICR(I)/10.

           ! FOLLOWING MODIFIER IS BASED ON DF DATA

           XMOD = -0.02647 + 0.71338*RELHT*RELHT + 0.06851*CR
           170 continue
           HTG(I) = POTHTG * XMOD
        end select

        ! CONSTRAIN HTG IF NEEDED
        if (HTG(I) .le.0.1)HTG(I) = 0.01

        !   HEIGHT GROWTH EQUATION, EVALUATED FOR EACH TREE EACH CYCLE
        !   MULTIPLIED BY SCALE TO CHANGE FROM A YR. PERIOD TO FINT AND
        !   MULTIPLIED BY XHT TO APPLY USER SUPPLIED GROWTH MULTIPLIERS.

        HTG(I) = SCALE*XHT*HTG(I)*EXP(HTCON(ISPC))
        if (DEBUG)write(JOSTND,*)' I,ISPC,XHT,SCALE,HTG,HTCON= ',&
             &I,ISPC,XHT,SCALE,HTG(I),HTCON(ISPC)
        2600 continue

        !    APPLY DWARF MISTLETOE HEIGHT GROWTH IMPACT HERE,
        !    INSTEAD OF AT EACH FUNCTION IF SPECIAL CASES EXIST.

        HTG(I) = HTG(I)*MISHGF(I,ISPC)
        TEMHTG = HTG(I)

        ! CHECK FOR SIZE CAP COMPLIANCE.

        if ((HT(I)+HTG(I)).gt.SIZCAP(ISPC,4)) then
           HTG(I) = SIZCAP(ISPC,4)-HT(I)
           if (HTG(I) .lt.0.1) HTG(I) = 0.1
        end if

        if (.not.LTRIP) go to 3000
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
        9001 format( ' UPPER HTG =',F8.4,' LOWER HTG =',F8.4)

        3000 continue
     end do

     !   END OF SPECIES LOOP

     4000 continue
  end do
  if (DEBUG)write(JOSTND,60)ICYC
  60 format(' LEAVING SUBROUTINE HTGF   CYCLE =',I5)
  return

  entry HTCONS

  !  ENTRY POINT FOR LOADING HEIGHT INCREMENT MODEL COEFFICIENTS THAT
  !  ARE SITE DEPENDENT AND REQUIRE ONE-TIME RESOLUTION.

  do ISPC = 1,MAXSP
     HTCON(ISPC) = 0.0
     if (LHCOR2.and. HCOR2(ISPC).gt.0.0) HTCON(ISPC) = &
          &HTCON(ISPC)+ALOG(HCOR2(ISPC))
     50 continue
  end do

  return
end
