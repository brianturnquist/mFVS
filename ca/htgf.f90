!======================================!
!   Compute large tree height growth   !
!======================================!
! CA $Id$
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
  real    :: AGP10,HGUESS,SCALE,XHT,SINDX,AGMAX,H,POTHTG,XMOD,CRATIO
  real    :: RELHT,CRMOD,RHMOD,TEMHTG,PBAL
  real    :: SITAGE,SITHT,HTMAX,HTMAX2,D1,D2,LTHTG,DGLT
  real    :: MISHGF
  real    :: BRAT,BRATIO,DG10,HGBND

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'HTGF',4,ICYC)
  if (DEBUG) write(JOSTND,3)ICYC
  3 format(' ENTERING SUBROUTINE HTGF CYCLE =',I5)

  SCALE = FINT/YR

  !  GET THE HEIGHT GROWTH MULTIPLIERS.

  call MULTS (2,IY(ICYC),XHMULT)
  if (DEBUG)write(JOSTND,*)'HTGF- IY(ICYC),XHMULT= ',&
       &IY(ICYC), XHMULT

  !   BEGIN SPECIES LOOP:

  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (I1.eq.0) go to 40
     I2 = ISCT(ISPC,2)
     XHT = XHMULT(ISPC)
     SINDX = SITEAR(ISPC)

     !   BEGIN TREE LOOP WITHIN SPECIES LOOP

     do I3 = I1,I2
        I = IND1(I3)
        HTG(I) = 0.
        if (PROB(I).le.0.0) go to 161
        H = HT(I)
        AGP10 = 0.0
        HGUESS = 0.0

        SITAGE = 0.0
        SITHT = 0.0
        AGMAX = 0.0
        HTMAX = 0.0
        HTMAX2 = 0.0
        D1 = DBH(I)
        D2 = 0.0
        DGLT = DG(I)

        PBAL = PTBAA(ITRE(I))*(1.0-(PCT(I)/100.))

        ! BEGIN PROCESSING SPECIES
        ! DETERMINE HTG CALCULATION METHOD BASED ON SPECIES
        select case(ISPC)

           ! BRANCH FOR RW AND GS
        case(23,50)

           !  CALCULATE HTG FOR REDWOOD AND GIANT SEQUIOA USING THE FOLLOWING
           !  FUNCTIONAL FORM

           !  HI = EXP(X)
           !  X = B1 + D1^2 + LOG(D1) + LOG(SI) + LOG(DGLT) + LOG(H)

           !  WHERE
           !  HI = ANNUAL HEIGHT INCREMENT
           !  D1 = DIAMETER AT BREAST HEIGHT
           !  SI = SITE INDEX (BASE AGE 50)
           !  DGLT = 10-YEAR OUTSIDE BARK DIAMETER GROWTH
           !  H = TOTAL TREE HEIGHT

           ! CONVERT DGLT TO OUTSIDE BARK DIAMETER GROWTH
           BRAT = BRATIO(ISPC,D1,H)
           DG10 = DGLT/BRAT

           ! APPLY CONSTRAINTS IF H OF INCOMING TREE IS LESS THAN 4.5 FT
           ! HERE DG10 IS ASSUMED TO BE 0.1
           if (H .lt.4.5) DG10 = 0.1

           if (DEBUG)write(JOSTND,*)' IN HTGF - RW/GS DEBUG',' I=',I,&
                &' ISPC=',ISPC,' D=',D1,' BRAT=',BRAT,' DGLT=', DGLT,&
                &' DG10=',DG10

           LTHTG = EXP(1.412947 - 0.000204*D1**2 + 0.31971*LOG(D1) +&
                &0.394005*LOG(SINDX) + 0.399888*LOG(DG10) - 0.451708*LOG(H))

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

           if (DEBUG)write(JOSTND,*)'IN HTGF - RW/GS DEBUG',' H=',H,&
                &' HGBND=',HGBND,' LTHTG=',LTHTG

           HTG(I) = LTHTG * HGBND

           ! DEBUG
           if (DEBUG)write(JOSTND,*)'IN HTGF - RW/GS DEBUG',' D=', D1,&
                &' H=',H,' SI=',SINDX,' DG10=', DG10,' HTG10YR=',HTG(I)

           !  BEGIN PROCESSING OF ALL OTHER SPECIES IN CA
        case default
           call FINDAG(I,ISPC,D1,D2,H,SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,&
                &DEBUG)

           !  NORMAL HEIGHT INCREMENT CALCULATON BASED ON INCOMMING TREE AGE
           !  FIRST CHECK FOR MAX, ASMYPTOTIC HEIGHT

           if (SITAGE .gt. AGMAX) then
              POTHTG = 0.10
              go to 1320
           else
              AGP10 = SITAGE + 10.0
           end if

           ! R5 USE DUNNING/LEVITAN SITE CURVE.
           ! R6 USE VARIOUS SPECIES SITE CURVES.
           ! SPECIES DIFFERENCES ARE ARE ACCOUNTED FOR BY THE SPECIES
           ! SPECIFIC SITE INDEX VALUES WHICH ARE SET AFTER KEYWORD PROCESSING.

           call HTCALC(IFOR,SINDX,ISPC,AGP10,HGUESS,JOSTND,DEBUG)

           POTHTG = HGUESS - SITHT

           if (DEBUG)write(JOSTND,91200)I,ISPC,AGP10,HGUESS,H
           91200 format(' IN GUESS AN AGE--I,ISPC,AGEP10,HGUESS,H ',2I5,3F10.2)

           ! ASSIGN A POTENTIAL HTG FOR THE ASYMPTOTIC AGE

           1320 continue
           XMOD = 1.0
           CRATIO = ICR(I)/100.0
           RELHT = H/AVH
           if (RELHT .gt.1.0)RELHT = 1.0
           if (PCCF(ITRE(I)) .lt.100.0)RELHT = 1.0

           !  THE TREE HEIGHT GROWTH MODIFIER (SMHMOD) IS BASED ON THE RITCHIE &
           !  HANN WORK (FOR.ECOL.&MGMT. 1986. 15:135-145).  THE ORIGINAL COEFF.
           !  (1.117148) IS CHANGED TO 1.016605 TO MAKE THE SMALL TREE HEIGHTS
           !  CLOSE TO THE SITE INDEX CURVE.  THE MODIFIER HAS TWO PARTS, ONE
           !  (CRMOD) FOR TREE VIGOR USING CROWN RATIO AS A SURROGATE; OTHER
           !  (RHMOD) FOR COMPETITION FROM NEIGHBORING TREES USING RELATIVE TREE
           !  HEIGHT AS A SURROGATE.

           CRMOD = (1.0-EXP(-4.26558*CRATIO))
           RHMOD = (EXP(2.54119*(RELHT**0.250537-1.0)))
           XMOD = 1.016605*CRMOD*RHMOD
           HTG(I) = POTHTG * XMOD

           if (DEBUG)write(JOSTND,901)ICR(I),PCT(I),BA,DG(I),HT(I),&
                &POTHTG,AVH,HTG(I),DBH(I),RMAI,HGUESS,AGP10,XMOD,ABIRTH(I)
           901 format(' HTGF',I5,14F9.2)
        end select

        !  ENFORCE MINIMUM HEIGHT GROWTH
        if (HTG(I) .lt.0.1) HTG(I) = 0.1

        !  HTG IS MULTIPLIED BY SCALE TO CHANGE FROM A YR  PERIOD TO FINT AND
        !  MULTIPLIED BY XHT TO APPLY USER SUPPLIED GROWTH MULTIPLIERS.

        HTG(I) = SCALE*XHT*HTG(I)*EXP(HTCON(ISPC))

        if (DEBUG)write(JOSTND,*)' I=',I,&
             &' D=',DBH(I),' DG=',DG(I),' H=',H,' HTG=',HTG(I)

        161 continue

        !    APPLY DWARF MISTLETOE HEIGHT GROWTH IMPACT HERE,
        !    INSTEAD OF AT EACH FUNCTION IF SPECIAL CASES EXIST.

        HTG(I) = HTG(I)*MISHGF(I,ISPC)
        TEMHTG = HTG(I)

        if (DEBUG)write(JOSTND,*)' TEMHTG=',TEMHTG

        ! CHECK FOR SIZE CAP COMPLIANCE.

        if ((HT(I)+HTG(I)).gt.SIZCAP(ISPC,4)) then
           HTG(I) = SIZCAP(ISPC,4)-HT(I)
           if (HTG(I) .lt.0.1) HTG(I) = 0.1
        end if

        if (DEBUG)write(JOSTND,*)' HTG(I)=',HTG(I)

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
        9001 format( ' UPPER HTG =',F8.4,' LOWER HTG =',F8.4)

        !   END OF TREE LOOP

        30 continue
     end do

     !   END OF SPECIES LOOP

     40 continue
  end do
  if (DEBUG)write(JOSTND,60)ICYC
  60 format(' LEAVING SUBROUTINE HTGF   CYCLE =',I5)
  return

  entry HTCONS

  !  ENTRY POINT FOR LOADING HEIGHT INCREMENT MODEL COEFFICIENTS THAT
  !  ARE SITE DEPENDENT AND REQUIRE ONE-TIME RESOLUTION.  HGHC
  !  CONTAINS HABITAT TYPE INTERCEPTS, HGLDD CONTAINS HABITAT
  !  DEPENDENT COEFFICIENTS FOR THE DIAMETER INCREMENT TERM, HGH2
  !  CONTAINS HABITAT DEPENDENT COEFFICIENTS FOR THE HEIGHT-SQUARED
  !  TERM, AND HGHC CONTAINS SPECIES DEPENDENT INTERCEPTS.  HABITAT
  !  TYPE IS INDEXED BY ITYPE (SEE /PLOT/ COMMON AREA).

  !  LOAD OVERALL INTERCEPT FOR EACH SPECIES.

  do ISPC = 1,MAXSP
     HTCON(ISPC) = 0.0
     if (LHCOR2.and. HCOR2(ISPC).gt.0.0) HTCON(ISPC) = &
          &HTCON(ISPC)+ALOG(HCOR2(ISPC))
     50 continue
  end do
  return
end
