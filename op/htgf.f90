!======================================!
!   Compute large tree height growth   !
!======================================!
! OP $Id$
subroutine HTGF
  implicit none

  !  THIS SUBROUTINE COMPUTES THE PREDICTED PERIODIC HEIGHT
  !  INCREMENT FOR EACH CYCLE AND LOADS IT INTO THE ARRAY HTG.
  !  HEIGHT INCREMENT IS PREDICTED FROM SPECIES, HABITAT TYPE,
  !  HEIGHT, DBH, AND PREDICTED DBH INCREMENT.  THIS ROUTINE
  !  IS CALLED FROM **TREGRO** DURING REGULAR CYCLING.  ENTRY
  !  **HTCONS** IS CALLED FROM **RCON** TO LOAD SITE DEPENDENT
  !  CONSTANTS THAT NEED ONLY BE RESOLVED ONCE. CALLS **FINDAG
  !  TO CALCULATE TREE AGE.

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
  include 'ORGANON.f90'

  ! Variables
  logical :: DEBUG
  integer :: ITFN,I3,I2,I1,ISPC,I,MAPPHD
  real    :: SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,D2,BARK,BRATIO
  real    :: RHR(MAXSP),RHYXS(MAXSP),RHM(MAXSP),RHB(MAXSP)
  real    :: TEMHTG,HTGMOD,WTRH,WTCR,HGMDRH,FCTRM,FCTRXB,FCTRRB,FCTRKX,RHX
  real    :: HGMDCR,RELHT,POTHTG,TEMPH,H,D,XHT,SINDX
  real    :: CRC,CRB,CRA,RHXS,RHK,AGP10,HGUESS,HGUESS1,HGUESS2,SCALE
  real    :: MAXGUESS
  real    :: MISHGF
  real    :: DG10,DGLT,HGBND,LTHTG

  !  COEFFICIENTS--CROWN RATIO (CR) BASED HT. GRTH. MODIFIER

  data CRA /100.0/, CRB /3.0/, CRC /-5.0/

  !  COEFFICIENTS--RELATIVE HEIGHT (RH) BASED HT. GRTH. MODIFIER

  data RHK /1.0/, RHXS /0.0/

  !  COEFFS BASED ON SPECIES SHADE TOLERANCE AS FOLLOWS:
  !                                   RHR  RHYXS    RHM    RHB
  !        VERY TOLERANT             20.0   0.20    1.1  -1.10
  !        TOLERANT                  16.0   0.15    1.1  -1.20
  !        INTERMEDIATE              15.0   0.10    1.1  -1.45
  !        INTOLERANT                13.0   0.05    1.1  -1.60
  !        VERY INTOLERANT           12.0   0.01    1.1  -1.60
  !  IN THE WC VARIANT, SILVICS OF NORTH AMERICA (AG.HNDBK-654)
  !  WAS USED TO GET SHADE TOLERANCE. SHADE TOLERANCE COEFFICIENTS WERE
  !  DEVEOLPED BY D.DONNELY, TO COMPUTE HGMDRH (RELAT. HT. CONTRIBUTION)
  !  SEQ. NO.   CHAR. CODE    SHADE TOL.   SEQ. NO.  CHAR. CODE    SHADE TOL.
  !      1      SF            TOLN            21     BM            VTOL
  !      2      WF            TOLN            22     RA            INTL
  !      3      GF            TOLN            23     MA            INTL
  !      4      AF            VTOL            24     TO            INTM
  !      5      RF            TOLN            25     GC            INTM
  !      6      SS            TOLN            26     AS            VINT
  !      7      NF            INTM            27     CW            VINT
  !      8      YC            TOLN            28     WO            INTM
  !      9      IC            VTOL            29     J             INTL
  !      10     ES            TOLN            30     LL            VINT
  !      11     LP            VINT            31     WB            INTM
  !      12     JP            INTL            32     KP            VINT
  !      13     SP            INTM            33     PY            VTOL
  !      14     WP            INTM            34     DG            VTOL
  !      15     PP            INTL            35     HT            VINT
  !      16     DF            INTM            36     CH            INTL
  !      17     RW            VTOL            37     WI            VINT
  !      18     RC            VTOL            38
  !      19     WH            VTOL            39     OT            INTM
  !      20     MH            VTOL

  data RHR    / 16.0,  16.0,  16.0,  20.0,  16.0,&
       &16.0,  15.0,  16.0,  20.0,  16.0,&
       &12.0,  13.0,  15.0,  15.0,  13.0,&
       &15.0,  20.0,  20.0,  20.0,  20.0,&
       &20.0,  13.0,  13.0,  15.0,  15.0,&
       &12.0,  12.0,  15.0,  13.0,  12.0,&
       &15.0,  12.0,  20.0,  20.0,  12.0,&
       &13.0,  12.0,  15.0,  15.0/
  data RHYXS /  0.15,  0.15,  0.15,  0.20,  0.15,&
       &0.15,  0.10,  0.15,  0.20,  0.15,&
       &0.01,  0.05,  0.10,  0.15,  0.05,&
       &0.10,  0.20,  0.20,  0.20,  0.20,&
       &0.20,  0.05,  0.05,  0.10,  0.10,&
       &0.01,  0.01,  0.10,  0.05,  0.01,&
       &0.10,  0.01,  0.20,  0.20,  0.01,&
       &0.05,  0.01,  0.10,  0.10/
  data RHM   /  MAXSP*1.10/
  data RHB   / -1.20, -1.20, -1.20, -1.10, -1.20,&
       &-1.20, -1.45, -1.20, -1.10, -1.20,&
       &-1.60, -1.60, -1.45, -1.45, -1.60,&
       &-1.45, -1.10, -1.10, -1.10, -1.10,&
       &-1.10, -1.60, -1.60, -1.45, -1.45,&
       &-1.60, -1.60, -1.45, -1.60, -1.60,&
       &0.10, -1.60, -1.10, -1.10, -1.60,&
       &-1.60, -1.60, -1.45, -1.45/

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'HTGF',4,ICYC)
  if (DEBUG) write(JOSTND,3)ICYC
  3 format(' ENTERING SUBROUTINE HTGF CYCLE =',I5)

  if (DEBUG)write(JOSTND,*) 'IN HTGF AT BEGINNING,HTCON=',&
       &HTCON,'RMAI=',RMAI,'ELEV=',ELEV
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
     SINDX = SITEAR(ISPC)
     XHT = XHMULT(ISPC)

     !   BEGIN TREE LOOP WITHIN SPECIES LOOP

     !   XHT CONTAINS THE HEIGHT GROWTH MULTIPLIER FROM THE HTGMULT KEYWORD
     !   HTCON CONTAINS THE HEIGHT GROWTH MULTIPLIER FROM THE READCORH KEYWORD

     do I3 = I1,I2
        I = IND1(I3)
        HTG(I) = 0.

        !  START ORGANON

        if (LORGANON .and. (IORG(I) .eq.1)) then
           HTG(I) = SCALE*XHT*HGRO(I)*EXP(HTCON(ISPC))
           if (DEBUG)write(JOSTND,*)' HTGF ORGANON I,ISP,DBH,HT,HTG,HGRO,',&
                &'SCALE,XHT,HTCON,IORG= ',I,ISP(I),DBH(I),HT(I),HTG(I),HGRO(I),&
                &SCALE,XHT,HTCON(ISPC),IORG(I)
           go to 161
        end if

        !  END ORGANON

        H = HT(I)
        D = DBH(I)

        HGUESS = 0.0
        SITAGE = 0.0
        SITHT = 0.0
        AGMAX = 0.0
        HTMAX = 0.0
        HTMAX2 = 0.0
        BARK = BRATIO(ISPC,D,H)
        D2 = D + DG(I)/BARK
        if (PROB(I).le.0.0) go to 161

        ! BEGIN PROCESSING SPECIES
        ! DETERMINE HTG CALCULATION METHOD BASED ON SPECIES
        select case(ISPC)

           ! BRANCH FOR RW
        case(17)

           !  CALCULATE HTG FOR REDWOOD USING THE FOLLOWING
           !  FUNCTIONAL FORM

           !  HI = EXP(X)
           !  X = B1 + D^2 + LOG(D) + LOG(SI) + LOG(DGLT) + LOG(H)

           !  WHERE
           !  HI = ANNUAL HEIGHT INCREMENT
           !  D = DIAMETER AT BREAST HEIGHT
           !  SI = SITE INDEX (BASE AGE 50)
           !  DGLT = 10-YEAR OUTSIDE BARK DIAMETER GROWTH
           !  H = TOTAL TREE HEIGHT

           ! CONVERT DGLT TO OUTSIDE BARK DIAMETER GROWTH
           DGLT = DG(I)

           ! SCALE DGLT TO 10 YEAR DIAMETER GROWTH
           DGLT = DGLT * 2.0
           DG10 = DGLT/BARK

           ! APPLY CONSTRAINTS IF H OF INCOMING TREE IS LESS THAN 4.5 FT
           ! HERE DG10 IS ASSUMED TO BE 0.1
           if (H .lt.4.5) DG10 = 0.1

           if (DEBUG)write(JOSTND,*)' IN HTGF - RW DEBUG',' I=',I,&
                &' ISPC=',ISPC,' D=',D,' BRAT=',BARK,' DGLT=', DGLT,&
                &' DG10=',DG10

           LTHTG = EXP(1.412947 - 0.000204*D**2 + 0.31971*LOG(D) +&
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

           if (DEBUG)write(JOSTND,*)'IN HTGF - RW/GS DEBUG',' H=',H,&
                &' HGBND=',HGBND,' LTHTG=',LTHTG

           HTG(I) = LTHTG * HGBND

           ! DEBUG
           if (DEBUG)write(JOSTND,*)'IN HTGF - RW/GS DEBUG',' D=', D,&
                &' H=',H,' SI=',SINDX,' DGLT=', DGLT,' HTG5YR=',HTG(I)

           !  BEGIN PROCESSING OF ALL OTHER SPECIES IN CA
        case default
           if (DEBUG)write(JOSTND,*)' IN HTGF, CALLING FINDAG I= ',I
           call FINDAG(I,ISPC,D,D2,H,SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,&
                &DEBUG)

           !  CHECK TO SEE IF TREE HT/DBH RATIO IS ABOVE THE MAXIMUM RATIO AT
           !  THE BEGINNING OF THE CYCLE. THIS COULD HAPPEN FOR TREES COMING
           !  OUT OF THE ESTAB MODEL.  IF IT IS, THEN CHECK TO SEE IF THE
           !  HT/NEWDBH RATIO IS ABOVE THE MAXIMUM.  IF THIS IS ALSO TRUE, LIMIT
           !  HTG TO 0.1 FOOT OR HALF THE DG, WHICH EVER IS GREATER.
           !  IF IT ISN'T, THEN LET HTG BE COMPUTED THE NORMAL
           !  WAY AND THEN CHECK IT AGAIN AT THAT POINT.

           TEMPH = H + HTG(I)
           if (H .gt.HTMAX) then
              if (H .ge. HTMAX2) then
                 HTG(I) = 0.5 * DG(I)
                 if (HTG(I).lt.0.1)HTG(I) = 0.1
                 HTG(I) = SCALE*XHT*HTG(I)*EXP(HTCON(ISPC))
                 if (DEBUG)write(JOSTND,*)' I,ISPC,H,HTMAX2,HTG,SCALE,',&
                      &'XHT,HTCON= ',I,ISPC,H,HTMAX2,HTG(I),SCALE,&
                      &XHT,HTCON(ISPC)
              end if
              go to 161
           end if

           !  NORMAL HEIGHT INCREMENT CALCULATON BASED ON TREE AGE
           !  FIRST CHECK FOR MAXIMUM TREE AGE

           if (SITAGE .ge. AGMAX) then
              POTHTG = 0.10
              if (ISPC.eq.2.or. ISPC.eq.3) then
                 POTHTG = (0.2+0.00264*SINDX)*5.
              end if
              go to 1320
           else
              AGP10 = SITAGE + 5.0
           end if

           !  CALL HTCALC FOR NORMAL CYCLING

           if (DEBUG)write(JOSTND,*)' ISPC,I,HGUESS,AGP10= ',&
                &ISPC,I,HGUESS,AGP10

           call HTCALC(SINDX,ISPC,AGP10,HGUESS,JOSTND,DEBUG)
           POTHTG = HGUESS-SITHT

           !  PATCH FOR OREGON WHITE OAK - WORK BY GOULD AND HARRINGTON, PNW
           !  USES A HT-DBH EQUATION MODIFIED BY SI AND BA, FIRST PREDICTS
           !  HEIGHT GUESS BASED ON PREVIOUS DIAMETER AND THEN PREDICT THE
           !  HEIGHT GUESS BASED ON PRESENT DIAMETER, SUBTRACT GUESSES
           !  TO CALCULATE HEIGHT GROWTH.

           if (ISPC .eq.28) then

              !  CALCULATE MAX HEIGHT BASED ON SI, THEN MODIFY BASED ON BA

              MAXGUESS = SINDX - 18.6024/ALOG(2.7 + BA)

              !  DUB HEIGHT BASED ON PRESENT DBH

              D2 = DBH(I) + DG(I)
              if (D2.lt.0.) D2 = 0.1
              HGUESS2 = 4.5 + MAXGUESS*(1-EXP(-0.137428*D2))**1.38994

              !  DUB HEIGHT BASED ON PAST DBH

              HGUESS1 = 4.5 + MAXGUESS*(1-EXP(-0.137428*D))**1.38994

              !  DIFFERENCE OF TWO DUBBED HEIGHTS IS POTENTIAL HEIGHT GROWTH

              !  SINCE DG IS ON A 10-YEAR BASIS HERE, POTHTG IS A 10-YEAR ESTIMATE.
              !  DIVIDE IT BY 2 TO GET IT ON A 5-YEAR BASIS. IT WILL BE SCALED TO
              !  THE CYCLE LENGTH BELOW

              POTHTG = (HGUESS2 - HGUESS1) / 2.0
              !      IF(DEBUG)WRITE(JOSTND,*)'ISPC,I,H,PHTG,HGS1,HGS2,MX,D,D2,BA,DG',
              !     &ISPC,I,H,POTHTG,HGUESS1,HGUESS2,MAXGUESS,D,D2,BA,DG(I)
           end if
           !--End OWO PATCH

           !  HEIGHT GROWTH MUST BE POSITIVE

           if (POTHTG .lt.0.1)POTHTG = 0.1

           ! ASSIGN A POTENTIAL HTG FOR THE ASYMPTOTIC AGE

           1320 continue

           !  HEIGHT GROWTH MODIFIERS

           RELHT = 0.0
           if (AVH .gt.0.0) RELHT = HT(I)/AVH
           if (RELHT .gt.1.5)RELHT = 1.5

           !     REVISED HEIGHT GROWTH MODIFIER APPROACH.

           !     CROWN RATIO CONTRIBUTION.  DATA AND READINGS INDICATE HEIGHT
           !     GROWTH PEAKS IN MID-RANGE OF CR, DECREASES SOMEWHAT FOR LARGE
           !     CROWN RATIOS DUE TO PHOTOSYNTHETIC ENERGY PUT INTO CROWN SUPPORT
           !     RATHER THAN HT. GROWTH.  CROWN RATIO FOR THIS COMPUTATION NEEDS
           !     TO BE IN (0-1) RANGE; DIVIDE BY 100.  FUNCTION IS HOERL'S
           !     SPECIAL FUNCTION (REF. P.23, CUTHBERT&WOOD, FITTING EQNS. TO DATA
           !     WILEY, 1971).  FUNCTION OUTPUT CONSTRAINED TO BE 1.0 OR LESS.

           HGMDCR = (CRA * (ICR(I)/100.0)**CRB) * EXP(CRC*(ICR(I)/100.0))
           if (HGMDCR .gt.1.0) HGMDCR = 1.0

           !     RELATIVE HEIGHT CONTRIBUTION.  DATA AND READINGS INDICATE HEIGHT
           !     GROWTH IS ENHANCED BY STRONG TOP LIGHT AND HINDERED BY HIGH
           !     SHADE EVEN IF SOME LIGHT FILTERS THROUGH.  ALSO RESPONSE IS
           !     GREATER FOR GIVEN LIGHT AS SHADE TOLERANCE INCREASES.  FUNCTION
           !     IS GENERALIZED CHAPMAN-RICHARDS (REF. P.2 DONNELLY ET AL. 1992.
           !     THINNING EVEN-AGED FOREST STANDS...OPTIMAL CONTROL ANALYSES.
           !     USDA FOR. SERV. RES. PAPER RM-307).
           !     PARTS OF THE GENERALIZED CHAPMAN-RICHARDS FUNCTION USED TO
           !     COMPUTE HGMDRH BELOW ARE SEGMENTED INTO FACTORS
           !     FOR PROGRAMMING CONVENIENCE.

           RHX = RELHT
           FCTRKX = ( (RHK/RHYXS(ISPC))**(RHM(ISPC)-1.0) ) - 1.0
           FCTRRB = -1.0*( RHR(ISPC)/(1.0-RHB(ISPC)) )
           FCTRXB = RHX**(1.0-RHB(ISPC)) - RHXS**(1.0-RHB(ISPC))
           FCTRM = -1.0/(RHM(ISPC)-1.0)
           if (DEBUG)&
                &write(JOSTND,*) ' HTGF-HGMDRH FACTORS = ',&
                &ISPC, RHX, FCTRKX, FCTRRB, FCTRXB, FCTRM
           HGMDRH = RHK * ( 1.0 + FCTRKX*EXP(FCTRRB*FCTRXB) ) ** FCTRM

           !     APPLY WEIGHTED MODIFIER VALUES.

           WTCR = .25
           WTRH = 1.0 - WTCR
           HTGMOD = WTCR*HGMDCR + WTRH*HGMDRH

           !    MULTIPLIED BY SCALE TO CHANGE FROM A YR. PERIOD TO FINT AND
           !    MULTIPLIED BY XHT TO APPLY USER SUPPLIED GROWTH MULTIPLIERS.

           if (DEBUG) then
              write(JOSTND,*)' IN HTGF, I= ',I,' ISPC= ',ISPC,&
                   &'HTGMOD= ',HTGMOD,' ICR= ',ICR(I),' HGMDCR= ',HGMDCR
              write(JOSTND,*)' HT(I)= ',HT(I),' AVH= ',AVH,' RELHT= ',&
                   &RELHT,' HGMDRH= ',HGMDRH
           end if

           if (HTGMOD .ge.2.0) HTGMOD = 2.0
           if (HTGMOD .le.0.0) HTGMOD = 0.1

           !     XMOD=1.0
           !      CRATIO=ICR(I)/100.0
           !      RELHT=H/AVH
           !      IF(RELHT .GT. 1.0)RELHT=1.0
           !      IF(PCCF(ITRE(I)) .LT. 100.0)RELHT=1.0
           !      XMOD = 1.117148 * (1.0-EXP(-4.26558 * CRATIO))
           !     &        *(EXP(2.54119 * (RELHT**0.250537 -1.0)))
           HTG(I) = POTHTG * HTGMOD

           if (DEBUG)write(JOSTND,901)ICR(I),PCT(I),BA,DG(I),HT(I),&
                &POTHTG,AVH,HTG(I),PCCF(ITRE(I)),ABIRTH(I),HGUESS,HTGMOD
           901 format(' HTGF',I5,13F9.2)

           TEMPH = H + HTG(I)
           if (TEMPH .gt. HTMAX2) HTG(I) = HTMAX2-H

           if (DEBUG) write(JOSTND,*)' I=',I,' TEMPH=',TEMPH,&
                &'MAP=',MAPPHD,' HTMAX2=',HTMAX2

        end select

        !  HEIGHT GROWTH GETS EVALUATED FOR EACH TREE EACH CYCLE. HTG GETS
        !  MULTIPLIED BY SCALE TO CHANGE FROM A YR  PERIOD TO FINT AND
        !  MULTIPLIED BY XHT AND HTCON TO APPLY USER SUPPLIED GROWTH MULTIPLIERS.

        if (HTG(I) .lt.0.1) HTG(I) = 0.1
        HTG(I) = SCALE*XHT*HTG(I)*EXP(HTCON(ISPC))

        if (DEBUG) write(JOSTND,*)' I=',I,&
             &' D=',DBH(I),' DG=',DG(I),' H=',H,' HTG=',HTG(I)

        161 continue

        !    APPLY DWARF MISTLETOE HEIGHT GROWTH IMPACT HERE,
        !    INSTEAD OF AT EACH FUNCTION IF SPECIAL CASES EXIST.

        HTG(I) = HTG(I)*MISHGF(I,ISPC)
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
