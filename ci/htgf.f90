!======================================!
!   Compute large tree height growth   !
!======================================!
! CI $Id$
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
  include 'CALCOM.f90'
  include 'ARRAYS.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'
  include 'OUTCOM.f90'
  include 'PLOT.f90'
  include 'MULTCM.f90'
  include 'HTCAL.f90'

  !   MODEL COEFFICIENTS AND CONSTANTS:

  !    BIAS -- THE AVERAGE RESIDUAL.

  !   HTCON -- AN ARRAY CONTAINING HABITAT TYPE CONSTANTS FOR
  !            HEIGHT GROWTH MODEL (SUBSCRIPTED BY SPECIES)

  !  HDGCOF -- COEFFICIENT FOR DIAMETER GROWTH TERMS.

  !    HGLD -- AN ARRAY, SUBSCRIPTED BY SPECIES, OF THE
  !             COEFFICIENTS FOR THE DIAMETER TERM IN THE HEIGHT
  !             GROWTH MODEL.

  !   H2COF -- COEFFICIENT FOR HEIGHT SQUARED TERMS.
  !    IND2 -- ARRAY OF POINTERS TO SMALL TREES.

  !   SCALE -- TIME FACTOR DERIVED BY DIVIDING FIXED POINT CYCLE
  !            LENGTH BY GROWTH PERIOD LENGTH FOR DATA FROM
  !            WHICH MODELS WERE DEVELOPED.

  ! Variables
  logical :: DEBUG
  integer :: ISPC,I1,I2,I3,I,ITFN,IICR,K,IXAGE
  real    :: HGLD(MAXSP),HGHC(30),HGLDD(30),HGSC(MAXSP),HGH2(30)
  real    :: HGLH,BIAS,SCALE,XHT,D,CON,HTNEW,HTI,COF1,COF2,COF3,COF4,COF5
  real    :: COF6,COF7,COF8,COF9,TEMD,Y1,Y2,FBY1,FBY2,Z,ZADJ,CLOSUR
  real    :: DIA,BRATIO,PSI,H,TEMHTG,MISHGF
  real    :: COFLM(9,3),COFAS(9,3)
  real    :: SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,D1,D2,SINDX,BAL,POTHTG,AGP10
  real    :: HGUESS,RELHT,HGMDCR,RHB,RHR,RHM,RHYXS,RHX,RHK,FCTRKX,FCTRRB
  real    :: RHXS,FCTRXB,FCTRM,HGMDRH,WTCR,WTRH,HTGMOD,HTNOW,TEMPH

  !     SPECIES LIST FOR CENTRAL IDAHO VARIANT.

  !     1 = WESTERN WHITE PINE (WP)          PINUS MONTICOLA
  !     2 = WESTERN LARCH (WL)               LARIX OCCIDENTALIS
  !     3 = DOUGLAS-FIR (DF)                 PSEUDOTSUGA MENZIESII
  !     4 = GRAND FIR (GF)                   ABIES GRANDIS
  !     5 = WESTERN HEMLOCK (WH)             TSUGA HETEROPHYLLA
  !     6 = WESTERN REDCEDAR (RC)            THUJA PLICATA
  !     7 = LODGEPOLE PINE (LP)              PINUS CONTORTA
  !     8 = ENGLEMANN SPRUCE (ES)            PICEA ENGELMANNII
  !     9 = SUBALPINE FIR (AF)               ABIES LASIOCARPA
  !    10 = PONDEROSA PINE (PP)              PINUS PONDEROSA
  !    11 = WHITEBARK PINE (WB)              PINUS ALBICAULIS
  !    12 = PACIFIC YEW (PY)                 TAXUS BREVIFOLIA
  !    13 = QUAKING ASPEN (AS)               POPULUS TREMULOIDES
  !    14 = WESTERN JUNIPER (WJ)             JUNIPERUS OCCIDENTALIS
  !    15 = CURLLEAF MOUNTAIN-MAHOGANY (MC)  CERCOCARPUS LEDIFOLIUS
  !    16 = LIMBER PINE (LM)                 PINUS FLEXILIS
  !    17 = BLACK COTTONWOOD (CW)            POPULUS BALSAMIFERA VAR. TRICHOCARPA
  !    18 = OTHER SOFTWOODS (OS)
  !    19 = OTHER HARDWOODS (OH)

  !  SURROGATE EQUATION ASSIGNMENT:

  !  FROM THE IE VARIANT:
  !      USE 17(PY) FOR 12(PY)             (IE17 IS REALLY TT2=LM)
  !      USE 18(AS) FOR 13(AS)             (IE18 IS REALLY UT6=AS)
  !      USE 13(LM) FOR 11(WB) AND 16(LM)  (IE13 IS REALLY TT2=LM)
  !      USE 19(CO) FOR 17(CW) AND 19(OH)  (IE19 IS REALLY CR38=OH)

  !  FROM THE UT VARIANT:
  !      USE 12(WJ) FOR 14(WJ)
  !      USE 20(MC) FOR 15(MC)             (UT20 = SO30=MC, WHICH IS
  !                                                  REALLY WC39=OT)

  !  DATA STATEMENTS

  data HGLD/&
       &-.04935,  -.3899,  -.4574, -.09775,  -.1555,&
       &-.1219,  -.2454,  -.5720,  -.1997,  -.5657,&
       &0.0,     0.0,     0.0,      0.,      0.,&
       &0.0,     0.0,  -.1219,     0.0/

  data (COFLM(I,1),I = 1,9)/&
       &37.0,85.0,1.77836,-0.51147,1.88795,1.20654,0.57697,&
       &3.57635,0.90283/
  data (COFLM(I,2),I = 1,9)/&
       &45.0,100.0,1.66674,0.25626,1.45477,1.11251,0.67375,&
       &2.17942,0.88103/
  data (COFLM(I,3),I = 1,9)/&
       &45.0,90.0,1.64770,0.30546,1.35015,0.94823,0.70453,&
       &2.46480,1.00316/

  data (COFAS(I,1),I = 1,9)/&
       &30.0,85.0,2.00995,0.03288,1.81059,1.28612,0.72051,&
       &3.00551,1.01433/
  data (COFAS(I,2),I = 1,9)/&
       &30.0,85.0,2.00995,0.03288,1.81059,1.28612,0.72051,&
       &3.00551,1.01433/
  data (COFAS(I,3),I = 1,9)/&
       &35.0,85.0,1.80388,-0.07682,1.70032,1.29148,0.72343,&
       &2.91519,0.95244/

  data BIAS/ .4809 /, HGLH/ 0.23315 /

  data RHK /1.0/, RHXS /0.0/

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'HTGF',4,ICYC)
  SCALE = FINT/YR
  ISMALL = 0

  !  GET THE HEIGHT GROWTH MULTIPLIERS.

  call MULTS (2,IY(ICYC),XHMULT)

  !   BEGIN SPECIES LOOP:

  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (I1.eq.0) go to 40
     I2 = ISCT(ISPC,2)
     XHT = 1.0
     XHT = XHMULT(ISPC)

     !   BEGIN TREE LOOP WITHIN SPECIES LOOP

     do I3 = I1,I2
        I = IND1(I3)
        HTG(I) = 0.
        if (PROB(I).le.0.0) then
           if (LTRIP) then
              ITFN = ITRN+2*I-1
              HTG(ITFN) = 0.
              HTG(ITFN+1) = 0.
           end if
           go to 30
        end if
        HTI = HT(I)
        D = DBH(I)

        !   HEIGHT GROWTH EQUATION, EVALUATED FOR EACH TREE EACH CYCLE

        select case (ISPC)

           !  11=WB, 12=PY, 13=AS, 16=LM, 17=CW, 19=OH

        case(11:13,16,17,19)
           IICR = INT(real(ICR(I))/10.0 + 0.5)
           if (IICR .gt.9) IICR = 9
           go to(101,101,102,102,102,102,102,103,103),IICR
           101 K = 1
           go to 110
           102 K = 2
           go to 110
           103 K = 3
           110 continue
           select case (ISPC)
           case(11,12,16)
              COF1 = COFLM(1,K)
              COF2 = COFLM(2,K)
              COF3 = COFLM(3,K)
              COF4 = COFLM(4,K)
              COF5 = COFLM(5,K)
              COF6 = COFLM(6,K)
              COF7 = COFLM(7,K)
              COF8 = COFLM(8,K)
              COF9 = COFLM(9,K)
           case(13,17,19)
              COF1 = COFAS(1,K)
              COF2 = COFAS(2,K)
              COF3 = COFAS(3,K)
              COF4 = COFAS(4,K)
              COF5 = COFAS(5,K)
              COF6 = COFAS(6,K)
              COF7 = COFAS(7,K)
              COF8 = COFAS(8,K)
              COF9 = COFAS(9,K)
           end select

           !  CHECK IF HEIGHT OR DBH EXCEED PARAMETERS

           if (HTI.le.4.5) goto 180
           if ((0.1 + COF1) .le. D) go to 180
           if ((4.5 + COF2) .le. HTI) go to 180
           go to 490
           180 continue

           !    THE SBB IS UNDEFINED IF CERTAIN INPUT VALUES EXCEED PARAMETERS IN
           !    THE FITTED DISTRIBUTION.  IN INPUT VALUES ARE EXCESSIVE THE HEIGHT
           !    GROWTH IS TAKEN TO BE ZERO.

           HTG(I) = 0.1
           go to 60
           490 continue

           ! CALCULATE ALPHA FOR THE TREE USING SCHREUDER + HAFLEY

           TEMD = D
           if (TEMD .le.0.2)TEMD = 0.2
           Y1 = (TEMD - 0.1)/COF1
           Y2 = (HTI - 4.5)/COF2
           FBY1 = ALOG(Y1/(1.0 - Y1))
           FBY2 = ALOG(Y2/(1.0 - Y2))
           Z = ( COF4 + COF6*FBY2 - COF7*( COF3 +&
                &COF5*FBY1))*(1.0 - COF7**2)**(-0.5)

           ! THE HT DIA MODEL NEEDS MODIFICATION TO CORRECT KNOWN BIAS

           select case (ISPC)
           case(13,17,19)
              ZADJ = .1 - .10273*Z + .00273*Z*Z
              if (ZADJ .lt.0.0)ZADJ = 0.0
              Z = Z+ZADJ
           end select

           ! YOUNG SMALL LODGEPOLE HTG ACCELLERATOR BASED ON TARGHEE HTG

           if (IAGE .eq.0.or. ICYC .gt.1)go to 184
           IXAGE = IAGE + IY(ICYC) -IY(1)
           if (IXAGE .lt.40..and. IXAGE .gt.10..and. D&
                &.lt.9.0) then
              if (Z .gt.2.0) go to 184
              ZADJ = .3564*DG(I)*FINT/YR
              CLOSUR = PCT(I)/100.0
              if (RELDEN .lt.100.0)CLOSUR = 1.0
              if (DEBUG)write(JOSTND,9650)ELEV,IXAGE,ZADJ,FINT,YR,&
                   &DG(I),CLOSUR
              9650 format(' ELEV',F6.1,'IXAGE',F5.0,'ZADJ',&
                   &F10.4,'FINT',F6.0,'YR',F6.0,'DG',F10.3,'CLOSUR',F10.1)
              ZADJ = ZADJ*CLOSUR

              ! ADJUSTMENT IS HIGHER FOR LONG CROWNED TREES

              if (IICR .eq.9.or. IICR .eq.8)ZADJ = ZADJ*1.1
              Z = Z + ZADJ
              if (Z .gt.2.0)Z = 2.0
           end if
           184 continue

           ! CALCULATE DIAMETER AFTER 10 YEARS

           DIA = D + DG(I)/BRATIO(ISPC,D,HTI)
           if ((0.1 + COF1) .gt. DIA) go to 185
           HTG(I) = 0.1
           go to 60
           185 continue

           !  CALCULATE HEIGHT AFTER 10 YEARS

           PSI = COF8*((DIA-0.1)/(0.1 + COF1 - DIA))**COF9&
                &* (EXP(Z*((1.0 - COF7**2  ))**0.5/COF6))

           H = ((PSI/(1.0 + PSI))* COF2) + 4.5

           if (.not. DEBUG)go to 191
           write(JOSTND,9631)D,DIA,HTI,DG(I),Z ,H
           9631 format(1X,'IN HTGF DIA=',F7.3,'DIA+10=',F7.3,'H=',F7.1,&
                &'DIA GR=',F8.3,'Z=',E15.8,'NEW H=',F8.1)
           191 continue

           !  CALCULATE HEIGHT GROWTH
           !  NEGATIVE HEIGHT GROWTH IS NOT ALLOWED

           if (H .lt. HT(I)) H = HT(I)
           HTG(I) = (H - HT(I))

           ! 14=WJ WILL GET HEIGHT ESTIMATE IN **REGENT**

        case(14)
           HTG(I) = 0.1

           ! 15=MC WILL GET HEIGHT ESTIMATE IN **REGENT**
           ! THIS IS OLD STUFF

        case(15)
           SITAGE = 0.0
           SITHT = 0.0
           AGMAX = 0.0
           HTMAX = 0.0
           HTMAX2 = 0.0
           D1 = DBH(I)
           D2 = 0.0
           if (DEBUG)write(JOSTND,*)' IN HTGF, CALLING FINDAG I= ',I
           call FINDAG(I,ISPC,D1,D2,H,SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,&
                &DEBUG)

           SINDX = SITEAR(ISPC)
           BAL = ((100.0-PCT(I))/100.0)*BA
           H = HT(I)
           D = DBH(I)
           if (H .ge. HTMAX) then
              HTG(I) = 0.1
              go to 161
           end if
           if (SITAGE .gt. AGMAX) then
              POTHTG = 0.10
              go to 1320
           else
              AGP10 = SITAGE+10.0
           end if
           HGUESS = (SINDX - 4.5) / ( 0.6192 - 5.3394/(SINDX - 4.5)&
                &+ 240.29 * AGP10**(-1.4) +(3368.9/(SINDX - 4.5))*AGP10**(-1.4))
           HGUESS = HGUESS + 4.5
           if (DEBUG)write(JOSTND,*)' SINDX,ISPC,AGP10,I,HGUESS= '
           if (DEBUG)write(JOSTND,*) SINDX,ISPC,AGP10,I,HGUESS

           POTHTG = HGUESS-SITHT

           if (DEBUG)write(JOSTND,*)' I, ISPC, AGP10, SITHT,HGUESS= ',&
                &I, ISPC, AGP10, SITHT,HGUESS

           1320 continue

           !  HEIGHT GROWTH MODIFIERS

           if (DEBUG)write(JOSTND,*) ' AT 1320 CONTINUE FOR TREE',I,' HT= ',&
                &HT(I),' AVH= ',AVH
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

           HGMDCR = (100.0*(ICR(I)/100.0)**3.0)* EXP((-5.0)*(ICR(I)/100.0))
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

           RHB = (-1.45)
           RHR = 15.0
           RHM = 1.10
           RHYXS = 0.10
           RHX = RELHT
           FCTRKX = ( (RHK/RHYXS)**(RHM-1.0) ) - 1.0
           FCTRRB = -1.0*( RHR/(1.0-RHB) )
           FCTRXB = RHX**(1.0-RHB) - RHXS**(1.0-RHB)
           FCTRM = -1.0/(RHM-1.0)

           if (DEBUG)&
                &write(JOSTND,*) ' HTGF-HGMDRH FACTORS = ',&
                &ISPC, RHX, FCTRKX, FCTRRB, FCTRXB, FCTRM

           HGMDRH = RHK * (1.0 + FCTRKX*EXP(FCTRRB*FCTRXB))**FCTRM

           !     APPLY WEIGHTED MODIFIER VALUES.

           WTCR = .25
           WTRH = 1.0 - WTCR
           HTGMOD = WTCR*HGMDCR + WTRH*HGMDRH

           if (DEBUG) then
              write(JOSTND,*)' IN HTGF, I= ',I,' ISPC= ',ISPC,'HTGMOD= ',&
                   &HTGMOD,' ICR= ',ICR(I),' HGMDCR= ',HGMDCR
              write(JOSTND,*)' HT(I)= ',HT(I),' AVH= ',AVH,' RELHT= ',RELHT,&
                   &' HGMDRH= ',HGMDRH
           end if

           if (HTGMOD .ge.2.0) HTGMOD = 2.0
           if (HTGMOD .le.0.0) HTGMOD = 0.1

           HTG(I) = POTHTG * HTGMOD

           HTNOW = HT(I)+POTHTG
           if (DEBUG)write(JOSTND,901)ICR(I),PCT(I),BA,DG(I),HT(I),&
                &POTHTG,BAL,AVH,HTG(I),DBH(I),RMAI,HGUESS
           901 format(' HTGF',I5,13F9.2)

           !    CHECK FOR HT GT MAX HT FOR THE SITE AND SPECIES

           TEMPH = H + HTG(I)
           if (TEMPH .gt. HTMAX) then
              HTG(I) = HTMAX - H
           end if
           if (HTG(I).lt.0.1)HTG(I) = 0.1

           161 continue
           if (DEBUG)write(JOSTND,*)&
                &' I,SCALE,HTG,HTMAX, H= ',I,SCALE,HTG(I),HTMAX, H

           !  ORIGINAL CI SPECIES

        case default
           CON = HTCON(ISPC)+H2COF*HTI*HTI+HGLD(ISPC)*ALOG(D)+&
                &HGLH*ALOG(HTI)
           HTG(I) = EXP(CON+HDGCOF*ALOG(DG(I)))+BIAS
           if (HTG(I).lt.0.1)HTG(I) = 0.1

        end select

        60 continue

        !    MULTIPLIED BY SCALE TO CHANGE FROM A YR. PERIOD TO FINT AND
        !    MULTIPLIED BY XHT TO APPLY USER SUPPLIED GROWTH MULTIPLIERS (HTGMULT).
        !    MULTIPLIED BY EXP(HTCON()) FOR SPECIES WHERE HCOR2 MULTIPLIER
        !    HAS NOT ALREADY BEEN ACCOUNTED FOR (READCORH).

        select case (ISPC)
        case(11:17,19)
           HTG(I) = HTG(I)*SCALE*XHT*EXP(HTCON(ISPC))
        case default
           HTG(I) = HTG(I)*SCALE*XHT
        end select

        !    APPLY DWARF MISTLETOE HEIGHT GROWTH IMPACT HERE,
        !    INSTEAD OF AT EACH FUNCTION IF SPECIAL CASES EXIST.

        HTG(I) = HTG(I)*MISHGF(I,ISPC)

        TEMHTG = HTG(I)

        if (DEBUG) then
           HTNEW = HT(I)+HTG(I)
           write (JOSTND,9000) HTG(I),CON,HTCON(ISPC),H2COF,D,&
                &WK1(I),HGLH,HTNEW,HDGCOF,I,ISPC
           9000 format(' 9000 HTGF, HTG=',F8.4,' CON=',F8.4,' HTCON=',F8.4,&
                &' H2COF=',F12.8,' D*BARK=',F8.4/' WK1=',F8.4,' HGLH=',F8.4,&
                &' HTNEW=',F8.4,' HDGCOF=',F8.4,' I=',I4,' ISPC=',I2)
        end if

        ! CHECK FOR SIZE CAP COMPLIANCE.

        if ((HT(I)+HTG(I)).gt.SIZCAP(ISPC,4)) then
           HTG(I) = SIZCAP(ISPC,4)-HT(I)
           if (HTG(I) .lt.0.1) HTG(I) = 0.1
        end if

        if (.not.LTRIP) go to 30
        ITFN = ITRN+2*I-1
        select case (ISPC)
        case(11:17,19)
           HTG(ITFN) = TEMHTG
        case default
           HTG(ITFN) = EXP(CON+HDGCOF*ALOG(DG(ITFN)))+BIAS
           if (HTG(ITFN).lt.0.1)HTG(ITFN) = 0.1
           HTG(ITFN) = HTG(ITFN)*SCALE*XHT
        end select

        ! CHECK FOR SIZE CAP COMPLIANCE.

        if ((HT(ITFN)+HTG(ITFN)).gt.SIZCAP(ISPC,4)) then
           HTG(ITFN) = SIZCAP(ISPC,4)-HT(ITFN)
           if (HTG(ITFN) .lt.0.1) HTG(ITFN) = 0.1
        end if

        select case (ISPC)
        case(11:17,19)
           HTG(ITFN+1) = TEMHTG
        case default
           HTG(ITFN+1) = EXP(CON+HDGCOF*ALOG(DG(ITFN+1)))+BIAS
           if (HTG(ITFN+1).lt.0.1)HTG(ITFN+1) = 0.1
           HTG(ITFN+1) = HTG(ITFN+1)*SCALE*XHT
        end select

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
  return

  entry HTCONS

  !  ENTRY POINT FOR LOADING HEIGHT INCREMENT MODEL COEFFICIENTS THAT
  !  ARE SITE DEPENDENT AND REQUIRE ONE-TIME RESOLUTION.  HGHC
  !  CONTAINS HABITAT TYPE INTERCEPTS, HGLDD CONTAINS HABITAT
  !  DEPENDENT COEFFICIENTS FOR THE DIAMETER INCREMENT TERM, HGH2
  !  CONTAINS HABITAT DEPENDENT COEFFICIENTS FOR THE HEIGHT-SQUARED
  !  TERM, AND HGHC CONTAINS SPECIES DEPENDENT INTERCEPTS.  HABITAT
  !  TYPE IS INDEXED BY ITYPE (SEE /PLOT/ COMMON AREA).

  data  HGHC/&
       &2*2.03035,7*1.72222,2*1.19728,1.81759,2.14781,1.76998,4*2.21104,&
       &2*1.81759,2.03035,2*1.81759,3*1.74090,4*2.03035/
  data  HGLDD/&
       &2*0.62144,7*1.02372,2*0.85493,0.75756,0.46238,0.49643,4*0.37042,&
       &2*0.75756,0.62144,2*0.75756,3*0.34003,4*0.62144/
  data  HGH2/&
       &2*-13.358E-05,7*-3.809E-05,2*-3.715E-05,-2.607E-05,-5.200E-05,&
       &-1.605E-05,4*-3.631E-05,2*-2.607E-05,-13.358E-05,2*-2.607E-05,&
       &3*-4.460E-05,4*-13.358E-05/
  data  HGSC/&
       &-.5342,  .1433,  .1641, -.6458, -.6959,&
       &-.9941, -.6004,  .2089, -.5478,  .7316,&
       &0.0,    0.0,    0.0,    0.0,    0.0,&
       &0.0,    0.0, -.9941,    0.0/

  !  ASSIGN COEFFICIENTS THAT ARE INDEPENDENT OF SPECIES.

  HGHCH = HGHC(ITYPE)
  H2COF = HGH2(ITYPE)
  HDGCOF = HGLDD(ITYPE)

  !  LOAD OVERALL INTERCEPT FOR EACH SPECIES.

  do ISPC = 1,MAXSP

     select case (ISPC)

     case(11:17,19)
        HTCON(ISPC) = 0.0

     case default
        HTCON(ISPC) = HGHCH+HGSC(ISPC)

     end select

     if (LHCOR2.and. HCOR2(ISPC).gt.0.0) HTCON(ISPC) = &
          &HTCON(ISPC)+ALOG(HCOR2(ISPC))
     50 continue
  end do

  return
end
