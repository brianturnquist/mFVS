!======================================!
!   Compute large tree height growth   !
!======================================!
! TT $Id$
subroutine HTGF
  implicit none

  !   THIS SUBROUTINE COMPUTES THE PREDICTED PERIODIC HEIGHT
  !   INCREMENT FOR EACH CYCLE AND LOADS IT INTO THE ARRAY HTG.
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
  include 'GGCOM.f90'

  !   MODEL COEFFICIENTS AND CONSTANTS:

  !    BIAS -- THE AVERAGE RESIDUAL.

  !   HTCON -- AN ARRAY CONTAINING HABITAT TYPE CONSTANTS FOR
  !            HEIGHT GROWTH MODEL (SUBSCRIPTED BY SPECIES)

  !  HDGCOF -- COEFFICIENT FOR DIAMETER GROWTH TERMS.

  !   H2COF -- COEFFICIENT FOR HEIGHT SQUARED TERMS.
  !    IND2 -- ARRAY OF POINTERS TO SMALL TREES.

  !   SCALE -- TIME FACTOR DERIVED BY DIVIDING FIXED POINT CYCLE
  !            LENGTH BY GROWTH PERIOD LENGTH FOR DATA FROM
  !            WHICH MODELS WERE DEVELOPED.

  external RANN
  ! Variables
  logical :: DEBUG
  integer :: ISPC,I1,I2,I3,I,IICR,KEYCR,K,IXAGE,ITFN,JSPC
  integer :: LSIGRP,LSIMAP,L,ICLS
  real    :: AZBIAS(MAXSP),BZBIAS(MAXSP),ZZRAN,HHU2,HHU1,HHE2,HHE1
  real    :: D,D1,D2,H,SITHT,SITAGE,AGMAX,HTMAX,HTMAX2
  real    :: PCCFI,DGI,AP,TSITE,AGETEM,RATIO,BAUTBA,BARK,HNOW,XWT
  real    :: COF11,COF10,COF9,COF8,COF7,COF6,COF5,COF4,COF3,COF2,COF1,COF
  real    :: XI1,XI2,SCALE,XSITE,Y1,Y2,FBY1,FBY2,Z,ZBIAS,ZTEST,ZADJ,BATEM
  real    :: CLOSUR,DIA,BRATIO,PSI,TEMHTG,ADJUST,AGEFUT,DFUT,HGE,HGU
  real    :: SINDX,BAL,POTHTG,AGP10,HGUESS,RELHT,CRC,CRB,CRA
  real    :: HGMDCR,RHX,RHK,FCTRKX,FCTRRB,RHB,RHXS,FCTRXB,FCTRM,HGMDRH
  real    :: WTCR,WTRH,HTGMOD,HTNOW,TEMPH,BACHLO,RHM,RHYXS,RHR,HTI,CON
  real    :: MISHGF

  ! PROGRAM TO CALCULATE HT GROWTH USING THE SBB DIST'N

  dimension COF(9,33),COF1(9,3),COF2(9,3),COF3(9,3),&
       &COF4(9,3),COF5(9,3),COF6(9,3),COF7(9,3),&
       &COF8(9,3),COF9(9,3),COF10(9,3),COF11(9,3)
  equivalence (COF(1,1),COF1(1,1)),(COF(1,4),COF2(1,1)),&
       &(COF(1,7),COF3(1,1)),(COF(1,10),COF4(1,1)),&
       &(COF(1,13),COF5(1,1)),(COF(1,16),COF6(1,1)),&
       &(COF(1,19),COF7(1,1)),(COF(1,22),COF8(1,1)),(COF(1,25),COF9(1,1))&
       &,(COF(1,28),COF10(1,1)),(COF(1,31),COF11(1,1))

  ! SPECIES ORDER FOR TETONS VARIANT:

  !  1=WB,  2=LM,  3=DF,  4=PM,  5=BS,  6=AS,  7=LP,  8=ES,  9=AF, 10=PP,
  ! 11=UJ, 12=RM, 13=BI, 14=MM, 15=NC, 16=MC, 17=OS, 18=OH

  ! VARIANT EXPANSION:
  ! BS USES ES EQUATIONS FROM TT
  ! PM USES PI (COMMON PINYON) EQUATIONS FROM UT
  ! PP USES PP EQUATIONS FROM CI
  ! UJ AND RM USE WJ (WESTERN JUNIPER) EQUATIONS FROM UT
  ! BI USES BM (BIGLEAF MAPLE) EQUATIONS FROM SO
  ! MM USES MM EQUATIONS FROM IE
  ! NC AND OH USE NC (NARROWLEAF COTTONWOOD) EQUATIONS FROM CR
  ! MC USES MC (CURL-LEAF MTN-MAHOGANY) EQUATIONS FROM SO
  ! OS USES OT (OTHER SP.) EQUATIONS FROM TT

  !    NINE COEFFICIENTS FOR JOHNSON'S SBB DISTRIBUTION FOR
  !    11 SPECIES, AND 3 CROWN RATIO GROUPS.

  !  DOUGLAS-FIR

  data COF3/&
       &60.0,105.0,2.43099,0.20403,1.28447,0.99886,0.79629,&
       &5.66171,1.02398,&
       &70.0,120.0,1.85710,-0.10692,1.40067,1.16053,0.78576,&
       &3.85554,0.94853,&
       &70.0,130.0,1.51547,0.30923,1.30655,1.23707,0.86427,2.24521,&
       &0.91281/

  !  WHITEBARK PINE

  data COF1/&
       &37.0,85.0,1.77836,-0.51147,1.88795,1.20654,0.57697,&
       &3.57635,0.90283,&
       &45.0,100.0,1.66674,0.25626,1.45477,1.11251,0.67375,&
       &2.17942,0.88103,&
       &45.0,90.0,1.64770,0.30546,1.35015,0.94823,0.70453,&
       &2.46480,1.00316/

  !  LIMBER PINE

  data COF2/&
       &37.0,85.0,1.77836,-0.51147,1.88795,1.20654,0.57697,&
       &3.57635,0.90283,&
       &45.0,100.0,1.66674,0.25626,1.45477,1.11251,0.67375,&
       &2.17942,0.88103,&
       &45.0,90.0,1.64770,0.30546,1.35015,0.94823,0.70453,&
       &2.46480,1.00316/

  !  NOT USED -- LOAD DOUGLAS-FIR COEFFICIENTS

  data COF4/&
       &60.0,105.0,2.43099,0.20403,1.28447,0.99886,0.79629,&
       &5.66171,1.02398,&
       &70.0,120.0,1.85710,-0.10692,1.40067,1.16053,0.78576,&
       &3.85554,0.94853,&
       &70.0,130.0,1.51547,0.30923,1.30655,1.23707,0.86427,2.24521,&
       &0.91281/

  !  BLUE SPRUCE -- LOAD ENGELMANN SPRUCE COEFFICIENTS

  data COF5/&
       &50.0,145.0,1.23692,0.30499,1.19486,1.09838,0.90058,&
       &2.08863,0.97969,&
       &50.0,145.0,1.23692,0.30499,1.19486,1.09838,0.90058,&
       &2.08863,0.97969,&
       &50.0,140.0,0.94647,0.31838,1.04318,0.95444,0.91934,&
       &1.78262,1.00481/

  !  QUAKING ASPEN

  data COF6/&
       &30.0,85.0,2.00995,0.03288,1.81059,1.28612,0.72051,&
       &3.00551,1.01433,&
       &30.0,85.0,2.00995,0.03288,1.81059,1.28612,0.72051,&
       &3.00551,1.01433,&
       &35.0,85.0,1.80388,-0.07682,1.70032,1.29148,0.72343,&
       &2.91519,0.95244/

  ! LODGEPOLE PINE

  data COF7/&
       &30.0,105.0,2.00207,-0.25204,2.04453,1.62734,0.72514,&
       &2.84910,0.91104,&
       &45.0,110.0,2.50885,0.09740,1.85457,1.48205,0.77851,&
       &3.49791,0.97420,&
       &35.0,90.0,1.31478,0.21254,1.29774,1.09363,0.85692,&
       &2.30681,1.01686/

  ! ENGELMANN SPRUCE

  data COF8/&
       &50.0,145.0,1.23692,0.30499,1.19486,1.09838,0.90058,&
       &2.08863,0.97969,&
       &50.0,145.0,1.23692,0.30499,1.19486,1.09838,0.90058,&
       &2.08863,0.97969,&
       &50.0,140.0,0.94647,0.31838,1.04318,0.95444,0.91934,&
       &1.78262,1.00481/

  !  SUBALPINE FIR

  data COF9/&
       &20.0,95.0,0.90779,0.33845,1.06402,0.81823,0.97688,&
       &1.95458,1.27034,&
       &35.0,110.0,1.36713,0.35062,1.25426,1.05571,0.90342,&
       &2.31128,1.07333,&
       &50.0,130.0,1.63172,0.60577,1.29877,1.16988,0.90860,&
       &2.11592,1.00870/

  !  NOT USED -- LOAD LIMBER PINE COEFFICIENTS

  data COF10/&
       &37.0,85.0,1.77836,-0.51147,1.88795,1.20654,0.57697,&
       &3.57635,0.90283,&
       &45.0,100.0,1.66674,0.25626,1.45477,1.11251,0.67375,&
       &2.17942,0.88103,&
       &45.0,90.0,1.64770,0.30546,1.35015,0.94823,0.70453,&
       &2.46480,1.00316/

  !  OTHER SOFTWOODS -- USE WHITEBARK/LIMBER PINE

  data COF11/&
       &37.0,85.0,1.77836,-0.51147,1.88795,1.20654,0.57697,&
       &3.57635,0.90283,&
       &45.0,100.0,1.66674,0.25626,1.45477,1.11251,0.67375,&
       &2.17942,0.88103,&
       &45.0,90.0,1.64770,0.30546,1.35015,0.94823,0.70453,&
       &2.46480,1.00316/

  data AZBIAS/&
       &0.0,     0.0, -.86001,     0.0, -.84735,&
       &0.0,  .40153, -.84735,  .89035,     0.0,&
       &0.0,     0.0,     0.0,     0.0,     0.0,&
       &0.0,     0.0,     0.0/
  data BZBIAS/&
       &0.0,     0.0,  .01051,     0.0,  .01102,&
       &0.0,  -.0078,  .01102, -.01331,     0.0,&
       &0.0,     0.0,     0.0,      0.,     0.0,&
       &0.0,     0.0,     0.0/

  !  COEFFICIENTS--CROWN RATIO (CR) BASED HT. GRTH. MODIFIER

  data CRA /100.0/, CRB /3.0/, CRC /-5.0/

  !  COEFFICIENTS--RELATIVE HEIGHT (RH) BASED HT. GRTH. MODIFIER

  data RHK /1.0/, RHXS /0.0/

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'HTGF',4,ICYC)
  if (DEBUG)write(JOSTND,*) 'IN HTGF AT BEGINNING, HTCON=',&
       &HTCON,' RMAI=',RMAI,' ELEV=',ELEV

  XI2 = 4.5
  XI1 = 0.1
  SCALE = FINT/YR
  ISMALL = 0

  !  GET THE HEIGHT GROWTH MULTIPLIERS.

  call MULTS (2,IY(ICYC),XHMULT)
  if (DEBUG)write(JOSTND,*)' HTGF MULT= ',XHMULT,'  SCALE= ',SCALE

  !  PUT SITE INDEX FOR THE SITE SPECIES INTO A SITE GROUP

  XSITE = SITEAR(ISISP)
  LSIGRP = INT(XSITE/10.0 - .5)
  if (LSIGRP .lt.2) LSIGRP = 2
  if (LSIGRP .gt.6) LSIGRP = 6
  LSIMAP = LSIGRP-1

  !   BEGIN SPECIES LOOP:

  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (I1.eq.0) go to 40
     I2 = ISCT(ISPC,2)

     !   BEGIN TREE LOOP WITHIN SPECIES LOOP

     do I3 = I1,I2
        I = IND1(I3)
        HTG(I) = 0.
        BARK = BRATIO(ISPC,DBH(I),HT(I))
        if (PROB(I) .le.0.0 ) go to 201

        !   BYPASS CALCULATION IF DIAMETER IS LESS THAN LOWER LIMIT
        !   SAVE POINTERS TO SMALL TREES

        select case (ISPC)
        case (14)
           if (DBH(I).lt.1.0) then
              ISMALL = ISMALL + 1
              IND2(ISMALL) = I
              go to 201
           end if
        case(15,18)
           if (DBH(I).lt.0.5.or. HT(I).le.4.5) then
              ISMALL = ISMALL + 1
              IND2(ISMALL) = I
              go to 201
           end if
        case default
           if (DBH(I).lt.1.5) then
              ISMALL = ISMALL + 1
              IND2(ISMALL) = I
              go to 201
           end if
        end select

        select case (ISPC)

           !  PP FROM THE CI VARIANT

        case(10)
           HTI = HT(I)
           D = DBH(I)
           CON = 2.03035 + 0.7316 - 0.00013358*HTI*HTI - 0.5657*ALOG(D) +&
                &0.23315*ALOG(HTI)
           HTG(I) = EXP(CON + 0.62144*ALOG(DG(I))) + 0.4809
           if (HTG(I).lt.0.1)HTG(I) = 0.1

           !  PM, UJ, AND RM
           !  WILL GET HEIGHT ESTIMATE IN **REGENT**

        case(4,11,12)
           HTG(I) = 0.

           !  SPECIES WITH SURROGATE EQUATIONS FROM THE CR VARIANT, VIA UT
           !  USES SPRUCE-FIR MODEL TYPE RELATIONSHIPS
           !  ADJUST FOR NC WAS 1.05 IN CR VARIANT, BUT IS A FUNCTION
           !  OF SITE INDEX IN THIS VARIANT SO OPEN GROWN TREES HIT THE SITE
           !  HEIGHT
           !  NC, OH

        case(15,18)
           ADJUST = 0.78 + 0.0023*SITEAR(ISPC)
           D = DBH(I)
           ICLS = IFIX(D + 1.0)
           if (ICLS .gt.41) ICLS = 41
           BARK = BRATIO(ISPC,D,HT(I))
           BAUTBA = BAU(ICLS)/BA
           HNOW = HT(I)
           HHE1 = 0.0
           HHE2 = 0.0
           HHU1 = 0.0
           HHU2 = 0.0
           XWT = 0.0
           PCCFI = PCCF(ITRE(I))
           DGI = DG(I)
           AP = ABIRTH(I)

           !  ADJUST AP FOR BREAST HIGH AGE CURVES IF NECESSARY

           TSITE = SITEAR(ISPC)
           if (TSITE.lt.20.)TSITE = 20.
           AP = AP-(4.5/(-0.22+0.0155*TSITE))
           if (AP.lt.1.)AP = 1.

           !  USE SITE INDEX CURVES AS BASIS FOR HEIGHT IN EVEN-AGED STANDS
           !  SITE CURVE BREAST HEIGHT AGE, BASE AGE 100 ENG SPRUCE/SALPINE FIR
           !  ALEXANDER 1967.  RES PAPER RM32

           TSITE = SITEAR(ISPC)
           AGETEM = AP
           if (AGETEM .lt.30.0) AGETEM = 30.0
           HHE1 = (2.75780*TSITE**0.83312) * ((1.0 - EXP(-0.015701*AGETEM))&
                &**(22.71944*TSITE**(-0.63557))) + 4.5
           if (AP .lt. AGETEM) HHE1 = ((HHE1 - 4.5) / AGETEM) * AP+4.5
           RATIO = 1.0 - BAUTBA
           if (RATIO .lt.0.728) RATIO = 0.728
           HHE1 = HHE1 * RATIO

           BATEM = BA
           if (BATEM .lt.10.) BATEM = 10.
           HHU1 = (-2.04+1.4534*TSITE) * ((1.0-EXP(-0.058112*D))&
                &** (1.894400 * BATEM ** (-0.192979))) + 4.5

           if (DEBUG)write(JOSTND,*)' IN HTGF 1ST CALL TO GEMHT= ',&
                &HHE1,HHU1,TSITE,D,AP,AGETEM,BAUTBA,RATIO

           AGEFUT = AP + 10.0
           AGETEM = AGEFUT
           if (AGETEM .lt.30.0) AGETEM = 30.0
           DFUT = D + DGI/BARK
           HHE2 = (2.75780*TSITE**0.83312) * ((1.0 - EXP(-0.015701*AGETEM))&
                &**(22.71944*TSITE**(-0.63557))) + 4.5
           if (AGEFUT .lt. AGETEM) HHE2 = ((HHE2 - 4.5) / AGETEM) * AGEFUT+4.5
           RATIO = 1.0 - BAUTBA
           if (RATIO .lt.0.728) RATIO = 0.728
           HHE2 = HHE2 * RATIO

           BATEM = BA
           if (BATEM .lt.10.) BATEM = 10.
           HHU2 = (-2.04+1.4534*TSITE) * ((1.0-EXP(-0.058112*DFUT))&
                &** (1.894400 * BATEM ** (-0.192979))) + 4.5

           if (DEBUG)write(JOSTND,*)' IN HTGF 2ND CALL TO GEMHT= ',&
                &HHE2,HHU2,TSITE,DFUT,AGEFUT,AGETEM,BAUTBA,RATIO

           ! IF EVEN-AGED,UNEVEN-AGED AND BA .LT. 70, OR UNEVEN-AGED AND PCT(I)
           ! IS .GE.40 (OVERSTORY) THEN HTG(I) IS EVEN-AGED HT GROWTH ESTIMATE.

           HTG(I) = (HHE2 - HHE1)*ADJUST

           ! IF STAND IS UNEVEN-AGED AND BA IS .GE. TO 70 THEN USE A BLEND.
           ! TREES WITH PCT() .LE. 10 (OVERTOPPED TREES) GET UNEVEN-AGED
           ! HEIGHT GROWTH; TREES WITH PCT() .GT. 10 AND .LT. 40 GET A
           ! WEIGHTED AVERAGE OF EVEN/UNEVEN-AGED HEIGHT GROWTH.

           if (AGERNG .gt.40.0.and. BA .ge.70.0) then
              if (PCT(I) .le.10.)HTG(I) = HHU2 - HHU1
              if ((PCT(I) .gt.10.0) .and. (PCT(I) .lt.40.0)) then
                 HGE = (HHE2 - HHE1)*ADJUST
                 HGU = HHU2 - HHU1
                 XWT = ((PCT(I)-10.)*(10./3.))/100.
                 HTG(I) = XWT*HGE + (1.0-XWT)*HGU
              end if
           end if

           !     ADD RANDOM INCREMENT TO HTG.  GETS AWAY FROM ALL TREES HAVING
           !     THE SAME INCREMENT (ESP UNDER EDMINSTERS EVEN-AGED LOGIC).

           189 continue
           ZZRAN = 0.0
           if (DGSD .gt.0.0) then
              ZZRAN = BACHLO(0.0,1.0,RANN)
              if (ZZRAN .gt. DGSD .or. ZZRAN .lt. (-DGSD)) go to 189
              if (DEBUG)write(JOSTND,9984) I,HTG(I),ZZRAN,SCALE
              9984 format(1H ,'IN HTGF 9984 FORMAT',I5,2X,3(F10.4,2X))
           end if
           HTG(I) = HTG(I) + ZZRAN*0.1

           !  IF STAGNATION EFFECT IS ON FOR THIS SPECIES,
           !  ONLY REDUCE HT GROWTH BY HALF OF DSTAG, NOT DSTAG. DIXON 3-9-93

           if (ISTAGF(ISPC).ne.0)HTG(I) = HTG(I)*(DSTAG+1.0)*.5
           if (HTG(I) .lt.0.1) HTG(I) = 0.1
           if (DEBUG)&
                &write(JOSTND,*)' HTGF I,ISPC,XWT,HHE1,HHE2,HHU1,HHU2,HT,HTG = ',&
                &I,ISPC,XWT,HHE1,HHE2,HHU1,HHU2,HT(I),HTG(I)

           !  SPECIES WITH SURROGATE EQUATIONS FROM THE SO VARIANT
           !  MC, BI
           !  THESE SPECIES GET THEIR HEIGHT GROWTH FROM **REGENT**. CODE HERE
           !  IS FROM THE SOURCE VARIANT, BUT ESTIMATE IS NOT USED.

        case(13,16)
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
              HTG(I) = SCALE*HTG(I)*XHMULT(ISPC)*EXP(HTCON(ISPC))
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

           select case (ISPC)
           case(16)
              RHB = (-1.45)
              RHR = 15.0
              RHM = 1.10
              RHYXS = 0.10
           case(13)
              RHB = (-1.10)
              RHR = 20.0
              RHM = 1.10
              RHYXS = 0.20
           end select
           RHX = RELHT
           FCTRKX = ( (RHK/RHYXS)**(RHM-1.0) ) - 1.0
           FCTRRB = -1.0*( RHR/(1.0-RHB) )
           FCTRXB = RHX**(1.0-RHB) - RHXS**(1.0-RHB)
           FCTRM = -1.0/(RHM-1.0)

           if (DEBUG)&
                &write(JOSTND,*) ' HTGF-HGMDRH FACTORS = ',&
                &ISPC, RHX, FCTRKX, FCTRRB, FCTRXB, FCTRM

           HGMDRH = RHK * ( 1.0 + FCTRKX*EXP(FCTRRB*FCTRXB) ) ** FCTRM

           !     APPLY WEIGHTED MODIFIER VALUES.

           WTCR = .25
           WTRH = 1.0 - WTCR
           HTGMOD = WTCR*HGMDCR + WTRH*HGMDRH

           !    MULTIPLIED BY SCALE TO CHANGE FROM A YR. PERIOD TO FINT AND
           !    MULTIPLIED BY XHMULT TO APPLY USER SUPPLIED GROWTH MULTIPLIERS.

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

           !    HEIGHT GROWTH EQUATION, EVALUATED FOR EACH TREE EACH CYCLE
           !    MULTIPLIED BY SCALE TO CHANGE FROM A YR. PERIOD TO FINT AND
           !    MULTIPLIED BY XHMULT TO APPLY USER SUPPLIED GROWTH MULTIPLIERS.
           !    CHECK FOR HT GT MAX HT FOR THE SITE AND SPECIES

           TEMPH = H + HTG(I)
           if (TEMPH .gt. HTMAX) then
              HTG(I) = HTMAX - H
           end if
           if (HTG(I).lt.0.1)HTG(I) = 0.1

           161 continue
           if (DEBUG)write(JOSTND,*)&
                &' I,SCALE,HTG,HTMAX, H= ',I,SCALE,HTG(I),HTMAX, H

           !  ORIGINAL TETONS SPECIES (1-3,6-9,17)
           !  BLUE SPRUCE (5) AND ROCKY MTN MAPLE (14)

        case default

           !    CHANGE THE CROWN RATIO TO AN INTEGER BETWEEN 0 AND 10

           IICR = INT(real(ICR(I))/10.0 + 0.5)

           !    PLACE THE CROWN RATIO INTO ONE OF THREE GROUPS

           if (IICR .gt.9) IICR = 9
           go to(101,101,102,102,102,102,102,103,103),IICR
           101 KEYCR = 1
           go to 110
           102 KEYCR = 2
           go to 110
           103 KEYCR = 3
           110 continue

           ! CALCULATE ROW IDENT FOR SBB COEF LOOKUP

           if (ISPC .le.10) then
              JSPC = ISPC
           else if (ISPC .eq.14) then
              JSPC = 6
           else
              JSPC = 11
           end if
           K = (JSPC- 1)*3  + KEYCR
           if (DEBUG)write(JOSTND,9101)(COF(L,K),L = 1,9)
           9101 format(' COFS= ',9F10.4)

           !  CHECK IF HEIGHT OR DBH EXCEED PARAMETERS

           if (HT(I).le.4.5) goto 180
           if ((XI1 + COF(1,K)) .le. DBH(I)) go to 180
           if ((XI2 + COF(2,K)) .le. HT(I)) go to 180

           !  TRAP TO AVOID LOG(0) ERRORS WITH XI1 AND XI2; HTG WILL COME FROM
           !  REGENT FOR THESE SMALL OF TREES ANYWAY.  GED 05/07/09

           if (DBH(I) .le.0.1.or. HT(I).le.4.5)go to 180

           go to 190
           180 continue

           !    THE SBB IS UNDEFINED IF CERTAIN INPUT VALUES EXCEED PARAMETERS IN
           !    THE FITTED DISTRIBUTION.  IN INPUT VALUES ARE EXCESSIVE THE HEIGHT
           !    GROWTH IS TAKEN TO BE 0.1 FOOT.

           HTG(I) = 0.1
           go to 200
           190 continue

           ! CALCULATE ALPHA FOR THE TREE USING SCHREUDER + HAFLEY

           Y1 = (DBH(I) - XI1)/COF(1,K)
           Y2 = (HT(I) - XI2)/COF(2,K)
           FBY1 = ALOG(Y1/(1.0 - Y1))
           FBY2 = ALOG(Y2/(1.0 - Y2))
           Z = ( COF(4,K) + COF(6,K)*FBY2 - COF(7,K)*( COF(3,K) +&
                &COF(5,K)*FBY1))*(1.0 - COF(7,K)**2)**(-0.5)

           ! THE HT DIA MODEL NEEDS MODIFICATION TO CORRECT KNOWN BIAS

           ZBIAS = AZBIAS(ISPC)+BZBIAS(ISPC)*ELEV
           if (ISPC.eq.6.or. ISPC.eq.14)&
                &ZBIAS = AZBIAS(ISPC)+BZBIAS(ISPC)*(ELEV-20.)
           if (ELEV .lt.55..or. ELEV .gt.80.0)ZBIAS = 0.0
           ZTEST = Z-ZBIAS
           if (ZTEST .ge.2.0.and. ZBIAS .lt.0.0)ZBIAS = 0.0
           Z = Z-ZBIAS
           if (ISPC.eq.6.or. ISPC.eq.14) then
              ZADJ = .1 - .10273*Z + .00273*Z*Z
              if (ZADJ .lt.0.0)ZADJ = 0.0
              Z = Z+ZADJ
           end if

           ! YOUNG SMALL LODGEPOLE HTG ACCELLERATOR BASED ON TARGHEE HTG
           ! TEMP BYPASS

           if ((ICYC .gt.1) .or. (IAGE .le.0))go to 184
           IXAGE = IAGE + IY(ICYC) -IY(1)

           if (DEBUG)&
                &write(JOSTND,*)' I, ISPC, IXAGE= ',I, ISPC, IXAGE

           if (IXAGE .lt.40..and. IXAGE .gt.10..and. DBH(I)&
                &.lt.9.0) then
              if (Z .gt.2.0) go to 184
              ZADJ = .3564*DG(I)*FINT/YR
              CLOSUR = PCT(I)/100.0
              if (RELDEN .lt.100.0)CLOSUR = 1.0
              if (DEBUG)write(JOSTND,9650)ZBIAS,ELEV,IXAGE,ZADJ,FINT,YR,&
                   &DG(I),CLOSUR
              9650 format(' ZBIAS',F8.0,'ELEV',F6.1,'AGE',F5.0,'ZADJ',&
                   &F10.4,'FINT',F6.0,'YR',F6.0,'DG',F10.3,'CLOSUR',F10.1)
              ZADJ = ZADJ*CLOSUR

              ! ADJUSTMENT IS HIGHER FOR LONG CROWNED TREES

              if (IICR .eq.9.or. IICR .eq.8)ZADJ = ZADJ*1.1
              Z = Z + ZADJ
              if (Z .gt.2.0)Z = 2.0
           end if
           184 continue

           ! CALCULATE DIAMETER AFTER 10 YEARS

           DIA = DBH(I) + DG(I)/BARK
           if ((XI1 + COF(1,K)) .gt. DIA) go to 185
           HTG(I) = 0.1
           go to 200
           185 continue

           !  CALCULATE HEIGHT AFTER 10 YEARS

           PSI = COF(8,K)*((DIA-XI1)/(XI1 + COF(1,K) - DIA))**COF(9,K)&
                &* (EXP(Z*((1.0 - COF(7,K)**2  ))**0.5/COF(6,K)))

           H = ((PSI/(1.0 + PSI))* COF(2,K)) + XI2

           if (.not. DEBUG)go to 191
           write(JOSTND,9631)DBH(I),DIA,HT(I),DG(I),Z ,H
           9631 format(1X,'IN HTGF DIA=',F7.3,'DIA+10=',F7.3,'H=',F7.1,&
                &'DIA GR=',F8.3,'Z=',E15.8,'NEW H=',F8.1)
           191 continue

           !  CALCULATE HEIGHT GROWTH
           !   NEGATIVE HEIGHT GROWTH IS NOT ALLOWED

           if (H .lt. HT(I)) H = HT(I)
           HTG(I) = H - HT(I)
           if (HTG(I).lt.0.1)HTG(I) = 0.1

           200 continue

        end select

        201 continue
        HTG(I) = HTG(I)*SCALE*XHMULT(ISPC)*EXP(HTCON(ISPC))

        !    APPLY DWARF MISTLETOE HEIGHT GROWTH IMPACT HERE,
        !    INSTEAD OF AT EACH FUNCTION IF SPECIAL CASES EXIST.

        HTG(I) = HTG(I)*MISHGF(I,ISPC)
        TEMHTG = HTG(I)

        ! CHECK FOR SIZE CAP COMPLIANCE.

        if ((HT(I)+HTG(I)).gt.SIZCAP(ISPC,4)) then
           HTG(I) = SIZCAP(ISPC,4)-HT(I)
           if (HTG(I) .lt.0.1) HTG(I) = 0.1
        end if

        if (.not.LTRIP)go to 30
        ITFN = ITRN + 2*I - 1
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

        if (DEBUG)write(JOSTND,9001)HTG(ITFN),HTG(ITFN+1)
        9001 format('  LOWER HTG = ',F8.4,'  UPPER HTG = ',F8.4)

        !   END OF TREE LOOP

        30 continue
     end do

     !   END OF SPECIES LOOP

     40 continue
  end do
  return

  entry HTCONS

  !  LOAD HTCON.  IF CORRECTION TERMS ARE TO BE USED, MODIFY ACCORDINGLY

  do ISPC = 1,MAXSP
     HTCON(ISPC) = 0.0
     if (LHCOR2.and. HCOR2(ISPC).gt.0.0) HTCON(ISPC) = &
          &HTCON(ISPC)+ALOG(HCOR2(ISPC))
     50 continue
  end do
  return
end
