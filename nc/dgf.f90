!========================================!
!   Compute large tree diameter growth   !
!========================================!
! NC $Id$
subroutine DGF(DIAM)
  implicit none

  !  THIS SUBROUTINE COMPUTES THE VALUE OF DDS (CHANGE IN SQUARED
  !  DIAMETER) FOR EACH TREE RECORD, AND LOADS IT INTO THE ARRAY
  !  WK2.  DDS IS PREDICTED FROM HABITAT TYPE, LOCATION, SLOPE,
  !  ASPECT, ELEVATION, DBH, CROWN RATIO, BASAL AREA IN LARGER TREES,
  !  AND CCF.  THE SET OF TREE DIAMETERS TO BE USED IS PASSED AS THE
  !  ARGUEMENT DIAM.  THE PROGRAM THUS HAS THE FLEXIBILITY TO
  !  PROCESS DIFFERENT CALIBRATION OPTIONS.  THIS ROUTINE IS CALLED
  !  BY **DGDRIV** DURING CALIBRATION AND WHILE CYCLING FOR GROWTH
  !  PREDICTION.  ENTRY **DGCONS** IS CALLED BY **RCON** TO LOAD SITE
  !  DEPENDENT COEFFICIENTS THAT NEED ONLY BE RESOLVED ONCE.

  ! Common blocks
  include  'PRGPRM.f90'

  include  'CALCOM.f90'

  include  'ARRAYS.f90'

  include  'COEFFS.f90'

  include  'CONTRL.f90'

  include   'OUTCOM.f90'

  include  'PLOT.f90'

  include  'PDEN.f90'

  include 'VARCOM.f90'

  !  DIMENSIONS FOR INTERNAL VARIABLES.

  !     DIAM -- ARRAY LOADED WITH TREE DIAMETERS (PASSED AS AN
  !             ARGUEMENT).
  !     DGLD -- ARRAY CONTAINING COEFFICIENTS FOR THE LOG(DIAMETER)
  !             TERM IN THE DDS MODEL (ONE COEFFICIENT FOR EACH
  !             SPECIES).
  !     DGCR -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
  !             RATIO TERM IN THE DDS MODEL (ONE COEFFICIENT FOR
  !             EACH SPECIES).
  !   DGCRSQ -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
  !             RATIO SQUARED TERM IN THE DDS MODEL (ONE
  !             COEFFICIENT FOR EACH SPECIES).
  !   DGDBAL -- ARRAY CONTAINING COEFFICIENTS FOR THE INTERACTION
  !             BETWEEN BASAL AREA IN LARGER TREES AND LN(DBH) (ONE
  !             COEFFICIENT PER SPECIES).
  !    DGCCFA -- ARRAY CONTAINING THE COEFFICIENTS FOR THE CROWN
  !             COMPETITION FACTOR TERM IN THE DDS MODEL (ONE
  !             COEFFICIENT FOR EACH SPECIES, LOADED IN RCON).

  ! Variables
  logical :: DEBUG
  real    :: DIAM(MAXTRE),DGLD(12),DGCR(12),DGCRSQ(12),DGCCFA(12)
  real    :: DGDBAL(12),DGFOR(6,12),DGDS(4,12),DGEL2(12)
  real    :: DGSASP(12),DGCASP(12),DGSLOP(12),DGSLSQ(12),DGSLQ2(12)
  integer :: MAPDSQ(7,12),MAPLOC(7,12)
  real    :: DGSLP2(12),DGSITE(12)
  integer :: OBSERV(12)
  real    :: DGBA(12),DGHAH(12),DGPCCF(12),DGLAT2(5,12)
  real    :: DGDBA2,DGBA2,DGLD2,DGDSQ2,DGCR2,ALBA,ALRD,CONSPP,DGLDS
  real    :: DGCRS,DGCRS2,DGDSQS,DGDBLS,D,ALD,CRID,CR,BAL,HOAVH,PBA,PBAL
  real    :: ALPBA,DDS,TDDS
  integer :: ISPC,I1,I2,I3,I,IPCCF,ILAT,IWHO
  real    :: PRD,DLO,DHI,SDICS,SDICZ,A,B,XMAX,DGLT
  real    :: TEMPD1,TEMPD2,BRAT,BRATIO,DUP
  real    :: ZRD(MAXPLT)

  ! COEFS FOR SPECIES 2,6,9 ARE FROM THE WESSIN VARIANT (DOLPH)

  dimension DGCR2(12),DGDSQ2(12),DGLD2(12),DGBA2(12),DGDBA2(12)

  data DGLD/&
       &0.88425, 0.0    , 0.86990, 1.01718, 1.14082, 0.0    ,&
       &1.23911, 0.99531, 0.0    , 0.96865, 0.99531, 0.0/

  data DGCR/&
       &2.83271, 0.0    , 2.96040, 3.01884, 2.82796, 0.0    ,&
       &-1.20841, 2.08524, 0.0    , 1.5466 , 2.08524, 0.0/

  data DGCRSQ/&
       &-0.84141, 0.0    ,-1.08219,-1.12464,-2.14739, 0.0    ,&
       &2.31782,-0.98396, 0.0    , 0.07152,-0.98396, 0.0/

  data DGDBAL/&
       &-0.00358, 0.0    ,-0.00443,-0.00257,-0.00126, 0.0    ,&
       &-0.00199,-0.00147, 0.0    ,-0.00408,-0.00147, 0.0/

  data DGBA /&
       &0.0    , 0.0    ,-0.01744,-0.16596, 0.0    , 0.0    ,&
       &0.0    , 0.0    , 0.0    , 0.0    , 0.0    , 0.0/

  data DGHAH /&
       &0.0    , 0.0    , 0.0    , 0.0    , 0.56348, 0.0    ,&
       &0.0    , 0.50155, 0.0    , 0.0    , 0.50155, 0.0/

  data DGPCCF/&
       &0.0    , 0.0    , 0.0    , 0.0    , 0.0    , 0.0    ,&
       &0.0    ,-0.00018, 0.0    ,-0.00002,-0.0018,  0.0/

  data DGLD2/&
       &1.52284, 1.26883, 1.92426, 1.53339, 1.26883, 1.41389,&
       &1.41389, 1.52284, 1.53339, 1.63568, 1.41389, 0.0/

  data DGCR2/&
       &0.51033, 0.27986, 0.40047, 0.35739, 0.27986, 0.32660,&
       &0.32660, 0.51033, 0.35739, 0.27516, 0.32660, 0.0/

  data DGDSQ2/&
       &-0.26590,-0.35325,-0.44612,-0.47442,-0.35325,-0.48938,&
       &-0.48938,-0.26590,-0.47442,-0.54162,-0.48938, 0.0/

  data DGBA2/&
       &-0.35579, 0.00000,-0.24596,-0.12359, 0.00000,-0.25287,&
       &-0.25287,-0.35579,-0.12359,-0.20902,-0.25287, 0.0/

  data DGDBA2/&
       &0.00000,-0.79922, 0.00000,-0.44256,-0.79922,-0.16000,&
       &-0.16000, 0.00000,-0.44256,-0.32497,-0.16000, 0.0/

  !  IDTYPE IS A HABITAT TYPE INDEX THAT IS COMPUTED IN **RCON**.
  !  ASPECT IS STAND ASPECT.  OBSERV CONTAINS THE NUMBER OF
  !  OBSERVATIONS BY HABITAT CLASS BY SPECIES FOR THE UNDERLYING
  !  MODEL (THIS DATA IS ACTUALLY USED BY **DGDRIV** FOR CALIBRATION).

  data  OBSERV/&
       &7019,  138, 7019,  395, 1762,  125,  583,&
       &6504,    9,  403, 6504, 8928/

  !  DGCCFA CONTAINS COEFFICIENTS FOR THE CCF TERM BY SPECIES

  data DGCCFA/&
       &-0.06784, 0.0    , 0.0    , 0.0    , 0.0    , 0.0    ,&
       &0.0    , 0.0    , 0.0    , 0.0    , 0.0    , 0.0/

  !  DGFOR CONTAINS LOCATION CLASS CONSTANTS FOR EACH SPECIES.
  !  MAPLOC IS AN ARRAY WHICH MAPS FOREST ONTO A LOCATION CLASS.

  data MAPLOC/&
       &1,1,1,2,3,3,2,&
       &1,1,1,1,1,1,1,&
       &1,1,1,2,3,3,2,&
       &1,1,1,2,3,3,2,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1/

  data DGFOR/&
       &-2.00201,-2.19449,-1.84083, 0.00000, 0.00000, 0.00000,&
       &0.0    , 0.0    , 0.0    , 0.0    , 0.00000, 0.00000,&
       &-2.54402,-2.41928,-2.75656, 0.0    , 0.0    , 0.00000,&
       &-1.88042,-2.06853,-1.69815, 0.0    , 0.00000, 0.00000,&
       &-1.69950, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
       &0.0    , 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
       &-2.68349, 0.0    , 0.0    , 0.0    , 0.0    , 0.0    ,&
       &-0.94563, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
       &0.0    , 0.0    , 0.0    , 0.0    , 0.0    , 0.00000,&
       &-4.6744 , 0.0    , 0.0    , 0.0    , 0.00000, 0.00000,&
       &-0.94563, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
       &0.0    , 0.0    , 0.0    , 0.0    , 0.00000, 0.00000/

  data DGLAT2/&
       &0.1630,  0.1630,  0.1630,  0.1630,  0.1630,&
       &-0.4297, -0.4297, -0.4297, -0.2777, -0.4297,&
       &-0.1043, -0.1043, -0.1043, -0.1043, -0.1043,&
       &0.1434,  0.3191,  0.1434,  0.2246,  0.1434,&
       &-0.4297, -0.4297, -0.4297, -0.2777, -0.4297,&
       &0.0540, -0.1232,  0.0540,  0.0540,  0.0540,&
       &0.0540, -0.1232,  0.0540,  0.0540,  0.0540,&
       &0.1630,  0.1630,  0.1630,  0.1630,  0.1630,&
       &0.1434,  0.3191,  0.1434,  0.2246,  0.1434,&
       &0.1035, -0.3955, -0.3955, -0.3955, -0.3995,&
       &0.0540, -0.1232,  0.0540,  0.0540,  0.0540,&
       &0.0000,  0.0000,  0.0000,  0.0000,  0.0000/

  !  DGSLOP CONTAINS THE COEFFICIENTS FOR THE
  !  SLOPE TERM IN THE DIAMETER GROWTH EQUATION.  DGSLSQ CONTAINS
  !  COEFFICIENTS FOR THE (SLOPE)**2 TERM IN THE DIAMETER GROWTH MODELS.
  !  ALL OF THESE ARRAYS ARE SUBSCRIPTED BY SPECIES.

  data DGSLP2/&
       &0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
       &0.00000, 0.00000, 0.00000, 0.80370, 0.00000, 0.00000/
  data DGSLQ2/&
       &0.00000, 0.00000, 0.00000,-0.83400, 0.00000, 0.00000,&
       &0.00000, 0.00000,-0.83400, 0.00000, 0.00000, 0.00000/
  data DGEL2/&
       &0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,&
       &0.00000, 0.00000,-0.00700, 0.00000, 0.00000, 0.00000/
  data DGSITE/&
       &0.47932, 0.01401, 0.56356, 0.47360, 0.20189, 0.01200,&
       &0.32093, 0.00659, 0.00734, 1.10842, 0.00659, 0.00000/

  !  DGDS CONTAINS COEFFICIENTS FOR THE DIAMETER SQUARED TERMS
  !  IN THE DIAMETER INCREMENT MODELS; ARRAYED BY FOREST BY
  !  SPECIES.  MAPDSQ IS AN ARRAY WHICH MAPS FOREST ONTO A DBH**2
  !  COEFFICIENT.
  data MAPDSQ/&
       &1,1,1,1,2,2,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,2,2,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1,&
       &1,1,1,1,1,1,1/
  data DGDS/&
       &-0.000328,-0.000248,      0.0,      0.0,&
       &0.0     , 0.000000,      0.0,      0.0,&
       &-0.000313, 0.0     , 0.000000, 0.000000,&
       &-0.000356,-0.000268, 0.0     ,      0.0,&
       &-0.000875, 0.000000, 0.000000,      0.0,&
       &0.0     , 0.0     ,      0.0,      0.0,&
       &-0.000338, 0.000000, 0.000000, 0.000000,&
       &-0.000373, 0.000000, 0.000000,      0.0,&
       &0.0     , 0.000000,      0.0,      0.0,&
       &-0.000728, 0.0     , 0.0     ,      0.0,&
       &-0.000373, 0.000000,      0.0,      0.0,&
       &0.0     , 0.0     ,      0.0,      0.0/

  !  DGEL2 CONTAINS THE COEFFICIENTS FOR
  !  THE ELEVATION SQUARED TERM IN THE DIAMETER GROWTH EQUATION.
  !  DGSASP CONTAINS THE COEFFICIENTS FOR THE SIN(ASPECT)*SLOPE
  !  TERM IN THE DIAMETER GROWTH EQUATION.  DGCASP CONTAINS THE
  !  COEFFICIENTS FOR THE COS(ASPECT)*SLOPE TERM IN THE DIAMETER
  !  GROWTH EQUATION.  DGSLOP CONTAINS THE COEFFICIENTS FOR THE
  !  SLOPE TERM IN THE DIAMETER GROWTH EQUATION.  DGSLSQ CONTAINS
  !  COEFFICIENTS FOR THE (SLOPE)**2 TERM IN THE DIAMETER GROWTH MODELS.
  !  ALL OF THESE ARRAYS ARE SUBSCRIPTED BY SPECIES.

  data DGCASP/&
       &-0.14319, 0.0    ,-0.16836,-0.15630,-0.19174, 0.0    ,&
       &0.08632,-0.19935, 0.0    , 0.0    ,-0.19935, 0.0/
  data DGSASP/&
       &-0.02884, 0.0    ,-0.040708,-0.01560,-0.10656, 0.0    ,&
       &-0.11954,-0.03587, 0.0    , 0.0    ,-0.03587,  0.0/
  data DGSLOP/&
       &0.63500, 0.0    , 0.46468, 0.58937,-1.29627, 0.0    ,&
       &0.85815, 0.73530, 0.0    , 0.0    , 0.73530, 0.0/
  data DGSLSQ/&
       &-1.09400, 0.0    ,-0.87145,-1.05045, 0.87335, 0.0    ,&
       &-1.17209,-0.99561, 0.0    , 0.0    ,-0.99561, 0.0/

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'DGF',3,ICYC)
  if (DEBUG) write(JOSTND,3)ICYC
  3 format(' ENTERING SUBROUTINE DGF  CYCLE =',I5)

  !  CALL SDICAL TO LOAD THE POINT MAX SDI WEIGHTED BY SPECIES ARRAY XMAXPT

  IWHO = 2
  call SDICAL (IWHO, XMAX)

  !  LOAD RELATIVE DENSITY (ZEIDI) FOR INDIVIDUAL POINTS.
  !  ALL SPECIES AND ALL SIZES INCLUDED FOR THIS CALCULATION.
  !  POINT RELATIVE DENSITY IS USED FOR RW DG PREDICTIONS.

  DLO = 0.0
  DHI = 500.0
  ISPC = 0
  IWHO = 1
  I2 = INT(PI)

  if (DEBUG)write(JOSTND,*) 'IN DGF, CALL SDICLS - POINT COUNT ',I2
  do I1 = 1, I2
     call SDICLS (ISPC,DLO,DHI,IWHO,SDICS,SDICZ,A,B,I1)
     if (DEBUG)write(JOSTND,*) 'IN DGF, POINT ',I1,' SDICZ= ',SDICZ
     ZRD(I1) = SDICZ
  end do

  !  DEBUG OUTPUT: MODEL COEFFICIENTS.

  if (DEBUG)write(JOSTND,*) 'IN DGF,HTCON=',HTCON,&
       &'RMAI=',RMAI,'ELEV=',ELEV,'RELDEN=',RELDEN
  if (DEBUG)&
       &write(JOSTND,9000) DGCON,DGDSQ,DGLD,DGCR,DGCRSQ,DGCCFA
  9000 format(/11(1X,F10.5))

  !  ASSIGN VARIABLES WHICH ARE STAND DEPENDENT.

  ALBA = 0.0
  if (BA.gt.0.0)ALBA = ALOG(BA)
  ALRD = 0.0
  if (RELDEN.gt.0.0)ALRD = ALOG(RELDEN)

  !  BEGIN SPECIES LOOP.  ASSIGN VARIABLES WHICH ARE SPECIES DEPENDENT

  do ISPC = 1,12
     I1 = ISCT(ISPC,1)
     if (I1.eq.0) go to 20
     I2 = ISCT(ISPC,2)

     !  DETERMINE CONSPP BASED ON SPECIES
     !  REDWOOD
     if (ISPC.eq.12) then
        CONSPP = DGCON(ISPC)
        !  ALL OTHER SPECIES
     else
        CONSPP = DGCON(ISPC) + COR(ISPC) + DGCCFA(ISPC)*ALRD&
             &+ DGBA(ISPC)*ALBA
     end if

     DGLDS = DGLD(ISPC)
     DGCRS = DGCR(ISPC)
     DGCRS2 = DGCRSQ(ISPC)
     DGDSQS = DGDSQ(ISPC)
     DGDBLS = DGDBAL(ISPC)

     !  BEGIN TREE LOOP WITHIN SPECIES ISPC.

     do I3 = I1,I2
        I = IND1(I3)
        D = DIAM(I)
        if (D.le.0.0) goto 10
        ALD = ALOG(D)
        CRID = ((ICR(I)*ICR(I))/(ALOG(D+1.0)))/1000.0
        if (D .lt.2.0)CRID = 1.8
        CR = ICR(I)*0.01
        BAL = (1.0 - (PCT(I)/100.)) * BA
        HOAVH = HT(I)/AVH
        if (HOAVH .gt.1.5)HOAVH = 1.5
        PBA = PTBAA(ITRE(I))
        PBAL = PBA*(1.0-(PCT(I)/100.))
        if (PBAL.le.0.0)PBAL = BAL
        if (PBA.le.0.0 )PBA = BA
        ALPBA = 0.0
        if (PBA .gt.0.0)ALPBA = ALOG(PBA)
        IPCCF = ITRE(I)

        !  RELATIVE DENSITY (ZEIDI) ON THE POINT
        !  CONSTRAIN RD IF NEEDED
        if (XMAXPT(ITRE(I)).le.0.0) then
           PRD = 0.0
        else
           PRD = ZRD(ITRE(I)) / XMAXPT(ITRE(I))
        end if

        if (DEBUG) write(JOSTND,333) PBA,D,BA,PBAL,PRD
        333 format(' DGF PBA,D,BA,PBAL,PRD =',4F12.3)

        !   DIFFERENT CALCULATION OF DG FOR SPECIES 2, 6 QND 9. IF NOT THESE
        !   SPECIES SKIP TO CALCULATION FOR OTHER SPECIES.

        !  PROCESS REDWOOD
        select case(ISPC)
        case(12)
           DGLT = EXP(CONSPP + 0.185911*LOG(D) -0.000073*D*D&
                &-0.001796*PBAL - 0.42078*PRD + 0.589318*LOG(CR*100)&
                &-0.000926*SLOPE*100 - 0.002203*(SLOPE*100)*COS(ASPECT))

           !  CONVERT OUTSIDE BARK DIAMETER INCREMENT TO INSIDE BARK
           !  CALCULATE CHANGE IN SQUARED DIAMETERS AND LOAD INTO WK2 ARRAY.
           BRAT = BRATIO(ISPC,D,HT(I))
           TEMPD1 = D * BRAT ! CURRENT DIB
           DUP = D + DGLT ! WITH GROWTH DOB
           TEMPD2 = DUP * BRAT ! WITH GROWTH DIB

           !  LOG OF CHANGE IN DIB SQUARED FOR THE PERIOD
           DDS = LOG(TEMPD2**2 - TEMPD1**2)

           !  RW DEBUG
           if (DEBUG)write(JOSTND,*)'IN DGF - RW DEBUG',' D=',D,&
                &' PBAL=',PBAL,' PRD=',PRD,' CR=',CR,' SI=',SITEAR(ISPC),&
                &' SLP=',SLOPE*100,' ASP=',ASPECT, ' DGLT=', DGLT,&
                &' BRAT=', BRAT,' DDS=',DDS

           !  APPLY COR AND COR2 TO DDS AND THEN CONVERT TO 5-YEAR DDS
           DDS = DDS + COR(ISPC) + LOG(COR2(ISPC))
           TDDS = EXP(DDS)
           DDS = ALOG(TDDS/2.0)

           if (DEBUG)write(JOSTND,*)'IN DGF - RW DEBUG ',' TDDS=',TDDS,&
                &' DDS=',DDS,' COR(ISPC)=',COR(ISPC),' COR2(ISPC)=',&
                &COR2(ISPC)

           !  PROCESS SUGAR PINE, INCENSE CEDAR, AND RED FIR
        case(2,6,9)
           DDS = CONSPP + DGLD2(ISPC)*ALD + DGDSQ2(ISPC)*D*D/1000.0&
                &+ DGCR2(ISPC)*CRID + DGDBA2(ISPC)*PBAL/(ALOG(D+1.0))/100.0&
                &+ DGBA2(ISPC)*ALPBA

           if (DEBUG) write(JOSTND,334) DDS,CONSPP,ALD,ALPBA,CRID
           334 format(' DGF DDS,CONSPP,ALD,ALPBA,CRID =',5F12.3)

           ! CONVERT TO 5 YEAR RATE IN REAL SCALE

           if (DDS .lt. -8.52) DDS = -8.52
           TDDS = EXP(DDS)
           DDS = ALOG(TDDS/2.0)

           !  PROCESS ALL OTHER SPECIES
        case default
           DDS = CONSPP + DGLDS*ALD  + CR*(DGCRS+CR*DGCRS2)&
                &+ DGDSQS*D*D  + DGDBLS*BAL/(ALOG(D+1.0))&
                &+ DGPCCF(ISPC)*PCCF(IPCCF) + DGHAH(ISPC)*HOAVH
           if (ISPC .eq.4)DDS = DDS - 0.15032
        end select

        ! THIS IS A SPECIES DUMMY TO ADJUST THE OTHERWISE DF COEFFICIENTS

        if (DDS.lt.-9.21) DDS = -9.21
        WK2(I) = DDS

        !  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.

        if (DEBUG) then
           write(JOSTND,9001) I,ISPC,D,BAL,CR,RELDEN,BA,DDS
           9001 format(' IN DGF, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,&
                &',  BAL=',F7.2,',  CR=',F7.4/&
                &'       RELDEN=',F9.3,',  BA=',F9.3,',   LN(DDS)=',F7.4)
        end if
        10 continue
     end do

     !  END OF SPECIES LOOP.

     20 continue
  end do
  if (DEBUG)write(JOSTND,100)ICYC
  100 format(' LEAVING SUBROUTINE DGF  CYCLE =',I5)
  return
  entry DGCONS
  call DBCHK (DEBUG,'DGCONS',6,ICYC)

  !  ENTRY POINT FOR LOADING COEFFICIENTS OF THE DIAMETER INCREMENT
  !  MODEL THAT ARE SITE SPECIFIC AND NEED ONLY BE RESOLVED ONCE.

  !  ENTER LOOP TO LOAD SPECIES DEPENDENT VECTORS.

  do ISPC = 1,12
     ILAT = 5
     ISPFOR = MAPLOC(IFOR,ISPC)
     ISPDSQ = MAPDSQ(IFOR,ISPC)

     !   DIFFERENT CALCULATION FOR SPECIES 2,6 AND 9. SKIP SPECIAL
     !   CODE IF NOT ONE OF THESE SPECIES.

     select case(ISPC)

        !  PROCESS REDWOOD
     case(12)
        DGCON(ISPC) = -3.502444 + 0.415435*LOG(SITEAR(ISPC))

        !  PROCESS SUGAR PINE, INCENSE CEDAR, AND RED FIR
     case(2,6,9)
        DGCON(ISPC) = DGLAT2(ILAT,ISPC) + DGEL2(ISPC)*ELEV&
             &+ DGSLP2(ISPC)*SLOPE + DGSLQ2(ISPC)*SLOPE*SLOPE&
             &+ DGSITE(ISPC)*SITEAR(ISPC)

        !  PROCESS ALL OTHER SPECIES
     case default
        DGCON(ISPC) = DGFOR(ISPFOR,ISPC)&
             &+ DGEL2(ISPC) * ELEV * ELEV&
             &+(DGSASP(ISPC) * SIN(ASPECT)&
             &+ DGCASP(ISPC) * COS(ASPECT)&
             &+ DGSLOP(ISPC)) * SLOPE&
             &+ DGSLSQ(ISPC) * SLOPE * SLOPE&
             &+ DGSITE(ISPC)*ALOG(SITEAR(3))
     end select

     DGDSQ(ISPC) = DGDS(ISPDSQ,ISPC)
     ATTEN(ISPC) = OBSERV(ISPC)
     SMCON(ISPC) = 0.

     if (DEBUG)write(JOSTND,9030)&
          &DGFOR(ISPFOR,ISPC),ELEV,DGEL2(ISPC),DGSASP(ISPC),&
          &ASPECT,DGCASP(ISPC),DGSLOP(ISPC),SLOPE,DGSLSQ(ISPC),&
          &RMAI,DGCON(ISPC),SITEAR(ISPC),DGSITE(ISPC)
     9030 format(' IN DGF 9030',7F10.5,/,1H ,11X,7F10.5)

     !  IF READCORD OR REUSCORD WAS SPECIFIED (LDCOR2 IS TRUE) ADD
     !  LN(COR2) TO THE BAI MODEL CONSTANT TERM (DGCON).  COR2 IS
     !  INITIALIZED TO 1.0 IN BLKDATA.
     !  RW DOES NOT HAVE COR2 APPLIED IN THIS CODE SEGMENT SINCE DDS IS
     !  NOT DIRECTLY CALCULATED FOR THIS SPECIES.

     if (LDCOR2.and.COR2(ISPC).gt.0.0) then
        if (ISPC .eq.12) then
           DGCON(ISPC) = DGCON(ISPC)
        else
           DGCON(ISPC) = DGCON(ISPC) + ALOG(COR2(ISPC))
        end if
     end if
     30 continue
  end do
  return
end
