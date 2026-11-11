!======================================!
!   Compute large tree height growth   !
!======================================!
! EC $Id$
subroutine HTGF
  implicit none

  !  THIS SUBROUTINE COMPUTES THE PREDICTED PERIODIC HEIGHT
  !  INCREMENT FOR EACH CYCLE AND LOADS IT INTO THE ARRAY HTG.
  !  THIS ROUTINE IS CALLED FROM **TREGRO** DURING REGULAR CYCLING.
  !  ENTRY **HTCONS** IS CALLED FROM **RCON** TO LOAD SITE
  !  DEPENDENT CONSTANTS THAT NEED ONLY BE RESOLVED ONCE.

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
  real    :: SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,D1,D2,BARK,BRATIO
  real    :: RHR(MAXSP),RHYXS(MAXSP),RHM(MAXSP),RHB(MAXSP)
  real    :: CRC,CRB,CRA,RHXS,RHK,HGUESS,SCALE,SINDX,XHT,BAL
  real    :: H,POTHTG,RELHT,HGMDCR,RHX,FCTRKX,FCTRRB,FCTRXB,FCTRM
  real    :: HGMDRH,WTCR,WTRH,HTGMOD,TEMPH,TEMHTG,AGP10
  real    :: MISHGF,MAXGUESS,HGUESS1,HGUESS2

  !  SPECIES LIST FOR EAST CASCADES VARIANT.

  !   1 = WESTERN WHITE PINE      (WP)    PINUS MONTICOLA
  !   2 = WESTERN LARCH           (WL)    LARIX OCCIDENTALIS
  !   3 = DOUGLAS-FIR             (DF)    PSEUDOTSUGA MENZIESII
  !   4 = PACIFIC SILVER FIR      (SF)    ABIES AMABILIS
  !   5 = WESTERN REDCEDAR        (RC)    THUJA PLICATA
  !   6 = GRAND FIR               (GF)    ABIES GRANDIS
  !   7 = LODGEPOLE PINE          (LP)    PINUS CONTORTA
  !   8 = ENGELMANN SPRUCE        (ES)    PICEA ENGELMANNII
  !   9 = SUBALPINE FIR           (AF)    ABIES LASIOCARPA
  !  10 = PONDEROSA PINE          (PP)    PINUS PONDEROSA
  !  11 = WESTERN HEMLOCK         (WH)    TSUGA HETEROPHYLLA
  !  12 = MOUNTAIN HEMLOCK        (MH)    TSUGA MERTENSIANA
  !  13 = PACIFIC YEW             (PY)    TAXUS BREVIFOLIA
  !  14 = WHITEBARK PINE          (WB)    PINUS ALBICAULIS
  !  15 = NOBLE FIR               (NF)    ABIES PROCERA
  !  16 = WHITE FIR               (WF)    ABIES CONCOLOR
  !  17 = SUBALPINE LARCH         (LL)    LARIX LYALLII
  !  18 = ALASKA CEDAR            (YC)    CALLITROPSIS NOOTKATENSIS
  !  19 = WESTERN JUNIPER         (WJ)    JUNIPERUS OCCIDENTALIS
  !  20 = BIGLEAF MAPLE           (BM)    ACER MACROPHYLLUM
  !  21 = VINE MAPLE              (VN)    ACER CIRCINATUM
  !  22 = RED ALDER               (RA)    ALNUS RUBRA
  !  23 = PAPER BIRCH             (PB)    BETULA PAPYRIFERA
  !  24 = GIANT CHINQUAPIN        (GC)    CHRYSOLEPIS CHRYSOPHYLLA
  !  25 = PACIFIC DOGWOOD         (DG)    CORNUS NUTTALLII
  !  26 = QUAKING ASPEN           (AS)    POPULUS TREMULOIDES
  !  27 = BLACK COTTONWOOD        (CW)    POPULUS BALSAMIFERA var. TRICHOCARPA
  !  28 = OREGON WHITE OAK        (WO)    QUERCUS GARRYANA
  !  29 = CHERRY AND PLUM SPECIES (PL)    PRUNUS sp.
  !  30 = WILLOW SPECIES          (WI)    SALIX sp.
  !  31 = OTHER SOFTWOODS         (OS)
  !  32 = OTHER HARDWOODS         (OH)

  !  SURROGATE EQUATION ASSIGNMENT:

  !  FROM THE EC VARIANT:
  !      USE 6(GF) FOR 16(WF)
  !      USE OLD 11(OT) FOR NEW 12(MH) AND 31(OS)

  !  FROM THE WC VARIANT:
  !      USE 19(WH) FOR 11(WH)
  !      USE 33(PY) FOR 13(PY)
  !      USE 31(WB) FOR 14(WB)
  !      USE  7(NF) FOR 15(NF)
  !      USE 30(LL) FOR 17(LL)
  !      USE  8(YC) FOR 18(YC)
  !      USE 29(WJ) FOR 19(WJ)
  !      USE 21(BM) FOR 20(BM) AND 21(VN)
  !      USE 22(RA) FOR 22(RA)
  !      USE 24(PB) FOR 23(PB)
  !      USE 25(GC) FOR 24(GC)
  !      USE 34(DG) FOR 25(DG)
  !      USE 26(AS) FOR 26(AS) AND 32(OH)
  !      USE 27(CW) FOR 27(CW)
  !      USE 28(WO) FOR 28(WO)
  !      USE 36(CH) FOR 29(PL)
  !      USE 37(WI) FOR 30(WI)

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
  !  IN THE EC VARIANT, SILVICS OF NORTH AMERICA (AG.HNDBK-654)
  !  WAS USED TO GET SHADE TOLERANCE.
  !  SEQ. NO.   CHAR. CODE    SHADE TOL.   SEQ. NO.  CHAR. CODE    SHADE TOL.
  !      1      WP            INTM            17     LL            VINT
  !      2      WL            VINT            18     YC            TOLN
  !      3      DF            INTM            19     WJ            INTL
  !      4      SF            VTOL            20     BM            VTOL
  !      5      RC            VTOL            21     VN            VTOL
  !      6      GF            TOLN            22     RA            INTL
  !      7      LP            VINT            23     PB            INTL
  !      8      ES            TOLN            24     GC            INTM
  !      9      AF            TOLN            25     DG            VTOL
  !     10      PP            INTL            26     AS            VINT
  !     11      WH            VTOL            27     CW            VINT
  !     12      MH            INTM            28     WO            INTM
  !     13      PY            VTOL            29     PL            INTL
  !     14      WB            INTM            30     WI            VINT
  !     15      NF            INTM            31     OS            INTM
  !     16      WF            TOLN            32     OH            VINT

  data RHR/&
       &15.0,  12.0,  15.0,  20.0,  20.0,&
       &16.0,  12.0,  16.0,  16.0,  13.0,&
       &20.0,  15.0,  20.0,  15.0,  15.0,&
       &16.0,  12.0,  16.0,  13.0,  20.0,&
       &20.0,  13.0,  13.0,  15.0,  20.0,&
       &12.0,  12.0,  15.0,  13.0,  12.0,&
       &15.0,  12.0/

  data RHYXS/&
       &0.10,  0.01,  0.10,  0.20,  0.20,&
       &0.15,  0.01,  0.15,  0.15,  0.05,&
       &0.20,  0.10,  0.20,  0.10,  0.10,&
       &0.15,  0.01,  0.15,  0.05,  0.20,&
       &0.20,  0.05,  0.05,  0.10,  0.20,&
       &0.01,  0.01,  0.10,  0.05,  0.01,&
       &0.10,  0.01/

  data RHM/&
       &1.10,  1.10,  1.10,  1.10,  1.10,&
       &1.10,  1.10,  1.10,  1.10,  1.10,&
       &1.10,  1.10,  1.10,  1.10,  1.10,&
       &1.10,  1.10,  1.10,  1.10,  1.10,&
       &1.10,  1.10,  1.10,  1.10,  1.10,&
       &1.10,  1.10,  1.10,  1.10,  1.10,&
       &1.10,  1.10/

  data RHB/&
       &-1.45, -1.60, -1.45, -1.10, -1.10,&
       &-1.20, -1.60, -1.20, -1.20, -1.60,&
       &-1.10, -1.45, -1.10,  0.10, -1.45,&
       &-1.20, -1.60, -1.20, -1.60, -1.10,&
       &-1.10, -1.60, -1.60, -1.45, -1.10,&
       &-1.60, -1.60, -1.45, -1.60, -1.60,&
       &-1.45, -1.60/

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

     do I3 = I1,I2
        I = IND1(I3)
        BAL = ((100.0-PCT(I))/100.0)*BA
        H = HT(I)
        HTG(I) = 0.

        SITAGE = 0.0
        SITHT = 0.0
        AGMAX = 0.0
        HTMAX = 0.0
        HTMAX2 = 0.0
        D1 = DBH(I)
        BARK = BRATIO(ISPC,D1,H)
        D2 = D1 + DG(I)/BARK
        if (PROB(I).le.0.0) go to 161
        if (DEBUG)write(JOSTND,*)' IN HTGF, CALLING FINDAG I= ',I
        call FINDAG(I,ISPC,D1,D2,H,SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,DEBUG)

        select case (ISPC)

           !  SPECIES USING EQUATIONS FROM THE WC VARIANT

        case(11,13:15,17:30,32)

           !  CHECK TO SEE IF TREE HT/DBH RATIO IS ABOVE THE MAXIMUM RATIO AT
           !  THE BEGINNING OF THE CYCLE. THIS COULD HAPPEN FOR TREES COMING
           !  OUT OF THE ESTAB MODEL.  IF IT IS, THEN CHECK TO SEE IF THE
           !  HT/NEWDBH RATIO IS ABOVE THE MAXIMUM.  IF THIS IS ALSO TRUE, LIMIT
           !  HTG TO 0.1 FOOT OR HALF THE DG, WHICH EVER IS GREATER.
           !  IF IT ISN'T, THEN LET HTG BE COMPUTED THE NORMAL
           !  WAY AND THEN CHECK IT AGAIN AT THAT POINT.

           if (H .gt.HTMAX) then
              if (H .ge. HTMAX2) then
                 HTG(I) = 0.5 * DG(I)
                 if (HTG(I).lt.0.1)HTG(I) = 0.1
                 HTG(I) = SCALE*XHT*HTG(I)*EXP(HTCON(ISPC))
              end if
              go to 161
           end if

           !  SPECIES USING EQUATIONS FROM THE EC VARIANT

        case default
           if (H .ge. HTMAX) then
              HTG(I) = 0.1
              HTG(I) = SCALE*XHT*HTG(I)*EXP(HTCON(ISPC))
              go to 161
           end if
        end select

        !  NORMAL HEIGHT INCREMENT CALCULATON BASED ON TREE AGE
        !  FIRST CHECK FOR MAXIMUM TREE AGE

        if (SITAGE .ge. AGMAX) then
           POTHTG = 0.10

           ! THE FOLLOWING 5 LINES ARE AN RJ FIX 7-28-88

           if (ISPC .eq.10) then
              POTHTG = -1.31 + 0.05 * SINDX
              if (POTHTG .lt.0.1) POTHTG = 0.1
           end if
           go to 1320
        else
           AGP10 = SITAGE+10.
        end if

        !   CALL HTCALC FOR NORMAL CYCLING

        HGUESS = 0.0
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

           HGUESS1 = 4.5 + MAXGUESS*(1-EXP(-0.137428*D1))**1.38994

           !  DIFFERENCE OF TWO DUBBED HEIGHTS IS POTENTIAL HEIGHT GROWTH

           POTHTG = HGUESS2 - HGUESS1
        end if
        !--End OWO PATCH

        if (DEBUG)write(JOSTND,*)' I,ISPC,AGP10,SITHT,HGUESS,POTHTG= ',&
             &I,ISPC,AGP10,SITHT,HGUESS,POTHTG

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
           write(JOSTND,*)' IN HTGF, I= ',I,' ISPC= ',ISPC,'HTGMOD= ',&
                &HTGMOD,' ICR= ',ICR(I),' HGMDCR= ',HGMDCR
           write(JOSTND,*)' HT(I)= ',HT(I),' AVH= ',AVH,' RELHT= ',RELHT,&
                &' HGMDRH= ',HGMDRH
        end if

        if (HTGMOD .ge.2.0) HTGMOD = 2.0
        if (HTGMOD .le.0.0) HTGMOD = 0.1

        HTG(I) = POTHTG * HTGMOD

        if (DEBUG)    write(JOSTND,901)ICR(I),PCT(I),BA,DG(I),HT(I),&
             &POTHTG,BAL,AVH,HTG(I),DBH(I),RMAI,HGUESS
        901 format(' HTGF',I5,14F9.2)

        !  HEIGHT GROWTH EQUATION, EVALUATED FOR EACH TREE EACH CYCLE
        !  MULTIPLIED BY SCALE TO CHANGE FROM A YR. PERIOD TO FINT AND
        !  MULTIPLIED BY XHT TO APPLY USER SUPPLIED GROWTH MULTIPLIERS.

        ! CHECK FOR HT GT MAX HT FOR THE SITE AND SPECIES

        TEMPH = H + HTG(I)
        select case (ISPC)
        case(11,13:15,17:30,32)
           if (TEMPH .gt. HTMAX2) HTG(I) = HTMAX2-H
        case default
           if (TEMPH .gt. HTMAX) HTG(I) = HTMAX-H
        end select
        if (HTG(I).lt.0.1)HTG(I) = 0.1
        HTG(I) = SCALE*XHT*HTG(I)*EXP(HTCON(ISPC))
        if (DEBUG) write(JOSTND,*)' I=',I,' TEMPH=',TEMPH,&
             &' D=',DBH(I),' DG=',DG(I),' H=',H,' HTG=',HTG(I),&
             &' HTMAX2=',HTMAX2
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
  !  ARE SITE DEPENDENT AND REQUIRE ONE-TIME RESOLUTION.
  !  LOAD OVERALL INTERCEPT FOR EACH SPECIES.

  do ISPC = 1,MAXSP
     HTCON(ISPC) = 0.0
     if (LHCOR2.and. HCOR2(ISPC).gt.0.0) HTCON(ISPC) = &
          &HTCON(ISPC)+ALOG(HCOR2(ISPC))
     50 continue
  end do

  return
end
