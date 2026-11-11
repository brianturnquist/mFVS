!===============================!
!   Compute small tree growth   !
!===============================!
! WS $Id$
subroutine REGENT(LESTB,ITRNIN)
  implicit none

  !  THIS SUBROUTINE COMPUTES HEIGHT AND DIAMETER INCREMENTS FOR
  !  SMALL TREES.  THE HEIGHT INCREMENT MODEL IS APPLIED TO TREES
  !  THAT ARE LESS THAN 10 INCHES DBH (5 INCHES FOR LODGEPOLE PINE),
  !  AND THE DBH INCREMENT MODEL IS APPLIED TO TREES THAT ARE LESS
  !  THAN 3 INCHES DBH.  FOR TREES THAT ARE GREATER THAN 2 INCHES
  !  DBH (1 INCH FOR LODGEPOLE PINE), HEIGHT INCREMENT PREDICTIONS
  !  ARE AVERAGED WITH THE PREDICTIONS FROM THE LARGE TREE MODEL.
  !  HEIGHT INCREMENT IS A FUNCTION OF HABITAT TYPE, SLOPE,
  !  ASPECT, TREE HEIGHT, BASAL AREA IN LARGER TREES, AND
  !  STAND CROWN COMPETITION FACTOR.  DIAMETER IS ASSIGNED FROM
  !  A HEIGHT-DIAMETER FUNCTION WITH ADJUSTMENTS FOR RELATIVE SIZE
  !  AND STAND DENSITY.  INCREMENT IS COMPUTED BY SUBTRACTION.
  !  THIS ROUTINE IS CALLED FROM **CRATET** DURING CALIBRATION AND
  !  FROM **TREGRO** DURING CYCLING.  ENTRY **REGCON** IS CALLED FROM
  !  **RCON** TO LOAD MODEL PARAMETERS THAT NEED ONLY BE RESOLVED ONCE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CALCOM.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'
  include 'OUTCOM.f90'
  include 'PLOT.f90'
  include 'HTCAL.f90'
  include 'MULTCM.f90'
  include 'ESTCOR.f90'
  include 'PDEN.f90'
  include 'VARCOM.f90'

  !  DIMENSIONS FOR INTERNAL VARIABLES:

  !   CORTEM -- A TEMPORARY ARRAY FOR PRINTING CORRECTION TERMS.
  !   NUMCAL -- A TEMPORARY ARRAY FOR PRINTING NUMBER OF HEIGHT
  !             INCREMENT OBSERVATIONS BY SPECIES.
  !    RHCON -- CONSTANT FOR THE HEIGHT INCREMENT MODEL.  ZERO FOR ALL
  !             SPECIES IN THIS VARIANT
  !     XMAX -- UPPER END OF THE RANGE OF DIAMETERS OVER WHICH HEIGHT
  !             INCREMENT PREDICTIONS FROM SMALL AND LARGE TREE MODELS
  !             ARE AVERAGED.
  !     XMIN -- LOWER END OF THE RANGE OF DIAMETERS OVER WHICH HEIGHT
  !             INCREMENT PREDICTIONS FROM THE SMALL AND LARGE TREE
  !             ARE AVERAGED.

  external RANN
  ! Variables
  logical :: DEBUG,LESTB,LSKIPH
  character(len=2) :: SPEC
  integer :: NUMCAL(MAXSP)
  integer :: SMTMAP(MAXSP)
  integer :: N,IREFI,KOUT,ISPEC,KK,I3,I
  integer :: ITRNIN,ISPC,I1,I2,IPCCF,K,L,MSP,IHDW
  real    :: CORTEM(MAXSP)
  real    :: XMAX(MAXSP),XMIN(MAXSP),DIAM(MAXSP),AB(9)
  real    :: REGYR,FNT,BACHLO,BRATIO,DGSM,DDS,HTNEW,SCALE3,CORNEW
  real    :: SNP,SNX,SNY,EDH,P,TERM,X,VIGOR,DGMX,BKPT,CCF,AVHT,PCTRED
  real    :: SCALE,SCALE2,XRHGRO,XRDGRO,CON,XMX,XMN,SI,D,CR,RAN,BAL,H
  real    :: BARK,HTGRR,HTGR,ZZRAN,XWT,HK,BX,AX,DK,DKK,XDWT
  real    :: POTHTG,DAT45,LTHG,DGLT

  !     SPECIES LIST FOR WESTERN SIERRAS VARIANT.

  !     1 = SUGAR PINE (SP)                   PINUS LAMBERTIANA
  !     2 = DOUGLAS-FIR (DF)                  PSEUDOTSUGA MENZIESII
  !     3 = WHITE FIR (WF)                    ABIES CONCOLOR
  !     4 = GIANT SEQUOIA (GS)                SEQUOIADENDRON GIGANTEAUM
  !     5 = INCENSE CEDAR (IC)                LIBOCEDRUS DECURRENS
  !     6 = JEFFREY PINE (JP)                 PINUS JEFFREYI
  !     7 = CALIFORNIA RED FIR (RF)           ABIES MAGNIFICA
  !     8 = PONDEROSA PINE (PP)               PINUS PONDEROSA
  !     9 = LODGEPOLE PINE (LP)               PINUS CONTORTA
  !    10 = WHITEBARK PINE (WB)               PINUS ALBICAULIS
  !    11 = WESTERN WHITE PINE (WP)           PINUS MONTICOLA
  !    12 = SINGLELEAF PINYON (PM)            PINUS MONOPHYLLA
  !    13 = PACIFIC SILVER FIR (SF)           ABIES AMABILIS
  !    14 = KNOBCONE PINE (KP)                PINUS ATTENUATA
  !    15 = FOXTAIL PINE (FP)                 PINUS BALFOURIANA
  !    16 = COULTER PINE (CP)                 PINUS COULTERI
  !    17 = LIMBER PINE (LM)                  PINUS FLEXILIS
  !    18 = MONTEREY PINE (MP)                PINUS RADIATA
  !    19 = GRAY PINE (GP)                    PINUS SABINIANA
  !         (OR CALIFORNIA FOOTHILL PINE)
  !    20 = WASHOE PINE (WE)                  PINUS WASHOENSIS
  !    21 = GREAT BASIN BRISTLECONE PINE (GB) PINUS LONGAEVA
  !    22 = BIGCONE DOUGLAS-FIR (BD)          PSEUDOTSUGA MACROCARPA
  !    23 = REDWOOD (RW)                      SEQUOIA SEMPERVIRENS
  !    24 = MOUNTAIN HEMLOCK (MH)             TSUGA MERTENSIANA
  !    25 = WESTERN JUNIPER (WJ)              JUNIPERUS OCIDENTALIS
  !    26 = UTAH JUNIPER (UJ)                 JUNIPERUS OSTEOSPERMA
  !    27 = CALIFORNIA JUNIPER (CJ)           JUNIPERUS CALIFORNICA
  !    28 = CALIFORNIA LIVE OAK (LO)          QUERCUS AGRIFOLIA
  !    29 = CANYON LIVE OAK (CY)              QUERCUS CHRYSOLEPSIS
  !    30 = BLUE OAK (BL)                     QUERCUS DOUGLASII
  !    31 = CALIFORNIA BLACK OAK (BO)         QUERQUS KELLOGGII
  !    32 = VALLEY OAK (VO)                   QUERCUS LOBATA
  !         (OR CALIFORNIA WHITE OAK)
  !    33 = INTERIOR LIVE OAK (IO)            QUERCUS WISLIZENI
  !    34 = TANOAK (TO)                       LITHOCARPUS DENSIFLORUS
  !    35 = GIANT CHINQUAPIN (GC)             CHRYSOLEPIS CHRYSOPHYLLA
  !    36 = QUAKING ASPEN (AS)                POPULUS TREMULOIDES
  !    37 = CALIFORNIA-LAUREL (CL)            UMBELLULARIA CALIFORNICA
  !    38 = PACIFIC MADRONE (MA)              ARBUTUS MENZIESII
  !    39 = PACIFIC DOGWOOD (DG)              CORNUS NUTTALLII
  !    40 = BIGLEAF MAPLE (BM)                ACER MACROPHYLLUM
  !    41 = CURLLEAF MOUNTAIN-MAHOGANY (MC)   CERCOCARPUS LEDIFOLIUS
  !    42 = OTHER SOFTWOODS (OS)
  !    43 = OTHER HARDWOODS (OH)

  !  SURROGATE EQUATION ASSIGNMENT:

  !    FROM EXISTING WS EQUATIONS --
  !      USE 1(SP) FOR 11(WP) AND 24(MH)
  !      USE 2(DF) FOR 22(BD)
  !      USE 3(WF) FOR 13(SF)
  !      USE 8(PP) FOR 18(MP)
  !      USE 34(TO) FOR 35(GC), 36(AS), 37(CL), 38(MA), AND 39(DG)
  !      USE 31(BO) FOR 28(LO), 29(CY), 30(BL), 32(VO), 33(IO), 40(BM), AND
  !                     43(OH)

  !    FROM CA VARIANT --
  !      USE CA11(KP) FOR 12(PM), 14(KP), 15(FP), 16(CP), 17(LM), 19(GP), 20(WE),
  !                       25(WJ), 26(WJ), AND 27(CJ)
  !      USE CA12(LP) FOR 9(LP) AND 10(WB)

  !    FROM SO VARIANT --
  !      USE SO30(MC) FOR 41(MC)

  !    FROM UT VARIANT --
  !      USE UT17(GB) FOR 21(GB)

  !    GS AND RW USE NEW RELATIONSHIP FROM CASTLE 2019.

  ! Data statements
  data XMAX/&
       &3.5,  3.5,  3.5,  10.0,  3.5,  3.5,  3.5,  3.5,  4.0,  4.0,&
       &3.5,  4.0,  3.5,   4.0,  4.0,  4.0,  4.0,  3.5,  4.0,  4.0,&
       &199.,  3.5,  10.0,  3.5,  4.0,  4.0,  4.0,  3.5,  3.5,  3.5,&
       &3.5,  3.5,   3.5,  3.5,  3.5,  3.5,  3.5,  3.5,  3.5,  3.5,&
       &4.0,  3.5,  3.5/

  data XMIN/&
       &2.0,  2.0,  2.0,  2.0,  2.0,  2.0,  2.0,  2.0,  2.0,  2.0,&
       &2.0,  2.0,  2.0,  2.0,  2.0,  2.0,  2.0,  2.0,  2.0,  2.0,&
       &99.,  2.0,  2.0,  2.0,  2.0,  2.0,  2.0,  1.0,  1.0,  1.0,&
       &1.0,  1.0,  1.0,  2.0,  2.0,  2.0,  2.0,  2.0,  2.0,  1.0,&
       &2.0,  2.0,  1.0/

  data SMTMAP/&
       &1,    2,    2,    0,    1,    1,    2,    1,    0,    0,&
       &1,    0,    2,    0,    0,    0,    0,    1,    0,    0,&
       &0,    2,    0,    1,    0,    0,    0,    3,    3,    3,&
       &3,    3,    3,    4,    4,    4,    4,    4,    4,    3,&
       &0,    1,    3/

  data DIAM/&
       &0.3,  0.3,  0.3,  0.2,  0.2,  0.3,  0.3,  0.5,  0.4,  0.4,&
       &0.3,  0.5,  0.3,  0.5,  0.5,  0.5,  0.5,  0.5,  0.5,  0.5,&
       &0.4,  0.3,  0.2,  0.3,  0.5,  0.5,  0.5,  0.4,  0.4,  0.4,&
       &0.4,  0.4,  0.4,  0.2,  0.2,  0.2,  0.2,  0.2,  0.2,  0.4,&
       &0.2,  0.4,  0.4/

  data AB/&
       &1.11436,-.011493,.43012E-4,-.72221E-7,.5607E-10,-.1641E-13,3*0./

  !  CHECK FOR DEBUG.

  LSKIPH = .false.
  call DBCHK (DEBUG,'REGENT',6,ICYC)
  if (DEBUG) write(JOSTND,9980)ICYC
  9980 format('ENTERING SUBROUTINE REGENT  CYCLE =',I5)

  !  IF THIS IS THE FIRST CALL TO REGENT, BRANCH TO STATEMENT 40 FOR
  !  MODEL CALIBRATION.

  if (LSTART) goto 40
  call MULTS (3,IY(ICYC),XRHMLT)
  call MULTS(6,IY(ICYC),XRDMLT)
  if (ITRN.le.0) go to 91

  !  HEIGHT INCREMENT IS DERIVED FROM A HEIGHT-AGE CURVE AND IS NOMINALLY
  !  BASED ON A 5-YEAR GROWTH PERIOD FOR SPECIES USING EQUATIONS FROM THE
  !  CA AND WS VARIANTS; 10-YEAR GROWTH PERIOD FOR SPECIES USING EQUATIONS
  !  FROM THE SO AND UT VARIANTS.  SCALE IS USED TO CONVERT
  !  HEIGHT INCREMENT PREDICTIONS TO A FINT-YEAR PERIOD.  DIAMETER
  !  INCREMENT IS PREDICTED FROM CHANGE IN HEIGHT, AND IS SCALED TO A 10-YEAR
  !  PERIOD USING THE VARIABLE SCALE2.
  !  DIAMETER INCREMENT IS CONVERTED TO A FINT-YEAR BASIS IN **GRADD**.

  FNT = FINT
  if (LESTB) then
     if (FINT.le.5.0) then
        LSKIPH = .true.
     else
        FNT = FNT-5.0
     end if
  end if

  !  IF CALLED FROM **ESTAB** INTERPOLATE MID-PERIOD CCF AND TOP HT
  !  FROM VALUES AT START AND END OF PERIOD.
  !  THIS IS NEEDED FOR SPECIES USING EQUATIONS FROM THE SO AND UT VARIANTS.

  CCF = RELDEN
  AVHT = AVH
  if (LESTB.and.FNT.gt.0.0) then
     CCF = (5.0/FINT)*RELDEN +((FINT-5.0)/FINT)*ATCCF
     AVHT = (5.0/FINT)*AVH +((FINT-5.0)/FINT)*ATAVH
  end if

  ! COMPUTE DENSITY MODIFIER FROM CCF AND TOP HEIGHT.

  X = AVHT*(CCF/100.0)
  if (X .gt.300.0) X = 300.0
  PCTRED = AB(1)&
       &+ X*(AB(2) + X*(AB(3) + X*(AB(4) + X*(AB(5)+ X*AB(6)))))
  if (PCTRED .gt.1.0) PCTRED = 1.0
  if (PCTRED .lt.0.01) PCTRED = 0.01
  if (DEBUG) write(JOSTND,9982) AVHT,CCF,X,PCTRED
  9982 format('IN REGENT AVHT,CCF,X,PCTRED = ',4F10.4)

  !  ENTER GROWTH PREDICTION LOOP.  PROCESS EACH SPECIES AS A GROUP;
  !  LOAD CONSTANTS FOR NEXT SPECIES.

  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (I1.eq.0) go to 30
     I2 = ISCT(ISPC,2)
     XRHGRO = XRHMLT(ISPC)
     XRDGRO = XRDMLT(ISPC)
     CON = RHCON(ISPC) * EXP(HCOR(ISPC))
     XMX = XMAX(ISPC)
     XMN = XMIN(ISPC)
     SI = SITEAR(ISPC)
     MSP = SMTMAP(ISPC)
     if (ISPC.eq.21.or. ISPC.eq.41) then
        REGYR = 10.
     else
        REGYR = 5.
     end if
     SCALE = FNT/REGYR
     SCALE2 = YR/FNT

     !  PROCESS NEXT TREE RECORD.

     do I3 = I1,I2
        I = IND1(I3)
        D = DBH(I)
        if (D .ge. XMX) go to 25

        !  BYPASS INCREMENT CALCULATIONS IF CALLED FROM ESTAB AND THIS IS NOT A
        !  NEWLY CREATED TREE.

        if (LESTB) then
           if (I.lt.ITRNIN) go to 25

           !  CALCULATE CROWN RATIO FOR NEWLY REGENERATED TREES.

           IPCCF = ITRE(I)
           CR = 0.89722 - 0.0000461*PCCF(IPCCF)
           1 continue
           RAN = BACHLO(0.0,1.0,RANN)
           if (RAN .lt. -1.0.or. RAN .gt.1.0) go to 1
           CR = CR + 0.07985 * RAN
           if (CR .gt..90) CR = .90
           if (CR .lt..20) CR = .20
           ICR(I) = INT((CR*100.0)+0.5)
        end if
        K = I
        L = 0
        CR = real(ICR(I))/10.
        BAL = ((100.0-PCT(I))/100.0)*BA
        H = HT(I)
        BARK = BRATIO(ISPC,D,H)

        if (LSKIPH) then
           HTG(K) = 0.0
           go to 4
        end if

        select case (ISPC)

           !  SPECIES USING EQUATIONS FROM THE SO VARIANT: 41=MC

        case(41)
           X = real(ICR(I))/100.
           VIGOR = (150.0 * (X**3.0)*EXP(-6.0*X))+0.3
           if (VIGOR .gt.1.0)VIGOR = 1.0
           POTHTG = ((1.47043 + 0.23317*SI)/(31.56252 - 0.05586*SI))*10.
           HTGRR = POTHTG*PCTRED*VIGOR
           if (DEBUG) write(JOSTND,9981) I,X,VIGOR,POTHTG,PCTRED,HTGRR
           9981 format('IN REGENT I,X,VIGOR,POTHTG,PCTRED,HTGRR= ',I5,5F10.4)

           !  SPECIES USING EQUATIONS FROM THE CR VARIANT (VIA UT): 21=GB

        case(21)
           X = real(ICR(I))/100.
           VIGOR = (150.0 * (X**3.0)*EXP(-6.0*X))+0.3
           if (VIGOR .gt.1.0)VIGOR = 1.0
           VIGOR = 1.-((1.-VIGOR)/3.)
           POTHTG = ((SI/5.0)*(SI*1.5-H)/(SI*1.5))* 0.83
           HTGRR = POTHTG*PCTRED*VIGOR
           if (DEBUG) write(JOSTND,9981) I,X,VIGOR,POTHTG,PCTRED,HTGRR

           !  ALL OTHER SPECIES

        case default
           call SMHTGF(ISPC,D,CR,BA,BAL,SI,H,JOSTND,DEBUG,HTGRR)

           if (DEBUG)write(JOSTND,*)'AFTER CALL SMHTGF MSP,D,CR,BA,BAL,SI,'&
                &,'HTGRR =',MSP,D,CR,BA,BAL,SI,HTGRR
        end select

        !     RETURN HERE TO PROCESS NEXT TRIPLE.

        2 continue
        HTGR = HTGRR * CON
        if (DEBUG) write(JOSTND,9983) HTGR,HTGRR,CON
        9983 format('IN REGENT HTGR,HTGRR,CON = ',3F10.4)
        3 continue
        ZZRAN = 0.0
        if (DGSD.ge.1.0) ZZRAN = BACHLO(0.0,1.0,RANN)
        if ((ZZRAN .gt.0.5) .or. (ZZRAN .lt. -2.0)) go to 3
        if (DEBUG)write(JOSTND,9984) HTGR,ZZRAN,XRHGRO,SCALE,WK4(I)
        9984 format('IN REGENT 9984 FORMAT',5(F10.4,2X))

        select case (ISPC)
        case(21)
           HTGR = (HTGR +ZZRAN*0.2)*XRHGRO * SCALE * WK4(I)
        case default
           HTGR = (HTGR +ZZRAN*0.1)*XRHGRO * SCALE
        end select

        !     COMPUTE WEIGHTS FOR THE LARGE AND SMALL TREE HEIGHT INCREMENT
        !     ESTIMATES.  IF DBH IS LESS THAN OR EQUAL TO XMN, THE LARGE TREE
        !     PREDICTION IS IGNORED (XWT=0.0).

        XWT = (D-XMN)/(XMX-XMN)
        if (D.le.XMN .or. LESTB) XWT = 0.0

        !     COMPUTE WEIGHTED HEIGHT INCREMENT FOR NEXT TRIPLE.

        if (DEBUG)write(JOSTND,9985)XWT,HTGR,HTG(K),I,K
        9985 format('IN REGENT 9985 FORMAT',3(F10.4,2X),2I7)
        if (ISPC .eq.4.or. ISPC .eq.23) then
           LTHG = HTG(K)
           if (LESTB) then
              HTGR = HTGR
           else
              HTGR = (HTGR + LTHG)/2.0
           end if
           HTG(K) = HTGR*(1.0-XWT) + XWT*LTHG
           if (DEBUG)write(JOSTND,*)'IN REGENT - RW/GS DEBUG: ',' LESTB=',&
                &LESTB,' D=',D,' XWT=',XWT,' HTGR=',HTGR,' LTHG=',LTHG,&
                &' HTG FINAL=', HTG(K)
        else
           HTG(K) = HTGR*(1.0-XWT) + XWT*HTG(K)
        end if
        if (HTG(K) .lt..1) HTG(K) = .1

        ! CHECK FOR SIZE CAP COMPLIANCE.

        if ((H+HTG(K)).gt.SIZCAP(ISPC,4)) then
           HTG(K) = SIZCAP(ISPC,4)-H
           if (HTG(K) .lt.0.1) HTG(K) = 0.1
        end if

        4 continue

        !     ASSIGN DBH AND COMPUTE DBH INCREMENT FOR TREES WITH DBH LESS
        !     THAN 3 INCHES (COMPUTE 10-YEAR DBH INCREMENT REGARDLESS OF
        !     PROJECTION PERIOD LENGTH).

        if (ISPC .eq.21) then
           BKPT = 99.0
        else if (ISPC .eq.4.or. ISPC .eq.23) then
           BKPT = 7.0
        else
           BKPT = 3.0
        end if
        if (D .ge. BKPT) go to 23
        HK = H + HTG(K)
        if (HK .le.4.5) then
           DG(K) = 0.0
           DBH(K) = D+0.001*HK
        else

           select case (ISPC)

              !  SPECIES USING EQUATIONS FROM THE CA VARIANT

           case(4,9:10,12,14:17,19:20,23,25:27)
              BX = HT2(ISPC)
              if (IABFLG(ISPC).eq.1) then
                 AX = HT1(ISPC)
              else
                 AX = AA(ISPC)
              end if
              DK = (BX/(ALOG(HK-4.5)-AX))-1.0
              if (H .le.4.5) then
                 DKK = D
              else
                 DKK = (BX/(ALOG(H-4.5)-AX))-1.0
              end if

              !  SPECIES USING EQUATIONS FROM THE SO VARIANT

           case(41)
              BX = HT2(ISPC)
              if (IABFLG(ISPC).eq.1) then
                 AX = HT1(ISPC)
              else
                 AX = AA(ISPC)
              end if
              DAT45 = 3.1020 + 0.0210*4.5

              DKK = 3.1020 + 0.0210*H
              if (DKK .lt.0.0) DKK = D

              DK = 3.1020 + 0.0210*HK
              if (DK .lt. DKK) DK = DKK+.01
              if (DEBUG)write(JOSTND,*)'I,ISPC,DBH,H,HK,DK,DKK,= ',&
                   &I,ISPC,DBH(I),H,HK,DK,DKK

              !  SPECIES USING EQUATIONS FROM THE CR VARIANT, VIA UT

           case(21)
              DK = (HK-4.5)*10./(SITEAR(ISPC)-4.5)
              if (DK .lt.0.1) DK = 0.1
              DKK = (H-4.5)*10./(SITEAR(ISPC)-4.5)
              if (DKK .lt.0.1) DKK = 0.1
              if (H .lt.4.5) DKK = D
              if (DEBUG)write(JOSTND,*)'I,ISPC,DBH,H,HK,DK,DKK= ',&
                   &I,ISPC,DBH(I),H,HK,DK,DKK

              !  SPECIES USING EQUATIONS FROM THE WS VARIANT

           case default
              IHDW = 0

              select case (MSP)

                 !   PINES

              case(1)
                 AX = -0.6197
                 BX = 0.2626

                 !   FIRS

              case(2)
                 AX = -0.6096
                 BX = 0.2433

                 !   BLACKOAK AND TANOAK
                 !   EQUATIONS FOR HDW FROM NC VARIANT BO

              case(3,4)
                 AX = 4.80420
                 BX = -9.92422
                 IHDW = 1

              end select

              if (IHDW .eq.0) then
                 DK = AX + BX*HK
              else
                 DK = BX/(ALOG(HK-4.5)-AX)-1.0
              end if
              if (H .le.4.5) then
                 DKK = D
              else
                 if (IHDW.eq.0) then
                    DKK = AX + BX*H
                 else
                    DKK = BX/(ALOG(H-4.5)-AX)-1.0
                 end if
              end if

           end select

           if (DEBUG)write(JOSTND,9986) AX,BX,ISPC,HK,BARK,&
                &XRDGRO,DK,DKK
           9986 format('IN REGENT 9986 FORMAT AX,BX,ISPC,HK',&
                &' BARK,XRDGRO,DK,DKK= ',/,T12, F10.3,2X,F10.3,2X,I5,2X,&
                &5F10.3)

           select case (ISPC)

              !  SPECIES USING EQUATIONS FROM THE CA AND SO VARIANTS
              !  USE INVENTORY EQUATIONS IF CALIBRATION OF THE HT-DBH FUNCTION IS TURNED
              !  OFF, OR IF WYKOFF CALIBRATION DID NOT OCCUR.
              !  NOTE: THIS SIMPLIFIES TO IF(IABFLB(ISPC).EQ.1) BUT IS SHOWN IN IT'S
              !        ENTIRITY FOR CLARITY.

           case(4,9:10,12,14:17,19:20,23,25:27,41)
              if (.not.LHTDRG(ISPC) .or.&
                   &(LHTDRG(ISPC) .and. IABFLG(ISPC).eq.1)) then
                 call HTDBH (IFOR,ISPC,DK,HK,1)
                 if (H .le.4.5) then
                    DKK = D
                 else
                    call HTDBH (IFOR,ISPC,DKK,H,1)
                 end if
                 if (DEBUG)write(JOSTND,*)'INV EQN DUBBING IFOR,ISPC,H,HK,',&
                      &'DK,DKK= ',IFOR,ISPC,H,HK,DK,DKK
                 if (DEBUG)write(JOSTND,*)'ISPC,LHTDRG,IABFLG= ',&
                      &ISPC,LHTDRG(ISPC),IABFLG(ISPC)
              end if

           end select

           !       IF CALLED FROM **ESTAB** ASSIGN DIAMETER

           if (LESTB) then
              select case(ISPC)

                 !  SO VARIANT SPECIES (REALLY WC VARIANT)
                 !  ADJUST REGRESSION TO PASS THROUGH BUD WIDTH AT 4.5 FEET.

              case(41)

                 !  ADJUST REGRESSION TO PASS THROUGH BUD WIDTH AT 4.5 FEET.

                 if (DAT45.gt.0.0.and. HK.ge.4.5.and. LHTDRG(ISPC) .and.&
                      &IABFLG(ISPC).eq.0) then
                    DBH(K) = DK - DAT45 + DIAM(ISPC)
                 else
                    DBH(K) = DK
                 end if
              case default
                 DBH(K) = DK
              end select

              if (DBH(K).lt.DIAM(ISPC) .or. HK.lt.4.5) DBH(K) = DIAM(ISPC)
              DBH(K) = DBH(K)+0.001*HK
              DG(K) = DBH(K)
           else

              select case (ISPC)

                 !  FROM UT (ORIGINALLY FROM CR)

              case(21)
                 DGMX = 2.0 * SCALE
                 if (DK.lt.0.0.or. DKK.lt.0.0) then
                    DG(K) = HTG(K)*0.2*BARK*XRDGRO
                    DK = D+DG(K)
                 else
                    DG(K) = (DK-DKK)*BARK*XRDGRO
                 end if
                 if (DEBUG)write(JOSTND,*)'K,D,DK,DKK,DG= ',K,D,DK,DKK,DG(K)
                 if (DG(K) .lt.0.0) DG(K) = 0.0
                 if (DG(K) .gt. DGMX) DG(K) = DGMX
                 DDS = DG(K)*(2.0*BARK*D + DG(K))*SCALE2
                 DG(K) = SQRT((D*BARK)**2.0 + DDS) - BARK*D
                 if (DEBUG)write(JOSTND,*)'K,DG= ',K,DG(K)

                 !  FROM SO (ORIGINALLY FROM WC)
                 !  COMPUTE DIAMETER INCREMENT BY SUBTRACTION, APPLY USER
                 !  SUPPLIED MULTIPLIERS, AND CHECK TO SEE IF COMPUTED VALUE
                 !  IS WITHIN BOUNDS.

                 ! IF THE TREE JUST REACHED 4.5 FEET, SET DKK TO PRESENT DBH.
                 ! RJ 12/6/91
                 ! PROBLEM WITH HARDWOOD EQN, REDUCES TO .021*HG. SET TO RULE
                 ! OF THUMB VALUE .1*HG FOR NOW. DIXON 11-04-92
                 ! DON'T USE R.O.T. IF USING INVENTORY EQNS.  DIXON 03-31-98

              case(41)
                 if (H .lt.4.5 )DKK = D
                 BARK = BRATIO(ISPC,D,H)
                 DGMX = 5.0 * SCALE
                 if (DEBUG)write(JOSTND,*)'BARK,XRDGRO,DGMX= ',&
                      &BARK,XRDGRO,DGMX

                 if (DK.lt.0.0.or. DKK.lt.0.0) then
                    DG(K) = HTG(K)*0.2*BARK*XRDGRO
                    DK = D+DG(K)
                 else
                    DG(K) = (DK-DKK)*BARK*XRDGRO
                 end if
                 if (DEBUG)write(JOSTND,*)'K,DK,DKK,DG,BARK,XRDGRO= ',&
                      &K,DK,DKK,DG(K),BARK,XRDGRO
                 if (LHTDRG(ISPC) .and. IABFLG(ISPC).eq.0)&
                      &DG(K) = 0.1*HTG(K)*XRDGRO
                 if (DG(K) .lt.0.0) DG(K) = 0.1
                 if (DG(K) .gt. DGMX) DG(K) = DGMX
                 if (DEBUG)write(JOSTND,*)'HARDWOOD EQU DG(K),DGMX,LHTDRG= '
                 if (DEBUG)write(JOSTND,*)DG(K),DGMX,LHTDRG(ISPC)

              case default

                 !         COMPUTE DIAMETER INCREMENT BY SUBTRACTION, APPLY USER
                 !         SUPPLIED MULTIPLIERS, AND CHECK TO SEE IF COMPUTED VALUE
                 !         IS WITHIN BOUNDS.  FOR SPECIES FROM THE CA AND WS VARIANTS,
                 !         THE INCREMENT FOR TREES BETWEEN 1.5 AND 3 INCHES DBH IS A
                 !         WEIGHTED AVERAGE OF PREDICTIONS FROM THE LARGE AND SMALL
                 !         TREE MODELS. SCALE ADJUSTMENT IS ON GROWTH IN DDS TERMS RATHER
                 !         THAN INCHES OF DG TO MAINTAIN CONSISTENCY WITH GRADD.

                 !         NOTE: LARGE TREE DG IS ON A 10-YR BASIS; SMALL TREE DG IS ON A
                 !         FINT-YR BASIS. CONVERT SMALL TREE DG TO A 10-YR BASIS BEFORE
                 !         WEIGHTING. DG GETS CONVERTED BACK TO A FINT-YR BASIS IN **GRADD**.

                 if (ISPC.eq.4.or. ISPC.eq.23) then
                    XDWT = (D-XMN)/(BKPT-XMN)
                    if (D.le.XMN) XDWT = 0.0
                 else
                    XDWT = (D-1.5)/1.5
                    if (D.le.1.5) XDWT = 0.0
                    if (D.ge.3.0) XDWT = 1.0
                 end if
                 DGSM = (DK-DKK)*BARK*XRDGRO
                 if (DGSM .lt.0.0) DGSM = 0.0
                 DDS = DGSM*(2.0*BARK*D+DGSM)*SCALE2
                 DGSM = SQRT((D*BARK)**2.0+DDS)-BARK*D
                 if (DGSM.lt.0.0) DGSM = 0.0
                 DGLT = DG(K)
                 DG(K) = DGSM*(1.0-XDWT)+DGLT*XDWT
                 if (DEBUG)write(JOSTND,*)'IN REGENT',' ISPC=',ISPC,' D=',D,&
                      &' XDWT=',XDWT,' DKK=', DKK,' DK=',DK,' BARK=', BARK,&
                      &' DGSM=',DGSM,' DGLT=', DGLT,' DGSM FINAL=',DG(K)
              end select
           end if
           if ((DBH(K)+DG(K)).lt.DIAM(ISPC)) then
              DG(K) = DIAM(ISPC)-DBH(K)
           end if
        end if

        !  CHECK FOR TREE SIZE CAP COMPLIANCE

        call DGBND(ISPC,DBH(K),DG(K))

        23 continue

        !  RETURN TO PROCESS NEXT TRIPLE IF TRIPLING.  OTHERWISE,
        !  PRINT DEBUG AND RETURN TO PROCESS NEXT TREE.

        if (LESTB .or..not.LTRIP .or. L.ge.2) go to 22
        L = L+1
        K = ITRN+2*I-2+L
        go to 2

        !  END OF GROWTH PREDICTION LOOP.  PRINT DEBUG INFO IF DESIRED.

        22 continue
        if (DEBUG) then
           HTNEW = HT(I)+HTG(I)
           write(JOSTND,9987) I,ISPC,HT(I),HTG(I),HTNEW,DBH(I),DG(I)
           9987 format('IN REGENT, I=',I4,',  ISPC=',I3,'  CUR HT=',F7.2,&
                &',  HT INC=',F7.4,',  NEW HT=',F7.2,',  CUR DBH=',F10.5,&
                &',  DBH INC=',F7.4)
        end if
        25 continue
     end do
     30 continue
  end do
  go to 91

  !  SMALL TREE HEIGHT CALIBRATION SECTION.

  40 continue
  do ISPC = 1,MAXSP
     HCOR(ISPC) = 0.0
     CORTEM(ISPC) = 1.0
     NUMCAL(ISPC) = 0
     45 continue
  end do
  if (ITRN.le.0) go to 91
  if (IFINTH .eq.0)  goto 95

  ! COMPUTE DENSITY MODIFIER FROM CCF AND TOP HEIGHT.
  ! THIS IS NEEDED FOR SPECIES USING EQUATIONS FROM THE SO VARIANT (42=MC)

  X = AVH*(RELDEN/100.0)
  if (X .gt.300.0) X = 300.0
  PCTRED = AB(1)&
       &+ X*(AB(2) + X*(AB(3) + X*(AB(4) + X*(AB(5)+ X*AB(6)))))
  if (PCTRED .gt.1.0) PCTRED = 1.0
  if (PCTRED .lt.0.01) PCTRED = 0.01
  if (DEBUG)write(JOSTND,9988)AVH,RELDEN,X,PCTRED
  9988 format('IN REGENT AVH,RELDEN,X,PCTRED = ',4F10.4)

  !  BEGIN PROCESSING TREE LIST IN SPECIES ORDER.  DO NOT CALCULATE
  !  CORRECTION TERMS IF THERE ARE NO TREES FOR THIS SPECIES.

  do ISPC = 1,MAXSP
     CORNEW = 1.0
     I1 = ISCT(ISPC,1)
     if (I1.eq.0.or..not. LHTCAL(ISPC)) go to 100
     N = 0
     SNP = 0.0
     SNX = 0.0
     SNY = 0.0
     I2 = ISCT(ISPC,2)
     IREFI = IREF(ISPC)
     SI = SITEAR(ISPC)
     MSP = SMTMAP(ISPC)
     if (ISPC.eq.21.or. ISPC.eq.41) then
        REGYR = 10.
     else
        REGYR = 5.
     end if
     SCALE3 = REGYR / FINTH

     !  BEGIN TREE LOOP WITHIN SPECIES.  IF MEASURED HEIGHT INCREMENT IS
     !  LESS THAN OR EQUAL TO ZERO, OR DBH IS LESS THAN 5.0, THE RECORD
     !  WILL BE EXCLUDED FROM THE CALIBRATION.

     do I3 = I1,I2
        I = IND1(I3)
        D = DBH(I)
        H = HT(I)
        BAL = ((100.0-PCT(I))/100.0)*BA
        CR = real(ICR(I))/10.0

        !  DIA GT 3 INCHES INCLUDED IN OVERALL MEAN

        if (IHTG.lt.2) H = H-HTG(I)
        if (DBH(I).ge.5.0.or.H.lt.0.01) go to 60

        !  COMPUTE VIGOR MODIFIER FROM CROWN RATIO.

        X = real(ICR(I))/100.
        VIGOR = (150.0 * (X**3.0)*EXP(-6.0*X))+0.3
        if (VIGOR .gt.1.0)VIGOR = 1.0
        VIGOR = 1.-((1.-VIGOR)/3.)

        select case (ISPC)

           !  SPECIES USING EQUATIONS FROM THE SO VARIANT: 41=MC

        case(41)
           X = real(ICR(I))/100.
           VIGOR = (150.0 * (X**3.0)*EXP(-6.0*X))+0.3
           if (VIGOR .gt.1.0)VIGOR = 1.0
           POTHTG = ((1.47043 + 0.23317*SI)/(31.56252 - 0.05586*SI))*10.
           EDH = POTHTG*PCTRED*VIGOR
           if (DEBUG) write(JOSTND,9989) I,X,VIGOR,POTHTG,PCTRED,EDH
           9989 format('IN REGENT I,X,VIGOR,POTHTG,PCTRED,EDH= ',I5,5F10.4)

           !  SPECIES USING EQUATIONS FROM THE CR VARIANT (VIA UT): 21=GB

        case(21)
           POTHTG = ((SI/5.0)*(SI*1.5-H)/(SI*1.5))* 0.83
           EDH = POTHTG*PCTRED*VIGOR

           !  ALL OTHER SPECIES

        case default
           call SMHTGF(ISPC,D,CR,BA,BAL,SI,H,JOSTND,DEBUG,EDH)
           if (DEBUG)write(JOSTND,*)'AFTER CALL SMHTGF MSP,D,CR,BA,BAL,',&
                &'SI,EDH =',MSP,D,CR,BA,BAL,SI,EDH

        end select

        EDH = EDH*RHCON(ISPC)
        if (DEBUG)write(JOSTND,9990) I,ISPC,EDH,RHCON(ISPC)
        9990 format('IN REGENT I,ISPC,EDH,RHCON = ',2I5,2F10.4)
        P = PROB(I)
        if (HTG(I).lt.0.001) go to 60
        TERM = HTG(I) * SCALE3
        SNP = SNP+P
        SNX = SNX+EDH*P
        SNY = SNY+TERM*P
        N = N+1

        !  PRINT DEBUG INFO IF DESIRED.

        if (DEBUG)write(JOSTND,9991) NPLT,I,ISPC,H,DBH(I),ICR(I),&
             &PCT(I),RELDM1,RHCON(ISPC),EDH,TERM
        9991 format('NPLT=',A26,',  I=',I5,',  ISPC=',I3,',  H=',F6.1,&
             &',  DBH=',F5.1,',  ICR',I5,',  PCT=',F6.1,',  RELDEN=',&
             &F6.1 / 12X,'RHCON=',F10.3,',  EDH=',F10.3,', TERM=',F10.3)

        !  END OF TREE LOOP WITHIN SPECIES.

        60 continue
     end do
     if (DEBUG) write(JOSTND,9992) ISPC,SNP,SNX,SNY
     9992 format(/'SUMS FOR SPECIES ',I2,':  SNP=',F10.2,&
          &';  SNX=',F10.2,';  SNY=',F10.2)

     !  COMPUTE CALIBRATION TERMS.  CALIBRATION TERMS ARE NOT COMPUTED
     !  IF THERE WERE FEWER THAN NCALHT (DEFAULT=5) HEIGHT INCREMENT
     !  OBSERVATIONS FOR A SPECIES.

     if (N.lt.NCALHT) go to 80

     !  CALCULATE MEANS FOR THE POPULATION AND FOR THE SAMPLE ON THE
     !  NATURAL SCALE.

     SNX = SNX/SNP
     SNY = SNY/SNP

     !  CALCULATE RATIO ESTIMATOR.

     CORNEW = SNY/SNX
     if (CORNEW.le.0.0) CORNEW = 1.0E-4
     HCOR(ISPC) = ALOG(CORNEW)

     !  TRAP CALIBRATION VALUES OUTSIDE 2.5 STANDARD DEVIATIONS FROM THE
     !  MEAN. IF C IS THE CALIBRATION TERM, WITH A DEFAULT OF 1.0, THEN
     !  LN(C) HAS A MEAN OF 0.  -2.5 < LN(C) < 2.5 IMPLIES
     !  0.0821 < C < 12.1825

     if (CORNEW.lt.0.0821.or. CORNEW.gt.12.1825) then
        call ERRGRO(.true.,27)
        write(JOSTND,9194)ISPC,JSP(ISPC),CORNEW
        9194 format(T28,'SMALL TREE HTG: SPECIES = ',I2,' (',A3,&
             &') CALCULATED CALIBRATION VALUE = ',F8.2)
        CORNEW = 1.0
        HCOR(ISPC) = 0.0
     end if
     80 continue
     CORTEM(IREFI) = CORNEW
     NUMCAL(IREFI) = N
     100 continue
  end do

  !  END OF CALIBRATION LOOP.  PRINT CALIBRATION STATISTICS AND RETURN

  write(JOSTND,9993) (NUMCAL(I),I = 1,NUMSP)
  9993 format(/'NUMBER OF RECORDS AVAILABLE FOR SCALING'/&
       &'THE SMALL TREE HEIGHT INCREMENT MODEL',&
       &((T48,11(I4,2X)/)))
  95 continue
  write(JOSTND,9994) (CORTEM(I),I = 1,NUMSP)
  9994 format(/'INITIAL SCALE FACTORS FOR THE SMALL TREE'/&
       &'HEIGHT INCREMENT MODEL',&
       &((T48,11(F5.2,1X)/)))

  ! OUTPUT CALIBRATION TO DATABASE.

  call DBSCALIB(2,CORTEM,NUMCAL,CORTEM) ! LAST ARG IGNORED

  ! OUTPUT CALIBRATION TERMS IF CALBSTAT KEYWORD WAS PRESENT.

  if (JOCALB .gt.0) then
     KOUT = 0
     do K = 1,MAXSP
        if (CORTEM(K).ne.1.0.or. NUMCAL(K).ge.NCALHT) then
           SPEC = NSP(MAXSP,1)(1:2)
           ISPEC = MAXSP
           do KK = 1,MAXSP
              if (K .ne. IREF(KK)) go to 203
              ISPEC = KK
              SPEC = NSP(KK,1)(1:2)
              go to 2031
              203 continue
           end do
           2031 write(JOCALB,204)ISPEC,SPEC,NUMCAL(K),CORTEM(K)
           204 format(' CAL: SH',1X,I2,1X,A2,1X,I4,1X,F6.3)
           KOUT = KOUT + 1
        end if
        207 continue
     end do
     if (KOUT .eq.0)write(JOCALB,209)
     209 format(' NO SH VALUES COMPUTED')
     write(JOCALB,210)
     210 format(' CALBSTAT END')
  end if
  91 if (DEBUG)write(JOSTND,9995)ICYC
  9995 format('LEAVING SUBROUTINE REGENT  CYCLE =',I5)
  return

  entry REGCON

  !  ENTRY POINT FOR LOADING OF REGENERATION GROWTH MODEL
  !  CONSTANTS  THAT REQUIRE ONE-TIME RESOLUTION.

  do ISPC = 1,MAXSP
     RHCON(ISPC) = 1.0
     if (LRCOR2.and.RCOR2(ISPC).gt.0.0) RHCON(ISPC) = RCOR2(ISPC)
     90 continue
  end do

  do I = 1,MAXTRE
     ZRAND(I) = -999.0
  end do

  return
end
