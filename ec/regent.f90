!===============================!
!   Compute small tree growth   !
!===============================!
! EC $Id$
subroutine REGENT(LESTB,ITRNIN)
  implicit none

  !  THIS SUBROUTINE COMPUTES HEIGHT AND DIAMETER INCREMENTS FOR
  !  SMALL TREES.  THE HEIGHT INCREMENT MODEL IS APPLIED TO TREES
  !  THAT ARE LESS THAN 10 INCHES DBH (5 INCHES FOR LODGEPOLE PINE),
  !  AND THE DBH INCREMENT MODEL IS APPLIED TO TREES THAT ARE LESS
  !  THAN 3 INCHES DBH.  FOR TREES THAT ARE GREATER THAN 2 INCHES
  !  DBH (1 INCH FOR LODGEPOLE PINE), HEIGHT INCREMENT PREDICTIONS
  !  ARE AVERAGED WITH THE PREDICTIONS FROM THE LARGE TREE MODEL.
  !  HEIGHT INCREMENT IS A FUNCTION OF SITE HEIGHT, CALCULATED
  !  IN **SMHTGF**, AND MODIFIED BY VIGOR AND DENSITY FUNCTIONS
  !  OF CCF, TOP HEIGHT AND CROWN RATIO. DIAMETER IS ASSIGNED FROM
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
  integer :: IREFI,ISPEC,KOUT,KK,MODE1
  integer :: ITRNIN,ISPC,I1,I2,I3,I,IPCCF,K,L,N
  real    :: H,BARK,VIGOR,TEMT,POTHTG,HTGR,ZZRAN,XWT,DAT45
  real    :: CON,XMX,XMN,SI,DGMX,D,CR,RAN,BACHLO,BRATIO,HK,DK,DKK
  real    :: BX,AX,DDS,HTNEW,SCALE3,CORNEW,SNP,SNX,SNY,EDH,P,TERM
  real    :: REGYR,FNT,SCALE,SCALE2,CCF,AVHT,X,PCTRED,XRHGRO,XRDGRO
  real    :: CORTEM(MAXSP)
  real    :: XMAX(MAXSP),XMIN(MAXSP)
  real    :: SLO(MAXSP),SHI(MAXSP),AB(9),DGMAX(MAXSP),DIAM(MAXSP)

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

  ! Data statements
  data DGMAX/&
       &2.8, 2.8, 2.4, 3.6, 2.5, 2.5, 3.5, 3.6, 3.6, 2.8,&
       &5.0, 2.8, 5.0, 5.0, 5.0, 2.5, 5.0, 5.0, 5.0, 5.0,&
       &5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0,&
       &2.8, 5.0/

  data XMAX/&
       &4.0, 4.0, 4.0, 4.0,10.0, 4.0, 5.0, 4.0, 6.0, 6.0,&
       &4.0, 6.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0,&
       &4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0,&
       &6.0, 4.0/

  data XMIN/&
       &2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 1.0, 2.0, 2.0, 2.0,&
       &2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0,&
       &2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0,&
       &2.0, 2.0/

  data SLO/&
       &20.,  50.,  50.,  50.,  15.,  50.,  30.,  40.,  50.,  70.,&
       &0.,  15.,   0.,   0.,   0.,  50.,   0.,   0.,   0.,   0.,&
       &0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,&
       &15.,   0./

  data SHI/&
       &80., 110., 110., 110.,  30., 110.,  70., 120., 150., 140.,&
       &999.,  30., 999., 999., 999., 110., 999., 999., 999., 999.,&
       &999., 999., 999., 999., 999., 999., 999., 999., 999., 999.,&
       &30., 999./

  data DIAM/&
       &0.4, 0.3, 0.3, 0.3, 0.2, 0.3, 0.4, 0.3, 0.3, 0.5,&
       &0.2, 0.2, 0.2, 0.4, 0.3, 0.3, 0.3, 0.2, 0.2, 0.2,&
       &0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2,&
       &0.2, 0.2/

  data AB/&
       &1.11436,-.011493,.43012E-4,-.72221E-7,.5607E-10,-.1641E-13,3*0./

  data REGYR/10.0/

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
  !  BASED ON A 10-YEAR GROWTH PERIOD.  THE VARIABLE SCALE IS USED TO CONVERT
  !  HEIGHT INCREMENT PREDICTIONS TO A FINT-YEAR PERIOD.  DIAMETER
  !  INCREMENT IS PREDICTED FROM CHANGE IN HEIGHT, AND IS SCALED TO A 10-
  !  YEAR PERIOD BY APPLICATION OF THE VARIABLE SCALE2.  DIAMETER INCREMENT
  !  IS CONVERTED TO A FINT-YEAR BASIS IN **UPDATE**.

  FNT = FINT
  if (LESTB) then
     if (FINT.le.5.0) then
        LSKIPH = .true.
     else
        FNT = FNT-5.0
     end if
  end if
  SCALE = FNT/REGYR
  SCALE2 = YR/FNT

  !  IF CALLED FROM **ESTAB** INTERPOLATE MID-PERIOD CCF AND TOP HT
  !  FROM VALUES AT START AND END OF PERIOD.

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
     SI = SITEAR(ISISP)
     if (SI .gt. SHI(ISISP)) SI = SHI(ISISP)
     if (SI .le. SLO(ISISP)) SI = SLO(ISISP) + 0.5

     !     PUT A CEILING ON DIAMETER GROWTH BASED ON OLIVER AND COCHRAN EMPR.
     !     RJ 11/28/88

     DGMX = DGMAX(ISPC) * SCALE

     !  PROCESS NEXT TREE RECORD.

     do I3 = I1,I2
        I = IND1(I3)
        D = DBH(I)
        if (D .ge. XMX) go to 25

        !  BYPASS INCREMENT CALCULATIONS IF CALLED FROM ESTAB AND THIS IS NOT A
        !  NEWLY CREATED TREE.

        if (LESTB) then
           if (I.lt.ITRNIN) go to 25

           !  ASSIGN CROWN RATIO FOR NEWLY ESTABLISHED TREES.

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

        ! COMPUTE VIGOR MODIFIER FROM CROWN RATIO.

        H = HT(I)
        BARK = BRATIO(ISPC,D,H)
        if (LSKIPH) then
           HTG(K) = 0.0
           go to 4
        end if
        X = FLOAT(ICR(I))/100.
        VIGOR = (150.0 * (X**3.0)*EXP(-6.0*X))+0.3
        if (VIGOR .gt.1.0)VIGOR = 1.0

        !     RETURN HERE TO PROCESS NEXT TRIPLE.

        2 continue

        !  CALL SMHTGF, THE SMALL TREE HEIGHT GROWTH ROUTINE.  SMHTGF IS ALSO
        !  CALLED FROM ESSUBH TO GROW PLANTED AND NATURAL TREES FROM
        !  ESTABLISHMENT TO 5 YEARS INTO THE CYCLE.

        if (DEBUG) write(JOSTND,*)'ABIRTH(I)= ',ABIRTH(I)
        TEMT = 10.
        MODE1 = 1
        call SMHTGF(MODE1,ICYC,ISPC,H,TEMT,POTHTG,JOSTND,DEBUG)

        HTGR = POTHTG*PCTRED*VIGOR*CON
        if (DEBUG) write(JOSTND,9983) X,VIGOR,HTGR,CON
        9983 format('IN REGENT X,VIGOR,HTGR,CON = ',4F10.4)
        3 continue
        ZZRAN = 0.0
        if (DGSD.ge.1.0) ZZRAN = BACHLO(0.0,1.0,RANN)
        if ((ZZRAN .gt.0.5) .or. (ZZRAN .lt. -2.0)) go to 3
        if (DEBUG)write(JOSTND,9984) HTGR,ZZRAN,XRHGRO,SCALE,WK4(I)
        9984 format('IN REGENT 9984 FORMAT',5(F10.4,2X))

        !  NOTE: WK4 IS A CLIMATE MODIFIER CALCULATED IN **CLGMULT**

        select case (ISPC)
        case(1:10,12,16,31)
           HTGR = (HTGR +ZZRAN*0.1)*XRHGRO * SCALE
           if (HTGR .lt.0.0) HTGR = 0.0
        case(11,13:15,17:30,32)
           HTGR = (HTGR +ZZRAN*0.1)*XRHGRO * SCALE * WK4(I)
           if (HTGR .lt.0.1) HTGR = 0.1
        end select

        !     COMPUTE WEIGHTS FOR THE LARGE AND SMALL TREE HEIGHT INCREMENT
        !     ESTIMATES.  IF DBH IS LESS THAN OR EQUAL TO XMN, THE LARGE TREE
        !     PREDICTION IS IGNORED (XWT=0.0).

        XWT = (D-XMN)/(XMX-XMN)
        if (D.le.XMN .or. LESTB) XWT = 0.0

        !     COMPUTE WEIGHTED HEIGHT INCREMENT FOR NEXT TRIPLE.

        if (DEBUG)write(JOSTND,9985)XWT,HTGR,HTG(K),I,K
        9985 format('IN REGENT 9985 FORMAT',3(F10.4,2X),2I7)
        HTG(K) = HTGR*(1.0-XWT) + XWT*HTG(K)
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

        if (D.ge.3.0) go to 23
        HK = H + HTG(K)
        if (HK .le.4.5) then
           DG(K) = 0.0
           DBH(K) = D+0.001*HK
        else

           !   BEGIN THE DIAMETER LOOKUP SECTION
           !      DKK = DIAMETER AT THE START OF THE PROJECTION
           !      DK  = DIAMETER WITH HTG ADDED TO THE STARTING HEIGHT
           !   DAT45  = DIAMETER AT 4.5 FEET PREDICTED FROM EQUATION.

           DAT45 = 0.
           DK = 0.
           DKK = 0.
           select case (ISPC)

              !  SPECIES USING WC LOGIC

           case(11,13:15,17:30,32)

              select case (ISPC)

              case(11)
                 DAT45 = -0.674 + 1.522*ALOG(4.5)
                 DKK = -0.674 + 1.522*ALOG(H)
                 DK = -0.674 + 1.522*ALOG(HK)

              case(13:15,17)
                 DAT45 = -2.089 + 1.980*ALOG(4.5)
                 DKK = -2.089 + 1.980*ALOG(H)
                 DK = -2.089 + 1.980*ALOG(HK)

              case(18,19)
                 DAT45 = -0.532 + 1.531*ALOG(4.5)
                 DKK = -0.532 + 1.531*ALOG(H)
                 DK = -0.532 + 1.531*ALOG(HK)

              case(20:30,32)
                 DAT45 = 3.102 + 0.021*4.5
                 DKK = 3.102 + 0.021*H
                 DK = 3.102 + 0.021*HK

              end select

              if (DKK .lt.0.0) DK = D
              if (DK .lt. DK) DKK = DK+.01
              if (DEBUG)write(JOSTND,*)'I,ISPC,DBH,H,HK,DKK,DK= ',&
                   &I,ISPC,DBH(I),H,HK,DKK,DK

              !  PONDEROSA PINE (EC LOGIC)

           case(10)
              DK = (HK-8.31485+.59200*7.)/3.03659
              DKK = (H-8.31485+.59200*7.)/3.03659
              if (H .lt.4.5) DKK = D

              !  OTHER SPECIES USING EC LOGIC (1-9,12,16,31)

           case default
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
              if (DEBUG)write(JOSTND,9986) AX,BX,ISPC,HK,BARK,&
                   &XRDGRO,DK,DKK
              9986 format('IN REGENT 9986 FORMAT AX,BX,ISPC,HK',&
                   &' BARK,XRDGRO,DK,DKK= '/T12, F10.3,2X,F10.3,2X,I5,2X,5F10.3)

           end select

           !  USE INVENTORY EQUATIONS IF CALIBRATION OF THE HT-DBH FUNCTION IS TURNED
           !  OFF, OR IF WYKOFF CALIBRATION DID NOT OCCUR.
           !  NOTE: THIS SIMPLIFIES TO IF(IABFLB(ISPC).EQ.1) BUT IS SHOWN IN IT'S
           !        ENTIRITY FOR CLARITY.

           if (.not.LHTDRG(ISPC) .or.&
                &(LHTDRG(ISPC) .and. IABFLG(ISPC).eq.1)) then
              call HTDBH(IFOR,ISPC,DK,HK,1)
              if (H .le.4.5) then
                 DKK = D
              else
                 call HTDBH(IFOR,ISPC,DKK,H,1)
              end if
              if (DEBUG)write(JOSTND,*)'INV EQN DUBBING I,IFOR,ISPC,H,HK,DK,'&
                   &,'DKK= ',I,IFOR,ISPC,H,HK,DK,DKK
              if (DEBUG)write(JOSTND,*)'ISPC,LHTDRG,IABFLG= ',&
                   &ISPC,LHTDRG(ISPC),IABFLG(ISPC)
           end if

           !         IF CALLED FROM **ESTAB** ASSIGN DIAMETER

           if (LESTB) then

              !         ADJUST REGRESSION TO PASS THROUGH BUD WIDTH AT 4.5 FEET.

              if (DAT45.gt.0.0.and. HK.ge.4.5.and. LHTDRG(ISPC) .and.&
                   &IABFLG(ISPC).eq.0) then
                 DBH(K) = DK - DAT45 + DIAM(ISPC)
              else
                 DBH(K) = DK
              end if
              if (DBH(K).lt.DIAM(ISPC) .or. HK.lt.4.5) DBH(K) = DIAM(ISPC)
              DBH(K) = DBH(K)+0.001*HK
              DG(K) = DBH(K)
           else

              !         COMPUTE DIAMETER INCREMENT BY SUBTRACTION, APPLY USER
              !         SUPPLIED MULTIPLIERS, AND CHECK TO SEE IF COMPUTED VALUE
              !         IS WITHIN BOUNDS.

              if (DEBUG)write(JOSTND,*)'BARK,XRDGRO= ',BARK,XRDGRO
              if (DK.lt.0.0.or. DKK.lt.0.0) then
                 DG(K) = HTG(K)*0.2*BARK*XRDGRO
                 DK = D+DG(K)
              else
                 DG(K) = (DK-DKK)*BARK*XRDGRO
              end if

              ! PROBLEM WITH HARDWOOD EQN, REDUCES TO .021*HG. SET TO RULE
              ! OF THUMB VALUE .1*HG FOR NOW. DIXON 11-04-92
              ! DON'T USE R.O.T. IF USING INVENTORY EQNS.  DIXON 03-31-98

              select case (ISPC)
              case(20:30,32)
                 if (LHTDRG(ISPC) .and. IABFLG(ISPC).eq.0)&
                      &DG(K) = 0.1*HTG(K)*XRDGRO
              end select

              if (DG(K) .lt.0.0) DG(K) = 0.0
              if (DG(K) .gt. DGMX) DG(K) = DGMX
              if (DEBUG)write(JOSTND,*)'K,DKK,DK,DG,DGMX= ',&
                   &K,DKK,DK,DG(K),DGMX

              !         SCALE DIAMETER INCREMENT TO 10-YR ESTIMATE.
              !         SCALE ADJUSTMENT IS ON GROWTH IN DDS RATHER THAN INCHES
              !         OF DG TO BE CONSISTENT WITH GRADD.

              DDS = DG(K)*(2.0*BARK*D+DG(K))*SCALE2
              DG(K) = SQRT((D*BARK)**2.0+DDS)-BARK*D
           end if
           if ((DBH(K)+DG(K)).lt.DIAM(ISPC)) then
              DG(K) = DIAM(ISPC)-DBH(K)
           end if
        end if
        if (DEBUG) then
           HTNEW = HT(I)+HTG(I)
           write(JOSTND,9987) I,ISPC,HT(I),HTG(I),HTNEW,DBH(I),DG(I)
           9987 format('IN REGENT, I=',I4,',  ISPC=',I3,'  CUR HT=',F7.2,&
                &',  HT INC=',F7.4,',  NEW HT=',F7.2,',  CUR DBH=',F10.5,&
                &',  DBH INC=',F7.4)
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
  SCALE3 = REGYR / FINTH

  ! COMPUTE DENSITY MODIFIER FROM CCF AND TOP HEIGHT.

  X = AVH*(RELDEN/100.0)
  if (X .gt.300.0) X = 300.0
  PCTRED = AB(1)&
       &+ X*(AB(2) + X*(AB(3) + X*(AB(4) + X*(AB(5)+ X*AB(6)))))
  if (PCTRED .gt.1.0) PCTRED = 1.0
  if (PCTRED .lt.0.01)PCTRED = 0.01
  if (DEBUG)write(JOSTND,9989)AVH,RELDEN,X,PCTRED
  9989 format('IN REGENT AVH,RELDEN,X,PCTRED = ',4F10.4)

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
     SI = SITEAR(ISISP)

     !  BEGIN TREE LOOP WITHIN SPECIES.  IF MEASURED HEIGHT INCREMENT IS
     !  LESS THAN OR EQUAL TO ZERO, OR DBH IS LESS THAN 5.0, THE RECORD
     !  WILL BE EXCLUDED FROM THE CALIBRATION.

     do I3 = I1,I2
        I = IND1(I3)
        H = HT(I)

        !  DIA GT 3 INCHES INCLUDED IN OVERALL MEAN

        if (IHTG.lt.2) H = H-HTG(I)
        if (DBH(I).ge.5.0.or.H.lt.0.01) go to 60
        if (SI .gt. SHI(ISISP)) SI = SHI(ISISP)
        if (SI .le. SLO(ISISP)) SI = SLO(ISISP) + 0.5

        !  COMPUTE VIGOR MODIFIER FROM CROWN RATIO.

        X = FLOAT(ICR(I))/100.
        VIGOR = (150.0 * (X**3.0)*EXP(-6.0*X))+0.3
        if (VIGOR .gt.1.0)VIGOR = 1.0

        MODE1 = 1
        call SMHTGF(MODE1,ICYC,ISPC,H,REGYR,POTHTG,JOSTND,DEBUG)

        EDH = POTHTG*PCTRED*VIGOR*RHCON(ISPC)
        if (EDH .lt.0.1) EDH = 0.1
        if (DEBUG)write(JOSTND,9990) X,VIGOR,EDH
        9990 format('IN REGENT X,VIGOR,EDH = ',3F10.4)
        P = PROB(I)
        if (HTG(I).lt.0.001) go to 60
        TERM = HTG(I) * SCALE3
        SNP = SNP+P
        SNX = SNX+EDH*P
        SNY = SNY+TERM*P
        N = N+1

        !  PRINT DEBUG INFO IF DESIRED.

        if (DEBUG)write(JOSTND,9991) NPLT,I,ISPC,H,DBH(I),ICR(I),&
             &PCT(I),ATCCF,RHCON(ISPC),EDH,TERM
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
     if (LRCOR2.and.RCOR2(ISPC).gt.0.0)&
          &RHCON(ISPC) = RCOR2(ISPC)
     90 continue
  end do
  return
end
