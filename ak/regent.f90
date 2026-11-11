!===============================!
!   Compute small tree growth   !
!===============================!
! AK $Id$
subroutine REGENT(LESTB,ITRNIN)
  implicit none

  !  THIS SUBROUTINE COMPUTES HEIGHT AND DIAMETER INCREMENTS FOR
  !  SMALL TREES. THE HEIGHT INCREMENT MODELS ARE APPLIED
  !  TO TREES THAT ARE LESS THAN 3 INCH DBH AND THE DIAMETER INCREMENT
  !  MODELS ARE APPLIED TO TREES LESS THAN 1 INCH DBH. FOR TREES THAT ARE
  !  GREATER THAN 1 INCH DBH, HEIGHT INCREMENT PREDICTIONS ARE AVERAGED WITH
  !  THE PREDICTIONS FROM THE LARGE TREE MODEL.
  !  THIS ROUTINE IS CALLED FROM **CRATET** DURING CALIBRATION AND
  !  FROM **TREGRO** DURING CYCLING.  ENTRY **REGCON** IS CALLED FROM
  !  **RCON** TO LOAD MODEL PARAMETERS THAT NEED ONLY BE RESOLVED ONCE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.F77'
  include 'CALCOM.F77'
  include 'COEFFS.F77'
  include 'CONTRL.F77'
  include 'ESTCOR.F77'
  include 'HTCAL.F77'
  include 'MULTCM.F77'
  include 'OUTCOM.F77'
  include 'PDEN.F77'
  include 'PLOT.F77'
  include 'VARCOM.F77'

  !  VARIABLE DEFINITIONS:

  !   CORTEM -- A TEMPORARY ARRAY FOR PRINTING CORRECTION TERMS.
  !   NUMCAL -- A TEMPORARY ARRAY FOR PRINTING NUMBER OF HEIGHT
  !             INCREMENT OBSERVATIONS BY SPECIES.
  !    RHCON -- CONSTANT FOR THE HEIGHT INCREMENT MODEL.
  !     XMAX -- UPPER END OF THE RANGE OF DIAMETERS OVER WHICH HEIGHT
  !             INCREMENT PREDICTIONS FROM SMALL AND LARGE TREE MODELS
  !             ARE AVERAGED.
  !     XMIN -- LOWER END OF THE RANGE OF DIAMETERS OVER WHICH HEIGHT
  !             INCREMENT PREDICTIONS FROM THE SMALL AND LARGE TREE
  !             ARE AVERAGED.

  ! Variables
  external RANN

  ! Variables
  logical :: DEBUG,LESTB,LSKIPH

  character(len=2) :: SPEC

  integer :: I,I1,I2,I3,IICR,IP,IREFI,ISPC,ISPEC,ITRNIN
  integer :: K,KK,KOUT,L,N,MODE0,MODE2
  integer :: NUMCAL(MAXSP)

  real    :: BACHLO,BARK,BRATIO,CON,CORNEW,D,DDS,DK,DKK,EDH
  real    :: FNT,H,HK,HTGR,HTNEW,P,RANC,RCR
  real    :: REGYR,RSI,SCALE1,SCALE2,SCALE3,SNP,SNX,SNY
  real    :: TERM,XMN,XMX,XRDGRO,XRHGRO,ZZRAN
  real    :: CR,BX,AX
  real    :: DGSM,DGLT,LTHG,XWT,XDWT,AGET,HTMAX
  real    :: CORTEM(MAXSP),DIAM(MAXSP),XMIN(MAXSP),XMAX(MAXSP)
  real    :: DGMAX(MAXSP)

  ! SPECIES LIST FOR ALASKA VARIANT.

  ! Number Code  Common Name         FIA  PLANTS Scientific Name
  !   1     SF   Pacific silver fir  011  ABAM   Abies amabilis
  !   2     AF   subalpine fir       019  ABLA   Abies lasiocarpa
  !   3     YC   Alaska cedar        042  CANO9  Callitropsis nootkatensis
  !   4     TA   tamarack            071  LALA   Larix laricina
  !   5     WS   white spruce        094  PIGL   Picea glauca
  !   6     LS   Lutz�s spruce            PILU   Picea lutzii
  !   7     BE   black spruce        095  PIMA   Picea mariana
  !   8     SS   Sitka spruce        098  PISI   Picea sitchensis
  !   9     LP   lodgepole pine      108  PICO   Pinus contorta
  !  10     RC   western redcedar    242  THPL   Thuja plicata
  !  11     WH   western hemlock     263  TSHE   Tsuga heterophylla
  !  12     MH   mountain hemlock    264  TSME   Tsuga mertensiana
  !  13     OS   other softwoods     298  2TN
  !  14     AD   alder species       350  ALNUS  Alnus species
  !  15     RA   red alder           351  ALRU2  Alnus rubra
  !  16     PB   paper birch         375  BEPA   Betula papyrifera
  !  17     AB   Alaska birch        376  BENE4  Betula neoalaskana
  !  18     BA   balsam poplar       741  POBA2  Populus balsamifera
  !  19     AS   quaking aspen       746  POTR5  Populus tremuloides
  !  20     CW   black cottonwood    747  POBAT  Populus trichocarpa
  !  21     WI   willow species      920  SALIX  Salix species
  !  22     SU   Scouler�s willow    928  SASC   Salix scouleriana
  !  23     OH   other hardwoods     998  2TB

  !  DIMENSIONS FOR INTERNAL VARIABLES:

  !   CORTEM -- A TEMPORARY ARRAY FOR PRINTING CORRECTION TERMS.
  !   NUMCAL -- A TEMPORARY ARRAY FOR PRINTING NUMBER OF HEIGHT
  !             INCREMENT OBSERVATIONS BY SPECIES.
  !    RHCON -- CONSTANT FOR THE HEIGHT INCREMENT MODEL.  ZERO FOR ALL
  !             SPECIES IN THIS VARIANT
  !     XMAX -- UPPER END OF THE DIAMETER RANGE USED FOR WEIGHTING
  !             SMALL AND LARGE TREE HEIGHT GROWTH.
  !     XMIN -- LOWER END OF THE DIAMETER RANGE USED FOR WEIGHTING
  !             SMALL AND LARGE TREE HEIGHT GROWTH.
  !     DIAM -- BUD WIDTH (I.E. MINIMUM DIAMETER)
  !  STHGMLT -- SMALL TREE HEIGHT GROWTH MULTIPLIERS BY SPECIES. THESE
  !             MULTIPLIERS ARE USED TO HELP GET TREES OUT OF THE SMALL
  !             SIZE CLASSE.

  !  DATA STATEMENTS:

  data REGYR / 10.0 /

  data XMIN / 1.0, 1.0, 1.0, 1.0, 1.0, &
       & 1.0, 1.0, 1.0, 1.0, 1.0, &
       & 1.0, 1.0, 1.0, 1.0, 1.0, &
       & 1.0, 1.0, 1.0, 1.0, 1.0, &
       & 1.0, 1.0, 1.0/

  data XMAX / 5.0, 5.0, 8.0, 3.0, 3.0, &
       & 3.0, 3.0, 5.0, 8.0, 8.0, &
       & 7.0, 8.0, 3.0, 3.0, 3.0, &
       & 3.0, 3.0, 3.0, 3.0, 3.0, &
       & 3.0, 3.0, 3.0/

  data DGMAX /5.0, 5.0, 5.0, 3.0, 3.0, &
       & 3.0, 3.0, 5.0, 5.0, 5.0, &
       & 5.0, 5.0, 3.0, 3.0, 3.0, &
       & 3.0, 3.0, 3.0, 3.0, 3.0, &
       & 3.0, 3.0, 3.0/

  data DIAM / 0.3, 0.3, 0.3, 0.2, 0.4, &
       & 0.4, 0.2, 0.4, 0.4, 0.3, &
       & 0.3, 0.3, 0.4, 0.2, 0.2, &
       & 0.1, 0.1, 0.2, 0.3, 0.2, &
       & 0.1, 0.1, 0.2 /

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
  !  BASED ON A 1-YEAR GROWTH PERIOD.  SCALE IS USED TO CONVERT
  !  HEIGHT INCREMENT PREDICTIONS TO A FINT-YEAR PERIOD.  DIAMETER
  !  INCREMENT IS PREDICTED FROM CHANGE IN HEIGHT, AND IS SCALED TO A 1-
  !  YEAR PERIOD BY APPLICATION OF SCALE2.  DIAMETER INCREMENT
  !  IS CONVERTED TO A FINT-YEAR BASIS IN **UPDATE**.

  FNT = FINT
  if (LESTB) then
     if (FINT.le.5.0) then
        LSKIPH = .true.
     else
        FNT = FNT-5.0
     end if
  end if

  SCALE1 = FNT/REGYR
  SCALE2 = YR/FNT
  if (DEBUG)write(JOSTND,*)'SCALE DEBUG',' FNT=',FNT, 'REGYR ', &
       & REGYR, 'SCALE1 ', SCALE1, 'SCALE2 ', SCALE2

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

     !  PROCESS NEXT TREE RECORD.

     do I3 = I1,I2
        I = IND1(I3)
        IP = ITRE(I)
        D = DBH(I)
        if (D .ge. XMX)go to 25
        H = HT(I)
        BARK = BRATIO(ISPC,D,H)
        CR = ICR(I)

        !  BYPASS INCREMENT CALCULATIONS IF CALLED FROM ESTAB AND THIS IS NOT A
        !  NEWLY CREATED TREE.

        if (LESTB) then
           if (I.lt.ITRNIN) go to 25

           !  ASSIGN CROWN RATIO FOR NEWLY ESTABLISHED TREES.

           CR = 0.89722 - 0.0000461*PCCF(IP)
           1 continue
           RANC = BACHLO(0.0,1.0,RANN)
           if (RANC .lt. -1.0 .or. RANC .gt. 1.0) go to 1
           CR = CR + 0.07985 * RANC
           if (CR .gt. .90) CR = .90
           if (CR .lt. .20) CR = .20
           ICR(I) = INT((CR*100.0) + 0.5)
           CR = CR * 100
        end if
        IICR = ((ICR(I)-1)/10)+1
        if (IICR.gt.9) IICR = 9
        RCR = real(IICR)
        K = I
        L = 0
        if (LSKIPH) then
           HTG(K) = 0.0
           go to 5
        end if

        !     RETURN HERE TO PROCESS NEXT TRIPLE.

        2 continue

        !  CALCULATE SMALL TREE HEIGHT GROWTH
        !  SET MODE ARGUMENT TO 0

        !  INITIALIZE VARIABLES AND CALL HTCALC
        MODE0 = 0
        AGET = 0.0
        HTGR = 0.0
        HTMAX = 0.0
        call HTCALC(MODE0,ISPC,AGET,H,HTMAX,HTGR,JOSTND,DEBUG)

        !  CALL HTCALC TO DETERMINE AN AGE
        !  SET MODE ARGUMENT TO 2

        !  IF INITIAL TREE HEIGHT IS >= MAX HEIGHT FOR SITE (HTMAX).
        !  IF TRUE, THEN SET HTG= 0.1 FT.
        if (HTMAX-H.le.1.) then
           if (DEBUG) write(JOSTND,*) 'HTI>=HTMAX , HTMAX ',HTMAX
           HTGR = 0.10
        else
           ! ELSE FIND HTGR
           MODE2 = 2
           call HTCALC (MODE2,ISPC,AGET,H,HTMAX,HTGR,JOSTND,DEBUG)
        end if

        4 continue
        ZZRAN = 0.0
        if (DGSD.ge.1.0) ZZRAN = BACHLO(0.0,1.0,RANN)
        if ((ZZRAN .gt. 0.5) .or. (ZZRAN .lt. -2.0)) goto 4
        if (DEBUG)write(JOSTND,9984) HTGR,ZZRAN,XRHGRO,SCALE1
        9984 format('IN REGENT 9984 FORMAT',4(F10.4,2X))
        HTGR = (HTGR +ZZRAN*0.1)*XRHGRO * SCALE1 * CON
        if (DEBUG)write(JOSTND,*)'IN REGENT AFTER 9984',' HTGR=',HTGR

        ! COMPUTE WEIGHTS FOR SMALL TREE GROWTH ESTIMATES.
        ! IF DBH IS LESS THAN OR EQUAL TO XMN, THE LARGE TREE
        ! PREDICTION IS IGNORED (XWT 0.0).

        XWT = (D-XMN)/(XMX-XMN)
        XDWT = (D-XMN)/(DGMAX(ISPC)-XMN)
        if (D.le.XMN.or.LESTB) then
           XWT = 0.0
           XDWT = 0.0
        end if

        !     COMPUTE WEIGHTED HEIGHT INCREMENT FOR NEXT TRIPLE.

        LTHG = HTG(K)

        !     IF TREE IS COMING OUT OF ESTAB THEN DO NOT AVERAGE HTGR WITH LTHG
        !     SINCE THERE IS NO LARGE TREE GROWTH ESTIMATE IN THIS SCENARIO.
        if (LESTB) then
           HTGR = HTGR
        else
           HTGR = (HTGR + LTHG)/2.0
        end if

        !     CALCULATE SMALL TREE GROWTH WHICH IS WEIGHTED AVERAGE OF HTGR
        !     AND LTHG.
        HTG(K) = HTGR*(1.0-XWT) + XWT*LTHG
        if (DEBUG)write(JOSTND,9985)LESTB,ISPC,D,XWT,HTGR,LTHG,HTG(K),I,K
        9985 format(' IN REGENT 9985 FORMAT',' LESTB=',L3,'  ISPC=',I3, &
             & '  D=',F7.2,'  XWT=',F7.2,'  HTGR=',F7.4,'  LTHG=',F7.4, &
             & '  HTG=',F7.4,'  I=',I4, '  K=',I4)
        if (HTG(K) .lt. .1) HTG(K) = .1

        ! CHECK FOR SIZE CAP COMPLIANCE.

        if ((H+HTG(K)).gt.SIZCAP(ISPC,4)) then
           HTG(K) = SIZCAP(ISPC,4)-H
           if (HTG(K) .lt. 0.1) HTG(K) = 0.1
        end if

        5 continue

        !     ASSIGN DBH AND COMPUTE DBH INCREMENT FOR TREES WITH DBH LESS
        !     THAN DGMAX (COMPUTE 10-YEAR DBH INCREMENT REGARDLESS OF
        !     PROJECTION PERIOD LENGTH).

        ! IF DBH IS GREATER THAN DGMAX, BYPASS DG INCREMENT CALCULATION
        if (D.ge.DGMAX(ISPC)) go to 23
        HK = H + HTG(K)
        if (DEBUG)write(JOSTND,*)'ISPC,H,HTG,HK,D= ',ISPC,H,HTG(K),HK,D

        ! IF HEIGHT IS LESS THAN 4.5' DG IS 0 AND DBH IS ADJUSTED
        if (HK .le. 4.5) then
           DG(K) = 0.0
           DBH(K) = D+0.001*HK
        else

           !  USE INVENTORY EQUATIONS IF CALIBRATION OF THE HT-DBH FUNCTION IS TURNED
           !  OFF, OR IF WYKOFF CALIBRATION DID NOT OCCUR.

           !  NOTE: THIS SIMPLIFIES TO IF(IABFLB(ISPC).EQ.1) BUT IS SHOWN IN IT'S
           !  ENTIRITY FOR CLARITY.

           !  TEST WHICH HT-DBH RELATIONSHIP SHOULD BE USED FOR DBH IMPUTATION
           if (.not.LHTDRG(ISPC) .or. &
                & (LHTDRG(ISPC) .and. IABFLG(ISPC).eq.1)) then

              !  USE INVERTED CHAPMAN RICHARDS FUNCTION (INVENTORY EQUATION) TO CALCULATE
              !  DIAMETERS (DK,DKK)

              DK = 1/HTT12(ISPC) * LOG(1-((HK-4.5)/HTT11(ISPC))** &
                   & (1/HTT13(ISPC)))
              if (H .le. 4.5) then
                 DKK = D
              else
                 DKK = 1/HTT12(ISPC) * LOG(1-((H-4.5)/HTT11(ISPC))** &
                      & (1/HTT13(ISPC)))
              end if

              if (DEBUG)write(JOSTND,*)'INVENTORY EQN DUB',' K=',K,' ISPC=', &
                   & ISPC,' H=',H,' HK=',HK,' DKK= ',DKK,' DK=',DK

              if (DEBUG)write(JOSTND,*)'ISPC,LHTDRG,IABFLG= ', &
                   & ISPC,LHTDRG(ISPC),IABFLG(ISPC)

              !  ELSE USE INVERTED WYKOFF FUNCTION TO CALCULATE DIAMETERS (DK,DKK)

           else
              BX = HT2(ISPC)
              if (IABFLG(ISPC).eq.1) then
                 AX = HT1(ISPC)
              else
                 AX = AA(ISPC)
              end if
              DK = BX/(ALOG(HK-4.5)-AX)-1.0
              if (H .le. 4.5) then
                 DKK = D
              else
                 DKK = BX/(ALOG(H-4.5)-AX)-1.0
              end if
              if (DEBUG)write(JOSTND,*)'WYKOFF EQN DUB',' K=',K,' ISPC=', &
                   & ISPC,' H=',H,' HK=',HK,' DKK= ',DKK,' DK=',DK
           end if

           !  IF CALLED FROM **ESTAB** ASSIGN DIAMETER

           if (LESTB) then
              DBH(K) = DK
              if (DBH(K).lt.0.1) DBH(K) = 0.1
              DBH(K) = DBH(K)+0.001*HK
              DG(K) = DBH(K)
           else

              !  COMPUTE DIAMETER INCREMENT BY SUBTRACTION, APPLY USER
              !  SUPPLIED MULTIPLIERS, AND CHECK TO SEE IF COMPUTED VALUE
              !  IS WITHIN BOUNDS.

              if (DK.lt.0.0 .or. DKK.lt.0.0) then
                 DG(K) = HTG(K)*0.2*BARK*XRDGRO
                 DK = D+DG(K)
              else
                 DGSM = (DK-DKK)*BARK*XRDGRO
              end if
              if (DGSM .lt. 0.0) DGSM = 0.0

              !    SCALE SMALL TREE DIAMETER INCREMENT TO 10-YR ESTIMATE.
              !    TO BE CONSISTENT WITH GRADD, THE SCALE ADJUSTMENT IS
              !    ON GROWTH IN DDS TERMS RATHER THAN INCHES OF DG.

              !    NOTE: SMALL TREE DIAMETER GROWTH IS SCALTED TO 10-YEAR
              !    ESTIMATE BEFORE WEIGHTING.

              DDS = (DGSM*(2.0*BARK*D+DGSM)) * SCALE2
              DGSM = SQRT((D*BARK)**2.0+DDS)-BARK*D
              DGLT = DG(K)
              DG(K) = DGSM*(1.0-XDWT) + XDWT*DGLT
              if (DEBUG)write(JOSTND,*)' K=',K,' ISPC=',ISPC,' D=',D, &
                   & ' BARK=',BARK,' DWT=',XDWT,' DGSM=',DGSM,' DGLT=',DGLT, &
                   & ' DG(K)=',DG(K),' SCALE2=',SCALE2,' YR=',YR,' FNT=',FNT
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

        if (LESTB .or. .not.LTRIP .or. L.ge.2) go to 22
        L = L+1
        K = ITRN+2*I-2+L
        go to 2

        !  END OF GROWTH PREDICTION LOOP.  PRINT DEBUG INFO IF DESIRED.

        22 continue
        if (DEBUG) then
           HTNEW = HT(I)+HTG(I)
           write(JOSTND,9987) I,ISPC,HT(I),HTG(I),HTNEW,DBH(I),DG(I)
           9987 format('IN REGENT, I=',I4,',  ISPC=',I3,'  CUR HT=',F7.2, &
                & ',  HT INC=',F7.4,',  NEW HT=',F7.2,',  CUR DBH=',F10.5, &
                & ',  DBH INC=',F7.4)
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
  if (IFINTH .eq. 0)  goto 95
  SCALE3 = REGYR / FINTH

  !  BEGIN PROCESSING TREE LIST IN SPECIES ORDER.  DO NOT CALCULATE
  !  CORRECTION TERMS IF THERE ARE NO TREES FOR THIS SPECIES.

  do ISPC = 1,MAXSP
     CORNEW = 1.0
     I1 = ISCT(ISPC,1)
     if (I1.eq.0 .or. .not. LHTCAL(ISPC)) go to 100
     N = 0
     SNP = 0.0
     SNX = 0.0
     SNY = 0.0
     I2 = ISCT(ISPC,2)
     IREFI = IREF(ISPC)
     RSI = SITEAR(ISPC)
     if (RSI.lt.40.)RSI = 40.

     !  BEGIN TREE LOOP WITHIN SPECIES.  IF MEASURED HEIGHT INCREMENT IS
     !  LESS THAN OR EQUAL TO ZERO, OR DBH IS GREATER THAN 1.0, THE RECORD
     !  WILL BE EXCLUDED FROM THE CALIBRATION.

     do I3 = I1,I2
        I = IND1(I3)
        H = HT(I)
        IICR = ((ICR(I)-1)/10)+1
        if (IICR.gt.9) IICR = 9
        RCR = real(IICR)
        CR = ICR(I)

        !  DIA GT 3 INCHES INCLUDED IN OVERALL MEAN

        if (IHTG.lt.2) H = H-HTG(I)
        if (DBH(I).ge.3.0.or.H.lt.0.01) go to 60
        EDH = 0.

        !  CALCULATE SMALL TREE HEIGHT GROWTH
        !  SET MODE ARGUMENT TO 0

        !  INITIALIZE VARIABLES AND CALL HTCALC
        MODE0 = 0
        AGET = 0.0
        HTGR = 0.0
        HTMAX = 0.0
        call HTCALC(MODE0,ISPC,AGET,H,HTMAX,HTGR,JOSTND,DEBUG)

        !  CALL HTCALC TO DETERMINE AN AGE
        !  SET MODE ARGUMENT TO 2

        !  IF INITIAL TREE HEIGHT IS >= MAX HEIGHT FOR SITE (HTMAX).
        !  IF TRUE, THEN SET HTG= 0.1 FT.
        if (HTMAX-H.le.1.) then
           if (DEBUG) write(JOSTND,*) 'HTI>=HTMAX , HTMAX ',HTMAX
           HTGR = 0.10
        else
           ! ELSE FIND HTGR
           MODE2 = 2
           call HTCALC (MODE2,ISPC,AGET,H,HTMAX,HTGR,JOSTND,DEBUG)
        end if

        EDH = HTGR

        P = PROB(I)
        if (HTG(I).lt.0.001) go to 60
        TERM = HTG(I) * SCALE3
        SNP = SNP+P
        SNX = SNX+EDH*P
        SNY = SNY+TERM*P
        N = N+1

        !  PRINT DEBUG INFO IF DESIRED.

        if (DEBUG)write(JOSTND,9991) NPLT,I,ISPC,H,DBH(I),ICR(I), &
             & PCT(I),RELDM1,RHCON(ISPC),EDH,TERM
        9991 format('NPLT=',A26,',  I=',I5,',  ISPC=',I3,',  H=',F6.1, &
             & ',  DBH=',F5.1,',  ICR',I5,',  PCT=',F6.1,',  RELDEN=', &
             & F6.1 / 12X,'RHCON=',F10.3,',  EDH=',F10.3,', TERM=',F10.3)

        !  END OF TREE LOOP WITHIN SPECIES.

        60 continue
     end do
     if (DEBUG) write(JOSTND,9992) ISPC,SNP,SNX,SNY
     9992 format(/'SUMS FOR SPECIES ',I2,':  SNP=',F10.2, &
          & ';  SNX=',F10.2,';  SNY=',F10.2)

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

     if (CORNEW.lt.0.0821 .or. CORNEW.gt.12.1825) then
        call ERRGRO(.true.,27)
        write(JOSTND,9194)ISPC,JSP(ISPC),CORNEW
        9194 format(T28,'SMALL TREE HTG: SPECIES = ',I2,' (',A3, &
             & ') CALCULATED CALIBRATION VALUE = ',F8.2)
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
  9993 format(/'NUMBER OF RECORDS AVAILABLE FOR SCALING'/ &
       & 'THE SMALL TREE HEIGHT INCREMENT MODEL', &
       & ((T48,11(I4,2X)/)))
  95 continue
  write(JOSTND,9994) (CORTEM(I),I = 1,NUMSP)
  9994 format(/'INITIAL SCALE FACTORS FOR THE SMALL TREE'/ &
       & 'HEIGHT INCREMENT MODEL', &
       & ((T48,11(F5.2,1X)/)))

  ! OUTPUT CALIBRATION TO DATABASE.

  call DBSCALIB(2,CORTEM,NUMCAL,CORTEM) ! LAST ARG IGNORED

  ! OUTPUT CALIBRATION TERMS IF CALBSTAT KEYWORD WAS PRESENT.

  if (JOCALB .gt. 0) then
     KOUT = 0
     do K = 1,MAXSP
        if (CORTEM(K).ne.1.0 .or. NUMCAL(K).ge.NCALHT) then
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
     if (KOUT .eq. 0)write(JOCALB,209)
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
     if (LRCOR2.and.RCOR2(ISPC).gt.0.0) &
          & RHCON(ISPC) = RCOR2(ISPC)
     90 continue
  end do
  return
end subroutine REGENT
