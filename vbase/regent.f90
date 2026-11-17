!===============================!
!   Compute small tree growth   !
!===============================!
! VBASE $Id$
subroutine REGENT(LESTB,ITRNIN)
  implicit none

  !  REGENT COMPUTES SMALL-TREE HEIGHT AND DIAMETER INCREMENTS; REGCON
  !  INITIALIZES THE SITE CONSTANTS BEFORE CALIBRATION. BOTH PUBLIC ENTRY
  !  CONTRACTS ARE UNCHANGED. CONSTANT VARIANTCD GUARDS SELECT ONLY THE
  !  NATIVE FAMILY, INCLUDING ITS COEFFICIENTS, RANDOM DRAWS, TRIPLING,
  !  PERIOD CONVERSIONS AND DIAGNOSTICS.
  !  CA/OC SHARE THEIR SMHTGF/HTDBH CALCULATION WITH OC ORGANON GUARDS.
  !  CS/LS/NE SHARE THE HEIGHT-AGE BODY WITH NATIVE DIAMETER TABLES AND
  !  BALMOD INTERFACES. OP/PN/WC SHARE SMHGDG PROCESSING: OP USES ONE
  !  FIVE-YEAR STEP, PN/WC TWO; ONLY OP RETAINS ORGANON OVERRIDES.
  !  OTHER REGIONAL MODELS REMAIN SEPARATE KERNELS. LOCAL TABLES KEEP
  !  THEIR ORIGINAL SPECIES EXTENTS; COMMON BLOCKS USE THE BUILD'S PRGPRM.

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
  include 'ORGANON.f90'
  include 'GGCOM.f90'
  include 'KOTCOM.f90'
  include 'BCPLOT.f90'
  include 'METRIC.f90'

  logical :: LESTB,LCON
  integer :: ITRNIN

  LCON = .false.
  go to 1
  entry REGCON
  LCON = .true.
  1 continue
  if (VARIANTCD.eq.'CA'.or.VARIANTCD.eq.'OC') &
       &call REGCAOC(LCON)
  if (VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'LS'.or.VARIANTCD.eq.'NE') &
       &call REGCLS(LCON)
  if (VARIANTCD.eq.'OP'.or.VARIANTCD.eq.'PN'.or.VARIANTCD.eq.'WC') &
       &call REGPAC(LCON)
  if (VARIANTCD.eq.'AK') &
       &call REGAK(LCON)
  if (VARIANTCD.eq.'BM') &
       &call REGBM(LCON)
  if (VARIANTCD.eq.'BC') &
       &call REGBC(LCON)
  if (VARIANTCD.eq.'ON') &
       &call REGON(LCON)
  if (VARIANTCD.eq.'CI') &
       &call REGCI(LCON)
  if (VARIANTCD.eq.'CR') &
       &call REGCR(LCON)
  if (VARIANTCD.eq.'EC') &
       &call REGEC(LCON)
  if (VARIANTCD.eq.'EM') &
       &call REGEM(LCON)
  if (VARIANTCD.eq.'IE') &
       &call REGIE(LCON)
  if (VARIANTCD.eq.'KT') &
       &call REGKT(LCON)
  if (VARIANTCD.eq.'NC') &
       &call REGNC(LCON)
  if (VARIANTCD.eq.'SN') &
       &call REGSN(LCON)
  if (VARIANTCD.eq.'SO') &
       &call REGSO(LCON)
  if (VARIANTCD.eq.'TT') &
       &call REGTT(LCON)
  if (VARIANTCD.eq.'UT') &
       &call REGUT(LCON)
  if (VARIANTCD.eq.'WS') &
       &call REGWS(LCON)
  return

contains

  !===============================!
  !   Compute small tree growth   !
  !===============================!
  ! CA $Id$
  subroutine REGCAOC(LCON)
    implicit none

    logical :: LCON
    ! Regional callees retain their native argument lists.
    interface
       subroutine SMHTGF(NSPC,D,H,CR,BA,BAL,SI,HTGR,RELHT,DEBUG,JOSTND)
         integer :: NSPC
         real :: D
         real :: H
         real :: CR
         real :: BA
         real :: BAL
         real :: SI
         real :: HTGR
         real :: RELHT
         logical :: DEBUG
         integer :: JOSTND
       end subroutine SMHTGF
    end interface


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

    !  SHARED BY CA AND OC.  OC ALSO USES ORGANON GROWTH FOR VALID ORGANON
    !  TREES: THEIR HTG WAS SET IN **HTGF** (FINT-YR BASIS) AND THEIR DG IN
    !  **DGDRIV** (5-YR BASIS), SO THEY TAKE FULL WEIGHT (XWT, XDWT = 1);
    !  NEW SEEDLINGS ARE MARKED AS NON-ORGANON TREES.  ITS DIAGNOSTICS
    !  DIFFER SLIGHTLY.

    ! Common blocks

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
    !     DIAM -- BUD WIDTH (I.E. MINIMUM DIAMETER)

    external RANN
    ! Variables
    logical :: DEBUG,LSKIPH
    real    :: DIAM(50),CORTEM(50),XMAX(50),XMIN(50)
    integer :: NUMCAL(50)
    integer :: N,IREFI,KOUT,ISPEC,KK,I3,I
    integer :: ISPC,I1,I2,IPCCF,K,L
    real    :: REGYR,FNT,BACHLO,BRATIO,DGSM,DDS,HTNEW,SCALE3,CORNEW
    real    :: SNP,SNX,SNY,EDH,P,TERM
    real    :: SCALE,SCALE2,XRHGRO,XRDGRO,CON,XMX,XMN,SI,D,CR,RAN,BAL,H
    real    :: RELHT,BARK,HTGRR,HTGR,ZZRAN,XWT,HK,BX,AX,DK,DKK,XDWT,LTHG
    real    :: DGMIN(50),DGLT
    character(len=2) :: SPEC

    ! Data statements
    !     DATA XMAX/ 50*4.0 /
    data XMAX/ 22*4.0, 10.0, 26*4.0, 10.0/,XMIN/50*2.0/
    data DGMIN/22*3.0, 7.0, 26*3.0, 7.0/
    data REGYR/5.0/
    data DIAM/&
         &9*0.2, 2*0.5,   0.4, 4*0.5,   0.3, 3*0.5, 5*0.3,&
         &9*0.2,   0.3, 2*0.1,   0.2,   0.1,   0.3,   0.4,&
         &2*0.2, 3*0.1, 3*0.2,   0.3 /

    if (LCON) go to 9998

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
    !  BASED ON A 5-YEAR GROWTH PERIOD.  SCALE IS USED TO CONVERT
    !  HEIGHT INCREMENT PREDICTIONS TO A FINT-YEAR PERIOD.  DIAMETER
    !  INCREMENT IS PREDICTED FROM CHANGE IN HEIGHT, AND IS SCALED TO A
    !  10-YEAR PERIOD USING THE VARIABLE SCALE2.  DIAMETER INCREMENT
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
    if (DEBUG)write(JOSTND,*)'FNT,REGYR,YR,SCALE,SCALE2= ',&
         &FNT,REGYR,YR,SCALE,SCALE2

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

       !  PROCESS NEXT TREE RECORD.

       do I3 = I1,I2
          I = IND1(I3)
          D = DBH(I)
          if (D .ge. XMX) go to 25

          !  BYPASS INCREMENT CALCULATIONS IF CALLED FROM ESTAB AND THIS IS NOT A
          !  NEWLY CREATED TREE.

          if (LESTB) then
             if (I.lt.ITRNIN) go to 25

             !  OC: SET ORGANON TREE FLAG TO NON-VALID TREE BECAUSE THIS IS A NEW
             !  SEEDLING

             if (VARIANTCD.eq.'OC') IORG(I) = 0

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
          CR = FLOAT(ICR(I))/10.
          BAL = ((100.0-PCT(I))/100.0)*BA
          H = HT(I)

          if (AVH .le.0.0) then
             RELHT = 1.0
          else
             RELHT = H/AVH
          end if
          if (RELHT .gt.1.05) RELHT = 1.05

          BARK = BRATIO(ISPC,D,H)
          if (LSKIPH) then
             HTG(K) = 0.0
             go to 4
          end if

          call SMHTGF(ISPC,D,H,CR,BA,BAL,SI,HTGRR,&
               &RELHT,DEBUG,JOSTND)

          if (DEBUG) then
             if (VARIANTCD.eq.'OC') then
                write(JOSTND,*)'AFTER CALL SMHTGF D,CR,BA,BAL,SI,',&
                     &'HTGRR =',D,CR,BA,BAL,SI,HTGRR
             else
                write(JOSTND,*)'AFTER CALL SMHTGF D,H,CR,BA,BAL,SI,',&
                     &'HTGRR =',D,H,CR,BA,BAL,SI,HTGRR
             end if
          end if

          !     RETURN HERE TO PROCESS NEXT TRIPLE.

          2 continue

          HTGR = HTGRR
          HTGR = HTGRR * CON
          if (DEBUG) write(JOSTND,9983) HTGR,HTGRR,CON
          9983 format('IN REGENT HTGR,HTGRR,CON = ',3F10.4)
          3 continue
          ZZRAN = 0.0
          if (DGSD.ge.1.0) ZZRAN = BACHLO(0.0,1.0,RANN)
          if ((ZZRAN .gt.0.5) .or. (ZZRAN .lt. -2.0)) go to 3
          if (DEBUG)write(JOSTND,9984) HTGR,ZZRAN,XRHGRO,SCALE
          9984 format('IN REGENT 9984 FORMAT',4(F10.4,2X))
          HTGR = (HTGR +ZZRAN*0.1)*XRHGRO * SCALE

          !     COMPUTE WEIGHTS FOR THE LARGE AND SMALL TREE HEIGHT INCREMENT
          !     ESTIMATES.  IF DBH IS LESS THAN OR EQUAL TO XMN, THE LARGE TREE
          !     PREDICTION IS IGNORED (XWT=0.0).

          XWT = (D-XMN)/(XMX-XMN)
          if (D.le.XMN .or. LESTB) XWT = 0.0

          !  OC: IF THIS IS A VALID ORGANON TREE, HTG WAS SET IN HTGF

          if (VARIANTCD.eq.'OC') then
             if (LORGANON .and. (IORG(K) .eq.1)) XWT = 1.0
          end if

          !     COMPUTE WEIGHTED HEIGHT INCREMENT FOR NEXT TRIPLE.

          if (DEBUG)write(JOSTND,9985)XWT,HTGR,HTG(K),I,K
          9985 format('IN REGENT 9985 FORMAT',3(F10.4,2X),2I7)
          if (ISPC .eq.50.or. ISPC .eq.23) then
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

          if (D.ge.DGMIN(ISPC)) go to 23
          HK = H + HTG(K)
          if (HK .le.4.5) then
             DG(K) = 0.0
             DBH(K) = D+0.001*HK
          else
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

             !  USE INVENTORY EQUATIONS IF CALIBRATION OF THE HT-DBH FUNCTION IS TURNED
             !  OFF, OR IF WYKOFF CALIBRATION DID NOT OCCUR.
             !  NOTE: THIS SIMPLIFIES TO IF(IABFLB(ISPC).EQ.1) BUT IS SHOWN IN IT'S
             !        ENTIRITY FOR CLARITY.

             if (.not.LHTDRG(ISPC) .or.&
                  &(LHTDRG(ISPC) .and. IABFLG(ISPC).eq.1)) then
                call HTDBH (IFOR,ISPC,DK,HK,1)
                if (H .le.4.5) then
                   DKK = D
                else
                   call HTDBH (IFOR,ISPC,DKK,H,1)
                end if
                if (DEBUG)write(JOSTND,*)'INV EQN DUBBING IFOR,ISPC,H,HK,DK,'&
                     &,'DKK= ',IFOR,ISPC,H,HK,DK,DKK
                if (DEBUG)write(JOSTND,*)'ISPC,LHTDRG,IABFLG= ',&
                     &ISPC,LHTDRG(ISPC),IABFLG(ISPC)
             end if

             !         IF CALLED FROM **ESTAB** ASSIGN DIAMETER

             if (LESTB) then
                DBH(K) = DK
                if (DBH(K).lt.DIAM(ISPC)) DBH(K) = DIAM(ISPC)
                DBH(K) = DBH(K)+0.001*HK
                DG(K) = DBH(K)
             else

                !         COMPUTE DIAMETER INCREMENT BY SUBTRACTION, APPLY USER
                !         SUPPLIED MULTIPLIERS, AND CHECK TO SEE IF COMPUTED VALUE
                !         IS WITHIN BOUNDS.  IN ICASCA, THE INCREMENT FOR TREES BETWEEN
                !         1.5 AND 3 INCHES DBH IS A WEIGHTED AVERAGE OF PREDICTIONS FROM
                !         THE LARGE AND SMALL TREE MODELS.
                !         SCALE ADJUSTMENT IS ON GROWTH IN DDS TERMS RATHER THAN INCHES
                !         OF DG TO MAINTAIN CONSISTENCY WITH GRADD.

                !         NOTE: LARGE TREE DG IS ON A 10-YR BASIS; SMALL TREE DG IS ON A
                !         FINT-YR BASIS. CONVERT SMALL TREE DG TO A 10-YR BASIS BEFORE
                !         WEIGHTING. DG GETS CONVERTED BACK TO A FINT-YR BASIS IN **GRADD**.

                if (ISPC.eq.23.or. ISPC.eq.50) then
                   XDWT = (D-XMN)/(DGMIN(ISPC)-XMN)
                   if (D.le.XMN) XDWT = 0.0
                else
                   XDWT = (D-1.5)/1.5
                   if (D.le.1.5) XDWT = 0.0
                   if (D.ge.3.0) XDWT = 1.0
                end if

                !  OC: IF THIS IS A VALID ORGANON TREE, DG WAS SET IN DGDRIV AND IS
                !  ALREADY ON A 5-YR BASIS

                if (VARIANTCD.eq.'OC') then
                   if (LORGANON .and. (IORG(K) .eq.1)) XDWT = 1.0
                end if
                DGSM = (DK-DKK)*BARK*XRDGRO
                if (DGSM .lt.0.0) DGSM = 0.0
                DDS = DGSM*(2.0*BARK*D+DGSM)*SCALE2
                DGSM = SQRT((D*BARK)**2+DDS)-BARK*D
                DGLT = DG(K)
                if (DGSM.lt.0.0) DGSM = 0.0
                DG(K) = DGSM*(1.0-XDWT)+DGLT*XDWT
                if (DEBUG)write(JOSTND,*)'IN REGENT',' ISPC=',ISPC,' D=',D,&
                     &' XDWT=',XDWT,' DKK=', DKK,' DK=',DK,' BARK=', BARK,&
                     &' DGSM=',DGSM,' DGLT=', DGLT,' DGSM FINAL=',DG(K)
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
    SCALE3 = REGYR / FINTH

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

       !  BEGIN TREE LOOP WITHIN SPECIES.  IF MEASURED HEIGHT INCREMENT IS
       !  LESS THAN OR EQUAL TO ZERO, OR DBH IS LESS THAN 5.0, THE RECORD
       !  WILL BE EXCLUDED FROM THE CALIBRATION.

       do I3 = I1,I2
          I = IND1(I3)
          D = DBH(I)
          H = HT(I)
          BAL = ((100.0-PCT(I))/100.0)*BA
          CR = FLOAT(ICR(I))/10.0
          RELHT = H/AVH

          !  DIA GT 3 INCHES INCLUDED IN OVERALL MEAN

          if (IHTG.lt.2) H = H-HTG(I)
          if (DBH(I).ge.5.0.or.H.lt.0.01) go to 60
          call SMHTGF(ISPC,D,H,CR,BA,BAL,SI,EDH,RELHT,&
               &DEBUG,JOSTND)
          if (DEBUG) then
             if (VARIANTCD.eq.'OC') then
                write(JOSTND,*)'AFTER CALL SMHTGF D,CR,BA,BAL,SI,',&
                     &'EDH =',D,CR,BA,BAL,SI,EDH
             else
                write(JOSTND,*)'AFTER CALL SMHTGF D,H,CR,BA,BAL,SI,',&
                     &'EDH =',D,H,CR,BA,BAL,SI,EDH
             end if
          end if
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

       call REGHCAL(ISPC,SNP,SNX,SNY,CORNEW)
       80 continue
       CORTEM(IREFI) = CORNEW
       NUMCAL(IREFI) = N
       100 continue
    end do

    !  END OF CALIBRATION LOOP.  PRINT CALIBRATION STATISTICS AND RETURN

    if (VARIANTCD.eq.'OC') then
       write(JOSTND,9996) (NUMCAL(I),I = 1,NUMSP)
    else
       write(JOSTND,9993) (NUMCAL(I),I = 1,NUMSP)
    end if
    9993 format(/'NUMBER OF RECORDS AVAILABLE FOR SCALING'/&
         &'THE SMALL TREE HEIGHT INCREMENT MODEL',&
         &((T48,11(I4,2X)/)))
    9996 format(/'NUMBER OF RECORDS AVAILABLE FOR SCALING'/&
         &'THE SMALL TREE HEIGHT INCREMENT MODEL',&
         &((T49,11(I4,2X)/)))
    95 continue
    write(JOSTND,9994) (CORTEM(I),I = 1,NUMSP)
    9994 format(/'INITIAL SCALE FACTORS FOR THE SMALL TREE'/&
         &'HEIGHT INCREMENT MODEL',&
         &((T48,11(F5.2,1X)/)))

    ! OUTPUT CALIBRATION TO DATABASE.

    call REGCALOUT(CORTEM,NUMCAL,KOUT,K,KK,ISPEC,SPEC)
    91 if (DEBUG)write(JOSTND,9995)ICYC
    9995 format('LEAVING SUBROUTINE REGENT  CYCLE =',I5)

    ! OC: IF DEBUGGING, WRITE RESULTS TO THE OUTPUT FILE

    if (VARIANTCD.eq.'OC') then
       if (DEBUG) then
          do I = 1,ITRN
             write(JOSTND,*)' LEAVING REGENT I,ISP,DBH,DG,HT,HTG,IORG= ',&
                  &I,ISP(I),DBH(I),DG(I),HT(I),HTG(I),IORG(I)
          end do
       end if
    end if
    return
    9998 continue

    !  ENTRY POINT FOR LOADING OF REGENERATION GROWTH MODEL
    !  CONSTANTS  THAT REQUIRE ONE-TIME RESOLUTION.

    do ISPC = 1,MAXSP
       RHCON(ISPC) = 1.0
       if (LRCOR2.and.RCOR2(ISPC).gt.0.0)&
            &RHCON(ISPC) = RCOR2(ISPC)
       90 continue
    end do
    return
  end subroutine REGCAOC

  !===============================!
  !   Compute small tree growth   !
  !===============================!
  ! VLS $Id$
  subroutine REGCLS(LCON)
    implicit none

    logical :: LCON
    ! Regional callees retain their native argument lists.
    interface
       subroutine HTCALC(N,IVAR,ISPC,SI,YRS,H,AGET,HTMAX,HTG1,JOSTND,DEBUG)
         integer :: N
         integer :: IVAR
         integer :: ISPC
         real :: SI
         real :: YRS
         real :: H
         real :: AGET
         real :: HTMAX
         real :: HTG1
         integer :: JOSTND
         logical :: DEBUG
       end subroutine HTCALC
    end interface
    interface
       subroutine BALMOD(ISPC,D,BAL,BA,GMOD,DEBUG)
         integer :: ISPC
         real :: D
         real :: BAL
         real :: BA
         real :: GMOD
         logical :: DEBUG
       end subroutine BALMOD
    end interface


    !  SHARED BY CS, LS AND NE.  THE VARIANTS DIFFER ONLY IN THE HTCALC
    !  EQUATION SET (IVAR: CS 2, LS 1, NE 3; LS ALSO PASSES THE TREE AGE),
    !  THE BALMOD FORM (CS: BAL AND BA; LS: BA AND RMSQD; NE: THREE ARGUMENTS), NE'S
    !  SMALLER DIAMETER FOR BLENDING WITH THE LARGE TREE MODEL (XMIN 1.5),
    !  NE'S CROWN RATIO ROUNDING, THE MINIMUM DIAMETERS (DIAM) AND SOME
    !  DIAGNOSTICS.

    !  THIS SUBROUTINE COMPUTES HEIGHT AND DIAMETER INCREMENTS FOR
    !  SMALL TREES.  THE HEIGHT INCREMENT MODEL IS APPLIED TO TREES
    !  THAT ARE LESS THAN  5 INCHES DBH,
    !  AND THE DBH INCREMENT MODEL IS APPLIED TO TREES THAT ARE LESS
    !  THAN 5 INCHES DBH.  FOR TREES THAT ARE GREATER THAN 1.5 INCHES
    !  DBH, HEIGHT INCREMENT PREDICTIONS
    !  ARE AVERAGED WITH THE PREDICTIONS FROM THE LARGE TREE MODEL.
    !  THIS ROUTINE IS CALLED FROM **CRATET** DURING CALIBRATION AND
    !  FROM **TREGRO** DURING CYCLING.  ENTRY **REGCON** IS CALLED FROM
    !  **RCON** TO LOAD MODEL PARAMETERS THAT NEED ONLY BE RESOLVED ONCE.

    ! Common blocks

    !  DIMENSIONS FOR INTERNAL VARIABLES:

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
    !     DIAM -- BUDWIDTH VALUES (INCHES) BY SPECIES

    !  DECLARATIONS
    !  SCALARS

    external RANN
    ! Variables
    real    :: AGET,AX,BARK,BAL,BX,CORNEW,CR,D,DDS,DK,DKK,DGMX
    real    :: EDH,H,HK,HTNEW,HTGR,HTG1,HTMAX,P,PTCCF,FNT,RAN
    real    :: REGYR,SCALE,SCALE2,SCALE3,SNP,SNX,SNY,TERM,XMX,XMN
    real    :: XRDGRO,XRHGRO,XWT,CON,YRS,GMOD,RELHTA,DGGR,DGSM
    integer :: I,I1,I2,I3,ISPC,ISPEC,IPCCF,IREFI,K,KK,KOUT
    integer :: L,MODE0,N,IVAR,IVARN
    character(len=2) :: SPEC
    logical :: DEBUG,LSKIPH

    !  ARRAYS

    real    :: CORTEM(MAXSP),DGMAX(MAXSP),DIAM(108),HGADJ(MAXSP)
    real    :: DIAMCS(96),DIAMLS(68),DIAMNE(108)
    real    :: XMAX(MAXSP),XMIN(MAXSP)
    integer :: NUMCAL(MAXSP)

    !  FUNCTIONS

    real    :: BACHLO,BRATIO

    !  INITIALIZATIONS

    data HGADJ/ MAXSP*1.0/
    data DGMAX/ MAXSP*5.0 /
    data XMIN / MAXSP*3.0 /, XMAX/ MAXSP*5.0 /
    data REGYR/10.0/
    data DIAMCS/&
         &0.5, 0.3, 0.5, 0.5, 0.5, 0.3, 0.4, 0.4, 0.3, 0.2,&
         &0.2, 0.2, 0.2, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3,&
         &0.3, 0.3, 0.3, 0.1, 0.2, 0.2, 0.2, 0.1, 0.2, 0.3,&
         &0.2, 0.1, 0.1, 0.3, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,&
         &0.2, 0.1, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.1, 0.2,&
         &0.2, 0.2, 0.1, 0.1, 0.2, 0.2, 0.1, 0.1, 0.2, 0.2,&
         &0.1, 0.2, 0.2, 0.1, 0.2, 0.1, 0.1, 0.2, 0.1, 0.1,&
         &0.3, 0.1, 0.1, 0.2, 0.2, 0.3, 0.1, 0.2, 0.1, 0.2,&
         &0.1, 0.2, 0.1, 0.1, 0.1, 0.2, 0.2, 0.1, 0.1, 0.1,&
         &0.2, 0.2, 0.2, 0.2, 0.2, 0.2/
    data DIAMLS/&
         &0.1, 0.5, 0.1, 0.1, 0.4, 0.2, 0.2, 0.1, 0.2, 0.1,&
         &0.1, 0.1, 0.3, 0.5, 0.2, 0.2, 0.1, 0.2, 0.2, 0.1,&
         &0.1, 0.1, 0.1, 0.1, 0.1, 0.2, 0.2, 0.1, 0.2, 0.2,&
         &0.1, 0.2, 0.1, 0.2, 0.2, 0.2, 0.3, 0.3, 0.3, 0.2,&
         &0.3, 0.2, 0.1, 0.2, 0.3, 0.4, 0.2, 0.1, 0.1, 0.3,&
         &0.1, 0.1, 0.2, 0.1, 0.1, 0.1, 0.1, 0.2, 0.2, 0.1,&
         &0.1, 0.1, 0.2, 0.1, 0.1, 0.1, 0.1, 0.1/
    data DIAMNE/&
         &0.1, 0.1, 0.2, 0.2, 0.2, 0.2, 0.2, 0.1, 0.4, 0.5,&
         &0.5, 0.1, 0.1, 0.5, 0.5, 0.1, 0.1, 0.5, 0.1, 0.4,&
         &0.5, 0.5, 0.5, 0.5, 0.3, 0.2, 0.2, 0.2, 0.2, 0.1,&
         &0.1, 0.1, 0.1, 0.1, 0.3, 0.3, 0.3, 0.3, 0.3, 0.1,&
         &0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.3, 0.2,&
         &0.1, 0.2, 0.2, 0.1, 0.2, 0.2, 0.1, 0.1, 0.2, 0.2,&
         &0.2, 0.1, 0.2, 0.2, 0.1, 0.2, 0.2, 0.1, 0.2, 0.1,&
         &0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.3, 0.4, 0.1,&
         &0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.1, 0.1, 0.1, 0.1,&
         &0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.3, 0.1,&
         &0.1, 0.1, 0.2, 0.1, 0.1, 0.2, 0.2, 0.1/

    if (LCON) go to 9998
    HTGR = 0.0
    if (VARIANTCD.eq.'CS') then
       IVARN = 2
       DIAM(1:96) = DIAMCS
    else if (VARIANTCD.eq.'LS') then
       IVARN = 1
       DIAM(1:68) = DIAMLS
    else
       IVARN = 3
       DIAM(1:108) = DIAMNE
    end if

    !  CHECK FOR DEBUG.

    LSKIPH = .false.
    call DBCHK (DEBUG,'REGENT',6,ICYC)
    if (DEBUG) write(JOSTND,9980)ICYC,FINT
    9980 format('ENTERING SUBROUTINE REGENT  CYCLE =',I5,F10.3)

    !  HEIGHT INCREMENT IS DERIVED FROM A HEIGHT-AGE CURVE AND IS NOMINALLY
    !  BASED ON A 10-YEAR GROWTH PERIOD.   SCALE IS USED TO CONVERT
    !  HEIGHT INCREMENT PREDICTIONS TO A FINT-YEAR PERIOD.  DIAMETER
    !  INCREMENT IS PREDICTED FROM CHANGE IN HEIGHT, AND IS SCALED TO A 10-
    !  YEAR PERIOD BY APPLICATION OF THE VARIABLE SCALE2. DIAMETER INCREMENT
    !  IS CONVERTED TO A FINT-YEAR BASIS IN **UPDATE**.

    FNT = FINT
    if (LSTART) FNT = IFINTH
    if (LESTB) then
       if (FINT.le.5.0) then
          LSKIPH = .true.
       else
          FNT = FNT-5.0
       end if
    end if

    !  IF THIS IS THE FIRST CALL TO REGENT, BRANCH TO STATEMENT 40 FOR
    !  MODEL CALIBRATION.

    if (LSTART) goto 40
    call MULTS (3,IY(ICYC),XRHMLT)
    call MULTS(6,IY(ICYC),XRDMLT)
    if (ITRN.le.0) go to 91

    SCALE = FNT/REGYR
    SCALE2 = YR/FNT

    !  ENTER GROWTH PREDICTION LOOP.  PROCESS EACH SPECIES AS A GROUP;
    !  LOAD CONSTANTS FOR NEXT SPECIES.

    do ISPC = 1,MAXSP
       I1 = ISCT(ISPC,1)
       if (I1.eq.0) go to 30
       I2 = ISCT(ISPC,2)

       XRDGRO = XRDMLT(ISPC)
       XRHGRO = XRHMLT(ISPC)
       CON = RHCON(ISPC) * EXP(HCOR(ISPC))
       XMX = XMAX(ISPC)
       XMN = XMIN(ISPC)
       if (VARIANTCD.eq.'NE') XMN = 1.5
       DGMX = DGMAX(ISPC) * SCALE

       !  PROCESS NEXT TREE RECORD.

       do I3 = I1,I2
          I = IND1(I3)
          if (VARIANTCD.eq.'CS') BAL = (1.0-(PCT(I)/100.))*BA
          IPCCF = ITRE(I)
          D = DBH(I)
          if (D .ge. XMX) go to 25

          !  BYPASS INCREMENT CALCULATIONS IF CALLED FROM ESTAB AND THIS IS NOT A
          !  NEWLY CREATED TREE.

          if (LESTB) then
             if (I.lt.ITRNIN) go to 25

             !  ASSIGN CROWN RATIO FOR NEWLY ESTABLISHED TREES.

             CR = 0.89722 - 0.0000461*PCCF(IPCCF)
             1 continue
             RAN = BACHLO(0.0,1.0,RANN)
             if (RAN .lt. -1.0.or. RAN .gt.1.0) go to 1
             CR = CR + 0.07985 * RAN
             if (CR .gt..90) CR = .90
             if (CR .lt..20) CR = .20
             if (VARIANTCD.eq.'NE') then
                ICR(I) = INT((CR*100.0)+0.5)
             else
                ICR(I) = NINT(CR*100.0)
             end if
          end if
          K = I
          L = 0
          H = HT(I)
          if (LSKIPH) then
             HTG(K) = 0.0
             go to 4
          end if

          !     RETURN HERE TO PROCESS NEXT TRIPLE.

          2 continue

          !  ESTIMATE SITE CURVE HEIGHT INCREMENT VALUES FOR INCOMING TREES.

          !  HEIGHT GROWTH EQUATION, USING NC128 TREE HEIGHT COEFFS.
          !  EVALUATED FOR EACH TREE EACH CYCLE, 10.0 YEAR CYCLE
          !  FIRST FIND EFFECTIVE TREE AGE BASED ON H

          MODE0 = 0
          IVAR = IVARN
          YRS = 10.
          H = HT(I)
          if (VARIANTCD.eq.'LS') AGET = ABIRTH(I)
          call HTCALC (MODE0,IVAR,ISPC,SITEAR(ISPC),YRS,H,AGET,HTMAX,&
               &HTG1,JOSTND,DEBUG)

          !  IF INITIAL TREE HEIGHT IS >= MAX HEIGHT FOR SITE (HTMAX).
          !  IF TRUE, THEN SET HTG= 0.1 FT.

          if (HTMAX-H.le.1.) then
             if (DEBUG) write(JOSTND,*) 'HTI>=HTMAX , ABIRTH= ',ABIRTH(I)
             HTGR = 0.10
          else

             !     FIND HTGR

             MODE0 = 9
             IVAR = IVARN
             YRS = 10.
             H = HT(I)
             call HTCALC (MODE0,IVAR,ISPC,SITEAR(ISPC),YRS,H,AGET,HTMAX,&
                  &HTGR,JOSTND,DEBUG)

             if (DEBUG)write(JOSTND,*)'I,ISPC,HGADJ,FNT,ABIRTH= ',&
                  &I,ISPC,HGADJ(ISPC),FNT,ABIRTH(I)
             if (DEBUG)write(JOSTND,*)'I,SCALE,SCALE2,HTGR,DBH,BA,IPCCF= ',&
                  &I,SCALE,SCALE2,HTGR,D,BA,IPCCF

             HTGR = HTGR*CON*SCALE*HGADJ(ISPC)*XRHGRO
          end if

          !  GET BAL MODIFIER BORROWED FROM THE DIAMETER GROWTH EQUATIONS AND
          !  ADJUST THE HEIGHT GROWTH ESTIMATE.
          !  POTENTIALLY ONLY AFFECT HEIGHT GROWTH HALF AS MUCH AS DIA GROWTH
          !  TEMPER THE ADJUSTMENT BY RELATIVE HEIGHT

          if (VARIANTCD.eq.'CS') then
             call BALMOD(ISPC,D,BAL,BA,GMOD,DEBUG)
          else if (VARIANTCD.eq.'LS') then
             call BALMOD(ISPC,D,BA,RMSQD,GMOD,DEBUG)
          else
             call NEBALM(ISPC,D,GMOD)
          end if
          if (VARIANTCD.eq.'LS') then
             if (DEBUG)write(JOSTND,*)'AFTER BALMOD GMOD,HT,AVH= ',&
                  &GMOD,HT(I),AVH
          else
             if (DEBUG)write(16,*)'AFTER BALMOD GMOD,HT,AVH= ',GMOD,HT(I),AVH
          end if
          !      GMOD = (GMOD+1.0)/2.0
          RELHTA = 0.
          if (AVH .gt.0.)RELHTA = MIN((HT(I)/AVH),1.0)
          GMOD = 1.0-((1.0-GMOD)*(1.0-RELHTA))
          HTGR = HTGR*GMOD
          if (DEBUG)write(JOSTND,*)'I,GMOD,HTGR= ',I,GMOD,HTGR

          if (HTGR .lt.0.1) HTGR = 0.1

          !     COMPUTE WEIGHTS FOR THE LARGE AND SMALL TREE HEIGHT INCREMENT
          !     ESTIMATES.  IF DBH IS LESS THAN OR EQUAL TO XMN, THE LARGE TREE
          !     PREDICTION IS IGNORED (XWT=0.0).

          XWT = (D-XMN)/(XMX-XMN)
          if (D.le.XMN .or. LESTB) XWT = 0.0

          !     COMPUTE WEIGHTED HEIGHT INCREMENT FOR NEXT TRIPLE.

          if (DEBUG)write(JOSTND,9982)XWT,HTGR,HTG(K),I,K
          9982 format('IN REGENT 9982 FORMAT',3(F10.4,2X),2I7)

          HTGR = HTGR*(1.0-XWT) + XWT*HTG(K)
          if (HTGR .lt..1) HTGR = .1

          !   ADD RANDOM AFFECTS. LIMIT TO + OR -  1/10 HTGR

          RAN = 0.
          if (DGSD .ge.1.0) then
             3 continue
             RAN = BACHLO(0.0,1.0,RANN)
             if (RAN.lt.-1.0.or. RAN.gt.1.0) go to 3
          end if
          HTGR = HTGR + RAN*0.1*HTGR

          HTG(K) = HTGR
          if (HTG(K) .lt..1) HTG(K) = .1

          if (DEBUG)write(JOSTND,9985)HTGR,HTG(K),I,K,LESTB
          9985 format('IN REGENT HTGR,HTG(K),I,K,LESTB= '/&
               &2(F10.4,2X),2I10,L10)

          ! CHECK FOR SIZE CAP COMPLIANCE.

          if ((H+HTG(K)).gt.SIZCAP(ISPC,4)) then
             HTG(K) = SIZCAP(ISPC,4)-H
             if (HTG(K) .lt.0.1) HTG(K) = 0.1
          end if

          4 continue

          !     ASSIGN DBH AND COMPUTE DBH INCREMENT FOR TREES WITH DBH LESS
          !     THAN 5 INCHES (COMPUTE 10-YEAR DBH INCREMENT REGARDLESS OF
          !     PROJECTION PERIOD LENGTH).

          if (D.ge.XMX) go to 23
          HK = H + HTG(K)
          if (HK .le.4.5) then
             DG(K) = 0.0
             DBH(K) = D+0.001*HK
          else

             !   USE WYKOFF EQ. FOR DBH
             !      DKK = DIAMETER WITH HTG ADDED TO THE STARTING HEIGHT
             !      DK  = DIAMETER AT THE START OF THE PROJECTION

             BX = HT2(ISPC)
             if (IABFLG(ISPC).eq.1) then
                AX = HT1(ISPC)
             else
                AX = AA(ISPC)
             end if
             DKK = (BX/(ALOG(HK-4.5)-AX))-1.0
             if (H .le.4.5) then
                DK = D
             else
                DK = (BX/(ALOG(H-4.5)-AX))-1.0
             end if

             IF(DEBUG)WRITE(JOSTND,*)'I,ISPC,DBH,H,HK,DK,DKK=                &
                  &  ',I,ISPC,DBH(K),H,HK,DK,DKK

             !  USE INVENTORY EQUATIONS IF CALIBRATION OF THE HT-DBH FUNCTION IS TURNED
             !  OFF, OR IF WYKOFF CALIBRATION DID NOT OCCUR.
             !  NOTE: THIS SIMPLIFIES TO IF(IABFLB(ISPC).EQ.1) BUT IS SHOWN IN IT'S
             !        ENTIRITY FOR CLARITY.

             if (.not.LHTDRG(ISPC) .or.&
                  &(LHTDRG(ISPC) .and. IABFLG(ISPC).eq.1)) then
                call HTDBH (IFOR,ISPC,DKK,HK,1)
                if (H .le.4.5) then
                   DK = D
                else
                   call HTDBH (IFOR,ISPC,DK,H,1)
                end if
                if (DEBUG)write(JOSTND,*)'INV EQN DUBBING IFOR,ISPC,H,HK,DK,'&
                     &,'DKK= ',IFOR,ISPC,H,HK,DK,DKK
                if (DEBUG)write(JOSTND,*)'ISPC,LHTDRG,IABFLG= ',&
                     &ISPC,LHTDRG(ISPC),IABFLG(ISPC)
             end if

             !         IF CALLED FROM **ESTAB** ASSIGN DIAMETER

             if (LESTB) then
                DBH(K) = DKK
                if (DBH(K).lt.DIAM(ISPC) .or. HK.lt.4.5) DBH(K) = DIAM(ISPC)
                DBH(K) = DBH(K)+0.001*HK
                DG(K) = DBH(K)
             else

                !         COMPUTE DIAMETER INCREMENT BY SUBTRACTION, APPLY USER
                !         SUPPLIED MULTIPLIERS, AND CHECK TO SEE IF COMPUTED VALUE
                !         IS WITHIN BOUNDS.
                !         THE INCREMENT FOR TREES BETWEEN 1.5 AND 5 INCHES DBH IS A
                !         WEIGHTED AVERAGE OF PREDICTIONS FROM THE LARGE AND SMALL
                !         TREE MODELS. SCALE ADJUSTMENT IS ON GROWTH IN DDS TERMS RATHER
                !         THAN INCHES OF DG TO MAINTAIN CONSISTENCY WITH GRADD.

                !         NOTE: LARGE TREE DG IS ON A 10-YR BASIS; SMALL TREE DG IS ON A
                !         FINT-YR BASIS. CONVERT SMALL TREE DG TO A 10-YR BASIS BEFORE
                !         WEIGHTING. DG GETS CONVERTED BACK TO A FINT-YR BASIS IN **GRADD**.

                BARK = BRATIO(ISPC,D,H)
                if (DEBUG)write(JOSTND,*)'BARK,XRDGRO= ',BARK,XRDGRO
                if (DK.lt.0.0.or. DKK.lt.0.0) then
                   DG(K) = HTG(K)*0.2*BARK*XRDGRO
                   DKK = D+DG(K)
                else
                   DGSM = (DKK-DK)*BARK*XRDGRO
                   if (DGSM .lt.0.0) DGSM = 0.0
                   DDS = DGSM*(2.0*BARK*D+DGSM)*SCALE2
                   DGSM = SQRT((D*BARK)**2+DDS)-BARK*D
                   if (DGSM.lt.0.0) DGSM = 0.0

                   !         COMPUTE WEIGHTED DIAMETER INCREMENT FOR NEXT TRIPLE.

                   if (DEBUG)write(JOSTND,9983)XWT,DGSM,DG(K),I,K
                   9983 format('IN REGENT 9983 FORMAT',3(F10.4,2X),2I7)

                   DGGR = DGSM*(1.0-XWT) + XWT*DG(K)
                   if (DGGR .lt..1) DGGR = .1
                   DG(K) = DGGR
                end if
                if (DEBUG)write(JOSTND,*)'K,DK,DKK,DG= ',K,DK,DKK,DG(K)

                if (DG(K) .lt.0.0) DG(K) = 0.1
                if (DG(K) .gt. DGMX) DG(K) = DGMX

             end if
             if ((DBH(K)+DG(K)).lt.DIAM(ISPC)) then
                DG(K) = DIAM(ISPC)-DBH(K)
             end if
          end if

          !  CHECK FOR TREE SIZE CAP COMPLIANCE

          call DGBND(ISPC,DBH(K),DG(K))

          if (DEBUG) then
             HTNEW = HT(I)+HTG(I)
             write(JOSTND,9987) I,ISPC,HT(I),HTG(I),HTNEW,DBH(I),DG(I),DG(K)
             9987 format('IN REGENT, I=',I4,',  ISPC=',I3,'  CUR HT=',F7.2,&
                  &',  HT INC=',F7.4,',  NEW HT=',F7.2,',  CUR DBH=',F10.5,&
                  &',  DBH INC(I,K)=',2F7.4)
          end if
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
    SCALE3 = REGYR / FINTH

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
       CON = RHCON(ISPC)

       !  BEGIN TREE LOOP WITHIN SPECIES.  IF MEASURED HEIGHT INCREMENT IS
       !  LESS THAN OR EQUAL TO ZERO, OR DBH IS LESS THAN 5.0, THE RECORD
       !  WILL BE EXCLUDED FROM THE CALIBRATION.

       do I3 = I1,I2
          I = IND1(I3)
          if (VARIANTCD.eq.'CS') BAL = (1.0-(PCT(I)/100.))*BA
          H = HT(I)
          K = I
          if (IHTG.lt.2) H = H-HTG(I)
          if (DBH(I).ge.5.0.or.H.lt.0.01) go to 60
          IPCCF = ITRE(I)
          PTCCF = PCCF(IPCCF)

          if (DEBUG) write(JOSTND,*)'IN REGENT 8900 H =',H

          !  CALCULATE TREE HEIGHT INCREMENT
          !  HEIGHT GROWTH EQUATION, USING NC128 TREE HEIGHT COEFFS.
          !  EVALUATED FOR EACH TREE EACH CYCLE, 5.0 YEAR CYCLE
          !  FIRST FIND EFFECTIVE TREE AGE BASED ON H

          MODE0 = 0
          IVAR = IVARN
          YRS = 10.
          H = HT(I)
          if (VARIANTCD.eq.'LS') AGET = ABIRTH(I)
          call HTCALC (MODE0,IVAR,ISPC,SITEAR(ISPC),YRS,H,AGET,HTMAX,&
               &HTG1,JOSTND,DEBUG)

          !     THEN FIND HTGR

          MODE0 = 9
          IVAR = IVARN
          YRS = 10.
          H = HT(I)
          call HTCALC (MODE0,IVAR,ISPC,SITEAR(ISPC),YRS,H,AGET,HTMAX,&
               &HTGR,JOSTND,DEBUG)

          !  GET BAL MODIFIER BORROWED FROM THE DIAMETER GROWTH EQUATIONS AND
          !  ADJUST THE HEIGHT GROWTH ESTIMATE.
          !  POTENTIALLY ONLY AFFECT HEIGHT GROWTH HALF AS MUCH AS DIA GROWTH
          !  TEMPER THE ADJUSTMENT BY RELATIVE HEIGHT

          if (VARIANTCD.eq.'CS') then
             call BALMOD(ISPC,DBH(I),BAL,BA,GMOD,DEBUG)
          else if (VARIANTCD.eq.'LS') then
             D = DBH(I)
             call BALMOD(ISPC,DBH(I),BA,RMSQD,GMOD,DEBUG)
          else
             call NEBALM(ISPC,DBH(I),GMOD)
          end if
          if (DEBUG)write(16,*)'AFTER BALMOD GMOD,HT,AVH= ',GMOD,HT(I),AVH
          !      GMOD = (GMOD+1.0)/2.0
          RELHTA = 0.
          if (AVH .gt.0.)RELHTA = MIN((HT(I)/AVH),1.0)
          GMOD = 1.0-((1.0-GMOD)*(1.0-RELHTA))
          HTGR = HTGR*GMOD
          if (DEBUG)write(JOSTND,*)'I,GMOD,HTGR= ',I,GMOD,HTGR

          if (HTGR .lt..1) HTGR = .1

          EDH = HTGR * RHCON(ISPC)
          if (EDH .lt.0.1) EDH = 0.1
          P = PROB(I)
          if (HTG(I).lt.0.001) go to 60
          TERM = HTG(I) * SCALE3
          SNP = SNP+P
          SNX = SNX+EDH*P
          SNY = SNY+TERM*P
          N = N+1

          !  PRINT DEBUG INFO IF DESIRED.

          if (DEBUG) then
             if (VARIANTCD.eq.'NE') then
             write(JOSTND,9997) NPLT,I,ISPC,H,DBH(I),ICR(I),&
                  &PCT(I),RELDM1,RHCON(ISPC),EDH,TERM,HTGR,SCALE3,HGADJ(ISPC),&
                  &ICR(I)
             else
             write(JOSTND,9991) NPLT,I,ISPC,H,DBH(I),ICR(I),&
                  &PCT(I),RELDM1,RHCON(ISPC),EDH,TERM,HTGR,SCALE3,HGADJ(ISPC),&
                  &ICR(I)
             end if
          end if
          9991 format('NPLT=',A8,',  I=',I5,',  ISPC=',I3,',  H=',F6.1,&
               &',  DBH=',F5.1,',  ICR',I5,',  PCT=',F6.1,',  RELDEN=',&
               &F6.1 / 12X,'RHCON=',F10.3,',  EDH=',F10.3,', TERM=',F10.3,&
               &' HTGR= ',F10.3,' SCALE3= ',F10.3,' HGADJ(ISPC)= ',F10.3/&
               &' ICR= ',I5)
          9997 format('NPLT=',A8,',  I=',I5,',  ISPC=',I3,',  H=',F6.1,&
               &',  DBH=',F5.1,',  ICR',I5,',  PCT=',F6.1,',  RELDEN=',&
               &F6.1 / 12X,'RHCON=',F10.3,',  EDH=',F10.3,', TERM=',F10.3,&
               &' HTGR= ',F10.3,' SCALE3= ',F10.3,' HGADJ(ISPC)= ',F10.3/&
               &'ICR= ',I5)

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

       call REGHCAL(ISPC,SNP,SNX,SNY,CORNEW)
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

    write(JOSTND,9994) (CORTEM(I),I = 1,NUMSP)
    9994 format(/'INITIAL SCALE FACTORS FOR THE SMALL TREE'/&
         &'HEIGHT INCREMENT MODEL',&
         &((T48,11(F5.2,1X)/)))

    ! OUTPUT CALIBRATION TO DATABASE.

    call REGCALOUT(CORTEM,NUMCAL,KOUT,K,KK,ISPEC,SPEC)
    91 if (DEBUG)write(JOSTND,9995)ICYC
    9995 format('LEAVING SUBROUTINE REGENT  CYCLE =',I5)
    return
    9998 continue

    !  ENTRY POINT FOR LOADING OF REGENERATION GROWTH MODEL
    !  CONSTANTS  THAT REQUIRE ONE-TIME RESOLUTION.
    !  IF THE BUDWIDTH VALUES IN THE DIAM ARRAY, BELOW, ARE MODIFIED
    !  THEN ALSO MODIFY THE VALUES IN THE SNDBAL ARRAY IN **HTDBH


    do ISPC = 1,MAXSP
       RHCON(ISPC) = 1.0
       if (LRCOR2.and.RCOR2(ISPC).gt.0.0)&
            &RHCON(ISPC) = RCOR2(ISPC)
       90 continue
    end do
    return
  end subroutine REGCLS

  !===============================!
  !   Compute small tree growth   !
  !===============================!
  ! OP $Id$
  subroutine REGPAC(LCON)
    implicit none

    logical :: LCON

    !  THIS SUBROUTINE COMPUTES HEIGHT AND DIAMETER INCREMENTS FOR
    !  SMALL TREES.  THE **SMHTGF ROUTINE IS CALLED TO CALCULATE
    !  HEIGHT INCREMENT.  THE MODEL IS APPLIED TO TREES
    !  THAT ARE LESS THAN  5 INCHES DBH,
    !  AND THE DBH INCREMENT MODEL IS APPLIED TO TREES THAT ARE LESS
    !  THAN 3 INCHES DBH.  FOR TREES THAT ARE GREATER THAN 3 INCHES
    !  DBH, HEIGHT INCREMENT PREDICTIONS
    !  ARE AVERAGED WITH THE PREDICTIONS FROM THE LARGE TREE MODEL.
    !  THIS ROUTINE IS CALLED FROM **CRATET** DURING CALIBRATION AND
    !  FROM **TREGRO** DURING CYCLING.  ENTRY **REGCON** IS CALLED FROM
    !  **RCON** TO LOAD MODEL PARAMETERS THAT NEED ONLY BE RESOLVED ONCE.
    !  **SMHGDG IS CALLED TO CALCULATE SMALL TREE HEIGHT AND DIAMETER
    !  INCREMENT

    ! Common blocks

    !  DIMENSIONS FOR INTERNAL VARIABLES:

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
    !     DIAM -- BUD WIDTH FOR SEEDLINGS, MINIMUM DBH OF ESTAB TREES.

    external RANN
    ! Variables
    logical :: DEBUG,LSKIPH
    real    :: CORTEM(39)
    integer :: NUMCAL(39)
    real    :: XMAX(39),XMIN(39)
    real    :: DGMAX(39),DIAM(39),DGMIN(39)
    integer :: IRDMAP(39)
    real    :: AB(6)
    integer :: IREFI,ISPEC,KOUT,KK
    real    :: H,BARK,HTGR,ZZRAN,XWT
    real    :: CON,XMX,XMN,DGMX,D,CR,RAN,BACHLO,BRATIO,HK,DK,DKK
    real    :: DDS,HTNEW,SCALE3,CORNEW,SNP,SNX,SNY,EDH,P,TERM
    real    :: REGYR,FNT,SCALE,SCALE2,CCF,AVHT,XRHGRO,XRDGRO
    integer :: ISPC,I1,I2,I3,I,IPCCF,K,L,N,MODE
    real    :: DDUM
    character(len=2) :: SPEC

    real    :: AX,BX,DGSM,DGR,DGR5,HG5,LTHG,XDWT

    ! IRDMAP IS A POINTER TO THE DIAMETER EQUATION COEF FOR EACH SPEC

    ! Data statements
    ! WH USED 10 IN IRDMAP.  BAD EQUATION. CHANGED TO 11. 2/8/95. GED.
    ! LP, SP, WP, LL, WB, KP, PY USED 6. CHANGED TO 5.  5/19/95. GD.
    ! PP & JP USED 7 IN IRDMAP.  BAD EQN. CHANGED TO 5. 5/19/95. GD.
    ! ALL THE ABOVE EQNS WERE UNDERESTIMATING SMALL TREE DG.

    data IRDMAP/&
         !         SF  WF  GF  AF  RF   DUM  NF  YC   IC   ES  LP  JP  SP
         &1,  2,  2,  3,  3,    3,  5,  4,   4,   4,  5,  5,  5,&
         !         WP   PP  DF  RW  RC   WH  MH  BM   RA  MA  TO   GC   AS
         &5,  5,  8,  8,   9,  11, 11, 12,  12, 12, 12,  12,  12,&
         !         CO   WO  J   LL  WB   KP  PY   DG   HW  BC  WI  DUM  DUM
         &12,  12, 4,   5,  5,   5,  5,  12,  12,  12, 12,  12, 12  /
    data AB/ 1.11436, -0.011493, 0.43015E-4,&
         &-0.72221E-7, 0.5607E-10, -0.1641E-13 /
    data DGMAX/ 39*5.0 /
    data XMAX/ 10*4.0,3.0,5*4.0,10.0,22*4.0 /
    data XMIN/ 10*2.0,1.0,28*2.0 /
    data REGYR/5.0 /
    data DIAM/ 7*0.3, 2*0.2, 0.3, 4*0.4, 0.5, 0.3, 13*0.2,&
         &0.3, 2*0.4, 7*0.2 /
    data DGMIN/ 16*3.0, 7.0, 22*3.0 /

    if (LCON) go to 9998

    if (VARIANTCD.ne.'OP') REGYR = 10.0

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

    !  FOR SPECIES/TREES USING FVS GROWTH ESTIMATES:
    !    HEIGHT INCREMENT IS COMPUTED IN **SMHGDG** AND IS BASED ON A 5-YEAR
    !    GROWTH PERIOD.
    !    SCALE IS USED TO CONVERT THE 5-YR HTG ESTIMATE TO A FINT-YR BASIS.

    !    DIAMETER INCREMENT IS PREDICTED FROM EQUATIONS IN **SMHGDG** AND
    !    IS ON A 5-YR BASIS. IT DOES NOT NEED TO BE SCALED.
    !    DIAMETER INCREMENT IS CONVERTED TO A FINT-YR BASIS IN **UPDATE**.

    !  FOR SPECIES/TREES USING ORGANON GROWTH ESTIMATES:
    !    USE THE LARGE TREE HEIGHT INCREMENT COMPUTED IN **HTGF**. THIS HAS
    !    ALREADY BEEN SCALED TO A FINT-YR BASIS.

    !    USE THE LARGE TREE DIAMETER INCREMENT COMPUTED IN **DGDRIV**. THIS
    !    IS ALREADY ON A 5-YEAR BASIS AND DOES NOT NEED TO BE SCALED.
    !    DIAMETER INCREMENT WILL BE CONVERTED TO A FINT-YR BASIS IN **UPDATE**.

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

       !     PUT A CEILING ON DIAMETER GROWTH BASED ON OLIVER AND COCHRAN EMPR.
       !     RJ 11/28/88

       DGMX = DGMAX(ISPC) * SCALE

       !  PROCESS NEXT TREE RECORD.

       do I3 = I1,I2
          I = IND1(I3)
          IPCCF = ITRE(I)
          D = DBH(I)
          if (D .ge. XMX) go to 25

          !  SET DDUM = 1 IF THIS IS A MANAGED STAND
          !  BYPASS INCREMENT CALCULATIONS IF CALLED FROM ESTAB AND THIS IS NOT A
          !  NEWLY CREATED TREE.

          DDUM = 0.0
          if (MANAGD .eq.1) DDUM = 1.0
          if (LESTB) then
             if (I.lt.ITRNIN) go to 25

             !  SET ORGANON VALID TREE FLAG

             if (VARIANTCD.eq.'OP') IORG(I) = 0

             !  ASSIGN CROWN RATIO FOR NEWLY ESTABLISHED TREES.
             !  SET THE DUMMY FOR DIA LOOKUP TO 1 (= MGT STAND)

             DDUM = 1.0
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
          H = HT(I)
          BARK = BRATIO(ISPC,D,H)
          if (LSKIPH) then
             DGR = 0.0
             HTG(K) = 0.0
             go to 4
          end if

          !     RETURN HERE TO PROCESS NEXT TRIPLE.

          2 continue

          !  CALL SMHGDG, THE SMALL TREE HEIGHT AND DIAMETER GROWTH ROUTINE.
          !  SMHGDG IS ALSO CALLED FROM ESSUBH TO GROW PLANTED TREES
          !  FROM ESTABLISHMENT TO 5 YEARS INTO THE CYCLE.

          !  NOTE: CAN NOT PASS K (TRIPLED RECORD) BECAUSE SMHGDG USES POINT
          !  STATISTICS THAT HAVE NOT BEEN CALCULATED FOR RECORDS TRIPLED IN
          !  **REGENT**.

          MODE = 1
          HG5 = 0.
          DGR5 = 0.
          call SMHGDG(I,ISPC,H,D,HG5,DGR5,ICYC,JOSTND,DEBUG,MODE)
          HTGR = HG5
          DGR = DGR5
          HK = H+HTGR
          DK = D+DGR
          ! PN/WC retain two successive five-year predictions.
          if (VARIANTCD.eq.'OP') then
          if (DEBUG) write(JOSTND,*)' AFTER SMHGDG K,ISPC,D,H,HTGR,DGR,',&
               &'HK,DK = ',K,ISPC,D,H,HTGR,DGR,HK,DK
          else
          if (DEBUG) write(JOSTND,*)'1ST-K = ',K,',    ISPC = ',ISPC,&
               &',    HK = ',HK, ',    DK = ',DK, ',    HTGR = ',HTGR,&
               &',    DGR = ',DGR, ',    FINT = ',FINT

          call SMHGDG(I,ISPC,HK,DK,HG5,DGR5,ICYC,JOSTND,DEBUG,MODE)
          HTGR = HTGR+HG5
          DGR = DGR+DGR5
          HK = H+HTGR
          DK = D+DGR

          if (DEBUG) write(JOSTND,*)' 2ND-K = ',K,',    ISPC = ',ISPC,&
               &',    HK = ',HK, ',    DK = ',DK,',    HTGR = ',HTGR,&
               &',    DGR = ',DGR

          end if
          3 continue
          ZZRAN = 0.0
          if (DGSD.ge.1.0) ZZRAN = BACHLO(0.0,1.0,RANN)
          if ((ZZRAN .gt.0.5) .or. (ZZRAN .lt. -2.0)) go to 3

          if (VARIANTCD.eq.'OP') then
          if (DEBUG)write(JOSTND,*)' ZZRAN,XRHGRO,SCALE,FNT,CON,WK4(I)= ',&
               &ZZRAN,XRHGRO,SCALE,FNT,CON,WK4(I)
          else
          if (DEBUG)write(JOSTND,*)' ZZRAN = ',ZZRAN,',    XRHGRO = ',&
               &XRHGRO,',    SCALE = ',SCALE,',    FNT = ',FNT,&
               &',    CON = ',CON,',    WK4(I)= ',WK4(I)
          end if

          ! WK4(I) IS THE CLIMATE MODIFIER LOADED IN **DGDRIV** FOR THIS TREE RECORD
          ! CON IS A COMBINATION OF HCOR (SMALL TREE CALIBRATION FACTOR) AND RCOR2
          ! (MULTIPLIER ENTERED WITH THE READCORR KEYWORD.
          ! XRHGRO IS THE MULTIPLIER ENTERED WITH THE REGHMULT KEYWORD

          HTGR = (HTGR+ZZRAN*0.1)*XRHGRO*SCALE*CON*WK4(I)
          if (HTGR .lt.0.1) HTGR = 0.1

          !     COMPUTE WEIGHTS FOR THE LARGE AND SMALL TREE HEIGHT INCREMENT
          !     ESTIMATES.  IF DBH IS LESS THAN OR EQUAL TO XMN, THE LARGE TREE
          !     PREDICTION IS IGNORED (XWT=0.0).

          XWT = (D-XMN)/(XMX-XMN)
          if (D.le.XMN .or. LESTB) XWT = 0.0

          !  IF THIS IS A VALID ORGANON TREE, HTG WAS SET IN HTGF

          if (VARIANTCD.eq.'OP') then
            if (LORGANON .and. (IORG(K) .eq.1)) XWT = 1.0
          end if

          !     COMPUTE WEIGHTED HEIGHT INCREMENT FOR NEXT TRIPLE.

          if (ISPC .eq.17) then
             LTHG = HTG(K) ! LTHG USED TO TRACK VALUE PRE MODIFICATION
             if (.not. LESTB) HTGR = (HTGR+HTG(K))/2.0

             HTG(K) = HTGR*(1.0-XWT) + XWT*HTG(K)

             if (DEBUG)write(JOSTND,*)'IN REGENT - RW DEBUG: ',&
                  &' LESTB=', LESTB,' D=',D,' XWT=',XWT,' HTGR=',HTGR,&
                  &' LTHG=',LTHG,' HTG FINAL=', HTG(K)
          end if

          HTG(K) = HTGR*(1.0-XWT) + XWT*HTG(K)
          if (HTG(K) .lt..1) HTG(K) = .1

          if (DEBUG)write(JOSTND,*)' XWT = ',XWT,',    HTGR = ',HTGR,&
               &',    HTG(K) = ',HTG(K),',    I = ',I,',    K = ',K

          ! CHECK FOR SIZE CAP COMPLIANCE.

          if ((H+HTG(K)).gt.SIZCAP(ISPC,4)) then
             HTG(K) = SIZCAP(ISPC,4)-H
             if (HTG(K) .lt.0.1) HTG(K) = 0.1
          end if

          4 continue

          !     ASSIGN DBH AND COMPUTE DBH INCREMENT FOR TREES WITH DBH LESS
          !     THAN 3 INCHES (COMPUTE 5-YEAR DBH INCREMENT REGARDLESS OF
          !     PROJECTION PERIOD LENGTH).

          !  IF THIS IS A VALID ORGANON TREE, DG WAS SET IN DGDRIV AND IS ALREADY
          !  ON A 5-YR BASIS

          if (VARIANTCD.eq.'OP') then
            if (LORGANON .and. (IORG(K) .eq.1)) go to 23
          end if

          if (D.ge.DGMIN(ISPC)) go to 23
          HK = H + HTG(K)

          if (DEBUG)write(JOSTND,*)' K = ',K,',    HK = ',HK,&
               &',    H = ',H,',    HTG(K)= ',HTG(K)

          if (HK .lt.4.5) then
             DG(K) = 0.0
             DBH(K) = D+0.001*HK
          else

             !  SCALE DG TO A FNT-YR BASIS SINCE DDS IS SCALED TO A 5-YEAR BASIS BELOW
             !  ALSO INCLUDE CLIMATE EFFECTS

             if (ISPC.eq.17) then
                !       WYKOFF EQUATION
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

                if (DEBUG) then
                   write(JOSTND,*)'WKY EQN DUBBING: ISPC = ', ISPC,&
                        &',    H = ',H,',    HK = ', HK,',    DK = ', DK,&
                        &',    DKK= ',DKK
                   write(JOSTND,*)'ISPC = ', ISPC, ',    LHTDRG = ',&
                        &LHTDRG(ISPC),',    IABFLG= ',IABFLG(ISPC)
                end if

                !       CURTIS ARNEY EQUATION
                if (.not.LHTDRG(ISPC) .or.&
                     &(LHTDRG(ISPC) .and. IABFLG(ISPC).eq.1)) then
                   call HTDBH (IFOR,ISPC,DK,HK,1)

                   if (H .le.4.5) then
                      DKK = D
                   else
                      call HTDBH (IFOR,ISPC,DKK,H,1)
                   end if

                   if (DEBUG) then
                      write(JOSTND,*)'INVCA EQN DUBBING: IFOR = ',IFOR,&
                           &',    ISPC = ',ISPC, ',    H = ',H, ',    HK = ',HK,&
                           &',    DK = ',DK, ',    DKK = ',DKK

                      write(JOSTND,*)'ISPC = ', ISPC, ',    LHTDRG = ',&
                           &LHTDRG(ISPC),',    IABFLG= ',IABFLG(ISPC)
                   end if
                end if

             else
                DG(K) = DGR*SCALE*WK4(I)
             end if

             !       IF CALLED FROM **ESTAB** ASSIGN DIAMETER
             !       REDWOOD IS HANDLED IN A DIFFERENT WAY THAN
             !       OTHER SPECIES IN OP.

             if (LESTB) then
                if (ISPC .eq.17) then
                   DBH(K) = DK
                   if (DBH(K).lt.DIAM(ISPC)) DBH(K) = DIAM(ISPC)
                   DBH(K) = DBH(K)+0.001*HK
                   DG(K) = DBH(K)
                else
                   DBH(K) = DG(K)
                   if (DBH(K).lt.DIAM(ISPC) .or. HK.lt.4.5) DBH(K) = DIAM(ISPC)
                end if
             else

                !         APPLY USER SUPPLIED MULTIPLIERS, AND CHECK TO SEE IF
                !         COMPUTED VALUE IS WITHIN BOUNDS.

                BARK = BRATIO(ISPC,D,H)

                if (DEBUG)write(JOSTND,*)' BARK = ',BARK,&
                     &',    XRDGRO = ',XRDGRO

                if ((D.lt.0.).or.(DG(K).lt.0.)) then
                   DG(K) = HTG(K)*0.2*BARK*XRDGRO
                   DBH(K) = D+DG(K)
                else
                   if (ISPC.eq.17) then
                      XDWT = (D-XMN)/(7.0-XMN)
                      if (D.le.XMN) XDWT = 0.0
                      DGSM = (DK-DKK)*BARK*XRDGRO
                      if (DGSM .lt.0.0) DGSM = 0.0
                      DDS = DGSM*(2.0*BARK*D+DGSM)*SCALE2
                      DGSM = SQRT((D*BARK)**2+DDS)-BARK*D
                      if (DEBUG)write(JOSTND,*)'IN REGENT - RW/GS DEBUG',&
                           &' D=',D,' XDWT=',XDWT,' DKK=', DKK,' DK=',DK,&
                           &' BARK=', BARK,' DGSM=',DGSM,' LTDG=', DG(K)
                      DG(K) = DGSM*(1.0-XDWT)+DG(K)*XDWT
                      if (DEBUG)write(JOSTND,*)'IN REGENT - RW/GS DEBUG',&
                           &' DGSM FINAL=', DG(K)
                      go to 200
                   end if
                   DG(K) = DG(K)*BARK*XRDGRO
                end if

                if (DEBUG)write(JOSTND,*)' K = ',K,',    DBH(K) = ',DBH(K),&
                     &',    DG(K) = ',DG(K)

                if (DG(K) .lt.0.0) DG(K) = 0.1
                if (DG(K) .gt. DGMX) DG(K) = DGMX

                !         SCALE DIAMETER INCREMENT TO 5-YR ESTIMATE.
                !         SCALE ADJUSTMENT IS ON GROWTH IN DDS RATHER THAN INCHES
                !         OF DG TO BE CONSISTENT WITH GRADD.

                DDS = DG(K)*(2.0*BARK*D+DG(K))*SCALE2
                DG(K) = SQRT((D*BARK)**2+DDS)-BARK*D
             end if
             200 continue
             if ((DBH(K)+DG(K)).lt.DIAM(ISPC)) then
                DG(K) = DIAM(ISPC)-DBH(K)
             end if
          end if

          if (DEBUG) then
             write(JOSTND,*)' SCALE2 = ',SCALE2,',    SCALE = ',SCALE,&
                  &',    YR = ',YR,',    FNT = ',FNT,',    FINT = ',FINT

             HTNEW = HT(K)+HTG(K)

             write(JOSTND,9987) K,ISPC,HT(K),HTG(K),HTNEW,DBH(K),DG(K)
             9987 format(' IN REGENT, K=',I4,',  ISPC=',I3,'  CUR HT=',F7.2,&
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
       CON = RHCON(ISPC)

       !  BEGIN TREE LOOP WITHIN SPECIES.  IF MEASURED HEIGHT INCREMENT IS
       !  LESS THAN OR EQUAL TO ZERO, OR DBH IS LESS THAN 5.0, THE RECORD
       !  WILL BE EXCLUDED FROM THE CALIBRATION.

       do I3 = I1,I2
          I = IND1(I3)
          H = HT(I)
          D = DBH(I)

          !  DIA GT 3 INCHES INCLUDED IN OVERALL MEAN

          if (IHTG.lt.2) H = H-HTG(I)
          if (DBH(I).ge.5.0.or.H.lt.0.01) go to 60

          ! CALL SMHGDG, THE SMALL TREE HEIGHT AND DIAMETER GROWTH ROUTINE
          ! IT PREDICTS A 5 YEAR GROWTH INCREMENT SO CALL 2 TIMES

          MODE = 1
          HG5 = 0.
          DGR5 = 0.
          call SMHGDG(I,ISPC,H,D,HG5,DGR5,ICYC,JOSTND,DEBUG,MODE)
          HTGR = HG5
          DGR = DGR5
          HK = H+HTGR
          DK = D+DGR

          if (DEBUG) write(JOSTND,*)'1ST-K = ',K,',    ISPC = ',ISPC,&
               &',    HK = ',HK,',    DK = ',DK,',    HTGR = ',HTGR,&
               &',    DGR = ',DGR,',    FINT = ',FINT

          call SMHGDG(I,ISPC,HK,DK,HG5,DGR5,ICYC,JOSTND,DEBUG,MODE)
          HTGR = HTGR+HG5
          DGR = DGR+DGR5
          HK = H+HTGR
          DK = D+DGR

          if (DEBUG) write(JOSTND,*)'2ND-K = ',K,',    ISPC = ',ISPC,&
               &',    HK = ',HK,',    DK = ',DK,',    HTGR = ',HTGR,&
               &',    DGR = ',DGR,',    SCALE3 = ',SCALE3

          EDH = HTGR*RHCON(ISPC)
          if (EDH .lt.0.1) EDH = 0.1

          if (DEBUG)write(JOSTND,9990) EDH
          9990 format(' IN REGENT-EDH = ',2F10.4)

          P = PROB(I)
          if (HTG(I).lt.0.001) go to 60
          TERM = HTG(I) * SCALE3
          SNP = SNP+P
          SNX = SNX+EDH*P
          SNY = SNY+TERM*P
          N = N+1

          !  PRINT DEBUG INFO IF DESIRED.

          if (DEBUG)write(JOSTND,9991) NPLT,I,ISPC,H,DBH(I),ICR(I),&
               &PCT(I),ATCCF,RHCON(ISPC),EDH,TERM,SCALE3,FINTH
          9991 format('NPLT=',A26,',  I=',I5,',  ISPC=',I3,',  H=',F6.1,&
               &',  DBH=',F5.1,',  ICR',I5,',  PCT=',F6.1,',  RELDEN=',&
               &F6.1 / 13X,'RHCON=',F10.3,',  EDH=',F10.3,', TERM=',F10.3,&
               &' SCALE3= ',F6.1,' FINTH= ',F6.1)

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

       call REGHCAL(ISPC,SNP,SNX,SNY,CORNEW)
       80 continue
       CORTEM(IREFI) = CORNEW
       NUMCAL(IREFI) = N
       100 continue
    end do

    !  END OF CALIBRATION LOOP.  PRINT CALIBRATION STATISTICS AND RETURN

    if (VARIANTCD.eq.'OP') then
       write(JOSTND,9993) (NUMCAL(I),I = 1,NUMSP)
    else
       write(JOSTND,9996) (NUMCAL(I),I = 1,NUMSP)
    end if
    9993 format(/'NUMBER OF RECORDS AVAILABLE FOR SCALING'/&
         &'THE SMALL TREE HEIGHT INCREMENT MODEL',&
         &((T49,11(I4,2X)/)))
    95 continue
    9996 format(/'NUMBER OF RECORDS AVAILABLE FOR SCALING'/&
         &'THE SMALL TREE HEIGHT INCREMENT MODEL',&
         &((T48,11(I4,2X)/)))
    write(JOSTND,9994) (CORTEM(I),I = 1,NUMSP)
    9994 format(/'INITIAL SCALE FACTORS FOR THE SMALL TREE'/&
         &'HEIGHT INCREMENT MODEL',&
         &((T48,11(F5.2,1X)/)))

    ! OUTPUT CALIBRATION TO DATABASE.

    call REGCALOUT(CORTEM,NUMCAL,KOUT,K,KK,ISPEC,SPEC)
    91 if (DEBUG)write(JOSTND,9995)ICYC
    9995 format('LEAVING SUBROUTINE REGENT  CYCLE =',I5)

    if (VARIANTCD.eq.'OP') then
    ! IF DEBUGGING, WRITE RESULTS TO THE OUTPUT FILE

    if (DEBUG) then
       do I = 1,ITRN
          write(JOSTND,*)' LEAVING REGENT I,ISP,DBH,DG,HT,HTG,IORG= ',&
               &I,ISP(I),DBH(I),DG(I),HT(I),HTG(I),IORG(I)
       end do
    end if

    end if
    return

    9998 continue

    !  ENTRY POINT FOR LOADING OF REGENERATION GROWTH MODEL
    !  CONSTANTS  THAT REQUIRE ONE-TIME RESOLUTION.

    do ISPC = 1,MAXSP
       RHCON(ISPC) = 1.0
       if (LRCOR2.and.RCOR2(ISPC).gt.0.0)&
            &RHCON(ISPC) = RCOR2(ISPC)
       90 continue
    end do
    return
  end subroutine REGPAC

  !===============================!
  !   Compute small tree growth   !
  !===============================!
  ! AK $Id$
  subroutine REGAK(LCON)
    implicit none

    logical :: LCON
    ! Regional callees retain their native argument lists.
    interface
       subroutine HTCALC(N,ISPC,AGET,H,HTMAX,HTG,JOSTND,DEBUG)
         integer :: N
         integer :: ISPC
         real :: AGET
         real :: H
         real :: HTMAX
         real :: HTG
         integer :: JOSTND
         logical :: DEBUG
       end subroutine HTCALC
    end interface


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
    logical :: DEBUG,LSKIPH

    character(len=2) :: SPEC

    integer :: I,I1,I2,I3,IICR,IP,IREFI,ISPC,ISPEC
    integer :: K,KK,KOUT,L,N,MODE0,MODE2
    integer :: NUMCAL(23)

    real    :: BACHLO,BARK,BRATIO,CON,CORNEW,D,DDS,DK,DKK,EDH
    real    :: FNT,H,HK,HTGR,HTNEW,P,RANC,RCR
    real    :: REGYR,RSI,SCALE1,SCALE2,SCALE3,SNP,SNX,SNY
    real    :: TERM,XMN,XMX,XRDGRO,XRHGRO,ZZRAN
    real    :: CR,BX,AX
    real    :: DGSM,DGLT,LTHG,XWT,XDWT,AGET,HTMAX
    real    :: CORTEM(23),DIAM(23),XMIN(23),XMAX(23)
    real    :: DGMAX(23)

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

    if (LCON) go to 9998

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
                DGSM = SQRT((D*BARK)**2+DDS)-BARK*D
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

       call REGHCAL(ISPC,SNP,SNX,SNY,CORNEW)
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

    call REGCALOUT(CORTEM,NUMCAL,KOUT,K,KK,ISPEC,SPEC)
    91 if (DEBUG)write(JOSTND,9995)ICYC
    9995 format('LEAVING SUBROUTINE REGENT  CYCLE =',I5)
    return
    9998 continue

    !  ENTRY POINT FOR LOADING OF REGENERATION GROWTH MODEL
    !  CONSTANTS  THAT REQUIRE ONE-TIME RESOLUTION.

    do ISPC = 1,MAXSP
       RHCON(ISPC) = 1.0
       if (LRCOR2.and.RCOR2(ISPC).gt.0.0) &
            & RHCON(ISPC) = RCOR2(ISPC)
       90 continue
    end do
    return
  end subroutine REGAK

  !===============================!
  !   Compute small tree growth   !
  !===============================!
  ! BM $Id$
  subroutine REGBM(LCON)
    implicit none

    logical :: LCON
    ! Regional callees retain their native argument lists.
    interface
       subroutine SMHTGF(I,HHT,H,MODE,DTIME,ICYC,JOSTND,DEBUG)
         integer :: I
         real :: HHT
         real :: H
         integer :: MODE
         real :: DTIME
         integer :: ICYC
         integer :: JOSTND
         logical :: DEBUG
       end subroutine SMHTGF
    end interface


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

    !  VARIABLE DEFINITIONS:

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

    ! Variables
    external RANN

    ! Variables
    logical :: DEBUG,LSKIPH

    character(len=2) :: SPEC

    integer :: I,I1,I2,I3,INDX,IPCCF,IREFI,ISPC,ISPEC,JCR
    integer :: K,KK,KOUT,L,N

    integer :: NUMCAL(18)

    real    :: AG1,AG2,AGMAX,ALHK,ALHT,AVHT,AX,BACHLO,BARK,BKPT,BRATIO
    real    :: BX,CCF,CON,CORNEW,CR,CRCODE,D,D2,DAT45,DDS,DDUM,DGMX,DK
    real    :: DKK,DLESS3,EDH,FNT,H,H2,HITE1,HITE2,HK,HLESS4,HTGR,HTMAX
    real    :: HTMAX2,HTNEW,P,PCTRED,POTHTG,PPCCF,RAN,REGYR,RELSI,RSIMOD
    real    :: SCALE,SCALE2,SCALE3,SHI,SI,SITAGE,SITHT,SLO,SNP,SNX,SNY
    real    :: TEMT,TERM,TPCCF,VIGOR,X,XMN,XMX,XRDGRO,XRHGRO,XWT
    real    :: ZZRAN

    real    :: AB(9),CORTEM(18),DGMAX(18),DIAM(18)
    real    :: RDCON(3),RDCR(3),RDDUM(3),RDHT(3),RDLHT(3)
    real    :: XMAX(18),XMIN(18)

    !  DATA STATEMENTS

    !  SPECIES ORDER:
    !   1=WP,  2=WL,  3=DF,  4=GF,  5=MH,  6=WJ,  7=LP,  8=ES,
    !   9=AF, 10=PP, 11=WB, 12=LM, 13=PY, 14=YC, 15=AS, 16=CW,
    !  17=OS, 18=OH

    !  SPECIES EXPANSION:
    !  WJ USES SO JU (ORIGINALLY FROM UT VARIANT; REALLY PP FROM CR VARIANT)
    !  WB USES SO WB (ORIGINALLY FROM TT VARIANT)
    !  LM USES UT LM
    !  PY USES SO PY (ORIGINALLY FROM WC VARIANT)
    !  YC USES WC YC
    !  AS USES SO AS (ORIGINALLY FROM UT VARIANT)
    !  CW USES SO CW (ORIGINALLY FROM WC VARIANT)
    !  OS USES BM PP BARK COEFFICIENT
    !  OH USES SO OH (ORIGINALLY FROM WC VARIANT)

    ! MAX DIAMETER GROWTHS

    data DGMAX/ 2.8, 2.8, 2.4, 3.6, 2.5, 2.0, 3.5, 3.6, 3.6, 2.8,&
         &2.8, 2.8, 5.0, 5.0, 2.5, 5.0, 2.8, 5.0/

    data XMAX/  3.0, 2.0, 4.0, 4.0, 2.0, 99., 4.0, 4.0, 4.0, 5.0,&
         &3.0, 4.0, 4.0, 4.0, 4.0, 4.0, 5.0, 4.0/&

         &XMIN/  2.0, 1.0, 2.0, 2.0, 1.0, 90., 2.0, 2.0, 2.0, 1.0,&
         &1.5, 2.0, 2.0, 2.0, 2.0, 2.0, 1.0, 2.0/

    data DIAM/  0.4, 0.3, 0.3, 0.3, 0.2, 0.3, 0.4, 0.3, 0.3, 0.5,&
         &0.4, 0.4, 0.2, 0.2, 0.2, 0.2, 0.5, 0.2/

    data AB/&
         &1.11436,-.011493,.43012E-4,-.72221E-7,.5607E-10,-.1641E-13,3*0./

    data REGYR/10.0/

    ! COEFS FOR THE DIA LOOKUP FUNCTION - WC VARIANT SPECIES
    ! 1=PY, 2=YC, 3=CW&OH

    data RDCON/ -2.089, -0.532, 3.102/

    data RDCR/     0.0,    0.0,   0.0/

    data RDLHT/  1.980,  1.531,   0.0/

    data RDHT/     0.0,    0.0, 0.021/

    data RDDUM/    0.0,    0.0,   0.0/

    if (LCON) go to 9998

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

       !  GET THE APPROPRIATE SITE INDEX RANGE VALUES FOR THIS SPECIES.

       SLO = 0.
       SHI = 999.
       call SITERANGE (2,ISPC,SLO,SHI)

       I1 = ISCT(ISPC,1)
       if (I1.eq.0) go to 30
       I2 = ISCT(ISPC,2)
       XRHGRO = XRHMLT(ISPC)
       XRDGRO = XRDMLT(ISPC)
       CON = RHCON(ISPC) * EXP(HCOR(ISPC))
       XMX = XMAX(ISPC)
       XMN = XMIN(ISPC)
       SI = SITEAR(ISPC)
       if (SI .gt. SHI) SI = SHI
       if (SI .le. SLO) SI = SLO + 0.5

       !     PUT A CEILING ON DIAMETER GROWTH BASED ON OLIVER AND COCHRAN EMPR.
       !     RJ 11/28/88
       !     FOR WB, USE TT SETTING

       DGMX = DGMAX(ISPC) * SCALE
       if (ISPC .eq.11)DGMX = FINT*0.2

       !  PROCESS NEXT TREE RECORD.

       do I3 = I1,I2
          I = IND1(I3)
          D = DBH(I)
          if (D .ge. XMX) go to 25
          IPCCF = ITRE(I)

          !  BYPASS INCREMENT CALCULATIONS IF CALLED FROM ESTAB AND THIS IS NOT A
          !  NEWLY CREATED TREE.

          if (LESTB) then
             if (I.lt.ITRNIN) go to 25

             !  ASSIGN CROWN RATIO FOR NEWLY ESTABLISHED TREES.

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

          JCR = INT(real(ICR(I))/10.)
          H = HT(I)
          BARK = BRATIO(ISPC,D,H)
          if (LSKIPH) then
             HTG(K) = 0.0
             go to 4
          end if
          X = real(ICR(I))/100.
          VIGOR = (150.0 * (X**3.0)*EXP(-6.0*X))+0.3
          if (VIGOR .gt.1.0)VIGOR = 1.0

          !  VIGOR ADJUSTMENT TO DRASTIC FOR PINYON; CUT IT BY TWO-THIRDS

          if (ISPC .eq.6)VIGOR = 1.-((1.-VIGOR)/3.)

          !     RETURN HERE TO PROCESS NEXT TRIPLE.

          2 continue

          !  BEGIN POTHTG SECTION

          select case(ISPC)

             ! ORIGINAL BM SPECIES

             ! CALL SMHTGF, THE SMALL TREE HEIGHT GROWTH ROUTINE.
             ! SMHTGF IS ALSO CALLED FROM ESSUBH TO GROW PLANTED TREES
             ! FROM ESTABLISHMENT TO 5 YEARS INTO THE CYCLE

          case(1:11,13:14,16:18)
             TEMT = 10.
             call SMHTGF(ISPC,POTHTG,H,1,TEMT,ICYC,JOSTND,DEBUG)
             if (DEBUG)write(JOSTND,*)'SMHTGF-ICYC,ISPC,H,TEMT,POTHTG= '
             if (DEBUG)write(JOSTND,*)ICYC,ISPC,H,TEMT,POTHTG

             ! LIMBER PINE FROM THE UT VARIANT

          case(12)
             POTHTG = SI/5.0

             !  QUAKING ASPEN

          case(15)
             RELSI = (SI-SLO)/(SHI-SLO)
             RSIMOD = 0.5 * (1.0 + RELSI)

             ! COMPUTE HT GROWTH AND AGE FOR ASPEN. EQN FROM WAYNE SHEPPARD RMRS.

             if (LESTB) then
                SITAGE = ABIRTH(I)
             else

                !  CALL FINDAG TO CALCULATE EFFECTIVE AGE

                SITAGE = 0.0
                SITHT = 0.0
                AGMAX = 0.0
                HTMAX = 0.0
                HTMAX2 = 0.0
                H = HT(I)
                D2 = 0.0
                call FINDAG(I,ISPC,D,D2,H,SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,&
                     &DEBUG)
             end if
             HITE1 = 26.9825 * SITAGE**1.1752
             AG2 = SITAGE+10.0
             HITE2 = 26.9825 * AG2**1.1752
             HTGR = (HITE2-HITE1)/(2.54*12.0) * RSIMOD * CON
             HTGR = HTGR*2.40

             ! GROWTH RATES APPEAR HIGH, REDUCE BY 25 PERCENT. DIXON 8-27-92

             HTGR = HTGR * .75
             go to 3

          end select

          HTGR = POTHTG*PCTRED*VIGOR*CON
          if (DEBUG) write(JOSTND,9983) X,VIGOR,HTGR,CON
          9983 format('IN REGENT X,VIGOR,HTGR,CON = ',4F10.4)
          3 continue
          ZZRAN = 0.0
          if (DGSD.ge.1.0) ZZRAN = BACHLO(0.0,1.0,RANN)
          if ((ZZRAN .gt.0.5) .or. (ZZRAN .lt. -2.0)) go to 3
          if (DEBUG)write(JOSTND,9984) HTGR,ZZRAN,XRHGRO,SCALE
          9984 format('IN REGENT 9984 FORMAT',4(F10.4,2X))
          HTGR = (HTGR +ZZRAN*0.1)*XRHGRO * SCALE

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

          BKPT = 3.
          if (ISPC .eq.6)BKPT = 99.
          if (D.ge.BKPT) go to 23
          HK = H + HTG(K)
          if (HK .le.4.5) then
             DG(K) = 0.0
             DBH(K) = D+0.001*HK
          else
             select case(ISPC)
             case(7)
                DK = (-9.8752/(ALOG(HK-4.5)-4.8656))-1.0
                if (H .le.4.5) then
                   DKK = D
                else
                   DKK = (-9.8752/(ALOG(H -4.5)-4.8656))-1.0
                end if
             case(10,17)
                DK = (HK-8.31485+.59200*7.)/3.03659
                DKK = (H-8.31485+.59200*7.)/3.03659
                if (H .lt.4.5) DKK = D
             case(1:6,8:9,11,12,15)
                BX = HT2(ISPC)
                if (IABFLG(ISPC).eq.1) then
                   AX = HT1(ISPC)
                else
                   AX = AA(ISPC)
                end if
             end select

             !  DIAMETER CALCUALTIONS

             select case(ISPC)

                !  ORIGINAL BM SPECIES EXCEPT LP(7), PP(10), OS(17)
                !  QUAKING ASPEN(15) FROM UT
                !  LIMBER PINE(12) FROM UT

             case(1:5,8:9,12,15)
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

                !  WESTERN JUNIPER FROM UT

             case(6)
                DK = (HK-4.5)*10./(SITEAR(ISPC)-4.5)
                if (DK .lt.0.1) DK = 0.1
                DKK = (H-4.5)*10./(SITEAR(ISPC)-4.5)
                if (DKK .lt.0.1) DKK = 0.1
                if (H .lt.4.5) DKK = D

                !  WHITEBARK PINE FROM TT

             case(11)

                !  COEFFICIENTS FROM EM/SMDGF
                !  ASSIGN DIAMETER INCREMENT AND SCALE FOR BARK THICKNESS AND
                !  PERIOD LENGTH.  SCALE ADJUSTMENT IS ON GROWTH IN DDS RATHER
                !  THAN INCHES OF DG TO MAINTAIN CONSISTENCY WITH GRADD.

                PPCCF = 1.
                TPCCF = PCCF(IPCCF)*PPCCF
                if (TPCCF.gt.300.0) TPCCF = 300.0
                if (TPCCF.lt.25.0) TPCCF = 25.0

                CR = real(ICR(K))
                HLESS4 = H - 4.5
                DLESS3 = 0.000231*HLESS4*CR - 0.00005 * HLESS4 * TPCCF&
                     &+ 0.001711 * CR + 0.17023 * HLESS4
                DKK = DLESS3 + 0.3

                if (DEBUG)write(JOSTND,*)'SMDGF CALL PARMS- ,I,ISPC,H,CR,',&
                     &'TPCCF,DKK= ',I,ISPC,H,real(ICR(K)),TPCCF,DKK

                !  PPCCF IS A PROPORTIONAL ADJUSTMENT FOR POINT CCF VALUES BASED ON
                !  CHANGE IN STAND CCF FOR THE SUBCYCLE.

                HLESS4 = HK - 4.5
                DLESS3 = 0.000231*HLESS4*CR - 0.00005 * HLESS4 * TPCCF&
                     &+ 0.001711 * CR + 0.17023 * HLESS4
                DK = DLESS3 + 0.3

                if (DEBUG)write(JOSTND,*)'SMDGF CALL PARMS- ,I,ISPC,HK,CR,',&
                     &'PCCF(IPCCF),DKK= ',I,ISPC,HK,real(ICR(K)),PCCF(IPCCF),DK

                !  SPECIES FROM WC VARIANT
                !  SET DDUM = 1 IF THIS IS A MANAGED STAND

             case(13,14,16,18)
                DDUM = 0.0
                if (MANAGD.eq.1.or. LESTB) DDUM = 1.0

                !   BEGIN THE DIAMETER LOOKUP SECTION
                !      DK = DIAMETER WITH HTG ADDED TO THE STARTING HEIGHT
                !      DKK  = DIAMETER AT THE START OF THE PROJECTION
                !   DAT45  = DIAMETER AT 4.5 FEET PREDICTED FROM EQUATION.

                CRCODE = real(ICR(K))/10.0
                ALHT = ALOG(H)
                ALHK = ALOG(HK)
                select case (ISPC)
                case(13)
                   INDX = 1
                case(14)
                   INDX = 2
                case(16,18)
                   INDX = 3
                end select
                DAT45 = RDCON(INDX) + RDCR(INDX)*CRCODE&
                     &+ RDLHT(INDX)*ALOG(4.5) + RDHT(INDX)*4.5 + RDDUM(INDX)*DDUM

                DKK = RDCON(INDX) + RDCR(INDX) * CRCODE&
                     &+ RDLHT(INDX)*ALHT + RDHT(INDX)*H  +RDDUM(INDX)*DDUM
                if (DKK .lt.0.0) DKK = D

                DK = RDCON(INDX) + RDCR(INDX) * CRCODE&
                     &+ RDLHT(INDX)*ALHK + RDHT(INDX)*HK + RDDUM(INDX)*DDUM
                if (DK .lt. DKK) DK = DKK+.01
                IF(DEBUG)WRITE(JOSTND,*)'I,INDX,DBH,H,HK,DK,DKK,CRCODE,DDUM=  &
                     &    ',I,INDX,DBH(I),H,HK,DK,DKK,CRCODE,DDUM

             end select
             if (ISPC.eq.6.or. ISPC.eq.11.or. ISPC.eq.12.or. ISPC.eq.15)&
                  &go to 300

             !  USE INVENTORY EQUATIONS IF CALIBRATION OF THE HT-DBH FUNCTION IS TURNED
             !  OFF, OR IF WYKOFF CALIBRATION DID NOT OCCUR.
             !  NOTE: THIS SIMPLIFIES TO IF(IABFLB(ISPC).EQ.1) BUT IS SHOWN IN IT'S
             !        ENTIRITY FOR CLARITY.

             if (.not.LHTDRG(ISPC) .or.&
                  &(LHTDRG(ISPC) .and. IABFLG(ISPC).eq.1)) then
                call HTDBH (IFOR,ISPC,DK,HK,1)
                if (H .le.4.5) then
                   DKK = D
                else
                   call HTDBH (IFOR,ISPC,DKK,H,1)
                end if
                if (DEBUG)write(JOSTND,*)'INV EQN DUBBING IFOR,ISPC,H,HK,DK,'&
                     &,'DKK= ',IFOR,ISPC,H,HK,DK,DKK
                if (DEBUG)write(JOSTND,*)'ISPC,LHTDRG,IABFLG= ',&
                     &ISPC,LHTDRG(ISPC),IABFLG(ISPC)
             end if
             300 continue

             !  IF CALLED FROM **ESTAB** ASSIGN DIAMETER

             if (LESTB) then
                if (ISPC.eq.13.or. ISPC.eq.14.or. ISPC.eq.16.or. ISPC.eq.18)&
                     &then
                   if (DAT45.gt.0.0.and. HK.ge.4.5.and. LHTDRG(ISPC) .and.&
                        &IABFLG(ISPC).eq.0) then
                      DBH(K) = DK - DAT45 + DIAM(ISPC)
                   else
                      DBH(K) = DK
                   end if
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

                ! IF THE TREE JUST REACHED 4.5 FEET, SET DKK TO PRESENT DBH.
                ! RJ 12/6/91
                if (H .lt.4.5 )DKK = D
                BARK = BRATIO(ISPC,D,H)

                if (DEBUG)write(JOSTND,*)'BARK,XRDGRO= ',BARK,XRDGRO

                select case (ISPC)

                   ! FROM WC
                   ! PROBLEM WITH HARDWOOD EQN, REDUCES TO .021*HG. SET TO RULE
                   ! OF THUMB VALUE .1*HG FOR NOW. DIXON 11-04-92
                   ! DON'T USE R.O.T. IF USING INVENTORY EQNS.  DIXON 03-31-98

                case(13,14,16,18)
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
                   if (DK.lt.0.0.or. DKK.lt.0.0) then
                      DG(K) = HTG(K)*0.2*BARK*XRDGRO
                      DK = D+DG(K)
                   else
                      DG(K) = (DK-DKK)*BARK*XRDGRO
                   end if

                end select

                !         SCALE DIAMETER INCREMENT TO 10-YR ESTIMATE.
                !         SCALE ADJUSTMENT IS ON GROWTH IN DDS TERMS RATHER THAN
                !         INCHES OF DG TO BE CONSISTENT WITH GRADD.

                if (DG(K) .lt.0.0) DG(K) = 0.0
                if (DG(K) .gt. DGMX) DG(K) = DGMX
                if (ISPC.eq.11.and. (DBH(K)+DG(K)).lt.DIAM(ISPC)) then
                   DG(K) = DIAM(ISPC)-DBH(K)
                end if

                DDS = DG(K)*(2.0*BARK*D+DG(K))*SCALE2
                DG(K) = SQRT((D*BARK)**2 + DDS)-BARK*D
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
    SCALE3 = REGYR / FINTH

    ! COMPUTE DENSITY MODIFIER FROM CCF AND TOP HEIGHT.

    X = AVH*(RELDEN/100.0)
    if (X .gt.300.0) X = 300.0
    PCTRED = AB(1)&
         &+ X*(AB(2) + X*(AB(3) + X*(AB(4) + X*(AB(5)+ X*AB(6)))))
    if (PCTRED .gt.1.0) PCTRED = 1.0
    if (PCTRED .lt.0.01) PCTRED = 0.01
    if (DEBUG)write(JOSTND,9989)AVH,RELDEN,X,PCTRED
    9989 format('IN REGENT AVH,RELDEN,X,PCTRED = ',4F10.4)

    !  BEGIN PROCESSING TREE LIST IN SPECIES ORDER.  DO NOT CALCULATE
    !  CORRECTION TERMS IF THERE ARE NO TREES FOR THIS SPECIES.

    do ISPC = 1,MAXSP

       !  GET THE APPROPRIATE SITE INDEX RANGE VALUES FOR THIS SPECIES.

       SLO = 0.
       SHI = 999.
       call SITERANGE (2,ISPC,SLO,SHI)

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
       if (SI .gt. SHI) SI = SHI
       if (SI .le. SLO) SI = SLO + 0.5

       !  BEGIN TREE LOOP WITHIN SPECIES.  IF MEASURED HEIGHT INCREMENT IS
       !  LESS THAN OR EQUAL TO ZERO, OR DBH IS LESS THAN 5.0, THE RECORD
       !  WILL BE EXCLUDED FROM THE CALIBRATION.

       do I3 = I1,I2
          I = IND1(I3)
          H = HT(I)
          JCR = INT(real(ICR(I))/10.)
          IPCCF = ITRE(I)

          !  DIA GT 3 INCHES INCLUDED IN OVERALL MEAN

          if (IHTG.lt.2) H = H-HTG(I)
          if (DBH(I).ge.5.0.or.H.lt.0.01) go to 60

          !  COMPUTE VIGOR MODIFIER FROM CROWN RATIO.

          X = real(ICR(I))/100.
          VIGOR = (150.0 * (X**3.0)*EXP(-6.0*X))+0.3
          if (VIGOR .gt.1.0)VIGOR = 1.0

          !  VIGOR ADJUSTMENT TO DRASTIC FOR PINYON, CUT IT BY TWO-THIRDS

          if (ISPC .eq.6)VIGOR = 1.-((1.-VIGOR)/3.)

          !  BEGIN POTHTG SECTION

          select case(ISPC)

             ! QUAKING ASPEN

          case(15)
             RELSI = (SI-SLO)/(SHI-SLO)
             RSIMOD = 0.5 * (1.0 + RELSI)

             ! COMPUTE HT GROWTH AND AGE FOR ASPEN. EQN FROM WAYNE SHEPPARD RMRS.

             AG1 = (H*12.0*2.54/26.9825)**0.8509
             AG2 = AG1 + 10.0
             H2 = (26.9825*AG2**1.1752)/(2.54*12.0)
             EDH = (H2-H) * RSIMOD * RHCON(ISPC)
             EDH = EDH*2.4

             ! GROWTH RATES APPEAR HIGH, REDUCE BY 25 PERCENT. DIXON 8-27-92.

             EDH = EDH*.75
             if (EDH .lt.0.0) EDH = 0.0
             go to 9988

             ! LIMBER PINE FROM THE UT VARIANT

          case(12)
             POTHTG = SI/5.0

             ! ORIGINAL BM SPECIES AND OTHERS (EXCEPT ASPEN AND LIMBER PINE)

          case default

             ! CALL SMHTGF, THE SMALL TREE HEIGHT GROWTH ROUTINE

             call SMHTGF(ISPC,POTHTG,H,1,REGYR,ICYC,JOSTND,DEBUG)

          end select

          EDH = POTHTG*PCTRED*VIGOR*RHCON(ISPC)

          9988 continue
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

       call REGHCAL(ISPC,SNP,SNX,SNY,CORNEW)
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

    call REGCALOUT(CORTEM,NUMCAL,KOUT,K,KK,ISPEC,SPEC)
    91 if (DEBUG)write(JOSTND,9995)ICYC
    9995 format('LEAVING SUBROUTINE REGENT  CYCLE =',I5)
    return

    9998 continue

    !  ENTRY POINT FOR LOADING OF REGENERATION GROWTH MODEL
    !  CONSTANTS THAT REQUIRE ONE-TIME RESOLUTION.

    do ISPC = 1,MAXSP
       RHCON(ISPC) = 1.0
       if (LRCOR2.and.RCOR2(ISPC).gt.0.0)&
            &RHCON(ISPC) = RCOR2(ISPC)
       90 continue
    end do
    return
  end subroutine REGBM

  !===============================!
  !   Compute small tree growth   !
  !===============================!
  ! CANADA-BC $Id$
  subroutine REGBC(LCON)
    implicit none

    logical :: LCON
    ! Regional callees retain their native argument lists.
    interface
       subroutine DUBSCR(LVER,ISPC,XC,D,H,BAX,BAL,CR)
         logical :: LVER
         integer :: ISPC
         real :: XC
         real :: D
         real :: H
         real :: BAX
         real :: BAL
         real :: CR
       end subroutine DUBSCR
    end interface


    !  **REGENT** COMPUTES HEIGHT AND DIAMETER INCREMENTS FOR SMALL
    !  TREES.  THE HEIGHT INCREMENT MODEL IS APPLIED TO TREES THAT ARE LESS
    !  THAN 10 INCHES DBH (5 INCHES FOR LODGEPOLE PINE), AND THE DBH
    !  INCREMENT MODEL IS APPLIED TO TREES THAT ARE LESS THAN 3 INCHES DBH.
    !  FOR TREES THAT ARE GREATER THAN 2 INCHES DBH (1 INCH FOR LODGEPOLE
    !  PINE), HEIGHT INCREMENT PREDICTIONS ARE AVERAGED WITH THE PREDICTIONS
    !  FROM THE LARGE TREE MODEL. HEIGHT INCREMENT IS A FUNCTION OF HABITAT
    !  TYPE, SLOPE, ASPECT, TREE HEIGHT, BASAL AREA IN LARGER TREES, AND
    !  STAND CROWN COMPETITION FACTOR.  DIAMETER IS ASSIGNED FROM A HEIGHT
    !  DIAMETER FUNCTION WITH ADJUSTMENTS FOR RELATIVE SIZE AND STAND
    !  DENSITY.  INCREMENT IS COMPUTED BY SUBTRACTION. WHEN THE TREE ATTAINS
    !  A DBH OF 3.0 INCHES IN A CYCLE, **DUBSCR** IS CALLED TO ASSIGN A CROWN
    !  RATIO.   **REGENT** IS CALLED FROM **CRATET** DURING CALIBRATION AND
    !  FROM **GRINCR** DURING CYCLING.  ENTRY **REGCON** IS CALLED FROM
    !  **RCON** TO LOAD MODEL PARAMETERS THAT NEED ONLY BE RESOLVED ONCE.

    !  **REGENT** IS ALSO USED TO PREDICT HEIGHT INCREMENT, DBH AND CROWN
    !  RATIO FOR TREES THAT ARE CREATED BY THE REGENERATION ESTABLISHMENT
    !  MODEL.  5-YEAR OLD TREES ARE GENERATED BY **ESTAB** AND **REGENT**
    !  COMPUTES INCREMENTS FROM AGE 5 TO THE END OF THE CYCLE.

    ! Common blocks

    !  DIMENSIONS FOR INTERNAL VARIABLES:

    !   BANEXT -- ESTIMATED STAND BASAL AREA AT START OF EACH HEIGHT INCREMENT
    !             PREDICTION SUBCYCLE.
    !   CORTEM -- A TEMPORARY ARRAY FOR PRINTING CORRECTION TERMS.
    !     KPER -- NUMBER OF YEARS (MAX OF 5) IN EACH HEIGHT INCREMENT PREDICTION
    !             SUBCYCLE.
    !   NUMCAL -- A TEMPORARY ARRAY FOR PRINTING NUMBER OF HEIGHT
    !             INCREMENT OBSERVATIONS BY SPECIES.
    !   RDNEXT -- RELATIVE DENSITY (CCF) AT START OF EACH HEIGHT INCREMENT
    !             PREDICTION SUBCYCLE.
    !    RHCON -- CONSTANT FOR THE HEIGHT INCREMENT MODEL.  INCLUDES EFFECTS
    !             OF HABITAT TYPE, LOCATION, SLOPE, AND ASPECT AS LOADED IN
    !             RCON.  HABITAT EFFECTS ARE BORROWED FROM LARGE TREE DBH
    !             INCREMENT MODEL FOR ALL BUT TYPES 520, 530, AND 570.
    !             ADJUSTMENTS ARE THE DIFFERENCE BETWEEN THE HABITAT
    !             CONSTANT FOR A GIVEN TYPE AND THE CONSTANT FOR TYPE 520.
    !    RHCCF -- THE COEFFICIENTS FOR EACH SPECIES FOR THE CROWN
    !             COMPETITION FACTOR TERM IN THE HEIGHT INCREMENT MODEL.
    !     RHLH -- THE COEFFICIENTS FOR EACH SPECIES FOR THE LN(HEIGHT)
    !             TERM IN THE HEIGHT INCREMENT MODEL.
    !    RHBAL -- THE COEFFICIENTS FOR EACH SPECIES FOR THE BAL TERM
    !             IN THE HEIGHT INCREMENT MODEL.   COEFFICIENTS WERE
    !             TAKEN FROM THE LARGE TREE HEIGHT INCREMENT MODEL AS OF
    !             OCTOBER, 1983.
    !     HHT1 -- SPECIES DEPENDENT CONSTANT IN THE HEIGHT-DBH MODEL USED
    !             TO ASSIGN DIAMETERS TO SMALL TREES.
    !     HHT2 -- SPECIES DEPENDENT EXPONENT IN THE HEIGHT-DBH MODEL USED
    !             TO ASSIGN DIAMETERS TO SMALL TREES.
    !     XMAX -- UPPER END OF THE RANGE OF DIAMETERS OVER WHICH HEIGHT
    !             INCREMENT PREDICTIONS FROM SMALL AND LARGE TREE MODELS
    !             ARE AVERAGED.
    !     XMIN -- LOWER END OF THE RANGE OF DIAMETERS OVER WHICH HEIGHT
    !             INCREMENT PREDICTIONS FROM THE SMALL AND LARGE TREE
    !             ARE AVERAGED.

    !  SPECIES EXPANSION
    !  WB USE COEFFICIENTS FOR L FROM NI
    !  LM AND PY USE COEFFICIENTS AND LOGIC FROM TT FOR LM
    !  LL USE COEFFICIENTS FOR AF FROM NI
    !  AS, MM, PB USE COEFFIECIENTS AND LOGIC FOR AS FROM UT
    !  CO, OH USE COEFFIECIENTS AND LOGIC FOR OH FROM CR
    !  OS USE COEFFEICIENTS FOR MH FROM NI
    !  PI, JU USE COEFFICIENTS AND LOGIC FROM UT

    !  NOTE: UT AND CR LOGIC WAS MODIFIED FROM 10-YEAR REGYR TO 5-YEAR

    !     P_ZL  - LENGTH OF ZONE AND SITE SERIES STRINGS
    !     P_ST  - NUMBER OF SITE SERIES STRINGS PER ZONE OR SIBEC GROUP
    !     P_ZN  - NUMBER OF MODELS TO SEARCH THROUGH

    ! Variables
    integer :: P_ZL,P_ST,P_ZN

    parameter (P_ZL = 15)
    parameter (P_ST = 30)
    parameter (P_ZN = 27)

    external RANN
    character(len=2) :: SPEC
    character(len=15) :: PAT1
    character(len=15) :: PAT2
    logical :: DEBUG,LSKIPH
    integer :: I,KPER,IYR,ITOT,NPER,N,J1,IS,K,NYR,IPCCF,KY
    integer :: ISPC,I1,I2,L,ICRK,NUMCAL,KOUT,ISPEC,KK,M,IREFI,J
    integer :: IPOS(15),INDX(MAXTRE),NTYR,IPAS,I3
    real    :: BRATIO,BACHLO
    real    :: DIAM,HHT1,HHT2,XMAXv3,XMINv3,REGYR
    real    :: BACON,RDNEXT,BANEXT,SCALE,D1,P,D2,B1,B2,C1,C2,CW
    real    :: BI,CI,PN,BAYR,CCFYR,CR,RAN,R,AH,DELMAX,BAJ
    real    :: RDJ,PPCCF,XRHGRO
    real    :: XRDGRO,CON,BCCF,BBAL,XMX,XMN,AA,BB,H1,BAL,D,DADJ
    real    :: H2,HTGRL,RELH,DGJ,H,HK,HTGR1,HTGR
    real    :: XWT,ZZRAN,DK,DGK,BARK,DDS,SCALE2,DDS2,DG2,DNEW,XCRCON
    real    :: CORTEM,SCALE3,CORNEW,SNP,SNX,SNY,EDH,TERM
    real    :: TEMBA,TEMCCF,TEMAHT
    real    :: SBSCC1(MAXTRE),SBSCC2(MAXTRE)
    real    :: SBSCCF,CCFS,CCFL
    !     v2
    logical :: LSPPOK
    integer :: MAPHAB
    real    :: XMAXv2(15),XMINv2(15)
    real    :: RHBAL,RHHAB,RHGL,RHSC,RHLH,RHCCF
    real    :: RSAB0,RSAB1,RSAB2
    real    :: XCRIT,BLH,BHAB,BH,HSIGMA
    real    :: STHG_CON,B1X,B2X,B3X,B4X,B5X,B6X

    dimension CORTEM(15),NUMCAL(15),XMAXv3(15),XMINv3(15),&
         &HHT1(15),HHT2(15),RHBAL(15),RHLH(15),RHCCF(15),&
         &RHHAB(6,15),MAPHAB(30,15),RHGL(3),RHSC(15),DIAM(15),&
         &BANEXT(10),RDNEXT(10),KPER(10)

    !     STRUCTURE FOR THE GENERAL FITTED MODELS (TEMESGEN)

    TYPE ST_STR
       integer              :: SPP(15) ! species list
       character (LEN = P_ZL) :: ZONE(P_ST) ! BEC list
       real                 :: CON ! constant

       real                 :: CASP ! cos-aspect*slope
       real                 :: SASP ! sin-aspect*slope
       real                 :: SLP ! slope
       real                 :: HT ! ht
       real                 :: LNHT ! ln(ht)
       real                 :: CCF ! ccf
       real                 :: BAL ! bal/100

       real                 :: EXPMLT ! Mutliplier  !SBS only
       real                 :: HCCFL ! HT/CCF      !SBS only
       real                 :: CCFSL ! CCFS/CCFL   !SBS only

       real                 :: SD ! standard deviation
    end TYPE

    ! Data statements
    data DIAM/&
         &0.4, 3*0.3, 2*0.2, 0.4, 2*0.3, 0.5, 3*0.2, 0.3, 0.2 /
    data HHT1/&
         &0.0781, 0.0751, 0.0828, 0.1155, 0.0729, 0.0730,&
         &0.0988, 0.0658, 0.0658, 0.2160,&
         &0.0729, 0.0729, 0.0729, 0.0828, 0.0729 /
    data HHT2/&
         &1.1645, 1.1176, 1.1713, 1.0688, 1.1988, 1.2343,&
         &1.0807, 1.3817, 1.3817, 1.0049,&
         &1.1988, 1.1988, 1.1988, 1.1713, 1.1988 /
    !     v2
    data RHBAL/&
         &-0.37199,-0.41532,-0.40043,-0.25355,-0.34693,-0.12013,&
         &-0.24172,-0.25300,-0.22957,-0.47299,-0.25349,-0.25349,&
         &-0.25349,-0.40043,-0.25349/
    data  RHLH/&
         &.4214,.2716,.3907,.3487,.3417,.2354,.5843,.2827,.3740,.4485,&
         &.2354,.2354,.2354,.3907,.2354/
    data  RHCCF/&
         &-.00591,-.00654,-.00591,-.00391,-.00391,-.00391,-.00654,&
         &-.00391,-.00391,-.00654,-.00391,-.00391,-.00391,-.00591,&
         &-.00391/
    data HSIGMA/0.59/
    data XMAXv2/6*10.0,5.0,8*10.0/
    data XMINv2/6*2.0,1.0,8*2.0/
    data XMAXv3/15*4.0/
    data XMINv3/15*2.0/

    data REGYR/5.0/,BACON/0.005454154/

    !     DATA FOR THE GENERAL FITTED MODELS (TEMESGEN, ABDEL-AZIM)

    !     NOTE ON THE ERROR TERMS: THE IDF MODELS USE SEE, WHICH IS EQUIVALENT
    !     TO SD IN THIS CONTEXT (I THINK); FROM MY READING OF TEMESGEN'S REPORT
    !     THE ERROR TERM IS APPLIED BEFORE EXPONENTIATION. ZUMRAWI'S SBS MODELS
    !     ARE MSE AND ADDITIVE TO THE PREDICTED HEIGHT GROWTH.

    TYPE (ST_STR) ST_COEF(P_ZN)

    data ST_COEF(1) /&
         &ST_STR (&
         &(/3,14, 0,0,0,0,0,0,0,0,0,0,0,0,0/),& ! FD, OC
         &(/'IDFdk3/01      ', 'IDFdk3/03      ',&
         &'IDFdk3/05      ', 'IDFdk3/06      ',&
         &'IDFdk3/07      ', 'IDFdk3/08      ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &-1.205542,& ! constant
         &0.0,& ! cos-aspect*slope
         &0.0,& ! sin-aspect*slope
         &0.0,& ! slope
         &-0.450012,& ! ht
         &2.047554,& ! ln(ht)
         &0.0,& ! ccf
         &-3.532143,& ! bal/100
         &0.0,& ! Multiplier !SBS only
         &0.0,& ! HT/CCF     !SBS only
         &0.0,& ! CCFS/CCFL  !SBS only
         &0.34& ! SE=0.34, n=375
         &) /

    data ST_COEF(2) /&
         &ST_STR (&
         &(/7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0/),& ! PL
         &(/'IDFdk3/01      ', 'IDFdk3/05      ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &0.229415,& ! constant
         &0.0,& ! cos-aspect*slope
         &-0.083809,& ! sin-aspect*slope
         &0.0,& ! slope
         &0.0,& ! ht
         &0.222915,& ! ln(ht)hail
         &0.0,& ! ccf
         &0.0,& ! bal/100
         &0.0,& ! Multiplier !SBS only
         &0.0,& ! HT/CCF     !SBS only
         &0.0,& ! CCFS/CCFL  !SBS only
         &0.40& ! SE=0.40, n=35
         &) /

    data ST_COEF(3) /&
         &ST_STR (&
         &(/8,0,0,0,0,0,0,0,0,0,0,0,0,0,0/),& ! SE
         &(/'IDFdk3/01      ', 'IDFdk3/05      ',&
         &'IDFdk3/07      ', 'IDFdk3/08      ',&
         &'IDFdk3/09      ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &-0.607370,& ! constant
         &0.0,& ! cos-aspect*slope
         &0.0,& ! sin-aspect*slope
         &0.0,& ! slope
         &0.0,& ! ht
         &0.556296,& ! ln(ht)
         &0.0,& ! ccf
         &-4.424631,& ! bal/100
         &0.0,& ! Multiplier !SBS only
         &0.0,& ! HT/CCF     !SBS only
         &0.0,& ! CCFS/CCFL  !SBS only
         &0.36& ! SE=.36, n=66
         &) /

    data ST_COEF(4) /&
         &ST_STR (&
         &(/12,13, 0,0,0,0,0,0,0,0,0,0,0,0,0/),& ! AT,AC
         &(/'IDFdk4/01      ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &-0.144493,& ! constant
         &0.0,& ! cos-aspect*slope
         &0.0,& ! sin-aspect*slope
         &0.0,& ! slope
         &0.0,& ! ht
         &0.603647,& ! ln(ht)
         &0.0,& ! ccf
         &-16.012251,& ! bal/100
         &0.0,& ! Multiplier !SBS only
         &0.0,& ! HT/CCF     !SBS only
         &0.0,& ! CCFS/CCFL  !SBS only
         &0.07& ! SE=0.07, n=36
         &) /

    data ST_COEF(5) /&
         &ST_STR (&
         &(/3,14, 0,0,0,0,0,0,0,0,0,0,0,0,0/),& ! FD,OC
         &(/'IDFdk4/01      ', 'IDFdk4/02      ',&
         &'IDFdk4/05      ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &-0.988801,& ! constant
         &0.0,& ! cos-aspect*slope
         &0.0,& ! sin-aspect*slope
         &0.0,& ! slope
         &0.089809,& ! ht
         &0.462662,& ! ln(ht)
         &-0.008244,& ! ccf
         &0.197880,& ! bal/100
         &0.0,& ! Multiplier !SBS only
         &0.0,& ! HT/CCF     !SBS only
         &0.0,& ! CCFS/CCFL  !SBS only
         &0.06& ! SE=0.06, n=686
         &) /

    data ST_COEF(6) /&
         &ST_STR (&
         &(/7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),& ! PL
         &(/'IDFdk4/01      ', 'IDFdk4/05      ',&
         &'SBS/all        ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &-1.242998,& ! constant
         &0.0,& ! cos-aspect*slope
         &0.0,& ! sin-aspect*slope
         &0.0,& ! slope
         &0.108263,& ! ht
         &0.354549,& ! ln(ht)
         &-0.008124,& ! ccf
         &0.0,& ! bal/100
         &0.0,& ! Multiplier !SBS only
         &0.0,& ! HT/CCF     !SBS only
         &0.0,& ! CCFS/CCFL  !SBS only
         &0.09& ! SE=0.09, n=291
         &) /

    data ST_COEF(7) /&
         &ST_STR (&
         &(/3,14, 0,0,0,0,0,0,0,0,0,0,0,0,0 /),& ! FD
         &(/'IDFxm/01       ', 'IDFxm/04       ',&
         &'IDFxm/05       ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &-0.814652,& ! constant
         &0.0,& ! cos-aspect*slope
         &0.0,& ! sin-aspect*slope
         &0.0,& ! slope
         &-0.440036,& ! ht
         &0.994877,& ! ln(ht)
         &-0.004449,& ! ccf
         &0.0,& ! bal/100
         &0.0,& ! Multiplier !SBS only
         &0.0,& ! HT/CCF     !SBS only
         &0.0,& ! CCFS/CCFL  !SBS only
         &0.01& ! SE=0.01, n=203
         &) /

    data ST_COEF(8) /&
         &ST_STR (& ! IDF - HARDWOODS (EP,AT,CT,OH)
         &(/11,12,13,15,0,0,0,0,0,0,0,0,0,0,0/),&
         &(/'IDF/all        ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &-0.2041308,& ! constant
         &0.0,& ! cos-aspect*slope
         &0.0,& ! sin-aspect*slope
         &0.0,& ! slope
         &0.0,& ! ht
         &0.520337043,& ! ln(ht)
         &-0.0181535,& ! ccf
         &7.53695999,& ! bal/100
         &0.0,& ! Multiplier !SBS only
         &0.0,& ! HT/CCF     !SBS only
         &0.0,& ! CCFS/CCFL  !SBS only
         &0.48& ! SE=0.48, n=112
         &) /

    data ST_COEF(9) /&
         &ST_STR (& ! IDF - INTERMEDIATE (FD,PW,OC)
         &(/ 3,1,14, 0,0,0,0,0,0,0,0,0,0,0,0/),&
         &(/'IDF/all        ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &-0.5738321,& ! constant
         &0.004423421,& ! cos-aspect*slope
         &0.0,& ! sin-aspect*slope
         &0.5121443,& ! slope
         &-0.416014668,& ! ht
         &1.873005401,& ! ln(ht)
         &-0.0058833,& ! ccf
         &-1.1949869,& ! bal/100
         &0.0,& ! Multiplier !SBS only
         &0.0,& ! HT/CCF     !SBS only
         &0.0,& ! CCFS/CCFL  !SBS only
         &0.47& ! SE=0.47, n=1285
         &) /

    data ST_COEF(10) /&
         &ST_STR (& ! IDF - INTOLERANT (LW,PL,PY)
         &(/ 2,7,10, 0,0,0,0,0,0,0,0,0,0,0,0/),&
         &(/'IDF/all        ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &-0.0998574,& ! constant
         &0.0,& ! cos-aspect*slope
         &0.0,& ! sin-aspect*slope
         &0.0,& ! slope
         &-0.277808048,& ! ht
         &1.380037667,& ! ln(ht)
         &-0.0049829,& ! ccf
         &0.0,& ! bal/100
         &0.0,& ! Multiplier !SBS only
         &0.0,& ! HT/CCF     !SBS only
         &0.0,& ! CCFS/CCFL  !SBS only
         &0.52& ! SE=0.52, n=480
         &) /

    data ST_COEF(11) /&
         &ST_STR (& ! IDF - TOLERANT (BG,BL,HW,SE,CW)
         &(/ 4,9,5,8,6, 0,0,0,0,0,0,0,0,0,0/),&
         &(/'IDF/all        ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &-0.9885648,& ! constant
         &-0.011543749,& ! cos-aspect*slope
         &0.0,& ! sin-aspect*slope
         &-1.1824214,& ! slope
         &-0.562750419,& ! ht
         &2.757015881,& ! ln(ht)
         &0.0,& ! ccf
         &-2.7702553,& ! bal/100
         &0.0,& ! Multiplier !SBS only
         &0.0,& ! HT/CCF     !SBS only
         &0.0,& ! CCFS/CCFL  !SBS only
         &0.49& ! SE=0.49, n=271
         &) /

    data ST_COEF(12) /&
         &ST_STR (& ! ICH - HARDWOODS (EP,AT,CT,OH)
         &(/11,12,13,15, 0,0,0,0,0,0,0,0,0,0,0/),&
         &(/'ICH/all        ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &0.25147917,& ! constant
         &0.0,& ! cos-aspect*slope
         &0.0,& ! sin-aspect*slope
         &0.0,& ! slope
         &0.0,& ! ht
         &0.254578123,& ! ln(ht)
         &-0.0017203,& ! ccf
         &-0.324012,& ! bal/100
         &0.0,& ! Multiplier !SBS only
         &0.0,& ! HT/CCF     !SBS only
         &0.0,& ! CCFS/CCFL  !SBS only
         &0.71& ! SE=0.71, n=139
         &) /

    data ST_COEF(13) /&
         &ST_STR (& ! ICH - INTERMEDIATE (FD,PW,OC)
         &(/ 3, 1,14, 0,0,0,0,0,0,0,0,0,0,0,0/),&
         &(/'ICH/all        ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &-0.6368518,& ! constant
         &0.0,& ! cos-aspect*slope
         &0.0,& ! sin-aspect*slope
         &0.0191203,& ! slope
         &-0.386893543,& ! ht
         &2.247773778,& ! ln(ht)
         &-0.0076175,& ! ccf
         &-1.0014793,& ! bal/100
         &0.0,& ! Multiplier !SBS only
         &0.0,& ! HT/CCF     !SBS only
         &0.0,& ! CCFS/CCFL  !SBS only
         &0.51& ! SE=0.51, n=124
         &) /

    data ST_COEF(14) /&
         &ST_STR (& ! ICH - INTOLERANT (LW,PL,PY)
         &(/ 2, 7,10, 0,0,0,0,0,0,0,0,0,0,0,0/),&
         &(/'ICH/all        ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &-0.3236232,& ! constant
         &0.0,& ! cos-aspect*slope
         &0.0,& ! sin-aspect*slope
         &0.0,& ! slope
         &-0.40686086,& ! ht
         &1.89791257,& ! ln(ht)
         &-0.00182229,& ! ccf
         &0.0,& ! bal/100
         &0.0,& ! Multiplier !SBS only
         &0.0,& ! HT/CCF     !SBS only
         &0.0,& ! CCFS/CCFL  !SBS only
         &0.51& ! SE=0.51, n=142
         &) /

    data ST_COEF(15) /&
         &ST_STR (& ! ICH - TOLERANT (BG,BL,HW,SE,CW)
         &(/ 4,9,5,8,6, 0,0,0,0,0,0,0,0,0,0/),&
         &(/'ICH/all        ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &-0.0878875,& ! constant
         &0.0,& ! cos-aspect*slope
         &0.0,& ! sin-aspect*slope
         &0.0,& ! slope
         &0.0,& ! ht
         &0.302126544,& ! ln(ht)
         &-0.0081857,& ! ccf
         &0.0,& ! bal/100
         &0.0,& ! Multiplier !SBS only
         &0.0,& ! HT/CCF     !SBS only
         &0.0,& ! CCFS/CCFL  !SBS only
         &0.43& ! SE=0.43, n=342
         &) /

    data ST_COEF(16) /&
         &ST_STR (&
         &(/ 3,14, 0,0,0,0,0,0,0,0,0,0,0,0,0 /),& ! SBS (FD,OC)
         &(/'SBS/all        ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &172.1,& ! constant
         &0.0,& ! cos-aspect*slope  !IDF,ICH only
         &0.0,& ! sin-aspect*slope  !IDF,ICH only
         &0.0,& ! slope             !IDF,ICH only
         &-0.3657,& ! ht
         &0.0,& ! ln(ht)            !IDF,ICH only
         &0.0,& ! ccf               !IDF,ICH only
         &0.0,& ! bal/100           !IDF,ICH only
         &11.8295,& ! ExpMlt
         &-102.4,& ! HT/CCFL
         &0.5829,& ! CCFS/CCFL
         &0.536& ! SD (MSE=266.7cm**2, n=55)
         &) /

    data ST_COEF(17) /&
         &ST_STR (&
         &(/ 9, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),& ! SBS (BL)
         &(/'SBS/all        ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &272.8,& ! constant
         &0.0,& ! cos-aspect*slope  !IDF,ICH only
         &0.0,& ! sin-aspect*slope  !IDF,ICH only
         &0.0,& ! slope             !IDF,ICH only
         &-0.8451,& ! ht
         &0.0,& ! ln(ht)            !IDF,ICH only
         &0.0,& ! ccf               !IDF,ICH only
         &0.0,& ! bal/100           !IDF,ICH only
         &22.5817,& ! ExpMlt
         &-88.5901,& ! HT/CCFL
         &-0.7802,& ! CCFS/CCFL
         &0.643& ! SD (MSE=384.5cm**2, n=64)
         &) /

    data ST_COEF(18) /&
         &ST_STR (&
         &(/ 8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0 /),& ! SBS (SE)
         &(/'SBS/all        ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &157.0,& ! constant
         &0.0,& ! cos-aspect*slope  !IDF,ICH only
         &0.0,& ! sin-aspect*slope  !IDF,ICH only
         &0.0,& ! slope             !IDF,ICH only
         &-0.1435,& ! ht
         &0.0,& ! ln(ht)            !IDF,ICH only
         &0.0,& ! ccf               !IDF,ICH only
         &0.0,& ! bal/100           !IDF,ICH only
         &8.8508,& ! ExpMlt
         &-68.3412,& ! HT/CCFL
         &-4.0851,& ! CCFS/CCFL
         &0.840& ! SD (MSE=655.1cm**2, n=77)
         &) /

    data ST_COEF(19) /&
         &ST_STR (& ! SBS - borrowed from IDF (PL Intol)
         &(/ 7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0/),&
         &(/'SBS/all        ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &-0.0998574,& ! constant
         &0.0,& ! cos-aspect*slope
         &0.0,& ! sin-aspect*slope
         &0.0,& ! slope
         &-0.277808048,& ! ht
         &1.380037667,& ! ln(ht)
         &-0.0049829,& ! ccf
         &0.0,& ! bal/100
         &0.0,& ! Multiplier !SBS only
         &0.0,& ! HT/CCF     !SBS only
         &0.0,& ! CCFS/CCFL  !SBS only
         &0.52& ! SE=0.52, n=480
         &) /

    data ST_COEF(20) /&
         &ST_STR (& ! SBS - HARDWOODS (AT,CT,OH) also IDF
         &(/12,13,15, 0,0,0,0,0,0,0,0,0,0,0,0/),&
         &(/'SBS/all        ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &-0.2041308,& ! constant
         &0.0,& ! cos-aspect*slope
         &0.0,& ! sin-aspect*slope
         &0.0,& ! slope
         &0.0,& ! ht
         &0.520337043,& ! ln(ht)
         &-0.0181535,& ! ccf
         &7.53695999,& ! bal/100
         &0.0,& ! Multiplier !SBS only
         &0.0,& ! HT/CCF     !SBS only
         &0.0,& ! CCFS/CCFL  !SBS only
         &0.48& ! SE=0.48, n=112
         &) /

    data ST_COEF(21) /&
         &ST_STR (& ! DR TODO: Model Set 1
         &(/3, 0,0,0,0,0,0,0,0,0,0,0,0,0,0/),& ! Fd - SBS
         &(/'SBPSmc/01      ', 'SBPSmk/01      ',&
         &'SBPSdc/01      ', 'SBPSxc/01      ',&
         &'SBSdw1/01      ', 'SBSdw2/01      ',&
         &'SBSmc1/01      ', 'SBSmc2/01      ',&
         &'SBSmh/01       ', 'SBSmm/01       ',&
         &'SBSmw/01       ', 'SBSwk1/01      ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &172.1,& ! constant
         &0.0,& ! cos-aspect*slope
         &0.0,& ! sin-aspect*slope
         &0.0,& ! slope
         &-0.3657,& ! ht
         &0.0,& ! ln(ht)
         &0.0,& ! ccf
         &0.0,& ! bal/100
         &11.8295,& ! Multiplier !SBS only
         &102.4,& ! HT/CCF     !SBS only
         &0.5829,& ! CCFS/CCFL  !SBS only
         &16.33& ! SE=16.33, n=55
         &) /

    data ST_COEF(22) /&
         &ST_STR (& ! DR TODO: Model Set 2
         &(/3, 0,0,0,0,0,0,0,0,0,0,0,0,0,0/),& ! Fd - SBS
         &(/'SBPSmc/01      ', 'SBPSmk/01      ',&
         &'SBPSdc/01      ', 'SBPSxc/01      ',&
         &'SBSdw1/01      ', 'SBSdw2/01      ',&
         &'SBSmc1/01      ', 'SBSmc2/01      ',&
         &'SBSmh/01       ', 'SBSmm/01       ',&
         &'SBSmw/01       ', 'SBSwk1/01      ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &154.5,& ! constant
         &0.0,& ! cos-aspect*slope
         &0.0,& ! sin-aspect*slope
         &0.0,& ! slope
         &4.4054,& ! ht
         &0.0,& ! ln(ht)
         &0.0,& ! ccf
         &0.0,& ! bal/100
         &3.4734,& ! Multiplier !SBS only
         &-80.3308,& ! HT/CCF     !SBS only
         &1.1924,& ! CCFS/CCFL  !SBS only
         &38.93& ! SE=38.93, n=168
         &) /

    data ST_COEF(23) /&
         &ST_STR (& ! DR TODO: Model Set 1
         &(/8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0/),& ! Sx - SBS
         &(/'SBPSmc/01      ', 'SBPSmk/01      ',&
         &'SBPSdc/01      ', 'SBPSxc/01      ',&
         &'SBSdw1/01      ', 'SBSdw2/01      ',&
         &'SBSmc1/01      ', 'SBSmc2/01      ',&
         &'SBSmh/01       ', 'SBSmm/01       ',&
         &'SBSmw/01       ', 'SBSwk1/01      ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &157.0,& ! constant
         &0.0,& ! cos-aspect*slope
         &0.0,& ! sin-aspect*slope
         &0.0,& ! slope
         &-0.1435,& ! ht
         &0.0,& ! ln(ht)
         &0.0,& ! ccf
         &0.0,& ! bal/100
         &8.8508,& ! Multiplier !SBS only
         &68.3412,& ! HT/CCF     !SBS only
         &-4.0851,& ! CCFS/CCFL  !SBS only
         &25.59& ! SE=25.59, n=77
         &) /

    data ST_COEF(24) /&
         &ST_STR (& ! DR TODO: Model Set 2
         &(/8, 0,0,0,0,0,0,0,0,0,0,0,0,0,0/),& ! Sx - SBS
         &(/'SBPSmc/01      ', 'SBPSmk/01      ',&
         &'SBPSdc/01      ', 'SBPSxc/01      ',&
         &'SBSdw1/01      ', 'SBSdw2/01      ',&
         &'SBSmc1/01      ', 'SBSmc2/01      ',&
         &'SBSmh/01       ', 'SBSmm/01       ',&
         &'SBSmw/01       ', 'SBSwk1/01      ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &142.3,& ! constant
         &0.0,& ! cos-aspect*slope
         &0.0,& ! sin-aspect*slope
         &0.0,& ! slope
         &-2.9969,& ! ht
         &0.0,& ! ln(ht)
         &0.0,& ! ccf
         &0.0,& ! bal/100
         &6.3043,& ! Multiplier !SBS only
         &-112.6,& ! HT/CCF     !SBS only
         &-9.4900,& ! CCFS/CCFL  !SBS only
         &36.81& ! SE=36.81, n=168
         &) /

    data ST_COEF(25) /&
         &ST_STR (&
         &(/7, 0,0,0,0,0,0,0,0,0,0,0,0,0,0/),& ! Pl - SBS & SBPS
         &(/'SBPSmc/01      ', 'SBPSmk/01      ',&
         &'SBPSdc/01      ', 'SBPSxc/01      ',&
         &'SBSdw1/01      ', 'SBSdw2/01      ',&
         &'SBSmc1/01      ', 'SBSmc2/01      ',&
         &'SBSmh/01       ', 'SBSmm/01       ',&
         &'SBSmw/01       ', 'SBSwk1/01      ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &301.2,& ! constant
         &0.0,& ! cos-aspect*slope
         &0.0,& ! sin-aspect*slope
         &0.0,& ! slope
         &14.4134,& ! ht
         &0.0,& ! ln(ht)
         &0.0,& ! ccf
         &0.0,& ! bal/100
         &1.00,& ! Multiplier !SBS only
         &4.3821,& ! HT/CCF     !SBS only
         &0.0,& ! CCFS/CCFL  !SBS only
         &69.87& ! SE=69.87, n=69
         &) /

    data ST_COEF(26) /&
         &ST_STR (&
         &(/9, 0,0,0,0,0,0,0,0,0,0,0,0,0,0/),& ! Bl - SBS
         &(/'SBPSmc/01      ', 'SBPSmk/01      ',&
         &'SBPSdc/01      ', 'SBPSxc/01      ',&
         &'SBSdw1/01      ', 'SBSdw2/01      ',&
         &'SBSmc1/01      ', 'SBSmc2/01      ',&
         &'SBSmh/01       ', 'SBSmm/01       ',&
         &'SBSmw/01       ', 'SBSwk1/01      ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &76.4,& ! constant
         &0.0,& ! cos-aspect*slope
         &0.0,& ! sin-aspect*slope
         &0.0,& ! slope
         &3.6502,& ! ht
         &0.0,& ! ln(ht)
         &0.0,& ! ccf
         &0.0,& ! bal/100
         &6.1621,& ! Multiplier !SBS only
         &-238.8,& ! HT/CCF     !SBS only
         &-2.9184,& ! CCFS/CCFL  !SBS only
         &29.41& ! SE=29.41, n=89
         &) /

    data ST_COEF(27) /&
         &ST_STR (&
         &(/12, 0,0,0,0,0,0,0,0,0,0,0,0,0,0/),& ! SBS - At
         &(/'SBPSmc/01      ', 'SBPSmk/01      ',&
         &'SBPSdc/01      ', 'SBPSxc/01      ',&
         &'SBSdw1/01      ', 'SBSdw2/01      ',&
         &'SBSmc1/01      ', 'SBSmc2/01      ',&
         &'SBSmh/01       ', 'SBSmm/01       ',&
         &'SBSmw/01       ',  'SBSwk1/01      ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               ',&
         &'               ', '               '/),&
         &178.2,& ! constant
         &0.0,& ! cos-aspect*slope
         &0.0,& ! sin-aspect*slope
         &0.0,& ! slope
         &5.0664,& ! ht
         &0.0,& ! ln(ht)
         &0.0,& ! ccf
         &0.0,& ! bal/100
         &1.00,& ! Multiplier !SBS only
         &273.3,& ! HT/CCF     !SBS only
         &-2034.2,& ! CCFS/CCFL  !SBS only
         &78.89& ! SE=78.89, n=21
         &) /

    if (LCON) go to 9998

    !  CHECK FOR DEBUG.

    call DBCHK (DEBUG,'REGENT',6,ICYC)

    !  THE SMALL TREE HEIGHT INCREMENT MODEL IS BASED ON 5-YEAR INCREMENT DATA.
    !  TO AVOID BIAS, PREDICTIONS ARE MADE FOR SUBCYCLES THAT ARE LESS THAN OR
    !  EQUAL TO 5 YEARS IN LENGTH.  THE FOLLOWING CODE DETERMINES THE NUMBER
    !  AND LENGTH OF SUBCYCLES FROM CYCLE LENGTH.

    LSKIPH = .false.
    SCALE2 = 1.
    do I = 1,10
       KPER(I) = 0
       BANEXT(I) = 0.0
       RDNEXT(I) = 0.0
       1 continue
    end do
    NTYR = INT(FINT)
    if (LSTART) NTYR = IFINTH
    if (LESTB) NTYR = NTYR-5
    if (LESTB.and.NTYR.le.0) LSKIPH = .true.
    IYR = INT(REGYR)
    NPER = NTYR/IYR
    if (MOD(NTYR,IYR).ne.0) NPER = NPER+1
    ITOT = NTYR
    N = NPER
    do I = 1,NPER
       if (N.eq.1) go to 3
       KPER(I) = ITOT/N
       ITOT = ITOT-KPER(I)
       N = N-1
       2 continue
    end do
    3 continue
    if (NPER.gt.0)KPER(NPER) = ITOT

    !  LOAD TEMPORARY VARIABLES WITH AFTER THINNING BA, CCF, AND TOPHT.

    TEMBA = ATBA
    if (ATBA .le.0.)TEMBA = BA
    TEMCCF = ATCCF
    if (ATCCF .le.0.)TEMCCF = RELDEN
    TEMAHT = ATAVH
    if (DEBUG)write(JOSTND,*)'REGENT: TEMBA,TEMCCF,TEMAHT,BA,RELDEN= ',&
         &TEMBA,TEMCCF,TEMAHT,BA,RELDEN

    !  IF THIS IS THE FIRST ENTRY, BRANCH TO THE CALIBRATION SECTION.

    if (DEBUG)write(JOSTND,*)'REGENT: LSTART= ', LSTART
    if (LSTART) goto 40

    !  LOAD MULTIPLIERS.

    call MULTS (3,IY(ICYC),XRHMLT)
    call MULTS(6,IY(ICYC),XRDMLT)
    SCALE = YR/FINT
    if (DEBUG) write(JOSTND,8996) LESTB,LSKIPH,ITRN,ITRNIN,SCALE2,NPER
    8996 format('IN REGENT, LESTB = ',L1,', LSKIPH = ',L1,&
         &', ITRN = ',I4,', ITRNIN = ',I4,', SCALE2 = ',F6.3,&
         &', NPER = ',I3)
    if (ITRN.le.0) return

    !  ESTIMATE THE CONTRIBUTION TO STAND BASAL AREA AND CCF FOR LARGE TREES AT
    !  THE START OF EACH PROJECTION SUBCYCLE.  IF CALLED FROM **ESTAB**, BRANCH
    !  TO STATEMENT 9 AND INTERPOLATE FROM OLD AND NEW DENSITY VALUES.
    !  OTHERWISE, 10-YEAR CHANGE IS COMPUTED FROM PREDICTED DBH INCREMENT AND
    !  ANNUALIZED.  THEN, ANNUAL RATE IS MULTIPLIED BY SUBCYCLE LENGTH TO
    !  DETERMINE CONTRIBUTION FOR EACH SUBCYCLE.  A ONE PERCENT ANNUAL RATE
    !  OF MORTALITY IS ASSUMED.  CONTRIBUTIONS OF SMALL TREES ARE COMPUTED AS
    !  INCREMENT PREDICTIONS ARE MADE FOR PRECEDING SUBCYCLE.

    if (LESTB) go to 8
    do J = 1,NPER
       BANEXT(J) = BA
       RDNEXT(J) = RELDEN
       4 continue
    end do
    if (DEBUG) write(JOSTND,*)'NPER,BA,RELDEN= ',NPER,BA,RELDEN
    if (NPER.eq.1) go to 11
    do I = 1,ITRN
       D1 = DBH(I)
       if (D1.lt.3.0) go to 6
       IS = ISP(I)
       P = PROB(I)
       D2 = D1+DG(I)/BRATIO(IS,D1,HT(I))
       B1 = BACON*D1*D1
       B2 = BACON*D2*D2
       call CCFCAL(IS,D1,HT(I),ICR(I),P,.false.,C1,CW,1)
       call CCFCAL(IS,D2,HT(I),ICR(I),P,.false.,C2,CW,1)
       BI = (B2-B1)/10.0
       CI = (C2-C1)/10.0
       K = 0
       do J = 2,NPER
          K = K+KPER(J-1)
          PN = P*0.985**K
          RDNEXT(J) = RDNEXT(J)+K*CI/P*PN
          BANEXT(J) = BANEXT(J)+K*BI*PN
          5 continue
       end do
       6 continue
    end do
    go to 11

    !  IF CALLED FROM **ESTAB**, DIAMETERS AND DENSITY STATISTICS HAVE
    !  BEEN UPDATED AND DENSITIES CORRESPONDING TO BEGINNINGS OF SUBCYCLES
    !  CAN BE INTERPOLATED FROM DIFFERENCE BETWEEN OLD AND NEW STATS.  THE
    !  TREES ARE SENT TO REGENT WITH A HEIGHT CORRESPONDING TO 5 YEARS AFTER
    !  THE START OF THE CYCLE.

    8 continue
    BAYR = 0.0
    CCFYR = 0.0
    if (LSKIPH) go to 9
    BAYR = (BA-TEMBA)/ITOT
    CCFYR = (RELDEN-TEMCCF)/ITOT
    9 continue
    NYR = 5
    do J = 1,NPER
       RDNEXT(J) = TEMCCF+NYR*CCFYR
       BANEXT(J) = TEMBA+NYR*BAYR
       NYR = NYR+KPER(J)
       10 continue
    end do
    if (DEBUG)write(JOSTND,*)'AFTER 10 NPER,RDNEXT1-2, BANEXT1-2= ',&
         &NPER,RDNEXT(1),RDNEXT(2),BANEXT(1),BANEXT(2)
    11 continue

    !  STORE INITIAL HEIGHTS IN WK3; DUB CROWN RATIO FOR NEWLY ESTABLISHED
    !  SEEDLINGS.

    do I = 1,ITRN
       if (LESTB.and.I.ge.ITRNIN) then
          IPCCF = ITRE(I)
          CR = 0.89722 - 0.0000461*PCCF(IPCCF)
          12 continue
          RAN = BACHLO(0.0,1.0,RANN)
          if (RAN .lt. -1.0.or. RAN .gt.1.0) go to 12
          CR = CR + 0.07985 * RAN
          if (CR .gt..90) CR = .90
          if (CR .lt..20) CR = .20
          ICR(I) = INT((CR*100.0)+0.5)
          IF(DEBUG)WRITE(JOSTND,*)'DUB CROWN ESTAB TREES I,PCCF,RAN,CR,I  &
               &  CR= ',I,PCCF(IPCCF),RAN,CR,ICR(I)
       end if
       WK3(I) = HT(I)
       WK5(I) = DBH(I)
       13 continue
    end do

    !  DELMAX IS USED TO COMPUTE A DENSITY-DEPENDENT ADJUSTMENT FOR
    !  THE HEIGHT-DIAMETER FUNCTION.  DELMAX IS BASED ON AVERAGE HEIGHT AND
    !  CCF AT THE BEGINNING OF THE CYCLE AND DOES NOT VARY BY SUBCYCLE.

    if (LESTB) then
       R = TEMCCF
       AH = TEMAHT
    else
       R = RELDEN
       AH = AVH
    end if
    DELMAX = (AH/36.0)*(0.01232*R-1.75)
    if (DELMAX.gt.0.0) DELMAX = 0.0
    KY = 0
    if (DEBUG) write(JOSTND,8995) R,AH,DELMAX
    8995 format(' R = ',F10.4,', AH = ',F10.4,', DELMAX = ',F10.4)

    !     SBS MODELS USE CCF IN LARGER AND SMALLER TREES. CALCULATE CUMULATIVE CCF;
    !     SBSCC1 CONTAINS CCF FOR EACH RECORD, SBSCC2 CONTAINS CCF IN TREES UP TO
    !     BUT NOT INCLUDING THE CURRENT RECORD. SORTING IS BY DBH.

    if (.not. LV2ATV .and. INDEX(BEC%Zone,'SBS') .gt.0) then
       SBSCCF = 0.0
       do I = 1,ITRN
          call CCFCAL(ISP(I),DBH(I),HT(I),ICR(I),PROB(I),&
               &.false.,C1,CW,1)
          SBSCC1(I) = C1
          SBSCCF = SBSCCF + C1
       end do
       call RDPSRT(ITRN,DBH,INDX,.true.)
       SBSCC2(INDX(ITRN)) = SBSCC1(INDX(ITRN))
       do I = ITRN-1,1,-1
          SBSCC2(INDX(I)) = SBSCC1(INDX(I)) + SBSCC2(INDX(I+1))
       end do
       do I = 1,ITRN-1
          SBSCC2(INDX(I)) = SBSCC2(INDX(I+1))
       end do
       SBSCC2(INDX(ITRN)) = 0.0
    end if

    !  ENTER LOOP FOR NEXT SUBCYCLE.

    do J = 1,NPER
       KY = KY+KPER(J)
       BAJ = BANEXT(J)
       RDJ = RDNEXT(J)

       !  PPCCF IS A PROPORTIONAL ADJUSTMENT FOR POINT CCF VALUES BASED ON
       !  CHANGE IN STAND CCF FOR THE SUBCYCLE.

       PPCCF = 0.0
       if (RELDEN.gt.0.0) PPCCF = 1.0+(RDJ-RELDEN)/RELDEN
       if (DEBUG)write(JOSTND,*)'J,KY,BAJ,RDJ,RELDEN,PPCCF= ',&
            &J,KY,BAJ,RDJ,RELDEN,PPCCF

       !  PROCESS ALL TREES WITHIN THIS SUBCYCLE IN SPECIES ORDER.  LOAD
       !  SPECIES DEPENDENT CONSTANTS.

       do ISPC = 1,MAXSP
          I1 = ISCT(ISPC,1)
          if (I1.eq.0) go to 16
          if (.not. LV2ATV .and..not. STG%FIT(ISPC)) goto 16

          I2 = ISCT(ISPC,2)
          XRHGRO = XRHMLT(ISPC)
          XRDGRO = XRDMLT(ISPC)
          AA = HHT1(ISPC)
          BB = HHT2(ISPC)

          if (LV2ATV) then
             CON = RHCON(ISPC)+HCOR(ISPC)
             BH = RHLH(ISPC)
             BCCF = RHCCF(ISPC)
             BBAL = RHBAL(ISPC)
             XMX = XMAXV2(ISPC)
             XMN = XMINV2(ISPC)
          else
             XMX = XMAXV3(ISPC)
             XMN = XMINV3(ISPC)
             BBAL = RHBAL(ISPC)
          end if

          !  PROCESS NEXT TREE RECORD.  STORE INTERMEDIATE HEIGHTS IN WK3.

          do I3 = I1,I2

             !  BYPASS INCREMENT CALCULATION IF DBH IS GREATER THAN XMX OR IF
             !  CALL IS FROM **ESTAB** AND TREE SUBSCRIPT IS LESS THAN ITRNIN.

             if (DEBUG)write(JOSTND,*)'REGENT: I3,i1,i2= ', I3,I1,I2
             I = IND1(I3)
             D = DBH(I)
             H = HT(I)
             if (DEBUG)write(JOSTND,*)'REGENT: I,D,H= ', I,D,H
             if (DEBUG)write(JOSTND,*)&
                  &'REGENT: ISPC,I,D,XMX,LESTB,ITRNIN= ',&
                  &ISPC,I,D,XMX,LESTB,ITRNIN
             if (D.ge.XMX) go to 15

             if (LESTB .and. I.lt.ITRNIN) go to 15
             P = PROB(I)
             H1 = WK3(I)
             D1 = WK5(I)

             !           CCFL is constrained to be at least 36, the minimum
             !           from the modelling dataset

             if (LV2ATV) then
                if (DEBUG)write(JOSTND,*)'CALLING CCFCAL V2'
                call CCFCAL(ISPC,D,H1,ICR(I),P,.false.,C1,CW,1)
                if (DEBUG)write(JOSTND,*)'BACK FROM CCFCAL V2'
             else
                if (INDEX(BEC%Zone,'SBS') .gt.0) then
                   CCFS = SBSCC2(I) * RDJ/SBSCCF
                   CCFL = (SBSCCF - SBSCC1(I)) * RDJ/SBSCCF
                end if
             end if

             BAL = (1.0 - (PCT(I)/100.)) * BAJ
             B1 = BACON*D*D

             !  NI VARIANT CODE
             !  COMPUTE ADJUSTMENT FOR BIAS, DENSITY, AND RELATIVE SIZE.

             if (ABS(AH-4.5).lt.0.01 ) then
                RELH = 0.0
             else
                RELH = (H1-4.5)/(AH-4.5)
             end if
             if (RELH.gt.1.0) RELH = 1.0
             if (RELH.lt.0.0) RELH = 0.0

             DADJ = DELMAX*RELH*RELH - 2.0*DELMAX*RELH + 0.65

             !  COMPUTE HEIGHT INCREMENT.  STORE UPDATED HEIGHT IN WK3.

             !           NOTE: SEISTHG/LSPPOK (IDF ONLY) IS ALL DEPRECATED IN
             !           FAVOUR OF V3 SMALL TREE HEIGHT GROWTH MODEL. CODE LEFT HERE
             !           FOR "HISTORICAL PURPOSES" ONLY.

             if (DEBUG)write(JOSTND,*)'REGENT: CALLING SEISTHG, LV2ATV',&
                  &LV2ATV
             if (LV2ATV) then
                call SEISTHG (ISPC,STHG_CON,B1X,B2X,B3X,B4X,B5X,B6X,&
                     &LSPPOK)
                if (LSPPOK) then
                   STHG_CON = -0.00101 * ELEV * 100. * FTtoM ! ELEV(m)
                   B1X = 0.518176 * ALOG(H1 * FTtoM) ! LN(HT)m)
                   B3X = -0.003254 * RDJ ! CCF
                   RAN = BACHLO(0.0,0.62921,RANN) ! Error
                   HTGRL = STHG_CON + B1X + B3X + RAN
                else
                   HTGRL = (CON+BH*ALOG(H1)+BCCF*RDJ+BBAL*BAL)
                end if
             else
                if (INDEX(BEC%Zone,'SBS') .gt.0) then
                   HTGRL = &
                        &(STG%CON(ISPC) +&
                        &(STG%HT(ISPC) * (H1 * FTtoM))) /&
                        &(1.0 + (STG%EXPMLT(ISPC) *&
                        &EXP((STG%HCCFL(ISPC) * H1 * FTtoM / MAX(CCFL,36.)) +&
                        &(STG%CCFSL(ISPC) * CCFS/MAX(CCFL,36.)))))
                   HTGRL = HTGRL * CMtoFT * EXP(RHCON(ISPC))
                else
                   if (DEBUG)write(JOSTND,*)'REGENT: PRE-HTGRL'
                   HTGRL = STG%CON(ISPC) + RHCON(ISPC) +&
                        &(STG%LNHT(ISPC) * ALOG(MAX(2.0,H1 * FTtoM))) +&
                        &(STG%HT(ISPC)   * MAX(2.0,H1 * FTtoM)) +&
                        &(STG%BAL(ISPC)  * BAL * FT2pACRtoM2pHA * 0.01) +&
                        &(STG%CCF(ISPC)  * RDJ)
                   HTGRL = EXP(MAX(-88.0,HTGRL)) * MtoFT
                   if (DEBUG)write(JOSTND,*)'REGENT: POST-HTGRL'
                end if
             end if

             if (DEBUG)write(JOSTND,*)'REGENT: LV2ATV', LV2ATV
             if (DEBUG)write(JOSTND,*)'REGENT: LSPPOK', LSPPOK
             if (DEBUG)write(JOSTND,*)'REGENT: HTGRL', HTGRL
             if (DEBUG)write(JOSTND,*)'REGENT: KPER(J)', KPER(j)
             if (DEBUG)write(JOSTND,*)'REGENT: REGYR', REGYR
             if (DEBUG)write(JOSTND,*)'REGENT: XRHGRO', XRHGRO
             if (DEBUG)write(JOSTND,*)'REGENT: H1', H1

             if (LV2ATV) then
                if (LSPPOK) then
                   H2 = H1+&
                        &(EXP(MAX(-40.0,HTGRL))*(FLOAT(KPER(J))/REGYR)*XRHGRO&
                        &* MtoFT)
                else
                   H2 = H1+&
                        &EXP(MAX(-40.0,HTGRL))*(FLOAT(KPER(J))/REGYR)*XRHGRO
                end if
             else
                H2 = H1+HTGRL*(FLOAT(KPER(J))/REGYR)*XRHGRO
             end if

             if (DEBUG)write(JOSTND,*)'REGENT: H2', H2
             if (DEBUG)write(JOSTND,*)'REGENT: POST-H2'
             WK3(I) = H2

             !  UPDATE DENSITY FOR NEXT SUBCYCLE.  IF ONLY ONE SUBCYCLE OR
             !  IF INITIAL DBH IS GREATER THAN 3 INCHES, BYPASS DENSITY UPDATE.
             !  DIAMETER IS ADJUSTED FOR DENSITY AND USER-SUPPLIED MULTIPLIER.
             !  INCREMENT IS DETERMINED FROM THE DIFFERENCE BETWEEN COMPUTED
             !  DIAMETERS AND IS BOUNDED TO BE GREATER THAN OR EQUAL TO ZERO.

             D2 = D
             if (DEBUG)write(JOSTND,*)'REGENT PRE-D2: D2,J,H2,BB,AA,DADJ',&
                  &D2,J,H2,BB,AA,DADJ
             if (J.ge.NPER.or.D.ge.3.0) go to 14
             if (H2.le.4.5) go to 14
             D1 = DIAM(ISPC)+DADJ
             if (H1.gt.4.5) D1 = AA*(H1-4.5)**BB + DADJ
             D2 = AA*(H2-4.5)**BB + DADJ
             DGJ = (D2-D1)*XRDGRO
             if (DEBUG)&
                  &write(JOSTND,*)' I,ISPC,AA,BB,DADJ,H1,D1,H2,D2,DGJ=',&
                  &I,ISPC,AA,BB,DADJ,H1,D1,H2,D2,DGJ
             if (DGJ.lt.0.0) DGJ = 0.0
             D2 = D+DGJ

             !  COMPUTE ADJUSTMENT TO STAND BASAL AREA AND CCF FOR NEXT SUBCYCLE.
             !  ASSUME A 1.5 PERCENT ANNUAL MORTALITY RATE.

             call CCFCAL(ISPC,D1,H1,ICR(I),P,.false.,C1,CW,1)
             call CCFCAL(ISPC,D2,H2,ICR(I),P,.false.,C2,CW,1)
             RDNEXT(J+1) = RDNEXT(J+1)+KY*(C2-C1)/10.*0.985**KY
             BANEXT(J+1) = BANEXT(J+1)+(BACON*D2*D2-B1)*P*0.985**KY

             if (DEBUG) write(JOSTND,8997) I,D2,BANEXT(J+1),RDNEXT(J+1)
             8997 format(' UPDATE DENSITY: TREE = ',I4,', D2 = ',F8.4,&
                  &', BANEXT(J+1) = ',F8.2,', RDNEXT(J+1) = ',F8.2)

             !  END OF TREE LOOP WITHIN SPECIES. PRINT DEBUG IF REQUESTED.

             14 continue
             if (DEBUG)write(JOSTND,*)'REGENT: AFTER 14 CONTINUE', DEBUG
             if (.not.DEBUG) go to 15
             if (DEBUG)write(JOSTND,*)'REGENT: RDJ', RDJ
             if (DEBUG)write(JOSTND,*)'REGENT: BAL', BAL
             if (DEBUG)write(JOSTND,*)'REGENT: BAJ', BAJ
             if (DEBUG)write(JOSTND,*)'REGENT: PCT(I)', PCT(I)
             if (DEBUG)write(JOSTND,*)'REGENT: BBAL', BBAL !! NOT DEFINED

             write(JOSTND,8999) J,KPER(J),ISPC,I,H1,H2,BCCF,RDJ,BBAL,BAL,&
                  &BAJ,PCT(I)
             8999 format('IN REGENT L1: SUBC=',I2,', KPER=',I2,', SP=',I2,&
                  &', I=',I4,', H1=',F8.4,', H2=',F8.4,', BCCF=',F8.4/T4,'RDJ=',&
                  &F8.4,', BBAL=',F8.4,', BAL=',F8.4,', BAJ=',F8.4,', PCT=',F8.4)
             15 continue
          end do

          !  END OF SPECIES LOOP WITHIN SUBCYCLE

          16 continue
       end do

       !  END OF SUBCYCLE LOOP.  END OF PERIOD HEIGHT IS STORED IN WK3.

       17 continue
    end do

    !  AFTER LAST SUBCYCLE, COMPUTE ACTUAL HEIGHT INCREMENT FOR FULL CYCLE BY
    !  SUBTRACTION; CONVERT BACK TO LOG SCALE TO ADD IN RANDOM ERROR.

    do ISPC = 1,MAXSP
       I1 = ISCT(ISPC,1)
       if (I1.eq.0) go to 30
       I2 = ISCT(ISPC,2)
       XRDGRO = XRDMLT(ISPC)
       if (LV2ATV) then
          XMX = XMAXV2(ISPC)
          XMN = XMINV2(ISPC)
       else
          XMX = XMAXV3(ISPC)
          XMN = XMINV3(ISPC)
       end if
       AA = HHT1(ISPC)
       BB = HHT2(ISPC)
       do I3 = I1,I2
          I = IND1(I3)
          D = DBH(I)
          CR = FLOAT(ICR(I))
          if (D.ge.XMX) go to 25
          if (LESTB .and. I.lt.ITRNIN) go to 25
          K = I
          L = 0
          H = HT(I)
          HK = WK3(I)

          !  IF CALLED FROM **ESTAB** AND TOTAL CYCLE LENGTH IS LESS THAN OR EQUAL
          !  TO 5 YEARS, SKIP HEIGHT INCREMENT CALCULATION BUT ASSIGN CR AND DBH.

          if (LSKIPH) then
             HTG(K) = 0
             go to 20
          end if
          HTGR1 = HK-H
          if (HTGR1.lt.0.0) HTGR1 = 0.0

          ICRK = ICR(K)

          !  DRAW RANDOM INCREMENT GIVEN EXPECTED VALUE, HTGR, AND STD. DEV.,
          !  BOUND DRAW TO (-1.5*SD < ZZRAN < 1.0*SD); IF TRIPLING,
          !  EACH TRIPLE GETS A NEW RANDOM NUMBER.  DO NOT TRIPLE IF CALLING
          !  FROM **ESTAB**.  DO NOT APPLY HEIGHT GROWTH MULTIPLIER; MULTIPLIER
          !  WAS USED DURING SUBCYCLING.

          !  BC VARIANT IN V3 SBS USES ADDITIVE ERROR INSTEAD OF MULTIPLICATIVE
          !  USED IN NI; HTGR CANNOT BE NEGATIVE!

          18 continue
          ZZRAN = 0.0
          if (DGSD.ge.1.0) ZZRAN = BACHLO(0.0,1.0,RANN)
          if ((ZZRAN .gt.1.0) .or. (ZZRAN .lt. -1.5)) go to 18

          if (LV2ATV) then
             HTGR = HTGR1*EXP(ZZRAN*HSIGMA)
          else
             HTGR = MAX(0.0, HTGR1 + ZZRAN*STG%SD(ISPC))
          end if

          !  COMPUTE WEIGHTS FOR THE LARGE AND SMALL TREE HEIGHT INCREMENT
          !  ESTIMATES.  IF DBH IS LESS THAN OR EQUAL TO XMN OR IF CALLED
          !  FROM **ESTAB**, THE LARGE TREE PREDICTION IS IGNORED (XWT=0.0).

          XWT = 0.0
          if (D.le.XMN.or.LESTB) go to 19
          XWT = (D-XMN)/(XMX-XMN)
          19 continue
          if (DEBUG)write(JOSTND,*)'WEIGHTING K,HTGR,HTG,XWT= ',&
               &K,HTGR,HTG(K),XWT
          HTG(K) = HTGR*(1.0-XWT) + XWT*HTG(K)

          ! CHECK FOR SIZE CAP COMPLIANCE.

          if ((H+HTG(K)).gt.SIZCAP(ISPC,4)) then
             HTG(K) = SIZCAP(ISPC,4)-H
             if (HTG(K) .lt.0.1) HTG(K) = 0.1
          end if

          20 continue

          !  ASSIGN DBH AND COMPUTE DBH INCREMENT FOR TREES WITH DBH LESS
          !  THAN 3 INCHES.  COMPUTE 10-YEAR DBH INCREMENT REGARDLESS OF
          !  PROJECTION PERIOD LENGTH.  BYPASS INCREMENT CALCULATION IF
          !  D GE 3.0 INCHES.

          if (D.ge.3.0) go to 23

          !  COMPUTE ADJUSTMENT FOR BIAS, DENSITY, AND RELATIVE SIZE.

          if (ABS(AH-4.5).lt.0.01 ) then
             RELH = 0.0
          else
             RELH = (H-4.5)/(AH-4.5)
          end if
          if (RELH.gt.1.0) RELH = 1.0
          if (RELH.lt.0.0) RELH = 0.0
          DADJ = DELMAX*RELH*RELH - 2.0*DELMAX*RELH + 0.65
          D1 = DIAM(ISPC)+DADJ
          if (H.gt.4.5) D1 = AA*(H-4.5)**BB+DADJ
          if (DEBUG)write(JOSTND,*)' I,ISPC,AA,BB,DADJ,H,D1= ',&
               &I,ISPC,AA,BB,DADJ,H,D1
          HK = H+HTG(K)
          if (HK .lt.4.5) then
             DBH(K) = 0.1 + DIAM(ISPC)*.01 + HK*0.001
             DG(K) = 0.0
          else
             DK = AA*(HK-4.5)**BB+DADJ
             if (DEBUG)write(JOSTND,*)' I,ISPC,AA,BB,DADJ,HK,DK= ',&
                  &I,ISPC,AA,BB,DADJ,HK,DK
             if (DK.lt.DIAM(ISPC)) DK = DIAM(ISPC)
             DK = DK+HK*0.001

             !       IF CALLING FROM **ESTAB** ASSIGN DIAMETER AND DIAMETER
             !       INCREMENT.

             if (LESTB) then
                DBH(K) = DK
                DG(K) = DK
             else

                !         DIAMETER INCREMENT IS THE DIFFERENCE BETWEEN COMPUTED
                !         DIAMETERS ON A FINT-YEAR BASIS.
                !         MULTIPLIER IS APPLIED TO DIAMETER INCREMENT AT THIS POINT.

                DGK = (DK-D1)*XRDGRO
                if (DGK.lt.0.0) DGK = 0.0

                !         SCALE DIAMETER INCREMENT FOR BARK AND PERIOD LENGTH.
                !         IN ORDER TO MAINTAIN CONSISTENCY WITH **GRADD**,
                !         ADJUSTMENTS ARE MADE ON THE DDS SCALE.

                BARK = BRATIO(ISPC,DBH(K),HT(K))
                DG(K) = DGK*BARK
                DDS = DG(K)*(2.0*BARK*D+DG(K))*SCALE
                DG(K) = SQRT((D*BARK)**2+DDS)-BARK*D
             end if
             if ((DBH(K)+DG(K)).lt.DIAM(ISPC)) then
                DG(K) = DIAM(ISPC)-DBH(K)
             end if
          end if

          !  CHECK FOR TREE SIZE CAP COMPLIANCE

          call DGBND(ISPC,DBH(K),DG(K))

          !  IF THE TREE WILL ATTAIN A DBH OF 2 OR MORE CM THIS CYCLE
          !  CALL **DUBSCR** TO ESTIMATE A NEW CROWN RATIO, IF IT DOESN'T HAVE ONE
          !  DIAMETER GROWTH SCALED TO CYCLE LENGTH FOR THIS CALCULATION.
          !  MAKE SURE NEW CROWN DOES NOT EXCEED MAX POSSIBLE GIVEN HTG.

          if (.not.LESTB) then
             SCALE2 = FINT/YR
             D = DBH(K)
             BARK = BRATIO(ISPC,D,HT(K))
             DDS2 = (DG(K)*(2.0*BARK*D+DG(K)))*SCALE2
             DG2 = SQRT((D*BARK)**2+DDS2)-BARK*D
             if (DG2.lt.0.0) DG2 = 0.0
             DNEW = D+DG(K)

             if (LV2ATV) then
                XCRIT = 3.0
                XCRCON = -1.0
                BAL = -1.0
             else
                XCRIT = 2.0*CMtoIN
                XCRCON = CRCON(ISPC) +&
                     &CRLNCCF(ISPC) * LOG(MAX(0.01,RELDEN))
                BAL = (1.0 - (PCT(K)/100.)) * BA
             end if

             if (DNEW.ge.XCRIT) then
                if (ICRK .eq.0) then
                   call DUBSCR(LV2ATV,ISPC,XCRCON,DNEW,HK,BA,BAL,CR)
                   ICRK = IFIX(CR*100. + 0.5)
                end if
                ICR(K) = ICRK
                if (DEBUG)write(JOSTND,*)' AFTER DUBSCR K,ISPC,DNEW,',&
                     &'HK,BA,ICR= ',K,ISPC,DNEW,HK,BA,ICR(K)
             end if
          end if
          23 continue

          !  PRINT DEBUG AND RETURN TO PROCESS NEXT TRIPLE OR NEXT TREE.

          if (.not.DEBUG) go to 24
          write(JOSTND,9000) I,K,ISPC,H,HTG(K),HK,DBH(K),DG(K)
          9000 format(' IN REGENT LOOP 2, I=',I4,', K=',I4,', ISPC=',I3,&
               &', CUR HT=',F7.2,', HT INC=',F7.4/T20,'NEW HT=',F7.2,&
               &', CUR DBH=',F7.2,', 10-YR DBH INC=',F7.4)
          24 continue
          if (LESTB .or..not.LTRIP .or. L.ge.2) go to 25
          L = L+1
          K = ITRN+2*I-2+L
          go to 18

          !  END OF GROWTH PREDICTION LOOP.  PRINT DEBUG INFO IF DESIRED.

          25 continue
       end do
       30 continue
    end do

    return

    !  SMALL TREE HEIGHT CALIBRATION SECTION.

    40 continue
    do ISPC = 1,MAXSP
       HCOR(ISPC) = 0.0
       CORTEM(ISPC) = 1.0
       NUMCAL(ISPC) = 0
       45 continue
    end do
    if (DEBUG) write(JOSTND,*) 'REGENT: ITRN,IFINTH,REGYR',&
         &ITRN,IFINTH,REGYR

    if (ITRN.le.0) return
    if (IFINTH .eq.0)  goto 100
    SCALE3 = REGYR/FINTH

    !  DETERMINE BASAL AREA IN LARGE TREES AT START OF EACH PREDICTION SUBCYCLE.

    if (DEBUG) write(JOSTND,*) 'REGENT: TEMBA,TEMCCF,NPER= ',&
         &TEMBA,TEMCCF,NPER
    do J = 1,NPER
       BANEXT(J) = TEMBA
       RDNEXT(J) = TEMCCF
       46 continue
    end do
    if (NPER.eq.1) go to 50

    !  IF THERE IS MORE THAN 1 SUBCYCLE FOR HEIGHT INCREMENT PREDICTION,
    !  (IFINTH > 5) ESTIMATE VALUES OF BA AND RELATIVE DENSITY (CCF) AT
    !  START OF EACH SUBCYCLE.  10-YEAR CHANGE IS COMPUTED FROM BACKDATED
    !  DIAMETER (WK3) AND 10-YEAR DIAMETER INCREMENT COMPUTED DURING CALIBRATION.
    !  LINEAR INTERPOLATION IS USED TO ASSIGN VALUES FOR SUBCYCLES.

    if (DEBUG) write(JOSTND,*) 'REGENT: ABOVE DO 49 LOOP'
    do I = 1,ITRN
       D1 = WK3(I)
       IS = ISP(I)
       P = PROB(I)
       D2 = WK3(I)+DG(I)/BRATIO(IS,D1,HT(I))
       B1 = BACON*D1*D1
       B2 = BACON*D2*D2
       if (DEBUG) write(JOSTND,*) 'REGENT: CALLING CCFCAL'
       call CCFCAL(IS,D1,HT(I),ICR(I),P,.false.,C1,CW,1)
       call CCFCAL(IS,D2,HT(I),ICR(I),P,.false.,C2,CW,1)
       if (DEBUG) write(JOSTND,*) 'REGENT: BACK FROM CCFCAL'
       BI = (B2-B1)/10.0
       CI = (C2-C1)/10.0
       K = 0
       do J = 2,NPER
          K = K+KPER(J-1)
          PN = P*0.985**K
          RDNEXT(J) = RDNEXT(J)+K*CI/P*PN
          BANEXT(J) = BANEXT(J)+K*BI*PN
          48 continue
       end do
       49 continue
    end do
    50 continue

    !     SBS MODELS USE CCF IN LARGER AND SMALLER TREES. CALCULATE CUMULATIVE CCF;
    !     SBSCC1 CONTAINS CCF FOR EACH RECORD, SBSCC2 CONTAINS CCF IN TREES UP TO
    !     BUT NOT INCLUDING THE CURRENT RECORD. SORTING IS BY DBH.

    if (DEBUG) write(JOSTND,*) 'REGENT: LV2ATV, BEC%Zone',&
         &LV2ATV, BEC%Zone
    if (.not. LV2ATV .and. INDEX(BEC%Zone,'SBS') .gt.0) then
       SBSCCF = 0.0
       do I = 1,ITRN
          call CCFCAL(ISP(I),DBH(I),HT(I),ICR(I),PROB(I),&
               &.false.,C1,CW,1)
          SBSCC1(I) = C1
          SBSCCF = SBSCCF + C1
       end do
       call RDPSRT(ITRN,DBH,INDX,.true.)
       SBSCC2(INDX(ITRN)) = SBSCC1(INDX(ITRN))
       do I = ITRN-1,1,-1
          SBSCC2(INDX(I)) = SBSCC1(INDX(I)) + SBSCC2(INDX(I+1))
       end do
       do I = 1,ITRN-1
          SBSCC2(INDX(I)) = SBSCC2(INDX(I+1))
       end do
       SBSCC2(INDX(ITRN)) = 0.0
    end if
    if (DEBUG) write(JOSTND,*) 'REGENT: After LV2ATV, BEC%Zone'

    do ISPC = 1,MAXSP

       !  BEGIN PROCESSING TREE LIST IN SPECIES ORDER.  DO NOT CALCULATE
       !  CORRECTION TERMS IF THERE ARE NO TREES FOR THIS SPECIES.

       if (DEBUG) write(JOSTND,*) 'REGENT: BEFORE GOTO 90 TEST'
       CORNEW = 1.0
       I1 = ISCT(ISPC,1)
       if (I1.eq.0.or..not. LHTCAL(ISPC)) go to 90
       if (.not. LV2ATV .and..not. STG%FIT(ISPC)) goto 90
       I2 = ISCT(ISPC,2)
       IREFI = IREF(ISPC)
       N = 0
       SNP = 0.0
       SNX = 0.0
       SNY = 0.0
       BBAL = RHBAL(ISPC)
       if (LV2ATV) then
          BHAB = RHCON(ISPC)
          BLH = RHLH(ISPC)
          BCCF = RHCCF(ISPC)
       end if
       if (DEBUG) write(JOSTND,*) 'REGENT: LHTCAL(ISPC), STG%FIT(ISPC)',&
            &LHTCAL(ISPC), STG%FIT(ISPC)

       !  BEGIN TREE LOOP WITHIN SPECIES.  IF MEASURED HEIGHT INCREMENT IS
       !  LESS THAN OR EQUAL TO ZERO, OR DBH IS GREATER THAN 5.0, THE RECORD
       !  WILL BE EXCLUDED FROM THE CALIBRATION.

       do I3 = I1,I2
          I = IND1(I3)
          H = HT(I)

          if (IHTG.lt.2) H = H-HTG(I)
          if (DBH(I).ge.5.0.or.H.lt.(1.37*MtoFT)) go to 60
          if (HTG(I).lt.0.001) go to 60

          HK = H
          D = DBH(I)
          CR = FLOAT(ICR(I))
          if (DEBUG) write(JOSTND,*) 'REGENT: HK= ', HK
          if (DEBUG) write(JOSTND,*) 'REGENT: D= ', D
          if (DEBUG) write(JOSTND,*) 'REGENT: CR= ', CR
          do J = 1,NPER
             BAJ = BANEXT(J)
             RDJ = RDNEXT(J)
             if (DEBUG) write(JOSTND,*) 'REGENT: BAJ= ', BAJ
             if (DEBUG) write(JOSTND,*) 'REGENT: RDJ= ', RDJ
             if (LV2ATV) then
                BAL = BAJ*(100.0-PCT(I))*0.0001
                if (DEBUG) write(JOSTND,*) 'REGENT: BAL1= ', BAL
                EDH = EXP(BHAB + BLH*ALOG(HK) + BCCF*RDJ + BBAL*BAL)
                if (DEBUG) write(JOSTND,*) 'REGENT: EDH1= ', EDH
             else
                BAL = (1.0 - (PCT(I)/100.)) * BAJ
                if (DEBUG) write(JOSTND,*) 'REGENT: BAL2= ', BAL
                if (INDEX(BEC%Zone,'SBS') .gt.0) then
                   CCFS = SBSCC2(I) * RDJ/SBSCCF
                   CCFL = (SBSCCF - SBSCC1(I)) * RDJ/SBSCCF
                   EDH = &
                        &(STG%CON(ISPC) +&
                        &(STG%HT(ISPC) * (H * FTtoM))) /&
                        &(1.0 + (STG%EXPMLT(ISPC) *&
                        &EXP((STG%HCCFL(ISPC) * H * FTtoM / CCFL) +&
                        &(STG%CCFSL(ISPC) * CCFS/CCFL) ) ) )
                   EDH = EDH * CMtoFT * EXP(RHCON(ISPC))
                   if (DEBUG) write(JOSTND,*) 'REGENT: EDH2= ', EDH
                else
                   if (DEBUG) write(JOSTND,*) 'R: BEFORE EDH3 '
                   if (DEBUG) write(JOSTND,*) 'R: STG%CON(ISPC)= ', STG%CON(ISPC)
                   if (DEBUG) write(JOSTND,*) 'R: RHCON(ISPC)= ', RHCON(ISPC)
                   if (DEBUG) write(JOSTND,*) 'R: STG%LNHT(ISPC)= ', STG%LNHT(ISPC)
                   if (DEBUG) write(JOSTND,*) 'R: H= ', H
                   if (DEBUG) write(JOSTND,*) 'R: STG%HT(ISPC)= ', STG%HT(ISPC)
                   if (DEBUG) write(JOSTND,*) 'R: STG%BAL(ISPC)= ', STG%BAL(ISPC)
                   if (DEBUG) write(JOSTND,*) 'R: BAL= ', BAL
                   if (DEBUG) write(JOSTND,*) 'R: STG%CCF(ISPC)= ', STG%CCF(ISPC)
                   if (DEBUG) write(JOSTND,*) 'R: RDJ= ', RDJ
                   EDH = STG%CON(ISPC) + RHCON(ISPC) +&
                        &(STG%LNHT(ISPC) * ALOG(MAX(2.0,H * FTtoM))) +&
                        &(STG%HT(ISPC)   * MAX(2.0,H * FTtoM)) +&
                        &(STG%BAL(ISPC)  * BAL * FT2pACRtoM2pHA * 0.01) +&
                        &(STG%CCF(ISPC)  * RDJ)
                   if (DEBUG) write(JOSTND,*) 'R: EDH-a= ', RDJ
                   EDH = EXP(MAX(-88.0,EDH)) * MtoFT
                   if (DEBUG) write(JOSTND,*) 'R: EDH-b= ', RDJ
                   if (DEBUG) write(JOSTND,*) 'REGENT: EDH3= ', EDH
                end if
             end if
             HK = HK + EDH
             55 continue
          end do

          EDH = HK-H
          P = PROB(I)
          TERM = HTG(I)*SCALE3
          SNP = SNP+P
          SNX = SNX+EDH*P
          SNY = SNY+TERM*P
          N = N+1

          !  PRINT DEBUG INFO IF DESIRED.

          if (DEBUG) write(JOSTND,9001) NPLT,I,ISPC,H,DBH(I),ICR(I),&
               &PCT(I),TEMCCF,RHCON(ISPC),EDH,TERM
          9001 format(' NPLT=',A26,',  I=',I5,',  ISPC=',I3,',  H=',F6.1,&
               &',  DBH=',F5.1,',  ICR',I5,',  PCT=',F6.1,',  RELDEN=',&
               &F6.1 / 12X,'RHCON=',F10.3,',  EDH=',F10.3,', TERM=',F10.3)

          !  END OF TREE LOOP WITHIN SPECIES.

          60 continue
       end do
       if (DEBUG) write(JOSTND,9010) ISPC,SNP,SNX,SNY
       9010 format(/' SUMS FOR SPECIES ',I2,':  SNP=',F10.2,&
            &';   SNX=',F10.2,';   SNY=',F10.2)

       !  COMPUTE CALIBRATION TERMS.  CALIBRATION TERMS ARE NOT COMPUTED
       !  IF THERE WERE FEWER THAN NCALHT (DEFAULT=5) HEIGHT INCREMENT
       !  OBSERVATIONS FOR A SPECIES.

       if (N.lt.NCALHT) go to 80

       !  CALCULATE MEANS FOR THE FOR THE GROWTH SAMPLE ON THE
       !  NATURAL SCALE.

       SNX = SNX/SNP
       SNY = SNY/SNP

       !  THE CORRECTION TERM IS THE MEAN OF OBSERVED INCREMENTS
       !  DIVIDED BY THE MEAN OF PREDICTED INCREMENTS (NOT THE REGRESSION
       !  ESTIMATE USED FOR DIAMETER INCREMENT CALIBRATION).

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
          9194 format(T28,' SMALL TREE HTG: SPECIES = ',I2,' (',A3,&
               &') CALCULATED CALIBRATION VALUE = ',F8.2)
          CORNEW = 1.0
          HCOR(ISPC) = 0.0
       end if
       80 continue
       CORTEM(IREFI) = CORNEW
       NUMCAL(IREFI) = N
       90 continue
    end do

    !  END OF CALIBRATION LOOP.  PRINT CALIBRATION STATISTICS AND RETURN

    write(JOSTND,9002) (NUMCAL(I),I = 1,NUMSP)
    9002 format(/' NUMBER OF RECORDS AVAILABLE FOR SCALING'/&
         &' THE SMALL TREE HEIGHT INCREMENT MODEL',&
         &((T48,11(I4,2X)/)))
    100 continue
    write(JOSTND,9003) (CORTEM(I),I = 1,NUMSP)
    9003 format(/' INITIAL SCALE FACTORS FOR THE SMALL TREE'/&
         &' HEIGHT INCREMENT MODEL',&
         &((T48,11(F5.2,1X)/)))

    ! OUTPUT CALIBRATION TO DATABASE.

    call REGCALOUT(CORTEM,NUMCAL,KOUT,K,KK,ISPEC,SPEC)
    return

    9998 continue

    !  ENTRY POINT FOR LOADING OF REGENERATION GROWTH MODEL
    !  CONSTANTS  THAT REQUIRE ONE-TIME RESOLUTION.
    !  IDTYPE IS A HABITAT INDEX THAT IS COMPUTED IN **RCON**.
    !  RHSC IS AN ARRAY FOR SPECIES CONSTANTS, RHGL IS AN ARRAY FOR
    !  LOCATION CONSTANTS, RHHAB IS AN ARRAY FOR HABITAT DEPENDENT
    !  CONSTANTS, AND, RSAB0, RSAB1, AND RSAB2 ARE COEFFICIENTS FOR
    !  THE SLOPE AND ASPECT TRANSFORMATION.

    data RHHAB/&
         &-0.2146, -0.0941, -0.3141,     0.0,     0.0,      0.0,&
         &-0.2146, -0.0941, -0.3296,     0.0,     0.0,      0.0,&
         &-0.2146, -0.0941, -0.5401, -0.3948,     0.0,      0.0,&
         &-0.2146, -0.0941, -0.2776,     0.0,     0.0,      0.0,&
         &-0.2146, -0.0941,     0.0,     0.0,     0.0,      0.0,&
         &-0.2146, -0.0941,     0.0,     0.0,     0.0,      0.0,&
         &-0.2146, -0.0941, -0.2484, -0.5134, -0.3495,      0.0,&
         &-0.2146, -0.0941, -0.3431,     0.0,     0.0,      0.0,&
         &-0.2146, -0.0941, -0.4916, -0.3582,     0.0,      0.0,&
         &-0.2146, -0.0941, -0.4345,     0.0,     0.0,      0.0,&
         &-0.2146, -0.0941, -0.3738,     0.0,     0.0,      0.0,&
         &-0.2146, -0.0941, -0.3738,     0.0,     0.0,      0.0,&
         &-0.2146, -0.0941, -0.3738,     0.0,     0.0,      0.0,&
         &-0.2146, -0.0941, -0.5401, -0.3948,     0.0,      0.0,& !OC=FD
         &-0.2146, -0.0941, -0.3738,     0.0,     0.0,      0.0/
    data MAPHAB/&
         &12*3, 1,2, 4*4, 1, 11*3,&
         &9*3, 4*1, 2, 4*4, 1, 11*3,&
         &7*4, 1, 3*4, 1,1,2, 4*5, 1,4,3, 6*4, 3,4,4,&
         &12*3, 1,2, 4*4, 1,3,3,1,1, 7*3,&
         &13*1, 2, 4*3, 12*1,&
         &13*1, 2, 4*3, 12*1,&
         &4*5, 3*1, 4*5, 4,4,2, 5*6, 4,4,3,4, 3*5, 4,4,5,5,&
         &13*1, 2, 4*4, 9*1, 3,1,1,&
         &12*4, 3,2, 4*5, 1, 7*4, 1,3,3,4,&
         &3*1, 3,3,1,1, 4*3, 1,1,2, 4*4, 1, 11*3,&
         &12*3, 1,2, 4*4, 1,3,3,2, 8*3,&
         &12*3, 1,2, 4*4, 1,3,3,2, 8*3,&
         &12*3, 1,2, 4*4, 1,3,3,2, 8*3,&
         &7*4, 1, 3*4, 1,1,2, 4*5, 1,4,3, 6*4, 3,4,4,& ! OC=FD
         &12*3, 1,2, 4*4, 1,3,3,2, 8*3/
    data  RHSC/&
         &1.4700,1.6204,1.4932,.9981,1.0202,.8953,1.2336,1.0964,1.0667,&
         &1.7311,.8953,.8953,.8953,1.4932,.8953/
    data  RHGL/-.2785,-.0480,0./
    data RSAB0,RSAB1,RSAB2/&
         &-0.10987,0.22157,-0.12432/

    if (LV2ATV) then
       REGCH = RHGL(IGL) +&
            &(RSAB0 + RSAB1*COS(ASPECT) + RSAB2*SIN(ASPECT))*SLOPE
       do ISPC = 1,MAXSP
          IRHHAB = MAPHAB(ITYPE,ISPC)
          RHCON(ISPC) = REGCH + RHSC(ISPC) + RHHAB(IRHHAB,ISPC)
          if (LRCOR2.and.RCOR2(ISPC).gt.0.0) RHCON(ISPC) = RHCON(ISPC) +&
               &ALOG(RCOR2(ISPC))

          !         SEI RECALIBRATION: IF THERE IS A SPECIES RE-FIT:
          !         v2SEICOR COMES FROM LARGE DIAM MODEL **DGF** (SPECIES SPECIFIC)
          !         SEISHT COMES FROM LARGE TREE HT MODEL **HTGF** (HABITAT SPECIFIC)

          if (LLTDGOK(ISPC)) then
             V2SEICOR(ISPC) = MAX(1.0E-6, V2SEICOR(ISPC))
             RHCON(ISPC) = RHCON(ISPC)+ SEISHT * LOG(V2SEICOR(ISPC))
          end if
       end do

    else

       !       REPLACE NULLS IN COMPILED DATA STRINGS WITH SPACES, SO THAT
       !       STRING LENGTHS WILL BE FIGURED CORRECTLY AND STRING COMPARISONS
       !       WILL WORK PROPERLY

       do J = 1, P_ZN
          do K = 1, P_ST
             do M = 1,P_ZL
                if (ICHAR(ST_COEF(J)%Zone(K)(M:M)) .eq.0)&
                     &ST_COEF(J)%Zone(K)(M:M) = ' '
             end do
          end do
       end do

       !       BEGIN SEARCH FOR ONE OF TEMESGEN'S MODELS

       do I = 1,MAXSP
          IPOS(I) = 0
          STG%FIT(I) = .false.
       end do
       PAT2 = BEC%PrettyName

       do I = 1,MAXSP

          IPAS = 0
          PAT1 = PAT2

          70 do J = 1, P_ZN
             do K = 1, MAXSP
                if (ST_COEF(J)%SPP(K) .eq.0) exit
                if (ST_COEF(J)%SPP(K) .eq. I) then
                   do M = 1, P_ST
                      if (LEN_TRIM(ST_COEF(J)%Zone(M))   .eq.0) exit
                      if (INDEX(ST_COEF(J)%Zone(M),PAT1) .gt.0) then
                         IPOS(I) = J
                         goto 71
                      end if
                   end do
                   exit
                end if
             end do
          end do

          if (J .gt. P_ZN .and. IPAS .eq.0) then
             IPAS = 1
             PAT1 = BEC%Zone(1:LEN_TRIM(BEC%Zone))//'/all '
             goto 70
          end if

       71 end do

       do I = 1,MAXSP
          J1 = IPOS(I)
          if (J1.gt.0) then
             STG%FIT(I) = .true.
             STG%CON(I) = ST_COEF(J1)%CON + (ST_COEF(J1)%SLP +&
                  &ST_COEF(J1)%CASP * COS(ASPECT) +&
                  &ST_COEF(J1)%SASP * SIN(ASPECT)) * SLOPE
             STG%LNHT(I) = ST_COEF(J1)%LNHT
             STG%HT(I) = ST_COEF(J1)%HT
             STG%CCF(I) = ST_COEF(J1)%CCF
             STG%BAL(I) = ST_COEF(J1)%BAL
             STG%EXPMLT(I) = ST_COEF(J1)%EXPMLT ! SBS only
             STG%HCCFL(I) = ST_COEF(J1)%HCCFL ! SBS only
             STG%CCFSL(I) = ST_COEF(J1)%CCFSL ! SBS only
             STG%SD(I) = ST_COEF(J1)%SD
          end if
       end do

       !       IF SCALE FACTORS HAVE BEEN INPUT WITH THE READCORR KEYWORD,
       !       INCORPORATE THEM INTO THE OVERALL CONSTANTS.

       do ISPC = 1,MAXSP
          RHCON(ISPC) = 0.0
          if (LRCOR2.and. RCOR2(ISPC) .gt.0.0)&
               &RHCON(ISPC) = ALOG(RCOR2(ISPC))
       end do

    end if

    do I = 1,MAXTRE
       ZRAND(I) = -999.0
    end do

    return
  end subroutine REGBC

  !===============================!
  !   Compute small tree growth   !
  !===============================!
  ! CANADA-ON $Id: regent.f 3958 2022-04-14 07:53:29Z nickcrookston $
  subroutine REGON(LCON)
    implicit none

    logical :: LCON
    ! Regional callees retain their native argument lists.
    interface
       subroutine HTCALC(N,IVAR,ISPC,SI,BA,YRS,H,AGET,HTMAX,HTG1,JOSTND,DEBUG,D,PCT1)
         integer :: N
         integer :: IVAR
         integer :: ISPC
         real :: SI
         real :: BA
         real :: YRS
         real :: H
         real :: AGET
         real :: HTMAX
         real :: HTG1
         integer :: JOSTND
         logical :: DEBUG
         real :: D
         real :: PCT1
       end subroutine HTCALC
    end interface
    interface
       subroutine BALMOD(ISPC,D,BA,RMSQD,GM,DEBUG)
         integer :: ISPC
         real :: D
         real :: BA
         real :: RMSQD
         real :: GM
         logical :: DEBUG
       end subroutine BALMOD
    end interface


    !  THIS SUBROUTINE COMPUTES HEIGHT AND DIAMETER INCREMENTS FOR
    !  SMALL TREES.  THE HEIGHT INCREMENT MODEL IS APPLIED TO TREES
    !  THAT ARE LESS THAN 12CM DBH,
    !  AND THE DBH INCREMENT MODEL IS APPLIED TO TREES THAT ARE LESS
    !  THAN 12CM DBH.  FOR TREES THAT ARE GREATER THAN 8CM
    !  DBH, HEIGHT INCREMENT PREDICTIONS
    !  ARE AVERAGED WITH THE PREDICTIONS FROM THE LARGE TREE MODEL.
    !  THIS ROUTINE IS CALLED FROM **CRATET** DURING CALIBRATION AND
    !  FROM **TREGRO** DURING CYCLING.  ENTRY **REGCON** IS CALLED FROM
    !  **RCON** TO LOAD MODEL PARAMETERS THAT NEED ONLY BE RESOLVED ONCE.

    ! Common blocks

    !  DIMENSIONS FOR INTERNAL VARIABLES:

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
    !     DIAM -- BUDWIDTH VALUES (INCHES) BY SPECIES

    !  DECLARATIONS
    !  SCALARS

    external RANN
    ! Variables
    real    :: AGET,AX,BARK,BX,CORNEW,CR,D,DDS,DK,DKK,DGMX
    real    :: EDH,H,HK,HTNEW,HTGR,HTG1,HTMAX,P,PTCCF,FNT,RAN
    real    :: REGYR,SCALE,SCALE2,SCALE3,SNP,SNX,SNY,TERM,XMX,XMN
    real    :: XRDGRO,XRHGRO,XWT,CON,YRS,GMOD,RELHTA,DGGR,DGSM
    real    :: D2,P2
    integer :: I,I1,I2,I3,ISPC,ISPEC,IPCCF,IREFI,K,KK,KOUT
    integer :: L,MODE0,N,IVAR
    character(len=2) :: SPEC
    logical :: DEBUG,LSKIPH

    !  ARRAYS

    real    :: CORTEM(72),DGMAX(72),DIAM(72),HGADJ(72)
    real    :: XMAX(72),XMIN(72)
    integer :: NUMCAL(72)

    !  FUNCTIONS

    real    :: BACHLO,BRATIO

    !  INITIALIZATIONS

    data HGADJ/ 72*1.0/
    data DGMAX/ 72*5.0 /
    ! new xmin/xmax = 8/12cm - September 2011, Penner 2010 model
    data XMIN / 72*3.15 /, XMAX/ 72*4.72 /
    data REGYR/10.0/

    if (LCON) go to 9998
    HTGR = 0.0
    AGET = 0

    !  CHECK FOR DEBUG.

    LSKIPH = .false.
    call DBCHK (DEBUG,'REGENT',6,ICYC)
    if (DEBUG) write(JOSTND,9980)ICYC,FINT
    9980 format(' ENTERING SUBROUTINE REGENT  CYCLE =',I5,F10.3)

    !  HEIGHT INCREMENT IS DERIVED FROM A HEIGHT-AGE CURVE AND IS NOMINALLY
    !  BASED ON A 10-YEAR GROWTH PERIOD.   SCALE IS USED TO CONVERT
    !  HEIGHT INCREMENT PREDICTIONS TO A FINT-YEAR PERIOD.  DIAMETER
    !  INCREMENT IS PREDICTED FROM CHANGE IN HEIGHT, AND IS SCALED TO A 10-
    !  YEAR PERIOD BY APPLICATION OF THE VARIABLE SCALE2. DIAMETER INCREMENT
    !  IS CONVERTED TO A FINT-YEAR BASIS IN **UPDATE**.

    FNT = FINT
    if (LSTART) FNT = IFINTH
    if (LESTB) then
       if (FINT.le.5.0) then
          LSKIPH = .true.
       else
          FNT = FNT-5.0
       end if
    end if

    !  IF THIS IS THE FIRST CALL TO REGENT, BRANCH TO STATEMENT 40 FOR
    !  MODEL CALIBRATION.

    if (LSTART) goto 40
    call MULTS (3,IY(ICYC),XRHMLT)
    call MULTS(6,IY(ICYC),XRDMLT)
    if (ITRN.le.0) go to 91

    SCALE = FNT/REGYR
    SCALE2 = YR/FNT

    !  ENTER GROWTH PREDICTION LOOP.  PROCESS EACH SPECIES AS A GROUP;
    !  LOAD CONSTANTS FOR NEXT SPECIES.

    do ISPC = 1,MAXSP
       I1 = ISCT(ISPC,1)
       if (I1.eq.0) go to 30
       I2 = ISCT(ISPC,2)

       XRDGRO = XRDMLT(ISPC)
       XRHGRO = XRHMLT(ISPC)
       CON = RHCON(ISPC) * EXP(HCOR(ISPC))
       XMX = XMAX(ISPC)
       XMN = XMIN(ISPC)
       DGMX = DGMAX(ISPC) * SCALE

       !  PROCESS NEXT TREE RECORD.

       do I3 = I1,I2
          I = IND1(I3)
          IPCCF = ITRE(I)
          D = DBH(I)
          if (D .ge. XMX) go to 25

          !  BYPASS INCREMENT CALCULATIONS IF CALLED FROM ESTAB AND THIS IS NOT A
          !  NEWLY CREATED TREE.

          if (LESTB) then
             if (I.lt.ITRNIN) go to 25

             !  ASSIGN CROWN RATIO FOR NEWLY ESTABLISHED TREES.

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
          H = HT(I)
          if (LSKIPH) then
             HTG(K) = 0.0
             go to 4
          end if

          !     RETURN HERE TO PROCESS NEXT TRIPLE.

          2 continue

          !  ESTIMATE SITE CURVE HEIGHT INCREMENT VALUES FOR INCOMING TREES.

          !  HEIGHT GROWTH EQUATION, USING NC128 TREE HEIGHT COEFFS.
          !  EVALUATED FOR EACH TREE EACH CYCLE, 10.0 YEAR CYCLE
          !  FIRST FIND EFFECTIVE TREE AGE BASED ON H

          MODE0 = 0
          IVAR = 3
          YRS = 10.
          H = HT(I)
          D2 = DBH(I)
          P2 = PCT(I)
          if (DEBUG) write(JOSTND,*)' IN REGENT 4 CALLING HTCALC'
          if (DEBUG) write(JOSTND,*)' MODE0', MODE0
          if (DEBUG) write(JOSTND,*)' IVAR', IVAR
          if (DEBUG) write(JOSTND,*)' ISPC', ISPC
          if (DEBUG) write(JOSTND,*)' SITEAR(ISPC)', SITEAR(ISPC)
          if (DEBUG) write(JOSTND,*)' BA', BA
          if (DEBUG) write(JOSTND,*)' YRS', YRS
          if (DEBUG) write(JOSTND,*)' H', H
          if (DEBUG) write(JOSTND,*)' AGET', AGET
          if (DEBUG) write(JOSTND,*)' HTMAX', HTMAX
          if (DEBUG) write(JOSTND,*)' HTMAX', HTMAX
          if (DEBUG) write(JOSTND,*)' JOSTND', JOSTND
          if (DEBUG) write(JOSTND,*)' DEBUG', DEBUG
          if (DEBUG) write(JOSTND,*)' D2', D2
          if (DEBUG) write(JOSTND,*)' P2', P2

          call HTCALC (MODE0,IVAR,ISPC,SITEAR(ISPC),BA,YRS,H,AGET,HTMAX,&
               &HTG1,JOSTND,DEBUG,D2,P2)

          !  IF INITIAL TREE HEIGHT IS >= MAX HEIGHT FOR SITE (HTMAX).
          !  IF TRUE, THEN SET HTG= 0.1 FT.

          if (HTMAX-H.le.1.) then
             if (DEBUG) write(JOSTND,*) ' HTI>=HTMAX , ABIRTH= ',ABIRTH(I)
             HTGR = 0.10
          else

             !     FIND HTGR

             MODE0 = 9
             IVAR = 3
             YRS = 10.
             H = HT(I)
             D2 = DBH(I)
             P2 = PCT(I)
             call HTCALC (MODE0,IVAR,ISPC,SITEAR(ISPC),BA,YRS,H,AGET,HTMAX,&
                  &HTGR,JOSTND,DEBUG,D2,P2)

             if (DEBUG)write(JOSTND,*)' I,ISPC,HGADJ,FNT,ABIRTH= ',&
                  &I,ISPC,HGADJ(ISPC),FNT,ABIRTH(I)
             if (DEBUG)write(JOSTND,*)' I,SCALE,SCALE2,HTGR,DBH,BA,IPCCF= ',&
                  &I,SCALE,SCALE2,HTGR,D,BA,IPCCF

             HTGR = HTGR*CON*SCALE*HGADJ(ISPC)*XRHGRO
          end if

          !  GET BAL MODIFIER BORROWED FROM THE DIAMETER GROWTH EQUATIONS AND
          !  ADJUST THE HEIGHT GROWTH ESTIMATE.
          !  POTENTIALLY ONLY AFFECT HEIGHT GROWTH HALF AS MUCH AS DIA GROWTH
          !  TEMPER THE ADJUSTMENT BY RELATIVE HEIGHT

          call BALMOD(ISPC,D,BA,RMSQD,GMOD,DEBUG)
          if (DEBUG)write(JOSTND,*)' AFTER BALMOD GMOD,HT,AVH= ',&
               &GMOD,HT(I),AVH
          !      GMOD = (GMOD+1.0)/2.0
          RELHTA = 0.
          if (AVH .gt.0.)RELHTA = MIN((HT(I)/AVH),1.0)
          GMOD = 1.0-((1.0-GMOD)*(1.0-RELHTA))
          HTGR = HTGR*GMOD
          if (DEBUG)write(JOSTND,*)' I,GMOD,HTGR= ',I,GMOD,HTGR

          if (HTGR .lt.0.1) HTGR = 0.1

          !     COMPUTE WEIGHTS FOR THE LARGE AND SMALL TREE HEIGHT INCREMENT
          !     ESTIMATES.  IF DBH IS LESS THAN OR EQUAL TO XMN, THE LARGE TREE
          !     PREDICTION IS IGNORED (XWT=0.0).

          XWT = (D-XMN)/(XMX-XMN)
          if (D.le.XMN .or. LESTB) XWT = 0.0

          !     COMPUTE WEIGHTED HEIGHT INCREMENT FOR NEXT TRIPLE.

          if (DEBUG)write(JOSTND,9982)XWT,HTGR,HTG(K),I,K
          9982 format(' IN REGENT 9982 FORMAT',3(F10.4,2X),2I7)

          HTGR = HTGR*(1.0-XWT) + XWT*HTG(K)
          if (HTGR .lt..1) HTGR = .1

          !   ADD RANDOM AFFECTS. LIMIT TO + OR -  1/10 HTGR

          RAN = 0.
          if (DGSD .ge.1.0) then
             3 continue
             RAN = BACHLO(0.0,1.0,RANN)
             if (RAN.lt.-1.0.or. RAN.gt.1.0) go to 3
          end if
          HTGR = HTGR + RAN*0.1*HTGR

          HTG(K) = HTGR
          if (HTG(K) .lt..1) HTG(K) = .1

          if (DEBUG)write(JOSTND,9985)HTGR,HTG(K),I,K,LESTB
          9985 format(' IN REGENT HTGR,HTG(K),I,K,LESTB= '/&
               &2(F10.4,2X),2I10,L10)

          ! CHECK FOR SIZE CAP COMPLIANCE.

          if ((H+HTG(K)).gt.SIZCAP(ISPC,4)) then
             HTG(K) = SIZCAP(ISPC,4)-H
             if (HTG(K) .lt.0.1) HTG(K) = 0.1
          end if

          4 continue

          !     ASSIGN DBH AND COMPUTE DBH INCREMENT FOR TREES WITH DBH LESS
          !     THAN 5 INCHES (COMPUTE 10-YEAR DBH INCREMENT REGARDLESS OF
          !     PROJECTION PERIOD LENGTH).

          if (D.ge.XMX) go to 23
          HK = H + HTG(K)
          if (HK .le.4.5) then
             DG(K) = 0.0
             DBH(K) = D+0.001*HK
          else

             !   USE WYKOFF EQ. FOR DBH
             !      DKK = DIAMETER WITH HTG ADDED TO THE STARTING HEIGHT
             !      DK  = DIAMETER AT THE START OF THE PROJECTION

             BX = HT2(ISPC)
             if (IABFLG(ISPC).eq.1) then
                AX = HT1(ISPC)
             else
                AX = AA(ISPC)
             end if
             DKK = (BX/(ALOG(HK-4.5)-AX))-1.0
             if (H .le.4.5) then
                DK = D
             else
                DK = (BX/(ALOG(H-4.5)-AX))-1.0
             end if

             IF(DEBUG)WRITE(JOSTND,*)' I,ISPC,DBH,H,HK,DK,DKK=               &
                  &  ',I,ISPC,DBH(K),H,HK,DK,DKK

             !  USE INVENTORY EQUATIONS IF CALIBRATION OF THE HT-DBH FUNCTION IS TURNED
             !  OFF, OR IF WYKOFF CALIBRATION DID NOT OCCUR.
             !  NOTE: THIS SIMPLIFIES TO IF(IABFLB(ISPC).EQ.1) BUT IS SHOWN IN IT'S
             !        ENTIRITY FOR CLARITY.

             if (.not.LHTDRG(ISPC) .or.&
                  &(LHTDRG(ISPC) .and. IABFLG(ISPC).eq.1)) then
                call HTDBH (IFOR,ISPC,DKK,HK,1)
                if (H .le.4.5) then
                   DK = D
                else
                   call HTDBH (IFOR,ISPC,DK,H,1)
                end if
                if (DEBUG)write(JOSTND,*)' INV EQN DUBBING IFOR,ISPC,H,HK,DK,'&
                     &,'DKK= ',IFOR,ISPC,H,HK,DK,DKK
                if (DEBUG)write(JOSTND,*)'ISPC,LHTDRG,IABFLG= ',&
                     &ISPC,LHTDRG(ISPC),IABFLG(ISPC)
             end if

             !         IF CALLED FROM **ESTAB** ASSIGN DIAMETER

             if (LESTB) then
                DBH(K) = DKK
                if (DBH(K).lt.DIAM(ISPC) .or. HK.lt.4.5) DBH(K) = DIAM(ISPC)
                DBH(K) = DBH(K)+0.001*HK
                DG(K) = DBH(K)
             else

                !         COMPUTE DIAMETER INCREMENT BY SUBTRACTION, APPLY USER
                !         SUPPLIED MULTIPLIERS, AND CHECK TO SEE IF COMPUTED VALUE
                !         IS WITHIN BOUNDS.
                !         THE INCREMENT FOR TREES BETWEEN 1.5 AND 5 INCHES DBH IS A
                !         WEIGHTED AVERAGE OF PREDICTIONS FROM THE LARGE AND SMALL
                !         TREE MODELS. SCALE ADJUSTMENT IS ON GROWTH IN DDS TERMS RATHER
                !         THAN INCHES OF DG TO MAINTAIN CONSISTENCY WITH GRADD.

                !         NOTE: LARGE TREE DG IS ON A 10-YR BASIS; SMALL TREE DG IS ON A
                !         FINT-YR BASIS. CONVERT SMALL TREE DG TO A 10-YR BASIS BEFORE
                !         WEIGHTING. DG GETS CONVERTED BACK TO A FINT-YR BASIS IN **GRADD**.

                BARK = BRATIO(ISPC,D,H)
                if (DEBUG)write(JOSTND,*)' BARK,XRDGRO= ',BARK,XRDGRO
                if (DK.lt.0.0.or. DKK.lt.0.0) then
                   DG(K) = HTG(K)*0.2*BARK*XRDGRO
                   DKK = D+DG(K)
                else
                   DGSM = (DKK-DK)*BARK*XRDGRO
                   if (DGSM .lt.0.0) DGSM = 0.0
                   DDS = DGSM*(2.0*BARK*D+DGSM)*SCALE2
                   DGSM = SQRT((D*BARK)**2+DDS)-BARK*D
                   if (DGSM.lt.0.0) DGSM = 0.0

                   !         COMPUTE WEIGHTED DIAMETER INCREMENT FOR NEXT TRIPLE.

                   if (DEBUG)write(JOSTND,9983)XWT,DGSM,DG(K),I,K
                   9983 format(' IN REGENT 9983 FORMAT',3(F10.4,2X),2I7)

                   DGGR = DGSM*(1.0-XWT) + XWT*DG(K)
                   if (DGGR .lt..1) DGGR = .1
                   DG(K) = DGGR
                end if
                if (DEBUG)write(JOSTND,*)' K,DK,DKK,DG= ',K,DK,DKK,DG(K)

                if (DG(K) .lt.0.0) DG(K) = 0.1
                if (DG(K) .gt. DGMX) DG(K) = DGMX

             end if
             if ((DBH(K)+DG(K)).lt.DIAM(ISPC)) then
                DG(K) = DIAM(ISPC)-DBH(K)
             end if
          end if

          !  CHECK FOR TREE SIZE CAP COMPLIANCE

          call DGBND(ISPC,DBH(K),DG(K))

          if (DEBUG) then
             HTNEW = HT(I)+HTG(I)
             write(JOSTND,9987) I,ISPC,HT(I),HTG(I),HTNEW,DBH(I),DG(I),DG(K)
             9987 format(' IN REGENT, I=',I4,',  ISPC=',I3,'  CUR HT=',F7.2,&
                  &',  HT INC=',F7.4,',  NEW HT=',F7.2,',  CUR DBH=',F10.5,&
                  &',  DBH INC(I,K)=',2F7.4)
          end if
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
    SCALE3 = REGYR / FINTH

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
       CON = RHCON(ISPC)

       !  BEGIN TREE LOOP WITHIN SPECIES.  IF MEASURED HEIGHT INCREMENT IS
       !  LESS THAN OR EQUAL TO ZERO, OR DBH IS LESS THAN 5.0, THE RECORD
       !  WILL BE EXCLUDED FROM THE CALIBRATION.

       do I3 = I1,I2
          I = IND1(I3)
          H = HT(I)
          K = I
          if (IHTG.lt.2) H = H-HTG(I)
          if (DBH(I).ge.5.0.or.H.lt.0.01) go to 60
          IPCCF = ITRE(I)
          PTCCF = PCCF(IPCCF)

          if (DEBUG) write(JOSTND,*)' IN REGENT 8900 H =',H

          !  CALCULATE TREE HEIGHT INCREMENT
          !  HEIGHT GROWTH EQUATION, USING NC128 TREE HEIGHT COEFFS.
          !  EVALUATED FOR EACH TREE EACH CYCLE, 5.0 YEAR CYCLE
          !  FIRST FIND EFFECTIVE TREE AGE BASED ON H

          MODE0 = 0
          IVAR = 3
          YRS = 10.
          H = HT(I)
          D2 = DBH(I)
          P2 = PCT(I)
          call HTCALC (MODE0,IVAR,ISPC,SITEAR(ISPC),BA,YRS,H,AGET,HTMAX,&
               &HTG1,JOSTND,DEBUG,D2,P2)

          !     THEN FIND HTGR

          MODE0 = 9
          IVAR = 3
          YRS = 10.
          H = HT(I)
          D2 = DBH(I)
          P2 = PCT(I)
          call HTCALC (MODE0,IVAR,ISPC,SITEAR(ISPC),BA,YRS,H,AGET,HTMAX,&
               &HTGR,JOSTND,DEBUG,D2,P2)

          !  GET BAL MODIFIER BORROWED FROM THE DIAMETER GROWTH EQUATIONS AND
          !  ADJUST THE HEIGHT GROWTH ESTIMATE.
          !  POTENTIALLY ONLY AFFECT HEIGHT GROWTH HALF AS MUCH AS DIA GROWTH
          !  TEMPER THE ADJUSTMENT BY RELATIVE HEIGHT

          call BALMOD(ISPC,D,BA,RMSQD,GMOD,DEBUG)
          if (DEBUG)write(JOSTND,*)' AFTER BALMOD GMOD,HT,AVH= ',GMOD,HT(I),&
               &AVH
          !      GMOD = (GMOD+1.0)/2.0
          RELHTA = 0.
          if (AVH .gt.0.)RELHTA = MIN((HT(I)/AVH),1.0)
          GMOD = 1.0-((1.0-GMOD)*(1.0-RELHTA))
          HTGR = HTGR*GMOD
          if (DEBUG)write(JOSTND,*)' I,GMOD,HTGR= ',I,GMOD,HTGR

          if (HTGR .lt..1) HTGR = .1

          EDH = HTGR * RHCON(ISPC)
          if (EDH .lt.0.1) EDH = 0.1
          P = PROB(I)
          if (HTG(I).lt.0.001) go to 60
          TERM = HTG(I) * SCALE3
          SNP = SNP+P
          SNX = SNX+EDH*P
          SNY = SNY+TERM*P
          N = N+1

          !  PRINT DEBUG INFO IF DESIRED.

          if (DEBUG)write(JOSTND,9991) NPLT,I,ISPC,H,DBH(I),ICR(I),&
               &PCT(I),RELDM1,RHCON(ISPC),EDH,TERM,HTGR,SCALE3,HGADJ(ISPC),&
               &ICR(I)
          9991 format(' NPLT=',A8,',  I=',I5,',  ISPC=',I3,',  H=',F6.1,&
               &',  DBH=',F5.1,',  ICR',I5,',  PCT=',F6.1,',  RELDEN=',&
               &F6.1 / 13X,'RHCON=',F10.3,',  EDH=',F10.3,', TERM=',F10.3,&
               &' HTGR= ',F10.3,' SCALE3= ',F10.3,' HGADJ(ISPC)= ',F10.3/&
               &' ICR= ',I5)

          !  END OF TREE LOOP WITHIN SPECIES.

          60 continue
       end do
       if (DEBUG) write(JOSTND,9992) ISPC,SNP,SNX,SNY
       9992 format(/' SUMS FOR SPECIES ',I2,':  SNP=',F10.2,&
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
          9194 format(T28,' SMALL TREE HTG: SPECIES = ',I2,' (',A3,&
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
    9993 format(/' NUMBER OF RECORDS AVAILABLE FOR SCALING'/&
         &' THE SMALL TREE HEIGHT INCREMENT MODEL',&
         &((T49,11(I4,2X)/)))
    write(JOSTND,9994) (CORTEM(I),I = 1,NUMSP)
    9994 format(/' INITIAL SCALE FACTORS FOR THE SMALL TREE'/&
         &' HEIGHT INCREMENT MODEL',&
         &((T49,11(F5.2,1X)/)))

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
    9995 format(' LEAVING SUBROUTINE REGENT  CYCLE =',I5)
    return
    9998 continue

    !  ENTRY POINT FOR LOADING OF REGENERATION GROWTH MODEL
    !  CONSTANTS  THAT REQUIRE ONE-TIME RESOLUTION.
    !  IF THE BUDWIDTH VALUES IN THE DIAM ARRAY, BELOW, ARE MODIFIED
    !  THEN ALSO MODIFY THE VALUES IN THE SNDBAL ARRAY IN **HTDBH

    data DIAM/&
         &0.1, 0.5, 0.1, 0.1, 0.4, 0.2, 0.2, 0.1, 0.2, 0.1,&
         &0.1, 0.1, 0.3, 0.5, 0.2, 0.2, 0.1, 0.2, 0.2, 0.1,&
         &0.1, 0.1, 0.1, 0.1, 0.1, 0.2, 0.2, 0.1, 0.2, 0.2,&
         &0.1, 0.2, 0.1, 0.2, 0.2, 0.2, 0.3, 0.3, 0.3, 0.2,&
         &0.3, 0.2, 0.1, 0.2, 0.3, 0.4, 0.2, 0.1, 0.1, 0.3,&
         &0.1, 0.1, 0.2, 0.1, 0.1, 0.1, 0.1, 0.2, 0.2, 0.1,&
         &0.1, 0.1, 0.2, 0.1, 0.1, 0.1, 0.1, 0.1,&
         &0.1, 0.4, 0.2, 0.2/

    do ISPC = 1,MAXSP
       RHCON(ISPC) = 1.0
       if (LRCOR2.and.RCOR2(ISPC).gt.0.0)&
            &RHCON(ISPC) = RCOR2(ISPC)
       90 continue
    end do
    do I = 1,MAXTRE
       ZRAND(I) = -999.0
    end do
    return
  end subroutine REGON

  !===============================!
  !   Compute small tree growth   !
  !===============================!
  ! CI $Id$
  subroutine REGCI(LCON)
    implicit none

    logical :: LCON
    ! Regional callees retain their native argument lists.
    interface
       subroutine SMHTGF(ISPC,I,HTGRTH,CR,TPCCF,LESTB,DEBUG)
         integer :: ISPC
         integer :: I
         real :: HTGRTH
         real :: CR
         real :: TPCCF
         logical :: LESTB
         logical :: DEBUG
       end subroutine SMHTGF
    end interface


    !  **REGENT** COMPUTES HEIGHT AND DIAMETER INCREMENTS FOR SMALL TREES.

    !  HEIGHT INCREMENT IS A FUNCTION OF HABITAT TYPE, SLOPE, ASPECT, TREE
    !  HEIGHT, BASAL AREA IN LARGER TREES, AND STAND CROWN COMPETITION FACTOR.

    !  THE HEIGHT INCREMENT MODEL IS APPLIED TO TREES THAT ARE LESS
    !  THAN THE MAXIMUM DIAMETER SPECIFIED IN ARRAY XMAX.
    !  FOR TREES THAT ARE GREATER THAN THE MINIMUM DIAMETER SPECIFIED IN
    !  ARRAY XMIN, HEIGHT INCREMENT PREDICTIONS ARE AVERAGED WITH PREDICTIONS
    !  FROM THE LARGE TREE MODEL. HEIGHT INCREMENT IS SCALED TO THE CYCLE
    !  LENGTH (FINT) BY APPLICATION OF THE VARIABLE SCALE.

    !  DIAMETER IS ASSIGNED FROM A HEIGHT DIAMETER FUNCTION WITH ADJUSTMENTS
    !  FOR RELATIVE SIZE AND STAND DENSITY.  INCREMENT IS COMPUTED BY
    !  SUBTRACTION. THE DBH INCREMENT MODEL IS APPLIED TO TREES THAT ARE LESS
    !  THAN THE BREAKPOINT DIAMETER SPECIFIED IN ARRAY BREAK IN **BLKDAT**.
    !  DIAMETER INCREMENT IS SCALED FROM A FINT-YEAR BASIS TO A 10-YR BASIS
    !  BY APPLICATION OF THE VARIABLE SCALE2. IT GETS SCALED BACK TO A
    !  FINT-YEAR BASIS IN SUBROUTINE **UPDATE**.

    !  WHEN THE TREE ATTAINS A DBH OF BREAK INCHES IN A CYCLE, **DUBSCR** IS
    !  CALLED TO ASSIGN A CROWN RATIO.

    !  **REGENT** IS CALLED FROM **CRATET** DURING CALIBRATION AND
    !  FROM **GRINCR** DURING CYCLING.  ENTRY **REGCON** IS CALLED FROM
    !  **RCON** TO LOAD MODEL PARAMETERS THAT NEED ONLY BE RESOLVED ONCE.

    !  **REGENT** IS ALSO USED TO PREDICT HEIGHT INCREMENT, DBH AND CROWN
    !  RATIO FOR TREES THAT ARE CREATED BY THE REGENERATION ESTABLISHMENT
    !  MODEL.  5-YEAR OLD TREES ARE GENERATED BY **ESTAB**, AND **REGENT**
    !  COMPUTES INCREMENTS FROM AGE 5 TO THE END OF THE CYCLE.

    ! Common blocks

    !  DIMENSIONS FOR INTERNAL VARIABLES:

    !   BANEXT -- ESTIMATED STAND BASAL AREA AT START OF EACH HEIGHT INCREMENT
    !             PREDICTION SUBCYCLE.
    !   CORTEM -- A TEMPORARY ARRAY FOR PRINTING CORRECTION TERMS.
    !     KPER -- NUMBER OF YEARS (MAX OF 5) IN EACH HEIGHT INCREMENT PREDICTION
    !             SUBCYCLE.
    !   NUMCAL -- A TEMPORARY ARRAY FOR PRINTING NUMBER OF HEIGHT
    !             INCREMENT OBSERVATIONS BY SPECIES.
    !   RDNEXT -- RELATIVE DENSITY (CCF) AT START OF EACH HEIGHT INCREMENT
    !             PREDICTION SUBCYCLE.
    !    RHCON -- CONSTANT FOR THE HEIGHT INCREMENT MODEL.  INCLUDES EFFECTS
    !             OF HABITAT TYPE, LOCATION, SLOPE, AND ASPECT AS LOADED IN
    !             RCON.  HABITAT EFFECTS ARE BORROWED FROM LARGE TREE DBH
    !             INCREMENT MODEL FOR ALL BUT TYPES 520, 530, AND 570.
    !             ADJUSTMENTS ARE THE DIFFERENCE BETWEEN THE HABITAT
    !             CONSTANT FOR A GIVEN TYPE AND THE CONSTANT FOR TYPE 520.
    !     XMAX -- UPPER END OF THE RANGE OF DIAMETERS OVER WHICH HEIGHT
    !             INCREMENT PREDICTIONS FROM SMALL AND LARGE TREE MODELS
    !             ARE AVERAGED.
    !     XMIN -- LOWER END OF THE RANGE OF DIAMETERS OVER WHICH HEIGHT
    !             INCREMENT PREDICTIONS FROM THE SMALL AND LARGE TREE
    !             ARE AVERAGED.

    !  NOTE: UT AND CR LOGIC IS BASED ON A 10-YEAR REGYR; SO IS ASPEN

    external RANN
    ! Variables
    character(len=2) :: SPEC
    logical :: DEBUG,LSKIPH,CIVAR,UTVAR,TTVAR

    integer :: I,I1,I2,I3,ICRK,IICR,IS,IP,IPCCF,IREFI,ISPC
    integer :: ISPEC,ITOT,IYR
    integer :: J
    integer :: K,KK,KOUT,KPER(10),KY
    integer :: L
    integer :: N,NPER,NTYR,NUMCAL(19),NYR

    real    :: AG1,AG2,AH,AGMAX,AX
    real    :: B1,B2,BACHLO,BACON,BAJ,BAL(19),BANEXT(10),BARK,BAYR
    real    :: BI,BRATIO,BKPT,BX
    real    :: C1,C2,CCFYR,CI,CNST(19),CON,CORNEW,CORTEM(19)
    real    :: CR(19),CRCODE,CRI,CRSQ(19),CW
    real    :: D,D1,D1F,D2,D2F,DAT45,DDS,DELMAX
    real    :: DGMX,DHCN(19),DHCR(19),DHHT(19)
    real    :: DIAM(19),DK,DKK,EDH,FNT,DGMAX(19)
    real    :: H,H1,H2,HAT3,HDM1(19),HDM2(19)
    real    :: HITE1,HITE2,HK,HLESS4
    real    :: HTBA(19),HTGR,HTGR1,HTGRL,HTMAX,HTMAX2
    real    :: P,P2,P3,P4,PBAL(19),PCTRED,PN,POTHTG
    real    :: PPCCF,PTBA(19),PTBALI
    real    :: R,RAN,RCR,RD,RDJ,RDNEXT(10),REGYR,RELHT,RELSI
    real    :: RHDM1,RHDM2,RLHT(19)
    real    :: RSIMOD
    real    :: SBA,SBAL,SCALE,SCALE2,SCALE3,SCNST,SCR,SCRSQ,SDHCN,SDHHT
    real    :: SDHCR,SHDM1,SHDM2,SHI,SHTBA,SI,SITAGE,SITHT,SJ
    real    :: SLO
    real    :: SNP,SNX,SNY,SPBAL,SPTBA,SRLHT,STBA(19)
    real    :: TBAL,TEMAHT,TEMBA,TEMCCF,TERM,TPCCF,VIGOR
    real    :: X,XMAX(19),XMIN(19),XMN,XMX,XRDGRO,XRHGRO,XWT
    real    :: ZBAL,ZZRAN

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

    ! Data statements
    data XMIN/&
         &2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0,&
         &1.5, 1.5, 2.0, 90., 90., 1.5, 0.5, 2.0, 0.5/
    data XMAX/&
         &5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0,&
         &3.0, 3.0, 4.0, 99., 99., 3.0, 2.0, 5.0, 2.0/

    data REGYR/ 5.0 /

    data BACON/0.005454154/

    data DIAM/&
         &0.4, 0.3, 0.3, 0.3, 0.2, 0.2, 0.4, 0.3, 0.3, 0.5,&
         &0.3, 0.3, 0.2, 0.3, 0.2, 0.3, 0.2, 0.2, 0.2/

    data PTBA/&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,       0.0,       0.0,   .041624,       0.0,&
         &0.,        0.,        0.,        0.,        0.,&
         &0.,        0.,       0.0,        0./

    data HDM1/&
         &0.0,       0.0,       0.0,  -.148242,  -.148242,&
         &0.0,  1.344571,       0.0,       0.0,       0.0,&
         &0.,        0.,        0.,        0.,        0.,&
         &0.,        0.,  1.344571,        0./

    data HDM2/&
         &0.0,       0.0,       0.0,       0.0,       0.0,&
         &0.0,   .941685,  -.170390,  -.351491,       0.0,&
         &0.,        0.,        0.,        0.,        0.,&
         &0.,        0.,   .941685,        0./

    data BAL/&
         &-.004243,  -.004243,  -.004243,       0.0,       0.0,&
         &-.004243,   .046846,  -.002575,       0.0,       0.0,&
         &0.,        0.,        0.,        0.,        0.,&
         &0.,        0.,   .046846,        0./

    data CR/&
         &0.0,       0.0,       0.0,  -.056683,  -.056683,&
         &0.0,  -.429924,  -.240189,  -.184459,       0.0,&
         &0.,        0.,        0.,        0.,        0.,&
         &0.,        0.,  -.429924,        0./

    data CRSQ/&
         &.019979,   .019979,   .019979,   .027878,   .027878,&
         &.019979,   .059260,   .033463,   .030547,   .025303,&
         &0.,        0.,        0.,        0.,        0.,&
         &0.,        0.,   .059260,        0./

    data RLHT/&
         &1.360483,  1.360483,  1.360483,   .625564,   .625564,&
         &1.360483,  2.559428,  1.564875,  1.919543,        .0,&
         &0.,        0.,        0.,        0.,        0.,&
         &0.,        0.,  2.559428,        0./

    data STBA/&
         &0.0,       0.0,       0.0,  -.001581,  -.001581,&
         &0.0,  -.057022,       0.0,       0.0,  -.009643,&
         &0.,        0.,        0.,        0.,        0.,&
         &0.,        0.,  -.057022,        0./

    data PBAL/&
         &-.002027,  -.002027,  -.002027,  -.001873,  -.001873,&
         &-.002027,       0.0,  -.001630,  -.043304,       0.0,&
         &0.,        0.,        0.,        0.,        0.,&
         &0.,        0.,       0.0,        0./

    data HTBA/&
         &0.0,       0.0,       0.0,   .007883,   .007883,&
         &0.0,  -.010395,       0.0,       0.0,       0.0,&
         &0.,        0.,        0.,        0.,        0.,&
         &0.,        0.,  -.010395,        0./

    data CNST/&
         &1.598975,  1.598975,  1.598975,  1.222128,  1.222128,&
         &1.598975,  2.133995,  1.768804,  1.253349,  2.764559,&
         &0.,        0.,        0.,        0.,        0.,&
         &0.,        0.,  2.133995,        0./

    data DHCN/&
         &-1.10700,  -2.47253,  -2.91444,  -2.44705,  -2.23082,&
         &-2.23082,  -2.34117,  -2.51259,  -2.59515,  -1.10700,&
         &0.,        0.,        0.,        0.,        0.,&
         &0.,        0.,  -2.23082,        0./

    data DHHT/&
         &.830144,  1.045526,  1.234512,  1.154547,  1.173253,&
         &1.173253,   .957510,  1.151275,  1.236187,   .830144,&
         &0.,        0.,        0.,        0.,        0.,&
         &0.,        0.,  1.173253,        0./

    data DHCR/&
         &0.0,       0.0,   .098784,   .019108,   .010090,&
         &0.0,   .191396,  0.071330,       0.0,       0.0,&
         &0.,        0.,        0.,        0.,        0.,&
         &0.,        0.,       0.0,        0./

    data DGMAX/&
         &99.,  99.,  99.,  99.,  99.,  99.,  99.,  99.,  99.,  99.,&
         &2.0,  2.0,  2.5,  2.0,  2.0,  2.0,  2.5,  99.,  2.5/

    if (LCON) go to 9998

    !  CHECK FOR DEBUG.

    call DBCHK (DEBUG,'REGENT',6,ICYC)
    if (DEBUG) write(JOSTND,9980)ICYC
    9980 format('ENTERING SUBROUTINE REGENT  CYCLE =',I5)

    !  THE SMALL TREE HEIGHT INCREMENT MODEL IS BASED ON 5-YEAR INCREMENT DATA.
    !  TO AVOID BIAS, PREDICTIONS ARE MADE FOR SUBCYCLES THAT ARE LESS THAN OR
    !  EQUAL TO 5 YEARS IN LENGTH.  THE FOLLOWING CODE DETERMINES THE NUMBER
    !  AND LENGTH OF SUBCYCLES FROM CYCLE LENGTH.

    LSKIPH = .false.
    SCALE2 = 1.
    do I = 1,10
       KPER(I) = 0
       BANEXT(I) = 0.0
       RDNEXT(I) = 0.0
       1 continue
    end do
    NTYR = INT(FINT)
    if (LSTART) NTYR = IFINTH
    if (LESTB) NTYR = NTYR-5
    if (LESTB.and.NTYR.le.0) LSKIPH = .true.
    IYR = INT(REGYR)
    NPER = NTYR/IYR
    if (MOD(NTYR,IYR).ne.0) NPER = NPER+1
    ITOT = NTYR
    N = NPER
    do I = 1,NPER
       if (N.eq.1) go to 3
       KPER(I) = ITOT/N
       ITOT = ITOT-KPER(I)
       N = N-1
       2 continue
    end do
    3 continue
    if (NPER.gt.0)KPER(NPER) = ITOT

    !  LOAD TEMPORARY VARIABLES WITH AFTER THINNING BA, CCF, AND TOPHT.

    TEMBA = ATBA
    if (ATBA .le.0.)TEMBA = BA
    TEMCCF = ATCCF
    if (ATCCF .le.0.)TEMCCF = RELDEN
    TEMAHT = ATAVH
    if (DEBUG)write(16,*)'TEMBA,TEMCCF,TEMAHT,BA,RELDEN= ',&
         &TEMBA,TEMCCF,TEMAHT,BA,RELDEN

    !  SET VARIABLES WHICH ARE HABITAT DEPENDENT.

    RHDM1 = 0.0
    RHDM2 = 0.0
    if (ICL5.ge.500.and. ICL5.lt.600)RHDM1 = 1.0
    if (ICL5.ge.600.and. ICL5.lt.700)RHDM2 = 1.0

    !  IF THIS IS THE FIRST ENTRY, BRANCH TO THE CALIBRATION SECTION.

    if (LSTART) goto 40

    !  LOAD MULTIPLIERS.

    call MULTS (3,IY(ICYC),XRHMLT)
    call MULTS(6,IY(ICYC),XRDMLT)
    if (DEBUG) write(JOSTND,8996) LESTB,LSKIPH,ITRN,ITRNIN,NPER
    8996 format('IN REGENT, LESTB = ',L1,', LSKIPH = ',L1,&
         &', ITRN = ',I4,', ITRNIN = ',I4,', NPER = ',I3)
    if (ITRN.le.0) go to 91

    !  ESTIMATE THE CONTRIBUTION TO STAND BASAL AREA AND CCF FOR LARGE TREES AT
    !  THE START OF EACH PROJECTION SUBCYCLE.  IF CALLED FROM **ESTAB**, BRANCH
    !  TO STATEMENT 9 AND INTERPOLATE FROM OLD AND NEW DENSITY VALUES.
    !  OTHERWISE, 10-YEAR CHANGE IS COMPUTED FROM PREDICTED DBH INCREMENT AND
    !  ANNUALIZED.  THEN, ANNUAL RATE IS MULTIPLIED BY SUBCYCLE LENGTH TO
    !  DETERMINE CONTRIBUTION FOR EACH SUBCYCLE.  A ONE PERCENT ANNUAL RATE
    !  OF MORTALITY IS ASSUMED.  CONTRIBUTIONS OF SMALL TREES ARE COMPUTED AS
    !  INCREMENT PREDICTIONS ARE MADE FOR PRECEDING SUBCYCLE.

    if (LESTB) go to 8
    do J = 1,NPER
       BANEXT(J) = BA
       RDNEXT(J) = RELDEN
       4 continue
    end do
    if (DEBUG) write(JOSTND,*)'NPER,BA,RELDEN= ',NPER,BA,RELDEN
    if (NPER.eq.1) go to 11
    do I = 1,ITRN
       D1 = DBH(I)
       if (D1.lt.3.0) go to 6
       IS = ISP(I)
       P = PROB(I)
       D2 = D1+DG(I)/BRATIO(IS,D1,HT(I))
       B1 = BACON*D1*D1
       B2 = BACON*D2*D2
       call CCFCAL(IS,D1,HT(I),ICR(I),P,.false.,C1,CW,1)
       call CCFCAL(IS,D2,HT(I),ICR(I),P,.false.,C2,CW,1)
       BI = (B2-B1)/10.0
       CI = (C2-C1)/10.0
       K = 0
       do J = 2,NPER
          K = K+KPER(J-1)
          PN = P*0.985**K
          RDNEXT(J) = RDNEXT(J)+K*CI/P*PN
          BANEXT(J) = BANEXT(J)+K*BI*PN
          5 continue
       end do
       6 continue
    end do
    if (DEBUG)write(16,*)'AFTER 6 NPER,RDNEXT1-2, BANEXT1-2= ',&
         &NPER,RDNEXT(1),RDNEXT(2),BANEXT(1),BANEXT(2)
    go to 11

    !  IF CALLED FROM **ESTAB**, DIAMETERS AND DENSITY STATISTICS HAVE
    !  BEEN UPDATED AND DENSITIES CORRESPONDING TO BEGINNINGS OF SUBCYCLES
    !  CAN BE INTERPOLATED FROM DIFFERENCE BETWEEN OLD AND NEW STATS.  THE
    !  TREES ARE SENT TO REGENT WITH A HEIGHT CORRESPONDING TO 5 YEARS AFTER
    !  THE START OF THE CYCLE.

    8 continue
    BAYR = 0.0
    CCFYR = 0.0
    if (LSKIPH) go to 9
    BAYR = (BA-TEMBA)/ITOT
    CCFYR = (RELDEN-TEMCCF)/ITOT
    9 continue
    NYR = 5
    do J = 1,NPER
       RDNEXT(J) = TEMCCF+NYR*CCFYR
       BANEXT(J) = TEMBA+NYR*BAYR
       NYR = NYR+KPER(J)
       10 continue
    end do
    if (DEBUG)write(16,*)'AFTER 10 NPER,RDNEXT1-2, BANEXT1-2= ',&
         &NPER,RDNEXT(1),RDNEXT(2),BANEXT(1),BANEXT(2)
    11 continue

    !  STORE INITIAL HEIGHTS IN WK3; DUB CROWN RATIO FOR NEWLY ESTABLISHED
    !  SEEDLINGS.

    do I = 1,ITRN
       if (LESTB.and.I.ge.ITRNIN) then
          IPCCF = ITRE(I)
          CRI = 0.89722 - 0.0000461*PCCF(IPCCF)
          12 continue
          RAN = BACHLO(0.0,1.0,RANN)
          if (RAN .lt. -1.0.or. RAN .gt.1.0) go to 12
          CRI = CRI + 0.07985 * RAN
          if (CRI .gt..90) CRI = .90
          if (CRI .lt..20) CRI = .20
          ICR(I) = INT((CRI*100.0)+0.5)
          IF(DEBUG)WRITE(JOSTND,*)'DUB CROWN ESTAB TREES I,PCCF,RAN,CRI,I &
               &  CR= ',I,PCCF(IPCCF),RAN,CRI,ICR(I)
       end if
       WK3(I) = HT(I)
       WK5(I) = DBH(I)
       13 continue
    end do

    !  DELMAX IS USED TO COMPUTE A DENSITY-DEPENDENT ADJUSTMENT FOR
    !  THE HEIGHT-DIAMETER FUNCTION.  DELMAX IS BASED ON AVERAGE HEIGHT AND
    !  CCF AT THE BEGINNING OF THE CYCLE AND DOES NOT VARY BY SUBCYCLE.

    if (LESTB) then
       R = TEMCCF
       AH = TEMAHT
    else
       R = RELDEN
       AH = AVH
    end if
    DELMAX = (AH/36.0)*(0.01232*R-1.75)
    if (DELMAX.gt.0.0) DELMAX = 0.0

    ! COMPUTE DENSITY MODIFIER FROM CCF AND TOP HEIGHT FOR CR & UT VARIANT

    X = AH*(R/100.0)
    if (X .gt.300.0) X = 300.0
    PCTRED = 1.11436 + X*(-.011493 + X*(.43012E-4 + X*(-.72221E-7&
         &+ X*(.5607E-10- X*0.1641E-13))))
    if (PCTRED .gt.1.0) PCTRED = 1.0
    if (PCTRED .lt.0.01) PCTRED = 0.01
    if (DEBUG) write(JOSTND,9982) AH,R,X,PCTRED
    9982 format('IN REGENT AVHT,CCF,X,PCTRED = ',4F10.4)
    if (DEBUG) write(JOSTND,8995) R,AH,DELMAX
    8995 format('R = ',F10.4,', AH = ',F10.4,', DELMAX = ',F10.4)

    !  ENTER LOOP FOR NEXT SUBCYCLE.
    !  AFTER ALL SUBCYCLES ARE COMPLETE, THE END OF CYCLE HEIGHT SHOULD
    !  BE IN ARRAY WK3 AND SCALED TO THE APPROPRIATE CYCLE LENGTH.
    !  END OF CYCLE DIAMETER IS STORED IN ARRAY WK5

    KY = 0
    do J = 1,NPER
       KY = KY+KPER(J)
       BAJ = BANEXT(J)
       RDJ = RDNEXT(J)

       !  PPCCF IS A PROPORTIONAL ADJUSTMENT FOR POINT CCF VALUES BASED ON
       !  CHANGE IN STAND CCF FOR THE SUBCYCLE.

       PPCCF = 0.0
       if (RELDEN.gt.0.0) PPCCF = 1.0+(RDJ-RELDEN)/RELDEN
       if (DEBUG)write(16,*)'J,KY,BAJ,RDJ,RELDEN,PPCCF= ',&
            &J,KY,BAJ,RDJ,RELDEN,PPCCF

       !  PROCESS ALL TREES WITHIN THIS SUBCYCLE IN SPECIES ORDER.  LOAD
       !  SPECIES DEPENDENT CONSTANTS.

       do ISPC = 1,MAXSP

          !  GET THE APPROPRIATE SITE INDEX RANGE VALUES FOR THIS SPECIES.

          SLO = 0.
          SHI = 999.
          call SITERANGE (2,ISPC,SLO,SHI)

          I1 = ISCT(ISPC,1)
          if (I1.eq.0) go to 16
          I2 = ISCT(ISPC,2)
          select case (ISPC)
          case(17,19)
             BKPT = 1.0
          case(14,15)
             BKPT = 99.0
          case default
             BKPT = 3.0
          end select

          !  ASSIGN VARIANT ACCORDING TO SPECIES
          !  SET SCALE AND SCALE2 DEPENDING ON WHAT VARIANT THE EQUATIONS FOR
          !  THE SPECIES CAME FROM.

          CIVAR = .false.
          TTVAR = .false.
          UTVAR = .false.

          select case (ISPC)
          case(11,12,16)
             TTVAR = .true.
             SCALE = FLOAT(KPER(J))/REGYR
             if (.not.LSKIPH)SCALE2 = YR/FLOAT(NTYR)
          case(13,14,15,17,19)
             UTVAR = .true.
             SCALE = FLOAT(NTYR)/YR
             if (.not.LSKIPH)SCALE2 = YR/FLOAT(NTYR)
          case default
             CIVAR = .true.
             SCALE = FLOAT(NTYR)/REGYR
             if (.not.LSKIPH)SCALE2 = YR/FLOAT(NTYR)
          end select

          if (DEBUG)write(JOSTND,*)'CIVAR,TTVAR,UTVAR,LSKIPH,SCALE,',&
               &'SCALE2= ',CIVAR,TTVAR,UTVAR,LSKIPH,SCALE,SCALE2
          if (TTVAR .and. LSKIPH)goto 16

          !  SPECIES BASED ON A 10-YR REGYR ONLY NEED ONE PASS THROUGH THIS LOGIC

          if ((CIVAR .or. UTVAR) .and. J.gt.1)go to 16

          XRHGRO = XRHMLT(ISPC)
          XRDGRO = XRDMLT(ISPC)
          if (CIVAR) then
             FNT = FINT
             if (LESTB) then
                if (FINT.le.5.0) then
                   LSKIPH = .true.
                else
                   FNT = FNT-5.0
                end if
             end if
             CON = RHCON(ISPC) * EXP(HCOR(ISPC))
          else
             CON = RHCON(ISPC)*EXP(HCOR(ISPC))
          end if
          XMX = XMAX(ISPC)
          XMN = XMIN(ISPC)
          if (DEBUG)write(16,*)'ISPC,RHCON,HCOR,CON,XMX,XMN= ',&
               &ISPC,RHCON(ISPC),HCOR(ISPC),CON,XMX,XMN

          !  COMPUTE POTENTIAL GROWTH FROM SITE INDEX AND SPECIES.

          SI = SITEAR(ISPC)
          if (SI .gt. SHI) SI = SHI
          if (SI .le. SLO) SI = SLO + 0.5
          RELSI = (SI-SLO)/(SHI-SLO)
          RSIMOD = 0.5 * (1.0 + RELSI)
          X = RELSI*100.0
          SJ = SITEAR(ISPC)
          if (DEBUG) write(JOSTND,9981) ISPC,SI,RELSI,X
          9981 format('IN REGENT ISPC,SI,RELSI,X = ',&
               &I6,F7.2,F10.3,F10.4)

          SPTBA = PTBA(ISPC)
          SHDM1 = HDM1(ISPC)
          SHDM2 = HDM2(ISPC)
          SBAL = BAL(ISPC)
          SCR = CR(ISPC)
          SCRSQ = CRSQ(ISPC)
          SRLHT = RLHT(ISPC)
          SBA = STBA(ISPC)
          SPBAL = PBAL(ISPC)
          SHTBA = HTBA(ISPC)
          SCNST = CNST(ISPC)

          !  PROCESS NEXT TREE RECORD.  STORE INTERMEDIATE HEIGHTS IN WK3.

          do I3 = I1,I2

             !  BYPASS INCREMENT CALCULATION IF DBH IS GREATER THAN XMX OR IF
             !  CALL IS FROM **ESTAB** AND TREE SUBSCRIPT IS LESS THAN ITRNIN.

             I = IND1(I3)
             D = DBH(I)
             H = HT(I)
             IP = ITRE(I)
             if (DEBUG)write(JOSTND,*)'ISPC,I,D,XMX,LESTB,ITRNIN= ',&
                  &ISPC,I,D,XMX,LESTB,ITRNIN
             if (D.ge.XMX) go to 15

             if (LESTB .and. I.lt.ITRNIN) go to 15
             P = PROB(I)
             H1 = WK3(I)
             D1 = WK5(I)
             ZBAL = BAJ*(100.0-PCT(I))*0.0001
             call CCFCAL(ISPC,D,H1,ICR(I),P,.false.,C1,CW,1)
             B1 = BACON*D*D
             BARK = BRATIO(ISPC,D,H)
             if (DEBUG)write(16,*)'I,D1,H1= ',I,D1,H1

             if (UTVAR) then
                POTHTG = ((SJ/5.0)*(SJ*1.5-H)/(SJ*1.5))* 0.83
                if (DEBUG)write(JOSTND,*)'I,SJ,H,POTHTG= ',&
                     &I,SJ,H,POTHTG
             end if

             !  DETERMINE POINT CCF FOR THIS SUBCYCLE (BOUND BETWEEN 25 AND 300).

             IPCCF = ITRE(I)
             TPCCF = PCCF(IPCCF)*PPCCF
             if (TPCCF.gt.300.0) TPCCF = 300.0
             if (TPCCF.lt.25.0) TPCCF = 25.0
             CRI = FLOAT(ICR(I))

             !  CI VARIANT CODE: SPECIES 1-10 AND 18
             !  COMPUTE ADJUSTMENT FOR BIAS, DENSITY, AND RELATIVE SIZE.

             if (CIVAR) then
                if (LESTB) then
                   PTBALI = BA       *(1.0-(PCT(I)/100.))
                else
                   PTBALI = PTBAA(IP)*(1.0-(PCT(I)/100.))
                end if
                if (ATAVH .gt.0.0) then
                   RELHT = HT(I)/ATAVH
                else if (AVH .gt.0.0) then
                   RELHT = HT(I)/AVH
                else
                   RELHT = 1.5
                end if
                if (RELHT.gt.1.5) RELHT = 1.5
                TBAL = (1.0 - (PCT(I)/100.)) * BA
                IICR = ((ICR(I)-1)/10)+1
                if (IICR.gt.9) IICR = 9
                RCR = FLOAT(IICR)

                !  COMPUTE HEIGHT INCREMENT.  STORE UPDATED HEIGHT IN WK3.

                HTGRL = SCNST + SHTBA*RELHT*PTBAA(IP) + SPBAL*PTBALI&
                     &+ SBA*BA + SRLHT*RELHT + SCRSQ*RCR*RCR + SCR*RCR&
                     &+ SBAL*TBAL + SHDM1*RHDM1 + SHDM2*RHDM2 + SPTBA*PTBAA(IP)

                if (DEBUG)write(JOSTND,*)'CI SECTION, I,HTGRL= ',I,HTGRL

                !  TT VARIANT CODE: 11=WB, 12=PY, 16=LM

             else if (TTVAR) then
                call SMHTGF(ISPC,I,HTGRL,CRI,TPCCF,LESTB,DEBUG)
                if (DEBUG)write(16,*)'AFTER SMHTGF,I,HTGRL,CRI,TPCCF= ',&
                     &I,HTGRL,CRI,TPCCF

                !  UT VARIANT CODE: 13=AS, 14=WJ, 15=MC, 17=CW, 19=OH

             else if (UTVAR) then

                ! COMPUTE HT GROWTH AND AGE FOR ASPEN. EQN FROM WAYNE SHEPPARD RMRS.

                if (ISPC .eq.13) then
                   SITAGE = 0.0
                   SITHT = 0.0
                   AGMAX = 0.0
                   HTMAX = 0.0
                   HTMAX2 = 0.0
                   D1F = DBH(I)
                   D2F = 0.0
                   if (DEBUG)write(JOSTND,*)'IN HTGF, CALLING FINDAG I= ',I
                   call FINDAG(I,ISPC,D1F,D2F,H,SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,&
                        &DEBUG)

                   if (LESTB) then
                      SITAGE = ABIRTH(I)
                   end if
                   HITE1 = 26.9825 * SITAGE**1.1752
                   AG2 = SITAGE+10.0
                   HITE2 = 26.9825 * AG2**1.1752
                   HTGR = (HITE2-HITE1)/(2.54*12.0) * RSIMOD * CON

                   ! GROWTH RATES APPEAR HIGH, REDUCE BY 25 PERCENT. DIXON 8-27-92

                   HTGRL = HTGR * .75
                   if (DEBUG)write(16,*)'I,HITE1,HITE2,RSIMOD,CON,HTGR,HTGRL= ',&
                        &I,HITE1,HITE2,RSIMOD,CON,HTGR,HTGRL
                   go to 33
                end if

                X = CRI/100.
                VIGOR = (150.0 * (X**3.0)*EXP(-6.0*X))+0.3
                if (VIGOR .gt.1.0)VIGOR = 1.0

                !  VIGOR ADJUSTMENT TO DRASTIC FOR PINYON, JUNIPER, CUT IT BY TWO-THIRDS

                select case (ISPC)
                case(14)
                   VIGOR = 1.-((1.-VIGOR)/3.)
                case default
                end select

                HTGRL = POTHTG*PCTRED*VIGOR*CON
                if (DEBUG) write(JOSTND,*)'UT SECTION, I,POTHTG,PCTRED,',&
                     &'VIGOR,CON,X,HTGRL= ',I,POTHTG,PCTRED,VIGOR,CON,X,HTGRL
             end if
             33 continue

             ! H2 IS THE END OF CYCLE HEIGHT ON A FINT-YEAR BASIS

             if (CIVAR) then
                H2 = H1 + HTGRL*SCALE*XRHGRO*CON
             else if (TTVAR) then
                H2 = H1 + HTGRL*SCALE*XRHGRO*CON*WK4(I)
             else if (UTVAR) then
                H2 = H1 + HTGRL*SCALE*XRHGRO
                if (DEBUG)write(16,*)'H2,H1,HTGRL,SCALE,XRHGRO= ',&
                     &H2,H1,HTGRL,SCALE,XRHGRO
             else
                H2 = H1 + HTGRL*SCALE
             end if
             WK3(I) = H2
             if (DEBUG)write(JOSTND,*)'I,H1,HTGRL,H2,KPER,REGYR,',&
                  &'XRHGRO,WK4,CON= ',&
                  &I,H1,HTGRL,H2,KPER(J),REGYR,XRHGRO,CON,WK4(I)

             !  UPDATE DENSITY FOR NEXT SUBCYCLE.  IF ONLY ONE SUBCYCLE OR
             !  IF INITIAL DBH IS GREATER THAN 3 INCHES, BYPASS DENSITY UPDATE.
             !  DIAMETER IS ADJUSTED FOR DENSITY AND USER-SUPPLIED MULTIPLIER.
             !  INCREMENT IS DETERMINED FROM THE DIFFERENCE BETWEEN COMPUTED
             !  DIAMETERS AND IS BOUNDED TO BE GREATER THAN OR EQUAL TO ZERO.

             if (CIVAR) then
                WK3(I) = H2
                D2 = D
                WK5(I) = D2
             else if (TTVAR) then
                RD = PCCF(ITRE(I))
                HLESS4 = H2 - 4.5
                D2 = 0.000231*HLESS4*CRI - 0.00005*HLESS4*RD&
                     &+ 0.001711*CRI + 0.17023*HLESS4 + 0.3
                D2 = D2*XRDGRO
                if (D2.lt.DIAM(ISPC)) D2 = DIAM(ISPC)
                if (DEBUG)write(JOSTND,*)'TT SECTION, I,ISPC,CRI,PCCF,H2,D2,',&
                     &'XRDGRO= ',I,ISPC,CRI,PCCF(ITRE(I)),H2,D2,XRDGRO
                WK3(I) = H2
                WK5(I) = D2
                if (J.ge.NPER .or. D.ge.3.0.or. H2.le.4.5)go to 14

                !  UTAH VARIANT SPECIES

             else
                select case (ISPC)
                case(13)
                   RD = PCCF(ITRE(I))
                   D2 = -0.41227 + 0.16944*H2 + 0.003191*CRI - 0.00220*RD
                   if (DEBUG)write(16,*)'I,H2,CRI,RD,D2= ',I,H2,CRI,RD,D2

                case default
                   WK3(I) = H2
                   D2 = D
                   WK5(I) = D2
                   if ((ISPC.eq.17.or. ISPC.eq.19) .and. D.ge.BKPT)&
                        &go to 14
                   if (H2.le.4.5) then
                      D2 = D+0.001*H2
                      WK5(I) = D2
                   end if
                   if (DEBUG)write(JOSTND,*)'I,D,D2,H2,SITEAR= ',&
                        &I,D,D2,H2,SITEAR(ISPC)
                end select
             end if

             !  COMPUTE ADJUSTMENT TO STAND BASAL AREA AND CCF FOR NEXT SUBCYCLE.
             !  ASSUME A 1.5 PERCENT ANNUAL MORTALITY RATE.

             call CCFCAL(ISPC,D1,H1,ICR(I),P,.false.,C1,CW,1)
             call CCFCAL(ISPC,D2,H2,ICR(I),P,.false.,C2,CW,1)
             RDNEXT(J+1) = RDNEXT(J+1)+KY*(C2-C1)/10.*0.985**KY
             BANEXT(J+1) = BANEXT(J+1)+(BACON*(D2*D2-D1*D1))*P*0.985**KY

             if (DEBUG) write(JOSTND,8997) I,D2,BANEXT(J+1),RDNEXT(J+1)
             8997 format('UPDATE DENSITY: TREE = ',I4,', D2 = ',F8.4,&
                  &', BANEXT(J+1) = ',F8.2,', RDNEXT(J+1) = ',F8.2)

             !  END OF TREE LOOP WITHIN SPECIES. PRINT DEBUG IF REQUESTED.

             14 continue
             if (.not.DEBUG) go to 15
             write(JOSTND,8999) J,KPER(J),ISPC,I,H1,H2,RDJ,ZBAL,&
                  &BAJ,PCT(I)
             8999 format('IN REGENT L1: SUBC=',I2,', KPER=',I2,', SP=',I2,&
                  &', I=',I4,', H1=',F8.4,', H2=',F8.4,/T4,'RDJ=',&
                  &F8.4,', ZBAL=',F8.4,', BAJ=',F8.4,', PCT=',F8.4)
             15 continue
          end do

          !  END OF SPECIES LOOP WITHIN SUBCYCLE

          16 continue
       end do
       if (DEBUG)write(16,*)'AFTER 16 CIVAR,TTVAR,UTVAR= ',&
            &CIVAR,TTVAR,UTVAR

       !  END OF SUBCYCLE LOOP.  END OF PERIOD HEIGHT IS STORED IN WK3.

       17 continue
    end do

    !  AFTER LAST SUBCYCLE, COMPUTE ACTUAL HEIGHT INCREMENT FOR FULL CYCLE BY
    !  SUBTRACTION; CONVERT BACK TO LOG SCALE TO ADD IN RANDOM ERROR.

    if (DEBUG)write(16,*)'ENTERING DO 30 LOOP'

    do ISPC = 1,MAXSP
       I1 = ISCT(ISPC,1)
       if (I1.eq.0) go to 30
       I2 = ISCT(ISPC,2)

       select case (ISPC)
       case(17,19)
          BKPT = 1.0
       case(14,15)
          BKPT = 99.0
       case default
          BKPT = 3.0
       end select

       !  ASSIGN VARIANT ACCORDING TO SPECIES

       CIVAR = .false.
       TTVAR = .false.
       UTVAR = .false.
       select case (ISPC)
       case(11,12,16)
          TTVAR = .true.
          SCALE = YR/FINT
          if (.not.LSKIPH)SCALE2 = YR/FLOAT(NTYR)
       case(13,14,15,17,19)
          UTVAR = .true.
          SCALE = FLOAT(NTYR)/YR
          if (.not.LSKIPH)SCALE2 = YR/FLOAT(NTYR)
       case default
          CIVAR = .true.
          SCALE = FLOAT(NTYR)/REGYR
          if (.not.LSKIPH)SCALE2 = YR/FLOAT(NTYR)
       end select

       XRHGRO = XRHMLT(ISPC)
       XRDGRO = XRDMLT(ISPC)
       XMX = XMAX(ISPC)
       XMN = XMIN(ISPC)
       DGMX = DGMAX(ISPC) * SCALE
       if (TTVAR)DGMX = FINT*DGMAX(ISPC)
       if (DEBUG)write(16,*) 'ENTERING DO 25 LOOP- ',&
            &'ISPC,CIVAR,TTVAR,UTVAR,XRDGRO,XMX,XMN,DGMX=',&
            &ISPC,CIVAR,TTVAR,UTVAR,XRDGRO,XMX,XMN,DGMX
       if (DEBUG)write(16,*)'LSKIPH,YR,NTYR,REGYR,SCALE,SCALE2= ',&
            &LSKIPH,YR,NTYR,REGYR,SCALE,SCALE2

       do I3 = I1,I2
          I = IND1(I3)
          D = DBH(I)
          if (DEBUG)write(16,*)'I,D= ',I,D
          CRI = FLOAT(ICR(I))
          IICR = ((ICR(I)-1)/10)+1
          if (IICR.gt.9) IICR = 9
          RCR = FLOAT(IICR)
          if (D.ge.XMX) go to 25
          if (LESTB .and. I.lt.ITRNIN) go to 25
          K = I
          L = 0
          H = HT(I)
          HK = WK3(I)

          D1 = DIAM(ISPC)
          DK = WK5(I)
          HTGR1 = HK-H
          HTGR = HTGR1
          if (DEBUG)write(16,*)'I,H,HK,HTGR1,DK= ',I,H,HK,HTGR1,DK
          if (TTVAR .and. D.lt.D1)D = D1

          !  IF CALLED FROM **ESTAB** AND TOTAL CYCLE LENGTH IS LESS THAN OR EQUAL
          !  TO 5 YEARS, SKIP HEIGHT INCREMENT CALCULATION BUT ASSIGN CR AND DBH.

          if ((CIVAR .or. UTVAR) .and. LSKIPH) then
             HTG(K) = 0
             go to 20
          else if (TTVAR .and. LSKIPH) then
             IPCCF = ITRE(I)
             TPCCF = PCCF(IPCCF)
             if (H.ge.4.5) then

                select case (ISPC)
                case(11,12,16)
                   HLESS4 = H - 4.5
                   DK = 0.000231*HLESS4*CRI - 0.00005*HLESS4*TPCCF&
                        &+ 0.001711*CRI + 0.17023*HLESS4 + 0.3
                case(13)
                   DK = -0.41227 + 0.16944*H + 0.003191*CRI - 0.00220*TPCCF
                case default
                end select

                DK = DK*XRDGRO
                if (DK.lt.D) DK = D
                DBH(I) = DK
                DG(I) = DK
                if (DG(I).gt.DGMX) DG(I) = DGMX
             end if
             go to 25
          end if
          if (CIVAR) then
             HTGR1 = HK-H
             if (HTGR1.lt.0.0) HTGR1 = 0.0
          end if

          !  DRAW RANDOM INCREMENT GIVEN EXPECTED VALUE, HTGR, AND STD. DEV.,
          !  BOUND DRAW TO (-1.5*SD < ZZRAN < 1.0*SD); IF TRIPLING,
          !  EACH TRIPLE GETS A NEW RANDOM NUMBER.  DO NOT TRIPLE IF CALLING
          !  FROM **ESTAB**.  DO NOT APPLY HEIGHT GROWTH MULTIPLIER; MULTIPLIER
          !  WAS USED DURING SUBCYCLING.

          ICRK = ICR(I)
          918 continue
          if (.not.TTVAR) then
             if (CIVAR) then
                920 continue
                ZZRAN = 0.0
                if (DGSD.ge.1.0) ZZRAN = BACHLO(0.0,1.0,RANN)
                if ((ZZRAN .gt.0.5) .or. (ZZRAN .lt. -2.0)) go to 920
                HTGR = (HTGR1 +ZZRAN*0.1*SCALE)
                if (DEBUG)write(JOSTND,*)'CI SECTION K,ZZRAN,XRHGRO,SCALE,',&
                     &'CON,HTGR,HTGR1= ',K,ZZRAN,XRHGRO,SCALE,CON,HTGR,HTGR1

                ! UTAH VARIANT LOGIC: 13=AS, 14=WJ, 15=MC, 17=CW, 19=OH

             else
                919 continue
                ZZRAN = 0.0
                if (DGSD.ge.1.0) ZZRAN = BACHLO(0.0,1.0,RANN)
                if ((ZZRAN .gt.0.5) .or. (ZZRAN.lt.-2.0)) goto 919
                if (DEBUG)write(JOSTND,9984) HTGR1,ZZRAN,XRHGRO,WK4(I)
                9984 format('IN REGENT 9984 FORMAT',4(F10.4,2X))

                select case (ISPC)
                case(13)
                   HTGR = (HTGR1 +ZZRAN*0.1*(FLOAT(NTYR)/10.))
                   if (DEBUG)write(16,*)'I,HTGR,ZZRAN,NTYR,XRHGRO= ',&
                        &I,HTGR,ZZRAN,NTYR,XRHGRO
                case(14)
                   HTGR = (HTGR1 +ZZRAN*0.1*(FLOAT(NTYR)/10.))
                case(17,19)
                   HTGR = (HTGR1 +ZZRAN*0.2*(FLOAT(NTYR)/10.))*WK4(I)
                case(15)
                   HTGR = (HTGR1 +ZZRAN*0.1*(FLOAT(NTYR)/10.))
                case default
                end select
                if (DEBUG)write(JOSTND,*)'UT SECTION K,ZZRAN,XRHGRO,',&
                     &'HTGR= ',K,ZZRAN,XRHGRO,HTGR
             end if
          end if

          ! THIS CHECK ADDED BY DIXON 3/4/09 TO PREVENT NEGATIVE HEIGHT GROWTH
          ! MAY NEED A MORE ELEGANT FIX AT SOME POINT; BUT IT WORKS.

          if (HTGR .lt.0.1) then
             HTGR = 0.1
             if (DEBUG)write(JOSTND,*)'CI/CR/UT RESETTING HTGR ',&
                  &'TO 0.1 FOOT'
          end if

          !  COMPUTE WEIGHTS FOR THE LARGE AND SMALL TREE HEIGHT INCREMENT
          !  ESTIMATES.  IF DBH IS LESS THAN OR EQUAL TO XMN OR IF CALLED
          !  FROM **ESTAB**, THE LARGE TREE PREDICTION IS IGNORED (XWT=0.0).

          XWT = 0.0
          if (D.le.XMN.or.LESTB) go to 19
          XWT = (D-XMN)/(XMX-XMN)
          19 continue
          if (DEBUG)write(JOSTND,*)'WEIGHTING K,HTGR,HTG,XWT= ',&
               &K,HTGR,HTG(K),XWT
          HTG(K) = HTGR*(1.0-XWT) + XWT*HTG(K)

          ! CHECK FOR SIZE CAP COMPLIANCE.

          if ((H+HTG(K)).gt.SIZCAP(ISPC,4)) then
             HTG(K) = SIZCAP(ISPC,4)-H
             if (HTG(K) .lt.0.1) HTG(K) = 0.1
          end if

          20 continue

          !  ASSIGN DBH AND COMPUTE DBH INCREMENT FOR TREES WITH DBH LESS
          !  THAN BREAKPOINT.  COMPUTE 10-YEAR DBH INCREMENT REGARDLESS OF
          !  PROJECTION PERIOD LENGTH.  BYPASS INCREMENT CALCULATION IF
          !  D GE BREAKPOINT.

          if (D .ge. BKPT) go to 23
          HK = H+HTG(K)
          if (.not.TTVAR .and. HK.lt.4.5) then
             DG(K) = 0.0
             DBH(K) = D+0.001*HK
          else
             if (CIVAR) then
                SDHCN = DHCN(ISPC)
                SDHHT = DHHT(ISPC)
                SDHCR = DHCR(ISPC)
                DK = EXP(  SDHCN + SDHHT*ALOG(HK) + SDHCR*ALOG(RCR) )
                if (H .lt.4.5) then
                   DKK = D
                else
                   DKK = EXP( SDHCN + SDHHT*ALOG(H) + SDHCR*ALOG(RCR) )
                end if
                if (DEBUG)write(JOSTND,*)'I,ISPC,H,HK,DK,DKK,XRDGRO= ',&
                     &I,ISPC,H,HK,DK,DKK,XRDGRO
             else if (UTVAR) then

                select case(ISPC)
                case(14)
                   DK = (HK-4.5)*10./(SITEAR(ISPC)-4.5)
                   if (DK .lt.0.1) DK = 0.1
                   DKK = (H-4.5)*10./(SITEAR(ISPC)-4.5)
                   if (DKK .lt.0.1) DKK = 0.1
                   if (H .lt.4.5) DKK = D
                case(13,15,17,19)
                   BX = HT2(ISPC)
                   if (IABFLG(ISPC).eq.1) then
                      AX = HT1(ISPC)
                   else
                      AX = AA(ISPC)
                   end if

                   !  SPECIES FROM SO (ORIGINALY FROM WC) VARIANT

                   if (ISPC.eq.15) then

                      !   BEGIN THE DIAMETER LOOKUP SECTION
                      !      DK = DIAMETER WITH HTG ADDED TO THE STARTING HEIGHT
                      !      DKK  = DIAMETER AT THE START OF THE PROJECTION
                      !   DAT45  = DIAMETER AT 4.5 FEET PREDICTED FROM EQUATION.

                      CRCODE = FLOAT(ICR(K))/10.0
                      DAT45 = 3.1020 + 0.0210*4.5

                      DKK = 3.1020 + 0.0210*H
                      if (DKK .lt.0.0) DKK = D

                      DK = 3.1020 + 0.0210*HK
                      if (DK .lt. DKK) DK = DKK+.01
                      if (DEBUG)write(JOSTND,*)'I,ISPC,DBH,H,HK,DK,DKK,CRCODE,',&
                           &'= ',I,ISPC,DBH(I),H,HK,DK,DKK,CRCODE

                      !  USE INVENTORY EQUATIONS IF CALIBRATION OF THE HT-DBH FUNCTION IS TURNED
                      !  OFF, OR IF WYKOFF CALIBRATION DID NOT OCCUR.
                      !  NOTE: THIS SIMPLIFIES TO IF(IABFLB(ISPC).EQ.1) BUT IS SHOWN IN IT'S
                      !        ENTIRITY FOR CLARITY.

                      if (.not.LHTDRG(ISPC) .or.&
                           &(LHTDRG(ISPC) .and. IABFLG(ISPC).eq.1)) then
                         P2 = 1709.7229
                         P3 = 5.8887
                         P4 = -0.2286
                         HAT3 = 4.5 + P2 * EXP(-1.*P3*3.0**P4)
                         if (HK .ge. HAT3) then
                            DK = EXP( ALOG((ALOG(HK-4.5)-ALOG(P2))/(-1.*P3)) *&
                                 &1./P4)
                         else
                            DK = (((HK-4.51)*2.7)/(4.5+P2*EXP(-1.*P3*(3.**P4))&
                                 &-4.51))+0.3
                         end if
                         if (H .le.4.5) then
                            DKK = D
                         else
                            if (H .ge. HAT3) then
                               DKK = EXP( ALOG((ALOG(H-4.5)-ALOG(P2))/(-1.*P3)) *&
                                    &1./P4)
                            else
                               DKK = (((H-4.51)*2.7)/(4.5+P2*EXP(-1.*P3*(3.**P4))&
                                    &-4.51))+0.3
                            end if
                         end if
                         if (DEBUG)write(JOSTND,*)'INV EQN DUBBING ISPC,H,HK,DK,'&
                              &,'DKK= ',ISPC,H,HK,DK,DKK
                         if (DEBUG)write(JOSTND,*)'ISPC,LHTDRG,IABFLG= ',&
                              &ISPC,LHTDRG(ISPC),IABFLG(ISPC)
                      end if
                   else
                      DK = (BX/(ALOG(HK-4.5)-AX))-1.0
                      if (DK .lt.0.1) DK = 0.1
                      if (H .le.4.5) then
                         DKK = D
                      else
                         DKK = (BX/(ALOG(H-4.5)-AX))-1.0
                      end if
                      if (DEBUG)write(JOSTND,9986) AX,BX,ISPC,HK,BARK,&
                           &XRDGRO,DK,DKK
                      9986 format('IN REGENT 9986 FORMAT AX,BX,ISPC,HK',&
                           &' BARK,XRDGRO,DK,DKK= '/T12, F10.3,2X,F10.3,2X,I5,2X,&
                           &5F10.3)
                   end if
                case default
                end select

             else if (TTVAR) then

                !  ASSIGN DIAMETER INCREMENT AND SCALE FOR BARK THICKNESS AND
                !  PERIOD LENGTH.  SCALE ADJUSTMENT IS ON GROWTH IN DDS RATHER
                !  THAN INCHES OF DG TO MAINTAIN CONSISTENCY WITH GRADD.

                if (HK .ge.4.5) then
                   RD = PCCF(ITRE(I))

                   select case (ISPC)
                   case(11,12,16)
                      HLESS4 = HT(I) - 4.5
                      DKK = 0.000231*HLESS4*CRI - 0.00005*HLESS4*RD&
                           &+ 0.001711*CRI + 0.17023*HLESS4 + 0.3
                   case(13)
                      !             DKK = -0.41227 + 0.16944*HT(I) + 0.003191*CRI - 0.00220*RD
                      if (DEBUG)write(16,*)'I,H,D= ',I,H,D
                      BX = HT2(ISPC)
                      if (IABFLG(ISPC).eq.1) then
                         AX = HT1(ISPC)
                      else
                         AX = AA(ISPC)
                      end if
                      DK = (BX/(ALOG(HK-4.5)-AX))-1.0
                      if (DK .lt.0.1) DK = 0.1
                      if (H .le.4.5) then
                         DKK = D
                      else
                         DKK = (BX/(ALOG(H-4.5)-AX))-1.0
                      end if
                      if (DEBUG)write(JOSTND,9786) AX,BX,ISPC,HK,BARK,&
                           &XRDGRO,DK,DKK
                      9786 format('IN REGENT 9986 FORMAT AX,BX,ISPC,HK',&
                           &'BARK,XRDGRO,DK,DKK= '/T12, F10.3,2X,F10.3,2X,I5,2X,5F10.3)
                   case default
                   end select

                   if (DEBUG)write(JOSTND,*)'I,ISPC,HT,CRI,RD,DKK = ',&
                        &I,ISPC,HT(I),CRI,RD,DKK
                   !            IF(DKK.LT.DIAM(ISPC))DKK=DIAM(ISPC)
                   BARK = BRATIO(ISPC,D,H)
                   DG(K) = (DK-DKK)*BARK
                   DDS = DG(K)*(2.0*BARK*D+DG(K))*SCALE2
                   if (DEBUG)write(JOSTND,*)'K,D,DKK,DK,BARK,DDS,DG(K)= ',&
                        &K,D,DKK,DK,BARK,DDS,DG(K)
                   DG(K) = SQRT((D*BARK)**2+DDS)-BARK*D
                else
                   DG(K) = 0.0
                end if
             end if

             !       IF CALLING FROM **ESTAB** ASSIGN DIAMETER AND DIAMETER
             !       INCREMENT.

             if (LESTB) then

                select case (ISPC)
                case(15)
                   if (DAT45.gt.0.0.and. HK.ge.4.5.and. LHTDRG(ISPC) .and.&
                        &IABFLG(ISPC).eq.0) then
                      DBH(K) = DK - DAT45 + DIAM(ISPC)
                   else
                      DBH(K) = DK
                   end if
                case default
                   DBH(K) = DK
                end select

                if (TTVAR.and.DG(K).gt.DGMX)DG(K) = DGMX
                if (UTVAR .or. CIVAR) then
                   if (DBH(K).lt.DIAM(ISPC)) DBH(K) = DIAM(ISPC)
                   DBH(K) = DBH(K)+0.001*HK
                   DG(K) = DBH(K)
                end if
             else

                !         DIAMETER INCREMENT IS THE DIFFERENCE BETWEEN COMPUTED
                !         DIAMETERS.  MULTIPLIER IS APPLIED TO DIAMETER INCREMENT
                !         AT THIS POINT.

                if (.not.TTVAR) then
                   if (CIVAR) then
                      if (H .lt.4.5 )DKK = D
                      if (DK.lt.0.0.or. DKK.lt.0.0) then
                         DG(K) = HTG(K)*0.2*BARK*XRDGRO
                         DK = D+DG(K)
                      else
                         DG(K) = (DK-DKK)*BARK*XRDGRO
                      end if
                      if (DG(K) .lt.0.0) DG(K) = 0.0
                   else if (UTVAR) then

                      select case (ISPC)

                         ! FROM SO (ORIGINALLY WC)
                         ! PROBLEM WITH HARDWOOD EQN, REDUCES TO .021*HG. SET TO RULE
                         ! OF THUMB VALUE .1*HG FOR NOW. DIXON 11-04-92
                         ! DON'T USE R.O.T. IF USING INVENTORY EQNS.  DIXON 03-31-98

                         ! IF THE TREE JUST REACHED 4.5 FEET, SET DKK TO PRESENT DBH.
                         ! RJ 12/6/91

                      case(15)
                         if (H .lt.4.5 )DKK = D
                         BARK = BRATIO(ISPC,D,H)
                         if (DEBUG)write(JOSTND,*)'BARK,XRDGRO= ',BARK,XRDGRO
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
                         if (DEBUG)write(JOSTND,*)'HARDWOOD EQU DG(K),DGMX,',&
                              &'LHTDRG= '
                         if (DEBUG)write(JOSTND,*)DG(K),DGMX,LHTDRG(ISPC)

                      case(13,14,17,19)
                         BARK = BRATIO(ISPC,D,H)
                         if (DK.lt.0.0.or. DKK.lt.0.0) then
                            DG(K) = HTG(K)*0.2*BARK*XRDGRO
                            DK = D+DG(K)
                         else
                            DG(K) = (DK-DKK)*BARK*XRDGRO
                         end if
                         if (DG(K).gt.DGMX)DG(K) = DGMX
                         if (DEBUG)write(JOSTND,*)'K,DK,DKK,DG= ',K,DK,DKK,DG(K)
                      case default
                      end select

                   end if
                end if
                if (DG(K).lt.0.0) DG(K) = 0.0

                !         SCALE DIAMETER INCREMENT FOR BARK AND PERIOD LENGTH.
                !         IN ORDER TO MAINTAIN CONSISTENCY WITH **GRADD**,
                !         ADJUSTMENTS ARE MADE ON THE DDS SCALE.

                BARK = BRATIO(ISPC,DBH(K),HT(K))
                if (CIVAR) then
                   DG(K) = (DK-DKK)*BARK*XRDGRO
                   if (DG(K).lt.0.)DG(K) = 0.
                else if (UTVAR) then
                   if (DG(K) .gt. DGMX) DG(K) = DGMX
                end if
                if (CIVAR .or. UTVAR) then
                   DDS = (DG(K)*(2.0*BARK*D+DG(K))) * SCALE2
                else
                   DDS = DG(K)*(2.0*BARK*D+DG(K))*SCALE2
                end if
                DG(K) = SQRT((D*BARK)**2+DDS)-BARK*D
             end if
             if ((DBH(K)+DG(K)).lt.DIAM(ISPC)) then
                DG(K) = DIAM(ISPC)-DBH(K)
             end if
          end if

          !  CHECK FOR TREE SIZE CAP COMPLIANCE

          call DGBND(ISPC,DBH(K),DG(K))

          23 continue

          !  PRINT DEBUG AND RETURN TO PROCESS NEXT TRIPLE OR NEXT TREE.

          if (.not.DEBUG) go to 24
          HK = H+HTG(K)
          write(JOSTND,9000) I,K,ISPC,H,HTG(K),HK,DBH(K),DG(K)
          9000 format('IN REGENT LOOP 2, I=',I4,', K=',I4,', ISPC=',I3,&
               &', CUR HT=',F7.2,', HT INC=',F7.4/T19,'NEW HT=',F7.2,&
               &', CUR DBH=',F7.2,', 10-YR DBH INC=',F7.4)
          24 continue
          if (LESTB .or..not.LTRIP .or. L.ge.2) go to 25
          if (TTVAR) then
             K = ITRN+2*I-1
             DBH(K) = DBH(I)
             DG(K) = DG(I)
             HT(K) = HT(I)
             HTG(K) = HTG(I)
             ICR(K) = ICR(I)
             K = K+1
             DBH(K) = DBH(I)
             DG(K) = DG(I)
             HT(K) = HT(I)
             HTG(K) = HTG(I)
             ICR(K) = ICR(I)
             go to 25
          else
             L = L+1
             K = ITRN+2*I-2+L
             go to 918
          end if

          !  END OF GROWTH PREDICTION LOOP.  PRINT DEBUG INFO IF DESIRED.

          25 continue
       end do
       30 continue
    end do
    return

    !  SMALL TREE HEIGHT CALIBRATION SECTION.

    40 continue
    do ISPC = 1,MAXSP
       HCOR(ISPC) = 0.0
       CORTEM(ISPC) = 1.0
       NUMCAL(ISPC) = 0
       45 continue
    end do
    if (ITRN.le.0) return
    if (IFINTH .eq.0)  goto 100

    select case (ISPC)

       !  SPECIES WITH REGYR=10: SPECIES FROM UT VARIANT

    case(13,14,15,17,19)
       SCALE3 = 10./FINTH

       !  SPECIES WITH REGYR=5: ORIGINAL TT SPECIES, SPECIES FROM CI AND IE VARIANTS

    case default
       SCALE3 = REGYR/FINTH
    end select

    !  DETERMINE BASAL AREA IN LARGE TREES AT START OF EACH PREDICTION SUBCYCLE.

    do J = 1,NPER
       BANEXT(J) = TEMBA
       RDNEXT(J) = TEMCCF
       46 continue
    end do
    if (NPER.eq.1) go to 50

    !  IF THERE IS MORE THAN 1 SUBCYCLE FOR HEIGHT INCREMENT PREDICTION,
    !  (IFINTH > 5) ESTIMATE VALUES OF BA AND RELATIVE DENSITY (CCF) AT
    !  START OF EACH SUBCYCLE.  10-YEAR CHANGE IS COMPUTED FROM BACKDATED
    !  DIAMETER (WK3) AND 10-YEAR DIAMETER INCREMENT COMPUTED DURING CALIBRATION.
    !  LINEAR INTERPOLATION IS USED TO ASSIGN VALUES FOR SUBCYCLES.

    do I = 1,ITRN
       D1 = WK3(I)
       IS = ISP(I)
       P = PROB(I)
       D2 = WK3(I)+DG(I)/BRATIO(IS,D1,HT(I))
       B1 = BACON*D1*D1
       B2 = BACON*D2*D2
       call CCFCAL(IS,D1,HT(I),ICR(I),P,.false.,C1,CW,1)
       call CCFCAL(IS,D2,HT(I),ICR(I),P,.false.,C2,CW,1)
       BI = (B2-B1)/10.0
       CI = (C2-C1)/10.0
       K = 0
       do J = 2,NPER
          K = K+KPER(J-1)
          PN = P*0.985**K
          RDNEXT(J) = RDNEXT(J)+K*CI/P*PN
          BANEXT(J) = BANEXT(J)+K*BI*PN
          48 continue
       end do
       49 continue
    end do
    50 continue

    ! COMPUTE DENSITY MODIFIER FROM CCF AND TOP HEIGHT FOR UT VARIANT

    X = AVH*(RELDEN/100.0)
    if (X .gt.300.0) X = 300.0
    PCTRED = 1.11436 + X*(-.011493 + X*(.43012E-4 + X*(-.72221E-7&
         &+ X*(.5607E-10- X*0.1641E-13))))
    if (PCTRED .gt.1.0) PCTRED = 1.0
    if (PCTRED .lt.0.01) PCTRED = 0.01
    if (DEBUG) write(JOSTND,9989) AVH,RELDEN,X,PCTRED
    9989 format('IN REGENT AVH,RELDEN,X,PCTRED = ',4F10.4)

    !  BEGIN PROCESSING TREE LIST IN SPECIES ORDER.  DO NOT CALCULATE
    !  CORRECTION TERMS IF THERE ARE NO TREES FOR THIS SPECIES.

    do ISPC = 1,MAXSP

       !  ASSIGN VARIANT ACCORDING TO SPECIES

       CIVAR = .false.
       TTVAR = .false.
       UTVAR = .false.
       select case (ISPC)
       case(11,12,16)
          TTVAR = .true.
       case(13,14,15,17,19)
          UTVAR = .true.
       case default
          CIVAR = .true.
       end select

       !  GET THE APPROPRIATE SITE INDEX RANGE VALUES FOR THIS SPECIES.

       SLO = 0.
       SHI = 999.
       call SITERANGE (2,ISPC,SLO,SHI)

       CORNEW = 1.0
       I1 = ISCT(ISPC,1)
       if (I1.eq.0.or..not. LHTCAL(ISPC)) go to 90
       I2 = ISCT(ISPC,2)
       IREFI = IREF(ISPC)
       N = 0
       SNP = 0.0
       SNX = 0.0
       SNY = 0.0

       !  BEGIN TREE LOOP WITHIN SPECIES.  IF MEASURED HEIGHT INCREMENT IS
       !  LESS THAN OR EQUAL TO ZERO, OR DBH IS GREATER THAN 5.0, THE RECORD
       !  WILL BE EXCLUDED FROM THE CALIBRATION.

       do I3 = I1,I2
          I = IND1(I3)
          H = HT(I)
          if (IHTG.lt.2) H = H-HTG(I)
          if (DBH(I).ge.5.0.or.H.lt.0.01) go to 60
          if (HTG(I).lt.0.001) go to 60
          HK = H
          D = DBH(I)
          CRI = FLOAT(ICR(I))
          do J = 1,NPER
             BAJ = BANEXT(J)
             RDJ = RDNEXT(J)
             ZBAL = BAJ*(100.0-PCT(I))*0.0001
             if (TEMCCF .le.0.) then
                PPCCF = 0.
             else
                PPCCF = (RDJ-TEMCCF)/TEMCCF
             end if
             TPCCF = PCCF(ITRE(I))*PPCCF
             if (TPCCF.gt.300.0) TPCCF = 300.0
             if (TPCCF.lt.25.0) TPCCF = 25.0
             if (CIVAR) then
                IICR = ((ICR(I)-1)/10)+1
                if (IICR.gt.9) IICR = 9
                RCR = FLOAT(IICR)

                !  DIA GT 3 INCHES INCLUDED IN OVERALL MEAN

                if (IHTG.lt.2) H = H-HTG(I)
                if (DBH(I).ge.5.0.or.H.lt.0.01) go to 60
                EDH = 2.764559 - 0.009643*BA + 0.025303*RCR*RCR

             else if (TTVAR) then
                call SMHTGF(ISPC,I,EDH,CRI,TPCCF,LESTB,DEBUG)
                HK = HK+EDH
                RD = PCCF(ITRE(I))

                select case (ISPC)
                case(11,12,16)
                   HLESS4 = HK - 4.5
                   D2 = 0.000231*HLESS4*CRI - 0.00005*HLESS4*RD&
                        &+ 0.001711*CRI + 0.17023*HLESS4 + 0.3
                case default
                end select

                if (D2.lt.DIAM(ISPC)) D2 = DIAM(ISPC)

             else if (UTVAR) then

                !  FOR UT VARIANT SPECIES
                !  COMPUTE POTENTIAL GROWTH FROM SITE INDEX AND SPECIES.

                SI = SITEAR(ISPC)
                if (SI .gt. SHI) SI = SHI
                if (SI .le. SLO) SI = SLO + 0.5
                RELSI = (SI-SLO)/(SHI-SLO)
                RSIMOD = 0.5 * (1.0 + RELSI)
                X = RELSI*100.0
                SJ = SITEAR(ISPC)
                POTHTG = ((SJ/5.0)*(SJ*1.5-H)/(SJ*1.5))* 0.83
                if (DEBUG)write(JOSTND,9988)SI,RELSI,X,ISPC,J,POTHTG
                9988 format('IN REGENT SI,RELSI,X,ISPC,J,POTHTG = ',&
                     &F7.2,F10.3,F10.4,2I6,F10.3)

                X = CRI/100.
                VIGOR = (150.0 * (X**3.0)*EXP(-6.0*X))+0.3
                if (VIGOR .gt.1.0)VIGOR = 1.0

                !  VIGOR ADJUSTMENT TO DRASTIC FOR PINYON, JUNIPER, AND OAK
                !  CUT IT BY TWO-THIRDS

                select case (ISPC)
                case(14)
                   VIGOR = 1.-((1.-VIGOR)/3.)
                case default
                end select

                if (ISPC .eq.13) then
                   AG1 = (H*12.0*2.54/26.9825)**0.8509
                   AG2 = AG1 + 10.0
                   H2 = (26.9825*AG2**1.1752)/(2.54*12.0)
                   EDH = (H2-H) * RSIMOD * RHCON(ISPC)

                   ! GROWTH RATES APPEAR HIGH, REDUCE BY 25 PERCENT. DIXON 8-27-92.

                   EDH = EDH*.75
                   if (EDH .lt.0.0) EDH = 0.0
                else
                   EDH = POTHTG*PCTRED*VIGOR*RHCON(ISPC)
                end if

                ! UT MODELS ARE 10-YEAR MODELS, ADJUST TO 5-YEAR BASIS

                EDH = EDH*0.5

                if (DEBUG)write(JOSTND,9990) X,VIGOR,EDH
                9990 format('IN REGENT X,VIGOR,EDH = ',3F10.4)
             end if
             55 continue
          end do
          if (CIVAR) then
             EDH = EDH * RHCON(ISPC)
          else if (TTVAR) then
             EDH = (HK-H)*RHCON(ISPC)
          end if
          P = PROB(I)
          if (HTG(I).lt.0.001) go to 60
          TERM = HTG(I)*SCALE3
          SNP = SNP+P
          SNX = SNX+EDH*P
          SNY = SNY+TERM*P
          N = N+1

          !  PRINT DEBUG INFO IF DESIRED.

          if (DEBUG) write(JOSTND,9001) NPLT,I,ISPC,H,DBH(I),ICR(I),&
               &PCT(I),TEMCCF,RHCON(ISPC),EDH,TERM
          9001 format('NPLT=',A26,',  I=',I5,',  ISPC=',I3,',  H=',F6.1,&
               &',  DBH=',F5.1,',  ICR',I5,',  PCT=',F6.1,',  RELDEN=',&
               &F6.1 / 12X,'RHCON=',F10.3,',  EDH=',F10.3,', TERM=',F10.3)

          !  END OF TREE LOOP WITHIN SPECIES.

          60 continue
       end do
       if (DEBUG) write(JOSTND,9010) ISPC,SNP,SNX,SNY
       9010 format(/'SUMS FOR SPECIES ',I2,':  SNP=',F10.2,&
            &';   SNX=',F10.2,';   SNY=',F10.2)

       !  COMPUTE CALIBRATION TERMS.  CALIBRATION TERMS ARE NOT COMPUTED
       !  IF THERE WERE FEWER THAN NCALHT (DEFAULT=5) HEIGHT INCREMENT
       !  OBSERVATIONS FOR A SPECIES.

       if (N.lt.NCALHT) go to 80

       !  CALCULATE MEANS FOR THE FOR THE GROWTH SAMPLE ON THE
       !  NATURAL SCALE.

       call REGHCAL(ISPC,SNP,SNX,SNY,CORNEW)
       80 continue
       CORTEM(IREFI) = CORNEW
       NUMCAL(IREFI) = N
       90 continue
    end do

    !  END OF CALIBRATION LOOP.  PRINT CALIBRATION STATISTICS AND RETURN

    write(JOSTND,9002) (NUMCAL(I),I = 1,NUMSP)
    9002 format(/'NUMBER OF RECORDS AVAILABLE FOR SCALING'/&
         &'THE SMALL TREE HEIGHT INCREMENT MODEL',&
         &((T48,11(I4,2X)/)))
    100 continue
    write(JOSTND,9003) (CORTEM(I),I = 1,NUMSP)
    9003 format(/'INITIAL SCALE FACTORS FOR THE SMALL TREE'/&
         &'HEIGHT INCREMENT MODEL',&
         &((T48,11(F5.2,1X)/)))

    ! OUTPUT CALIBRATION TO DATABASE.

    call REGCALOUT(CORTEM,NUMCAL,KOUT,K,KK,ISPEC,SPEC)
    91 if (DEBUG)write(JOSTND,9995)ICYC
    9995 format('LEAVING SUBROUTINE REGENT  CYCLE =',I5)
    return

    9998 continue

    !  ENTRY POINT FOR LOADING OF REGENERATION GROWTH MODEL
    !  CONSTANTS  THAT REQUIRE ONE-TIME RESOLUTION.

    do ISPC = 1,MAXSP
       RHCON(ISPC) = 1.0
       if (LRCOR2.and. RCOR2(ISPC).gt.0.0) RHCON(ISPC) = RCOR2(ISPC)
       110 continue
    end do

    do I = 1,MAXTRE
       ZRAND(I) = -999.0
    end do

    return
  end subroutine REGCI

  !===============================!
  !   Compute small tree growth   !
  !===============================!
  ! CR $Id$
  subroutine REGCR(LCON)
    implicit none

    logical :: LCON

    !  THIS SUBROUTINE COMPUTES HEIGHT AND DIAMETER INCREMENTS FOR
    !  SMALL TREES. HEIGHT INCREMENT IS A FUNCTION OF SITE INDEX,
    !  TOP HEIGHT, AND STAND CROWN COMPETITION FACTOR.  DIAMETER IS
    !  ASSIGNED FROM A HEIGHT-DIAMETER FUNCTION; INCREMENT IS COMPUTED
    !  BY SUBTRACTION.
    !  THIS ROUTINE IS CALLED FROM **CRATET** DURING CALIBRATION AND
    !  FROM **TREGRO** DURING CYCLING.  ENTRY **REGCON** IS CALLED FROM
    !  **RCON** TO LOAD MODEL PARAMETERS THAT NEED ONLY BE RESOLVED ONCE.

    ! Common blocks

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
    !    HTADJ -- SPECIES SPECIFIC ADJUSTMENT FACTOR TO POTENTIAL
    !             HEIGHT CALCULATON

    external RANN
    ! Variables
    logical :: DEBUG,LSKIPH
    real    :: CORTEM(38),XMAX(38),XMIN(38),AB(9),DGMAX(38)
    real    :: DIAM(38),HTADJ(38)
    integer :: NUMCAL(38),MPCC(38),IVFLAG

    real    :: D,H,BARK,RAN,HTGR,ZZRAN
    real    :: XWT,HK,REGYR,FNT,SCALE
    real    :: SCALE2,XRHGRO,XRDGRO,CON,XMX,XMN
    real    :: BRATIO,BACHLO,DK,DKK,DDS,HTNEW,SCALE3,CORNEW,SNP
    real    :: SNX,SNY,EDH,P,TERM,CCF,AVHT,X,PCTRED,BKPT,SI,RELSI,RSIMOD
    real    :: POTHTG,CR,AG1,HITE1,HITE2,AG2,VIGOR,BX,AX,H2,DGMX
    integer :: J,IPCCF
    integer :: ISPC,I1,I2,I3,I,K,L,N,IREFI,KOUT,ISPEC,KK
    character(len=2) :: SPEC

    !  SPECIES ORDER:
    !   1=AF,  2=CB,  3=DF,  4=GF,  5=WF,  6=MH,  7=RC,  8=WL,  9=BC, 10=LM,
    !  11=LP, 12=PI, 13=PP, 14=WB, 15=SW, 16=UJ, 17=BS, 18=ES, 19=WS, 20=AS,
    !  21=NC, 22=PW, 23=GO, 24=AW, 25=EM, 26=BK, 27=SO, 28=PB, 29=AJ, 30=RM,
    !  31=OJ, 32=ER, 33=PM, 34=PD, 35=AZ, 36=CI, 37=OS, 38=OH

    !  SPECIES EXPANSION:
    !  UJ,AJ,RM,OJ,ER USE CR JU
    !  NC,PW USE CR CO
    !  GO,AW,EM,BK,SO USE CR OA
    !  PB USES CR AS
    !  PM,PD,AZ USE CR PI
    !  CI USES CR PP

    ! Data statements
    data DGMAX/&
         &3.6, 3.6, 2.4, 3.6, 3.6, 3.0, 3.0, 3.0, 2.0, 2.8,&
         &3.5, 2.0, 2.8, 2.8, 3.5, 2.0, 3.6, 3.6, 3.6, 2.5,&
         &2.5, 2.5, 2.0, 2.0, 2.0, 2.0, 2.0, 2.5, 2.0, 2.0,&
         &2.0, 2.0, 2.0, 2.0, 2.0, 2.8, 2.8, 2.5/
    data XMAX /&
         &2.0, 2.0, 2.0, 5.0, 2.0, 4.0,  5.,  5.,199., 4.0,&
         &2.0,199., 2.0, 5.0, 2.0,199., 2.0, 2.0, 2.0, 2.0,&
         &2.0, 2.0,199.,199.,199.,199.,199., 2.0,199.,199.,&
         &199.,199.,199.,199.,199., 2.0, 2.0, 2.0/
    data XMIN/&
         &0.5, 0.5, 0.5, 2.0, 0.5, 2.0, 2.0, 2.0, 99., 0.5,&
         &0.5, 99., 0.5, 2.0, 0.5, 99., 0.5, 0.5, 0.5, 0.5,&
         &0.5, 0.5, 99., 99., 99., 99., 99., 0.5, 99., 99.,&
         &99., 99., 99., 99., 99., 0.5, 0.5, 0.5/
    data REGYR/10.0/
    data MPCC/ 2, 2, 1, 2, 2, 2, 1, 1, 4, 4,&
         &3, 4, 4, 4, 4, 4, 5, 5, 5, 4,&
         &4, 4, 4, 4, 4, 4, 4, 4, 4, 4,&
         &4, 4, 4, 4, 4, 4, 4, 4/
    data DIAM/&
         &0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.4, 0.4,&
         &0.4, 0.4, 0.5, 0.4, 0.4, 0.3, 0.3, 0.3, 0.3, 0.2,&
         &0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.2, 0.3, 0.3,&
         &0.3, 0.3, 0.4, 0.4, 0.4, 0.5, 0.2, 0.2/
    data AB/&
         &1.11436,-.011493,.43012E-4,-.72221E-7,.5607E-10,-.1641E-13,3*0./
    data HTADJ/&
         &1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.10, 1.10, 1.00, 1.00,&
         &1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00,&
         &1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00,&
         &1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00/

    if (LCON) go to 9998

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
    !  BASED ON A 10-YR GROWTH PERIOD. THE VARIABLE SCALE IS USED TO CONVERT
    !  HEIGHT INCREMENT PREDICTIONS TO A FINT-YEAR PERIOD.  DIAMETER
    !  INCREMENT IS PREDICTED FROM CHANGE IN HEIGHT, AND IS SCALED TO A
    !  10-YEAR PERIOD BY THE VARIABLE SCALE2.  DIAMETER INCREMENT
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
       BKPT = BREAK(ISPC)
       XRHGRO = XRHMLT(ISPC)
       XRDGRO = XRDMLT(ISPC)
       CON = RHCON(ISPC) * EXP(HCOR(ISPC))
       XMX = XMAX(ISPC)
       XMN = XMIN(ISPC)
       SI = SITEAR(ISPC)
       if (SI .gt. SITEHI(ISPC)) SI = SITEHI(ISPC)
       if (SI .le. SITELO(ISPC)) SI = SITELO(ISPC) + 0.5

       !  COMPUTE POTENTIAL GROWTH FROM SITE INDEX AND SPECIES.

       RELSI = (SI-SITELO(ISPC))/(SITEHI(ISPC)-SITELO(ISPC))
       RSIMOD = 0.5 * (1.0 + RELSI)
       X = RELSI*100.0
       J = MPCC(ISPC)
       POTHTG = SITEAR(ISPC)/(15.0 - 4.0*RELSI) * HTADJ(ISPC)

       if (DEBUG) write(JOSTND,9981) SI,RELSI,X,ISPC,J,POTHTG
       9981 format('IN REGENT SI,RELSI,X,ISPC,J,POTHTG = ',&
            &F7.2,F10.3,F10.4,2I6,F10.3)

       !     PUT A CEILING ON DIAMETER GROWTH BASED ON OLIVER AND COCHRAN EMPR.
       !     RJ 11/28/88

       DGMX = DGMAX(ISPC) * SCALE

       !  SET FLAG FOR VIGOR ADJUSTMENT

       select case(ISPC)
       case(9,12,16,23:27,29:35)
          IVFLAG = 1
       case default
          IVFLAG = 0
       end select

       !  PROCESS NEXT TREE RECORD.

       do I3 = I1,I2
          I = IND1(I3)
          D = DBH(I)
          H = HT(I)

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

          !     RETURN HERE TO PROCESS NEXT TRIPLE.

          2 continue

          ! COMPUTE VIGOR MODIFIER FROM CROWN RATIO.

          if (D.ge.XMX) go to 25
          H = HT(I)
          BARK = BRATIO(ISPC,D,H)
          if (LSKIPH) then
             HTG(K) = 0.0
             go to 4
          end if

          ! COMPUTE HT GROWTH AND AGE FOR ASPEN. EQN FROM WAYNE SHEPPARD RMS.
          ! ALSO USE FOR PAPER BIRCH

          if (ISPC.eq.20.or. ISPC.eq.28) then
             AG1 = ABIRTH(I)
             if (AG1.lt.5.)AG1 = 5.
             HITE1 = 26.9825 * AG1**1.1752
             AG2 = AG1+10.0
             HITE2 = 26.9825 * AG2**1.1752
             HTGR = (HITE2-HITE1)/(2.54*12.0) * RSIMOD * CON

             ! GROWTH RATES APPEAR HIGH, REDUCE BY 25 PERCENT. DIXON 8-27-92

             HTGR = HTGR * .75
             go to 3
          end if
          X = FLOAT(ICR(I))/100.
          VIGOR = (150.0 * (X**3.0)*EXP(-6.0*X))+0.3
          if (VIGOR .gt.1.0)VIGOR = 1.0

          !  VIGOR ADJUSTMENT TO DRASTIC FOR PINYON, JUNIPER, OAK, BRISTLECONE
          !  CUT IT BY TWO-THIRDS

          if (IVFLAG .eq.1) VIGOR = 1.-((1.-VIGOR)/3.)

          HTGR = POTHTG*PCTRED*VIGOR*CON
          if (DEBUG) write(JOSTND,*)'POTHTG,PCTRED,VIGOR,CON,X,HTGR= ',&
               &POTHTG,PCTRED,VIGOR,CON,X,HTGR
          3 continue
          ZZRAN = 0.0
          if (DGSD.ge.1.0) ZZRAN = BACHLO(0.0,1.0,RANN)
          if ((ZZRAN .gt.0.5) .or. (ZZRAN .lt. -2.0)) go to 3
          if (DEBUG)write(JOSTND,9984) HTGR,ZZRAN,XRHGRO,SCALE,WK4(I)
          9984 format('IN REGENT 9984 FORMAT',5(F10.4,2X))
          HTGR = (HTGR +ZZRAN*0.2)*XRHGRO * SCALE * WK4(I)

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
          !     THAN BREAKPOINT (COMPUTE 10-YEAR DBH INCREMENT REGARDLESS OF
          !     PROJECTION PERIOD LENGTH).

          if (D.ge.BKPT) go to 23
          HK = H + HTG(K)
          if (HK .le.4.5) then
             DG(K) = 0.0
             DBH(K) = D+0.001*HK
          else

             !  PONDEROSA PINE, CHIHUAHUA PINE

             if (ISPC.eq.13.or. ISPC.eq.36) then
                DK = (HK-8.31485+.59200*7.)/3.03659
                if (DK .lt.0.1) DK = 0.1
                DKK = (H-8.31485+.59200*7.)/3.03659
                if (DKK .lt.0.1) DKK = 0.1
                if (H .lt.4.5) DKK = D

                !  THE FOLLOWING FOR PINYON, JUNIPER & OAK DEVELOPED BY DIXON
                !  ALSO USED FOR BRISTLECONE

             else if (IVFLAG .eq.1) then
                DK = (HK-4.5)*10./(SITEAR(ISPC)-4.5)
                if (DK .lt.0.1) DK = 0.1
                DKK = (H-4.5)*10./(SITEAR(ISPC)-4.5)
                if (DKK .lt.0.1) DKK = 0.1
                if (H .lt.4.5) DKK = D

                !  ALL OTHER SPECIES

             else
                BX = HT2(ISPC)
                if (IABFLG(ISPC).eq.1) then
                   AX = HT1(ISPC)
                else
                   AX = AA(ISPC)
                end if
                DK = (BX/(ALOG(HK-4.5)-AX))-1.0
                if (DK .lt.0.1) DK = 0.1
                if (H .le.4.5) then
                   DKK = D
                else
                   DKK = (BX/(ALOG(H-4.5)-AX))-1.0
                end if
                if (DEBUG)write(JOSTND,9986) AX,BX,ISPC,HK,BARK,&
                     &XRDGRO,DK,DKK
                9986 format('IN REGENT 9986 FORMAT AX,BX,ISPC,HK',&
                     &' BARK,XRDGRO,DK,DKK= '/T12, F10.3,2X,F10.3,2X,I5,2X,5F10.3)
             end if

             !         IF CALLED FROM **ESTAB** ASSIGN DIAMETER

             if (LESTB) then
                DBH(K) = DK
                if (DBH(K).lt.DIAM(ISPC)) DBH(K) = DIAM(ISPC)
                DBH(K) = DBH(K)+0.001*HK
                DG(K) = DBH(K)
             else

                !         COMPUTE DIAMETER INCREMENT BY SUBTRACTION, APPLY USER
                !         SUPPLIED MULTIPLIERS, AND CHECK TO SEE IF COMPUTED VALUE
                !         IS WITHIN BOUNDS.

                if (DK.lt.0.0.or. DKK.lt.0.0) then
                   DG(K) = HTG(K)*0.2*BARK*XRDGRO
                   DK = D+DG(K)
                else
                   DG(K) = (DK-DKK)*BARK*XRDGRO
                end if
                if (DG(K) .lt.0.0) DG(K) = 0.0
                if (DG(K) .gt. DGMX) DG(K) = DGMX

                !         SCALE DIAMETER INCREMENT TO 10-YR ESTIMATE.
                !         SCALE ADJUSTMENT IS ON GROWTH IN DDS RATHER THAN INCHES
                !         OF DG TO BE CONSISTENT WITH GRADD.

                DDS = DG(K)*(2.0*BARK*D + DG(K))*SCALE2
                DG(K) = SQRT((D*BARK)**2 + DDS) - BARK*D
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
    SCALE3 = REGYR / FINTH

    ! COMPUTE DENSITY MODIFIER FROM CCF AND TOP HEIGHT.

    X = AVH*(RELDEN/100.0)
    if (X .gt.300.0) X = 300.0
    PCTRED = AB(1)&
         &+ X*(AB(2) + X*(AB(3) + X*(AB(4) + X*(AB(5)+ X*AB(6)))))
    if (PCTRED .gt.1.0) PCTRED = 1.0
    if (PCTRED .lt.0.01) PCTRED = 0.01
    if (DEBUG)write(JOSTND,9989)AVH,RELDEN,X,PCTRED
    9989 format('IN REGENT AVH,RELDEN,X,PCTRED = ',4F10.4)
    do ISPC = 1,MAXSP

       !  BEGIN PROCESSING TREE LIST IN SPECIES ORDER.  DO NOT CALCULATE
       !  CORRECTION TERMS IF THERE ARE NO TREES FOR THIS SPECIES.

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
       if (SI .gt. SITEHI(ISPC)) SI = SITEHI(ISPC)
       if (SI .le. SITELO(ISPC)) SI = SITELO(ISPC) + 0.5

       !  COMPUTE POTENTIAL HEIGHT GROWTH AS A FUNCTION OF SI AND SPECIES.

       RELSI = (SI-SITELO(ISPC))/(SITEHI(ISPC)-SITELO(ISPC))
       RSIMOD = 0.5 * (1.0 + RELSI)
       X = RELSI*100.0
       J = MPCC(ISPC)
       POTHTG = SITEAR(ISPC)/(15.0 - 4.0*RELSI) * HTADJ(ISPC)

       if (DEBUG)write(JOSTND,9988)SI,RELSI,X,ISPC,J,POTHTG
       9988 format('IN REGENT SI,RELSI,X,ISPC,J,POTHTG = ',&
            &F7.2,F10.3,F10.4,2I6,F10.3)

       !  SET FLAG FOR VIGOR ADJUSTMENT

       select case(ISPC)
       case(9,12,16,23:27,29:35)
          IVFLAG = 1
       case default
          IVFLAG = 0
       end select

       !  BEGIN TREE LOOP WITHIN SPECIES.  IF MEASURED HEIGHT INCREMENT IS
       !  LESS THAN OR EQUAL TO ZERO, OR DBH IS GREATER THAN 5.0,
       !  THE RECORD WILL BE EXCLUDED FROM THE CALIBRATION.

       do I3 = I1,I2
          I = IND1(I3)
          H = HT(I)

          !  DIA GT 3 INCHES INCLUDED IN OVERALL MEAN

          if (IHTG.lt.2) H = H-HTG(I)
          if (DBH(I).ge.5.0.or. H.lt.0.01) go to 60

          !  COMPUTE VIGOR MODIFIER FROM CROWN RATIO.

          X = FLOAT(ICR(I))/100.
          VIGOR = (150.0 * (X**3.0)*EXP(-6.0*X))+0.3
          if (VIGOR .gt.1.0)VIGOR = 1.0

          !  VIGOR ADJUSTMENT TO DRASTIC FOR PINYON, JUNIPER, OAK, BRISTLECONE
          !  CUT IT BY TWO-THIRDS

          if (IVFLAG .eq.1) VIGOR = 1.-((1.-VIGOR)/3.)
          EDH = POTHTG*PCTRED*VIGOR*RHCON(ISPC)

          ! COMPUTE ASPEN HTG FOR CALIBRATION. EQNS FROM WAYNE SHEPPARD RMS.
          ! ALSO USED FOR PAPER BIRCH

          if (ISPC.eq.20.or. ISPC.eq.28) then
             AG1 = (H*12.0*2.54/26.9825)**0.8509
             AG2 = AG1 + 10.0
             H2 = (26.9825*AG2**1.1752)/(2.54*12.0)
             EDH = (H2-H) * RSIMOD * RHCON(ISPC)

             ! GROWTH RATES APPEAR HIGH, REDUCE BY 25 PERCENT. DIXON 8-27-92.

             EDH = EDH*.75
             if (EDH .lt.0.0) EDH = 0.0
          end if
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

       call REGHCAL(ISPC,SNP,SNX,SNY,CORNEW)
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

    call REGCALOUT(CORTEM,NUMCAL,KOUT,K,KK,ISPEC,SPEC)
    91 if (DEBUG)write(JOSTND,9995)ICYC
    9995 format('LEAVING SUBROUTINE REGENT  CYCLE =',I5)
    return
    9998 continue

    !  ENTRY POINT FOR LOADING OF REGENERATION GROWTH MODEL
    !  CONSTANTS  THAT REQUIRE ONE-TIME RESOLUTION.

    do ISPC = 1,MAXSP
       RHCON(ISPC) = 1.0
       if (LRCOR2.and. RCOR2(ISPC).gt.0.0)&
            &RHCON(ISPC) = RCOR2(ISPC)
       90 continue
    end do
    return
  end subroutine REGCR

  !===============================!
  !   Compute small tree growth   !
  !===============================!
  ! EC $Id$
  subroutine REGEC(LCON)
    implicit none

    logical :: LCON
    ! Regional callees retain their native argument lists.
    interface
       subroutine SMHTGF(MODE,ICYC,ISPC,H,DTIME,HHT,JOSTND,DEBUG)
         integer :: MODE
         integer :: ICYC
         integer :: ISPC
         real :: H
         real :: DTIME
         real :: HHT
         integer :: JOSTND
         logical :: DEBUG
       end subroutine SMHTGF
    end interface


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
    logical :: DEBUG,LSKIPH
    character(len=2) :: SPEC
    integer :: NUMCAL(32)
    integer :: IREFI,ISPEC,KOUT,KK,MODE1
    integer :: ISPC,I1,I2,I3,I,IPCCF,K,L,N
    real    :: H,BARK,VIGOR,TEMT,POTHTG,HTGR,ZZRAN,XWT,DAT45
    real    :: CON,XMX,XMN,SI,DGMX,D,CR,RAN,BACHLO,BRATIO,HK,DK,DKK
    real    :: BX,AX,DDS,HTNEW,SCALE3,CORNEW,SNP,SNX,SNY,EDH,P,TERM
    real    :: REGYR,FNT,SCALE,SCALE2,CCF,AVHT,X,PCTRED,XRHGRO,XRDGRO
    real    :: CORTEM(32)
    real    :: XMAX(32),XMIN(32)
    real    :: SLO(32),SHI(32),AB(9),DGMAX(32),DIAM(32)

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

    if (LCON) go to 9998

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
                DG(K) = SQRT((D*BARK)**2+DDS)-BARK*D
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

       call REGHCAL(ISPC,SNP,SNX,SNY,CORNEW)
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

    call REGCALOUT(CORTEM,NUMCAL,KOUT,K,KK,ISPEC,SPEC)
    91 if (DEBUG)write(JOSTND,9995)ICYC
    9995 format('LEAVING SUBROUTINE REGENT  CYCLE =',I5)
    return

    9998 continue

    !  ENTRY POINT FOR LOADING OF REGENERATION GROWTH MODEL
    !  CONSTANTS  THAT REQUIRE ONE-TIME RESOLUTION.

    do ISPC = 1,MAXSP
       RHCON(ISPC) = 1.0
       if (LRCOR2.and.RCOR2(ISPC).gt.0.0)&
            &RHCON(ISPC) = RCOR2(ISPC)
       90 continue
    end do
    return
  end subroutine REGEC

  !===============================!
  !   Compute small tree growth   !
  !===============================!
  ! EM $Id$
  subroutine REGEM(LCON)
    implicit none

    logical :: LCON
    ! Regional callees retain their native argument lists.
    interface
       subroutine SMHTGF(ISPC,I,HTGRTH,CR,TPCCF)
         integer :: ISPC
         integer :: I
         real :: HTGRTH
         real :: CR
         real :: TPCCF
       end subroutine SMHTGF
    end interface


    !  **REGENT** COMPUTES HEIGHT AND DIAMETER INCREMENTS FOR SMALL
    !  TREES.  THE HEIGHT INCREMENT MODEL IS APPLIED TO TREES THAT ARE LESS
    !  THAN 10 INCHES DBH (5 INCHES FOR LODGEPOLE PINE), AND THE DBH
    !  INCREMENT MODEL IS APPLIED TO TREES THAT ARE LESS THAN 3 INCHES DBH.
    !  FOR TREES THAT ARE GREATER THAN 2 INCHES DBH (1 INCH FOR LODGEPOLE
    !  PINE), HEIGHT INCREMENT PREDICTIONS ARE AVERAGED WITH THE PREDICTIONS
    !  FROM THE LARGE TREE MODEL. HEIGHT INCREMENT IS A FUNCTION OF HABITAT
    !  TYPE, SLOPE, ASPECT, TREE HEIGHT, BASAL AREA IN LARGER TREES, AND
    !  STAND CROWN COMPETITION FACTOR.  DIAMETER IS ASSIGNED FROM A HEIGHT
    !  DIAMETER FUNCTION WITH ADJUSTMENTS FOR RELATIVE SIZE AND STAND
    !  DENSITY.  INCREMENT IS COMPUTED BY SUBTRACTION. WHEN THE TREE ATTAINS
    !  A DBH OF 3.0 INCHES IN A CYCLE, **DUBSCR** IS CALLED TO ASSIGN A CROWN
    !  RATIO.   **REGENT** IS CALLED FROM **CRATET** DURING CALIBRATION AND
    !  FROM **GRINCR** DURING CYCLING.  ENTRY **REGCON** IS CALLED FROM
    !  **RCON** TO LOAD MODEL PARAMETERS THAT NEED ONLY BE RESOLVED ONCE.

    !  **REGENT** IS ALSO USED TO PREDICT HEIGHT INCREMENT, DBH AND CROWN
    !  RATIO FOR TREES THAT ARE CREATED BY THE REGENERATION ESTABLISHMENT
    !  MODEL.  5-YEAR OLD TREES ARE GENERATED BY **ESTAB** AND **REGENT**
    !  COMPUTES INCREMENTS FROM AGE 5 TO THE END OF THE CYCLE.

    ! Common blocks

    !  DIMENSIONS FOR INTERNAL VARIABLES:

    !   BANEXT -- ESTIMATED STAND BASAL AREA AT START OF EACH HEIGHT INCREMENT
    !             PREDICTION SUBCYCLE.
    !   CORTEM -- A TEMPORARY ARRAY FOR PRINTING CORRECTION TERMS.
    !     KPER -- NUMBER OF YEARS (MAX OF 5) IN EACH HEIGHT INCREMENT PREDICTION
    !             SUBCYCLE.
    !   NUMCAL -- A TEMPORARY ARRAY FOR PRINTING NUMBER OF HEIGHT
    !             INCREMENT OBSERVATIONS BY SPECIES.
    !   RDNEXT -- RELATIVE DENSITY (CCF) AT START OF EACH HEIGHT INCREMENT
    !             PREDICTION SUBCYCLE.
    !    RHCON -- CONSTANT FOR THE HEIGHT INCREMENT MODEL.  INCLUDES EFFECTS
    !             OF HABITAT TYPE, LOCATION, SLOPE, AND ASPECT AS LOADED IN
    !             RCON.  HABITAT EFFECTS ARE BORROWED FROM LARGE TREE DBH
    !             INCREMENT MODEL FOR ALL BUT TYPES 520, 530, AND 570.
    !             ADJUSTMENTS ARE THE DIFFERENCE BETWEEN THE HABITAT
    !             CONSTANT FOR A GIVEN TYPE AND THE CONSTANT FOR TYPE 520.
    !     XMAX -- UPPER END OF THE RANGE OF DIAMETERS OVER WHICH HEIGHT
    !             INCREMENT PREDICTIONS FROM SMALL AND LARGE TREE MODELS
    !             ARE AVERAGED.
    !     XMIN -- LOWER END OF THE RANGE OF DIAMETERS OVER WHICH HEIGHT
    !             INCREMENT PREDICTIONS FROM THE SMALL AND LARGE TREE
    !             ARE AVERAGED.

    external RANN
    ! Variables
    logical :: DEBUG,LSKIPH,CRVAR,NIVAR,UTVAR,TTVAR,EMVAR
    real    :: CORTEM(19)
    integer :: NUMCAL(19)
    real    :: XMAX(19),XMIN(19),RHHAB(5)
    integer :: MAPHAB(30)
    real    :: RHGL(3),DIAM(19),BANEXT(10),RDNEXT(10)
    integer :: KPER(10)
    real    :: DGMAX(19),SLO(19),SHI(19)
    real    :: RELH,DADJ,HTGRL,BETA1,BETA2,HTG1,STDDEV,VIGOR,HITE1,AG2
    real    :: HITE2,H2,DGJ,HLESS4,DLESS3,DGMX,HK,DK,HTGR,HTGR1
    real    :: ZZRAN,DKK,DDS,DGK,SCALE2,DDS2,DG2,DNEW,SCALE3,X,PCTRED,BAJ
    real    :: RDJ,PPCCF,XRHGRO,XRDGRO,CON,BH,BCCF,BBAL,XMX,XMN,AX,BX,D
    real    :: H1,H,BAL,BARK,SI,RSIMOD,POTHTG,TPCCF,XWT,CR,RAN,R
    real    :: AH,DELMAX,CORNEW,SNP,SNX,SNY,BHAB,BLH,EDH,AG1,TERM
    real    :: RSAB2,RSAB1,RSAB0,BAYR,CCFYR,BACHLO,D1,P,D2,BRATIO,B1,B2
    real    :: CW,C1,C2,BI,CI,PN,BACON,HSIGMA,REGYR,TEMBA,TEMCCF,TEMAHT
    real    :: SCALE,RELSI,HTGRR,HKK
    integer :: I,NTYR,IYR,NPER,ITOT,N,J,IS,K,NYR,IPCCF,KY
    integer :: ISPC,I1,I2,I3,L,ICRK,IITRE,IREFI,KOUT,ISPEC,KK
    character(len=2) :: SPEC

    !  SPECIES ORDER:
    !   1=WB,  2=WL,  3=DF,  4=LM,  5=LL,  6=RM,  7=LP,  8=ES,
    !   9=AF, 10=PP, 11=GA, 12=AS, 13=CW, 14=BA, 15=PW, 16=NC,
    !  17=PB, 18=OS, 19=OH

    !  SPECIES EXPANSION
    !  LM USES IE LM (ORIGINALLY FROM TT VARIANT)
    !  LL USES IE AF (ORIGINALLY FROM NI VARIANT)
    !  RM USES IE JU (ORIGINALLY FROM UT VARIANT)
    !  AS,PB USE IE AS (ORIGINALLY FROM UT VARIANT)
    !  GA,CW,BA,PW,NC,OH USE IE CO (ORIGINALLY FROM CR VARIANT)

    !  NOTE: UT AND CR LOGIC WAS MODIFIED FROM 10-YEAR REGYR TO 5-YEAR

    ! Data statements
    data DIAM/&
         &0.4,      0.3,      0.3,      0.3,      0.3,      0.3,&
         &0.4,      0.3,      0.3,      0.5,      0.2,      0.2,&
         &0.2,      0.2,      0.2,      0.2,      0.2,      0.2,&
         &0.2/
    data XMAX/&
         &3.0,      3.0,      3.0,      3.0,     10.0,     99.0,&
         &3.0,      3.0,      3.0,      3.0,      2.0,      4.0,&
         &2.0,      2.0,      2.0,      2.0,      4.0,      3.0,&
         &2.0/

    data XMIN/&
         &1.5,      1.5,      1.5,      1.5,      2.0,     90.0,&
         &1.5,      1.5,      1.5,      1.5,      0.5,      2.0,&
         &0.5,      0.5,      0.5,      0.5,      2.0,      1.5,&
         &0.5/

    data REGYR/5.0/,HSIGMA/0.59/,BACON/0.005454154/

    data DGMAX/&
         &0.0,      0.0,      0.0,      2.0,     99.0,      2.0,&
         &0.0,      0.0,      0.0,      0.0,      2.5,      2.5,&
         &2.5,      2.5,      2.5,      2.5,      2.5,      0.0,&
         &2.5/

    data SLO/&
         &0.0,      0.0,      0.0,      0.0,      0.0,      5.0,&
         &0.0,      0.0,      0.0,      0.0,     30.0,     30.0,&
         &30.0,     30.0,     30.0,     30.0,     30.0,      0.0,&
         &30.0/

    data SHI/&
         &0.0,      0.0,      0.0,      0.0,      0.0,     15.0,&
         &0.0,      0.0,      0.0,      0.0,    120.0,     70.0,&
         &120.0,    120.0,    120.0,    120.0,     70.0,      0.0,&
         &120.0/

    if (LCON) go to 9998

    !  CHECK FOR DEBUG.

    call DBCHK (DEBUG,'REGENT',6,ICYC)
    if (DEBUG) write(JOSTND,9980)ICYC
    9980 format('ENTERING SUBROUTINE REGENT  CYCLE =',I5)

    !  THE SMALL TREE HEIGHT INCREMENT MODEL IS BASED ON 5-YEAR INCREMENT DATA.
    !  TO AVOID BIAS, PREDICTIONS ARE MADE FOR SUBCYCLES THAT ARE LESS THAN OR
    !  EQUAL TO 5 YEARS IN LENGTH.  THE FOLLOWING CODE DETERMINES THE NUMBER
    !  AND LENGTH OF SUBCYCLES FROM CYCLE LENGTH.

    LSKIPH = .false.
    do I = 1,10
       KPER(I) = 0
       BANEXT(I) = 0.0
       RDNEXT(I) = 0.0
       1 continue
    end do
    NTYR = INT(FINT)
    if (LSTART) NTYR = IFINTH
    if (LESTB) NTYR = NTYR-5
    if (LESTB.and.NTYR.le.0) LSKIPH = .true.
    IYR = INT(REGYR)
    NPER = NTYR/IYR
    if (MOD(NTYR,IYR).ne.0) NPER = NPER+1
    ITOT = NTYR
    N = NPER
    do I = 1,NPER
       if (N.eq.1) go to 3
       KPER(I) = ITOT/N
       ITOT = ITOT-KPER(I)
       N = N-1
       2 continue
    end do
    3 continue
    if (NPER.gt.0)KPER(NPER) = ITOT

    !  LOAD TEMPORARY VARIABLES WITH AFTER THINNING BA, CCF, AND TOPHT.

    TEMBA = ATBA
    if (ATBA .le.0.)TEMBA = BA
    TEMCCF = ATCCF
    if (ATCCF .le.0.)TEMCCF = RELDEN
    TEMAHT = ATAVH

    !  IF THIS IS THE FIRST ENTRY, BRANCH TO THE CALIBRATION SECTION.

    if (LSTART) goto 40

    !  LOAD MULTIPLIERS.

    call MULTS (3,IY(ICYC),XRHMLT)
    call MULTS(6,IY(ICYC),XRDMLT)
    SCALE = YR/FINT
    if (DEBUG) write(JOSTND,8996) LESTB,LSKIPH,ITRN,ITRNIN,SCALE,NPER
    8996 format('IN REGENT, LESTB = ',L1,', LSKIPH = ',L1,&
         &', ITRN = ',I4,', ITRNIN = ',I4,', SCALE = ',F6.3,&
         &', NPER = ',I3)
    if (ITRN.le.0) return

    !  ESTIMATE THE CONTRIBUTION TO STAND BASAL AREA AND CCF FOR LARGE TREES AT
    !  THE START OF EACH PROJECTION SUBCYCLE.  IF CALLED FROM **ESTAB**, BRANCH
    !  TO STATEMENT 9 AND INTERPOLATE FROM OLD AND NEW DENSITY VALUES.
    !  OTHERWISE, 10-YEAR CHANGE IS COMPUTED FROM PREDICTED DBH INCREMENT AND
    !  ANNUALIZED.  THEN, ANNUAL RATE IS MULTIPLIED BY SUBCYCLE LENGTH TO
    !  DETERMINE CONTRIBUTION FOR EACH SUBCYCLE.  A ONE PERCENT ANNUAL RATE
    !  OF MORTALITY IS ASSUMED.  CONTRIBUTIONS OF SMALL TREES ARE COMPUTED AS
    !  INCREMENT PREDICTIONS ARE MADE FOR PRECEDING SUBCYCLE.

    if (LESTB) go to 8
    do J = 1,NPER
       BANEXT(J) = BA
       RDNEXT(J) = RELDEN
       4 continue
    end do
    if (DEBUG) write(JOSTND,*)'NPER,BA,RELDEN= ',NPER,BA,RELDEN
    if (NPER.eq.1) go to 11
    do I = 1,ITRN
       D1 = DBH(I)
       if (D1.lt.3.0) go to 6
       IS = ISP(I)
       P = PROB(I)
       D2 = D1+DG(I)/BRATIO(IS,D1,HT(I))
       B1 = BACON*D1*D1
       B2 = BACON*D2*D2
       call CCFCAL(IS,D1,HT(I),ICR(I),P,.false.,C1,CW,1)
       call CCFCAL(IS,D2,HT(I),ICR(I),P,.false.,C2,CW,1)
       BI = (B2-B1)/10.0
       CI = (C2-C1)/10.0
       K = 0
       do J = 2,NPER
          K = K+KPER(J-1)
          PN = P*0.985**K
          RDNEXT(J) = RDNEXT(J)+K*CI/P*PN
          BANEXT(J) = BANEXT(J)+K*BI*PN
          5 continue
       end do
       6 continue
    end do
    go to 11

    !  IF CALLED FROM **ESTAB**, DIAMETERS AND DENSITY STATISTICS HAVE
    !  BEEN UPDATED AND DENSITIES CORRESPONDING TO BEGINNINGS OF SUBCYCLES
    !  CAN BE INTERPOLATED FROM DIFFERENCE BETWEEN OLD AND NEW STATS.  THE
    !  TREES ARE SENT TO REGENT WITH A HEIGHT CORRESPONDING TO 5 YEARS AFTER
    !  THE START OF THE CYCLE.

    8 continue
    BAYR = 0.0
    CCFYR = 0.0
    if (LSKIPH) go to 9
    BAYR = (BA-TEMBA)/ITOT
    CCFYR = (RELDEN-TEMCCF)/ITOT
    9 continue
    NYR = 5
    do J = 1,NPER
       RDNEXT(J) = TEMCCF+NYR*CCFYR
       BANEXT(J) = TEMBA+NYR*BAYR
       NYR = NYR+KPER(J)
       10 continue
    end do
    11 continue
    if (DEBUG)write(JOSTND,*)'IN REGENT, NPER,RDNEXT,BANEXT= ',&
         &NPER,RDNEXT,BANEXT

    !  STORE INITIAL HEIGHTS IN WK3; DUB CROWN RATIO FOR NEWLY ESTABLISHED
    !  SEEDLINGS.

    do I = 1,ITRN
       if (LESTB.and.I.ge.ITRNIN) then
          IPCCF = ITRE(I)
          CR = 0.89722 - 0.0000461*PCCF(IPCCF)
          12 continue
          RAN = BACHLO(0.0,1.0,RANN)
          if (RAN .lt. -1.0.or. RAN .gt.1.0) go to 12
          CR = CR + 0.07985 * RAN
          if (CR .gt..90) CR = .90
          if (CR .lt..20) CR = .20
          ICR(I) = INT((CR*100.0)+0.5)
          IF(DEBUG)WRITE(JOSTND,*)'DUB CROWN ESTAB TREES I,PCCF,RAN,CR,I  &
               &  CR= ',I,PCCF(IPCCF),RAN,CR,ICR(I)
       end if
       WK3(I) = HT(I)
       WK5(I) = DBH(I)
       13 continue
    end do

    !  DELMAX IS USED TO COMPUTE A DENSITY-DEPENDENT ADJUSTMENT FOR
    !  THE HEIGHT-DIAMETER FUNCTION.  DELMAX IS BASED ON AVERAGE HEIGHT AND
    !  CCF AT THE BEGINNING OF THE CYCLE AND DOES NOT VARY BY SUBCYCLE.

    if (LESTB) then
       R = TEMCCF
       AH = TEMAHT
    else
       R = RELDEN
       AH = AVH
    end if
    DELMAX = (AH/36.0)*(0.01232*R-1.75)
    if (DELMAX.gt.0.0) DELMAX = 0.0

    ! COMPUTE DENSITY MODIFIER FROM CCF AND TOP HEIGHT FOR SPECIES FROM
    ! THE CR & UT VARIANTS

    X = AH*(R/100.0)
    if (X .gt.300.0) X = 300.0
    PCTRED = 1.11436 + X*(-.011493 + X*(.43012E-4 + X*(-.72221E-7&
         &+ X*(.5607E-10- X*0.1641E-13))))
    if (PCTRED .gt.1.0) PCTRED = 1.0
    if (PCTRED .lt.0.01) PCTRED = 0.01
    if (DEBUG) write(JOSTND,9982) AH,R,X,PCTRED
    9982 format('IN REGENT AVHT,CCF,X,PCTRED = ',4F10.4)
    if (DEBUG) write(JOSTND,8995) R,AH,DELMAX
    8995 format('R = ',F10.4,', AH = ',F10.4,', DELMAX = ',F10.4)

    !  ENTER LOOP FOR NEXT SUBCYCLE.

    KY = 0
    do J = 1,NPER
       KY = KY+KPER(J)
       BAJ = BANEXT(J)
       RDJ = RDNEXT(J)

       !  PPCCF IS A PROPORTIONAL ADJUSTMENT FOR POINT CCF VALUES BASED ON
       !  CHANGE IN STAND CCF FOR THE SUBCYCLE.

       PPCCF = 0.0
       if (RELDEN.gt.0.0) PPCCF = 1.0+(RDJ-RELDEN)/RELDEN

       !  PROCESS ALL TREES WITHIN THIS SUBCYCLE IN SPECIES ORDER.  LOAD
       !  SPECIES DEPENDENT CONSTANTS.

       do ISPC = 1,MAXSP
          I1 = ISCT(ISPC,1)
          if (I1.eq.0) go to 16
          I2 = ISCT(ISPC,2)

          !  ASSIGN VARIANT ACCORDING TO SPECIES

          EMVAR = .false.
          NIVAR = .false.
          TTVAR = .false.
          UTVAR = .false.
          CRVAR = .false.
          if (ISPC .eq.5) then
             NIVAR = .true.
          else if (ISPC .eq.4) then
             TTVAR = .true.
          else if (ISPC.eq.11.or. (ISPC.ge.13.and. ISPC.le.16) .or.&
               &ISPC.eq.19 ) then
             CRVAR = .true.
          else if (ISPC.eq.6.or. ISPC.eq.12.or. ISPC.eq.17) then
             UTVAR = .true.
          else
             EMVAR = .true.
          end if
          if (DEBUG)write(JOSTND,*)'NIVAR,TTVAR,UTVAR,CRVAR,LSKIPH= ',&
               &NIVAR,TTVAR,UTVAR,CRVAR,LSKIPH
          if ((TTVAR.or.CRVAR.or.EMVAR).and.LSKIPH)goto 16

          !  SPECIES BASED ON A 10-YR REGYR ONLY NEED ONE PASS THROUGH THIS LOGIC

          if ((CRVAR .or. UTVAR) .and. J.gt.1)go to 16

          XRHGRO = XRHMLT(ISPC)
          XRDGRO = XRDMLT(ISPC)
          if (NIVAR) then
             CON = RHCON(ISPC) +HCOR(ISPC)
          else
             CON = RHCON(ISPC)*EXP(HCOR(ISPC))
          end if

          !  VALUES FOR THE NEXT 3 VARIABLES ARE FROM SUBALPINE FIR IN NI/IE
          !  USED FOR SUBALPINE LARCH (LL) IN THIS VARIANT

          BH = 0.3740
          BCCF = -.00391
          BBAL = -0.22957
          XMX = XMAX(ISPC)
          XMN = XMIN(ISPC)

          !  PROCESS NEXT TREE RECORD.  STORE INTERMEDIATE HEIGHTS IN WK3.

          do I3 = I1,I2

             !  BYPASS INCREMENT CALCULATION IF DBH IS GREATER THAN XMX OR IF
             !  CALL IS FROM **ESTAB** AND TREE SUBSCRIPT IS LESS THAN ITRNIN.

             I = IND1(I3)
             D = DBH(I)
             H = HT(I)
             if (DEBUG)write(JOSTND,*)'ISPC,I,D,XMX,LESTB,ITRNIN= ',&
                  &ISPC,I,D,XMX,LESTB,ITRNIN
             if (D.ge.XMX) go to 15

             if (LESTB .and. I.lt.ITRNIN) go to 15
             P = PROB(I)
             H1 = WK3(I)
             D1 = WK5(I)
             BAL = BAJ*(100.0-PCT(I))*0.0001
             call CCFCAL(ISPC,D,H1,ICR(I),P,.false.,C1,CW,1)
             B1 = BACON*D*D
             BARK = BRATIO(ISPC,D,H)

             if (CRVAR.or.UTVAR) then
                SI = SITEAR(ISPC)
                if (SI .gt. SHI(ISPC)) SI = SHI(ISPC)
                if (SI .le. SLO(ISPC)) SI = SLO(ISPC) + 0.5

                !  COMPUTE POTENTIAL GROWTH FROM SITE INDEX AND SPECIES.

                RELSI = (SI-SLO(ISPC))/(SHI(ISPC)-SLO(ISPC))
                RSIMOD = 0.5 * (1.0 + RELSI)
                X = RELSI*100.0
                if (CRVAR) then
                   POTHTG = SITEAR(ISPC)/(15.0 - 4.0*RELSI) * 1.0
                else if (ISPC.eq.6) then
                   POTHTG = (SI/10.0)*(SI*1.5-H1)/(SI*1.5)
                else
                   POTHTG = 0.
                end if
                if (DEBUG)write(JOSTND,*)'I,SI,H1,RELSI,POTHTG= ',&
                     &I,SI,H1,RELSI,POTHTG
             end if

             !  DETERMINE POINT CCF FOR THIS SUBCYCLE (BOUND BETWEEN 25 AND 300).

             IPCCF = ITRE(I)
             TPCCF = PCCF(IPCCF)*PPCCF
             if (TPCCF.gt.300.0) TPCCF = 300.0
             if (TPCCF.lt.25.0) TPCCF = 25.0
             CR = FLOAT(ICR(I))

             !  NI VARIANT CODE
             !  COMPUTE ADJUSTMENT FOR BIAS, DENSITY, AND RELATIVE SIZE.

             if (NIVAR) then
                if (ABS(AH-4.5).lt.0.01 ) then
                   RELH = 0.0
                else
                   RELH = (H1-4.5)/(AH-4.5)
                end if
                if (RELH.gt.1.0) RELH = 1.0
                if (RELH.lt.0.0) RELH = 0.0
                DADJ = DELMAX*RELH*RELH - 2.0*DELMAX*RELH + 0.65

                !  COMPUTE HEIGHT INCREMENT.  STORE UPDATED HEIGHT IN WK3.

                HTGRL = (CON+BH*ALOG(H1)+BCCF*RDJ+BBAL*BAL)
                IF(DEBUG)WRITE(JOSTND,*)'NI SECTION, I,AH,H1,RELH,DELMAX,DADJ,  &
                     &  CON,BH,BCCF,RDJ,BAL,HTGRL= ',I,AH,H1,RELH,DELMAX,DADJ,&
                     &CON,BH,BCCF,RDJ,BAL,HTGRL

                !  TT VARIANT CODE

             else if (TTVAR) then

                !  RESET ZRAND IF THIS IS THE START OF THE PROJECTION, THIS IS THE FIRST
                !  PREDICTION FOR A NEW TRIPLE, OR IF GROWTH IN THE PREVIOUS CYCLE WAS
                !  LESS THAN 0.1 FT.

                if (ZRAND(I).ne.-999.) go to 920
                910 continue
                ZRAND(I) = BACHLO(0.0,1.0,RANN)
                if (ZRAND(I).lt.-2.0.or. ZRAND(I).gt.2.0) go to 910
                920 continue
                D = DBH(I)
                if (D.le.0.0) then
                   HTGRL = 0.0
                   go to 930
                end if
                BETA1 = EXP(1.17527 - 0.42124*ALOG(TPCCF))
                BETA2 = EXP(-2.56002 - 0.58642*ALOG(TPCCF))
                HTG1 = BETA1 + BETA2*CR
                STDDEV = HTG1*(1.08720 - 0.00230*CR)
                HTGRL = HTG1 + ZRAND(I)*STDDEV
                IF(DEBUG)WRITE(JOSTND,*)'TT SECTION, I,D,TPCCF,BETA1,BETA2,CR,  &
                     &  HTG1,STDDEV,ZRAND,HTGRL= ',I,D,TPCCF,BETA1,BETA2,CR,HTG1,&
                     &STDDEV,ZRAND(I),HTGRL

                !  IF ESTIMATED HEIGHT INCREMENT IS CLOSE TO ZERO, SET FLAG TO DRAW
                !  A NEW RANDOM ERROR ON NEXT PASS THROUGH.

                if (HTGRL .le.0.1) then
                   HTGRL = 0.1
                   ZRAND(I) = -999.
                end if
                930 continue

                !  CR AND UT VARIANT CODE

             else if (CRVAR .or. UTVAR) then
                X = CR/100.
                VIGOR = (150.0 * (X**3.0)*EXP(-6.0*X))+0.3
                if (VIGOR .gt.1.0)VIGOR = 1.0

                !  VIGOR ADJUSTMENT TO DRASTIC FOR JUNIPER,
                !  CUT IT BY TWO-THIRDS

                if (ISPC.eq.6)VIGOR = 1.-((1.-VIGOR)/3.)

                ! COMPUTE HT GROWTH AND AGE FOR ASPEN. EQN FROM WAYNE SHEPPARD RMS.
                ! ALSO USED FOR PAPER BIRCH

                if (ISPC.eq.12.or. ISPC.eq.17) then
                   HITE1 = 26.9825 * ABIRTH(I)**1.1752
                   AG2 = ABIRTH(I)+10.0
                   HITE2 = 26.9825 * AG2**1.1752
                   HTGRL = (HITE2-HITE1)/(2.54*12.0) * RSIMOD * CON

                   ! GROWTH RATES APPEAR HIGH, REDUCE BY 25 PERCENT. DIXON 8-27-92

                   HTGRL = HTGRL * .75
                else
                   HTGRL = POTHTG*PCTRED*VIGOR*CON
                end if
                if (DEBUG) write(JOSTND,*)'CR/UT SECTION, I,POTHTG,PCTRED,',&
                     &'VIGOR,CON,X,HTGRL= ',I,POTHTG,PCTRED,VIGOR,CON,X,HTGRL

                !  EM VARIANT CODE

             else
                if (DEBUG)write(JOSTND,*)'EM SECTION CALLING SMHTGF,I,ISPC,CR,',&
                     &'TPCCF= ',I,ISPC,CR,TPCCF
                call SMHTGF(ISPC,I,HTGRR,CR,TPCCF)
                H2 = H1+HTGRR*(FLOAT(KPER(J))/REGYR)*XRHGRO*CON
                if (DEBUG)write(JOSTND,*)'AFTER SMHTGF H1,HTGRR,H2,KPER,REGYR,',&
                     &'XRHGRO,CON= ',H1,HTGRR,H2,KPER(J),REGYR,XRHGRO,CON
             end if

             ! H2 IS THE END OF CYCLE HEIGHT ON A FINT-YEAR BASIS
             ! CR AND UT ESTIMATES ARE 10-YEAR ESTIMATES AND THUS ADJUSTED BY FINT/10

             if (NIVAR) then
                H2 = H1+EXP(HTGRL)*(FLOAT(KPER(J))/REGYR)*XRHGRO
             else if (TTVAR) then
                H2 = H1+HTGRL*(FLOAT(KPER(J))/REGYR)*XRHGRO*CON
             else if (CRVAR .or. UTVAR) then
                H2 = H1+HTGRL*FINT/10.
             else
                H2 = H2
             end if
             WK3(I) = H2
             if (DEBUG)write(JOSTND,*)'I,H1,H2,KPER,REGYR,',&
                  &'XRHGRO,CON= ',&
                  &I,H1,H2,KPER(J),REGYR,XRHGRO,CON

             !  UPDATE DENSITY FOR NEXT SUBCYCLE.  IF ONLY ONE SUBCYCLE OR
             !  IF INITIAL DBH IS GREATER THAN 3 INCHES, BYPASS DENSITY UPDATE.
             !  DIAMETER IS ADJUSTED FOR DENSITY AND USER-SUPPLIED MULTIPLIER.
             !  INCREMENT IS DETERMINED FROM THE DIFFERENCE BETWEEN COMPUTED
             !  DIAMETERS AND IS BOUNDED TO BE GREATER THAN OR EQUAL TO ZERO.

             if (NIVAR) then
                AX = 0.0658
                BX = 1.3817
                D2 = D
                if (J.ge.NPER.or.D.ge.3.0) go to 14
                if (H2.le.4.5) go to 14
                D1 = DIAM(ISPC)+DADJ
                if (H1.gt.4.5) D1 = AX*(H1-4.5)**BX + DADJ
                D2 = AX*(H2-4.5)**BX + DADJ
                DGJ = (D2-D1)*XRDGRO
                if (DEBUG)write(JOSTND,*)'NI SECTION, I,ISPC,AX,BX,DADJ,H1,D1,',&
                     &'H2,D2,DGJ= ',&
                     &I,ISPC,AX,BX,DADJ,H1,D1,H2,D2,DGJ
                if (DGJ.lt.0.0) DGJ = 0.0
                D2 = D+DGJ
             else if (TTVAR) then
                HLESS4 = H2-4.5
                DLESS3 = 0.000231*HLESS4*CR - 0.00005*HLESS4*PCCF(ITRE(I))&
                     &+ 0.001711*CR + 0.17023*HLESS4
                D2 = (DLESS3+0.3)*XRDGRO
                if (D2.lt.DIAM(ISPC)) D2 = DIAM(ISPC)
                if (DEBUG)write(JOSTND,*)'TT SECTION, I,ISPC,CR,PCCF,H2,D2,',&
                     &'XRDGRO= ',I,ISPC,CR,PCCF(ITRE(I)),H2,D2,XRDGRO
                WK3(I) = H2
                WK5(I) = D2
                if (J.ge.NPER .or. D.ge.3.0.or. H2.le.4.5)go to 14
             else if (CRVAR) then
                D2 = D
                if (D.ge.1.0)go to 15
                if (H2.le.4.5)D2 = D+0.0001*H2
                if (DEBUG)write(JOSTND,*)'CR SECTION, I,D,D2,H2= ',I,D,D2,H2
             else if (UTVAR) then
                D2 = D
                if ((ISPC.eq.12.or. ISPC.eq.17) .and. D.ge.3.0)&
                     &go to 15
                if (H2.le.4.5) then
                   D2 = D+0.001*H2
                else if (ISPC.eq.15.or. ISPC.eq.16) then
                   D2 = (H2-4.5)*10./(SITEAR(ISPC)-4.5)
                   if (D2.lt.0.1) D2 = 0.1
                end if
                if (DEBUG)write(JOSTND,*)'I,D,D2,H2,SITEAR= ',&
                     &I,D,D2,H2,SITEAR(ISPC)
             else
                if (DEBUG)write(JOSTND,*)'CALLING SMDGF I,ISPC,H2,CR,PCCF= ',&
                     &I,ISPC,H2,CR,PCCF(ITRE(I))
                call SMDGF(ISPC,H2,CR,PCCF(ITRE(I)),D2)
                D2 = D2*XRDGRO
                if (DEBUG)write(JOSTND,*)'AFTER SMDGF D2,XRDGRO,DIAM= ',&
                     &D2,XRDGRO,DIAM(ISPC)
                if (D2.lt.DIAM(ISPC)) D2 = DIAM(ISPC)
                if (DEBUG)write(JOSTND,*)'I,ISPC,CR,TPCCF,H2,D2= ',&
                     &I,ISPC,CR,TPCCF,H2,D2
             end if

             !  STORE UPDATED DIAMETER IN WK5

             WK5(I) = D2
             if (TTVAR.and.H2.lt.4.5) goto 8897

             !  COMPUTE ADJUSTMENT TO STAND BASAL AREA AND CCF FOR NEXT SUBCYCLE.
             !  ASSUME A 1.5 PERCENT ANNUAL MORTALITY RATE.

             if (EMVAR) then
                if (J.lt.NPER .and. D.lt.3.0.and. H2.gt.4.5) then

                   !  COMPUTE ADJUSTMENT TO STAND BASAL AREA AND CCF FOR NEXT SUBCYCLE.
                   !  ASSUME A 1.5 PERCENT ANNUAL MORTALITY RATE.

                   call CCFCAL(ISPC,D1,H1,ICR(I),P,.false.,C1,CW,1)
                   call CCFCAL(ISPC,D2,H2,ICR(I),P,.false.,C2,CW,1)
                   RDNEXT(J+1) = RDNEXT(J+1)+KY*(C2-C1)/10.*0.985**KY
                   BANEXT(J+1) = BANEXT(J+1)+(BACON*(D2*D2-D1*D1))*P*0.985**KY

                   !  END OF TREE LOOP WITHIN SPECIES. PRINT DEBUG IF REQUESTED.

                end if
             else
                call CCFCAL(ISPC,D2,H2,ICR(I),P,.false.,C2,CW,1)
                RDNEXT(J+1) = RDNEXT(J+1)+KY*(C2-C1)/10.*0.985**KY
                BANEXT(J+1) = BANEXT(J+1)+(BACON*D2*D2-B1)*P*0.985**KY
             end if
             8897 continue
             if (DEBUG) write(JOSTND,8997) I,D2,BANEXT(J+1),RDNEXT(J+1)
             8997 format('UPDATE DENSITY: TREE = ',I4,', D2 = ',F8.4,&
                  &', BANEXT(J+1) = ',F8.2,', RDNEXT(J+1) = ',F8.2)

             !  END OF TREE LOOP WITHIN SPECIES. PRINT DEBUG IF REQUESTED.

             14 continue
             if (.not.DEBUG) go to 15
             write(JOSTND,8999) J,KPER(J),ISPC,I,H1,H2,BCCF,RDJ,BBAL,BAL,&
                  &BAJ,PCT(I)
             8999 format('IN REGENT L1: SUBC=',I2,', KPER=',I2,', SP=',I2,&
                  &', I=',I4,', H1=',F8.4,', H2=',F8.4,', BCCF=',F8.4/T4,'RDJ=',&
                  &F8.4,', BBAL=',F8.4,', BAL=',F8.4,', BAJ=',F8.4,', PCT=',F8.4)
             15 continue
          end do

          !  END OF SPECIES LOOP WITHIN SUBCYCLE

          16 continue
       end do
       if (DEBUG)write(16,*)'AFTER 16 NIVAR,TTVAR,UTVAR,CRVAR,EMVAR= ',&
            &NIVAR,TTVAR,UTVAR,CRVAR,EMVAR

       !  END OF SUBCYCLE LOOP.  END OF PERIOD HEIGHT IS STORED IN WK3.

       17 continue
    end do

    !  AFTER LAST SUBCYCLE, COMPUTE ACTUAL HEIGHT INCREMENT FOR FULL CYCLE BY
    !  SUBTRACTION; CONVERT BACK TO LOG SCALE TO ADD IN RANDOM ERROR.

    do ISPC = 1,MAXSP
       I1 = ISCT(ISPC,1)
       if (I1.eq.0) go to 30
       I2 = ISCT(ISPC,2)

       !  ASSIGN VARIANT ACCORDING TO SPECIES

       EMVAR = .false.
       NIVAR = .false.
       TTVAR = .false.
       UTVAR = .false.
       CRVAR = .false.
       if (ISPC .eq.5) then
          NIVAR = .true.
       else if (ISPC .eq.4) then
          TTVAR = .true.
       else if (ISPC.eq.11.or. (ISPC.ge.13.and. ISPC.le.16) .or.&
            &ISPC.eq.19 ) then
          CRVAR = .true.
       else if (ISPC.eq.6.or. ISPC.eq.12.or. ISPC.eq.17) then
          UTVAR = .true.
       else
          EMVAR = .true.
       end if

       XRHGRO = XRHMLT(ISPC)
       XRDGRO = XRDMLT(ISPC)
       XMX = XMAX(ISPC)
       XMN = XMIN(ISPC)
       if (EMVAR) then
          DGMX = FINT*0.2
       else if (TTVAR) then
          DGMX = FINT*DGMAX(ISPC)
       else if (CRVAR) then
          DGMX = DGMAX(ISPC)*FINT/10.
       else
          DGMX = DGMAX(ISPC)
       end if
       if (DEBUG)write(JOSTND,*)'ENTERING 25 LOOP, NIVAR,TTVAR,',&
            &'UTVAR,CRVAR,EMVAR,XMX,XMN,DGMX,LSKIPH= ',&
            &NIVAR,TTVAR,UTVAR,CRVAR,EMVAR,XMX,XMN,DGMX,LSKIPH

       do I3 = I1,I2
          I = IND1(I3)
          D = DBH(I)
          CR = FLOAT(ICR(I))
          if (D.ge.XMX) go to 25
          if (LESTB .and. I.lt.ITRNIN) go to 25
          K = I
          L = 0
          H = HT(I)
          HK = WK3(I)

          D1 = DIAM(ISPC)
          if (NIVAR.and.H.gt.4.5) then
             AX = 0.0658
             BX = 1.3817
             D1 = AX*(H-4.5)**BX+DADJ
          end if
          DK = WK5(I)
          HTGR = HK-H
          if ((EMVAR.or.TTVAR).and.D.lt.D1)D = D1

          !  IF CALLED FROM **ESTAB** AND TOTAL CYCLE LENGTH IS LESS THAN OR EQUAL
          !  TO 5 YEARS, SKIP HEIGHT INCREMENT CALCULATION BUT ASSIGN CR AND DBH.

          if (EMVAR.and.LSKIPH) then
             IPCCF = ITRE(I)
             TPCCF = PCCF(IPCCF)
             if (H.ge.4.5) then
                call SMDGF(ISPC,H,CR,TPCCF,DK)
                if (DK.lt.D) DK = D
                DBH(I) = DK
                DG(I) = DK
                if (DG(I).gt.DGMX) DG(I) = DGMX
             end if
             go to 25
          else if (NIVAR.and.LSKIPH) then
             HTG(K) = 0
             go to 20
          else if (TTVAR .and. LSKIPH) then
             IPCCF = ITRE(I)
             TPCCF = PCCF(IPCCF)
             if (H.ge.4.5) then
                HLESS4 = H-4.5
                DLESS3 = 0.000231*HLESS4*CR - 0.00005*HLESS4*PCCF(ITRE(I))&
                     &+ 0.001711*CR + 0.17023*HLESS4
                DK = (DLESS3+0.3)*XRDGRO
                if (DK.lt.D) DK = D
                DBH(I) = DK
                DG(I) = DK
                if (DG(I).gt.DGMX) DG(I) = DGMX
             end if
             go to 25
          end if
          if (NIVAR) then
             HTGR1 = HK-H
             if (HTGR1.lt.0.0) HTGR1 = 0.0
          end if

          !  DRAW RANDOM INCREMENT GIVEN EXPECTED VALUE, HTGR, AND STD. DEV.,
          !  HSIGMA.  BOUND DRAW TO (-1.5*SD < ZZRAN < 1.0*SD); IF TRIPLING,
          !  EACH TRIPLE GETS A NEW RANDOM NUMBER.  DO NOT TRIPLE IF CALLING
          !  FROM **ESTAB**.  DO NOT APPLY HEIGHT GROWTH MULTIPLIER; MULTIPLIER
          !  WAS USED DURING SUBCYCLING.

          ICRK = ICR(I)
          918 continue
          if (.not.TTVAR) then
             if (EMVAR) then
                HK = WK3(I)
                DK = WK5(I)
                HTGR = HK-H
                if (DEBUG)write(JOSTND,*)'K,HK,DK,HTGR= ',K,HK,DK,HTGR
             else if (NIVAR) then
                18 continue
                ZZRAN = 0.0
                if (DGSD.ge.1.0) ZZRAN = BACHLO(0.0,1.0,RANN)
                if ((ZZRAN .gt.1.0) .or. (ZZRAN .lt. -1.5)) go to 18
                HTGR = HTGR1*EXP(ZZRAN*HSIGMA)
                if (DEBUG)write(JOSTND,*)'NI SECTION K,ZZRAN,HSIGMA,HTGR1,',&
                     &'HTGR= ',K,ZZRAN,HSIGMA,HTGR1,HTGR
             else
                919 continue
                ZZRAN = 0.0
                if (DGSD.ge.1.0) ZZRAN = BACHLO(0.0,1.0,RANN)
                if ((ZZRAN .gt.0.5) .or. (ZZRAN.lt.-2.0)) goto 919
                if (UTVAR)HTGR = (HTGR +ZZRAN*0.1)*XRHGRO
                if (CRVAR)HTGR = (HTGR +ZZRAN*0.2)*XRHGRO
                if (DEBUG)write(JOSTND,*)'CR/UT SECTION K,ZZRAN,XRHGRO,',&
                     &'SCALE,HTGR= ',K,ZZRAN,XRHGRO,SCALE,HTGR

                ! THIS CHECK ADDED BY DIXON 3/4/09 TO PREVENT NEGATIVE HEIGHT GROWTH
                ! MAY NEED A MORE ELEGANT FIX AT SOME POINT; BUT IT WORKS.

                if (HTGR .lt.0.1) then
                   HTGR = 0.1
                   if (DEBUG)write(JOSTND,*)'CR/UT RESETTING HTGR ',&
                        &'TO 0.1 FOOT'
                end if

             end if
          end if

          !  COMPUTE WEIGHTS FOR THE LARGE AND SMALL TREE HEIGHT INCREMENT
          !  ESTIMATES.  IF DBH IS LESS THAN OR EQUAL TO XMN OR IF CALLED
          !  FROM **ESTAB**, THE LARGE TREE PREDICTION IS IGNORED (XWT=0.0).

          XWT = 0.0
          if (D.le.XMN.or.LESTB) go to 19
          XWT = (D-XMN)/(XMX-XMN)
          19 continue
          if (DEBUG)write(JOSTND,*)'WEIGHTING K,HTGR,HTG,XWT= ',&
               &K,HTGR,HTG(K),XWT
          HTG(K) = HTGR*(1.0-XWT) + XWT*HTG(K)
          if (CRVAR.and.HTG(K) .lt..1)HTG(K) = .1

          ! CHECK FOR SIZE CAP COMPLIANCE.

          if ((H+HTG(K)).gt.SIZCAP(ISPC,4)) then
             HTG(K) = SIZCAP(ISPC,4)-H
             if (HTG(K) .lt.0.1) HTG(K) = 0.1
          end if

          20 continue

          !  ASSIGN DBH AND COMPUTE DBH INCREMENT FOR TREES WITH DBH LESS
          !  THAN BREAKPOINT.  COMPUTE 10-YEAR DBH INCREMENT REGARDLESS OF
          !  PROJECTION PERIOD LENGTH.  BYPASS INCREMENT CALCULATION IF
          !  D GE BREAKPOINT.

          if (CRVAR.and.D.ge.1.0)goto 23
          if (UTVAR.and.ISPC.eq.6)go to 1020
          if (D.ge.3.0) go to 23
          1020 continue

          !  COMPUTE ADJUSTMENT FOR BIAS, DENSITY, AND RELATIVE SIZE.

          if (NIVAR) then
             AX = 0.0658
             BX = 1.3817
             if (ABS(AH-4.5).lt.0.01) then
                RELH = 0.0
             else
                RELH = (H1-4.5)/(AH-4.5)
             end if
             if (RELH.gt.1.0) RELH = 1.0
             if (RELH.lt.0.0) RELH = 0.0
             DADJ = DELMAX*RELH*RELH - 2.0*DELMAX*RELH + 0.65
             D1 = DIAM(ISPC)+DADJ
             if (H.gt.4.5) D1 = AX*(H-4.5)**BX+DADJ
             if (DEBUG)write(JOSTND,*)'K,ISPC,AX,BX,DADJ,H,D1= ',&
                  &K,ISPC,AX,BX,DADJ,H,D1
          end if
          HKK = HK
          HK = H+HTG(K)
          if (EMVAR)HK = HKK
          if (.not.TTVAR .and. HK.lt.4.5) then
             if (NIVAR) DBH(K) = 0.1 + DIAM(ISPC)*.01 + (H+HTG(K))*0.001
             if (CRVAR) DBH(K) = D+.001*(H+HTG(K))
             DG(K) = 0.0
          else
             if (EMVAR) then
                call SMDGF(ISPC,HT(K),CR,PCCF(ITRE(I)),DKK)
                BARK = BRATIO(ISPC,D,H)
                DG(K) = (DK-DKK)*BARK
                DDS = DG(K)*(2.0*BARK*D+DG(K))*SCALE
                if (DEBUG)write(JOSTND,*)'K,D,DKK,DK,BARK,DDS,DG(K)= ',&
                     &K,D,DKK,DK,BARK,DDS,DG(K)
                DG(K) = SQRT((D*BARK)**2+DDS)-BARK*D

                !  IF CALLED FROM ESTAB, BOUND DIAMETER INCREMENT FOR ADVANCED
                !  REGENERATION.

                if (LESTB) then
                   DBH(K) = DK
                   if (DG(K).gt.DGMX) DG(K) = DGMX
                end if
                if (DEBUG)write(JOSTND,*)'K,DBH,DG,DIAM= ',&
                     &K,DBH(K),DG(K),DIAM(ISPC)
                if ((DBH(K)+DG(K)).lt.DIAM(ISPC) .and. HK.ge.4.5) then
                   DG(K) = DIAM(ISPC)-DBH(K)
                end if
                goto 22
             else if (NIVAR) then
                DK = AX*(HK-4.5)**BX+DADJ
                if (DEBUG)write(JOSTND,*)'K,ISPC,AX,BX,DADJ,HK,DK= ',&
                     &K,ISPC,AX,BX,DADJ,HK,DK
                if (DK.lt.DIAM(ISPC)) DK = DIAM(ISPC)
                DK = DK+HK*0.001
             else if (CRVAR .or. UTVAR) then
                if (ISPC.eq.6) then
                   DK = (HK-4.5)*10./(SITEAR(ISPC)-4.5)
                   if (DK .lt.0.1) DK = 0.1
                   DKK = (H-4.5)*10./(SITEAR(ISPC)-4.5)
                   if (DKK .lt.0.1) DKK = 0.1
                   if (H .lt.4.5) DKK = D
                else
                   BX = HT2(ISPC)
                   if (IABFLG(ISPC).eq.1) then
                      AX = HT1(ISPC)
                   else
                      AX = AA(ISPC)
                   end if
                   DK = (BX/(ALOG(HK-4.5)-AX))-1.0
                   if (DK .lt.0.1) DK = 0.1
                   if (H .le.4.5) then
                      DKK = D
                   else
                      DKK = (BX/(ALOG(H-4.5)-AX))-1.0
                   end if
                end if
                if (DEBUG)write(JOSTND,9986) AX,BX,ISPC,HK,BARK,&
                     &XRDGRO,DK,DKK
                9986 format('IN REGENT 9986 FORMAT AX,BX,ISPC,HK',&
                     &' BARK,XRDGRO,DK,DKK= '/T12, F10.3,2X,F10.3,2X,I5,2X,5F10.3)
             else if (TTVAR) then

                !  ASSIGN DIAMETER INCREMENT AND SCALE FOR BARK THICKNESS AND
                !  PERIOD LENGTH.  SCALE ADJUSTMENT IS ON GROWTH IN DDS RATHER
                !  THAN INCHES OF DG TO MAINTAIN CONSISTENCY WITH GRADD.

                if (HK .ge.4.5) then
                   HLESS4 = HT(K)-4.5
                   DLESS3 = 0.000231*HLESS4*CR - 0.00005*HLESS4*PCCF(ITRE(I))&
                        &+ 0.001711*CR + 0.17023*HLESS4
                   DKK = (DLESS3+0.3)
                   if (DKK.lt.DIAM(ISPC))DKK = DIAM(ISPC)
                   BARK = BRATIO(ISPC,D,H)
                   DG(K) = (DK-DKK)*BARK
                   DDS = DG(K)*(2.0*BARK*D+DG(K))
                   if (DEBUG)write(JOSTND,*)'K,D,DKK,DK,BARK,DDS,DG(K)= ',&
                        &K,D,DKK,DK,BARK,DDS,DG(K)
                   DG(K) = SQRT((D*BARK)**2+DDS)-BARK*D
                else
                   DG(K) = 0.0
                end if
             end if

             !       IF CALLING FROM **ESTAB** ASSIGN DIAMETER AND DIAMETER
             !       INCREMENT.

             if (LESTB) then
                DBH(K) = DK
                if (TTVAR.and.DG(K).gt.DGMX)DG(K) = DGMX
                if (NIVAR.or.CRVAR)DG(K) = DK
                if (CRVAR .or. UTVAR) then
                   if (DBH(K).lt.DIAM(ISPC)) DBH(K) = DIAM(ISPC)
                   DBH(K) = DBH(K)+0.001*HK
                   DG(K) = DBH(K)
                end if
             else

                !         DIAMETER INCREMENT IS THE DIFFERENCE BETWEEN COMPUTED
                !         DIAMETERS.  MULTIPLIER IS APPLIED TO DIAMETER INCREMENT
                !         AT THIS POINT.

                DGK = 0.0
                if (.not.TTVAR) then
                   if (NIVAR) then
                      DGK = (DK-D1)*XRDGRO
                   else if (CRVAR.or.UTVAR) then
                      if (DK.lt.0.0.or. DKK.lt.0.0) then
                         DGK = HTG(K)*0.2*BARK*XRDGRO
                         DK = D+DGK
                      else
                         DGK = (DK-DKK)*BARK*XRDGRO
                      end if
                      if (DGK.gt.DGMX)DGK = DGMX
                   end if
                end if
                if (DGK.lt.0.0) DGK = 0.0

                !         SCALE DIAMETER INCREMENT FOR BARK AND PERIOD LENGTH.
                !         IN ORDER TO MAINTAIN CONSISTENCY WITH **GRADD**,
                !         ADJUSTMENTS ARE MADE ON THE DDS SCALE.

                BARK = BRATIO(ISPC,DBH(K),HT(K))
                if (NIVAR) then
                   DG(K) = DGK*BARK
                else if (CRVAR.or.UTVAR) then
                   DG(K) = DGK
                end if
                DDS = DG(K)*(2.0*BARK*D+DG(K))*SCALE
                DG(K) = SQRT((D*BARK)**2+DDS)-BARK*D
             end if
             if ((DBH(K)+DG(K)).lt.DIAM(ISPC)) then
                DG(K) = DIAM(ISPC)-DBH(K)
             end if
          end if

          !  CHECK FOR TREE SIZE CAP COMPLIANCE

          22 continue
          call DGBND(ISPC,DBH(K),DG(K))

          !  IF THE TREE WILL ATTAIN A DBH OF 3 OR MORE INCHES THIS CYCLE
          !  CALL **DUBSCR** TO ESTIMATE A NEW CROWN RATIO, IF IT DOESN'T HAVE ONE
          !  DIAMETER GROWTH SCALED TO CYCLE LENGTH FOR THIS CALCULATION.
          !  MAKE SURE NEW CROWN DOES NOT EXCEED MAX POSSIBLE GIVEN HTG.

          if (EMVAR)go to 23
          if (.not.LESTB) then
             SCALE2 = FINT/YR
             D = DBH(K)
             BARK = BRATIO(ISPC,D,HT(K))
             DDS2 = (DG(K)*(2.0*BARK*D+DG(K)))*SCALE2
             DG2 = SQRT((D*BARK)**2+DDS2)-BARK*D
             if (DG2.lt.0.0) DG2 = 0.0
             DNEW = D+DG2
             IITRE = ITRE(I)
             TPCCF = PCCF(IITRE)
             if (DNEW.ge.3.0) then
                if (ICRK .eq.0) then
                   call DUBSCR(ISPC,DNEW,HK,BA,CR,TPCCF,AVH,RMAI)
                   ICRK = IFIX(CR*100. + 0.5)
                end if
                ICR(K) = ICRK
                if (DEBUG)write(JOSTND,*)'AFTER DUBSCR K,ISPC,DNEW,HK,BA,CR,',&
                     &'ICR= ',K,ISPC,DNEW,HK,BA,CR,ICR(K)
             end if
          end if
          23 continue

          !  PRINT DEBUG AND RETURN TO PROCESS NEXT TRIPLE OR NEXT TREE.

          if (.not.DEBUG) go to 24
          write(JOSTND,9000) I,K,ISPC,H,HTG(K),HK,DBH(K),DG(K)
          9000 format('IN REGENT LOOP 2, I=',I4,', K=',I4,', ISPC=',I3,&
               &', CUR HT=',F7.2,', HT INC=',F7.4/T19,'NEW HT=',F7.2,&
               &', CUR DBH=',F7.2,', 10-YR DBH INC=',F7.4)
          24 continue
          if (LESTB .or..not.LTRIP .or. L.ge.2) go to 25
          if (EMVAR .or. TTVAR) then
             K = ITRN+2*I-1
             DBH(K) = DBH(I)
             DG(K) = DG(I)
             HT(K) = HT(I)
             HTG(K) = HTG(I)
             ICR(K) = ICR(I)
             K = K+1
             DBH(K) = DBH(I)
             DG(K) = DG(I)
             HT(K) = HT(I)
             HTG(K) = HTG(I)
             ICR(K) = ICR(I)
             go to 25
          else
             L = L+1
             K = ITRN+2*I-2+L
             go to 918
          end if

          !  END OF GROWTH PREDICTION LOOP.  PRINT DEBUG INFO IF DESIRED.

          25 continue
       end do
       30 continue
    end do
    return

    !  SMALL TREE HEIGHT CALIBRATION SECTION.

    40 continue
    do ISPC = 1,MAXSP
       HCOR(ISPC) = 0.0
       CORTEM(ISPC) = 1.0
       NUMCAL(ISPC) = 0
       45 continue
    end do
    if (ITRN.le.0) return
    if (IFINTH .eq.0)  goto 100
    SCALE3 = REGYR/FINTH

    !  DETERMINE BASAL AREA IN LARGE TREES AT START OF EACH PREDICTION SUBCYCLE.

    do J = 1,NPER
       BANEXT(J) = TEMBA
       RDNEXT(J) = TEMCCF
       46 continue
    end do
    if (NPER.eq.1) go to 50

    !  IF THERE IS MORE THAN 1 SUBCYCLE FOR HEIGHT INCREMENT PREDICTION,
    !  (IFINTH > 5) ESTIMATE VALUES OF BA AND RELATIVE DENSITY (CCF) AT
    !  START OF EACH SUBCYCLE.  10-YEAR CHANGE IS COMPUTED FROM BACKDATED
    !  DIAMETER (WK3) AND 10-YEAR DIAMETER INCREMENT COMPUTED DURING CALIBRATION.
    !  LINEAR INTERPOLATION IS USED TO ASSIGN VALUES FOR SUBCYCLES.

    do I = 1,ITRN
       D1 = WK3(I)
       IS = ISP(I)
       P = PROB(I)
       D2 = WK3(I)+DG(I)/BRATIO(IS,D1,HT(I))
       B1 = BACON*D1*D1
       B2 = BACON*D2*D2
       call CCFCAL(IS,D1,HT(I),ICR(I),P,.false.,C1,CW,1)
       call CCFCAL(IS,D2,HT(I),ICR(I),P,.false.,C2,CW,1)
       BI = (B2-B1)/10.0
       CI = (C2-C1)/10.0
       K = 0
       do J = 2,NPER
          K = K+KPER(J-1)
          PN = P*0.985**K
          RDNEXT(J) = RDNEXT(J)+K*CI/P*PN
          BANEXT(J) = BANEXT(J)+K*BI*PN
          48 continue
       end do
       49 continue
    end do
    50 continue

    ! COMPUTE DENSITY MODIFIER FROM CCF AND TOP HEIGHT FOR CR & UT VARIANT

    X = AVH*(RELDEN/100.0)
    if (X .gt.300.0) X = 300.0
    PCTRED = 1.11436 + X*(-.011493 + X*(.43012E-4 + X*(-.72221E-7&
         &+ X*(.5607E-10- X*0.1641E-13))))
    if (PCTRED .gt.1.0) PCTRED = 1.0
    if (PCTRED .lt.0.01) PCTRED = 0.01
    if (DEBUG) write(JOSTND,9989) AVH,RELDEN,X,PCTRED
    9989 format('IN REGENT AVH,RELDEN,X,PCTRED = ',4F10.4)

    !  BEGIN PROCESSING TREE LIST IN SPECIES ORDER.  DO NOT CALCULATE
    !  CORRECTION TERMS IF THERE ARE NO TREES FOR THIS SPECIES.

    do ISPC = 1,MAXSP

       !  ASSIGN VARIANT ACCORDING TO SPECIES

       EMVAR = .false.
       NIVAR = .false.
       TTVAR = .false.
       UTVAR = .false.
       CRVAR = .false.
       if (ISPC .eq.5) then
          NIVAR = .true.
       else if (ISPC .eq.4) then
          TTVAR = .true.
       else if (ISPC.eq.11.or. (ISPC.ge.13.and. ISPC.le.16) .or.&
            &ISPC.eq.19 ) then
          CRVAR = .true.
       else if (ISPC.eq.6.or. ISPC.eq.12.or. ISPC.eq.17) then
          UTVAR = .true.
       else
          EMVAR = .true.
       end if

       CORNEW = 1.0
       I1 = ISCT(ISPC,1)
       if (I1.eq.0.or..not. LHTCAL(ISPC)) go to 90
       I2 = ISCT(ISPC,2)
       IREFI = IREF(ISPC)
       N = 0
       SNP = 0.0
       SNX = 0.0
       SNY = 0.0
       BHAB = RHCON(ISPC)

       !  VALUES FOR THE NEXT 3 VARIABLES ARE FROM SUBALPINE FIR IN NI/IE
       !  USED FOR SUBALPINE LARCH (LL) IN THIS VARIANT

       BLH = 0.3740
       BCCF = -.00391
       BBAL = -0.22957

       if (CRVAR.or.UTVAR) then
          SI = SITEAR(ISPC)
          if (SI .gt. SHI(ISPC)) SI = SHI(ISPC)
          if (SI .le. SLO(ISPC)) SI = SLO(ISPC) + 0.5
          RELSI = (SI-SLO(ISPC))/(SHI(ISPC)-SLO(ISPC))
          RSIMOD = 0.5 * (1.0 + RELSI)
          X = RELSI*100.0
       end if

       !  BEGIN TREE LOOP WITHIN SPECIES.  IF MEASURED HEIGHT INCREMENT IS
       !  LESS THAN OR EQUAL TO ZERO, OR DBH IS GREATER THAN 5.0, THE RECORD
       !  WILL BE EXCLUDED FROM THE CALIBRATION.

       do I3 = I1,I2
          I = IND1(I3)
          H = HT(I)
          if (IHTG.lt.2) H = H-HTG(I)
          if (DBH(I).ge.5.0.or.H.lt.0.01) go to 60
          if (HTG(I).lt.0.001) go to 60
          HK = H
          D = DBH(I)
          CR = FLOAT(ICR(I))
          do J = 1,NPER
             BAJ = BANEXT(J)
             RDJ = RDNEXT(J)
             BAL = BAJ*(100.0-PCT(I))*0.0001

             !  FOR CR AND UT VARIANT SPECIES
             !  COMPUTE POTENTIAL GROWTH FROM SITE INDEX AND SPECIES.

             if (CRVAR.or.UTVAR) then
                if (CRVAR) then
                   POTHTG = SI/(15.0 - 4.0*RELSI) * 1.0
                else if (ISPC.eq.15.or. ISPC.eq.16) then
                   POTHTG = (SI/10.0)*(SI*1.5-H)/(SI*1.5)
                else if (ISPC.eq.6) then
                   POTHTG = ((SI/5.0)*(SI*1.5-H)/(SI*1.5))* 0.83
                else
                   POTHTG = 0.
                end if
                if (DEBUG)write(JOSTND,9988)SI,RELSI,X,ISPC,J,POTHTG
                9988 format('IN REGENT SI,RELSI,X,ISPC,J,POTHTG = ',&
                     &F7.2,F10.3,F10.4,2I6,F10.3)
             end if
             if (EMVAR) then
                if (TEMCCF .le.0.) then
                   PPCCF = 0.
                else
                   PPCCF = (RDJ-TEMCCF)/TEMCCF
                end if
             else
                PPCCF = (RDJ-RELDM1)/RELDM1
             end if
             TPCCF = PCCF(ITRE(I))*PPCCF
             if (TPCCF.gt.300.0) TPCCF = 300.0
             if (TPCCF.lt.25.0) TPCCF = 25.0
             if (EMVAR) then
                call SMHTGF(ISPC,I,EDH,CR,TPCCF)
                HK = HK+EDH
                call SMDGF(ISPC,HK,CR,PCCF(ITRE(I)),D2)
                if (D2.lt.DIAM(ISPC)) D2 = DIAM(ISPC)
             else if (NIVAR) then
                EDH = EXP(BHAB + BLH*ALOG(HK) + BCCF*RDJ + BBAL*BAL)
                HK = HK+EDH

             else if (TTVAR) then

                !  RESET ZRAND IF THIS IS THE START OF THE PROJECTION, THIS IS THE FIRST
                !  PREDICTION FOR A NEW TRIPLE, OR IF GROWTH IN THE PREVIOUS CYCLE WAS
                !  LESS THAN 0.1 FT.

                if (ZRAND(I).ne.-999.) go to 921
                911 continue
                ZRAND(I) = BACHLO(0.0,1.0,RANN)
                if (ZRAND(I).lt.-2.0.or. ZRAND(I).gt.2.0) go to 911
                921 continue
                D = DBH(I)
                if (D.le.0.0) then
                   EDH = 0.0
                   go to 931
                end if
                BETA1 = EXP(1.17527 - 0.42124*ALOG(TPCCF))
                BETA2 = EXP(-2.56002 - 0.58642*ALOG(TPCCF))
                HTG1 = BETA1 + BETA2*CR
                STDDEV = HTG1*(1.08720 - 0.00230*CR)
                EDH = HTG1 + ZRAND(I)*STDDEV

                !  IF ESTIMATED HEIGHT INCREMENT IS CLOSE TO ZERO, SET FLAG TO DRAW
                !  A NEW RANDOM ERROR ON NEXT PASS THROUGH.

                if (EDH .le.0.1) then
                   EDH = 0.1
                   ZRAND(I) = -999.
                end if
                931 continue

                HK = HK+EDH
                HLESS4 = HK-4.5
                DLESS3 = 0.000231*HLESS4*CR - 0.00005*HLESS4*PCCF(ITRE(I))&
                     &+ 0.001711*CR + 0.17023*HLESS4
                D2 = (DLESS3+0.3)
                if (D2.lt.DIAM(ISPC)) D2 = DIAM(ISPC)

             else if (CRVAR .or. UTVAR) then
                X = CR/100.
                VIGOR = (150.0 * (X**3.0)*EXP(-6.0*X))+0.3
                if (VIGOR .gt.1.0)VIGOR = 1.0

                !  VIGOR ADJUSTMENT TO DRASTIC FOR JUNIPER,
                !  CUT IT BY TWO-THIRDS

                if (ISPC.eq.6)VIGOR = 1.-((1.-VIGOR)/3.)

                ! COMPUTE HT GROWTH AND AGE FOR ASPEN. EQN FROM WAYNE SHEPPARD RMS.
                ! ALSO USED FOR PAPER BIRCH

                if (ISPC.eq.12.or. ISPC.eq.17) then
                   AG1 = (H*12.0*2.54/26.9825)**0.8509
                   AG2 = AG1+10.0
                   H2 = (26.9825*AG2**1.1752)/(2.54*12.0)
                   EDH = (H2-H) * RSIMOD * RHCON(ISPC)*0.75
                else
                   EDH = POTHTG*PCTRED*VIGOR*RHCON(ISPC)
                end if

                ! CR AND UT MODELS ARE 10-YEAR MODELS, ADJUST TO 5-YEAR BASIS

                EDH = EDH*0.5

                if (DEBUG)write(JOSTND,9990) X,VIGOR,EDH
                9990 format('IN REGENT X,VIGOR,EDH = ',3F10.4)
             end if
             55 continue
          end do
          if (EMVAR) then
             EDH = (HK-H)*RHCON(ISPC)
          else if (NIVAR) then
             EDH = HK-H
          else if (TTVAR) then
             EDH = (HK-H)*RHCON(ISPC)
          end if
          P = PROB(I)
          TERM = HTG(I)*SCALE3
          SNP = SNP+P
          SNX = SNX+EDH*P
          SNY = SNY+TERM*P
          N = N+1

          !  PRINT DEBUG INFO IF DESIRED.

          if (DEBUG) write(JOSTND,9001) NPLT,I,ISPC,H,DBH(I),ICR(I),&
               &PCT(I),TEMCCF,RHCON(ISPC),EDH,TERM
          9001 format('NPLT=',A26,',  I=',I5,',  ISPC=',I3,',  H=',F6.1,&
               &',  DBH=',F5.1,',  ICR',I5,',  PCT=',F6.1,',  RELDEN=',&
               &F6.1 / 12X,'RHCON=',F10.3,',  EDH=',F10.3,', TERM=',F10.3)

          !  END OF TREE LOOP WITHIN SPECIES.

          60 continue
       end do
       if (DEBUG) write(JOSTND,9010) ISPC,SNP,SNX,SNY
       9010 format(/'SUMS FOR SPECIES ',I2,':  SNP=',F10.2,&
            &';   SNX=',F10.2,';   SNY=',F10.2)

       !  COMPUTE CALIBRATION TERMS.  CALIBRATION TERMS ARE NOT COMPUTED
       !  IF THERE WERE FEWER THAN NCALHT (DEFAULT=5) HEIGHT INCREMENT
       !  OBSERVATIONS FOR A SPECIES.

       if (N.lt.NCALHT) go to 80

       !  CALCULATE MEANS FOR THE FOR THE GROWTH SAMPLE ON THE
       !  NATURAL SCALE.

       call REGHCAL(ISPC,SNP,SNX,SNY,CORNEW)
       80 continue
       CORTEM(IREFI) = CORNEW
       NUMCAL(IREFI) = N
       90 continue
    end do

    !  END OF CALIBRATION LOOP.  PRINT CALIBRATION STATISTICS AND RETURN

    write(JOSTND,9002) (NUMCAL(I),I = 1,NUMSP)
    9002 format(/'NUMBER OF RECORDS AVAILABLE FOR SCALING'/&
         &'THE SMALL TREE HEIGHT INCREMENT MODEL',&
         &((T48,11(I4,2X)/)))
    100 continue
    write(JOSTND,9003) (CORTEM(I),I = 1,NUMSP)
    9003 format(/'INITIAL SCALE FACTORS FOR THE SMALL TREE'/&
         &'HEIGHT INCREMENT MODEL',&
         &((T48,11(F5.2,1X)/)))

    ! OUTPUT CALIBRATION TO DATABASE.

    call REGCALOUT(CORTEM,NUMCAL,KOUT,K,KK,ISPEC,SPEC)
    return
    9998 continue

    !  ENTRY POINT FOR LOADING OF REGENERATION GROWTH MODEL
    !  CONSTANTS  THAT REQUIRE ONE-TIME RESOLUTION.
    !  IDTYPE IS A HABITAT INDEX THAT IS COMPUTED IN **RCON**.
    !  RHGL IS AN ARRAY FOR LOCATION CONSTANTS,
    !  RHHAB IS AN ARRAY FOR HABITAT DEPENDENT CONSTANTS, AND
    !  RSAB0, RSAB1, AND RSAB2 ARE COEFFICIENTS FOR
    !  THE SLOPE AND ASPECT TRANSFORMATION.

    data RHHAB/  -0.2146, -0.0941, -0.4916, -0.3582, 0.0/
    data MAPHAB/12*4, 3,2, 4*5, 1, 7*4, 1,3,3,4/
    data  RHGL/  -.2785,-.0480,0./
    data RSAB0,RSAB1,RSAB2/ -0.10987, 0.22157, -0.12432/
    !*    WRITE(JOSTND,777)  IGL,ITYPE,ASPECT,SLOPE
    !*777 FORMAT('IN REGCON IGL ITYPE ASPECT SLOPE=',2I5,2F8.2)

    !  COMBINE SITE EFFECTS THAT ARE INDEPENDENT OF SPECIES.

    REGCH = RHGL(IGL) +&
         &(RSAB0 + RSAB1*COS(ASPECT) + RSAB2*SIN(ASPECT))*SLOPE

    !  LOAD OVERALL SITE CONSTANTS INTO RHCON BY SPECIES.  IF SCALE FACTORS
    !  HAVE BEEN INPUT WITH THE READCORR KEYWORD, INCORPORATE THEM INTO
    !  THE OVERALL CONSTANTS.

    do ISPC = 1,MAXSP

       !  ASSIGN VARIANT ACCORDING TO SPECIES

       EMVAR = .false.
       NIVAR = .false.
       TTVAR = .false.
       UTVAR = .false.
       CRVAR = .false.
       if (ISPC .eq.5) then
          NIVAR = .true.
       else if (ISPC .eq.4) then
          TTVAR = .true.
       else if (ISPC.eq.11.or. (ISPC.ge.13.and. ISPC.le.16) .or.&
            &ISPC.eq.19 ) then
          CRVAR = .true.
       else if (ISPC.eq.6.or. ISPC.eq.12.or. ISPC.eq.17) then
          UTVAR = .true.
       else
          EMVAR = .true.
       end if

       if (NIVAR) then
          IRHHAB = MAPHAB(ITYPE)

          !*      WRITE(JOSTND,778) ISPC,IRHHAB
          !*778   FORMAT('REGCON ISPC IRHHAB=',2I6)

          RHCON(ISPC) = REGCH + 1.0667 + RHHAB(IRHHAB)
          if (LRCOR2.and.RCOR2(ISPC).gt.0.0) RHCON(ISPC) = RHCON(ISPC) +&
               &ALOG(RCOR2(ISPC))
       else
          RHCON(ISPC) = 1.0
          if (LRCOR2.and. RCOR2(ISPC).gt.0.0)&
               &RHCON(ISPC) = RCOR2(ISPC)
       end if
       110 continue
    end do
    return
  end subroutine REGEM

  !===============================!
  !   Compute small tree growth   !
  !===============================!
  ! IE $Id$
  subroutine REGIE(LCON)
    implicit none

    logical :: LCON
    ! Regional callees retain their native argument lists.
    interface
       subroutine DUBSCR(ISPC,D,H,BA,CR,TPCCF,AVH,TMAI)
         integer :: ISPC
         real :: D
         real :: H
         real :: BA
         real :: CR
         real :: TPCCF
         real :: AVH
         real :: TMAI
       end subroutine DUBSCR
    end interface


    !  **REGENT** COMPUTES HEIGHT AND DIAMETER INCREMENTS FOR SMALL
    !  TREES.  THE HEIGHT INCREMENT MODEL IS APPLIED TO TREES THAT ARE LESS
    !  THAN 10 INCHES DBH (5 INCHES FOR LODGEPOLE PINE), AND THE DBH
    !  INCREMENT MODEL IS APPLIED TO TREES THAT ARE LESS THAN 3 INCHES DBH.
    !  FOR TREES THAT ARE GREATER THAN 2 INCHES DBH (1 INCH FOR LODGEPOLE
    !  PINE), HEIGHT INCREMENT PREDICTIONS ARE AVERAGED WITH THE PREDICTIONS
    !  FROM THE LARGE TREE MODEL. HEIGHT INCREMENT IS A FUNCTION OF HABITAT
    !  TYPE, SLOPE, ASPECT, TREE HEIGHT, BASAL AREA IN LARGER TREES, AND
    !  STAND CROWN COMPETITION FACTOR.  DIAMETER IS ASSIGNED FROM A HEIGHT
    !  DIAMETER FUNCTION WITH ADJUSTMENTS FOR RELATIVE SIZE AND STAND
    !  DENSITY.  INCREMENT IS COMPUTED BY SUBTRACTION. WHEN THE TREE ATTAINS
    !  A DBH OF 3.0 INCHES IN A CYCLE, **DUBSCR** IS CALLED TO ASSIGN A CROWN
    !  RATIO.   **REGENT** IS CALLED FROM **CRATET** DURING CALIBRATION AND
    !  FROM **GRINCR** DURING CYCLING.  ENTRY **REGCON** IS CALLED FROM
    !  **RCON** TO LOAD MODEL PARAMETERS THAT NEED ONLY BE RESOLVED ONCE.

    !  **REGENT** IS ALSO USED TO PREDICT HEIGHT INCREMENT, DBH AND CROWN
    !  RATIO FOR TREES THAT ARE CREATED BY THE REGENERATION ESTABLISHMENT
    !  MODEL.  5-YEAR OLD TREES ARE GENERATED BY **ESTAB** AND **REGENT**
    !  COMPUTES INCREMENTS FROM AGE 5 TO THE END OF THE CYCLE.

    ! Common blocks

    !  DIMENSIONS FOR INTERNAL VARIABLES:

    !   BANEXT -- ESTIMATED STAND BASAL AREA AT START OF EACH HEIGHT INCREMENT
    !             PREDICTION SUBCYCLE.
    !   CORTEM -- A TEMPORARY ARRAY FOR PRINTING CORRECTION TERMS.
    !     KPER -- NUMBER OF YEARS (MAX OF 5) IN EACH HEIGHT INCREMENT PREDICTION
    !             SUBCYCLE.
    !   NUMCAL -- A TEMPORARY ARRAY FOR PRINTING NUMBER OF HEIGHT
    !             INCREMENT OBSERVATIONS BY SPECIES.
    !   RDNEXT -- RELATIVE DENSITY (CCF) AT START OF EACH HEIGHT INCREMENT
    !             PREDICTION SUBCYCLE.
    !    RHCON -- CONSTANT FOR THE HEIGHT INCREMENT MODEL.  INCLUDES EFFECTS
    !             OF HABITAT TYPE, LOCATION, SLOPE, AND ASPECT AS LOADED IN
    !             RCON.  HABITAT EFFECTS ARE BORROWED FROM LARGE TREE DBH
    !             INCREMENT MODEL FOR ALL BUT TYPES 520, 530, AND 570.
    !             ADJUSTMENTS ARE THE DIFFERENCE BETWEEN THE HABITAT
    !             CONSTANT FOR A GIVEN TYPE AND THE CONSTANT FOR TYPE 520.
    !    RHCCF -- THE COEFFICIENTS FOR EACH SPECIES FOR THE CROWN
    !             COMPETITION FACTOR TERM IN THE HEIGHT INCREMENT MODEL.
    !     RHLH -- THE COEFFICIENTS FOR EACH SPECIES FOR THE LN(HEIGHT)
    !             TERM IN THE HEIGHT INCREMENT MODEL.
    !    RHBAL -- THE COEFFICIENTS FOR EACH SPECIES FOR THE BAL TERM
    !             IN THE HEIGHT INCREMENT MODEL.   COEFFICIENTS WERE
    !             TAKEN FROM THE LARGE TREE HEIGHT INCREMENT MODEL AS OF
    !             OCTOBER, 1983.
    !     HHT1 -- SPECIES DEPENDENT CONSTANT IN THE HEIGHT-DBH MODEL USED
    !             TO ASSIGN DIAMETERS TO SMALL TREES.
    !     HHT2 -- SPECIES DEPENDENT EXPONENT IN THE HEIGHT-DBH MODEL USED
    !             TO ASSIGN DIAMETERS TO SMALL TREES.
    !     XMAX -- UPPER END OF THE RANGE OF DIAMETERS OVER WHICH HEIGHT
    !             INCREMENT PREDICTIONS FROM SMALL AND LARGE TREE MODELS
    !             ARE AVERAGED.
    !     XMIN -- LOWER END OF THE RANGE OF DIAMETERS OVER WHICH HEIGHT
    !             INCREMENT PREDICTIONS FROM THE SMALL AND LARGE TREE
    !             ARE AVERAGED.

    !  SPECIES EXPANSION
    !  WB USE COEFFICIENTS FOR L FROM NI
    !  LM AND PY USE COEFFICIENTS AND LOGIC FROM TT FOR LM
    !  LL USE COEFFICIENTS FOR AF FROM NI
    !  AS, MM, PB USE COEFFIECIENTS AND LOGIC FOR AS FROM UT
    !  CO, OH USE COEFFIECIENTS AND LOGIC FOR OH FROM CR
    !  OS USE COEFFEICIENTS FOR MH FROM NI
    !  PI, JU USE COEFFICIENTS AND LOGIC FROM UT

    !  NOTE: UT AND CR LOGIC WAS MODIFIED FROM 10-YEAR REGYR TO 5-YEAR

    external RANN
    ! Variables
    logical :: DEBUG,LSKIPH,CRVAR,NIVAR,UTVAR,TTVAR
    real    :: CORTEM(23)
    integer :: NUMCAL(23)
    real    :: RHCCF(23),RHLH(23),XMAX(23),XMIN(23),HHT1(23),HHT2(23),RHBAL(23),&
         &RHHAB(6,23)
    integer :: MAPHAB(30,23)
    real    :: RHGL(3),DIAM(23),RHSC(23),BANEXT(10),RDNEXT(10)
    integer :: KPER(10)
    real    :: DGMAX(23),SLO(23),SHI(23)
    real    :: RELH,DADJ,HTGRL,BETA1,BETA2,HTG1,STDDEV,VIGOR,HITE1,AG2
    real    :: HITE2,H2,DGJ,HLESS4,DLESS3,DGMX,HK,DK,HTGR,HTGR1
    real    :: ZZRAN,DKK,DDS,DGK,SCALE2,DDS2,DG2,DNEW,SCALE3,X,PCTRED,BAJ
    real    :: RDJ,PPCCF,XRHGRO,XRDGRO,CON,BH,BCCF,BBAL,XMX,XMN,AX,BX,D
    real    :: H1,H,BAL,BARK,SI,RSIMOD,POTHTG,TPCCF,XWT,CR,RAN,R
    real    :: AH,DELMAX,CORNEW,SNP,SNX,SNY,BHAB,BLH,EDH,AG1,TERM
    real    :: RSAB2,RSAB1,RSAB0,BAYR,CCFYR,BACHLO,D1,P,D2,BRATIO,B1,B2
    real    :: CW,C1,C2,BI,CI,PN,BACON,HSIGMA,REGYR,TEMBA,TEMCCF,TEMAHT
    real    :: SCALE,RELSI,SJ,SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,D1F,D2F
    integer :: I,NTYR,IYR,NPER,ITOT,N,J,IS,K,NYR,IPCCF,KY
    integer :: ISPC,I1,I2,I3,L,ICRK,IITRE,IREFI,KOUT,ISPEC,KK
    character(len=2) :: SPEC

    ! Data statements
    data DIAM/&
         &0.4, 3*0.3, 2*0.2, 0.4, 2*0.3, 0.5, 0.2,&
         &0.3, 0.3, 0.3, 0.4, 0.3, 0.3, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2/
    data HHT1/&
         &0.0781, 0.0751, 0.0828, 0.1155, 0.0729, 0.0730,&
         &0.0988, 0.0658, 0.0658, 0.2160, 0.0729,&
         &0.0751,    0.0, 0.0658,    0.0,    0.0,    0.0,    0.0,&
         &0.0,    0.0,    0.0,    0.0, 0.0729/
    data HHT2/&
         &1.1645, 1.1176, 1.1713, 1.0688, 1.1988, 1.2343,&
         &1.0807, 1.3817, 1.3817, 1.0049, 1.1988,&
         &1.1176,    0.0, 1.3817,    0.0,    0.0,    0.0,    0.0,&
         &0.0,    0.0,    0.0,    0.0, 1.1988/
    data RHBAL/&
         &-0.37199,-0.41532,-0.40043,-0.25355,-0.34693,-0.12013,&
         &-0.24172,-0.25300,-0.22957,-0.47299,-0.25349,&
         &-0.41532,     0.0,-0.22957,     0.0,     0.0,     0.0,&
         &0.0,     0.0,     0.0,     0.0,     0.0,-0.25349/
    data  RHLH/&
         &0.4214, 0.2716, 0.3907, 0.3487, 0.3417, 0.2354, 0.5843,&
         &0.2827, 0.3740, 0.4485, 0.2354,&
         &0.2716,    0.0, 0.3740,    0.0,    0.0,    0.0,    0.0,&
         &0.0,    0.0,    0.0,    0.0, 0.2354/
    data  RHCCF/&
         &-.00591,-.00654,-.00591,-.00391,-.00391,-.00391,-.00654,&
         &-.00391,-.00391,-.00654,-.00391,&
         &-.00654,     .0,-.00391,     .0,     .0,     .0,     .0,&
         &.0,     .0,     .0,     .0,-.00391/
    data XMAX/6*10.0,5.0,4*10.0, 10., 3., 10.,99.,99., 3., 4.,&
         &2., 4., 4., 2., 10./
    data XMIN/6*2.0,1.0,4*2.0,    2.,1.5,  2.,90.,90.,1.5, 2.,&
         &.5, 2., 2., .5,  2./
    data REGYR/5.0/,HSIGMA/0.59/,BACON/0.005454154/

    data DGMAX/12*99.0,2.0,99.0,3*2.0,5*2.5,99.0/
    data SLO/14*0.0,2*5.0,0.0,4*30.0,20.,0.0/
    data SHI/14*0.0,20.0,15.0,0.0,70.0,120.0,2*70.0,100.0,0.0/

    if (LCON) go to 9998

    !  CHECK FOR DEBUG.

    call DBCHK (DEBUG,'REGENT',6,ICYC)
    if (DEBUG) write(JOSTND,9980)ICYC
    9980 format('ENTERING SUBROUTINE REGENT  CYCLE =',I5)

    !  THE SMALL TREE HEIGHT INCREMENT MODEL IS BASED ON 5-YEAR INCREMENT DATA.
    !  TO AVOID BIAS, PREDICTIONS ARE MADE FOR SUBCYCLES THAT ARE LESS THAN OR
    !  EQUAL TO 5 YEARS IN LENGTH.  THE FOLLOWING CODE DETERMINES THE NUMBER
    !  AND LENGTH OF SUBCYCLES FROM CYCLE LENGTH.

    LSKIPH = .false.
    SCALE2 = 1.
    do I = 1,10
       KPER(I) = 0
       BANEXT(I) = 0.0
       RDNEXT(I) = 0.0
       1 continue
    end do
    NTYR = INT(FINT)
    if (LSTART) NTYR = IFINTH
    if (LESTB) NTYR = NTYR-5
    if (LESTB.and.NTYR.le.0) LSKIPH = .true.
    IYR = INT(REGYR)
    NPER = NTYR/IYR
    if (MOD(NTYR,IYR).ne.0) NPER = NPER+1
    ITOT = NTYR
    N = NPER
    do I = 1,NPER
       if (N.eq.1) go to 3
       KPER(I) = ITOT/N
       ITOT = ITOT-KPER(I)
       N = N-1
       2 continue
    end do
    3 continue
    if (NPER.gt.0)KPER(NPER) = ITOT

    !  LOAD TEMPORARY VARIABLES WITH AFTER THINNING BA, CCF, AND TOPHT.

    TEMBA = ATBA
    if (ATBA .le.0.)TEMBA = BA
    TEMCCF = ATCCF
    if (ATCCF .le.0.)TEMCCF = RELDEN
    TEMAHT = ATAVH
    if (DEBUG)write(16,*)'TEMBA,TEMCCF,TEMAHT,BA,RELDEN= ',&
         &TEMBA,TEMCCF,TEMAHT,BA,RELDEN

    !  IF THIS IS THE FIRST ENTRY, BRANCH TO THE CALIBRATION SECTION.

    if (LSTART) goto 40

    !  LOAD MULTIPLIERS.

    call MULTS (3,IY(ICYC),XRHMLT)
    call MULTS (6,IY(ICYC),XRDMLT)
    if (DEBUG) write(JOSTND,8996) LESTB,LSKIPH,ITRN,ITRNIN,SCALE2,NPER
    8996 format('IN REGENT, LESTB = ',L1,', LSKIPH = ',L1,&
         &', ITRN = ',I4,', ITRNIN = ',I4,', SCALE2 = ',F6.3,&
         &', NPER = ',I3)
    if (ITRN.le.0) return

    !  ESTIMATE THE CONTRIBUTION TO STAND BASAL AREA AND CCF FOR LARGE TREES AT
    !  THE START OF EACH PROJECTION SUBCYCLE.  IF CALLED FROM **ESTAB**, BRANCH
    !  TO STATEMENT 9 AND INTERPOLATE FROM OLD AND NEW DENSITY VALUES.
    !  OTHERWISE, 10-YEAR CHANGE IS COMPUTED FROM PREDICTED DBH INCREMENT AND
    !  ANNUALIZED.  THEN, ANNUAL RATE IS MULTIPLIED BY SUBCYCLE LENGTH TO
    !  DETERMINE CONTRIBUTION FOR EACH SUBCYCLE.  A ONE PERCENT ANNUAL RATE
    !  OF MORTALITY IS ASSUMED.  CONTRIBUTIONS OF SMALL TREES ARE COMPUTED AS
    !  INCREMENT PREDICTIONS ARE MADE FOR PRECEDING SUBCYCLE.

    if (LESTB) go to 8
    do J = 1,NPER
       BANEXT(J) = BA
       RDNEXT(J) = RELDEN
       4 continue
    end do
    if (DEBUG) write(JOSTND,*)'NPER,BA,RELDEN= ',NPER,BA,RELDEN
    if (NPER.eq.1) go to 11
    do I = 1,ITRN
       D1 = DBH(I)
       if (D1.lt.3.0) go to 6
       IS = ISP(I)
       P = PROB(I)
       D2 = D1+DG(I)/BRATIO(IS,D1,HT(I))
       B1 = BACON*D1*D1
       B2 = BACON*D2*D2
       call CCFCAL(IS,D1,HT(I),ICR(I),P,.false.,C1,CW,1)
       call CCFCAL(IS,D2,HT(I),ICR(I),P,.false.,C2,CW,1)
       BI = (B2-B1)/10.0
       CI = (C2-C1)/10.0
       K = 0
       do J = 2,NPER
          K = K+KPER(J-1)
          PN = P*0.985**K
          RDNEXT(J) = RDNEXT(J)+K*CI/P*PN
          BANEXT(J) = BANEXT(J)+K*BI*PN
          5 continue
       end do
       6 continue
    end do
    go to 11

    !  IF CALLED FROM **ESTAB**, DIAMETERS AND DENSITY STATISTICS HAVE
    !  BEEN UPDATED AND DENSITIES CORRESPONDING TO BEGINNINGS OF SUBCYCLES
    !  CAN BE INTERPOLATED FROM DIFFERENCE BETWEEN OLD AND NEW STATS.  THE
    !  TREES ARE SENT TO REGENT WITH A HEIGHT CORRESPONDING TO 5 YEARS AFTER
    !  THE START OF THE CYCLE.

    8 continue
    BAYR = 0.0
    CCFYR = 0.0
    if (LSKIPH) go to 9
    BAYR = (BA-TEMBA)/ITOT
    CCFYR = (RELDEN-TEMCCF)/ITOT
    9 continue
    NYR = 5
    do J = 1,NPER
       RDNEXT(J) = TEMCCF+NYR*CCFYR
       BANEXT(J) = TEMBA+NYR*BAYR
       NYR = NYR+KPER(J)
       10 continue
    end do
    if (DEBUG)write(16,*)'AFTER 10 NPER,RDNEXT1-2, BANEXT1-2= ',&
         &NPER,RDNEXT(1),RDNEXT(2),BANEXT(1),BANEXT(2)
    11 continue

    !  STORE INITIAL HEIGHTS IN WK3; DUB CROWN RATIO FOR NEWLY ESTABLISHED
    !  SEEDLINGS.

    do I = 1,ITRN
       if (LESTB.and.I.ge.ITRNIN) then
          IPCCF = ITRE(I)
          CR = 0.89722 - 0.0000461*PCCF(IPCCF)
          12 continue
          RAN = BACHLO(0.0,1.0,RANN)
          if (RAN .lt. -1.0.or. RAN .gt.1.0) go to 12
          CR = CR + 0.07985 * RAN
          if (CR .gt..90) CR = .90
          if (CR .lt..20) CR = .20
          ICR(I) = INT((CR*100.0)+0.5)
          IF(DEBUG)WRITE(JOSTND,*)'DUB CROWN ESTAB TREES I,PCCF,RAN,CR,I  &
               &  CR= ',I,PCCF(IPCCF),RAN,CR,ICR(I)
       end if
       WK3(I) = HT(I)
       WK5(I) = DBH(I)
       13 continue
    end do

    !  DELMAX IS USED TO COMPUTE A DENSITY-DEPENDENT ADJUSTMENT FOR
    !  THE HEIGHT-DIAMETER FUNCTION.  DELMAX IS BASED ON AVERAGE HEIGHT AND
    !  CCF AT THE BEGINNING OF THE CYCLE AND DOES NOT VARY BY SUBCYCLE.

    if (LESTB) then
       R = TEMCCF
       AH = TEMAHT
    else
       R = RELDEN
       AH = AVH
    end if
    DELMAX = (AH/36.0)*(0.01232*R-1.75)
    if (DELMAX.gt.0.0) DELMAX = 0.0

    ! COMPUTE DENSITY MODIFIER FROM CCF AND TOP HEIGHT FOR CR & UT VARIANT

    X = AH*(R/100.0)
    if (X .gt.300.0) X = 300.0
    PCTRED = 1.11436 + X*(-.011493 + X*(.43012E-4 + X*(-.72221E-7&
         &+ X*(.5607E-10- X*0.1641E-13))))
    if (PCTRED .gt.1.0) PCTRED = 1.0
    if (PCTRED .lt.0.01) PCTRED = 0.01
    if (DEBUG) write(JOSTND,9982) AH,R,X,PCTRED
    9982 format('IN REGENT AVHT,CCF,X,PCTRED = ',4F10.4)
    if (DEBUG) write(JOSTND,8995) R,AH,DELMAX
    8995 format('R = ',F10.4,', AH = ',F10.4,', DELMAX = ',F10.4)

    !  ENTER LOOP FOR NEXT SUBCYCLE.

    KY = 0
    do J = 1,NPER
       KY = KY+KPER(J)
       BAJ = BANEXT(J)
       RDJ = RDNEXT(J)

       !  PPCCF IS A PROPORTIONAL ADJUSTMENT FOR POINT CCF VALUES BASED ON
       !  CHANGE IN STAND CCF FOR THE SUBCYCLE.

       PPCCF = 0.0
       if (RELDEN.gt.0.0) PPCCF = 1.0+(RDJ-RELDEN)/RELDEN
       if (DEBUG)write(16,*)'J,KY,BAJ,RDJ,RELDEN,PPCCF= ',&
            &J,KY,BAJ,RDJ,RELDEN,PPCCF

       !  PROCESS ALL TREES WITHIN THIS SUBCYCLE IN SPECIES ORDER.  LOAD
       !  SPECIES DEPENDENT CONSTANTS.

       do ISPC = 1,MAXSP
          I1 = ISCT(ISPC,1)
          if (I1.eq.0) go to 16
          I2 = ISCT(ISPC,2)

          !  ASSIGN VARIANT ACCORDING TO SPECIES
          !  SET SCALE AND SCALE2 DEPENDING ON WHAT VARIANT THE EQUATIONS FOR
          !  THE SPECIES CAME FROM.

          CRVAR = .false.
          NIVAR = .false.
          TTVAR = .false.
          UTVAR = .false.

          select case (ISPC)
          case(1:12,14,23)
             NIVAR = .true.
             SCALE = FLOAT(KPER(J))/REGYR
             if (.not.LSKIPH)SCALE2 = YR/FLOAT(NTYR)
          case(13,17)
             TTVAR = .true.
             SCALE = FLOAT(KPER(J))/REGYR
             if (.not.LSKIPH)SCALE2 = YR/FLOAT(NTYR)
          case(19,22)
             CRVAR = .true.
             SCALE = FLOAT(NTYR)/YR
             if (.not.LSKIPH)SCALE2 = YR/FLOAT(NTYR)
          case default
             UTVAR = .true.
             SCALE = FLOAT(NTYR)/YR
             if (.not.LSKIPH)SCALE2 = YR/FLOAT(NTYR)
          end select
          if (DEBUG)write(JOSTND,*)&
               &' CRVAR,NIVAR,TTVAR,UTVAR,SCALE,SCALE2,LSKIPH= ',&
               &CRVAR,NIVAR,TTVAR,UTVAR,SCALE,SCALE2,LSKIPH
          if ((TTVAR.or.CRVAR).and.LSKIPH)goto 16

          !  SPECIES BASED ON A 10-YR REGYR ONLY NEED ONE PASS THROUGH THIS LOGIC

          if ((CRVAR .or. UTVAR) .and. J.gt.1)go to 16

          XRHGRO = XRHMLT(ISPC)
          XRDGRO = XRDMLT(ISPC)
          if (NIVAR) then
             CON = RHCON(ISPC) +HCOR(ISPC)
          else
             CON = RHCON(ISPC)*EXP(HCOR(ISPC))
          end if
          BH = RHLH(ISPC)
          BCCF = RHCCF(ISPC)
          BBAL = RHBAL(ISPC)
          XMX = XMAX(ISPC)
          XMN = XMIN(ISPC)
          AX = HHT1(ISPC)
          BX = HHT2(ISPC)

          !  PROCESS NEXT TREE RECORD.  STORE INTERMEDIATE HEIGHTS IN WK3.

          do I3 = I1,I2

             !  BYPASS INCREMENT CALCULATION IF DBH IS GREATER THAN XMX OR IF
             !  CALL IS FROM **ESTAB** AND TREE SUBSCRIPT IS LESS THAN ITRNIN.

             I = IND1(I3)
             D = DBH(I)
             H = HT(I)
             if (DEBUG)write(JOSTND,*)'ISPC,I,D,XMX,LESTB,ITRNIN= ',&
                  &ISPC,I,D,XMX,LESTB,ITRNIN
             if (D.ge.XMX) go to 15

             if (LESTB .and. I.lt.ITRNIN) go to 15
             P = PROB(I)
             H1 = WK3(I)
             D1 = WK5(I)
             BAL = BAJ*(100.0-PCT(I))*0.0001
             call CCFCAL(ISPC,D,H1,ICR(I),P,.false.,C1,CW,1)
             B1 = BACON*D*D
             BARK = BRATIO(ISPC,D,H)

             if (CRVAR.or.UTVAR) then
                SI = SITEAR(ISPC)
                if (SI .gt. SHI(ISPC)) SI = SHI(ISPC)
                if (SI .le. SLO(ISPC)) SI = SLO(ISPC) + 0.5

                !  COMPUTE POTENTIAL GROWTH FROM SITE INDEX AND SPECIES.
                !  THESE SWITCHED FROM THE CR FORMULATION TO THE UT FORMULATION
                !  BECAUSE THEY WORK BETTER.  GED  8/16/11

                RELSI = (SI-SLO(ISPC))/(SHI(ISPC)-SLO(ISPC))
                RSIMOD = 0.5 * (1.0 + RELSI)
                X = RELSI*100.0
                SJ = SITEAR(ISPC)
                if (CRVAR) then
                   !OLDCR          POTHTG = SI/(15.0 - 4.0*RELSI) * 1.0
                   POTHTG = ((SJ/5.0)*(SJ*1.5-H)/(SJ*1.5))* 0.83
                   if (DEBUG)write(JOSTND,*)'CRVAR I,SJ,H,POTHTG= ',&
                        &I,SJ,H,POTHTG
                else if (ISPC.eq.15.or. ISPC.eq.16) then
                   !OLDCR          POTHTG = (SI/10.0)*(SI*1.5-H1)/(SI*1.5)
                   POTHTG = ((SJ/5.0)*(SJ*1.5-H)/(SJ*1.5))* 0.83
                   if (DEBUG)write(JOSTND,*)'I,SJ,H,POTHTG= ',&
                        &I,SJ,H,POTHTG
                else
                   POTHTG = 0.
                end if
                if (DEBUG)write(JOSTND,*)'I,SI,H1,RELSI,POTHTG= ',&
                     &I,SI,H1,RELSI,POTHTG
             end if

             !  DETERMINE POINT CCF FOR THIS SUBCYCLE (BOUND BETWEEN 25 AND 300).

             IPCCF = ITRE(I)
             TPCCF = PCCF(IPCCF)*PPCCF
             if (TPCCF.gt.300.0) TPCCF = 300.0
             if (TPCCF.lt.25.0) TPCCF = 25.0
             CR = FLOAT(ICR(I))

             !  NI VARIANT CODE
             !  COMPUTE ADJUSTMENT FOR BIAS, DENSITY, AND RELATIVE SIZE.

             if (NIVAR) then
                if (ABS(AH-4.5).lt.0.01 ) then
                   RELH = 0.0
                else
                   RELH = (H1-4.5)/(AH-4.5)
                end if
                if (RELH.gt.1.0) RELH = 1.0
                if (RELH.lt.0.0) RELH = 0.0
                DADJ = DELMAX*RELH*RELH - 2.0*DELMAX*RELH + 0.65

                !  COMPUTE HEIGHT INCREMENT.  STORE UPDATED HEIGHT IN WK3.

                HTGRL = (CON+BH*ALOG(H1)+BCCF*RDJ+BBAL*BAL)
                IF(DEBUG)WRITE(JOSTND,*)'NI SECTION, I,AH,H1,RELH,DELMAX,DADJ,  &
                     &  CON,BH,BCCF,RDJ,BAL,HTGRL= ',I,AH,H1,RELH,DELMAX,DADJ,&
                     &CON,BH,BCCF,RDJ,BAL,HTGRL

                !  TT VARIANT CODE

             else if (TTVAR) then

                !  RESET ZRAND IF THIS IS THE START OF THE PROJECTION, THIS IS THE FIRST
                !  PREDICTION FOR A NEW TRIPLE, OR IF GROWTH IN THE PREVIOUS CYCLE WAS
                !  LESS THAN 0.1 FT.

                if (ZRAND(I).ne.-999.) go to 920
                910 continue
                ZRAND(I) = BACHLO(0.0,1.0,RANN)
                if (ZRAND(I).lt.-2.0.or. ZRAND(I).gt.2.0) go to 910
                920 continue
                D = DBH(I)
                if (D.le.0.0) then
                   HTGRL = 0.0
                   go to 930
                end if
                BETA1 = EXP(1.17527 - 0.42124*ALOG(TPCCF))
                BETA2 = EXP(-2.56002 - 0.58642*ALOG(TPCCF))
                HTG1 = BETA1 + BETA2*CR
                STDDEV = HTG1*(1.08720 - 0.00230*CR)
                HTGRL = HTG1 + ZRAND(I)*STDDEV
                IF(DEBUG)WRITE(JOSTND,*)'TT SECTION, I,D,TPCCF,BETA1,BETA2,CR,  &
                     &  HTG1,STDDEV,ZRAND,HTGRL= ',I,D,TPCCF,BETA1,BETA2,CR,HTG1,&
                     &STDDEV,ZRAND(I),HTGRL

                !  IF ESTIMATED HEIGHT INCREMENT IS CLOSE TO ZERO, SET FLAG TO DRAW
                !  A NEW RANDOM ERROR ON NEXT PASS THROUGH.

                if (HTGRL .le.0.1) then
                   HTGRL = 0.1
                   ZRAND(I) = -999.
                end if
                930 continue

                !  CR AND UT VARIANT CODE

             else
                if (CRVAR.or.UTVAR) then
                   X = CR/100.
                   VIGOR = (150.0 * (X**3.0)*EXP(-6.0*X))+0.3
                   if (VIGOR .gt.1.0)VIGOR = 1.0

                   !  VIGOR ADJUSTMENT TO DRASTIC FOR PINYON, JUNIPER, AND OAK
                   !  CUT IT BY TWO-THIRDS

                   if (ISPC.eq.15.or. ISPC.eq.16)VIGOR = 1.-((1.-VIGOR)/3.)

                   ! COMPUTE HT GROWTH AND AGE FOR ASPEN. EQN FROM WAYNE SHEPPARD RMS.

                   if (ISPC.eq.18.or. ISPC.eq.20.or. ISPC.eq.21) then
                      SITAGE = 0.0
                      SITHT = 0.0
                      AGMAX = 0.0
                      HTMAX = 0.0
                      HTMAX2 = 0.0
                      D1F = DBH(I)
                      D2F = 0.0
                      if (DEBUG)write(JOSTND,*)'IN HTGF, CALLING FINDAG I= ',I
                      call FINDAG(I,ISPC,D1F,D2F,H,SITAGE,SITHT,AGMAX,HTMAX,&
                           &HTMAX2,DEBUG)

                      if (LESTB) then
                         SITAGE = ABIRTH(I)
                      end if
                      HITE1 = 26.9825 * SITAGE**1.1752
                      AG2 = SITAGE+10.0
                      HITE2 = 26.9825 * AG2**1.1752
                      HTGRL = (HITE2-HITE1)/(2.54*12.0) * RSIMOD * CON
                      if (DEBUG)write(JOSTND,*)'HITE1,HITE2,RSIMOD,CON,HTGRL= ',&
                           &HITE1,HITE2,RSIMOD,CON,HTGRL

                      ! GROWTH RATES APPEAR HIGH, REDUCE BY 25 PERCENT. DIXON 8-27-92

                      HTGRL = HTGRL * .75
                      if (DEBUG)write(JOSTND,*)'HTGRL= ',HTGRL
                      go to 33
                   else
                      HTGRL = POTHTG*PCTRED*VIGOR*CON
                   end if
                   if (DEBUG) write(JOSTND,*)'CR/UT SECTION, I,POTHTG,PCTRED,',&
                        &'VIGOR,CON,X,HTGRL= ',I,POTHTG,PCTRED,VIGOR,CON,X,HTGRL
                end if
             end if
             33 continue

             ! H2 IS THE END OF CYCLE HEIGHT ON A FINT-YEAR BASIS
             ! CR AND UT ESTIMATES ARE 10-YEAR ESTIMATES AND THUS ADJUSTED BY FINT/10

             if (NIVAR) then
                H2 = H1+EXP(HTGRL)*SCALE*XRHGRO*WK4(I)
             else if (TTVAR) then
                H2 = H1+HTGRL*SCALE*XRHGRO*CON*WK4(I)
             else
                H2 = H1+HTGRL*SCALE
             end if
             WK3(I) = H2
             if (DEBUG)write(JOSTND,*)'I,H1,HTGRL,H2,KPER,REGYR,',&
                  &'XRHGRO,WK4,CON= ',&
                  &I,H1,HTGRL,H2,KPER(J),REGYR,XRHGRO,CON,WK4(I)

             !  UPDATE DENSITY FOR NEXT SUBCYCLE.  IF ONLY ONE SUBCYCLE OR
             !  IF INITIAL DBH IS GREATER THAN 3 INCHES, BYPASS DENSITY UPDATE.
             !  DIAMETER IS ADJUSTED FOR DENSITY AND USER-SUPPLIED MULTIPLIER.
             !  INCREMENT IS DETERMINED FROM THE DIFFERENCE BETWEEN COMPUTED
             !  DIAMETERS AND IS BOUNDED TO BE GREATER THAN OR EQUAL TO ZERO.

             if (NIVAR) then
                D2 = D
                if (J.ge.NPER.or.D.ge.3.0) go to 14
                if (H2.le.4.5) go to 14
                D1 = DIAM(ISPC)+DADJ
                if (H1.gt.4.5) D1 = AX*(H1-4.5)**BX + DADJ
                D2 = AX*(H2-4.5)**BX + DADJ
                DGJ = (D2-D1)*XRDGRO
                if (DEBUG)write(JOSTND,*)'NI SECTION, I,ISPC,AX,BX,DADJ,H1,D1,',&
                     &'H2,D2,DGJ= ',&
                     &I,ISPC,AX,BX,DADJ,H1,D1,H2,D2,DGJ
                if (DGJ.lt.0.0) DGJ = 0.0
                D2 = D+DGJ
             else if (TTVAR) then
                HLESS4 = H2-4.5
                DLESS3 = 0.000231*HLESS4*CR - 0.00005*HLESS4*PCCF(ITRE(I))&
                     &+ 0.001711*CR + 0.17023*HLESS4
                D2 = (DLESS3+0.3)*XRDGRO
                if (D2.lt.DIAM(ISPC)) D2 = DIAM(ISPC)
                if (DEBUG)write(JOSTND,*)'TT SECTION, I,ISPC,CR,PCCF,H2,D2,',&
                     &'XRDGRO= ',I,ISPC,CR,PCCF(ITRE(I)),H2,D2,XRDGRO
                WK3(I) = H2
                WK5(I) = D2
                if (J.ge.NPER .or. D.ge.3.0.or. H2.le.4.5)go to 14
             else if (CRVAR) then
                D2 = D
                if (D.ge.1.0)go to 15
                if (H2.le.4.5)D2 = D+0.0001*H2
                if (DEBUG)write(JOSTND,*)'CR SECTION, I,D,D2,H2= ',I,D,D2,H2
             else
                D2 = D
                if (ISPC.eq.18.or. ISPC.eq.20.or. ISPC.eq.21.and. D.ge.3)&
                     &go to 15
                if (H2.le.4.5) then
                   D2 = D+0.001*H2
                else if (ISPC.eq.15.or. ISPC.eq.16) then
                   D2 = (H2-4.5)*10./(SITEAR(ISPC)-4.5)
                   if (D2.lt.0.1) D2 = 0.1
                end if
                if (DEBUG)write(JOSTND,*)'I,D,D2,H2,SITEAR= ',&
                     &I,D,D2,H2,SITEAR(ISPC)
             end if

             !  STORE UPDATED DIAMETER IN WK5

             WK5(I) = D2
             if (TTVAR.and.H2.lt.4.5) goto 8897

             !  COMPUTE ADJUSTMENT TO STAND BASAL AREA AND CCF FOR NEXT SUBCYCLE.
             !  ASSUME A 1.5 PERCENT ANNUAL MORTALITY RATE.

             call CCFCAL(ISPC,D2,H2,ICR(I),P,.false.,C2,CW,1)
             RDNEXT(J+1) = RDNEXT(J+1)+KY*(C2-C1)/10.*0.985**KY
             BANEXT(J+1) = BANEXT(J+1)+(BACON*D2*D2-B1)*P*0.985**KY
             8897 continue
             if (DEBUG) write(JOSTND,8997) I,D2,BANEXT(J+1),RDNEXT(J+1)
             8997 format('UPDATE DENSITY: TREE = ',I4,', D2 = ',F8.4,&
                  &', BANEXT(J+1) = ',F8.2,', RDNEXT(J+1) = ',F8.2)

             !  END OF TREE LOOP WITHIN SPECIES. PRINT DEBUG IF REQUESTED.

             14 continue
             if (.not.DEBUG) go to 15
             write(JOSTND,8999) J,KPER(J),ISPC,I,H1,H2,BCCF,RDJ,BBAL,BAL,&
                  &BAJ,PCT(I)
             8999 format('IN REGENT L1: SUBC=',I2,', KPER=',I2,', SP=',I2,&
                  &', I=',I4,', H1=',F8.4,', H2=',F8.4,', BCCF=',F8.4/T4,'RDJ=',&
                  &F8.4,', BBAL=',F8.4,', BAL=',F8.4,', BAJ=',F8.4,', PCT=',F8.4)
             15 continue
          end do

          !  END OF SPECIES LOOP WITHIN SUBCYCLE

          16 continue
       end do
       if (DEBUG)write(16,*)'AFTER 16 NIVAR,TTVAR,UTVAR,CRVAR= ',&
            &NIVAR,TTVAR,UTVAR,CRVAR

       !  END OF SUBCYCLE LOOP.  END OF PERIOD HEIGHT IS STORED IN WK3.

       17 continue
    end do

    !  AFTER LAST SUBCYCLE, COMPUTE ACTUAL HEIGHT INCREMENT FOR FULL CYCLE BY
    !  SUBTRACTION; CONVERT BACK TO LOG SCALE TO ADD IN RANDOM ERROR.

    do ISPC = 1,MAXSP
       I1 = ISCT(ISPC,1)
       if (I1.eq.0) go to 30
       I2 = ISCT(ISPC,2)

       !  ASSIGN VARIANT ACCORDING TO SPECIES

       CRVAR = .false.
       NIVAR = .false.
       TTVAR = .false.
       UTVAR = .false.

       select case (ISPC)
       case(1:12,14,23)
          NIVAR = .true.
          SCALE = FLOAT(NTYR)/YR
          if (.not.LSKIPH)SCALE2 = YR/FLOAT(NTYR)
       case(13,17)
          TTVAR = .true.
          SCALE = FLOAT(NTYR)/YR
          if (.not.LSKIPH)SCALE2 = YR/FLOAT(NTYR)
       case(19,22)
          CRVAR = .true.
          SCALE = FLOAT(NTYR)/YR
          if (.not.LSKIPH)SCALE2 = YR/FLOAT(NTYR)
       case default
          UTVAR = .true.
          SCALE = FLOAT(NTYR)/YR
          if (.not.LSKIPH)SCALE2 = YR/FLOAT(NTYR)
       end select
       if (DEBUG)write(JOSTND,*)&
            &' CRVAR,NIVAR,TTVAR,UTVAR,SCALE,SCALE2,LSKIPH= ',&
            &CRVAR,NIVAR,TTVAR,UTVAR,SCALE,SCALE2,LSKIPH

       XRHGRO = XRHMLT(ISPC)
       XRDGRO = XRDMLT(ISPC)
       XMX = XMAX(ISPC)
       XMN = XMIN(ISPC)
       AX = HHT1(ISPC)
       BX = HHT2(ISPC)
       DGMX = DGMAX(ISPC)
       if (TTVAR)DGMX = FINT*DGMAX(ISPC)
       if (CRVAR)DGMX = DGMAX(ISPC)*FINT/10.
       if (DEBUG)write(16,*)'ENTERING 25 LOOP, NIVAR,TTVAR,UTVAR,CRVAR,',&
            &'XMX,XMN,AX,BX,DGMX,LSKIPH= ',&
            &NIVAR,TTVAR,UTVAR,CRVAR,XMX,XMN,AX,BX,DGMX,LSKIPH

       do I3 = I1,I2
          I = IND1(I3)
          D = DBH(I)
          CR = FLOAT(ICR(I))
          if (D.ge.XMX) go to 25
          if (LESTB .and. I.lt.ITRNIN) go to 25
          K = I
          L = 0
          H = HT(I)
          HK = WK3(I)

          D1 = DIAM(ISPC)
          if (NIVAR.and.H.gt.4.5) D1 = AX*(H-4.5)**BX+DADJ
          DK = WK5(I)
          HTGR1 = HK-H
          HTGR = HTGR1
          if (TTVAR.and.D.lt.D1)D = D1

          !  IF CALLED FROM **ESTAB** AND TOTAL CYCLE LENGTH IS LESS THAN OR EQUAL
          !  TO 5 YEARS, SKIP HEIGHT INCREMENT CALCULATION BUT ASSIGN CR AND DBH.

          if (NIVAR .and. LSKIPH) then
             HTG(K) = 0
             go to 20
          else if (TTVAR .and. LSKIPH) then
             IPCCF = ITRE(I)
             TPCCF = PCCF(IPCCF)
             if (H.ge.4.5) then
                HLESS4 = H-4.5
                DLESS3 = 0.000231*HLESS4*CR - 0.00005*HLESS4*PCCF(ITRE(I))&
                     &+ 0.001711*CR + 0.17023*HLESS4
                DK = (DLESS3+0.3)*XRDGRO
                if (DK.lt.D) DK = D
                DBH(I) = DK
                DG(I) = DK
                if (DG(I).gt.DGMX) DG(I) = DGMX
             end if
             go to 25
          else if (CRVAR .and. LSKIPH) then
             HTG(K) = 0
             go to 20
          else if (UTVAR .and. LSKIPH) then
             HTG(K) = 0
             go to 20
          end if
          if (NIVAR) then
             HTGR1 = HK-H
             if (HTGR1.lt.0.0) HTGR1 = 0.0
          end if

          !  DRAW RANDOM INCREMENT GIVEN EXPECTED VALUE, HTGR, AND STD. DEV.,
          !  HSIGMA.  BOUND DRAW TO (-1.5*SD < ZZRAN < 1.0*SD); IF TRIPLING,
          !  EACH TRIPLE GETS A NEW RANDOM NUMBER.  DO NOT TRIPLE IF CALLING
          !  FROM **ESTAB**.  DO NOT APPLY HEIGHT GROWTH MULTIPLIER; MULTIPLIER
          !  WAS USED DURING SUBCYCLING.

          ICRK = ICR(I)
          918 continue
          if (.not.TTVAR) then
             if (NIVAR) then
                18 continue
                ZZRAN = 0.0
                if (DGSD.ge.1.0) ZZRAN = BACHLO(0.0,1.0,RANN)
                if ((ZZRAN .gt.1.0) .or. (ZZRAN .lt. -1.5)) go to 18
                HTGR = HTGR1*EXP(ZZRAN*HSIGMA)
                if (DEBUG)write(JOSTND,*)'NI SECTION K,ZZRAN,HSIGMA,HTGR1,',&
                     &'HTGR= ',K,ZZRAN,HSIGMA,HTGR1,HTGR
             else
                919 continue
                ZZRAN = 0.0
                if (DGSD.ge.1.0) ZZRAN = BACHLO(0.0,1.0,RANN)
                if ((ZZRAN .gt.0.5) .or. (ZZRAN.lt.-2.0)) goto 919
                if (UTVAR)HTGR = (HTGR1 +ZZRAN*0.1)*XRHGRO
                if (CRVAR)HTGR = (HTGR1 +ZZRAN*0.2)*XRHGRO
                if (DEBUG)write(JOSTND,*)'CR/UT SECTION K,ZZRAN,XRHGRO,',&
                     &'SCALE,HTGR= ',K,ZZRAN,XRHGRO,SCALE,HTGR

                ! THIS CHECK ADDED BY DIXON 3/4/09 TO PREVENT NEGATIVE HEIGHT GROWTH
                ! MAY NEED A MORE ELEGANT FIX AT SOME POINT; BUT IT WORKS.

                if (HTGR .lt.0.1) then
                   HTGR = 0.1
                   if (DEBUG)write(JOSTND,*)'CR/UT RESETTING HTGR ',&
                        &'TO 0.1 FOOT'
                end if

             end if
          end if

          !  COMPUTE WEIGHTS FOR THE LARGE AND SMALL TREE HEIGHT INCREMENT
          !  ESTIMATES.  IF DBH IS LESS THAN OR EQUAL TO XMN OR IF CALLED
          !  FROM **ESTAB**, THE LARGE TREE PREDICTION IS IGNORED (XWT=0.0).

          XWT = 0.0
          if (D.le.XMN.or.LESTB) go to 19
          XWT = (D-XMN)/(XMX-XMN)
          19 continue
          if (DEBUG)write(JOSTND,*)'WEIGHTING K,HTGR,HTG,XWT= ',&
               &K,HTGR,HTG(K),XWT
          HTG(K) = HTGR*(1.0-XWT) + XWT*HTG(K)
          if (CRVAR.and.HTG(K) .lt..1)HTG(K) = .1

          ! CHECK FOR SIZE CAP COMPLIANCE.

          if ((H+HTG(K)).gt.SIZCAP(ISPC,4)) then
             HTG(K) = SIZCAP(ISPC,4)-H
             if (HTG(K) .lt.0.1) HTG(K) = 0.1
          end if

          20 continue

          !  ASSIGN DBH AND COMPUTE DBH INCREMENT FOR TREES WITH DBH LESS
          !  THAN BREAKPOINT.  COMPUTE 10-YEAR DBH INCREMENT REGARDLESS OF
          !  PROJECTION PERIOD LENGTH.  BYPASS INCREMENT CALCULATION IF
          !  D GE BREAKPOINT.

          if (CRVAR.and.D.ge.1.0)goto 23
          if (UTVAR.and.ISPC.eq.15.or. ISPC.eq.16)go to 1020
          if (D.ge.3.0) go to 23
          1020 continue

          !  COMPUTE ADJUSTMENT FOR BIAS, DENSITY, AND RELATIVE SIZE.

          if (NIVAR) then
             if (ABS(AH-4.5).lt.0.01) then
                RELH = 0.0
             else
                RELH = (H-4.5)/(AH-4.5)
             end if
             if (RELH.gt.1.0) RELH = 1.0
             if (RELH.lt.0.0) RELH = 0.0
             DADJ = DELMAX*RELH*RELH - 2.0*DELMAX*RELH + 0.65
             D1 = DIAM(ISPC)+DADJ
             if (H.gt.4.5) D1 = AX*(H-4.5)**BX+DADJ
             if (DEBUG)write(JOSTND,*)'K,ISPC,AX,BX,DADJ,H,D1= ',&
                  &K,ISPC,AX,BX,DADJ,H,D1
          end if
          HK = H+HTG(K)
          if (.not.TTVAR .and. HK.lt.4.5) then
             if (NIVAR) DBH(K) = 0.1 + DIAM(ISPC)*.01 + HK*0.001
             if (CRVAR) DBH(K) = D+.001*HK
             DG(K) = 0.0
          else
             if (NIVAR) then
                DK = AX*(HK-4.5)**BX+DADJ
                if (DEBUG)write(JOSTND,*)'K,ISPC,AX,BX,DADJ,HK,DK= ',&
                     &K,ISPC,AX,BX,DADJ,HK,DK
                if (DK.lt.DIAM(ISPC)) DK = DIAM(ISPC)
                DK = DK+HK*0.001
             else if (CRVAR .or. UTVAR) then
                if (ISPC.eq.15.or. ISPC.eq.16) then
                   DK = (HK-4.5)*10./(SITEAR(ISPC)-4.5)
                   if (DK .lt.0.1) DK = 0.1
                   DKK = (H-4.5)*10./(SITEAR(ISPC)-4.5)
                   if (DKK .lt.0.1) DKK = 0.1
                   if (H .lt.4.5) DKK = D
                else
                   BX = HT2(ISPC)
                   if (IABFLG(ISPC).eq.1) then
                      AX = HT1(ISPC)
                   else
                      AX = AA(ISPC)
                   end if
                   DK = (BX/(ALOG(HK-4.5)-AX))-1.0
                   if (DK .lt.0.1) DK = 0.1
                   if (H .le.4.5) then
                      DKK = D
                   else
                      DKK = (BX/(ALOG(H-4.5)-AX))-1.0
                   end if
                end if
                if (DEBUG)write(JOSTND,9986) AX,BX,ISPC,HK,BARK,&
                     &XRDGRO,DK,DKK
                9986 format('IN REGENT 9986 FORMAT AX,BX,ISPC,HK',&
                     &' BARK,XRDGRO,DK,DKK= '/T12, F10.3,2X,F10.3,2X,I5,2X,5F10.3)
             else if (TTVAR) then

                !  ASSIGN DIAMETER INCREMENT; APPLY MULTIPLIER

                if (HK .ge.4.5) then
                   HLESS4 = HT(K)-4.5
                   DLESS3 = 0.000231*HLESS4*CR - 0.00005*HLESS4*PCCF(ITRE(I))&
                        &+ 0.001711*CR + 0.17023*HLESS4
                   DKK = (DLESS3+0.3)
                   if (DKK.lt.DIAM(ISPC))DKK = DIAM(ISPC)
                   DG(K) = (DK-DKK)*XRDGRO
                else
                   DG(K) = 0.0
                end if
                DGK = DG(K)
             end if

             !       IF CALLING FROM **ESTAB** ASSIGN DIAMETER AND DIAMETER
             !       INCREMENT.

             if (LESTB) then
                DBH(K) = DK
                if (TTVAR.and.DG(K).gt.DGMX)DG(K) = DGMX
                if (NIVAR.or.CRVAR)DG(K) = DK
                if (CRVAR .or. UTVAR) then
                   if (DBH(K).lt.DIAM(ISPC)) DBH(K) = DIAM(ISPC)
                   DBH(K) = DBH(K)+0.001*HK
                   DG(K) = DBH(K)
                end if
             else

                !         DIAMETER INCREMENT IS THE DIFFERENCE BETWEEN COMPUTED
                !         DIAMETERS ON A FINT-YEAR BASIS.
                !         MULTIPLIER IS APPLIED TO DIAMETER INCREMENT AT THIS POINT.

                if (.not.TTVAR) then
                   if (NIVAR) then
                      DGK = (DK-D1)*XRDGRO
                   else if (CRVAR.or.UTVAR) then
                      if (DK.lt.0.0.or. DKK.lt.0.0) then
                         DGK = HTG(K)*0.2*BARK*XRDGRO
                         DK = D+DGK
                      else
                         DGK = (DK-DKK)*BARK*XRDGRO
                      end if
                      if (DGK.gt.DGMX)DGK = DGMX
                   end if
                end if
                if (DGK.lt.0.0) DGK = 0.0

                !         SCALE DIAMETER INCREMENT FOR BARK AND PERIOD LENGTH.
                !         IN ORDER TO MAINTAIN CONSISTENCY WITH **GRADD**,
                !         ADJUSTMENTS ARE MADE ON THE DDS SCALE.

                !  NOTE: DIAMETER INCREMENT IS ON A FINT-YR BASIS. SCALE TO 10-YR BASIS
                !        TO MAINTAIN CONSISTENCY WITH **GRADD**

                BARK = BRATIO(ISPC,DBH(K),HT(K))
                if (NIVAR .or. TTVAR) then
                   DG(K) = DGK*BARK
                else if (CRVAR.or.UTVAR) then
                   DG(K) = DGK
                end if
                DDS = DG(K)*(2.0*BARK*D+DG(K))*SCALE2
                DG(K) = SQRT((D*BARK)**2+DDS)-BARK*D
             end if
             if ((DBH(K)+DG(K)).lt.DIAM(ISPC)) then
                DG(K) = DIAM(ISPC)-DBH(K)
             end if
          end if

          !  CHECK FOR TREE SIZE CAP COMPLIANCE

          call DGBND(ISPC,DBH(K),DG(K))

          !  IF THE TREE WILL ATTAIN A DBH OF 3 OR MORE INCHES THIS CYCLE
          !  CALL **DUBSCR** TO ESTIMATE A NEW CROWN RATIO, IF IT DOES NOT HAVE ONE
          !  DIAMETER GROWTH SCALED TO CYCLE LENGTH FOR THIS CALCULATION.
          !  MAKE SURE NEW CROWN DOES NOT EXCEED MAX POSSIBLE GIVEN HTG.

          if (.not.LESTB) then
             D = DBH(K)
             BARK = BRATIO(ISPC,D,HT(K))
             DDS2 = (DG(K)*(2.0*BARK*D+DG(K)))*SCALE
             DG2 = SQRT((D*BARK)**2+DDS2)-BARK*D
             if (DG2.lt.0.0) DG2 = 0.0
             DNEW = D+DG2
             IITRE = ITRE(I)
             TPCCF = PCCF(IITRE)
             if (DNEW.ge.3.0) then
                if (ICRK .eq.0) then
                   call DUBSCR(ISPC,DNEW,HK,BA,CR,TPCCF,AVH,RMAI)
                   ICRK = IFIX(CR*100. + 0.5)
                end if
                ICR(K) = ICRK
                if (DEBUG)write(JOSTND,*)'AFTER DUBSCR K,ISPC,DNEW,HK,BA,CR,',&
                     &'ICR= ',K,ISPC,DNEW,HK,BA,CR,ICR(K)
             end if
          end if
          23 continue

          !  PRINT DEBUG AND RETURN TO PROCESS NEXT TRIPLE OR NEXT TREE.

          if (.not.DEBUG) go to 24
          write(JOSTND,9000) I,K,ISPC,H,HTG(K),HK,DBH(K),DG(K)
          9000 format('IN REGENT LOOP 2, I=',I4,', K=',I4,', ISPC=',I3,&
               &', CUR HT=',F7.2,', HT INC=',F7.4/T19,'NEW HT=',F7.2,&
               &', CUR DBH=',F7.2,', 10-YR DBH INC=',F7.4)
          24 continue
          if (LESTB .or..not.LTRIP .or. L.ge.2) go to 25
          if (TTVAR) then
             K = ITRN+2*I-1
             DBH(K) = DBH(I)
             DG(K) = DG(I)
             HT(K) = HT(I)
             HTG(K) = HTG(I)
             ICR(K) = ICR(I)
             K = K+1
             DBH(K) = DBH(I)
             DG(K) = DG(I)
             HT(K) = HT(I)
             HTG(K) = HTG(I)
             ICR(K) = ICR(I)
             go to 25
          else
             L = L+1
             K = ITRN+2*I-2+L
             go to 918
          end if

          !  END OF GROWTH PREDICTION LOOP.  PRINT DEBUG INFO IF DESIRED.

          25 continue
       end do
       30 continue
    end do
    return

    !  SMALL TREE HEIGHT CALIBRATION SECTION.

    40 continue
    do ISPC = 1,MAXSP
       HCOR(ISPC) = 0.0
       CORTEM(ISPC) = 1.0
       NUMCAL(ISPC) = 0
       45 continue
    end do
    if (ITRN.le.0) return
    if (IFINTH .eq.0)  goto 100
    SCALE3 = REGYR/FINTH

    !  DETERMINE BASAL AREA IN LARGE TREES AT START OF EACH PREDICTION SUBCYCLE.

    do J = 1,NPER
       BANEXT(J) = TEMBA
       RDNEXT(J) = TEMCCF
       46 continue
    end do
    if (NPER.eq.1) go to 50

    !  IF THERE IS MORE THAN 1 SUBCYCLE FOR HEIGHT INCREMENT PREDICTION,
    !  (IFINTH > 5) ESTIMATE VALUES OF BA AND RELATIVE DENSITY (CCF) AT
    !  START OF EACH SUBCYCLE.  10-YEAR CHANGE IS COMPUTED FROM BACKDATED
    !  DIAMETER (WK3) AND 10-YEAR DIAMETER INCREMENT COMPUTED DURING CALIBRATION.
    !  LINEAR INTERPOLATION IS USED TO ASSIGN VALUES FOR SUBCYCLES.

    do I = 1,ITRN
       D1 = WK3(I)
       IS = ISP(I)
       P = PROB(I)
       D2 = WK3(I)+DG(I)/BRATIO(IS,D1,HT(I))
       B1 = BACON*D1*D1
       B2 = BACON*D2*D2
       call CCFCAL(IS,D1,HT(I),ICR(I),P,.false.,C1,CW,1)
       call CCFCAL(IS,D2,HT(I),ICR(I),P,.false.,C2,CW,1)
       BI = (B2-B1)/10.0
       CI = (C2-C1)/10.0
       K = 0
       do J = 2,NPER
          K = K+KPER(J-1)
          PN = P*0.985**K
          RDNEXT(J) = RDNEXT(J)+K*CI/P*PN
          BANEXT(J) = BANEXT(J)+K*BI*PN
          48 continue
       end do
       49 continue
    end do
    50 continue

    ! COMPUTE DENSITY MODIFIER FROM CCF AND TOP HEIGHT FOR CR & UT VARIANT

    X = AVH*(RELDEN/100.0)
    if (X .gt.300.0) X = 300.0
    PCTRED = 1.11436 + X*(-.011493 + X*(.43012E-4 + X*(-.72221E-7&
         &+ X*(.5607E-10- X*0.1641E-13))))
    if (PCTRED .gt.1.0) PCTRED = 1.0
    if (PCTRED .lt.0.01) PCTRED = 0.01
    if (DEBUG) write(JOSTND,9989) AVH,RELDEN,X,PCTRED
    9989 format('IN REGENT AVH,RELDEN,X,PCTRED = ',4F10.4)

    !  BEGIN PROCESSING TREE LIST IN SPECIES ORDER.  DO NOT CALCULATE
    !  CORRECTION TERMS IF THERE ARE NO TREES FOR THIS SPECIES.

    do ISPC = 1,MAXSP

       !  ASSIGN VARIANT ACCORDING TO SPECIES

       NIVAR = .false.
       TTVAR = .false.
       UTVAR = .false.
       CRVAR = .false.
       if (ISPC.le.12.or. ISPC.eq.14.or. ISPC.eq.23) then
          NIVAR = .true.
       else if (ISPC.eq.13.or. ISPC.eq.17) then
          TTVAR = .true.
       else if (ISPC.eq.19.or. ISPC.eq.22) then
          CRVAR = .true.
       else
          UTVAR = .true.
       end if

       CORNEW = 1.0
       I1 = ISCT(ISPC,1)
       if (I1.eq.0.or..not. LHTCAL(ISPC)) go to 90
       I2 = ISCT(ISPC,2)
       IREFI = IREF(ISPC)
       N = 0
       SNP = 0.0
       SNX = 0.0
       SNY = 0.0
       BHAB = RHCON(ISPC)
       BLH = RHLH(ISPC)
       BCCF = RHCCF(ISPC)
       BBAL = RHBAL(ISPC)

       !  BEGIN TREE LOOP WITHIN SPECIES.  IF MEASURED HEIGHT INCREMENT IS
       !  LESS THAN OR EQUAL TO ZERO, OR DBH IS GREATER THAN 5.0, THE RECORD
       !  WILL BE EXCLUDED FROM THE CALIBRATION.

       do I3 = I1,I2
          I = IND1(I3)
          H = HT(I)
          if (IHTG.lt.2) H = H-HTG(I)
          if (DBH(I).ge.5.0.or.H.lt.0.01) go to 60
          if (HTG(I).lt.0.001) go to 60
          HK = H
          D = DBH(I)
          CR = FLOAT(ICR(I))
          do J = 1,NPER
             BAJ = BANEXT(J)
             RDJ = RDNEXT(J)
             BAL = BAJ*(100.0-PCT(I))*0.0001
             PPCCF = (RDJ-RELDM1)/RELDM1
             TPCCF = PCCF(ITRE(I))*PPCCF
             if (TPCCF.gt.300.0) TPCCF = 300.0
             if (TPCCF.lt.25.0) TPCCF = 25.0
             if (NIVAR) then
                EDH = EXP(BHAB + BLH*ALOG(HK) + BCCF*RDJ + BBAL*BAL)
                HK = HK+EDH

             else if (TTVAR) then

                !  RESET ZRAND IF THIS IS THE START OF THE PROJECTION, THIS IS THE FIRST
                !  PREDICTION FOR A NEW TRIPLE, OR IF GROWTH IN THE PREVIOUS CYCLE WAS
                !  LESS THAN 0.1 FT.

                if (ZRAND(I).ne.-999.) go to 921
                911 continue
                ZRAND(I) = BACHLO(0.0,1.0,RANN)
                if (ZRAND(I).lt.-2.0.or. ZRAND(I).gt.2.0) go to 911
                921 continue
                D = DBH(I)
                if (D.le.0.0) then
                   EDH = 0.0
                   go to 931
                end if
                BETA1 = EXP(1.17527 - 0.42124*ALOG(TPCCF))
                BETA2 = EXP(-2.56002 - 0.58642*ALOG(TPCCF))
                HTG1 = BETA1 + BETA2*CR
                STDDEV = HTG1*(1.08720 - 0.00230*CR)
                EDH = HTG1 + ZRAND(I)*STDDEV

                !  IF ESTIMATED HEIGHT INCREMENT IS CLOSE TO ZERO, SET FLAG TO DRAW
                !  A NEW RANDOM ERROR ON NEXT PASS THROUGH.

                if (EDH .le.0.1) then
                   EDH = 0.1
                   ZRAND(I) = -999.
                end if
                931 continue

                HK = HK+EDH
                HLESS4 = HK-4.5
                DLESS3 = 0.000231*HLESS4*CR - 0.00005*HLESS4*PCCF(ITRE(I))&
                     &+ 0.001711*CR + 0.17023*HLESS4
                D2 = (DLESS3+0.3)
                if (D2.lt.DIAM(ISPC)) D2 = DIAM(ISPC)

             else if (CRVAR .or. UTVAR) then

                !  FOR CR AND UT VARIANT SPECIES
                !  COMPUTE POTENTIAL GROWTH FROM SITE INDEX AND SPECIES.

                SI = SITEAR(ISPC)
                if (SI .gt. SHI(ISPC)) SI = SHI(ISPC)
                if (SI .le. SLO(ISPC)) SI = SLO(ISPC) + 0.5
                RELSI = (SI-SLO(ISPC))/(SHI(ISPC)-SLO(ISPC))
                RSIMOD = 0.5 * (1.0 + RELSI)
                X = RELSI*100.0
                SJ = SITEAR(ISPC)

                !  THESE SWITCHED FROM THE CR FORMULATION TO THE UT FORMULATION
                !  BECAUSE THEY WORK BETTER.  GED  8/16/11

                if (CRVAR) then
                   !OLDCR          POTHTG = SI/(15.0 - 4.0*RELSI) * 1.0
                   POTHTG = ((SJ/5.0)*(SJ*1.5-H)/(SJ*1.5))* 0.83
                   if (DEBUG)write(JOSTND,*)'CRVAR I,SJ,H,POTHTG= ',&
                        &I,SJ,H,POTHTG
                else if (ISPC.eq.15.or. ISPC.eq.16) then
                   !OLDCR          POTHTG = (SI/10.0)*(SI*1.5-H1)/(SI*1.5)
                   POTHTG = ((SJ/5.0)*(SJ*1.5-H)/(SJ*1.5))* 0.83
                   if (DEBUG)write(JOSTND,*)'I,SJ,H,POTHTG= ',&
                        &I,SJ,H,POTHTG
                else
                   POTHTG = 0.
                end if
                if (DEBUG)write(JOSTND,9988)SI,RELSI,X,ISPC,J,POTHTG
                9988 format('IN REGENT SI,RELSI,X,ISPC,J,POTHTG = ',&
                     &F7.2,F10.3,F10.4,2I6,F10.3)

                X = CR/100.
                VIGOR = (150.0 * (X**3.0)*EXP(-6.0*X))+0.3
                if (VIGOR .gt.1.0)VIGOR = 1.0

                !  VIGOR ADJUSTMENT TO DRASTIC FOR PINYON, JUNIPER, AND OAK
                !  CUT IT BY TWO-THIRDS

                if (ISPC.eq.15.or. ISPC.eq.16)VIGOR = 1.-((1.-VIGOR)/3.)

                ! COMPUTE HT GROWTH AND AGE FOR ASPEN. EQN FROM WAYNE SHEPPARD RMS.

                if (ISPC.eq.18.or. ISPC.eq.20.or. ISPC.eq.21) then
                   AG1 = (H*12.0*2.54/26.9825)**0.8509
                   AG2 = AG1+10.0
                   H2 = (26.9825*AG2**1.1752)/(2.54*12.0)
                   EDH = (H2-H) * RSIMOD * RHCON(ISPC)*0.75
                else
                   EDH = POTHTG*PCTRED*VIGOR*RHCON(ISPC)
                end if

                ! CR AND UT MODELS ARE 10-YEAR MODELS, ADJUST TO 5-YEAR BASIS

                EDH = EDH*0.5

                if (DEBUG)write(JOSTND,9990) X,VIGOR,EDH
                9990 format('IN REGENT X,VIGOR,EDH = ',3F10.4)
             end if
             55 continue
          end do
          if (NIVAR) then
             EDH = HK-H
          else if (TTVAR) then
             EDH = (HK-H)*RHCON(ISPC)
          end if
          P = PROB(I)
          TERM = HTG(I)*SCALE3
          SNP = SNP+P
          SNX = SNX+EDH*P
          SNY = SNY+TERM*P
          N = N+1

          !  PRINT DEBUG INFO IF DESIRED.

          if (DEBUG) write(JOSTND,9001) NPLT,I,ISPC,H,DBH(I),ICR(I),&
               &PCT(I),TEMCCF,RHCON(ISPC),EDH,TERM
          9001 format('NPLT=',A26,',  I=',I5,',  ISPC=',I3,',  H=',F6.1,&
               &',  DBH=',F5.1,',  ICR',I5,',  PCT=',F6.1,',  RELDEN=',&
               &F6.1 / 12X,'RHCON=',F10.3,',  EDH=',F10.3,', TERM=',F10.3)

          !  END OF TREE LOOP WITHIN SPECIES.

          60 continue
       end do
       if (DEBUG) write(JOSTND,9010) ISPC,SNP,SNX,SNY
       9010 format(/'SUMS FOR SPECIES ',I2,':  SNP=',F10.2,&
            &';   SNX=',F10.2,';   SNY=',F10.2)

       !  COMPUTE CALIBRATION TERMS.  CALIBRATION TERMS ARE NOT COMPUTED
       !  IF THERE WERE FEWER THAN NCALHT (DEFAULT=5) HEIGHT INCREMENT
       !  OBSERVATIONS FOR A SPECIES.

       if (N.lt.NCALHT) go to 80

       !  CALCULATE MEANS FOR THE FOR THE GROWTH SAMPLE ON THE
       !  NATURAL SCALE.

       call REGHCAL(ISPC,SNP,SNX,SNY,CORNEW)
       80 continue
       CORTEM(IREFI) = CORNEW
       NUMCAL(IREFI) = N
       90 continue
    end do

    !  END OF CALIBRATION LOOP.  PRINT CALIBRATION STATISTICS AND RETURN

    write(JOSTND,9002) (NUMCAL(I),I = 1,NUMSP)
    9002 format(/'NUMBER OF RECORDS AVAILABLE FOR SCALING'/&
         &'THE SMALL TREE HEIGHT INCREMENT MODEL',&
         &((T48,11(I4,2X)/)))
    100 continue
    write(JOSTND,9003) (CORTEM(I),I = 1,NUMSP)
    9003 format(/'INITIAL SCALE FACTORS FOR THE SMALL TREE'/&
         &'HEIGHT INCREMENT MODEL',&
         &((T48,11(F5.2,1X)/)))

    ! OUTPUT CALIBRATION TO DATABASE.

    call REGCALOUT(CORTEM,NUMCAL,KOUT,K,KK,ISPEC,SPEC)
    return
    9998 continue

    !  ENTRY POINT FOR LOADING OF REGENERATION GROWTH MODEL
    !  CONSTANTS  THAT REQUIRE ONE-TIME RESOLUTION.
    !  IDTYPE IS A HABITAT INDEX THAT IS COMPUTED IN **RCON**.
    !  RHSC IS AN ARRAY FOR SPECIES CONSTANTS, RHGL IS AN ARRAY FOR
    !  LOCATION CONSTANTS, RHHAB IS AN ARRAY FOR HABITAT DEPENDENT
    !  CONSTANTS, AND, RSAB0, RSAB1, AND RSAB2 ARE COEFFICIENTS FOR
    !  THE SLOPE AND ASPECT TRANSFORMATION.

    data RHHAB/&
         &-0.2146, -0.0941, -0.3141,     0.0,     0.0,      0.0,&
         &-0.2146, -0.0941, -0.3296,     0.0,     0.0,      0.0,&
         &-0.2146, -0.0941, -0.5401, -0.3948,     0.0,      0.0,&
         &-0.2146, -0.0941, -0.2776,     0.0,     0.0,      0.0,&
         &-0.2146, -0.0941,     0.0,     0.0,     0.0,      0.0,&
         &-0.2146, -0.0941,     0.0,     0.0,     0.0,      0.0,&
         &-0.2146, -0.0941, -0.2484, -0.5134, -0.3495,      0.0,&
         &-0.2146, -0.0941, -0.3431,     0.0,     0.0,      0.0,&
         &-0.2146, -0.0941, -0.4916, -0.3582,     0.0,      0.0,&
         &-0.2146, -0.0941, -0.4345,     0.0,     0.0,      0.0,&
         &-0.2146, -0.0941, -0.3738,     0.0,     0.0,      0.0,&
         &-0.2146, -0.0941, -0.3296,     0.0,     0.0,      0.0,&
         &0.0000,  0.0000,  0.0000,     0.0,     0.0,      0.0,&
         &-0.2146, -0.0941, -0.4916, -0.3582,     0.0,      0.0,&
         &0.0000,  0.0000,  0.0000,     0.0,     0.0,      0.0,&
         &0.0,     0.0,     0.0,     0.0,     0.0,      0.0,&
         &0.0,     0.0,     0.0,     0.0,     0.0,      0.0,&
         &0.0,     0.0,     0.0,     0.0,     0.0,      0.0,&
         &0.0,     0.0,     0.0,     0.0,     0.0,      0.0,&
         &0.0,     0.0,     0.0,     0.0,     0.0,      0.0,&
         &0.0,     0.0,     0.0,     0.0,     0.0,      0.0,&
         &0.0,     0.0,     0.0,     0.0,     0.0,      0.0,&
         &-0.2146, -0.0941, -0.3738,     0.0,     0.0,      0.0/
    data MAPHAB/&
         &12*3, 1,2, 4*4, 1, 11*3,&
         &9*3, 4*1, 2, 4*4, 1, 11*3,&
         &7*4, 1, 3*4, 1,1,2, 4*5, 1,4,3, 6*4, 3,4,4,&
         &12*3, 1,2, 4*4, 1,3,3,1,1, 7*3,&
         &13*1, 2, 4*3, 12*1,&
         &13*1, 2, 4*3, 12*1,&
         &4*5, 3*1, 4*5, 4,4,2, 5*6, 4,4,3,4, 3*5, 4,4,5,5,&
         &13*1, 2, 4*4, 9*1, 3,1,1,&
         &12*4, 3,2, 4*5, 1, 7*4, 1,3,3,4,&
         &3*1, 3,3,1,1, 4*3, 1,1,2, 4*4, 1, 11*3,&
         &12*3, 1,2, 4*4, 1,3,3,2, 8*3,&
         &9*3, 4*1, 2, 4*4, 1, 11*3,&
         &30*1,&
         &12*4, 3,2, 4*5, 1, 7*4, 1,3,3,4,&
         &30*1,&
         &30*1,&
         &30*1,&
         &30*1,&
         &30*1,&
         &30*1,&
         &30*1,&
         &30*1,&
         &12*3, 1,2, 4*4, 1,3,3,2, 8*3/
    data  RHSC/&
         &1.4700,1.6204,1.4932,.9981,1.0202,.8953,1.2336,1.0964,1.0667,&
         &1.7311,.8953,&
         &1.6204, 0., 1.0667, 8*0., 0.8953/
    data  RHGL/&
         &-.2785,-.0480,0./
    data RSAB0,RSAB1,RSAB2/&
         &-0.10987,0.22157,-0.12432/
    !*    WRITE(JOSTND,777)  IGL,ITYPE,ASPECT,SLOPE
    !*777 FORMAT('IN REGCON IGL ITYPE ASPECT SLOPE=',2I5,2F8.2)

    !  COMBINE SITE EFFECTS THAT ARE INDEPENDENT OF SPECIES.

    REGCH = RHGL(IGL) +&
         &(RSAB0 + RSAB1*COS(ASPECT) + RSAB2*SIN(ASPECT))*SLOPE

    !  LOAD OVERALL SITE CONSTANTS INTO RHCON BY SPECIES.  IF SCALE FACTORS
    !  HAVE BEEN INPUT WITH THE READCORR KEYWORD, INCORPORATE THEM INTO
    !  THE OVERALL CONSTANTS.

    do ISPC = 1,MAXSP

       !  ASSIGN VARIANT ACCORDING TO SPECIES

       NIVAR = .false.
       TTVAR = .false.
       UTVAR = .false.
       CRVAR = .false.
       if (ISPC.le.12.or. ISPC.eq.14.or. ISPC.eq.23) then
          NIVAR = .true.
       else if (ISPC.eq.13.or. ISPC.eq.17) then
          TTVAR = .true.
       else if (ISPC.eq.19.or. ISPC.eq.22) then
          CRVAR = .true.
       else
          UTVAR = .true.
       end if

       if (NIVAR) then
          IRHHAB = MAPHAB(ITYPE,ISPC)

          !*      WRITE(JOSTND,778) ISPC,IRHHAB
          !*778   FORMAT('REGCON ISPC IRHHAB=',2I6)

          RHCON(ISPC) = REGCH + RHSC(ISPC) + RHHAB(IRHHAB,ISPC)
          if (LRCOR2.and.RCOR2(ISPC).gt.0.0) RHCON(ISPC) = RHCON(ISPC) +&
               &ALOG(RCOR2(ISPC))
       else
          RHCON(ISPC) = 1.0
          if (LRCOR2.and. RCOR2(ISPC).gt.0.0)&
               &RHCON(ISPC) = RCOR2(ISPC)
       end if
       110 continue
    end do
    return
  end subroutine REGIE

  !===============================!
  !   Compute small tree growth   !
  !===============================!
  ! KT $Id$
  subroutine REGKT(LCON)
    implicit none

    logical :: LCON
    ! Regional callees retain their native argument lists.
    interface
       subroutine DUBSCR(ISPC,D,H,BA,CR)
         integer :: ISPC
         real :: D
         real :: H
         real :: BA
         real :: CR
       end subroutine DUBSCR
    end interface


    !  **REGENT** COMPUTES HEIGHT AND DIAMETER INCREMENTS FOR SMALL
    !  TREES.  THE HEIGHT INCREMENT MODEL IS APPLIED TO TREES THAT ARE LESS
    !  THAN 10 INCHES DBH (5 INCHES FOR LODGEPOLE PINE), AND THE DBH
    !  INCREMENT MODEL IS APPLIED TO TREES THAT ARE LESS THAN 3 INCHES DBH.
    !  FOR TREES THAT ARE GREATER THAN 2 INCHES DBH (1 INCH FOR LODGEPOLE
    !  PINE), HEIGHT INCREMENT PREDICTIONS ARE AVERAGED WITH THE PREDICTIONS
    !  FROM THE LARGE TREE MODEL. HEIGHT INCREMENT IS A FUNCTION OF HABITAT
    !  TYPE, GEOGRAPHIC LOCATION, SLOPE, SLOPE SQUARED, ASPECT, TREE HEIGHT,
    !  TREE HEIGHT SQUARED, TREE HEIGHT SQUARED MODIFICATION,
    !  STAND BASAL AREA, BASAL AREA IN LARGER TREES,
    !  CROWN RATIO, CROWN RATIO SQUARED, ELEVATION, ELEVATION
    !  SQUARED, AND POINT CROWN COMPETITION FACTOR.  DIAMETER IS ASSIGNED
    !  FROM A HEIGHT DIAMETER FUNCTION WITH ADJUSTMENTS FOR RELATIVE
    !  SIZE AND STAND DENSITY.  INCREMENT IS COMPUTED BY SUBTRACTION.
    !  WHEN THE TREE ATTAINS A DBH OF 3.0 INCHES IN A CYCLE, **DUBSCR**
    !  IS CALLED TO ASSIGN A CROWN RATIO.  **REGENT IS CALLED FROM
    !  **CRATET** DURING CALIBRATION AND FROM **GRINCR** DURING CYCLING.
    !  ENTRY **REGCON** IS CALLED FROM **RCON** TO LOAD MODEL PARAMETERS
    !  THAT NEED ONLY BE RESOLVED ONCE.

    !  **REGENT** IS ALSO USED TO PREDICT HEIGHT INCREMENT, DBH AND CROWN
    !  RATIO FOR TREES THAT ARE CREATED BY THE REGENERATION ESTABLISHMENT
    !  MODEL.  5-YEAR OLD TREES ARE GENERATED BY **ESTAB** AND **REGENT**
    !  COMPUTES INCREMENTS FROM AGE 5 TO THE END OF THE CYCLE.

    ! Common blocks

    !  DIMENSIONS FOR INTERNAL VARIABLES:

    !   BANEXT -- ESTIMATED STAND BASAL AREA AT START OF EACH HEIGHT INCREMENT
    !             PREDICTION SUBCYCLE.
    !   CORTEM -- A TEMPORARY ARRAY FOR PRINTING CORRECTION TERMS.
    !     KPER -- NUMBER OF YEARS (MAX OF 5) IN EACH HEIGHT INCREMENT PREDICTION
    !             SUBCYCLE.
    !   NUMCAL -- A TEMPORARY ARRAY FOR PRINTING NUMBER OF HEIGHT
    !             INCREMENT OBSERVATIONS BY SPECIES.
    !   RDNEXT -- RELATIVE DENSITY (CCF) AT START OF EACH HEIGHT INCREMENT
    !             PREDICTION SUBCYCLE.
    !    RHCON -- CONSTANT, BY SPECIES, FOR THE HEIGHT INCREMENT MODEL.
    !             INCLUDES EFFECTS OF HABITAT TYPE, LOCATION, SLOPE, ASPECT,
    !             ELEVATION, CROWN RATIO, AND BRUSH AS LOADED IN RCON.
    !     RHLH -- THE COEFFICIENTS FOR EACH SPECIES FOR THE LN(HEIGHT)
    !             TERM IN THE HEIGHT INCREMENT MODEL.
    !    RHBAL -- THE COEFFICIENTS FOR EACH SPECIES FOR THE BAL TERM
    !             IN THE HEIGHT INCREMENT MODEL.
    !     RHBA -- THE COEFFICIENTS FOR EACH SPECIES FOR THE BA TERM
    !             IN THE HEIGHT INCREMENT MODEL.
    !     HCON -- COEFFICIENT FOR THE HEIGHT TERM IN THE LINEAR DIAMETER
    !             DUBBING FUNCTION.
    !     DCON -- CONSTANT TERM FOR THE LINEAR DIAMETER DUBBING FUNCTION.
    !     XMAX -- UPPER END OF THE RANGE OF DIAMETERS OVER WHICH HEIGHT
    !             INCREMENT PREDICTIONS FROM SMALL AND LARGE TREE MODELS
    !             ARE AVERAGED.
    !     XMIN -- LOWER END OF THE RANGE OF DIAMETERS OVER WHICH HEIGHT
    !             INCREMENT PREDICTIONS FROM THE SMALL AND LARGE TREE
    !             ARE AVERAGED.
    !   HT2MOD -- FACTOR ADJUSTING THE HEIGHT SQUARED TERM IN HEIGHT
    !             INCREMENT EQUATION.

    external RANN
    ! Variables
    logical :: DEBUG,LSKIPH
    real    :: CORTEM(11)
    integer :: NUMCAL(11)
    real    :: RHLH(11),RHSC(11)
    real    :: XMAX(11),XMIN(11),HCON(11),RHBAL(11)
    real    :: RHHAB(6,11)
    integer :: MAPHAB(175,11)
    real    :: HTFOR(5,11),DIAM(11)
    integer :: MAPLOC(10,11)
    real    :: RHBA(11),HTSLOP(11),HTCASP(11)
    real    :: HTEL(11),HTSLSQ(11),HTCR(11),HTCR2(11)
    real    :: HTH2(11),HTPCC1(11),HT2MOD(11),HTSASP(11)
    real    :: BANEXT(10),RDNEXT(10)
    integer :: KPER(10)
    real    :: RHCCF(11),RHGL(3),DCON(11)
    real    :: H1,BAL,BALMH,RELH,DADJ
    real    :: HTGRL,H2,DGJ,H,HK,HTGR1,ZZRAN,HTGR,XWT,DK,DGK,BARK
    real    :: DDS,SCALE2,DDS2,DG2,DNEW,TEMCR,CRLN,CRMAX,SCALE3,CORNEW
    real    :: CR,RAN,R,AH,DELMAX,BAJ,RDJ,ALBA,XRHGRO,XRDGRO,CON,BH
    real    :: HTHS2,HTCRS,HTCRS2,BBA,BCCF,RHCONS,BBALMH,XMX,XMN,D
    real    :: DUM1,HTPC1,PCCF1,TEMBA,TEMCCF,TEMAHT,SCALE,D1,P,D2,B1,B2
    real    :: CW,C1,C2,BI,CI,PN,BAYR,CCFYR,BBAL,BACON,HSIGMA,REGYR
    real    :: BRATIO,BACHLO,SNP,SNX,SNY,BHAB,BLH,EDH,TERM
    real    :: RSAB2,RSAB1,RSAB0
    integer :: I,NTYR,IYR,NPER,ITOT,N,J,IS,K,NYR,IPCCF,KY,ISPC
    integer :: I1,I2,I3,L,IREFI,KOUT,ISPEC,KK,IRHFOR
    character(len=2) :: SPEC

    ! Data statements
    data DIAM/&
         &0.4, 3*0.3, 2*0.2, 0.4, 2*0.3, 0.5, 0.2/
    data DCON/&
         &-.145707,-.111929,-.127690,-.125198,-.178162,-.233566,&
         &-.010818,-.186874,-.158870,-.185011,0.0/
    data HCON/&
         &.125404,.098451,.115066,.128790,.129191,.143211,.090927,&
         &.131209,.128174,.141907,0.0/
    data RHBAL/&
         &-0.002569,.002745,.002367,.000683,6*0.0,-0.25349/
    data  RHLH/&
         &.233302,.187195,.185496,.192256,.126835,.083622,.175631,.160429,&
         &.169834,.387617,.2354  /
    data RHCCF/&
         &10*0.0,-.00391/
    data HTH2/&
         &-.007483,-.005570,-.005958,-.006796,-.004391,-.002450,-.004546,&
         &-.005592,-.005955,-.015505,.000000/
    data HT2MOD/&
         &0.5,0.4,0.5,0.5,0.6,0.6,0.7,0.5,0.5,0.5,1.0/
    data RHBA/&
         &-.225869,-.612994,-.580046,-.303384,-.310048,-.188838,-.523915,&
         &-.270415,-.337342,-.259310,.000000/
    data HTCR/&
         &-.037519,1.793489,.416130,1.384217,.802352,.708060,.771466,&
         &-.520312,-.274359,2.976688,.000000/
    data HTCR2/&
         &1.895808,1.713339,1.070272,.000000,.343107,.231768,2.223783,&
         &1.859532,1.212637,-.803694,.000000/
    data HTPCC1/&
         &.002726,.001935,.002364,.001374,.000325,.000516,.002926,&
         &.001409,.001964,.003596,.000000/
    data XMAX/6*10.0,5.0,4*10.0/,XMIN/6*2.0,1.0,4*2.0/
    data REGYR/5.0/,HSIGMA/0.59/,BACON/0.005454154/

    if (LCON) go to 9998

    !  CHECK FOR DEBUG.

    call DBCHK (DEBUG,'REGENT',6,ICYC)

    !  THE SMALL TREE HEIGHT INCREMENT MODEL IS BASED ON 5-YEAR INCREMENT DATA.
    !  TO AVOID BIAS, PREDICTIONS ARE MADE FOR SUBCYCLES THAT ARE LESS THAN OR
    !  EQUAL TO 5 YEARS IN LENGTH.  THE FOLLOWING CODE DETERMINES THE NUMBER
    !  AND LENGTH OF SUBCYCLES FROM CYCLE LENGTH.

    LSKIPH = .false.
    do I = 1,10
       KPER(I) = 0
       BANEXT(I) = 0.0
       RDNEXT(I) = 0.0
       1 continue
    end do
    NTYR = INT(FINT)
    if (LSTART) NTYR = IFINTH
    if (LESTB) NTYR = NTYR-5
    if (LESTB.and.NTYR.le.0) LSKIPH = .true.
    IYR = INT(REGYR)
    NPER = NTYR/IYR
    if (DEBUG) write(JOSTND,*)'ENTERING REGENT','NPER=',NPER
    if (MOD(NTYR,IYR).ne.0) NPER = NPER+1
    ITOT = NTYR
    N = NPER
    do I = 1,NPER
       if (N.eq.1) go to 3
       KPER(I) = ITOT/N
       ITOT = ITOT-KPER(I)
       N = N-1
       2 continue
    end do
    3 continue
    if (NPER.gt.0) KPER(NPER) = ITOT

    !  LOAD TEMPORARY VARIABLES WITH AFTER THINNING BA, CCF, AND TOPHT.

    TEMBA = ATBA
    if (ATBA .le.0.)TEMBA = BA
    TEMCCF = ATCCF
    if (ATCCF .le.0.)TEMCCF = RELDEN
    TEMAHT = ATAVH

    !  IF THIS IS THE FIRST ENTRY, BRANCH TO THE CALIBRATION SECTION.

    if (LSTART) goto 40

    !  LOAD MULTIPLIERS.

    call MULTS (3,IY(ICYC),XRHMLT)
    call MULTS(6,IY(ICYC),XRDMLT)
    SCALE = YR/FINT
    if (DEBUG) write(JOSTND,8996) LESTB,LSKIPH,ITRN,ITRNIN,SCALE
    8996 format('IN REGENT, LESTB = ',L1,', LSKIPH = ',L1,&
         &', ITRN = ',I4,', ITRNIN = ',I4,', SCALE = ',F6.3)
    if (ITRN.le.0) return

    !  ESTIMATE THE CONTRIBUTION TO STAND BASAL AREA AND CCF FOR LARGE TREES AT
    !  THE START OF EACH PROJECTION SUBCYCLE.  IF CALLED FROM **ESTAB**, BRANCH
    !  TO STATEMENT 9 AND INTERPOLATE FROM OLD AND NEW DENSITY VALUES.
    !  OTHERWISE, 10-YEAR CHANGE IS COMPUTED FROM PREDICTED DBH INCREMENT AND
    !  ANNUALIZED.  THEN, ANNUAL RATE IS MULTIPLIED BY SUBCYCLE LENGTH TO
    !  DETERMINE CONTRIBUTION FOR EACH SUBCYCLE.  A ONE PERCENT ANNUAL RATE
    !  OF MORTALITY IS ASSUMED.  CONTRIBUTIONS OF SMALL TREES ARE COMPUTED AS
    !  INCREMENT PREDICTIONS ARE MADE FOR PRECEDING SUBCYCLE.

    if (LESTB) go to 8
    do J = 1,NPER
       BANEXT(J) = BA
       RDNEXT(J) = RELDEN
       4 continue
    end do
    if (NPER.eq.1) go to 11
    do I = 1,ITRN
       D1 = DBH(I)
       if (D1.lt.3.0) go to 6
       IS = ISP(I)
       P = PROB(I)
       D2 = D1+DG(I)/BRATIO(IS,D1,HT(I))
       B1 = BACON*D1*D1
       B2 = BACON*D2*D2
       call CCFCAL(IS,D1,HT(I),ICR(I),P,.false.,C1,CW,1)
       call CCFCAL(IS,D2,HT(I),ICR(I),P,.false.,C2,CW,1)
       BI = (B2-B1)/10.0
       CI = (C2-C1)/10.0
       K = 0
       do J = 2,NPER
          K = K+KPER(J-1)
          PN = P*0.985**K
          RDNEXT(J) = RDNEXT(J)+K*CI/P*PN
          BANEXT(J) = BANEXT(J)+K*BI*PN
          5 continue
       end do
       6 continue
    end do
    go to 11

    !  IF CALLED FROM **ESTAB**, DIAMETERS AND DENSITY STATISTICS HAVE
    !  BEEN UPDATED AND DENSITIES CORRESPONDING TO BEGINNINGS OF SUBCYCLES
    !  CAN BE INTERPOLATED FROM DIFFERENCE BETWEEN OLD AND NEW STATS.  THE
    !  TREES ARE SENT TO REGENT WITH A HEIGHT CORRESPONDING TO 5 YEARS AFTER
    !  THE START OF THE CYCLE.

    8 continue
    BAYR = 0.0
    CCFYR = 0.0
    if (LSKIPH) go to 9
    BAYR = (BA-TEMBA)/ITOT
    CCFYR = (RELDEN-TEMCCF)/ITOT
    9 continue
    NYR = 5
    do J = 1,NPER
       RDNEXT(J) = TEMCCF+NYR*CCFYR
       BANEXT(J) = TEMBA+NYR*BAYR
       NYR = NYR+KPER(J)
       10 continue
    end do
    11 continue

    !  STORE INITIAL HEIGHTS IN WK3; DUB CROWN RATIO FOR NEWLY ESTABLISHED
    !  SEEDLINGS.

    do I = 1,ITRN
       if (LESTB.and.I.ge.ITRNIN) then
          IPCCF = ITRE(I)
          CR = 0.89722 - 0.0000461*PCCF(IPCCF)
          12 continue
          RAN = BACHLO(0.0,1.0,RANN)
          if (RAN .lt. -1.0.or. RAN .gt.1.0) go to 12
          CR = CR + 0.07985 * RAN
          if (CR .gt..90) CR = .90
          if (CR .lt..20) CR = .20
          ICR(I) = INT((CR*100.0)+0.5)
       end if
       WK3(I) = HT(I)
       13 continue
    end do

    !  DELMAX IS USED TO COMPUTE A DENSITY-DEPENDENT ADJUSTMENT FOR
    !  THE HEIGHT-DIAMETER FUNCTION.  DELMAX IS BASED ON AVERAGE HEIGHT AND
    !  CCF AT THE BEGINNING OF THE CYCLE AND DOES NOT VARY BY SUBCYCLE.

    if (LESTB) then
       R = TEMCCF
       AH = TEMAHT
    else
       R = RELDEN
       AH = AVH
    end if
    DELMAX = (AH/36.0)*(0.01232*R-1.75)
    if (DELMAX.gt.0.0) DELMAX = 0.0
    KY = 0
    if (DEBUG) write(JOSTND,8995) R,AH,DELMAX
    8995 format('R = ',F10.4,', AH = ',F10.4,', DELMAX = ',F10.4)

    !  ENTER LOOP FOR NEXT SUBCYCLE.

    do J = 1,NPER
       KY = KY+KPER(J)
       BAJ = BANEXT(J)
       RDJ = RDNEXT(J)
       ALBA = 0.0
       if (BAJ.gt.0.0) ALBA = ALOG(BAJ)

       !  PROCESS ALL TREES WITHIN THIS SUBCYCLE IN SPECIES ORDER.  LOAD
       !  SPECIES DEPENDENT CONSTANTS.

       do ISPC = 1,MAXSP
          I1 = ISCT(ISPC,1)
          if (I1.eq.0) go to 16
          I2 = ISCT(ISPC,2)
          XRHGRO = XRHMLT(ISPC)
          XRDGRO = XRDMLT(ISPC)
          CON = 1.0
          if (LRCOR2.and. RCOR2(ISPC) .gt.0.0) CON = RCOR2(ISPC)
          CON = CON*EXP(HCOR(ISPC))
          BH = RHLH(ISPC)
          HTHS2 = HTH2(ISPC)*HT2MOD(ISPC)
          HTCRS = HTCR(ISPC)
          HTCRS2 = HTCR2(ISPC)
          BBA = RHBA(ISPC)
          BCCF = RHCCF(ISPC)
          BBAL = RHBAL(ISPC)
          RHCONS = RHCON(ISPC)
          if (ISPC .eq.11) then
             BBALMH = BBAL
             BBAL = 0.0
          else
             BBALMH = 0.0
          end if
          XMX = XMAX(ISPC)
          XMN = XMIN(ISPC)

          !  PROCESS NEXT TREE RECORD.  STORE INTERMEDIATE HEIGHTS IN WK3.

          do I3 = I1,I2

             !  BYPASS INCREMENT CALCULATION IF DBH IS GREATER THAN XMX OR IF
             !  CALL IS FROM **ESTAB** AND TREE SUBSCRIPT IS LESS THAN ITRNIN.

             I = IND1(I3)
             D = DBH(I)

             !  THE FOLLOWING CODE SEGMENT SETS DUM1 TO UNITY FOR A MANAGED STAND

             DUM1 = 0.0
             if (MANAGD .eq.1) DUM1 = 1.0
             HTPC1 = HTPCC1(ISPC)*DUM1
             IPCCF = ITRE(I)
             PCCF1 = PCCF(IPCCF)
             CR = ICR(I)/100.
             if (D.ge.XMX) go to 15
             if (LESTB .and. I.lt.ITRNIN) go to 15
             P = PROB(I)
             H1 = WK3(I)
             BAL = BAJ*(100.0-PCT(I))*0.01
             BALMH = BAJ*(100.0-PCT(I))*0.0001
             call CCFCAL(ISPC,D,H1,ICR(I),P,.false.,C1,CW,1)
             B1 = BACON*D*D

             !  COMPUTE ADJUSTMENT FOR BIAS, DENSITY, AND RELATIVE SIZE.

             RELH = (H1-4.5)/(AH-4.5)
             if (RELH.gt.1.0) RELH = 1.0
             if (RELH.lt.0.0) RELH = 0.0
             DADJ = DELMAX*RELH*RELH - 2.0*DELMAX*RELH + 0.65
             if (DEBUG)write(JOSTND,*)'CR=',CR,' BAJ=',BAJ,' BAL=',BAL,&
                  &' RHCONS=',RHCONS,' BH=',BH,' H1=',H1,' HTHS2=',HTHS2,&
                  &' BBAL=',BBAL,' BBA=',BBA,' BAJ=',BAJ,&
                  &' HTPC1=',HTPC1,' PCCF1=',PCCF1,' HTCRS=',HTCRS,&
                  &' HTCRS2=',HTCRS2,' ALBA=',ALBA

             !  COMPUTE HEIGHT INCREMENT.  STORE UPDATED HEIGHT IN WK3.

             if (ISPC .ne.11) then
                HTGRL = RHCONS+BH*H1+HTHS2*H1*H1+BBAL*BAL+BBA*ALBA+HTPC1*PCCF1&
                     &+  (HTCRS + HTCRS2*CR)*CR
                if (HTGRL .lt.0.0) HTGRL = 0.0
                H2 = H1+HTGRL*(FLOAT(KPER(J))/REGYR)*XRHGRO*CON
             else
                HTGRL = EXP(RHCONS+BH*ALOG(H1)+BCCF*RDJ+BBALMH*BALMH + HCOR(ISPC))
                if (HTGRL .lt.0.0) HTGRL = 0.0
                H2 = H1+HTGRL*(FLOAT(KPER(J))/REGYR)*XRHGRO
             end if
             if (DEBUG)write(JOSTND,*)'HTGRL=',HTGRL,' H2=',H2,' REGYR=',&
                  &REGYR,' XRHGRO=',XRHGRO,' CON=',CON
             WK3(I) = H2

             !  UPDATE DENSITY FOR NEXT SUBCYCLE.  IF ONLY ONE SUBCYCLE OR
             !  IF INITIAL DBH IS GREATER THAN 3 INCHES, BYPASS DENSITY UPDATE.
             !  DIAMETER IS ADJUSTED FOR DENSITY AND USER-SUPPLIED MULTIPLIER.
             !  INCREMENT IS DETERMINED FROM THE DIFFERENCE BETWEEN COMPUTED
             !  DIAMETERS AND IS BOUNDED TO BE GREATER THAN OR EQUAL TO ZERO.

             D2 = D
             if (J.eq.NPER.or.D.ge.3.0) go to 14
             if (H2.le.4.5) go to 14
             D1 = DIAM(ISPC) + DADJ

             !  DUB DIAMETERS BY LINEAR FUNCTION OF HEIGHT
             !  EXCEPT WHEN SPECIES IS MOUNTAIN HEMLOCK, THEN USE ORIGINAL NI
             !  EQUATION.

             if (ISPC .eq.11) then
                if (H1.gt.4.5) D1 = .0729*(H1-4.5)**1.1988 + DADJ
                D2 = .0729*(H2-4.5)**1.1988 + DADJ
             else
                if (H1.gt.4.5) D1 = HCON(ISPC)*H1+DCON(ISPC)+DADJ
                D2 = HCON(ISPC)*H2+DCON(ISPC)+DADJ
             end if
             DGJ = (D2-D1)*XRDGRO
             if (DGJ.lt.0.0) DGJ = 0.0
             D2 = D+DGJ
             if (DEBUG)write(JOSTND,*)'ISPC=',ISPC,' H1=',H1,&
                  &' D1=',D1,' H2=',H2,' D2=',D2,' DGJ',DGJ,' DADJ=',DADJ,&
                  &' D=',D,' XRDGRO=',XRDGRO

             !  COMPUTE ADJUSTMENT TO STAND BASAL AREA AND CCF FOR NEXT SUBCYCLE.
             !  ASSUME A 1.5 PERCENT ANNUAL MORTALITY RATE.

             call CCFCAL(ISPC,D2,H2,ICR(I),P,.false.,C2,CW,1)
             RDNEXT(J+1) = RDNEXT(J+1)+KY*(C2-C1)/10.*0.985**KY
             BANEXT(J+1) = BANEXT(J+1)+(BACON*D2*D2-B1)*P*0.985**KY
             if (DEBUG) write(JOSTND,8997) I,D2,BANEXT(J+1),RDNEXT(J+1)
             8997 format(' PDATE DENSITY: TREE = ',I4,', D2 = ',F8.4,&
                  &', BANEXT(J+1) = ',F8.2,', RDNEXT(J+1) = ',F8.2)

             !  END OF TREE LOOP WITHIN SPECIES. PRINT DEBUG IF REQUESTED.

             14 continue
             if (.not.DEBUG) go to 15
             write(JOSTND,8999) J,KPER(J),ISPC,I,H1,H2,RDJ,BBAL,BAL,&
                  &BAJ,PCT(I)
             8999 format('IN REGENT L1: SUBC=',I2,', KPER=',I2,', SP=',I2,&
                  &', I=',I4,', H1=',F8.4,', H2=',F8.4,/T4,'RDJ=',&
                  &F8.4,', BBAL=',F8.4,', BAL=',F8.4,', BAJ=',F8.4,', PCT=',F8.4)
             15 continue
          end do

          !  END OF SPECIES LOOP WITHIN SUBCYCLE

          16 continue
       end do

       !  END OF SUBCYCLE LOOP.  END OF PERIOD HEIGHT IS STORED IN WK3.

       17 continue
    end do

    !  AFTER LAST SUBCYCLE, COMPUTE ACTUAL HEIGHT INCREMENT FOR FULL CYCLE BY
    !  SUBTRACTION; CONVERT BACK TO LOG SCALE TO ADD IN RANDOM ERROR.

    do ISPC = 1,MAXSP
       I1 = ISCT(ISPC,1)
       if (I1.eq.0) go to 30
       I2 = ISCT(ISPC,2)
       XRDGRO = XRDMLT(ISPC)
       XMX = XMAX(ISPC)
       XMN = XMIN(ISPC)
       do I3 = I1,I2
          I = IND1(I3)
          D = DBH(I)
          if (D.ge.XMX) go to 25
          if (LESTB .and. I.lt.ITRNIN) go to 25
          K = I
          L = 0
          H = HT(I)
          CR = FLOAT(ICR(I))/100.
          HK = WK3(I)

          !  IF CALLED FROM **ESTAB** AND TOTAL CYCLE LENGTH IS LESS THAN OR EQUAL
          !  TO 5 YEARS, SKIP HEIGHT INCREMENT CALCULATION BUT ASSIGN CR AND DBH.

          if (LSKIPH) then
             HTG(K) = 0
             go to 20
          end if
          HTGR1 = HK-H
          if (HTGR1.lt.0.0) HTGR1 = 0.0

          !  DRAW RANDOM INCREMENT GIVEN EXPECTED VALUE, HTGR, AND STD. DEV.,
          !  HSIGMA.  BOUND DRAW TO (-1.5*SD < ZZRAN < 1.0*SD); IF TRIPLING,
          !  EACH TRIPLE GETS A NEW RANDOM NUMBER.  DO NOT TRIPLE IF CALLING
          !  FROM **ESTAB**.  DO NOT APPLY HEIGHT GROWTH MULTIPLIER; MULTIPLIER
          !  WAS USED DURING SUBCYCLING.

          18 continue
          ZZRAN = 0.0
          if (DGSD.ge.1.0) ZZRAN = BACHLO(0.0,1.0,RANN)
          if ((ZZRAN .gt.1.0) .or. (ZZRAN .lt. -1.5)) go to 18
          HTGR = HTGR1+(ZZRAN*HSIGMA)
          if (HTGR .lt..15)HTGR = .15

          !  COMPUTE WEIGHTS FOR THE LARGE AND SMALL TREE HEIGHT INCREMENT
          !  ESTIMATES.  IF DBH IS LESS THAN OR EQUAL TO XMN OR IF CALLED
          !  FROM **ESTAB**, THE LARGE TREE PREDICTION IS IGNORED (XWT=0.0).

          XWT = 0.0
          if (D.le.XMN.or.LESTB) go to 19
          XWT = (D-XMN)/(XMX-XMN)
          19 continue
          HTG(K) = HTGR*(1.0-XWT) + XWT*HTG(K)
          if (DEBUG)write(JOSTND,*)'XMN=',XMN,' K=',K,' XMX=',XMX,&
               &' HTGR=',HTGR,' XWT=',XWT,' HTG=',HTG(K)

          ! CHECK FOR SIZE CAP COMPLIANCE.

          if ((H+HTG(K)).gt.SIZCAP(ISPC,4)) then
             HTG(K) = SIZCAP(ISPC,4)-H
             if (HTG(K) .lt.0.1) HTG(K) = 0.1
          end if

          20 continue

          !  ASSIGN DBH AND COMPUTE DBH INCREMENT FOR TREES WITH DBH LESS
          !  THAN 3 INCHES.  COMPUTE 10-YEAR DBH INCREMENT REGARDLESS OF
          !  PROJECTION PERIOD LENGTH.  BYPASS INCREMENT CALCULATION IF
          !  D GE 3.0 INCHES.

          if (D.ge.3.0) go to 23

          !  COMPUTE ADJUSTMENT FOR BIAS, DENSITY, AND RELATIVE SIZE.

          RELH = (H-4.5)/(AH-4.5)
          if (RELH.gt.1.0) RELH = 1.0
          if (RELH.lt.0.0) RELH = 0.0
          DADJ = DELMAX*RELH*RELH - 2.0*DELMAX*RELH + 0.65
          D1 = DIAM(ISPC) + DADJ

          !  DUB DIAMETERS BY LINEAR FUNCTION OF HEIGHT
          !  EXCEPT WHEN SPECIES IS MOUNTAIN HEMLOCK, THEN USE ORIGINAL
          !  EQUATION.

          if (ISPC .eq.11) then
             if (H .gt.4.5) D1 = .0729*(H-4.5)**1.1988 + DADJ
          else
             if (H .gt.4.5) D1 = HCON(ISPC)*H+DCON(ISPC)+DADJ
          end if
          if (DEBUG)write(JOSTND,*)'ISPC=',ISPC,' I=',I,' D1=',D1
          HK = H+HTG(K)
          if (HK .lt.4.5) then
             DBH(K) = 0.1 + DIAM(ISPC)*.01 + HK*0.001
             DG(K) = 0.0
          else

             !  DUB IN DIAMETERS WITH LINEAR FUNCTION OF HEIGHT.
             !  USE ORIGINAL NI EQUATION FOR MOUNTAIN HEMLOCK.

             if (ISPC .eq.11) then
                DK = .0729*(HK-4.5)**1.1988+DADJ
             else
                DK = HCON(ISPC)*HK+DCON(ISPC)+DADJ
             end if
             if (DK.lt.DIAM(ISPC)) DK = DIAM(ISPC)
             DK = DK+HK*0.001
             if (DEBUG)write(JOSTND,*)'ISPC=',ISPC,' HK=',HK,&
                  &' DK=',DK

             !       IF CALLING FROM **ESTAB** ASSIGN DIAMETER AND DIAMETER
             !       INCREMENT.

             if (LESTB) then
                DBH(K) = DK
                DG(K) = DK
             else

                !         DIAMETER INCREMENT IS THE DIFFERENCE BETWEEN COMPUTED
                !         DIAMETERS.  MULTIPLIER IS APPLIED TO DIAMETER INCREMENT
                !         AT THIS POINT.

                DGK = (DK-D1)*XRDGRO
                if (DGK.lt.0.0) DGK = 0.0

                !         SCALE DIAMETER INCREMENT FOR BARK AND PERIOD LENGTH.
                !         IN ORDER TO MAINTAIN CONSISTENCY WITH **GRADD**,
                !         ADJUSTMENTS ARE MADE ON THE DDS SCALE.

                BARK = BRATIO(ISPC,DBH(K),HT(K))
                DG(K) = DGK*BARK
                DDS = DG(K)*(2.0*BARK*D+DG(K))*SCALE
                DG(K) = SQRT((D*BARK)**2+DDS)-BARK*D
             end if
             if ((DBH(K)+DG(K)).lt.DIAM(ISPC)) then
                DG(K) = DIAM(ISPC)-DBH(K)
             end if
          end if

          !  CHECK FOR TREE SIZE CAP COMPLIANCE

          call DGBND(ISPC,DBH(K),DG(K))

          !  IF THE TREE WILL ATTAIN A DBH OF 3 OR MORE INCHES THIS CYCLE
          !  CALL **DUBSCR** TO ESTIMATE A NEW CROWN RATIO.
          !  DIAMETER GROWTH SCALED TO CYCLE LENGTH FOR THIS CALCULATION.
          !  MAKE SURE NEW CROWN DOES NOT EXCEED MAX POSSIBLE GIVEN HTG.

          if (.not.LESTB) then
             SCALE2 = FINT/YR
             D = DBH(K)
             BARK = BRATIO(ISPC,D,HT(K))
             DDS2 = (DG(K)*(2.0*BARK*D+DG(K)))*SCALE2
             DG2 = SQRT((D*BARK)**2+DDS2)-BARK*D
             if (DG2.lt.0.0) DG2 = 0.0
             DNEW = D+DG2
             if (DNEW.ge.3.0) then
                call DUBSCR(ISPC,DNEW,HK,BA,CR)
                TEMCR = CR*100.0+0.5

                ! CALC CROWN LENGTH NOW

                if (ICR(K).eq.0)go to 53
                CRLN = HT(K)*ICR(K)/100.

                ! CALC CROWN LENGTH MAX POSSIBLE IF ALL HTG GOES TO NEW CROWN

                CRMAX = (CRLN+HTG(K))/(HT(K)+HTG(K))*100.0
                if (DEBUG)write(JOSTND,*)'K,HT,HTG,ICR,CRLN,CRMAX= ',&
                     &K,HT(K),HTG(K),ICR(K),CRLN,CRMAX

                ! IF NEW CROWN EXCEEDS MAX POSSIBLE LIMIT IT TO MAX POSSIBLE

                if (TEMCR.gt.CRMAX) TEMCR = CRMAX

                53 continue
                ICR(K) = INT(TEMCR)
                IF(DEBUG)WRITE(JOSTND,*)'AFTER DUBSCR K,ISPC,DNEW,HK,BA,CR,IC &
                     &    R= ',K,ISPC,DNEW,HK,BA,CR,ICR(K)
             end if
          end if
          23 continue

          !  PRINT DEBUG AND RETURN TO PROCESS NEXT TRIPLE OR NEXT TREE.

          if (.not.DEBUG) go to 24
          write(JOSTND,9000) I,K,ISPC,H,HTG(K),HK,DBH(K),DG(K)
          9000 format('IN REGENT LOOP 2, I=',I4,', K=',I4,', ISPC=',I3,&
               &', CUR HT=',F7.2,', HT INC=',F7.4/T19,'NEW HT=',F7.2,&
               &', CUR DBH=',F7.2,', 10-YR DBH INC=',F7.4)
          24 continue
          if (LESTB .or..not.LTRIP .or. L.ge.2) go to 25
          L = L+1
          K = ITRN+2*I-2+L
          go to 18

          !  END OF GROWTH PREDICTION LOOP.  PRINT DEBUG INFO IF DESIRED.

          25 continue
       end do
       30 continue
    end do
    return

    !  SMALL TREE HEIGHT CALIBRATION SECTION.

    40 continue
    do ISPC = 1,MAXSP
       HCOR(ISPC) = 0.0
       CORTEM(ISPC) = 1.0
       NUMCAL(ISPC) = 0
       45 continue
    end do
    if (ITRN.le.0) return
    if (IFINTH .eq.0)  goto 100
    if (DEBUG)write(JOSTND,*)'FINTH=',FINTH
    SCALE3 = REGYR/FINTH

    !  DETERMINE BASAL AREA IN LARGE TREES AT START OF EACH PREDICTION SUBCYCLE.

    do J = 1,NPER
       BANEXT(J) = TEMBA
       RDNEXT(J) = TEMCCF
       46 continue
    end do
    if (NPER.eq.1) go to 50

    !  IF THERE IS MORE THAN 1 SUBCYCLE FOR HEIGHT INCREMENT PREDICTION,
    !  (IFINTH > 5) ESTIMATE VALUES OF BA AND RELATIVE DENSITY (CCF) AT
    !  START OF EACH SUBCYCLE.  10-YEAR CHANGE IS COMPUTED FROM BACKDATED
    !  DIAMETER (WK3) AND 10-YEAR DIAMETER INCREMENT COMPUTED DURING CALIBRATION.
    !  LINEAR INTERPOLATION IS USED TO ASSIGN VALUES FOR SUBCYCLES.

    do I = 1,ITRN
       D1 = WK3(I)
       IS = ISP(I)
       P = PROB(I)
       D2 = WK3(I)+DG(I)/BRATIO(IS,D1,HT(I))
       B1 = BACON*D1*D1
       B2 = BACON*D2*D2
       call CCFCAL(IS,D1,HT(I),ICR(I),P,.false.,C1,CW,1)
       call CCFCAL(IS,D2,HT(I),ICR(I),P,.false.,C2,CW,1)
       BI = (B2-B1)/10.0
       CI = (C2-C1)/10.0
       K = 0
       do J = 2,NPER
          K = K+KPER(J-1)
          PN = P*0.985**K
          RDNEXT(J) = RDNEXT(J)+K*CI/P*PN
          BANEXT(J) = BANEXT(J)+K*BI*PN
          48 continue
       end do
       49 continue
    end do
    50 continue
    do ISPC = 1,MAXSP

       !  BEGIN PROCESSING TREE LIST IN SPECIES ORDER.  DO NOT CALCULATE
       !  CORRECTION TERMS IF THERE ARE NO TREES FOR THIS SPECIES.

       ALBA = 0.0
       CORNEW = 1.0
       I1 = ISCT(ISPC,1)
       if (I1.eq.0.or..not. LHTCAL(ISPC)) go to 90
       I2 = ISCT(ISPC,2)
       IREFI = IREF(ISPC)
       N = 0
       SNP = 0.0
       SNX = 0.0
       SNY = 0.0
       BHAB = RHCON(ISPC)
       BLH = RHLH(ISPC)
       BCCF = RHCCF(ISPC)
       HTHS2 = HTH2(ISPC)*HT2MOD(ISPC)
       BBA = RHBA(ISPC)
       HTCRS = HTCR(ISPC)
       HTCRS2 = HTCR2(ISPC)
       BBAL = RHBAL(ISPC)
       if (ISPC .eq.11) then
          BBALMH = BBAL
          BBAL = 0.0
       else
          BBALMH = 0.0
       end if
       CON = 1.0
       if (LRCOR2.and. RCOR2(ISPC) .gt.0.0) CON = RCOR2(ISPC)

       !  BEGIN TREE LOOP WITHIN SPECIES.  IF MEASURED HEIGHT INCREMENT IS
       !  LESS THAN OR EQUAL TO ZERO, OR DBH IS GREATER THAN 5.0, THE RECORD
       !  WILL BE EXCLUDED FROM THE CALIBRATION.

       do I3 = I1,I2
          I = IND1(I3)
          H = HT(I)

          !  THE FOLLOWING TWO LINES SETS DUM1 TO UNITY FOR A MANAGED STAND.

          DUM1 = 0.0
          if (MANAGD .eq.1) DUM1 = 1.0
          HTPC1 = HTPCC1(ISPC)*DUM1
          IPCCF = ITRE(I)
          PCCF1 = PCCF(IPCCF)
          CR = ICR(I)/100.
          if (IHTG.lt.2) H = H-HTG(I)
          if (DBH(I).ge.5.0.or.H.lt.0.01) go to 60
          if (HTG(I).lt.0.001) go to 60
          HK = H
          do J = 1,NPER
             BAJ = BANEXT(J)
             RDJ = RDNEXT(J)
             if (BAJ .gt.0.0) ALBA = ALOG(BAJ)
             BAL = BAJ*(100.0-PCT(I))*0.01
             BALMH = BAJ*(100.0-PCT(I))*0.0001
             if (ISPC .ne.11) then
                EDH = BHAB + BLH*HK + HTHS2*HK*HK + BBA*ALBA + BBAL*BAL&
                     &+ HTPC1*PCCF1+(HTCRS+HTCRS2*CR)*CR
                if (EDH .lt.0.0) EDH = 0.0
                HK = HK+EDH*CON
             else
                EDH = EXP(BHAB+BLH*ALOG(HK)+BCCF*RDJ+BBALMH*BALMH)
                if (DEBUG)write(JOSTND,*)'BALMH=',BALMH,' RDJ=',RDJ
                if (EDH .lt.0.0) EDH = 0.0
                HK = HK+EDH
             end if
             if (DEBUG)write(JOSTND,*)'BHAB=',BHAB,' BLH=',BLH,' HTHS2=',&
                  &HTHS2,' PCCF1=',PCCF1,' BBA=',BBA,' BBAL=',BBAL,' HTPC1=',&
                  &HTPC1,' HTCRS=',HTCRS,' HTCRS2',HTCRS2,' BCCF',BCCF,' EDH=',&
                  &EDH,' HK=',HK,' BAJ=',BAJ,' ht=',HT(I),' BAJ=',BAJ,' BAL=',&
                  &BAL,' PCT(I)=',PCT(I)
             55 continue
          end do
          EDH = HK-H
          P = PROB(I)
          TERM = HTG(I)*SCALE3
          SNP = SNP+P
          SNX = SNX+EDH*P
          SNY = SNY+TERM*P
          N = N+1

          !  PRINT DEBUG INFO IF DESIRED.

          if (DEBUG) write(JOSTND,9001) NPLT,I,ISPC,H,DBH(I),ICR(I),&
               &PCT(I),TEMCCF,RHCON(ISPC),EDH,TERM
          9001 format('NPLT=',A26,',  I=',I5,',  ISPC=',I3,',  H=',F6.1,&
               &',  DBH=',F5.1,',  ICR',I5,',  PCT=',F6.1,',  RELDEN=',&
               &F6.1 / 12X,'RHCON=',F10.3,',  EDH=',F10.3,', TERM=',F10.3)

          !  END OF TREE LOOP WITHIN SPECIES.

          60 continue
       end do
       if (DEBUG) write(JOSTND,9010) ISPC,SNP,SNX,SNY
       9010 format(/'SUMS FOR SPECIES ',I2,':  SNP=',F10.2,&
            &';   SNX=',F10.2,';   SNY=',F10.2)

       !  COMPUTE CALIBRATION TERMS.  CALIBRATION TERMS ARE NOT COMPUTED
       !  IF THERE WERE FEWER THAN NCALHT (DEFAULT=5) HEIGHT INCREMENT
       !  OBSERVATIONS FOR A SPECIES.

       if (N.lt.NCALHT) go to 80

       !  CALCULATE MEANS FOR THE FOR THE GROWTH SAMPLE ON THE
       !  NATURAL SCALE.

       call REGHCAL(ISPC,SNP,SNX,SNY,CORNEW)
       80 continue
       CORTEM(IREFI) = CORNEW
       NUMCAL(IREFI) = N
       90 continue
    end do

    !  END OF CALIBRATION LOOP.  PRINT CALIBRATION STATISTICS AND RETURN

    write(JOSTND,9002) (NUMCAL(I),I = 1,NUMSP)
    9002 format(/'NUMBER OF RECORDS AVAILABLE FOR SCALING'/&
         &'THE SMALL TREE HEIGHT INCREMENT MODEL',&
         &((T48,11(I4,2X)/)))
    100 continue
    write(JOSTND,9003) (CORTEM(I),I = 1,NUMSP)
    9003 format(/'INITIAL SCALE FACTORS FOR THE SMALL TREE'/&
         &'HEIGHT INCREMENT MODEL',&
         &((T48,11(F5.2,1X)/)))

    ! OUTPUT CALIBRATION TO DATABASE.

    call REGCALOUT(CORTEM,NUMCAL,KOUT,K,KK,ISPEC,SPEC)
    return
    9998 continue

    !  ENTRY POINT FOR LOADING OF REGENERATION GROWTH MODEL
    !  CONSTANTS  THAT REQUIRE ONE-TIME RESOLUTION.
    !  IDTYPE IS A HABITAT INDEX THAT IS COMPUTED IN **RCON**.
    !  RHSC IS AN ARRAY FOR SPECIES CONSTANTS, RHGL IS AN ARRAY FOR
    !  LOCATION CONSTANTS, RHHAB IS AN ARRAY FOR HABITAT DEPENDENT
    !  CONSTANTS, AND, HTSLOP, HTCASP, AND HTSASP ARE COEFFICIENTS FOR
    !  THE SLOPE AND ASPECT TRANSFORMATION.

    data RHHAB/&
         &0.0,-0.288729, .222331,     0.0,     0.0,     0.0,&
         &0.0, 0.095681,     0.0,     0.0,     0.0,     0.0,&
         &0.0, 0.259585, .143218, .107012,-.136078,     0.0,&
         &0.0, 0.111443, .182470,     0.0,     0.0,     0.0,&
         &0.0, 0.128458,-.115672, .271298,     0.0,     0.0,&
         &0.0,-0.107416,     0.0,     0.0,     0.0,     0.0,&
         &0.0,-0.637727,-.808173,-.396110, .666094,     0.0,&
         &0.0, 0.245211, .726173, .341746, .553888, .427478,&
         &0.0, 0.252628, .326399, .401200, .568246, .785996,&
         &0.0,-0.121653,-.374652,     0.0,     0.0,      0.0,&
         &-0.2146, -0.0941, -0.3738,     0.0,     0.0,  0.0/
    data MAPHAB/&
         &23*1,2,3*1,4*2,1,2*2,1,2,1,4*2,1,2*2,1,2,1,3*2,3*1,2,41*1,&
         &2*3,13*1,3,2*1,3*3,3*1,3,9*1,3,2*1,3*3,4*1,3,1,3,2*1,3,&
         &1,3,1,2*3,2*1,2*3,6*1,2,14*1,&
         &11*1,2,1,2,2*1,2,6*1,2*2,1,18*2,1,10*2,1,2,2*1,5*2,1,2,1,&
         &2*2,2*1,2*2,1,30*2,1,12*2,1,3*2,2*1,2*2,1,2*2,1,6*2,1,&
         &6*2,2*1,2,1,9*2,5*1,2,1,2,2*1,3*2,5*1,2,1,&
         &11*1,2,1,2,2*1,2,2*1,2,3*1,2,3,1,2*2,3,5,4,3,2*4,3,4,2*3,&
         &2*4,2*3,4,5,1,4,3,2*4,2,3,4,3,4,2,4*1,4,3*3,2*1,3,1,2*3,&
         &1,2*3,2,3,2,2*3,2,2*3,2*4,2*3,4,2,4,2,2*4,2,2*3,4,2*2,&
         &3,4*2,1,2*2,1,3,4,3,4,2*3,4,3,4,3*3,1,3,1,3,3*1,3,6*1,&
         &3,1,3,2*1,3,1,3*3,2*4,1,3,1,2*3,4,3*1,3,13*1,3,8*1,&
         &23*1,2,2*1,8*2,1,6*2,1,2,2*1,5*2,20*1,3,1,2,1,10*3,1,3,1,&
         &3*3,4*1,7*2,1,2*2,2*1,2*3,2*1,6*3,4*1,3,8*1,5*3,4*1,4*3,&
         &2*1,3,1,3*3,3*1,3,7*1,3,14*1,&
         &71*1,4,2,3*1,4,2*1,2*4,2*1,4,1,5*2,4,2,1,4,7*2,1,3*2,4*1,3,&
         &2*1,3,1,3,1,3,1,2*3,3*1,3,7*1,3*3,41*1,&
         &137*1,2,1,3*2,2*1,2,2*1,3*2,2*1,2*2,6*1,2,14*1,&
         &13*1,2,2*1,2,6*1,3,2,1,2*2,4,2*3,6*2,3,4*2,2*3,1,4,3*2,4,3,1,&
         &3,2*2,1,2,2*1,5,4,2*3,2,1,4*2,4,1,3*4,1,4,2,4,1,3*4,2,4,1,&
         &3*4,2,2*4,2,1,4,2,4,2,2*4,2*2,1,2*2,2*1,2*4,2,2*5,3,2*2,3,&
         &4,5,2*1,5,3,2*1,2,3,2,3,2,1,4,3*3,2,3*1,2*4,3,2*2,3,1,4,5,&
         &2,2*3,2*1,2*2,4,2,5*1,2,2*1,2,3,9*1,5,&
         &11*1,2,15*1,14*2,1,2*2,1,4*2,10*1,5,4*2,1,2,1,2,5,2*1,4,3,3*1,&
         &3,2*1,3,4,3,2*4,1,5*4,5*3,4,2,3,2,4,3,2,2*4,2*1,2*3,4,3,3*4,&
         &2*6,5,4,1,3,2*4,2*1,3,2*1,3,4,1,6,4,6,4,6,3*4,3,6,2*5,6,5,1,&
         &4,3,2*4,6,2*4,1,2*5,6*1,4,1,6,2*1,5,2*6,7*1,&
         &27*1,4*2,4,9*2,1,2*2,1,6*2,3*1,2,1,2,2*1,4*2,2*1,2,1,2*2,2*1,4,&
         &3,3*1,4,2*1,2*4,6,4,2*1,4*4,3*5,1,5,4,3,4,3,4,3,4,3*3,2*1,&
         &3*2,6,5,4,5,2*3,2*5,1,6,3,5,2*1,6,2,2*1,3,1,2,2*3,2,2*3,5,&
         &7*3,1,6,1,2*2,3,2*2,1,2*3,6*1,2,3*1,6,5,6,3,5*1,3,1,&
         &13*1,2,1,2*2,2*1,2,2*1,2,2*3,1,3,2,1,2*3,5*2,2*1,6*2,2*1,3,2,1,&
         &3,2,2*1,2*2,15*1,2*3,2,3,1,3*3,1,5*2,1,3*3,5*1,3,1,4*2,3*1,2,&
         &72*1,&
         &79*3,6*1,8*2,17*4,10*1,13*3,5*2,37*3/
    data  RHSC/&
         &10*0.0,.8953/
    !    &  1.5772381,2.3031712,2.3698511,1.3920336,1.7477264,1.0443697,
    !    &  3.5429926,.87052488,1.0984526,1.5626745,.8953  /
    data RHGL/&
         &-.2785,-.0480,0.0/
    data RSAB0,RSAB1,RSAB2/&
         &-.010987,0.22157,-0.12432/

    !  HTFOR CONTAINS LOCATION CLASS CONSTANTS FOR EACH SPECIES.  MAPLOC
    !  IS AN ARRAY WHICH MAPS GEOGRAPHIC AREA ONTO A LOCATION CLASS.

    data MAPLOC/&
         &1,2,1,2,2,2,1,3,3,3,&
         &1,1,2,1,2,2,3,1,3,4,&
         &1,2,2,2,2,1,3,3,3,3,&
         &1,2,1,2,1,1,1,3,2,3,&
         &1,2,2,1,1,2,1,1,2,1,&
         &1,1,2,1,1,2,1,3,1,2,&
         &1,1,2,3,1,1,1,2,2,3,&
         &1,2,2,2,5,4,3,1,5,1,&
         &1,2,2,2,4,3,4,4,4,4,&
         &1,2,1,3,2,1,1,3,2,2,&
         &1,1,1,1,1,1,1,1,1,1/
    data HTFOR/&
         &2.269835,2.106430,2.643390,     0.0,     0.0,&
         &2.887315,2.138032,2.577398,3.316322,     0.0,&
         &2.776090,2.731853,2.898513,     0.0,     0.0,&
         &1.508043,1.336607,1.653463,     0.0,     0.0,&
         &1.856635,1.882195,     0.0,     0.0,     0.0,&
         &1.102706,1.227212,1.508519,     0.0,     0.0,&
         &3.900741,3.814237,4.274753,     0.0,     0.0,&
         &1.605062,1.395650,2.304788,2.051997,1.297702,&
         &1.401828,1.625757,1.993350,1.668620,     0.0,&
         &1.370572,1.604815,1.083899,     0.0,     0.0,&
         &0.0,     0.0,     0.0,     0.0,     0.0/

    !  HTEL CONTAINS THE COEFFICIENTS FOR THE ELEVATION TERM IN THE
    !  HEIGHT GROWTH EQUATION.
    !  HTSASP CONTAINS THE COEFFICIENTS FOR THE SIN(ASPECT)*SLOPE TERM
    !  IN THE HEIGHT GROWTH EQUATION.  HTCASP CONTAINS THE COEFFICIENTS
    !  FOR THE COS(ASPECT)*SLOPE TERM IN THE HEIGHT GROWTH EQUATION.
    !  HTSLOP CONTAINS THE COEFFICIENTS FOR THE SLOPE TERM IN THE
    !  HEIGHT GROWTH EQUATION. HTSLSQ CONTAINS THE COEFFICIENTS FOR THE
    !  (SLOPE)**2 TERM IN THE HEIGHT GROWTH MODEL.  ALL OF THESE ARRAYS
    !  ARE SUBSCRIPTED BY SPECIES.

    data HTSLOP/&
         &-.739841,-1.150486,-.749560,-.730825,.047743,-.034439,&
         &-.171346,-.140742,.459091,-1.923583,.000000/
    data HTSLSQ/&
         &-.347447,.000000,.726317,.755344,.000000,.000000,-1.705130,&
         &.000000,-1.015800,.000000,.000000/
    data HTEL/&
         &-.017447,-.001659,-.009145,-.009281,-.008651,.001325,-.007956,&
         &-.006651,-.005558,.005946,.000000/
    data HTCASP/&
         &-.319768,.506689,.025470,-.019985,-.200859,-.148969,&
         &.238473,-.174545,.047339,-.562835,.00000/
    data HTSASP/&
         &-.226560,-.019319,-.061316,-.002931,.035112,-.086932,.171464,&
         &-.083216,-.016025,.567420,-.00000/

    !  LOAD SPECIES DEPENDENT VARIABLES AND OVERALL SITE CONSTANTS INTO
    !  RHCON BY SPECIES.  IF SCALE FACTORS HAVE BEEN INPUT WITH THE
    !  READCORR KEYWORD, INCORPERATE THEM INTO THE OVERALL CONSTANTS.

    do ISPC = 1,MAXSP
       IRHHAB = MAPHAB(KKTYPE,ISPC)
       IRHFOR = MAPLOC(KOTFOR,ISPC)

       !  FOR MOUNTAIN HEMLOCK, SPECIES 11, COMBINE SITE EFFECTS THAT ARE
       !  INDEPENDENT OF SPECIES.

       if (ISPC .eq.11) then
          REGCH = RHGL(IGL)+(RSAB0+RSAB1*COS(ASPECT)+RSAB2*SIN(ASPECT))&
               &*SLOPE
          RHCON(ISPC) = REGCH + RHSC(ISPC) +RHHAB(IRHHAB,ISPC)
          if (LRCOR2.and.RCOR2(ISPC).gt.0.0)RHCON(ISPC) = RHCON(ISPC) +&
               &ALOG(RCOR2(ISPC))
       else
          REGCH = 0.0
          RHCON(ISPC) = HTFOR(IRHFOR,ISPC)&
               &+ (HTSLOP(ISPC) + HTSLSQ(ISPC)*SLOPE + HTCASP(ISPC)*COS(ASPECT)&
               &+ HTSASP(ISPC)*SIN(ASPECT))*SLOPE&
               &+ HTEL(ISPC)*ELEV
          RHCON(ISPC) = RHCON(ISPC) + RHSC(ISPC) + RHHAB(IRHHAB,ISPC)
       end if
       110 continue
    end do
    return
  end subroutine REGKT

  !===============================!
  !   Compute small tree growth   !
  !===============================!
  ! NC $Id$
  subroutine REGNC(LCON)
    implicit none

    logical :: LCON

    !  **REGENT** COMPUTES HEIGHT AND DIAMETER INCREMENTS FOR SMALL
    !  TREES.  THE HEIGHT INCREMENT MODEL AND DBH INCREMENT MODEL ARE
    !  APPLIED TO TREES THAT ARE LESS THAN  5 INCHES DBH. FOR TREES
    !  GREATER THAN 2 INCHES DBH, HEIGHT AND DBH INCREMENT PREDICTIONS
    !  ARE AVERAGED WITH PREDICTIONS FROM THE LARGE TREE MODEL.

    !  **REGENT** IS CALLED FROM **CRATET** DURING CALIBRATION AND
    !  FROM **GRINCR** DURING CYCLING.  ENTRY **REGCON** IS CALLED FROM
    !  **RCON** TO LOAD MODEL PARAMETERS THAT NEED ONLY BE RESOLVED ONCE.

    !  **REGENT** IS ALSO USED TO PREDICT HEIGHT INCREMENT, DBH AND CROWN
    !  RATIO FOR TREES THAT ARE CREATED BY THE REGENERATION ESTABLISHMENT
    !  MODEL.  5-YEAR OLD TREES ARE GENERATED BY **ESTAB** AND **REGENT**
    !  COMPUTES INCREMENTS FROM AGE 5 TO THE END OF THE CYCLE.

    ! Common blocks

    !  DIMENSIONS FOR INTERNAL VARIABLES:

    !   CORTEM -- A TEMPORARY ARRAY FOR PRINTING CORRECTION TERMS.
    !   NUMCAL -- A TEMPORARY ARRAY FOR PRINTING NUMBER OF HEIGHT
    !             INCREMENT OBSERVATIONS BY SPECIES.
    !      HT1 -- SPECIES DEPENDENT CONSTANT IN THE HEIGHT-DBH MODEL USED
    !             TO ASSIGN DIAMETERS TO SMALL TREES.
    !      HT2 -- SPECIES DEPENDENT EXPONENT IN THE HEIGHT-DBH MODEL USED
    !             TO ASSIGN DIAMETERS TO SMALL TREES.
    !     XMAX -- UPPER END OF THE RANGE OF DIAMETERS OVER WHICH HEIGHT
    !             INCREMENT PREDICTIONS FROM SMALL AND LARGE TREE MODELS
    !             ARE AVERAGED. ALSO DBH INCREMENT.
    !     XMIN -- LOWER END OF THE RANGE OF DIAMETERS OVER WHICH HEIGHT
    !             INCREMENT PREDICTIONS FROM THE SMALL AND LARGE TREE
    !             ARE AVERAGED. ALSO DBH INCREMENT.

    external RANN
    ! Variables
    logical :: DEBUG,LSKIPH
    real    :: CORTEM(12)
    integer :: NUMCAL(12),K,L
    real    :: XMAX(12),XMIN(12),DIAM(12)
    real    :: P,EDH,XHTGR,SNY,SNX,SSITE,DDS,SCALE,SCALE2,SCALE3,FNT
    integer :: ISPEC,KK,KOUT,N,IREFI,I3,I2,I1,ISPC,I,IPCCF
    real    :: REGYR,CR,RAN,BACHLO,XRHGRO,XRDGRO,CON,XMX,XMN,BX,AX,XSITE
    real    :: D,H,XBA,RELHT,TPCCF,HTGR,XWT,HK,DK,BARK
    real    :: BRATIO,CORNEW,SNP,TERM,DKK
    character(len=2) :: SPEC
    real    :: DGSM,XDWT,DGLT,LTHG,HTGR2
    real    :: DGMIN(12)

    ! Data statements
    data DIAM/0.3, 0.4, 2*0.3, 4*0.2, 0.3, 0.5, 0.2, 0.3/
    data XMAX/ 11*5.0, 10.0 /,  XMIN/ 12*2.0 /
    data DGMIN/11*3.0, 7.0/
    data REGYR/5.0/

    if (LCON) go to 9998

    !  CHECK FOR DEBUG.

    LSKIPH = .false.
    call DBCHK (DEBUG,'REGENT',6,ICYC)

    !  THE SMALL TREE HEIGHT INCREMENT MODEL IS BASED ON 5-YEAR GROWTH DATA.

    FNT = FINT
    if (LSTART) FNT = IFINTH
    if (LESTB) then
       if (FINT.le.5.0) then
          LSKIPH = .true.
       else
          FNT = FNT-5.0
       end if
    end if

    !  IF THIS IS THE FIRST ENTRY, BRANCH TO THE CALIBRATION SECTION.

    if (LSTART) goto 40

    !  LOAD MULTIPLIERS.

    call MULTS (3,IY(ICYC),XRHMLT)
    call MULTS(6,IY(ICYC),XRDMLT)
    SCALE = FNT/REGYR
    SCALE2 = YR/FNT

    if (DEBUG)write(JOSTND,8996)LESTB,LSKIPH,ITRN,ITRNIN,SCALE,SCALE2
    8996 format('IN REGENT, LESTB = ',L1,', LSKIPH = ',L1,&
         &', ITRN = ',I4,', ITRNIN = ',I4,', SCALE = ',F6.3,&
         &', SCALE2 = ',F6.3)
    if (ITRN.le.0) return

    !  STORE INITIAL HEIGHTS IN WK3; DUB CROWN RATIO FOR NEWLY ESTABLISHED
    !  SEEDLINGS.

    do I = 1,ITRN
       if (LESTB.and.I.ge.ITRNIN) then
          IPCCF = ITRE(I)
          CR = 0.89722 - 0.0000461*PCCF(IPCCF)
          12 continue
          RAN = BACHLO(0.0,1.0,RANN)
          if (RAN .lt. -1.0.or. RAN .gt.1.0) go to 12
          CR = CR + 0.07985 * RAN
          if (CR .gt..90) CR = .90
          if (CR .lt..20) CR = .20
          ICR(I) = INT((CR*100.0)+0.5)
       end if
       WK3(I) = HT(I)
       13 continue
    end do

    !  ENTER GROWTH PREDICTION LOOP.  TREES ARE PROCESSED ONE AT A TIME.

    do ISPC = 1,MAXSP
       I1 = ISCT(ISPC,1)
       if (I1.eq.0) go to 30
       I2 = ISCT(ISPC,2)
       XRHGRO = XRHMLT(ISPC)*SCALE
       XRDGRO = XRDMLT(ISPC)
       CON = RHCON(ISPC)*EXP(HCOR(ISPC))
       XMX = XMAX(ISPC)
       XMN = XMIN(ISPC)
       BX = HT2(ISPC)
       if (IABFLG(ISPC) .eq.1) then
          AX = HT1(ISPC)
       else
          AX = AA(ISPC)
       end if
       XSITE = SITEAR(ISPC)
       do I3 = I1,I2
          I = IND1(I3)
          D = DBH(I)
          if (D.ge.XMX) go to 25
          if (LESTB .and. I.lt.ITRNIN) go to 25
          K = I
          L = 0
          H = HT(I)

          !  IF CALLED FROM **ESTAB** AND TOTAL CYCLE LENGTH IS LESS THAN OR EQUAL
          !  TO 5 YEARS, SKIP HEIGHT INCREMENT CALCULATION BUT ASSIGN CR AND DBH.

          if (LSKIPH) then
             HTG(K) = 0
             go to 20
          end if
          XBA = BA
          if (XBA .le.0.0) XBA = 0.1
          CR = ICR(I)/10.0

          !  CALCULATE RELATIVE HEIGHT AND TPCCF
          RELHT = 1.
          if (H.gt.0..and. AVH.gt.0.)RELHT = H/AVH
          TPCCF = PCCF(ITRE(I))
          if (TPCCF .le.75.) RELHT = 1.0-((RELHT-1.0)/75.)*TPCCF
          if (RELHT .gt.1.5) RELHT = 1.5

          !  CALL SMALL TREE HEIGHT GROWTH ROUTINE
          call HTGR5(ISPC,XSITE,XBA,HTGR,RELHT,CR,H,DEBUG)

          if (DEBUG)write(JOSTND,*)'AFTER HTGR5 I,ISPC,XSITE,XBA,HTGR,',&
               &'RELHT,CR,CON,XRHGRO=',I,ISPC,XSITE,XBA,HTGR,RELHT,CR,H,&
               &CON,XRHGRO

          HTGR = HTGR*CON*XRHGRO
          18 continue

          !  COMPUTE WEIGHTS FOR THE LARGE AND SMALL TREE HEIGHT INCREMENT
          !  ESTIMATES.  IF DBH IS LESS THAN OR EQUAL TO XMN OR IF CALLED
          !  FROM **ESTAB**, THE LARGE TREE PREDICTION IS IGNORED (XWT=0.0).

          XWT = 0.0
          if (D.le.XMN.or.LESTB) go to 19
          XWT = (D-XMN)/(XMX-XMN)
          19 continue
          if (ISPC .eq.12) then
             LTHG = HTG(K)
             if (LESTB) then
                HTGR2 = HTGR
             else
                HTGR2 = (HTGR + LTHG)/2.0
             end if
             HTG(K) = HTGR2*(1.0-XWT) + XWT*LTHG
             if (DEBUG)write(JOSTND,*)'IN REGENT - RW DEBUG: ',' LESTB=',&
                  &LESTB,' D=',D,' XWT=',XWT,' HTGR=',HTGR2,' LTHG=',LTHG,&
                  &' HTG FINAL=', HTG(K)
          else
             HTG(K) = HTGR*(1.0-XWT) + XWT*HTG(K)
          end if

          ! CHECK FOR SIZE CAP COMPLIANCE.

          if ((H+HTG(K)).gt.SIZCAP(ISPC,4)) then
             HTG(K) = SIZCAP(ISPC,4)-H
             if (HTG(K) .lt.0.1) HTG(K) = 0.1
          end if

          20 continue

          !  ASSIGN DBH AND COMPUTE DBH INCREMENT FOR TREES WITH DBH LESS
          !  THAN 3 INCHES.  COMPUTE 10-YEAR DBH INCREMENT REGARDLESS OF
          !  PROJECTION PERIOD LENGTH.  BYPASS INCREMENT CALCULATION IF
          !  D GE 3.0 INCHES.

          if (D.ge.DGMIN(ISPC)) go to 23
          HK = H+HTG(K)
          if (HK .le.4.5) then
             DBH(K) = D + HK*0.001
             DG(K) = 0.0
          else
             DK = BX/(ALOG(HK-4.5)-AX)-1.0
             if (H .le.4.5) then
                DKK = D
             else
                DKK = BX/(ALOG(H-4.5)-AX)-1.0
             end if

             !  USE INVENTORY EQUATIONS IF CALIBRATION OF THE HT-DBH FUNCTION IS TURNED
             !  OFF, OR IF WYKOFF CALIBRATION DID NOT OCCUR.
             !  NOTE: THIS SIMPLIFIES TO IF(IABFLB(ISPC).EQ.1) BUT IS SHOWN IN IT IS
             !        ENTIRITY FOR CLARITY.

             if (.not.LHTDRG(ISPC) .or.&
                  &(LHTDRG(ISPC) .and. IABFLG(ISPC).eq.1)) then
                call HTDBH (IFOR,ISPC,DK,HK,1)
                if (H .le.4.5) then
                   DKK = D
                else
                   call HTDBH (IFOR,ISPC,DKK,H,1)
                end if
                if (DEBUG)write(JOSTND,*)'INV EQN DUBBING IFOR,ISPC,H,HK,DK,'&
                     &,'DKK= ',IFOR,ISPC,H,HK,DK,DKK
                if (DEBUG)write(JOSTND,*)'ISPC,LHTDRG,IABFLG= ',&
                     &ISPC,LHTDRG(ISPC),IABFLG(ISPC)
             end if

             !       IF CALLING FROM **ESTAB** ASSIGN DIAMETER AND DIAMETER
             !       INCREMENT.

             if (LESTB) then
                DBH(K) = DK
                if (DBH(K) .lt. DIAM(ISPC)) DBH(K) = DIAM(ISPC)
                DBH(K) = DBH(K) + 0.001*HK
                DG(K) = DBH(K)
             else

                !         DIAMETER INCREMENT IS THE DIFFERENCE BETWEEN COMPUTED
                !         DIAMETERS.  MULTIPLIER IS APPLIED TO DIAMETER INCREMENT
                !         AT THIS POINT.

                BARK = BRATIO(ISPC,DBH(K),HT(K))
                if (DK.lt.0..or. DKK.lt.0.) then
                   DG(K) = HTG(K)*0.2*BARK*XRDGRO
                   DK = D+DG(K)
                else
                   DGSM = (DK-DKK)*BARK*XRDGRO
                end if
                if (DG(K).lt.0.) DG(K) = 0.
                if (DGSM.lt.0.) DGSM = 0.

                !         SCALE DIAMETER INCREMENT FOR BARK AND PERIOD LENGTH.
                !         IN ORDER TO MAINTAIN CONSISTENCY WITH **GRADD**,
                !         ADJUSTMENTS ARE MADE ON THE DDS SCALE.
                !         REDWOOD ASSUMES WEIGHTED AVERAGE OF SMALL AND LARGE
                !         TREE DIAMETER GROWTH.

                DDS = DGSM*(2.0*BARK*D+DGSM)*SCALE2
                if (ISPC .eq.12) then
                   DGSM = SQRT((D*BARK)**2+DDS)-BARK*D
                   XDWT = (D-XMN)/(DGMIN(ISPC)-XMN)
                   if (D .le. XMN) XDWT = 0.0
                   DGLT = DG(K)
                   DG(K) = DGSM*(1.0-XDWT)+DGLT*XDWT
                   if (DEBUG)write(JOSTND,*)'IN REGENT - RW DEBUG: ',' D=',D,&
                        &' XDWT=',XDWT,' DKK=',DKK,' DK=',DK,' BARK=', BARK,&
                        &' DGSM=',DGSM,' DGLT=',DGLT,' DGSM FINAL=', DG(K),&
                        &' SCALE2=',SCALE2
                else
                   DG(K) = SQRT((D*BARK)**2+DDS)-BARK*D
                end if
             end if
             if ((DBH(K)+DG(K)).lt.DIAM(ISPC)) then
                DG(K) = DIAM(ISPC)-DBH(K)
             end if
          end if

          !  CHECK FOR TREE SIZE CAP COMPLIANCE

          call DGBND(ISPC,DBH(K),DG(K))

          23 continue

          !  PRINT DEBUG AND RETURN TO PROCESS NEXT TRIPLE OR NEXT TREE.

          if (.not.DEBUG) go to 24
          write(JOSTND,9000) I,K,ISPC,H,HTG(K),DBH(K),DG(K)
          9000 format('IN REGENT LOOP 2, I=',I4,', K=',I4,', ISPC=',I3,&
               &', CUR HT=',F7.2,', HT INC=',F7.4/T19,&
               &', CUR DBH=',F7.2,', 10-YR DBH INC=',F7.4)
          24 continue
          if (LESTB .or..not.LTRIP .or. L.ge.2) go to 25
          L = L+1
          K = ITRN+2*I-2+L
          go to 18

          !  END OF GROWTH PREDICTION LOOP.  PRINT DEBUG INFO IF DESIRED.

          25 continue
       end do
       30 continue
    end do
    return

    !  SMALL TREE HEIGHT CALIBRATION SECTION.

    40 continue
    do ISPC = 1,MAXSP
       HCOR(ISPC) = 0.0
       CORTEM(ISPC) = 1.0
       NUMCAL(ISPC) = 0
       45 continue
    end do
    if (ITRN.le.0) return
    if (IFINTH .eq.0)  goto 100
    SCALE3 = REGYR/FINTH

    !  BEGIN PROCESSING TREE LIST IN SPECIES ORDER.  DO NOT CALCULATE
    !  CORRECTION TERMS IF THERE ARE NO TREES FOR THIS SPECIES.

    do ISPC = 1,MAXSP
       CORNEW = 1.0
       I1 = ISCT(ISPC,1)
       if (I1.eq.0.or..not. LHTCAL(ISPC)) go to 90
       I2 = ISCT(ISPC,2)
       IREFI = IREF(ISPC)
       SSITE = SITEAR(ISPC)
       N = 0
       SNP = 0.0
       SNX = 0.0
       SNY = 0.0

       !  BEGIN TREE LOOP WITHIN SPECIES.  IF MEASURED HEIGHT INCREMENT IS
       !  LESS THAN OR EQUAL TO ZERO, OR DBH IS GREATER THAN 5.0, THE RECORD
       !  WILL BE EXCLUDED FROM THE CALIBRATION.

       do I3 = I1,I2
          I = IND1(I3)
          H = HT(I)
          if (IHTG.lt.2) H = H-HTG(I)
          if (DBH(I).ge.5.0.or.H.lt.0.01) go to 60
          if (HTG(I).lt.0.001) go to 60
          CR = FLOAT(ICR(I))/10.
          RELHT = H/AVH
          if (RELHT .gt.1.5) RELHT = 1.5
          XBA = BA
          if (XBA .le.0.0) XBA = 0.1
          call HTGR5(ISPC,SSITE,XBA,XHTGR,RELHT,CR,H,DEBUG)
          EDH = XHTGR*RHCON(ISPC)
          P = PROB(I)
          TERM = HTG(I)*SCALE3
          SNP = SNP+P
          SNX = SNX+EDH*P
          SNY = SNY+TERM*P
          N = N+1

          !  PRINT DEBUG INFO IF DESIRED.

          if (DEBUG) write(JOSTND,9001) NPLT,I,ISPC,H,DBH(I),ICR(I),&
               &PCT(I),XHTGR,RHCON(ISPC),EDH,TERM
          9001 format('NPLT=',A26,',  I=',I5,',  ISPC=',I3,',  H=',F6.1,&
               &',  DBH=',F5.1,',  ICR',I5,',  PCT=',F6.1,',  XHTGR=',&
               &F6.1 / 12X,'RHCON=',F10.3,',  EDH=',F10.3,', TERM=',F10.3)

          !  END OF TREE LOOP WITHIN SPECIES.

          60 continue
       end do
       if (DEBUG) write(JOSTND,9010) ISPC,SNP,SNX,SNY
       9010 format(/'SUMS FOR SPECIES ',I2,':  SNP=',F10.2,&
            &';   SNX=',F10.2,';   SNY=',F10.2)

       !  COMPUTE CALIBRATION TERMS.  CALIBRATION TERMS ARE NOT COMPUTED
       !  IF THERE WERE FEWER THAN NCALHT (DEFAULT=5) HEIGHT INCREMENT
       !  OBSERVATIONS FOR A SPECIES.

       if (N.lt.NCALHT) go to 80

       !  CALCULATE MEANS FOR THE FOR THE GROWTH SAMPLE ON THE
       !  NATURAL SCALE.

       call REGHCAL(ISPC,SNP,SNX,SNY,CORNEW)
       80 continue
       CORTEM(IREFI) = CORNEW
       NUMCAL(IREFI) = N
       90 continue
    end do

    !  END OF CALIBRATION LOOP.  PRINT CALIBRATION STATISTICS AND RETURN

    write(JOSTND,9002) (NUMCAL(I),I = 1,NUMSP)
    9002 format(/'NUMBER OF RECORDS AVAILABLE FOR SCALING'/&
         &'THE SMALL TREE HEIGHT INCREMENT MODEL',&
         &((T48,11(I4,2X)/)))
    100 continue
    write(JOSTND,9003) (CORTEM(I),I = 1,NUMSP)
    9003 format(/'INITIAL SCALE FACTORS FOR THE SMALL TREE'/&
         &'HEIGHT INCREMENT MODEL',&
         &((T48,11(F5.2,1X)/)))

    ! OUTPUT CALIBRATION TO DATABASE.

    call REGCALOUT(CORTEM,NUMCAL,KOUT,K,KK,ISPEC,SPEC)
    return

    9998 continue

    !  ENTRY POINT FOR LOADING OF REGENERATION GROWTH MODEL
    !  CONSTANTS  THAT REQUIRE ONE-TIME RESOLUTION.

    do ISPC = 1,MAXSP
       RHCON(ISPC) = 1.0
       if (LRCOR2.and.RCOR2(ISPC).gt.0.0)&
            &RHCON(ISPC) = RCOR2(ISPC)
       110 continue
    end do
    return
  end subroutine REGNC

  !===============================!
  !   Compute small tree growth   !
  !===============================!
  ! SN $Id$
  subroutine REGSN(LCON)
    implicit none

    logical :: LCON
    ! Regional callees retain their native argument lists.
    interface
       subroutine HTCALC(N,ISPC,AGET,H,HTMAX,HTG1,JOSTND,DEBUG)
         integer :: N
         integer :: ISPC
         real :: AGET
         real :: H
         real :: HTMAX
         real :: HTG1
         integer :: JOSTND
         logical :: DEBUG
       end subroutine HTCALC
    end interface


    !  THIS SUBROUTINE COMPUTES HEIGHT AND DIAMETER INCREMENTS FOR
    !  SMALL TREES.  THE HEIGHT INCREMENT MODEL IS APPLIED TO TREES
    !  THAT ARE LESS THAN  5 INCHES DBH,
    !  AND THE DBH INCREMENT MODEL IS APPLIED TO TREES THAT ARE LESS
    !  THAN 3 INCHES DBH.  FOR TREES THAT ARE GREATER THAN 3 INCHES
    !  DBH, HEIGHT INCREMENT PREDICTIONS
    !  ARE AVERAGED WITH THE PREDICTIONS FROM THE LARGE TREE MODEL.
    !  THIS ROUTINE IS CALLED FROM **CRATET** DURING CALIBRATION AND
    !  FROM **TREGRO** DURING CYCLING.  ENTRY **REGCON** IS CALLED FROM
    !  **RCON** TO LOAD MODEL PARAMETERS THAT NEED ONLY BE RESOLVED ONCE.

    ! Common blocks

    !  DIMENSIONS FOR INTERNAL VARIABLES:

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
    !     DIAM -- BUDWIDTH VALUES (INCHES) BY SPECIES

    !  DECLARATIONS
    !  SCALARS

    external RANN
    ! Variables
    real    :: AGET,AX,BARK,BX,CORNEW,CR,D,DDS,DDUM,DK,DKK,DGMX
    real    :: EDH,H,HK,HTNEW,HTGR,HTG1,HTMAX,P,PTCCF,FNT,RAN
    real    :: REGYR,SCALE,SCALE2,SCALE3,SNP,SNX,SNY,TERM,XMX,XMN
    real    :: XRDGRO,XRHGRO,XWT,CON
    integer :: I,I1,I2,I3,ISPC,ISPEC,IPCCF,IREFI,K,KK,KOUT
    integer :: L,MODE0,MODE9,N
    character(len=2) :: SPEC
    logical :: DEBUG,LSKIPH

    !  ARRAYS

    real    :: CORTEM(90),DGMAX(90),DIAM(90),HGADJ(90)
    real    :: XMAX(90),XMIN(90)
    integer :: NUMCAL(90)

    !  FUNCTIONS

    real    :: BACHLO,BRATIO

    !  INITIALIZATIONS

    data HGADJ/ 90*1.0/
    data DGMAX/ 90*5.0 /
    data XMIN / 90*1.0 /, XMAX/ 90*3.0 /
    data REGYR/5.0/

    if (LCON) go to 9998
    HTGR = 0.0

    !  CHECK FOR DEBUG.

    LSKIPH = .false.
    call DBCHK (DEBUG,'REGENT',6,ICYC)
    if (DEBUG) write(JOSTND,9980)ICYC,FINT
    9980 format('ENTERING SUBROUTINE REGENT  CYCLE =',I5,F10.3)

    !  HEIGHT INCREMENT IS DERIVED FROM A HEIGHT-AGE CURVE AND IS NOMINALLY
    !  BASED ON A 10-YEAR GROWTH PERIOD.   SCALE IS USED TO CONVERT
    !  HEIGHT INCREMENT PREDICTIONS TO A FINT-YEAR PERIOD.  DIAMETER
    !  INCREMENT IS PREDICTED FROM CHANGE IN HEIGHT, AND IS SCALED TO A 10-
    !  YEAR PERIOD BY APPLICATION OF THE VARIABLE SCALE2. DIAMETER INCREMENT
    !  IS CONVERTED TO A FINT-YEAR BASIS IN **UPDATE**.

    FNT = FINT
    if (LSTART) FNT = IFINTH
    if (LESTB) then
       if (FINT.le.5.0) then
          LSKIPH = .true.
       else
          FNT = FNT-5.0
       end if
    end if

    !  IF THIS IS THE FIRST CALL TO REGENT, BRANCH TO STATEMENT 40 FOR
    !  MODEL CALIBRATION.

    if (LSTART) goto 40
    call MULTS (3,IY(ICYC),XRHMLT)
    call MULTS(6,IY(ICYC),XRDMLT)
    if (ITRN.le.0) go to 91

    SCALE = FNT/REGYR
    SCALE2 = YR/FNT

    !  ENTER GROWTH PREDICTION LOOP.  PROCESS EACH SPECIES AS A GROUP;
    !  LOAD CONSTANTS FOR NEXT SPECIES.

    do ISPC = 1,MAXSP
       I1 = ISCT(ISPC,1)
       if (I1.eq.0) go to 30
       I2 = ISCT(ISPC,2)

       XRDGRO = XRDMLT(ISPC)
       XRHGRO = XRHMLT(ISPC)
       CON = RHCON(ISPC) * EXP(HCOR(ISPC))
       XMX = XMAX(ISPC)
       XMN = XMIN(ISPC)

       !     PUT A CEILING ON DIAMETER GROWTH BASED ON OLIVER AND COCHRAN EMPR.
       !     RJ 11/28/88

       DGMX = DGMAX(ISPC) * SCALE

       !  PROCESS NEXT TREE RECORD.

       do I3 = I1,I2
          I = IND1(I3)
          IPCCF = ITRE(I)
          D = DBH(I)
          if (D .ge. XMX) go to 25

          !  SET DDUM = 1 IF THIS IS A MANAGED STAND
          !  BYPASS INCREMENT CALCULATIONS IF CALLED FROM ESTAB AND THIS IS NOT A
          !  NEWLY CREATED TREE.

          DDUM = 0.0
          if (MANAGD .eq.1) DDUM = 1.0
          if (LESTB) then
             if (I.lt.ITRNIN) go to 25

             !  ASSIGN CROWN RATIO FOR NEWLY ESTABLISHED TREES.
             !  SET THE DUMMY FOR DIA LOOKUP TO 1 (= MGT STAND)

             DDUM = 1.0

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
          H = HT(I)
          if (LSKIPH) then
             HTG(K) = 0.0
             go to 4
          end if

          !     RETURN HERE TO PROCESS NEXT TRIPLE.

          2 continue

          !  ESTIMATE SITE CURVE HEIGHT INCREMENT VALUES FOR INCOMING TREES.

          !    HEIGHT GROWTH EQUATION, USING NC128 TREE HEIGHT COEFFS.
          !    EVALUATED FOR EACH TREE EACH CYCLE, 5.0 YEAR CYCLE
          !    FIRST FIND EFFECTIVE TREE AGE BASED ON H

          MODE0 = 0
          AGET = 0.
          call HTCALC (MODE0,ISPC,AGET,H,HTMAX,HTG1,JOSTND,DEBUG)

          if (HTMAX-H.le.1.) then

             !  IF INITIAL TREE HEIGHT IS >= MAX HEIGHT FOR SITE (HTMAX).
             !  IF TRUE, THEN SET HTG= 0.1 FT.

             if (DEBUG) write(JOSTND,*) 'HTI>=HTMAX , ABIRTH= ',ABIRTH(I)

             HTGR = 0.10
          else

             !     FIND HTGR

             MODE9 = 9
             HTGR = 0.
             call HTCALC (MODE9,ISPC,AGET,H,HTMAX,HTGR,JOSTND,DEBUG)

             PTCCF = PCCF(IPCCF)

             if (DEBUG)write(JOSTND,*)'I,ISPC,PTCCF,HGADJ,FNT,ABIRTH= ',&
                  &I,ISPC,PTCCF,HGADJ(ISPC),FNT,ABIRTH(I)
             if (DEBUG)write(JOSTND,*)'I,SCALE,SCALE2,HTGR,DBH,BA,IPCCF= ',&
                  &I,SCALE,SCALE2,HTGR,D,BA,IPCCF

             HTGR = HTGR*CON*SCALE*HGADJ(ISPC)*XRHGRO
             if (HTGR .lt.0.1) HTGR = 0.1
          end if

          !     COMPUTE WEIGHTS FOR THE LARGE AND SMALL TREE HEIGHT INCREMENT
          !     ESTIMATES.  IF DBH IS LESS THAN OR EQUAL TO XMN, THE LARGE TREE
          !     PREDICTION IS IGNORED (XWT=0.0).

          XWT = (D-XMN)/(XMX-XMN)
          if (D.le.XMN .or. LESTB) XWT = 0.0

          !     COMPUTE WEIGHTED HEIGHT INCREMENT FOR NEXT TRIPLE.

          if (DEBUG)write(JOSTND,9982)XWT,HTGR,HTG(K),I,K
          9982 format('IN REGENT 9982 FORMAT',3(F10.4,2X),2I7)

          HTGR = HTGR*(1.0-XWT) + XWT*HTG(K)
          if (HTGR .lt..1) HTGR = .1

          !   ADD RANDOM AFFECTS. LIMIT TO + OR -  1/10 HTGR

          RAN = 0.
          if (DGSD .ge.1.0) then
             3 continue
             RAN = BACHLO(0.0,1.0,RANN)
             if (RAN.lt.-1.0.or. RAN.gt.1.0) go to 3
          end if
          HTGR = HTGR + RAN*0.1*HTGR

          HTG(K) = HTGR
          if (HTG(K) .lt..1) HTG(K) = .1

          if (DEBUG)write(JOSTND,9985)HTGR,HTG(K),I,K,LESTB
          9985 format('IN REGENT HTGR,HTG(K),I,K,LESTB= '/&
               &2(F10.4,2X),2I10,L10)

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

             !   USE WYKOFF EQ. FOR DBH
             !      DKK = DIAMETER WITH HTG ADDED TO THE STARTING HEIGHT
             !      DK  = DIAMETER AT THE START OF THE PROJECTION

             BX = HT2(ISPC)
             if (IABFLG(ISPC).eq.1) then
                AX = HT1(ISPC)
             else
                AX = AA(ISPC)
             end if
             DKK = (BX/(ALOG(HK-4.5)-AX))-1.0
             if (H .le.4.5) then
                DK = D
             else
                DK = (BX/(ALOG(H-4.5)-AX))-1.0
             end if

             IF(DEBUG)WRITE(JOSTND,*)'I,ISPC,DBH,H,HK,DK,DKK,DDUM=           &
                  &  ',I,ISPC,DBH(K),H,HK,DK,DKK,DDUM

             !  USE INVENTORY EQUATIONS IF CALIBRATION OF THE HT-DBH FUNCTION IS TURNED
             !  OFF, OR IF WYKOFF CALIBRATION DID NOT OCCUR.
             !  NOTE: THIS SIMPLIFIES TO IF(IABFLB(ISPC).EQ.1) BUT IS SHOWN IN IT'S
             !        ENTIRITY FOR CLARITY.

             if (.not.LHTDRG(ISPC) .or.&
                  &(LHTDRG(ISPC) .and. IABFLG(ISPC).eq.1)) then
                call HTDBH (IFOR,ISPC,DKK,HK,1)
                if (H .le.4.5) then
                   DK = D
                else
                   call HTDBH (IFOR,ISPC,DK,H,1)
                end if
                if (DEBUG)write(JOSTND,*)'INV EQN DUBBING IFOR,ISPC,H,HK,DK,'&
                     &,'DKK= ',IFOR,ISPC,H,HK,DK,DKK
                if (DEBUG)write(JOSTND,*)'ISPC,LHTDRG,IABFLG= ',&
                     &ISPC,LHTDRG(ISPC),IABFLG(ISPC)
             end if

             !         IF CALLED FROM **ESTAB** ASSIGN DIAMETER

             if (LESTB) then
                DBH(K) = DKK
                if (DBH(K).lt.DIAM(ISPC) .or. HK.lt.4.5) DBH(K) = DIAM(ISPC)
                DBH(K) = DBH(K)+0.001*HK
                DG(K) = DBH(K)
             else

                !         COMPUTE DIAMETER INCREMENT BY SUBTRACTION, APPLY USER
                !         SUPPLIED MULTIPLIERS, AND CHECK TO SEE IF COMPUTED VALUE
                !         IS WITHIN BOUNDS.

                BARK = BRATIO(ISPC,D,H)
                if (DEBUG)write(JOSTND,*)'BARK,XRDGRO= ',BARK,XRDGRO
                if (DK.lt.0.0.or. DKK.lt.0.0) then
                   DG(K) = HTG(K)*0.2*BARK*XRDGRO
                   DKK = D+DG(K)
                else
                   DG(K) = (DKK-DK)*BARK*XRDGRO
                end if
                if (DEBUG)write(JOSTND,*)'K,DK,DKK,DG= ',K,DK,DKK,DG(K)

                if (DG(K) .lt.0.0) DG(K) = 0.1
                if (DG(K) .gt. DGMX) DG(K) = DGMX

                !         SCALE DIAMETER INCREMENT TO 5-YR ESTIMATE.
                !         SCALE ADJUSTMENT IS ON GROWTH IN DDS RATHER THAN INCHES
                !         OF DG TO BE CONSISTENT WITH GRADD.

                DDS = DG(K)*(2.0*BARK*D+DG(K))*SCALE2
                DG(K) = SQRT((D*BARK)**2+DDS)-BARK*D

             end if
             if ((DBH(K)+DG(K)).lt.DIAM(ISPC)) then
                DG(K) = DIAM(ISPC)-DBH(K)
             end if
          end if

          !  CHECK FOR TREE SIZE CAP COMPLIANCE

          call DGBND(ISPC,DBH(K),DG(K))

          if (DEBUG) then
             HTNEW = HT(I)+HTG(I)
             write(JOSTND,9987) I,ISPC,HT(I),HTG(I),HTNEW,DBH(I),DG(I),DG(K)
             9987 format('IN REGENT, I=',I4,',  ISPC=',I3,'  CUR HT=',F7.2,&
                  &',  HT INC=',F7.4,',  NEW HT=',F7.2,',  CUR DBH=',F10.5,&
                  &',  DBH INC(I,K)=',2F7.4)
          end if
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
    SCALE3 = REGYR / FINTH

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
       CON = RHCON(ISPC)

       !  BEGIN TREE LOOP WITHIN SPECIES.  IF MEASURED HEIGHT INCREMENT IS
       !  LESS THAN OR EQUAL TO ZERO, OR DBH IS LESS THAN 5.0, THE RECORD
       !  WILL BE EXCLUDED FROM THE CALIBRATION.

       do I3 = I1,I2
          I = IND1(I3)
          H = HT(I)
          K = I

          !  DIA GT 3 INCHES INCLUDED IN OVERALL MEAN

          if (IHTG.lt.2) H = H-HTG(I)
          if (DBH(I).ge.5.0.or.H.lt.0.01) go to 60
          IPCCF = ITRE(I)
          PTCCF = PCCF(IPCCF)

          if (DEBUG) write(JOSTND,*)'IN REGENT 8900 H =',H

          !  CALCULATE TREE HEIGHT INCREMENT
          !  HEIGHT GROWTH EQUATION, USING NC128 TREE HEIGHT COEFFS.
          !  EVALUATED FOR EACH TREE EACH CYCLE, 5.0 YEAR CYCLE
          !  FIRST FIND EFFECTIVE TREE AGE BASED ON H

          MODE0 = 0
          AGET = 0.
          call HTCALC (MODE0,ISPC,AGET,H,HTMAX,HTG1,JOSTND,DEBUG)

          !     THEN FIND HTGR

          MODE9 = 9
          HTGR = 0.
          call HTCALC (MODE9,ISPC,AGET,H,HTMAX,HTGR,JOSTND,DEBUG)

          if (HTGR .lt..1) HTGR = .1

          EDH = HTGR * RHCON(ISPC)
          if (EDH .lt.0.1) EDH = 0.1
          P = PROB(I)
          if (HTG(I).lt.0.001) go to 60
          TERM = HTG(I) * SCALE3
          SNP = SNP+P
          SNX = SNX+EDH*P
          SNY = SNY+TERM*P
          N = N+1

          !  PRINT DEBUG INFO IF DESIRED.

          if (DEBUG)write(JOSTND,9991) NPLT,I,ISPC,H,DBH(I),ICR(I),&
               &PCT(I),RELDM1,RHCON(ISPC),EDH,TERM,HTGR,SCALE3,HGADJ(ISPC),&
               &ICR(I)
          9991 format('NPLT=',A8,',  I=',I5,',  ISPC=',I3,',  H=',F6.1,&
               &',  DBH=',F5.1,',  ICR',I5,',  PCT=',F6.1,',  RELDEN=',&
               &F6.1 / 12X,'RHCON=',F10.3,',  EDH=',F10.3,', TERM=',F10.3,&
               &' HTGR= ',F10.3,' SCALE3= ',F10.3,' HGADJ(ISPC)= ',F10.3/&
               &' ICR= ',I5)

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

       call REGHCAL(ISPC,SNP,SNX,SNY,CORNEW)
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
    write(JOSTND,9994) (CORTEM(I),I = 1,NUMSP)
    9994 format(/'INITIAL SCALE FACTORS FOR THE SMALL TREE'/&
         &'HEIGHT INCREMENT MODEL',&
         &((T48,11(F5.2,1X)/)))

    ! OUTPUT CALIBRATION TO DATABASE.

    call REGCALOUT(CORTEM,NUMCAL,KOUT,K,KK,ISPEC,SPEC)
    91 if (DEBUG)write(JOSTND,9995)ICYC
    9995 format('LEAVING SUBROUTINE REGENT  CYCLE =',I5)
    return
    9998 continue

    !  ENTRY POINT FOR LOADING OF REGENERATION GROWTH MODEL
    !  CONSTANTS  THAT REQUIRE ONE-TIME RESOLUTION.
    !  IF THE BUDWIDTH VALUES IN THE DIAM ARRAY, BELOW, ARE MODIFIED
    !  THEN ALSO MODIFY THE VALUES IN THE SNDBAL ARRAY IN **HTDBH

    data DIAM/&
         &0.1,  0.3,  0.2,  0.5,  0.5,  0.5,  0.5,  0.5,  0.5,  0.5,&
         &0.5,  0.4,  0.5,  0.5,  0.2,  0.2,  0.1,  0.2,  0.2,  0.2,&
         &0.2,  0.2,  0.3,  0.1,  0.1,  0.2,  0.3,  0.3,  0.1,  0.2,&
         &0.1,  0.2,  0.1,  0.2,  0.2,  0.2,  0.2,  0.1,  0.2,  0.2,&
         &0.1,  0.3,  0.4,  0.2,  0.2,  0.2,  0.2,  0.2,  0.2,  0.2,&
         &0.2,  0.2,  0.2,  0.2,  0.2,  0.2,  0.2,  0.2,  0.1,  0.1,&
         &0.2,  0.1,  0.2,  0.2,  0.1,  0.1,  0.2,  0.1,  0.2,  0.2,&
         &0.2,  0.1,  0.1,  0.2,  0.2,  0.1,  0.1,  0.2,  0.2,  0.1,&
         &0.1,  0.1,  0.1,  0.1,  0.1,  0.1,  0.1,  0.3,  0.2,  0.2/

    do ISPC = 1,MAXSP
       RHCON(ISPC) = 1.0
       if (LRCOR2.and.RCOR2(ISPC).gt.0.0)&
            &RHCON(ISPC) = RCOR2(ISPC)
       90 continue
    end do
    return
  end subroutine REGSN

  !===============================!
  !   Compute small tree growth   !
  !===============================!
  ! SO $Id$
  subroutine REGSO(LCON)
    implicit none

    logical :: LCON
    ! Regional callees retain their native argument lists.
    interface
       subroutine SMHTGF(MODE,ICYC,I,H,CR1,DTIME,HHT,JOSTND,DEBUG)
         integer :: MODE
         integer :: ICYC
         integer :: I
         real :: H
         real :: CR1
         real :: DTIME
         real :: HHT
         integer :: JOSTND
         logical :: DEBUG
       end subroutine SMHTGF
    end interface


    !  THIS SUBROUTINE COMPUTES HEIGHT AND DIAMETER INCREMENTS FOR
    !  SMALL TREES.  THE HEIGHT INCREMENT MODEL IS APPLIED TO TREES
    !  THAT ARE LESS THAN 10 INCHES DBH (5 INCHES FOR LODGEPOLE PINE),
    !  AND THE DBH INCREMENT MODEL IS APPLIED TO TREES THAT ARE LESS
    !  THAN 3 INCHES DBH.  FOR TREES THAT ARE GREATER THAN 2 INCHES
    !  DBH (1 INCH FOR LODGEPOLE PINE), HEIGHT INCREMENT PREDICTIONS
    !  ARE AVERAGED WITH THE PREDICTIONS FROM THE LARGE TREE MODEL.
    !  HEIGHT INCREMENT IS A FUNCTION OF SITE HEIGHT, CALCULATED
    !  IN **SMHTGF**, AND MODIFIED BY VIGOR AND DENSITY FUNCTIONS
    !  OF CCF, TOP HEIGHT AND CROWN RATIO.  DIAMETER IS ASSIGNED FROM
    !  A HEIGHT-DIAMETER FUNCTION WITH ADJUSTMENTS FOR RELATIVE SIZE
    !  AND STAND DENSITY.  INCREMENT IS COMPUTED BY SUBTRACTION.
    !  THIS ROUTINE IS CALLED FROM **CRATET** DURING CALIBRATION AND
    !  FROM **TREGRO** DURING CYCLING.  ENTRY **REGCON** IS CALLED FROM
    !  **RCON** TO LOAD MODEL PARAMETERS THAT NEED ONLY BE RESOLVED ONCE.

    ! Common blocks

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
    !  SPECIES ORDER:
    !  1=WP,  2=SP,  3=DF,  4=WF,  5=MH,  6=IC,  7=LP,  8=ES,  9=SH,  10=PP,
    ! 11=JU, 12=GF, 13=AF, 14=SF, 15=NF, 16=WB, 17=WL, 18=RC, 19=WH,  20=PY,
    ! 21=WA, 22=RA, 23=BM, 24=AS, 25=CW, 26=CH, 27=WO, 28=WI, 29=GC,  30=MC,
    ! 31=MB, 32=OS, 33=OH

    external RANN
    ! Variables
    logical :: DEBUG,LSKIPH
    character(len=2) :: SPEC
    integer :: KK,ISPEC,KOUT,IREFI,N,IPCCF,MODE1,K,L,JCR
    integer :: ISPC,I1,I2,I3,I,NUMCAL(33)
    real    :: RDCON(33),RDCR(33)
    real    :: RDLHT(33),RDHT(33),RDDUM(33)
    real    :: REGYR,CORTEM(33),DGMAX(33),DIAM(33)
    real    :: XMAX(33),XMIN(33),SLO,SHI,AB(9)
    real    :: D,CR,RAN,H,BARK,VIGOR,RSIMOD,HITE1,HTGR,TEMT
    real    :: POTHTG,ZZRAN,XWT,BKPT,HK,BX,AX,DK,DKK,PPCCF,DDUM
    real    :: CRCODE,ALHT,TERM,P,EDH,H2,AG1,SCALE,SNY,SNX,SNP,CORNEW,HTNEW
    real    :: DDS,DGSM,XDWT,DAT45,ALHK,TPCCF,HITE2,AG2,RELSI,BRATIO,BACHLO
    real    :: DGMX,SCALE2,SI,XMN,XMX,CON,FNT,CCF,AVHT,PCTRED,XRHGRO
    real    :: XRDGRO,SCALE3,X,HLESS4,DLESS3
    real    :: SITHT,SITAGE,AGMAX,HTMAX,HTMAX2,D2

    ! Data statements
    ! RJ 11/28/88 MAX DIAMETER GROWTHS

    data DGMAX /&
         &2.8, 2.8, 2.4, 3.6, 2.5, 2.5, 3.5, 3.6, 3.6, 2.8,&
         &2.0, 3.6, 3.6, 3.6, 5.0, 2.8, 2.8, 2.5, 5.0, 5.0,&
         &5.0, 5.0, 5.0, 2.5, 5.0, 5.0, 2.8, 5.0, 5.0, 5.0,&
         &5.0, 2.4, 5.0/

    data XMAX /&
         &3.0, 5.0, 4.0, 4.0, 2.0, 4.0, 4.0, 4.0, 4.0, 5.0,&
         &99., 4.0, 4.0, 4.0, 4.0, 3.0, 4.0, 10., 4.0, 4.0,&
         &4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0,&
         &4.0, 4.0, 4.0/

    data XMIN /&
         &2.0, 1.0, 2.0, 2.0, 1.0, 2.0, 2.0, 2.0, 2.0, 1.0,&
         &90., 2.0, 2.0, 2.0, 2.0, 1.5, 2.0, 2.0, 2.0, 2.0,&
         &2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0,&
         &2.0, 2.0, 2.0/

    data DIAM/&
         &0.4, 0.4, 0.3, 0.3, 0.2, 0.2, 0.4, 0.3, 0.2, 0.5,&
         &0.3, 0.3, 0.3, 0.3, 0.3, 0.4, 0.3, 0.2, 0.2, 0.2,&
         &0.2, 0.3, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2,&
         &0.2, 0.3, 0.2/

    data AB/&
         &1.11436,-.011493,.43012E-4,-.72221E-7,.5607E-10,-.1641E-13,3*0./

    ! COEFS FOR THE DIA LOOKUP FUNCTION - WC VARIANT SPECIES

    data RDCON/&
         &0.0000, 0.0000, 0.0000, 0.0000, 0.0000,&
         &0.0000, 0.0000, 0.0000, 0.0000, 0.0000,&
         &0.0000, 0.0000, 0.0000, 0.0000,-2.0890,&
         &0.0000, 0.0000, 0.0000,-0.6740,-2.0890,&
         &3.1020, 3.1020, 3.1020, 0.0000, 3.1020,&
         &3.1020, 0.0000, 3.1020, 3.1020, 3.1020,&
         &3.1020, 0.0000, 3.1020/

    data RDCR/&
         &0.0000, 0.0000, 0.0000, 0.0000, 0.0000,&
         &0.0000, 0.0000, 0.0000, 0.0000, 0.0000,&
         &0.0000, 0.0000, 0.0000, 0.0000, 0.0000,&
         &0.0000, 0.0000, 0.0000, 0.0000, 0.0000,&
         &0.0000, 0.0000, 0.0000, 0.0000, 0.0000,&
         &0.0000, 0.0000, 0.0000, 0.0000, 0.0000,&
         &0.0000, 0.0000, 0.0000/

    data RDLHT/&
         &0.0000, 0.0000, 0.0000, 0.0000, 0.0000,&
         &0.0000, 0.0000, 0.0000, 0.0000, 0.0000,&
         &0.0000, 0.0000, 0.0000, 0.0000, 1.9800,&
         &0.0000, 0.0000, 0.0000, 1.5220, 1.9800,&
         &0.0000, 0.0000, 0.0000, 0.0000, 0.0000,&
         &0.0000, 0.0000, 0.0000, 0.0000, 0.0000,&
         &0.0000, 0.0000, 0.0000/

    data RDHT/&
         &0.0000, 0.0000, 0.0000, 0.0000, 0.0000,&
         &0.0000, 0.0000, 0.0000, 0.0000, 0.0000,&
         &0.0000, 0.0000, 0.0000, 0.0000, 0.0000,&
         &0.0000, 0.0000, 0.0000, 0.0000, 0.0000,&
         &0.0210, 0.0210, 0.0210, 0.0000, 0.0210,&
         &0.0210, 0.0000, 0.0210, 0.0210, 0.0210,&
         &0.0210, 0.0000, 0.0210/

    data RDDUM/&
         &0.0000, 0.0000, 0.0000, 0.0000, 0.0000,&
         &0.0000, 0.0000, 0.0000, 0.0000, 0.0000,&
         &0.0000, 0.0000, 0.0000, 0.0000, 0.0000,&
         &0.0000, 0.0000, 0.0000, 0.0000, 0.0000,&
         &0.0000, 0.0000, 0.0000, 0.0000, 0.0000,&
         &0.0000, 0.0000, 0.0000, 0.0000, 0.0000,&
         &0.0000, 0.0000, 0.0000/

    if (LCON) go to 9998

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

       !  GET THE APPROPRIATE SITE INDEX RANGE VALUES FOR THIS SPECIES.

       SLO = 0.
       SHI = 999.
       call SITERANGE (2,ISPC,SLO,SHI)

       I1 = ISCT(ISPC,1)
       if (I1.eq.0) go to 30
       I2 = ISCT(ISPC,2)
       XRHGRO = XRHMLT(ISPC)
       XRDGRO = XRDMLT(ISPC)
       CON = RHCON(ISPC) * EXP(HCOR(ISPC))
       XMX = XMAX(ISPC)
       XMN = XMIN(ISPC)
       SI = SITEAR(ISPC)
       if (SI .gt. SHI) SI = SHI
       if (SI .le. SLO) SI = SLO + 0.5

       if (ISPC.eq.9.or.ISPC.eq.27) then
          REGYR = 5.
       else
          REGYR = 10.
       end if
       SCALE = FNT/REGYR
       SCALE2 = YR/FNT

       !     PUT A CEILING ON DIAMETER GROWTH BASED ON OLIVER AND COCHRAN EMPR.
       !     RJ 11/28/88

       DGMX = DGMAX(ISPC) * SCALE
       if (ISPC.eq.16)DGMX = FINT*0.2

       !  PROCESS NEXT TREE RECORD.

       do I3 = I1,I2
          I = IND1(I3)
          D = DBH(I)
          if (D .ge. XMX)go to 25
          IPCCF = ITRE(I)

          !  BYPASS INCREMENT CALCULATIONS IF CALLED FROM ESTAB AND THIS IS NOT A
          !  NEWLY CREATED TREE.

          if (LESTB) then
             if (I.lt.ITRNIN) go to 25

             !  ASSIGN CROWN RATIO FOR NEWLY ESTABLISHED TREES.

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

          JCR = INT(real(ICR(I))/10.)
          H = HT(I)
          BARK = BRATIO(ISPC,D,H)
          if (LSKIPH) then
             HTG(K) = 0.0
             go to 5
          end if
          X = real(ICR(I))/100.
          VIGOR = (150.0 * (X**3.0)*EXP(-6.0*X))+0.3
          if (VIGOR .gt.1.0)VIGOR = 1.0

          !  VIGOR ADJUSTMENT TO DRASTIC FOR PINYON, JUNIPER, AND OAK
          !  CUT IT BY TWO-THIRDS (FROM UT 6(AS) USED FOR SO 24(AS)

          if (ISPC.eq.11)VIGOR = 1.-((1.-VIGOR)/3.)

          !     RETURN HERE TO PROCESS NEXT TRIPLE.

          2 continue

          !  BEGIN POTHTG SECTION

          select case(ISPC)
          case(24)
             if (LESTB) then
                SITAGE = ABIRTH(I)
             else
                if (DEBUG)write(JOSTND,*)'IN REGENT, CALLING FINDAG I= ',I

                !  CALL FINDAG TO CALCULATE EFFECTIVE AGE

                SITAGE = 0.0
                SITHT = 0.0
                AGMAX = 0.0
                HTMAX = 0.0
                HTMAX2 = 0.0
                H = HT(I)
                D2 = 0.0
                call FINDAG(I,ISPC,D,D2,H,SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,&
                     &DEBUG)
             end if
             RELSI = (SI-SLO)/(SHI-SLO)
             RSIMOD = 0.5 * (1.0 + RELSI)

             ! COMPUTE HT GROWTH AND AGE FOR ASPEN. EQN FROM WAYNE SHEPPARD RMRS.

             HITE1 = 26.9825 * SITAGE**1.1752
             AG2 = SITAGE + 10.0
             HITE2 = 26.9825 * AG2**1.1752
             HTGR = (HITE2-HITE1)/(2.54*12.0) * RSIMOD * CON
             HTGR = HTGR*2.40

             ! GROWTH RATES APPEAR HIGH, REDUCE BY 25 PERCENT. DIXON 8-27-92

             HTGR = HTGR * .75
             go to 4
          case default

             ! CALL SMHTGF, THE SMALL TREE HEIGHT GROWTH ROUTINE. SMHTGF IS ALSO
             ! CALLED FROM ESSUBH TO GROW PLANTED AND NATURAL TREES FROM
             ! ESTABLISHMENT TO 5 YEARS INTO THE CYCLE

             TEMT = 10.
             MODE1 = 1
             call SMHTGF(MODE1,ICYC,ISPC,H,real(ICR(I)),&
                  &TEMT,POTHTG,JOSTND,DEBUG)
             if (DEBUG)write(JOSTND,*)'SMHTGF-ICYC,ISPC,H,CR,TEMT,POTHTG= '
             if (DEBUG)write(JOSTND,*)ICYC,ISPC,H,real(ICR(I)),TEMT,POTHTG

          end select

          !  END POTHTG SECTION
          !  BEGIN VIGOR ADJUSTMENT
          !  SPECIES FROM CA (9, 27) DO NOT USE VIGOR KNOCK-DOWN METOD)

          if (ISPC.eq.9.or.ISPC.eq.27) then
             HTGR = POTHTG*CON
          else
             HTGR = POTHTG*PCTRED*VIGOR*CON
          end if
          if (DEBUG) write(JOSTND,9983) X,VIGOR,HTGR,CON
          9983 format('IN REGENT X,VIGOR,HTGR,CON = ',4F10.4)

          !  RANDOM HT COMPONENT

          4 continue
          ZZRAN = 0.0
          if (DGSD.ge.1.0) ZZRAN = BACHLO(0.0,1.0,RANN)
          if ((ZZRAN .gt.0.5) .or. (ZZRAN .lt. -2.0)) go to 4
          if (DEBUG)write(JOSTND,9984) HTGR,ZZRAN,XRHGRO,SCALE
          9984 format('IN REGENT 9984 FORMAT',4(F10.4,2X))
          HTGR = (HTGR +ZZRAN*0.1)*XRHGRO * SCALE

          !     COMPUTE WEIGHTS FOR THE LARGE AND SMALL TREE HEIGHT INCREMENT
          !     ESTIMATES.  IF DBH IS LESS THAN OR EQUAL TO XMN, THE LARGE TREE
          !     PREDICTION IS IGNORED (XWT=0.0).

          XWT = (D-XMN)/(XMX-XMN)
          if (D.le.XMN.or.LESTB) XWT = 0.0

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

          5 continue

          !     ASSIGN DBH AND COMPUTE DBH INCREMENT FOR TREES WITH DBH LESS
          !     THAN 3 INCHES (COMPUTE 10-YEAR DBH INCREMENT REGARDLESS OF
          !     PROJECTION PERIOD LENGTH).

          BKPT = 3.
          if (ISPC.eq.11)BKPT = 99.
          if (D.ge.BKPT) go to 23
          HK = H + HTG(K)
          if (HK .le.4.5) then
             DG(K) = 0.0
             DBH(K) = D+0.001*HK
          else
             select case(ISPC)

                !  LOGIC FROM SORNEC 11 (HTT1,HTT2 VARIABLES)
                !  HEIGHT SET HEIGHT REGRESSION COEFFICIENTS

             case(1:8,10,12,13,32)
                if (JCR .gt.7)JCR = 7
                if (JCR .le.0) JCR = 1
                BX = HTT2(ISPC,JCR)
                if (IABFLG(ISPC).eq.1) then
                   AX = HTT1(ISPC,JCR)
                else
                   AX = AA(ISPC)
                end if
             case default

                !  SPECIES FROM ALL OTHER VARIANTS

                BX = HT2(ISPC)
                if (IABFLG(ISPC).eq.1) then
                   AX = HT1(ISPC)
                else
                   AX = AA(ISPC)
                end if
             end select

             !  DIAMETER CALCUALTIONS

             select case(ISPC)
             case(1:10,12:14,17,18,24,27,32)

                !  SO11,UT(6),AND CA SPECIES

                if (ISPC .eq.10) then
                   DK = (HK-8.31485+.59200*7.)/3.03659
                   DKK = (H-8.31485+.59200*7.)/3.03659
                else
                   DK = (BX/(ALOG(HK-4.5)-AX))-1.0
                   if (H .le.4.5) then
                      DKK = D
                   else
                      DKK = (BX/(ALOG(H-4.5)-AX))-1.0
                   end if
                   if (DEBUG)write(JOSTND,9986) AX,BX,ISPC,JCR,HK,BARK,&
                        &XRDGRO,DK,DKK
                   9986 format('IN REGENT 9986 FORMAT AX,BX,ISPC,JCR,HK',&
                        &' BARK,XRDGRO,DK,DKK= '/T12, F10.3,2X,F10.3,2X,I5,2X,&
                        &I7,5F10.3)
                end if

                !  WJ IS FROM UT SP 12

             case(11)
                DK = (HK-4.5)*10./(SITEAR(ISPC)-4.5)
                if (DK .lt.0.1) DK = 0.1
                DKK = (H-4.5)*10./(SITEAR(ISPC)-4.5)
                if (DKK .lt.0.1) DKK = 0.1
                if (H .lt.4.5) DKK = D
             case(16)

                !  ASSIGN DIAMETER INCREMENT AND SCALE FOR BARK THICKNESS AND
                !  PERIOD LENGTH.  SCALE ADJUSTMENT IS ON GROWTH IN DDS RATHER
                !  THAN INCHES OF DG TO MAINTAIN CONSISTENCY WITH GRADD.

                PPCCF = 1.
                TPCCF = PCCF(IPCCF)*PPCCF
                if (TPCCF.gt.300.0) TPCCF = 300.0
                if (TPCCF.lt.25.0) TPCCF = 25.0

                !         USES WB EQ FROM EM/SMDGF(1,H,REAL(ICR(K)),TPCCF,DKK)

                HLESS4 = H - 4.5
                DLESS3 = 0.000231 * HLESS4*real(ICR(K))&
                     &- 0.00005 * HLESS4 * TPCCF&
                     &+ 0.001711 * real(ICR(K))&
                     &+ 0.17023 * HLESS4
                DKK = DLESS3 + 0.3

                if (DEBUG)write(JOSTND,*)'I,ISPC,H,CR,TPCCF,DKK= ',&
                     &I,ISPC,H,real(ICR(K)),TPCCF,DKK

                !  PPCCF IS A PROPORTIONAL ADJUSTMENT FOR POINT CCF VALUES BASED ON
                !  CHANGE IN STAND CCF FOR THE SUBCYCLE.

                !         USES WB EQ FROM EM/CALL SMDGF(1,HK,REAL(ICR(K)),TPCCF,DK)

                HLESS4 = HK - 4.5
                DLESS3 = 0.000231 * HLESS4*real(ICR(K))&
                     &- 0.00005 * HLESS4 * TPCCF&
                     &+ 0.001711 * real(ICR(K))&
                     &+ 0.17023 * HLESS4
                DK = DLESS3 + 0.3

                IF(DEBUG)WRITE(JOSTND,*)'SMDGF CALL PARMS- ,I,ISPC,HK,CR,     &
                     &    PCCF(IPCCF),DKK= ',I,ISPC,HK,REAL(ICR(K)),PCCF(IPCCF),DK

             case(15,19:23,25,26,28:31,33)

                !  SPECIES FROM WC VARIANT
                !  SET DDUM = 1 IF THIS IS A MANAGED STAND

                DDUM = 0.0
                if (MANAGD.eq.1.or. LESTB) DDUM = 1.0

                !   BEGIN THE DIAMETER LOOKUP SECTION
                !      DK = DIAMETER WITH HTG ADDED TO THE STARTING HEIGHT
                !      DKK  = DIAMETER AT THE START OF THE PROJECTION
                !   DAT45  = DIAMETER AT 4.5 FEET PREDICTED FROM EQUATION.

                CRCODE = real(ICR(K))/10.0
                ALHT = ALOG(H)
                ALHK = ALOG(HK)
                DAT45 = RDCON(ISPC) + RDCR(ISPC)*CRCODE&
                     &+ RDLHT(ISPC)*ALOG(4.5) + RDHT(ISPC)*4.5 + RDDUM(ISPC)*DDUM

                DKK = RDCON(ISPC) + RDCR(ISPC) * CRCODE&
                     &+ RDLHT(ISPC)*ALHT + RDHT(ISPC)*H  +RDDUM(ISPC)*DDUM
                if (DKK .lt.0.0) DKK = D

                DK = RDCON(ISPC) + RDCR(ISPC) * CRCODE&
                     &+ RDLHT(ISPC)*ALHK + RDHT(ISPC)*HK + RDDUM(ISPC)*DDUM
                if (DK .lt. DKK) DK = DKK+.01
                IF(DEBUG)WRITE(JOSTND,*)'I,ISPC,DBH,H,HK,DK,DKK,CRCODE,DDUM=  &
                     &    ',I,ISPC,DBH(I),H,HK,DK,DKK,CRCODE,DDUM

             end select
             if (ISPC.eq.11.or.ISPC.eq.16.or.ISPC.eq.24)goto 300

             !  USE INVENTORY EQUATIONS IF CALIBRATION OF THE HT-DBH FUNCTION IS TURNED
             !  OFF, OR IF WYKOFF CALIBRATION DID NOT OCCUR.
             !  NOTE: THIS SIMPLIFIES TO IF(IABFLB(ISPC).EQ.1) BUT IS SHOWN IN IT'S
             !        ENTIRITY FOR CLARITY.

             if (.not.LHTDRG(ISPC) .or.&
                  &(LHTDRG(ISPC) .and. IABFLG(ISPC).eq.1)) then
                call HTDBH (IFOR,ISPC,DK,HK,1)
                if (H .le.4.5) then
                   DKK = D
                else
                   call HTDBH (IFOR,ISPC,DKK,H,1)
                end if
                if (DEBUG)write(JOSTND,*)'INV EQN DUBBING IFOR,ISPC,H,HK,DK,'&
                     &,'DKK= ',IFOR,ISPC,H,HK,DK,DKK
                if (DEBUG)write(JOSTND,*)'ISPC,LHTDRG,IABFLG= ',&
                     &ISPC,LHTDRG(ISPC),IABFLG(ISPC)
             end if
             300 continue

             !  IF CALLED FROM **ESTAB** ASSIGN DIAMETER

             if (LESTB) then
                select case(ISPC)

                   !  WC VARIANT SPECIES
                   !  ADJUST REGRESSION TO PASS THROUGH BUD WIDTH AT 4.5 FEET.

                case(15,19:23,25,26,28:31,33)

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

                if (DBH(K).lt.DIAM(ISPC).or.HK.lt.4.5) DBH(K) = DIAM(ISPC)
                DBH(K) = DBH(K)+0.001*HK
                DG(K) = DBH(K)
             else

                !  COMPUTE DIAMETER INCREMENT BY SUBTRACTION, APPLY USER
                !  SUPPLIED MULTIPLIERS, AND CHECK TO SEE IF COMPUTED VALUE
                !  IS WITHIN BOUNDS.

                ! IF THE TREE JUST REACHED 4.5 FEET, SET DKK TO PRESENT DBH.
                ! RJ 12/6/91
                if (H .lt.4.5 )DKK = D
                BARK = BRATIO(ISPC,D,H)

                if (DEBUG)write(JOSTND,*)'BARK,XRDGRO= ',BARK,XRDGRO

                select case (ISPC)
                case(15,19:23,25,26,28:31,33)

                   ! FROM WC
                   ! PROBLEM WITH HARDWOOD EQN, REDUCES TO .021*HG. SET TO RULE
                   ! OF THUMB VALUE .1*HG FOR NOW. DIXON 11-04-92
                   ! DON'T USE R.O.T. IF USING INVENTORY EQNS.  DIXON 03-31-98

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
                   DDS = DG(K)*(2.0*BARK*D+DG(K))*SCALE2
                   DG(K) = SQRT((D*BARK)**2+DDS)-BARK*D
                   if (DEBUG)write(JOSTND,*)'HARDWOOD EQU DG(K),DGMX,LHTDRG= '
                   if (DEBUG)write(JOSTND,*)DG(K),DGMX,LHTDRG(ISPC)

                case(9,27)

                   !  IN ICASCA, THE INCREMENT FOR TREES BETWEEN
                   !  1.5 AND 3 INCHES DBH IS A WEIGHTED AVERAGE OF PREDICTIONS FROM
                   !  THE LARGE AND SMALL TREE MODELS.
                   !  SCALE ADJUSTMENT IS ON GROWTH IN DDS TERMS RATHER THAN INCHES
                   !  OF DG TO MAINTAIN CONSISTENCY WITH GRADD.

                   !         NOTE: LARGE TREE DG IS ON A 10-YR BASIS; SMALL TREE DG IS ON A
                   !         FINT-YR BASIS. CONVERT SMALL TREE DG TO A 10-YR BASIS BEFORE
                   !         WEIGHTING. DG GETS CONVERTED BACK TO A FINT-YR BASIS IN **GRADD**.

                   XDWT = (D-1.5)/1.5
                   if (D.le.1.5) XDWT = 0.0
                   if (D.ge.3.0) XDWT = 1.0
                   DGSM = (DK-DKK)*BARK*XRDGRO
                   if (DGSM .lt.0.0) DGSM = 0.0
                   DDS = DGSM*(2.0*BARK*D+DGSM)*SCALE2
                   DGSM = SQRT((D*BARK)**2+DDS)-BARK*D
                   if (DGSM.lt.0.0) DGSM = 0.0
                   DG(K) = DGSM*(1.0-XDWT)+DG(K)*XDWT
                   if (DEBUG)write(JOSTND,*)'XDWT,D,DDS,DG(K),DK,DKK,DGSM= '
                   if (DEBUG)write(JOSTND,*)XDWT,D,DDS,DG(K),DK,DKK,DGSM
                   if ((DBH(K)+DG(K)).lt.DIAM(ISPC)) then
                      DG(K) = DIAM(ISPC)-DBH(K)
                   end if
                   if (DEBUG)write(JOSTND,*)'K,D,DBH,DG,DK,DKK,DGSM,XDWT='
                   if (DEBUG)write(JOSTND,*)K,D,DBH(K),DG(K),DK,DKK,DGSM,XDWT
                   go to 23

                case default
                   if (DK.lt.0.0.or. DKK.lt.0.0) then
                      DG(K) = HTG(K)*0.2*BARK*XRDGRO
                      DK = D+DG(K)
                   else
                      DG(K) = (DK-DKK)*BARK*XRDGRO
                   end if
                   if (DEBUG)write(JOSTND,*)'K,DK,DKK,DG= ',K,DK,DKK,DG(K)
                end select

                !  SCALE DIAMETER INCREMENT TO 10-YR ESTIMATE.

                ! RJ   12/6/91
                ! TO BE CONSISTENT WITH GRADD, THE SCALE ADJUSTMENT IS
                ! ON GROWTH IN DDS TERMS RATHER THAN INCHES OF DG

                if (DG(K) .lt.0.0) DG(K) = 0.0
                if (DG(K) .gt. DGMX) DG(K) = DGMX
                if (ISPC.eq.16.and.(DBH(K)+DG(K)).lt.DIAM(ISPC)) then
                   DG(K) = DIAM(ISPC)-DBH(K)
                end if

                DDS = (DG(K)*(2.0*BARK*D+DG(K))) * SCALE2
                DG(K) = SQRT((D*BARK)**2+DDS)-BARK*D
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

          !  END OF GROWTH PREDICTION LOOP

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

       !  GET THE APPROPRIATE SITE INDEX RANGE VALUES FOR THIS SPECIES.

       SLO = 0.
       SHI = 999.
       call SITERANGE (2,ISPC,SLO,SHI)

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
       if (SI .gt. SHI) SI = SHI
       if (SI .le. SLO) SI = SLO + 0.5
       if (ISPC.eq.9.or.ISPC.eq.27) then
          REGYR = 5.
       else
          REGYR = 10.
       end if
       SCALE3 = REGYR / FINTH

       !  BEGIN TREE LOOP WITHIN SPECIES.  IF MEASURED HEIGHT INCREMENT IS
       !  LESS THAN OR EQUAL TO ZERO, OR DBH IS LESS THAN 5.0, THE RECORD
       !  WILL BE EXCLUDED FROM THE CALIBRATION.

       do I3 = I1,I2
          I = IND1(I3)
          JCR = INT(real(ICR(I))/10.)
          H = HT(I)
          IPCCF = ITRE(I)

          !  DIA GT 3 INCHES INCLUDED IN OVERALL MEAN

          if (IHTG.lt.2) H = H-HTG(I)
          if (DBH(I).ge.5.0.or.H.lt.0.01) go to 60

          !  COMPUTE VIGOR MODIFIER FROM CROWN RATIO.

          X = real(ICR(I))/100.
          VIGOR = (150.0 * (X**3.0)*EXP(-6.0*X))+0.3
          if (VIGOR .gt.1.0)VIGOR = 1.0

          !  VIGOR ADJUSTMENT TO DRASTIC FOR PINYON, JUNIPER, AND OAK
          !  CUT IT BY TWO-THIRDS (FROM UT 6(AS) USED FOR SO 11(AS)

          if (ISPC.eq.11)VIGOR = 1.-((1.-VIGOR)/3.)

          !  BEGIN POTHTG SECTION

          select case(ISPC)
          case(24)
             RELSI = (SI-SLO)/(SHI-SLO)
             RSIMOD = 0.5 * (1.0 + RELSI)

             ! COMPUTE HT GROWTH AND AGE FOR ASPEN. EQN FROM WAYNE SHEPPARD RMRS.

             AG1 = (H*12.0*2.54/26.9825)**0.8509
             AG2 = AG1 + 10.0
             H2 = (26.9825*AG2**1.1752)/(2.54*12.0)
             EDH = (H2-H) * RSIMOD * RHCON(ISPC)
             EDH = EDH*2.4

             ! GROWTH RATES APPEAR HIGH, REDUCE BY 25 PERCENT. DIXON 8-27-92.

             EDH = EDH*.75
             if (EDH .lt.0.0) EDH = 0.0
             go to 9989
          case default

             ! CALL SMHTGF, THE SMALL TREE HEIGHT GROWTH ROUTINE. SMHTGF IS ALSO
             ! CALLED FROM ESSUBH TO GROW PLANTED AND NATURAL TREES FROM
             ! ESTABLISHMENT TO 5 YEARS INTO THE CYCLE

             TEMT = 10.
             MODE1 = 1
             call SMHTGF(MODE1,ICYC,ISPC,H,real(ICR(I)),&
                  &TEMT,POTHTG,JOSTND,DEBUG)

          end select

          !  END POTHTG SECTION
          !  BEGIN VIGOR ADJUSTMENT
          !  SPECIES FROM CA (9, 27) DO NOT USE THE DENSITY MODIFIER OR
          !  VIGOR MODIFIER

          if (ISPC.eq.9.or.ISPC.eq.27) then
             EDH = POTHTG*RHCON(ISPC)
          else
             EDH = POTHTG*PCTRED*VIGOR*RHCON(ISPC)
          end if

          9989 continue
          if (DEBUG)write(JOSTND,9990) X,VIGOR,FINT,EDH
          9990 format('IN REGENT X,VIGOR,FINT,EDH = ',4F10.4)
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

       call REGHCAL(ISPC,SNP,SNX,SNY,CORNEW)
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

    call REGCALOUT(CORTEM,NUMCAL,KOUT,K,KK,ISPEC,SPEC)
    91 if (DEBUG)write(JOSTND,9995)ICYC
    9995 format('LEAVING SUBROUTINE REGENT  CYCLE =',I5)
    return
    9998 continue

    !  ENTRY POINT FOR LOADING OF REGENERATION GROWTH MODEL
    !  CONSTANTS  THAT REQUIRE ONE-TIME RESOLUTION.

    do ISPC = 1,MAXSP
       RHCON(ISPC) = 1.0
       if (LRCOR2.and.RCOR2(ISPC).gt.0.0)&
            &RHCON(ISPC) = RCOR2(ISPC)
       90 continue
    end do
    return
  end subroutine REGSO

  !===============================!
  !   Compute small tree growth   !
  !===============================!
  ! TT $Id$
  subroutine REGTT(LCON)
    implicit none

    logical :: LCON
    ! Regional callees retain their native argument lists.
    interface
       subroutine SMHTGF(ISPC,I,HTGRTH,CR,TPCCF,LESTB,DEBUG)
         integer :: ISPC
         integer :: I
         real :: HTGRTH
         real :: CR
         real :: TPCCF
         logical :: LESTB
         logical :: DEBUG
       end subroutine SMHTGF
    end interface


    !  **REGENT** COMPUTES HEIGHT AND DIAMETER INCREMENTS FOR SMALL TREES.

    !  HEIGHT INCREMENT IS A FUNCTION OF HABITAT TYPE, SLOPE, ASPECT, TREE
    !  HEIGHT, BASAL AREA IN LARGER TREES, AND STAND CROWN COMPETITION FACTOR.

    !  THE HEIGHT INCREMENT MODEL IS APPLIED TO TREES THAT ARE LESS
    !  THAN THE MAXIMUM DIAMETER SPECIFIED IN ARRAY XMAX.
    !  FOR TREES THAT ARE GREATER THAN THE MINIMUM DIAMETER SPECIFIED IN
    !  ARRAY XMIN, HEIGHT INCREMENT PREDICTIONS ARE AVERAGED WITH PREDICTIONS
    !  FROM THE LARGE TREE MODEL. HEIGHT INCREMENT IS SCALED TO THE CYCLE
    !  LENGTH (FINT) BY APPLICATION OF THE VARIABLE SCALE.

    !  DIAMETER IS ASSIGNED FROM A HEIGHT DIAMETER FUNCTION WITH ADJUSTMENTS
    !  FOR RELATIVE SIZE AND STAND DENSITY.  INCREMENT IS COMPUTED BY
    !  SUBTRACTION. THE DBH INCREMENT MODEL IS APPLIED TO TREES THAT ARE LESS
    !  THAN THE BREAKPOINT DIAMETER SPECIFIED IN ARRAY BREAK IN **BLKDAT**.
    !  DIAMETER INCREMENT IS SCALED FROM A FINT-YEAR BASIS TO A 10-YR BASIS
    !  BY APPLICATION OF THE VARIABLE SCALE2. IT GETS SCALED BACK TO A
    !  FINT-YEAR BASIS IN SUBROUTINE **UPDATE**.

    !  WHEN THE TREE ATTAINS A DBH OF BREAK INCHES IN A CYCLE, **DUBSCR** IS
    !  CALLED TO ASSIGN A CROWN RATIO.

    !  **REGENT** IS CALLED FROM **CRATET** DURING CALIBRATION AND
    !  FROM **GRINCR** DURING CYCLING.  ENTRY **REGCON** IS CALLED FROM
    !  **RCON** TO LOAD MODEL PARAMETERS THAT NEED ONLY BE RESOLVED ONCE.

    !  **REGENT** IS ALSO USED TO PREDICT HEIGHT INCREMENT, DBH AND CROWN
    !  RATIO FOR TREES THAT ARE CREATED BY THE REGENERATION ESTABLISHMENT
    !  MODEL.  5-YEAR OLD TREES ARE GENERATED BY **ESTAB**, AND **REGENT**
    !  COMPUTES INCREMENTS FROM AGE 5 TO THE END OF THE CYCLE.

    ! Common blocks

    !  DIMENSIONS FOR INTERNAL VARIABLES:

    !   BANEXT -- ESTIMATED STAND BASAL AREA AT START OF EACH HEIGHT INCREMENT
    !             PREDICTION SUBCYCLE.
    !   CORTEM -- A TEMPORARY ARRAY FOR PRINTING CORRECTION TERMS.
    !     KPER -- NUMBER OF YEARS (MAX OF 5) IN EACH HEIGHT INCREMENT PREDICTION
    !             SUBCYCLE.
    !   NUMCAL -- A TEMPORARY ARRAY FOR PRINTING NUMBER OF HEIGHT
    !             INCREMENT OBSERVATIONS BY SPECIES.
    !   RDNEXT -- RELATIVE DENSITY (CCF) AT START OF EACH HEIGHT INCREMENT
    !             PREDICTION SUBCYCLE.
    !    RHCON -- CONSTANT FOR THE HEIGHT INCREMENT MODEL.  INCLUDES EFFECTS
    !             OF HABITAT TYPE, LOCATION, SLOPE, AND ASPECT AS LOADED IN
    !             RCON.  HABITAT EFFECTS ARE BORROWED FROM LARGE TREE DBH
    !             INCREMENT MODEL FOR ALL BUT TYPES 520, 530, AND 570.
    !             ADJUSTMENTS ARE THE DIFFERENCE BETWEEN THE HABITAT
    !             CONSTANT FOR A GIVEN TYPE AND THE CONSTANT FOR TYPE 520.
    !     RHLH -- THE COEFFICIENTS FOR EACH SPECIES FOR THE LN(HEIGHT)
    !             TERM IN THE HEIGHT INCREMENT MODEL.
    !     HHT1 -- SPECIES DEPENDENT CONSTANT IN THE HEIGHT-DBH MODEL USED
    !             TO ASSIGN DIAMETERS TO SMALL TREES.
    !     HHT2 -- SPECIES DEPENDENT EXPONENT IN THE HEIGHT-DBH MODEL USED
    !             TO ASSIGN DIAMETERS TO SMALL TREES.
    !     XMAX -- UPPER END OF THE RANGE OF DIAMETERS OVER WHICH HEIGHT
    !             INCREMENT PREDICTIONS FROM SMALL AND LARGE TREE MODELS
    !             ARE AVERAGED.
    !     XMIN -- LOWER END OF THE RANGE OF DIAMETERS OVER WHICH HEIGHT
    !             INCREMENT PREDICTIONS FROM THE SMALL AND LARGE TREE
    !             ARE AVERAGED.

    !  NOTE: UT AND CR LOGIC IS BASED ON A 10-YEAR REGYR

    external RANN
    ! Variables
    character(len=2) :: SPEC
    logical :: DEBUG,LSKIPH,CIVAR,UTVAR,TTVAR

    integer :: I,I1,I2,I3,ICRK,IICR,IS,IPCCF,IREFI,ISPC
    integer :: ISPEC,ITOT,IYR
    integer :: J
    integer :: K,KK,KOUT,KPER(10),KY
    integer :: L
    integer :: N,NPER,NTYR,NUMCAL(18),NYR

    real    :: AB(9),AG1,AG2,AH,AGMAX,AX
    real    :: B1,B2,BACHLO,BACON,BAJ,BAL,BANEXT(10),BARK,BAYR
    real    :: BI,BRATIO,BKPT,BX
    real    :: C1,C2,CCFYR,CI,CON,CORNEW,CORTEM(18),CR,CRCODE,CW
    real    :: D,D1,D1F,D2,D2F,DAT45,DDS,DELMAX
    real    :: DGMX,DIAM(18),DK,DKK,EDH,FNT,DGMAX(18)
    real    :: H,H1,H2,HAT3,HITE1,HITE2,HK
    real    :: HTGR,HTGR1,HTGRL,HTMAX,HTMAX2
    real    :: P,P2,P3,P4,PCTRED,PN,POTHTG,PPCCF
    real    :: R,RAN,RCR,RDJ,RDNEXT(10),REGYR,RELSI
    real    :: RSIMOD
    real    :: SCALE,SCALE2,SCALE3
    real    :: SHI,SI,SITAGE,SITHT,SJ,SLO
    real    :: SNP,SNX,SNY
    real    :: TEMAHT,TEMBA,TEMCCF,TERM,TPCCF,VIGOR
    real    :: X,XMAX(18),XMIN(18),XMN,XMX,XRDGRO,XRHGRO,XWT
    real    :: ZZRAN

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

    ! Data statements
    data DIAM/&
         &0.4,  0.3,  0.3,  0.4,  0.3,  0.2,  0.4,  0.3,  0.3,  0.5,&
         &0.3,  0.3,  0.2,  0.2,  0.3,  0.2,  0.2,  0.3/

    data XMIN/&
         &1.5,  1.5,  1.5, 90.0,  1.5,  1.5,  1.5,  1.5,  1.5,  2.0,&
         &90.0, 90.0, 90.0,  2.0,  0.5, 90.0,  1.5,  0.5/

    data XMAX/&
         &3.0,  3.0,  3.0, 99.0,  3.0,  3.0,  3.0,  3.0,  3.0,  5.0,&
         &99.0, 99.0, 99.0,  4.0,  2.0, 99.0,  3.0,  2.0/

    data REGYR/5.0/,BACON/0.005454154/

    data DGMAX/&
         &0.2,  0.2,  0.2,  2.0,  0.2,  0.2,  0.2,  0.2,  0.2, 99.0,&
         &2.0,  2.0,  2.0,  2.5,  2.5,  2.0,  0.2,  2.5/

    data AB/&
         &1.11436, -.011493, .43012E-4, -.72221E-7, .5607E-10, -.1641E-13,&
         &3*0. /

    if (LCON) go to 9998

    !  CHECK FOR DEBUG.

    call DBCHK (DEBUG,'REGENT',6,ICYC)
    if (DEBUG) write(JOSTND,9980)ICYC
    9980 format('ENTERING SUBROUTINE REGENT  CYCLE =',I5)

    !  THE SMALL TREE HEIGHT INCREMENT MODEL IS BASED ON 5-YEAR INCREMENT DATA.
    !  TO AVOID BIAS, PREDICTIONS ARE MADE FOR SUBCYCLES THAT ARE LESS THAN OR
    !  EQUAL TO 5 YEARS IN LENGTH.  THE FOLLOWING CODE DETERMINES THE NUMBER
    !  AND LENGTH OF SUBCYCLES FROM CYCLE LENGTH.

    LSKIPH = .false.
    SCALE2 = 1.
    do I = 1,10
       KPER(I) = 0
       BANEXT(I) = 0.0
       RDNEXT(I) = 0.0
       1 continue
    end do
    NTYR = INT(FINT)
    if (LSTART) NTYR = IFINTH
    if (LESTB) NTYR = NTYR-5
    if (LESTB.and.NTYR.le.0) LSKIPH = .true.
    IYR = INT(REGYR)
    NPER = NTYR/IYR
    if (MOD(NTYR,IYR).ne.0) NPER = NPER+1
    ITOT = NTYR
    N = NPER
    do I = 1,NPER
       if (N.eq.1) go to 3
       KPER(I) = ITOT/N
       ITOT = ITOT-KPER(I)
       N = N-1
       2 continue
    end do
    3 continue
    if (NPER.gt.0)KPER(NPER) = ITOT

    !  LOAD TEMPORARY VARIABLES WITH AFTER THINNING BA, CCF, AND TOPHT.

    TEMBA = ATBA
    if (ATBA .le.0.)TEMBA = BA
    TEMCCF = ATCCF
    if (ATCCF .le.0.)TEMCCF = RELDEN
    TEMAHT = ATAVH

    !  IF THIS IS THE FIRST ENTRY, BRANCH TO THE CALIBRATION SECTION.

    if (LSTART) goto 40

    !  LOAD MULTIPLIERS.

    call MULTS (3,IY(ICYC),XRHMLT)
    call MULTS(6,IY(ICYC),XRDMLT)
    if (DEBUG) write(JOSTND,8996) LESTB,LSKIPH,ITRN,ITRNIN,NPER
    8996 format('IN REGENT, LESTB = ',L1,', LSKIPH = ',L1,&
         &', ITRN = ',I4,', ITRNIN = ',I4,', NPER = ',I3)
    if (ITRN.le.0) return

    !  ESTIMATE THE CONTRIBUTION TO STAND BASAL AREA AND CCF FOR LARGE TREES AT
    !  THE START OF EACH PROJECTION SUBCYCLE.  IF CALLED FROM **ESTAB**, BRANCH
    !  TO STATEMENT 9 AND INTERPOLATE FROM OLD AND NEW DENSITY VALUES.
    !  OTHERWISE, 10-YEAR CHANGE IS COMPUTED FROM PREDICTED DBH INCREMENT AND
    !  ANNUALIZED.  THEN, ANNUAL RATE IS MULTIPLIED BY SUBCYCLE LENGTH TO
    !  DETERMINE CONTRIBUTION FOR EACH SUBCYCLE.  A ONE PERCENT ANNUAL RATE
    !  OF MORTALITY IS ASSUMED.  CONTRIBUTIONS OF SMALL TREES ARE COMPUTED AS
    !  INCREMENT PREDICTIONS ARE MADE FOR PRECEDING SUBCYCLE.

    if (LESTB) go to 8
    do J = 1,NPER
       BANEXT(J) = BA
       RDNEXT(J) = RELDEN
       4 continue
    end do
    if (DEBUG) write(JOSTND,*)'NPER,BA,RELDEN= ',NPER,BA,RELDEN
    if (NPER.eq.1) go to 11
    do I = 1,ITRN
       D1 = DBH(I)
       if (D1.lt.3.0) go to 6
       IS = ISP(I)
       P = PROB(I)
       D2 = D1+DG(I)/BRATIO(IS,D1,HT(I))
       B1 = BACON*D1*D1
       B2 = BACON*D2*D2
       call CCFCAL(IS,D1,HT(I),ICR(I),P,.false.,C1,CW,1)
       call CCFCAL(IS,D2,HT(I),ICR(I),P,.false.,C2,CW,1)
       BI = (B2-B1)/10.0
       CI = (C2-C1)/10.0
       K = 0
       do J = 2,NPER
          K = K+KPER(J-1)
          PN = P*0.985**K
          RDNEXT(J) = RDNEXT(J)+K*CI/P*PN
          BANEXT(J) = BANEXT(J)+K*BI*PN
          5 continue
       end do
       6 continue
    end do
    go to 11

    !  IF CALLED FROM **ESTAB**, DIAMETERS AND DENSITY STATISTICS HAVE
    !  BEEN UPDATED AND DENSITIES CORRESPONDING TO BEGINNINGS OF SUBCYCLES
    !  CAN BE INTERPOLATED FROM DIFFERENCE BETWEEN OLD AND NEW STATS.  THE
    !  TREES ARE SENT TO REGENT WITH A HEIGHT CORRESPONDING TO 5 YEARS AFTER
    !  THE START OF THE CYCLE.

    8 continue
    BAYR = 0.0
    CCFYR = 0.0
    if (LSKIPH) go to 9
    BAYR = (BA-TEMBA)/ITOT
    CCFYR = (RELDEN-TEMCCF)/ITOT
    9 continue
    NYR = 5
    do J = 1,NPER
       RDNEXT(J) = TEMCCF+NYR*CCFYR
       BANEXT(J) = TEMBA+NYR*BAYR
       NYR = NYR+KPER(J)
       10 continue
    end do
    11 continue

    !  STORE INITIAL HEIGHTS IN WK3; DUB CROWN RATIO FOR NEWLY ESTABLISHED
    !  SEEDLINGS.

    do I = 1,ITRN
       if (LESTB.and.I.ge.ITRNIN) then
          IPCCF = ITRE(I)
          CR = 0.89722 - 0.0000461*PCCF(IPCCF)
          12 continue
          RAN = BACHLO(0.0,1.0,RANN)
          if (RAN .lt. -1.0.or. RAN .gt.1.0) go to 12
          CR = CR + 0.07985 * RAN
          if (CR .gt..90) CR = .90
          if (CR .lt..20) CR = .20
          ICR(I) = INT((CR*100.0)+0.5)
          IF(DEBUG)WRITE(JOSTND,*)'DUB CROWN ESTAB TREES I,PCCF,RAN,CR,I  &
               &  CR= ',I,PCCF(IPCCF),RAN,CR,ICR(I)
       end if
       WK3(I) = HT(I)
       WK5(I) = DBH(I)
       13 continue
    end do

    !  DELMAX IS USED TO COMPUTE A DENSITY-DEPENDENT ADJUSTMENT FOR
    !  THE HEIGHT-DIAMETER FUNCTION.  DELMAX IS BASED ON AVERAGE HEIGHT AND
    !  CCF AT THE BEGINNING OF THE CYCLE AND DOES NOT VARY BY SUBCYCLE.

    if (LESTB) then
       R = TEMCCF
       AH = TEMAHT
    else
       R = RELDEN
       AH = AVH
    end if
    DELMAX = (AH/36.0)*(0.01232*R-1.75)
    if (DELMAX.gt.0.0) DELMAX = 0.0

    ! COMPUTE DENSITY MODIFIER FROM CCF AND TOP HEIGHT FOR CR & UT VARIANT

    X = AH*(R/100.0)
    if (X .gt.300.0) X = 300.0
    PCTRED = 1.11436 + X*(-.011493 + X*(.43012E-4 + X*(-.72221E-7&
         &+ X*(.5607E-10- X*0.1641E-13))))
    if (PCTRED .gt.1.0) PCTRED = 1.0
    if (PCTRED .lt.0.01) PCTRED = 0.01
    if (DEBUG) write(JOSTND,9982) AH,R,X,PCTRED
    9982 format('IN REGENT AVHT,CCF,X,PCTRED = ',4F10.4)
    if (DEBUG) write(JOSTND,8995) R,AH,DELMAX
    8995 format('R = ',F10.4,', AH = ',F10.4,', DELMAX = ',F10.4)

    !  ENTER LOOP FOR NEXT SUBCYCLE.
    !  AFTER ALL SUBCYCLES ARE COMPLETE, THE END OF CYCLE HEIGHT SHOULD
    !  BE IN ARRAY WK3 AND SCALED TO THE APPROPRIATE CYCLE LENGTH.
    !  END OF CYCLE DIAMETER IS STORED IN ARRAY WK5

    KY = 0
    do J = 1,NPER
       KY = KY+KPER(J)
       BAJ = BANEXT(J)
       RDJ = RDNEXT(J)

       !  PPCCF IS A PROPORTIONAL ADJUSTMENT FOR POINT CCF VALUES BASED ON
       !  CHANGE IN STAND CCF FOR THE SUBCYCLE.

       PPCCF = 0.0
       if (RELDEN.gt.0.0) PPCCF = 1.0+(RDJ-RELDEN)/RELDEN

       !  PROCESS ALL TREES WITHIN THIS SUBCYCLE IN SPECIES ORDER.  LOAD
       !  SPECIES DEPENDENT CONSTANTS.

       do ISPC = 1,MAXSP

          !  GET THE APPROPRIATE SITE INDEX RANGE VALUES FOR THIS SPECIES.

          SLO = 0.
          SHI = 999.
          call SITERANGE (2,ISPC,SLO,SHI)

          I1 = ISCT(ISPC,1)
          if (I1.eq.0) go to 16
          I2 = ISCT(ISPC,2)
          BKPT = BREAK(ISPC)

          !  ASSIGN VARIANT ACCORDING TO SPECIES
          !  SET SCALE AND SCALE2 DEPENDING ON WHAT VARIANT THE EQUATIONS FOR
          !  THE SPECIES CAME FROM.

          CIVAR = .false.
          TTVAR = .false.
          UTVAR = .false.
          select case (ISPC)
          case(1:3,5:9,17)
             TTVAR = .true.
             SCALE = FLOAT(KPER(J))/REGYR
             if (.not.LSKIPH)SCALE2 = YR/FLOAT(NTYR)
          case(10)
             CIVAR = .true.
             SCALE = FLOAT(NTYR)/REGYR
             if (.not.LSKIPH)SCALE2 = YR/FLOAT(NTYR)
          case(4,11:16,18)
             UTVAR = .true.
             SCALE = FLOAT(NTYR)/YR
             if (.not.LSKIPH)SCALE2 = YR/FLOAT(NTYR)
          end select
          if (DEBUG)write(JOSTND,*)'CIVAR,TTVAR,UTVAR,LSKIPH,SCALE,',&
               &'SCALE2= ',CIVAR,TTVAR,UTVAR,LSKIPH,SCALE,SCALE2
          if (TTVAR .and. LSKIPH)goto 16

          !  SPECIES BASED ON A 10-YR REGYR ONLY NEED ONE PASS THROUGH THIS LOGIC

          if ((ISPC.eq.6.or. UTVAR .or. CIVAR) .and. J.gt.1)go to 16

          XRHGRO = XRHMLT(ISPC)
          XRDGRO = XRDMLT(ISPC)
          if (CIVAR) then
             FNT = FINT
             if (LESTB) then
                if (FINT.le.5.0) then
                   LSKIPH = .true.
                else
                   FNT = FNT-5.0
                end if
             end if
             CON = RHCON(ISPC) * EXP(HCOR(ISPC))
          else
             CON = RHCON(ISPC)*EXP(HCOR(ISPC))
          end if
          XMX = XMAX(ISPC)
          XMN = XMIN(ISPC)

          !  COMPUTE POTENTIAL GROWTH FROM SITE INDEX AND SPECIES.

          SI = SITEAR(ISPC)
          if (SI .gt. SHI) SI = SHI
          if (SI .le. SLO) SI = SLO + 0.5
          RELSI = (SI-SLO)/(SHI-SLO)
          RSIMOD = 0.5 * (1.0 + RELSI)
          X = RELSI*100.0
          SJ = SITEAR(ISPC)
          if (DEBUG) write(JOSTND,9981) SI,RELSI,X,ISPC
          9981 format('IN REGENT SI,RELSI,X,ISPC = ',&
               &F7.2,F10.3,F10.4,I6,F10.3)

          !  PROCESS NEXT TREE RECORD.  STORE INTERMEDIATE HEIGHTS IN WK3.

          do I3 = I1,I2

             !  BYPASS INCREMENT CALCULATION IF DBH IS GREATER THAN XMX OR IF
             !  CALL IS FROM **ESTAB** AND TREE SUBSCRIPT IS LESS THAN ITRNIN.

             I = IND1(I3)
             D = DBH(I)
             H = HT(I)
             if (DEBUG)write(JOSTND,*)'ISPC,I,D,XMX,LESTB,ITRNIN= ',&
                  &ISPC,I,D,XMX,LESTB,ITRNIN
             if (D.ge.XMX) go to 15

             if (LESTB .and. I.lt.ITRNIN) go to 15
             P = PROB(I)
             H1 = WK3(I)
             D1 = WK5(I)
             BAL = BAJ*(100.0-PCT(I))*0.0001
             call CCFCAL(ISPC,D,H1,ICR(I),P,.false.,C1,CW,1)
             B1 = BACON*D*D
             BARK = BRATIO(ISPC,D,H)

             if (UTVAR) then
                if (ISPC .eq.14) then
                   SITAGE = 0.0
                   SITHT = 0.0
                   AGMAX = 0.0
                   HTMAX = 0.0
                   HTMAX2 = 0.0
                   D1F = DBH(I)
                   D2F = 0.0
                   if (DEBUG)write(JOSTND,*)'IN HTGF, CALLING FINDAG I= ',I
                   call FINDAG(I,ISPC,D1F,D2F,H,SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,&
                        &DEBUG)

                   if (LESTB)SITAGE = ABIRTH(I)
                   HITE1 = 26.9825 * SITAGE**1.1752
                   AG2 = SITAGE+10.0
                   HITE2 = 26.9825 * AG2**1.1752
                   HTGRL = (HITE2-HITE1)/(2.54*12.0) * RSIMOD * CON

                   ! GROWTH RATES APPEAR HIGH, REDUCE BY 25 PERCENT. DIXON 8-27-92

                   HTGRL = HTGRL * .75
                   if (DEBUG)write(JOSTND,*)'I,SITAGE,AG2,HITE1,HITE2,HTGRL,',&
                        &'RSIMOD,CON= ',I,SITAGE,AG2,HITE1,HITE2,HTGRL,RSIMOD,CON
                else
                   POTHTG = ((SJ/5.0)*(SJ*1.5-H)/(SJ*1.5))* 0.83
                   if (DEBUG)write(JOSTND,*)'I,SJ,H,POTHTG= ',&
                        &I,SJ,H,POTHTG
                end if
             end if

             !  DETERMINE POINT CCF FOR THIS SUBCYCLE (BOUND BETWEEN 25 AND 300).

             IPCCF = ITRE(I)
             TPCCF = PCCF(IPCCF)*PPCCF
             if (TPCCF.gt.300.0) TPCCF = 300.0
             if (TPCCF.lt.25.0) TPCCF = 25.0
             CR = FLOAT(ICR(I))

             !  CI VARIANT CODE
             !  COMPUTE ADJUSTMENT FOR BIAS, DENSITY, AND RELATIVE SIZE.

             if (CIVAR) then
                IICR = ((ICR(I)-1)/10)+1
                if (IICR.gt.9) IICR = 9
                RCR = FLOAT(IICR)

                !  COMPUTE HEIGHT INCREMENT.  STORE UPDATED HEIGHT IN WK3.

                HTGRL = 2.764559 - 0.009643*BA + 0.025303*RCR*RCR

                if (DEBUG)write(JOSTND,*)'CI SECTION, I,BA,RCR,HTGRL= ',&
                     &I,BA,RCR,HTGRL

                !  TT VARIANT CODE

             else if (TTVAR) then
                call SMHTGF(ISPC,I,HTGRL,CR,TPCCF,LESTB,DEBUG)
                if (ISPC .eq.6) then
                   RELSI = (SITEAR(6)-30.)/70.
                   if (RELSI .gt.1.0) RELSI = 1.0
                   if (RELSI .lt.0.0) RELSI = 0.0
                   RSIMOD = 0.5*(1.0+RELSI)
                   HTGRL = HTGRL*RSIMOD
                end if

                !  UT VARIANT CODE

             else if (UTVAR) then
                X = CR/100.
                VIGOR = (150.0 * (X**3.0)*EXP(-6.0*X))+0.3
                if (VIGOR .gt.1.0)VIGOR = 1.0

                !  VIGOR ADJUSTMENT TO DRASTIC FOR PINYON, JUNIPER, CUT IT BY TWO-THIRDS

                select case (ISPC)
                case(4,11,12)
                   VIGOR = 1.-((1.-VIGOR)/3.)
                end select

                if (ISPC .ne.14) HTGRL = POTHTG*PCTRED*VIGOR*CON
                if (DEBUG) write(JOSTND,*)'UT SECTION, I,POTHTG,PCTRED,',&
                     &'VIGOR,CON,X,HTGRL= ',I,POTHTG,PCTRED,VIGOR,CON,X,HTGRL
             end if

             ! H2 IS THE END OF CYCLE HEIGHT ON A FINT-YEAR BASIS
             ! UT ESTIMATES ARE 10-YEAR ESTIMATES AND THUS ADJUSTED BY FINT/10

             if (CIVAR) then
                H2 = H1+HTGRL*SCALE*XRHGRO*CON
             else if (TTVAR) then
                H2 = H1+HTGRL*SCALE*XRHGRO*CON
             else
                H2 = H1+HTGRL*SCALE
             end if
             WK3(I) = H2
             if (DEBUG)write(JOSTND,*)'I,H1,HTGRL,H2,KPER,REGYR,',&
                  &'XRHGRO,WK4,CON= ',&
                  &I,H1,HTGRL,H2,KPER(J),REGYR,XRHGRO,CON,WK4(I)

             !  UPDATE DENSITY FOR NEXT SUBCYCLE.  IF ONLY ONE SUBCYCLE OR
             !  IF INITIAL DBH IS GREATER THAN 3 INCHES, BYPASS DENSITY UPDATE.
             !  DIAMETER IS ADJUSTED FOR DENSITY AND USER-SUPPLIED MULTIPLIER.
             !  INCREMENT IS DETERMINED FROM THE DIFFERENCE BETWEEN COMPUTED
             !  DIAMETERS AND IS BOUNDED TO BE GREATER THAN OR EQUAL TO ZERO.

             if (CIVAR) then
                WK3(I) = H2
                D2 = D
                go to 15
             else if (TTVAR) then
                call SMDGF(ISPC,H2,CR,PCCF(ITRE(I)),D2)
                D2 = D2*XRDGRO
                if (D2.lt.DIAM(ISPC)) D2 = DIAM(ISPC)
                if (DEBUG)write(JOSTND,*)'TT SECTION, I,ISPC,CR,PCCF,H2,D2,',&
                     &'XRDGRO= ',I,ISPC,CR,PCCF(ITRE(I)),H2,D2,XRDGRO
                WK3(I) = H2
                WK5(I) = D2
                if (J.lt.NPER .and. D.lt.3.0.and. H2.gt.4.5) then

                   !  COMPUTE ADJUSTMENT TO STAND BASAL AREA AND CCF FOR NEXT SUBCYCLE.
                   !  ASSUME A 1.5 PERCENT ANNUAL MORTALITY RATE.

                   call CCFCAL(ISPC,D1,H1,ICR(I),P,.false.,C1,CW,1)
                   call CCFCAL(ISPC,D2,H2,ICR(I),P,.false.,C2,CW,1)
                   RDNEXT(J+1) = RDNEXT(J+1)+KY*(C2-C1)/10.*0.985**KY
                   BANEXT(J+1) = BANEXT(J+1)+(BACON*(D2*D2-D1*D1))*P*0.985**KY
                   if (DEBUG) write(JOSTND,8994) I,D2,BANEXT(J+1),RDNEXT(J+1)
                   8994 format('UPDATE DENSITY: TREE = ',I4,', D2 = ',F8.4,&
                        &', BANEXT(J+1) = ',F8.2,', RDNEXT(J+1) = ',F8.2)

                   !  END OF TREE LOOP WITHIN SPECIES. PRINT DEBUG IF REQUESTED.

                end if
                go to 14

                !  UTAH VARIANT SPECIES

             else
                WK3(I) = H2
                D2 = D
                if ((ISPC.eq.13.or. ISPC.eq.15.or. ISPC.eq.18) .and. D.ge.BKPT)&
                     &go to 15
                if (H2.le.4.5) then
                   D2 = D+0.001*H2
                   WK5(I) = D2
                end if
                if (DEBUG)write(JOSTND,*)'I,D,D2,H2,SITEAR= ',&
                     &I,D,D2,H2,SITEAR(ISPC)
             end if

             !  STORE UPDATED DIAMETER IN WK5

             WK5(I) = D2
             if (TTVAR.and.H2.lt.4.5) goto 8897

             !  COMPUTE ADJUSTMENT TO STAND BASAL AREA AND CCF FOR NEXT SUBCYCLE.
             !  ASSUME A 1.5 PERCENT ANNUAL MORTALITY RATE.

             call CCFCAL(ISPC,D2,H2,ICR(I),P,.false.,C2,CW,1)
             RDNEXT(J+1) = RDNEXT(J+1)+KY*(C2-C1)/10.*0.985**KY
             BANEXT(J+1) = BANEXT(J+1)+(BACON*D2*D2-B1)*P*0.985**KY
             8897 continue
             if (DEBUG) write(JOSTND,8997) I,D2,BANEXT(J+1),RDNEXT(J+1)
             8997 format('UPDATE DENSITY: TREE = ',I4,', D2 = ',F8.4,&
                  &', BANEXT(J+1) = ',F8.2,', RDNEXT(J+1) = ',F8.2)

             !  END OF TREE LOOP WITHIN SPECIES. PRINT DEBUG IF REQUESTED.

             14 continue
             if (.not.DEBUG) go to 15
             write(JOSTND,8999) J,KPER(J),ISPC,I,H1,H2,RDJ,BAL,&
                  &BAJ,PCT(I)
             8999 format('IN REGENT L1: SUBC=',I2,', KPER=',I2,', SP=',I2,&
                  &', I=',I4,', H1=',F8.4,', H2=',F8.4,/T4,'RDJ=',&
                  &F8.4,', BAL=',F8.4,', BAJ=',F8.4,', PCT=',F8.4)
             15 continue
          end do

          !  END OF SPECIES LOOP WITHIN SUBCYCLE

          16 continue
       end do
       if (DEBUG)write(16,*)'AFTER 16 CIVAR,TTVAR,UTVAR= ',&
            &CIVAR,TTVAR,UTVAR

       !  END OF SUBCYCLE LOOP.  END OF PERIOD HEIGHT IS STORED IN WK3.

       17 continue
    end do

    !  AFTER LAST SUBCYCLE, COMPUTE ACTUAL HEIGHT INCREMENT FOR FULL CYCLE BY
    !  SUBTRACTION; CONVERT BACK TO LOG SCALE TO ADD IN RANDOM ERROR.

    do ISPC = 1,MAXSP
       I1 = ISCT(ISPC,1)
       if (I1.eq.0) go to 30
       I2 = ISCT(ISPC,2)
       BKPT = BREAK(ISPC)

       !  ASSIGN VARIANT ACCORDING TO SPECIES

       CIVAR = .false.
       TTVAR = .false.
       UTVAR = .false.
       select case (ISPC)
       case(1:3,5:9,17)
          TTVAR = .true.
          SCALE = YR/FINT
          if (.not.LSKIPH)SCALE2 = YR/FLOAT(NTYR)
       case(10)
          CIVAR = .true.
          SCALE = FLOAT(NTYR)/REGYR
          if (.not.LSKIPH)SCALE2 = YR/FLOAT(NTYR)
       case(4,11:16,18)
          UTVAR = .true.
          SCALE = FLOAT(NTYR)/YR
          if (.not.LSKIPH)SCALE2 = YR/FLOAT(NTYR)
       end select

       XRDGRO = XRDMLT(ISPC)
       XMX = XMAX(ISPC)
       XMN = XMIN(ISPC)
       DGMX = DGMAX(ISPC)
       if (TTVAR)DGMX = FINT*DGMAX(ISPC)
       if (DEBUG)write(16,*)'ENTERING 25 LOOP, CIVAR,TTVAR,UTVAR,',&
            &'XMX,XMN,DGMX,LSKIPH,SCALE,SCALE2= ',&
            &CIVAR,TTVAR,UTVAR,XMX,XMN,DGMX,LSKIPH,SCALE,SCALE2

       do I3 = I1,I2
          I = IND1(I3)
          D = DBH(I)
          CR = FLOAT(ICR(I))
          if (D.ge.XMX) go to 25
          if (LESTB .and. I.lt.ITRNIN) go to 25
          K = I
          L = 0
          H = HT(I)
          HK = WK3(I)

          D1 = DIAM(ISPC)
          DK = WK5(I)
          HTGR = HK-H
          if (TTVAR.and.D.lt.D1)D = D1

          !  IF CALLED FROM **ESTAB** AND TOTAL CYCLE LENGTH IS LESS THAN OR EQUAL
          !  TO 5 YEARS, SKIP HEIGHT INCREMENT CALCULATION BUT ASSIGN CR AND DBH.

          if ((CIVAR .or. UTVAR) .and. LSKIPH) then
             HTG(K) = 0
             go to 20
          else if (TTVAR .and. LSKIPH) then
             IPCCF = ITRE(I)
             TPCCF = PCCF(IPCCF)
             if (H.ge.4.5) then
                call SMDGF(ISPC,H,CR,TPCCF,DK)
                DK = DK*XRDGRO
                if (DK.lt.D) DK = D
                DBH(I) = DK
                DG(I) = DK
                if (DG(I).gt.DGMX) DG(I) = DGMX
             end if
             go to 25
          end if
          if (TTVAR) then
             HK = WK3(I)
             DK = WK5(I)

             !  IF CALLED FROM **ESTAB** AND TOTAL CYCLE LENGTH IS LESS THAN OR EQUAL
             !  TO 5 YEARS, SKIP HEIGHT INCREMENT CALCULATION BUT ASSIGN CR AND DBH.

             HTG(I) = 0.0
             HTGR = HK-H
          end if
          if (CIVAR) then
             HTGR1 = HK-H
             if (HTGR1.lt.0.0) HTGR1 = 0.0
          end if

          !  DRAW RANDOM INCREMENT GIVEN EXPECTED VALUE, HTGR, AND STD. DEV.,
          !  HSIGMA.  BOUND DRAW TO (-1.5*SD < ZZRAN < 1.0*SD); IF TRIPLING,
          !  EACH TRIPLE GETS A NEW RANDOM NUMBER.  DO NOT TRIPLE IF CALLING
          !  FROM **ESTAB**.  DO NOT APPLY HEIGHT GROWTH MULTIPLIER; MULTIPLIER
          !  WAS USED DURING SUBCYCLING.

          ICRK = ICR(I)
          918 continue
          if (.not.TTVAR) then
             if (CIVAR) then
                920 continue
                ZZRAN = 0.0
                if (DGSD.ge.1.0) ZZRAN = BACHLO(0.0,1.0,RANN)
                if ((ZZRAN .gt.0.5) .or. (ZZRAN .lt. -2.0)) go to 920
                HTGR = (HTGR +ZZRAN*0.1*SCALE)*XRHGRO * CON
                if (DEBUG)write(JOSTND,*)'CI SECTION K,ZZRAN,XRHGRO,SCALE,',&
                     &'CON,HTGR= ',K,ZZRAN,XRHGRO,SCALE,CON,HTGR

                ! UTAH VARIANT LOGIC

             else
                919 continue
                ZZRAN = 0.0
                if (DGSD.ge.1.0) ZZRAN = BACHLO(0.0,1.0,RANN)
                if ((ZZRAN .gt.0.5) .or. (ZZRAN.lt.-2.0)) goto 919
                if (DEBUG)write(JOSTND,9984) HTGR,ZZRAN,XRHGRO,WK4(I)
                9984 format('IN REGENT 9984 FORMAT',4(F10.4,2X))
                select case (ISPC)
                case(4,11,12,14)
                   HTGR = (HTGR +ZZRAN*0.1*(FLOAT(NTYR)/10.))*XRHGRO
                case(15,18)
                   HTGR = (HTGR +ZZRAN*0.2*(FLOAT(NTYR)/10.))*XRHGRO*WK4(I)
                case(13,16)
                   HTGR = (HTGR +ZZRAN*0.1*(FLOAT(NTYR)/10.))*XRHGRO
                end select
                if (DEBUG)write(JOSTND,*)'UT SECTION K,ZZRAN,XRHGRO,',&
                     &'HTGR= ',K,ZZRAN,XRHGRO,HTGR

                ! THIS CHECK ADDED BY DIXON 3/4/09 TO PREVENT NEGATIVE HEIGHT GROWTH
                ! MAY NEED A MORE ELEGANT FIX AT SOME POINT; BUT IT WORKS.

                if (HTGR .lt.0.1) then
                   HTGR = 0.1
                   if (DEBUG)write(JOSTND,*)'CR/UT RESETTING HTGR ',&
                        &'TO 0.1 FOOT'
                end if

             end if
          end if

          !  COMPUTE WEIGHTS FOR THE LARGE AND SMALL TREE HEIGHT INCREMENT
          !  ESTIMATES.  IF DBH IS LESS THAN OR EQUAL TO XMN OR IF CALLED
          !  FROM **ESTAB**, THE LARGE TREE PREDICTION IS IGNORED (XWT=0.0).

          XWT = 0.0
          if (D.le.XMN.or.LESTB) go to 19
          XWT = (D-XMN)/(XMX-XMN)
          19 continue
          if (DEBUG)write(JOSTND,*)'WEIGHTING K,HTGR,HTG,XWT= ',&
               &K,HTGR,HTG(K),XWT
          HTG(K) = HTGR*(1.0-XWT) + XWT*HTG(K)

          ! CHECK FOR SIZE CAP COMPLIANCE.

          if ((H+HTG(K)).gt.SIZCAP(ISPC,4)) then
             HTG(K) = SIZCAP(ISPC,4)-H
             if (HTG(K) .lt.0.1) HTG(K) = 0.1
          end if

          20 continue

          !  ASSIGN DBH AND COMPUTE DBH INCREMENT FOR TREES WITH DBH LESS
          !  THAN BREAKPOINT.  COMPUTE 10-YEAR DBH INCREMENT REGARDLESS OF
          !  PROJECTION PERIOD LENGTH.  BYPASS INCREMENT CALCULATION IF
          !  D GE BREAKPOINT.

          if (D .ge. BKPT) go to 23
          HK = H+HTG(K)
          if (.not.TTVAR .and. HK.lt.4.5) then
             DG(K) = 0.0
             DBH(K) = D+0.001*HK
          else
             if (CIVAR) then
                DK = EXP( -1.10700 + 0.830144*ALOG(HK) )
                if (H .le.4.5) then
                   DKK = D
                else
                   DKK = EXP( -1.10700 + 0.830144*ALOG(H) )
                end if
                if (DEBUG)write(JOSTND,*)'I,ISPC,H,HK,DK,DKK,XRDGRO= ',&
                     &I,ISPC,H,HK,DK,DKK,XRDGRO
             else if (UTVAR) then

                select case(ISPC)
                case(4,11,12)
                   DK = (HK-4.5)*10./(SITEAR(ISPC)-4.5)
                   if (DK .lt.0.1) DK = 0.1
                   DKK = (H-4.5)*10./(SITEAR(ISPC)-4.5)
                   if (DKK .lt.0.1) DKK = 0.1
                   if (H .lt.4.5) DKK = D
                case(13:16,18)
                   BX = HT2(ISPC)
                   if (IABFLG(ISPC).eq.1) then
                      AX = HT1(ISPC)
                   else
                      AX = AA(ISPC)
                   end if

                   !  SPECIES FROM SO (ORIGINALY FROM WC) VARIANT

                   if (ISPC.eq.13.or. ISPC.eq.16) then

                      !   BEGIN THE DIAMETER LOOKUP SECTION
                      !      DK = DIAMETER WITH HTG ADDED TO THE STARTING HEIGHT
                      !      DKK  = DIAMETER AT THE START OF THE PROJECTION
                      !   DAT45  = DIAMETER AT 4.5 FEET PREDICTED FROM EQUATION.

                      CRCODE = FLOAT(ICR(K))/10.0
                      DAT45 = 3.1020 + 0.0210*4.5

                      DKK = 3.1020 + 0.0210*H
                      if (DKK .lt.0.0) DKK = D

                      DK = 3.1020 + 0.0210*HK
                      if (DK .lt. DKK) DK = DKK+.01
                      if (DEBUG)write(JOSTND,*)'I,ISPC,DBH,H,HK,DK,DKK,CRCODE,',&
                           &'= ',I,ISPC,DBH(I),H,HK,DK,DKK,CRCODE

                      !  USE INVENTORY EQUATIONS IF CALIBRATION OF THE HT-DBH FUNCTION IS TURNED
                      !  OFF, OR IF WYKOFF CALIBRATION DID NOT OCCUR.
                      !  NOTE: THIS SIMPLIFIES TO IF(IABFLB(ISPC).EQ.1) BUT IS SHOWN IN IT'S
                      !        ENTIRITY FOR CLARITY.

                      if (.not.LHTDRG(ISPC) .or.&
                           &(LHTDRG(ISPC) .and. IABFLG(ISPC).eq.1)) then
                         if (ISPC .eq.16) then
                            P2 = 1709.7229
                            P3 = 5.8887
                            P4 = -0.2286
                         else
                            P2 = 76.5170
                            P3 = 2.2107
                            P4 = -0.6365
                         end if
                         HAT3 = 4.5 + P2 * EXP(-1.*P3*3.0**P4)
                         if (HK .ge. HAT3) then
                            DK = EXP( ALOG((ALOG(HK-4.5)-ALOG(P2))/(-1.*P3)) *&
                                 &1./P4)
                         else
                            DK = (((HK-4.51)*2.7)/(4.5+P2*EXP(-1.*P3*(3.**P4))&
                                 &-4.51))+0.3
                         end if
                         if (H .le.4.5) then
                            DKK = D
                         else
                            if (H .ge. HAT3) then
                               DKK = EXP( ALOG((ALOG(H-4.5)-ALOG(P2))/(-1.*P3)) *&
                                    &1./P4)
                            else
                               DKK = (((H-4.51)*2.7)/(4.5+P2*EXP(-1.*P3*(3.**P4))&
                                    &-4.51))+0.3
                            end if
                         end if
                         if (DEBUG)write(JOSTND,*)'INV EQN DUBBING ISPC,H,HK,DK,'&
                              &,'DKK= ',ISPC,H,HK,DK,DKK
                         if (DEBUG)write(JOSTND,*)'ISPC,LHTDRG,IABFLG= ',&
                              &ISPC,LHTDRG(ISPC),IABFLG(ISPC)
                      end if
                   else
                      DK = (BX/(ALOG(HK-4.5)-AX))-1.0
                      if (DK .lt.0.1) DK = 0.1
                      if (H .le.4.5) then
                         DKK = D
                      else
                         DKK = (BX/(ALOG(H-4.5)-AX))-1.0
                      end if
                      if (DEBUG)write(JOSTND,9986) AX,BX,ISPC,HK,BARK,&
                           &XRDGRO,DK,DKK
                      9986 format('IN REGENT 9986 FORMAT AX,BX,ISPC,HK',&
                           &' BARK,XRDGRO,DK,DKK= '/T12, F10.3,2X,F10.3,2X,I5,2X,&
                           &5F10.3)
                   end if
                end select

             else if (TTVAR) then

                !  ASSIGN DIAMETER INCREMENT AND SCALE FOR BARK THICKNESS AND
                !  PERIOD LENGTH.  SCALE ADJUSTMENT IS ON GROWTH IN DDS RATHER
                !  THAN INCHES OF DG TO MAINTAIN CONSISTENCY WITH GRADD.

                if (HK .ge.4.5) then
                   call SMDGF(ISPC,HT(I),CR,PCCF(ITRE(I)),DKK)
                   if (DEBUG)write(JOSTND,*)'SMDGF CALL PARMS = ',I,ISPC,HT(I),&
                        &CR,PCCF(ITRE(I)),DKK
                   !            IF(DKK.LT.DIAM(ISPC))DKK=DIAM(ISPC)
                   BARK = BRATIO(ISPC,D,HT(K))
                   DG(K) = (DK-DKK)*BARK
                   DDS = DG(K)*(2.0*BARK*D+DG(K))*SCALE2
                   if (DEBUG)write(JOSTND,*)'K,D,DKK,DK,BARK,DDS,DG(K)= ',&
                        &K,D,DKK,DK,BARK,DDS,DG(K)
                   DG(K) = SQRT((D*BARK)**2+DDS)-BARK*D
                else
                   DG(K) = 0.0
                end if
             end if

             !       IF CALLING FROM **ESTAB** ASSIGN DIAMETER AND DIAMETER
             !       INCREMENT.

             if (LESTB) then

                select case (ISPC)
                case(13,16)
                   if (DAT45.gt.0.0.and. HK.ge.4.5.and. LHTDRG(ISPC) .and.&
                        &IABFLG(ISPC).eq.0) then
                      DBH(K) = DK - DAT45 + DIAM(ISPC)
                   else
                      DBH(K) = DK
                   end if
                case default
                   DBH(K) = DK
                end select

                if (TTVAR.and.DG(K).gt.DGMX)DG(K) = DGMX
                if (UTVAR .or. CIVAR) then
                   if (DBH(K).lt.DIAM(ISPC)) DBH(K) = DIAM(ISPC)
                   DBH(K) = DBH(K)+0.001*HK
                   DG(K) = DBH(K)
                end if
             else

                !         DIAMETER INCREMENT IS THE DIFFERENCE BETWEEN COMPUTED
                !         DIAMETERS.  MULTIPLIER IS APPLIED TO DIAMETER INCREMENT
                !         AT THIS POINT.

                if (.not.TTVAR) then
                   if (CIVAR) then
                      if (H .lt.4.5 )DKK = D
                      if (DK.lt.0.0.or. DKK.lt.0.0) then
                         DG(K) = HTG(K)*0.2*BARK*XRDGRO
                         DK = D+DG(K)
                      else
                         DG(K) = (DK-DKK)*BARK*XRDGRO
                      end if
                      if (DG(K) .lt.0.0) DG(K) = 0.0
                   else if (UTVAR) then

                      select case (ISPC)

                         ! FROM SO (ORIGINALLY WC)
                         ! PROBLEM WITH HARDWOOD EQN, REDUCES TO .021*HG. SET TO RULE
                         ! OF THUMB VALUE .1*HG FOR NOW. DIXON 11-04-92
                         ! DON'T USE R.O.T. IF USING INVENTORY EQNS.  DIXON 03-31-98

                         ! IF THE TREE JUST REACHED 4.5 FEET, SET DKK TO PRESENT DBH.
                         ! RJ 12/6/91

                      case(13,16)
                         if (H .lt.4.5 )DKK = D
                         BARK = BRATIO(ISPC,D,H)
                         if (DEBUG)write(JOSTND,*)'BARK,XRDGRO= ',BARK,XRDGRO
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
                         if (DEBUG)write(JOSTND,*)'HARDWOOD EQU DG(K),DGMX,',&
                              &'LHTDRG= '
                         if (DEBUG)write(JOSTND,*)DG(K),DGMX,LHTDRG(ISPC)

                      case(4,11,12,14,15,18)
                         if (DK.lt.0.0.or. DKK.lt.0.0) then
                            DG(K) = HTG(K)*0.2*BARK*XRDGRO
                            DK = D+DG(K)
                         else
                            DG(K) = (DK-DKK)*BARK*XRDGRO
                         end if
                         if (DEBUG)write(JOSTND,*)'K,DK,DKK,DG= ',K,DK,DKK,DG(K)
                      end select

                   end if
                end if
                if (DG(K).lt.0.0) DG(K) = 0.0

                !         SCALE DIAMETER INCREMENT FOR BARK AND PERIOD LENGTH.
                !         IN ORDER TO MAINTAIN CONSISTENCY WITH **GRADD**,
                !         ADJUSTMENTS ARE MADE ON THE DDS SCALE.

                BARK = BRATIO(ISPC,DBH(K),HT(K))
                if (CIVAR) then
                   DG(K) = (DK-DKK)*BARK*XRDGRO
                else if (UTVAR) then
                   if (DG(K) .gt. DGMX) DG(K) = DGMX
                end if
                if (CIVAR .or. UTVAR) then
                   DDS = (DG(K)*(2.0*BARK*D+DG(K))) * SCALE2
                else
                   DDS = DG(K)*(2.0*BARK*D+DG(K))*SCALE2
                end if
                DG(K) = SQRT((D*BARK)**2+DDS)-BARK*D
             end if
             if ((DBH(K)+DG(K)).lt.DIAM(ISPC)) then
                DG(K) = DIAM(ISPC)-DBH(K)
             end if
          end if

          !  CHECK FOR TREE SIZE CAP COMPLIANCE

          call DGBND(ISPC,DBH(K),DG(K))

          23 continue

          !  PRINT DEBUG AND RETURN TO PROCESS NEXT TRIPLE OR NEXT TREE.

          if (.not.DEBUG) go to 24
          HK = H+HTG(K)
          write(JOSTND,9000) I,K,ISPC,H,HTG(K),HK,DBH(K),DG(K)
          9000 format('IN REGENT LOOP 2, I=',I4,', K=',I4,', ISPC=',I3,&
               &', CUR HT=',F7.2,', HT INC=',F7.4/T20,'NEW HT=',F7.2,&
               &', CUR DBH=',F7.2,', 10-YR DBH INC=',F7.4)
          24 continue
          if (LESTB .or..not.LTRIP .or. L.ge.2) go to 25
          if (TTVAR) then
             K = ITRN+2*I-1
             DBH(K) = DBH(I)
             DG(K) = DG(I)
             HT(K) = HT(I)
             HTG(K) = HTG(I)
             ICR(K) = ICR(I)
             K = K+1
             DBH(K) = DBH(I)
             DG(K) = DG(I)
             HT(K) = HT(I)
             HTG(K) = HTG(I)
             ICR(K) = ICR(I)
             go to 25
          else
             L = L+1
             K = ITRN+2*I-2+L
             go to 918
          end if

          !  END OF GROWTH PREDICTION LOOP.  PRINT DEBUG INFO IF DESIRED.

          25 continue
       end do
       30 continue
    end do
    return

    !  SMALL TREE HEIGHT CALIBRATION SECTION.

    40 continue
    do ISPC = 1,MAXSP
       HCOR(ISPC) = 0.0
       CORTEM(ISPC) = 1.0
       NUMCAL(ISPC) = 0
       45 continue
    end do
    if (ITRN.le.0) return
    if (IFINTH .eq.0)  goto 100

    select case (ISPC)

       !  SPECIES WITH REGYR=5: ORIGINAL TT SPECIES, SPECIES FROM CI AND IE VARIANTS

    case(1:3,5:9,10,14,17)
       SCALE3 = REGYR/FINTH

       !  SPECIES WITH REGYR=10: SPECIES FROM UT VARIANT

    case default
       SCALE3 = 10./FINTH
    end select

    !  DETERMINE BASAL AREA IN LARGE TREES AT START OF EACH PREDICTION SUBCYCLE.

    do J = 1,NPER
       BANEXT(J) = TEMBA
       RDNEXT(J) = TEMCCF
       46 continue
    end do
    if (NPER.eq.1) go to 50

    !  IF THERE IS MORE THAN 1 SUBCYCLE FOR HEIGHT INCREMENT PREDICTION,
    !  (IFINTH > 5) ESTIMATE VALUES OF BA AND RELATIVE DENSITY (CCF) AT
    !  START OF EACH SUBCYCLE.  10-YEAR CHANGE IS COMPUTED FROM BACKDATED
    !  DIAMETER (WK3) AND 10-YEAR DIAMETER INCREMENT COMPUTED DURING CALIBRATION.
    !  LINEAR INTERPOLATION IS USED TO ASSIGN VALUES FOR SUBCYCLES.

    do I = 1,ITRN
       D1 = WK3(I)
       IS = ISP(I)
       P = PROB(I)
       D2 = WK3(I)+DG(I)/BRATIO(IS,D1,HT(I))
       B1 = BACON*D1*D1
       B2 = BACON*D2*D2
       call CCFCAL(IS,D1,HT(I),ICR(I),P,.false.,C1,CW,1)
       call CCFCAL(IS,D2,HT(I),ICR(I),P,.false.,C2,CW,1)
       BI = (B2-B1)/10.0
       CI = (C2-C1)/10.0
       K = 0
       do J = 2,NPER
          K = K+KPER(J-1)
          PN = P*0.985**K
          RDNEXT(J) = RDNEXT(J)+K*CI/P*PN
          BANEXT(J) = BANEXT(J)+K*BI*PN
          48 continue
       end do
       49 continue
    end do
    50 continue

    ! COMPUTE DENSITY MODIFIER FROM CCF AND TOP HEIGHT FOR UT VARIANT

    X = AVH*(RELDEN/100.0)
    if (X .gt.300.0) X = 300.0
    PCTRED = 1.11436 + X*(-.011493 + X*(.43012E-4 + X*(-.72221E-7&
         &+ X*(.5607E-10- X*0.1641E-13))))
    if (PCTRED .gt.1.0) PCTRED = 1.0
    if (PCTRED .lt.0.01) PCTRED = 0.01
    if (DEBUG) write(JOSTND,9989) AVH,RELDEN,X,PCTRED
    9989 format('IN REGENT AVH,RELDEN,X,PCTRED = ',4F10.4)

    !  BEGIN PROCESSING TREE LIST IN SPECIES ORDER.  DO NOT CALCULATE
    !  CORRECTION TERMS IF THERE ARE NO TREES FOR THIS SPECIES.

    do ISPC = 1,MAXSP

       !  GET THE APPROPRIATE SITE INDEX RANGE VALUES FOR THIS SPECIES.

       SLO = 0.
       SHI = 999.
       call SITERANGE (2,ISPC,SLO,SHI)

       !  ASSIGN VARIANT ACCORDING TO SPECIES

       CIVAR = .false.
       TTVAR = .false.
       UTVAR = .false.
       select case (ISPC)
       case(1:3,5:9,17)
          TTVAR = .true.
       case(10)
          CIVAR = .true.
       case(4,11:16,18)
          UTVAR = .true.
       end select

       CORNEW = 1.0
       I1 = ISCT(ISPC,1)
       if (I1.eq.0.or..not. LHTCAL(ISPC)) go to 90
       I2 = ISCT(ISPC,2)
       IREFI = IREF(ISPC)
       N = 0
       SNP = 0.0
       SNX = 0.0
       SNY = 0.0

       !  FOR UT VARIANT SPECIES
       !  COMPUTE POTENTIAL GROWTH FROM SITE INDEX AND SPECIES.

       if (UTVAR) then
          SI = SITEAR(ISPC)
          if (SI .gt. SHI) SI = SHI
          if (SI .le. SLO) SI = SLO + 0.5
          RELSI = (SI-SLO)/(SHI-SLO)
          RSIMOD = 0.5 * (1.0 + RELSI)
          X = RELSI*100.0
          SJ = SITEAR(ISPC)
          POTHTG = ((SJ/5.0)*(SJ*1.5-H)/(SJ*1.5))* 0.83
          if (DEBUG)write(JOSTND,9988)SI,RELSI,X,ISPC,J,POTHTG
          9988 format('IN REGENT SI,RELSI,X,ISPC,J,POTHTG = ',&
               &F7.2,F10.3,F10.4,2I6,F10.3)

       end if

       !  BEGIN TREE LOOP WITHIN SPECIES.  IF MEASURED HEIGHT INCREMENT IS
       !  LESS THAN OR EQUAL TO ZERO, OR DBH IS GREATER THAN 5.0, THE RECORD
       !  WILL BE EXCLUDED FROM THE CALIBRATION.

       do I3 = I1,I2
          I = IND1(I3)
          H = HT(I)
          if (IHTG.lt.2) H = H-HTG(I)
          if (DBH(I).ge.5.0.or.H.lt.0.01) go to 60
          if (HTG(I).lt.0.001) go to 60
          HK = H
          D = DBH(I)
          CR = FLOAT(ICR(I))
          do J = 1,NPER
             BAJ = BANEXT(J)
             RDJ = RDNEXT(J)
             BAL = BAJ*(100.0-PCT(I))*0.0001
             if (TEMCCF .le.0.) then
                PPCCF = 0.
             else
                PPCCF = (RDJ-TEMCCF)/TEMCCF
             end if
             TPCCF = PCCF(ITRE(I))*PPCCF
             if (TPCCF.gt.300.0) TPCCF = 300.0
             if (TPCCF.lt.25.0) TPCCF = 25.0
             if (CIVAR) then
                IICR = ((ICR(I)-1)/10)+1
                if (IICR.gt.9) IICR = 9
                RCR = FLOAT(IICR)

                !  DIA GT 3 INCHES INCLUDED IN OVERALL MEAN

                if (IHTG.lt.2) H = H-HTG(I)
                if (DBH(I).ge.5.0.or.H.lt.0.01) go to 60
                EDH = 2.764559 - 0.009643*BA + 0.025303*RCR*RCR

             else if (TTVAR) then
                call SMHTGF(ISPC,I,EDH,CR,TPCCF,LESTB,DEBUG)
                if (ISPC .eq.6) then
                   RELSI = (SITEAR(6)-30.)/70.
                   if (RELSI .gt.1.0)RELSI = 1.0
                   if (RELSI .lt.0.0)RELSI = 0.0
                   RSIMOD = 0.5*(1.0+RELSI)
                   EDH = EDH*RSIMOD
                end if
                HK = HK+EDH
                call SMDGF(ISPC,HK,CR,PCCF(ITRE(I)),D2)
                if (D2.lt.DIAM(ISPC)) D2 = DIAM(ISPC)

             else if (UTVAR) then
                X = CR/100.
                VIGOR = (150.0 * (X**3.0)*EXP(-6.0*X))+0.3
                if (VIGOR .gt.1.0)VIGOR = 1.0

                !  VIGOR ADJUSTMENT TO DRASTIC FOR PINYON, JUNIPER, AND OAK
                !  CUT IT BY TWO-THIRDS

                select case (ISPC)
                case(4,11,12)
                   VIGOR = 1.-((1.-VIGOR)/3.)
                end select

                !  COMPUTE HT GROWTH AND AGE FOR ROCKY MTN MAPLE;
                !  USE APSPEN EQN FROM WAYNE SHEPPARD RMS.

                if (ISPC .eq.14) then
                   AG1 = (H*12.0*2.54/26.9825)**0.8509
                   AG2 = AG1+10.0
                   H2 = (26.9825*AG2**1.1752)/(2.54*12.0)
                   EDH = (H2-H) * RSIMOD * RHCON(ISPC)*0.75
                else
                   EDH = POTHTG*PCTRED*VIGOR*RHCON(ISPC)
                end if

                ! UT MODELS ARE 10-YEAR MODELS, ADJUST TO 5-YEAR BASIS

                EDH = EDH*0.5

                if (DEBUG)write(JOSTND,9990) X,VIGOR,EDH
                9990 format('IN REGENT X,VIGOR,EDH = ',3F10.4)
             end if
             55 continue
          end do
          if (CIVAR) then
             EDH = EDH * RHCON(ISPC)
          else if (TTVAR) then
             EDH = (HK-H)*RHCON(ISPC)
          end if
          P = PROB(I)
          if (HTG(I).lt.0.001) go to 60
          TERM = HTG(I)*SCALE3
          SNP = SNP+P
          SNX = SNX+EDH*P
          SNY = SNY+TERM*P
          N = N+1

          !  PRINT DEBUG INFO IF DESIRED.

          if (DEBUG) write(JOSTND,9001) NPLT,I,ISPC,H,DBH(I),ICR(I),&
               &PCT(I),TEMCCF,RHCON(ISPC),EDH,TERM
          9001 format('NPLT=',A26,',  I=',I5,',  ISPC=',I3,',  H=',F6.1,&
               &',  DBH=',F5.1,',  ICR',I5,',  PCT=',F6.1,',  RELDEN=',&
               &F6.1 / 12X,'RHCON=',F10.3,',  EDH=',F10.3,', TERM=',F10.3)

          !  END OF TREE LOOP WITHIN SPECIES.

          60 continue
       end do
       if (DEBUG) write(JOSTND,9010) ISPC,SNP,SNX,SNY
       9010 format(/'SUMS FOR SPECIES ',I2,':  SNP=',F10.2,&
            &';   SNX=',F10.2,';   SNY=',F10.2)

       !  COMPUTE CALIBRATION TERMS.  CALIBRATION TERMS ARE NOT COMPUTED
       !  IF THERE WERE FEWER THAN NCALHT (DEFAULT=5) HEIGHT INCREMENT
       !  OBSERVATIONS FOR A SPECIES.

       if (N.lt.NCALHT) go to 80

       !  CALCULATE MEANS FOR THE FOR THE GROWTH SAMPLE ON THE
       !  NATURAL SCALE.

       call REGHCAL(ISPC,SNP,SNX,SNY,CORNEW)
       80 continue
       CORTEM(IREFI) = CORNEW
       NUMCAL(IREFI) = N
       90 continue
    end do

    !  END OF CALIBRATION LOOP.  PRINT CALIBRATION STATISTICS AND RETURN

    write(JOSTND,9002) (NUMCAL(I),I = 1,NUMSP)
    9002 format(/'NUMBER OF RECORDS AVAILABLE FOR SCALING'/&
         &'THE SMALL TREE HEIGHT INCREMENT MODEL',&
         &((T48,11(I4,2X)/)))
    100 continue
    write(JOSTND,9003) (CORTEM(I),I = 1,NUMSP)
    9003 format(/'INITIAL SCALE FACTORS FOR THE SMALL TREE'/&
         &'HEIGHT INCREMENT MODEL',&
         &((T48,11(F5.2,1X)/)))

    ! OUTPUT CALIBRATION TO DATABASE.

    call REGCALOUT(CORTEM,NUMCAL,KOUT,K,KK,ISPEC,SPEC)
    return

    9998 continue

    !  ENTRY POINT FOR LOADING OF REGENERATION GROWTH MODEL
    !  CONSTANTS  THAT REQUIRE ONE-TIME RESOLUTION.

    do ISPC = 1,MAXSP
       RHCON(ISPC) = 1.0
       if (LRCOR2.and. RCOR2(ISPC).gt.0.0) RHCON(ISPC) = RCOR2(ISPC)
       110 continue
    end do

    return
  end subroutine REGTT

  !===============================!
  !   Compute small tree growth   !
  !===============================!
  ! UT $Id$
  subroutine REGUT(LCON)
    implicit none

    logical :: LCON

    !  THIS SUBROUTINE COMPUTES HEIGHT AND DIAMETER INCREMENTS FOR
    !  SMALL TREES.  THE HEIGHT INCREMENT MODEL IS APPLIED TO TREES
    !  THAT ARE LESS THAN 6 INCHES DBH AND THE DBH INCREMENT MODEL IS
    !  APPLIED TO TREES THAT ARE LESS THAN 3 INCHES DBH.  FOR TREES THAT
    !  ARE GREATER THAN 2 INCHES DBH (1 INCH FOR LODGEPOLE PINE), HEIGHT
    !  INCREMENT PREDICTIONS ARE AVERAGED WITH THE PREDICTIONS FROM THE
    !  LARGE TREE MODEL.  HEIGHT INCREMENT IS A FUNCTION OF SITE INDEX,
    !  TOP HEIGHT, AND STAND CROWN COMPETITION FACTOR.  DIAMETER IS
    !  ASSIGNED FROM A HEIGHT-DIAMETER FUNCTION; INCREMENT IS COMPUTED
    !  BY SUBTRACTION.
    !  THIS ROUTINE IS CALLED FROM **CRATET** DURING CALIBRATION AND
    !  FROM **TREGRO** DURING CYCLING.  ENTRY **REGCON** IS CALLED FROM
    !  **RCON** TO LOAD MODEL PARAMETERS THAT NEED ONLY BE RESOLVED ONCE.
    !  CALLS **FINDAG TO CALCULATE ASPEN TREE AGE

    ! Common blocks

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
    logical :: DEBUG,LSKIPH
    character(len=2) :: SPEC
    integer :: NUMCAL(24)
    integer :: I,N,J,K,IPCCF
    integer :: ISPC,I1,I2,I3,IREFI,KOUT,ISPEC,KK,L
    real    :: D1F,D2F,SITAGE,SITHT,AG1,AGMAX
    real    :: XMAX(24),XMIN(24),SLO,SHI,DAT45
    real    :: AB(9),DGMAX(24),DIAM(24),P2,P4,HAT3,P3
    real    :: DGMX,CR,RAN,TERM,EDH,SNY,SNX,SNP
    real    :: CORNEW,SCALE3,DDS,BARK,DKK,XWT,HTGR,HK,DK,H,H2
    real    :: RSIMOD,RELSI,D,XMN,XMX,XRDGRO,XRHGRO
    real    :: CON,BACHLO,BRATIO,P,SCALE
    real    :: REGYR,CORTEM(24),CRCODE
    real    :: FNT,SCALE2,CCF,AVHT,X,PCTRED,SI,POTHTG,HITE1,AG2,HITE2,SJ
    real    :: VIGOR,ZZRAN,BKPT,BX,AX,HTNEW,HTMAX,HTMAX2

    ! Data statements
    ! RJ 11/28/88 MAX DIAMETER GROWTHS

    data DGMAX/&
         &2.8,  2.8,  2.4,  3.6,  3.6,  2.5,  3.5,  3.6,  3.6,  2.8,&
         &2.0,  2.0,  2.0,  2.0,  2.0,  2.0,  2.0,  2.5,  2.5,  2.0,&
         &2.0,  2.5,  2.8,  2.0/

    data XMAX/&
         &4.0,  4.0,  4.0,  4.0,  4.0,  4.0,  5.0,  4.0,  6.0,  6.0,&
         &99.0, 99.0, 99.0, 99.0, 99.0, 99.0, 199.,  2.0,  2.0, 99.0,&
         &99.0,  2.0,  6.0, 99.0/

    data XMIN/&
         &2.0,  2.0,  2.0,  2.0,  2.0,  2.0,  1.0,  2.0,  2.0,  2.0,&
         &90.0, 90.0, 90.0, 90.0, 90.0, 90.0,  99.,  0.5,  0.5, 90.0,&
         &90.0,  0.5,  2.0, 90.0/

    data DIAM/&
         &0.4,  0.4,  0.3,  0.3,  0.3,  0.2,  0.4,  0.3,  0.3,  0.5,&
         &0.4,  0.3,  0.3,  0.4,  0.3,  0.3,  0.4,  0.3,  0.3,  0.2,&
         &0.2,  0.3,  0.2,  0.3/

    data AB/&
         &1.11436, -.011493, .43012E-4, -.72221E-7, .5607E-10, -.1641E-13,&
         &3*0. /

    data REGYR/10.0/

    if (LCON) go to 9998

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

       !  GET THE APPROPRIATE SITE INDEX RANGE VALUES FOR THIS SPECIES.

       SLO = 0.
       SHI = 999.
       call SITERANGE (2,ISPC,SLO,SHI)

       I1 = ISCT(ISPC,1)
       if (I1.eq.0) go to 30
       I2 = ISCT(ISPC,2)
       BKPT = BREAK(ISPC)
       XRHGRO = XRHMLT(ISPC)
       XRDGRO = XRDMLT(ISPC)
       CON = RHCON(ISPC) * EXP(HCOR(ISPC))
       XMX = XMAX(ISPC)
       XMN = XMIN(ISPC)
       SI = SITEAR(ISPC)
       if (SI .gt. SHI) SI = SHI
       if (SI .le. SLO) SI = SLO + 0.5

       !  COMPUTE POTENTIAL GROWTH FROM SITE INDEX AND SPECIES.

       RELSI = (SI-SLO)/(SHI-SLO)
       RSIMOD = 0.5 * (1.0 + RELSI)
       X = RELSI*100.0
       SJ = SITEAR(ISPC)

       if (DEBUG) write(JOSTND,9981) SI,RELSI,X,ISPC
       9981 format('IN REGENT SI,RELSI,X,ISPC = ',&
            &F7.2,F10.3,F10.4,I6,F10.3)

       !     PUT A CEILING ON DIAMETER GROWTH BASED ON OLIVER AND COCHRAN EMPR.
       !     RJ 11/28/88

       DGMX = DGMAX(ISPC) * SCALE

       !  PROCESS NEXT TREE RECORD.

       do I3 = I1,I2
          I = IND1(I3)
          D = DBH(I)
          H = HT(I)

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

          if (D.ge.XMX) go to 25
          BARK = BRATIO(ISPC,D,H)
          if (LSKIPH) then
             HTG(K) = 0.0
             go to 4
          end if

          !  COMPUTE POTENTIAL HEIGHT GROWTH AS A FUNCTION OF SJ AND SPECIES.
          !  ADJUSTMENT OF 0.83 IS SO OPEN GROWN TREES OF THESE SPECIES HIT THE
          !  SITE HEIGHT ACROSS THE RANGE OF POSSIBLE SITE INDICIES

          select case (ISPC)

          case(1:5,7:10,23)
             POTHTG = SJ/5.0

          case(11:16,24)
             POTHTG = ((SJ/5.0)*(SJ*1.5-H)/(SJ*1.5))* 0.83

             ! COMPUTE HT GROWTH AND AGE FOR ASPEN. EQN FROM WAYNE SHEPPARD RMRS.

          case(6)
             SITAGE = 0.0
             SITHT = 0.0
             AGMAX = 0.0
             HTMAX = 0.0
             HTMAX2 = 0.0
             D1F = DBH(I)
             D2F = 0.0
             if (DEBUG)write(JOSTND,*)'IN HTGF, CALLING FINDAG I= ',I
             call FINDAG(I,ISPC,D1F,D2F,H,SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,&
                  &DEBUG)

             if (LESTB) then
                SITAGE = ABIRTH(I)
             end if
             HITE1 = 26.9825 * SITAGE**1.1752
             AG2 = SITAGE+10.0
             HITE2 = 26.9825 * AG2**1.1752
             POTHTG = (HITE2-HITE1)/(2.54*12.0) * RSIMOD * CON

             ! GROWTH RATES APPEAR HIGH, REDUCE BY 25 PERCENT. DIXON 8-27-92

             POTHTG = POTHTG * .75
             go to 2

             !  SPECIES WITH SURROGATE EQUATIONS FROM THE CR VARIANT
             !  EQUATIONS SWITCHED TO THE UT FORMULATION FOR THESE SPECIES BECAUSE
             !  THEY PERFORM BETTER THAN THE CR EQUATIONS.

          case(17)
             POTHTG = ((SJ/5.0)*(SJ*1.5-H)/(SJ*1.5))* 0.83

          case(18:19,22)
             POTHTG = ((SJ/5.0)*(SJ*1.5-H)/(SJ*1.5))* 0.83

             !  SPECIES WITH SURROGATE EQUATIONS FROM THE SO VARIANT

          case(20,21)
             !        POTHTG = ((1.47043 + 0.23317*SJ)/(31.56252 - 0.05586*SJ))*10.
             POTHTG = ((SJ/5.0)*(SJ*1.5-H)/(SJ*1.5))* 0.83

          end select

          X = FLOAT(ICR(I))/100.
          VIGOR = (150.0 * (X**3.0)*EXP(-6.0*X))+0.3
          if (VIGOR .gt.1.0)VIGOR = 1.0

          !  VIGOR ADJUSTMENT TO DRASTIC FOR PINYON, JUNIPER, OAK, AND
          !  OTHER HARDWOODS. CUT IT BY TWO-THIRDS

          select case (ISPC)
          case(11:17,24)
             VIGOR = 1.-((1.-VIGOR)/3.)
          end select

          !  RETURN HERE TO PROCESS NEXT TRIPLE.
          !  DON'T RESET HTGR FOR ASPEN

          2 continue
          select case (ISPC)
          case (6)
             HTGR = POTHTG
          case default
             HTGR = POTHTG*PCTRED*VIGOR*CON
          end select
          if (DEBUG) write(JOSTND,9983) X,VIGOR,HTGR,CON,POTHTG
          9983 format('IN REGENT X,VIGOR,HTGR,CON,POTHTG = ',5F10.4)
          3 continue
          ZZRAN = 0.0
          if (DGSD.ge.1.0) ZZRAN = BACHLO(0.0,1.0,RANN)
          if ((ZZRAN .gt.0.5) .or. (ZZRAN .lt. -2.0)) go to 3
          if (DEBUG)write(JOSTND,9984) HTGR,ZZRAN,XRHGRO,SCALE,WK4(I)
          9984 format('IN REGENT 9984 FORMAT',5(F10.4,2X))
          select case (ISPC)
          case(1:16,23:24)
             HTGR = (HTGR +ZZRAN*0.1)*XRHGRO * SCALE
          case(17:19,22)
             HTGR = (HTGR +ZZRAN*0.2)*XRHGRO * SCALE * WK4(I)
          case(20,21)
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
          HTG(K) = HTGR*(1.0-XWT) + XWT*HTG(K)
          if (HTG(K) .lt..1) HTG(K) = .1

          ! CHECK FOR SIZE CAP COMPLIANCE.

          if ((H+HTG(K)).gt.SIZCAP(ISPC,4)) then
             HTG(K) = SIZCAP(ISPC,4)-H
             if (HTG(K) .lt.0.1) HTG(K) = 0.1
          end if

          4 continue

          !     ASSIGN DBH AND COMPUTE DBH INCREMENT FOR TREES WITH DBH LESS
          !     THAN BREAKPOINT (COMPUTE 10-YEAR DBH INCREMENT REGARDLESS OF
          !     PROJECTION PERIOD LENGTH).

          if (D.ge.BKPT) go to 23
          HK = H + HTG(K)
          if (HK .le.4.5) then
             DG(K) = 0.0
             DBH(K) = D+0.001*HK
          else
             select case(ISPC)
             case(10)
                DK = (HK-8.31485+.59200*7.)/3.03659
                DKK = (H-8.31485+.59200*7.)/3.03659
                if (H .lt.4.5) DKK = D
             case(11:17,24)
                DK = (HK-4.5)*10./(SITEAR(ISPC)-4.5)
                if (DK .lt.0.1) DK = 0.1
                DKK = (H-4.5)*10./(SITEAR(ISPC)-4.5)
                if (DKK .lt.0.1) DKK = 0.1
                if (H .lt.4.5) DKK = D
             case default
                BX = HT2(ISPC)
                if (IABFLG(ISPC).eq.1) then
                   AX = HT1(ISPC)
                else
                   AX = AA(ISPC)
                end if

                !  SPECIES FROM SO (ORIGINALY FROM WC) VARIANT

                if (ISPC.eq.20.or. ISPC.eq.21) then

                   !   BEGIN THE DIAMETER LOOKUP SECTION
                   !      DK = DIAMETER WITH HTG ADDED TO THE STARTING HEIGHT
                   !      DKK  = DIAMETER AT THE START OF THE PROJECTION
                   !   DAT45  = DIAMETER AT 4.5 FEET PREDICTED FROM EQUATION.

                   CRCODE = FLOAT(ICR(K))/10.0
                   DAT45 = 3.1020 + 0.0210*4.5

                   DKK = 3.1020 + 0.0210*H
                   if (DKK .lt.0.0) DKK = D

                   DK = 3.1020 + 0.0210*HK
                   if (DK .lt. DKK) DK = DKK+.01
                   if (DEBUG)write(JOSTND,*)'I,ISPC,DBH,H,HK,DK,DKK,CRCODE,',&
                        &'= ',I,ISPC,DBH(I),H,HK,DK,DKK,CRCODE

                   !  USE INVENTORY EQUATIONS IF CALIBRATION OF THE HT-DBH FUNCTION IS TURNED
                   !  OFF, OR IF WYKOFF CALIBRATION DID NOT OCCUR.
                   !  NOTE: THIS SIMPLIFIES TO IF(IABFLB(ISPC).EQ.1) BUT IS SHOWN IN IT'S
                   !        ENTIRITY FOR CLARITY.

                   if (.not.LHTDRG(ISPC) .or.&
                        &(LHTDRG(ISPC) .and. IABFLG(ISPC).eq.1)) then
                      if (ISPC .eq.20) then
                         P2 = 1709.7229
                         P3 = 5.8887
                         P4 = -0.2286
                      else
                         P2 = 76.5170
                         P3 = 2.2107
                         P4 = -0.6365
                      end if
                      HAT3 = 4.5 + P2 * EXP(-1.*P3*3.0**P4)
                      if (HK .ge. HAT3) then
                         DK = EXP( ALOG((ALOG(HK-4.5)-ALOG(P2))/(-1.*P3)) *&
                              &1./P4)
                      else
                         DK = (((HK-4.51)*2.7)/(4.5+P2*EXP(-1.*P3*(3.**P4))&
                              &-4.51))+0.3
                      end if
                      if (H .le.4.5) then
                         DKK = D
                      else
                         if (H .ge. HAT3) then
                            DKK = EXP( ALOG((ALOG(H-4.5)-ALOG(P2))/(-1.*P3)) *&
                                 &1./P4)
                         else
                            DKK = (((H-4.51)*2.7)/(4.5+P2*EXP(-1.*P3*(3.**P4))&
                                 &-4.51))+0.3
                         end if
                      end if
                      if (DEBUG)write(JOSTND,*)'INV EQN DUBBING ISPC,H,HK,DK,',&
                           &'DKK= ',ISPC,H,HK,DK,DKK
                      if (DEBUG)write(JOSTND,*)'ISPC,LHTDRG,IABFLG= ',&
                           &ISPC,LHTDRG(ISPC),IABFLG(ISPC)
                   end if
                else
                   DK = (BX/(ALOG(HK-4.5)-AX))-1.0
                   if (DK .lt.0.1) DK = 0.1
                   if (H .le.4.5) then
                      DKK = D
                   else
                      DKK = (BX/(ALOG(H-4.5)-AX))-1.0
                   end if
                   if (DEBUG)write(JOSTND,9986) AX,BX,ISPC,HK,BARK,&
                        &XRDGRO,DK,DKK
                   9986 format('IN REGENT 9986 FORMAT AX,BX,ISPC,HK',&
                        &' BARK,XRDGRO,DK,DKK= '/T12, F10.3,2X,F10.3,2X,I5,2X,5F10.3)
                end if
             end select

             !         IF CALLED FROM **ESTAB** ASSIGN DIAMETER

             if (LESTB) then
                select case (ISPC)
                case(20,21)
                   if (DAT45.gt.0.0.and. HK.ge.4.5.and. LHTDRG(ISPC) .and.&
                        &IABFLG(ISPC).eq.0) then
                      DBH(K) = DK - DAT45 + DIAM(ISPC)
                   else
                      DBH(K) = DK
                   end if
                case default
                   DBH(K) = DK
                end select
                if (DBH(K).lt.DIAM(ISPC)) DBH(K) = DIAM(ISPC)
                DBH(K) = DBH(K)+0.001*HK
                DG(K) = DBH(K)
             else

                !         COMPUTE DIAMETER INCREMENT BY SUBTRACTION, APPLY USER
                !         SUPPLIED MULTIPLIERS, AND CHECK TO SEE IF COMPUTED VALUE
                !         IS WITHIN BOUNDS.

                select case (ISPC)

                   ! FROM SO (ORIGINALLY WC)
                   ! PROBLEM WITH HARDWOOD EQN, REDUCES TO .021*HG. SET TO RULE
                   ! OF THUMB VALUE .1*HG FOR NOW. DIXON 11-04-92
                   ! DON'T USE R.O.T. IF USING INVENTORY EQNS.  DIXON 03-31-98

                   ! IF THE TREE JUST REACHED 4.5 FEET, SET DKK TO PRESENT DBH.
                   ! RJ 12/6/91

                case(20,21)
                   if (H .lt.4.5 )DKK = D
                   BARK = BRATIO(ISPC,D,H)
                   if (DEBUG)write(JOSTND,*)'BARK,XRDGRO= ',BARK,XRDGRO
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
                   if (DK.lt.0.0.or. DKK.lt.0.0) then
                      DG(K) = HTG(K)*0.2*BARK*XRDGRO
                      DK = D+DG(K)
                   else
                      DG(K) = (DK-DKK)*BARK*XRDGRO
                   end if
                   if (DEBUG)write(JOSTND,*)'K,DK,DKK,DG= ',K,DK,DKK,DG(K)
                end select

                !         SCALE DIAMETER INCREMENT TO 10-YR ESTIMATE.
                !         SCALE ADJUSTMENT IS ON GROWTH IN DDS RATHER THAN INCHES
                !         OF DG TO BE CONSISTENT WITH GRADD.

                if (DG(K) .lt.0.0) DG(K) = 0.0
                if (DG(K) .gt. DGMX) DG(K) = DGMX
                DDS = DG(K)*(2.0*BARK*D + DG(K))*SCALE2
                DG(K) = SQRT((D*BARK)**2 + DDS) - BARK*D
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
    SCALE3 = REGYR / FINTH

    ! COMPUTE DENSITY MODIFIER FROM CCF AND TOP HEIGHT.

    X = AVH*(RELDEN/100.0)
    if (X .gt.300.0) X = 300.0
    PCTRED = AB(1)&
         &+ X*(AB(2) + X*(AB(3) + X*(AB(4) + X*(AB(5)+ X*AB(6)))))
    if (PCTRED .gt.1.0) PCTRED = 1.0
    if (PCTRED .lt.0.01) PCTRED = 0.01
    if (DEBUG)write(JOSTND,9989)AVH,RELDEN,X,PCTRED
    9989 format('IN REGENT AVH,RELDEN,X,PCTRED = ',4F10.4)

    !  BEGIN PROCESSING TREE LIST IN SPECIES ORDER.  DO NOT CALCULATE
    !  CORRECTION TERMS IF THERE ARE NO TREES FOR THIS SPECIES.

    do ISPC = 1,MAXSP

       !  GET THE APPROPRIATE SITE INDEX RANGE VALUES FOR THIS SPECIES.

       SLO = 0.
       SHI = 999.
       call SITERANGE (2,ISPC,SLO,SHI)

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
       if (SI .gt. SHI) SI = SHI
       if (SI .le. SLO) SI = SLO + 0.5
       RELSI = (SI-SLO)/(SHI-SLO)
       RSIMOD = 0.5 * (1.0 + RELSI)
       X = RELSI*100.0
       SJ = SITEAR(ISPC)

       if (DEBUG)write(JOSTND,9988)SI,RELSI,X,ISPC,J
       9988 format('IN REGENT SI,RELSI,X,ISPC,J = ',&
            &F7.2,F10.3,F10.4,2I6)

       !  BEGIN TREE LOOP WITHIN SPECIES.  IF MEASURED HEIGHT INCREMENT IS
       !  LESS THAN OR EQUAL TO ZERO, OR DBH IS LESS THAN 5.0, THE RECORD
       !  WILL BE EXCLUDED FROM THE CALIBRATION.

       do I3 = I1,I2
          I = IND1(I3)
          H = HT(I)

          !  DIA GT 3 INCHES INCLUDED IN OVERALL MEAN

          if (IHTG.lt.2) H = H-HTG(I)
          if (DBH(I).ge.5.0.or.H.lt.0.01) go to 60

          !  COMPUTE VIGOR MODIFIER FROM CROWN RATIO.

          X = FLOAT(ICR(I))/100.
          VIGOR = (150.0 * (X**3.0)*EXP(-6.0*X))+0.3
          if (VIGOR .gt.1.0)VIGOR = 1.0

          !  VIGOR ADJUSTMENT TO DRASTIC FOR PINYON, JUNIPER, AND OAK
          !  CUT IT BY TWO-THIRDS

          select case (ISPC)
          case(11:17,24)
             VIGOR = 1.-((1.-VIGOR)/3.)
          end select

          !  COMPUTE POTENTIAL HEIGHT GROWTH AS A FUNCTION OF SJ AND SPECIES.

          select case (ISPC)

          case(1:5,7:10,23)
             POTHTG = SJ/5.0
             EDH = POTHTG*PCTRED*VIGOR*RHCON(ISPC)

          case(11:16,24)
             POTHTG = ((SJ/5.0)*(SJ*1.5-H)/(SJ*1.5))* 0.83
             EDH = POTHTG*PCTRED*VIGOR*RHCON(ISPC)

             !  ASPEN USE EQUATIONS FROM WAYNE SHEPPARD RMRS

          case(6)
             AG1 = (H*12.0*2.54/26.9825)**0.8509
             AG2 = AG1 + 10.0
             H2 = (26.9825*AG2**1.1752)/(2.54*12.0)
             EDH = (H2-H) * RSIMOD * RHCON(ISPC)

             ! GROWTH RATES APPEAR HIGH, REDUCE BY 25 PERCENT. DIXON 8-27-92.

             EDH = EDH*.75
             if (EDH .lt.0.0) EDH = 0.0

             !  SPECIES WITH SURROGATE EQUATIONS FROM THE CR VARIANT

          case(17)
             POTHTG = ((SJ/5.0)*(SJ*1.5-H)/(SJ*1.5))* 0.83
             EDH = POTHTG*PCTRED*VIGOR*RHCON(ISPC)

          case(18:19,22)
             POTHTG = ((SJ/5.0)*(SJ*1.5-H)/(SJ*1.5))* 0.83
             EDH = POTHTG*PCTRED*VIGOR*RHCON(ISPC)

             !  SPECIES WITH SURROGATE EQUATIONS FROM THE SO VARIANT

          case(20,21)
             !        POTHTG = ((1.47043 + 0.23317*SJ)/(31.56252 - 0.05586*SJ))*10.
             POTHTG = ((SJ/5.0)*(SJ*1.5-H)/(SJ*1.5))* 0.83
             EDH = POTHTG*PCTRED*VIGOR*RHCON(ISPC)

          end select

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

       call REGHCAL(ISPC,SNP,SNX,SNY,CORNEW)
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

    call REGCALOUT(CORTEM,NUMCAL,KOUT,K,KK,ISPEC,SPEC)
    91 if (DEBUG)write(JOSTND,9995)ICYC
    9995 format('LEAVING SUBROUTINE REGENT  CYCLE =',I5)
    return

    9998 continue

    !  ENTRY POINT FOR LOADING OF REGENERATION GROWTH MODEL
    !  CONSTANTS  THAT REQUIRE ONE-TIME RESOLUTION.

    do ISPC = 1,MAXSP
       RHCON(ISPC) = 1.0
       if (LRCOR2.and. RCOR2(ISPC).gt.0.0)&
            &RHCON(ISPC) = RCOR2(ISPC)
       90 continue
    end do
    do I = 1,MAXTRE
       ZRAND(I) = -999.0
    end do
    return
  end subroutine REGUT

  !===============================!
  !   Compute small tree growth   !
  !===============================!
  ! WS $Id$
  subroutine REGWS(LCON)
    implicit none

    logical :: LCON
    ! Regional callees retain their native argument lists.
    interface
       subroutine SMHTGF(NSPC,D,CR,BA,BAL,SI,H,JOSTND,DEBUG,HTGR)
         integer :: NSPC
         real :: D
         real :: CR
         real :: BA
         real :: BAL
         real :: SI
         real :: H
         integer :: JOSTND
         logical :: DEBUG
         real :: HTGR
       end subroutine SMHTGF
    end interface


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
    logical :: DEBUG,LSKIPH
    character(len=2) :: SPEC
    integer :: NUMCAL(43)
    integer :: SMTMAP(43)
    integer :: N,IREFI,KOUT,ISPEC,KK,I3,I
    integer :: ISPC,I1,I2,IPCCF,K,L,MSP,IHDW
    real    :: CORTEM(43)
    real    :: XMAX(43),XMIN(43),DIAM(43),AB(9)
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

    if (LCON) go to 9998

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
                   DG(K) = SQRT((D*BARK)**2 + DDS) - BARK*D
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
                   DGSM = SQRT((D*BARK)**2+DDS)-BARK*D
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

       call REGHCAL(ISPC,SNP,SNX,SNY,CORNEW)
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

    call REGCALOUT(CORTEM,NUMCAL,KOUT,K,KK,ISPEC,SPEC)
    91 if (DEBUG)write(JOSTND,9995)ICYC
    9995 format('LEAVING SUBROUTINE REGENT  CYCLE =',I5)
    return

    9998 continue

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
  end subroutine REGWS

  subroutine NEBALM(ISPC,D,GMOD)
    implicit none

    ! NE'S THREE-ARGUMENT BALMOD IS DISTINCT FROM CS/LS'S SIX-ARGUMENT
    ! FORM. THE LOCAL INTERFACE KEEPS THAT EXTERNAL CONTRACT EXPLICIT.
    integer :: ISPC
    real :: D,GMOD
    interface
       subroutine BALMOD(ISPC,D,GMOD)
         integer :: ISPC
         real :: D,GMOD
       end subroutine BALMOD
    end interface

    call BALMOD(ISPC,D,GMOD)
  end subroutine NEBALM


  subroutine REGHCAL(ISPC,SNP,SNX,SNY,CORNEW)
    implicit none

    ! SMALL-TREE HEIGHT CALIBRATION: RATIO ESTIMATOR AND OUT-OF-RANGE TRAP FOR
    ! ONE SPECIES, SHARED BY EVERY FAMILY EXCEPT BC AND ON.

    integer :: ISPC
    real :: SNP
    real :: SNX
    real :: SNY
    real :: CORNEW

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
  end subroutine REGHCAL


  subroutine REGCALOUT(CORTEM,NUMCAL,KOUT,K,KK,ISPEC,SPEC)
    implicit none

    ! SMALL-TREE HEIGHT CALIBRATION OUTPUT: DATABASE TABLE AND, WITH THE
    ! CALBSTAT KEYWORD, THE CALIBRATION-STATISTICS FILE. SHARED BY EVERY
    ! FAMILY EXCEPT ON. THE SCALE-FACTOR ARRAYS KEEP EACH FAMILY'S EXTENT.

    real :: CORTEM(*)
    integer :: NUMCAL(*)
    integer :: KOUT
    integer :: K
    integer :: KK
    integer :: ISPEC
    character(len=2) :: SPEC

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
  end subroutine REGCALOUT

end subroutine REGENT
