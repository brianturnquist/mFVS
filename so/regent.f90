!===============================!
!   Compute small tree growth   !
!===============================!
! SO $Id$
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
  !  OF CCF, TOP HEIGHT AND CROWN RATIO.  DIAMETER IS ASSIGNED FROM
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
  !  SPECIES ORDER:
  !  1=WP,  2=SP,  3=DF,  4=WF,  5=MH,  6=IC,  7=LP,  8=ES,  9=SH,  10=PP,
  ! 11=JU, 12=GF, 13=AF, 14=SF, 15=NF, 16=WB, 17=WL, 18=RC, 19=WH,  20=PY,
  ! 21=WA, 22=RA, 23=BM, 24=AS, 25=CW, 26=CH, 27=WO, 28=WI, 29=GC,  30=MC,
  ! 31=MB, 32=OS, 33=OH

  external RANN
  ! Variables
  logical :: DEBUG,LESTB,LSKIPH
  character(len=2) :: SPEC
  integer :: KK,ISPEC,KOUT,IREFI,N,IPCCF,MODE1,K,L,JCR
  integer :: ISPC,I1,I2,I3,I,ITRNIN,NUMCAL(MAXSP)
  real    :: RDCON(MAXSP),RDCR(MAXSP)
  real    :: RDLHT(MAXSP),RDHT(MAXSP),RDDUM(MAXSP)
  real    :: REGYR,CORTEM(MAXSP),DGMAX(MAXSP),DIAM(MAXSP)
  real    :: XMAX(MAXSP),XMIN(MAXSP),SLO,SHI,AB(9)
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
                 DG(K) = SQRT((D*BARK)**2.0+DDS)-BARK*D
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
                 DGSM = SQRT((D*BARK)**2.0+DDS)-BARK*D
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
              DG(K) = SQRT((D*BARK)**2.0+DDS)-BARK*D
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
