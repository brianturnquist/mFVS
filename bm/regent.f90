!===============================!
!   Compute small tree growth   !
!===============================!
! BM $Id$
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
  include 'ESTCOR.f90'
  include 'HTCAL.f90'
  include 'MULTCM.f90'
  include 'OUTCOM.f90'
  include 'PDEN.f90'
  include 'PLOT.f90'
  include 'VARCOM.f90'

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
  logical :: DEBUG,LESTB,LSKIPH

  character(len=2) :: SPEC

  integer :: I,I1,I2,I3,INDX,IPCCF,IREFI,ISPC,ISPEC,ITRNIN,JCR
  integer :: K,KK,KOUT,L,N

  integer :: NUMCAL(MAXSP)

  real    :: AG1,AG2,AGMAX,ALHK,ALHT,AVHT,AX,BACHLO,BARK,BKPT,BRATIO
  real    :: BX,CCF,CON,CORNEW,CR,CRCODE,D,D2,DAT45,DDS,DDUM,DGMX,DK
  real    :: DKK,DLESS3,EDH,FNT,H,H2,HITE1,HITE2,HK,HLESS4,HTGR,HTMAX
  real    :: HTMAX2,HTNEW,P,PCTRED,POTHTG,PPCCF,RAN,REGYR,RELSI,RSIMOD
  real    :: SCALE,SCALE2,SCALE3,SHI,SI,SITAGE,SITHT,SLO,SNP,SNX,SNY
  real    :: TEMT,TERM,TPCCF,VIGOR,X,XMN,XMX,XRDGRO,XRHGRO,XWT
  real    :: ZZRAN

  real    :: AB(9),CORTEM(MAXSP),DGMAX(MAXSP),DIAM(MAXSP)
  real    :: RDCON(3),RDCR(3),RDDUM(3),RDHT(3),RDLHT(3)
  real    :: XMAX(MAXSP),XMIN(MAXSP)

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
              DG(K) = SQRT((D*BARK)**2.0 + DDS)-BARK*D
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
  !  CONSTANTS THAT REQUIRE ONE-TIME RESOLUTION.

  do ISPC = 1,MAXSP
     RHCON(ISPC) = 1.0
     if (LRCOR2.and.RCOR2(ISPC).gt.0.0)&
          &RHCON(ISPC) = RCOR2(ISPC)
     90 continue
  end do
  return
end
