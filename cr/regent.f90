!===============================!
!   Compute small tree growth   !
!===============================!
! CR $Id$
subroutine REGENT(LESTB,ITRNIN)
  implicit none

  !  THIS SUBROUTINE COMPUTES HEIGHT AND DIAMETER INCREMENTS FOR
  !  SMALL TREES. HEIGHT INCREMENT IS A FUNCTION OF SITE INDEX,
  !  TOP HEIGHT, AND STAND CROWN COMPETITION FACTOR.  DIAMETER IS
  !  ASSIGNED FROM A HEIGHT-DIAMETER FUNCTION; INCREMENT IS COMPUTED
  !  BY SUBTRACTION.
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
  include 'GGCOM.f90'

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
  logical :: DEBUG,LESTB,LSKIPH
  real    :: CORTEM(MAXSP),XMAX(MAXSP),XMIN(MAXSP),AB(9),DGMAX(MAXSP)
  real    :: DIAM(MAXSP),HTADJ(MAXSP)
  integer :: NUMCAL(MAXSP),MPCC(MAXSP),IVFLAG

  real    :: D,H,BARK,RAN,HTGR,ZZRAN
  real    :: XWT,HK,REGYR,FNT,SCALE
  real    :: SCALE2,XRHGRO,XRDGRO,CON,XMX,XMN
  real    :: BRATIO,BACHLO,DK,DKK,DDS,HTNEW,SCALE3,CORNEW,SNP
  real    :: SNX,SNY,EDH,P,TERM,CCF,AVHT,X,PCTRED,BKPT,SI,RELSI,RSIMOD
  real    :: POTHTG,CR,AG1,HITE1,HITE2,AG2,VIGOR,BX,AX,H2,DGMX
  integer :: J,IPCCF
  integer :: ITRNIN,ISPC,I1,I2,I3,I,K,L,N,IREFI,KOUT,ISPEC,KK
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
              DG(K) = SQRT((D*BARK)**2.0 + DDS) - BARK*D
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
     if (LRCOR2.and. RCOR2(ISPC).gt.0.0)&
          &RHCON(ISPC) = RCOR2(ISPC)
     90 continue
  end do
  return
end
