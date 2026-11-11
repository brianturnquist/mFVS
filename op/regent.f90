!===============================!
!   Compute small tree growth   !
!===============================!
! OP $Id$
subroutine REGENT(LESTB,ITRNIN)
  implicit none

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
  logical :: DEBUG,LESTB,LSKIPH
  real    :: CORTEM(MAXSP)
  integer :: NUMCAL(MAXSP)
  real    :: XMAX(MAXSP),XMIN(MAXSP)
  real    :: DGMAX(MAXSP),DIAM(MAXSP),DGMIN(MAXSP)
  integer :: IRDMAP(MAXSP)
  real    :: AB(6)
  integer :: IREFI,ISPEC,KOUT,KK
  real    :: H,BARK,HTGR,ZZRAN,XWT
  real    :: CON,XMX,XMN,DGMX,D,CR,RAN,BACHLO,BRATIO,HK,DK,DKK
  real    :: DDS,HTNEW,SCALE3,CORNEW,SNP,SNX,SNY,EDH,P,TERM
  real    :: REGYR,FNT,SCALE,SCALE2,CCF,AVHT,XRHGRO,XRDGRO
  integer :: ITRNIN,ISPC,I1,I2,I3,I,IPCCF,K,L,N,MODE
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

           IORG(I) = 0

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
        if (DEBUG) write(JOSTND,*)' AFTER SMHGDG K,ISPC,D,H,HTGR,DGR,',&
             &'HK,DK = ',K,ISPC,D,H,HTGR,DGR,HK,DK
        3 continue
        ZZRAN = 0.0
        if (DGSD.ge.1.0) ZZRAN = BACHLO(0.0,1.0,RANN)
        if ((ZZRAN .gt.0.5) .or. (ZZRAN .lt. -2.0)) go to 3

        if (DEBUG)write(JOSTND,*)' ZZRAN,XRHGRO,SCALE,FNT,CON,WK4(I)= ',&
             &ZZRAN,XRHGRO,SCALE,FNT,CON,WK4(I)

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

        if (LORGANON .and. (IORG(K) .eq.1)) XWT = 1.0

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

        if (LORGANON .and. (IORG(K) .eq.1)) go to 23

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
                    DGSM = SQRT((D*BARK)**2.0+DDS)-BARK*D
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
              DG(K) = SQRT((D*BARK)**2.0+DDS)-BARK*D
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
       &((T49,11(I4,2X)/)))
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

  ! IF DEBUGGING, WRITE RESULTS TO THE OUTPUT FILE

  if (DEBUG) then
     do I = 1,ITRN
        write(JOSTND,*)' LEAVING REGENT I,ISP,DBH,DG,HT,HTG,IORG= ',&
             &I,ISP(I),DBH(I),DG(I),HT(I),HTG(I),IORG(I)
     end do
  end if

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
