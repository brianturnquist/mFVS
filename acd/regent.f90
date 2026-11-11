!===============================!
!   Compute small tree growth   !
!===============================!
! ACD $Id$
subroutine REGENT(LESTB,ITRNIN)
  implicit none

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
  include 'PRGPRM.F77'
  include 'ARRAYS.F77'
  include 'CALCOM.F77'
  include 'COEFFS.F77'
  include 'CONTRL.F77'
  include 'OUTCOM.F77'
  include 'PLOT.F77'
  include 'HTCAL.F77'
  include 'MULTCM.F77'
  include 'ESTCOR.F77'
  include 'PDEN.F77'
  include 'VARCOM.F77'

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
  integer :: I,I1,I2,I3,ISPC,ISPEC,ITRNIN,IPCCF,IREFI,K,KK,KOUT
  integer :: L,MODE0,MODE9,N,IVAR
  character(len=2) :: SPEC
  logical :: DEBUG,LESTB,LSKIPH

  !  ARRAYS

  real    :: CORTEM(MAXSP),DGMAX(MAXSP),DIAM(MAXSP),HGADJ(MAXSP)
  real    :: XMAX(MAXSP),XMIN(MAXSP)
  integer :: NUMCAL(MAXSP)

  !  FUNCTIONS

  real    :: BACHLO,BRATIO

  !  INITIALIZATIONS

  data HGADJ/ MAXSP*1.0/
  data DGMAX/ MAXSP*5.0 /
  data XMIN / MAXSP*1.5 /, XMAX/ MAXSP*5.0 /
  data REGYR/10.0/
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
           if (RAN .lt. -1.0 .or. RAN .gt. 1.0) go to 1
           CR = CR + 0.07985 * RAN
           if (CR .gt. .90) CR = .90
           if (CR .lt. .20) CR = .20
           ICR(I) = (CR*100.0)+0.5
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
        call HTCALC (MODE0,IVAR,ISPC,SITEAR(ISPC),YRS,H,AGET,HTMAX, &
             & HTG1,JOSTND,DEBUG)

        !  IF INITIAL TREE HEIGHT IS >= MAX HEIGHT FOR SITE (HTMAX).
        !  IF TRUE, THEN SET HTG= 0.1 FT.

        if (HTMAX-H.le.1.) then
           if (DEBUG) write(JOSTND,*) 'HTI>=HTMAX , ABIRTH= ',ABIRTH(I)
           HTGR = 0.10
        else

           !     FIND HTGR

           MODE0 = 9
           IVAR = 3
           YRS = 10.
           H = HT(I)
           call HTCALC (MODE0,IVAR,ISPC,SITEAR(ISPC),YRS,H,AGET,HTMAX, &
                & HTGR,JOSTND,DEBUG)

           if (DEBUG)write(JOSTND,*)'I,ISPC,HGADJ,FNT,ABIRTH= ', &
                & I,ISPC,HGADJ(ISPC),FNT,ABIRTH(I)
           if (DEBUG)write(JOSTND,*)'I,SCALE,SCALE2,HTGR,DBH,BA,IPCCF= ', &
                & I,SCALE,SCALE2,HTGR,D,BA,IPCCF

           HTGR = HTGR*CON*SCALE*HGADJ(ISPC)*XRHGRO
        end if

        !  GET BAL MODIFIER BORROWED FROM THE DIAMETER GROWTH EQUATIONS AND
        !  ADJUST THE HEIGHT GROWTH ESTIMATE.
        !  POTENTIALLY ONLY AFFECT HEIGHT GROWTH HALF AS MUCH AS DIA GROWTH
        !  TEMPER THE ADJUSTMENT BY RELATIVE HEIGHT

        call BALMOD(ISPC,D,GMOD)
        if (DEBUG)write(16,*)'AFTER BALMOD GMOD,HT,AVH= ',GMOD,HT(I),AVH
        !      GMOD = (GMOD+1.0)/2.0
        RELHTA = 0.
        if (AVH .gt. 0.)RELHTA = MIN((HT(I)/AVH),1.0)
        GMOD = 1.0-((1.0-GMOD)*(1.0-RELHTA))
        HTGR = HTGR*GMOD
        if (DEBUG)write(JOSTND,*)'I,GMOD,HTGR= ',I,GMOD,HTGR

        if (HTGR .lt. 0.1) HTGR = 0.1

        !     COMPUTE WEIGHTS FOR THE LARGE AND SMALL TREE HEIGHT INCREMENT
        !     ESTIMATES.  IF DBH IS LESS THAN OR EQUAL TO XMN, THE LARGE TREE
        !     PREDICTION IS IGNORED (XWT=0.0).

        XWT = (D-XMN)/(XMX-XMN)
        if (D.le.XMN .or. LESTB) XWT = 0.0

        !     COMPUTE WEIGHTED HEIGHT INCREMENT FOR NEXT TRIPLE.

        if (DEBUG)write(JOSTND,9982)XWT,HTGR,HTG(K),I,K
        9982 format('IN REGENT 9982 FORMAT',3(F10.4,2X),2I7)

        HTGR = HTGR*(1.0-XWT) + XWT*HTG(K)
        if (HTGR .lt. .1) HTGR = .1

        !   ADD RANDOM AFFECTS. LIMIT TO + OR -  1/10 HTGR

        RAN = 0.
        if (DGSD .ge. 1.0) then
           3 continue
           RAN = BACHLO(0.0,1.0,RANN)
           if (RAN.lt.-1.0 .or. RAN.gt.1.0) go to 3
        end if
        HTGR = HTGR + RAN*0.1*HTGR

        HTG(K) = HTGR
        if (HTG(K) .lt. .1) HTG(K) = .1

        if (DEBUG)write(JOSTND,9985)HTGR,HTG(K),I,K,LESTB
        9985 format('IN REGENT HTGR,HTG(K),I,K,LESTB= '/ &
             & 2(F10.4,2X),2I10,L10)

        ! CHECK FOR SIZE CAP COMPLIANCE.

        if ((H+HTG(K)).gt.SIZCAP(ISPC,4)) then
           HTG(K) = SIZCAP(ISPC,4)-H
           if (HTG(K) .lt. 0.1) HTG(K) = 0.1
        end if

        4 continue

        !     ASSIGN DBH AND COMPUTE DBH INCREMENT FOR TREES WITH DBH LESS
        !     THAN 5 INCHES (COMPUTE 10-YEAR DBH INCREMENT REGARDLESS OF
        !     PROJECTION PERIOD LENGTH).

        if (D.ge.XMX) go to 23
        HK = H + HTG(K)
        if (HK .le. 4.5) then
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
           if (H .le. 4.5) then
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

           if (.not.LHTDRG(ISPC) .or. &
                & (LHTDRG(ISPC) .and. IABFLG(ISPC).eq.1)) then
              call HTDBH (IFOR,ISPC,DKK,HK,1)
              if (H .le. 4.5) then
                 DK = D
              else
                 call HTDBH (IFOR,ISPC,DK,H,1)
              end if
              if (DEBUG)write(JOSTND,*)'INV EQN DUBBING IFOR,ISPC,H,HK,DK,' &
                   & ,'DKK= ',IFOR,ISPC,H,HK,DK,DKK
              if (DEBUG)write(JOSTND,*)'ISPC,LHTDRG,IABFLG= ', &
                   & ISPC,LHTDRG(ISPC),IABFLG(ISPC)
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
              if (DK.lt.0.0 .or. DKK.lt.0.0) then
                 DG(K) = HTG(K)*0.2*BARK*XRDGRO
                 DKK = D+DG(K)
              else
                 DGSM = (DKK-DK)*BARK*XRDGRO

                 !         COMPUTE WEIGHTED DIAMETER INCREMENT FOR NEXT TRIPLE.

                 if (DEBUG)write(JOSTND,9983)XWT,DGSM,DG(K),I,K
                 9983 format('IN REGENT 9983 FORMAT',3(F10.4,2X),2I7)

                 DGGR = DGSM*(1.0-XWT) + XWT*DG(K)
                 if (DGGR .lt. .1) DGGR = .1
                 DG(K) = DGGR*BARK*XRDGRO
              end if
              if (DEBUG)write(JOSTND,*)'K,DK,DKK,DG= ',K,DK,DKK,DG(K)

              if (DG(K) .lt. 0.0) DG(K) = 0.1
              if (DG(K) .gt. DGMX) DG(K) = DGMX

              !         SCALE DIAMETER INCREMENT TO 10-YR ESTIMATE.
              !         SCALE ADJUSTMENT IS ON GROWTH IN DDS RATHER THAN INCHES
              !         OF DG TO BE CONSISTENT WITH GRADD.

              DDS = DG(K)*(2.0*BARK*DK+DG(K))*SCALE2
              DG(K) = SQRT((DK*BARK)**2.0+DDS)-BARK*DK

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
           9987 format('IN REGENT, I=',I4,',  ISPC=',I3,'  CUR HT=',F7.2, &
                & ',  HT INC=',F7.4,',  NEW HT=',F7.2,',  CUR DBH=',F10.5, &
                & ',  DBH INC(I,K)=',2F7.4)
        end if
        23 continue

        !  RETURN TO PROCESS NEXT TRIPLE IF TRIPLING.  OTHERWISE,
        !  PRINT DEBUG AND RETURN TO PROCESS NEXT TREE.

        if (LESTB .or. .not.LTRIP .or. L.ge.2) go to 22
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
     if (I1.eq.0 .or. .not. LHTCAL(ISPC)) go to 100
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

        if (DEBUG) write(JOSTND,*)'IN REGENT 8900 H =',H

        !  CALCULATE TREE HEIGHT INCREMENT
        !  HEIGHT GROWTH EQUATION, USING NC128 TREE HEIGHT COEFFS.
        !  EVALUATED FOR EACH TREE EACH CYCLE, 5.0 YEAR CYCLE
        !  FIRST FIND EFFECTIVE TREE AGE BASED ON H

        MODE0 = 0
        IVAR = 3
        YRS = 10.
        H = HT(I)
        call HTCALC (MODE0,IVAR,ISPC,SITEAR(ISPC),YRS,H,AGET,HTMAX, &
             & HTG1,JOSTND,DEBUG)

        !     THEN FIND HTGR

        MODE0 = 9
        IVAR = 3
        YRS = 10.
        H = HT(I)
        call HTCALC (MODE0,IVAR,ISPC,SITEAR(ISPC),YRS,H,AGET,HTMAX, &
             & HTGR,JOSTND,DEBUG)

        !  GET BAL MODIFIER BORROWED FROM THE DIAMETER GROWTH EQUATIONS AND
        !  ADJUST THE HEIGHT GROWTH ESTIMATE.
        !  POTENTIALLY ONLY AFFECT HEIGHT GROWTH HALF AS MUCH AS DIA GROWTH
        !  TEMPER THE ADJUSTMENT BY RELATIVE HEIGHT

        call BALMOD(ISPC,D,GMOD)
        if (DEBUG)write(16,*)'AFTER BALMOD GMOD,HT,AVH= ',GMOD,HT(I),AVH
        !      GMOD = (GMOD+1.0)/2.0
        RELHTA = 0.
        if (AVH .gt. 0.)RELHTA = MIN((HT(I)/AVH),1.0)
        GMOD = 1.0-((1.0-GMOD)*(1.0-RELHTA))
        HTGR = HTGR*GMOD
        if (DEBUG)write(JOSTND,*)'I,GMOD,HTGR= ',I,GMOD,HTGR

        if (HTGR .lt. .1) HTGR = .1

        EDH = HTGR * RHCON(ISPC)
        if (EDH .lt. 0.1) EDH = 0.1
        P = PROB(I)
        if (HTG(I).lt.0.001) go to 60
        TERM = HTG(I) * SCALE3
        SNP = SNP+P
        SNX = SNX+EDH*P
        SNY = SNY+TERM*P
        N = N+1

        !  PRINT DEBUG INFO IF DESIRED.

        if (DEBUG)write(JOSTND,9991) NPLT,I,ISPC,H,DBH(I),ICR(I), &
             & PCT(I),RELDM1,RHCON(ISPC),EDH,TERM,HTGR,SCALE3,HGADJ(ISPC), &
             & ICR(I)
        9991 format('NPLT=',A8,',  I=',I5,',  ISPC=',I3,',  H=',F6.1, &
             & ',  DBH=',F5.1,',  ICR',I5,',  PCT=',F6.1,',  RELDEN=', &
             & F6.1 / 12X,'RHCON=',F10.3,',  EDH=',F10.3,', TERM=',F10.3, &
             & ' HTGR= ',F10.3,' SCALE3= ',F10.3,' HGADJ(ISPC)= ',F10.3/ &
             & ' ICR= ',I5)

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
  !  IF THE BUDWIDTH VALUES IN THE DIAM ARRAY, BELOW, ARE MODIFIED
  !  THEN ALSO MODIFY THE VALUES IN THE SNDBAL ARRAY IN **HTDBH

  data DIAM/ &
       & 0.1, 0.1, 0.2, 0.2, 0.2, 0.2, 0.2, 0.1, 0.4, 0.5, &
       & 0.5, 0.1, 0.1, 0.5, 0.5, 0.1, 0.1, 0.5, 0.1, 0.4, &
       & 0.5, 0.5, 0.5, 0.5, 0.3, 0.2, 0.2, 0.2, 0.2, 0.1, &
       & 0.1, 0.1, 0.1, 0.1, 0.3, 0.3, 0.3, 0.3, 0.3, 0.1, &
       & 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.3, 0.2, &
       & 0.1, 0.2, 0.2, 0.1, 0.2, 0.2, 0.1, 0.1, 0.2, 0.2, &
       & 0.2, 0.1, 0.2, 0.2, 0.1, 0.2, 0.2, 0.1, 0.2, 0.1, &
       & 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.3, 0.4, 0.1, &
       & 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.1, 0.1, 0.1, 0.1, &
       & 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.3, 0.1, &
       & 0.1, 0.1, 0.2, 0.1, 0.1, 0.2, 0.2, 0.1/

  do ISPC = 1,MAXSP
     RHCON(ISPC) = 1.0
     if (LRCOR2.and.RCOR2(ISPC).gt.0.0) &
          & RHCON(ISPC) = RCOR2(ISPC)
     90 continue
  end do
  return
end subroutine REGENT
