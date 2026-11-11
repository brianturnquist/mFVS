!============================!
!   Compute tree mortality   !
!============================!
! NI $Id$
subroutine MORTS
  implicit none

  !  THIS SUBROUTINE COMPUTES PERIODIC MORTALITY RATES FOR EACH TREE
  !  RECORDS AND THEN REDUCES THE NUMBER OF TREES/ACRE REPRESENTED BY THE
  !  TREE RECORD.  MORTALITY RATE IS PREDICTED FROM DBH, DBH INCREMENT,
  !  RELATIVE DIAMETER, AND STAND BASAL AREA. AN ADJUSTMENT IS MADE TO
  !  INCREASE MORTALITY RATE AS THE STAND BASAL AREA APPROACHES THE
  !  THEORETICAL MAXIMUM THAT THE HABITAT TYPE CAN SUPPORT.  THIS ROUTINE
  !  IS CALLED FROM **TREGRO** WHEN CYCLING FOR GROWTH PREDICTION.  ENTRY
  !  **MORCON** IS ACCESSED TO LOAD SITE DEPENDENT CONSTANTS.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'
  include 'CALCOM.f90'
  include 'CONTRL.f90'
  include 'COEFFS.f90'
  include 'ESTREE.f90'
  include 'MULTCM.f90'
  include 'PDEN.f90'
  include 'WORKCM.f90'

  !  DEFINITIONS:

  !     PMSC -- CONSTANT TERMS FOR EACH SPECIES FOR THE MORTALITY
  !             RATE EQUATION.
  !        I -- TREE SUBSCRIPT.
  !        D -- TREE DIAMETER.
  !      RIP -- ESTIMATED ANNUAL MORTALITY RATE BASED ON HAMILTON'S
  !             MODEL.
  !       RZ -- RATE SUCH THAT BASAL AREA IN 10 YEARS IS EQUAL TO BA
  !             PLUS (BAMAX-BA)/BAMAX PROPORTION OF THE PREDICTED BA
  !             INCREMENT.
  !     RIPP -- WEIGHTED AVERAGE MORTALITY RATE BASED ON BA, BAMAX,
  !             RIP, AND RZ.  THIS IS THE RATE THAT IS USED TO PREDICT
  !             TREE MORTALITY.  IT IS AN ANNUAL RATE THAT WILL BE
  !             COMPOUNDED TO OBTAIN A FINT-YEAR RATE.
  !        P -- NUMBER OF TREES PER ACRE REPRESENTED BY A TREE.
  !      WKI -- SCALAR USED FOR CALCULATION AND TEMPORARY STORAGE
  !             OF TREE MORTALITY (TREES/ACRE).

  ! Variables
  logical :: DEBUG,LINCL
  real    :: PMSC(MAXSP),PRM(6)
  integer :: MYACTS(2)
  real    :: POT(54)
  integer :: IPDG(30,11),IPDG2(30,11)
  integer :: IPT,IPT2,IX,IG,IULIM,IGRP,IDMFLG,I1,I2,ITODO,KPOINT
  integer :: KBIG,IP,ISPC,I3,IS,ISPCC,IDATE,IACTK,NP,NTODO
  integer :: I,J
  real    :: CREDIT,TEMP,XMORE,VLOS,PRES,XCHECK,X,POTENT,RIPP,DGT,RELDBH
  real    :: B5,D1,D2,XMORT,AVED,WPROB,DSUM,RZ,TTB,TB,BA10,DELTBA
  real    :: T,SD2SQ,P,D,BARK,BRATIO,G,CIOBDS,DQ10,WKI,RIP

  ! Data statements
  data MYACTS/94,97/
  data PMSC/&
       &0.0,-.17603,2*0.317888,0.607725,1.57976,-.12057,.94019,&
       &2*.21180,0.0/

  ! IN THE ARRAYS IPDG AND IPDG2, THE I INDEX REFERS TO HABITAT TYPE,
  ! THE J INDEX REFERS TO NATIONAL FOREST.

  data ((IPDG(I,J),I = 1,30),J = 1,2)/&
       &7, 5*6, 5,6,5, 4*6, 8, 5*7, 5,3,4,3,4,4,6,5,1,1,6,&
       &4*15, 14,14,12,14,13,14,14,15,15,17,15,15,16,15,15,11,10,11,9,&
       &12,12,15,12,7,10,15/
  data ((IPDG(I,J),I = 1,30),J = 3,5)/&
       &4*14, 13,13,11,14,12,14, 3*14, 17,14,14, 3*15, 11,9,10,8,11,11,&
       &15,11,5,10,15,&
       &4*12, 11,11, 3*10, 11,11,12,12, 3*14, 3*13, 9,6,8,7,9,10,12,10,&
       &4,8,12,&
       &4*10, 9,9,8,9,8,9, 3*10, 12, 5*11, 8,6,7,5,7,7,10,8,6,7,10/
  data ((IPDG(I,J),I = 1,30),J = 6,7)/&
       &6*9, 7, 3*8, 3*9, 11, 4*10, 11,7,5,6,4,7,7,9,8,3,6,9,&
       &13, 5*12, 10,12, 3*11, 12,12,15,13,13,14,13,13,9,8,9,7,10,10,&
       &12,10,4,8,12/
  data ((IPDG(I,J),I = 1,30),J = 8,9)/&
       &4*9, 8,9,7,8,7, 3*8, 9,11, 5*10, 7,5,6,5,7,7,9,8,3,6,9,&
       &4*7, 6,7,5,6,7,6,6,7,7,9,9,8,9,8,8,5,3,5,4,5,5,7,5,2,4,7/
  data ((IPDG(I,J),I = 1,30),J = 10,11)/&
       &4*15, 14,14,9,13,13,14,14,13,14,18, 5*14, 10,9,9,8,11,11,15,9,&
       &6,8,15,&
       &4*15, 14,14,12,14,13,14,14,15,15,17,15,15,16,15,15,11,10,11,9,&
       &12,12,15,12,7,10,15/
  data ((IPDG2(I,J),I = 1,30),J = 1,2)/&
       &30, 3*29, 28,28,27,31,27,27,28,31,32,32,31,31,32,31,31,25,23,24,&
       &23,24,24,27,26,18,16,27,&
       &50, 3*49, 48,48,47,52,46,47,48,53,54,54,52,52,54,50,51,41,39,44,&
       &43,44,44,50,53,36,37,50/
  data ((IPDG2(I,J),I = 1,30),J = 3,5)/&
       &4*45, 44,44,43,48,43,43,44,49,50,50, 3*49, 47,47,39,36,40,39,40,&
       &39,45,41,33,34,45,&
       &4*41, 40,40,39,42,39,39,40,44, 5*45, 43,44,36,33,36,36,37,38,41,&
       &38,30,31,41,&
       &38, 3*37, 36,37,37,40,35,36,37,41,41,42,40,40,41,40,40,33,30,33,&
       &31,32,31,36,34,32,27,36/
  data ((IPDG2(I,J),I = 1,30),J = 6,7)/&
       &38, 3*37, 3*36, 38,36,35,37, 3*41, 40,40,41,40,40,33, 3*32, 34,&
       &33,38,35,26,28,38,&
       &4*41, 40,40,39,45,37,38,39,43,44,46,43,43,45,43,43,35,33,35,35,&
       &36,36,41,37,30,32,41/
  data ((IPDG2(I,J),I = 1,30),J = 8,9)/&
       &38, 6*37, 41,33,37,37,41,41,42,39,39,41,40,40,33,32,33,33,34,33,&
       &38,35,28,32,38,&
       &31,29,30,31,29,30,29,33,31,30,29,34,34,35,34,34,35,33,33,27,23,&
       &26,26,27,26,31,26,19,23,31/
  data ((IPDG2(I,J),I = 1,30),J = 10,11)/&
       &4*45, 44,44,43,48,41,42,43,47,48,50,47,47,46,44,44,36,34,37,37,&
       &38,37,43,34,30,33,43,&
       &50, 3*49, 48,48,47,52,46,47,48,53,54,54,52,52,54,50,51,41,39,44,&
       &43,44,44,50,53,36,37,50/
  data POT/&
       &.25,.30,.35,.40,.45,.50,.55,.60,.65,.70,.75,.80,.85,.90,.95,1.00,&
       &1.05,1.10,1.15,1.20,1.25,1.30,1.35,1.40,1.45,1.50,1.55,1.60,1.65,&
       &1.70,1.75,1.80,1.85,1.90,1.95,2.00,2.05,2.10,2.15,2.20,2.25,2.30,&
       &2.35,2.40,2.45,2.50,2.55,2.60,2.65,2.70,2.75,2.80,2.85,2.90/

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'MORTS',5,ICYC)

  !  IF THERE ARE NO TREE RECORDS, SIMPLY PROCESS THE MORTALITY
  !  MULTIPLIERS AND BRANCH TO END (FOR THE METHOD USED TO GET
  !  MULTIPLIERS, SEE SUBROUTINE MULTS).

  call OPFIND (1,MYACTS(1),NTODO)
  if (NTODO.eq.0) goto 12
  do I = 1,NTODO
     call OPGET(I,4,IDATE,IACTK,NP,PRM)
     if (IACTK.lt.0) goto 9
     call OPDONE(I,IY(ICYC))
     ISPCC = IFIX(PRM(1))
     if (ISPCC.eq.0) goto 2
     XMMULT(ISPCC) = PRM(2)
     XMDIA1(ISPCC) = PRM(3)
     XMDIA2(ISPCC) = PRM(4)
     goto 9
     2 continue
     do ISPCC = 1,MAXSP
        XMMULT(ISPCC) = PRM(2)
        XMDIA1(ISPCC) = PRM(3)
        XMDIA2(ISPCC) = PRM(4)
        5 continue
     end do
     9 continue
  end do
  12 continue

  ! IF BARE GROUND PLANT, LIMITS WERE NOT ADJUSTED FROM A PERCENT TO A
  ! PROPORTION IN CRATET, ADJUST THEM HERE.

  if (PMSDIL .gt.1.0)PMSDIL = PMSDIL/100.
  if (PMSDIU .gt.1.0)PMSDIU = PMSDIU/100.

  if (ITRN.le.0) goto 100

  ! SDIMAX IS USED HERE TO CARRY WEIGHTED SDI MAXIMUM. IF A USER-DEFINED
  ! BASAL AREA MAXIMUM HAS NOT BEEN ENTERED, THEN BASAL AREA MAXIMUM
  ! WILL BE RESET TO BE CONSISTENT WITH THE SDI MAXIMUM. THIS IS NEEDED
  ! FOR MODELING CLIMATE CHANGE.

  call SDICAL(0,SDIMAX)
  if (DEBUG)write(JOSTND,*)'IN MORTS CYCLE= ',ICYC,'  BAMAX= ',&
       &BAMAX,'  SDIMAX= ',SDIMAX

  !  ESTIMATE QUADRATIC MEAN DIAMETER 10 YEARS HENCE.

  T = 0.0
  SD2SQ = 0.0
  do I = 1,ITRN
     P = PROB(I)
     IS = ISP(I)
     D = DBH(I)
     BARK = BRATIO(IS,D,HT(I))
     G = DG(I)/BARK
     CIOBDS = (2.0*D*G+G*G)
     SD2SQ = SD2SQ+P*(D*D+CIOBDS)
     T = T+P
     20 continue
  end do
  DQ10 = SQRT(SD2SQ/T)
  DELTBA = .005454154*DQ10*DQ10*T-BA

  !  PROJECT BASAL AREA FORWARD 10 YEARS ASSUMING THAT BA/BAMAX
  !  PROPORTION OF THE PREDICTED BASAL AREA INCREMENT WILL BE LOST TO
  !  MORTALITY.  AT BA=BAMAX, MORTALITY WILL ABSORB ALL INCREMENT.

  BA10 = BA+((BAMAX-BA)/BAMAX)*DELTBA
  TB = BA10/(.005454154*DQ10*DQ10)

  !  NOW CONVERT TO AN ESTIMATE ON ANNUAL TPA MORTALITY RATE.

  TTB = (T-TB)/T
  if (TTB.gt.0.9999) TTB = 0.9999
  RZ = 1.0-(1.0-(TTB))**0.1

  !  CALCULATE AVERAGE DBH

  DSUM = 0.0
  WPROB = 0.0
  do I = 1,ITRN
     WPROB = WPROB+PROB(I)
     51 DSUM = DSUM+DBH(I)*PROB(I)
  end do
  AVED = DSUM/WPROB

  !  START LOOP TO ESTIMATE MORTALITY RATE.  TREES ARE PROCESSED
  !  ONE AT A TIME WITHIN A SPECIES.

  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (I1.le.0) go to 50
     I2 = ISCT(ISPC,2)
     XMORT = XMMULT(ISPC)
     D1 = XMDIA1(ISPC)
     D2 = XMDIA2(ISPC)
     B5 = PMSC(ISPC)

     !  START TREE LOOP WITHIN SPECIES.

     do I3 = I1,I2

        !  INITIALIZE FOR NEXT TREE.

        I = IND1(I3)
        P = PROB(I)
        WKI = 0.0
        WK2(I) = 0.0
        D = DBH(I)
        BARK = BRATIO(ISPC,D,HT(I))
        if (P.le.0.0) go to 40
        RELDBH = D/AVED
        if (D.le.0.5)D = 0.5
        DGT = WK1(I)/OLDFNT
        if (D.le.1.0.and. DGT.lt.0.05) DGT = 0.05
        if (D.le.5.0.and. D.gt.1.0.and. DGT.lt.0.05)&
             &DGT = 0.05*(5.0-D)/4.0
        G = WK1(I)/(BARK*OLDFNT)
        if (WK1(I)/OLDFNT .lt. DGT)G = DGT/BARK

        !  COMPUTE MORTALITY RATE

        if ((ICYC.eq.1.or. WK1(I).eq.0.0) .and. DG(I).gt.0.5)&
             &G = DG(I)/(BARK*10.0)
        IP = 1
        if (D.le.5.0)IP = 2
        G = G*GMULT(IP)
        RIP = 2.76253+0.222310*SQRT(D)-0.0460508*SQRT(BA)+&
             &11.2007*G-0.554421/D+B5+0.246301*RELDBH+6.07129*G/D
        if (RIP.gt.70.0) RIP = 70.0
        if (RIP.lt.-70.0) RIP = -70.0
        RIP = (1.0/(1.0+EXP(RIP)))
        POTENT = REIN(IP)
        RIP = RIP*POTENT
        RIPP = BA*RZ

        !  MAKE ADJUSTMENT FOR APPROACH TO MAXIMUM BASAL AREA.

        if (BA.gt.BAMAX)go to 27
        RIPP = RIPP+(BAMAX-BA)*RIP
        27 RIPP = RIPP/BAMAX
        if (RIPP.lt.RIP)RIPP = RIP
        if (RIPP.gt.1.0)RIPP = 1.0

        !  APPLY MORTALITY MULTIPLIERS WITHIN USER-PROVIDED DBH LIMITS
        !  AND STORE TREES PER ACRE DYING IN THE ARRAY WK2.

        X = 1.0
        if (D.ge.D1.and. D.lt.D2) X = XMORT

        !  FROM HERE TO STATEMENT 29 IS ESTABLISHMENT MODEL CODE.
        !  "BEST" TREES NOT SUBJECT TO MORTALITY FOR 20 YRS
        !  AFTER DISTURBANCE DATE.

        if (IESTAT(I).le.0) go to 29
        if (IY(ICYC).ge.IESTAT(I)) IESTAT(I) = 0
        XCHECK = FLOAT(IESTAT(I)-IY(ICYC))/FINT
        if (XCHECK.gt.1.0) XCHECK = 1.0
        if (XCHECK.lt.0.0) XCHECK = 0.0
        X = X* (1.0-XCHECK)
        29 continue
        WKI = P*(1.0-(1.0-RIPP)**FINT)*X

        BARK = BRATIO(ISPC,D,HT(I))
        G = (DG(I)/BARK) * (FINT/10.0)
        IDMFLG = IFIX(SIZCAP(ISPC,3))
        if ((D+G).ge.SIZCAP(ISPC,1) .and. IDMFLG.ne.1) then
           WKI = AMAX1(WKI,(P*SIZCAP(ISPC,2)*FINT/10.0))
           if (DEBUG)write(JOSTND,*)'SIZE CAP RESTRICTION IMPOSED, ',&
                &'I,ISPC,D,P,SIZCAP 1-3,WKI = ',&
                &I,ISPC,D,P,SIZCAP(ISPC,1),SIZCAP(ISPC,2),SIZCAP(ISPC,3),WKI
        end if
        if (WKI.gt.P) WKI = P

        ! IF SDIMAX IS LESS THAN 5, ASSUME CLIMATE HAS CHANGED ENOUGH THAT THE
        ! SITE WILL NO LONGER SUPPORT TREES, AND KILL ALL EXISTING TREES.

        if (SDIMAX .lt.5) then
           WKI = P
        end if
        WK2(I) = WKI

        !  END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.

        if (.not.DEBUG) go to 40
        PRES = P-WKI
        VLOS = WKI*CFV(I)/FINT
        write(JOSTND,9000) I,ISPC,D,G,RELDBH,BA,P,WKI,PRES,VLOS
        9000 format('IN MORTS, I=',I4,',  ISPC=',I3,',  DBH=',F7.2,&
             &',  DBH INCREMENT=',F7.4,',  RELDBH=',F7.4,',  BA=',F7.2,&
             &',  INIT PROB=',F9.3/&
             &', TREES DYING=',F9.3,'  RES PROB=',F9.3,&
             &',  VOL LOST=',F9.3)
        40 continue
     end do

     !  END OF SPECIES LOOP.  PRINT DEBUG INFO IF DESIRED.

     if (.not.DEBUG) go to 50
     write(JOSTND,9001)  ISPC,B5
     9001 format('IN MORTS,  ISPC=',I3,&
          &11X,'B5=',F8.6)
     50 continue
  end do

  100 continue

  !  COMPUTE THE CLIMATE-PREDICTED MORTALITY RATES BY SPECIES

  call CLMORTS

  !  COMPUTE THE FIXMORT OPTION.  LOOP OVER ALL SCHEDULED FIXMORT'S
  !  LINCL IS USED TO INDICATE WHETHER A TREE GETS AFFECTED OR NOT

  call OPFIND (1,MYACTS(2),NTODO)
  if (NTODO.gt.0) then
     if (DEBUG)write(JOSTND,*)'FIXMORT PROCESSING, ITODO= ',ITODO
     do ITODO = 1,NTODO
        call OPGET (ITODO,6,IDATE,IACTK,NP,PRM)
        if (IACTK.lt.0) goto 300
        call OPDONE(ITODO,IY(ICYC))
        ISPCC = IFIX(PRM(1))
        if (NP .le.4) then
           if (PRM(2).gt.1.0)PRM(2) = 1.0
        end if
        if (PRM(3).lt.0.0)PRM(3) = 0.0
        if (PRM(4).le.0.0)PRM(4) = 999.
        IP = 1
        if (NP.gt.4) then
           if (PRM(5).lt.3.) then
              if (PRM(2).gt.1.0)PRM(2) = 1.0
              if (PRM(2).lt.0.0)PRM(2) = 0.0
           end if
           if (PRM(5).eq.1.0) then
              IP = 2
           else if (PRM(5).eq.2.0) then
              IP = 3
           else if (PRM(5).eq.3.) then
              IP = 4
           end if
        end if

        !  SET FLAG FOR POINT MORTALITY, OR KILLING FROM ABOVE
        !    PRM(6)    POINT      SIZE   KBIG     KILL DIRECTION
        !      0         NO        NO     0                       DEFAULT CONDITION
        !      1         YES       NO     0
        !     10         NO        YES    1       BOTTOM UP
        !     11         YES       YES    1       BOTTOM UP
        !     20         NO        YES    2       TOP DOWN
        !     21         YES       YES    2       TOP DOWN

        KPOINT = 0
        KBIG = 0
        if (PRM(6).gt.0.) then
           if (PRM(6) .eq.1) then
              KPOINT = 1
           else if (PRM(6) .eq.10) then
              KBIG = 1
           else if (PRM(6) .eq.11) then
              KPOINT = 1
              KBIG = 1
           else if (PRM(6) .eq.20) then
              KBIG = 2
           else if (PRM(6) .eq.21) then
              KPOINT = 1
              KBIG = 2
           end if
        end if
        if (ITRN.gt.0) then

           ! IF CONCENTRATING MORTALITY ON A POINT, AND/OR BY SIZE TREES IS IN
           ! EFFECT, DETERMINE EFFECT OF THIS FIXMORT AND REALLOCATE BY POINT:
           !   REALLOCATE ALL MORTALITY IF REPLACE OPTION OR MULTIPLY OPTION
           !   ARE IN EFFECT.
           !   ONLY REALLOCATE ADDITIONAL MORTALITY IF "ADD" OPTION IS IN EFFECT
           !   ONLY REALLOCATE ADDITIONAL MORTALITY IF "MAX" OPTION IS IN EFFECT
           !   (I.E. MORTALITY OVER AND ABOVE WHAT WAS PREVIOUSLY PREDICTED.

           if (KBIG.ge.1.or. (KPOINT.eq.1.and. IPTINV.gt.1)) then
              XMORE = 0.
              do I = 1,ITRN
                 LINCL = .false.
                 if (ISPCC.eq.0.or. ISPCC.eq.ISP(I)) then
                    LINCL = .true.
                 else if (ISPCC.lt.0) then
                    IGRP = -ISPCC
                    IULIM = ISPGRP(IGRP,1)+1
                    do IG = 2,IULIM
                       if (ISP(I) .eq. ISPGRP(IGRP,IG)) then
                          LINCL = .true.
                          go to 91
                       end if
                       90 continue
                    end do
                 end if
                 91 continue
                 if (LINCL .and.&
                      &(PRM(3).le.DBH(I) .and. DBH(I).lt.PRM(4))) then
                    goto (191,192,193,194),IP
                    191 continue
                    XMORE = XMORE+PROB(I)*PRM(2)
                    WK2(I) = 0.
                    goto 199
                    192 continue
                    XMORE = XMORE+(AMAX1(0.0,PROB(I)-WK2(I))*PRM(2))
                    goto 199
                    193 continue
                    TEMP = AMAX1(WK2(I),(PROB(I)*PRM(2)))
                    if (TEMP .gt. WK2(I)) then
                       XMORE = XMORE+TEMP-WK2(I)
                    end if
                    goto 199
                    194 continue
                    XMORE = XMORE+WK2(I)*PRM(2)
                    WK2(I) = 0.
                    goto 199
                 end if
                 199 continue
              end do
              if (DEBUG)write(JOSTND,*)'KPOINT,KBIG,ITRN,XMORE= ',&
                   &KPOINT,KBIG,ITRN,XMORE
              CREDIT = 0.
              do I = 1,ITRN
                 IWORK1(I) = IND1(I)
                 if (KBIG .eq.1) then
                    WORK3(I) = (-1.0)*&
                         &(DBH(I)+DG(I)/BRATIO(ISP(I),DBH(I),HT(I)))
                 else
                    WORK3(I) = DBH(I)+DG(I)/BRATIO(ISP(I),DBH(I),HT(I))
                 end if
                 201 continue
              end do
              call RDPSRT(ITRN,WORK3,IWORK1,.false.)
              if (DEBUG)write(JOSTND,*)'DBH= ',(DBH(IG),IG = 1,ITRN)
              if (DEBUG)write(JOSTND,*)'IWORK1= ',(IWORK1(IG),IG = 1,ITRN)
              if (DEBUG)write(JOSTND,*)'WK2= ',(WK2(IG),IG = 1,ITRN)

              if (KBIG.ge.1.and. KPOINT.eq.0) then

                 !  CONCENTRATION BY SIZE ONLY

                 do I = 1,ITRN
                    IX = IWORK1(I)
                    LINCL = .false.
                    if (ISPCC.eq.0.or. ISPCC.eq.ISP(IX)) then
                       LINCL = .true.
                    else if (ISPCC.lt.0) then
                       IGRP = -ISPCC
                       IULIM = ISPGRP(IGRP,1)+1
                       do IG = 2,IULIM
                          if (ISP(IX) .eq. ISPGRP(IGRP,IG)) then
                             LINCL = .true.
                             go to 93
                          end if
                          92 continue
                       end do
                    end if
                    93 continue
                    if (LINCL .and.&
                         &(PRM(3).le.DBH(IX) .and. DBH(IX).lt.PRM(4))) then
                       TEMP = CREDIT+PROB(IX)-WK2(IX)
                       if ((TEMP .le. XMORE).or.&
                            &(ABS(TEMP-XMORE).lt.0.0001)) then
                          CREDIT = CREDIT+PROB(IX)-WK2(IX)
                          WK2(IX) = PROB(IX)
                       else
                          WK2(IX) = WK2(IX)+XMORE-CREDIT
                          CREDIT = XMORE
                          go to 295
                       end if
                    end if
                    310 continue
                 end do
                 go to 295

              else if (KPOINT.eq.1.and. KBIG.eq.0) then

                 !  CONCENTRATION ON POINTS ONLY

                 do J = 1,IPTINV
                    do I = 1,ITRN
                       if (ITRE(I) .ne. J)go to 204
                       LINCL = .false.
                       if (ISPCC.eq.0.or. ISPCC.eq.ISP(I)) then
                          LINCL = .true.
                       else if (ISPCC.lt.0) then
                          IGRP = -ISPCC
                          IULIM = ISPGRP(IGRP,1)+1
                          do IG = 2,IULIM
                             if (ISP(I) .eq. ISPGRP(IGRP,IG)) then
                                LINCL = .true.
                                go to 95
                             end if
                             94 continue
                          end do
                       end if
                       95 continue
                       if (LINCL .and.&
                            &(PRM(3).le.DBH(I) .and. DBH(I).lt.PRM(4))) then
                          TEMP = CREDIT+PROB(I)-WK2(I)
                          if ((TEMP .le. XMORE).or.&
                               &(ABS(TEMP-XMORE).lt.0.0001)) then
                             CREDIT = CREDIT+PROB(I)-WK2(I)
                             WK2(I) = PROB(I)
                          else
                             WK2(I) = WK2(I)+XMORE-CREDIT
                             CREDIT = XMORE
                             go to 295
                          end if
                       end if
                       204 continue
                    end do
                    205 continue
                 end do
                 go to 295

                 !  CONCENTRATION BY SIZE ON POINTS (POINTS HAVE PRIORITY, SO TREES
                 !  WILL BE KILLED BY SIZE ON ONE POINT BEFORE MOVING TO THE NEXT
                 !  POINT TO START WITH THE BIGGEST/SMALLEST TREES ON THAT POINT.
              else
                 do J = 1,IPTINV
                    do I = 1,ITRN
                       IX = IWORK1(I)
                       if (ITRE(IX) .ne. J)go to 311
                       LINCL = .false.
                       if (ISPCC.eq.0.or. ISPCC.eq.ISP(IX)) then
                          LINCL = .true.
                       else if (ISPCC.lt.0) then
                          IGRP = -ISPCC
                          IULIM = ISPGRP(IGRP,1)+1
                          do IG = 2,IULIM
                             if (ISP(IX) .eq. ISPGRP(IGRP,IG)) then
                                LINCL = .true.
                                go to 97
                             end if
                             96 continue
                          end do
                       end if
                       97 continue
                       if (LINCL .and.&
                            &(PRM(3).le.DBH(IX) .and. DBH(IX).lt.PRM(4))) then
                          TEMP = CREDIT+PROB(IX)-WK2(IX)
                          if ((TEMP .le. XMORE).or.&
                               &(ABS(TEMP-XMORE).lt.0.0001)) then
                             CREDIT = CREDIT+PROB(IX)-WK2(IX)
                             WK2(IX) = PROB(IX)
                          else
                             WK2(IX) = WK2(IX)+XMORE-CREDIT
                             CREDIT = XMORE
                             go to 295
                          end if
                       end if
                       311 continue
                    end do
                    312 continue
                 end do
                 go to 295
              end if

           end if

           !  NORMAL FIXMORT PROCESSING WHEN POINT OR SIZE CONCENTRATION
           !  IS NOT IN EFFECT.

           do I = 1,ITRN
              LINCL = .false.
              if (ISPCC.eq.0.or. ISPCC.eq.ISP(I)) then
                 LINCL = .true.
              else if (ISPCC.lt.0) then
                 IGRP = -ISPCC
                 IULIM = ISPGRP(IGRP,1)+1
                 do IG = 2,IULIM
                    if (ISP(I) .eq. ISPGRP(IGRP,IG)) then
                       LINCL = .true.
                       go to 99
                    end if
                    98 continue
                 end do
              end if
              99 continue
              if (LINCL .and.&
                   &(PRM(3).le.DBH(I) .and. DBH(I).lt.PRM(4))) then
                 goto (610,620,630,640),IP
                 610 continue
                 WK2(I) = PROB(I)*PRM(2)
                 goto 290
                 620 continue
                 WK2(I) = WK2(I)+(AMAX1(0.0,PROB(I)-WK2(I))*PRM(2))
                 goto 290
                 630 continue
                 WK2(I) = AMAX1(WK2(I),(PROB(I)*PRM(2)))
                 goto 290
                 640 continue
                 WK2(I) = AMIN1(PROB(I),WK2(I)*PRM(2))
                 goto 290
              end if
              290 continue
           end do
           295 continue
           if (DEBUG)write(JOSTND,*)'ITODO,WK2= ',&
                &ITODO,(WK2(IG),IG = 1,ITRN)
        end if
        300 continue
     end do
  end if
  return

  entry MORCON

  !  ENTRY POINT FOR LOADING MORTALITY MODEL CONSTANTS THAT REQUIRE ONE-
  !  TIME RESOLUTION.  ITYPE IS A HABITAT INDEX THAT IS COMPUTED IN
  !  **HABTYP** .

  IPT = IPDG(ITYPE,IFOR)
  POTEN = POT(IPT)
  GMULT(1) = .90/POTEN
  REIN(1) = (1-(POTEN/20.+1.)**(-1.605))/.06821
  IPT2 = IPDG2(ITYPE,IFOR)
  POTEN = POT(IPT2)
  GMULT(2) = 2.50/POTEN
  REIN(2) = (1-(POTEN+1.)**(-1.605))/.86610
  return
end
