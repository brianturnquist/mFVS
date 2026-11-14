!====================!
!   lpmpb : mpbmod   !
!====================!
! LPMPB $Id$
subroutine MPBMOD
  implicit none

  !     MOUNTAIN PINE BEETLE POPULATION DYNAMICS SIMULATOR

  !     BOB ROELKE; PROGRAMMER
  !     DON BURNELL; BOSS MAN
  !     NICK CROOKSTON; INTERFACE PROGRAMMER

  ! Revision History
  !   02/18/88 Last noted revision date.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.
  !   08/22/14 Lance R. David (FMSC)
  !     Function name was used as variable name.
  !     changed variable INT to INCRS

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'MPBCOM.f90'

  ! Variables
  character(len=1) :: IC1
  character(len=1) :: IC2
  character(len=1) :: IC22
  character(len=1) :: IC3
  character(len=1) :: IC4
  character(len=1) :: IC5
  character(len=1) :: IC6
  character(len=1) :: IC7
  character(len=1) :: ICBLK
  double precision :: BETIN,XX,PIODEN,DTA,DSMTA
  integer :: INC,INC1,KK,I,IB,ICALYR,IG,IP,LASTYR,NINC,NTRY

  real    :: ADEN,AMPAVE,APD,B1INC,B3ALL,B3INC,BATK,BATKI,BNEW,BYINC,BYT,E1,E2,E3,EARAT,EDEN,EF3,EFFS,&
       &EGGS,FMX,GEX,GFML1,GFML2,GFML3,GTEB,PCTGEM,PCTGEX,PCTGF1,PCTGF2,PCTGF3,PCTGSV,PSE1,PSE3,PSPE1,PSPE3,&
       &PSURV,RHO1,RHO2,RHO3,SKILL,SS,TAGG,TMMPB,TRKILL,XEG,XXG,YOUNG,AYEAR,BY,EMERG,OS,P,PERCNT,PMSLP,Q,&
       &RESIST,SEXRAT,SQFTPA,SSCUM,TA,TAFIT,TB1,TE1

  real    :: DIAM(30),TREES(30),AGG(30,15),AD(15),PHLOEM(30),B0(3),B1(3),B2(3),B3(3),B3SUM(3),&
       &EXODUS(3),BOLD(3),FM1(3),FM2(3),FM3(3),TEB(3),ANEX(3),EXLOSS(3),ANFML1(3),ANFML2(3),ANFML3(3),&
       &PCTTEB(3),PCTEX(3),PCTFM1(3),PCTFM2(3),PCTFM3(3),PCTSV(3),FML1(3),FML2(3),FML3(3),GENO(3),PX(10),&
       &TM(30),TKYR(30),SAK(3),TATRY(3),SEXDBH(30),AMP(15),PGRX(5),TAY(5)

  logical :: LGO,PASS1

  equivalence (CLASS(1,2),DIAM(1)), (CLASS(1,3),PHLOEM(1))

  data PGRX /.0,.7,1.,1.3,1.5/, TAY /2.0,2.1,2.4,2.7,3.0/

  data IB/1/,SEXRAT/.66/,SQFTPA/43560./,IC1/'*'/,IC2/'$'/,&
       &IC22/'B'/,IC3/'D'/,IC4/'A'/,IC5/'E'/,IC6/'X'/,IC7/'P'/,&
       &ICBLK/' '/

  !     SURF  = SURFACE AREA OF EACH TREE
  !     AMP   = AMPLITUDE MULTIPLIER FOR AGGREGATING TREES
  !     IBACK = PHERMONE EFFECTIVENESS (NUM OF TIME INTERVALS)
  !     PWR   = NO LONGER USED
  !     CE    = EXODUS CONSTANT
  !     CF1   = CONSTANT FOR FLIGHT MORTALITY OF GENOTYPE ONE
  !     CF2   = CONSTANT FOR FLIGHT MORTALITY OF GENOTYPE TWO
  !     CF3   = CONSTANT FOR FLIGHT MORTALITY OF GENOTYPE THREE
  !     TA    = THRESHOLD OF AGGREGATION
  !     DST   = DISTANCE EACH GENOTYPE CAN FLY
  !     EXCON = STAND SIZE IN ACRES
  !     ACTSRF= ACTUAL SURFACE AREA KILLED IN A REAL OUTBREAK
  !     SNOHST= SURFACE AREA OF ALL NON-HOST TREES
  !     CRITAD= CRITICAL ATTACK DENSITY. 1.5 BEETLES/SQ FT.
  !     NG    = NUMBER OF GENOTYPES
  !     MPMXYR= MAX NUMBER OF EPIDEMIC YEARS
  !     INCRS = NUM OF EMERGENCE INCREMENTS
  !     NACLAS= NUM OF TREE CLASSES, (PREVIOUSLY CALLED N)

  !     STRBUG= INITIAL BEETLES AS DEFINED BY USER.
  !     BY    = NUMBER OF BEETLES PER ACRE
  !     HS    = HABITAT SUITABILITY CLASSES ONE FOR EACH
  !             RESISTANCE CLASS
  !     P     = PROBABILITY OF SELECTING GENOTYPE ONE
  !     IPLTNO= PLOT NUMBER FOR GRAPHICS PACKAGE
  !     MPBYR = EPIDEMIC YEAR; ALSO USED TO SIGNAL RE-ENTRY
  !     ICALYR= CALENDAR YEAR OF EPIDEMIC
  !     TM    = TOTAL TREES PER ACRE KILLED ( CUM )
  !     TKYR  = NUMBER OF TREES/ACRE KILLLED THIS YEAR
  !     MPBGRF= TRUE IF GRAPHICS HAVE BEEN REQUESTED
  !     LASTYR= NUMBER OR YEARS MODEL HAS RUN UPON THIS ENTRY
  !     SQFTPA= NUMBER SQUARE FEET PER ACRE
  !     SEXDBH= PROPORTION FEMALES BY DBH CLASS
  !     LAGG  = TRUE IF AGGREGATION PHERMONE IS PRESENT
  !     LPS   = TRUE IF PREVENTITIVE SPRAY IS APPLIED
  !     LREP  = TRUE IF REPELLING PHERMONE IS PRESENT
  !     LDC   = TRUE IF DIRECT CONTROL IS TO BE APPLIED
  !     AGGPH = PROPORTIONATE EFFECT OF AGGREGATION PHERM.
  !     PSPK  = PROPORTION BEETLES KILLED BY PREVENTITIVE SPRAY
  !     PSE3  = EFFECTIVE SURFACE OF PREVENT. SPRAYED TREES
  !     PSDL  = PREV. SPRAY DIAMETER LIMITS
  !     PSPF  = PROPORTION OF TREES ABOVE 'PSDL' WHICH WERE
  !             REALLY SPRAYED
  !     REPL  = PROPORTIONATE EFFECT OF REPELLING PHERM.
  !     DCPF  = PROPORTION OF ATTACKED TREES REALLY DIR. CONT.
  !     DCPK  = PROPORTION OF BEETLES KILLED BY DIR. CONT.

  !     **********  EXECUTION BEGINS  **********

  SSCUM = SADLPP
  if ( MPBYR .gt.0 ) go to 25

  !     -- BRANCH TO RE-ENTRY POINT IF THIS IS NOT THE INITIAL CALL

  LGO = .true.
  PASS1 = .true.
  if (NEPIYR .le.0) go to 5

  !     *** PARTIAL EPIDEMIC IS IN EFFECT ***

  TAMID = (TAMIN + TAMAX)/2.
  NTRY = 1

  !     ** TURN OFF GRAPHICS AND REGULAR OUTPUT

  SAVGRF = MPBGRF
  MPBGRF = .false.
  LGO = .false.

  !     ** PRINT SURFACE KILLED IN PARTIAL EPIDEMIC

  write (JOMPB,6800) SADLPP

  5 continue

  !     ** COMPUTE EFFECT OF ELEVATION

  if (ELEV .le.0.) ELEV = 63
  EFELEV = 2.62 - 2.70E-2*ELEV

  !     -- NOTE: ELEVATION IS IN HUNDREDS OF FEET

  if (EFELEV .gt.1.) EFELEV = 1.
  if (EFELEV .lt.0.) EFELEV = 0.

  !     ** COMPUTE EFFECT OF LATITUDE

  EFLAT = 4.667 - 8.333E-2*FORLAT
  if (EFLAT .lt.0.) EFLAT = 0.
  if (EFLAT .gt.1.5) EFLAT = 1.5

  if (DEBUIN) write (JOMPB,7200) EFELEV, EFLAT

  !     WRITE INITIAL SWITCH VALUES (DEBUG)

  if ( DEBUIN ) write ( JOMPB,2 )  LAGG,LPS,LDC,LREP,MPBGRF
  2 format (//,'SWITCHES:   LAGG     LPS     LDC     ',&
       &'LREP    MPBGRF',/,T7,5L8)

  !     **** BRANCH BACK HERE TO RESTART PARTIAL EPIDEMIC ****

  8 continue
  MPBYR = 0
  BY = STRBUG
  P = STRP

  !     ** SET MORTALITY TABLE TO ZERO

  do I = 1,NACLAS
     TM(I) = 0.0
     10 continue
  end do
  do I = 1,NG
     FM1(I) = 0.
     15 continue
  end do

  !     ** SET UP GRAPHS

  if ( .not. MPBGRF ) go to 25
  call PTSYM(IPLTNO,IC1,IC2,IC22,IC3,IC4,IC5,IC6,IC7,ICBLK,ICBLK)
  call PTGRP ( IPLTNO, 2, 10, 0., 0.)
  call PTGRP ( IPLTNO, 1, 10, 0., 0.)
  call PTGRP ( IPLTNO, 1, 10, 0., 0.)
  call PTGRP ( IPLTNO, 2, 11, 0., 40.)
  if (NG .eq.1) call PTGRP ( IPLTNO, 1, 11, 0., 1.)
  if (NG .gt.1) call PTGRP ( IPLTNO, 2, 11, 0., 1.)

  !     ** INITIALIZE PLOT VARIABLES

  do IP = 1,10
     PX(IP) = 0.
     20 continue
  end do
  PX(3) = BY
  PX(7) = SEXRAT
  PX(8) = P
  AYEAR = IY(ICYC)
  call PTINT( IPLTNO, 0, AYEAR, PX)

  !      **  BRANCH IN HERE UPON RE-ENTRY OF EPIDEMIC IN PROGRESS  **

  25 continue
  NINC = INCRS+1
  LASTYR = MPBYR - 1
  if (LASTYR .lt.0.) LASTYR = 0.

  !     ** DISTRIBUTE TREES INTO RESISTANCE CLASSES,

  OS = 0.

  if (.not. PASS1) go to 29

  !     -- RESISTANCE CALCULATED ON FIRST PASS ONLY

  TA = TAFAC
  if (.not.LCRES) goto 26
  RESIST = PMSLP(PGR,PGRX,TAY,5)
  TA = RESIST
  if (RESIST .lt. TAMIN) TA = TAMIN
  if (RESIST .gt. TAMAX) TA = TAMAX
  26 continue
  write (JOMPB,6900) PGR, RESIST, TA

  29 continue
  do I = 1,NACLAS
     TREES(I) = CLASS (I,IMPROB)

     SEXDBH(I) = .918 - .0168*DIAM(I)
     EFPHLM(I) = 16.67*PHLOEM(I) - .667
     if (EFPHLM(I) .lt.0.) EFPHLM(I) = 0.
     OS = OS + SURF(I)*TREES(I)
     40 continue
  end do
  if (LGO) OS = OS+SADLPP

  if (DEBUIN) write (JOMPB,7000) (PHLOEM(I), I = 1,NACLAS)
  if (DEBUIN) write (JOMPB,7100) (EFPHLM(I), I = 1,NACLAS)
  if (DEBUIN) write (JOMPB,7250) (SEXDBH(I), I = 1,NACLAS)

  !     ** COMPUTE CONSTANT EXODUS RATE

  do IG = 1,NG
     EXODUS(IG) = (1. - EXP(-CE*DST(IG)*DST(IG)/(EXCON*SQFTPA)))
     50 continue
  end do

  !     ** WRITE INITIAL SURFACE AREAS & MORTALITY RATES

  if (.not. (LGO .or. DEBUIN)) go to 55
  if ( MPBYR .gt.0 ) write (JOMPB,4950)
  write (JOMPB,5000) (DIAM(I),I = 1,NACLAS)
  write(JOMPB,6000) (SURF(I),I = 1,NACLAS)
  write(JOMPB,6030) (TREES(I),I = 1,NACLAS)

  if ( .not. DEBUIN ) go to 55

  write(JOMPB,6100) (IG, EXODUS(IG), IG, FM1(IG), IG = 1,NG)
  if (NG .gt.1) write(JOMPB,6130)

  !     *****  BEGIN ANNUAL LOOP  *****

  55 continue
  ICALYR = IY(ICYC) + MPBYR - LASTYR
  MPBYR = MPBYR + 1
  TB1 = 0.0
  TE1 = 0.0
  do IG = 1,NG
     BOLD(IG) = 0.
     B3SUM(IG) = 0.
     ANEX(IG) = 0.
     ANFML1(IG) = 0.
     ANFML2(IG) = 0.
     ANFML3(IG) = 0.
     60 continue
  end do

  !     ZERO OUT ARRAY OF TREES KILLED THIS YEAR

  do I = 1,NACLAS
     TKYR(I) = 0.0
     65 continue
  end do

  !     **COMPUTE GENOTYPE RATIOS

  Q = 1. - P
  if (NG .ne.3) go to 70
  GENO(1) = P*P
  GENO(2) = 2.*P*Q
  GENO(3) = Q*Q
  go to 80
  70 continue

  GENO(1) = P
  GENO(2) = Q
  80 continue

  !      SIMULATE THE EFFECT OF AGGREGATION PHERMONE
  !      (TO SIMULATE IMMIGRATION, CODE AN ADDITIVE FUNCTION)

  if (LAGG) BY = BY + BY * AGGPH(MPBYR)

  !     ** SAVE TOTAL EMERGING BEETLES FOR PRINTOUT

  GTEB = BY
  do IG = 1,NG
     TEB(IG) = BY*GENO(IG)
     90 continue
  end do

  if ((MPBYR .eq.1.and. LGO) .or. DEBUIN) write(JOMPB,6140)
  if ( DEBUIN ) write ( JOMPB,6150 )

  !     ***  BEGIN EMERGENCE LOOP  ***

  do INC = 1,NINC
     INC1 = INC - 1

     !     ** COMPUTE SURFACE AREA OF LIVE TREES (E1)

     PSE1 = 0.0
     E1 = 0.0
     do I = 1,NACLAS
        E1 = E1 + SURF(I)*TREES(I)

        !     PREVENTITIVE SPRAY SURFACE (PSE1)

        if (LPS .and. DIAM(I) .ge. PSDL(MPBYR))&
             &PSE1 = PSE1 + SURF(I) * TREES(I) * PSPF(MPBYR)
        110 continue
     end do

     !     ** COMPUTE SURFACE AREA AND DENSITY OF AGGREGATING TREES (E3)

     PSE3 = 0.0
     E3 = 0.
     EF3 = 0.
     TAGG = 0.
     AMP(INC) = 0.
     if (INC .le.1)  go to 150
     do KK = IB,INC1

        !     ** COMPUTE AMPLIFIER

        AMP(KK) = AMP1 - AMP2*AD(KK)*(1. - SEXRAT)
        if (AMP(KK) .lt.0.) AMP(KK) = 0.

        do I = 1,NACLAS
           TAGG = TAGG + AGG(I,KK)
           E3 = E3 + SURF(I)*AGG(I,KK)
           EF3 = EF3 + SURF(I)*AGG(I,KK)*AMP(KK)

           !     PREVENTITIVE SPRAY SURFACE (PSE3)

           if (LPS .and. DIAM(I) .ge. PSDL(MPBYR))&
                &PSE3 = PSE3 + SURF(I) * AGG(I,KK) * PSPF(MPBYR)
           130 continue
        end do
        140 continue
     end do
     150 continue

     !     COMPUTE DENSITY OF LIVE TREES

     RHO1 = 0.
     do I = 1,NACLAS
        RHO1 = RHO1+TREES(I)
        151 continue
     end do

     !     ** COMPUTE AVERAGE AMPLITUDE

     AMPAVE = 0.
     if (E3.gt.0.) AMPAVE = EF3/E3

     !     COMPUTE PROPORTION OF E1 AND E3 SURFACE SPRAYED WITH
     !     PREVENTITIVE SPRAY

     if ( .not. LPS ) go to 152
     PSPE1 = 1.0
     PSPE3 = 1.0
     if (E1.gt.1.E-30) PSPE1 = PSE1/E1
     if (E3.gt.1.E-30) PSPE3 = PSE3/E3
     152 continue

     !     ** COMPUTE SURFACE OF DEAD & POST-AGGREGATING TREES (E2)

     E2 = OS - (E1 + E3) + SNOHST

     !     ** COMPUTE EFFECTIVE SURFACE AREA

     EFFS = E1 + E2 + EF3

     !     ** CALCULATE RHO1, RHO2, AND RHO3 IN TREES PER SQUARE FOOT

     RHO3 = TAGG/SQFTPA
     RHO2 = TPROB - TAGG - RHO1
     if (RHO2.le.0.0) RHO2 = 0.0
     RHO2 = RHO2 / SQFTPA
     RHO1 = RHO1 / SQFTPA

     !     ** EMERGENCE THIS INC

     BNEW = EMERG(BY,INC1,INCRS)

     !     ** GENOTYPE LOOP **

     B1INC = 0.
     B3INC = 0.
     do IG = 1,NG

        !     ** DIVIDE INTO GENOTYPES AND ADD IN LEFTOVERS FROM LAST INC

        B0(IG) = GENO(IG)*BNEW + BOLD(IG)

        !     ** COMPUTE EXODUS FROM STAND

        EXLOSS(IG) = B0(IG)*EXODUS(IG)

        !     SIMULATE THE EFFECT OF REPELLING PHERMONE

        if (LREP) EXLOSS(IG) = EXLOSS(IG) + EXLOSS(IG) * REPL(MPBYR)
        B0(IG) = B0(IG) - EXLOSS(IG)

        !     ** SPLIT OFF BEETLES

        B1(IG) = B0(IG)*E1/EFFS
        B2(IG) = B0(IG)*E2/EFFS
        B3(IG) = B0(IG)*EF3/EFFS

        !     ** COMPUTE FLIGHT MORTALITY ATES FOR BEETLES FLYING TO:
        !     1-LIVE, 2-DEAD & NON-HOST, AND 3-AGGREGATING SURFACE

        FM1(IG) = 0.0
        FMX = -CF1*2.*SQRT(RHO1)*DST(IG)*DST(IG)/DST(1)
        if (FMX.gt. -80.0) FM1(IG) = EXP(FMX)
        FM2(IG) = 0.0
        FMX = -CF2*2.*SQRT(RHO2)*DST(IG)*DST(IG)/DST(1)
        if (FMX.gt. -80.0) FM2(IG) = EXP(FMX)
        FM3(IG) = 0.0
        FMX = -CF3*2.*SQRT(RHO3)*DST(IG)*DST(IG)/DST(1)
        if (FMX.gt. -80.0) FM3(IG) = EXP(FMX)

        !     ** COMPUTE FLIGHT LOSSES

        FML1(IG) = B1(IG)*FM1(IG)
        FML2(IG) = B2(IG)*FM2(IG)
        FML3(IG) = B3(IG)*FM3(IG)

        !     ** COMPUTE SURVIVAL AFTER FLIGHT

        B1(IG) = B1(IG)- FML1(IG)
        B2(IG) = B2(IG)- FML2(IG)
        B3(IG) = B3(IG)- FML3(IG)

        if ( .not. LPS ) go to 155
        B1(IG) = B1(IG) * (1-PSPE1*PSPK(MPBYR))
        B3(IG) = B3(IG) * (1-PSPE3*PSPK(MPBYR))
        155 continue

        !     ** B1 & B2 BEETLES GET TO TRY AGAIN NEXT INC

        BOLD(IG) = B1(IG) + B2(IG)

        !     ** KEEP SUMS

        B1INC = B1INC + B1(IG)
        B3INC = B3INC + B3(IG)
        B3SUM(IG) = B3SUM(IG) + B3(IG)
        ANEX(IG) = ANEX(IG) + EXLOSS(IG)
        ANFML1(IG) = ANFML1(IG) + FML1(IG)
        ANFML2(IG) = ANFML2(IG) + FML2(IG)
        ANFML3(IG) = ANFML3(IG) + FML3(IG)
        160 continue
     end do

     !     ** COMPUTE PIONEER DENSITY

     PIODEN = B1INC/E1

     !     ** COMPUTE PROBABILITY OF AT LEAST 1 ATTACK/SQ FT

     XX = 1D0 - DEXP(-PIODEN)

     !     ** COMPUTE NO. OF AGGREGATORS

     do I = 1,NACLAS
        AGG(I,INC) = 0.
        DTA = TA
        DSMTA = SURF(I) - DTA + 1.
        if (XX .ne.0D0.and. DSMTA .gt.0D0 )&
             &AGG(I,INC) = TREES(I)*BETIN(DTA, DSMTA, XX)

        !        -- REMEMBER THAT BETIN REQUIRES DOUBLE PRECISION ARGUMENTS.

        !     ** REMOVE AGGREGATORS FROM TREE POOL **

        TREES(I) = TREES(I) - AGG(I,INC)
        180 continue
     end do

     !     ** COMPUTE ATTACK DENSITY

     AD(INC) = 0.0
     if (EF3.le.0.)  go to 200
     do KK = IB,INC1
        AD(KK) = AD(KK) + AMP(KK)*B3INC/EF3
        190 continue
     end do
     200 continue

     !     ** ACCUMULATE TOTALS

     do IG = 1,NG
        TB1 = TB1 + B1(IG)
        210 continue
     end do
     TE1 = TE1 + E1

     220 continue
  end do

  !     *****  END OF EMERGENCE LOOP  *****

  SS = 0.
  BATK = 0.
  BY = 0.
  BYT = 0.
  AYEAR = ICALYR

  !     ***  PRODUCTIVITY LOOP  ***

  do INC = 1,NINC

     !     ** COMPUTE NUMBER OF EGGS LAID PER SQUARE FOOT

     XEG = 0.0
     XXG = -.117*AD(INC)
     if (XXG.gt. -80.0) XEG = EXP(XXG)
     EGGS = 630.*(1. - XEG)

     do I = 1,NACLAS

        !     ** COMPUTE YOUNG SURVIVING TO EMERGENCE

        PSURV = 1. - EXP(-AD(INC)*.04328)
        if (AD(INC).ge.2.595) PSURV = PSURV*6.812*EXP(-1.191*SQRT(AD(INC)))

        YOUNG = EGGS*PSURV*EFELEV*EFLAT*EFPHLM(I)

        if (AD(INC) .ge. CRITAD) go to 230

        !     ** STRIP KILL -- PUT BACK IN LIVE TREE POOL  **

        TREES(I) = TREES(I) + AGG(I,INC)
        AGG(I,INC) = 0.
        go to 240
        230 continue

        !     **  COMPUTE TOTAL MORTALITY BY DBH CLASS

        TRKILL = AGG(I,INC)
        TKYR(I) = TKYR(I) + TRKILL
        TM(I) = TM(I) + TRKILL

        !     ** COMPUTE SURFACE KILLED AND NUMBER OF BEETLES ATTACKING

        SKILL = SURF(I)*TRKILL
        SS = SS + SKILL
        BATKI = SKILL*AD(INC)
        BATK = BATK + BATKI

        !     ** COMPUTE ANNUAL PRODUCTIVITY

        BYINC = YOUNG*SKILL*HS
        BYT = BYT + BYINC
        BY = BY + BYINC*SEXDBH(I)
        240 continue
        270 continue
     end do
     280 continue
  end do

  !     ** COMPUTE AVERAGE SEX RATIO FOR EMERGING BEETLES

  if (BYT .gt.0.) SEXRAT = BY/BYT

  !     SIMULATE THE EFFECT OF DIRECT CONTROL

  if (LDC) BY = BY - BY * DCPF(MPBYR) * DCPK(MPBYR)

  !     ** COMPUTE NEW VALUE OF P

  B3ALL = 0.
  do IG = 1,NG
     B3ALL = B3ALL + B3SUM(IG)
     290 continue
  end do
  if (B3ALL .eq.0.) go to 300
  if (NG .eq.3) P = (B3SUM(1) + .5*B3SUM(2))/B3ALL
  if (NG .ne.3) P = (B3SUM(1)/B3ALL)**2
  300 continue

  !     ** ACCUMULATE KILLED SURFACE

  SSCUM = SSCUM + SS

  !     ** COMPUTE AVERAGE ATTACK, EMERGENCE & PIONEER DENSITIES

  ADEN = 0.
  EDEN = 0.
  EARAT = 0.
  if (SS .le.0.) go to 310
  ADEN = BATK/SS
  EDEN = BYT/SS
  310 continue
  if (BATK .gt.0.) EARAT = BY/BATK
  APD = TB1/TE1

  !     ** COMPUTE ANNUAL GRAND TOTALS

  GEX = 0.
  GFML1 = 0.
  GFML2 = 0.
  GFML3 = 0.
  do IG = 1,NG
     GEX = GEX + ANEX(IG)
     GFML1 = GFML1 + ANFML1(IG)
     GFML2 = GFML2 + ANFML2(IG)
     GFML3 = GFML3 + ANFML3(IG)
     320 continue
  end do

  !     ** COMPUTE % LOSS & SURVIVAL

  do IG = 1,NG
     PCTTEB(IG) = PERCNT(TEB(IG), GTEB)
     PCTEX(IG) = PERCNT(ANEX(IG), TEB(IG))
     PCTFM1(IG) = PERCNT(ANFML1(IG), TEB(IG))
     PCTFM2(IG) = PERCNT(ANFML2(IG), TEB(IG))
     PCTFM3(IG) = PERCNT(ANFML3(IG), TEB(IG))
     PCTSV(IG) = PERCNT(B3SUM(IG), TEB(IG))
     330 continue
  end do
  PCTGEM = PERCNT(GTEB, GTEB)
  PCTGEX = PERCNT(GEX, GTEB)
  PCTGF1 = PERCNT(GFML1, GTEB)
  PCTGF2 = PERCNT(GFML2, GTEB)
  PCTGF3 = PERCNT(GFML3, GTEB)
  PCTGSV = PERCNT(B3ALL, GTEB)

  !     ** SAVE NEW VALUES FOR PLOT

  if (.not. MPBGRF) go to 333
  PX(1) = SS
  PX(2) = ACTSRF(MPBYR)
  PX(3) = BY
  PX(4) = APD
  PX(5) = ADEN
  PX(6) = EDEN
  PX(7) = SEXRAT
  PX(8) = P
  call PTINT( IPLTNO, NINC, AYEAR, PX)
  333 continue

  !     ** WRITE OUTPUT

  if (DEBUIN) write(JOMPB,6250)  ICALYR, (DIAM(I), I = 1,NACLAS)
  if (.not. (DEBUIN .or. LGO)) go to 375
  write(JOMPB,6300) ICALYR, (TREES(I), I = 1,NACLAS)
  write(JOMPB,6325) ICALYR, (TKYR(I),I = 1,NACLAS)
  write(JOMPB,6350) ICALYR, (TM(I),I = 1,NACLAS)
  write(JOMPB,6400)
  if (NG .le.1) go to 350
  do IG = 1,NG
     write(JOMPB,6450) IG, ICALYR, IG, TEB(IG), ANEX(IG), ANFML1(IG),&
          &ANFML2(IG), ANFML3(IG), B3SUM(IG)
     340 continue
  end do
  350 continue
  write(JOMPB,6500) ICALYR, GTEB, GEX, GFML1, GFML2, GFML3, B3ALL
  if (NG .le.1) go to 370
  do IG = 1,NG
     write(JOMPB,6550) IG, ICALYR, IG, PCTTEB(IG), PCTEX(IG),&
          &PCTFM1(IG), PCTFM2(IG), PCTFM3(IG), PCTSV(IG)
     360 continue
  end do
  370 continue
  write(JOMPB,6600) ICALYR, PCTGEM, PCTGEX, PCTGF1, PCTGF2, PCTGF3,&
       &PCTGSV
  write(JOMPB,6650) ICALYR,SS,SSCUM,BY,P,ICALYR,ADEN,EDEN,EARAT,APD

  375 continue
  if (.not. LGO) go to 420

  !     ** UPDATE STAND AND CALCULATE MPBTPAK (TMMPB) FOR EVENT MONITOR

  TMMPB = 0.0
  do I = 1,NACLAS
     CLASS(I,IMPROB) = TREES(I)
     TMMPB = TMMPB + TM(I)
     380 continue
  end do
  write(JOMPB,889) TMMPB
  889 format ('MPBTPAK=',F7.1)

  !     PASS VAULE OF TMMPB TO EVENT MONITOR

  call EVSET4 (4,TMMPB)

  !     ** END RUN IF .LT. 1 BEETLE PER ACRE
  !     *****  END OF ANNUAL LOOP  *****

  if ( BY .ge.1.0.and. MPBYR .lt. MPMXYR) go to 390
  MPBYR = 0
  SADLPP = 0.
  return

  390 continue
  if ( ICALYR .ne. IY(ICYC) + IFINT ) go to 55
  if ( ICYC .lt. NCYC ) go to 410
  write (JOMPB,6700)
  go to 55

  410 continue
  SADLPP = SSCUM
  WORKIN(ICYC+1) = .true.
  return

  !     **** PARTIAL EPIDEMIC CONTROL LOGIC ****

  420 continue
  if (BY .ge.1..and. MPBYR .lt. NEPIYR) go to 55
  !     -- LOOP FOR ANOTHER YEAR

  SAK(NTRY) = SSCUM
  TATRY(NTRY) = TA

  if (NTRY .ne.1) go to 430

  !     ** END OF FIRST CALIBRATION TRY

  TA = TAMIN
  if (TATRY(1) .lt. (TAMIN + TATOL)) TA = TAMID
  if (DEBUIN) write (JOMPB,7400) RESIST, TA
  PASS1 = .false.
  NTRY = 2
  go to 8

  !     ** END OF SECOND CALIBRATION TRY

  430 continue
  if (NTRY .ne.2) go to 440
  TA = TAMAX
  if (TATRY(1) .gt. (TAMAX - TATOL)) TA = TAMID
  if (DEBUIN) write (JOMPB,7400) RESIST, TA
  NTRY = 3
  go to 8

  !     ** END OF THIRD CALIBRATION TRY -- GO FOR ACTUAL SIMULATION

  440 continue
  TA = TAFIT(TATRY,SAK)
  if (DEBUIN) write (JOMPB,7400) RESIST, TA
  LGO = .true.
  MPBGRF = SAVGRF
  go to 8

  4950 format (//,40('*'),10X,'STAND HAS BEEN UPDATED',&
       &10X,40('*'),//)
  5000 format (   T9,'DIAMETER CLASSES',5(T2,'ID',T40,10F9.2,/))
  6000 format (//,T9,'SURFACE AREA/TREE',5(T2,'IS',T40,10F9.2,/))
  6030 format (//,T9,'TREES/ACRE',5(T2,'IT',T40,10F9.2,/))
  6100 format(//,'IE',T9,'EXODUS(',I1,') =',F8.5,&
       &';  FM1(',I1,') =',F8.5)
  6130 format(/,'%  **NOTE -- IN THE % LOSS TABLES BELOW, THE',&
       &' PROPORTION OF THE WHOLE POPULATION REPRESENTED',/,&
       &'%    BY EACH GENOTYPE IS GIVEN IN THE ''EMERGENCE''',&
       &' COLUMN.  ALL OTHER VALUES ARE % WITHIN A GENOTYPE.')
  6140 format (//,'   YEAR ')
  6150 format('         INC   PIONEER     EFFECTIVE   LIVE (E1)   ',&
       &'DEAD AND    AGG. (E3)  AMPLITUDE     # AGG.     RHO3',&
       &'      FM3''S:',/,&
       &16X,'DENSITY',2('     SURFACE'),'   NON-HOST SUR  SURFACE',/)
  6250 format (//,T3,I5,T10, 'DIAMETER CLASSES',5(T2,'DC',T40,&
       &10F9.2,/))
  6300 format (//,T3,I5,T10, 'SURVIVING TREES/ACRE',5(T2,'ST',T40,&
       &10F9.2,/))
  6325 format (//,T3,I5,T10, 'T/A KILLED THIS YEAR',5(T2,'TK',T40,&
       &10F9.2,/))
  6350 format ('0',T4,I5,T10, 'CUMULATIVE MORTALITY',5(T2,'CM',T40,&
       &10F9.2,/))
  6400 format(/,T39,'   EMERGENCE      EXODUS  MORTALITY1  MORTALITY2',&
       &'  MORTALITY3   SURVIVORS')
  6450 format('B',I1,I5,T9,'BEETLES -- GENOTYPE ',I1,T40,6F12.3)
  6500 format('TB',I5,T9,'TOTAL BEETLES',T40,6F12.3,/,'B')
  6550 format('%',I1,I5,T9,'% GENOTYPE(',I1,')',T40,6F12.3)
  6600 format('%T',I5,T9,'% TOTAL',T40,6F12.3,/,'%')
  6650 format('S ',I5,T9,'SURFACE KILLED =',F10.2,';',&
       &'  CUMULATIVE SURFACE KILLED =',F10.2,';',&
       &'  OFFSPRING =',F10.2,';',&
       &'  NEXT P =',F8.5,&
       &/,'D ',I5,T9,'ATTACK DENSITY =',F8.4,';',&
       &' EMERGENCE DENSITY =',F8.3,';',&
       &' EMERGENCE/ATTACK =',F8.3,';',&
       &' PIONEER DENSITY =',F12.8)
  6700 format (//,25('*'),'    NOTE:  EPIDEMIC IS EXTENDING',&
       &' PAST THE LAST TREE GROWTH CYCLE YEAR    ',&
       &25('*'))
  6800 format (//,'SADLPP = ',F10.2)
  6900 format(/,'PERIODIC GROWTH RATIO = ',F5.3,' RESIST = ',F5.2,&
       &'  THRESHOLD OF AGGREGATION = ',F5.2,/)
  7000 format (//,'PHLOEM = ',20F6.3)
  7100 format (//,'EFPHLM = ',20F6.3)
  7250 format (//,'SEXDBH = ',20F6.3)
  7200 format (//,'EFELEV = ',F4.2,'  EFLAT = ',F4.2)
  7400 format (/,100('*'),//,'RESIST = ',F5.2,'  TA = ',5F5.2)
end
