!======================================!
!   Compute large tree height growth   !
!======================================!
! IE $Id$
subroutine HTGF
  implicit none

  !  THIS SUBROUTINE COMPUTES THE PREDICTED PERIODIC HEIGHT INCREMENT FOR
  !  EACH CYCLE AND LOADS IT INTO THE ARRAY HTG. HEIGHT INCREMENT IS
  !  PREDICTED FROM SPECIES, HABITAT TYPE, HEIGHT, DBH, AND PREDICTED DBH
  !  INCREMENT.  THIS ROUTINE IS CALLED FROM **TREGRO** DURING REGULAR
  !  CYCLING.  ENTRY **HTCONS** IS CALLED FROM **RCON** TO LOAD SITE
  !  DEPENDENT CONSTANTS THAT NEED ONLY BE RESOLVED ONCE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CALCOM.f90'
  include 'ARRAYS.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'MULTCM.f90'
  include 'HTCAL.f90'

  !   MODEL COEFFICIENTS AND CONSTANTS:

  !    BIAS -- THE AVERAGE RESIDUAL.

  !   HTCON -- AN ARRAY CONTAINING HABITAT TYPE CONSTANTS FOR
  !            HEIGHT GROWTH MODEL (SUBSCRIPTED BY SPECIES)

  !  HDGCOF -- COEFFICIENT FOR DIAMETER GROWTH TERMS.

  !    HGLD -- AN ARRAY, SUBSCRIPTED BY SPECIES, OF THE
  !             COEFFICIENTS FOR THE DIAMETER TERM IN THE HEIGHT
  !             GROWTH MODEL.

  !   H2COF -- COEFFICIENT FOR HEIGHT SQUARED TERMS.
  !    IND2 -- ARRAY OF POINTERS TO SMALL TREES.

  !   SCALE -- TIME FACTOR DERIVED BY DIVIDING FIXED POINT CYCLE
  !            LENGTH BY GROWTH PERIOD LENGTH FOR DATA FROM
  !            WHICH MODELS WERE DEVELOPED.

  !  SPECIES ORDER:
  !  1=WP,  2=WL,  3=DF,  4=GF,  5=WH,  6=RC,  7=LP,  8=ES,  9=AF, 10=PP,
  ! 11=MH, 12=WB, 13=LM, 14=LL, 15=PI, 16=JU, 17=PY, 18=AS, 19=CO, 20=MM,
  ! 21=PB, 22=OH, 23=OS

  ! WB USES COEFFICIENTS FOR WL
  ! LM AND PY USE COEFFICIENTS FROM TT FOR LM
  ! LL USES COEFFICIENTS FOR AF
  ! AS, MM, PB, CO, AND OH USE COEFFICIENTS FROM UT FOR AS
  ! OS USES COEFFICIENTS FOR MH
  ! PI AND JU USE COEFFICIENTS FROM UT

  ! Variables
  logical :: DEBUG
  real    :: HGLD(MAXSP),HGHC(8),HGLDD(8),HGSC(MAXSP),HGH2(8)
  integer :: MAPHAB(30),I,ISPC,I1,I2,I3,ITFN,IICR,K,IXAGE,IHT
  real    :: HGLH,BIAS,SCALE,XHT,HTI,D,CON,COF1,COF2,COF3,COF4,COF5
  real    :: COF6,COF7,COF8,COF9,TEMD,Y1,Y2,FBY1,FBY2,Z,ZADJ,CLOSUR
  real    :: DIA,BRATIO,PSI,H,HTNEW,TEMHTG
  real    :: COFLM(9,3),COFAS(9,3)
  real    :: MISHGF

  data HGLD/-.04935,-.3899,-.4574,-.09775,-.1555,-.1219,&
       &-.2454,-.5720,-.1997,-.5657,-.1219,&
       &-.3899,0.0,-.1997,8*0.0,-.1219/

  data (COFLM(I,1),I = 1,9)/&
       &37.0,85.0,1.77836,-0.51147,1.88795,1.20654,0.57697,&
       &3.57635,0.90283/
  data (COFLM(I,2),I = 1,9)/&
       &45.0,100.0,1.66674,0.25626,1.45477,1.11251,0.67375,&
       &2.17942,0.88103/
  data (COFLM(I,3),I = 1,9)/&
       &45.0,90.0,1.64770,0.30546,1.35015,0.94823,0.70453,&
       &2.46480,1.00316/

  data (COFAS(I,1),I = 1,9)/&
       &30.0,85.0,2.00995,0.03288,1.81059,1.28612,0.72051,&
       &3.00551,1.01433/
  data (COFAS(I,2),I = 1,9)/&
       &30.0,85.0,2.00995,0.03288,1.81059,1.28612,0.72051,&
       &3.00551,1.01433/
  data (COFAS(I,3),I = 1,9)/&
       &35.0,85.0,1.80388,-0.07682,1.70032,1.29148,0.72343,&
       &2.91519,0.95244/

  data BIAS/ .4809 /, HGLH/ 0.23315 /

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'HTGF',4,ICYC)
  SCALE = FINT/YR
  ISMALL = 0

  !  GET THE HEIGHT GROWTH MULTIPLIERS.

  call MULTS (2,IY(ICYC),XHMULT)

  !   BEGIN SPECIES LOOP:

  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (I1.eq.0) go to 40
     I2 = ISCT(ISPC,2)
     XHT = 1.0
     XHT = XHMULT(ISPC)

     !   BEGIN TREE LOOP WITHIN SPECIES LOOP

     do I3 = I1,I2
        I = IND1(I3)
        HTG(I) = 0.
        if (PROB(I).le.0.0) then
           if (LTRIP) then
              ITFN = ITRN+2*I-1
              HTG(ITFN) = 0.
              HTG(ITFN+1) = 0.
           end if
           go to 30
        end if
        HTI = HT(I)
        D = DBH(I)

        !   HEIGHT GROWTH EQUATION, EVALUATED FOR EACH TREE EACH CYCLE

        ! ORIGINAL NI SECTION

        if (ISPC.le.12.or. ISPC.eq.14.or. ISPC.eq.23) then
           CON = HTCON(ISPC)+H2COF*HTI*HTI+HGLD(ISPC)*ALOG(D)+&
                &HGLH*ALOG(HTI)
           HTG(I) = EXP(CON+HDGCOF*ALOG(DG(I)))+BIAS
           if (HTG(I).lt.0.1)HTG(I) = 0.1

           !  SKIP THIS SECTION FOR PI AND JU

        else if (ISPC.eq.15.or. ISPC.eq.16) then
           go to 30

           !  EQUATIONS FROM UT AND TT VARIANTS

        else
           IICR = INT(real(ICR(I))/10.0 + 0.5)
           if (IICR .gt.9) IICR = 9
           go to(101,101,102,102,102,102,102,103,103),IICR
           101 K = 1
           go to 110
           102 K = 2
           go to 110
           103 K = 3
           110 continue
           if (ISPC.eq.13.or. ISPC.eq.17) then
              COF1 = COFLM(1,K)
              COF2 = COFLM(2,K)
              COF3 = COFLM(3,K)
              COF4 = COFLM(4,K)
              COF5 = COFLM(5,K)
              COF6 = COFLM(6,K)
              COF7 = COFLM(7,K)
              COF8 = COFLM(8,K)
              COF9 = COFLM(9,K)
           else
              COF1 = COFAS(1,K)
              COF2 = COFAS(2,K)
              COF3 = COFAS(3,K)
              COF4 = COFAS(4,K)
              COF5 = COFAS(5,K)
              COF6 = COFAS(6,K)
              COF7 = COFAS(7,K)
              COF8 = COFAS(8,K)
              COF9 = COFAS(9,K)
           end if

           !  CHECK IF HEIGHT OR DBH EXCEED PARAMETERS

           if (HTI.le.4.5) goto 180
           if ((0.1 + COF1) .le. D) go to 180
           if ((4.5 + COF2) .le. HTI) go to 180
           go to 490
           180 continue

           !    THE SBB IS UNDEFINED IF CERTAIN INPUT VALUES EXCEED PARAMETERS IN
           !    THE FITTED DISTRIBUTION.  IN INPUT VALUES ARE EXCESSIVE THE HEIGHT
           !    GROWTH IS TAKEN TO BE ZERO.

           HTG(I) = 0.1
           go to 60
           490 continue

           ! CALCULATE ALPHA FOR THE TREE USING SCHREUDER + HAFLEY

           TEMD = D
           if (TEMD .le.0.2)TEMD = 0.2
           Y1 = (TEMD - 0.1)/COF1
           Y2 = (HTI - 4.5)/COF2
           FBY1 = ALOG(Y1/(1.0 - Y1))
           FBY2 = ALOG(Y2/(1.0 - Y2))
           Z = ( COF4 + COF6*FBY2 - COF7*( COF3 +&
                &COF5*FBY1))*(1.0 - COF7**2)**(-0.5)

           ! THE HT DIA MODEL NEEDS MODIFICATION TO CORRECT KNOWN BIAS

           if (ISPC.ne.13.and. ISPC.ne.17) then
              ZADJ = .1 - .10273*Z + .00273*Z*Z
              if (ZADJ .lt.0.0)ZADJ = 0.0
              Z = Z+ZADJ
           end if

           ! YOUNG SMALL LODGEPOLE HTG ACCELLERATOR BASED ON TARGHEE HTG

           if (IAGE .eq.0.or. ICYC .gt.1)go to 184
           IXAGE = IAGE + IY(ICYC) -IY(1)
           if (IXAGE .lt.40..and. IXAGE .gt.10..and. D&
                &.lt.9.0) then
              if (Z .gt.2.0) go to 184
              ZADJ = .3564*DG(I)*FINT/YR
              CLOSUR = PCT(I)/100.0
              if (RELDEN .lt.100.0)CLOSUR = 1.0
              if (DEBUG)write(JOSTND,9650)ELEV,IXAGE,ZADJ,FINT,YR,&
                   &DG(I),CLOSUR
              9650 format(' ELEV',F6.1,'IXAGE',F5.0,'ZADJ',&
                   &F10.4,'FINT',F6.0,'YR',F6.0,'DG',F10.3,'CLOSUR',F10.1)
              ZADJ = ZADJ*CLOSUR

              ! ADJUSTMENT IS HIGHER FOR LONG CROWNED TREES

              if (IICR .eq.9.or. IICR .eq.8)ZADJ = ZADJ*1.1
              Z = Z + ZADJ
              if (Z .gt.2.0)Z = 2.0
           end if
           184 continue

           ! CALCULATE DIAMETER AFTER 10 YEARS

           DIA = D + DG(I)/BRATIO(ISPC,D,HTI)
           if ((0.1 + COF1) .gt. DIA) go to 185
           HTG(I) = 0.1
           go to 60
           185 continue

           !  CALCULATE HEIGHT AFTER 10 YEARS

           PSI = COF8*((DIA-0.1)/(0.1 + COF1 - DIA))**COF9&
                &* (EXP(Z*((1.0 - COF7**2  ))**0.5/COF6))

           H = ((PSI/(1.0 + PSI))* COF2) + 4.5

           if (.not. DEBUG)go to 191
           write(JOSTND,9631)D,DIA,HTI,DG(I),Z ,H
           9631 format(1X,'IN HTGF DIA=',F7.3,'DIA+10=',F7.3,'H=',F7.1,&
                &'DIA GR=',F8.3,'Z=',E15.8,'NEW H=',F8.1)
           191 continue

           !  CALCULATE HEIGHT GROWTH
           !   NEGATIVE HEIGHT GROWTH IS NOT ALLOWED

           if (H .lt. HT(I)) H = HT(I)
           HTG(I) = (H - HT(I))
        end if

        60 continue

        !    MULTIPLIED BY SCALE TO CHANGE FROM A YR. PERIOD TO FINT AND
        !    MULTIPLIED BY XHT TO APPLY USER SUPPLIED GROWTH MULTIPLIERS (HTGMULT).
        !    MULTIPLIED BY EXP(HTCON()) FOR SPECIES WHERE HCOR2 MULTIPLIER
        !    HAS NOT ALREADY BEEN ACCOUNTED FOR (READCORH).

        select case (ISPC)
        case(13,15:22)
           HTG(I) = HTG(I)*SCALE*XHT*EXP(HTCON(ISPC))
        case default
           HTG(I) = HTG(I)*SCALE*XHT
        end select

        !    APPLY DWARF MISTLETOE HEIGHT GROWTH IMPACT HERE,
        !    INSTEAD OF AT EACH FUNCTION IF SPECIAL CASES EXIST.

        HTG(I) = HTG(I)*MISHGF(I,ISPC)

        TEMHTG = HTG(I)

        if (DEBUG) then
           HTNEW = HT(I)+HTG(I)
           write (JOSTND,9000) HTG(I),CON,HTCON(ISPC),H2COF,D,&
                &WK1(I),HGLH,HTNEW,HDGCOF,I,ISPC
           9000 format(' 9000 HTGF, HTG=',F8.4,' CON=',F8.4,' HTCON=',F8.4,&
                &' H2COF=',F12.8,' D =',F8.4/' WK1=',F8.4,' HGLH=',F8.4,&
                &' HTNEW=',F8.4,' HDGCOF=',F8.4,' I=',I4,' ISPC=',I2)
        end if

        ! CHECK FOR SIZE CAP COMPLIANCE.

        if ((HT(I)+HTG(I)).gt.SIZCAP(ISPC,4)) then
           HTG(I) = SIZCAP(ISPC,4)-HT(I)
           if (HTG(I) .lt.0.1) HTG(I) = 0.1
        end if

        if (.not.LTRIP) go to 30
        ITFN = ITRN+2*I-1
        if (ISPC.eq.13.or. (ISPC.ge.17.and. ISPC.le.22)) then
           HTG(ITFN) = TEMHTG
        else
           HTG(ITFN) = EXP(CON+HDGCOF*ALOG(DG(ITFN)))+BIAS
           if (HTG(ITFN).lt.0.1)HTG(ITFN) = 0.1
           HTG(ITFN) = HTG(ITFN)*SCALE*XHT
        end if

        ! CHECK FOR SIZE CAP COMPLIANCE.

        if ((HT(ITFN)+HTG(ITFN)).gt.SIZCAP(ISPC,4)) then
           HTG(ITFN) = SIZCAP(ISPC,4)-HT(ITFN)
           if (HTG(ITFN) .lt.0.1) HTG(ITFN) = 0.1
        end if

        if (ISPC.eq.13.or. (ISPC.ge.17.and. ISPC.le.22)) then
           HTG(ITFN+1) = TEMHTG
        else
           HTG(ITFN+1) = EXP(CON+HDGCOF*ALOG(DG(ITFN+1)))+BIAS
           HTG(ITFN+1) = HTG(ITFN+1)*SCALE*XHT
           if (HTG(ITFN+1).lt.0.1)HTG(ITFN+1) = 0.1
        end if

        ! CHECK FOR SIZE CAP COMPLIANCE.

        if ((HT(ITFN+1)+HTG(ITFN+1)).gt.SIZCAP(ISPC,4)) then
           HTG(ITFN+1) = SIZCAP(ISPC,4)-HT(ITFN+1)
           if (HTG(ITFN+1) .lt.0.1) HTG(ITFN+1) = 0.1
        end if

        if (DEBUG) write(JOSTND,9001) HTG(ITFN),HTG(ITFN+1)
        9001 format( ' UPPER HTG =',F8.4,' LOWER HTG =',F8.4)

        !   END OF TREE LOOP

        30 continue
     end do

     !   END OF SPECIES LOOP

     40 continue
  end do
  return

  entry HTCONS

  !  ENTRY POINT FOR LOADING HEIGHT INCREMENT MODEL COEFFICIENTS THAT ARE
  !  SITE DEPENDENT AND REQUIRE ONE-TIME RESOLUTION.  HGHC CONTAINS
  !  HABITAT TYPE INTERCEPTS, HGLDD CONTAINS HABITAT DEPENDENT
  !  COEFFICIENTS FOR THE DIAMETER INCREMENT TERM, HGH2 CONTAINS HABITAT
  !  DEPENDENT COEFFICIENTS FOR THE HEIGHT-SQUARED TERM, AND HGHC CONTAINS
  !  SPECIES DEPENDENT INTERCEPTS.  HABITAT TYPE IS INDEXED BY ITYPE (SEE
  !  /PLOT/ COMMON AREA).

  data MAPHAB/&
       &1,1, 7*2, 3,3,4,5,6, 4*7, 4,4,1,4,4, 3*8, 4*1/
  data  HGHC /&
       &2.03035, 1.72222, 1.19728, 1.81759, 2.14781, 1.76998, 2.21104,&
       &1.74090/
  data  HGLDD /&
       &0.62144, 1.02372, 0.85493, 0.75756, 0.46238, 0.49643, 0.37042,&
       &0.34003/
  data  HGH2 /&
       &-13.358E-05, -3.809E-05, -3.715E-05, -2.607E-05, -5.200E-05,&
       &-1.605E-05, -3.631E-05, -4.460E-05/
  data  HGSC /&
       &-.5342, .1433, .1641,-.6458,-.6959,-.9941,-.6004, .2089,&
       &-.5478, .7316,-.9941, .1433, .0,   -.5478, 8*0.0,-.9941/

  !  ASSIGN HABITAT DEPENDENT COEFFICIENTS.

  IHT = MAPHAB(ITYPE)
  HGHCH = HGHC(IHT)
  H2COF = HGH2(IHT)
  HDGCOF = HGLDD(IHT)

  !  LOAD OVERALL INTERCEPT FOR EACH SPECIES.

  do ISPC = 1,MAXSP
     if (ISPC.le.12.or.ISPC.eq.14.or.ISPC.eq.23) then
        HTCON(ISPC) = HGHCH+HGSC(ISPC)
     else
        HTCON(ISPC) = 0.0
     end if
     if (LHCOR2.and. HCOR2(ISPC).gt.0.0) HTCON(ISPC) = &
          &HTCON(ISPC)+ALOG(HCOR2(ISPC))
     50 continue
  end do

  return
end
