!======================================!
!   Compute large tree height growth   !
!======================================!
! BM $Id$
subroutine HTGF
  implicit none

  !  THIS SUBROUTINE COMPUTES THE PREDICTED PERIODIC HEIGHT
  !  INCREMENT FOR EACH CYCLE AND LOADS IT INTO THE ARRAY HTG.
  !  HEIGHT INCREMENT IS PREDICTED FROM SPECIES, HABITAT TYPE,
  !  HEIGHT, DBH, AND PREDICTED DBH INCREMENT.  THIS ROUTINE
  !  IS CALLED FROM **TREGRO** DURING REGULAR CYCLING.  ENTRY
  !  **HTCONS** IS CALLED FROM **RCON** TO LOAD SITE DEPENDENT
  !  CONSTANTS THAT NEED ONLY BE RESOLVED ONCE.
  !  CALLS ***FINDAG

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'
  include 'HTCAL.f90'
  include 'MULTCM.f90'
  include 'OUTCOM.f90'
  include 'PDEN.f90'
  include 'PLOT.f90'

  ! Variables
  logical :: DEBUG

  integer :: I,I1,I2,I3,IICR,ISPC,ITFN,IXAGE,J,K,KEYCR,L

  real    :: AGMAX,AGP10,AZBIAS,BAL,BARK,BRATIO,BZBIAS,CLOSUR,CRA,CRB,CRC
  real    :: D1,D2,DIA,FBY1,FBY2,FCTRKX,FCTRM,FCTRRB,FCTRXB,H,HGMDCR
  real    :: HGMDRH,HGUESS,HHT,HTGMOD,HTI,HTMAX,HTMAX2,HTNEW,MISHGF,POTHTG
  real    :: PSI,RELHT,RELSI,RHK,RHX,RHXS,SCALE,SHI,SI,SINDX,SITAGE,SITHT
  real    :: SLO,TEMHTG,TEMPH,WTCR,WTRH,XHT,XI1,XI2,Y1,Y2,Z,ZADJ,ZBIAS
  real    :: ZTEST

  real    :: AASM(MAXSP),BASM(MAXSP),COF(9,5),COF1(9,3),COF6(9,5)
  real    :: RHB(MAXSP),RHM(MAXSP),RHR(MAXSP),RHYXS(MAXSP)

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

  data AASM/    1.5,    2.0,     0.4,   2.1,    0.0,&
       &0.,    1.5,     1.5,   1.5,    1.3,&
       &0.,     0.,      0.,    0.,     0.,&
       &0.,    1.3,      0./

  data BASM/  0.003, 0.0026, 0.0080,    0.0,    0.0,&
       &0.,    0.0,    0.0,    0.0,  0.002,&
       &0.,     0.,     0.,     0.,     0.,&
       &0.,  0.002,     0./

  !  COEFFICIENTS--CROWN RATIO (CR) BASED HT. GRTH. MODIFIER

  data CRA /100.0/, CRB /3.0/, CRC /-5.0/

  !  COEFFICIENTS--RELATIVE HEIGHT (RH) BASED HT. GRTH. MODIFIER

  data RHK /1.0/, RHXS /0.0/

  !  COEFFS BASED ON SPECIES SHADE TOLERANCE AS FOLLOWS:
  !                                   RHR  RHYXS    RHM    RHB
  !        VERY TOLERANT             20.0   0.20    1.1  -1.10
  !        TOLERANT                  16.0   0.15    1.1  -1.20
  !        INTERMEDIATE              15.0   0.10    1.1  -1.45
  !        INTOLERANT                13.0   0.05    1.1  -1.60
  !        VERY INTOLERANT           12.0   0.01    1.1  -1.60
  !  IN THE BM VARIANT THE SHADE TOLLERANCE WAS RESOLVED TO THE RANGE
  !  1 THROUGH 11, WITH 1 REPRESENTING THE MOST TOLERANT AND 11 THE
  !  LEAST SHADE TOLERANT AS FOLLOWS
  !  SEQ. NO.   CHAR. CODE    SHADE TOLERANCE     INDEX
  !      1      WP            INTM                 7
  !      2      WL            VINT                11
  !      3      DF            INTM                 6
  !      4      WF            TOLN                 3
  !      5      MH            VTOL                 1
  !      6
  !      7      LP            VINT                10
  !      8      ES            TOLN                 5
  !      9      RF            TOLN                 4
  !      10     PP            INTL                 9
  !      17     OS (USE PP)   INTL                 9

  data RHR    / 15.0,  12.0,  15.0,  20.0,  20.0,&
       &0.0,  12.0,  16.0,  16.0,  13.0,&
       &0.0,   0.0,  20.0,  16.0,   0.0,&
       &12.0,  13.0,  15.0/

  data RHYXS  / 0.10,  0.01,  0.10,  0.20,  0.20,&
       &0.0,  0.01,  0.15,  0.15,  0.05,&
       &0.0,   0.0,  0.20,  0.15,   0.0,&
       &0.01,  0.05,  0.10/

  data RHM    / 1.10,  1.10,  1.10,  1.10,  1.10,&
       &0.0,  1.10,  1.10,  1.10,  1.10,&
       &0.0,   0.0,  1.10,  1.10,   0.0,&
       &1.10,  1.10,  1.10/

  data RHB    /-1.45, -1.60, -1.45, -1.10, -1.10,&
       &0.0, -1.60, -1.20, -1.20, -1.60,&
       &0.0,   0.0, -1.10, -1.20,   0.0,&
       &-1.60, -1.60, -1.45/

  data AZBIAS /0.0/

  data BZBIAS /0.0/

  !  COF1 IS FROM TT/HTGF FOR WHITEBARK PINE AND ALSO
  !          FROM UT/HTGF FOR LIMBER PINE

  data COF1/&
       &37.0,85.0,1.77836,-0.51147,1.88795,1.20654,0.57697,&
       &3.57635,0.90283,&
       &45.0,100.0,1.66674,0.25626,1.45477,1.11251,0.67375,&
       &2.17942,0.88103,&
       &45.0,90.0,1.64770,0.30546,1.35015,0.94823,0.70453,&
       &2.46480,1.00316/

  !  COF6 IS FROM UT/HTGF FOR QUAKING ASPEN

  data COF6/&
       &30.0,85.0,2.00995,0.03288,1.81059,1.28612,0.72051,&
       &3.00551,1.01433,&
       &30.0,85.0,2.00995,0.03288,1.81059,1.28612,0.72051,&
       &3.00551,1.01433,&
       &35.0,85.0,1.80388,-0.07682,1.70032,1.29148,0.72343,&
       &2.91519,0.95244,&
       &18*0.0/

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'HTGF',4,ICYC)
  if (DEBUG) write(JOSTND,3) ICYC
  3 format(' ENTERING SUBROUTINE HTGF  CYCLE =',I5)
  if (DEBUG)write(JOSTND,*) 'IN HTGF AT BEGINNING,HTCON=',&
       &HTCON,'RMAI=',RMAI,'ELEV=',ELEV

  SCALE = FINT/YR

  !  GET THE HEIGHT GROWTH MULTIPLIERS.

  call MULTS (2,IY(ICYC),XHMULT)
  if (DEBUG)write(JOSTND,*)'HTGF- IY(ICYC),XHMULT= ',&
       &IY(ICYC), XHMULT

  !   BEGIN SPECIES LOOP.

  do ISPC = 1,MAXSP

     !  GET THE APPROPRIATE SITE INDEX RANGE VALUES FOR THIS SPECIES.

     SLO = 0.
     SHI = 999.
     call SITERANGE (3,ISPC,SLO,SHI)

     I1 = ISCT(ISPC,1)
     if (I1.eq.0) go to 40
     I2 = ISCT(ISPC,2)
     SINDX = SITEAR(ISPC)
     XHT = XHMULT(ISPC)

     !   BEGIN TREE LOOP WITHIN SPECIES LOOP

     do I3 = I1,I2
        I = IND1(I3)
        HTG(I) = 0.
        BAL = ((100.0-PCT(I))/100.)*BA
        H = HT(I)
        if (PROB(I).le.0.) go to 161
        HGUESS = 0.0
        AGP10 = 0.0

        SITAGE = 0.0
        SITHT = 0.0
        AGMAX = 0.0
        HTMAX = 0.0
        HTMAX2 = 0.0
        D1 = DBH(I)
        D2 = 0.0
        call FINDAG(I,ISPC,D1,D2,H,SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,DEBUG)

        if (H .ge. HTMAX) then
           SI = SITEAR(ISPC)
           if (SI .gt. SHI)SI = SHI
           if (SI .le. SLO)SI = SLO + 0.5
           RELSI = (SI -SLO)/(SHI - SLO)
           HTG(I) = AASM(ISPC) + BASM(ISPC) * RELSI
           if (HTG(I).lt.0.1)HTG(I) = 0.1
           HTG(I) = HTG(I)*XHT*SCALE*EXP(HTCON(ISPC))
           go to 161
        end if

        !  NORMAL HEIGHT INCREMENT CALCULATON BASED ON INCOMMING TREE AGE
        !  FIRST CHECK FOR MAX, ASMYPTOTIC HEIGHT

        if (SITAGE .gt. AGMAX) then
           POTHTG = 0.1

           ! THE FOLLOWING 'IF' BLOCK IS AN RJ FIX.  8-22-88

           if (ISPC .eq.10.or. ISPC .eq.17) then
              POTHTG = -1.31 + 0.05 * SINDX
              if (POTHTG .lt.0.1) POTHTG = 0.1
           end if
           go to 1320
        else
           AGP10 = SITAGE + 10.
        end if

        !  WJ FROM UT USES HEIGHT INCREMENT FROM **REGENT**

        if (ISPC.eq.6)go to 999

        !  CALL HTCALC FOR NORMAL CYCLING

        select case (ISPC)

        case(11,12,15)

           !  WHITEBARK PINE, LIMBER PINE, AND QUAKING ASPEN - JOHNSON'S SBB METHOD

           if (ISPC.eq.11.or. ISPC.eq.12) then
              do J = 1,3
                 do K = 1,9
                    COF(K,J) = COF1(K,J)
                 end do
              end do
           else
              do J = 1,5
                 do K = 1,9
                    COF(K,J) = COF6(K,J)
                 end do
              end do
           end if
           XI2 = 4.5
           XI1 = 0.1

           !  TRAP TO AVOID LOG(0) ERRORS WITH XI1 AND XI2; HTG WILL COME FROM
           !  REGENT FOR THESE SMALL OF TREES ANYWAY.  GED 05/07/09

           if (DBH(I) .le.0.1.or. HT(I).le.4.5)go to 180

           HTG(I) = 0.
           HTI = HT(I)
           BARK = BRATIO(ISPC,DBH(I),HTI)
           if (DEBUG)write(JOSTND,*)' HTI, BARK= ',HTI, BARK

           !    CHANGE THE CROWN RATIO TO AN INTEGER BETWEEN 0 AND 10

           IICR = INT(real(ICR(I))/10.0 + 0.5)

           !    PLACE THE CROWN RATIO INTO ONE OF THREE GROUPS

           if (IICR .gt.9) IICR = 9
           go to(101,101,102,102,102,102,102,103,103),IICR
           101 KEYCR = 1
           go to 110
           102 KEYCR = 2
           go to 110
           103 KEYCR = 3
           110 continue

           !  ROW IDENT FOR SBB COEF LOOKUP

           K = KEYCR

           !  CHECK IF HEIGHT OR DBH EXCEED PARAMETERS

           if (DEBUG)&
                &write(JOSTND,9101)(COF(L,K),L = 1,9)
           9101 format(' COFS= ',9F10.4)

           if (HTI.le.4.5) goto 180
           if ((XI1 + COF(1,K)) .le. DBH(I)) go to 180
           if ((XI2 + COF(2,K)) .le. HTI) go to 180

           if (DEBUG)write(JOSTND,*)' XI1, XI2, DBH, HTI,COF(1,K),',&
                &'COF(2,K)= ',XI1, XI2, DBH(I), HTI,COF(1,K),COF(2,K)

           go to 190
           180 continue

           !    THE SBB IS UNDEFINED IF CERTAIN INPUT VALUES EXCEED PARAMETERS IN
           !    THE FITTED DISTRIBUTION.  IN INPUT VALUES ARE EXCESSIVE THE HEIGHT
           !    GROWTH IS TAKEN TO BE 0.1 FOOT.

           HTG(I) = 0.1
           go to 25
           190 continue

           ! CALCULATE ALPHA FOR THE TREE USING SCHREUDER + HAFLEY

           Y1 = (DBH(I) - XI1)/COF(1,K)
           Y2 = (HT(I) - XI2)/COF(2,K)
           if (DEBUG)write(JOSTND,*)' K,COF,DBH,XI1= ',K,COF(1,K),DBH(I),XI1
           if (DEBUG)write(JOSTND,*)' Y1,Y2,HT,XI2= ',Y1,Y2,HT(I),XI2

           FBY1 = ALOG(Y1/(1.0 - Y1))
           FBY2 = ALOG(Y2/(1.0 - Y2))
           Z = ( COF(4,K) + COF(6,K)*FBY2 - COF(7,K)*( COF(3,K) +&
                &COF(5,K)*FBY1))*(1.0 - COF(7,K)**2)**(-0.5)
           if (DEBUG)write(JOSTND,*)' K,COF(1,K)= ',K,COF(1,K)
           if (DEBUG)write(JOSTND,*)' K,COF(2,K)= ',K,COF(2,K)

           ! THE HT DIA MODEL NEEDS MODIFICATION TO CORRECT KNOWN BIAS
           ! THE COEFFS FOR AZBIAS AND BZBIAS ARE ZERO FOR THESE SPECIES

           ZBIAS = AZBIAS+BZBIAS*ELEV
           if (ISPC.eq.15)ZBIAS = AZBIAS+BZBIAS*(ELEV-20.)
           if (ELEV .lt.55..or. ELEV .gt.80.0)ZBIAS = 0.0
           ZTEST = Z-ZBIAS
           if (ZTEST .ge.2.0.and. ZBIAS .lt.0.0)ZBIAS = 0.0
           Z = Z-ZBIAS
           if (ISPC.eq.12.or. ISPC.eq.15) then
              ZADJ = .1 - .10273*Z + .00273*Z*Z
              if (ZADJ .lt.0.0)ZADJ = 0.0
              Z = Z+ZADJ
           end if
           if (DEBUG)write(JOSTND,*)' I,Z,K,Y1,Y2,DBH(I),FBY1,FBY2= '
           if (DEBUG)write(JOSTND,*)I,Z,K,Y1,Y2,DBH(I),FBY1,FBY2

           ! YOUNG SMALL LODGEPOLE HTG ACCELLERATOR BASED ON TARGHEE HTG
           ! TEMP BYPASS

           if ((ICYC .gt.1) .or. (IAGE .le.0))go to 184
           IXAGE = IAGE + IY(ICYC) -IY(1)

           if (DEBUG)&
                &write(JOSTND,*)' I, ISPC, IXAGE= ',I, ISPC, IXAGE

           if (IXAGE .lt.40..and. IXAGE .gt.10..and. DBH(I)&
                &.lt.9.0) then
              if (Z .gt.2.0) go to 184
              ZADJ = .3564*DG(I)*FINT/YR
              CLOSUR = PCT(I)/100.0
              if (RELDEN .lt.100.0)CLOSUR = 1.0
              if (DEBUG)write(JOSTND,9650)ZBIAS,ELEV,IXAGE,ZADJ,FINT,YR,&
                   &DG(I),CLOSUR
              9650 format(' ZBIAS',F8.0,'ELEV',F6.1,'AGE',F5.0,'ZADJ',&
                   &F10.4,'FINT',F6.0,'YR',F6.0,'DG',F10.3,'CLOSUR',F10.1)
              ZADJ = ZADJ*CLOSUR

              ! ADJUSTMENT IS HIGHER FOR LONG CROWNED TREES

              if (IICR .eq.9.or. IICR .eq.8)ZADJ = ZADJ*1.1
              Z = Z + ZADJ
              if (Z .gt.2.0)Z = 2.0
           end if
           184 continue

           ! CALCULATE DIAMETER AFTER 10 YEARS

           DIA = DBH(I) + DG(I)/BARK
           if ((XI1 + COF(1,K)) .gt. DIA) go to 185
           HTG(I) = 0.1
           go to 25
           185 continue

           !  CALCULATE HEIGHT AFTER 10 YEARS

           PSI = COF(8,K)*((DIA-XI1)/(XI1 + COF(1,K) - DIA))**COF(9,K)&
                &* (EXP(Z*((1.0 - COF(7,K)**2  ))**0.5/COF(6,K)))

           HHT = ((PSI/(1.0 + PSI))* COF(2,K)) + XI2

           if (.not. DEBUG)go to 191
           write(JOSTND,9631)DBH(I),DIA,HTI,DG(I),Z ,HHT
           9631 format(1X,'IN HTCALC DIA=',F7.3,'DIA+10=',F7.3,'HTI',F7.1,&
                &'DIA GR=',F8.3,'Z=',E15.8,'NEW H=',F8.1)
           191 continue

           !  CALCULATE HEIGHT GROWTH
           !   NEGATIVE HEIGHT GROWTH IS NOT ALLOWED

           if (HHT .lt. HTI) HHT = HTI
           HTG(I) = HHT - HTI
           if (HTG(I).lt.0.1)HTG(I) = 0.1

           if (DEBUG)write(JOSTND,*)' I,HHT,HTI,HTG(I)= ',&
                &I,HHT,HTI,HTG(I)
           25 continue

        case default

           !  CALL HTCALC HERE TO CALCULATE POTENTIAL HT GROWTH FOR ALL
           !  SPECIES EXCEPT 6, 11, 12, AND 15

           if (DEBUG)write(JOSTND,*)' ISPC,I,HT,AGP10= ',ISPC,I,HT(I),AGP10

           call HTCALC (SINDX,ISPC,AGP10,HGUESS,JOSTND,DEBUG)
           POTHTG = HGUESS - SITHT

           !  HEIGHT GROWTH MUST BE POSITIVE

           if (POTHTG .lt.0.1)POTHTG = 0.1

        end select

        !  WB(11), LM(12) AND AS(15) DO NOT GET MODIFIED HERE

        if (ISPC.eq.11.or. ISPC.eq.12.or. ISPC.eq.15) go to 999

        !  HEIGHT GROWTH MODIFIERS

        1320 continue
        if (DEBUG)write(JOSTND,*) ' AT 1320 CONTINUE FOR TREE',I,' HT= ',&
             &HT(I),' AVH= ',AVH
        RELHT = 0.
        if (AVH .gt.0.) RELHT = HT(I)/AVH
        if (RELHT .gt.1.5)RELHT = 1.5

        !     REVISED HEIGHT GROWTH MODIFIER APPROACH.

        !     CROWN RATIO CONTRIBUTION.  DATA AND READINGS INDICATE HEIGHT
        !     GROWTH PEAKS IN MID-RANGE OF CR, DECREASES SOMEWHAT FOR LARGE
        !     CROWN RATIOS DUE TO PHOTOSYNTHETIC ENERGY PUT INTO CROWN SUPPORT
        !     RATHER THAN HT. GROWTH.  CROWN RATIO FOR THIS COMPUTATION NEEDS
        !     TO BE IN (0-1) RANGE; DIVIDE BY 100.  FUNCTION IS HOERL'S
        !     SPECIAL FUNCTION (REF. P.23, CUTHBERT&WOOD, FITTING EQNS. TO DATA
        !     WILEY, 1971).  FUNCTION OUTPUT CONSTRAINED TO BE 1.0 OR LESS.

        HGMDCR = (CRA * (ICR(I)/100.)**CRB) * EXP(CRC*(ICR(I)/100.))
        if (HGMDCR .gt.1.) HGMDCR = 1.

        !     RELATIVE HEIGHT CONTRIBUTION.  DATA AND READINGS INDICATE HEIGHT
        !     GROWTH IS ENHANCED BY STRONG TOP LIGHT AND HINDERED BY HIGH
        !     SHADE EVEN IF SOME LIGHT FILTERS THROUGH.  ALSO RESPONSE IS
        !     GREATER FOR GIVEN LIGHT AS SHADE TOLERANCE INCREASES.  FUNCTION
        !     IS GENERALIZED CHAPMAN-RICHARDS (REF. P.2 DONNELLY ET AL. 1992.
        !     THINNING EVEN-AGED FOREST STANDS...OPTIMAL CONTROL ANALYSES.
        !     USDA FOR. SERV. RES. PAPER RM-307).
        !     PARTS OF THE GENERALIZED CHAPMAN-RICHARDS FUNCTION USED TO
        !     COMPUTE HGMDRH BELOW ARE SEGMENTED INTO FACTORS
        !     FOR PROGRAMMING CONVENIENCE.

        RHX = RELHT
        FCTRKX = ( (RHK/RHYXS(ISPC))**(RHM(ISPC)-1.) ) - 1.
        FCTRRB = -1.0*( RHR(ISPC)/(1.0-RHB(ISPC)) )
        FCTRXB = RHX**(1.0-RHB(ISPC)) - RHXS**(1.0-RHB(ISPC))
        FCTRM = -1.0/(RHM(ISPC)-1.)
        if (DEBUG)&
             &write(JOSTND,*) ' HTGF-HGMDRH FACTORS = ',&
             &ISPC, RHX, FCTRKX, FCTRRB, FCTRXB, FCTRM
        HGMDRH = RHK * ( 1.0 + FCTRKX*EXP(FCTRRB*FCTRXB) ) ** FCTRM

        !     APPLY WEIGHTED MODIFIER VALUES.

        WTCR = .25
        WTRH = 1.0 - WTCR
        HTGMOD = WTCR*HGMDCR + WTRH*HGMDRH

        !    MULTIPLIED BY SCALE TO CHANGE FROM A YR. PERIOD TO FINT AND
        !    MULTIPLIED BY XHT TO APPLY USER SUPPLIED GROWTH MULTIPLIERS.

        if (DEBUG) then
           write(JOSTND,*)' IN HTGF, I= ',I,' ISPC= ',ISPC,'HTGMOD= ',&
                &HTGMOD,' ICR= ',ICR(I),' HGMDCR= ',HGMDCR
           write(JOSTND,*)' HT(I)= ',HT(I),' AVH= ',AVH,' RELHT= ',RELHT,&
                &' HGMDRH= ',HGMDRH
        end if

        if (HTGMOD .ge.2.) HTGMOD = 2.
        if (HTGMOD .le.0.1) HTGMOD = 0.1

        HTG(I) = POTHTG * HTGMOD

        if (DEBUG)write(JOSTND,901)ICR(I),PCT(I),BA,DG(I),HT(I),POTHTG,&
             &BAL,AVH,HTG(I),PCCF(ITRE(I)),ABIRTH(I),HGUESS,HTGMOD
        901 format(' HTGF',I5,13F9.2)

        999 continue

        !   HEIGHT GROWTH EQUATION, EVALUATED FOR EACH TREE EACH CYCLE
        !   MULTIPLIED BY SCALE TO CHANGE FROM A YR. PERIOD TO FINT AND
        !   MULTIPLIED BY XHT TO APPLY USER SUPPLIED GROWTH MULTIPLIERS.

        !   CHECK FOR HT GT MAX HT FOR THE SITE AND SPECIES

        TEMPH = H + HTG(I)
        if (TEMPH .gt. HTMAX) then
           HTG(I) = HTMAX - H
        end if
        if (HTG(I).lt.0.1)HTG(I) = 0.1
        HTG(I) = SCALE*XHT*HTG(I)*EXP(HTCON(ISPC))

        if (DEBUG) then
           HTNEW = HT(I)+HTG(I)
           write (JOSTND,9000) HTG(I),HTCON(ISPC),H2COF,&
                &WK1(I),HTNEW,I,ISPC
           9000 format(' 9000 HTGF, HTG=',F8.4,' HTCON=',F8.4,&
                &' H2COF=',F12.8,' WK1=',F8.4/,&
                &' HTNEW=',F8.4,' I=',I4,' ISPC=',I2)
        end if

        161 continue

        !    APPLY DWARF MISTLETOE HEIGHT GROWTH IMPACT HERE,
        !    INSTEAD OF AT EACH FUNCTION IF SPECIAL CASES EXIST.

        HTG(I) = HTG(I)*MISHGF(I,ISPC)
        TEMHTG = HTG(I)

        ! CHECK FOR SIZE CAP COMPLIANCE.

        if ((HT(I)+HTG(I)).gt.SIZCAP(ISPC,4)) then
           HTG(I) = SIZCAP(ISPC,4)-HT(I)
           if (HTG(I) .lt.0.1) HTG(I) = 0.1
        end if

        if (.not.LTRIP) go to 30
        ITFN = ITRN+2*I-1
        HTG(ITFN) = TEMHTG

        ! CHECK FOR SIZE CAP COMPLIANCE.

        if ((HT(ITFN)+HTG(ITFN)).gt.SIZCAP(ISPC,4)) then
           HTG(ITFN) = SIZCAP(ISPC,4)-HT(ITFN)
           if (HTG(ITFN) .lt.0.1) HTG(ITFN) = 0.1
        end if

        HTG(ITFN+1) = TEMHTG

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
  if (DEBUG)write(JOSTND,60)ICYC
  60 format(' LEAVING SUBROUTINE HTGF   CYCLE =',I5)
  return

  entry HTCONS

  !  ENTRY POINT FOR LOADING HEIGHT INCREMENT MODEL COEFFICIENTS THAT
  !  ARE SITE DEPENDENT AND REQUIRE ONE-TIME RESOLUTION.  HGHC
  !  CONTAINS HABITAT TYPE INTERCEPTS, HGLDD CONTAINS HABITAT
  !  DEPENDENT COEFFICIENTS FOR THE DIAMETER INCREMENT TERM, HGH2
  !  CONTAINS HABITAT DEPENDENT COEFFICIENTS FOR THE HEIGHT-SQUARED
  !  TERM, AND HGHC CONTAINS SPECIES DEPENDENT INTERCEPTS.  HABITAT
  !  TYPE IS INDEXED BY ITYPE (SEE /PLOT/ COMMON AREA).

  !  LOAD OVERALL INTERCEPT FOR EACH SPECIES.

  !  LOAD OVERALL INTERCEPT FOR EACH SPECIES.

  do ISPC = 1,MAXSP
     HTCON(ISPC) = 0.0
     if (LHCOR2.and. HCOR2(ISPC).gt.0.0) HTCON(ISPC) = &
          &HTCON(ISPC)+ALOG(HCOR2(ISPC))
     50 continue
  end do

  return
end
