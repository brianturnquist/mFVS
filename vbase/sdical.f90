!====================!
!   vbase : sdical   !
!====================!
! VBASE $Id$
subroutine SDICAL (IWHO,XMAX)
  implicit none

  !  THIS SUBROUTINE COMPUTES THE MAXIMUM SDI IN EFFECT FOR A STAND.
  !  MAXIMUM SDI IS A WEIGHTED AVERAGE OF THE SDI MAXIMUMS BY SPECIES
  !  CONTAINED IN THE SDIDEF() ARRAY.  THE WEIGHT IS THE PROPORTION
  !  OF TOTAL STAND BASAL AREA REPRESENTED BY A SPECIES.

  !  IT ALSO ADJUSTS THE BAMAX VALUE TO MATCH THE MAXIMUM SDI VALUE IF
  !  A USER DEFINED BA MAXIMUM IS NOT IN EFFECT.

  !  THIS ROUTINE IS CALLED FROM:
  !    **DGF** IN THE CR, EM, IE, TT, UT, AND WS VARIANTS.
  !    **DISPLY**
  !    **GRINCR** TO SET VALUES FOR THE EVENT MONITOR VARIABLES "BSDIMAX"
  !               AND "ASDIMAX".
  !    **MORTS** IN VARIANTS WHICH USE SDI BASED MORTALITY ALGORITHMS.
  !    **SDICHK**
  !    **CLAUESTB** (CLIMATE EXTENSION)

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'VARCOM.f90'

  !  DEFINITIONS:
  !    BAXSP -- BASAL AREA BY SPECIES
  !   BAXPSP -- BASAL AREA BY SPECIES BY POINT (SPECIES, POINT)
  !      DBH -- TREE DIAMETER.
  !     IWHO -- 1 IF RECENT DEAD TREES NEED TO BE USED IN THE CALCULATION;
  !             SUBROUTINE **DGF**, CR, TT & UT VARIANTS, DURING CALIBRATION
  !             TO GET THE STAGNATION MULTIPLIER SET CORRECTLY.
  !             2 IF RECENT DEAD TREES SHOULD NOT BE USED IN THE CALCULATION
  !    LINCL -- USED TO INDICATE WHETHER A TREE GETS INCLUDED IN
  !             THE CALCULATION OR NOT.
  !     PROB -- NUMBER OF TREES PER ACRE REPRESENTED BY A TREE.
  !   TREEBA -- BA REPRESENTED BY A TREE RECORD
  !    TOTBA -- TOTAL STAND BA
  !     XMAX -- MAXIMUM SDI WEIGHTED BY BA PROPORTION FOR THE STAND
  !   XMAXPT -- MAXIMUM SDI WEIGHTED BY BA PROPORTION FOR EACH POINT
  !    MAPNE -- INDICATES GROUP THAT A SPECIES BELONGS TO FOR THE
  !             SILVAH-TYPE DENSITY CALCULATION

  ! Variables
  integer :: ISPC,JSPEC,I,IWHO,IGRP,IULIM,IG,JPNUM,MAPNE(108),IEQN,IT
  integer :: II,ISLFTM(108)
  integer :: NPNT
  real    :: BAXSP(MAXSP),XMAX,TOTBA,TREEBA,DLO,DHI,SDIC,SPROB,TREERD
  real    :: BAXPSP(MAXSP,MAXPLT),PNTBA
  real    :: A,B,CCCL,CRA,CWDI,SDSQ,TPACRE,SDIC2
  real    :: CRD,D2,TEM,DPROB
  real    :: BA1,BA2,BA3,BA4,BA5,BATOT,RATIO1,RATIO2,RATIO3,RATIO5
  real    :: CMPNT1,CMPNT2,CMPNT3,CMPNT4,CMPNT5,CMPNT6,TPAFAC,DIAMFAC,CLSD2,CLSTPA
  logical :: DEBUG,LINCL

  data MAPNE /&
       &2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,&
       &2, 2, 2, 2, 2, 2, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3,&
       &2, 1, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 1, 3, 3, 3, 3, 3, 3,&
       &3, 3, 3, 3, 3, 3, 2, 2, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,&
       &2, 2, 2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3,&
       &2, 2, 2, 2, 2, 2, 2, 2/

  data ISLFTM /&
       !     1  2  3  4  5  6  7  8  9  0  1  2  3  4  5  6  7  8  9  0

       &0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,&
       &0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 2, 2, 2, 2, 2, 1,&
       &0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 2, 2, 2, 2, 2, 2,&
       &2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,&
       &0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,&
       &0, 0, 0, 0, 0, 0, 0, 0/

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'SDICAL',6,ICYC)
  if (DEBUG)write(JOSTND,10)ICYC, ITRN, IREC1, IREC2
  10 format(' ENTERING SUBROUTINE SDICAL CYCLE =',I4,&
       &' ITRN=',I4,' IREC1=',I4,' IREC2=',I4)

  ! DETERMINE BA BY SPECIES, AND TOTAL STAND BA
  ! IF THIS PASS THROUGH **SDICAL** IS DURING CALIBRATION (LSTART = .TRUE.)
  ! INCLUDE RECENT MORTALITY TREES (IMC(I)=7) IN THE CALCULATION.

  XMAX = 0.0
  TOTBA = 0.0
  do I = 1,MAXSP
     BAXSP(I) = 0.0
     20 continue
  end do

  !     SET NUMBER OF INVENTORY POINTS IN DATA AND INITIALIZE ARRAY
  !     TO STORE BA BY SPECIES AND POINT

  NPNT = INT(PI)
  do I = 1,MAXSP
     do II = 1,(NPNT+10) ! initialize for up to 10 unregistered plots
        BAXPSP(I,II) = 0.0
     end do
  end do

  if (ITRN .le.0) go to 55
  do II = 1,ITRN
     I = IND1(II)
     ISPC = ISP(I)
     if (I.ge.IREC2) go to 30
     TREEBA = 0.0054542*DBH(I)*DBH(I)*PROB(I)
     BAXSP(ISPC) = BAXSP(ISPC) + TREEBA
     TOTBA = TOTBA + TREEBA
     BAXPSP(ISPC,ITRE(I)) = BAXPSP(ISPC,ITRE(I)) + TREEBA
     if (DEBUG)write(JOSTND,*)' SDICAL II,I,ISPC,DBH,PROB,TREEBA,',&
          &'BAXSP,TOTBA= ',II,I,ISPC,DBH(I),PROB(I),TREEBA,BAXSP(ISPC),&
          &TOTBA
     30 continue
  end do

  !  CR, TT, AND UT VARIANTS: DGF DURING CALIBRATION, INCLUDE RECENT
  !  MORTALITY TREES

  if (DEBUG)write(JOSTND,*)' IWHO,LSTART,IREC2= ',IWHO,LSTART,IREC2
  if ((IWHO .eq.1) .and. LSTART) then
     do II = IREC2,MAXTRE
        ISPC = ISP(II)
        DPROB = PROB(II)/(FINT/FINTM)
        if (DEBUG)write(JOSTND,*)' II,ISPC,IMC,DBH,DPROB= ',&
             &II,ISPC,IMC(II),DBH(II),DPROB
        if (IMC(II) .eq.7) then
           TREEBA = 0.0054542*DBH(II)*DBH(II)*DPROB
           BAXSP(ISPC) = BAXSP(ISPC) + TREEBA
           TOTBA = TOTBA + TREEBA
           if (DEBUG)write(JOSTND,*)' SDICAL II,ISPC,DBH,DPROB,TREEBA,',&
                &'BAXSP,TOTBA= ',II,ISPC,DBH(II),DPROB,TREEBA,BAXSP(ISPC),&
                &TOTBA
        end if
     end do
  end if

  !  DEBUG OUTPUT

  if (DEBUG) then
     write(JOSTND,40)BAXSP
     40 format(' BAXSP(1-MAXSP) =',10(11F10.4,/' ',16X))
     write(JOSTND,50)TOTBA
     50 format(' TOTAL BA = ',F10.4)
  end if

  ! COMPUTE MAXIMUM SDI WEIGHTED BY SPECIES BA PROPORTION FOR THE STAND
  ! LACK OF STAND BA WILL RESULT IN MAXIMUM SDI (XMAX) GETTING SET TO 1.0
  ! WHICH WILL THEN BE USED TO SET THE POINT MAX SDI (XMAXPT) ARRAY.

  55 if (TOTBA .le.0.) then
     XMAX = 1.0
  else
     do I = 1,MAXSP
        XMAX = XMAX + SDIDEF(I) * BAXSP(I)
        60 continue
     end do
     XMAX = XMAX / TOTBA
  end if

  ! COMPUTE MAXIMUM SDI WEIGHTED BY SPECIES BA PROPORTION FOR EACH POINT

  !     COMPUTE THESE FOR UP TO 10 ADDITIONAL UNREGISTERED PLOTS, THAT IS
  !     WHERE NUMBER OF PLOTS SPECIFIED ON THE SAMPLE DESIGN IS LESS THAN
  !     THE NUMBER OF PLOT ID'S FOUND IN THE TREE RECORDS.

  do II = 1,(NPNT+10)
     !      DO 65 II=1,NPNT
     XMAXPT(II) = 0.0
     PNTBA = 0.0
     do I = 1,MAXSP
        XMAXPT(II) = XMAXPT(II) + SDIDEF(I) * BAXPSP(I,II)
        PNTBA = PNTBA + BAXPSP(I,II)
        64 continue
     end do
     if (PNTBA .eq.0.0) then
        XMAXPT(II) = XMAX
     else
        XMAXPT(II) = XMAXPT(II) / PNTBA
     end if
     if (DEBUG)write(JOSTND,*)' SDICAL - NPNT, II, PNTBA, XMAXPT =',&
          &NPNT,II,PNTBA,XMAXPT(II)
     65 continue
  end do

  ! ADJUST MAXIMUM BA IF A USER-DEFINED VALUE IS NOT IN EFFECT.

  if (.not.LBAMAX) then
     BAMAX = XMAX * 0.5454154 * PMSDIU
  else
     TEM = PMSDIU
     if (TEM .gt.1.0) TEM = TEM/100.
     XMAX = BAMAX/(0.5454154*TEM)
  end if

  if (DEBUG)write(JOSTND,*)' CALLING CLMAXDEN,',&
       &' CURRENT SDI MAXIMUM = ',XMAX

  !     FURTHER MODIFY XMAX FOR CLIMATE-CHANGE

  call CLMAXDEN (SDIDEF,XMAX)
  if (DEBUG)write(JOSTND,*)' LEAVING SDICAL, SDI MAXIMUM = ',XMAX

  return

  entry SDICLS (JSPEC,DLO,DHI,IWHO,SDIC,SDIC2,A,B,JPNUM)

  !  THIS ENTRY COMPUTES THE SDI FOR A GIVEN CLASS OF TREES
  !  DEFINED BY SPEICES AND DBH RANGE, STAGES
  !  SUMMATION METHOD (1968 RES. NOTE INT-77)
  !  ZEIDE METHOD (ZEIDE, B., CAN. J. FOR. RES. VOL. 13, 1983;
  !                SHAW, J.D., WAJF 15(1) 2000)

  !  JSPEC = SPECIES INDEX (0 FOR ALL SPECIES)
  !    DLO = LOWER DBH LIMIT (GREATER THAN OR EQUAL TO)
  !    DHI = UPPER DBH LIMIT (LESS THAN)
  !   IWHO = 1 IF PROB NEEDS TO BE USED IN THE CALCULATION
  !          2 IF WK4 NEEDS TO BE USED IN THE CALCULATION (PARTS OF CUTS)
  !   SDIC = STAND DENSITY INDEX FOR THE DEFINED CLASS (STAGE)
  !   SDIC2 = STAND DENSITY INDEX FOR THE DEFINED CLASS (ZEIDE METHOD)
  !    A,B = SDI SUMMATION PARAMETR ESTIMATES
  !  JPNUM = POINT NUMBER (IN FVS SEQUENCE FORMAT), 0 = ALL POINTS

  call DBCHK (DEBUG,'SDICLS',6,ICYC)
  if (DEBUG)write(JOSTND,100)ICYC, ITRN, IREC1, IREC2
  100 format(' ENTERING SDICLS (ENTRY PT IN SDICAL) CYCLE =',I4,&
       &' ITRN=',I4,' IREC1=',I4,' IREC2=',I4)

  !  FIRST, DETERMINE STAGES METHOD PARAMETERS BASED ON ALL TREES IN THE STAND.

  SDSQ = 0.
  SPROB = 0.
  SDIC = 0.
  SDIC2 = 0.
  A = 0.
  B = 0.

  !  LOOP THROUGH THE TREE LIST.
  !  EXCLUDE RECENT AND OLDER DEAD TREES AT INVENTORY TIME.

  if (ITRN .le.0) go to 150
  do II = 1,ITRN
     I = IND1(II)
     if (I.ge.IREC2) go to 110
     if (DEBUG)write(JOSTND,*)&
          &' IN SDICLS I,IWHO,DBHSTAGE,PROB(I),WK4(I)=',&
          &I,IWHO,DBHSTAGE,PROB(I),WK4(I)

     !  BRANCH IF DIAMETER IS LT MIN DBH

     if (DBH(I) .lt. DBHSTAGE) go to 110
     if (IWHO .eq.1) then
        TPACRE = PROB(I)
     else
        TPACRE = WK4(I)
     end if
     SDSQ = SDSQ + (DBH(I)**2.0)*TPACRE
     SPROB = SPROB+TPACRE
     110 continue
  end do
  if (SPROB .eq.0.) then
     SDIC = 0.
  else
     A = (10.0**(-1.605))*(1.-1.605/2.)*((SDSQ/SPROB)**(1.605/2.))
     B = (10.0**(-1.605))*(1.605/2.)*((SDSQ/SPROB)**(1.605/2. - 1.))
     SDIC = SPROB*A + B*SDSQ
  end if
  if (DEBUG)write(JOSTND,*)&
       & ' IN SDICLS ITRN,SDSQ,SPROB,SDIC,A,B,JPNUM                       &
       & = ',ITRN,SDSQ,SPROB,SDIC,A,B,JPNUM

  !  NEXT, DETERMINE SDI BY SPECIES AND SIZE CLASS IN THE DEFINED CLASS
  !  EXCLUDE RECENT AND OLDER DEAD TREES AT INVENTORY TIME.

  SDIC = 0.
  SDIC2 = 0.

  if (ITRN .le.0) go to 150
  do II = 1,ITRN
     I = IND1(II)
     if (I.ge.IREC2) go to 140
     if (JPNUM.gt.0.and. ITRE(I).ne.JPNUM)go to 140
     if (IWHO .eq.1) then
        TPACRE = PROB(I)
     else
        TPACRE = WK4(I)
     end if
     if (JPNUM .gt.0)TPACRE = TPACRE*(PI-FLOAT(NONSTK))

     LINCL = .false.
     if (JSPEC.eq.0) then
        LINCL = .true.
     else if ((JSPEC.eq.ISP(I)).and..not.LEAVESP(ISP(I))) then
        LINCL = .true.
     else if (JSPEC.lt.0) then
        IGRP = -JSPEC
        IULIM = ISPGRP(IGRP,1)+1
        do IG = 2,IULIM
           if ((ISP(I) .eq. ISPGRP(IGRP,IG)).and..not.LEAVESP(ISP(I))) then
              LINCL = .true.
              go to 130
           end if
           120 continue
        end do
     end if
     130 continue

     if (LINCL) then
        if (DBH(I).ge.DLO .and. DBH(I).lt.DHI) then
           if (DBH(I).ge.DBHZEIDE)SDIC2 = SDIC2+TPACRE*(DBH(I)/10.)**1.605
           if (DBH(I).ge.DBHSTAGE)SDIC = SDIC + (A+B*(DBH(I)**2.0))*TPACRE
           if (DEBUG)write(JOSTND,*)' CALC SDIC- I,SDIC,SDIC2,JSPEC,',&
                &'ISP,DBH,TPACRE,A,B,= ',I,SDIC,SDIC2,JSPEC,ISP(I),&
                &DBH(I),TPACRE,A,B
        end if
     end if
     140 continue
  end do
  if (DEBUG)write(JOSTND,*)' LEAVING SDICLS SDIC,SDIC2,JPNUM= ',&
       &SDIC,SDIC2,JPNUM

  150 continue
  return

  entry CCCLS (JSPEC,DLO,DHI,IWHO,CRA,JPNUM)

  !  THIS ENTRY COMPUTES THE CANOPY COVER FOR A GIVEN CLASS OF TREES
  !  DEFINED BY SPEICES AND DBH RANGE, USING THE METHOD DESCRIBED
  !  BY CROOKSTON & STAGE GTR-RMRS-24, 1999.

  !  JSPEC = SPECIES INDEX (0 FOR ALL SPECIES)
  !    DLO = LOWER DBH LIMIT (GREATER THAN OR EQUAL TO)
  !    DHI = UPPER DBH LIMIT (LESS THAN)
  !   IWHO = 1 IF PROB NEEDS TO BE USED IN THE CALCULATION
  !          2 IF WK4 NEEDS TO BE USED IN THE CALCULATION (PARTS OF CUTS)
  !    CRA = SUM OF CROWN AREAS FOR THE DEFINED CLASS
  !   CCCL = CANOPY COVER FOR THE DEFINED CLASS
  !  JPNUM = POINT NUMBER (IN FVS SEQUENCE FORMAT), 0 = ALL POINTS

  call DBCHK (DEBUG,'CCCLS',5,ICYC)
  if (DEBUG)write(JOSTND,*)' ENTERING CCCLS (ENTRY PT IN SDICAL)'

  !  DETERMINE CROWN COVER BY SPECIES AND SIZE CLASS
  !  EXCLUDE RECENT AND OLDER DEAD TREES AT INVENTORY TIME.

  CCCL = 0.
  CRA = 0.

  if (ITRN .le.0) go to 250
  do II = 1,ITRN
     I = IND1(II)
     if (I.ge.IREC2) go to 240
     if (JPNUM.gt.0.and. ITRE(I).ne.JPNUM)go to 240

     LINCL = .false.
     if ((JSPEC.eq.0.or. JSPEC.eq.ISP(I)).and..not.LEAVESP(ISP(I))) then
        LINCL = .true.
     else if (JSPEC.lt.0) then
        IGRP = -JSPEC
        IULIM = ISPGRP(IGRP,1)+1
        do IG = 2,IULIM
           if ((ISP(I) .eq. ISPGRP(IGRP,IG)).and..not.LEAVESP(ISP(I))) then
              LINCL = .true.
              go to 230
           end if
           220 continue
        end do
     end if
     230 continue

     if (LINCL) then
        if (DBH(I).ge.DLO .and. DBH(I).lt.DHI) then
           if (IWHO .eq.1) then
              TPACRE = PROB(I)
           else
              TPACRE = WK4(I)
           end if
           if (JPNUM .gt.0)TPACRE = TPACRE*(PI-FLOAT(NONSTK))
           CWDI = CRWDTH(I)
           CRA = CRA + CWDI*CWDI*TPACRE
           if (DEBUG)write(JOSTND,*)' I,JSPEC,ISP,DBH,HT,ICR,TPACRE,',&
                &'CWDI,CRA= ',I,JSPEC,ISP(I),DBH(I),HT(I),ICR(I),TPACRE,CWDI,&
                &CRA
        end if
     end if
     240 continue
  end do
  CCCL = 100.0*CRA*0.785398/43560.
  !     CCCL = 100.0*(1.0-EXP(-CCCOEF*CCCL))
  !     NEED LIMIT ON EXP VALUE TO AVOID UNDERFLOW   LD 4/2/2019
  TEM = MIN(86.0,(CCCOEF*CCCL))
  CCCL = 100.0*(1.0-EXP(-TEM))
  IF(DEBUG)WRITE(JOSTND,*)' LEAVING CCCLS ITRN,CCCL,CRA,JPNUM,      &
       &CCCOEF= ',ITRN,CCCL,CRA,JPNUM,CCCOEF

  250 continue

  return

  entry RDCLS (JSPEC,DLO,DHI,IWHO,CLSD2,CLSTPA,CRD,JPNUM,&
       &TPAFAC,DIAMFAC)

  !  THIS ENTRY COMPUTES THE RELATIVE DENSITY FOR A GIVEN CLASS OF TREES
  !  DEFINED BY SPEICES AND DBH RANGE, USING THE METHOD DESCRIBED
  !  BY LU, H.B., F. MARTIN, AND R. JOHNSON. 2017.
  !  (Keyser, Chad E.; Keyser, Tara L. eds. 2017. Proceeding of the 2017
  !  Forest vegetation simulator (FVS) e-Conference. e-Gen. Tech. Rep. SRS-224.
  !  Asheville, NC: U.S. Department of Agriculture Forest Service,
  !  Southern Research Station. 200 p).

  !  JSPEC = SPECIES INDEX (0 FOR ALL SPECIES)
  !    DLO = LOWER DBH LIMIT (GREATER THAN OR EQUAL TO)
  !    DHI = UPPER DBH LIMIT (LESS THAN)
  !   IWHO = 1 IF PROB NEEDS TO BE USED IN THE CALCULATION
  !          2 IF WK4 NEEDS TO BE USED IN THE CALCULATION (PARTS OF CUTS)
  !  TPAFAC = CRD SUMMATION PARAMETR ESTIMATE FACTORS FOR TPA
  ! DIAMFAC   AND DIAMETER USED IN EVLDX.F SPMCDBH OPTION FOR COMPUTE OF
  !           CURTIS RELATIVE DENSITY.
  !   CLSD2 = SUM OF DIAMETERS SQUARED FOR DEFINED CLASS
  !  CLSTPA = NUMBER OF TREES IN DEFINED CLASS
  !    CRD = RELATIVE DENSITY FOR THE DEFINED CLASS
  !  JPNUM = POINT NUMBER (IN FVS SEQUENCE FORMAT), 0 = ALL POINTS

  ! INITIALIZE RETURN VARIABLES

  TPAFAC = 0.0
  DIAMFAC = 0.0
  CLSD2 = 0.0
  CLSTPA = 0.0
  CRD = 0.0

  call DBCHK (DEBUG,'RDCLS',5,ICYC)
  if (DEBUG)write(JOSTND,*)' ENTERING RDCLS (ENTRY PT IN SDICAL)',&
       &' JSPEC,DLO,DHI,IWHO,CLSD2,CLSTPA,CRD,JPNUM,TPAFAC,DIAMFAC =',&
       &JSPEC,DLO,DHI,IWHO,CLSD2,CLSTPA,CRD,JPNUM,TPAFAC,DIAMFAC

  ! FIRST, DETERMINE PARAMETERS BY EULERS THEOREM BASED ON ALL TREES
  ! IN THE STAND.

  if (ITRN .le.0) go to 355

  !     STATIC COMPONENTS OF EQUATIONS SET HERE.

  CMPNT1 = 0.25*3.14159
  CMPNT2 = 24.0**2.0
  CMPNT4 = 1.5/2.0
  CMPNT5 = CMPNT1/CMPNT2
  CMPNT6 = 0.75*3.14159

  !  LOOP THROUGH ALL TREES IN THE STAND.
  !  EXCLUDE RECENT AND OLDER DEAD TREES AT INVENTORY TIME.

  do II = 1,ITRN
     I = IND1(II)
     if (I.ge.IREC2) go to 320
     if (DBH(I).lt.DBHSTAGE) go to 320 ! BRANCH IF D IS LT MIN DBH
     if (IWHO .eq.1) then
        TPACRE = PROB(I)
     else
        TPACRE = WK4(I)
     end if
     CLSD2 = CLSD2 + (DBH(I)**2.0)*TPACRE
     CLSTPA = CLSTPA + TPACRE
     320 continue
  end do
  if (CLSTPA .gt.0.) then

     !       *** PI was implemented as FUNCTION API() IN ORIGINAL CODE
     !       A = (0.25*API()/24.0**2.0)*((SDSQ/SPROB)**(1.5/2.0))
     !       A = (0.25*3.14159/24.0**2.0)*((CLSD2/CLSTPA)**(1.5/2.0))
     !            --CMPNT1---  --CMPNT2-    ---CMPNT3---    -CMPNT4-
     !            --------CMPNT5--------

     CMPNT3 = CLSD2/CLSTPA
     TPAFAC = CMPNT5 * (CMPNT3**CMPNT4)

     !       *** PI was implemented as FUNCTION API() IN ORIGINAL CODE
     !       B = (0.75*API()/24.0**2.0)*((SDSQ/SPROB)**(1.5/2.0 - 1.0))
     !       B = (0.75*3.14159/24.0**2.0)*((CLSD2/CLSTPA)**(1.5/2.0 - 1.0))
     !            --CMPNT6---  --CMPNT2-    ---CMPNT3---    -CMPNT4-

     DIAMFAC = (CMPNT6/CMPNT2) * (CMPNT3**(CMPNT4 - 1.0))

     CRD = CLSTPA*TPAFAC + CLSD2*DIAMFAC
  end if
  if (DEBUG)write(JOSTND,*)' IN RDCLS ALL TREES CRD= ',CRD

  ! NEXT, DETERMINE RELATIVE DENSITY BY SPECIES AND SIZE CLASS IN
  ! THE DEFINED CLASS

  CRD = 0.

  do II = 1,ITRN
     I = IND1(II)
     if (I.ge.IREC2) go to 350
     if (JPNUM.gt.0.and. ITRE(I).ne.JPNUM) go to 350
     if (IWHO .eq.1) then
        TPACRE = PROB(I)
     else
        TPACRE = WK4(I)
     end if
     if (JPNUM .gt.0)TPACRE = TPACRE*(PI-FLOAT(NONSTK))

     LINCL = .false.
     if (JSPEC.eq.0) then
        LINCL = .true.
     else if ((JSPEC.eq.ISP(I)).and..not.LEAVESP(ISP(I))) then
        LINCL = .true.
     else if (JSPEC.lt.0) then
        IGRP = -JSPEC
        IULIM = ISPGRP(IGRP,1)+1
        do IG = 2,IULIM
           if ((ISP(I).eq.ISPGRP(IGRP,IG)).and..not.LEAVESP(ISP(I))) then
              LINCL = .true.
              go to 335
           end if
           330 continue
        end do
     end if
     335 continue

     if (LINCL) then
        if (DBH(I).ge.DLO .and. DBH(I).lt.DHI) then
           if (DBH(I).ge.DBHSTAGE) then
              CRD = CRD + (TPAFAC+DIAMFAC*(DBH(I)**2.0))*TPACRE
           end if
           if (DEBUG)write(JOSTND,*)' CALC CRD CLASS- I,CRD,JSPEC,',&
                &'ISP,DBH,TPACRE= ',I,CRD,JSPEC,ISP(I),DBH(I),TPACRE
        end if
     end if
     350 continue
  end do
  355 continue
  if (DEBUG) write(JOSTND,*)&
       &' LEAVING RDCLS CRD,JPNUM,CLSD2,CLSTPA,TPAFAC,DIAMFAC= ',&
       &CRD,JPNUM,CLSD2,CLSTPA,TPAFAC,DIAMFAC

  return

  entry RDCLS2 (JSPEC,DLO,DHI,IWHO,CRD,JPNUM)

  !  THIS ENTRY COMPUTES THE RELATIVE DENSITY FOR A GIVEN CLASS OF TREES
  !  DEFINED BY SPEICES AND DBH RANGE, USING THE METHOD DESCRIBED
  !  BY MARQUIS, D.A., AND R.L. ERNST. 1992. USERS GUIDE TO SILVAH.
  !  USDA FOREST SERVICE, NORTHEAST FOREST EXP STA, GEN TECH REP NE-96.

  !  JSPEC = SPECIES INDEX (0 FOR ALL SPECIES)
  !    DLO = LOWER DBH LIMIT (GREATER THAN OR EQUAL TO)
  !    DHI = UPPER DBH LIMIT (LESS THAN)
  !   IWHO = 1 IF PROB NEEDS TO BE USED IN THE CALCULATION
  !          2 IF WK4 NEEDS TO BE USED IN THE CALCULATION (PARTS OF CUTS)
  !    CRD = RELATIVE DENSITY FOR THE DEFINED CLASS
  !  JPNUM = POINT NUMBER (IN FVS SEQUENCE FORMAT), 0 = ALL POINTS

  call DBCHK (DEBUG,'RDCLS2',6,ICYC)
  if (DEBUG)write(JOSTND,*)' ENTERING RDCLS2 (ENTRY PT IN SDICAL)'

  ! DETERMINE RELATIVE DENSITY BY SPECIES AND SIZE CLASS

  CRD = 0.

  !  THIS IS CURRENTLY ALLOWED ONLY FOR THE NE VARIANT
  !  EXCLUDE RECENT AND OLDER DEAD TREES AT INVENTORY TIME.

  if (VARACD.ne.'NE')go to 460

  if (ITRN .le.0) go to 460
  do II = 1,ITRN
     I = IND1(II)
     if (I.ge.IREC2) go to 450
     if (JPNUM.gt.0.and. ITRE(I).ne.JPNUM)go to 450

     LINCL = .false.
     if ((JSPEC.eq.0.or. JSPEC.eq.ISP(I)).and..not.LEAVESP(ISP(I))) then
        LINCL = .true.
     else if (JSPEC.lt.0) then
        IGRP = -JSPEC
        IULIM = ISPGRP(IGRP,1)+1
        do IG = 2,IULIM
           if ((ISP(I) .eq. ISPGRP(IGRP,IG)).and..not.LEAVESP(ISP(I))) then
              LINCL = .true.
              go to 440
           end if
           420 continue
        end do
     end if
     440 continue

     if (LINCL) then
        if ((DBH(I).ge.DLO).and.(DBH(I).lt.DHI).and.(DBH(I).ge.1.)) then
           if (IWHO .eq.1) then
              TPACRE = PROB(I)
           else
              TPACRE = WK4(I)
           end if
           if (JPNUM .gt.0)TPACRE = TPACRE*(PI-FLOAT(NONSTK))
           IEQN = MAPNE(ISP(I))
           D2 = DBH(I)*DBH(I)

           if (IEQN .eq.1) then
              CRD = CRD + MAX(0.,TPACRE*(0.0033033+0.020426*&
                   &DBH(I)+0.0006776*D2))
           else if (IEQN .eq.2) then
              CRD = CRD + MAX(0.,TPACRE*(-0.027142+0.024257*&
                   &DBH(I)+0.0015225*D2))
           else if (IEQN .eq.3) then
              CRD = CRD + MAX(0.,TPACRE*(-0.0027935+0.0058959*&
                   &DBH(I)+0.0047289*D2))
           end if
           if (DEBUG)write(JOSTND,*)' I,FIAJSP,JSPEC,ISP,DBH,HT,TPACRE,',&
                &'D2,CRD,JPNUM,IEQN= ',&
                &I,FIAJSP(ISP(I)),JSPEC,ISP(I),DBH(I),HT(I),TPACRE,D2,&
                &CRD,JPNUM,IEQN
        end if
     end if
     450 continue
  end do
  if (DEBUG)write(JOSTND,*)' LEAVING RDCLS2 ICYC,ITRN,',&
       &'CRD,JPNUM,IEQN= ',ICYC,ITRN,CRD,JPNUM,IEQN

  460 continue
  return

  entry RDSLTR (JSPEC,IT,TREERD)

  !  THIS ENTRY COMPUTES THE RELATIVE DENSITY CONTRIBUTION OF A GIVEN
  !  TREE, USING THE METHOD DESCRIBED
  !  BY MARQUIS, D.A., AND R.L. ERNST. 1992. USERS GUIDE TO SILVAH.
  !  USDA FOREST SERVICE, NORTHEAST FOREST EXP STA, GEN TECH REP NE-96.

  !  JSPEC = TREE SPECIES
  !     IT = TREE INDEX
  ! TREERD = RELATIVE DENSITY CONTRIBUTION OF THIS TREE

  call DBCHK (DEBUG,'RDSLTR',6,ICYC)
  if (DEBUG)write(JOSTND,*)' ENTERING RDSLTR (ENTRY PT IN SDICAL)'

  !  DETERMINE RELATIVE DENSITY BY SPECIES AND SIZE CLASS
  !  EXCLUDE RECENT AND OLDER DEAD TREES AT INVENTORY TIME.

  TREERD = 0.
  if (IT.ge.IREC2) go to 560

  !  THIS IS CURRENTLY ALLOWED ONLY FOR THE NE VARIANT

  if (VARACD.ne.'NE')go to 560
  if (DBH(IT).lt.1.)goto 560
  IEQN = MAPNE(JSPEC)
  D2 = DBH(IT)*DBH(IT)
  if (IEQN .eq.1) then
     TREERD = MAX(0.,(0.0033033+0.020426*DBH(IT)+0.0006776*D2))
  else if (IEQN .eq.2) then
     TREERD = MAX(0.,(-0.027142+0.024257*DBH(IT)+0.0015225*D2))
  else if (IEQN .eq.3) then
     TREERD = MAX(0.,(-0.0027935+0.0058959*DBH(IT)+0.0047289*D2))
  end if
  if (DEBUG)write(JOSTND,*)' LEAVING RDSLTR  - IEQN,JSPEC,IT,DBH,',&
       &' TREERD= ',IEQN,JSPEC,IT,DBH(IT),TREERD
  560 continue
  return

  entry SILFTY

  !  THIS ENTRY COMPUTES THE FOREST TYPE ACCORDING TO THE SILVAH
  !  DEFINITIONS. THE SILVAH FOREST TYPE IS USED TO DETERMINE WHETHER
  !  A THINRDSL TYPE THINNING IS VALID; A THINRDSL IS ONLY VALID IN
  !  ONE OF THE DEFINED TYPES.

  !  ISILFT = SILVAH FOREST TYPE (CARRIED IN THE VARCOM COMMON BLOCK)
  !     0 = NO, THIS STAND DOES NOT MEET ONE OF THE DEFINED TYPES
  !     1 = NORTHERN HARDWOOD: STANDS HAVING 65% OF THE BASAL AREA IN
  !         SUGAR MAPLE, RED MAPLE, AMERICAN BEECH, YELLOW BIRCH, SWEET
  !         BIRCH, EASTERN HEMLOCK, AMERICAN BASSWOOD, CUCUMBERTREE, BLACK
  !         CHERRY, WHITE ASH, OR YELLOW-POPLAR
  !     2 = NORTHERN HARDWOOD-HEMLOCK: STANDS MEETING THE REQUIREMENTS FOR
  !         THE NORTHERN HARDWOOD TYPE THAT ALSO HAVE AT LEAST 50% OF THE
  !         BASAL AREA IN EASTERN HEMLOCK
  !     3 = ALLEGHENY HARDWOOD: STANDS MEETING THE REQUIREMENTS FOR THE
  !         NORTHERN HARDWOOD TYPE THAT HAVE AT LEAST 25% OF THE BASAL AREA
  !         IN BLACK CHERRY, WHITE ASH, AND YELLOW-POPLAR, AND LESS THAN
  !         50% OF THE BASAL AREA IN EASTERN HEMLOCK
  !     4 = OAK-HICKORY: STANDS THAT HAVE AT LEAST PART OF THEIR BASAL AREA
  !         IN ANY OAK OR ANY HICKORY SPECIES
  !     5 = TRANSITION: STANDS THAT HAVE AT LEAST 65% OF THE BASAL AREA IN
  !         SPECIES OF EITHER THE NORTHERN HARDWOOD OR OAK-HICKORY TYPES,
  !         BUT DO NOT QUALIFY FOR ANY OF THE OTHER TYPES ALONE

  call DBCHK (DEBUG,'SILFTY',6,ICYC)
  if (DEBUG)write(JOSTND,*)' ENTERING SILFTY (ENTRY PT IN SDICAL)'

  ISILFT = 0
  BA1 = 0.
  BA2 = 0.
  BA3 = 0.
  BA4 = 0.
  BA5 = 0.
  BATOT = 0.
  RATIO1 = 0.
  RATIO2 = 0.
  RATIO3 = 0.
  RATIO5 = 0.

  !  THIS IS CURRENTLY ALLOWED ONLY FOR THE NE VARIANT
  !  EXCLUDE RECENT AND OLDER DEAD TREES AT INVENTORY TIME.

  if (VARACD.ne.'NE')go to 610

  do II = 1,ITRN
     I = IND1(II)
     if (I.ge.IREC2) go to 600
     ISPC = ISP(I)
     BATOT = BATOT+DBH(I)*DBH(I)*0.0054542*PROB(I)
     if (ISLFTM(ISPC) .eq.1) then
        BA1 = BA1+DBH(I)*DBH(I)*0.0054542*PROB(I)
        if (ISPC.eq.16)BA2 = BA2+DBH(I)*DBH(I)*0.0054542*PROB(I)
        if (ISPC.eq.54.or. ISPC.eq.42.or. ISPC.eq.46)&
             &BA3 = BA3+DBH(I)*DBH(I)*0.0054542*PROB(I)
     else if (ISLFTM(ISPC) .eq.2) then
        BA4 = BA4+DBH(I)*DBH(I)*0.0054542*PROB(I)
     end if
     600 continue
  end do
  BA5 = BA1+BA4
  if (DEBUG)write(JOSTND,*)' IN SILFTY BATOT,BA1,BA2,BA3,BA4,BA5= ',&
       &BATOT,BA1,BA2,BA3,BA4,BA5
  if (BATOT .gt.0.) then
     RATIO1 = BA1/BATOT
     RATIO2 = BA2/BATOT
     RATIO3 = BA3/BATOT
     RATIO5 = BA5/BATOT
  end if
  if (RATIO1.ge.0.65)ISILFT = 1
  if (RATIO1.ge.0.65.and. RATIO2.ge.0.50)ISILFT = 2
  if (RATIO1.ge.0.65.and. RATIO3.ge.0.25.and. RATIO2.lt.0.50)&
       &ISILFT = 3
  if (BA4.gt.0.)ISILFT = 4
  if (ISILFT.eq.0.and. RATIO5.ge.0.65)ISILFT = 5
  610 continue
  if (DEBUG)write(JOSTND,*)' LEAVING SILFTY ISILFT= ',ISILFT
  return

end
