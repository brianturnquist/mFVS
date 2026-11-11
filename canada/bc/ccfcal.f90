!=======================================================!
!   Compute individual tree crown competition factors   !
!=======================================================!
! CANADA-BC $Id$
subroutine CCFCAL(ISPC,D,H,JCR,P,LTHIN,CCFT,CRWDTH,MODE)
  implicit none

  !  THIS ROUTINE COMPUTES CROWN WIDTH AND CCF FOR INDIVIDUAL TREES.
  !  CALLED FROM DENSE, REGENT, PRTRLS, SSTAGE, AND CVCW.

  !  CROWN WIDTH EQUATIONS FOR SPECIES 1-10 ARE FROM ANALYSIS BY
  !  NICK CROOKSTON USING DATA FROM RENATE BUSH. CROWN WIDTH EQUATION
  !  FOR SPECIES 11 IS FROM ANALYSIS BY NICK CROOKSTON USING CVS DATA
  !  FROM THE COLVILLE NF.

  !  THIS IS A BLEND OF NI- AND PN- DERIVED RELATIONSHIPS

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'

  !  ARGUMENT DEFINITIONS:
  !    ISPC = NUMERIC SPECIES CODE
  !       D = DIAMETER AT BREAST HEIGHT
  !       H = TOTAL TREE HEIGHT
  !     JCR = CROWN RATIO IN PERCENT (0-100)
  !       P = TREES PER ACRE
  !   LTHIN = .TRUE. IF THINNING HAS JUST OCCURRED
  !         = .FALSE. OTHERWISE
  !    CCFT = CCF REPRESENTED BY THIS TREE
  !  CRWDTH = CROWN WIDTH OF THIS TREE
  !    MODE = 1 IF ONLY NEED CCF RETURNED
  !           2 IF ONLY NEED CRWDTH RETURNED

  !  DIMENSION AND DATA STATEMENTS FOR INTERNAL VARIABLES.

  !     CCF COEFFICIENTS FOR TREES THAT ARE GREATER THAN 10.0 IN. DBH:
  !      RD1 -- CONSTANT TERM IN CROWN COMPETITION FACTOR EQUATION,
  !             SUBSCRIPTED BY SPECIES
  !      RD2 -- COEFFICIENT FOR SUM OF DIAMETERS TERM IN CROWN
  !             COMPETITION FACTOR EQUATION,SUBSCRIPTED BY SPECIES
  !      RD3 -- COEFFICIENT FOR SUM OF DIAMETER SQUARED TERM IN
  !             CROWN COMPETITION EQUATION, SUBSCRIPTED BY SPECIES

  !     CCF COEFFICIENTS FOR TREES THAT ARE LESS THAN 10.0 IN. DBH:
  !      RDA -- MULTIPLIER.
  !      RDB -- EXPONENT.  CCF(I) = RDA*DBH**RDB

  !   ** FROM PN-CCFCAL **

  !  DEFINITIONS OF TERMS IN CCF EQUATION:
  !      pnRD1 -- CONSTANT TERM SUBSCRIPTED BY SPECIES.
  !      pnRD2 -- DIAMETER TERM SUBSCRIPTED BY SPECIES.
  !      pnRD3 -- DIAMETER SQUARED TERM SUBSCRIPTED BY SPECIES.
  !             NOTE: D=1.0 IS USED FOR D-SQUARED IF D LE 1.0 INCHES

  !  SPECIES ORDER 1=SF,  2=WF,  3=GF,  4=AF,  5=RF,  6=SS,  7=NF,  8=YC,
  !                9=IC, 10=ES, 11=LP, 12=JP, 13=SP, 14=WP, 15=PP, 16=DF,
  !               17=RW, 18=RC, 19=WH, 20=MH, 21=BM, 22=RA, 23=WA, 24=PB,
  !               25=GC, 26=AS, 27=CW, 28=WO, 29=J , 30=LL, 31=WB, 32=KP,
  !               33=PY, 34=DG, 35=HT, 36=CH, 37=WI, 38=  , 39=OT

  !  SOURCES OF COEFFICIENTS:
  !      PAINE AND HANN, 1982. MAXIMUM CROWN WIDTH EQUATIONS FOR
  !        SOUTHWESTERN OREGON TREE SPECIES. RES PAP 46, FOR RES LAB
  !        SCH FOR, OSU, CORVALLIS. 20PP.
  !      RITCHIE AND HANN, 1985. EQUATIONS FOR PREDICTING BASAL AREA
  !        INCREMENT IN DOUGLAS-FIR AND GRAND FIR. RES PAP 51, FOR RES
  !        LAB SCH FOR, OSU, CORVALLIS. 9PP. (TABLE 2 PG 8)
  !      SMITH 1966. STUDIES OF CROWN DEVELOPMENT ARE IMPROVING CANADIAN
  !        FOREST MANAGEMENT. PROCEEDINGS, SIXTH WORLD FORESTRY CONGRESS.
  !        MADRID, SPAIN. VOL 2:2309-2315. (TABLES 1 & 2, PG 2310)

  ! Variables
  logical :: LTHIN
  real    :: CCFT,CRWDTH,P,D,H,BAREA,CL
  integer :: MODE,JCR,ISPC,IC
  real    :: RD1(11),RD2(11),RD3(11),RDA(11),RDB(11)
  real    :: B1(11),B2(11),B3(11),B4(11),B5(11),B6(11)
  data RD1 /.03,.02,.11,.04,.03,.03,.01925,.03,.03,.03,.03/
  data RD2 /.0167,.0148,.0333,.0270,.0215,.0238,.01676,.0173,&
       &.0216,.0180,.0215/
  data RD3 /.00230,.00338,.00259,.00405,.00363,.00490,.00365,&
       &.00259,.00405,.00281,.00363/
  data RDA/&
       &0.009884, 0.007244, 0.017299, 0.015248, 0.011109,&
       &0.008915, 0.009187, 0.007875, 0.011402, 0.007813, 0.011109/
  data RDB/&
       &1.6667,  1.8182,  1.5571,  1.7333,  1.7250,&
       &1.7800,  1.7600,  1.7360,  1.7560,  1.7680,  1.7250/

  !     B1  = BIAS CORRECTION COEFFICIENT
  !     B2  = CONSTANT TERM
  !     B3  = LOG-NATURAL OF CROWN LENGTH COEFFICIENT
  !     B4  = LOG-NATURAL OF DBH COEFFICIENT
  !     B5  = LOG-NATURAL OF TOTAL TREE HEIGHT COEFFICIENT
  !     B6  = LOG-NATURAL OF BASAL AREA COEFFICIENT

  data B1/&
       &1.04050, 1.02478, 1.01685, 1.03030, 1.02460, 1.03597,&
       &1.03992, 1.02687, 1.02886, 1.02687, 0.00000/
  data B2/&
       &1.27990, 0.99889, 1.48372, 1.14079, 1.35223, 1.46111,&
       &1.58777, 1.28027, 1.01255, 1.49085, 0.00000/
  data B3/&
       &0.11941, 0.19422, 0.27378, 0.20904, 0.24844, 0.26289,&
       &0.30812, 0.22490, 0.30374, 0.18620, 0.00000/
  data B4/&
       &0.42745, 0.59423, 0.49646, 0.38787, 0.41212, 0.18779,&
       &0.64934, 0.47075, 0.37093, 0.68272, 0.00000/
  data B5/&
       &0.00000, -0.09078, -0.18669, 0.00000, -0.10436, 0.00000,&
       &-0.38964, -0.15911, -0.13731, -0.28242, 0.00000/
  data B6/&
       &-0.07182,-0.02341,-0.01509, 0.00000, 0.03539, 0.00000,&
       &0.00000, 0.00000, 0.00000, 0.00000, 0.00000/

  !     VARIABLES FROM THE PN

  real    :: pnRD1(19),pnRD2(19),pnRD3(19)
  integer :: INDCCF(39)

  data INDCCF/&
       &1,  2,  2,  3,  4,  8,  3,  5,  5,  7,&
       &10, 19, 19, 11, 19, 12, 12, 13, 14, 14,&
       &15, 16, 16, 17, 15, 17, 18, 17,  6, 19,&
       &9,  9,  6, 15, 17, 15, 15, 10, 10    /

  data pnRD1/&
       &1.01420E-1, 6.90403E-2, 2.45276E-2, 1.72000E-2, 1.94415E-2,&
       &3.18054E-2, 2.88484E-2, 7.61779E-2, 2.00000E-2, 2.20871E-2,&
       &1.85728E-2, 3.87616E-2, 2.88484E-2, 3.75770E-2, 1.60051E-2,&
       &1.15394E-1, 1.70887E-2, 4.50757E-4, 2.19000E-2 /

  data pnRD2/&
       &4.32725E-2, 2.24682E-2, 1.14741E-2, 0.87600E-2, 1.42461E-2,&
       &2.15065E-2, 1.73091E-2, 4.21908E-2, 1.68000E-2, 2.52424E-2,&
       &1.46210E-2, 2.68821E-2, 2.37999E-2, 2.32893E-2, 1.66659E-2,&
       &4.41381E-2, 2.13617E-2, 2.92090E-3, 1.68000E-2 /

  data pnRD3/&
       &4.61575E-3, 1.82799E-3, 1.34190E-3, 1.12000E-3, 2.60979E-3,&
       &3.63562E-3, 2.59636E-3, 5.84180E-3, 3.25000E-3, 7.21210E-3,&
       &2.87750E-3, 4.66086E-3, 4.90874E-3, 3.60853E-3, 4.33848E-3,&
       &4.22070E-3, 6.67579E-3, 4.73186E-3, 3.25000E-3 /

  !  INITIALIZE RETURN VARIABLES.

  CCFT = 0.0
  CRWDTH = 0.0

  !     COMPUTE OC (14) AND OH (15) USING FD AND EP

  select case (ISPC)

  case (1:10,14) ! EQUATIONS DERIVED FROM NI

     select case (ISPC)
     case (1:10)
        IC = ISPC
     case (14)
        IC = 3
     end select

     !         COMPUTE CCF

     if (MODE .eq.1) then
        if (D.ge.10.0) then
           CCFT = RD1(IC) + D*RD2(IC) + D*D*RD3(IC)
        else
           CCFT = RDA(IC) * (D**RDB(IC))
        end if
        if (CCFT .lt.0.001) CCFT = 0.001
        CCFT = CCFT * P
     end if

     !         COMPUTE CROWN WIDTH.

     if (JCR .le.0) go to 100
     if (MODE .eq.2) then
        CL = FLOAT(JCR) * H * 0.01
        BAREA = BA
        if (LTHIN.or.LFIRE) BAREA = OLDBA
        if (BAREA .le.0.0.or. CL .le.0.0.or.&
             &H .le.0.0.or. D.le.0.0) goto 100
        if (BAREA.le.1.)BAREA = 1.
        CRWDTH = B1(IC)*EXP(B2(IC)+B3(IC)*ALOG(CL)+B4(IC)*&
             &ALOG(D)+B5(IC)*ALOG(H)+B6(IC)*ALOG(BAREA))
        if (CRWDTH .gt.99.9) CRWDTH = 99.9
     end if

  case (11:13,15) ! EQUATIONS DERIVED FROM PN

     select case (ISPC)
     case (11,15)
        IC = INDCCF(24)
     case (12)
        IC = INDCCF(26)
     case (13)
        IC = INDCCF(27)
     end select

     !         COMPUTE CCF

     if (MODE.eq.1.or. MODE.eq.3) then
        if (D .ge.1.0) then
           CCFT = pnRD1(IC) + pnRD2(IC)*D + pnRD3(IC)*D**2.0
        else
           CCFT = D * (pnRD1(IC) + pnRD2(IC) + pnRD3(IC))
        end if
        if (CCFT .lt.0.001) CCFT = 0.001
        CCFT = CCFT * P
     end if

     !         COMPUTE CROWN WIDTH

     if (MODE.eq.2.or. MODE.eq.3) then
        call R6CRWD (ISPC,D,H,CRWDTH)
        if (CRWDTH .gt.99.9) CRWDTH = 99.9
     end if

  end select

  100 continue
  if (CRWDTH .lt.0.1) CRWDTH = 0.1
  return
end
