!=======================================================!
!   Compute individual tree crown competition factors   !
!=======================================================!
! KT $Id$
subroutine CCFCAL(ISPC,D,H,JCR,P,LTHIN,CCFT,CRWDTH,MODE)
  implicit none

  !  THIS ROUTINE COMPUTES CROWN WIDTH AND CCF FOR INDIVIDUAL TREES.
  !  CALLED FROM DENSE, REGENT, PRTRLS, SSTAGE, AND CVCW.

  !  CROWN WIDTH EQUATIONS FOR SPECIES 1-10 ARE FROM ANALYSIS BY
  !  NICK CROOKSTON USING DATA FROM RENATE BUSH. CROWN WIDTH EQUATION
  !  FOR SPECIES 11 IS FROM ANALYSIS BY NICK CROOKSTON USING CVS DATA
  !  FROM THE COLVILLE NF.

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

  ! Variables
  logical :: LTHIN
  real    :: CCFT,CRWDTH,C1,C2,P,D,H,BAREA,CL,W
  integer :: MODE,JCR,ISPC
  real    :: RD1(MAXSP),RD2(MAXSP),RD3(MAXSP),RDA(MAXSP),RDB(MAXSP)
  real    :: B1(MAXSP),B2(MAXSP),B3(MAXSP),B4(MAXSP),B5(MAXSP),B6(MAXSP)

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

  !  INITIALIZE RETURN VARIABLES.

  CCFT = 0.
  CRWDTH = 0.

  !  COMPUTE CCF

  if (MODE.eq.1) then
     if (D.ge.10.0) then
        CCFT = RD1(ISPC) + D*RD2(ISPC) + D*D*RD3(ISPC)
     else
        CCFT = RDA(ISPC) * (D**RDB(ISPC))
     end if
     CCFT = CCFT*P
  end if

  !  COMPUTE CROWN WIDTH.

  if (JCR .le.0) go to 100
  if (MODE.eq.2) then
     CL = FLOAT(JCR)*H*.01
     BAREA = BA
     if (LTHIN.or.LFIRE) BAREA = OLDBA
     if (BAREA.le.0..or. CL.le.0..or. H.le.0..or.D.le.0.)goto 100

     !  FOR COLVILLE (R6) USE R6 CROWN WIDTH LOGIC

     if (IFOR .eq.5) then
        call R6CRWD (ISPC,D,H,CRWDTH)
     else
        select case (ISPC)
        case(1:10)
           if (BAREA.le.1.)BAREA = 1.
           CRWDTH = B1(ISPC)*EXP(B2(ISPC)+B3(ISPC)*ALOG(CL)+B4(ISPC)*&
                &ALOG(D)+B5(ISPC)*ALOG(H)+B6(ISPC)*ALOG(BAREA))

        case(11)
           if (H.le.5.) then
              CRWDTH = .8*H*MAX(.5,JCR*.01)
           else if (H.ge.15.) then
              CRWDTH = 6.90396*D**0.55645*H**(-.28509)*CL**0.20430
           else
              C1 = .8*H*MAX(.5,JCR*.01)
              C2 = 6.90396*D**0.55645*H**(-0.28509)*CL**0.20430
              W = (H-5.)*.1
              CRWDTH = C1*(1.-W)+C2*W
           end if
        end select

     end if
     if (CRWDTH .gt.99.9) CRWDTH = 99.9
  end if

  100 continue
  return
end
