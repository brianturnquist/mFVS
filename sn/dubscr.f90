!====================================================!
!   Estimate missing and regeneration crown ratios   !
!====================================================!
! SN $Id$
subroutine DUBSCR(D,CR)
  implicit none

  !  THIS SUBROUTINE CALCULATES CROWN RATIOS FOR CYCLE 0.0
  !  DEAD TREES

  !      INCLUDE 'PRGPRM.F77'

  !      INCLUDE 'CONTRL.F77'

  !      EXTERNAL RANN

  !  EXPECTED CROWN RATIO IS A FUNCTION OF, DBH,
  !  AND RETURNS A VALUE BETWEEN 0.30 ZERO AND 0.70

  ! Variables
  real    :: CR,D

  if (D .le.24.) then
     CR = 0.70-0.40/24.*D
  else
     CR = 0.30
  end if

  !  RANDOM COMPONENT NOT CURRENTLY IMPLEMENTED

  !  A RANDOM ERROR IS ASSIGNED TO THE CROWN RATIO PREDICTION
  !  PRIOR TO THE LOGISTIC TRANSFORMATION.  LINEAR REGRESSION
  !  WAS USED TO FIT THE MODEL AND THE ELEMENTS OF CRSD ARE THE
  !  STANDARD ERRORS FOR THE LINEARIZED MODEL BY SPECIES.

  !      SD=CRSD(ISPC)
  !   10 CONTINUE
  !      FCR=0.0
  !      IF (DGSD.GE.1.0) FCR=BACHLO(0.0,SD,RANN)
  !      IF(ABS(FCR).GT.SD) GO TO 10
  !      CR=1.0/(1.0 + EXP(CR+FCR))

  if (CR.lt.0.05) CR = 0.05
  if (CR.gt.0.95) CR = 0.95
  return
end
