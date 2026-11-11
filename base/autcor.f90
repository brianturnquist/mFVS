!=====================================================!
!   Compute cycle length and growth autocorrelation   !
!=====================================================!
! BASE $Id$
subroutine AUTCOR (COV,VRNEXT)
  implicit none

  !  PROCEDURE FOR CALCULATING EFFECT OF CYCLE LENGTH ON RANDOM COMPONENT OF
  !  DIAMETER INCREMENT AND ITS AUTO-REGRESSIVE PROPERTIES.
  !  PROCEDURE ASSUMES ANNUAL TREE RINGS ARE CHARACTERIZED BY ARMA(1,1)
  !  TIME SERIES MODEL.  PERIODIC INCREMENT IS CONSIDERED TO BE DISTRIBUTED
  !  AS THE SUM OF IFINT RANDOM VALUES WITH VARIANCE-COVARIANCE MATRIX
  !  DEFINED BY THE SERIAL CORRELATION FUNCTION OF THE ARMA(1,1) MODEL.

  !OMMON

  ! Common blocks
  include 'PRGPRM.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PDEN.f90'
  include 'PLOT.f90'

  !OMMON

  ! Variables
  integer :: I,NEW,NOLD,NLIM,L,NBIG,NSML
  real    :: DT,T,COVAR,VAR,COV,VRNEXT

  if ( .not.LSTART ) go to 100

  !  INITIALIZE SERIAL CORRELATION FUNCTION FROM ARMA(1,1) PARAMETERS.

  BJRHO(1) = ( 1.-BJPHI*BJTHET)*(BJPHI-BJTHET)/ &
       & (1.+BJTHET*(BJTHET-2.*BJPHI))
  do I = 2, 40
     BJRHO(I) = BJRHO(I-1)*BJPHI
     50 continue
  end do

  !  PRECOMPUTE VARIANCE AND COVARIANCE MULTIPLIERS FOR "YR" YEARS.

  NEW = INT(YR)
  NOLD = INT(YR)
  go to 105
  100 continue
  NEW = IFINT
  NOLD = INT(OLDFNT)

  !  CALCULATE VARIANCE MULTIPLIER FOR CURRENT CYCLE.

  105 continue
  NLIM = NEW - 1
  VAR = 0.0
  do I = 1,NLIM
     VAR = VAR + BJRHO(I)*( NEW - I )
     110 continue
  end do
  VAR = NEW + 2.*VAR
  VRNEXT = VAR

  !  CALCULATE MULTIPLIER FOR COVARIANCE BETWEEN CURRENT AND PRECEDING
  !  CYCLES.

  COVAR = 0.0
  L = NEW + NOLD -1
  if ( L .gt. 40 )  L = 40

  !  PREPARE TO LOOP OVER LARGER DIMENSION OF COVARIANCE MATRIX.

  NBIG = NEW
  NSML = NOLD
  if ( NEW .gt. NOLD ) go  to 120
  NBIG = NOLD
  NSML = NEW
  120 continue
  T = 0.
  DT = 1.
  do I = 1,L
     T = T + DT
     COVAR = COVAR + BJRHO(I)*T
     if ( I .eq. NSML ) DT = 0.0
     if ( I .eq. NBIG ) DT = -1.
     130 continue
  end do
  COV = COVAR
  return
end subroutine AUTCOR
