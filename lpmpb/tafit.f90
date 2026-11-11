!===================!
!   lpmpb : tafit   !
!===================!
! LPMPB $Id$
function TAFIT (X, Y)
  implicit none

  !     PART OF THE MOUNTAIN PINE BEETLE EXTENSION OF PROGNOSIS SYSTEM.

  !        TAFIT FITS A CURVE OF THE FORM Y = AX**2 + BX + C TO ANY
  !        THREE POINTS.

  !        X'S CORRESPOND TO TA'S, X(1) BEING THE ORIGINAL
  !        ESTIMATE OF TA.

  !        Y'S CORRESPOND TO CUMULATIVE SURFACE KILLED IN THE THREE
  !        TRIAL RUNS.

  ! Revision History
  !   02/08/88 Last noted revision date.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'MPBCOM.f90'

  ! Variables
  integer :: I
  real    :: A,B,C,DISCRM,TAFIT,X(3),X0,X01,X02,XAVE,XSQR(3),Y(3),Y0

  data X01 /-999./, X02 /-999./
  do I = 1,3
     XSQR(I) = X(I)**2
     10 continue
  end do

  Y0 = SADLPP

  A = (Y(1) - Y(2))/((X(1) - X(3))*(X(1) - X(2))) -&
       &(Y(2) - Y(3))/((X(2) - X(3))*(X(1) - X(3)))

  B = (Y(2) - Y(3) - A*(XSQR(2) - XSQR(3)))/(X(2) - X(3))

  C = Y(1) - A*XSQR(1) - B*X(1)

  if (A .eq.0.) go to 30

  DISCRM = B**2 - 4.*A*(C - Y0)
  if (DISCRM .lt.0.) go to 20

  X01 = (-B + SQRT(DISCRM))/(2.*A)
  X02 = (-B - SQRT(DISCRM))/(2.*A)

  !     ** CHOOSE X0 AND CALCULATE TAFIT

  X0 = X01
  if (ABS(TAMID-X01).gt.ABS(TAMID-X02)) X0 = X02
  !     -- CHOOSE SOLUTION ON SIDE OF CURVE MOST WITHIN RANGE
  !     -- NOTE: PROBABLY ALWAYS CHOOSES X02 (LEFT SIDE)
  go to 40

  !     **  IMAGINARY SOLUTION -- USE INFLECTION POINT OF CURVE...
  !     POINT WHERE FIRST DERIVATIVE IS ZERO.
  20 continue
  X0 = -B/(A+A)
  go to 40

  !     ** LINEAR FUNCTION (A = 0.)
  30 continue
  X0 = (Y0 - C)/B

  !     ** USE AVERAGE OF ORIGINAL ESTIMATION AND FITTED VALUE
  40 continue
  XAVE = X0
  if (DEBUIN) write (JOMPB,100) A,B,C,DISCRM, XAVE,X0,X01,X02
  100 format (//,'A,B,C,DISCRM =',1P4G12.4,'  XAVE,X0,X01,X02 =',4G12.4)
  TAFIT = XAVE
  if (TAFIT .lt. TAMIN) TAFIT = TAMIN
  if (TAFIT .gt. TAMAX) TAFIT = TAMAX

  return
end
