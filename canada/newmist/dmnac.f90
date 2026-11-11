!============================!
!   canada/newmist : dmnac   !
!============================!
! CANADA-NEWMIST $Id$
subroutine DMNAC(A,B)
  implicit none

  ! **DMNAC -- NISI  Date of last revision: January 10, 2002

  ! Purpose:
  !     set the SF() based on alpha, beta

  ! Called by:

  !     DMOPTS
  !     DMTREG
  !     PPDMTREG

  ! Argument list definitions:

  !     REAL      A         ALPHA TERM
  !     REAL      B         BETA TERM

  ! Local variable definitions:

  !     INTEGER   i         Loop counter for various things: options,
  !                          trees, plots and DM categories.
  !     INTEGER   j         Loop counter for sampling rings.
  !     REAL      x         Scalar used in calculation of sample ring
  !                          modified density; also used to hold
  !                          trees/acre density of individual records.
  !     REAL      tmp       Temporary calculation for sample ring
  !                          density scaling.
  !     REAL      PltDns   Dnsty (trees/acre) for each sample point.

  ! Common block variables and parameters:

  !     MXTHRX    DMCOM
  !     DMALPH    DMCOM
  !     DMBETA    DMCOM
  !     SF        DMCOM

  ! Common blocks
  include 'PRGPRM.f90'
  include 'DMCOM.f90'

  !     LOCAL VARIABLES.

  ! Variables
  integer :: I,J
  real    :: X,A,B,TMP(MXTHRX)

  do J = 1, MXTHRX
     TMP(J) = EXP(DSTNCE(J) * B)
  end do

  do I = 0,6
     X = FLOAT(I) * A
     do J = 1,MXTHRX
        SF(I, J) = EXP(X * TMP(J))
     end do
  end do

  return
end
