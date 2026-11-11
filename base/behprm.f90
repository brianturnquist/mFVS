!====================================!
!   Compute Behre taper parameters   !
!====================================!
! BASE $Id$
subroutine BEHPRM (VMAX,D,H,BARK,LCONE)
  implicit none

  !  SUBROUTINE TO COMPUTE THE TREE SPECIFIC PARAMETERS FOR THE
  !  BEHRE HYPERBOLA   (I.E. AHAT, BHAT, LCONE)

  ! Common blocks
  include 'PRGPRM.f90'
  include 'COEFFS.f90'

  ! Variables
  logical :: LCONE
  real    :: BARK,H,D,VMAX

  LCONE = .false.

  !  BHAT IS INITIALIZED WITH VALUE OF CYLINDRICAL FORM FACTOR.

  BHAT = VMAX / (.00545415*D*D*BARK*BARK*H)
  if (BHAT.gt.0.95) BHAT = 0.95

  !     AHAT = AN ESTIMATE OF THE BEHRE TAPER CURVE PARAMETER 'A'

  AHAT = .44277 - .99167/BHAT - 1.43237*ALOG(BHAT) &
       & + 1.68581*SQRT(BHAT) - .13611*BHAT*BHAT
  if (ABS(AHAT) .lt. 0.05) then
     LCONE = .true.
     if (AHAT.lt. 0.) then
        AHAT = -0.05
     else
        AHAT = 0.05
     end if
  end if
  BHAT = 1.-AHAT
  if (BHAT .lt. 0.0001) BHAT = 0.0001
  return
end subroutine BEHPRM
