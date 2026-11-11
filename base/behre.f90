!================================!
!   Compute Behre taper volume   !
!================================!
! BASE $Id$
function BEHRE(L1,L2)
  implicit none

  !  THIS FUNCTION CALCULATES THE VOLUME OF A SOLID OF REVOLUTION
  !  DESCRIBED BY A BEHRE TAPER CURVE USING PARAMETERS 'AHAT' AND 'BHAT'.
  !  THE LIMITS OF INTEGRATION ARE 'L1' AND 'L2'.

  !  IT SHOULD BE NOTED THAT THE VOLUME CALCULATED IS OFF BY A FACTOR
  !  OF PI/(A CUBED) - THIS VALUE WOULD CANCEL OUT IN THE RATIOS OF
  !  BEHRE VOLUMES USED IN CALLING ROUTINE 'VOLS'.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'COEFFS.f90'

  ! Variables
  real    :: L1,L2,BEHRE,ALB1,ALB2

  ALB1 = AHAT*L1 + BHAT
  ALB2 = AHAT*L2 + BHAT
  BEHRE = ALB2 - ALB1 - 2.0*BHAT*(ALOG(ALB2) - ALOG(ALB1)) &
       & - BHAT*BHAT/ALB2 + BHAT*BHAT/ALB1
  return
end function BEHRE
