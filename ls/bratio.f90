!=========================!
!   Compute bark ratios   !
!=========================!
! LS $Id$
function BRATIO(IS,D,H)
  implicit none

  ! FUNCTION TO COMPUTE BARK RATIOS. THIS ROUTINE IS VARIANT SPECIFIC
  ! AND EACH VARIANT USES ONE OR MORE OF THE ARGUMENTS PASSED TO IT.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'COEFFS.f90'

  ! Variables
  integer :: IS
  real    :: H,D,BRATIO
  real    :: RDANUW

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = H
  RDANUW = D

  if (IS .eq.0) then
     BRATIO = 0.93
  else
     BRATIO = BKRAT(IS)
  end if
  return
end
