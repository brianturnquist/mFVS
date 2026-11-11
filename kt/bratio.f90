!=========================!
!   Compute bark ratios   !
!=========================!
! KT $Id$
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

  BRATIO = BKRAT(IS)
  return
end
