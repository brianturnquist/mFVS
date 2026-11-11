!========================!
!   canada/on : balmod   !
!========================!
! CANADA-ON $Id$
subroutine BALMOD(ISPC,D,BA,RMSQD,GM,DEBUG)
  implicit none

  !  THIS SUBROUTINE COMPUTES THE VALUE OF A GROWTH MODIFIER BASED
  !  ON BAL. ORIGINALLY THIS WAS JUST PART OF THE LARGE TREE DIAMETER
  !  GROWTH SEQUENCE. HOWEVER, THERE NEEDS TO BE A SIMILAR ACCOUNTING
  !  OF STAND POSITION IN THE LARGE TREE AND SMALL TREE HEIGHT GROWTH
  !  ESTIMATION SEQUENCE. THIS ROUTINE IS CALLED BY DGF, HTGF, AND
  !  RGNTHW.

  !  ON VERSION: this is currently not required

  ! Variables
  logical :: DEBUG
  integer :: ISPC
  real    :: D,BA,RMSQD,GM

  GM = 1.0
  return
end
