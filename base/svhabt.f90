!=================================!
!   Stand visualization: SVHABT   !
!=================================!
! BASE $Id$
subroutine SVHABT(XMOD)
  implicit none

  !     STAND VISUALIZATION GENERATION
  !     J.J.MARCHINEK -- RMRS MOSCOW -- JUNE 1998

  !     USED FOR PROCESSING HABITAT INFORMATION FOR SNAGS

  ! Common blocks
  include 'PRGPRM.f90'
  include 'SVDEAD.f90'

  ! Variables
  real    :: XMOD
  XMOD = 1.

  return
end subroutine SVHABT
