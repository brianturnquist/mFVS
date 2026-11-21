!===============================!
!   Initialize activity links   !
!===============================!
! BASE $Id$
subroutine LNKINT
  use VARCFG_MOD, only: CFG_MAXSP
  implicit none

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'

  ! Variables
  integer :: I

  NUMSP = 0
  do I = 1,CFG_MAXSP
     ISCT(I,1) = 0
     ISCT(I,2) = 0
     IBEGIN(I) = 0
     IREF(I) = 0
     KOUNT(I) = 0
     20 continue
  end do
  return
end subroutine LNKINT
