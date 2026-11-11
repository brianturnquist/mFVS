!================================!
!   Set simulator return codes   !
!================================!
! BASE $Id$
subroutine RCDSET (IC,LRETRN)
  implicit none

  !     SETS THE RETURN CODE FOR THE PROGNOSIS MODEL.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'

  ! Variables
  integer :: IC
  logical :: LRETRN

  if (IC.gt.ICCODE) ICCODE = IC
  if (.not.LRETRN) call fvsSetRtnCode (1)
  return
end subroutine RCDSET
