!=================================!
!   Stand visualization: SVCMP1   !
!=================================!
! BASE $Id$
subroutine SVCMP1
  implicit none

  !     STAND VISUALIZATION GENERATION
  !     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998

  !     INITIALIZE THE TREE STATUS CODES SO THEY CAN HOLD RE-
  !     REFERENCING DATA.

  !     CALLED FROM COMPRS AND TREDEL.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'SVDATA.f90'
  include 'CONTRL.f90'
  include 'WORKCM.f90'

  ! Variables
  integer :: IWKCM1(MAXTRE)
  equivalence (IWKCM1,WORK1)
  integer :: I

  if (JSVOUT.eq.0) return
  if (NSVOBJ.eq.0) return
  if (ITRN.eq.0) return
  do I = 1,ITRN
     IWKCM1(I) = 0
  end do
  return
end subroutine SVCMP1
