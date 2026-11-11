!===================================!
!   Compute basal area distribution  !
!===================================!
! ACD $Id$
subroutine BADIST(DEBUG)
  implicit none

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'TWIGCOM.f90'

  ! Arguments
  logical :: DEBUG

  ! Variables
  integer :: I,J,ICLS
  real    :: TDBH,TREEBA

  ! Initialize basal area by diameter class
  do J = 1,50
     BAU(J) = 0.0
  end do

  ! Accumulate basal area by diameter class
  do I = 1,ITRN
     ICLS = IFIX(DBH(I) + 1.0)
     if (ICLS.gt.50) ICLS = 50
     TDBH = DBH(I)
     if (TDBH.lt.1.0) TDBH = 1.0
     TREEBA = 0.0054542 * TDBH * TDBH * PROB(I)
     BAU(ICLS) = BAU(ICLS) + TREEBA
  end do

  ! Sum basal area in this diameter class and all larger classes
  do I = 49,1,-1
     BAU(I) = BAU(I+1) + BAU(I)
  end do

  return
end subroutine BADIST
