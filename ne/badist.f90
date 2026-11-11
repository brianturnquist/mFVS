!=================!
!   ne : badist   !
!=================!
! NE $Id$
subroutine BADIST(DEBUG)
  implicit none

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'TWIGCOM.f90'

  ! Variables
  logical :: DEBUG

  integer :: I,J,ICLS
  real    :: TDBH,TREEBA
  logical :: LDANUW

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  LDANUW = DEBUG

  ! Initialize variables
  do J = 1,50
     EBAU(J) = 0.0
     5 continue
  end do

  !  COMPUTE BA BY DBH CLASS, BA BY SPECIES, AND BA BY DBH CLASS BY
  !  SPECIES.

  do I = 1,ITRN
     ICLS = IFIX(DBH(I) + 1.0)
     if (ICLS .gt.50) ICLS = 50
     TDBH = DBH(I)
     if (TDBH .lt.1.0) TDBH = 1.0
     TREEBA = 0.0054542 * TDBH * TDBH * PROB(I)
     EBAU(ICLS) = EBAU(ICLS) + TREEBA
     100 continue
  end do

  do I = 49,1,-1
     EBAU(I) = EBAU(I+1)+EBAU(I)
     150 continue
  end do

  return
end
