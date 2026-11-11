!=================================!
!   Stand visualization: SVCMP2   !
!=================================!
! BASE $Id$
subroutine SVCMP2(ITARG,ISOUR)
  implicit none

  !     STAND VISUALIZATION GENERATION
  !     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998

  !     REGISTER MOVEMENTS OF TREE RECORDS WITH THE VISUALIZATION DATA

  !     INPUT:
  !     ITARG IS A TARGET LOCATION TO WHICH A TREE WILL BE PLACED
  !     ISOUR IS THE INDEX OF THE TREE BEING MOVED.

  !     CALLED FROM COMPRS AND TREDEL.

  ! Common blocks
  include 'PRGPRM.F77'
  include 'SVDATA.F77'
  include 'CONTRL.F77'
  include 'WORKCM.F77'

  ! Variables
  integer :: ISOUR,ITARG
  integer :: IWKCM1(MAXTRE)
  equivalence (IWKCM1,WORK1)

  if (JSVOUT.eq.0) return

  IWKCM1(ISOUR) = ITARG
  return
end subroutine SVCMP2
