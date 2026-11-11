!==========================!
!   Link activity chains   !
!==========================!
! BASE $Id$
subroutine LNKCHN (ITREE)
  implicit none

  ! Common blocks
  include 'PRGPRM.F77'
  include 'ARRAYS.F77'
  include 'CONTRL.F77'
  include 'PLOT.F77'

  ! Variables
  integer :: ITREE,II,KPTRII,IR

  II = ISP(ITREE)
  if (IBEGIN(II) .ne. 0) go to 30
  IBEGIN(II) = ITREE
  NUMSP = NUMSP + 1
  IUSED(NUMSP) = NSP(II,1)
  IREF(II) = NUMSP
  go to 40
  30 continue
  KPTRII = KPTR(II)
  IND2(KPTRII) = ITREE
  40 continue
  KPTR(II) = ITREE
  IR = IREF(II)

  !     COUNT NUMBER OF TREE RECORDS PER SPECIES

  KOUNT(IR) = KOUNT(IR) + 1

  return
end subroutine LNKCHN
