!===================================!
!   Initialize debugging controls   !
!===================================!
! BASE $Id$
subroutine DBINIT
  implicit none

  ! Common blocks
  include 'DBSTK.F77'

  ALLSUB = '$**SUB'
  MAXTOP = 255
  ITOP = 0
  MAXLEN = 20
  return
end subroutine DBINIT
