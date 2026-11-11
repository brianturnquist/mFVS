!===================================!
!   Initialize debugging controls   !
!===================================!
! BASE $Id$
subroutine DBINIT
  implicit none

  ! Common blocks
  include 'DBSTK.f90'

  ALLSUB = '$**SUB'
  MAXTOP = 255
  ITOP = 0
  MAXLEN = 20
  return
end subroutine DBINIT
