!==============================!
!   Check debugging requests   !
!==============================!
! BASE $Id$
subroutine DBCHK (LDEBG,SUBIN,NC,ICYC)
  implicit none

  !     CALLED TO FIND OUT IF 'SUBIN' IS BEING DEBUGED DURING CYCLE
  !     'ICYC'.  LDEBG IS RETURNED TRUE IF YES, FALSE OTHERWISE.  NC
  !     IS THE NUMBER OF CHARACTER IN STRING SUBIN.

  ! Common blocks
  include 'DBSTK.f90'

  ! Variables
  character(len=*) :: SUBIN
  integer :: ICYC,NC
  logical :: LDEBG

  LDEBG = .false.
  if (ITOP.gt.0) then
     call DBSCAN (LDEBG,ALLSUB,6,0)
     if (.not.LDEBG) call DBSCAN (LDEBG,ALLSUB,6,ICYC)
     if (.not.LDEBG) call DBSCAN (LDEBG,SUBIN,NC,ICYC)
  end if
  return
end subroutine DBCHK
