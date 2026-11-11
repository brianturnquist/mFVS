!====================================!
!   Set debugging for all routines   !
!====================================!
! BASE $Id$
subroutine DBALL (ICYC)
  implicit none

  ! Common blocks
  include 'PRGPRM.f90'
  include 'DBSTK.f90'

  ! Variables
  integer :: ICYC,IRC

  if ((ICYC.lt.1).or.(ICYC.gt.MAXCYC)) then
     call DBINIT
     call DBADD (ALLSUB,6,0,IRC)
  else
     call DBADD (ALLSUB,6,ICYC,IRC)
  end if
  return
end subroutine DBALL
