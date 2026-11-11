!====================================!
!   Set debugging for all routines   !
!====================================!
! BASE $Id$
subroutine DBALL (ICYC)
  implicit none

  ! Common blocks
  include 'PRGPRM.F77'
  include 'DBSTK.F77'

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
