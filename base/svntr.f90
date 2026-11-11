!================================!
!   Stand visualization: SVNTR   !
!================================!
! BASE $Id$
subroutine SVNTR (XP,NP)
  implicit none

  !     STAND VISUALIZATION GENERATION
  !     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998

  !     GIVEN XP, THE NUMBER OF TREES PER ACRE,
  !     RETURN NP, THE NUMBER OF TREE RECORDS TO PROCESS

  ! Variables
  real    :: XX,XP,X
  integer :: NP
  NP = 0
  XX = XP
  if (XX.gt.1.) then
     NP = IFIX(XP)
     XX = XX-FLOAT(NP)
  end if
  if (XX.gt. .00001) then
     call SVRANN(X)
     if (X.lt.XX) NP = NP+1
  end if
  return
end subroutine SVNTR
