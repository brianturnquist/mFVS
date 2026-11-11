!===================!
!   lpmpb : pmslp   !
!===================!
! LPMPB $Id$
function PMSLP(XX,X,Y,N)
  implicit none

  dimension X(N),Y(N)

  !.....A LINEAR-INTERPOLATION FUNCTION

  !     SUPPLIED WITH THE BWMOD AS WRITTEN AT THE MODELING
  !     WORKSHOP.  THE NAME IS CHANGED FROM 'SLP' TO 'PMSLP'.

  !     PART OF THE MOUNTAIN PINE BEETLE EXTENSION OF THE PROGNOSIS
  !     SYSTEM. INT-MOSCOW FORESTRY SC. LAB. DEC. 1980

  ! Revision History
  !   02/08/88 Last noted revision date.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  ! Variables
  integer :: I,N,NN
  real    :: PMSLP,X,XX,Y

  NN = N-1
  do I = 1,NN
     if (XX.lt.X(I).or.XX.gt.X(I+1)) go to 30
     PMSLP = Y(I)+((Y(I+1)-Y(I))/(X(I+1)-X(I)))*(XX-X(I))
     return
     30 continue
  end do
  PMSLP = Y(N)
  return
end
