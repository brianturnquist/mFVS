!==================================!
!   Load expression slope values   !
!==================================!
! BASE $Id$
function ALGSLP (XX,X,Y,N)
  implicit none

  !     PART OF THE PROGNOSIS MODEL FOR STAND DEVELOPMENT.
  !     N.L. CROOKSTON--INTERMOUNTAIN RESEARCH STATION--MOSCOW--AUG 1988

  !     THIS IS A LINEAR INTERPOLATION FUNCTION

  !     THE FUNCTION RETURNS THE VALUE OF A LINEAR SEGMENTED FUNCTION
  !     DEFINED BY THE PAIRS X(I), Y(I) EVALUATED AT X=XX FOR X
  !     INCLUDED IN THE INTERVAL {X(1),X(N)} IF XX < X(1), ALGSLP=X(1)
  !     AND IF XX >= X(N), ALGSLP=X(NN).

  !     XX = POINT AT WHICH THE FUNCTION IS TO BE EVALUATED.
  !     X  = ARRAY OF X(I), SEGMENT ENDPOINTS FOR X
  !     Y  = ARRAY OF Y(I), SEGMENT ENDPOINTS FOR Y
  !     N  = ARRAY SIZE OF X AND Y.
  !     ALGSLP=FUNCTION EVALUATED AT XX.

  ! Variables
  real    :: XX,ALGSLP
  integer :: N,NN,I
  real    :: X(N),Y(N)

  if (XX.lt.X(1)) then
     ALGSLP = Y(1)
  else
     if (XX.ge.X(N)) then
        ALGSLP = Y(N)
     else
        NN = N-1
        do I = 1,NN
           if (XX.ge.X(I+1)) goto 30
           ALGSLP = Y(I)+((Y(I+1)-Y(I))/(X(I+1)-X(I)))*(XX-X(I))
           goto 50
           30 continue
        end do
        50 continue
     end if
  end if
  return
end function ALGSLP
