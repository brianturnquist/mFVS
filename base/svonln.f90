!=================================!
!   Stand visualization: SVONLN   !
!=================================!
! BASE $Id$
subroutine SVONLN (X,Y,X1,Y1,X2,Y2,KODE)
  implicit none

  !     STAND VISUALIZATION GENERATION
  !     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998

  !     RETURN KODE = 0 IF X,Y IS NOT WITHIN THE INTERVAL (X1,Y1),(X2,Y2)
  !     RETURN KODE = 1 IF THIS POINT IS WITHIN THE INTERVAL.

  !     THIS ROUTINE ONLY WORKS IF (X,Y) IS ON THE LINE...IT ONLY
  !     CHECKS IF THE POINT IS ON THE INTERVAL...INCLUDING THE
  !     ENDPOINTS.

  ! Variables
  integer :: KODE
  real    :: X,Y,X1,Y1,X2,Y2
  KODE = 0
  if (X1.lt.X2) then
     if (X1.le.X .and. X.le.X2) goto 10
  else
     if (X2.le.X .and. X.le.X1) goto 10
  end if
  return

  10 continue
  if (Y1.lt.Y2) then
     if (Y1.le.Y .and. Y.le.Y2) goto 20
  else
     if (Y2.le.Y .and. Y.le.Y1) goto 20
  end if

  return
  20 continue
  KODE = 1
  return
end subroutine SVONLN
