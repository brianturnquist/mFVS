!=================================!
!   Stand visualization: SVDFLN   !
!=================================!
! BASE $Id$
subroutine SVDFLN (X1,Y1,X2,Y2,A,B,KODE)
  implicit none

  !     STAND VISUALIZATION GENERATION
  !     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998

  !     RETURNS A AND B FOR THE FORMULA Y=A+BX OF THE LINE
  !     KODE = 0 IF OK, 1 = INFINITE SLOPE, NO INTERCEPT

  ! Variables
  real    :: X1,Y1,X2,Y2,A,B
  integer :: KODE
  A = 0.
  B = 0.
  KODE = 0

  !     IF THE Y'S ARE EQUAL, THEN WE HAVE 0 SLOPE.  THE INTERCEPT
  !     IS Y1, SLOPE IS SET TO 0.

  if (ABS(Y1-Y2) .lt. .000001) then
     A = Y1
     return
  end if

  !     IF THE X'S ARE EQUAL, THEN WE HAVE INFINITE SLOPE AND THE
  !     INTERCEPT IS UNDEFINED.

  if (ABS(X1-X2) .lt. .000001) then
     KODE = 1
     return
  end if

  B = (Y2-Y1) / (X2-X1)

  A = Y1 - (B*X1)

  return
end subroutine SVDFLN
