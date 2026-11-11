!=================================!
!   Stand visualization: SVLCOL   !
!=================================!
! BASE $Id$
subroutine SVLCOL (X,Y,R,X1,Y1,X2,Y2,KODE)
  implicit none

  !     STAND VISUALIZATION GENERATION
  !     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998

  !     RETURN KODE = 0 IF THE CIRCLE AND LINE DO NOT OVERLAP
  !     RETURN KODE = 1 IF THEY DO.

  ! Variables
  integer :: KODE
  real    :: Y1,Y2,X1,X2,R,X,Y,B1,A1,B,A,XS,YS

  !     COMPUTE THE FORMULA FOR THE LINE.

  call SVDFLN (X1,Y1,X2,Y2,A1,B1,KODE)

  !     CASE 1: KODE = 1 IF THE SLOPE IS INFINITE.

  if (KODE.eq.1) then

     !        THE CIRCLE AND LINE INTERSECT IF X1 IS LESS THAN R FROM
     !        THE LINE.

     if (ABS(X1-X) .lt. R) then
        KODE = 1
     else
        KODE = 0
     end if

     !     CASE 2: IF THE SLOPE IS ZERO.

  else if (ABS(B1) .le. .000001) then

     if (ABS(Y1-Y) .lt. R) then
        KODE = 1
     else
        KODE = 0
     end if

  else

     !     CASE 3: SLOPE IS NON ZERO AND NOT INFINITE.

     !        DEFINE A PERPENDICULAR LINE THAT IS THROUGH THE CENTER OF THE
     !        CIRCLE.

     B = -1.0/B1
     A = Y-(B*X)

     !        FIND THE POINT WHERE THE TWO LINES CROSS.

     XS = (A-A1)/(B1-B)
     YS = A+(B*XS)

     !        IF XS,YS IS CLOSER THAN R AWAY FROM X,Y THEN THE LINE BISECTS
     !        THE CIRCLE.

     if ( ((X-XS)**2)+((Y-YS)**2) .lt. R*R) then
        KODE = 1
     else
        KODE = 0
     end if
  end if
  return
end subroutine SVLCOL
