!=================================!
!   Stand visualization: SVOBOL   !
!=================================!
! BASE $Id$
subroutine SVOBOL (J,XJ1,YJ1,XJ2,YJ2, &
     & K,XK1,YK1,XK2,YK2,XS,YS,KODE)
  implicit none

  !     STAND VISUALIZATION GENERATION
  !     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998

  !     RETURN KODE = 1 IF OBJ J OVERLAPS OBJ K
  !     RETURN KODE = 0 IF NOT

  !     IF BOTH OBJECT TYPES ARE LINES, THEN XS,YS, IS RETURNED
  !     AS THE LOCATION WHERE THE LINES CROSS. OTHERWISE XS,YS
  !     ARE BOTH SET TO ZERO AND SHOULD BE CONSIDERED UNDERFINED.

  !     J AND K ARE OBJECT TYPES.  1 FOR CIRCLES, 2 FOR LINES
  !     IF THE OBJECT IS A CIRCLE, THEN "X2" IS THE RADIUS
  !                                 AND "Y2" IS NOT USED,
  !     IF THE OBJECT IS A LINE, THEN "X2,Y2" IS IT'S OTHER END.

  ! Variables
  real    :: XS,YS,YK2,XK2,YK1,XK1,YJ1,XJ1,YJ2,XJ2
  integer :: KODE,K,J
  XS = 0.
  YS = 0.

  if (J.eq.1 .and. K.eq.1) then

     !        BOTH ARE CIRCLES.

     call SVCROL (XJ1,YJ1,XJ2,XK1,YK1,XK2,KODE)

  else if (J.eq.1 .and. K.eq.2) then

     !        CIRCLE AND LINE

     call SVLCOL (XJ1,YJ1,XJ2,XK1,YK1,XK2,YK2,KODE)

  else if (J.eq.2 .and. K.eq.1) then

     !        LINE AND CIRCLE

     call SVLCOL (XK1,YK1,XK2,XJ1,YJ1,XJ2,YJ2,KODE)

  else if (J.eq.2 .and. K.eq.2) then

     !        LINE AND LINE

     call SVLNOL (XJ1,YJ1,XJ2,YJ2,XK1,YK1,XK2,YK2,XS,YS,KODE)

  end if
  return
end subroutine SVOBOL
