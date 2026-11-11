!=================================!
!   Stand visualization: SVLNOL   !
!=================================!
! BASE $Id$
subroutine SVLNOL (XJ1,YJ1,XJ2,YJ2, &
     & XK1,YK1,XK2,YK2,XS,YS,KODE)
  implicit none

  !     STAND VISUALIZATION GENERATION
  !     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998

  !     RETURNS KODE = 0 IF THE LINES DON'T CROSS, 1 IF THEY DO.
  !     IF THEY DO, THEN XS,YS ARE RETURNED AS A LOCATION IN COMMON
  !     OTHERWISE XS,YS ARE BOTH UNDEFINED (SET TO ZERO).

  ! Variables
  real    :: XS,YS,YK2,XK2,YK1,XK1,YJ2,XJ2,YJ1,XJ1,BJ,AJ,BK,AK
  integer :: KODE,KODEJ,KODEK

  XS = 0.
  YS = 0.
  KODE = 0

  call SVDFLN (XJ1,YJ1,XJ2,YJ2,AJ,BJ,KODEJ)
  call SVDFLN (XK1,YK1,XK2,YK2,AK,BK,KODEK)

  !     IF EITHER LINE IS VERTICAL, THEN WE HAVE SOME SPECIAL
  !     CASES...

  if (KODEJ.eq.1 .and. KODEK.eq.1) then

     !        WE HAVE 2 VERTICAL LINES.  IF THE X'S ARE NOT EQUAL, THEN
     !        THEY ARE PARALLEL AND DON'T CROSS.

     if (ABS(XJ1-XK1) .gt. .000001) return

     !        THEY ARE THE SAME LINE...DO THE LINE SEGMENTS OVERLAP
     !        ALONG THE Y AXIS.

     call SVLSOL (YJ1,YJ2,YK1,YK2,YS,KODE)
     XS = XJ1
     return

     !     LINE J IS VERTICAL AND K IS NOT.

  else if (KODEJ.eq.1) then

     XS = XJ1
     YS = AK + (BK*XS)

     !     LINE K IS VERTICAL AND J IS NOT.

  else if (KODEK.eq.1) then

     XS = XK1
     YS = AJ + (BJ*XS)

     !     THE LINES HAVE THE SAME SLOPE.

  else if (ABS(BJ-BK).lt. .000001) then

     !        IF THEY HAVE DIFFERENT INTERCEPTS, THEN THEY HAVE NO
     !        POINTS IN COMMON...THEY DON'T CROSS.

     if (ABS(AJ-AK).gt. .000001) return

     !        THEY ARE THE SAME LINE...(THEY HAVE THE SAME SLOPE AND
     !        INTERCEPT) CHECK OF OVERLAP ALONG THE X AXIS...

     call SVLSOL (XJ1,XJ2,XK1,XK2,XS,KODE)
     if (KODE.eq.1) YS = AJ + (BJ*XS)
     return
  else

     XS = (AJ-AK)/(BK-BJ)
     YS = AJ+(BJ*XS)

  end if

  !     IS XS,YS IS ON LINE J BETWEEN XJ1,YJ1 & XJ2,YJ2?

  call SVONLN (XS,YS,XJ1,YJ1,XJ2,YJ2,KODE)
  if (KODE.eq.1) then

     !        IS XS,YS IS LINE K BETWEEN XK1,YK1 & XK2,YK2?

     call SVONLN (XS,YS,XK1,YK1,XK2,YK2,KODE)
     if (KODE.eq.1) return
  end if
  XS = 0.
  YS = 0.
  return
end subroutine SVLNOL
