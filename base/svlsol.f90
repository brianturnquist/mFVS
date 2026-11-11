!=================================!
!   Stand visualization: SVLSOL   !
!=================================!
! BASE $Id$
subroutine SVLSOL (XJ1,XJ2,XK1,XK2,XS,KODE)
  implicit none

  !     STAND VISUALIZATION GENERATION
  !     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998

  !     RETURN KODE = 1 IF THE 2 SEGMENTS OF THE SAME LINE OVERLAP
  !     RETURN KODE = 0 IF THE 2 SEGMENTS OF THE SAME LINE DO NOT OVERLAP

  !     RETURN XS IS ONE POINT WITHIN THE OVERLAPING SEGMENTS, UNDEFINED
  !     (SET TO ZERO) IF THE SEGMENTS DO NOT OVERLAP

  ! Variables
  integer :: KODE
  real    :: XJ1,XJ2,XK1,XK2,XS

  KODE = 1

  !     FIND THE LONGEST LINE.

  if (ABS(XJ2-XJ1) .gt. ABS(XK2-XK1)) then

     !        LINE J IS LONGEST.

     if (XJ1.lt.XJ2) then
        XS = XK1
        if (XJ1.le.XK1 .and. XK1.le.XJ2) return
        XS = XK2
        if (XJ1.le.XK2 .and. XK2.le.XJ2) return
     else
        XS = XK1
        if (XJ2.le.XK1 .and. XK1.le.XJ1) return
        XS = XK2
        if (XJ2.le.XK2 .and. XK2.le.XJ1) return
     end if
  else

     !        LINE K IS LONGEST.

     if (XK1.lt.XK2) then
        XS = XJ1
        if (XK1.le.XJ1 .and. XJ1.le.XK2) return
        XS = XJ2
        if (XK1.le.XJ2 .and. XJ2.le.XK2) return
     else
        XS = XJ1
        if (XK2.le.XJ1 .and. XJ1.le.XK1) return
        XS = XJ2
        if (XK2.le.XJ2 .and. XJ2.le.XK1) return
     end if
  end if
  XS = 0.
  KODE = 0
  return
end subroutine SVLSOL
