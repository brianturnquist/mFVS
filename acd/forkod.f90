!============================!
!   Translate forest codes   !
!============================!
! ACD $Id$
subroutine FORKOD
  implicit none

  !     TRANSLATES FOREST CODE INTO A SUBSCRIPT, IFOR, AND IF
  !     KODFOR IS ZERO, THE ROUTINE RETURNS THE DEFAULT CODE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.F77'
  include 'CONTRL.F77'

  !  NATIONAL FORESTS:
  !  914 = WAYNE
  !  922 = WHITE MOUNTAIN
  !  919 = ALLEGHENY
  !  920 = GREEN MTN - FINGER LAKES
  !  921 = MONONGAHELA
  !  911 = OLD WAYNE-HOOSIER (MAPE TO WAYNE)
  !  930 = FINGER LAKES (MAP TO GREEN MTN - FINGER LAKES)

  ! Variables
  integer :: JFOR(7),KFOR(7),NUMFOR,I
  data JFOR/914,922,919,920,921,911,930/
  data NUMFOR/7/
  data KFOR/7*1/

  if (KODFOR .eq. 0) goto 30
  do I = 1,NUMFOR
     if (KODFOR .eq. JFOR(I)) goto 20
     10 continue
  end do
  call ERRGRO (.true.,3)
  write(JOSTND,11) JFOR(IFOR)
  11 format(T12,'FOREST CODE USED FOR THIS PROJECTION IS',I4)
  goto 30
  20 continue
  if (I .eq. 6) then
     write(JOSTND,21)
     21 format(T12,'WAYNE-HOOSIER NF (911) BEING MAPPED TO WAYNE ', &
          & '(914) FOR FURTHER PROCESSING.')
     I = 1
  else if (I .eq. 7) then
     write(JOSTND,22)
     22 format(T12,'FINGER LAKES NF (930) BEING MAPPED TO GREEN MTN-', &
          & 'FINGER LAKES (920) FOR FURTHER PROCESSING.')
     I = 4
  end if
  IFOR = I
  IGL = KFOR(I)
  30 continue
  KODFOR = JFOR(IFOR)

  !  SET DEFAULT TLAT, TLONG, AND ELEVATION VALUES, BY FOREST

  select case(KODFOR)
  case(914)
  if (TLAT.eq.0) TLAT = 39.33
  if (TLONG.eq.0)TLONG = 82.10
  if (ELEV.eq.0) ELEV = 9.
  case(919)
  if (TLAT.eq.0) TLAT = 41.84
  if (TLONG.eq.0)TLONG = 79.15
  if (ELEV.eq.0) ELEV = 17.
  case(920)
  if (TLAT.eq.0) TLAT = 43.61
  if (TLONG.eq.0)TLONG = 72.97
  if (ELEV.eq.0) ELEV = 19.
  case(921)
  if (TLAT.eq.0) TLAT = 38.93
  if (TLONG.eq.0)TLONG = 79.85
  if (ELEV.eq.0) ELEV = 30.
  case(922)
  if (TLAT.eq.0) TLAT = 43.53
  if (TLONG.eq.0)TLONG = 71.47
  if (ELEV.eq.0) ELEV = 20.
  end select
  return
end subroutine FORKOD
