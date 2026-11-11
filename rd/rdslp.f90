!================!
!   rd : rdslp   !
!================!
! RD $Id$
  ! Variables
real function RDSLP(X,XX,YY,N)
  implicit none

  !  Given two arrays XX and YY, each of dimension N, representing
  !  a series of line segments (XX strictly increasing), and an X-
  !  coordinate, return its Y-coordinate on the line.

  !  CALLED BY :
  !     RDGROW  [ROOT DISEASE]
  !     RDINOC  [ROOT DISEASE]
  !     RDSPRD  [ROOT DISEASE]
  !     ANCARY  [ROOT DISEASE]
  !     RDMORT  [ROOT DISEASE]

  !  CALLS     :
  !     NONE

  !  PARAMETERS :
  !     X      - (I ) X-coordinate of desired point.
  !     XX     - (I ) X points on series of line segments.
  !     YY     - (I ) Y points on series of line segments.
  !     N      - (I ) number of points in XX and YY arrays.

  !  Revision History:
  !    21-MAR-00 Lance David (FHTET)
  !       Reduced RETURN statements to 1 at the end of routine.
  !       Modified Debug code.
  !    06-AUG-01 Lance R. David (FHTET)
  !       Changed dimensions on XX and YY arrays from 5 to *.
  !   09/03/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  !.... PARAMETER INCLUDE FILES

  ! Common blocks
  include 'PRGPRM.f90'

  !.... COMMON INCLUDE FILES

  include 'CONTRL.f90'

  !.... Local variables

  logical :: DEBUG
  integer :: I,N
  real    :: X,XX(*),YY(*)

  !.... Check for DEBUG

  call DBCHK(DEBUG,'RDSLP',5,ICYC)

  if (X .lt. XX(1)) then
     RDSLP = YY(1)
  else if (X .gt. XX(N)) then
     RDSLP = YY(N)
  else
     I = 1
     10 if (X.gt. XX(I+1)) then
        I = I + 1
        goto 10
     end if

     !....    Now X in the range XX(I) to XX(I+1); find Y.

     RDSLP = YY(I) + (YY(I+1) - YY(I)) /&
          &(XX(I+1) - XX(I)) * (X-XX(I))
  end if

  if (DEBUG) then
     write (JOSTND,910) ' IN RDSLP:  N= ', N
     write (JOSTND,900) ' IN RDSLP: XX= ', (XX(I),I = 1,5), X
     write (JOSTND,900) ' IN RDSLP: YY= ', (YY(I),I = 1,5), RDSLP
     900 format (A15, 5F10.3, ';  -> ', F10.3)
     910 format (A15, I5)
  end if
  return
end
