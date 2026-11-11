!===================!
!   lpmpb : ptsav   !
!===================!
! LPMPB $Id$
subroutine PTSAV(IPLTNO,XAXIS,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9,Y10)
  implicit none

  !     LIBRARY ROUTINE TO SPOOL PLOT OUTPUT

  ! Revision History
  !   02/08/88 Last noted revision date.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  ! Common blocks
  include 'PT.f90'

  ! Variables
  integer :: ILUN,IPLTNO,J,N
  real    :: SAV,XAXIS,Y1,Y10,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9

  dimension SAV(10)
  !             BLOCK OF 10 PLOT VARIABLES TO BE SPOOLED

  SAV(1) = Y1
  SAV(2) = Y2
  SAV(3) = Y3
  SAV(4) = Y4
  SAV(5) = Y5
  SAV(6) = Y6
  SAV(7) = Y7
  SAV(8) = Y8
  SAV(9) = Y9
  SAV(10) = Y10

  !        UPDATE MAX AND MIN
  N = IPTVAR(IPLTNO)
  do J = 1,N
     if (SAV(J) .gt. PTMAX(IPLTNO,J)) PTMAX(IPLTNO,J) = SAV(J)
     if (SAV(J) .lt. PTMIN(IPLTNO,J)) PTMIN(IPLTNO,J) = SAV(J)
     1 continue
  end do

  IPTSPL(IPLTNO) = IPTSPL(IPLTNO)+1
  ILUN = 8+IPLTNO
  write(ILUN) XAXIS,SAV

  return
end
