!===================!
!   lpmpb : ptary   !
!===================!
! LPMPB $Id$
subroutine PTARY(IPLTNO,XAXIS,SAV)
  implicit none

  !     LIBRARY ROUTINE TO SPOOL PLOT OUTPUT

  !     THIS ROUTINE MAY BE USED TO SAVE AN ARRAY OF UP
  !     TO TEN VARIABLES TO BE PLOTTED.

  !     THE ACTUAL NUMBER OF VARIABLES USED IS DETERMINED
  !     BY IPTVAR(IPLTNO) AS DEFINED BY THE INITIAL
  !     CALL(S) TO PTGRP.

  ! Revision History
  !   02/16/88 Last noted revision date.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  ! Common blocks
  include 'PT.f90'

  ! Variables
  integer :: I,ILUN,IPLTNO,J,N
  real    :: SAV(10),XAXIS

  !             BLOCK OF 10 PLOT VARIABLES TO BE SPOOLED

  !        UPDATE MAX AND MIN
  N = IPTVAR(IPLTNO)
  do J = 1,N
     if (SAV(J) .gt. PTMAX(IPLTNO,J)) PTMAX(IPLTNO,J) = SAV(J)
     if (SAV(J) .lt. PTMIN(IPLTNO,J)) PTMIN(IPLTNO,J) = SAV(J)
     20 continue
  end do

  IPTSPL(IPLTNO) = IPTSPL(IPLTNO)+1
  ILUN = 19+IPLTNO
  write(ILUN) XAXIS,(SAV(I),I = 1,10)

  return
end
