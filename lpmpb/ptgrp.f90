!===================!
!   lpmpb : ptgrp   !
!===================!
! LPMPB $Id$
subroutine PTGRP (IPLTNO,IVAR,ICODE,BELOW,ABOVE)
  implicit none

  !     ROUTINE TO SPECIFY GROUP OF VALUES TO BE PLOTTED ON
  !     SAME SCALE, AND WHICH BOUNDS NEED TO BE CALCULATED.

  !     ICODE-- 00  NO BOUNDS SPECIFIED, SET BOTH
  !             11  BOTH BOUNDS SPECIFIED, SET NEITHER
  !             10  LOWER BOUND SPECIFIED, SET UPPER
  !             01  UPPER BOUND SPECIFIED, SET LOWER

  ! Revision History
  !   02/08/88 Last noted revision date.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  ! Common blocks
  include 'PT.f90'

  ! Variables
  real    :: ABOVE,BELOW
  integer :: ICODE,IPLTNO,IV,IVAR

  IPTVAR(IPLTNO) = IPTVAR(IPLTNO)+IVAR
  if (IPTVAR(IPLTNO) .gt.10) IPTVAR(IPLTNO) = 10
  IV = IPTVAR(IPLTNO)
  IPTCOD(IPLTNO,IV) = ICODE
  PTL(IPLTNO,IV) = BELOW
  PTU(IPLTNO,IV) = ABOVE

  return
end
