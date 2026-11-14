!================!
!   lpmpb : PT   !
!================!
! LPMPB $Id$

! Revision History
!   02/08/88 Last noted revision date.
!   07/02/10 Lance R. David (FMSC)
!     Declared variables for Addition of IMPLICIT NONE to subroutines.

! Variables
character(len=1) :: IPTLET(5,10)
integer :: IPTVAR(5),IPTSPL(5),IPTCOD(5,10)
real    :: PTMAX(5,10),PTMIN(5,10),PTU(5,10),PTL(5,10)

common/PTCHR/ IPTLET
common/PT/ PTMAX,PTMIN,PTU,PTL,IPTVAR,IPTSPL,IPTCOD
