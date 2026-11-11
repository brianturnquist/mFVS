!===================!
!   lpmpb : ptsym   !
!===================!
! LPMPB $Id$
subroutine PTSYM (IPLTNO,L1,L2,L3,L4,L5,L6,L7,L8,L9,L10)
  implicit none

  !      ROUTINE TO ESTABLISH PLOTTING SYMBOLS AND INITIALIZE
  !      PLOTTING SYSTEM.

  !     NOTE- CALLING ROUTINE MUST SPECIFY THE SYMBOLS TO BE
  !     USED AS HOLLERITH CONSTANTS. SYMBOL PARAMETERS NOT USED
  !     MUST BE PRESENT BUT NEED NOT BE HOLLERITH.
  !     EG.  CALL PTSYM(1,1H1,1H2,1HX,1H+,0,0,0,0,0,0)

  ! Revision History
  !   02/08/88 Last noted revision date.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  ! Common blocks
  include 'PT.f90'

  ! Variables
  character(len=1) :: L1
  character(len=1) :: L2
  character(len=1) :: L3
  character(len=1) :: L4
  character(len=1) :: L5
  character(len=1) :: L6
  character(len=1) :: L7
  character(len=1) :: L8
  character(len=1) :: L9
  character(len=1) :: L10
  integer :: IPLTNO,J

  do J = 1,10
     PTMAX(IPLTNO,J) = -1E35
     PTMIN(IPLTNO,J) = +1E35
     PTU(IPLTNO,J) = 0.
     PTL(IPLTNO,J) = 0.
     IPTCOD(IPLTNO,J) = 100
     1 continue
  end do

  IPTSPL(IPLTNO) = 0
  IPTVAR(IPLTNO) = 0

  IPTLET(IPLTNO,1 ) = L1
  IPTLET(IPLTNO,2 ) = L2
  IPTLET(IPLTNO,3 ) = L3
  IPTLET(IPLTNO,4 ) = L4
  IPTLET(IPLTNO,5 ) = L5
  IPTLET(IPLTNO,6 ) = L6
  IPTLET(IPLTNO,7 ) = L7
  IPTLET(IPLTNO,8 ) = L8
  IPTLET(IPLTNO,9 ) = L9
  IPTLET(IPLTNO,10) = L10

  return
end
