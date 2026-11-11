!================!
!   rd : rdtim   !
!================!
! RD $Id$
subroutine RDTIM
  implicit none

  !  SUBROUTINE FOR SUMMING UP OPROB FOR TIM

  !  CALLED BY :
  !     RDTREG  [ROOT DISEASE]

  !  CALLS     :
  !     NONE

  !  Revision History:
  !   03/01/95 - Last revision date.
  !   09/04/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'
  include 'RDCOM.f90'
  include 'RDARRY.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'RDADD.f90'

  ! Variables
  integer :: I,IDI

  RRGEN(1,1) = 0.0
  RRGEN(2,1) = 0.0
  if (ITRN .lt.1) return

  IDI = MAXRR
  do I = 1,ITRN
     if (MAXRR .lt.3) IDI = IDITYP(IRTSPC(ISP(I)))

     !     If non-host species skip operation (RNH May98)

     if (IDI .le.0) go to 1000

     RRGEN(IDI,1) = RRGEN(IDI,1) + PROB(I)
     1000 continue
  end do

  return
end
