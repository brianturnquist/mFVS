!========================!
!   metric/so : avht40   !
!========================!
! METRIC-SO $Id$
subroutine AVHT40
  implicit none

  !  THIS SUBROUTINE IS USED TO CALCULATE THE AVERAGE HEIGHT
  !  OF THE 40 TPA OF LARGEST DIAMETER. (METRIC EQUIV)

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'
  include 'METRIC.f90'

  ! Variables
  integer :: I,II
  real    :: SSUMN,TARG,P

  AVH = 0.
  if (ITRN.le.0) goto 70
  SSUMN = 0.
  TARG = 100.0/HAtoACR ! METRIC VERSION = 40.47/AC
  do I = 1,ITRN
     II = IND(I)
     P = PROB(II)
     if (SSUMN+P.gt.TARG) P = TARG-SSUMN
     SSUMN = SSUMN+P
     AVH = AVH+HT(II)*P
     if (SSUMN.ge.TARG) go to 65
     60 continue
  end do
  65 continue
  if (SSUMN .gt.0.) AVH = AVH/SSUMN
  70 continue
  return
end
