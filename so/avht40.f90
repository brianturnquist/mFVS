!=================!
!   so : avht40   !
!=================!
! SO $Id$
subroutine AVHT40
  implicit none

  !  THIS SUBROUTINE IS USED TO CALCULATE THE AVERAGE HEIGHT
  !  OF THE 40 TPA OF LARGEST DIAMETER.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'

  ! Variables
  integer :: I,II
  real    :: SSUMN,P

  AVH = 0.
  if (ITRN.le.0) goto 70
  SSUMN = 0.
  do I = 1,ITRN
     II = IND(I)
     P = PROB(II)
     if (SSUMN+P.gt.40.0) P = 40.0-SSUMN
     SSUMN = SSUMN+P
     AVH = AVH+HT(II)*P
     if (SSUMN.ge.40.0) go to 65
     60 continue
  end do
  65 continue
  if (SSUMN .gt.0.) AVH = AVH/SSUMN
  70 continue
  return
end
