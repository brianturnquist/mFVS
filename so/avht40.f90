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
  include 'METRIC.f90'

  ! Variables
  integer :: I,II
  real    :: SSUMN,P
  real :: TARG

  AVH = 0.
  if (ITRN.le.0) goto 70
  SSUMN = 0.
  if (LMETRICVAR) then
     TARG = 100.0/HAtoACR ! METRIC VERSION = 40.47/AC
  end if
  do I = 1,ITRN
     II = IND(I)
     P = PROB(II)
     if (LMETRICVAR) then
        if (SSUMN+P.gt.TARG) P = TARG-SSUMN
     else
        if (SSUMN+P.gt.40.0) P = 40.0-SSUMN
     end if
     SSUMN = SSUMN+P
     AVH = AVH+HT(II)*P
     if (LMETRICVAR) then
        if (SSUMN.ge.TARG) go to 65
     else
        if (SSUMN.ge.40.0) go to 65
     end if
     60 continue
  end do
  65 continue
  if (SSUMN .gt.0.) AVH = AVH/SSUMN
  70 continue
  return
end
