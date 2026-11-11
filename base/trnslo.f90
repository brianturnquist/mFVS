!=========================!
!   Decode slope ratios   !
!=========================!
! BASE $Id$
subroutine TRNSLO
  implicit none

  !  TRNSLO DECODES THE INPUT SLOPE VALUE TO A RATIO.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'

  !     CONVERT SLOPE TO A RATIO.

  SLOPE = SLOPE/100.
  if (SLOPE.lt.0.0) SLOPE = 0.0
  return
end subroutine TRNSLO
