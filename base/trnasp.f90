!=================================!
!   Convert azimuths to radians   !
!=================================!
! BASE $Id$
subroutine TRNASP
  implicit none

  !  TRNASP DECODES THE INPUT AZIMUTH VALUE GIVEN IN DEGREES AND CONVERTS
  !  THE VALUE TO RADIANS. THE VALUE IS THEN ASSIGNED TO THE VARIABLE
  !  ASPECT.

  ! Common blocks
  include 'PRGPRM.F77'
  include 'PLOT.F77'

  !   CONVERT ASPECT TO RADIANS.

  ASPECT = ASPECT*0.0174533
  return
end subroutine TRNASP
