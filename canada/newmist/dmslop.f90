!=============================!
!   canada/newmist : dmslop   !
!=============================!
! CANADA-NEWMIST $Id$
subroutine DMSLOP(Dstnce, Offset)
  implicit none

  ! **DMSLOP -- NISI  Date of last revision: April 14 1994

  ! Purpose:
  !   Trees that are uphill or downhill from the target may have a
  ! different effect upon spread, if their position is different enough
  ! to shift the frame of reference more than 1 (or more) MESH units.
  ! Given the distance between a target and a source (in MESH units),
  ! the routine simulates the difference in height (in MESH units)
  ! between the ground under the source and target trees. This involves
  ! knowing the site slope (0-1, in rise/run; 1 = 45 degrees) and
  ! choosing a random angle (0 - 2 pi radians) relative to the gradient
  ! of the slope. Azimuth angle 0 is 'uphill'.

  ! Called by:

  !     DMTREG

  ! Other routines called:

  !     DMRANN

  ! Argument list definitions:

  !     INTEGER Dstnce   (I) The sampling ring in which the target tree
  !                           is located.
  !     INTEGER Offset   (O) The MESH unit offset betwee source and
  !                           target trees.

  ! Local variable definitions:

  !     REAL    Rnd          A uniform {0,1} random number.
  !     REAL    D            The distance between the target tree and
  !                           the midpoint of the sampling ring.

  ! Common block variables and parameters:

  !     SLOPE   PLOT

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'

  ! Argument list variables.

  ! Variables
  integer :: Dstnce
  integer :: Offset

  ! Local variables.

  real    :: Rnd
  real    :: D

  D = FLOAT(Dstnce) - 0.5

  ! Choose a random position around the ring quadrat, then determine the
  ! integer amount of the difference.

  call DMRANN(Rnd)
  Offset = INT(D * COS(6.283185 * Rnd) * SLOPE)

  return
end
