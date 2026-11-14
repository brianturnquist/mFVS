!================!
!   dftm : uv2   !
!================!
! DFTM $Id$
function UV2 (G, KP, X, B, ALPHA, ETA)
  implicit none

  !     DFTM MODEL SUBROUTINE - JIM COLBERT - JAN 1978.

  ! Variables
  integer :: I,KP
  real    :: ALPHA,B(21),ETA,G(12),U,UV2,X(4)

  UV2 = 0.0
  if (G(12) .le.0.000001) return
  if (KP .eq.0.or. KP .gt.6) return

  U = X(3) * X(4)
  I = KP + 12
  if (G(7) .eq.0.0.and. ETA .eq.0.0) return

  UV2 = U * (B(19) + B(20) / G(12)) * (EXP(G(12)) - 1.0) * 0.001 /&
       &B(21) * B(I) * G(6)**ALPHA * EXP(ALPHA * G(12)) *&
       &(1.0 - (G(6) * G(7) * EXP(G(12)))**ETA) /&
       &(1.0 - G(6) * G(7) * EXP(G(12)))

  return
end
