!=========================!
!   archive/ak : oldsec   !
!=========================!
! AK $Id$
subroutine OLDSEC(ISPC,VN,D,H)
  implicit none

  ! Variables
  integer :: ISPC

  real    :: D,H,VN

  ! ENTRY SECGRO COMPUTES VOLUMES FOR SECOND GROWTH TREES
  ! (D GE 4 AND H GE 18) UP TO (D LE 9 AND H LT 40)

  VN = -5.577 + 1.9067 * ALOG(D) + 0.9416 * ALOG(H)
  if (VN .le.0.0) go to 120
  VN = EXP(VN)
  go to 130
  120 continue
  VN = 0.0
  130 continue
  return
end
