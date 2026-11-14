!=========================!
!   archive/ak : oldfst   !
!=========================!
! AK $Id$
subroutine OLDFST(ISPC,VN,D,H)
  implicit none

  !  FSTGRO COMPUTES CUBIC FOOT VOLUMES FOR TREES WITH (D LT 4 OR H LT 18)

  ! Variables
  integer :: ISPC

  real    :: D,FORM,H,TERM1,TERM2,VN

  if (H .le.4.5) then
     VN = 0.
     go to 10
  end if

  if (H .le.18.0) then
     TERM1 = ((H-0.9)*(H-0.9)) / ((H-4.5)*(H-4.5))
     TERM2 = TERM1*(H-0.9)/(H-4.5)
     FORM = 0.406098*TERM1 - 0.0762998*D*TERM2 + 0.00262615*D*H*TERM2
     go to 130
  end if
  FORM = 0.480961 + 42.46542/(H*H) - 10.99643*D/(H*H)&
       &- 0.107809*D/H - 0.00409083*D
  130 VN = 0.005454154*FORM*D*D*H
  if (VN .lt.0.0)VN = 0.0
  10 continue
  return
end
