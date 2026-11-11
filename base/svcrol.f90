!=================================!
!   Stand visualization: SVCROL   !
!=================================!
! BASE $Id$
subroutine SVCROL (X1,Y1,R1,X2,Y2,R2,KODE)
  implicit none

  !     STAND VISUALIZATION GENERATION
  !     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998

  !     RETURN KODE = 0 IF TWO CIRCLES WITH CENTER X,Y AND RADUS
  !                   R, DO NOT OVERLAP, 1 IF THEY DO.

  ! Variables
  real    :: X1,Y1,R1,X2,Y2,R2,D2
  integer :: KODE
  D2 = ((X2-X1)**2)+((Y2-Y1)**2)
  R2 = R1+R2
  R2 = R2*R2
  if (R2 .gt. D2) then
     KODE = 1
  else
     KODE = 0
  end if
  return
end subroutine SVCROL
