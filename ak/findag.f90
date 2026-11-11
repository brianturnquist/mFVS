!=======================================!
!   Estimate tree age and site height   !
!=======================================!
! AK $Id$
subroutine FINDAG(I,ISPC,D1,D2,H,SITAGE,SITHT,AGMAX1,HTMAX1, &
     & HTMAX2,DEBUG)
  implicit none

  !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  !C                                                                   CC
  !C THERE ARE N O SITE CURVES FOR NEW ALASKA VARIANT SO THIS ROUTINE  CC
  !C DOES NOTHING. ONLY RETAINED FOR BASE CODE COMPATIBILITY.          CC
  !C                                                                   CC
  !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

  !  IN OTHER VARIANTS WITH SITE CURVES AVAILABLE, ITS PURPOSE IS TO
  !  DETERMINE TREE AGE BASED ON HEIGHT OR HEIGHT BASED ON TREE AGE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'

  ! Variables
  logical :: DEBUG

  integer :: I,ISPC

  real    :: AGMAX1,D1,D2,H,HTMAX1,HTMAX2
  real    :: SITAGE,SITHT

  if (DEBUG) write(JOSTND,1) I,ISPC, &
       & D1,D2,H,SITAGE,SITHT,AGMAX1,HTMAX1,HTMAX2
  1 format(' IN FINDAG AK STUB --I,ISPC=',2I5, &
       & ' D1,D2,H,SITAGE,SITHT,AGMAX1,HTMAX1,HTMAX2 ',8F10.2)

  return
end subroutine FINDAG
  !**END OF CODE SEGMENT
