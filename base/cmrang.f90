!=============================================!
!   Find the range of indexed vector values   !
!=============================================!
! BASE $Id$
function CMRANG (LEN,INDX,ARR)
  implicit none

  !     FINDS THE MAX RANGE, CMRANG, OF THE VECTOR ARR.  ONLY THE
  !     ELEMENTS OF ARR INDEXED BY INDX ARE CONSIDERED.

  !     PART OF THE COMPRESSION ROUTINE COMPRS, WHICH IS PART OF THE
  !     PROGNOSIS MODEL FOR STAND DEVELOPMENT.
  !     N.L. CROOKSTON - FORESTRY SCIENCES LAB, MOSCOW, ID - JUNE 1982.

  ! Variables
  integer :: INDX(*),LEN,I,J
  real    :: ARR(*),CMRANG,X1,X2

  X1 = 1E30
  X2 = -1E30
  do J = 1,LEN
     I = INDX(J)
     if (ARR(I).lt.X1) X1 = ARR(I)
     if (ARR(I).gt.X2) X2 = ARR(I)
     10 continue
  end do
  CMRANG = X2-X1
  return
end function CMRANG
