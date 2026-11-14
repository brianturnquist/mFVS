!====================!
!   lpmpb : coldbh   !
!====================!
! LPMPB $Id$
subroutine COLDBH
  implicit none

  !     INITIALIZATION ROUTINE FOR COLE'S MPB MODEL

  ! Revision History
  !   05/31/00 Last noted revision date.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'COLCOM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'MPBCOM.f90'

  ! Variables
  integer :: I,J,I1,I2,INDEX

  !     INITIALIZE ARRAYS

  STOTAL = 0.0
  do I = 1,10
     DEADCL(I) = 0.0
     START(I) = 0.0
     TGREEN(I) = 0.0
     TDEAD(I) = 0.0
     do J = 1,10
        DEAD(I,J) = 0.0
        GREEN(I,J) = 0.0
        4 continue
     end do
     5 continue
  end do

  !     DETERMINE NUMBER OF TREES PER DBH CLASS IN THE STAND

  !     Changed pointer array ISCT subscript from 7 to IDXLP to correspond
  !     with new species mapping and new location for LP (RNH Dec98, GEB May2000)

  !      I1=ISCT(7,1)
  I1 = ISCT(IDXLP,1)

  if (I1.eq.0) goto 20

  !     Changed pointer array ISCT subscript from 7 to IDXLP to correspond
  !     with new species mapping and new location for LP (RNH Dec98, GEB May2000)

  !      I2=ISCT(7,2)
  I2 = ISCT(IDXLP,2)
  do I = I1,I2
     J = IND1(I)

     !       LOAD THE STARTING NUMBER OF TREES WITH THE
     !       NUMBER OF TREES PER ACRE THAT THIS TREE RECORD
     !       REPRESENTS.

     call COLIND (DBH(J),INDEX)
     START(INDEX) = START(INDEX)+PROB(J)
     10 continue
  end do
  20 continue
  return
end
