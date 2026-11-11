!====================!
!   lpmpb : colind   !
!====================!
! LPMPB $Id$
subroutine COLIND (DBH,INDEX)
  implicit none

  !     PART OF THE MOUNTAIN PINE BEETLE EXTENSION OF PROGNOSIS.
  !     P.W. THOMAS -- FORESTRY SCIENCES LAB -- MOSCOW

  !     THIS ROUTINE ASSIGNS A SIZE CLASS BETWEEN ONE AND TEN
  !     TO A TREE DIAMETER BETWEEN 1.0 INCHES AND 20.0 INCHES.
  !     IF THE TREE'S DBH IS LESS THAN 1.0 INCHES, THE TREE IS
  !     ASSIGNED TO SIZE CLASS ONE.  IF THE TREES DBH IS GREATER
  !     THAN 20.0 INCHES, THEN THE TREE IS ASSIGNED TO SIZE CLASS
  !     TEN.

  !     PARAMETERS:

  !        DBH - FOUR BYTE REAL DBH OF TREE.
  !        INDEX - FOUR BYTE INTEGER SIZE CLASS.

  ! Revision History
  !   02/08/88 Last noted revision date.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  ! Variables
  integer :: IVAL,INDEX
  real    :: DBH

  IVAL = IFIX(DBH)
  INDEX = IVAL/2+MOD(IVAL,2)

  !     TAKE INTO ACCOUNT EXTREME VALUES LESS THAN 1 OR
  !     GREATER THAN 20 INCHES.

  if (INDEX.gt.10) INDEX = 10
  if (INDEX.lt.1) INDEX = 1
  return
end
