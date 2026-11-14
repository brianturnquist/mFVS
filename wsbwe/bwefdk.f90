!====================!
!   wsbwe : bwefdk   !
!====================!
! WSBWE $Id$
subroutine BWEFDK (NUMBER,KEYWRD,TABLE,ISIZE,KODE,IOUT)
  implicit none

  !     FUNCTION TO FIND A KEYWORD IN A TABLE.

  !     PART OF THE WESTERN SPRUCE BUDWORM MODEL.
  !     NICHOLAS L. CROOKSTON                          SEPT 1979
  !     INT-FORESTRY SCIENCES LABORATORY-MOSCOW, IDAHO

  !     NUMBER= THE KEYWORD NUMBER (TABLE INDEX VALUE) RETURNED
  !             TO CALLING SUBROUTINE.
  !     KEYWRD= KEYWORD TO BE FOUND IN TABLE ARRAY
  !     TABLE = CHARACTER*8 ARRAY CONTAINING THE KEYWORDS
  !     ISIZE = THE DIMENSIONAL SIZE OF TABLE ARRAY
  !     KODE  = RETURN CODE:
  !             0= NO ERRORS FOUND.
  !             1= KEYWORD NOT FOUND
  !     IOUT  = OUTPUT WRITE UNIT REFERENCE NUMBER

  ! Revision History:
  !   12-MAY-00 Lance David
  !     Brought comments and variable descriptions up to date.
  !     Changed TABLE array definition to inherit dimension from
  !     calling program unit.
  !   14-JUL-2010 Lance R. David (FMSC)
  !     Added IMPLICIT NONE and declared variables as needed.

  ! Variables
  integer :: I,IOUT,ISIZE,KODE,NUMBER
  character(len=8) :: KEYWRD
  character(len=8) :: TABLE(*)

  NUMBER = 0
  do I = 1,ISIZE
     if (KEYWRD.ne.TABLE(I)) goto 10
     NUMBER = I
     goto 15
     10 continue
  end do
  15 continue
  if (NUMBER.eq.0) then
     KODE = 1
     write (IOUT,85) KEYWRD
     85 format (/,'''',A8,''' :KEYWORD SPECIFIED')
  else
     KODE = 0
  end if
  return
end
