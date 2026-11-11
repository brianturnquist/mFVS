!=======================================!
!   Find the first nonblank character   !
!=======================================!
! BASE $Id$
function ISTFNB (STRING)
  implicit none

  !     FIND THE LOCATION OF THE FIRST NON-BLANK CHAR IN A STRING.
  !     RETURN A ZERO IF THE ENTIRE STRING IS BLANK.

  ! Variables
  integer :: ISTFNB,I
  character(len=*) :: STRING
  if (STRING.eq.' ') then
     ISTFNB = 0
  else
     do I = 1,LEN(STRING)
        if (STRING(I:I).ne.' ') then
           ISTFNB = I
           return
        end if
        10 continue
     end do
  end if
  return
end function ISTFNB
