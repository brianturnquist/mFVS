!======================================!
!   Find the last nonblank character   !
!======================================!
! BASE $Id$
function ISTLNB (STRING)
  implicit none

  !     FIND THE LOCATION OF THE LAST NON-BLANK CHAR IN A STRING.
  !     RETURNS A ZERO IF THE ENTIRE STRING IS BLANK.

  ! Variables
  integer :: ISTLNB,MAX,J,K,I
  character(len=*) :: STRING
  if (STRING.eq.' ') then
     ISTLNB = 0
  else
     MAX = LEN(STRING)
     J = MAX/2
     K = MAX
     10 continue
     if (K-J.gt.10) then
        if (STRING(J:K).eq.' ') then
           K = J+1
           J = J/2
        else
           J = J+((K-J)/2)-1
        end if
        goto 10
     end if
     do I = K,1,-1
        if (STRING(I:I).ne.' ') then
           ISTLNB = I
           return
        end if
        20 continue
     end do
  end if
  return
end function ISTLNB
