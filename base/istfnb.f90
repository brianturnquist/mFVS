!=======================================!
!   Find the first nonblank character   !
!=======================================!
! BASE $Id$
! External compatibility wrapper for ISTFNB_MOD's ISTFNB. It keeps the legacy
! external symbol, its default-integer result and its assumed-length character
! argument for API clients and any caller not yet using the module.
function ISTFNB (STRING)
  use ISTFNB_MOD, only: ISTFNB_CHECKED => ISTFNB
  implicit none

  !     FIND THE LOCATION OF THE FIRST NON-BLANK CHAR IN A STRING.
  !     RETURN A ZERO IF THE ENTIRE STRING IS BLANK.

  ! Variables
  integer :: ISTFNB
  character(len=*) :: STRING
  ISTFNB = ISTFNB_CHECKED(STRING)
  return
end function ISTFNB
