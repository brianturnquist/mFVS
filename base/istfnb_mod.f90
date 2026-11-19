!=======================================!
!   Find the first nonblank character   !
!=======================================!
! BASE $Id$
! Checked home of ISTFNB (MOD07 pilot). Internal callers import ISTFNB from
! this module so the compiler checks their argument; base/istfnb.f90 keeps the
! legacy external function for API clients and any unconverted caller.
module ISTFNB_MOD
  implicit none
  private
  public :: ISTFNB

contains

  function ISTFNB (STRING)
    implicit none

    !     FIND THE LOCATION OF THE FIRST NON-BLANK CHAR IN A STRING.
    !     RETURN A ZERO IF THE ENTIRE STRING IS BLANK.

    ! Variables
    integer :: ISTFNB,I
    character(len=*), intent(in) :: STRING
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

end module ISTFNB_MOD
