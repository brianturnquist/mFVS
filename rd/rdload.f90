!=================!
!   rd : rdload   !
!=================!
! RD $Id$
subroutine RDLOAD(A,B,N)
  implicit none

  !  SUBROUTINE FOR COPYING ONE ARRAY INTO ANOTHER.

  !  CALLED BY :
  !     RDTREG  [ROOT DISEASE]

  !  CALLS     :
  !     NONE

  !  PARAMETERS :
  !     A      - DESTINATION ARRAY OF COPY.
  !     B      - SOURCE ARRAY OF COPY.
  !     N      - NUMBER OF ELEMENTS IN THE 2 ARRAYS.

  !  Revision History :
  !   12/15/87 - Last revision date.
  !   08/29/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  ! Variables
  integer :: I,N
  real    :: A(N),B(N)

  if (N .gt.0) then
     do I = 1,N
        A(I) = B(I)
        1000 continue
     end do
  end if

  return
end
