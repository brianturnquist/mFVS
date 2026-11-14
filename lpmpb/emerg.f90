!===================!
!   lpmpb : emerg   !
!===================!
! LPMPB $Id$
function EMERG(BY,T,INCRS)
  implicit none

  !     PART OF THE MOUNTAIN PINE BEETLE EXTENSION OF PROGNOSIS SYSTEM.

  ! Revision History
  !   02/08/88 Last noted revision date.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.
  !   08/22/14 Lance R. David (FMSC)
  !     Function name was used as variable name.
  !     changed variable INT to INCRS

  double precision :: C
  ! Variables
  integer :: T,INCRS
  real    :: EMERG,BY

  if (T .ne.0) go to 1
  C = 1.0D00/2**INCRS
  go to 2
  1 C = C*(INCRS-T+1)/T
  2 EMERG = BY*C
  return
end
