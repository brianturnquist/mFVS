!=================!
!   rd : rdssiz   !
!=================!
! RD $Id$
subroutine RDSSIZ(ISP,A,STCUT,ISL,ISPS,IRTSPC)
  implicit none

  !  SUBROUTINE FOR ASSIGNING SIZE CLASS TO STUMPS.

  !  CALLED BY :
  !     RDDAM   [ROOT DISEASE]
  !     RDEND   [ROOT DISEASE]
  !     RDIN    [ROOT DISEASE]
  !     RDSADD  [ROOT DISEASE]
  !     RDSTR   [ROOT DISEASE]

  !  CALLS :
  !     NONE

  !  PARAMETERS :
  !     ISP    -
  !     A      -
  !     STCUT  -
  !     ISL    -
  !     ISPS   -
  !     IRTSPC -

  !  Revision History :
  !   03/05/95 - Last revision date.
  !   09/03/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  !.... PARAMETER INCLUDE FILES

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'

  ! Variables
  integer :: ISL,ISP,ISPS(ITOTSP),IRTSPC(MAXSP),J
  real    :: A,STCUT(5)
  integer :: IDANUW

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  IDANUW = IRTSPC(1)
  IDANUW = ISP
  IDANUW = ISPS(1)

  do J = 2,5
     if (A .lt. STCUT(J-1) .or. A .gt. STCUT(J)) goto 100
     ISL = J - 1
     return
     100 continue
  end do

  ISL = 5

  return
end
