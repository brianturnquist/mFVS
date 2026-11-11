!==================!
!   dfb : dfbprb   !
!==================!
! DFB $Id$
subroutine DFBPRB (PROTBK)
  implicit none

  !  THIS ROUTINE IS USED TO CALCULATE THE PROBABILITY OF A STAND
  !  OUTBREAK.

  !  CALLED BY :
  !     DFBGO  [DFB]

  !  CALLS :
  !     NONE

  !  PARAMETERS :
  !     PROTBK - THE PROBABILITY OF A DFB STAND OUTBREAK (OUTPUT).

  !  COMMON BLOCK VARIABLES USED :
  !     A45DBH - (DFBCOM)  INPUT
  !     BA9    - (DFBCOM)  INPUT
  !     BADF9  - (DFBCOM)  INPUT
  !     DEBUIN - (DFBCOM)  INPUT
  !     JODFB  - (DFBCOM)  INPUT
  !     JOSTND - (CONTRL)  INPUT
  !     PBADF4 - (DFBCOM)  INPUT

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'DFBCOM.f90'

  ! Variables
  real    :: PROTBK

  !     CALCULATE THE PROBABILITY OF A STAND OUTBREAK OCCURING.

  !     PROBABILITY IS EQUAL TO ZERO IF ANY OF THE MINIMUM CONDITIONS
  !     ARE NOT PASSED.

  PROTBK = 0.0
  if (A45DBH .lt.9.0) goto 10
  if (PBADF4.lt.0.25) goto 10

  PROTBK = BADF9 / BA9
  if (PROTBK .gt.0.9) PROTBK = 0.9

  10 continue

  !     PRINT VARIABLES IF DEBUG IS USED.

  if (DEBUIN) then
     write (JOSTND,*) 'IN DFBPRB   *******************'
     write (JOSTND,*) 'A45DBH = ', A45DBH
     write (JOSTND,*) 'PBADF4 = ', PBADF4
     write (JOSTND,*) 'BADF9  = ', BADF9
     write (JOSTND,*) 'BA9    = ', BA9
     write (JOSTND,*) 'PROTBK = ', PROTBK
     write (JOSTND,*) 'LEAVING DFBPRB   **************'
  end if

  return
end
