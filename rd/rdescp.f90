!=================!
!   rd : rdescp   !
!=================!
! RD $Id$
subroutine RDESCP (LMAXTR,MXRR)
  implicit none

  !  THIS ROOT DISEASE MODEL SUBROUTINE INSURES THAT THERE IS ROOM
  !  IN THE TREELIST FOR ESTAB TREES. IF THERE IS ACTIVE ROOT DISEASE,
  !  MXRR WILL BE RETURNED AS THE RD MODEL TREE RECORD LIMIT (IRRTRE).

  !  CALLED BY :
  !     ESTAB   [PROGNOSIS]

  !  PARAMETERS :
  !     LMAXTR - MAXIMUM NUMBER OF PROGNOSIS MODEL TREE RECORDS
  !              (NAME CHANGED TO AVOID CONFLICT WITH MAXTRE IN
  !               PRGPRM FILE.)
  !     MXRR   - MAXIMUM NUMBER OF RROT TREE RECORDS

  !  Revision History :
  !   08/14/12 - Last revision date.
  !   08/28/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  ! Common blocks
  include  'PRGPRM.f90'
  include  'RDPARM.f90'
  include  'RDCOM.f90'

  ! Variables
  integer :: IDI,LMAXTR,MXRR
  real    :: TPAREA

  TPAREA = 0.0
  do IDI = 1,ITOTRR
     TPAREA = TPAREA + PAREA(IDI)
     100 continue
  end do

  if (IROOT .eq.0.or. TPAREA .eq.0.0) then
     MXRR = LMAXTR
  else
     MXRR = IRRTRE
  end if

  return
end
