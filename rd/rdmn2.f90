!================!
!   rd : rdmn2   !
!================!
! RD $Id$
subroutine RDMN2 (XFINT)
  implicit none

  !  SETS UP INITIAL CONDITIONS FOR THE OTHER AGENTS AND ROOT DISEASE
  !  MODELS, AND UPDATES THE STUMP LISTS. NOTE THAT THIS HAPPENS BEFORE
  !  TRIPLING AND COMPRESSION

  !  CALLED BY :
  !     GRINCR  [PROGNOSIS]

  !  CALLS     :
  !     DBCHK   (SUBROUTINE)   [PROGNOSIS]
  !     RDSUM   (SUBROUTINE)   [ROOT DISEASE]
  !     RDSHST  (SUBROUTINE)   [ROOT DISEASE]

  !  PARAMETERS :
  !     XFINT  -

  !  Revision History :
  !   03/01/95 - Last revision date.
  !   08/29/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.
  !   03/01/2016 Lance R. David (FMSC)
  !     Moved check to exit if not active, no trees or no disease area
  !     to top.

  ! PARAMETER FILES
  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'
  include 'RDCOM.f90'
  include 'RDARRY.f90'
  include 'RDCRY.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'RDADD.f90'

  ! Variables
  logical :: DEBUG
  integer :: I,IDI,J
  real    :: TPAREA,XFINT

  !     Exit if not active or no trees.
  if (IROOT .eq.0.or. ITRN .eq.0) return

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'RDMN2',5,ICYC)

  ISTEP = ISTEP + 1
  if (DEBUG) write (JOSTND,100) ICYC, ISTEP
  100 format (' RDMN2 :  ICYC ISTEP=',2I5)
  IYEAR = IYEAR + INT(XFINT)
  TPAREA = 0.0
  do IDI = MINRR,MAXRR
     TPAREA = TPAREA + PAREA(IDI)
     105 continue
  end do

  !     Exit if no disease area.
  if (TPAREA .eq.0.0) return

  !     INCREMENT THE TIME STEP COUNTERS FOR THE GENERAL ROOT DISEASE
  !     MODEL.

  !     ZERO OUT VARIABLES FOR THE SIMULATION.

  !     PROBIN IS USED THE CARRYOVER MODEL

  do IDI = MINRR,MAXRR
     do I = 1,2
        do J = 1,5
           PROBIN(IDI,I,J) = 0.0
           200 continue
        end do
        300 continue
     end do
     350 continue
  end do

  !     INITIALIZE RRKILL (RDKILL IS NEEDED NEXT TIMESTEP (AS PRANKL) SO WILL
  !                        NOT BE ZEROED HERE)

  do I = 1,ITRN
     RRKILL(I) = 0.0
     !         RDKILL(I) = 0.0
     400 continue
  end do

  !     SUM THE ARRAY PROBI INTO THE ACCUMULATOR ARRAY PROBIT.

  call RDSUM (ITRN,PROBIT,PROBI,ISTEP)

  !     UPDATE THE STUMP HISTORY ARRAYS.

  call RDSHST

  return
end
