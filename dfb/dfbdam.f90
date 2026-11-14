!==================!
!   dfb : dfbdam   !
!==================!
! DFB $Id$
subroutine DFBDAM (II,ICODES)
  implicit none

  !  PROCESSES THE DFB DAMAGE CODES.

  !  CALLED BY :
  !     DAMCDS  [PROGNOSIS]

  !  CALLS :
  !     NONE

  !  PARAMETERS :
  !     II     - TREE POINTER FOR LIVE AND DEAD TREES.
  !     ICODES - DISEASE AND DAMAGE CODES ARRAY.

  !  LOCAL VARIABLES :
  !     I      - COUNTER FOR DAMAGE CODES.

  !  COMMON BLOCK VARIABLES USED :
  !     NDAMS  - (DFBCOM)   OUTPUT
  !     IPT    - (DFBCOM)   OUTPUT

  !  Revision History :
  !  11/06/89 -
  !  07/10/07 - Lance R. David (FHTET)
  !    Time at which damage codes are processed is now at the end of
  !    keyword processing instead of during the reading of tree data.
  !    So, tree data items that were passed as arguments are now
  !    available from the FVS common area. Original arguments were:
  !    (II,ICODES,IITH,IREC2,LDELTR)
  !    FVS array IMC(II) is used as replacement for input tree history
  !    code (IITH).
  !    No need for special handling of dead tree index (IREC2) because
  !    dead trees are already at the end of the arrays. Argument II is
  !    correct index value for both live and dead.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'DFBCOM.f90'

  ! Variables
  logical :: LDELTR

  integer :: I,II,ICODES(6),IITH,IREC2

  !     IF THE DAMAGE CODE IS 3, THERE IS DFB DAMAGE...IF THE
  !     CORRESPONDING SEVERITY CODE IS >= 3, SUCCESSFUL ATTACK, THEN SAVE
  !     THE NUMBER OF TREES.

  do I = 1,5,2
     if (ICODES(I) .eq.3) then
        if (ICODES(I+1) .ge.3) then
           NDAMS = NDAMS + 1

           !              IF THE TREE IS RECENT MORTALITY (DIED WITH MORTALITY
           !              OBSERVATION PERIOD, IMC (MANAGEMENT CODE) 7. IMC SET
           !              BASED ON TREE HISTORY CODE OF 6 OR 7 DURING READING
           !              OF INPUT TREE DATA IN SUBROUTINE INTREE.

           if (IMC(II) .eq.7) then
              IPT(NDAMS) = II
           end if

           goto 200
        end if
     end if
     100 continue
  end do

  200 continue
  return
end
