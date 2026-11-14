!====================!
!   wsbwe : bwesin   !
!====================!
! WSBWE $Id$
subroutine BWESIN (PRMS,IBWSPM,IS1,IS2,ICRC1,ICRC2,ICRC3)
  implicit none

  !     CALLED BY BWMDEF AND BWSDIT TO SET DO-LOOP INDICIES FOR THE
  !     BUDWORM MODEL OPTIONS.  (HOST AND CROWN).

  !     PART OF THE WESTERN SPRUCE BUDWORM MODEL/PROGNOSIS LINKAGE CODE.
  !     N.L. CROOKSTON--FORESTRY SCIENCES LAB, MOSCOW, ID--AUG 1983

  !     CALLED FROM :

  !       BWEDEF - APPLY MANUAL DEFOLIATION TO BUDWORM MODEL TREES.
  !       BWESIT - CREATE A BUDWORM MODEL STAND FROM A PROGNOSIS STAND.

  !     PARAMETERS :

  !       PRMS   - ARRAY OF PARAMETERS FROM OPTION (ASK MR. WIZARD.)
  !       IBWSPM - ARRAY WHICH MAPS PROGNOSIS SPECIES CODES TO BUDWORM
  !                SPECIES CODES.
  !       IS1    - FIRST RETURNED HOST DO LOOP INDEX.
  !       IS2    - SECOND RETURNED HOST DO LOOP INDEX.
  !       ICRC1  - FIRST RETURNED CROWN DO LOOP INDEX.
  !       ICRC2  - SECOND RETURNED CROWN DO LOOP INDEX.
  !  Revision History:
  !    14-JUL-2010 Lance R. David (FMSC)
  !       Previous noted revision 03/18/93
  !       Added IMPLICIT NONE and declared variables as needed.

  ! Common blocks
  include 'PRGPRM.f90'

  ! Variables
  integer :: IBWSPM(MAXSP),ICRC1,ICRC2,ICRC3,IS1,IS2
  real    :: PRMS(6)

  IS1 = IFIX(PRMS(1))
  if (IS1.le.0) then
     IS1 = 1
     IS2 = 5
  else
     IS1 = IBWSPM(IS1)
     IS2 = IS1
  end if

  ICRC1 = IFIX(PRMS(2))
  ICRC3 = 1
  if (ICRC1.le.0) then
     ICRC1 = 1
     ICRC2 = 9
  else if (ICRC1.le.9) then
     ICRC2 = ICRC1
  else if (ICRC1.eq.10) then
     ICRC1 = 1
     ICRC2 = 3
  else if (ICRC1.eq.11) then
     ICRC1 = 4
     ICRC2 = 6
  else if (ICRC1.eq.12) then
     ICRC1 = 7
     ICRC2 = 9
  else if (ICRC1.eq.13) then
     ICRC1 = 1
     ICRC2 = 7
     ICRC3 = 3
  else if (ICRC1.eq.14) then
     ICRC1 = 2
     ICRC2 = 8
     ICRC3 = 3
  else if (ICRC1.eq.15) then
     ICRC1 = 3
     ICRC2 = 9
     ICRC3 = 3
  end if
  return
end
