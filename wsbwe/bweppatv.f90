!======================!
!   wsbwe : bweppatv   !
!======================!
! WSBWE $Id$
subroutine BWEPPATV (L)
  implicit none

  !  RETURNS L=TRUE, TO INDICATE THAT GENDEFOL/BUDWORM MODEL IS AVAILABLE
  !  TO PARALLEL PROCESSOR.

  !  CALLED BY :
  !     GETSTD  [PARALLEL PROCESSOR]
  !     PUTSTD  [PARALLEL PROCESSOR]

  !  CALLS     :
  !     NONE

  !  PARAMETERS :
  !     L     - FLAG THAT IS SET TO TRUE WHEN THE BUDWORM MODEL IS
  !             ATTACHED WITH PPE.

  ! Variables
  logical :: L

  L = .true.

  return
end
