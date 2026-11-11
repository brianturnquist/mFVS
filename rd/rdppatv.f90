!==================!
!   rd : rdppatv   !
!==================!
! RD $Id$
subroutine RDPPATV (L)

  !  RETURNS L=TRUE, TO INDICATE THAT ROOT DISEASE MODEL IS AVAILABLE
  !  TO PARALLEL PROCESSOR.

  !  CALLED BY :
  !     GETSTD  [PARALLEL PROCESSOR]
  !     PUTSTD  [PARALLEL PROCESSOR]

  !  CALLS     :
  !     NONE

  !  PARAMETERS :
  !     L     - FLAG THAT IS SET TO TRUE WHEN THE ROOT DISEASE MODEL IS
  !             ATTACHED WITH PPE.

  ! Variables
  logical :: L

  L = .true.

  return
end
