!==============================!
!   canada/newmist : dmsiflg   !
!==============================!
! CANADA-NEWMIST $Id$
subroutine DMSIFLG(NEWSI)
  implicit none

  !  CALLED FROM: MISTOE

  !  Purpose:
  !     Connects the shared MISTOE (mistoe/mistoe.f90) to the BC "new
  !  S+I" (NISI) dwarf mistletoe model. DMSIFLG sets NEWSI, which tells
  !  MISTOE whether the new model is in effect; DMSIRUN (below) runs it.
  !  This code was formerly inline in the BC copy of MISTOE
  !  (canada/newmist/mistoe.f90). The other mistletoe variants link the
  !  stubs in mistoe/exdmsi.f90 instead.

  !  Call list definitions:
  !     NEWSI:  (O) Logical flag, true if new S+I model is to be used.

  ! Common block variables and parameters:
  !     MISFLG: From MISCOM; mistletoe processing on/off (MISTOFF).
  !     NEWMOD: From DMCOM;  true if the NISI model is in effect.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'MISCOM.f90'
  include 'DMCOM.f90'

  ! Variables
  logical :: NEWSI

  !.... If the new S+I model is in effect and if the MISTOFF
  !.... keyword is not active, then NEWSI is true.

  NEWSI = .false.
  if (NEWMOD .and. MISFLG) NEWSI = .true.

  return
end

subroutine DMSIRUN
  implicit none

  !  CALLED FROM: MISTOE, WHEN DMSIFLG HAS SET NEWSI TRUE.

  !  Purpose:
  !     New model calls DMTREG, then maps results back to IMIST.

  ! Common block variables and parameters:
  !     DMRATE: From DMCOM;
  !     IMIST:  From MISCOM; individual tree record mistletoe rating.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'MISCOM.f90'
  include 'DMCOM.f90'

  call DMTREG
  call DMMDMR(DMRATE,IMIST)

  return
end
