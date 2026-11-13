!=====================!
!   mistoe : exdmsi   !
!=====================!
! MISTOE $Id$
subroutine DMSIFLG(NEWSI)
  implicit none

  !  SATISFY EXTERNAL REFERENCES FOR THE BC "NEW S+I" (NISI) DWARF
  !  MISTLETOE MODEL, WHICH ONLY BC LINKS (canada/newmist/dmsiflg.f90).

  !  ENTRY DMSIFLG   CALLED FROM MISTOE.  THE NEW MODEL IS NEVER IN
  !  EFFECT HERE, SO NEWSI IS ALWAYS FALSE AND MISTOE NEVER CALLS DMSIRUN.

  ! Variables
  logical :: NEWSI

  NEWSI = .false.
  return
end

subroutine DMSIRUN
  implicit none

  !  NEVER CALLED: MISTOE CALLS DMSIRUN ONLY WHEN DMSIFLG RETURNS TRUE.

  return
end
