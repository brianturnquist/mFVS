!========================!
!   canada/on : revise   !
!========================!
subroutine REVISE(VAR, REV)
  use FVSVERSION
  implicit none

  ! CANADA-ON

  !  REVISE -- Returns the build date for use in run output headers.
  !  CALLED FROM: GROHED, FILOPN, SUMHED, SUMOUT, ECVOLS, PRTRLS, DGDRIV

  ! Variables
  character(len=2) :: VAR
  character(len=10) :: REV

  REV = FVSVER_DATE

  return
end
