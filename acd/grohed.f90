!===================================!
!   Write the model output header   !
!===================================!
! ACD $Id$
subroutine GROHED (IUNIT)
  use FVSVERSION
  implicit none

  !     WRITES HEADER FOR BASE MODEL PORTION OF PROGNOSIS SYSTEM

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.F77'

  ! Variables
  character(len=10) :: DAT
  character(len=10) :: REV
  character(len=32) :: SVN
  character(len=8) :: TIM

  integer :: IUNIT

  !     CALL REVISE TO GET THE LATEST REVISION DATE FOR THIS VARIANT.

  call REVISE (VARACD,REV)

  !     CALL THE DATE AND TIME ROUTINE FOR THE HEADING.

  call GRDTIM (DAT,TIM)
  SVN = FVSVER_VERSION

  write (IUNIT,40) SVN,REV,DAT,TIM
  40 format (//T10,'FOREST VEGETATION SIMULATOR', &
       & 5X,'VERSION ',A,' -- ACADIAN', &
       & T98,'RV:',A,T113,A,2X,A)

  return
end subroutine GROHED
