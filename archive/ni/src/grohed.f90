!===================================!
!   Write the model output header   !
!===================================!
subroutine GROHED (IUNIT)
  implicit none

  !  **GROHED--NI DATE OF LAST REVISION:  02/22/13

  !     WRITES HEADER FOR BASE MODEL PORTION OF PROGNOSIS SYSTEM

  ! Variables
  integer :: IUNIT
  character(len=10) :: DAT
  character(len=8) :: TIM
  character(len=7) :: VVER
  character(len=7) :: DVVER
  character(len=10) :: REV
  character(len=8) :: SVN
  data DVVER/'NI     '/
  data SVN/'INCLUDESVN.F77'/

  !     CALL REVISE TO GET THE LATEST REVISION DATE FOR THIS VARIANT.

  call REVISE (DVVER,REV)

  !     CALL THE DATE AND TIME ROUTINE FOR THE HEADING.

  call GRDTIM (DAT,TIM)

  !     CALL PPE TO CLOSE OPTION TABLE IF IT IS OPEN.

  call PPCLOP (IUNIT)

  write (IUNIT,40) SVN,REV,DAT,TIM
  40 format (//T6,'FOREST VEGETATION SIMULATOR',&
       &5X,'VERSION ',A,' -- INLAND EMPIRE PROGNOSIS',&
       &T97,'RV:',A,T112,A,2X,A)
  return

  entry VARVER (VVER)

  !     SUPPLY THE VARIANT AND VERSION NUMBER.

  VVER = DVVER
  return
end
