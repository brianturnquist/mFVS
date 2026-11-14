!====================!
!   wsbwe : bwekey   !
!====================!
! WSBWE $Id$
subroutine BWEKEY(KEY,PASKEY)
  implicit none

  !     RETRIEVE A BUDWORM MODEL KEYWORD FROM THE KEYWORD TABLE.

  !     PART OF THE WESTERN SPRUCE BUDWORM MODEL/PROGNOSIS LINKAGE CODE.
  !     N.L. CROOKSTON--FORESTRY SCIENCES LAB, MOSCOW, ID--JUNE 1983

  !     CALLED FROM :

  !       OPLIST - PROGNOSIS ROUTINE WHICH PRINTS LIST OF SCHEDULED
  !                OPTIONS.

  !     PARAMETERS :

  !     KEY - INDEX INTO TABLE.
  !     PASKEY - TABLE (KEY)

  !  Revision History:
  !    17-MAY-2005 Lance R. David (FHTET)
  !       Added FVS parameter file PRGPRM.F77.
  !    14-JUL-2010 Lance R. David (FMSC)
  !       Added IMPLICIT NONE and declared variables as needed.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'BWECOM.f90'

  ! Variables
  character(len=8) :: PASKEY
  integer :: KEY
  PASKEY = TABLE(KEY)
  return
end
