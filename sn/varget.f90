!=================================!
!   Read variant specific state   !
!=================================!
! SN $Id$
subroutine VARGET (WK3,IPNT,ILIMIT,REALS,LOGICS,INTS)
  implicit none

  !     READ THE VARIANT SPECIFIC VARIABLES.

  !     PART OF THE PARALLEL PROCESSING EXTENSION TO PROGNOSIS.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'SNCOM.f90'

  !     NOTE: THE ACTUAL STORAGE LIMIT FOR INTS, LOGICS, AND REALS
  !     IS MAXTRE (SEE PRGPRM).

  ! Variables
  integer :: ILIMIT,IPNT,MXL,MXI,MXR
  parameter (MXL = 1,MXI = 2,MXR = 1)
  logical :: LOGICS(*)
  real    :: WK3(MAXTRE)
  integer :: INTS (*)
  real    :: REALS (*)
  real    :: RDANUW
  logical :: LDANUW

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = REALS(1)
  LDANUW = LOGICS(1)

  !     GET THE INTEGER SCALARS.

  call IFREAD (WK3, IPNT, ILIMIT, INTS, MXI, 2)
  KODIST = INTS(1)
  ISEFOR = INTS(2)

  !     GET THE LOGICAL SCALARS.

  !**   CALL LFREAD (WK3, IPNT, ILIMIT, LOGICS, MXL, 2)

  !     GET THE REAL SCALARS.

  !**   CALL BFREAD (WK3, IPNT, ILIMIT, REALS, MXR, 2)
  return
end
