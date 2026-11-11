!==================================!
!   Write variant specific state   !
!==================================!
! KT $Id$
subroutine VARPUT (WK3,IPNT,ILIMIT,REALS,LOGICS,INTS)
  implicit none

  !     WRITE THE VARIANT SPECIFIC VARIABLES.

  !     PART OF THE PARALLEL PROCESSING EXTENSION TO PROGNOSIS.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'KOTCOM.f90'

  !     NOTE: THE ACTUAL STORAGE LIMIT FOR INTS, LOGICS, AND REALS
  !     IS MAXTRE (SEE PRGPRM).

  ! Variables
  integer :: ILIMIT,IPNT,MXL,MXI,MXR
  parameter (MXL = 1,MXI = 2,MXR = 1)
  logical :: LOGICS(*)
  real    :: WK3(MAXTRE)
  integer :: INTS(*)
  real    :: REALS(*)
  real    :: RDANUW
  logical :: LDANUW

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = REALS(1)
  LDANUW = LOGICS(1)

  !     GET THE INTEGER SCALARS.

  INTS(1) = KKTYPE
  INTS(2) = KOTFOR
  call IFWRIT (WK3, IPNT, ILIMIT, INTS, MXI, 2)

  !     GET THE LOGICAL SCALARS.

  !**   CALL LFWRIT (WK3, IPNT, ILIMIT, LOGICS, MXL, 2)

  !     GET THE REAL SCALARS.

  !**   CALL BFWRIT (WK3, IPNT, ILIMIT, REALS, MXR, 2)
  return
end
