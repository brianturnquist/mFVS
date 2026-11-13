!=================================!
!   Read variant specific state   !
!=================================!
! AK $Id$
subroutine VARGET (WK3,IPNT,ILIMIT,REALS,LOGICS,INTS)
  implicit none

  !     READ THE VARIANT SPECIFIC VARIABLES.

  !     PART OF THE PARALLEL PROCESSING EXTENSION TO PROGNOSIS.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ESCOMN.f90'
  include 'PLOT.f90'
  include 'VARCOM.f90'

  !     NOTE: THE ACTUAL STORAGE LIMIT FOR INTS, LOGICS, AND REALS
  !     IS MAXTRE (SEE PRGPRM).

  ! Variables
  integer :: ILIMIT,IPNT,MXL,MXI,MXR
  parameter (MXL = 1,MXI = 2,MXR = 5)
  logical :: LOGICS(*)
  real    :: WK3(MAXTRE)
  integer :: INTS(*)
  real    :: REALS(*)
  logical :: LDANUW
  real    :: RDANUW

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  LDANUW = LOGICS(1)
  RDANUW = REALS(1)
  RDANUW = WK3(1)

  !     GET THE INTEGER SCALARS.

  call IFREAD (WK3, IPNT, ILIMIT, INTS, MXI, 2)
  IIFORTP = INTS ( 1)
  IFT0 = INTS ( 2)

  !     GET THE INTEGER ARRAYS.

  !     GET THE LOGICAL SCALARS.

  !**   CALL LFREAD (WK3, IPNT, ILIMIT, LOGICS, MXL, 2)

  !     GET THE REAL SCALARS.

  !**   CALL BFREAD (WK3, IPNT, ILIMIT, REALS, MXR, 2)
  !**           = REALS( 1)

  !     READ THE REAL ARRAYS

  call BFREAD (WK3, IPNT, ILIMIT, OCURFT,    MAXSP*14, 2) ! from ESCOMN.F77
  call BFREAD (WK3, IPNT, ILIMIT, HTT11,     MAXSP,    2) ! from ESCOMN.F77
  call BFREAD (WK3, IPNT, ILIMIT, HTT12,     MAXSP,    2)
  call BFREAD (WK3, IPNT, ILIMIT, HTT13,     MAXSP,    2)
  call BFREAD (WK3, IPNT, ILIMIT, XMAXPT,    MAXPLT,   2)

  return
end subroutine VARGET
