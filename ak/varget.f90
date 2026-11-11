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
  include 'ESPARM.f90'
  include 'ESCOMN.f90'
  include 'PLOT.F77'
  include 'VARCOM.F77'

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

subroutine VARCHGET (CBUFF, IPNT, LNCBUF)
  implicit none

  !     Get variant-specific character data

  ! Common blocks
  include 'PRGPRM.f90'

  ! Variables
  integer :: LNCBUF
  character(len=1) :: CBUFF(LNCBUF)
  integer :: IPNT
  integer :: IDANUW
  character(len=1) :: CDANUW
   ! Stub for variants which need to get/put character data
   ! See /bc/varget.f and /bc/varput.f for examples of VARCHGET and VARCHPUT

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  IDANUW = IPNT
  CDANUW = CBUFF(1)

  return
end subroutine VARCHGET
