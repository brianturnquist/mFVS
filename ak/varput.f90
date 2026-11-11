!==================================!
!   Write variant specific state   !
!==================================!
! AK $Id$
subroutine VARPUT (WK3,IPNT,ILIMIT,REALS,LOGICS,INTS)
  implicit none

  !     WRITE THE VARIANT SPECIFIC VARIABLES.

  !     PART OF THE PARALLEL PROCESSING EXTENSION TO PROGNOSIS.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ESPARM.f90'
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

  !     STORE THE INTEGER SCALARS IN THE ARRAY INTS

  INTS ( 1) = IIFORTP ! from PLOT.F77
  INTS ( 2) = IFT0 ! from ESCOMN.F77
  call IFWRIT (WK3, IPNT, ILIMIT, INTS, MXI, 2)

  !     WRITE THE INTEGER ARRAYS

  !     STORE THE LOGICAL SCALARS.

  !**   CALL LFWRIT (WK3, IPNT, ILIMIT, LOGICS, MXL, 2)

  !     STORE THE REAL SCALARS IN THE ARRAY REALS.

  !**   REALS( 1) =
  !**   CALL BFWRIT (WK3, IPNT, ILIMIT, REALS, MXR, 2)

  !     WRITE THE REAL ARRAYS

  call BFWRIT (WK3, IPNT, ILIMIT, OCURFT,    MAXSP*14, 2) ! from ESCOMN.F77
  call BFWRIT (WK3, IPNT, ILIMIT, HTT11,     MAXSP,    2) ! from ESCOMN.F77
  call BFWRIT (WK3, IPNT, ILIMIT, HTT12,     MAXSP,    2)
  call BFWRIT (WK3, IPNT, ILIMIT, HTT13,     MAXSP,    2)
  call BFWRIT (WK3, IPNT, ILIMIT, XMAXPT,    MAXPLT,   2)

  return
end subroutine VARPUT

subroutine VARCHPUT (CBUFF, IPNT, LNCBUF)
  implicit none

  !     Put variant-specific character data

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
end subroutine VARCHPUT
