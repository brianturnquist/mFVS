!==================================!
!   Write variant specific state   !
!==================================!
! CR $Id$
subroutine VARPUT (WK3,IPNT,ILIMIT,REALS,LOGICS,INTS)
  implicit none

  !     WRITE THE VARIANT SPECIFIC VARIABLES.

  !     PART OF THE PARALLEL PROCESSING EXTENSION TO PROGNOSIS.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'GGCOM.f90'

  !     NOTE: THE ACTUAL STORAGE LIMIT FOR INTS, LOGICS, AND REALS
  !     IS MAXTRE (SEE PRGPRM).

  ! Variables
  integer :: ILIMIT,IPNT,MXL,MXI,MXR
  parameter (MXL = 1,MXI = 1,MXR = 5)
  logical :: LOGICS(*)
  real    :: WK3(MAXTRE)
  integer :: INTS(*)
  real    :: REALS(*)
  logical :: LDANUW

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  LDANUW = LOGICS(1)

  !     STORE THE INTEGER SCALARS IN THE ARRAY INTS

  INTS ( 1) = IGFOR
  call IFWRIT (WK3, IPNT, ILIMIT, INTS, MXI, 2)

  !     WRITE THE INTEGER ARRAYS

  !     STORE THE LOGICAL SCALARS.

  !**   CALL LFWRIT (WK3, IPNT, ILIMIT, LOGICS, MXL, 2)

  !     STORE THE REAL SCALARS IN THE ARRAY REALS.

  REALS( 1) = AGERNG
  REALS( 2) = SEEDS
  REALS( 3) = TPAT
  REALS( 4) = DSTAG
  REALS( 5) = BAINIT
  call BFWRIT (WK3, IPNT, ILIMIT, REALS, MXR, 2)

  !     WRITE THE REAL ARRAYS

  call BFWRIT (WK3, IPNT, ILIMIT, BCLAS,  MAXSP*41, 2)
  call BFWRIT (WK3, IPNT, ILIMIT, TBA,    MAXSP,    2)
  call BFWRIT (WK3, IPNT, ILIMIT, BAU,    41,       2)
  call BFWRIT (WK3, IPNT, ILIMIT, BARK1,  MAXSP,    2)
  call BFWRIT (WK3, IPNT, ILIMIT, BARK2,  MAXSP,    2)
  call BFWRIT (WK3, IPNT, ILIMIT, DBHMAX, MAXSP,    2)
  call BFWRIT (WK3, IPNT, ILIMIT, TCLAS,  MAXSP*41, 2)
  call BFWRIT (WK3, IPNT, ILIMIT, BREAK,  MAXSP,    2)
  call BFWRIT (WK3, IPNT, ILIMIT, SITELO, MAXSP,    2)
  call BFWRIT (WK3, IPNT, ILIMIT, SITEHI, MAXSP,    2)

  return
end

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
end
