!=================================!
!   Read variant specific state   !
!=================================!
! PG $Id$
subroutine VARGET (WK3,IPNT,ILIMIT,REALS,LOGICS,INTS)
  implicit none

  !     READ THE VARIANT SPECIFIC VARIABLES.

  !     PART OF THE PARALLEL PROCESSING EXTENSION TO PROGNOSIS.

  ! Common blocks
  include 'PRGPRM.f90'

  !     NOTE: THE ACTUAL STORAGE LIMIT FOR INTS, LOGICS, AND REALS
  !     IS MAXTRE (SEE PRGPRM).

  ! Variables
  integer :: ILIMIT,IPNT,MXL,MXI,MXR
  parameter (MXL = 1,MXI = 1,MXR = 1)
  logical :: LOGICS(*)
  real    :: WK3(MAXTRE)
  integer :: INTS(*)
  real    :: REALS(*)
  logical :: LDANUW
  real    :: RDANUW
  integer :: IDANUW

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  IDANUW = ILIMIT
  IDANUW = INTS(1)
  IDANUW = IPNT
  LDANUW = LOGICS(1)
  RDANUW = REALS(1)
  RDANUW = WK3(1)

  return
end

subroutine VARCHGET (CBUFF, IPNT, LNCBUF)
  implicit none

  !     Get variant-specific character data

  ! Common blocks
  include 'PRGPRM.f90'

  ! Variables
  integer :: LNCBUF
  character(len=1) :: CBUFF(LNCBUF)
  character(len=1) :: CDANUW
  integer :: IDANUW
  integer :: IPNT
  ! Stub for variants which need to get/put character data
  ! See /bc/varget.f and /bc/varput.f for examples of VARCHGET and VARCHPUT

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  CDANUW = CBUFF(1)
  IDANUW = IPNT
  IDANUW = LNCBUF

  return
end
