!===========================================!
!   Read variant specific character state   !
!===========================================!
! PG $Id$
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
end subroutine VARCHGET
