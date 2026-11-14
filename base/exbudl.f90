!=====================================!
!   Provide budworm extension stubs   !
!=====================================!
! BASE $Id$
subroutine EXBUDL
  implicit none

  !     THE PURPOSE OF THIS SUBROUTINE IS TO SATISFY EXTRA EXTERNAL
  !     REFERENCES TO THE GENDEFOL/BUDWORM ("BUDLITE") MODEL

  ! Variables
  logical :: L,LKECHO
  integer :: KEY
  character(len=8) :: KEYWRD
  character(len=8) :: NOKEY
  integer :: IDANUW
  logical :: LDANUW

  data NOKEY/'*NO WSBE'/

  !  ENTRY BWECUP

  entry BWECUP
  return

  !  ENTRY BWEGO

  entry BWEGO (L)
  L = .false.
  return

  !  ENTRY BWEINT

  entry BWEINT
  return

  !  ENTRY BWEIN

  entry BWEIN(LKECHO)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  LDANUW = LKECHO
  return

  !  ENTRY BWEOUT

  entry BWEOUT
  return

  !  ENTRY BWEKEY

  entry BWEKEY (KEY, KEYWRD)
  KEYWRD = NOKEY

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = KEY
  return


end subroutine EXBUDL
