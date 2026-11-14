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
  integer :: I1,I2,I3,KEY
  real    :: R1
  character(len=8) :: KEYWRD
  character(len=8) :: NOKEY
  real    :: RDANUW
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

  !  ENTRY BWEPPATV

  entry BWEPPATV (L)
  L = .false.
  return

  !  ENTRY BWEPPGT

  entry BWEPPGT (R1,I1,I2,I3)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  RDANUW = R1
  IDANUW = I1
  IDANUW = I2
  IDANUW = I3
  return

  !  ENTRY BWEPPPT

  entry BWEPPPT (R1,I1,I2,I3)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  RDANUW = R1
  IDANUW = I1
  IDANUW = I2
  IDANUW = I3
  return

end subroutine EXBUDL
