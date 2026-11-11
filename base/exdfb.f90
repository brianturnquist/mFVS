!================================================!
!   Provide Douglas fir beetle extension stubs   !
!================================================!
! BASE $Id$
subroutine EXDFB
  implicit none

  !     EXTRA EXTERNAL REFERENCES FOR DFB CALLS

  ! Variables
  logical :: L,LNOTBK,LKECHO
  character(len=8) :: KEYWRD
  character(len=8) :: NODFB
  real    :: ARRAY(12)
  dimension LNOTBK(12)
  integer :: ICODES(6),II,KEY
  real    :: RDANUW
  integer :: IDANUW
  character(len=8) :: CDANUW
  logical :: LDANUW

  data NODFB/'*NO DFB'/

  !  ENTRY DFBINT

  entry DFBINT
  return

  !  ENTRY DFBIN

  entry DFBIN (KEYWRD,ARRAY,LNOTBK,LKECHO)
  call ERRGRO (.true.,11)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  CDANUW(1:8) = KEYWRD(1:8)
  RDANUW = ARRAY(1)
  LDANUW = LNOTBK(1)
  LDANUW = LKECHO
  return

  !  ENTRY DFBDAM

  entry DFBDAM (II,ICODES)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = II
  IDANUW = ICODES(1)
  return

  !  ENTRY DFBSCH

  entry DFBSCH
  return

  !  ENTRY DFBDRV

  entry DFBDRV
  return

  !  ENTRY DFBBO

  entry DFBGO (L)
  L = .false.
  return

  !  ENTRY DFBOUT

  entry DFBOUT
  return

  !  ENTRY DFBINV

  entry DFBINV
  return

  !  ENTRY DFBWIN

  entry DFBWIN (L)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  LDANUW = L
  return

  !  ENTRY DFBKEY

  entry DFBKEY (KEY,KEYWRD)
  KEYWRD = NODFB

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = KEY
  return

end subroutine EXDFB
