!==================================================!
!   Provide mountain pine beetle extension stubs   !
!==================================================!
! BASE $Id$
subroutine EXMPB
  implicit none

  !     EXTRA EXTERNAL REFERENCES FOR MPB CALLS

  ! Variables
  logical :: L,LNOTBK,LKECHO
  character(len=8) :: KEYWRD
  character(len=8) :: NOMPB
  dimension LNOTBK(12)
  integer :: ICODES(6)
  real    :: ARRAY(12)
  integer :: II,KEY
  real    :: RDANUW
  integer :: IDANUW
  character(len=8) :: CDANUW
  logical :: LDANUW

  data NOMPB/'*NO MPB'/

  !  ENTRY MPBINT

  entry MPBINT
  return


  !  ENTRY MPBIN

  entry MPBIN (KEYWRD,ARRAY,LNOTBK,LKECHO)
  call ERRGRO (.true.,11)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  CDANUW(1:8) = KEYWRD(1:8)
  RDANUW = ARRAY(1)
  LDANUW = LNOTBK(1)
  LDANUW = LKECHO
  return

  !  ENTRY MPBDAM

  entry MPBDAM (II,ICODES)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = II
  IDANUW = ICODES(1)
  return

  !  ENTRY MPBOPS

  entry MPBOPS
  return

  !  ENTRY MPBCUP

  entry MPBCUP
  return

  !  ENTRY MPBGO

  entry MPBGO (L)
  L = .false.
  return

  !  ENTRY MPBOUT

  entry MPBOUT
  return

  !  ENTRY MPSDLP

  entry MPSDLP
  return

  !  ENTRY MPKEY

  entry MPKEY(KEY,KEYWRD)
  KEYWRD = NOMPB

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = KEY
  return

end subroutine EXMPB
