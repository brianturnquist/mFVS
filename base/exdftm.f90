!==========================================!
!   Provide tussock moth extension stubs   !
!==========================================!
! BASE $Id$
subroutine EXDFTM
  implicit none

  !     EXTRA EXTERNAL REFERENCES FOR DFTM CALLS

  ! Variables
  integer :: II,MGMID,KEY
  integer :: ICODES(6)
  logical :: L,LKECHO
  character(len=8) :: KEYWRD
  character(len=8) :: NODFTM
  character(len=26) :: NPLT
  integer :: IDANUW
  character(len=8) :: CDANUW
  logical :: LDANUW

  data NODFTM/'*NO DFTM'/

  !  ENTRY TMINIT

  entry TMINIT
  return

  !  ENTRY DFTMIN

  entry DFTMIN(LKECHO)
  call ERRGRO (.true.,11)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  LDANUW = LKECHO
  return

  !  ENTRY TMDAM

  entry TMDAM (II,ICODES)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = II
  IDANUW = ICODES(1)
  return

  !  ENTRY TMOPS

  entry TMOPS
  return

  !  ENTRY TMCOUP

  entry TMCOUP
  return

  !  ENTRY DFTMGO

  entry DFTMGO (L)
  L = .false.
  return

  !  ENTRY TMBMAS

  entry TMBMAS
  return

  !  ENTRY TMHED

  entry TMHED (NPLT,MGMID)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  CDANUW(1:8) = NPLT(1:8)
  IDANUW = MGMID
  return

  !  ENTRY TMOUT

  entry TMOUT
  return

  !  ENTRY TMKEY

  entry TMKEY(KEY,KEYWRD)
  KEYWRD = NODFTM

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = KEY
  return

end subroutine EXDFTM
