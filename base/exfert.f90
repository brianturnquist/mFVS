!========================================!
!   Provide fertilizer extension stubs   !
!========================================!
! BASE $Id$
subroutine EXFERT
  implicit none

  !     EXTRA EXTERNAL REFERENCES FOR FERTILIZER CALLS

  ! Variables
  logical :: LNOTBK(12),LKECHO
  real    :: ARRAY(12)
  integer :: IRECNT,JOSTND
  character(len=8) :: KEYWRD
  character(len=10) :: KARD(12)
  real    :: RDANUW
  integer :: IDANUW
  character(len=1) :: CDANUW
  logical :: LDANUW

  ! ENTRY FFIN

  entry FFIN (JOSTND,IRECNT,KEYWRD,ARRAY,LNOTBK,KARD,LKECHO)
  call ERRGRO (.true.,11)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = JOSTND
  IDANUW = IRECNT
  CDANUW = KEYWRD(1:1)
  RDANUW = ARRAY(1)
  LDANUW = LNOTBK(1)
  CDANUW = KARD(1)(1:1)
  LDANUW = LKECHO
  return

  ! ENTRY FFIN

  entry FFERT
  return

end subroutine EXFERT
