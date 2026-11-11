!=============================================!
!   Provide biogeochemistry extension stubs   !
!=============================================!
! BASE $Id$
subroutine EXBGC
  implicit none

  !     EXTERNAL REFERENCES FOR THE BGC MODEL (KELSEY MILNER)

  ! Variables
  integer :: I1,I2
  real    :: ARRAY(12)
  logical :: LNOTBK(12),L,LKECHO
  character(len=8) :: KEYWRD
  integer :: IDANUW
  real    :: RDANUW
  character(len=8) :: CDANUW
  logical :: LDANUW

  ! BGCINT CALLED FROM INITRE

  entry BGCINT
  return

  ! BGCIN CALLED FROM INITRE

  entry BGCIN(KEYWRD,ARRAY,LNOTBK,LKECHO)

  call ERRGRO (.true.,11)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  RDANUW = ARRAY(1)
  CDANUW(1:8) = KEYWRD(1:8)
  LDANUW = LKECHO
  LDANUW = LNOTBK(1)

  return

  ! BGCGO CALLED FROM GRINCR

  entry BGCGO(L)
  L = .false.
  return

  ! BGCGRO CALLED FROM GRINCR

  entry BGCGRO(I1,I2)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = I1
  IDANUW = I2

  return

  ! BGCFVS CALLED FROM GRADD

  entry BGCFVS(I1)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = I1

  return

end subroutine EXBGC
