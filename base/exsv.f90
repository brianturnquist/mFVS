!=================================================!
!   Provide stand visualization extension stubs   !
!=================================================!
! BASE $Id$
subroutine EXSV
  implicit none

  !     STAND VISUALIZATION EXTERNAL REFERENCES
  !     N.L.CROOKSTON -- RMRS MOSCOW -- MARCH 1998
  !     A.H.DALLMANN -- RMRS MOSCOW -- JANUARY 2000

  ! Variables
  integer :: I1,I2,I
  logical :: L(*)
  real    :: A(*),A1(*),A2(*)
  character(len=*) :: C
  integer :: IDX(*)
  real    :: RDANUW
  integer :: IDANUW
  character(len=1) :: CDANUW
  logical :: LDANUW

  !  ENTRY SVCMP1

  entry SVCMP1
  return

  !  ENTRY SVCMP2

  entry SVCMP2(I1,I2)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = I1
  IDANUW = I2
  return

  !  ENTRY SVCMP3

  entry SVCMP3
  return

  !  ENTRY SVCUTS

  entry SVCUTS(I,A,A1,A2)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = I
  RDANUW = A(1)
  RDANUW = A1(1)
  RDANUW = A2(1)
  return

  !  ENTRY SVMORT

  entry SVMORT (I1, A, I2)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = I1
  IDANUW = I2
  RDANUW = A(1)
  return

  !  ENTRY SVOUT

  entry SVOUT(I1,I2,C)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = I1
  IDANUW = I2
  CDANUW = C(1:1)
  return

  !  ENTRY SVTOBJ

  entry SVTOBJ(I1)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = I1
  return

  !  ENTRY SVINIT

  entry SVINIT
  return

  !  ENTRY SVKEY

  entry SVKEY(C,L,A)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  CDANUW = C(1:1)
  LDANUW = L(1)
  RDANUW = A(1)
  return

  !  ENTRY SVSTART

  entry SVSTART
  return

  !  ENTRY SVTDEL

  entry SVTDEL(IDX,I2)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = IDX(1)
  IDANUW = I2
  return

  !  ENTRY SVTRIP

  entry SVTRIP(I1,I2)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = I1
  IDANUW = I2
  return

  !  ENTRY SVESTB

  entry SVESTB(I1)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = I1
  return

end subroutine EXSV
