!=====================================!
!   Provide Organon extension stubs   !
!=====================================!
! BASE $Id$
subroutine EXORGANON
  implicit none

  !  SATISFY EXTERNAL REFRENCES FOR THE ORGANON EXTENSION

  ! Variables
  logical :: DEBUG,LKECHO
  integer :: I,ITFN,JOSTND,IMODTY
  real    :: VAL
  integer :: IDANUW
  logical :: LDANUW

  !  ENTRY ORIN   CALLED FROM INITRE

  entry ORIN(DEBUG,LKECHO)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  LDANUW = DEBUG
  LDANUW = LKECHO
  return

  !  ENTRY ORGTRIP   CALLED FROM TRIPLE

  entry ORGTRIP(I,ITFN)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = I
  IDANUW = ITFN
  return

  !  ENTRY ORGTAB   CALLED FROM INITRE

  entry ORGTAB(JOSTND,IMODTY)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = JOSTND
  IDANUW = IMODTY
  return

  !  ENTRY GETORGV   CALLED FROM EVTSTV

  entry GETORGV(I,VAL)
  VAL = 0.

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = I
  return

end subroutine EXORGANON
