!==============================================!
!   Provide vegetation cover extension stubs   !
!==============================================!
! BASE $Id$
subroutine EXCOV
  implicit none

  !  EXTRA EXTERNAL REFERENCES FOR CVCNOP AND CVBROW CALLS.

  ! Variables
  real    :: ARRAY(12)
  real    :: XDUM(*)
  integer :: KEY,I,J,JJ,II
  logical :: LTHIN,LACTV,LNOTBK(12),LKECHO
  character(len=8) :: KEYWRD
  character(len=8) :: NOCOV
  real    :: RDANUW
  integer :: IDANUW
  character(len=8) :: CDANUW
  logical :: LDANUW

  data NOCOV/'*NO COVR'/

  !  ENTRY CVINIT

  entry CVINIT
  return

  !  ENTRY CVIN

  entry CVIN (KEYWRD,ARRAY,LNOTBK,LKECHO)
  call ERRGRO (.true.,11)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  CDANUW = KEYWRD
  RDANUW = ARRAY(1)
  LDANUW = LNOTBK(1)
  LDANUW = LKECHO
  return

  !  ENTRY CVGO

  entry CVGO (LACTV)
  LACTV = .false.
  return

  !  ENTRY CVCNOP

  entry CVCNOP(LTHIN)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  LDANUW = LTHIN
  return

  !  ENTRY CVBROW

  entry CVBROW(LTHIN)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  LDANUW = LTHIN
  return

  !  ENTRY CVOUT

  entry CVOUT
  return

  !  ENTRY CVKEY

  entry CVKEY(KEY,KEYWRD)
  KEYWRD = NOCOV

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = KEY
  return

  !     REFERENCES FOR THE PARALLEL PROCESSING SYSTEM.

  !  ENTRY CVGET

  entry CVGET (XDUM, I, J)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  RDANUW = XDUM(1)
  IDANUW = I
  IDANUW = J
  return

  !  ENTRY CVPUT

  entry CVPUT (XDUM, I, J, II, JJ)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  RDANUW = XDUM(1)
  IDANUW = I
  IDANUW = J
  IDANUW = II
  IDANUW = JJ
  return

  !  ENTRY CVACTV

  entry CVACTV (LACTV)
  LACTV = .false.
  return

end subroutine EXCOV
