!=========================================!
!   Provide pine beetle extension stubs   !
!=========================================!
! BASE $Id$
subroutine EXBM
  implicit none

  !     EXTERNAL REFERENCES FOR THE Westwide Pine Beetle model

  ! Variables
  integer :: I3(6),JOPPRT,IRECNT,I2,I1,KEY

  character(len=8) :: NOBM
  character(len=8) :: KEYWRD
  logical :: LACTV,LKECHO
  real    :: XBM1,XBM2
  real    :: RDANUW
  integer :: IDANUW

  data NOBM/'**NO BM '/

  ! BMSLSH CALLED FROM CUTS

  entry BMSLSH (I1,XBM1,XBM2,I2)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = I1
  IDANUW = I2
  RDANUW = XBM1
  RDANUW = XBM2
  return

  ! BMTRIP CALLED FROM TRIPLE

  entry BMTRIP (I1,I2,XBM1)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = I1
  IDANUW = I2
  RDANUW = XBM1
  return

  ! BMKEY CALLED FROM OPLIST

  entry BMKEY (KEY,KEYWRD)
  KEYWRD = NOBM

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = KEY
  return

  ! BMDAM  CALLED FROM DAMCDS

  entry BMDAM (I1,I3)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = I1
  IDANUW = I3(1)
  return

  ! BMIN  CALLED FROM INITRE

  entry BMIN(LKECHO)
  if (LKECHO)write(JOPPRT,20) IRECNT,NOBM
  20 format (/1X,A8,'RECORD: ', I6, A8)
  call ERRGRO (.true.,11)
  return

  ! BMLNKD  CALLED FROM DBSIN

  entry BMLNKD(LACTV)
  LACTV = .false.
  return

end subroutine EXBM
