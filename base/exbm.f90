!=========================================!
!   Provide pine beetle extension stubs   !
!=========================================!
! BASE $Id$
subroutine EXBM
  implicit none

  !     EXTERNAL REFERENCES FOR THE Westwide Pine Beetle model

  ! Variables
  integer :: I3(6),JOPPRT,IRECNT,IREAD,I2,I1,KEY,IOUT,I

  character(len=8) :: NOBM
  character(len=8) :: KEYWRD
  logical :: LACTV,LKECHO,LTEE
  real    :: R1(*),XBM1,XBM2
  real    :: RDANUW
  integer :: IDANUW

  data NOBM/'**NO BM '/

  ! BMSDIT CALLED FROM PPMAIN.

  entry BMSDIT
  return

  ! BMKILL CALLED FROM PPMAIN

  entry BMKILL
  return

  ! BMSETP CALLED FROM ALSTD1

  entry BMSETP
  return

  ! BMDRV CALLED FROM ALSTD2

  entry BMDRV
  return

  ! BMPPIN CALLED FROM PPIN

  entry BMPPIN(IREAD,IRECNT,JOPPRT)
  write(JOPPRT,10) IRECNT,NOBM
  call ERRGRO (.true.,11)
  10 format (/1X,A8,'RECORD: ', I6, A8)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = IREAD
  return

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

  ! BMPPPT CALLED FROM PUTSTD

  entry BMPPPT (I1,I2)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = I1
  IDANUW = I2
  return

  ! BMPPGT CALLED FROM GETSTD

  entry BMPPGT (I1,I2)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = I1
  IDANUW = I2
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

  ! BMDBS--CALLED FROM DBSIN

  entry BMDBS(I,IOUT)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = I
  IDANUW = IOUT
  return

  ! RRATV CALLED FROM GETSTD AND PUTSTD

  entry RRATV(LACTV,LTEE)
  LACTV = .false.
  LTEE = .false.
  return

  ! RRPPPT CALLED FROM PUTSTD

  entry RRPPPT(R1,I1,I2)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = I1
  IDANUW = I2
  RDANUW = R1(1)
  return

  ! RRPPGT CALLED FROM GETSTD

  entry RRPPGT(R1,I1,I2)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = I1
  IDANUW = I2
  RDANUW = R1(1)
  return

end subroutine EXBM
