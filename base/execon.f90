!======================================!
!   Provide economic extension stubs   !
!======================================!
! BASE $Id$
subroutine EXECON
  implicit none

  !     EXTRA EXTERNAL REFERENCES FOR THE ECON EXTENSION.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ECNCOM.f90'

  ! Variables
  integer :: I1,I2,I3,ICYC,IT,IS,II,IRECNT,KEY
  real    :: PREM2,GROSPC,ARRAY22,D
  logical :: LKECHO,LPRTND,LTMP
  character(len=4) :: CARRAY(*)
  character(len=4) :: CH1_4
  character(len=4) :: CARRAY2(*)
  character(len=8) :: KEYWRD
  character(len=26) :: NPLT
  character(len=72) :: ITITLE
  real    :: RDANUW
  integer :: IDANUW
  character(len=1) :: CDANUW
  logical :: LDANUW

  real    :: ARRAY(*),ARRAY1(*)
  integer :: IY(*),IARRAY(*)

  ! ENTRY ECSETP CALLED FROM CUTS

  entry ECSETP(IY)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = IY(1)
  return

  ! ENTRY ECSETP CALLED FROM GRINCR

  entry ECSTATUS(I3, I2, IARRAY, I1)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = I3
  IDANUW = I2
  IDANUW = IARRAY(1)
  IDANUW = I1
  return

  ! ENTRY ECHARV CALLED FROM CUTS

  entry ECHARV (ARRAY,D,ARRAY22,GROSPC,PREM2,IS, &
       & IT,ICYC,IY)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  RDANUW = ARRAY(1)
  RDANUW = D
  RDANUW = ARRAY22
  RDANUW = GROSPC
  RDANUW = PREM2
  IDANUW = IS
  IDANUW = IT
  IDANUW = ICYC
  IDANUW = IY(1)
  return

  ! ENTRY ECCALC CALLED FROM GRADD

  entry ECCALC(IARRAY,II,CARRAY, CH1_4, NPLT, ITITLE)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = IARRAY(1)
  IDANUW = II
  CDANUW = CARRAY(1)(1:1)
  CDANUW = CH1_4(1:1)
  CDANUW = NPLT(1:1)
  CDANUW = ITITLE(1:1)
  return

  ! ENTRY ECLOAD CALLED FROM CUTS

  entry GETISPRETENDACTIVE(LPRTND)
  LPRTND = .false.
  return

  ! ENTRY ECIN CALLED FROM INITRE

  entry ECIN(IRECNT,I1,I2,CARRAY2,I3,LKECHO,IARRAY)
  call ERRGRO (.true.,11)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = IRECNT
  IDANUW = I1
  IDANUW = I2
  CDANUW = CARRAY2(1)(1:1)
  IDANUW = I3
  LDANUW = LKECHO
  IDANUW = IARRAY(1)
  return

  ! ENTRY ECKEY CALLED FROM OPLIST

  entry ECKEY (KEY,KEYWRD)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  CDANUW = KEYWRD(1:1)
  IDANUW = KEY
  return

  ! ENTRY ECINIT CALLED FROM INITRE

  entry ECINIT
  isEconToBe = .false.
  return

  ! ENTRY ECVOL CALLED FROM VOLS

  entry ECVOL (I1,I2,ARRAY,ARRAY1,LTMP)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = I1
  IDANUW = I2
  RDANUW = ARRAY(1)
  RDANUW = ARRAY1(1)
  LDANUW = LTMP
  return

  ! ENTRY ECNGET CALLED FROM GETSTD

  entry ECNGET (ARRAY,I1,I2)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  RDANUW = ARRAY(1)
  IDANUW = I1
  IDANUW = I2
  return

  ! ENTRY ECNPUT CALLED FROM PUTSTD

  entry ECNPUT (ARRAY,I1,I2)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  RDANUW = ARRAY(1)
  IDANUW = I1
  IDANUW = I2
  return

end subroutine EXECON
