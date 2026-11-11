!=================!
!   covr : cvgo   !
!=================!
! COVR $Id$
subroutine CVGO (LCVATV)
  implicit none

  !     FIND OUT IF THE COVER/BROWSE MODELS WILL BE ROUTINELY
  !     CALLED DURING THE PROJECTION.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CVCOM.f90'

  ! Variables
  logical :: LCVATV
  integer :: NPRMS,KODE,ISTAT,IDT,NTIMES

  !     IF LCOV IS TRUE, THEN LCVATV MUST ALSO BE TRUE.

  LCVATV = LCOV
  if (LCVATV) return

  !     FIND OUT IF THE "COVER" ACTIVITY IS IN THE ACTIVITY SCHEDULE
  !     OR IF IT IS SET UP TO BE SCHEDULED AFTER AN EVENT IS SPECIFIED.

  NPRMS = 1
  call OPSTUS (900,0,29999,1,NTIMES,IDT,NPRMS,ISTAT,KODE)
  LCVATV = NTIMES.gt.0
  if (LCVATV) return
  call OPEVAC (900,NTIMES)
  LCVATV = NTIMES.gt.0
  return
end
