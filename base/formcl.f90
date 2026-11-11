!=================================!
!   Compute volume form factors   !
!=================================!
! BASE $Id$
subroutine FORMCL(ISPC,IFOR,D,FC)
  implicit none

  ! THIS PROGRAM CALCULATES FORM FACTORS FOR CALCULATING CUBIC AND
  ! BOARD FOOT VOLUMES.

  ! THIS IS THE VANILLA VERSION FOR USE IN VARIANTS THAT DON'T USE
  ! FORM CLASS, OR ONLY SET DEFAULT FORM CLASSES BY SPECIES.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'

  ! Variables
  integer :: IFOR,ISPC
  real    :: FC,D
  real    :: RDANUW
  integer :: IDANUW

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = D
  IDANUW = IFOR

  !  LOAD FORM CLASS VALUE

  FC = FRMCLS(ISPC)

  return
end subroutine FORMCL
