!=======================================!
!   Estimate tree age and site height   !
!=======================================!
! KT $Id$
subroutine FINDAG(I,ISPC,D1,D2,H,SITAGE,SITHT,AGMAX,HTMAX1,HTMAX2,&
     &DEBUG)
  implicit none

  !  IN THE NI VARIANT, FINDAG IS A DUMMY ROUTINE THAT RETURNS WITHOUT
  !  EXECUTING ANY COMPUTATIONS.  IN OTHER VARIANTS THIS ROUTINE
  !  MAY SET EFFECTIVE TREE AGE BASED ON INPUT VARIABLE(S)
  !  SUCH AS TREE HEIGHT.
  !  CALLED FROM **COMCUP
  !  CALLED FROM **CRATET

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'

  ! Variables
  logical :: DEBUG
  integer :: I,ISPC
  real    :: D1,D2,H,SITAGE,SITHT,AGMAX,HTMAX1,HTMAX2
  real    :: RDANUW
  integer :: IDANUW

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = AGMAX
  RDANUW = D1
  RDANUW = D2
  RDANUW = H
  RDANUW = HTMAX1
  RDANUW = HTMAX2
  IDANUW = ISPC

  if (DEBUG)write(JOSTND,50)I,SITAGE,SITHT
  50 format(' LEAVING SUBROUTINE FINDAG  I,SITAGE,SITHT =',I5,2F10.3)

  return
end
  !**END OF CODE SEGMENT
