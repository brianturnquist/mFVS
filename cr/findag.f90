!=======================================!
!   Estimate tree age and site height   !
!=======================================!
! CR $Id$
subroutine FINDAG(I,ISPC,D1,D2,H,SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,&
     &DEBUG)
  implicit none

  !  THIS ROUTINE FINDS EFFECTIVE TREE AGE BASED ON INPUT VARIABLE(S)
  !  CALLED FROM **COMCUP
  !  CALLED FROM **CRATET

  !ALLS **FNDAG

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'GGCOM.f90'

  ! Variables
  logical :: DEBUG
  integer :: I,ISPC,ICLS
  real    :: BAUTBA,SITE,EFFAGE
  real    :: D1,D2,H,SITAGE,SITHT,AGMAX,HTMAX,HTMAX2
  real    :: RDANUW

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = AGMAX
  RDANUW = D2
  RDANUW = HTMAX
  RDANUW = HTMAX2
  RDANUW = SITHT

  ICLS = IFIX(D1+1.0)
  if (ICLS .gt.41) ICLS = 41
  BAUTBA = BAU(ICLS)/BA
  if (BAUTBA.lt.0.0)BAUTBA = 0.0
  SITE = SITEAR(ISPC)
  call FNDAG(I,EFFAGE,SITE,H,BAUTBA,ISPC,DEBUG)
  if (DEBUG)write(JOSTND,*)' IN FINDAG I,EFFAGE,SITE,H,',&
       &'BAUTBA,ISPC= ',I,EFFAGE,SITE,H,BAUTBA,ISPC
  SITAGE = EFFAGE
  if (SITAGE .le.0.0) SITAGE = 1.0

  return
end
