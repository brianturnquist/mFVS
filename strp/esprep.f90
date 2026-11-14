!====================================================!
!   Predict default site preparation probabilities   !
!====================================================!
! STRP $Id$
subroutine ESPREP (ISER,PNONE,PMECH,PBURN)
  implicit none

  !     PREDICT DEFAULT SITE PREP PROBABILITIES.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ESPARM.f90'
  include 'ESCOMN.f90'
  include 'PLOT.f90'

  ! Variables
  real    :: PBURN,PMECH,PNONE
  integer :: ISER
  integer :: IDANUW

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  IDANUW = ISER

  !     PROB(NO SITE PREP)

  PNONE = 0.75

  !     PROB(MECH SITE PREP)

  PMECH = 0.20

  !     PROB(BURN SITE PREP)

  PBURN = 0.05
  return
end
