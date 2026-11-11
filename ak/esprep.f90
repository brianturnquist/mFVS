!====================================================!
!   Predict default site preparation probabilities   !
!====================================================!
! AK $Id$
subroutine ESPREP (PNONE,PMECH,PBURN)
  implicit none

  !     PREDICT DEFAULT SITE PREP PROBABILITIES.

  ! Variables
  real    :: PBURN,PMECH,PNONE

  !     PROB(NO SITE PREP)

  PNONE = 1.0

  !     PROB(MECH SITE PREP)

  PMECH = 0.0

  !     PROB(BURN SITE PREP)

  PBURN = 0.0

  return
end subroutine ESPREP
