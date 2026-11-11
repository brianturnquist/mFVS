!========================!
!   fire/base : fmtrip   !
!========================!
! FIRE-BASE $Id$
subroutine FMTRIP (ITFN,I,WEIGHT)
  implicit none

  !  THIS FIRE/SNAGE MODEL SUBROUTINE IS USED
  !  TO TRIPLE THE FIRE/SNAG ATTRIBUTES

  !  CALLED BY :
  !     TRIPLE  [PROGNOSIS]

  !  CALLS     :
  !     FMATV

  !  PARAMETERS :
  !     ITFN   -
  !     I      -
  !     WEIGHT -

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'ARRAYS.f90'
  include 'FMCOM.f90'

  ! Variables
  integer :: I,ITFN,JJ
  real    :: WEIGHT

  !     NO TRIPLING IS REQUIRED IF THE FIRE/SNAG MODEL IS ABSENT

  if (.not. LFMON) return

  FMPROB(ITFN) = FMPROB(I) * WEIGHT
  FMICR(ITFN) = FMICR(I)

  OLDHT(ITFN) = OLDHT(I)
  OLDCRL(ITFN) = OLDCRL(I)
  GROW(ITFN) = GROW(I)

  do JJ = 0, 5
     OLDCRW(ITFN, JJ) = OLDCRW(I, JJ)
     CROWNW(ITFN, JJ) = CROWNW(I, JJ)
  end do

  return
end
