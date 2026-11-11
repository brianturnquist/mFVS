!========================!
!   fire/base : fmtdel   !
!========================!
! FIRE-BASE $Id$
subroutine FMTDEL (IVAC,IREC)
  implicit none

  !     THIS SUBROUTINE IS USED TO DELETE TREE RECORDS BY MOVING
  !     RECORD IREC TO POSITION IVAC.

  !     THE FIRE/SNAG MODEL.

  !  CALLED BY :
  !     TREDEL

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'ARRAYS.f90'
  include 'FMCOM.f90'

  ! Variables
  integer :: IREC,IVAC,JJ

  if (.not. LFMON) return

  !     fmprob and fmicr MAY not need to be compressed, tre-del'ed, etc???

  FMPROB(IVAC) = FMPROB(IREC)
  FMICR(IVAC) = FMICR(IREC)

  OLDHT(IVAC) = OLDHT(IREC)
  OLDCRL(IVAC) = OLDCRL(IREC)
  GROW(IVAC) = GROW(IREC)

  do JJ = 0, 5
     OLDCRW(IVAC, JJ) = OLDCRW(IREC, JJ)
     CROWNW(IVAC, JJ) = CROWNW(IREC, JJ)
  end do

  return
end
