!=================!
!   rd : rdtrip   !
!=================!
! RD $Id$
subroutine RDTRIP (ITFN,I,WEIGHT)
  implicit none

  !  THIS ROOT DISEASE MODEL SUBROUTINE IS USED
  !  TO TRIPLE THE ROOT DISEASE TREE LISTS.

  !  CALLED BY :
  !     TRIPLE  [PROGNOSIS]

  !  CALLS     :
  !     NONE

  !  PARAMETERS :
  !     ITFN   -
  !     I      -
  !     WEIGHT -

  !  Revision History:
  !    22-JUL-02 Lance R. David (FHTET)
  !      Previous revision date noted was 11/06/89.
  !      Removed unused array PROBO. It was also unused in the old
  !      annosus model.
  !   09/04/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.
  !   03/01/2016 Lance R. David (FMSC)
  !     Moved one condition to exit to top.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'
  include 'CONTRL.f90'
  include 'RDCOM.f90'
  include 'RDADD.f90'
  include 'RDARRY.f90'

  ! Variables
  integer :: I,IDI,ITFN,ITYP,J,JINF
  real    :: TPAREA,WEIGHT

  if (IROOT .eq.0) return

  TPAREA = 0.0
  do IDI = MINRR,MAXRR
     TPAREA = TPAREA + PAREA(IDI)
     59 continue
  end do
  if (TPAREA .eq.0) return

  if (WEIGHT .eq.0.6) ITFN = I

  FPROB(ITFN) = FPROB(I) * WEIGHT
  FFPROB(ITFN,1) = FFPROB(I,1) * WEIGHT
  FFPROB(ITFN,2) = FFPROB(I,2) * WEIGHT
  RROOTT(ITFN) = RROOTT(I)
  WK22(ITFN) = WK22(I) * WEIGHT

  do J = 1,4
     ROOTH(J,ITFN) = ROOTH(J,I)
     XMTH(J,ITFN) = XMTH(J,I) * WEIGHT
     100 continue
  end do

  ROOTL(ITFN) = ROOTL(I)
  RRKILL(ITFN) = RRKILL(I) * WEIGHT
  RDKILL(ITFN) = RDKILL(I) * WEIGHT
  PROBIT(ITFN) = PROBIT(I) * WEIGHT
  PROBIU(ITFN) = PROBIU(I) * WEIGHT
  PROBL(ITFN) = PROBL(I) * WEIGHT

  !     WRITE (JOSTND,200) FPROB(ITFN),FPROB(I),PROBIU(ITFN),PROBIU(I)
  ! 200 FORMAT (' IN RDTRIP :  FPROB(ITFN),FPROB(I),PROBIU',4F8.2)

  !     OAKL ALSO NEEDS TO BE TRIPLED SINCE IT IS USED (AS PROAKL) NEXT TIME

  do ITYP = 1,3
     OAKL(ITYP,ITFN) = OAKL(ITYP,I) * WEIGHT
     250 continue
  end do

  do JINF = 1,ISTEP
     PROBI(ITFN,JINF,1) = PROBI(I,JINF,1) * WEIGHT
     PROPI(ITFN,JINF,1) = PROPI(I,JINF,1)
     PROBI(ITFN,JINF,2) = PROBI(I,JINF,2) * WEIGHT
     PROPI(ITFN,JINF,2) = PROPI(I,JINF,2)
     300 continue
  end do

  return
end
