!=========================================!
!   Predict regenerating trees per plot   !
!=========================================!
! ESTB $Id$
subroutine ESTPP (VAL,TPP)
  implicit none

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ESPARM.f90'
  include 'ESCOMN.f90'
  include 'ESCOM2.f90'

  ! Variables
  real    :: TPP,VAL,BB,CC

  !     CALCULATION OF TREES PER STOCKED PLOT.

  BB = 0.697061 +.715825*XCOS -.203452*XSIN -2.762848*SLO&
       &+.041235*REGT +.031435*BWAF
  if (IHAB.gt.4.and.IHAB.lt.9) BB = BB+.294473
  if (IHAB.eq.9.or.IHAB.eq.10) BB = BB+1.237000
  if (IHAB.gt.10) BB = BB+.465788
  BB = EXP(BB)

  CC = 0.6836

  TPP = ((-(ALOG(1.0-VAL)))**(1.0/CC)) *BB   +0.9
  return
end
