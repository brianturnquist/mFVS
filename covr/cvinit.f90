!===================!
!   covr : cvinit   !
!===================!
! COVR $Id$
subroutine CVINIT
  implicit none

  !  INITIALIZES COVER VARIABLES FOR THE CURRENT STAND.
  !  CALLED FROM **INITRE**.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'CVCOM.f90'

  ! Variables
  integer :: I,J
  ICVBGN = 0
  JOSHRB = JOSTND

  !     VARIABLES ADDED TO CONTROL PRINTING NOHEADING OUTPUT OF COVER.

  JCVNOH = 24
  LCVNOH = .false.

  LBROW = .false.
  LCOV = .false.
  LCNOP = .false.
  LCOVER = .true.
  LSHOW = .false.
  LSHRUB = .true.
  LCVSUM = .true.
  LCAL1 = .false.
  LCAL2 = .false.
  LCALIB = .false.
  do I = 1,3
     AVGBHT(I) = -99999.0
     AVGBPC(I) = -99999.0
     10 continue
  end do
  do I = 1,31
     SHRBHT(I) = -99999.0
     SHRBPC(I) = -99999.0
     BHTCF(I) = 1.0
     BPCCF(I) = 1.0
     15 continue
  end do
  do J = 1,MAXCY1
     LTHIND(J) = .false.
     20 continue
  end do
  return
end
