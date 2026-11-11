!================================================!
!   Initialize structural stage classification   !
!================================================!
! METRIC-BASE $Id$
subroutine ISSTAG
  implicit none

  !   INITIALIZE THE STAND STRUCTURAL STAGE CLASSES CALCULATIONS.
  !   SEE SUBROUTINE SSTAGE.

  !   N.L.CROOKSTON - INT MOSCOW - JUNE 1996 AND WITH
  !   A.R.STAGE - INT MOSCOW - JUNE 1997

  ! Common blocks
  include 'PRGPRM.f90'
  include 'SSTGMC.f90'
  include 'METRIC.f90'

  ! Variables
  logical :: LON,LPRT
  integer :: I,J

  !     INITIALIZE THE STAND STRUCTURAL STAGE ROUTINE.

  ISTRCL = 0
  LCALC = .false.
  LPRNT = .true.
  SSDBH = 5.    * INtoCM
  SAWDBH = 25.   * INtoCM
  GAPPCT = 30.
  PCTSMX = 30.
  CCMIN = 5.
  TPAMIN = 200. / ACRtoHA
  IRREF = -1
  do I = 1,33
     do J = 1,2
        OSTRST(I,J) = 0
     end do
  end do
  return

  entry RQSSTG (LON,LPRT)

  !     ENTRY USED TO REQUEST STRUCTURE CLASSIFICATION WITHOUT KEYWORDS

  LCALC = LON
  LPRNT = LPRT

end
