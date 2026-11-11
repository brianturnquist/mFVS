!================================================!
!   Initialize structural stage classification   !
!================================================!
! BASE $Id$
subroutine ISSTAG
  implicit none

  !   INITIALIZE THE STAND STRUCTURAL STAGE CLASSES CALCULATIONS.
  !   SEE SUBROUTINE SSTAGE.

  !   N.L.CROOKSTON - INT MOSCOW - JUNE 1996 AND WITH
  !   A.R.STAGE - INT MOSCOW - JUNE 1997

  ! Common blocks
  include 'PRGPRM.F77'
  include 'SSTGMC.F77'

  ! Variables
  logical :: LON,LPRT
  integer :: I,J

  !     INITIALIZE THE STAND STRUCTURAL STAGE ROUTINE.

  ISTRCL = 0
  LCALC = .false.
  LPRNT = .true.
  SSDBH = 5.
  SAWDBH = 25.
  GAPPCT = 30.
  PCTSMX = 30.
  CCMIN = 5.
  TPAMIN = 200.
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

end subroutine ISSTAG
