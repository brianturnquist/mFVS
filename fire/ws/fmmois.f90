!======================!
!   fire/ws : fmmois   !
!======================!
! FIRE-WS $Id$
subroutine FMMOIS (FMOIS, MOIS)
  implicit none

  !     CALLED FROM: FMBURN

  !  PURPOSE:
  !     THIS SUBROUTINE RETURNS THE PRESET MOISTURE LEVELS

  !  CALL LIST DEFINITIONS:
  !     FMOIS:   MOISTURE MODEL NUMBER
  !     MOIS:    MOISTURE VALUES

  !  LOCAL VARIABLE DEFINITIONS:
  !     MOIS: (1,*):DEAD STUFF: ,1=0-.25;,2=.25-1;,3=1-3;,4=3+;,5=DUFF
  !           (2,*):LIVE STUFF

  !  COMMON BLOCK VARIABLES AND PARAMETERS:

  !.... PARAMETER STATEMENTS.

  !.... PARAMETER INCLUDE FILES.

  !.... COMMON INCLUDE FILES.
  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'

  !     LOCAL VARIABLE DECLARATIONS

  ! Variables
  integer :: FMOIS
  real    :: MOIS(2,5)
  logical :: DEBUG

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'FMMOIS',6,ICYC)
  if (DEBUG) write(JOSTND,7) ICYC
  7 format(' ENTERING ROUTINE FMMOIS CYCLE = ',I2)

  !     BEGIN ROUTINE

  if (FMOIS .eq.0) then
     return

  else if (FMOIS .eq.1) then

     !         "very low moisture" / wildfire

     MOIS(1,1) = 0.03
     MOIS(1,2) = 0.04
     MOIS(1,3) = 0.05
     MOIS(1,4) = 0.10
     MOIS(1,5) = 0.15
     MOIS(2,1) = 0.70 ! live woody
     MOIS(2,2) = 0.70 ! live herb

  else if (FMOIS .eq.2) then

     !         "low moisture"

     MOIS(1,1) = 0.08
     MOIS(1,2) = 0.08
     MOIS(1,3) = 0.10
     MOIS(1,4) = 0.15
     MOIS(1,5) = 0.50
     MOIS(2,1) = 1.10
     MOIS(2,2) = 1.10

  else if (FMOIS .eq.3) then

     !         "moderate moisture"

     MOIS(1,1) = 0.12
     MOIS(1,2) = 0.12
     MOIS(1,3) = 0.14
     MOIS(1,4) = 0.25
     MOIS(1,5) = 1.25
     MOIS(2,1) = 1.50
     MOIS(2,2) = 1.50

  else if (FMOIS .eq.4) then

     !         "high moisture"

     MOIS(1,1) = 0.12
     MOIS(1,2) = 0.12
     MOIS(1,3) = 0.14
     MOIS(1,4) = 0.25
     MOIS(1,5) = 1.25
     MOIS(2,1) = 1.50
     MOIS(2,2) = 1.50

  end if

  return
end
