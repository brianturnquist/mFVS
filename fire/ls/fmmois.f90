!======================!
!   fire/ls : fmmois   !
!======================!
! FIRE-LS $Id$
subroutine FMMOIS (FMOIS, MOIS)
  implicit none

  !  CALLED FROM: FMBURN, FMPOFL, FMIN, FMTRET

  !  PURPOSE:
  !     THIS SUBROUTINE RETURNS THE PRESET MOISTURE LEVELS USED WITH
  !     SIMULATED FIRES AND POTENTIAL FIRES.
  !     THE MOISTURE VALUES WERE PROVIDED BY JEREMY BENNETT, MENOMINEE
  !     TRIBE, FROM WEATHER STATION DATA FROM WI, MI, AND MN.
  !     DUFF MOISTURE VALUES ARE THE DEFAULTS IN FOFEM.

  !  CALL LIST DEFINITIONS:
  !     FMOIS:   MOISTURE MODEL NUMBER
  !     MOIS:    MOISTURE VALUES

  !  LOCAL VARIABLE DEFINITIONS:
  !     MOIS: (1,*):DEAD STUFF: ,1=0-.25;,2=.25-1;,3=1-3;,4=3+;,5=DUFF
  !           (2,*):LIVE STUFF

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'

  !  LOCAL VARIABLE DECLARATIONS

  ! Variables
  integer :: FMOIS
  real    :: MOIS(2,5)
  logical :: DEBUG

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'FMMOIS',6,ICYC)
  if (DEBUG) write(JOSTND,7) ICYC
  7 format(' ENTERING ROUTINE FMMOIS CYCLE = ',I2)

  !  BEGIN ROUTINE

  if (FMOIS .eq.0) then
     return

  else if (FMOIS .eq.1) then

     !  VERY DRY

     MOIS(1,1) = .05 ! 1hr, 0-.25"
     MOIS(1,2) = .08 ! 10hr, .25-1"
     MOIS(1,3) = .12 ! 100hr, 1-3"
     MOIS(1,4) = .15 ! 3+
     MOIS(1,5) = .40 ! Duff
     MOIS(2,1) = .89 ! Live woody
     MOIS(2,2) = .60 ! Live herb

  else if (FMOIS .eq.2) then

     !  DRY

     MOIS(1,1) = .07
     MOIS(1,2) = .09
     MOIS(1,3) = .14
     MOIS(1,4) = .17
     MOIS(1,5) = .75
     MOIS(2,1) = 1.05
     MOIS(2,2) = .82

  else if (FMOIS .eq.3) then

     !  MOIST

     MOIS(1,1) = .10
     MOIS(1,2) = .13
     MOIS(1,3) = .17
     MOIS(1,4) = .21
     MOIS(1,5) = 1.0
     MOIS(2,1) = 1.35
     MOIS(2,2) = 1.16

  else if (FMOIS .eq.4) then

     !  WET

     MOIS(1,1) = .19
     MOIS(1,2) = .29
     MOIS(1,3) = .22
     MOIS(1,4) = .25
     MOIS(1,5) = 1.75
     MOIS(2,1) = 1.40
     MOIS(2,2) = 1.20

  end if

  return
end
