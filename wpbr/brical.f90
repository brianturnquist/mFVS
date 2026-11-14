!===================!
!   wpbr : brical   !
!===================!
! WPBR $Id$
subroutine BRICAL
  implicit none

  !  Purpose:
  !  BRIBA calculates the Rust Index based on exposure time.

  !  The functions used to determine the rust index from exposure time were
  !  provided by Geral McDonald, Intermountain Research Station, Moscow,
  !  ID.

  !  Rust Index (RI) is calculated once per cycle. The function has four
  !  parameters and utilizes stand age as the exposure time variable.
  !  The parameters are:
  !     MINRI - minumum Rust Index
  !     MAXRI - maximum Rust Index
  !     PKAGE - stand age (exposure time) when Rust Index peaks
  !     RISHP - defines shape of curve about the peak

  !  Called from:
  !     BRTREG depending on RI assignment method.

  !  Revision History:

  !  dd-MMM-YYYY programmer_name
  !     description of change or update.

  !.... Common include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'BRCOM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'

  !.... Local variable declarations.

  ! Variables
  real    :: BRI
  integer :: NYEAR,NAGE
  logical :: DEBUG

  !.... See if we need to do some debug.

  call DBCHK(DEBUG,'BRICAL',6,ICYC)
  if (DEBUG) write(JOSTND,10) ICYC
  10 format('Entering subroutine BRICAL: cycle = ',I2)

  !.... Get current year and age.

  NYEAR = IY(ICYC+1)
  NAGE = IAGE + NYEAR - IY(1)

  if (RIMETH.eq.3) then
     !....    Calculate Rust Index with Gaussian equation.
     !....    This equation provides a normal curve around the peak age.
     BRI = MINRI + MAXRI * EXP(-0.5 * ((NAGE - PKAGE)/PKSHP)**2)

  else if (RIMETH.eq.4) then
     !....    Calculate Rust Index with log function.
     !....    This equation provides a skewed curve around the peak age
     !....    so that a greater number of cankers can be attained early.
     BRI = MINRI + MAXRI * EXP(-0.5*(LOG(NAGE - PKAGE)/PKSHP)**2)

  end if

  RIDEF = BRI

  if (DEBUG) write(JOSTND,60) RIDEF,RIMETH,NAGE,ICYC
  60 format(27X,'RIDEF = ',F10.8,' RIMETH=',I1,' NAGE=',I4,/,&
       &' Leaving subroutine BRICAL: cycle = ',I2)
  return
end
