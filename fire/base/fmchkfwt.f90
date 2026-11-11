!==========================!
!   fire/base : fmchkfwt   !
!==========================!
! FIRE-BASE $Id$
  ! Variables
logical function FMCHKFWT(I2)
  implicit none

  !     CALLED FROM: FMDYN, FMCFMD (WS,NC,IC)

  !  PURPOSE:

  !     PREVENT PLACING MORE THAN MXFMOD ELEMENTS IN THE
  !     FMOD/FWT ARRAY

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'FMPARM.f90'

  integer :: I2

  FMCHKFWT = .false.
  if (I2.le. MXFMOD) then
     FMCHKFWT = .true.
  else
     WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: TOO ',              &
          &    'MANY FIRE MODELS SPECIFIED; MAXIMUM= ',I2,                   &
          &    ' CYCLE= ',I4)") MXFMOD, ICYC
     call RCDSET (2,.true.)
  end if

  return
end
