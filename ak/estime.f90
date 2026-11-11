!=============================!
!   Set regeneration timing   !
!=============================!
! AK $Id$
subroutine ESTIME
  implicit none

  ! ESTB $Id: estime.f 2448 2018-07-10 17:04:02Z gedixon $

  !     CALLED FROM ESTAB.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ESPARM.f90'
  include 'ESCOMN.f90'
  include 'ESCOM2.f90'
  include 'ESWSBW.f90'

  !     DEFINE TIME AND WSBW VARIABLES FOR REGENERATION MODEL.
  !     WSBW NOT INCLUDED IN AK VARIANT. BWB4, BWAF, SQBWAF SET TO 0.
  !     SQREGT AND REGT SET TO TIME.

  BWB4 = 0.0
  BWAF = 0.0
  SQBWAF = 0.0
  SQREGT = SQRT(TIME)
  REGT = TIME
  return
end subroutine ESTIME
