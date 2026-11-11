!=====================!
!   Reset stand age   !
!=====================!
! BASE $Id$
subroutine RESAGE
  implicit none

  !     RESAGE RESETS THE AGE OF THE STAND TO THE VALUE SPECIFIED
  !     BY KEYWORD. THIS ROUTINE IS CALLED FROM MAIN AFTER THE CALL
  !     TO DISPLY AND THE REGENERATION/ESTABLISHMENT SYSTEM.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'

  ! Variables
  integer :: MYACT2(1)
  real    :: PRMS(1)
  integer :: NTODO,KDT,NP,IACTK,IDT
  data MYACT2/443/

  !     PROCESS RESETAGE KEYWORD

  call OPFIND (1,MYACT2,NTODO)
  if (NTODO.le.0) return
  KDT = IY(ICYC+1)-1
  call OPGET (NTODO,1,IDT,IACTK,NP,PRMS)
  if (IACTK.lt.0) return
  call OPDONE (NTODO,KDT)
  IAGE = IFIX(PRMS(1))-IDT+IY(1)

  return
end subroutine RESAGE
