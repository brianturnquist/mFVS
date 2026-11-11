!================!
!   rd : rdatv   !
!================!
! RD $Id$
subroutine RDATV (L,LTREE)
  implicit none

  !  RETURNS L=TRUE, TO INDICATE THAT ROOT DISEASE MODEL IS AVAILABLE
  !  WITH THE PROGRAM BEING RUN AND IS ACTUALLY BEING CALLED.

  !  CALLED BY :
  !     RDCMPR  [ROOT DISEASE]
  !     RDMN1   [ROOT DISEASE]
  !     RDTDEL  [ROOT DISEASE]
  !     RDTRP   [ROOT DISEASE]
  !     INITRE  [PROGNOSIS]
  !     INTREE  [PROGNOSIS]

  !  CALLS     :
  !     NONE

  !  PARAMETERS :
  !     L     - FLAG THAT IS SET TO TRUE WHEN THE ROOT DISEASE MODEL IS
  !             BEING RUN.
  !     LTREE - FLAG IS TRUE IF MODEL IS BEING INITIALIZED BY THE
  !             INPUT TREELIST.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'
  include 'RDADD.f90'

  ! Variables
  logical :: L,LTREE

  L = RRTINV .or. RRMAN
  LTREE = RRTINV

  return
end
