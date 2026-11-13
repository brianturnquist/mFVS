!=============================!
!   canada/newmist : dmnewm   !
!=============================!
! CANADA-NEWMIST $Id$
logical function DMNEWM()
  implicit none

  !  CALLED FROM: MISPRT

  !  Purpose:
  !     Returns NEWMOD, which is true if the BC "new S+I" (NISI) dwarf
  !  mistletoe model is in effect. NEWMOD is in DMCOM, which only BC
  !  compiles, so the shared MISPRT (mistoe/misprt.f90) reads it through
  !  this function. MISPRT calls it only in its metric (LMETRICVAR)
  !  branches, so the other mistletoe variants do not need a stub.

  ! Common block variables and parameters:
  !     NEWMOD: From DMCOM;  true if the NISI model is in effect.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'DMCOM.f90'

  DMNEWM = NEWMOD

  return
end
