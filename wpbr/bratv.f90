!==================!
!   wpbr : bratv   !
!==================!
! WPBR $Id$
subroutine BRATV(L)
  implicit none

  !  **BRATV        DATE OF LAST REVISION:  06/05/2014

  !  Purpose:
  !  BRATV returns L=.TRUE. to indicate that the Blister Rust model is
  !  available from the current link and is actually being called.

  !  Revision History:

  !  dd-MMM-YYYY programmer_name
  !     description of change or update.

  !.... Common include files

  ! Common blocks
  include 'PRGPRM.f90'
  include 'BRCOM.f90'

  !.... Local variable declarations

  ! Variables
  logical :: L

  L = BRYES

  !.... Common return

  return
end
