!=================!
!   wpbr : brpr   !
!=================!
! WPBR $Id$
subroutine BRPR
  implicit none

  !  Purpose:
  !  BRPR performs the following functions:
  !     1) calls BRCSTA to assign canker status when a compression
  !        has been performed
  !     2) calls BRTSTA to calculate the status of a tree
  !     3) calls BRSTAT to calculate Blister Rust statistics
  !     4) calls BRSUM to load the summary information
  !     5) calls BRCOUT to write detailed canker output
  !     6) calls BRTOUT to write detailed tree output

  !  Revision History:

  !  dd-MMM-YYYY programmer_name
  !     description of change or update.

  !.... Common include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'
  include 'BRCOM.f90'

  !.... Local variable declarations.

  ! Variables
  logical :: BRGO,DEBUG

  !.... See if we need to do some debug.

  call DBCHK(DEBUG,'BRPR',4,ICYC)
  if (DEBUG) write(JOSTND,10) ICYC
  10 format('Entering subroutine BRPR: cycle = ',I2)

  !.... See if Blister Rust model is being used in this simulation.
  !.... If not, then return.

  call BRATV(BRGO)
  if (.not.BRGO) go to 90

  !.... When a compression has been performed, canker statuses must be
  !.... assigned.

  if (BRCMP) then
     call BRCSTA
     BRCMP = .false.
  end if

  !.... Call BRTSTA to calcualte status for tree based on the tree's
  !.... worst canker.

  call BRTSTA

  !.... Call BRSTAT to calculate statistics. BRSTAT is called from BRSETP
  !.... during the initialization phase.

  if (.not.LSTART) call BRSTAT

  !.... Call BRSUM to load summary statistics arrays

  call BRSUM

  !.... Call BRCOUT to print list of cankers of requested.

  if (BRCL) call BRCOUT

  !.... Call BRTOUT to print list of trees if requested.

  if (BRTL) call BRTOUT

  !.... Common return.

  90 continue
  if (DEBUG) write(JOSTND,100) ICYC
  100 format('Leaving subroutine BRPR: cycle = ',I2)
  return
end
