!====================!
!   wsbwe : txnote   !
!====================!
! WSBWE $Id$
subroutine TXNOTE (LXNOTE)
  implicit none

  !     This module sets the logical flag LXNOTE to true when the
  !     ??X.exe executables are being linked.  This flag causes,
  !     the following warning to be written in the disease model output
  !     (RNH May98). LXN_ON is set to true after the warning has been
  !     written once during a model execution to prevent multiple messages
  !     in one output file

  !     TXNOTE is called by:
  !     - BWIN  where LXNOTE is passed by BWCNTL.F77
  !     - DFBIN where LXNOTE is passed by DFBCOM.F77
  !     - MPBIN where LXNOTE is passed by MPBCOM.F77
  !     - RRIN  where LXNOTE is passed by RRCOM.F77

  ! Variables
  logical :: LXNOTE,LXN_ON

  if (LXN_ON) then
     LXNOTE = .false.
  else
     LXNOTE = .true.
     LXN_ON = .true.
  end if

  return
end
