!====================!
!   wsbwe : bweout   !
!====================!
! WSBWE $Id$
subroutine BWEOUT
  implicit none

  !     COPIES PRINT DATA FROM FILE JOWSBW TO JOSTND.

  !     PART OF THE WESTERN SPRUCE BUDWORM MODEL/PROGNOSIS LINKAGE CODE.
  !     N.L. CROOKSTON--FORESTRY SCIENCES LAB, MOSCOW, ID--JUNE 1983

  !     minor changes by K.Sheehan 7/96 to remove LBWDEB

  !     CALLED FROM :

  !       MAIN   - MAIN PROGRAM FOR THE SINGLE STAND PROGNOSIS MODEL.

  !  Revision History:
  !    07-JUN-00 Lance R. David (FHTET)
  !      .Changed final REWIND of JOWSBW to CLOSE so that output from
  !       from a previous stand is not mistaken as output for the current
  !       FVS stand in a multiple stand serial run. This routine interprets
  !       an open JOWSBW as the defoliation model being active.
  !    16-OCT-2006 Lance R. David (FHTET)
  !       Changed local variable name from RECORD to RECRD.
  !    14-JUL-2010 Lance R. David (FMSC)
  !       Added IMPLICIT NONE and declared variables as needed.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'BWESTD.f90'
  include 'BWECOM.f90'
  include 'BWEBOX.f90'

  ! Variables
  logical :: LOPEN
  character(len=133) :: RECRD
  integer :: ISTLNB

  if (JOWSBW.eq.JOSTND) goto 1000

  !     FIND OUT IF THE FILE IS OPENED...IF NOT BYPASS.

  inquire (UNIT = JOWSBW,OPENED = LOPEN)
  if (.not.LOPEN) goto 1000

  !     REWIND THE TEMPORARY OUTPUT FILE.

  !REWIND JOWSBW
  rewind (JOWSBW, ERR = 1000) ! temporary patch to allow stop/start

  !     COPY THE FILE TO THE PRINTER.

  10 continue
  read (JOWSBW,'(A)',end = 40) RECRD
  write (JOSTND,'(A)') RECRD(1:MAX0(1,ISTLNB(RECRD)))
  goto 10
  40 continue

  !     PREPARE THE FILE FOR THE NEXT STAND.

  !     REWIND JOWSBW
  close (JOWSBW)
  1000 continue

  !  PRINT THE SPECIAL EVENTS TABLE

  if (LP4) call BWEP4(2)

  return
end
