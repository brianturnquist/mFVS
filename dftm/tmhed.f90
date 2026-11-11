!==================!
!   dftm : tmhed   !
!==================!
! DFTM $Id$
subroutine TMHED (IOUT, NPLT, MGMID)
  implicit none

  ! Revision History:
  !     23-DEC-99; Lance R. David (FHTET-FC)
  !        Updated for expansion of FVS stand id (variable NPLT)
  !        from 8 to 26 characters.

  ! Variables
  character(len=26) :: NPLT
  character(len=4) :: MGMID

  integer :: IOUT

  !     WRITES HEADING(S) FOR INSECT MODEL OUTPUT

  write (IOUT,10) NPLT, MGMID
  10 format (' * PRE-RELEASE *  DOUGLAS-FIR TUSSOCK MOTH IN ',&
       &'DOUGLAS-FIR AND GRAND FIR:  DFTM VERSION 3.1;  ',&
       &'PROGNOSIS (INLAND EMPIRE) 4.0'//&
       &T19,' STAND ID= ',A26,'; MANAGEMENT ID= ',A4/)

  return
end
