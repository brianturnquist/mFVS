!====================!
!   lpmpb : mpbhed   !
!====================!
! LPMPB $Id$
subroutine MPBHED
  implicit none

  !     WRITES HEADING FOR MPB MODEL OUTPUT

  !     Writes a warning for the cix, ttx or bmx multi-pest model
  !     (RNH June 98),  if the logical variable LXNOTE is set in
  !     mpbin.f

  ! Revision History
  !   12/23/99 Lance R. David (FHTET-FC)
  !     Updated for expansion of FVS stand id (variable NPLT)
  !     from 8 to 26 characters.
  !   11/1100 Lance R. David (FHTET-FC)
  !     Removed use of Hollerith constants from FORMAT.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'MPBCOM.f90'

  write (JOMPB,10)  NPLT,MGMID

  if (LXNOTE) then
     write(JOMPB, 9991)
     LXNOTE = .false.
  end if

  10 format (/,T34,'MOUNTAIN PINE BEETLE IN LODGE',&
       &'POLE PINE          VERSION 2.0',//,&
       &'STAND ID: ',A26,'; MANAGEMENT ID: ',A4,/)

  9991 format (' *=================================================',&
       &'===========================*',/,&
       &' *---> Note:  The combined insect and pathogen models (in ',&
       &'one executable) <---*',/,&
       &' *---> should NOT be used without close consultation with ',&
       &'the forest''s    <---*',/,&
       &' *---> pathologist and entomologist.  Because of the ',&
       &'potential for more   <---*',/,&
       &' *---> than one insect and/or pathogen acting on the same ',&
       &'tree species,   <---*',/,&
       &' *---> the interpretation of the results of the combined ',&
       &'model can be     <---*',/,&
       &' *---> inaccurate without appropriate model knowledge ',&
       &'and/or experience.  <---*',/,&
       &' *==========================================================',&
       &'==================*',/)

  return
end
