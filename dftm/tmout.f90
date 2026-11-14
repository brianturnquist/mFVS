!==================!
!   dftm : tmout   !
!==================!
! DFTM $Id$
subroutine TMOUT
  implicit none

  !     PRINT FINAL TUSSOCK MOTH ACTIVITY SUMMARY

  !     PART OF THE DFTM EXTENSION OF THE PROGNOSIS SYSTEM.

  ! Revision History:
  !   23-DEC-99; Lance R. David (FHTET-FC)
  !      Updated for expansion of FVS stand id (variable NPLT)
  !      from 8 to 26 characters.
  !   01-APR-2013 Lance R. David (FMSC)
  !      A few variables defined locally were already defined
  !      in a common block. Local declaration removed.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'TMCOM1.f90'

  ! Variables
  character(len=3) :: YES
  character(len=3) :: NO
  character(len=3) :: WASTHR

  character(len=132) :: PRNT

  integer :: I

  data YES /'YES'/,  NO /'NO'/
  logical :: LOPEN

  !     CHECK TO SEE IF OUTPUT FILE IS OPEN.  IF NOT OPEN THEN EXIT.

  inquire (UNIT = JODFTM, OPENED = LOPEN)
  if (.not. LOPEN) goto 200

  !     IF THE DFTM OUTPUT FILE DATA SET REFERENCE NUMBER IS
  !     EQUAL TO THE MAJOR PRINT FILE NUMBER; THEN RETURN.

  if (JODFTM .eq. JOSTND) return

  !     REWIND DFTM OUTPUT FILE.

  rewind JODFTM

  !     IF THE REPORT LEVEL IS ZERO; RETURN

  if (ITMREP .eq.0) return

  call TMHED (JOSTND, NPLT, MGMID)
  if (ITMREP .eq.1) goto 50

  10 continue
  read (JODFTM,20,end = 40) PRNT
  20 format (A132)

  write (JOSTND,20) PRNT
  goto 10

  40 continue

  !     READY THE OUTPUT FILE FOR ANOTHER STAND.

  rewind JODFTM

  50 continue

  write (JOSTND,9016)NPLT
  9016 format (/22('-'),' DFTM OUTBREAK SUMMARY TABLE  ',25('-'),/,&
       &'STAND ID = ',A26,//,&
       &'------ CYCLE ------',8X,'YEAR OF',T46,'CONDITIONAL',T67,&
       &'WAS THERE AN'/'NUMBER',T15,'YEARS',5X,'REGIONAL DFTM',&
       &T46,'PROBABILITY',10X,'OUTBREAK IN'/T28,'OUTBREAK',7X,&
       &'OF STAND OUTBREAK         STAND?'/79('-')/)

  do I = 1,NCYC
     WASTHR = NO
     if (TMWORK(I)) WASTHR = YES
     if (TMYRS(I) .eq.0) write (JOSTND,9017) I, IY(I), IY(I+1),&
          &TMPRB(I), WASTHR
     if (TMYRS(I) .ne.0) write (JOSTND,9018) I, IY(I), IY(I+1),&
          &TMYRS(I), TMPRB(I), WASTHR
     9017 format(I3,I9,' -',I5,T45,F10.3,T71,A3)
     9018 format(I3,I9,' -',I5,I14,T45,F10.3,T71,A3)
     140 continue
  end do

  200 continue

  return
end
