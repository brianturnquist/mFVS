!==================!
!   dfb : dfbout   !
!==================!
! DFB $Id$
subroutine DFBOUT
  implicit none

  !  WRITES DFB OUTBREAK SUMMARY AND COPIES THE DFBB OUTPUT TABLES TO
  !  THE MAIN PROGNOSIS OUTPUT FILE.

  !  CALLED BY :
  !     MAIN   [PROGNOSIS]

  !  CALLS :
  !     DFBHED  (SUBROUTINE)  [DFB]

  !  LOCAL VARIABLES :
  !     DSTRNG - CHARACTER STRING USED TO READ THE DFB OUTPUT FILE AND
  !              WRITE IT TO THE PROGNOSIS OUTPUT FILE.
  !     I      - COUNTER INDEX.
  !     NO     - THE WORD 'NO'.
  !     OUTBRK - SET TO 'YES' OR 'NO' DEPENDING ON IF THERE WAS AN
  !              OUTBREAK OR NOT.
  !     YES    - THE WORD 'YES'.

  !  COMMON BLOCK VARIABLES USED :
  !     IDFBYR - (DFBCOM)  INPUT
  !     IY     - (CONTRL)  INPUT
  !     JODFB  - (DFBCOM)  INPUT
  !     JOSTND - (CONTRL)  INPUT
  !     LDFBON - (DFBCOM)  INPUT
  !     NCYC   - (CONTRL)  INPUT
  !     PRBDFB - (DFBCOM)  INPUT
  !     WORKIN - (DFBCOM)  INPUT

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'DFBCOM.f90'

  ! Variables
  integer :: I

  character(len=133) :: DSTRNG
  character(len=3) :: OUTBRK
  character(len=3) :: YES
  character(len=2) :: NO

  data YES /'YES'/, NO /'NO'/

  if (.not. LDFBON) return

  !     WRITE OUT TABLE HEADER.

  call DFBHED
  write (JODFB,100)
  100 format (22('-'),' DOUGLAS-FIR BEETLE ACTIVITY SUMMARY ',&
       &23('-'),//,'-------- CYCLE --------',8X,'YEAR OF',11X,&
       &'CONDITIONAL',10X,'WAS THERE AN',/,'NUMBER',12X,'YEARS',&
       &5X,'REGIONAL DFB',8X,'PROBABILITY',10X,'OUTBREAK IN',/,&
       &31X,'OUTBREAK',7X,'OF STAND OUTBREAK',7X,'THIS STAND',/,&
       &82('-'),/)

  !     PRINT OUT SUMMARY OUTBREAK TABLE.

  do I = 1,NCYC
     OUTBRK = NO
     if (WORKIN(I)) OUTBRK = YES
     if (IDFBYR(I) .eq.0) write (JODFB,300) I, IY(I), IY(I+1),&
          &PRBDFB(I), OUTBRK
     if (IDFBYR(I) .ne.0) write (JODFB,400) I, IY(I), IY(I+1),&
          &IDFBYR(I), PRBDFB(I), OUTBRK
     200 continue
  end do

  300 format(I3,9X,I4,' - ',I4,T48,F10.3,T75,A3)
  400 format(I3,9X,I4,' - ',I4,10X,I4,T48,F10.3,T75,A3)

  !     THE REST OF THE SUBROUTINE COPIES THE DOUGLAS-FIR BEETLE
  !     OUTPUT TO THE MAIN PROGNOSIS PRINT FILE (JOSTND).

  if (JODFB .eq. JOSTND) return
  rewind JODFB

  500 continue
  read (JODFB,600,end = 1000) DSTRNG
  write (JOSTND,600) DSTRNG
  goto 500

  600 format(A133)

  1000 continue
  rewind JODFB

  return
end
