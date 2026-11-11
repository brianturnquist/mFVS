!==================!
!   estb : esout   !
!==================!
! ESTB $Id$
subroutine ESOUT (LFG)
  implicit none

  !     PART OF THE ESTABLISHMENT MODEL SUBSYSTEM.  COPIES THE
  !     PRINT DATA FROM FILE JOREGT TO JOSTND. CALLED FROM MAIN.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'ESHAP.f90'
  include 'ESHAP2.f90'

  ! Variables
  integer :: ISTLNB,ISTAT,ICOUNT
  character(len=132) :: RECORD
  logical :: LFG,LL

  !     IF NO OUTPUT IS ASKED FOR (IPRINT = 0), DO NOT OPEN THE FILE.

  if (IPRINT.eq.0.or. JOREGT.eq.JOSTND) return

  !     IS FILE OPENED?

  inquire(UNIT = JOREGT,OPENED = LL)
  if (.not.LL) return

  !     REWIND THE ESTAB OUTPUT FILE.

  ENDFILE JOREGT
  rewind JOREGT

  !     COPY THE FILE TO THE PRINTER.

  10 continue
  read (JOREGT,20,end = 40) RECORD
  20 format (A)
  if (.not.LFG) then
     LFG = .true.
     write (JOSTND,30)
     30 format (132('-'))
  end if

  if (RECORD.ne.' ') then
     write (JOSTND,20) RECORD(1:MAX0(1,ISTLNB(RECORD)))
  else
     write (JOSTND,20)
  end if
  goto 10
  40 continue

  !     PREPARE THE FILE FOR THE NEXT STAND.

  !      REWIND JOREGT
  close (JOREGT, STATUS = "DELETE",IOSTAT = ISTAT)
  ICOUNT = 0
  do while (ISTAT /= 0)
     ICOUNT = ICOUNT + 1
     call SLEEP(100)
     close (JOREGT, STATUS = "DELETE",IOSTAT = ISTAT)
     if (ICOUNT .ge.5) then
        write(*,*) 'ATTEMPT TO CLOSE FILE AT ESOUT LINE 60 FAILED'
        exit
     end if
  end do
  return
end
