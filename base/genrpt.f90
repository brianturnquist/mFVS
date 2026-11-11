!===================================!
!   Handle multiple print reports   !
!===================================!
! BASE $Id$
subroutine GENRPT
  implicit none

  !     THIS ROUTINE HANDLES MULTIPLE PRINT REPORTS.  THAT IS
  !     YOU CAN WRITE SEVERAL REPORTS TO THE FILE OWNED BY THIS
  !     ROUTINE AND ALL THE REPORTS WILL BE WRITTEN TO THE FVS OUTPUT
  !     IN SEQUENCE.

  ! Common blocks
  include "PRGPRM.f90"
  include "CONTRL.f90"

  common /GENRCM/ JOSCRT,NRPTS,IFOPN

!DEC$ ATTRIBUTES DLLEXPORT,C,DECORATE,ALIAS:'GENRPT'::GENRPT
!DEC$ ATTRIBUTES REFERENCE :: JROUT,IFID

  ! Variables
  integer :: IFOPN,NRPTS,JOSCRT,IFID,JROUT,ID,IR
  logical :: LOPEN
  character(len=140) :: RECORD

  !     INITIALIZE THIS ROUTINE.

  JOSCRT = 93
  inquire(unit = JOSCRT,opened = LOPEN)
  if (LOPEN) close(unit = JOSCRT)
  NRPTS = 0
  IFOPN = 0
  return

  entry GETID (IFID)

  !     RETURN A REPORT ID.  IF THE SCRATCH FILE IS NOT OPENED, IT
  !     OPENS IT.

  if (IFOPN.eq.0) then
     open (unit = JOSCRT,file = TRIM(KWDFIL)//'_genrpt.txt', &
          & position = "append",err = 1)
     IFOPN = 1
  end if
  NRPTS = NRPTS+1
  IFID = NRPTS
  return

  1 continue
  call ERRGRO (.true.,26)
  IFID = -1
  return

  entry GETLUN (JROUT)

  if (IFOPN.eq.0) then
     open (unit = JOSCRT,file = TRIM(KWDFIL)//'_genrpt.txt', &
          & position = "append",err = 2)
     IFOPN = 1
  end if
  JROUT = JOSCRT
  return

  2 continue
  call ERRGRO (.true.,26)
  JROUT = -1
  return

  entry GENPRT

  if (IFOPN.eq.0 .or. NRPTS.eq.0) return

  ENDFILE JOSCRT

  !     USE ALTERNATIVE PROCESSING IF THERE ARE MANY REPORTS

  do ID = 1,NRPTS

     rewind JOSCRT

     10 continue

     read (JOSCRT,'(1X,I5,1X,A)',end = 100) IR,RECORD

     if (RECORD(1:4).eq.'$#*%' .and. RECORD(5:).eq.' ') then
        20 continue
        read (JOSCRT,'(A)',end = 100) RECORD
        if (RECORD(1:4).eq.'$#*%'.and.RECORD(5:).eq.' ')goto 10
        if (IR.eq.ID) write (JOSTND,25) TRIM(RECORD)
        25 format (A)
        goto 20
     else
        if (IR.eq.ID) write (JOSTND,25) TRIM(RECORD)
     end if
     goto 10
     100 continue
  end do

  close (unit = JOSCRT,status = 'delete')
  IFOPN = 0
  NRPTS = 0

  return

  entry GETNRPTS(IFID)
  IFID = NRPTS
  return

  entry SETNRPTS(IFID)
  NRPTS = IFID
  return

end subroutine GENRPT
