!================!
!   pg : stash   !
!================!
! PG $Id$
subroutine STASH (BUFFER,ILIMIT)
  implicit none

  !     STASH A BUFFER FULL OF DATA

  !     BUFFER= BUFFER FULL OF DATA TO BE STASHED.
  !     ILIMIT= LENGTH OF THE BUFFER.

  ! Common blocks
  include 'GLBLCNTL.f90'

  ! Variables
  integer :: ILIMIT
  real    :: BUFFER (ILIMIT)
  write (JSTASH) BUFFER
  return
end

subroutine CHSTSH (CBUFF,LNCBUF)
  implicit none

  !     STASH A BUFFER FULL OF DATA

  !     CBUFF= A CHAR BUFFER FULL OF DATA TO BE STASHED
  !     ILIMIT= LENGTH OF THE BUFFER.

  ! Common blocks
  include 'GLBLCNTL.f90'

  ! Variables
  integer :: LNCBUF
  character(len=1) :: CBUFF(LNCBUF)
  write (JSTASH) CBUFF
  return
end

subroutine DSTASH (BUFFER,IPNT)
  implicit none

  !     GET DATA STASHED WITH STASH.

  ! Common blocks
  include 'GLBLCNTL.f90'

  ! Variables
  integer :: IPNT,ISIZE
  real    :: BUFFER (IPNT)
  if (seekReadPos.gt.0) then
     inquire (UNIT = JDSTASH,SIZE = ISIZE)
     if (seekReadPos .gt. isize) goto 10
     read (JDSTASH,ERR = 10,POS = seekReadPos) BUFFER
     seekReadPos = -1
  else
     read (JDSTASH,end = 10,ERR = 10) BUFFER
  end if
  return
  10 continue
  call fvsSetRtnCode(2) ! signal end of file
  return
end

subroutine CHDSTH (CBUFF,IPNT)
  implicit none

  !     GET A BUFFER FULL OF DATA

  !     CBUFF= A CHAR BUFFER FULL OF DATA FETCHED
  !     IPNT= LENGTH OF THE BUFFER.

  ! Common blocks
  include 'GLBLCNTL.f90'

  ! Variables
  integer :: IPNT
  character(len=1) :: CBUFF(IPNT)
  read (JDSTASH,end = 10,ERR = 10) CBUFF
  return
  10 continue
  call fvsSetRtnCode(2) ! signal end of file
  return
end
