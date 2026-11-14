!=====================!
!   dbs : dbspusget   !
!=====================!
! DBS $Id$
subroutine DBSPUSGET (CBUFF,IPNT,LNCBUF)
  implicit none

  ! Common blocks
  include 'DBSCOM.f90'

  ! Variables
  integer :: I
  integer :: LNCBUF,IPNT
  character(len=1) :: CBUFF(*)
  character(len=1) :: CT

  DSNOUT = ' '
  I = 1
  do
     call CHREAD (CBUFF,IPNT,LNCBUF,CT,2)
     if (CT.eq.CHAR(0)) exit
     DSNOUT(I:I) = CT
     I = I+1
  end do

  DBMSOUT = ' '
  I = 1
  do
     call CHREAD (CBUFF,IPNT,LNCBUF,CT,2)
     if (CT.eq.CHAR(0)) exit
     DBMSOUT(I:I) = CT
     I = I+1
  end do

  DSNIN = ' '
  I = 1
  do
     call CHREAD (CBUFF,IPNT,LNCBUF,CT,2)
     if (CT.eq.CHAR(0)) exit
     DSNIN(I:I) = CT
     I = I+1
  end do

  DBMSIN = ' '
  I = 1
  do
     call CHREAD (CBUFF,IPNT,LNCBUF,CT,2)
     if (CT.eq.CHAR(0)) exit
     DBMSIN(I:I) = CT
     I = I+1
  end do

  return
end
