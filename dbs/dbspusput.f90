!=====================!
!   dbs : dbspusput   !
!=====================!
! DBS $Id$
subroutine DBSPUSPUT (CBUFF,IPNT,LNCBUF)
  implicit none

  ! Common blocks
  include 'DBSCOM.f90'

  ! Variables
  integer :: I
  integer :: LNCBUF,IPNT
  character(len=1) :: CBUFF(*)

  do I = 1,LEN_TRIM(DSNOUT)
     call CHWRIT (CBUFF,IPNT,LNCBUF,DSNOUT(I:I),2)
  end do
  call CHWRIT (CBUFF,IPNT,LNCBUF,CHAR(0),2)
  do I = 1,LEN_TRIM(DBMSOUT)
     call CHWRIT (CBUFF,IPNT,LNCBUF,DBMSOUT(I:I),2)
  end do
  call CHWRIT (CBUFF,IPNT,LNCBUF,CHAR(0),2)
  do I = 1,LEN_TRIM(DSNIN)
     call CHWRIT (CBUFF,IPNT,LNCBUF,DSNIN(I:I),2)
  end do
  call CHWRIT (CBUFF,IPNT,LNCBUF,CHAR(0),2)
  do I = 1,LEN_TRIM(DBMSIN)
     call CHWRIT (CBUFF,IPNT,LNCBUF,DBMSIN(I:I),2)
  end do
  call CHWRIT (CBUFF,IPNT,LNCBUF,CHAR(0),2)
  return
end
