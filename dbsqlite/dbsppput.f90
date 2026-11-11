!=========================!
!   dbsqlite : dbsppput   !
!=========================!
! DBSQLITE $Id$
subroutine DBSPPPUT (WK3, IPNT, ILIMIT)
  implicit none

  ! Common blocks
  include 'DBSCOM.f90'

  ! Variables
  integer :: MXI
  parameter (MXI = 57)

  integer :: INTS(MXI),ILIMIT,IPNT
  real    :: WK3(*)

  INTS(  1) = ISUMARY
  INTS(  2) = ICOMPUTE
  INTS(  3) = ITREELIST
  INTS(  4) = IPOTFIRE
  INTS(  5) = IFUELS
  INTS(  6) = ITREEIN
  INTS(  7) = ICUTLIST
  INTS(  8) = IDM1
  INTS(  9) = IDM2
  INTS( 10) = IDM3
  INTS( 11) = IDM5
  INTS( 12) = IDM6
  INTS( 13) = IFUELC
  INTS( 14) = IBURN
  INTS( 15) = IMORTF
  INTS( 16) = ISSUM
  INTS( 17) = ISDET
  INTS( 18) = ISTRCLAS
  INTS( 19) = IBMMAIN
  INTS( 20) = IBMBKP
  INTS( 21) = IBMTREE
  INTS( 22) = IBMVOL
  INTS( 23) = IDBSECON
  INTS( 24) = ISPOUT6
  INTS( 25) = ISPOUT17
  INTS( 26) = ISPOUT21
  INTS( 27) = ISPOUT23
  INTS( 28) = ISPOUT30
  INTS( 29) = ISPOUT31
  INTS( 30) = IATRTLIST
  INTS( 31) = I_CMPU
  INTS( 32) = IADDCMPU
  INTS( 33) = ICMRPT
  INTS( 34) = ICHRPT
  INTS( 35) = ICANPR
  INTS( 36) = IRGIN
  INTS( 37) = IDWDVOL
  INTS( 38) = IDWDCOV
  INTS( 39) = 0
  INTS( 40) = 0
  if (IinDBref .ne.-1) INTS( 39) = 1 ! The connection was openned
  if (IoutDBref.ne.-1) INTS( 40) = 1
  INTS( 41) = ICLIM
  INTS( 42) = IRD1
  INTS( 43) = IRD2
  INTS( 44) = IRD3
  INTS( 45) = ICALIB
  INTS( 46) = ISTATS1
  INTS( 47) = ISTATS2
  INTS( 48) = IREG1
  INTS( 49) = IREG2
  INTS( 50) = IREG3
  INTS( 51) = IREG4
  INTS( 52) = IREG5
  INTS( 53) = IPOTFIREC
  INTS( 54) = IVBCSUM
  INTS( 55) = IVBCTRELST
  INTS( 56) = IVBCCUTLST
  INTS( 57) = IVBCATRLST

  call IFWRIT (WK3, IPNT, ILIMIT, INTS, MXI, 2)
  LENSTRINGS(1) = LEN_TRIM(DSNIN)
  LENSTRINGS(2) = LEN_TRIM(DSNOUT)
  LENSTRINGS(3) = LEN_TRIM(KEYFNAME)
  call IFWRIT (WK3, IPNT, ILIMIT, LENSTRINGS, 3, 2)
  return
end

subroutine DBSCHPUT (CBUFF, IPNT, LNCBUF)
  implicit none

  !  **DBSCHPUT--DBS DATE OF LAST REVISION: 05/14/2015

  ! Common blocks
  include 'DBSCOM.f90'

  ! Variables
  integer :: LNCBUF
  character(len=1) :: CBUFF(LNCBUF)
  integer :: J,IPNT

  if (LENSTRINGS(1).gt.0) then
     do J = 1,LENSTRINGS(1)
        call CHWRIT(CBUFF,IPNT,LNCBUF,DSNIN(J:J),2)
     end do
  end if

  if (LENSTRINGS(2).gt.0) then
     do J = 1,LENSTRINGS(2)
        call CHWRIT(CBUFF,IPNT,LNCBUF,DSNOUT(J:J),2)
     end do
  end if

  if (LENSTRINGS(3).gt.0) then
     do J = 1,LENSTRINGS(3)
        call CHWRIT(CBUFF,IPNT,LNCBUF,KEYFNAME(J:J),2)
     end do
  end if

  do J = 1,36
     call CHWRIT(CBUFF,IPNT,LNCBUF,CASEID(J:J),2)
  end do

  call DBSCLOSE(.true.,.true.)

  return
end
