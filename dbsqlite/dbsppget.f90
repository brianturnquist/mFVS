!=========================!
!   dbsqlite : dbsppget   !
!=========================!
! DBSQLITE $Id$
subroutine DBSPPGET (WK3, IPNT, ILIMIT)
  implicit none

  ! Common blocks
  include 'DBSCOM.f90'

  ! Variables
  integer :: MXI
  parameter (MXI = 57)

  integer :: INTS(MXI),ILIMIT,IPNT
  real    :: WK3(*)

  call IFREAD(WK3, IPNT, ILIMIT, INTS, MXI, 2)
  ISUMARY = INTS(  1)
  ICOMPUTE = INTS(  2)
  ITREELIST = INTS(  3)
  IPOTFIRE = INTS(  4)
  IFUELS = INTS(  5)
  ITREEIN = INTS(  6)
  ICUTLIST = INTS(  7)
  IDM1 = INTS(  8)
  IDM2 = INTS(  9)
  IDM3 = INTS( 10)
  IDM5 = INTS( 11)
  IDM6 = INTS( 12)
  IFUELC = INTS( 13)
  IBURN = INTS( 14)
  IMORTF = INTS( 15)
  ISSUM = INTS( 16)
  ISDET = INTS( 17)
  ISTRCLAS = INTS( 18)
  IBMMAIN = INTS( 19)
  IBMBKP = INTS( 20)
  IBMTREE = INTS( 21)
  IBMVOL = INTS( 22)
  IDBSECON = INTS( 23)
  ISPOUT6 = INTS( 24)
  ISPOUT17 = INTS( 25)
  ISPOUT21 = INTS( 26)
  ISPOUT23 = INTS( 27)
  ISPOUT30 = INTS( 28)
  ISPOUT31 = INTS( 29)
  IATRTLIST = INTS( 30)
  I_CMPU = INTS( 31)
  IADDCMPU = INTS( 32)
  ICMRPT = INTS( 33)
  ICHRPT = INTS( 34)
  ICANPR = INTS( 35)
  IRGIN = INTS( 36)
  IDWDVOL = INTS( 37)
  IDWDCOV = INTS( 38)
  ! Make sure the databases are closed
  call DBSCLOSE(.true.,.true.)
  if (INTS(39).eq.1) IinDBref = -1 ! signal to reopen
  if (INTS(40).eq.1) IoutDBref = -1
  ICLIM = INTS( 41)
  IRD1 = INTS( 42)
  IRD2 = INTS( 43)
  IRD3 = INTS( 44)
  ICALIB = INTS( 45)
  ISTATS1 = INTS( 46)
  ISTATS2 = INTS( 47)
  IREG1 = INTS( 48)
  IREG2 = INTS( 49)
  IREG3 = INTS( 50)
  IREG4 = INTS( 51)
  IREG5 = INTS( 52)
  IPOTFIREC = INTS( 53)
  IVBCSUM = INTS( 54)
  IVBCTRELST = INTS( 55)
  IVBCCUTLST = INTS( 56)
  IVBCATRLST = INTS( 57)

  call IFREAD(WK3, IPNT, ILIMIT, LENSTRINGS, 3, 2)

  return
end

subroutine DBSCHGET (CBUFF, IPNT, LNCBUF)
  implicit none

  !  **DBSCHGET--DBS DATE OF LAST REVISION: 05/14/2015

  ! Common blocks
  include 'DBSCOM.f90'

  ! Variables
  integer :: LNCBUF
  character(len=1) :: CBUFF(LNCBUF)
  integer :: J,IPNT,KODE

  if (LENSTRINGS(1).gt.0) then
     DSNIN = " "
     do J = 1,LENSTRINGS(1)
        call CHREAD(CBUFF,IPNT,LNCBUF,DSNIN(J:J),2)
     end do
  end if

  if (LENSTRINGS(2).gt.0) then
     DSNOUT = " "
     do J = 1,LENSTRINGS(2)
        call CHREAD(CBUFF,IPNT,LNCBUF,DSNOUT(J:J),2)
     end do
  end if

  if (LENSTRINGS(3).gt.0) then
     do J = 1,LENSTRINGS(3)
        call CHREAD(CBUFF,IPNT,LNCBUF,KEYFNAME(J:J),2)
     end do
  end if

  do J = 1,36
     call CHREAD(CBUFF,IPNT,LNCBUF,CASEID(J:J),2)
  end do

  ! reopen connections that were in use.

  if (IinDBref.eq.-1) then
     IinDBref = 0
     call DBSOPEN(.false.,.true.,KODE)

     if (KODE.eq.1) print *,"Reopen DBSIN failed. DSNIN=",&
          &TRIM(DSNIN)
  end if
  if (IoutDBref.eq.-1) then
     IoutDBref = 0
     call DBSOPEN(.true.,.false.,KODE)

     if (KODE.eq.1) print *,"Reopen DSNOUT failed. DSNOUT=",&
          &TRIM(DSNOUT)
  end if

  return
end
