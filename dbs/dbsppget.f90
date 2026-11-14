!====================!
!   dbs : dbsppget   !
!====================!
! DBS $Id$
subroutine DBSPPGET (WK3, IPNT, ILIMIT)
  implicit none

  ! Common blocks
  include 'DBSCOM.f90'

  ! Variables
  integer :: MXI
  parameter (MXI = 44)

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
  ! note that ConnHndl[In,Out] could be long ints! If
  ! the ConnHndlIn values are positive, then the data
  ! bases are already opened. Make sure they are closed
  call DBSCLOSE(.true.,.true.)
  if (INTS(39).eq.1) ConnHndlIn = 0 ! signal to reopen
  if (INTS(40).eq.1) ConnHndlOut = 0
  ICLIM = INTS( 41)
  IRD1 = INTS( 42)
  IRD2 = INTS( 43)
  IRD3 = INTS( 44)

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
     do J = 1,LENSTRINGS(1)
        call CHREAD(CBUFF,IPNT,LNCBUF,DSNIN(J:J),2)
     end do
  end if

  if (LENSTRINGS(2).gt.0) then
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

  if (ConnHndlIn.eq.0) then
     ConnHndlIn = -1
     call DBSOPEN(DSNIN,EnvHndlIn,ConnHndlIn,DBMSIN,0,&
          &.false.,KODE)

     if (KODE.eq.0) print *,"Reopen DBSIN failed. DSNIN=",&
          &DBMSIN(:LEN_TRIM(DBMSIN))
  end if
  if (ConnHndlOut.eq.0) then
     ConnHndlOut = -1
     call DBSOPEN(DSNOUT,EnvHndlOut,ConnHndlOut,DBMSOUT,0,&
          &.false.,KODE)

     if (KODE.eq.0) print *,"Reopen DBMSOUT failed. DSNOUT=",&
          &DBMSOUT(:LEN_TRIM(DBMSOUT))
  end if

  return
end
