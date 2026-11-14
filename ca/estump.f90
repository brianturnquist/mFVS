!=================!
!   ca : estump   !
!=================!
! CA $Id$
subroutine ESTUMP (JSSP,DBH,PREM,JPLOT,ISHAG)
  implicit none

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ESPARM.f90'
  include 'ESHOOT.f90'
  include 'ESHAP.f90'
  include 'ESCOMN.f90'

  !     STORE INFORMATION FOR STUMP SPROUT SUBROUTINE.

  ! Variables
  integer*4 :: MDBH,IPLT,MSP
  integer :: ISHAG,JPLOT,JSSP,I,ISSP,IDBH
  real    :: PREM,DBH,DBHEND
  data MDBH/10000000/,MSP/10000/
  do I = 1,NSPSPE
     if (JSSP.eq.ISPSPE(I)) go to 11
     10 continue
  end do
  go to 900
  11 continue
  ISSP = I

  !     DETERMINE DIAMETER CLASS OF THE TREE BEING CUT.

  do I = 1,NDBHCL-1
     DBHEND = (DBHMID(I)+DBHMID(I+1))/2.0
     if (DBH.gt.DBHEND) go to 100
     IDBH = I
     go to 110
     100 continue
  end do
  IDBH = NDBHCL
  110 continue
  IPLT = JPLOT
  if (IPLT.gt.9999) IPLT = 9999
  ITRNRM = ITRNRM+1

  ! ISHOOT() IS A NUMBER COMPOSED OF DDDSSSPPPP, WHERE DDD = DBH CLASS,
  ! SSS = SPECIES, AND PPPP = PLOT.  DECODED IN **ESUCKR**.
  ! PRBREM() CONTAINS AMOUNT OF PROB REMOVED IN CUTTING TREE RECORD.

  DSTUMP(ITRNRM) = DBH
  ISHOOT(ITRNRM) = IDBH*MDBH + ISSP*MSP + IPLT
  PRBREM(ITRNRM) = PREM
  JSHAGE(ITRNRM) = ISHAG
  if (JSSP.eq.44) then
     ASTPAR = ASTPAR + PREM
     ASBAR = ASBAR + 0.0054542*PREM*DBH**2.
  end if
  900 continue
  return
end
