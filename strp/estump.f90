!===================!
!   strp : estump   !
!===================!
! STRP $Id$
subroutine ESTUMP (JSSP,DBH,PREM,JPLOT,ISHAG)
  implicit none

  !  STORE INFORMATION FOR STUMP SPROUT SUBROUTINE.

  !  STRP AND ESTB VERSIONS ARE IDENTICAL; IF YOU UPDATE ONE,
  !  UPDATE BOTH.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'ESPARM.f90'
  include 'ESHOOT.f90'
  include 'ESHAP.f90'
  include 'ESCOMN.f90'

  ! Variables
  integer :: JPLOT,JSSP,ISSP,IDBH,ISHAG,INDXAS
  integer*4 :: MDBH,IPLT,MSP,ISHI,I,MNAGDF,IBEST
  real    :: PREM,DBH,DBHEND

  !  DATA STATEMENTS

  data MDBH/10000000/,MSP/10000/

  !  DETERMINE IF THIS IS A SPROUTING SPECIES

  do I = 1,NSPSPE
     if (JSSP.eq.ISPSPE(I)) go to 11
     10 continue
  end do
  go to 900
  11 continue
  ISSP = I

  !  DETERMINE DIAMETER CLASS OF THE TREE BEING CUT.

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
  ISHI = IDBH*MDBH + ISSP*MSP + IPLT

  !  IF THERE IS ROOM IN THE LIST, THEN CREATE A NEW STUMP RECORD.

  ITRNRM = ITRNRM+1

  !  ISHOOT() IS A NUMBER COMPOSED OF DDDSSSPPPP, WHERE DDD = DBH CLASS,
  !  SSS = SPECIES, AND PPPP = PLOT.  DECODED IN **ESUCKR**.
  !  PRBREM() CONTAINS AMOUNT OF PROB REMOVED IN CUTTING TREE RECORD.

  if (ITRNRM.le.MAXTRE) then
     DSTUMP(ITRNRM) = DBH
     ISHOOT(ITRNRM) = ISHI
     PRBREM(ITRNRM) = PREM
     JSHAGE(ITRNRM) = ISHAG
  else
     ITRNRM = MAXTRE

     !  THE LIST IS FULL. FIND AN EXISTING RECORD THAT MATCHES AS BEST
     !  IT CAN AND ADD THE PROB TO THAT RECORD. IF NO MATCH IS FOUND,
     !  THEN IGNORE THE STUMP.

     MNAGDF = 9999
     IBEST = 0

     do I = 1,ITRNRM
        if (ISHI.eq.ISHOOT(I)) then
           if (ABS(ISHAG-JSHAGE(I)).lt.MNAGDF) then
              IBEST = I
              MNAGDF = ABS(ISHAG-JSHAGE(I))
           end if
        end if
        if (MNAGDF.eq.0) exit
     end do

     if (IBEST.gt.0) PRBREM(IBEST) = PRBREM(IBEST)+PREM
  end if

  900 continue

  !  DETERMINE IF QUAKING ASPEN WAS CUT; IF SO, ACCUMULATE BA AND TPA
  !  OF ASPEN THAT WAS REMOVED FOR ESTIMATION OF NUMBER OF ASPEN SPROUTS
  !  IN SUBROUTINE **ESUCKR**.

  call ESASID(VARACD,INDXAS)
  if (JSSP .eq. INDXAS) then
     ASTPAR = ASTPAR + PREM
     ASBAR = ASBAR + 0.0054542*PREM*DBH**2.
  end if

  return
end
