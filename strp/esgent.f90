!============================!
!   Grow regenerated trees   !
!============================!
! STRP $Id$
subroutine ESGENT (ITRNIN)
  implicit none

  !     USES REGENT TO ADD HEIGHT INCREMENT TO REGENERATED TREES

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'ESCOMN.f90'
  include 'ESHAP.f90'

  ! Variables
  integer :: ITRNIN,I,N
  real    :: HTEMP
  logical :: DEBUG

  call DBCHK (DEBUG,'ESGENT',6,ICYC)

  if (DEBUG) write(JOSTND,10) ITRNIN,ITRN
  10 format('IN ESGENT: ITRNIN=',I5,'  ITRN=',I5)

  !     SET VALUE OF IREC1 TO POINT TO THE LAST LOCATION IN THE TREELIST.
  !     THEN CALL SPESRT TO RE-ESTABLISH THE SPECIES ORDER SORT.

  IREC1 = ITRN
  call SPESRT

  !     'GROW' TREES TO THE END OF THE CYCLE.  THEN SUMMARIZE HEIGHTS.

  call REGENT (.true.,ITRNIN)
  do I = ITRNIN,ITRN
     N = ISP(I)
     HTEMP = HT(I) +HTG(I)
     HTG(I) = HTG(I)*WK4(I)
     HT(I) = HT(I) +HTG(I)
     if (WK4(I).lt.1.0) then
        if (HT(I).lt.4.5) then
           DBH(I) = 0.1 +0.001*(HT(I))
           DG(I) = 0.0
        else
           DBH(I) = DBH(I)*(HT(I)/HTEMP)
           DG(I) = DBH(I)*(HT(I)/HTEMP)
        end if
     end if
     if (HT(I).gt.HHTMAX(N)) then
        HT(I) = HHTMAX(N)
        !       DBH(I)=2.95
     end if
     if (DEBUG) write(JOSTND,20) I,N,HT(I),HTG(I),DBH(I),DG(I),WK4(I)
     20 format('IN ESGENT: RECORD#',I5,'  SPECIES=',I3,'  HT=',F5.1,&
          &'  HTG=',F5.2,'  DBH=',F5.2,'  DG=',F5.2,'  WK4=',F5.3)
     if (IMC(I).ne.1) go to 100
     SUMPX(N) = SUMPX(N) +PROB(I)*HT(I)
     SUMPI(N) = SUMPI(N) +PROB(I)
     100 continue
  end do
  return
end
