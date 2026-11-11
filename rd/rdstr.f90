!================!
!   rd : rdstr   !
!================!
! RD $Id$
subroutine RDSTR (IT,PREM,PREPRB)
  implicit none

  !  SUBROUTINE FOR DECREASING INFESTED TREES WHEN CUTTING OR
  !  THINNING

  !  CALLED BY :
  !     CUTS    [PROGNOSIS]

  !  CALLS     :
  !     DBCHK   (SUBROUTINE)   [PROGNOSIS]
  !     RDSADD  (SUBROUTINE)   [ROOT DISEASE]
  !     RDSSIZ  (SUBROUTINE)   [ROOT DISEASE]
  !     RDSUM   (SUBROUTINE)   [ROOT DISEASE]
  !     RDSPUP  (SUBROUTINE)   [ROOT DISEASE]

  !  PARAMETERS :
  !     IT     -
  !     PREM   -
  !     PREPRB -

  !  REVISION HISTORY:
  !    01-JUL-2002 Lance R. David (FHTET)
  !      Previous date might have been June 1998.
  !      Root radius for live trees was only being set when spore infection
  !      was active. This value is needed elsewhere regardless of spore
  !      infection processes.
  !    16-JUL-2002 Lance R. David (FHTET)
  !      Modified comment and debug statements.
  !   09/03/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.
  !   03/01/2016 Lance R. David (FMSC)
  !     Moved check to exit if not active, no trees or no disease area
  !     to top.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'
  include 'RDCOM.f90'
  include 'RDARRY.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'RDADD.f90'

  ! Variables
  logical :: DEBUG
  integer :: I,IDI,IRG,ISL,IT,JJ
  real    :: ANS,PREM,PREPRB,TESTAG,TP,TPAREA,XXX

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'RDSTR',5,ICYC)
  if (DEBUG) write (JOSTND,100) ICYC
  100 format (' ENTER RDSTR: CYCLE=',I5)

  if (IROOT .eq.0.or. PREPRB .eq.0.0) goto 1100

  TPAREA = 0.0
  do IDI = MINRR,MAXRR
     TPAREA = TPAREA + PAREA(IDI)
     101 continue
  end do

  TP = (1.0 - (PREM / PREPRB))

  !     IF THE ROOT ARRAYS HAVE NOT YET BEEN CREATED (BECAUSE THERE IS
  !     NO INFECTION IN THE STAND OR BECAUSE A CUT OCCURS IN THE FIRST
  !     TIMESTEP) THEN DO SO.

  JJ = ISP(IT)
  IDI = MAXRR
  if (MAXRR .lt.3) IDI = IDITYP(IRTSPC(JJ))

  !     This block of code is replaced below. Why array that is suppose
  !     to hold root radius for all each tree records is only calculated
  !     when spore infection is active does not make sense.
  !     lrd 01-jul-02
  !      IF (ROOTL(IT) .GT. 0.0 .OR. SPINF(IDI) .LE. 0.0) GOTO 300

  !          CALL RDROOT(JJ,DBH(IT),ANS,PROOT(IRTSPC(JJ)),
  !     &                RSLOP(IRTSPC(JJ)),HT(IT))
  !          ROOTL(IT) = ANS

  !  300 CONTINUE

  if (ROOTL(IT) .eq.0.0) then
     call RDROOT(JJ,DBH(IT),ANS,PROOT(IRTSPC(JJ)),&
          &RSLOP(IRTSPC(JJ)),HT(IT))
     ROOTL(IT) = ANS
  end if

  !     WHY EQUAL WEIGHTS FOR OUTSIDE AND INSIDE ON A PER ACRE BASIS ?

  XXX = TP * PREPRB
  RROOTT(IT) = (RROOTT(IT) * WK22(IT) +&
       &ROOTL(IT) * XXX) / (XXX + WK22(IT) + 0.0001)

  !     CALL THE SPORE MODEL.

  !      IF (SPINF(IDI) .GT. 0.0) THEN

  !     Only call spore model if both IDI > 0, and SPINF(IDI) > 0
  !     (RNH June98)

  if ((IDI .gt.0) .and. (SPINF(IDI) .gt.0.0)) then

     call RDSSIZ(JJ,DBH(IT),STCUT,ISL,ISPS,IRTSPC)
     call RDSPUP(IT,ISL,JJ,TP,DBH(IT),ROOTL(IT))
  end if

  !     REDUCE TREE DENSITY VARIABLES

  PROBIU(IT) = PROBIU(IT) * TP
  FPROB(IT) = FPROB(IT) * TP
  WK22(IT) = WK22(IT) + XXX

  do I = 1,ISTEP
     PROBI(IT,I,1) = PROBI(IT,I,1) * TP
     PROBI(IT,I,2) = PROBI(IT,I,2) * TP
     500 continue
  end do

  !     ADD THE ALREADY INFECTED STUMPS TO THE STUMP LIST
  call RDSADD(IT,TP)

  call RDSUM (IT,PROBIT,PROBI,ISTEP)
  TESTAG = FLOAT (IY(ICYC))

  if (DEBUG) write (JOSTND,1010) TESTAG, AGECUR, INFLAG
  1010 format(' IN RDSTR: TESTAG AGECUR INFLAG=',2F6.0,I5)

  if ((TESTAG .eq. AGECUR) .and. (INFLAG .ne.0)) INFLAG = 3
  if (INFLAG .ne.0) goto 1100

  !     SET THE CUTTING FLAG ON FOR ROOT DISEASE MODEL

  INFLAG = 1
  if (DEBUG) write (JOSTND,1020)
  1020 format (' IN RDSTR: SET INFLAG=1')

  !     SET UP AGE FOR THE CARRYOVER MODEL CALL TIMING

  IRG = ICYC + IRGEN(1)
  if (IRG .le. NCYC) AGECUR = IY(IRG)
  if (DEBUG) write (JOSTND,1030) AGECUR
  1030 format(' IN RDSTR: AGECUR=',F6.0)

  1100 continue
  if (DEBUG) write (JOSTND,*) 'EXIT RDSTR'
  return
end
