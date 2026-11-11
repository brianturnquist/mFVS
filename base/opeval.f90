!===================================!
!   Evaluate activity expressions   !
!===================================!
! BASE $Id$
subroutine OPEVAL (IREFN,IRC)
  implicit none

  !     OPTION PROCESSING ROUTINE - NL CROOKSTON - JULY 1988 - MOSCOW

  !     OPEVAL IS USED TO EVALUATE AN EXPRESSION ASSOCIATED WITH AN
  !     ACTIVITY AND LOAD THE RESULTS INTO THE PARMS ARRAY.

  !     IREFN = THE POINTER TO THE ACTIVITY.
  !     IRC   = 0   ALL WENT OK
  !             1   ERROR IN COMPUTATION OR PARMS IS FULL.

  ! Common blocks
  include 'PRGPRM.F77'
  include 'OPCOM.F77'
  include 'CONTRL.F77'

  ! Variables
  integer :: IRC,IREFN,J1,NP,I
  logical :: LDEB
  data LDEB/.false./

  !     IS DEBUG WANTED.

  call DBCHK (LDEB,'OPEVAL',6,ICYC)

  !     IF AT CYCLE ZERO, THEN DO NOT EVALUATE THE EXPRESSION.

  if (LDEB) &
       & write (JOSTND,*) 'OPEVAL ICYC, IREFN, IRC=', ICYC, IREFN, IRC

  if (ICYC.eq.0) then
     IRC = -1
     return
  end if

  !     EVALUATE THE EXPRESSION.

  J1 = -IACT(IREFN,2)
  call ALGEVL (LREG,MXLREG,XREG,MXXREG,IEVCOD(J1),MAXCOD-J1+1,IY(1), &
       & IY(ICYC),LDEB,JOSTND,IRC)
  if (LDEB) then
     WRITE (JOSTND,'(/'' IN OPEVAL: IRC,XREG(1),LREG(1)='',I3,F9.2, &
          &          L3)') IRC,XREG(1),LREG(1)
     NP = IFIX(XREG(1))
     IF (NP.GT.0 .AND. NP.LT.50) WRITE (JOSTND,'('' XREG LREG...''/ &
          &      ((1X,10(F9.2,L1))))') (XREG(I),LREG(I),I=1,NP+1)
  end if

  !     IF THE RETURN CODE IS ZERO, THEN: STORE THE PARAMETERS IN PARMS
  !     FOR THE ACTIVITY AND SET THE POINTERS TO THE LOCATION IN PARMS.

  if (IRC.eq.0) then
     NP = IFIX(XREG(1))

     !        IF THERE IS ENOUGH ROOM IN PARMS, STORE THE RESULTS.

     if (IMPL+NP-1.le.ITOPRM) then
        IACT(IREFN,2) = IMPL
        do I = 2,NP+1
           PARMS(IMPL) = XREG(I)
           IMPL = IMPL+1
           20 continue
        end do
        IACT(IREFN,3) = IMPL-1
     else

        !           DELETE THE ACTIVITY AND ISSUE ERROR MSG.

        IRC = 1
        IACT(IREFN,2) = 0
        IACT(IREFN,3) = 0
        IACT(IREFN,4) = -1
        call ERRGRO (.true.,10)
     end if
  else

     !        DELETE THE ACTIVITY AND ISSUE AN ERROR MSG.

     IACT(IREFN,2) = 0
     IACT(IREFN,3) = 0
     IACT(IREFN,4) = -1
     call ERRGRO (.true.,21)
  end if
  return
end subroutine OPEVAL
