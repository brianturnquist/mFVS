!=======================!
!   Update event ages   !
!=======================!
! BASE $Id$
subroutine EVAGE (IDTE)
  implicit none

  !     CALLED FROM EVMON, AGES THE EVENTS WHICH HAVE ALREADY OCCURED
  !     AND RESETS THEM AS VIABLE FOR REOCCURANCE.

  !     EVENT MONITOR ROUTINE - NL CROOKSTON - AUG 1982 - MOSCOW, ID

  !     IDTE  = POSTING DATE

  ! Common blocks
  include 'PRGPRM.f90'
  include 'OPCOM.f90'

  ! Variables
  integer :: IDTE,ISTRT,NEVA,NE,IEN,I1,I

  !     IF THERE ARE NO EVENTS, THEN: RETURN.

  if (IEVT.le.1) return
  ISTRT = 1
  NEVA = IEVA-1

  !     DO FOR ALL EVENTS

  NE = IEVT-1
  do IEN = 1,NE

     !     IF THE EVENT HASN'T HAPPENED (CODE=-1); THEN: SKIP

     if (IEVNTS(IEN,2).eq.-1) goto 20

     !     IF THE EVENT HAS AGED, SET THE CODE TO -1 AND RESET THE
     !     CORRESPONDING ACTIVITIY GROUP SCHEDULING STATUS CODE(S).

     if (IEVNTS(IEN,2)+IEVNTS(IEN,3).gt.IDTE) goto 20
     IEVNTS(IEN,2) = -1
     I1 = ISTRT
     do I = I1,NEVA
        ISTRT = I
        if (IEVACT(I,1).lt.IEN) goto 10
        if (IEVACT(I,1).gt.IEN) goto 20

        !     DO NOT RESET THE SCHEDULING STATUS CODE IF IT IS CURRENTLY SET
        !     AT -1, WHICH INDECATES AN ATTEMPT TO SCHEDULE FAILED.

        if (IEVACT(I,2).gt.-1) IEVACT(I,2) = 0
        10 continue
     end do
     20 continue
  end do
  return
end subroutine EVAGE
