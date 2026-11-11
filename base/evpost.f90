!=================================!
!   Post event monitor messages   !
!=================================!
! BASE $Id$
subroutine EVPOST (JOSTND,IEN,IDTE,NGRPS,IALNK)
  implicit none

  !     CALLED FROM EVMON, POSTS THE OCCURANCE OF AN EVENT.

  !     EVENT MONITOR ROUTINE - NL CROOKSTON - JAN 1987 - MOSCOW, ID

  !     JOSTND= PRINT MESSAGE DATA SET REFERENCE NUMBER.
  !     IEN   = THE EVENT NUMBER
  !     IDTE  = THE YEAR THE EVENT OCCURS.
  !     NGRPS = THE NUMBER OF ACTIVITY GROUPS THAT ARE LINKED TO
  !             THE EVENT
  !     IALNK = THE POINTER TO THE FIRST ACTIVITY GROUP (ROW IN IEVACT)
  !             THAT CORRESPONDS TO THE EVENT.

  ! Common blocks
  include 'PRGPRM.F77'
  include 'OPCOM.F77'

  ! Variables
  integer :: IALNK,IDTE,IEN,JOSTND,NEVA,I,NGRPS

  NGRPS = 0

  !     POST THE DATE THE EVENT OCCURRED.

  IEVNTS(IEN,2) = IDTE

  !     LINK THE EVENT TO THE FIRST ACTIVITY GROUP

  call EVALNK (JOSTND,IEN,IALNK)
  if (IALNK.eq.0) goto 20

  !     COUNT UP THE NUMBER OF ACTIVITY GROUPS.

  NEVA = IEVA-1
  do I = IALNK,NEVA

     !     IF THE EVENT NUMBER IN IEVACT IS GREATER THAN THE VALUE OF IEN,
     !     THEN BRANCH TO EXIT

     if (IEVACT(I,1).gt.IEN) goto 20

     !     COUNT THE NUMBER OF ACTIVITY GROUPS AND SET THE SCHEDULE STATUS
     !     CODE TO 1.

     NGRPS = NGRPS+1
     IEVACT(I,2) = 1
     10 continue
  end do
  20 continue
  return
end subroutine EVPOST
