!===================================!
!   Coordinate tree growth cycles   !
!===================================!
! BASE $Id$
subroutine TREGRO
  implicit none

  !    CALLED FROM **MAIN** EACH CYCLE.

  ! Common blocks
  include 'PRGPRM.F77'
  include 'CONTRL.F77'

  !  DECLARATIONS:

  ! Variables
  logical :: LTMGO,LMPBGO,LDFBGO,LBWEGO,LCVATV,DEBUG
  integer :: ISTOPRES,IRTNCD,ISTOPDONE
  LTMGO = .false.
  LMPBGO = .false.
  LDFBGO = .false.
  LBWEGO = .false.
  LCVATV = .false.

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'TREGRO',6,ICYC)

  !  GET THE RESTART CODE AND BRANCH ACCORDINGLY.

  call fvsGetRestartCode (ISTOPRES)
  if (DEBUG) write(JOSTND,2) ICYC,ISTOPRES
  2 format (' IN TREGRO, ICYC=',I3,' ISTOPRES=',I3)
  if (ISTOPRES.ge.5 .and. .not. ISTOPRES.eq.7) goto 10

  !  CALL GRINCR TO COMPUTE INCREMENTS AND SEE IF BUG MODELS ARE ACTIVE.

  call GRINCR (DEBUG,1,LTMGO,LMPBGO,LDFBGO,LBWEGO,LCVATV)
  call getAmStopping (ISTOPDONE)
  if (ISTOPDONE.ne.0) return
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return
  10 continue

  !  CALL GRADD TO COMPUTE BUGS AND ADD THE INCREMENTS.

  call GRADD (DEBUG,1,LTMGO,LMPBGO,LDFBGO,LBWEGO,LCVATV)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  !  END OF CYCLE.

  if (DEBUG) write(JOSTND,9022) ICYC
  9022 format(' END OF TREGRO, CYCLE=',I2)
  return
end subroutine TREGRO
