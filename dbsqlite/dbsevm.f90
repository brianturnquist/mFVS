!=======================!
!   dbsqlite : dbsevm   !
!=======================!
! DBSQLITE $Id$
subroutine DBSEVM (ITODO,IACTK,IDT,JOSTND)
  implicit none

  !     PURPOSE: TO RUN SCHEDULED DBS COMMANDS

  ! Common blocks
  include 'DBSCOM.f90'

  ! Variables
  character(len=2000) :: SQLCMD
  integer :: ITODO,IACTK,IDT,IRC,KODE,JOSTND
  call OPGETC (ITODO,SQLCMD)

  if (SQLCMD.eq.' ') return

  !     PREPROCESS THE COMMAND STRING.

  IRC = 1
  KODE = 0
  if (IACTK.eq.101) then ! 101 IS SQLIN
     !SPECIFY TRUE FOR SCHEDULED
     if (IinDBref.eq.-1) call DBSOPEN(.false.,.true.,KODE)
     !       CHECK TO SEE IF CONNECTION WAS SUCCESSFUL
     if (KODE.eq.1) then
        write (JOSTND,100)TRIM(DSNIN)
        100 format(T12,'DBS ERROR: INPUT CONNECTION FAILED FOR DSN: ',A)
        return
     end if
     call DBSEXECSQL(SQLCMD,IinDBref,.true.,IRC)
  else if (IACTK.eq.102) then ! 102 IS SQLOUT
     !SPECIFY TRUE FOR SCHEDULED
     if (IoutDBref.eq.-1) call DBSOPEN(.false.,.true.,KODE)
     !       CHECK TO SEE IF CONNECTION WAS SUCCESSFUL
     if (KODE.eq.1) then
        write (JOSTND,200)TRIM(DSNOUT)
        200 format(T12,'DBS ERROR: OUTPUT CONNECTION FAILED FOR DSN: ',A)
        return
     end if
     call DBSEXECSQL(SQLCMD,IoutDBref,.true.,IRC)
  end if
  if (IRC.eq.0) call OPDONE (ITODO,IDT)
  return
end
