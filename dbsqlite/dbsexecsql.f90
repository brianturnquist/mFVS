!===========================!
!   dbsqlite : dbsexecsql   !
!===========================!
! DBSQLITE $Id$
subroutine DBSEXECSQL (SQLCMD,IConn,LSCHED,IRC)
  implicit none

  !     PURPOSE: EXECUTES USER DEFINED QUERIES UPON OPENED
  !              DATABASE CONNECTIONS
  !     INPUT: SQLCMD   - SQL COMMAND QUERY TO BE EXECUTED
  !            IConn - THE CONNECTION TO THE DBMS
  !            LSCHED   - SPECIFIES WHETHER THIS IS COMING FROM THE EVENT
  !                       MONITOR

  ! Common blocks
  include  'PRGPRM.f90'

  include  'CONTRL.f90'

  include  'OPCOM.f90'

  include  'PLOT.f90'

  include  'DBSCOM.f90'

  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_ADDCOLIFABSENT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_DOUBLE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_INT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_TEXT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_CLOSE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLCNT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLDOUBLE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLINT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLISNULL
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLNAME
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLREAL
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLTEXT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLTYPE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_ERRMSG
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_EXEC
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_FINALIZE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_OPEN
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_PREPARE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_RESET
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_STEP
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_TABLEEXISTS
#if !(_WIN64)
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_ADDCOLIFABSENT' :: FSQL3_ADDCOLIFABSENT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_BIND_DOUBLE'    :: FSQL3_BIND_DOUBLE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_BIND_INT'       :: FSQL3_BIND_INT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_BIND_TEXT'      :: FSQL3_BIND_TEXT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_CLOSE'          :: FSQL3_CLOSE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLCNT'         :: FSQL3_COLCNT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLDOUBLE'      :: FSQL3_COLDOUBLE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLINT'         :: FSQL3_COLINT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLISNULL'      :: FSQL3_COLISNULL
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLNAME'        :: FSQL3_COLNAME
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLREAL'        :: FSQL3_COLREAL
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLTEXT'        :: FSQL3_COLTEXT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLTYPE'        :: FSQL3_COLTYPE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_ERRMSG'         :: FSQL3_ERRMSG
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_EXEC'           :: FSQL3_EXEC
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_FINALIZE'       :: FSQL3_FINALIZE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_OPEN'           :: FSQL3_OPEN
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_PREPARE'        :: FSQL3_PREPARE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_RESET'          :: FSQL3_RESET
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_STEP'           :: FSQL3_STEP
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_TABLEEXISTS'    :: FSQL3_TABLEEXISTS
#endif

  ! Variables
  integer :: fsql3_prepare,fsql3_step,fsql3_colcnt,fsql3_colname,fsql3_colisnull,fsql3_finalize
  real    :: fsql3_colreal
  character(len=*) :: SQLCMD
  integer :: IConn,ColumnCount,MxNameLen,ColNumber,NameLen
  parameter (MxNameLen = 50)
  character(LEN = MxNameLen) :: ColName
  integer :: KODE,I,IOPKD
  logical :: LSCHED
  integer :: iRet,IRC

  IRC = 1

  !     MAKE SURE WE HAVE AN OPEN CONNECTION

  if (IConn.eq.-1) return

  !     PARSE OUT AND REPLACE WITH USER DEFINED AND EVENT MONITOR VAR VALS
  call DBSPRSSQL(SQLCMD,LSCHED,KODE)
  if (KODE.eq.0) then
     !       THERE WAS A PROBLEM IN PARSING THE SQL STATEMENT
     write (JOSTND,110) TRIM(SQLCMD)
     110 format (/T12,'SQL STMT: ',A/&
          &'********   ERROR: SQLOUT/SQLIN PARSING FAILED. ')
     call RCDSET(2,.true.)
     return
  end if

  iRet = fsql3_prepare(Iconn,trim(SQLCMD)//CHAR(0))
  if (iRet.ne.0) then
     iRet = fsql3_finalize(Iconn)
     write (JOSTND,120) TRIM(SQLCMD)
     120 format (/T12,'SQL STMT: ',A/&
          &'********   ERROR: SQLOUT/SQLIN PREPARE FAILED. ')
     call RCDSET(2,.true.)
     return
  end if
  do while(fsql3_step(Iconn)==1)
     ColumnCount = fsql3_colcnt(Iconn)
     if (ColumnCount.gt.0) then
        do ColNumber = 0,ColumnCount-1
           NameLen = fsql3_colname(Iconn,ColNumber,ColName,MxNameLen)
           ColName(NameLen+1:) = ""
           IOPKD = 0
           do I = 1,ITST5
              if (ColName(:8).eq.CTSTV5(I)) then
                 IOPKD = I
                 exit
              end if
           end do
           if (IOPKD.eq.0) then
              if (ITST5.lt.MXTST5) then
                 ITST5 = ITST5+1
                 CTSTV5(ITST5) = ColName(:8)
                 LTSTV5(ITST5) = .false.
                 IOPKD = ITST5
              end if
           end if
           if (IOPKD.gt.0) then
              if (fsql3_colisnull(Iconn,ColNumber) .eq.1) then
                 TSTV5(IOPKD) = 0
                 LTSTV5(IOPKD) = .false.
              else
                 TSTV5(IOPKD) = fsql3_colreal(Iconn,ColNumber,0)
                 LTSTV5(IOPKD) = .true.
              end if
           end if
        end do
     end if
  end do
  iRet = fsql3_finalize(Iconn)
  IRC = 0
  return
end
