!======================!
!   dbs : dbscknrows   !
!======================!
! DBS $Id$
subroutine DBSCKNROWS(IRCODE,TABLENAME,NRESERV,ISEXCEL)

  !     PURPOSE: SEE IF THE NUMBER OF ROWS ON AN OUTPUT DATABASE
  !              IS TOO LARGE FOR EXCEL...OR EVEN IF THE DATA BASE
  !              TABLE EXISTS.
  !     IRCODE: return code where:
  !            0 = table exists, ok to add rows
  !            1 = table does not exist
  !            2 = ISEXCEL is true and there are too many rows
  !     TABLENAME is the table name, with decorations if needed
  !     NRESERV is an estimate of how many more rows will be inserted,
  !           ignored if ISEXCEL is false
  !     ISEXCEL true if database type is EXCEL

  implicit none

  ! Common blocks
  include 'DBSCOM.f90'

  ! Variables
  integer*4 :: IRCNT
  integer(SQLLEN_KIND)::IRCNT_LI
  logical :: ISEXCEL
  character(len=20) :: TABLENAME
  character(len=100) :: SQLStmtStr
  integer :: IRCODE,NRESERV
  IRCODE = 0

  !     CHECK TO SEE IF THE TREELIST TABLE EXISTS IN DATBASE
  !     IF IT DOESNT THEN WE NEED TO CREATE IT

  if (ISEXCEL) then
     SQLStmtStr = 'SELECT Count(*) FROM '//TABLENAME

  else if (TRIM(DBMSOUT) .eq. 'ACCESS') then
     SQLStmtStr = 'SELECT Count(*) FROM '//&
          &'(SELECT TOP 1 * FROM '//TRIM(TABLENAME)//')'

  else
     SQLStmtStr = 'SELECT Count(*) FROM '//&
          &'(SELECT * FROM '//TRIM(TABLENAME)//' LIMIT 1 )'
  end if

  iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),&
       &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

  !*      PRINT *,SQLStmtStr,"iRet=",iRet

  if (iRet.ne.SQL_SUCCESS.and.&
       &iRet.ne.SQL_SUCCESS_WITH_INFO) then
     IRCODE = 1
  else
     if (ISEXCEL) then
        iRet = fvsSQLBindCol (StmtHndlOut,1,SQL_F_INTEGER,&
             &IRCNT,int(4,SQLLEN_KIND),IRCNT_LI)
        iRet = fvsSQLFetch(StmtHndlOut)
        if (IRCNT .ge.1048576-NRESERV) then
           IRCODE = 2
           iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)
           return
        end if
     end if
  end if
  iRet = fvsSQLCloseCursor(StmtHndlOut)
  return
end
