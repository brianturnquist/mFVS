!=====================!
!   dbs : dbsfmhrpt   !
!=====================!
! DBS $Id$
subroutine DBSFMHRPT(IYEAR,NPLT,VAR,VARDIM,KODE)
  implicit none

  !     PURPOSE: TO POPULATE A DATABASE WITH THE CARBON HARVEST
  !              PRODUCTS REPORT INFORMATION
  !     AUTH: D. ROBINSON - ESSA
  !     INPUT:
  !       IYEAR  - CALENDAR YEAR
  !       NPLT   - CASE NUMBER
  !       VAR    - ARRAY WITH VARIABLES TO REPORT
  !       VARDIM - LENGTH OF VAR ARRAY
  !         1 = PRODUCTS
  !         2 = LANDFILL
  !         3 = ENERGY
  !         4 = EMISSIONS
  !         5 = MERCH CARBON STORED
  !         6 = MERCH CARBON REMOVED

  ! Common blocks
  include 'DBSCOM.f90'

  ! Variables
  integer :: IYEAR,KODE,VARDIM,IRCODE
  character(len=26) :: NPLT
  real    :: VAR
  dimension VAR(VARDIM)

  double precision :: VARD(VARDIM)
  integer :: I
  integer(SQLSMALLINT_KIND)::ColNumber
  character(len=2000) :: SQLStmtStr
  character(len=20) :: TABLENAME

  !     Initialize variables

  if (ICHRPT.eq.0) return
  if (ICHRPT.eq.2) KODE = 0

  !     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

  call DBSCASE(1)

  !     ALLOCATE A STATEMENT HANDLE

  iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
  if (iRet.ne.SQL_SUCCESS .and. iRet.ne. SQL_SUCCESS_WITH_INFO) then
     ICMRPT = 0
     print *,'Error connecting to data source'
     call  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,&
          &'DBSFMHRPT:DSN Connection')
     goto 200
  end if

  !     CHECK TO SEE IF THE CARBON HARVEST TABLE EXISTS IN DATBASE

  if (TRIM(DBMSOUT).eq."EXCEL") then
     TABLENAME = '[FVS_Hrv_Carbon$]'
  else
     TABLENAME = 'FVS_Hrv_Carbon'
  end if
  call DBSCKNROWS(IRCODE,TABLENAME,1,TRIM(DBMSOUT).eq.'EXCEL')
  if (IRCODE.eq.2) then
     ICMRPT = 0
     return
  end if
  if (IRCODE.eq.1) then
     if (TRIM(DBMSOUT).eq."ACCESS") then
        SQLStmtStr = 'CREATE TABLE FVS_Hrv_Carbon('//&
             &'CaseID Text not null,'//&
             &'StandID Text null,'//&
             &'Year Int null,' //&
             &'Products double null,' //&
             &'Landfill double null,' //&
             &'Energy double null,' //&
             &'Emissions double null,' //&
             &'Merch_Carbon_Stored double null,' //&
             &'Merch_Carbon_Removed double null)'

     else if (TRIM(DBMSOUT).eq."EXCEL") then
        SQLStmtStr = 'CREATE TABLE FVS_Hrv_Carbon('//&
             &'CaseID Text,'//&
             &'StandID Text,'//&
             &'Year Int,' //&
             &'Products Number,' //&
             &'Landfill Number,' //&
             &'Energy Number,' //&
             &'Emissions Number,' //&
             &'Merch_Carbon_Stored Number,' //&
             &'Merch_Carbon_Removed Number)'

     else
        SQLStmtStr = 'CREATE TABLE FVS_Hrv_Carbon('//&
             &'CaseID char(36) not null,'//&
             &'StandID char(26) not null,'//&
             &'Year int null,' //&
             &'Products real null,' //&
             &'Landfill real null,' //&
             &'Energy real null,' //&
             &'Emissions real null,' //&
             &'Merch_Carbon_Stored real null,' //&
             &'Merch_Carbon_Removed real null)'

     end if
     !Close Cursor
     iRet = fvsSQLCloseCursor(StmtHndlOut)

     iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),&
          &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
     call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
          &'DBSFMHRPT:Creating Table: '//trim(SQLStmtStr))
  end if

  !     COPY INPUT VECTOR TO DOUBLE-PRECISION

  do I = 1,VARDIM
     VARD(I) = VAR(I)
  end do

  write(SQLStmtStr,*)'INSERT INTO ',TABLENAME,' (CaseID,',&
       &'StandID,Year,Products,Landfill,Energy,Emissions,',&
       &'Merch_Carbon_Stored,Merch_Carbon_Removed) VALUES(''',&
       &CASEID,''',''',TRIM(NPLT),''',?,?,?,?,?,?,?)'

  !     CLOSE CURSOR

  iRet = fvsSQLCloseCursor(StmtHndlOut)

  !     PREPARE THE SQL QUERY

  iRet = fvsSQLPrepare(StmtHndlOut, trim(SQLStmtStr),&
       &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

  !     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

  ColNumber = 1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),IYEAR,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  do I = 1,VARDIM
     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
          &SQL_PARAM_INPUT,&
          &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
          &INT(5,SQLSMALLINT_KIND),VARD(I),int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)
  end do

  !Close Cursor
  iRet = fvsSQLCloseCursor(StmtHndlOut)

  iRet = fvsSQLExecute(StmtHndlOut)
  call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
       &'DBSFMHRPT:Inserting Row')

  200 continue
  !Release statement handle
  iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

end
