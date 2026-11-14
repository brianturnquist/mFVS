!======================!
!   dbs : dbsfmdwcov   !
!======================!
! DBS $Id$
subroutine DBSFMDWCOV(IYEAR,NPLT,VAR,VARDIM,KODE)
  implicit none

  !     PURPOSE: TO POPULATE A DATABASE WITH THE DOWN WOOD COVER REPORT
  !              INFORMATION
  !     AUTH: S. REBAIN
  !     INPUT:
  !       IYEAR  - CALENDAR YEAR
  !       NPLT   - CASE NUMBER
  !       VAR    - ARRAY WITH VARIABLES TO REPORT
  !       VARDIM - LENGTH OF VAR ARRAY
  !       UNITS ARE % COVER
  !         1 = DOWN WOOD 3 - 6" HARD
  !         2 = DOWN WOOD 6 - 12" HARD
  !         3 = DOWN WOOD 12 - 20" HARD
  !         4 = DOWN WOOD 20 - 35" HARD
  !         5 = DOWN WOOD 35 - 50" HARD
  !         6 = DOWN WOOD 50"+ HARD
  !         7 = DOWN WOOD TOTAL HARD
  !         8 = DOWN WOOD 3 - 6" SOFT
  !         9 = DOWN WOOD 6 - 12" SOFT
  !         10 = DOWN WOOD 12 - 20" SOFT
  !         11 = DOWN WOOD 20 - 35" SOFT
  !         12 = DOWN WOOD 35 - 50" SOFT
  !         13 = DOWN WOOD 50"+ SOFT
  !         14 = DOWN WOOD TOTAL SOFT
  !       KODE   - RETURN CODE

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

  if (IDWDCOV.eq.0) return
  if (IDWDCOV.eq.2) KODE = 0

  !     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

  call DBSCASE(1)

  !     ALLOCATE A STATEMENT HANDLE

  iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
  if (iRet.ne.SQL_SUCCESS .and. iRet.ne. SQL_SUCCESS_WITH_INFO) then
     IDWDCOV = 0
     print *,'Error connecting to data source'
     call  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,&
          &'DBSFMDWCOV:DSN Connection')
     goto 200
  end if

  !     CHECK TO SEE IF THE DOWN WOOD COVER TABLE EXISTS IN DATBASE

  if (TRIM(DBMSOUT).eq."EXCEL") then
     TABLENAME = '[FVS_Down_Wood_Cov$]'
  else
     TABLENAME = 'FVS_Down_Wood_Cov'
  end if
  call DBSCKNROWS(IRCODE,TABLENAME,1,TRIM(DBMSOUT).eq.'EXCEL')
  if (IRCODE.eq.2) then
     IDWDCOV = 0
     return
  end if
  if (IRCODE.eq.1) then
     if (TRIM(DBMSOUT).eq."ACCESS") then
        SQLStmtStr = 'CREATE TABLE FVS_Down_Wood_Cov('//&
             &'CaseID Text not null,'//&
             &'StandID Text null,'//&
             &'Year Int null,'//&
             &'DWD_Cover_3to6_Hard double null,'//&
             &'DWD_Cover_6to12_Hard double null,'//&
             &'DWD_Cover_12to20_Hard double null,'//&
             &'DWD_Cover_20to35_Hard double null,'//&
             &'DWD_Cover_35to50_Hard double null,'//&
             &'DWD_Cover_ge_50_Hard double null,'//&
             &'DWD_Cover_Total_Hard double null,'//&
             &'DWD_Cover_3to6_Soft double null,'//&
             &'DWD_Cover_6to12_Soft double null,'//&
             &'DWD_Cover_12to20_Soft double null,'//&
             &'DWD_Cover_20to35_Soft double null,'//&
             &'DWD_Cover_35to50_Soft double null,'//&
             &'DWD_Cover_ge_50_Soft double null,'//&
             &'DWD_Cover_Total_Soft double null)'

     else if (TRIM(DBMSOUT).eq."EXCEL") then
        SQLStmtStr = 'CREATE TABLE FVS_Down_Wood_Cov('//&
             &'CaseID Text,'//&
             &'StandID Text,'//&
             &'Year Int ,'//&
             &'DWD_Cover_3to6_Hard Number,'//&
             &'DWD_Cover_6to12_Hard Number,'//&
             &'DWD_Cover_12to20_Hard Number,'//&
             &'DWD_Cover_20to35_Hard Number,'//&
             &'DWD_Cover_35to50_Hard Number,'//&
             &'DWD_Cover_ge_50_Hard Number,'//&
             &'DWD_Cover_Total_Hard Number,'//&
             &'DWD_Cover_3to6_Soft Number,'//&
             &'DWD_Cover_6to12_Soft Number,'//&
             &'DWD_Cover_12to20_Soft Number,'//&
             &'DWD_Cover_20to35_Soft Number,'//&
             &'DWD_Cover_35to50_Soft Number,'//&
             &'DWD_Cover_ge_50_Soft Number,'//&
             &'DWD_Cover_Total_Soft Number)'
     else
        SQLStmtStr = 'CREATE TABLE FVS_Down_Wood_Cov('//&
             &'CaseID char(36) not null,'//&
             &'StandID char(26) not null,'//&
             &'Year Int null,'//&
             &'DWD_Cover_3to6_Hard real null,'//&
             &'DWD_Cover_6to12_Hard real null,'//&
             &'DWD_Cover_12to20_Hard real null,'//&
             &'DWD_Cover_20to35_Hard real null,'//&
             &'DWD_Cover_35to50_Hard real null,'//&
             &'DWD_Cover_ge_50_Hard real null,'//&
             &'DWD_Cover_Total_Hard real null,'//&
             &'DWD_Cover_3to6_Soft real null,'//&
             &'DWD_Cover_6to12_Soft real null,'//&
             &'DWD_Cover_12to20_Soft real null,'//&
             &'DWD_Cover_20to35_Soft real null,'//&
             &'DWD_Cover_35to50_Soft real null,'//&
             &'DWD_Cover_ge_50_Soft real null,'//&
             &'DWD_Cover_Total_Soft real null)'
     end if

     iRet = fvsSQLCloseCursor(StmtHndlOut)

     iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),&
          &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
     call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
          &'DBSFMDWCOV:Creating Table: '//trim(SQLStmtStr))
  end if

  !     COPY INPUT VECTOR TO DOUBLE-PRECISION

  do I = 1,VARDIM
     VARD(I) = VAR(I)
  end do

  write(SQLStmtStr,*)'INSERT INTO ',TRIM(TABLENAME),&
       &' (CaseID,StandID,Year,DWD_Cover_3to6_Hard,',&
       &'DWD_Cover_6to12_Hard,DWD_Cover_12to20_Hard,',&
       &'DWD_Cover_20to35_Hard,DWD_Cover_35to50_Hard,',&
       &'DWD_Cover_ge_50_Hard,DWD_Cover_Total_Hard,',&
       &'DWD_Cover_3to6_Soft,',&
       &'DWD_Cover_6to12_Soft,DWD_Cover_12to20_Soft,',&
       &'DWD_Cover_20to35_Soft,DWD_Cover_35to50_Soft,',&
       &'DWD_Cover_ge_50_Soft,DWD_Cover_Total_Soft) VALUES (''',&
       &CASEID,''',''',TRIM(NPLT),''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'

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
       &'DBSFMDWCOV:Inserting Row')

  200 continue
  !Release statement handle
  iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

end
