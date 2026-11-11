!=====================!
!   dbs : dbsfmcrpt   !
!=====================!
! DBS $Id$
subroutine DBSFMCRPT(IYEAR,NPLT,VAR,VARDIM,KODE)
  implicit none

  !     PURPOSE: TO POPULATE A DATABASE WITH THE MAIN CARBON REPORT
  !              INFORMATION
  !     AUTH: D. ROBINSON - ESSA
  !     INPUT:
  !       IYEAR  - CALENDAR YEAR
  !       NPLT   - CASE NUMBER
  !       VAR    - ARRAY WITH VARIABLES TO REPORT
  !       VARDIM - LENGTH OF VAR ARRAY
  !       UNITS ARE TONS/ACRE IF USER HAS REQUESTED IMPERIAL UNITS; TONNES/HA IF METRIC
  !         1 = ABOVEGROUND TOTAL LIVE
  !         2 = ABOVEGOURND MERCH LIVE
  !         3 = BELOWGROUND LIVE
  !         4 = BELOWGROUND DEAD (WHICH DECAYS)
  !         5 = STANDING DEAD
  !         6 = FOREST DOWN DEAD WOOD
  !         7 = FOREST FLOOR (SOILS?)
  !         8 = FOREST SHRUB+HERB
  !         9 = TOTAL STAND CARBON
  !        10 = TOTAL CARBON REMOVED THIS REPORTING PERIOD
  !        11 = CARBON RELEASED THRU FIRE
  !       KODE   - RETURN CODE

  ! Common blocks
  include 'DBSCOM.f90'

  ! Variables
  integer :: IYEAR,KODE,VARDIM
  character(len=26) :: NPLT
  real    :: VAR
  dimension VAR(VARDIM)

  double precision :: VARD(VARDIM)
  integer :: IRCODE,I
  integer(SQLSMALLINT_KIND)::ColNumber
  character(len=2000) :: SQLStmtStr
  character(len=20) :: TABLENAME

  !     Initialize variables

  if (ICMRPT.eq.0) return
  if (ICMRPT.eq.2) KODE = 0

  !     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

  call DBSCASE(1)

  !     ALLOCATE A STATEMENT HANDLE

  iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
  if (iRet.ne.SQL_SUCCESS .and. iRet.ne. SQL_SUCCESS_WITH_INFO) then
     ICMRPT = 0
     print *,'Error connecting to data source'
     call  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,&
          &'DBSFMCRPT:DSN Connection')
     goto 200
  end if

  !     CHECK TO SEE IF THE MAIN CARBON TABLE EXISTS IN DATBASE

  if (TRIM(DBMSOUT).eq."EXCEL") then
     TABLENAME = '[FVS_Carbon$]'
  else
     TABLENAME = 'FVS_Carbon'
  end if
  call DBSCKNROWS(IRCODE,TABLENAME,1,TRIM(DBMSOUT).eq.'EXCEL')
  if (IRCODE.eq.2) then
     ICMRPT = 0
     return
  end if
  if (IRCODE.eq.1) then
     if (TRIM(DBMSOUT).eq."ACCESS") then
        SQLStmtStr = 'CREATE TABLE FVS_Carbon('//&
             &'CaseID Text not null,'//&
             &'StandID Text null,'//&
             &'Year Int null,'//&
             &'Aboveground_Total_Live double null,'//&
             &'Aboveground_Merch_Live double null,'//&
             &'Belowground_Live double null,'//&
             &'Belowground_Dead double null,'//&
             &'Standing_Dead double null,'//&
             &'Forest_Down_Dead_Wood double null,'//&
             &'Forest_Floor double null,'//&
             &'Forest_Shrub_Herb double null,'//&
             &'Total_Stand_Carbon double null,'//&
             &'Total_Removed_Carbon double null,'//&
             &'Carbon_Released_From_Fire double null)'

     else if (TRIM(DBMSOUT).eq."EXCEL") then
        SQLStmtStr = 'CREATE TABLE FVS_Carbon('//&
             &'CaseID Text,'//&
             &'StandID Text,'//&
             &'Year Int ,'//&
             &'Aboveground_Total_Live Number,'//&
             &'Aboveground_Merch_Live Number,'//&
             &'Belowground_Live Number,'//&
             &'Belowground_Dead Number,'//&
             &'Standing_Dead Number,'//&
             &'Forest_Down_Dead_Wood Number,'//&
             &'Forest_Floor Number,'//&
             &'Forest_Shrub_Herb Number,'//&
             &'Total_Stand_Carbon Number,'//&
             &'Total_Removed_Carbon Number,'//&
             &'Carbon_Released_From_Fire Number)'
     else
        SQLStmtStr = 'CREATE TABLE FVS_Carbon('//&
             &'CaseID char(36) not null,'//&
             &'StandID char(26) not null,'//&
             &'Year Int null,'//&
             &'Aboveground_Total_Live real null,'//&
             &'Aboveground_Merch_Live real null,'//&
             &'Belowground_Live real null,'//&
             &'Belowground_Dead real null,'//&
             &'Standing_Dead real null,'//&
             &'Forest_Down_Dead_Wood real null,'//&
             &'Forest_Floor real null,'//&
             &'Forest_Shrub_Herb real null,'//&
             &'Total_Stand_Carbon real null,'//&
             &'Total_Removed_Carbon real null,'//&
             &'Carbon_Released_From_Fire real null)'
     end if
     !Close Cursor
     iRet = fvsSQLCloseCursor(StmtHndlOut)

     iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),&
          &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
     call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
          &'DBSFMCRPT:Creating Table: '//trim(SQLStmtStr))
  end if
  !     COPY INPUT VECTOR TO DOUBLE-PRECISION

  do I = 1,VARDIM
     VARD(I) = VAR(I)
  end do

  write(SQLStmtStr,*)'INSERT INTO ',TRIM(TABLENAME),' (CaseID,',&
       &'StandID,Year,Aboveground_Total_Live,Aboveground_Merch_Live,',&
       &'Belowground_Live,Belowground_Dead,Standing_Dead,',&
       &'Forest_Down_Dead_Wood,Forest_Floor,Forest_Shrub_Herb,',&
       &'Total_Stand_Carbon,Total_Removed_Carbon,',&
       &'Carbon_Released_From_Fire) VALUES (''',&
       &CASEID,''',''',TRIM(NPLT),&
       &''',?,?,?,?,?,?,?,?,?,?,?,?)'

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
       &'DBSFMCRPT:Inserting Row')

  200 continue
  !Release statement handle
  iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

end
