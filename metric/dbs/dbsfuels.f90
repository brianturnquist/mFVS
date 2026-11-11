!===========================!
!   metric/dbs : dbsfuels   !
!===========================!
! METRIC-DBS $Id$
subroutine DBSFUELS(IYEAR,NPLT,LITTER,DUFF,SDEADLT3,SDEADGT3,&
     &SDEAD3TO6,SDEAD6TO12,SDEADGT12,HERB,SHRUB,SURFTOTAL,SNAGSLT3,&
     &SNAGSGT3,FOLIAGE,STANDLT3,STANDGT3,STANDTOTAL,BIOMASS,CONSUMED,&
     &REMOVED,KODE)
  implicit none

  !     PURPOSE: TO POPULATE A DATABASE WITH THE FUELS REPORT INFORMATION
  !     AUTH: D. GAMMEL -- RMRS -- NOVEMBER 2002
  !     INPUT:
  !              THE ALL FUELS OUTPUT FROM THE FIRE MODEL.
  !              1: SURFACE LITTER
  !              2: SURFACE DUFF
  !              3: SURFACE DEAD FUEL LESS THAN 3"
  !              4: SURFACE DEAD FUEL GREATER THAN OR EQUAL TO 3"
  !              5: SURFACE DEAD FUEL BETWEEN 3" AND 6"
  !              6: SURFACE DEAD FUEL BETWEEN 6" AND 12"
  !              7: SURFACE DEAD FUEL GREATER THAN OR EQUAL TO 12"
  !              8: SURFACE HERB FUEL
  !              9: SURFACE SHRUB FUEL
  !             10: SURFACE TOTAL FUEL
  !             11: STANDING SNAGS LESS THAN 3"
  !             12: STANDING SNAGS GREATER THAN OR EQUAL TO 3"
  !             13: STANDING FOLIAGE
  !             14: STANDING LIVE LESS THAN 3"
  !             15: STANDING LIVE GREATER THAN OR EQUAL TO 3"
  !             16: STANDING TOTAL
  !             17: TOTAL BIOMASS
  !             18: TOTAL CONSUMED
  !             19: BIOMASS REMOVED
  !             20: KODE FOR WHETHER REPORT ALSO DUMPS TO FILE

  !     ICASE - CASE NUMBER FROM THE FVSRUN TABLE

  ! Common blocks
  include 'DBSCOM.f90'

  ! Variables
  integer :: IYEAR,BIOMASS,CONSUMED,REMOVED,ID,KODE,STANDGT3,STANDTOTAL
  integer(SQLSMALLINT_KIND)::ColNumber
  real    :: LITTER,DUFF,SDEADLT3,SDEADGT3,SDEAD3TO6,SDEAD6TO12,SDEADGT12,HERB,SHRUB,SURFTOTAL,&
       &SNAGSLT3,SNAGSGT3,FOLIAGE,STANDLT3
  double precision :: LITTERB,DUFFB,SDEADLT3B,SDEADGT3B,SDEAD3TO6B,SDEAD6TO12B,SDEADGT12B,HERBB,&
       &SHRUBB,SURFTOTALB,SNAGSLT3B,SNAGSGT3B,FOLIAGEB,STANDLT3B
  character(len=2000) :: SQLStmtStr
  character(len=20) :: TABLENAME
  character(len=26) :: NPLT

  !OMMONS END

  !     Initialize variables

  if (IFUELS.eq.0) return
  if (IFUELS.eq.2) KODE = 0

  !     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

  call DBSCASE(1)

  !     ALLOCATE A STATEMENT HANDLE

  iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
  if (iRet.ne.SQL_SUCCESS .and. iRet.ne. SQL_SUCCESS_WITH_INFO) then
     IFUELS = 0
     print *,'Error connecting to data source'
     call  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,&
          &'DBSSUMRY:DSN Connection')
     goto 200
  end if

  !     CHECK TO SEE IF THE POTFIRE TABLE EXISTS IN DATBASE

  if (TRIM(DBMSOUT).eq."EXCEL") then
     TABLENAME = '[FVS_Fuels$]'
  else
     TABLENAME = 'FVS_Fuels'
  end if
  SQLStmtStr = 'SELECT * FROM ' // TABLENAME

  iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),&
       &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

  if (.not.(iRet.eq.SQL_SUCCESS .or.&
       &iRet.eq.SQL_SUCCESS_WITH_INFO)) then
     if (TRIM(DBMSOUT).eq."ACCESS") then
        SQLStmtStr = 'CREATE TABLE FVS_Fuels('//&
             &'Id int primary key,'//&
             &'CaseID int not null,'//&
             &'StandID Text null,'//&
             &'Year Int null,'//&
             &'Surface_Litter double null,'//&
             &'Surface_Duff double null,'//&
             &'Surface_lt76 double null,'//&
             &'Surface_ge76 double null,'//&
             &'Surface_76to152 double null,'//&
             &'Surface_152to305 double null,'//&
             &'Surface_ge305 double null,'//&
             &'Surface_Herb double null,'//&
             &'Surface_Shrub double null,'//&
             &'Surface_Total double null,'//&
             &'Standing_Snag_lt76 double null,'//&
             &'Standing_Snag_ge76 double null,'//&
             &'Standing_Foliage double null,'//&
             &'Standing_Live_lt76 double null,'//&
             &'Standing_Live_ge76 double null,'//&
             &'Standing_Total double null,'//&
             &'Total_Biomass Int null,'//&
             &'Total_Consumed Int null,'//&
             &'Biomass_Removed Int null)'

     else if (TRIM(DBMSOUT).eq."EXCEL") then
        SQLStmtStr = 'CREATE TABLE FVS_Fuels('//&
             &'ID Int,'//&
             &'CaseID int,'//&
             &'StandID Text,'//&
             &'Year Int,'//&
             &'Surface_Litter Number,'//&
             &'Surface_Duff Number,'//&
             &'Surface_lt76 Number,'//&
             &'Surface_ge76 Number,'//&
             &'Surface_76to152 Number,'//&
             &'Surface_152to305 Number,'//&
             &'Surface_ge305 Number,'//&
             &'Surface_Herb Number,'//&
             &'Surface_Shrub Number,'//&
             &'Surface_Total Number,'//&
             &'Standing_Snag_lt76 Number,'//&
             &'Standing_Snag_ge76 Number,'//&
             &'Standing_Foliage Number,'//&
             &'Standing_Live_lt76 Number,'//&
             &'Standing_Live_ge76 Number,'//&
             &'Standing_Total Number,'//&
             &'Total_Biomass Int,'//&
             &'Total_Consumed Int,'//&
             &'Biomass_Removed Int)'
     else
        SQLStmtStr = 'CREATE TABLE FVS_Fuels('//&
             &'Id int primary key,'//&
             &'CaseID int not null,'//&
             &'StandID char(26) not null,'//&
             &'Year Int null,'//&
             &'Surface_Litter real null,'//&
             &'Surface_Duff real null,'//&
             &'Surface_lt76 real null,'//&
             &'Surface_ge76 real null,'//&
             &'Surface_76to152 real null,'//&
             &'Surface_152to305 real null,'//&
             &'Surface_ge305 real null,'//&
             &'Surface_Herb real null,'//&
             &'Surface_Shrub real null,'//&
             &'Surface_Total real null,'//&
             &'Standing_Snag_lt76 real null,'//&
             &'Standing_Snag_ge76 real null,'//&
             &'Standing_Foliage real null,'//&
             &'Standing_Live_lt76 real null,'//&
             &'Standing_Live_ge76 real null,'//&
             &'Standing_Total real null,'//&
             &'Total_Biomass Int null,'//&
             &'Total_Consumed Int null,'//&
             &'Biomass_Removed Int null)'
     end if

     iRet = fvsSQLCloseCursor(StmtHndlOut)
     iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),&
          &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
     call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
          &'DBSFUELS:Creating Table: '//trim(SQLStmtStr))
     FUELID = 0
  end if

  !     CREATE ENTRY FROM DATA FOR SUMMARYSTAT TABLE

  if (FUELID.eq.-1) then
     call DBSGETID(TABLENAME,'Id',ID)
     FUELID = ID
  end if
  FUELID = FUELID + 1

  !     MAKE SURE WE DO NOT EXCEED THE MAX TABLE SIZE IN EXCEL

  if (FUELID.ge.65535.and.TRIM(DBMSOUT).eq.'EXCEL') goto 100

  !     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS

  LITTERB = LITTER
  DUFFB = DUFF
  SDEADLT3B = SDEADLT3
  SDEADGT3B = SDEADGT3
  SDEAD3TO6B = SDEAD3TO6
  SDEAD6TO12B = SDEAD6TO12
  SDEADGT12B = SDEADGT12
  HERBB = HERB
  SHRUBB = SHRUB
  SURFTOTALB = SURFTOTAL
  SNAGSLT3B = SNAGSLT3
  SNAGSGT3B = SNAGSGT3
  FOLIAGEB = FOLIAGE
  STANDLT3B = STANDLT3

  WRITE(SQLStmtStr,*)'INSERT INTO ',TABLENAME,' (Id,CaseID,         &
       &  StandID,Year,Surface_Litter,Surface_Duff,Surface_lt76,          &
       &  Surface_ge76,Surface_76to152,Surface_152to305,Surface_ge305,    &
       &  Surface_Herb,Surface_Shrub,Surface_Total,Standing_Snag_lt76,    &
       &  Standing_Snag_ge76,Standing_Foliage,Standing_Live_lt76,         &
       &  Standing_Live_ge76,Standing_Total,Total_Biomass,                &
       &  Total_Consumed,Biomass_Removed) VALUES(?,?,',&
       &  CHAR(39),TRIM(NPLT),CHAR(39),',?,?,?,?,?,?,?,?,?,?,?,?,?,?      &
       &  ,?,?,?,?,?,?)'

  iRet = fvsSQLCloseCursor(StmtHndlOut)
  iRet = fvsSQLPrepare(StmtHndlOut, trim(SQLStmtStr),&
       &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

  !     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

  ColNumber = 1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),FUELID,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),ICASE,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),IYEAR,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),LITTERB,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),DUFFB,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),SDEADLT3B,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),SDEADGT3B,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),SDEAD3TO6B,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),SDEAD6TO12B,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),SDEADGT12B,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),HERBB,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),SHRUBB,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),SURFTOTALB,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),SNAGSLT3B,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),SNAGSGT3B,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),FOLIAGEB,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),STANDLT3B,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),STANDGT3,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),STANDTOTAL,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),BIOMASS,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),CONSUMED,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),REMOVED,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  100 continue
  !Close Cursor
  iRet = fvsSQLCloseCursor(StmtHndlOut)
  iRet = fvsSQLExecute(StmtHndlOut)
  call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
       &'DBSFUELS:Inserting Row')

  200 continue
  !Release statement handle
  iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

end
