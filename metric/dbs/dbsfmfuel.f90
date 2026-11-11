!============================!
!   metric/dbs : dbsfmfuel   !
!============================!
! METRIC-DBS $Id$
subroutine DBSFMFUEL(IYEAR,NPLT,MSE,LITTER,DUFF,CLT3,CGT3,&
     &C3TO6,C6TO12,CGT12,HERB,CROWN,CTOTAL,PERCDUFF,PERCGT3,&
     &PERTRCR,SM25,SM10,KODE)
  implicit none

  !     PURPOSE: TO POPULATE A DATABASE WITH THE FUELS CONSUMPTION REPORT
  !              INFORMATION
  !     AUTH: S. REBAIN -- FMSC -- DECEMBER 2004
  !     INPUT:
  !              THE FUEL CONSUMPTION OUTPUT FROM THE FIRE MODEL.
  !              1: MINERAL SOIL EXPOSURE
  !              2: LITTER CONSUMPTION
  !              3: DUFF CONSUMPTION
  !              4: CONSUMPTION 0 - 7.6cm
  !              5: CONSUMPTION >= 7.6cm
  !              6: CONSUMPTION 7.6 - 15.2cm
  !              7: CONSUMPTION 1.2 - 30.6cm
  !              8: CONSUMPTION > 30.5cm
  !              9: HERB / SHRUB CONSUMPTION
  !             10: CROWN CONSUMPTION
  !             11: TOTAL CONSUMPTION
  !             12: % CONSUMPTION DUFF
  !             13: % CONSUMPTION >= 7.6cm
  !             14: % TREES WITH CROWNING
  !             15: SMOKE PRODUCTION < 2.5
  !             16: SMOKE PRODUCTION < 10
  !             17: KODE FOR WHETHER THE REPORT ALSO DUMPS TO FILE

  !     ICASE - CASE NUMBER FROM THE FVSRUN TABLE

  ! Common blocks
  include 'DBSCOM.f90'

  ! Variables
  integer :: IYEAR,ID,KODE,PERTRCR
  integer(SQLSMALLINT_KIND)::ColNumber
  real    :: MSE,LITTER,DUFF,CLT3,CGT3,C3TO6,C6TO12,CGT12,HERB,CROWN,CTOTAL,PERCDUFF,PERCGT3,SM25,SM10
  double precision :: MSEB,LITTERB,DUFFB,CLT3B,CGT3B,C3TO6B,C6TO12B,CGT12B,HERBB,CROWNB,CTOTALB,&
       &PERCDUFFB,PERCGT3B,SM25B,SM10B
  character(len=2000) :: SQLStmtStr
  character(len=20) :: TABLENAME
  character(len=26) :: NPLT

  !OMMONS END

  !     Initialize variables

  if (IFUELC.eq.0) return
  if (IFUELC.eq.2) KODE = 0

  !     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

  call DBSCASE(1)

  !     ALLOCATE A STATEMENT HANDLE

  iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
  if (iRet.ne.SQL_SUCCESS .and. iRet.ne. SQL_SUCCESS_WITH_INFO) then
     IFUELC = 0
     print *,'Error connecting to data source'
     call  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,&
          &'DBSFMFUEL:DSN Connection')
     goto 200
  end if

  !     CHECK TO SEE IF THE FUEL CONSUMP. TABLE EXISTS IN DATBASE

  if (TRIM(DBMSOUT).eq."EXCEL") then
     TABLENAME = '[FVS_Consumption$]'
  else
     TABLENAME = 'FVS_Consumption'
  end if
  SQLStmtStr = 'SELECT * FROM ' // TABLENAME

  iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),&
       &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

  if (.not.(iRet.eq.SQL_SUCCESS .or.&
       &iRet.eq.SQL_SUCCESS_WITH_INFO)) then
     if (TRIM(DBMSOUT).eq."ACCESS") then
        SQLStmtStr = 'CREATE TABLE FVS_Consumption('//&
             &'Id int primary key,'//&
             &'CaseID int not null,'//&
             &'StandID Text null,'//&
             &'Year Int null,'//&
             &'Min_Soil_Exp double null,'//&
             &'Litter_Consumption double null,'//&
             &'Duff_Consumption double null,'//&
             &'Consumption_lt76 double null,'//&
             &'Consumption_ge76 double null,'//&
             &'Consumption_76to152 double null,'//&
             &'Consumption_152to305 double null,'//&
             &'Consumption_ge305 double null,'//&
             &'Consumption_Herb_Shrub double null,'//&
             &'Consumption_Crowns double null,'//&
             &'Total_Consumption double null,'//&
             &'Percent_Consumption_Duff double null,'//&
             &'Percent_Consumption_ge76 double null,'//&
             &'Percent_Trees_Crowning double null,'//&
             &'Smoke_Production_25 double null,'//&
             &'Smoke_Production_10 double null)'

     else if (TRIM(DBMSOUT).eq."EXCEL") then
        SQLStmtStr = 'CREATE TABLE FVS_Consumption('//&
             &'ID Int,'//&
             &'CaseID int,'//&
             &'StandID Text,'//&
             &'Year Int,'//&
             &'Min_Soil_Exp Number,'//&
             &'Litter_Consumption Number,'//&
             &'Duff_Consumption Number,'//&
             &'Consumption_lt76 Number,'//&
             &'Consumption_ge76 Number,'//&
             &'Consumption_76to152 Number,'//&
             &'Consumption_152to305 Number,'//&
             &'Consumption_ge305 Number,'//&
             &'Consumption_Herb_Shrub Number,'//&
             &'Consumption_Crowns Number,'//&
             &'Total_Consumption Number,'//&
             &'Percent_Consumption_Duff Number,'//&
             &'Percent_Consumption_ge76 Number,'//&
             &'Percent_Trees_Crowning Number,'//&
             &'Smoke_Production_25 Number,'//&
             &'Smoke_Production_10 Number)'
     else
        SQLStmtStr = 'CREATE TABLE FVS_Consumption('//&
             &'Id int primary key,'//&
             &'CaseID int not null,'//&
             &'StandID char(26) not null,'//&
             &'Year Int null,'//&
             &'Min_Soil_Exp real null,'//&
             &'Litter_Consumption real null,'//&
             &'Duff_Consumption real null,'//&
             &'Consumption_lt76 real null,'//&
             &'Consumption_ge76 real null,'//&
             &'Consumption_76to152 real null,'//&
             &'Consumption_152to305 real null,'//&
             &'Consumption_ge305 real null,'//&
             &'Consumption_Herb_Shrub real null,'//&
             &'Consumption_Crowns real null,'//&
             &'Total_Consumption real null,'//&
             &'Percent_Consumption_Duff real null,'//&
             &'Percent_Consumption_ge76 real null,'//&
             &'Percent_Trees_Crowning real null,'//&
             &'Smoke_Production_25 real null,'//&
             &'Smoke_Production_10 real null)'
     end if

     iRet = fvsSQLCloseCursor(StmtHndlOut)
     iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),&
          &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
     call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
          &'DBSFMFUEL:Creating Table: '//trim(SQLStmtStr))
     CONID = 0
  end if

  !     CREATE ENTRY FROM DATA FOR FUEL CONSUMPTION TABLE

  if (CONID.eq.-1) then
     call DBSGETID(TABLENAME,'Id',ID)
     CONID = ID
  end if
  CONID = CONID + 1

  !     MAKE SURE WE DO NOT EXCEED THE MAX TABLE SIZE IN EXCEL

  if (CONID.ge.65535.and.TRIM(DBMSOUT).eq.'EXCEL') goto 100

  !     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS

  MSEB = MSE
  LITTERB = LITTER
  DUFFB = DUFF
  CLT3B = CLT3
  CGT3B = CGT3
  C3TO6B = C3TO6
  C6TO12B = C6TO12
  CGT12B = CGT12
  HERBB = HERB
  CROWNB = CROWN
  CTOTALB = CTOTAL
  PERCDUFFB = PERCDUFF
  PERCGT3B = PERCGT3
  SM25B = SM25
  SM10B = SM10

  WRITE(SQLStmtStr,*)'INSERT INTO ',TABLENAME,' (Id,CaseID,         &
       &  StandID,Year,Min_Soil_Exp,Litter_Consumption,Duff_Consumption,  &
       &  Consumption_lt76,Consumption_ge76,Consumption_76to152,          &
       &  Consumption_152to305,Consumption_ge305,Consumption_Herb_Shrub,  &
       &  Consumption_Crowns,Total_Consumption,Percent_Consumption_Duff,  &
       &  Percent_Consumption_ge76,Percent_Trees_Crowning,                &
       &  Smoke_Production_25,Smoke_Production_10) VALUES(?,?,',&
       &  CHAR(39),TRIM(NPLT),CHAR(39),',?,?,?,?,?,?,?,?,?,?,?,?,?,?      &
       &  ,?,?,?)'

  iRet = fvsSQLCloseCursor(StmtHndlOut)
  iRet = fvsSQLPrepare(StmtHndlOut, trim(SQLStmtStr),&
       &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

  !     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

  ColNumber = 1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),CONID,int(4,SQLLEN_KIND),&
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
       &INT(5,SQLSMALLINT_KIND),MSEB,int(4,SQLLEN_KIND),&
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
       &INT(5,SQLSMALLINT_KIND),CLT3B,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),CGT3B,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),C3TO6B,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),C6TO12B,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),CGT12B,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),HERBB,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),CROWNB,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),CTOTALB,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),PERCDUFFB,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),PERCGT3B,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),PERTRCR,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),SM25B,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),SM10B,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  100 continue
  !Close Cursor
  iRet = fvsSQLCloseCursor(StmtHndlOut)

  iRet = fvsSQLExecute(StmtHndlOut)
  call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
       &'DBSFMFUEL:Inserting Row')

  200 continue
  !Release statement handle
  iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

end
