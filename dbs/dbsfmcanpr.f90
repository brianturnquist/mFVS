!======================!
!   dbs : dbsfmcanpr   !
!======================!
! DBS $Id$
subroutine DBSFMCANPR(IYEAR,CRFILL,NPLT)
  implicit none

  !     PURPOSE: TO POPULATE A DATABASE WITH CANOPY PROFILE
  !              INFORMATION
  !     AUTH: S. REBAIN -- FMSC -- JUNE 2006
  !     INPUT:
  !              THE CANOPY PROFILE INFO FROM THE FIRE MODEL.
  !              1: BIOMASS OF CANOPY FUELS AT VARIOUS HEIGHTS
  !                  ABOVE THE GROUND IN LBS/ACRE/FOOT.

  !     THIS TABLE IS UNIQUE IN THAT THERE IS NOT A CORRESPONDING
  !     TEXT FILE TABLE.

  ! Common blocks
  include 'DBSCOM.f90'

  ! Variables
  integer :: IYEAR,IRCODE,I
  integer(SQLSMALLINT_KIND)::ColNumber
  real    :: CRFILL
  double precision :: CRFILLB,CRFILLKG,HTFT,HTM
  dimension CRFILL(400), CRFILLB(400), CRFILLKG(400)
  character(len=2000) :: SQLStmtStr
  character(len=20) :: TABLENAME
  character(len=26) :: NPLT

  !     Initialize variables

  if (ICANPR.eq.0) return

  !     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

  call DBSCASE(1)

  !     ALLOCATE A STATEMENT HANDLE

  iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
  if (iRet.ne.SQL_SUCCESS .and. iRet.ne. SQL_SUCCESS_WITH_INFO) then
     ICANPR = 0
     print *,'Error connecting to data source'
     call  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,&
          &'DBSFMCANPR:DSN Connection')
     goto 200
  end if

  !     CHECK TO SEE IF THE CANOPY PROFILE TABLE EXISTS IN DATABASE

  if (TRIM(DBMSOUT).eq."EXCEL") then
     TABLENAME = '[FVS_CanProfile$]'
  else
     TABLENAME = 'FVS_CanProfile'
  end if
  call DBSCKNROWS(IRCODE,TABLENAME,400,TRIM(DBMSOUT).eq.'EXCEL')
  if (IRCODE.eq.2) then
     ICANPR = 0
     return
  end if
  if (IRCODE.eq.1) then
     if (TRIM(DBMSOUT).eq."ACCESS") then
        SQLStmtStr = 'CREATE TABLE FVS_CanProfile('//&
             &'CaseID Text not null,'//&
             &'StandID Text null,'//&
             &'Year Int null,'//&
             &'Height_m double null,'//&
             &'Canopy_Fuel_kg_m3 double null,'//&
             &'Height_ft double null,'//&
             &'Canopy_Fuel_lbs_acre_ft double null)'

     else if (TRIM(DBMSOUT).eq."EXCEL") then
        SQLStmtStr = 'CREATE TABLE FVS_CanProfile('//&
             &'CaseID Text,'//&
             &'StandID Text,'//&
             &'Year Int,'//&
             &'Height_m Number,'//&
             &'Canopy_Fuel_kg_m3 Number,'//&
             &'Height_ft Number,'//&
             &'Canopy_Fuel_lbs_acre_ft Number)'

     else
        SQLStmtStr = 'CREATE TABLE FVS_CanProfile('//&
             &'CaseID char(36) not null,'//&
             &'StandID char(26) not null,'//&
             &'Year Int null,'//&
             &'Height_m real null,'//&
             &'Canopy_Fuel_kg_m3 real null,'//&
             &'Height_ft real null,'//&
             &'Canopy_Fuel_lbs_acre_ft real null)'

     end if
     !Close Cursor
     iRet = fvsSQLCloseCursor(StmtHndlOut)

     iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),&
          &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
     call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
          &'DBSFMCANPR:Creating Table: '//trim(SQLStmtStr))
  end if

  do I = 1,400

     if (CRFILL(I) .le.0) goto 150

     !       ASSIGN VALUES TO DOUBLE PRECISION VARS

     CRFILLB(I) = CRFILL(I)
     CRFILLKG(I) = CRFILL(I)*0.45359237 / (4046.856422 * 0.3048)
     HTFT = I
     HTM = I*0.3048

     write(SQLStmtStr,*)'INSERT INTO ',TABLENAME,' (CaseID,',&
          &'StandID,Year,Height_m,Canopy_Fuel_kg_m3,Height_ft,',&
          &'Canopy_Fuel_lbs_acre_ft) VALUES (''',CASEID,&
          &''',''',TRIM(NPLT),''',?,?,?,?,?)'

     !PRINT*, SQLStmtStr

     !       CLOSE CURSOR

     iRet = fvsSQLCloseCursor(StmtHndlOut)

     !       PREPARE THE SQL QUERY

     iRet = fvsSQLPrepare(StmtHndlOut, trim(SQLStmtStr),&
          &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

     !       BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

     ColNumber = 1
     iRet = fvsSQLBindParameter(StmtHndlOut, ColNumber,&
          &SQL_PARAM_INPUT, SQL_F_INTEGER, SQL_INTEGER,&
          &INT(15,SQLUINTEGER_KIND),INT(0,SQLSMALLINT_KIND),&
          &IYEAR,int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut, ColNumber,&
          &SQL_PARAM_INPUT, SQL_F_DOUBLE, SQL_DOUBLE,&
          &INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),&
          &HTM,int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
          &SQL_PARAM_INPUT, SQL_F_DOUBLE, SQL_DOUBLE,&
          &INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),&
          &CRFILLKG(I),int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut, ColNumber,&
          &SQL_PARAM_INPUT, SQL_F_DOUBLE, SQL_DOUBLE,&
          &INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),&
          &HTFT,int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
          &SQL_PARAM_INPUT, SQL_F_DOUBLE, SQL_DOUBLE,&
          &INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),&
          &CRFILLB(I),int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     !Close Cursor
     iRet = fvsSQLCloseCursor(StmtHndlOut)

     iRet = fvsSQLExecute(StmtHndlOut)
     call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
          &'DBSFMCANPR:Inserting Row')
     150 continue

  end do

  200 continue
  !Release statement handle
  iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

end
