!======================!
!   dbs : dbsfmdsnag   !
!======================!
! DBS $Id$
subroutine DBSFMDSNAG(IYEAR,SDBH,SHTH,SHTS,SVLH,SVLS,&
     &SDH,SDS,YRLAST,KODE)
  implicit none

  !     PURPOSE: TO POPULATE A DATABASE WITH THE DETAILED SNAG REPORT
  !              INFORMATION
  !     AUTH: S. REBAIN -- FMSC -- DECEMBER 2004
  !     INPUT:
  !              THE DETAILED SNAG OUTPUT FROM THE FIRE MODEL.
  !              1: AVERAGE SNAG DIAMETER FOR THE RECORD
  !              2: CURRENT HEIGHT OF HARD SNAGS
  !              3: CURRENT HEIGHT OF SOFT SNAGS
  !              4: CURRENT VOLUME OF HARD SNAGS
  !              5: CURRENT VOLUME OF SOFT SNAGS
  !              6: DENSITY OF HARD SNAGS
  !              7: DENSITY OF SOFT SNAGS
  !              8: YRLAST
  !              9: KODE FOR WHETHER THE REPORT ALSO DUMPS TO FILE

  ! Common blocks
  include 'PRGPRM.f90'
  include 'DBSCOM.f90'
  include 'PLOT.f90'

  ! Variables
  integer :: IYEAR,IRCODE,KODE,YRDEAD,SVLH,SVLS,SVLT,YRLAST,JYR,IDC,JCL
  integer(SQLSMALLINT_KIND)::ColNumber
  real    :: SDBH,SHTH,SHTS,SDH,SDS,SDT
  double precision :: SDBHB,SHTHB,SHTSB,SDHB,SDSB,SDTB
  dimension SVLH(MAXSP,100,6), SVLS(MAXSP,100,6), SVLT(MAXSP,100,6),&
       &SDBH(MAXSP,100,6), SDBHB(MAXSP,100,6), SHTH(MAXSP,100,6),&
       &SHTHB(MAXSP,100,6), SHTS(MAXSP,100,6), SHTSB(MAXSP,100,6),&
       &SDH(MAXSP,100,6),SDS(MAXSP,100,6),SDT(MAXSP,100,6),&
       &SDHB(MAXSP,100,6),SDSB(MAXSP,100,6),SDTB(MAXSP,100,6)
  character(len=2000) :: SQLStmtStr
  character(len=20) :: TABLENAME
  character(len=3) :: CSP
  character(LEN = 8) :: CSPECIES

  !     Initialize variables

  if (ISDET.eq.0) return
  if (ISDET.eq.2) KODE = 0

  !     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

  call DBSCASE(1)

  !     ALLOCATE A STATEMENT HANDLE

  iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
  if (iRet.ne.SQL_SUCCESS .and. iRet.ne. SQL_SUCCESS_WITH_INFO) then
     ISDET = 0
     print *,'Error connecting to data source'
     call  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,&
          &'DBSFMDSNAG:DSN Connection')
     goto 200
  end if

  !     CHECK TO SEE IF THE DETAILED SNAG TABLE EXISTS IN DATBASE

  if (TRIM(DBMSOUT).eq."EXCEL") then
     TABLENAME = '[FVS_SnagDet$]'
  else
     TABLENAME = 'FVS_SnagDet'
  end if
  call DBSCKNROWS(IRCODE,TABLENAME,MAXSP*YRLAST*6,&
       &TRIM(DBMSOUT).eq.'EXCEL')
  if (IRCODE.eq.2) then
     ISDET = 0
     return
  end if
  if (IRCODE.eq.1) then
     if (TRIM(DBMSOUT).eq."ACCESS") then
        SQLStmtStr = 'CREATE TABLE FVS_SnagDet('//&
             &'CaseID Text not null,'//&
             &'StandID Text null,'//&
             &'Year Int null,'//&
             &'Species Text null,'//&
             &'DBH_Class int null,'//&
             &'Death_DBH double null,'//&
             &'Current_Ht_Hard double null,'//&
             &'Current_Ht_Soft double null,'//&
             &'Current_Vol_Hard double null,'//&
             &'Current_Vol_Soft double null,'//&
             &'Total_Volume double null,'//&
             &'Year_Died int null,'//&
             &'Density_Hard double null,'//&
             &'Density_Soft double null,'//&
             &'Density_Total double null)'

     else if (TRIM(DBMSOUT).eq."EXCEL") then
        SQLStmtStr = 'CREATE TABLE FVS_SnagDet('//&
             &'CaseID Text,'//&
             &'StandID Text,'//&
             &'Year Int,'//&
             &'Species Text,'//&
             &'DBH_Class int,'//&
             &'Death_DBH Number,'//&
             &'Current_Ht_Hard Number,'//&
             &'Current_Ht_Soft Number,'//&
             &'Current_Vol_Hard Number,'//&
             &'Current_Vol_Soft Number,'//&
             &'Total_Volume Number,'//&
             &'Year_Died int,'//&
             &'Density_Hard Number,'//&
             &'Density_Soft Number,'//&
             &'Density_Total Number)'

     else
        SQLStmtStr = 'CREATE TABLE FVS_SnagDet('//&
             &'CaseID char(36) not null,'//&
             &'StandID char(26) not null,'//&
             &'Year Int null,'//&
             &'Species char(3) null,'//&
             &'DBH_Class int null,'//&
             &'Death_DBH real null,'//&
             &'Current_Ht_Hard real null,'//&
             &'Current_Ht_Soft real null,'//&
             &'Current_Vol_Hard real null,'//&
             &'Current_Vol_Soft real null,'//&
             &'Total_Volume real null,'//&
             &'Year_Died int null,'//&
             &'Density_Hard real null,'//&
             &'Density_Soft real null,'//&
             &'Density_Total real null)'

     end if

     !Close Cursor
     iRet = fvsSQLCloseCursor(StmtHndlOut)
     iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),&
          &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
     call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
          &'DBSFMDSNAG:Creating Table: '//trim(SQLStmtStr))
  end if

  do JYR = 1,YRLAST
     do IDC = 1,MAXSP
        do JCL = 1,6

           CSP = JSP(IDC)(1:3)
           YRDEAD = IYEAR - JYR + 1
           SDT(IDC,JYR,JCL) = SDH(IDC,JYR,JCL) + SDS(IDC,JYR,JCL)
           SVLT(IDC,JYR,JCL) = SVLH(IDC,JYR,JCL) + SVLS(IDC,JYR,JCL)
           if (SDT(IDC,JYR,JCL) .le.0) goto 150

           !             DETERMINE PREFERED OUTPUT FORMAT FOR SPECIES CODE
           !             KEYWORD OVER RIDES

           if (JSPIN(IDC).eq.1) then
              CSPECIES = ADJUSTL(JSP(IDC))
           else if (JSPIN(IDC).eq.2) then
              CSPECIES = ADJUSTL(FIAJSP(IDC))
           else if (JSPIN(IDC).eq.3) then
              CSPECIES = ADJUSTL(PLNJSP(IDC))
           else
              CSPECIES = ADJUSTL(PLNJSP(IDC))
           end if

           if (ISPOUT23.eq.1)CSPECIES = ADJUSTL(JSP(IDC))
           if (ISPOUT23.eq.2)CSPECIES = ADJUSTL(FIAJSP(IDC))
           if (ISPOUT23.eq.3)CSPECIES = ADJUSTL(PLNJSP(IDC))

           !             ASSIGN REAL VALUES TO DOUBLE PRECISION VARS

           SDBHB(IDC,JYR,JCL) = SDBH(IDC,JYR,JCL)
           SHTHB(IDC,JYR,JCL) = SHTH(IDC,JYR,JCL)
           SHTSB(IDC,JYR,JCL) = SHTS(IDC,JYR,JCL)
           SDHB(IDC,JYR,JCL) = SDH(IDC,JYR,JCL)
           SDSB(IDC,JYR,JCL) = SDS(IDC,JYR,JCL)
           SDTB(IDC,JYR,JCL) = SDT(IDC,JYR,JCL)

           write(SQLStmtStr,*)'INSERT INTO ',TABLENAME,&
                &'(CaseID,StandID,Year,Species,DBH_Class,Death_DBH,',&
                &'Current_Ht_Hard,Current_Ht_Soft,Current_Vol_Hard,',&
                &'Current_Vol_Soft,Total_Volume,Year_Died,Density_Hard,',&
                &'Density_Soft,Density_Total) VALUES (''',&
                &CASEID,''',''',TRIM(NPLT),''',?,''',&
                &TRIM(CSPECIES),''',?,?,?,?,?,?,?,?,?,?,?)'

           !PRINT*, SQLStmtStr

           !             CLOSE CURSOR

           iRet = fvsSQLCloseCursor(StmtHndlOut)

           !             PREPARE THE SQL QUERY

           iRet = fvsSQLPrepare(StmtHndlOut, trim(SQLStmtStr),&
                &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

           !             BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

           ColNumber = 1
           iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
                &SQL_PARAM_INPUT, SQL_F_INTEGER, SQL_INTEGER,&
                &INT(15,SQLUINTEGER_KIND),INT(0,SQLSMALLINT_KIND),&
                &IYEAR,int(4,SQLLEN_KIND),&
                &SQL_NULL_PTR)

           ColNumber = ColNumber+1
           iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
                &SQL_PARAM_INPUT, SQL_F_INTEGER, SQL_INTEGER,&
                &INT(15,SQLUINTEGER_KIND),INT(0,SQLSMALLINT_KIND),&
                &JCL,int(4,SQLLEN_KIND),&
                &SQL_NULL_PTR)

           ColNumber = ColNumber+1
           iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
                &SQL_PARAM_INPUT, SQL_F_DOUBLE, SQL_DOUBLE,&
                &INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),&
                &SDBHB(IDC,JYR,JCL),int(4,SQLLEN_KIND),&
                &SQL_NULL_PTR)

           ColNumber = ColNumber+1
           iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
                &SQL_PARAM_INPUT,SQL_F_DOUBLE, SQL_DOUBLE,&
                &INT(15,SQLUINTEGER_KIND), INT(5,SQLSMALLINT_KIND),&
                &SHTHB(IDC,JYR,JCL),int(4,SQLLEN_KIND),&
                &SQL_NULL_PTR)

           ColNumber = ColNumber+1
           iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
                &SQL_PARAM_INPUT, SQL_F_DOUBLE, SQL_DOUBLE,&
                &INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),&
                &SHTSB(IDC,JYR,JCL),int(4,SQLLEN_KIND),&
                &SQL_NULL_PTR)

           ColNumber = ColNumber+1
           iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
                &SQL_PARAM_INPUT,SQL_F_INTEGER, SQL_INTEGER,&
                &INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),&
                &SVLH(IDC,JYR,JCL),int(4,SQLLEN_KIND),&
                &SQL_NULL_PTR)

           ColNumber = ColNumber+1
           iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
                &SQL_PARAM_INPUT,SQL_F_INTEGER, SQL_INTEGER,&
                &INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),&
                &SVLS(IDC,JYR,JCL),int(4,SQLLEN_KIND),&
                &SQL_NULL_PTR)

           ColNumber = ColNumber+1
           iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
                &SQL_PARAM_INPUT, SQL_F_INTEGER, SQL_INTEGER,&
                &INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),&
                &SVLT(IDC,JYR,JCL),int(4,SQLLEN_KIND),&
                &SQL_NULL_PTR)

           ColNumber = ColNumber+1
           iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
                &SQL_PARAM_INPUT, SQL_F_INTEGER, SQL_INTEGER,&
                &INT(15,SQLUINTEGER_KIND), INT(5,SQLSMALLINT_KIND),&
                &YRDEAD,int(4,SQLLEN_KIND),&
                &SQL_NULL_PTR)

           ColNumber = ColNumber+1
           iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
                &SQL_PARAM_INPUT, SQL_F_DOUBLE, SQL_DOUBLE,&
                &INT(15,SQLUINTEGER_KIND), INT(5,SQLSMALLINT_KIND),&
                &SDHB(IDC,JYR,JCL),int(4,SQLLEN_KIND),&
                &SQL_NULL_PTR)

           ColNumber = ColNumber+1
           iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
                &SQL_PARAM_INPUT, SQL_F_DOUBLE, SQL_DOUBLE,&
                &INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),&
                &SDSB(IDC,JYR,JCL),int(4,SQLLEN_KIND),&
                &SQL_NULL_PTR)

           ColNumber = ColNumber+1
           iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
                &SQL_PARAM_INPUT,SQL_F_DOUBLE, SQL_DOUBLE,&
                &INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),&
                &SDTB(IDC,JYR,JCL),int(4,SQLLEN_KIND),&
                &SQL_NULL_PTR)

           iRet = fvsSQLCloseCursor(StmtHndlOut)

           iRet = fvsSQLExecute(StmtHndlOut)
           call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
                &'DBSFMDSNAG:Inserting Row')
           150 continue
        end do
     end do
  end do

  200 continue
  !Release statement handle
  iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

end
