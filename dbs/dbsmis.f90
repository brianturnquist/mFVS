!==================!
!   dbs : dbsmis   !
!==================!
! DBS $Id$
subroutine DBSMIS1(IYEAR,NPLT,CSP,&
     &SPDMR4,SPDMI4,SPINF4,SPMRT4,SPPIN4,SPPMR4,SPPOC4,&
     &KODE)
  implicit none

  !     PURPOSE: TO POPULATE A DATABASE WITH THE 1ST MISTLETOE REPORT INFORMATION
  !     AUTH: D. ROBINSON, ESSA - BASED ON D. GAMMEL (DBSFUELS)

  !     1:     Year
  !     2:     Mean DMR for species - SPDMR4
  !     3:     Mean DMI for species - SPDMI4
  !     4:     Infected trees/acre for species - SPINF4
  !     5:     Mortality trees/acre for species - SPMRT4
  !     6:     Infected trees/acre % for species - SPPIN4
  !     7:     Mortality trees/acre % for species - SPPMR4
  !     8:     Stand trees/acre % for species - SPPOC4

  ! Common blocks
  include 'DBSCOM.f90'

  !     ARGUMENT LIST

  ! Variables
  integer :: IYEAR
  real    :: SPDMR4(4),SPDMI4(4),SPINF4(4),SPMRT4(4)
  real    :: SPPIN4(4),SPPMR4(4),SPPOC4(4)
  integer :: KODE,IRCODE

  !     LOCAL VARIABLES

  integer(SQLSMALLINT_KIND)::ColNumber
  integer :: I

  double precision :: SPDMR4B,SPDMI4B
  integer :: ISPINF4,ISPMRT4,ISPPIN4,ISPPMR4,ISPPOC4

  character(len=2000) :: SQLStmtStr
  character(LEN = 20) :: TABLENAME
  character(LEN = 26) :: NPLT
  character(LEN = 2) :: CSP(4)

  !     INITIALIZE VARIABLES

  if (IDM1.eq.0) return
  if (IDM1.eq.2) KODE = 0

  !     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

  call DBSCASE(1)

  !     ALLOCATE A STATEMENT HANDLE

  iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut,StmtHndlOut)

  if (iRet.ne.SQL_SUCCESS .and. iRet.ne. SQL_SUCCESS_WITH_INFO) then
     IDM1 = 0
     print *,'Error connecting to data source'
     call  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,&
          &'DBSMIS1:DSN Connection')
     goto 100
  end if

  !     CHECK TO SEE IF THE DM TABLE EXISTS IN THE DATBASE

  if (TRIM(DBMSOUT).eq."EXCEL") then
     TABLENAME = '[FVS_DM_Spp_Sum$]'
  else if (TRIM(DBMSOUT).eq."ORACLE") then
     TABLENAME = '"FVS_DM_Spp_Sum"'
  else
     TABLENAME = 'FVS_DM_Spp_Sum'
  end if

  call DBSCKNROWS(IRCODE,TABLENAME,1,TRIM(DBMSOUT).eq.'EXCEL')

  if (IRCODE.eq.2) then
     IDM1 = 0
     return
  end if
  if (IRCODE.eq.1) then
     if (TRIM(DBMSOUT).eq."ACCESS") then
        SQLStmtStr = 'CREATE TABLE FVS_DM_Spp_Sum ('//&
             &'CaseID text not null,'//&
             &'StandID text null,'//&
             &'Year Int null,'//&
             &'Spp Text null,'//&
             &'Mean_DMR double null,'//&
             &'Mean_DMI double null,'//&
             &'Inf_TPA int null,'//&
             &'Mort_TPA int null,'//&
             &'Inf_TPA_Pct int null,'//&
             &'Mort_TPA_Pct int null,'//&
             &'Stnd_TPA_Pct int null)'
     else if (TRIM(DBMSOUT).eq."EXCEL") then
        SQLStmtStr = 'CREATE TABLE FVS_DM_Spp_Sum ('//&
             &'CaseID Text,'//&
             &'StandID Text,'//&
             &'Year Int,'//&
             &'Spp Text,'//&
             &'Mean_DMR Number,'//&
             &'Mean_DMI Number,'//&
             &'Inf_TPA Int,'//&
             &'Mort_TPA Int,'//&
             &'Inf_TPA_Pct Int,'//&
             &'Mort_TPA_Pct Int,'//&
             &'Stnd_TPA_Pct Int)'
     else
        SQLStmtStr = 'CREATE TABLE FVS_DM_Spp_Sum ('//&
             &'CaseID char(36) not null,'//&
             &'StandID char(26) not null,'//&
             &'Year Int null,'//&
             &'Spp char(2) null,'//&
             &'Mean_DMR real null,'//&
             &'Mean_DMI real null,'//&
             &'Inf_TPA int null,'//&
             &'Mort_TPA int  null,'//&
             &'Inf_TPA_Pct int null,'//&
             &'Mort_TPA_Pct int null,'//&
             &'Stnd_TPA_Pct int null)'
     end if

     !       CLOSE CURSOR

     iRet = fvsSQLCloseCursor(StmtHndlOut)

     iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),&
          &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

     call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
          &'DBSMIS1:Creating Table: '//trim(SQLStmtStr))
  end if

  !     LOOP OVER 4 TOP SPECIES

  do I = 1,4

     !       IF THERE ARE LESS THAN 4 SPECIES INFECTED IN THE STAND,
     !       DO NOT WRITE THE BLANK RECORDS TO THE DATABASE.

     if (CSP(I) .eq. '**') goto 100

     !       DOUBLE PRECISION COPIES OF SINGLE PRECISION INPUTS AND
     !       INTEGER COPIES OF REAL VECTOR INPUTS

     SPDMR4B = SPDMR4(I)
     SPDMI4B = SPDMI4(I)

     ISPINF4 = NINT(SPINF4(I))
     ISPMRT4 = NINT(SPMRT4(I))
     ISPPIN4 = NINT(SPPIN4(I))
     ISPPMR4 = NINT(SPPMR4(I))
     ISPPOC4 = NINT(SPPOC4(I))

     write(SQLStmtStr,*) 'INSERT INTO ',TRIM(TABLENAME),&
          &' (CaseID,StandID,Year,Spp,Mean_DMR,Mean_DMI,Inf_TPA,',&
          &'Mort_TPA,Inf_TPA_Pct,Mort_TPA_Pct,Stnd_TPA_Pct) ',&
          &' VALUES (''',CASEID,''',''',TRIM(NPLT),&
          &''',?,''',TRIM(CSP(I)),''',?,?,?,?,?,?,?)'

     iRet = fvsSQLCloseCursor(StmtHndlOut)

     iRet = fvsSQLPrepare(StmtHndlOut, SQLStmtStr,&
          &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

     !       BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

     ColNumber = 1 ! 1 YEAR
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
          &SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
          &INT(0,SQLSMALLINT_KIND),IYEAR,int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1 ! MEAN DMR
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
          &SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
          &INT(5,SQLSMALLINT_KIND),SPDMR4B,int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1 ! MEAN DMI
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
          &SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
          &INT(5,SQLSMALLINT_KIND),SPDMI4B,int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1 ! TPA INFECTED
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
          &SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
          &INT(0,SQLSMALLINT_KIND),ISPINF4,int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1 ! TPA MORTALITY
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
          &SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
          &INT(0,SQLSMALLINT_KIND),ISPMRT4,int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1 ! % TPA INFECTED
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
          &SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
          &INT(0,SQLSMALLINT_KIND),ISPPIN4,int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1 ! % TPA MORTALITY
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
          &SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
          &INT(0,SQLSMALLINT_KIND),ISPPMR4,int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1 ! % STAND TPA
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
          &SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
          &INT(0,SQLSMALLINT_KIND),ISPPOC4,int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     !       CLOSE CURSOR

     iRet = fvsSQLCloseCursor(StmtHndlOut)

     iRet = fvsSQLExecute(StmtHndlOut)
     call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
          &'DBSMIS1:Inserting Row')

  end do

  !     RELEASE STATEMENT HANDLE

  100 continue
  iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT,StmtHndlOut)

  return
end

subroutine DBSMIS2(IYEAR,NPLT,NAGE,&
     &ISTTPAT,IBA,ISTVOL,ISTTPAI,ISTBAI,ISTVOLI,ISTTPAM,ISTBAM,&
     &ISTVOLM,ISTPIT,ISTPIV,ISTPMT,ISTPMV,STDMR,STDMI,KODE)
  implicit none

  !     PURPOSE: TO POPULATE A DATABASE WITH THE 2ND MISTLETOE REPORT INFORMATION
  !     AUTH: D. ROBINSON, ESSA - BASED ON D. GAMMEL (DBSFUELS)

  !     1:     Year - IYEAR
  !     2:     Age - NAGE
  !     3:     Stand trees/acre - ISTTPAT
  !     4:     Stand basal area - IBA
  !     5:     Stand volume - ISTVOL
  !     6:     Infected trees/acre - ISTTPAI
  !     7:     Infected basal area - ISTBAI
  !     8:     Infected volume - ISTVOLI
  !     9:     Mortality trees/acre - ISTTPAM
  !     10:    Mortality basal area - ISTBAM
  !     11:    Mortality volume - ISTVOLM
  !     12:    Infected trees/acre % - ISTPIT
  !     13:    Infected volume % - ISTPIV
  !     14:    Mortality trees/acre % - ISTPMT
  !     15:    Mortality volume % - ISTPMV
  !     16:    Mean DMR - STDMR
  !     17:    Mean DMI - STDMI

  ! Common blocks
  include 'DBSCOM.f90'

  !     ARGUMENT LIST

  ! Variables
  integer :: IYEAR,NAGE
  character(LEN = 26) :: NPLT
  integer :: ISTTPAT,IBA,ISTVOL,ISTTPAI,ISTBAI,ISTVOLI,ISTTPAM,ISTBAM
  integer :: ISTVOLM,ISTPIT,ISTPIV,ISTPMT,ISTPMV
  real    :: STDMR,STDMI
  integer :: KODE,IRCODE

  !     LOCAL VARIABLES

  integer(SQLSMALLINT_KIND)::ColNumber

  double precision :: STDMRB,STDMIB

  character(len=1000) :: SQLStmtStr
  character(LEN = 20) :: TABLENAME

  !     INITIALIZE VARIABLES

  if (IDM2.eq.0) return
  if (IDM2.eq.2) KODE = 0

  !     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

  call DBSCASE(1)

  !     ALLOCATE A STATEMENT HANDLE

  iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut,StmtHndlOut)
  if (iRet.ne.SQL_SUCCESS .and. iRet.ne. SQL_SUCCESS_WITH_INFO) then
     IDM2 = 0
     print *,'Error connecting to data source'
     call  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,&
          &'DBSMIS2:DSN Connection')
     goto 100
  end if

  !     CHECK TO SEE IF THE DM TABLE EXISTS IN THE DATBASE

  if (TRIM(DBMSOUT).eq."EXCEL") then
     TABLENAME = '[FVS_DM_Stnd_Sum$]'
  else if (TRIM(DBMSOUT).eq."ORACLE") then
     TABLENAME = '"FVS_DM_Stnd_Sum"'
  else
     TABLENAME = 'FVS_DM_Stnd_Sum'
  end if
  call DBSCKNROWS(IRCODE,TABLENAME,1,TRIM(DBMSOUT).eq.'EXCEL')
  if (IRCODE.eq.2) then
     IDM2 = 0
     return
  end if
  if (IRCODE.eq.1) then
     if (TRIM(DBMSOUT).eq."ACCESS") then
        SQLStmtStr = 'CREATE TABLE FVS_DM_Stnd_Sum ('//&
             &'CaseID text not null,'//&
             &'StandID text null,'//&
             &'Year int null,'//&
             &'Age int null,'//&
             &'Stnd_TPA int null,'//&
             &'Stnd_BA int null,'//&
             &'Stnd_Vol int null,'//&
             &'Inf_TPA int null,'//&
             &'Inf_BA int null,'//&
             &'Inf_Vol int null,'//&
             &'Mort_TPA int null,'//&
             &'Mort_BA int null,'//&
             &'Mort_Vol int null,'//&
             &'Inf_TPA_Pct int null,'//&
             &'Inf_Vol_Pct int null,'//&
             &'Mort_TPA_Pct int null,'//&
             &'Mort_Vol_Pct int null,'//&
             &'Mean_DMR double null,'//&
             &'Mean_DMI double null)'
     else if (TRIM(DBMSOUT).eq."EXCEL") then
        SQLStmtStr = 'CREATE TABLE FVS_DM_Stnd_Sum ('//&
             &'CaseID Text,'//&
             &'StandID Text,'//&
             &'Year Int,'//&
             &'Age Int,'//&
             &'Stnd_TPA Int,'//&
             &'Stnd_BA Int,'//&
             &'Stnd_Vol Int,'//&
             &'Inf_TPA Int,'//&
             &'Inf_BA Int,'//&
             &'Inf_Vol Int,'//&
             &'Mort_TPA Int,'//&
             &'Mort_BA Int,'//&
             &'Mort_Vol Int,'//&
             &'Inf_TPA_Pct Int,'//&
             &'Inf_Vol_Pct Int,'//&
             &'Mort_TPA_Pct Int,'//&
             &'Mort_Vol_Pct Int,'//&
             &'Mean_DMR Number,'//&
             &'Mean_DMI Number)'
     else
        SQLStmtStr = 'CREATE TABLE FVS_DM_Stnd_Sum ('//&
             &'CaseID char(36) not null,'//&
             &'StandID char(26) not null,'//&
             &'Year int null,'//&
             &'Age int null,'//&
             &'Stnd_TPA int null,'//&
             &'Stnd_BA int null,'//&
             &'Stnd_Vol int null,'//&
             &'Inf_TPA int null,'//&
             &'Inf_BA int null,'//&
             &'Inf_Vol int null,'//&
             &'Mort_TPA int null,'//&
             &'Mort_BA int null,'//&
             &'Mort_Vol int null,'//&
             &'Inf_TPA_Pct int null,'//&
             &'Inf_Vol_Pct int null,'//&
             &'Mort_TPA_Pct int null,'//&
             &'Mort_Vol_Pct int null,'//&
             &'Mean_DMR real null,'//&
             &'Mean_DMI real null)'
     end if

     !       CLOSE CURSOR

     iRet = fvsSQLCloseCursor(StmtHndlOut)
     iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),&
          &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
     call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
          &'DBSMIS2:Creating Table: '//trim(SQLStmtStr))
  end if

  !     DOUBLE PRECISION COPIES OF SINGLE PRECISION INPUTS

  STDMRB = STDMR
  STDMIB = STDMI

  write(SQLStmtStr,*) 'INSERT INTO ',TRIM(TABLENAME),' (CaseID,',&
       &'StandID,Year,Age,Stnd_TPA,Stnd_BA,Stnd_Vol,Inf_TPA,Inf_BA,',&
       &'Inf_Vol,Mort_TPA,Mort_BA,Mort_Vol,Inf_TPA_Pct,Inf_Vol_Pct,',&
       &'Mort_TPA_Pct,Mort_Vol_Pct,Mean_DMR,Mean_DMI)',&
       &'VALUES (''',CASEID,''',''',TRIM(NPLT),&
       &''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'

  iRet = fvsSQLCloseCursor(StmtHndlOut)
  iRet = fvsSQLPrepare(StmtHndlOut, trim(SQLStmtStr),&
       &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

  !     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

  ColNumber = 1 ! YEAR
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),IYEAR,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1 ! AGE
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),NAGE,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1 ! STAND TPA
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),ISTTPAT,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1 ! STAND BA
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),IBA,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1 ! STAND VOL
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),ISTVOL,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1 ! INF TPA
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),ISTTPAI,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1 ! INF BA
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),ISTBAI,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1 ! INF VOL
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),ISTVOLI,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1 ! MORT TPA
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),ISTTPAM,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1 ! MORT BA
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),ISTBAM,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1 ! MORT VOL
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),ISTVOLM,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1 ! INF TPA %
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),ISTPIT,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1 ! INF VOL %
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),ISTPIV,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1 ! 16 MORT TPA %
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),ISTPMT,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1 ! MORT VOL %
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),ISTPMV,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1 ! MEAN DMR
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),STDMRB,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1 ! MEAN DMI
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE,SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),STDMIB,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  !     CLOSE CURSOR

  iRet = fvsSQLCloseCursor(StmtHndlOut)
  iRet = fvsSQLExecute(StmtHndlOut)
  call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
       &'DBSMIS2:Inserting Row')

  !     RELEASE STATEMENT HANDLE

  100 iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT,StmtHndlOut)

  return
end

subroutine DBSMIS3(IYEAR,NPLT,NLABS,&
     &DCTPA,DCINF,DCMRT,DCDMR,DCDMI,KODE)
  implicit none

  !     PURPOSE: TO POPULATE A DATABASE WITH THE 3RD MISTLETOE REPORT INFORMATION
  !     AUTH: D. ROBINSON, ESSA - BASED ON D. GAMMEL (DBSFUELS)

  !     1:     Year - IYEAR
  !     2:     Age - NAGE
  !     3:     Stand trees/acre - ISTTPAT
  !     4:     Stand basal area - IBA
  !     5:     Stand volume - ISTVOL
  !     6:     Infected trees/acre - ISTTPAI
  !     7:     Infected basal area - ISTBAI
  !     8:     Infected volume - ISTVOLI
  !     9:     Mortality trees/acre - ISTTPAM
  !     10:    Mortality basal area - ISTBAM
  !     11:    Mortality volume - ISTVOLM
  !     12:    Infected trees/acre % - ISTPIT
  !     13:    Infected volume % - ISTPIV
  !     14:    Mortality trees/acre % - ISTPMT
  !     15:    Mortality volume % - ISTPMV
  !     16:    Mean DMR - STDMR
  !     17:    Mean DMI - STDMI

  ! Common blocks
  include 'DBSCOM.f90'

  !     ARGUMENT LIST

  ! Variables
  integer :: IYEAR
  character(LEN = 26) :: NPLT
  character(LEN = 3) :: NLABS(5)
  real    :: DCTPA(10),DCINF(10),DCMRT(10),DCDMR(10),DCDMI(10)
  integer :: KODE,IRCODE

  !     LOCAL VARIABLES

  integer(SQLSMALLINT_KIND)::ColNumber
  integer :: I,J

  double precision :: X(10)

  character(len=1000) :: SQLStmtStr
  character(LEN = 20) :: TABLENAME

  !     INITIALIZE VARIABLES

  if (IDM3.eq.0) return
  if (IDM3.eq.2) KODE = 0

  !     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

  call DBSCASE(1)

  !     ALLOCATE A STATEMENT HANDLE

  iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut,StmtHndlOut)
  if (iRet.ne.SQL_SUCCESS .and. iRet.ne. SQL_SUCCESS_WITH_INFO) then
     IDM3 = 0
     print *,'Error connecting to data source'
     call  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,&
          &'DBSMIS3:DSN Connection')
     goto 100
  end if

  !     CHECK TO SEE IF THE DM TABLE EXISTS IN THE DATBASE

  if (TRIM(DBMSOUT).eq."EXCEL") then
     TABLENAME = '[FVS_DM_Sz_Sum$]'
  else if (TRIM(DBMSOUT).eq."ORACLE") then
     TABLENAME = '"FVS_DM_Sz_Sum"'
  else
     TABLENAME = 'FVS_DM_Sz_Sum'
  end if
  call DBSCKNROWS(IRCODE,TABLENAME,1,TRIM(DBMSOUT).eq.'EXCEL')
  if (IRCODE.eq.2) then
     IDM3 = 0
     return
  end if
  if (IRCODE.eq.1) then
     if (TRIM(DBMSOUT).eq."ACCESS") then
        SQLStmtStr = 'CREATE TABLE FVS_DM_Sz_Sum ('//&
             &'CaseID text not null,'//&
             &'StandID text null,'//&
             &'Year int null,'//&
             &'Type text null,'//&
             &char(39)//'0-3in'  //char(39)//' double null,'//&
             &char(39)//'3-5in' //char(39)//' double null,'//&
             &char(39)//'5-7in'//char(39)//' double null,'//&
             &char(39)//'7-9in'//char(39)//' double null,'//&
             &char(39)//'9-11in'//char(39)//' double null,'//&
             &char(39)//'11-13in'//char(39)//' double null,'//&
             &char(39)//'13-15in'//char(39)//' double null,'//&
             &char(39)//'15-17in'//char(39)//' double null,'//&
             &char(39)//'17-19in'//char(39)//' double null,'//&
             &char(39)//'gt19in' //char(39)//' double null)'

     else if (TRIM(DBMSOUT).eq."EXCEL") then
        SQLStmtStr = 'CREATE TABLE FVS_DM_Sz_Sum ('//&
             &'CaseID Text,'//&
             &'StandID Text,'//&
             &'Year Int,'//&
             &'Type Text,'//&
             &char(39)//'0-3in'   //char(39)//' Number,'//&
             &char(39)//'3-5in'  //char(39)//' Number,'//&
             &char(39)//'5-7in' //char(39)//' Number,'//&
             &char(39)//'7-9in' //char(39)//' Number,'//&
             &char(39)//'9-11in' //char(39)//' Number,'//&
             &char(39)//'11-13in' //char(39)//' Number,'//&
             &char(39)//'13-15in' //char(39)//' Number,'//&
             &char(39)//'15-17in' //char(39)//' Number,'//&
             &char(39)//'17-19in' //char(39)//' Number,'//&
             &char(39)//'gt19in'  //char(39)//' Number)'
     else
        SQLStmtStr = 'CREATE TABLE FVS_DM_Sz_Sum ('//&
             &'CaseID char(36) not null,'//&
             &'StandID char(26) not null,'//&
             &'Year int null,'//&
             &'Type char(3) not null,'//&
             &char(39)//'0-3in'   //char(39)//' real null,'//&
             &char(39)//'3-5in'  //char(39)//' real null,'//&
             &char(39)//'5-7in' //char(39)//' real null,'//&
             &char(39)//'7-9in' //char(39)//' real null,'//&
             &char(39)//'9-11in' //char(39)//' real null,'//&
             &char(39)//'11-13in' //char(39)//' real null,'//&
             &char(39)//'13-15in' //char(39)//' real null,'//&
             &char(39)//'15-17in' //char(39)//' real null,'//&
             &char(39)//'17-19in' //char(39)//' real null,'//&
             &char(39)//'gt19in'  //char(39)//' real null)'
     end if

     !       CLOSE CURSOR

     iRet = fvsSQLCloseCursor(StmtHndlOut)
     iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),&
          &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
     call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
          &'DBSMIS3:Creating Table: '//trim(SQLStmtStr))
  end if

  !     LOOP OVER 5 TYPES

  do I = 1,5

     !     LOOP OVER 5 TYPES, MAKING DOUBLE PRECISION COPIES OF SINGLE PRECISION INPUTS

     select case (I)
     case (1)
        do J = 1,10
           X(J) = DCTPA(J)
        end do
     case (2)
        do J = 1,10
           X(J) = DCINF(J)
        end do
     case (3)
        do J = 1,10
           X(J) = DCMRT(J)
        end do
     case (4)
        do J = 1,10
           X(J) = DCDMR(J)
        end do
     case (5)
        do J = 1,10
           X(J) = DCDMI(J)
        end do
     end select

     write(SQLStmtStr,*) 'INSERT INTO ',TRIM(TABLENAME),&
          &' (CaseID,StandID,Year,Type,',&
          &char(39),'0-3in',  char(39),',',&
          &char(39),'3-5in', char(39),',',&
          &char(39),'5-7in',char(39),',',&
          &char(39),'7-9in',char(39),',',&
          &char(39),'9-11in',char(39),',',&
          &char(39),'11-13in',char(39),',',&
          &char(39),'13-15in',char(39),',',&
          &char(39),'15-17in',char(39),',',&
          &char(39),'17-19in',char(39),',',&
          &char(39),'gt19in', char(39),') VALUES ',&
          &'(''',CASEID,''',''',TRIM(NPLT),&
          &''',?,''',TRIM(NLABS(I)),&
          &''',?,?,?,?,?,?,?,?,?,?)'

     !       CLOSE CURSOR

     iRet = fvsSQLCloseCursor(StmtHndlOut)

     !       PREPARE QUERY

     iRet = fvsSQLPrepare(StmtHndlOut, trim(SQLStmtStr),&
          &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

     !       BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

     ColNumber = 1 ! 1 YEAR
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
          &SQL_F_INTEGER,SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
          &INT(0,SQLSMALLINT_KIND),IYEAR,int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     do J = 1,10
        ColNumber = ColNumber+1 ! SIZE CLASSES 2-11
        iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
             &SQL_PARAM_INPUT,SQL_F_DOUBLE,SQL_DOUBLE,&
             &INT(15,SQLUINTEGER_KIND),INT(5,SQLSMALLINT_KIND),&
             &X(J),int(4,SQLLEN_KIND),&
             &SQL_NULL_PTR)
     end do

     !       CLOSE CURSOR

     iRet = fvsSQLCloseCursor(StmtHndlOut)
     iRet = fvsSQLExecute(StmtHndlOut)
     call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
          &'DBSMIS3:Inserting Row')

  end do

  !     RELEASE STATEMENT HANDLE

  100 iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT,StmtHndlOut)

  return
end
