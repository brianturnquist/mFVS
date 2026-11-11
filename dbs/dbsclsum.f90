!====================!
!   dbs : dbsclsum   !
!====================!
! DBS $Id$
subroutine DBSCLSUM(NPLT,IYR,SP,SPVIAB,&
     &SPBA,SPTPA,SPMORT1,SPMORT2,SPGMULT,&
     &SPSITGM,MXDENMLT,POTESTAB)
  implicit none

  !     POPULATE A DATABASE WITH THE CLIMATE SUMMARY

  ! Common blocks
  include 'DBSCOM.f90'

  ! Variables
  integer :: IYR,IRCODE
  character(LEN = *) :: NPLT
  character(LEN = *) :: SP
  real    :: SPVIAB,SPBA,SPTPA,SPMORT1,SPMORT2,SPGMULT,SPSITGM,MXDENMLT,POTESTAB

  character(len=2000) :: SQLStmtStr
  character(len=20) :: TABLENAME

  if (ICLIM.eq.0) return

  !     MAKE SURE WE HAVE AN UP TO DATE CASEID

  call DBSCASE(1)

  !     ALLOCATE A STATEMENT HANDLE

  iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
  if (iRet.ne.SQL_SUCCESS .and. iRet.ne. SQL_SUCCESS_WITH_INFO) then
     print *,'Error connecting to data source'
     call  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,&
          &'DBSCLSUM:DSN Connection')
     ICLIM = 0
     goto 200
  end if

  !     CHECK TO SEE IF THE Climate TABLE EXISTS IN DATBASE

  if (TRIM(DBMSOUT).eq."EXCEL") then
     TABLENAME = '[FVS_Climate$]'
  else
     TABLENAME = 'FVS_Climate'
  end if
  call DBSCKNROWS(IRCODE,TABLENAME,1,TRIM(DBMSOUT).eq.'EXCEL')
  if (IRCODE.eq.2) return
  if (IRCODE.eq.1) then
     if (TRIM(DBMSOUT).eq."ACCESS") then
        SQLStmtStr = 'CREATE TABLE FVS_Climate('//&
             &'CaseID Text not null,'//&
             &'StandID Text null,'//&
             &'Year Int null,'//&
             &'Species Text null,'//&
             &'Viability double null,'//&
             &'BA double null,'//&
             &'TPA double null,'//&
             &'ViabMort double null,'//&
             &'dClimMort double null,'//&
             &'GrowthMult double null,'//&
             &'SiteMult double null,'//&
             &'MxDenMult double null,'//&
             &'AutoEstbTPA double null)'

     else if (TRIM(DBMSOUT).eq."EXCEL") then
        SQLStmtStr = 'CREATE TABLE FVS_Climate('//&
             &'CaseID Text,'//&
             &'StandID Text,'//&
             &'Year Int,'//&
             &'Species Text,'//&
             &'Viability Number,'//&
             &'BA Number,'//&
             &'TPA Number,'//&
             &'ViabMort Number,'//&
             &'dClimMort Number,'//&
             &'GrowthMult Number,'//&
             &'SiteMult Number,'//&
             &'MxDenMult Number,'//&
             &'AutoEstbTPA Number)'
     else
        SQLStmtStr = 'CREATE TABLE FVS_Climate('//&
             &'CaseID char(36) not null,'//&
             &'StandID char(26) not null,'//&
             &'Year Int null,'//&
             &'Species char(3) null,'//&
             &'Viability real null,'//&
             &'BA real null,'//&
             &'TPA real null,'//&
             &'ViabMort real null,'//&
             &'dClimMort real null,'//&
             &'GrowthMult real null,'//&
             &'SiteMult real null,'//&
             &'MxDenMult real null,'//&
             &'AutoEstbTPA real null)'
     end if
     print*, trim(SQLStmtStr)

     !Close Cursor
     iRet = fvsSQLCloseCursor(StmtHndlOut)

     iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),&
          &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
     call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
          &'DBSCLSUM:Creating Table: '//trim(SQLStmtStr))
     if (iRet.ne.SQL_SUCCESS) goto 200
  end if

  write(SQLStmtStr,*)'INSERT INTO ',TABLENAME,' (CaseID,'//&
       &'StandID,Year,Species,Viability,BA,TPA,ViabMort,'//&
       &'dClimMort,GrowthMult,SiteMult,MxDenMult,AutoEstbTPA) '//&
       &'VALUES(''',CASEID,''',''',trim(NPLT),''',',IYR,',''',&
       &trim(SP),''',',SPVIAB,',',SPBA,',',SPTPA,',',&
       &SPMORT1,',',SPMORT2,',',SPGMULT,',',&
       &SPSITGM,',',MXDENMLT,',',POTESTAB,')'

  ! PRINT*, trim(SQLStmtStr)
  iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),&
       &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
  ! PRINT*, iRet
  call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
       &'DBSCLSUM:Inserting Row')

  200 continue
  !Release statement handle
  iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

end
