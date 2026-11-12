!===========================!
!   dbsqlite : dbsfmdwcov   !
!===========================!
! DBSQLITE $Id$
subroutine DBSFMDWCOV(IYEAR,NPLT,VAR,VARDIM,KODE)
  implicit none
  include 'PRGPRM.f90'

  !     PURPOSE: TO POPULATE A DATABASE WITH THE DOWN WOOD COVER REPORT
  !              INFORMATION
  !     AUTH: S. REBAIN
  !     INPUT:
  !       IYEAR  - CALENDAR YEAR
  !       NPLT   - CASE NUMBER
  !       VAR    - ARRAY WITH VARIABLES TO REPORT
  !       VARDIM - LENGTH OF VAR ARRAY
  !       UNITS ARE % COVER
  !         1 = DOWN WOOD 3 - 6 HARD
  !         2 = DOWN WOOD 6 - 12 HARD
  !         3 = DOWN WOOD 12 - 20 HARD
  !         4 = DOWN WOOD 20 - 35 HARD
  !         5 = DOWN WOOD 35 - 50 HARD
  !         6 = DOWN WOOD 50+ HARD
  !         7 = DOWN WOOD TOTAL HARD
  !         8 = DOWN WOOD 3 - 6 SOFT
  !         9 = DOWN WOOD 6 - 12 SOFT
  !         10 = DOWN WOOD 12 - 20 SOFT
  !         11 = DOWN WOOD 20 - 35 SOFT
  !         12 = DOWN WOOD 35 - 50 SOFT
  !         13 = DOWN WOOD 50+ SOFT
  !         14 = DOWN WOOD TOTAL SOFT
  !       KODE   - RETURN CODE

  ! Common blocks
  include 'DBSCOM.f90'

  ! Variables
  integer :: IYEAR,KODE,VARDIM,iRet
  character(len=26) :: NPLT
  real    :: VAR
  dimension VAR(VARDIM)

  double precision :: VARD(VARDIM)
  integer :: I,ColNumber
  character(len=2000) :: SQLStmtStr

  integer :: fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,fsql3_prepare,fsql3_bind_double,&
       &fsql3_finalize

  if (IDWDCOV.eq.0) return
  if (IDWDCOV.eq.2) KODE = 0

  !     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

  call DBSCASE(1)
  iRet = fsql3_exec (IoutDBref,"Begin;"//Char(0))
  if (LMETRICVAR) then
     iRet = fsql3_tableexists(IoutDBref,&
          &"FVS_Down_Wood_Cov_Metric"//CHAR(0))
  else
     iRet = fsql3_tableexists(IoutDBref,&
          &"FVS_Down_Wood_Cov"//CHAR(0))
  end if
  if (iRet.eq.0) then
     if (LMETRICVAR) then
        SQLStmtStr = 'CREATE TABLE FVS_Down_Wood_Cov_Metric ('//&
             &'CaseID text not null,'//&
             &'StandID text not null,'//&
             &'Year Int null,'//&
             &'DWD_Cover_76to152_Hard real null,'//&
             &'DWD_Cover_152to305_Hard real null,'//&
             &'DWD_Cover_305to508_Hard real null,'//&
             &'DWD_Cover_508to889_Hard real null,'//&
             &'DWD_Cover_889to1270_Hard real null,'//&
             &'DWD_Cover_ge_1270_Hard real null,'//&
             &'DWD_Cover_Total_Hard real null,'//&
             &'DWD_Cover_76to152_Soft real null,'//&
             &'DWD_Cover_152to305_Soft real null,'//&
             &'DWD_Cover_305to508_Soft real null,'//&
             &'DWD_Cover_508to889_Soft real null,'//&
             &'DWD_Cover_889to1270_Soft real null,'//&
             &'DWD_Cover_ge_1270_Soft real null,'//&
             &'DWD_Cover_Total_Soft real null);'//CHAR(0)
     else
        SQLStmtStr = 'CREATE TABLE FVS_Down_Wood_Cov ('//&
             &'CaseID text not null,'//&
             &'StandID text not null,'//&
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
             &'DWD_Cover_Total_Soft real null);'//CHAR(0)
     end if
     iRet = fsql3_exec(IoutDBref,SQLStmtStr)
     if (iRet .ne.0) then
        IDWDCOV = 0
        return
     end if
  end if

  VARD = VAR

  if (LMETRICVAR) then
     write(SQLStmtStr,*)'INSERT INTO FVS_Down_Wood_Cov_Metric',&
          &' (CaseID,StandID,Year,DWD_Cover_76to152_Hard,',&
          &'DWD_Cover_152to305_Hard,DWD_Cover_305to508_Hard,',&
          &'DWD_Cover_508to889_Hard,DWD_Cover_889to1270_Hard,',&
          &'DWD_Cover_ge_1270_Hard,DWD_Cover_Total_Hard,',&
          &'DWD_Cover_76to152_Soft,',&
          &'DWD_Cover_152to305_Soft,DWD_Cover_305to508_Soft,',&
          &'DWD_Cover_508to889_Soft,DWD_Cover_889to1270_Soft,',&
          &'DWD_Cover_ge_1270_Soft,DWD_Cover_Total_Soft) VALUES (''',&
          &CASEID,''',''',TRIM(NPLT),&
          &''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'//CHAR(0)
  else
     write(SQLStmtStr,*)'INSERT INTO FVS_Down_Wood_Cov',&
          &' (CaseID,StandID,Year,DWD_Cover_3to6_Hard,',&
          &'DWD_Cover_6to12_Hard,DWD_Cover_12to20_Hard,',&
          &'DWD_Cover_20to35_Hard,DWD_Cover_35to50_Hard,',&
          &'DWD_Cover_ge_50_Hard,DWD_Cover_Total_Hard,',&
          &'DWD_Cover_3to6_Soft,',&
          &'DWD_Cover_6to12_Soft,DWD_Cover_12to20_Soft,',&
          &'DWD_Cover_20to35_Soft,DWD_Cover_35to50_Soft,',&
          &'DWD_Cover_ge_50_Soft,DWD_Cover_Total_Soft) VALUES (''',&
          &CASEID,''',''',TRIM(NPLT),&
          &''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'//CHAR(0)
  end if

  iRet = fsql3_prepare(IoutDBref,SQLStmtStr)

  !     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

  ColNumber = 1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

  do I = 1,VARDIM
     ColNumber = ColNumber+1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,VARD(I))
  end do

  iRet = fsql3_step(IoutDBref)
  iRet = fsql3_finalize(IoutDBref)
  if (iRet.ne.0) then
     IDWDCOV = 0
  end if
  iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))
  return
end
