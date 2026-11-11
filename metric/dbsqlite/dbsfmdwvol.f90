!==================================!
!   metric/dbsqlite : dbsfmdwvol   !
!==================================!
! METRIC-DBSQLITE $Id$
subroutine DBSFMDWVOL(IYEAR,NPLT,VAR,VARDIM,KODE)
  implicit none

  !     PURPOSE: TO POPULATE A DATABASE WITH THE DOWN WOOD VOLUME REPORT
  !              INFORMATION
  !     AUTH: S. REBAIN
  !     INPUT:
  !       IYEAR  - CALENDAR YEAR
  !       NPLT   - CASE NUMBER
  !       VAR    - ARRAY WITH VARIABLES TO REPORT
  !       VARDIM - LENGTH OF VAR ARRAY

  !       DBH BREAKPOINGS IMMEDIATELY BELOW ARE INCHES; THESE ARE
  !       CONVERTED TO CM IN THE TABLE; e.g. "3to6" becomes "76to152",
  !       DROPPING THE DECIMAL POINT IN THE METRIC VERSION
  !       UNITS ARE CUM/HA

  !         1 = DOWN WOOD 0 - 3 HARD
  !         2 = DOWN WOOD 3 - 6 HARD
  !         3 = DOWN WOOD 6 - 12 HARD
  !         4 = DOWN WOOD 12 - 20 HARD
  !         5 = DOWN WOOD 20 - 35 HARD
  !         6 = DOWN WOOD 35 - 50 HARD
  !         7 = DOWN WOOD 50+ HARD
  !         8 = DOWN WOOD TOTAL HARD
  !         9 = DOWN WOOD 0 - 3 SOFT
  !         10 = DOWN WOOD 3 - 6 SOFT
  !         11 = DOWN WOOD 6 - 12 SOFT
  !         12 = DOWN WOOD 12 - 20 SOFT
  !         13 = DOWN WOOD 20 - 35 SOFT
  !         14 = DOWN WOOD 35 - 50 SOFT
  !         15 = DOWN WOOD 50+ SOFT
  !         16 = DOWN WOOD TOTAL SOFT
  !       KODE   - RETURN CODE

  ! Common blocks
  include 'DBSCOM.f90'

  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_ADDCOLIFABSENT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_DOUBLE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_INT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_TEXT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_CLOSE  
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLCNT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLDOUBLE  
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLINT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLISNULL  
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLNAME  
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLREAL
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLTEXT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLTYPE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_ERRMSG
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_EXEC
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_FINALIZE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_OPEN  
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_PREPARE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_RESET  
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_STEP
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_TABLEEXISTS
#if !(_WIN64)
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_ADDCOLIFABSENT' :: FSQL3_ADDCOLIFABSENT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_BIND_DOUBLE'    :: FSQL3_BIND_DOUBLE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_BIND_INT'       :: FSQL3_BIND_INT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_BIND_TEXT'      :: FSQL3_BIND_TEXT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_CLOSE'          :: FSQL3_CLOSE  
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLCNT'         :: FSQL3_COLCNT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLDOUBLE'      :: FSQL3_COLDOUBLE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLINT'         :: FSQL3_COLINT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLISNULL'      :: FSQL3_COLISNULL  
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLNAME'        :: FSQL3_COLNAME  
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLREAL'        :: FSQL3_COLREAL
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLTEXT'        :: FSQL3_COLTEXT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLTYPE'        :: FSQL3_COLTYPE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_ERRMSG'         :: FSQL3_ERRMSG
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_EXEC'           :: FSQL3_EXEC
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_FINALIZE'       :: FSQL3_FINALIZE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_OPEN'           :: FSQL3_OPEN
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_PREPARE'        :: FSQL3_PREPARE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_RESET'          :: FSQL3_RESET
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_STEP'           :: FSQL3_STEP
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_TABLEEXISTS'    :: FSQL3_TABLEEXISTS 
#endif

  ! Variables
  integer :: IYEAR,KODE,VARDIM
  character(len=26) :: NPLT
  real    :: VAR
  dimension VAR(VARDIM)

  double precision :: VARD(VARDIM)
  integer :: iRet,I,ColNumber
  character(len=2000) :: SQLStmtStr

  integer :: fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,fsql3_prepare,fsql3_bind_double,&
       &fsql3_finalize

  if (IDWDVOL.eq.0) return
  if (IDWDVOL.eq.2) KODE = 0

  call DBSCASE(1)
  iRet = fsql3_exec (IoutDBref,"Begin;"//Char(0))
  !     INSURE THE DOWN WOOD VOLUME TABLE EXISTS IN DATBASE
  iRet = fsql3_tableexists(IoutDBref,&
       &"FVS_Down_Wood_Vol_Metric"//CHAR(0))
  if (iRet.eq.0) then
     SQLStmtStr = 'CREATE TABLE FVS_Down_Wood_Vol_Metric('//&
          &'CaseID text not null,'//&
          &'StandID text not null,'//&
          &'Year Int null,'//&
          &'DWD_Volume_0to76_Hard real null,'//&
          &'DWD_Volume_76to152_Hard real null,'//&
          &'DWD_Volume_152to305_Hard real null,'//&
          &'DWD_Volume_305to508_Hard real null,'//&
          &'DWD_Volume_508to889_Hard real null,'//&
          &'DWD_Volume_889to1270_Hard real null,'//&
          &'DWD_Volume_ge_1270_Hard real null,'//&
          &'DWD_Volume_Total_Hard real null,'//&
          &'DWD_Volume_0to76_Soft real null,'//&
          &'DWD_Volume_76to152_Soft real null,'//&
          &'DWD_Volume_152to305_Soft real null,'//&
          &'DWD_Volume_305to508_Soft real null,'//&
          &'DWD_Volume_508to889_Soft real null,'//&
          &'DWD_Volume_889to1270_Soft real null,'//&
          &'DWD_Volume_ge_1270_Soft real null,'//&
          &'DWD_Volume_Total_Soft real null);'//CHAR(0)
     iRet = fsql3_exec(IoutDBref,SQLStmtStr)
     if (iRet .ne.0) then
        IDWDVOL = 0
        return
     end if
  end if

  VARD = VAR

  write(SQLStmtStr,*)'INSERT INTO FVS_Down_Wood_Vol_Metric(CaseID,',&
       &'StandID,Year,DWD_Volume_0to76_Hard,DWD_Volume_76to152_Hard,',&
       &'DWD_Volume_152to305_Hard,DWD_Volume_305to508_Hard,',&
       &'DWD_Volume_508to889_Hard,DWD_Volume_889to1270_Hard,',&
       &'DWD_Volume_ge_1270_Hard,DWD_Volume_Total_Hard,',&
       &'DWD_Volume_0to76_Soft,DWD_Volume_76to152_Soft,',&
       &'DWD_Volume_152to305_Soft,DWD_Volume_305to508_Soft,',&
       &'DWD_Volume_508to889_Soft,DWD_Volume_889to1270_Soft,',&
       &'DWD_Volume_ge_1270_Soft,DWD_Volume_Total_Soft) VALUES (''',&
       &CASEID,''',''',TRIM(NPLT),&
       &''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'//CHAR(0)

  iRet = fsql3_prepare(IoutDBref, SQLStmtStr)

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
     IDWDVOL = 0
  end if
  iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))
  return
end
