!==========================!
!   dbsqlite : dbsfmhrpt   !
!==========================!
! DBSQLITE $Id$
subroutine DBSFMHRPT(IYEAR,NPLT,VAR,VARDIM,KODE)
  implicit none

  !     PURPOSE: TO POPULATE A DATABASE WITH THE CARBON HARVEST
  !              PRODUCTS REPORT INFORMATION
  !     AUTH: D. ROBINSON - ESSA
  !     INPUT:
  !       IYEAR  - CALENDAR YEAR
  !       NPLT   - CASE NUMBER
  !       VAR    - ARRAY WITH VARIABLES TO REPORT
  !       VARDIM - LENGTH OF VAR ARRAY
  !         1 = PRODUCTS
  !         2 = LANDFILL
  !         3 = ENERGY
  !         4 = EMISSIONS
  !         5 = MERCH CARBON STORED
  !         6 = MERCH CARBON REMOVED

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
  integer :: IYEAR,KODE,VARDIM,iRet
  character(len=26) :: NPLT
  real    :: VAR
  dimension VAR(VARDIM)

  double precision :: VARD(VARDIM)
  integer :: I,ColNumber
  character(len=2000) :: SQLStmtStr

  integer :: fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,fsql3_prepare,fsql3_bind_double,&
       &fsql3_finalize

  if (ICHRPT.eq.0) return
  if (ICHRPT.eq.2) KODE = 0

  !     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

  call DBSCASE(1)
  iRet = fsql3_exec (IoutDBref,"Begin;"//Char(0))
  iRet = fsql3_tableexists(IoutDBref,&
       &"FVS_Hrv_Carbon"//CHAR(0))
  if (iRet.eq.0) then
     SQLStmtStr = 'CREATE TABLE FVS_Hrv_Carbon('//&
          &'CaseID text not null,'//&
          &'StandID text not null,'//&
          &'Year int null,' //&
          &'Products real null,' //&
          &'Landfill real null,' //&
          &'Energy real null,' //&
          &'Emissions real null,' //&
          &'Merch_Carbon_Stored real null,' //&
          &'Merch_Carbon_Removed real null);'//CHAR(0)
     iRet = fsql3_exec(IoutDBref,SQLStmtStr)
     if (iRet .ne.0) then
        ICHRPT = 0
        return
     end if
  end if

  VARD = VAR

  write(SQLStmtStr,*)'INSERT INTO FVS_Hrv_Carbon (CaseID,',&
       &'StandID,Year,Products,Landfill,Energy,Emissions,',&
       &'Merch_Carbon_Stored,Merch_Carbon_Removed) VALUES(''',&
       &CASEID,''',''',TRIM(NPLT),''',?,?,?,?,?,?,?)'
  iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
  if (iRet .ne.0) then
     ICHRPT = 0
     return
  end if

  ColNumber = 1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

  do I = 1,VARDIM
     ColNumber = ColNumber+1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,VARD(I))
  end do
  iRet = fsql3_step(IoutDBref)
  iRet = fsql3_finalize(IoutDBref)
  if (iRet.ne.0) then
     ICHRPT = 0
  end if
  iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))
  return
end
