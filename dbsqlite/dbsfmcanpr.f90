!===========================!
!   dbsqlite : dbsfmcanpr   !
!===========================!
! DBSQLITE $Id$
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
  integer :: IYEAR,iRet,I,ColNumber
  real    :: CRFILL
  double precision :: CRFILLB,CRFILLKG,HTFT,HTM
  dimension CRFILL(400)
  character(len=2000) :: SQLStmtStr
  character(len=26) :: NPLT

  integer :: fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_prepare,fsql3_bind_double,&
       &fsql3_finalize,fsql3_step,fsql3_reset

  if (ICANPR.eq.0) return

  iRet = 0
  do I = 1,400
     if (CRFILL(I).ge.0) then
        iRet = 1
        exit
     end if
  end do
  if (iRet.eq.0) return

  call DBSCASE(1)
  iRet = fsql3_exec (IoutDBref,"Begin;"//Char(0))
  iRet = fsql3_tableexists(IoutDBref,&
       &"FVS_CanProfile"//CHAR(0))
  if (iRet.eq.0) then
     SQLStmtStr = 'CREATE TABLE FVS_CanProfile ('//&
          &'CaseID text not null,'//&
          &'StandID text not null,'//&
          &'Year Int null,'//&
          &'Height_m real null,'//&
          &'Canopy_Fuel_kg_m3 real null,'//&
          &'Height_ft real null,'//&
          &'Canopy_Fuel_lbs_acre_ft real null);'//CHAR(0)
     iRet = fsql3_exec(IoutDBref,SQLStmtStr)
     if (iRet .ne.0) then
        ICANPR = 0
        return
     end if
  end if

  write(SQLStmtStr,*)'INSERT INTO FVS_CanProfile (CaseID,',&
       &'StandID,Year,Height_m,Canopy_Fuel_kg_m3,Height_ft,',&
       &'Canopy_Fuel_lbs_acre_ft) VALUES (''',CASEID,&
       &''',''',TRIM(NPLT),''',?,?,?,?,?);'
  iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
  if (iRet .ne.0) then
     ICANPR = 0
     return
  end if

  do I = 1,400
     if (CRFILL(I).le.0) cycle

     !       ASSIGN VALUES TO DOUBLE PRECISION VARS

     CRFILLB = CRFILL(I)
     CRFILLKG = CRFILL(I)*0.45359237 / (4046.856422 * 0.3048)
     HTFT = I
     HTM = I*0.3048

     ColNumber = 1
     iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)
     ColNumber = ColNumber+1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,HTM)
     ColNumber = ColNumber+1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,CRFILLKG)
     ColNumber = ColNumber+1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,HTFT)
     ColNumber = ColNumber+1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,CRFILLB)

     iRet = fsql3_step(IoutDBref)
     iRet = fsql3_reset(IoutDBref)
  end do
  iRet = fsql3_finalize(IoutDBref)
  if (iRet.ne.0) then
     ICANPR = 0
  end if
  iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))
  return
end
