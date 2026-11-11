!===========================!
!   dbsqlite : dbsplothab   !
!===========================!
! DBSQLITE $Id: dbsstats.f 2620 2019-03-08 18:22:51Z nickcrookston $
subroutine DBSPLOTHAB(NUMPTS,HABTYP,SERIES,M)
  implicit none

  !     PURPOSE: TO POPULATE A DATABASE WITH THE PROGNOSIS MODEL
  !              PLOT HABITAT TYPE SUMMARY OUTPUT
  !     AUTH: M. SHETTLES -- FMSC -- AUGUST 2019

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'DBSCOM.f90'
  include 'PLOT.f90'

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
  integer :: ColNumber,iret,M,NUMPTS
  character(len=2000) :: SQLStmtStr
  character(len=47) :: HABTYP
  character(len=7) :: SERIES
  !OMMONS END

  integer :: fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,fsql3_prepare,fsql3_finalize,&
       &fsql3_bind_text

  if (IREG3.ne.1) return

  call DBSCASE(1)

  !     DEFINE TABLENAME

  iRet = fsql3_tableexists(IoutDBref,'FVS_Regen_HabType'//CHAR(0))

  !       SITE PREP SUMMARY

  if (iRet.eq.0) then
     SQLStmtStr = 'CREATE TABLE FVS_Regen_HabType('//&
          &'CaseID text not null,'//&
          &'StandID text not null,'//&
          &'Series text null,'//&
          &'GroupNum int null,'//&
          &'HabitatType text null,'//&
          &'NumPlots int null);'//CHAR(0)

     iRet = fsql3_exec(IoutDBref,SQLStmtStr)
     if (iRet .ne.0) then
        IREG3 = 0
        return
     end if
  end if

  write(SQLStmtStr,*)'INSERT INTO FVS_Regen_HabType',&
       &' (CaseID,StandID,Series,GroupNum,HabitatType,',&
       &'NumPlots)',&
       &'VALUES(''',CASEID,''',''',TRIM(NPLT),''',?,?,?,?);'

  iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
  if (iRet .ne.0) then
     IREG3 = 0
     return
  end if

  ColNumber = 1
  iRet = fsql3_bind_text(IoutDBref,ColNumber,SERIES,&
       &LEN_TRIM(SERIES))
  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,M)
  ColNumber = ColNumber+1
  iRet = fsql3_bind_text(IoutDBref,ColNumber,HABTYP,&
       &LEN_TRIM(HABTYP))
  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,NUMPTS)
  iRet = fsql3_step(IoutDBref)
  iRet = fsql3_finalize(IoutDBref)
  if (iRet.ne.0) then
     IREG3 = 0
     return
  end if
end
