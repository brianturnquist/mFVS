!===============================!
!   metric/dbsqlite : dbssprt   !
!===============================!
! METRIC-DBSQLITE $Id: dbssprt.f 2620 2019-03-08 18:22:51Z nickcrookston $
subroutine DBSSPRT(CSP,TPASUM,HTAVE,TPATOT,&
     &WTHTAVE,TOT,IYEAR)
  implicit none

  !     PURPOSE: TO POPULATE A DATABASE WITH THE PROGNOSIS MODEL
  !              SPROUTING OUTPUT
  !     AUTH: M. SHETTLES -- FMSC -- AUGUST 2019

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'DBSCOM.f90'
  include 'PLOT.f90'
  include 'METRIC.f90'

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
  integer :: ColNumber,I,iret,TOT,IYEAR,TPASUM1,IYEAR1
  real    :: TPASUM,HTAVE,TPATOT,WTHTAVE
  double precision :: HTAVE1
  character(len=2000) :: SQLStmtStr
  character(len=3) :: CSP
  character(len=8) :: CSP1
  character(len=8) :: CSP2
  character(len=8) :: CSP3

  !OMMONS END

  integer :: fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,fsql3_prepare,fsql3_bind_double,&
       &fsql3_finalize,fsql3_bind_text

  if (IREG1.ne.1) return

  call DBSCASE(1)

  !     DEFINE TABLENAME

  iRet = fsql3_tableexists(IoutDBref,'FVS_Regen_Sprouts_Metric'&
       &//CHAR(0))

  !       REGENERATION FROM STUMP & ROOT SPROUTS

  if (iRet.eq.0) then
     SQLStmtStr = 'CREATE TABLE FVS_Regen_Sprouts_Metric('//&
          &'CaseID text not null,'//&
          &'StandID text not null,'//&
          &'Year int null,'//&
          &'SpeciesFVS    text null,'//&
          &'SpeciesPLANTS text null,'//&
          &'SpeciesFIA    text null,'//&
          &'SprtTph int,'//&
          &'SprtAveHt real);'//CHAR(0)

     iRet = fsql3_exec(IoutDBref,SQLStmtStr)
     if (iRet .ne.0) then
        IREG1 = 0
        return
     end if
  end if

  !     ASSIGN FVS, PLANTS AND FIA SPECIES CODES

  if (CSP .eq. "ALL") then
     CSP1 = "ALL"
     CSP2 = "ALL"
     CSP3 = "ALL"
  else
     do I = 1,MAXSP
        if (CSP(1:2) .eq. JSP(I)) then
           CSP1 = JSP(I)
           CSP2 = PLNJSP(I)
           CSP3 = FIAJSP(I)
        end if
     end do
  end if

  write(SQLStmtStr,*)'INSERT INTO FVS_Regen_Sprouts_Metric',&
       &' (CaseID,StandID,Year,',&
       &'SpeciesFVS,SpeciesPLANTS,SpeciesFIA,SprtTph,SprtAveHt)',&
       &'VALUES(''',CASEID,''',''',TRIM(NPLT),''',?,?,?,?,?,?);'

  iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
  if (iRet .ne.0) then
     IREG1 = 0
     return
  end if

  !     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS

  if (TOT.eq.1) then
     TPASUM1 = NINT(TPASUM)
     HTAVE1 = HTAVE
  else
     TPASUM1 = NINT(TPATOT)
     HTAVE1 = WTHTAVE
  end if
  IYEAR1 = IYEAR+1

  ColNumber = 1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR1)
  ColNumber = ColNumber+1
  iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP1,&
       &LEN_TRIM(CSP1))
  ColNumber = ColNumber+1
  iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP2,&
       &LEN_TRIM(CSP2))
  ColNumber = ColNumber+1
  iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP3,&
       &LEN_TRIM(CSP3))
  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,TPASUM1)
  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,HTAVE1)
  iRet = fsql3_step(IoutDBref)
  iRet = fsql3_finalize(IoutDBref)
  if (iRet.ne.0) then
     IREG1 = 0
  end if
  return
end
