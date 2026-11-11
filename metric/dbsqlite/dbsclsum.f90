!================================!
!   metric/dbsqlite : dbsclsum   !
!================================!
! METRIC-DBSQLITE $Id$
subroutine DBSCLSUM(CNPLT,IYR,SP,SPVIAB,&
     &SPBA,SPTPA,SPMORT1,SPMORT2,SPGMULT,&
     &SPSITGM,MXDENMLT,POTESTAB)
  implicit none

  !     POPULATE A DATABASE WITH THE CLIMATE SUMMARY

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
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
  integer :: I,IYR,IRCODE
  character(LEN = *) :: CNPLT
  character(LEN = *) :: SP
  real    :: SPVIAB,SPBA,SPTPA,SPMORT1,SPMORT2,SPGMULT,SPSITGM,MXDENMLT,POTESTAB

  character(len=8) :: CSP1
  character(len=8) :: CSP2
  character(len=8) :: CSP3
  character(len=2000) :: SQLStmtStr

  integer :: fsql3_tableexists,fsql3_exec
  if (ICLIM.eq.0) return

  !     MAKE SURE WE HAVE AN UP TO DATE CASEID

  call DBSCASE(1)

  IRCODE = fsql3_tableexists(IoutDBref,&
       &"FVS_Climate_Metric"//CHAR(0))
  if (IRCODE.eq.0) then
     SQLStmtStr = 'CREATE TABLE FVS_Climate_Metric('//&
          &'CaseID text not null,'//&
          &'StandID text not null,'//&
          &'Year Int null,'//&
          &'SpeciesFVS    text null,'//&
          &'SpeciesPLANTS text null,'//&
          &'SpeciesFIA    text null,'//&
          &'Viability real null,'//&
          &'BA real null,'//&
          &'TPH real null,'//&
          &'ViabMort real null,'//&
          &'dClimMort real null,'//&
          &'GrowthMult real null,'//&
          &'SiteMult real null,'//&
          &'MxDenMult real null,'//&
          &'AutoEstbTPH real null)'//CHAR(0)
     IRCODE = fsql3_exec(IoutDBref,SQLStmtStr)
     if (IRCODE .ne.0) then
        ICLIM = 0
        return
     end if
  end if

  !     ASSIGN FVS, PLANTS AND FIA SPECIES CODES

  do I = 1,MAXSP
     if (SP .eq. JSP(I)) then
        CSP1 = JSP(I)
        CSP2 = PLNJSP(I)
        CSP3 = FIAJSP(I)
     end if
  end do

  write(SQLStmtStr,*)'INSERT INTO FVS_Climate (CaseID,'//&
       &'StandID,Year,SpeciesFVS,SpeciesPLANTS,SpeciesFIA,'//&
       &'Viability,BA,TPH,ViabMort,'//&
       &'dClimMort,GrowthMult,SiteMult,MxDenMult '//&
       &'VALUES(''',CASEID,''',''',TRIM(CNPLT),''',',IYR,',',&
       &'''',TRIM(CSP1),''',',&
       &'''',TRIM(CSP2),''',',&
       &'''',TRIM(CSP3),''',',&
       &SPVIAB,',',SPBA,',',SPTPA,',',&
       &SPMORT1,',',SPMORT2,',',SPGMULT,',',&
       &SPSITGM,',',MXDENMLT,',',POTESTAB,')'

  IRCODE = fsql3_exec(IoutDBref,trim(SQLStmtStr)//CHAR(0))
  if (IRCODE .ne.0) ICLIM = 0
  return
end
