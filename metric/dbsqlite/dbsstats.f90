!================================!
!   metric/dbsqlite : dbsstats   !
!================================!
! METRIC-DBSQLITE $Id: dbsstats.f 2620 2019-03-08 18:22:51Z nickcrookston $
subroutine DBSSTATS(SPECCD,TPA,BAREA,CFVOL,BFVOL,STDIST1,&
     &STDIST2,STDIST3,STDIST4,STDIST5,STDIST6,STDIST7,STDIST8,&
     &STDIST9,LABEL,TBL,IYEAR)
  implicit none

  !     PURPOSE: TO POPULATE A DATABASE WITH THE PROGNOSIS MODEL
  !              CRUISE STATISTICS OUTPUT
  !     AUTH: M. SHETTLES -- FMSC -- JULY 2019

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
  integer :: ColNumber,I,iret1,iret2,TBL,STDIST41,IYEAR,STDIST51
  real    :: STDIST1,STDIST2,STDIST3,STDIST4,STDIST5
  real    :: STDIST6,STDIST7,STDIST8,STDIST9
  real    :: TPA,BAREA,CFVOL,BFVOL
  double precision :: TPA1,BAREA1,CFVOL1,BFVOL1
  double precision :: STDIST11,STDIST21,STDIST31
  double precision :: STDIST61,STDIST71,STDIST81,STDIST91
  character(len=2000) :: SQLStmtStr
  character(len=4) :: SPECCD
  character(len=8) :: CSP1
  character(len=8) :: CSP2
  character(len=8) :: CSP3
  character(len=16) :: LABEL

  !OMMONS END

  integer :: fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,fsql3_prepare,fsql3_bind_double,&
       &fsql3_finalize,fsql3_bind_text

  if (ISTATS1.ne.1) return

  call DBSCASE(1)

  if (TBL.eq.2)goto 110

  !     DEFINE TAABLENAME

  iRet1 = fsql3_tableexists(IoutDBref,'FVS_Stats_Species_Metric'&
       &//CHAR(0))

  !       GENERAL SPECIES SUMMARY FOR THE CRUISE (PER HA)

  if (iRet1.eq.0) then
     SQLStmtStr = 'CREATE TABLE FVS_Stats_Species_Metric('//&
          &'CaseID text not null,'//&
          &'StandID text not null,'//&
          &'Year int null,'//&
          &'SpeciesFVS    text null,'//&
          &'SpeciesPLANTS text null,'//&
          &'SpeciesFIA    text null,'//&
          &'BoardM real,'//&
          &'CubicM real,'//&
          &'TreesPerHa real,'//&
          &'BasalArea real);'//CHAR(0)

     iRet1 = fsql3_exec(IoutDBref,SQLStmtStr)
     if (iRet1.ne.0) then
        ISTATS1 = 0
        return
     end if
  end if

  write(SQLStmtStr,*)'INSERT INTO FVS_Stats_Species_Metric',&
       &' (CaseID,StandID,Year,',&
       &'SpeciesFVS,SpeciesPLANTS,SpeciesFIA,',&
       &'BoardM,CubicM,TreesPerHa,BasalArea)',&
       &'VALUES(''',CASEID,''',''',TRIM(NPLT),''',?,?,?,?,?,?,?,?);'

  iRet1 = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
  if (iRet1.ne.0) then
     ISTATS1 = 0
     return
  end if

  !     ASSIGN FVS, PLANTS AND FIA SPECIES CODE

  do I = 1,MAXSP
     if (SPECCD(1:2) .eq. JSP(I)) then
        CSP1 = JSP(I)
        CSP2 = PLNJSP(I)
        CSP3 = FIAJSP(I)
     end if
  end do

  !     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS

  BFVOL1 = BFVOL
  CFVOL1 = CFVOL
  TPA1 = TPA
  BAREA1 = BAREA

  ColNumber = 1
  iRet1 = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)
  ColNumber = ColNumber+1
  iRet1 = fsql3_bind_text(IoutDBref,ColNumber,CSP1,&
       &LEN_TRIM(CSP1))
  ColNumber = ColNumber+1
  iRet1 = fsql3_bind_text(IoutDBref,ColNumber,CSP2,&
       &LEN_TRIM(CSP2))
  ColNumber = ColNumber+1
  iRet1 = fsql3_bind_text(IoutDBref,ColNumber,CSP3,&
       &LEN_TRIM(CSP3))
  ColNumber = ColNumber+1
  iRet1 = fsql3_bind_double(IoutDBref,ColNumber,BFVOL1)
  ColNumber = ColNumber+1
  iRet1 = fsql3_bind_double(IoutDBref,ColNumber,CFVOL1)
  ColNumber = ColNumber+1
  iRet1 = fsql3_bind_double(IoutDBref,ColNumber,TPA1)
  ColNumber = ColNumber+1
  iRet1 = fsql3_bind_double(IoutDBref,ColNumber,BAREA1)
  iRet1 = fsql3_step(IoutDBref)
  iRet1 = fsql3_finalize(IoutDBref)
  if (iRet1.ne.0) then
     ISTATS1 = 0
  end if
  return

  !     DISTRIBUTION OF STAND ATTRIBUTES AMONG SAMPLE POINTS

  !     DEFINE TABLENAME

  110 continue

  iRet2 = fsql3_tableexists(IoutDBref,'FVS_Stats_Stand'//CHAR(0))
  if (iRet2.eq.0) then
     SQLStmtStr = 'CREATE TABLE FVS_Stats_Stand('//&
          &'CaseID text not null,'//&
          &'StandID text not null,'//&
          &'Year int null,'//&
          &'Characteristic text null,'//&
          &'Average real,'//&
          &'Standard_Dev real,'//&
          &'Coeff_of_Var real,'//&
          &'Sample_Size int,'//&
          &'Conf_Level_Percent int,'//&
          &'CI_LB real,'//&
          &'CI_UB real,'//&
          &'Samp_Error_Percent real,'//&
          &'Samp_Error_Units real);'//CHAR(0)

     iRet2 = fsql3_exec(IoutDBref,SQLStmtStr)
     if (iRet2.ne.0) then
        ISTATS2 = 0
        return
     end if
  end if
  write(SQLStmtStr,*)'INSERT INTO FVS_Stats_Stand',&
       &' (CaseID,StandID,Year,',&
       &'Characteristic,Average,Standard_Dev,Coeff_of_Var,',&
       &'Sample_Size,Conf_Level_Percent,CI_LB,CI_UB,',&
       &'Samp_Error_Percent,Samp_Error_Units) VALUES(''',CASEID,&
       &''',''',TRIM(NPLT),''',?,?,?,?,?,?,?,?,?,?,?);'

  iRet2 = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
  if (iRet2.ne.0) then
     ISTATS2 = 0
     return
  end if

  !     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS

  STDIST11 = STDIST1
  STDIST21 = STDIST2
  STDIST31 = STDIST3
  STDIST61 = STDIST6
  STDIST71 = STDIST7
  STDIST81 = STDIST8
  STDIST91 = STDIST9

  !     ASSIGN INTEGER VALUE TO REAL VAR FOR SAMPLE SIZE

  STDIST41 = NINT(STDIST4)
  STDIST51 = NINT(STDIST5)

  ColNumber = 1
  iRet1 = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)
  ColNumber = ColNumber+1
  iRet2 = fsql3_bind_text(IoutDBref,ColNumber,LABEL,&
       &LEN_TRIM(LABEL))
  ColNumber = ColNumber+1
  iRet2 = fsql3_bind_double(IoutDBref,ColNumber,STDIST11)
  ColNumber = ColNumber+1
  iRet2 = fsql3_bind_double(IoutDBref,ColNumber,STDIST21)
  ColNumber = ColNumber+1
  iRet2 = fsql3_bind_double(IoutDBref,ColNumber,STDIST31)
  ColNumber = ColNumber+1
  iRet2 = fsql3_bind_int(IoutDBref,ColNumber,STDIST41)
  ColNumber = ColNumber+1
  iRet2 = fsql3_bind_int(IoutDBref,ColNumber,STDIST51)
  ColNumber = ColNumber+1
  iRet2 = fsql3_bind_double(IoutDBref,ColNumber,STDIST61)
  ColNumber = ColNumber+1
  iRet2 = fsql3_bind_double(IoutDBref,ColNumber,STDIST71)
  ColNumber = ColNumber+1
  iRet2 = fsql3_bind_double(IoutDBref,ColNumber,STDIST81)
  ColNumber = ColNumber+1
  iRet2 = fsql3_bind_double(IoutDBref,ColNumber,STDIST91)
  iRet2 = fsql3_step(IoutDBref)
  iRet2 = fsql3_finalize(IoutDBref)
  if (iRet2.ne.0) then
     ISTATS2 = 0
  end if
  return
end
