!==========================!
!   dbsqlite : dbsingrow   !
!==========================!
! DBSQLITE $Id$
subroutine DBSINGROW(IYEAR,INGROW,CSP,TPAIN)
  implicit none

  !     PURPOSE: TO POPULATE A DATABASE WITH REGENERATION INGROWTH
  !              SPECIES AND TPA INFORMATION.
  !     AUTH: M. SHETTLES -- FMSC -- AUGUST 2019

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'DBSCOM.f90'
  include 'PLOT.f90'
  include 'METRIC.f90'

  ! Variables
  integer :: ColNumber,I,iret,IYEAR,IYEAR1
  real    :: INGROW,TPAIN
  double precision :: INGROW1,TPAIN1
  character(len=2000) :: SQLStmtStr
  character(len=2) :: CSP
  character(len=8) :: CSP1
  character(len=8) :: CSP2
  character(len=8) :: CSP3

  !OMMONS END

  integer :: fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,fsql3_prepare,fsql3_bind_double,&
       &fsql3_finalize,fsql3_bind_text

  if (IREG5.ne.1) return

  call DBSCASE(1)

  !     DEFINE TAABLENAME

  if (LMETRICVAR) then
     iRet = fsql3_tableexists(IoutDBref,&
          &'FVS_Regen_Ingrow_Metric'//CHAR(0))
  else
     iRet = fsql3_tableexists(IoutDBref,'FVS_Regen_Ingrow'//CHAR(0))
  end if

  !     SITE PREP SUMMARY

  if (iRet.eq.0) then
     if (LMETRICVAR) then
        SQLStmtStr = 'CREATE TABLE FVS_Regen_Ingrow_Metric('//&
             &'CaseID           text not null,'//&
             &'StandID          text not null,'//&
             &'Year             int  null,'//&
             &'IngrowthTotalTph real null,'//&
             &'SpeciesFVS       text null,'//&
             &'SpeciesPLANTS    text null,'//&
             &'SpeciesFIA       text null,'//&
             &'IngrowthTph      int  null);'//CHAR(0)
     else
        SQLStmtStr = 'CREATE TABLE FVS_Regen_Ingrow('//&
             &'CaseID           text not null,'//&
             &'StandID          text not null,'//&
             &'Year             int  null,'//&
             &'IngrowthTotalTpa real null,'//&
             &'SpeciesFVS       text null,'//&
             &'SpeciesPLANTS    text null,'//&
             &'SpeciesFIA       text null,'//&
             &'IngrowthTpa      int  null);'//CHAR(0)
     end if

     iRet = fsql3_exec(IoutDBref,SQLStmtStr)
     if (iRet .ne.0) then
        IREG5 = 0
        return
     end if
  end if

  if (LMETRICVAR) then
     write(SQLStmtStr,*)'INSERT INTO FVS_Regen_Ingrow_Metric',&
          &' (CaseID,StandID,Year,IngrowthTotalTph,',&
          &' SpeciesFVS,SpeciesPLANTS,SpeciesFIA,IngrowthTph)',&
          &' VALUES(''',CASEID,''',''',TRIM(NPLT),''',?,?,?,?,?,?);'
  else
     write(SQLStmtStr,*)'INSERT INTO FVS_Regen_Ingrow',&
          &' (CaseID,StandID,Year,IngrowthTotalTpa,',&
          &'SpeciesFVS,SpeciesPLANTS,SpeciesFIA,IngrowthTpa)',&
          &' VALUES(''',CASEID,''',''',TRIM(NPLT),''',?,?,?,?,?,?);'
     iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
  end if
  if (iRet .ne.0) then
     IREG5 = 0
     return
  end if

  !     ASSIGN FVS, PLANTS AND FIA SPECIES CODE

  do I = 1,MAXSP
     if (CSP .eq. JSP(I)) then
        CSP1 = JSP(I)
        CSP2 = PLNJSP(I)
        CSP3 = FIAJSP(I)
     end if
  end do

  !     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS

  INGROW1 = INGROW
  TPAIN1 = TPAIN
  IYEAR1 = IYEAR

  ColNumber = 1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR1)
  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,INGROW1)
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
  iRet = fsql3_bind_double(IoutDBref,ColNumber,TPAIN1)
  iRet = fsql3_step(IoutDBref)
  iRet = fsql3_finalize(IoutDBref)
  if (iRet.ne.0) then
     IREG5 = 0
  end if
  return
end
