!=========================!
!   dbsqlite : dbsclsum   !
!=========================!
! DBSQLITE $Id$
subroutine DBSCLSUM(CNPLT,IYR,SP,SPVIAB,&
     &SPBA,SPTPA,SPMORT1,SPMORT2,SPGMULT,&
     &SPSITGM,MXDENMLT,POTESTAB)
  implicit none

  !     POPULATE A DATABASE WITH THE CLIMATE SUMMARY

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'DBSCOM.f90'

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

  if (LMETRICVAR) then
     IRCODE = fsql3_tableexists(IoutDBref,&
          &"FVS_Climate_Metric"//CHAR(0))
  else
     IRCODE = fsql3_tableexists(IoutDBref,"FVS_Climate"//CHAR(0))
  end if
  if (IRCODE.eq.0) then
     if (LMETRICVAR) then
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
     else
        SQLStmtStr = 'CREATE TABLE FVS_Climate('//&
             &'CaseID text not null,'//&
             &'StandID text not null,'//&
             &'Year Int null,'//&
             &'SpeciesFVS    text null,'//&
             &'SpeciesPLANTS text null,'//&
             &'SpeciesFIA    text null,'//&
             &'Viability real null,'//&
             &'BA real null,'//&
             &'TPA real null,'//&
             &'ViabMort real null,'//&
             &'dClimMort real null,'//&
             &'GrowthMult real null,'//&
             &'SiteMult real null,'//&
             &'MxDenMult real null,'//&
             &'AutoEstbTPA real null)'//CHAR(0)
     end if
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

  if (LMETRICVAR) then
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
  else
     write(SQLStmtStr,*)'INSERT INTO FVS_Climate (CaseID,'//&
          &'StandID,Year,SpeciesFVS,SpeciesPLANTS,SpeciesFIA,'//&
          &'Viability,BA,TPA,ViabMort,'//&
          &'dClimMort,GrowthMult,SiteMult,MxDenMult,AutoEstbTPA) '//&
          &'VALUES(''',CASEID,''',''',TRIM(CNPLT),''',',IYR,',',&
          &'''',TRIM(CSP1),''',',&
          &'''',TRIM(CSP2),''',',&
          &'''',TRIM(CSP3),''',',&
          &SPVIAB,',',SPBA,',',SPTPA,',',&
          &SPMORT1,',',SPMORT2,',',SPGMULT,',',&
          &SPSITGM,',',MXDENMLT,',',POTESTAB,')'
  end if

  IRCODE = fsql3_exec(IoutDBref,trim(SQLStmtStr)//CHAR(0))
  if (IRCODE .ne.0) ICLIM = 0
  return
end
