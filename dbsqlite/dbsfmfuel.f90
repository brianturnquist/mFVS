!==========================!
!   dbsqlite : dbsfmfuel   !
!==========================!
! DBSQLITE $Id$
subroutine DBSFMFUEL(IYEAR,NPLT,MSE,LITTER,DUFF,CLT3,CGT3,&
     &C3TO6,C6TO12,CGT12,HERB,CROWN,CTOTAL,PERCDUFF,PERCGT3,&
     &PERTRCR,SM25,SM10,KODE)
  implicit none

  !     PURPOSE: TO POPULATE A DATABASE WITH THE FUELS CONSUMPTION REPORT
  !              INFORMATION
  !     AUTH: S. REBAIN -- FMSC -- DECEMBER 2004
  !     INPUT:
  !              THE FUEL CONSUMPTION OUTPUT FROM THE FIRE MODEL.
  !              1: MINERAL SOIL EXPOSURE
  !              2: LITTER CONSUMPTION
  !              3: DUFF CONSUMPTION
  !              4: CONSUMPTION 0 - 3
  !              5: CONSUMPTION >= 3
  !              6: CONSUMPTION 3 -  6
  !              7: CONSUMPTION 6 - 12
  !              8: CONSUMPTION >= 12
  !              9: HERB / SHRUB CONSUMPTION
  !             10: CROWN CONSUMPTION
  !             11: TOTAL CONSUMPTION
  !             12: % CONSUMPTION DUFF
  !             13: % CONSUMPTION >= 3
  !             14: % TREES WITH CROWNING
  !             15: SMOKE PRODUCTION < 2.5
  !             16: SMOKE PRODUCTION < 10
  !             17: KODE FOR WHETHER THE REPORT ALSO DUMPS TO FILE

  ! Common blocks
  include 'DBSCOM.f90'

  ! Variables
  integer :: IYEAR,iRet,KODE,PERTRCR,ColNumber
  real    :: MSE,LITTER,DUFF,CLT3,CGT3,C3TO6,C6TO12,CGT12,HERB,CROWN,CTOTAL,PERCDUFF,PERCGT3,SM25,SM10
  double precision :: MSEB,LITTERB,DUFFB,CLT3B,CGT3B,C3TO6B,C6TO12B,CGT12B,HERBB,CROWNB,CTOTALB,&
       &PERCDUFFB,PERCGT3B,SM25B,SM10B
  character(len=2000) :: SQLStmtStr
  character(len=26) :: NPLT

  integer :: fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,fsql3_prepare,fsql3_bind_double,&
       &fsql3_finalize

  if (IFUELC.eq.0) return
  if (IFUELC.eq.2) KODE = 0

  call DBSCASE(1)

  iRet = fsql3_tableexists(IoutDBref,&
       &"FVS_Consumption"//CHAR(0))
  if (iRet.eq.0) then
     SQLStmtStr = 'CREATE TABLE FVS_Consumption('//&
          &'CaseID text not null,'//&
          &'StandID text not null,'//&
          &'Year Int null,'//&
          &'Min_Soil_Exp real null,'//&
          &'Litter_Consumption real null,'//&
          &'Duff_Consumption real null,'//&
          &'Consumption_lt3 real null,'//&
          &'Consumption_ge3 real null,'//&
          &'Consumption_3to6 real null,'//&
          &'Consumption_6to12 real null,'//&
          &'Consumption_ge12 real null,'//&
          &'Consumption_Herb_Shrub real null,'//&
          &'Consumption_Crowns real null,'//&
          &'Total_Consumption real null,'//&
          &'Percent_Consumption_Duff real null,'//&
          &'Percent_Consumption_ge3 real null,'//&
          &'Percent_Trees_Crowning int null,'//&
          &'Smoke_Production_25 real null,'//&
          &'Smoke_Production_10 real null);'//CHAR(0)
     iRet = fsql3_exec(IoutDBref,SQLStmtStr)
     if (iRet .ne.0) then
        IFUELC = 0
        return
     end if
  end if

  !     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS

  MSEB = MSE
  LITTERB = LITTER
  DUFFB = DUFF
  CLT3B = CLT3
  CGT3B = CGT3
  C3TO6B = C3TO6
  C6TO12B = C6TO12
  CGT12B = CGT12
  HERBB = HERB
  CROWNB = CROWN
  CTOTALB = CTOTAL
  PERCDUFFB = PERCDUFF
  PERCGT3B = PERCGT3
  SM25B = SM25
  SM10B = SM10

  write(SQLStmtStr,*)'INSERT INTO FVS_Consumption (CaseID,',&
       &'StandID,Year,Min_Soil_Exp,Litter_Consumption,',&
       &'Duff_Consumption,',&
       &'Consumption_lt3,Consumption_ge3,Consumption_3to6,',&
       &'Consumption_6to12,Consumption_ge12,Consumption_Herb_Shrub,',&
       &'Consumption_Crowns,Total_Consumption,',&
       &'Percent_Consumption_Duff,',&
       &'Percent_Consumption_ge3,Percent_Trees_Crowning,',&
       &'Smoke_Production_25,Smoke_Production_10) VALUES (''',CASEID,&
       &''',''',TRIM(NPLT),''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'

  iRet = fsql3_prepare(IoutDBref, trim(SQLStmtStr)//CHAR(0))

  !     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

  ColNumber = 1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,MSEB)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,LITTERB)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DUFFB)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,CLT3B)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,CGT3B)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,C3TO6B)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,C6TO12B)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,CGT12B)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,HERBB)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,CROWNB)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,CTOTALB)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,PERCDUFFB)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,PERCGT3B)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,PERTRCR)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,SM25B)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,SM10B)

  iRet = fsql3_step(IoutDBref)
  iRet = fsql3_finalize(IoutDBref)
  if (iRet.ne.0) then
     IFUELC = 0
  end if
  return
end
