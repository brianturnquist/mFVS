!==========================!
!   vdbsqlite : dbsfmpfc   !
!==========================!
! VDBSQLITE $Id$
subroutine DBSFMPFC(NPLT,WINDSP,TEMP,MOIST1,MOIST2,MOIST3,MOIST4,&
     &MOIST5,MOIST6,MOIST7,KODE)
  implicit none

  !     PURPOSE: TO POPULATE A DATABASE WITH THE FIRE MODELS POTENTIAL FIRE
  !              CONDITIONS OUTPUT.
  !     AUTH: M.SHETTLES -- FMSC -- NOVEMBER 2020

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'DBSCOM.f90'

  ! Variables
  integer,parameter :: MxMsg = 500
  character(LEN = MxMsg) :: Msg
  integer :: TEMP,KODE,ColNumber,iRet
  real    :: WINDSP,MOIST1,MOIST2,MOIST3,MOIST4,MOIST5,MOIST6,MOIST7
  double precision :: DWINDSP,DMOIST1,DMOIST2,DMOIST3,DMOIST4,DMOIST5
  double precision :: DMOIST6,DMOIST7
  character(len=2000) :: SQLStmtStr
  character(len=8) :: FCOND
  character(len=26) :: NPLT

  integer :: fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,fsql3_prepare,fsql3_bind_double,&
       &fsql3_finalize,fsql3_errmsg

  !     Initialize variables

  if (IPOTFIREC.eq.0) return

  !     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

  call DBSCASE(1)

  if (KODE.eq.1) then
     FCOND = "Severe"
  else
     FCOND = "Moderate"
  end if

  if (LMETRICVAR) then
     iRet = fsql3_tableexists(IoutDBref,&
          &"FVS_PotFire_Cond_Metric"//CHAR(0))
  else
     iRet = fsql3_tableexists(IoutDBref,&
          &"FVS_PotFire_Cond"//CHAR(0))
  end if
  if (iRet.eq.0) then
     if (LMETRICVAR) then
        SQLStmtStr = 'CREATE TABLE FVS_PotFire_Cond_Metric ('//&
             &'CaseID text not null,'//&
             &'StandID text not null,'//&
             &'Fire_Condition text null,'//&
             &'Wind_Speed real null,'//&
             &'Temperature int null,'//&
             &'One_Hr_Moisture real null,'//&
             &'Ten_Hr_Moisture real null,'//&
             &'Hundred_Hr_Moisture real null,'//&
             &'Thousand_Hr_Moisture real null,'//&
             &'Duff_Moisture real null,'//&
             &'Live_Woody_Moisture real null,'//&
             &'Live_Herb_Moisture real null);'//CHAR(0)
     else
        SQLStmtStr = 'CREATE TABLE FVS_PotFire_Cond ('//&
             &'CaseID text not null,'//&
             &'StandID text not null,'//&
             &'Fire_Condition text null,'//&
             &'Wind_Speed real null,'//&
             &'Temperature int null,'//&
             &'One_Hr_Moisture real null,'//&
             &'Ten_Hr_Moisture real null,'//&
             &'Hundred_Hr_Moisture real null,'//&
             &'Thousand_Hr_Moisture real null,'//&
             &'Duff_Moisture real null,'//&
             &'Live_Woody_Moisture real null,'//&
             &'Live_Herb_Moisture real null);'//CHAR(0)
     end if
     iRet = fsql3_exec(IoutDBref,SQLStmtStr)
     if (iRet.ne.0) then
        iRet = fsql3_errmsg(IinDBref, Msg, MxMsg)
        if (LMETRICVAR) then
           print *,"FVS_PotFire_Cond_Metric exec direct error:",&
                &Msg(:iRet)
        else
           print *,"FVS_PotFire_Cond exec direct error:",Msg(:iRet)
        end if
        IPOTFIREC = 0
        return
     end if
  end if

  DWINDSP = WINDSP
  DMOIST1 = MOIST1
  DMOIST2 = MOIST2
  DMOIST3 = MOIST3
  DMOIST4 = MOIST4
  DMOIST5 = MOIST5
  DMOIST6 = MOIST6
  DMOIST7 = MOIST7

  if (LMETRICVAR) then
     write(SQLStmtStr,*)'INSERT INTO FVS_PotFire_Cond_Metric ',&
          &'(CaseID,StandID,Fire_Condition,Wind_Speed,Temperature,',&
          &'One_Hr_Moisture,Ten_Hr_Moisture,',&
          &'Hundred_Hr_Moisture,Thousand_Hr_Moisture,Duff_Moisture,',&
          &'Live_Woody_Moisture,Live_Herb_Moisture) VALUES (''',&
          &CASEID,''',''',TRIM(NPLT),''',''',TRIM(FCOND),&
          &''',?,?,?,?,?,?,?,?,?);'
  else
     write(SQLStmtStr,*)'INSERT INTO FVS_PotFire_Cond (CaseID,',&
          &'StandID,Fire_Condition,Wind_Speed,Temperature,',&
          &'One_Hr_Moisture,Ten_Hr_Moisture,',&
          &'Hundred_Hr_Moisture,Thousand_Hr_Moisture,Duff_Moisture,',&
          &'Live_Woody_Moisture,Live_Herb_Moisture) VALUES (''',&
          &CASEID,''',''',TRIM(NPLT),''',''',TRIM(FCOND),&
          &''',?,?,?,?,?,?,?,?,?);'
  end if

  iRet = fsql3_prepare(IoutDBref, TRIM(SQLStmtStr)//CHAR(0))

  if (iRet.ne.0) then
     iRet = fsql3_errmsg(IinDBref, Msg, MxMsg)
     if (LMETRICVAR) then
        print *,"FVS_PotFire_Cond_Metric prepare error:",Msg(:iRet)
     else
        print *,"FVS_PotFire_Cond prepare error:",Msg(:iRet)
     end if
     IPOTFIREC = 0
     return
  end if

  !      ColNumber=1
  !      iRet = fsql3_bind_double(IoutDBref,ColNumber,FCOND)

  ColNumber = 1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DWINDSP)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,TEMP)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DMOIST1)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DMOIST2)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref, ColNumber,DMOIST3)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DMOIST4)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DMOIST5)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DMOIST6)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DMOIST7)

  iRet = fsql3_step(IoutDBref)
  if (iRet.ne.0) then
     iRet = fsql3_errmsg(IinDBref, Msg, MxMsg)
     if (LMETRICVAR) then
        print *,"FVS_PotFire_Cond_Metric step error:",Msg(:iRet)
     else
        print *,"FVS_PotFire_Cond step error:",Msg(:iRet)
     end if
     IPOTFIREC = 0
  end if
  iRet = fsql3_finalize(IoutDBref)
  if (iRet.ne.0) then
     iRet = fsql3_errmsg(IinDBref, Msg, MxMsg)
     if (LMETRICVAR) then
        print *,"FVS_PotFire_Cond_Metric finalize error:",Msg(:iRet)
     else
        print *,"FVS_PotFire_Cond finalize error:",Msg(:iRet)
     end if
     IPOTFIREC = 0
  end if
  return

end
