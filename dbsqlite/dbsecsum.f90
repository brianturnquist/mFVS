!=========================!
!   dbsqlite : dbsecsum   !
!=========================!
! DBSQLITE $Id$
subroutine DBSECSUM(STDID,beginAnalYear, endTime, pretend,&
     &costUndisc,revUndisc, costDisc, revDisc, npv, irr,&
     &irrCalculated,bcRatio, bcRatioCalculated, rrr,&
     &rrrCalculated,sev, sevCalculated, forestValue,&
     &forestValueCalculated,reprodValue, reprodValueCalculated,&
     &ft3Volume, bfVolume,discountRate, sevInput, sevInputUsed)

  implicit none
  include 'PRGPRM.f90'

  ! Common blocks
  include 'DBSCOM.f90'

  ! Variables
  character(len=26)   :: STDID
  character(len=*)    :: pretend
  character(len=2000) :: SQLStmtStr

  integer,parameter :: zero = 0
  integer :: beginAnalYear,endTime,status,ft3Volume,bfVolume,iRet

  logical :: sevCalculated,rrrCalculated,forestValueCalculated,reprodValueCalculated,irrCalculated,&
       &bcRatioCalculated,sevInputUsed

  real :: costUndisc,revUndisc,costDisc,revDisc,npv,irr,bcRatio,rrr,sev,forestValue,reprodValue,&
       &discountRate,sevInput
  real*8 :: costUndisc8,revUndisc8,costDisc8,revDisc8,npv8,irr8,bcRatio8,rrr8,sev8,forestValue8,&
       &reprodValue8,discountRate8,sevInput8

  integer :: fsql3_bind_double,fsql3_finalize,fsql3_errmsg,fsql3_bind_int,fsql3_bind_text,fsql3_exec,&
       &fsql3_tableexists,fsql3_prepare,fsql3_step

  if (IDBSECON == 0) return

  !    Make sure we have an up-to-date case ID.

  call DBSCASE(1)

  !     Ensure that the FVS_EconSummary table exists in the DB.
  if (LMETRICVAR) then
     iRet = fsql3_tableexists(IoutDBref,&
          &"FVS_EconSummary_Metric"//CHAR(0))
  else
     iRet = fsql3_tableexists(IoutDBref,"FVS_EconSummary"//CHAR(0))
  end if
  if (iRet.eq.0) then
     if (LMETRICVAR) then
        SQLStmtStr = 'CREATE TABLE FVS_EconSummary_Metric ('&
             &// 'CaseID text not null, '&
             &// 'StandID text not null, '&
             &// 'Year int null, '&
             &// 'Period int null, '&
             &// 'Pretend_Harvest text null, '&
             &// 'Undiscounted_Cost real null, '&
             &// 'Undiscounted_Revenue real null, '&
             &// 'Discounted_Cost real null, '&
             &// 'Discounted_Revenue real null, '&
             &// 'PNV real null, '&
             &// 'IRR real null, '&
             &// 'BC_Ratio real null, '&
             &// 'RRR real null, '&
             &// 'SEV real null, '&
             &// 'Value_of_Forest real null, '&
             &// 'Value_of_Trees real null, '&
             &// 'Mrch_Cubic_Volume int null, '&
             &// 'Mrch_Board_Volume int null, '&
             &// 'Discount_Rate real null, '&
             &// 'Given_SEV real null);' // CHAR(0)
     else
        SQLStmtStr = 'CREATE TABLE FVS_EconSummary ('&
             &// 'CaseID text not null, '&
             &// 'StandID text not null, '&
             &// 'Year int null, '&
             &// 'Period int null, '&
             &// 'Pretend_Harvest text null, '&
             &// 'Undiscounted_Cost real null, '&
             &// 'Undiscounted_Revenue real null, '&
             &// 'Discounted_Cost real null, '&
             &// 'Discounted_Revenue real null, '&
             &// 'PNV real null, '&
             &// 'IRR real null, '&
             &// 'BC_Ratio real null, '&
             &// 'RRR real null, '&
             &// 'SEV real null, '&
             &// 'Value_of_Forest real null, '&
             &// 'Value_of_Trees real null, '&
             &// 'Mrch_Cubic_Volume int null, '&
             &// 'Mrch_BoardFoot_Volume int null, '&
             &// 'Discount_Rate real null, '&
             &// 'Given_SEV real null);' // CHAR(0)
     end if
     iRet = fsql3_exec(IoutDBref,SQLStmtStr)
     if (iRet .ne.0) then
        IDBSECON = 0
        return
     end if
  end if

  if (LMETRICVAR) then
     write(SQLStmtStr, *) 'INSERT INTO ',&
          &'FVS_EconSummary_Metric (CaseID, StandID, Year, Period,',&
          &'Pretend_Harvest, Undiscounted_Cost, Undiscounted_Revenue,',&
          &'Discounted_Cost, Discounted_Revenue, PNV, IRR, BC_Ratio,',&
          &'RRR, SEV, Value_of_Forest, Value_of_Trees,',&
          &'Mrch_Cubic_Volume, Mrch_Board_Volume, Discount_Rate,',&
          &'Given_SEV)','VALUES (''',CASEID,''',''',TRIM(STDID),&
          &''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);' // CHAR(0)
  else
     write(SQLStmtStr, *) 'INSERT INTO ',&
          &'FVS_EconSummary (CaseID, StandID, Year, Period,',&
          &'Pretend_Harvest, Undiscounted_Cost, Undiscounted_Revenue,',&
          &'Discounted_Cost, Discounted_Revenue, PNV, IRR, BC_Ratio,',&
          &'RRR, SEV, Value_of_Forest, Value_of_Trees,',&
          &'Mrch_Cubic_Volume, Mrch_BoardFoot_Volume, Discount_Rate,',&
          &'Given_SEV)','VALUES (''',CASEID,''',''',TRIM(STDID),&
          &''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);' // CHAR(0)
  end if
  iRet = fsql3_prepare(IoutDBref,SQLStmtStr)
  if (iRet .ne.0) then
     iRet = fsql3_errmsg(SQLStmtStr, 200)
     print *,"dbsecsum prepare error: ",TRIM(SQLStmtStr)
     IDBSECON = 0
     return
  end if

  iRet = fsql3_bind_int(IoutDBref,1, beginAnalYear)
  iRet = fsql3_bind_int(IoutDBref,2, endTime)
  iRet = fsql3_bind_text(IoutDBref,3, pretend, 3)
  if (costUndisc >= 0) then
     costUndisc8 = costUndisc
     iRet = fsql3_bind_double(IoutDBref,4, costUndisc8)
  end if
  if (revUndisc >= 0) then
     revUndisc8 = revUndisc
     iRet = fsql3_bind_double(IoutDBref,5, revUndisc8)
  end if
  if (costDisc >= 0) then
     costDisc8 = costDisc
     iRet = fsql3_bind_double(IoutDBref,6, costDisc8)
  end if
  if (revDisc >= 0) then
     revDisc8 = revDisc
     iRet = fsql3_bind_double(IoutDBref,7, revDisc8)
  end if
  npv8 = npv
  iRet = fsql3_bind_double(IoutDBref,8, npv8)
  if (irrCalculated) then
     irr8 = irr
     iRet = fsql3_bind_double(IoutDBref,9, irr8)
  end if
  if (bcRatioCalculated) then
     bcRatio8 = bcRatio
     iRet = fsql3_bind_double(IoutDBref,10, bcRatio8)
  end if
  if (rrrCalculated) then
     rrr8 = rrr
     iRet = fsql3_bind_double(IoutDBref,11, rrr8)
  end if
  if (sevCalculated) then
     sev8 = sev
     iRet = fsql3_bind_double(IoutDBref,12, sev8)
  end if
  if (forestValueCalculated) then
     forestValue8 = forestValue
     iRet = fsql3_bind_double(IoutDBref,13, forestValue8)
  end if
  if (reprodValueCalculated) then
     reprodValue8 = reprodValue
     iRet = fsql3_bind_double(IoutDBref,14, reprodValue8)
  end if
  iRet = fsql3_bind_int(IoutDBref,15, ft3Volume)
  iRet = fsql3_bind_int(IoutDBref,16, bfVolume)
  discountRate8 = discountRate
  iRet = fsql3_bind_double(IoutDBref,17, discountRate8)
  if (sevInputUsed) then
     sevInput8 = sevInput
     iRet = fsql3_bind_double(IoutDBref,18, sevInput8)
  end if

  iRet = fsql3_step(IoutDBref)
  iRet = fsql3_finalize(IoutDBref)
  if (iRet>0) then
     iRet = fsql3_errmsg(SQLStmtStr, 200)
     print *,"dbsecsum finalize error: ",TRIM(SQLStmtStr)
     IDBSECON = 0
  end if

  return

  entry getDbsEconStatus(status)
  if (IDBSECON == 0) then
     status = 0 !DB-FVS output not requested
  else if (IDBSECON == 1) then
     status = 1 ! Write to summary output to DB.
  else
     status = 2 ! Write to summary and harvest output to DB.
  end if
  return
end
