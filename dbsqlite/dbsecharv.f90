!==========================!
!   dbsqlite : dbsecharv   !
!==========================!
! DBSQLITE $Id$
subroutine DBSECHARV_open()

  implicit none

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'DBSCOM.f90'

  ! Variables
  integer :: fsql3_tableexists,fsql3_exec,fsql3_prepare
  character(len=1000) :: SQLStmtStr
  integer :: iRet

  if (IDBSECON < 2) return
  if (IDBSECON .eq.0) return

  !     Make sure we have an up-to-date case ID.
  call DBSCASE(1)

  if (LMETRICVAR) then
     iRet = fsql3_tableexists(IoutDBref,&
          &"FVS_EconHarvestValue_Metric"//CHAR(0))
  else
     iRet = fsql3_tableexists(IoutDBref,"FVS_EconHarvestValue"&
          &//CHAR(0))
  end if
  if (iRet.eq.0) then
     if (LMETRICVAR) then
        SQLStmtStr = 'CREATE TABLE FVS_EconHarvestValue_Metric ('&
             &// 'CaseID text not null,'&
             &// 'Year int not null,'&
             &// 'SpeciesFVS    text null,'&
             &// 'SpeciesPLANTS text null,'&
             &// 'SpeciesFIA    text null,'&
             &// 'Min_DIB real null,'&
             &// 'Max_DIB real null,'&
             &// 'Min_DBH real null,'&
             &// 'Max_DBH real null,'&
             &// 'TPH_Removed int null,'&
             &// 'TPH_Value int null,'&
             &// 'Tonne_Per_Ha int null,'&
             &// 'CuM_Removed int null,'&
             &// 'CuM_Value int null,'&
             &// 'Board_Removed int null,'&
             &// 'Board_Value int null,'&
             &// 'Total_Value int null);'
     else
        SQLStmtStr = 'CREATE TABLE FVS_EconHarvestValue ('&
             &// 'CaseID text not null,'&
             &// 'Year int not null,'&
             &// 'SpeciesFVS    text not null,'&
             &// 'SpeciesPLANTS text not null,'&
             &// 'SpeciesFIA    text not null,'&
             &// 'Min_DIB real null,'&
             &// 'Max_DIB real null,'&
             &// 'Min_DBH real null,'&
             &// 'Max_DBH real null,'&
             &// 'TPA_Removed int null,'&
             &// 'TPA_Value int null,'&
             &// 'Tons_Per_Acre int null,'&
             &// 'Ft3_Removed int null,'&
             &// 'Ft3_Value int null,'&
             &// 'Board_Ft_Removed int null,'&
             &// 'Board_Ft_Value int null,'&
             &// 'Total_Value int null);'
     end if
     iRet = fsql3_exec(IoutDBref,SQLStmtStr//CHAR(0))
     if (iRet .ne.0) then
        IDBSECON = 0
        return
     end if
  end if

  if (LMETRICVAR) then
     SQLStmtStr = 'INSERT INTO FVS_EconHarvestValue_Metric ' //&
          &'(CaseID,Year,SpeciesFVS,SpeciesPLANTS,SpeciesFIA,' //&
          &'Min_DIB,Max_DIB,Min_DBH,Max_DBH,TPH_Removed,TPH_Value,' //&
          &'Tonne_Per_Ha,CuM_Removed,CuM_Value,Board_Removed,' //&
          &'Board_Value,Total_Value) VALUES (''' // CASEID //&
          &''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);' // CHAR(0)
  else
     SQLStmtStr = 'INSERT INTO FVS_EconHarvestValue ' //&
          &'(CaseID,Year,SpeciesFVS,SpeciesPLANTS,SpeciesFIA,' //&
          &'Min_DIB,Max_DIB,Min_DBH,Max_DBH,TPA_Removed,TPA_Value,' //&
          &'Tons_Per_Acre,Ft3_Removed,Ft3_Value,Board_Ft_Removed,' //&
          &'Board_Ft_Value,Total_Value) VALUES (''' // CASEID //&
          &''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);' // CHAR(0)
  end if
  iRet = fsql3_exec(IoutDBref,"Begin;"//CHAR(0))
  iRet = fsql3_prepare(IoutDBref,SQLStmtStr)
  if (iRet .ne.0) then
     IDBSECON = 0
     return
  end if

  end subroutine DBSECHARV_open

subroutine DBSECHARV_insert(beginAnalYear, speciesId, minDia,&
     &maxDia, minDbh, maxDbh, tpaCut, tpaValue, tonsPerAcre,&
     &ft3Volume, ft3Value, bfVolume, bfValue, totalValue)

  implicit none

  ! Common blocks
  include 'PRGPRM.f90' !Contains MAXSP
  include 'PLOT.f90' !Contains JSPIN(speciesId,speciesSymboltype), JSP, FIAJSP, & PLNJSP
  include 'DBSCOM.f90'

  ! Variables
  integer :: fsql3_bind_int,fsql3_bind_text,fsql3_bind_double,fsql3_step,fsql3_errmsg,fsql3_reset,iRet

  character(len=8) :: species1,species2,species3
  character(len=100) :: msg
  real    :: minDia,maxDia,minDbh,maxDbh
  integer :: beginAnalYear,tpaCut,tpaValue,tonsPerAcre,ft3Volume,ft3Value,bfVolume,bfValue,totalValue
  integer,intent(in) :: speciesId
  real*8  :: minDia8,maxDia8,minDbh8,maxDbh8

  if (IDBSECON < 2) return !ECON harvest table was not requested

  !     assign FVS, PLANTS and FIA species codes

  species1 = JSP(speciesId)
  species2 = PLNJSP(speciesId)
  species3 = FIAJSP(speciesId)

  minDia8 = minDia
  maxDia8 = maxDia
  minDbh8 = minDbh
  maxDbh8 = maxDbh
  iRet = fsql3_bind_int(IoutDBref,1,beginAnalYear)
  iRet = fsql3_bind_text(IoutDBref,2,species1,len_trim(species1))
  iRet = fsql3_bind_text(IoutDBref,3,species2,len_trim(species2))
  iRet = fsql3_bind_text(IoutDBref,4,species3,len_trim(species3))
  if (minDia8.ge.0)&
       &iRet = fsql3_bind_double(IoutDBref,5,  minDia8)
  if (maxDia8.ge.0)&
       &iRet = fsql3_bind_double(IoutDBref,6,  maxDia8)
  if (minDbh8.ge.0)&
       &iRet = fsql3_bind_double(IoutDBref,7,  minDbh8)
  if (maxDbh8.ge.0)&
       &iRet = fsql3_bind_double(IoutDBref,8,  maxDbh8)
  if (tpaCut     .ge.0)&
       &iRet = fsql3_bind_int   (IoutDBref,9,  tpaCut)
  if (tpaValue   .ge.0)&
       &iRet = fsql3_bind_int   (IoutDBref,10,  tpaValue)
  if (tonsPerAcre.ge.0)&
       &iRet = fsql3_bind_int   (IoutDBref,11,  tonsPerAcre)
  if (ft3Volume  .ge.0)&
       &iRet = fsql3_bind_int   (IoutDBref,12, ft3Volume)
  if (ft3Value   .ge.0)&
       &iRet = fsql3_bind_int   (IoutDBref,13, ft3Value)
  if (bfVolume   .ge.0)&
       &iRet = fsql3_bind_int   (IoutDBref,14, bfVolume)
  if (bfValue    .ge.0)&
       &iRet = fsql3_bind_int   (IoutDBref,15, bfValue)
  if (totalValue .ge.0)&
       &iRet = fsql3_bind_int    (IoutDBref,16, totalValue)
  iRet = fsql3_step(IoutDBref)
  iRet = fsql3_reset(IoutDBref)
  if (iRet>0) then
     iRet = fsql3_errmsg(IoutDBref, msg, 100)
     if (LMETRICVAR) then
        print *,"FVS_EconHarvestValue_Metric step error: ",trim(msg)
     else
        print *,"FVS_EconHarvestValue step error: ",trim(msg)
     end if
  end if
  end subroutine DBSECHARV_insert

subroutine DBSECHARV_close()
  implicit none
  include 'PRGPRM.f90'

  ! Common blocks
  include 'DBSCOM.f90'
  ! Variables
  integer :: iRet,fsql3_finalize,fsql3_errmsg,fsql3_exec
  character(len=101) :: msg

  if (IDBSECON < 2) return !ECON harvest table was not requested

  iRet = fsql3_exec(IoutDBref,"Commit;"//CHAR(0))
  if (iRet>0) then
     iRet = fsql3_errmsg(IoutDBref, msg, 100)
     if (LMETRICVAR) then
        print *,"FVS_EconHarvestValue_Metric commit error: ",trim(msg)
     else
        print *,"FVS_EconHarvestValue commit error: ",trim(msg)
     end if
  end if
  iRet = fsql3_finalize (IoutDBref)
  if (iRet>0) then
     iRet = fsql3_errmsg(IoutDBref, msg, 100)
     if (LMETRICVAR) then
        print *,"FVS_EconHarvestValue_Metric finalize error: ",trim(msg)
     else
        print *,"FVS_EconHarvestValue finalize error: ",trim(msg)
     end if
  end if

  end subroutine DBSECHARV_close
