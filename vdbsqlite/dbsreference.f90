!==============================!
!   vdbsqlite : dbsreference   !
!==============================!
subroutine DBSREFERENCE
  implicit none

  !  ROUTINE TO OUTPUT INVENTORY REFERENCE INFORMATION

  ! Common blocks
  include 'PRGPRM.f90'
  include 'DBSCOM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'VOLSTD.f90'

  ! Variables
  integer :: iRet,ColNumber,iInvRef,I,ISPPSDI,ISTIDX
  double precision :: MIND,MERCHTOPD,STUMP,SAWD,SAWTD,SAWSTMP,BFD,BFTD,BFSTUMP
  character(len=2000) :: SQLStmtStr
  character(len=20) :: TABLENAME
  character(len=3) :: CRUISETYPE
  character(len=7) :: SDITYPE

  integer :: fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,fsql3_prepare,fsql3_bind_double,&
       &fsql3_finalize,fsql3_addcolifabsent

  call DBSCASE(1)

  TABLENAME = 'FVS_InvReference'

  iRet = fsql3_tableexists(IoutDBref,TRIM(TABLENAME)//CHAR(0))
  if (iRet.eq.0) then
     SQLStmtStr = 'CREATE TABLE '//TRIM(TABLENAME)//&
          &' (CaseID text not null, '//&
          &'StandID text not null, '//&
          &'LocationCode int, '//&
          &'SpeciesNum int, '//&
          &'SpeciesFVS text, '//&
          &'SpeciesPlants text, '//&
          &'SpeciesFIA text, '//&
          &'SDIType text,'//&
          &'SDIMax int, '//&
          &'SiteIndex int, '//&
          &'CFCruiseType text,'//&
          &'CFVolEq text, '//&
          &'CFMinDBH real, '//&
          &'CFTopDia real, '//&
          &'CFStump real, '//&
          &'CFSawMinDBH real, '//&
          &'CFSawTopDia real, '//&
          &'CFSawStump real, '//&
          &'BFVolEq text, '//&
          &'BFMinDBH real, '//&
          &'BFTopDia real, '//&
          &'BFStump real);'//CHAR(0)

     iRet = fsql3_exec(IoutDBref, SQLStmtStr)
     if (iRet.ne.0) then
        iInvRef = 0
        return
     end if
  end if

  !     CHECK EXISTING TABLE FOR COLUMN(S) ADDED WITH LOCCODE UPGRADE (2026)
  !     `LocationCode`,
  !     TO ACCOUNT FOR ADDING TO DATABASE CREATED PROIR TO UPGRADE

  iRet = fsql3_addcolifabsent(IoutDBref,TRIM(TABLENAME)//CHAR(0),&
       &"LocationCode"//CHAR(0),"int"//CHAR(0))

  do I = 1,MAXSP
     if (TRIM(JSP(I)).eq.'') cycle
     ISPPSDI = NINT(SDIDEF(I))
     ISTIDX = NINT(SITEAR(I))
     MIND = DBHMIN(I)
     MERCHTOPD = TOPD(I)
     STUMP = STMP(I)
     SAWD = SCFMIND(I)
     SAWTD = SCFTOPD(I)
     SAWSTMP = SCFSTMP(I)
     BFD = BFMIND(I)
     BFTD = BFTOPD(I)
     BFSTUMP = BFSTMP(I)
     if (CFCTYPE .eq. 'I') then
        CRUISETYPE = 'FIA'
     else
        CRUISETYPE = 'FVS'
     end if

     if (LZEIDE) then
        SDITYPE = '  ZEIDE'
     else
        SDITYPE = 'REINEKE'
     end if

     SQLStmtStr = 'INSERT INTO '//TRIM(TABLENAME)//&
          &' (CaseID,StandID,LocationCode,'//&
          &'SpeciesNum,SpeciesFVS,SpeciesPlants,SpeciesFIA,'//&
          &'SDIType,SDIMax,SiteIndex,'//&
          &'CFCruiseType,CFVolEq,CFMinDBH,CFTopDia,CFStump,'//&
          &'CFSawMinDBH,CFSawTopDia,CFSawStump,'//&
          &'BFVolEQ,BFMinDBH,BFTopDia,BFStump)'//&
          &" VALUES('"//CASEID//"','"//TRIM(NPLT)//"',?,?,'"//&
          &TRIM(JSP(I))//"','"//TRIM(PLNJSP(I))//"','"//&
          &TRIM(FIAJSP(I))//"','"//&
          &SDITYPE//"',"//'?,?,'//&
          &"'"//CRUISETYPE//"','"//TRIM(VEQNNC(I))//"',?,?,?,"//&
          &'?,?,?,'//&
          &"'"//TRIM(VEQNNB(I))//"',?,?,?);"//CHAR(0)

     iRet = fsql3_prepare(IoutDBref,SQLStmtStr)
     if (iRet.ne.0) then
        iInvRef = 0
        return
     end if

     ColNumber = 1
     iRet = fsql3_bind_int(IoutDBref,ColNumber,KODFOR)

     ColNumber = ColNumber + 1
     iRet = fsql3_bind_int(IoutDBref,ColNumber,I)

     ColNumber = ColNumber + 1
     iRet = fsql3_bind_int(IoutDBref,ColNumber,ISPPSDI)

     ColNumber = ColNumber + 1
     iRet = fsql3_bind_int(IoutDBref,ColNumber,ISTIDX)

     ColNumber = ColNumber + 1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,MIND)

     ColNumber = ColNumber + 1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,MERCHTOPD)

     ColNumber = ColNumber + 1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,STUMP)

     ColNumber = ColNumber + 1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,SAWD)

     ColNumber = ColNumber + 1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,SAWTD)

     ColNumber = ColNumber + 1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,SAWSTMP)

     ColNumber = ColNumber + 1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,BFD)

     ColNumber = ColNumber + 1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,BFTD)

     ColNumber = ColNumber + 1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,BFSTUMP)

     iRet = fsql3_step(IoutDBref)
  end do
  iRet = fsql3_finalize(IoutDBref)
  if (iRet.ne.0) iInvRef = 0

  return
end
