!========================!
!   dbsqlite : dbssprt   !
!========================!
! DBSQLITE $Id$
subroutine DBSSPRT(CSP,TPASUM,HTAVE,TPATOT,&
     &WTHTAVE,TOT,IYEAR)
  implicit none

  !----------C
  !     PURPOSE: TO POPULATE A DATABASE WITH THE PROGNOSIS MODEL
  !              SPROUTING OUTPUT
  !     AUTH: M. SHETTLES -- FMSC -- AUGUST 2019

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'DBSCOM.f90'
  include 'PLOT.f90'

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

  iRet = fsql3_tableexists(IoutDBref,'FVS_Regen_Sprouts'//CHAR(0))

  !       REGENERATION FROM STUMP & ROOT SPROUTS

  if (iRet.eq.0) then
     SQLStmtStr = 'CREATE TABLE FVS_Regen_Sprouts('//&
          &'CaseID text not null,'//&
          &'StandID text not null,'//&
          &'Year int null,'//&
          &'SpeciesFVS    text null,'//&
          &'SpeciesPLANTS text null,'//&
          &'SpeciesFIA    text null,'//&
          &'SprtTpa int,'//&
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

  write(SQLStmtStr,*)'INSERT INTO FVS_Regen_Sprouts',&
       &' (CaseID,StandID,Year,',&
       &'SpeciesFVS,SpeciesPLANTS,SpeciesFIA,SprtTpa,SprtAveHt)',&
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
