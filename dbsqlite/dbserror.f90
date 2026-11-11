!=========================!
!   dbsqlite : dbserror   !
!=========================!
subroutine DBSERROR(NPLT,CMSG)
  implicit none

  ! DBSQLITE

  !     PURPOSE: CREATE AND POPULATE A DATABASE TABLE WITH WARNING AND
  !              ERROR MESSAGES.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'DBSCOM.f90'

  ! Variables
  integer :: iRet
  character(len=2000) :: SQLStmtStr
  character(len=*) :: NPLT
  character(len=*) :: CMSG

  !OMMONS END

  integer :: fsql3_tableexists,fsql3_exec,fsql3_step,fsql3_prepare,fsql3_finalize

  call DBSCASE(1)

  !     DEFINE TABLENAME

  iRet = fsql3_tableexists(IoutDBref,'FVS_Error'//CHAR(0))

  if (iRet.eq.0) then

     !       TABLE CREATION

     SQLStmtStr = 'CREATE TABLE FVS_Error('//&
          &'CaseID text not null,'//&
          &'StandID text not null,'//&
          &'Message text);'//CHAR(0)

     iRet = fsql3_exec(IoutDBref,SQLStmtStr)
  end if

  !     INSERT RECORD

  write(SQLStmtStr,*)'INSERT INTO FVS_Error',&
       &' (CaseID,StandID,Message)',&
       &'VALUES(''',CASEID,''',''',TRIM(NPLT),''',',&
       &'''',CMSG,''');'
  iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))

  iRet = fsql3_step(IoutDBref)
  iRet = fsql3_finalize(IoutDBref)
  return
end
