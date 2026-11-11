!====================!
!   dbs : dbsdiags   !
!====================!
! DBS $Id$
subroutine DBSDIAGS(HndlType,Hndl,CallFrom)
  implicit none

  !      THIS SUBROUTINE PRINTS ERROR DIAGNOSTICS FOR THE DBS
  !      CALLS TO THE SQL ODBC API

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'DBSCOM.f90'

  ! Variables
  integer(SQLHANDLE_KIND)::Hndl
  integer(SQLSMALLINT_KIND)::HndlType

  character(LEN = 6):: SqlState
  character(LEN = *):: CallFrom
  character(LEN = SQL_MAX_MESSAGE_LENGTH)::Msg
  integer(SQLINTEGER_KIND)::NativeError
  integer(SQLSMALLINT_KIND):: iDiag,MsgLen
  integer(SQLRETURN_KIND):: DiagRet

  iDiag = 1
  do while (.true..and. iDiag.le.5)
     DiagRet = fvsSQLGetDiagRec(loc(HndlType), Hndl, iDiag,&
          &SqlState, NativeError, Msg,&
          &SQL_MAX_MESSAGE_LENGTH,&
          &MsgLen)
     if (DiagRet.eq.SQL_NO_DATA) exit
     if (DiagRet.ne.SQL_SUCCESS.and.DiagRet.ne.&
          &SQL_SUCCESS_WITH_INFO) then

        exit
     end if
     write(JOSTND,100)TRIM(CallFrom),NativeError,Msg(1:MsgLen)
     100 format(/'DBS Error: ',A/'Native error number= ',I16/&
          &'Message text: ',A)

     call RCDSET (2,.true.)

     iDiag = iDiag+1

  end do
  return
end
