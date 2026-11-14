!====================!
!   dbs : dbsclose   !
!====================!
! DBS $Id$
subroutine DBSCLOSE(LCOUT,LCIN)
  implicit none

  !     PURPOSE: TO CLOSE DATABASE CONNECTION

  ! Common blocks
  include 'DBSCOM.f90'

  ! Variables
  logical :: LCOUT,LCIN
  if (ConnHndlOut.ne.-1.and. LCOUT) then
     !make sure transactions are committed
     iRet = fvsSQLEndTran(SQL_HANDLE_DBC, ConnHndlOut, SQL_COMMIT)

     !disconnect
     iRet = fvsSQLDisconnect(ConnHndlOut)

     !release connection handles
     iRet = fvsSQLFreeHandle(SQL_HANDLE_DBC, ConnHndlOut)
     ConnHndlOut = -1

     !release environment handle
     iRet = fvsSQLFreeHandle(SQL_HANDLE_ENV, EnvHndlOut)
     EnvHndlOut = -1
  end if
  if (ConnHndlIn.ne.-1.and. LCIN) then
     !make sure transactions are committed
     iRet = fvsSQLEndTran(SQL_HANDLE_DBC, ConnHndlIn, SQL_COMMIT)

     !disconnect
     iRet = fvsSQLDisconnect(ConnHndlIn)

     !release connection handles
     iRet = fvsSQLFreeHandle(SQL_HANDLE_DBC, ConnHndlIn)
     ConnHndlIn = -1

     !release environment handle
     iRet = fvsSQLFreeHandle(SQL_HANDLE_ENV, EnvHndlIn)
     EnvHndlIn = -1
  end if

end
