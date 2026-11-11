!===================!
!   dbs : dbsdriv   !
!===================!
! DBS $Id$
subroutine DBSDRIV(JOSTND)
  implicit none

  !     PURPOSE: TO LIST THE ODBC DRIVERS ON THE MACHINE
  !     INPUT: JOSTND  - OUTPUT FILE REFERENCE NUMBER

  ! Common blocks
  include 'DBSCOM.f90'

#if _WIN64
      !DEC$ ATTRIBUTES DLLIMPORT :: FVSSQLSETENVATTR
      !DEC$ ATTRIBUTES DLLIMPORT :: FVSSQLALLOCHANDLE
      !DEC$ ATTRIBUTES DLLIMPORT :: FVSSQLFREEHANDLE
#else
      !DEC$ ATTRIBUTES DLLIMPORT :: _FVSSQLSETENVATTR
      !DEC$ ATTRIBUTES DLLIMPORT :: _FVSSQLALLOCHANDLE
      !DEC$ ATTRIBUTES DLLIMPORT :: _FVSSQLFREEHANDLE

      !DEC$ ATTRIBUTES ALIAS:'_FVSSQLSETENVATTR' :: FVSSQLSETENVATTR
      !DEC$ ATTRIBUTES ALIAS:'_FVSSQLALLOCHANDLE' :: FVSSQLALLOCHANDLE
      !DEC$ ATTRIBUTES ALIAS:'_FVSSQLFREEHANDLE' :: FVSSQLFREEHANDLE
#endif

  ! Variables
  integer(SQLHENV_KIND):: EnvHndl
  integer(SQLSMALLINT_KIND)::DriverComplete
  integer(SQLUSMALLINT_KIND) :: direction
  integer(SQLSMALLINT_KIND) :: driverStrLen,attrStrLen
  integer :: JOSTND,driverCount,i,i1
  character(LEN = 256) :: driver
  character(LEN = 256) :: attr

  driverCount = 0

  !     ALLOCATE AND ENVIRONMENT HANDLE

  iRet = fvsSQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, EnvHndl)
  if (iRet.ne.0) goto 100

  !     SET ODBC VERSION TO USE 3.X

  iRet = fvsSQLSetEnvAttr(EnvHndl, SQL_ATTR_ODBC_VERSION,&
       &SQL_OV_ODBC3)
  if (iRet.ne.0) goto 100

  direction = SQL_FETCH_FIRST
  do
     iRet = fvsSQLDrivers(EnvHndl, direction,&
          &driver, len(driver), driverStrLen,&
          &attr, len(attr), attrStrLen)
     if (iRet.ne.0) exit
     driverCount = driverCount+1
     print *
     print *, "name:",driver(:driverStrLen)
     write (JOSTND,20) driverCount,driver(:driverStrLen)
     20 format(/T12,'DRIVER ',I2,'; NAME: ',A,'; ATTRIBUTES:')
     if (attrStrLen.gt.0) then
        i1 = 1
        do i = 1,attrStrLen
           if (iachar(attr(i:i)).eq.0) then
              print *, "attr:",attr(i1:(i-1))
              write (JOSTND,30) attr(i1:(i-1))
              30 format (T12,A)
              i1 = i+1
           end if
        end do
        if (i1.eq.1) then ! maybe there are no null terminators
           print *, "attr:", attr(:attrStrLen)
           write (JOSTND,30) attr(:attrStrLen)
        end if
     end if
     direction = SQL_FETCH_NEXT
  end do
  iRet = fvsSQLFreeHandle(SQL_HANDLE_ENV, EnvHndl)
  100 continue
  if (driverCount.eq.0) write (JOSTND,110)
  110 format (/T12,I3,'NO DRIVERS FOUND')
  return
end
