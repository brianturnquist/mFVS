!===================!
!   dbs : dbsopen   !
!===================!
! DBS $Id$
subroutine DBSOPEN(CONNECT,EnvHndl,ConnHndl,DBMS,JOSTND,&
     &LKECHO,KODE)
  implicit none

  !     PURPOSE: TO OPEN A DATABASE CONNECTION
  !     INPUT: CONNECT  - THE INPUT CONNECTION STRING (or file name)
  !            EnvHndl  - ENVIRONMENT HANDLE
  !            ConnHndl - CONNECTION HANDLE
  !            DBMS     - DATABASE MANAGEMENT SYSTEM NAME
  !            JOSTND   - OUTPUT FILE REFERENCE NUMBER
  !            LKECHO   - TRUE IF KEYWORD OUTPUT IS BEING OUTPUT
  !            KODE     - RETURN CODE 0: CONNECTION UN-SUCCESSFUL
  !                                   1: CONNECTION SUCCESSFUL

  ! Common blocks
  include 'DBSCOM.f90'

  ! Variables
  integer(SQLHENV_KIND):: EnvHndl
  integer(SQLHDBC_KIND):: ConnHndl
  integer(SQLSMALLINT_KIND)::ConnStrLength,ConnStrLengthOut,LenDBMS
  integer(SQLSMALLINT_KIND),parameter::LenConnStr = 500
  integer(SQLSMALLINT_KIND)::DriverComplete
  integer :: JOSTND,I,KODE
  character(LEN = *) :: CONNECT
  character(LEN = *) :: DBMS
  character(LEN = LEN_TRIM(CONNECT)) :: SOURCE
  character(LEN = LenConnStr) :: ConnStr
  character(LEN = LenConnStr) :: ConnStrOut
  character(len=256) :: DSN
  character(len=40) :: USER
  character(len=40) :: PSSWRD
  character(len=40) :: SUFFIX
  character(LEN = 10) :: ConnPrefix
  logical :: LKECHO

  !     CLOSE ANY OPEN CONNECTION

  if (ConnHndl.ne.-1.and. ConnHndl.eq.ConnHndlOut)&
       &call DBSCLOSE(.true.,.false.)
  if (ConnHndl.ne.-1.and. ConnHndl.eq.ConnHndlIn)&
       &call DBSCLOSE(.false.,.true.)
  KODE = 0

  !     ALLOCATE AND ENVIRONMENT HANDLE

  iRet = fvsSQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, EnvHndl)

  !     SET ODBC VERSION TO USE 3.X

  iRet = fvsSQLSetEnvAttr(EnvHndl, SQL_ATTR_ODBC_VERSION,&
       &SQL_OV_ODBC3)

  !     ALLOCATE A CONNECTION HANDLE

  iRet = fvsSQLAllocHandle(SQL_HANDLE_DBC,EnvHndl, ConnHndl)

  !     ENABLE AUTOCOMMIT ATTRIBUTE

  iRet = fvsSQLSetConnectAttr(ConnHndl,SQL_AUTOCOMMIT,&
       &SQL_AUTOCOMMIT_ON,SQLINTEGER_KIND)
  iRet = fvsSQLSetConnectAttr (ConnHndl, SQL_ACCESS_MODE,&
       &SQL_MODE_READ_WRITE,SQLINTEGER_KIND)

  !     If the first part CONNECT is "DRIVER=", "DSN=", or "FILEDSN=",
  !     then this is considered a full connection string.
  !     Jump directly to the DB connection.

  ConnPrefix = CONNECT(1:10)
  do I = 1,len_trim(ConnPrefix)
     call upcase(ConnPrefix(I:I))
  end do
  if (&
       &(ConnPrefix(1:7).eq."DRIVER=")&
       &.or.(ConnPrefix(1:4).eq."DSN=")&
       &.or.(ConnPrefix(1:8).eq."FILEDSN=")&
       &) then
     ConnStr = CONNECT
     DriverComplete = SQL_DRIVER_NOPROMPT
     goto 10
  end if

  !     PARSE OUT NEEDED DATA FROM DSNOUT CHECKING FOR QUOTED STRINGS

  SOURCE = TRIM(CONNECT)
  PSSWRD = ' '
  USER = ' '
  if (SOURCE(1:1).eq.CHAR(34)) then
     SOURCE = TRIM(SOURCE(2:))
     call DBSPRS(DSN,SOURCE,CHAR(34))
  else
     call DBSPRS(DSN,SOURCE,' ')
  end if
  if (SOURCE(1:1).eq.CHAR(34)) then
     SOURCE = TRIM(SOURCE(2:))
     call DBSPRS(USER,SOURCE,CHAR(34))
  else
     call DBSPRS(USER,SOURCE,' ')
  end if
  if (SOURCE(1:1).eq.CHAR(34)) then
     SOURCE = TRIM(SOURCE(2:))
     call DBSPRS(PSSWRD,SOURCE,CHAR(34))
  else
     call DBSPRS(PSSWRD,SOURCE,' ')
  end if

  !     IF THERE IS A FILE NAME SUFFIX, GET IT (LOAD INTO SUFFIX)

  SUFFIX = ' '
  do I = LEN_TRIM(DSN),1,-1
     if (DSN(I:I).eq.'.') then
        SUFFIX = DSN(I+1:)
        exit
     end if
  end do
  ConnStr = ' '
  if (SUFFIX.eq.' ') then
     ConnStr = 'DSN='//TRIM(DSN)//';UID='//TRIM(USER)//&
          &';PWD='//TRIM(PSSWRD)
     DriverComplete = SQL_DRIVER_COMPLETE
  else
     do I = 1,LEN_TRIM(SUFFIX)
        call UPCASE(SUFFIX(I:I))
     end do
     if (DSN(2:2).ne.':') then
        call GETCWD(ConnStr)
        DSN = TRIM(ConnStr)//"/"//TRIM(DSN)
     end if
     if (SUFFIX.eq.'DB'.or.SUFFIX.eq.'SQLITE') then
        ConnStr = 'DRIVER=SQLite3 ODBC Driver'//&
             &';Database='//TRIM(DSN)//&
             &';Version=3;LongNames=0;Timeout=1000;NoTXN=0'//&
             &';SyncPragma=OFF;StepAPI=0;NoWCHAR=1'
     else
        if (SUFFIX.eq.'XLS') then
           ConnStr = 'DRIVER={Microsoft Excel Driver (*.xls)}'//&
                &';ReadOnly=False'
        else if (SUFFIX.eq.'XLSX') then
           ConnStr = 'DRIVER={Microsoft Excel Driver (*.xls, '//&
                &'*.xlsx, *.xlsm, *.xlsb)}'//&
                &';ReadOnly=False'
        else if (SUFFIX.eq.'MDB'.or.SUFFIX.eq.'ACCDB') then
           ConnStr = 'DRIVER={Microsoft Access Driver (*.mdb, '//&
                &'*.accdb)}'
        end if
        if (ConnStr.ne.' ') then
           ConnStr = trim(ConnStr)//';DBQ='//TRIM(DSN)//&
                &';UID='//TRIM(USER)//&
                &';PWD='//TRIM(PSSWRD)
        end if
     end if
     DriverComplete = SQL_DRIVER_NOPROMPT
  end if
  10 continue
  ConnStrLength = LEN_TRIM(ConnStr)
  if (ConnStrLength.gt.0) then
     iRet = fvsSQLDriverConnect(ConnHndl, SQL_NULL_PTR,&
          &ConnStr,ConnStrLength,ConnStrOut,LenConnStr,&
          &ConnStrLengthOut,DriverComplete)

     !       MAKE SURE WE CONNECTED OK

     if (iRet.ne.SQL_SUCCESS.and.iRet.ne.SQL_SUCCESS_WITH_INFO) then
        print *,"Data base connection error using connection string:"
        print *,ConnStr(:ConnStrLength)
        write(JOSTND,20) ConnStr(:ConnStrLength)
        call  DBSDIAGS(SQL_HANDLE_DBC,ConnHndl,&
             &'DBSOPEN: Connection error.')
        return
     else
        if (LKECHO) then
           write(JOSTND,20) ConnStr(:ConnStrLength)
           20 format(T12,'ODBC CONNECT STRING: ',A)
        end if
     end if

  else
     print *,"Data base connection error: connection string ",&
          &"could not be formed."
     return
  end if

  !     CONNECTION SUCCESSFULLY OPENED

  KODE = 1

  !     DETERMINE THE DBMS NAME

  iRet = fvsSQLGetInfo(ConnHndl,SQL_DBMS_NAME,DBMS,&
       &int(LEN(DBMS),SQLSMALLINT_KIND),&
       &LenDBMS)
  DBMS(LenDBMS+1:) = ' '

  !     SET CONNECTION ATTRIBUTES TO ALLOW READ/WRITE
  !      (USUALLLY THIS IS SET BEFORE THE CONNECTION IS ESTABLISHED
  !      BUT THERE SEEMS TO BE A BUG IN THE EXCEL ODBC DRIVER THAT
  !      DOES NOT RECOGNIZE THIS SETTING IF DONE BEFORE CONNECTION)

  if (DBMS.eq.'EXCEL') then
     iRet = fvsSQLSetConnectAttr (ConnHndl, SQL_ACCESS_MODE,&
          &SQL_MODE_READ_WRITE,int(4,SQLINTEGER_KIND))
  end if

end
