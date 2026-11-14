!======================!
!   dbs : dbsexecsql   !
!======================!
! DBS $Id$
subroutine DBSEXECSQL (SQLCMD,ConnHndl,LSCHED,IRC)
  implicit none

  !     AUTH: D. GAMMEL -- RMRS -- MAY 2003
  !     PURPOSE: EXECUTES USER DEFINED QUERIES UPON OPENED
  !              DATABASE CONNECTIONS
  !     INPUT: SQLCMD   - SQL COMMAND QUERY TO BE EXECUTED
  !            ConnHndl - THE CONNECTION HANDLE TO THE DBMS
  !            LSCHED   - SPECIFIES WHETHER THIS IS COMING FROM THE EVENT
  !                       MONITOR

  ! Common blocks
  include  'PRGPRM.f90'

  include  'CONTRL.f90'

  include  'OPCOM.f90'

  include  'PLOT.f90'

  include  'DBSCOM.f90'

  ! Variables
  character(len=*) :: SQLCMD
  integer(SQLSMALLINT_KIND) :: ColumnCount,MxNameLen,ColNumber,NameLen
  parameter (MxNameLen = 20)
  character(LEN = MxNameLen) :: ColName
  integer(SQLLEN_KIND):: NumAttrPtr
  integer(SQLINTEGER_KIND)::LI(MXTST5)
  integer(SQLHDBC_KIND):: ConnHndl
  integer(SQLHSTMT_KIND):: StmtHndl
  integer :: KODE,I,INDX(MXTST5),IBound,IOPKD
  real    :: SQLDATA(MXTST5)
  logical :: LSCHED
  integer :: IRC

  IRC = 1

  !     MAKE SURE WE HAVE AN OPEN CONNECTION
  if (ConnHndl.eq.-1) return

  !     PARSE OUT AND REPLACE WITH USER DEFINED AND EVENT MONITOR VAR VALS
  call DBSPRSSQL(SQLCMD,LSCHED,KODE)
  if (KODE.eq.0) then
     !       THERE WAS A PROBLEM IN PARSING THE SQL STATEMENT
     write (JOSTND,110) TRIM(SQLCMD)
     110 format (/'********   ERROR: SQLOUT/SQLIN PARSING FAILED. '&
          &'SQL STMT: ',A)
     call RCDSET(2,.true.)
     return
  end if

  !     ALLOCATE A STATEMENT HANDLE

  iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndl,&
       &StmtHndl)
  if (iRet.ne.SQL_SUCCESS .and.&
       &iRet.ne.SQL_SUCCESS_WITH_INFO) then
     call  DBSDIAGS(SQL_HANDLE_DBC,ConnHndl,&
          &'DBSEXEC:DSN Connection')
  else
     !       EXECUTE QUERY
     iRet = fvsSQLExecDirect(StmtHndl,trim(SQLCMD),&
          &int(len_trim(SQLCMD),SQLINTEGER_KIND))
     if (iRet.ne.SQL_SUCCESS .and.&
          &iRet.ne.SQL_SUCCESS_WITH_INFO) then
        call DBSDIAGS(SQL_HANDLE_STMT,StmtHndl,&
             &'DBSEXEC:Executing SQL KeyWrd: '//trim(SQLCMD))
        write (JOSTND,120) trim(SQLCMD)
        120 format ('SQL STMT: ',A)
        goto 500
     end if

     !       IF THIS IS A SELECT QUERY THEN MAKE NEEDED UPDATES TO VARIABLES

     iRet = fvsSQLNumResultCols(StmtHndl,ColumnCount)

     if (ColumnCount.gt.0) then

        !         DETERMINE THE KEWORD AND EDIT THE APPROPRIATE VARIABLE

        IBound = 0
        do ColNumber = 1,ColumnCount
           !           GET COLUMN NAME
           iRet = fvsSQLColAttribute(StmtHndl, ColNumber,&
                &SQL_DESC_NAME, ColName,&
                &MxNameLen,NameLen,NumAttrPtr)

           !          UPPER CASE THE COLUMN NAME FOR EASIER COMPARISONS

           do I = 1, NameLen
              call UPCASE(ColName(I:I))
           end do
           if (NameLen .lt. MxNameLen) ColName(NameLen+1:) = ""
           IOPKD = 0
           do I = 1,ITST5
              if (ColName(:8).eq.CTSTV5(I)) then
                 IOPKD = I
                 exit
              end if
           end do
           if (IOPKD.eq.0) then
              if (ITST5.lt.MXTST5) then
                 ITST5 = ITST5+1
                 CTSTV5(ITST5) = ColName(:8)
                 LTSTV5(ITST5) = .false.
                 IOPKD = ITST5
              end if
           end if

           if (IOPKD.gt.0) then
              IBound = IBound + 1
              LI(IBound) = SQL_NULL_DATA
              !             FOUND THE EVMON VARIABLE INDEX
              INDX(IBound) = IOPKD
              !             THE VALUE IS DEFINED SO BIND VALUE TO SQL DATA ARRAY
              iRet = fvsSQLBindCol (StmtHndl,ColNumber,SQL_F_FLOAT,&
                   &SQLDATA(IBound),int(4,SQLLEN_KIND),LI(IBound))
           end if
        end do
        !         FETCH THE FIRST ROW OF DATA FROM THE QUERY
        iRet = fvsSQLFetch (StmtHndl)
        if (iRet.eq.SQL_SUCCESS .or.&
             &iRet.eq.SQL_SUCCESS_WITH_INFO) then
           do I = 1, IBound
              !              IF THE FETCH RETURNED NULL THEN SET EVMON VAR TO UNDEFINED
              if (LI(I).eq.SQL_NULL_DATA) then
                 TSTV5(INDX(I)) = 0
                 LTSTV5(INDX(I)) = .false.
              else
                 TSTV5(INDX(I)) = SQLDATA(I)
                 LTSTV5(INDX(I)) = .true.
              end if
              IRC = 0
           end do
        end if
     else
        IRC = 0
     end if
  end if
  !make sure transactions are committed
  iRet = fvsSQLEndTran(SQL_HANDLE_DBC, ConnHndl, SQL_COMMIT)

  500 continue
  !     FREE HANDLE BEFORE LEAVING
  iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndl)
  return
end
