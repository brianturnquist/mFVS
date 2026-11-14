!===================!
!   dbs : dbscmpu   !
!===================!
! DBS $Id$
subroutine DBSCMPU
  implicit none

  !     AUTH: D. GAMMEL -- SEM -- JUNE 2002
  !     PURPOSE: TO POPULATE A DATABASE WITH THE COMPUTE TABLE
  !              INFORMATION

  ! Common blocks
  include 'PRGPRM.f90'
  include 'OPCOM.f90'
  include 'CONTRL.f90'
  include 'KEYCOM.f90'
  include 'DBSCOM.f90'
  include 'PLOT.f90'

  ! Variables
  character(len=10000) :: SQLStmtStr
  character(len=10000) :: TABLESTR
  character(len=20) :: TABLENAME
  character(len=20) :: DTYPE
  character(len=20) :: COLNAME
  character(len=8) :: KEYWRD
  character(LEN = 1) :: LWRAP
  character(LEN = 1) :: RWRAP
  character(LEN = 8),dimension (MXTST5)::KWLIST,KWINSRT
  real,dimension(MXTST5)::KWVALS
  integer :: NUMKW,I,I1,I2,I3,I4,INSRTNUM,II,X,ICY,THISYR,IRCODE
  logical :: KWINLIST,LDUPKW
  integer(SQLUSMALLINT_KIND)::ColNumber
  integer(SQLSMALLINT_KIND)::NameLen,ColumnCount,DTp,NDecs,Nullable
  integer(SQLULEN_KIND) :: NColSz
  NUMKW = ITST5

  !     IF COMPUTE IS NOT TURNED ON OR THE NUMBER OF VARIABLES IS 0
  !     THEN RETURN

  !*      print *,"dbscmpu - NUMKW=",NUMKW
  if (ICOMPUTE.eq.0.or.NUMKW.eq.0) return

  !     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

  call DBSCASE(1)

  if (TRIM(DBMSOUT).eq.'EXCEL') then
     TABLENAME = '[FVS_Compute$]'
     DTYPE = 'Number'
     LWRAP = '['
     RWRAP = ']'
  else if (TRIM(DBMSOUT).eq.'ACCESS') then
     TABLENAME = 'FVS_Compute'
     DTYPE = 'Double'
     LWRAP = '['
     RWRAP = ']'
  else if (TRIM(DBMSOUT).eq.'ORACLE') then
     TABLENAME = 'FVS_Compute'
     DTYPE = 'real'
     LWRAP = '"'
     RWRAP = '"'
  else
     TABLENAME = 'FVS_Compute'
     DTYPE = 'real'
     LWRAP = ' '
     RWRAP = ' '
  end if

  !     ALLOCATE A STATEMENT HANDLE

  iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
  if (iRet.ne.SQL_SUCCESS .and. iRet.ne. SQL_SUCCESS_WITH_INFO) then
     ICOMPUTE = 0
     print *,'Error connecting to data source'
     call  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,&
          &'DBSCMPU:Connecting to DSN')
     goto 10
  end if

  !     CHECK TO SEE IF THE COMPUTE TABLE EXISTS IN DATBASE
  !     IF IT DOES NOT THEN WE NEED TO CREATE IT

  call DBSCKNROWS(IRCODE,TABLENAME,NCYC,TRIM(DBMSOUT).eq.'EXCEL')
  !*      print *,"at DBSCKNROWS - IRCODE=",IRCODE," TABLENAME-",TABLENAME

  if (IRCODE.eq.2) then
     ICOMPUTE = 0
     return
  end if
  if (IRCODE.eq.1) then
     KWLIST = CTSTV5
     if (TRIM(DBMSOUT).eq."EXCEL".or.TRIM(DBMSOUT).eq."ACCESS") then
        TABLESTR = 'CREATE TABLE FVS_Compute('//&
             &'CaseID Text,'//&
             &'StandID Text null,'//&
             &'Year INT null'

        do I = 1,NUMKW
           !CHECK TO SEE IF WE WANT TO SKIP UNDERSCORE COMPUTES
           if (.not.(KWLIST(I)(1:1).eq.'_'.and.I_CMPU.lt.1)) then
              SQLStmtStr = TRIM(TABLESTR)//',['//&
                   &TRIM(KWLIST(I))//'] '//TRIM(DTYPE)//' null'
              TABLESTR = SQLStmtStr
           end if
        end do
     else
        TABLESTR = 'CREATE TABLE FVS_Compute('//&
             &'CaseID char(36) ,'//&
             &'StandID char(26) null,'//&
             &'Year int null'
        do I = 1,NUMKW
           !CHECK TO SEE IF WE WANT TO SKIP UNDERSCORE COMPUTES
           if (.not.(KWLIST(I)(1:1).eq.'_'.and.I_CMPU.lt.1)) then
              SQLStmtStr = TRIM(TABLESTR)//','//LWRAP//&
                   &TRIM(KWLIST(I))//RWRAP//' '//TRIM(DTYPE)//' null'
              TABLESTR = SQLStmtStr
           end if
        end do
     end if

     SQLStmtStr = TRIM(TABLESTR)//')'
     iRet = fvsSQLCloseCursor(StmtHndlOut)
     iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),&
          &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

     call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
          &'DBSCMPU:Creating Table: '//trim(SQLStmtStr))
  else

     !       POPULATE THE KWLIST WITH THE CURRENT COLUMN NAMES
     !       FIRST, FETCH ALL THE COLS SO WE CAN GET THE NAMES

     SQLStmtStr = 'SELECT * FROM FVS_Compute'
     iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),&
          &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
     iRet = fvsSQLNumResultCols(StmtHndlOut,ColumnCount)
     !*        print *,"iRet=",iRet," ColumnCount=",ColumnCount
     do ColNumber = 1,ColumnCount
        iRet = fvsSQLDescribeCol (StmtHndlOut, ColNumber, ColName,&
             &int(LEN(ColName),SQLSMALLINT_KIND), NameLen, DTp,&
             &NColSz, NDecs, Nullable)
        ColName(NameLen+1:) = ' '
        !*          print *,"ColNumber=",ColNumber," iRet=",iRet,
        !*     >    " NameLen=",NameLen," ColName=",ColName
        if (ColNumber.gt.3) KWLIST(ColNumber-3) = ColName(:NameLen)
     end do
     NUMKW = ColumnCount - 3
  end if
  iRet = fvsSQLCloseCursor(StmtHndlOut)

  !     CREATE AND EXECUTE THE INSERT COMMANDS FOR EACH YEAR

  THISYR = -1
  INSRTNUM = 0
  !*      print *,"at INSERT COMMAND - NCYC=",NCYC,
  !*     > "INSRTNUM=",INSRTNUM
  do ICY = 1,NCYC
     I1 = IMGPTS(ICY,1)
     I2 = IMGPTS(ICY,2)
     do II = I1,I2
        I = IOPSRT(II)
        !MAKE SURE WE ARE GRABBING ONLY THE COMPUTES

        if (IACT(I,1).eq.33.and. IACT(I,4).gt.0) then
           if (THISYR.eq.-1) THISYR = IACT(I,4)
           if (IACT(I,4).ne.THISYR) then
              !CHECK TO SEE IF WE HAVE SOMETHING TO INSERT
              if (INSRTNUM.gt.0) then

                 !BUILD THE INSERT QUERY
                 call INSERTCMPU(KWINSRT,KWVALS,THISYR,NPLT,INSRTNUM)

              end if
              INSRTNUM = 0
              THISYR = IACT(I,4)
           end if
           X = IFIX(PARMS(IACT(I,2)+1))
           if (X.gt.500) X = X-500
           KEYWRD = CTSTV5(X)
           !CHECK TO SEE IF WE WANT TO SKIP UNDERSCORE COMPUTES
           if (KEYWRD(1:1).eq.'_'.and.I_CMPU.lt.1) then
              KWINLIST = .true.
           else
              KWINLIST = .false.
              !CHECK TO SEE IF KEYWORD IS IN LIST ALREADY
              do I3 = 1,NUMKW
                 !IF IT IS IN THE LIST THEN ADD THE VALUE TO THE ARRAYS
                 !*                print *,"at LIST CHECK - I3=",I3," KWLIST=",
                 !*     >          TRIM(KWLIST(I3))," KEYWRD=",TRIM(KEYWRD)
                 if (TRIM(KWLIST(I3)).eq.TRIM(KEYWRD)) then
                    !NEED TO CHECK FOR DUPLICATES
                    LDUPKW = .false.
                    do I4 = 1,INSRTNUM
                       if (TRIM(KWINSRT(I4)).eq.TRIM(KEYWRD)) then
                          KWVALS(I4) = PARMS(IACT(I,2))
                          LDUPKW = .true.
                       end if
                    end do
                    !IF NOT A DUPLICATE THEN ADD NEW ENTRY TO ARRAYS
                    if (.not.LDUPKW) then
                       INSRTNUM = INSRTNUM + 1
                       KWINSRT(INSRTNUM) = KEYWRD
                       KWVALS(INSRTNUM) = PARMS(IACT(I,2))
                    end if
                    KWINLIST = .true.
                    exit
                 end if
              end do
           end if

           !IF KW NOT IN LIST THEN DETERMINE IF WE WANT TO ALTER TABLE
           !*            print *,"at ALTER TABLE - KWINLIST=",KWINLIST," NUMKW=",
           !*     >      NUMKW," INSRTNUM=",INSRTNUM," IADDCMPU=",IADDCMPU,
           !*     >      " DBMSOUT=",TRIM(DBMSOUT)

           if ((.not.KWINLIST).and.TRIM(DBMSOUT).ne.'EXCEL'&
                &.and.IADDCMPU.lt.1) then
              SQLStmtStr = 'ALTER TABLE '//trim(TABLENAME)//' ADD '//&
                   &LWRAP//TRIM(KEYWRD)//RWRAP//' '//trim(DTYPE)//' null'
              !Close Cursor
              iRet = fvsSQLCloseCursor(StmtHndlOut)
              iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),&
                   &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
              call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
                   &'DBSCMPU:Alter Table: '//trim(SQLStmtStr))

              !ADD NEW COLUMN TO MASTER LIST
              NUMKW = NUMKW+1
              KWLIST(NUMKW) = KEYWRD
              !ADD THE KEYWORD AND VALUES TO THE SQL ARRAYS
              KWINSRT(INSRTNUM+1) = KEYWRD
              KWVALS(INSRTNUM+1) = PARMS(IACT(I,2))
              INSRTNUM = INSRTNUM + 1
           end if
        end if
     end do
  end do

  !     CHECK TO SEE IF WE HAVE SOMETHING MORE TO INSERT

  if (INSRTNUM.gt.0) then

     !DO NOT INSERT 0 YEAR ENTRIES
     if (.not.THISYR.gt.0)goto 10

     call INSERTCMPU(KWINSRT,KWVALS,THISYR,NPLT,INSRTNUM)

  end if
  10 continue
  !make sure transactions are committed
  iRet = fvsSQLEndTran(SQL_HANDLE_DBC, ConnHndlOut, SQL_COMMIT)
  !Close Cursor
  iRet = fvsSQLCloseCursor(StmtHndlOut)
  !Release statement handle
  iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

end

subroutine INSERTCMPU(KWINSRT,KWVALS,THISYR,STANDID,NUMCMPU)
  implicit none

  !     BUILDCMPUSQL
  !     AUTH: D. GAMMEL -- SEM -- JUNE 2002
  !     PURPOSE: TO POPULATE A DATABASE WITH THE COMPUTE TABLE
  !              INFORMATION

  !     INPUT: KWINSRT - THE ARRAY OF COMPUTE VARS TO INSERT
  !            KWVALS  - THE ARRAY OF VALUES ASSOCIATED W/ THE VARS
  !            NUMCMPU - THE NUMBER OF COMPUTE VARS TO INSERT

  !     RETURN:SQLStmtStr - THE QUERY STRING TO USE FOR THE INSERT

  ! Common blocks
  include 'DBSCOM.f90'

  ! Variables
  integer :: NUMCMPU,X,THISYR
  integer(SQLSMALLINT_KIND)::ColNumber
  character(len=7000) :: SQLStmtStr
  character(len=7000) :: SQLStr1
  character(len=7000) :: SQLStr2
  character(len=20) :: TABLENAME
  character(len=20) :: DTYPE
  character(LEN = 26) :: STANDID
  character(LEN = 1) :: RWRAP
  character(LEN = 1) :: LWRAP
  character(LEN = 8),dimension (NUMCMPU)::KWINSRT
  real,dimension(NUMCMPU)::KWVALS
  double precision,dimension(NUMCMPU)::CURVAL
  select case(TRIM(DBMSOUT))
  case('EXCEL')
     TABLENAME = '[FVS_Compute$]'
     DTYPE = 'Number'
     LWRAP = '['
     RWRAP = ']'
  case('ACCESS')
     TABLENAME = 'FVS_Compute'
     DTYPE = 'Double'
     LWRAP = '['
     RWRAP = ']'
  case('ORACLE')
     TABLENAME = 'FVS_Compute'
     DTYPE = 'real'
     LWRAP = '"'
     RWRAP = '"'
  case default
     TABLENAME = 'FVS_Compute'
     DTYPE = 'real'
     LWRAP = ' '
     RWRAP = ' '
  end select

  !BUILD THE EXECUTE QUERY
  SQLStr1 = 'INSERT INTO '//TABLENAME//&
       &'(CaseID,StandID,Year,'
  do X = 1,NUMCMPU
     SQLStr2 = TRIM(SQLStr1)//LWRAP//TRIM(KWINSRT(X))&
          &//RWRAP//','
     SQLStr1 = TRIM(SQLStr2)
  end do
  SQLStr2 = SQLStr1(1:(LEN_TRIM(SQLStr1)-1))
  write(SQLStr1,*)TRIM(SQLStr2),') VALUES(''',&
       &CASEID,''',''',TRIM(STANDID),''',?,'

  do X = 1,NUMCMPU
     SQLStr2 = TRIM(SQLStr1)//'?,'
     SQLStr1 = TRIM(SQLStr2)
  end do
  SQLStr2 = SQLStr1(1:(LEN(TRIM(SQLStr1))-1))
  SQLStmtStr = TRIM(SQLStr2)//')'

  iRet = fvsSQLCloseCursor(StmtHndlOut)
  iRet = fvsSQLPrepare(StmtHndlOut, trim(SQLStmtStr),&
       &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

  !BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES
  ColNumber = 1
  iRet = fvsSQLBindParameter(StmtHndlOut, ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),THISYR,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  do X = 1,(NUMCMPU)
     CURVAL(X) = KWVALS(X)
     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
          &SQL_PARAM_INPUT,&
          &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
          &INT(5,SQLSMALLINT_KIND),CURVAL(X),int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)
  end do

  iRet = fvsSQLExecute(StmtHndlOut)
  call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
       &'DBSCMPU:Inserting Row, CMD='//trim(SQLStmtStr))

end
