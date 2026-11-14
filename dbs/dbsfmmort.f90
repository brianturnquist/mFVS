!=====================!
!   dbs : dbsfmmort   !
!=====================!
! DBS $Id$
subroutine DBSFMMORT(IYEAR,KILLED,TOTAL,BAKILL,&
     &VOKILL,KODE)
  implicit none

  !     PURPOSE: TO POPULATE A DATABASE WITH THE MORTALITY REPORT
  !              INFORMATION
  !     AUTH: S. REBAIN -- FMSC -- DECEMBER 2004
  !     INPUT:
  !              THE MORTALITY OUTPUT FROM THE FIRE MODEL.
  !              1: KILLED TREES PER ACRE FOR EACH SIZE CLASS
  !              2: TOTAL TREES PER ACRE FOR EACH SIZE CLASS
  !              3: MORTALITY IN TERMS OF BASAL AREA
  !              4: MORTALITY IN TERMS OF CUFT VOLUME
  !              5: KODE FOR WHETHER THE REPORT ALSO DUMPS TO FILE

  ! Common blocks
  include 'PRGPRM.f90'
  include 'DBSCOM.f90'
  include 'PLOT.f90'

  ! Variables
  integer :: MXSP1
  parameter (MXSP1 = MAXSP + 1)
  integer :: IYEAR,IRCODE,KODE,I,J
  integer(SQLSMALLINT_KIND)::ColNumber
  real    :: KILLED,TOTAL,BAKILL,VOKILL
  double precision :: KILLEDB,TOTALB,BAKILLB,VOKILLB
  dimension KILLED(MXSP1,8),TOTAL(MXSP1,8),KILLEDB(MXSP1,8),&
       &TOTALB(MXSP1,8)
  dimension BAKILL(MXSP1),VOKILL(MXSP1),BAKILLB(MXSP1),&
       &VOKILLB(MXSP1)
  character(len=2000) :: SQLStmtStr
  character(len=20) :: TABLENAME
  character(len=3) :: CSP
  character(LEN = 8) :: CSPECIES

  !     Initialize variables

  if (IMORTF.eq.0) return
  if (IMORTF.eq.2) KODE = 0

  !     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

  call DBSCASE(1)

  !     ALLOCATE A STATEMENT HANDLE

  iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
  if (iRet.ne.SQL_SUCCESS .and. iRet.ne. SQL_SUCCESS_WITH_INFO) then
     IMORTF = 0
     print *,'Error connecting to data source'
     call  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,&
          &'DBSFMMORT:DSN Connection')
     goto 200
  end if

  !     CHECK TO SEE IF THE MORTALITY TABLE EXISTS IN DATBASE

  if (TRIM(DBMSOUT).eq."EXCEL") then
     TABLENAME = '[FVS_Mortality$]'
  else
     TABLENAME = 'FVS_Mortality'
  end if
  call DBSCKNROWS(IRCODE,TABLENAME,1,TRIM(DBMSOUT).eq.'EXCEL')
  if (IRCODE.eq.2) then
     IMORTF = 0
     return
  end if
  if (IRCODE.eq.1) then
     if (TRIM(DBMSOUT).eq."ACCESS") then
        SQLStmtStr = 'CREATE TABLE FVS_Mortality('//&
             &'CaseID Text not null,'//&
             &'StandID Text null,'//&
             &'Year Int null,'//&
             &'Species Text null,'//&
             &'Killed_class1 double null,'//&
             &'Total_class1 double null,'//&
             &'Killed_class2 double null,'//&
             &'Total_class2 double null,'//&
             &'Killed_class3 double null,'//&
             &'Total_class3 double null,'//&
             &'Killed_class4 double null,'//&
             &'Total_class4 double null,'//&
             &'Killed_class5 double null,'//&
             &'Total_class5 double null,'//&
             &'Killed_class6 double null,'//&
             &'Total_class6 double null,'//&
             &'Killed_class7 double null,'//&
             &'Total_class7 double null,'//&
             &'Bakill double null,'//&
             &'Volkill double null)'

     else if (TRIM(DBMSOUT).eq."EXCEL") then
        SQLStmtStr = 'CREATE TABLE FVS_Mortality('//&
             &'CaseID Text,'//&
             &'StandID Text,'//&
             &'Year Int,'//&
             &'Species Text,'//&
             &'Killed_class1 Number,'//&
             &'Total_class1 Number,'//&
             &'Killed_class2 Number,'//&
             &'Total_class2 Number,'//&
             &'Killed_class3 Number,'//&
             &'Total_class3 Number,'//&
             &'Killed_class4 Number,'//&
             &'Total_class4 Number,'//&
             &'Killed_class5 Number,'//&
             &'Total_class5 Number,'//&
             &'Killed_class6 Number,'//&
             &'Total_class6 Number,'//&
             &'Killed_class7 Number,'//&
             &'Total_class7 Number,'//&
             &'Bakill Number,'//&
             &'Volkill Number)'
     else
        SQLStmtStr = 'CREATE TABLE FVS_Mortality('//&
             &'CaseID char(36) not null,'//&
             &'StandID char(26) not null,'//&
             &'Year Int null,'//&
             &'Species char(3) null,'//&
             &'Killed_class1 real null,'//&
             &'Total_class1 real null,'//&
             &'Killed_class2 real null,'//&
             &'Total_class2 real null,'//&
             &'Killed_class3 real null,'//&
             &'Total_class3 real null,'//&
             &'Killed_class4 real null,'//&
             &'Total_class4 real null,'//&
             &'Killed_class5 real null,'//&
             &'Total_class5 real null,'//&
             &'Killed_class6 real null,'//&
             &'Total_class6 real null,'//&
             &'Killed_class7 real null,'//&
             &'Total_class7 real null,'//&
             &'Bakill real null,'//&
             &'Volkill real null)'
     end if
     !PRINT*, SQLStmtStr

     !Close Cursor
     iRet = fvsSQLCloseCursor(StmtHndlOut)

     iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),&
          &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
     call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
          &'FMMORT:Creating Table: '//trim(SQLStmtStr))
  end if

  do J = 1,MXSP1

     !! this is where the defect occurs...
     if (J .eq. MXSP1) then
        CSP = 'ALL'
     else
        CSP = JSP(J)(1:3)
     end if

     !       ONLY WRITE INFO FOR SPECIES IN THE STAND

     if (TOTAL(J,8) .le.0) goto 150

     !       DETERMINE PREFERED OUTPUT FORMAT FOR SPECIES CODE
     !       KEYWORD OVER RIDES

     !! write out individual species
     if ( J .lt. MXSP1 ) then

        if (JSPIN(J).eq.1) then
           CSPECIES = ADJUSTL(JSP(J))
        else if (JSPIN(J).eq.2) then
           CSPECIES = ADJUSTL(FIAJSP(J))
        else if (JSPIN(J).eq.3) then
           CSPECIES = ADJUSTL(PLNJSP(J))
        else
           CSPECIES = ADJUSTL(PLNJSP(J))
        end if

        if (ISPOUT21.eq.1)CSPECIES = ADJUSTL(JSP(J))
        if (ISPOUT21.eq.2)CSPECIES = ADJUSTL(FIAJSP(J))
        if (ISPOUT21.eq.3)CSPECIES = ADJUSTL(PLNJSP(J))

     else

        if (CSP.eq.'ALL') then
           CSPECIES = 'ALL'
        end if

     end if

     !       ASSIGN REAL VALUES TO DOUBLE PRECISION VARS

     BAKILLB(J) = BAKILL(J)
     VOKILLB(J) = VOKILL(J)

     do I = 1,8
        KILLEDB(J,I) = KILLED(J,I)
        TOTALB(J,I) = TOTAL(J,I)
     end do

     write(SQLStmtStr,*)'INSERT INTO ',TABLENAME,' (CaseID,',&
          &'StandID,Year,Species,Killed_class1,Total_class1,',&
          &'Killed_class2,',&
          &'Total_class2,Killed_class3,Total_class3,Killed_class4,',&
          &'Total_class4,Killed_class5,Total_class5,Killed_class6,',&
          &'Total_class6,Killed_class7,Total_class7,Bakill,Volkill)',&
          &'VALUES(''',CASEID,''',''',TRIM(NPLT),''',?,''',&
          &TRIM(CSPECIES),''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'

     !PRINT*, SQLStmtStr

     !       CLOSE CURSOR

     iRet = fvsSQLCloseCursor(StmtHndlOut)

     !       PREPARE THE SQL QUERY

     iRet = fvsSQLPrepare(StmtHndlOut, trim(SQLStmtStr),&
          &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

     !       BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

     ColNumber = 1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
          &&
          &SQL_PARAM_INPUT,&
          &SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
          &INT(0,SQLSMALLINT_KIND),IYEAR,int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
          &SQL_PARAM_INPUT,&
          &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
          &INT(5,SQLSMALLINT_KIND),KILLEDB(J,1),int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
          &SQL_PARAM_INPUT,&
          &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
          &INT(5,SQLSMALLINT_KIND),TOTALB(J,1),int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
          &SQL_PARAM_INPUT,&
          &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
          &INT(5,SQLSMALLINT_KIND),KILLEDB(J,2),int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
          &SQL_PARAM_INPUT,&
          &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
          &INT(5,SQLSMALLINT_KIND),TOTALB(J,2),int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
          &SQL_PARAM_INPUT,&
          &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
          &INT(5,SQLSMALLINT_KIND),KILLEDB(J,3),int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
          &SQL_PARAM_INPUT,&
          &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
          &INT(5,SQLSMALLINT_KIND),TOTALB(J,3),int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
          &SQL_PARAM_INPUT,&
          &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
          &INT(5,SQLSMALLINT_KIND),KILLEDB(J,4),int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
          &SQL_PARAM_INPUT,&
          &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
          &INT(5,SQLSMALLINT_KIND),TOTALB(J,4),int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
          &SQL_PARAM_INPUT,&
          &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
          &INT(5,SQLSMALLINT_KIND),KILLEDB(J,5),int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
          &SQL_PARAM_INPUT,&
          &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
          &INT(5,SQLSMALLINT_KIND),TOTALB(J,5),int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
          &SQL_PARAM_INPUT,&
          &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
          &INT(5,SQLSMALLINT_KIND),KILLEDB(J,6),int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
          &SQL_PARAM_INPUT,&
          &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
          &INT(5,SQLSMALLINT_KIND),TOTALB(J,6),int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
          &SQL_PARAM_INPUT,&
          &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
          &INT(5,SQLSMALLINT_KIND),KILLEDB(J,7),int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
          &SQL_PARAM_INPUT,&
          &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
          &INT(5,SQLSMALLINT_KIND),TOTALB(J,7),int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
          &SQL_PARAM_INPUT,&
          &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
          &INT(5,SQLSMALLINT_KIND),BAKILLB(J),int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
          &SQL_PARAM_INPUT,&
          &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
          &INT(0,SQLSMALLINT_KIND),VOKILLB(J),int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     !Close Cursor
     iRet = fvsSQLCloseCursor(StmtHndlOut)

     iRet = fvsSQLExecute(StmtHndlOut)
     call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
          &'FMMORT:Inserting Row')

     150 continue
  end do

  200 continue
  !Release statement handle
  iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

end
