!===================!
!   dbs : dbscase   !
!===================!
! DBS $Id$
subroutine DBSCASE(IFORSURE)
  use FVSVERSION
  implicit none

  !     PURPOSE: TO POPULATE A DATABASE WITH THE PROGNOSIS MODEL
  !              OUTPUT.

  !     INPUT: IFORSURE - 1 NEED CONNECTION, 0 CHECK IF CONNECTION IS
  !                       NEEDED.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'DBSCOM.f90'
  include 'KEYCOM.f90'
  include 'OPCOM.f90'
  include 'OUTCOM.f90'
  include 'PLOT.f90'

  ! Variables
  integer(SQLINTEGER_KIND),parameter:: MaxStringLen = 255

  character(len=2000) :: SQLStmtStr
  character(len=10) :: DATO
  character(len=10) :: REV
  character(len=8) :: TIM
  character(len=32) :: SVN
  character(len=*) :: CFN
  character(len=MaxStringLen) :: TIMESTAMP
  integer :: IFORSURE,IFORSR,I,KODE,IRCODE
  character(len=36) :: CID

  character(len=20) :: TABLENAME

  character(len=2) :: VAR

  SVN = FVSVER_VERSION

  call REVISE(VARACD,REV)
  VAR = VARACD

  !     CHECK TO SEE IF WE ARE NEEDING TO CONTINUE

  IFORSR = IFORSURE
  if (IFORSR.eq.0) then
     if (ISUMARY.ge.1.or.&
          &ICOMPUTE.ge.1.or.&
          &IATRTLIST.ge.1.or.&
          &ITREELIST.ge.1.or.&
          &ICUTLIST.ge.1.or.&
          &IDM1.ge.1.or.IDM2.ge.1.or.IDM3.ge.1.or.&
          &IDM5.ge.1.or.IDM6.ge.1.or.&
          &IPOTFIRE.ge.1.or.&
          &IFUELS.ge.1.or.&
          &ICMRPT.ge.1.or.&
          &ICHRPT.ge.1.or.&
          &ISTRCLAS.ge.1.or.&
          &IFUELC.ge.1.or.&
          &IBURN.ge.1.or.&
          &IMORTF.ge.1.or.&
          &ISSUM.ge.1.or.&
          &ISDET.ge.1.or.&
          &ICANPR.ge.1.or.&
          &IDWDVOL.ge.1.or.&
          &IDWDCOV.ge.1.or.&
          &IBMMAIN.ge.1.or.&
          &IBMBKP.ge.1.or.&
          &IBMTREE.ge.1.or.&
          &IBMVOL.ge.1) IFORSR = 1
  end if
  if (IFORSR.eq.0) return

  !     IF ALREADY HAVE A CURRENT CASE NUMBER, JUST BAIL

  if (CASEID.ne."") return

  !     Initialize variables
  !     CREATE DATETIME
  call GRDTIM(DATO,TIM)
  TIMESTAMP = DATO(7:10)//'-'//DATO(1:5)//'-'//TIM

  !     STRIP 3-CHARACTER EXTENSION (IF PRESENT) FROM KEYFNAME

  KEYFNAME = TRIM(KEYFNAME)
  I = INDEX(".KEY",KEYFNAME)
  if (I.gt.0.and. I.eq.LEN_TRIM(KEYFNAME)-3) then
     KEYFNAME = KEYFNAME(1:I-4)
  end if

  !     MAKE SURE WE HAVE AN OPEN CONNECTION

  if (ConnHndlOut.eq.-1) call DBSOPEN(DSNOUT,EnvHndlOut,&
       &ConnHndlOut,DBMSOUT,0,.false.,KODE)

  !     ALLOCATE A STATEMENT HANDLE

  iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
  if (iRet.ne.SQL_SUCCESS .and. iRet.ne. SQL_SUCCESS_WITH_INFO) then
     ICOMPUTE = 0
     ISUMARY = 0
     IATRTLIST = 0
     ITREELIST = 0
     ICUTLIST = 0
     IDM1 = 0
     IDM2 = 0
     IDM3 = 0
     IDM5 = 0
     IDM6 = 0
     IPOTFIRE = 0
     IFUELS = 0
     ICMRPT = 0
     ICHRPT = 0
     ISTRCLAS = 0
     IFUELC = 0
     IBURN = 0
     IMORTF = 0
     ISSUM = 0
     ISDET = 0
     ICANPR = 0
     IDWDVOL = 0
     IDWDCOV = 0
     IBMMAIN = 0
     IBMBKP = 0
     IBMTREE = 0
     IBMVOL = 0

     call  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,&
          &'DBSRun:Connecting to DSN')
     goto 200
  end if

  !     MAKE SURE WE HAVE AN OPEN CONNECTION

  if (ConnHndlOut.eq.-1) call DBSOPEN(DSNOUT,EnvHndlOut,&
       &ConnHndlOut,DBMSOUT,0,.false.,KODE)

  !     CHECK TO SEE IF THE FVS_Cases TABLE EXISTS IN DATBASE

  if (TRIM(DBMSOUT).eq."EXCEL") then
     TABLENAME = '[FVS_Cases$]'
  else if (TRIM(DBMSOUT).eq."ORACLE") then
     TABLENAME = '"FVS_Cases"'
  else
     TABLENAME = 'FVS_Cases'
  end if
  call DBSCKNROWS(IRCODE,TABLENAME,1,TRIM(DBMSOUT).eq.'EXCEL')
  if (IRCODE.eq.2) return
  if (IRCODE.eq.1) then
     if (TRIM(DBMSOUT).eq."ACCESS") then
        SQLStmtStr = "CREATE TABLE FVS_Cases("//&
             &"CaseID Text primary key,"//&
             &"Stand_CN Text null,"//&
             &"StandID Text null,"//&
             &"MgmtID Text null,"//&
             &"RunTitle Text null,"//&
             &"KeywordFile Text null,"//&
             &"SamplingWt double null,"//&
             &"Variant Text null,"//&
             &"Version Text null,"//&
             &"RV Text null,"//&
             &"Groups Text null,"//&
             &"RunDateTime Text null)"
     else if (TRIM(DBMSOUT).eq."EXCEL") then
        SQLStmtStr = "CREATE TABLE FVS_Cases("//&
             &"CaseID Text,"//&
             &"Stand_CN Text,"//&
             &"StandID Text,"//&
             &"MgmtID Text,"//&
             &"RunTitle Text,"//&
             &"KeywordFile Text,"//&
             &"SamplingWt Number,"//&
             &"Variant Text,"//&
             &"Version Text,"//&
             &"RV Text,"//&
             &"Groups Text,"//&
             &"RunDateTime Text);"
     else
        SQLStmtStr = "CREATE TABLE "//TABLENAME//&
             &" (CaseID char(36) primary key,"//&
             &"Stand_CN char(40),"//&
             &"StandID char(26),"//&
             &"MgmtID char(4),"//&
             &"RunTitle char(72),"//&
             &"KeywordFile char(50),"//&
             &"SamplingWt real,"//&
             &"Variant char(2),"//&
             &"Version char(10),"//&
             &"RV char(8),"//&
             &"Groups char(250),"//&
             &"RunDateTime char(19))"
     end if

     iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),&
          &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
     call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
          &'DBSCase:Creating Table: '//trim(SQLStmtStr))
     !Close Cursor
     iRet = fvsSQLCloseCursor(StmtHndlOut)

  end if

  !     CREATE ENTRY FROM DATA FOR FVSRUN TABLE

  call UUIDGEN(CASEID)

  !           MAKE SURE WE DO NOT EXCEED THE MAX TABLE SIZE IN EXCEL

  if (LENSLS.eq.-1) SLSET = ""
  if (KEYFNAME.eq.' ') KEYFNAME = 'Unknown'
  write(SQLStmtStr,*)"INSERT INTO ",trim(TABLENAME),&
       &" (CaseID,Stand_CN,StandID,MgmtID,RunTitle,KeywordFile,",&
       &"SamplingWt,Variant,Version,RV,Groups,RunDateTime) ",&
       &"VALUES('",CASEID,"','",&
       &TRIM(ADJUSTL(DBCN)),"','",TRIM(ADJUSTL(NPLT)),"','",&
       &TRIM(ADJUSTL(MGMID)),"','",TRIM(ADJUSTL(ITITLE)),"','",&
       &TRIM(ADJUSTL(KEYFNAME)),"',",SAMWT,",'",VAR,"',",&
       &"'",SVN,"','",REV,"','",&
       &TRIM(ADJUSTL(SLSET)),"','",TRIM(ADJUSTL(TIMESTAMP)),"')"
  !Close Cursor
  iRet = fvsSQLCloseCursor(StmtHndlOut)

  !Execute Query
  iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),&
       &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
  call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
       &'DBSCase:Inserting Row: '//trim(SQLStmtStr))

  !Release statement handle
  200 continue
  iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)
  return

  !     CALLED BY FILOPN: ENTRY TO SAVE THE KEYWORD FILE NAME AND TO SET
  !                       THE DEFAULT DBS CONNECTIONS

  entry DBSVKFN (CFN)
  I = LEN_TRIM(CFN)
  if (I.gt.LEN(KEYFNAME)) then
     KEYFNAME = CFN(1:4)//'...'//CFN(I+8-LEN(KEYFNAME):)
  else
     KEYFNAME = CFN
  end if
  return

  !     ENTRY FOR WWPBM, FETCHING CASEID
  !    (NOTE: THE WWPBM NEEDS TO KNOW AND SAVE (INTERNALLY) CASEID, BECAUSE
  !     IT IS DOING ITS DB-WRITING FROM WITHIN ITS OWN INTERNAL STAND LOOP

  entry DBSWW2(CID)
  CID = CASEID
  return

end
