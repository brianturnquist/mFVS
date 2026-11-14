!==========================!
!   metric/dbs : dbscuts   !
!==========================!
! METRIC-DBS $Id$
subroutine DBSCUTS(IWHO,KODE)
  implicit none

  !     PURPOSE: TO OUTPUT THE CUTS LIST DATA TO THE DATABASE

  !     AUTH: D. GAMMEL -- RMRS -- JULY 2003

  !     INPUT: IWHO  - THE WHO CALLED ME VALUE WHICH MUST BE 2
  !                     INORDER FOR US TO CONTINUE
  !            KODE  - FOR LETTING CALLING ROUTINE KNOW IF THIS IS A
  !                     REDIRECT OF THE FLAT FILE REPORT OR IN
  !                     ADDITION TO

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'ESTREE.f90'
  include 'VARCOM.f90'
  include 'WORKCM.f90'
  include 'DBSCOM.f90'
  include 'METRIC.f90'

  ! Variables
  character(len=8) :: TID
  character(len=8) :: CSPECIES
  character(len=2000) :: SQLStmtStr
  character(len=20) :: TABLENAME
  character(len=20) :: DTYPE
  integer :: IWHO,I,JYR,IP,ITPLAB,ID,IDMR,ICDF,IBDF,IPTBAL,KODE
  integer :: ISPC,I1,I2,I3,J
  integer*4 :: IDCMP1,IDCMP2
  data IDCMP1,IDCMP2/10000000,20000000/
  real    :: CW,P,CCFT,DGI,DP,Ht2TDCF,Ht2TDBF

  !     IF CUTSOUT IS NOT TURNED ON OR THE IWHO VARIABLE IS NOT 2
  !     THEN JUST RETURN

  if (ICUTLIST.eq.0.or.IWHO.ne.2) return

  !     IS THIS OUTPUT A REDIRECT OF THE REPORT THEN SET KODE TO 0

  if (ICUTLIST.eq.2) KODE = 0

  !     ALWAYS CALL CASE TO MAKE SURE WE HAVE AN UP TO DATE CASE NUMBER

  call DBSCASE(1)

  if (TRIM(DBMSOUT).eq.'EXCEL') then
     TABLENAME = '[FVS_CutList$]'
     DTYPE = 'Number'
  else if (TRIM(DBMSOUT).eq.'ACCESS') then
     TABLENAME = 'FVS_CutList'
     DTYPE = 'Double'
  else
     TABLENAME = 'FVS_CutList'
     DTYPE = 'real'
  end if

  !     ALLOCATE A STATEMENT HANDLE

  iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
  if (iRet.ne.SQL_SUCCESS .and. iRet.ne. SQL_SUCCESS_WITH_INFO) then
     ICUTLIST = 0
     print *,'Error connecting to data source'
     call  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,&
          &'DBSCUTS:Connecting to DSN')
     goto 100
  end if

  !     CHECK TO SEE IF THE CUTS LIST TABLE EXISTS IN DATBASE
  !     IF IT DOESN'T THEN WE NEED TO CREATE IT

  SQLStmtStr = 'SELECT * FROM '//TABLENAME

  iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),&
       &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

  if (iRet.ne.SQL_SUCCESS.and.&
       &iRet.ne.SQL_SUCCESS_WITH_INFO) then
     if (TRIM(DBMSOUT).eq."ACCESS") then
        SQLStmtStr = 'CREATE TABLE FVS_CutList'//&
             &'(Id int primary key,'//&
             &'CaseID int not null,'//&
             &'StandID Text null,'//&
             &'Year int null,'//&
             &'PrdLen int null,'//&
             &'TreeId Text null,'//&
             &'TreeIndex int null,'//&
             &'Species Text null,'//&
             &'TreeVal int null,'//&
             &'SSCD int null,'//&
             &'PtIndex int null,'//&
             &'TPH double null,'//&
             &'MortPH double null,'//&
             &'DBH double null,'//&
             &'DG double null,'//&
             &'Ht double null,'//&
             &'HtG double null,'//&
             &'PctCr int null,'//&
             &'CrWidth double null,'//&
             &'MistCD int null,'//&
             &'BAPctile double null,'//&
             &'PtBAL double null,'//&
             &'TCuM double null,'//&
             &'MCuM double null,'//&
             &'NCuM double null,'//&
             &'MDefect int null,'//&
             &'BDefect int null,'//&
             &'TruncHt int null,'//&
             &'Ht2TDCF real null,'//&
             &'Ht2TDBF real null)'

     else if (TRIM(DBMSOUT).eq."EXCEL") then
        SQLStmtStr = 'CREATE TABLE FVS_CutList'//&
             &'(Id INT null,'//&
             &'CaseID INT null,'//&
             &'StandID Text null,'//&
             &'Year INT null,'//&
             &'PrdLen int null,'//&
             &'TreeId Text null,'//&
             &'TreeIndex int null,'//&
             &'Species Text null,'//&
             &'TreeVal int null,'//&
             &'SSCD int null,'//&
             &'PtIndex int null,'//&
             &'TPH Number null,'//&
             &'MortPH Number null,'//&
             &'DBH Number null,'//&
             &'DG Number null,'//&
             &'Ht Number null,'//&
             &'HtG Number null,'//&
             &'PctCr int null,'//&
             &'CrWidth Number null,'//&
             &'MistCD int null,'//&
             &'BAPctile Number null,'//&
             &'PtBAL Number null,'//&
             &'TCuM Number null,'//&
             &'MCuM Number null,'//&
             &'NCuM Number null,'//&
             &'MDefect int null,'//&
             &'BDefect int null,'//&
             &'TruncHt int null,'//&
             &'Ht2TDCF real null,'//&
             &'Ht2TDBF real null)'
     else
        SQLStmtStr = 'CREATE TABLE FVS_CutList'//&
             &'(Id int primary key,'//&
             &'CaseID int null,'//&
             &'StandID char(26) null,'//&
             &'Year int null,'//&
             &'PrdLen int null,'//&
             &'TreeId char(8) null,'//&
             &'TreeIndex int null,'//&
             &'Species char(3) null,'//&
             &'TreeVal int null,'//&
             &'SSCD int null,'//&
             &'PtIndex int null,'//&
             &'TPH real null,'//&
             &'MortPH real null,'//&
             &'DBH real null,'//&
             &'DG real null,'//&
             &'Ht real null,'//&
             &'HtG real null,'//&
             &'PctCr int null,'//&
             &'CrWidth real null,'//&
             &'MistCD int null,'//&
             &'BAPctile real null,'//&
             &'PtBAL real null,'//&
             &'TCuM real null,'//&
             &'MCuM real null,'//&
             &'NCuM real null,'//&
             &'MDefect int null,'//&
             &'BDefect int null,'//&
             &'TruncHt int null,'//&
             &'Ht2TDCF real null,'//&
             &'Ht2TDBF real null)'
     end if
     iRet = fvsSQLCloseCursor(StmtHndlOut)
     iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),&
          &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
     call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
          &'DBSCUTS:Creating Table: '//trim(SQLStmtStr))
     CUTSID = 0
  end if

  !     SET THE CUTS LIST TYPE FLAG (LET IP BE THE RECORD OUTPUT COUNT).
  !     AND THE OUTPUT REPORTING YEAR.

  JYR = IY(ICYC)
  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (I1.ne.0) then
        I2 = ISCT(ISPC,2)
        do I3 = I1,I2
           IP = 0
           do I = 1,ITRN
              if (WK3(I).gt.0) IP = IP+1
           end do
           I = IND1(I3)
           ITPLAB = 3
           P = WK3(I)/GROSPC
           DP = 0.0
           !           SKIP OUTPUT IF P <= 0
           if (P.le.0.0) goto 50

           !           TRANSLATE TREES IDS FOR TREES THAT HAVE BEEN COMPRESSED OR
           !           GENERATED THROUGH THE ESTAB SYSTEM.

           if (IDTREE(I) .gt. IDCMP1) then
              if (IDTREE(I) .gt. IDCMP2) then
                 write(TID,'(''CM'',I6.6)') IDTREE(I)-IDCMP2
              else
                 write(TID,'(''ES'',I6.6)') IDTREE(I)-IDCMP1
              end if
           else
              write(TID,'(I8)') IDTREE(I)
           end if

           !           GET MISTLETOE RATING FOR CURRENT TREE RECORD.

           call MISGET(I,IDMR)

           !           SET CROWN WIDTH.

           CW = CRWDTH(I)

           !           DECODE DEFECT AND ROUND OFF POINT BAL.

           ICDF = (DEFECT(I)-((DEFECT(I)/10000)*10000))/100
           IBDF = DEFECT(I)-((DEFECT(I)/100)*100)
           IPTBAL = NINT(PTBALT(I)*FT2pACRtoM2pHA)

           !           GET DG INPUT

           DGI = DG(I)

           !           DETERMINE PREFERED OUTPUT FORMAT FOR SPECIES CODE
           !           KEYWORD OVER RIDES

           if (JSPIN(ISP(I)).eq.1) then
              CSPECIES = ADJUSTL(TRIM(JSP(ISP(I))))
           else if (JSPIN(ISP(I)).eq.2) then
              CSPECIES = ADJUSTL(TRIM(FIAJSP(ISP(I))))
           else if (JSPIN(ISP(I)).eq.3) then
              CSPECIES = ADJUSTL(TRIM(PLNJSP(ISP(I))))
           else
              CSPECIES = ADJUSTL(PLNJSP(ISP(I)))
           end if

           if (ISPOUT17.eq.1)CSPECIES = ADJUSTL(TRIM(JSP(ISP(I))))
           if (ISPOUT17.eq.2)CSPECIES = ADJUSTL(TRIM(FIAJSP(ISP(I))))
           if (ISPOUT17.eq.3)CSPECIES = ADJUSTL(TRIM(PLNJSP(ISP(I))))

           !           CREATE ENTRY FROM DATA FOR CUTSLIST TABLE

           if (CUTSID.eq.-1) then
              call DBSGETID(TABLENAME,'Id',ID)
              CUTSID = ID
           end if
           CUTSID = CUTSID + 1

           !           MAKE SURE WE DO NOT EXCEED THE MAX TABLE SIZE IN EXCEL

           if (CUTSID.ge.65535.and.TRIM(DBMSOUT).eq.'EXCEL') goto 100

           WRITE(SQLStmtStr,*)'INSERT INTO ',TABLENAME,'(              &
                &           Id,CaseID,StandID,Year,PrdLen,                         &
                &           TreeId,TreeIndex,Species,TreeVal,SSCD,PtIndex,         &
                &           TPH,MortPH,DBH,DG,                                     &
                &           HT,HTG,PctCr,CrWidth,MistCD,                           &
                &           BAPctile,PtBAL,TCuM,                                   &
                &           MCuM,NCuM,MDefect,BDefect,TruncHt,                     &
                &           Ht2TDCF,Ht2TDBF)                                       &
                &           VALUES(',CUTSID,',',ICASE,',',CHAR(39),&
                &TRIM(NPLT),CHAR(39),',',JYR,',',IFINT,",'",A&
                &DJUSTL(TID),"',",I,",'",ADJUSTL(NSP(ISP(I),1)(1:2)),&
                &"',",IMC(I),',',ISPECL(I),',',ITRE(I),&
                &',',P/ACRtoHA,',',DP/ACRtoHA,',',DBH(I)*INtoCM,',',&
                &DGI*INtoCM,',',HT(I)*FTtoM,',',HTG(I)*FTtoM,',',&
                &ICR(I),',',CW*FTtoM,',',IDMR,',',PCT(I),',',IPTBAL,',',&
                &CFV(I)*FT3toM3,',',WK1(I)*FT3toM3,',',BFV(I)*FT3toM3,&
                &',',ICDF,',',IBDF,',',&
                &NINT(FLOAT(ITRUNC(I)+5)*.01*FTtoM),&
                &',',HT2TD(I,1)*FTtoM,',',HT2TD(I,2)*FTtoM*0.0,')'

           iRet = fvsSQLCloseCursor(StmtHndlOut)
           iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),&
                &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
           call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
                &'DBSCUTS:Inserting Row')
           50 continue
        end do
     end if
  end do

  100 continue

  !     DO NOT ALLOW MORE THAN ONE CUTS LIST OUTPUT TO THE DATABASE PER RUN

  !      ICUTLIST = 0     !This is incorrect since you can have
  !                       !thinning in many different cycles - SAR 01/05

  !Release statement handle
  iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

end
