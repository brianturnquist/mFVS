!=====================!
!   dbs : dbsatrtls   !
!=====================!
! DBS $Id$
subroutine DBSATRTLS(IWHO,KODE,TEM)

  !     PURPOSE: TO OUTPUT THE ATRTLIST DATA TO THE DATABASE

  !     INPUT: IWHO  - THE WHO CALLED ME VALUE WHICH MUST BE 3
  !                     INORDER FOR US TO CONTINUE
  !            KODE  - FOR LETTING CALLING ROUTINE KNOW IF THIS IS A
  !                     REDIRECT OF THE FLAT FILE REPORT OR IN
  !                     ADDITION TO

  implicit none

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'ESTREE.f90'
  include 'VARCOM.f90'
  include 'WORKCM.f90'
  include 'DBSCOM.f90'

  ! Variables
  character(len=8) :: TID
  character(len=8) :: CSPECIES
  character(len=2000) :: SQLStmtStr
  character(len=20) :: TABLENAME
  character(len=20) :: DTYPE
  character(len=20) :: CREATENAM
  character(len=5) :: NTCUFT
  character(len=5) :: NMCUFT
  character(len=5) :: NSCUFT
  character(len=5) :: NBDFT
  character(len=8) :: NAMDCF
  character(len=8) :: NAMDBF
  integer :: IWHO,I,JYR,IP,ITPLAB,IRCODE,IDMR,ICDF,IBDF,IPTBAL,KODE
  integer :: ISPC,I1,I2,I3
  integer*4 :: IDCMP1,IDCMP2
  data IDCMP1,IDCMP2/10000000,20000000/
  real    :: CW,P,DGI,DP,TEM,ESTHT,TREAGE

  !     IF TREEOUT IS NOT TURNED ON OR THE IWHO VARIABLE IS NOT 1
  !     THEN JUST RETURN

  if (IATRTLIST.eq.0.or.IWHO.ne.3) return

  !     IS THIS OUTPUT A REDIRECT OF THE REPORT THEN SET KODE TO 0

  if (IATRTLIST.eq.2) KODE = 0

  !     ALWAYS CALL CASE TO MAKE SURE WE HAVE AN UP TO DATE CASE NUMBER

  call DBSCASE(1)

  !     For CS, LS, NE and SN, the table name is FVS_ATRTList_East and the following
  !     Column names change from: TCuFt, MCuFt, BdFt to MCuFt, SCuFt, SBdFt

  if (TRIM(DBMSOUT).eq.'EXCEL') then
     !     IF (VARACD.EQ.'CS' .OR. VARACD.EQ.'LS' .OR. VARACD.EQ.'SN' .OR.
     !  -      VARACD.EQ.'NE') THEN
     !       TABLENAME = '[FVS_ATRTList_East$]'
     !       CREATENAM =  'FVS_ATRTList_East'
     !       NTCUFT  = 'MCuFt'
     !       NMCUFT  = 'SCuFt'
     !       NBDFT   = 'SBdFt'
     !       NAMDCF  = 'Ht2TDMCF'
     !       NAMDBF  = 'Ht2TDSCF'
     !     ELSE
     TABLENAME = '[FVS_ATRTList$]'
     CREATENAM = 'FVS_ATRTList'
     NTCUFT = 'TCuFt'
     NMCUFT = 'MCuFt'
     NSCUFT = 'SCuFt'
     NBDFT = 'BdFt'
     NAMDCF = 'Ht2TDCF '
     NAMDBF = 'Ht2TDBF '
     ! ENDIF
     DTYPE = 'Number'
  else if (TRIM(DBMSOUT).eq.'ACCESS') then
     !     IF (VARACD.EQ.'CS' .OR. VARACD.EQ.'LS' .OR. VARACD.EQ.'SN' .OR.
     !  -      VARACD.EQ.'NE') THEN
     !       TABLENAME = 'FVS_ATRTList_East'
     !       CREATENAM = 'FVS_ATRTList_East'
     !       NTCUFT  = 'MCuFt'
     !       NMCUFT  = 'SCuFt'
     !       NBDFT   = 'SBdFt'
     !       NAMDCF  = 'Ht2TDMCF'
     !       NAMDBF  = 'Ht2TDSCF'
     !     ELSE
     TABLENAME = 'FVS_ATRTList'
     CREATENAM = 'FVS_ATRTList'
     NTCUFT = 'TCuFt'
     NMCUFT = 'MCuFt'
     NSCUFT = 'SCuFt'
     NBDFT = 'BdFt'
     NAMDCF = 'Ht2TDCF '
     NAMDBF = 'Ht2TDBF '
     ! ENDIF
     DTYPE = 'Double'
  else
     !     IF (VARACD.EQ.'CS' .OR. VARACD.EQ.'LS' .OR. VARACD.EQ.'SN' .OR.
     !  -      VARACD.EQ.'NE') THEN
     !       TABLENAME = 'FVS_ATRTList_East'
     !       CREATENAM = 'FVS_ATRTList_East'
     !       NTCUFT  = 'MCuFt'
     !       NMCUFT  = 'SCuFt'
     !       NBDFT   = 'SBdFt'
     !       NAMDCF  = 'Ht2TDMCF'
     !       NAMDBF  = 'Ht2TDSCF'
     !     ELSE
     TABLENAME = 'FVS_ATRTList'
     CREATENAM = 'FVS_ATRTList'
     NTCUFT = 'TCuFt'
     NMCUFT = 'MCuFt'
     NSCUFT = 'SCuFt'
     NBDFT = 'BdFt'
     NAMDCF = 'Ht2TDCF '
     NAMDBF = 'Ht2TDBF '
     ! ENDIF
     DTYPE = 'real'
  end if

  !     ALLOCATE A STATEMENT HANDLE

  iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
  if (iRet.ne.SQL_SUCCESS .and. iRet.ne. SQL_SUCCESS_WITH_INFO) then
     IATRTLIST = 0
     print *,'Error connecting to data source'
     call  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,&
          &'DBSATRTLS:Connecting to DSN')
     goto 100
  end if

  !     CHECK TO SEE IF THE TREELIST TABLE EXISTS IN DATBASE
  !     IF IT DOESNT THEN WE NEED TO CREATE IT

  call DBSCKNROWS(IRCODE,TABLENAME,MAXTRE,TRIM(DBMSOUT).eq.'EXCEL')
  if (IRCODE.eq.2) then
     IATRTLIST = 0
     return
  end if
  if (IRCODE.eq.1) then
     if (TRIM(DBMSOUT).eq."ACCESS") then
        SQLStmtStr = 'CREATE TABLE '// TRIM(CREATENAM) //&
             &' (CaseID Text not null,'//&
             &'StandID Text null,'//&
             &'Year int null,'//&
             &'PrdLen int null,'//&
             &'TreeId Text null,'//&
             &'TreeIndex int null,'//&
             &'Species Text null,'//&
             &'TreeVal int null,'//&
             &'SSCD int null,'//&
             &'PtIndex int null,'//&
             &'TPA double null,'//&
             &'MortPA double null,'//&
             &'DBH double null,'//&
             &'DG double null,'//&
             &'Ht double null,'//&
             &'HtG double null,'//&
             &'PctCr int null,'//&
             &'CrWidth double null,'//&
             &'MistCD int null,'//&
             &'BAPctile double null,'//&
             &'PtBAL double null,'//&
             &NTCUFT // ' double null,'//&
             &NMCUFT // ' double null,'//
        NSCUFT // ' double null,'//&
             &NBDFT  // ' double null,'//&
             &'MDefect int null,'//&
             &'BDefect int null,'//&
             &'TruncHt int null,'//&
             &'EstHt double null,'//&
             &'ActPt int null,'//&
             &NAMDCF // ' real null,'//&
             &NAMDBF // ' real null,'//&
             &'TreeAge double null)'

     else if (TRIM(DBMSOUT).eq."EXCEL") then
        SQLStmtStr = 'CREATE TABLE '// TRIM(CREATENAM) //&
             &' (CaseID Text null,'//&
             &'StandID Text null,'//&
             &'Year INT null,'//&
             &'PrdLen int null,'//&
             &'TreeId Text null,'//&
             &'TreeIndex int null,'//&
             &'Species Text null,'//&
             &'TreeVal int null,'//&
             &'SSCD int null,'//&
             &'PtIndex int null,'//&
             &'TPA Number null,'//&
             &'MortPA Number null,'//&
             &'DBH Number null,'//&
             &'DG Number null,'//&
             &'Ht Number null,'//&
             &'HtG Number null,'//&
             &'PctCr int null,'//&
             &'CrWidth Number null,'//&
             &'MistCD int null,'//&
             &'BAPctile Number null,'//&
             &'PtBAL Number null,'//&
             &NTCUFT // ' Number null,'//&
             &NMCUFT // ' Number null,'//
        NSCUFT // ' Number null,'//&
             &NBDFT  // ' Number null,'//&
             &'MDefect int null,'//&
             &'BDefect int null,'//&
             &'TruncHt int null,'//&
             &'EstHt Number null,'//&
             &'ActPt int null,'//&
             &NAMDCF // ' real null,'//&
             &NAMDBF // ' real null,'//&
             &'TreeAge Number null)'
     else
        SQLStmtStr = 'CREATE TABLE '// TRIM(CREATENAM) //&
             &' (CaseID char(36) not null,'//&
             &'StandID char(26) null,'//&
             &'Year int null,'//&
             &'PrdLen int null,'//&
             &'TreeId char(8) null,'//&
             &'TreeIndex int null,'//&
             &'Species char(3) null,'//&
             &'TreeVal int null,'//&
             &'SSCD int null,'//&
             &'PtIndex int null,'//&
             &'TPA real null,'//&
             &'MortPA real null,'//&
             &'DBH real null,'//&
             &'DG real null,'//&
             &'Ht real null,'//&
             &'HtG real null,'//&
             &'PctCr int null,'//&
             &'CrWidth real null,'//&
             &'MistCD int null,'//&
             &'BAPctile real null,'//&
             &'PtBAL real null,'//&
             &NTCUFT // ' real null,'//&
             &NMCUFT // ' real null,'//
        NSCUFT // ' real null,'//&
             &NBDFT  // ' real null,'//&
             &'MDefect int null,'//&
             &'BDefect int null,'//&
             &'TruncHt int null,'//&
             &'EstHt real null,'//&
             &'ActPt int null,'//&
             &NAMDCF // ' real null,'//&
             &NAMDBF // ' real null,'//&
             &'TreeAge real null)'
     end if

     iRet = fvsSQLCloseCursor(StmtHndlOut)
     iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),&
          &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
     call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
          &'DBSATRTLS:Creating Table: '//trim(SQLStmtStr))
  end if

  !     SET THE TREELIST TYPE FLAG (LET IP BE THE RECORD OUTPUT COUNT).
  !     AND THE OUTPUT REPORTING YEAR.

  JYR = IY(ICYC)
  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (I1.ne.0) then
        I2 = ISCT(ISPC,2)
        do I3 = I1,I2
           IP = 0
           do I = 1,ITRN
              if (PROB(I).gt.0) IP = IP+1
           end do
           I = IND1(I3)
           ITPLAB = 4
           P = PROB(I)/GROSPC
           DP = 0.0
           !           SKIP OUTPUT IF P <= 0
           if (P.le.0.0) goto 50

           !           TRANSLATE TREE IDS FOR TREES THAT HAVE BEEN COMPRESSED OR
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
           IPTBAL = NINT(PTBALT(I))

           !           DETERMINE ESTIMATED HEIGHT
           !           ESTIMATED HEIGHT IS NORMAL HEIGHT, UNLESS THE LATTER HAS NOT
           !           BEEN SET, IN WHICH CASE IT IS EQUAL TO CURRENT HEIGHT

           if (NORMHT(I) .ne.0) then
              ESTHT = (real(NORMHT(I))+5)/100
           else
              ESTHT = HT(I)
           end if

           !           DETERMINE TREE AGE

           if (LBIRTH(I)) then
              TREAGE = ABIRTH(I)
           else
              TREAGE = 0
           end if

           !           GET DG INPUT

           DGI = DG(I)
           if (ICYC.eq.0.and. TEM.eq.0) DGI = WORK1(I)

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

           if (ISPOUT31.eq.1)CSPECIES = ADJUSTL(TRIM(JSP(ISP(I))))
           if (ISPOUT31.eq.2)CSPECIES = ADJUSTL(TRIM(FIAJSP(ISP(I))))
           if (ISPOUT31.eq.3)CSPECIES = ADJUSTL(TRIM(PLNJSP(ISP(I))))

           write(SQLStmtStr,*)'INSERT INTO ',TABLENAME,&
                &'(CaseID,StandID,Year,PrdLen,',&
                &'TreeId,TreeIndex,Species,TreeVal,SSCD,PtIndex,TPA,',&
                &'MortPA,DBH,DG,',&
                &'HT,HTG,PctCr,CrWidth,MistCD,BAPctile,PtBAL,',&
                &NTCUFT,',',NMCUFT,',',NSCUFT,',',NBDFT,',',&
                &'MDefect,BDefect,TruncHt,EstHt,ActPt,',&
                &NAMDCF,',',NAMDBF,',','TreeAge) VALUES(''',&
                &CASEID,''',''',TRIM(NPLT),''',',&
                &JYR,',',IFINT,',''',ADJUSTL(TID),''',',I,',''',&
                &trim(CSPECIES),''',',IMC(I),',',ISPECL(I),',',ITRE(I),&
                &',',P,',',DP,',',DBH(I),',',DGI,',',HT(I),',',HTG(I),&
                &',',ICR(I),',',CW,',',IDMR,',',PCT(I),',',IPTBAL,',',&
                &CFV(I),',',MCFV(I),',',SCFV(I),',',BFV(I),',',ICDF,','&
                &,IBDF,',',&
                &((ITRUNC(I)+5)/100),',',ESTHT,',',IPVEC(ITRE(I)),&
                &',',HT2TD(I,2),',',HT2TD(I,1),',',TREAGE,')'

           !PRINT*, SQLStmtStr

           !Close Cursor
           iRet = fvsSQLCloseCursor(StmtHndlOut)

           iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),&
                &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
           if (iRet.ne.SQL_SUCCESS) IATRTLIST = 0
           call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
                &'DBSATRTLS:Inserting Row: '//trim(SQLStmtStr))
           50 continue
        end do
     end if
  end do

  100 continue

  !Release statement handle
  iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

end
