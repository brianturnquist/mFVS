!===================!
!   dbs : dbstrls   !
!===================!
! DBS $Id$
subroutine DBSTRLS(IWHO,KODE,TEM)
  implicit none

  !     PURPOSE: TO OUTPUT THE TREELIST DATA TO THE DATABASE

  !     AUTH: D. GAMMEL -- SEM -- JULY 2002

  !     INPUT: IWHO  - THE WHO CALLED ME VALUE WHICH MUST BE 1
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
  include 'wdbkwtdata.inc'

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
  real    :: CARBFACT,ABVGRD_BIO,MERCH_BIO,SAW_BIO
  real    :: ABVGRD_CARB,MERCH_CARB,SAW_CARB

  !     IF TREEOUT IS NOT TURNED ON OR THE IWHO VARIABLE IS NOT 1
  !     THEN JUST RETURN

  if (ITREELIST.eq.0.or.IWHO.ne.1) return

  !     IS THIS OUTPUT A REDIRECT OF THE REPORT THEN SET KODE TO 0

  if (ITREELIST.eq.2) KODE = 0

  !     ALWAYS CALL CASE TO MAKE SURE WE HAVE AN UP TO DATE CASE NUMBER

  call DBSCASE(1)

  !     For CS, LS, NE and SN, the table name is FVS_TreeList_East and the following
  !     Column names change from: TCuFt, MCuFt, BdFt to MCuFt, SCuFt, SBdFt

  if (TRIM(DBMSOUT).eq.'EXCEL') then
     !     IF (VARACD.EQ.'CS' .OR. VARACD.EQ.'LS' .OR. VARACD.EQ.'SN' .OR.
     !  -      VARACD.EQ.'NE') THEN
     !       TABLENAME = '[FVS_TreeList_East$]'
     !       CREATENAM =  'FVS_TreeList_East'
     !       NTCUFT  = 'MCuFt'
     !       NMCUFT  = 'SCuFt'
     !       NBDFT   = 'SBdFt'
     !       NAMDCF  = 'Ht2TDMCF'
     !       NAMDBF  = 'Ht2TDSCF'
     !     ELSE
     TABLENAME = '[FVS_TreeList$]'
     CREATENAM = 'FVS_TreeList'
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
     !       TABLENAME = 'FVS_TreeList_East'
     !       CREATENAM = 'FVS_TreeList_East'
     !       NTCUFT  = 'MCuFt'
     !       NMCUFT  = 'SCuFt'
     !       NBDFT   = 'SBdFt'
     !       NAMDCF  = 'Ht2TDMCF'
     !       NAMDBF  = 'Ht2TDSCF'
     !     ELSE
     TABLENAME = 'FVS_TreeList'
     CREATENAM = 'FVS_TreeList'
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
     !       TABLENAME = 'FVS_TreeList_East'
     !       CREATENAM = 'FVS_TreeList_East'
     !       NTCUFT  = 'MCuFt'
     !       NMCUFT  = 'SCuFt'
     !       NBDFT   = 'SBdFt'
     !       NAMDCF  = 'Ht2TDMCF'
     !       NAMDBF  = 'Ht2TDSCF'
     !     ELSE
     TABLENAME = 'FVS_TreeList'
     CREATENAM = 'FVS_TreeList'
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
     ITREELIST = 0
     call  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,&
          &'DBSTRLS:Connecting to DSN')
     goto 100
  end if

  !     CHECK TO SEE IF THE TREELIST TABLE EXISTS IN DATBASE
  !     IF NOT, THEN WE NEED TO CREATE IT

  call DBSCKNROWS(IRCODE,TABLENAME,MAXTRE,TRIM(DBMSOUT).eq.'EXCEL')
  if (IRCODE.eq.2) then
     ITREELIST = 0
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
             &NMCUFT // ' double null,'//&
             &NSCUFT // ' double null,'//&
             &NBDFT  // ' double null,'//&
             &'AbvGrd_Bio double null,'//&
             &'Merch_Bio double null,'//&
             &'Sawtimber_Bio double null,'//&
             &'AbvGrd_Carbon double null,'//&
             &'Merch_Carbon double null,'//&
             &'Sawtimber_Carbon dbouble null,'//&
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
             &NMCUFT // ' Number null,'//&
             &NSCUFT // ' Number null,'//&
             &NBDFT  // ' Number null,'//&
             &'AbvGrd_Bio Number null,'//&
             &'Merch_Bio Number null,'//&
             &'Sawtimber_Bio Number null,'//&
             &'AbvGrd_Carbon Number null,'//&
             &'Merch_Carbon Number null,'//&
             &'Sawtimber_Carbon Number null,'//&
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
             &' (CaseID char(36),'//&
             &'StandID char(26),'//&
             &'Year int null,'//&
             &'PrdLen int null,'//&
             &'TreeId char(8) null,'//&
             &'TreeIndex int null,'//&
             &'Species char(8) null,'//&
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
             &NMCUFT // ' real null,'//&
             &NSCUFT // ' real null,'//&
             &NBDFT  // ' real null,'//&
             &'AbvGrd_Bio real null,'//&
             &'Merch_Bio real null,'//&
             &'Sawtimber_Bio real null,'//&
             &'AbvGrd_Carbon real null,'//&
             &'Merch_Carbon real null,'//&
             &'Sawtimber_Carbon real null,'//&
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
          &'DBSTRLS:Creating Table: '//trim(SQLStmtStr))
  end if

  !     SET THE TREELIST TYPE FLAG (LET IP BE THE RECORD OUTPUT COUNT).
  !     AND THE OUTPUT REPORTING YEAR.

  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (I1.ne.0) then
        I2 = ISCT(ISPC,2)
        do I3 = I1,I2
           I = IND1(I3)

           JYR = IY(ICYC+1)
           IP = ITRN
           ITPLAB = 1
           P = PROB(I) / GROSPC
           if (ICYC.gt.0) then
              DP = WK2(I)/ GROSPC
           else
              DP = 0.0
           end if

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
           !           ESTIMATED HEIGHT IS NORMAL HEIGHT, UNLESS THE IT WAS NOT
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

           CARBFACT = 0
           CARBFACT = WDBKWT(FIASJSP(ISP(I)),12)
           ABVGRD_BIO = DRYBIO(1,I)+DRYBIO(13,I)
           MERCH_BIO = DRYBIO(6,I)+DRYBIO(8,I)
           SAW_BIO = DRYBIO(6,I)
           ABVGRD_CARB = ABVGRD_BIO * CARBFACT
           MERCH_CARB = MERCH_BIO * CARBFACT
           SAW_CARB = SAW_BIO * CARBFACT

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

           if (ISPOUT6.eq.1)CSPECIES = ADJUSTL(TRIM(JSP(ISP(I))))
           if (ISPOUT6.eq.2)CSPECIES = ADJUSTL(TRIM(FIAJSP(ISP(I))))
           if (ISPOUT6.eq.3)CSPECIES = ADJUSTL(TRIM(PLNJSP(ISP(I))))

           write(SQLStmtStr,*)'INSERT INTO ',TABLENAME,&
                &' (CaseID,StandID,Year,PrdLen,',&
                &'TreeId,TreeIndex,Species,TreeVal,SSCD,PtIndex,TPA,',&
                &'MortPA,DBH,DG,',&
                &'HT,HTG,PctCr,CrWidth,MistCD,BAPctile,PtBAL,',&
                &NTCUFT,',',NMCUFT,',',NSCUFT,',',NBDFT,',',&
                &'AbvGrd_Bio,Merch_Bio,Sawtimber_Bio,'&
                &'AbvGrd_Carbon,Merch_Carbon,Sawtimber_Carbon,'&
                &'MDefect,BDefect,TruncHt,EstHt,ActPt,',&
                &NAMDCF,',',NAMDBF,',','TreeAge) VALUES(''',&
                &CASEID,''',''',TRIM(NPLT),&
                &''',',JYR,',',IFINT,",'",ADJUSTL(TID),"',",I,",'",&
                &trim(CSPECIES),"',",IMC(I),',',ISPECL(I),',',ITRE(I),&
                &',',P,',',DP,',',DBH(I),',',DGI,',',HT(I),',',HTG(I),&
                &',',ICR(I),',',CW,',',IDMR,',',PCT(I),',',IPTBAL,',',&
                &CFV(I),',',MCFV(I),',',SCFV(I),',',BFV(I),','&
                &ABVGRD_BIO,',',MERCH_BIO,',',SAW_BIO,','&
                &ABVGRD_CARB,',',MERCH_CARB,',',SAW_CARB,','&
                &,ICDF,',',IBDF,',',&
                &((ITRUNC(I)+5)/100),',',ESTHT,',',IPVEC(ITRE(I)),&
                &',',HT2TD(I,2),',',HT2TD(I,1),',',TREAGE,')'

           iRet = fvsSQLCloseCursor(StmtHndlOut)

           iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),&
                &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
           if (iRet.ne.SQL_SUCCESS) ITREELIST = 0
           call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
                &'DBSTRLS:Inserting Row: '//trim(SQLStmtStr))
        end do
     end if
  end do

  !     FOR CYCLE 0 TREELIST, PRINT DEAD TREES WHICH WERE PRESENT IN
  !     THE INVENTORY DATA AT THE BOTTOM OF THE TREELIST.

  if (ITREELIST .eq.0) goto 100
  if ((IREC2.ge.MAXTP1).or.(ITPLAB.eq.3).or.(ICYC.ge.1)) go to 100
  do I = IREC2,MAXTRE
     P = (PROB(I) / GROSPC) / (FINT/FINTM)
     write(TID,'(I8)') IDTREE(I)

     !       GET MISTLETOE RATING FOR CURRENT TREE RECORD.
     call MISGET(I,IDMR)

     !       SET CROWN WIDTH.
     CW = CRWDTH(I)

     !       DECODE DEFECT AND ROUND OFF POINT BAL.
     ICDF = (DEFECT(I)-((DEFECT(I)/10000)*10000))/100
     IBDF = DEFECT(I)-((DEFECT(I)/100)*100)
     IPTBAL = NINT(PTBALT(I))

     !       DETERMINE ESTIMATED HEIGHT
     !       ESTIMATED HEIGHT IS NORMAL HEIGHT, UNLESS THE IT WAS NOT
     !       BEEN SET, IN WHICH CASE IT IS EQUAL TO CURRENT HEIGHT

     if (NORMHT(I) .ne.0) then
        ESTHT = (real(NORMHT(I))+5)/100
     else
        ESTHT = HT(I)
     end if

     !       DETERMINE TREE AGE

     if (LBIRTH(I)) then
        TREAGE = ABIRTH(I)
     else
        TREAGE = 0
     end if

     !       CYCLE 0, PRINT INPUT DG ONLY, UNLESS DIRECTED TO PRINT ESTIMATES.
     DGI = DG(I)
     if (ICYC.eq.0.and. TEM.eq.0) DGI = WORK1(I)

     !       PUT PROB IN MORTALITY COLUMN
     DP = P
     P = 0.
     !       GET BIOMASS AND CARBON
     CARBFACT = 0
     CARBFACT = WDBKWT(FIASJSP(ISP(I)),12)
     ABVGRD_BIO = DRYBIO(1,I)+DRYBIO(13,I)
     MERCH_BIO = DRYBIO(6,I)+DRYBIO(8,I)
     SAW_BIO = DRYBIO(6,I)
     ABVGRD_CARB = ABVGRD_BIO * CARBFACT
     MERCH_CARB = MERCH_BIO * CARBFACT
     SAW_CARB = SAW_BIO * CARBFACT

     !       DETERMINE PREFERED OUTPUT FORMAT FOR SPECIES CODE
     !       KEYWORD OVER RIDES

     if (JSPIN(ISP(I)).eq.1) then
        CSPECIES = ADJUSTL(TRIM(JSP(ISP(I))))
     else if (JSPIN(ISP(I)).eq.2) then
        CSPECIES = ADJUSTL(TRIM(FIAJSP(ISP(I))))
     else if (JSPIN(ISP(I)).eq.3) then
        CSPECIES = ADJUSTL(TRIM(PLNJSP(ISP(I))))
     else
        CSPECIES = ADJUSTL(TRIM(PLNJSP(ISP(I))))
     end if

     if (ISPOUT6.eq.1)CSPECIES = ADJUSTL(TRIM(JSP(ISP(I))))
     if (ISPOUT6.eq.2)CSPECIES = ADJUSTL(TRIM(FIAJSP(ISP(I))))
     if (ISPOUT6.eq.3)CSPECIES = ADJUSTL(TRIM(PLNJSP(ISP(I))))

     write(SQLStmtStr,*)'INSERT INTO ',TABLENAME,&
          &' (CaseID,StandID,Year,PrdLen,',&
          &'TreeId,TreeIndex,Species,TreeVal,SSCD,PtIndex,TPA,',&
          &'MortPA,DBH,DG,',&
          &'HT,HTG,PctCr,CrWidth,MistCD,BAPctile,PtBAL,',&
          &NTCUFT,',',NMCUFT,',',NSCUFT,',',NBDFT,',',&
          &'AbvGrd_Bio,Merch_Bio,Sawtimber_Bio,'&
          &'AbvGrd_Carbon,Merch_Carbon,Sawtimber_Carbon,'&
          &'MDefect,BDefect,TruncHt,EstHt,ActPt,',&
          &NAMDCF,',',NAMDBF,',','TreeAge) VALUES(''',&
          &CASEID,''',''',TRIM(NPLT),&
          &''',',JYR,',',IFINT,",'",ADJUSTL(TID),"',",I,&
          &",'",trim(CSPECIES),"',",IMC(I),',',ISPECL(I),',',ITRE(I),&
          &',',P,',',DP,',',DBH(I),',',DGI,',',HT(I),',',HTG(I),&
          &',',ICR(I),',',CW,',',IDMR,',',PCT(I),',',IPTBAL,',',&
          &CFV(I),',',MCFV(I),',',SCFV(I),',',BFV(I),','&
          &ABVGRD_BIO,',',MERCH_BIO,',',SAW_BIO,','&
          &ABVGRD_CARB,',',MERCH_CARB,',',SAW_CARB,','&
          &,ICDF,',',IBDF,',',&
          &((ITRUNC(I)+5)/100),',',ESTHT,',',IPVEC(ITRE(I)),&
          &',',HT2TD(I,2),',',HT2TD(I,1),',',TREAGE,')'

     iRet = fvsSQLCloseCursor(StmtHndlOut)

     iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),&
          &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

     if (iRet.ne.SQL_SUCCESS) ITREELIST = 0
     call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
          &'DBSTRLS:Inserting Row: '//trim(SQLStmtStr))

  end do
  100 continue
  iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)
end
