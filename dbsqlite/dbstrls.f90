!========================!
!   dbsqlite : dbstrls   !
!========================!
! DBSQLITE $Id$
subroutine DBSTRLS(IWHO,KODE,TEM)
  implicit none

  !     PURPOSE: TO OUTPUT THE TREELIST DATA TO THE DATABASE

  !     INPUT: IWHO  - THE WHO CALLED ME VALUE WHICH MUST BE 1
  !                     INORDER FOR US TO CONTINUE
  !            KODE  - FOR LETTING CALLING ROUTINE KNOW IF THIS IS A
  !                     REDIRECT OF THE FLAT FILE REPORT OR IN
  !                     ADDITION TO IT

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
  character(len=8) :: CSPECIE1
  character(len=8) :: CSPECIE2
  character(len=8) :: CSPECIE3
  character(len=17) :: TBLNAME
  character(len=5) :: NTCUFT
  character(len=5) :: NMCUFT
  character(len=5) :: NBDFT
  character(len=8) :: NAMDCF
  character(len=8) :: NAMDBF
  character(len=2000) :: SQLStmtStr
  integer :: IWHO,I,IP,ITPLAB,iRet,IDMR,ICDF,IBDF,IPTBAL,KODE
  integer :: ISPC,I1,I2,I3,ColNumber
  integer :: IDCMP1,IDCMP2,ITRNK
  data IDCMP1,IDCMP2/10000000,20000000/
  real    :: TEM
  real*8  :: CW,P,DGI,DP,ESTHT,TREAGE,DDBH,DHT,DHTG,DPCT,DCFV,DWK1,DSWK,DBFV,DHT2TD2,DHT2TD1

  integer :: fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,fsql3_prepare,fsql3_bind_double,&
       &fsql3_finalize,fsql3_bind_text,fsql3_reset,fsql3_addcolifabsent
  double precision :: XB

  !     IF TREEOUT IS NOT TURNED ON OR THE IWHO VARIABLE IS NOT 1
  !     THEN JUST RETURN

  if (ITREELIST.eq.0.or.IWHO.ne.1) return

  !     IS THIS OUTPUT A REDIRECT OF THE REPORT THEN SET KODE TO 0

  if (ITREELIST.eq.2) KODE = 0

  call DBSCASE(1)

  if (LMETRICVAR) then
     if (VARACD.eq.'CS' .or. VARACD.eq.'LS' .or. VARACD.eq.'SN' .or.&
          &VARACD.eq.'NE' .or. VARACD.eq.'ON') then
        TBLNAME = 'FVS_TreeList_East_Metric'
        NTCUFT = 'TCuM'
        NMCUFT = 'MCuM'
        NBDFT = 'CCum'
        NAMDCF = 'Ht2TDMCM'
        NAMDBF = 'Ht2TDSCM'
     else
        TBLNAME = 'FVS_TreeList_Metric'
        NTCUFT = 'TCuM'
        NMCUFT = 'MCuM'
        NBDFT = 'CCum'
        NAMDCF = 'Ht2TDCM '
        NAMDBF = 'Ht2TDBM '
     end if
  else
     TBLNAME = 'FVS_TreeList'
  end if

  iRet = fsql3_exec (IoutDBref,"Begin;"//Char(0))
  iRet = fsql3_tableexists(IoutDBref,TRIM(TBLNAME)//CHAR(0))
  if (iRet.eq.0) then
     if (LMETRICVAR) then
        SQLStmtStr = 'CREATE TABLE ' // TRIM(TBLNAME) //&
             &' (CaseID text not null,'//&
             &'StandID text not null,'//&
             &'Year int null,'//&
             &'PrdLen int null,'//&
             &'TreeId text null,'//&
             &'TreeIndex int null,'//&
             &'SpeciesFVS text null,'//&
             &'SpeciesPLANTS text null,'//&
             &'SpeciesFIA text null,'//&
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
             &NTCUFT // ' real null,'//&
             &NMCUFT // ' real null,'//&
             &NBDFT  // ' real null,'//&
             &'MDefect int null,'//&
             &'BDefect int null,'//&
             &'TruncHt int null,'//&
             &'EstHt real null,'//&
             &'ActPt int null,'//&
             &NAMDCF // ' real null,'//&
             &NAMDBF // ' real null,'//&
             &'TreeAge real null);'//CHAR(0)
     else
        SQLStmtStr = 'CREATE TABLE ' // TRIM(TBLNAME) //&
             &' (CaseID text not null,'//&
             &'StandID text not null,'//&
             &'Year int null,'//&
             &'PrdLen int null,'//&
             &'TreeId text null,'//&
             &'TreeIndex int null,'//&
             &'SpeciesFVS text null,'//&
             &'SpeciesPLANTS text null,'//&
             &'SpeciesFIA text null,'//&
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
             &'TCuFt real null,'//&
             &'MCuFt real null,'//&
             &'SCuFt real null,'//&
             &'BdFt real null,'//&
             &'MDefect int null,'//&
             &'BDefect int null,'//&
             &'TruncHt int null,'//&
             &'EstHt real null,'//&
             &'ActPt int null,'//&
             &'Ht2TDCF real null,'//&
             &'Ht2TDBF real null,'//&
             &'TreeAge real null);'//CHAR(0)
     end if
     iRet = fsql3_exec(IoutDBref,SQLStmtStr)
     if (iRet .ne.0) then
        ITREELIST = 0
        iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))
        return
     end if
  end if

  !     CHECK EXISTING TABLE FOR COLUMN(S) ADDED WITH NVB UPGRADE (2024)
  !     `SCuFt`
  !     TO ACCOUNT FOR ADDING TO DATABASE CREATED PROIR TO UPGRADE

  if (LMETRICVAR) then
     write(SQLStmtStr,*)'INSERT INTO ',TBLNAME,&
          &' (CaseID,StandID,Year,PrdLen,',&
          &'TreeId,TreeIndex,SpeciesFVS,SpeciesPLANTS,SpeciesFIA,',&
          &'TreeVal,SSCD,PtIndex,TPH,',&
          &'MortPH,DBH,DG,',&
          &'HT,HTG,PctCr,CrWidth,MistCD,BAPctile,PtBAL,',NTCUFT,',',&
          &NMCUFT,',',NBDFT,',MDefect,BDefect,TruncHt,',&
          &'EstHt,ActPt,',NAMDCF,',',NAMDBF,',','TreeAge) VALUES (''',&
          &CASEID,''',''',TRIM(NPLT),''',',IY(ICYC+1),',',IFINT,&
          &',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'
  else
     iRet = fsql3_addcolifabsent(IoutDBref,TRIM(TBLNAME)//CHAR(0),&
          &"SCuFt"//CHAR(0),"real"//CHAR(0))
     write(SQLStmtStr,*)'INSERT INTO ',TBLNAME,&
          &' (CaseID,StandID,Year,PrdLen,',&
          &'TreeId,TreeIndex,SpeciesFVS,SpeciesPLANTS,SpeciesFIA,',&
          &'TreeVal,SSCD,PtIndex,TPA,',&
          &'MortPA,DBH,DG,',&
          &'HT,HTG,PctCr,CrWidth,MistCD,BAPctile,PtBAL,',&
          &'TCuFt,MCuFt,SCuFt,BdFt,',&
          &'MDefect,BDefect,TruncHt,',&
          &'EstHt,ActPt,Ht2TDCF,Ht2TDBF,TreeAge) VALUES (''',&
          &CASEID,''',''',TRIM(NPLT),''',',IY(ICYC+1),',',IFINT,',',&
          &'?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,',&
          &'?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'
  end if
  iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
  if (iRet .ne.0) then
     ITREELIST = 0
     iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))
     return
  end if

  !     SET THE TREELIST TYPE FLAG (LET IP BE THE RECORD OUTPUT COUNT).
  !     AND THE OUTPUT REPORTING YEAR.

  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (I1.ne.0) then
        I2 = ISCT(ISPC,2)
        do I3 = I1,I2
           I = IND1(I3)

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
              TID = ADJUSTL(TID)
           end if

           !           GET MISTLETOE RATING FOR CURRENT TREE RECORD.

           call MISGET(I,IDMR)

           !           SET CROWN WIDTH.

           if (.not. LMETRICVAR) then
              CW = CRWDTH(I)
           end if

           !           DECODE DEFECT AND ROUND OFF POINT BAL.

           ICDF = (DEFECT(I)-((DEFECT(I)/10000)*10000))/100
           IBDF = DEFECT(I)-((DEFECT(I)/100)*100)
           IPTBAL = NINT(PTBALT(I))

           !           SET TRUNCATED (TOPKILL) HEIGHT

           if (LMETRICVAR) then
              ITRNK = INT(real((ITRUNC(I)+5)*.01*FTtoM))
           else
              ITRNK = INT((ITRUNC(I)+5)/100)
           end if

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

           !           LOAD SPECIES CODES FROM FVS, PLANTS AND FIA ARRAYS.

           CSPECIE1 = JSP(ISP(I))
           CSPECIE2 = PLNJSP(ISP(I))
           CSPECIE3 = FIAJSP(ISP(I))

           ColNumber = 1
           iRet = fsql3_bind_text(IoutDBref,ColNumber,TID,&
                &LEN_TRIM(TID))
           ColNumber = ColNumber+1
           iRet = fsql3_bind_int(IoutDBref,ColNumber,I)
           ColNumber = ColNumber+1
           iRet = fsql3_bind_text(IoutDBref,ColNumber,CSPECIE1,&
                &LEN_TRIM(CSPECIE1))
           ColNumber = ColNumber+1
           iRet = fsql3_bind_text(IoutDBref,ColNumber,CSPECIE2,&
                &LEN_TRIM(CSPECIE2))
           ColNumber = ColNumber+1
           iRet = fsql3_bind_text(IoutDBref,ColNumber,CSPECIE3,&
                &LEN_TRIM(CSPECIE3))
           ColNumber = ColNumber+1
           iRet = fsql3_bind_int(IoutDBref,ColNumber,IMC(I))
           ColNumber = ColNumber+1
           iRet = fsql3_bind_int(IoutDBref,ColNumber,ISPECL(I))
           ColNumber = ColNumber+1
           if (LMETRICVAR) then
              iRet = fsql3_bind_int(IoutDBref,ColNumber,IPVEC(ITRE(I)))
           else
              iRet = fsql3_bind_int(IoutDBref,ColNumber,ITRE(I))
           end if
           ColNumber = ColNumber+1
           if (LMETRICVAR) then
              XB = P/ACRtoHA
              iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)
           else
              iRet = fsql3_bind_double(IoutDBref,ColNumber,P)
           end if
           ColNumber = ColNumber+1
           if (LMETRICVAR) then
              XB = DP/ACRtoHA
              iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)
           else
              iRet = fsql3_bind_double(IoutDBref,ColNumber,DP)
              DDBH = DBH(I)
           end if
           ColNumber = ColNumber+1
           if (LMETRICVAR) then
              XB = DBH(I)*INtoCM
              iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)
           else
              iRet = fsql3_bind_double(IoutDBref,ColNumber,DDBH)
           end if
           ColNumber = ColNumber+1
           if (LMETRICVAR) then
              XB = DGI*INtoCM
              iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)
           else
              iRet = fsql3_bind_double(IoutDBref,ColNumber,DGI)
              DHT = HT(I)
           end if
           ColNumber = ColNumber+1
           if (LMETRICVAR) then
              XB = HT(I)*FTtoM
              iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)
           else
              iRet = fsql3_bind_double(IoutDBref,ColNumber,DHT)
              DHTG = HTG(I)
           end if
           ColNumber = ColNumber+1
           if (LMETRICVAR) then
              XB = HTG(I)*FTtoM
              iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)
           else
              iRet = fsql3_bind_double(IoutDBref,ColNumber,DHTG)
           end if
           ColNumber = ColNumber+1
           iRet = fsql3_bind_int(IoutDBref,ColNumber,ICR(I))
           ColNumber = ColNumber+1
           if (LMETRICVAR) then
              XB = CRWDTH(I)*FTtoM
              iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)
           else
              iRet = fsql3_bind_double(IoutDBref,ColNumber,CW)
           end if
           ColNumber = ColNumber+1
           iRet = fsql3_bind_int(IoutDBref,ColNumber,IDMR)
           if (.not. LMETRICVAR) then
              DPCT = PCT(I)
           end if
           ColNumber = ColNumber+1
           if (LMETRICVAR) then
              XB = PCT(I)
              iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)
           else
              iRet = fsql3_bind_double(IoutDBref,ColNumber,DPCT)
           end if
           ColNumber = ColNumber+1
           iRet = fsql3_bind_int(IoutDBref,ColNumber,IPTBAL)
           ColNumber = ColNumber+1
           if (LMETRICVAR) then
              XB = CFV(I)*FT3toM3
              iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)
           else
              DCFV = CFV(I)
              iRet = fsql3_bind_double(IoutDBref,ColNumber,DCFV)
           end if
           ColNumber = ColNumber+1
           if (LMETRICVAR) then
              XB = WK1(I)*FT3toM3
              iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)
           else
              DWK1 = MCFV(I)
              iRet = fsql3_bind_double(IoutDBref,ColNumber,DWK1)
           end if
           ColNumber = ColNumber+1
           if (LMETRICVAR) then
              XB = BFV(I)*FT3toM3
              iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)
           else
              DSWK = SCFV(I)
              iRet = fsql3_bind_double(IoutDBref,ColNumber,DSWK)
              ColNumber = ColNumber+1
              DBFV = BFV(I)
              iRet = fsql3_bind_double(IoutDBref,ColNumber,DBFV)
           end if
           !           Report Volume Defect
           ColNumber = ColNumber+1
           iRet = fsql3_bind_int(IoutDBref,ColNumber,ICDF)
           ColNumber = ColNumber+1
           iRet = fsql3_bind_int(IoutDBref,ColNumber,IBDF)
           ColNumber = ColNumber+1
           iRet = fsql3_bind_int(IoutDBref,ColNumber,ITRNK)
           ColNumber = ColNumber+1
           if (LMETRICVAR) then
              XB = ESTHT*FTtoM
              iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)
           else
              iRet = fsql3_bind_double(IoutDBref,ColNumber,ESTHT)
           end if
           ColNumber = ColNumber+1
           iRet = fsql3_bind_int(IoutDBref,ColNumber,IPVEC(ITRE(I)))
           if (.not. LMETRICVAR) then
              DHT2TD2 = HT2TD(I,2)
           end if
           ColNumber = ColNumber+1
           if (LMETRICVAR) then
              XB = HT2TD(I,2)*FTtoM
              iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)
           else
              iRet = fsql3_bind_double(IoutDBref,ColNumber,DHT2TD2)
           end if
           ColNumber = ColNumber+1
           if (LMETRICVAR) then
              XB = HT2TD(I,1)*FTtoM
              iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)
           else
              DHT2TD1 = HT2TD(I,1)
              iRet = fsql3_bind_double(IoutDBref,ColNumber,DHT2TD1)
           end if
           ColNumber = ColNumber+1
           if (LMETRICVAR) then
              XB = TREAGE
              iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)
           else
              iRet = fsql3_bind_double(IoutDBref,ColNumber,TREAGE)
           end if

           iRet = fsql3_step(IoutDBref)
           iRet = fsql3_reset(IoutDBref)
        end do
     end if
  end do

  !     FOR CYCLE 0 TREELIST, PRINT DEAD TREES WHICH WERE PRESENT IN
  !     THE INVENTORY DATA AT THE BOTTOM OF THE TREELIST.

  if (ITREELIST .eq.0) return
  if ((IREC2.ge.MAXTP1).or.(ITPLAB.eq.3).or.&
       &(ICYC.ge.1)) then
     iRet = fsql3_finalize(IoutDBref)
     iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))
     return
  end if

  do I = IREC2,MAXTRE

     P = (PROB(I) / GROSPC) / (FINT/FINTM)
     write(TID,'(I8)') IDTREE(I)
     TID = ADJUSTL(TID)

     !       GET MISTLETOE RATING FOR CURRENT TREE RECORD.
     call MISGET(I,IDMR)

     !       SET CROWN WIDTH.
     CW = CRWDTH(I)

     !       DECODE DEFECT AND ROUND OFF POINT BAL.

     ICDF = (DEFECT(I)-((DEFECT(I)/10000)*10000))/100
     IBDF = DEFECT(I)-((DEFECT(I)/100)*100)
     IPTBAL = NINT(PTBALT(I))

     !       SET TRUNCATED (TOPKILL) HEIGHT

     if (LMETRICVAR) then
        ITRNK = INT(real((ITRUNC(I)+5)*.01*FTtoM))
     else
        ITRNK = INT((ITRUNC(I)+5)/100)
     end if

     !       DETERMINE ESTIMATED HEIGHT
     !       ESTIMATED HEIGHT IS NORMAL HEIGHT, UNLESS THE IT WAS NOT
     !       BEEN SET, IN WHICH CASE IT IS EQUAL TO CURRENT HEIGHT

     if (NORMHT(I) .ne.0) then
        if (LMETRICVAR) then
           ESTHT = ((real(NORMHT(I))+5)/100)
        else
           ESTHT = (real(NORMHT(I))+5)/100
        end if
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

     !       LOAD SPECIES CODES FROM FVS, PLANTS AND FIA ARRAYS.

     CSPECIE1 = JSP(ISP(I))
     CSPECIE2 = PLNJSP(ISP(I))
     CSPECIE3 = FIAJSP(ISP(I))

     ColNumber = 1
     iRet = fsql3_bind_text(IoutDBref,ColNumber,TID,&
          &LEN_TRIM(TID))
     ColNumber = ColNumber+1
     iRet = fsql3_bind_int(IoutDBref,ColNumber,I)
     ColNumber = ColNumber+1
     iRet = fsql3_bind_text(IoutDBref,ColNumber,CSPECIE1,&
          &LEN_TRIM(CSPECIE1))
     ColNumber = ColNumber+1
     iRet = fsql3_bind_text(IoutDBref,ColNumber,CSPECIE2,&
          &LEN_TRIM(CSPECIE2))
     ColNumber = ColNumber+1
     iRet = fsql3_bind_text(IoutDBref,ColNumber,CSPECIE3,&
          &LEN_TRIM(CSPECIE3))
     ColNumber = ColNumber+1
     iRet = fsql3_bind_int(IoutDBref,ColNumber,IMC(I))
     ColNumber = ColNumber+1
     iRet = fsql3_bind_int(IoutDBref,ColNumber,ISPECL(I))
     ColNumber = ColNumber+1
     if (LMETRICVAR) then
        iRet = fsql3_bind_int(IoutDBref,ColNumber,IPVEC(ITRE(I)))
     else
        iRet = fsql3_bind_int(IoutDBref,ColNumber,ITRE(I))
     end if
     ColNumber = ColNumber+1
     if (LMETRICVAR) then
        XB = P/ACRtoHA
        iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)
     else
        iRet = fsql3_bind_double(IoutDBref,ColNumber,P)
     end if
     ColNumber = ColNumber+1
     if (LMETRICVAR) then
        XB = DP/ACRtoHA
        iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)
     else
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DP)
        DDBH = DBH(I)
     end if
     ColNumber = ColNumber+1
     if (LMETRICVAR) then
        XB = DBH(I)*INtoCM
        iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)
     else
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DDBH)
     end if
     ColNumber = ColNumber+1
     if (LMETRICVAR) then
        XB = DGI*INtoCM
        iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)
     else
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DGI)
        DHT = HT(I)
     end if
     ColNumber = ColNumber+1
     if (LMETRICVAR) then
        XB = HT(I)*FTtoM
        iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)
     else
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DHT)
        DHTG = HTG(I)
     end if
     ColNumber = ColNumber+1
     if (LMETRICVAR) then
        XB = HTG(I)*FTtoM
        iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)
     else
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DHTG)
     end if
     ColNumber = ColNumber+1
     iRet = fsql3_bind_int(IoutDBref,ColNumber,ICR(I))
     ColNumber = ColNumber+1
     if (LMETRICVAR) then
        XB = CRWDTH(I)*FTtoM
        iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)
     else
        iRet = fsql3_bind_double(IoutDBref,ColNumber,CW)
     end if
     ColNumber = ColNumber+1
     iRet = fsql3_bind_int(IoutDBref,ColNumber,IDMR)
     if (.not. LMETRICVAR) then
        DPCT = PCT(I)
     end if
     ColNumber = ColNumber+1
     if (LMETRICVAR) then
        XB = PCT(I)
        iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)
     else
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DPCT)
     end if
     ColNumber = ColNumber+1
     iRet = fsql3_bind_int(IoutDBref,ColNumber,IPTBAL)
     ColNumber = ColNumber+1
     if (LMETRICVAR) then
        XB = CFV(I)*FT3toM3
        iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)
     else
        DCFV = CFV(I)
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DCFV)
     end if
     ColNumber = ColNumber+1
     if (LMETRICVAR) then
        XB = WK1(I)*FT3toM3
        iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)
     else
        DWK1 = MCFV(I)
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DWK1)
     end if
     ColNumber = ColNumber+1
     if (LMETRICVAR) then
        XB = BFV(I)*FT3toM3
        iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)
     else
        DSWK = SCFV(I)
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DSWK)
        ColNumber = ColNumber+1
        DBFV = BFV(I)
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DBFV)
     end if
     !       Report Volume Defect
     ColNumber = ColNumber+1
     iRet = fsql3_bind_int(IoutDBref,ColNumber,ICDF)
     ColNumber = ColNumber+1
     iRet = fsql3_bind_int(IoutDBref,ColNumber,IBDF)
     ColNumber = ColNumber+1
     iRet = fsql3_bind_int(IoutDBref,ColNumber,ITRNK)
     ColNumber = ColNumber+1
     if (LMETRICVAR) then
        XB = ESTHT*FTtoM
        iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)
     else
        iRet = fsql3_bind_double(IoutDBref,ColNumber,ESTHT)
     end if
     ColNumber = ColNumber+1
     iRet = fsql3_bind_int(IoutDBref,ColNumber,IPVEC(ITRE(I)))
     if (.not. LMETRICVAR) then
        DHT2TD2 = HT2TD(I,2)
     end if
     ColNumber = ColNumber+1
     if (LMETRICVAR) then
        XB = HT2TD(I,2)*FTtoM
        iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)
     else
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DHT2TD2)
     end if
     ColNumber = ColNumber+1
     if (LMETRICVAR) then
        XB = HT2TD(I,1)*FTtoM
        iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)
     else
        DHT2TD1 = HT2TD(I,1)
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DHT2TD1)
     end if
     ColNumber = ColNumber+1
     if (LMETRICVAR) then
        XB = TREAGE
        iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)
     else
        iRet = fsql3_bind_double(IoutDBref,ColNumber,TREAGE)
     end if

     iRet = fsql3_step(IoutDBref)
     iRet = fsql3_reset(IoutDBref)
  end do
  iRet = fsql3_finalize(IoutDBref)
  iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))

  return
end
