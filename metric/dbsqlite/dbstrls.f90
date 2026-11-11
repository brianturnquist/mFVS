!===============================!
!   metric/dbsqlite : dbstrls   !
!===============================!
! DBSQLITE-METRIC $Id: dbstrls.f 2910 2020-01-03 17:39:57Z ckeyser01 $
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

  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_ADDCOLIFABSENT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_DOUBLE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_INT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_TEXT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_CLOSE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLCNT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLDOUBLE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLINT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLISNULL
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLNAME
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLREAL
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLTEXT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLTYPE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_ERRMSG
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_EXEC
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_FINALIZE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_OPEN
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_PREPARE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_RESET
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_STEP
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_TABLEEXISTS
#if !(_WIN64)
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_ADDCOLIFABSENT' :: FSQL3_ADDCOLIFABSENT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_BIND_DOUBLE'    :: FSQL3_BIND_DOUBLE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_BIND_INT'       :: FSQL3_BIND_INT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_BIND_TEXT'      :: FSQL3_BIND_TEXT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_CLOSE'          :: FSQL3_CLOSE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLCNT'         :: FSQL3_COLCNT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLDOUBLE'      :: FSQL3_COLDOUBLE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLINT'         :: FSQL3_COLINT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLISNULL'      :: FSQL3_COLISNULL
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLNAME'        :: FSQL3_COLNAME
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLREAL'        :: FSQL3_COLREAL
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLTEXT'        :: FSQL3_COLTEXT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLTYPE'        :: FSQL3_COLTYPE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_ERRMSG'         :: FSQL3_ERRMSG
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_EXEC'           :: FSQL3_EXEC
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_FINALIZE'       :: FSQL3_FINALIZE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_OPEN'           :: FSQL3_OPEN
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_PREPARE'        :: FSQL3_PREPARE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_RESET'          :: FSQL3_RESET
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_STEP'           :: FSQL3_STEP
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_TABLEEXISTS'    :: FSQL3_TABLEEXISTS
#endif

  ! Variables
  character(len=8) :: TID
  character(len=8) :: CSPECIE1
  character(len=8) :: CSPECIE2
  character(len=8) :: CSPECIE3
  character(len=24) :: TBLNAME
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
  real*8  :: CW,P,DGI,DP,ESTHT,TREAGE
  double precision :: XB

  integer :: fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,fsql3_prepare,fsql3_bind_double,&
       &fsql3_finalize,fsql3_bind_text,fsql3_reset

  !     IF TREEOUT IS NOT TURNED ON OR THE IWHO VARIABLE IS NOT 1
  !     THEN JUST RETURN

  if (ITREELIST.eq.0.or.IWHO.ne.1) return

  !     IS THIS OUTPUT A REDIRECT OF THE REPORT THEN SET KODE TO 0

  if (ITREELIST.eq.2) KODE = 0

  call DBSCASE(1)

  !     For CS, LS, NE and SN, the table name is FVS_TreeList_East and the following
  !     Column names change from: TCuFt, MCuFt, BdFt to MCuFt, SCuFt, SBdFt

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

  iRet = fsql3_exec (IoutDBref,"Begin;"//Char(0))
  iRet = fsql3_tableexists(IoutDBref,TRIM(TBLNAME)//CHAR(0))
  if (iRet.eq.0) then
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
     iRet = fsql3_exec(IoutDBref,SQLStmtStr)
     if (iRet .ne.0) then
        ITREELIST = 0
        iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))
        return
     end if
  end if
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

           !           DECODE DEFECT AND ROUND OFF POINT BAL.

           ICDF = (DEFECT(I)-((DEFECT(I)/10000)*10000))/100
           IBDF = DEFECT(I)-((DEFECT(I)/100)*100)
           IPTBAL = NINT(PTBALT(I))

           !           SET TRUNCATED (TOPKILL) HEIGHT

           ITRNK = INT(real((ITRUNC(I)+5)*.01*FTtoM))

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
           iRet = fsql3_bind_int(IoutDBref,ColNumber,IPVEC(ITRE(I)))

           ColNumber = ColNumber+1
           XB = P/ACRtoHA
           iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

           ColNumber = ColNumber+1
           XB = DP/ACRtoHA
           iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

           ColNumber = ColNumber+1
           XB = DBH(I)*INtoCM
           iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

           ColNumber = ColNumber+1
           XB = DGI*INtoCM
           iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

           ColNumber = ColNumber+1
           XB = HT(I)*FTtoM
           iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

           ColNumber = ColNumber+1
           XB = HTG(I)*FTtoM
           iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

           ColNumber = ColNumber+1
           iRet = fsql3_bind_int(IoutDBref,ColNumber,ICR(I))

           ColNumber = ColNumber+1
           XB = CRWDTH(I)*FTtoM
           iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

           ColNumber = ColNumber+1
           iRet = fsql3_bind_int(IoutDBref,ColNumber,IDMR)

           ColNumber = ColNumber+1
           XB = PCT(I)
           iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

           ColNumber = ColNumber+1
           iRet = fsql3_bind_int(IoutDBref,ColNumber,IPTBAL)

           ColNumber = ColNumber+1
           XB = CFV(I)*FT3toM3
           iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

           ColNumber = ColNumber+1
           XB = WK1(I)*FT3toM3
           iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

           ColNumber = ColNumber+1
           XB = BFV(I)*FT3toM3
           iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

           ColNumber = ColNumber+1
           iRet = fsql3_bind_int(IoutDBref,ColNumber,ICDF)

           ColNumber = ColNumber+1
           iRet = fsql3_bind_int(IoutDBref,ColNumber,IBDF)

           ColNumber = ColNumber+1
           iRet = fsql3_bind_int(IoutDBref,ColNumber,ITRNK)

           ColNumber = ColNumber+1
           XB = ESTHT*FTtoM
           iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

           ColNumber = ColNumber+1
           iRet = fsql3_bind_int(IoutDBref,ColNumber,IPVEC(ITRE(I)))

           ColNumber = ColNumber+1
           XB = HT2TD(I,2)*FTtoM
           iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

           ColNumber = ColNumber+1
           XB = HT2TD(I,1)*FTtoM
           iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

           ColNumber = ColNumber+1
           XB = TREAGE
           iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

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

     ITRNK = INT(real((ITRUNC(I)+5)*.01*FTtoM))

     !       DETERMINE ESTIMATED HEIGHT
     !       ESTIMATED HEIGHT IS NORMAL HEIGHT, UNLESS THE IT WAS NOT
     !       BEEN SET, IN WHICH CASE IT IS EQUAL TO CURRENT HEIGHT

     if (NORMHT(I) .ne.0) then
        ESTHT = ((real(NORMHT(I))+5)/100)
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
     iRet = fsql3_bind_int(IoutDBref,ColNumber,IPVEC(ITRE(I)))

     ColNumber = ColNumber+1
     XB = P/ACRtoHA
     iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

     ColNumber = ColNumber+1
     XB = DP/ACRtoHA
     iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

     ColNumber = ColNumber+1
     XB = DBH(I)*INtoCM
     iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

     ColNumber = ColNumber+1
     XB = DGI*INtoCM
     iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

     ColNumber = ColNumber+1
     XB = HT(I)*FTtoM
     iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

     ColNumber = ColNumber+1
     XB = HTG(I)*FTtoM
     iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

     ColNumber = ColNumber+1
     iRet = fsql3_bind_int(IoutDBref,ColNumber,ICR(I))

     ColNumber = ColNumber+1
     XB = CRWDTH(I)*FTtoM
     iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

     ColNumber = ColNumber+1
     iRet = fsql3_bind_int(IoutDBref,ColNumber,IDMR)

     ColNumber = ColNumber+1
     XB = PCT(I)
     iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

     ColNumber = ColNumber+1
     iRet = fsql3_bind_int(IoutDBref,ColNumber,IPTBAL)

     ColNumber = ColNumber+1
     XB = CFV(I)*FT3toM3
     iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

     ColNumber = ColNumber+1
     XB = WK1(I)*FT3toM3
     iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

     ColNumber = ColNumber+1
     XB = BFV(I)*FT3toM3
     iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

     ColNumber = ColNumber+1
     iRet = fsql3_bind_int(IoutDBref,ColNumber,ICDF)

     ColNumber = ColNumber+1
     iRet = fsql3_bind_int(IoutDBref,ColNumber,IBDF)

     ColNumber = ColNumber+1
     iRet = fsql3_bind_int(IoutDBref,ColNumber,ITRNK)

     ColNumber = ColNumber+1
     XB = ESTHT*FTtoM
     iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

     ColNumber = ColNumber+1
     iRet = fsql3_bind_int(IoutDBref,ColNumber,IPVEC(ITRE(I)))

     ColNumber = ColNumber+1
     XB = HT2TD(I,2)*FTtoM
     iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

     ColNumber = ColNumber+1
     XB = HT2TD(I,1)*FTtoM
     iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

     ColNumber = ColNumber+1
     XB = TREAGE
     iRet = fsql3_bind_double(IoutDBref,ColNumber,XB)

     iRet = fsql3_step(IoutDBref)
     iRet = fsql3_reset(IoutDBref)
  end do
  iRet = fsql3_finalize(IoutDBref)
  iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))

  return
end
