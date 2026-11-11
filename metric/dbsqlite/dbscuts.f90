!===============================!
!   metric/dbsqlite : dbscuts   !
!===============================!
! METRIC-DBSQLITE $Id: dbscuts.f 2667 2019-05-13 19:38:28Z lancedavid $
subroutine DBSCUTS(IWHO,KODE)

  !     PURPOSE: TO OUTPUT THE CUTS LIST DATA TO THE DATABASE

  !     INPUT: IWHO  - THE WHO CALLED ME VALUE WHICH MUST BE 2
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
  character(len=23) :: TBLNAME
  character(len=5) :: NTCUFT
  character(len=5) :: NMCUFT
  character(len=5) :: NBDFT
  character(len=8) :: NAMDCF
  character(len=8) :: NAMDBF
  character(len=2000) :: SQLStmtStr
  integer :: IWHO,I,JYR,IP,ITPLAB,IRCODE,IDMR,ICDF,IBDF,IPTBAL,KODE
  integer :: ISPC,I1,I2,I3
  integer*4 :: IDCMP1,IDCMP2
  data IDCMP1,IDCMP2/10000000,20000000/
  real    :: CW,P,DGI,DP,ESTHT,TREAGE

  integer :: fsql3_tableexists,fsql3_exec

  !     IF CUTSOUT IS NOT TURNED ON OR THE IWHO VARIABLE IS NOT 2
  !     THEN JUST RETURN

  if (ICUTLIST.eq.0.or.IWHO.ne.2) return

  !     IS THIS OUTPUT A REDIRECT OF THE REPORT THEN SET KODE TO 0

  if (ICUTLIST.eq.2) KODE = 0

  !     ALWAYS CALL CASE TO MAKE SURE WE HAVE AN UP TO DATE CASE NUMBER

  call DBSCASE(1)

  !     For CS, LS, NE, SN and ON, the table name is FVS_TreeList_East and the following
  !     Column names change from: TCuFt, MCuFt, BdFt to MCuFt, SCuFt, SBdFt

  if (VARACD.eq.'CS' .or. VARACD.eq.'LS' .or. VARACD.eq.'SN' .or.&
       &VARACD.eq.'NE' .or. VARACD.eq.'ON') then
     TBLNAME = 'FVS_CutList_East_Metric'
     NTCUFT = 'TCuM'
     NMCUFT = 'MCuM'
     NBDFT = 'CCuM'
     NAMDCF = 'Ht2TDMCM'
     NAMDBF = 'Ht2TDSCM'
  else
     TBLNAME = 'FVS_CutList_Metric'
     NTCUFT = 'TCuM'
     NMCUFT = 'MCuM'
     NBDFT = 'CCuM'
     NAMDCF = 'Ht2TDCM '
     NAMDBF = 'Ht2TDBM '
  end if

  !     CHECK TO SEE IF THE TREELIST TABLE EXISTS IN DATBASE
  !     IF IT DOESNT THEN WE NEED TO CREATE IT

  IRCODE = fsql3_exec (IoutDBref,"Begin;"//Char(0))
  if (IRCODE .ne.0) then
     ICUTLIST = 0
     return
  end if
  IRCODE = fsql3_tableexists(IoutDBref,TRIM(TBLNAME)//CHAR(0))
  if (IRCODE.eq.0) then
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
          &'TreeAge real null);'// CHAR(0)
     IRCODE = fsql3_exec(IoutDBref,SQLStmtStr)
     if (IRCODE .ne.0) then
        ICUTLIST = 0
        return
     end if
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
           if (P.le.0.0) cycle

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

           !           LOAD SPECIES CODES FROM FVS, PLANTS AND FIA ARRAYS.

           CSPECIE1 = JSP(ISP(I))
           CSPECIE2 = PLNJSP(ISP(I))
           CSPECIE3 = FIAJSP(ISP(I))

           write(SQLStmtStr,*)'INSERT INTO ',TBLNAME,&
                &' (CaseID,StandID,Year,PrdLen,',&
                &'TreeId,TreeIndex,SpeciesFVS,SpeciesPLANTS,SpeciesFIA,',&
                &'TreeVal,SSCD,PtIndex,TPH,',&
                &'MortPH,DBH,DG,HT,HTG,PctCr,',&
                &'CrWidth,MistCD,BAPctile,PtBAL,',NTCUFT,',',&
                &NMCUFT,',',NBDFT,',MDefect,BDefect,TruncHt,',&
                &'EstHt,ActPt,',NAMDCF,',',NAMDBF,',','TreeAge) VALUES (''',&
                &CASEID,''',''',TRIM(NPLT),''',',JYR,',',&
                &IFINT,",'",TRIM(ADJUSTL(TID)),"',",I,&
                &",'",TRIM(CSPECIE1),"'",&
                &",'",TRIM(CSPECIE2),"'",&
                &",'",TRIM(CSPECIE3),"',",&
                &IMC(I),',',ISPECL(I),',',ITRE(I),&
                &',',P/ACRtoHA,',',DP/ACRtoHA,',',DBH(I)*INtoCM,&
                &',',DGI*INtoCM,',',HT(I)*FTtoM,',',HTG(I)*FTtoM,&
                &',',ICR(I),',',CW*FTtoM,',',IDMR,',',PCT(I),',',IPTBAL,&
                &',',CFV(I)*FT3toM3,',',WK1(I)*FT3toM3,&
                &',',BFV(I)*FT3toM3,',',ICDF,',',IBDF,&
                &',',NINT(FLOAT(ITRUNC(I)+5)*.01*FTtoM),',',ESTHT*FTtoM,&
                &',',IPVEC(ITRE(I)),',',HT2TD(I,2)*FTtoM,&
                &',',HT2TD(I,1)*FTtoM,',',TREAGE,');'

           IRCODE = fsql3_exec(IoutDBref,trim(SQLStmtStr)//CHAR(0))
           if (IRCODE .ne.0) then
              ICUTLIST = 0
              return
           end if
        end do
     end if
  end do
  IRCODE = fsql3_exec (IoutDBref,"Commit;"//Char(0))
  return
end
