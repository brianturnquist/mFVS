!==========================!
!   dbsqlite : dbsatrtls   !
!==========================!
! DBSQLITE $Id$
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
  include 'wdbkwtdata.inc'

  ! Variables
  character(len=8) :: TID
  character(len=8) :: CSPECIE1
  character(len=8) :: CSPECIE2
  character(len=8) :: CSPECIE3
  character(len=17) :: TBLNAME
  character(len=5) :: NTCUFT
  character(len=5) :: NMCUFT
  character(len=5) :: NSCUFT
  character(len=5) :: NBDFT
  character(len=8) :: NAMDCF
  character(len=8) :: NAMDBF
  character(len=2000) :: SQLStmtStr
  integer :: IWHO,I,JYR,IP,ITPLAB,IRCODE,IDMR,ICDF,IBDF,IPTBAL,KODE
  integer :: ISPC,I1,I2,I3
  integer*4 :: IDCMP1,IDCMP2
  data IDCMP1,IDCMP2/10000000,20000000/
  real    :: CW,P,DGI,DP,TEM,ESTHT,TREAGE

  integer :: fsql3_tableexists,fsql3_exec,fsql3_addcolifabsent,iRet

  !     IF TREEOUT IS NOT TURNED ON OR THE IWHO VARIABLE IS NOT 1
  !     THEN JUST RETURN

  if (IATRTLIST.eq.0.or.IWHO.ne.3) return

  !     IS THIS OUTPUT A REDIRECT OF THE REPORT THEN SET KODE TO 0

  if (IATRTLIST.eq.2) KODE = 0

  !     ALWAYS CALL CASE TO MAKE SURE WE HAVE AN UP TO DATE CASE NUMBER

  call DBSCASE(1)

  !     UPDATED TO CONSOLIDATE TABLES.  DISTINCT EAST/WEST TABLES NO LONGER NEEDED (2024 DW)

  TBLNAME = 'FVS_ATRTList'
  NTCUFT = 'TCuFt'
  NMCUFT = 'MCuFt'
  NSCUFT = 'SCuFt'
  NBDFT = 'BdFt'
  NAMDCF = 'Ht2TDCF '
  NAMDBF = 'Ht2TDBF '

  IRCODE = fsql3_exec (IoutDBref,"Begin;"//Char(0))

  !     CHECK TO SEE IF THE TREELIST TABLE EXISTS IN DATBASE
  !     IF IT DOESNT THEN WE NEED TO CREATE IT

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
          &'MDefect int null,'//&
          &'BDefect int null,'//&
          &'TruncHt int null,'//&
          &'EstHt real null,'//&
          &'ActPt int null,'//&
          &NAMDCF // ' real null,'//&
          &NAMDBF // ' real null,'//&
          &'TreeAge real null);' // CHAR(0)
     IRCODE = fsql3_exec(IoutDBref,SQLStmtStr)
     if (IRCODE .ne.0) then
        IRCODE = fsql3_exec (IoutDBref,"Commit;"//Char(0))
        IATRTLIST = 0
        return
     end if
  end if

  !     CHECK TABLE FOR COLUMN(S) ADDED WITH NVB UPGRADE (2024)
  !     `SCuFt`,
  !     TO ACCOUNT FOR ADDING TO DATABASE CREATED PROIR TO UPGRADE

  iRet = fsql3_addcolifabsent(IoutDBref,TRIM(TBLNAME)//CHAR(0),&
       &"SCuFt"//CHAR(0),"real"//CHAR(0))

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
           if (P.le.0.0) cycle

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

           !           LOAD SPECIES CODES FROM FVS, PLANTS AND FIA ARRAYS.

           CSPECIE1 = JSP(ISP(I))
           CSPECIE2 = PLNJSP(ISP(I))
           CSPECIE3 = FIAJSP(ISP(I))

           write(SQLStmtStr,*)'INSERT INTO ',TBLNAME,&
                &' (CaseID,StandID,Year,PrdLen,TreeId,',&
                &'TreeIndex,SpeciesFVS,SpeciesPLANTS,SpeciesFIA,TreeVal,',&
                &'SSCD,PtIndex,TPA,MortPA,DBH,',&
                &'DG,HT,HTG,PctCr,CrWidth,',&
                &'MistCD,BAPctile,PtBAL,',NTCUFT,',',NMCUFT,',',&
                &NSCUFT,',',NBDFT,',',&
                &'MDefect,BDefect,TruncHt,EstHt,ActPt,',&
                &NAMDCF,',',NAMDBF,',','TreeAge)',&
                &' VALUES (''',&
                &CASEID,''',''',TRIM(NPLT),''',',&
                &JYR,',',IFINT,",'",TRIM(ADJUSTL(TID)),"',",I,&
                &",'",TRIM(CSPECIE1),"'",&
                &",'",TRIM(CSPECIE2),"'",&
                &",'",TRIM(CSPECIE3),"',",&
                &IMC(I),',',ISPECL(I),',',ITRE(I),&
                &',',P,',',DP,',',DBH(I),',',DGI,',',HT(I),',',HTG(I),&
                &',',ICR(I),',',CW,',',IDMR,',',PCT(I),',',IPTBAL,',',&
                &CFV(I),',',MCFV(I),',',SCFV(I),',',BFV(I),',',&
                &ICDF,',',IBDF,',',&
                &((ITRUNC(I)+5)/100),',',ESTHT,',',IPVEC(ITRE(I)),&
                &',',HT2TD(I,2),',',HT2TD(I,1),',',TREAGE,');'

           IRCODE = fsql3_exec(IoutDBref,trim(SQLStmtStr)//CHAR(0))
           if (IRCODE .ne.0) then
              IATRTLIST = 0
              IRCODE = fsql3_exec (IoutDBref,"Commit;"//Char(0))
              return
           end if
        end do
     end if
  end do
  IRCODE = fsql3_exec (IoutDBref,"Commit;"//Char(0))
  return
end
