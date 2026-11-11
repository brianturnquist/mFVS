!==================================!
!   dbsqlite : dbs_fiavbc_cutlst   !
!==================================!
! DBSQLITE $Id$
subroutine DBS_FIAVBC_CUTLST()

  !     PURPOSE: TO OUTPUT THE FIAVBC CUTS LIST DATA TO THE DATABASE

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
  character(len=18) :: TBLNAME
  character(len=2000) :: SQLStmtStr
  integer :: I,IP,IRCODE,ITRNK
  integer :: ISPC,I1,I2,I3,iRet
  integer*4 :: IDCMP1,IDCMP2
  data IDCMP1,IDCMP2/10000000,20000000/

  integer :: ColNumber
  real*8  :: P,DDBH,DP,DHT,ESTHT,DCFV,DMCFV,DSCFV,DCULL,DCRBFRC,DAGBIO,DMERCHBIO,DSAWBIO,DFOLIBIO,&
       &DAGCARB,DMERCHCARB,DSAWCARB,DFOLICARB

  integer :: fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,fsql3_prepare,fsql3_bind_double,&
       &fsql3_bind_text,fsql3_reset

  !     IF IVBCCUTLST IS NOT TURNED ON OR THE FIAVBC NOT ACTIVE
  !     THEN JUST RETURN

  if (IVBCCUTLST.eq.0) return
  if (.not.LFIANVB) then
     call ERRGRO(.true.,52)
     return
  end if

  !     ALWAYS CALL CASE TO MAKE SURE WE HAVE AN UP TO DATE CASE NUMBER

  call DBSCASE(1)

  TBLNAME = 'FVS_FIAVBC_CutList'

  !     CHECK TO SEE IF THE TREELIST TABLE EXISTS IN DATBASE
  !     IF IT DOESNT THEN WE NEED TO CREATE IT

  IRCODE = fsql3_exec (IoutDBref,"Begin;"//Char(0))
  if (IRCODE .ne.0) then
     IVBCCUTLST = 0
     return
  end if
  IRCODE = fsql3_tableexists(IoutDBref,TRIM(TBLNAME)//CHAR(0))
  if (IRCODE.eq.0) then
     SQLStmtStr = 'CREATE TABLE ' // TRIM(TBLNAME) //&
          &' (CaseID text not null,'//&
          &'StandID text not null,'//&
          &'PtIndex int null,'//&
          &'ActPt int null,'//&
          &'Year int null,'//&
          &'TreeId text null,'//&
          &'TreeIndex int null,'//&
          &'SpeciesFVS text null,'//&
          &'SpeciesPLANTS text null,'//&
          &'SpeciesFIA text null,'//&
          &'TPA real null,'//&
          &'MortTPA real null,'//&
          &'DBH real null,'//&
          &'Ht real null,'//&
          &'EstHt real null,' //&
          &'TruncHt int null,'//&
          &'PctCr int null,'//&
          &'Cull real null,' //&
          &'WdldStem int null,' //&
          &'DecayCd int null,' //&
          &'CarbFrac real null,' //&
          &'TCuFt real null,'//&
          &'MCuFt real null,'//&
          &'SCuFt real null,'//&
          &'AbvGrdBio real null,'//&
          &'MerchBio real null,'//&
          &'SawBio real null,'//&
          &'FoliBio real null,' //&
          &'AbvGrdCarb real null,'//&
          &'MerchCarb real null,'//&
          &'SawCarb real null,'//&
          &'FoliCarb real null);'//CHAR(0)

     IRCODE = fsql3_exec(IoutDBref,SQLStmtStr)
     if (IRCODE .ne.0) then
        IVBCCUTLST = 0
        iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))
        return
     end if
  end if

  write(SQLStmtStr,*)'INSERT INTO ',TBLNAME,&
       &' (CaseID,StandID,PtIndex,ActPt,Year,TreeId,TreeIndex,',& !1
       &'SpeciesFVS,SpeciesPLANTS,SpeciesFIA,',& !2
       &'TPA,MortTPA,DBH,Ht,EstHt,TruncHt,PctCr,',& !3
       &'Cull,WdldStem,DecayCd,CarbFrac,',& !4
       &'TCuFt,MCuFt,SCuFt,',& !5
       &'AbvGrdBio,MerchBio,SawBio,FoliBio,',& !6
       &'AbvGrdCarb,MerchCarb,SawCarb,FoliCarb) ',& !7
       &' VALUES (''',&
       &CASEID,''',''',TRIM(NPLT),''',?,?,',IY(ICYC),',?,?,',& !1
       &'?,?,?,',& !2
       &'?,?,?,?,?,?,?,',& !3
       &'?,?,?,?,',& !4
       &'?,?,?,',& !5
       &'?,?,?,?,',& !6
       &'?,?,?,?);' !7

  iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
  if (iRet .ne.0) then
     IVBCCUTLST = 0
     iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))
     return
  end if

  !     SET THE CUTS LIST TYPE FLAG (LET IP BE THE RECORD OUTPUT COUNT).

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

           !           DETERMINE ESTIMATED HEIGHT
           !           ESTIMATED HEIGHT IS NORMAL HEIGHT, UNLESS THE LATTER HAS NOT
           !           BEEN SET, IN WHICH CASE IT IS EQUAL TO CURRENT HEIGHT
           ! IF (NORMHT(I) .NE. 0) THEN
           !   ESTHT = (REAL(NORMHT(I))+5)/100
           ! ELSE
           !   ESTHT = HT(I)
           ! ENDIF

           !           SET TRUNCATED (TOPKILL) HEIGHT

           ITRNK = INT((ITRUNC(I)+5)/100)

           !           LOAD SPECIES CODES FROM FVS, PLANTS AND FIA ARRAYS.

           CSPECIE1 = JSP(ISP(I))
           CSPECIE2 = PLNJSP(ISP(I))
           CSPECIE3 = FIAJSP(ISP(I))

           ColNumber = 1
           iRet = fsql3_bind_int(IoutDBref,ColNumber,ITRE(I)) !PtIndex (Plot or point index)

           ColNumber = ColNumber+1
           iRet = fsql3_bind_int(IoutDBref,ColNumber,IPVEC(ITRE(I))) !ActPt

           ColNumber = ColNumber+1
           iRet = fsql3_bind_text(IoutDBref,ColNumber,TID,&
                &LEN_TRIM(TID)) !TreeID

           ColNumber = ColNumber+1
           iRet = fsql3_bind_int(IoutDBref,ColNumber,I) !TreeIndex

           ColNumber = ColNumber+1
           iRet = fsql3_bind_text(IoutDBref,ColNumber,CSPECIE1,&
                &LEN_TRIM(CSPECIE1)) !SpeciesFVS

           ColNumber = ColNumber+1
           iRet = fsql3_bind_text(IoutDBref,ColNumber,CSPECIE2,&
                &LEN_TRIM(CSPECIE2)) !SpeciesPLANTS

           ColNumber = ColNumber+1
           iRet = fsql3_bind_text(IoutDBref,ColNumber,CSPECIE3,&
                &LEN_TRIM(CSPECIE3)) !SpeciesFIA

           ColNumber = ColNumber+1
           iRet = fsql3_bind_double(IoutDBref,ColNumber,P) !TPA

           ColNumber = ColNumber+1
           iRet = fsql3_bind_double(IoutDBref,ColNumber,DP) !MortTPA

           DDBH = DBH(I)
           ColNumber = ColNumber+1
           iRet = fsql3_bind_double(IoutDBref,ColNumber,DDBH) !DBH

           DHT = HT(I)
           ColNumber = ColNumber+1
           iRet = fsql3_bind_double(IoutDBref,ColNumber,DHT) !Ht

           if (NORMHT(I) .gt.0) then
              ESTHT = (real(NORMHT(I))+5)/100
           else
              ESTHT = 0
           end if
           ColNumber = ColNumber+1
           iRet = fsql3_bind_double(IoutDBref,ColNumber,ESTHT) !NormHt

           ColNumber = ColNumber+1
           iRet = fsql3_bind_int(IoutDBref,ColNumber,ITRNK) !TruncHt

           ColNumber = ColNumber+1
           iRet = fsql3_bind_int(IoutDBref,ColNumber,ICR(I)) !PctCr (Percent live crown)

           !           Add FIA Cull, Number WoodlandStems, Decay Code, Carbon fraction
           ColNumber = ColNumber+1
           DCULL = CULL(I)
           iRet = fsql3_bind_double(IoutDBref,ColNumber,DCULL) !Cull

           ColNumber = ColNumber+1
           iRet = fsql3_bind_int(IoutDBref,ColNumber,WDLDSTEM(I)) !WdldStem

           ColNumber = ColNumber+1
           iRet = fsql3_bind_int(IoutDBref,ColNumber,DECAYCD(I)) !DecayCd

           ColNumber = ColNumber+1
           DCRBFRC = CARB_FRAC(I)
           iRet = fsql3_bind_double(IoutDBref,ColNumber,DCRBFRC) !CarbFrac

           !           Add cubic volume outputs
           ColNumber = ColNumber+1
           DCFV = CFV(I)
           iRet = fsql3_bind_double(IoutDBref,ColNumber,DCFV) !TCuFt

           ColNumber = ColNumber+1
           DMCFV = MCFV(I)
           iRet = fsql3_bind_double(IoutDBref,ColNumber,DMCFV) !MCuFt

           ColNumber = ColNumber+1
           DSCFV = SCFV(I)
           iRet = fsql3_bind_double(IoutDBref,ColNumber,DSCFV) !SCuFt

           !           Add biomass and carbon outputs
           ColNumber = ColNumber+1
           DAGBIO = ABVGRD_BIO(I)
           iRet = fsql3_bind_double(IoutDBref,ColNumber,DAGBIO) !AbvGrdBio

           ColNumber = ColNumber+1
           DMERCHBIO = MERCH_BIO(I)
           iRet = fsql3_bind_double(IoutDBref,ColNumber,DMERCHBIO) !MerchBio

           ColNumber = ColNumber+1
           DSAWBIO = CUBSAW_BIO(I)
           iRet = fsql3_bind_double(IoutDBref,ColNumber,DSAWBIO) !SawBio

           ColNumber = ColNumber+1
           DFOLIBIO = FOLI_BIO(I)
           iRet = fsql3_bind_double(IoutDBref,ColNumber,DFOLIBIO) !FoliBio

           ColNumber = ColNumber+1
           DAGCARB = ABVGRD_CARB(I)
           iRet = fsql3_bind_double(IoutDBref,ColNumber,DAGCARB) !AbvGrdCarb

           ColNumber = ColNumber+1
           DMERCHCARB = MERCH_CARB(I)
           iRet = fsql3_bind_double(IoutDBref,ColNumber,DMERCHCARB) !MerchCarb

           ColNumber = ColNumber+1
           DSAWCARB = CUBSAW_CARB(I)
           iRet = fsql3_bind_double(IoutDBref,ColNumber,DSAWCARB) !SawCarb

           ColNumber = ColNumber+1
           DFOLICARB = FOLI_CARB(I)
           iRet = fsql3_bind_double(IoutDBref,ColNumber,DFOLICARB) !FoliCarb

           iRet = fsql3_step(IoutDBref)
           iRet = fsql3_reset(IoutDBref)
        end do
     end if
  end do
  IRCODE = fsql3_exec (IoutDBref,"Commit;"//Char(0))
  return
end
