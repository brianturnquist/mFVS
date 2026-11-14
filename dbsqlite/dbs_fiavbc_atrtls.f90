!==================================!
!   dbsqlite : dbs_fiavbc_atrtls   !
!==================================!
! DBSQLITE $Id$
subroutine DBS_FIAVBC_ATRTLS

  !     PURPOSE: TO OUTPUT THE ATRTLIST DATA TO THE DATABASE

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
  character(len=8) :: CSPECIE1
  character(len=8) :: CSPECIE2
  character(len=8) :: CSPECIE3
  character(len=19) :: TBLNAME
  character(len=2000) :: SQLStmtStr
  integer :: I,IP,iRet
  integer :: ISPC,I1,I2,I3,ITRNK
  integer*4 :: IDCMP1,IDCMP2
  data IDCMP1,IDCMP2/10000000,20000000/

  integer :: ColNumber
  real*8  :: P,DP,DCFV,DMCFV,DSCFV,DAGBIO,DMERCHBIO,DSAWBIO,DFOLIBIO,&
       &DAGCARB,DMERCHCARB,DSAWCARB,DFOLICARB

  integer :: fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,fsql3_prepare,fsql3_bind_double,&
       &fsql3_bind_text,fsql3_reset

  !     IF VBC AFTER TREATMENT LIST IS NOT TURNED ON OR FIAVBC NOT ACTIVE
  !     THEN JUST RETURN

  if (IVBCATRLST.eq.0) return
  if (.not.LFIANVB) then
     call ERRGRO(.true.,52)
     return
  end if

  !     ALWAYS CALL CASE TO MAKE SURE WE HAVE AN UP TO DATE CASE NUMBER

  call DBSCASE(1)

  !     UPDATED TO CONSOLIDATE TABLES.  DISTINCT EAST/WEST TABLES NO LONGER NEEDED (2024 DW)

  TBLNAME = 'FVS_FIAVBC_ATRTList'

  iRet = fsql3_exec (IoutDBref,"Begin;"//Char(0))

  !     CHECK TO SEE IF THE TREELIST TABLE EXISTS IN DATBASE
  !     IF IT DOESNT THEN WE NEED TO CREATE IT

  iRet = fsql3_tableexists(IoutDBref,TRIM(TBLNAME)//CHAR(0))
  if (iRet.eq.0) then
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
     iRet = fsql3_exec(IoutDBref,SQLStmtStr)
     if (iRet .ne.0) then
        iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))
        IVBCATRLST = 0
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
     IVBCATRLST = 0
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
           IP = 0
           do I = 1,ITRN
              if (PROB(I).gt.0) IP = IP+1
           end do
           I = IND1(I3)
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

           ! Shared numeric fields; retain writer-specific reporting above and below.
           call DBS_FIAVBC_BINDTREE(I,ITRNK,ColNumber,iRet)

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
  iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))
  return
end
