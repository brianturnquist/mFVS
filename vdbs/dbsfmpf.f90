!====================!
!   vdbs : dbsfmpf   !
!====================!
! VDBS $Id$
subroutine DBSFMPF(IYEAR,NPLT,SFLMSU,MFLMSU,SFLMTO,MFLMTO,SFTYPE,&
     &MFTYPE,SPTRCH,MPTRCH,TORCHI,CROWNI,CNPYHT,CNPYDNST,SMORTBA,&
     &MMORTBA,SMORTVOL,MMORTVOL,SPSMOKE,MPSMOKE,SFUELMOD,SFUELWT,&
     &FUELMOD,FUELWT,KODE)
  implicit none

  !     PURPOSE: TO POPULATE A DATABASE WITH THE FIRE MODELS POTENTIAL FIRE
  !              OUTPUT.
  !     AUTH: D. GAMMEL -- RMRS -- NOVEMBER 2002
  !     INPUT:
  !            THE POTFIRE OUTPUT FROM THE FIRE MODEL.
  !              1: SURFACE FLAME LENGTH SEVERE
  !              2: SURFACE FLAME LENGHT MODERATE
  !              3: TOTAL FLAME LENGTH SEVERE
  !              4: TOTAL FLAME LENGHT MODERATE
  !              5: FIRE TYPE SEVERE
  !              6: FIRE TYPE MODERATE
  !              7: P-TORCH SEVERE
  !              8: P-TORCH MODERATE
  !              9: TORCH INDEX
  !             10: CROWN INDEX
  !             11: CANOPY HEIGHT
  !             12: CANOPY DENSITY
  !             13: MORTALITY BA SEVERE
  !             14: MORTALITY BA MODERATE
  !             15: MORTALITY VOLUME SEVERE
  !             16: MORTALITY VOLUME MODERATE
  !             17: POTENTIAL SMOKE SEVERE
  !             18: POTENTIAL SMOKE MODERATE
  !             19: SEVERE FUEL MODEL
  !             20: SEVERE FUEL WEIGHT
  !             21: FUEL MODEL
  !             22: FUEL WEIGHT
  !             23: KODE FOR WHETHER OR NOT THE REPORT ALSO DUMPS TO FILE

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'DBSCOM.f90'

  ! Variables
  integer :: IYEAR,CNPYHT,SMORTBA,MMORTBA,SMORTVOL,MMORTVOL,KODE,IRCODE
  integer :: FUELMOD,SFUELMOD
  integer(SQLSMALLINT_KIND)::ColNumber
  real    :: SFLMTO,MFLMTO,TORCHI,CROWNI,CNPYDNST,SPSMOKE,MPSMOKE
  real    :: SFLMSU,MFLMSU,SPTRCH,MPTRCH
  double precision :: BSFLMTO,BMFLMTO,BTORCHI,BCROWNI
  double precision :: BSFLMSU,BMFLMSU,BSPTRCH,BMPTRCH
  double precision :: BCNPYDNST,BSPSMOKE,BMPSMOKE
  real    :: FUELWT,SFUELWT
  double precision,dimension(4)::BFUELWT,BSFUELWT
  dimension FUELMOD(4),FUELWT(4),SFUELMOD(4),SFUELWT(4)
  character(len=2000) :: SQLStmtStr
  character(len=8) :: SFTYPE
  character(len=8) :: MFTYPE
  character(len=20) :: TABLENAME
  character(len=26) :: NPLT

  !     Initialize variables

  if (IPOTFIRE.eq.0) return
  if (IPOTFIRE.eq.2) KODE = 0

  !     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

  call DBSCASE(1)

  !     ALLOCATE A STATEMENT HANDLE

  iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlOut, StmtHndlOut)
  if (iRet.ne.SQL_SUCCESS .and. iRet.ne. SQL_SUCCESS_WITH_INFO) then
     IPOTFIRE = 0
     print *,'Error connecting to data source'
     call  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlOut,&
          &'DBSSUMRY:DSN Connection')
     goto 200
  end if

  !     CHECK TO SEE IF THE POTFIRE TABLE EXISTS IN DATBASE

  if (TRIM(DBMSOUT).eq."EXCEL") then
     if ((VARACD .eq. 'SN') .or. (VARACD .eq. 'CS')) then
        TABLENAME = '[FVS_PotFire_East$]'
     else
        TABLENAME = '[FVS_PotFire$]'
     end if
  else
     if ((VARACD .eq. 'SN') .or. (VARACD .eq. 'CS')) then
        TABLENAME = 'FVS_PotFire_East'
     else
        TABLENAME = 'FVS_PotFire'
     end if
  end if
  call DBSCKNROWS(IRCODE,TABLENAME,1,TRIM(DBMSOUT).eq.'EXCEL')
  if (IRCODE.eq.2) then
     IPOTFIRE = 0
     return
  end if
  if (IRCODE.eq.1) then
     if ((VARACD .eq. 'SN') .or. (VARACD .eq. 'CS')) then

        if (TRIM(DBMSOUT).eq."ACCESS") then
           SQLStmtStr = 'CREATE TABLE FVS_PotFire_East('//&
                &'CaseID Text not null,'//&
                &'StandID Text null,'//&
                &'Year int null,'//&
                &'Flame_Len_Sev double null,'//&
                &'Flame_Len_Mod double null,'//&
                &'Canopy_Ht double null,'//&
                &'Canopy_Density double null,'//&
                &'Mortality_BA_Sev double null,'//&
                &'Mortality_BA_Mod double null,'//&
                &'Mortality_VOL_Sev double null,'//&
                &'Mortality_VOL_Mod double null,'//&
                &'Pot_Smoke_Sev double null,'//&
                &'Pot_Smoke_Mod double null,'//&
                &'Fuel_Mod1_Sev double null,'//&
                &'Fuel_Mod2_Sev double null,'//&
                &'Fuel_Mod3_Sev double null,'//&
                &'Fuel_Mod4_Sev double null,'//&
                &'Fuel_Wt1_Sev double null,'//&
                &'Fuel_Wt2_Sev double null,'//&
                &'Fuel_Wt3_Sev double null,'//&
                &'Fuel_Wt4_Sev double null,'//&
                &'Fuel_Mod1_Mod double null,'//&
                &'Fuel_Mod2_Mod double null,'//&
                &'Fuel_Mod3_Mod double null,'//&
                &'Fuel_Mod4_Mod double null,'//&
                &'Fuel_Wt1_Mod double null,'//&
                &'Fuel_Wt2_Mod double null,'//&
                &'Fuel_Wt3_Mod double null,'//&
                &'Fuel_Wt4_Mod double null)'
        else if (TRIM(DBMSOUT).eq."EXCEL") then
           SQLStmtStr = 'CREATE TABLE FVS_PotFire_East('//&
                &'CaseID Text,'//&
                &'StandID Text,'//&
                &'Year int,'//&
                &'Flame_Len_Sev number,'//&
                &'Flame_Len_Mod number,'//&
                &'Canopy_Ht number,'//&
                &'Canopy_Density number,'//&
                &'Mortality_BA_Sev number,'//&
                &'Mortality_BA_Mod number,'//&
                &'Mortality_VOL_Sev number,'//&
                &'Mortality_VOL_Mod number,'//&
                &'Pot_Smoke_Sev number,'//&
                &'Pot_Smoke_Mod number,'//&
                &'Fuel_Mod1_Sev number,'//&
                &'Fuel_Mod2_Sev number,'//&
                &'Fuel_Mod3_Sev number,'//&
                &'Fuel_Mod4_Sev number,'//&
                &'Fuel_Wt1_Sev number,'//&
                &'Fuel_Wt2_Sev number,'//&
                &'Fuel_Wt3_Sev number,'//&
                &'Fuel_Wt4_Sev number,'//&
                &'Fuel_Mod1_Mod number,'//&
                &'Fuel_Mod2_Mod number,'//&
                &'Fuel_Mod3_Mod number,'//&
                &'Fuel_Mod4_Mod number,'//&
                &'Fuel_Wt1_Mod number,'//&
                &'Fuel_Wt2_Mod number,'//&
                &'Fuel_Wt3_Mod number,'//&
                &'Fuel_Wt4_Mod number)'
        else
           SQLStmtStr = 'CREATE TABLE FVS_PotFire_East('//&
                &'CaseID char(36) not null,'//&
                &'StandID char(26) null,'//&
                &'Year int null,'//&
                &'Flame_Len_Sev real null,'//&
                &'Flame_Len_Mod real null,'//&
                &'Canopy_Ht real null,'//&
                &'Canopy_Density real null,'//&
                &'Mortality_BA_Sev real null,'//&
                &'Mortality_BA_Mod real null,'//&
                &'Mortality_VOL_Sev real null,'//&
                &'Mortality_VOL_Mod real null,'//&
                &'Pot_Smoke_Sev real null,'//&
                &'Pot_Smoke_Mod real null,'//&
                &'Fuel_Mod1_Sev real null,'//&
                &'Fuel_Mod2_Sev real null,'//&
                &'Fuel_Mod3_Sev real null,'//&
                &'Fuel_Mod4_Sev real null,'//&
                &'Fuel_Wt1_Sev real null,'//&
                &'Fuel_Wt2_Sev real null,'//&
                &'Fuel_Wt3_Sev real null,'//&
                &'Fuel_Wt4_Sev real null,'//&
                &'Fuel_Mod1_Mod real null,'//&
                &'Fuel_Mod2_Mod real null,'//&
                &'Fuel_Mod3_Mod real null,'//&
                &'Fuel_Mod4_Mod real null,'//&
                &'Fuel_Wt1_Mod real null,'//&
                &'Fuel_Wt2_Mod real null,'//&
                &'Fuel_Wt3_Mod real null,'//&
                &'Fuel_Wt4_Mod real null)'
        end if
     else !NOT SN VARIANT
        if (TRIM(DBMSOUT).eq."ACCESS") then
           SQLStmtStr = 'CREATE TABLE FVS_PotFire('//&
                &'CaseID Text not null,'//&
                &'StandID Text null,'//&
                &'Year int null,'//&
                &'Surf_Flame_Sev double null,'//&
                &'Surf_Flame_Mod double null,'//&
                &'Tot_Flame_Sev double null,'//&
                &'Tot_Flame_Mod double null,'//&
                &'Fire_Type_Sev Text null,'//&
                &'Fire_Type_Mod Text null,'//&
                &'PTorch_Sev double null,'//&
                &'PTorch_Mod double null,'//&
                &'Torch_Index double null,'//&
                &'Crown_Index double null,'//&
                &'Canopy_Ht double null,'//&
                &'Canopy_Density double null,'//&
                &'Mortality_BA_Sev double null,'//&
                &'Mortality_BA_Mod double null,'//&
                &'Mortality_VOL_Sev double null,'//&
                &'Mortality_VOL_Mod double null,'//&
                &'Pot_Smoke_Sev double null,'//&
                &'Pot_Smoke_Mod double null,'//&
                &'Fuel_Mod1 double null,'//&
                &'Fuel_Mod2 double null,'//&
                &'Fuel_Mod3 double null,'//&
                &'Fuel_Mod4 double null,'//&
                &'Fuel_Wt1 double null,'//&
                &'Fuel_Wt2 double null,'//&
                &'Fuel_Wt3 double null,'//&
                &'Fuel_Wt4 double null)'
        else if (TRIM(DBMSOUT).eq."EXCEL") then
           SQLStmtStr = 'CREATE TABLE FVS_PotFire('//&
                &'CaseID Text,'//&
                &'StandID Text,'//&
                &'Year int,'//&
                &'Surf_Flame_Sev number,'//&
                &'Surf_Flame_Mod number,'//&
                &'Tot_Flame_Sev number,'//&
                &'Tot_Flame_Mod number,'//&
                &'Fire_Type_Sev Text,'//&
                &'Fire_Type_Mod Text,'//&
                &'PTorch_Sev number,'//&
                &'PTorch_Mod number,'//&
                &'Torch_Index number,'//&
                &'Crown_Index number,'//&
                &'Canopy_Ht number,'//&
                &'Canopy_Density number,'//&
                &'Mortality_BA_Sev number,'//&
                &'Mortality_BA_Mod number,'//&
                &'Mortality_VOL_Sev number,'//&
                &'Mortality_VOL_Mod number,'//&
                &'Pot_Smoke_Sev number,'//&
                &'Pot_Smoke_Mod number,'//&
                &'Fuel_Mod1 number,'//&
                &'Fuel_Mod2 number,'//&
                &'Fuel_Mod3 number,'//&
                &'Fuel_Mod4 number,'//&
                &'Fuel_Wt1 number,'//&
                &'Fuel_Wt2 number,'//&
                &'Fuel_Wt3 number,'//&
                &'Fuel_Wt4 number)'
        else
           SQLStmtStr = 'CREATE TABLE FVS_PotFire('//&
                &'CaseID char(36) not null,'//&
                &'StandID char(26) null,'//&
                &'Year int null,'//&
                &'Surf_Flame_Sev real null,'//&
                &'Surf_Flame_Mod real null,'//&
                &'Tot_Flame_Sev real null,'//&
                &'Tot_Flame_Mod real null,'//&
                &'Fire_Type_Sev Char(8) null,'//&
                &'Fire_Type_Mod Char(8) null,'//&
                &'PTorch_Sev real null,'//&
                &'PTorch_Mod real null,'//&
                &'Torch_Index real null,'//&
                &'Crown_Index real null,'//&
                &'Canopy_Ht real null,'//&
                &'Canopy_Density real null,'//&
                &'Mortality_BA_Sev real null,'//&
                &'Mortality_BA_Mod real null,'//&
                &'Mortality_VOL_Sev real null,'//&
                &'Mortality_VOL_Mod real null,'//&
                &'Pot_Smoke_Sev real null,'//&
                &'Pot_Smoke_Mod real null,'//&
                &'Fuel_Mod1 real null,'//&
                &'Fuel_Mod2 real null,'//&
                &'Fuel_Mod3 real null,'//&
                &'Fuel_Mod4 real null,'//&
                &'Fuel_Wt1 real null,'//&
                &'Fuel_Wt2 real null,'//&
                &'Fuel_Wt3 real null,'//&
                &'Fuel_Wt4 real null)'
        end if
     end if

     iRet = fvsSQLCloseCursor(StmtHndlOut)

     iRet = fvsSQLExecDirect(StmtHndlOut,trim(SQLStmtStr),&
          &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))
     call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
          &'DBSFMPF:Creating Table: '//trim(SQLStmtStr))
  end if

  BSFLMTO = 0D0
  BMFLMTO = 0D0
  BSFLMSU = 0D0
  BMFLMSU = 0D0
  BSPTRCH = 0D0
  BMPTRCH = 0D0
  BTORCHI = 0D0
  BCROWNI = 0D0
  BCNPYDNST = 0D0
  BSPSMOKE = 0D0
  BMPSMOKE = 0D0
  BSFLMTO = SFLMTO
  BMFLMTO = MFLMTO
  BSFLMSU = SFLMSU
  BMFLMSU = MFLMSU
  BSPTRCH = SPTRCH
  BMPTRCH = MPTRCH
  BTORCHI = TORCHI
  BCROWNI = CROWNI
  BCNPYDNST = CNPYDNST
  BSPSMOKE = SPSMOKE
  BMPSMOKE = MPSMOKE

  BFUELWT(1) = INT((FUELWT(1)*100.)+0.5)
  BFUELWT(2) = INT((FUELWT(2)*100.)+0.5)
  BFUELWT(3) = INT((FUELWT(3)*100.)+0.5)
  BFUELWT(4) = INT((FUELWT(4)*100.)+0.5)

  if ((VARACD .eq. 'SN') .or. (VARACD .eq. 'CS')) then
     BSFUELWT(1) = INT((SFUELWT(1)*100.)+0.5)
     BSFUELWT(2) = INT((SFUELWT(2)*100.)+0.5)
     BSFUELWT(3) = INT((SFUELWT(3)*100.)+0.5)
     BSFUELWT(4) = INT((SFUELWT(4)*100.)+0.5)
     write(SQLStmtStr,*)'INSERT INTO ',TABLENAME,'(CaseID,',&
          &'StandID,Year,Flame_Len_Sev,Flame_Len_Mod,',&
          &'Canopy_Ht,Canopy_Density,Mortality_BA_Sev,',&
          &'Mortality_BA_Mod,Mortality_VOL_Sev,Mortality_VOL_Mod,',&
          &'Pot_Smoke_Sev,Pot_Smoke_Mod,Fuel_Mod1_mod,Fuel_Mod2_mod,',&
          &'Fuel_Mod3_mod,Fuel_Mod4_mod,Fuel_Wt1_mod,Fuel_Wt2_mod,',&
          &'Fuel_Wt3_mod,Fuel_Wt4_mod,Fuel_Mod1_Sev,',&
          &'Fuel_Mod2_Sev,Fuel_Mod3_Sev,Fuel_Mod4_Sev,',&
          &'Fuel_Wt1_Sev,Fuel_Wt2_Sev,Fuel_Wt3_Sev,',&
          &'Fuel_Wt4_Sev) VALUES (''',CASEID,''',''',TRIM(NPLT),&
          &''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,? ',&
          &',?,?,?,?)'
  else
     write(SQLStmtStr,*)'INSERT INTO ',TABLENAME,'(CaseID,',&
          &'StandID,Year,Surf_Flame_Sev,Surf_Flame_Mod,',&
          &'Tot_Flame_Sev,Tot_Flame_Mod,Fire_Type_Sev,Fire_Type_Mod,',&
          &'PTorch_Sev,PTorch_Mod,Torch_Index,Crown_Index,',&
          &'Canopy_Ht,Canopy_Density,Mortality_BA_Sev,',&
          &'Mortality_BA_Mod,Mortality_VOL_Sev,Mortality_VOL_Mod,',&
          &'Pot_Smoke_Sev,Pot_Smoke_Mod,Fuel_Mod1,Fuel_Mod2,',&
          &'Fuel_Mod3,Fuel_Mod4,Fuel_Wt1,Fuel_Wt2,',&
          &'Fuel_Wt3,Fuel_Wt4) VALUES (''',CASEID,''',''',TRIM(NPLT),&
          &''',?,?,?,?,?,''',TRIM(SFTYPE),''',''',TRIM(MFTYPE),&
          &''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'
  end if
  iRet = fvsSQLCloseCursor(StmtHndlOut)
  iRet = fvsSQLPrepare(StmtHndlOut, trim(SQLStmtStr),&
       &int(len_trim(SQLStmtStr),SQLINTEGER_KIND))

  ColNumber = 1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),IYEAR,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),BSFLMSU,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),BMFLMSU,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  if ((VARACD .ne. 'SN') .and. (VARACD .ne. 'CS')) then
     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
          &SQL_PARAM_INPUT,&
          &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
          &INT(5,SQLSMALLINT_KIND),BSFLMTO,int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
          &SQL_PARAM_INPUT,&
          &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
          &INT(5,SQLSMALLINT_KIND),BMFLMTO,int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
          &SQL_PARAM_INPUT,&
          &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
          &INT(5,SQLSMALLINT_KIND),BSPTRCH,int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
          &SQL_PARAM_INPUT,&
          &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
          &INT(5,SQLSMALLINT_KIND),BMPTRCH,int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
          &SQL_PARAM_INPUT,&
          &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
          &INT(5,SQLSMALLINT_KIND),BTORCHI,int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,&
          &SQL_PARAM_INPUT,&
          &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
          &INT(5,SQLSMALLINT_KIND),BCROWNI,int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)
  end if

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut, ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),CNPYHT,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),BCNPYDNST,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),SMORTBA,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),MMORTBA,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),SMORTVOL,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),MMORTVOL,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),BSPSMOKE,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),BMPSMOKE,int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),FUELMOD(1),int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),FUELMOD(2),int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),FUELMOD(3),int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
       &INT(0,SQLSMALLINT_KIND),FUELMOD(4),int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),BFUELWT(1),int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),BFUELWT(2),int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),BFUELWT(3),int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  ColNumber = ColNumber+1
  iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
       &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
       &INT(5,SQLSMALLINT_KIND),BFUELWT(4),int(4,SQLLEN_KIND),&
       &SQL_NULL_PTR)

  if ((VARACD .eq. 'SN') .or. (VARACD .eq. 'CS')) then
     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
          &SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
          &INT(0,SQLSMALLINT_KIND),SFUELMOD(1),int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
          &SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
          &INT(0,SQLSMALLINT_KIND),SFUELMOD(2),int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
          &SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
          &INT(0,SQLSMALLINT_KIND),SFUELMOD(3),int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
          &SQL_F_INTEGER, SQL_INTEGER,INT(15,SQLUINTEGER_KIND),&
          &INT(0,SQLSMALLINT_KIND),SFUELMOD(4),int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
          &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
          &INT(5,SQLSMALLINT_KIND),BSFUELWT(1),int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
          &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
          &INT(5,SQLSMALLINT_KIND),BSFUELWT(2),int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
          &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
          &INT(5,SQLSMALLINT_KIND),BSFUELWT(3),int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

     ColNumber = ColNumber+1
     iRet = fvsSQLBindParameter(StmtHndlOut,ColNumber,SQL_PARAM_INPUT,&
          &SQL_F_DOUBLE, SQL_DOUBLE,INT(15,SQLUINTEGER_KIND),&
          &INT(5,SQLSMALLINT_KIND),BSFUELWT(4),int(4,SQLLEN_KIND),&
          &SQL_NULL_PTR)

  end if

  iRet = fvsSQLExecute(StmtHndlOut)
  call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlOut,&
       &'DBSFMPF:Inserting Row')

  200 continue
  !Release statement handle
  iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlOut)

end
