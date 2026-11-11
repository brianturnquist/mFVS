!=========================!
!   vdbsqlite : dbsfmpf   !
!=========================!
! VDBSQLITE $Id$
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
  integer,parameter :: MxMsg = 500
  character(LEN = MxMsg) :: Msg
  integer :: IYEAR,CNPYHT,SMORTBA,MMORTBA,SMORTVOL,MMORTVOL,KODE,iRet
  integer :: FUELMOD,SFUELMOD,ColNumber
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
  character(len=26) :: NPLT

  integer :: fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,fsql3_prepare,fsql3_bind_double,&
       &fsql3_finalize,fsql3_errmsg

  !     Initialize variables

  if (IPOTFIRE.eq.0) return
  if (IPOTFIRE.eq.2) KODE = 0

  !     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

  call DBSCASE(1)

  if ((VARACD .eq. 'SN') .or. (VARACD .eq. 'CS')) then
     iRet = fsql3_tableexists(IoutDBref,&
          &"FVS_PotFire_East"//CHAR(0))
     if (iRet.eq.0) then
        SQLStmtStr = 'CREATE TABLE FVS_PotFire_East('//&
             &'CaseID text not null,'//&
             &'StandID text not null,'//&
             &'Year int null,'//&
             &'Flame_Len_Sev real null,'//&
             &'Flame_Len_Mod real null,'//&
             &'Canopy_Ht int null,'//&
             &'Canopy_Density real null,'//&
             &'Mortality_BA_Sev real null,'//&
             &'Mortality_BA_Mod real null,'//&
             &'Mortality_VOL_Sev real null,'//&
             &'Mortality_VOL_Mod real null,'//&
             &'Pot_Smoke_Sev real null,'//&
             &'Pot_Smoke_Mod real null,'//&
             &'Fuel_Mod1_Sev int null,'//&
             &'Fuel_Mod2_Sev int null,'//&
             &'Fuel_Mod3_Sev int null,'//&
             &'Fuel_Mod4_Sev int null,'//&
             &'Fuel_Wt1_Sev real null,'//&
             &'Fuel_Wt2_Sev real null,'//&
             &'Fuel_Wt3_Sev real null,'//&
             &'Fuel_Wt4_Sev real null,'//&
             &'Fuel_Mod1_Mod int null,'//&
             &'Fuel_Mod2_Mod int null,'//&
             &'Fuel_Mod3_Mod int null,'//&
             &'Fuel_Mod4_Mod int null,'//&
             &'Fuel_Wt1_Mod real null,'//&
             &'Fuel_Wt2_Mod real null,'//&
             &'Fuel_Wt3_Mod real null,'//&
             &'Fuel_Wt4_Mod real null);'//CHAR(0)
        iRet = fsql3_exec(IoutDBref,SQLStmtStr)
        if (iRet.ne.0) then
           iRet = fsql3_errmsg(IinDBref, Msg, MxMsg)
           print *,"FVS_PotFire_East exec direct east error:",&
                &Msg(:iRet)
           IPOTFIRE = 0
           return
        end if
     end if
  else !NOT SN VARIANT
     iRet = fsql3_tableexists(IoutDBref,&
          &"FVS_PotFire"//CHAR(0))
     if (iRet.eq.0) then
        SQLStmtStr = 'CREATE TABLE FVS_PotFire ('//&
             &'CaseID text not null,'//&
             &'StandID text not null,'//&
             &'Year int null,'//&
             &'Surf_Flame_Sev real null,'//&
             &'Surf_Flame_Mod real null,'//&
             &'Tot_Flame_Sev real null,'//&
             &'Tot_Flame_Mod real null,'//&
             &'Fire_Type_Sev text null,'//&
             &'Fire_Type_Mod text null,'//&
             &'PTorch_Sev real null,'//&
             &'PTorch_Mod real null,'//&
             &'Torch_Index real null,'//&
             &'Crown_Index real null,'//&
             &'Canopy_Ht int null,'//&
             &'Canopy_Density real null,'//&
             &'Mortality_BA_Sev real null,'//&
             &'Mortality_BA_Mod real null,'//&
             &'Mortality_VOL_Sev real null,'//&
             &'Mortality_VOL_Mod real null,'//&
             &'Pot_Smoke_Sev real null,'//&
             &'Pot_Smoke_Mod real null,'//&
             &'Fuel_Mod1 int null,'//&
             &'Fuel_Mod2 int null,'//&
             &'Fuel_Mod3 int null,'//&
             &'Fuel_Mod4 int null,'//&
             &'Fuel_Wt1 real null,'//&
             &'Fuel_Wt2 real null,'//&
             &'Fuel_Wt3 real null,'//&
             &'Fuel_Wt4 real null);'//CHAR(0)
        iRet = fsql3_exec(IoutDBref,SQLStmtStr)
        if (iRet.ne.0) then
           iRet = fsql3_errmsg(IinDBref, Msg, MxMsg)
           print *,"FVS_PotFire west exec direct error:",Msg(:iRet)
           IPOTFIRE = 0
           return
        end if
     end if
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

  BFUELWT(1) = DBLE(INT((FUELWT(1)*100.)+0.5))
  BFUELWT(2) = DBLE(INT((FUELWT(2)*100.)+0.5))
  BFUELWT(3) = DBLE(INT((FUELWT(3)*100.)+0.5))
  BFUELWT(4) = DBLE(INT((FUELWT(4)*100.)+0.5))

  if ((VARACD .eq. 'SN') .or. (VARACD .eq. 'CS')) then
     BSFUELWT(1) = DBLE(INT((SFUELWT(1)*100.)+0.5))
     BSFUELWT(2) = DBLE(INT((SFUELWT(2)*100.)+0.5))
     BSFUELWT(3) = DBLE(INT((SFUELWT(3)*100.)+0.5))
     BSFUELWT(4) = DBLE(INT((SFUELWT(4)*100.)+0.5))
     write(SQLStmtStr,*)'INSERT INTO FVS_PotFire_East (CaseID,',&
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
          &',?,?,?,?);'
  else
     write(SQLStmtStr,*)'INSERT INTO FVS_PotFire (CaseID,',&
          &'StandID,Year,Surf_Flame_Sev,Surf_Flame_Mod,',&
          &'Tot_Flame_Sev,Tot_Flame_Mod,Fire_Type_Sev,Fire_Type_Mod,',&
          &'PTorch_Sev,PTorch_Mod,Torch_Index,Crown_Index,',&
          &'Canopy_Ht,Canopy_Density,Mortality_BA_Sev,',&
          &'Mortality_BA_Mod,Mortality_VOL_Sev,Mortality_VOL_Mod,',&
          &'Pot_Smoke_Sev,Pot_Smoke_Mod,Fuel_Mod1,Fuel_Mod2,',&
          &'Fuel_Mod3,Fuel_Mod4,Fuel_Wt1,Fuel_Wt2,',&
          &'Fuel_Wt3,Fuel_Wt4) VALUES (''',CASEID,''',''',TRIM(NPLT),&
          &''',?,?,?,?,?,''',TRIM(SFTYPE),''',''',TRIM(MFTYPE),&
          &''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'
  end if

  iRet = fsql3_prepare(IoutDBref, TRIM(SQLStmtStr)//CHAR(0))

  if (iRet.ne.0) then
     iRet = fsql3_errmsg(IinDBref, Msg, MxMsg)
     print *,"FVS_PotFire prepare error:",Msg(:iRet)
     IPOTFIRE = 0
     return
  end if

  ColNumber = 1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,BSFLMSU)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,BMFLMSU)

  if ((VARACD .ne. 'SN') .and. (VARACD .ne. 'CS')) then
     ColNumber = ColNumber+1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,BSFLMTO)

     ColNumber = ColNumber+1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,BMFLMTO)

     ColNumber = ColNumber+1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,BSPTRCH)

     ColNumber = ColNumber+1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,BMPTRCH)

     ColNumber = ColNumber+1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,BTORCHI)

     ColNumber = ColNumber+1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,BCROWNI)
  end if

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref, ColNumber,CNPYHT)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,BCNPYDNST)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,SMORTBA)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,MMORTBA)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,SMORTVOL)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,MMORTVOL)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,BSPSMOKE)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,BMPSMOKE)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,FUELMOD(1))

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,FUELMOD(2))

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,FUELMOD(3))

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,FUELMOD(4))

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,BFUELWT(1))

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,BFUELWT(2))

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,BFUELWT(3))

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,BFUELWT(4))

  if ((VARACD .eq. 'SN') .or. (VARACD .eq. 'CS')) then
     ColNumber = ColNumber+1
     iRet = fsql3_bind_int(IoutDBref,ColNumber,SFUELMOD(1))

     ColNumber = ColNumber+1
     iRet = fsql3_bind_int(IoutDBref,ColNumber,SFUELMOD(2))

     ColNumber = ColNumber+1
     iRet = fsql3_bind_int(IoutDBref,ColNumber,SFUELMOD(3))

     ColNumber = ColNumber+1
     iRet = fsql3_bind_int(IoutDBref,ColNumber,SFUELMOD(4))

     ColNumber = ColNumber+1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,BSFUELWT(1))

     ColNumber = ColNumber+1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,BSFUELWT(2))

     ColNumber = ColNumber+1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,BSFUELWT(3))

     ColNumber = ColNumber+1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,BSFUELWT(4))

  end if

  iRet = fsql3_step(IoutDBref)
  if (iRet.ne.0) then
     iRet = fsql3_errmsg(IinDBref, Msg, MxMsg)
     print *,"FVS_PotFire step error:",Msg(:iRet)
     IPOTFIRE = 0
  end if
  iRet = fsql3_finalize(IoutDBref)
  if (iRet.ne.0) then
     iRet = fsql3_errmsg(IinDBref, Msg, MxMsg)
     print *,"FVS_PotFire finalize error:",Msg(:iRet)
     IPOTFIRE = 0
  end if
  return

end
