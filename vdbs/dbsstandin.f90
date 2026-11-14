!=======================!
!   vdbs : dbsstandin   !
!=======================!
! VDBS $Id$
subroutine DBSSTANDIN(SQLSTR,LKECHO)
  implicit none

  !     PURPOSE: TO POPULATE FVS STAND LEVEL DATA FROM THE DATABASE
  !     AUTH: D. GAMMEL -- SEM -- AUGUST 2002
  !     OVERHAUL: NL CROOKTON -- RMRS MOSCOW -- SEPTEMBER 2004

  ! Common blocks
  include  'PRGPRM.f90'

  include  'ARRAYS.f90'

  include  'COEFFS.f90'

  include  'CONTRL.f90'

  include  'PLOT.f90'

  include  'OUTCOM.f90'

  include  'HTCAL.f90'

  include  'ECON.f90'

  include  'KEYCOM.f90'

  include  'MULTCM.f90'

  include  'VOLSTD.f90'

  include  'SCREEN.f90'

  include  'VARCOM.f90'

  include  'DBSCOM.f90'

  ! Variables
  character(len=100) :: ColName
  character(len=5000) :: SQLSTR
  character(len=10) :: KARD2
  character(LEN = 11) :: CHAB
  character(LEN = 11) :: CECOREG
  character(LEN = 15) :: CFotoCode
  character(LEN = LEN(DBCN)+1) :: TMP_DBCN
  character(LEN = LEN(NPLT)+1) :: CSTAND
  character(len=10) :: CSITECODE
  character(len=40) :: PHOTOREF(32)
  character(len=40) :: REF
  real    :: ARRAY2,X(1)
  real    :: XXG,FOTODATA(2)
  real(KIND = 4) :: RSTANDDATA(63)
  real    :: DUM1,XTMP
  integer(KIND = 4) :: ISTANDDATA(63)
  equivalence (RSTANDDATA,ISTANDDATA)
  integer :: J,I,KODE,FKOD,NUMPVREF,IXTMP,IXF
  integer(SQLUSMALLINT_KIND)::ColNumber
  integer(SQLSMALLINT_KIND)::NameLen,ColumnCount,DType,NDecs,Nullable
  integer(SQLULEN_KIND) :: NColSz
  logical :: LSITEISNUM,LHABISNUM,LSTDISNUM,LFMLK,LFMYES,LKECHO,LFMD
  logical :: LFOTO,LFOTO2,LECOISNUM,LFMYES2
  integer(SQLLEN_KIND)::tmpNotUsed
  integer(SQLLEN_KIND)::IY_LI,Lat_LI,Long_LI,Location_LI,Habitat_LI,Age_LI,Aspect_LI,Slope_LI,&
       &MaxSDI_LI,Elev_LI,Basal_LI,PlotArea_LI,BPDBH_LI,NumPlots_LI,NonStock_LI,SamWt_LI,Stock_LI,DGT_LI,&
       &DGM_LI,HTT_LI,HTM_LI,SiteSp_LI,SiteIndx_LI,Mort_LI,MaxB_LI,Model_LI,PhysioR_LI,ForType_LI,Stand_LI,&
       &DBCN_LI,Region_LI,Forest_LI,District_LI,Compartment_LI,Ecoregion_LI,ElevFT_LI,State_LI,Connty_LI,&
       &Fuel0_LI,Fuel1_LI,Fuel3_LI,Fuel6_LI,Fuel12_LI,FuelLt_LI,FuelDf_LI,FuelModel_LI,Fuel025_LI,&
       &Fuel251_LI,Fuel20_LI,Fuel35_LI,Fuel50_LI,FotoRef_LI,FotoCode_LI,PvRefCode_LI,FuelS025_LI,&
       &FuelS251_LI,FuelS1_LI,FuelS3_LI,FuelS6_LI,FuelS12_LI,FuelS20_LI,FuelS35_LI,FuelS50_LI

  data PHOTOREF / 'Fischer INT-96                      ',&
       &'Fischer INT-97                      ',&
       &'Fischer INT-98                      ',&
       &'                                    ',&
       &'Koski and Fischer INT-46            ',&
       &'Maxwell and Ward PNW-52             ',&
       &'Blonski and Schramel PSW-56         ',&
       &'Maxwell and Ward PNW-105            ',&
       &'Ottmar and Hardy PNW-GTR-231        ',&
       &'                                    ',&
       &'Maxwell A-89-6-82                   ',&
       &'Southwestern region compilation     ',&
       &'Maxwell and Ward PNW-51             ',&
       &'Ottmar and others Volume I          ',&
       &'Ottmar and others Volume I          ',&
       &'Ottmar and Vihnanek Volume II / IIa ',&
       &'Ottmar and others Volume III        ',&
       &'Ottmar and others Volume V / Va     ',&
       &'Ottmar and others Volume VI / VIa   ',&
       &'Maxwell A-89-1-90                   ',&
       &'Ottmar and others Volume IV         ',&
       &'Wright and others PNW-GTR-545       ',&
       &'Ottmar and others PNW-GTR-258       ',&
       &'Lynch and Horton NA-FR-25           ',&
       &'Wilcox and others NA-FR-22          ',&
       &'Scholl and Waldrop GTR-SRS-26       ',&
       &'Ottmar and others Volume VII        ',&
       &'Maxwell and Ward PNW-95             ',&
       &'Sanders and Van Lear GTR-SE-49      ',&
       &'Wade and others GTR-SE-82           ',&
       &'Blank GTR-NC-77                     ',&
       &'Popp and Lundquist RMRS-GTR-172     ' /
  IY_LI = SQL_NULL_DATA
  Lat_LI = SQL_NULL_DATA
  Long_LI = SQL_NULL_DATA
  Location_LI = SQL_NULL_DATA
  Habitat_LI = SQL_NULL_DATA
  Age_LI = SQL_NULL_DATA
  Aspect_LI = SQL_NULL_DATA
  Slope_LI = SQL_NULL_DATA
  MaxSDI_LI = SQL_NULL_DATA
  Elev_LI = SQL_NULL_DATA
  Basal_LI = SQL_NULL_DATA
  PlotArea_LI = SQL_NULL_DATA
  BPDBH_LI = SQL_NULL_DATA
  NumPlots_LI = SQL_NULL_DATA
  NonStock_LI = SQL_NULL_DATA
  SamWt_LI = SQL_NULL_DATA
  Stock_LI = SQL_NULL_DATA
  DGT_LI = SQL_NULL_DATA
  DGM_LI = SQL_NULL_DATA
  HTT_LI = SQL_NULL_DATA
  HTM_LI = SQL_NULL_DATA
  SiteSp_LI = SQL_NULL_DATA
  SiteIndx_LI = SQL_NULL_DATA
  Mort_LI = SQL_NULL_DATA
  MaxB_LI = SQL_NULL_DATA
  Model_LI = SQL_NULL_DATA
  PhysioR_LI = SQL_NULL_DATA
  ForType_LI = SQL_NULL_DATA
  Stand_LI = SQL_NULL_DATA
  DBCN_LI = SQL_NULL_DATA
  Region_LI = SQL_NULL_DATA
  Forest_LI = SQL_NULL_DATA
  District_LI = SQL_NULL_DATA
  Compartment_LI = SQL_NULL_DATA
  Ecoregion_LI = SQL_NULL_DATA
  ElevFT_LI = SQL_NULL_DATA
  State_LI = SQL_NULL_DATA
  Connty_LI = SQL_NULL_DATA
  Fuel0_LI = SQL_NULL_DATA
  Fuel1_LI = SQL_NULL_DATA
  Fuel3_LI = SQL_NULL_DATA
  Fuel6_LI = SQL_NULL_DATA
  Fuel12_LI = SQL_NULL_DATA
  FuelLt_LI = SQL_NULL_DATA
  FuelDf_LI = SQL_NULL_DATA
  Fuel025_LI = SQL_NULL_DATA
  Fuel251_LI = SQL_NULL_DATA
  FuelModel_LI = SQL_NULL_DATA
  PvRefCode_LI = SQL_NULL_DATA
  FotoRef_LI = SQL_NULL_DATA
  FotoCode_LI = SQL_NULL_DATA
  Fuel20_LI = SQL_NULL_DATA
  Fuel35_LI = SQL_NULL_DATA
  Fuel50_LI = SQL_NULL_DATA
  FuelS025_LI = SQL_NULL_DATA
  FuelS251_LI = SQL_NULL_DATA
  FuelS1_LI = SQL_NULL_DATA
  FuelS3_LI = SQL_NULL_DATA
  FuelS6_LI = SQL_NULL_DATA
  FuelS12_LI = SQL_NULL_DATA
  FuelS20_LI = SQL_NULL_DATA
  FuelS35_LI = SQL_NULL_DATA
  FuelS50_LI = SQL_NULL_DATA

  !     MAKE SURE WE HAVE AN OPEN CONNECTION

  if (ConnHndlIn.eq.-1) call DBSOPEN(DSNIN,EnvHndlIn,&
       &ConnHndlIn,DBMSIN,0,.false.,KODE)

  !     ALLOCATE A STATEMENT HANDLE

  iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlIn,StmtHndlIn)
  if (iRet.ne.SQL_SUCCESS .and.&
       &iRet.ne. SQL_SUCCESS_WITH_INFO) then
     call  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlIn,&
          &'STANDIN:DSN Connection')
  else

     !       EXECUTE QUERY

     iRet = fvsSQLExecDirect(StmtHndlIn,trim(SQLSTR),&
          &int(len_trim(SQLSTR),SQLINTEGER_KIND))
     if (iRet.ne.0) call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlIn,&
          &'STANDIN:Query: '//trim(SQLSTR))
  end if

  !     GET NUMBER OF COLUMNS RETURNED

  iRet = fvsSQLNumResultCols(StmtHndlIn,ColumnCount)

  !     INITIALIZE DATA ARRAY THAT BINDS TO COLUMNS

  do ColNumber = 1,ColumnCount

     iRet = fvsSQLDescribeCol (StmtHndlIn, ColNumber, ColName,&
          &int(LEN(ColName),SQLSMALLINT_KIND), NameLen, DType,&
          &NColSz, NDecs, Nullable)

     do I = 1, NameLen
        call UPCASE(ColName(I:I))
     end do

     !       BIND COLUMNS TO THEIR VARIABLES

     select case(ColName(1:NameLen))

     case('STAND_CN')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_CHAR,&
             &TMP_DBCN,int(LEN(TMP_DBCN),SQLLEN_KIND),&
             &DBCN_LI)

     case('STAND_ID')
        if (DType.eq.SQL_CHAR.or.DType.eq.SQL_VARCHAR.or.&
             &DType.eq.SQL_LONGVARCHAR) then

           iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_CHAR,&
                &CSTAND,int(LEN(CSTAND),SQLLEN_KIND),Stand_LI)
           LSTDISNUM = .false.
        else
           iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,&
                &ISTANDDATA(28),int(4,SQLLEN_KIND),Stand_LI)
           LSTDISNUM = .true.
        end if

     case('INV_YEAR')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,&
             &ISTANDDATA(1),int(4,SQLLEN_KIND),IY_LI)

     case('LATITUDE')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(2),int(4,SQLLEN_KIND),Lat_LI)

     case('LONGITUDE')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(3),int(4,SQLLEN_KIND),Long_LI)

     case('REGION')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,&
             &ISTANDDATA(29),int(4,SQLLEN_KIND),Region_LI)

     case('FOREST')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,&
             &ISTANDDATA(30),int(4,SQLLEN_KIND),Forest_LI)

     case('DISTRICT')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,&
             &ISTANDDATA(31),int(4,SQLLEN_KIND),District_LI)

     case('COMPARTMENT')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,&
             &ISTANDDATA(32),int(4,SQLLEN_KIND),Compartment_LI)

     case('ECOREGION')
        if (DType.eq.SQL_CHAR.or.DType.eq.SQL_VARCHAR.or.&
             &DType.eq.SQL_LONGVARCHAR) then
           iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_CHAR,&
                &CECOREG,int(LEN(CECOREG)-1,SQLLEN_KIND),Ecoregion_LI)
           LECOISNUM = .false.
        else
           iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,&
                &ISTANDDATA(54),int(4,SQLLEN_KIND),Ecoregion_LI)
           LECOISNUM = .true.
        end if

     case('LOCATION')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,&
             &ISTANDDATA(4),int(4,SQLLEN_KIND),Location_LI)

     case('HABITAT','PV_CODE')
        ISTANDDATA(5) = 0
        CHAB = ' '
        if (DType.eq.SQL_CHAR.or.DType.eq.SQL_VARCHAR.or.&
             &DType.eq.SQL_LONGVARCHAR) then
           iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_CHAR,&
                &CHAB,int(LEN(CHAB)-1,SQLLEN_KIND),Habitat_LI)
           LHABISNUM = .false.
        else
           iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,&
                &ISTANDDATA(5),int(4,SQLLEN_KIND),Habitat_LI)
           LHABISNUM = .true.
        end if

     case('PV_REF_CODE')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,&
             &NUMPVREF,int(4,SQLLEN_KIND),PvRefCode_LI)

     case('AGE')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,&
             &ISTANDDATA(6),int(4,SQLLEN_KIND),Age_LI)

     case('ASPECT')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(7),int(4,SQLLEN_KIND),Aspect_LI)

     case('SLOPE')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(8),int(4,SQLLEN_KIND),Slope_LI)

     case('ELEVATION')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(9),int(4,SQLLEN_KIND),Elev_LI)

     case('ELEVFT')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(33),int(4,SQLLEN_KIND),ElevFt_LI)

     case('BASAL_AREA_FACTOR')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(10),int(4,SQLLEN_KIND),Basal_LI)

     case('INV_PLOT_SIZE')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(11),int(4,SQLLEN_KIND),PlotArea_LI)

     case('BRK_DBH')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(12),int(4,SQLLEN_KIND),BPDBH_LI)

     case('NUM_PLOTS')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,&
             &ISTANDDATA(13),int(4,SQLLEN_KIND),NumPlots_LI)

     case('NONSTK_PLOTS')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,&
             &ISTANDDATA(14),int(4,SQLLEN_KIND),NonStock_LI)

     case('SAM_WT')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(15),int(4,SQLLEN_KIND),SamWt_LI)

     case('STK_PCNT')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(16),int(4,SQLLEN_KIND),Stock_LI)

     case('DG_TRANS')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,&
             &ISTANDDATA(17),int(4,SQLLEN_KIND),DGT_LI)

     case('DG_MEASURE')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,&
             &ISTANDDATA(18),int(4,SQLLEN_KIND),DGM_LI)

     case('HTG_TRANS')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,&
             &ISTANDDATA(19),int(4,SQLLEN_KIND),HTT_LI)

     case('HTG_MEASURE')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,&
             &ISTANDDATA(20),int(4,SQLLEN_KIND),HTM_LI)

     case('MORT_MEASURE')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,&
             &ISTANDDATA(21),int(4,SQLLEN_KIND),Mort_LI)

     case('SITE_SPECIES')
        if (DType.eq.SQL_CHAR.or.DType.eq.SQL_VARCHAR.or.&
             &DType.eq.SQL_LONGVARCHAR) then
           iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_CHAR,&
                &CSITECODE,int(LEN(CSITECODE)-1,SQLLEN_KIND),SiteSp_LI)
           LSITEISNUM = .false.
        else
           iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,&
                &ISTANDDATA(34),int(4,SQLLEN_KIND),SiteSp_LI)
           LSITEISNUM = .true.
        end if

     case('SITE_INDEX')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(35),int(4,SQLLEN_KIND),SiteIndx_LI)

     case('MAX_BA')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(22),int(4,SQLLEN_KIND),MaxB_LI)

     case('MAX_SDI')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(36),int(4,SQLLEN_KIND),MaxSDI_LI)

     case('MODEL_TYPE')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,&
             &ISTANDDATA(25),int(4,SQLLEN_KIND),Model_LI)

     case('PHYSIO_REGION')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,&
             &ISTANDDATA(26),int(4,SQLLEN_KIND),PhysioR_LI)

     case('FOREST_TYPE')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,&
             &ISTANDDATA(27),int(4,SQLLEN_KIND),ForType_LI)

     case('STATE')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,&
             &ISTANDDATA(37),int(4,SQLLEN_KIND),State_LI)

     case('COUNTY')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,&
             &ISTANDDATA(38),int(4,SQLLEN_KIND),Connty_LI)

     case('FUEL_0_1')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(39),int(4,SQLLEN_KIND), Fuel0_LI)
     case('FUEL_1_3','FUEL_1_3_H')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(40),int(4,SQLLEN_KIND), Fuel1_LI)
     case('FUEL_3_6','FUEL_3_6_H')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(41),int(4,SQLLEN_KIND), Fuel3_LI)
     case('FUEL_6_12','FUEL_6_12_H')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(42),int(4,SQLLEN_KIND), Fuel6_LI)
     case('FUEL_GT_12','FUEL_12_20','FUEL_12_20_H')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(43),int(4,SQLLEN_KIND), Fuel12_LI)
     case('FUEL_LITTER')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(44),int(4,SQLLEN_KIND), FuelLt_LI)
     case('FUEL_DUFF')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(45),int(4,SQLLEN_KIND), FuelDf_LI)
     case('FUEL_0_25','FUEL_0_25_H')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(46),int(4,SQLLEN_KIND), Fuel025_LI)
     case('FUEL_25_1','FUEL_25_1_H')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(47),int(4,SQLLEN_KIND), Fuel251_LI)
     case('FUEL_20_35','FUEL_20_35_H')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(48),int(4,SQLLEN_KIND), Fuel20_LI)
     case('FUEL_35_50','FUEL_35_50_H')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(49),int(4,SQLLEN_KIND), Fuel35_LI)
     case('FUEL_GT_50','FUEL_GT_50_H')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(50),int(4,SQLLEN_KIND), Fuel50_LI)

     case('FUEL_0_25_S')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(55),int(4,SQLLEN_KIND), FuelS025_LI)
     case('FUEL_25_1_S')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(56),int(4,SQLLEN_KIND), FuelS251_LI)
     case('FUEL_1_3_S')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(57),int(4,SQLLEN_KIND), FuelS1_LI)
     case('FUEL_3_6_S')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(58),int(4,SQLLEN_KIND), FuelS3_LI)
     case('FUEL_6_12_S')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(59),int(4,SQLLEN_KIND), FuelS6_LI)
     case('FUEL_12_20_S')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(60),int(4,SQLLEN_KIND), FuelS12_LI)
     case('FUEL_20_35_S')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(61),int(4,SQLLEN_KIND), FuelS20_LI)
     case('FUEL_35_50_S')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(62),int(4,SQLLEN_KIND), FuelS35_LI)
     case('FUEL_GT_50_S')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_FLOAT,&
             &RSTANDDATA(63),int(4,SQLLEN_KIND), FuelS50_LI)

     case('FUEL_MODEL')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,&
             &ISTANDDATA(51),int(4,SQLLEN_KIND), FuelModel_LI)
     case('PHOTO_REF')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_INTEGER,&
             &ISTANDDATA(52),int(4,SQLLEN_KIND), FotoRef_LI)
     case('PHOTO_CODE')
        iRet = fvsSQLBindCol (StmtHndlIn,ColNumber,SQL_F_CHAR,&
             &CFotoCode,int(LEN(CFotoCode)-1,SQLLEN_KIND), FotoCode_LI)
     end select

  end do

  if (LKECHO)write(JOSTND,'(/T12,''STAND-LEVEL DATA BASE READ:'')')

  !     Fetch the Row Data

  iRet = fvsSQLFetch(StmtHndlIn)
  if (iRet.ne.SQL_SUCCESS.and.&
       &iRet.ne.SQL_SUCCESS_WITH_INFO) then
     if (iRet.eq.SQL_NO_DATA) then
        call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlIn,&
             &'STANDIN:No data returned on fetch: '//trim(SQLSTR))
     else
        call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlIn,&
             &'STANDIN:Fetch error: '//trim(SQLSTR))
     end if
  end if

  !     CHECK FOR NULLS AND ASSIGN VALUES THAT WERE NOT NULL

  !     PROCESS STAND IDENTIFICATION CODE.
  !     STAND CONTROL NUMBER SET BY KEYWORD (STANDCN OR STDIDENT)
  !     IS HIGHEST PROIORITY. OTHERWISE USE VALUE FROM DATA BASE

  if (DBCN.eq.' ') then
     if (DBCN_LI.gt.0) then
        I = INDEX(TMP_DBCN,CHAR(0))
        if (I.gt.0) TMP_DBCN(I:) = ' '
        DBCN = ADJUSTL(TMP_DBCN)
        DBCN = ADJUSTL(TMP_DBCN(:DBCN_LI))
        if (LKECHO)write(JOSTND,'(T12,''STAND_CN: '',A)') TRIM(DBCN)
     end if
  else
     DBCN = TRIM(ADJUSTL(DBCN))
  end if
  if (NPLT.eq.' ') then
     if (Stand_LI.gt.0) then
        if (LSTDISNUM) then
           write (CSTAND,'(I26)') ISTANDDATA(28)
        else
           I = INDEX(CSTAND,CHAR(0))
           if (I.gt.0) CSTAND(I:) = ' '
        end if
        NPLT = ADJUSTL(CSTAND)
        if (LKECHO)write(JOSTND,'(T12,''STAND_ID: '',A)') TRIM(NPLT)
     end if
  else
     NPLT = TRIM(ADJUSTL(NPLT))
  end if

  if (IY_LI.ne.SQL_NULL_DATA) then
     IY(1) = ISTANDDATA(1)
     if (LKECHO)write(JOSTND,'(T12,''INV_YEAR: '',T34,I6)') IY(1)
  end if
  if ((Region_LI.ne.SQL_NULL_DATA).and.&
       &LKECHO)write(JOSTND,'(T12,''REGION: '',T34,I6)')&
       &ISTANDDATA(29)
  if ((Forest_LI.ne.SQL_NULL_DATA).and.&
       &LKECHO)write(JOSTND,'(T12,''FOREST: '',T34,I6)')&
       &ISTANDDATA(30)
  if ((District_LI.ne.SQL_NULL_DATA).and.&
       &LKECHO)write(JOSTND,'(T12,''DISTRICT: '',T34,I6)')&
       &ISTANDDATA(31)
  if ((Compartment_LI.ne.SQL_NULL_DATA).and.&
       &LKECHO)write(JOSTND,'(T12,''COMPARTMENT: '',T34,I6)')&
       &ISTANDDATA(32)

  !     CONVERT REGION, FOREST, DISTRICT, COMPARTMENT INTO LOCATION
  !     FOLLOWING VARIANT-SPECIFIC RULES.

  if (Region_LI.ne.SQL_NULL_DATA .and.& ! Start with RFF
       &Forest_LI.ne.SQL_NULL_DATA) then
     KODFOR = ISTANDDATA(29) * 100 + ISTANDDATA(30)
     if (VARACD.eq.'KT' .or.& ! Convert to RFFDD
          &VARACD.eq.'WS' .or.&
          &VARACD.eq.'SN') then
        KODFOR = KODFOR * 100
        if (District_LI.ne.SQL_NULL_DATA)&
             &KODFOR = KODFOR + ISTANDDATA(31)
     end if
     if (VARACD.eq.'KT') then ! Convert to RFFDDCCC
        KODFOR = KODFOR * 1000
        if (Compartment_LI.ne.SQL_NULL_DATA)&
             &KODFOR = KODFOR + ISTANDDATA(32)
     end if
     if (LKECHO)write(JOSTND,'(T12,'' COMPOSITE LOC: '',T32,I8)')&
          &KODFOR
     call FORKOD
  end if

  !     Location code overrides.

  if (Location_LI.ne.SQL_NULL_DATA) then
     KODFOR = ISTANDDATA(4)
     if (LKECHO)write(JOSTND,'(T12,''LOCATION: '',T32,I8)') KODFOR
     call FORKOD
  end if

  !     SET DEFAULT LOCATION CODE IF NOT PRESENT IN INPUT DATA

  if (KODFOR.eq.0)call FORKOD

  if (Long_LI.ne.SQL_NULL_DATA) then
     TLONG = RSTANDDATA(3)
     if (LKECHO)write(JOSTND,'(T12,''LONGITUDE: '',T29,F11.4)') TLONG
  end if

  if (Lat_LI.ne.SQL_NULL_DATA) then
     TLAT = RSTANDDATA(2)
     if (LKECHO)write(JOSTND,'(T12,''LATITUDE: '',T29,F11.4)') TLAT
  end if

  if (PvRefCode_LI.ne.SQL_NULL_DATA) then
     if (NUMPVREF.le.0) then
        CPVREF = '          '
     else
        write(CPVREF,'(I10)')NUMPVREF
     end if
  end if
  if (Habitat_LI.ne.0) then
     if (LHABISNUM) then ! HABITAT CODE IS NUMBER.
        CHAB = ' '
        write (CHAB,'(I10)') ISTANDDATA(5)
        goto 45
     else
        I = INDEX(CHAB,CHAR(0))
        if (I.eq.1) goto 45
        if (I.gt.0) CHAB(I:) = ' '
        CHAB = ADJUSTL(CHAB)
        read (CHAB,'(I10)',ERR = 40)  ISTANDDATA(5)
        goto 45
        40 continue
        ISTANDDATA(5) = 0
     end if
     45 continue
     KARD2 = ADJUSTL(CHAB(1:10))
     ARRAY2 = ISTANDDATA(5)
     if ((VARACD.eq.'SN').and.&
          &(Ecoregion_LI.ne.SQL_NULL_DATA)) then
        KODTYP = 0
        ICL5 = 0
        if (LKECHO)write(JOSTND,'(T12,'' HABITAT/PV_CODE IGNORED.'')')
     else if (CHAB .ne. ' ') then
        KODTYP = IFIX(ARRAY2)
        ICL5 = KODTYP
        call HABTYP (KARD2,ARRAY2)
        if (ICL5.le.0) ICL5 = KODTYP
        if (KODTYP.ne.ICL5) then
           if (LKECHO)write (JOSTND,50) ADJUSTR(CHAB),KODTYP
           if (LKECHO)write (JOSTND,62) ADJUSTR(CPVREF)
        else
           if (LKECHO)write (JOSTND,50) ADJUSTR(CHAB)
           if (LKECHO)write (JOSTND,62) ADJUSTR(CPVREF)
        end if
        50 format (T12,'HABITAT/PV_CODE:',T29,A:&
             &' CONVERTED TO CODE: ',I4)
        62 format (T12,'PV REFERENCE CODE:',T30,A)
     end if
  end if

  if (Ecoregion_LI.gt.0) then
     if (VARACD.eq.'SN') then
        if (LECOISNUM) then ! ECOREGION CODE IS NUMBER.
           CECOREG = " "
           write (CECOREG,'(I10)') ISTANDDATA(54)
           goto 46
        else
           I = INDEX(CECOREG,CHAR(0))
           if (I.eq.1) goto 46
           if (I.gt.0) CECOREG(I:) = ' '
           CECOREG = ADJUSTL(CECOREG)
           read (CECOREG,'(I10)',ERR = 41)  ISTANDDATA(54)
           goto 46
           41 continue
           ISTANDDATA(54) = 0
        end if
        46 continue
        KARD2 = ADJUSTL(CECOREG(1:10))
        ARRAY2 = ISTANDDATA(54)
        KODTYP = IFIX(ARRAY2)
        ICL5 = KODTYP
        call HABTYP (KARD2,ARRAY2)
        if (ICL5.le.0) ICL5 = KODTYP
        if (KODTYP.ne.ICL5) then
           if (LKECHO)write (JOSTND,51) ADJUSTR(CECOREG),KODTYP
        else
           if (LKECHO)write (JOSTND,51) ADJUSTR(CECOREG)
        end if
        51 format (T12,'ECOLOGICAL UNIT:',T29,A:&
             &' CONVERTED TO CODE: ',I4)
     end if
  end if

  if (Age_LI.ne.SQL_NULL_DATA) then
     IAGE = ISTANDDATA(6)
     if (LKECHO)write(JOSTND,'(T12,''AGE: '',T34,I6)') IAGE
  end if
  if (Aspect_LI.ne.SQL_NULL_DATA) then
     ASPECT = RSTANDDATA(7)
     if (LKECHO)write(JOSTND,'(T12,''ASPECT: '',T34,F6.1)') ASPECT
  end if
  if (Slope_LI.ne.SQL_NULL_DATA) then
     SLOPE = RSTANDDATA(8)
     if (LKECHO)write(JOSTND,'(T12,''SLOPE: '',T34,F6.2)') SLOPE
  end if
  if (Elev_LI.ne.SQL_NULL_DATA .and.&
       &ElevFT_LI.eq.SQL_NULL_DATA) then
     if (RSTANDDATA(9).gt.0)ELEV = RSTANDDATA(9)
     if (LKECHO)write(JOSTND,'(T12,''ELEVATION: '',T34,F6.1)') ELEV
  end if
  if (ElevFT_LI.ne.SQL_NULL_DATA ) then
     if (RSTANDDATA(33).gt.0.)ELEV = RSTANDDATA(33)*.01
     if (LKECHO)write(JOSTND,10) RSTANDDATA(33),ELEV
     10 format (T12,'ELEVFT: ',T34,F6.1,' CONVERTED TO: ',F6.1)
  end if
  if (Basal_LI.ne.SQL_NULL_DATA) then
     BAF = RSTANDDATA(10)
     IF(LKECHO)WRITE(JOSTND,'(T12,''BASAL_AREA_FACTOR: '',          &
          &   T34,F6.1)') BAF
  end if
  if (PlotArea_LI.ne.SQL_NULL_DATA) then
     FPA = RSTANDDATA(11)
     if (LKECHO)write(JOSTND,'(T12,''INV_PLOT_SIZE: '',T34,F6.0)')FPA
  end if
  if (BPDBH_LI.ne.SQL_NULL_DATA) then
     BRK = RSTANDDATA(12)
     if (LKECHO)write(JOSTND,'(T12,''BRK_DBH: '',T34,F6.1)') BRK
  end if
  if (NumPlots_LI.ne.SQL_NULL_DATA) then
     IPTINV = ISTANDDATA(13)
     if (LKECHO)write(JOSTND,'(T12,''NUM_PLOTS: '',T34,I6)') IPTINV
  end if
  if (NonStock_LI.ne.SQL_NULL_DATA) then
     NONSTK = ISTANDDATA(14)
     if (LKECHO)write(JOSTND,'(T12,''NONSTK_PLOTS: '',T34,I6)')&
          &NONSTK
  end if
  if (SamWt_LI.ne.SQL_NULL_DATA) then
     SAMWT = RSTANDDATA(15)
     if (LKECHO)write(JOSTND,'(T12,''SAM_WT: '',T24,F16.6)') SAMWT
  end if
  if (Stock_LI.ne.SQL_NULL_DATA) then
     if (RSTANDDATA(16).gt.1.0.and. RSTANDDATA(16).le.100.)&
          &GROSPC = RSTANDDATA(16)*.01
     if (RSTANDDATA(16).gt.0.0.and. RSTANDDATA(16).le.1.0)&
          &GROSPC = RSTANDDATA(16)
     if (GROSPC.lt.0.) then
        XXG = 1.0
        if (IPTINV.gt.0.and. NONSTK.gt.0.and.&
             &IPTINV-NONSTK.gt.0) XXG = (real(IPTINV)-real(NONSTK))/&
             &real(IPTINV)
     else
        XXG = GROSPC
     end if
     if (LKECHO)write(JOSTND,'(T12,''STK_PCNT: '',T34,F6.3)') XXG
  end if
  if (DGT_LI.ne.SQL_NULL_DATA) then
     IDG = ISTANDDATA(17)
     if (LKECHO)write(JOSTND,'(T12,''DG_TRANS: '',T34,I6)') IDG
  end if
  if (DGM_LI.ne.SQL_NULL_DATA) then
     if (ISTANDDATA(18).gt.0.)IFINT = ISTANDDATA(18)
     FINT = real(IFINT)
     if (LKECHO)write(JOSTND,'(T12,''DG_MEASURE: '',T34,I6)') IFINT
  end if
  if (HTT_LI.ne.SQL_NULL_DATA) then
     IHTG = ISTANDDATA(19)
     if (LKECHO)write(JOSTND,'(T12,''HTG_TRANS: '',T34,I6)') IHTG
  end if
  if (HTM_LI.ne.SQL_NULL_DATA) then
     if (ISTANDDATA(20).gt.0.)IFINTH = ISTANDDATA(20)
     FINTH = real(IFINTH)
     if (LKECHO)write(JOSTND,'(T12,''HTG_MEASURE: '',T34,I6)') IFINTH
  end if
  if (Mort_LI.ne.SQL_NULL_DATA) then
     FINTM = real(ISTANDDATA(21))
     if (FINTM.le.0.)FINTM = 5.
     if (LKECHO)write(JOSTND,'(T12,''MORT_MEASURE: '',T34,I6)')&
          &IFIX(FINTM)
  end if

  !     SITE SPECIES CODE PROCESSING
  if (SiteSp_LI.gt.0) then
     if (LSITEISNUM) then ! SITE SPECIES CODE IS NUMBER.
        CSITECODE = " "
        write (CSITECODE,'(I8)') ISTANDDATA(34)
     end if

     I = INDEX(CSITECODE,CHAR(0))
     if (I.gt.0) CSITECODE(I:) = ' '
     CSITECODE = ADJUSTL(CSITECODE)
     NAMELEN = LEN_TRIM(CSITECODE)
     do J = 1,NAMELEN
        call UPCASE(CSITECODE(J:J))
     end do

     if (LEN_TRIM(CSITECODE).le.2) then
        if ((ICHAR(ADJUSTL(CSITECODE(1:1))).ge.48).and.&
             &(ICHAR(ADJUSTL(CSITECODE(1:1))).le.57).and.&
             &(ICHAR(ADJUSTL(CSITECODE(2:2))).ge.48).and.&
             &(ICHAR(ADJUSTL(CSITECODE(2:2))).le.57)) then
           CSITECODE = '0'//ADJUSTL(CSITECODE)
        end if
     end if

     do I = 1,MAXSP
        if (CSITECODE.eq.NSP(I,1)(1:2).or.CSITECODE.eq.PLNJSP(I).or.&
             &CSITECODE.eq.FIAJSP(I)) then
           ISISP = I
           LSITE = .true.
           exit
        end if
     end do
     if (LSITE) then
        if (LKECHO)write (JOSTND,20) ADJUSTR(CSITECODE),&
             &NSP(ISISP,1)(1:2)
        20 format (T12,'SITE_SPECIES:',T30,A10,&
             &' MAPPED TO INTERNAL CODE: ',A)
     else
        write (JOSTND,25) ADJUSTR(CSITECODE)
        25 format (T12,'SITE_SPECIES:',T30,A10,' WAS NOT RECOGNIZED')
     end if
  end if

  !     SITE INDEX PROCESSING

  if (SiteIndx_LI.ne.SQL_NULL_DATA) then
     if (RSTANDDATA(35).le.7.) then ! DUNNING CODE.
        select case (VARACD)
        case('CA','NC','SO','WS')
           if (LKECHO)write(JOSTND,27)
           27 format(38X,'   SITE INDEX IS THAN 8 AND ',&
                &'WILL BE INTERPRETED AS A DUNNING CODE.')
        end select
        call DUNN(RSTANDDATA(35))
        if (LKECHO)write(JOSTND,&
             &'(T12,''SITE_INDEX (DUNNING CODE): '',T34,F6.1)')&
             &RSTANDDATA(35)
        CSITECODE = 'ALL'
     else
        if (ISISP.eq.0) then
           do I = 1,MAXSP
              SITEAR(I) = RSTANDDATA(35)
           end do
           CSITECODE = 'ALL'
        else
           SITEAR(ISISP) = RSTANDDATA(35)
           CSITECODE = NSP(ISISP,1)(1:2)
        end if
     end if
     if (LKECHO)write(JOSTND,30) RSTANDDATA(35),TRIM(CSITECODE)
     30 format (T12,'SITE_INDEX: ',T34,F6.1,' FOR SPECIES: ',A)

  end if

  if (MaxB_LI.ne.SQL_NULL_DATA) then
     BAMAX = RSTANDDATA(22)
     if (BAMAX.gt.0.)LBAMAX = .true.
     if (LKECHO)write(JOSTND,'(T12,''MAX_BA: '',T34,F6.1)') BAMAX
  end if
  if (MaxSDI_LI.ne.SQL_NULL_DATA) then
     do I = 1,MAXSP
        SDIDEF(I) = RSTANDDATA(36)
     end do
     if (LKECHO)write(JOSTND,'(T12,''MAX_SDI: '',T34,F6.1)')SDIDEF(1)
  end if
  if (Model_LI.ne.SQL_NULL_DATA) then
     !         IMODTY = ISTANDDATA(38)
     IMODTY = ISTANDDATA(25)
     if (LKECHO)write(JOSTND,'(T12,''MODEL_TYPE: '',T34,I6)') IMODTY
  end if
  if (PhysioR_LI.ne.SQL_NULL_DATA) then
     IPHREG = ISTANDDATA(26)
     if (LKECHO)write(JOSTND,'(T12,''PHYSIO_REGION: '',T34,I6)')&
          &IPHREG
  end if
  if (ForType_LI.ne.SQL_NULL_DATA) then
     IFORTP = ISTANDDATA(27)
     if (IFORTP .gt.999) then

        !  THE LAST 3 CHARACTERS INDICATE THE FOREST TYPE AND THE FIRST
        !  CHARACTER INDICATES THAT THE USER SET THE FOREST TYPE TO BE
        !  CONSTANT FOR ALL CYCLES.  THE FIELD 3 INPUT IS DECODED
        !  AND LFLAGV IS SET TO TRUE TO INDICATE CONSTANT FOREST TYPE

        XTMP = real(IFORTP)
        XTMP = XTMP/1000. + 0.00001
        IXTMP = INT(XTMP)
        IFORTP = INT((XTMP-real(IXTMP))*1000.)
        LFLAGV = .true.
     end if

     !  CALL FORTYP TO CHECK FOR VALID USER INPUT OF IFORTP VALUE

     DUM1 = 0.
     IXF = 1
     call FORTYP(IXF,DUM1)
     if (LKECHO)write(JOSTND,'(T12,''FOREST_TYPE: '',T34,I6)') IFORTP
  end if
  if (State_LI.ne.SQL_NULL_DATA) then
     ISTATE = ISTANDDATA(37)
     if (LKECHO)write(JOSTND,'(T12,''STATE: '',T34,I6)') ISTATE
  end if
  if (Connty_LI.ne.SQL_NULL_DATA) then
     ICNTY = ISTANDDATA(38)
     if (LKECHO)write(JOSTND,'(T12,''COUNTY: '',T34,I6)') ICNTY
  end if

  !     FUEL LOAD PARAMETERS

  LFMYES = .false.
  if (Fuel0_LI.ne.SQL_NULL_DATA) then
     LFMYES = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_0_1_H: '',T32,F8.3)')&
          &RSTANDDATA(39)
  else
     RSTANDDATA(39) = -1.
  end if
  if (Fuel1_LI.ne.SQL_NULL_DATA) then
     LFMYES = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_1_3_H: '',T32,F8.3)')&
          &RSTANDDATA(40)
  else
     RSTANDDATA(40) = -1.
  end if
  if (Fuel3_LI.ne.SQL_NULL_DATA) then
     LFMYES = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_3_6_H: '',T32,F8.3)')&
          &RSTANDDATA(41)
  else
     RSTANDDATA(41) = -1.
  end if
  if (Fuel6_LI.ne.SQL_NULL_DATA) then
     LFMYES = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_6_12_H: '',T32,F8.3)')&
          &RSTANDDATA(42)
  else
     RSTANDDATA(42) = -1.
  end if
  if (Fuel12_LI.ne.SQL_NULL_DATA) then
     LFMYES = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_12_20_H: '',T32,F8.3)')&
          &RSTANDDATA(43)
  else
     RSTANDDATA(43) = -1.
  end if
  if (FuelLt_LI.ne.SQL_NULL_DATA) then
     LFMYES = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_LITTER: '',T32,F8.3)')&
          &RSTANDDATA(44)
  else
     RSTANDDATA(44) = -1.
  end if
  if (FuelDf_LI.ne.SQL_NULL_DATA) then
     LFMYES = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_DUFF: '',T32,F8.3)')&
          &RSTANDDATA(45)
  else
     RSTANDDATA(45) = -1.
  end if
  if (Fuel025_LI.ne.SQL_NULL_DATA) then
     LFMYES = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_0_25_H: '',T32,F8.3)')&
          &RSTANDDATA(46)
  else
     RSTANDDATA(46) = -1.
  end if
  if (Fuel251_LI.ne.SQL_NULL_DATA) then
     LFMYES = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_25_1_H: '',T32,F8.3)')&
          &RSTANDDATA(47)
  else
     RSTANDDATA(47) = -1.
  end if
  if (Fuel20_LI.ne.SQL_NULL_DATA) then
     LFMYES = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_20_35_H: '',T32,F8.3)')&
          &RSTANDDATA(48)
  else
     RSTANDDATA(48) = -1.
  end if
  if (Fuel35_LI.ne.SQL_NULL_DATA) then
     LFMYES = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_35_50_H: '',T32,F8.3)')&
          &RSTANDDATA(49)
  else
     RSTANDDATA(49) = -1.
  end if
  if (Fuel50_LI.ne.SQL_NULL_DATA) then
     LFMYES = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_GT_50_H: '',T32,F8.3)')&
          &RSTANDDATA(50)
  else
     RSTANDDATA(50) = -1.
  end if

  LFMYES2 = .false.
  if (FuelS025_LI.ne.SQL_NULL_DATA) then
     LFMYES2 = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_0_25_S: '',T32,F8.3)')&
          &RSTANDDATA(55)
  else
     RSTANDDATA(55) = -1.
  end if
  if (FuelS251_LI.ne.SQL_NULL_DATA) then
     LFMYES2 = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_25_1_S: '',T32,F8.3)')&
          &RSTANDDATA(56)
  else
     RSTANDDATA(56) = -1.
  end if
  if (FuelS1_LI.ne.SQL_NULL_DATA) then
     LFMYES2 = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_1_3_S: '',T32,F8.3)')&
          &RSTANDDATA(57)
  else
     RSTANDDATA(57) = -1.
  end if
  if (FuelS3_LI.ne.SQL_NULL_DATA) then
     LFMYES2 = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_3_6_S: '',T32,F8.3)')&
          &RSTANDDATA(58)
  else
     RSTANDDATA(58) = -1.
  end if
  if (FuelS6_LI.ne.SQL_NULL_DATA) then
     LFMYES2 = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_6_12_S: '',T32,F8.3)')&
          &RSTANDDATA(59)
  else
     RSTANDDATA(59) = -1.
  end if
  if (FuelS12_LI.ne.SQL_NULL_DATA) then
     LFMYES2 = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_12_20_S: '',T32,F8.3)')&
          &RSTANDDATA(60)
  else
     RSTANDDATA(60) = -1.
  end if
  if (FuelS20_LI.ne.SQL_NULL_DATA) then
     LFMYES2 = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_20_35_S: '',T32,F8.3)')&
          &RSTANDDATA(61)
  else
     RSTANDDATA(61) = -1.
  end if
  if (FuelS35_LI.ne.SQL_NULL_DATA) then
     LFMYES2 = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_35_50_S: '',T32,F8.3)')&
          &RSTANDDATA(62)
  else
     RSTANDDATA(62) = -1.
  end if
  if (FuelS50_LI.ne.SQL_NULL_DATA) then
     LFMYES2 = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_GT_50_S: '',T32,F8.3)')&
          &RSTANDDATA(63)
  else
     RSTANDDATA(63) = -1.
  end if

  !     FUEL MODEL

  LFMD = .false.
  if (FuelModel_LI.ne.SQL_NULL_DATA) then
     LFMD = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_MODEL: '',T34,I6)')&
          &ISTANDDATA(51)
  else
     ISTANDDATA(51) = -1
  end if

  !     FUEL PHOTO REFERENCE

  LFOTO = .false.
  if (FOTOREF_LI.ne.SQL_NULL_DATA) then
     LFOTO = .true.

     if ((ISTANDDATA(52) .ne.4) .and. (ISTANDDATA(52) .ne.10)&
          &.and. (ISTANDDATA(52) .ge.1) .and. (ISTANDDATA(52) .le.32))&
          &then
        REF = PHOTOREF(ISTANDDATA(52))
     else
        REF = 'UNKNOWN'
        LFOTO = .false.
     end if
     if (LKECHO)write (JOSTND,55) ISTANDDATA(52), REF

     55 format (T12,'PHOTO_REF: ',T34,I6, ' = ',A)

  else
     ISTANDDATA(52) = -1
  end if

  !     FUEL PHOTO CODE

  FKOD = -1
  LFOTO2 = .false.
  if (FotoCode_LI.gt.0) then
     LFOTO2 = .true.
     I = INDEX(CFotocode,CHAR(0))
     if (I.gt.0) CFotoCode(I:) = ' '
     if (CFotoCode .ne. ' ') then
        CFotoCode = ADJUSTL(CFotoCode)
        read (CFotoCode,'(I10)',ERR = 58)  ISTANDDATA(53)
        58 continue
        call FMPHOTOCODE(ISTANDDATA(52),CFotoCode(1:13),FKOD,1)
        if (LKECHO)write (JOSTND,60) ADJUSTR(CFotoCode),FKOD
        60 format (T12,'PHOTO_CODE:',T26,A:&
             &' CONVERTED TO CODE: ',I4)
        if (FKOD .eq. -1) LFOTO2 = .false.
     end if
  end if

  FOTODATA(1) = ISTANDDATA(52)
  FOTODATA(2) = FKOD

  !     Schedule an activity that changes the initial fuel values. This mimics
  !     the method used in the fire model.  First the fuels photo series photo
  !     selected is set, followed by tons/acre entered directly.

  call  FMLNKD(LFMLK)

  if (LFMLK.and.LFOTO.and.LFOTO2) then
     call OPNEW(I,1,2548,2,FOTODATA(1))
  else if (LFOTO.and.LFOTO2.and..not. LFMLK) then
     write(JOSTND,&
          &'(T12,''FIRE MODEL NOT LINKED, FUELS PHOTO DATA IGNORED.'')')
  else if ((FOTOREF_LI.gt.0).or.(FotoCode_LI.gt.0)) then
     WRITE(JOSTND,'(T12,''MISSING PHOTO ''                           &
          &  ''REFERENCE OR PHOTO CODE, FUELS PHOTO DATA IGNORED.'')')
  end if

  if (LFMLK.and.LFMYES) call OPNEW(I,1,2521,12,RSTANDDATA(39))
  if (LFMYES.and..not. LFMLK) write(JOSTND,&
       &'(T12,''FIRE MODEL NOT LINKED, FUELS DATA IGNORED.'')')

  if (LFMLK.and.LFMYES2) call OPNEW(I,1,2553,9,RSTANDDATA(55))
  if (LFMYES2.and..not. LFMLK) write(JOSTND,&
       &'(T12,''FIRE MODEL NOT LINKED, SOFT FUELS DATA IGNORED.'')')

  !     Schedule an activity that changes the initial fuel model. This mimics
  !     the method used in the fire model.

  if (LFMLK.and.LFMD) then
     X(1) = real(ISTANDDATA(51))
     call OPNEW(I,1,2538,1,X)
  end if

  if (LFMD.and..not. LFMLK) write(JOSTND,&
       &'(T12,''FIRE MODEL NOT LINKED, FUEL MODEL IGNORED.'')')

  if (LKECHO)write(JOSTND,'(T12,''END OF DATA BASE READ.'')')

  iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT, StmtHndlIn)
  return
end
