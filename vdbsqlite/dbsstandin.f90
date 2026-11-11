!============================!
!   vdbsqlite : dbsstandin   !
!============================!
! VDBSQLITE $Id$
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
  integer,parameter :: MxCname = 50,MxMsg = 500
  integer,parameter :: NullInt = -88885672
  real,parameter    :: NullReal = -88885672.
  character(LEN = *),parameter :: NullChar = CHAR(0)
  character(LEN = MxCname) :: ColName
  character(LEN = *) :: SQLSTR
  character(LEN = MxMsg) :: Msg
  character(len=10) :: KARD2
  character(LEN = 10) :: CHAB
  character(LEN = 10) :: CECOREG
  character(LEN = 15) :: CFotoCode
  character(LEN = LEN(DBCN)+1) :: TMP_DBCN
  character(LEN = LEN(NPLT)+1) :: CSTAND
  character(len=10) :: CSITECODE
  character(len=40) :: PHOTOREF(32)
  character(len=40) :: REF
  real    :: ARRAY2,X(1)
  real    :: XXG,FOTODATA(2),RSTANDDATA(64),DUM1,XTMP
  integer(KIND = 4) :: ISTANDDATA(64)
  equivalence (RSTANDDATA,ISTANDDATA)
  integer :: J,I,KODE,FKOD,NUMPVREF,IXTMP,IXF,iRet
  integer :: ColNumber,NameLen,ColumnCount
  logical :: LFMLK,LFMYES,LKECHO,LFMD,LFOTO,LFOTO2,LFMYES2

  integer :: fsql3_prepare,fsql3_step,fsql3_finalize,fsql3_errmsg,fsql3_colname,fsql3_colcnt,&
       &fsql3_colint,fsql3_coltext
  real*4  :: fsql3_colreal

  integer :: IY_LI,Lat_LI,Long_LI,Location_LI,Habitat_LI,Age_LI,Aspect_LI,Slope_LI,MaxSDI_LI,Elev_LI,&
       &Basal_LI,PlotArea_LI,BPDBH_LI,NumPlots_LI,NonStock_LI,SamWt_LI,Stock_LI,DGT_LI,DGM_LI,HTT_LI,HTM_LI,&
       &SiteSp_LI,SiteIndx_LI,Mort_LI,MaxB_LI,Model_LI,PhysioR_LI,ForType_LI,Stand_LI,DBCN_LI,Region_LI,&
       &Forest_LI,District_LI,Compartment_LI,Ecoregion_LI,ElevFT_LI,State_LI,Connty_LI,Fuel0_LI,Fuel1_LI,&
       &Fuel3_LI,Fuel6_LI,Fuel12_LI,FuelLt_LI,FuelDf_LI,FuelModel_LI,Fuel025_LI,Fuel251_LI,Fuel20_LI,&
       &Fuel35_LI,Fuel50_LI,FotoRef_LI,FotoCode_LI,PvRefCode_LI,FuelS025_LI,FuelS251_LI,FuelS1_LI,FuelS3_LI,&
       &FuelS6_LI,FuelS12_LI,FuelS20_LI,FuelS35_LI,FuelS50_LI,StdOrgCd_LI

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

  IY_LI = NullInt
  Lat_LI = NullInt
  Long_LI = NullInt
  Location_LI = NullInt
  Habitat_LI = NullInt
  Age_LI = NullInt
  Aspect_LI = NullInt
  Slope_LI = NullInt
  MaxSDI_LI = NullInt
  Elev_LI = NullInt
  Basal_LI = NullInt
  PlotArea_LI = NullInt
  BPDBH_LI = NullInt
  NumPlots_LI = NullInt
  NonStock_LI = NullInt
  SamWt_LI = NullInt
  Stock_LI = NullInt
  DGT_LI = NullInt
  DGM_LI = NullInt
  HTT_LI = NullInt
  HTM_LI = NullInt
  SiteSp_LI = NullInt
  SiteIndx_LI = NullInt
  Mort_LI = NullInt
  MaxB_LI = NullInt
  Model_LI = NullInt
  PhysioR_LI = NullInt
  ForType_LI = NullInt
  Stand_LI = NullInt
  DBCN_LI = NullInt
  Region_LI = NullInt
  Forest_LI = NullInt
  District_LI = NullInt
  Compartment_LI = NullInt
  Ecoregion_LI = NullInt
  ElevFT_LI = NullInt
  State_LI = NullInt
  Connty_LI = NullInt
  Fuel0_LI = NullInt
  Fuel1_LI = NullInt
  Fuel3_LI = NullInt
  Fuel6_LI = NullInt
  Fuel12_LI = NullInt
  FuelLt_LI = NullInt
  FuelDf_LI = NullInt
  Fuel025_LI = NullInt
  Fuel251_LI = NullInt
  FuelModel_LI = NullInt
  PvRefCode_LI = NullInt
  FotoRef_LI = NullInt
  FotoCode_LI = NullInt
  Fuel20_LI = NullInt
  Fuel35_LI = NullInt
  Fuel50_LI = NullInt
  FuelS025_LI = NullInt
  FuelS251_LI = NullInt
  FuelS1_LI = NullInt
  FuelS3_LI = NullInt
  FuelS6_LI = NullInt
  FuelS12_LI = NullInt
  FuelS20_LI = NullInt
  FuelS35_LI = NullInt
  FuelS50_LI = NullInt
  StdOrgCd_LI = NullInt

  if (LKECHO)write(JOSTND,'(/T12,''STAND-LEVEL DATA BASE READ:'')')

  !     MAKE SURE WE HAVE AN OPEN CONNECTION

  if (IinDBref.eq.-1) call DBSOPEN(.false.,.true.,KODE)
  iRet = fsql3_prepare(IinDBref,trim(SQLSTR)//CHAR(0))
  if (iRet.gt.0) then
     iRet = fsql3_errmsg(IinDBref, Msg, MxMsg)
     write (JOSTND,5) TRIM(SQLSTR), Msg(:iRet)
     5 format(/T12,'SQL=',A/T12,'Error Msg=',A/,&
          &'********   ERROR: SQL COMMAND FAILED.')
     call RCDSET (2,.true.)
     return
  end if

  !     Get the first record...do not step through all records.

  iRet = fsql3_step(IinDBref)
  if (iRet.ne.1) return

  !     Stand data is ready for processing. Create Cases record so that any
  !     warnings or errors can be written to error table.
  call DBSCASE(1)

  !     GET NUMBER OF COLUMNS RETURNED

  ColumnCount = fsql3_colcnt(IinDBref)

  do ColNumber = 0,ColumnCount-1

     iRet = fsql3_colname(IinDBref,ColNumber,ColName,MxCname)
     ColName((iRet+1):) = ''

     select case(ColName)

     case('STAND_CN')
        iRet = fsql3_coltext (IinDBref,ColNumber,TMP_DBCN,&
             &LEN(TMP_DBCN),NullChar)
        if (iRet.lt.LEN(TMP_DBCN)) TMP_DBCN((iRet+1):) = ' '
        if (TMP_DBCN .ne. NullChar) DBCN_LI = 1

     case('STAND_ID')
        iRet = fsql3_coltext (IinDBref,ColNumber,CSTAND,&
             &LEN(CSTAND),NullChar)
        if (iRet.lt.LEN(CSTAND)) CSTAND((iRet+1):) = ' '
        if (CSTAND .ne. NullChar) Stand_LI = 1

     case('INV_YEAR')
        ISTANDDATA(1) = fsql3_colint(IinDBref,ColNumber,NullInt)
        if (ISTANDDATA(1) .ne. NullInt) IY_LI = 1

     case('LATITUDE')
        RSTANDDATA(2) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(2) .ne. NullReal) Lat_LI = 1

     case('LONGITUDE')
        RSTANDDATA(3) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(3) .ne. NullReal) Long_LI = 1

     case('REGION')
        ISTANDDATA(29) = fsql3_colint(IinDBref,ColNumber,NullInt)
        if (ISTANDDATA(29) .ne. NullInt) Region_LI = 1

     case('FOREST')
        ISTANDDATA(30) = fsql3_colint(IinDBref,ColNumber,NullInt)
        if (ISTANDDATA(30) .ne. NullInt) Forest_LI = 1

     case('DISTRICT')
        ISTANDDATA(31) = fsql3_colint(IinDBref,ColNumber,NullInt)
        if (ISTANDDATA(31) .ne. NullInt) District_LI = 1

     case('COMPARTMENT')
        ISTANDDATA(32) = fsql3_colint(IinDBref,ColNumber,NullInt)
        if (ISTANDDATA(32) .ne. NullInt) Compartment_LI = 1

     case('ECOREGION')
        iRet = fsql3_coltext (IinDBref,ColNumber,CECOREG,&
             &LEN(CECOREG),NullChar)
        if (iRet.lt.LEN(CECOREG)) CECOREG((iRet+1):) = ' '
        if (CECOREG .ne. NullChar&
             &.and. LEN_TRIM(ADJUSTL(CECOREG)) .gt.0) Ecoregion_LI = 1

     case('LOCATION')
        ISTANDDATA(4) = fsql3_colint(IinDBref,ColNumber,NullInt)
        if (ISTANDDATA(4) .ne. NullInt) Location_LI = 1

     case('HABITAT','PV_CODE')
        iRet = fsql3_coltext (IinDBref,ColNumber,CHAB,&
             &LEN(CHAB),NullChar)
        if (iRet.lt.LEN(CHAB)) CHAB((iRet+1):) = ' '
        if (CHAB .ne. ' ') Habitat_LI = 1

     case('PV_REF_CODE')
        NUMPVREF = fsql3_colint(IinDBref,ColNumber,NullInt)
        if (NUMPVREF .ne. NullInt) PvRefCode_LI = 1

     case('AGE')
        ISTANDDATA(6) = fsql3_colint(IinDBref,ColNumber,NullInt)
        if (ISTANDDATA(6) .ne. NullInt) Age_LI = 1

     case('ASPECT')
        RSTANDDATA(7) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(7) .ne. NullReal) Aspect_LI = 1

     case('SLOPE')
        RSTANDDATA(8) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(8) .ne. NullReal) Slope_LI = 1

     case('ELEVATION')
        RSTANDDATA(9) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(9) .ne. NullReal) Elev_LI = 1

     case('ELEVFT')
        RSTANDDATA(33) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(33) .ne. NullReal) ElevFt_LI = 1

     case('BASAL_AREA_FACTOR')
        RSTANDDATA(10) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(10) .ne. NullReal) Basal_LI = 1

     case('INV_PLOT_SIZE')
        RSTANDDATA(11) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(11) .ne. NullReal) PlotArea_LI = 1

     case('BRK_DBH')
        RSTANDDATA(12) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(12) .ne. NullReal) BPDBH_LI = 1

     case('NUM_PLOTS')
        ISTANDDATA(13) = fsql3_colint(IinDBref,ColNumber,NullInt)
        if (ISTANDDATA(13) .ne. NullInt) NumPlots_LI = 1

     case('NONSTK_PLOTS')
        ISTANDDATA(14) = fsql3_colint(IinDBref,ColNumber,NullInt)
        if (ISTANDDATA(14) .ne. NullInt) NonStock_LI = 1

     case('SAM_WT')
        RSTANDDATA(15) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(15) .ne. NullReal) SamWt_LI = 1

     case('STK_PCNT')
        RSTANDDATA(16) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(16) .ne. NullReal) Stock_LI = 1

     case('DG_TRANS')
        ISTANDDATA(17) = fsql3_colint(IinDBref,ColNumber,NullInt)
        if (ISTANDDATA(17) .ne. NullInt) DGT_LI = 1

     case('DG_MEASURE')
        RSTANDDATA(18) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(18) .ne. NullReal) DGM_LI = 1

     case('HTG_TRANS')
        ISTANDDATA(19) = fsql3_colint(IinDBref,ColNumber,NullInt)
        if (ISTANDDATA(19) .ne. NullInt) HTT_LI = 1

     case('HTG_MEASURE')
        RSTANDDATA(20) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(20) .ne. NullReal) HTM_LI = 1

     case('MORT_MEASURE')
        RSTANDDATA(21) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(21) .ne. NullReal) Mort_LI = 1

     case('SITE_SPECIES')
        iRet = fsql3_coltext (IinDBref,ColNumber,CSITECODE,&
             &LEN(CSITECODE),NullChar)
        if (iRet.lt.LEN(CSITECODE)) CSITECODE((iRet+1):) = ' '
        if (CSITECODE .ne. ' ') SiteSp_LI = 1

     case('SITE_INDEX')
        RSTANDDATA(35) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(35) .ne. NullReal) SiteIndx_LI = 1

     case('MAX_BA')
        RSTANDDATA(22) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(22) .ne. NullReal) MaxB_LI = 1

     case('MAX_SDI')
        RSTANDDATA(36) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(36) .ne. NullReal) MaxSDI_LI = 1

     case('MODEL_TYPE')
        ISTANDDATA(25) = fsql3_colint(IinDBref,ColNumber,NullInt)
        if (ISTANDDATA(25) .ne. NullInt) Model_LI = 1

     case('PHYSIO_REGION')
        ISTANDDATA(26) = fsql3_colint(IinDBref,ColNumber,NullInt)
        if (ISTANDDATA(26) .ne. NullInt) PhysioR_LI = 1

     case('FOREST_TYPE')
        ISTANDDATA(27) = fsql3_colint(IinDBref,ColNumber,NullInt)
        if (ISTANDDATA(27) .ne. NullInt) ForType_LI = 1

     case('STATE')
        ISTANDDATA(37) = fsql3_colint(IinDBref,ColNumber,NullInt)
        if (ISTANDDATA(37) .ne. NullInt) State_LI = 1

     case('COUNTY')
        ISTANDDATA(38) = fsql3_colint(IinDBref,ColNumber,NullInt)
        if (ISTANDDATA(38) .ne. NullInt) Connty_LI = 1

     case('FUEL_0_1')
        RSTANDDATA(39) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(39) .ne. NullReal) Fuel0_LI = 1
     case('FUEL_1_3','FUEL_1_3_H')
        RSTANDDATA(40) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(40) .ne. NullReal) Fuel1_LI = 1
     case('FUEL_3_6','FUEL_3_6_H')
        RSTANDDATA(41) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(41) .ne. NullReal) Fuel3_LI = 1
     case('FUEL_6_12','FUEL_6_12_H')
        RSTANDDATA(42) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(42) .ne. NullReal) Fuel6_LI = 1
     case('FUEL_GT_12','FUEL_12_20','FUEL_12_20_H')
        RSTANDDATA(43) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(43) .ne. NullReal) Fuel12_LI = 1
     case('FUEL_LITTER')
        RSTANDDATA(44) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(44) .ne. NullReal) FuelLt_LI = 1
     case('FUEL_DUFF')
        RSTANDDATA(45) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(45) .ne. NullReal) FuelDf_LI = 1
     case('FUEL_0_25','FUEL_0_25_H')
        RSTANDDATA(46) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(46) .ne. NullReal) Fuel025_LI = 1
     case('FUEL_25_1','FUEL_25_1_H')
        RSTANDDATA(47) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(47) .ne. NullReal) Fuel251_LI = 1
     case('FUEL_20_35','FUEL_20_35_H')
        RSTANDDATA(48) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(48) .ne. NullReal) Fuel20_LI = 1
     case('FUEL_35_50','FUEL_35_50_H')
        RSTANDDATA(49) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(49) .ne. NullReal) Fuel35_LI = 1
     case('FUEL_GT_50','FUEL_GT_50_H')
        RSTANDDATA(50) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(50) .ne. NullReal) Fuel50_LI = 1
     case('FUEL_0_25_S')
        RSTANDDATA(55) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(55) .ne. NullReal) FuelS025_LI = 1
     case('FUEL_25_1_S')
        RSTANDDATA(56) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(56) .ne. NullReal) FuelS251_LI = 1
     case('FUEL_1_3_S')
        RSTANDDATA(57) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(57) .ne. NullReal) FuelS1_LI = 1
     case('FUEL_3_6_S')
        RSTANDDATA(58) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(58) .ne. NullReal) FuelS3_LI = 1
     case('FUEL_6_12_S')
        RSTANDDATA(59) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(59) .ne. NullReal) FuelS6_LI = 1
     case('FUEL_12_20_S')
        RSTANDDATA(60) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(60) .ne. NullReal) FuelS12_LI = 1
     case('FUEL_20_35_S')
        RSTANDDATA(61) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(61) .ne. NullReal) FuelS20_LI = 1
     case('FUEL_35_50_S')
        RSTANDDATA(62) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(62) .ne. NullReal) FuelS35_LI = 1
     case('FUEL_GT_50_S')
        RSTANDDATA(63) = fsql3_colreal(IinDBref,ColNumber,NullReal)
        if (RSTANDDATA(63) .ne. NullReal) FuelS50_LI = 1

     case('FUEL_MODEL')
        ISTANDDATA(51) = fsql3_colint(IinDBref,ColNumber,NullInt)
        if (ISTANDDATA(51) .ne. NullInt) FuelModel_LI = 1
     case('PHOTO_REF')
        ISTANDDATA(52) = fsql3_colint(IinDBref,ColNumber,NullInt)
        if (ISTANDDATA(52) .ne. NullInt) FotoRef_LI = 1
     case('PHOTO_CODE')
        iRet = fsql3_coltext (IinDBref,ColNumber,CFotoCode,&
             &LEN(CFotoCode),NullChar)
        if (iRet.lt.LEN(CFotoCode)) CFotoCode((iRet+1):) = ' '
        if (CFotoCode .ne. ' ') FotoCode_LI = 1
     case('STDORGCD')
        ISTANDDATA(64) = fsql3_colint(IinDBref,ColNumber,NullInt)
        if (ISTANDDATA(64) .ne. NullInt) StdOrgCd_LI = 1

     end select

  end do

  iRet = fsql3_finalize(IinDBref)

  !     CHECK FOR NULLS AND ASSIGN VALUES THAT WERE NOT NULL

  !     PROCESS STAND IDENTIFICATION CODE.
  !     STAND CONTROL NUMBER SET BY KEYWORD (STANDCN OR STDIDENT)
  !     IS HIGHEST PROIORITY. OTHERWISE USE VALUE FROM DATA BASE

  if (DBCN.eq.' ') then
     if (DBCN_LI.gt.0) then
        TMP_DBCN = ADJUSTL(TMP_DBCN)
        DBCN = TMP_DBCN(:LEN(DBCN))
        if (LKECHO)write(JOSTND,'(T12,''STAND_CN: '',A)') TRIM(DBCN)
     end if
  else
     DBCN = TRIM(ADJUSTL(DBCN))
  end if
  if (NPLT.eq.' ') then
     if (Stand_LI.gt.0) then
        CSTAND = ADJUSTL(CSTAND)
        NPLT = CSTAND(:LEN(NPLT))
        if (LKECHO)write(JOSTND,'(T12,''STAND_ID: '',A)') TRIM(NPLT)
     end if
  else
     NPLT = TRIM(ADJUSTL(NPLT))
  end if

  if (IY_LI.ne.NullInt) then
     IY(1) = ISTANDDATA(1)
     if (LKECHO)write(JOSTND,'(T12,''INV_YEAR: '',T34,I6)') IY(1)
  end if
  if ((Region_LI.ne.NullInt).and.&
       &LKECHO)write(JOSTND,'(T12,''REGION: '',T34,I6)')&
       &ISTANDDATA(29)
  if ((Forest_LI.ne.NullInt).and.&
       &LKECHO)write(JOSTND,'(T12,''FOREST: '',T34,I6)')&
       &ISTANDDATA(30)
  if ((District_LI.ne.NullInt).and.&
       &LKECHO)write(JOSTND,'(T12,''DISTRICT: '',T34,I6)')&
       &ISTANDDATA(31)
  if ((Compartment_LI.ne.NullInt).and.&
       &LKECHO)write(JOSTND,'(T12,''COMPARTMENT: '',T34,I6)')&
       &ISTANDDATA(32)

  !     CONVERT REGION, FOREST, DISTRICT, COMPARTMENT INTO LOCATION
  !     FOLLOWING VARIANT-SPECIFIC RULES.

  if (Region_LI.ne.NullInt .and.& ! Start with RFF
       &Forest_LI.ne.NullInt) then
     KODFOR = ISTANDDATA(29) * 100 + ISTANDDATA(30)
     if (VARACD.eq.'KT' .or.& ! Convert to RFFDD
          &VARACD.eq.'WS' .or.&
          &VARACD.eq.'SN') then
        KODFOR = KODFOR * 100
        if (District_LI.ne.NullInt)&
             &KODFOR = KODFOR + ISTANDDATA(31)
     end if
     if (VARACD.eq.'KT') then ! Convert to RFFDDCCC
        KODFOR = KODFOR * 1000
        if (Compartment_LI.ne.NullInt)&
             &KODFOR = KODFOR + ISTANDDATA(32)
     end if
     if (LKECHO)write(JOSTND,'(T12,'' COMPOSITE LOC: '',T32,I8)')&
          &KODFOR
     call FORKOD
  end if

  !     Location code overrides.

  if (Location_LI.ne.NullInt) then
     KODFOR = ISTANDDATA(4)
     if (LKECHO)write(JOSTND,'(T12,''LOCATION: '',T32,I8)') KODFOR
     call FORKOD
  end if

  !     SET DEFAULT LOCATION CODE IF NOT PRESENT IN INPUT DATA

  if (KODFOR.eq.0)call FORKOD

  if (Long_LI.ne.NullInt) then
     TLONG = RSTANDDATA(3)
     if (LKECHO)write(JOSTND,'(T12,''LONGITUDE: '',T29,F11.4)') TLONG
  end if

  if (Lat_LI.ne.NullInt) then
     TLAT = RSTANDDATA(2)
     if (LKECHO)write(JOSTND,'(T12,''LATITUDE: '',T29,F11.4)') TLAT
  end if

  if (PvRefCode_LI.ne.NullInt) then
     if (NUMPVREF.le.0) then
        CPVREF = ' '
     else
        write(CPVREF,'(I10)')NUMPVREF
     end if
  end if
  if (Habitat_LI.ne.NullInt) then
     CHAB = ADJUSTL(CHAB)
     read (CHAB,'(I10)',ERR = 40)  ISTANDDATA(5)
     goto 45
     40 continue
     ISTANDDATA(5) = 0
     45 continue
     KARD2 = ADJUSTL(CHAB(1:10))
     ARRAY2 = ISTANDDATA(5)
     if ((VARACD.eq.'SN').and.&
          &(Ecoregion_LI.ne.NullInt)) then
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
     CECOREG = ADJUSTL(CECOREG)
     ECOREG = CECOREG(:4)
     if (VARACD.eq.'SN') then
        read (CECOREG,'(I10)',ERR = 41)  ISTANDDATA(54)
        goto 46
        41 continue
        ISTANDDATA(54) = 0
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

  if (Age_LI.ne.NullInt) then
     IAGE = ISTANDDATA(6)
     if (LKECHO)write(JOSTND,'(T12,''AGE: '',T34,I6)') IAGE
  end if
  if (Aspect_LI.ne.NullInt) then
     ASPECT = RSTANDDATA(7)
     if (LKECHO)write(JOSTND,'(T12,''ASPECT: '',T34,F6.1)') ASPECT
  end if
  if (Slope_LI.ne.NullInt) then
     SLOPE = RSTANDDATA(8)
     if (LKECHO)write(JOSTND,'(T12,''SLOPE: '',T34,F6.2)') SLOPE
  end if
  if (Elev_LI.ne.NullInt .and.&
       &ElevFT_LI.eq.NullInt) then
     if (RSTANDDATA(9).gt.0)ELEV = RSTANDDATA(9)
     if (LKECHO)write(JOSTND,'(T12,''ELEVATION: '',T34,F6.1)') ELEV
  end if
  if (ElevFT_LI.ne.NullInt ) then
     if (RSTANDDATA(33).gt.0.)ELEV = RSTANDDATA(33)*.01
     if (LKECHO)write(JOSTND,10) RSTANDDATA(33),ELEV
     10 format (T12,'ELEVFT: ',T34,F6.1,' CONVERTED TO: ',F6.1)
  end if
  if (Basal_LI.ne.NullInt) then
     BAF = RSTANDDATA(10)
     IF(LKECHO)WRITE(JOSTND,'(T12,''BASAL_AREA_FACTOR: '',          &
          &   T34,F6.1)') BAF
  end if
  if (PlotArea_LI.ne.NullInt) then
     FPA = RSTANDDATA(11)
     if (LKECHO)write(JOSTND,'(T12,''INV_PLOT_SIZE: '',T34,F6.0)')FPA
  end if
  if (BPDBH_LI.ne.NullInt) then
     BRK = RSTANDDATA(12)
     if (LKECHO)write(JOSTND,'(T12,''BRK_DBH: '',T34,F6.1)') BRK
  end if
  if (NumPlots_LI.ne.NullInt) then
     IPTINV = ISTANDDATA(13)
     if (LKECHO)write(JOSTND,'(T12,''NUM_PLOTS: '',T34,I6)') IPTINV
  end if
  if (NonStock_LI.ne.NullInt) then
     NONSTK = ISTANDDATA(14)
     if (LKECHO)write(JOSTND,'(T12,''NONSTK_PLOTS: '',T34,I6)')&
          &NONSTK
  end if
  if (SamWt_LI.ne.NullInt) then
     SAMWT = RSTANDDATA(15)
     if (LKECHO)write(JOSTND,'(T12,''SAM_WT: '',T24,F16.6)') SAMWT
  end if
  if (Stock_LI.ne.NullInt) then
     if (RSTANDDATA(16).gt.1.0.and. RSTANDDATA(16).le.100.)&
          &GROSPC = RSTANDDATA(16)*.01
     if (RSTANDDATA(16).gt.0.0.and. RSTANDDATA(16).le.1.0)&
          &GROSPC = RSTANDDATA(16)
     if (GROSPC.lt.0.) then
        XXG = 1.0
        if (IPTINV.gt.0.and. NONSTK.gt.0.and.&
             &IPTINV-NONSTK.gt.0) XXG = (FLOAT(IPTINV)-FLOAT(NONSTK))/&
             &FLOAT(IPTINV)
     else
        XXG = GROSPC
     end if
     if (LKECHO)write(JOSTND,'(T12,''STK_PCNT: '',T34,F6.3)') XXG
  end if
  if (DGT_LI.ne.NullInt) then
     IDG = ISTANDDATA(17)
     if (LKECHO)write(JOSTND,'(T12,''DG_TRANS: '',T34,I6)') IDG
  end if
  if (DGM_LI.ne.NullInt) then
     if (RSTANDDATA(18).gt.0.)FINT = RSTANDDATA(18)
     IFINT = IFIX(FINT)
     if (LKECHO)write(JOSTND,'(T12,''DG_MEASURE: '',T36,F4.1)') FINT
  end if
  if (HTT_LI.ne.NullInt) then
     IHTG = ISTANDDATA(19)
     if (LKECHO)write(JOSTND,'(T12,''HTG_TRANS: '',T34,I6)') IHTG
  end if
  if (HTM_LI.ne.NullInt) then
     if (RSTANDDATA(20).gt.0.)FINTH = RSTANDDATA(20)
     IFINTH = IFIX(FINTH)
     if (LKECHO)write(JOSTND,'(T12,''HTG_MEASURE: '',T36,F4.1)')&
          &FINTH
  end if
  if (Mort_LI.ne.NullInt) then
     FINTM = RSTANDDATA(21)

     if (FINTM.le.0.)FINTM = 5.
     if (LKECHO)write(JOSTND,'(T12,''MORT_MEASURE: '',T36,F4.1)')&
          &FINTM
  end if

  !     SITE SPECIES CODE PROCESSING
  if (SiteSp_LI.gt.0) then
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
           CSITECODE = ADJUSTL(CSITECODE)
           CSITECODE = '0'//CSITECODE(:LEN(CSITECODE)-1)
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
        20 format (T12,'SITE_SPECIES: ',T30,A,&
             &' MAPPED TO INTERNAL CODE: ',A)
     else
        write (JOSTND,25) ADJUSTR(CSITECODE)
        25 format (T12,'SITE_SPECIES: ',T30,A,' WAS NOT RECOGNIZED')
     end if
  end if

  !     SITE INDEX PROCESSING

  if (SiteIndx_LI.ne.NullInt) then
     if (RSTANDDATA(35).le.7.) then ! DUNNING CODE.
        select case (VARACD)
        case('CA','NC','SO','WS')
           if (LKECHO)write(JOSTND,27)
           27 format(38X,'   SITE INDEX IS GREATER THAN 8 AND ',&
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

  if (MaxB_LI.ne.NullInt) then
     BAMAX = RSTANDDATA(22)
     if (BAMAX.gt.0.)LBAMAX = .true.
     if (LKECHO)write(JOSTND,'(T12,''MAX_BA: '',T34,F6.1)') BAMAX
  end if
  if (MaxSDI_LI.ne.NullInt) then
     do I = 1,MAXSP
        SDIDEF(I) = RSTANDDATA(36)
     end do
     if (LKECHO)write(JOSTND,'(T12,''MAX_SDI: '',T34,F6.1)')SDIDEF(1)
  end if
  if (Model_LI.ne.NullInt) then
     IMODTY = ISTANDDATA(25)
     if (LKECHO)write(JOSTND,'(T12,''MODEL_TYPE: '',T34,I6)') IMODTY
  end if
  if (PhysioR_LI.ne.NullInt) then
     IPHREG = ISTANDDATA(26)
     if (LKECHO)write(JOSTND,'(T12,''PHYSIO_REGION: '',T34,I6)')&
          &IPHREG
  end if
  if (ForType_LI.ne.NullInt) then
     IFORTP = ISTANDDATA(27)
     if (IFORTP .gt.999) then

        !  THE LAST 3 CHARACTERS INDICATE THE FOREST TYPE AND THE FIRST
        !  CHARACTER INDICATES THAT THE USER SET THE FOREST TYPE TO BE
        !  CONSTANT FOR ALL CYCLES.  THE FIELD 3 INPUT IS DECODED
        !  AND LFLAGV IS SET TO TRUE TO INDICATE CONSTANT FOREST TYPE

        XTMP = FLOAT(IFORTP)
        XTMP = XTMP/1000. + 0.00001
        IXTMP = INT(XTMP)
        IFORTP = INT((XTMP-FLOAT(IXTMP))*1000)
        LFLAGV = .true.
     end if

     !  CALL FORTYP TO CHECK FOR VALID USER INPUT OF IFORTP VALUE

     DUM1 = 0.
     IXF = 1
     call FORTYP(IXF,DUM1)
     if (LKECHO)write(JOSTND,'(T12,''FOREST_TYPE: '',T34,I6)') IFORTP
  end if
  if (State_LI.ne.NullInt) then
     ISTATE = ISTANDDATA(37)
     if (LKECHO)write(JOSTND,'(T12,''STATE: '',T34,I6)') ISTATE
  end if
  if (Connty_LI.ne.NullInt) then
     ICNTY = ISTANDDATA(38)
     if (LKECHO)write(JOSTND,'(T12,''COUNTY: '',T34,I6)') ICNTY
  end if

  !     FUEL LOAD PARAMETERS

  LFMYES = .false.
  if (Fuel0_LI.ne.NullInt) then
     LFMYES = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_0_1_H: '',T32,F8.3)')&
          &RSTANDDATA(39)
  else
     RSTANDDATA(39) = -1.
  end if
  if (Fuel1_LI.ne.NullInt) then
     LFMYES = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_1_3_H: '',T32,F8.3)')&
          &RSTANDDATA(40)
  else
     RSTANDDATA(40) = -1.
  end if
  if (Fuel3_LI.ne.NullInt) then
     LFMYES = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_3_6_H: '',T32,F8.3)')&
          &RSTANDDATA(41)
  else
     RSTANDDATA(41) = -1.
  end if
  if (Fuel6_LI.ne.NullInt) then
     LFMYES = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_6_12_H: '',T32,F8.3)')&
          &RSTANDDATA(42)
  else
     RSTANDDATA(42) = -1.
  end if
  if (Fuel12_LI.ne.NullInt) then
     LFMYES = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_12_20_H: '',T32,F8.3)')&
          &RSTANDDATA(43)
  else
     RSTANDDATA(43) = -1.
  end if
  if (FuelLt_LI.ne.NullInt) then
     LFMYES = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_LITTER: '',T32,F8.3)')&
          &RSTANDDATA(44)
  else
     RSTANDDATA(44) = -1.
  end if
  if (FuelDf_LI.ne.NullInt) then
     LFMYES = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_DUFF: '',T32,F8.3)')&
          &RSTANDDATA(45)
  else
     RSTANDDATA(45) = -1.
  end if
  if (Fuel025_LI.ne.NullInt) then
     LFMYES = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_0_25_H: '',T32,F8.3)')&
          &RSTANDDATA(46)
  else
     RSTANDDATA(46) = -1.
  end if
  if (Fuel251_LI.ne.NullInt) then
     LFMYES = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_25_1_H: '',T32,F8.3)')&
          &RSTANDDATA(47)
  else
     RSTANDDATA(47) = -1.
  end if
  if (Fuel20_LI.ne.NullInt) then
     LFMYES = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_20_35_H: '',T32,F8.3)')&
          &RSTANDDATA(48)
  else
     RSTANDDATA(48) = -1.
  end if
  if (Fuel35_LI.ne.NullInt) then
     LFMYES = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_35_50_H: '',T32,F8.3)')&
          &RSTANDDATA(49)
  else
     RSTANDDATA(49) = -1.
  end if
  if (Fuel50_LI.ne.NullInt) then
     LFMYES = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_GT_50_H: '',T32,F8.3)')&
          &RSTANDDATA(50)
  else
     RSTANDDATA(50) = -1.
  end if

  LFMYES2 = .false.
  if (FuelS025_LI.ne.NullInt) then
     LFMYES2 = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_0_25_S: '',T32,F8.3)')&
          &RSTANDDATA(55)
  else
     RSTANDDATA(55) = -1.
  end if
  if (FuelS251_LI.ne.NullInt) then
     LFMYES2 = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_25_1_S: '',T32,F8.3)')&
          &RSTANDDATA(56)
  else
     RSTANDDATA(56) = -1.
  end if
  if (FuelS1_LI.ne.NullInt) then
     LFMYES2 = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_1_3_S: '',T32,F8.3)')&
          &RSTANDDATA(57)
  else
     RSTANDDATA(57) = -1.
  end if
  if (FuelS3_LI.ne.NullInt) then
     LFMYES2 = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_3_6_S: '',T32,F8.3)')&
          &RSTANDDATA(58)
  else
     RSTANDDATA(58) = -1.
  end if
  if (FuelS6_LI.ne.NullInt) then
     LFMYES2 = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_6_12_S: '',T32,F8.3)')&
          &RSTANDDATA(59)
  else
     RSTANDDATA(59) = -1.
  end if
  if (FuelS12_LI.ne.NullInt) then
     LFMYES2 = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_12_20_S: '',T32,F8.3)')&
          &RSTANDDATA(60)
  else
     RSTANDDATA(60) = -1.
  end if
  if (FuelS20_LI.ne.NullInt) then
     LFMYES2 = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_20_35_S: '',T32,F8.3)')&
          &RSTANDDATA(61)
  else
     RSTANDDATA(61) = -1.
  end if
  if (FuelS35_LI.ne.NullInt) then
     LFMYES2 = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_35_50_S: '',T32,F8.3)')&
          &RSTANDDATA(62)
  else
     RSTANDDATA(62) = -1.
  end if
  if (FuelS50_LI.ne.NullInt) then
     LFMYES2 = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_GT_50_S: '',T32,F8.3)')&
          &RSTANDDATA(63)
  else
     RSTANDDATA(63) = -1.
  end if

  if (StdOrgCd_LI.ne.NullInt) ISTDORG = ISTANDDATA(64)

  !     FUEL MODEL

  LFMD = .false.
  if (FuelModel_LI.ne.NullInt) then
     LFMD = .true.
     if (LKECHO)write(JOSTND,'(T12,''FUEL_MODEL: '',T34,I6)')&
          &ISTANDDATA(51)
  else
     ISTANDDATA(51) = -1
  end if

  !     FUEL PHOTO REFERENCE

  LFOTO = .false.
  if (FOTOREF_LI.ne.NullInt) then
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
     WRITE(JOSTND,'(T12,''MISSING PHOTO '',                          &
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
     X(1) = FLOAT(ISTANDDATA(51))
     call OPNEW(I,1,2538,1,X)
  end if

  if (LFMD.and..not. LFMLK) write(JOSTND,&
       &'(T12,''FIRE MODEL NOT LINKED, FUEL MODEL IGNORED.'')')

  if (LKECHO)write(JOSTND,'(T12,''END OF DATA BASE READ.'')')

  return
end
