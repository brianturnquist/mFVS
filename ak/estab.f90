!=========================================!
!   Establish regeneration tree records   !
!=========================================!
! AK $Id$
subroutine ESTAB (KDT)
  implicit none

  ! AK estab.f

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ESPARM.f90'
  include 'ARRAYS.f90'
  include 'CALDEN.f90'
  include 'CONTRL.f90'
  include 'ESCOM2.f90'
  include 'ESCOMN.f90'
  include 'ESHAP.f90'
  include 'ESHAP2.f90'
  include 'ESTREE.f90'
  include 'PDEN.f90'
  include 'PLOT.f90'
  include 'STDSTK.f90'
  include 'VARCOM.f90'

  !     REGENERATION ESTABLISHMENT MODEL   --   VERSION 2.0
  !     USDA FOREST SERVICE, INTERMOUNTAIN RESEARCH STATION
  !     1221 S. MAIN, MOSCOW, IDAHO  83843   (208) 882-3557

  !     THIS PROGRAM MODELS REGENERATION ESTABLISHMENT IN THE ALASKA
  !     VARIANT. EXISTING MODELS REPLACED IN 2020 REFIT

  ! SPECIES LIST FOR ALASKA VARIANT.

  ! Number Code  Common Name         FIA  PLANTS Scientific Name           MIN.HT

  !   1     SF   Pacific silver fir  011  ABAM   Abies amabilis            1.0 FT
  !   2     AF   subalpine fir       019  ABLA   Abies lasiocarpa          0.5 FT
  !   3     YC   Alaska cedar        042  CANO9  Callitropsis nootkatensis 0.5 FT
  !   4     TA   tamarack            071  LALA   Larix laricina            0.5 FT
  !   5     WS   white spruce        094  PIGL   Picea glauca              0.5 FT
  !   6     LS   Lutz’s spruce            PILU   Picea lutzii              0.5 FT
  !   7     BE   black spruce        095  PIMA   Picea mariana             0.5 FT
  !   8     SS   Sitka spruce        098  PISI   Picea sitchensis          0.5 FT
  !   9     LP   lodgepole pine      108  PICO   Pinus contorta            1.0 FT
  !  10     RC   western redcedar    242  THPL   Thuja plicata             0.5 FT
  !  11     WH   western hemlock     263  TSHE   Tsuga heterophylla        0.5 FT
  !  12     MH   mountain hemlock    264  TSME   Tsuga mertensiana         0.5 FT
  !  13     OS   other softwoods     298  2TN                              0.5 FT
  !  14     AD   alder species       350  ALNUS  Alnus species             1.0 FT
  !  15     RA   red alder           351  ALRU2  Alnus rubra               1.0 FT
  !  16     PB   paper birch         375  BEPA   Betula papyrifera         1.0 FT
  !  17     AB   Alaska birch        376  BENE4  Betula neoalaskana        1.0 FT
  !  18     BA   balsam poplar       741  POBA2  Populus balsamifera       1.0 FT
  !  19     AS   quaking aspen       746  POTR5  Populus tremuloides       1.0 FT
  !  20     CW   black cottonwood    747  POBAT  Populus trichocarpa       1.0 FT
  !  21     WI   willow species      920  SALIX  Salix species             1.0 FT
  !  22     SU   Scouler’s willow    928  SASC   Salix scouleriana         1.0 FT
  !  23     OH   other hardwoods     998  2TB                              1.0 FT

  ! Variables
  external ESRANN

  ! Variables
  logical :: DEBUG

  character(len=2) :: CBLANK

  character(len=2) :: CLAST(MAXSP)
  character(len=4) :: CPREP(4),CTOPO(5)
  character(len=47) :: HABTYP(16)
  character(len=7) :: SERIES(16)
  character(len=3) :: FINAL

  integer :: I,IACTK,IBRKUP,IDO,IDUP,IEND,IESFT,IEVTYR,II,INGRO,IPNSPE
  integer :: IPOLD,IPYEAR,IREP,ISER,ISHADE,ISTART,ITIME,ITODO,ITP,M
  integer :: ITPP,ITRNIN,ITYPEP,IZERO,J,JJ,K,KDT,L,MATCH,MYDO,MXRR
  integer :: N,NBEST,NCOUNT,NDRAW,NEWTPP,NN,NOFSPE,NP,NSPNZ,NTIMES
  integer :: NTODO,NUM2PK,NUMSPE,IFT

  integer*4 IDCMP1,ITEMP,ITMP,ITOOMP

  integer :: IASEP(99),IBEST(MAXSP),ICHOI(2,MAXSP),ICODE(99)
  integer :: ILSP(MAXSP),IPLANT(MAXSP),MAXING(14),MAXSPP(14)
  integer :: MAXTPP(14),MYACTS(4),MYHABG(16),NNPREP(4)
  integer :: NOTE(99),JNULL(MAXSP)

  real    :: BAAOLD,BACHLO,BRKUP,CRDUM,CW,DELAY,DILATE,DRAW,DUP,DUPNPT
  real    :: EMSQR,ESAVE,EXPAND,FLOKDT,FLONPT,FTEMP,FTEMP2,GENTIM,HHT
  real    :: PASMAX,PN,PNONE,PTREE,RADIAN,SHADE,SHORTY
  real    :: SUM0,SUM1,SUM2,TBAAA,TCROP1,TCROP2,TMTIME,TPP,TRAGE,TREEHT
  real    :: TTOTTP,XCSMAX,XH,XXH,ZHARV,WTAVEHT
  real    :: PTA,TPAOLD

  real    :: AGADSB(99),AGEPL(99),AGEXC(99),AVEHT(MAXSP),BESPCT(MAXSP)
  real    :: BESTPA(MAXSP),ESPROB(99),EXCESS(MAXSP),FIRST(2,MAXSP)
  real    :: HEIGHT(99),HTIMLT(99),PASPCT(MAXSP),PASTPA(MAXSP),PRMS(6)
  real    :: PSPE(6),STOMLT(MAXSP),SUMESP(MAXSP),SUMHTS(MAXSP)
  real    :: SUMUP(MAXSP),TALL(MAXSP),TOTPCT(MAXSP),TOTTPA(MAXSP)

  !  DATA STATEMENTS:

  data CBLANK/'  '/, &
       & CPREP/'NONE','MECH','BURN','ROAD'/, &
       & CTOPO/'BOTM','LOWR',' MID','UPPR','RIDG'/, &
       & IDCMP1/10000000/, &
       & MYACTS/430,431,491,493/, &
       & NOFSPE/MAXSP/

  ! MAXTPP: CONTAINS MAX TPP VALUES USED IN TALLY SEQUENCES. VALUES ARE
  ! BASED ON THE 75TH PERCENTILE TPP VALUES FOUND IN THE DATA FOR
  ! EACH FOREST TYPE.

  ! MAXSPP: CONTAINS MAX NUMBER OF SPECIES THAT CAN OCCUR ON A STOCKED
  ! PLOT BY FOREST TYPE. VALUES ARE BASED ON THE 75TH PERCENTILE VALUES
  ! FOUND IN THE DATA FOR FOREST TYPE.

  ! MAXING: CONTAINS MAX TPP VALUES USED IN INGROWTH PREDICTIONS BY FOREST
  ! TYPE. VALUES ARE ALL SET TO 3.FOR MOST FOREST TYPES, A VALUE OF 3
  ! WAS CLOSE TO THE 25TH PERCENTILE TPP VALUE. THE 25TH PERCENTILE TPP
  ! VALUE WAS GREATER THAN 3 FOR BE, YC, LP, AND RC, FOREST TYPES BUT
  ! THESE VALUES YIELDED INGROWTH VALUES THAT SEEMED TO HIGH FOR 20
  ! YEAR INGROWTH INTERVALS.

  data &
       & MAXTPP/17,30,10,22,21,21,21,10,10,15,15,17,5,0/, &
       & MYHABG/4*1,4*2,3,4,6*5/, &
       & MAXSPP/4,4,4,5,6,5,6,4,3,4,4,4,3,0/, &
       & MAXING/3,3,3,3,3,3,3,3,3,3,3,3,3,0/
  data HABTYP/ &
       & 'PSME/ VAGL, LIBO, VACA', &
       & 'PSME/ CARU, CAGE, GRASS', &
       & 'PSME/ PHMA, ACGL', &
       & 'PSME/ SYAL, SPBE, SYOR, ARUV, ARCO', &
       & 'ABGR/ LIBO, CLUN-XETE', &
       & 'ABGR/ XETE, VAGL, COOC, VACA', &
       & 'ABGR/ CLUN (EXCEPT CLUN-XETE)', &
       & 'ABGR/ SPBE, ACGL, PHMA, ASCA, SETR', &
       & 'THPL/ ALL', &
       & 'TSHE/ ALL', &
       & 'ABLA/ VAGL, VASC, VACA', &
       & 'ABLA/ XETE, LIBO', &
       & 'ABLA/ CLUN, GATR', &
       & 'ABLA/ CAGE, CARU, ACGL, SPBE', &
       & 'ABLA/ MEFE, ALSI, TSME/ CLUN, XETE, MEFE, STAM', &
       & 'ABLA/ CACA, STAM, LUHI'/
  data SERIES/ &
       & 'DF     ', &
       & 'DF     ', &
       & 'DF     ', &
       & 'DF     ', &
       & 'GF     ', &
       & 'GF     ', &
       & 'GF     ', &
       & 'GF     ', &
       & 'C      ', &
       & 'WH     ', &
       & 'AF & MH', &
       & 'AF & MH', &
       & 'AF & MH', &
       & 'AF & MH', &
       & 'AF & MH', &
       & 'AF & MH'/
  data FINAL/"ALL"/

  call DBCHK (DEBUG,'ESTAB',5,ICYC)
  if (DEBUG) write(JOSTND,7000) KDT,IDSDAT,NTALLY
  7000 format(/'IN ESTAB; BEGIN DEBUG.  KDT=',I5,' IDSDAT=',I5, &
       & ' NTALLY=',I2)

  ! STEP 1: INITIALIZE ARRAYS AND VARIABLES, AND REPLICATE PLOTS

  if (DEBUG) write(JOSTND,9991)
  9991 format(/,'STEP 1: INITIALIZE ARRAYS AND VARIABLES,', &
       & ' AND REPLICATE PLOTS.')
  ITRNIN = ITRN+1
  if (DEBUG) write(JOSTND,*)'ITRNIN:',ITRNIN,'ITRN:', ITRN
  INGRO = 0
  if (DEBUG) write(JOSTND,*)'INGRO:',INGRO

  !     ZHARV IS DATE OF DISTURBANCE, SET TO YEAR OF HARVEST
  ZHARV = real(IDSDAT)
  if (DEBUG) write(JOSTND,*)'ZHARV IS SET TO DATE OF DISTURBANCE', &
       & ' ZHARV:',ZHARV,'IDSDAT:', IDSDAT

  !     FLOKDT IS YEAR OF TALLY (FALL OF LAST YEAR OF CYCLE)
  FLOKDT = real(KDT)
  if (DEBUG) write(JOSTND,*)'FLOKDT IS YEAR OF TALLY. FLOKDT:', &
       & FLOKDT

  !     TIME (GLOBAL) IS YEARS SINCE DISTURBANCE
  TIME = FLOKDT - ZHARV +1.0
  if (DEBUG) write(JOSTND,*)'TIME IS YEARS SINCE DISTURBANCE AND ', &
       & 'SET TO FLOKDT-ZHARV+1. '
  if (DEBUG) write(JOSTND,*)' TIME:',TIME,'FLOKDT:',FLOKDT,'ZHARV:', &
       & ZHARV

  !     FLONPT (LOCAL) IS THE NUMBER OF INVENTORY POINTS IN BASE MODEL
  FLONPT = real(NPTIDS)
  if (DEBUG) write(JOSTND,*)'FLONPT IS # OF INV. POINTS IN BASE. ', &
       & 'FLONPT:',FLONPT
  SUMPRB = 0.0
  ELEVSQ = ELEV*ELEV
  if (DEBUG) write(JOSTND,*)'ELEVATION SET TO STAND ELEVATION. ', &
       & 'ELEV:', ELEV,'ELEVSQ:',ELEVSQ
  !     ITMP IS SET TO CURRENT CYCLE*10000, I DON'T KNOW WHY YET
  ITMP = ICYC
  ITMP = ITMP*10000
  if (DEBUG) write(JOSTND,*)'ITMP IS SET TO CURRENT CYCLE*10000.', &
       & ' ITMP:',ITMP, 'ICYC:', ICYC
  !     TURN ON FULL ESTABLISHMENT MODEL (0), PLANT AND NATURAL ONLY (1)
  MODE = 0
  if (DEBUG) write(JOSTND,*)'IF MODE=0, TURN ON FULL ESTAB. IF ', &
       & 'MODE=1, PLANT AND NATURAL ONLY. MODE:', MODE

  !     IF TALLY DATE IS 20 YEARS PAST INVENTORY, SET FVS TO ADD
  !     REGENERATION FROM ADVANCED COMPONENT. DO NOT LOAD SITE PREP FROM
  !     INPUT DATA
  if (KDT+1-IY(1).gt.20) then
     INADV = 1
     LOAD = 0
  end if
  if (DEBUG) write(JOSTND,*)'IF (KDT+1-IY(1).GT.20) THEN INADV=1 ', &
       & 'AND LOAD=0.'
  if (DEBUG) write(JOSTND,*)' KDT:',KDT,'IY(1):', IY(1),' INADV:', &
       & INADV,'LOAD:',LOAD

  !     PASMAX IS SET TO THE NUMBER OF EXCESS TREES PER SPECIES PASSED
  !     TO MAIN MODEL
  PASMAX = CONFID
  if (DEBUG) write(JOSTND,*)'PASMAX SET TO # OF EXCESS TREES TO ', &
       & 'PASS TO MAIN MODEL.'
  if (DEBUG) write(JOSTND,*)' PASMAX:', PASMAX, 'CONFID:',CONFID
  do I = 1,99
     NOTE(I) = 0
     AGADSB(I) = 0.0
     AGEPL(I) = 0.0
     AGEXC(I) = 0.0
     40 continue
  end do
  if (DEBUG) write(JOSTND,*)'FIRST IS A 2x23 ARRAY. ALL VALUES SET', &
       & ' TO 0.1.'
  do I = 1,2
     do N = 1,NOFSPE
        FIRST(I,N) = 0.1
     end do
  end do

  !     SUMMATION OF TREES TO PASS TO MAIN MODEL
  TCROP1 = 0.0
  TCROP2 = 0.0
  TTOTTP = 0.0

  !     SOME SPECIES LEVEL ATTRIBUTES
  do I = 1,NOFSPE
     TOTTPA(I) = 0.0
     AVEHT(I) = 0.0
     SUMPX(I) = 0.0
     SUMPI(I) = 0.0
     BESTPA(I) = 0.0
     PASTPA(I) = 0.0
     IPLANT(I) = 0
     181 continue
  end do

  !   CALCULATE FOREST TYPE FOR TALLY SEQUENCE
  !   IFORTP IS CURRENT STAND FOREST TYPE, FROM PLOT.F77 AND IF IT HAS
  !   BECOME UNDEFINED, THE INVENTORY FOREST TYPE (IIFORTP) WILL BE USED.

  select case (IFORTP)
  case (122, 125, 264, 268, 270, 271, 281, 301, 304, 305, &
       & 703, 704, 709, 901, 902, 904, 911)
     IESFT = IFORTP
  case default
     IESFT = IIFORTP
  end select
  if (DEBUG) write(JOSTND,*)'SET FOREST TYPE. IF CURRENTLY ', &
       & 'UNDEFINED, SET TO INVENTORY FOREST TYPE.'
  if (DEBUG) write(JOSTND,*)' IESFT:', IESFT, &
       & 'CURRENT FT IFORTP:',IFORTP,'INVENTORY FT IIFORTP:',IIFORTP
  !   THIS FOREST TYPE WILL BE USED THROUGHOUT THE DURATION OF
  !   THE TALLY SEQUENCE
  !   SET IFT TO FOREST TYPE GROUPS 1-13.

  !   IFT USED TO SET PLOT SIZE AND DETERMINING PROBABILITIES
  select case (IESFT)
  case (122)
     IFT = 1
  case (125)
     IFT = 2
  case (270, 268)
     IFT = 3
  case (271)
     IFT = 4
  case (281)
     IFT = 5
  case (301, 264)
     IFT = 6
  case (304)
     IFT = 7
  case (305)
     IFT = 8
  case (703, 704, 709)
     IFT = 9
  case (901)
     IFT = 10
  case (902)
     IFT = 11
  case (904)
     IFT = 12
  case (911)
     IFT = 13
  case default
     IFT = 14
  end select

  ! DISPLAY IFT
  if (DEBUG) write(JOSTND,*) 'IFT DETERMINED FROM FOREST TYPE: ', IFT

  ! STORE IFT0 IF GOING INTO A TALLY SEQUENCE
  if (NTALLY .eq. 1) then
     IFT0 = IFT
     if (DEBUG) write(JOSTND, *) 'IF FIRST TALLY, STORE FOREST ', &
          & 'TYPE. IFT0: ', IFT0, 'IFT', IFT
  end if

  ! USE IFT0 IF IN A TALLY SEQUENCE
  if (NTALLY .lt. 99) then
     IFT = IFT0
     if (DEBUG) write(JOSTND, *) 'IF IN TALLY SEQUENCE, SET FOREST ', &
          & ' TYPE TO FIRST TALLY FOREST TYPE.'
     if (DEBUG) write(JOSTND, *) ' IFT: ', IFT,'IFT0:',IFT0
  end if

  !     DETERMINE NUMBER OF PLOT REPLICATES ON EACH INVENTORY POINT
  !     DUP    IS THE NUMBER OF DUPLICATE REGENERATION PLOTS PER BASE
  !            MODEL PLOT.
  !     MINREP IS TOTAL NUMBER OF REGEN PLOTS. BY DEFAULT IT IS 50
  !            BUT CAN BE RESET BY THE USER. MINREP SET IN ESINIT OR BY
  !            USER.
  !     NPTIDS IS NUMBER OF INVENTORY POINTS IN
  !     NUMBER OF REPLICATES (DUP) PER PLOT IS ESSENTIALLY AMOUNTS TO
  !     FOLLOWING RELATIONSHIP IN LOOP 138:
  !     DUP = MINREP / NPTIDS ROUNDED UP TO NEAREST INTEGER
  !     TOTAL NUMBER OF REGEN PLOTS: DUP * NPTIDS
  if (DEBUG) write(JOSTND,*)'CREATE REGENERATION PLOTS BY ', &
       & 'DETERMINING NUMBER OF REPLICATES OF BASE MODEL PLOTS.'
  if (DEBUG) write(JOSTND,*)'TARGET NUMBER OF REPLICATES. MINREP:', &
       & MINREP
  if (DEBUG) write(JOSTND,*)'MAXPLT IS: ', &
       & MAXPLT
  DUP = 0.0
  do I = 1,MINREP
     DUP = DUP+1.0
     N = NPTIDS*I
     if (NPTIDS*(I+1).gt.MAXPLT) go to 139
     if (N.ge.MINREP) go to 139
     138 continue
  end do
  139 continue
  IDUP = INT(DUP+0.5)
  !     DUPNPT IS THE TOTAL NUMBER OF DUPLICATE PLOTS
  DUPNPT = FLONPT*DUP
  if (DEBUG) write(JOSTND,*)'TOTAL NUMBER OF REGENERATION PLOTS:', &
       & DUPNPT,'FROM',FLONPT,'BASE PLOTS AND',DUP,'DUPLICATES.'

  !     EXPAND IS THE INVERSE OF INVENTORY PLOT SIZE USED IN FITTING
  !     ESTABLISHMENT MODEL RELATIONSHIPS. SET BY FOREST TYPE.
  !     BOREAL FOREST TYPES SET TO 1/250TH (UAF DATASET)
  !     COASTAL SET TO 1/300TH (FIA DATASET)
  select case (IFT)
  case (1,2,10,11,12)
     EXPAND = 250.
  case default
     EXPAND = 300.
  end select
  if (DEBUG) write(JOSTND,*)'EXPAND IS USED TO DETERMINE TPA OF', &
       & ' RECORDS BASED ON PLOT SIZE OF FITTING DATASET.'
  if (DEBUG) write(JOSTND,*)'COASTAL FOREST TYPES SET TO 300 AND ', &
       & 'BOREAL SET TO 250. EXPAND:', EXPAND

  ! STEP 2: INITIALIZE REGENERATION VECTORS AND CALIBRATION TASKS

  ! VARIABLE DEFINITIONS
  ! XTES    -- RATIO OF REMOVALS AS CALCULATED IN ESNUTR.
  ! THRES1  -- LOWER THRESHOLD OF REMOVALS THAT TRIGGERS ONE TALLY.
  ! ZHARV   -- DATE OF HARVEST
  ! ZMECH   -- DATE OF MECHANICAL SITE PREP
  ! ZBURN   -- DATE OF BURN SITE PREP
  ! ITEMP   -- VARIABLE USED TO DEFINE VALUE OF SHORTY IN INGROWTH
  !            INITIALIZATION
  ! ITRN    -- NUMBER OF TREE RECORDS
  ! FTEMP   -- DIAMETER OF TREE RECORD
  ! NN      -- SPECIES OF TREE RECORD
  ! FTEMP2  -- TREES PER ACRE OF TREE RECORD
  ! PASTTPA -- SUM OF TREES PER ACRE BY SPECIES
  ! TCROP2  -- SUM OF TREES PER ACRE
  ! SUM0    -- SUM OF TREES PER ACRE
  ! N       -- PLOT NUMBER
  ! PLPROB  -- STAND LEVEL TPA ESTIMATE OF PLOT REPLICATES, SUM TO GET
  !            STAND TPA
  ! IALN    -- ARRAY CONTAINING VALUES INDICATING WHETHER SITE PREP WAS
  !            ENTERED OR NOT. 0 IF NOT SITE PREP IS ENTERED AND 1 IF
  !            SITE PREP WAS SPECIFIED.
  ! NPTIDS  -- NUMBER OF INVENTORY PLOTS.

  !     IF NOT THE FIRST TALLY, CANCEL MECH & BURNPREP'S IN THIS CYCLE.
  if (DEBUG) write(JOSTND,9992)
  9992 format(/,'STEP 2: INITIALIZE REGENERATION ', &
       & 'VECTORS AND CALIBRATION TASKS.')
  if (NTALLY.gt.1) then
     call OPFIND (2,MYACTS(3),NTODO)
     if (NTODO.gt.0) then
        do ITODO = 1,NTODO
           call OPDEL1 (ITODO)
           6 continue
        end do
     end if
  end if
  if (DEBUG)write(JOSTND,*)'NTALLY=',NTALLY

  !     DO NOT CREATE TREE RECORDS THAT WOULD ESSENTIALLY BE REDUNDANT
  !     WITH THOSE IN THE INVENTORY WHEN NTALLY IS 1 AND 20 YEARS HAS
  !     ELAPSED SINCE THE INVENTORY (I.E. NO CALIBRATION).
  MATCH = 0
  if (NTALLY.eq.1.and.KDT+1-IY(1).gt.20) MATCH = 1
  if (DEBUG)write(JOSTND,*) 'NTALLY=',NTALLY, ' KDT=', KDT, ' IY', &
       & IY(1),' MATCH=',MATCH
  !     WORK WITH INGROWTH (99) OPTION - INITIALIZE PARAMETERS
  if (NTALLY.eq.99) then
     !       PNONE IS THE PROPORTION OF PLOTS UNTREATED, SET TO 100%
     PNONE = 1.0
     PMECH = 0.0
     PBURN = 0.0
     !       METH IS SITE PREP METHOD: 1=NONE,2=MECH,3=BURN,4=ROADS
     METH = 0
     LOAD = 0
     IALN(2) = 1
     INGRO = 1
     PASMAX = 15.0
     !       ITEMP IS A TIME ESTIMATE, SET TO 1 HERE -INGROWTH SECTION
     ITEMP = INT(((XTES/THRES1) *FINT) +0.5)
     if (ITEMP.lt.1) ITEMP = 1
     SHORTY = real(ITEMP)
     !       DATES OF SITE PREPS
     ZHARV = real(KDT+1) -SHORTY
     ZMECH = ZHARV
     ZBURN = ZHARV
     if (DEBUG)write(JOSTND,*) 'INGROWTH INITIALIZATION TIME', &
          & ' VARIABLES.',' ITEMP=',ITEMP,' XTES=', XTES,' THRES1=',THRES1
     if (DEBUG)write(JOSTND,*)' FINT=',FINT,' SHORTY=',SHORTY, &
          & ' ZHARV=',ZHARV,' ZMECH=',ZMECH,' ZBURN=',ZBURN
     NTALLY = 1
     go to 44
  end if

  !     IF NOT THE FIRST TALLY, THEN SKIP SITE PREPS.
  if (NTALLY.ne.1) go to 276

  !     PROCESS SITE PREPARATION KEYWORDS
  call ESETPR (METH,ZMECH,ZBURN,PNONE,PMECH,PBURN,IALN, &
       & IDSDAT,KDT,LOAD)

  !     SET DATES OF MECH AND BURN SITE PREP TO OCCUR NOT BEFORE
  !     HARVEST DATE
  if (ZMECH.lt.ZHARV) ZMECH = ZHARV
  if (ZBURN.lt.ZHARV) ZBURN = ZHARV
  if (DEBUG) write(JOSTND,7011) METH,IDSDAT,KDT,ZMECH,ZBURN, &
       & PNONE,PMECH,PBURN,(IALN(I),I = 1,3)
  7011 format('AFTER ESETPR: METH=',I2,' IDSDAT=',I5,' KDT=',I5, &
       & ' ZMECH=',F7.1,' ZBURN=',F7.1,/,4X,'PNONE=',F7.4,' PMECH=', &
       & F7.4,' PBURN=',F7.4,3X,'IALN(I)=',3I5)
  44 continue

  !     KDTOLD IS LAST CYCLE TALLY DATE
  KDTOLD = INT(ZHARV-0.5)
  if (DEBUG)write(JOSTND,*) ' KDTOLD=',KDTOLD

  !     INITIALIZE SUMPRE(1:5) TO 0. SUMPRE IS SUMMARY COUNT OF SITE PREPS
  !     1=NONE, 2=MECH, 3=BURN, 4&5=?
  do I = 1,5
     SUMPRE(I) = 0.0
     45 continue
  end do

  !     INITIALIZE IHABT TO 0. IHABT IS SUMMARY COUNT OF HABITAT TYPES
  do I = 1,16
     IHABT(I) = 0
     46 continue
  end do

  !     INITIALIZE VARIABLES ON ESTAB MODEL REGENERATION PLOTS
  do I = 1,NPTIDS*IDUP
     ESB1(I) = 0.0
     PNN(I) = 0.0
     PLPROB(I) = 0.0
     NSTORE(I) = 0
     12 continue
  end do
  ESB = 0.0
  276 continue

  !     IF FIRST TALLY, SET ITEMP TO RANDOM NUMBER. ESDRAW IS REAL OF
  !     CALL TO ESDRAW SETS UP RANDOM NUMBER SEED
  if (NTALLY.eq.1) then
     call ESRANN (DRAW)
     ITEMP = INT(DRAW*100000.0 + 0.5)
     ESDRAW = real(ITEMP)
  end if
  call ESRNSD (.true.,ESDRAW)
  if (DEBUG) write(JOSTND,7014) ESDRAW
  7014 format(' RANDOM NUMBER SEED=',F9.2)

  !     LOAD REGENERATION VECTORS FROM CURRENT INVENTORY.
  !     ONLY INCLUDE TREES LESS THAN BREAK DIAMETER (REGNBK)
  !     ITRN = NUMBER OF TREE RECORDS
  !     FTEMP = DIAMETER OF TREE RECORD
  !     NN = SPECIES OF TREE RECORD
  !     FTEMP2 = TREES PER ACRE OF TREE RECORD
  !     PASTTPA = SUM OF TREES PER ACRE PER SPECIES
  !     TCROP2 = SUM OF TREES PER ACRE
  !     SUM0 = SUM OF TREES PER ACRE
  !     N = PLOT NUMBER
  !     PLPROB = STAND LEVEL TPA ESTIMATE OF PLOT REPLICATES, SUM TO GET
  !              STAND TPA
  SUM0 = 0.0
  if (ITRN.eq.0) go to 198
  do I = 1,ITRN
     FTEMP = DBH(I)
     if (DEBUG)write(JOSTND,*)' IN DO LOOP 177 BEFORE CALIBRATION OF', &
          & ' CALCULATION BASED ON INVENTORY REGEN.', &
          & ' I=',I,' ISP=',ISP(I),' FTEMP=',FTEMP,' REGNBK=',REGNBK
     if (FTEMP.ge.REGNBK) go to 177
     NN = ISP(I)
     if (NN.lt.1.or.NN.gt.NOFSPE) go to 177
     FTEMP2 = PROB(I)
     PASTPA(NN) = PASTPA(NN)+FTEMP2
     TCROP2 = TCROP2 +FTEMP2
     SUM0 = SUM0+FTEMP2
     N = ITRE(I)
     PLPROB(N) = PLPROB(N) +(FTEMP2/DUP)
     if (DEBUG)write(JOSTND,*)' BOTTOM OF DO LOOP 177.',' I=',I, &
          & ' FTEMP2=',FTEMP2,' ISP=',NN,' PASTPA=',PASTPA(NN), &
          & ' TCROP2=',TCROP2,' SUM0=',SUM0,' ITRE=',N,' PLPROB=',PLPROB(N), &
          & ' DUP=',DUP
     177 continue
  end do
  198 continue

  !     CALCULATE CALIBRATION VARIABLES FOR DETERMINING
  !     INTERCEPT OF ACTUAL STOCKING FROM THE INVENTORY.
  !     DO NOT CALCULATE IF NOT THE FIRST TALLY OR IF PAST
  !     THE INITIAL TALLY PERIOD -20 YEARS
  !     TPACRE IS TREES PER ACRE OF REGENERATION (< REGNBK)
  !     ESA/FTEMP IS THE PROPORTION OF INVENTORY PLOTS THAT ARE STOCKED,
  !       DETERMINED FROM THE TPA(<REGNBK) IN THE STAND. EQUATION FROM
  !       CASTLE (2019) USING COMBINED UAF AND FIA DATASETS
  !     ESB IS AN INTERMEDIATE VARIABLE BASED ON ESA/FTEMP THAT IS USED
  !       IN CALIBRATION OF THE PROBABILITY OF STOCKING OF PLOTS.
  if (DEBUG)write(JOSTND,*)' INADV=',INADV,' NTALLY=',NTALLY
  if (INADV.eq.1.or.NTALLY.ne.1) go to 64
  TPACRE = SUM0
  if (TPACRE.lt.1.0) TPACRE = 1.0
  ESA = 1.0/(1.0+EXP(3.190754 - 0.59894*ALOG(TPACRE)))
  FTEMP = ESA
  if (FTEMP.lt.0.10) FTEMP = 0.10
  if (FTEMP.gt.0.90) FTEMP = 0.90
  ESB = -1.0 * ALOG(1.0/FTEMP -1.0)
  if (DEBUG) write(JOSTND,*)'IN CALIBRATION LOGIC AFTER ESA AND', &
       & ' ESB HAVE BEEN CALCULATED.',' TPACRE=',TPACRE,' ESA=',ESA, &
       & ' FTEMP=', FTEMP,'ESB=',ESB
  64 continue

  !     WORK WITH SITE PREPARATION OPTIONS. EXIT = STMT 242
  !     ASSIGN RANDOM NUMBER TO ESTABLISHMENT MODEL SUBPLOTS IN WK6 ARRAY
  do I = 1,IDUP*NPTIDS
     call ESRANN (DRAW)
     WK6(I) = DRAW
     183 continue
  end do
  if (NTALLY.ne.1) go to 242
  if (LOAD.ne.1) go to 249

  !     IF LOAD=0, USE SITE PREP ON KEYWORDS OR INVENTORY DATA
  !     IF LOAD=1, REP PLOT SITE PREPS; IF NEEDED COMPRESS ARRAY IPPREP.
  NN = 0

  !     LOOP ACROSS BASE MODEL PLOTS
  do I = 1,NPTIDS
     NN = NN+1
     N = IPTIDS(I)
     IPPREP(NN) = IPPREP(N)
     243 continue
  end do
  if (IDUP.lt.2) go to 49

  !     NESTED LOOP TO SET SITE PREP BASED ON INVENTORY PLOTS
  do I = 1,IDUP
     do N = 1,NPTIDS
        J = I*NPTIDS +N
        IPPREP(J) = IPPREP(N)
        247 continue
     end do
     251 continue
  end do
  go to 49
  249 continue

  !  BRANCH TO 246 IF USER HAS NOT SUPPLIED SITE PREPARATION KEYWORDS.
  if (IALN(2).eq.0.and.IALN(3).eq.0) go to 246
  SUM0 = PMECH+PBURN
  if (SUM0.le.1.0) go to 248
  PMECH = PMECH/SUM0
  PBURN = PBURN/SUM0
  248 continue
  PNONE = 1.0-PMECH-PBURN
  go to 252
  246 continue

  !     USER HAS NOT SUPPLIED ANY SITE PREPARATION KEYWORDS.
  !     CALL ESPREP TO GET SITE PREPARATION VALUES.
  call ESPREP (PNONE,PMECH,PBURN)
  252 continue

  !     SECTION TO CHOOSE SITE PREPS.  SAMPLE WITHOUT REPLACEMENT.
  !     SUM0 = TOTAL SITE PREP PROPORTIONS
  !     SUMUP(1) = PROBABILITY OF NO SITE PREP
  !     SUMUP(2) = PROBABILITY OF MECH SITE PREP
  !     SUMUP(3) = PROBABILITY OF BURN SITE PREP
  SUM0 = PNONE+PMECH+PBURN
  SUMUP(1) = PNONE/SUM0
  SUMUP(2) = PMECH/SUM0
  SUMUP(3) = PBURN/SUM0
  if (DEBUG) write(JOSTND,7051) (SUMUP(I),I = 1,3)
  7051 format(' PNONE=',F8.4,'  PMECH=',F8.4,'  PBURN=',F8.4)
  !     TRIPLE NESTED DO LOOP TO ASSIGN SITE PREP TO INDIVIDUAL
  !     REGENERATION PLOTS.
  N = 0
  do II = 1,IDUP
     do NN = 1,NPTIDS
        N = N+1
        DRAW = WK6(N)
        DRAW = DRAW* (((DUPNPT+1.0)-real(N))/DUPNPT)
        SUM0 = 0.0
        do I = 1,2
           SUM0 = SUM0+SUMUP(I)
           if (DRAW.gt.SUM0) go to 2
           go to 8
           2 continue
        end do
        I = 3
        if (SUMUP(3).lt.0.0) I = 1
        8 continue
        IPPREP(N) = I
        SUMUP(I) = SUMUP(I)-(1.0/DUPNPT)
        if (SUMUP(I).lt.0.0) SUMUP(I) = 0.0
        9 continue
     end do
     10 continue
  end do
  49 continue
  242 continue

  !     GET THE PLANT & NATURAL KEYWORDS READY TO ACCESS
  call OPFIND (2,MYACTS,NTODO)
  if (DEBUG) write (JOSTND,'('' # OF PLANT & NATURAL='',I4)') NTODO
  if (NTODO.gt.0) MODE = 1
  !     IF NUMBER TO ESTABLISH IS NEAR ZERO, THEN MARK KEYWORD FOR DELETION.
  !     IF THIS WAS THE ONLY ACTIVITY FOUND, SET MODE FLAG TO 0.
  !     GED 10-23-97
  MYDO = NTODO
  do IZERO = 1,NTODO
     call OPGET (IZERO,6,IPYEAR,IACTK,NP,PRMS)
     if (PRMS(2).le.0.001 .or. PRMS(3).le. 0.001) then
        call OPDEL1(IZERO)
        MYDO = MYDO-1
     end if
     244 continue
  end do
  if (MYDO .eq. 0) MODE = 0

  !     ACCUMULATE SUMS FOR CALCULATION OF SHADE ADJUSTMENTS
  SUM1 = 0.
  SUM2 = 0.
  do NN = 1,NPTIDS
     NNID = IPTIDS(NN)
     TBAAA = BAAA(NNID)
     if (TBAAA .lt. 1.)TBAAA = 1.
     SUM1 = SUM1+TBAAA
     if (DEBUG)write(JOSTND,*)' IN FIRST SHADE ADJUSTMENT SUMS LOOP.', &
          & ' (245).',' NPTID=',NN,' BAAA=',BAAA(NNID),' TBAAA=',TBAAA, &
          & ' SUM1=',SUM1
     245 continue
  end do
  do NN = 1,NPTIDS
     NNID = IPTIDS(NN)
     TBAAA = BAAA(NNID)
     if (TBAAA .lt. 1.)TBAAA = 1.
     SUM2 = SUM2+SUM1/TBAAA
     if (DEBUG)write(JOSTND,*)'IN SECOND SHADE ADJUSTMENT SUMS LOOP', &
          & ' (2451).',' NPTID=',NN,' BAAA=',BAAA(NNID),' TBAAA=',TBAAA, &
          & ' SUM1=',SUM1,' SUM2=',SUM2
     2451 continue
  end do
  if (DEBUG)write(JOSTND,*)'SUM1,SUM2= ',SUM1,SUM2

  !     BEGIN PROCESSING OF PLOTS                             HERE WE GO
  !     TRIPLE DO-LOOP TO DETERMINE REGENERATION RECORDS TO ADD TO EACH
  !     INVENTORY PLOT.
  !---->LOOP ACROSS BASE MODEL INVENTORY PLOTS (DO 203  NN=1,NPTIDS),
  !------>LOOP ACROSS SITE PREPARATION METHODS (DO 202 ITYPEP=1,4),
  !-------->LOOP ACROSS EACH REGENERATION PLOT (DO 201 IREP=1,NTIMES)

  NCOUNT = 0
  GENTIM = FINT-5.0
  if (GENTIM.lt.0.0) GENTIM = 0.0
  if (DEBUG)write(JOSTND,*)' FINT=',FINT,' GENTIM=',GENTIM

  !     DO LOOP ACROSS BASE MODEL INVENTORY PLOTS
  do NN = 1,NPTIDS
     if (DEBUG)write(JOSTND,*)'ENTERING LOOP ACROSS INVENTORY PLOTS', &
          & ' (203)',' PLOT ',NN,' OUT OF ',NPTIDS,' INVENTORY PLOTS.'

     !     NESTED DO LOOP TO INITIALIZE NNPREP (SITE PREPS)
     do I = 1,4
        NNPREP(I) = 0
        38 continue
     end do

     !     ISTART/IEND ARE THE START AND STOP REGENERATION PLOT NUMBER FOR
     !     EACH BASE MODEL INVENTORY PLOT
     ISTART = NN*IDUP -IDUP+1
     IEND = NN*IDUP
     if (DEBUG)write(JOSTND,*) ' ISTART=', ISTART,' NN=',NN,' IDUP=', &
          & IDUP,' IEND=',IEND

     !     NESTED DO LOOP TO SET SITE PREPARATION FOR EACH REGENERATION PLOT
     !     N IS THE REGENERATION PLOT SITE PREPARATION METHOD
     !     NNPREP IS SUM OF SITE PREPS BY SITE PREPARATION METHOD
     do I = ISTART,IEND
        N = IPPREP(I)
        if (N.lt.1.or.N.gt.4) N = 1
        NNPREP(N) = NNPREP(N)+1
        if (DEBUG)write(JOSTND,*) ' INSIDE LOOP FOR ASSIGNING SITE PREP', &
             & ' TO EACH REGEN PLOT (39)',' IPPREP(I)=',IPPREP(I),' N=',N, &
             & ' NNPREP(N)=',NNPREP(N)
        39 continue
     end do

     !     PROCESS REPLICATED PLOTS BY EACH OF THE 4 SITE PREPS
     !     NESTED DO LOOP ACROSS EACH SITE PREPARATION METHOD
     do ITYPEP = 1,4
        if (DEBUG)write(JOSTND,*)'ENTERING LOOP ACROSS SITE PREP METHODS', &
             & ' (202)',' ITYPEP ',ITYPEP,' OUT OF ',4,' ITYPEP.'
        IPOLD = 0
        !     IF SITE PREPARATION METHOD HAS 0 PLOTS, SKIP METHOD
        if (DEBUG)write(JOSTND,*) &
             & ' NUMBER OF PLOTS FOR SITE PREP METHOD', &
             & ' NNPREP(ITYPEP)=',NNPREP(ITYPEP)
        if (NNPREP(ITYPEP).lt.1) go to 202

        !     DOUBLE NESTED DO LOOP ACROSS EACH REGENERATION PLOT
        !     NTIMES IS THE NUMBER OF REGENERATION PLOTS IN SITE PREP METHOD
        !     IREP IS A SPECIFIC REGENERATION PLOT
        NTIMES = NNPREP(ITYPEP)
        do IREP = 1,NTIMES
           if (DEBUG)write(JOSTND,*)'ENTERING LOOP ACROSS REGEN PLOTS', &
                & ' (201)',' IREP ',IREP,' OUT OF ',NTIMES, ' REGEN PLOTS.'

           !     NCOUNT IS A COUNTER, NUMBER OF REGN PLOTS IN SITE PREP METHOD
           !     IPREP IS SITE PREPARATION METHOD
           NCOUNT = NCOUNT+1
           IPREP = ITYPEP
           if (DEBUG)write(JOSTND,*)' NCOUNT=',NCOUNT,' IPREP=',IPREP
           !     BYPASS RE-SETTING OF REGENERATION PLOT VARIABLES IF IN THE SAME
           !     SITE PREPARATION METHOD
           if (IPREP.eq.IPOLD) go to 137
           !     IPOLD IS SITE PREPARATION METHOD
           !     NNID IS BASE MODEL PLOT ID
           !     PROB1 IS A PROBABILITY WHICH IS RESET TO 1.0 EACH LOOP
           !     NEED TO CHECK IF THESE VARIABLES ARE STILL NEEDED AND IF
           !     NEW ONEWS ARE NEEDED
           !     RADIAN IS PLOT ASPECT IN RADIANS (DEFAULTS TO STAND)
           !     SLO IS PLOT SLOPE IN PERCENT (DEFAULTS TO STAND)
           !     IPHY IS PLOT PHYSIOGRAPHIC POSITION, DEFAULT IS MID-SLOPE
           !     XCOSAS IS COSINE OF PLOT ASPECT
           !     XSINAS IS SINE OF PLOT ASPEC
           !     XCOS IS XCOSAS*SLO INTERACTION
           !     XSIN IS XSINAS*SLO INTERACTION
           !     BAA IS PLOT BASAL AREA >= REGNBK
           !     BAASQ IS PLOT BASAL AREA SQUARED
           !     BAALN IS NATURAL LOG OF PLOT BASAL AREA
           !     BAAOLD IS INVENTORY PLOT BASAL AREA >= REGNBK
           !     PTA IS PLOT TREES PER ACRE >= REGNBK
           !     TPAOLD IS INVENTORY PLOT TREES PER ACRE >= REGNBK
           !     IHAB IS PLOT HABITAT TYPE GROUP (16 OPTIONS)
           !     ISER IS PLOT HABITAT TYPE GROUPS CODE (1-5)
           IPOLD = IPREP
           NNID = IPTIDS(NN)
           PROB1(NCOUNT) = 1.0
           RADIAN = PASP(NNID)
           SLO = PSLO(NNID)
           IPHY = IPHYS(NNID)
           XCOSAS = COS(RADIAN)
           XSINAS = SIN(RADIAN)
           XCOS = XCOSAS*SLO
           XSIN = XSINAS*SLO
           BAA = BAAA(NNID)
           if (BAA.lt.1.0) BAA = 1.0
           if (BAA.gt.400.0) BAA = 400.0
           BAASQ = BAA*BAA
           BAALN = ALOG(BAA)
           BAAOLD = BAAINV(NNID)
           PTA = PTPAA(NNID)
           TPAOLD = TPAAINV(NNID)
           if (BAAOLD.lt.1.0) BAAOLD = 1.0
           if (BAAOLD.gt.400.0) BAAOLD = 400.0
           IHAB = IPHAB(NNID)
           ISER = MYHABG(IHAB)
           if (DEBUG) write(JOSTND, *) ' PLOT VARIABLE DEBUG',' SLO=',SLO, &
                & ' ASPECT=',RADIAN,' TPA=',PTA,' TPAOLD=',TPAOLD,' BAA=',BAA, &
                & ' BAAOLD=',BAAOLD

           if (DEBUG) then
              write(JOSTND,6033) NNID,NOFSPE,IPREP,NTALLY,ISER,IFT, &
                   & SLO,BAA,ELEV,ASPECT
              6033 format(/,72('='),/,'PLOT=',I4,'  NOFSPE=',I4,'  IPREP=',I3, &
                   & '  NTALLY=',I3,'  ISER=',I3,/,'IFT=',I3, &
                   & '  SLO=',F5.2,'  BAA=',F7.2,'  ELEV=',F6.1,'  ASPECT=',F7.3)
              write(JOSTND,*) 'OCURFT = ',(OCURFT(I,IFT),I = 1,NOFSPE)
              write(JOSTND,*) 'XESMLT = ',(XESMLT(I),I = 1,NOFSPE)
           end if
           if (STOADJ.lt.0.0001) go to 137

           !     CALCULATE P(STOCKING) AT INVENTORY OR HARVEST, WHICHEVER CAME
           !     LAST. IF ADVANCE COMPONENT OF THE INVENTORY IS INVALID (INADV=1)
           !     OR THIS IS NOT THE FIRST TALLY OF REGEN, BYPASS PROCEDURES TO
           !     CORRECT ACTUAL (INVENTORY) VS. PREDICTED (MODEL) INTERCEPT.
           !     FTEMP IS YEAR OF INVENTORY
           !     TIME IS TIME SINCE SITE PREP
           !     IEVTYR IS INTEGER OF YEAR OF SITE PREP
           if (INADV.eq.1.or.NTALLY.ne.1) go to 241
           FTEMP = real(IY(1))
           if (FTEMP.lt.ZHARV) FTEMP = ZHARV
           if (DEBUG)write(JOSTND,*) ' FTEMP=',FTEMP,' ZHARV=',ZHARV
           if (IPREP.eq.2) then
              TIME = FTEMP -ZMECH
              IEVTYR = INT(ZMECH)
              if (DEBUG)write(JOSTND,*) ' IN TIME SET FOR MECH SITE PREP.', &
                   & ' TIME=',TIME,' FTEMP=',FTEMP,' ZMECH=',ZMECH,' IEVTYR=', &
                   & IEVTYR
           else if (IPREP.eq.3) then
              TIME = FTEMP -ZBURN
              IEVTYR = INT(ZBURN)
              if (DEBUG)write(JOSTND,*) ' IN TIME SET FOR BURN SITE PREP.', &
                   & ' TIME=',TIME,' FTEMP=',FTEMP,' ZBURN=',ZBURN,' IEVTYR=', &
                   & IEVTYR
           else
              TIME = FTEMP  -ZHARV
              IEVTYR = IDSDAT
              if (DEBUG)write(JOSTND,*) ' IN TIME SET FOR HARV SITE PREP.', &
                   & ' TIME=',TIME,' FTEMP=',FTEMP,' ZHARV=',ZHARV,' IEVTYR=', &
                   & IEVTYR
           end if
           if (TIME.lt.0.0) TIME = 0.0

           !     DEFINE WSBW VARIABLES AT TIME OF INVENTORY OR HARVEST.
           !     ITEMP IS # YEARS FROM LAST DISTURBANCE TO INVENTORY.
           ITIME = INT(TIME+0.5)
           if (DEBUG) write (JOSTND,7010) TIME,IEVTYR
           7010 format('TIME=',F7.1,'; IEVTYR=',I5)
           call ESTIME
           if (DEBUG) write (JOSTND,7020) BWB4,BWAF,REGT,SQREGT,SQBWAF
           7020 format('BWB4=',F7.3,'  BWAF=',F7.3,'  REGT=',F7.3,'  SQREGT=', &
                & F7.3,'  SQBWAF=',F7.3)

           !     CALL ESTOCK TO COMPUTE PROBABILITY OF STOCKING
           !     FOR INITIAL INVENTORY
           !     ESTOCK NEEDS:
           !       TLAT = STAND LATTITUDE, IN PLOT.F77
           !       ELEV = STAND ELEVATION IN 100S OF FEET, IN PLOT.F77
           !       IFT = FOREST TYPE GROUP (1-14)
           !       TPAOLD = PLOT TPA >= REGNBK
           !     PN/ESB1 IS AN INTERMEDIATE VARIABLE USED IN DETERMINING THE
           !       PROBABILITY OF STOCKING OF PLOTS
           !     FTEMP IS THE PREDICTED PROPORTION OF INVENTORY PLOTS THAT
           !       ARE STOCKED
           if (DEBUG)write(JOSTND,*)' CALLING ESTOCK FOR INITIAL INVENTORY'
           call ESTOCK (ELEV,IFT,BAAOLD,TPAOLD,PN,DEBUG)
           ESB1(NCOUNT) = PN
           if (DEBUG) write (JOSTND,6031) PN
           6031 format('PN FOR STOCKING= ',F10.4)
           FTEMP = EXP(PN)/(1+EXP(PN))
           !     ITPP IS INVENTORY TREES PER PLOT
           ITPP = INT((PLPROB(NNID)*DUPNPT)/(FTEMP*EXPAND) +0.5)
           if (DEBUG)write(JOSTND,*) ' VARIABLES USED FOR ITPP CALCULATION', &
                & ' (INVENTORY TREES PER PLOT).', &
                & ' PLPROB(NNID)=',PLPROB(NNID),' DUPNPT=',DUPNPT, &
                & ' FTEMP=',FTEMP,' EXPAND=',EXPAND
           if (DEBUG) write(JOSTND,7021) ITPP
           7021 format('CALCULATION OF ITPP AT INVENTORY=',I10)

           !     NSTORE IS ESTIMATED INVENTORY TREES PER PLOT, EXPANDED ACROSS
           !     EACH SITE PREP PLOT (NTIMES). NSTORE SHOULD BE 0, ONCE REGENERATION
           !     SEQUENCES DO NOT COINCIDE WITH INVENTORY PERIOD.
           !     PNN IS INVENTORY PROBABILITY OF STOCKING
           NSTORE(NCOUNT) = ITPP
           PNN(NCOUNT) = ESA
           if (NTIMES.gt.1) then
              do I = 1,NTIMES-1
                 NSTORE(NCOUNT+I) = ITPP
                 PNN(NCOUNT+I) = ESA
                 if (DEBUG)write(JOSTND,*) ' NSTORE(NCOUNT+I)',NSTORE(NCOUNT+I), &
                      & ' PNN(NCOUNT+I)',PNN(NCOUNT+I)
                 31 continue
              end do
           end if
           241 continue

           ! STEP 3: COMPUTE YEARS SINCE LAST DISTURBANCE

           if (DEBUG) write(JOSTND,9993)
           9993 format(/,'STEP 3: COMPUTE YEARS SINCE LAST DISTURBANCE BASED ', &
                & 'ON TIME SINCE LAST SITE PREP.')

           !     TIME IS FROM SITE PREP TO END OF CYCLE
           if (DEBUG) write(JOSTND,*)'DISTURBANCE TYPE, IPREP:', IPREP
           if (IPREP.eq.2) then
              IEVTYR = INT(ZMECH)
              TIME = FLOKDT +1.0 -ZMECH
              if (DEBUG) write(JOSTND,*)'TIME SET TO TALLY YEAR(+1) - ', &
                   & 'MECHPREP YEAR.'
              if (DEBUG) write(JOSTND,*)'IPREP:',IPREP,'TIME: ', TIME, &
                   & 'FLOKDT:',FLOKDT,'ZMECH:',ZMECH,'IEVTYR:', IEVTYR
           else if (IPREP.eq.3) then
              IEVTYR = INT(ZBURN)
              TIME = FLOKDT +1.0 -ZBURN
              if (DEBUG) write(JOSTND,*)'TIME SET TO TALLY YEAR(+1) - ', &
                   & 'BURNPREP YEAR.'
              if (DEBUG) write(JOSTND,*)'IPREP:',IPREP,'TIME: ', TIME, &
                   & 'FLOKDT:',FLOKDT,'ZBURN:',ZBURN,'IEVTYR:', IEVTYR
           else
              IEVTYR = IDSDAT
              TIME = FLOKDT +1.0 -ZHARV
              if (DEBUG) write(JOSTND,*)'TIME SET TO TALLY YEAR(+1) - ', &
                   & 'HARVEST YEAR.'
              if (DEBUG) write(JOSTND,*)'IPREP:',IPREP,'TIME: ', TIME, &
                   & 'FLOKDT:',FLOKDT,'ZHARV:',ZHARV,'IEVTYR:', IEVTYR
           end if
           call ESTIME
           if (DEBUG) write(JOSTND,*)'SET REGT AND SQREGT TO BE ', &
                & 'BASED ON TIME WITHOUT BUDWORM ADJUSTMENT.'
           if (DEBUG) write(JOSTND,*)' REGT:',REGT, &
                & 'SQREGT:',SQREGT

           ! STEP 4: CALCULATE INCREMENT IN STOCKING AND SPECIES PROBABILITIES

           if (DEBUG) write(JOSTND,9994)
           9994 format(/,'STEP 4: CALCULATE INCREMENT IN STOCKING AND SPECIES ', &
                & 'PROBABILITIES.')

           !     CALCULATE P(STOCKING) AT THE END OF THE CYCLE
           if (DEBUG)write(JOSTND,*)' CALLING ESTOCK AT END OF CYCLE'
           call ESTOCK (ELEV,IFT,BAA,PTA,PN,DEBUG)
           if (DEBUG) write (JOSTND,6031) PN

           !     CORRECT FOR ACTUAL VS. PREDICTED REGENERATION. ESB IS FROM THE
           !     INVENTORY. ESB1 IS PREDICTED AT INVENTORY. THEIR DIFFERENCE IS
           !     USED TO ADJUST PROBABILITY OF STOCKING, ULTIMATELY PROB1(PLOT)
           if (STOADJ.lt.0.001) STOADJ = 0.001
           FTEMP = 1.0/(1.0+EXP(-(PN+ESB-ESB1(NCOUNT) )))
           FTEMP = FTEMP *STOADJ
           if (DEBUG) write (JOSTND,*)' CALCULATION OF FINAL PROBABILITY', &
                & ' OF STOCKING (FTEMP).',' FTEMP=',FTEMP,' STOADJ', STOADJ, &
                & ' ESB=',ESB,' ESB1=',ESB1(NCOUNT),' NCOUNT=',NCOUNT
           if (FTEMP.lt.0.0001) FTEMP = 0.0001
           if (FTEMP.gt.0.9990) FTEMP = 0.9990
           if (FTEMP.lt.PNN(NCOUNT)) FTEMP = PNN(NCOUNT) +0.0001
           if (DEBUG) write (JOSTND,*) ' FTEMP AFTER ADJUSTMENTS', FTEMP
           PROB1(NCOUNT) = FTEMP
           if (DEBUG) write (JOSTND,6028) ESB,ESB1(NCOUNT),TPACRE,FTEMP
           6028 format('ESB(ACT)=',F7.4,'  ESB1(PRED)=',F7.4,'  TPACRE=',F7.1, &
                & '      PROB1=',F7.4)

           !     ITPP OF INVENTORY IS CALCULTED FOR CURRENT INVENTORY PLOT (NNID). ITPP
           !     IS NUMBER OF RECORDS LESS THAN 1 INCH IN DBH. ITPP IS THEN STORED IN
           !     NSTORE ARRAY. THIS CALCULATION OCCURS IN INGROWTH SEQUENCE OR IF
           !     MATCH IS 1.
           if (DEBUG)write(JOSTND,*)' CHECKING IF INGRO OR A MATCH.', &
                & ' INGRO=',INGRO,' MATCH=',MATCH
           if (INGRO.eq.1.or.MATCH.eq.1) then
              ITPP = INT((PLPROB(NNID)*DUPNPT)/(FTEMP*EXPAND) +0.5)
              NSTORE(NCOUNT) = ITPP
              if (DEBUG) write (JOSTND,*) ' INGROWTH OR MATCH OCCURED.', &
                   & ' ITPP=', ITPP,' NNID=',NNID,' PLPROB(NNID)=',PLPROB(NNID), &
                   & ' DUPNPT=',DUPNPT,' FTEMP=',FTEMP,' EXPAND=',EXPAND,' ITPP=', &
                   & ITPP
           end if

           !     LOOP ACROSS ALL REGEN PLOT REPLICATES (NTIMES) FOR INVENTORY PLOT (NNID)
           !     STORE PREVIOUSLY DETERMINED IVNENTORY ITPP WITH EACH PLOT REPLICATE.
           !     STORE PROBABILITY OF STOCKING (FTEMP) WITH EACH PLOT REPLICATE.
           !     STORE ESB1 WITH EACH PLOT REPLICATE
           if (NTIMES.gt.1) then
              do I = 1,NTIMES-1
                 PROB1(NCOUNT+I) = FTEMP
                 ESB1(NCOUNT+I) = ESB1(NCOUNT)
                 if (INGRO.eq.1.or.MATCH.eq.1) NSTORE(NCOUNT+I) = ITPP
                 if (DEBUG) write (JOSTND,*) ' LOOPING BY NTIMES (DO LOOP 30).', &
                      & ' I=',I,' FTEMP=',FTEMP,' NCOUNT=',NCOUNT,' ESB1(NCOUNT)=', &
                      & ESB1(NCOUNT),' INGRO=',INGRO,' MATCH=',MATCH, ' ITPP=', &
                      & ITPP
                 30 continue
              end do
           end if
           !     STATEMENT 137 IS USED WHEN BYPASSING PLOT REPS
           137 continue

           !     CALCULATE PROB OF ADV, SUBS, AND EXCESS SPECIES AT 10 YEARS.
           TIME = 10.0
           if (INGRO.eq.1) TIME = SHORTY
           ITIME = INT(TIME+0.5)
           if (DEBUG) write (JOSTND,*) ' BEFORE CALL TO ESTIME.', &
                & ' IDSDAT=',IDSDAT,' ITIME=',ITIME, ' INGRO=',INGRO
           call ESTIME
           if (DEBUG) write (JOSTND,*) ' AFTER CALL TO ESTIME.', &
                & ' IDSDAT=',IDSDAT,' ITIME=',ITIME
           !     INITIALIZE PROBABILITY OF ADVANCED (PADV), SUBSEQUENT(PSUB), AND
           !     EXCESS (PXCS) REGENERATION BY SPECIES
           do I = 1,NOFSPE
              PADV(I) = 0.0
              PSUB(I) = 0.0
              PXCS(I) = 0.0
              115 continue
           end do

           !    CALCULATE PROBABILITY OF ADVANCED, SUBSUQENT, AND EXCESS REGEN FOR ALL
           !    SPECIES WITH ESPADV, ESPSUB, AND ESPXCS CALLS.
           call ESPADV (IFT)
           if (ITIME.gt.2) call ESPSUB (IFT)
           call ESPXCS (IFT)
           if (DEBUG) then
              write(JOSTND,7010) TIME
              write(JOSTND,7020) BWB4,BWAF,REGT,SQREGT,SQBWAF
              write(JOSTND,7002) (PADV(I),I = 1,NOFSPE)
              7002 format('PADV=',23F6.3)
              write(JOSTND,7003) (PSUB(I),I = 1,NOFSPE)
              7003 format('PSUB=',23F6.3)
              write(JOSTND,7005) (PXCS(I),I = 1,NOFSPE)
              7005 format('PXCS=',23F6.3)
           end if

           !     RESET TIME TO YEARS SINCE DISTURBANCE OR SITE PREP.
           if (IPREP.eq.2) then
              IEVTYR = INT(ZMECH)
              TIME = FLOKDT +1.0 -ZMECH
              if (DEBUG)write(JOSTND,*) ' IN TIME SET FOR MECH SITE PREP.', &
                   & ' TIME=',TIME,' ZMECH=',ZMECH,' IEVTYR=', &
                   & IEVTYR
           else if (IPREP.eq.3) then
              IEVTYR = INT(ZBURN)
              TIME = FLOKDT +1.0 -ZBURN
              if (DEBUG)write(JOSTND,*) ' IN TIME SET FOR BURN SITE PREP.', &
                   & ' TIME=',TIME,' ZBURN=',ZBURN,' IEVTYR=', &
                   & IEVTYR
           else
              IEVTYR = IDSDAT
              TIME = FLOKDT + 1.0 - ZHARV
              if (DEBUG)write(JOSTND,*) ' IN TIME SET FOR HARV SITE PREP.', &
                   & ' TIME=',TIME,' ZHARV=',ZHARV,' IEVTYR=', &
                   & IEVTYR
           end if
           if (DEBUG) write (JOSTND,7010) TIME,IEVTYR
           call ESTIME
           if (DEBUG) write (JOSTND,7020) BWB4,BWAF,REGT,SQREGT,SQBWAF

           !     ASSIGN A RANDOM NUMBER TO EMSQR TO BE USED LATER IN ASSIGNING
           !     RANDOM VARIATION TO TREES ON DIFFERENT PLOTS
           call ESRANN (DRAW)
           EMSQR = 1.0
           if (DRAW.lt.0.5) EMSQR = -1.0
           call ESRANN (DRAW)
           EMSQR = EMSQR*DRAW
           if (DEBUG) write (JOSTND,7015) EMSQR
           7015 format('RANDOM NUMBER DRAW, EMSQR=',F10.3)

           !     IF STOCK ADJUSTEMENT IS SET TO 0, CALL RANDOM NUMBER DRAWS TO KEEP
           !     RANDOM VARIATION APPLIED TO PLANTED TREES THE SAME.
           if (STOADJ.lt.0.0001) then
              call ESRANN (DRAW)
              do I = 1,6
                 call ESRANN (DRAW)
                 71 continue
              end do
              do I = 1,6
                 call ESRANN (DRAW)
                 72 continue
              end do
              do I = 1,NOFSPE
                 call ESRANN (DRAW)
                 73 continue
              end do
              do I = 1,NOFSPE*2
                 call ESRANN (DRAW)
                 74 continue
              end do
              do I = 1,MAXTPP(IFT)*2
                 call ESRANN (DRAW)
                 75 continue
              end do
              go to 163
           end if

           ! STEP 5: ESTIMATE THE NUMBER OF NATURAL REGENERATING TREES PER PLOT

           if (DEBUG) write(JOSTND,9995)
           9995 format(/,'STEP 5: ESTIMATE THE NUMBER OF NATURAL REGENERATING ', &
                & 'TREES PER PLOT (TPP).')
           call ESRANN (DRAW)
           if (NTALLY.eq.1) XSTORE(NCOUNT) = DRAW
           DRAW = XSTORE(NCOUNT)
           if (DEBUG) write (JOSTND,*)'RANDOM DRAW FOR TPP AT START OF ', &
                & 'TALLY SEQUENCE. DRAW:', DRAW

           !     CALL ESTPP TO CALCULATE NUMBER OF NATURAL REGENERATING TREES PER PLOT
           call ESTPP (DRAW,IFT,DEBUG,JOSTND,TPP)
           ITPP = INT(TPP + 0.5)
           if (ITPP.lt.1) ITPP = 1

           !     CHECK ITPP AGAINST ALLOWED FOREST TYPE MAXIMUMS FOR TALLY SEQUENCE
           if (ITPP.gt.MAXTPP(IFT)) ITPP = MAXTPP(IFT)

           !     CHECK ITPP AGAINST ALLOWED FOREST TYPE MAXIMUMS FOR INGROWTH SEQUENCE
           if (INGRO.eq.1.and.ITPP.gt.MAXING(IFT)) ITPP = MAXING(IFT)

           if (DEBUG) write (JOSTND,*)'NUMBER OF TREES PER PLOT, ITPP:',ITPP
           if (DEBUG) write (JOSTND,*)'ITPP MUST BE <= TALLY FOREST TYPE ', &
                & 'MAXIMUM OF',MAXTPP(IFT)
           if (DEBUG) write (JOSTND,*)'IF INGROWTH, MUST BE <= ', &
                & 'INGROWTH FOREST TYPE MAXIMUM OF',MAXING(IFT)

           !     NSTORE (NCOUNT) TRACKS THE TOTAL TREES TO ESTABLISH BY PLOT
           !     NEWTPP TRACKS ONLY NEW TREES TO ESTABLISH ON PLOT
           NEWTPP = ITPP-NSTORE(NCOUNT)
           if (NEWTPP.lt.0) NEWTPP = 0
           NSTORE(NCOUNT) = ITPP
           TPP = real(ITPP)
           if (DEBUG) write (JOSTND,7009) NEWTPP,ITPP,PLPROB(NNID)
           7009 format(' NEW TREES/PLOT=',I5,' TOTAL TREES/PLOT=',I5, &
                & ' PLOT LEVEL TPA (PLPROB(NNID))=',F8.3)

           ! STEP 6: ESTIMATE THE NUMBER OF NATURAL REGENERATING SPECIES ON PLOT

           if (DEBUG) write(JOSTND,9996)
           9996 format(/,'STEP 6: ESTIMATE THE NUMBER OF NATURAL REGENERATING ', &
                & 'SPECIES PER PLOT (NUMSPE).')
           if (DEBUG) write (JOSTND,7010) TIME

           !     PSPE IS THE PROPORTION OF PLOTS HAVING 1-6 SPECIES.
           do I = 1,6
              PSPE(I) = 0.0
              call ESRANN (DRAW)
              WK6(I) = DRAW
              15 continue
           end do
           NUMSPE = 1
           if (ITPP.eq.1) go to 19

           !     CALL ESNSPE TO ESTIMATE THE PROBABILITIES OF 1-6 SPECIES OCCUPYING A PLOT.
           !     THIS SUBROUTINE WILL POPULATE PSPE ARRAY.
           call ESNSPE (IFT,PSPE,ITPP,DEBUG,JOSTND)
           !     DO LOOP 16:SUM PROBABILITES OF 1-6 SPECIES OCCURING ON PLOT
           !     SUM0 IS THE SUM OF THE SPECIES PROPORTIONS
           SUM0 = PSPE(1)
           do I = 2,6
              SUM0 = SUM0+PSPE(I)
              if (DEBUG)write(JOSTND,*)' AFTER ESNSPE CALL.', ' PSPE=',PSPE(I), &
                   & ' SUM0=',SUM0
              16 continue
           end do

           !     DO LOOP 17: ADJUST SO SUM OF PROBABILITIES EQUAL 1.0 FOR USE WITH
           !     RANDOM NUMBER DRAW. SUMUP IS FIRST NUMBER OF SPECIES PROPORTIONS ON
           !     A 0-1 SCALE AND THEN BECOMES AN ARRAY CONTAINING CUMULATIVE SUM OF
           !     NUMBER OF SPECIES PROPORTIONS
           SUMUP(1) = PSPE(1)/SUM0
           do I = 2,5
              SUMUP(I) = SUMUP(I-1) + PSPE(I)/SUM0
              if (DEBUG)write(JOSTND,*)' SPECIES PROPORTION ON 0-1 SCALE (17).', &
                   & ' PSPE=',PSPE(I),' SUM0=',SUM0
              17 continue
           end do

           !     DO LOOP 18: DETERMINE NUMSPE (NUMBER OF SPECIES OCCURING ON PLOT)
           !     NUMSPE IS DETERMINED WHEN SUMUP(I) IS GREATER THEN DRAW.
           do I = 1,5
              DRAW = WK6(I)
              if (DEBUG)write(JOSTND,*)' COMPARE DRAW VERSUS SUMUP VALUE (18).', &
                   & ' DRAW=',DRAW,' SUMUP(I)=',SUMUP(I)
              if (DRAW.gt.SUMUP(I)) go to 18
              NUMSPE = I
              go to 19
              18 continue
           end do
           NUMSPE = 6
           19 continue
           !     CHECK NUMSPE AGAINST ALLOWED FOREST TYPE MAXIMUMS
           if (NUMSPE.gt.MAXSPP(IFT)) NUMSPE = MAXSPP(IFT)
           if (DEBUG) write (JOSTND,7001) (PSPE(I),I = 1,6),NUMSPE
           7001 format('P(1-6 SPECIES)',6F6.3,'; #SPECIES CHOSEN=',I3)

           !     COMPARE NUMBER OF SPECIES TO NUMBER OF SPECIES WITH PROBABILITES
           !     OF OCCURANCE OF ADVANCED (PADV) AND SUBSEQUENT (PSUB)
           !     REGENERATION (NSPNZ). IF NSPNZ LESS THAN NUMSPE, THEN
           !     SET VALUE OF NUMSPE TO NSPNZ. THIS CHECK ENSURES THAT NUMBER OF
           !     SPECIES WITH PROBABILITY OF REGEN VALUES IS AT LEAST EQUIVALENT
           !     TO NUMBER OF SPECIES PREDICTED TO OCCUR ON PLOT.
           SUM0 = 0.0
           NSPNZ = 0
           do I = 1,NOFSPE
              IBEST(I) = 0
              FTEMP = PADV(I)+PSUB(I)
              if (FTEMP.gt.0.0001) NSPNZ = NSPNZ+1
              SUM0 = SUM0+FTEMP
              SUMUP(I) = FTEMP
              if (DEBUG)write(JOSTND,*)' COMPARE NUMBER OF SPECIES TO NUMBER OF', &
                   & ' SPECIES WITH PROBS OF ADV AND SUB (56).',' PADV=', PADV(I), &
                   & ' PSUB=',PSUB(I),' FTEMP=',FTEMP,' NSPNZ=',NSPNZ,' SUM0=',SUM0, &
                   & ' SUMUP=',SUMUP(I)
              56 continue
           end do

           !     LOOP 57: CALCULATE ADJUSTED SPECIES PROBABILITY OF OCCURENCE VALUES
           !     (FROM PADV AND PSUB). THESE WILL BE USED INTIALLY IN STEP 7.
           do I = 1,NOFSPE
              if (SUM0.lt.0.0001)go to 57
              SUMUP(I) = SUMUP(I)/SUM0
              if (DEBUG)write(JOSTND,*)'IN DO LOOP 57 ADJUSTING SUMUP SO VALUES', &
                   & ' SUM TO 1.',' I=',I,' SUMUP=',SUMUP(I),' SUM0=',SUM0
              57 continue
           end do
           if (DEBUG) write (JOSTND,7003) (SUMUP(I),I = 1,NOFSPE)
           if (NUMSPE.gt.NSPNZ) NUMSPE = NSPNZ
           if (DEBUG)write(JOSTND,*)' NUMSPE=',NUMSPE,' NSPNZ=',NSPNZ

           ! STEP 7: PICK WHICH SPECIES WILL OCCUPY THE PLOT

           if (DEBUG) write(JOSTND,9997)
           9997 format(/,'STEP 7: PICK WHICH SPECIES WILL OCCUPY THE PLOT.')
           if (DEBUG) write(JOSTND,*)'SELECT RANDOM NUMBERS FOR UP TO 6 ', &
                & 'SPECIES. LOAD INTO WK6 ARRAY.'

           !     INITIALIZE RANDOM DRAWS THAT WILL BE USED TO DETERMINE WHICH SPECIES
           !     OCCUPY PLOT.
           do I = 1,6
              call ESRANN (DRAW)
              WK6(I) = DRAW
              if (DEBUG) write(JOSTND,*)' WK6(', I,'):', WK6(I)
              120 continue
           end do

           !     OUTER LOOP FOR IDENTIFYING "BEST SPECIES". LOOP ITERATES ACROSS NUMBER
           !     OF SPECIES DETERMINED TO OCCUR ON PLOT (NUMSPE). ANY SPECIES THAT IS
           !     SELECTED TO OCCUR ON A REGEN PLOT IS A "BEST SPECIES" (IBEST FLAG FOR SPECIES
           !     WILL BE SET TO 1).
           if (DEBUG) write(JOSTND,*)'IDENTIFY UP TO',NUMSPE,'BEST SPECIES.'
           do I = 1,NUMSPE
              SUM0 = 0.0
              DRAW = WK6(I)
              if (DEBUG) write(JOSTND,*)' I:',I,'RANDOM DRAW:',DRAW
              if (DEBUG) write(JOSTND,*)'CHOOSE SPECIES BY SUMMING UP SPECIES ', &
                   & 'PROBABILITIES AND SELECTING SPECIES '
              if (DEBUG) write(JOSTND,*)'WHEN TOTAL ELAPSES RANDOM DRAW.'
              !     59 LOOP IS WHERE SUM0 IS WHERE SPECIES ARE SELECTED. LOOP ITERATES
              !     ACROSS NUMBER OF SPECIES IN VARIANT (NOFSPE) - 1.
              do J = 1,NOFSPE-1
                 SUM0 = SUM0+SUMUP(J)
                 if (DEBUG) write(JOSTND,*)' SPECIES:',J,'SUMUP(J)',SUMUP(J), &
                      & ' SUM0:',SUM0
                 !     COMPARE DRAW TO SUM0 VALUE. SUM0 IS A CUMULATIVE SUM OF SPECIES
                 !     PROBABILITY OF OCCURENCE VALUES. TWO POSSIBLE OUTCOMES.
                 !     1) IF SUM0 LESS THAN DRAW MOVE TO STMT 59 AND CONTINUE LOOKING
                 !        FOR SPECIES TO SELECT.
                 !     2) ELSE DRAW IS LESS THAN SUM0 AND SPECIES (J) IS SELECTED TO OCCUR
                 !        ON PLOT. SUMUP(J) IS ZEROED OUT TO ENSURE SPECIES J IS NOT SELECTED
                 !        AGAIN. BEST SPECIES FLAG FOR SPECIES J IS SET TO 1. MOVE TO STMT
                 !        58.
                 if (DRAW.gt.SUM0) go to 59
                 SUMUP(J) = 0.0
                 IBEST(J) = 1
                 if (DEBUG) write(JOSTND,*)'SELECTED SPECIES:',J,'SUMUP RESET:', &
                      & SUMUP(J),'BEST TREE FLAG SET TO:', IBEST(J)
                 go to 58
                 59 continue
              end do
              !     SITUATION WHERE NO SPECIES UP TO NOFSPE - 1 IN VARIANT ARE SELECTED
              !     TO REGENERATE. LAST SPECIES IN VARIANT (NOFPSE) IS THEN SELECTED.
              IBEST(NOFSPE) = 1
              SUMUP(NOFSPE) = 0.0
              if (DEBUG) write(JOSTND,*)'NO SPECIES SELECTED, LAST SPECIES ', &
                   & 'SELECTED. ',NOFSPE,'IBEST(NOFSPE):',IBEST(NOFSPE), &
                   & 'SUMUP(NOFSPE):',SUMUP(NOFSPE)
              58 continue
              !     47 LOOP SPECIES WAS SELECTED AND NOW SUM OF SPECIES PROBABILITES MUST
              !     BE RECULCATED (SUM0). THIS SUM DEALS WITH UNADJUSTED SPECIES
              !     PROBABILITES AND DOES NOT INCLUDE PROBABILITY OF SPECIES THAT HAVE
              !     ALREADY BEEN SELECTED.
              SUM0 = 0.0
              do N = 1,NOFSPE
                 SUM0 = SUM0+SUMUP(N)
                 47 continue
              end do
              if (DEBUG) write(JOSTND,*)'RECALCULATE SUM OF SPECIES PROBALITY ', &
                   & 'WITHOUT SELECTED SPECIES INCLUDED.'
              if (DEBUG) write(JOSTND,*)' NEW SUM (SUM(0)) IS:',SUM0
              if (SUM0.lt.0.0001) go to 60
              !     LOOP 61; CALCULATE ADJUSTED SPECIES PROBABILITES SO THEY SUM TO 1.
              !     THE VALUES IN SUMUP WILL NOW BE USED TO SELECT ANOTHER SPECIES TO
              !     OCCUR ON PLOT IN LOOP 59.
              if (DEBUG) write(JOSTND,*)'ADJUSTED SPECIES PROBABILITIES.'
              do N = 1,NOFSPE
                 SUMUP(N) = SUMUP(N)/SUM0
                 if (DEBUG) write(JOSTND,*)' N:',N, 'SUMUP(N):',SUMUP(N)
                 61 continue
              end do
              60 continue
           end do
           if (DEBUG) write (JOSTND,7004) (IBEST(I),I = 1,NOFSPE)
           7004 format(' ARRAY IBEST ',23I5)

           !     DETERMINE ADV/SUBS STATUS OF THE FIRST TREE OF EACH SPECIES.
           if (DEBUG) write(JOSTND,*)'DETERMINE SPECIES PROBABILITIES.'
           do I = 1,NOFSPE
              PADV(I) = 0.0
              PSUB(I) = 0.0
              53 continue
           end do
           if (DEBUG) write(JOSTND,*)'NTALLY:',NTALLY
           if (NTALLY.eq.1) call ESPADV (IFT)
           call ESPSUB (IFT)
           if (DEBUG) write(JOSTND,*)' PADV(I):',PADV
           if (DEBUG) write(JOSTND,*)' PSUB(I):',PSUB
           if (DEBUG) write(JOSTND,*)'ICHOI IS A SPECIES ARRAY WHERE IF ', &
                & 'IDENTIFIED AS BEST TREE AND A RANDOM DRAW IS GREATER'
           if (DEBUG) write(JOSTND,*)'THAN THE PROBABILITY OF THE SPECIES ', &
                & 'ADVANCED PROBABILITY (PADV/(PADV+PSUB), ICHOI(2)=1,'
           if (DEBUG) write(JOSTND,*)'ELSE ICHOI(1)=1. IF NOT ADJUSTED, ', &
                & 'ICHOI VALUES ARE 0.'
           !     YTBD: IF 1, ICHOI(1) IS ADV, ICHOI(2) IS SUB.
           do I = 1,2
              do N = 1,NOFSPE
                 ICHOI(I,N) = 0
              end do
           end do
           !     DETERMINE WHETHER SPECIES SELECTED ABOVE ARE ADVANCED OR
           !     SUBSEQUENT REGENERATION.
           !     ADVANCED REGEN: ICHOI(1, I) = 1
           !     SUBSEQUENT REGEN: ICHOI(2, I) = 1
           !     I IS SPECIES INDEX
           do I = 1,NOFSPE
              call ESRANN (DRAW)
              if (IBEST(I).ne.1) go to 63
              SUM0 = PADV(I)+PSUB(I)
              if (SUM0.lt.0.0001)go to 63
              FTEMP = PADV(I)/SUM0
              J = 1
              if (DRAW.gt.FTEMP) J = 2
              ICHOI(J,I) = 1
              if (DEBUG) write(JOSTND,*)' DRAW:',DRAW,'SPECIES:',I,'ICHOI(1,I):', &
                   & ICHOI(1,I),'ICHOI(2,I):',ICHOI(2,I)
              63 continue
           end do

           ! STEP 8: ESTIMATE HEIGHTS AND ASSIGN SPECIES TO REGENERATION

           if (DEBUG) write(JOSTND,9998)
           9998 format(/,'STEP 8: ESTIMATE HEIGHTS AND ASSIGN SPECIES TO', &
                & ' REGENERATION**')

           !     DIFFERENCE BETWEEN TALLY DATES
           TIME = FLOKDT-real(KDTOLD)
           if (DEBUG)write(JOSTND,*)' FLOKDT=',FLOKDT,' KDTOLD=',KDTOLD
           IEVTYR = KDTOLD+1
           if (DEBUG) write (JOSTND,7010) TIME,IEVTYR

           !     CALL ESTIME TO COMPUTE BUDWORM TIME VARIABLES
           call ESTIME
           if (DEBUG) write (JOSTND,7020) BWB4,BWAF,REGT,SQREGT,SQBWAF

           !     DO TWO RANDOM NUMBER DRAWS PER SPECIES
           do I = 1,NOFSPE*2
              call ESRANN (DRAW)
              WK6(I) = DRAW
              122 continue
           end do
           NDRAW = 0

           !     DO LOOP ACROSS ALL SPECIES TO PREDICT HEIGHT OF ADVANCE AND
           !     SUBSEQUENT REGENERATION.
           !     ADVANCED REGEN: ICHOI(1, I) = 1; I IS SPECIES INDEX
           !     SUBSEQUENT REGEN: ICHOI(2, I) = 1; I IS SPECIES INDEX
           !     POPULATION OF ICHOI ARRAY IS DONE IN DO LOOP 63
           do I = 1,NOFSPE
              NDRAW = NDRAW+1
              TALL(I) = 0.001
              STOMLT(I) = 1.0
              !     IF NOT ADVANCED THEN GO TO 95
              if (ICHOI(1,I).ne.1) go to 95
              DRAW = WK6(NDRAW)
              !     GET GERMINATION DELAY TIME 1-10 YEARS
              call ESDLAY (I,1,DRAW,DELAY)
              !     DILATE IS SET TO 0.1
              DILATE = FIRST(1,I)
              TRAGE = 0.0
              !     ESADVH CALCULATES HEIGHT (HHT) OF ADVANCED REGEN
              call ESADVH (I,HHT,DELAY,GENTIM,TRAGE,JOSTND,DEBUG)
              if (DEBUG) write (JOSTND,7007) DELAY,I,GENTIM,TRAGE
              7007 format('DELAY TO GERM=',F10.4,'  SPECIES=',I5, &
                   & '  GENTIM=',F5.2,'  TRAGE=',F5.1)
              TALL(I) = HHT
              AGADSB(I) = TRAGE
              FTEMP = TRAGE
              if (FTEMP.gt.GENTIM) FTEMP = GENTIM
              STOMLT(I) = FTEMP/(GENTIM+0.0001)
              FIRST(1,I) = SQRT(DILATE)
              if (DEBUG)write(JOSTND,*)' AFTER ESADVH IN STEP 8.',' I=',I, &
                   & ' HTT=',HHT,' TRAGE=',TRAGE,' GENTIM=',GENTIM,' FTEMP=',FTEMP, &
                   & ' STOMLT=',STOMLT(I),' DILATE=',DILATE
              95 continue
              !     IF NOT SUBSEQUENT THEN GO TO 99
              if (ICHOI(2,I).ne.1) go to 99
              NDRAW = NDRAW+1
              DRAW = WK6(NDRAW)
              !     GET GERMINATION DELAY TIME 1-10 YEARS
              call ESDLAY (I,2,DRAW,DELAY)
              !     DILATE IS SET TO 0.1
              DILATE = FIRST(2,I)
              TRAGE = 0.0
              !     ESSUBH CALCULATES HEIGHT (HHT) OF SUBSEQUENT REGEN
              call ESSUBH (I,HHT,DELAY,GENTIM,TRAGE,JOSTND,DEBUG)
              if (DEBUG) write (JOSTND,7007) DELAY,I,GENTIM,TRAGE
              TALL(I) = HHT
              AGADSB(I) = TRAGE
              FTEMP = TRAGE
              if (FTEMP.gt.GENTIM) FTEMP = GENTIM
              STOMLT(I) = FTEMP/(GENTIM+0.0001)
              FIRST(2,I) = SQRT(DILATE)
              if (DEBUG)write(JOSTND,*)' I=',I, &
                   & ' HTT=',HHT,' TRAGE=',TRAGE,' GENTIM=',GENTIM,' FTEMP=',FTEMP, &
                   & ' STOMLT=',STOMLT(I),' DILATE=',DILATE
              99 continue
           end do

           !     LOOP ACROSS SPECIES TO ADJUST HEIGHTS OF BEST TREES
           !     TALL IS HEIGHT OF BEST TREE BY SPECIES
           do I = 1,NOFSPE
              if (IBEST(I).ne.1) go to 114
              TALL(I) = TALL(I)+HTADJ(I)
              if (TALL(I)-XMIN(I).lt.0.2) TALL(I) = XMIN(I)+0.2
              if (TALL(I).gt.HHTMAX(I)) TALL(I) = HHTMAX(I)
              if (DEBUG)write(JOSTND,*)'IN HT ADJUST LOOP 114 FOR BEST TREES.', &
                   & ' I=',I,' HTADJ=',HTADJ(I),' TALL=',TALL(I),' XMIN=',XMIN(I), &
                   & ' HHTMAX=',HHTMAX(I)
              114 continue
           end do
           if (DEBUG) write (JOSTND,7008) (TALL(I),I = 1,NOFSPE),(HTADJ(I), &
                & I = 1,NOFSPE),(ICHOI(1,I),I = 1,NOFSPE),(ICHOI(2,I),I = 1,NOFSPE), &
                & (STOMLT(I),I = 1,NOFSPE)
           7008 format('TALLEST TREE',T15,23F5.2,/,'ARRAY HTADJ',T15,23F5.2, &
                & /,'ICHOI (ADV)',T15,23I5,/,'       (SUB)',T15,23I5, &
                & /,'ARRAY STOMLT',T15,23F5.2)

           !     POPULATE WK6 WITH PROBABILITY OF EXCESS REGEN VALUES FOR SPECIES.
           !     AN ARRAY INDEX WILL ONLY RECIEVE A NON-ZERO PROBABILITY OF EXCESS
           !     REGEN VALUE WHEN A BEST TREE (ADVANCE OR SUBSEQUENT REGEN) OF THE
           !     SAME SPECIES WERE DETERMINED TO OCCUR ON THE PLOT (IBEST = 1). THIS
           !     LOOP IS CRUCIAL FOR THE ASSIGNMENT OF SPECIES TO EXCESS TREES THAT
           !     OCCURS LATER IN LOOP 148.
           do I = 1,NOFSPE
              WK6(I) = PXCS(I) *real(IBEST(I))
              if (DEBUG)write(JOSTND,*)'IN DO LOOP 141 ASSIGNING VALUES TO WK6.', &
                   & ' I=',I,' IBEST=',IBEST(I),' PXCS=',PXCS(I)
              if (IBEST(I).ne.1) go to 141
              if (WK6(I).lt.0.0001) WK6(I) = 0.0001
              141 continue
           end do
           SUM0 = 0.0
           do I = 1,NOFSPE
              SUM0 = SUM0+WK6(I)
              if (DEBUG)write(JOSTND,*)' IN LOOP 140 SUMMING VALUES FROM WK6', &
                   & ' TREES',' I=',I,' SUM0=',SUM0,' WK6(I)',WK6(I)
              140 continue
           end do

           !     DETERMINE HOW TO CALCULATE SUMUP(I) VALUES
           !     IF SUM0 IS ZERO, THEN NO BEST TREES WERE IDENTIFIED AND ALL SUMUP VALUES
           !     SHOULD BE ASSIGNED A VALUE OF ZERO. THESE SUMUP VALUES WILL BE USED IN
           !     LOOP 148 FOR ASSINGING SPECIES TO EXCESS TREE RECORDS.
           if (SUM0 .le. 0.0) then
              do I = 1, NOFSPE
                 SUMUP(I) = 0.0
                 if (DEBUG)write(JOSTND,*)' IN LOOP FOR DETERMINING SUMUP WHEN', &
                      & ' NO BEST TREES EXIST',' I=',I,' SUMUP=',SUMUP(I)
              end do
           else

              !     THIS LOOP IS USED TO CALCULATE THE SUMUP VALUES THAT WILL BE USED TO
              !     DETERMINE SPECIES OF EXCESS TREE RECORDS.
              SUMUP(1) = WK6(1)/SUM0
              do I = 2,NOFSPE-1
                 SUMUP(I) = SUMUP(I-1) + WK6(I)/SUM0
                 if (DEBUG)write(JOSTND,*)' IN LOOP 142 FOR DETERMINING SUMUP', &
                      & ' WHEN BEST TREES EXIST',' I=',I,' SUM0=',SUM0,' WK6(I)', &
                      & WK6(I),' SUMUP=',SUMUP(I)
                 142 continue
              end do
           end if
           if (DEBUG) write (JOSTND,7005) (SUMUP(I),I = 1,NOFSPE)
           ITP = ITPP
           J = ITPP+NOFSPE
           if (DEBUG)write(JOSTND,*)' ITPP=',ITPP,' NOFSPE=',NOFSPE,' J=',J
           do I = 1,J
              NOTE(I) = 0
              ICODE(I) = 0
              IASEP(I) = 0
              HEIGHT(I) = 0.0
              ESPROB(I) = 0.0
              HTIMLT(I) = 1.0
              144 continue
           end do

           !     ASSIGN SPECIES & HEIGHT TO FIRST TREE OF EACH SPECIES OCCURING ON
           !     REGEN PLOT. TRANSLATION: EVERY SPECIES THAT IS PREDICTED TO OCCUR ON
           !     REGEN PLOT (SPECIES THAT HAVE A VALUE OF 1 IN IBEST ARRAY) WILL HAVE
           !     ONE TREE RECORD ASSOCIATED WITH IT.THE VALUE OF N IS USED TO STORE
           !     SPECIES CODE AND HEIGHT FOR EACH SPECIES PREDICTED TO OCCUR ON A
           !     REGEN PLOT.EVENTUALLY N WILL EQUATE TO THE NUMBER OF TREE RECORDS THAT
           !     OCCUR ON A PLOT AS DEFINED BY ITPP. N WILL LATER BE USED TO POPULATE
           !     INFORMATION FOR EACH TREE RECORD THAT IS PASSED INTO THE TREE LIST
           !     FROM A REGEN PLOT(SEE STEP 12 OF ESTAB).
           N = 0
           do I = 1,NOFSPE
              JNULL(I) = 0
              ILSP(I) = 0
              if (IBEST(I).eq.0) go to 146
              N = N+1
              ICODE(N) = I
              IASEP(N) = 2
              if (ICHOI(1,I).eq.1) IASEP(N) = 1
              HEIGHT(N) = TALL(I)
              HTIMLT(N) = STOMLT(I)
              if (DEBUG)write(JOSTND,*)' IN LOOP 146 ASSINGING HEIGHTS TO BEST', &
                   & ' TREES OF EACH SPECIES',' N=',N,' ICODE=',ICODE(N),' IASEP(N)=', &
                   & IASEP(N),' HEIGHT(N)=',HEIGHT(N),' HTIMLT(N)=',HTIMLT(N)
              146 continue
           end do

           !     ASSIGN SPECIES AND HEIGHTS TO 'EXCESS' TREES (REMAINDER
           !     OF ITPP THAT IS NOT CONSIDERED EITHER ADVANCE IAESP =1 OR SUBSEQUENT
           !     REGENERATION IAESP = 2). HTS MUST BE BETWEEN THE MINIMUM ESTABLISHMENT
           !     HEIGHT FOR THAT SPECIES AND THE TALLEST TREE OF THE SAME SPECIES.
           !     EXIT=STMT 156. ASSIGNMENT OF SPECIES AND HEIGHT OCCURS IN LOOPS 156
           !     AND 148 BELOW.
           do I = 1,MAXTPP(IFT)*2
              call ESRANN (DRAW)
              WK6(I) = DRAW
              123 continue
           end do
           NDRAW = 0

           !     ITEMP IS THE STARTING POINT FOR PROCESSING EXCESS TREES. ITEMP IS
           !     DEFINED AS THE NUMBER OF SPECIES PREDICTED TO OCCUR ON REGEN PLOT + 1.
           !     N (CURRENTLY EQUIVALENT TO NUMSPE) TREE RECORDS HAVE BEEN PROCESSED
           !     SO FAR. ITEMP TO ITPP TREE RECORDS WILL NOW BE PROCESSED (EXCESS TREES).
           ITEMP = NUMSPE+1
           if (DEBUG)write(JOSTND,*)' NUMSPE=',NUMSPE,' ITEMP=',ITEMP, &
                & ' ITPP=',ITPP

           !     IF ALL TREES ON PLOT ARE ADVANCE OR SUBSEQUENT REGEN AND HAVE ALREADY
           !     BEEN ASSIGNED SPECIES AND HEIGHT IN LOOP 146, MOVE TO STMT 158
           if (ITEMP.gt.ITPP) go to 158
           !     START LOOP 156 ACROSS ITEMP TO ITPP TREE RECORDS
           do N = ITEMP,ITPP
              if (DEBUG)write(JOSTND,*)' IN DO LOOP 156 GETTING READY TO', &
                   & ' CALCULATE AND ASSIGN HEIGHTS OF EXCESS TREES.'
              NDRAW = NDRAW+1
              DRAW = WK6(NDRAW)

              !     LOOP 148: ASSIGN SPECIES TO EXCESS TREES
              !     LOOP ITERATES ACROSS NUMBER OF SPECIES IN VARIANT (NOFSPE) - 1.
              !     SPECIES ARE ASSIGNED TO EXCESS TREES THROUGH RANDOM SELECTION USING
              !     DRAW VARIABLE. ONLY SPECIES DETERMINED TO OCCUR ON PLOT WILL BE ASSIGNED
              !     TO EXCESS TREE RECORDS.
              do I = 1,NOFSPE-1
                 if (DEBUG)write(JOSTND,*)' IN DO LOOP TO 148',' I=',I,' SUMUP=', &
                      & SUMUP(I),' DRAW=',DRAW
                 !     COMPARE DRAW TO SUMUMP VALUE. SUMUP IS A CUMULATIVE SUM OF SPECIES
                 !     PROBABILITY OF OCCURENCE VALUES FOR EXCESS TREES. TWO POSSIBLE OUTCOMES:
                 !     1) IF SUMUP LESS THAN DRAW MOVE TO STMT 148 AND CONTINUE LOOKING
                 !        FOR SPECIES TO SELECT FOR EXCESS TREE RECORD N.
                 !     2) ELSE DRAW IS LESS THAN SUMUP AND SPECIES (I) IS ASSIGNED TO II (AND
                 !        LATER TO ICODE). MOVE TO STMT 150 TO CONTINUE DEFINING INFORMATION
                 !        FOR EXCESS TREE RECORD N.
                 if (DRAW.gt.SUMUP(I)) go to 148
                 II = I
                 if (DEBUG)write(JOSTND,*)' DRAW WAS NOT GREATER THAN SUMUP AND', &
                      & ' SPECIES (I) WAS SELECTED IN LOOP 148. ',' II=',II,' DRAW=', &
                      & DRAW,' SUMUP=', SUMUP(I)
                 go to 150
                 148 continue
              end do
              II = NOFSPE
              if (DEBUG)write(JOSTND,*)'NO SPECIES SELECTED IN LOOP 148, LAST', &
                   & ' SPECIES (NOFSPE) SELECTED. ',' II=',II,' DRAW=', DRAW,' SUMUP=', &
                   & SUMUP(I)
              150 continue
              ICODE(N) = II
              IASEP(N) = 3
              NDRAW = NDRAW+1
              DRAW = WK6(NDRAW)
              FTEMP = FLOKDT-real(KDTOLD)
              !     ESXCSH CALCULATES HEIGHT (HHT) OF SUBSEQUENT REGEN
              call ESXCSH (FTEMP,II,HHT,JOSTND,DEBUG)
              HEIGHT(N) = HHT+HTADJ(II)
              AGEXC(N) = FTEMP-DELAY-GENTIM
              if (DEBUG)write(JOSTND,*)' AFTER ESXCSH:',' II=',II, &
                   & ' HHT=',HHT,' HTADJ=',HTADJ(II),' AGEXC=',AGEXC(N),' FTEMP=', &
                   & FTEMP,' DELAY=',DELAY,' GENTIM=',GENTIM,' STOMLT=',STOMLT(I)
              !     CONSTRAIN HEIGHT OF EXECESS TREE RECORD N BETWEEN XMIN AND HHTMAX
              !     FOR SPECIES II.
              if (HEIGHT(N).lt.XMIN(II)) HEIGHT(N) = XMIN(II)
              if (HEIGHT(N).gt.HHTMAX(II)) HEIGHT(N) = HHTMAX(II)
              HTIMLT(N) = STOMLT(I)
              156 continue
           end do
           158 continue

           !     ASSIGN PROBABILITY OF STOCKING TO FTEMP
           FTEMP = PROB1(NCOUNT)

           !     IF ITPP IS GREATER THAN 0, THEN CALCULATE FTEMP2 VARIABLE AT THIS
           !     POINT BASED ON RATIO OF NEW TPP TO ADD TO PLOT (NEWTPP) AND PREDICTED
           !     TPP (ITPP).OTHERWISE SET FTEMP2 TO 0. NEWTPP TRACKS ONLY NEW TREES TO
           !     ESTABLISH ON PLOT AS DETERMINED IN STEP 5. IF NEWTPP IS 0, THEN NO TREES
           !     WILL BE PREDICTED TO OCCUR ON PLOT - PROBABILITY OF STOCKING FOR ALL TREE
           !     RECORDS WILL BE SET TO 0.0001. NEWTPP IS 0, WHEN PREDICTED ITPP IS LESS
           !     THAN OR EQUAL TO ACTUAL ITPP ON INVENTORY PLOT AT TIME OF INVENTORY.
           if (ITPP .gt. 0) then
              FTEMP2 = real(NEWTPP)/real(ITPP)
           else
              FTEMP2 = 0.0
           end if
           ITEMP = ITPP-NEWTPP
           if (DEBUG) write(JOSTND,*) ' FTEMP DEBUG',' FTEMP=',FTEMP, &
                & ' FTEMP2=',FTEMP2,' NEWTPP=',NEWTPP,' ITPP=',ITPP,' ITEMP=', &
                & ITEMP

           !     LOOP ACROSS TREE RECORDS PREDICTED TO OCCUR ON PLOT (ITPP)
           !     ASSIGN PROBABILITY OF STOCKING TO ESPROB
           !     ESPROB WILL BE SET TO 0.0001 IF FTEMP2 IS 0 FOR REASONS STATED IN COMMENT
           !     BLOCK ABOVE.
           do I = 1,ITPP
              ESPROB(I) = FTEMP
              if (I.lt.ITEMP+1) ESPROB(I) = FTEMP-PNN(NCOUNT)
              if (INGRO.eq.1.or.MATCH.eq.1) ESPROB(I) = FTEMP*FTEMP2
              if (ESPROB(I).lt.0.0001) ESPROB(I) = 0.0001
              if (DEBUG) write(JOSTND,*) ' IN DO LOOP 55',' I=',I,' FTEMP=', &
                   & FTEMP,' ITEMP=',ITEMP,' PNN(NCOUNT)=',PNN(NCOUNT),' INGRO=', &
                   & INGRO,' MATCH=',MATCH,' ESPROB=', ESPROB(I)
              55 continue
           end do
           PNN(NCOUNT) = FTEMP
           if (DEBUG)write(JOSTND,*)'AFTER DO LOOP 55: PNN(NCOUNT)=', &
                & PNN(NCOUNT)
           !     STATEMENT 163 IS USED WHEN STOADJ .LE. 0.0
           163 continue
           !     ZERO OUT ITPP, TPP, NEWTPP IF 0 IS SPECIFIED WITH STOCKADJ
           !     KEYWORD.
           if (STOADJ.lt.0.0001) then
              ITPP = 0
              TPP = 0.0
              NEWTPP = 0
           end if

           ! STEP 9: ADD PLANTED TREES FROM PLANT AND NATURAL KEYWORDS, EXIT=STMT 321

           if (DEBUG) write(JOSTND,9999)
           9999 format(/,'STEP 9: ADD TREES FROM PLANT/NATURAL KEYWORDS.')
           call ESRANN (DRAW)
           ITEMP = INT(DRAW*100000.0 + 0.5)
           ESAVE = real(ITEMP)

           ! IF NO PLANT/NATURALS SKIP BY GOING TO 321.
           if (DEBUG.and.MODE.ne.1) write(JOSTND,*)'NO PLANT/NATURAL ', &
                & 'KEYWORDS. SKIP STEP 9.'
           if (MODE.ne.1) go to 321
           ITODO = 0
           call OPFIND (2,MYACTS,NTODO)
           if (DEBUG) write (JOSTND,*)'NUMBER OF PLANT & NATURAL KEYWORDS=', &
                & NTODO
           ITOOMP = 0
           do IDO = 1,NTODO
              call OPGET (IDO,6,IPYEAR,IACTK,NP,PRMS)
              if (IACTK .le. 0) go to 322
              ITODO = ITODO+1
              if (ITPP+ITODO.gt.99) then
                 ITOOMP = ITOOMP+1
                 call OPDEL1(IDO)
                 ITODO = ITODO-1
                 go to 322
              end if

              !     SET PLANTING/NATURAL SPECIFICATIONS FROM KEYWORDS
              !     IPNSPE IS SPECIES
              !     PTREE IS THE NUMBER OF TREES TO PLANT ON REGEN PLOT
              !     TRAGE IS TREE AGE
              !     TREEHT IS HEIGHT OF REGEN 5 YEARS FROM PLANTING OR END OF CYCLE
              !     ISHADE/SHADE IS SHADE CODE
              !     FTEMP IS PLOT BASAL AREA
              if (DEBUG) write (JOSTND,*)'PLANT/NATURAL KEYWORD #',IDO,':', &
                   & PRMS(1),PRMS(2),PRMS(3),PRMS(4),PRMS(5),PRMS(6)
              IPNSPE = INT(PRMS(1))
              PTREE = (PRMS(2)*(PRMS(3)/100.0)) /DUPNPT
              TRAGE = PRMS(4)
              if (TRAGE.lt.0.5) TRAGE = 1.0
              if (TRAGE.gt.2.0) TRAGE = 2.0
              AGEPL(ITODO) = TRAGE
              DELAY = real(IPYEAR) -(KDT+1-FINT)
              TREEHT = PRMS(5)
              ISHADE = INT(PRMS(6))
              SHADE = 0.0
              if (ISHADE.eq.1) SHADE = 1.0
              if (ISHADE.eq.2) SHADE = -1.0
              FTEMP = BAA
              if (FTEMP.lt.1.0) FTEMP = 1.0

              !     THE ORIGINAL SHADE CODE ADJUSTMENT WAS NOT HAVING MUCH EFFECT.
              !     REPLACED BY A STRAIGHT RATIO OF BA ON THE PLOT BY DIXON 5/22/08
              !     NOTE CHANGED SETTING OF FTEMP, AND ACCUMULATIONS OF SUMS, ABOVE
              if (ISHADE.eq.1) then
                 PTREE = PTREE*NPTIDS*FTEMP/SUM1
              else if (ISHADE.eq.2) then
                 PTREE = PTREE*NPTIDS*(SUM1/FTEMP)/SUM2
              end if
              if (DEBUG) write(JOSTND,7048) PTREE,SHADE,FTEMP,BA,DELAY,TRAGE
              7048 format(' PLOT PTREE=',F6.1,'  SHADE=',F4.1,'  FTEMP=',F6.1, &
                   & '  BA=',F6.1,'  DELAY=',F4.1,'  TRAGE=',F4.1)
              DILATE = FIRST(2,IPNSPE)
              TMTIME = TIME
              TIME = FINT
              if (DEBUG) write (JOSTND,*)'TMTIME:',TMTIME,'TIME:',TIME
              call ESSUBH (IPNSPE,HHT,DELAY,GENTIM,TRAGE,JOSTND,DEBUG)
              TIME = TMTIME
              if (DEBUG) write (JOSTND,*)'TIME:',TIME
              if (TREEHT.ge.0.1) then
                 HHT = TREEHT
                 XH = ALOG(HHT)
                 161 continue
                 XXH = EXP(BACHLO(XH,0.5,ESRANN))
                 if (XXH.lt.(0.5*HHT) .or. XXH.gt.(2.0*HHT)) go to 161
                 HHT = XXH
                 HHT = HHT+HTADJ(IPNSPE)
                 if (HHT.lt.0.05) HHT = 0.05
              else
                 HHT = HHT+HTADJ(IPNSPE)
                 if (HHT.lt.XMIN(IPNSPE) ) HHT = XMIN(IPNSPE)
              end if
              FIRST(2,IPNSPE) = SQRT(DILATE)
              if (HHT.gt.HHTMAX(IPNSPE)) HHT = HHTMAX(IPNSPE)
              if (DEBUG) write(JOSTND,7050) IPNSPE,HHT,PTREE,ITODO,ITPP,IACTK, &
                   & TRAGE
              7050 format(' IPNSPE=',I3,'  HHT=',F6.2,'  PTREE=',F7.2,'  ITODO=',I4, &
                   & '  ITPP=',I4,'  IACTK=',I4,'  TRAGE=',F4.1)
              ICODE(ITPP+ITODO) = IPNSPE
              IASEP(ITPP+ITODO) = 4
              if (IACTK.eq.430) IASEP(ITPP+ITODO) = 3
              HEIGHT(ITPP+ITODO) = HHT
              ESPROB(ITPP+ITODO) = PTREE
              FTEMP = TRAGE
              if (FTEMP.gt.GENTIM) FTEMP = GENTIM
              HTIMLT(ITPP+ITODO) = FTEMP/(GENTIM+0.0001)
              AGEPL(ITPP+ITODO) = FINT-DELAY-GENTIM+AGEPL(ITODO)
              if (DEBUG) write (JOSTND,*)'MISCELLANEOUS VARIABLES. ICODE:', &
                   & ICODE(ITPP+ITODO),'IASEP:',IASEP(ITPP+ITODO),'HEIGHT:', &
                   & HEIGHT(ITPP+ITODO),'ESPROB:',ESPROB(ITPP+ITODO),'FTEMP:',FTEMP, &
                   & 'HTIMLT:',HTIMLT(ITPP+ITODO),'AGEPL:',AGEPL(ITPP+ITODO)
              322 continue
           end do
           if (ITOOMP.gt.0) then
              call RCDSET(1,.true.)
              write (JOSTND,323) ITOOMP
              323 format (/'******** WARNING:',I4,' PLANT OR NATURAL REQUESTS', &
                   & ' WERE CANCELED BECAUSE THERE WERE TOO MANY.')
           end if
           ITP = ITPP+ITODO
           321 continue
           call ESRNSD (.true.,ESAVE)
           if (DEBUG) write(JOSTND,7014) ESAVE
           if (STOADJ.lt.0.0001) go to 229

           !     SINCE INGROWTH SEQUENCE WILL AT MOST PRODUCE 3 TREES ON A PLOT,
           !     LOGIC IN STEP 10 UP TO STMT 176 GETS BYPASSED. ITP WILL BE EQUIVALENT
           !     TO ITPP DETERMINED BEFORE STEP 9 WHEN PLANTING/NATURAL KEYWORDS ARE
           !     NOT IN EFFECT. IF ITP IS LESS THAN 5, THEN ALL TREE RECORDS ARE
           !     CONSIDERED ADVANCED OR SUBSEQUEN (BEST TREES).
           if (ITP.lt.5) then
              do I = 1,ITP
                 NOTE(I) = 1
                 160 continue
              end do
              goto 176
           end if

           !     SCREEN OUT TREES WITH VERY LOW PROBABILITIES.
           ISTART = 1
           if (PROB1(NCOUNT).lt.0.00011) ISTART = ITPP +1

           ! STEP 10: IDENTIFY BEST TREES

           !     RULES FOR DETERMINING BEST TREES AS DESCRIBED IN FURGESON 1996
           !     'MODELING NATURAL REGENERATION ESTABLISHMENT IN THE NORTHERN
           !     ROCKY MOUNTAINS OF THE U.S'. - PAGE 31
           !     POTENTIAL CROP TREES WERE SAMPLED ON STOCKED PLOTS. THESE TREES
           !     ARE CALLED "BEST TREES" AND WERE CHOSEN BY THE FOLLOWING RULES:
           !     1) THE TWO TALLEST TREES ON THE PLOT REGARDLESS OF SPECIES.
           !     2) THE TALLEST TREE OF EACH ADDITIONAL SPECIES ON THE PLOT NOT CHOSEN
           !        IN RULE 1.
           !     3) IF RULES 1 AND 2 DO NOT, RESULT IN AT LEAST FOUR TREES, SELECT THE
           !        TALLEST REMAINING TREES, IF PRESENT, UNTIL AT LEAST FOUR TREES ARE
           !        CHOSEN.

           if (DEBUG) write(JOSTND,9989)
           9989 format(/,'STEP 10: IDENTIFY BEST TREES.')
           NBEST = 0
           NUM2PK = ITP-ISTART+1

           !     STEP 1:  TWO TALLEST TREES REGARDLESS OF SPECIES
           !     LOOP ACROSS TWO RECORDS (J)
           do J = 1,2
              if (DEBUG)write(JOSTND,*)'START OF LOOP 166 TO PICK TWO BEST', &
                   & ' TREES BASED ON HEIGHT',' NUM2PK=',NUM2PK,' NBEST=',NBEST, &
                   & ' ITP=',ITP,' ISTART=',ISTART
              !     IF NUMBER OF BEST TREES ALREADY GREATER THAN NUMBER OF BEST TREES
              !     TO PICK (NUM2PK), MOVE TO STMT 176
              if (NBEST.ge.NUM2PK) go to 176
              FTEMP = 0.001
              !     ITERATE FROM ISTART TO ITP
              do I = ISTART,ITP
                 if (DEBUG)write(JOSTND,*)' LOOP 164 PROCESSING ISTART TO ITP', &
                      & ' CHECKING',' NOTE AND HEIGHT.',' I=',I,' ITP=',ITP,' NOTE=', &
                      & NOTE(I),' HEIGHT=',HEIGHT(I),' FTEMP=',FTEMP
                 !     IF NOTE IS ALREADY 1, THEN DON'T ASSIGN HEIGHT TO FTEMP AND I TO ITEMP
                 if (NOTE(I).eq.1) go to 164
                 !     THIS CONDITIONAL ACTS AS A SORTING MECHANISM BASED ON HEIGHT
                 !     IF HEIGHT GREATER THAN VALUE IN FTEMP, THEN REPLACE FTEMP WITH HEIGHT
                 !     OTHERWISE PROCEED TO END OF LOOP 164
                 if (HEIGHT(I).lt.FTEMP) go to 164
                 FTEMP = HEIGHT(I)
                 ITEMP = I
                 if (DEBUG)write(JOSTND,*)' AT END OF DO LOOP TO 164.',' HEIGHT=', &
                      & HEIGHT(I)
                 164 continue
              end do
              !     BEST TREE HAS BEEN SELECTED
              !     SET SPECIES, NOTE AND INCREMENT NBEST
              !     NOTE = 1 CORRESPONDS TO A BEST TREE. EXCESS TREES HAVE VALUE OF 0 FOR NOTE.
              !     JNULL = 1 CORRESPONDS TO A BEST TREE. JNULL IS USED LATER IN LOOP 170 TO
              !     DETERMINE IF TREE RECORD OF A SPECIES HAS ALREADY BEEN SELECTED AS BEST TREE.
              N = ICODE(ITEMP)
              JNULL(N) = 1
              NOTE(ITEMP) = 1
              NBEST = NBEST+1
              if (DEBUG)write(JOSTND,*)' END OF DO LOOP 166. PRINT INFO FOR', &
                   & ' BEST TREE',' J=', J,' SPECIES(N)=',N,' NOTE=',NOTE(ITEMP), &
                   & ' NBEST=',NBEST
              166 continue
           end do

           !     STEP 2: LOOP 168 AND 169 IDENTIFY THE TALLEST TREES BY SPECIES AND ARE
           !     EXCLUSIVE TO THOSE TREES ALREADY SELECTED IN STEP 1 OF BEST TREE SELECTION
           !     (LOOP 166 AND 164). A BEST TREE WILL BE SELECTED FOR SPECIES THAT HAVE NOT
           !     HAD ONE DESIGNATED YET IN LOOP 170
           !     START OUTER LOOP ACROSS SPECIES 169
           !     START INNER LOOP ACROSS NUMBER OF TREES ON REGEN PLOT 168 (ITP)
           do J = 1,NOFSPE
              do I = ISTART,ITP
                 if (DEBUG)write(JOSTND,*)' IDENTIFYING TALLEST TREE OF EACH', &
                      & ' ADDITIONAL SPP (LOOP 168 WITHIN 169).',' SPP=',J,' I=',I, &
                      & ' ITP',ITP,' ICODE=',ICODE(I),' NOTE=',NOTE(I),' HEIGHT=', &
                      & HEIGHT(I),' TALL=',TALL(J)
                 !     IF:
                 !     SPECIES IN ICODE IS NOT EQUAL TO ONE BEING PROCESSED (J), MOVE TO 168
                 !     TREE WAS ALREADY SELECTED AS BEST TREE, MOVE TO 168
                 !     HEIGHT OF TREE IS LESS THAN HEIGHT FOR SPEICES, MOVE TO 168
                 if (ICODE(I).ne.J) go to 168
                 if (NOTE(I).eq.1) go to 168
                 if (HEIGHT(I).lt.TALL(J)) go to 168
                 !     COLLECT HEIGHT AND SPECIES CODE FOR SPECIES J
                 !     STORE HEIGHT I FOR SPECIES J
                 !     STORE TREE INDEX FOR SPECIES J
                 TALL(J) = HEIGHT(I)
                 ILSP(J) = I
                 if (DEBUG)write(JOSTND,*)' END OF LOOP 168.',' TALL=',TALL(J), &
                      & ' ILSP=',ILSP(J)
                 168 continue
              end do
              169 continue
           end do
           !     START LOOP 170 ACROSS ALL SPECIES
           !     IF ANY SPECIES THAT OCCURS ON REGEN PLOT HAS NOT HAD A BEST TREE
           !     SELECTED THEN THIS WILL HAPPEN IN LOOP 170.
           do J = 1,NOFSPE
              if (DEBUG)write(JOSTND,*)' LOOP 170 SELECT BEST TREE FOR', &
                   & ' ADDITIONAL SPECIES'
              !     IF TREE WAS ALREADY SELECTED AS BEST TREE (JNULL = 1) THEN MOVE TO END
              !     OF LOOP. NOTE ARRAY IS NOT USED IN THIS CONDITIONAL BECAUSE IT IS
              !     BEING UPDATED IN THIS LOOP AND WE NEED INFORMATION FROM BEST TREES
              !     DETERMINED FROM LOOP 168 WHICH IS STORED IN JNULL.
              if (JNULL(J).eq.1) go to 170
              ITEMP = ILSP(J)
              if (ITEMP.eq.0) go to 170
              NOTE(ITEMP) = 1
              NBEST = NBEST+1
              if (DEBUG)write(JOSTND,*)' END OF LOOP 170. BEST TREE WAS', &
                   & ' SELECTED FOR ADDITIONAL SPECIES:', J, 'TREE(ITEMP)=',ITEMP, &
                   & ' NOTE=',NOTE(ITEMP),' NBEST=',NBEST
              170 continue
           end do
           if (DEBUG)write(JOSTND,*)' AFTER LOOP 170',' NBEST=',NBEST, &
                & ' NUM2PK=',NUM2PK

           !     IF 4 BEST TREES ALREADY SELECTED OR NBEST GE NUM2PK, SKIP STEP 3
           if (NBEST.ge.4.or.NBEST.ge.NUM2PK) go to 176

           !     STEP 3:  IDENTIFY TALLEST REMAINING TREES BY DECREASING HEIGHT
           !     UNTIL A MINIMUM OF 4 TREES IS REACHED, IF POSSIBLE.
           172 continue
           FTEMP = 0.001
           !     LOOP ACROSS TREE RECORDS ON PLOT
           do J = ISTART,ITP
              if (DEBUG)write(JOSTND,*)'LOOP 174 SELECTING TALLEST REMAINING', &
                   & ' TREES UNTIL 4 IS REACHED'
              !     IF TREE ALREADY SELECTED AS BEST TREE, MOVE TO STMT 174
              !     IF HEIGHT IS LESS THAN FTEMP MOVE TO 174
              !     THIS LATTER CONDITIONAL ACTS AS A SORTING MECHANISM BASED ON HEIGHT.
              !     IF HEIGHT GREATER THAN VALUE IN FTEMP, THEN REPLACE FTEMP WITH HEIGHT
              !     OTHERWISE PROCEED TO 174.
              if (NOTE(J).eq.1) go to 174
              if (HEIGHT(J).lt.FTEMP) go to 174
              !     STORE HEIGHT AND TREE INDEX
              FTEMP = HEIGHT(J)
              ITEMP = J
              174 continue
           end do
           NBEST = NBEST+1
           NOTE(ITEMP) = 1
           if (DEBUG)write(JOSTND,*)'AFTER LOOP 174. PRINTING INFO FOR', &
                & ' REMAINING BEST TREE SELECTED:',' ITEMP=', ITEMP,' HT(FTEMP)=', &
                & FTEMP,' NBEST=',NBEST,' NOTE=',NOTE(ITEMP),' NUM2PK=',NUM2PK
           if (NBEST.ge.NUM2PK) go to 176
           !     IF 4 BEST TREES ARE STILL NOT IDENTIFIED REPEAT LOOP 174
           if (NBEST.lt.4) go to 172
           176 continue
           !     NOTE: STMT 229 IS USED WHEN STOADJ .LE. 0.0
           229 continue

           !     PRINT OUT PLOT STATISTICS IF DEBUG IS ON.
           if (DEBUG) then
              ITEMP = INT((RADIAN*57.2958) + 0.5)
              write(JOSTND,6002) NN,IHAB,ITEMP,SLO,IPREP,PROB1(NCOUNT), &
                   & ITPP,NEWTPP,NCOUNT
              6002 format(/,'PLOT  H.TYPE  ASP  SLO  PREP',2X,'P(STOCK)  TREES ', &
                   & 'PER PLOT    NCOUNT',/,4('-'),2X,6('-'),2X,2(3('-'),2X), &
                   & 4('-'),2X,8('-'),2X,16('-'),2X,6('-'),/,I3,T8,I3,T14,I4,T19, &
                   & F4.2,T27,I1,T32,F6.4,T40,I3,' TOTAL,',I3,' NEW',T60,I3,//,T4, &
                   & 'TREE RECORDS:',T19,'TREE#  SPECIES  HEIGHT  BEST?  ASEP#  ', &
                   & 'ESPROB  HTIMLT',/,T19,'-----  -------  ------  -----  -----', &
                   & '  ------  ------')
              if (ITPP.gt.0) write(JOSTND,6006) (I,ICODE(I),HEIGHT(I), &
                   & NOTE(I),IASEP(I),ESPROB(I),HTIMLT(I),  I = 1,ITPP)
              6006 format(T20,I3,T29,I2,T36,F4.1,T45,I1,T49,I4,2X,F8.4,2X,F6.3)
              if (MODE.eq.1) write(JOSTND,6007)
              6007 format(T4,'PREDICTED NATURALS ABOVE; PLANT/NATURAL BELOW.')
              if (MODE.eq.1) write(JOSTND,6006) (I,ICODE(I),HEIGHT(I),NOTE(I), &
                   & IASEP(I),ESPROB(I),HTIMLT(I),  I = ITPP+1,ITP)
           end if
           do I = 1,NOFSPE
              EXCESS(I) = 0.0
              SUMHTS(I) = 0.0
              SUMESP(I) = 0.0
              205 continue
           end do

           !     MAKE SURE THAT THERE IS ROOM IN THE TREE LIST FOR REGENERATION.
           !     THE CALL TO RDESCP INSURES THAT THE SPACE REQUIRED IS BELOW THAT
           !     NEEDED BY THE ROOT DISEASE MODEL.  RDESCP RETURNS THE MAXIMUM
           !     NUMBER OF TREES THAT ROOT DISEASE CAN HANDLE.  IF RROT IS NOT
           !     BEING RUN, MXRR IS RETURNED AS MAXTRE.
           call RDESCP (MAXTRE,  MXRR)
           if (ITRN.ge.MXRR-99) then
              call ESGENT (ITRNIN)
              ITEMP = MXRR/2
              call ESCPRS (ITEMP,DEBUG)
              ITRNIN = ITRN+1
              !       OPFIND IS CALLED TO RESET THE PARAMETERS FOR THE
              !       PLANT & NATURAL KEYWORDS.
              call OPFIND (2,MYACTS,NTODO)
           end if

           ! STEP 12: PASS THE TREE RECORDS TO THE FVS TREE LIST

           if (DEBUG) write(JOSTND,9987)
           9987 format(/,'STEP 12: PASS THE TREE RECORDS TO THE FVS TREE LIST.')

           !     NATURALS FIRST.  EXIT=STMT 200
           if (ITPP.lt.1) go to 200

           !     PASS ADVANCE AND SUBSEQUENT REGENERATION FIRST (BEST TREES)
           !     THIS PROCESS ENDS AT STMT 33
           do N = 1,ITPP
              if (DEBUG)write(JOSTND,*)'START DO LOOP 33 PASSING TREES TO', &
                   & ' TREELIST (ADV AND SUB) AND INDENTIFYING EXCESS TREES.'
              !     N IS TREE NUMBER
              !     I IS SPECIES CODE
              I = ICODE(N)
              if ((ISCT(I,1).eq.0).and.(ISCT(I,2).eq.0))JSPIN(I) = JSPINDEF
              !     HHT IS TREE HEIGHT
              HHT = HEIGHT(N)
              if (DEBUG)write(JOSTND,*)'DO LOOP 33 CHECKING FOR EXCESS TREE.', &
                   & ' N=',N,' SPECIES=',I,' ESPROB=',ESPROB(N),' NOTE=', NOTE(N)
              !     IF ESPROB IS LESS THAN 0.00011 AND NOTE IS NOT ONE THEN RECORD
              !     IS EXCESS TREE. GO TO STMT 199.
              if (ESPROB(N).lt.0.00011) go to 199
              if (NOTE(N).ne.1) go to 199
              ITRN = ITRN+1
              IMC(ITRN) = 1
              ISP(ITRN) = I
              ITRE(ITRN) = NNID
              CFV(ITRN) = 0.
              MCFV(ITRN) = 0.
              SCFV(ITRN) = 0.
              CULL(ITRN) = 0.
              DECAYCD(ITRN) = 0
              WDLDSTEM(ITRN) = 0
              ABVGRD_BIO(ITRN) = 0.
              ABVGRD_CARB(ITRN) = 0.
              MERCH_BIO(ITRN) = 0.
              MERCH_CARB(ITRN) = 0.
              CUBSAW_BIO(ITRN) = 0.
              CUBSAW_CARB(ITRN) = 0.
              FOLI_BIO(ITRN) = 0.
              FOLI_CARB(ITRN) = 0.
              CARB_FRAC(ITRN) = 0.

              ITRUNC(ITRN) = 0
              DEFECT(ITRN) = 0
              ISPECL(ITRN) = 0
              NORMHT(ITRN) = 0
              HT2TD(ITRN,1) = 0.
              HT2TD(ITRN,2) = 0.
              !     PROB IS STAND TPA REPRESENTED BY TREE RECORD
              PROB(ITRN) = (ESPROB(N)*EXPAND) /DUPNPT
              DBH(ITRN) = 0.1
              HT(ITRN) = HHT
              ABIRTH(ITRN) = AGADSB(N)
              PTOCFV(ITRN) = 0.
              PMRCFV(ITRN) = 0.
              PSCFV(ITRN) = 0.
              PMRBFV(ITRN) = 0.
              NCFDEF(ITRN) = 0
              NBFDEF(ITRN) = 0
              PDBH(ITRN) = 0.
              PHT(ITRN) = 0.
              ZRAND(ITRN) = -999.
              call RDESTB (ITRN,PROB(ITRN))
              ICR(ITRN) = 0
              DG(ITRN) = 0.0
              HTG(ITRN) = 0.0
              PCT(ITRN) = 0.0
              OLDPCT(ITRN) = 0.0
              OLDRN(ITRN) = 0.0
              WK1(ITRN) = 0.0
              WK2(ITRN) = 0.0
              WK4(ITRN) = HTIMLT(N)
              BFV(ITRN) = 0.0
              PTBALT(ITRN) = PTBAA(NNID)
              IDTREE(ITRN) = IDCMP1+ITMP+ITRN
              call MISPUTZ(ITRN,0)
              !     CALL BLISTER RUST TO PROCESS PLANTED TREES.
              call BRESTB (TIME,5,I)
              !     BEST TREES NOT SUBJECT TO MORTALITY FOR THE FIRST 20 YEARS. THIS
              !     ONLY APPLIES TO BEST TREES OCCURING IN AUTOMATIC TALLY SEQUENCES.
              !     THIS MORTALITY RULE DOES NOT APPLY TO TREES REGENERATING DURING
              !     INGROWTH GROWTH SEQUENCES. THE CALCULATION OF IEASTAT WILL EVALUATE
              !     SO THAT THERE IS NO DELAY TIME IN MORTALITY DURING INGROWTH SEQUENCE.
              IESTAT(ITRN) = IDSDAT+20
              if (DEBUG)write(JOSTND,*)'IN DO LOOP 33 CHECKING VALUES FOR TREE', &
                   & ' RECORD (ADV AND SUB REGEN) BEING PASSED - (1)',' N=',N, &
                   & ' ITRN=',ITRN,' ISP=', ISP(ITRN),' ITRE=',ITRE(ITRN),' IDTREE=', &
                   & IDTREE(ITRN),' PROB=',PROB(ITRN),' HT=', HT(ITRN),' DUPNPT=', &
                   & DUPNPT
              if (DEBUG)write(JOSTND,*)'IN DO LOOP 33 CHECKING VALUES FOR TREE', &
                   & ' RECORD (ADV AND SUB REGEN) BEING PASSED - (2)', ' ABIRTH=', &
                   & ABIRTH(N),' IESTAT=',IESTAT(ITRN),' WK4=',WK4(ITRN),' HTIMLT=', &
                   & HTIMLT(N),' IDCMP1=',IDCMP1,' ITMP=',ITMP
              go to 33
              199 continue
              !     EXCESS TREE HAS BEEN FOUND
              !     UPDATE NUMBER OF EXCESS RECORDS FOR SPECIES I
              !     UPDATE SUM OF HEIGHTS FOR SPECIES I
              !     UPDATE SUM OF PSTOCK VALUES FOR SPECIES I
              EXCESS(I) = EXCESS(I)+1.0
              SUMHTS(I) = SUMHTS(I)+HHT
              SUMESP(I) = SUMESP(I)+ESPROB(N)
              if (DEBUG)write(JOSTND,*)'EXCESS TREE FOUND -- IN DO LOOP 33', &
                   & ' AFTER  STMT 199.',' I=',I,' ITRE=',ITRE(ITRN),' EXCESS=', &
                   & EXCESS(I),' SUMHTS=', SUMHTS(I),' HHT=',HHT,' SUMESP=',SUMESP(I), &
                   & ' ESPROB=',ESPROB(N)
              33 continue
           end do

           200 continue
           !     PASS EXCESS TREES TO THE TREE LIST.  EXIT=STMT 228
           do I = 1,NOFSPE
              if (EXCESS(I).lt.0.5) go to 228
              if (SUMESP(I).lt.0.00011.and.I.lt.NOFSPE) then
                 SUMESP(I+1) = SUMESP(I+1)+SUMESP(I)
                 SUMESP(I) = 0.0
                 go to 228
              end if
              if ((ISCT(I,1).eq.0).and.(ISCT(I,2).eq.0))JSPIN(I) = JSPINDEF
              !     IBRKUP: IS THE NUMBER OF EXCESS TREES THAT WILL BE CREATED FOR SPECIES I
              !     IT IS DEFINED AS TOTAL NUMBER OF EXCESS RECORDS FOR SPECIES I DIVIDED BY
              !     5 AND CAST TO INTEGER.
              !     IF EXCESS(I) < 5 THEN 1 EXCESS RECORD WILL BE CREATED
              !     IF EXCESS(I) >= 5 THEN 2 EXCESS RECORDS WILL BE CREATED
              !     IF EXCESS(I) >= 10 THEN 3 EXCESS RECORDS WILL BE CREATED
              !     FTEMP2: IS VARIABLE THAT WILL USED TO DERIVE EXPANSION FACTOR FOR
              !     EXCESS TREE RECORDS. IT IS DEFINED AS THE FOLLOWING RATIO:
              !     SUM OF PROB STOCK VALUES / TOTAL EXCESS RECORDS FOR SPECIES I AND
              !     WILL YIELD PROBABILITY OF STOCKING.
              FTEMP2 = SUMESP(I)/(EXCESS(I)+0.00001)
              IBRKUP = INT(EXCESS(I)/5.0 +1.0)
              BRKUP = real(IBRKUP)
              if (DEBUG)write(JOSTND,*)'IN DO LOOP 228 PROCESSING EXCESS TREES', &
                   & ' FOR SPECIES: ', I
              if (DEBUG)write(JOSTND,*)'IN DO LOOP 228 CHECKING NUMBER OF', &
                   & ' EXCESS TREES THAT WILL BE CREATED FOR SPECIES: ',I,' FTEMP2=', &
                   & FTEMP2,' SUMESP=', SUMESP(I),' EXCESS=',EXCESS(I),' IBRKUP=', &
                   & IBRKUP,' BRKUP=',BRKUP
              !     LOOP ACROSS EXCESS TREE RECORDS FOR SPECIES I AND POPULATE INFORMATION
              !     FOR EACH RECORD.
              do II = 1,IBRKUP
                 ITRN = ITRN+1
                 HHT = SUMHTS(I)/EXCESS(I)
                 IMC(ITRN) = 2
                 ISP(ITRN) = I
                 ITRE(ITRN) = NNID
                 CFV(ITRN) = 0.0
                 MCFV(ITRN) = 0.
                 SCFV(ITRN) = 0.
                 CULL(ITRN) = 0.
                 DECAYCD(ITRN) = 0
                 WDLDSTEM(ITRN) = 0
                 ABVGRD_BIO(ITRN) = 0.
                 ABVGRD_CARB(ITRN) = 0.
                 MERCH_BIO(ITRN) = 0.
                 MERCH_CARB(ITRN) = 0.
                 CUBSAW_BIO(ITRN) = 0.
                 CUBSAW_CARB(ITRN) = 0.
                 FOLI_BIO(ITRN) = 0.
                 FOLI_CARB(ITRN) = 0.
                 CARB_FRAC(ITRN) = 0.

                 ITRUNC(ITRN) = 0
                 DEFECT(ITRN) = 0
                 ISPECL(ITRN) = 0
                 NORMHT(ITRN) = 0
                 XCSMAX = EXCESS(I)/BRKUP
                 FTEMP = PASMAX/BRKUP
                 if (XCSMAX.gt.FTEMP) XCSMAX = FTEMP
                 PROB(ITRN) = (FTEMP2*EXPAND*XCSMAX) /DUPNPT
                 DBH(ITRN) = 0.1
                 HT(ITRN) = HHT
                 ABIRTH(ITRN) = AGEXC(I)
                 PTOCFV(ITRN) = 0.
                 PMRCFV(ITRN) = 0.
                 PSCFV(ITRN) = 0.
                 PMRBFV(ITRN) = 0.
                 NCFDEF(ITRN) = 0
                 NBFDEF(ITRN) = 0
                 PDBH(ITRN) = 0.
                 PHT(ITRN) = 0.
                 ZRAND(ITRN) = -999.
                 call RDESTB (ITRN,PROB(ITRN))
                 ICR(ITRN) = 0
                 DG(ITRN) = 0.0
                 HTG(ITRN) = 0.0
                 PCT(ITRN) = 0.0
                 OLDPCT(ITRN) = 0.0
                 OLDRN(ITRN) = 0.0
                 WK1(ITRN) = 0.0
                 WK2(ITRN) = 0.0
                 WK4(ITRN) = STOMLT(I)
                 BFV(ITRN) = 0.0
                 IESTAT(ITRN) = 0
                 PTBALT(ITRN) = PTBAA(NNID)
                 IDTREE(ITRN) = IDCMP1+ITMP+ITRN
                 call MISPUTZ(ITRN,0)
                 !     CALL BLISTER RUST TO PROCESS PLANTED TREES.
                 call BRESTB (TIME,5,I)
                 PASTPA(I) = PASTPA(I) +(FTEMP2*EXPAND*XCSMAX) /DUPNPT
                 TCROP2 = TCROP2 +(FTEMP2*EXPAND*XCSMAX) /DUPNPT
                 if (DEBUG)write(JOSTND,*)'IN LOOP 226 CHECKING VALUES FOR TREE', &
                      & ' RECORD (EXCESS) BEING PASSED - (1)', ' ITRN=',ITRN,' II=',II, &
                      & ' ISP=',ISP(ITRN),' ITRE=', ITRE(ITRN), ' IDTREE=', IDTREE(ITRN), &
                      & ' PROB=', PROB(ITRN),' HT=', HT(ITRN),' DUPNPT=',DUPNPT
                 if (DEBUG)write(JOSTND,*)'IN LOOP 226 CHECKING VALUES FOR TREE', &
                      & ' RECORD (EXCESS) BEING PASSED- (2)',' ABIRTH=',ABIRTH(ITRN), &
                      & ' WK4=', WK4(ITRN),' STOMLT=',STOMLT(I),' IDCMP1=',IDCMP1, &
                      & ' ITMP=',ITMP,' FTEMP2=',FTEMP2,' XCSMAX=',XCSMAX
                 226 continue
              end do
              228 continue
           end do

           !     PASS TREES FROM 'PLANT' AND 'NATURAL' KEYWORDS.  EXIT = STMT 227
           if (MODE.ne.1) go to 227
           J = ITP - ITPP
           if (DEBUG)write(JOSTND,*)'ENTERING SECTION PERTAINING TO PASSING', &
                & ' PLANTED TREE RECORDS TO TREELIST.'
           if (DEBUG)write(JOSTND,*)' J=',J,' ITP=',ITP,' ITPP=',ITPP
           do JJ = 1,J
              I = ICODE(ITPP+JJ)
              if (DEBUG)write(JOSTND,*)'IN DO LOOP 35',' I=',I,' ITPP=',ITPP, &
                   & ' JJ=',JJ,' ISCT(I,1)=',ISCT(I,1),' ISCT(I,2)',ISCT(I,2), &
                   & ' JSPINDEF=',JSPINDEF,' FTEMP=',FTEMP
              if ((ISCT(I,1).eq.0).and.(ISCT(I,2).eq.0))JSPIN(I) = JSPINDEF
              FTEMP = ESPROB(ITPP+JJ)
              if (FTEMP.lt.0.00011.and.JJ.lt.J) then
                 ESPROB(ITPP+JJ+1) = ESPROB(ITPP+JJ+1)+FTEMP
                 ESPROB(ITPP+JJ) = 0.0
                 go to 35
              end if
              IBRKUP = INT(FTEMP/10.0 + 1.0)
              BRKUP = real(IBRKUP)
              TOTTPA(I) = TOTTPA(I)+FTEMP
              PASTPA(I) = PASTPA(I)+FTEMP
              BESTPA(I) = BESTPA(I)+FTEMP
              TCROP1 = TCROP1+FTEMP
              TCROP2 = TCROP2+FTEMP
              TTOTTP = TTOTTP+FTEMP
              do II = 1,IBRKUP
                 ITRN = ITRN+1
                 HHT = HEIGHT(ITPP+JJ)
                 IMC(ITRN) = 1
                 if (NOTE(ITPP+JJ).ne.1.and.STOADJ.gt.0.0) IMC(ITRN) = 2
                 ISP(ITRN) = I
                 CFV(ITRN) = 0.
                 MCFV(ITRN) = 0.
                 SCFV(ITRN) = 0.
                 CULL(ITRN) = 0.
                 DECAYCD(ITRN) = 0
                 WDLDSTEM(ITRN) = 0
                 ABVGRD_BIO(ITRN) = 0.
                 ABVGRD_CARB(ITRN) = 0.
                 MERCH_BIO(ITRN) = 0.
                 MERCH_CARB(ITRN) = 0.
                 CUBSAW_BIO(ITRN) = 0.
                 CUBSAW_CARB(ITRN) = 0.
                 FOLI_BIO(ITRN) = 0.
                 FOLI_CARB(ITRN) = 0.
                 CARB_FRAC(ITRN) = 0.

                 ITRUNC(ITRN) = 0
                 DEFECT(ITRN) = 0
                 ISPECL(ITRN) = 0
                 NORMHT(ITRN) = 0
                 ITRE(ITRN) = NNID
                 FTEMP2 = FTEMP/BRKUP
                 PROB(ITRN) = FTEMP2
                 DBH(ITRN) = 0.1
                 HT(ITRN) = HHT
                 ABIRTH(ITRN) = AGEPL(ITPP+JJ)
                 PTOCFV(ITRN) = 0.
                 PMRCFV(ITRN) = 0.
                 PSCFV(ITRN) = 0.
                 PMRBFV(ITRN) = 0.
                 NCFDEF(ITRN) = 0
                 NBFDEF(ITRN) = 0
                 PDBH(ITRN) = 0.
                 PHT(ITRN) = 0.
                 ZRAND(ITRN) = -999.
                 call RDESTB (ITRN,PROB(ITRN))
                 ICR(ITRN) = 0
                 DG(ITRN) = 0.0
                 HTG(ITRN) = 0.0
                 PCT(ITRN) = 0.0
                 OLDPCT(ITRN) = 0.0
                 OLDRN(ITRN) = 0.0
                 WK1(ITRN) = 0.0
                 WK2(ITRN) = 0.0
                 WK4(ITRN) = HTIMLT(ITPP+JJ)
                 BFV(ITRN) = 0.0
                 IESTAT(ITRN) = 0
                 PTBALT(ITRN) = PTBAA(NNID)
                 IDTREE(ITRN) = IDCMP1+ITMP+ITRN
                 if (DEBUG)write(JOSTND,*)'IN LOOP 24 CHECKING VALUES FOR TREE', &
                      & ' RECORD (PLANT/NATURAL) - (1)',' ITRN=',ITRN,' II=',II, &
                      & ' ISP=',ISP(ITRN),' IDTREE=',IDTREE(ITRN), ' PROB=',PROB(ITRN), &
                      & ' ABIRTH=',ABIRTH(ITRN),' ITPP=',ITPP,' HTIMLT=',HTIMLT(ITPP+JJ)
                 call MISPUTZ(ITRN,0)
                 !     CALL BLISTER RUST TO PROCESS PLANTED TREES.
                 call BRESTB (TIME,5,I)
                 24 continue
              end do
              35 continue
           end do
           227 continue

           !     STEP 11: ACCUMULATE PLOT STATISTICS - INSIDE STEP 12

           SUMPRB = SUMPRB+PROB1(NCOUNT)
           SUMPRE(IPREP) = SUMPRE(IPREP) +1.0
           SUMPRE(5) = SUMPRE(5)+1.0
           IHABT(IHAB) = IHABT(IHAB) +1
           if (ITPP.lt.1) go to 217
           do I = 1,ITPP
              N = ICODE(I)
              FTEMP2 = (ESPROB(I)*EXPAND) /DUPNPT
              TOTTPA(N) = TOTTPA(N)+FTEMP2
              TTOTTP = TTOTTP+FTEMP2
              if (NOTE(I).ne.1) go to 215
              BESTPA(N) = BESTPA(N)+FTEMP2
              TCROP1 = TCROP1+FTEMP2
              PASTPA(N) = PASTPA(N)+FTEMP2
              TCROP2 = TCROP2+FTEMP2
              215 continue
           end do
           217 continue
           !     END OF PLOT CYCLE. RETURN AND CONTINUE PROJECTION.
           201 continue
        end do
        202 continue
     end do
     203 continue
  end do
  !     ALL PLOTS ARE PROCESSED.
  if (DEBUG) write (JOSTND,6030) ITRNIN,ITRN
  6030 format(36('= '),/,'ITRNIN=',I6,'  ITRN=',I6)
  if (NTODO.gt.0) then
     do ITODO = 1,NTODO
        call OPGET (ITODO,6,IPYEAR,IACTK,NP,PRMS)
        if (IACTK .le. 0) go to 225
        ITEMP = INT(PRMS(1))
        IPLANT(ITEMP) = 1
        call OPDONE (ITODO,IPYEAR)
        225 continue
     end do
  end if
  !     IF NEW TREES HAVE BEEN ADDED TO THE TREELIST
  !     'GROW' TREES TO THE END OF THE CYCLE
  if (ITRN.ge.ITRNIN)call ESGENT (ITRNIN)
  if (DEBUG)write(JOSTND,*)'AFTER CALL TO ESGENT-ITRN,ITRNIN= ', &
       & ITRN,ITRNIN
  do I = ITRNIN,ITRN
     !     CALCULATE A CROWN WIDTH FOR SEEDLINGS/SPROUTS
     CRDUM = 1.
     call CWCALC(ISP(I),PROB(I),DBH(I),HT(I),CRDUM, &
          & ICR(I),CW,0,JOSTND)
     CRWDTH(I) = CW
     !     INCREMENT SEEDLING AGE
     ABIRTH(I) = ABIRTH(I)+GENTIM
     230 continue
  end do

  ! STEP 13: PRINT REGENERATION SUMMARY

  if (IPRINT.eq.0) go to 260
  SUMPRB = SUMPRB/DUPNPT
  if (NTALLY.ne.1) go to 233
  do I = 1,4
     SUMPRE(I) = (SUMPRE(I)/SUMPRE(5))*100.0
     204 continue
  end do
  do I = 1,16
     IHABT(I) = IHABT(I)/IDUP
     232 continue
  end do
  233 continue
  do N = 1,NOFSPE
     AVEHT(N) = SUMPX(N)/(SUMPI(N)+ .00001)
     if (BESTPA(N).lt.0.5) AVEHT(N) = 0.0
     TOTPCT(N) = (TOTTPA(N)/(TTOTTP+.00001))*100.0
     BESPCT(N) = (BESTPA(N)/(TCROP1+.00001))*100.0
     PASPCT(N) = (PASTPA(N)/(TCROP2+.00001))*100.0
     206 continue
  end do
  N = INT(SUMPRE(1) + 0.5)
  NN = INT(SUMPRE(2) + SUMPRE(4) + 0.5)
  ITEMP = INT(SUMPRE(3) + 0.5)
  J = INT(ZHARV + .5)
  K = INT(ZMECH + .5)
  L = INT(ZBURN + .5)
  if (INGRO.eq.1) go to 258
  if (NTALLY.eq.1) then
     call GROHED (JOREGT)
     write(JOREGT,6012) NPLT,MGMID
     6012 format(/,76('-'),/,T17,'REGENERATION ESTABLISHMENT ', &
          & 'MODEL VERSION 2.0',/,T9,'STAND ID: ',A26,5X, &
          & 'MANAGEMENT CODE: ',A4,/,76('-') )
     write(JOREGT,6013) J,K,L,N,NN,ITEMP,(IHABT(I),I = 1,16)
     6013 format(/,T3,'SITE PREP SUMMARY',T38,'PLOT HABITAT TYPE SUMMARY', &
          & /,19('-'),T23,54('-'),/,'PREP:NONE MECH BURN   SERIES:', &
          & '<--- DF --> <--- GF -->  C WH <--- AF & MH --->',/,'YEAR:', &
          & 3(I4,1X),'   GROUP: 1  2  3  4  5  6  7  8  9 10 11 12 13 14 ', &
          & '15 16',/,T2,'PCT:',I4,2I5,'   #PLOTS:',I2,15I3)
     call DBSSITEPREP(J,K,L,N,NN,ITEMP)
     do M = 1,16
        if (IHABT(M).gt.0) call DBSPLOTHAB(IHABT(M),HABTYP(M), &
             & SERIES(M),M)
     end do
     if (IPINFO.eq.3) write(JOREGT,6022)
     6022 format (/,'NOTE: SOME OR ALL PLOT ID CODES FROM THE TREE ', &
          & 'DATA FILE DID NOT MATCH',/,T7,'THOSE IN THE PLOTINFO FILE.  ', &
          & 'STAND VALUES WERE SUBSITUTED.')
     if (NONSTK.gt.0) write(JOREGT,6017) NONSTK
     6017 format(/,'NOTE:',I3,' NON-STOCKABLE PLOTS EXCLUDED FROM ', &
          & 'THIS SUMMARY','.')
  end if
  ITEMP = KDT+1-J
  if (STOADJ.lt.0.0001) SUMPRB = 0.0
  write(JOREGT,6021) NTALLY,ITEMP,SUMPRB,KDT
  6021 format(/,'TALLY',I2,' AT',I3,' YEARS.  PROBABILITY OF', &
       & ' STOCKING IS ',F6.4,' IN THE FALL OF',I5,'.')
  N = 0
  CLAST(1) = CBLANK
  do I = 1,NOFSPE
     if (IPLANT(I).lt.1) go to 207
     N = N+1
     CLAST(N) = NSP(I,1)(1:2)
     207 continue
  end do
  if (N.eq.0) N = 1
  if (MODE.eq.1) write(JOREGT,6015) (CLAST(I),I = 1,N)
  6015 format(/'NOTE: REPORTED PROBABILITY OF STOCKING DOES NOT ', &
       & 'INCLUDE TREES FROM PLANT ',/,T7,'AND NATURAL KEYWORDS.  ', &
       & 'SPECIES= ',23A4)
  write(JOREGT,6016) REGNBK
  6016 format(/,T6,'SUMMARY OF ALL',T30,'SUMMARY OF BEST         ', &
       & 'TREES <',F4.1,' IN. DBH',/,T6,'TREES REGENERATING',T30, &
       & 'TREES REGENERATING      BEING PROJECTED BY',/,T6, &
       & 'DURING THIS TALLY.',T30,'DURING THIS TALLY.      THE ', &
       & 'PROGNOSIS MODEL',/,T6,19('-'),5X,19('-'),5X,19('-'),/,13X, &
       & 'TREES % OF      TREES % OF  AVERAGE     TREES % OF',/,5X, &
       & 'SPECIES /ACRE TOTAL     /ACRE TOTAL HEIGHT      /ACRE TOTAL ', &
       & 'SPECIES',/,5X,7('-'),1X,5('-'),1X,5('-'),5X,5('-'),1X,5('-'), &
       & 1X,7('-'),5X,5('-'),1X,5('-'),1X,7('-') )
  WTAVEHT = 0.0
  do I = 1,NOFSPE
     if (TOTTPA(I).le.0. .and. BESTPA(I).le.0. .and. PASTPA(I).le.0.) &
          & go to 222
     write(JOREGT,6014) NSP(I,1),TOTTPA(I),TOTPCT(I),BESTPA(I), &
          & BESPCT(I),AVEHT(I),PASTPA(I),PASPCT(I),NSP(I,1)
     6014 format(T9,A2,2X,2F6.0,4X,2F6.0,1X,F5.1,6X,2F6.0,3X,A2)
     WTAVEHT = AVEHT(I)*BESTPA(I)+WTAVEHT
     call DBSTALLY(SUMPRB,NTALLY,NSP(I,1),TOTTPA(I), &
          & TOTPCT(I),BESTPA(I),BESPCT(I),AVEHT(I),PASTPA(I),PASPCT(I),KDT, &
          & WTAVEHT,1,TTOTTP,TCROP1,TCROP2)
     222 continue
  end do
  if (TCROP1 .gt. 0.0) WTAVEHT = WTAVEHT/TCROP1
  call DBSTALLY(SUMPRB,NTALLY,FINAL,TOTTPA(I), &
       & TOTPCT(I),BESTPA(I),BESPCT(I),AVEHT(I),PASTPA(I),PASPCT(I),KDT, &
       & WTAVEHT,2,TTOTTP,TCROP1,TCROP2)
  write(JOREGT,6026) TTOTTP,TCROP1,TCROP2
  6026 format(T14,5('-'),T30,5('-'),T54,5('-'),/,T12,F7.0,T28,F7.0, &
       & T52,F7.0,/)
  go to 259
  !     THIS SECTION PRINTS INGROWTH SUMMARY
  258 continue
  write(JOREGT,6012) NPLT,MGMID
  if (MODE.eq.1) then
     if (STOADJ.gt.0.) then
        write(JOREGT,6011) TTOTTP, &
             & 'INGROWTH, PLANT, AND NATURAL ',KDT
     else
        write(JOREGT,6011) TTOTTP, &
             & 'PLANT AND NATURAL ',KDT
     end if
  else
     write(JOREGT,6011) TTOTTP,'INGROWTH ',KDT
  end if
  6011 format(F8.1,' ',A, &
       & 'TREES/ACRE ADDED IN THE FALL OF',I5)
  write(JOREGT,6029) (NSP(I,1),I = 1,NOFSPE),(TOTTPA(J),J = 1,NOFSPE)
  6029 format(/,'   SPECIES:',23(2X,A2,1X),/,'TREES/ACRE:',23F5.0,/)
  do M = 1,NOFSPE
     if (TOTTPA(M).gt.0) call DBSINGROW(KDT,TTOTTP,NSP(M,1), &
          & TOTTPA(M))
  end do
  259 continue
  260 continue
  !     PRINT PLOT SUMMARY TABLE IF REQUESTED.
  call DBCHK (DEBUG,'ESPSUM',6,ICYC)
  if (DEBUG) then
     write(JOREGT,6018) (NSP(I,1),I = 1,NOFSPE)
     6018 format('IN ESTAB (ESPSUM PLOT SUMMARY): '/ &
          & '     SLOPE ASPECT HAB. TOPO SITE       ', &
          & ' <------PLOT OVERSTORY DENSITY (SQ.FT./ACRE)----->',/, &
          & 'PLOT %/100 (DEG.) GRP. POS. PREP  P(S)  TOTAL ',23(A2,2X))
     do I = 1,NPTIDS
        NNID = IPTIDS(I)
        N = INT((PASP(NNID)*57.2958) + 0.5)
        NN = IPHYS(NNID)
        ITEMP = IPPREP(I*IDUP)
        write(JOREGT,6020) NNID,PSLO(NNID),N,IPHAB(NNID),CTOPO(NN), &
             & CPREP(ITEMP),PROB1(I*IDUP),BAAA(NNID),(OVER(J,NNID),J = 1,NOFSPE)
        6020 format(1X,I3,F6.2,I6,2X,I3,1X,2(1X,A4),F7.4,F6.0, &
             & (/,2X,11F4.0))
        221 continue
     end do
  end if
  KDTOLD = KDT
  MODE = 0
  return
end subroutine ESTAB
