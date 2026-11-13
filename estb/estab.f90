!=========================================!
!   Establish regeneration tree records   !
!=========================================!
! ESTB $Id$
subroutine ESTAB (KDT)
  implicit none

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'ESTREE.f90'
  include 'ESHAP.f90'
  include 'ESHAP2.f90'
  include 'PDEN.f90'
  include 'CALDEN.f90'
  include 'ESCOMN.f90'
  include 'ESCOM2.f90'
  include 'VARCOM.f90'
  include 'STDSTK.f90'

  !     REGENERATION ESTABLISHMENT MODEL   --   VERSION 2.0
  !     USDA FOREST SERVICE, INTERMOUNTAIN RESEARCH STATION
  !     1221 S. MAIN, MOSCOW, IDAHO  83843   (208) 882-3557

  !     THIS PROGRAM MODELS REGENERATION ESTABLISHMENT IN MONTANA AND
  !     IDAHO.  HABITAT TYPE SERIES = DF, GF, WRC, WH, AND SAF.

  !     CODE ALPHA SPECIES MIN.HT | NAME & CODE     IFO     | PHY.POS.
  !     ---- ----- ------- ------ | --------------- ---     | --------
  !       1    WP   PIMO   1.0 FT | BITTERROOT(103)  3      | 1=BOTTOM
  !       2    L    LAOC   1.0 FT | PANHANDLE (104)  4      | 2=LOWER
  !       3    DF   PSME   1.0 FT | CLEARWATER(105)  5      | 3=MID
  !       4    GF   ABGR   0.5 FT | COEUR D A.(106)  4      | 4=UPPER
  !       5    WH   TSHE   0.5 FT | COLVILLE  (621)  7      | 5=RIDGE
  !       6    C    THPL   0.5 FT | FLATHEAD  (110) 10
  !       7    LP   PICO   1.0 FT | KANIKSU   (113)  4 ->DEERLODGE(109) 9
  !       8    S    PIEN   0.5 FT | KOOTENAI  (114) 14 | GALLATIN (111) 11
  !       9    AF   ABLA   0.5 FT | LOLO      (116) 16 | HELENA   (112) 12
  !      10    PP   PIPO   1.0 FT | NEZPERCE  (117) 17 | PAYETTE  (412) 19
  !      11    MH   TSME   0.5 FT | ST. JOE   (118)  4-| BOISE    (402) 20

  external ESRANN
  ! Variables
  logical :: DEBUG
  integer :: K,L,JJ,IBRKUP,ISHADE,IPNSPE,IDO,ITP,NDRAW,NSPNZ,NEWTPP,M
  real    :: CW,CRDUM,XCSMAX,BRKUP,XXH,XH,TMTIME,SHADE,TREEHT,PTREE,ESAVE
  real    :: HHT,TRAGE,DILATE,DELAY,TPPLN,TPP,EMSQR,PN,BAAOLN,BAAOLD
  real    :: RADIAN,GENTIM,TBAAA,SUM1,SUM2,FTEMP2,FTEMP,SUM
  integer :: MXRR,NUM2PK,NBEST,NUMSPE,ITPP,ITIME,IEVTYR,IHTSER,IREP
  integer :: NTIMES,IPOLD,ITYPEP,IEND,ISTART,NCOUNT,IPYEAR,IACTK,NP
  integer :: IZERO,MYDO,II,ISER,J,NN,MATCH,ITODO,NTODO,IDUP,N,I,INGRO
  integer :: ITRNIN,NOFSPE,KDT
  real    :: BACHLO,DRAW,SHORTY,PNONE,DUPNPT,DUP,TTOTTP,TCROP1,TCROP2
  real    :: PASMAX,FLONPT,FLOKDT,ZHARV,SUMHTS(MAXSP)
  integer*4 :: IDCMP1,ITMP,ITEMP,ITOOMP
  character(len=4) :: CPREP(4)
  character(len=4) :: CTOPO(5)
  character(len=2) :: CBLANK
  character(len=2) :: CLAST(MAXSP)
  real    :: EXCESS(MAXSP),PRMS(6),SUMESP(MAXSP),SUMUP(MAXSP)
  integer :: MYTYPE(30),MYHABG(16),MYHTS(16),MYACTS(4)
  real    :: TOTTPA(MAXSP),TOTPCT(MAXSP),BESTPA(MAXSP)
  real    :: AVEHT(MAXSP),BESPCT(MAXSP)
  real    :: PASTPA(MAXSP),PASPCT(MAXSP),PSPE(6)
  integer :: ILSP(MAXSP),ICHOI(2,MAXSP),NULL(MAXSP)
  real    :: FIRST(2,MAXSP)
  integer :: NNPREP(4),MAXTPP(16),IBEST(MAXSP)
  real    :: TALL(MAXSP)
  integer :: ICODE(99)
  real    :: HEIGHT(99)
  integer :: NOTE(99)
  real    :: ESPROB(99)
  integer :: IASEP(99)
  real    :: HTIMLT(99),STOMLT(MAXSP)
  integer :: MAXSPP(16),IPLANT(MAXSP),MAXING(16)
  real    :: AGADSB(99),AGEPL(99),AGEXC(99),WTAVEHT
  character(len=47) :: HABTYP(16)
  character(len=7) :: SERIES(16)
  character(len=3) :: FINAL

  data CBLANK/'  '/,CPREP/'NONE','MECH','BURN','ROAD'/,CTOPO/&
       &'BOTM','LOWR',' MID','UPPR','RIDG'/,MYTYPE/9*1,2*5,2*2,3*3,4,&
       &12*5,1/,MAXTPP/9,7,5,5,10,8,9,5,21,25,10,10,11,7,10,8/,&
       &IDCMP1/10000000/,MYHABG/4*1,4*2,3,4,6*5/,MYHTS/1,3*2,4*3,2*4,&
       &6*5/,MAXSPP/4,3*3,5,4,6,4,2*6,4,2*5,4,6,4/,MYACTS/430,431,&
       &491,493/,NOFSPE/MAXSP/,MAXING/4,4,3,3,5,4,5,4,7,7,5,5,5,4,5,4/
  data HABTYP/&
       &'PSME/ VAGL, LIBO, VACA',&
       &'PSME/ CARU, CAGE, GRASS',&
       &'PSME/ PHMA, ACGL',&
       &'PSME/ SYAL, SPBE, SYOR, ARUV, ARCO',&
       &'ABGR/ LIBO, CLUN-XETE',&
       &'ABGR/ XETE, VAGL, COOC, VACA',&
       &'ABGR/ CLUN (EXCEPT CLUN-XETE)',&
       &'ABGR/ SPBE, ACGL, PHMA, ASCA, SETR',&
       &'THPL/ ALL',&
       &'TSHE/ ALL',&
       &'ABLA/ VAGL, VASC, VACA',&
       &'ABLA/ XETE, LIBO',&
       &'ABLA/ CLUN, GATR',&
       &'ABLA/ CAGE, CARU, ACGL, SPBE',&
       &'ABLA/ MEFE, ALSI, TSME/ CLUN, XETE, MEFE, STAM',&
       &'ABLA/ CACA, STAM, LUHI'/
  data SERIES/&
       &'DF     ',&
       &'DF     ',&
       &'DF     ',&
       &'DF     ',&
       &'GF     ',&
       &'GF     ',&
       &'GF     ',&
       &'GF     ',&
       &'C      ',&
       &'WH     ',&
       &'AF & MH',&
       &'AF & MH',&
       &'AF & MH',&
       &'AF & MH',&
       &'AF & MH',&
       &'AF & MH'/
  data FINAL/"ALL"/

  !     INITIALIZE ARRAYS AND VARIABLES

  call DBCHK (DEBUG,'ESTAB',5,ICYC)
  if (DEBUG) write(JOSTND,7000) KDT,IDSDAT,NTALLY
  7000 format(/'IN ESTAB; BEGIN DEBUG.  KDT=',I5,' IDSDAT=',I5,&
       &' NTALLY=',I2)
  ITRNIN = ITRN+1
  INGRO = 0
  ZHARV = FLOAT(IDSDAT)
  FLOKDT = FLOAT(KDT)
  TIME = FLOKDT - ZHARV +1.0
  FLONPT = FLOAT(NPTIDS)
  SUMPRB = 0.0
  ELEVSQ = ELEV*ELEV
  ITMP = ICYC
  ITMP = ITMP*10000
  MODE = 0
  if (KDT+1-IY(1).gt.20) then
     INADV = 1
     LOAD = 0
  end if
  PASMAX = CONFID
  do I = 1,99
     NOTE(I) = 0
     AGADSB(I) = 0.0
     AGEPL(I) = 0.0
     AGEXC(I) = 0.0

     40 continue
  end do
  do I = 1,2
     do N = 1,NOFSPE
        FIRST(I,N) = 0.1
     end do
  end do
  TCROP1 = 0.0
  TCROP2 = 0.0
  TTOTTP = 0.0
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

  !     PLOT REPLICATION

  DUP = 0.0
  do I = 1,MINREP
     DUP = DUP+1.0
     N = NPTIDS*I
     if (NPTIDS*(I+1).gt.MAXPLT) go to 139
     if (N.ge.MINREP) go to 139
     138 continue
  end do
  139 continue
  IDUP = INT(DUP + 0.5)
  DUPNPT = FLONPT*DUP

  !     CHANGE FOREST CODES

  do I = 1,20
     if (KODFOR.ne.IFORCD(I)) go to 4
     IFO = IFORST(I)
     go to 5
     4 continue
  end do
  STOADJ = 0.0
  IFO = 4
  5 continue

  !     IF NOT THE FIRST TALLY, CANCEL MECH & BURN PREPS IN THIS CYCLE.

  if (NTALLY.gt.1) then
     call OPFIND (2,MYACTS(3),NTODO)
     if (NTODO.gt.0) then
        do ITODO = 1,NTODO
           call OPDEL1 (ITODO)
           6 continue
        end do
     end if
  end if

  !     DO NOT CREATE TREE RECORDS THAT WOULD ESSENTIALLY BE REDUNDANT
  !     WITH THOSE IN THE INVENTORY WHEN NTALLY IS 1 AND 20 YEARS HAS
  !     ELAPSED SINCE THE INVENTORY (I.E. NO CALIBRATION).

  MATCH = 0
  if (NTALLY.eq.1.and.KDT+1-IY(1).gt.20) MATCH = 1

  !     WORK WITH INGROWTH (99) OPTION

  if (NTALLY.eq.99) then
     PNONE = 1.0
     PMECH = 0.0
     PBURN = 0.0
     METH = 0
     LOAD = 0
     IALN(2) = 1
     INGRO = 1
     PASMAX = 15.0
     ITEMP = INT(((XTES/THRES1) *FINT) + 0.5)
     if (ITEMP.lt.1) ITEMP = 1
     SHORTY = FLOAT(ITEMP)
     ZHARV = FLOAT(KDT+1) -SHORTY
     ZMECH = ZHARV
     ZBURN = ZHARV
     NTALLY = 1
     if (DEBUG)write(JOSTND,*)'SHORTY, XTES, THRES1, ITEMP= ',&
          &SHORTY, XTES, THRES1, ITEMP
     go to 44
  end if

  !     IF NOT THE FIRST TALLY, THEN SKIP SITE PREPS.

  if (NTALLY.ne.1) go to 276
  call ESETPR (METH,ZMECH,ZBURN,PNONE,PMECH,PBURN,IALN,&
       &IDSDAT,KDT,LOAD)
  if (ZMECH.lt.ZHARV) ZMECH = ZHARV
  if (ZBURN.lt.ZHARV) ZBURN = ZHARV
  if (DEBUG) write(JOSTND,7011) METH,IDSDAT,KDT,ZMECH,ZBURN,&
       &PNONE,PMECH,PBURN,(IALN(I),I = 1,3)
  7011 format('AFTER ESETPR: METH=',I2,' IDSDAT=',I5,' KDT=',I5,&
       &' ZMECH=',F7.1,' ZBURN=',F7.1,/,4X,'PNONE=',F7.4,' PMECH=',&
       &F7.4,' PBURN=',F7.4,3X,'IALN(I)=',3I5)
  44 continue
  KDTOLD = INT(ZHARV-0.5)
  do I = 1,5
     SUMPRE(I) = 0.0
     45 continue
  end do
  do I = 1,16
     IHABT(I) = 0
     46 continue
  end do
  do I = 1,NPTIDS*IDUP
     ESB1(I) = 0.0
     PNN(I) = 0.0
     PLPROB(I) = 0.0
     NSTORE(I) = 0
     12 continue
  end do
  ESB = 0.0
  276 continue
  if (NTALLY.eq.1) then
     call ESRANN (DRAW)
     ITEMP = INT(DRAW*100000.0 + 0.5)
     ESDRAW = FLOAT(ITEMP)
  end if
  call ESRNSD (.true.,ESDRAW)
  if (DEBUG) write(JOSTND,7014) ESDRAW
  7014 format('RANDOM NUMBER SEED=',F9.2)

  !     LOAD REGENERATION VECTORS FROM CURRENT INVENTORY.

  SUM = 0.0
  if (ITRN.eq.0) go to 198
  do I = 1,ITRN
     FTEMP = DBH(I)
     if (FTEMP.ge.REGNBK) go to 177
     NN = ISP(I)
     if (NN.lt.1.or.NN.gt.NOFSPE) go to 177
     FTEMP2 = PROB(I)
     PASTPA(NN) = PASTPA(NN)+FTEMP2
     TCROP2 = TCROP2 +FTEMP2
     SUM = SUM+FTEMP2
     N = ITRE(I)
     PLPROB(N) = PLPROB(N) +(FTEMP2/DUP)
     177 continue
  end do
  198 continue

  !     CALCULATE INTERCEPT OF ACTUAL STOCKING FROM THE INVENTORY.

  if (INADV.eq.1.or.NTALLY.ne.1) go to 64
  TPACRE = SUM
  if (TPACRE.lt.1.0) TPACRE = 1.0
  ESA = 1.0/(1.0+EXP(-1.0*(-5.17397 +0.85131*ALOG(TPACRE))))
  FTEMP = ESA
  if (FTEMP.lt.0.10) FTEMP = 0.10
  if (FTEMP.gt.0.90) FTEMP = 0.90
  ESB = -1.0 * ALOG(1.0/FTEMP -1.0)
  if (DEBUG) write(JOSTND,7030) ESA
  7030 format('IN CALIBRATION LOGIC; ESA=',F9.4)
  64 continue

  !     WORK WITH SITE PREPARATION OPTIONS. EXIT = STMT 242

  do I = 1,IDUP*NPTIDS
     call ESRANN (DRAW)
     WK6(I) = DRAW
     183 continue
  end do
  if (NTALLY.ne.1) go to 242
  if (LOAD.ne.1) go to 249

  !     IF LOAD=1, REP PLOT PREPS; IF NEEDED COMPRESS ARRAY IPPREP.

  NN = 0
  do I = 1,NPTIDS
     NN = NN+1
     N = IPTIDS(I)
     IPPREP(NN) = IPPREP(N)
     243 continue
  end do
  if (IDUP.lt.2) go to 49
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
  if (IALN(2).eq.0.and.IALN(3).eq.0) go to 246
  SUM = PMECH+PBURN
  if (SUM.le.1.0) go to 248
  PMECH = PMECH/SUM
  PBURN = PBURN/SUM
  248 continue
  PNONE = 1.0-PMECH-PBURN
  go to 252
  246 continue

  !     USER HAS NOT SUPPLIED ANY SITE PREPARATION KEYWORDS.

  ISER = MYTYPE(ITYPE)
  call ESPREP (ISER,PNONE,PMECH,PBURN)
  252 continue

  !     SECTION TO CHOOSE SITE PREPS.  SAMPLE WITHOUT REPLACEMENT.

  SUM = PNONE+PMECH+PBURN
  SUMUP(1) = PNONE/SUM
  SUMUP(2) = PMECH/SUM
  SUMUP(3) = PBURN/SUM
  if (DEBUG) write(JOSTND,7051) (SUMUP(I),I = 1,3)
  7051 format('PNONE=',F8.4,'  PMECH=',F8.4,'  PBURN=',F8.4)
  N = 0
  do II = 1,IDUP
     do NN = 1,NPTIDS
        N = N+1
        DRAW = WK6(N)
        DRAW = DRAW* (((DUPNPT+1.0)-FLOAT(N))/DUPNPT)
        SUM = 0.0
        do I = 1,2
           SUM = SUM+SUMUP(I)
           if (DRAW.gt.SUM) go to 2
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
  if (DEBUG) write (JOSTND,'(''# OF PLANT & NATURAL='',I4)') NTODO
  if (NTODO.gt.0) MODE = 1

  !  IF NUMBER TO ESTABLISH IS NEAR ZERO, THEN MARK KEYWORD FOR DELETION.
  !  IF THIS WAS THE ONLY ACTIVITY FOUND, SET MODE FLAG TO 0.
  !  GED 10-23-97

  MYDO = NTODO
  do IZERO = 1,NTODO
     call OPGET (IZERO,6,IPYEAR,IACTK,NP,PRMS)
     if (PRMS(2).le.0.001.or. PRMS(3).le.0.001) then
        call OPDEL1(IZERO)
        MYDO = MYDO-1
     end if
     244 continue
  end do
  if (MYDO .eq.0) MODE = 0

  !     ACCUMULATE SUMS FOR CALCULATION OF SHADE ADJUSTMENTS

  SUM1 = 0.
  SUM2 = 0.
  do NN = 1,NPTIDS
     NNID = IPTIDS(NN)
     TBAAA = BAAA(NNID)
     if (TBAAA .lt.1.)TBAAA = 1.
     SUM1 = SUM1+TBAAA
     245 continue
  end do
  do NN = 1,NPTIDS
     NNID = IPTIDS(NN)
     TBAAA = BAAA(NNID)
     if (TBAAA .lt.1.)TBAAA = 1.
     SUM2 = SUM2+SUM1/TBAAA
     if (DEBUG)&
          &write(JOSTND,*)'NNID,BAAA,TBAAA,SUM1,SUM2= ',&
          &NNID,BAAA(NNID),TBAAA,SUM1,SUM2
     2451 continue
  end do
  if (DEBUG)write(JOSTND,*)'SUM1,SUM2= ',SUM1,SUM2

  !     BEGIN PROCESSING OF PLOTS                             HERE WE GO

  NCOUNT = 0
  GENTIM = FINT-5.0
  if (GENTIM.lt.0.0) GENTIM = 0.0
  do NN = 1,NPTIDS
     do I = 1,4
        NNPREP(I) = 0
        38 continue
     end do
     ISTART = NN*IDUP -IDUP+1
     IEND = NN*IDUP
     do I = ISTART,IEND
        N = IPPREP(I)
        if (N.lt.1.or.N.gt.4) N = 1
        NNPREP(N) = NNPREP(N)+1
        39 continue
     end do

     !     PROCESS REPLICATED PLOTS BY EACH OF THE 4 SITE PREPS

     do ITYPEP = 1,4
        IPOLD = 0
        if (NNPREP(ITYPEP).lt.1) go to 202
        NTIMES = NNPREP(ITYPEP)
        do IREP = 1,NTIMES
           NCOUNT = NCOUNT+1
           IPREP = ITYPEP
           if (IPREP.eq.IPOLD) go to 137
           IPOLD = IPREP
           NNID = IPTIDS(NN)
           PROB1(NCOUNT) = 0.00001
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
           if (BAAOLD.lt.1.0) BAAOLD = 1.0
           if (BAAOLD.gt.400.0) BAAOLD = 400.0
           BAAOLN = ALOG(BAAOLD)
           IHAB = IPHAB(NNID)
           ISER = MYHABG(IHAB)
           IHTSER = MYHTS(IHAB)
           if (DEBUG) then
              write(JOSTND,6033) NNID,NOFSPE,IPREP,NTALLY,ISER,IFO,&
                   &SLO,BAA,ELEV,ASPECT
              6033 format(/,72('='),/,'PLOT=',I4,'  NOFSPE=',I4,'  IPREP=',I3,&
                   &'  NTALLY=',I3,'  ISER=',I3,/,'IFO=',I3,&
                   &'  SLO=',F5.2,'  BAA=',F7.2,'  ELEV=',F6.1,'  ASPECT=',F7.3)
              write(JOSTND,*) 'OCURHT = ',(OCURHT(IHAB,I),I = 1,NOFSPE)
              write(JOSTND,*) 'OCURNF = ',(OCURNF(IFO,I),I = 1,NOFSPE)
              write(JOSTND,*) 'XESMLT = ',(XESMLT(I),I = 1,NOFSPE)
           end if
           if (STOADJ.lt.0.0001) go to 137

           !     CALCULATE P(STOCKING) AT INVENTORY OR HARVEST, WHICHEVER CAME
           !     LAST. IF ADVANCE COMPONENT OF THE INVENTORY IS INVALID (INADV=1)
           !     OR THIS IS NOT THE FIRST TALLY OF REGEN, BYPASS PROCEDURES TO
           !     CORRECT ACTUAL (INVENTORY) VS. PREDICTED (MODEL) INTERCEPT.

           if (INADV.eq.1.or.NTALLY.ne.1) go to 241
           FTEMP = FLOAT(IY(1))
           if (FTEMP.lt.ZHARV) FTEMP = ZHARV
           if (IPREP.eq.2) then
              TIME = FTEMP -ZMECH
              IEVTYR = IFIX(ZMECH)
           else if (IPREP.eq.3) then
              TIME = FTEMP -ZBURN
              IEVTYR = IFIX(ZBURN)
           else
              TIME = FTEMP  -ZHARV
              IEVTYR = IDSDAT
           end if
           if (TIME.lt.0.0) TIME = 0.0

           !     DEFINE WSBW VARIABLES AT TIME OF INVENTORY OR HARVEST.
           !     ITEMP IS # YEARS FROM LAST DISTURBANCE TO INVENTORY.

           ITIME = INT(TIME+0.5)
           if (DEBUG) write (JOSTND,7010) TIME,IEVTYR
           7010 format('TIME=',F7.1,'; IEVTYR=',I5)
           call ESTIME (IEVTYR,KDT)
           if (DEBUG) write (JOSTND,7020) BWB4,BWAF,REGT,SQREGT,SQBWAF
           7020 format('BWB4=',F7.3,'  BWAF=',F7.3,'  REGT=',F7.3,'  SQREGT=',&
                &F7.3,'  SQBWAF=',F7.3)
           call ESTOCK (ELEV,IFO,BAAOLD,BAAOLN,PN)
           ESB1(NCOUNT) = PN
           if (DEBUG) write (JOSTND,6031) PN
           6031 format('PN FOR STOCKING= ',F10.4)
           FTEMP = 1.0/(1.0+EXP(-PN))
           ITPP = INT((PLPROB(NNID)*DUPNPT)/(FTEMP*300.0) + 0.5)
           if (DEBUG) write(JOSTND,7021) ITPP
           7021 format('CALCULATION OF ITPP AT INVENTORY=',I10)
           NSTORE(NCOUNT) = ITPP
           PNN(NCOUNT) = ESA
           if (NTIMES.gt.1) then
              do I = 1,NTIMES-1
                 NSTORE(NCOUNT+I) = ITPP
                 PNN(NCOUNT+I) = ESA
                 31 continue
              end do
           end if
           241 continue

           !     CHOOSE TIME INTERVAL FOR THE PLOT.

           if (IPREP.eq.2) then
              IEVTYR = IFIX(ZMECH)
              TIME = FLOKDT +1.0 -ZMECH
           else if (IPREP.eq.3) then
              IEVTYR = IFIX(ZBURN)
              TIME = FLOKDT +1.0 -ZBURN
           else
              IEVTYR = IDSDAT
              TIME = FLOKDT +1.0 -ZHARV
           end if
           if (DEBUG) write (JOSTND,7010) TIME,IEVTYR
           call ESTIME (IEVTYR,KDT)
           if (DEBUG) write (JOSTND,7020) BWB4,BWAF,REGT,SQREGT,SQBWAF

           !     CALCULATE P(STOCKING) AT THE END OF THE CYCLE

           call ESTOCK (ELEV,IFO,BAA,BAALN,PN)
           if (DEBUG) write (JOSTND,6031) PN

           !     CORRECT FOR ACTUAL VS. PREDICTED REGENERATION. ESB IS FROM THE
           !     INVENTORY. ESB1 IS PREDICTED AT INVENTORY.

           if (STOADJ.lt.0.001) STOADJ = 0.001
           FTEMP = 1.0/(1.0+EXP(-(PN+ESB-ESB1(NCOUNT) )))
           FTEMP = FTEMP *STOADJ
           if (FTEMP.lt.0.0001) FTEMP = 0.0001
           if (FTEMP.gt.0.9990) FTEMP = 0.9990
           if (FTEMP.lt.PNN(NCOUNT)) FTEMP = PNN(NCOUNT) +0.0001
           PROB1(NCOUNT) = FTEMP
           if (DEBUG) write (JOSTND,6028) ESB,ESB1(NCOUNT),TPACRE,FTEMP
           6028 format('ESB(ACT)=',F7.4,'  ESB1(PRED)=',F7.4,'  TPACRE=',F7.1,&
                &'      PROB1=',F7.4)
           if (INGRO.eq.1.or.MATCH.eq.1) then
              ITPP = INT((PLPROB(NNID)*DUPNPT)/(FTEMP*300.0) + 0.5)
              NSTORE(NCOUNT) = ITPP
           end if
           if (NTIMES.gt.1) then
              do I = 1,NTIMES-1
                 PROB1(NCOUNT+I) = FTEMP
                 ESB1(NCOUNT+I) = ESB1(NCOUNT)
                 if (INGRO.eq.1.or.MATCH.eq.1) NSTORE(NCOUNT+I) = ITPP
                 30 continue
              end do
           end if

           !     STATEMENT 137 IS USED WHEN BYPASSING PLOT REPS

           137 continue

           !     CALCULATE PROB OF ADV, SUBS, AND EXCESS SPECIES AT 10 YEARS.

           TIME = 10.0
           if (INGRO.eq.1) TIME = SHORTY
           ITIME = INT(TIME+0.5)
           call ESTIME (IDSDAT,(IDSDAT+ITIME))
           do I = 1,NOFSPE
              PADV(I) = 0.0
              PSUB(I) = 0.0
              PXCS(I) = 0.0
              115 continue
           end do
           call ESPADV
           if (ITIME.gt.2) call ESPSUB
           call ESPXCS
           if (DEBUG) then
              write(JOSTND,7010) TIME
              write(JOSTND,7020) BWB4,BWAF,REGT,SQREGT,SQBWAF
              write(JOSTND,7002) (PADV(I),I = 1,NOFSPE)
              7002 format('PADV=',15F6.3)
              write(JOSTND,7003) (PSUB(I),I = 1,NOFSPE)
              7003 format('PSUB=',15F6.3)
              write(JOSTND,7005) (PXCS(I),I = 1,NOFSPE)
              7005 format('PXCS=',15F6.3)
           end if

           !     RESET TIME TO YEARS SINCE DISTURBANCE OR SITE PREP.

           if (IPREP.eq.2) then
              IEVTYR = IFIX(ZMECH)
              TIME = FLOKDT +1.0 -ZMECH
           else if (IPREP.eq.3) then
              IEVTYR = IFIX(ZBURN)
              TIME = FLOKDT +1.0 -ZBURN
           else
              IEVTYR = IDSDAT
              TIME = FLOKDT + 1.0 - ZHARV
           end if
           if (DEBUG) write (JOSTND,7010) TIME,IEVTYR
           call ESTIME (IEVTYR,KDT)
           if (DEBUG) write (JOSTND,7020) BWB4,BWAF,REGT,SQREGT,SQBWAF
           if (DEBUG)write(JOSTND,*)'IPREP,IEVTYR,FLOKDT,ZMECH,ZBURN,ZHARV='
           if (DEBUG)write(JOSTND,*)IPREP,IEVTYR,FLOKDT,ZMECH,ZBURN,ZHARV
           call ESRANN (DRAW)
           EMSQR = 1.0
           if (DRAW.lt.0.5) EMSQR = -1.0
           call ESRANN (DRAW)
           EMSQR = EMSQR*DRAW
           if (DEBUG) write (JOSTND,7015) EMSQR
           7015 format('EMSQR=',F10.3)
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
              do I = 1,MAXTPP(IHAB)*2
                 call ESRANN (DRAW)
                 75 continue
              end do
              go to 163
           end if

           !     CHOOSE THE NUMBER OF TREES ON A STOCKED PLOT.

           call ESRANN (DRAW)
           if (NTALLY.eq.1) XSTORE(NCOUNT) = DRAW
           DRAW = XSTORE(NCOUNT)
           call ESTPP (DRAW,TPP)
           ITPP = INT(TPP + 0.5)
           if (ITPP.lt.1) ITPP = 1
           if (ITPP.gt.MAXTPP(IHAB)) ITPP = MAXTPP(IHAB)
           if (INGRO.eq.1.and.ITPP.gt.MAXING(IHAB)) ITPP = MAXING(IHAB)
           NEWTPP = ITPP-NSTORE(NCOUNT)
           if (NEWTPP.lt.0) NEWTPP = 0
           NSTORE(NCOUNT) = ITPP
           TPP = FLOAT(ITPP)
           TPPLN = ALOG(TPP)
           if (DEBUG) write (JOSTND,7009) ITPP,NEWTPP,PLPROB(NNID)
           7009 format('TREES/PLOT=',I5,'  NEW TREES/PLOT=',I5,'  PLPROB=',F8.3)

           !     DETERMINE NUMBER OF SPECIES ON THE PLOT.

           if (DEBUG) write (JOSTND,7010) TIME
           do I = 1,6
              PSPE(I) = 0.0
              call ESRANN (DRAW)
              WK6(I) = DRAW
              15 continue
           end do
           NUMSPE = 1
           if (ITPP.eq.1) go to 19
           call ESNSPE (PSPE,TPPLN,ISER,ITPP,TPP)
           SUM = PSPE(1)
           do I = 2,6
              SUM = SUM+PSPE(I)
              16 continue
           end do
           SUMUP(1) = PSPE(1)/SUM
           do I = 2,5
              SUMUP(I) = SUMUP(I-1) + PSPE(I)/SUM
              17 continue
           end do
           do I = 1,5
              DRAW = WK6(I)
              if (DRAW.gt.SUMUP(I)) go to 18
              NUMSPE = I
              go to 19
              18 continue
           end do
           NUMSPE = 6
           19 continue
           if (NUMSPE.gt.MAXSPP(IHAB)) NUMSPE = MAXSPP(IHAB)
           if (DEBUG) write (JOSTND,7001) (PSPE(I),I = 1,6),NUMSPE
           7001 format('P(1-6 SPECIES)',6F6.3,'; #SPECIES CHOSEN=',I3)

           !     CHOOSE THE SPECIES TO OCCUPY THE PLOT

           SUM = 0.0
           NSPNZ = 0
           do I = 1,NOFSPE
              IBEST(I) = 0
              FTEMP = PADV(I)+PSUB(I)
              if (FTEMP.gt.0.0001) NSPNZ = NSPNZ+1
              SUM = SUM+FTEMP
              SUMUP(I) = FTEMP
              56 continue
           end do
           do I = 1,NOFSPE
              if (SUM .gt.0.)SUMUP(I) = SUMUP(I)/SUM
              57 continue
           end do
           if (DEBUG) write (JOSTND,7003) (SUMUP(I),I = 1,NOFSPE)
           if (NUMSPE.gt.NSPNZ) NUMSPE = NSPNZ
           do I = 1,6
              call ESRANN (DRAW)
              WK6(I) = DRAW
              120 continue
           end do
           do I = 1,NUMSPE
              SUM = 0.0
              DRAW = WK6(I)
              do J = 1,NOFSPE-1
                 SUM = SUM+SUMUP(J)
                 if (DRAW.gt.SUM) go to 59
                 SUMUP(J) = 0.0
                 IBEST(J) = 1
                 go to 58
                 59 continue
              end do
              IBEST(NOFSPE) = 1
              SUMUP(NOFSPE) = 0.0
              58 continue
              SUM = 0.0
              do N = 1,NOFSPE
                 SUM = SUM+SUMUP(N)
                 47 continue
              end do
              if (SUM.lt.0.0001) go to 60
              do N = 1,NOFSPE
                 if (SUM .gt.0.)SUMUP(N) = SUMUP(N)/SUM
                 61 continue
              end do
              60 continue
           end do
           if (DEBUG) write (JOSTND,7004) (IBEST(I),I = 1,NOFSPE)
           7004 format('ARRAY IBEST ',20I5)

           !     DETERMINE ADV/SUBS STATUS OF THE FIRST TREE OF EACH SPECIES.

           do I = 1,NOFSPE
              PADV(I) = 0.0
              PSUB(I) = 0.0
           end do
           if (NTALLY.eq.1) call ESPADV
           call ESPSUB
           do I = 1,2
              do N = 1,NOFSPE
                 ICHOI(I,N) = 0
              end do
           end do
           do I = 1,NOFSPE
              call ESRANN (DRAW)
              if (IBEST(I).ne.1) go to 63
              SUM = PADV(I)+PSUB(I)
              FTEMP = PADV(I)/SUM
              J = 1
              if (DRAW.gt.FTEMP) J = 2
              ICHOI(J,I) = 1
              63 continue
           end do

           !     ASSIGN HEIGHTS TO TALLEST TREE BY SPECIES & ADVANCE/SUBS.

           TIME = FLOKDT-FLOAT(KDTOLD)
           IEVTYR = KDTOLD+1
           if (DEBUG) write (JOSTND,7010) TIME,IEVTYR
           call ESTIME (IEVTYR,KDT)
           if (DEBUG) write (JOSTND,7020) BWB4,BWAF,REGT,SQREGT,SQBWAF
           do I = 1,NOFSPE*2
              call ESRANN (DRAW)
              WK6(I) = DRAW
              122 continue
           end do
           NDRAW = 0
           do I = 1,NOFSPE
              NDRAW = NDRAW+1
              TALL(I) = 0.001
              STOMLT(I) = 1.0
              if (ICHOI(1,I).ne.1) go to 95
              DRAW = WK6(NDRAW)
              call ESDLAY (I,1,DRAW,DELAY)
              DILATE = FIRST(1,I)
              TRAGE = 0.0
              call ESADVH (EMSQR,I,HHT,DELAY,ELEV,DILATE,IHTSER,GENTIM,TRAGE)
              if (DEBUG) write (JOSTND,7007) DELAY,I,GENTIM,TRAGE
              7007 format('DELAY TO GERM=',F10.4,'  SPECIES=',I5,&
                   &'  GENTIM=',F5.2,'  TRAGE=',F5.1)
              TALL(I) = HHT
              AGADSB(I) = TRAGE
              FTEMP = TRAGE
              if (FTEMP.gt.GENTIM) FTEMP = GENTIM
              STOMLT(I) = FTEMP/(GENTIM+0.0001)
              FIRST(1,I) = SQRT(DILATE)
              95 continue
              if (ICHOI(2,I).ne.1) go to 99
              NDRAW = NDRAW+1
              DRAW = WK6(NDRAW)
              call ESDLAY (I,2,DRAW,DELAY)
              DILATE = FIRST(2,I)
              TRAGE = 0.0
              !     PROVIDE AN INITIAL VALUE OF HHT AS IT IS USED IN SOME VARIANTS PRIOR TO BEING SET
              HHT = 1.
              call ESSUBH (I,HHT,EMSQR,DILATE,DELAY,ELEV,IHTSER,GENTIM,TRAGE)
              if (DEBUG) write (JOSTND,7007) DELAY,I,GENTIM,TRAGE
              TALL(I) = HHT
              AGADSB(I) = TRAGE
              FTEMP = TRAGE
              if (FTEMP.gt.GENTIM) FTEMP = GENTIM
              STOMLT(I) = FTEMP/(GENTIM+0.0001)
              FIRST(2,I) = SQRT(DILATE)
              99 continue
           end do
           do I = 1,NOFSPE
              if (IBEST(I).ne.1) go to 114
              TALL(I) = TALL(I)+HTADJ(I)
              if (TALL(I)-XMIN(I).lt.0.2) TALL(I) = XMIN(I)+0.2
              if (TALL(I).gt.HHTMAX(I)) TALL(I) = HHTMAX(I)
              114 continue
           end do
           if (DEBUG) then
              write (JOSTND,7008) (TALL(I),I = 1,NOFSPE)
              7008 format('TALLEST TREE',T14,20F5.2)
              write (JOSTND,7016) (HTADJ(I),I = 1,NOFSPE)
              7016 format('ARRAY HTADJ',T14,20F5.2)
              write (JOSTND,7017) (ICHOI(1,I),I = 1,NOFSPE)
              7017 format('ICHOI (ADV)',T14,20I5)
              write (JOSTND,7018) (ICHOI(2,I),I = 1,NOFSPE)
              7018 format('      (SUB)',T14,20I5)
              write (JOSTND,7019) (STOMLT(I),I = 1,NOFSPE)
              7019 format('ARRAY STOMLT',T14,20F5.2)
           end if

           !     MAKE SURE THERE IS A PROB FOR EXCESS TREES IF THERE IS A BEST
           !     TREE OF THE SAME SPECIES ON THE PLOT.

           do I = 1,NOFSPE
              WK6(I) = PXCS(I) *FLOAT(IBEST(I))
              if (IBEST(I).ne.1) go to 141
              if (WK6(I).lt.0.0001) WK6(I) = 0.0001
              141 continue
           end do
           SUM = 0.0
           do I = 1,NOFSPE
              SUM = SUM+WK6(I)
              140 continue
           end do
           if (SUM .gt.0.)SUMUP(1) = WK6(1)/SUM
           do I = 2,NOFSPE-1
              if (SUM .gt.0.) then
                 SUMUP(I) = SUMUP(I-1) + WK6(I)/SUM
              else
                 SUMUP(I) = SUMUP(I-1)
              end if
              142 continue
           end do
           if (DEBUG) write (JOSTND,7005) (SUMUP(I),I = 1,NOFSPE)
           ITP = ITPP
           J = ITPP+NOFSPE
           do I = 1,J
              NOTE(I) = 0
              ICODE(I) = 0
              IASEP(I) = 0
              HEIGHT(I) = 0.0
              ESPROB(I) = 0.0
              HTIMLT(I) = 1.0
              144 continue
           end do

           !     ASSIGN SPECIES & HEIGHT TO FIRST TREE OF EACH SPECIES

           N = 0
           do I = 1,NOFSPE
              NULL(I) = 0
              ILSP(I) = 0
              if (IBEST(I).eq.0) go to 146
              N = N+1
              ICODE(N) = I
              IASEP(N) = 2
              if (ICHOI(1,I).eq.1) IASEP(N) = 1
              HEIGHT(N) = TALL(I)
              HTIMLT(N) = STOMLT(I)
              146 continue
           end do

           !     ASSIGN SPECIES AND HEIGHTS TO 'EXCESS' TREES.  HTS MUST BE
           !     BETWEEN THE MINIMUM ESTABLISHMENT HEIGHT FOR THAT SPECIES AND
           !     THE TALLEST TREE OF THE SAME SPECIES.  EXIT=STMT 156

           do I = 1,MAXTPP(IHAB)*2
              call ESRANN (DRAW)
              WK6(I) = DRAW
              123 continue
           end do
           NDRAW = 0
           ITEMP = NUMSPE+1
           if (ITEMP.gt.ITPP) go to 158
           do N = ITEMP,ITPP
              NDRAW = NDRAW+1
              DRAW = WK6(NDRAW)
              do I = 1,NOFSPE-1
                 if (DRAW.gt.SUMUP(I)) go to 148
                 II = I
                 go to 150
                 148 continue
              end do
              II = NOFSPE
              150 continue
              ICODE(N) = II
              IASEP(N) = 3
              NDRAW = NDRAW+1
              DRAW = WK6(NDRAW)
              FTEMP = FLOKDT-FLOAT(KDTOLD)
              call ESXCSH (TALL(II),XMIN(II),FTEMP,II,DRAW,HHT)
              HEIGHT(N) = HHT+HTADJ(II)
              AGEXC(N) = FTEMP-DELAY

              if (HEIGHT(N).lt.XMIN(II)) HEIGHT(N) = XMIN(II)
              if (HEIGHT(N).gt.HHTMAX(II)) HEIGHT(N) = HHTMAX(II)
              HTIMLT(N) = STOMLT(I)

              if (DEBUG)write(JOSTND,*)'N, ITEMP, ITPP, HHT, FTEMP= ',&
                   &N,ITEMP,ITPP,HHT,FTEMP

              156 continue
           end do
           158 continue
           FTEMP = PROB1(NCOUNT)
           FTEMP2 = FLOAT(NEWTPP)/FLOAT(ITPP)
           ITEMP = ITPP-NEWTPP
           do I = 1,ITPP
              ESPROB(I) = FTEMP
              if (I.lt.ITEMP+1) ESPROB(I) = FTEMP-PNN(NCOUNT)
              if (INGRO.eq.1.or.MATCH.eq.1) ESPROB(I) = FTEMP*FTEMP2
              if (ESPROB(I).lt.0.0001) ESPROB(I) = 0.0001
              55 continue
           end do
           PNN(NCOUNT) = FTEMP

           !     STATEMENT 163 IS USED WHEN STOADJ .LE. 0.0

           163 continue
           if (STOADJ.lt.0.0001) then
              ITPP = 0
              TPPLN = 0.0
              TPP = 0.0
              NEWTPP = 0
           end if

           !     CREATE TREES FROM 'PLANT' AND 'NATURAL' KEYWORDS.  EXIT=STMT 321

           call ESRANN (DRAW)
           ITEMP = INT(DRAW*100000.0 + 0.5)
           ESAVE = FLOAT(ITEMP)
           if (MODE.ne.1) go to 321
           ITODO = 0
           call OPFIND (2,MYACTS,NTODO)
           if (DEBUG) write (JOSTND,'(''# PLANT & NATURAL 322='',I4)') NTODO
           ITOOMP = 0
           do IDO = 1,NTODO
              call OPGET (IDO,6,IPYEAR,IACTK,NP,PRMS)
              if (IACTK .le.0) go to 322
              ITODO = ITODO+1
              if (ITPP+ITODO.gt.99) then
                 ITOOMP = ITOOMP+1
                 call OPDEL1(IDO)
                 ITODO = ITODO-1
                 go to 322
              end if
              IPNSPE = IFIX(PRMS(1))
              PTREE = (PRMS(2)*(PRMS(3)/100.0)) /DUPNPT
              TRAGE = PRMS(4)
              if (TRAGE.lt.0.5) TRAGE = 2.0
              if (TRAGE.gt.10.0) TRAGE = 10.0
              AGEPL(ITODO) = TRAGE
              DELAY = FLOAT(IPYEAR) -(KDT+1-FINT)
              TREEHT = PRMS(5)
              ISHADE = IFIX(PRMS(6))
              SHADE = 0.0
              if (ISHADE.eq.1) SHADE = 1.0
              if (ISHADE.eq.2) SHADE = -1.0
              FTEMP = BAA
              !      IF(FTEMP.LT.1.5) FTEMP=0.0
              if (FTEMP.lt.1.0) FTEMP = 1.0
              if (DEBUG) write(JOSTND,7048) PTREE,SHADE,FTEMP,DUPNPT,BA,TRAGE
              7048 format('PTREE=',F6.1,'  SHADE=',F4.1,'  FTEMP=',F6.1,&
                   &'  DUPNPT=',F4.1,'  BA=',F6.1,'  TRAGE=',F4.1)

              ! THE ORIGINAL SHADE CODE ADJUSTMENT SHOWN HERE WAS NOT HAVING MUCH EFFECT.
              ! REPLACED BY A STRAIGHT RATIO OF BA ON THE PLOT BY DIXON 5/22/08
              ! NOTE CHANGED SETTING OF FTEMP, AND ACCUMULATIONS OF SUMS, ABOVE

              !      IF(BA.GT.10.0) PTREE=PTREE +2.0*(PTREE*SHADE*(((FTEMP/DUPNPT)/BA)
              !     &  -(1.0/DUPNPT) ) )
              if (ISHADE.eq.1) then
                 PTREE = PTREE*NPTIDS*FTEMP/SUM1
              else if (ISHADE.eq.2) then
                 PTREE = PTREE*NPTIDS*(SUM1/FTEMP)/SUM2
              end if
              if (DEBUG)write(JOSTND,*)'NNID,FTEMP,SUM1,SUM2,PTREE= ',&
                   &NNID,FTEMP,SUM1,SUM2,PTREE

              DILATE = FIRST(2,IPNSPE)
              TMTIME = TIME
              TIME = FINT
              !     PROVIDE AN INITIAL VALUE OF HHT AS IT IS USED IN SOME VARIANTS PRIOR TO BEING SET
              HHT = 1.
              call ESSUBH (IPNSPE,HHT,EMSQR,DILATE,DELAY,ELEV,IHTSER,GENTIM,&
                   &TRAGE)
              TIME = TMTIME
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
              if (DEBUG) write(JOSTND,7050) IPNSPE,HHT,PTREE,ITODO,ITPP,IACTK,&
                   &TRAGE
              7050 format('IPNSPE=',I3,'  HHT=',F6.2,'  PTREE=',F7.2,'  ITODO=',I4,&
                   &'  ITPP=',I4,'  IACTK=',I4,'  TRAGE=',F4.1)
              ICODE(ITPP+ITODO) = IPNSPE
              IASEP(ITPP+ITODO) = 4
              if (IACTK.eq.430) IASEP(ITPP+ITODO) = 3
              HEIGHT(ITPP+ITODO) = HHT
              ESPROB(ITPP+ITODO) = PTREE

              !     FOR PLANTED TREES, DELAY IS THE NUMBER OF YEARS BETWEEN THE START
              !     OF THE PROJECTION CYCLE AND THE PLANTING OCCURRANCE (0 to FINT-1)
              !     RESET GENTIM ACCORDINGLY. GED  03-27-09

              if (FINT-DELAY .lt.5) then
                 GENTIM = 0.
              else
                 GENTIM = FINT-DELAY-5.0
              end if

              FTEMP = TRAGE
              if (FTEMP.gt.GENTIM) FTEMP = GENTIM
              HTIMLT(ITPP+ITODO) = FTEMP/(GENTIM+0.0001)
              AGEPL(ITPP+ITODO) = FINT-DELAY+AGEPL(ITODO)

              322 continue
           end do
           if (ITOOMP.gt.0) then
              call RCDSET(1,.true.)
              write (JOSTND,323) ITOOMP
              323 format (/'******** WARNING:',I4,' PLANT OR NATURAL REQUESTS',&
                   &' WERE CANCELED BECAUSE THERE WERE TOO MANY.')
           end if
           ITP = ITPP+ITODO
           321 continue
           call ESRNSD (.true.,ESAVE)
           if (DEBUG) write(JOSTND,7014) ESAVE
           if (STOADJ.lt.0.0001) go to 229
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

           !     PICK BEST TREES.  STEP 1:  TWO TALLEST TREES REGARDLESS OF SPECIES

           NBEST = 0
           NUM2PK = ITP-ISTART+1
           do J = 1,2
              if (NBEST.ge.NUM2PK) go to 176
              FTEMP = 0.001
              do I = ISTART,ITP
                 if (NOTE(I).eq.1) go to 164
                 if (HEIGHT(I).lt.FTEMP) go to 164
                 FTEMP = HEIGHT(I)
                 ITEMP = I
                 164 continue
              end do
              N = ICODE(ITEMP)
              NULL(N) = 1
              NOTE(ITEMP) = 1
              NBEST = NBEST+1
              166 continue
           end do

           !     STEP 2:  IDENTIFY THE TALLEST TREE OF EACH ADDITIONAL SPECIES

           do J = 1,NOFSPE
              do I = ISTART,ITP
                 if (ICODE(I).ne.J) go to 168
                 if (NOTE(I).eq.1) go to 168
                 if (HEIGHT(I).lt.TALL(J)) go to 168
                 TALL(J) = HEIGHT(I)
                 ILSP(J) = I
                 168 continue
              end do
              169 continue
           end do
           do J = 1,NOFSPE
              if (NULL(J).eq.1) go to 170
              ITEMP = ILSP(J)
              if (ITEMP.eq.0) go to 170
              NOTE(ITEMP) = 1
              NBEST = NBEST+1
              170 continue
           end do
           if (NBEST.ge.4.or.NBEST.ge.NUM2PK) go to 176

           !     STEP 3:  IDENTIFY TALLEST REMAINING TREES BY DECREASING HEIGHT
           !     UNTIL A MINIMUM OF 4 TREES IS REACHED, IF POSSIBLE.

           172 continue
           FTEMP = 0.001
           do J = ISTART,ITP
              if (NOTE(J).eq.1) go to 174
              if (HEIGHT(J).lt.FTEMP) go to 174
              FTEMP = HEIGHT(J)
              ITEMP = J
              174 continue
           end do
           NBEST = NBEST+1
           NOTE(ITEMP) = 1
           if (NBEST.ge.NUM2PK) go to 176
           if (NBEST.lt.4) go to 172
           176 continue

           !     NOTE: STMT 229 IS USED WHEN STOADJ .LE. 0.0

           229 continue

           !     PRINT OUT PLOT STATISTICS IF DEBUG IS ON.

           if (DEBUG) then
              ITEMP = INT((RADIAN*57.2958) + 0.5)
              write(JOSTND,6002) NN,IHAB,ITEMP,SLO,IPREP,PROB1(NCOUNT),&
                   &ITPP,NEWTPP,NCOUNT
              6002 format(/,'PLOT  H.TYPE  ASP  SLO  PREP',2X,'P(STOCK)  TREES ',&
                   &'PER PLOT    NCOUNT',/,4('-'),2X,6('-'),2X,2(3('-'),2X),&
                   &4('-'),2X,8('-'),2X,16('-'),2X,6('-'),/,I3,T8,I3,T14,I4,T19,&
                   &F4.2,T27,I1,T32,F6.4,T40,I3,' TOTAL,',I3,' NEW',T60,I3,//,T4,&
                   &'TREE RECORDS:',T19,'TREE#  SPECIES  HEIGHT  BEST?  ASEP#  ',&
                   &'ESPROB  HTIMLT',/,T19,'-----  -------  ------  -----  -----',&
                   &'  ------  ------')
              if (ITPP.gt.0) write(JOSTND,6006) (I,ICODE(I),HEIGHT(I),&
                   &NOTE(I),IASEP(I),ESPROB(I),HTIMLT(I),  I = 1,ITPP)
              6006 format(T20,I3,T29,I2,T36,F4.1,T45,I1,T49,I4,2X,F8.4,2X,F6.3)
              if (MODE.eq.1) write(JOSTND,6007)
              6007 format(T3,' PREDICTED NATURALS ABOVE; PLANT/NATURAL BELOW.')
              if (MODE.eq.1) write(JOSTND,6006) (I,ICODE(I),HEIGHT(I),NOTE(I),&
                   &IASEP(I),ESPROB(I),HTIMLT(I),  I = ITPP+1,ITP)
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

           call RDESCP (MAXTRE, MXRR)
           if (ITRN.ge.MXRR-99) then
              call ESGENT (ITRNIN)
              ITEMP = MXRR/2
              call ESCPRS (ITEMP,DEBUG)
              ITRNIN = ITRN+1

              !       OPFIND IS CALLED TO RESET THE PARAMETERS FOR THE
              !       PLANT & NATURAL KEYWORDS.

              call OPFIND (2,MYACTS,NTODO)
           end if

           !     PASS REGENERATION TO TREE LIST, NATURALS FIRST.  EXIT=STMT 200

           if (ITPP.lt.1) go to 200
           do N = 1,ITPP
              I = ICODE(N)
              if ((ISCT(I,1).eq.0).and.(ISCT(I,2).eq.0))JSPIN(I) = JSPINDEF
              HHT = HEIGHT(N)
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
              PROB(ITRN) = (ESPROB(N)*300.0) /DUPNPT
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

              !     BEST TREES NOT SUBJECT TO MORTALITY FOR THE FIRST 20 YEARS.

              IESTAT(ITRN) = IDSDAT+20
              go to 33
              199 continue
              EXCESS(I) = EXCESS(I)+1.0
              SUMHTS(I) = SUMHTS(I)+HHT
              SUMESP(I) = SUMESP(I)+ESPROB(N)
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
              FTEMP2 = SUMESP(I)/(EXCESS(I)+0.00001)
              IBRKUP = INT(EXCESS(I)/5.0 + 1.0)
              BRKUP = FLOAT(IBRKUP)
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
                 PROB(ITRN) = (FTEMP2*300.0*XCSMAX) /DUPNPT
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
                 PASTPA(I) = PASTPA(I) +(FTEMP2*300.0*XCSMAX) /DUPNPT
                 TCROP2 = TCROP2 +(FTEMP2*300.0*XCSMAX) /DUPNPT
                 226 continue
              end do
              228 continue
           end do

           !     PASS TREES FROM 'PLANT' AND 'NATURAL' KEYWORDS.  EXIT = STMT 227

           if (MODE.ne.1) go to 227
           J = ITP - ITPP
           do JJ = 1,J
              I = ICODE(ITPP+JJ)
              if ((ISCT(I,1).eq.0).and.(ISCT(I,2).eq.0))JSPIN(I) = JSPINDEF
              FTEMP = ESPROB(ITPP+JJ)
              if (FTEMP.lt.0.00011.and.JJ.lt.J) then
                 ESPROB(ITPP+JJ+1) = ESPROB(ITPP+JJ+1)+FTEMP
                 ESPROB(ITPP+JJ) = 0.0
                 go to 35
              end if
              IBRKUP = INT(FTEMP/10.0 + 1.0)
              BRKUP = FLOAT(IBRKUP)
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
                 call MISPUTZ(ITRN,0)

                 !     CALL BLISTER RUST TO PROCESS PLANTED TREES.

                 call BRESTB (TIME,5,I)
                 24 continue
              end do
              35 continue
           end do
           227 continue

           !     SUMMARIZE STATISTICS FOR THE PLOT

           SUMPRB = SUMPRB+PROB1(NCOUNT)
           SUMPRE(IPREP) = SUMPRE(IPREP) +1.0
           SUMPRE(5) = SUMPRE(5)+1.0
           IHABT(IHAB) = IHABT(IHAB) +1
           if (ITPP.lt.1) go to 217
           do I = 1,ITPP
              N = ICODE(I)
              FTEMP2 = (ESPROB(I)*300.0) /DUPNPT
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
        if (IACTK .le.0) go to 225
        ITEMP = IFIX(PRMS(1))
        IPLANT(ITEMP) = 1
        call OPDONE (ITODO,IPYEAR)
        225 continue
     end do
  end if

  !     IF NEW TREES HAVE BEEN ADDED TO THE TREELIST
  !     'GROW' TREES TO THE END OF THE CYCLE

  if (ITRN.ge.ITRNIN)call ESGENT (ITRNIN)
  if (DEBUG)write(JOSTND,*)'AFTER CALL TO ESGENT-ITRN,ITRNIN= ',&
       &ITRN,ITRNIN
  do I = ITRNIN,ITRN

     !  CALCULATE A CROWN WIDTH FOR SEEDLINGS/SPROUTS

     CRDUM = 1.
     call CWCALC(ISP(I),PROB(I),DBH(I),HT(I),CRDUM,&
          &ICR(I),CW,0,JOSTND)
     CRWDTH(I) = CW
     ABIRTH(I) = ABIRTH(I)+GENTIM
     230 continue
  end do

  !     PRINT REGENERATION SUMMARY

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
  J = INT(ZHARV + 0.5)
  K = INT(ZMECH + 0.5)
  L = INT(ZBURN + 0.5)
  if (INGRO.eq.1) go to 258
  if (NTALLY.eq.1) then
     call GROHED (JOREGT)
     write(JOREGT,6012) NPLT,MGMID
     6012 format(/,76('-'),/,T17,'REGENERATION ESTABLISHMENT ',&
          &'MODEL VERSION 2.0',/,T9,'STAND ID: ',A26,5X,&
          &'MANAGEMENT CODE: ',A4,/,76('-') )
     write(JOREGT,6013) J,K,L,N,NN,ITEMP,(IHABT(I),I = 1,16)
     6013 format(/,T3,'SITE PREP SUMMARY',T37,'PLOT HABITAT TYPE SUMMARY',&
          &/,19('-'),T23,54('-'),/,'PREP:NONE MECH BURN   SERIES:',&
          &'<--- DF --> <--- GF -->  C WH <--- AF & MH --->',/,'YEAR:',&
          &3(I4,1X),'   GROUP: 1  2  3  4  5  6  7  8  9 10 11 12 13 14 ',&
          &'15 16',/,T2,'PCT:',I4,2I5,'   #PLOTS:',I2,15I3)
     call DBSSITEPREP(J,K,L,N,NN,ITEMP)
     do M = 1,16
        if (IHABT(M).gt.0) call DBSPLOTHAB(IHABT(M),HABTYP(M),&
             &SERIES(M),M)
     end do
     if (IPINFO.eq.3) write(JOREGT,6022)
     6022 format (/,'NOTE: SOME OR ALL PLOT ID CODES FROM THE TREE ',&
          &'DATA FILE DID NOT MATCH',/,T7,'THOSE IN THE PLOTINFO FILE.  ',&
          &'STAND VALUES WERE SUBSITUTED.')
     if (NONSTK.gt.0) write(JOREGT,6017) NONSTK
     6017 format(/,'NOTE:',I3,' NON-STOCKABLE PLOTS EXCLUDED FROM ',&
          &'THIS SUMMARY','.')
  end if
  ITEMP = KDT+1-J
  if (STOADJ.lt.0.0001) SUMPRB = 0.0
  write(JOREGT,6021) NTALLY,ITEMP,SUMPRB,KDT
  6021 format(/,'TALLY',I2,' AT',I3,' YEARS.  PROBABILITY OF',&
       &' STOCKING IS ',F6.4,' IN THE FALL OF',I5,'.')
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
  6015 format(/'NOTE: REPORTED PROBABILITY OF STOCKING DOES NOT ',&
       &'INCLUDE TREES FROM PLANT ',/,T7,'AND NATURAL KEYWORDS.  ',&
       &'SPECIES= ',20A4,(/,T40,20A4))
  write(JOREGT,6016) REGNBK
  6016 format(/,T6,'SUMMARY OF ALL',T30,'SUMMARY OF BEST         ',&
       &'TREES <',F4.1,' IN. DBH',/,T6,'TREES REGENERATING',T30,&
       &'TREES REGENERATING      BEING PROJECTED BY',/,T6,&
       &'DURING THIS TALLY.',T30,'DURING THIS TALLY.      THE ',&
       &'PROGNOSIS MODEL',/,T6,19('-'),5X,19('-'),5X,19('-'),/,13X,&
       &'TREES % OF      TREES % OF  AVERAGE     TREES % OF',/,5X,&
       &'SPECIES /ACRE TOTAL     /ACRE TOTAL HEIGHT      /ACRE TOTAL ',&
       &'SPECIES',/,5X,7('-'),1X,5('-'),1X,5('-'),5X,5('-'),1X,5('-'),&
       &1X,7('-'),5X,5('-'),1X,5('-'),1X,7('-') )
  WTAVEHT = 0.0
  do I = 1,NOFSPE
     if (TOTTPA(I).le.0..and. BESTPA(I).le.0..and. PASTPA(I).le.0.)&
          &go to 222
     write(JOREGT,6014) NSP(I,1),TOTTPA(I),TOTPCT(I),BESTPA(I),&
          &BESPCT(I),AVEHT(I),PASTPA(I),PASPCT(I),NSP(I,1)
     6014 format(T9,A2,2X,2F6.0,4X,2F6.0,1X,F5.1,6X,2F6.0,3X,A2)
     WTAVEHT = AVEHT(I)*BESTPA(I)+WTAVEHT
     call DBSTALLY(SUMPRB,NTALLY,NSP(I,1),TOTTPA(I),&
          &TOTPCT(I),BESTPA(I),BESPCT(I),AVEHT(I),PASTPA(I),PASPCT(I),KDT,&
          &WTAVEHT,1,TTOTTP,TCROP1,TCROP2)
     222 continue
  end do
  if (TCROP1.gt.0.0) WTAVEHT = WTAVEHT/TCROP1
  call DBSTALLY(SUMPRB,NTALLY,FINAL,TOTTPA(I),&
       &TOTPCT(I),BESTPA(I),BESPCT(I),AVEHT(I),PASTPA(I),PASPCT(I),KDT,&
       &WTAVEHT,2,TTOTTP,TCROP1,TCROP2)
  write(JOREGT,6026) TTOTTP,TCROP1,TCROP2
  6026 format(T14,5('-'),T30,5('-'),T54,5('-'),/,T12,F7.0,T28,F7.0,&
       &T52,F7.0,/)
  go to 259

  !     THIS SECTION PRINTS INGROWTH SUMMARY

  258 continue
  write(JOREGT,6012) NPLT,MGMID
  if (MODE.eq.1) then
     if (STOADJ.gt.0.) then
        write(JOREGT,6011) TTOTTP,&
             &'INGROWTH, PLANT, AND NATURAL ',KDT
     else
        write(JOREGT,6011) TTOTTP,&
             &'PLANT AND NATURAL ',KDT
     end if
  else
     write(JOREGT,6011) TTOTTP,'INGROWTH ',KDT
  end if
  6011 format(F7.1,' ',A,&
       &'TREES/ACRE ADDED IN THE FALL OF',I5)
  write(JOREGT,6029) (NSP(I,1),I = 1,NOFSPE)
  6029 format(/('   SPECIES:',20(2X,A2,1X)))
  write(JOREGT,6032)(TOTTPA(J),J = 1,NOFSPE)
  6032 format(/('TREES/ACRE:',20F5.0))
  do M = 1,NOFSPE
     if (TOTTPA(M).gt.0) call DBSINGROW(KDT,TTOTTP,NSP(M,1),&
          &TOTTPA(M))
  end do
  259 continue
  260 continue

  !     PRINT PLOT SUMMARY TABLE IF REQUESTED.

  call DBCHK (DEBUG,'ESPSUM',6,ICYC)
  if (DEBUG) then
     write(JOREGT,6018) (NSP(I,1),I = 1,NOFSPE)
     6018 format('IN ESTAB (ESPSUM PLOT SUMMARY): '/&
          &'     SLOPE ASPECT HAB. TOPO SITE       ',&
          &' <------PLOT OVERSTORY DENSITY (SQ.FT./ACRE)----->',/,&
          &'PLOT %/100 (DEG.) GRP. POS. PREP  P(S)  TOTAL ',11(A2,2X))
     do I = 1,NPTIDS
        NNID = IPTIDS(I)
        N = INT((PASP(NNID)*57.2958) + 0.5)
        NN = IPHYS(NNID)
        ITEMP = IPPREP(I*IDUP)
        write(JOREGT,6020) NNID,PSLO(NNID),N,IPHAB(NNID),CTOPO(NN),&
             &CPREP(ITEMP),PROB1(I*IDUP),BAAA(NNID),(OVER(J,NNID),J = 1,NOFSPE)
        6020 format(1X,I3,F6.2,I6,2X,I3,1X,2(1X,A4),F7.4,F6.0,&
             &(/,2X,25F4.0))
        221 continue
     end do
  end if
  KDTOLD = KDT
  MODE = 0
  return
end
