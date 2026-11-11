!=========================================!
!   Establish regeneration tree records   !
!=========================================!
! LS $Id$
subroutine ESTAB (KDT)
  implicit none

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'ESPARM.f90'
  include 'ESTREE.f90'
  include 'ESHAP.f90'
  include 'PDEN.f90'
  include 'CALDEN.f90'
  include 'ESCOMN.f90'
  include 'VARCOM.f90'
  include 'STDSTK.f90'

  !     REGENERATION ESTABLISHMENT MODEL   --   VERSION 2.0
  !     USDA FOREST SERVICE, INTERMOUNTAIN RESEARCH STATION
  !     1221 S. MAIN, MOSCOW, IDAHO  83843   (208) 882-3557
  !     ******* THIS SUBROUTINE FOR USE IN VARIANTS *******

  external ESRANN
  ! Variables
  logical :: DEBUG
  integer*4 :: IDCMP1,ITMP,ITEMP,ITOOMP
  character(len=4) :: CPREP(4)
  character(len=4) :: CTOPO(5)
  character(len=2) :: CBLANK
  character(len=2) :: CLAST(MAXSP)
  real    :: PRMS(6),SUMUP(MAXSP)
  integer :: MYTYPE(30),MYHABG(16)
  real    :: TOTTPA(MAXSP),TOTPCT(MAXSP),BESTPA(MAXSP),AVEHT(MAXSP)
  integer :: MYACTS(4)
  real    :: BESPCT(MAXSP),PASTPA(MAXSP),PASPCT(MAXSP)
  real    :: FIRST(2,MAXSP),WTAVEHT
  integer :: NNPREP(4),ICODE(99)
  real    :: HEIGHT(99),ESPROB(99)
  integer :: IASEP(99),IPLANT(MAXSP)
  real    :: HTIMLT(99),AGEPL(99)
  real    :: CW,CRDUM,BRKUP,RAN,XXH,XH,HHT,SITEI,TMTIME,DILATE,SHADE
  real    :: TREEHT,DELAY,TRAGE,PTREE,ESAVE,EMSQR,BAAOLD,RADIAN
  integer :: L,K,IBRKUP,JJ,MXRR,ITP,ISHADE,IPNSPE,IDO,ITPP,IEVTYR
  integer :: IREP,NTIMES,IPOLD,ITYPEP,IEND,ISTART,NCOUNT,IPYEAR
  integer :: IACTK,NP,IZERO,MYDO,II,ISER,J,NN,ITODO,NTODO,IDUP,N,I
  integer :: KDT,NOFSPE,ITRNIN,NEWTPP
  real    :: ZHARV,FLOKDT,FLONPT,TCROP1,TCROP2,TTOTTP,DUP,DUPNPT,PNONE
  real    :: DRAW,SUM,FTEMP,FTEMP2,GENTIM,BACHLO,SUM1,SUM2,TBAAA,SUMPRB
  character(len=3) :: FINAL

  data CBLANK/'  '/,CPREP/'NONE','MECH','BURN','ROAD'/,CTOPO/&
       &'BOTM','LOWR',' MID','UPPR','RIDG'/,MYTYPE/9*1,2*5,2*2,3*3,4,&
       &12*5,1/,IDCMP1/10000000/,MYHABG/4*1,4*2,3,4,6*5/,&
       &MYACTS/430,431,491,493/,NOFSPE/MAXSP/
  data FINAL/"ALL"/

  !     INITIALIZE ARRAYS AND VARIABLES

  call DBCHK (DEBUG,'ESTAB',5,ICYC)
  if (DEBUG) write(JOSTND,7000) KDT,IDSDAT,NTALLY
  7000 format(/'IN ESTAB; BEGIN DEBUG.  KDT=',I5,' IDSDAT=',I5,&
       &' NTALLY=',I2)
  ITRNIN = ITRN+1
  ZHARV = FLOAT(IDSDAT)
  FLOKDT = FLOAT(KDT)
  TIME = FLOKDT - ZHARV +1.0
  FLONPT = FLOAT(NPTIDS)
  ELEVSQ = ELEV*ELEV
  ITMP = ICYC
  ITMP = ITMP*10000
  MODE = 0
  if (KDT+1-IY(1).gt.20) then
     INADV = 1
     LOAD = 0
  end if
  do I = 1,99
     AGEPL(I) = 0.0
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
  WTAVEHT = 0.0
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
  IDUP = INT(DUP+0.5)
  DUPNPT = FLONPT*DUP

  !     CHANGE FOREST CODES

  do I = 1,20
     if (KODFOR.ne.IFORCD(I)) go to 4
     IFO = IFORST(I)
     go to 5
     4 continue
  end do
  IFO = 4
  5 continue

  !     IF NOT THE FIRST TALLY, CANCEL MECH & BURNPREP'S IN THIS CYCLE.

  if (NTALLY.gt.1) then
     call OPFIND (2,MYACTS(3),NTODO)
     if (NTODO.gt.0) then
        do ITODO = 1,NTODO
           call OPDEL1 (ITODO)
           6 continue
        end do
     end if
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

  KDTOLD = INT(ZHARV-0.5)
  do I = 1,5
     SUMPRE(I) = 0.0
     45 continue
  end do
  do I = 1,16
     IHABT(I) = 0
     46 continue
  end do
  276 continue
  if (NTALLY.eq.1) then
     call ESRANN (DRAW)
     ITEMP = INT(DRAW*100000.0 +0.5)
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
     177 continue
  end do
  198 continue

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

  ISER = 1
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
           IHAB = IPHAB(NNID)
           ISER = MYHABG(IHAB)
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

           !     PROBABILITY OF STOCKING CODE USED TO BE HERE.

           137 continue

           !     RESET TIME TO YEARS SINCE DISTURBANCE OR SITE PREP.

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
           7010 format('TIME=',F7.1,'; IEVTYR=',I5)
           call ESTIME (IEVTYR,KDT)
           if (DEBUG) write (JOSTND,7020) BWB4,BWAF,REGT,SQREGT,SQBWAF
           7020 format('BWB4=',F7.3,' BWAF=',F7.3,' REGT=',F7.3,' SQREGT=',&
                &F7.3,' SQBWAF=',F7.3)
           call ESRANN (DRAW)
           EMSQR = 1.0
           if (DRAW.lt.0.5) EMSQR = -1.0
           call ESRANN (DRAW)
           EMSQR = EMSQR*DRAW
           if (DEBUG) write (JOSTND,7015) EMSQR
           7015 format('EMSQR=',F10.3)

           !     CHOOSE THE NUMBER OF TREES ON A STOCKED PLOT.

           !     DETERMINE NUMBER OF SPECIES ON THE PLOT.

           !     CHOOSE THE SPECIES TO OCCUPY THE PLOT

           !     ASSIGN HEIGHTS TO TALLEST TREE BY SPECIES & ADVANCE/SUBS.

           ITPP = 0
           NEWTPP = 0

           !     CREATE TREES FROM 'PLANT' AND 'NATURAL' KEYWORDS.  EXIT=STMT 321

           call ESRANN (DRAW)
           ITEMP = INT(DRAW*100000.0+0.5)
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
              if (DEBUG)write(JOSTND,*)'ITODO, AGEPL= ',ITODO, AGEPL(ITODO)
              DELAY = FLOAT(IPYEAR) -(KDT+1-FINT)
              TREEHT = PRMS(5)
              ISHADE = IFIX(PRMS(6))
              SHADE = 0.0
              if (ISHADE.eq.1) SHADE = 1.0
              if (ISHADE.eq.2) SHADE = -1.0
              FTEMP = BAA
              !      IF(FTEMP.LT.1.5) FTEMP=0.0
              if (FTEMP.lt.1.0) FTEMP = 1.0
              if (DEBUG) write(JOSTND,7048) PTREE,SHADE,FTEMP,DUPNPT,BA
              7048 format('PTREE=',F6.1,'  SHADE=',F4.1,'  FTEMP=',F6.1,&
                   &'  DUPNPT=',F4.1,'  BA=',F6.1)

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
              SITEI = SITEAR(IPNSPE)
              call ESSUBH (IPNSPE,HHT,EMSQR,DILATE,DELAY,ELEV,ISER,GENTIM,&
                   &TRAGE,SITEI,JOSTND)
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
                 162 RAN = BACHLO(0.5,0.25,ESRANN)
                 if (RAN.lt.-2.5.or. RAN.gt.2.5) go to 162
                 HHT = HHT+RAN
                 HHT = HHT+HTADJ(IPNSPE)
                 if (HHT.lt.XMIN(IPNSPE) ) HHT = XMIN(IPNSPE)
              end if
              FIRST(2,IPNSPE) = SQRT(DILATE)
              if (HHT.gt.HHTMAX(IPNSPE)) HHT = HHTMAX(IPNSPE)
              if (DEBUG) write(JOSTND,7050) IPNSPE,HHT,PTREE,ITODO,ITPP,IACTK,&
                   &TIME,DELAY,TRAGE
              7050 format('IPNSPE=',I3,'  HHT=',F6.2,'  PTREE=',F7.2,'  ITODO=',I4,&
                   &'  ITPP=',I4,'  IACTK=',I4,' TIME= ',F6.3, 'DELAY= ',F6.2,&
                   &' TRAGE= ',F6.2)
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

           !     PICKING BEST TREES USED TO BE DONE HERE.

           !     NOTE: STMT 229 IS USED WHEN STOADJ .LE. 0.0

           229 continue

           !     PRINT OUT PLOT STATISTICS IF DEBUG IS ON.

           if (DEBUG) then
              ITEMP = INT((RADIAN*57.2958) +0.5)
              write(JOSTND,6002) NN,IHAB,ITEMP,SLO,IPREP,&
                   &ITPP,NEWTPP,NCOUNT
              6002 format(/,'PLOT  H.TYPE  ASP  SLO  PREP',2X,'P(STOCK)  TREES ',&
                   &'PER PLOT    NCOUNT',/,4('-'),2X,6('-'),2X,2(3('-'),2X),&
                   &4('-'),2X,8('-'),2X,16('-'),2X,6('-'),/,I3,T8,I3,T14,I4,T19,&
                   &F4.2,T27,I1,T40,I3,' TOTAL,',I3,' NEW',T60,I3,//,T4,&
                   &'TREE RECORDS:',T19,'TREE#  SPECIES  HEIGHT  BEST?  ASEP#  ',&
                   &'ESPROB  HTIMLT',/,T19,'-----  -------  ------  ----- ',&
                   &' -----  ------  ------')
              if (MODE.eq.1) write(JOSTND,6006) (I,ICODE(I),HEIGHT(I),&
                   &IASEP(I),ESPROB(I),HTIMLT(I),  I = ITPP+1,ITP)
              6006 format(T20,I3,T29,I2,T36,F4.1,T49,I4,2X,F8.4,2X,F6.3)
           end if

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
              IBRKUP = INT(FTEMP/10.0 +1.0)
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
                 NORMHT(ITRN) = 0
                 HT2TD(ITRN,1) = 0.
                 HT2TD(ITRN,2) = 0.
                 ITRE(ITRN) = NNID
                 FTEMP2 = FTEMP/BRKUP
                 PROB(ITRN) = FTEMP2
                 DBH(ITRN) = 0.1
                 HT(ITRN) = HHT
                 ABIRTH(ITRN) = AGEPL(JJ)
                 DEFECT(ITRN) = 0
                 ISPECL(ITRN) = 0

                 PTOCFV(ITRN) = 0.
                 PMRCFV(ITRN) = 0.
                 PSCFV (ITRN) = 0.
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

           SUMPRE(IPREP) = SUMPRE(IPREP) +1.0
           SUMPRE(5) = SUMPRE(5)+1.0
           IHABT(IHAB) = IHABT(IHAB) +1

           !     END OF PLOT CYCLE. RETURN AND CONTINUE PROJECTION.

           201 continue
        end do
        202 continue
     end do
     203 continue
  end do

  !     ALL PLOTS ARE PROCESSED.

  if (DEBUG) write (JOSTND,6030) ITRNIN,ITRN
  6030 format(36('= '),/,' ITRNIN=',I6,'  ITRN=',I6)
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
  N = INT(SUMPRE(1) +0.5)
  NN = INT(SUMPRE(2) +SUMPRE(4) +0.5)
  ITEMP = INT(SUMPRE(3) +0.5)
  J = INT(ZHARV+.5)
  K = INT(ZMECH+.5)
  L = INT(ZBURN+.5)
  if (NTALLY.eq.1.) then
     call GROHED (JOREGT)
     write(JOREGT,6012) NPLT,MGMID
     6012 format(/,76('-'),/,T17,'REGENERATION ESTABLISHMENT ',&
          &'MODEL VERSION 2.0',/,T9,'STAND ID: ',A26,5X,&
          &'MANAGEMENT CODE: ',A4,/,76('-') )
     write(JOREGT,6013) J,K,L,N,NN,ITEMP
     6013 format(/,T3,'SITE PREP SUMMARY',&
          &/,19('-'),/,'PREP:NONE MECH BURN',&
          &/,'YEAR:',&
          &3(I4,1X),&
          &/,T2,'PCT:',I4,2I5)
  end if
  ITEMP = KDT+1-J
  write(JOREGT,6021) NTALLY,ITEMP,KDT
  6021 format(/,'TALLY',I2,' AT',I3,' YEARS.  REGENERATION SUMMARY',&
       &' IN THE FALL OF',I5,'.')
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
  6015 format('TREES FROM PLANT AND NATURAL KEYWORDS: SPECIES= ',&
       &11A4)
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
  SUMPRB = 1.1
  do I = 1,NOFSPE
     if (TOTTPA(I).le.0..and. BESTPA(I).le.0..and. PASTPA(I).le.0.)&
          &go to 222
     write(JOREGT,6014) NSP(I,1)(1:2),TOTTPA(I),TOTPCT(I),BESTPA(I),&
          &BESPCT(I),AVEHT(I),PASTPA(I),PASPCT(I),NSP(I,1)(1:2)
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
        N = INT((PASP(NNID)*57.2958) +0.5)
        NN = IPHYS(NNID)
        ITEMP = IPPREP(I*IDUP)
        write(JOREGT,6020) NNID,PSLO(NNID),N,IPHAB(NNID),CTOPO(NN),&
             &CPREP(ITEMP),BAAA(NNID),(OVER(J,NNID),J = 1,NOFSPE)
        6020 format(1X,I3,F6.2,I6,2X,I3,1X,1X,A4,5X,A4,F6.0,&
             &(/,2X,11F4.0))
        221 continue
     end do
  end if
  KDTOLD = KDT
  MODE = 0
  return
end
