!=============================!
!   canada/newmist : dmcycl   !
!=============================!
! CANADA-NEWMIST $Id$
subroutine DMCYCL
  implicit none

  ! **DMCYCL -- NISI  Date of last revision April 7 1994

  ! Purpose:
  !   This routine creates the mistletoe life history, independent of
  ! the cycle length (normally 10 years). For each crownthird of each
  ! record four life history compartments are maintained:

  !   IMMATURE:      Young infections not yet large enough to produce
  !                   flowers
  !                   (IMMAT = 1)
  !   LATENT         Infections large enough to flower, but suppressed
  !                   by low light conditions
  !                   (LATENT = 2)
  !   NON-FLOWERING  Once-flowering infections that have been
  !                   suppressed by low light conditions (swellings)
  !                   (SUPRSD = 3)
  !   FLOWERING      Flowering infections that contribute to spread and
  !                   intensification
  !                   (ACTIVE = 4)

  ! These compartments are equilibrated when the routine is first
  ! entered, by doing a 30 year iteration of the pools, followed by
  ! scaling back to the initial condition. During normal timesteps.
  ! the pools are iterated on an annual basis. The interesting part of
  ! this is the role of light in moving latent and non-flowering
  ! infections to flowering, and flowering infections to non-flowering.
  ! Users are allowed to define a 4-piece linear function (using the
  ! DMLIGHT keyword) that describes the shape of the forward
  ! ( -> flowering) and backward ( <- flowering) reactions. Last of
  ! all, annual mortality is added to each of the life history pools.
  ! These keywords can be specified for each species.

  ! ** NOTE ON UNITS **

  !   The units of infection are traditionally density: "DMR/MESH**3"
  ! In this routine, infection is occasionally transformed into an
  ! absolute number, by multiplying by the crownthird volume, and then
  ! transformed back again at the end.

  ! Called by:

  !     DMTREG

  ! Other routines called:

  !     [none]

  ! Argument list definitions:

  !     [none]

  ! Local variable definitions:

  !     INTEGER i        Index of tree record
  !     INTEGER j        Loop counter for crownthird
  !     INTEGER k        Loop counter to piecewise light function
  !     INTEGER m        Loop counter for annual timestep
  !     INTEGER r        Loop counter to breakpoint array
  !     INTEGER s        Loop counter for MESH bands in breakpoints
  !     INTEGER ISPC     Loop counter for species
  !     INTEGER i1       Index to first record of each species
  !     INTEGER i2       Index to last record of each species
  !     INTEGER i3       Index to species-sorted array of records
  !     INTEGER LHt      MESH band in which lower breakpoint lies
  !     INTEGER UHt      MESH band in which upper breakpoint lies
  !     INTEGER MidHt    MESH band for midpoint of crownthird
  !     INTEGER Spin     Startup time delay (30 years)
  !     INTEGER LastYr   Last year of timestep loop (usually 10)
  !     INTEGER LghtIndx height-index position of each crownthird
  !     REAL    SpSurv   Annual survival rate of mistletoe
  !     REAL    New      New S+I addition to crownthird pool
  !     REAL    FProp    Prop'n of light-suppressed becoming Flowering
  !     REAL    FProp2   Prop'n of Immature becoming Latent
  !     REAL    BProp    Prop'n of Flowering becoming light-suppressed
  !     REAL    ImmLat   Amount of Immature becoming Latent
  !     REAL    LatAct   Amount of Latent becoming Flowering
  !     REAL    ActSpr   Amount of Flowering becoming Nonflowering
  !     REAL    Spr      Amount of light-suppressed becoming Flowering
  !     REAL    xImm     Size of Immature pool
  !     REAL    xLat     Size of Latent pool
  !     REAL    xSpr     Size of light-suppressed pool
  !     REAL    xAct     Size of Flowering pool
  !     REAL    xImmBC   Size of biocontrol suppressed immature pool
  !     REAL    xLatBC   Size of biocontrol suppressed latent pool
  !     REAL    xSprBC   Size of biocontrol suppressed nonflowering pool
  !     REAL    xActBC   Size of biocontrol suppressed flowering pool
  !     REAL    xDedBC   Size of biocontrol killed pool
  !     REAL    mult     Multiplier used during equilibration
  !     REAL    x        Initial Flowering+Nonflowering pool size
  !     REAL    y        Volume (MESH**3) of each crownthird
  !     REAL    TVol     Array of crownthird volumes (MESH**3)
  !     REAL    FvecX    X-points array for forward light reaction
  !     REAL    FvecY    Y-points array for forward light reaction
  !     REAL    BvecX    X-points array for backward light reaction
  !     REAL    BvecY    Y-points array for backward light reaction

  ! Common block variables and parameters:

  !     MAXSP   PRGPRM
  !     ISCT    CONTRL
  !     IND1    ARRAYS
  !     MXHT    DMCOM
  !     BPCNT   DMCOM
  !     VOLUME  DMCOM
  !     CRTHRD  DMCOM
  !     DMTINY  DMCOM
  !     IMMAT   DMCOM
  !     LATENT  DMCOM
  !     SUPRSD  DMCOM
  !     ACTIVE  DMCOM
  !     DMFLWR  DMCOM
  !     DMSURV  DMCOM
  !     DMLtnp  DMCOM
  !     DMLtRx  DMCOM
  !     BrkPnt  DMCOM
  !     DMRDMX  DMCOM
  !     DMINF   DMCOM
  !     ZPDn    DMCOM
  !     DMCAP   DMCOM

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'DMCOM.f90'
  include 'PLOT.f90'

  ! Local variables.

  ! Variables
  logical :: LOK,DEBUG,TF1,TF2
  integer :: i,j,k,m,N,L,r,s
  integer :: ISPC,ISPP,i1,i2,i3,BCT,IHT
  integer :: LHt,UHt,MidHt(MAXTRE,CRTHRD)
  integer :: Spin,LastYr
  integer :: IYR,JYR,NTODO,IACTK,NPAR,MYACTS(1)

  real    :: SpSurv
  real    :: New,FProp2,HtWt,xNow,xPrv
  real    :: ImmLat,LatAct,ActSpr,SprAct
  real    :: xImm,xLat,xSpr,xAct,xDedBC
  real    :: xImmBC(MAXBC),xLatBC(MAXBC)
  real    :: xSprBC(MAXBC),xActBC(MAXBC)
  real    :: ImmImmBC(MAXBC),LatLatBC(MAXBC)
  real    :: SprSprBC(MAXBC),ActActBC(MAXBC)
  real    :: ImmBCImm(MAXBC),LatBCLat(MAXBC)
  real    :: SprBCSpr(MAXBC),ActBCAct(MAXBC)
  real    :: SprDedBC,ActDedBC
  real    :: x,y,XTiny
  real    :: xOriginal(MAXTRE,CRTHRD),TVol(MAXTRE,CRTHRD)
  real    :: FProp(MAXTRE,CRTHRD),BProp(MAXTRE,CRTHRD)
  real    :: FvecX(4),FvecY(4),BvecX(4),BvecY(4)
  real    :: BCMORT(MAXSP,ACTIVE,MXHT)
  real    :: BCSUPP(MAXSP,ACTIVE,MXHT,MAXBC)
  real    :: PRM(3),ATTEN(MXHT)

  real    :: ALGSLP

  data Spin   /  30/
  data MYACTS /2010/ ! Bio-control scheduling code

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'DMCYCL',6,ICYC)

  XTiny = LOG(DMTINY)

  !     THIS LOOP WALKS THROUGH THE CROWN THIRD IN MESH METER BANDS FROM
  !     'LHT' TO 'UHT'. IT IS ASSUMED THAT THE COVER MODEL-DERIVED 'VOLUME'
  !     PARAMETER IS CORRECTLY WEIGHTED FOR BREAKPOINTS CROSSING MESH BANDS.
  !     THE ASSUMPTION HAS BEEN INSPECTED AND LOOKS OK. 'Y' ADDS UP THE
  !     CONTRIBUTIONS FROM EACH BAND; THEN THEY ARE ADDED INTO 'TVOL' TO
  !     GIVE THE VOLUME (MESH**3) OF EACH CROWNTHIRD.

  do i = 1,ITRN
     do r = 2,BPCNT
        UHt = INT(BrkPnt(i,r-1)) + 1
        LHt = INT(BrkPnt(i,r  )) + 1
        y = 0.0
        TVol(i,r-1) = 0.0
        do s = LHt,UHt
           if (s .gt. MXHT) goto 800
           if ((LHt .eq. s) .and. (r .lt. BPCNT)) then
              HtWt = 1.0 - (BrkPnt(i,r) - INT(BrkPnt(i,r)))
           else if ((UHt .eq. s) .and. (r .gt.2)) then
              HtWt = BrkPnt(i,r-1) - INT(BrkPnt(i,r-1))
           else
              HtWt = 1.0
           end if
           y = y + HtWt * DMRDMX(i,s,VOLUME)
           800 continue
        end do
        TVol(i,r-1) = y
     end do
  end do

  !     STEP 2: FIGURE OUT FORWARD AND BACKWARD RATES

  !     CALCULATE LIGHT LEVEL AS FOUND AT THE MIDDLE CELL OF EACH CROWN
  !     THIRD. PROPORTIONS GOING FORWARD AND BACKWARD DEPEND UPON THE LIGHT
  !     LEVEL. THE COEFFICIENTS ARE UNDER USER CONTROL THROUGH THE DMLIGHT
  !     KEYWORD.IT WOULD BE MORE ACCURATE TO USE A WEIGHTED MEASURE OF THE
  !     VOLUME OF INFECTION IN EACH MESH.

  !     THERE IS PROBABLY A CLEVER WAY OF ORGANIZING DMLTRX() SO THAT IT
  !     CAN BE PASSED DIRECTLY TO ALGSLP. HELP YOURSELF IF YOU ARE FEELING
  !     CLEVER TODAY.

  do ISPC = 1,MAXSP

     I1 = ISCT(ISPC,1)
     if (I1.eq.0) goto 98
     I2 = ISCT(ISPC,2)

     do k = 1,DMLtnp(ISPC,1)
        FvecX(k) = DMLtRx(ISPC,1,1,k)
        FvecY(k) = DMLtRx(ISPC,1,2,k)
     end do
     do k = 1,DMLtnp(ISPC,2)
        BvecX(k) = DMLtRx(ISPC,2,1,k)
        BvecY(k) = DMLtRx(ISPC,2,2,k)
     end do

     !       COMPUTE THE LIGHT-DRIVEN FORWARD AND BACKWARD REACTION
     !       FOR EACH TREE AND CROWNTHIRD. STORE THE INDEX POSITION
     !       FOR THE CROWNTHIRD. STORE ORIGINAL ACTIVE POOL FOR USE
     !       IN CYCLE 1

     do I3 = I1,I2
        I = IND1(I3)
        do j = 1,CRTHRD
           IHT = INT((BrkPnt(i,j) + BrkPnt(i,j+1)) / 2.) + 1
           IHT = MAX(1,MIN(IHT,MXHT))
           FProp(i,j) = &
                &ALGSLP(FLOAT(IHT),FvecX,FvecY,DMLtnp(ISPC,1))
           BProp(i,j) = &
                &ALGSLP(FLOAT(IHT),BvecX,BvecY,DMLtnp(ISPC,2))
           MidHt(i,j) = IHT
           xOriginal(i,j) = DMINF(i,j,ACTIVE)
        end do
     end do
  98 end do

  !     IF THIS IS THE BEGINNING OF THE SIMULATION, THEN SPIN WHEELS TO
  !     ALLOCATE TO OTHER POOLS BESIDES 'ACTIVE'. OTHERWISE THEY ARE ZERO
  !     ON FIRST ENTRY. THE FLAG 'ZPDN' IS SET TO .TRUE. AT THE END OF THE ROUTINE.

  if (ZPDn) then
     LastYr = IFINT
  else
     LastYr = IFINT + Spin
  end if

  !     MAIN LOOP OVER YEARS. IN MOST CYCLES LastYr IS THE CYCLE LENGTH;
  !     IN THE FIRST CYCLE THERE ARE 'SPIN' EXTRA LOOPS TO HELP EQUILIBRATE
  !     THE INITIAL POOLS

  do M = 1, LastYr

     !       ZERO BC-MORTALITY, BC-SUPPRESSION THIS YEAR

     do J = 1,MAXSP
        do K = 1,ACTIVE
           do L = 1,MXHT
              BCMORT(J,K,L) = 0.0 ! MORTALITY
              do N = 1,MAXBC
                 BCSUPP(J,K,L,N) = 0.0 ! SUPPRESSION
              end do
           end do
        end do
     end do

     !       PICK UP BIO-CONTROL (BC) KEYWORD PARAMETERS. IN SPITE OF THE VARIABLE NAME,
     !       'BCMORT' STORES *SURVIVAL* AT THE POINT AND 'BCSUPP' STORES NON-SUPPRESSED
     !       THESE ARE CONVERTED TO THEIR COMPLEMENTS LATER ON.

     if (ZPDn) then
        IYR = IY(ICYC)+M-1
     else
        IYR = IY(ICYC)+M-1-Spin
     end if

     NTODO = 0
     LOK = .true.
     call OPFIND(1,MYACTS,NTODO)
     if (NTODO .ne.0) then
        do I = 1,NTODO
           call OPGET(I,3,JYR,IACTK,NPAR,PRM)
           if (JYR .eq. IYR) then
              BCT = IFIX(PRM(1))
              if (BCT .le.0.or. BCT .gt. MAXBC) then
                 LOK = .false.
                 goto 11
              end if
              ISPP = BC(BCT)%Spp
              if (ISPP .lt.1.or. ISPP .gt. MAXSP) LOK = .false.
              11 if (LOK) then
                 PRM(2) = MAX(0.0, PRM(2))
                 PRM(3) = MAX(0.0, MIN(FLOAT(MXHT*MESH), PRM(3)))
                 IHT = MAX(1, MIN(MXHT, 1+IFIX(PRM(3)/FLOAT(MESH))))
                 ATTEN(IHT) = 1.0
                 do L = IHT+1,MXHT
                    ATTEN(L) = ATTEN(L-1) * (1.0 - SHADE(L-1))
                 end do
                 do L = IHT-1,1,-1
                    ATTEN(L) = ATTEN(L+1) * (1.0 - SHADE(L+1))
                 end do
                 do L = 1,MXHT
                    do N = 1,ACTIVE
                       BCMORT(ISPP,N,L) = BCMORT(ISPP,N,L) +&
                            &ATTEN(L) * PRM(2) * LOG(MAX(DMTINY,&
                            &1.0 - (BC(BCT)%Mort(N)/100.0)))
                       BCSUPP(ISPP,N,L,BCT) = BCSUPP(ISPP,N,L,BCT) +&
                            &ATTEN(L) * PRM(2) * LOG(MAX(DMTINY,&
                            &1.0 - (BC(BCT)%Suprs(N)/100.0)))
                    end do
                 end do
                 call OPDONE(I,IYR)
              else
                 call OPDEL1(I)
              end if
           end if
        end do
     end if

     do ISPC = 1,MAXSP

        I1 = ISCT(ISPC,1)
        if (I1.eq.0) goto 99
        I2 = ISCT(ISPC,2)

        SpSurv = DMSURV(ISPC)

        !         COMPUTE IMMATURE->LATENT FORWARD REACTIONS THE DMFLWR (NOMINALLY
        !         4 YEAR) LAG MEANS ABOUT ONE QUARTER MATURE EACH YEAR, ASSUMING A FLAT
        !         AGE-STRUCTURE (NOT QUITE RIGHT).

        FProp2 = 1.0/FLOAT(DMFLWR(ISPC))

        !         COMPUTE MULTIPLICATIVE MORTALITY AND SUPPRESSION FOR EACH SPECIES
        !         FOR BIOLOGICAL CONTROL EFFECTS.
        !         CONVERT FROM SUM(LOG(SURVIVAL)) TO MORTALITY; SAME FOR SUPPRESSION
        !         BOUND FOR NUMERICAL CONTROL

        do N = 1,ACTIVE
           do L = 1,MXHT
              BCMORT(ISPC,N,L) = 1.0 -&
                   &EXP(MAX(XTiny,BCMORT(ISPC,N,L)))
              do R = 1,MAXBC
                 BCSUPP(ISPC,N,L,R) = 1.0 -&
                      &EXP(MAX(XTiny,BCSUPP(ISPC,N,L,R)))
              end do
           end do
        end do

        do I3 = I1,I2
           I = IND1(I3)

           do j = 1,CRTHRD

              if (DEBUG) write(JOSTND,1) i,j, m
              1 format('DMCYCL-1: i= ',I4, ' j= ', I4, ' m= ', I4)

              !             CONVERT THE NEW INPUTS INTO MESH BASED ENSITY AND ADD TO EXISTING POOLS.
              !             TVOL(I,J) SHOULD NEVER BE ZERO, BUT IN PRACTICE SOMETIMES IS.

              if (DEBUG) write(JOSTND,2)&
                   &NewSpr(i,j), NewInt(i,j), TVol(i,j)
              2 FORMAT('DMCYCL-2: NewSpr=',F10.6, ' NewInt= ', F10.6, ',  &
                   &          TVol= ', F10.6)

              if (TVol(i,j) .gt. DMTINY) then
                 New = (NewSpr(i,j) + NewInt(i,j)) / TVol(i,j)
              else
                 New = DMTINY
              end if

              if (DEBUG) write(JOSTND,3)&
                   &NewSpr(i,j), NewInt(i,j), TVol(i,j)
              3 FORMAT('DMCYCL-3: NewSpr=',F10.6, ' NewInt= ', F10.6, ',  &
                   &          TVol= ', F10.6)

              !             MESH-POSITION OF EACH CROWNTHIRD; NEEDED FOR BCMORT, BCSUPP
              IHT = MIDHT(I,J)

              xImm = DMINF(i,j,IMMAT)
              xLat = DMINF(i,j,LATENT)
              xSpr = DMINF(i,j,SUPRSD)
              xAct = DMINF(i,j,ACTIVE)
              do L = 1,MAXBC
                 xImmBC(L) = DMINF_BC(i,j,IMMAT, L)
                 xLatBC(L) = DMINF_BC(i,j,LATENT,L)
                 xSprBC(L) = DMINF_BC(i,j,SUPRSD,L)
                 xActBC(L) = DMINF_BC(i,j,ACTIVE,L)
              end do
              xDedBC = DMINF(i,j,DEAD_BC)

              if (DEBUG) write(JOSTND,4)
              4 format('DMCYCL-4')

              !             COMPUTE AND TRANSFER TO BC-KILL: IMM,LAT,SPR,ACT
              xImm = xImm * (1.0 - BCMORT(ISPC,IMMAT,IHT))
              xLat = xLat * (1.0 - BCMORT(ISPC,LATENT,IHT))
              SprDedBC = xSpr * BCMORT(ISPC,SUPRSD,IHT)
              ActDedBC = xAct * BCMORT(ISPC,ACTIVE,IHT)
              xSpr = xSpr - SprDedBC
              xAct = xAct - ActDedBC

              !             COMPUTE AND TRANSFER TO BC-SUPPRESSED FOR IMM,LAT,SPR,ACT
              do L = 1,MAXBC
                 ImmImmBC(L) = xImm * BCSUPP(ISPC,IMMAT, IHT,L)
                 LatLatBC(L) = xLat * BCSUPP(ISPC,LATENT,IHT,L)
                 SprSprBC(L) = xSpr * BCSUPP(ISPC,SUPRSD,IHT,L)
                 ActActBC(L) = xAct * BCSUPP(ISPC,ACTIVE,IHT,L)
                 xImm = xImm - ImmImmBC(L)
                 xLat = xLat - LatLatBC(L)
                 xSpr = xSpr - SprSprBC(L)
                 xAct = xAct - ActActBC(L)
              end do

              !             COMPUTE TRANSFERS FOR LIFE-HISTORY POOLS
              !             BASED IB LIGHT AND BC
              ImmLat = xImm * FProp2
              LatAct = xLat * FProp(i,j)
              SprAct = xSpr * FProp(i,j)
              ActSpr = xAct * BProp(i,j)
              do L = 1,MAXBC
                 ImmBCImm(L) = xImmBC(L) * (1.0 - BC(L)%HfLf(IMMAT))
                 LatBCLat(L) = xLatBC(L) * (1.0 - BC(L)%HfLf(LATENT))
                 SprBCSpr(L) = xSprBC(L) * (1.0 - BC(L)%HfLf(SUPRSD))
                 ActBCAct(L) = xActBC(L) * (1.0 - BC(L)%HfLf(ACTIVE))
              end do

              !             TRANSFER BY REMOVAL AMONG POOLS
              xImm = xImm - ImmLat
              xLat = xLat - LatAct
              xSpr = xSpr - SprAct
              xAct = xAct - ActSpr
              do L = 1,MAXBC
                 xImmBC(L) = xImmBC(L) - ImmBCImm(L)
                 xLatBC(L) = xLatBC(L) - LatBCLat(L)
                 xSprBC(L) = xSprBC(L) - SprBCSpr(L)
                 xActBC(L) = xActBC(L) - ActBCAct(L)
              end do

              !             TRANSFER BY ADDITION AMONG POOLS
              xLat = xLat + ImmLat
              xSpr = xSpr + ActSpr
              xAct = xAct + LatAct
              xAct = xAct + SprAct
              do L = 1,MAXBC
                 xImm = xImm      + ImmBCImm(L)
                 xLat = xLat      + LatBCLat(L)
                 xSpr = xSpr      + SprBCSpr(L)
                 xAct = xAct      + ActBCAct(L)
                 xImmBC(L) = xImmBC(L) + ImmImmBC(L)
                 xLatBC(L) = xLatBC(L) + LatLatBC(L)
                 xSprBC(L) = xSprBC(L) + SprSprBC(L)
                 xActBC(L) = xActBC(L) + ActActBC(L)
              end do
              xDedBC = xDedBC + SprDedBC
              xDedBC = xDedBC + ActDedBC

              !             REMOVE BY NATURAL MORTALITY: ALL POOLS
              xImm = xImm * SpSurv
              xLat = xLat * SpSurv
              xSpr = xSpr * SpSurv
              xAct = xAct * SpSurv
              do L = 1,MAXBC
                 xImmBC(L) = xImmBC(L) * SpSurv
                 xLatBC(L) = xLatBC(L) * SpSurv
                 xSprBC(L) = xSprBC(L) * SpSurv
                 xActBC(L) = xActBC(L) * SpSurv
              end do
              xDedBC = xDedBC * SpSurv

              !             LAST, ADD NEW SPREAD & INTENSIFICATION TO THE IMM POOL
              xImm = xImm + New

              !             Adjust for "pushback" of DM loading, following M-M style kinetics
              !             RESCALE TO ADJUST ALL POOL SIZES BASED ON THE DMCAP
              !             CARRYING CAPACITY OF THE CROWNTHIRD, USING OBSERVABLE
              !             CATEGORIES. xNow IS THE CURRENT OBSERVABLES, xPrv is the
              !             PREVIOUS, AND X IS THE ADJUSTED VALUE, USED TO RESCALE ALL POOLS.

              !             DM' = DM + [3 - DM] * [1 - EXP(-r * New)]

              xNow = xAct + xSpr
              do L = 1,MAXBC
                 xNow = xNow + xActBC(L) + xSprBC(L)
              end do

              xPrv = DMINF(i,j,ACTIVE) + DMINF(i,j,SUPRSD)
              do L = 1,MAXBC
                 xPrv = xPrv + DMINF_BC(i,j,SUPRSD,L) +&
                      &DMINF_BC(i,j,ACTIVE,L)
              end do

              New = xPrv + (DMCAP(ISPC) - xPrv) *&
                   &(1.0 - EXP((-1.0/DMCAP(ISPC)) * (xNow-xPrv)))

              if (DEBUG) write(JOSTND,5) New, xNow
              5 format('DMCYCL-5; New= ', F10.6, ', xNow= ', F10.6)

              tf1 = .false.
              tf2 = .false.

              if (DEBUG) write(JOSTND,6) tf1, tf2
              6 format('DMCYCL-6; tf1= ', L6, ' tf2= ', L6)

              !              tf1 = isnan(New/xNow)
              !              IF(DEBUG) WRITE(JOSTND,7) tf1
              !    7         FORMAT('DMCYCL-7; tf1= ', L6)

              !              x = New/xNow
              !              tf2 = isnan(x)
              !              IF(DEBUG) WRITE(JOSTND,8) tf2
              !    8         FORMAT('DMCYCL-8 isnan= ', L6)

              if (xNow > DMTINY) then
                 x = New/xNow
              else
                 x = 1.0
              end if

              if (DEBUG) write(JOSTND,9) x
              9 format('DMCYCL-9; x= ', F10.6)

              xImm = xImm * x
              xLat = xLat * x
              xSpr = xSpr * x
              xAct = xAct * x
              do L = 1,MAXBC
                 xImmBC(L) = xImmBC(L) * x
                 xLatBC(L) = xLatBC(L) * x
                 xSprBC(L) = xSprBC(L) * x
                 xActBC(L) = xActBC(L) * x
              end do
              xDedBC = xDedBC * x

              !             After the one-time spinup period, adjust the pools so that they
              !             agree with the inventory.
              x = 1.0
              if (m .eq. Spin .and..not. ZpDn) then
                 xNow = xAct + xSpr
                 do L = 1,MAXBC
                    xNow = xNow + xActBC(L) + xSprBC(L)
                 end do
                 if (xNow > DMTINY) then
                    x = xOriginal(i,j)/xNow
                 else
                    x = 1.0
                 end if

                 !                if (isnan(x)) then
                 !                  x = 1.0
                 !                endif
              end if

              !             WRITE THE TEMPORARY VALUES BACK TO PERMANENT VARIABLES
              DMINF(i,j,IMMAT) = xImm * x
              DMINF(i,j,LATENT) = xLat * x
              DMINF(i,j,SUPRSD) = xSpr * x
              DMINF(i,j,ACTIVE) = xAct * x
              do L = 1,MAXBC
                 DMINF_BC(i,j,IMMAT,L) = xImmBC(L) * x
                 DMINF_BC(i,j,LATENT,L) = xLatBC(L) * x
                 DMINF_BC(i,j,SUPRSD,L) = xSprBC(L) * x
                 DMINF_BC(i,j,ACTIVE,L) = xActBC(L) * x
              end do
              DMINF(i,j,DEAD_BC) = xDedBC * x

           end do ! end of j CRTHRD loop
        end do ! end of i tree loop
     99 end do ! end of ISPC/99 species loop
  end do ! end of m IFINT year loop (cycle or cycle + Spinup)
  ZpDn = .true.

  if (DEBUG) write(JOSTND,999)
  999 format('DMCYCL-999: leaving sub')

  return
end
