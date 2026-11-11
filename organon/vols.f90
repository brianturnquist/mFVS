!====================!
!   organon : vols   !
!====================!
! ORGANON $Id$
subroutine VOLS
  implicit none

  !  THIS SUBROUTINE CALCULATES TREE VOLUMES TO THREE MERCHANTABILITY
  !  STANDARDS, AND COMPUTES DISTRIBUTION AND COMPOSITION VECTORS FOR
  !  TOTAL CUBIC VOLUME AND ACCRETION.  TOTAL CUBIC FOOT VOLUME AND
  !  MERCHANTABLE CUBIC FOOT VOLUME ARE COMPUTED IN **CFVOL**,
  !  **NATCRS**, OR **OCFVOL**.  BOARD FOOT VOLUME IS COMPUTED IN
  !  **BFVOL**, **NATCRS**, OR **OBFVOL**.  ADJUSTMENTS ARE MADE FOR
  !  TOP DAMAGE AS APPROPRIATE.  ALL VOLUMES ARE THEN ADJUSTED FOR
  !  DEFECT.

  !  NATCRS, OCFVOL, AND OBFVOL ARE ENTRY POINTS IN SUBROUTINE
  !  **VARVOL**, WHICH IS VARIANT SPECIFIC.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'
  include 'OUTCOM.f90'
  include 'PLOT.f90'
  include 'VOLSTD.f90'
  include 'GGCOM.f90'
  include 'ORGANON.f90'
  include 'wdbkwtdata.inc'

  !  DIMENSIONS FOR INTERNAL VARIABLES.

  !  SPCCC -- TOTAL CUBIC VOLUME BY SPECIES AND TREE CLASS.
  !  SPCAC -- TOTAL CUBIC VOLUME ACCRETION BY SPECIES AND TREE CLASS.
  !  SPCMC -- MERCH CUBIC VOLUME BY SPECIES AND TREE CLASS.
  !  SPCBV -- MERCH BOARD VOLUME BY SPECIES AND TREE CLASS.

  !  THESE VALUES DO NOT INCLUDE CYCLE 0 DEAD TREES.
  !  IPASS=1 FOR LIVE TREES;  IPASS=2 FOR CYCLE 0 DEAD TREES.

  !  BTKFLG - LOGICAL VARIABLES TO INDICATE WHETHER THE VOLUME ESTIMATE
  !  CTKFLG   NEEDS TO BE ADJUSTED FOR TOPKILL OR NOT.
  !           BTKFLG FOR BOARD FOOT VOLUME
  !           CTKFLG FOR CUBIC FOOT VOLUME
  !           .TRUE.  = ADJUST ESTIMATE FOR TOPKILL
  !           .FALSE. = NO ADJUSTMENT NEEDED (ALREADY ACCOUNTED FOR).

  ! Variables
  integer :: DLIEQN,DLLMOD
  logical :: DEBUG
  real    :: SPCCC(MAXSP,3),SPCAC(MAXSP,3),SPCBV(MAXSP,3),SPCMC(MAXSP,3)
  real    :: SPCSC(MAXSP,3)
  real    :: DBHCLS(9),P,D,H,BARK,BRATIO,CARBFACTOR
  real    :: D2H,VM,VN,VMAX,BFMAX,BBFV,ALGSLP,VOLCOR,TCFTD,TEMVOL
  real    :: TCF,MCF,SCF,BIOMAS(15)
  integer :: I,J,IPASS,ILOW,IHI,ISPC,IT,IM,ICDF,IBDF,ORGFIA,IFIASP,K
  logical :: TKILL,LCONE,BTKFLG,CTKFLG
  character(len=1) :: LIVEDEAD

  !  ORGANON: DEFINE THE VARIABLES REQUIRED BY THE ORGANON ORGVOLS DLL
  !           AND THE VOLCAL INTERFACE FUNCTION

  integer*4 :: SPP ! SPECIES CODE FOR TREE RECORD
  real*4  :: FCR ! FLOATING POINT CROWN RATIO, UNITLESS.
  integer*4 :: ORGVOL_VERROR(5) ! VOLUME ERROR FLAGS
  integer*4 :: ORGVOL_TERROR(4) ! TREE ERROR FLAGS
  integer*4 :: ORGVOL_VWARNING(5) ! TREE WARNING FLAGS
  integer*4 :: ORGVOL_TWARNING ! TREE THREW A WARNING
  integer*4 :: ORGVOL_IERROR ! SOME GENERIC ERROR FLAG
  integer*4 :: Z ! TEMP COUNTER FOR WARNING/ERRORS

  !  INITIALIZE DBH CLASSES.

  data DBHCLS/0.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0/

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'VOLS',4,ICYC)
  do J = 1,2
     do I = 1,MAXTRE
        HT2TD(I,J) = 0.
     end do
  end do
  if (ITRN.le.0) goto 2
  do I = 1,MAXSP
     do J = 1,3
        SPCCC(I,J) = 0.0
        SPCAC(I,J) = 0.0
        SPCMC(I,J) = 0.0
        SPCSC(I,J) = 0.0
        SPCBV(I,J) = 0.0
     end do
  end do
  2 continue
  IPASS = 1
  ILOW = 1
  IHI = ITRN

  !  CALL VOLKEY TO PROCESS KEYWORDS USED TO CHANGE VOLUME STANDARDS AND
  !  EQUATIONS.

  call VOLKEY (DEBUG)
  do J = 1,MAXSP
     if (BFMIND(J).lt.2.0) BFMIND(J) = 2.0
     if (BFTOPD(J).lt.2.0) BFTOPD(J) = 2.0
  end do

  if (ITRN.le.0) goto 205

  !  ENTER TREE BY TREE LOOP TO CALCULATE VOLUMES AND COMPILE
  !  SUMMARY ARRAYS.

  !  I -- SUBSCRIPT TO TREE RECORD.
  !  P -- NUMBER OF TREES PER ACRE REPRESENTED BY TREE I.
  !  D -- DIAMETER OF TREE I.
  !  H -- HEIGHT OF TREE I.
  !  ISPC -- SPECIES OF TREE I.
  !  WK5 -- ARRAY CONTAINING TOTAL ANNUAL CUBIC VOLUME ACCRETION
  !    PER ACRE FOR EACH TREE.  THIS ARRAY IS NOT LOADED
  !    IF LSTART IS TRUE.
  !  VN -- USED TO TEMPORARILY STORE TOTAL CUBIC VOLUME FOR TREE I.

  10 continue
  do I = ILOW,IHI
     WK5(I) = 0.0
     P = PROB(I)
     if (P.le.0.0) go to 200
     ISPC = ISP(I)

     !  CLEAR THE LOGS PASSED TO ECON SO A TREE WITHOUT NVEL VOLUMES
     !  ISN'T PRICED ON ANOTHER TREE'S LOGS.
     CFLDIA = 0.0
     CFLVOL = 0.0
     BFLDIA = 0.0
     BFLVOL = 0.0
     D = DBH(I)
     H = HT(I)

     if (IMC(I) .ge.6) then
        LIVEDEAD = 'D'
     else
        LIVEDEAD = 'L'
     end if

     !  INITIALIZE TOP KILL FLAG FOR NEXT TREE; IF TOPKILLED, ASSIGN H TO
     !  NORMHT.

     TKILL = H.ge.4.5.and. ITRUNC(I).gt.0
     if (TKILL) H = NORMHT(I)/100.0

     !  IF NOT INITIAL SUMMARY, ADD DG TO DBH; ASSIGN D2H.

     BARK = BRATIO(ISPC,D,H)
     if (.not.LSTART) D = D+DG(I)/BARK
     D2H = D*D*H

     !  ORGANON: IF USING THE ORGVOLS DLL,
     !  COMPUTE THE VOLUMES USING THE VOLCAL SUBROUTINE
     !  USE HT(I) INSTEAD OF NORMHT(I) FOR TRUNCATED TREES  GED 11/24/17

     if ( LORGVOLS ) then
        H = HT(I)
        TCF = 0.
        MCF = 0.
        SCF = 0.
        VMAX = 0.
        BFMAX = 0.
        CFV(I) = 0.
        !        WK1(I)=0.
        MCFV(I) = 0.
        SCFV(I) = 0.
        BFV(I) = 0.

        ! SET FIA SPECIES CODE (ACTUAL FOR VALID ORGANON SPECIES, SURROGATE
        ! FOR NON-VALID SPECIES.

        call ORGSPC(ISP(I),ORGFIA)
        SPP = ORGFIA

        !  GET THE SPECIES GROUP FOR THE TREE RECORD.
        !  EXTRACT THE FIA SPECIES CODE FROM THE FVS DATA.

        if (DEBUG) write(JOSTND,*)' CALLING VOLCAL SPP= ',SPP

        !  CONVERT THE CROWN RATIO FROM THE INTEGER (FVS) TO ORGANON'S REQUIRED FLOAT

        FCR = real(ICR(I)) / 100.0 ! MEASURED/PREDICTED CROWN RATIO

        !  SET TEMPORARY VARIABLE FOR CUBIC FOOT TOP DIAMETER DEPENDING ON
        !  WHETHER THE TREE IS A SOFTWOOD OR HARDWOOD.

        TCFTD = CFTD
        if (SPP .ge.300) TCFTD = CFTDHW

        !  ORGANON: CALL THE ORGVOL.DLL VOLCALC SUBROUTINE

        IF(DEBUG)WRITE(JOSTND,*)' CALLING VOLCAL TCFTD,CFSH,LOGTD,LOGTA,&
             &   LOGSH,LOGLL,LOGML= ',TCFTD,CFSH,LOGTD,LOGTA,LOGSH,LOGLL,LOGML
        if (DEBUG)write(JOSTND,*)' I,D,H,FCR= ',I,D,H,FCR
        call VOLCAL(      IMODTY,SPP,&
             &TCFTD,CFSH,&
             &LOGLL,LOGML,LOGTD,LOGSH,LOGTA,&
             &D,H,FCR,&
             &ORGVOL_VERROR,ORGVOL_TERROR,ORGVOL_VWARNING,&
             &ORGVOL_TWARNING,ORGVOL_IERROR,&
             &MCF,BBFV)

        !  ORGANON: PERFORM A BASIC ERROR CHECK.
        !         INTEGER*4   VERROR(5)  ! VOLUME ERROR FLAG BUFFER
        !         INTEGER*4   TERROR(4)  ! TREE ERROR FLAG BUFFER
        !         INTEGER*4   VWARNING(5) ! VOLUME WARNING FLAG BUFFER
        !         INTEGER*4   TWARNING   ! TREE VARIABLE WARNING FLAG
        !         INTEGER*4   IERROR     ! SOME GENERIC ERROR FLAG

        !  ORGANON: PROCESS THE TWARNING FLAG

        if ( ORGVOL_TWARNING .eq.1 ) then
           if (DEBUG) write(JOSTND,12) ICYC, I, ORGVOL_TWARNING
           12 format(' CALLING ORGANON DLL::VOLCAL::BETA, CYCLE=',I2,&
                &' IDX=',I2,&
                &' TWARNING=',I2 )
        end if
        if ( ORGVOL_IERROR .eq.1 ) then
           if (DEBUG) write(JOSTND,13) ICYC, ORGVOL_IERROR
           13 format(' CALLING ORGANON.DLL::VOLCAL::BETA, CYCLE=',I2,&
                &' ORGVOL_IERROR=',I2 )

           !  ORGANON: PROCESS THE VOLUME WARNINGS AND ERRORS ARRAYS
           !           VWARNING(5) AND VERROR(5)

           do Z = 1, 5
              if ( ORGVOL_VERROR(Z) .eq.1 ) then
                 if (DEBUG) write(JOSTND,14) ICYC, I, ORGVOL_VERROR(Z),&
                      &ORGVOL_VWARNING(Z)
                 14 format(' CALLING ORGANON DLL::VOLCAL::BETA, CYCLE=',I2,&
                      &' IDX=',I2,&
                      &' VERROR=',I2,&
                      &' VWARNING=',I2 )
              end if
           end do

           !  ORGANON: PROCESS THE TREE WARNING/ERRORS
           !           TWARNING AND TERROR(4)

           do Z = 1, 4
              if ( ORGVOL_TERROR(Z) .eq.1 ) then
                 if (DEBUG) write(JOSTND,16) ICYC, I, ORGVOL_TERROR(Z)
                 16 format(' CALLING ORGANON DLL::VOLCAL::BETA, CYCLE=',I2,&
                      &' IDX=',I2,&
                      &' TERROR=',I2 )
              end if
           end do
        end if

        !  ORGANON: ASSIGN THE FVS INTERNAL VARIABLES
        !           TO THE VALUES SET BY THE ORGANON DLL

        TCF = MCF
        CFV(I) = TCF
        MCFV(I) = MCF
        BFV(I) = BBFV
        if (DEBUG)write(JOSTND,*)' IN VOLS AFTER VOLCAL I,VM,BBFV= ',&
             &I,VM,BBFV

        !  CALL ECON EXTENSION TO PROCESS VOLUME INFORMATION

        if (MCFV(I) .gt.0.0) call ECVOL(IT,CFLDIA,CFLVOL,.true.)
        if (BFV(I) .gt.0.0) call ECVOL(IT,BFLDIA,BFLVOL,.false.)
        go to 200
     end if

     !  ORGANON: END OF ORGANON

     !              CUBIC VOLUME SECTION               *

     !  INITIALIZE VOLUME ESTIMATES.

     VN = 0.0
     VM = 0.0

     TCF = 0.0
     MCF = 0.0
     SCF = 0.0

     VMAX = 0.0
     BFMAX = 0.0
     BBFV = 0.0

     BIOMAS = 0.
     if (DEBUG)write(JOSTND,*)' CUBIC SECTION, I,ISPC,METHC= ',&
          &I,ISPC,METHC(ISPC)

     !  CALCULATE TOTAL CUBIC FOOT VOLUME. CORRECT FOR TOP KILL IF NEEDED.

     IT = I
     if (METHC(ISPC).eq.6.or. METHC(ISPC).eq.10) then
        call NATCRS (TCF,MCF,SCF,BBFV,ISPC,D,H,TKILL,&
             &ICR(I),BARK,ITRUNC(I),&
             &VMAX,BFMAX,CULL(I),DECAYCD(I),WDLDSTEM(I),&
             &BIOMAS,LIVEDEAD,CTKFLG,BTKFLG,IT)
     else if (METHC(ISPC).eq.8) then
        call OCFVOL (VN,VM,ISPC,D,H,TKILL,BARK,ITRUNC(I),VMAX,LCONE,&
             &CTKFLG,IT)
     else
        call CFVOL (ISPC,D,H,D2H,VN,VM,VMAX,TKILL,LCONE,BARK,ITRUNC(I),&
             &CTKFLG)
     end if
     if (VEQNNC(ISPC)(1:3) .ne. 'NVB'.and..not. LFIANVB .and.&
          &(CTKFLG .and. TKILL .and. VMAX .gt.0.))&
          &call CFTOPK (ISPC,D,H,TCF,MCF,SCF,VMAX,LCONE,BARK,ITRUNC(I))

     !  LOAD WK1 WITH MERCH CUBIC VOLUME PER TREE.
     !  WK1 array has been replaced with MCFV

     !      WK1(I)=VM
     MCFV(I) = MCF
     if (LFIANVB) then
        read(FIAJSP(ISPC),'(I4)')IFIASP
        do K = 1,2677
           if (WDBKWT(K,1).eq.IFIASP) then
              CARBFACTOR = WDBKWT(K,12)
              exit
           end if
        end do
        ABVGRD_BIO(I) = BIOMAS(1)
        FOLI_BIO(I) = BIOMAS(13)
        ABVGRD_CARB(I) = BIOMAS(15)
        FOLI_CARB(I) = BIOMAS(13)*0.5
        !------- MAKE SURE TREE(I) GETS ASSIGNED A VALUE
        MERCH_BIO(I) = 0
        MERCH_CARB(I) = 0
        CUBSAW_BIO(I) = 0
        CUBSAW_CARB = 0

        if (LIVEDEAD.eq.'L' .and. DECAYCD(I).gt.0) DECAYCD(I) = 0
        if (DECAYCD(I).gt.0) then
           select case(DECAYCD(I))
           case(1)
              CARBFACTOR = 0.501
              if (IFIASP.ge.300) CARBFACTOR = 0.47
           case(2)
              CARBFACTOR = 0.504
              if (IFIASP.ge.300) CARBFACTOR = 0.473
           case(3)
              CARBFACTOR = 0.506
              if (IFIASP.ge.300) CARBFACTOR = 0.481
           case(4)
              CARBFACTOR = 0.52
              if (IFIASP.ge.300) CARBFACTOR = 0.48
           case(5)
              CARBFACTOR = 0.527
              if (IFIASP.ge.300) CARBFACTOR = 0.472
           end select
        end if
        CARB_FRAC(I) = CARBFACTOR
        select case (IFIASP)
        case (62,  63,  65,  66, 69, 106, 133, 134,&
             &143, 321, 322, 475, 803, 810, 814, 843)
           MCFV(I) = 0
           SCFV(I) = 0
           MERCH_BIO(I) = 0
           MERCH_CARB(I) = 0
           CUBSAW_BIO(I) = 0
           CUBSAW_CARB(I) = 0
        case default
           if (D .ge. DBHMIN(ISPC)) then
              MERCH_BIO(I) = BIOMAS(6) + BIOMAS(8)
              MERCH_CARB(I) = MERCH_BIO(I) * CARBFACTOR
           end if
           if (D .ge. SCFMIND(ISPC)) then
              CUBSAW_BIO(I) = BIOMAS(6)
              CUBSAW_CARB(I) = CUBSAW_BIO(I)*CARBFACTOR
           end if
        end select
     end if

     !  SUMMARIZE VOLUME BY SPECIES AND TREE CLASS.  IF LSTART IS
     !  FALSE, LOAD WK5 AND SUMMARIZE ACCRETION BY SPECIES AND TREE
     !  CLASS. DO NOT DO THIS FOR CYCLE 0 DEAD TREES.

     if (IPASS .eq.2) go to 15
     IM = IMC(I)
     if (.not.LSTART) then
        if (CFV(I).gt.TCF) then
           WK5(I) = 0.
        else
           WK5(I) = (TCF-CFV(I))*P/FINT
        end if
        SPCAC(ISPC,IM) = SPCAC(ISPC,IM)+WK5(I)
     end if
     SPCCC(ISPC,IM) = SPCCC(ISPC,IM)+TCF*P

     !  LOAD CFV WITH TOTAL CUBIC VOLUME PER TREE.

     15 continue
     CFV(I) = TCF
     ICDF = 0
     TEMVOL = MCFV(I)
     ICDF = DEFECT(I)/1000000
     if (MCFV(I).gt.0.0.and. LCVOLS) then

        !       COMPUTE DEFECT CORRECTION FACTOR FOR CUBIC FOOT VOLUME.
        !       TAKE LARGEST OF 1) INPUT CF DEFECT PERCENT, 2) COMPUTED LINEAR
        !       INTERPOLATION VALUE, OR 3) COMPUTED LOG-LINEAR MODEL VALUE.

        DLIEQN = NINT(ALGSLP(D,DBHCLS,CFDEFT(1,ISPC),9) * 100.)
        if (DLIEQN.gt.ICDF) ICDF = DLIEQN
        if (CFLA0(ISPC).eq.0.0.and. CFLA1(ISPC).eq.1.0) then
           VOLCOR = TEMVOL
        else
           VOLCOR = EXP(CFLA0(ISPC)+CFLA1(ISPC)*ALOG(TEMVOL))
        end if
        if (TEMVOL .eq.0) go to 25
        DLLMOD = NINT(((TEMVOL-VOLCOR)/TEMVOL) * 100.)
        if (DLLMOD.gt.ICDF) ICDF = DLLMOD
        25 continue
        if (ICDF.gt.99) ICDF = 99
        if (ICDF.lt.0) ICDF = 0
     end if

     !        CORRECT MERCHANTABLE CUBIC VOLUME FOR FORM AND DEFECT.
     !        CONSIDER 99% DEFECT AS 100% DEFECT.

     if (ICDF.lt.99) then
        if (.not.LFIANVB) MCFV(I) = MCFV(I)*(1.-FLOAT(ICDF)/100.)
     else
        MCFV(I) = 0.
     end if

     continue

     !  CALL ECON EXTENSION TO PROCESS VOLUME INFORMATION

     if (MCFV(I) .gt.0.0) call ECVOL(IT,CFLDIA,CFLVOL,.true.)
     !      CALL ECVOL(ISPC,IT,LOGDIA,LOGVOL,.TRUE.)

     !           BOARD VOLUME SECTION                  *

     !  COMPUTE SCRIBNER BOARD FOOT VOLUME TO A VARIABLE TOP FOR TREES
     !  OF MINIMUM MERCHANTABLE DBH AND LARGER.

     BFV(I) = 0.0
     IBDF = DEFECT(I)/10000 - (DEFECT(I)/1000000)*100

     if ((D.lt.BFMIND(ISPC).or.D.le.BFTOPD(ISPC)).and.SCFV(I).gt.0) then
        go to 100
     else if (D.lt.BFMIND(ISPC).or.D.le.BFTOPD(ISPC)) then
        go to 150
     end if

     !   CALCULATE SCRIBNER BOARD FOOT VOLUME.

     if (DEBUG)write(JOSTND,*)' BOARD SECTION, I,ISPC,D,H,METHB= ',&
          &I,ISPC,D,H,METHB(ISPC)

     !   IF USING AN NVEL METHOD CODES WITH MATCHING CF/BF EQUATIONS
     !   NO NEED TO CALL NATCRS A SECOND TIME AS DIFFERENCES IN TOP DIAMETER
     !   SPECIFICATION IS HANDLED IN FVSVOLS.F (NATCRS)

     if ((METHB(ISPC).eq.6.or. METHB(ISPC).eq.9).and.&
          &(METHC(ISPC).eq.6.or. METHC(ISPC).eq.10)) then
        go to 100
     else if (METHB(ISPC).eq.6.or. METHB(ISPC).eq.9) then
        call NATCRS (TCF,MCF,SCF,BBFV,ISPC,D,H,TKILL,&
             &ICR(I),BARK,ITRUNC(I),&
             &VMAX,BFMAX,CULL(I),DECAYCD(I),WDLDSTEM(I),&
             &BIOMAS,LIVEDEAD,CTKFLG,BTKFLG,IT)
     else if (METHB(ISPC).eq.8) then
        IT = I
        call OBFVOL (BBFV,ISPC,D,H,TKILL,BARK,ITRUNC(I),BFMAX,LCONE,&
             &BTKFLG,IT)
     else
        call BFVOL (ISPC,D,H,D2H,BBFV,TKILL,LCONE,BARK,BFMAX,ITRUNC(I),&
             &BTKFLG)
     end if

     !  CORRECT FOR TOPKILL IF NEEDED.
     !  LOAD BFV WITH SCRIBNER BOARD FOOT VOLUME CORRECTED FOR TOPKILL.

     100 continue
     if (BTKFLG .and. TKILL .and. BFMAX .gt.0.)&
          &call BFTOPK (ISPC,D,H,BBFV,LCONE,BARK,BFMAX,ITRUNC(I))
     BFV(I) = BBFV
     if (BFV(I).gt.0.0.and. LBVOLS) then

        !     COMPUTE DEFECT CORRECTION FACTOR FOR BOARD FOOT VOLUME.
        !     TAKE LARGEST OF 1) INPUT BF DEFECT PERCENT, 2) COMPUTED LINEAR
        !     INTERPOLATION BF VALUE, OR 3) COMPUTED LOG-LINEAR MODEL BF VALUE.

        DLIEQN = NINT(ALGSLP(D,DBHCLS,BFDEFT(1,ISPC),9) * 100.)
        if (DLIEQN.gt.IBDF)IBDF = DLIEQN
        if (BFLA0(ISPC).eq.0.0.and. BFLA1(ISPC).eq.1.0) then
           VOLCOR = BFV(I)
        else
           VOLCOR = EXP(BFLA0(ISPC)+BFLA1(ISPC)*ALOG(BFV(I)))
        end if
        DLLMOD = NINT(((BFV(I)-VOLCOR)/BFV(I)) * 100.)
        if (DLLMOD.gt.IBDF)IBDF = DLLMOD
        if (IBDF.gt.99)IBDF = 99
        if (IBDF.lt.0)IBDF = 0
     end if

     !        CORRECT MERCHANTABLE BOARD FOOT VOLUME FOR FORM AND DEFECT.
     !        CONSIDER 99% DEFECT AS 100% DEFECT.

     if (IBDF.lt.99) then
        BFV(I) = BFV(I)*(1.-FLOAT(IBDF)/100.)
        SCFV(I) = SCFV(I)*(1.-FLOAT(IBDF)/100.)
     else
        BFV(I) = 0.
        SCFV(I) = 0.
     end if

     !  CALL ECON EXTENSION TO PROCESS VOLUME INFORMATION

     if (BFV(I) .gt.0.0) call ECVOL(IT,BFLDIA,BFLVOL,.false.)
     !      CALL ECVOL(ISPC,IT,LOGDIA,LOGVOL,.FALSE.)

     !  RESET NEGATIVE VOLUMES TO ZERO AND COMPILE TOTAL.

     if (BFV(I).lt.0.0) BFV(I) = 0.0
     150 continue
     if (IPASS .eq.1) then
        SPCBV(ISPC,IM) = SPCBV(ISPC,IM)+ BFV(I)*P
        SPCMC(ISPC,IM) = SPCMC(ISPC,IM)+MCFV(I)*P
        SPCSC(ISPC,IM) = SPCSC(ISPC,IM)+SCFV(I)*P
     end if

     !  SET VALUES IN CODED DEFECT NUMBER TO INDICATE THE ACTUAL DEFECT
     !  PERCENTAGES APPLIED THIS CYCLE.
     !  DEFECT CODED AS 11223344, WHERE 11=INPUT MC DEFECT %, 22=INPUT BF
     !  DEFECT PERCENT, 33=MC DEFECT % USED THIS CYCLE, 44=BF DEFECT % USED
     !  THIS CYCLE.

     DEFECT(I) = ((DEFECT(I)/10000)*10000) + ICDF*100 + IBDF
     if (DEBUG)write(JOSTND,*)' IN VOLS, I= ',I,' DEFECT= ',DEFECT(I)

     !  PRINT DEBUG OUTPUT IF DESIRED

     if (.not.DEBUG) go to 200
     write(JOSTND,9000)I,TCF,MCFV(I),WK5(I),CFV(I),&
          &SPCAC(ISPC,1),SPCAC(ISPC,2),SPCAC(ISPC,3),SPCCC(ISPC,1),&
          &SPCCC(ISPC,2),SPCCC(ISPC,3),ISPC,D,H
     9000 format(' IN VOLS, I=',I4,' TCF=',E15.6,' MCFV=',E15.6,/,&
          &'  WK5=',E15.6,' CFV=',E15.6,&
          &' SPCAC=',3(E15.6,',')/'  SPCCC=',3(E15.6,','),' ISPC=',I2,&
          &' DBH=',E11.2,' NORMAL HT=',E11.2)

     !  END OF TREE LOOP.

     200 continue
  end do
  205 continue
  if (IPASS .eq.2) go to 250

  !  DETERMINE SPECIES-TREE CLASS COMPOSITION FOR ALL VOLUME STANDARDS.

  call COMP(OSPCV,IOSPCV,SPCCC)
  call COMP(OSPBV,IOSPBV,SPCBV)
  call COMP(OSPMC,IOSPMC,SPCMC)
  call COMP(OSPSC,IOSPSC,SPCSC)

  !  DETERMINE SPECIES-TREE CLASS COMPOSITION AND PERCENTILE POINTS IN
  !  THE DISTRIBUTION OF DIAMETERS FOR ANNUAL TOTAL CUBIC FOOT ACCRETION.
  !  BYPASS IF LSTART IS TRUE.

  if (LSTART) go to 210
  call COMP(OSPAC,IOSPAC,SPCAC)
  call PCTILE(ITRN,IND,WK5,WK3,OACC(7))
  call DIST(ITRN,OACC,WK3)
  210 continue

  !  CALCULATE VOLUMES FOR CYCLE 0 DEAD TREES.

  if (IREC2.gt. MAXTRE) go to 250
  IPASS = 2
  ILOW = IREC2
  IHI = MAXTRE
  go to 10

  !  END OF VOLS.

  250 continue
  do I = 1,10
     if (DEBUG)write(JOSTND,*)' LEAVING VOLS, I,CFV,BFV= ',&
          &I,CFV(I),BFV(I)
  end do
  return
end
