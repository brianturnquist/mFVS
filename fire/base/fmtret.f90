!========================!
!   fire/base : fmtret   !
!========================!
! FIRE-BASE $Id$
subroutine FMTRET (IYR)
  implicit none

  !     SINGLE-STAND VERSION
  !     CALLED FROM: FMMAIN
  !     CALLS:   FMCONS
  !              FMMOIS
  !              FMSSEE
  !              FMSADD

  !  Purpose:
  !     This routine activates the fuel treatment options (jackpot burns
  !     and pile burns).  CWD is moved into the piled category, and then
  !     FMCONS is called to calculate the fuel consumption. Any remain
  !     fuel is considered unpiled, and is moved back to the unpiled category.
  !     Some tree mortality may occur, if the user requests it.

  !  Local variable definitions:
  !     AFFECT:  % area in the stand that is affected by the treatment.
  !     ATREAT:  % of affected area in which the fuel is piled and burned.
  !     FULCON:  % of fuel from the affected area that is in the burned area
  !     TRMORT:  % trees (in stand) which are killed by the treatment.

  !  Common block variables and parameters:

  !.... Parameter statements.

  !.... Parameter include files.
  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'

  !.... Common include files.
  include 'FMCOM.f90'
  include 'FMFCOM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'

  !.... Variable declarations.
  ! Variables
  integer :: FMOIS
  real    :: AFFECT,ATREAT,FULCON,TRMORT,PILE
  real    :: PRMS(5)
  integer :: MYACTS(1)
  logical :: DEBUG
  data      MYACTS /2523/
  integer :: IYR,JTODO,JDO,NPRM,IACTK,JYR,K,ISPD,ISZ,I
  real    :: PSMOKE,TRKIL

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'FMTRET',6,ICYC)
  if (DEBUG) write(JOSTND,7) ICYC
  7 format(' ENTERING FMTRET CYCLE=',I3)

  !.... Begin routine.

  !     First, check to see if fuel treatment (FUELBURN KEYWORD) is to
  !     be done this year.

  LFLBRN = .false.
  call OPFIND(1,MYACTS,JTODO)
  if (JTODO .le.0) return
  do JDO = 1,JTODO
     call OPGET(JDO,5,JYR,IACTK,NPRM,PRMS)
     !        IF (JYR .EQ. IYR) THEN
     AFFECT = PRMS(2) / 100.0
     ATREAT = PRMS(3) / 100.0
     FULCON = PRMS(4) / 100.0
     TRMORT = MIN(MAX(0.,PRMS(5) / 100.0),1.)
     LFLBRN = .true.
     call OPDONE(JDO,IYR)
     !        ENDIF
  end do

  if (.not. LFLBRN) return

  if (DEBUG) write(JOSTND,8) AFFECT,ATREAT,FULCON,TRMORT,LFLBRN
  8 format(' IN FMTRET AFFECT=',F10.3,' ATREAT=',F10.3,&
       &' FULCON=',F10.3,' TRMORT=',F10.3,' LFLBRN=',L2)

  !     CWD:     Array for Coarse Woody Debris. Indexed 4 ways:
  !              (1:3,  -,  -,  -) 1 = Unpiled, 2 = Piled, 3 = summation
  !              (  -,1:11,  -,  -) 1 = <.25 in,...,duff (standard breakpoints)
  !              (  -,  -,1:2,  -) 1 = SOFT, 2 = HARD
  !              (  -,  -,  -,1:5) 1 = V.Slow,..., 4 = Fast decay rate

  !     The amount of fuel burned is the amount that is in the affected area
  !     (CWD*AFFECT) times the amount from the affected area that is in the
  !     treated area (*FULCON).  Transfer this amount from the unpiled to the
  !     piled.  For litter and duff, only the material under the piles gets burned,
  !     so we multiply by ATREAT instead.

  do K = 1,2
     do ISPD = 1,4
        do ISZ = 1,9
           PILE = CWD(1,ISZ,K,ISPD) * AFFECT * FULCON
           CWD(1,ISZ,K,ISPD) = CWD(1,ISZ,K,ISPD) - PILE
           CWD(2,ISZ,K,ISPD) = CWD(2,ISZ,K,ISPD) + PILE
        end do
        do ISZ = 10,11
           PILE = CWD(1,ISZ,K,ISPD) * AFFECT * ATREAT
           CWD(1,ISZ,K,ISPD) = CWD(1,ISZ,K,ISPD) - PILE
           CWD(2,ISZ,K,ISPD) = CWD(2,ISZ,K,ISPD) + PILE
        end do
     end do
  end do

  !     Call the fuel consumption routine (FMCONS) to burn the piled stuff.
  !        **(WE STILL NEED TO ASSIGN SOME MOISTURE LEVELS SO CAN CALCULATE
  !           SMOKE PRODUCTION PROPERLY)**
  !           (for now, assume that it is a medium moisture level)

  FMOIS = 3
  call FMMOIS(FMOIS, MOIS)
  call FMCONS(FMOIS, 1, AFFECT*ATREAT, IYR, 0, PSMOKE,100.)

  !     Now, transfer any remaining piled stuff back into the unpiled pool.
  do K = 1,2
     do ISPD = 1,4
        do ISZ = 1,MXFLCL
           PILE = CWD(2,ISZ,K,ISPD)
           CWD(2,ISZ,K,ISPD) = CWD(2,ISZ,K,ISPD) - PILE
           CWD(1,ISZ,K,ISPD) = CWD(1,ISZ,K,ISPD) + PILE
        end do
     end do
  end do

  !     Now kill some trees, if requested.
  !     Also set PBurnYR, since this will determine sprout ages in fmkill

  PBURNYR = IYR
  if (TRMORT .gt.0.0) then
     do I = 1,ITRN
        TRKIL = FMPROB(I) * TRMORT
        if (TRKIL.ge.FMPROB(I)) then ! watch out for rounding errors
           TRKIL = FMPROB(I)
           FMPROB(I) = 0.
        else
           FMPROB(I) = MAX(0.,FMPROB(I) - TRKIL)
        end if
        CURKIL(I) = CURKIL(I) + TRKIL
        FIRKIL(I) = FIRKIL(I) + TRKIL

        !         Store the fire-caused mortality in the snag mgmt routines for
        !         addition to the snag pools.  R&C 07/11/96

        call FMSSEE (I,ISP(I),DBH(I),HT(I),CURKIL(I),&
             &1,DEBUG,JOSTND)

        !         Include call to fmscro to make sure crown of trees killed by pileburns
        !         aren't getting missed.  SAR May 2014

        call FMSCRO(I,ISP(I),IYR,TRKIL,1)

     end do

     !        Store the fire-caused mortality in the snag pools. R&C 07/11/96

     call FMSADD (IYR,1)

  end if

  return
end

subroutine FMFMOV(IYR)

  !     CALLED FROM: FMMAIN
  !     This routine has been extracted from FMCWD.
  !     If desired, it could be made into a seperate file/routine.
  !     Comments below will help in that process.

  !  Purpose:
  !     This subroutine implements the FUELMOVE keyword

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'FMCOM.f90'
  include 'FMFCOM.f90'
  include 'CONTRL.f90'

  !.... Variable declarations.
  !         These are declared above
  ! Variables
  logical :: DEBUG,LALTER
  integer :: I,K,L
  integer :: IYR
  integer :: J1,J2
  integer :: MYACT(1),NTODO,JYR,IACTK,NPRM,IFROM,ITO
  real    :: XGET
  real    :: FTRG(0:MXFLCL),FSRC(0:MXFLCL),FORG(0:MXFLCL),PRMS(6)
  real    :: X,Y,Z,Q

  !     OPTION PROCESSOR CODES FOR
  !     FUELMOVE (2530) - TRANSFER FUEL AMONG CATEGORIES

  data     MYACT/2530/

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'FMFMOV',5,ICYC)
  if (DEBUG) write(JOSTND,107) 'FMFMOV',ICYC
  107 format(' ENTERING ',A,' CYCLE = ',I2)

  !     SEE IF FUELMOVE KEYWORD IS SCHEDULED; TRANSFER AMONG
  !     FUEL POOLS AS REQUIRED

  TONRMC = 0.0
  call OPFIND(1,MYACT,NTODO)
  if (NTODO.eq.0) goto 506

  !     FORG - ORIGINAL VALUE OF SOURCE FUEL POOL - UNALTERED
  !     FTRG - FUEL TO BE PUT IN TARGET POOL - VARIABLE
  !     FSRC - FUEL TO BE TAKEN FROM SOURCE POOL - VARIABLE

  do I = 0,MXFLCL
     FORG(I) = 0.0
     FTRG(I) = 0.0
  end do

  do I = 1,2
     do K = 1,2
        do L = 1,4
           do J1 = 1,MXFLCL
              FORG(J1) = FORG(J1) + CWD(I,J1,K,L)
           end do
        end do
     end do
  end do

  do I = 0,MXFLCL
     FSRC(I) = FORG(I)
  end do

  LALTER = .false.
  do I = 1,NTODO
     call OPGET(I,6,JYR,IACTK,NPRM,PRMS)
     !SB        IF ((IACTK .GE. 0) .AND. (JYR .EQ. IYR)) THEN
     if (IACTK .ge.0) then

        !         IFROM - SOURCE CATEGORY (0,1-11)
        !         ITO   - TARGET CATEGORY (0,1-11)
        !         X     - AMOUNT TO TAKE FROM SOURCE (T/AC)
        !         Y     - PROPORTION TO TAKE FROM SOURCE (0-1 PROPORTION)
        !         Z     - AMOUNT TO LEAVE IN SOURCE (T/AC)
        !         Q     - AMOUNT TO END WITH IN TARGET (T/AC)

        IFROM = INT(PRMS(1))
        ITO = INT(PRMS(2))
        X = PRMS(3)
        Y = PRMS(4)
        Z = PRMS(5)
        Q = PRMS(6)

        !         MIMIC TESTS OF FUELMOVE KEYWORD IN **FMIN**

        if ((IFROM .ge.0) .and. (IFROM .le. MXFLCL) .and.&
             &(ITO   .ge.0) .and. (ITO   .le. MXFLCL) .and.&
             &(IFROM .ne. ITO) .and.&
             &(X .ge.0.0) .and.&
             &(Y .ge.0.0) .and. (Y .le.1.0) .and.&
             &(Z .ge.0.0)) then

           !           CHOOSE THE GREATER OF THE 3 REMOVALS (IGNORE IF IMPORTING: IFROM=0)
           !           CONSTRAIN XGET TO TAKE ONLY FUEL THAT IS PRESENT.

           XGET = 0.0
           if (IFROM .gt.0) then

              if ((FSRC(IFROM) .le.0.0)) then
                 call OPDEL1(I)
                 goto 550
              end if

              !             CHOOSE MAXIMUM OF THESE 3, CONVERTING TO T/A:

              !               X             - AMOUNT TO TAKE FROM SOURCE (T/AC)
              !               Y*FSCR(IFROM) - PROPORTION TO TAKE FROM SOURCE (0-1 PROPORTION)
              !               FSCR(IFROM)-Z - RESIDUAL AMOUNT TO LEAVE IN SOURCE (T/AC)
              !               Q-FSRC(ITO)   - FINAL AMOUNT TO HAVE IN TARGET (T/AC)

              !             NOTE THAT THE AMOUNT IS BASED ON THE CURRENT AMOUNT (DECLINING
              !             BALANCE) AND NOT ON THE AMOUNT PRESENT AT THE BEGINNING OF THE
              !             KEYWORD PROCESSING FOR THIS YEAR. 'Q' IS NOT IN USE IF IT IS LESS
              !             THAN ZERO.

              if (Q .ge.0.0) then
                 XGET = MAX(X, Y*FSRC(IFROM), FSRC(IFROM)-Z, Q-FSRC(ITO))
              else
                 XGET = MAX(X, Y*FSRC(IFROM), FSRC(IFROM)-Z)
              end if

              if (XGET .gt. FSRC(IFROM)) XGET = FSRC(IFROM)
              PRMS(3) = XGET
              PRMS(4) = XGET/(FSRC(IFROM))
              PRMS(5) = FSRC(IFROM) - XGET
              PRMS(6) = FSRC(ITO) + XGET
              FSRC(IFROM) = FSRC(IFROM) - XGET
              if (ITO .eq.0) TONRMC = TONRMC + XGET
           else
              if (Q .ge.0.0) then
                 XGET = MAX(X,Q-FSRC(ITO))
              else
                 XGET = X
              end if

              PRMS(3) = XGET
              PRMS(4) = 0.0
              PRMS(5) = 0.0
              PRMS(6) = FTRG(ITO) + XGET
              TONRMC = TONRMC - XGET
           end if
           FTRG(ITO) = FTRG(ITO) + XGET

           !           RECORD ACTIVITIES OR CANCEL THOSE THAT MOVE NOTHING

           if (XGET .gt.0.0) then
              call OPCHPR(I,6,PRMS)
              call OPDONE(I,IYR)
              LALTER = .true.
           else
              call OPDEL1(I)
           end if
        else
           call OPDEL1(I)
        end if
        550 continue
     end if
  end do

  !     ALTER ORIGINAL FUELS IF ANY OPTIONS WERE PROCESSED
  !     STORE NEW POOL VALUES IN IN FTRG.
  !     - SKIP ASSIGNMENT TO SUBPOOLS IF THERE HAS BEEN NO CHANGE
  !     - IN THE CASE WHERE FUEL IS ADDED TO A PREVIOUSLY EMPTY CATEGORY
  !       ADD ALL THE FUEL TO UNPILED(I=1), HARD(K=2), FAST(L=3);
  !      -OTHERWISE ADD IN PROPORTION TO THE EXISTING FUEL, BY CREATING
  !       A SCALAR 'X' TO MODIFY THE EXISTING FUEL.

  if (LALTER) then
     do I = 0,MXFLCL
        FTRG(I) = FSRC(I) + FTRG(I)
     end do
     do J1 = 1,MXFLCL
        if (ABS(FORG(J1) - FTRG(J1)) .ge.1.0E-6) then
           if (FORG(J1) .le.1.0E-6) then
              CWD(1,J1,2,3) = FTRG(J1)
           else
              X = FTRG(J1)/FORG(J1)
              do K = 1,2
                 do L = 1,4
                    do I = 1,2
                       CWD(I,J1,K,L) = CWD(I,J1,K,L) * X
                    end do
                 end do
              end do
           end if
        end if
     end do
  end if

  !     CALCULATE LARGE AND SMALL FUEL; PILED AND UNPILED.

  506 OLARGE = LARGE
  OSMALL = SMALL

  LARGE = 0.0
  SMALL = 0.0

  do I = 1,2
     do K = 1,2
        do L = 1,4
           do J1 = 1,3
              SMALL = SMALL + CWD(I,J1,K,L)
           end do
           SMALL = SMALL   + CWD(I, 10,K,L) ! LITTER IS SMALL
           do J2 = 4,9
              LARGE = LARGE + CWD(I,J2,K,L)
           end do
        end do
     end do
  end do

  !     COMPUTE PERCENT CHANGE IN FUELS; TRIGGERS
  !     ACTIVITY FUELS LOGIC IN **FMCFMD**

  X = OLARGE + OSMALL
  if (X .gt.1.0E-6) then
     SLCHNG = 100.0 * (LARGE + SMALL - X) / X
  else
     SLCHNG = 0.0
  end if

  return
end
