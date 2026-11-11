!========================!
!   fire/vbase : fmeff   !
!========================!
! FIRE-VBASE $Id$
subroutine FMEFF (IYR, FM, FLAME, ICALL, POMORT, PVOLKL, MKODE,&
     &PSBURN)
  implicit none

  !     SINGLE-STAND VERSION
  !     CALLED FROM: FMBURN
  !                  FMPOFL
  !     CALLS:   FMBRKT
  !              FMSSEE
  !              FMSCRO
  !              FMSADD
  !              FMSVTOBJ
  !              SVMORT
  !  PURPOSE:
  !     THIS SUBROUTINE CALCULATES THE PROBABILITY OF TREE MORTALITY.
  !     FROM SIMPLIFIED INFORMATION SENT BY E. REINHARDT

  !  CALL LIST DEFINITIONS:
  !     FM:      FUEL MODEL
  !     FLAME:   FLAME LENGTH (FT)
  !     ICALL:   WHERE IS THIS CALLED FROM? (0=FMBURN, 1=FMPOFL)
  !     POMORT:  POTENTIAL MORTALITY (AS PROP OF BASAL AREA)
  !     PVOLKL:  VOLUME KILLED.
  !     MKODE:   MORTALITY CODE (0=TURN OFF FFE MORTALITY,
  !                              1=FFE ESTIMATES MORTALITY)
  !     PSBURN:  PERCENTAGE OF STAND THAT IS BURNED

  !  LOCAL VARIABLE DEFINITIONS:
  !     CLSP:    PROPORTION OF CROWN LENGTH SCORCHED
  !     CRBNL:   CRown BurN Length - the length of crown that was below
  !              the flame height
  !     CRW1BN:  Weight of CRown component 1 (0-0.25") that was BurNed
  !     CSV:     CROWN SCORCH VOLUME
  !     CRL:     CROWN LENGTH (FT)
  !     DTHISC:  Density of snags with THIS Crown size (depends on what
  !              happened to different portions of the tree record during
  !              the fire).
  !     PMORT:   PROBABILITY (PROPORTION) OF MORTALITY
  !     PROPCR:  PROPortion of the CRown to burn
  !     SL:      SCORCH LENGTH (FT)
  !     TCROWN:  Temporary CROWN weights array
  !     TOLDCR:  Temporary OLD CRown weights array (material dead from
  !              crown lifting)
  !     YRSCYC:  YeaRS left in current FVS CYCle (including current year)
  !     BCROWN:  Burned crown material

  !  COMMON BLOCK VARIABLES AND PARAMETERS:
  !     SCH:   SCORCH HEIGHT (IN FEET)

  !.... PARAMETER STATEMENTS.

  !.... PARAMETER INCLUDE FILES.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'

  !.... COMMON INCLUDE FILES.

  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'FMCOM.f90'
  include 'FMFCOM.f90'

  !.... VARIABLE DECLARATIONS.

  ! Variables
  logical :: DEBUG,LPSBURN
  integer :: I,FM,KSP,ICALL,IYR,ISZ,IDC,ITM,MORTGP,MKODE
  integer :: P,H,D
  real    :: CRW1BN,CRBNL,CRL,CSV,DTHISC,PROPCR,TCROWN
  real    :: BAMORT,TOTBA,POMORT,TOLDCR,PMORT,YRSCYC
  real    :: PVOLKL,FLAME,BCR,SL,CLSP,XM,FMBRKT,CRBOT
  real    :: BCROWN,MORTB0(5),MORTB1(5),MORTB2(5)
  real    :: CHARHT,MNMORT,PSBURN,XRAN
  double precision :: SAVESO
  dimension TCROWN(0:5)
  dimension TOLDCR(0:5)

  !     COEFFICIENTS FOR SOME SN MORTALITY EQUATIONS

  data     MORTB0 / 1.0229, 0.1683, 1.2165, 0.8221, 2.775 /
  data     MORTB1 / -0.2646, -0.1332, -0.4758, -0.4098, -1.1224 /
  data     MORTB2 / 2.6232, 3.4152, 6.0415, 8.4682, 2.8312 /

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'FMEFF',5,ICYC)
  if (DEBUG) write(JOSTND,7) ICYC,IYR,FM,FLAME,CRBURN,ICALL
  7 format(' ENTERING FMEFF CYCLE = ',I2,&
       &' IYR=',I5,' FM=',I3,' FLAME=',F14.4,&
       &' CRBURN=',F10.4,' ICALL=',I2)

  !.... BEGIN ROUTINE

  !sng  YRSCYC = FLOAT( IY(ICYC+1)-IYR )
  YRSCYC = FLOAT( IY(ICYC+1)-IYR )

  BAMORT = 0.0
  TOTBA = 0.0
  POMORT = 0.0
  PVOLKL = 0.0

  !     Burn the entire crown of pre-existing snags caught in the area with
  !     crown fire (if crown fire occurred).  This is done here so that
  !     CWD2B2 will not contain material from trees killed in the current
  !     fire (only from fires earlier in this FVS cycle).
  !     Note, because we want to get potential smoke production, we now need to
  !     also calculate the potential amount of crowns burned (PBRNCR)

  BCROWN = 0
  if (CRBURN .gt.0.0) then
     do ISZ = 0,5
        do IDC = 1,4
           do ITM = 1,TFMAX
              BCR = CRBURN * CWD2B(IDC,ISZ,ITM)*PSBURN/100
              if (ICALL .eq.0)&
                   &CWD2B(IDC,ISZ,ITM) = CWD2B(IDC,ISZ,ITM) - BCR
              BCROWN = BCROWN + BCR * P2T
              BCR = CRBURN * CWD2B2(IDC,ISZ,ITM)*PSBURN/100
              if (ICALL .eq.0)&
                   &CWD2B2(IDC,ISZ,ITM) = CWD2B2(IDC,ISZ,ITM) - BCR
              BCROWN = BCROWN + BCR * P2T

              if (DEBUG) write (JOSTND,10)ISZ,IDC,ITM,&
                   &CWD2B(IDC,ISZ,ITM),CWD2B2(IDC,ISZ,ITM)
              10 format(' FMEFF: ISZ,IDC,ITM=',3I3,' CWD2B=',F12.2,&
                   &' CWD2B2=',F12.2)
           end do
        end do
     end do
  end if

  !     NOW LOOK AT WHAT HAPPENS TO THE LIVE TREES.

  call RANNGET(SAVESO)
  do I = 1,ITRN

     !       CALL A RANDOM NUMBER AND COMPARE IT TO THE % OF THE STAND AREA
     !       BURNED (PSBURN) TO SEE IF WE ARE IN THE BURNED PORTION OR NOT.
     !       IF WE ARE IN THE UNBURNED PORTION, SKIP LOTS - MORTALITY IS 0.

     PMORT = 0.0
     LPSBURN = .true.
     call RANN(XRAN)

     if (DEBUG) write(JOSTND,12) IYR,I,XRAN,ISP(I),DBH(I)
     12 format(' FMEFF YEAR = ',I5,&
          &' I=',I5,' XRAN=',F6.4,' KSP=',I4,' DBH=',F6.2)

     XRAN = XRAN*100
     if (XRAN .gt. PSBURN) then
        LPSBURN = .false.
        goto 90
     end if

     KSP = ISP(I)

     !       COMPUTE SCORCH LENGTH
     !       CRL = CROWN LENGTH
     !       SL = SCORCH LENGTH

     CRL = HT(I) * (FLOAT(FMICR(I)) / 100.0)
     SL = SCH - (HT(I) - CRL)

     if (SL .lt.0.0) SL = 0.0
     if (SL .gt. CRL) SL = CRL

     if ((FMPROB(I) .gt.0.0) .and. (MKODE .ne.0)) then

        !         PERCENT OF CROWN LENGTH SCORCHED & % CROWN VOLUME SCORCHED

        if (CRL .gt.0.0) then
           CLSP = 100.0 * (SL / CRL)
           if (CLSP .gt.100.0) CLSP = 100.0
           CSV = 100.0 * (SL * (2.0 * CRL - SL) / (CRL * CRL))
        else
           CSV = 100.0
        end if

        XM = EXP(-1.941 + 6.316 * (1 - EXP(-FMBRKT(DBH(I),KSP)))&
             &- .000535 * CSV * CSV)

        PMORT = (1.0 / (1.0 + XM))

        !         ADJUST MORTALITY FOR SOME SOUTHERN SPECIES BASED ON
        !         EQUATIONS IN REGELBRUGGE AND SMITH (NJAF 11(3) 1994)

        if ((VARACD .eq. 'SN') .or. (VARACD .eq. 'CS')) then

           !           THESE EQUATIONS USE MAX (UPHILL) CHAR HT.  OPINION AT
           !           MISSOULA FIRE LAB IS THAT DOWNHILL CHAR HT IS 70% OF FLAME
           !           LENGTH AND THAT UPHILL CHAR HT APPROX. EQUALS FLAME LENGTH.
           !           HOWEVER, AN ARTICLE BY MICHAEL CAIN IN FIRE MANAGEMENT NOTES
           !           SUGGESTS THAT MAX CHAR HT IS 50-65% OF FLAME LENGTH.  I
           !           WENT IN BETWEEN WITH THE ASSUMPTION BELOW.

           CHARHT = FLAME*.7

           if (VARACD .eq. 'SN') then
              select case (KSP)
              case (63,74) ! white oak and chestnut oak
                 MORTGP = 1
              case (64,75,78) ! scarlet, black, and northern red oak
                 MORTGP = 2
              case (27) ! hickory
                 MORTGP = 3
              case (20) ! red maple
                 MORTGP = 4
              case (54) ! black gum
                 MORTGP = 5
              case default
                 MORTGP = 6
              end select
           else !VARACD = cs
              select case (KSP)
              case (47,59) ! white oak and chestnut oak
                 MORTGP = 1
              case (48:51) ! scarlet, black, and northern/southern red oak
                 MORTGP = 2
              case (14:23) ! hickories
                 MORTGP = 3
              case (29) ! red maple
                 MORTGP = 4
              case (11,13) ! black and swamp tupelo
                 MORTGP = 5
              case default
                 MORTGP = 6
              end select
           end if

           select case (MORTGP)
           case (1:5)
              XM = -1*(MORTB0(MORTGP) +&
                   &MORTB1(MORTGP)*DBH(I)*2.54 +&
                   &MORTB2(MORTGP)*CHARHT/3.28)
              ! see whether XM is so large that pmort will be less
              ! than .000001 and taking the exp of xm may cause a blowup
              MNMORT = ALOG(1/.000001 - 1)
              if (XM .ge. MNMORT) then
                 PMORT = 0
              else
                 XM = EXP(XM)
                 PMORT = (1.0 /(1.0 + XM))
              end if
           case (6)
              PMORT = PMORT ! use old estimate from FOFEM
           end select
        end if

        !         ENGELMANN SPRUCE HAS MINIMUM MORTALITY OF 0.8

        select case (VARACD)
        case ('IE','EM','KT')
           if (KSP .eq.8) PMORT = MAX(0.8, PMORT)
        case default
           PMORT = PMORT
        end select

        !         MAKE SOME ADJUSTMENTS FOR THE LAKE STATES FFE
        !         CONIFERS GET THEIR MORTALITY REDUCED BY HALF IF
        !         THE BURN WAS BEFORE GREENUP.
        !         BALSAM FIR GETS A MINIMUM MORTALITY OF 70%
        !         ALL MAPLES UNDER 4" DIE.
        !         HARDWOODS GET THEIR MORTALITY REDUCED TO 80% IF THE BURN WAS
        !         BEFORE GREENUP.  OAKS 2.5"+ GET THEIR MORTALITY REDUCED BY
        !         HALF BEFORE GREENUP SINCE THEY ARE ESPECIALLY RESISTANT.
        !         ALL HARDWOODS LESS THAN 1" DIE

        if (VARACD .eq. 'LS' .or. VARACD .eq. 'ON') then
           if ((BURNSEAS .le.2) .and. (KSP .le.14)) PMORT = PMORT/2
           if (KSP .eq.8) PMORT = MAX(0.7, PMORT)
           select case (KSP)
           case (18,19,26,27,51,52)
              if (DBH(I) .lt.4) PMORT = 1.0
           case default
              PMORT = PMORT
           end select
           if ((BURNSEAS .le.2) .and. (KSP .gt.14)) then
              select case (KSP)
              case (30:36)
                 if (DBH(I) .ge.2.5) then
                    PMORT = PMORT/2
                 else
                    PMORT = PMORT*0.8
                 end if
              case default
                 PMORT = PMORT*0.8
              end select
           end if
           if ((KSP .gt.14) .and. (DBH(I) .le.1)) PMORT = 1.0
        end if

        !         MAKE THE SAME ADJUSTMENTS FOR THE NE VARIANT

        if (VARACD .eq. 'NE') then
           if ((BURNSEAS .le.2) .and. (KSP .le.25)) PMORT = PMORT/2
           if (KSP .eq.1) PMORT = MAX(0.7, PMORT)
           select case (KSP)
           case (26:29,99:100)
              if (DBH(I) .lt.4) PMORT = 1.0
           case default
              PMORT = PMORT
           end select
           if ((BURNSEAS .le.2) .and. (KSP .gt.25)) then
              select case (KSP)
              case (55:70,89)
                 if (DBH(I) .ge.2.5) then
                    PMORT = PMORT/2
                 else
                    PMORT = PMORT*0.8
                 end if
              case default
                 PMORT = PMORT*0.8
              end select
           end if
           if ((KSP .gt.25) .and. (DBH(I) .le.1)) PMORT = 1.0
        end if

        !          ADJUST MORTALITY FOR TREES WITH DBH < 1

        if (DBH(I) .le.1.0.and. CSV .gt.50.0) PMORT = 1.0

     end if

     if (DEBUG) write(JOSTND,15) ICYC,IYR,PSBURN
     15 format(' ENTERING FMEFF CYCLE = ',I2,&
          &' IYR=',I5,' PSBURN=',F6.1)

     !       MODIFY PMORT BY THE MULTIPLIER

     PMORT = PMORT*FMORTMLT(I)

     if (PMORT .gt.1.0) PMORT = 1.0
     if (PMORT .lt.0.0) PMORT = 0.0

     !        DETERMINE THE FATE OF CROWN MATERIAL. THIS CAN BE SKIPPED IF THERE
     !        WEREN'T ANY TREES IN THE RECORD TO START WITH. MOST LINES CAN ALSO BE
     !        SKIPPED IF WE ARE JUST WANTING TO GET POTENTIAL INFORMATION.

     if (FMPROB(I) .le.0.0) goto 90

     !       FIRST, CONSIDER THE PORTION OF THE STAND WITH CROWN FIRE (=CRBURN).

     if (CRBURN .le.0.0.or. MKODE .eq.0) goto 40

     !       USE TCROWN TO REMEMBER THE CROWN WEIGHTS OF THE TREES THAT
     !       ESCAPED THE CROWN FIRE:

     TCROWN(0) = CROWNW(I,0)
     TCROWN(1) = CROWNW(I,1)
     TOLDCR(1) = OLDCRW(I,1)

     !       BURN 100% OF THE FOLIAGE IN THIS PART OF THE STAND, AND 50%
     !       OF THE 0-0.25" BRANCHES

     !       NOTE1: CONVERT CROWNW TO TONS TO MATCH BURNCR & PBRNCR
     !       NOTE2: THE CROWNW REDUCTION HERE IS JUST TEMPORARY, TO MAKE
     !              FMSCRO USE THE RIGHT WEIGHTS
     !       NOTE3: 0-0.25" BRANCHES INCLUDE OLDCRW MATERIAL, WHICH IS

     !       KEPT AS THE AMOUNT SCHEDULED TO FALL IN EACH REMAINING YEAR OF
     !       THE FVS CYCLE:

     BCROWN = BCROWN + CRBURN * FMPROB(I) * P2T * CROWNW(I,0)
     BCROWN = BCROWN + 0.5 * CRBURN * FMPROB(I) * P2T *&
          &(CROWNW(I,1) + YRSCYC * OLDCRW(I,1))

     if (ICALL .eq.0) then
        CROWNW(I,0) = 0
        CROWNW(I,1) = 0.5 * CROWNW(I,1)
        OLDCRW(I,1) = 0.5 * OLDCRW(I,1)

        !         ALL TREES INVOLVED IN THE CROWN FIRE WILL DIE, AND THEIR SNAGS
        !         WILL HAVE THE CROWN WEIGHTS JUST DETERMINED.  CALL FMSCRO TO
        !         PUT THEIR CROWNS INTO CWD2B2/CWD2B TO FALL AT APPROPRIATE TIMES IN
        !         THE FUTURE (NOTE:  THE TIMES WILL NOT *REALLY* BE APPROPRIATE
        !         IF PBTIME < THE NORMAL SNAG CANOPY LIFESPAN FOR THIS SPECIES,
        !         BUT THIS TIMING IS NOT CRITICAL TO MODEL BEHAVIOUR).

        DTHISC = FMPROB(I) * CRBURN
        call FMSCRO(I,ISP(I),IYR,DTHISC,1)

        !         RE-SET THE CROWN WEIGHTS OF TREES THAT ESCAPED CROWN FIRE:

        CROWNW(I,0) = TCROWN(0)
        CROWNW(I,1) = TCROWN(1)
        OLDCRW(I,1) = TOLDCR(1)
     end if

     40 continue

     if (CRBURN .ge.1.0) goto 90

     !        NOW CONSIDER THE PORTION OF THE STAND WITHOUT CROWN FIRE.  IF THE
     !        TREES IN THIS RECORD HAD PART OF THEIR CROWN BELOW THE FLAME HEIGHT,
     !        THEIR CROWNS SHOULD BE PARTIALLY BURNT.  CHANGE THE REAL CROWNW
     !        WEIGHTS FOR THIS, BECAUSE IT AFFECTS BOTH THE STILL-LIVING AND THE
     !        KILLED TREES.

     !        FIRST, SAVE THE CROWN WEIGHTS.
     do ISZ = 0, 5
        TCROWN(ISZ) = CROWNW(I,ISZ)
        TOLDCR(ISZ) = OLDCRW(I,ISZ)
     end do

     !        CHANGED FROM FLAME HEIGHT TO SCORCH HEIGHT (SB & ER: 2/97)

     CRBOT = HT(I) - CRL

     if (SCH .gt. CRBOT) then

        !          THE AMOUNT OF CROWN TO BURN IS THE PROPORTION OF BURNED CROWN
        !          LENGTH TO TOTAL CROWN LENGTH.  THIS PROPORTION IS EQUAL TO BOTH
        !          THE VOLUME PROPORTION AND SURFACE-AREA PROPORTION THAT WOULD HAVE
        !          BEEN BURNED IF THE CROWN WAS CYLINDRICAL.  THIS CYCLINDRICAL
        !          APPROXIMATION SHOULD CAUSE MINIMAL ERROR BECAUSE:  FLAME HEIGHTS
        !          ARE GENERALLY < 10 FEET, AND LARGE TREES WON'T HAVE ANY CROWN THAT
        !          LOW, WHILE SMALL TREES WILL CONTRIBUTE LITTLE WEIGHT OF BURNED
        !          MATERIAL EVEN IF WE DO BURN TOO MUCH, AND MEDIUM TREES MAY BE
        !          ROUGHLY CYLINDRICAL OVER MUCH OF THEIR TOTAL CROWN LENGTH.

        CRBNL = SCH - CRBOT

        if (CRBNL .gt. CRL) CRBNL = CRL
        PROPCR = 0.0
        if (CRL.gt.0.) PROPCR = CRBNL / CRL

        !          AS FOR CROWN FIRES, BURN ALL FOLIAGE REACHED BY THE FIRE, AND
        !          HALF THE 0-0.25" MATERIAL.  CRW1BN IS USED BECAUSE IT'S NEEDED
        !          LATER.  FOR OLDCRW MATERIAL (WHICH IS DEAD MATERIAL BELOW THE
        !          BOTTOM OF THE CURRENT CROWN), JUST ASSUME THAT ALL OF IT WAS
        !          IN WITHIN REACH OF THE FIRE.

        CRW1BN = 0.5 * PROPCR * CROWNW(I,1)

        if (MKODE .ne.0) then
           BCROWN = BCROWN + (1.0 - CRBURN) * FMPROB(I) * P2T *&
                &CROWNW(I,0) * PROPCR
           BCROWN = BCROWN + (CRW1BN + 0.5*YRSCYC*OLDCRW(I,1))&
                &* (1.0 - CRBURN) * FMPROB(I) * P2T
        else ! MKODE .EQ. 0
           BCROWN = BCROWN + (1.0) * FMPROB(I) * P2T *&
                &CROWNW(I,0) * PROPCR
           BCROWN = BCROWN + (CRW1BN + 0.5*YRSCYC*OLDCRW(I,1))&
                &* (1.0) * FMPROB(I) * P2T
        end if

        if (ICALL .eq.0) then
           CROWNW(I,0) = CROWNW(I,0) - PROPCR * CROWNW(I,0)
           CROWNW(I,1) = CROWNW(I,1) - CRW1BN
           OLDCRW(I,1) = 0.5 * OLDCRW(I,1)

           !            NOW CALL **FMSCRO** TO POOL THE CROWNS OF THE FIRE-KILLED TREES:

           DTHISC = (1.0 - CRBURN) * PMORT * FMPROB(I)
           call FMSCRO(I,ISP(I),IYR,DTHISC,1)

           !            THE PARTS OF THE CROWN OF STILL-LIVING TREES THAT WERE TOO
           !            LARGE TO BURN BUT WERE WITHIN THE FLAME HEIGHT ARE ALSO DEAD
           !            AND SHOULD ALSO BE PUT IN CWD2B2/CWD2B POOLS. **FMSCRO** CAN DO THIS
           !            IF YOU PASS IT ONLY THE WEIGHTS OF THE KILLED CROWN MATERIAL.
           !            REMEMBER THAT HALF OF THE 0-0.25" KILLED MATERIAL HAS BEEN
           !            CONSUMED.

           do ISZ = 0, 5
              if (ISZ .eq.0) CROWNW(I,ISZ) = 0
              if (ISZ .eq.1) then
                 CROWNW(I,ISZ) = PROPCR *(CROWNW(I,ISZ)+CRW1BN)&
                      &- CRW1BN
              else
                 CROWNW(I,ISZ) = PROPCR * CROWNW(I,ISZ)
              end if
              OLDCRW(I,ISZ) = 0.0
           end do

           if (MKODE .ne.0) then
              DTHISC = ((1.0-CRBURN) - (1-CRBURN)*PMORT)&
                   &* FMPROB(I)
           else ! MKODE .EQ. 0
              DTHISC = FMPROB(I)
           end if
           call FMSCRO(I,ISP(I),IYR,DTHISC,1)

           !            RE-SET CROWNW TO BE THE ORIGINAL WEIGHTS, MINUS THE WEIGHT
           !            OF CONSUMED AND FIRE-KILLED MATERIAL.

           do ISZ = 0,5
              CROWNW(I,ISZ) = TCROWN(ISZ)*(1-PROPCR)
              if (ISZ .eq.1) then
                 OLDCRW(I,ISZ) = TOLDCR(ISZ)*0.5
              else
                 OLDCRW(I,ISZ) = TOLDCR(ISZ)
              end if
           end do

           !            SET GROW = -1 SO THAT THE FULL CROWN WEIGHT WON'T BE RESTORED
           !            UNTIL THE START OF THE 2ND TIME FROM NOW THAT A NEW FVS CYCLE
           !            STARTS.

           GROW(I) = -1

           !            SET THE NEW FIRE MODEL VERSION OF CROWN LENGTH.

           FMICR(I) = IFIX(100.0 * (CRL - CRBNL) / HT(I))

        end if

        !         SCORCHING DOESN'T AFFECT THE CROWN IN THIS CASE. IF THESE TREES
        !         DIDN'T HAVE PART OF THEIR CROWN BELOW THE FLAME HEIGHT, AND IF
        !         THIS IS CALLED WHEN ACTUALLY APPLYING THE BURN, ALL YOU HAVE TO
        !         DO IS CALL **FMSCRO** FOR THE DEAD TREES.

     else if (ICALL .eq.0) then

        DTHISC = (1.0 - CRBURN) * PMORT * FMPROB(I)
        call FMSCRO(I,ISP(I),IYR,DTHISC,1)

     end if

     !        THIS CONCLUDES THE DETERMINATION OF THE FATE OF CROWN MATERIAL.

     90 continue

     !        DETERMINE HOW MANY TREES DIE. WE ASSUME THAT PMORT IS THE
     !        PROPORTION OF TREES IN THE RECORD WHICH DIE (RATHER THAN THE
     !        PROBABILITY THAT ALL THE TREES IN THE RECORD DIE). ALSO,
     !        CROWNING KILLS SOME ADDITIONAL TREES. LET CROWNING ACT ON THE
     !        TREES REMAINING AFTER PMORT KILLS THE TREES.

     if (ICALL .eq.0) then
        CURKIL(I) = PMORT * FMPROB(I)
        if (MKODE .ne.0.and. LPSBURN) then
           CURKIL(I) = CURKIL(I) + CRBURN * (FMPROB(I) - CURKIL(I))
        end if
        FIRKIL(I) = FIRKIL(I) + CURKIL(I)
        FMPROB(I) = FMPROB(I) - CURKIL(I)
        if (FMPROB(I) .lt.0.0) FMPROB(I) = 0.0

        !           STORE THE FIRE-CAUSED MORTALITY IN THE SNAG MGMT ROUTINES FOR
        !           ADDITION TO THE SNAG POOLS.  R&C 07/11/96

        call FMSSEE (I,ISP(I),DBH(I),HT(I),CURKIL(I),1,DEBUG,JOSTND)

     else if (ICALL .ne.0) then
        POMORT = PMORT * FMPROB(I)
        if (LPSBURN) POMORT = POMORT + CRBURN * (FMPROB(I) - POMORT)
        BAMORT = BAMORT + POMORT * (DBH(I) / 24.0)**2
        TOTBA = TOTBA + FMPROB(I) * (DBH(I) / 24.0)**2
        if (VARACD.eq.'CS' .or. VARACD.eq.'LS'&
             &.or. VARACD.eq.'NE' .or. VARACD.eq.'SN') then
           PVOLKL = PVOLKL + (MCFV(I) * POMORT)
        else
           PVOLKL = PVOLKL + (CFV(I) * POMORT)
        end if
     end if

     100 continue
  end do
  call RANNPUT(SAVESO)

  !     IF THIS CALL IS FROM FMBURN (A FIRE), THEN REPORT THE MORTAILITY
  !     TO THE SV ROUTINES AND WRITE THE REPORT(S).

  if (ICALL .eq.0) then

     !       CREATE A TEMPORARY COPY OF THE OBJECT LIST, THEN
     !       CALL SVMORT TO PROCESS THE NEW FIRE MORTALITIES AND OUTPUT THEM.

     call FMSVTOBJ(FIRTYPE)
     call SVMORT(1,FIRKIL,IYR)

     !       MAKE ANOTHER COMPRESSED COPY OF CWD TO GIVE TO THE SVS ROUTINES.

     do P = 1,6
        TCWD2(P) = 0.0
     end do
     do P = 1,2
        do H = 1,2
           do D = 1,4
              TCWD2(1) = TCWD2(1)+CWD(P,10,H,D)
              TCWD2(2) = TCWD2(2)+CWD(P,11,H,D)
              TCWD2(3) = TCWD2(3)+CWD(P,1,H,D)+CWD(P,2,H,D)+CWD(P,3,H,D)
              TCWD2(4) = TCWD2(4)+CWD(P,4,H,D)
              TCWD2(5) = TCWD2(5)+CWD(P,5,H,D)
              TCWD2(6) = TCWD2(6)+CWD(P,6,H,D)+CWD(P,7,H,D)+CWD(P,8,H,D)&
                   &+CWD(P,9,H,D)
           end do
        end do
     end do
     call FMSVOUT(IYR,FLAME,FIRTYPE)
  end if

  if (ICALL .ne.0.and. TOTBA .ne.0) POMORT = BAMORT / TOTBA

  !     STORE THE FIRE-CAUSED MORTALITY IN THE SNAG POOLS. R&C 07/11/96

  if (ICALL .eq.0) then
     call FMSADD (IYR,1)
     BURNCR = BCROWN
  else
     PBRNCR = BCROWN
  end if

  return
end
