!==========================!
!   fire/base : fmcrbout   !
!==========================!
! FIRE-BASE $Id$
subroutine FMCRBOUT (IYR)
  implicit none

  !     SINGLE-STAND VERSION
  !     CALLED FROM: FMMAIN
  !     CALLS:  FMCBIO
  !  PURPOSE:
  !     PRINT THE STAND CARBON REPORT

  !  CALL LIST DEFINITIONS:
  !     IYR:     CURRENT YEAR

  !     PARAMETER INCLUDE FILES.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'

  !     COMMON INCLUDE FILES

  include 'PLOT.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'FMCOM.f90'
  include 'FMFCOM.f90'
  !      INCLUDE 'FMCLCOM.F77'
  include 'METRIC.f90'

  !     VARIABLE DECLARATIONS

  ! Variables
  integer :: IYR

  logical :: DEBUG,LDCAY,LMERCH
  integer :: I,JROUT,DBSKODE,CRWNRTO
  integer :: JS
  real    :: VT,X,H,D
  real    :: V(11)
  real    :: ABIO,MBIO,RBIO
  character(len=1) :: LIVEDEAD

  !     CHECK FOR DEBUG.

  call DBCHK (DEBUG,'FMCRBOUT',8,ICYC)
  if (DEBUG) write(JOSTND,1) ICYC
  1 format(' ENTERING FMCRBOUT CYCLE = ',I2)

  !     MAIN (STAND) CARBON REPORT.
  !     RETRIEVE THE UNIT NUMBER TO BE USED FOR THE REPORT.

  call GETLUN (JROUT)

  if (DEBUG) write(JOSTND,40) ICRPTB,ICRPTE,IDCRPT,JROUT
  40 format(' FMDOUT: ICRPTB=',I5,' ICRPTE=',I5,&
       &' IDCRPT=',I5,' JROUT=',I3)

  if (CRDCAY .gt.0.0) then
     LDCAY = .true.
  else
     LDCAY = .false.
  end if

  !     THERE ARE 11 INDICATORS:

  !      1 = ABOVEGROUND TOTAL LIVE
  !      2 = ABOVEGOURND TOTAL MERCH
  !      3 = BELOWGROUND LIVE
  !      4 = BELOWGROUND DEAD (WHICH DECAYS)
  !      5 = STANDING DEAD
  !      6 = FOREST DOWN DEAD WOOD
  !      7 = FOREST FLOOR (LITTER AND DUFF)
  !      8 = FOREST SHRUB+HERB
  !      9 = TOTAL STAND CARBON
  !     10 = TOTAL CARBON REMOVED THIS REPORTING PERIOD
  !     11 = TOTAL CARBON RELEASED FROM FIRE

  !     1: CALCULATE THE INDICATORS, USING EITHER FFE OR JENKINS LOGIC
  !        THERE MAY BE SOME EAST VS WEST VARIANT DIFFERENCES (THIS VERSION
  !        IS IN /FIRE/NI/SRC, SO WILL DO FOR WEST; OTHER WOULD BE /FIRE/LS/SRC
  !        WITH SAME FILENAME

  !     2: LOAD ANY CONVERSION FACTORS (IMPERIAL -> METRIC) INTO THE
  !        CNV() ARRAY BEFORE THE CALL TO THE DATABASE WRITE.

  !      NOTES: ABG, BG, SNAGS, DDW, SHRUBS, HARVEST: C = BIOMASS*0.5
  !      FOREST FLOOR: C = BIOMASS * 0.37 (Smith & Heath, research paper NE-722)
  !     - JENKINS EQUATIONS GIVE KG. WE NEED TO CHANGE TO TONS

  do I = 1,11
     V(I) = 0.0
  end do

  do I = 1,ITRN
     D = DBH(I)

     !     1. ABOVEGROUND BIOMASS (ABIO)
     !     2. MERCH BIOMASS       (MBIO) ("Stem wood" in Jenkin's paper; proportion)
     !     3. BELOWGROUND LIVE    (RBIO) ("Coarse roots" in Jenkin's paper; proportion)
     !     CALCULATE JENKINS EQUATIONS OF BIOMASS

     call FMCBIO(D, ISP(I), ABIO, MBIO, RBIO)

     ABIO = ABIO * FMPROB(I)
     MBIO = MBIO * FMPROB(I)
     RBIO = RBIO * FMPROB(I)

     V(3) = V(3) + RBIO

     !       EASTERN FVS VARIANTS USE "TOTAL" VOLUME AS COMMERCIALLY
     !       USEFUL, SO LMERCH IS .FALSE.; WESTERN VARIANTS USE
     !       "MERCH" VOLUME AS COMMERCIALLY USEFUL, SO LMERCH IS .TRUE.

     if (ICMETH .eq.1) then ! JENKINS
        V(1) = V(1) + ABIO
        V(2) = V(2) + MBIO
     else ! FFE
        JS = ISP(I)
        H = HT(I)
        X = -1.0
        LMERCH = .false.
        CRWNRTO = ICR(I)
        LIVEDEAD = 'L'
        if (LVWEST) LMERCH = .true.
        call FMSVL2(JS,D,H,X,VT,CRWNRTO,&
             &LIVEDEAD,LMERCH,DEBUG,JOSTND)
        V(2) = V(2) + FMPROB(I) * VT * V2T(JS)
     end if
  end do

  !     USE FFE EQUATIONS FOR BIOMASS OF ABIO, AND REPLACE THE VALUE IN THE ARRAY

  if (ICMETH .eq.1) then ! JENKINS
     V(10) = BIOREM(1)
  else ! FFE
     V(1) = BIOLIVE
     V(10) = BIOREM(2)
  end if

  !     SNAGS, DOWN DEAD WOOD, FOREST FLOOR, SHRUBS, ALL USE FFE EQUATIONS
  !     ROOTS (WHEN DECAY >0) USE JENKINS EQUATIONS.

  V(4) = BIOROOT
  V(5) = BIOSNAG
  V(6) = BIODDW
  V(7) = BIOFLR
  V(8) = BIOSHRB
  V(11) = BIOCON(1) * 0.37 + BIOCON(2) * 0.50

  !     CONVERT BIOMASS TO C, BUT NOT 9TH OR 11TH

  do I = 1,10
     select case (I)
     case (1:6,8,10)
        V(I) = V(I) * 0.50
     case (7)
        V(I) = V(I) * 0.37
     end select
  end do

  !     CONVERT UNITS IF NECESSARY

  if (ICMETRC.eq.1) then
     do I = 1,11
        V(I) = V(I) * TItoTM / ACRtoHA
     end do
  else if (ICMETRC.eq.2) then
     do I = 1,11
        V(I) = V(I) * TItoTM
     end do
  end if

  !     SUM UP POOLS, POSSIBLY INCLUDING 4

  V(9) = V(1)+V(3)+V(5)+V(6)+V(7)+V(8)
  if (LDCAY) then
     V(9) = V(9) + V(4)
  else
     V(4) = -1.0
  end if

  !     SET ARRAY FOR CARBSTAT EVENT MONITOR FUNCTION

  do I = 1,11
     CARBVAL(I) = V(I)
  end do

  !     ZERO OUT THE ACCUMULATING VARIABLES
  !     DECAY THE DEAD ROOTS IF DECAY>0 AFTER 900 STATEMENT

  BIOSNAG = 0.0
  BIODDW = 0.0
  BIOFLR = 0.0
  BIOSHRB = 0.0
  BIOREM(1) = 0.0
  BIOREM(2) = 0.0
  BIOLIVE = 0.0
  BIOCON(1) = 0.0
  BIOCON(2) = 0.0

  !     JUMP TO THE END IF REPORT BEGIN YEAR REMAINS 0.
  !     THIS USED TO BE BASED ON A REPORTING PERIOD, BEGIN YEAR TO END YEAR.

  if (ICRPTB .eq.0) goto 900

  !     CALL THE DBS MODULE TO OUTPUT FUEL DATA TO A DATABASE

  DBSKODE = 1
  call DBSFMCRPT(IYR,NPLT,V,11,DBSKODE)
  if (DBSKODE.eq.0) goto 900

  !     IF HEADER REQUESTED AND THIS IS THE FIRST OPPORTUNITY TO PRINT
  !     IT, THEN DO SO.

  ICRPAS = ICRPAS + 1
  if (ICRPAS .eq.1) then
     write(JROUT,699) IDCRPT
     write(JROUT,699) IDCRPT
     write(JROUT,700) IDCRPT
     write(JROUT,701) IDCRPT
     write(JROUT,702) IDCRPT
     if (ICMETRC.eq.1) then
        write(JROUT,707) IDCRPT
     else if (ICMETRC.eq.2) then
        write(JROUT,709) IDCRPT
     else
        write(JROUT,708) IDCRPT
     end if
     write(JROUT,699) IDCRPT
     write(JROUT, 44) IDCRPT,NPLT,MGMID
     write(JROUT,700) IDCRPT
     write(JROUT,704) IDCRPT
     write(JROUT,705) IDCRPT
     write(JROUT,706) IDCRPT
     write(JROUT,700) IDCRPT

     699 format(1(/1X,I5))
     700 format(1X,I5,1X,110('-'))
     701 format(1X,I5,1X,30X,'******  CARBON REPORT VERSION 1.0 ******')
     702 format(1X,I5,1X,41X,'STAND CARBON REPORT ',&
          &'(BASED ON STOCKABLE AREA)')
     44 format(1X,I5,' STAND ID: ',A26,4X,'MGMT ID: ',A4)
     704 format(1X,I5,1X,6(' '),'Aboveground Live    Belowground',&
          &24(' '),'Forest',13(' '),'Total    Total     Carbon')
     705 format(1X,I5,1X,5(' '),17('-'),' ',17('-'),'    Stand  ',&
          &25('-'),'    Stand  Removed   Released')
     706 format(1X,I5,1X,'YEAR    Total    Merch     Live     ',&
          &'Dead     Dead      DDW    Floor  Shb/Hrb   ',&
          &'Carbon   Carbon  from Fire')
     707 format(1X,I5,1X,25(' '),&
          &('ALL VARIABLES ARE REPORTED IN METRIC TONS/HECTARE'))
     708 format(1X,I5,1X,30(' '),&
          &('ALL VARIABLES ARE REPORTED IN TONS/ACRE'))
     709 format(1X,I5,1X,27(' '),&
          &('ALL VARIABLES ARE REPORTED IN METRIC TONS/ACRE'))
  end if

  write(JROUT,800) IDCRPT,IYR,(V(I),I = 1,11)
  800 format(1X,I5,1X,I4,10(2X,F7.1),4X,F7.1)

  !     REGARDLESS OF PRINTING STATUS, DECAY ROOTS
  !     CALLING ROUTINE IS ANNUAL (IFMYR1, IFMYR2), SO
  !     JUST A COMPLEMENT-MULTIPLIER.
  !     CLCWD PROVIDES POSSIBLE CLIMATE-SENSITIVE CHANGE
  !     TO DECAY RATE. SEE SUB FMCWD FOR OTHER CWD EXAMPLES.

  900 if (LDCAY) then
     X = 1.0
     !        CALL CLCWD(Q10CWD(12),REFMATCWD(12),X)
     BIOROOT = BIOROOT * (1.0 - CRDCAY*X)**NYRS
  else
     BIOROOT = 0.0
  end if

  return
end
