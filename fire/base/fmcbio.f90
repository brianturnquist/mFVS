!========================!
!   fire/base : fmcbio   !
!========================!
! FIRE-BASE $Id$
subroutine FMCBIO (D, KSP, ABIO, MBIO, RBIO)
  implicit none

  !     SINGLE-STAND VERSION
  !     CALLED FROM: FMSCUT
  !                  FMCRBOUT
  !  PURPOSE:
  !     CALCULATE THE JENKINS BIOMASS EQUATIONS FOR ABOVEGROUND BIOMASS,
  !     MERCHANTABLE BIOMASS, AND BELOWGROUND BIOMASS

  !  CALL LIST DEFINITIONS:
  !     D:    DBH (IN)
  !     JSP:  SPECIES CODE
  !     ABIO: (RETURNED) ABOVEGROUND BIOMASS (KG)
  !     MBIO: (RETURNED) MERCHANTABLE
  !     RBIO: (RETURNED) ROOT BIOMASS

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
  include 'FMPROP.f90'
  include 'METRIC.f90'

  !     VARIABLE DECLARATIONS

  ! Variables
  logical :: DEBUG
  real    :: ABIO,MBIO,RBIO,D,DCM,KGtoTI
  integer :: KSP,IGRP,JGRP
  real    :: B0A(10),B1A(10),B0M(2),B1M(2),B0B(2),B1B(2)

  !     ABOVEGROUND COEFFICIENTS:
  !     CEDAR/LARCH, DOUGLAS-FIR, FIR/HEMLOCK, PINE, SPRUCE
  !     ASPEN/ALDER/COTTONWOOD/WILLOW, SOFT MAPLE/BIRCH, MIXED HARDWOOD,
  !     HARD MAPLE/OAKHICKORY/BEECH, WOODLAND JUNIPER/OAK/MESQUITE

  data B0A/&
       &-2.0336, -2.2304, -2.5384, -2.5356, -2.0773,&
       &-2.2094, -1.9123, -2.4800, -2.0127, -0.7152/
  data B1A/&
       &2.2592,  2.4435,  2.4814,  2.4349,  2.3323,&
       &2.3867,  2.3651,  2.4835,  2.4342,  1.7029/

  !     MERCH AND BELOWGROUND COEFFICIENTS:
  !     SOFTWOOD,  HARDWOOD

  data B0M/ -0.3737, -0.3065/
  data B1M/ -1.8055, -5.4240/
  data B0B/ -1.5619, -1.6911/
  data B1B/  0.6614,  0.8160/

  !     CHECK FOR DEBUG.

  call DBCHK (DEBUG,'FMCBIO',6,ICYC)
  if (DEBUG) write(JOSTND,1) ICYC
  1 format(' ENTERING FMCBIO CYCLE = ',I2)

  IGRP = BIOGRP(KSP)
  JGRP = 1
  if (IGRP .gt.5) JGRP = 2

  DCM = D * INtoCM
  KGtoTI = TMtoTI / 1000.

  ABIO = 0.0
  MBIO = 0.0
  RBIO = 0.0

  !  1. ABOVEGROUND BIOMASS (ABM)
  !  2. MERCH BIOMASS  (MBM) ("Stem wood" in Jenkin's paper -> defined as a proportion)
  !                          (Note that merch is defined from a 12in stump height, so we
  !                           will define it as being for trees above the merch limit.)
  !  3. BELOWGROUND LIVE (BBM) ("Coarse roots" in Jenkin's paper -> defined as a proportion)

  !  CALCULATE JENKINS BIOMASS (EQUATIONS WERE FIT TO TREES GE 2.5 CM)

  if (DCM .gt.0.0) then
     if (DCM .ge.2.5) then
        ABIO = EXP(B0A(IGRP) + B1A(IGRP)*LOG(DCM))
        RBIO = ABIO * EXP(B0B(JGRP) + (B1B(JGRP) / DCM))
     else
        !         IF TREE IS LESS THAN 2.5 CM, THEN CALCULATE FOR A 2.5 CM TREE AND SCALE
        !         BASED ON ACTUAL DIAMETER.  USE THE BELOWGROUND PROPORTION FOR A 2.5 CM TREE.
        ABIO = EXP(B0A(IGRP) + B1A(IGRP)*LOG(2.5))
        ABIO = ABIO * (DCM / 2.5)
        RBIO = ABIO * EXP(B0B(JGRP) + (B1B(JGRP) / 2.5))
     end if

     if (D .ge. DBHMIN(KSP))&
          &MBIO = ABIO * EXP(B0M(JGRP) + (B1M(JGRP) / DCM))

     ABIO = ABIO * KGtoTI
     MBIO = MBIO * KGtoTI
     RBIO = RBIO * KGtoTI
  end if

  return
end
