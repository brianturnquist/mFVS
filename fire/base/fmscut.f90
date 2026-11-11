!========================!
!   fire/base : fmscut   !
!========================!
! FIRE-BASE $Id$
subroutine FMSCUT (MXVOL,NR,NC,SSNG,DSNG,CTCRWN,TKCRWN)
  implicit none

  !     SINGLE-STAND VERSION

  !     PART OF THE FIRE MODEL EXTENSION.
  !     THIS ROUTINE ADDS THE MATERIAL THAT WAS LEFT BEHIND AFTER CUTS.
  !     THIS MUST BE DONE *BEFORE* CUTS REMOVES THE TREE RECORDS,
  !     BECAUSE IT RELIES ON GETTING THE INFORMATION ABOUT THE DBH, HT AND
  !     SPECIES OF EACH RECORD.

  !     CALLED FROM -- CUTS
  !     CALLS FMSSEE
  !           CWD3
  !           FMSADD
  !           FMCBIO

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'
  include 'FMCOM.f90'
  include 'FMFCOM.f90'
  include 'FMPROP.f90'

  !  Local Variable Definitions:

  ! Variables
  integer :: I,ISZ,IDC,NR,NC,J,K
  real    :: MXVOL(NR,NC),SSNG(MAXTRE),DSNG(MAXTRE)
  real    :: CTCRWN(MAXTRE),TKCRWN(MAXTRE)
  real    :: X,Y,Z,LVSNBM,TKCRBM,XNEG1
  real    :: ABIO,MBIO,RBIO,HRVTRE,LVCRWN
  logical :: DEBUG,LMERCH,LMRCH2
  real    :: RDANUW

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = MXVOL(1,1)

  !  CHECK FOR DEBUG

  call DBCHK (DEBUG,'FMSCUT',6,ICYC)
  if (DEBUG) write(JOSTND,7) ICYC, LFMON
  7 format(' ENTERING FMSCUT CYCLE = ',I2,' LFMON=',L2)

  !     CHECK TO SEE IF THE FIRE MODEL IS ACTIVE

  if (.not. LFMON) return

  !     Add standing snags created by **CUTS** to the list of new snags
  !     ADD CROWN MATERIAL FROM **CUTS** TO DEBRIS POOLS
  !     ADD DOWNED SNAGS TO DEBRIS POOLS VIA CWD3

  LVSNBM = 0.0
  TKCRBM = 0.0
  LVCRWN = 0.0
  LMERCH = .false.
  if (LVWEST) LMERCH = .true.

  do I = 1,ITRN

     !       SET THE HARVEST YEAR IF THERE IS ANY MATERIAL TO ADD TO THE
     !       DEBRIS POOLS (CROWNS OR DOWNED SNAGS)

     if (CTCRWN(I) .gt.0.0.or. DSNG(I) .gt.0.0)&
          &HARVYR = IY(ICYC)

     call FMSSEE (I,ISP(I),DBH(I),HT(I),SSNG(I),0,DEBUG,JOSTND)

     IDC = DKRCLS(ISP(I))
     X = CTCRWN(I) * P2T
     Y = TKCRWN(I) * P2T

     !           (NOTE: When calculating LVCRWN, we want only those crowns
     !               that are on harvested trees that left the stand. Therefore,
     !               we must remove the trees that were left as downed snags.)
     Z = (CTCRWN(I) - DSNG(I)) * P2T

     CWD(1,10,2,IDC) = CWD(1,10,2,IDC) + (CROWNW(I,0) * X)
     TKCRBM = TKCRBM + (CROWNW(I,0) * Y)
     LVCRWN = LVCRWN + (CROWNW(I,0) * Z)

     do ISZ = 1,5
        CWD(1,ISZ,2,IDC) = CWD(1,ISZ,2,IDC) + (CROWNW(I,ISZ) * X)
        TKCRBM = TKCRBM + CROWNW(I,ISZ) * Y
        LVCRWN = LVCRWN + (CROWNW(I,ISZ) * Z)
     end do
     call CWD3(ISP(I),DBH(I),DSNG(I),HT(I),ICR(I))

     !       CALCULATE VOLUME AND BIOMASS OF CUT MATERIAL NOW IN
     !       THE SNAG (SSNG) OR DOWNED SNAG (DSNG) POOLS; IN
     !       PREPARATION FOR SUBTRACTING FROM THE HARVESTED VOLUME

     XNEG1 = -1.0
     LMRCH2 = .false.

     call FMSVL2(ISP(I),DBH(I),HT(I),XNEG1,X,ICR(I),&
          &'L',LMRCH2,DEBUG,JOSTND)
     LVSNBM = LVSNBM +&
          &(X * V2T(ISP(I)) * (SSNG(I) + DSNG(I)))

     !       CALCULATE THE BIOMASS EQUATIONS FOR REMOVED TREES
     !       OR DOWN TREES.
     !       (TREES LEFT STANDING AS SNAGS ARE CAPTURED BY FMSADD)

     HRVTRE = (WK3(I) - SSNG(I) - DSNG(I))

     call FMCBIO(DBH(I), ISP(I), ABIO, MBIO, RBIO)
     BIOROOT = BIOROOT + RBIO * (WK3(I) - SSNG(I))

     !       BIOMASS REMOVED IS ABOVEGROUND BIOMASS (USUSALLY MORE THAN
     !       THE HARVESTED BIOMASS, WHICH IS BASED ON MERCH)

     BIOREM(1) = BIOREM(1) + ABIO * HRVTRE

     !       DETERMINE THE VOLUME OF ROUNDWOOD AND PULPWOOD.
     !       VALUES BELOW BREAKPOINT (POTENTIALLY ALTERED BY THE USER)
     !       ARE FOR PULPWOOD, VALUES ABOVE ARE FOR ROUNDWOOD.
     !       SAVE THE VALUES BY HW/SW. RW/PW, YEAR
     !       EASTERN FVS VARIANTS USE "TOTAL" VOLUME AS COMMERCIALLY
     !       USEFUL, SO LMERCH IS .FALSE.; WESTERN VARIANTS USE
     !       "MERCH" VOLUME AS USEFUL, SO LMERCH IS .TRUE.
     !       REPORTED USING MBIO WHEN JENKINS EQUATIONS ARE IN USE

     if (ICMETH .eq.0) then ! FFE-CALCULATION METHOD
        X = 0.0
        if (HRVTRE .gt.0.0) then
           XNEG1 = -1.0
           call FMSVL2(ISP(I),DBH(I),HT(I),XNEG1,X,ICR(I),&
                &'L',LMERCH,DEBUG,JOSTND)
           X = X * V2T(ISP(I)) * HRVTRE
        end if
     else ! JENKINS CALCULATION METHOD
        X = MBIO * HRVTRE
     end if

     K = 1
     if (BIOGRP(ISP(I)) .gt.5) K = 2
     J = 1
     if (DBH(I) .gt. CDBRK(K))  J = 2
     FATE(J, K, ICYC) = FATE(J, K, ICYC) + X

  end do

  !     ADD NEW SNAG LIST ELEMENTS FROM **CUTS** TO SNAG LIST

  call FMSADD (IY(ICYC),2)

  !     COMPUTE TOTAL VOLUME REMOVED THROUGH HARVEST AND CONVERT TO
  !     TOTAL WEIGHT REMOVED

  TONRMH = 0.0
  do I = 1,ITRN
     X = 0.
     XNEG1 = -1.0
     LMERCH = .false.
     call FMSVL2(ISP(I),DBH(I),HT(I),XNEG1,X,ICR(I),&
          &'L',LMERCH,DEBUG,JOSTND)
     X = X * V2T(ISP(I)) * WK3(I)
     TONRMH = TONRMH + X
  end do

  !     ADJUST REMOVED TONS BY SUBTRACTING SNAGS THAT HAVE BEEN
  !     LEFT BEHIND IN THE STAND AND ADDING CROWNS THAT HAVE BEEN
  !     TAKEN FROM THE STAND

  TONRMH = TONRMH - LVSNBM + TKCRBM

  !     IN THE JENKINS CASE, SUBTRACT THE CROWNS LEFT IN THE STAND
  !     FROM THE TOTAL AMOUNT OF BIOMASS REMOVED FROM THE STAND.
  !     NOTE THAT IN THIS CASE SINCE THE TOTAL BIOMASS AND THE CROWN BIOMASS
  !     ARE CALCULATED FROM DIFFERENT SOURCES, THE AMOUNT REMOVED COULD
  !     BE NEGATIVE, SO WE SET IT TO 0.
  BIOREM(1) = BIOREM(1) - LVCRWN
  if (BIOREM(1) .lt.0.0) BIOREM(1) = 0.0

  BIOREM(1) = MAX(BIOREM(1),0.)

  if (DEBUG) write(JOSTND,10) TONRMH
  10 format(' FMSCUT, TONRMH= ',F12.3)

  return
end
