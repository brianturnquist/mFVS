!==========================!
!   fire/base : fmmodsel   !
!==========================!
! FIRE-BASE $Id$
subroutine FMMODSEL (LDYN,FMD)
  implicit none

  !     CALLED FROM: FMDYN, FMSHRUB

  !  PURPOSE:
  !     FINISHES THE FUEL MODEL SET HELD IN FMOD() AND FWT(): TRUNCATES
  !     IT TO 4 MODELS AND REWEIGHTS, SELECTS THE DOMINANT MODEL, COUNTS
  !     THE ACTIVE MODELS (NFMODS) AND, IF THE STATIC FUEL MODEL IS
  !     DESIRED, KEEPS ONLY THE DOMINANT MODEL. THIS WAS THE LAST PART
  !     OF **FMDYN**, OF WHICH **FMSHRUB** HELD A CUT/PASTE COPY.

  !  CALL LIST DEFINITIONS:
  !     LDYN:    IS DYNAMIC MODELLING NEEDED OR NOT
  !     FMD:     DOMINANT FUEL MODEL (ASSIGNED ON RETURN)

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'FMFCOM.f90'

  ! Variables
  integer :: FMD
  integer :: I
  real    :: XWT
  logical :: LDYN

  !     TRUNCATE NUMBER OF FUEL MODELS TO 4 AND REWEIGHT,
  !     IN THE RARE CASE THAT >4 MODELS HAVE BEEN SELECTED
  !     THIS SOMETIMES HAPPENS IN THE EC, WITH THE 5TH MODEL
  !     WEIGHT AT 2-3% OF THE TOTAL.
  !     (CONVERSATION WITH NICK; 3 DECEMBER 2002)

  XWT = 0.0
  do I = 1,4
     XWT = XWT + FWT(I)
  end do
  if (XWT .gt.1.0E-6) then
     do I = 1,4
        FWT(I) = FWT(I) / XWT
     end do
     do I = 5,MXFMOD
        FMOD(I) = 0
        FWT(I) = 0.0
     end do
  end if

  !     SELECT NON-DYNAMIC MODEL BASED ON MOST HEAVILY WEIGHTED MODEL
  !     FAILSAFE IS TO NAME MODEL 8 AND ISSUE A WARNING

  FMD = -1
  if (FWT(1) .gt.1.0E-6) FMD = FMOD(1)
  if (FMD .lt.0) then
     FMOD(1) = 8
     FWT(1) = 1.0
     NFMODS = 1
     do I = 2,MXFMOD
        FMOD(I) = 0
        FWT(I) = 0.0
     end do
     call RCDSET(2, .true.)
  end if

  !     COMPUTE THE NUMBER OF ACTIVE MODELS

  do I = 1,MXFMOD
     if (FWT(I) .le.1.0E-6) then
        NFMODS = I-1
        exit
     end if
  end do
  NFMODS = MIN(NFMODS,4)

  !     IF STATIC FUEL MODEL IS DESIRED, UNSET ALL OTHER WEIGHTS

  if (.not. LDYN) then
     FMOD(1) = FMD
     FWT(1) = 1.0
     NFMODS = 1
     do I = 2, MXFMOD
        FMOD(I) = 0
        FWT(I) = 0.0
     end do
  end if

  return
end
