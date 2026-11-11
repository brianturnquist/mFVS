!========================!
!   fire/base : fmgfmv   !
!========================!
! FIRE-BASE $Id$
subroutine FMGFMV(IYR, IFMD)
  implicit none

  !  PURPOSE:
  !     THIS SUBROUTINE LOADS THE FUEL MODEL PARAMETER VALUES FOR
  !     THE FUEL MODEL PASSED AS IFMOD.

  !  CALL LIST DEFINITIONS:
  !    IYR:      CALENDAR YEAR
  !    IFMD:     FUEL MODEL NUMBER

  !  LOCAL VARIABLE DEFINITIONS:

  !  COMMON BLOCK VARIABLES AND PARAMETERS:

  !.... PARAMETER INCLUDE FILES.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'

  !.... COMMON INCLUDE FILES.

  include 'FMFCOM.f90'
  include 'FMCOM.f90'
  include 'CONTRL.f90'

  !     LOCAL VARIABLE DECLARATIONS

  ! Variables
  integer :: IYR,IFMD,I,J,IRTNCD
  logical :: DEBUG
  real    :: SUMPS
  real    :: X(2),Y(2),ALGSLP,WT

  !     FIXED VALUES FOR INTERPOLATION FUNCTION

  data     Y / 0.0, 1.0 /

  !     BEGIN ROUTINE
  !  INITIALIZE VARIABLES

  do I = 1,2
     do J = 1,4
        MPS(I,J) = 0
     end do

     do J = 1,7
        FWG(I,J) = 0.0
     end do
  end do

  MEXT(1) = 0.0
  MEXT(2) = 0.0
  MEXT(3) = 0.0

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'FMGFMV',6,1)
  if (DEBUG) write(JOSTND,7)
  7 format(' ENTERING ROUTINE FMGFMV ')

  !     Number of fuel categories:
  !     ND: 0=no dead, 1=1hr, 2=10hrs, 3=100hrs. NL: 0=No live, 1=live
  !        Even though these values are saved in NUMD and NUML,
  !        we are going to recalculate them here (because the user may
  !        have changed them in the keyword DEFULMOD). If FWG > 0 then
  !        the fuel category is present.

  ND = 0
  NL = 0
  SUMPS = 0.0

  !     SET DEAD HERB SAV TO LIVE HERB SAV
  SURFVL(IFMD,1,4) = SURFVL(IFMD,2,2)

  do I = 1,2
     do J = 1,4
        MPS(I, J) = SURFVL(IFMD,I,J)
        FWG(I, J) = FMLOAD(IFMD,I,J)
     end do
  end do

  !     PUT SOME OF THE LIVE HERBS INTO THE DEAD HERB CATEGORY (BASED ON
  !     MOISTURE) FOR THE DYNAMIC FUEL MODELS (not fm 2)
  if ((FMLOAD(IFMD,2,2) .gt.0) .and. (IFMD .ne.2) .and.&
       &(MOIS(2,2) .lt.1.2)) then
     X(1) = .30
     X(2) = 1.2
     WT = ALGSLP(MOIS(2,2),X,Y,2)
     FWG(1,4) = (1 - WT)*FMLOAD(IFMD,2,2)
     FWG(2,2) = WT*FMLOAD(IFMD,2,2)
  end if

  do I = 1,2
     do J = 1,4
        if (I .eq.1.and. FWG(I,J) .gt.0.0) ND = ND + 1
        if (I .eq.2.and. FWG(I,J) .gt.0.0) NL = NL + 1
        SUMPS = SUMPS + MPS(I,J)
     end do
  end do
  DEPTH = FMDEP(IFMD)
  MEXT(1) = MOISEX(IFMD)

  !     CHECK FOR ERRORS THAT CAN HAPPEN IF THE USER WANTED TO USE A FUEL
  !     MODEL (FUELMODL) THAT HAS NOT YET BEEN DEFINED OR HAS BEEN INCOMPLETELY
  !     DEFINED.

  !     IF THERE ARE NO LIVE OR DEAD CLASSES, OR IF THE SURFACE TO VOLUME
  !     RATIO IS <=0 FOR ALL CLASSES. OR IF DEPTH IS ZERO, THEN SOMETHING
  !     IS SERIOUSLY WRONG. EXIT WITH AN ERROR CODE.

  if (DEBUG) write(JOSTND,9) ND, NL, SUMPS, DEPTH
  9 format(' FMGFMV, ND=',I2,' NL=',I2,' SUMPS=',F10.3,'DEPTH=',F7.3)

  if ((ND .le.0.and. NL .le.0) .or. SUMPS .le.0.0.or.&
       &DEPTH .le.0.0) then
     write(JOSTND,'("/ ")')
     write(JOSTND,'("/ *** FFE: FATAL PROBLEM: YEAR = ", I4)') IYR
     write(JOSTND,'("/ *** FFE: FUEL MODEL = ", I2)') IFMD
     WRITE(JOSTND,'("/ *** FFE: NO LIVE OR DEAD CLASSES, NO",        &
          &    " FUEL SURF/VOL, OR NO DEPTH DEFINED")')
     WRITE(JOSTND,'("/ *** FFE: CHECK ""DEFULMOD"" AND",             &
          &    " ""FUELMODL"" KEYWORD USE")')
     write(JOSTND,'("/ *** FFE: EXITING")')
     call ERRGRO(.false.,4)
     call fvsGetRtnCode(IRTNCD)
     if (IRTNCD.ne.0) return
  end if

  !     ADD THE DEPTH MODIFIER BASED ON ACTIVITY-RELATED FUEL TREATMENTS
  !     SEE **FMUSRFM** FOR CALCULATION OF DPMOD

  DEPTH = DEPTH * DPMOD

  return
end
