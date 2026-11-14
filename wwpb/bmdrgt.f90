!===================!
!   wwpb : bmdrgt   !
!===================!
! WWPB $Id$
subroutine BMDRGT (IYR)

  !Purpose:  Calculates RVDSC, the rating value due to 'drought' (precipitation
  !     effects) of each size class in each stand in the beetle model.  Also
  !     calculates RVOD, the average rating value due to drought in the Outside,
  !     from the average RVDST of all the stockable stands in the landscape.

  !  Notes:
  !     It is possible that some of the needed HIGH and WET values will be
  !     missing in the input PPE data.  The missing values will have been
  !     read as zeros.  Since 0 is a legitimate value for both these variables,
  !     this routine cannot detect missing data and must assume that all zeros
  !     are intended.

  !     When the coefficient of a variable which influences RVDL is negative,
  !     the effect of higher-than-average values of the variable is to decrease
  !     RVDL while lower-than-average values will increase RVDL.  The opposite
  !     occurs when the coefficient is positive.

  !  Local variable definitions:
  !     KDBH:   coefficient which determines the influence of DBH on RVDSC
  !     KHIGH:  coefficient which determines the influence of HIGH on RVDSC
  !     KSDI:   coefficient which determines the influence of SDI on RVDSC
  !     KWET:   coefficient which determines the influence of WET on RVDSC
  !     PRESET: indicates whether the user has pre-set RVDL to some constant
  !             value (PRESET = 1), or the subroutine should draw a random
  !             value for RVDL (PRESET = 0).
  !     RND1, RND2: uniform random numbers between 0 and 1.
  !     RVTEMP: Rating Value due to Drought effects in the current year alone,
  !             for the current size class.
  !     RVDL:   the average Rating Value due to Drought at the Landscape level
  !     SDD:    standard deviation of RVDL (or the change in RVDL from 1 due to
  !             drought effects, if PRESET = 1).
  !     STDEFF: sum of the Stand Effects on RVDSC, for the current stand
  !     TOTEFF: Total of the stand and size-class Effects on RVDSC, for the
  !             current stand and size class

  !  Common block variables and parameters:
  !     AVDBH:  basal-area-weighted average of SCDBH, for each stand
  !     AVHIGH: stand-area-weighted average of HIGH
  !     AVSDI:  stand-area-weighted average of BMSDI
  !     AVWET:  stand-area-weighted average of WET
  !     BMSDI:  Beetle Model's Stand Density Index for each stand,
  !             calculated from TREE (host + nonhost), BAH and BANH so that it
  !             is updated for beetle activity during the growth cycle.
  !     HIGH:   height (i.e. elevation) of each stand
  !     RVOD:   host Rating Value Outside due to Drought effects
  !     SCDBH:  DBH of a tree with the average basal area of the Size Class
  !     SDDBH:  standard deviation of SCDBH (basal-area-weighted)
  !     SDHIGH: standard deviation of HIGH (stand-area-weighted)
  !     SDSDI:  standard deviation of BMSDI (stand-area-weighted)
  !     SDWET:  standard deviation of WET (stand-area-weighted)
  !     STOCK:  logical variable to indicate whether each stand is STOCKable
  !     TRURVD: the 'true' rating value due to drought of each size class
  !             in each stand, as calculated by DMDRGT.  Unlike RVDSC, this
  !             value can be > 1 and < 0.01.  It is this value that is used
  !             to provide memory of drought effects between years (i.e. this
  !             year's TRURVD is the average of the rating value due to
  !             drought effects in the current year and last year's TRURVD).
  !     WASDRY: logical variable that indicates whether the drought model
  !             was run last year (from 'WAS DRY')
  !     WET:    any measure of site moisture availability or drought resistance
  !             that the user cares to enter

  !.... Parameter include files.
  ! Common blocks
  include 'PRGPRM.f90'
  include 'PPEPRM.f90'
  include 'BMPRM.f90'

  !.... Common include files.
  include 'PPCNTL.f90'
  include 'BMCOM.f90'

  !.... Variable declarations.
  ! Variables
  real    :: KDBH,KHIGH,KSDI,KWET,RVTEMP,RVDL
  real    :: RND1,RND2,STDEFF,RVDST
  integer :: ISTD,SINDX,ISIZ,PRESET,IRC
  integer :: DUM(1)
  logical :: LOK
  real    :: PRMS(7)

  save

  !.... Begin Routine

  ! First determine if the drought model is supposed to run this year.
  IYR1 = IYR
  NPRMS = 6
  call GPGET2 (309,IYR1,7,NPRMS,PRMS,1,I,DUM,LOK)

  if (LOK) then

     PRESET = IFIX(PRMS(1))
     if (PRMS(1) .ne. -1.0) then
        DRYON = .true.
        SDD = PRMS(2)
        KDBH = PRMS(3)
        KHIGH = PRMS(4)
        KSDI = PRMS(5)
        KWET = PRMS(6)
     else
        DRYON = .false.
        SDD = 0.0
        KDBH = 0.0
        KHIGH = 0.0
        KSDI = 0.0
        KWET = 0.0
     end if

     if (LBMDEB) write (JBMBPR,91) MICYC,IYR1,IFIX(PRMS(1)),PRMS(2)
     91 format (/' IN BMDRGT: MICYC=', I5, 'IYR1= ',I4,&
          &'Off/Fixed/Variable (-1/0/1) = ',I1,'SD= ', F7.4 )
  end if

  ! If drought model not running this year then set WASDRY to FALSE to indicate
  ! that the model was not run, set RVDSC and RVOD to 1.0, and return.

  if (.not. DRYON) then
     WASDRY = .false.
     RVOD = 1.0
     do I = 1,MXSTND
        do J = 1, NSCL
           RVDSC(I,J) = 1.0
           11 continue
        end do
        10 continue
     end do
     return
  end if

  ! Get BMAVG to calculate the necessary values, averages and sd's.
  ! Initialize RVOD and TAREA.

  call BMAVG (KDBH, KHIGH, KSDI, KWET)
  RVOD = 0.0
  TAREA = 0.0

  ! Find RVDL.  If PRESET=0, this is a random number from a normal distribution
  ! with a mean of 1 and a sd of SDD.  Otherwise, RVDL is just 1 + SDD.

  if (PRESET .eq.0) then
     call BMRANN(RND1)
     call BMRANN(RND2)

     !       this conversion of two uniform numbers to a random normal number is
     !       from pg. 92 of JA Rice 1988 Mathematical Statistics and Data Analysis,
     !       published by Wadsworth and Brooks, Pacific Grove, California.

     RVDL = 1.0&
          &+ SDD * SQRT(-2.0*LOG(RND1)) * COS(2.0*3.141592654*RND2)
  else
     RVDL = 1.0 + SDD
  end if

  ! Loop through all the stands finding RVDSC for each size class in each stand.
  ! Skip the non-stockable stands.

  do ISTD = 1,BMSTND
     if (.not. STOCK(ISTD)) goto 900
     SINDX = BMSDIX(ISTD)
     RVDST = 0.0

     !       find STDEFF

     STDEFF = 0.0
     if ((KHIGH .ne.0.0) .and. (SDHIGH .ne.0.0))&
          &STDEFF = STDEFF + KHIGH * (HIGH(SINDX)-AVHIGH) / SDHIGH
     if ((KSDI .ne.0.0) .and. (SDSDI .ne.0.0))&
          &STDEFF = STDEFF + KSDI * (BMSDI(SINDX)-AVSDI) / SDSDI
     if ((KWET .ne.0.0) .and. (SDWET .ne.0.0))&
          !     &    STDEFF = STDEFF + KWET * (WET(SINDX)-AVWET) / SDWET

          !     Changed WET( to WTSC(  (RNH July98)

          &STDEFF = STDEFF + KWET * (WTSC(SINDX)-AVWET) / SDWET

     !       loop through all the size classes

     do ISIZ = 1,NSCL

        !         find TOTEFF

        if ((KDBH .ne.0.0) .and. (SDDBH(SINDX) .ne.0.0)) then
           TOTEFF = STDEFF + KDBH *&
                &(SCDBH(SINDX,ISIZ)-AVDBH(SINDX)) / SDDBH(SINDX)
        else
           TOTEFF = STDEFF
        end if

        !         find RVTEMP and average it to the existing value of TRURVD (i.e. the
        !         value calculated last year) if the drought model ran last year.
        !         otherwise, average it to 1.

        RVTEMP = RVDL + ABS(1-RVDL) * TOTEFF

        if (WASDRY) then
           TRURVD(SINDX,ISIZ) = (TRURVD(SINDX,ISIZ) + RVTEMP) / 2.0
        else
           TRURVD(SINDX,ISIZ) = (1.0 + RVTEMP) / 2.0
        end if

        !     Change the upper bound to ? instead of 1.  (RNH June98)

        !         find RVDSC making sure that it is between 0.01 and 1

        RVDSC(SINDX,ISIZ) = TRURVD(SINDX,ISIZ)
        if (RVDSC(SINDX,ISIZ) .lt.0.01) RVDSC(SINDX,ISIZ) = 0.01
        if (RVDSC(SINDX,ISIZ) .gt. (1. + ABS(SDD)))&
             &RVDSC(SINDX,ISIZ) = (1. + ABS(SDD))
        if (RVDSC(SINDX,ISIZ) .gt.1.0) RVDSC(SINDX,ISIZ) = 1.0

        !         sum up RVDST, the BA-weighted stand average RVDSC
        RVDST = RVDST + RVDSC(SINDX,ISIZ) * BAH(SINDX,ISIZ)
        100 continue
     end do

     !       sum up RVOD, the stand-area-weighted average of RVDST.  Only include
     !       stands with host in them in the average.
     if (BAH(SINDX,NSCL+1) .gt.0.0) then
        RVDST = RVDST / BAH(SINDX,NSCL+1)
        call SPLAAR(SINDX, SAREA, IRC)
        TAREA = TAREA + SAREA
        RVOD = RVOD + RVDST * SAREA
     end if
     900 continue
  end do

  !     find RVOD
  if (TAREA .gt.0.0) then
     RVOD = RVOD / TAREA
  else
     RVOD = 1.0
  end if

  ! Set WASDRY to TRUE to indicate that the drought model has been run.

  WASDRY = .true.

  return
end
