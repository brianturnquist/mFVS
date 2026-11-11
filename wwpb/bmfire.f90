!===================!
!   wwpb : bmfire   !
!===================!
! WWPB $Id$
subroutine BMFIRE (ISTD, IYR)

  !     CALLED FROM BMDRV
  !     CALLS   BMFLD
  !             BMFMRT
  !             BMFINT
  !             GPGET2
  !             GPADD

  !  **BMFIRE  Date of last revision:  June 14, 1994

  !  Purpose:
  !     This subroutine is the driving part of the fire model. It first
  !     determines the environmental variables, then selects the appropriate
  !     fuel model. Next, all the other subroutines are called to determine
  !     prob of mort. This is based on information from Jim Brown
  !     and Elizabeth Weinhardt.

  !  Local variable definitions:
  !     FDONE:  Logical indicating if any fire occurred this year
  !     FMOIS:  Fuel/moisture codes
  !     SH:     Scorch height (in feet)
  !     STLAST: Last stand that could be called

  !  Common block variables and parameters:
  !     DWPHOS: From BMCOM; Array to hold volumes of downed host tree volume for
  !             Ips (stratified by standing/dead and size class)
  !     MXDWSZ: From BMPRM; Maximum number of Dead Woody Pool size classes

  !     Keywords: FIRED yrstart, period, firetype (fmois)    (use defaults)
  !        or     FIRES yrstart, period, MOIS(), ATEMP, FWIND

  !.... Parameter statements.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PPEPRM.f90'
  include 'BMPRM.f90'

  !.... Common include files.
  include 'BMCOM.f90'
  include 'BMFCOM.f90'

  !.... Variable declarations.

  ! Variables
  integer :: I,IPC,IAGE,FMOIS,FMD
  integer :: DUM(1)
  logical :: FDONE,LOK
  real    :: SMBURN,LGBURN
  real    :: SH

  dimension PRMS(2)

  save

  !.... Check for debug.
  if (LBMDEB) write(JBMBPR,10) IYR, ISTD
  10 format (' Begin BMFIRE: Year= ',I5, 'Stand= ', I6)

  !     KLUDGE TO GET AROUND COUNTING OF NONSTOCKABLE STANDS.

  if (ICNT .ge. BMEND) ICNT = 0
  ICNT = ICNT + 1

  !     CHECK ACTIVITY SCHEDULER.

  if (ICNT .eq.1) then

     IYR1 = IYR
     NPRMS = 2
     FMOIS = 0
     FDONE = .false.

     !     FETCH THE MOST RECENTLY SCEHEDULED UNTRIGGERED ACTIVITY. IF THERE
     !     ARE ANY SET FOR THIS YEAR (IYR), THEN THEY TAKE OVER ANY CURRENT
     !     SET OF PARAMETERS.

     call GPGET2 (313, IYR1, 7, NPRMS, PRMS, 1, I, DUM, LOK)

     if (LOK) then

        !         (iyr2 is the next year that fire could occur, assuming it occurred
        !           this year - prms(1) is the fire period, in years)
        IYR2 = IYR1 + IFIX(PRMS(1))

        FMOIS = INT(PRMS(2))

        if (LBMDEB) write (JBMBPR,101) IYR1,FMOIS
        101 format (/' IN BMFIRE: IYR1=',I5,' FMOIS=',I4)

     end if
  end if

  !     Zero the scorch array
  do ISIZ = 1,NSCL
     SCORCH(ISTD,ISIZ) = 0.0
     203 continue
  end do

  !     If the fuel model type is 0 then fire is not active this year.

  if (FMOIS .le.0) return

  !.... If fire model contains different number of classes, ND and NL will be
  !     changed there.

  !     1hr, 10hrs, 100hrs
  ND = 3

  !     live
  NL = 0

  !.... Begin Routine

  !.... Select environmental variables: fuel mositure, air temp, wind speed

  if (FMOIS .eq.1) then

     !         "very low moisture" or wildfire case

     MOIS(1, 1) = .04
     MOIS(1, 2) = .05
     MOIS(1, 3) = .05
     MOIS(2, 1) = .7
     ATEMP = 90.0
     FWIND = 20.0 * .3

  else if (FMOIS .eq.2) then

     !         "low moisture" or precribed fire-low mc

     MOIS(1, 1) = .08
     MOIS(1, 2) = .08
     MOIS(1, 3) = .1
     MOIS(2, 1) = 1.1
     ATEMP = 70.0
     FWIND = 10.0 * .3

  else if (FMOIS .eq.3) then

     !         "moderate moisture" or prescribed fire-mod mc

     MOIS(1, 1) = .12
     MOIS(1, 2) = .12
     MOIS(1, 3) = .14
     MOIS(2, 1) = 1.5
     ATEMP = 70.0
     FWIND = 4.0 * .3

  else if (FMOIS .eq.4) then

     !         "high moisture"

     MOIS(1, 1) = .16
     MOIS(1, 2) = .16
     MOIS(1, 3) = .18
     MOIS(2, 1) = 1.5
     ATEMP = 70.0
     FWIND = 6.0 * .3

  end if

  !.... Calculate fuel loadings which will modify fuel model selection

  !      CALL BMFLD(SMALL, LARGE)

  !     These calculations ignore any crownfall from standing dead trees
  do IAG = 1,MXDWAG
     SMALL = SMALL + DDWP(ISTD,1,IAG)
     LARGE = LARGE + DDWP(ISTD,2,IAG)
     100 continue
  end do

  !.... select appropriate fire model
  !     based (somewhat) on habitat type and fuel loading
  if (HABTYP(ISTD) .le.0) then
     FMD = 8
  else if (HABTYP(ISTD) .lt.400) then
     FMD = 9
  else
     FMD = 8
  end if

  !     the amount of small and large fuel present can modify the choice of model.
  if ((SMALL .le.6) .and. (LARGE .gt.15)) FMD = 10
  if ((SMALL .gt.6) .and. (SMALL .le.15)&
       &.and. (LARGE .le.20)) FMD = 10
  if ((SMALL .gt.6) .and. (SMALL .le.15)&
       &.and. (LARGE .gt.20)) FMD = 12
  if ((SMALL .gt.15) .and. (SMALL .le.30)&
       &.and. (LARGE .le.25)) FMD = 12
  if ((SMALL .gt.15) .and. (SMALL .le.30)&
       &.and. (LARGE .gt.25)) FMD = 13
  if ((SMALL .gt.30) .and. (LARGE .ge.30)) FMD = 13

  !.... from letter from Jim Brown
  if (FMD .eq.1) then

     !        Short grass

     MPS(1, 1) = 3500
     FWG(1, 1) = .034
     DEPTH = 1.0
     MEXT(1) = .12
     ND = 1

  else if (FMD .eq.2) then

     !        Timber (grass & understory)

     MPS(1, 1) = 3000
     MPS(1, 2) = 109
     MPS(1, 3) = 30
     MPS(2, 1) = 1500
     FWG(1, 1) = .092
     FWG(1, 2) = .046
     FWG(1, 3) = .023
     FWG(2, 1) = .023
     DEPTH = 1.0
     MEXT(1) = .15
     NL = 1
  else if (FMD .eq.8) then

     !         Closed timber litter

     MPS(1, 1) = 2000
     MPS(1, 2) = 109
     MPS(1, 3) = 30
     FWG(1, 1) = .069
     FWG(1, 2) = .046
     FWG(1, 3) = .115
     DEPTH = .2
     MEXT(1) = .3

  else if (FMD .eq.9) then

     !         Hardwood litter

     MPS(1, 1) = 2500
     MPS(1, 2) = 109
     MPS(1, 3) = 30
     FWG(1, 1) = .134
     FWG(1, 2) = .019
     FWG(1, 3) = .007
     DEPTH = .2
     MEXT(1) = .25

  else if (FMD .eq.10) then

     !         Timber (litter & understory)

     MPS(1, 1) = 2000
     MPS(1, 2) = 109
     MPS(1, 3) = 30
     MPS(2, 1) = 1500
     FWG(1, 1) = .138
     FWG(1, 2) = .092
     FWG(1, 3) = .23
     MPS(2, 1) = .092
     DEPTH = 1.0
     MEXT(1) = .25

     !         living class is present

     NL = 1

  else if (FMD .eq.12) then

     !         Medium logging slash

     MPS(1, 1) = 1500
     MPS(1, 2) = 109
     MPS(1, 3) = 30
     FWG(1, 1) = .184
     FWG(1, 2) = .644
     FWG(1, 3) = .759
     DEPTH = 2.3
     MEXT(1) = .2

  else if (FMD .eq.13) then

     !         Heavy logging slash

     MPS(1, 1) = 1500
     MPS(1, 2) = 109
     MPS(1, 3) = 30
     FWG(1, 1) = .322
     FWG(1, 2) = 1.058
     FWG(1, 3) = 1.288
     DEPTH = 3
     MEXT(1) = .25
  end if

  !.... Compute Byram's Fireline Intensity

  call BMFINT(BYRAM, FLAME, ISTD)

  !.... Calculate scorch height (in feet)
  !     Convert byram to BTU/ft/second (rather than min)

  BYRAM = BYRAM / 60.0
  SH = (63.0 / (140.0 - ATEMP)) * (BYRAM ** (7.0 / 6.0)&
       &/ (BYRAM + FWIND ** 3.0) ** 0.5)

  !.... If fire occurs then reduce fuel loadings: 'SMALL' is assumed to
  !      comprise 'S/DDWP( )' size category 1 (<3") & LARGE is
  !      everything above that

  if (FLAG(1) .eq.0) then

     !        FIRE OCCURRED
     FDONE = .true.

     if ((FMD .eq.10) .or. (FMD .eq.12)&
          &.or. (FMD .eq.13)) then
        SMBURN = .2
     else
        SMBURN = .35
     end if

     LGBURN = .5

     do IAGE = 1,5
        DDWP(ISTD,1,IAGE) = DDWP(ISTD,1,IAGE) * SMBURN

        do I = 2,MXDWSZ
           DDWP(ISTD,I,IAGE) = DDWP(ISTD,I,IAGE) * LGBURN
           1100 continue
        end do

        do IPC = 1,3

           SDWP(ISTD,IPC,1,IAGE) = SDWP(ISTD,IPC,1,IAGE) * SMBURN

           do I = 2,MXDWHZ + 1
              SDWP(ISTD,IPC,I,IAGE) = SDWP(ISTD,IPC,I,IAGE) * LGBURN
              1115 continue
           end do

           1110 continue
        end do
        1000 continue
     end do

     !....    Don't forget to burn new slash too

     do IPC = 1,2
        do I = 1,MXDWHZ
           DWPHOS(ISTD,IPC,I) = DWPHOS(ISTD,IPC,I) * LGBURN
           2200 continue
        end do
        2000 continue
     end do

     !....    Calculate probability of tree mortality

     call BMFMRT(ISTD, IYR, SH, FMD)

  end if

  if (ICNT .ge. BMEND .and. FDONE) then

     !        SINCE FIRE OCCURRED IN SOME STAND THIS YEAR, RESCHEDULE FIRE
     !        TO OCCUR AFTER PERIOD NUMBER OF YEARS (IYR2)

     call GPADD (KODE, IYR2, 313, NPRMS, PRMS, 1, DUM)
     if (LBMDEB) write (JBMBPR,902) IYR2
     902 format (/' IN BMFIRE: FIRE OCCURRED AND IS RESCHEDULED ',&
          &'FOR ',I5)

  else if (ICNT .ge. BMEND) then

     !     FIRE DID NOT OCCUR IN ANY STAND, SO FIRE SHOULD BE RESET TO
     !         OCCUR POSSIBLY NEXT YEAR.

     call GPADD (KODE, (IYR+1), 313, NPRMS, PRMS, 1, DUM)
     if (LBMDEB) write (JBMBPR,903) IYR+1
     903 format (/' IN BMFIRE: FIRE DID NOT OCCUR AND IS RESCHEDULED ',&
          &'FOR NEXT YEAR:',I5)
  end if

  !.... Common Return

  9999 continue

  if (LBMDEB) write (JBMBPR,999) IYR, ISTD
  999 format (' End BMFIRE :  Year=',I5, ' Stand= ',I6)

  return
end
