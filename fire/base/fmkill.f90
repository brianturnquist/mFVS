!========================!
!   fire/base : fmkill   !
!========================!
! FIRE-BASE $Id$
subroutine FMKILL(ICALL)
  implicit none

  !     SINGLE-STAND VERSION

  !     CONVERT THE MORTALITY ESTIMATES PRODUCED BY THE FIRE
  !     MODEL INTO MODIFIED PROGNOSIS MODEL RATES.
  !     Add all newly-killed trees to the snag list.
  !     NOTE:  There is no information on cause-of-death at this time.

  !     IF ICALL = 1 THEN CALLED JUST TO ENSURE FIRE MORTALITY
  !                  IS ADDED TO WK2
  !     IF ICALL = 2 THEN CALLED TO GET ALL EXTRA WK2 MORTALITY
  !                  INTO THE SNAG LIST.

  !     CALLED FROM:  GRADD  [SINGLE-STAND]

  !     CALLS:   FMSSEE
  !              FMSADD

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'PLOT.f90'
  include 'OUTCOM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'FMCOM.f90'

  ! Variables
  integer :: I,YEAR,ICALL,ISHAG
  logical :: DEBUG

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'FMKILL',6,ICYC)
  if (DEBUG) write(JOSTND,7) ICYC,LFMON
  7 format(' ENTERING FMKILL CYCLE = ',I2,' LFMON=',L2)

  !     IF THE FIRE MODEL EXTENSION IS NOT ACTIVE, THEN RETURN

  if (.not. LFMON) return

  if (ICALL .eq.1) then

     !        Loop over all the trees to see if the WK(2) array needs to be
     !        updated. Also activate sprouting and adjust crown ratios.

     !        Set sprout age.  If there was a simulated fire this cycle, use
     !        its year to determine sprout age.  If not, use the year of the
     !        pileburn this cycle.  It is done this way because if both a
     !        simulated fire and a pileburn treatment occur in the same
     !        cycle, the simulated fire was probably the most severe.

     if ((IY(ICYC+1) - BURNYR) .le. IFINT) then
        ISHAG = IY(ICYC+1)-BURNYR
     else
        ISHAG = IY(ICYC+1)-PBURNYR
     end if

     do I = 1, ITRN

        if (DEBUG) write(JOSTND,10) I,PROB(I),FIRKIL(I),WK2(I),&
             &FMICR(I),ICR(I)
        10 format (' IN FMKILL(1), I=',I4,' PROB=',F10.4,' FIRKIL=',&
             &F10.4,' WK2=',F10.4,' FMICR=',I3,' ICR=',I3)

        !           MAKE SURE WE ARE NOT KILL MORE TREES THAN WE HAVE!!!

        if (FIRKIL(I).gt.PROB(I)) FIRKIL(I) = PROB(I)

        !           ADD KILLED TREES TO LIST FOR REGENERATION SPROUTS.

        if (FIRKIL(I) .gt.0.00001) then
           call ESTUMP (ISP(I),DBH(I),FIRKIL(I),ITRE(I),ISHAG)
        end if

        !           If the fire model predicts higher mortality,
        !           then the FVS values need to be adjusted.

        if (FIRKIL(I) .gt. WK2(I)) WK2(I) = FIRKIL(I)

        !           If the fire model has changed the crown ratios, give the
        !           new ones to FVS, and make them negative to tell FVS not
        !           to calculate new ones next cycle.

        if (FMICR(I) .lt.1) FMICR(I) = 1
        if (FMICR(I) .lt. IABS(ICR(I))) then
           ICR(I) = -FMICR(I)
           if (DEBUG) write(JOSTND,20) I,FMICR(I),ICR(I)
           20 format (' IN FMKILL CROWN CHANGED, I=',I4,' FMICR=',I3,&
                &' ICR=',I3)
        end if
     end do

  else if (ICALL .eq.2) then

     !        Now check WK2 again. If any of its values are higher than
     !        FIRKIL, then the mortality needs to be added to the snags

     do I = 1, ITRN

        if (DEBUG) write(JOSTND,30) I,PROB(I),FIRKIL(I),WK2(I),&
             &FMICR(I),ICR(I)
        30 format (' IN FMKILL(2), I=',I4,' PROB=',F10.4,' FIRKIL=',&
             &F10.4,' WK2=',F10.4,' FMICR=',I3,' ICR=',I3)

        !           If the fire model predicts higher mortality, then the FVS
        !           values were adjusted in the previous call. If WK2 is now
        !           higher than the FFE values (FIRKIL), then we need to pass
        !           that information to the snag model.
        !           We do nothing if FIRKIL=WK2.

        if (FIRKIL(I) .lt. WK2(I)) then

           !              Store the FVS mortality in the snag mgmt routines for addition to
           !              the snag pools.  This is only done for tree records where the
           !              background rate is higher than the Fire mortality rate.  Fire
           !              killed trees are placed in the snag pools via calls inside of
           !              FMEFFS and FMTRET.  R&C 07/11/96
           !              Subtract off the fire-killed trees first, since they have already
           !              been added (SB 2/97)

           call FMSSEE (I,ISP(I),DBH(I),HT(I),WK2(I)-FIRKIL(I),&
                &1,DEBUG,JOSTND)

        end if

     end do
     !        end of tree loop.

     !        Now group new snags into species-dbh-ht records,
     !        and collect the canopy material of non-fire-killed snags.

     !sng     YEAR =  IY(ICYC+1) - 1
     YEAR = IY(ICYC+1) - 1

     call FMSADD (YEAR,4)

     !        Now you can zero out the fire model mortality array

     do I = 1, ITRN
        FIRKIL(I) = 0.0
     end do

     !        SET FLAG TO MARK COMPLETION OF THE FIRST MASTER CYCLE.

     if (LFMON2) LFMON2 = .false.

  end if

  return
end
