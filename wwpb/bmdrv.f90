!==================!
!   wwpb : bmdrv   !
!==================!
! WWPB $Id$
subroutine BMDRV

  !     PART OF THE PINE BEETLE EXTENSION OF THE PPE.

  !     CALLED FROM -- ALSTD2

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PPEPRM.f90'
  include 'BMPRM.f90'
  include 'PPCNTL.f90'
  include 'BMCOM.f90'

  ! Local variable definitions:

  ! Variables
  integer :: IYR,ISTD,SINDX

  !...THE FOLLOWING ADDED FOR TEST 3/27/00
  real    :: OLDGRF(NSCL)

  !     WRITE REQUESTED DEBUG MSG.

  if (LBMDEB) write (JBMBPR,1) LBMSPR,IBMYR1,IBMYR2
  1 format (/' IN BMDRV: LBMSPR=',L2,'; IBMYR1,IBMYR2=',2I6)

  !     IF THE WPB MODE IS ON, THEN: PROCESS THE OUTBREAK.

  !      IF (LBMSPR .AND. BMSTND .GT. 0) THEN
  !      BETTER TO USE IBMYR1 THAN BMSTND.  COMMENT OUT ABOVE.  AJM 12/05
  if (LBMSPR .and. IBMYR1.gt.0) then

     !     Zero arrays that store volume removal through sanitation and
     !     salvage management actions.

     do ISTD = 1, BMSTND
        CVOLREM(ISTD,1) = 0.0
        CVOLREM(ISTD,2) = 0.0
        SREMOV(ISTD) = 0.0
        MYLSTS(ISTD) = 0
        10 continue
     end do

     !++++++++++++++++++++++++++++++++++++++++++++++++++++++
     !     Initialize ATREEI and AREMS for cycle (RNH Aug98)

     !      IF (LOKS) THEN

     do ISTD1 = 1, BMSTND
        do ISIZ1 = 1, NSCL+1
           ATREEI(ISTD1,ISIZ1) = TREE(ISTD1,ISIZ1,1)
           AREMS(ISTD1,ISIZ1) = 0.0
           15 continue
        end do
        20 continue
     end do
     !      ENDIF
     !+++++++++++++++++++++++++++++++++++++++++++++++++++++++
     !       Loop over the years within the cycle

     do IYR = IBMYR1,IBMYR2

        !         CALL GPCYCL

        write(*,*) iyr

        !         Collect pheromone management info.

        call BMPHER (IYR)
        call BMAPH (IYR)

        !         Other management (except sanitation cuts)

        call BMPSTC (IYR)
        call BMSALV (IYR)

        !         Moved BMSANI here to make stand level (RNH, May98)

        call BMSANI (IYR)
        call BMSMGT (IYR)

        !         Determine the rating values due to 'drought' in this year.

        call BMDRGT (IYR)

        !         Do the first loop over the stands

        do ISTD = 1, BMSTND
           if (.not. STOCK(ISTD)) goto 40

           SINDX = BMSDIX(ISTD)

           call BMCWIN(SINDX,IYR)

           call BMLITE (SINDX,IYR)

           call BMFIRE (SINDX,IYR)

           !           Note that OBB and Defols have become "fast" for now because then
           !           we don't have to worry about what happens over cycle boundaries
           !           when the attacked trees grow. The calls are just commented out
           !           below.

           !           Other bark beetles

           call BMOBB (SINDX,IYR)

           !           Defoliators

           call BMDFOL (SINDX,IYR)

           !           Extra mortality calculations

           call BMQMRT (SINDX,IYR)

           !           Remove trees killed by the fast acting agents: windthrow and fire
           !           and extra mortality (Set slow toggle to false)

           call BMMORT(SINDX,IYR,.false.)

           !           Calculate GRF
           !            WRITE (*,*) STOCK(ISTD), SINDX, ISTD, BMSTND
           !            CALL BMCGRF (SINDX,IYR)
           call BMCGRF (SINDX,IYR,OLDGRF)
           !            WRITE (*,*) ISTD, RVDNST(ISTD)
           !           Do a sanitation cut (note that this is lanscape-level)
           !     Comment out BMSANI to make stand level (RNH, May 98)

           !            CALL BMSANI (SINDX,IYR)

           !           Calculate BKP

           !            CALL BMCBKP (SINDX,IYR)
           call BMCBKP (SINDX,IYR,OLDGRF)

           !           Calculate numerator of attractiveness eqn.

           call BMCNUM (SINDX,IYR)

           40 continue
        end do

        !         Calculate and apply negative feedback if triggered by a high
        !         landscape-level BKP (MJO April98). Added LFDBK flag (MJO July98).

        !     The following call commented out 12/1/99. AJM.  This process now
        !     happens WITHIN BMCBKP, and is now a stand-level phenomena.

        !          IF (LFDBK) CALL BMFDBK (IYR)

        !         Compute attractiveness and move BKP around.

        call BMATCT (IYR)

        !         Do the second loop over stands.

        do ISTD = 1, BMSTND
           if (.not. STOCK(ISTD)) goto 45

           SINDX = BMSDIX(ISTD)

           !           These are now called above with the fast DVs. See comment there.
           !           Other bark beetles
           !           CALL BMOBB (SINDX,IYR)
           !           Defoliators
           !           CALL BMDFOL (SINDX,IYR)

           !          Run Ips routine if Ips is a driving variable or if Ips is main beetle

           if ((PBSPEC .eq.3) .or. (IPSON))&
                &call BMIPS (SINDX,IYR)

           !           Do the non-Ips within-stand beetle dynamics.

           if (PBSPEC .ne.3) call BMISTD(SINDX,IYR)

           !           Print out beetle specific output.

           call BMOUT (SINDX,IYR) ! replaces bmoutm ajm 9/05
           !            CALL BMOUTM (SINDX,IYR)
           !            CALL BMDVO (SINDX,IYR) ! obsolete ajm 9/05

           !           Remove beetle-killed (and slow DV killed) trees.

           call BMMORT(SINDX,IYR,.true.)

           !           Age dead woody pools one year

           call BMAGDW (SINDX,IYR)

           45 continue
        end do

        30 continue
     end do

  end if

  return
end
