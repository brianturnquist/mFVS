!===================!
!   wwpb : bmqmrt   !
!===================!
! WWPB $Id$
subroutine BMQMRT (ISTD,IYR)

  !     This routine simly calculates extra mortality on host or non-host
  !     trees that is increasing each year of the period.  Note that this
  !     is condsidered a "slow" mortality source and will never be more
  !     than is predicted by FVS

  !  CALLED FROM BMDRV

  !  Definitions:
  !     ITYP:   Type of tree: 1=host (PB host), 2=non-host
  !     MINSIZ: Minimum size class for mortality
  !     MAXSIZ: Maximum size class for mortality
  !     NYEAR:  Total number of years of extra mortality
  !     R:      Temporary variable used in calcs of yearly mortality
  !     SUM:    Variable needed in calcs of yearly mortality
  !     TSURV:  Total mortality over the course of the period
  !     YMORT:  Mortality in current year (proportion)

  !  Common block variables and parameters:
  !     NYR:    Number of years into the current extra mortality period

  !.... Parameter statements.

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
  logical :: LOK
  integer :: ISIZ
  integer :: ITYP
  integer :: MINSIZ,MAXSIZ
  integer :: NYEAR,NPRMS
  integer :: DUM(1)
  real    :: R
  real    :: SUM
  real    :: TSURV
  real    :: YMORT

  dimension PRMS(6)

  save

  if (LBMDEB) write(JBMBPR,10) IYR, ISTD
  10 format(' Begin BMDFOL: Year= ',I5, 'Stand= ', I6)

  !     KLUDGE TO GET AROUND COUNTING OF NONSTOCKABLE STANDS.

  if (ICNT .ge. BMEND) ICNT = 0
  ICNT = ICNT + 1

  !     FETCH THE MOST RECENTLY SCEHEDULED UNTRIGGERED ACTIVITY. IF THERE
  !     ARE ANY SET FOR THIS YEAR (IYR), THEN THEY TAKE OVER ANY CURRENT
  !     SET OF PARAMETERS.

  if (ICNT .eq.1) then

     IYR1 = IYR
     NPRMS = 6
     TSURV = 1.0

     call GPGET2 (307, IYR1, 7, NPRMS, PRMS, 1, I, DUM, LOK)

     if (LOK) then

        IYR2 = IYR1 + IFIX(PRMS(1)) - 1

        ITYP = INT(PRMS(2))
        MINSIZ = INT(PRMS(3))
        MAXSIZ = INT(PRMS(4))
        TSURV = 1 - PRMS(5)
        NYEAR = INT(PRMS(6))

        if (LBMDEB) write (JBMBPR,101) IYR1,NYEAR,MINSIZ,MAXSIZ,&
             &ITYP, TSURV
        101 format (/' IN BMQMRT: IYR1=',I5,' NYEAR=',I4,' MINSIZE=',&
             &I4,' MAXSIZE=',I4,' SPECIES=',I3,' SURVIVAL=',F5.3)

        if (IYR2.ge.MIY(MICYC)) then
           PRMS(1) = IYR2 - MIY(MICYC) + 1
           call GPADD (KODE, MIY(MICYC), 307, NPRMS, PRMS, 1, DUM)
           IYR2 = MIY(MICYC) - 1
           if (LBMDEB) write (JBMBPR,103) PRMS(1),IYR2
           103 format (/' IN BMQMRT: QUICK MORTALITY SCHEDULED FOR ',&
                &'THE NEXT MASTER CYCLE. DURATION WILL BE =',F5.0,&
                &'  NEW IYR2=',I5)
        end if
     else
        !         Set survival to 100% to signify that no extra mortalty occurs this year.
        if (IYR .gt. IYR2) TSURV = 1.0
     end if
  end if

  if (TSURV .ge.1.0) return

  !     Check to make sure some trees exist in those size classes
  do ISIZ = MINSIZ, MAXSIZ
     if (TREE(ISTD,ISIZ,ITYP) .gt.0.0) goto 17
     15 continue
  end do
  return
  17 continue

  !     Mortality is determined through an equation relating the total mortality
  !     (or survival) over the period to decreasing survival each year of the
  !     outbreak to simulate the cummulative effects of defoliators.
  !     The basic equation is: TSURV=MULT(r^i) where MULT goes from
  !     1 to NYEAR.

  NYR(ISTD) = NYR(ISTD) + 1

  SUM = 0.0
  do I = 1,NYEAR
     SUM = SUM + I
     20 continue
  end do

  R = TSURV**(1/SUM)
  YMORT = 1 - (R**NYR(ISTD))

  YMORT = AMIN1(YMORT,1.0)
  YMORT = AMAX1(YMORT,0.0)

  do ISIZ = MINSIZ,MAXSIZ
     if (TREE(ISTD,ISIZ,ITYP) .le.0.0) goto 30

     TKILL = TREE(ISTD,ISIZ,ITYP) * YMORT

     !            Accumulate OAKILL and dead woody pools

     OAKILL(ISTD,ISIZ,ITYP) = OAKILL(ISTD,ISIZ,ITYP) + YMORT
     J = L2D(ISIZ)
     if (ITYP .eq.1.and. J .gt.0)&
          &DWPHOS(ISTD,1,J) = DWPHOS(ISTD,1,J) + TKILL
     J = J + 1
     K = IQPTYP(ISTD,ITYP)
     SDWP(ISTD,K,J,1) = SDWP(ISTD,K,J,1) +&
          &TKILL * TVOL(ISTD,ISIZ,1)

     30 continue
  end do

  !     Reset the number of years if this is last year of outbreak

  if (NYR(ISTD) .eq. NYEAR) NYR(ISTD) = 0

  if (LBMDEB) write(JBMBPR,99) IYR, ISTD
  99 format(' End BMQMRT: Year= ',I5, 'Stand= ', I6)

  return
end
