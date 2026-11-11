!=======================!
!   fire/pn : fmsfall   !
!=======================!
! FIRE-PN $Id$
subroutine FMSFALL(IYR,KSP,D,ORIGDEN,DENTTL,ISWTCH,&
     &RSOFT,RSMAL,DFALLN)
  implicit none

  !     SNAG FALL PREDICTION

  !   Purpose:
  !     This routine calculates the base fall rates for snags, based
  !     on species, DBH (large vs small), and whether we're down to the
  !     last 5% of the snag's original stem/ac representation.

  !     The logic in this routine was extracted from its original
  !     location in FMSNAG, in order to generalize the logic for use
  !     with both the FFE snag pools, and the base FVS model snag records.

  !   Called from: FMSNAG to compute fall rate for a given FFE snag pool.
  !                SVSNAGE to compute fall rate for a given FVS snag record.

  !   Local variable definitions:
  !     BASE:    Base rate-of-fall for snags of this size (I.E., rate for
  !              snags with FALLX=1)
  !     D:       Diameter of current snag pool/record.
  !     DENTTL:  If called from FMSNAG:
  !                 Sum of DENIH + DENIS for current snag pool (sum of
  !                 hard + soft snags/ac).
  !              If called from SVSNAGE:
  !                 The total number of SVS snag records still standing
  !                 that were generated from the same source tree, in
  !                 the same year, as the current snag record.
  !     DFALLN:  Target density of snags to fall under normal conditions
  !              (where hard and soft snags fall at the same rate).
  !     DZERO:   Density level (#/acre), at which snag is considered
  !              equal to ZERO.
  !     ISWTCH:  =1 if FMSNAG called this subroutine.
  !              =2 if SVSNAGE called this subroutine.
  !     KSP:     Species number for current snag pool/record.
  !     ORIGDEN: If called from FMSNAG:
  !                 The density (stems per acre) of snags at the time of
  !                 death, for the current snag pool.
  !              If called from SVSNAGE:
  !                 The total number of SVS snag records that were
  !                 generated from the same source tree, in the same
  !                 year, as the current snag record.
  !     RSMAL:   Rate of snag fall implied by PBSMAL and PBTIME
  !     RSOFT:   Rate of snag fall implied by PBSOFT and PBTIME

  !   Common variable definitions:
  !     ALLDWN:  Time by which the last 5% of lrg snags in each group
  !              have all fallen.
  !     FALLX:   Rate-of-FALL correction factors for each species.
  !              Adjusts fall rates for each species, relative to rate
  !              predicted by base equation for a single base spp (PP in
  !              NI variant).
  !              Internal FALLX values can be overridden by the user, via
  !              the SNAGFALL keyword.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'CONTRL.f90'
  include 'FMCOM.f90'

  ! Variables
  integer :: ISWTCH,IYR,JADJ,JSML,JYRSOFT,KSP
  logical :: DEBUG
  real    :: BASE,D,DENTTL,DFALLN,DZERO,ORIGDEN,RSOFT,RSMAL
  real    :: RDANUW
  integer :: IDANUW

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  IDANUW = ISWTCH
  RDANUW = ORIGDEN

  !  Check for debug:

  call DBCHK (DEBUG,'FMSFALL',7,ICYC)

  !  In the first year after a fire, some work is required to determine
  !  what fall rates to use in the coming years.  First, calculate
  !  RSOFT and RSMAL.  These rates are the constant proportion
  !  of snags that must fall each year in order for a total proportion
  !  PBSOFT (or PBSMAL) of snags to have fallen after PBTIME (e.g.,
  !  28% of remaining snags must fall every year in order for 90% of
  !  the initial number to have fallen in 7 years:  (1-0.28)**7 = 0.1).
  !  This could be done outside the snag loop except when either PBSOFT
  !  of PBSMAL equals 1, which may often be the case.  So it's done here.

  DFALLN = 0.
  RSOFT = 0.0
  RSMAL = 0.0
  if (DENTTL .le.0) return
  if ((IYR - BURNYR) .lt. PBTIME) then
     DZERO = NZERO / 50.0

     if (PBSOFT .gt.0.0) then
        if (PBSOFT .lt.1.0) then
           RSOFT = 1 - EXP( LOG(1-PBSOFT) / PBTIME )
        else
           RSOFT = 1 - EXP( LOG(DZERO/DENTTL) / PBTIME )
        end if
     end if

     if (PBSMAL .gt.0.0) then
        if (PBSMAL .lt.1.0) then
           RSMAL = 1 - EXP( LOG(1-PBSMAL) / PBTIME )
        else
           RSMAL = 1 - EXP( LOG(DZERO/DENTTL) / PBTIME )
        end if
     end if
  end if

  !  Calculate the density of snags in this record that would fall under
  !  normal conditions.  This depends on species, dbh and whether 5% are
  !  left.

  !  Call fmr6sdcy now because snag decay affects the snag fall rate through
  !  an adjustment factor.  Also, fmr6sdcy holds the dbh breakpoints used
  !  to determine whether a snag is small, or large.

  call FMR6SDCY(KSP, D, JYRSOFT, JADJ, JSML)

  !  Call fmr6all to determine the snag fall rate.

  call FMR6FALL(KSP, JSML, JADJ, BASE)
  DFALLN = BASE * FALLX(KSP) * DENTTL

  if (DEBUG) then
     write(JOSTND,8) KSP, D, JSML, JADJ, JYRSOFT
     8 format(' FMSFALL KSP=',I5,' DBHS=',F6.2,' JSML=',I3,' JADJ=',I3,&
          &' JYRSOFT=',I5)
     write(JOSTND,9) BASE, FALLX(KSP)
     9 format(' FMSNAG BASE=',F6.3,' FALLX=',F6.2)
  end if

  return
end
