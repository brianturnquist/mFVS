!========================!
!   fire/base : fmscro   !
!========================!
! FIRE-BASE $Id$
subroutine FMSCRO (I,SP,DEADYR,DSNAGS,ICALL)
  implicit none

  !     SINGLE-STAND VERSION
  !     CALLED FROM: FMSADD

  !  Purpose:
  !     This subroutine divides the crowns of newly created snags into
  !     pools that will become down debris at specified years in the
  !     future.  The weight of crown material that each snag has in each
  !     size class at the time of death is taken from CROWNW.  The time by
  !     which all of the material of a given size will have fallen from
  !     the dead snag is extrapolated from data in (**Al's reference to
  !     the old Division of Forest Economics report (1961).  The model
  !     assumes that a constant proportion of the material will fall in
  !     each future year up to that time.

  !  Local variable definitions:
  !     ANNUAL:  amount of material in a size class that will come down
  !              each year
  !     DEADYR:  YEAR of Death of current snag (note:  during initializ-
  !              ation, this may be more than one year before the next
  !              year that will be simulated).
  !     DSNAGS:  Density of SNAGS to use in calculating total weight of
  !              crown components (depends on whether FMSCRO has been
  !              called to deal with fire-killed snags or other snags).
  !     DKCL:    decay class
  !     ILIFE:   lifespan of the current class of crown material, as an
  !              integer value
  !     ICALL:   Where was this called from? 1=after a fire,2=after a cut
  !     FALLYR:  position in CWD2B array that corresponds to the year in
  !              which a pool of material should fall.
  !     RLIFE:   lifespan of the current class of crown material, as a
  !              real value
  !     SP:      snag species
  !     TSOFT:   time for snag of this species and size to become soft
  !     YNEXTY:  the number of years between the year of death and the
  !              next year that will be simulated.
  !     YRSCYC:  YeaRS left in current FVS CYCle (including current year)

  !  Common block variables and parameters:

  !.... Parameter statements.

  !.... Parameter include files.
  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'

  !.... Common include files.
  include 'FMCOM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'

  !.... Variable declarations.
  ! Variables
  integer :: I,SP,DEADYR,SIZE,DKCL,IYR,ILIFE,YNEXTY,FALLYR
  real    :: DSNAGS,TSOFT,RLIFE,ANNUAL,NEWBOT,OLDBOT,X
  integer :: ICALL
  real    :: YRSCYC
  logical :: DEBUG

  !     CHECK FOR DEBUG.

  call DBCHK (DEBUG,'FMSCRO',6,ICYC)

  !.... Begin routine.
  if (DSNAGS .le.0.0) return
  DKCL = DKRCLS(SP)

  YRSCYC = FLOAT( IY(ICYC+1)-DEADYR )
  !sng  YRSCYC = FLOAT( IY(ICYC+1)-DEADYR )

  !     find out how long it will be between the year of death and the
  !     next year simulated, so that only crown material to fall in that
  !     year or later is added to CWD2B2 or CWD2B.

  !sng  IF (DEADYR .LT. IY(1)) THEN
  !sng    YNEXTY = IY(1) - DEADYR

  if (DEADYR .lt. IY(1)) then
     YNEXTY = IY(1) - DEADYR
  else
     YNEXTY = 1
  end if

  !     You can skip everything else if all material will fall before
  !     the simulation begins.

  if (YNEXTY .gt. TFMAX) goto 101

  !     Call FMSNGDK to predict years, since death, for snag to become
  !     soft.

  call FMSNGDK(VARACD,SP,DBH(I),TSOFT)
  if (DEBUG) write(JOSTND,7) ICYC, TSOFT, KODFOR, VARACD
  7 format(' FMSCRO CYCLE=',I2,' TSOFT=',F10.1,' KODFOR=',I5,&
       &' VARACD=',A2)

  !     If called from CUTS, then OLDCRW will be holding last year's
  !     crown info, not the dead part of the crown. Thus, we need to do
  !     some additional calculations (which are normally done in FMSDIT).

  X = 1.0
  if (ICALL .eq.2) then
     OLDBOT = OLDHT(I) - OLDCRL(I)
     NEWBOT = HT(I) - (HT(I) * FLOAT(FMICR(I)) / 100.0)
     if (OLDCRL(I) .gt.0.0.and. (NEWBOT-OLDBOT) .gt.0.0) then
        X = ( (NEWBOT-OLDBOT) / OLDCRL(I) ) / YRSCYC
     else
        X = 0.0
     end if
  end if

  !     Divide each class of crown material equally among all the CWD2B2
  !     pools between next year and the shorter of TSOFT or TFALL.

  do SIZE = 0,5

     RLIFE = TFALL(SP,SIZE)
     if (RLIFE .gt. TSOFT) RLIFE = TSOFT
     ILIFE = INT(RLIFE)

     !        next line asks whether ILIFE had to be truncated (or is zero),
     !        and rounds it up to the next highest integer if so.

     if (real(ILIFE) .lt. RLIFE .or. ILIFE .le.0) ILIFE = ILIFE+1
     RLIFE = real(ILIFE)

     !        don't forget to consider the OLDCRW material as well as CROWNW.
     !        but only do this if it's not mortality reconciliation time (icall =4)
     !        since then oldcrw is no longer holding the dead part of the crown
     !        that is to fall each year, but is carrying the crown weight instead.
     !        SAR 11/20/12
     !        Apply a minimum value of 0.001 ounce check on OLDCRW to avoid a
     !        floating point error when call is not mortality reconciliation.
     !        LRD 09/19/24

     ANNUAL = CROWNW(I,SIZE)
     if (ICALL .ne.4) then
        if (OLDCRW(I,SIZE) .lt.0.0000625) OLDCRW(I,SIZE) = 0.0
        if (SIZE .gt.0) ANNUAL = ANNUAL + YRSCYC*OLDCRW(I,SIZE)*X
     end if
     ANNUAL = ANNUAL * DSNAGS / RLIFE

     if (DEBUG) write(JOSTND,*) 'annual=',annual,' yrscyc=',yrscyc,&
          &' oldcrw=',OLDCRW(I,SIZE),' x=',x,' i=',i,' size=',size,&
          &' CROWNW=', CROWNW(I,SIZE),' dsnags =',dsnags,' rlife=',rlife

     if (ANNUAL .gt.0.0) then
        FALLYR = 0
        do IYR = YNEXTY,ILIFE

           FALLYR = IYR + 1 - YNEXTY

           !            Normally, we want to put the stuff into CWD2B2, but if this is
           !            called during mortality reconciliation, CWD2B2 has already been
           !            copied to CWD2B in preparation for the next cycle, so that is
           !            where we need to put the stuff.

           if (ICALL .ne.4) then
              CWD2B2(DKCL,SIZE,FALLYR) = CWD2B2(DKCL,SIZE,FALLYR)&
                   &+ ANNUAL
           else
              CWD2B(DKCL,SIZE,FALLYR) = CWD2B(DKCL,SIZE,FALLYR)&
                   &+ ANNUAL
           end if

        end do
     end if
  end do

  101 continue

  return
end
