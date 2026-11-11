!=====================!
!   mistoe : misdgf   !
!=====================!
! MISTOE $Id$
  ! Variables
real function MISDGF(ITREE,ISPC)

  !  Purpose:
  !     Calculates the percentage of diameter growth lost periodically
  !  due to dwarf mistletoe infection based on tree species and the
  !  intensity of infection (individual tree record DMR) and adjusts
  !  the normal diameter growth accordingly (called from DGDRIV once per
  !  tree record). Also processes the MISTGMOD keyword.

  !  Call list definitions:
  !     ISPC:   (I) Current tree species.
  !     ITREE:  (I) Current tree record number.
  !     MISDGF: (O) Returns the 10 year proportion of potential diameter
  !                growth due to mistletoe infection.

  !  Local variable definitions:
  !     DEBUG:  Logical flag to turn debug on or off.
  !     IACTK:  Passed back from OPGET (unused).
  !     IDATE:  Passed back from OPGET (unused).
  !     IDMR:   Dwarf mistletoe rating for current tree record (0-6).
  !     IKSPC:  Species of current tree record (0-11) used for keyword.
  !     NP:     Passed back from OPGET (unused).
  !     NTODO:  Number of actions to perform in a cycle.
  !     PRMS:   Array containing MISTGMOD keyword values.

  !  Common block variables and parameters:
  !     DGPDMR: From MISCOM; diameter growth potentials based on species
  !                and DMR (0-6).
  !     FINT:   From PLOT; current cycle length.
  !     ICYC:   From CONTRL; cycle index number.
  !     IMIST:  From MISCOM; individual tree mistletoe rating.
  !     IY:     From CONTRL; inventory year.
  !     JOSTND: From CONTRL; unit number for stand output.
  !     MAXSP:  From PRGPRM; maximum number species.
  !     MISCYC: From MISCOM; logical flag to process MISTGMOD keyword
  !                only once per cycle.
  !     MISFIT: From MISCOM; which species affected by DM (see MISINT).

  !  Revision History :
  !     28-FEB-95; Lance R. David (MAG)
  !        Changed MISTGMOD keyword to use supplemental record to
  !        supply growth modification proportions for one specie's
  !        DMRs 1-6. Field 2 of keyword record specifies species
  !        being modified.
  !        This replaces the use of a species specific growth
  !        modifier multiplier (variable DMGMLT) that was applied
  !        to all DMRs.
  !     02-AUG-99; Lance R. David (FHTET-FC)
  !        Added definition and data statements for MYACTS array
  !        and replaced the activity code literal in the CALL OPFIND
  !        statements.
  !        This change was provided by Bob Havis (FMSC) to eliminate
  !        LF95 FORTRAN compiler warnings.
  !     20-DEC-03
  !        Removed parameter file (misprm.f77)

  implicit none

  !.... Parameter statements.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'

  !.... Common include files.

  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'MISCOM.f90'

  !.... Variable declarations.

  logical :: DEBUG
  real    :: PRMS(7)
  integer :: MYACTS(1)
  integer :: I,IACTK,IDATE,IDMR,IKSPC,ISPC,ITREE,NP,NTODO

  !     DATA STATEMENTS

  data MYACTS(1) / 2005 /

  !.... Check for debug.

  call DBCHK(DEBUG,'MISDGF',6,ICYC)

  if (DEBUG) write(JOSTND,100)ICYC
  100 format(' Begin MISDGF: Cycle = ',I5)

  !.... Initializations.

  MISDGF = 1.0

  !.... Check MISCYC; this flag set to TRUE if the MISTGMOD keyword has
  !.... been processed this cycle (i.e. this will happen only once per
  !.... cycle even though this function is called for every tree record).

  if (.not.MISCYC(1,ICYC)) then
     MISCYC(1,ICYC) = .true.

     !....    Process MISTGMOD keyword (activity code 2005) if scheduled.

     NTODO = 0
     call OPFIND(1,MYACTS(1),NTODO)

     if (NTODO.gt.0) then
        do I = 1,NTODO
           call OPGET(I,7,IDATE,IACTK,NP,PRMS)
           call OPDONE(I,IY(ICYC))

           !....          Check for a particular species number.
           !....          Get mistletoe diameter growth proportions from keyword;
           !....          1.0 indicates no growth potential impact (lessen DM
           !....          effects on diameter); a value near 0.0 will decrease
           !....          growth potential (heighten DM effects).
           !....          MISFIT insures that only species of this variant that
           !....          are affected by mistletoe will get diameter growth
           !....          modifiers, others will be set to 1.0 which is no effect.

           IKSPC = IFIX(PRMS(1))
           if (IKSPC.ne.0) then
              do IDMR = 2,7
                 if (MISFIT(IKSPC).eq.1) then
                    DGPDMR(IKSPC,IDMR) = PRMS(IDMR)
                 else
                    DGPDMR(IKSPC,IDMR) = 1.0
                 end if
                 120 continue
              end do
           else

              !....             Otherwise new growth modifiers applied to all
              !....             affected species.

              do IKSPC = 1,MAXSP
                 do IDMR = 2,7
                    if (MISFIT(IKSPC).eq.1) then
                       DGPDMR(IKSPC,IDMR) = PRMS(IDMR)
                    else
                       DGPDMR(IKSPC,IDMR) = 1.0
                    end if
                    140 continue
                 end do
                 150 continue
              end do
           end if
           200 continue
        end do
     end if
  end if

  !.... Calculate mistletoe diameter growth proportion including effects
  !.... of user supplied proportions based on species and DMR.

  IDMR = IMIST(ITREE)
  MISDGF = DGPDMR(ISPC,IDMR+1)

  !.... Force normal growth for uninfected trees.

  if (IDMR.eq.0) MISDGF = 1.0

  !.... Force upper and lower bounds on diameter growth.

  if (MISDGF.gt.1.0) MISDGF = 1.0
  if (MISDGF.lt.0.0) MISDGF = 0.0

  !.... Common return.

  if (DEBUG) write(JOSTND,9010) ICYC, ITREE, IMIST(ITREE), ISPC,&
       &MISDGF
  9010 format(' End MISDGF: Cycle, Tree record, IMIST, SPC, MISDGF = ',&
       &I5,I7,I3,I5,F6.2)

  return
end
