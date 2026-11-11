!========================!
!   fire/base : fmsdit   !
!========================!
! FIRE-BASE $Id$
subroutine FMSDIT
  implicit none

  !     PART OF THE FIRE MODEL EXTENSION. THIS ROUTINE IS ENTERED
  !     AT THE START OF EACH CYCLE. VALUES FOR SCCF, AND FIRKIL
  !     ARE ZEROED HERE,

  !     Called from GRINCR

  !  Local Variable Definitions:
  !     CYCLEN: length in years of the current FVS cycle
  !     NEWBOT: the height of the bottom of the crown in the current cycle
  !     OLDBOT: the height of the bottom of the crown in the previous cycle

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'
  include 'FMCOM.f90'
  include 'FMFCOM.f90'

  ! Variables
  integer :: I,J
  real    :: CYCLEN,NEWBOT,OLDBOT
  logical :: DEBUG
  real    :: TONREM,X

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'FMSDIT',6,ICYC)
  if (DEBUG) write(JOSTND,7) ICYC,LFMON
  7 format(' ENTERING ROUTINE FMSDIT CYCLE = ',I2,' LFMON=',L2)

  !     CHECK TO SEE IF THE FIRE MODEL IS ACTIVE

  if (.not. LFMON) return

  !     SET IFMYR1 TO -1 TO SIGNAL THAT THE FIRE SIMULATION
  !     HAS NOT STARTED...THEN SET SOME FIRE MODEL EVENT
  !     MONITOR VARIABLES TO UNSET.

  IFMYR1 = -1

  call EVUST4(22) ! CROWNIDX
  call EVUST4(26) ! CRBASEHT
  call EVUST4(27) ! TORCHIDX
  call EVUST4(28) ! CRBULKDN

  !     SET INITIAL VALUES TO -1 (SEE **EVTSTV** FOR FULL LIST)
  !     421 MINSOIL   PERCENTAGE OF MINERAL SOIL EXPOSURE (FM)
  !     423 FIREYEAR  CALENDAR YEAR OF LAST FIRE; -1 OTHERWISE (FM)
  !     420 FIRE      = 1 IF THERE WAS A FIRE THE PREVIOUS CYCLE;
  !                   = 0 OTHERWISE (FM)

  if (ICYC .eq.1) then
     call EVSET4(21, -1.0)
     call EVSET4(23, FLOAT(BURNYR))
     call EVSET4(20, 0.0)
  end if

  FMKOD = KODTYP

  FMSLOP = SLOPE
  SCCF = 0.0
  TONREM = 0.0

  CYCLEN = FLOAT( ( IY(ICYC+1)) - IY(ICYC) )

  !     If we are on the first cycle, then the old crown is not
  !     known for the tree records.

  do I = 1,ITRN
     FMPROB(I) = PROB(I)
     FMICR(I) = ICR(I)
  end do
  do I = 1,MAXTRE
     FIRKIL(I) = 0.0
  end do

  TONRMS = 0.0
  TONRMH = 0.0
  TONRMC = 0.0

  if (ICYC.gt.1) then
     do I = 1, ITRN

        !       Also calculate the annual amount of old crown material to fall
        !       as a result of crown lifting in the current FVS cycle.  See
        !       documentation of FMOLDC for further explanation.
        !       Apply a minimum value of 0.001 ounce check on OLDCRW to avoid a
        !       floating point error when call is not mortality reconciliation.
        !       LRD 09/19/24

        OLDBOT = OLDHT(I) - OLDCRL(I)
        NEWBOT = HT(I) - (HT(I) * FLOAT(ICR(I)) / 100.0)

        if ((OLDCRL(I) .gt.0.001).and.(NEWBOT-OLDBOT.gt.0.0)) then
           X = ( (NEWBOT-OLDBOT) / OLDCRL(I) ) / CYCLEN
           do J = 0,5
              if (OLDCRW(I,J) .lt.0.0000625) then
                 OLDCRW(I,J) = 0.0
              else
                 OLDCRW(I,J) = X * OLDCRW(I,J)
              end if
           end do
        else
           do J = 0,5
              OLDCRW(I,J) = 0.0
           end do
        end if

     end do
  end if

  !     Calculate the weight of crown components, which several FM
  !     routines will want. This only needs to be done once each
  !     cycle because it depends mostly on ht and dbh.

  call FMCROW

  !     If this is the first year of the simulation, add in the
  !     snags that are picked up during the inventory.  Set the year
  !     of death equal to the mortality measurement period.  These
  !     trees include "OLD" dead and "RECENT" mortlity.

  if (LFMON2) call FMSADD (IY(1)-IFIX(FINTM),3)

  return
end
