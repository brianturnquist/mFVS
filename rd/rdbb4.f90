!================!
!   rd : rdbb4   !
!================!
! RD $Id$
subroutine RDBB4
  implicit none

  !  Purpose :
  !     Determines how many races of bark beetle type 4 are eligible to
  !     act in this growth cycle under current stand conditions.  The
  !     mortality rates etc. specified for each beetle race that is
  !     active are reported back to RDOAGM.  By default, each race that
  !     was scheduled for this time period is rescheduled for the next
  !     time period (whether it was active this time or not, unless the
  !     user specifies otherwise).

  !     Keyword :  BBTYPE4
  !     Inputs : 1 = first outbreak year
  !              2 = host tree species
  !              3 = DBH limit
  !              4 = threshold density of lg.enough sick-enough host
  !                  (live or newly dead)
  !              5 = infected mortality rate
  !              6 = proportion-of-roots-infected limit
  !              7 = inside uninf. mort. rate
  !              8 = fringe mortality rate
  !              9 = outside center+fringe mortality rate
  !             10 = 0 for reschedule, 1 for don't

  !  Called By :
  !     RDOAGM  [ROOT DISEASE]

  !  Calls :
  !     OPFIND  (SUBROUTINE)   [FVS]
  !     OPGET   (SUBROUTINE)   [FVS]
  !     OPDONE  (SUBROUTINE)   [FVS]
  !     RDAREA  (SUBROUTINE)   [ROOT DISEASE]
  !     OPCOPY  (SUBROUTINE)   [FVS]
  !     OPINCR  (SUBROUTINE)   [FVS]
  !     OPREDT  (SUBROUTINE)   [FVS]

  !  Revision History :
  !     06/12/96 - Matthew K. Thompson
  !                Moved the declaration of DSII to the parameter
  !                include file RDPARM.

  !     03/04/98 - Robert N. Havis
  !                Increased size of arrays PREVA and PREVB from 2 to 4
  !                to accomodate 4 types of root diseases. Increase
  !                corresponding number of loops in statement 500 and 501
  !    06-AUG-01 Lance R. David (FHTET)
  !       Change MYACT to IACTK in call to OPCOPY because of array to scalar
  !       mismatch reported by LF95.
  !    10-DEC-07 Lance R. David (FHTET)
  !       Update to argument list in call to OPCOPY, added variable
  !       DONE for debug/tracking (from Don Robinson).
  !   08/26/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.
  !....................................................................

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'

  !.... Common include files.

  include 'RDCOM.f90'
  include 'RDARRY.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'RDADD.f90'
  include 'PLOT.f90'

  !.... Local variable declarations.

  ! Variables
  integer :: I,J,IK,I1,I2,ISPI,ONLY1X,NOCOPY,DONE,RRTYPE,RACE,IACTK,MYACT(1)

  integer :: IP,KDT,KODE,NCOPYS,NPS,NTODO

  real    :: DBHLIM,THRESH,MORTII,MORTIU,MORTF,MORTO,STEMS

  real    :: PRMS(9),PREVA(4),PREVB(4),RROTEX,XXXX

  NOCOPY = 0
  DONE = 0
  if (ITRN .le.0) return

  !.... Ask OPFIND how many races of this beetle type are scheduled for
  !.... this time period.

  data MYACT /2432/
  call OPFIND (1,MYACT,NTODO)
  if (NTODO .le.0) return

  !.... For each race, first get the parameter values, determine which
  !.... root rot type affects the beetle host species, and determine the
  !.... first (I1) and last (I2) tree records of the host species.  Check
  !.... that vulnerable host exists before proceeding.

  do RACE = 1, NTODO
     call OPGET (RACE,9,KDT,IACTK,NPS,PRMS)

     ISPI = INT(PRMS(1))
     DBHLIM = PRMS(2)
     THRESH = PRMS(3)
     MORTII = PRMS(4)
     RROTEX = PRMS(5)
     MORTIU = PRMS(6)
     MORTF = PRMS(7)
     MORTO = PRMS(8)
     ONLY1X = INT(PRMS(9))

     RRTYPE = MAXRR
     if (MAXRR .lt.3) RRTYPE = IDITYP(IRTSPC(ISPI))
     I1 = ISCT(ISPI,1)
     I2 = ISCT(ISPI,2)

     if (ISCT(ISPI,1) .eq.0) goto 888
     if (PAREA(RRTYPE) .le.0.0) goto 888
     if (PCOLO(IRTSPC(ISPI),RRTYPE) .lt. RROTEX) goto 888

     !....    Second, go through the host tree records and count the number
     !....    of living stems PLUS stems that died in the last two years
     !....    that meet the user's criteria for size and % of roots infected
     !....    with annosus.  Determine whether the density of these stems
     !....    exceeds the user's threshold for an active beetle outbreak.

     STEMS = 0.0

     do J = I1,I2
        I = IND1(J)
        if (DBH(I) .lt. DBHLIM) goto 6

        do IK = 1,ISTEP
           do IP = 1,2
              if (PROPI(I,IK,IP) .lt. RROTEX) goto 4
              STEMS = STEMS + ((2/FINT) * PROBI(I,IK,IP))
              4 continue
           end do
           5 continue
        end do

        STEMS = STEMS + ((2/FINT) * (PROAKL(DSII,I) + PRANKL(I)))
        6 continue
     end do

     if ((STEMS / PAREA(RRTYPE)) .lt. THRESH) goto 888

     !....    If the beetle is active, call OPDONE to signal the outbreak,
     !....    increment NUMBB and store the specified mortality rates for
     !....    later use in killing trees.

     call OPDONE (RACE,IY(ICYC))
     DONE = 1

     NUMBB = NUMBB + 1
     if (NUMBB .gt.3*MAXSP) NUMBB = 3*MAXSP
     HOST(NUMBB) = ISPI
     MINDBH(NUMBB) = DBHLIM
     MININF(NUMBB) = RROTEX
     ORATE(NUMBB) = MORTO
     OFRATE(NUMBB) = MORTF
     IURATE(NUMBB) = MORTIU
     IIRATE(NUMBB) = MORTII

     !....    Now, if it's not already been done for a previous beetle race,
     !....    calculate the "fringe" area that is within 1 root diameter of
     !....    an infection center in the stand - RDBBDO will need this to
     !....    know how many trees to kill at the higher rate that applies in
     !....    this area.

     if (FRINGE(RRTYPE) .gt.0.0) goto 888

     !....    This procedure is pasted from RRJUMP, to compute the areas in
     !....    SAREA and PAREA() at the next timestep.  Then a bunch of
     !....    values have to be set back, so that the "true" call to RRJUMP
     !....    can do them. The future area of the uninfected stand is called
     !....    NSAREA; the area of the new torus around existing patches is
     !....    called FRINGE().

     if (NCENTS(RRTYPE) .eq.0) goto 888

     !....   Save PAREA() and OOAREA() so that they can be restored at the
     !....   end.

     !     Set number of loops to 4 rather than 2 to correspond to total
     !     number of RR diseases available (4). RNH(FEB98)

     do I = 1,4
        PREVA(I) = PAREA(I)
        PREVB(I) = OOAREA(I)
        500 continue
     end do

     XXXX = 2.0 * RRJINC(RRTYPE)

     do I = 1,NCENTS(RRTYPE)
        if (PCENTS(RRTYPE,I,3) .le.0) goto 700
        PCENTS(RRTYPE,I,3) = PCENTS(RRTYPE,I,3) + XXXX
        700 continue
     end do

     IRRSP = RRTYPE
     call RDAREA

     FRINGE(RRTYPE) = PAREA(RRTYPE) - PREVA(RRTYPE)
     FRINGE(RRTYPE) = AMAX1(0.0,FRINGE(RRTYPE))

     !....   Put PAREA() and OOAREA() back to their earlier state.

     !     Set number of loops to 4 rather than 2 to correspond to total
     !     number of RR diseases available (4). RNH(FEB98)

     do I = 1,4
        PAREA(I) = PREVA(I)
        OOAREA(I) = PREVB(I)
        501 continue
     end do

     !....   Put PCENTS back to its earlier state.

     do I = 1,NCENTS(RRTYPE)
        if (PCENTS(RRTYPE,I,3) .le.0) goto 701
        PCENTS(RRTYPE,I,3) = PCENTS(RRTYPE,I,3) - XXXX
        701 continue
     end do

     !....   Check whether the Type 4 beetle races are to remain potentially
     !....   active after the current growth cycle.

     888 continue

     if (ONLY1X .eq.1) NOCOPY = 1

     999 continue
  end do

  !.... If the Type 4 beetle races are to remain active, then copy them
  !.... to the next growth cycle.

  if (NOCOPY .eq.1) goto 1000
  call OPCOPY (MYACT(1),IY(ICYC),IY(ICYC+1),NCOPYS,KODE)
  call OPINCR (IY,ICYC,NCYC)

  1000 continue

  return
end
