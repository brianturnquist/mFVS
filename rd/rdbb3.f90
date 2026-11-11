!================!
!   rd : rdbb3   !
!================!
! RD $Id$
subroutine RDBB3
  implicit none

  !  Purpose :
  !     Determines how many races of bark beetle type 3 are eligible to
  !     act in this growth cycle under the current stand conditions.
  !     The mortality rates etc. specified for each beetle race that is
  !     active are reported back to RDOAGM.  By default, each race that
  !     was scheduled for this time period is rescheduled for the next
  !     time period (whether it was active this time or not, unless the
  !     user specifies otherwise).

  !     Keyword:  BBTYPE3
  !     Inputs:  1 = first outbreak year    2 = host tree species
  !              3 = DBH limit              4 = threshold density of lg.
  !                                             enough sick-enough host
  !              5 = mortality rate         6 = proportion of roots
  !                                             infected limit
  !              7 = 0 for reschedule, 1 for don't

  !  Called By :
  !     RDOAGM  [ROOT DISEASE]

  !  Calls :
  !     OPFIND  (SUBROUTINE)   [FVS]
  !     OPGET   (SUBROUTINE)   [FVS]
  !     OPDONE  (SUBROUTINE)   [FVS]
  !     OPCOPY  (SUBROUTINE)   [FVS]
  !     OPINCR  (SUBROUTINE)   [FVS]
  !     OPREDT  (SUBROUTINE)   [FVS]

  ! REVISION HISTORY:
  !    06-AUG-01 Lance R. David (FHTET)
  !       Change MYACT to IACTK in call to OPCOPY because of array to scalar
  !       mismatch reported by LF95.
  !       (previous date of last revision was March 1, 1995)
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

  !.... Local variable declarations.

  ! Variables
  integer :: I,J,IK,I1,I2,ISPI,ONLY1X,NOCOPY,RRTYPE,RACE,DONE,IACTK
  integer :: IP,KDT,KODE,MYACT(1),NCOPYS,NPS,NTODO

  real    :: DBHLIM,THRESH,MORT,RROTEX,STEMS

  real    :: PRMS(6)

  if (ITRN .le.0) return
  NOCOPY = 0
  DONE = 0

  !.... Ask OPFIND how many races of this beetle type are scheduled for
  !.... this time period.

  data MYACT /2417/
  call OPFIND (1,MYACT,NTODO)
  if (NTODO .le.0) return

  !.... For each race, first get the parameter values, determine which
  !.... root rot type affects the beetle's host species, and determine
  !.... the first (I1) and last (I2) records of the host species.  Before
  !.... proceeding, check that vulnerable host exists.

  do RACE = 1, NTODO
     call OPGET (RACE,6,KDT,IACTK,NPS,PRMS)

     ISPI = INT(PRMS(1))
     DBHLIM = PRMS(2)
     THRESH = PRMS(3)
     MORT = PRMS(4)
     RROTEX = PRMS(5)
     ONLY1X = INT(PRMS(6))

     RRTYPE = MAXRR
     if (MAXRR .lt.3) RRTYPE = IDITYP(IRTSPC(ISPI))
     I1 = ISCT(ISPI,1)
     I2 = ISCT(ISPI,2)

     if (ISCT(ISPI,1) .eq.0) goto 888
     if (PAREA(RRTYPE) .le.0.0) goto 888

     !....    Second, go through the host tree records and count the number
     !....    of live stems that meet the user's criteria for size and % of
     !....    roots infected with annosus.  Determine whether the density of
     !....    these stems exceeds the user's threshold for an active beetle
     !....    outbreak.

     STEMS = 0.0
     do J = I1,I2
        I = IND1(J)
        if (DBH(I) .lt. DBHLIM) goto 6

        do IK = 1,ISTEP
           do IP = 1,2
              if (PROPI(I,IK,IP) .lt. RROTEX) goto 4
              STEMS = STEMS + PROBI(I,IK,IP)
              4 continue
           end do
           5 continue
        end do
        6 continue
     end do

     if ((STEMS / PAREA(RRTYPE)) .lt. THRESH) goto 888

     !....    If the beetle is active, call OPDONE to signal the outbreak,
     !....    increment NUMBB and store the specified mortality rates for
     !....    later use in killing trees.  Reschedule the beetle race by
     !....    copying it to the next growth cycle, unless restricted by
     !....    the user.

     call OPDONE (RACE,IY(ICYC))
     DONE = 1

     NUMBB = NUMBB + 1
     if (NUMBB .gt.3*MAXSP) NUMBB = 3*MAXSP
     HOST(NUMBB) = ISPI
     MINDBH(NUMBB) = DBHLIM
     MININF(NUMBB) = RROTEX
     ORATE(NUMBB) = 0.0
     OFRATE(NUMBB) = 0.0
     IURATE(NUMBB) = 0.0
     IIRATE(NUMBB) = MORT

     !....    Check whether the Type 3 beetle races are to remain
     !....    potentially active after the current growth cycle.

     888 continue

     if (ONLY1X .eq.1) NOCOPY = 1

     999 continue
  end do

  !.... If the Type 3 beetle races are to remain active, then copy them
  !.... to the next growth cycle.

  if (NOCOPY .eq.1) goto 1000
  call OPCOPY (MYACT(1),IY(ICYC),IY(ICYC+1),NCOPYS,KODE)
  call OPINCR (IY,ICYC,NCYC)

  1000 continue

  return
end
