!================!
!   rd : rdbb1   !
!================!
! RD $Id$
subroutine RDBB1
  implicit none

  !  Purpose :
  !     Determines how many races of bark beetle type 1 are eligible to
  !     act in this growth cycle under current stand conditions.  The
  !     mortality rates etc. specified for each beetle race that is active
  !     are reported back to RDOAGM.  By default, each race  that was
  !     scheduled for this time period is rescheduled for the next time
  !     period (whether it was active this time or not, unless the user
  !     specifies otherwise).

  !     Keyword:  BBTYPE1
  !     Inputs:  1 = first outbreak year    2 = host tree species
  !              3 = DBH limit              4 = threshold density of
  !                                             lg.enough host
  !              5 = mortality rate         6 = 0 for reschedule, 1 for
  !                                             don't

  !     Keyword:  DNSCALC
  !     Inputs:  1 = 0 for actual trees/acre count, 1 for SDI calculation
  !              2 = 0 for whole stand, 1 for outside only
  !              3 = 0 for live only, 1 for live + died during last 2 years
  !              4 = slope of SDI function (between -0.5 and -2.5)

  !  Called By :
  !     RDOAGM  [ROOT DISEASE]

  !  Calls :
  !     DBCHK   (SUBROUTINE)   [FVS]
  !     OPFIND  (SUBROUTINE)   [FVS]
  !     OPGET   (SUBROUTINE)   [FVS]
  !     OPDONE  (SUBROUTINE)   [FVS]
  !     OPCOPY  (SUBROUTINE)   [FVS]
  !     OPREDT  (SUBROUTINE)   [FVS]
  !     OPINCR  (SUBROUTINE)   [FVS]
  !     RDCSD   (SUBROUTINE)   [FVS]

  !  Revision History:
  !    21-MAR-00 Lance David (FHTET)
  !       Reduced RETURN statements to 1 at the end of routine.
  !       Added Debug code.
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

  !.... Local variable declarations.

  ! Variables
  logical :: CRIT,DEBUG
  integer :: ISPI,ONLY1X,RACE,NOCOPY,DONE,IACTK
  integer :: KDT,KODE,MYACT(1),NCOPYS,NPS,NTODO
  real    :: DBHLIM,THRESH,MORT
  real    :: PRMS(5)

  !.... Data statements.

  data MYACT /2415/

  !.... SEE IF WE NEED TO DO SOME DEBUG.

  call DBCHK (DEBUG,'RDBB1',5,ICYC)
  if (DEBUG) write (JOSTND,*) 'ENTER RDBB1'

  if (ITRN .le.0) goto 1000
  NOCOPY = 0
  DONE = 0

  !.... Ask OPFIND how many races of this beetle type are scheduled for
  !.... this time period.

  call OPFIND (1,MYACT,NTODO)
  if (NTODO .le.0) goto 1000

  !.... For each race, first get the parameter values.  Check that the
  !.... beetle host species exists before proceeding.

  do RACE = 1,NTODO
     call OPGET (RACE,5,KDT,IACTK,NPS,PRMS)

     if (DEBUG) write (JOSTND,*)&
          &'IN RDBB1: RACE=',RACE,' PRMS=',PRMS

     ISPI = INT(PRMS(1))
     DBHLIM = PRMS(2)
     THRESH = PRMS(3)
     MORT = PRMS(4)
     ONLY1X = INT(PRMS(5))

     if (ISCT(ISPI,1) .eq.0) goto 888

     !....    Second, call RDCSD to calculate the density of trees that
     !....    meet the user's criteria, and indicate via CRIT whether
     !....    this density exceeds the threshold for an active outbreak.

     CRIT = .false.
     call RDCSD(ISPI,DBHLIM,THRESH,CRIT)
     if (DEBUG) write (JOSTND,*) 'IN RDBB1: CRIT=',CRIT
     if (.not. CRIT) goto 888

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
     MININF(NUMBB) = 0.0
     ORATE(NUMBB) = MORT
     OFRATE(NUMBB) = MORT
     IURATE(NUMBB) = MORT
     IIRATE(NUMBB) = MORT

     if (DEBUG) write (JOSTND,*) 'IN RDBB1: NUMBB=',NUMBB

     !....    Check whether the Type 1 beetle races are to remain
     !....    potentially active after the current growth cycle.

     888 continue

     if (ONLY1X .eq.1) NOCOPY = 1

     999 continue
  end do

  !.... If the Type 1 beetle races are to remain active, then copy them
  !.... to the next growth cycle.

  if (NOCOPY .ne.1) then
     call OPCOPY (MYACT(1),IY(ICYC),IY(ICYC+1),NCOPYS,KODE)
     call OPINCR (IY,ICYC,NCYC)
  end if

  1000 continue
  if (DEBUG) write (JOSTND,*) 'EXIT RDBB1'
  return
end
