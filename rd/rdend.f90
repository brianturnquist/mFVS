!================!
!   rd : rdend   !
!================!
! RD $Id$
subroutine RDEND
  implicit none

  !  Purpose :
  !     Reconciles mortality estimates from inside and outside the
  !     root disease areas.

  !  Called By :
  !     RDTREG  [ROOT DISEASE]

  !  Calls :
  !     DBCHK   (SUBROUTINE)   [PROGNOSIS]
  !     RDSSIZ  (SUBROUTINE)   [ROOT DISEASE]
  !     RDSTP   (SUBROUTINE)   [ROOT DISEASE]
  !     RDSUM   (SUBROUTINE)   [ROOT DISEASE]

  !  Local Variables :
  !     <incomplete>

  !  Common Block Variables Used :
  !     <incomplete>

  !  Revision History :
  !  06/12/96 - Matthew K. Thompson
  !           Moved the declaration of DSO, DSII, and DSIU to the
  !           parameter include file RDPARM.
  !  06/15/2006 - Lance R. David
  !           WK2 was being set to 99.9% when it potentially could have been
  !           set to 100% by another model.
  !   08/28/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'

  !.... Common include files.

  include 'CONTRL.f90'
  include 'RDCOM.f90'
  include 'RDARRY.f90'
  include 'ARRAYS.f90'
  include 'RDADD.f90'

  !.... Local variable declarations

  ! Variables
  logical :: DEBUG
  integer :: DRR,DBB,DWND,DNAT,I,IDI,IP,ISL,J,JJ
  real    :: BACKGD,DIE,DIENAT,DIFF,NATIU,PBB,PMORT,TDIEN,TDIEOU,TDIUN,TEST,TPAREA,UNAPP,WMESS

  data DRR      /1/
  data DBB      /2/
  data DWND     /3/
  data DNAT     /4/

  !     Special debug ***

  !     CALL DBCHK (DEBUG,'RDDBUG',6,ICYC)
  !     IF (DEBUG) CALL RDDBUG(1)

  !.... See if we need to do some debug.

  call DBCHK (DEBUG,'RDEND',5,ICYC)

  !.... Zero the dead tree arrays.

  do I = 1,ITRN
     do J = 1,4
        DPROB(I,J,1) = 0.0
        DPROB(I,J,2) = 0.0
        23 continue
     end do
     27 continue
  end do

  TPAREA = 0.0

  do IDI = MINRR,MAXRR
     TPAREA = TPAREA + PAREA(IDI)
     76 continue
  end do

  if (ITRN .le.0.or. TPAREA .eq.0.0) return

  IDI = MAXRR
  do I = 1,ITRN

     !        Calculate total number of trees dying inside infected area.

     if (MAXRR .lt.3) IDI = IDITYP(IRTSPC(ISP(I)))

     !     Skip operation if non-host tree (RNH May98)

     if (IDI .le.0) go to 1000

     !....    Infected trees always have at least background mortality.
     !....    PROBIT + RRKILL is the number of infected trees before death.

     BACKGD = WK2(I) / PROB(I)
     TDIEN = AMAX1(RRKILL(I), BACKGD*(PROBIT(I)+RRKILL(I)))
     !        TDIUN = AMIN1(PROBIU(I) * 0.95, BACKGD*PROBIU(I))

     !....    Uninfected trees may not have any mortality during the root
     !....    disease model.

     TDIUN = AMIN1(PROBIU(I) * 0.95, OAKL(DSIU,I))

     !....    Calculate density of trees dying outside infected area.
     !....    Root disease model may not predict any mortality here.

     !        PMORT = AMIN1(FPROB(I)*0.95, BACKGD*FPROB(I))

     DIFF = SAREA - PAREA(IDI)
     TEST = 0.0
     if (DIFF .gt.0.0) TEST = OAKL(DSO,I) / DIFF
     PMORT = AMIN1(FPROB(I) * 0.95, TEST)

     TDIEOU = PMORT * DIFF

     !....    Calculate newest value of WK2 to pass to FVS.

     WMESS = (TDIEOU + TDIEN + TDIUN) / SAREA
     WK2(I) = AMAX1(WMESS,WK2(I))

     !....    This condition conflicts with FFE and any other extension
     !....    that can impose 100% mortality. LRD 06/16/2006
     !        IF (WK2(I) .GT. PROB(I) * 0.9999) WK2(I) = PROB(I) * 0.999

     if (WK2(I) .gt. PROB(I)) WK2(I) = PROB(I)

     if (DEBUG) write(JOSTND,888) I, WK2(I), PROB(I)
     888 format (' IN RDEND :  I WK2 PROB=',I4,2E15.8)

     !....    Take weighted average over two kinds of roots.

     RROOTT(I) = (RROOTT(I) * WK22(I) + (ROOTL(I) * WK2(I))) /&
          &(WK22(I) + WK2(I) + .00001)
     WK22(I) = WK22(I) + WK2(I)

     !....    Apply natural mortality to inside uninfected trees if not all
     !....    mortality could be accounted for by root disease predictions.
     !....    give the outside trees the first chance at natural mortality
     !....    and then give any of the remaining natural mortality to the
     !....    inside uninfected trees. (this mortality should be weighted
     !....    somehow because it is calculated by prognosis using the
     !....    average density of the stand, not that inside and outside
     !....    centers.)
     !....    Otherwise just apply the mortality calculated from the root
     !....    disease model.

     DIE = TDIUN
     NATIU = 0.0
     !        OUTNAT = 0.0
     UNAPP = 0.0

     if (WMESS .lt. WK2(I)) then

        !....       figure out how much mortality has not been allocated yet
        !....       (in #trees)

        UNAPP = (WK2(I) - WMESS) * SAREA

        !....       some of it will be applied to trees on the outside

        !           OUNAT = BACKGD * FPROB(I) * DIFF - TDIEOU
        UNAPP = UNAPP - (BACKGD * FPROB(I) * DIFF - TDIEOU)

        !....       figure out how much natural mortality could be applied to
        !....       IU trees

        NATIU = BACKGD * PROBIU(I)

        !....       but we will assign at most the remaining amount

        if (NATIU .gt. UNAPP) NATIU = MAX(UNAPP,0.0)

        !....       incorporate the natural mortality into the previously
        !....       calculated mortality

        if (NATIU .gt. TDIUN) DIE = NATIU
     end if
     PROBIU(I) = PROBIU(I) - DIE

     !....    Reduce outside tree density by mortality from root disease.

     if (FPROB(I) - PMORT .gt.1E-6) FPROB(I) = FPROB(I) - PMORT

     !....    Add dead trees to the dead tree list for use by the main snag
     !....    model.
     !....
     !....    TDIEOU is # trees outside dying
     !....    TDIUN  is # trees inside uninfected dying
     !....    TDIEN  is # trees inside, infected dying
     !....    BBKILL is bark beetle killed trees
     !....    OAKL   is all other agent killed trees

     !....    Outside trees: windthrow and bark beetles

     if (OAKL(DSO,I) .gt.0.0) then
        PBB = BBKILL(DSO,I) / OAKL(DSO,I)
        DPROB(I,DBB,DSO) = TDIEOU * PBB
        DPROB(I,DWND,DSO) = TDIEOU * (1 - PBB)
     end if

     !....    Inside, uninfected trees: windthrow, bark beetles, and natural
     !....    (note that in DPROB, DSO is used to mean dead, uninfected)

     if (OAKL(DSIU,I) .gt.0.0) then
        PBB = BBKILL(DSIU,I) / OAKL(DSIU,I)
        DPROB(I,DBB,DSO) = TDIUN * PBB
        DPROB(I,DWND,DSO) = TDIUN * (1 - PBB)
        DPROB(I,DNAT,DSO) = DIE - TDIUN
     end if

     !....    Inside, infected trees: root disease, windthrow and bark
     !....    beetles
     !....    (note that all root disease model mortality to infected trees
     !....    is always applied).

     DPROB(I,DBB,DSII) = BBKILL(DSII,I)
     DPROB(I,DWND,DSII) = OAKL(DSII,I) - BBKILL(DSII,I)
     DPROB(I,DRR,DSII) = RDKILL(I)

     if (DPROB(I,DWND,DSII) .lt.0.0) DPROB(I,DWND,DSII) = 0.0

     !        Calculate natural mortality of infected trees.

     DIENAT = TDIEN - RRKILL(I)
     if (DIENAT .le.1E-6) DIENAT = 0.0

     do J = 1,ISTEP
        do IP = 1,2
           if (PROBI(I,J,IP) .le.0.0) goto 900
           DIE = PROBI(I,J,IP) * DIENAT / PROBIT(I)
           PROBI(I,J,IP) = PROBI(I,J,IP) - DIE
           RRKILL(I) = RRKILL(I) + DIE
           DPROB(I,DNAT,DSII) = DPROB(I,DNAT,DSII) + DIE
           if (PROBI(I,J,IP) .le.1E-6) PROBI(I,J,IP) = 0.0
           899 continue
        end do
        900 continue
     end do

     !....    Update the stump list with natural mortality of infected
     !....    trees.

     if (DIENAT .gt.0.0) then
        JJ = ISP(I)
        call RDSSIZ(JJ,DBH(I),STCUT,ISL,ISPS,IRTSPC)
        call RDSTP (ISL,JJ,DIENAT,DBH(I),ROOTL(I))
     end if

     1000 continue
  end do

  !.... Re-calculate PROBIT

  call RDSUM(ITRN,PROBIT,PROBI,ISTEP)

  return
end
