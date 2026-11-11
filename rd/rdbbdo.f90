!=================!
!   rd : rdbbdo   !
!=================!
! RD $Id$
subroutine RDBBDO
  implicit none

  !  Purpose :
  !     Determines for each component of each tree record the maximum
  !     mortality that any one of the bark beetles that are currently
  !     eligible to act could inflict on that tree record, and the number
  !     of trees killed by applying this mortality rate.  Reports the
  !     total number of trees killed via :
  !     OAKL(DSO/DSIU/DSII,#TREES), RRKILL(#TREES), and
  !     RROBNK(#TREESPECIES).

  !      ** DOES NOT RECALCULATE THE NUMBER OF LIVE TREES REMAINING, **
  !      ** except in the case of PROBI(#TREES,#GROWTHCYCLES) and    **
  !      ** PROBIT(#TREES).                                          **

  !  Calls:
  !     RDSUM   [ROOT DISEASE]

  !  Called By :
  !     RDOAGM  [ROOT DISEASE]

  !  Local Variables :
  !     DSF    - INTEGER
  !              Dead Standing Fringe (only calculated when BB4 is active
  !              (will DSF=0 if BB4 is not active, because FRINGE=0 unless
  !              BB4 is active)) (note windthrow is included in all cases,
  !              "standing" dead is not really accurate)
  !     MAXKL  - REAL
  !              The maximum mortality rate applicable to the current
  !              component of the current tree record (other than in
  !              'fringe' areas).
  !     MAXFKL - REAL
  !              The maximum fringe-specific mortality rate applicable
  !              to the current component of the current tree record.
  !     NUMDED - REAL
  !              The number of trees in the current component of the
  !              current tree record that just died from bark beetle
  !              attacks.
  !     NUMDEF - REAL
  !              NUMDED for trees in the "fringe".

  !  Revision History :
  !    06/12/96 - Matthew K. Thompson
  !      Moved the declaration of DSO, DSII, and DSIU to the
  !      parameter include file RDPARM.
  !    12-JUL-2002 Lance R. David
  !      Added debug code.
  !   08/26/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

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
  integer :: DSF,RRTYPE,I,IK,BB,ISPI,IP,ISL
  real    :: MAXKL,MAXFKL,NUMDED,NUMDEF,TOTDED
  logical :: DEBUG

  !.... Data statements.

  data DSF /4/

  !.... SEE IF WE NEED TO DO SOME DEBUG.

  call DBCHK (DEBUG,'RDBBDO',6,ICYC)
  if (DEBUG) write (JOSTND,*) 'ENTER: RDBBDO'

  if (ITRN .le.0) return

  !.... Look at every tree record.  Figure out what tree species it is
  !.... and what root rot type attacks it.

  do I = 1,ITRN
     ISPI = ISP(I)
     RRTYPE = MAXRR
     if (MAXRR .lt.3) RRTYPE = IDITYP(IRTSPC(ISPI))

     if (DEBUG) write (JOSTND,*) 'IN: RDBBDO I=',I,' ISPI=',ISPI,&
          &' RRTYPE=',RRTYPE

     !....    Kill Inside Infected trees:  look at the portion of the record
     !....    infected in each time period, find the maximum
     !....    bark-beetle-induced mortality that applies to it, and kill it
     !....    accordingly.

     TOTDED = 0.0

     do IK = 1,ISTEP
        do IP = 1,2
           if (PROBI(I,IK,IP) .eq.0.0) goto 255
           MAXKL = 0.0

           do BB = 1,NUMBB
              if ((HOST(BB).eq.ISPI) .and.&
                   &(MINDBH(BB).le.DBH(I))) then

                 if (MININF(BB) .le. PROPI(I,IK,IP)) then
                    if (IIRATE(BB) .gt. MAXKL) MAXKL = IIRATE(BB)
                 else
                    if (IURATE(BB) .gt. MAXKL) MAXKL = IURATE(BB)
                 end if

              end if
              222 continue
           end do

           if (MAXKL .gt.0.0) then
              NUMDED = PROBI(I,IK,IP) * MAXKL
              PROBI(I,IK,IP) = PROBI(I,IK,IP) - NUMDED
              OAKL(DSII,I) = OAKL(DSII,I) + NUMDED
              BBKILL(DSII,I) = BBKILL(DSII,I) + NUMDED
              RRKILL(I) = RRKILL(I) + NUMDED
              RROBNK(ISPI) = RROBNK(ISPI) + NUMDED
              TOTDED = TOTDED + NUMDED
           end if
           255 continue
        end do
        333 continue
     end do

     !....    Update the stump list with beetle-killed, infected trees.

     if (TOTDED .gt.0.0) then
        call RDSSIZ(ISPI,DBH(I),STCUT,ISL,ISPS,IRTSPC)
        call RDSTP (ISL,ISPI,TOTDED,DBH(I),ROOTL(I))
     end if

     !....    Kill Inside Uninfected trees:  find the maximum applicable
     !....    mortality, and apply it.

     if (PROBIU(I) .eq.0.0) goto 555
     MAXKL = 0.0

     do BB = 1,NUMBB
        if ((HOST(BB) .eq. ISPI)&
             &.and. (MINDBH(BB) .le. DBH(I))&
             &.and. (IURATE(BB) .gt. MAXKL)) then
           MAXKL = IURATE(BB)
        end if
        444 continue
     end do

     if (DEBUG) write (JOSTND,*) 'IN: RDBBDO PROBIU=',PROBIU(I),&
          &' MAXKL=',MAXKL,' KILL INSIDE UNINFECTED'

     if (MAXKL .gt.0.0) then
        NUMDED = PROBIU(I) * MAXKL
        OAKL(DSIU,I) = OAKL(DSIU,I) + NUMDED
        BBKILL(DSIU,I) = BBKILL(DSIU,I) + NUMDED
        RROBNK(ISPI) = RROBNK(ISPI) + NUMDED
        if (DEBUG) write (JOSTND,*) 'IN: RDBBDO NUMDED=',NUMDED,&
             &' DSIU=',DSIU,' OAKL=',OAKL(DSIU,I),' BBKILL=',&
             &BBKILL(DSIU,I),' RROBNK=',RROBNK(ISPI)
     end if

     555 continue

     !....    Kill Outside trees:  find the maximum mortality applicable
     !....    to trees in the "fringe" and non-fringe areas of the stand,
     !....    and apply these mortality rates in proportion to the number
     !....    of trees in each area.   Note that FPROB is a density of trees,
     !....    not a total number of trees.

     if (FPROB(I) .eq.0.0) goto 777
     MAXKL = 0.0
     MAXFKL = 0.0

     do BB = 1, NUMBB
        if ((HOST(BB) .eq. ISPI) .and.&
             &(MINDBH(BB) .le. DBH(I))) then
           if (ORATE(BB) .gt. MAXKL) MAXKL = ORATE(BB)
           if (OFRATE(BB) .gt. MAXFKL) MAXFKL = OFRATE(BB)
        end if
        666 continue
     end do

     if ((MAXKL .gt.0.0) .or. (MAXFKL .gt.0.0)) then
        NUMDEF = FPROB(I) * FRINGE(RRTYPE) * MAXFKL
        NUMDED = FPROB(I) *(SAREA-PAREA(RRTYPE)-FRINGE(RRTYPE)) *&
             &MAXKL + NUMDEF
        BBKILL(DSF,I) = BBKILL(DSF,I) + NUMDEF
        BBKILL(DSO,I) = BBKILL(DSO,I) + NUMDED
        OAKL(DSO,I) = OAKL(DSO,I) + NUMDED
        RROBNK(ISPI) = RROBNK(ISPI) + NUMDED
        FFPROB(I,2) = FFPROB(I,2) - (FFPROB(I,2) * MAXFKL)
     end if

     777 continue

     !....    End of loop over tree records.

     999 continue
  end do

  !.... Get PROBIT totalled up for the new PROBI values.

  call RDSUM (ITRN,PROBIT,PROBI,ISTEP)

  if (DEBUG) write (JOSTND,*) 'EXIT: RDBBDO'

  return
end
