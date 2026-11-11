!================!
!   rd : rdbb2   !
!================!
! RD $Id$
subroutine RDBB2
  implicit none

  !  Purpose :
  !     Determines how many races of bark beetle type 2 are eligible to
  !     act in this growth cycle under current stand conditions.  The
  !     mortality rates etc. specified for each beetle race that is
  !     active are reported back to RDOAGM. By default, each race that
  !     was scheduled for this time period is rescheduled for the next
  !     time period (whether it was active this time or not, unless the
  !     user specifies otherwise).

  !     Keyword:  BBTYPE2
  !     Inputs:  1 = first outbreak year    2 = host tree species
  !              3 = DBH limit              4 = threshold density of
  !                                             lg. enough windfallen host
  !              5 = "KILL Factor"          6 = 0 for reschedule,
  !                                             1 for don't

  !  Called By :
  !     RDOAGM  [ROOT DISEASE]

  !  Calls :
  !     OPFIND  (SUBROUTINE)   [FVS]
  !     OPGET   (SUBROUTINE)   [FVS]
  !     OPDONE  (SUBROUTINE)   [FVS]
  !     OPREDT  (SUBROUTINE)   [FVS]
  !     OPCOPY  (SUBROUTINE)   [FVS]
  !     OPINCR  (SUBROUTINE)   [FVS]

  ! REVISION HISTORY:
  !    06-AUG-01 Lance R. David (FHTET)
  !       Change MYACT to IACTK in call to OPCOPY because of array to scalar
  !       mismatch reported by LF95.
  !       (previous date of last revision was 12/7/93)
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

  include 'CONTRL.f90'
  include 'RDCOM.f90'
  include 'RDARRY.f90'
  include 'ARRAYS.f90'
  include 'RDADD.f90'

  !.... Local variable declarations.

  ! Variables
  integer :: I,J,I1,I2,ISPI,ONLY1X,NOCOPY,RACE,DONE,IACTK
  integer :: KDT,KODE,MYACT(1),NCOPYS,NPS,NTODO
  real    :: KILLF,CLSUML,CLSUMI,SPSUMI,SPSUML,CLCNT,CLCNTA

  real    :: DBHLIM,THRESH,MORT,TSTEM,TDENS,CLKILL
  real    :: PRMS(5)

  if (ITRN .le.0) return
  NOCOPY = 0
  DONE = 0

  !.... Ask OPFIND how many races of this beetle type are scheduled for
  !.... this time period.

  data MYACT /2416/
  call OPFIND (1,MYACT,NTODO)
  if (NTODO .le.0) return

  !.... For each race, first get the parameter values, and determine the
  !.... first (I1) and last (I2) records of the host species.  Check that
  !.... there is at least one record of the host species, and that some
  !.... were windthrown.  NOTE that in this case PRMS(4) is NOT the
  !.... mortality rate applied to eligible trees in a beetle outbreak.
  !.... Rather, it is the number of standing trees that will die from
  !.... bark beetles FOR EACH TREE KILLED BY WINDTHROW.  The default
  !.... is that, for each tree killed by windthrow, 0.875 trees will
  !.... die from beetles (from McNamee et al.1991).

  do RACE = 1, NTODO
     call OPGET (RACE,5,KDT,IACTK,NPS,PRMS)

     ISPI = INT(PRMS(1))
     DBHLIM = PRMS(2)
     THRESH = PRMS(3)
     KILLF = PRMS(4)
     ONLY1X = INT(PRMS(5))

     I1 = ISCT(ISPI,1)
     I2 = ISCT(ISPI,2)

     if (ISCT(ISPI,1) .eq.0) goto 888
     if (WINDSP(ISPI,3) .le.0.0) goto 888

     !....    Second, go through the host tree records and determine the
     !....    number of windfallen stems larger than the user's size
     !....    criteria.  This is done by some conversion of the proportion
     !....    of the total crown area that is due to trees of this size.
     !....    The 'L' subscript means "Clean", not "Live".

     !....    Zero out class & species accumulators.
     !....    'class' is defined as greater than specified DBH.

     CLSUML = 0
     CLSUMI = 0
     SPSUML = 0
     SPSUMI = 0

     !     GET PROP'NS OF FALLEN STEMS IN SIZE CLASS
     !     FROM SUM OF WEIGHT COEFFS FOR ALL FALLEN IN
     !     PROP TO THOSE WHICH QUALIFY
     !     NB. WEIGHT COEFFICIENTS ARE THOSE USED TO CALCULATE
     !     SUSCEPTIBLITY TO WINDFALL IN SUBROUTINE RROWIN

     do J = 1,ILEND
        I = ISTEML(J)
        if (ISP(I) .ne. ISPI) goto 10
        SPSUML = SPSUML + WINDWL(I)
        if (DBH(I) .lt. DBHLIM) goto 10
        CLSUML = CLSUML + WINDWL(I)
        10 continue
     end do

     !....    Infected stems.

     do J = 1,IIEND
        I = ISTEMI(J)
        if (ISP(I) .ne. ISPI) goto 20
        SPSUMI = SPSUMI + WINDWI(I)
        if (DBH(I) .lt. DBHLIM) goto 20
        CLSUMI = CLSUMI + WINDWI(I)
        20 continue
     end do

     !....    Number of fallen stems in size class.

     if (SPSUML .lt.1E-3) SPSUML = 1E-3
     if (SPSUMI .lt.1E-3) SPSUMI = 1E-3
     CLCNT = (WINDSP(ISPI,1) * CLSUML / SPSUML) +&
          &(WINDSP(ISPI,2) * CLSUMI / SPSUMI)
     CLCNTA = CLCNT / SAREA

     ! Compare CLCNTA, the density of windfallen stems that met the user's size criteria, to
     ! the user's threshold for an active beetle outbreak.

     if (CLCNTA .lt. THRESH) goto 888

     ! If the beetle is active, call OPDONE to signal the outbreak, increment NUMBB and
     ! store the specified parameter values for later use in killing trees.  Calculate the
     ! mortality rate needed from KILLF and the number of windfallen trees.

     call OPDONE (RACE,IY(ICYC))
     DONE = 1

     NUMBB = NUMBB + 1
     if (NUMBB .gt.3*MAXSP) NUMBB = 3*MAXSP
     HOST(NUMBB) = ISPI
     MINDBH(NUMBB) = DBHLIM
     MININF(NUMBB) = 0.0

     !....    target number of trees to kill

     CLKILL = CLCNT * KILLF

     !....    number of live trees (note:  this is total before windthrow occurred)

     TDENS = 0.0

     do J = I1,I2
        I = IND1(J)
        if (DBH(I) .lt. DBHLIM) goto 30
        TDENS = TDENS + PROBL(I)
        30 continue
     end do

     TSTEM = TDENS * SAREA

     !....    mortality rate = number to kill / number living

     if (CLKILL .lt. TSTEM) then
        MORT = CLKILL / TSTEM
     else
        MORT = 1.0
     end if

     ORATE(NUMBB) = MORT
     OFRATE(NUMBB) = MORT
     IURATE(NUMBB) = MORT
     IIRATE(NUMBB) = MORT

     ! Check whether the Type 2 beetle races are to remain potentially active after the
     ! current growth cycle.

     888 continue

     if (ONLY1X .eq.1) NOCOPY = 1

     999 continue
  end do

  ! If the Type 2 beetle races are to remain active, then copy them to the next growth
  ! cycle.

  if (NOCOPY .eq.1) goto 1000
  call OPCOPY (MYACT(1),IY(ICYC),IY(ICYC+1),NCOPYS,KODE)
  call OPINCR (IY,ICYC,NCYC)

  1000 continue

  return
end
