!=============================!
!   canada/newmist : dmopts   !
!=============================!
! CANADA-NEWMIST $Id$
subroutine DMOPTS
  implicit none

  ! **DMOPTS -- NISI  Date of last revision: April 10 1994

  ! Purpose:
  !  Users are provided three date-sensitive options in the model:
  ! DMCLMP, DMALPH and DMBETA. This routine checks to see if any of
  ! those keywords has been scheduled for this cycle, and performs
  ! necessary calculations if required. The calculation of the 'SF()'
  ! array mirrors the calculation performed in DMINIT.

  ! Called by:

  !     DMTREG

  ! Other routines called:

  !     OPFIND
  !     OPGET
  !     OPDONE

  ! Argument list definitions:

  !     [none]

  ! Local variable definitions:

  !     INTEGER   i         Loop counter for various things: options,
  !                          trees, plots and DM categories.
  !     INTEGER   j         Loop counter for sampling rings.
  !     INTEGER   n         Counter for actual number of sample points
  !                          encountered.
  !     INTEGER   p         Sample plot number.
  !     INTEGER   NTODO     The number of activities scheduled for the
  !                          option in the current cycle.
  !     INTEGER   IDATE     Dummy value containing the date for which
  !                          the OP call is scheduled.
  !     INTEGER   IACTK     Dummy value holding the completion status
  !                          of the OP call.
  !     INTEGER   NPAR      Dummy value containing the number of
  !                          parameters returned from the OP call.
  !     REAL      Parm      The array in which option parameters are
  !                          returned.
  !     REAL      Mean      The mean density of trees.
  !     REAL      Var       The variance of the density, computed
  !                          either by the Dispersion index or by
  !                          the variance of the sample points.
  !     REAL      x         Scalar used in calculation of sample ring
  !                          modified density; also used to hold
  !                          trees/acre density of individual records.
  !     REAL      tmp       Temporary calculation for sample ring
  !                          density scaling.
  !     REAL      PltDns   Dnsty (trees/acre) for each sample point.

  ! Common block variables and parameters:

  !     MXTHRX    DMCOM
  !     MAXPLT    PRGPRM
  !     IY        CONTRL
  !     ICYC      CONTRL
  !     DMALPH    DMCOM
  !     DMBETA    DMCOM
  !     SF        DMCOM
  !     IPTINV    PLOT
  !     ITRE      ARRAYS
  !     PROB      ARRAYS
  !     PI        PLOT

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'DMCOM.f90'

  ! Local variables.

  ! Variables
  integer :: i,n,p
  integer :: NTODO,IDATE,IACTK,NPAR
  integer :: MYACTS(2)
  real    :: Parm(2)
  real    :: Mean,Var,x
  real    :: PltDns(MAXPLT)

  data MYACTS /2008,2009/

  ! See if options have been specified for DMALPH and/or DMBETA. A
  ! Parm() value of -999 means "don't change the existing value".

  NTODO = 0
  call OPFIND(1, MYACTS(2), NTODO)
  if (NTODO .ne.0) then
     do I = 1,NTODO
        call OPGET(I, 2, IDATE, IACTK, NPAR, Parm)
        call OPDONE(I,IY(ICYC))
        if (Parm(1) .ne. -999.0) DMALPH = Parm(1)
        if (Parm(2) .ne. -999.0) DMBETA = Parm(2)
        8 continue
     end do
     call DMNAC(DMALPH,DMBETA)
  end if

  ! Determine if DMCLMP has been specified. If not, the stems/acre
  ! dispersion among plots is used. Plots are not assumed to be in any
  ! order, even though I suspect that they probably are.

  NTODO = 0
  call OPFIND(1, MYACTS(1), NTODO)
  if (NTODO .ne.0) then
     do I = 1, NTODO
        call OPGET(I, 1, IDATE, IACTK, NPAR, Parm)
        call OPDONE(I, IY(ICYC))
        DMCLMP = Parm(1)
        19 continue
     end do

  else

     if (IPTINV .gt.1) then

        do i = 1, MAXPLT
           PltDns(i) = 0.0
           20 continue
        end do

        Mean = 0.0
        do i = 1, ITRN
           p = ITRE(i)
           x = PROB(i)
           PltDns(p) = PltDns(p) + x
           Mean = Mean + x
           21 continue
        end do
        Mean = Mean / PI

        Var = 0.0
        n = 0
        do i = 1, MAXPLT
           if (PltDns(i) .gt.0.0) then
              Var = Var + (PltDns(i) - Mean)**2
              n = n + 1
           end if
           22 continue
        end do

        if (n .gt.1) then
           Var = Var / (FLOAT(n) - 1.0)
           DMCLMP = Var / Mean
        end if

     end if

  end if

  return
end
