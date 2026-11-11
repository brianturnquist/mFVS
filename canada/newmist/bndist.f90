!=============================!
!   canada/newmist : bndist   !
!=============================!
! CANADA-NEWMIST $Id$
subroutine BNDIST(M, V, err, UBound, PDF, end)
  implicit none

  ! **BNDIST -- NISI  Date of last revision: April 15 1994

  ! Purpose:
  !   Returns the probability distribution associated with a population
  ! of a given mean and variance. The distributions are taken from the
  ! Binomial/Poisson/Negative Binomial family. If the variance (nearly)
  ! equals the mean, the Poisson is used. If V < M the Binomial is used
  ! and if V > M the Negative Binomial. The traditional notation is
  ! given in the comments sections below. The coding could be enhanced
  ! to give better error reporting; as it is, the error code is not
  ! used. This routine is **NOT** able or intended to simulate negative
  ! sample sizes or continuous distributions.

  ! Called by:

  !     DMNB

  ! Other routines called:

  !     GAMMLN [part of this routine's file]

  ! Argument list definitions:

  !     REAL    M       (I) The mean of the distribution.
  !     REAL    V       (I) The variance of the distribution.
  !     LOGICAL err     (O) Flag indicating if any anomalous conditions
  !                          occurred (.TRUE.); .FALSE. otherwise.
  !     INTEGER UBound  (I) The length of the array 'PDF()'.
  !     REAL    PDF     (O) The array containing the probability
  !                          distribution.
  !     INTEGER End     (O) The position of the largest non-zero
  !                          probability, ie: the last value of the
  !                          upper tail.

  ! Local variable definitions:

  !     INTEGER method      The kind of distribution: 1= Poisson;
  !                          2= Negative Binomial; 3= Binomial.
  !     INTEGER jp          Offset counter to get around compiler bug.
  !     INTEGER j           Loop counter for the distribution.
  !     REAL    k           Computed constant; meaning depends on
  !                          distribution type.
  !     REAL    p           Like 'k'
  !     REAL    t1          Like 'k'.
  !     REAL    t2          Like 'k'.
  !     REAL    t3          Like 'k'.
  !     REAL    sum         Counter for the summation of the
  !                          distribution. This is used to insure that
  !                          the distribution does not exceed 1.0.
  !     REAL    plast       The probability associated with the
  !                          previous sample size.
  !     REAL    pnow        The probabilitiy of the current sample
  !                          size.
  !     REAL    z           The logarithm of the probability of the
  !                          current sample size. This is the value
  !                          that is actually computed, *then*
  !                          converted back to a {0,1} number.
  !     REAL    tol         The value used to discriminate between
  !                          distribution types; whether the mean and
  !                          variance are valid at all; and whether
  !                          the upper tail of the distribution has
  !                          been reached.

  ! Common block variables and parameters:

  !     [none]

  ! Subroutine arguments.

  ! Variables
  real    :: M
  real    :: V
  logical :: err
  integer :: UBound
  real    :: PDF(*)
  integer :: end

  ! Local variables.

  integer :: method,jp,j
  real    :: k,p,t1,t2,t3,sum,plast,pnow,z,GAMMLN,X
  real    :: tol = 1.0e-6

  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>> READ THIS <<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ! ERROR IN VARIABLE LENGTH ARRAYS: Note that 'jp' is required       <
  ! because the variable-length array PDF(*) is actually              <
  ! PDF(0:UBound) in the calling routine. An error in MS PowerStation <
  ! 1.0 prevents this from being recognized properly. Therefore, the  <
  ! 'jp' index bumps things up one. The actual 'j' location will be   <
  ! correct upon return.                                              <
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> +++++++++ <<<<<<<<<<<<<<<<<<<<<<<<

  ! Zero the distribution.

  do j = 0, UBound
     jp = j+1
     PDF(jp) = 0.
     end = 1
     10 continue
  end do

  ! If mean or variance is very small, RETURN.

  err = .false.
  if ((M .lt. tol) .or. (V .lt. tol)) then
     err = .true.
     return
  end if

  ! Choose the method and compute constant terms.

  ! Poisson.

  if (ABS(V - M) .lt. tol) then

     method = 1

     t1 = -(M)
     t2 = LOG(M)

     ! Negative Binomial.

  else if (V .gt. M) then

     method = 2

     p = (V / M) - 1.0
     k = M / p

     ! kpq = variance
     ! kp  = mean
     ! q   = kpq/kp
     ! q-p = 1
     ! p   = q-1

     t1 = -k * LOG(1. + (M / k))
     t2 = GAMMLN(k)
     t3 = LOG(M / (M + k))

     ! Binomial.

  else

     method = 3

     p = 1. - (V / M)
     k = M / p

     ! kpq = variance
     ! kp  = mean
     ! q+p = 1
     ! p   = 1-q

     t1 = GAMMLN(k + 1.0)
     t2 = LOG(p)
     t3 = LOG(1.0 - p)

  end if

  sum = 0.
  do j = 0, UBound

     ! The 'jp' indexing circumvents the 0:UBound error in variable length
     ! arrays.

     jp = j + 1

     x = FLOAT(j)

     ! The order here is Poisson (100), Negative Binomial (200),
     ! Binomial (300).

     goto (100,200,300), method

     ! Poisson if mean is nearly equal to the variance. The 'nearly'
     ! covers a lot of ground, since some of the limiting cases of the B()
     ! and NB() arise when the variance/mean ratio is 1. The chosen
     ! distribution is P() near these limits.

     !     z= -(M) + (x * LOG(M)) - GAMMLN(x + 1.)

     100 continue
     z = t1 + (x * t2) - GAMMLN(x + 1.)
     goto 400

     ! Negative binomial if variance > mean. Note that many terms have
     ! been precomputed in the setup call. The old form of the equation
     ! is:

     !     z = -k * LOG(1. + (M / k))
     !    >    + GAMMLN(k + x) - GAMMLN(x + 1.)  - GAMMLN(k)
     !    >    + x * LOG(M / (M + k))

     200 continue
     z = t1 + GAMMLN(k + x) - GAMMLN(x + 1.) - t2 + (x * t3)
     goto 400

     ! Binomial if if variance < mean. Note precomputed terms. The old
     ! form of the equation is:

     !     z = GAMMLN(k + 1.) - GAMMLN(x + 1.) - GAMMLN(k - x + 1.)
     !    >    + (x * LOG(p)) + ((k - x) * LOG(1. - p))

     300 continue

     if ((k - x + 1.0) .lt.0.0) then
        z = -99.
     else
        z = t1 - GAMMLN(x + 1.) - GAMMLN(k - x + 1.)&
             &+ (x * t2) + ((k - x) * t3)
     end if

     400 continue

     ! Convert logarithms back to real values. If 'v' is less than -75,
     ! the result is nearly zero (in single precision), so no assignment
     ! is made: PDF(j) is 0.0 prior to assignment. The 'jp' indexing gets
     ! around the 0:UBound problem described above.

     if (z .gt. -75.) PDF(jp) = EXP(z)

     sum = sum + PDF(jp)

     ! Set 'plast' for the n=0 case

     if (j .eq.0) then

        plast = PDF(jp)

     else

        ! Stop computing if a) the value is less than 1e-6 on the descending
        ! limb; or b) the sum has passed 1.0 (this happens for pathological
        ! binomial cases, and some adjustment is needed... even then it is
        ! biased); or c) the end of the PDF array has been reached.

        pnow = PDF(jp)

        if ((pnow .lt. plast) .and. (pnow .lt. tol)) then
           end = j
           goto 1001
        end if

        if (sum .ge.1.0) then
           end = j
           PDF(jp) = PDF(jp) - (sum - 1.0)
           ! SHOULD CALL ERROR HANDLER TO SIGNAL UNRELIABLE RESULTS
           goto 1001
        end if

        if (j .eq. UBound) then
           end = j
           ! SHOULD CALL ERROR HANDLER TO SIGNAL UNRELIABLE RESULTS
           goto 1001
        end if

        plast = pnow

     end if

     1000 continue
  end do
  1001 continue

  return
end

function GAMMLN(Arg)

  ! **GAMMLN -- NISI  Date of last revision: April 15 1994

  ! Purpose:
  !   Compute the natural logarithm of the gamma function for the
  ! argument. Taken from Numerical Recipes in C.

  ! Called by:

  !     BNDIST

  ! Other routines called:

  !     [none]

  ! Argument list definitions:

  !     REAL    Arg (I) The argument to the gamma function.

  ! Local variable definitions:

  !     INTEGER j       Loop counter.
  !     REAL    SqPI    Square root of PI.
  !     REAL    x       Temporary place holder.
  !     REAL    tmp     Temporary place holder.
  !     REAL    ser     Series to be accumulated.
  !     REAL    cof     Coefficients of interpolation function.

  ! Common block variables and parameters:

  !     [none]

  ! Function arguments.

  ! Variables
  real    :: Arg

  ! Local variables

  integer :: j
  real    :: SqPI,x,tmp,ser
  real    :: cof(6)

  data cof / 76.18009173,&
       &-86.50532033,&
       &24.01409822,&
       &-1.231739516,&
       &0.12085003e-2,&
       &-0.536382e-5 /

  data SqPI / 2.50662827465 /

  x = Arg - 1.0
  tmp = x + 5.5
  tmp = tmp - (x + 0.5) * LOG(tmp)

  ser = 1.0
  do j = 1, 6
     x = x + 1.0
     ser = ser + cof(j) / x
     10 continue
  end do

  GAMMLN = (-tmp + LOG(SqPI * ser))

end
