!===================!
!   wwpb : bmcbet   !
!===================!
! WWPB $Id$
subroutine BMCBET(ABETA, MINSIZE, MAXSIZE, BETA)

  !     CALLED BY: BMISTD
  !     CALLS: FUNCTIONS ALNGAM AND BETAIN

  !        BMCBET  Date of last revision APRIL 22, 1998, By Shaoang
  !       REVISED: 9/8/99 ajm.  Made A a function of BKP/Acre.

  !     PURPOSE:
  !    A SUBROUTINE TO CALCULATE PROBABILITY WITHIN EACH INTERVAL FOR A BETA
  !        RANDOM VARIATE (IN THIS CASE, THE RANDOM VARIATE IS dbh). IT IS ADOCPTED
  !        FROM ALGORITHM AS226 APPL. ATATIST. (1987) VOL. 36, NO. 2

  !     CALL LIST DEFINITIONS:
  !    MINSIZE: THE MINIMUM SIZE CLASS THE BEETLE CAN ATTACK
  !       MAXSIZE: THE MAXIMUM SIZE CLASS THAT BEETLE CAN ATTACK
  !    BETA: ARRAY CONTAINING THE BETA DISTRIBUTION VALUES FOR EACH SIZE CLASS

  !     LOCAL VARIABLE DEFINITIONS:
  !    A: PARAMETER ALPHA IN THE BETA DISTRIBUTION
  !    B: PARAMETER BETA IN THE BETA DISTRIBUTION
  !    IFAULT: FLAG VARIABLE
  !    X: SIZE CLASS FALLING IN THE RANGE OF MINSIZE AND MAXSIZE
  !    TEMP: TEMPORARY VARIABLES USED TO STORE CUMULATIVE BETA VALUES

  !     PARAMETER INCLUDE FILE

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PPEPRM.f90'
  include 'BMPRM.f90'

  !     COMMON INCLUDE FILES

  include 'CONTRL.f90'
  include 'BMCOM.f90'

  !     FUNCTION DEFINITIONS

  ! Variables
  real    :: ALNGAM,BETAIN

  !     ARG LIST

  integer :: MINSIZE
  integer :: MAXSIZE
  real    :: BETA

  dimension BETA(NSCL)

  !     VARIABLE DECLARATIONS

  integer :: I
  real    :: A,X,MINX,MAXX,LOGBETA,TEMP0,TEMP1

  !     Beta distribution parameters a and b is set to be 2.0 and 2.0 based on the
  !     experience and sample simulation for beta distribution with different beta
  !     parameters. For beta(x,2,2), probability increases as x increases. If it is
  !     desired to make the parameters change with the maximum size class (MAXSIZE)
  !     used in this subroutine, two functions: a=f(maxsize) and b=g(maxsize) can
  !     be put in here instead of using data statement to define their values

  !      DATA A,B /2.0,2.0/
  data B /2.0/

  A = ABETA

  !     Set A equal to ABETA (above), and declared A as REAL (above).
  !     ABETA is defined in subroutine BMISTD and is a function of BKP/A.

  data ZERO, HALF, ONE /0.0, 0.5, 1.0/

  if (LBMDEB) write(JBMBPR,10)
  10 format(' Begin BMCBET:')

  !     ZERO ARRAY (actually set to a small number so that all size classes
  !            could potentially be attacked)

  do I = 1, NSCL
     BETA(I) = 1E-5
     12 continue
  end do
  !...

  MINX = real(MINSIZE)-HALF
  MAXX = real(MAXSIZE)+HALF
  LOGBETA = ALNGAM(A,IFAULT) + ALNGAM(B, IFAULT) - ALNGAM(A+B,IFAULT)

  TEMP0 = ZERO
  do I = MINSIZE, MAXSIZE
     X = ((real(I)+HALF)-MINX)/(MAXX-MINX)

     if (X .le. ZERO) X = 0.001
     if (X .eq. ONE) X = ONE - 0.001

     if (X .lt. ZERO .or. X .gt. ONE) then
        IFAULT = 3
        return
     end if

     if (X .eq. ZERO .or. X .eq. ONE) then
        IFAULT = 0
        return
     end if

     TEMP1 = BETAIN(X,A,B,LOGBETA,IFAULT)
     BETA(I) = TEMP1 - TEMP0
     TEMP0 = TEMP1

     15 continue
  end do

  if (LBMDEB) write(JBMBPR,99) A, B
  99 format ('End BMCBET: A= ', F4.1, 'B= ', F4.1)

  return
end

  ! This file is copy from Statlib on the WWW with address
  ! http://lib.stat.cmu.edu/apstat/245. It is incoporated into BMCBET to calculate
  ! the weight to determine the probablity of beetle attacks by diameter classes.
  ! Adoption date: 4/22/98

  ! This file contains Algorithm AS 245 is gives an accuracy of about
  ! 10-12 significant decimal digits except for small regions around X = 1 and
  ! X = 2, where the function goes to zero.

  ! Variables
real function ALNGAM(XVALUE, IFAULT)

  !     ALGORITHM AS245  APPL. STATIST. (1989) VOL. 38, NO. 2

  !     Calculation of the logarithm of the gamma function

  integer :: IFAULT
  real    :: XVALUE
  real    :: ALR2PI,FOUR,HALF,ONE,ONEP5,R1(9),R2(9),R3(9),R4(5),TWELVE,X,X1,X2,XLGE,XLGST,Y,ZERO
  !***  DOUBLE PRECISION XVALUE

  !     Coefficients of rational functions

  data R1/-2.6668551E0, -2.4438753E1,&
       &-2.1969895E1,  1.1166754E1,&
       &3.1306054E0,  6.0777138E-1,&
       &1.1940090E1,  3.1469011E1,&
       &1.5234687E1/
  data R2/-7.8335929E1, -1.4204629E2,&
       &1.3751941E2,  7.8699492E1,&
       &4.1643892E0,  4.7066876E1,&
       &3.1339921E2,  2.6350507E2,&
       &4.3340002E1/
  data R3/-2.1215957E5,  2.3066151E5,&
       &2.7464764E4, -4.0262111E4,&
       &-2.2966072E3, -1.1632849E5,&
       &-1.4602593E5, -2.4235740E4,&
       &-5.7069100E2/
  data R4/ 2.7919531791E-1, 4.9173176105E-1,&
       &6.9291059929E-2, 3.3503438150E0,&
       &6.0124592597E0/

  !     Fixed constants

  data ALR2PI/9.1893853320E-1/, FOUR/4.E0/, HALF/0.5E0/,&
       &ONE/1.E0/, ONEP5/1.5E0/, TWELVE/12.E0/, ZERO/0.E0/

  !     Machine-dependant constants.
  !     A table of values is given at the top of page 399 of the paper.
  !     These values are for the IEEE double-precision format for which
  !     B = 2, t = 53 and U = 1023 in the notation of the paper.

  data XLGE/5.10E6/, XLGST/1.E+38/

  X = XVALUE
  ALNGAM = ZERO

  !     Test for valid function argument

  IFAULT = 2
  if (X .ge. XLGST) return
  IFAULT = 1
  if (X .le. ZERO) return
  IFAULT = 0

  !     Calculation for 0 < X < 0.5 and 0.5 <= X < 1.5 combined

  if (X .lt. ONEP5) then
     if (X .lt. HALF) then
        ALNGAM = -LOG(X)
        Y = X + ONE

        !     Test whether X < machine epsilon

        if (Y .eq. ONE) return
     else
        ALNGAM = ZERO
        Y = X
        X = (X - HALF) - HALF
     end if
     ALNGAM = ALNGAM + X * ((((R1(5)*Y + R1(4))*Y + R1(3))*Y&
          &+ R1(2))*Y + R1(1)) / ((((Y + R1(9))*Y + R1(8))*Y&
          &+ R1(7))*Y + R1(6))
     return
  end if

  !     Calculation for 1.5 <= X < 4.0

  if (X .lt. FOUR) then
     Y = (X - ONE) - ONE
     ALNGAM = Y * ((((R2(5)*X + R2(4))*X + R2(3))*X + R2(2))*X&
          &+ R2(1)) / ((((X + R2(9))*X + R2(8))*X + R2(7))*X&
          &+ R2(6))
     return
  end if

  !     Calculation for 4.0 <= X < 12.0

  if (X .lt. TWELVE) then
     ALNGAM = ((((R3(5)*X + R3(4))*X + R3(3))*X + R3(2))*X + R3(1)) /&
          &((((X + R3(9))*X + R3(8))*X + R3(7))*X + R3(6))
     return
  end if

  !     Calculation for X >= 12.0

  Y = LOG(X)
  ALNGAM = X * (Y - ONE) - HALF * Y + ALR2PI
  if (X .gt. XLGE) return
  X1 = ONE / X
  X2 = X1 * X1
  ALNGAM = ALNGAM + X1 * ((R4(3)*X2 + R4(2))*X2 + R4(1)) /&
       &((X2 + R4(5))*X2 + R4(4))
  return
end

  ! Variables
real function BETAIN(X, P, Q, BETA, IFAULT)
  implicit real (A-H, O-Z)

  !***      implicit double precision (a-h, o-z)

  !    This is an algorithm copied from Statlib on the internet. The web site
  !    for this algorithm is http://lib.stat.cmu.edu/apstat/63

  !     algorithm as 63  appl. statist. (1973), vol.22, no.3

  !     computes incomplete beta function ratio for arguments
  !     x between zero and one, p and q positive.
  !     log of complete beta function, beta, is assumed to be known

  logical :: INDX

  real    :: X,P,Q,BETA
  integer :: IFAULT

  !     define accuracy and initialise

  data ZERO/0.0E0/, ONE/1.0E0/, ACU/0.1E-14/
  BETAIN = X

  !     test for admissibility of arguments

  IFAULT = 1
  if (P.le.ZERO .or. Q.le.ZERO) return
  IFAULT = 2
  if (X.lt.ZERO .or. X.gt.ONE) return
  IFAULT = 0
  if (X.eq.ZERO .or. X.eq. ONE) return

  !     change tail if necessary and determine s

  PSQ = P+Q
  CX = ONE-X
  if (P.ge.PSQ*X) goto 1
  XX = CX
  CX = X
  PP = Q
  QQ = P
  INDX = .true.
  goto 2
  1 XX = X
  PP = P
  QQ = Q
  INDX = .false.
  2 TERM = ONE
  AI = ONE
  BETAIN = ONE
  NS = QQ+CX*PSQ

  !     user soper's reduction formulae.

  RX = XX/CX
  3 TEMP = QQ-AI
  if (NS.eq.0) RX = XX
  4 TERM = TERM*TEMP*RX/(PP+AI)
  BETAIN = BETAIN+TERM
  TEMP = ABS(TERM)
  if (TEMP.le.ACU .and. TEMP.le.ACU*BETAIN) goto 5
  AI = AI+ONE
  NS = NS-1
  if (NS.ge.0) goto 3
  TEMP = PSQ
  PSQ = PSQ+ONE
  goto 4

  !     calculate result

  5 BETAIN = BETAIN*EXP(PP*LOG(XX)+(QQ-ONE)*LOG(CX)-BETA)/PP
  if (INDX) BETAIN = ONE-BETAIN
  return
end
