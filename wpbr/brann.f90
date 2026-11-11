!==================!
!   wpbr : brann   !
!==================!
! WPBR $Id$
subroutine BRANN(SEL)
  implicit none

  !  **BRANN        DATE OF LAST REVISION:  06/05/2014

  !  Purpose:

  !  This random number generator was modfied from the algorithm
  !  found in the IMSL Library Vol. 1 Edition 4 to 5,
  !  December 1, 1975, and can be found in the following references:

  !     Lewis, P.A.W., Goodman, A.S., and Miller, J.M. Psuedo-Random
  !     Number Generator for the System/360, IBM Systems Juornal, No. 2,
  !     1969.

  !     Learmouth, G.P. and Lewis, P.A.W., Naval Postgraduate School
  !     Random Number Generator Package LLRANDOM, NPS55LW73061A, Naval
  !     Postgraduate School, Monterey, California, June, 1973.

  !     Learmouth, G.P., and Lewis, P.A.W., Statistical Tests of Some
  !     Widely Used and Recently Proposed Uniform Random Number
  !     Generators. NPS55LW73111A, Naval Postgraduate School, Monterey
  !     California, November, 1973.

  !  The code was written by N.L. Crookston, Forestry Sciences Lab,
  !  October 1982, from the algorithm published in the IMSL manual and
  !  tested by comparing the first 10000 draws from this version and
  !  and the generator 'GGUBS' in the 1981 edition of IMSL.

  !  Revision History:

  !  dd-MMM-YYYY programmer_name
  !     description of change or update.
  !  01-OCT-1982
  !     Original source date noted.
  !  08-MAY-2006 Lance R. David (FHTET)
  !     Changed random number seed variable names to unique variables
  !     BRS0, BRS1, BRSS.

  !.... Common include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'BRCOM.f90'

  !.... Local variable declarations.

  ! Variables
  real    :: SEED,SEL
  logical :: LSET

  BRS1 = DMOD(16807D0*BRS0,2147483647D0)
  SEL = BRS1/2147483648D0
  BRS0 = BRS1
  go to 100

  !.... You may reseed the generator by supplying an odd number of type
  !.... real with LSET=TRUE; if LSET=FALSE, a call to RANSED will
  !.... cause the random number generator to start over.

  entry BRNSED(LSET,SEED)
  if (.not.LSET) then
     SEED = BRSS
     BRS0 = SEED
     go to 100
  end if

  if (AMOD(SEED,2.0).eq.0.) SEED = SEED+1
  BRSS = SEED
  BRS0 = SEED

  !.... Common return.

  100 continue
  return
end
