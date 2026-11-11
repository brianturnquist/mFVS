!=====================!
!   mistoe : misran   !
!=====================!
! MISTOE $Id$
subroutine MISRAN(IARRAY,ISIZE)

  !  Purpose:
  !     Randomly inserts integers from 1 to ISIZE into the first ISIZE
  !  slots of IARRAY (which has a maximum dimension of MAXTRE).

  !  Algorithm was taken from "Numerical Recipes: The Art of Scientific
  !  Computing"; Press, Flannery, Teukolsky, and Vetterling - a portable
  !  integer random number generator bewteen given bounds, pp.196-198.

  !  Call list definitions:
  !     IARRAY: (IO) Array containing random integers between 1 and ISIZE.
  !     ISIZE:  (I)  Range of integers to randomize.

  !  Local variable definitions:
  !     DEBUG:  Logical flag to turn debug on or off.
  !     I:      Loop counter.
  !     IA:     The multiplier (constant for this random # generator).
  !     IC:     The increment (constant for this random # generator).
  !     IM:     The modulus (constant for this random # generator).
  !     IRAN:   Integer representation of random number.
  !     JRAN:   Initialized to a seed value between 0 and IM-1; used to
  !                seed the equation each time through the loop.
  !     TARRAY: Temporary array to keep track of integers already used.

  !  Common block variables and parameters:
  !     ICYC:   From CONTRL; cycle index number.
  !     JOSTND: From CONTRL; unit number of stand output.
  !     MAXTRE: From PRGPRM; maximum number tree records.

  implicit none

  ! Variables
  integer :: IM,IA,IC

  !.... Parameter statements.

  !.... These values listed in the above-mentioned documentation as
  !.... "good" choices for constants to obtain pseudo-randomness and
  !.... to avoid machine overflow.

  parameter (IM = 233280)
  parameter (IA = 1861)
  parameter (IC = 49297)

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'

  !.... Common include files.

  include 'CONTRL.f90'
  include 'MISCOM.f90'

  !.... Variable declarations.

  logical :: DEBUG
  integer :: IARRAY(MAXTRE),TARRAY(MAXTRE)
  integer :: I,IRAN,ISIZE

  !.... Check for debug.

  call DBCHK(DEBUG,'MISRAN',6,ICYC)

  if (DEBUG) write(JOSTND,100) ICYC, JRAN
  100 format(' Begin MISRAN: Cycle = ',I5,'; Jran = ',I14)

  !.... Initializations.

  do I = 1,ISIZE
     TARRAY(I) = 0
     110 continue
  end do

  !.... Check on ISIZE boundaries.

  if (ISIZE.gt.MAXTRE.or.ISIZE.lt.1) then
     print*,' *MISRAN* Error. Invalid ISIZE: ',ISIZE
     go to 9000
  end if

  !.... Top of random number loop.

  do I = 1,ISIZE

     !....    Generate a random integer.

     200 continue
     JRAN = MOD(JRAN*IA+IC,IM)
     IRAN = INT(1+(ISIZE*JRAN/IM))

     !....    Make sure IRAN is between 1 and ISIZE (inclusive).

     if (IRAN.gt.ISIZE.or.IRAN.lt.1) go to 200

     !....    TARRAY is a temporary storage array to keep track of what
     !....    integers have already been taken (can't use the same one
     !....    twice). Loop until an opening in TARRAY is found.

     250 continue
     if (TARRAY(IRAN).eq.1) then
        IRAN = IRAN+1
        if (IRAN.gt.ISIZE) IRAN = 1
        go to 250
     end if

     !....    An opening was found. Mark TARRAY slot as used and deposit
     !....    random integer into IARRAY.

     TARRAY(IRAN) = 1
     IARRAY(I) = IRAN
     300 continue
  end do

  !.... Common return.

  9000 continue

  if (DEBUG) write(JOSTND,9010) ICYC
  9010 format(' End MISRAN: Cycle = ',I5)

  return
end
