!===================!
!   wpbr : bribes   !
!===================!
! WPBR $Id$
subroutine BRIBES(REDFAC,LREDF)
  implicit none

  !  Purpose:
  !  BRIBES calculates the reduction factor to be applied to the each
  !  tree's RI as a result of the change in ribes populations, if both
  !  old and new population values are present.  If only new population
  !  values are present or the cycle is 0, a new stand rust index (RIDEF)
  !  is calculated.

  !  RIBUS(1,x) old number of ribes bushes/acre
  !  RIBUS(2,x) new number of ribes bushes/acre

  !  Revision History:

  !  13-DEC-2000 Lance R. David (FHTET)
  !     Added a divisor in calculation of rust index (see not at equation).

  !.... Common include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'BRCOM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'

  !.... Local variable declarations.

  ! Variables
  integer :: IBUSH,ITYP
  real    :: NOLD,REDFAC,BRCON
  logical :: DEBUG,LREDF

  !.... See if we need to do some debug.

  call DBCHK(DEBUG,'BRIBES',6,ICYC)
  if (DEBUG) write(JOSTND,10) ICYC
  10 format('Entering subroutine BRIBES: cycle = ',I2)

  !.... Initializations

  REDFAC = 0.0
  LREDF = .false.

  !.... Sum RI values for old and new number of bushes per acre.

  do IBUSH = 1,2
     RIBSUM(IBUSH) = 0.0

     !....    Cycle the 3 ribes species through the 'baseline' function
     !....    for estimating rust index from the ribes species.

     do ITYP = 1,3
        BRCON = RSF(ITYP)
        FACTOR(ITYP) = (BRCON*&
             &(0.499675+(0.4*ATAN((RIBUS(IBUSH,ITYP)/150)-3))))/0.2652
        RIBSUM(IBUSH) = RIBSUM(IBUSH)+FACTOR(ITYP)

        !           The divisor 0.2652 was added to the above equation based
        !           on testing performed by Geral McDonald and John Schwandt
        !           that showed the function generated results too high by a
        !           factor of 3.78. 13-DEC-2000 Lance R. David

        30 continue
     end do
     40 continue
  end do

  !.... Calculate reduction factor or set new stand rust index.

  NOLD = RIBUS(1,1)+RIBUS(1,2)+RIBUS(1,3)

  if (NOLD.eq.0.0.or.ICYC.eq.0) then
     RIDEF = RIBSUM(2)
  else
     REDFAC = RIBSUM(2)/RIBSUM(1)
     LREDF = .true.
  end if

  !.... Common return.

  if (DEBUG) write(JOSTND,201) ICYC
  201 format ('Leaving subroutine BRIBES: cycle = ',I2)
  return
end
