!===========================================!
!   Regeneration establishment parameters   !
!===========================================!
! ACD-COMMON $Id$

! Model dimensions
integer :: NDBHCL,NSPSPE
parameter (NDBHCL=10,NSPSPE=81)

! NDBHCL -- Number of DBH classes for stump sprouts. Change DBHMID
!           in BLKDAT to correspond to the correct number.
! NSPSPE -- Number of sprouting species. Change ISPSPE in BLKDAT
!           to correspond to the correct number.
!
! NDBHCL and NSPSPE define an imaginary array with cells that
! are numbered consecutively. The number in the grid indexes
! species and diameter class. The product of NDBHCL*NSPSPE cannot
! exceed 999 because any larger number would cause ISHOOT(I) to
! become larger than a 10-digit number.
