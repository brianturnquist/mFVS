!=====================!
!   common : KEYCOM   !
!=====================!
! COMMON $Id$

! Variables
character(len=8) :: TABLE

integer :: ISIZE
parameter (ISIZE = 150)

common / KEYCOM / TABLE(ISIZE)

!  VARIABLE DEFINITIONS:

!  TABLE -- ARRAY HOLDING VALID KEYWORD NAMES.
