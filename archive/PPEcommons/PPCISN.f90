!=================================!
!   archive/PPEcommons : PPCISN   !
!=================================!
! COMMON $Id$

! Variables
integer :: MXPRTD
parameter (MXPRTD = 3000)
character(len=11) :: CISNS(MXPRTD)
common /PPCISN/ CISNS

!     CISNS = INTERNAL STAND NUMBERS AS READ BY THE HISTORY FILE AND
!             USED BY SNDTBS AND THE ROUTINES IT CALLS.
