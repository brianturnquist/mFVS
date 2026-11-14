!=================================!
!   archive/PPEcommons : PPSPRD   !
!=================================!
! COMMON $Id$

! Variables
character(len=26) :: CRDID(MXSTND)
common / PPSPCC / CRDID
integer :: IRDORD(MXSTND),NRDS
real    :: RDDIST(MXSTND)
common / PPSPCM / RDDIST,IRDORD,NRDS

!     CRDID = STAND ID CODE FOR THE ROAD DISTANCE DATA.
!     RDDIST= RDDIST OF EACH STAND, -99999. IF NO DATA FOR RDDIST
!     IRDORD= ACCESS KEY TO RDDIST.
!     NRDS  = ACTUAL NUMBER OF RDDIST RECORDS.
