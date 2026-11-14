!===================!
!   wwpb : BMFCOM   !
!===================!
! WWPB $Id$

!  **BMFCOM:  Date of last revision:  June 10, 1994

!  Purpose:
!     Common block variables related to the fire model

!  Common block variables:
!     ATEMP:  Air temperature
!     DEPTH:
!     FWG:    Surface loading, lb.sqft fuel type
!     FLAG:   Error flag: 1=dead fuel too moist, 2=wind speed exceeds reliable
!             extrapolation, 3=gross surf/vol too small.
!     FWIND:  Dominant wind speed
!     HABTYP: Stand habitat type
!     ISPH:   Main tree species in host and non-host types.
!     MEXT:   Moisture of extinction of dead fuel
!     MOIS:   Moisture content of fuel types
!     MPS:    Mean surf/vol per ft of fuel type
!     ND:     Number of dead fuel size classes to be condsidered
!     NL:     Same as ND but for live fuel
!     SLP:    Slope of the stand (< 1.0)

! Variables
integer :: ATEMP
real    :: DEPTH
real    :: FWG(2,7)
integer :: FLAG(3)
real    :: FWIND
integer :: HABTYP(MXSTND)
integer :: ISPH(MXSTND,2)
real    :: MEXT(3)
real    :: MOIS(2,3)
integer :: MPS(2,3)
integer :: ND,NL
real    :: SLP(MXSTND)

common/BMFCOM/ ATEMP, DEPTH, FWG, FLAG, FWIND, ISPH,&
     &HABTYP, MEXT, MOIS, MPS,&
     &ND, NL, SLP

!.... End segment.
