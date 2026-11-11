!=====================!
!   common : ESTREE   !
!=====================!
! COMMON $Id$

! Variables
integer :: IESTAT(MAXTRE)

common /ESTREE/ IESTAT

!  VARIABLE DEFINITIONS:

!  IESTAT -- ARRAY HOLDING THE YEAR WHEN MORTALITY CAN BEGIN ON TREES
!            REGENERATED IN **ESTAB**; REGEN-SIZE TREES ARE ALSO FLAGGED
!            IN **ESFLTR**
