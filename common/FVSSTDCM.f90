!=======================!
!   common : FVSSTDCM   !
!=======================!
! COMMON $Id$

! Variables
logical :: FSTOPEN

integer :: KOLIST

common /FVSSTDCM/ FSTOPEN,KOLIST

!  VARIABLE DEFINITIONS:

!    FSTOPEN =
!     KOLIST = UNIT NUMBER WHERE THE TREE DATA WILL BE WRITTEN
!              FOR INPUT TO THE FVSSTAND POST-PROCESSOR.
!              DEFAULT = 27; SET IN **BLKDAT**
