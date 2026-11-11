!====================!
!   common : EMCOM   !
!====================!
! COMMON $Id$

! Variables
integer :: IEMTYP

common /EMCOM/ IEMTYP

!  VARIABLE DEFINITIONS:

!    IEMTYP -- INDEX TO VALID HABITAT TYPE CODES FOR THE EM
!              VARIANT.  SET IN **HABTYP, AND USED TO SET
!              MAX SDI (SDIDEF ARRAY) IN *SITSET
