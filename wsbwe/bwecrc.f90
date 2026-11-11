!====================!
!   wsbwe : bwecrc   !
!====================!
! WSBWE $Id$
subroutine BWECRC (XHT,IHTC,ICRC1,ICRC2)
  implicit none

  !     SELECT THE HEIGHT CLASS AND CROWN CLASS INDICIES.

  !     PART OF THE WESTERN SPRUCE BUDWORM MODEL/PROGNOSIS LINKAGE CODE.
  !     N.L. CROOKSTON--FORESTRY SCIENCES LAB, MOSCOW, ID--JUNE 1983

  !     CALLED FROM :

  !       BWESIT - CREATE A BUDWORM STAND FROM A PROGNOSIS STAND.
  !       BWEDAM - COMPUTE PERIODIC DAMAGE BY BUDWORM TO PROGNOSIS
  !                SAMPLE TREES.
  !       BWEPRB - ADJUST PROPORTION OF RETAINED BIOMASS ARRAY.

  !     PARAMETERS :

  !     XHT   = TREE HEIGHT IN FEET.
  !     IHTC  = HEIGHT CLASS NUMBER, WHERE 1= 0 TO 7 METERS,
  !             2= 7 TO 14 METERS, AND 3= 14 PLUS METERS.
  !     ICRC1 = THE INDEX POINTING TO THE FIRST CROWN CLASS FOR THE TREE.
  !     ICRC2 = THE INDEX POINTING TO THE LAST CROWN CLASS FOR THE TREE.

  !  Revision History:
  !    14-JUL-2010 Lance R. David (FMSC)
  !       Added IMPLICIT NONE and declared variables as needed.

  ! Variables
  integer :: ICRC1,ICRC2,IHTC
  real    :: XHT

  IHTC = 1
  if (XHT.lt.23.) goto 10
  IHTC = 2
  if (XHT.lt.46.) goto 10
  IHTC = 3
  10 continue
  ICRC2 = IHTC*3
  ICRC1 = ICRC2-2
  return
end
