!===================================!
!   Realign species order indices   !
!===================================!
! BASE $Id$
subroutine SPESRT
  implicit none

  !     REALIGNS THE SPECIES-ORDER SORT AFTER THE TREE LIST HAS
  !     BEEN COMPRESSED AND/OR TREES HAVE BEEN ADDED VIA THE
  !     REGENERATION ESTABLISHMENT SUBMODEL.

  ! Common blocks
  include 'PRGPRM.F77'
  include 'CONTRL.F77'
  include 'ARRAYS.F77'

  ! Variables
  integer :: I

  !     INITIALIZE VARIABLES

  NUMSP = 0
  do I = 1,MAXSP
     ISCT(I,1) = 0
     ISCT(I,2) = 0
     IBEGIN(I) = 0
     IREF(I) = 0
     KOUNT(I) = 0
  end do
  if (IREC1.eq.0) then
     ITRN = 0
  else

     !        SET UP INDEX POINTERS FOR CHAIN SORTING.

     !        ESTABLISH A LINK IN THE CHAIN SORT FOR ALL LIVE TREES.

     do I = 1,IREC1
        call LNKCHN (I)
     end do

     !        SET UP THE POINTERS TO THE SPECIES-ORDER SORT; IND1 AND ISCT.

     call SETUP
  end if

  return
end subroutine SPESRT
