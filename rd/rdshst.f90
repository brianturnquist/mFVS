!=================!
!   rd : rdshst   !
!=================!
! RD $Id$
subroutine RDSHST
  implicit none

  !  SUBROUTINE FOR UPDATING THE STUMP HISTORY ARRAYS

  !  CALLED BY :
  !     RDMN2   [ROOT DISEASE]

  !  CALLS     :
  !     NONE

  !  Revision History :
  !   08/14/12 - Last revision date.
  !   09/03/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'
  include 'RDCOM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'

  ! Variables
  integer :: I,J,K,KK

  do I = 1,IRRTRE
     do J = 1,4
        K = 4 - J + 1
        KK = K - 1
        if (J .eq.4) goto 503
        ROOTH(K,I) = ROOTH(KK,I)
        XMTH(K,I) = XMTH(KK,I)
        goto 502

        503 continue
        if (I .gt. ITRN) goto 504
        ROOTH(1,I) = RROOTT(I)
        XMTH(1,I) = WK22(I)
        goto 505

        504 continue
        ROOTH(1,I) = -1.0
        XMTH(1,I) = -1.0

        505 continue
        RROOTT(I) = 0.0
        WK22(I) = 0.0
        502 continue
     end do
     501 continue
  end do

  return
end
