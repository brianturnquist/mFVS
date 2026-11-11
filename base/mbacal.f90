!=============================================!
!   Identify the site species by basal area   !
!=============================================!
! BASE $Id$
subroutine MBACAL
  implicit none

  !   THIS SUBROUTINE IDENTIFIES THE SITE SPECIES (IE SPECIES OF
  !   MAXIMUM BASAL AREA IN THE STAND. THIS ROUTINE IS CALLED FROM
  !   CRATET AND DENSE.

  ! Common blocks
  include 'PRGPRM.F77'
  include 'ARRAYS.F77'
  include 'COEFFS.F77'
  include 'CONTRL.F77'
  include 'PLOT.F77'
  include 'HTCAL.F77'
  include 'PDEN.F77'
  include 'ESHAP.F77'

  ! Variables
  integer :: ISPC,II,I,MDX
  real    :: P,XMAX

  !     FIND SITE SPECIES. IF SITE SPECIES HAS BEEN ENTERED ON KEYWORD
  !     THEN BYPASS THE CALCULATION BASED ON THE TREELIST.

  if (LSITE) return
  if (ITRN .le. 0 .or. ISISP .gt. 0) return
  do ISPC = 1,MAXSP
     if (ISCT(ISPC,1).eq.0) goto 20
     do II = ISCT(ISPC,1),ISCT(ISPC,2)
        I = IND1(II)

        !     SKIP DEAD TREES.

        if (I.ge.IREC2) goto 10
        P = PROB(I)
        BARANK(ISPC) = .005454154*P*DBH(I)*DBH(I)+BARANK(ISPC)
        10 continue
     end do
     20 continue
  end do
  MDX = 0
  XMAX = 0.
  do I = 1,MAXSP
     if (XMAX .gt. BARANK(I)) goto 30
     MDX = I
     XMAX = BARANK(I)
     30 continue
  end do
  ISISP = MDX
  return
end subroutine MBACAL
