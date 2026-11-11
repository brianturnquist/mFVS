!============================================!
!   Compute the basal area growth modifier   !
!============================================!
! ACD $Id$
subroutine BALMOD(ISPC,D,GMOD)
  implicit none

  !  THIS SUBROUTINE COMPUTES THE VALUE OF A GROWTH MODIFIER BASED
  !  ON BAL. ORIGINALLY THIS WAS JUST PART OF THE LARGE TREE DIAMETER
  !  GROWTH SEQUENCE. HOWEVER, THERE NEEDS TO BE A SIMILAR ACCOUNTING
  !  OF STAND POSITION IN THE LARGE TREE AND SMALL TREE HEIGHT GROWTH
  !  ESTIMATION SEQUENCE. THIS ROUTINE IS CALLED BY DGF, HTGF, AND
  !  RGNTHW.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'TWIGCOM.F77'

  ! Variables
  real    :: B3(MAXSP)
  real    :: GMOD,D,BAL
  integer :: ISPC,ICLS

  data B3/ &
       & 0.012785, 0.018831, 0.013427,4*.011942, 0.017300, 0.015496, &
       & 0.017300, 0.016835,4*.012329,2*.009149,8*.016835, 0.016191, &
       & 3*.016240,3*.019046,2*.023978,5*.015963, 0.013029,5*.015004, &
       & 3*.019904,5*.016877, 0.016537,5*.014235,4*.018560,3*.013762, &
       & 2*.018024,2*.020843,27*.020653,11*.011620/

  GMOD = 1.
  ICLS = IFIX(D + 1.0)

  !  THIS NEXT LINE IS TO GET SOME COMPETITION FROM NEIGHBORING
  !  TREES THAT ARE ABOUT THE SAME SIZE.

  ICLS = ICLS-2

  if (ICLS .lt. 1)ICLS = 1
  if (ICLS .gt. 50)ICLS = 50
  BAL = BAU(ICLS)
  if (BAL .le. 0.)go to 10
  GMOD = EXP(-(B3(ISPC)*BAL))
  if (GMOD.lt.0.5)GMOD = 0.5
  10 continue
  return
end subroutine BALMOD
