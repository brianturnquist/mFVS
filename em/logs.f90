!===============================!
!   Compute board foot volume   !
!===============================!
! EM $Id$
subroutine LOGS(DBH,HT,IBCD,BDMIN,ISP,STMP,BV)
  implicit none

  !  REGION 5 BOARD FOOT VOLUME MODELS.
  !  BY K.STUMPF, ADAPTED BY B.KRUMLAND, P.J.DAUGHERTY
  !  NOT FOR USE IN REGION 1, SO JUST SET BV TO ZERO AND RETURN

  ! Variables
  real    :: DBH,HT,BDMIN,STMP,BV
  integer :: IBCD,ISP
  real    :: RDANUW
  integer :: IDANUW

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = BDMIN
  RDANUW = DBH
  RDANUW = HT
  IDANUW = IBCD
  IDANUW = ISP
  RDANUW = STMP

  BV = 0.
  return
end
