!================================================!
!   Correct board foot volume for damaged tops   !
!================================================!
! BASE $Id$
subroutine BFTOPK(ISPC,D,H,BBFV,LCONE,BARK,VMAX,ITHT)
  implicit none

  !    THIS SUBROUTINE CORRECTS BOARD FOOT VOLUMES FOR DEAD/DAMAGED
  !    TOPS.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'COEFFS.f90'
  include 'VOLSTD.f90'

  ! Variables
  logical :: LCONE
  integer :: ITHT,ISPC
  real    :: VMAX,BARK,BBFV,H,D,HTRUNC,PHT,DTRUNC,HTMRCH,STUMP
  real    :: VOLTK,BEHRE,VOLM

  !  SET PARAMETERS FOR BEHRE HYPERBOLA.

  if (BBFV .le. 0.) go to 100
  call BEHPRM (VMAX,D,H,BARK,LCONE)

  !  COMPUTE TOTAL BOARD FOOT VOLUME LOSS DUE TO TOPKILL.
  !  HEIGHT AND DIAMETER AT THE POINT OF TRUNCATION ARE EXPRESSED
  !  AS RATIOS OF "NORMAL" HEIGHT AND DBH (PHT AND DTRUNC,
  !  RESPECTIVELY).

  HTRUNC = ITHT / 100.0
  PHT = 1. - (HTRUNC/H)
  DTRUNC = PHT / (AHAT*PHT + BHAT)

  !  IF THE TOP IS DAMAGED AND THE DIAMETER AT THE POINT OF
  !  TRUNCATION IS GREATER THAN THE MERCHANTABLE TOP DIAMETER,
  !  CORRECT BOARD FOOT VOLUME ESTIMATE.

  if (DTRUNC .gt. BFTOPD(ISPC)/D) then

     !     HTMRCH = RATIO OF MERCH. HT (HEIGHT WHERE D=BFTOPD) TO TOTAL
     !     HEIGHT.

     HTMRCH = ((BHAT*BFTOPD(ISPC))/D)/(1. - (AHAT*BFTOPD(ISPC)/D))

     !     ADJUST THE STUMP HEIGHT.

     STUMP = 1.0-BFSTMP(ISPC)/H
     VOLTK = BEHRE(PHT,STUMP)

     !     CORRECT FOR THE TRUNCATED TOP IN THE BFV ESTIMATE

     if (LCONE) then
        VOLM = STUMP**3-HTMRCH**3.
        VOLTK = STUMP**3. - PHT**3.
        BBFV = BBFV*VOLTK/VOLM
     else
        BBFV = (BBFV*VOLTK) / BEHRE(HTMRCH,STUMP)
     end if
  end if
  100 continue
  return
end subroutine BFTOPK
