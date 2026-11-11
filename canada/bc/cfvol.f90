!=======================!
!   canada/bc : cfvol   !
!=======================!
! CANADA-BC $Id$
subroutine CFVOL(ISPC,D,H,D2H,VN,VM,VMAX,TKILL,LCONE,BARK,ITHT,&
     &CTKFLG)
  implicit none

  ! THIS ROUTINE CALCULATES TOTAL CUBIC FOOT VOLUME USING A NUMBER OF
  ! DIFFERENT METHODS.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'

  !  DIMENSION STATEMENT FOR INTERNAL ARRAYS.

  ! Variables
  logical :: TKILL,LCONE,CTKFLG
  integer :: ISPC,ITHT,FIZ

  real    :: D,H,D2H,VN,VM,VMAX,BARK,VOLT,VOLM
  real    :: HTRUNC

  data FIZ /2/

  !  INITIALIZE VOLUME ESTIMATES.

  VN = 0.0
  VM = 0.0
  VMAX = 0.0

  !  METHC=4 -- COMPUTE VOLUMES BASED ON KOZAK'S TAPER FUNCTIONS. IF HEIGHT IS LESS THAN
  !             4.5 FT OR DBH IS LESS THAN MINIMUM, OR DBH IS LESS THAN TOP DIAMETER
  !             THEN THE TREE IS COMPLETELY IGNORED IN MERCHANTABILITY CALCULATIONS. ALL
  !             TREES ARE USED FOR TOTAL VOLUME CALCULATIONS.

  HTRUNC = FLOAT(ITHT)
  if (HTRUNC .gt.0.0) HTRUNC = HTRUNC / 100.0
  if (HTRUNC .eq.0.0) HTRUNC = H
  if (H .ge.4.5) then

     !         This needs to be updated to use Kozak's 2002 refit; also to calculate log
     !         volumes by BEC zone; which the new LOG(...) subroutine uses.

     call MIN(ISPC,IAGE,FIZ,D,H,STMP(ISPC),TOPD(ISPC),&
          &HTRUNC,VOLT,VOLM)
     if (D .ge. DBHMIN(ISPC) .and. D .ge. TOPD(ISPC)) then
        VN = VOLT
        VM = VOLM
     else
        VN = VOLT
        VM = 0.0
     end if
  end if
  return

  return
end
