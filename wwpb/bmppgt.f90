!===================!
!   wwpb : bmppgt   !
!===================!
! WWPB $Id$
subroutine BMPPGT (IPNT, ILIMIT)

  !  Purpose:
  !     Get the damage code information for the given stand.
  !  This is part of the Parallel Processing Extension, and freely
  !  adapted from MSPPGT.FOR. In this case, damage code information is
  !  only useful from the initialization. Therefore, if the master cycle
  !  is more than 2, the variable is not saved or restored.

  !  Call list definitions:
  !     ILIMIT: (I)  Size of print buffer WK3.
  !     IPNT:   (IO) Pointer to curent element in print buffer WK3.

  !  Local variable definitions:

  !  Common block variables and parameters:
  !     LBMDAM:   From BMCOM; TRUE if damage code in inventory
  !     WK3:      From ARRAYS; Work array used as a print buffer.

  !.... Parameter statements.

  parameter (MXL = 1, MXR = 0, MXI = 0)

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PPEPRM.f90'
  include 'BMPRM.f90'

  !.... Common include files.

  include 'ARRAYS.f90'
  include 'PPCNTL.f90'
  include 'CONTRL.f90'
  include 'BMCOM.f90'

  !.... Variable declarations.

  ! Variables
  logical :: LOGICS(MXL),LX

  !     GET THE LOGICAL SCALAR. THIS IS .TRUE. IF THE MASTER
  !     CYCLE IS 2 OR LESS, .FALSE. OTHERWISE.

  call LFREAD (WK3, IPNT, ILIMIT, LOGICS, MXL, 2)
  LX = LOGICS (1)

  !     GET THE LOGICAL ARRAYS.

  if (LX) call LFREAD (WK3,IPNT,ILIMIT,LBMDAM,ITRN,   2)

  return
end
