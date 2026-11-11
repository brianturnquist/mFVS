!=================!
!   rd : rdsadd   !
!=================!
! RD $Id$
subroutine RDSADD(I,TP)
  implicit none

  !  ADD TO THE STUMP LIST.

  !  CALLED BY :
  !     RDSTR   [ROOT DISEASE]

  !  CALLS     :
  !     RDSSIZ  (SUBROUTINE)  [ROOT DISEASE]
  !     RDSTP   (SUBROUTINE)  [ROOT DISEASE]
  !     RDDBUG  (SUBROUTINE)  [ROOT DISEASE]

  !  PARAMETERS :
  !     I      - (I ) Tree record number.
  !     TP     - (I ) Proportion of trees in current tree record not
  !                   cut by (Prognosis) subroutine CUTS.

  !  COMMON BLOCK VARIABLES :

  !  LOCAL VARIABLES :
  !     DEN
  !     JJ

  !  Revision History :
  !   04/07/93 - Last revision date.
  !   09/02/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  !.... PARAMETER INCLUDE FILES

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'

  !.... COMMON INCLUDE FILES

  include 'CONTRL.f90'
  include 'RDCOM.f90'
  include 'RDARRY.f90'
  include 'RDADD.f90'
  include 'ARRAYS.f90'

  !.... Local variables

  ! Variables
  integer :: I,ISL,JJ
  real    :: TP,DEN

  DEN = PROBIT(I) * (1.0 - TP)
  JJ = ISP(I)

  call RDSSIZ (JJ,DBH(I),STCUT,ISL,ISPS,IRTSPC)
  call RDSTP  (ISL,JJ,DEN,DBH(I),ROOTL(I))

  return
end
