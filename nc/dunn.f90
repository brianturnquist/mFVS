!===============!
!   nc : dunn   !
!===============!
! NC $Id$
subroutine DUNN (SS)
  implicit none

  ! THIS SUBROUTINE PROCESSES THE DUNNING CODE INFORMATION THAT WAS
  ! ENTERED BY KEYWORD.

  ! THIS ROUTINE IS ENTERED WITH A VLAUE BETWEEN 0 AND 7. THIS VALUE
  ! IS THE TRANSLATED INTO THE SITE INDEX FOR LARGE TREES. THE SITE
  ! INDEX FOR SMALL TREES IS ALSO SET AS THE VALUE OF DU50.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'

  ! Variables
  real    :: ADJFAC(12),DUNN50(8),XST2(8),SS,DU50
  integer :: IST,ISPC,I
  data ADJFAC/ 0.90, 0.90, 1.00, 1.00, 0.57, 0.76,&
       &0.57, 0.57, 1.00, 1.00, 0.57, 1.00/
  data DUNN50/106.,90.,75.,56.,49.,39.,31.,23./

  IST = INT(SS+1.0)
  DU50 = DUNN50(IST)

  !   SET SITE INDEX VALUES BASED ON DUNNING VALUES ENTERED.

  do ISPC = 1,MAXSP
     SITEAR(ISPC) = DU50 * ADJFAC(ISPC)
     20 continue
  end do
  return

  !   NC FIRE MODEL NEEDS SITE INDICES FOR DUNNING CODES.

  entry GETDUNN(XST2)
  do I = 1,8
     XST2(I) = DUNN50(I)
  end do
  return
end
