!=============================!
!   Set regeneration timing   !
!=============================!
! STRP $Id$
subroutine ESTIME (IEVTYR,KDT)
  implicit none

  !     CALLED FROM ESTAB.

  !     IEVTYR= A YEAR OF INTEREST.  THE DATA OF DISTURBANCE OR SITE
  !             PREP; SOMETIMES THE BEGINNING OF THE CURRENT TALLY.
  !     KDT   = THE TALLY YEAR.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ESCOMN.f90'
  include 'ESWSBW.f90'

  !     DEFINE TIME AND WSBW VARIABLES FOR REGENERATION MODEL.

  ! Variables
  real    :: SQRVEC(20)
  integer :: KDT,IEVTYR,IYR,I
  data SQRVEC/1.0, .41421, .31784, .26795, .23607, .21342,&
       &.19626, .18268,.17157, .16228, .15435, .14748, .14145,&
       &.13611, .13133,.12702, .12311, .11954, .11626, .11324/

  BWB4 = 0.0
  BWAF = 0.0
  SQBWAF = 0.0
  if (NBWHST.gt.0) then
     do IYR = (IEVTYR-5),(IEVTYR-1)
        do I = 1,NBWHST
           if (IYR.ge.IBWHST(1,I).and.IYR.le.IBWHST(2,I)) then
              BWB4 = BWB4+1.0
              goto 20
           end if
           10 continue
        end do
        20 continue
     end do
     do IYR = IEVTYR,KDT
        do I = 1,NBWHST
           if (IYR.ge.IBWHST(1,I).and.IYR.le.IBWHST(2,I)) then
              BWAF = BWAF+1.0
              SQBWAF = SQBWAF+ SQRVEC(IYR-IEVTYR+1)
              goto 40
           end if
           30 continue
        end do
        40 continue
     end do
  end if
  SQREGT = SQRT(TIME)-SQBWAF
  REGT = TIME-BWAF
  return
end
