!===============================!
!   Write stand policy labels   !
!===============================!
! BASE $Id$
subroutine LBSPLW (JOSTND)
  implicit none

  !     WRITE THE STAND POLICY LABEL SET ON DATA SET JOSTND

  !     PART OF THE LABEL PROCESSING COMPONENT OF THE PROGNOSIS SYSTEM
  !     N.L. CROOKSTON -- INTERMOUNTAIN RESEARCH STATION -- JAN 1987

  !     JOSTND= PRINT MESSAGE FILE.

  ! Common blocks
  include 'PRGPRM.F77'
  include 'OPCOM.F77'

  ! Variables
  integer :: JOSTND,I1,I2

  !     IF LABEL PROCESSING HAS NOT BEEN ACTIVIATED, BRANCH TO EXIT.

  if (.not.LBSETS) goto 40

  !     IF THE STAND POLICY LABEL IS UNSET (LENSLS=-1) THEN: BRANCH
  !     TO EXIT.

  if (LENSLS.eq.-1) goto 40

  !     WRITE THE STAND LABEL SET.

  I1 = 1
  I2 = 100
  20 continue
  if (I2.gt.LENSLS) I2 = LENSLS
  if (I1.eq.1) then
     write (JOSTND,'(/''STAND POLICIES:'',T19,A)') SLSET(I1:I2)
  else
     write (JOSTND,'(T19,A)') SLSET(I1:I2)
  end if
  if (I2.lt.LENSLS) then
     I1 = I2+1
     I2 = I2+100
     goto 20
  end if
  40 continue
  return
end subroutine LBSPLW
