!=====================================!
!   Select example trees for output   !
!=====================================!
! BASE $Id$
subroutine EXTREE
  implicit none

  !     ASSIGNS THE EXAMPLE TREES TO THE OUTPUT ARRAYS.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'OUTCOM.f90'
  include 'PLOT.f90'

  ! Variables
  integer :: I,INS1,IMCI,ISP1
  character(len=3) :: NS
  data NS/'---'/

  !  IF THE TREE LIST IS EMPTY, THEN: ZERO OUT THE SAMPLE TREE LIST.

  if (ITRN.eq.0) goto 20
  do I = 1,6
     INS1 = INS(I)
     IMCI = IMC(INS1)
     ISP1 = ISP(INS1)
     IONSP(I) = NSP(ISP1,IMCI)(1:3)
     DBHIO(I) = DBH(INS1)
     HTIO(I) = HT(INS1)
     IOICR(I) = ICR(INS1)
     DGIO(I) = DG(INS1)
     PCTIO(I) = PCT(INS1)
     PRBIO(I) = PROB(INS1)/TRM
     10 continue
  end do

  return

  20 continue
  do I = 1,6
     IONSP(I) = NS
     DBHIO(I) = 0.
     HTIO(I) = 0.
     IOICR(I) = 0
     DGIO(I) = 0.
     PCTIO(I) = 0.
     PRBIO(I) = 0.
     30 continue
  end do
  return
end subroutine EXTREE
