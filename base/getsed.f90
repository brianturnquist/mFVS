!===================================!
!   Generate a random number seed   !
!===================================!
! BASE $Id$
subroutine GETSED (SED)
  implicit none

  !     GENERATES A SEED FOR THE RANDOM NUMBER GENERATOR.

  ! Variables
  real    :: SED
  integer :: I1,I2,I3,I4
  character(len=10) :: DAT
  character(len=8) :: TIM

  save I4
  data I4/100/

  call GRDTIM (DAT,TIM)
  read (TIM,'(T4,I2,T7,I2,T8,I1)') I1,I2,I3
  I4 = I4+I3
  if (I4.gt.300) I4 = 100+I3
  I3 = MOD(I3,2)+1
  SED = FLOAT(IFIX( (I2*10000.+I1*100.+I2)/(I1+1.)* &
       & FLOAT(I2+I4)/10**I3 ) )
  if (AMOD(SED,2.0).eq.0.) SED = SED+1.
  write (DAT,'(F9.0)') SED
  read  (DAT,'(F10.0)') SED
  return
end subroutine GETSED
