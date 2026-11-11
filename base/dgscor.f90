!=============================================!
!   Carry diameter growth prediction errors   !
!=============================================!
! BASE $Id$
subroutine DGSCOR (SSIG,FRM,RHO,RHOCP,IT)
  implicit none

  !  THIS SUBROUTINE CALCULATES THE ERROR IN DIAMETER GROWTH PREDICTION
  !  THAT WILL BE CARRIED OVER INTO THE NEXT CYCLE.

  ! Common blocks
  include 'PRGPRM.F77'
  include 'ARRAYS.F77'
  include 'CONTRL.F77'
  include 'PLOT.F77'

  external RANN

  ! Variables
  integer :: IT
  real    :: RHOCP,RHO,FRM,SSIG,BACHLO,DDS

  !   IF NOT TRIPLING, ASSIGN DIAMETER GROWTH FROM A NORMAL
  !   DISTRIBUTION ABOUT PREDICTED LN(DDS). DISTRIBUTION IS BOUNDED
  !   TO + OR - DGSD STANDARD DEVIATIONS.

  FRM = 0.0
  if (DGSD .lt. 1.0) go to 25
  20 continue
  FRM = BACHLO(0.0,SSIG,RANN)
  FRM = FRM*RHOCP+RHO*OLDRN(IT)
  if (ABS(FRM).gt.DGSD*SSIG) go to 20
  25 continue
  DDS = WK2(IT)
  if (DDS.gt.5.0) then
     FRM = 0.0
  else
     if (DDS.gt.4.) FRM = (DDS-4.)*FRM
  end if
  OLDRN(IT) = FRM
  FRM = EXP(FRM)
  return
end subroutine DGSCOR
