!===============================!
!   fiavbc : nvb_region_check   !
!===============================!
subroutine NVB_REGION_CHECK
  implicit none

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'
  include 'VARCOM.f90'

  ! Variables
  character(LEN = 4) :: DIVS(19)
  logical :: VALIDDIV
  integer :: I

  data (DIVS(I), I = 1,19) /&
       &"130 ", "210 ", "220 ", "230 ", "240 ",&
       &"250 ", "260 ", "310 ", "330 ", "340 ",&
       &"M130", "M210", "M220", "M230", "M240",&
       &"M260", "M310", "M330", "M340"/

  I = SCAN(ECOREG, '0123456789', .true.)
  ECOREG(I:I) = '0'
  ECOREG = ECOREG(:I)

  VALIDDIV = .false.
  do I = 1,19
     if (ECOREG .eq. DIVS(I)) VALIDDIV = .true.
  end do

  if (.not. VALIDDIV) then
     ECOREG = '0000'
     call ERRGRO(.true., 43)
  end if

end
