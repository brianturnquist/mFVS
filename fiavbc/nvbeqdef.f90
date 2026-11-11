!=======================!
!   fiavbc : nvbeqdef   !
!=======================!
subroutine NVBEQDEF (SPCD, VOLEQ)
  implicit none

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'
  include 'VARCOM.f90'

  ! Variables
  character(LEN = 4) :: SppID
  character(LEN = 4) :: DivSplit
  character(LEN = 4) :: DIVISION
  character(LEN = 10) :: WoodlandEqs(16)
  character(LEN = 11) :: VOLEQ
  integer :: I,SPCD

  integer :: WoodlandSpp(16)

  data (WoodlandSpp(I), I = 1,16) /&
       &62,  63,  65,  66,&
       &69, 106, 133, 134,&
       &143, 321, 322, 475,&
       &803, 810, 814, 843/

  data (WoodlandEqs(I), I = 1,16) /&
       &"R03CHO0065", "R03CHO0066", "R03CHO0065", "R03CHO0066",&
       &"R03CHO0065", "R03CHO0106", "400DVEW133", "R03CHO0106",&
       &"R03CHO0106", "200DVEW475", "200DVEW814", "200DVEW475",&
       &"300DVEW800", "300DVEW800", "200DVEW814", "300DVEW800" /

  DivSplit = '0000'
  DIVISION = ECOREG

  select case (SPCD)
  case (62, 63, 65, 66, 69, 106, 133, 134,&
       &143, 321, 322, 475, 803, 810, 814, 843)
     if (ISTATE.eq.0) call ERRGRO(.true., 44)
     do I = 1, size(WoodlandSpp)
        if (WoodlandSpp(I).eq.SPCD) then
           VOLEQ = WoodlandEqs(I)
           exit
        end if
     end do
     if (ISTATE.eq.6.or. ISTATE.eq.41.or. ISTATE.eq.53) then
        if (SPCD.eq.62.or. SPCD.eq.65)   VOLEQ = '400DVEW065'
        if (SPCD.eq.66)                   VOLEQ = '200DVEW066'
        if (SPCD.eq.322)                  VOLEQ = '200DVEW475'
     else if (ISTATE.eq.4.or. ISTATE.eq.35) then
        if (SPCD.eq.332.or. SPCD.eq.814) VOLEQ = '300DVEW800'
     end if

  case default
     ! Species is not woodland and uses NVB species
     DIVISION = ADJUSTL(DIVISION)
     if (LEN_TRIM(DIVISION) .lt.4) DIVISION = '0'//TRIM(DIVISION)
     DivSplit = TRIM(DIVISION)
     write(SppID, FMT = '(I4)') SPCD
     SppID = ADJUSTL(SppID)

     do while (LEN_TRIM(SppID) .lt.3)
        SppID = '0' // TRIM(SppID)
     end do

     VOLEQ = 'NVB' // DivSplit // TRIM(SppID)

     !       Check for managed plantation
     if ((DivSplit .eq.'0000' .or. DivSplit .eq. '0230').and.&
          &(SPCD .eq.111.or. SPCD .eq.131)&
          &.and. ISTDORG .eq.1) then
        VOLEQ(11:11) = 'P'
     end if
  end select

  return
end
