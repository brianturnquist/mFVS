!============================!
!   Translate forest codes   !
!============================!
! CANADA-ON $Id$
subroutine FORKOD
  implicit none

  !     TRANSLATES FOREST CODE INTO A SUBSCRIPT, IFOR, AND IF
  !     KODFOR IS ZERO, THE ROUTINE RETURNS THE DEFAULT CODE.

  !     CODES 902-913 ARE FROM THE LS VARIANT
  !     CODES 915 & 916 ARE FOR ONTARIO

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'

  ! Variables
  integer :: JFOR(11),KFOR(11),NUMFOR,I
  logical :: USEIGL,FORFOUND
  data JFOR/902,903,904,906,907,909,910,913,915,916,924/
  data NUMFOR/11/
  data KFOR/11*1/

  USEIGL = .true.
  FORFOUND = .false.

  select case (KODFOR)

  case default

     !         CONFIRMS THAT KODFOR IS AN ACCEPTED FVS LOCATION CODE
     !         FOR THIS VARIANT FOUND IN DATA ARRAY JFOR
     do I = 1,NUMFOR
        if (KODFOR .eq. JFOR(I)) then
           IFOR = I
           FORFOUND = .true.
           exit
        end if
        10 continue
     end do

     !         LOCATION CODE ERROR TRAP
     if (.not. FORFOUND) then
        call ERRGRO (.true.,3)
        write(JOSTND,11) JFOR(IFOR)
        11 format(/,'********',T12,'FOREST CODE USED IN THIS ',&
             &'PROJECTION IS',I4)
        USEIGL = .false.
     end if

  end select

  !     FOREST MAPPING CORRECTION
  select case (IFOR)
  case (11)
     write(JOSTND,21)
     21 format(/,'********',T12,'MANISTEE NF (924) BEING MAPPED TO ',&
          &'HURON-MANISTEE (904) FOR FURTHER PROCESSING.')
     IFOR = 3
  end select

  !     SET THE IGL VARIABLE ONLY IF DEFAULT FOREST IS USED
  !     GEOGRAPHIC LOCATION CODE: 1=NORTH, 2=CENTRAL, 3=SOUTH
  !     USED TO SET SOME EQUATIONS IN REGENERATION AND PERHAPS
  !     HEIGHT-DIAMETER IN DIFFERENT VARIANTS.
  if (USEIGL) IGL = KFOR(IFOR)

  KODFOR = JFOR(IFOR)

  !  SET DEFAULT TLAT, TLONG, AND ELEVATION VALUES, BY FOREST
  !  = use US SUPERIOR for now

  select case(KODFOR)
  case(915)
     if (TLAT.eq.0) TLAT = 46.78
     if (TLONG.eq.0)TLONG = 92.11
     if (ELEV.eq.0) ELEV = 16.
  case(916)
     if (TLAT.eq.0) TLAT = 46.78
     if (TLONG.eq.0)TLONG = 92.11
     if (ELEV.eq.0) ELEV = 16.

  end select
  return
end
