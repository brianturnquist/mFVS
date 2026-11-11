!============================!
!   Translate forest codes   !
!============================!
! WC $Id$
subroutine FORKOD
  implicit none

  !     TRANSLATES FOREST CODE INTO A SUBSCRIPT, IFOR, AND IF
  !     KODFOR IS ZERO, THE ROUTINE RETURNS THE DEFAULT CODE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'

  !  NATIONAL FORESTS:
  !  603 = GIFFORD PINCHOT
  !  605 = MT BAKER - SNOQUALMIE
  !  606 = MT HOOD
  !  610 = ROGUE RIVER
  !  615 = UMPQUA
  !  618 = WILLAMETTE
  !  708 = BLM SALEM ADU
  !  709 = BLM EUGENE ADU
  !  710 = BLM ROSEBURG ADU
  !  711 = BLM MEDFORD ADU
  !  613 = MT BAKER-SNOQUALMIE (MAP TO MT BAKER-SNOQUALMIE)

  !  RESERVATION PSUEDO CODES:
  !  8124 = SAUK-SUIATTLE RESERVATION (MAPPED TO 605 MT. BAKER)
  !  8130 = YAKAMA NATION RESERVATION (MAPPED TO 603 GIFFORD PINCHOT)

  ! Variables
  integer :: JFOR(11),KFOR(11),NUMFOR,I
  logical :: USEIGL,FORFOUND
  data JFOR/603,605,606,610,615,618,708,709,710,711,613/, NUMFOR/11/
  data KFOR/ 11*1 /

  USEIGL = .true.
  FORFOUND = .false.

  select case (KODFOR)

     !       CROSSWALK FOR RESERVATION PSUEDO CODES & LOCATION CODE
  case (8124)
     write(JOSTND,62)
     62 format(/,'********   ',T12,'SAUK-SUIATTLE RESERVATION (8124)',&
          &' BEING MAPPED TO MT. BAKER NF (605) FOR FURTHER PROCESSING.')
     IFOR = 2
  case (8130)
     write(JOSTND,63)
     63 format(/,'********   ',T12,'YAKAMA NATION RESERVATION (8130)',&
          &' BEING MAPPED TO GIFFORD PINCHOT NF (603) FOR FURTHER',&
          &' PROCESSING.')
     IFOR = 1
     !       END CROSSWALK FOR RESERVATION PSUEDO CODES & LOCATION CODE

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
     21 format(/,'********',T12,'MT BAKER-SNOQUALMIE NF (613) BEING ',&
          &'MAPPED TO MT BAKER-SNOQUALMIE (605) FOR FURTHER PROCESSING.')
     IFOR = 2
  end select

  !     SET THE IGL VARIABLE ONLY IF DEFAULT FOREST IS USED
  !     GEOGRAPHIC LOCATION CODE: 1=NORTH, 2=CENTRAL, 3=SOUTH
  !     USED TO SET SOME EQUATIONS IN REGENERATION AND PERHAPS
  !     HEIGHT-DIAMETER IN DIFFERENT VARIANTS.
  if (USEIGL) IGL = KFOR(IFOR)

  KODFOR = JFOR(IFOR)
  return
end
