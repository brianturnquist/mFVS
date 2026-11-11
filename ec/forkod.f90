!============================!
!   Translate forest codes   !
!============================!
! EC $Id$
subroutine FORKOD
  implicit none

  !     TRANSLATES FOREST CODE INTO A SUBSCRIPT, IFOR, AND IF
  !     KODFOR IS ZERO, THE ROUTINE RETURNS THE DEFAULT CODE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'

  !  NATIONAL FORESTS:
  !  606 = MT HOOD
  !  608 = OKANOGAN
  !  617 = WENATCHEE
  !  699 = OKANOGAN (TONASKET RD)
  !  603 = GIFFORD PINCHOT (MAPPED TO WENATCHEE)
  !        GP WAS IN THE AREA OF INTEREST BUT NO DATA WERE AVAILABLE;
  !        THUS IT IS MAPPED TO THE WENATCHEE FOR PROCESSING.
  !  613 = MT BAKER - SNOQUALMIE (MAPPED TO WENATCHEE)
  !  621 = COLVILLE NF (MAPPED TO OKANOGAN - TONASKET RD) 04/2023 DW

  !  NOTE: GIFFORD PINCHOT GETS IGL=2 AND MT BAKER/SNOQUALMIE GETS IGL=3.
  !        THIS IS SO FOREST SPECIFIC HT-DBH EQNS CAN BE INDEXED IN
  !        SUBROUTINE **HTDBH**.

  !  RESERVATION PSUEDO CODES:

  !  8106 = COLVILLE RESERVATION      (MAPPED TO 608 OKANOGAN)
  !  8117 = UMATILLA RESERVATION      (MAPPED TO 606 MT. HOOD)
  !  8130 = YAKAMA NATION RESERVATION (MAPPED TO 613 SNOQUALMIE)
  !  8131 = SPOKANE RESERVATION       (MAPPED TO 617 WENATCHEE)

  ! Variables
  integer :: JFOR(8),KFOR(8),NUMFOR,I
  logical :: USEIGL,FORFOUND
  data JFOR/606,608,617,699,603,613,621,0 /, NUMFOR /7/
  data KFOR/4*1,2,3,1,1/

  USEIGL = .true.
  FORFOUND = .false.

  select case (KODFOR)

     !       CROSSWALK FOR RESERVATION PSUEDO CODES & LOCATION CODE
  case (8106)
     write(JOSTND,60)
     60 format(/,'********',T12,'COLVILLE RESERVATION (8106) ',&
          &'BEING MAPPED TO OKANOGAN NF (608) FOR FURTHER PROCESSING.')
     IFOR = 2
  case (8117)
     write(JOSTND,61)
     61 format(/,'********',T12,'UMATILLA RESERVATION (8117) ',&
          &'BEING MAPPED TO MT. HOOD NF (606) FOR FURTHER PROCESSING.')
     IFOR = 1
  case (8130)
     write(JOSTND,63)
     63 format(/,'********',T12,'YAKAMA NATION RESERVATION ',&
          &'(8130) BEING MAPPED TO SNOQUALMIE NF (613) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 6
  case (8131)
     write(JOSTND,64)
     64 format(/,'********',T12,'SPOKANE RESERVATION (8131) BEING',&
          &' MAPPED TO WENATCHEE NF (617) FOR FURTHER PROCESSING.')
     IFOR = 3
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
  case (5)
     write(JOSTND,21)
     21 format(/,'********',T12,'GIFFORD PINCHOT NF (603) BEING ',&
          &'MAPPED TO WENATCHEE (617) FOR FURTHER PROCESSING.')
     I = 3
     IFOR = 3
     IGL = 2
     USEIGL = .false.
  case (6)
     write(JOSTND,22)
     22 format(/,'********',T12,'MT BAKER-SNOQUALMIE NF (613) BEING ',&
          &'MAPPED TO WENATCHEE (617) FOR FURTHER PROCESSING.')
     IFOR = 3
     IGL = 3
     USEIGL = .false.
  case (7)
     write(JOSTND,23)
     23 format(/,'********',T12,'COLVILLE NF (621) BEING ',&
          &'MAPPED TO OKANOGAN (TONASKET RD) (699)',&
          &'FOR FURTHER PROCESSING.')
     IFOR = 4
     IGL = 1
     USEIGL = .false.
  end select

  !     SET THE IGL VARIABLE ONLY IF DEFAULT FOREST IS USED
  !     GEOGRAPHIC LOCATION CODE: 1=NORTH, 2=CENTRAL, 3=SOUTH
  !     USED TO SET SOME EQUATIONS IN REGENERATION AND PERHAPS
  !     HEIGHT-DIAMETER IN DIFFERENT VARIANTS.
  if (USEIGL) IGL = KFOR(IFOR)

  KODFOR = JFOR(IFOR)
  return
end
