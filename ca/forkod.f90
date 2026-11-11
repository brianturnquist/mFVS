!============================!
!   Translate forest codes   !
!============================!
! CA $Id$
subroutine FORKOD
  implicit none

  !     TRANSLATES FOREST CODE INTO A SUBSCRIPT, IFOR, AND IF
  !     KODFOR IS ZERO, THE ROUTINE RETURNS THE DEFAULT CODE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'

  ! Variables
  integer :: JFOR(11),KFOR(11),NUMFOR

  !  FORESTS IN THIS VARIANT:
  !  505 = KLAMATH
  !  506 = LASSEN
  !  508 = MENDOCINO
  !  511 = PLUMAS
  !  514 = SHASTA-TRINITY
  !  610 = ROGUE RIVER
  !  611 = SISKIYOU
  !  710 = ROSEBURG
  !  711 = MEDFORD
  !  712 = COOS BAY
  !  518 = TRINITY (MAPPED TO SHASTA-TRINITY)

  !  RESERVATION PSUEDO CODES:
  !  7801 = BERRY CREEK OFF-RES. TRUST LAND    (MAPPED TO 511 PLUMAS)
  !  7803 = COLD SPRINGS RANCHERIA             (MAPPED TO 511 PLUMAS)
  !  7804 = COLUSA RANCHERIA                   (MAPPED TO 508 MENDOCINO)
  !  7805 = CORTINA INDIAN RANCHERIA           (MAPPED TO 508 MENDOCINO)
  !  7809 = GRINDSTONE INDIAN RANCHERIA        (MAPPED TO 508 MENDOCINO)
  !  7811 = JACKSON RANCHERIA                  (MAPPED TO 511 PLUMAS)
  !  7812 = CHICKEN RANCH OFF-RES. TRUST LAND  (MAPPED TO 511 PLUMAS)
  !  7818 = PICAYUNE OFF-RES. TRUST LAND       (MAPPED TO 511 PLUMAS)
  !  7822 = RUMSEY INDIAN RANCHERIA            (MAPPED TO 508 MENDOCINO)
  !  7823 = SHINGLE SPRINGS RANCHERIA          (MAPPED TO 511 PLUMAS)
  !  7826 = TABLE MOUNTAIN RANCHERIA           (MAPPED TO 511 PLUMAS)
  !  7827 = TULE RIVER RESERVATION             (MAPPED TO 511 PLUMAS)
  !  7829 = MOORETOWN OFF-RES. TRUST LAND      (MAPPED TO 511 PLUMAS)
  !  7837 = PIT RIVER TRUST LAND               (MAPPED TO 506 LASSEN)
  !  7842 = QUARTZ VALLEY RESERVATION          (MAPPED TO 505 KLAMATH)
  !  7846 = KARUK OFF-RES. TRUST LAND          (MAPPED TO 505 KLAMATH)
  !  7864 = SANTA ROSA RANCHERIA               (MAPPED TO 511 PLUMAS)
  !  8104 = COW CREEK RESERVATION              (MAPPED TO 611 SISKIYOU)

  integer :: I
  logical :: USEIGL,FORFOUND
  data JFOR/505,506,508,511,514,610,611,710,711,712,518/,NUMFOR /11/
  data KFOR/11*1/

  USEIGL = .true.
  FORFOUND = .false.

  select case (KODFOR)
     !       CROSSWALK FOR RESERVATION PSUEDO CODES & LOCATION CODE
  case (7801)
     write(JOSTND,70)
     70 format(/,'********',T12,'BERRY CREEK OFF-RES. TRUST LAND ',&
          &'RESERVATION (7801) BEING MAPPED TO PLUMAS NF (511) FOR ',&
          &'FURTHER PROCESSING.')
     IFOR = 4

  case (7803)
     write(JOSTND,71)
     71 format(/,'********',T12,'COLD SPRINGS RANCHERIA (7803) ',&
          &'BEING MAPPED TO PLUMAS NF (511) FOR FURTHER PROCESSING.')
     IFOR = 4

  case (7804)
     write(JOSTND,72)
     72 format(/,'********',T12,'COLUSA RANCHERIA (7804) BEING ',&
          &'MAPPED TO MENDOCINO NF (508) FOR FURTHER PROCESSING.')
     IFOR = 3

  case (7805)
     write(JOSTND,73)
     73 format(/,'********',T12,'CORTINA INDIAN RANCHERIA (7805)',&
          &' BEING MAPPED TO MENDOCINO NF (508) FOR FURTHER',&
          &' PROCESSING.')
     IFOR = 3

  case (7809)
     write(JOSTND,74)
     74 format(/,'********',T12,'GRINDSTONE INDIAN RANCHERIA ',&
          &'(7809) BEING MAPPED TO MENDOCINO NF (508) FOR ',&
          &'FURTHER PROCESSING.')
     IFOR = 3

  case (7811)
     write(JOSTND,75)
     75 format(/,'********',T12,'JACKSON RANCHERIA (7811) BEING ',&
          &'MAPPED TO PLUMAS NF (511) FOR FURTHER PROCESSING.')
     IFOR = 4

  case (7812)
     write(JOSTND,76)
     76 format(/,'********',T12,'CHICKEN RANCH OFF-RES. TRUST ',&
          &'LAND (7812) BEING MAPPED TO PLUMAS NF (511) FOR ',&
          &'FURTHER PROCESSING.')
     IFOR = 4

  case (7818)
     write(JOSTND,77)
     77 format(/,'********',T12,'PICAYUNE OFF-RES. TRUST LAND ',&
          &'(7818) BEING MAPPED TO PLUMAS NF (511) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 4

  case (7822)
     write(JOSTND,78)
     78 format(/,'********',T12,'RUMSEY INDIAN RANCHERIA (7822) ',&
          &'BEING MAPPEDTO MENDOCINO NF (508) FOR FURTHER PROCESSING.')
     IFOR = 3

  case (7823)
     write(JOSTND,79)
     79 format(/,'********',T12,'SHINGLE SPRINGS RANCHERIA ',&
          &'(7823) BEING MAPPED TO PLUMAS NF (511) FOR FURTHER ',&
          &' PROCESSING.')
     IFOR = 4

  case (7826)
     write(JOSTND,80)
     80 format(/,'********',T12,'TABLE MOUNTAIN RANCHERIA (7826)',&
          &' BEING MAPPED TO PLUMAS NF (511) FOR FURTHER PROCESSING.')
     IFOR = 4

  case (7827)
     write(JOSTND,81)
     81 format(/,'********',T12,'TULE RIVER RESERVATION (7827) ',&
          &'BEING MAPPED TO PLUMAS NF (511) FOR FURTHER PROCESSING.')
     IFOR = 4

  case (7829)
     write(JOSTND,82)
     82 format(/,'********',T12,'MOORETOWN OFF-RES. TRUST LAND ',&
          &'(7829) BEING MAPPED TO PLUMAS NF (511) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 4

  case (7837)
     write(JOSTND,83)
     83 format(/,'********',T12,'PIT RIVER TRUST LAND (7837) ',&
          &'BEING MAPPED TO LASSEN NF (506) FOR FURTHER PROCESSING.')
     IFOR = 2

  case (7842)
     write(JOSTND,84)
     84 format(/,'********',T12,'QUARTZ VALLEY RESERVATION ',&
          &'(7842) BEING MAPPED TO KLAMATH NF (505) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 1

  case (7846)
     write(JOSTND,85)
     85 format(/,'********',T12,'KARUK OFF-RES. TRUST LAND ',&
          &'(7846) BEING MAPPED TO KLAMATH NF (505) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 1

  case (7864)
     write(JOSTND,86)
     86 format(/,'********',T12,'SANTA ROSA RANCHERIA (7864) ',&
          &'BEING MAPPED TO PLUMAS NF (511) FOR FURTHER PROCESSING.')
     IFOR = 4

  case (8104)
     write(JOSTND,87)
     87 format(/,'********',T12,'COW CREEK RESERVATION (8104) ',&
          &'BEING MAPPED TO SISKIYOU NF (611) FOR FURTHER PROCESSING.')
     IFOR = 7
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
     21 format(/,'********',T12,'TRINITY NF (518) BEING MAPPED TO ',&
          &'SHASTA-TRINITY (514) FOR FURTHER PROCESSING.')
     IFOR = 5
  end select

  !     SET THE IGL VARIABLE ONLY IF DEFAULT FOREST IS USED.
  !     GEOGRAPHIC LOCATION CODE: 1=NORTH, 2=CENTRAL, 3=SOUTH
  !     USED TO SET SOME EQUATIONS IN REGENERATION AND PERHAPS
  !     HEIGHT-DIAMETER IN DIFFERENT VARIANTS.
  if (USEIGL)IGL = KFOR(IFOR)

  KODFOR = JFOR(IFOR)
  return
end
