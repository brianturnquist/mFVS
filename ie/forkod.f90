!============================!
!   Translate forest codes   !
!============================!
! IE $Id$
subroutine FORKOD
  implicit none

  !     TRANSLATES FOREST CODE INTO A SUBSCRIPT, IFOR, AND IF
  !     KODFOR IS ZERO, THE ROUTINE RETURNS THE DEFAULT CODE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'

  !  NATIONAL FORESTS:
  !  103 = BITTERROOT
  !  104 = IDAHO PANHANDLE
  !  105 = CLEARWATER
  !  106 = COEUR D'ALENE
  !  621 = COLVILLE
  !  110 = FLATHEAD
  !  113 = KANIKSU
  !  114 = KOOTENAI
  !  116 = LOLO
  !  117 = NEZPERCE
  !  118 = ST. JOE
  !  613 = KANIKSU ADMINISTERED BY COLVILLE (MAPPED TO KANIKSU 113)
  !  102 = BEAVERHEAD                       (MAPPED TO BITTERROOT 103)
  !  109 = DEERLODGE                        (MAPPED TO BITTERROOT 103)
  !  112 = HELENA                           (MAPPED TO LOLO 116)

  !  RESERVATION PSUEDO CODES:
  !  8106 = COLVILLE RESERVATION            (MAPPED TO COLVILLE  621)
  !  8107 = NEZ PERCE RESERVATION           (MAPPED TO NEZ PERCE 117)
  !  8109 = KOOTENAI OFF-RES. TRUST LAND    (MAPPED TO KANIKSU   113)
  !  8131 = SPOKANE RESERVATION             (MAPPED TO COLVILLE  621)
  !  8132 = KALISPEL RESERVATION            (MAPPED TO KANIKSU   113)
  !  8133 = FLATHEAD RESERVATION            (MAPPED TO FLATHEAD  110)
  !  8137 = COEUR D'ALENE RESERVATION       (MAPPED TO ST. JOE   118)

  ! Variables
  integer :: JFOR(15),KFOR(15),NUMFOR,I
  logical :: USEIGL,FORFOUND
  data JFOR/103,104,105,106,621,110,113,114,116,117,118,613,&
       &102,109,112/,&
       &NUMFOR /15/
  data KFOR/1,1,3,2,1,1,1,1,1,3,2,1,1,1,1 /

  USEIGL = .true.
  FORFOUND = .false.

  select case (KODFOR)
     !       CROSSWALK FOR RESERVATION PSUEDO CODES & LOCATION CODE
  case (8106)
     write(JOSTND,70)
     70 format(/,'********',T12,'COLVILLE RESERVATION (8106) ',&
          &'BEING MAPPED TO COLVILLE NF (621) FOR FURTHER PROCESSING.')
     IFOR = 5

  case (8107)
     write(JOSTND,71)
     71 format(/,'********',T12,'NEZ PERCE RESERVATION (8107) ',&
          &'BEING MAPPED TO NEZ PERCE NF (117) FOR FURTHER PROCESSING.')
     IFOR = 10

  case (8109)
     write(JOSTND,72)
     72 format(/,'********',T12,'KOOTENAI OFF-RES. TRUST (8109) ',&
          &'BEING MAPPED TO KANIKSU NF (113) FOR FURTHER PROCESSING.')
     IFOR = 7

  case (8131)
     write(JOSTND,73)
     73 format(/,'********',T12,'SPOKANE RESERVATION (8131) ',&
          &'BEING MAPPED TO COLVILLE NF (621) FOR FURTHER PROCESSING.')
     IFOR = 5

  case (8132)
     write(JOSTND,74)
     74 format(/,'********',T12,'KALISPEL RESERVATION (8132) ',&
          &'BEING MAPPED TO KANIKSU NF (113) FOR FURTHER PROCESSING.')
     IFOR = 7

  case (8133)
     write(JOSTND,75)
     75 format(/,'********',T12,'FLATHEAD RESERVATION (8133) ',&
          &'BEING MAPPED TO FLATHEAD NF (110) FOR FURTHER PROCESSING.')
     IFOR = 6

  case (8137)
     write(JOSTND,76)
     76 format(/,'********',T12,'COEUR D''ALENE RESERV. (8137) ',&
          &'BEING MAPPED TO ST. JOE NF (118) FOR FURTHER PROCESSING.')
     IFOR = 11
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
  case (12)
     write(JOSTND,22)
     22 format(/,'********',T12,'KANIKSU NF (613) BEING MAPPED TO ',&
          &'KANIKSU (113) FOR FURTHER PROCESSING.')
     IFOR = 7
  case (13)
     write(JOSTND,24)
     24 format(/,'********',T12,'BEAVERHEAD (102) BEING MAPPED TO ',&
          &'BITTERROOT (103) FOR FURTHER PROCESSING.')
     IFOR = 1
  case (14)
     write(JOSTND,26)
     26 format(/,'********',T12,'DEERLODGE (109) BEING MAPPED TO ',&
          &'BITTERROOT (103) FOR FURTHER PROCESSING.')
     IFOR = 1
  case (15)
     write(JOSTND,28)
     28 format(/,'********',T12,'HELENA (112) BEING MAPPED TO ',&
          &'LOLO (116) FOR FURTHER PROCESSING.')
     IFOR = 9
  end select

  !     SET THE IGL VARIABLE ONLY IF DEFAULT FOREST IS USED
  !     GEOGRAPHIC LOCATION CODE: 1=NORTH, 2=CENTRAL, 3=SOUTH
  !     USED TO SET SOME EQUATIONS IN REGENERATION AND PERHAPS
  !     HEIGHT-DIAMETER IN DIFFERENT VARIANTS.
  if (USEIGL)IGL = KFOR(IFOR)

  KODFOR = JFOR(IFOR)
  return
end
