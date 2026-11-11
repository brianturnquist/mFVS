!============================!
!   Translate forest codes   !
!============================!
! NE $Id$
subroutine FORKOD
  implicit none

  !     TRANSLATES FOREST CODE INTO A SUBSCRIPT, IFOR, AND IF
  !     KODFOR IS ZERO, THE ROUTINE RETURNS THE DEFAULT CODE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'

  !  NATIONAL FORESTS:
  !  914 = WAYNE
  !  922 = WHITE MOUNTAIN
  !  919 = ALLEGHENY
  !  920 = GREEN MTN - FINGER LAKES
  !  921 = MONONGAHELA
  !  911 = OLD WAYNE-HOOSIER (MAP TO WAYNE)
  !  930 = FINGER LAKES (MAP TO GREEN MTN - FINGER LAKES)

  !  RESERVATION PSUEDO CODES:
  !  8200 = PASSAMAQUODDY RESERVATION    (MAPPED TO 922 WHITE MOUNTAIN)
  !  8201 = PENOBSCOT OFF-RESERVATION TL (MAPPED TO 922 WHITE MOUNTAIN)
  !  8202 = HOULTON MALISEET RESERVATION (MAPPED TO 922 WHITE MOUNTAIN)
  !  8203 = MASHANTUCKET PEQUOT RES      (MAPPED TO 922 WHITE MOUNTAIN)
  !  8204 = PAUCATUCK EASTERN PEQUOT RES (MAPPED TO 922 WHITE MOUNTAIN)
  !  8206 = NARRAGANSETT RESERVATION     (MAPPED TO 922 WHITE MOUNTAIN)
  !  8208 = WAMPANOAG-AQUINNAH TL        (MAPPED TO 922 WHITE MOUNTAIN)
  !  8209 = AROOSTOOK BAND OF MICMAC TL  (MAPPED TO 922 WHITE MOUNTAIN)
  !  8211 = MOHEGAN RESERVATION          (MAPPED TO 922 WHITE MOUNTAIN)
  !  8214 = CAYUGA NATION TDSA           (MAPPED TO 930 FINGER LAKES)
  !  8215 = ONONDAGA NATION RESERVATION  (MAPPED TO 930 FINGER LAKES)
  !  8216 = TONAWANDA RESERVATION        (MAPPED TO 930 FINGER LAKES)
  !  8217 = TUSCARORA NATION RESERVATION (MAPPED TO 930 FINGER LAKES)
  !  8218 = ONEIDA NATION RESERVATION    (MAPPED TO 930 FINGER LAKES)

  ! Variables
  integer :: JFOR(7),KFOR(7),NUMFOR,I
  logical :: USEIGL,FORFOUND
  data JFOR/914,922,919,920,921,911,930/
  data NUMFOR/7/
  data KFOR/7*1/

  USEIGL = .true.
  FORFOUND = .false.

  select case (KODFOR)

     !       CROSSWALK FOR RESERVATION PSUEDO CODES & LOCATION CODE
  case (8200)
     write(JOSTND,60)
     60 format(/,'********',T12,'PASSAMAQUODDY RESERVATION (8200) ',&
          &'BEING MAPPED TO WHITE MOUNTAIN NF (922) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 2
  case (8201)
     write(JOSTND,61)
     61 format(/,'********',T12,'PENOBSCOT OFF-RESERVATION TL (8201)',&
          &' BEING MAPPED TO WHITE MOUNTAIN NF (922) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 2
  case (8202)
     write(JOSTND,62)
     62 format(/,'********',T12,'HOULTON MALISEET RESERVATION (8202)',&
          &' BEING MAPPED TO WHITE MOUNTAIN NF (922) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 2
  case (8203)
     write(JOSTND,63)
     63 format(/,'********',T12,'MASHANTUCKET PEQUOT RES (8203) ',&
          &'BEING MAPPED TO WHITE MOUNTAIN NF (922) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 2
  case (8204)
     write(JOSTND,64)
     64 format(/,'********',T12,'PAUCATUCK EASTERN PEQUOT RES (8204)',&
          &' BEING MAPPED TO WHITE MOUNTAIN NF (922) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 2
  case (8206)
     write(JOSTND,65)
     65 format(/,'********',T12,'NARRAGANSETT RESERVATION (8206) ',&
          &'BEING MAPPED TO WHITE MOUNTAIN NF (922) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 2
  case (8208)
     write(JOSTND,66)
     66 format(/,'********',T12,'WAMPANOAG-AQUINNAH TL (8208) BEING ',&
          &'MAPPED TO WHITE MOUNTAIN NF (922) FOR FURTHER PROCESSING.')
     IFOR = 2
  case (8209)
     write(JOSTND,67)
     67 format(/,'********',T12,'AROOSTOOK BAND OF MICMAC TL (8209) ',&
          &'BEING MAPPED TO WHITE MOUNTAIN NF (922) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 2
  case (8211)
     write(JOSTND,68)
     68 format(/,'********',T12,'MOHEGAN RESERVATION (8211) BEING ',&
          &'MAPPED TO WHITE MOUNTAIN NF (922) FOR FURTHER PROCESSING.')
     IFOR = 2
  case (8214)
     write(JOSTND,69)
     69 format(/,'********',T12,'CAYUGA NATION TDSA (8214) BEING ',&
          &'MAPPED TO FINGER LAKES NF (930) FOR FURTHER PROCESSING.')
     IFOR = 7
  case (8215)
     write(JOSTND,70)
     70 format(/,'********',T12,'ONONDAGA NATION RESERVATION (8215) ',&
          &'BEING MAPPED TO FINGER LAKES NF (930) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 7
  case (8216)
     write(JOSTND,71)
     71 format(/,'********',T12,'TONAWANDA RESERVATION (8216) BEING ',&
          &'MAPPED TO FINGER LAKES NF (930) FOR FURTHER PROCESSING.')
     IFOR = 7
  case (8217)
     write(JOSTND,72)
     72 format(/,'********',T12,'TUSCARORA NATION RESERVATION (8217)',&
          &' BEING MAPPED TO FINGER LAKES NF (930) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 7
  case (8218)
     write(JOSTND,73)
     73 format(/,'********',T12,'ONEIDA NATION RESERVATION (8218) ',&
          &'BEING MAPPED TO FINGER LAKES NF (930) FOR FURTHER ',&
          &'PROCESSING.')
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
  case (6)
     write(JOSTND,21)
     21 format(/,'********',T12,'WAYNE-HOOSIER NF (911) BEING ',&
          &'MAPPED TO WAYNE (914) FOR FURTHER PROCESSING.')
     IFOR = 1
  case (7)
     write(JOSTND,22)
     22 format(/,'********',T12,'FINGER LAKES NF (930) BEING MAPPED ',&
          &'TO GREEN MTN-FINGER LAKES (920) FOR FURTHER PROCESSING.')
     IFOR = 4
  end select

  !  SET DEFAULT TLAT, TLONG, AND ELEVATION VALUES, BY FOREST

  select case(IFOR)
  case(1)
     if (TLAT.eq.0) TLAT = 39.33
     if (TLONG.eq.0)TLONG = 82.10
     if (ELEV.eq.0) ELEV = 9.
  case(3)
     if (TLAT.eq.0) TLAT = 41.84
     if (TLONG.eq.0)TLONG = 79.15
     if (ELEV.eq.0) ELEV = 17.
  case(4)
     if (TLAT.eq.0) TLAT = 43.61
     if (TLONG.eq.0)TLONG = 72.97
     if (ELEV.eq.0) ELEV = 19.
  case(5)
     if (TLAT.eq.0) TLAT = 38.93
     if (TLONG.eq.0)TLONG = 79.85
     if (ELEV.eq.0) ELEV = 30.
  case(2)
     if (TLAT.eq.0) TLAT = 43.53
     if (TLONG.eq.0)TLONG = 71.47
     if (ELEV.eq.0) ELEV = 20.
  end select
  !     SET THE IGL VARIABLE ONLY IF DEFAULT FOREST IS USED
  !     GEOGRAPHIC LOCATION CODE: 1=NORTH, 2=CENTRAL, 3=SOUTH
  !     USED TO SET SOME EQUATIONS IN REGENERATION AND PERHAPS
  !     HEIGHT-DIAMETER IN DIFFERENT VARIANTS.
  if (USEIGL) IGL = KFOR(IFOR)

  KODFOR = JFOR(IFOR)
  return
end
