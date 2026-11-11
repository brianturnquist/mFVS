!============================!
!   Translate forest codes   !
!============================!
! SO $Id$
subroutine FORKOD
  implicit none

  !     TRANSLATES FOREST CODE INTO A SUBSCRIPT, IFOR, AND IF
  !     KODFOR IS ZERO, THE ROUTINE RETURNS THE DEFAULT CODE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'

  !  NATIONAL FORESTS:
  !  601 = DESCHUTES
  !  602 = FREMONT
  !  620 = WINEMA
  !  505 = KLAMATH
  !  506 = LASSEN
  !  509 = MODOC
  !  511 = PLUMAS
  !  701 = INDUSTRY LANDS
  !  702 = INDUSTRY LANDS
  !         **702 is new code introduced to eliminate
  !           REDUNDANCY ACROSS VARIANTS. 702 WILL BE
  !           MAPPED TO 701 FOR PROCESSING
  !  514 = SHASTA-TRINITY (MAPPED TO KLAMATH)
  !  799 = WARM SPRINGS INDIAN RESERVATION

  !  RESERVATION PSUEDO CODES:
  !  7711 = FORT MCDERMITT RESERVATION  (MAPPED TO 602 FREMONT)
  !  7836 = FORT BIDWELL RESERVATION    (MAPPED TO 509 MODOC)
  !  7838 = SUSANVILLE INDIAN RANCHERIA (MAPPED TO 506 LASSEN)
  !  7844 = CEDARVILLE RANCHERIA        (MAPPED TO 509 MODOC)

  ! Variables
  integer :: JFOR(11),KFOR(11),NUMFOR,I
  logical :: USEIGL,FORFOUND
  data JFOR/601,602,620,505,506,509,511,701,514,799,&
       &702/
  data NUMFOR /11/
  data KFOR/11*1/

  USEIGL = .true.
  FORFOUND = .false.

  select case (KODFOR)
     !       CROSSWALK FOR RESERVATION PSUEDO CODES & LOCATION CODE
  case (7711)
     write(JOSTND,61)
     61 format(/,'********',T12,'FORT MCDERMITT RESERVATION (7711) ',&
          &'BEING MAPPED TO FREMONT (602) FOR FURTHER PROCESSING.')
     IFOR = 2
  case (7836)
     write(JOSTND,62)
     62 format(/,'********',T12,'FORT BIDWELL RESERVATION (7836) ',&
          &'BEING MAPPED TO MODOC (509) FOR FURTHER PROCESSING.')
     IFOR = 6
  case (7838)
     write(JOSTND,63)
     63 format(/,'********',T12,'SUSANVILLE INDIAN RANCHERIA (7838) ',&
          &'BEING MAPPED TO LASSEN (506) FOR FURTHER PROCESSING.')
     IFOR = 5
  case (7844)
     write(JOSTND,64)
     64 format(/,'********',T12,'CEDARVILLE RANCHERIA (7844) BEING ',&
          &'MAPPED TO MODOC (509) FOR FURTHER PROCESSING.')
     IFOR = 6
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
  case (9)
     write(JOSTND,21)
     21 format(/,'********',T12,'SHASTA NF (514) BEING MAPPED TO ',&
          &'KLAMATH (505) FOR FURTHER PROCESSING.')
     IFOR = 4
  case (11)
     write(JOSTND,22)
     22 format(/,'********',T12,'INDUSTRY LANDS (702) IS BEING ',&
          &'MAPPED TO (701) FOR FURTHER PROCESSING.')
     IFOR = 8
  end select

  !     SET THE IGL VARIABLE ONLY IF DEFAULT FOREST IS USED
  !     GEOGRAPHIC LOCATION CODE: 1=NORTH, 2=CENTRAL, 3=SOUTH
  !     USED TO SET SOME EQUATIONS IN REGENERATION AND PERHAPS
  !     HEIGHT-DIAMETER IN DIFFERENT VARIANTS.
  if (USEIGL) IGL = KFOR(IFOR)

  KODFOR = JFOR(IFOR)
  return
end
