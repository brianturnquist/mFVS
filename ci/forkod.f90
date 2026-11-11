!============================!
!   Translate forest codes   !
!============================!
subroutine FORKOD
  implicit none

  !  **FORKOD--CI   DATE OF LAST REVISION:  06/20/11

  !     TRANSLATES FOREST CODE INTO A SUBSCRIPT, IFOR, AND IF
  !     KODFOR IS ZERO, THE ROUTINE RETURNS THE DEFAULT CODE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'

  !  NATIONAL FORESTS:
  !  117 = NEZ PERCE
  !  402 = BOISE
  !  406 = CHALLIS
  !  412 = PAYETTE
  !  413 = SALMON
  !  414 = SAWTOOTH

  !  RESERVATION PSUEDO CODES:
  !  7721 = DUCK VALLEY RESERVATION   (MAPPED TO 402 BOISE)
  !  8107 = FORT HALL RESERVATION     (MAPPED TO 414 SAWTOOTH)

  ! Variables
  integer :: JFOR(6),KFOR(6),NUMFOR,I
  logical :: USEIGL,FORFOUND

  !  DATA STATEMENTS

  data JFOR/117,402,406,412,413,414/, NUMFOR /6/
  data KFOR/1,2,2,3,2,2 /

  USEIGL = .true.
  FORFOUND = .false.

  select case (KODFOR)

     !       CROSSWALK FOR RESERVATION PSUEDO CODES & LOCATION CODE
  case (7721)
     write(JOSTND,70)
     70 format(/,'********',T12,'DUCK VALLEY RESERVATION (7721) ',&
          &'BEING MAPPED TO BOISE NF (402) FOR FURTHER PROCESSING.')
     IFOR = 3
  case (8107)
     write(JOSTND,72)
     72 format(/,'********',T12,'FORT HALL RESERVATION (8107) ',&
          &'BEING MAPPED TO SAWTOOTH NF (414) FOR FURTHER PROCESSING.')
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

  !     FOREST MAPPING CORRECTION {NONE FOR THIS VARIANT}
  !     SELECT CASE (IFOR)
  !     END SELECT

  !     SET THE IGL VARIABLE ONLY IF DEFAULT FOREST IS USED
  !     GEOGRAPHIC LOCATION CODE: 1=NORTH, 2=CENTRAL, 3=SOUTH
  !     USED TO SET SOME EQUATIONS IN REGENERATION AND PERHAPS
  !     HEIGHT-DIAMETER IN DIFFERENT VARIANTS.
  if (USEIGL) IGL = KFOR(IFOR)

  KODFOR = JFOR(IFOR)
  return
end
