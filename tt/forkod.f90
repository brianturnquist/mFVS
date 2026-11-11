!============================!
!   Translate forest codes   !
!============================!
! TT $Id$
subroutine FORKOD
  implicit none

  !     TRANSLATES FOREST CODE INTO A SUBSCRIPT, IFOR, AND IF
  !     KODFOR IS ZERO, THE ROUTINE RETURNS THE DEFAULT CODE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'

  !  NATIONAL FORESTS:
  !  403 = BRIDGER
  !  405 = CARIBOU
  !  415 = TARGHEE
  !  416 = TETON

  !  7306 = WIND RIVER RESERVATION  (MAPPED TO 403 BRIDGER)
  !  8107 = FORT HALL RESERVATION   (MAPPED TO 405 CARIBOU)

  ! Variables
  integer :: JFOR(4),KFOR(4),NUMFOR,I
  logical :: USEIGL,FORFOUND
  data JFOR/403,405,415,416/,NUMFOR/4/

  USEIGL = .true.
  FORFOUND = .false.

  select case (KODFOR)
     !       CROSSWALK FOR RESERVATION PSUEDO CODES & LOCATION CODE
  case (7306)
     write(JOSTND,61)
     61 format(/,'********',T12,'WIND RIVER RESERVATION (7306) ',&
          &'BEING MAPPED TO BRIDGER NF (403) FOR FURTHER PROCESSING.')
     IFOR = 1
  case (8107)
     write(JOSTND,62)
     62 format(/,'********',T12,'FORT HALL RESERVATION (8107) BEING ',&
          &'MAPPED TO CARIBOU NF (405) FOR FURTHER PROCESSING.')
     IFOR = 2
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

  !     FOREST MAPPING CORRECTION  {N/A FOR THIS VARIANT}
  !      SELECT CASE (IFOR)
  !      END SELECT

  !     SET THE IGL VARIABLE ONLY IF DEFAULT FOREST IS USED
  !     GEOGRAPHIC LOCATION CODE: 1=NORTH, 2=CENTRAL, 3=SOUTH
  !     USED TO SET SOME EQUATIONS IN REGENERATION AND PERHAPS
  !     HEIGHT-DIAMETER IN DIFFERENT VARIANTS.
  if (USEIGL) IGL = KFOR(IFOR)

  KODFOR = JFOR(IFOR)
  return
end
