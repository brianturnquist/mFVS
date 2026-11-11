!============================!
!   Translate forest codes   !
!============================!
subroutine FORKOD
  implicit none

  !  **FORKOD--BM   DATE OF LAST REVISION:  02/20/08

  !     TRANSLATES FOREST CODE INTO A SUBSCRIPT, IFOR, AND IF
  !     KODFOR IS ZERO, THE ROUTINE RETURNS THE DEFAULT CODE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'

  !  NATIONAL FORESTS:
  !  604 = MALHEUR
  !  607 = OCHOCO
  !  614 = UMATILLA
  !  616 = WALLOWA-WHITMAN
  !  619 = WHITMAN                         (MAPPED TO WALLOWA-WHITMAN)

  !  RESERVATION PSUEDO CODES:
  !  8117 = UMATILLA RESERVATION           (MAPPED TO UMATILLA  614)

  ! Variables
  integer :: JFOR(7),KFOR(7),NUMFOR,I
  logical :: USEIGL,FORFOUND
  data JFOR/604,607,614,616,619, 0, 0 /, NUMFOR /5/
  data KFOR/1,1,1,1,1,1,1 /

  USEIGL = .true.
  FORFOUND = .false.

  select case (KODFOR)

     !       CROSSWALK FOR RESERVATION PSUEDO CODES & LOCATION CODE
  case (8117)
     write(JOSTND,60)
     60 format(/,'********',T12,'UMATILLA RESERVATION (8117)',&
          &' BEING MAPPED TO UMATILLA NF (614) FOR FURTHER PROCESSING!')
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
     if ( .not. FORFOUND ) then
        call ERRGRO (.true.,3)
        write(JOSTND,11) JFOR(IFOR)
        11 format(/,'********',T12,'FOREST CODE USED IN THIS ',&
             &'PROJECTION IS',I4)
     end if

  end select

  !     FOREST MAPPING CORRECTION
  select case (IFOR)
  case (5)
     write(JOSTND,21)
     21 format(/,'********',T12,'WHITMAN NF (619) BEING MAPPED TO ',&
          &'WALLOWA-WHITMAN (616) FOR FURTHER PROCESSING.')
     IFOR = 4
  end select

  !     SET THE IGL VARIABLE ONLY IF DEFAULT FOREST IS USED.
  !     GEOGRAPHIC LOCATION CODE: 1=NORTH, 2=CENTRAL, 3=SOUTH
  !     USED TO SET SOME EQUATIONS IN REGENERATION AND PERHAPS
  !     HEIGHT-DIAMETER IN DIFFERENT VARIANTS.
  if (USEIGL) IGL = KFOR(IFOR)

  KODFOR = JFOR(IFOR)
  return
end
