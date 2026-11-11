!======================!
!   fire/ls : fmlsft   !
!======================!
! FIRE-LS $Id$
subroutine FMLSFT(IFFEFT)
  implicit none

  !  SINGLE-STAND VERSION
  !  CALLED FROM: FMCBA
  !  PURPOSE:
  !     THIS SUBROUTINES CALCULATES A CATEGORICAL FOREST TYPE BASED ON
  !     FIA FOREST TYPE.  THIS FOREST TYPE IS USED IN SETTING DEFAULT
  !     SURFACE FUEL LEVELS.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'FMCOM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90' ! since using FIA algorithm info

  !     LOCAL VARIABLE DECLARATIONS

  ! Variables
  integer :: IFFEFT

  !  ROUTINE BEGINS.
  !  DETERMINE FFE FOREST TYPE (1 OF 10 CATEGORIES) FROM FIA FOR. TYPE

  IFFEFT = 0
  select case (IFORTP)
  case (102:105,381)
     IFFEFT = 1 ! white / red pine (includes eastern hemlock)
  case (101)
     IFFEFT = 2 ! jack pine
  case (121:127)
     IFFEFT = 3 ! spruce fir, (includes tamarck, n. white cedar)
  case (181)
     IFFEFT = 4 ! eastern redcedar
  case (401:409)
     IFFEFT = 5 ! oak - pine
  case (501:520)
     IFFEFT = 6 ! oak - hickory
  case (701:709)
     IFFEFT = 7 ! elm - ash - cottonwood
  case (801:809)
     IFFEFT = 8 ! maple-beech-birch
  case (901:904)
     IFFEFT = 9 ! aspen-birch
  case (999)
     IFFEFT = 10 ! nonstocked
  case default
     IFFEFT = 1 ! white / red pine
  end select

  return
end
