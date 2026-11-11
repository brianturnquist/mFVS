!======================!
!   fire/ne : fmneft   !
!======================!
! FIRE-NE $Id$
subroutine FMNEFT(IFFEFT)
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
  case (101:105)
     IFFEFT = 1 ! white / red / jack pine (includes eastern hemlock)
  case (121:127)
     IFFEFT = 2 ! spruce fir, (includes tamarck, n. white cedar)
  case (161:168)
     IFFEFT = 3 ! loblolly - shortleaf pine
  case (381:383)
     IFFEFT = 4 ! exotic softwoods
  case (401:409)
     IFFEFT = 5 ! oak - pine
  case (501:520)
     IFFEFT = 6 ! oak - hickory
  case (601:608)
     IFFEFT = 7 ! oak - gum - cypress
  case (701:709)
     IFFEFT = 8 ! elm - ash - cottonwood
  case (801:809)
     IFFEFT = 9 ! maple-beech-birch
  case (901:904)
     IFFEFT = 10 ! aspen-birch
  case (999)
     IFFEFT = 11 ! nonstocked
  case default
     IFFEFT = 2 ! spruce - fir
  end select

  return
end
