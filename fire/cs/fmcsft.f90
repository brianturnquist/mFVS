!======================!
!   fire/cs : fmcsft   !
!======================!
! FIRE-CS $Id$
subroutine FMSNFT(IFFEFT)
  implicit none

  !  OBSOLETE: REPLACED BY fire/base/fmsnft.f90. RENAMED FROM FMCSFT ONLY SO
  !  THE CS BUILD LINKS UNTIL ITS SOURCE LIST IS SWITCHED; THEN DELETE.

  !  SINGLE-STAND VERSION
  !  CALLED FROM: FMCBA, FMCFMD
  !  PURPOSE:
  !     THIS SUBROUTINES CALCULATES A CATEGORICAL FOREST TYPE BASED ON
  !     SN WORKSHOP INPUT.  THIS FOREST TYPE IS USED IN SETTING DEFAULT
  !     SURFACE FUEL LEVELS AND IN THE FUEL MODEL LOGIC

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'FMCOM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90' ! since using FIA algorithm info

  !     LOCAL VARIABLE DECLARATIONS

  ! Variables
  integer :: IFFEFT,I
  real    :: PINEBA,NPINEBA,X

  !  ROUTINE BEGINS.
  !  DETERMINE FFE FOREST TYPE (1 OF 9 CATEGORIES) FROM FIA FOR. TYPE

  IFFEFT = 0
  select case (IFORTP)
  case (997, 504, 505, 510, 512, 515, 519, 520)
     IFFEFT = 1 ! hardwood
  case (103, 104, 141, 142, 161:168, 996,401, 403:407, 409)
     PINEBA = 0
     NPINEBA = 0
     do I = 1,ITRN
        X = FMPROB(I) * DBH(I) * DBH(I) * 0.0054542
        select case (ISP(I))
        case (3:7) ! pine
           PINEBA = PINEBA + X
        case default ! non-pine
           NPINEBA = NPINEBA + X
        end select
     end do
     if ((PINEBA+NPINEBA) .gt.0) then
        if ((PINEBA/(PINEBA+NPINEBA)) .le.0.50) then
           IFFEFT = 2 ! hardwood/pine
        else if ((PINEBA/(PINEBA+NPINEBA)) .le.0.70) then
           IFFEFT = 3 ! pine/hardwood
        else
           IFFEFT = 4 ! pine
        end if
     end if
     if (IFORTP .eq.162) then
        if ((ATAVH .gt.50) .and. (ISTCL .ge.3)) then
           IFFEFT = 5 ! pine bluestem
        end if
     end if
  case (501, 503)
     if ((ATAVH .gt.30) .and. (ISTCL .ge.3)) then
        IFFEFT = 6 ! oak savannah
     else
        IFFEFT = 1 ! hardwood
     end if
  case (181, 402)
     IFFEFT = 7 ! eastern redcedar
  case (602, 605, 701, 706, 708, 807)
     IFFEFT = 8 ! saint francis types
  case (999)
     IFFEFT = 9 ! nonstocked
  case default
     IFFEFT = 1 ! hardwood
  end select

  return
end
