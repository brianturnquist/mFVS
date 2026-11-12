!===================================!
!   Write the model output header   !
!===================================!
! VBASE $Id$
subroutine GROHED (IUNIT)
  use FVSVERSION
  implicit none

  !     WRITES HEADER FOR BASE MODEL PORTION OF PROGNOSIS SYSTEM

  !     THE TITLE TEXT IS SELECTED BY VARIANT (AND, IN CR, BY MODEL TYPE).
  !     TITLES ARE WRITTEN EXACTLY AS IN THE FORMER PER-VARIANT ROUTINES,
  !     INCLUDING TRAILING BLANKS, BECAUSE THE RV: FIELD IS TAB-POSITIONED.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'

  ! Variables
  character(len=10) :: DAT
  character(len=10) :: REV
  character(len=32) :: SVN
  character(len=8) :: TIM
  character(len=:), allocatable :: TITLE
  character(len=:), allocatable :: INDENT

  integer :: IUNIT

  !     CALL REVISE TO GET THE LATEST REVISION DATE FOR THIS VARIANT.

  call REVISE (VARACD,REV)

  !     CALL THE DATE AND TIME ROUTINE FOR THE HEADING.

  call GRDTIM (DAT,TIM)
  SVN = FVSVER_VERSION

  !     ONTARIO USES ITS OWN LAYOUT, WHICH ALSO SHOWS THE VARIANT CODE.

  if (VARACD .eq. 'ON') then
     write (IUNIT,30) VARACD,SVN,REV,DAT,TIM
     return
  end if

  INDENT = ''
  select case (VARACD)
  case ('AK')
     TITLE = ' -- ALASKA '
  case ('BC')
     TITLE = ' -- BC '
  case ('BM')
     TITLE = ' -- BLUE MOUNTAINS PROGNOSIS'
  case ('CA')
     TITLE = ' -- INLAND CALIFORNIA / SOUTHERN CASCADES'
  case ('CR')
     select case (IMODTY)
     case (1)
        TITLE = ' -- CEN. ROCKIES SW MIXED CONIFERS GENGYM'
     case (2)
        TITLE = ' -- CEN. ROCKIES SW PONDEROSA PINE GENGYM'
     case (3)
        TITLE = ' -- CEN. ROCKIES BLACK HILLS/NEBR GENGYM'
     case (4)
        TITLE = ' -- CEN. ROCKIES SPRUCE-FIR GENGYM        '
     case (5)
        TITLE = ' -- CEN. ROCKIES LODGEPOLE PINE GENGYM    '
     case default
        TITLE = ' -- CENTRAL ROCKIES                         '
     end select
  case ('CI')
     TITLE = ' -- CENTRAL IDAHO PROGNOSIS'
  case ('CS')
     TITLE = ' -- CENTRAL STATES'
  case ('EC')
     TITLE = ' -- EAST CASCADES PROGNOSIS '
  case ('EM')
     TITLE = ' -- EASTERN MONTANA PROGNOSIS'
  case ('IE')
     TITLE = ' -- INLAND EMPIRE EXPANDED '
  case ('KT')
     TITLE = ' -- KOOTENAI, KANIKSU, TALLY LAKE PROGNOSIS'
  case ('LS')
     TITLE = ' -- LAKE STATES'
  case ('NC')
     TITLE = ' -- KLAMATH MOUNTAINS PROGNOSIS'
  case ('NE')
     TITLE = ' -- NORTHEAST'
  case ('OC')
     TITLE = ' ORGANON SWO'
  case ('OP')
     TITLE = ' � ORGANON NWO&SMC'
  case ('PN')
     TITLE = ' -- PACIFIC NW COAST PROGNOSIS'
  case ('SN')
     TITLE = ' -- SOUTHERN U.S. PROGNOSIS'
  case ('SO')
     TITLE = ' -- S. CEN. OREGON/N.E. CALIF. PROGNOSIS'
  case ('TT')
     TITLE = ' -- EASTERN IDAHO - WESTERN WYOMING PROGNOSIS'
  case ('UT')
     TITLE = ' -- UTAH PROGNOSIS'
  case ('WC')
     TITLE = ' -- WESTSIDE CASCADES PROGNOSIS'
  case ('WS')
     INDENT = '    '
     TITLE = ' -- WEST SIDE SIERRA NEVADA PROGNOSIS'
  case default
     TITLE = ' -- '//VARACD
  end select

  write (IUNIT,40) INDENT,SVN,TITLE,REV,DAT,TIM

  30 format (//T6,'FOREST VEGETATION SIMULATOR',&
       &5X,'VERSION ',A,' (',A,') -- ONTARIO',&
       &T98,'RV:',A,T111,A,2X,A)
  40 format (//T6,A,'FOREST VEGETATION SIMULATOR',&
       &5X,'VERSION ',A,A,&
       &T97,'RV:',A,T112,A,2X,A)
  return
end subroutine GROHED
