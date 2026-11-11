!===================================!
!   Write the model output header   !
!===================================!
! CR $Id$
subroutine GROHED (IUNIT)
  use FVSVERSION
  implicit none

  !     WRITES HEADER FOR BASE MODEL PORTION OF PROGNOSIS SYSTEM

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'

  ! Variables
  integer :: IUNIT
  character(len=10) :: DAT
  character(len=8) :: TIM
  character(len=10) :: REV
  character(len=32) :: SVN

  !     CALL REVISE TO GET THE LATEST REVISION DATE FOR THIS VARIANT.

  call REVISE (VARACD,REV)

  !     CALL THE DATE AND TIME ROUTINE FOR THE HEADING.

  call GRDTIM (DAT,TIM)
  SVN = FVSVER_VERSION

  ! BRANCH TO APPROPRIATE MODEL TYPE

  select case (IMODTY)

     ! SOUTHWEST MIXED CONIFER TYPE             **

  case (1)
     write (IUNIT,140) SVN,REV,DAT,TIM
     140 format (//T6,'FOREST VEGETATION SIMULATOR',&
          &5X,'VERSION ',A,' -- CEN. ROCKIES SW MIXED CONIFERS GENGYM',&
          &T97,'RV:',A,T112,A,2X,A)

     ! SOUTHWEST PONDEROSA PINE TYPE              **

  case (2)
     write (IUNIT,240) SVN,REV,DAT,TIM
     240 format (//T6,'FOREST VEGETATION SIMULATOR',&
          &5X,'VERSION ',A,' -- CEN. ROCKIES SW PONDEROSA PINE GENGYM',&
          &T97,'RV:',A,T112,A,2X,A)

     ! BLACK HILLS PONDEROSA PINE TYPE           **

  case (3)
     write (IUNIT,340) SVN,REV,DAT,TIM
     340 format (//T6,'FOREST VEGETATION SIMULATOR',&
          &5X,'VERSION ',A,' -- CEN. ROCKIES BLACK HILLS/NEBR GENGYM',&
          &T97,'RV:',A,T112,A,2X,A)

     ! SPRUCE-FIR TYPE                         **

  case (4)
     write (IUNIT,440) SVN,REV,DAT,TIM
     440 format (//T6,'FOREST VEGETATION SIMULATOR',&
          &5X,'VERSION ',A,' -- CEN. ROCKIES SPRUCE-FIR GENGYM        ',&
          &T97,'RV:',A,T112,A,2X,A)

     ! LODGEPOLE PINE TYPE                     **

  case (5)
     write (IUNIT,540) SVN,REV,DAT,TIM
     540 format (//T6,'FOREST VEGETATION SIMULATOR',&
          &5X,'VERSION ',A,' -- CEN. ROCKIES LODGEPOLE PINE GENGYM    ',&
          &T97,'RV:',A,T112,A,2X,A)

     ! MODEL TYPE 0 -- HASN'T BEEN SET YET (AT BEGINNING OF KEYWORD LISTING)
     ! ALSO FOR FUTURE MODEL TYPES INCLUDING ASPEN     **

  case default
     write (IUNIT,50) SVN,REV,DAT,TIM
     50 format (//T6,'FOREST VEGETATION SIMULATOR',&
          &5X,'VERSION ',A,' -- CENTRAL ROCKIES                         ',&
          &T97,'RV:',A,T112,A,2X,A)

  end select

  return
end
