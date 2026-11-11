!=======================================!
!   Estimate tree age and site height   !
!=======================================!
! CANADA-ON $Id$
subroutine FINDAG(I,ISPC,D1,D2,H,SITAGE,SITHT,AGMAX,HTMAX1,HTMAX2,&
     &DEBUG)
  implicit none

  !  THIS ROUTINE FINDS EFFECTIVE TREE AGE
  !  CALLED FROM **COMCUP
  !  CALLED FROM **CRATET

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'

  !  DECLARATIONS

  ! Variables
  logical :: DEBUG
  integer :: I,ISPC,MODE0,IVAR
  real    :: D1,D2,SITAGE,SITHT,AGMAX,HTMAX1,HTMAX2
  real    :: HTG1,H,YRS,HTMAX,AGET

  HTG1 = 0.

  !   CALL HTCALC TO CALCULATE AGE BASED ON INITIAL TREE HEIGHT

  MODE0 = 0
  IVAR = 1
  YRS = 10.
  call HTCALC (MODE0,IVAR,ISPC,SITEAR(ISPC),YRS,H,AGET,HTMAX,&
       &HTG1,JOSTND,DEBUG)

  !  IF H >= HTMAX THEN CALCULATE AGE BASED ON HTMAX - 1. FT.
  !  THE FORMULA FOR AGE IS UNSTABLE FOR HEIGHTS >= HTMAX.

  if (HTMAX-H.le.1.) then
     H = HTMAX - 1.1
     call HTCALC (MODE0,IVAR,ISPC,SITEAR(ISPC),YRS,H,AGET,HTMAX,&
          &HTG1,JOSTND,DEBUG)
  end if

  SITAGE = AGET

  if (DEBUG)write(JOSTND,91200)I,ISPC,AGET,H
  91200 format(' FINDAG - I,ISPC,AGE,H ',2I5,2F10.2)

  if (DEBUG)write(JOSTND,50)
  50 format(' LEAVING SUBROUTINE FINDAG')

  return
end
  !**END OF CODE SEGMENT
