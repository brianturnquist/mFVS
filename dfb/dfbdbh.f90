!==================!
!   dfb : dfbdbh   !
!==================!
! DFB $Id$
subroutine DFBDBH
  implicit none

  !  INITIALIZATION ROUTINE FOR DFB MODEL.  INITIALIZES THE START ARRAY.

  !  CALLED BY :
  !     DFBDRV  [DFB]

  !  CALLS :
  !     DFBIND  (SUBROUTINE)   [DFB]

  !  LOCAL VARIABLES :
  !     I,J    - COUNTER INDEXES.
  !     I1,I2  - INDEXES FOR THE ARRAY IND1.  I1 IS THE STARTING
  !              INDEX FOR A SPECIES IN ARRAY IND1 (DF = IDFSPC).
  !              I2 IS THE ENDING INDEX FOR A SPECIES IN ARRAY IND1
  !              (DF = IDFSPC).
  !     INDEX  - HOLDS DBH CLASS (1-10) OF THE TREE RECORD.

  !  COMMON BLOCK VARIABLES USED :
  !     DBH    - (ARRAYS)   INPUT
  !     IDFSPC - (DFBCOM)   INPUT
  !     IND1   - (ARRAYS)   INPUT
  !     ISCT   - (CONTRL)   INPUT
  !     PROB   - (ARRAYS)   INPUT
  !     START  - (DFBCOM)   OUTPUT

  ! Common blocks
  include 'PRGPRM.f90'
  include 'DFBCOM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'

  ! Variables
  integer :: I,I1,I2,J,I3

  !.... INITIALIZE START ARRAY

  do I = 1,20
     START(I) = 0.0
     100 continue
  end do

  !.... DETERMINE NUMBER OF TREES PER DBH CLASS IN THE STAND

  I1 = ISCT(IDFSPC,1)
  if (I1.eq.0) goto 400
  I2 = ISCT(IDFSPC,2)

  do I = I1,I2
     J = IND1(I)

     !....    LOAD THE START ARRAY WITH THE NUMBER OF TREES PER ACRE THAT
     !....    THIS TREE RECORD REPRESENTS.

     call DFBIND (DBH(J),I3)
     START(I3) = START(I3) + PROB(J)
     300 continue
  end do

  400 continue

  return
end
