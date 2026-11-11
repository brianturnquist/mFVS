!=================================!
!   Find user defined variables   !
!=================================!
! BASE $Id$
subroutine EVKEY (CTOK,NUM,IRC)
  implicit none

  !     CALLED FROM ALGCMP

  !     FINDS CTOK IN THE ARRAY OF USER-DEFINED VARIABLES.

  !     N.L.CROOKSTON - APR 87 - FORESTRY SCIENCES LAB - MOSCOW, ID

  !     CTOK  = C*8 TOKEN FOUND IN AN EXPRESSION.
  !     NUM   = THE LOAD OP-CODE FOR THE TOKEN, IF IT IS DEFINED.
  !     IRC   = RETURN CODE, 0=CTOK WAS FOUND, NUM IS DEFINED.
  !             1=CTOK WAS NOT FOUND, NUM IS UNDEFINED.

  ! Common blocks
  include 'PRGPRM.F77'
  include 'OPCOM.F77'

  ! Variables
  integer :: IRC,NUM,I
  character(len=8) :: CTOK

  !     IF THERE ARE NO USER DEFINED VARIABLES, THEN BRANCH TO EXIT

  if (ITST5.le.0) goto 110

  !     SEARCH THROUGH LIST OF USER DEFINED VARILABLES.

  do I = 1,ITST5
     NUM = I
     if (CTOK.eq.CTSTV5(I)) goto 20
     10 continue
  end do
  NUM = 0
  goto 110
  20 continue
  NUM = 500+NUM
  IRC = 0
  return
  110 continue
  IRC = 1
  return
end subroutine EVKEY
