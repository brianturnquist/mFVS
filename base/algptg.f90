!====================================!
!   Compile point group references   !
!====================================!
! BASE $Id$
subroutine ALGPTG(CTOK,LEN,NUM,IRC)
  implicit none

  !     CALLED FROM ALGKEY

  !     CONVERTS CTOK TO A SPECIES CODE LOAD OPERATION.

  !     CTOK  = C*10 TOKEN FOUND IN AN EXPRESSION.
  !     LEN   = THE LENGTH OF THE TOKEN.
  !     NUM   = THE LOAD OP-CODE FOR THE SPECIES.
  !     IRC   = RETURN CODE, 0=CTOK WAS FOUND, NUM IS DEFINED.
  !             1=CTOK WAS NOT FOUND, NUM IS UNDEFINED.

  ! Common blocks
  include 'PRGPRM.F77'
  include 'CONTRL.F77'
  include 'PLOT.F77'

  ! Variables
  character(len=20) :: CTOK
  character(len=20) :: CTEMP
  integer :: IRC,NUM,LEN,I

  !     IF CTOK IS A 1 CHAR TOKEN, THEN RIGHT JUSTIFY.  OTHERWISE
  !     SIMPLY LOAD CTOK INTO CTEMP.

  CTEMP(1:20) = ''
  if (LEN.eq.1) then
     CTEMP(1:1) = ' '
     CTEMP(2:2) = CTOK(1:1)
  else
     CTEMP = CTOK
  end if

  !     SEARCH TRHOUGH POINT GROUP CODES.

  do I = 1,NPTGRP
     if (CTOK(1:LEN).eq.PTGNAME(I)) then
        NUM = -(I+2000)
        IRC = 0
        goto 100
     end if
     955 continue
  end do

  IRC = 1
  return
  100 continue
  IRC = 0
  return
end subroutine ALGPTG
