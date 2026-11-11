!================================!
!   Compile species references   !
!================================!
! BASE $Id$
subroutine ALGSPP (CTOK,LEN,NUM,IRC)
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

  !     SEARCH THROUGH SPECIES CODES.

  do I = 1,MAXSP
     if (CTEMP.eq.NSP(I,1)(1:2)) then
        NUM = -I
        goto 100
     end if
     10 continue
  end do

  !     SEARCH TRHOUGH SPECIES GROUP CODES.

  do I = 1,NSPGRP
     if (CTOK(1:LEN).eq.NAMGRP(I)) then
        NUM = -(I+1000)
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
end subroutine ALGSPP
