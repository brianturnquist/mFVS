!===================================!
!   Sort activities within cycles   !
!===================================!
! BASE $Id$
subroutine OPCSET (ICYC)
  implicit none

  !     OPTION PROCESSING ROUTINE - NL CROOKSTON - JAN 1981 - MOSCOW

  !     SETS UP IOPCYC AS AN WITHIN CYCLE SORT INTO ASSENDING ORDER
  !     OF ACTIVITY NUMBER.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'OPCOM.f90'

  ! Variables
  integer :: ICYC,I

  !     IF IMG1 IS ZERO (THERE ARE NO ACTIVITIES FOR THE CYCLE);
  !     THEN: RETURN

  IMG1 = IMGPTS(ICYC,1)
  if (IMG1 .eq. 0) return
  IMG2 = IMGPTS(ICYC,2)

  !     COPY THIS CYCLE'S PORTION OF THE ALL CYCLE SORT ARRAY INTO
  !     THE WITHIN CYCLE SORT ARRAY

  do I = IMG1,IMG2
     IOPCYC(I) = IOPSRT(I)
     10 continue
  end do

  !     SORT IOPCYC ON ASSENDING ACTIVITY NUMBERS.

  call IAPSRT(IMG2-IMG1+1,IACT,IOPCYC(IMG1),.false.)
  return
end subroutine OPCSET
