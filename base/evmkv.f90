!============================!
!   Create event variables   !
!============================!
! BASE $Id$
subroutine EVMKV(CTOK)
  implicit none

  !     CREATES A NEW VARIABLE, IF IT CAN!

  ! Common blocks
  include 'PRGPRM.f90'
  include 'OPCOM.f90'

  ! Variables
  character(len=*) :: CTOK

  if (ITST5.lt.MXTST5) then
     ITST5 = ITST5+1
     CTSTV5(ITST5) = CTOK
     LTSTV5(ITST5) = .false.
     LEVUSE = .true.
  else
     call ERRGRO (.true.,10)
     call ERRGRO (.true.,12)
     write (16,*) 'ISSUED IN EVMKV'
  end if
  return
end subroutine EVMKV
