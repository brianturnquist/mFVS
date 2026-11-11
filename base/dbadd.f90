!============================!
!   Add debugging requests   !
!============================!
! BASE $Id$
subroutine DBADD (SUBIN,NC,ICYC,IRC)
  implicit none

  ! Common blocks
  include 'DBSTK.f90'

  ! Variables
  integer :: IRC,ICYC,NC,IPLEN,I,IS,IE
  character(len=*) :: SUBIN

  IPLEN = ITOP+NC+4
  if (IPLEN.lt.MAXTOP) then
     do I = 1,NC
        ITOP = ITOP+1
        SUBNAM (ITOP:ITOP) = SUBIN(I:I)
        40 continue
     end do
     ITOP = ITOP+1
     SUBNAM(ITOP:ITOP) = ' '
     ITOP = ITOP+3
     SUBNAM(ITOP:ITOP) = ' '
     IS = ITOP-2
     IE = ITOP-1
     call CH2NUM (SUBNAM (IS:IE),ICYC)
     IRC = 0
  else
     IRC = 2
  end if
  return
end subroutine DBADD
