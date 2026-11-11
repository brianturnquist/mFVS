!==================================!
!   Find debugging request names   !
!==================================!
! BASE $Id$
subroutine DBSCAN (LDEBG,SUBIN,NC,ICYC)
  implicit none

  ! Common blocks
  include 'DBSTK.f90'

  ! Variables
  integer :: ICYC,NC,IB1,IS,IE,IP,IQ
  character(len=*) :: SUBIN
  character(len=20) :: CHECK
  logical :: LDEBG

  LDEBG = .false.
  if ((NC.gt.0).and.(NC.le.MAXLEN).and.(ITOP.gt.0)) then
     CHECK(1:NC) = SUBIN(1:NC)
     IB1 = NC+1
     CHECK(IB1:IB1) = ' '
     IB1 = NC+4
     CHECK(IB1:IB1) = ' '
     IS = NC+2
     IE = NC+3
     call CH2NUM (CHECK(IS:IE),ICYC)
     IE = IE+1
     IP = INDEX(SUBNAM,CHECK(1:IE))
     call CH2NUM (CHECK(IS:IE),0)
     IQ = INDEX(SUBNAM,CHECK(1:IE))
     if ((IP.gt.0).or.(IQ.gt.0)) LDEBG = .true.
  end if
  return
end subroutine DBSCAN
