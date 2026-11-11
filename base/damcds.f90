!============================!
!   Read tree damage codes   !
!============================!
! BASE $Id$
subroutine DAMCDS (II,ICODES)
  implicit none

  !     II    = TREE POINTER FOR LIVE TREES
  !     ICODES= DISEASE AND DAMAGE CODES ARRAY

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  ! Variables
  integer :: I,II
  integer :: ICODES(6)

  !  CALL SUBROUTINE TO PROCESS DAMAGE CODES FOR BASE MODEL (SPECIAL
  !  TREE STATUS CODES, PERCENT DEFECT).

  call BASDAM (II,ICODES)

  !  STORE DAMAGE CODES FOR LATER PEST EXTENSION PROCESSING.

  do I = 1,6
     DAMSEV(I,II) = ICODES(I)
  end do

  return
end subroutine DAMCDS
