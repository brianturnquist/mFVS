!================!
!   rd : rdsum   !
!================!
! RD $Id$
subroutine RDSUM(N,A1,A2,IYEND)
  implicit none

  !  SUBROUTINE TO SUM UP THE ARRAY PROBI(1350,41)

  !  CALLED BY :
  !     RDBBDO  [ROOT DISEASE]
  !     RDEND   [ROOT DISEASE]
  !     RDESTB  [ROOT DISEASE]
  !     RDGROW  [ROOT DISEASE]
  !     RDINF   [ROOT DISEASE]
  !     RDINSD  [ROOT DISEASE]
  !     RDMN2   [ROOT DISEASE]
  !     RDMORT  [ROOT DISEASE]
  !     RDSETP  [ROOT DISEASE]
  !     RDSTR   [ROOT DISEASE]
  !     RDTREG  [ROOT DISEASE]

  !  CALLS     :
  !     NONE

  !  PARAMETERS :
  !     N      - NUMBER OF TREE RECORDS TO SUM UP OVER
  !     A1     - ARRAY TO SUM UP INTO
  !     A2     - ARRAY TO BE SUMMED UP
  !     IYEND  - NUMBER OF YEARS TO SUM UP OVER

  !  Revision History :
  !   11/23/93 - Last revision date.
  !   09/03/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  ! Common blocks
  include 'RDPARM.f90'

  ! Variables
  integer :: I,IYEND,J,N
  real    :: A1(IRRTRE),A2(IRRTRE,41,2)

  if (N .le.0) return

  do I = 1,N
     A1(I) = 0.0

     do J = 1,IYEND
        A1(I) = A1(I) + A2(I,J,1) + A2(I,J,2)
        900 continue
     end do
     1000 continue
  end do

  return
end
