!===================!
!   lpmpb : ptscl   !
!===================!
! LPMPB $Id$
subroutine PTSCL(IPLTNO,IPTLUN)
  implicit none

  !        ROUTINE TO PRINT SCALES

  ! Revision History
  !   02/08/88 Last noted revision date.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.
  !   08/22/14 Lance R. David (FMSC)
  !     Function name was used as variable name.
  !     changed variable INT to I2

  ! Common blocks
  include 'PT.f90'

  ! Variables
  character(len=1) :: L(10)
  character(len=4) :: IFORM
  character(len=35) :: IFORM4

  integer :: I,I2,IPLTNO,IPTLUN,J,K

  real    :: DIV,S(5),SI,TEMP,XJ

  !        SET UP LINE LENGTH  AND DEFAULT FORMAT

  IFORM4 = '(1X,0PF10.3,4(15X,0PF10.3),1X,10A1)'
  I2 = 5
  DIV = I2-1

  I = 1
  7 SI = (PTU(IPLTNO,I)-PTL(IPLTNO,I))/DIV
  do J = 1,I2
     XJ = J-1
     1 S(J) = PTL(IPLTNO,I)+XJ*SI
  end do
  !             SETS UP 3 OR 5 MARKERS ACROSS TOP

  do J = 1,10
     L(J) = IPTLET(IPLTNO,I)
     I = I+1
     if (I .gt. IPTVAR(IPLTNO)) go to 4
     if (PTL(IPLTNO,I) .ne. PTL(IPLTNO,I-1)) go to 4
     if (PTU(IPLTNO,I) .ne. PTU(IPLTNO,I-1)) go to 4
     2 continue
  end do
  !             LOOP FORCES ALL SIMILAR SCALES INTO
  !                     A SINGLE LINE

  4 TEMP = ABS(PTU(IPLTNO,I-1))
  if (TEMP .eq.0.) go to 5
  if (1..le. TEMP .and. TEMP .lt.100000.) go to 5
  IFORM4 = '(1X,1PE10.3,4(15X,1PE10.3),1X,10A1)'
  5 TEMP = ABS(PTL(IPLTNO,I-1))
  if (TEMP .eq.0.) go to 6
  if (1..le. TEMP .and. TEMP .lt.100000.) go to 6
  IFORM4 = '(1X,1PE10.3,4(15X,1PE10.3),1X,10A1)'

  6 write(IPTLUN,IFORM4) (S(K),K = 1,I2),(L(K),K = 1,J)
  IFORM4 = '(1X,0PF10.3,4(15X,0PF10.3),1X,10A1)'
  if (I .le. IPTVAR(IPLTNO)) go to 7

  return
end
