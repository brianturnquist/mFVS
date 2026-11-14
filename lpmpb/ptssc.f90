!===================!
!   lpmpb : ptssc   !
!===================!
! LPMPB $Id$
subroutine PTSSC(IPLTNO)
  implicit none

  !     LIBRARY ROUTINE TO SET SCALES

  ! Revision History
  !   02/08/88 Last noted revision date.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  ! Common blocks
  include 'PT.f90'

  ! Variables
  integer :: IVR,case,FNSH1
  integer :: LOG,TRUE,FALSE
  integer :: IPLTNO,START,FINSH,J
  real    :: AMAX,AMIN,DVLN(3),DVLN2(3),PTBNO,PTSRG,RANGE,RT,T,TEMP,UPFCT(3),VR,MXFCT(3)

  external PTBNO

  !        DIVIDING LINE DATA
  data  DVLN/2.0,0.5,0.0/
  data  DVLN2/3.,1.,.33333/
  data MXFCT/1.3333,2.,4./
  data UPFCT/.75,.5,.25/
  data  TRUE/-1/, FALSE/0/

  START = 1

  !     DETERMINE GROUP WITH SAME SCALES
  1 FINSH = START
  4 if (IPTCOD(IPLTNO,FINSH) .ne.100) go to 3
  FINSH = FINSH+1
  go to 4
  3 continue
  !             HERE START IS BEGINNING OF GROUP
  !                     FINSH  END OF GROUP

  if (IPTCOD(IPLTNO,FINSH) .eq.11) go to 5
  !                     IPTCOD=011  BOTH UPPER/LOWER USER SPEC

  !------------USER HAS NOT SPECIFIED BOTH SCALES

  !             GET MAX OF MAX, MIN OF MINS FOR THIS GROUP
  AMAX = -1E35
  AMIN = +1E35
  do J = START,FINSH
     if (PTMAX(IPLTNO,J) .gt. AMAX) AMAX = PTMAX(IPLTNO,J)
     if (PTMIN(IPLTNO,J) .lt. AMIN) AMIN = PTMIN(IPLTNO,J)
     10 continue
  end do

  !             WHAT SCALES NEED BE CACULATED
  if (IPTCOD(IPLTNO,FINSH) .eq.0) go to 100
  if (IPTCOD(IPLTNO,FINSH) .eq.10) go to 200
  go to 300

  !------------SET BOTH SCALES-----------------
  100 if (AMIN .le..999*AMAX) go to 110

  !        AMAX EQUALS AMIN
  if (AMAX .ne.0.) go to 101
  PTL(IPLTNO,FINSH) = 0.
  PTU(IPLTNO,FINSH) = 1.
  go to 5

  101 if (AMAX .le.0.) go to 102
  PTL(IPLTNO,FINSH) = 0.
  go to 200

  102 PTU(IPLTNO,FINSH) = 0.
  go to 300

  !        AMAX GREATER THAN AMIN
  110 if (AMIN .lt.0..and. AMAX .gt.0.) go to 120

  !     AMAX,AMIN THE SAME SIGN
  LOG = FALSE
  if (AMAX .gt.0.) go to 111
  LOG = TRUE
  TEMP = AMAX
  AMAX = -AMIN
  AMIN = -TEMP
  111 if (AMAX .le.2.*AMIN) go to 113

  !     CHOOSE 0. AS LOWER SCALE
  PTL(IPLTNO,FINSH) = 0.
  PTU(IPLTNO,FINSH) = PTSRG(AMAX,PTBNO(AMAX))
  go to 112

  !     CHOOSE SCALES WITH 0. OFF SCALE
  113 T = PTBNO(AMAX-AMIN)
  IVR = AMAX/T
  VR = IVR
  TEMP = VR*T
  if (TEMP .le. AMAX) TEMP = TEMP+T
  PTL(IPLTNO,FINSH) = TEMP-PTSRG(TEMP-AMIN,T)
  PTU(IPLTNO,FINSH) = TEMP

  112 if (LOG .eq. FALSE) go to 5
  TEMP = PTU(IPLTNO,FINSH)
  PTU(IPLTNO,FINSH) = -PTL(IPLTNO,FINSH)
  PTL(IPLTNO,FINSH) = -TEMP
  go to 5

  !     AMAX,AMIN SIGNS DIFFER
  120 RT = -AMAX/AMIN
  do case = 1,3
     if (RT .gt. DVLN(case)) go to 140
     130 continue
  end do
  140 J = 4-case
  if (RT .le. DVLN2(case)) go to 141
  T = AMAX*MXFCT(case)
  go to 142
  141 T = -AMIN*MXFCT(J)
  142 TEMP = PTSRG(T,PTBNO(T))
  PTU(IPLTNO,FINSH) = UPFCT(case)*TEMP
  PTL(IPLTNO,FINSH) = -UPFCT(J)*TEMP
  go to 5

  !------------USER SPECIFIED LOWER SCALE, SET UPPER
  200 if (AMAX.gt.0..or. AMAX.lt..50*PTL(IPLTNO,FINSH))go to 201

  !             CHOOSE 0. AS UPPER SCALE
  PTU(IPLTNO,FINSH) = 0.
  go to 5

  201 RANGE = AMAX-PTL(IPLTNO,FINSH)

  !             CHOOSE UPPER SCALE AS REQUIRED
  PTU(IPLTNO,FINSH) = PTL(IPLTNO,FINSH)+PTSRG(RANGE,PTBNO(RANGE))
  go to 5

  !--------USER SPECIFIED UPPER SCALE, SET LOWER--------
  300 if (AMIN.lt.0..or. AMIN.gt..50*PTU(IPLTNO,FINSH))go to 301

  !             CHOOSE 0. AS LOWER SCALE
  PTL(IPLTNO,FINSH) = 0.
  go to 5

  301 RANGE = PTU(IPLTNO,FINSH)-AMIN

  !             CHOOSE LOWER SCALE AS REQUIRED
  PTL(IPLTNO,FINSH) = PTU(IPLTNO,FINSH)-PTSRG(RANGE,PTBNO(RANGE))

  !-----STORE SCALES
  5 if (FINSH .eq. START) go to 8
  FNSH1 = FINSH-1
  do J = START,FNSH1
     PTU(IPLTNO,J) = PTU(IPLTNO,FINSH)
     7 PTL(IPLTNO,J) = PTL(IPLTNO,FINSH)
  end do

  !             HAVE ALL SCALES BEEN SET
  8 if (FINSH .eq. IPTVAR(IPLTNO)) return
  START = FINSH+1
  go to 1
end
