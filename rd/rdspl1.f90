!=================!
!   rd : rdspl1   !
!=================!
! RD $Id$
subroutine RDSPL1(RCROSS,STNEW,IDI,DIAM,RTD)
  implicit none

  !  SUBROUTINE TO CREATE NEW STUMPS AND DISEASE CENTERS FROM
  !  SPORE INFECTED STUMPS THAT HAVE CHANGED DISEASE TYPE

  !  CALLED BY :
  !     RDSPOR  [ROOT DISEASE]

  !  CALLS     :
  !     RDRANN  (FUNCTION)  [ROOT DISEASE]

  !  PARAMETERS :
  !     RCROSS -
  !     STNEW  -
  !     IDI    -
  !     DIAM   -
  !     RTD    -

  !  Revision History :
  !   01/01/91 - Last revision date.
  !   09/03/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'
  include 'CONTRL.f90'
  include 'RDCOM.f90'
  include 'RDADD.f90'

  ! Variables
  integer :: II,IDI,JJ,NCROSS
  real    :: DIAM,R,R1,R2,R3,RCROSS,RDRANN,REMCRO,RMX,RTD,STNEW,XC,YC,XDIF,YDIF,DIS

  STNEW = 0.0
  if (DIAM .lt. SPDBH(IDI)) goto 300

  DIMEN = SQRT(SAREA) * 208.7

  !     GET STUMPS THAT CHANGE DISEASE TYPE

  NCROSS = IFIX(RCROSS)
  REMCRO = RCROSS - NCROSS
  R = RDRANN(0)
  if (R .lt. REMCRO) NCROSS = NCROSS + 1

  if (NCROSS .lt.1) return

  do II = 1,NCROSS

     !        CHECK TO SEE IF STUMP IS INSIDE CENTERS

     XC = RDRANN(0) * DIMEN
     YC = RDRANN(0) * DIMEN
     JJ = 0

     400 continue
     JJ = JJ + 1
     if (JJ .gt. NCENTS(IDI)) goto 600

     XDIF = PCENTS(IDI,JJ,1) - XC
     YDIF = PCENTS(IDI,JJ,2) - YC
     DIS = SQRT((XDIF * XDIF) + (YDIF * YDIF))

     if (DIS .gt. PCENTS(IDI,JJ,3)) goto 400

     !        STUMP IS INSIDE OF OTHER TYPE'S CENTERS.
     !        SO IGNORE IT

     goto 350

     600 continue

     !        STUMP IS OUTSIDE OF OTHER TYPE'S CENTERS
     !        SO IT BECOMES A NEW CENTER

     STNEW = STNEW + 1.0

     !        SO ADD IT TO THE PATCH LIST.

     !        Perhaps IDI should be ID2 (the actual disease type instead of the other) --Sarah

     if (NCENTS(IDI) .ge.100) goto 550

     NCENTS(IDI) = NCENTS(IDI) + 1
     ICENSP(IDI,NCENTS(IDI)) = 1
     PCENTS(IDI,NCENTS(IDI),1) = XC
     PCENTS(IDI,NCENTS(IDI),2) = YC

     !        PATCH SIZE (RADIUS) IS 50% OF STUMP ROOT DIAMETER

     PCENTS(IDI,NCENTS(IDI),3) = 0.25 * RTD

     goto 350

     550 continue

     !        ADD PATCH AREA TO LAST PATCH

     R1 = PCENTS(IDI,NCENTS(IDI),3)
     R2 = (0.25 * RTD)
     R3 = SQRT((R1 * R1) + (R2 * R2))

     !        Should the following line be .5 * DIMEN instead (half of one side of a square stand)?
     !           (If so, also change line in RDSPL2) -- Sarah

     RMX = 2*DIMEN
     if (R3.lt. RMX) PCENTS(IDI,NCENTS(IDI),3) = R3
     350 continue
  end do

  300 continue

  return
end
