!=================!
!   rd : rdspl2   !
!=================!
! RD $Id$
subroutine RDSPL2(RCEN,IDI,DIAM,RTD)
  implicit none

  !  CREATES NEW DISEASE CENTERS FOR SPORE INFECTED
  !  STUMPS THAT ARE OUTSIDE OF EXISTING CENTERS AND DON'T CHANGE
  !  DISEASE TYPE.

  !  CALLED BY :
  !     RDSPOR  [ROOT DISEASE]

  !  CALLS     :
  !     RDRANN  (FUNCTION)  [ROOT DISEASE]

  !  PARAMETERS :
  !     RCEN   -
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
  integer :: IDI,II,JJ,NCEN,NTRY
  real    :: DIAM,DIS,R,R1,R2,R3,RCEN,RDRANN,REMCEN,RMX,RTD,XC,XDIF,YC,YDIF

  if (DIAM .lt. SPDBH(IDI)) goto 300

  DIMEN = SQRT(SAREA) * 208.7

  !     GET NUMBER OF CENTERS TO CREATE.

  NCEN = IFIX(RCEN)
  REMCEN = RCEN - NCEN
  R = RDRANN(0)
  if (R .lt. REMCEN) NCEN = NCEN + 1

  if (NCEN .lt.1) return

  do II = 1,NCEN

     !        KEEP TRYING UNTIL THE CENTER IS LOCATED OUTSIDE.

     NTRY = 0

     380 continue
     NTRY = NTRY + 1

     if (NTRY .gt.20) goto 550

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

     goto 380

     600 continue

     !        STUMP IS OUTSIDE OF OWN TYPE'S CENTERS
     !        SO ADD IT TO THE PATCH LIST.

     if (NCENTS(IDI) .ge.100) goto 550

     NCENTS(IDI) = NCENTS(IDI) + 1
     ICENSP(IDI,NCENTS(IDI)) = 1
     JCENSP(IDI,NCENTS(IDI)) = IYEAR
     PCENTS(IDI,NCENTS(IDI),1) = XC
     PCENTS(IDI,NCENTS(IDI),2) = YC

     !        PATCH SIZE (RADIUS) IS 50% OF STUMP ROOT RADIUS

     PCENTS(IDI,NCENTS(IDI),3) = 0.5 * RTD

     goto 350

     550 continue

     !        ADD PATCH AREA TO LAST PATCH IF THERE ARE ALREADY 100 CENTERS

     R1 = PCENTS(IDI,NCENTS(IDI),3)
     R2 = (0.5 * RTD)
     R3 = SQRT((R1 * R1) + (R2 * R2))
     RMX = 2 * DIMEN
     if (R3.lt. RMX) PCENTS(IDI,NCENTS(IDI),3) = R3
     350 continue
  end do

  300 continue

  return
end
