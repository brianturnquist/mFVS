!===========================!
!   volume : fvssierralog   !
!===========================!
! VOLUME $Id$

subroutine FVSSIERRALOG(VN,VM,VMAX,ISPC,D,H,BARK,LCONE,CTKFLG)
  implicit none
  !  This routine

  ! Common blocks
  include 'PRGPRM.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'

  ! Variables
  real    :: VN,VM,VMAX,D,H,BARK,ALVN,BEHRE,STUMP
  real    :: DMRCH,HTMRCH,S3,VOLM,VOLT
  logical :: LCONE,CTKFLG
  integer :: ISPC

  !  WESTERN SIERRA LOG RULES

  go to (801,802,803,804,805,806,807,808,809,810,811,803),ISPC

  !  OTHER CONIFER

  801 ALVN = -6.5819 + 2.0022*ALOG(D) + 1.0598*ALOG(H)
  VN = EXP(ALVN)
  go to 820

  !  SUGAR PINE

  802 ALVN = -6.0657 + 2.1126*ALOG(D) + 0.8635*ALOG(H)
  VN = EXP(ALVN)
  go to 820

  !  DOUGLAS-FIR

  803 ALVN = -6.5807 + 1.8332*ALOG(D) + 1.1602*ALOG(H)
  VN = EXP(ALVN)
  go to 820

  !  WHITE FIR

  804 ALVN = -6.3998 + 1.9203*ALOG(D) + 1.0802*ALOG(H)
  VN = EXP(ALVN)
  go to 820

  !  MADRONE

  805 continue
  VN = 0.006732  *(D**1.96628)*(H**0.83458)
  go to 820

  !  INCENSE CEDAR

  806 ALVN = -5.8157 + 1.9023*ALOG(D) + 0.9114*ALOG(H)
  VN = EXP(ALVN)
  go to 820

  !  BLACK OAK

  807 VN = 0.00705381*(D**1.97437)*(H**0.85034)
  go to 820

  ! TANOAK

  808 continue
  VN = 0.00588700*(D**1.94165)*(H**0.86562)
  go to 820

  ! RED FIR

  809 ALVN = -6.4929 + 1.9271*ALOG(D) + 1.0884*ALOG(H)
  VN = EXP(ALVN)
  go to 820

  !  PONDEROSA PINE.

  810 ALVN = -6.5819 + 2.0022*ALOG(D) + 1.0598*ALOG(H)
  VN = EXP(ALVN)
  go to 820

  ! TANOAK

  811 VN = 0.00588700*(D**1.94165)*(H**0.86562)

  !  NEGATIVE VOLUMES ARE RESET TO ZERO.

  820 continue
  if (VN .lt.0.) VN = 0.0

  ! SET VMAX.

  VMAX = VN

  ! COMPUTE MERCHANTABLE CUBIC FOOT VOLUME.

  CTKFLG = .true.
  if (VN .eq.0.) then
     VM = 0.
     CTKFLG = .false.
     go to 850
  end if
  call BEHPRM (VMAX,D,H,BARK,LCONE)
  VOLT = BEHRE(0.0,1.0)

  !  COMPUTE MERCHANTABLE CUBIC VOLUME USING TOP DIAMETER, MINIMUM
  !  DBH, AND STUMP HEIGHT SPECIFIED BY THE USER.

  STUMP = 1. - (STMP(ISPC)/H)
  if (D.lt.DBHMIN(ISPC).or.D.lt.TOPD(ISPC)) then
     VM = 0.0
  else
     DMRCH = TOPD(ISPC)/D
     HTMRCH = ((BHAT*DMRCH)/(1.0-(AHAT*DMRCH)))
     if (.not.LCONE) then
        VOLM = BEHRE(HTMRCH,STUMP)
        VM = VMAX*VOLM/VOLT
     else

        !       PROCESS CONES.

        S3 = STUMP**3
        VOLM = S3-HTMRCH**3
        VM = VMAX*VOLM
     end if
  end if
  850 continue
  return
end
