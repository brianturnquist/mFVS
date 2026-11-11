!=============================!
!   volume : fvsbrucedemars   !
!=============================!
! VOLUME $Id$

subroutine FVSBRUCEDEMARS(VN,VM,VMAX,D,H,ISPC,BARK,LCONE,CTKFLG)
  implicit none
  !  This routine calculates volumes for the AK variant using
  !  the Bruce and Demars method METHC = 8
  !  called from **FVSVOL

  ! Common blocks
  include 'PRGPRM.f90'

  !      INCLUDE 'ARRAYS.F77'

  include 'COEFFS.f90'
  include 'CONTRL.f90'

  ! Variables
  real    :: VMAX,BARK,H,D,VM,VN,BBF,VVN,VOLT
  real    :: DMRCH,HTMRCH,VOLM,S3,STUMP
  real    :: BEHRE
  integer :: ISPC
  logical :: LCONE,CTKFLG

  VVN = 0.0
  BBF = 0.0
  if (H .le.4.5) go to 50
  if (D .le.3.5.or. H .lt.18.0) go to 10
  if (D .ge.9.0.and. H .gt.40.0)go to 30
  call FVSOLDSEC(ISPC,VVN,D,H)
  go to 50
  10 call FVSOLDFST (ISPC,VVN,D,H)
  go to 50
  30 call FVSOLDGRO(ISPC,VVN,D,H,BBF)
  50 continue
  VN = VVN
  VMAX = VVN

  !  COMPUTE MERCHANTABLE CUBIC VOLUME USING TOP DIAMETER, MINIMUM
  !  DBH, AND STUMP HEIGHT SPECIFIED BY THE USER.

  if (VN .eq.0.) then
     VM = 0.
     CTKFLG = .false.
     go to 60
  end if
  call BEHPRM (VMAX,D,H,BARK,LCONE)
  VOLT = BEHRE(0.0,1.0)
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
  CTKFLG = .true.
  60 continue
  return
end
