!=====================================!
!   Canadian snag-volume regressions   !
!=====================================!
program SNAG_VOLUME_CASES
  implicit none
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'FMCOM.f90'
  include 'CONTRL.f90'
  include 'COEFFS.f90'

  integer :: CASE_NO,MODE,IHT,NCASE
  real :: D,H,XHT,VOL,TCF,MCF,SCF,VMAX,BARK,EXPECTED
  logical :: LCONE

  VARACD = VARIANTCD
  ICYC = 0
  JOSTND = 6
  LFIANVB = .false.
  METHC = 0
  DBHMIN = 5.0
  TOPD = 4.0
  STMP = 1.0
  ! Neither Canadian provider supplies a separate sawtimber-cubic result.
  ! Poisoning its unused standards catches accidental entry into that branch.
  SCFTOPD = -100.0
  SCFSTMP = -100.0
  NCASE = 0

  do CASE_NO = 1,4
     if (VARACD.eq.'BC'.and.CASE_NO.eq.4) cycle
     D = 12.0
     H = 80.0
     XHT = 40.0
     if (CASE_NO.eq.2) XHT = H
     if (CASE_NO.eq.3) XHT = -1.0
     if (CASE_NO.eq.4) then
        ! ON OCFVOL's below-minimum positive placeholders caused the trap:
        ! its top diameter exceeds this tree's DBH, outside the taper domain.
        METHC = 8
        D = 0.1
        H = 4.7
        XHT = 4.0
     end if
     SPS(1) = 1
     DBHS(1) = D
     HTDEAD(1) = H

     do MODE = 0,2
        TCF = 40.0
        MCF = 30.0
        if (CASE_NO.eq.4) then
           TCF = 0.0001
           MCF = 0.0
        end if
        SCF = 0.0
        VMAX = TCF
        BARK = 0.9
        LCONE = .false.
        IHT = int(XHT*100.0)
        if (XHT.gt.-1.0) call CFTOPK(1,D,H,TCF,MCF,SCF,VMAX,LCONE,BARK,IHT)
        EXPECTED = TCF
        if (VARACD.eq.'ON') EXPECTED = max(0.005454154*H,TCF)
        if (MODE.eq.2) EXPECTED = MCF
        VOL = -99.0
        if (MODE.eq.0) then
           call FMSVOL(1,XHT,VOL,.false.,6)
        else
           call FMSVL2(1,D,H,XHT,VOL,0,'D',MODE.eq.2,.false.,6)
        end if
        if (transfer(VOL,0).ne.transfer(EXPECTED,0)) error stop 'snag volume differs'
        if (CASE_NO.eq.3) then
           if (XHT.ne.H) error stop 'negative-height argument not updated'
           XHT = -1.0
        end if
        NCASE = NCASE+1
     end do
  end do
  write(*,*) 'PASS snag volumes ',VARACD,': ',NCASE,' exact entry/height/merchant cases'
end program SNAG_VOLUME_CASES

subroutine CFVOL(ISPC,D,H,D2H,VN,VM,VMAX,TKILL,LCONE,BARK,ITHT,CTKFLG)
  implicit none
  include 'PRGPRM.f90'
  integer :: ISPC,ITHT
  real :: D,H,D2H,VN,VM,VMAX,BARK
  logical :: TKILL,LCONE,CTKFLG
  VN = 40.0
  VM = 30.0
  VMAX = VN
  if (VARIANTCD.eq.'BC') VMAX = 0.0
  CTKFLG = .true.
end subroutine CFVOL

subroutine OCFVOL(VN,VM,ISPC,D,H,TKILL,BARK,ITHT,VMAX,LCONE,CTKFLG,IT)
  implicit none
  integer :: ISPC,ITHT,IT
  real :: D,H,VN,VM,VMAX,BARK
  logical :: TKILL,LCONE,CTKFLG
  VN = 0.0001
  VM = 0.0001
  VMAX = VN
  CTKFLG = .true.
end subroutine OCFVOL

real function BRATIO(ISPC,D,H)
  implicit none
  integer :: ISPC
  real :: D,H
  BRATIO = 0.9
end function BRATIO

subroutine DBCHK(DEBUG,NAME,NCHAR,CYCLE)
  implicit none
  logical :: DEBUG
  character(len=*) :: NAME
  integer :: NCHAR,CYCLE
  DEBUG = .false.
end subroutine DBCHK

subroutine NATCRS
  implicit none
  error stop 'unexpected NATCRS path in Canadian volume-contract fixture'
end subroutine NATCRS
