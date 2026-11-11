!===========================!
!   organon : growth_mods   !
!===========================!
! ORGANON $Id$

subroutine GG_MODS(TAGE,GWDG,GWHG,DGMOD,HGMOD)
  implicit none
  ! Variables
  real*4  :: TAGE,GWDG,GWHG,DGMOD,HGMOD,XGWHG,XGWDG,A1,A2,B1,B2,A,B
  XGWHG = GWHG
  if (GWHG .gt.20.0) XGWHG = 20.0
  if (GWHG.lt.0.0) XGWHG = 0.0
  XGWDG = GWDG
  if (GWDG .gt.20.0) XGWDG = 20.0
  if (GWDG.lt.0.0) XGWDG = 0.0

  !     SET THE PARAMETERS FOR THE DIAMETER GROWTH MODIFIER

  A1 = 0.0101054 ! VALUE FOR TAGE = 5
  A2 = 0.0031 ! VALUE FOR TAGE => 10
  if (TAGE .le.5.0) then
     A = A1
  else if (TAGE .gt.5.0.and. TAGE .lt.10.0) then
     A = A1-(A1-A2)*((TAGE-5.0)/5.0)
  else
     A = A2
  end if

  !     SET THE PARAMETERS FOR THE HEIGHT GROWTH MODIFIER

  B1 = 0.0062770 ! VALUE FOR TAGE = 5
  B2 = 0.0036 ! VALUE FOR TAGE => 10
  if (TAGE .le.5.0) then
     B = B1
  else if (TAGE .gt.5.0.and. TAGE .lt.10.0) then
     B = B1-(B1-B2)*((TAGE-5.0)/5.0)
  else
     B = B2
  end if

  !     GENETIC GAIN DIAMETER GROWTH RATE MODIFIER

  DGMOD = 1.0+A*XGWDG

  !     GENETIC GAIN HEIGHT GROWTH RATE MODIFIER

  HGMOD = 1.0+B*XGWHG
  return
end

subroutine SNC_MODS(FR,DGMOD,HGMOD)
  implicit none
  ! Variables
  real*4  :: FR,DGMOD,HGMOD,XFR,A0,A1,A2,B0,B1,B2
  XFR = FR
  if (FR .gt.4.0) XFR = 4.0
  if (FR.lt.0.85) XFR = 0.85

  !     SET THE PARAMETERS FOR THE DIAMETER GROWTH MODIFIER

  A0 = -1.02300676
  A1 = -0.11705018
  A2 = 3.0

  !     SET THE PARAMETERS FOR THE HEIGHT GROWTH MODIFIER

  B0 = -1.80656927
  B1 = -0.08555130
  B2 = 3.0

  !     SWISS NEEDLE CAST DIAMETER GROWTH RATE MODIFIER

  DGMOD = 1.0-EXP(A0+A1*XFR**A2)

  !     SWISS NEEDLE CAST HEIGHT GROWTH RATE MODIFIER

  HGMOD = 1.0-EXP(B0+B1*XFR**B2)
  return
end
