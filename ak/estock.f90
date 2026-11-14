!===============================================!
!   Compute regeneration stocking probability   !
!===============================================!
! AK $Id$
subroutine ESTOCK (ELEV,IFT,XBA,XTPA,PN,DEBUG)
  implicit none

  !     CALCULATES PROBABILITY OF STOCKING FOR REGENERATION MODEL.
  !     COEFFICIENTS FOR PLANTING ARE NOT INCLUDED IN EQUATIONS.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ESCOMN.f90'
  include 'ESCOM2.f90'
  include 'CONTRL.f90'

  !  VARIABLE DEFINITIONS:

  !  IFT      -- STAND FOREST TYPE CATEGORY WHERE:
  !                1 = 122
  !                2 = 125
  !                3 = 270
  !                4 = 271
  !                5 = 281
  !                6 = 301
  !                7 = 304
  !                8 = 305
  !                9 = 703
  !               10 = 901
  !               11 = 902
  !               12 = 904 (MAPPED TO 703)
  !               13 = 911
  !               14 = OTHER (MAPPED TO 122)
  !  XBA      -- PLOT BASAL AREA PER ACRE >= REGNBK
  !  XPTA     -- PLOT TREES PER ACRE >= REGNBK
  !  XQMD     -- PLOT LEVEL QMD >= REGNBK
  !  SLO      -- PLOT SLOPE AS PROPORTION (ESCOMN.F77)
  !  ELEV     -- STAND ELEVATION (100S OF FEET)
  !  ELEVSQ   -- STAND ELEVATION (100S OF FEET) SQUARED (ESCOMN.F77)
  !  XCOS     -- COSINE(ASPECT IN RADIANS) * SLOPE (ESCOMN.F77)
  !  PNFT     -- FOREST TYPE COEFFICIENT (B1)
  !  PNSBA    -- BASAL AREA COEFFICIENT (B2)
  !  PNQMD    -- QMD COEFFICIENT (B3)
  !  PNSLO    -- SLOPE COEFFICIENT (B4)
  !  PNEVEL   -- ELEVATION COEFFICIENT (B5)
  !  PNELEVSQ -- ELEVATION SQUARED COEFFICIENT (B6)
  !  PNXCOS   -- COS(ASPECT)*SLOPE COEFFICIENT (B7)
  !  PNBAQMD  -- BASAL AREA AND QMD INTERACTION COEFFICIENT (B8)
  !  PN       -- USED IN DETERMINING PROBABILITY OF STOCKING

  ! Variables
  real    :: PNFT(14),PNELEV(14),PNXCOS(14)
  real    :: PNSLO,PNELEVSQ,PNSBA,PNQMD,PNBAQMD
  real    :: B1,B2,B3,B4,B5,B6,B7,B8,PN
  real    :: ELEV,ADJSLO,ADJXC,XBA,XTPA,XQMD
  integer :: IFT
  logical :: DEBUG

  data PNFT / &
       & 1.428325, 3.401549, 1.906730, 3.481250, 3.172262, &
       & 2.816007, 4.073819, 1.255094, 0.357560, 2.756703, &
       & 1.471673, 1.428325, 1.554008, 1.428325 /

  data PNSBA / -0.00212/

  data PNQMD / -0.194748/

  data PNSLO / -0.002364/

  data PNELEV / &
       & 0.022431, -0.045237, 0.036110, 0.006615, 0.017794, &
       & 0.072023,  0.012058, 0.066790, 0.135631, 0.012600, &
       & 0.032712,  0.022431, 0.037521, 0.022431/

  data PNELEVSQ / -0.000742 /

  data PNXCOS / &
       & 0.006748,  0.000011,  0.000738, -0.000432,  -0.000715, &
       & -0.002044,  0.000694, -0.003398, -0.013222,   0.003788, &
       & -0.009518,  0.006748, -0.001041,  0.006748/

  data PNBAQMD / 0.000316 /

  !      SET COEFFICIENTS FOR COMPUTING PN
  !      LP,YC,RC FOREST TYPES: USE WESTERN HEMLOCK FOREST TYPE
  !      COEFFICIENTS.

  !      B1 = PNFT(IFT)
  !      B2 = PNBA
  !      B3 = PNQMD
  !      B4 = PNSLO
  !      B5 = PNELEV
  !      B6 = PNELEVSQ
  !      B7 = PNXCOS
  !      B8 = PNBAQMD

  !     SET COEFFICIENTS FOR COMPUTING PN
  B1 = PNFT(IFT)
  B2 = PNSBA
  B3 = PNQMD
  B4 = PNSLO
  B5 = PNELEV(IFT)
  B6 = PNELEVSQ
  B7 = PNXCOS(IFT)
  B8 = PNBAQMD

  !     CALCULATE PLOT QMD FROM PLOT BA AND TPA
  if (XTPA .gt. 0.0) then
     XQMD = SQRT((XBA/XTPA)/0.005454)
  else
     XQMD = 0.0
  end if

  !     ADJUST PLOT SLO-BASED VARIABLES TO PERCENT SLOPE.
  ADJSLO = SLO*100
  ADJXC = XCOS*100

  !  PREDICT STOCKING PROBABILITY

  !  PN = B1 + B2 * XBA + B3 * XQMD + B4 * ADJSLO + B5 * ELEV +
  !  B6 * ELEVSQ + B7 * ADJXC + B8 * XQMD*XBA

  !  VARIABLES DEFINED IN SECTION ABOVE
  !  PN IS CONVERTED TO PROBABILITY OF STOCKING IN ESTAB.F BY
  !  EXP(PN)/(1 + EXP(PN))

  !     PREDICT STOCKING PROBABILITY
  PN = B1 + B2 * XBA + B3 * XQMD + B4 * ADJSLO + B5 * ELEV + &
       & B6 * ELEVSQ + B7 * ADJXC + B8 * XBA * XQMD

  if (DEBUG)write(JOSTND,*)'IN ESTOCK CHECKING VALUES',' XBA=', &
       & XBA,' XTPA=',XTPA,' XQMD=',XQMD,' ADJSLO=',ADJSLO,' ELEV=',ELEV, &
       & ' ELEVSQ=',ELEVSQ,' ADJXC=',ADJXC,' PN=',PN

  return
end subroutine ESTOCK
