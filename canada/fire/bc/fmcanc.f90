!=============================!
!   canada/fire/bc : fmcanc   !
!=============================!
! CANADA-FIRE-BC $Id$
subroutine FMCANC (IYR,FMOIS,CFTMP,CANBURN,ROS,INTSTY,FCLS)
  implicit none

  !     SINGLE-STAND VERSION
  !     CALLED FROM: FMPOFL
  !                  FMBURN

  !  PURPOSE:  CALCULATES THE CROWN FIRES USING THE CANADIAN METHOD

  !  CALL LIST DEFINITIONS:
  !     FMOIS    FIRE TYPE BEING CALCULATED
  !     CFTMP    LABEL FOR TYPE OF FIRE
  !     OINIT1   TORCHING INDEX
  !     OACT1    CROWNING INDEX
  !     CANBURN  FLAG FOR WHETHER CROWNING IS EVEN POSSIBLE

  !  LOCAL VARIABLE DEFINITIONS:
  !     CAC       CRITERION FOR ACTIVE CROWNING
  !     CCR       CRITICAL MINIMUM RATE OF SPREAD FOR ACTIVE CROWNING
  !     CFB       CROWN FRACTION BURNED
  !     CROSA     PREDICTED CROWN FIRE SPERAD RATE
  !     INTSTY    INTENSITY - MAY BE EQUIVALENT TO FUEL CONSUMPTION
  !     USEMOIS   FUEL MOISTURE: EFFM, WHICH EQUATES TO 1HR DEAD FUEL MOISTURE
  !     ROSP      PASSIVE CROWN FIRE SPREAD

  !  COMMON BLOCK VARIABLES AND PARAMETERS:
  !     ACTCBH   ACTUAL CROWN BASE HEIGHT (FT) (-1 means that there isn't one)
  !     CBD      CROWN BULK DENSITY (METRIC UNITS)

  !     PARAMETER STATEMENTS.

  !     PARAMETER INCLUDE FILES.
  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'

  !     COMMON INCLUDE FILES.
  include 'PLOT.f90'
  include 'FMCOM.f90'
  include 'FMFCOM.f90'
  include 'CONTRL.f90'
  include 'METRIC.f90'

  !     VARIABLE DECLARATIONS.

  ! Variables
  integer :: IYR,FMOIS,CANBURN
  real    :: OINIT1(3),OACT1(3)
  character(len=8) :: CFTMP
  logical :: DEBUG
  integer :: FCLS
  real    :: CCR,CFB
  real    :: CROSA
  real    :: CAC
  real    :: ROSP,ROS
  real    :: MWIND,USEMOIS
  real    :: INTSTY,TOTLOAD
  real    :: TA2KGM2

  call DBCHK (DEBUG,'FMCANC',6,ICYC)
  if (DEBUG) write(JOSTND,7) ICYC,FMOIS,CBD
  7 format(' ENTERING FMCANC CYCLE=',I3,' FMOIS=',I2,' CBD=',F7.3)
  TA2KGM2 = (0.90718 / 0.4046945) * (1000. / 10000.)

  OINIT1(FMOIS) = -1.0
  OACT1(FMOIS) = -1.0
  FCLS = 0
  INTSTY = 0.0

  !     IF THE CROWN IS VERY SPARSE, THEN CAN HAVE NO CROWN FIRE, SO IGNORE
  !     THIS IS DIRECTLY FROM FMCFIR

  if (CBD .le.0.0.or. CANBURN .eq.0) then
     CFTMP = 'SURFACE'
     CRBURN = 0.0
     OINIT1(FMOIS) = -1.0
     OACT1(FMOIS) = -1.0
     RFINAL = ROS
     return
  end if

  !     FROM EMAIL FROM MIGUEL CRUZ

  !     MWIND IS METRIC WIND - M/S INSTEAD OF MILES/HOUR
  MWIND = FWIND * MItoKM * 1000. / (60. * 60.)
  !     EMAIL SAYS "FUEL MOISTURE": WE WILL USE 1 HR FUELS
  USEMOIS = MOIS(1,1)

  !     CHANGE LOADING TO METRIC
  TOTLOAD = (TCLOAD + SMALL + LARGE) * FT3pACRtoM3pHA

  CCR = 3. / CBD
  CROSA = 11.021*(MWIND*3.6)**0.8966 * CBD**0.1901 *&
       &Exp(-0.1714* USEMOIS)
  CAC = CROSA / CCR
  ROSP = CROSA*Exp(-CAC)

  if (CAC .gt.1.0) then
     CFTMP = "ACTIVE"
     !           (NOTE FROM STEVE: THE ROSA IN THE EMAIL SHOULD BE CROSA)
     RFINAL = CROSA * MtoFT
     FIRTYPE = 1
     CRBURN = 1.0
     !        INTSTY =  18600*CROSA* TOTLOAD
     !       calculate intensity from surface fuel and crown consumption instead of load
     INTSTY = 300*CROSA*  (CFIM_INPUT(25) + TCLOAD*TA2KGM2*CRBURN)
  else
     CFTMP = "PASSIVE"
     RFINAL = ROSP  * MtoFT
     FIRTYPE = 2
     CFB = 1 - EXP(-.23*(CROSA - (ROS*FTtoM)))
     CRBURN = CFB
     !        INTSTY =  18600*ROSP* TOTLOAD
     INTSTY = 300*ROSP* (CFIM_INPUT(25) + TCLOAD*TA2KGM2*CRBURN)
  end if
  !     Change from KJ/min/m to Kw/m
  !      INTSTY = INTSTY / 60       !now done by changing 18600 to 300

  !     CALCULATE INTENSITY CLASS
  if (INTSTY .lt.10.0) then
     FCLS = 1
  else if (INTSTY .lt.500.0) then
     FCLS = 2
  else if (INTSTY .lt.2000.0) then
     FCLS = 3
  else if (INTSTY .lt.4000.0) then
     FCLS = 4
  else if (INTSTY .lt.10000.0) then
     FCLS = 5
  else
     FCLS = 6
  end if

  if (DEBUG) write(JOSTND,*) 'CAC = ',CAC
  if (DEBUG) write(JOSTND,*) 'CROSA = ',CROSA
  if (DEBUG) write(JOSTND,*) 'CRBURN = ',CRBURN

  if (DEBUG) write(JOSTND,8) FMOIS,CFTMP,RFINAL
  8 format(' FMCFIR FMOIS=',I2,' CFTMP=',A,&
       &' RFINAL=',F13.3)

  return
end
