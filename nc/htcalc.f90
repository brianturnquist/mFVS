!===============================================!
!   Compute tree age height and height growth   !
!===============================================!
! NC $Id$
subroutine HTCALC(SINDX,ISPC,AG,HGUESS,JOSTND,DEBUG)
  implicit none

  ! THIS ROUTINE CALCULATES A POTENTIAL HT GIVEN AN SPECIES SITE AND AGE
  ! IT IS USED TO CAL POTHTG AND SITE

  ! Variables
  logical :: DEBUG
  integer :: ISPC,JOSTND
  real    :: HGUESS,AG,SINDX,Z,A,B,C,X1,X2

  if (DEBUG)write(JOSTND,15)ISPC,AG,SINDX
  15 format(' ENTERING HTCALC ISPC,AG,SINDX= ',I5,2F10.4)

  ! GO TO A DIFFERENT POTENTIAL HEIGHT CURVE DEPENDING ON THE SPECIES

  select case (ISPC)

     ! DOUGLAS FIR USE KING
     ! OTHER SOFTWOODS USE DOUGLAS-FIR
     ! SPECIES: OS, DF, RW
     ! POTENTIAL HEIGHT GROWTH IS CALCULATED FOR RW
     ! BUT NOT USED.

  case(1,3,12)
     Z = 2500.0/(SINDX - 4.5)
     A = -0.954038 + 0.109757*Z
     B = 0.055818 + 0.0079224*Z
     C = -0.0007338 + 0.0001977*Z
     HGUESS = ((AG*AG)/(A + B*AG + C*AG*AG)) + 4.5
     if (DEBUG)write(JOSTND,*)' HTCALC DF/OS Z,A,B,C,HGUESS= ',&
          &Z,A,B,C,HGUESS

     ! WHITE FIR USE DOLPH
     ! INCENSE CEDAR USE WF
     ! RED FIR USE WF
     ! SPECIES: WF, IC, RF

  case(4,6,9)
     X1 = (38.0202*AG**(-1.05213)*EXP(0.009557*AG))
     X2 = 101.842894*(1.0 - EXP(-0.001442*AG**1.679259))
     HGUESS = (SINDX - 69.91 + X1*X2)/X1 + 4.5
     if (DEBUG)write(JOSTND,*)' HTCALC WF/IC/RF X1,X2,HGUESS= ',&
          &X1,X2,HGUESS

     ! MADRONE USE PORTER AND WIANT
     ! SPECIES: MA

  case(5)
     HGUESS = SINDX/(0.375 + 31.233/AG)
     if (DEBUG)write(JOSTND,*)' HTCALC MA HGUESS= ',HGUESS

     ! BLACK OAK   POWERS
     ! SCALE FACTOR ADDED SO THAT TREES WOULD HIT SITE HEIGHT
     ! SPECIES: BO

  case(7)
     A = SQRT(AG) - SQRT(50.0)
     HGUESS = SINDX*(1.0+0.322*(A))-6.413*A
     HGUESS = HGUESS*.80
     if (DEBUG)write(JOSTND,*)' HTCALC BO A,HGUESS= ',A,HGUESS

     ! TANOAK USE WIANT
     ! SCALE FACTOR ADDED SO THAT TREES WOULD HIT SITE HEIGHT
     ! MISC SPECIES USE TANOAK
     ! SPECIES: TO, OH

  case(8,11)
     HGUESS = SINDX /(0.204 + 39.787/AG)
     HGUESS = HGUESS*0.85
     if (DEBUG)write(JOSTND,*)' HTCALC TO/OH HGUESS= ',HGUESS

     ! PONDEROSA PINE USE POWERS AND OLIVER
     ! SUGAR PINE USE POWERS  PONDEROSA PINE
     ! SPECIES: SP, PP

  case(2,10)
     HGUESS = (1.88*SINDX - 7.178) * (1.0-EXP(-0.025*AG))**&
          &(0.001*SINDX+1.64)
     if (DEBUG)write(JOSTND,*)' HTCALC SP/PP HGUESS= ',HGUESS

     !  FUTURE SPECIES

  case default
     HGUESS = 0.

  end select

  if (DEBUG)write(JOSTND,*)' LEAVING HTCALC HGUESS= ',HGUESS

  return
end
