!=============================!
!   canada/newmist : dmcwwc   !
!=============================!
! CANADA-NEWMIST $Id$
subroutine DMCW(DMTRCW)
  implicit none

  ! **DMCW --NISI Date of last revision: 08/05/94
  ! This module has been made by modifing the COVER MODEL module CVCW
  !  Modified for West Cascades.

  ! Purpose:
  !   Computes crown width for individual trees. Widths are measured
  ! in feet. Further documentation can be found in: Moeur, Melinda.
  ! 1981. Crown width and foliage weight of northern Rocky Mountain
  ! Confifers. USDA Forest Service Res. Pap. INT-283.

  ! Called by:

  !     DMMTRX

  ! Other routines called:

  !     DBCHK

  ! Argument list definitions:

  !     REAL    DMTRCW (O)  Predicted maximum crown width (feet).

  ! Local variable definitions (not complete):

  !     REAL    BH1         COEFFICIENTS FOR HEIGHT TERM FOR CROWN
  !                          WIDTH FUNCTION FOR TREES LESS THAN
  !                          3.5 INCHES DBH.
  !     REAL    BINT2       INTERCEPTS FOR CROWN WIDTH FUNCTION FOR.
  !                          TREES 3.5 INCHES AND LARGER
  !     REAL    BH2         COEFFICIENTS FOR HEIGHT TERM FOR TREES
  !                          .GE. 3.5 INCHES.
  !     REAL    BAREA       STAND BASAL AREA (sq feet?).
  !     REAL    CL          CROWN LENGTH (feet).
  !     REAL    D           TREE DBH (inches).
  !     REAL    H           TREE HEIGHT (feet).

  ! Common block variables and parameters:

  !     [none related to NISI; FVS commons are not documented]

  !********************************************************************C

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'
  include 'DMCOM.f90'

  ! Argument list variables

  ! Variables
  real    :: DMTRCW

  dimension DMTRCW(MAXTRE)

  ! Local variables.

  logical :: DEBUG
  dimension BH1(MAXSP)
  dimension BINT2(MAXSP)
  dimension BH2(MAXSP)

  ! Data assignments.

  data BH1   / 0.38503,  0.38503,  0.38503,  0.33722,&
       &0.33722,  0.07049,  0.33722,  0.46452,&
       &0.46452,  0.33089,  0.26342,  0.36380,&
       &0.37031,  0.37031,  0.36380,  0.32874,&
       &0.07049,  0.46452,  0.25622,  0.25622,&
       &0.07049,  0.07049,  0.07049,  0.07049,&
       &0.07049,  0.07049,  0.07049,  0.07049,&
       &0.07049,  0.23846,  0.37031,  0.36380,&
       &0.07049,  0.07049,  0.07049,  0.07049,&
       &0.07049,  0.07049,  0.07049  /

  data BINT2 / 2.20611,  2.20611,  2.20611,  1.74558,&
       &1.74558, -0.91984,  1.74558,  2.79784,&
       &2.79784,  3.76535,  1.06804,  1.62365,&
       &4.30800,  4.30800,  1.62365,  3.02271,&
       &-0.91984,  2.79784,  1.32772,  1.32772,&
       &-0.91984, -0.91984, -0.91984, -0.91984,&
       &-0.91984, -0.91984, -0.91984, -0.91984,&
       &-0.91984,  2.31359,  4.30800,  1.62365,&
       &-0.91984, -0.91984, -0.91984, -0.91984,&
       &-0.91984, -0.91984, -0.91984  /

  data BH2   /-0.76936, -0.76936, -0.76936, -0.73972,&
       &-0.73972, -0.07299, -0.73972, -0.89666,&
       &-0.89666, -1.18257, -0.55987, -0.68098,&
       &-1.37265, -1.37265, -0.68098, -1.00486,&
       &-0.07299, -0.89666, -0.52554, -0.52554,&
       &-0.07299, -0.07299, -0.07299, -0.07299,&
       &-0.07299, -0.07299, -0.07299, -0.07299,&
       &-0.07299, -0.80919, -1.37265, -0.68098,&
       &-0.07299, -0.07299, -0.07299, -0.07299,&
       &-0.07299, -0.07299, -0.07299  /

  !  CHECK FOR DEBUG.

  call DBCHK(DEBUG,'DMCW',4,ICYC)
  if (DEBUG) write (JOSTND,9000) ICYC
  9000 format (/' **CALLING DMCW, CYCLE = ',I2 / '         I      ISPI',&
       &'         D         H        CL     BAREA   DMTRCW')

  !     RETURN IF NOTREES OPTION IN EFFECT.

  if (ITRN .gt.0) goto 5
  if (DEBUG) write (JOSTND,9001) ITRN
  9001 format (' ITRN =', I5,' : NOTREES : RETURN TO **DMMTRX**')
  return
  5 continue

  !  USE PRE-THIN DENSITY STATISTICS IF A THINNING HAS JUST OCCURRED.

  BAREA = BA
  ALOGBA = ALOG(BAREA)

  !  ENTER TREE LOOP

  do I = 1,ITRN
     D = DBH(I)
     H = HT(I)
     ISPI = ISP(I)
     IICR = ICR(I)
     CL = FLOAT(IICR)*H/100.

     !  BRANCH ON DBH

     if (D .lt.3.5) goto 10

     !  COMPUTE CROWN WIDTH FOR TREES 3.5 INCHES AND LARGER

     DMTRCW(I) = EXP (BINT2(ISPI) +  1.08137*ALOG(D) +&
          &BH2(ISPI)*ALOG(H) + 0.29786*ALOG(CL))

     !  CORRECT ESTIMATE FOR NEGATIVE BIAS. (BASKERVILLE 1972)
     !  BIAS ADJUSTMENT = EXP(.5*MSE) = EXP (.5*.04898)

     DMTRCW(I) = DMTRCW(I) * 1.02479
     goto 90

     !  COMPUTE CROWN WIDTH FOR TREES LESS THAN 3.5 INCHES

     10 continue

     DMTRCW(I) = EXP (BH1(ISPI)*ALOG(H) + 0.28283&
          &*ALOG(CL) + 0.04032*ALOGBA)

     !  CORRECT ESTIMATE FOR NEGATIVE BIAS
     !  BIAS ADJUSTMENT = EXP(.5*MSE) = EXP(.5*.06036)

     DMTRCW(I) = DMTRCW(I)*1.03064

     90 continue

     if (DEBUG) write (JOSTND,9002) I,ISPI,D,H,CL,BAREA,&
          &DMTRCW(I)
     9002 format (2I10,5F10.1)

     100 continue
  end do

  return
end
