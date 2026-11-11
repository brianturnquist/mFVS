!=============================!
!   canada/newmist : dmcwcr   !
!=============================!
! CANADA-NEWMIST $Id$
subroutine DMCW(DMTRCW)
  implicit none

  ! **DMCW --NISI Date of last revision: 05/19/94
  ! This module has been made by modifing the COVER MODEL module CVCW
  !  Modified for Central Rockies.

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

  !  Data assignments.

  !   All the data for the 5 subvariants is stored in these data
  !   statements.  We learn at run-time which subvariant the user
  !   selected, and adjust accordingly.

  !   1) Southwest mixed conifer
  !   2) Southwest PP and PJ
  !   3) Black Hills
  !   4) Spruce-fir
  !   5) Lodgepole pine

  ! this is totally faked: PP is in position 13, and
  ! **only** that position is guaranteed.

  data BH1   / 0.37031,  0.32874,  0.32874,  0.38503,&
       &0.07049,  0.07049,  0.26342,  0.33089,&
       &0.33722,  0.36380,  0.07049,&
       &0.37031,  0.36380,  0.32874,  0.38503,&
       &0.07049,  0.07049,  0.26342,  0.33089,&
       &0.33722,  0.36380,  0.07049,&
       &0.33722,  0.33722 /

  data BINT2 / 4.30800,  3.02271,  3.02271,  2.20611,&
       &-0.91984, -0.91984,  1.06804,  3.76535,&
       &1.74558,  1.62365, -0.91984,&
       &4.30800,  1.62365,  3.02271,  2.20611,&
       &-0.91984, -0.91984,  1.06804,  3.76535,&
       &1.74558,  1.62365, -0.91984,&
       &1.74558,  1.62365 /

  data BH2   /-1.37265, -1.00486, -1.00486, -0.76936,&
       &-0.07299, -0.07299, -0.55987, -1.18257,&
       &-0.73972, -0.68098, -0.07299,&
       &-1.37265, -0.68098, -1.00486, -0.76936,&
       &-0.07299, -0.07299, -0.55987, -1.18257,&
       &-0.73972, -0.68098, -0.07299,&
       &-0.73972, -0.68098 /

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

  !.... Mistletoe model initializations for CR subvariant

  !      IF (ITYPE .LT. 1 .OR. ITYPE .GT. 5) THEN
  !         LTYPE = 1
  !      ELSE
  !         LTYPE = ITYPE
  !      ENDIF

  !      IF (LTYPE .EQ. 1) THEN
  !         BH1 (7) = BH1 (8)
  !         BINT2(7)= BINT2(8)
  !         BH2 (7) = BH2 (8)
  !      ENDIF

  !     IF (LTYPE .EQ. 2) THEN
  !         BH1 (7) = BH1 (8)
  !         BINT2(7)= BINT2(8)
  !         BH2 (7) = BH2 (8)
  !         DO 200 I=8,9
  !            BH1 (I) = BH1 (11)
  !            BINT2(I)= BINT2(11)
  !            BH2 (I) = BH2 (11)
  !  200    CONTINUE
  !      ENDIF

  !      IF (LTYPE .EQ. 3) THEN
  !         BH1 (1) = BH1 (11)
  !         BINT2(1)= BINT2(11)
  !         BH2 (1) = BH2 (11)
  !         BH1 (9) = BH1 (11)
  !         BINT2(9)= BINT2(11)
  !         BH2 (9) = BH2 (11)
  !      ENDIF

  !      IF (LTYPE .EQ. 4) THEN
  !         BH1 (1) = BH1 (11)
  !         BINT2(1)= BINT2(11)
  !         BH2 (1) = BH2 (11)
  !         BH1 (10) = BH1 (11)
  !         BINT2(10)= BINT2(11)
  !         BH2 (10) = BH2 (11)
  !      ENDIF

  !      IF (LTYPE .EQ. 5) THEN
  !         BH1 (1) = BH1 (11)
  !         BINT2(1)= BINT2(11)
  !         BH2 (1) = BH2 (11)
  !      ENDIF

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
