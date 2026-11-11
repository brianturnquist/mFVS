!=============================!
!   canada/newmist : dmcwib   !
!=============================!
! CANADA-NEWMIST $Id$
subroutine DMCW(DMTRCW)
  implicit none

  ! **DMCW --NISI Date of last revision: 15-July-2008
  ! This module has been made by modifing the COVER MODEL module CVCW
  !  Modified for Northern Idaho.

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

  !  ENTER TREE LOOP

  do I = 1,ITRN
     DMTRCW(I) = CRWDTH(I)
     if (DEBUG) write (JOSTND,9002) I,ISPI,D,H,CL,BAREA,&
          &DMTRCW(I)
     9002 format (2I10,5F10.1)

     100 continue
  end do

  return
end
