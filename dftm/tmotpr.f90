!===================!
!   dftm : tmotpr   !
!===================!
! DFTM $Id$
subroutine TMOTPR (PROTBK)
  implicit none

  !     PROTBK= THE CONDITIONAL PROBABILITY THAT THIS STAND WILL
  !             SUSTAIN A TUSSOCK MOTH OUTBREAK GIVEN THAT AN OUTBREAK
  !             IS OCCURING IN THE GENERAL VICINITY.
  !     N. L. CROOKSTON, PROGRAMMER. 5/1978 MODIFIED 3/79 INT -- MOSCOW

  ! Revision History:
  !   01-APR-2013 Lance R. David (FMSC)
  !      A few variables defined locally were already defined
  !      in a common block. Local declaration removed.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'TMCOM1.f90'

  ! Variables
  integer :: I,I1,I2,II
  real    :: AVCRDI,HOST,PGFBA,PROTBK,STDCLO,TMB0,TMB1,TMR0

  if (IPRBMT .gt.1) goto 50

  !     THIS EQUATION IS FROM R. C. HELLER, UNIVERSITY OF IDAHO,
  !     COLLEGE OF FORESTRY WILDLIFE AND RANGE SCIENCES, MOSCOW 83843

  !     VARIABLES:

  !     ELEV  = ELEVATION IN HUNDREDS OF FEET. EQUATIONS CALL
  !             FOR FEET:  ELEV*100.0
  !     SLOPE = SCALED SLOPE (0-1). EQUATIONS CALL FOR PERCENT
  !             SLOPE:  SLOPE*100.0
  !     ASPECT= SCALED ASPECT(0-8). EQUATIONS CALL FOR AZIMUTH
  !             FROM NORTH:  THE UNITS USED HERE RADIANS
  !     TOPO  = TOPOGRAPHIC POSITION. 1=RIDGETOP,2=SIDEHILL,3=BOTTOM.
  !             INPUT BY KEYWORD, DEFAULTED IN TMINIT.
  !     STDCLO= PERCENTAGE OF CROWN CLOSURE AS VIEWED ON AERIAL
  !             PHOTOGRAPHS;  ESTIMATED AS A FUNCTION OF CCF.
  !     HOST  = PERCENTAGE OF CROWN AREA COMPOSED OF HOST.
  !             ESTIMATED AS A FUNCTION OF CCF PER SPECIES AND CCF.
  !     AVCRDI= AVERAGE CROWN DIAMETER. ESTIMATED USING CCF AND TREES
  !             PER ACRE POST CUTTING.
  !     PGFBA = PROPORTION OF BASAL AREA IN GRAND FIR.
  !     THASHD= SOIL ASH DEPTH. DEFAULTED IN TMINIT.

  !     CALCULATE MODEL PARAMETERS

  if (ISCT(IDFCOD,1) .eq.0) then
     PROTBK = 0.0
     goto 110
  end if

  STDCLO = 100.0
  if (RELDEN .lt.150.0) STDCLO = (RELDEN / 150.0) * 100.0
  HOST = ((RELDSP(3) + RELDSP(4)) / RELDEN) * 100.0
  AVCRDI = 2.0 * SQRT((435.6 * (RELDEN / TPROB)) / 3.141593)

  !     CALCULATE THE CONDITIONAL PROBABILITY

  PROTBK = 1.0 / (1.0 + EXP(-(-0.852926 -&
       &(0.00023762 * ELEV  * 100.0) -&
       &(0.00052207 * SLOPE * 100.0) +&
       &(0.00348125 * SLOPE * 100.0 * COS(ASPECT)) +&
       &(0.00733782 * SLOPE * 100.0 * SIN(ASPECT)) -&
       &(0.37562400 * TOPO) +&
       &(0.02049930 * STDCLO) +&
       &(0.01866930 * HOST) +&
       &(0.01685520 * AVCRDI))))

  if (TMDEBU) then
     write (JODFTM,20) ELEV, SLOPE, ASPECT, TOPO, HOST, AVCRDI,&
          &TPROB, RELDEN, RELDSP(3), RELDSP(4)
     20 format (//,' SUBROUTINE TMOTPR:  ELEV     SLOPE     ASPECT  ',&
          &'  TOPO      HOST      AVCRDI    TPROB     RELDEN   ',&
          &'RELDSP(3) RELDSP(4)',/,T17,10F10.4)
  end if
  goto 110

  50 continue

  !     THESE EQUATIONS ARE FROM PETER MIKA AND JIM MOORE, UN OF IDAHO,
  !     COLLEGE OF FORESTRY WILDLIFE AND RANGE SCIENCES, MOSCOW 83843

  !     CALCULATE GRAND FIR BASAL AREA.

  PGFBA = 0.0
  I1 = ISCT(IGFCOD,1)
  if (I1.eq.0) goto 70
  I2 = ISCT(IGFCOD,2)

  do II = I1,I2
     I = IND1(II)
     PGFBA = PGFBA + (0.005454154 * DBH(I)**2 * PROB(I))
     60 continue
  end do

  70 continue

  !     CALCULATE PROPORTION IF BASAL AREA IN GRAND FIR.

  PGFBA = PGFBA / BA
  if (IPRBMT .gt.2) goto 80

  PROTBK = 1.0 / (1.0 + EXP(-(-12.7746 -&
       &1.4764 * (TOPO - 1.0) -&
       &0.0992 * TMASHD +&
       &4.8334 * PGFBA +&
       &2.5529 * ALOG(BA))))

  go to 90

  80 continue
  PROTBK = 1.0 / (1.0 + EXP(-(-12.3652 -&
       &1.4050 * (TOPO - 1.0) +&
       &3.8230 * PGFBA +&
       &2.2426 * ALOG(BA))))

  90 continue
  if (TMDEBU) then
     write (JODFTM,100) PROTBK, IPRBMT, TOPO, TMASHD, PGFBA, BA
     100 format (//,' SUBROUTINE TMOTPR:  PROTBK   IPRBMT    TOPO    ',&
          &'  TMASHD    PGFBA     BA',/,T17,F10.4,I10,4F10.4)
  end if

  110 continue
  return
end
