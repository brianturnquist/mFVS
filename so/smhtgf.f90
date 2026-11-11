!=================!
!   so : smhtgf   !
!=================!
! SO $Id$
subroutine SMHTGF (MODE,ICYC,I,H,CR1,DTIME,HHT,JOSTND,DEBUG)
  implicit none

  !  THIS ROUTINE CALCULATES THE HEIGHT GROWTH OF SMALL TREES (D<3.0 IN).
  !  WHEN CALLED FROM ESSUBH (MODE=0), DTIME= TOTAL TREE AGE.
  !  IF CALLED FROM REGENT (MODE=1), DTIME= FINT AND THE
  !  RETURN VARIABLE (HHT) IS THE HEIGHT INCREMENT.
  !  CALLED FROM **ESSUBH
  !  CALLED FROM **REGENT

  !  INPUT VARIABLES
  !  I      - SPECIES SEQUENCE NUMBER
  !  H      - BEGINING HEIGHT
  !  SI     - SITE INDEX FOR SPECIES
  !  MODE   - = 0 IF CALL IS FROM ESSUBH,
  !           = 1 IF CALL IS FROM REGENT
  !  DTIME  - DTIME=TOTAL SEEDLING AGE (MODE= 0), DTIME=FINT (MODE= 1)

  !  RETURN VARIABLES
  !  HHT    - HEIGHT GROWTH OVER TIME INCREMENT DTIME (REGENT)
  !  HHT    - HEIGHT 5 YEARS INTO CYCLE, OR END OF CYCLE FINT<5 (ESSUBH)

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'
  include 'PDEN.f90'

  ! Variables
  logical :: DEBUG
  integer :: I,ICYC,JOSTND,MODE
  real    :: AGEPDT,C1,C2,C3,C4,EFFAGE,DTIME,H,HHT,HHT1,HHT2,SI
  real    :: RELHT,DOMHTGR,CRMOD,CR1,RHMOD,SMHMOD,FACTOR,BAL,TEMBAL
  real    :: SHI,SLO,CR
  external RANN

  !  IF NO GROWTH IS TO BE CALCULATED ON ESTAB TREES, I.E. PLANTING
  !  IS MORE THAN 5 YEARS INTO THE SIMULATION, THEN RETURN AND
  !  LEAVE THE SMALL TREE GROWTH, DURING THE PLANTING CYCLE, TO
  !  THE REGENT ROUTINE.

  HHT = 0.
  if (DTIME .le.0.0) go to 120

  SI = SITEAR(I)

  select case(I)

     !  HEIGHT OF TALLEST SUBSEQUENT SPECIES 1 (WP) **********

  case(1)

     !  FOR NON-LINEAR HEIGHT GROWTH MODEL SOLVE FIRST FOR EFFECTIVE
     !  AGE BASED ON INPUT HEIGHT.

     C1 = 0.375045
     C2 = 0.92503
     C3 = -0.020796
     C4 = 2.48811

     if (MODE .eq.0) then
        EFFAGE = 0.
        AGEPDT = EFFAGE + DTIME
     else if (MODE .eq.1) then
        EFFAGE = ALOG( (1.0 - (C1/SI * H)**(1/C4))/C2 ) / C3
        AGEPDT = EFFAGE + DTIME
     end if

     HHT1 = (SI/C1)*(1.0-C2*EXP(C3*EFFAGE))**C4
     HHT2 = (SI/C1)*(1.0-C2*EXP(C3*AGEPDT))**C4
     HHT = HHT2 - HHT1

     !  HEIGHT OF TALLEST SUBS. SPECIES 2 (SP) **********

  case(2)

     HHT = ((-1.0 + 0.32857*SI)/(28.0 - 0.042857*SI))*DTIME
     go to 120

     !  HT OF TALLEST SUBS. SPECIES 3 (DF) **********
     !  ALSO USED FOR 32 (OS)

  case(3,32)

     HHT = ((2.0 + 0.420*SI)/(28.5 - 0.05*SI))*DTIME
     HHT = HHT*1.1

     !  HT OF TALLEST SUBS. SPECIES 4 (WF) AND 12 (GF) **********

  case(4,12)

     HHT = ((4.2435 + 0.1510*SI)/(19.0184 - 0.0570*SI))*DTIME
     HHT = HHT*1.2

     !  HT OF TALLEST SUBS. SPECIES 5 (MH) **********
     !  IN METERS CONVERTED TO FEET

  case(5)

     HHT = ((0.965758 + 0.082969*SI)/(55.249612 - 1.288852*SI))*DTIME&
          &*3.280833
     HHT = HHT*1.60

     !  HT OF TALLEST SUBS. SPECIES 6 (IC) **********

  case(6)

     !  ADD SEEDLING AGE TO DTIME IF USER DOES NOT ENTER SEEDLING HEIGHT
     !  WHEN SERVING ESSUBH ROUTINE

     HHT = ((4.2435 + 0.1510*SI)/(19.0184 - 0.0570*SI))*DTIME
     HHT = HHT*1.3

     !  HT OF TALLEST SUBS. SPECIES 7 (LP) and 16 (WB) **********

  case(7,16)

     HHT = (0.02008805*SI)*DTIME
     if (I.eq.16)HHT = 1.6*HHT
     if (DEBUG)write(16,*)' HHT= ',HHT
     if (DEBUG)write(16,*)' HHT= ',HHT

     !  HT OF TALLEST SUBS. SPECIES 8 (ES) **********

  case(8)
     HHT = ((0.09211 + 0.208517*SI)/(43.358 - 0.168166*SI))*DTIME
     HHT = HHT*1.35

     !  RED SHASTA FIR FROM CA VARIANT

  case(9)
     if (AVH .le.0.0) then
        RELHT = 1.0
     else
        RELHT = H/AVH
     end if
     if (RELHT .gt.1.05) RELHT = 1.05

     DOMHTGR = 5*(2.2227 + 0.4314*SI)/(29.0 - 0.05*SI)
     CR = CR1/100.
     CRMOD = (1.0-EXP(-4.26558*CR))
     RHMOD = (EXP(2.54119*(RELHT**0.250537-1.0)))
     SMHMOD = 1.016605*CRMOD*RHMOD
     HHT = DOMHTGR*SMHMOD
     if (DEBUG) write (JOSTND,9900) DOMHTGR, SMHMOD,HHT, SI,CR,RELHT
     9900 format(' DEBUG-SMHTGF--9900--',3F10.4, 2F6.1,F7.4)

     !  HT OF TALLEST SUBS. SPECIES 10 (PP) **********

  case(10)
     HHT = ((-1.0 + 0.32857*SI)/(28.0 - 0.042857*SI))*DTIME

     !  HT OF TALLEST SUBS. SPECIES 11 (WJ) FROM UT **********
     !  IF THIS CHANGES ALSO CHANGE IN HTCALC

  case(11)
     SHI = 75.
     SLO = 5.
     SI = SITEAR(I)
     if (SI .gt. SHI) SI = SHI
     if (SI .le. SLO) SI = SLO + 0.5
     HHT = (SI/5.0)*(SI*1.5-H)/(SI*1.5)
     if (DEBUG)&
          &write(JOSTND,*)' LEAVING SMHTGF, ICYC,I,HHT,DTIME,SI= ',&
          &ICYC,I,HHT,DTIME,SI

     !  HT OF TALLEST SUBS. SPECIES 13 (AF) **********

  case(13)
     HHT = ((6.0 + 0.14*SI)/(33.882 - 0.06588*SI))*DTIME

     !  HT OF TALLEST SUBS. SPECIES 14 (SF) FROM EC(4) **********

  case(14)
     HHT = ((-0.6667 + 0.4333*SI)/(28.5 - 0.05*SI))*DTIME

     !  HT OF TALLEST SUBS. SPECIES 15 (NF) FROM WC(7) **********

  case(15)
     HHT = ((11.26677 + 0.12027*SI)/(27.93806 - 0.02873*SI))*DTIME

     !  HEIGHT OF TALLEST SUBS. SPECIES 17 (WL)  FROM EC(2) **********

  case(17)
     HHT = ((-3.9725 + 0.50995*SI)/(28.1168 - 0.05661*SI))*DTIME

     !  HT OF TALLEST SUBS. SPECIES 18 (RC) FROM EC(5) **********

  case(18)
     C1 = 0.752842
     C2 = 1.0
     C3 = -0.0174
     C4 = 1.4711
     if (MODE .eq.0) then
        EFFAGE = 0.
        AGEPDT = EFFAGE + DTIME
     else if (MODE .eq.1) then
        EFFAGE = ALOG( (1.0 - (C1/SI * H)**(1/C4))/C2 ) / C3
        AGEPDT = EFFAGE + DTIME
     end if

     HHT1 = (SI/C1)*(1.0-C2*EXP(C3*EFFAGE))**C4
     HHT2 = (SI/C1)*(1.0-C2*EXP(C3*AGEPDT))**C4
     HHT = HHT2 - HHT1

     !  HT OF TALLEST SUBS. SPECIES 19 (WH) FROM WC (19) **********

  case(19)
     HHT = ((-5.74874 + 0.54576*SI)/(26.15767 - 0.03596*SI))*DTIME

     !  HT OF TALLEST SUBS. SPECIES 20 (PY) FROM WC (33) **********
     !  ALSO SPECIES 21,23,25,26,28:31,33

  case(20,21,23,25,26,28:31,33)
     HHT = ((1.47043 + 0.23317*SI)/(31.56252 - 0.05586*SI))*DTIME

     !  HT OF TALLEST SUBS. SPECIES 22 (RA) FROM WC (22) **********

  case(22)
     HHT = (-0.007025 + 0.056794*SI)*DTIME
     HHT = HHT*1.20

     !  FROM CA BLACKOAK

  case(27)
     BAL = ((100.0-PCT(I))/100.0)*BA
     FACTOR = 1.*(.80+0.004*(SI-50.))
     TEMBAL = BAL
     if (TEMBAL .lt.5.0) TEMBAL = 5.0
     HHT = EXP(3.817 - 0.7829*ALOG(TEMBAL)) * FACTOR
  end select
  120 continue

  return
end
