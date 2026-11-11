!===============================================!
!   Compute tree age height and height growth   !
!===============================================!
! SO $Id$
subroutine HTCALC(JFOR,SINDX,ISPC,AG,HGUESS,JOSTND,DEBUG)
  implicit none

  ! THIS ROUTINE CALCULATES A POTENTIAL HT GIVEN AN SPECIES SITE AND AGE
  ! IT IS USED TO CAL POTHTG AND SITE

  ! Variables
  logical :: DEBUG
  integer :: ISPC,JOSTND,INDX,JFOR
  real    :: HGUESS,AG
  real    :: SINDX,X1,X2,X3,TERM,B,TERM2,B50,Z
  real    :: DUNL1(6),DUNL2(6),DUNL3(6)

  !  SPECIES ORDER:
  !  1=WP,  2=SP,  3=DF,  4=WF,  5=MH,  6=IC,  7=LP,  8=ES,  9=SH,  10=PP,
  ! 11=WJ, 12=GF, 13=AF, 14=SF, 15=NF, 16=WB, 17=WL, 18=RC, 19=WH,  20=PY,
  ! 21=WA, 22=RA, 23=BM, 24=AS, 25=CW, 26=CH, 27=WO, 28=WI, 29=GC,  30=MC,
  ! 31=MB, 32=OS, 33=OH

  !  DATA STATEMENTS:

  data DUNL1/ -88.9, -82.2, -78.3, -82.1, -56.0, -33.8 /
  data DUNL2/ 49.7067, 44.1147, 39.1441,&
       &35.4160, 26.7173, 18.6400 /
  data DUNL3/ 2.375, 2.025, 1.650, 1.225, 1.075, 0.875 /

  if (DEBUG)write(JOSTND,*)' IN HTCALC JFOR,ISPC,SINDX,AG= ',&
       &JFOR,ISPC,SINDX,AG

  select case(ISPC)

     ! WHITE PINE USE BRICKELL EQUATIONS
     ! SPECIES: WP

  case(1)
     HGUESS = SINDX/(0.37504453*(1.0-0.92503*EXP(-0.0207959*AG))**&
          &(-2.4881068))
     if (DEBUG)write(JOSTND,*)' HTCALC WP HGUESS= ',HGUESS

     ! DOUGLAS FIR USE COCHRAN PNW 251. THIS EQUATION ALSO USED FOR
     ! OTHER SOFTWOODS
     ! SPECIES: DF, OS

  case(3,32)
     HGUESS = 4.5 + EXP(-0.37496 + 1.36164*ALOG(AG)&
          &- 0.00243434*(ALOG(AG))**4)&
          &- 79.97*(-0.2828 + 1.87947*(1.0 - EXP(-0.022399*AG))&
          &**0.966998) + (SINDX - 4.5)*(-0.2828 + 1.87947*(1.0 -&
          &EXP(-0.022399*AG)**0.966998))
     if (DEBUG)write(JOSTND,*)' HTCALC DF/OS HGUESS= ',HGUESS

     ! WHITE FIR USE COCHRAN PNW 252. THIS EQUATION IS ALSO USED IN FOR
     ! INCENSE CEDAR, GRAND FIR, AND PACIFIC SILVER FIR
     ! SPECIES: WF, IC, GF, SF

  case(4,6,12,14)
     X2 = -0.30935 + 1.2383*ALOG(AG) + 0.001762*(ALOG(AG))**4&
          &- 5.4E-6*(ALOG(AG))**9 + 2.046E-7*(ALOG(AG))**11&
          &- 4.04E-13*(ALOG(AG))**18
     X3 = -6.2056 + 2.097*ALOG(AG) - 0.09411*(ALOG(AG))**2&
          &- 0.00004382*(ALOG(AG))**7 + 2.007E-11*(ALOG(AG))**16&
          &- 2.054E-17*(ALOG(AG))**24
     HGUESS = EXP(X2) - 84.73*EXP(X3) + (SINDX - 4.5)*EXP(X3)&
          &+ 4.5
     if (DEBUG)write(JOSTND,*)' HTCALC WF/IC/GF/SF X2,X3,HGUESS= ',&
          &X2,X3,HGUESS

     ! MTN HEMLOCK USE INTERIM MEANS PUB
     ! SPECIES: MH

  case(5)
     HGUESS = (22.8741 + 0.950234*SINDX)*(1.0 - EXP(-0.00206465*&
          &SQRT(SINDX)*AG))**(1.365566 + 2.045963/SINDX)
     HGUESS = (HGUESS + 1.37)*3.281
     if (DEBUG)write(JOSTND,*)' HTCALC MH HGUESS= ',HGUESS

     ! LODGEPOLE PINE USE DAHMS PNW 8
     ! SPECIES: LP

  case(7)
     HGUESS = SINDX*(-0.0968 + 0.02679*AG - 0.00009309*AG*AG)
     if (DEBUG)write(JOSTND,*)' HTCALC LP HGUESS= ',HGUESS

     ! ENGLEMANN SPRUCE USE ALEXANDER
     ! SPECIES: ES

  case(8)
     HGUESS = 4.5+((2.75780*SINDX**0.83312)*(1.0-EXP(-0.015701*AG))&
          &**(22.71944*SINDX**(-0.63557)))
     if (DEBUG)write(JOSTND,*)' HTCALC ES HGUESS= ',HGUESS

     ! SHASTA FIR: R6 USES DOLPH RED FIR CURVES, RES PAP PSW 206
     !             R5 USES DUNNING-LEVITATN CURVES
     ! SPECIES: SH

  case(9)
     select case (JFOR)
     case (1:3,10)
        TERM = AG*EXP(AG*(-0.0440853))*1.41512E-6
        B = SINDX*TERM - 3.04951E6*TERM*TERM + 5.72474E-4
        TERM2 = 50.0 * EXP(50.0*(-0.0440853)) * 1.41512E-6
        B50 = SINDX*TERM2 - 3.04951E6*TERM2*TERM2 + 5.72474E-4
        HGUESS = ((SINDX-4.5) * (1.0-EXP(-B*(AG**1.51744)))) /&
             &(1.0-EXP(-B50*(50.0**1.51744)))
        HGUESS = HGUESS + 4.5
        if (DEBUG)write(JOSTND,*)' HTCALC SH TERM,B,TERM2,B50,HGUESS= ',&
             &TERM,B,TERM2,B50,HGUESS

     case default

        ! SET UP MAPPING TO THE CORRECT DUNNING-LEVITAN SITE CURVE

        if (SINDX .le.44.) then
           INDX = 6
        else if (SINDX.gt.44..and. SINDX.le.52.) then
           INDX = 5
        else if (SINDX.gt.52..and. SINDX.le.65.) then
           INDX = 4
        else if (SINDX.gt.65..and. SINDX.le.82.) then
           INDX = 3
        else if (SINDX.gt.82..and. SINDX.le.98.) then
           INDX = 2
        else
           INDX = 1
        end if
        if (AG .le.40.) then
           HGUESS = DUNL3(INDX) * AG
        else
           HGUESS = DUNL1(INDX) + DUNL2(INDX)*ALOG(AG)
        end if
        if (DEBUG)write(JOSTND,*)' HTCALC SH DUNNING INDX,HGUESS= ',&
             &INDX,HGUESS
     end select

     ! PONDEROSA PINE USE BARRETT. THIS EQUATION ALSO USED FOR SUGAR PINE.
     ! SPECIES: SP, PP

  case(2,10)
     HGUESS = (128.89522*(1.0 -EXP(-0.016959*AG))**1.23114)&
          &- ((-0.7864 + 2.49717*(1.0-EXP(-0.0045042*AG))**0.33022)&
          &*100.43) + ((-0.7864 + 2.49717*(1.0- EXP(-0.0045042*AG))&
          &**0.33022)*(SINDX - 4.5)) + 4.5
     if (DEBUG)write(JOSTND,*)' HTCALC SP/PP HGUESS= ',HGUESS

     ! SUB ALPINE FIR USES JOHNSON'S EQUIV OF DEMARS
     ! SPECIES: AF

  case(13)
     HGUESS = SINDX*(-0.07831 + 0.0149*AG - 4.0818E-5*AG*AG)
     if (DEBUG)write(JOSTND,*)' HTCALC AF HGUESS= ',HGUESS

     ! NOBLE FIR - HERMAN, PNW-243
     ! SPECIES: NF

  case(15)
     X1 = -564.38 + 22.250*(SINDX - 4.5) - 0.04995*(SINDX - 4.5)**2
     X2 = 6.8 + 2843.21*(SINDX - 4.5)**(-1) + 34735.54*&
          &(SINDX - 4.5)**(-2)
     HGUESS = 4.5 + (SINDX - 4.5)/(X1*(1.0/AG)**2 +&
          &X2*(1.0/AG) + 1.0 - 0.0001*X1 - 0.01*X2)
     if (DEBUG)write(JOSTND,*)' HTCALC NF X1,X2,HGUESS= ',X1,X2,HGUESS

     ! WESTERN LARCH USE COCHRAN PNW 424
     ! SPECIES: WL

  case(17)
     HGUESS = 4.5 + 1.46897*AG + 0.0092466*AG*AG - 0.00023957*AG**3 +&
          &1.1122E-6*AG**4 + (SINDX -4.5)*(-0.12528 + 0.039636*AG&
          &- 0.0004278*AG*AG + 1.7039E-6*AG**3)-&
          &73.57*(-0.12528 + 0.039636*AG - 0.0004278*AG*AG +&
          &1.7039E-6*AG**3)
     if (DEBUG)write(JOSTND,*)' HTCALC WL HGUESS= ',HGUESS

     ! RED CEDAR ---- USE HEGYI, JELINEK, VISZLAI,
     ! & CARPENTER 1981 FOR SITE REFERENCE
     ! SPECIES: RC

  case(18)
     HGUESS = 1.3283*SINDX * ((1.0 - EXP(-0.0174*AG))**1.4711)
     if (DEBUG)write(JOSTND,*)' HTCALC RC HGUESS= ',HGUESS

     ! WESTERN HEMLOCK - WILEY, 1978
     ! SPECIES: WH

  case(19)
     Z = 2500./(SINDX-4.5)
     HGUESS = (AG*AG/(-1.73070 + 0.1394*Z + (-0.0616 + 0.0137*Z)*AG&
          &+ (0.00192 + 0.00007*Z)*(AG*AG))) + 4.5
     if (DEBUG)write(JOSTND,*)' HTCALC WH Z,HGUESS= ',Z,HGUESS

     ! MISC. SPECIES - USE CURTIS, FOR. SCI. 20:307-316.  CURTIS CURVES
     ! ARE PRESENTED IN METRIC (3.2808 ?)

     ! BECAUSE OF EXCESSIVE HT GROWTH -- APPROX 30-40 FT/CYCLE, TOOK OUT
     ! THE METRIC MULTIPLIER. DIXON 11-05-92
     ! SPECIES: PY, WA, BM, CW, CH, WI, GC, MC, MB, OH

  case(20,21,23,25,26,28:31,33)
     HGUESS = (SINDX - 4.5) / (0.6192 - 5.3394/(SINDX - 4.5)&
          &+ 240.29*AG**(-1.4) + (3368.9/(SINDX-4.5))*AG**(-1.4))
     HGUESS = HGUESS + 4.5
     if (DEBUG)write(JOSTND,*)' HTCALC MISC HGUESS= ',HGUESS

     ! RED ALDER - HARRINGTON, PNW-358
     ! SPECIES: RA

  case(22)
     HGUESS = SINDX&
          &+ (59.5864 + 0.79530*SINDX)*(1.0-EXP((0.00194 -&
          &0.000740*SINDX)*AG))**0.9198&
          &- (59.5864 + 0.79530*SINDX)*(1.0-EXP((0.00194 -&
          &0.000740*SINDX)*20.0))**0.9198
     if (DEBUG)write(JOSTND,*)' HTCALC RA HGUESS= ',HGUESS

     ! R6 POWERS BLACK OAK RES NOTE PSW-262
     ! R5 DUNNING-LEVITAN SITE CURVES
     ! SPECIES: WO

  case(27)
     select case (JFOR)
     case (1:3,10)
        TERM = SQRT(AG)-SQRT(50.)
        HGUESS = (SINDX * (1 + 0.322*TERM)) - 6.413*TERM
        if (DEBUG)write(JOSTND,*)' HTCALC WO HGUESS= ',HGUESS

     case default

        ! SET UP MAPPING TO THE CORRECT DUNNING-LEVITAN SITE CURVE

        if (SINDX .le.44.) then
           INDX = 6
        else if (SINDX.gt.44..and. SINDX.le.52.) then
           INDX = 5
        else if (SINDX.gt.52..and. SINDX.le.65.) then
           INDX = 4
        else if (SINDX.gt.65..and. SINDX.le.82.) then
           INDX = 3
        else if (SINDX.gt.82..and. SINDX.le.98.) then
           INDX = 2
        else
           INDX = 1
        end if
        if (AG .le.40.) then
           HGUESS = DUNL3(INDX) * AG
        else
           HGUESS = DUNL1(INDX) + DUNL2(INDX)*ALOG(AG)
        end if
        if (DEBUG)write(JOSTND,*)' HTCALC WO DUNNING INDX,HGUESS= ',&
             &INDX,HGUESS
     end select

     ! WESTERN JUNIPER, WHITEBARK PINE, AND QUAKING ASPEN DO NOT HAVE SITE CURVES
     ! SPECIES: WJ, WB, AS

  case(11,16,24)
     HGUESS = 0.0
     if (DEBUG)write(JOSTND,*)' HTCALC WJ/WB/AS HGUESS= ',HGUESS

     ! FUTURE SPECIES

  case default
     HGUESS = 0.0
     if (DEBUG)write(JOSTND,*)' HTCALC DEFAULT HGUESS= ',HGUESS

  end select

  return
end
