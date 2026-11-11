!===============================================!
!   Compute tree age height and height growth   !
!===============================================!
! CA $Id$
subroutine HTCALC(JFOR,SINDX,KSPEC,AG,HGUESS,JOSTND,DEBUG)
  implicit none

  ! THIS ROUTINE CALCULATES A POTENTIAL HT GIVEN A SITE INDEX, SPECIES,
  ! AND AGE. IT IS USED TO CALCULATE POTENTIAL HEIGHT GROWTH AND SITE
  ! EQUIVALENTS.

  ! Variables
  logical :: DEBUG
  integer :: INDX,KSPEC,JFOR,JOSTND
  real    :: HGUESS,AG,SINDX
  real    :: TOPTRM,BOTTRM,TERM,TERM2,B,B50
  real    :: DUNL1(6),DUNL2(6),DUNL3(6)

  ! SPECIES ORDER IN CA VARIANT:
  !  1=PC  2=IC  3=RC  4=WF  5=RF  6=SH  7=DF  8=WH  9=MH 10=WB
  ! 11=KP 12=LP 13=CP 14=LM 15=JP 16=SP 17=WP 18=PP 19=MP 20=GP
  ! 21=JU 22=BR 23=GS 24=PY 25=OS 26=LO 27=CY 28=BL 29=EO 30=WO
  ! 31=BO 32=VO 33=IO 34=BM 35=BU 36=RA 37=MA 38=GC 39=DG 40=FL
  ! 41=WN 42=TO 43=SY 44=AS 45=CW 46=WI 47=CN 48=CL 49=OH 50=RW

  data DUNL1/ -88.9, -82.2, -78.3, -82.1, -56.0, -33.8 /
  data DUNL2/ 49.7067, 44.1147, 39.1441,&
       &35.4160, 26.7173, 18.6400 /
  data DUNL3/ 2.375, 2.025, 1.650, 1.225, 1.075, 0.875 /

  if (DEBUG)write(JOSTND,15)
  15 format(' ENTERING HTCALC')
  if (DEBUG)write(JOSTND,30)AG,SINDX,KSPEC
  30 format(' IN HTCALC 30F AG,SINDX,KSPEC=',2F10.4,I10)

  select case (JFOR)

     ! REGION 5 LOGIC
     ! R5 USE DUNNING/LEVITAN SITE CURVE.

  case(1:5)
     if (JFOR .le.5) then

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
     end if

  case default

     ! REGION 6 LOGIC

     ! GO TO A DIFFERENT POTENTIAL HEIGHT CURVE DEPENDING ON THE SPECIES.
     ! SCALE FACTORS ADDED TO SOME SPECIES SO THEY WOULD
     ! HIT THE SITE HEIGHT.

     select case (KSPEC)

        ! HANN-SCRIVANI OSU RES BULL 59, FEB 1987
        ! SPECIES: PC, IC, RC, WF, DF, WH, SP, BR, GS, OS, RW

     case(1:4,7,8,16,22,23,25,50)
        TOPTRM = 1.0-EXP(-EXP(-6.21693+0.281176*ALOG(SINDX-4.5)&
             &+ 1.14354*ALOG(AG)))
        BOTTRM = 1.0-EXP(-EXP(-6.21693+0.281176*ALOG(SINDX-4.5)&
             &+ 1.14354*ALOG(50.)))
        HGUESS = ((SINDX-4.5)*TOPTRM/BOTTRM) + 4.5
        HGUESS = HGUESS*1.05
        if (DEBUG)write(JOSTND,*)' SINDX,AG,TOPTRM,BOTTRM,HGUESS= ',&
             &SINDX,AG,TOPTRM,BOTTRM,HGUESS

        ! HANN-SCRIVANI OSU RES BULL 59, FEB 1987
        ! SPECIES: JP, WP, PP, MP, GP

     case(15,17:20)
        TOPTRM = 1.0-EXP(-EXP(-6.54707+0.288169*ALOG(SINDX-4.5)&
             &+ 1.21297*ALOG(AG)))
        BOTTRM = 1.0-EXP(-EXP(-6.54707+0.288169*ALOG(SINDX-4.5)&
             &+ 1.21297*ALOG(50.)))
        HGUESS = ((SINDX-4.5)*TOPTRM/BOTTRM) + 4.5
        HGUESS = HGUESS*1.05
        if (DEBUG)write(JOSTND,*)' SINDX,AG,TOPTRM,BOTTRM,HGUESS= ',&
             &SINDX,AG,TOPTRM,BOTTRM,HGUESS

        ! DOLPH RED FIR CURVES, RES PAP PSW 206
        ! SPECIES: RF, SH, MH

     case(5,6,9)
        TERM = AG*EXP(AG*(-0.0440853))*1.41512E-6
        B = SINDX*TERM - 3.04951E6*TERM*TERM + 5.72474E-4
        TERM2 = 50.0 * EXP(50.0*(-0.0440853)) * 1.41512E-6
        B50 = SINDX*TERM2 - 3.04951E6*TERM2*TERM2 + 5.72474E-4
        HGUESS = ((SINDX-4.5) * (1.0-EXP(-B*(AG**1.51744)))) /&
             &(1.0-EXP(-B50*(50.0**1.51744)))
        HGUESS = HGUESS + 4.5
        if (DEBUG)write(JOSTND,*)' SINDX,AG,TERM,B,TERM2,B50,HGUESS= ',&
             &SINDX,AG,TERM,B,TERM2,B50,HGUESS

        ! DAHMS LODGEPOLE PINE, RES PAP PNW-8
        ! SPECIES: WB, KP, LP, CP, LM, WJ

     case(10:14,21)
        HGUESS = SINDX * (-0.0968 + 0.02679*AG - 0.00009309*AG*AG)
        HGUESS = HGUESS*1.10
        if (DEBUG)write(JOSTND,*)' SINDX,AG,HGUESS= ',SINDX,AG,HGUESS

        ! POWERS BLACK OAK RES NOTE PSW-262
        ! SPECIES: PY, WO, BO, VO, BU, DG, FL

     case(24,30:32,35,39,40)
        TERM = SQRT(AG)-SQRT(50.)
        HGUESS = (SINDX * (1 + 0.322*TERM)) - 6.413*TERM
        HGUESS = HGUESS * 0.70
        if (DEBUG)write(JOSTND,*)' SINDX,AG,TERM,HGUESS= ',&
             &SINDX,AG,TERM,HGUESS

        ! PORTER & WIANT TANOAK, JOF 4/1965 286-287p
        ! SPECIES: TO

     case(42)
        HGUESS = SINDX / (0.204 + 39.787/AG)
        HGUESS = HGUESS * 0.80
        if (DEBUG)write(JOSTND,*)' SINDX,AG,HGUESS= ',SINDX,AG,HGUESS

        ! PORTER & WIANT MADRONE, JOF 4/1965 286-287p
        ! SPECIES: LO, CY, BL, EO, IO, MA, GC

     case(26:29,33,37,38)
        HGUESS = SINDX / (0.375 + 31.233/AG)
        HGUESS = HGUESS * 0.80
        if (DEBUG)write(JOSTND,*)' SINDX,AG,HGUESS= ',SINDX,AG,HGUESS

        ! PORTER & WIANT RED ALDER, JOF 4/1965 286-287p
        ! SPECIES: BM, RA, WN, SY, AS, CW, WI, CN, CL, OH

     case(34,36,41,43:49)
        HGUESS = SINDX / (0.649 + 17.556/AG)
        HGUESS = HGUESS * 0.80
        if (DEBUG)write(JOSTND,*)' SINDX,AG,HGUESS= ',SINDX,AG,HGUESS

        ! KING DOUGLAS-FIR, WEYERHAUSER FOR PAP 8, 1966.
        ! **NOT USED ANY MORE IN THIS VARIANT**

        !       CASE(none)
        !         Z = 2500./(SINDX - 4.5)
        !         A = -0.954038  + 0.109757*Z
        !         B =  0.055818  + 0.0079224*Z
        !         C = -0.0007338 + 0.0001977*Z
        !         HGUESS = ((AG*AG)/(A + B*AG + C*AG*AG)) + 4.5

        ! DOLPH WHITE FIR, RES PAP PSW-185, FEB 1987
        ! **NOT USED ANY MORE IN THIS VARIANT**

        !       CASE(none)
        !         X1 = (38.0202*AG**(-1.05213)*EXP(0.009557*AG))
        !         X2 = 101.842894*(1.0 - EXP(-0.001442*AG**1.679259))
        !         HGUESS = (SINDX - 69.91 + X1*X2)/X1 + 4.5
        !         IF(DEBUG)WRITE(JOSTND,*)' X1,X2,SINDX,AG,HGUESS= ',
        !    &    X1,X2,SINDX,AG,HGUESS

        ! FUTURE SPECIES

     case default
        HGUESS = 0.

     end select

  end select

  return
end
