!==============================!
!   archive/bgc/src : bwater   !
!==============================!
subroutine BWATER

  ! SUBROUTINE WATER CREATES THE PRECIPITATION AND SOIL WATER VARIABLES
  ! NEEDED IN SUBSEQUENT CALCULATIONS. CALL ON DAILY BASIS
  !    AFTER HUNT, 1993.
  ! ADDING NEW VARS TO DAYWATER.OUT AJM 1/04

  !    Z1(15) is min. night temperature
  !    Z1(3) is precipitation (m)
  !    Z1(14) is daylight average air temperature (C)
  !    Z1(8) is daily shortwave radiation (kj/day)
  !    Z1(16) vapor pressure deficit (mbar)
  !    Z1(18) daylength (seconds)
  !    B2(1) is interception coefficient. Was B(4).
  !    S(5) is snowmelt coefficient

  ! Variables
  integer :: YEAR,DSRAIN
  real    :: TOTPPT(60),RADMELT,RADTRANS
  real    :: EVAPPT,X,LATHEAT,EVAPRAD,EXCESS,GSOIL,GB
  real    :: EVAP,DAYCONST,MAXOUT(10),PENMON,MAXH2O(10),VCONT(10)
  real    :: VCONT_FC,SWC_FC(10),GRAVOUT(10) !New vars for saturation-to-FC adjustments ajm 5/01

  ! Common blocks
  include 'ENTITY.f90' ! Structure, etc.
  include 'SITE.f90' ! S() variables

  NSL = 2 !*!  (added 10/97 to implement soil layers -epv)

  !---If leaves on then total LAI is sum of live and dead standing LAI
  !   All LAI numbers are in subroutine LAIJ.
  !   SITELAI is sum LA(I,J)/AREA
  !      IF (LEAF_ON) THEN SITELAI=LIVELAI+DEADLAI
  !      ELSE SITELAI=DEADLAI
  !---Initialize SWC & DAYTRANS. SNOWPACK initialized in ZCOMP.
  !   SWC & SNOWPACK are initially defined in SITE.DAT.
  !   DAYTRANS is updated at the end of each day in GSV.FOR with
  !   DAYTRANS=SUMTRANS right before SUMTRANS is zeroed for the next day.
  YEAR = Z1(1)
  if (YEAR.eq.1.and. JD.eq.1) then
     SWC = S(1)
     DAYTRANS = 0.0
     DAYTRANS_GR = 0.0 !*!
     DAYTRANS_SH = 0.0 !added 10/00 ajm
     DAYTRANS_SM = 0.0 !*!
     DAYTRANS_LG = 0.0 !*!
     DAYTRANS_LG1 = 0.0 !*!
     DAYTRANS_LG2 = 0.0 !*!

     do SL = 1,NSL !*!
        SWC(SL) = S(1)/NSL !*!  divides total SWC equally between layers
        SD(SL) = S(2)/NSL !*!  divides total SD equally between layers
        10 continue !*!
     end do

  end if
  !---Determine whether precip is rain or snow
  if (Z1(15).gt.0.0) then !RAIN gets interception removed
     RAIN = (Z1(3) - SITELAI * B2(1)) * AREA
     if (RAIN.lt.0.0) RAIN = 0.0
     SNOW = 0.0
  else
     SNOW = Z1(3)*AREA
     if (SNOW.lt.0.0) SNOW = 0.0
     RAIN = 0.0
  end if
  !---Accumulate daily precipitation (YEAR=Z1(1) & is defined in ZCOMP.FOR)
  TOTPPT(YEAR) = TOTPPT(YEAR)+Z1(3)
  !---Temperature based snowmelt
  SNWMELT = S(5) * Z1(14) * AREA
  SNWMELT = MAX(SNWMELT,0.0)
  !---Radiation based snowmelt (Coughlan, Ph.D thesis, 1991)
  !   latent heat of fusion in Kj/m m2
  !   0.6=1-albedo of snow
  !   RADTRANS=ZQ1(INDEX(1)) is radiation incident at top of bottom layer.
  !   calculated in subroutine ABSRAD.
  RADTRANS = ZQ1(INDEX(1))
  if (Z1(14).ge.0.0) then
     RADMELT = 0.6 * (RADTRANS/3.472E5) * AREA
     SNWMELT = MAX(RADMELT,0.0) + SNWMELT
  else
     RADMELT = 0.6 * (RADTRANS/2.84512E6) * AREA
     SNWMELT = MAX(RADMELT,0.0)
  end if
  !---Update SNOWPACK
  SNOWPACK = SNOWPACK+SNOW
  !---Make sure don't melt more snowpack than exists
  if ((SNOWPACK-SNWMELT).lt.0.0) SNWMELT = SNOWPACK
  SNOWPACK = SNOWPACK-SNWMELT
  !---Calculate potential evaporation of intercepted PPT
  EVAPPT = (Z1(3)*AREA) - RAIN - SNOW
  !---Calculate potential evaporation with radiation limit.
  !   First calculate latent heat of fusion, LATHEAT.
  X = 2501.2 - 2.3787 * Z1(15)
  LATHEAT = X*1000.
  EVAPRAD = (Z1(8)/LATHEAT)*AREA
  if (EVAPPT.lt.0.0) EVAPPT = 0.0
  if (EVAPPT.gt.EVAPRAD) then
     EXCESS = EVAPPT-EVAPRAD
     EVAPPT = EVAPRAD
  else
     EXCESS = 0.0
  end if
  !---Calculate bare soil evaporation using Penmon-Monteith equation. Soil
  !   is wet when it has rained or when snowmelt
  if (RAIN.gt.0.0.or. SNWMELT.gt.0.0) then
     DSRAIN = 0
  else
     DSRAIN = DSRAIN+1
  end if
  !---Set gsoil constant according to days since last rain or snowmelt
  select case(DSRAIN)
  case(0)
     GSOIL = 0.002 ! 2mm/s data from Baldocchi and Meyers
  case(1)
     GSOIL = 0.001
  case(2)
     GSOIL = 0.0005
  case(3)
     GSOIL = 0.00025
  case(4)
     GSOIL = 0.000125
  case(5)
     GSOIL = 0.000063
  case(6)
     GSOIL = 0.00001
  case default
     GSOIL = 0.000001
  end select
  !---Call Penmon-Monteith function
  GB = 0.001 ! boundary layer conductance for soil
  call BPENMAN(Z1(14),Z1(16),ZQ1(INDEX(1)),GB,GSOIL,&
       &Z1(18),PENMON)
  EVAP = PENMON
  DAYCONST = 0.85
  EVAP = EVAP*AREA*Z1(18)*DAYCONST
  !---Put ppt limit on evaporation
  if (RAIN.gt.0.0.and. EVAP.gt.(0.5*RAIN)) then
     EVAP = 0.5*RAIN
     DSRAIN = 4
  end if
  if (SNOW.gt.0.0) then
     EVAP = 0.0
     DSRAIN = 0
  end if
  !---Calculate ground water outflow. SWC is input at start
  !   of run. SWC is updated daily.

  !         WRITE(*,999) JD,RAIN,SNWMELT,EVAP,SWC(1),DAYTRANS
  ! 999     FORMAT(I4,5(1X,F8.2))

  !--- VERS 6 -------------------------------------------------------------------------
  !--- Partition daily precip into the different soil layers   !*!

  do SL = 1,NSL
     MAXH2O(SL) = S(3) * SD(SL) * AREA

     if (MAXSL.eq.1) then
        TF1 = 1 ! TF1 = true factor 1
        TF2 = 0 ! TF2 = true factor 2
     else
        TF1 = 0
        TF2 = 1
     end if
     !--- update soil water content, SWC.
     !--- DAYTRANS is the transpiration of all plants the day before.
     !--- 1st layer = rain +1/2 snow +excess -evap -SM_trans -Lg_trans -out(1)
     !--- 2nd layer = 1/2 snow +out(1) -LG_trans -out(2)

     !*****Major logic change being made 5/24/01 AJM.
     !   First, we will remove yesterday's transpiration.
     !   Then we will evaluate water content; if greater than FIELD CAPACITY (FC)
     !   then SWC will be reduced by 1/2 the difference between FC and the SWC
     !   That is, we're--in essence--draining off 1/2 the gravitational water present every 24 hours
     !   {Prior to this change, I attempted to remove all gravitational water after 24 hours
     !   (i.e. reduced SWC to FC after 1 day).  This was too harsh.
     !   (heretofore, FC was never considered; soils got up
     !   to saturation and SWC got reduced below sat ONLY via evap and transp.
     !   Now, gravity will drain away gravitational water.  Equations will again be from
     !   Saxton et al 1986.
     !   After gravitational water moved to outflow, (bookkept separately than original OUTFLOW)
     !   THEN rain and snow will be added, and evap and original outflow (outflow of water beyond
     !   saturation) will be subtracted (as will the grav water outflow).
     !   The timing of this calculation (at the beginning of the day) effectively drains away (at least some)
     !   gravitational water AFTER allowing it to remain (i.e. hence available) for one day (the day before).

     !   VCONT_FC IS VOLUMETRIC CONTENT AT FIELD CAPACITY.  AA & BB are soil-specific texture parameters
     !   They are set in BINITIAL.  They and the eqns are from Saxton et al 1986.

     VCONT_FC = EXP((2.302 - LOG(AA)) / BB)
     if (SL.eq.1) then ! top layer
        ! First take out yesterday's transpiration
        SWC(SL) = SWC(SL)-DAYTRANS_GR -DAYTRANS_SM -DAYTRANS_LG1&
             &-DAYTRANS_SH
        !   Next, evaluated status relative to FC.
        SWC_FC(SL) = VCONT_FC * SD(SL) * AREA
        GRAVOUT(SL) = 0.0
        if (SWC(SL) .gt. SWC_FC(SL)) then
           GRAVOUT(SL) = (SWC(SL) - SWC_FC(SL)) / 2 !Keep track of gravitational water, add to next layer
           SWC(SL) = SWC(SL)- GRAVOUT(SL) !Reduce SWC by amount of grav water drained
        end if

        MAXOUT(SL) = SWC(SL)+RAIN +SNWMELT/2+EXCESS-MAXH2O(SL)
        OUTFLOW(SL) = MAX ( MAXOUT(SL), 0.0 )
        SWC(SL) = SWC(SL) +RAIN +SNWMELT/2 +EXCESS -EVAP - OUTFLOW(1)

     else ! 2nd layer
        SWC(SL) = SWC(SL)-DAYTRANS_LG2 ! Yesterday's transpiration
        SWC_FC(SL) = VCONT_FC * SD(SL) * AREA
        GRAVOUT(SL) = 0.0
        if (SWC(SL) .gt. SWC_FC(SL)) then
           GRAVOUT(SL) = (SWC(SL) - SWC_FC(SL)) / 2 !Keep track of gravitational water, to be drained away
           SWC(SL) = SWC(SL) - GRAVOUT(SL) !Reduce SWC by amount of grav water drained
        end if

        !           MAXOUT(SL) = SWC(SL) +SNWMELT/2 -MAXH2O(SL)
        !   Am going to modify above eqn.  Seems layer one's outflow should've been added to
        !   determin MAXOUT.  I will add that, as well as the GRAVOUT.
        MAXOUT(SL) = SWC(SL) + SNWMELT/2 + OUTFLOW(1) + GRAVOUT(1)&
             &- MAXH2O(SL)
        OUTFLOW(SL) = MAX ( MAXOUT(SL), 0.0 )
        SWC(SL) = SWC(SL) + SNWMELT/2 + OUTFLOW(1) +GRAVOUT(1)&
             &-OUTFLOW(2) - GRAVOUT(2)
     end if
     ! ***** END OF MODIFICATIONS (REDUCING SWC TO FC). *****

     SWC(SL) = MIN ( SWC(SL), MAXH2O(SL) )
     !--- Calculate site soil water potential (-MPa)
     VCONT(SL) = SWC(SL) / (SD(SL) * AREA)
     SITEWP(SL) = 0.001 * AA * VCONT(SL) **BB

     20 continue !*! end looping through soil layers
  end do

  !--- sum water stress for use in calculating allocation fractions
  if (SITEWP(1).lt.SITEWP(2)) then !*! check which layer
     BESTSL = 1 !*! has highest SWP
  else !*!
     BESTSL = 2 !*! and set MAXSL
  end if
  ! Variables below used in BALLOCA
  STRESS(1) = SITEWP(1) !*! for grass and small trees [and now shrubs (10/00ajm)
  STRESS(2) = SITEWP(BESTSL) !*! for large trees                  !*!
  if (STRESS(1).gt.B2(3)) STRESS(1) = B2(3) !*!
  if (STRESS(2).gt.B2(3)) STRESS(2) = B2(3) !*!
  SUMWP(1) = SUMWP(1)+STRESS(1) !*! for grass and small trees [and now shrubs (10/00, ajm)
  SUMWP(2) = SUMWP(2)+STRESS(2) !*! for large trees                  !*!

  !--- write water variables out to daywatr.out.  ADDING 7 MORE 1/04 AJM
  !   NOTE: "RAIN" IS THAT PORTION OF Z1(3) [ppt] MAKING IT TO SOIL (i.e. NOT
  !   INTERCEPTED BY LEAVES).
  !   Z13*1000 IS PPT IN MM.  EVAPPT IS PORTION OF PPT INTERCEPTED AND
  !   EVAPORATED FROM VEG SURFACES
  !   Z1(3) IS IN METERS.  REPORTED TO OUTPUT IS Z1(3)*1000 (i.e. mm)
  !   EXCESS IS THAT MOISTURE ORIGINALLY INTERCEPTED BY VEG, BUT NOT SUBSEQUENTLY
  !   EVAPORATED (IF ANY)--IT EVENTUALLY GOES TO SOIL.
  !   TO BALANCE: OUTPUT_PPT * 10 = RAIN+SNOW+EVAPPT+EXCESS ( in CUBIC METERS/HA)
  !   "*10" BECAUSE, PPT(mm) * 10,000 m2/ha / 1000 mm/m = CUBIC METERS PER HA
  if (Z1(1).eq.1.and. JD.eq.1)  write (71,890)
  890 format ('YEAR',T7,'JD',T14,'PPT',&
       &T21,'MAX1',T29,'MAX2',T37,'SWC1',T45,'SWC2',T51,'RATIO1',&
       &T59,'RATIO2',T70,'WP1',T78,'WP2',T85,'OUT1',&
       &T92,'TR_GR',T100,'TR_SM',T108,'TR_LG',T114,'SITELAI',&
       &T125,'RAIN',T133,'SNOW',T139,'EVAPPT',T147,'EXCESS'&
       &T157,'EVAP',T164,'SNWPK',T171,'SNWMLT')
  write(71,892) Z1(1),JD,Z1(3)*1000.,&
       &MAXH2O(1),MAXH2O(2),SWC(1),SWC(2),SWC(1)/MAXH2O(1),&
       &SWC(2)/MAXH2O(2),SITEWP(1),SITEWP(2),OUTFLOW(1),&
       &DAYTRANS_GR,DAYTRANS_SM,DAYTRANS_LG,SITELAI,&
       &RAIN,SNOW,EVAPPT,EXCESS,EVAP,SNOWPACK,SNWMELT
  892 format (F4.0,I4,21(F8.2))
  !      write(71,*) 'value of volmax used is= ', S(3)  !COMMENTED OUT 11/02 AJM

  return
end
