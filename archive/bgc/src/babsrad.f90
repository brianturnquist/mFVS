!===============================!
!   archive/bgc/src : babsrad   !
!===============================!
subroutine BABSRAD

  ! SUBROUTINE MODELS THE MOVEMENT AND ABSORPTION OF SOLAR RADIATION
  ! THROUGH THE CANOPY. TWO MEASURES OF RADIATIONS ARE ACOUNTED FOR:
  ! (1) INSTANTANEOUS PAR---CALCULATED A LA HUNT, AS A MAXIMUM AT SOLAR NOON,
  ! WHICH IS USED IN THE CALCULATION OF CONDUCTANCE AND PHOTOSYNTHESIS, AND
  ! (2) TOTAL DAILY SHORTWAVE RADIATION WHICH IS USED TO DRIVE SNOW MELT,
  ! EVAPORATION, AND TRANSPIRATION.

  ! SUBROUTINE IS CALLED DAILY

  ! NOTE: RADIATION(2)=QNOON in umol/m2/sec before expansion
  !       RADIATION(1)=QDAY in kj/m2/day before expansion

  ! Variables
  integer :: J,I1
  real    :: QDAY,QNOON
  real    :: Q1,Q2,Q1DIR,Q2DIR,MAXRAD
  ! Common blocks
  include 'ENTITY.f90'
  include 'SITE.f90'

  Q1DIR = 0.
  Q2DIR = 0.
  SUMCOV = 0.

  ! FIRST CALCULATE THE AMOUNT OF TOTAL DAILY RADIATION INCIDENT AT THE
  ! TOP OF EACH CANOPY ZONE.  (kJ/m2/day).

  QDAY = XRAD*(1.-S(6))
  ! NOW CALCULATE THE INSTANTANEOUS INCIDENT PAR AT SOLAR NOON (umol/m2/sec)
  call MAXPPFD(JD,TR,SLOPE,ASPECT,LAT,MAXRAD)
  QNOON = MAXRAD

  ! ATTENUATE QDAY AND QNOON THROUGH THE CANOPY ZONES. CALCULATE THE AMOUNT
  ! ABSORBED BY EACH ENTITY IN EACH ZONE, AND WHAT THE DIRECT AND DIFFUSE
  ! RADIATION IS AT THE TOP OF THE NEXT ZONE.
  Q1 = QDAY
  Q2 = QNOON
  RADTOP = Q1
  do L = NUM,1,-1 !canopy layer loop
     I1 = INDEX(L)
     I2 = INDEX(L+1)
     ! reference zone according to lower boundary and define light incident
     ! at the top of that zone (kj/m2/day and umol/m2/sec)
     J = I1
     ZQ1(J) = Q1
     ZQ2(J) = Q2
     do I = 1,NB !entity loop
        if (LAI(I,J).gt.0.0) then
           ! radiation absorbed by entity=i, zone=j. (kj/day and umol/sec)
           ZQ1ABS(I,J) = ZQ1(J)*(1-EXP(B2(2)*LAI(I,J)/B2(15)))*BASE(I,J)
           ZQ2ABS(I,J) = ZQ2(J)*(1-EXP(B2(2)*LAI(I,J)/B2(15)))*BASE(I,J)
           ! accumulate rad across entities within zone and expand. (kj/day & umol/sec)
           TZQ1(J) = TZQ1(J)+ZQ1ABS(I,J)*EXPAND(I)
           TZQ2(J) = TZQ2(J)+ZQ2ABS(I,J)*EXPAND(I)
           ! accumulate rad across zones for each entity. (kj/day & umol/sec)
           ETZQ1(I) = ETZQ1(I)+ZQ1ABS(I,J)
           ETZQ2(I) = ETZQ2(I)+ZQ2ABS(I,J)
           ! use write statements to track variables
           !           IF(JD.EQ.200) WRITE(*,*)  JD, I, J, QNOON, ZQ2(J), Q2
        end if
        20 continue
     end do
     ! calculate total light passing to next zone (kj/m2/day & umol/m2/sec)
     Q1 = (ZQ1(J)*AREA-TZQ1(J)) / AREA
     Q2 = (ZQ2(J)*AREA-TZQ2(J)) / AREA
     ! set Q1 and Q2 to zero if less than .01
     !        IF(Q1.LT.01) Q1=0.0
     !        IF(Q2.LT.01) Q2=0.0
     ! sum cover and accumulate thru layer j.
     ! note: ZCOVER is defined in subroutine LAIJ.
     SUMCOV = SUMCOV+ZCOVER(J)
     ! calculate potential direct beam radiation available to next layer
     ! note: this is the light in the gaps of the canopy.
     Q1DIR = QDAY*(AREA-SUMCOV)
     Q2DIR = QNOON*(AREA-SUMCOV)
     ! zero-out potential direct rad. if ZCOVER exceeds 10,000 m2
     if (Q1DIR.lt.0.) then
        Q1DIR = 0.
     end if
     if (Q2DIR.lt.0.) then
        Q2DIR = 0.
     end if
     !!!! if the Q?DIR value is needed in a future subroutine,
     !!!! do a new variable assignment here. ie, NEW_VAR=Q1DIR.

     ! zero-out dimensioned variables for next day
     TZQ1(J) = 0.
     TZQ2(J) = 0.
     30 continue
  end do
  RADBOT = Q1
  return
end

subroutine MAXPPFD(JD,TR,SLOPE,ASPECT,LAT,MAXPFD)

  ! TAKEN DIRECTLY FROM BIOM-BGC. WRITTEN BY E.R. HUNT.
  ! SUBROUTINE RETURNS THE MIDDAY MAXIMUM PPFD FOR A GIVEN YEARDAY (JD) AND
  ! TRANSMISIVITY (TR). SLOPE=SLOPE IN PERCENT, ASPECT IN DEGREES,
  ! LATITUDE IN DEGREES.

  ! DIFFUSE RADIATION AFTER GATES 1980

  ! Variables
  integer :: SOLARCON,AZIMUTH,XDAY
  real    :: ASPECT,SLOPE
  real    :: HOUR,EPHOTON,RAD2PAR,SLP,SSLP,DECLIN,ZENITH,MU,LAT
  real    :: ALT,COSINCID,TRMOD,DIRPPFD,DIFPPFD,TRD,MAXPFD

  ! DEFINE FUNCTION FOR CONVERTING ANGLES TO RADIANS

  XRADS(X) = X*2.*3.1416/360.

  ! DEFINE CONSTANTS

  !     Solar Noon
  HOUR = 0.
  ! Photosynthetic Photon Energy (umol/J)
  EPHOTON = 4.55
  ! Campbell's PAR is 50% of solar radiation
  RAD2PAR = 0.5
  ! Solar Constant (W/m2)
  SOLARCON = 1365
  ! Azimuth for Northern Hemisphere (degrees)
  AZIMUTH = 180

  ! BEGIN CALCULATION
  SSLP = SLOPE/100.
  SLP = XRADS(90.)-ATAN(SSLP)
  !-----get declination in degrees
  if ((JD+9).gt.364) then
     XDAY = JD+9-364
  else
     XDAY = JD+9
  end if
  DECLIN = 23.47*(-COS(XDAY*2.*3.1416/365.))
  !-----mu is the cos of the zenith angle
  MU = SIN(XRADS(LAT))*SIN(XRADS(DECLIN))+&
       &COS(XRADS(LAT))*COS(XRADS(DECLIN))*COS(XRADS(HOUR))
  !-----get zenith in degrees
  ZENITH = ACOS(MU)*360./(2.*3.1416)
  !-----get altitude in radians. Complement to zenith.
  ALT = XRADS(90.-ZENITH)
  COSINCID = COS(SLP)*COS(ALT)*COS(XRADS(AZIMUTH-ASPECT))&
       &+SIN(SLP)*SIN(ALT)
  if (TR.lt.0.01) THENTR = 0.01
  TRMOD = TR**(1./MU)
  DIRPPFD = SOLARCON*RAD2PAR*EPHOTON*COSINCID*TRMOD
  TRD = 0.271-0.294*TRMOD
  if (TRD.lt.0.01) THENTRD = 0.01
  DIFPPFD = SOLARCON*RAD2PAR*EPHOTON*TRD
  !-----calculate total
  MAXPFD = DIRPPFD+DIFPPFD
  return
end
