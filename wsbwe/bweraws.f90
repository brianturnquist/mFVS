!=====================!
!   wsbwe : bweraws   !
!=====================!
! WSBWE $Id$
subroutine BWERAWS
  implicit none

  ! Part of the General Defoliator (GenDefol) model

  ! Processes RAWS daily climate data and loads the necessary
  ! weather parameters for the General Defoliator model.
  ! If a range of weather years has been specified, only those
  ! years are processed.

  !   PARAMETERS:

  ! Weather data is available from Western Region Climate Center (WRCC)
  ! and the format used in this subroutine is the FWX format.
  ! Weather station data is provided in two files, one of metadata and
  ! one of daily weather data. Examples of the metadata and daily weather
  ! records of this format follows.

  ! Weather station metadata file (ex: 20877-METADATA.txt)
  !+--------------+-----------+-------+-------+-------+------+--------+-------+---------+------+
  !| Station Name |   FPU ID  | Ver.  | Start |  End  | WRCC |    ID  |  Lat. |  Lon.   | Elev |
  !+--------------+-----------+-------+-------+-------+------+--------+-------+---------+------+
  !|Elk Creek     | NW_OR_009 | Dec05 | 07-00 | 12-04 | OECK | 352126 | 44.76 | -117.97 | 6576 |
  !+--------------+-----------+-------+-------+-------+------+--------+-------+---------+------+

  ! Daily weather data file (ex: OECK-unvalidated-vDec05.fwx)
  ! Column Name
  !   1-6  Station Number (4-character code assigned by WRCC)
  !   7-8  Year
  !   9-10 Month
  !  11-12 Day
  !  13    State of Weather Code
  !  14-16 Dry Bulb Temp (F)
  !  17-19 Relative Humidity (%)
  !  28    Wind Direction (8 point)
  !  29-31 Wind Speed (MPH)
  !  39-41 Max Temperature (F)
  !  42-44 Min Temperature (F)
  !  45-47 Max RH (%)
  !  48-50 Min RH (%)
  !  51    Season Code
  !  52-53 Precipitation Duration (Hrs)
  !  54-57 Precipitation Amount (IN 100ths))
  !  58-60 Lightning Activity Level
  !  61    Relative Humidity Variable Indicator (1=Wet Bulb, 2=RH%, 3=dew point)

  !                                       Temp F RH(%) Precip
  ! StanumYYMMDD Tmp RH        WSpd       MaxMinMaxMin HHIIII
  !   oeck0007100 68 37        6  3        69 44 63 333 0   0   2
  !   oeck0007110 75 30        8  2        75 49 64 303 0   0   2
  !   oeck0007128 80 34        7  2        80 57 49 303 0   0   2
  !   oeck0007130 80 21        5  3        80 57 48 203 0   0   2
  !   oeck0007140 74 27        7  3        84 50 54 163 0   0   2
  !   oeck0007150 65 39        7  1        75 45 79 223 0   0   2
  !   oeck0007163 73 27        7  3        74 51 44  83 0   0   2
  !   oeck0007172 70 32        5  4        74 58 37 253 0   0   2
  !   oeck0007188 64 47        8  2        70 54 47 313 0   0   2
  !   oeck0007198 71 40        2  3        74 52 75 323 2   3   2
  !   oeck0007201 77 27        5  4        77 52 73 273 3  13   2

  ! Weather data is processed one year at a time with the period
  ! beginning Jan 1 and ending Dec 31. Climate values relevant to
  ! Specific budworm life stage periods is accumulated and processed.
  ! Dates associated with specific events are specified and captured
  ! using Julian date instead of month and day.

  ! Degree days for larvae are based on 42 deg F (5.5 Deg C). Degree
  ! days for bud flushing is based on 40 deg F (4.4 Deg C). Warm
  ! degree days that after adult flight in the fall is based on 75
  ! deg F (23.9 deg C).

  ! Budworm life stage requirements (5.5 oC deg-days):
  ! peak emerg: 100, L2: 102, L3: 60, L4: 85, L5: 90, L6: 141,
  ! adult: 112; phen. assumptions from Sheehan et al 1989 except
  ! adults from Thomson et al. 1983.

  ! Julian dates of interest:
  !     1 - Jan 1, beginning of annual period
  !    79 - March 20, start accumulating degree days for bud flush
  !    ?? - Emergence of L2, begin small larvae period (100 DD)
  !    ?? - Mid L4, begin large larvae period (305 DD)
  !    ?? - Begin pupal stage (578 DD)
  !    ?? - Adult eclosion (emergence) (764 DD)
  !    ?? - (Date of 764 DD) + 4, begin accumulating warm DD at 75 F

  !  Lance R. David, USDA-FS, FMSC, Fort Collins, CO (METI, Inc)

  !   CALLED FROM: BWELIT

  !   SUBROUTINES AND FUNCTIONS CALLED:

  !      BWENOR - CALC. AND SCALE A NORMALLY DISTRIB. DEVIATE
  !      BWEMUL - SCALE A RANDOM NUMBER TO GET A MULTIPLIER

  !   PARAMETERS:

  !   BWEATH - ARRAY THAT STORES WEATHER PARAMETERS FOR 1 STATION
  !            (X,1)=MEAN,(X,2)=S.D., (X,3)=MINIMUM, (X,4)=MAX. FOR X=
  !            1=NO. OF DAYS FROM L2 EMERGENCE TO MID-L4
  !            2=NO. OF DAYS FROM MID-L4 THROUGH PUPAE
  !            3=NO. OF DAYS AS PUPAE
  !            4=WARM DEGREE-DAYS AFTER ADULT FLIGHT IN FALL
  !            5=NO. OF DAYS FROM L2 EMERGENCE TO BUD-FLUSHING
  !            6=TREE DEGREE-DAYS ACCUMULATED AT MID-L4
  !            7=MEAN PPT. FOR SMALL LARVAE
  !            8=MEAN PPT. FOR LARGE LARVAE
  !            9=MEAN PPT. FOR PUPAE
  !           10=MEAN PPT. DURING L2 EMERGENCE

  !  WEATH - Holds one year of weather data (3,365) (TMAX, TMIN, PRECIP)

  !  OBYRC - Outbreak year current, year of weather data to be processed.
  ! DDAYSF - Degree days for foliage
  ! DDAYSL - Degree days for larvae (budworm)
  !   JDAY - Julian day used to cycle through 1 year of weather data.
  !  LYRNG - Logical. True if range of RAWS weather years is to be processed.

  ! Common files
  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'BWECM2.f90'
  include 'BWEBOX.f90'
  include 'BWECOM.f90'

  ! Define variables
  ! Variables
  character(len=1) :: CTMP
  character(len=14) :: WSNAME
  character(len=9) :: FPUID
  character(len=5) :: WVER
  character(len=4) :: WRCCID
  character(len=6) :: WSTAID
  character(len=4) :: STANUM
  integer :: I,I2,I3,JDAY,KODE,WSTMM,WSTYY,WENDMM,WENDYY,WELEV,WYR,WMON,WDAY,DAYL2,DAYL4,DAYL7,DAYL8,DAYF
  real    :: DDAYS,DDAYSF,DDAYSL,MAXTC,MAXTF,MINTC,MINTF,WLAT,WLONG,PRECIP,WEATH(3,365),PPTL2E,PRDL24,&
       &PPTL24,PRDL46,PPTL46,PRDL7,PPTL7,DDFALL,PRDE2B
  logical :: LYRNG

  ! Load static variables

  ! initialize variables
  JDAY = 0
  IYRCNT = 0
  LYRNG = .false.

  ! Determine if range of weather years has been specified.

  ! FOR TESTING ONLY, LOAD STATIC YEAR RANGE. ******************
  !      IYRNG(1) = 1999
  !      IYRNG(2) = 2001
  !      WRITE(*,*) 'WEATHER YEAR RANGE: ',IYRNG(1),' TO ',IYRNG(2)
  if (IYRNG(1) .ne.0.and. IYRNG(2) .ne.0) LYRNG = .true.

  ! Open weather file.

  call MYOPEN (JOWE,WFNAME,3,133,1,1,1,0,KODE)

  ! Check if Open was successful

  if (KODE.eq.1) write(JOSTND,11)
  11 format (/T12,'**********   OPEN FAILED   **********')

  ! Read weather file
  ! First read the header record and store info. Note that when FPA RAWS
  ! data is downloaded, there is a metadata file and the .fwx file. It is
  ! expected here that the user has inserted the records from the metadata
  ! file as the first 5 records in the .fwx file. Following is a sample.

  !+--------------+-----------+-------+-------+-------+------+--------+-------+---------+------+
  !| Station Name |   FPU ID  | Ver.  | Start |  End  | WRCC |    ID  |  Lat. |  Lon.   | Elev |
  !+--------------+-----------+-------+-------+-------+------+--------+-------+---------+------+
  !|Elk Creek     | NW_OR_009 | Dec05 | 07-00 | 12-04 | OECK | 352126 | 44.76 | -117.97 | 6576 |
  !+--------------+-----------+-------+-------+-------+------+--------+-------+---------+------+
  !  oeck0007100 68 37        6  3        69 44 63 333 0   0   2
  !  oeck0007110 75 30        8  2        75 49 64 303 0   0   2
  !  oeck0007128 80 34        7  2        80 57 49 303 0   0   2

  ! Then advance, if necessary, to January 1 to beging processing one
  ! year of data.

  read (JOWE,10) CTMP
  read (JOWE,10) CTMP
  read (JOWE,10) CTMP
  10 format (A1)
  read (JOWE,20,end = 400) WSNAME,FPUID,WVER,WSTMM,WSTYY,WENDMM,&
       &WENDYY,WRCCID,WSTAID,WLAT,WLONG,WELEV
  20 format(1X,A14,2X,A9,3X,A5,3X,I2,1X,I2,3X,I2,&
       &1X,I2,3X,A4,3X,A6,3X,F5.2,3X,F7.2,2X,I6)
  read (JOWE,10) CTMP
  25 read (JOWE,30,end = 400) STANUM,WYR,WMON,WDAY,MAXTF,MINTF,PRECIP
  30 format (2X,A4,3(I2),26X,2(F3.0),9X,F4.0)

  !     Translate station name to upper case
  do I = 1,4
     call UPCASE(STANUM(I:I))
     40 continue
  end do

  if (STANUM .ne. WRCCID) then
     !       Header record WRCC ID does not match station ID for daily data.
     !       Write error message and exit
     write (*,*)&
          &'Weather data header record WRCC ID does not match data.'
     !       Need to set error flag
     goto 450
  end if

  if (WMON .eq.1.and. WDAY .eq.1) then

     !       At jan 1, start processing year.
     !       If a range of weather years is in play, check to see if this
     !       year is in it. If not in range, move on to next Jan 1.

     if (LYRNG) then
        !         Translate weather year from 2-digit to 4-digit.
        if (WYR .le.29) then
           WYR = 2000 + WYR
        else
           WYR = 1900 + WYR
        end if

        if (WYR .ge. IYRNG(1) .and. WYR .le. IYRNG(2)) then
           continue
        else
           goto 25
        end if
     end if

     !       Process this year of data.
     !       Set record counter (JDAY) to 1 and initialize variables used
     !       in the annual weather processing.

     JDAY = 1
     DAYF = 0
     DDAYSF = 0.0
     DDAYSL = 0.0
     DAYL2 = 0
     DAYL4 = 0
     DAYL7 = 0
     DAYL8 = 0
     TREEDD = 0.0

     goto 50
  else
     !       read next record
     goto 25
  end if

  !     Loop to process 1 year of data.
  !     Temperature data must be converted from Fahrenheit to Celsius.
  !     **** (If a lapse rate is to be applied, it would be done ****
  !     **** here standard lapse rate = 3.46 oF / 1000 ft)       ****
  !     Save data in array for final processing based on the degree day
  !     targets identified, then read next record.
  !     Accumulate degree days for:
  !        budworm life cycle phases based on 42 Deg F (5.5 Deg C)
  !        trees up to budworm mid-L4 based on 42 Deg F (5.5 Deg C)
  !        bud flush based on 40 Deg F (4.4 Deg C)
  !        adult budworm after flight based on 75 Deg F (23.9 Deg C)
  !     PRECIPitation is read in 100ths of inches and must be converted.

  50 continue
  MAXTC = (5.0 / 9.0) * (MAXTF - 32.0)
  MINTC = (5.0 / 9.0) * (MINTF - 32.0)
  WEATH(1,JDAY) = MAXTC
  WEATH(2,JDAY) = MINTC
  WEATH(3,JDAY) = PRECIP/100.0

  DDAYS = ((MAXTC + MINTC)/2.0) - 5.5

  if (DDAYS .gt.0.0) then
     DDAYSL = DDAYSL + DDAYS
     !       Accumulate tree degree days up to mid-L4
     if (DAYL4.eq.0.0) TREEDD = TREEDD + DDAYS
  else
     !       No degree days at 5.5 C skip to bud flush at 4.4 C
     goto 70
  end if

  if (DDAYSL .ge.100.0.and. DAYL2.eq.0) DAYL2 = JDAY
  if (DDAYSL .ge.305.0.and. DAYL4.eq.0) DAYL4 = JDAY
  if (DDAYSL .ge.578.0.and. DAYL7.eq.0) DAYL7 = JDAY
  if (DDAYSL .ge.764.0.and. DAYL8.eq.0) DAYL8 = JDAY

  70 continue

  if (JDAY .ge.79.and. DAYF .eq.0) then
     !       Accumulate DDays after March 20 for foliage (bud flush)
     !       Target number of DDays is 220.
     DDAYS = ((MAXTC + MINTC)/2.0) - 4.4
     if (DDAYS .gt.0.0) DDAYSF = DDAYSF + DDAYS
     if (DDAYSF .ge.220.0) DAYF = JDAY
  end if

  if (JDAY .eq.365) then
     !       Have completed reading and processing 1 year of weather data.
     !       Now calculate values that are based on the days identified above.

     !       Accumulate precipitation around L2 emergence

     PPTL2E = 0.0
     I2 = DAYL2 - 5
     I3 = DAYL2 + 5
     do I = I2,I3
        PPTL2E = PPTL2E + WEATH(3,I)
     end do

     !       Accumulate precipitation L2 to mid-L4 (small larvae)
     !       Calc period length.
     PRDL24 = DAYL4 - DAYL2
     PPTL24 = 0.0
     do I = DAYL2,DAYL4
        PPTL24 = PPTL24 + WEATH(3,I)
     end do

     !       Accumulate precipitation mid-L4 to L7 (large larvae)
     !       Calc period length.
     PRDL46 = DAYL7 - DAYL4
     PPTL46 = 0.0
     do I = DAYL4,DAYL7
        PPTL46 = PPTL46 + WEATH(3,I)
     end do

     !       Accumulate precipitation during L7 (pupal period)
     !       Calc period length.
     PRDL7 = DAYL8 - DAYL7
     PPTL7 = 0.0
     do I = DAYL7,DAYL8
        PPTL7 = PPTL7 + WEATH(3,I)
     end do

     !       Accummulate warm degree days 75 Deg F (23.9 Deg C) after flight.
     I2 = DAYL8 + 4
     DDFALL = 0.0
     do I = I2, 365
        DDAYS = ((WEATH(1,I) + WEATH(2,I))/2.0) - 23.9
        if (DDAYS .gt.0.0) DDFALL = DDFALL + DDAYS
     end do

     !       Length of period from L2 emergence to bud flush.
     PRDE2B = DAYF - DAYL2
     goto 300
  else
     !       Read the next weather record. Should not hit the end of file,
     !       if so, it is an incomplete year.
     read (JOWE,30,end = 400) STANUM,WYR,WMON,WDAY,MAXTF,MINTF,PRECIP
     JDAY = JDAY + 1
     goto 50
  end if

  !     Save budworm variables for year just processed.
  !     Translate weather year from 2-digit to 4-digit.

  300 IYRCNT = IYRCNT + 1

  if (WYR .le.29) then
     WYR = 2000 + WYR
  else
     WYR = 1900 + WYR
  end if

  write (JOSTND, 310) WYR, DAYL2, DAYL4, DAYL7, DAYL8,&
       &DAYF, DDAYSF, DDAYSL
  310 format (/,6X,'WYR=',I4,' DAYL2=',I3,' DAYL4=',I3, ' DAYL7=',I3,&
       &' DAYL8=',I3, ' DAYF=',I3,' DDAYSF=',F6.1,' DDAYSL=',F6.1)

  BWPRMS(1,IYRCNT) = PRDL24
  BWPRMS(2,IYRCNT) = PRDL46
  BWPRMS(3,IYRCNT) = PRDL7
  BWPRMS(4,IYRCNT) = DDFALL
  BWPRMS(5,IYRCNT) = PRDE2B
  BWPRMS(6,IYRCNT) = TREEDD
  BWPRMS(7,IYRCNT) = PPTL24
  BWPRMS(8,IYRCNT) = PPTL46
  BWPRMS(9,IYRCNT) = PPTL7
  BWPRMS(10,IYRCNT) = PPTL2E
  BWPRMS(11,IYRCNT) = WYR

  !     Process next year if less than 50.
  if (IYRCNT .lt.50) then
     goto 25
  end if

  !     End of file was reached, indicating an incomplete year.
  !     Write error message if less than 1 complete year has been processed.
  !     (Maybe add some corective measure, if possible, so that the
  !     run does not terminate. To be determined with Kathy Sheehan)

  400 if (IYRCNT .eq.0) then
     write (*,*) 'No complete years of weather data present.'
  else
     write (*,410) IYRCNT, WFNAME
     410 format (/,I2,' years of weather data processed from file: ',&
          &A)

     write (JOSTND,*) '       PRDL24  PRDL46   PRDL7  DDFALL  ',&
          &'PRDE2B  TREEDD  PPTL24  PPTL46   PPTL7  PPTL2E     WYR'
     do I2 = 1, IYRCNT
        write (JOSTND,420) (BWPRMS(I,I2), I = 1,11)
        420 format (/,5X,11(2X,F6.1))
     end do
  end if

  !     Close weather data file and set final variables.

  450 close (JOWE)
  IWYR = 0

  return
end
