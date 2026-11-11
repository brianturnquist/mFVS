!============================!
!   Translate forest codes   !
!============================!
! AK $Id$
subroutine FORKOD
  implicit none

  !     TRANSLATES FOREST CODE INTO A SUBSCRIPT, IFOR, AND IF
  !     KODFOR IS ZERO, THE ROUTINE RETURNS THE DEFAULT CODE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.F77'
  include 'PLOT.F77'

  !  NATIONAL FORESTS:
  !   1004 = CHUGACH
  !   1005 = TONGASS (DEFAULT)
  !   1002 = TONGASS STIKINE AREA OLD CODE (MAPPED TO 1005 TONGASS)
  !   1003 = TONGASS CHATHAM AREA OLD CODE (MAPPED TO 1005 TONGASS)
  !  CANADIAN:
  !   701 = BC/MAKAH COMBINED CODE (MAPPED TO 703 BC)
  !   703 = BRITISH COLUMBIA
  !  BUREAU OF LAND MANAGEMENT
  !   713 = BLM LAND
  !  STATE:
  !   720 = STATE OF ALASKA, DEPARTMENT OF NATURAL RESOURCES

  !  TRIBAL CONSORTIUM
  !   7400 = TANANA CHIEFS CONFERENCE (ANY LOCATION)

  !  RESERVATION PSUEDO CODES:
  !   7401 = ARCTIC VILLAGE ANVSA
  !   7402 = AHTNA
  !   7403 = CHUGACH
  !   7404 = COOK INLET
  !   7405 = DOYON
  !   7406 = KONGIGANAK ANVSA
  !   7407 = NANWALEK ANVSA
  !   7408 = SELAWIK ANVSA
  !   8134 = ANNETTE ISLAND
  !   8135 = MAKAH INDIAN RES.
  !   8112 = QUILEUTE RES.

  ! Variables
  integer :: I,NUMFOR

  integer :: JFOR(17),KFOR(17)

  !  DATA STATEMENTS:

  ! SEAPROG OLD LOCATIONS 1002 AND 1003 MAPPED TO 1005

  data JFOR/1004,1005,703,713,720,7400, 7401,7402,7403,7404,7405, &
       & 7406,7407,7408,8134,8135,8112/
  data NUMFOR/17/,KFOR/1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1/
  logical :: FORFOUND = .false.,USEIGL = .true.

  select case (KODFOR)
     !       CROSSWALK FOR OLD TONGASS CODES
  case (1002)
     write(JOSTND,61)
     61 format(T12,'TONGASS: STIKINE AREA (1002) BEING ', &
          & 'MAPPED TO TONGASS (1005) FOR FURTHER PROCESSING.')
     IFOR = 2
  case (1003)
     write(JOSTND,62)
     62 format(T12,'TONGASS: CHATHAM AREA (1003) BEING ', &
          & 'MAPPED TO TONGASS (1005) FOR FURTHER PROCESSING.')
     IFOR = 2
  case (701)
     write(JOSTND,63)
     63 format(T12,'BC/MAKAH COMBINED CODE (701) BEING ', &
          & 'MAPPED TO BRITISH COLUMBIA (703) FOR FURTHER PROCESSING.')
     IFOR = 3
     !       END CROSSWALK FOR OLD TONGASS CODES

  case default

     !         CONFIRMS THAT KODFOR IS AN ACCEPTED FVS LOCATION CODE
     !         FOR THIS VARIANT FOUND IN DATA ARRAY JFOR
     do I = 1,NUMFOR
        if (KODFOR .eq. JFOR(I)) then
           IFOR = I
           FORFOUND = .true.
           exit
        end if
        10 continue
     end do
     !     LOCATION CODE ERROR TRAP
     if (.not. FORFOUND) then
        call ERRGRO (.true.,3)
        write(JOSTND,11) JFOR(IFOR)
        11 format(T12,'FOREST CODE USED IN THIS PROJECTION IS ',I4)
        USEIGL = .false.
     end if

  end select

  !     SET THE IGL VARIABLE ONLY IF DEFAULT FOREST IS USED
  !     GEOGRAPHIC LOCATION CODE: 1=NORTH, 2=CENTRAL, 3=SOUTH
  !     USED TO SET SOME EQUATIONS IN REGENERATION AND PERHAPS
  !     HEIGHT-DIAMETER IN DIFFERENT VARIANTS.
  if (USEIGL) IGL = KFOR(IFOR)
  KODFOR = JFOR(IFOR)

  !  SET DEFAULT TLAT, TLONG, AND ELEVATION VALUES, BY FOREST

  select case(KODFOR)
  case(1004)
     if (TLAT.eq.0)  TLAT = 61.22
     if (TLONG.eq.0) TLONG = -149.88
     if (ELEV.eq.0)  ELEV = 1.
  case(1005)
     if (TLAT.eq.0)  TLAT = 55.34
     if (TLONG.eq.0) TLONG = -131.64
     if (ELEV.eq.0)  ELEV = 1.
  case(703)
     if (TLAT.eq.0)  TLAT = 48.53
     if (TLONG.eq.0) TLONG = -123.37
     if (ELEV.eq.0)  ELEV = 1.
  case(713)
     if (TLAT.eq.0)  TLAT = 61.22
     if (TLONG.eq.0) TLONG = -149.89
     if (ELEV.eq.0)  ELEV = 2.
  case(720)
     if (TLAT.eq.0)  TLAT = 61.22
     if (TLONG.eq.0) TLONG = -149.89
     if (ELEV.eq.0)  ELEV = 2.
  case(7400)
     if (TLAT.eq.0)  TLAT = 64.84
     if (TLONG.eq.0) TLONG = -147.71
     if (ELEV.eq.0)  ELEV = 5.
  case(7401)
     if (TLAT.eq.0)  TLAT = 68.13
     if (TLONG.eq.0) TLONG = -145.54
     if (ELEV.eq.0)  ELEV = 10.
  case(7402)
     if (TLAT.eq.0)  TLAT = 62.14
     if (TLONG.eq.0) TLONG = -145.47
     if (ELEV.eq.0)  ELEV = 15.
  case(7403)
     if (TLAT.eq.0)  TLAT = 61.19
     if (TLONG.eq.0) TLONG = -149.89
     if (ELEV.eq.0)  ELEV = 1.
  case(7404)
     if (TLAT.eq.0)  TLAT = 61.21
     if (TLONG.eq.0) TLONG = -149.81
     if (ELEV.eq.0)  ELEV = 1.
  case(7405)
     if (TLAT.eq.0)  TLAT = 64.85
     if (TLONG.eq.0) TLONG = -147.72
     if (ELEV.eq.0)  ELEV = 4.
  case(7406)
     if (TLAT.eq.0)  TLAT = 59.99
     if (TLONG.eq.0) TLONG = -162.89
     if (ELEV.eq.0)  ELEV = 1.
  case(7407)
     if (TLAT.eq.0)  TLAT = 59.36
     if (TLONG.eq.0) TLONG = -151.92
     if (ELEV.eq.0)  ELEV = 2.
  case(7408)
     if (TLAT.eq.0)  TLAT = 66.73
     if (TLONG.eq.0) TLONG = -160.05
     if (ELEV.eq.0)  ELEV = 1.
  case(8134)
     if (TLAT.eq.0)  TLAT = 48.00
     if (TLONG.eq.0) TLONG = -124.61
     if (ELEV.eq.0)  ELEV = 1.
  case(8135)
     if (TLAT.eq.0)  TLAT = 55.12
     if (TLONG.eq.0) TLONG = -131.57
     if (ELEV.eq.0)  ELEV = 1.
  case(8112)
     if (TLAT.eq.0)  TLAT = 48.37
     if (TLONG.eq.0) TLONG = -124.61
     if (ELEV.eq.0)  ELEV = 3.
  end select
  return
end subroutine FORKOD
