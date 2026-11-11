!============================!
!   Translate forest codes   !
!============================!
! KT $Id$
subroutine FORKOD
  implicit none

  !     TRANSLATES FOREST CODE INTO A SUBSCRIPT, IFOR, AND IF
  !     KODFOR IS ZERO, THE ROUTINE RETURNS THE DEFAULT CODE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'
  include 'KOTCOM.f90'

  !  NATIONAL FORESTS:
  !  103 = BITTERROOT
  !  104 = IDAHO PANHANDLE
  !  105 = CLEARWATER
  !  106 = COEUR D'ALENE
  !  621 = COLVILLE
  !  110 = FLATHEAD
  !  113 = KANIKSU
  !  114 = KOOTENAI
  !  116 = LOLO
  !  117 = NEZ PERCE
  !  118 = ST. JOE
  !  613 = KANIKSU ADMINISTERED BY COLVILLE (MAPPED TO KANIKSU 113)

  !  RESERVATION PSUEDO CODES:
  !  8109 = KOOTENAI OFF-RES. TRUST LAND    (MAPPED TO KANIKSU   113)
  !  8133 = FLATHEAD RESERVATION            (MAPPED TO FLATHEAD  110)
  !  8137 = COEUR D'ALENE RESERVATION       (MAPPED TO ST. JOE   118)

  ! Variables
  logical :: FORFOUND
  integer :: JFOR(12),KFOR(12),NUMFOR,I,IFORDI,INTER,IFORE,IDIST,ICOMP
  data JFOR/103,104,105,106,621,110,113,114,116,117,118,613 /,&
       &NUMFOR /12/
  data KFOR/1,1,3,2,1,1,1,1,1,3,2,1 /
  FORFOUND = .false.

  !     CROSSWALK FOR RESERVATION PSUEDO CODES & LOCATION CODE
  select case (KODFOR)

  case (8109)
     write(JOSTND,72)
     72 format(/,'********   ',T12,'KOOTENAI OFF-RES. TRUST (8109) ',&
          &'BEING MAPPED TO KANIKSU NF (113) FOR FURTHER PROCESSING.')
     KODFOR = 11300000

  case (8133)
     write(JOSTND,75)
     75 format(/,'********   ',T12,'FLATHEAD RESERVATION (8133) ',&
          &'BEING MAPPED TO FLATHEAD NF (110) FOR FURTHER PROCESSING.')
     KODFOR = 11000000

  case (8137)
     write(JOSTND,76)
     76 format(/,'********   ',T12,'COEUR D''ALENE RESERV. (8137) ',&
          &'BEING MAPPED TO ST. JOE NF (118) FOR FURTHER PROCESSING.')
     KODFOR = 11800000

  end select
  !     END CROSSWALK FOR RESERVATION PSUEDO CODES & LOCATION CODE

  !     PAD SHORTER FOREST LOCATION CODES WITH ZEROS
  if (KODFOR .ge.100.and. KODFOR .le.9999999) then

     select case (KODFOR)
     case (100 : 999)
        KODFOR = KODFOR * 100000
     case (1000: 9999)
        KODFOR = KODFOR * 10000
     case (10000: 99999)
        KODFOR = KODFOR * 1000
     case (100000: 999999)
        KODFOR = KODFOR * 100
     case (1000000: 9999999)
        KODFOR = KODFOR * 10
     end select
  end if
  !     END PAD SHORTER FOREST LOCATION CODES WITH ZEROS

  if (KODFOR .gt.0) then
     do I = 1,NUMFOR
        IFORDI = KODFOR/100000
        if (IFORDI .eq. JFOR(I)) then
           IFOR = I
           if (I .eq.12) then
              write(JOSTND,21)
              21 format(/,'********',T12,'KANIKSU NF (613) BEING MAPPED ',&
                   &'TO KANIKSU (113) FOR FURTHER PROCESSING.')
              IFOR = 7
           end if
           IGL = KFOR(IFOR)
           FORFOUND = .true.
           exit
        end if
        10 continue
     end do

     !     LOCATION CODE ERROR TRAP
     if (.not. FORFOUND) then
        call ERRGRO (.true.,3)
        write(JOSTND,200) JFOR(IFOR)
        200 format(/,'********',T12,'FOREST CODE USED FOR THIS ',&
             &'PROJECTION IS',I4)
     end if

  end if

  !  SET DEFAULT KOTFOR TO 8 TO AVOID BLOWUPS IN REGENT & DGF
  !  GED 8-29-96.

  KOTFOR = 8

  !  THIS NEXT SECTION TRANSLATES THE FOREST CODES INTO LOCATION
  !  CODES FOR THE KOOTENAI VARIANT.

  INTER = KODFOR - 10000000
  if (INTER .lt.0) INTER = 0
  IFORE = INTER/100000
  IDIST = INTER/1000
  ICOMP = INTER-IDIST*1000
  if (IFORE .eq.10.or. IFORE .eq.0) KOTFOR = 8
  if (IFORE .eq.14)   KOTFOR = 7
  if (IDIST .eq.1402) KOTFOR = 2
  if (IDIST .eq.1403) KOTFOR = 3
  if (IDIST .eq.406) KOTFOR = 7
  if (IDIST .eq.1404.or. IDIST .eq.407) KOTFOR = 4
  if (IDIST .eq.1401) then
     if (ICOMP .ge.16.and. ICOMP .le.27) then
        KOTFOR = 1
     else
        KOTFOR = 2
     end if
  end if
  if (IDIST .eq.1405) then
     if ((ICOMP .ge.1.and. ICOMP .le.4) .or. (ICOMP .ge.8&
          &.and. ICOMP .le.19) .or. (ICOMP .eq.27)) then
        KOTFOR = 5
     else
        KOTFOR = 10
     end if
  end if
  if (IDIST .eq.1406) then
     if (ICOMP .ge.1.and. ICOMP .le.4) then
        KOTFOR = 6
     else
        KOTFOR = 9
     end if
  end if
  KOTNUM = KODFOR

  KODFOR = JFOR(IFOR)
  return
end
