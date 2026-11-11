!============================!
!   Translate forest codes   !
!============================!
! WS $Id$
subroutine FORKOD
  implicit none

  !     TRANSTLATES FOREST CODE INTO A SUBSCRIPT, IFOR, AND IF
  !     KODFOR IS ZERO, THE ROUTINE RETURNS THE DEFAULT CODE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'

  !  NATIONAL FORESTS:
  !  503 = ELDORADO
  !  511 = PLUMAS
  !  513 = SEQUOIA
  !  515 = SIERRA
  !  516 = STANISLAUS
  !  517 = TAHOE
  !  501 = ANGELES (MAPPED TO SEQUOIA)
  !  502 = CLEVELAND (MAPPED TO SEQUOIA)
  !  504 = INYO (MAPPED TO SEQUOIA)
  !  507 = LOS PADRES (MAPPED TO SEQUOIA)
  !  512 = SAN BERNADINO (MAPPED TO SEQUOIA)
  !  519 = LAKE TAHOE BASIN MGMT UNIT (MAPPED TO ELDORADO)
  !  417 = TOIYABE (MAPPED TO STANISLAUS)

  !  RESERVATION PSUEDO CODES:
  !  7712 = PYRAMID LAKE PAIUTE RES.     (MAPPED TO 417 TOIYABE)
  !  7713 = RENO-SPARKS INDIAN COLONY    (MAPPED TO 417 TOIYABE)
  !  7715 = WALKER RIVER RES.            (MAPPED TO 417 TOIYABE)
  !  7801 = BERRY CREEK RANCHERIA        (MAPPED TO 511 PLUMAS)
  !  7808 = ENTERPRISE RANCHERIA         (MAPPED TO 511 PLUMAS)
  !  7814 = FORT INDEPENDENCE RES.       (MAPPED TO 504 INYO)
  !  7817 = NORTH FORK OFF-RES. TL       (MAPPED TO 515 SIERRA)
  !  7825 = BISHOP RES.                  (MAPPED TO 504 INYO)
  !  7827 = TULE RIVER RES.              (MAPPED TO 513 SEQUOIA)
  !  7828 = LONE PINE RES.               (MAPPED TO 504 INYO)
  !  7832 = TUOLUMNE RANCHERIA           (MAPPED TO 516 STANISLAUS)
  !  7835 = TIMBI-SHA SHOSHONE RES.      (MAPPED TO 504 INYO)
  !  7847 = AGUA CALIENTE INDIAN RES.    (MAPPED TO 512 SAN BERNARDINO)
  !  7849 = CAHUILLA RES.                (MAPPED TO 512 SAN BERNARDINO)
  !  7850 = CAMPO INDIAN RES.            (MAPPED TO 502 CLEVELAND)
  !  7851 = CAPITAN GRANDE RES.          (MAPPED TO 502 CLEVELAND)
  !  7852 = BARONA RES.                  (MAPPED TO 502 CLEVELAND)
  !  7853 = EWIIAAPAAYP RES.             (MAPPED TO 502 CLEVELAND)
  !  7854 = INAJA AND COSMIT RES.        (MAPPED TO 502 CLEVELAND)
  !  7855 = LA JOLLA RES.                (MAPPED TO 502 CLEVELAND)
  !  7856 = LOS COYOTES RES.             (MAPPED TO 502 CLEVELAND)
  !  7857 = MANZANITA RES.               (MAPPED TO 502 CLEVELAND)
  !  7858 = MESA GRANDE RES.             (MAPPED TO 502 CLEVELAND)
  !  7859 = MORONGO RES.                 (MAPPED TO 512 SAN BERNARDINO)
  !  7860 = PALA RES.                    (MAPPED TO 502 CLEVELAND)
  !  7861 = PAUMA AND YUIMA RES.         (MAPPED TO 502 CLEVELAND)
  !  7862 = PECHANGA RES.                (MAPPED TO 502 CLEVELAND)
  !  7863 = SANTA ROSA RES.              (MAPPED TO 512 SAN BERNARDINO)
  !  7865 = SANTA YSABEL RES.            (MAPPED TO 502 CLEVELAND)
  !  7866 = SOBOBA RES.                  (MAPPED TO 512 SAN BERNARDINO)
  !  7867 = VIEJAS RES.                  (MAPPED TO 502 CLEVELAND)

  ! Variables
  integer :: JFOR(13),KFOR(13),NUMFOR,I,KFOR1,KDIS
  logical :: USEIGL,FORFOUND

  !  DATA STATEMENTS:
  !     KFOR IS USED IN THIS VARIANT TO SEND A GEOGRAPHIC LOCATOR TO
  !     **DGF** FOR SETTING THE FORCON VARIABLE FOR THE ANGELES NF
  !     THIS GETS SET AFTER 30 CONTINUE

  data JFOR/503,511,513,515,516,517,501,502,504,507,512,519,417/
  data NUMFOR/13/
  data KFOR/13*1/

  USEIGL = .true.
  FORFOUND = .false.

  select case (KODFOR)

     !       ACCOUNT FOR REGION/FOREST/DISTRICT TO BE INPUT
  case (40000:)
     KFOR1 = INT(real(KODFOR)/100.0)
     KDIS = KODFOR-KFOR1*100

     select case (KFOR1)
     case (501,502,504,507,512,519,417)
        IFOR = MINLOC(ABS(JFOR - KFOR1), 1)
     case (503)
        IFOR = MINLOC(ABS(JFOR - KFOR1), 1)
        TLAT = 38.
        if (KDIS .eq.53) TLAT = 39.
     case (511)
        IFOR = MINLOC(ABS(JFOR - KFOR1), 1)
        TLAT = 39.
        if (KDIS .le.52) TLAT = 40.
     case (513)
        IFOR = MINLOC(ABS(JFOR - KFOR1), 1)
        TLAT = 35.
        if (KDIS .le.52) TLAT = 36.
     case (515)
        IFOR = MINLOC(ABS(JFOR - KFOR1), 1)
        TLAT = 37.
        if (KDIS .eq.54) TLAT = 36.
     case (516)
        IFOR = MINLOC(ABS(JFOR - KFOR1), 1)
        TLAT = 38.
        if (KDIS .eq.54) TLAT = 37.
     case (517)
        IFOR = MINLOC(ABS(JFOR - KFOR1), 1)
        TLAT = 39.
     case default
        call ERRGRO (.true.,3)
        write(JOSTND,11) JFOR(IFOR)
        11 format(/,'********',T12,'FOREST CODE USED IN THIS ',&
             &'PROJECTION IS',I4)
        USEIGL = .false.
     end select

     !       CROSSWALK FOR RESERVATION PSUEDO CODES & LOCATION CODE
  case (7712)
     write(JOSTND,61)
     61 format(/,'********',T12,'PYRAMID LAKE PAIUTE RES. (7712) ',&
          &'BEING MAPPED TO TOIYABE NF (417) FOR FURTHER PROCESSING.')
     IFOR = 13
  case (7713)
     write(JOSTND,62)
     62 format(/,'********',T12,'RENO-SPARKS INDIAN COLONY (7713) ',&
          &'BEING MAPPED TO TOIYABE NF (417) FOR FURTHER PROCESSING.')
     IFOR = 13
  case (7715)
     write(JOSTND,63)
     63 format(/,'********',T12,'WALKER RIVER RES. (7715) BEING ',&
          &'MAPPED TO TOIYABE NF (417) FOR FURTHER PROCESSING.')
     IFOR = 13
  case (7801)
     write(JOSTND,64)
     64 format(/,'********',T12,'BERRY CREEK RANCHERIA (7801) BEING ',&
          &'MAPPED TO PLUMAS NF (511) FOR FURTHER PROCESSING.')
     IFOR = 2
  case (7808)
     write(JOSTND,65)
     65 format(/,'********',T12,'ENTERPRISE RANCHERIA (7808) BEING ',&
          &'MAPPED TO PLUMAS NF (511) FOR FURTHER PROCESSING.')
     IFOR = 2
  case (7814)
     write(JOSTND,66)
     66 format(/,'********',T12,'FORT INDEPENDENCE RES. (7814) ',&
          &'BEING MAPPED TO INYO NF (504) FOR FURTHER PROCESSING.')
     IFOR = 9
  case (7817)
     write(JOSTND,67)
     67 format(/,'********',T12,'NORTH FORK OFF-RES. TL (7817) ',&
          &'BEING MAPPED TO SIERRA NF (515) FOR FURTHER PROCESSING.')
     IFOR = 4
  case (7825)
     write(JOSTND,68)
     68 format(/,'********',T12,'BISHOP RES. (7825) BEING ',&
          &'MAPPED TO INYO NF (504) FOR FURTHER PROCESSING.')
     IFOR = 9
  case (7827)
     write(JOSTND,69)
     69 format(/,'********',T12,'TULE RIVER RES. (7827) BEING ',&
          &'MAPPED TO SEQUOIA NF (513) FOR FURTHER PROCESSING.')
     IFOR = 3
  case (7828)
     write(JOSTND,70)
     70 format(/,'********',T12,'LONE PINE RES. (7828) BEING ',&
          &'MAPPED TO INYO NF (504) FOR FURTHER PROCESSING.')
     IFOR = 9
  case (7832)
     write(JOSTND,71)
     71 format(/,'********',T12,'TUOLUMNE RANCHERIA (7832) BEING ',&
          &'MAPPED TO STANISLAUS NF (516) FOR FURTHER PROCESSING.')
     IFOR = 5
  case (7835)
     write(JOSTND,72)
     72 format(/,'********',T12,'TIMBI-SHA SHOSHONE RES. (7835) ',&
          &'BEING MAPPED TO INYO NF (504) FOR FURTHER PROCESSING.')
     IFOR = 9
  case (7847)
     write(JOSTND,73)
     73 format(/,'********',T12,'AGUA CALIENTE INDIAN RES. (7847) ',&
          &'BEING MAPPED TO SAN BERNARDINO NF (512) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 11
  case (7849)
     write(JOSTND,74)
     74 format(/,'********',T12,'CAHUILLA RES. (7849) BEING ',&
          &'MAPPED TO SAN BERNARDINO NF (512) FOR FURTHER PROCESSING.')
     IFOR = 11
  case (7850)
     write(JOSTND,75)
     75 format(/,'********',T12,'CAMPO INDIAN RES. (7850) BEING ',&
          &'MAPPED TO CLEVELAND NF (502) FOR FURTHER PROCESSING.')
     IFOR = 8
  case (7851)
     write(JOSTND,76)
     76 format(/,'********',T12,'CAPITAN GRANDE RES. (7851) BEING ',&
          &'MAPPED TO CLEVELAND NF (502) FOR FURTHER PROCESSING.')
     IFOR = 8
  case (7852)
     write(JOSTND,77)
     77 format(/,'********',T12,'BARONA RES. (7852) BEING ',&
          &'MAPPED TO CLEVELAND NF (502) FOR FURTHER PROCESSING.')
     IFOR = 8
  case (7853)
     write(JOSTND,78)
     78 format(/,'********',T12,'EWIIAAPAAYP RES. (7853) BEING ',&
          &'MAPPED TO CLEVELAND NF (502) FOR FURTHER PROCESSING.')
     IFOR = 8
  case (7854)
     write(JOSTND,79)
     79 format(/,'********',T12,'INAJA AND COSMIT RES. (7854) BEING ',&
          &'MAPPED TO CLEVELAND NF (502) FOR FURTHER PROCESSING.')
     IFOR = 8
  case (7855)
     write(JOSTND,80)
     80 format(/,'********',T12,'LA JOLLA RES. (7855) BEING ',&
          &'MAPPED TO CLEVELAND NF (502) FOR FURTHER PROCESSING.')
     IFOR = 8
  case (7856)
     write(JOSTND,81)
     81 format(/,'********',T12,'LOS COYOTES RES. (7856) BEING ',&
          &'MAPPED TO CLEVELAND NF (502) FOR FURTHER PROCESSING.')
     IFOR = 8
  case (7857)
     write(JOSTND,82)
     82 format(/,'********',T12,'MANZANITA RES. (7857) BEING ',&
          &'MAPPED TO CLEVELAND NF (502) FOR FURTHER PROCESSING.')
     IFOR = 8
  case (7858)
     write(JOSTND,83)
     83 format(/,'********',T12,'MESA GRANDE RES. (7858) BEING ',&
          &'MAPPED TO CLEVELAND NF (502) FOR FURTHER PROCESSING.')
     IFOR = 8
  case (7859)
     write(JOSTND,84)
     84 format(/,'********',T12,'MORONGO RES. (7859) BEING MAPPED ',&
          &'TO SAN BERNARDINO NF (512) FOR FURTHER PROCESSING.')
     IFOR = 11
  case (7860)
     write(JOSTND,85)
     85 format(/,'********',T12,'PALA RES. (7860) BEING ',&
          &'MAPPED TO CLEVELAND NF (502) FOR FURTHER PROCESSING.')
     IFOR = 8
  case (7861)
     write(JOSTND,86)
     86 format(/,'********',T12,'PAUMA AND YUIMA RES. (7861) BEING ',&
          &'MAPPED TO CLEVELAND NF (502) FOR FURTHER PROCESSING.')
     IFOR = 8
  case (7862)
     write(JOSTND,87)
     87 format(/,'********',T12,'PECHANGA RES. (7862) BEING ',&
          &'MAPPED TO CLEVELAND NF (502) FOR FURTHER PROCESSING.')
     IFOR = 8
  case (7863)
     write(JOSTND,88)
     88 format(/,'********',T12,'SANTA ROSA RES. (7863) BEING ',&
          &'MAPPED TO SAN BERNARDINO NF (512) FOR FURTHER PROCESSING.')
     IFOR = 11
  case (7865)
     write(JOSTND,89)
     89 format(/,'********',T12,'SANTA YSABEL RES. (7865) BEING ',&
          &'MAPPED TO CLEVELAND NF (502) FOR FURTHER PROCESSING.')
     IFOR = 8
  case (7866)
     write(JOSTND,90)
     90 format(/,'********',T12,'SOBOBA RES. (7866) BEING MAPPED ',&
          &'TO SAN BERNARDINO NF (512) FOR FURTHER PROCESSING.')
     IFOR = 11
  case (7867)
     write(JOSTND,91)
     91 format(/,'********',T12,'VIEJAS RES. (7867) BEING ',&
          &'MAPPED TO CLEVELAND NF (502) FOR FURTHER PROCESSING.')
     IFOR = 8
     !       END CROSSWALK FOR RESERVATION PSUEDO CODES & LOCATION CODE

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

     !         LOCATION CODE ERROR TRAP
     if (.not. FORFOUND) then
        call ERRGRO (.true.,3)
        write(JOSTND,13) JFOR(IFOR)
        13 format(/,'********',T12,'FOREST CODE USED IN THIS ',&
             &'PROJECTION IS',I4)
        USEIGL = .false.
     end if

  end select

  !     FOREST MAPPING CORRECTION
  select case (IFOR)
  case (7)
     write(JOSTND,21)
     21 format(/,'********',T12,'ANGELES NF (501) BEING MAPPED TO ',&
          &'SEQUOIA (513) FOR FURTHER PROCESSING.')
     IFOR = 3
  case (8)
     write(JOSTND,22)
     22 format(/,'********',T12,'CLEVELAND NF (502) BEING MAPPED TO ',&
          &'SEQUOIA (513) FOR FURTHER PROCESSING.')
     IFOR = 3
  case (9)
     write(JOSTND,23)
     23 format(/,'********',T12,'INYO NF (504) BEING MAPPED TO ',&
          &'SEQUOIA (513) FOR FURTHER PROCESSING.')
     IFOR = 3
  case (10)
     write(JOSTND,24)
     24 format(/,'********',T12,'LOS PADRES NF (507) BEING MAPPED ',&
          &'TO SEQUOIA (513) FOR FURTHER PROCESSING.')
     IFOR = 3
  case (11)
     write(JOSTND,25)
     25 format(/,'********',T12,'SAN BERNARDINO NF (512) BEING ',&
          &'MAPPED TO SEQUOIA (513) FOR FURTHER PROCESSING.')
     IFOR = 3
  case (12)
     write(JOSTND,26)
     26 format(/,'********',T12,'LAKE TAHOE BASIN MGMT UNIT (519) ',&
          &'BEING MAPPED TO ELDORADO (503) FOR FURTHER PROCESSING.')
     IFOR = 1
  case (13)
     write(JOSTND,27)
     27 format(/,'********',T12,'TOIYABE NF (417) BEING MAPPED TO ',&
          &'STANISLAUS (516) FOR FURTHER PROCESSING.')
     IFOR = 5
  end select

  !     SET THE IGL VARIABLE ONLY IF DEFAULT FOREST IS USED
  !     GEOGRAPHIC LOCATION CODE: 1=NORTH, 2=CENTRAL, 3=SOUTH
  !     USED TO SET SOME EQUATIONS IN REGENERATION AND PERHAPS
  !     HEIGHT-DIAMETER IN DIFFERENT VARIANTS.
  if (USEIGL .and. IFOR .eq.7) then
     IGL = 2
  else if (USEIGL) then
     IGL = KFOR(IFOR)
  end if

  KODFOR = JFOR(IFOR)
  return
end
