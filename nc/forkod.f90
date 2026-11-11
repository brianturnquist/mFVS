!============================!
!   Translate forest codes   !
!============================!
! NC $Id$
subroutine FORKOD
  implicit none

  !     TRANSLATES FOREST CODE INTO A SUBSCRIPT, IFOR, AND IF
  !     KODFOR IS ZERO, THE ROUTINE RETURNS THE DEFAULT CODE.

  ! Common blocks
  include 'PRGPRM.f90'
  include  'PLOT.f90'

  include  'CONTRL.f90'

  !  NATIONAL FORESTS:
  !  505 = KLAMATH
  !  510 = SIX RIVERS
  !  514 = SHASTA-TRINITY
  !  611 = SISKIYOU
  !  705 = HOOPA VALLEY INDIAN RESERVATION
  !  715 = SIMPSON TIMBER
  !        **715 is new code introduced to eliminate
  !           redundancy across variants. 715 will be
  !           mapped to 800 for processing
  !  800 = SIMPSON TIMBER
  !  712 = BLM COOS BAY
  !  518 = TRINITY (MAPPED TO SHASTA-TRINITY)
  !  507 = LOS PADRES (MAPPED TO SIX RIVERS)
  !  508 = MENDOCINO (MAPPED TO SIX RIVERS)

  !  RESERVATION PSUEDO CODES:
  !  7806 = DRY CREEK RANCHERIA              (MAPPED TO 508 MENDOCINO)
  !  7807 = ROBINSON RANCHERIA               (MAPPED TO 508 MENDOCINO)
  !  7810 = HOPLAND RANCHERIA                (MAPPED TO 508 MENDOCINO)
  !  7813 = LAYTONVILLE RANCHERIA            (MAPPED TO 508 MENDOCINO)
  !  7815 = MANCHESTER-PT. ARENA RANCHERIA   (MAPPED TO 508 MENDOCINO)
  !  7816 = MIDDLETOWN RANCHERIA             (MAPPED TO 508 MENDOCINO)
  !  7820 = REDWOOD VALLEY RANCHERIA         (MAPPED TO 508 MENDOCINO)
  !  7821 = ROUND VALLEY OFF-RES. TRUST LAND (MAPPED TO 508 MENDOCINO)
  !  7824 = STEWARTS POINT RANCHERIA         (MAPPED TO 508 MENDOCINO)
  !  7830 = SHERWOOD VALLEY RANCHERIA        (MAPPED TO 508 MENDOCINO)
  !  7831 = SULPHUR BANK RANCHERIA           (MAPPED TO 508 MENDOCINO)
  !  7833 = UPPER LAKE RANCHERIA             (MAPPED TO 508 MENDOCINO)
  !  7834 = COYOTE VALLEY RESERVATION        (MAPPED TO 508 MENDOCINO)
  !  7839 = ELK VALLEY OFF-RES. TRUST LAND   (MAPPED TO 510 SIX RIVERS)
  !  7841 = YUROK RESERVATION                (MAPPED TO 510 SIX RIVERS)
  !  7843 = TRINIDAD RANCHERIA               (MAPPED TO 510 SIX RIVERS)
  !  7845 = KARUK OFF-RES. TRUST LAND        (MAPPED TO 505 KLAMATH)
  !  8103 = COOS, LOWER UMPQUA, SIUSLAW RES. (MAPPED TO 611 SISKIYOU)
  !  8105 = COQUILLE RESERVATION             (MAPPED TO 611 SISKIYOU)

  ! Variables
  integer :: JFOR(11),KFOR(11),NUMFOR,I
  logical :: USEIGL,FORFOUND
  data JFOR/505,510,514,611,705,800,712,518,507,508,&
       &715/
  data NUMFOR /11/
  data KFOR/11*1/

  USEIGL = .true.
  FORFOUND = .false.

  select case (KODFOR)

     !       CROSSWALK FOR RESERVATION PSUEDO CODES & LOCATION CODE
  case (7806)
     write(JOSTND,60)
     60 format(/,'********',T12,'DRY CREEK RANCHERIA (7806) BEING ',&
          &'MAPPED TO MENDOCINO NF (508) FOR FURTHER PROCESSING.')
     IFOR = 10
  case (7807)
     write(JOSTND,61)
     61 format(/,'********',T12,'ROBINSON RANCHERIA (7807) BEING ',&
          &'MAPPED TO MENDOCINO NF (508) FOR FURTHER PROCESSING.')
     IFOR = 10
  case (7810)
     write(JOSTND,62)
     62 format(/,'********',T12,'HOPLAND RANCHERIA (7810) BEING ',&
          &'MAPPED TO MENDOCINO NF (508) FOR FURTHER PROCESSING.')
     IFOR = 10
  case (7813)
     write(JOSTND,63)
     63 format(/,'********',T12,'LAYTONVILLE RANCHERIA (7813) BEING ',&
          &'MAPPED TO MENDOCINO NF (508) FOR FURTHER PROCESSING.')
     IFOR = 10
  case (7815)
     write(JOSTND,64)
     64 format(/,'********',T12,'MANCHESTER-PT. ARENA RANCHERIA ',&
          &'(7815) BEING MAPPED TO MENDOCINO NF (508) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 10
  case (7816)
     write(JOSTND,65)
     65 format(/,'********',T12,'MIDDLETOWN RANCHERIA (7816) BEING ',&
          &'MAPPED TO MENDOCINO NF (508) FOR FURTHER PROCESSING.')
     IFOR = 10
  case (7820)
     write(JOSTND,66)
     66 format(/,'********',T12,'REDWOOD VALLEY RANCHERIA (7820) ',&
          &'BEING MAPPED TO MENDOCINO NF (508) FOR FURTHER PROCESSING.')
     IFOR = 10
  case (7821)
     write(JOSTND,67)
     67 format(/,'********',T12,'ROUND VALLEY OFF-RES. TRUST LAND ',&
          &'(7821) BEING MAPPED TO MENDOCINO NF (508) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 10
  case (7824)
     write(JOSTND,68)
     68 format(/,'********',T12,'STEWARTS POINT RANCHERIA (7824) ',&
          &'BEING MAPPED TO MENDOCINO NF (508) FOR FURTHER PROCESSING.')
     IFOR = 10
  case (7830)
     write(JOSTND,69)
     69 format(/,'********',T12,'SHERWOOD VALLEY RANCHERIA (7830) ',&
          &'BEING MAPPED TO MENDOCINO NF (508) FOR FURTHER PROCESSING.')
     IFOR = 10
  case (7831)
     write(JOSTND,70)
     70 format(/,'********',T12,'SULPHUR BANK RANCHERIA (7831) ',&
          &'BEING MAPPED TO MENDOCINO NF (508) FOR FURTHER PROCESSING.')
     IFOR = 10
  case (7833)
     write(JOSTND,71)
     71 format(/,'********',T12,'UPPER LAKE RANCHERIA (7833) BEING ',&
          &'MAPPED TO MENDOCINO NF (508) FOR FURTHER PROCESSING.')
     IFOR = 10
  case (7834)
     write(JOSTND,72)
     72 format(/,'********',T12,'COYOTE VALLEY RESERVATION (7834) ',&
          &'BEING MAPPED TO MENDOCINO NF (508) FOR FURTHER PROCESSING.')
     IFOR = 10
  case (7839)
     write(JOSTND,73)
     73 format(/,'********',T12,'ELK VALLEY OFF-RES. TRUST LAND ',&
          &'(7839) BEING MAPPED TO SIX RIVERS NF (510) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 2
  case (7841)
     write(JOSTND,75)
     75 format(/,'********',T12,'YUROK RESERVATION (7841) BEING ',&
          &'MAPPED TO SIX RIVERS NF (510) FOR FURTHER PROCESSING.')
     IFOR = 2
  case (7843)
     write(JOSTND,76)
     76 format(/,'********',T12,'TRINIDAD RANCHERIA (7843) BEING ',&
          &'MAPPED TO SIX RIVERS NF (510) FOR FURTHER PROCESSING.')
     IFOR = 2
  case (7845)
     write(JOSTND,77)
     77 format(/,'********',T12,'KARUK OFF-RES. TRUST LAND (7845) ',&
          &'BEING MAPPED TO KLAMATH NF (505) FOR FURTHER PROCESSING.')
     IFOR = 1
  case (8103)
     write(JOSTND,78)
     78 format(/,'********',T12,'COOS, LOWER UMPQUA, SIUSLAW RES. ',&
          &'(8103) BEING MAPPED TO SISKIYOU NF (611) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 4
  case (8105)
     write(JOSTND,79)
     79 format(/,'********',T12,'COQUILLE RESERVATION (8105) BEING ',&
          &'MAPPED TO SISKIYOU NF (611) FOR FURTHER PROCESSING.')
     IFOR = 4
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
        write(JOSTND,11) JFOR(IFOR)
        11 format(/,'********',T12,'FOREST CODE USED IN THIS ',&
             &'PROJECTION IS',I4)
        USEIGL = .false.
     end if

  end select

  !     FOREST MAPPING CORRECTION
  select case (IFOR)
  case (8)
     write(JOSTND,21)
     21 format(/,'********',T12,'TRINITY NF (518) BEING MAPPED TO ',&
          &'SHASTA-TRINITY (514) FOR FURTHER PROCESSING.')
     IFOR = 3
  case (9)
     write(JOSTND,22)
     22 format(/,'********',T12,'LOS PADRES NF (507) BEING MAPPED TO ',&
          &'SIX RIVERS (510) FOR FURTHER PROCESSING.')
     IFOR = 2
  case (10)
     write(JOSTND,23)
     23 format(/,'********',T12,'MENDOCINO NF (508) BEING MAPPED TO ',&
          &'SIX RIVERS (510) FOR FURTHER PROCESSING.')
     IFOR = 2
  case (11)
     write(JOSTND,24)
     24 format(/,'********',T12,'SIMPSON TIMBER (715) IS BEING ',&
          &'MAPPED TO (800) FOR FURTHER PROCESSING.')
     IFOR = 6
  end select

  !     SET THE IGL VARIABLE ONLY IF DEFAULT FOREST IS USED
  !     GEOGRAPHIC LOCATION CODE: 1=NORTH, 2=CENTRAL, 3=SOUTH
  !     USED TO SET SOME EQUATIONS IN REGENERATION AND PERHAPS
  !     HEIGHT-DIAMETER IN DIFFERENT VARIANTS.
  if (USEIGL) IGL = KFOR(IFOR)

  KODFOR = JFOR(IFOR)
  return
end
