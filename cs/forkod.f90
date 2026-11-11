!============================!
!   Translate forest codes   !
!============================!
! CS $Id$
subroutine FORKOD
  implicit none

  !     TRANSLATES FOREST CODE INTO A SUBSCRIPT, IFOR, AND IF
  !     KODFOR IS ZERO, THE ROUTINE RETURNS THE DEFAULT CODE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'

  !  NATIONAL FORESTS:
  !  905 = MARK TWAIN
  !  908 = SHAWNEE
  !  912 = HOOSIER
  !  911 = OLD WAYNE-HOOSIER COMBINED CODE (MAP TO HOOSIER)

  !  RESERVATION PSUEDO CODES:

  !  7110 = OMAHA RESERVATION                 (MAPPED TO  905 MARK TWAIN)
  !  7111 = SANTEE RESERVATION                (MAPPED TO  905 MARK TWAIN)
  !  7112 = WINNEBAGO RESERVATION             (MAPPED TO  905 MARK TWAIN)
  !  7202 = IOWA (KS-NE) RESERVATION          (MAPPED TO  905 MARK TWAIN)
  !  7203 = KICKAPOO (KS) RESERVATION         (MAPPED TO  905 MARK TWAIN)
  !  7204 = PRAIRIE BAND OF POTAWATOMI        (MAPPED TO  905 MARK TWAIN)
  !  7205 = SAC AND FOX NATION RESERVATION    (MAPPED TO  905 MARK TWAIN)
  !  7210 = KAW OTSA                          (MAPPED TO  905 MARK TWAIN)
  !  7509 = SAC AND FOX/MESKWAKI SETTLEMENT   (MAPPED TO  908 SHAWNEE)
  !  7602 = QUAPAW OTSA                       (MAPPED TO  905 MARK TWAIN)
  !  7606 = MIAMI OTSA                        (MAPPED TO  905 MARK TWAIN)
  !  7608 = MODOC OTSA                        (MAPPED TO  905 MARK TWAIN)
  !  7609 = OSAGE RESERVATION                 (MAPPED TO  905 MARK TWAIN)
  !  7611 = CHEROKEE OTSA                     (MAPPED TO  905 MARK TWAIN)

  ! Variables
  integer :: JFOR(4),KFOR(4),NUMFOR,I
  logical :: USEIGL,FORFOUND
  data JFOR/905,908,912,911/
  data NUMFOR/4/
  data KFOR/1,1,1,1/

  USEIGL = .true.
  FORFOUND = .false.

  select case (KODFOR)

     !       CROSSWALK FOR RESERVATION PSUEDO CODES & LOCATION CODE
  case (7110)
     write(JOSTND,60)
     60 format(/,'********',T12,'OMAHA RESERVATION (7110) BEING ',&
          &'MAPPED TO MARK TWAIN NF (905) FOR FURTHER PROCESSING.')
     IFOR = 1
  case (7111)
     write(JOSTND,61)
     61 format(/,'********',T12,'SANTEE RESERVATION (7111) BEING ',&
          &'MAPPED TO MARK TWAIN NF (905) FOR FURTHER PROCESSING.')
     IFOR = 1
  case (7112)
     write(JOSTND,62)
     62 format(/,'********',T12,'WINNEBAGO RESERVATION (7112) ',&
          &'BEING MAPPED TO MARK TWAIN NF (905) FOR FURTHER PROCESSING.')
     IFOR = 1
  case (7202)
     write(JOSTND,63)
     63 format(/,'********',T12,'IOWA (KS-NE) RESERVATION (7202) ',&
          &'BEING MAPPED TO MARK TWAIN NF (905) FOR FURTHER PROCESSING.')
     IFOR = 1
  case (7203)
     write(JOSTND,64)
     64 format(/,'********',T12,'KICKAPOO (KS) RESERVATION (7203)',&
          &' BEING MAPPED TO MARK TWAIN NF (905) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 1
  case (7204)
     write(JOSTND,65)
     65 format(/,'********',T12,'PRAIRIE BAND OF POTAWATOMI ',&
          &'(7204) BEING MAPPED TO MARK TWAIN NF (905) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 1
  case (7205)
     write(JOSTND,66)
     66 format(/,'********',T12,'SAC AND FOX NATION RESERVATION ',&
          &'(7205) BEING MAPPED TO MARK TWAIN NF (905) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 1
  case (7210)
     write(JOSTND,67)
     67 format(/,'********',T12,'KAW OTSA (7210) BEING MAPPED',&
          &'TO MARK TWAIN NF (905) FOR FURTHER PROCESSING.')
     IFOR = 1
  case (7509)
     write(JOSTND,68)
     68 format(/,'********',T12,'SAC AND FOX/MESKWAKI SETTLEMENT ',&
          &'(7509) BEING MAPPED TO SHAWNEE NF (908) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 2
  case (7602)
     write(JOSTND,69)
     69 format(/,'********',T12,'QUAPAW OTSA (7602) BEING MAPPED ',&
          &'TO MARK TWAIN NF (905) FOR FURTHER PROCESSING.')
     IFOR = 1
  case (7606)
     write(JOSTND,70)
     70 format(/,'********',T12,'MIAMI OTSA (7606) BEING MAPPED ',&
          &'TO MARK TWAIN NF (905) FOR FURTHER PROCESSING.')
     IFOR = 1
  case (7608)
     write(JOSTND,71)
     71 format(/,'********',T12,'MODOC OTSA (7608) BEING MAPPED ',&
          &'TO MARK TWAIN NF (905) FOR FURTHER PROCESSING.')
     IFOR = 1
  case (7609)
     write(JOSTND,72)
     72 format(/,'********',T12,'OSAGE RESERVATION (7609) BEING ',&
          &'MAPPED TO MARK TWAIN NF (905) FOR FURTHER PROCESSING.')
     IFOR = 1
  case (7611)
     write(JOSTND,73)
     73 format(/,'********',T12,'CHEROKEE OTSA (7611) BEING',&
          &' MAPPED TO MARK TWAIN NF (905) FOR FURTHER PROCESSING.')
     IFOR = 1
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
  case (4)
     write(JOSTND,21)
     21 format(/,'********',T12,'WAYNE-HOOSIER NF (911) BEING ',&
          &'MAPPED TO HOOSIER (912) FOR FURTHER PROCESSING.')
     IFOR = 3
  end select

  !  SET DEFAULT TLAT, TLONG, AND ELEVATION VALUES, BY FOREST

  select case(IFOR)
  case(1)
     if (TLAT.eq.0) TLAT = 37.95
     if (TLONG.eq.0)TLONG = 91.77
     if (ELEV.eq.0) ELEV = 10.
  case(2)
     if (TLAT.eq.0) TLAT = 37.74
     if (TLONG.eq.0)TLONG = 88.54
     if (ELEV.eq.0) ELEV = 4.
  case(3,4)
     if (TLAT.eq.0) TLAT = 38.86
     if (TLONG.eq.0)TLONG = 86.49
     if (ELEV.eq.0) ELEV = 6.
  end select

  !     SET THE IGL VARIABLE ONLY IF DEFAULT FOREST IS USED
  !     GEOGRAPHIC LOCATION CODE: 1=NORTH, 2=CENTRAL, 3=SOUTH
  !     USED TO SET SOME EQUATIONS IN REGENERATION AND PERHAPS
  !     HEIGHT-DIAMETER IN DIFFERENT VARIANTS.
  if (USEIGL) IGL = KFOR(IFOR)

  KODFOR = JFOR(IFOR)
  return
end
