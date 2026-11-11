!============================!
!   Translate forest codes   !
!============================!
! CR $Id$
subroutine FORKOD
  implicit none

  !     TRANSLATES FOREST CODE INTO A SUBSCRIPT, IFOR, AND IF
  !     KODFOR IS ZERO, THE ROUTINE RETURNS THE DEFAULT CODE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'

  !  NATIONAL FORESTS:
  !  201 = ARAPAHO (MAPPED TO ARAPAHO-ROOSEVELT)
  !  202 = BIGHORN
  !  203 = BLACK HILLS
  !  204 = GRAND MESA, UNCOMPAHGRE, GUNNISON
  !  205 = GUNNISON (MAPPED TO GMUG)
  !  206 = MEDICINE BOW-ROUTT
  !  207 = NEBRASKA
  !  208 = PIKE (MAPPED TO PIKE-SAN ISABEL)
  !  209 = RIO GRANDE
  !  210 = ARAPAHO-ROOSEVELT
  !  211 = ROUTT (MAPPED TO MEDICINE BOW-ROUTT)
  !  212 = PIKE-SAN ISABEL
  !  213 = SAN JUAN
  !  214 = SHOSHONE
  !  215 = WHITE RIVER
  !  216 = SAMUEL R. MCKELVIE (MAPPED TO NEBRASKA)
  !  224 = GRAND MESA (MAPPED TO GMUG)
  !  301 = APACHE-SITGREAVES
  !  302 = CARSON
  !  303 = CIBOLA
  !  304 = COCONINO
  !  305 = CORONADO
  !  306 = GILA
  !  307 = KAIBAB
  !  308 = LINCOLN
  !  309 = PRESCOTT
  !  310 = SANTE FE
  !  311 = SITGREAVES (MAPPED TO APACHE-SITGREAVES)
  !  312 = TONTO

  !  RESERVATION PSUEDO CODES:
  !  7101 = CHEYENNE RIVER RES.              (MAPPED TO 203 BLACK HILLS)
  !  7104 = PINE RIDGE RES.                  (MAPPED TO 203 BLACK HILLS)
  !  7105 = ROSEBUD INDIAN RES.              (MAPPED TO 203 BLACK HILLS)
  !  7106 = YANKTON RES.                     (MAPPED TO 203 BLACK HILLS)
  !  7108 = STANDING ROCK RES.               (MAPPED TO 203 BLACK HILLS)
  !  7111 = SANTEE RES.                      (MAPPED TO 207 NEBRASKA)
  !  7113 = CROW CREEK RES.                  (MAPPED TO 203 BLACK HILLS)
  !  7114 = LOWER BRULE RES.                 (MAPPED TO 203 BLACK HILLS)
  !  7206 = CHEYENNE-ARAPAHO OTSA            (MAPPED TO 310 SANTA FE)
  !  7207 = KIOWA-COMANCHE-APACHE-           (MAPPED TO 310 SANTA FE)
  !         FORT SILL APACHE OTSA
  !  7208 = KIOWA-COMANCHE-APACHE-FT SILL-   (MAPPED TO 310 SANTA FE)
  !         APACHE/CADDO-WICHITA-DELAWARE
  !          JOINT-USE OTSA
  !  7209 = CADDO-WICHITA-DELAWARE OTSA      (MAPPED TO 310 SANTA FE)
  !  7210 = KAW OTSA                         (MAPPED TO 310 SANTA FE)
  !  7211 = OTOE-MISSOURIA OTSA              (MAPPED TO 310 SANTA FE)
  !  7213 = PONCA OTSA                       (MAPPED TO 310 SANTA FE)
  !  7214 = TONKAWA OTSA                     (MAPPED TO 310 SANTA FE)
  !  7217 = KICKAPOO (TX) RES.               (MAPPED TO 308 LINCOLN)
  !  7302 = CROW RES.                        (MAPPED TO 202 BIGHORN)
  !  7305 = NORTHERN CHEYENNE OFF-RES.       (MAPPED TO 203 BLACK HILLS)
  !         TRUST LAND
  !  7306 = WIND RIVER RES.                  (MAPPED TO 214 SHOSHONE)
  !  7601 = CHICKASAW OTSA                   (MAPPED TO 310 SANTA FE)
  !  7609 = OSAGE RES.                       (MAPPED TO 310 SANTA FE)
  !  7701 = COLORADO RIVER INDIAN RES.       (MAPPED TO 312 TONTO)
  !  7702 = FORT MOJAVE RES.                 (MAPPED TO 312 TONTO)
  !  7703 = CHEMEHUEVI RES.                  (MAPPED TO 312 TONTO)
  !  7704 = FORT APACHE RES.                 (MAPPED TO 311 SITGREAVES)
  !  7705 = TOHONO O'ODHAM NATION RES.       (MAPPED TO 312 TONTO)
  !  7706 = FORT MCDOWELL YAVAPAI NATION RES.(MAPPED TO 312 TONTO)
  !  7707 = SALT RIVER RES.                  (MAPPED TO 312 TONTO)
  !  7708 = MARICOPA (AK CHIN) INDIAN RES.   (MAPPED TO 312 TONTO)
  !  7709 = GILA RIVER INDIAN RES.           (MAPPED TO 312 TONTO)
  !  7710 = SAN CARLOS RES.                  (MAPPED TO 305 CORONADO)
  !  7718 = UINTAH AND OURAY RES.            (MAPPED TO 215 WHITE RIVER)
  !  7719 = COCOPAH RES.                     (MAPPED TO 312 TONTO)
  !  7720 = FORT YUMA INDIAN RES.            (MAPPED TO 312 TONTO)
  !  7724 = HOPI RES.                        (MAPPED TO 311 SITGREAVES)
  !  7725 = HAVASUPAI RES.                   (MAPPED TO 307 KAIBAB)
  !  7726 = HUALAPAI INDIAN RES.             (MAPPED TO 307 KAIBAB)
  !  7727 = YAVAPAI-PRESCOTT RES.            (MAPPED TO 309 PRESCOTT)
  !  7728 = KAIBAB INDIAN RES.               (MAPPED TO 307 KAIBAB)
  !  7835 = TIMBI-SHA SHOSHONE RES.          (MAPPED TO 312 TONTO)
  !  7847 = AGUA CALIENTE INDIAN RES.        (MAPPED TO 312 TONTO)
  !  7848 = AUGUSTINE RES.                   (MAPPED TO 312 TONTO)
  !  7859 = MORONGO RES.                     (MAPPED TO 312 TONTO)
  !  7901 = ACOMA PUEBLO                     (MAPPED TO 303 CIBOLA)
  !  7902 = PUEBLO DE COCHITI                (MAPPED TO 310 SANTA FE)
  !  7903 = ISLETA PUEBLO                    (MAPPED TO 303 CIBOLA)
  !  7904 = JEMEZ PUEBLO                     (MAPPED TO 310 SANTA FE)
  !  7905 = SANDIA PUEBLO                    (MAPPED TO 303 CIBOLA)
  !  7906 = SAN FELIPE PUEBLO                (MAPPED TO 310 SANTA FE)
  !  7907 = SANTA ANA PUEBLO                 (MAPPED TO 310 SANTA FE)
  !  7908 = SANTO DOMINGO PUEBLO             (MAPPED TO 310 SANTA FE)
  !  7909 = ZIA PUEBLO                       (MAPPED TO 310 SANTA FE)
  !  7910 = LAGUNA PUEBLO                    (MAPPED TO 303 CIBOLA)
  !  7911 = NAMBE PUEBLO                     (MAPPED TO 310 SANTA FE)
  !  7912 = PICURIS PUEBLO                   (MAPPED TO 302 CARSON)
  !  7913 = PUEBLO OF POJOAQUE               (MAPPED TO 310 SANTA FE)
  !  7914 = SAN ILDEFONSO PUEBLO             (MAPPED TO 310 SANTA FE)
  !  7915 = OHKAY OWINGEH                    (MAPPED TO 302 CARSON)
  !  7916 = SANTA CLARA PUEBLO               (MAPPED TO 310 SANTA FE)
  !  7917 = TAOS PUEBLO                      (MAPPED TO 302 CARSON)
  !  7918 = TESUQUE PUEBLO                   (MAPPED TO 310 SANTA FE)
  !  7919 = SOUTHERN UTE RES.                (MAPPED TO 213 SAN JUAN)
  !  7920 = UTE MOUNTAIN RES.                (MAPPED TO 213 SAN JUAN)
  !  7921 = JICARILLA APACHE NATION RES.     (MAPPED TO 302 CARSON)
  !  7922 = MESCALERO RES.                   (MAPPED TO 308 LINCOLN)
  !  7923 = FORT SILL APACHE INDIAN RES.     (MAPPED TO 306 GILA)
  !  7924 = ZUNI RES.                        (MAPPED TO 303 CIBOLA)
  !  7925 = RAMAH-NAVAJO                     (MAPPED TO 303 CIBOLA)
  !  8001 = NAVAJO NATION RES.               (MAPPED TO 311 SITGREAVES)

  ! Variables
  integer :: JFOR(29),NUMFOR,I
  logical :: FORFOUND
  data JFOR/ 202, 203, 204, 206, 207, 209, 210, 211, 212, 213,&
       &214, 215, 301, 302, 303, 304, 305, 306, 307, 308, 309, 310,&
       &312, 201, 205, 208, 224, 311, 216/
  data NUMFOR/29/

  FORFOUND = .false.

  select case (KODFOR)
     !       CROSSWALK FOR RESERVATION PSUEDO CODES & LOCATION CODE
  case (7101)
     write(JOSTND,60)
     60 format(/,'********',T12, 'CHEYENNE RIVER RES. (7101) ',&
          &'BEING MAPPED TO BLACK HILLS NF (203) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 2

  case (7104)
     write(JOSTND,61)
     61 format(/,'********',T12, 'PINE RIDGE RES. (7104) BEING ',&
          &'MAPPED TO BLACK HILLS NF (203) FOR FURTHER PROCESSING.')
     IFOR = 2

  case (7105)
     write(JOSTND,62)
     62 format(/,'********',T12, 'ROSEBUD INDIAN RES. (7105) ',&
          &'BEING MAPPED TO BLACK HILLS NF (203) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 2

  case (7106)
     write(JOSTND,63)
     63 format(/,'********',T12, 'YANKTON RES. (7106) BEING ',&
          &'MAPPED TO BLACK HILLS NF (203) FOR FURTHER PROCESSING.')
     IFOR = 2

  case (7108)
     write(JOSTND,64)
     64 format(/,'********',T12, 'STANDING ROCK RES. (7108) ',&
          &'BEING MAPPED TO BLACK HILLS NF (203) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 2

  case (7111)
     write(JOSTND,65)
     65 format(/,'********',T12, 'SANTEE RES. (7111) BEING ',&
          &'MAPPED TO NEBRASKA NF (207) FOR FURTHER PROCESSING.')
     IFOR = 5

  case (7113)
     write(JOSTND,66)
     66 format(/,'********',T12, 'CROW CREEK RES. (7113) BEING ',&
          &' MAPPED TO BLACK HILLS NF (203) FOR FURTHER PROCESSING.')
     IFOR = 2

  case (7114)
     write(JOSTND,67)
     67 format(/,'********',T12, 'LOWER BRULE RES. (7114) BEING',&
          &' MAPPED TO BLACK HILLS NF (203) FOR FURTHER PROCESSING.')
     IFOR = 2

  case (7206)
     write(JOSTND,68)
     68 format(/,'********',T12, 'CHEYENNE-ARAPAHO OTSA (7206) ',&
          &'BEING MAPPED TO SANTA FE NF (310) FOR FURTHER PROCESSING.')
     IFOR = 22

  case (7207)
     write(JOSTND,69)
     69 format(/,'********',T12, 'KIOWA-COMANCHE-APACHE-FORT ',&
          &'SILL APACHE OTSA (7207) BEING MAPPED TO SANTA FE NF ',&
          &'(310) FOR FURTHER PROCESSING.')
     IFOR = 22

  case (7208)
     write(JOSTND,70)
     70 format(/,'********',T12, 'KIOWA-COMANCHE-APACHE-FT SILL-',&
          &'APACHE CADDO-WICHITA-DELAWARE JOINT-USE OTSA (7208) BEING ',&
          &'MAPPED TO SANTA FE NF (310) FOR FURTHER PROCESSING.')
     IFOR = 22

  case (7209)
     write(JOSTND,71)
     71 format(/,'********',T12, 'CADDO-WICHITA-DELAWARE OTSA ',&
          &'(7209) BEING MAPPED TO SANTA FE NF (310) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 22

  case (7210)
     write(JOSTND,72)
     72 format(/,'********',T12, 'KAW OTSA (7210) BEING MAPPED ',&
          &'TO SANTA FE NF (310) FOR FURTHER PROCESSING.')
     IFOR = 22

  case (7211)
     write(JOSTND,73)
     73 format(/,'********',T12, 'OTOE-MISSOURIA OTSA (7211) ',&
          &'BEING MAPPED TO SANTA FE NF (310) FOR FURTHER PROCESSING.')
     IFOR = 22

  case (7213)
     write(JOSTND,74)
     74 format(/,'********',T12, 'PONCA OTSA (7213) BEING ',&
          &'MAPPED TO SANTA FE NF (310) FOR FURTHER PROCESSING.')
     IFOR = 22

  case (7214)
     write(JOSTND,75)
     75 format(/,'********',T12, 'TONKAWA OTSA (7214) BEING ',&
          &'MAPPED TO SANTA FE NF (310) FOR FURTHER PROCESSING.')
     IFOR = 22

  case (7217)
     write(JOSTND,76)
     76 format(/,'********',T12, 'KICKAPOO (TX) RES. (7217) ',&
          &'BEING MAPPED TO LINCOLN NF (308) FOR FURTHER PROCESSING.')
     IFOR = 20

  case (7302)
     write(JOSTND,77)
     77 format(/,'********',T12, 'CROW RES. (7302) BEING MAPPED',&
          &' TO BIGHORN NF (202) FOR FURTHER PROCESSING.')
     IFOR = 1

  case (7305)
     write(JOSTND,78)
     78 format(/,'********',T12, 'NORTHERN CHEYENNE OFF-RES. ',&
          &'TRUST LAND (7305) BEING MAPPED TO BLACK HILLS NF (203) ',&
          &'FOR FURTHER PROCESSING.')
     IFOR = 2

  case (7306)
     write(JOSTND,79)
     79 format(/,'********',T12, 'WIND RIVER RES. (7306) BEING ',&
          &'MAPPED TO SHOSHONE NF (214) FOR FURTHER PROCESSING.')
     IFOR = 11

  case (7601)
     write(JOSTND,80)
     80 format(/,'********',T12, 'CHICKASAW OTSA (7601) BEING ',&
          &'MAPPED TO SANTA FE NF (310) FOR FURTHER PROCESSING.')
     IFOR = 22

  case (7609)
     write(JOSTND,81)
     81 format(/,'********',T12, 'OSAGE RES. (7609) BEING MAPPED',&
          &' TO SANTA FE NF (310) FOR FURTHER PROCESSING.')
     IFOR = 22

  case (7701)
     write(JOSTND,82)
     82 format(/,'********',T12, 'COLORADO RIVER INDIAN RES. ',&
          &'(7701) BEING MAPPED TO TONTO NF (312) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 23

  case (7702)
     write(JOSTND,83)
     83 format(/,'********',T12, 'FORT MOJAVE RES. (7702) BEING ',&
          &'MAPPED TO TONTO NF (312) FOR FURTHER PROCESSING.')
     IFOR = 23

  case (7703)
     write(JOSTND,84)
     84 format(/,'********',T12, 'CHEMEHUEVI RES. (7703) BEING ',&
          &'MAPPED TO TONTO NF (312) FOR FURTHER PROCESSING.')
     IFOR = 23

  case (7704)
     write(JOSTND,85)
     85 format(/,'********',T12, 'FORT APACHE RES. (7704) BEING ',&
          &'MAPPED TO SITGREAVES NF (311) FOR FURTHER PROCESSING.')
     IFOR = 28

  case (7705)
     write(JOSTND,86)
     86 format(/,'********',T12, 'TOHONO O''ODHAM NATION RES. ',&
          &'(7705) BEING MAPPED TO TONTO NF (312) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 23

  case (7706)
     write(JOSTND,87)
     87 format(/,'********',T12, 'FORT MCDOWELL YAVAPAI NATION ',&
          &'RES. (7706) BEING MAPPED TO TONTO NF (312) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 23

  case (7707)
     write(JOSTND,88)
     88 format(/,'********',T12, 'SALT RIVER RES. (7707) BEING ',&
          &'MAPPED TO TONTO NF (312) FOR FURTHER PROCESSING.')
     IFOR = 23

  case (7708)
     write(JOSTND,89)
     89 format(/,'********',T12, 'MARICOPA (AK CHIN) INDIAN ',&
          &'RES. (7708) BEING MAPPED TO TONTO NF (312) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 23

  case (7709)
     write(JOSTND,90)
     90 format(/,'********',T12, 'GILA RIVER INDIAN RES. (7709) ',&
          &'BEING MAPPED TO TONTO NF (312) FOR FURTHER PROCESSING.')
     IFOR = 23

  case (7710)
     write(JOSTND,91)
     91 format(/,'********',T12, 'SAN CARLOS RES. (7710) BEING ',&
          &'MAPPED TO CORONADO NF (305) FOR FURTHER PROCESSING.')
     IFOR = 17

  case (7718)
     write(JOSTND,92)
     92 format(/,'********',T12, 'UINTAH AND OURAY RES. (7718) ',&
          &'BEING MAPPED TO WHITE RIVER NF (215) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 12

  case (7719)
     write(JOSTND,93)
     93 format(/,'********',T12, 'COCOPAH RES. (7719) BEING ',&
          &'MAPPED TO TONTO NF (312) FOR FURTHER PROCESSING.')
     IFOR = 23

  case (7720)
     write(JOSTND,94)
     94 format(/,'********',T12, 'FORT YUMA INDIAN RES. (7720) ',&
          &'BEING MAPPED TO TONTO NF (312) FOR FURTHER PROCESSING.')
     IFOR = 23

  case (7724)
     write(JOSTND,95)
     95 format(/,'********',T12, 'HOPI RES. (7724) BEING MAPPED ',&
          &'TO SITGREAVES NF (311) FOR FURTHER PROCESSING.')
     IFOR = 28

  case (7725)
     write(JOSTND,96)
     96 format(/,'********',T12, 'HAVASUPAI RES. (7725) BEING ',&
          &'MAPPED TO KAIBAB NF (307) FOR FURTHER PROCESSING.')
     IFOR = 19

  case (7726)
     write(JOSTND,97)
     97 format(/,'********',T12, 'HUALAPAI INDIAN RES. (7726) ',&
          &'BEING MAPPED TO KAIBAB NF (307) FOR FURTHER PROCESSING.')
     IFOR = 19

  case (7727)
     write(JOSTND,98)
     98 format(/,'********',T12, 'YAVAPAI-PRESCOTT RES. (7727) ',&
          &'BEING MAPPED TO PRESCOTT NF (309) FOR FURTHER PROCESSING.')
     IFOR = 21

  case (7728)
     write(JOSTND,99)
     99 format(/,'********',T12, 'KAIBAB INDIAN RES. (7728) ',&
          &'BEING MAPPED TO KAIBAB NF (307) FOR FURTHER PROCESSING.')
     IFOR = 19

  case (7835)
     write(JOSTND,100)
     100 format(/,'********',T12, 'TIMBI-SHA SHOSHONE RES. (7835)',&
          &' BEING MAPPED TO TONTO NF (312) FOR FURTHER PROCESSING.')
     IFOR = 23

  case (7847)
     write(JOSTND,101)
     101 format(/,'********',T12, 'AGUA CALIENTE INDIAN RES. ',&
          &'(7847) BEING MAPPED TO TONTO NF (312) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 23

  case (7848)
     write(JOSTND,102)
     102 format(/,'********',T12, 'AUGUSTINE RES. (7848) BEING ',&
          &'MAPPED TO TONTO NF (312) FOR FURTHER PROCESSING.')
     IFOR = 23

  case (7859)
     write(JOSTND,103)
     103 format(/,'********',T12, 'MORONGO RES. (7859) BEING ',&
          &'MAPPED TO TONTO NF (312) FOR FURTHER PROCESSING.')
     IFOR = 23

  case (7901)
     write(JOSTND,104)
     104 format(/,'********',T12, 'ACOMA PUEBLO (7901) BEING ',&
          &'MAPPED TO CIBOLA NF (303) FOR FURTHER PROCESSING.')
     IFOR = 15

  case (7902)
     write(JOSTND,105)
     105 format(/,'********',T12, 'PUEBLO DE COCHITI (7902) BEING',&
          &' MAPPED TO SANTA FE NF (310) FOR FURTHER PROCESSING.')
     IFOR = 22

  case (7903)
     write(JOSTND,106)
     106 format(/,'********',T12, 'ISLETA PUEBLO (7903) BEING ',&
          &'MAPPED TO CIBOLA NF (303) FOR FURTHER PROCESSING.')
     IFOR = 15

  case (7904)
     write(JOSTND,107)
     107 format(/,'********',T12, 'JEMEZ PUEBLO (7904) BEING ',&
          &'MAPPED TO SANTA FE NF (310) FOR FURTHER PROCESSING.')
     IFOR = 22

  case (7905)
     write(JOSTND,108)
     108 format(/,'********',T12, 'SANDIA PUEBLO (7905) BEING ',&
          &'MAPPED TO CIBOLA NF (303) FOR FURTHER PROCESSING.')
     IFOR = 15

  case (7906)
     write(JOSTND,109)
     109 format(/,'********',T12, 'SAN FELIPE PUEBLO (7906) ',&
          &'BEING MAPPED TO SANTA FE NF (310) FOR FURTHER PROCESSING.')
     IFOR = 22

  case (7907)
     write(JOSTND,110)
     110 format(/,'********',T12, 'SANTA ANA PUEBLO (7907) BEING ',&
          &'MAPPED TO SANTA FE NF (310) FOR FURTHER PROCESSING.')
     IFOR = 22

  case (7908)
     write(JOSTND,111)
     111 format(/,'********',T12, 'SANTO DOMINGO PUEBLO (7908) ',&
          &'BEING MAPPED TO SANTA FE NF (310) FOR FURTHER PROCESSING.')
     IFOR = 22

  case (7909)
     write(JOSTND,112)
     112 format(/,'********',T12, 'ZIA PUEBLO (7909) BEING MAPPED',&
          &' TO SANTA FE NF (310) FOR FURTHER PROCESSING.')
     IFOR = 22

  case (7910)
     write(JOSTND,113)
     113 format(/,'********',T12, 'LAGUNA PUEBLO (7910) BEING ',&
          &'MAPPED TO CIBOLA NF (303) FOR FURTHER PROCESSING.')
     IFOR = 15

  case (7911)
     write(JOSTND,114)
     114 format(/,'********',T12, 'NAMBE PUEBLO (7911) BEING ',&
          &'MAPPED TO SANTA FE NF (310) FOR FURTHER PROCESSING.')
     IFOR = 22

  case (7912)
     write(JOSTND,115)
     115 format(/,'********',T12, 'PICURIS PUEBLO (7912) BEING ',&
          &'MAPPED TO CARSON NF (302) FOR FURTHER PROCESSING.')
     IFOR = 14

  case (7913)
     write(JOSTND,116)
     116 format(/,'********',T12, 'PUEBLO OF POJOAQUE (7913) ',&
          &'BEING MAPPED TO SANTA FE NF (310) FOR FURTHER PROCESSING.')
     IFOR = 22

  case (7914)
     write(JOSTND,117)
     117 format(/,'********',T12, 'SAN ILDEFONSO PUEBLO (7914) ',&
          &'BEING MAPPED TO SANTA FE NF (310) FOR FURTHER PROCESSING.')
     IFOR = 22

  case (7915)
     write(JOSTND,118)
     118 format(/,'********',T12, 'OHKAY OWINGEH (7915) BEING ',&
          &'MAPPED TO CARSON NF (302) FOR FURTHER PROCESSING.')
     IFOR = 14

  case (7916)
     write(JOSTND,119)
     119 format(/,'********',T12, 'SANTA CLARA PUEBLO (7916) ',&
          &'BEING MAPPED TO SANTA FE NF (310) FOR FURTHER PROCESSING.')
     IFOR = 22

  case (7917)
     write(JOSTND,120)
     120 format(/,'********',T12, 'TAOS PUEBLO (7917) BEING ',&
          &'MAPPED TO CARSON NF (302) FOR FURTHER PROCESSING.')
     IFOR = 14

  case (7918)
     write(JOSTND,121)
     121 format(/,'********',T12, 'TESUQUE PUEBLO (7918) BEING ',&
          &'MAPPED TO SANTA FE NF (310) FOR FURTHER PROCESSING.')
     IFOR = 22

  case (7919)
     write(JOSTND,122)
     122 format(/,'********',T12, 'SOUTHERN UTE RES. (7919) BEING ',&
          &'MAPPED TO SAN JUAN NF (213) FOR FURTHER PROCESSING.')
     IFOR = 10

  case (7920)
     write(JOSTND,123)
     123 format(/,'********',T12, 'UTE MOUNTAIN RES. (7920) BEING ',&
          &'MAPPED TO SAN JUAN NF (213) FOR FURTHER PROCESSING.')
     IFOR = 10

  case (7921)
     write(JOSTND,124)
     124 format(/,'********',T12, 'JICARILLA APACHE NATION RES. ',&
          &'(7921) BEING MAPPED TO CARSON NF (302) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 14

  case (7922)
     write(JOSTND,125)
     125 format(/,'********',T12, 'MESCALERO RES. (7922) BEING ',&
          &'MAPPED TO LINCOLN NF (308) FOR FURTHER PROCESSING.')
     IFOR = 20

  case (7923)
     write(JOSTND,126)
     126 format(/,'********',T12, 'FORT SILL APACHE INDIAN RES. ',&
          &'(7923) BEING MAPPED TO GILA NF (306) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 18

  case (7924)
     write(JOSTND,127)
     127 format(/,'********',T12, 'ZUNI RES. (7924) BEING MAPPED ',&
          &'TO CIBOLA NF (303) FOR FURTHER PROCESSING.')
     IFOR = 15

  case (7925)
     write(JOSTND,128)
     128 format(/,'********',T12, 'RAMAH-NAVAJO (7925) BEING ',&
          &'MAPPED TO CIBOLA NF (303) FOR FURTHER PROCESSING.')
     IFOR = 15

  case (8001)
     write(JOSTND,129)
     129 format(/,'********',T12, 'NAVAJO NATION RES. (8001) ',&
          &'BEING MAPPED TO SITGREAVES NF (311) FOR FURTHER PROCESSING.')
     IFOR = 28
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

        !           ERROR TRAPPING FOR MISSING LOCATIONS BASED UPON
        !           CENTRAL ROCKIES VARIANT - 5 SUBMODELS IMODTY
        !             SOUTHWEST MIXED CONIFER    : 1
        !             SOUTHWEST PONDEROSA PINE   : 2
        !             BLACK HILLS PONDEROSA PINE : 3
        !             SPRUCE-FIR                 : 4
        !             LODGEPOLE PINE             : 5

        call ERRGRO (.true.,3)
        select case (IMODTY)
        case ( :2)
           if (IFOR.eq.0) then
              !                 CIBOLA
              IFOR = 15
           end if
           if (KODFOR.gt.0.and. KODFOR.lt.300) then
              !                 SAN JUAN
              IFOR = 10
           end if
        case (3)
           if (IFOR.eq.0) then
              !                 BIGHORN
              IFOR = 2
           end if
        case default
           if (IFOR.eq.0) then
              !                 GMUG
              IFOR = 4
           end if
        end select

        if (KODFOR.gt.0) then
           write(JOSTND,149) JFOR(IFOR)
           149 format(/,'********',T12,'FOREST CODE USED IN THIS ',&
                &'PROJECTION IS ',I4)
        end if

     end if

  end select

  !     FOREST MAPPING CORRECTION
  select case (IFOR)
  case (24)
     write(JOSTND,201)
     201 format(/,'********',T12,'ARAPAHO NF (201) BEING MAPPED TO ',&
          &'ARAPAHO-ROOSEVELT (210) FOR FURTHER PROCESSING.')
     IFOR = 7
  case (25)
     write(JOSTND,202)
     202 format(/,'********',T12,'GUNNISON NF (205) BEING MAPPED TO ',&
          &'GRAND MESA-UNCOMPAHGRE-GUNNISON (204) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 3
  case (26)
     write(JOSTND,203)
     203 format(/,'********',T12,'PIKE NF (208) BEING MAPPED TO ',&
          &'PIKE-SAN ISABEL (212) FOR FURTHER PROCESSING.')
     IFOR = 9
  case (27)
     write(JOSTND,204)
     204 format(/,'********',T12,'GRAND MESA NF (224) BEING MAPPED ',&
          &'TO GRAND MESA-UNCOMPAHGRE-GUNNISON (204) FOR FURTHER ',&
          &'PROCESSING.')
     IFOR = 3
  case (28)
     write(JOSTND,205)
     205 format(/,'********',T12,'SITGREAVES NF (311) BEING MAPPED ',&
          &'TO APACHE-SITGREAVES (301) FOR FURTHER PROCESSING.')
     IFOR = 13
  case (8)
     write(JOSTND,206)
     206 format(/,'********',T12,'ROUTT NF (211) BEING MAPPED TO ',&
          &'MEDICINE BOW-ROUTT (206) FOR FURTHER PROCESSING.')
     IFOR = 4
  case (29)
     write(JOSTND,207)
     207 format(/,'********',T12,'SAMUEL R MCKELVIE NF (216) BEING ',&
          &'MAPPED TO NEBRASKA (207) FOR FURTHER PROCESSING.')
     IFOR = 5
  end select

  KODFOR = JFOR(IFOR)
  return
end
