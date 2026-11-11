!============================!
!   Translate forest codes   !
!============================!
subroutine FORKOD
  implicit none

  ! SN $ID: FORKOD.F 2591 2018-12-14 18:21:52Z TCOURTER $

  !     TRANSLATES FOREST CODE INTO A SUBSCRIPT, IFOR, AND IF
  !     KODFOR IS ZERO, THE ROUTINE RETURNS THE DEFAULT CODE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'
  include 'SNCOM.f90'

  ! Variables
  integer :: JFOR(20),NUMFOR,IFORDI,I
  logical :: FORFOUND
  data JFOR/801,802,803,804,805,806,807,808,809,810,&
       &811,812,813,905,908,836,824,860,835,701/
  data NUMFOR/20/

  FORFOUND = .false.

  !  PROCLAIMED NATIONAL FOREST  DISTRICT     REGION/FOREST/DISTRICT CODE
  !  NATIONAL FORESTS IN ALABAMA BANKHEAD             80101
  !         CONECUH                                   80103
  !         OAKMULGEE                                 80104
  !         SHOAL CREEK                               80105
  !         TALLEDEGA                                 80106
  !         TUSKEGEE                                  80107
  !  DANIEL BOONE  MOREHEAD                           80211
  !         STANTON                                   80212
  !         BEREA                                     80213
  !         LONDON                                    80214
  !         SOMERSET                                  80215
  !         STEARNS                                   80216
  !         REDBIRD                                   80217
  !  CHATTAHOOCHEE-OCONEE ARMUCHEE                    80301
  !         TOCCOA                                    80302
  !         BRASSTOWN                                 80304
  !         TALLUAH                                   80305
  !         CHATTOOGA                                 80306
  !         COHUTTA                                   80307
  !         OCONEE                                    80308
  !  CHEROKEE      HIWASSEE                           80401
  !         NOLICHUCKY                                80402
  !         OCOEE                                     80403
  !         TELLICO                                   80404
  !         UNAKA                                     80405
  !         WATUGA                                    80406
  !  NATIONAL FORESTS IN FLORIDA APALACHICOLA         80501
  !         LAKE GEORGE                               80502
  !         OSCEOLA                                   80504
  !         SEMINOLE                                  80505
  !         WAKULLA                                   80506
  !  KISATCHIE     CATAHOULA                          80601
  !         EVANGELINE/VERNON                         80602
  !         KISATCHIE                                 80603
  !         WINN                                      80604
  !         CANEY                                     80605
  !  NATIONAL FORESTS IN MISS. BIENVILLE              80701
  !         DESOTO                                    80702
  !         HOMOCHITTO                                80704
  !         CHICKASAWHAY                              80705
  !         DELTA                                     80706
  !         HOLLY SPRINGS                             80707
  !         TOMBIGBEE                                 80717
  !   GEORGE WASHINGTON/JEFFERSON NFS DEERFIELD       80801
  !         DRY RIVER                                 80802
  !         JAMES RIVER                               80803
  !         LEE                                       80804
  !         PEDLAR                                    80805
  !         WARM SPRINGS                              80806
  !         BLACKSBURG                                80811
  !         CLINCH                                    80812
  !         GLENWOOD                                  80813
  !         MT. ROGERS                                80814
  !         NEW CASTLE                                80815
  !         WYTHE                                     80816
  !  QUACHITA      CHOCTAW                            80901
  !         CADDO                                     80902
  !         COLD SPRINGS                              80903
  !         FOURCHE                                   80904
  !         JESSIEVILLE                               80905
  !         KIAMICHI                                  80906
  !         MENA                                      80907
  !         ODEN                                      80908
  !         POTEAU                                    80909
  !         WOMBLE                                    80910
  !         WINONA                                    80911
  !         TIAK                                      80912
  !  OZARK & ST. FRANCIS NFS     SYLAMORE             81001
  !         BUFFALO                                   81002
  !         BAYOU                                     81003
  !         PLEASANT HILL                             81004
  !         BOSTON MOUNTAIN                           81005
  !         MAGAZINE                                  81006
  !         ST. FRANCIS                               81007
  !  NATIONAL FORESTS IN NORTH CAROLINA CHEOAH        81102
  !         CROATAN                                   81103
  !         APPALACHIAN (OLD)                         81104
  !         GRANDFATHER                               81105
  !         HIGHLANDS                                 81106
  !         PISGAH                                    81107
  !         APPALACHIAN                               81108
  !         TUSQUITEE                                 81109
  !         UWHARRIE                                  81110
  !         WAYSH                                     81111
  !  FRANCIS MARION & SUMTER NFS ENOREE/TYGER         81201
  !         ANDREW PICKENS                            81202
  !         LONG CANE                                 81203
  !         WAMBAW/WITHERBEE                          81205
  !  NATIONAL FORESTS IN TEXAS   ANGELINA             81301
  !         DAVY CROCKETT                             81303
  !         SAM HOUSTON                               81304
  !         SABINE                                    81307
  !         CADDO/LBJ                                 81308

  !  MARK TWAIN NATIONAL FOREST                       905

  !  SHAWNEE NATIONAL FOREST                          908

  !  SAVANNAH RIVER (ADMINISTRATIVE)                  836 (MAP TO SUMTER)
  !  SAVANNAH RIVER (PROCLAIMED)                      824 (MAP TO SUMTER)
  !  LAND BETWEEN THE LAKES (ADMINISTRATIVE)          860 (MAP TO DANIEL BOONE)
  !  LAND BETWEEN THE LAKES (PROCLAIMED)              835 (MAP TO DANIEL BOONE)

  !  DEPT OF DEFENSE, FORT BRAGG                      701

  !  RESERVATION PSUEDO CODES:

  !  7201 = ALABAMA-COUSHATTA RES.      81304 TEXAS NF SAM HOUSTON DIST
  !  7207 = KIOWA-COMANCHE-APACHE-      80906 OUACHITA NF KIAMICHI DIST
  !         FORT SILL APACHE OTSA
  !  7210 = KAW OTSA                    81005 OZARK NF BOSTON MOUNTAIN DIST
  !  7211 = OTOE-MISSOURIA OTSA         81005 OZARK NF BOSTON MOUNTAIN DIST
  !  7212 = PAWNEE OTSA                 81005 OZARK NF BOSTON MOUNTAIN DIST
  !  7213 = PONCA OTSA                  81005 OZARK NF BOSTON MOUNTAIN DIST
  !  7215 = CITIZEN POTAWATOMI NATION-  80901 OUACHITA NF CHOCTAW DIST
  !         ABSENTEE SHAWNEE OTSA
  !  7216 = IOWA OTSA                   80901 OUACHITA NF CHOCTAW DIST
  !  7218 = SAC AND FOX OTSA            80901 OUACHITA NF CHOCTAW DIST
  !  7601 = CHICKASAW OTSA              80906 OUACHITA NF KIAMICHI DIST
  !  7602 = QUAPAW OTSA                 81005 OZARK NF BOSTON MOUNTAIN DIST
  !  7603 = EASTERN SHAWNEE OTSA        81005 OZARK NF BOSTON MOUNTAIN DIST
  !  7604 = SENECA-CAYUGA OTSA          81005 OZARK NF BOSTON MOUNTAIN DIST
  !  7605 = WYANDOTTE OTSA              81005 OZARK NF BOSTON MOUNTAIN DIST
  !  7606 = MIAMI OTSA                  81005 OZARK NF BOSTON MOUNTAIN DIST
  !  7607 = PEORIA OTSA                 81005 OZARK NF BOSTON MOUNTAIN DIST
  !  7608 = MODOC OTSA                  81005 OZARK NF BOSTON MOUNTAIN DIST
  !  7609 = OSAGE RES.                  81005 OZARK NF BOSTON MOUNTAIN DIST
  !  7610 = CREEK OTSA                  80901 OUACHITA NF CHOCTAW DIST
  !  7611 = CHEROKEE OTSA               81005 OZARK NF BOSTON MOUNTAIN DIST
  !  7612 = CHOCTAW OTSA                80906 OUACHITA NF KIAMICHI DIST
  !  7613 = SEMINOLE OTSA               80901 OUACHITA NF CHOCTAW DIST
  !  8205 = MICCOSUKEE RES.             80505 FLORIDA NF SEMINOLE DIST
  !  8207 = POARCH CREEK RES.           80103 ALABAMA NF CONECUH DIST
  !  8210 = CATAWBA RES.                81201 SUMTER NF ENOREE DIST
  !  8212 = TUNICA-BILOXI RES.          80601 KISATCHIE NF CATAHOULA DIST
  !  8213 = COUSHATTA RES.              80602 KISATCHIE NF CALCASIEU DIST
  !  8219 = EASTERN CHEROKEE RES.       81111 NORTH CAROLINA NF NANTAHALA DIST
  !  8220 = SEMINOLE (FL) TRUST LAND    80505 FLORIDA NF SEMINOLE DIST
  !  8221 = MISSISSIPPI CHOCTAW RES.    80701 MISSISSIPPI NF BIENVILLE DIST

  !  DETERMINE IF KODFOR IS STANDARD FIVE DIGIT REGION/FOREST/DISTRICT CODE
  !  FOR REGION 8; EX. 80101. ESTABLISH IFORDI AS THREE DIGIT FOREST CODE
  !  FOR LOGICAL COMPARISON

  IFORDI = KODFOR/100

  if (IFORDI.eq.905.or. IFORDI.eq.908.or.&
       &IFORDI.eq.836.or. IFORDI.eq.824.or.&
       &IFORDI.eq.860.or. IFORDI.eq.835.or.&
       &IFORDI .eq.701) KODFOR = IFORDI

  !  INTIAL KODFOR EVALUATION FOR DISCRETE GEOGRAPHIES AND RE-MAPPING

  select case (KODFOR)

     !  HANDLE SAVANNAH RIVER

  case (836,824)
     KODFOR = 81203
     IFORDI = 812
     write(JOSTND,21)
     21 format(/,'********',T12,'SAVANNAH RIVER BEING MAPPED TO ',&
          &'SUMTER NF (81203) FOR FURTHER PROCESSING.')

     !  HANDLE LAND BETWEEN THE LAKES

  case (860,835)
     KODFOR = 80216
     IFORDI = 802
     write(JOSTND,22)
     22 format(/,'********',T12,'LAND BETWEEN THE LAKES BEING MAPPED',&
          &' TO DANIEL BOONE (80216) FOR FURTHER PROCESSING.')

     !  HANDLE RESERVATION PSUEDO CODES

  case (7201)
     write(JOSTND,62)
     62 format(/,'********',T12,'ALABAMA-COUSHATTA RES. (7201) BEING',&
          &' MAPPED TO 81304 TEXAS NF SAM HOUSTON DIST FOR FURTHER ',&
          &'PROCESSING.')
     KODFOR = 81304
     IFORDI = 813
  case (7207)
     write(JOSTND,63)
     63 format(/,'********',T12,'KIOWA-COMANCHE-APACHE-FORT SILL ',&
          &'APACHE OTSA (7207) BEING MAPPED TO 80906 OUACHITA NF ',&
          &'KIAMICHI DIST FOR FURTHER PROCESSING.')
     KODFOR = 80906
     IFORDI = 809
  case (7210)
     write(JOSTND,64)
     64 format(/,'********',T12,'KAW OTSA (7210) BEING MAPPED ',&
          &'TO 81005 OZARK NF BOSTON MOUNTAIN DIST FOR FURTHER ',&
          &'PROCESSING.')
     KODFOR = 81005
     IFORDI = 810
  case (7211)
     write(JOSTND,65)
     65 format(/,'********',T12,'OTOE-MISSOURIA OTSA (7211) BEING ',&
          &'MAPPED TO 81005 OZARK NF BOSTON MOUNTAIN DIST FOR FURTHER ',&
          &'PROCESSING.')
     KODFOR = 81005
     IFORDI = 810
  case (7212)
     write(JOSTND,66)
     66 format(/,'********',T12,'PAWNEE OTSA (7212) BEING MAPPED ',&
          &'TO 81005 OZARK NF BOSTON MOUNTAIN DIST FOR FURTHER ',&
          &'PROCESSING.')
     KODFOR = 81005
     IFORDI = 810
  case (7213)
     write(JOSTND,67)
     67 format(/,'********',T12,'PONCA OTSA (7213) BEING MAPPED ',&
          &'TO 81005 OZARK NF BOSTON MOUNTAIN DIST FOR FURTHER ',&
          &'PROCESSING.')
     KODFOR = 81005
     IFORDI = 810
  case (7215)
     write(JOSTND,68)
     68 format(/,'********',T12,'CITIZEN POTAWATOMI NATION-ABSENTEE ',&
          &'SHAWNEE OTSA (7215) BEING MAPPED TO 80901 OUACHITA NF ',&
          &'CHOCTAW DIST FOR FURTHER PROCESSING.')
     KODFOR = 80901
     IFORDI = 809
  case (7216)
     write(JOSTND,69)
     69 format(/,'********',T12,'IOWA OTSA (7216) BEING MAPPED TO ',&
          &'80901 OUACHITA NF CHOCTAW DIST FOR FURTHER PROCESSING.')
     KODFOR = 80901
     IFORDI = 809
  case (7218)
     write(JOSTND,70)
     70 format(/,'********',T12,'SAC AND FOX OTSA (7218) BEING ',&
          &'MAPPED TO 80901 OUACHITA NF CHOCTAW DIST FOR FURTHER ',&
          &'PROCESSING.')
     KODFOR = 80901
     IFORDI = 809
  case (7601)
     write(JOSTND,71)
     71 format(/,'********',T12,'CHICKASAW OTSA (7601) BEING MAPPED ',&
          &'TO 80906 OUACHITA NF KIAMICHI DIST FOR FURTHER PROCESSING.')
     KODFOR = 80906
     IFORDI = 809
  case (7602)
     write(JOSTND,72)
     72 format(/,'********',T12,'QUAPAW OTSA (7602) BEING MAPPED TO ',&
          &'81005 OZARK NF BOSTON MOUNTAIN DIST FOR FURTHER PROCESSING.')
     KODFOR = 81005
     IFORDI = 810
  case (7603)
     write(JOSTND,73)
     73 format(/,'********',T12,'EASTERN SHAWNEE OTSA (7603) BEING ',&
          &'MAPPED TO 81005 OZARK NF BOSTON MOUNTAIN DIST FOR FURTHER ',&
          &'PROCESSING.')
     KODFOR = 81005
     IFORDI = 810
  case (7604)
     write(JOSTND,74)
     74 format(/,'********',T12,'SENECA-CAYUGA OTSA (7604) BEING ',&
          &'MAPPED TO 81005 OZARK NF BOSTON MOUNTAIN DIST FOR FURTHER ',&
          &'PROCESSING.')
     KODFOR = 81005
     IFORDI = 810
  case (7605)
     write(JOSTND,75)
     75 format(/,'********',T12,'WYANDOTTE OTSA (7605) BEING MAPPED ',&
          &'TO 81005 OZARK NF BOSTON MOUNTAIN DIST FOR FURTHER ',&
          &'PROCESSING.')
     KODFOR = 81005
     IFORDI = 810
  case (7606)
     write(JOSTND,76)
     76 format(/,'********',T12,'MIAMI OTSA (7606) BEING MAPPED TO ',&
          &'81005 OZARK NF BOSTON MOUNTAIN DIST FOR FURTHER PROCESSING.')
     KODFOR = 81005
     IFORDI = 810
  case (7607)
     write(JOSTND,77)
     77 format(/,'********',T12,'PEORIA OTSA (7607) BEING ',&
          &'MAPPED TO 81005 OZARK NF BOSTON MOUNTAIN DIST FOR ',&
          &'FURTHER PROCESSING.')
     KODFOR = 81005
     IFORDI = 810
  case (7608)
     write(JOSTND,78)
     78 format(/,'********',T12,'MODOC OTSA (7608) BEING ',&
          &'MAPPED TO 81005 OZARK NF BOSTON MOUNTAIN DIST FOR ',&
          &'FURTHER PROCESSING.')
     KODFOR = 81005
     IFORDI = 810
  case (7609)
     write(JOSTND,79)
     79 format(/,'********',T12,'OSAGE RES. (7609) BEING ',&
          &'MAPPED TO 81005 OZARK NF BOSTON MOUNTAIN DIST FOR ',&
          &'FURTHER PROCESSING.')
     KODFOR = 81005
     IFORDI = 810
  case (7610)
     write(JOSTND,80)
     80 format(/,'********',T12,'CREEK OTSA (7610) BEING ',&
          &'MAPPED TO 80901 OUACHITA NF CHOCTAW DIST FOR FURTHER ',&
          &'PROCESSING.')
     KODFOR = 80901
     IFORDI = 809
  case (7611)
     write(JOSTND,81)
     81 format(/,'********',T12,'CHEROKEE OTSA (7611) BEING MAPPED ',&
          &'TO 81005 OZARK NF BOSTON MOUNTAIN DIST FOR FURTHER ',&
          &'PROCESSING.')
     KODFOR = 81005
     IFORDI = 810
  case (7612)
     write(JOSTND,82)
     82 format(/,'********',T12,'CHOCTAW OTSA (7612) BEING MAPPED TO',&
          &' 80906 OUACHITA NF KIAMICHI DIST FOR FURTHER PROCESSING.')
     KODFOR = 80906
     IFORDI = 809
  case (7613)
     write(JOSTND,83)
     83 format(/,'********',T12,'SEMINOLE OTSA (7613) BEING MAPPED ',&
          &'TO 80901 OUACHITA NF CHOCTAW DIST FOR FURTHER PROCESSING.')
     KODFOR = 80901
     IFORDI = 809
  case (8205)
     write(JOSTND,84)
     84 format(/,'********',T12,'MICCOSUKEE RES. (8205) BEING MAPPED',&
          &' TO 80505 FLORIDA NF SEMINOLE DIST FOR FURTHER PROCESSING.')
     KODFOR = 80505
     IFORDI = 805
  case (8207)
     write(JOSTND,85)
     85 format(/,'********',T12,'POARCH CREEK RES. (8207) BEING ',&
          &'MAPPED TO 80103 ALABAMA NF CONECUH DIST FOR FURTHER ',&
          &'PROCESSING.')
     KODFOR = 80103
     IFORDI = 801
  case (8210)
     write(JOSTND,86)
     86 format(/,'********',T12,'CATAWBA RES. (8210) BEING MAPPED ',&
          &'TO 81201 SUMTER NF ENOREE DIST FOR FURTHER PROCESSING.')
     KODFOR = 81201
     IFORDI = 812
  case (8212)
     write(JOSTND,87)
     87 format(/,'********',T12,'TUNICA-BILOXI RES. (8212) BEING ',&
          &'MAPPED TO 80601 KISATCHIE NF CATAHOULA DIST FOR FURTHER ',&
          &'PROCESSING.')
     KODFOR = 80601
     IFORDI = 806
  case (8213)
     write(JOSTND,88)
     88 format(/,'********',T12,'COUSHATTA RES. (8213) BEING MAPPED ',&
          &'TO 80602 KISATCHIE NF CALCASIEU DIST FOR FURTHER ',&
          &'PROCESSING.')
     KODFOR = 80602
     IFORDI = 806
  case (8219)
     write(JOSTND,89)
     89 format(/,'********',T12,'EASTERN CHEROKEE RES. (8219) BEING ',&
          &'MAPPED TO 81111 NORTH CAROLINA NF NANTAHALA DIST FOR ',&
          &'FURTHER PROCESSING.')
     KODFOR = 81111
     IFORDI = 811
  case (8220)
     write(JOSTND,90)
     90 format(/,'********',T12,'SEMINOLE (FL) TRUST LAND (8220) ',&
          &'BEING MAPPED TO 80505 FLORIDA NF SEMINOLE DIST FOR FURTHER ',&
          &'PROCESSING.')
     KODFOR = 80505
     IFORDI = 805
  case (8221)
     write(JOSTND,91)
     91 format(/,'********',T12,'MISSISSIPPI CHOCTAW RES. (8221) ',&
          &'BEING MAPPED TO 80701 MISSISSIPPI NF BIENVILLE DIST FOR ',&
          &'FURTHER PROCESSING.')
     KODFOR = 80701
     IFORDI = 807

  end select

  !     FINAL EVALUATION AND ASSIGN REGION/FOREST/DISTRICT CODE

  select case (KODFOR)

     !       HANDLE REGION 9 FORESTS DIFFERENTLY

  case (905,908)
     if (KODFOR .eq.905) then
        IFOR = 14
     else if (KODFOR .eq.908) then
        IFOR = 15
     end if
     KODIST = 1
     ISEFOR = 0
     write(JOSTND,11)JFOR(IFOR),KODIST

     KODFOR = (JFOR(IFOR)*100)+KODIST

     !       HANDLE FORT BRAGG MILITARY INSTALLATION

  case (701)

     write(JOSTND,23)
     23 format(/,'********',T12,'FORT BRAGG BEING MAPPED TO NFS IN ',&
          &'NC, UWHARRIE DISTRICT (81110) FOR FURTHER PROCESSING.')
     IFOR = 20
     KODIST = 10
     ISEFOR = 701

     KODFOR = 81110

     !       STANDARD REGION 8 REGION/FOREST/DISTRICT CODES

  case default
     do I = 1,NUMFOR

        if (IFORDI .eq. JFOR(I)) then

           !             DEFINE DISTRICT CODES (KODIST)

           KODIST = KODFOR-(IFORDI*100)
           if (JFOR(I) .eq.801) then
              if (KODIST.lt.1.or.KODIST.eq.2.or.KODIST.gt.7) then
                 KODIST = 3
              end if
           else if (JFOR(I) .eq.802) then
              if (KODIST.lt.11.or.KODIST.eq.16.or.KODIST.gt.17) then
                 KODIST = 11
              end if
           else if (JFOR(I) .eq.803) then
              if (KODIST.lt.1.or.KODIST.eq.3.or.KODIST.gt.8) then
                 KODIST = 8
              end if
           else if (JFOR(I) .eq.804) then
              if (KODIST.lt.1.or.KODIST.gt.6) KODIST = 1
           else if (JFOR(I) .eq.805) then
              if (KODIST.lt.1.or.KODIST.eq.3.or.KODIST.gt.6) then
                 KODIST = 4
              end if
           else if (JFOR(I) .eq.806) then
              if (KODIST.lt.1.or.KODIST.gt.5)KODIST = 3
           else if (JFOR(I) .eq.807) then
              if (KODIST.lt.1.or.KODIST.eq.5.or.(KODIST.gt.7.and.&
                   &KODIST.lt.17).or.KODIST.gt.17)KODIST = 7
           else if (JFOR(I) .eq.808) then
              if (KODIST.lt.1.or.(KODIST.ge.7.and.KODIST.le.10).or.&
                   &KODIST.gt.16)KODIST = 16
           else if (JFOR(I) .eq.809) then
              if (KODIST.lt.1.or.KODIST.gt.12)KODIST = 7
           else if (JFOR(I) .eq.810) then
              if (KODIST.lt.1.or.KODIST.gt.7)KODIST = 5
           else if (JFOR(I) .eq.811) then
              if (KODIST.lt.2.or.KODIST.gt.11)KODIST = 5
           else if (JFOR(I) .eq.812) then
              if (KODIST.lt.1.or.KODIST.eq.4.or.KODIST.gt.5) then
                 KODIST = 1
              end if
           else
              if (KODIST.lt.1.or.KODIST.eq.2.or.KODIST.eq.5.or.&
                   &KODIST.eq.6.or.KODIST.gt.8)KODIST = 3
           end if

           IFOR = I
           ISEFOR = JFOR(IFOR)
           KODFOR = (JFOR(IFOR)*100) + KODIST
           FORFOUND = .true.

           exit

        end if

        10 continue
     end do

     !         DEFAULT ERROR TRAP ASSIGN Talledega NF ALABAMA 80106

     if (.not. FORFOUND) then
        call ERRGRO (.true.,3)
        write(JOSTND,11) JFOR(IFOR),KODIST
        11 format(/,'********',T12,'FOREST CODE USED FOR THIS ',&
             &'PROJECTION IS',I4,&
             &/,'********',T12,'DISTRICT USED FOR THIS PROJECTION IS',I3)
        ISEFOR = JFOR(IFOR)
        KODFOR = (JFOR(IFOR)*100) + KODIST
     end if

  end select

  !  SET DEFAULT TLAT, TLONG, AND ELEVATION VALUES, BY FOREST

  select case(JFOR(IFOR))
  case(801)
     if (TLAT.eq.0)  TLAT = 32.37
     if (TLONG.eq.0) TLONG = 86.30
     if (ELEV.eq.0)  ELEV = 7.
  case(802)
     if (TLAT.eq.0)  TLAT = 37.99
     if (TLONG.eq.0) TLONG = 84.18
     if (ELEV.eq.0)  ELEV = 12.
  case(803)
     if (TLAT.eq.0)  TLAT = 34.30
     if (TLONG.eq.0) TLONG = 83.82
     if (ELEV.eq.0)  ELEV = 17.
  case(804)
     if (TLAT.eq.0)  TLAT = 35.16
     if (TLONG.eq.0) TLONG = 84.88
     if (ELEV.eq.0)  ELEV = 22.
  case(805)
     if (TLAT.eq.0)  TLAT = 30.44
     if (TLONG.eq.0) TLONG = 84.28
     if (ELEV.eq.0)  ELEV = 1.
  case(806)
     if (TLAT.eq.0)  TLAT = 31.32
     if (TLONG.eq.0) TLONG = 92.43
     if (ELEV.eq.0)  ELEV = 2.
  case(807)
     if (TLAT.eq.0)  TLAT = 33.31
     if (TLONG.eq.0) TLONG = 89.17
     if (ELEV.eq.0)  ELEV = 3.
  case(808)
     if (TLAT.eq.0)  TLAT = 37.27
     if (TLONG.eq.0) TLONG = 79.94
     if (ELEV.eq.0)  ELEV = 21.
  case(809)
     if (TLAT.eq.0)  TLAT = 34.50
     if (TLONG.eq.0) TLONG = 93.06
     if (ELEV.eq.0)  ELEV = 9.
  case(810)
     if (TLAT.eq.0)  TLAT = 35.28
     if (TLONG.eq.0) TLONG = 93.13
     if (ELEV.eq.0)  ELEV = 13.
  case(811)
     if (TLAT.eq.0)  TLAT = 35.60
     if (TLONG.eq.0) TLONG = 82.55
     if (ELEV.eq.0)  ELEV = 25.
  case(812)
     if (TLAT.eq.0)  TLAT = 34.00
     if (TLONG.eq.0) TLONG = 81.04
     if (ELEV.eq.0)  ELEV = 4.
  case(813)
     if (TLAT.eq.0)  TLAT = 31.34
     if (TLONG.eq.0) TLONG = 94.73
     if (ELEV.eq.0)  ELEV = 3.
  case(905)
     if (TLAT.eq.0)  TLAT = 37.95
     if (TLONG.eq.0) TLONG = 91.77
     if (ELEV.eq.0)  ELEV = 10.
  case(908)
     if (TLAT.eq.0)  TLAT = 37.74
     if (TLONG.eq.0) TLONG = 88.54
     if (ELEV.eq.0)  ELEV = 4.
  case(701)
     if (TLAT.eq.0)  TLAT = 35.60
     if (TLONG.eq.0) TLONG = 82.55
     if (ELEV.eq.0)  ELEV = 25.

  end select
  return
end
