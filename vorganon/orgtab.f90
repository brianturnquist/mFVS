!=======================!
!   vorganon : orgtab   !
!=======================!
! VORGANON $Id$
subroutine ORGTAB
  implicit none

  !    WRITES TABLE OF FVS-ORGANON PARAMETER SETTINGS.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'ORGANON.f90'
  include 'PLOT.f90'
  include 'VOLSTD.f90'

  ! Variables
  integer :: I,J

  !     WRITE THE START DELIMITER TO THE ORGANON TABLE

  write(JOSTND, 20)
  20 format (130('-'))

  !  HEADER, TREE DATA AND PARAMETER SETTING INFORMATION

  select case (VARACD)

  case ('OC')
     write(JOSTND,100)
     100 format(/,'PARAMETER SETTINGS FOR FVS-ORGANON ',&
          &'SOUTHWEST OREGON MODEL TYPE:')

  case default
     if (IMODTY .eq.2) then
        write(JOSTND,101)
        101 format(/,'PARAMETER SETTINGS FOR FVS-ORGANON ',&
             &'NORTHWEST OREGON MODEL TYPE:')
     else
        write(JOSTND,102)
        102 format(/,'PARAMETER SETTINGS FOR FVS-ORGANON ',&
             &'STAND MANAGEMENT COOP MODEL TYPE:')
     end if

  end select

  ! WRITE ORGANON TYPE HEADINGS:

  write(JOSTND,120)
  120 format(/T30,'O R G A N O N',&
       &/T15,'ORegon Growth ANalysis and projectiON system',&
       &/T25,'Growth & Yield Model for')

  select case (VARACD)

  case ('OC')
     write(JOSTND,125)
     125 format(T17,'Southwest Oregon Mixed Conifer Forests',&
          &//T22,'SW OREGON VERSION, EDITION 9.1')

  case default
     if (IMODTY .eq.2) then
        write(JOSTND,126)
        126 format(T24,'Northwest Oregon Forests',&
             &//T22,'NW OREGON VERSION, EDITION 9.1')
     else
        write(JOSTND,127)
        127 format(T20,'Stand Management Coop Conifer Forests',&
             &//T19,'STAND MANAGEMENT COOP VERSION, EDITION 9.1')
     end if

  end select

  select case (VARACD)

  case ('OC')
     write(JOSTND,130)
     130 format(T28,'by David W. Hann',&
          &/T27,'and Mark Hanus',&
          &/T28,'College of Forestry',&
          &/T25,'Oregon State University',&
          &//T24,'This model was funded by:',&
          &/T11,'Forestry Intensified Research (FIR) and the USDI BLM')

  case default
     if (IMODTY .eq.2) then
        write(JOSTND,131)
        131 format(T28,'by David W. Hann',&
             &/T12,'Martin Ritchie, Chao-Huan Wang, and Abdel Azim Zumrawi',&
             &/T28,'College of Forestry',&
             &/T25,'Oregon State University',&
             &//T24,'This model was funded by the',&
             &/T18,'College of Forestry Research Forests')
     else
        write(JOSTND,132)
        132 format(T28,'by David W. Hann',&
             &/T18,'David D. Marshall and Mark L. Hanus',&
             &/T28,'College of Forestry',&
             &/T25,'Oregon State University',&
             &//T24,'This model was funded by:',&
             &/T18,'The Stand Management Cooperative (SMC)')
     end if

  end select

  !  IMPORTANT SITE INDEX VALUES; SDI; STAND AGE

  select case (VARACD)

  case ('OC')
     write(JOSTND,200) RVARS(1),RVARS(2) !DF, PP
     200 format(//,'                Douglas-fir Site Index: ',F6.2,/,&
          &'             Ponderosa Pine Site Index: ',F6.2)
     write(JOSTND,201) RVARS(3),RVARS(4),RVARS(5) !DF, GF/WF, PP
     201 format(/,'                   Douglas-fir Max SDI: ',F6.2,/,&
          &'               Grand/White Fir Max SDI: ',F6.2,/,&
          &'                Ponderosa Pine Max SDI: ',F6.2)

  case default
     write(JOSTND,210) RVARS(1),RVARS(2) !DF, WH
     210 format(//,'                Douglas-fir Site Index: ',F6.2,/,&
          &'            Western Hemlock Site Index: ',F6.2)
     write(JOSTND,211) RVARS(3),RVARS(4),RVARS(5) !DF, GF, WH
     211 format(/,'                   Douglas-fir Max SDI: ',F6.2,/,&
          &'                     Grand Fir Max SDI: ',F6.2,/,&
          &'               Western Hemlock Max SDI: ',F6.2)

  end select

  !  SELECTED INITIAL INDS(i) ARRAY VALUES

  if (INDS(1) .eq.0) then
     write(JOSTND,310)
     310 format(/,'                    HEIGHT CALIBRATION: No')
  else
     write(JOSTND,311)
     311 format(/,'                    HEIGHT CALIBRATION: Yes')
  end if

  if (INDS(2) .eq.0) then
     write(JOSTND,320)
     320 format(1X,'     HEIGHT-TO-CROWN BASE CALIBRATION: No')
  else
     write(JOSTND,321)
     321 format(1X,'     HEIGHT-TO-CROWN BASE CALIBRATION: Yes')
  end if

  if (INDS(3) .eq.0) then
     write(JOSTND,330)
     330 format(1X,'          DIAMETER GROWTH CALIBRATION: No')
  else
     write(JOSTND,331)
     331 format(1X,'          DIAMETER GROWTH CALIBRATION: Yes')
  end if

  if (INDS(4) .eq.0) then
     write(JOSTND,340)
     340 format(1X,'                      EVEN-AGED STAND: No')
  else
     write(JOSTND,341)
     341 format(1X,'                      EVEN-AGED STAND: Yes')
  end if
  if (ITEST .eq.1) then
     write(JOSTND,342)
     342 format(/'*NOTE -- Stand was declared even-aged, but stand ',&
          &'age was not entered.',/,'Stand indicator was reset to ',&
          &'uneven-aged so ORGANON model will run.',/,'However, if this',&
          &' is a bare-ground planting scenario, the stand indicator',&
          &' will be set back to even-aged.',/)
  end if

  if (INDS(5) .eq.0) then
     write(JOSTND,350)
     350 format(1X,'                             TRIPLING: No')
  else
     write(JOSTND,351)
     351 format(1X,'                             TRIPLING: Yes')
  end if

  if (INDS(9) .eq.0) then
     write(JOSTND,360)
     360 format(1X,'     MAX SDI FOR ADDITIONAL MORTALITY: No')
  else
     write(JOSTND,361)
     361 format(1X,'     MAX SDI FOR ADDITIONAL MORTALITY: Yes')
  end if

  !  INITIAL ORGANON CALIBRATION VALUES

  select case (VARACD)

  case ('OC')
     write(JOSTND,400) ((ACALIB(I,J),I = 1,3),J = 1,18)
     400 format(/T3,'CALIBRATION RATIOS USED FOR THIS RUN:',&
          &/T42,'HT/DBH    HTCB    DIAM',&
          &/T28,'DOUGLAS FIR:',3F8.2,&
          &/T24,'WHITE/GRAND FIR:',3F8.2,&
          &/T25,'PONDEROSA PINE:',3F8.2,&
          &/T29,'SUGAR PINE:',3F8.2,&
          &/T26,'INCENSE-CEDAR:',3F8.2,&
          &/T24,'WESTERN HEMLOCK:',3F8.2,&
          &/T23,'WESTERN REDCEDAR:',3F8.2,&
          &/T28,'PACIFIC YEW:',3F8.2,&
          &/T24,'PACIFIC MADRONE:',3F8.2,&
          &/T23,'GIANT CHINQUAPIN:',3F8.2,&
          &/T33,'TANOAK:',3F8.2,&
          &/T24,'CANYON LIVE OAK:',3F8.2,&
          &/T26,'BIGLEAF MAPLE:',3F8.2,&
          &/T23,'OREGON WHITE OAK:',3F8.2,&
          &/T19,'CALIFORNIA BLACK OAK:',3F8.2,&
          &/T30,'RED ALDER:',3F8.2,&
          &/T24,'PACIFIC DOGWOOD:',3F8.2,&
          &/T25,'WILLOW SPECIES:',3F8.2)

  case default
     write(JOSTND,410) ((ACALIB(I,J),I = 1,3),J = 1,11)
     410 format(/T3,'CALIBRATION RATIOS USED FOR THIS RUN:',&
          &/T42,'HT/DBH    HTCB    DIAM',&
          &/T28,'DOUGLAS FIR:',3F8.2,&
          &/T30,'GRAND FIR:',3F8.2,&
          &/T24,'WESTERN HEMLOCK:',3F8.2,&
          &/T23,'WESTERN REDCEDAR:',3F8.2,&
          &/T28,'PACIFIC YEW:',3F8.2,&
          &/T10,'WHITE ALDER / PACIFIC MADRONE:',3F8.2,&
          &/T26,'BIGLEAF MAPLE:',3F8.2,&
          &/T11,'OREGON WHITE / CAL BLACK OAK:',3F8.2,&
          &/T30,'RED ALDER:',3F8.2,&
          &/T24,'PACIFIC DOGWOOD:',3F8.2,&
          &/T25,'WILLOW SPECIES:',3F8.2)

  end select

  !  VOLUME SPECIFICATIONS
  !  LORGVOLS .TRUE. OSU VOLUME SPECIFICATIONS
  !  LORGVOLS .FALSE. NVEL VOLUME SPECIFICATIONS

  if (LORGVOLS) then

     ! ORGANON OSU CUBIC FOOT

     write(JOSTND,500)
     500 format(/T5,'VOLUME SPECIFICATIONS FOR THIS RUN:',&
          &/T23,'VOLUME EQUATIONS: ORGANON OSU')
     write(JOSTND,501) CFTD,CFSH,CFTDHW
     501 format(/T19,'SOFTWOODS CF TOP DIB:',F5.1,' INCHES',&
          &/T14,'SOFTWOODS CF STUMP HEIGHT:',F5.1,' FEET',&
          &/T19,'HARDWOODS CF TOP DIB:',F5.1,' INCHES')

     ! ORGANON OSU BOARD FOOT

     write(JOSTND,502) LOGTD,LOGTA,LOGSH,LOGLL,LOGML
     502 format(/T29,'BF TOP DIB:',F5.1,' INCHES',&
          &/T21,'LOG TRIM ALLOWANCE:',F5.1,' INCHES',&
          &/T28,'BF STUMP HT:',F5.1,' FEET',&
          &/T22,'TARGET LOG LENGTH:',F5.1,' FEET',&
          &/T21,'MINIMUM LOG LENGTH:',F5.1,' FEET')

     ! FVS NVEL VOLUME SPECIFICATIONS, BY SPECIES

  else
     write(JOSTND,510)
     510 format(/T5,'VOLUME SPECIFICATIONS FOR THIS RUN:',/T23,&
          &'VOLUME EQUATIONS: FVS NATIONAL VOLUME ESTIMATOR LIBRARY')
     write(JOSTND,511)
     511 format(/T42,'----- CUBIC FEET -----  ----- BOARD FEET -----',&
          &/T32,'SPECIES    MIN       TOP   STUMP     MIN     TOP   STUMP',&
          &/T35,'CODE    DBH       DIB      HT     DBH     DIB      HT')
     do I = 1,MAXSP
        write(JOSTND,512)JSP(I),DBHMIN(I),TOPD(I),STMP(I),BFMIND(I),&
             &BFTOPD(I),BFSTMP(I)
        512 format(T37,A2,':',F6.1,2X,5F8.1)
     end do
  end if

  !     WRITE THE END DELIMITER TO THE ORGANON TABLE

  write(JOSTND, 20)
  return
end
