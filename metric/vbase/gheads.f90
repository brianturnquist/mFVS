!===========================!
!   metric/vbase : gheads   !
!===========================!
! METRIC-VBASE $Id: gheads.f 2438 2018-07-05 16:54:21Z gedixon $
subroutine GHEADS(NPLT,MGMID,KOSTND,KOTREE,ITITLE)
  implicit none

  !     PRINTS HEADINGS. IF I/O UNIT(S) IS/ARE ZERO THE PRINTING
  !     OF THE CORRESPONDING HEADING IS SKIPPED.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'

  ! Variables
  character(len=4) :: MGMID
  character(len=26) :: NPLT
  character(len=72) :: ITITLE

  integer :: ISTLNB,KOSTND,KOTREE

  if (KOSTND.eq.0) go to 30

  !     WRITE HEADING FOR STAND COMPOSITION TABLE.

  call GROHED(KOSTND)
  write (KOSTND,5) NPLT,MGMID,ITITLE(1:MAX(1,ISTLNB(ITITLE)))
  5 format(/'STAND ID: ',A26,4X,'MGMT ID: ',A4,4X,A/)

  select case (VARACD)

     !  EASTERN TWIGS AND CANADIAN ONTARIO VARIANTS

  case ('CS','LS','NE','ON')
     write (KOSTND,10)
     10 format(/T41,&
          &'STAND COMPOSITION (BASED ON STOCKABLE AREA)'/&
          &122('-')/T29,'PERCENTILE POINTS IN THE'/&
          &T22,'DISTRIBUTION OF STAND ATTRIBUTES BY DBH       TOTAL/HA'/&
          &T9,'STAND',7X,41('-'),6X,'OF STAND',9X,&
          &'DISTRIBUTION OF STAND ATTRIBUTES BY'/'YEAR  ATTRIBUTES',&
          &6X,'10     30     50     70     90    100',7X,'ATTRIBUTES',7X,&
          &'SPECIES AND 3 USER-DEFINED SUBCLASSES'/,4('-'),2X,11('-'),3X,&
          &6(6('-'),1X),2X,14('-'),2X,42('-')/T34,'(DBH IN CM)')

     !  WESTERN, SOUTHERN, AND CANADIAN BRITISH COLUMBIA VARIANTS

  case default
     write (KOSTND,20)
     20 format(/T41,&
          &'STAND COMPOSITION (BASED ON STOCKABLE AREA)'/&
          &122('-')/T29,'PERCENTILE POINTS IN THE'/&
          &T22,'DISTRIBUTION OF STAND ATTRIBUTES BY DBH       TOTAL/HA'/&
          &T9,'STAND',7X,41('-'),6X,'OF STAND',9X,&
          &'DISTRIBUTION OF STAND ATTRIBUTES BY'/'YEAR  ATTRIBUTES',&
          &6X,'10     30     50     70     90    100',7X,'ATTRIBUTES',7X,&
          &'SPECIES AND 3 USER-DEFINED SUBCLASSES'/,4('-'),2X,11('-'),3X,&
          &6(6('-'),1X),2X,14('-'),2X,42('-')/T34,'(DBH IN CM)')
  end select

  30 continue

  !     WRITE HEADING FOR SAMPLE TREE TABLE.

  if (KOTREE .eq.0) go to 50

  !     WRITE LABELS FOR EXAMPLE TREE OUTPUT.

  call GROHED(KOTREE)
  write (KOTREE,5) NPLT,MGMID,ITITLE(1:MAX(1,ISTLNB(ITITLE)))

  select case (VARACD)

     !  EASTERN TWIGS AND CANADIAN ONTARIO VARIANTS

  case ('CS','LS','NE','ON')
     write (KOTREE,35)
     35 format(&
          &/126('-')&
          &/T22,'ATTRIBUTES OF SELECTED SAMPLE TREES',16X,&
          &' ADDITIONAL STAND ATTRIBUTES (BASED ON STOCKABLE AREA)'/&
          &6X,65('-'),2X,53('-')/6X,'INITIAL',&
          &27X,'  LIVE  PAST DBH   BASAL  TREES',9X,'QUADRATIC    TREES',&
          &4X,' BASAL    TOP HT       '/6X,'TREE/HA',12X,&
          &'  DBH   HEIGHT',&
          &2X,'CROWN    GROWTH    AREA    PER  STAND   MEAN DBH      PER',&
          &5X,' AREA   LARGEST        '/'YEAR    %TILE SPECIES     (CM)  ',&
          &'    (M)  RATIO      (CM)   %TILE     HA    AGE       (CM)',&
          &6X,' HA   (M2/HA)     40/HA            '/'----',2X,2(7('-'),1X),&
          &2(8('-'),1X),6('-'),1X,9('-'),1X,7('-'),1X,6('-'),2X,5('-'),2X,&
          &9('-'),3X,6('-'),1X,2(9('-'),1X),7('-'))

     !  WESTERN, SOUTHERN, AND CANADIAN BRITISH COLUMBIA VARIANTS

  case default
     write (KOTREE,40)
     40 format(&
          &/126('-')&
          &/T22,'ATTRIBUTES OF SELECTED SAMPLE TREES',16X,&
          &' ADDITIONAL STAND ATTRIBUTES (BASED ON STOCKABLE AREA)'/&
          &6X,65('-'),2X,53('-')/6X,'INITIAL',&
          &27X,'  LIVE  PAST DBH   BASAL  TREES',9X,'QUADRATIC    TREES',&
          &4X,' BASAL     TOPHT   CROWN'/5X,' TREE/HA',12X,&
          &'  DBH   HEIGHT',&
          &2X,'CROWN    GROWTH    AREA    PER  STAND   MEAN DBH      PER',&
          &5X,' AREA   LARGEST    COMP'/'YEAR    %TILE SPECIES     (CM) ',&
          &'     (M)  RATIO      (CM)   %TILE     HA    AGE       (CM)',&
          &4X,'   HA   (M2/HA)     40/HA  FACTOR'/'----',2X,2(7('-'),1X),&
          &2(8('-'),1X),6('-'),1X,9('-'),1X,7('-'),1X,6('-'),2X,5('-'),2X,&
          &9('-'),3X,6('-'),1X,2(9('-'),1X),7('-'))
  end select

  50 continue

  return

  !  CALLED FROM **SITSET** ROUTINES TO WRITE TABLE HEADERS FR THE
  !  1) FIAHEAD   - THE SPECIES FIA NUMBER / ALPHA CODE TRANSLATION TABLE
  !  2) VOLEQHEAD - VOLUME EQUATION NUMBERS

  entry FIAHEAD(KOSTND)

  write(KOSTND,210)
  210 format(/T12,'ALPHA SPECIES - FIA CODE CROSS REFERENCE:')
  return

  entry VOLEQHEAD(KOSTND)

  write(KOSTND,220)
  220 format(/T35,'NATIONAL VOLUME ESTIMATOR LIBRARY EQUATION NUMBERS',&
       &/,4('SPECIES CUBIC FOOT BOARD FOOT ')/&
       !     & 4('          EQ. NO.    EQ. NO.  ')/
       &4('------- ---------- ---------- '))

  return
end
