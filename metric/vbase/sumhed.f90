!===========================!
!   metric/vbase : sumhed   !
!===========================!
! METRIC-VBASE $Id: sumhed.f 2494 2018-09-12 20:34:17Z lancedavid $
subroutine SUMHED
  implicit none

  !  THIS SUBROUTINE WRITES A HEADER FOR THE SUMMARY OUTPUT ON THE
  !  SCREEN.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'OUTCOM.f90'
  include 'PLOT.f90'
  include 'SCREEN.f90'

  ! Variables
  character(len=7) :: FMT
  character(len=10) :: REV

  character(len=2) :: CRMT(5)

  integer :: I1,I2,ISTFNB,ISTLNB

  !  DEFINITION OF LOCAL VARIABLES:

  !     CRMT -- CENTRAL ROCKIES ALPHABETIC MODEL TYPE DESIGNATOR

  !  DATA STATEMENTS

  data CRMT/'SM','SP','BP','SF','LP'/

  if (.not.LSCRN) goto 1000

  !  GET VARIANT REVISION DATE.

  call REVISE (VARACD,REV)

  select case (VARACD)

  case ('CR')
     write(JOSCRN,1) CRMT(IMODTY),REV
     1 format(/T19,'CR-',A2,' FVS METRIC VARIANT -- RV:',A10/)

  case default
     write(JOSCRN,2) VARACD,REV
     2 format(/T19,A2,' FVS METRIC VARIANT -- RV:',A10/)

  end select

  write(JOSCRN,5) NPLT,MGMID
  5 format(/T9,'STAND = ',A26,'  MANAGEMENT CODE = ',A4/)
  I1 = ISTFNB(ITITLE)
  if (I1.gt.0) then
     I2 = ISTLNB(ITITLE)
     write (FMT,'(''(T'',I2.2,'',A)'')') (81-I2+I1)/2
     write (JOSCRN,FMT) ITITLE(I1:I2)
  end if

  select case (VARACD)

     !  EASTERN VARIANTS

  case ('CS','LS','NE','SN')
     write (JOSCRN,10)
     10 format (/T16,'SUMMARY STATISTICS (BASED ON TOTAL STAND AREA)'&
          &/,76('-')/&
          &T8,'START OF SIMULATION PERIOD    REMOVALS/HA  ',5X,&
          &'AFTER TREATMENT  GROWTH',/,T6,28('-'),1X,17('-'),1X,16('-'),&
          &'  CU M',/, T6,'TREES',9X,'TOP',6X,'MERCH TREES MERCH SAWLG',&
          &9X,'TOP',6X,'PER YR',/,'YEAR  /HA  BA  SDI HT  QMD',&
          &'CU  M  /HA  CU  M BD  M  BA SDI  HT  QMD ACC MOR',/,&
          &'---- ----- --- --- --- ---- ----- ----- ----- ----- ',&
          &'--- --- --- ---- --- ---')

     !  CANADIAN VARIANTS

  case ('BC','ON')
     write (JOSCRN,15)
     15 format (/T16,'SUMMARY STATISTICS (BASED ON TOTAL STAND AREA)'&
          &/,76('-')/&
          &T7,'START OF SIMULATION PERIOD    REMOVALS/HA  ',3X,&
          &'AFTER TREATMENT   GROWTH',/,T6,28('-'),1X,17('-'),1X,16('-'),&
          &'    CU M',/, T6,'TREES',9X,'TOP',6X,'  GTV TREES   GTV   GMV',&
          &9X,'TOP',6X,' PER YR',/,'YEAR   /HA  BA SDI  HT  QMD ',&
          &' CU M   /HA  CU M  CU M  BA SDI  HT  QMD ACC MOR',/,&
          &'---- ----- --- --- --- ---- ----- ----- ----- ----- ',&
          &'--- --- --- ---- --- ---')

     !  WESTERN AND SOUTHERN VARIANTS

  case default
     write (JOSCRN,20)
     20 format (/T16,'SUMMARY STATISTICS (BASED ON TOTAL STAND AREA)'&
          &/,76('-')/&
          &T8,'START OF SIMULATION PERIOD    REMOVALS/HA  ',4X,&
          &'AFTER TREATMENT GROWTH',/,T6,28('-'),1X,17('-'),1X,16('-'),&
          &'  CU M',/, T6,'TREES',9X,'TOP',6X,'TOTAL TREES TOTAL MERCH',&
          &9X,'TOP',6X,'PER YR',/,'YEAR /HA    BA SDI  HT  QMD ',&
          &'  CU M/HA    CU M  CU M  BA SDI  HT  QMD ACC MOR',/,&
          &'---- ----- --- --- --- ---- ----- ----- ----- ----- ',&
          &'--- --- --- ---- --- ---')

  end select

  1000 continue

  return
end
