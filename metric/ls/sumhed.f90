!========================!
!   metric/ls : sumhed   !
!========================!
! METRIC-LS $Id$
subroutine SUMHED
  implicit none

  !  THIS SUBROUTINE WRITES A HEADER FOR THE SUMMARY OUTPUT ON THE
  !  SCREEN.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'OUTCOM.f90'
  include 'SCREEN.f90'

  ! Variables
  character(len=7) :: FMT
  character(len=7) :: VVER
  character(len=10) :: REV
  integer :: I1,I2,ISTFNB,ISTLNB
  if (.not.LSCRN) goto 1000

  !  GET VARIANT NAME AND REVISION DATE.

  call VARVER (VVER)
  call REVISE (VVER,REV)
  if (VVER(:2).eq.'SM' .or. VVER(:2).eq.'SP' .or. VVER(:2).eq.'BP'&
       &.or. VVER(:2).eq.'SF' .or. VVER(:2).eq.'LP') then
     write(JOSCRN,1) VVER(:2),REV
     1 format(/T20,'CR-',A2,' FVS METRIC VARIANT -- RV:',A10/)
  else
     write(JOSCRN,2) VVER(:2),REV
     2 format(/T20,A2,' FVS-ONTARIO METRIC VARIANT -- RV:',A10/)
  end if

  write(JOSCRN,5) NPLT,MGMID
  5 format(/T10,'STAND = ',A26,'  MANAGEMENT CODE = ',A4/)
  I1 = ISTFNB(ITITLE)
  if (I1.gt.0) then
     I2 = ISTLNB(ITITLE)
     write (FMT,'(''(T'',I2.2,'',A)'')') (81-I2+I1)/2
     write (JOSCRN,FMT) ITITLE(I1:I2)
  end if
  write (JOSCRN,10)
  10 format (/T17,'SUMMARY STATISTICS (BASED ON TOTAL STAND AREA)'&
       &/1X,76('-')/&
       &T8,'START OF SIMULATION PERIOD    REMOVALS/HA  ',4X,&
       &'AFTER TREATMENT GROWTH',/,T7,28('-'),1X,17('-'),1X,16('-'),&
       &' CU  M',/, T7,'TREES',9X,'TOP',6X,'MERCH TREES MERCH SAWLG',&
       &9X,'TOP',6X,'PER YR',/,1X,'YEAR  /HA   BA SDI  HT  QMD ',&
       &'CU  M  /HA  CU  M CU  M  BA SDI  HT  QMD ACC MOR',/,1X,&
       &'---- ----- --- --- --- ---- ----- ----- ----- ----- ',&
       &'--- --- --- ---- --- ---')
  1000 continue
  return
end
