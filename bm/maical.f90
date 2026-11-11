!=========================================!
!   Compute stand mean annual increment   !
!=========================================!
! BM $Id$
subroutine MAICAL
  implicit none

  !  THIS SUBROUTINE CALCULATES THE MAI FOR THE STAND. IT IS CALLED
  !  FROM CRATET.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'
  include 'HTCAL.f90'
  include 'OUTCOM.f90'
  include 'PLOT.f90'

  ! Variables
  logical :: DEBUG

  integer :: IERR,ISICD

  integer :: ISPNUM(MAXSP)

  real    :: ADJMAI,SSSI

  !  DATA STATEMENTS

  !  SPECIES ORDER:
  !   1=WP,  2=WL,  3=DF,  4=GF,  5=MH,  6=WJ,  7=LP,  8=ES,
  !   9=AF, 10=PP, 11=WB, 12=LM, 13=PY, 14=YC, 15=AS, 16=CW,
  !  17=OS, 18=OH

  !  SPECIES EXPANSION:
  !  WJ USES SO JU (ORIGINALLY FROM UT VARIANT; REALLY PP FROM CR VARIANT)
  !  WB USES SO WB (ORIGINALLY FROM TT VARIANT)
  !  LM USES UT LM
  !  PY USES SO PY (ORIGINALLY FROM WC VARIANT)
  !  YC USES WC YC
  !  AS USES SO AS (ORIGINALLY FROM UT VARIANT)
  !  CW USES SO CW (ORIGINALLY FROM WC VARIANT)
  !  OS USES BM PP BARK COEFFICIENT
  !  OH USES SO OH (ORIGINALLY FROM WC VARIANT)

  data ISPNUM/119, 117, 202, 015, 264, 101, 108, 093, 021, 122,&
       &101, 101, 101, 042, 746, 746, 122, 746/

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'MAICAL',6,ICYC)

  if (DEBUG) write(JOSTND,3)ICYC
  3 format(' ENTERING SUBROUTINE MAICAL  CYCLE =',I5)

  !   RMAI IS FUNCTION TO CALCULATE ADJUSTED MAI.

  if (ISISP .eq.0) ISISP = 3
  SSSI = SITEAR(ISISP)
  if (SSSI .eq.0.) SSSI = 140.0
  ISICD = ISPNUM(ISISP)
  RMAI = ADJMAI(ISICD,SSSI,10.0,IERR)
  if (RMAI .gt.128.0)RMAI = 128.0
  if (DEBUG) write(JOSTND,10)ICYC,RMAI
  10 format(' LEAVING SUBROUTINE MAICAL  CYCLE =',I5,5X,'RMAI =',F10.3)
  return
end
