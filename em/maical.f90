!=========================================!
!   Compute stand mean annual increment   !
!=========================================!
! EM $Id$
subroutine MAICAL
  implicit none

  !  THIS SUBROUTINE CALCULATES THE MAI FOR THE STAND. IT IS CALLED
  !  FROM CRATET.

  ! Common blocks
  include 'PRGPRM.f90'
  include  'ARRAYS.f90'

  include  'PLOT.f90'

  include  'COEFFS.f90'

  include  'CONTRL.f90'

  include  'OUTCOM.f90'

  include  'HTCAL.f90'

  ! Variables
  logical :: DEBUG
  integer :: ISPNUM(MAXSP),ISICD,IERR
  real    :: SSSI,ADJMAI

  !  SPECIES ORDER:
  !   1=WB,  2=WL,  3=DF,  4=LM,  5=LL,  6=RM,  7=LP,  8=ES,
  !   9=AF, 10=PP, 11=GA, 12=AS, 13=CW, 14=BA, 15=PW, 16=NC,
  !  17=PB, 18=OS, 19=OH

  !  SPECIES EXPANSION
  !  LM USES IE LM (ORIGINALLY FROM TT VARIANT)
  !  LL USES IE AF (ORIGINALLY FROM NI VARIANT)
  !  RM USES IE JU (ORIGINALLY FROM UT VARIANT)
  !  AS,PB USE IE AS (ORIGINALLY FROM UT VARIANT)
  !  GA,CW,BA,PW,NC,OH USE IE CO (ORIGINALLY FROM CR VARIANT)

  !  INITIALIZE INTERNAL VARIABLES:
  !  (ISPNUM CONTAINS THE FIA CODE FOR THE PROXY EQUATION FOR THE CALL TO
  !   SUBROUTINE **ADJMAI**)

  data ISPNUM/ 119, 117, 202, 101, 019, 101, 108, 093, 021, 122,&
       &101, 101, 101, 101, 101, 101, 101, 122, 101/

  !  CHECK FOR DEBUG.

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
