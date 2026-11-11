!=========================================!
!   Compute stand mean annual increment   !
!=========================================!
! TT $Id$
subroutine MAICAL
  implicit none

  !  THIS SUBROUTINE CALCULATES THE MAI FOR THE STAND. IT IS CALLED
  !  FROM CRATET.

  ! Common blocks
  include  'PRGPRM.f90'

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

  ! SPECIES ORDER FOR TETONS VARIANT:

  !  1=WB,  2=LM,  3=DF,  4=PM,  5=BS,  6=AS,  7=LP,  8=ES,  9=AF, 10=PP,
  ! 11=UJ, 12=RM, 13=BI, 14=MM, 15=NC, 16=MC, 17=OS, 18=OH

  ! VARIANT EXPANSION:
  ! BS USES ES EQUATIONS FROM TT
  ! PM USES PI (COMMON PINYON) EQUATIONS FROM UT
  ! PP USES PP EQUATIONS FROM CI
  ! UJ AND RM USE WJ (WESTERN JUNIPER) EQUATIONS FROM UT
  ! BI USES BM (BIGLEAF MAPLE) EQUATIONS FROM SO
  ! MM USES MM EQUATIONS FROM IE
  ! NC AND OH USE NC (NARROWLEAF COTTONWOOD) EQUATIONS FROM CR
  ! MC USES MC (CURL-LEAF MTN-MAHOGANY) EQUATIONS FROM SO
  ! OS USES OT (OTHER SP.) EQUATIONS FROM TT

  !  INITIALIZE INTERNAL VARIABLES:

  data ISPNUM/&
       &101, 101, 202, 101, 093, 746, 108, 093, 019, 122,&
       &101, 101, 746, 746, 101, 746, 101, 101/

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
