!=========================================!
!   Compute stand mean annual increment   !
!=========================================!
! SO $Id$
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

  !  INITIALIZE INTERNAL VARIABLES:

  !  SPECIES ORDER:
  !  1=WP,  2=SP,  3=DF,  4=WF,  5=MH,  6=IC,  7=LP,  8=ES,  9=SH,  10=PP,
  ! 11=JU, 12=GF, 13=AF, 14=SF, 15=NF, 16=WB, 17=WL, 18=RC, 19=WH,  20=PY,
  ! 21=WA, 22=RA, 23=BM, 24=AS, 25=CW, 26=CH, 27=WO, 28=WI, 29=GC,  30=MC,
  ! 31=MB, 32=OS, 33=OH

  data ISPNUM /&
       &119, 117, 202, 015, 264, 122, 108, 093, 101, 122,&
       &101, 015, 021, 101, 101, 101, 101, 101, 101, 101,&
       &746, 746, 746, 746, 746, 746, 746, 746, 746, 746,&
       &746, 202, 746/

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'MAICAL',6,ICYC)

  if (DEBUG) write(JOSTND,3)ICYC
  3 format(' ENTERING SUBROUTINE MAICAL  CYCLE =',I5)

  !   RMAI IS FUNCTION TO CALCULATE ADJUSTED MAI.

  !   IF SPECIES IS DF CONVERT COCHRAN SITE INDEX TO MCARDLE SITE
  !   INDEX. IF SPECIES IS HEMLOCK CONVERT SITE INDEX TO ENGLISH
  !   (SITE INDEX WAS PUBLISHED IN METRIC).

  if (ISISP .eq.0) ISISP = 3
  SSSI = SITEAR(ISISP)
  if (SSSI .eq.0.) SSSI = 140.0
  if (ISISP .eq.3) SSSI = -43.78 + 2.16*SSSI
  if (ISISP .eq.5) SSSI = SSSI*3.28
  ISICD = ISPNUM(ISISP)
  RMAI = ADJMAI(ISICD,SSSI,10.0,IERR)
  !      RMAI=50.
  if (RMAI .gt.128.0)RMAI = 128.0
  if (DEBUG) write(JOSTND,10)ICYC,RMAI
  10 format(' LEAVING SUBROUTINE MAICAL  CYCLE =',I5,5X,'RMAI =',F10.3)
  return
end
