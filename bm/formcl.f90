!=================================!
!   Compute volume form factors   !
!=================================!
! BM $Id$
subroutine FORMCL(ISPC,IFOR,D,FC)
  implicit none

  ! THIS PROGRAM CALCULATES FORM FACTORS FOR CALCULATING CUBIC AND
  ! BOARD FOOT VOLUMES.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'

  ! Variables
  integer :: IFCDBH,IFOR,ISPC

  real    :: D,FC

  real    :: MALHFC(MAXSP,5),OCHOFC(MAXSP,5),UMATFC(MAXSP,5)
  real    :: WLWHFC(MAXSP,5)

  !  FOREST ORDER: (IFOR)
  !  1=MALHEUR(604)          2=OCHOCO(607)         3=UMATILLA(614)
  !  4=WALLOWA-WHITMAN(616)

  !  DATA STATEMENTS:

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

  !  MALHEUR FORM CLASS VALUES

  data MALHFC/&
       &78., 78., 78., 76., 75., 60., 80., 77., 78., 78.,&
       &80., 80., 56., 56., 77., 76., 60., 77.,&
       &78., 79., 77., 78., 79., 60., 83., 80., 80., 78.,&
       &81., 81., 60., 66., 77., 78., 60., 77.,&
       &79., 80., 77., 77., 79., 60., 83., 82., 80., 80.,&
       &81., 81., 60., 68., 77., 78., 60., 77.,&
       &81., 82., 80., 76., 79., 60., 80., 84., 82., 82.,&
       &82., 82., 60., 68., 77., 78., 60., 77.,&
       &78., 77., 77., 76., 78., 60., 80., 84., 82., 83.,&
       &82., 82., 60., 68., 76., 78., 60., 76./

  !  OCHOCO FORM CLASS VALUES

  data OCHOFC/&
       &78., 78., 79., 76., 75., 60., 70., 82., 78., 76.,&
       &80., 80., 56., 56., 77., 76., 60.,  77.,&
       &80., 78., 79., 78., 78., 60., 75., 82., 76., 78.,&
       &81., 81., 60., 66., 77., 78., 60.,  77.,&
       &80., 80., 76., 77., 79., 60., 75., 82., 74., 78.,&
       &81., 81., 60., 68., 77., 78., 60.,  77.,&
       &82., 80., 76., 74., 79., 60., 75., 82., 74., 80.,&
       &82., 82., 60., 68., 77., 78., 60.,  77.,&
       &80., 80., 76., 74., 78., 60., 75., 82., 74., 80.,&
       &82., 82., 60., 68., 76., 78., 60.,  76./

  !  UMATILLA FORM CLASS VALUES

  data UMATFC/&
       &78., 78., 77., 76., 75., 60., 86., 77., 74., 78.,&
       &80., 80., 56., 56., 77., 76., 60., 77.,&
       &78., 78., 77., 78., 75., 60., 86., 77., 74., 78.,&
       &81., 81., 60., 66., 77., 78., 60., 77.,&
       &80., 78., 77., 77., 75., 60., 86., 75., 74., 80.,&
       &81., 81., 60., 68., 77., 78., 60., 77.,&
       &81., 78., 77., 76., 79., 60., 86., 75., 75., 81.,&
       &82., 82., 60., 68., 77., 78., 60., 77.,&
       &81., 78., 77., 76., 78., 60., 86., 75., 75., 81.,&
       &82., 82., 60., 68., 76., 78., 60., 76./

  !  WALLOWA-WHITMAN FORM CLASS VALUES

  data WLWHFC/&
       &78., 78., 78., 76., 75., 60., 85., 84., 78., 78.,&
       &80., 80., 56., 56., 77., 76., 60., 77.,&
       &78., 82., 77., 78., 79., 60., 86., 84., 79., 78.,&
       &81., 81., 60., 66., 77., 78., 60., 77.,&
       &78., 77., 77., 77., 79., 60., 85., 84., 79., 80.,&
       &81., 81., 60., 68., 77., 78., 60., 77.,&
       &78., 75., 77., 76., 79., 60., 85., 84., 79., 82.,&
       &82., 82., 60., 68., 77., 78., 60., 77.,&
       &78., 75., 77., 76., 78., 60., 85., 84., 79., 83.,&
       &82., 82., 60., 68., 76., 78., 60., 76./

  !  FOR REGION 6 FORESTS, LOAD THE FORM CLASS USING TABLE VALUES.
  !  IF A FORM CLASS HAS BEEN ENTERED VIA KEYWORD, USE IT INSTEAD.

  if (FRMCLS(ISPC).le.0.) then
     IFCDBH = INT((D - 1.0) / 10.0 + 1.0)
     if (IFCDBH .lt.1) IFCDBH = 1
     if (D.gt.40.9) IFCDBH = 5
     if (IFOR.eq.1) then
        FC = MALHFC(ISPC,IFCDBH)
     else if (IFOR.eq.2) then
        FC = OCHOFC(ISPC,IFCDBH)
     else if (IFOR.eq.3) then
        FC = UMATFC(ISPC,IFCDBH)
     else
        FC = WLWHFC(ISPC,IFCDBH)
     end if
  else
     FC = FRMCLS(ISPC)
  end if

  return
end
