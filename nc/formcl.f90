!=================================!
!   Compute volume form factors   !
!=================================!
! NC $Id$
subroutine FORMCL(ISPC,IFOR,D,FC)
  implicit none

  ! THIS PROGRAM CALCULATES FORM FACTORS FOR CALCULATING CUBIC AND
  ! BOARD FOOT VOLUMES.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'

  ! Variables
  real    :: SISKFC(MAXSP,5),BLM712(MAXSP),FC,D
  integer :: IFOR,ISPC,IFCDBH

  !  FOREST ORDER: (IFOR)
  !  1=KLAMATH(505)       2=SIX RIVERS(510)     3=TRINITY(514)
  !  4=SISKIYOU(611)      5=HOOPA(705)          6=SIMPSON(800)
  !  7=BLM COOS BAY(712)

  !  SPECIES ORDER: (ISPC)
  !  1=OC  2=SP  3=DF  4=WF  5=M   6=IC  7=BO  8=TO  9=RF 10=PP 11=OH, 12=RW

  !  SISKIYOU  FORM CLASS VALUES

  data SISKFC/&
       &91., 96., 90., 98., 98., 89., 98., 91., 92., 93., 95., 82.,&
       &84., 91., 86., 90., 88., 89., 98., 91., 83., 89., 86., 82.,&
       &79., 85., 81., 86., 84., 77., 98., 82., 80., 83., 78., 79.,&
       &78., 83., 80., 85., 81., 73., 98., 80., 80., 81., 76., 78.,&
       &78., 82., 80., 85., 80., 72., 98., 79., 79., 80., 75., 78./

  data BLM712/&
       &74., 76., 76., 78., 72., 66., 72., 74., 78., 80., 70., 75./

  !  FOR REGION 6 FORESTS, LOAD THE FORM CLASS USING TABLE VALUES.
  !  IF A FORM CLASS HAS BEEN ENTERED VIA KEYWORD, USE IT INSTEAD.

  !  REGION 5 VOLUME ROUTINES DON'T USE FORM CLASS.

  if (IFOR.eq.4.and. FRMCLS(ISPC).le.0.) then
     IFCDBH = INT((D - 1.0) / 10.0 + 1.0)
     if (IFCDBH .lt.1) IFCDBH = 1
     if (D.gt.40.9) IFCDBH = 5
     FC = SISKFC(ISPC,IFCDBH)
  else if (IFOR.eq.7.and. FRMCLS(ISPC).le.0.) then
     FC = BLM712(ISPC)
  else
     FC = FRMCLS(ISPC)
     if (FC .le.0.) FC = 80.
  end if

  return
end
