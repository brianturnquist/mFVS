!=================================!
!   Compute volume form factors   !
!=================================!
subroutine FORMCL(ISPC,IFOR,D,FC)
  implicit none

  !  **FORMCL--NI     DATE OF LAST REVISION:  04/09/08

  ! THIS PROGRAM CALCULATES FORM FACTORS FOR CALCULATING CUBIC AND
  ! BOARD FOOT VOLUMES.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'

  ! Variables
  real    :: COLVFC(MAXSP,5),FC,D
  integer :: IFOR,ISPC,IFCDBH

  !  FOREST ORDER: (IFOR)
  !  1=BITTERROOT(103)    2=IDAHO PANHANDLE(104) 3=CLEARWATER(105)
  !  4=COEUR D'ALENE(106) 5=COLVILLE(621)        6=FLATHEAD(110)
  !  7=KANIKSU(113)       8=KOOTENAI(114)        9=LOLO(116)
  ! 10=NEZPERCE(117)     11=ST JOE(118)

  !  SPECIES ORDER: (ISPC)
  !  1=WP  2=L   3=DF  4=GF  5=WH  6=C   7=LP  8=S   9=AF 10=PP 11=OT

  !  COLVILLE FORM CLASS VALUES

  data COLVFC/&
       &78., 78., 78., 76., 76., 64., 80., 77., 78., 78., 75.,&
       &80., 78., 76., 78., 78., 65., 82., 79., 76., 80., 78.,&
       &80., 80., 75., 77., 80., 66., 82., 80., 74., 80., 79.,&
       &82., 80., 74., 76., 80., 66., 80., 80., 74., 82., 79.,&
       &80., 80., 74., 76., 82., 66., 80., 81., 74., 80., 78./

  !  FOR REGION 6 FOREST, LOAD THE FORM CLASS USING TABLE VALUES.
  !  IF A FORM CLASS HAS BEEN ENTERED VIA KEYWORD, USE IT INSTEAD.

  !  REGION 1 VOLUME ROUTINES DON'T USE FORM CLASS.

  if (IFOR.eq.5.and. FRMCLS(ISPC).le.0.) then
     IFCDBH = (D - 1.0) / 10.0 + 1.0
     if (IFCDBH .lt.1) IFCDBH = 1
     if (D.gt.40.9) IFCDBH = 5
     FC = COLVFC(ISPC,IFCDBH)
  else
     FC = FRMCLS(ISPC)
     if (FC .le.0.) FC = 80.
  end if

  return
end
