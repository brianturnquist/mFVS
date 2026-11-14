!=====================================!
!   Initialize species site indices   !
!=====================================!
subroutine SITSET
  implicit none

  !  **SITSET--NI   DATE OF LAST REVISION:  02/25/09

  !  THIS SUBROUTINE IS USED TO SET SIMULATION CONTROLLING VALUES
  !  THAT HAVE NOT BEEN SET USING THE KEYWORDS --- SDIMAX, BAMAX.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'VOLSTD.f90'

  ! Variables
  integer :: IFIASP,ERRFLAG
  character(len=2) :: FORST
  character(len=2) :: DIST
  character(len=2) :: PROD
  character(len=2) :: VAR
  character(len=10) :: VOLEQ
  character(len=7) :: VVER
  real    :: BAMAXA(30)
  integer :: I,ISPC,INTFOR,IREGN,J,JJ,K
  data BAMAXA/&
       &140.,220.,250.,310.,240.,270.,310.,310.,200.,310.,&
       &290.,330.,380.,440.,500.,500.,390.,390.,440.,180.,&
       &290.,400.,350.,390.,260.,300.,220.,220.,160.,300./

  call VARVER(VVER)

  !  SET SDIDEF AND BAMAX VALUES WHICH HAVE NOT BEEN SET BY KEYWORD.

  if (BAMAX.le.0) BAMAX = BAMAXA(ITYPE)
  do I = 1,MAXSP
     if (SDIDEF(I).le.0.) SDIDEF(I) = BAMAX/(0.5454154*(PMSDIU/100.))
     10 continue
  end do

  do I = 1,15
     J = (I-1)*10 + 1
     JJ = J+9
     if (JJ.gt.MAXSP)JJ = MAXSP
     write(JOSTND,90)(NSP(K,1)(1:2),K = J,JJ)
     90 format(/'SPECIES ',5X,10(A2,6X))
     write(JOSTND,91)(SDIDEF(K),K = J,JJ )
     91 format('SDI MAX ',   10F8.0)
     if (JJ .eq. MAXSP)go to 93
     92 continue
  end do
  93 continue

  !  SET METHB & METHC DEFAULTS.  DEFAULTS ARE INITIALIZED TO 999 IN
  !  **GRINIT**.  IF THEY HAVE A DIFFERENT VALUE NOW, THEY WERE CHANGED
  !  BY KEYWORD IN INITRE. ONLY CHANGE THOSE NOT SET BY KEYWORD.

  !  COLVILLE  (IFOR=5) IS A REGION 6 FOREST. ALL OTHERS FORESTS IN THIS
  !  VARIANT ARE IN REGION 1.

  do ISPC = 1,MAXSP
     if (IFOR.eq.5) then
        if (METHB(ISPC).eq.999)METHB(ISPC) = 6
        if (METHC(ISPC).eq.999)METHC(ISPC) = 6
     else
        if (METHB(ISPC).eq.999)METHB(ISPC) = 6
        if (METHC(ISPC).eq.999)METHC(ISPC) = 6
     end if
     50 continue
  end do

  !  LOAD VOLUME EQUATION ARRAYS FOR ALL SPECIES

  INTFOR = KODFOR - (KODFOR/100)*100
  write(FORST,'(I2)')INTFOR
  if (INTFOR.lt.10)FORST(1:1) = '0'
  IREGN = KODFOR/100
  DIST = '  '
  PROD = '  '
  VAR = VVER(:2)
  do ISPC = 1,MAXSP
     read(FIAJSP(ISPC),'(I4)')IFIASP
     if (((METHC(ISPC).eq.6).or.(METHC(ISPC).eq.9)).and.&
          &(VEQNNC(ISPC).eq.'           ')) then
        call VOLEQDEF(VAR,IREGN,FORST,DIST,IFIASP,PROD,VOLEQ,ERRFLAG)
        VEQNNC(ISPC) = VOLEQ
     end if
     if (((METHB(ISPC).eq.6).or.(METHB(ISPC).eq.9)).and.&
          &(VEQNNB(ISPC).eq.'           ')) then
        call VOLEQDEF(VAR,IREGN,FORST,DIST,IFIASP,PROD,VOLEQ,ERRFLAG)
        VEQNNB(ISPC) = VOLEQ
     end if
  end do

  !  IF FIA CODES WERE IN INPUT DATA, WRITE TRANSLATION TABLE

  if (LFIA) then
     call FIAHEAD(JOSTND)
     write(JOSTND,211) (NSP(I,1)(1:2),FIAJSP(I),I = 1,MAXSP)
     211 format ((T12,8(A3,'=',A6,:,'; '),A,'=',A6))
  end if

  !  WRITE VOLUME EQUATION NUMBER TABLE

  call VOLEQHEAD(JOSTND)
  write(JOSTND,230)(NSP(J,1)(1:2),VEQNNC(J),VEQNNB(J),J = 1,MAXSP)
  230 format(4(2X,A2,4X,A10,1X,A10,1X))

  return
end
